-module(client_handler).

-behaviour(gen_server).

-include("settings.hrl").

-export([start_client/1, execute_handover/2, moved/2, nearby_event/2, add_player_pid/2, stop_client/1, stop_client/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(client_state, {client_pid, player_pid, position}).
-record(player_event, {client_pid, action, from, to}).

%%% Client API
start_client(WardId) ->
  gen_server:start(?MODULE, [WardId], []).

execute_handover(WardId, ClientState) ->
  gen_server:start(?MODULE, [WardId, ClientState], []).

moved(Pid, Position) ->
	gen_server:cast(Pid, {moved, Position}).

nearby_event(Pid, Event) ->
	gen_server:cast(Pid, {event, Event}).

add_player_pid(Pid, PlayerPid) ->
  gen_server:cast(Pid, {add_player_pid, PlayerPid}).

%% Synchronous call
stop_client(Pid, handed_over) ->
  % io:format("*** HANDEDOVER HAPPENED ***~n", []),
  gen_server:call(Pid, terminate).

stop_client(Pid) ->
  gen_server:call(Pid, cleanup),
  gen_server:call(Pid, terminate).

%%% Server functions
init([WardId]) ->
	put(my_ward, WardId),
	{ok, #client_state{position = {0,0}}};

init([WardId, ClientState]) ->
  put(my_ward, WardId),
  [Ward] = mnesia:dirty_read(wards, WardId),
  OldClientPid = ClientState#client_state.client_pid,
  ward:replace_client(Ward#wards.pid, OldClientPid, self()),
  ClientState#client_state.player_pid ! {handover_done, self()},
  {ok, ClientState#client_state{client_pid = self()}}.

handle_call(cleanup, _From, ClientState) ->
  ClientState#client_state.player_pid ! stop,
  {reply, ok, ClientState};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

%% resubscribaj klijenta ako je promjenio ward...
%% obavijesti sve wardove, kojih se to tice, da se klijent pomaknuo.
handle_cast({moved, {X, Y}}, ClientState) ->
  if X < 0 orelse X > ?MAX_X orelse Y < 0 orelse Y > ?MAX_Y ->
    ClientState#client_state.player_pid ! nok;
    true ->
      SubResult = subscribe(self(), get(my_ward), get_ward(X, Y), ClientState),
      case SubResult of
        nok -> ClientState#client_state.player_pid ! nok;
        _ ->
          Wards = get_wards({X, Y}, [?N, ?NE, ?E, ?SE, ?S, ?SW, ?W, ?NW]),
          notify(Wards,
              #player_event{
                client_pid = self(),
                action = moved,
                from = ClientState#client_state.position,
                to = {X, Y}
                })
      %  io:format("CLIENT: ~p, my ward: ~p, affected wards: ~p~n", [self(), get(my_ward), Wards])
      end
  end,
  {noreply, ClientState#client_state{position = {X, Y}}};

handle_cast({event, Event}, ClientState) ->
  case ClientState#client_state.player_pid of
    undefinded ->
      io:format("No player PID!~n", []);
    PlayerPid ->
      PlayerPid ! {ok, Event#player_event.to}
  end,
  {noreply, ClientState};

handle_cast({add_player_pid, PlayerPid}, ClientState) ->
  PlayerPid ! start_moving,
  {noreply, ClientState#client_state{player_pid = PlayerPid}}.

handle_info(Msg, ClientState) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, ClientState}.

terminate(_, _ClientState) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
subscribe(_, Ward, Ward, _) -> ok; % ako su isti stari i novi - nema potrebe za sub
subscribe(ClientPid, OldWard, NewWard, ClientState) -> % ward se promijenio evidentno
	[OldW] = mnesia:dirty_read(wards, OldWard),
	NewWardQuery = mnesia:dirty_read(wards, NewWard),
  case NewWardQuery of
    [] -> nok;
    [NewW] ->
      %R = lists:member(NewW#wards.node, ['gnode1@game.cluster', 'gnode2@game.cluster']),
      %case R of %% privremena mjera radi samo dva game node-a
      %  true ->
          put(my_ward, NewWard),
          ward:remove_client(OldW#wards.pid, ClientPid),
          ward:add_client(NewW#wards.pid, ClientPid),
          MyNode = node(),
          case NewW#wards.node of
            MyNode -> ok;
            GameNode ->
              handover(GameNode, NewW#wards.id, ClientState#client_state{client_pid = ClientPid}) %% podrazumijeva da handover po zavrsetku napravi na lokalnom hostu ward:replace_client(NewWardPid, OldClientPid, NewClientPid)
          end
      %  _ -> nok
      % end
  end.

notify([], _) -> ok;
notify([WardId|Wards], Event) ->
  Any = mnesia:dirty_read(wards, WardId),
  case Any of
    [] -> no_ward;
    [Ward] ->
      MyNode = node(),
      ward:broadcast(WardId, Ward#wards.pid, Event),
      case Ward#wards.node of
        MyNode -> ok;
        GameNode -> rpc:call('admiral@game.cluster', admiral, ping, [{WardId, node(), GameNode}])
      end
  end,
  notify(Wards, Event).

handover(GameNode, WardId, ClientState) ->
  % io:format("******************** HANDEDOVER START ********************~n", []),
  rpc:call(GameNode, client_handler, execute_handover, [WardId, ClientState]).

get_wards(Position, Sides) -> get_wards(Position, Sides, []).
get_wards(_, [], Wards) -> Wards;
get_wards(Position, [S|Sides], Wards) ->
  AffectedWard = translate_ward(Position, S),
  case lists:member(AffectedWard, Wards) of
    true -> get_wards(Position, Sides, Wards);
    _ -> get_wards(Position, Sides, [AffectedWard|Wards])
  end.

translate_ward({X, Y}, {XCoef, YCoef}) -> get_ward(X + round(?PLAYER_VISIBILITY*XCoef), Y + round(?PLAYER_VISIBILITY*YCoef)).

get_ward(X, Y) ->
  {X div (?MAX_X div ?SQRT_OF_WARDS), Y div (?MAX_Y div ?SQRT_OF_WARDS)}.
