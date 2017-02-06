-module(player_handler).

-behaviour(gen_server).

-include("settings.hrl").

-export([start/1, execute_handover/2, moved/2, nearby_event/2,
      add_player_pid/2, ward_changed_node/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(player_state, {handler_pid, player_pid, position, ward_handover}). 
-record(player_event, {handler_pid, action, from, to}).

%%% Handler API
start(WardId) ->
  gen_server:start(?MODULE, [WardId], []).

execute_handover(WardId, HandlerState) ->
  gen_server:start(?MODULE, [WardId, HandlerState], []).

moved(Pid, Position) ->
	gen_server:cast(Pid, {moved, Position}).

nearby_event(Pid, Event) ->
	gen_server:cast(Pid, {event, Event}).

add_player_pid(Pid, PlayerPid) ->
  gen_server:cast(Pid, {add_player_pid, PlayerPid}).

ward_changed_node(Pid, Node) ->
  gen_server:cast(Pid, {ward_changed_node, Node}).

stop(Pid) ->
  gen_server:call(Pid, cleanup),
  gen_server:call(Pid, terminate).

%%% Server functions
init([WardId]) ->
	put(my_ward, WardId),
	{ok, #player_state{position = {0,0}, handler_pid = self()}};

init([WardId, HandlerState]) ->
  put(my_ward, WardId),
  {ok, HandlerState#player_state{handler_pid = self()}}.

handle_call(cleanup, _From, HandlerState) ->
  HandlerState#player_state.player_pid ! stop,
  {reply, ok, HandlerState};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

%% resubscribaj klijenta ako je promjenio ward...
%% obavijesti sve wardove, kojih se to tice, da se klijent pomaknuo.
handle_cast({moved, {X, Y}}, HandlerState) ->
  if X < 0 orelse X > ?MAX_X orelse Y < 0 orelse Y > ?MAX_Y ->
      HandlerState#player_state.player_pid ! nok,
      {noreply, HandlerState#player_state{position = {X, Y}}};
    true ->
      Subscribe = subscribe(get(my_ward), get_ward(X, Y), HandlerState),
      case Subscribe of
        nok ->
          HandlerState#player_state.player_pid ! nok,
          {noreply, HandlerState#player_state{position = {X, Y}}};
        Sub ->
          Wards = get_wards({X, Y}, [?N, ?NE, ?E, ?SE, ?S, ?SW, ?W, ?NW]),
          notify(Wards,
              #player_event{
                handler_pid = self(),
                action = moved,
                from = HandlerState#player_state.position,
                to = {X, Y}
                }),
          case Sub of
            ok -> {noreply, HandlerState#player_state{position = {X, Y}}};
            handover_done -> {stop, normal, HandlerState}
          end
      end
  end;

handle_cast({event, Event}, HandlerState) ->
  HandlerState#player_state.player_pid ! {ok, Event#player_event.to},
  {noreply, HandlerState};

handle_cast({ward_changed_node, Node}, HandlerState) ->
  {noreply, HandlerState#player_state{ward_handover = Node}};

handle_cast({add_player_pid, PlayerPid}, HandlerState) ->
  PlayerPid ! start_moving,
  {noreply, HandlerState#player_state{player_pid = PlayerPid}}.

handle_info(Msg, HandlerState) ->
  io:format("Handler unexpected message: ~p~n",[Msg]),
  {noreply, HandlerState}.

terminate(_, _HandlerState) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
subscribe(WardId, WardId, HandlerState) ->
  case HandlerState#player_state.ward_handover of
    undefined -> ok;
    NewNode -> handover(NewNode, WardId, HandlerState#player_state{ward_handover = undefined})
  end;

subscribe(OldWard, NewWard, HandlerState) ->
	[OldW] = mnesia:dirty_read(wards, OldWard),
	NewWardQuery = mnesia:dirty_read(wards, NewWard),
  case NewWardQuery of
    [] -> nok;
    [NewW] ->
      put(my_ward, NewWard),
      ward:remove_player(OldW#wards.pid, self()),
      ward:add_player(NewW#wards.pid, self()),
      MyNode = node(),
      case NewW#wards.node of
        MyNode -> ok;
        GameNode ->
          handover(GameNode, NewWard, HandlerState)
      end
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
        GameNode ->
          MyWard = get(my_ward),
          case WardId of
             MyWard -> ok;
             _ -> rpc:call(?ADMIRAL, admiral, ping, [{{get(my_ward), node()}, {Ward#wards.id, GameNode}}])
          end
      end
  end,
  notify(Wards, Event).

handover(GameNode, WardId, HandlerState) ->
  {ok, NewHandlerPid} = rpc:call(GameNode, player_handler, execute_handover, [WardId, HandlerState]),
  [Ward] = mnesia:dirty_read(wards, WardId),
  ward:replace_player(Ward#wards.pid, self(), NewHandlerPid),
  HandlerState#player_state.player_pid ! {handover_done, NewHandlerPid},
  handover_done.

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
