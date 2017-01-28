-module(client_handler).

-behaviour(gen_server).

-include("settings.hrl").

-export([start_client/1, moved/2, nearby_event/2, stop_client/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(player_state, {position}).
-record(player_event, {client_pid, action, from, to}).

%%% Client API
start_client(WardId) ->
  gen_server:start(?MODULE, [WardId], []).

moved(Pid, Position) ->
	gen_server:cast(Pid, {moved, Position}).

nearby_event(Pid, Event) ->
	gen_server:cast(Pid, {event, Event}).

%% Synchronous call
stop_client(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([WardId]) ->
	put(my_ward, WardId),
	{ok, #player_state{position = {0,0}}}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.


%% resubscribaj klijenta ako je promjenio ward...
%% obavijesti sve wardove, kojih se to tice, da se klijent pomaknuo.
handle_cast({moved, {X, Y}}, ClientState) ->
	subscribe(self(), get(my_ward), get_ward(X, Y)),
	Wards = get_wards({X, Y}, [?N, ?NE, ?E, ?SE, ?S, ?SW, ?W, ?NW]),
	notify(Wards,
        #player_event{
          client_pid = self(),
          action = moved,
          from = ClientState#player_state.position,
					to = {X, Y}
					}),
  io:format("my ward: ~p~n affected wards: ~p~n position: ~p~n", [get(my_ward), Wards, {X,Y}]),
  {noreply, ClientState#player_state{position = {X, Y}}};

handle_cast({event, Event}, ClientState) ->
  io:format("got notified about event:~nFROM: ~p~n  TO:~p~n", [Event#player_event.from, Event#player_event.to]), %% TODO
  {noreply, ClientState}.

handle_info(Msg, ClientState) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, ClientState}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
subscribe(_, Ward, Ward) -> ok; % ako su isti stari i novi- nema potrebe za sub
subscribe(ClientPid, OldWard, NewWard) -> % ward se promijenio evidentno
	[OldW] = mnesia:dirty_read(wards, OldWard),
	[NewW] = mnesia:dirty_read(wards, NewWard),
  ward:remove_client(OldW#wards.pid, ClientPid),
  ward:add_client(NewW#wards.pid, ClientPid), %% bezuvjetno se dodaje na ward podrazumijevaci da moze zavrsiti na wardu sa drugog node-a, medjutim racuna se na to da ce prilikom handovera doci do zamjene pid-ova.
  put(my_ward, NewWard),
  MyNode = node(),
  case node(NewW#wards.pid) of
    MyNode -> ok;
    GameNode -> handover(GameNode, ClientPid) %% podrazumijeva da handover po zavrsetku napravi na lokalnom hostu ward:replace_client(NewWardPid, OldClientPid, NewClientPid)
  end,
  ok.

notify([], _) -> ok;
notify([WardId|Wards], Event) ->
  [Ward] = mnesia:dirty_read(wards, WardId),
  ward:broadcast(WardId, Ward#wards.pid, Event), %% ako se ustanovi da undefined nodove ipak treba notifieat treba napraviti broadcast_sync call koji vraca pid novostvorenog warda
  MyNode = node(),
  case Ward#wards.pid of
      undefined -> ok;
      _ ->
        case node(Ward#wards.pid) of
          MyNode -> ok;
          GameNode -> ?SUPER_NODE ! {node(), GameNode, WardId} %% kopija
        end
  end,
  notify(Wards, Event).

handover(GameNode, PlayerData) ->
  gen_server:cast({node_commodore, GameNode}, {self(), PlayerData}). %% self() pid je potreban da spawn-ani proces, kad je spreman, moze playeru poslati upute o novom hostu, napraviti ward:replace_client(OldClient, NewClient) gdje ce new client na tom hostu ubiti biti self() te potom ubiti client_handler koji vise za njega nije odgovoran.

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
