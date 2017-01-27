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
	{ok, [WardId]}. %% no treatment of info here!

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
  {noreply, ClientState#player_state{position = {X, Y}}};

handle_cast({event, Event}, ClientState) ->
	ok. %% TODO

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
	{_, OldWardPid, _, _} = ets:lookup(wards, OldWard),
	{_, NewWardPid, _, _} = ets:lookup(wards, NewWard),
  ward:remove_client(OldWardPid, ClientPid),
  ward:add_client(NewWardPid, ClientPid), %% bezuvjetno se dodaje na ward podrazumijevaci da moze zavrsiti na wardu sa drugog node-a, medjutim racuna se na to da ce prilikom handovera doci do zamjene pid-ova.
  MyNode = node(),
  case node(NewWardPid) of
    MyNode -> ok;
    GameNode -> handover(GameNode, ClientPid) %% podrazumijeva da handover po zavrsetku napravi na lokalnom hostu ward:replace_client(NewWardPid, OldClientPid, NewClientPid)
  end,
  ok.

notify([], _) -> ok;
notify([WardId|Wards], Event) ->
  {_, WardPid, _, _} = ets:lookup(wards, WardId),
	ward:broadcast(WardId, WardPid, Event),
  MyNode = node(),
  case node(WardPid) of
    MyNode -> ok;
    GameNode -> ?SUPER_NODE ! {node(), GameNode, WardId} %% kopija
  end,
  notify(Wards, Event).

handover(GameNode, PlayerData) ->
  gen_server:cast({node_commodore, GameNode}, {self(), PlayerData}). %% self() pid je potreban da spawn-ani proces, kad je spreman, moze playeru poslati upute o novom hostu, napraviti ward:replace_client(OldClient, NewClient) gdje ce new client na tom hostu ubiti biti self() te potom ubiti client_handler koji vise za njega nije odgovoran.

get_wards(Position, Sides) -> get_wards(Position, Sides, []).
get_wards(_, [], Wards) -> Wards;
get_wards(Position, [S|Sides], Wards) ->
  get_wards(Position, Sides, [translate_ward(Position,S) ++ Wards]).

translate_ward({X, Y}, {XCoef, YCoef}) -> get_ward(X + X*XCoef, Y + Y*YCoef).

get_ward(X, Y) ->
  {X / (?MAX_X / ?SQRT_OF_WARDS), Y / (?MAX_Y / ?SQRT_OF_WARDS)}.
