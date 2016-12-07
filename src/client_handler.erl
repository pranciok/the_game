-module(client_handler).

-behaviour(gen_server).

-include(include/settings.hrl).

%% u initu: set(my_ward)!!!

%% resubscribaj klijenta ako je promjenio ward...
%% obavijesti sve wardove, kojih se to tice, da se klijent pomaknuo.

handle_cast(moved, {x, y}, State) ->
	subscribe(self(), get(my_ward), get_ward_hash(x, y)),
	Wards = get_wards({x, y}, [?N, ?NE, ?E, ?SE, ?S, ?SW, ?W, ?NW]),
	notify(Wards,
        #player_event{
          client_pid = self(),
          action = moved,
          position = {x, y}}).

subscribe(_, Ward, Ward) -> ok; % ako su isti stari i novi- nema potrebe za sub
subscribe(ClientPid, OldWard, NewWard) -> % ward se promijenio evidentno
  ward:remove_client(OldWard, ClientPid),
  ward:add_client(NewWard, ClientPid), %% bezuvijetno se dodaje na ward podrazumijevaci da moze zavrsiti na wardu sa drugog node-a, medjutim racuna se na to da ce prilikom handovera doci do zamjene pid-ova.
  {NewWardId, NewWardPid} = ets:get(wards, NewWard)
  case node(NewWardPid) of
    node() -> ok;
    GameNode -> handover(GameNode, ClientPid) %% podrazumijeva da handover po zavrsetku napravi na lokalnom hostu ward:replace_client(OldClientPid, NewClientPid)
  set(my_ward, NewWard),
  ok.

notify([W|Wards], Event)
  {_, WardPid} = ets:get(wards, W),
  WardPid ! Event,
  case node(WardPid) of
    node() -> ok;
    GameNode -> ?SUPER_NODE ! {node(), GameNode, W} %% kopija

handover(GameNode, PlayerData) ->
  spawn(GameNode, node_conductor, player_handover/2, [self(), PlayerData]). %% self() pid je potreban da spawn-ani proces, kad je spreman, moze playeru poslati upute o novom hostu, napraviti ward:replace_client(OldClient, NewClient) gdje ce new client na tom hostu ubiti biti self() te potom ubiti client_handler koji vise za njega nije odgovoran.

get_wards(Position, Sides) -> get_wards(Position, Sides, []).
get_wards(_, [], Wards) -> Wards;
get_wards(Position, [S|Sides], Wards) ->
  get_wards(Position, Sides, [translate_ward(Position,S) ++ Wards]).

translate_ward({X, Y}, {XCoef, YCoef}) ->get_ward({X + X*XCoef, Y + Y*YCoef})))

get_ward(X, Y) ->
  {X / (?MAX_X / ?SQRT_NO_OF_WARDS), Y / (?MAX_Y / ?SQRT_NO_OF_WARDS)}.
