-module(players).
-export([start/0, stop/0, init/0, get/0, create_players/2, add_players/1, stop_all_players/1]).


start()->
  spawn(?MODULE, init, []).

create_players(NoOfPlayers, Node) ->
  create_players(NoOfPlayers, Node, []).

create_players(0, _, Players) -> add_players(Players);
create_players(NoOfPlayers, Node, Players) ->
  Pid = rpc:call(Node, node_commodore, create_player, [node_commodore]),
  create_players(NoOfPlayers - 1, Node, [Pid|Players]).

get() ->
  player_stack ! {get, self()},
  receive
    Result -> Result
  end.

add_players(Players) ->
  player_stack ! {add, Players},
  ok.

stop() -> player_stack ! stop.

stop_all_players([]) -> ok;
stop_all_players([Node|Nodes]) ->
  rpc:call(Node, node_commodore, stop_all_players, [node_commodore]),
  stop_all_players(Nodes).

init() ->
  ets:new(players,[public, set, named_table]),
  register(player_stack, self()),
  loop([]).

loop(Players) ->
  receive
    {get, Pid} ->
      case Players of
        [] ->
          Pid ! false,
          loop(Players);
        [Player|TheRest] ->
          Pid ! Player,
          loop(TheRest)
      end;
    {add, NewPlayers} ->
      loop(NewPlayers ++ Players);
    stop -> ok
  end.
