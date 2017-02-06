-module(player_stack).
-export([start/0, stop/0, init/0, get/0, create_players/3, add_players/1, stop_all_players/1]).


start()->
  spawn(?MODULE, init, []).

create_players(NoOfPlayers, Node, Mode) ->
  create_players(NoOfPlayers, Node, Mode, []).

create_players(0, _Node, _Mode, Players) -> add_players(Players);
create_players(NoOfPlayers, Node, Mode, Players) ->
  Pid = rpc:call(Node, node_commodore, create_player, [Mode]),
  create_players(NoOfPlayers - 1, Node, Mode, [Pid|Players]).

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
  rpc:call(Node, node_commodore, stop_all_players, []),
  stop_all_players(Nodes).

init() ->
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
