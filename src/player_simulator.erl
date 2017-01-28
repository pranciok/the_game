-module(player_simulator).

-export([spawn_player/3]).
-export([player_init/3]).

-include("settings.hrl").

spawn_player(ClientPid, X, Y) -> %% spawn procesa potreban. gen_server?
  spawn(?MODULE, player_init, [ClientPid, X, Y]).


player_init(ClientPid, X, Y) ->
  put(client, ClientPid),
  Directions = [?N, ?NE, ?E, ?SE, ?S, ?SW, ?W, ?NW],
  {DirX, DirY} = lists:nth(rand:uniform(8), Directions),
  put(dirX, DirX),
  put(dirY, DirY),
  move_player(X, Y, 500).

move_player(_, _, 0) -> ok;
move_player(X, Y, Steps) ->
  client_handler:moved(get(client), {X, Y}),

  NewX = X + round(10 * get(dirX)),
  NewY = Y + round(10 * get(dirY)),
  timer:sleep(300),
  move_player(NewX, NewY, Steps - 1).
