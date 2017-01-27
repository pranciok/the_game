-module(player_simulator).
-export([spawn_player/3]).

spawn_player(X, Y, ClientPid) -> %% spawn procesa potreban. gen_server?
  client_handler:moved(ClientPid, {X, Y}).
