-module(player_simulator).

-export([spawn_player/3]).
-export([player_init/1]).

-include("settings.hrl").

-record(player_state, {client, from, to, x_coef, y_coef, stop, node}).

spawn_player(ClientPid, X, Y) ->
  spawn('admiral@game.cluster', ?MODULE, player_init,
      [#player_state{
          client = ClientPid,
          to = {X , Y}}]).

player_init(PlayerState) ->
  ets:insert(players, {self(), scale_down(PlayerState#player_state.to)}),
  receive
    start_moving -> start_moving(PlayerState)
  end.

start_moving(PlayerState) ->
  Directions = [?N, ?NE, ?E, ?SE, ?S, ?SW, ?W, ?NW],
  Direction = lists:nth(random:uniform(8), Directions),
  io:format("player changed direction: ~p~n", [Direction]),
  {DirX, DirY} = Direction,
  % {DirX, DirY} = {1, 0},
  NewDirectionPlayerState = PlayerState#player_state{x_coef = DirX, y_coef = DirY},
  NewPlayerState = loop_player(NewDirectionPlayerState),
  case PlayerState#player_state.stop of
    true -> io:format("PLAYER KILLED."), ok;
    _ -> start_moving(NewPlayerState)
  end.

loop_player(PlayerState) ->
  {X, Y} = PlayerState#player_state.to,
  client_handler:moved(PlayerState#player_state.client, {X, Y}),
  receive
    {ok, {X, Y}} ->
      R = ets:insert(players, {self(), scale_down({X,Y})}),
      io:format("pid: ~p, result: ~p, answer:~p~n", [self(), {X,Y}, R]),
      NewX = X + round(200 * PlayerState#player_state.x_coef),
      NewY = Y + round(200 * PlayerState#player_state.y_coef),
      NewPlayerState = PlayerState#player_state{from = {X, Y}, to = {NewX, NewY}},
      timer:sleep(100),
      loop_player(NewPlayerState);
    nok ->
      {OldX, OldY} = PlayerState#player_state.from,
      start_moving(PlayerState#player_state{to={OldX, OldY}});
    stop -> start_moving(PlayerState#player_state{stop = true});
    {handover_done, NewClientPid} ->
      client_handler:stop_client(PlayerState#player_state.client, handed_over),
      loop_player(PlayerState#player_state{client = NewClientPid})
  end.

scale_down({RealX, RealY}) ->
  ScaledX = round(RealX / 500),
  ScaledY = round(RealY / 500),
  {ScaledX, ScaledY}.
