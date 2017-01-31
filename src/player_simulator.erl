-module(player_simulator).

-export([spawn_player/3]).
-export([player_init/1]).

-include("settings.hrl").

-record(player_state, {client, from, to, x_coef, y_coef, stop, node, skip_counter = 0}).

spawn_player(ClientPid, X, Y) ->
  spawn('admiral@game.cluster', ?MODULE, player_init,
      [#player_state{
          client = ClientPid,
          to = {X , Y}}]).

player_init(PlayerState) ->
  ets:insert(players, {self(), scale_down(PlayerState#player_state.to), {0.8, 0.8, 0.8, 0.8}}),
  receive
    start_moving -> start_moving(PlayerState)
  end.

start_moving(PlayerState) ->  
  Directions = [?N, ?NE, ?E, ?SE, ?S, ?SW, ?W, ?NW],
  random:seed(erlang:timestamp()),
  Direction = lists:nth(random:uniform(8), Directions),
  {DirX, DirY} = Direction,
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
      NodeColour = proplists:get_value(node(PlayerState#player_state.client), ?COLOURS),
      SkipCounter = case PlayerState#player_state.skip_counter of
                      0 ->
                        ets:insert(players, {self(), scale_down({X,Y}), NodeColour}),
                        5;
                      Counter -> Counter - 1
                    end,
      NewX = X + round(150 * PlayerState#player_state.x_coef),
      NewY = Y + round(150 * PlayerState#player_state.y_coef),
      timer:sleep(50),
      NewPlayerState = PlayerState#player_state{from = {X, Y}, to = {NewX, NewY}, skip_counter = SkipCounter},
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
  ScaledY = round(RealY / 700),
  {ScaledX, ScaledY}.
