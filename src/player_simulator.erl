-module(player_simulator).

-export([spawn_player/4]).
-export([player_init/1]).

-include("settings.hrl").

-record(player_state, {handler, from, to, x_coef, y_coef, stop, node, skip_counter = 0, mode}).

spawn_player(HandlerPid, X, Y, Mode) ->
  spawn(?MODULE, player_init,
      [#player_state{
          handler = HandlerPid,
          to = {X , Y},
          mode = Mode}]).

player_init(PlayerState) ->
  % ets:insert(players, {self(), scale_down(PlayerState#player_state.to), {0.8, 0.8, 0.8, 0.8}}),
  mnesia:dirty_write(#players{pid=self(), xy = scale_down(PlayerState#player_state.to), color = {0.8, 0.8, 0.8, 0.8}}),
  receive
    start_moving -> start_moving(PlayerState)
  end.

start_moving(PlayerState) ->
  {DirX, DirY} = pick_direction(PlayerState#player_state.mode),
  NewDirectionPlayerState = PlayerState#player_state{x_coef = DirX, y_coef = DirY},
  NewPlayerState = loop_player(NewDirectionPlayerState),
  case PlayerState#player_state.stop of
    true -> io:format("PLAYER KILLED."), ok;
    _ -> start_moving(NewPlayerState)
  end.

loop_player(PlayerState) ->
  {X, Y} = PlayerState#player_state.to,
  player_handler:moved(PlayerState#player_state.handler, {X, Y}),
  receive
    {ok, {X, Y}} ->
      NodeColour = proplists:get_value(node(PlayerState#player_state.handler), ?COLOURS),
      SkipCounter = case PlayerState#player_state.skip_counter of
                      0 ->
                        % ets:insert(players, {self(), scale_down({X,Y}), NodeColour}),
                        mnesia:dirty_write(#players{pid=self(), xy = scale_down({X, Y}), color = NodeColour}),
                        5; % odgadjanje pisanja u bazu
                      Counter -> Counter - 1
                    end,
      NewX = X + round(10 * PlayerState#player_state.x_coef),
      NewY = Y + round(10 * PlayerState#player_state.y_coef),
      timer:sleep(50),
      NewPlayerState = PlayerState#player_state{from = {X, Y}, to = {NewX, NewY}, skip_counter = SkipCounter},
      loop_player(NewPlayerState);
    nok ->
      {OldX, OldY} = PlayerState#player_state.from,
      start_moving(PlayerState#player_state{to={OldX, OldY}});
    stop -> start_moving(PlayerState#player_state{stop = true});
    {handover_done, NewHandlerPid} ->
      loop_player(PlayerState#player_state{handler = NewHandlerPid})
  end.

scale_down({RealX, RealY}) ->
  ScaledX = round(RealX / 50),
  ScaledY = round(RealY / 70),
  {ScaledX, ScaledY}.

pick_direction(Mode)->
  Directions = [?N, ?NE, ?E, ?SE, ?S, ?SW, ?W, ?NW],
  case Mode of
    multidirectional -> random:seed(erlang:timestamp());
    _ -> ok
  end,
  lists:nth(random:uniform(8), Directions).
