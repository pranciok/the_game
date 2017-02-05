
%%%         fps_actor
%%%
%%% @author Joseph Lenton
%%% @doc
%%% This is a helper actor for use with debugging. When it acts it will update
%%% it's frame count. If a second has gone past then it will print out the
%%% currrent frame count to the terminal.
%%% 

-module( fps_actor ).
-author("Joseph Lenton").

-export([
        new/0
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Construction
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          new
%% 
%% @doc Creates a new FPS Actor.
%% @spec new() -> FPS::actor()
new() ->
    actor:new(
            new_act(),
            new_paint(),
            new_state()
    ).

%%          new_state
%%
%% @doc Creates the initial state for the FPS actor.
%% @spec new_state() -> State::actor_state()
new_state() -> set_fps( actor_state:new(), 0, util:get_time() ).

%%          set_fps
%%
%% @doc Sets the fps values to the given actor_state.
%% @spec set_fps( State::actor_state(), FPS::integer(), SwitchTime::number() ) -> NewState::actor_state()
set_fps( State, FPS, SwitchTime ) ->
    actor_state:set(
        State,
        [ { fps       , FPS        },
          { switchTime, SwitchTime } ]
    ).

%%          new_act
%%
%% @doc Creates the act callback function.
%% @spec new_act() -> Act::fun( State::actor_state(), Parent::pid() )
new_act() ->
    fun( State, _Parent) ->
        Now = util:get_time(),
        SwitchTime = actor_state:get(State, switchTime),
        IsFPSTimeout = (SwitchTime < Now),
        FPS = actor_state:get( State, fps ),
        
        if
            IsFPSTimeout ->
                io:format("fps: ~w~n", [FPS]),
                
                set_fps( State,
                        0,
                        SwitchTime + 1000
                );
            true ->
                set_fps( State,
                        FPS + 1,
                        SwitchTime
                )
        end
    end.

%%          new_paint
%%
%% @doc Creates the paint callback function.
%% @spec new_paint() -> Paint::fun( State::actor_state(), G::graphics() )
new_paint() ->
    fun( _State, _G ) ->
        ok
    end.

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
