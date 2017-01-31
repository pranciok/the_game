-module(world_view).

% values for setting up the display
-define(WIDTH, 1000).
-define(HEIGHT, 1000).
-define(TITLE , "Game Simulator").

-export([ start/0 ]).

start() ->
    Display = display:new( ?WIDTH, ?HEIGHT, ?TITLE ),
    MainLoop = mainloop:new( Display ),
    CircleImg = image:new( Display, "circle.png" ),
    World = new_world(CircleImg),
    mainloop:run( MainLoop, World ).

new_world(CircleImg) ->
    World = world:new(
            fun(State, _P) -> world_state:act_actors(State) end,
            fun(State, G)  ->
                graphics:set_clear_color(G, color:white()),
                world_state:paint_actors(State, G)
            end
    ),
    world:add_actor( World, initiate_players(CircleImg) ),
    World.

initiate_players(CircleImg) ->
    Act = fun( AS, Parent ) ->
        LastLocation = actor_state:get_xy( AS ),
        Player = players:get(),
        Location = {0,0},
        add_player( Parent, CircleImg, Player ),
        NewAS = actor_state:set( AS, last_time, now() ),
        actor_state:set_xy(
                actor_state:set( NewAS, last_location, LastLocation ),
                Location )
    end,
    Paint = fun (_AS, _G) -> ok end,
    State = actor_state:new([
            { last_location, {0, 0} },
            { last_time    , now()  }
    ]),
    actor:new( Act, Paint, State ).

add_player( _World, _CircleImg, false ) -> ok;
add_player( World, CircleImg,  Pid ) ->
    case is_pid(Pid) of
      true ->
        Result = ets:lookup(players, Pid),
        Location = case Result of
                      [] -> {0, 0};
                      [L] ->
                        io:format("Outer:~p~n", [L]),
                        L
                   end,
        Circle = new_player( CircleImg, Location, Pid ),
        world:add_actor( World, Circle );
      _ -> ok
    end.

new_player(CircleImg, XY, Pid) ->
    Radius = 10,
    Color  = { 0.6, 0.6, 0.6, 0.6 },
    Size   = { Radius*2, Radius*2 },

    State  = actor_state:new( circle, XY, Size, [
           { pid  , Pid  }
    ]),

    Act = fun( AS, _Parent ) ->
        Pid = actor_state:get(AS, pid),
        Result = ets:lookup(players, Pid),
        {X, Y} = case Result of
                    [] -> {0, 0};
                    [{XTemp, YTemp}] ->
                      io:format("Inner:~p~n", [{XTemp, YTemp}]),
                      {XTemp, YTemp}
                 end,
        actor_state:set_xy(AS, X, Y)
    end,

    Paint = fun( AS, G ) ->
        graphics:set_color( G, Color ),
        graphics:draw_image( G, CircleImg, actor_state:get_xy(AS), actor_state:get_size(AS), true )
    end,

    actor:new( Act, Paint, State ).

string_to_num(N) ->
    case string:to_integer(N) of
        {error, Whatever} -> io:format("FAILED: ~p, ~p~n", [Whatever, N]);
        {F,_Rest} -> F
    end.
