-module(ward_initialization).
-include("settings.hrl").

-export([create_ward_table/0, populate_blank_ward_table/0]).

create_ward_table() ->
  ets:new(wards, [set, named_table]).

populate_blank_ward_table() ->
  y_axis(?SQRT_OF_WARDS - 1).

y_axis(-1) ->ok;
y_axis(Y) ->
  x_axis(?SQRT_OF_WARDS - 1, Y),
  y_axis(Y - 1).

x_axis(-1, _) -> ok;
x_axis(X, Y) ->
  NodeKey = {X div 33, Y div 33},
  {_, GNodeName} = proplists:lookup(NodeKey, ?GAME_NODES),
  ets:insert(wards, {{X,Y}, undefined, GNodeName, 0}),
  x_axis(X - 1, Y).
