-module(initialize).

-export([all/0, all/1]).

-include("settings.hrl").

all() ->
  {_, Nodes} = lists:unzip(?GAME_NODES),
  all(Nodes).
all(Nodes) ->
  % ok = mnesia:create_schema(Nodes), %% SAMO PRI INSTALACIJI!
  rpc:multicall(Nodes, application, start, [mnesia]),
  mnesia:create_table(wards,
      [{attributes, record_info(fields, wards)},
      {ram_copies, Nodes},
      {type, set}]),
  populate_blank_ward_table(),
  rpc:multicall(Nodes, node_commodore, start_link, []).

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
  IsMember = lists:member(GNodeName, ['gnode1@game.cluster', 'gnode2@game.cluster']), %% temporary measure
  case IsMember of
    true ->
      mnesia:dirty_write(#wards{id={X,Y}, pid=undefined, node=GNodeName, weight=0});
    _-> ok
  end,
  x_axis(X - 1, Y).
