-module(initialize).

-export([all/0, all/1]).

-include("settings.hrl").

all() ->
  net_kernel:connect_node('gnode1@game.cluster'),
  ets:new(players,[public, set, named_table]),
  % ok = mnesia:create_schema(['admiral@game.cluster']),
  % {_, Nodes} = lists:unzip(?GAME_NODES),
  % ok = mnesia:create_schema(['admiral@game.cluster'|Nodes]), %% SAMO PRI INSTALACIJI!
  % ok = mnesia:create_schema(['admiral@game.cluster', 'gnode1@game.cluster', 'gnode2@game.cluster']),
  all(['gnode1@game.cluster', 'gnode2@game.cluster']).
all(Nodes) ->
  rpc:multicall(['admiral@game.cluster'|Nodes], application, start, [mnesia]),
  mnesia:create_table(wards,
      [{attributes, record_info(fields, wards)},
      {ram_copies, ['admiral@game.cluster'|Nodes]},
      {type, set}]),
  populate_blank_ward_table(),
  rpc:multicall(Nodes, node_commodore, start_link, []),
  players:start(),
  world_view:start(),
  timer:sleep(2000),
  players:create_players(10, 'gnode1@game.cluster'),
  players:create_players(10, 'gnode2@game.cluster').

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
