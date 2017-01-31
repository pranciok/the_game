-module(initialize).

-export([all/0, all/1, cleanup/0]).

-include("settings.hrl").

all() ->
  net_kernel:connect_node('gnode1@game.cluster'),
  ets:new(players,[public, set, named_table]),
  {_, Nodes} = lists:unzip(?GAME_NODES),
   % all(['gnode1@game.cluster', 'gnode2@game.cluster']).
  all(Nodes).

all(Nodes) ->
  AllNodes = ['admiral@game.cluster'|Nodes],
  % ok = mnesia:create_schema(AllNodes), %% SAMO PRI INSTALACIJI!
  rpc:multicall(AllNodes, application, start, [mnesia]),
  mnesia:create_table(wards,
      [{attributes, record_info(fields, wards)},
      {ram_copies, AllNodes},
      {type, set}]),
  populate_blank_ward_table(),
  timer:sleep(1000),
  rpc:multicall(Nodes, node_commodore, start_link, []),
  players:start(),
  world_view:start(),
  timer:sleep(2000),
  create_players_per_node(5, Nodes).

create_players_per_node(_, []) -> ok;
create_players_per_node(N, [Node|Nodes]) ->
  players:create_players(N, Node),
  create_players_per_node(N, Nodes).

cleanup() ->
  {_, Nodes} = lists:unzip(?GAME_NODES),
  players:stop_all_players(Nodes),
  rpc:multicall(Nodes, node_commodore, stop_node, [node_commodore]),
  rpc:multicall(Nodes, application, stop, [mnesia]).

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
  % IsMember = lists:member(GNodeName, ['gnode1@game.cluster', 'gnode2@game.cluster']), %% temporary measure
  % case IsMember of
  % true ->
      mnesia:dirty_write(#wards{id={X,Y}, pid=undefined, node=GNodeName, weight=0}),
  %  _-> ok
  % end,
  x_axis(X - 1, Y).
