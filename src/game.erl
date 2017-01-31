-module(game).

-export([init/0, init/1, cleanup/0]).

-include("settings.hrl").

init() ->
  net_kernel:connect_node('gnode1@game.cluster'),
  {_, Nodes} = lists:unzip(?GAME_NODES),
  init(Nodes).

init(Nodes) ->
  AllNodes = ['admiral@game.cluster'|Nodes],
  % ok = mnesia:create_schema(AllNodes), %% SAMO PRI INSTALACIJI!
  rpc:multicall(AllNodes, application, start, [mnesia]),
  mnesia:create_table(wards,
      [{attributes, record_info(fields, wards)},
      {ram_copies, AllNodes},
      {type, set}]),
  populate_blank_ward_table(),
  admiral:start_link(),
  timer:sleep(5000),
  rpc:multicall(Nodes, node_commodore, start_link, []),
  players:start(),
  world_view:start(),
  create_players_per_node(5, Nodes),
  admiral:add_clients_total(2 * ?NO_OF_NODES).

create_players_per_node(_, []) -> ok;
create_players_per_node(N, [Node|Nodes]) ->
  players:create_players(N, Node),
  create_players_per_node(N, Nodes).

cleanup() ->
  {_, Nodes} = lists:unzip(?GAME_NODES),
  players:stop_all_players(Nodes),
  rpc:multicall(Nodes, node_commodore, stop_node, [node_commodore]),
  admiral:stop(),
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

  F = fun() ->
        mnesia:write(#wards{id={X,Y}, pid=undefined, node=GNodeName, weight=0})
      end,
  mnesia:activity(transaction, F),

  x_axis(X - 1, Y).
