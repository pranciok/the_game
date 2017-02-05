-module(game).

-export([create_mnesia_schemas/0,
         init_all/0,
         init_mast_off/0,
         init_admiral_off/0,
         stop_simulation/0,
         cleanup/0,

         cluster_unidirectional/2,
         cluster_multidirectional/2,
         uniform_unidirectional/1,
         uniform_multidirectional/1]).

-include("settings.hrl").

create_mnesia_schemas() -> % mozda dodati make:all([netload])?
  {_, GameNodes} = lists:unzip(?GAME_NODES),
  AllNodes = [?ADMIRAL|GameNodes],
  ok = mnesia:create_schema(AllNodes). %% SAMO PRI INSTALACIJI!

init_all() ->
  init(all).

init_mast_off() ->
  init(mast_off).

init_admiral_off() ->
  init(admiral_off).

init(AdmiralMode) ->
  {_, Nodes} = lists:unzip(?GAME_NODES),
  connect_nodes(Nodes),
  AllNodes = [?ADMIRAL|Nodes],
  rpc:multicall(AllNodes, application, start, [mnesia]),
  mnesia:create_table(wards,
      [{attributes, record_info(fields, wards)},
      {ram_copies, AllNodes},
      {type, set}]),
  mnesia:create_table(players,
      [{attributes, record_info(fields, players)},
      {ram_copies, AllNodes},
      {type, set}]),
  populate_blank_ward_table(),
  admiral:start_link(AdmiralMode),
  rpc:multicall(Nodes, node_commodore, start_link, []),
  players:start(),
  MainLoop = world_view:start(),
  put(mainloop, MainLoop),
  ok.

stop_simulation() ->
  MainLoop = get(mainloop),
  mainloop:stop(MainLoop).

cleanup() ->
  {_, Nodes} = lists:unzip(?GAME_NODES),
  players:stop_all_players(Nodes),
  rpc:multicall(Nodes, node_commodore, stop, []),
  rpc:call('admiral@gamel.cluster', admiral, stop, []),
  rpc:multicall(Nodes, application, stop, [mnesia]).


cluster_unidirectional(NoPlayers, NoClusters) ->
  cluster(NoPlayers, NoClusters, unidirectional).

cluster_multidirectional(NoPlayers, NoClusters) ->
  cluster(NoPlayers, NoClusters, multidirectional).

uniform_unidirectional(NoPlayers) ->
  uniform(NoPlayers, unidirectional).

uniform_multidirectional(NoPlayers) ->
  uniform(NoPlayers, multidirectional).


%% internal
cluster(NoPlayers, NoClusters, Mode) ->
  {_, Nodes} = lists:unzip(?GAME_NODES),
  ClusterSize = NoPlayers div NoClusters,

  case NoClusters < length(Nodes) orelse NoClusters =< 0 of
    true ->
      Clusters = get_cluster_nodes(NoClusters, Nodes),
      create_players_per_node(ClusterSize, Clusters, Mode);
    _ -> io:format("Wrong number of clusters!~nNumber of clusters must be greater than zero~n and less than number of Game Nodes!~n")
  end.

uniform(No, Mode) ->
  {_, Nodes} = lists:unzip(?GAME_NODES),
  create_players_per_node(No div erlang:length(Nodes), Nodes, Mode).


create_players_per_node(_, [], _Mode) -> ok;
create_players_per_node(N, [Node|Nodes], Mode) ->
  players:create_players(N, Node, Mode),
  create_players_per_node(N, Nodes, Mode).

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

get_cluster_nodes(NoClusters, Nodes) ->
  get_cluster_nodes(NoClusters, Nodes, []).
get_cluster_nodes(0, _Nodes, Clusters) -> Clusters;
get_cluster_nodes(NoCluster, Nodes, Clusters) ->
  Cluster = lists:nth(random:uniform(erlang:length(Nodes)), Nodes),
  get_cluster_nodes(NoCluster - 1, lists:delete(Cluster, Nodes), [Cluster|Clusters]).

connect_nodes([]) -> ok;
connect_nodes([N|Nodes]) ->
  net_kernel:connect_node(N),
  connect_nodes(Nodes).
