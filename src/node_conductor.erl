-module(node_conductor).
-behaviour(gen_server).

-include("../include/settings.hrl").

-export([start_link/0, create_player/1, stop_node/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(node_state, {name, no_of_wards}).

%%% Client API
start_link() ->
  ward_initialization:create_ward_table(),
  ward_initialization:populate_blank_ward_table(),
  gen_server:start_link(?MODULE, [#node_state{name = ?THIS_NODE, no_of_wards = ?NO_OF_WARDS}], []).

create_player(Pid) ->
  gen_server:cast(Pid, create_player).

%% Synchronous call
stop_node(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([State]) -> {ok, State}. %% no treatment of info here!

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(create_player, NodeState) ->
  RandWard = rand:uniform(NodeState#node_state.no_of_wards),
  io:format("~p~n", [RandWard]),
  {Match, _} = ets:select(wards, [{ {'_','_',gnode1,'_'},[],['$_'] }], RandWard),
  {WardId, WardPid, _, _} = lists:nth(1, Match),
  start_player_on_ward(WardId, WardPid),
  {noreply, NodeState}.

handle_info(Msg, Cats) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, Cats}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
start_player_on_ward(WardId, undefined) ->
  WardPid = ward:start_ward(WardId), %% dodaje svoj pid u ets pri stvaranju!
  start_player(WardId, WardPid);
start_player_on_ward(WardId, WardPid) ->
  start_player(WardId, WardPid).

start_player(WardId, WardPid) ->
  {X, Y} = WardId,
  TopLeftWardCornerX = X * ?WARD_SIZE,
  TopLeftWardCornerY = Y * ?WARD_SIZE,
  PlayerSpawnX = TopLeftWardCornerX + rand(?WARD_SIZE),
  PlayerSpawnY = TopLeftWardCornerY + rand(?WARD_SIZE),
  ClientPid = client_handler:start_client(WardId),
  ward:add_client(WardPid, ClientPid),
  player_simulator:spawn_player(ClientPid, PlayerSpawnX, PlayerSpawnY).
