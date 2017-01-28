-module(node_commodore).
-behaviour(gen_server).

-include("settings.hrl").

-export([start_link/0, create_player/1, stop_node/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(node_state, {name, no_of_wards}).

%%% Client API
start_link() ->
  gen_server:start_link(?MODULE, [#node_state{name = node(), no_of_wards = ?NO_OF_WARDS}], []).

create_player(Pid) ->
  gen_server:cast(Pid, create_player).

%% Synchronous call
stop_node(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([State]) ->
  register(node_commodore, self()),
  {ok, State}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(create_player, NodeState) ->
  RandWard = rand:uniform(NodeState#node_state.no_of_wards),
  io:format("~p~n", [RandWard]),
  Match = [{#wards{id = '$1',pid = '$2',node = node(),weight = '$3'},
            [{'<','$3',0.5}],
            [{{'$1','$2'}}]}],
  MyWards = mnesia:dirty_select(wards, Match),
  {WardId, WardPid} = lists:nth(RandWard, MyWards),
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
  {ok, WardPid} = ward:start_ward(WardId),
  start_player(WardId, WardPid);
start_player_on_ward(WardId, WardPid) ->
  start_player(WardId, WardPid).

start_player(WardId, WardPid) ->
  {X, Y} = WardId,
  TopLeftWardCornerX = X * ?WARD_SIZE,
  TopLeftWardCornerY = Y * ?WARD_SIZE,
  PlayerSpawnX = TopLeftWardCornerX + rand:uniform(?WARD_SIZE),
  PlayerSpawnY = TopLeftWardCornerY + rand:uniform(?WARD_SIZE),
  {ok, ClientPid} = client_handler:start_client(WardId),
  ward:add_client(WardPid, ClientPid),
  player_simulator:spawn_player(ClientPid, PlayerSpawnX, PlayerSpawnY).
