-module(node_commodore).
-behaviour(gen_server).

-include("settings.hrl").

-export([start_link/0, create_player/1, stop_all_players/1, stop_node/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(node_state, {name, no_of_wards}).

%%% Client API
start_link() ->
  gen_server:start_link(?MODULE, [#node_state{name = node(), no_of_wards = ?NO_OF_WARDS}], []).

create_player(Pid) ->
  gen_server:call(Pid, create_player).

stop_all_players(Pid) ->
  gen_server:cast(Pid, stop_all_players).

%% Synchronous call

stop_node(Pid) ->
  gen_server:call(Pid, terminate).

%%% Server functions
init([State]) ->
  random:seed(erlang:timestamp()),
  register(node_commodore, self()),
  {ok, State}.

handle_call(create_player, _From, NodeState) ->
  RandWard = random:uniform(?NO_OF_WARDS - 1),
  Match = [{#wards{id = '$1',pid = '$2',node = node(),weight = '$3'},
            [{'<','$3',0.5}],
            [{{'$1','$2'}}]}],
  F = fun() ->
        mnesia:select(wards, Match)
      end,
  MyWards = mnesia:activity(transaction, F),

  {WardId, WardPid} = lists:nth(RandWard, MyWards),
  PlayerPid = start_player_on_ward(WardId, WardPid),
  {reply, PlayerPid, NodeState};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(stop_all_players, NodeState) ->
  Match = [{#wards{id = '$1',pid = '$2', node = node(), weight = '$3'},
            [{'<','$3',0.5}],
            [{{'$1','$2'}}]}],
  MyWards = mnesia:dirty_select(wards, Match),
  stop_players(MyWards),
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
  PlayerSpawnX = TopLeftWardCornerX + random:uniform(?WARD_SIZE),
  PlayerSpawnY = TopLeftWardCornerY + random:uniform(?WARD_SIZE),
  {ok, ClientPid} = client_handler:start_client(WardId),
  ward:add_client(WardPid, ClientPid),
  PlayerPid = player_simulator:spawn_player(ClientPid, PlayerSpawnX, PlayerSpawnY),
  client_handler:add_player_pid(ClientPid, PlayerPid),
  PlayerPid.

stop_players([]) -> ok;
stop_players([{_, WardPid}|Wards]) ->
  ward:stop_ward_players(WardPid),
  stop_players(Wards).
