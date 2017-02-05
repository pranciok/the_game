-module(node_commodore).
-behaviour(gen_server).

-include("settings.hrl").

-export([start_link/0, create_player/1, stop_all_players/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(node_state, {name, no_of_wards}).

%%% Client API
start_link() ->
  gen_server:start_link(?MODULE, [#node_state{name = node(), no_of_wards = ?NO_OF_WARDS}], []).

create_player(Mode) ->
  gen_server:call(node_commodore, {create_player, Mode}).

stop_all_players() ->
  gen_server:cast(node_commodore, stop_all_players).

stop() ->
  gen_server:call(node_commodore, terminate).

%%% Server functions
init([State]) ->
  % ets:new(players,[public, set, named_table]),
  random:seed(erlang:timestamp()),
  register(node_commodore, self()),
  mast:start(),
  {ok, State}.

%% TODO: promjeni u diplomskome da su wardovi sortirani po težini!!!

handle_call({create_player, Mode}, _From, NodeState) ->
  Match = [{#wards{id = '$1',pid = '$2',node = node(),weight = '$3'},
            [{'<','$3', 2}],
            [{{'$1','$2'}}]}],
  F = fun() ->
        mnesia:select(wards, Match)
      end,
  MyWards = mnesia:activity(transaction, F),
  RandWard = random:uniform(length(MyWards)),

  {WardId, WardPid} = lists:nth(RandWard, MyWards),
  PlayerPid = start_player_on_ward(WardId, WardPid, Mode),
  rpc:call(?ADMIRAL, admiral, add_clients_total, [1]),
  {reply, PlayerPid, NodeState};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(stop_all_players, NodeState) ->
  Match = [{#wards{id = '$1',pid = '$2', node = node(), weight = '$3'},
            [],
            [{{'$1','$2'}}]}],
  MyWards = mnesia:dirty_select(wards, Match),
  stop_players(MyWards),
  {noreply, NodeState}.

handle_info(Msg, Cats) ->
    io:format("Commodore unexpected message: ~p~n",[Msg]),
    {noreply, Cats}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
start_player_on_ward(WardId, undefined, Mode) ->
  {ok, WardPid} = ward:start_ward(WardId),
  start_player(WardId, WardPid, Mode);
start_player_on_ward(WardId, WardPid, Mode) ->
  start_player(WardId, WardPid, Mode).

start_player(WardId, WardPid, Mode) ->
  {X, Y} = WardId,
  TopLeftWardCornerX = X * ?WARD_SIZE,
  TopLeftWardCornerY = Y * ?WARD_SIZE,
  PlayerSpawnX = TopLeftWardCornerX + random:uniform(?WARD_SIZE),
  PlayerSpawnY = TopLeftWardCornerY + random:uniform(?WARD_SIZE),
  {ok, ClientPid} = player_handler:start(WardId),
  ward:add_client(WardPid, ClientPid),
  PlayerPid = player_simulator:spawn_player(ClientPid, PlayerSpawnX, PlayerSpawnY, Mode),
  player_handler:add_player_pid(ClientPid, PlayerPid),
  PlayerPid.

stop_players([]) -> ok;
stop_players([{_, WardPid}|Wards]) ->
  ward:stop_ward_players(WardPid),
  stop_players(Wards).
