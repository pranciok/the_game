-module(admiral).
-behaviour(gen_server).

-include("settings.hrl").

-export([start_link/0, stop/0, ping/1, add_clients_total/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Client API
start_link() ->
  gen_server:start_link(?MODULE, [], []).

stop() ->
  gen_server:call(admiral, terminate).

ping(Message) ->
  gen_server:cast(admiral, {ping, Message}).

add_clients_total(No) ->
  gen_server:cast(admiral, {add_clients, No}).

%%% Server functions
init([]) ->
  ets:new(disputes,[set, named_table]),
  register(admiral, self()),
  {ok, 0}.

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({add_clients, No}, NoOfClientsTotal) ->
  {noreply, NoOfClientsTotal + No};

handle_cast({ping, {{_FromWard, FromNode}, {ToWard, ToNode}}}, NoOfClientsTotal) ->
  Result = ets:lookup(disputes, {ToWard, FromNode}),
  case Result of
    [] -> ets:insert(disputes, {{ToWard, FromNode}, 1, erlang:timestamp(), ?PING_TRESHOLD});
    [{_Key, PingNo, LastPing, Treshold}] ->
      Diff = timer:now_diff(erlang:timestamp(), LastPing) / 1000000,
      case Diff >= ?PING_TRESHOLD  of
        true -> ets:delete(disputes, {ToWard, FromNode});
        _ ->
          NewTreshold = Treshold - (1/Diff),
          case NewTreshold =< 0 of
            true -> settle_dispute(ToWard, FromNode, ToNode, NoOfClientsTotal);
            _ -> ets:insert(disputes, {{ToWard, FromNode}, PingNo + 1, erlang:timestamp(), NewTreshold})
          end
      end
  end,
  {noreply, NoOfClientsTotal}.

handle_info(Msg, Cats) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, Cats}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

settle_dispute(WardId, CallerNode, OwnerNode, ClientsTotal) ->
  [{_Key, NoOfPings, _LastPing, _PingTreshold}] = ets:lookup(disputes, {WardId, CallerNode}),
  [DisputedWard] = mnesia:dirty_read(wards, WardId),
  WardWeight = DisputedWard#wards.weight,
  CallerNodeLoad = calculate_load(CallerNode, ClientsTotal),
  OwnerNodeLoad = calculate_load(OwnerNode, ClientsTotal),
  Resolution = NoOfPings - WardWeight - CallerNodeLoad + OwnerNodeLoad,
  % io:format("RESOLUTION: ~p~n
  %           pings: ~p~n
  %           ward weight: ~p~n
  %           caller load: ~p~n
  %           owner load:~p~n",
  %           [Resolution, NoOfPings, WardWeight, CallerNodeLoad, OwnerNodeLoad]),
  case Resolution > 0 of
    true ->
      rpc:call(CallerNode, ward, execute_handover, [DisputedWard]),
      ets:delete(disputes, {WardId, CallerNode}),
      %io:format("from:~p, to:~p, ward:~p~n", [OwnerNode, CallerNode, DisputedWard#wards.id]),
      ok; %%% WARD HANDOVER
    _ -> ok
  end.

calculate_load(Node, ClientsTotal) ->
  Match = [{#wards{id = '_',pid = '_',node = Node, weight = '$1'},
            [],
            ['$1']}],
  NodeWeight = lists:sum(mnesia:dirty_select(wards, Match)), %% add weights of all wards belonging to node
  NodeLoad = math:pow(NodeWeight, 2) / (ClientsTotal/?NO_OF_NODES),
  NodeLoad.
