-module(ward).
-behaviour(gen_server).

-include("settings.hrl").

-export([start_ward/1, stop_ward/1, stop_ward_players/1, get_clients/1, add_client/2,
        execute_handover/1, remove_client/2, replace_client/3, broadcast/3, node_change/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Client API %%%
start_ward(WardId) ->
  {ok, WardPid} = gen_server:start(?MODULE, [WardId], []),
  [Ward] = mnesia:dirty_read(wards, WardId),
  mnesia:dirty_write(Ward#wards{pid=WardPid}),
  {ok, WardPid}.

execute_handover(DisputedWard) ->
  OldPid = DisputedWard#wards.pid,
  Clients = ward:get_clients(OldPid),
  {ok, NewPid} = gen_server:start(?MODULE, [DisputedWard#wards.id, Clients], []),
  mnesia:dirty_write(DisputedWard#wards{pid = NewPid, node = node()}),
  ward:node_change(OldPid, node()).

get_clients(Pid) ->
  gen_server:call(Pid, get_clients).

add_client(Pid, ClientPid) ->
  gen_server:cast(Pid, {add, ClientPid}).

remove_client(Pid, ClientPid) ->
  gen_server:cast(Pid, {remove, ClientPid}).

replace_client(Pid, OldClientPid, NewClientPid) ->
  gen_server:cast(Pid, {replace, OldClientPid, NewClientPid}).

broadcast(WardId, undefined, Event) ->
  [Ward] = mnesia:dirty_read(wards, WardId),
  MyNode = node(),
  case Ward#wards.node of
        MyNode ->
          {ok, WardPid} = ward:start_ward(WardId),
          gen_server:cast(WardPid, {broadcast, Event});
        OtherNode ->
          rpc:call(OtherNode, ward, broadcast, [WardId, Ward#wards.pid, Event])
  end;

broadcast(_, WardPid, Event) ->
  gen_server:cast(WardPid, {broadcast, Event}).

node_change(Pid, Node) ->
  gen_server:cast(Pid, {node_change, Node}).

stop_ward_players(Pid) ->
  gen_server:cast(Pid, stop_ward_players).

%% Synchronous call
stop_ward(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([WardId]) ->
  put(ward_id, WardId),
  {ok, []};

init([WardId, Clients]) ->
  put(ward_id, WardId),
  {ok, Clients}.

handle_call(get_clients, _From, Clients) ->
  {reply, Clients, Clients};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({add, ClientPid}, Clients) ->
  WardId = get(ward_id),
  [Ward] = mnesia:dirty_read(wards, WardId),
  Weight = Ward#wards.weight + 1,
  mnesia:dirty_write(Ward#wards{weight=Weight}),
  {noreply, [ClientPid|Clients]};

handle_cast({remove, ClientPid}, Clients) ->
  WardId = get(ward_id),
  [Ward] = mnesia:dirty_read(wards, WardId),
  Weight = Ward#wards.weight - 1,
  mnesia:dirty_write(Ward#wards{weight=Weight}),

  {noreply, lists:delete(ClientPid, Clients)};

handle_cast({replace, OldPid, NewPid}, Clients) ->
  NewClients = lists:delete(OldPid, Clients),
  {noreply, [NewPid | NewClients]};

handle_cast({broadcast, Event}, Clients) ->
  lists:foreach(fun(ClientPid) ->
                      client_handler:nearby_event(ClientPid, Event)
                end, Clients),
  {noreply, Clients};

handle_cast(stop_ward_players, Clients) ->
  stop_players(Clients),
  {noreply, []};

handle_cast({node_change, Node}, Clients) ->
  lists:foreach(fun(ClientPid) ->
                      client_handler:ward_changed_node(ClientPid, Node)
                end, Clients),
{stop, normal, []}.

handle_info(Msg, Cats) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, Cats}.

terminate(_, _) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% internal
stop_players([]) -> ok;
stop_players([Client|Clients]) ->
  client_handler:stop_client(Client),
  stop_players(Clients).
