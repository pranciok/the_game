-module(ward).
-behaviour(gen_server).

-include("../include/settings.hrl").

-export([start_link/0, start_ward/1, add_client/2, remove_client/2, replace_client/3, broadcast/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(node_state, {name, no_of_wards}).

%%% Client API
start_ward(WardId) ->
  WardPid = gen_server:start(?MODULE, [], []),
  ets:insert(wards, {WardId, WardPid, Node(), 0}), %% node() je vjerovatno bespotreban, postoji samo za potrebe testiranja
  WardPid.

add_client(Pid, ClientPid) ->
  gen_server:cast(Pid, {add, ClientPid}).

remove_client(Pid, ClientPid) ->
  gen_server:cast(Pid, {remove, ClientPid}).

replace_client(Pid, OldClientPid, NewClientPid) ->
  gen_server:cast(Pid, {replace, OldClientPid, NewClientPid}).

broadcast(Pid, Event) ->
  gen_server:cast(Pid, {broadcast, Event}).

%% Synchronous call
stop_node(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> {ok, []}. %% no treatment of info here!

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({add, ClinetPid}, Clients) ->
  {noreply, [ClientPid|Clients]};

handle_cast({remove, ClinetPid}, Clients) ->
  {noreply, lists:delete(ClientPid, Clients)};

handle_cast({replace, OldPid, NewPid}, Clients) ->
  NewClients = lists:delete(OldPid, Clients),
  {noreply, [NewPid | NewClients]};

handle_cast({broadcast, Event}, Clients) ->
  lists:foreach(fun(ClientPid) ->
                      client_handler:nearby_event(ClientPid, Event)
                end, Clients)
  {noreply, Clients}.

handle_info(Msg, Cats) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, Cats}.

terminate(_, _) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% internal
