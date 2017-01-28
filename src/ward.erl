-module(ward).
-behaviour(gen_server).

-include("settings.hrl").

-export([start_ward/1, stop_ward/1, add_client/2, remove_client/2, replace_client/3, broadcast/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Client API
start_ward(WardId) ->
  {ok, WardPid} = gen_server:start_link(?MODULE, [], []),
  mnesia:dirty_write(#wards{id=WardId, pid=WardPid, node=node(), weight=0}), %% node() je vjerovatno bespotreban, postoji samo za potrebe testiranja
  {ok,WardPid}.

add_client(Pid, ClientPid) ->
  gen_server:cast(Pid, {add, ClientPid}).

remove_client(Pid, ClientPid) ->
  gen_server:cast(Pid, {remove, ClientPid}).

replace_client(Pid, OldClientPid, NewClientPid) ->
  gen_server:cast(Pid, {replace, OldClientPid, NewClientPid}).

broadcast(WardId, undefined, Event) ->
  {ok, WardPid} = start_ward(WardId),
  gen_server:cast(WardPid, {broadcast, Event});
broadcast(_, WardPid, Event) ->
  gen_server:cast(WardPid, {broadcast, Event}).

%% Synchronous call
stop_ward(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> {ok, []}. %% no treatment of info here!

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({add, ClientPid}, Clients) ->
  {noreply, [ClientPid|Clients]};

handle_cast({remove, ClientPid}, Clients) ->
  {noreply, lists:delete(ClientPid, Clients)};

handle_cast({replace, OldPid, NewPid}, Clients) ->
  NewClients = lists:delete(OldPid, Clients),
  {noreply, [NewPid | NewClients]};

handle_cast({broadcast, Event}, Clients) ->
  lists:foreach(fun(ClientPid) ->
                      client_handler:nearby_event(ClientPid, Event)
                end, Clients),
  {noreply, Clients}.

handle_info(Msg, Cats) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, Cats}.

terminate(_, _) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% internal
