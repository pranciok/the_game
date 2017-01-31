-module(admiral).
-behaviour(gen_server).

-include("settings.hrl").

-export([start_link/0, stop_admiral/0, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Client API
start_link() ->
  gen_server:start_link(?MODULE, [], []).

stop_admiral() ->
  gen_server:call(admiral, terminate).

ping(Message) ->
  gen_server:cast(admiral, {ping, Message}).

%%% Server functions
init([]) ->
  ets:new(disputes,[set, named_table]),
  register(admiral, self()),
  {ok, []}.

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({ping, {WardId, FromNode, ToNode}}, State) ->
  ets:insert(disputes, {WardId, FromNode, ToNode}),
  {noreply, State}.

handle_info(Msg, Cats) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, Cats}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
