%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Simple in-memory datastore dummy for test purposes.
-module(datastore).
-behaviour(gen_server).

-include("adprep.hrl").

% interface calls
-export([start/0, stop/0, create/2, read/1, update/2, remove/1]).
    
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

%%====================================================================
%% Server interface
%%====================================================================
%% @doc Starts server.
-spec start() -> {ok, pid()} | ignore | {error, _ }.
start() -> 
    io:format("    (datastore): Starting datastore ~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stops server asynchronously.
-spec stop() -> ok.
stop() ->
    io:format ("    (datastore): Stopping datastore ~n"),
    gen_server:cast(?MODULE, shutdown).

%% @doc Creates a new entry.
-spec create(key(), term()) -> ok | {error, already_created}.
create(Id, Obj) ->
    io:format ("    (datastore): Creating entry for ~p ~n",[Id]),
    gen_server:call(?MODULE, {create, Id, Obj}).

%% @doc Reads an entry.
-spec read(key()) -> {ok, term()} | {error, not_found}.
read(Id) ->
    io:format ("    (datastore): Reading entry for ~p ~n",[Id]),
    gen_server:call(?MODULE, {read, Id}).

%% @doc Updates an entry by merging the states.
-spec update(key(), term()) -> ok | {error, not_found}.
update(Id, Obj) ->
    io:format ("    (datastore): Updating entry for ~p ~n",[Id]),
    gen_server:call(?MODULE, {update, Id, Obj}).

%% @doc Removes an entry.
-spec remove(key()) -> ok.
remove(Id) ->
    io:format ("    (datastore): Removing entry for ~p ~n",[Id]),
    gen_server:call(?MODULE, {remove, Id}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
   ?MODULE = ets:new(?MODULE, [set, named_table, protected]),
   {ok, ?MODULE}.

handle_call({create, Id, Obj}, _From, Tid) ->
    case ets:lookup(Tid, Id) of
        [{_Id,Obj}] ->
            {reply, {error, already_created}, Tid};
        [] ->
            ets:insert(Tid, {Id, Obj}),
            {reply, ok, Tid}
    end;

handle_call({read, Id}, _From, Tid) ->
    case ets:lookup(Tid, Id) of
        [{_Id,Obj}] ->
            {reply, {ok, Obj}, Tid};
        [] ->
            {reply, {error, not_found}, Tid}
    end;

handle_call({update, Id, Obj}, _From, Tid) ->
    case ets:lookup(Tid, Id) of
        [{_Id,_Obj}] ->
            %%TODO: Add CRDT merge here! For now, just take the new version
            ets:insert(Tid, {Id, Obj}),
            {reply, ok, Tid};
        [] ->        
            {reply, {error, not_found}, Tid}
    end;

handle_call({remove, Id}, _From, Tid) ->
    case ets:lookup(Tid, Id) of
        [{_Id, _Obj}] ->
            ets:delete(Tid, Id),
            {reply, ok, Tid};
        [] ->
            {reply, ok, Tid}
    end;

handle_call(_Message, _From, State) ->
    {noreply, State}.

%% Termination
handle_cast(shutdown, Tid) ->
    io:format ("Shutting down the datastore"),
    ets:delete(Tid),
    {stop, normal, Tid};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) -> 
    {noreply, State}.

%% Server termination
terminate(_Reason, _State) -> 
    ok.

%% Code change
code_change(_OldVersion, State, _Extra) -> 
    {ok, State}.
