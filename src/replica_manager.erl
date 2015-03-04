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
%% =============================================================================
%% Adaptive Replications DC - SyncFree
%%
%% Managing information about replica placement
%% 
%% @author Amadeo Asco, Annette Bieniusa
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%% @doc Replication management

-module(replica_manager).
-author(['aas@trifork.co.uk','bieniusa@cs.uni-kl.de']).
-behaviour(gen_server).

-ifdef(TEST).
-compile(export_all).
-else.
-compile(report).
% interface calls
-export([start/0, stop/0, create/4, read/1, update/2, remove_replica/1]).
    
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).
-endif.

-include("adprep.hrl").

%TODO: Parametrize by strategy
%TODO: Parameterize by data store

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Start the replication manager.
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Creates the first instance of the specified data in this DC. 
-spec create(key(), value(), strategy(), args()) -> ok | {error, reason()}.
create(Key, Value, Strategy, Args) ->
    gen_server:call(?MODULE, {create, Key, Value, Strategy, Args}, infinity).

%% @doc Reads the value of the specified data. 
-spec read(key()) -> {ok, value()} | {error, reason()}.
read(Key) ->
    gen_server:call(?MODULE, {read, Key}, infinity).
    
%% @doc Writes the new value of the specified data. 
-spec update(key(), value()) -> ok | {error, reason()}.
update(Key, Value) ->
    gen_server:call(?MODULE, {write, Key, Value}, infinity).

%% @doc Remove the local replica.
-spec remove_replica(key()) -> ok | {error, reason()}.
remove_replica(Key) ->
    gen_server:call(?MODULE, {remove, Key}, infinity).

%% @doc Shutdown replication manager.
stop() ->
    gen_server:call(?MODULE, terminate).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
   lager:info("Initializing the replica manager"),
   ?MODULE = ets:new(?MODULE, [set, named_table, protected]),
   {ok, ?MODULE}.

handle_call({create, Key, Value, _Strategy, Args}, _From, Tid) ->
    %TODO Handle the case that replica has already been created at other DC
    Result = strategy_adprep:init_strategy(Key, true, Args),
    case Result of
        {ok,_Pid}      -> 
            ok = datastore:create(Key,Value),
            ThisDC = inter_dc_manager:get_my_dc(),
            Info = #replica{key=Key,num_replicas=1,dcs=[ThisDC]},
            true = ets:insert(Tid,{Key,Info}),
            {reply, {ok}, Tid};
        {error,_Error} -> {reply, {error, _Error}, Tid};
        ignore         -> {reply, {error, ignored}, Tid}
    end;

handle_call({read, Key}, _From, Tid) ->
   {ok, _ShouldReplicate} = stategey_adprep:local_read(Key),
   CurrValue = datastore:read(Key),
   {reply, {ok, CurrValue}, Tid};

handle_call({write, Key, Value}, _From, Tid) ->
   {ok, _ShouldReplicate} = stategey_adprep:local_write(Key),
   datastore:update(Key, Value),
   {reply, {ok}, Tid};

handle_call({remove, Key}, _From, Tid) ->
    datastore:remove(Key),
    true = ets:delete(Tid, Key),
    {reply, {ok}, Tid}.

handle_cast(shutdown, Tid) ->
    lager:info("Shutting down the replica manager"),
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