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

%% @doc Persistent data store for test purposes.

-module(datastore_mnesia_data_info).
-author(['vladu@rhrk.uni-kl.de']).
-behaviour(gen_server).

% interface calls
-export([
    start/0,
    stop/0,
    create/2,
    read/1,
    update/2,
    remove/1,
    reinitialize/0
    ]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("adprep.hrl").

%%====================================================================
%% Public API
%%====================================================================

%% Starting server
start() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Stopping server asynchronously
stop() ->
    gen_server:cast(?MODULE, shutdown).

%% Create a new entry
create(Id, Obj) ->
    gen_server:call(?MODULE, {create, Id, Obj}).

%% Reads an entry
read(Id) ->
    gen_server:call(?MODULE, {read, Id}).

%% Updates an entry by merging the states
update(Id, Obj) ->
    gen_server:call(?MODULE, {update, Id, Obj}).

%% Removes an entry
remove(Id) ->
    gen_server:call(?MODULE, {remove, Id}).

%% Reinitialises the database table
reinitialize() ->
    gen_server:call(?MODULE, {reinitialize}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    lager:info("Initializing the mnesia datastore"),
    MnesiaState = init_mnesia_data_info_with_key(),
    {MnesiaState, ?MODULE}.

handle_call({create, Id, Obj}, _From, Tid) ->
    lager:info("Creating Data info with key for  ~p",[Id]),

    FunWrite = fun() ->mnesia:write(#data_info_with_key{key=Id,value=Obj}) end,
    mnesia:transaction(FunWrite),
    {reply, ok, Tid};

handle_call({read, Id}, _From, Tid) ->
    lager:info("Reading Data info with key for  ~p",[Id]),
    DataItemId = #data_info_with_key{key = Id, _ = '_'},
    FunRead = fun() ->mnesia:select(data_info_with_key, [{DataItemId, [], ['$_']}]) end,

    Result = mnesia:transaction(FunRead),
    case Result of
        {atomic, []} ->
            lager:info("Could not retrieve key ~p", [Id]),
            {reply, {error, {"No entry found"}}, Tid};
        {atomic, [Obj | _]} ->
            {_, Key,  { _, Replicated, Strength, Strategy, DCs, TimeStamp}} = Obj,
            DataInfoWithKey = #data_info_with_key{key=Key,
                value = #data_info{
                    replicated = Replicated,
                    strength   = Strength,
                    strategy   = Strategy,
                    dcs        = DCs,
                    timestamp  = TimeStamp
                }
            },
            {reply, {ok, DataInfoWithKey}, Tid};
        _Info ->
            lager:info("Failed with: ~p", [_Info]),
            {reply, {error, _Info}, Tid}
    end;

handle_call({update, Id, Obj}, _From, Tid) ->
    lager:info("Updating Data info with key for  ~p",[Id]),

    FunWrite = fun() ->mnesia:write(#data_info_with_key{key=Id,value=Obj}) end,
    mnesia:transaction(FunWrite),
    {reply, {ok, Obj}, Tid};

handle_call({remove, Id}, _From, Tid) ->
    lager:info("Removing Data info with key for  ~p",[Id]),

    FunDelete = fun() -> mnesia:delete({data_info_with_key, Id}) end,
    {_, Message} = mnesia:transaction(FunDelete),
    lager:info("Mnesia item deleted with message: ~p", [Message]),

    {reply, Message, Tid};

handle_call({reinitialize}, _From, Tid) ->
    lager:info("Deleting table data_info_with_key"),
    mnesia:delete_table(data_info_with_key),

    init_mnesia_data_info_with_key(),

    {reply, {ok}, Tid};

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(shutdown, Tid) ->
    lager:info("Shutting down the mnesia datastore"),
    Mnesia_state = mnesia:stop(),
    lager:info("Mnesia server shutdown with status: ~p", [Mnesia_state]),
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

init_mnesia_data_info_with_key() ->
   Message = mnesia:create_schema([node()]),
   lager:info("Schema created with message: ~p", [Message]),

   MnesiaState = mnesia:start(),
   lager:info("Mnesia started with status: ~p", [MnesiaState]),

   {StateDataInfoTable, MessageDataInfoTable} = mnesia:create_table(data_info_with_key,
        [{attributes, record_info(fields, data_info_with_key)},
        {ram_copies, nodes()}, {disc_only_copies, [node()]},
        {storage_properties, [{ets, [compressed]},
        {dets, [{auto_save, 5000}]}]}]),
   lager:info("Table created with status: ~p and message: ~p",
        [StateDataInfoTable, MessageDataInfoTable]),
   MnesiaState.