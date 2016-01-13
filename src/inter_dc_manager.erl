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

-module(inter_dc_manager).
-behaviour(gen_server).

-export([start_link/0,
         get_my_dc/0,
         start_receiver/1,
         get_dcs/0,
         set_dcs/1,
         add_dc/1,
         add_list_dcs/1,
         receive_data_item_location/2,
         send_data_item_location/1,
         get_other_dcs/1,
         read_from_any_dc/2
         ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
        dcs,
        port
    }).

%% ===================================================================
%% Public API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_my_dc() ->
    gen_server:call(?MODULE, get_my_dc, infinity).

start_receiver(Port) ->
    gen_server:call(?MODULE, {start_receiver, Port}, infinity).

get_dcs() ->
    gen_server:call(?MODULE, get_dcs, infinity).

set_dcs(DCs) ->
    gen_server:call(?MODULE, {set_dcs, DCs}, infinity).

add_dc(NewDC) ->
    gen_server:call(?MODULE, {add_dc, NewDC}, infinity).

add_list_dcs(DCs) ->
    gen_server:call(?MODULE, {add_list_dcs, DCs}, infinity).

send_data_item_location(Key) ->
    gen_server:call(?MODULE, {send_data_item_location, Key}).

receive_data_item_location(Key, DC) ->
    gen_server:call(?MODULE, {receive_data_item_location, Key, DC}).

read_from_any_dc(Key, DCs) ->
    gen_server:call(?MODULE, {read_from_any_dc, Key, DCs}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #state{
        dcs=[]
        }
    }.

handle_call(get_my_dc, _From, #state{dcs=_DCs} = State) ->
    {reply, {ok, node()}, State};

handle_call({start_receiver, Port}, _From, State) ->
    %{ok, _} = antidote_sup:start_rep(Port),
    {reply, {ok, {my_ip(),Port}}, State#state{port=Port}};

handle_call(get_dcs, _From, #state{dcs=DCs} = State) ->
    {reply, {ok, DCs}, State};

handle_call({set_dcs, DCs}, _From, #state{dcs=_DCs0} = State) ->
    {reply, ok, State#state{dcs=DCs}};

handle_call({add_dc, NewDC}, _From, #state{dcs=DCs0} = State) ->
    DCs = DCs0 ++ [NewDC],
    {reply, ok, State#state{dcs=DCs}};

handle_call({add_list_dcs, DCs}, _From, #state{dcs=DCs0} = State) ->
    DCs1 = DCs0 ++ DCs,
    {reply, ok, State#state{dcs=DCs1}};

handle_call({send_data_item_location, Key}, _From, #state{dcs=DCs} = _State) ->
    lager:info("Key is: ~p and From is: ~p", [Key, _From]),
    lager:info("DCs are: ~p", [DCs]),
    DCsWithoutNode = get_other_dcs(DCs),
    lager:info("DCs without this node are: ~p", [DCsWithoutNode]),
    Result = rpc:multicall(DCsWithoutNode, inter_dc_manager,
        receive_data_item_location,
        [Key, node()], infinity),
    lager:info("Response ~p", [Result]),

    {reply, {ok, DCs}, _State};

handle_call({receive_data_item_location, Key, DC}, _From, #state{dcs=DCs} = _State) ->
    lager:info("Key is: ~p and From is: ~p and DCs are: ~p and DC is: ~p",
        [Key, _From, DCs, DC]),
    replica_manager:add_dc_to_replica(Key, DC),
    {reply, {ok, DC}, _State};

handle_call({read_from_any_dc, Key, DCsWithReplica}, _From, #state{dcs=_DCs} = _State) ->
    lager:info("Read from any dc the key value: ~p", [DCsWithReplica]),
    Result = get_replica_from_first_dc(Key, DCsWithReplica),
    {reply, Result, _State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

my_ip() ->
    {ok, List} = inet:getif(),
    {Ip, _, _} = hd(List),
    inet_parse:ntoa(Ip).

get_other_dcs(DCs) ->
    ThisDC = node(),
    lists:filtermap(
        fun(X) -> case X of
            ThisDC -> false;
            _ -> {true, X}
        end
    end, DCs).

get_replica_from_dc(DC, Key) ->
    rpc:call(DC, replica_manager, read, [Key]).

get_replica_from_first_dc(_Key, []) ->
    {error, "Failed to get the key value"};

get_replica_from_first_dc(Key, [H | T]) ->
    case get_replica_from_dc(H, Key) of
        {ok, Value}  -> {ok, Value};
        {error, _}   -> get_replica_from_first_dc(Key, T)
    end
    .
