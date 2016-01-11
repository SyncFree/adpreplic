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
%%
%% =============================================================================
%% Adaptive Replication User Interface
%%
%% @author Amadeo Asco, Annette Bieniusa, Adrian Vladu
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%% @doc Provides the public client interface.

-module(adpreplic).
-author(['aas@trifork.co.uk','bieniusa@cs.uni-kl.de', 'vladu@rhrk.uni-kl.de']).

-include("adprep.hrl").

-ifdef(TEST).
-compile(export_all).
-else.
-compile(report).
-export([create/10, read/1, update/2, delete/1]).
-endif.

%% Public API, can be called by clients using RPC.

%% @doc The create/10 function creates a new entry under some key,
%%      with an initial value and with a defined strategy.
%-spec create(key(), value(), strategy(), integer(), float(), 
%    float(), float(), float(), float(), -> ok | {error, reason()}.
create(Key, Value, Strategy, DecayTime, ReplThreshold, RmvThreshold, MaxStrength,
    DecayFactor, RStrength, WStrength) ->

    lager:info("Creating key ~B", [Key]),

    StrategyParams = #strategy_params{
    decay_time     = DecayTime,
    repl_threshold = ReplThreshold,
    rmv_threshold  = RmvThreshold,
    max_strength   = MaxStrength,
    decay_factor   = DecayFactor,
    rstrength      = RStrength,
    wstrength      = WStrength
    },

    lager:info("Using strategy params ~p", [StrategyParams]),

    replica_manager:create(Key, Value, Strategy, StrategyParams).

%% @doc The read/1 function returns the current value for the
%%      object stored at some key.
%-spec read(key() -> {ok, value()} | {error, reason()}.
read(Key) ->
    lager:info("Retrieving key ~B", [Key]),

    replica_manager:read(Key).

%% @doc The update/2 function updates the current value for the
%%      data stored at some key.
%-spec write(key(), value()) -> ok | {error, reason()}.
update(Key, Value) ->
    lager:info("Updating key ~B", [Key]),

    replica_manager:update(Key, Value).

%% @doc The delete/1 function deletes the data stored at some key.
%-spec delete(key(),) -> ok | {error, reason()}.
delete(Key) ->
    lager:info("Deleting key ~B", [Key]),

    replica_manager:remove_replica(Key).
