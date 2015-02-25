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
%% @author Amadeo Asco, Annette Bieniusa
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%% @doc Provides the user interface.

-module(adpreplic).
-author(['aas@trifork.co.uk','bieniusa@cs.uni-kl.de']).

-include("adprep.hrl").

%% Public API, can be called by clients using RPC.
-export([create/5, read/2, write/3 ]).

%% Public API

%% @doc The create/2 function creates a new entry under some key,
%%      with an initial value.
%-spec create(key(), id(), value(), strategy(), args()) -> ok | {error, reason()}.
create(Key, Id, Value, Strategy, Args) ->
    adpreps_:create(Key, Id, Value, Strategy, Args).

%% @doc The read/2 function returns the current value for the
%%      object stored at some key.
%-spec read(key(), id()) -> {ok, value()} | {error, reason()}.
read(Key, Id) ->
    adpreps_:read(Key, Id).

%% @doc The write/3 function updates the current value for the
%%      data stored at some key.
%-spec write(key(), id(), value()) -> ok | {error, reason()}.
write(Key, Id, Value) ->
    adpreps_:write(Key, Id, Value).
