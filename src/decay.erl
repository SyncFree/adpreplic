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
%% Adaptive Replication
%% 
%% @author Amadeo Asco, Annette Bieniusa, Adrian Vladu
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================
%% 
%% @doc Decay timer.

-module(decay).
-author(['aas@trifork.co.uk','bieniusa@cs.uni-kl.de', 'vladu@rhrk.uni-kl.de']).

-include("adprep.hrl").

-ifdef(TEST).
-compile(export_all).
-else.
-compile(report).
-export([start/0, startDecayTimer/3, stopDecayTimer/1]).
-endif.


%% =============================================================================
%% Decay process interface
%% =============================================================================

%FIXME: Start Timer service? Documentation says the following:
start() ->
    timer:start().
% Starts the timer server. Normally, the server does not need to be started explicitly.
% It is started dynamically if it is needed. This is useful during development,
% but in a target system the server should be started explicitly.
% Use configuration parameters for kernel for this.


%% @doc Starts the decay process for the specified key and time period.
-spec startDecayTimer(time(), pid(), none | timer()) 
        -> {ok, timer:tref()} | {error, reason()}.

startDecayTimer(DecayTime, Receiver, none) ->
    lager:info("Receiver is : ~p", [Receiver]),
    timer:apply_interval(DecayTime * 1000, strategy_adprep, notify_decay, [Receiver]);

startDecayTimer(DecayTime, Key, Timer) ->
    %%_ = stopDecayTimer(Timer), %% FIXME?
    lager:info("Timer is: ~p", [Timer]),
    startDecayTimer(DecayTime, Key, none).

%% @doc Stops the decay process.
-spec stopDecayTimer(timer()) -> ok | {error, reason()}.

stopDecayTimer(Timer) ->
    {ok, cancel} = timer:cancel(Timer),
    ok.