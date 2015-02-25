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
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================
%% 
%% @doc Decay timer.
-module(decay).
-author(['aas@trifork.co.uk', 'bieniusa@cs.uni-kl.de']).

-include("adprep.hrl").

-ifdef(EUNIT).
-compile(export_all).
-else.
-compile(report).
-export([startDecay/3, stopDecay/1]).
-export([init/1]).
-endif.


%% =============================================================================
%% Decay process
%% =============================================================================

%% @doc Applies the decay as time passes.
-spec init({integer(), key()}) -> ok.
init({Time, Key}) ->
	loop(Time, Key).

%% @doc Processes the messages hold by the mailbox.
-spec loop(integer(), key()) -> ok.
loop(Time, Key) ->
	receive
		{stop, _Pid, _Id} ->
			ok
	after 
        Time ->
			Key ! {decay, self(), 0},
			loop(Time, Key)
	end.

%% =============================================================================
%% Decay process interface
%% =============================================================================

%% @doc Starts the decay process for the specified key and time period.
-spec startDecay(integer(), key(), boolean()) -> boolean().
startDecay(DecayTime, Key, true) ->
	_ = stopDecay(Key),	%FIXME
	startDecay(DecayTime, Key, false);
startDecay(DecayTime, Key, false) ->
	DecayKey = buildPid(Key),
	register(DecayKey, spawn_link(decay, init, [{DecayTime, Key}])).

%% @doc Stops the decay process.
-spec stopDecay(key()) -> ok | {error, reason()}.
stopDecay(Key) ->
	% Stops the decay process
	DecayKey = buildPid(Key),
	% No reply is sent back to sender
	try DecayKey ! {stop, self(), 0} of
		_ ->
			% Succeed
			ok
	catch
		error:badarg -> 
			{error, may_not_exist}
	end.

%% @doc Builds the decay process ID for the specified key.
-spec buildPid(key()) -> atom().
buildPid(Key) ->
	list_to_atom(string:concat(atom_to_list(Key), "decay")).
