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
%% First Proposed Adaptive Replication Strategy - SyncFree
%%
%% Any strategy must implement the function run(Key, DCs, Args) and process the 
%% received messages.
%%
%% @author Amadeo Asco, Annette Bieniusa
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%%
%% @doc Provides operations required in a database.
-module(strategy_adprep).
-author(['aas@trifork.co.uk','bieniusa@cs.uni-kl.de']).

-ifdef(TEST).
-compile(export_all).
-else.
-compile(report).
%% Public API
-export([notify_decay/1, local_write/1, local_read/1]).
%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-endif.
-behaviour(gen_server).

-include("strategy_adprep.hrl").
-include("adprep.hrl").

%TODO Methods should return whether local replica should be installed / removed.

%% =============================================================================
%% Public API.
%% =============================================================================
-spec notify_decay(pid()) -> ok.
notify_decay(Pid) ->
    gen_server:cast(Pid, decay).

%% @doc Read to the local replica
-spec local_write(key()) -> ok.
local_write(Key) ->
    gen_server:call(Key, {local_write}, infinity).

%% @doc Update to the local replica
-spec local_read(key()) -> ok.
local_read(Key) ->
    gen_server:call(Key, {loca_read}, infinity).

%% =============================================================================
%% Gen_server callbacks.
%% =============================================================================
%% @doc Initializes the process and start the process 
%%      with the specified arguments.
-spec init({key(),boolean(),strategy_params()}) -> {ok,strategy_state()}.
init({Key, Replicated,
		#strategy_params{ 	
		            decay_time 		 = DecayTime, 
				  	repl_threshold 	 = ReplThreshold,
				  	wstrength 		 = WStrength} = Strategy }) ->
	% Calculate strength of the replica
	Strength = case Replicated of 
		true  -> ReplThreshold + WStrength;
		false -> 0.0
	end,
	{ok, Timer} = decay:startDecayTimer(DecayTime, self(), none),
	{ok, #strategy_state{key=Key, strength=Strength, replicated=Replicated, params=Strategy, timer=Timer}}.

%% =============================================================================
%% Messages handlers
%% =============================================================================

handle_call({local_write}, _From, 
	{#strategy_state{strength=Strength, replicated=Replicated, 
	params=#strategy_params{wstrength=WStrength, max_strength=MaxStrength, 
	repl_threshold=ReplThreshold}}=StrategyState}) ->
	%Update strength
	NewStrength = incStrength(Strength, WStrength, MaxStrength),
	ShouldReplicate = (NewStrength > ReplThreshold) or Replicated,
	{reply, {ok, ShouldReplicate}, StrategyState#strategy_state{strength=NewStrength}};

handle_call({local_read}, _From, 
	{#strategy_state{strength=Strength, replicated=Replicated, 
	params=#strategy_params{rstrength=RStrength, max_strength=MaxStrength, 
	repl_threshold=ReplThreshold}}=StrategyState}) ->
	NewStrength = incStrength(Strength, RStrength, MaxStrength),
	ShouldReplicate = (NewStrength > ReplThreshold) or Replicated,
	{reply, {ok, ShouldReplicate}, StrategyState#strategy_state{strength=NewStrength}}.

handle_cast({decay}, 
	{#strategy_state{key=Key, strength=Strength, replicated=Replicated, 
	params=#strategy_params{rmv_threshold=RmvThreshold, decay_factor=DecayFactor}}=StrategyState}) ->
	% Time decay
	NewStrength = decrStrength(Strength, DecayFactor),
	ShouldStopReplicate = (RmvThreshold > NewStrength) and Replicated,
	% Notify replication manager if replica should not longer be replicated
	if ShouldStopReplicate ->
		replication_manager:remove_replica(Key)
	end,
	{noreply, StrategyState#strategy_state{strength=NewStrength}};

handle_cast({stop}, State) ->
	{stop, normal, State}.

%% @doc Does nothing.
handle_info(_Msg, State) ->
	{noreply, State}.

%% @doc Does nothing.
terminate(_Reason, _State) ->
	ok.

%% @doc Does nothing. No change planned yet.
code_change(_PreviousVersion, State, _Extra) ->
	% The function is there for the behavior, but will not be used. 
	{ok, State}.


%% =============================================================================
%% Internal functions
%% =============================================================================

%% @doc Decrements the strength.
-spec decrStrength(float(), float()) -> float().
decrStrength(Strength, Decay) ->
	max(Strength - Decay, 0).

%% @doc Increments the strength by the specified amount and returns the new strength.
-spec incStrength(float(), float(), float()) -> float().
incStrength(Strength, Inc, MaxStrength) ->
	min(Strength + Inc, MaxStrength).


