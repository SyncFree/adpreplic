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

%% =============================================================================
%% Public API.
%% =============================================================================

notify_decay(Pid) ->
    gen_server:cast(Pid, decay).

%% @doc Read to the local replica
local_write(Key) ->
    gen_server:call(Key, {write}, infinity).

%% @doc Update to the local replica
local_read(Key) ->
    gen_server:call(Key, {read}, infinity).

%% @doc Removing the local replica
%local_remove(Key) ->
%	gen_server:call(Key, {remove}, infinity).

%% =============================================================================
%% Gen_server callbacks.
%% =============================================================================
%% @spec init({Key::atom(), Args::tuple()}) -> {ok, LoopData::tuple()}
%%
%% @doc Initializes the process and start the process 
%%      with the specified arguments.
init({Key, Replicated,
		#strategyState{ 	
		            decay_time 		 = DecayTime, 
				  	min_num_replicas = MinNumReplicas, 
				  	replication_threshold = ReplicationThreshold,
				  	rmv_threshold	 = RmvThreshold,
				  	max_strength	 = MaxStrength, 
					decay 			 = Decay,
					wdecay 			 = WDecay,
					rstrength 		 = RStrength,
					wstrength 		 = WStrength}}) ->
	% Calculate strength of the replica
	Strength = case Replicated of 
		true  -> ReplicationThreshold + WStrength;
		false -> 0
	end,
	{ok, Timer} = decay:startDecayTimer(DecayTime, self(), none),
	{ok, {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength, Timer}}.

%% =============================================================================
%% Messages handlers
%% =============================================================================

handle_call({write}, _From, {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength, Timer}) ->
	%Update strength
	NewStrength = incStrength(Strength, WStrength, MaxStrength),
	% TODO: Install local replica if strength passes threshold
	StillReplicated =  NewStrength > ReplicationThreshold or Replicated,
	% TODO: Install local replica if strength passes threshold
	% Result = dcs:write(Key, Value),
		
	{reply, {ok}, {Key, StillReplicated, NewStrength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength, Timer}};



handle_call({read}, _From, {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength, Timer}) ->
	NewStrength = incStrength(Strength, RStrength, MaxStrength),
	StillReplicated =  NewStrength > ReplicationThreshold or Replicated,
	{reply, {ok}, {Key, StillReplicated, NewStrength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength, Timer}}.

% TODO: Inter_DC communication
% handle_call({update, Id, Value}, _From, {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}) ->
% 	% Should only come from another DC. Maybe it should be cheked before it is processed
% 	dcs:updates(Key, Value),
% 	Strength1 = Strength - WDecay,
% 	Replicated1 = processStrength(Key, Replicated, Strength1, MinNumReplicas, RmvThreshold),
% 	{reply, dcs:buildReply(update, Id, {ok}), {Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}}.

handle_cast({decay}, {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength,Timer}) ->
	% Time decay
	NewStrength = decrStrength(Strength, Decay),
	StillReplicated = RmvThreshold < NewStrength and Replicated,
	% Notify replication manager if replica should not longer be replicated
	replication_manager:remove_replica(Key),
	{noreply, {Key, StillReplicated, NewStrength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength,Timer}};

handle_cast({stop}, State) ->
	{stop, normal, State}.

%% @spec handle_info(Msg, LoopData) -> {noreply, LoopData}
%%
%% @doc Does nothing.
handle_info(_Msg, State) ->
	{noreply, State}.

%% @spec terminate(Reason, LoopData) -> ok
%%
%% @doc Does nothing.
terminate(_Reason, _State) ->
	ok.

%% @spec code_change(PreviousVersion, State, Extra) -> {ok, State}
%%
%% @doc Does nothing. No change planned yet.
code_change(_PreviousVersion, State, _Extra) ->
	% The function is there for the behaviour, but will not be used. 
	{ok, State}.


%% =============================================================================
%% Internal functions
%% =============================================================================

%% @doc Decrements the strength.
decrStrength(Strength, Decay) ->
	max(Strength - Decay, 0).

%% @doc Increments the strength by the specified amount and returns the new strength.
-spec incStrength(float(), float(), float()) -> float().
incStrength(Strength, Inc, MaxStrength) ->
	min(Strength + Inc, MaxStrength).


