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
-export([notify_decay/1]).
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
    gen_server:call(Pid, decay, infinity).

%% =============================================================================
%% Gen_server callbacks.
%% =============================================================================
%% @spec init({Key::atom(), Args::tuple()}) -> {ok, LoopData::tuple()}
%%
%% @doc Initializes the process and start the process 
%%      with the specified arguments.
init({Key, Value, 
		#adpargs{ 	decay_time 		 = DecayTime, 
				  	min_num_replicas = MinNumReplicas, 
				  	replication_threshold = ReplicationThreshold,
				  	rmv_threshold	 = RmvThreshold,
				  	max_strength	 = MaxStrength, 
					decay 			 = Decay,
					wdecay 			 = WDecay,
					rstrength 		 = RStrength,
					wstrength 		 = WStrength}}) ->
	{ok, Replicated} = dcs:replicated(Key),
	ok = datastore:create(Key,Value),
	% Calculate strength of the replica
	Strength = case Replicated of 
		true  -> ReplicationThreshold + WStrength;
		false -> 0
	end,
	{ok, Timer} = decay:startDecayTimer(DecayTime, self(), none),
	{ok, {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}, Timer}.

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
	% The function is there for the behaviour, but will not be used. Only a version on the
	% next
	{ok, State}.

%% =============================================================================
%% Messages handlers
%% =============================================================================
handle_cast({decay, _Id}, {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}) ->
	% Time decay
	Strength1 = Strength - Decay,
	Replicated1 = processStrength(Key, Replicated, Strength1, MinNumReplicas, RmvThreshold),
	{noreply, {Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}};

handle_cast({stop, _Dc, _Id}, LoopData) ->
	{stop, normal, LoopData}.

handle_call({write, Id, Value}, _From, {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}) ->
	{Replicated1, Strength1, Result} = write(Key, Id, Value, Replicated, Strength, ReplicationThreshold, WStrength, MaxStrength),
	{reply, dcs:buildReply(write, Id, Result), {Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}};

handle_call({create, Id, Value}, _From, {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}) ->
	NextDCsFunc = fun nextDCsFunc/3,
	Result = dcs:create(Key, {Value, NextDCsFunc, MinNumReplicas}),
	{Replicated1, Strength1} = case Result of
		{ok} ->
			% Successful creation implies the data has been replicated
			{true, ReplicationThreshold + WStrength};
		true ->
			{Replicated, Strength}
	end,
	{reply, dcs:buildReply(create, Id, Result), {Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}};

handle_call({read, Id}, _From, {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}) ->
	{Replicated1, Strength1, ReplyMsg} = read(Key, Id, Replicated, Strength, ReplicationThreshold, RStrength, MaxStrength),
	{reply, ReplyMsg, {Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}};

handle_call({update, Id, Value}, _From, {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}) ->
	% Should only come from another DC. Maybe it should be cheked before it is processed
	dcs:updates(Key, Value),
	Strength1 = Strength - WDecay,
	Replicated1 = processStrength(Key, Replicated, Strength1, MinNumReplicas, RmvThreshold),
	{reply, dcs:buildReply(update, Id, {ok}), {Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}}.

%% @spec processStrength(Key::atom(), Replicated::boolean(), Strength::float(), MinNumReplicas::float(), RmvThreshold::float()) -> {Replicated1::boolean(), Strength::float()}
%% 
%% @doc Processes the forwarded message.
processStrength(Key, Replicated, Strength, MinNumReplicas, RmvThreshold) ->
	% Make sure that the Strength does no goes under zero
	Strength1 = if
		Strength < 0 ->
			0;

		true ->
			Strength
	end,
	Replicated1 = if
		Replicated -> 
			% It was replicated before
			if 
				Strength =< 0 ->
					% Remove the current replica and stop this process
					Response = dcs:rmvFromReplica(Key, MinNumReplicas),
					if
						Response == {ok} ->
							adpreps_:stop(Key),
                    		false;

						true ->
							Replicated
					end;

				Strength =< RmvThreshold ->
					% Remove the current replica, but don't stop
					Response = dcs:rmvFromReplica(Key, MinNumReplicas),
					if
						Response == {ok} ->
                    		false;

						true ->
							Replicated
					end;

				true ->
					% Data is still replicated
					true
			end;

		Strength =< 0 ->
			% Stop this process
			adpreps_:stop(Key),
            false;

		true ->
			% Data is not replicated
			Replicated
	end,
	{Replicated1, Strength1}.

%% @spec read(Key::atom(), Id::integer(), Replicated::boolean(), Strength::float(), ReplicationThreshold::float(), RStrength::float(), MaxStrength::float()) -> {Replicated1::boolean(), Strength1::float()}
%% 
%% @doc Reads the specified data, irrespective of where it is located.
read(Key, Id, Replicated, Strength, ReplicationThreshold, RStrength, MaxStrength) ->
	% Calculate new strength
	Strength1 = incStrength(Strength, RStrength, MaxStrength),
	% Continue processing based on new strength
	Replicated1 = if 
		Strength1 > ReplicationThreshold -> 
			% Create replica
			Result = dcs:createReplica(Key),
			true;

		true ->
            % Already replicated or not
			Result = dcs:read(Key),
            Replicated
	end,
	{Replicated1, Strength1, dcs:buildReply(read, Id, Result)}.

%% @spec write(Key::atom(), Id::integer(), Value, Replicated::boolean(), Strength::float(), ReplicationThreshold::float(), WStrength::float(), MaxStrength::float()) -> {Replicated1::boolean(), Strength1::float()}
%% 
%% @doc Writes the new value of the specified data and take appropiate action to update 
%%		replicated sites (DCs).
write(Key, Id, Value, Replicated, Strength, ReplicationThreshold, WStrength, MaxStrength) ->
	% Calculate new strength
	Strength1 = incStrength(Strength, WStrength, MaxStrength),
	% Continue processing based on new strength
	Replicated1 = if 
		Strength1 > ReplicationThreshold -> 
			% Create replica
			Result = dcs:createReplica(Key, Value),
			true;

		true ->
			% Already replicated or not
			Result = dcs:write(Key, Value),
			Replicated
	end,
	{Replicated1, Strength1, dcs:buildReply(write, Id, Result)}.

%% @spec incStrength(Strength::float(), Inc::float(), MaxStrength::float()) -> Strength1::float()
%% 
%% @doc Increments the strength by the specified amount and returnst the new strength.
incStrength(Strength, Inc, MaxStrength) ->
	Strength1 = Strength + Inc,
	if
		Strength1 > MaxStrength ->
			MaxStrength;
		true ->
			Strength1
	end.

%% @spec nextDCsFunc(Dc::atom(), AllDCs::List, MinNumReplicas::integer()) -> Result::tuple()
%% 
%% @doc Builds the list of DCs where a replica will be created from within those in the 
%%		provides list of all DCs, except fro specified DC, and another to select from in 
%%		case any of the previous fail.
nextDCsFunc(Dc, AllDCs, MinNumReplicas) -> 
	nextDCsFunc_(Dc, AllDCs, MinNumReplicas, []).

nextDCsFunc_(Dc, [D | AllDCs], MinNumReplicas, ListDCs) -> 
	if
		MinNumReplicas > 0 ->
			{MinNumReplicas1, ListDCs1} = if
				D == Dc ->
					% Ignore
					{MinNumReplicas, ListDCs};
				true ->
					{MinNumReplicas-1, [D | ListDCs]}
			end,
			nextDCsFunc_(Dc, AllDCs, MinNumReplicas1, ListDCs1);
		true ->
			{ListDCs, AllDCs}
	end;
nextDCsFunc_(_Dc, [], _MinNumReplicas, List) -> 
	% No sufficient DCs, full replication
	{List, []}.
