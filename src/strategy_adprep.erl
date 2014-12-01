%% =============================================================================
%% Adaptive Replication Strategy - SyncFree
%%
%% Any strategy must implement the function run(Key, DCs, Args) and process the 
%% received messages.
%%
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%%
%% @doc Provides operations required in a database.
-module(strategy_adprep).
-author('aas@trifork.co.uk').

-ifdef(EUNIT).
-compile(export_all).
-else.
-compile(report).
-export([run/3]).
-endif.


%% =============================================================================
%% Propossed Adaptive Replication Strategy process
%% =============================================================================
%% @spec run(Key::atom(), DC::list(), Args::tuple()) -> Result::tuple()
%%
%% @doc Processes the messaged from the mailbox base on the specified arguments and the given data 
%%		(its key).
run(Key, DCs, Args) ->
    Result = dcs:setDCsReplica(Key, DCs),
	{DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength} = Args,
	% Calculate strength of the replica
	{Replicated, Strength} = case Result of
		{ok, Replicated1} ->
			if 
				Replicated1 -> 
					% With Replica
					{Replicated1, ReplicationThreshold + WStrength};

				true ->
					% No replica
					{Replicated1, 0}
			end;

		true ->
			% No replica
			{false, 0}
	end,
	decay:startDecay(DecayTime, Key, false),
	run(Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength).

%% @spec run(Key::atom(), Replicated::boolean(), Strength::float(), DecayTime::integer(), MinNumReplicas::integer(), ReplicationThreshold::float(), RmvThreshold::float(), MaxStrength::float(), Decay::float(), WDecay::float(), RStrength::float(), WStrength::float()) -> Result::tuple()
%%
%% @doc Processes the messaged from the mailbox. ReplicationThreshold > RmvThreshold >= 0.
run(Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength) ->
	% Process the messages in the mailbox. Message of the format {Type, Pid, Id[, Value]} or {Type} 
	% only for decay
	receive
		{rmv_replica, Dc, _Id} -> 
			dcs:rmvReplica(Key, Dc),
			run(Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength);

		{new_replica, Dc, _Id} ->
			dcs:newReplica(Key, Dc),
			run(Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength);

		{write, Pid, Id, Value} -> 
			{Replicated1, Strength1} = write(Key, Pid, Id, Value, Replicated, Strength, ReplicationThreshold, WStrength, MaxStrength),
			run(Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength);

		{create, Pid, Id, V} -> 
			Result = dcs:create(Key, V),
			dcs:sendReply(Pid, create, Id, Result),
			{Replicated1, Strength1} = case Result of
				{ok} ->
					% Successful creation implies tha data has been replicated
					{true, ReplicationThreshold + WStrength};

				true ->
					{Replicated, Strength}
			end,
			run(Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength);

		{read, Pid, Id} -> 
			{Replicated1, Strength1} = read(Key, Pid, Id, Replicated, Strength, ReplicationThreshold, RStrength, MaxStrength),
			run(Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength);

		{decay} ->
			% Time decay
			{Replicated1, Strength1} = decay(Key, Replicated, Strength, MinNumReplicas, RmvThreshold, Decay),
			run(Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength);

		{has_replica, Dc, Id} -> 
			% Only process if there is a replica
			if 
				Replicated -> 
					% It has a replica
					dcs:sendReply(Dc, has_replica, Id, {ok, {adprep, dcs:getDCsReplica(Key), [DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength]}})
			end,
            run(Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength);

		{update, _Dc, _Id, Value} -> 
			% Should only come from another DC. Maybe it should be cheked before it is processed
			{Replicated1, Strength1} = update(Key, Value, Replicated, Strength, MinNumReplicas, RmvThreshold, WDecay),
			run(Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength);

		{forward, Type, Pid, Id, Msg} -> 
			% Should only come from another DC. Maybe it should be cheked before it is processed
			forward(Key, Type, Pid, Id, Msg, Replicated),
			run(Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength);

		{stop, _Pid, _Id} ->
			stop(Key)
	end.

%% =============================================================================
%% Support functions
%% =============================================================================
%% @spec decay(Key::atom(), Replicated::boolean(), Strength::float(), MinNumReplicas::float(), RmvThreshold::float(), Decay::float()) -> {Replicated1::boolean(), Strength1::float()}
%% 
%% @doc Processes the decay message.
decay(Key, Replicated, Strength, MinNumReplicas, RmvThreshold, Decay) ->
	% Time decay
	Strength1 = Strength - Decay,
	processStrength(Key, Replicated, Strength1, MinNumReplicas, RmvThreshold).

%% @spec forward(Key::atom(), Type::atom(), Pid::pid(), Id::integer(), Msg, Replicated::bolean()) -> {ok}
%% 
%% @doc Builds the forward message and send it to the specified process.
forward(Key, Type, Pid, Id, Msg, Replicated) -> 
    {Id, Respose} = if
        Replicated ->
			% With replica
            case Msg of 
                {read, _P, Id1} -> 
                    Value = dcs:read(Key),
                    {Id1, Value};

                {write, _P, Id1, V} ->
                    Value = dcs:updates(Key, V),
                    {Id1, Value};

                _ ->
                	{Type, _P, Id1, _} = Msg,
               		{Id1, {error, unknown_msg}}
            end;

		true ->
			% No replica
            Id1 = case Msg of 
                {read, _P, Id2} -> 
                    Id2;

                {write, _P, Id2, _V} ->
                    Id2;

                _ ->
                	{Type, _P, Id2, _} = Msg,
               		Id2
            end,
            {Id1, {error, no_replica}}
    end,
    Pid ! {replay, Type, self(), Id, Respose}.

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

%% @spec read(Key::atom(), Pid::pid(), Id::integer(), Replicated::boolean(), Strength::float(), ReplicationThreshold::float(), RStrength::float(), MaxStrength::float()) -> {Replicated1::boolean(), Strength1::float()}
%% 
%% @doc Reads the specified data, irrespective of where it is located.
read(Key, Pid, Id, Replicated, Strength, ReplicationThreshold, RStrength, MaxStrength) ->
	% Calculate new strength
	Strength2 = Strength + RStrength,
	Strength1 = if
		Strength2 > MaxStrength ->
			MaxStrength;
		true ->
			Strength2
	end,
	% Process based on new strength
	Replicated1 = if 
		Replicated -> 
			% Already replicated
			Result = dcs:read(Key),
			Replicated;

		Strength1 > ReplicationThreshold -> 
			% Create replica
			Result = dcs:createReplica(Key),
			true;

		true ->
            % Get value from other DC
            Result = dcs:forwardMsg(Key, {read, Pid, Id}),
            Replicated
	end,
	dcs:sendReply(Pid, read, Id, Result),
	{Replicated1, Strength1}.

%% @spec stop(Key::atom()) -> {ok}
%%
%% @doc Stops the process associated to the data. No replay is sent to sender.
stop(Key) -> 
	decay:stopDecay(Key),
	%% Response with error all the messages currently in the mailbox
	%% TODO: Maybe forward current messages to other DC?
	flush(Key),
	{ok}.

%% @spec flush(Key::atom()) -> {ok}
%%
%% @doc Removes all pre-existing messages in the mailbox and replies to those that require a 
%%		response.
flush(Key) ->
	receive
		{read, Pid, Id} -> 
			dcs:sendReply(Pid, read, Id, {error, stopped}), 
			flush(Key);
		{write, Pid, Id, _Value} ->
			dcs:sendReply(Pid, write, Id, {error, stopped}), 
			flush(Key);
		{forward, Type, Pid, Id, _Msg} -> 
			dcs:sendReply(Pid, Type, Id, {error, stopped}), 
			flush(Key);
		{create, Pid, Id, _Value} -> 
			dcs:sendReply(Pid, create, Id, {error, stopped}), 
			flush(Key);
		{decay} ->
			flush(Key);
		{has_replica, _Dc, _Id} -> 
			flush(Key);
		{update, _Dc, _Id, _Value} -> 
			flush(Key);
		{rmv_replica, _Dc, _Id} -> 
			flush(Key);
		{new_replica, _Dc, _Id} ->
			flush(Key);
		{stop, _Pid, _Id} ->
			flush(Key)
	after
		0 ->
			{ok}
	end.

%% @spec update(Key::atom(), Value, Replicated::boolean(), Strength::float(), MinNumReplicas::float(), RmvThreshold::float(), WDecay::float()) -> {Replicated1::boolean(), Strength1::float()}
%% 
%% @doc Updates only this replica and may request to stop process.
update(Key, Value, Replicated, Strength, MinNumReplicas, RmvThreshold, WDecay) -> 
	dcs:updates(Key, Value),
	Strength1 = Strength - WDecay,
	processStrength(Key, Replicated, Strength1, MinNumReplicas, RmvThreshold).

%% @spec write(Key::atom(), Pid::pid(), Id::integer(), Value, Replicated::boolean(), Strength::float(), ReplicationThreshold::float(), WStrength::float(), MaxStrength::float()) -> {Replicated1::boolean(), Strength1::float()}
%% 
%% @doc Writes the new value of the specified data and take appropiate action to update replicated sites (DCs).
write(Key, Pid, Id, Value, Replicated, Strength, ReplicationThreshold, WStrength, MaxStrength) ->
	Strength2 = Strength + WStrength,
	Strength1 = if
		Strength2 > MaxStrength ->
			MaxStrength;
		true ->
			Strength2
	end,
	Replicated1 = if 
		Replicated -> 
			Result = dcs:write(Key, Value),
         	Replicated;

		Strength1 > ReplicationThreshold -> 
			% Create replica
			Result = dcs:createReplica(Key, Value),
			true;

		true ->
			% Get value from other DC
			Result = dcs:forwardMsg(Key, {write, Pid, Id, Value}),
			Replicated
	end,
	dcs:sendReply(Pid, write, Id, Result),
	{Replicated1, Strength1}.
