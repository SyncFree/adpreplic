%% =============================================================================
%% First Propossed Adaptive Replication Strategy - SyncFree
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
%% @doc Provides operations required to control the location of the data replicas within 
%%      the overall system.
-module(strategy_adprep).
-author('aas@trifork.co.uk').

-ifdef(EUNIT).
% Unit-test
-compile(export_all).
-else.
-compile(report).
% Strate interface for decay
-export([decay/2]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-endif.
-behaviour(gen_server).


-include("adprep.hrl").
-include("strategy_adprep.hrl").


%% @doc Sends a decay message. Not to be used directly but from decay.erl.
-spec decay(key(), integer()) -> ok.
decay(Key, Id) ->
    gen_server:cast(Key, {decay, Id}).


%% =============================================================================
%% Propossed Adaptive Replication Strategy process
%% =============================================================================

%% @spec init({Key::atom(), Args::tuple()}) -> {ok, LoopData::tuple()}
%%
%% @doc Initialises the process and start the process with the specified arguments.
init({Key, #adpargs{decay_time=DecayTime, 
                    min_num_replicas=MinNumReplicas, 
                    replication_threshold=ReplicationThreshold,
                    rmv_threshold=RmvThreshold,
                    max_strength=MaxStrength, 
                    decay=Decay,
                    wdecay=WDecay,
                    rstrength=RStrength,
                    wstrength=WStrength}}) ->
    Result = adprep:hasReplica(Key),
    % Calculate strength of the replica
    {Replicated, Strength} = case Result of
        true ->
            % With Replica
            {true, ReplicationThreshold + WStrength};
        false ->
            % No replica
            {false, 0}
    end,
    decay:startDecay(DecayTime, Key, false),
    {ok, {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}}.

%% @spec handle_info(Msg, LoopData) -> {noreply, LoopData}
%%
%% @doc Does nothing.
handle_info(_Msg, LoopData) ->
    {noreply, LoopData}.

%% @spec terminate(Reason, LoopData) -> ok
%%
%% @doc Does nothing.
terminate(_Reason, _LoopData) ->
    ok.

%% @spec code_change(PreviousVersion, State, Extra) -> {ok, State}
%%
%% @doc Does nothing. No change planned yet.
code_change(_PreviousVersion, State, _Extra) ->
    % The function is there for the behaviour, but will not be used. Only a version on the
    % next
    {ok, State}.


%% =============================================================================
%% Messages handler
%% =============================================================================

%% @spec handle_cast(Msg, Args::tuple()) -> Result::tuple()
%%
%% @doc Handles the passed message.
handle_cast(shutdown, {Key, Replicated, Strength, DecayTime, MinNumReplicas, 
                       ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay,
                       RStrength, WStrength}) ->
%    lager:info("Shutting down the replication layer"),
    % Stop the decay process
    ok = decay:stopDecay(Key),
    {stop, normal, {Key, Replicated, Strength, DecayTime, MinNumReplicas, 
                    ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay,
                    RStrength, WStrength}};

handle_cast({decay, _Id}, {Key, Replicated, Strength, DecayTime, MinNumReplicas, 
                           ReplicationThreshold, RmvThreshold, MaxStrength, Decay, 
                           WDecay, RStrength, WStrength}) ->
    % Time decay
    Strength1 = Strength - Decay,
    Replicated1 = processStrength(Key, Replicated, Strength1, MinNumReplicas, RmvThreshold),
    {noreply, {Key, Replicated1, Strength1, DecayTime, MinNumReplicas, 
               ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay, RStrength, 
               WStrength}};

handle_cast(_Msg, LoopData) ->
    {noreply, LoopData}.

handle_call({create, Value}, _From, {Key, Replicated, Strength, DecayTime, 
                                     MinNumReplicas, ReplicationThreshold, RmvThreshold, 
                                     MaxStrength, Decay, WDecay, RStrength, WStrength}) ->
    NextDCsFunc = fun nextDCsFunc/3,
    Result = adprep:create(Key, Value, NextDCsFunc, MinNumReplicas),
    {Replicated1, Strength1} = case Result of
        ok ->
            % Successful creation implies the data has been replicated
            {true, ReplicationThreshold + WStrength};
        _ ->
            {Replicated, Strength}
    end,
    {reply, adpreps_:buildReply(create, Result), 
     {Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, 
      RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}};

handle_call({read}, _From, {Key, Replicated, Strength, DecayTime, MinNumReplicas, 
                            ReplicationThreshold, RmvThreshold, MaxStrength, Decay, 
                            WDecay, RStrength, WStrength}) ->
    {Replicated1, Strength1, Result} = read(Key, Replicated, Strength, ReplicationThreshold, RStrength, MaxStrength),
    {reply, Result, 
     {Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, 
      RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}};

handle_call({write, Value}, _From, {Key, Replicated, Strength, DecayTime, MinNumReplicas, 
                                    ReplicationThreshold, RmvThreshold, MaxStrength, 
                                    Decay, WDecay, RStrength, WStrength}) ->
    {Replicated1, Strength1, Result} = write(Key, Value, Replicated, Strength, ReplicationThreshold, WStrength, MaxStrength),
    {reply, Result, 
     {Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, 
      RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}};

handle_call({update, Value}, _From, {Key, Replicated, Strength, DecayTime, MinNumReplicas, 
                                     ReplicationThreshold, RmvThreshold, MaxStrength, 
                                     Decay, WDecay, RStrength, WStrength}) ->
    % Should only come from another DC. Maybe it should be cheked before it is processed
    % TODO: What to do in case of error?
    ok = adprep:update(Key, Value),
    Strength1 = Strength - WDecay,
    Replicated1 = processStrength(Key, Replicated, Strength1, MinNumReplicas, RmvThreshold),
    {reply, adpreps_:buildReply(update, ok), 
     {Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, 
      RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}};

handle_call({delete}, _From, {Key, Replicated, Strength, DecayTime, MinNumReplicas, 
                              ReplicationThreshold, RmvThreshold, MaxStrength, Decay, 
                              WDecay, RStrength, WStrength}) ->
    case adprep:delete(Key) of
        ok -> 
            gen_server:cast(self(), shutdown),
            {reply, adpreps_:buildReply(delete, ok), 
             {Key, false, 0, DecayTime, MinNumReplicas, ReplicationThreshold, 
              RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}};
        Result ->
            {reply, adpreps_:buildReply(delete, {error, Result}), 
             {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, 
              RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}}
    end;

handle_call(_Msg, _From, LoopData) ->
    {noreply, LoopData}.


%% =============================================================================
%% Support functions
%% =============================================================================

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
                    VerifyRemove = fun verifyRemove/2,
                    case adprep:remove(Key, VerifyRemove, MinNumReplicas) of
                        ok ->
                        % TODO Fix the error case here!
                            _ = adpreps_:stop(Key),
                            false;
                        _ ->
                            Replicated
                    end;
                Strength =< RmvThreshold ->
                    % Remove the current replica, but don't stop
                    VerifyRemove = fun verifyRemove/2,
                    case adprep:remove(Key, VerifyRemove, MinNumReplicas) of
                        ok ->
                            false;
                        _ ->
                            Replicated
                    end;
                true ->
                    % Data is still replicated
                    true
            end;
        Strength1 =< 0 ->
            % Stop this process
            % TODO Fix the error case here!
            _ = adpreps_:stop(Key),
            erlang:yield(), % give a chance to shutdown
            false;
        true ->
            % Data is not replicated
            Replicated
    end,
    {Replicated1, Strength1}.

%% @spec verifyRemove(Record::record(), MinNumReplicas::integer()) ->Result::boolean()
%% 
%% @doc Checks if the current state of the system allows the current DC to delete its 
%%      replica of the data.
verifyRemove(Record, MinNumReplicas) ->
    #replica{num_replicas=NumReplicas}=Record,
    NumReplicas > MinNumReplicas.

%% @spec read(Key::atom(), Replicated::boolean(), Strength::float(), ReplicationThreshold::float(), RStrength::float(), MaxStrength::float()) -> {Replicated1::boolean(), Strength1::float(), Reply}
%% 
%% @doc Reads the specified data, irrespective of where it is located.
read(Key, Replicated, Strength, ReplicationThreshold, RStrength, MaxStrength) ->
    % Calculate new strength
    Strength1 = incStrength(Strength, RStrength, MaxStrength),
    {Replicated1, Result1} = case Replicated of
        true ->
            % Already replicated
            Result = adprep:read(Key),
            {Replicated, Result};
        _ ->
            % Continue processing based on new strength
            if 
                Strength1 > ReplicationThreshold -> 
                    % Create replica
                    Result = adprep:create(Key),
                    {true, Result};
                true ->
                    % Not replicated
                    Result = adprep:read(Key),
                    {Replicated, Result}
            end
    end,
    {Replicated1, Strength1, Result1}.

%% @spec write(Key::atom(), Value, Replicated::boolean(), Strength::float(), ReplicationThreshold::float(), WStrength::float(), MaxStrength::float()) -> {Replicated1::boolean(), Strength1::float()}
%% 
%% @doc Writes the new value of the specified data and take appropiate action to update 
%%        replicated sites (DCs).
write(Key, Value, Replicated, Strength, ReplicationThreshold, WStrength, MaxStrength) ->
    % Calculate new strength
    Strength1 = incStrength(Strength, WStrength, MaxStrength),
    Replicated1 = if
        Replicated == true ->
            % Already replicated
            Result = adprep:update(Key, Value),
            Replicated;
        true ->
            % Continue processing based on new strength
            if 
                Strength1 > ReplicationThreshold -> 
                    % Create replica
                    Result = adprep:create(Key, Value),
                    true;
                true ->
                    % Not replicated
                    Result = adprep:update(Key, Value),
                    Replicated
            end
    end,
    {Replicated1, Strength1, Result}.

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
%%      provides list of all DCs, except fro specified DC, and another to select from in 
%%      case any of the previous fail.
nextDCsFunc(Dc, AllDCs, MinNumReplicas) -> 
    nextDCsFunc_(Dc, AllDCs, MinNumReplicas - 1, []).

%% @spec nextDCsFunc_(Dc::node(), AllDCs::list(), MinNumExtrReplicas::integer(), ListDCs::list()) -> {ListDCs::list(), AllDCs::list()}
%% 
%% @doc Builds a list of the specified min. number of DCs and an alternative in case of 
%%      errors when contacting any of them. 
nextDCsFunc_(Dc, [D | AllDCs], MinNumExtrReplicas, ListDCs) -> 
    if
        MinNumExtrReplicas > 0 ->
            {MinNumExtrReplicas1, ListDCs1} = if
                D == Dc ->
                    % Ignore
                    {MinNumExtrReplicas, ListDCs};
                true ->
                    {MinNumExtrReplicas - 1, [D | ListDCs]}
            end,
            nextDCsFunc_(Dc, AllDCs, MinNumExtrReplicas1, ListDCs1);
        true ->
            {ListDCs, AllDCs}
    end;
nextDCsFunc_(_Dc, [], _MinNumExtrReplicas, List) -> 
    % No sufficient DCs, full replication
    {List, []}.
