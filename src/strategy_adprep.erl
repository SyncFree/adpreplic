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
%% @doc Provides operations required in a database.
-module(strategy_adprep).
-author('aas@trifork.co.uk').

-ifdef(EUNIT).
-compile(export_all).
-else.
-compile(report).
% StrateInterface for decay
-export([decay/2]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-endif.
-behaviour(gen_server).


-include("adprep.hrl").
-include("strategy_adprep.hrl").


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
        {yes} ->
            % With Replica
            {true, ReplicationThreshold + WStrength};
        {no} ->
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

%% @spec decay(Key::atom(), Id::integer()) -> {ok}
%%
%% @doc Sends a decay message. Not to be used directly but from decay.erl.
decay(Key, Id) ->
    gen_server:cast(Key, {decay, Id}).

%% =============================================================================
%% Messages handler
%% =============================================================================

handle_cast(shutdown, {Key, Replicated, Strength, DecayTime, MinNumReplicas, 
                       ReplicationThreshold, RmvThreshold, MaxStrength, Decay, WDecay,
                       RStrength, WStrength}) ->
%    lager:info("Shutting down the replication layer"),
    % Stop the decay process
    decay:stopDecay(Key),
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
               WStrength}}.

handle_call({create, Value}, _From, {Key, Replicated, Strength, DecayTime, 
                                     MinNumReplicas, ReplicationThreshold, RmvThreshold, 
                                     MaxStrength, Decay, WDecay, RStrength, WStrength}) ->
    NextDCsFunc = fun nextDCsFunc/3,
    Result = adprep:create(Key, Value, NextDCsFunc, MinNumReplicas),
    {Replicated1, Strength1} = case Result of
        {ok} ->
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
    {Replicated1, Strength1, ReplyMsg} = read(Key, Replicated, Strength, ReplicationThreshold, RStrength, MaxStrength),
    {reply, ReplyMsg, 
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
    adprep:update(Key, Value),
    Strength1 = Strength - WDecay,
    Replicated1 = processStrength(Key, Replicated, Strength1, MinNumReplicas, RmvThreshold),
    {reply, adpreps_:buildReply(update, {ok}), 
     {Key, Replicated1, Strength1, DecayTime, MinNumReplicas, ReplicationThreshold, 
      RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}};

handle_call({delete}, _From, {Key, Replicated, Strength, DecayTime, MinNumReplicas, 
                              ReplicationThreshold, RmvThreshold, MaxStrength, Decay, 
                              WDecay, RStrength, WStrength}) ->
    case adprep:delete(Key) of
        {ok} -> 
            gen_server:cast(self(), shutdown),
            {reply, adpreps_:buildReply(delete, {ok}), 
             {Key, false, 0, DecayTime, MinNumReplicas, ReplicationThreshold, 
              RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}};
        Result ->
            {reply, adpreps_:buildReply(delete, {error, Result}), 
             {Key, Replicated, Strength, DecayTime, MinNumReplicas, ReplicationThreshold, 
              RmvThreshold, MaxStrength, Decay, WDecay, RStrength, WStrength}}
    end.


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
                    Response = adprep:remove(Key, VerifyRemove, MinNumReplicas),
                    if
                        Response == {ok} ->
                            adpreps_:stop(Key),
                            false;
                        true ->
                            Replicated
                    end;
                Strength =< RmvThreshold ->
                    % Remove the current replica, but don't stop
                    VerifyRemove = fun verifyRemove/2,
                    Response = adprep:remove(Key, VerifyRemove, MinNumReplicas),
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
        Strength1 =< 0 ->
            % Stop this process
            adpreps_:stop(Key),
            erlang:yield(), % give a chance to shutdown
            false;
        true ->
            % Data is not replicated
            Replicated
    end,
    {Replicated1, Strength1}.

verifyRemove(Record, MinNumReplicas) ->
    #replica{num_replicas=NumReplicas}=Record,
    NumReplicas > MinNumReplicas.

%% @spec read(Key::atom(), Replicated::boolean(), Strength::float(), ReplicationThreshold::float(), RStrength::float(), MaxStrength::float()) -> {Replicated1::boolean(), Strength1::float()}
%% 
%% @doc Reads the specified data, irrespective of where it is located.
read(Key, Replicated, Strength, ReplicationThreshold, RStrength, MaxStrength) ->
    % Calculate new strength
    Strength1 = incStrength(Strength, RStrength, MaxStrength),
    Replicated1 = if
        Replicated == true ->
            % Already replicated
            Result = adprep:read(Key),
            Replicated;
        true ->
            % Continue processing based on new strength
            if 
                Strength1 > ReplicationThreshold -> 
                    % Create replica
                    Result = adprep:create(Key),
                    true;
                true ->
                    % Not replicated
                    Result = adprep:read(Key),
                    Replicated
            end
    end,
    {Replicated1, Strength1, adpreps_:buildReply(read, Result)}.

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
    {Replicated1, Strength1, adpreps_:buildReply(write, Result)}.

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
