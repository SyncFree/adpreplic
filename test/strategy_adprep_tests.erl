%% =============================================================================
%% EUnits for the user interface - SyncFree
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

-module(strategy_adprep_tests).
-author('aas@trifork.co.uk').

%% =============================================================================
%% API functions
%% =============================================================================
-compile(export_all).
-export([]).


-include_lib("eunit/include/eunit.hrl").
-include("strategy_adprep.hrl").


%% =============================================================================
%% Internal test functions
%% =============================================================================

removeByDecay_test() ->
    % Initialise
    Key = 'read_test',
    DelayName = decay:buildPid(Key),
    Value = "VALUE",
    DecayTime = 100,
    initialise(),
    Args = #adpargs{decay_time = DecayTime,
                    min_num_replicas = 1,
                    replication_threshold = 1.0,
                    rmv_threshold = 0.0,
                    max_strength = 1.5,
                    decay = 2.0,
                    wdecay = 0.5,
                    rstrength = 1.0,
                    wstrength = 1.5},
    adpreplic:create(Key, Value, adprep, Args), % start Strategy
    timer:sleep(50),
    % Test
    Reg = registered(),
    ?assertEqual(true, lists:member(Key, Reg)),
    ?assertEqual(true, lists:member(DelayName, Reg)),
    timer:sleep(2 * DecayTime), % wait for the strategy to shutdown
    Reg1 = registered(),
%?debugFmt("Processes: ~64p~n", [Reg1]),
    ?assertEqual(false, lists:member(DelayName, Reg1)),
    ?assertEqual(false, lists:member(Key, Reg1)),
    stop(Key).

createAlreadyReplica_test() ->
    % Initialise
    Key = 'read_test',
    DelayName = decay:buildPid(Key),
    Value = "VALUE",
    DecayTime = 100,
    initialise(),
    NextDCFunc = fun(_Rl, _AllDCs, _Args) -> {[], []} end,
    Result = adprep:create(Key, Value, NextDCFunc, []),
    ?assertEqual(ok, Result),
    Result1 = adprep:hasReplica(Key),
    ?assertEqual(true, Result1),
    Args = #adpargs{decay_time = DecayTime,
                    min_num_replicas = 1,
                    replication_threshold = 1.0,
                    rmv_threshold = 0.0,
                    max_strength = 1.5,
                    decay = 2.0,
                    wdecay = 0.5,
                    rstrength = 1.0,
                    wstrength = 1.5},
    % Test - already replica but strategy not running yet
    Reg = registered(),
    ?assertEqual(false, lists:member(Key, Reg)),
    ?assertEqual(false, lists:member(DelayName, Reg)),
    Result2 = adpreplic:create(Key, Value, adprep, Args), % start Strategy
    ?assertEqual({error,already_exists_replica}, Result2),
    Reg1 = registered(),
    ?assertEqual(true, lists:member(Key, Reg1)),
    ?assertEqual(true, lists:member(DelayName, Reg1)),
    stop(Key).

invalidMsg_test() ->
    % Initialise
    initialise(),
    Value = "VALUE",
    Key = 'invalidMsg_test',
    DecayTime = 100,
    Args = #adpargs{decay_time = DecayTime,
                    min_num_replicas = 1,
                    replication_threshold = 1.0,
                    rmv_threshold = 0.0,
                    max_strength = 1.5,
                    decay = 2.0,
                    wdecay = 0.5,
                    rstrength = 1.0,
                    wstrength = 1.5},
    adpreplic:create(Key, Value, adprep, Args), % start Strategy
    timer:sleep(50),
    % Test
    try gen_server:call(Key, Key, 500) of
        R ->
            ?assertEqual(ok, R)
    catch
        exit:{Type,_} ->    
            ?assertEqual(normal, Type)
    end,
    gen_server:cast(Key, Key),
    receive
        Response ->
            ?assertEqual([], Response)
    after
        500 ->
            Result = ok,
            ?assertEqual(ok, Result)
    end,
    try Key ! Key of
        Reply ->
            ?assertEqual(ok, Reply)
    catch
        error:Type1 ->
            ?assertEqual(badarg, Type1)
    end,
    % Clean-up
    stop(Key).


%% =============================================================================
%% Internal functions
%% =============================================================================
initialise() ->
    adprep_tests:initialise(),
    erlang:yield().

stop(Key) ->
    io:format("(startegy_adprep_tests): Stopping strategy~n", []),
    adpreps_:stop(Key),
    erlang:yield(),
    adprep_tests:stop().
