%% =============================================================================
%% EUnits for Adapive Replications DC - SyncFree
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

-module(adprep_tests).
-author('aas@trifork.co.uk').

%% =============================================================================
%% API functions
%% =============================================================================
-compile(export_all).
-export([initialise/0,stop/0]).


-include_lib("eunit/include/eunit.hrl").
-include("strategy_adprep.hrl").


%% =============================================================================
%% Test functions
%% =============================================================================
getNumReplicas_test() ->
    % Initialise
    initialise(),
    Key = 'getNumReplicas_test',
    % Test - already running process
    NumReplicas = adprep:getNumReplicas(Key),
    ?assertEqual(0, NumReplicas),
    % Clean-up
    stop().

create_test() ->
    % Initialise
    initialise(),
    Key = 'create_test',
    Value = "value",
    % Test - does not exist
    Response = create(Key, Value),
    ?assertEqual(ok, Response),
    % Clean-up
    stop().

createExist_test() ->
    % Initialise
    initialise(),
    Key = 'create1_test',
    Value = "value",
    create(Key, Value),
    % Test - already exists
    Response1 = create(Key, Value),
    ?assertEqual({error, already_exists_replica}, Response1),
    Response2 = adprep:create(Key),
    ?assertEqual({error, already_exists_replica}, Response2),
    Response3 = adprep:create(Key, Value),
    ?assertEqual({error, already_exists_replica}, Response3),
    % Clean-up
    stop().

createNone_test() ->
    % Initialise
    initialise(),
    Key = 'createNone_test',
    Value = "VVVV",
    % Test - does not exist and cannot be created; missing value an no replica anywhere 
    %         else
    Response = adprep:create(Key),
    ?assertEqual({error, does_not_exist}, Response),
    Response1 = adprep:create(Key, Value),
    ?assertEqual(ok, Response1),
    % Clean-up
    stop().

read_test() ->
    % Initialise
    initialise(),
    Key = 'read_test',
    Value = "value1",
    % Test - does not exist
    Response = adprep:read(Key),
    ?assertEqual({error, no_replicas}, Response),
    % Test - already exists
    create(Key, Value),
    Response1 = adprep:read(Key),
    ?assertEqual({ok, Value}, Response1),
    % Clean-up
    stop().

write_test() ->
    % Initialise
    initialise(),
    Key = 'write_test',
    Value = "value1",
    NewValue = "new_value",
    % Test - does not exist
    Response = adprep:update(Key, Value),
    ?assertEqual({error, no_replicas}, Response),
    % Test - already exist
    create(Key, Value),
    Response1 = adprep:update(Key, NewValue),
    ?assertEqual(ok, Response1),
    Response2 = adprep:read(Key),
    ?assertEqual({ok, NewValue}, Response2),
    % Clean-up
    stop().

delete_test() ->
    % Initialise
    initialise(),
    Key = 'delete_test',
    Value = "value",
    % Test - already exist
    create(Key, Value),
    Response = adprep:delete(Key),
    ?assertEqual(ok, Response),
    Response1 = adprep:read(Key),  % should not exists
    ?assertEqual({error, no_replicas}, Response1),
    Respose2 = create(Key, Value), % should be able to create it again
    ?assertEqual(ok, Respose2),
    % Clean-up
    stop().

hasAReplica_test() ->
    % Initialise
    initialise(),
    Key = 'hasAReplica_test',
    Value = "value",
    % Test - no replica
    Response = adprep:hasReplica(Key),
    ?assertEqual(false, Response),
    % Test - with replica
    create(Key, Value),
    Response1 = adprep:hasReplica(Key),
    ?assertEqual(true, Response1),
    % Clean-up
    stop().

remove_test() ->
    % Initialise
    initialise(),
    Key = 'remove_test',
    Value = "value",
    % Test - already exists
    create(Key, Value),
    Response = adprep:remove(Key),
    ?assertEqual(ok, Response),
    Response1 = adprep:read(Key),  % should not exists
    ?assertEqual({error, no_replicas}, Response1),
    Respose2 = create(Key, Value), % should be able to create it again
    ?assertEqual(ok, Respose2),
    VerifyRemove = fun(_Record, _Args) -> false end,
    Response3 = adprep:remove(Key, VerifyRemove, []),
    ?assertEqual({ok, failed_verification}, Response3),
    Respose4 = create(Key, Value), % should be able to create it again
    ?assertEqual({error,already_exists_replica}, Respose4),
    VerifyRemove1 = fun(_Record, _Args) -> true end,
    Response5 = adprep:remove(Key, VerifyRemove1, []),
    ?assertEqual(ok, Response5),
    % Test - does not exist
    VerifyRemove2 = fun(_Record, _Args) -> true end,
    Response4 = adprep:remove(Key, VerifyRemove2, []),
    ?assertEqual(ok, Response4),
    % Clean-up
    stop().

removeUnexisting_test() ->
    % Initialise
    initialise(),
    Key = 'remove_test',
    % Test - does not exist
    Response = adprep:remove(Key),
    ?assertEqual(ok, Response),
    % Clean-up
    stop().

removeVerify_test() ->
    % Initialise
    initialise(),
    Key = 'removeVerify_test',
    Value = "value",
    VerifyRemove = fun(_Record, _Args) -> false end,
    % Test - does not exist
    Response = adprep:remove(Key, VerifyRemove, []),
    ?assertEqual(ok, Response),
    % Test - already exist
    create(Key, Value),
    Response1 = adprep:remove(Key, VerifyRemove, []),
    ?assertEqual({ok, failed_verification}, Response1),
    Response2 = adprep:read(Key),  % should not exists
    ?assertEqual({ok, Value}, Response2),
    % Clean-up
    stop().

unsuportedMsg_test() ->
    % Initialise
    initialise(),
    % Test
    gen_server:cast(adprep, {unsuported}),
    try gen_server:call(adprep, {unsuported}, 500) of
        Result ->
            ?assertEqual(nothing, Result)
    catch
        exit:{Result,_} ->
            ?assertEqual(timeout, Result)
    end,
    % Clean-up
    stop().

code_change_test() ->
    PreviousVersion = "0.00",
    State = none,
    Extra = [],
    Result = adprep:code_change(PreviousVersion, State, Extra),
    ?assertEqual({ok, State}, Result).

%% ============================================================================
createNew_test() ->
    % Initialise
    initialise(),
    Key = 'createNew_test',
    Value = "value",
    DCs = sets:new(),
    % Test - does not exist
    Response = gen_server:call(adprep, {create_new, Key, Value, DCs}),
    ?assertEqual(ok, Response),
    % Test - already exists
%    create(Key, Value),
    Response1 = gen_server:call(adprep, {create_new, Key, Value, DCs}),
    ?assertEqual({error, already_has_replica}, Response1),
    % Clean-up
    stop().

getDCs_test() ->
    % Initialise
    initialise(),
    Key = 'getDCs_test',
    Value = "ValUe",
    % Test - does not exist
    ?assertEqual({ok, []}, gen_server:call(adprep, {get_dcs, Key})),
    % Test - already exists
    create(Key, Value),
    Result1 = gen_server:call(adprep, {get_dcs, Key}),
    ?assertEqual({ok, [node()]}, Result1),
    % Clean-up
    stop().

hasReplica_test() ->
    % Initialise
    initialise(),
    Key = 'hasReplica_test',
    Value = "value",
    % Test - data does not exist
    Response = gen_server:call(adprep, {has_replica, Key}),
    ?assertEqual(no, Response),
    % Test - data exist
    create(Key, Value),
    Response1 = gen_server:call(adprep, {has_replica, Key}),
    ?assertEqual({yes, [node()]}, Response1),
    % Clean-up
    stop().

newId_test() ->
    % Initialise
    initialise(),
    Key = 'newId_test',
    Id = adprep:newId(Key),
    ?assertEqual(0, Id),
    Id1 = adprep:newId(),
    ?assertEqual(1, Id1),
    % Clean-up
    stop().

newId__test() ->
    % Initialise
    initialise(),
    Key = 'newId__test',
    Id = adpreps_:getNewID(Key),
    ?assertEqual(0, Id),
    Id1 = adpreps_:getNewID(),
    ?assertEqual(1, Id1),
    % Clean-up
    stop().

handle_info_test() ->
    % Initialise
    initialise(),
    % Test
    Result = adprep ! {unsuported},
    ?assertEqual({unsuported}, Result),
    % Clean-up
    stop().

rmvReplicat_test() ->
    % Initialise
    initialise(),
    Key = 'rmvReplicat_test',
    % Test - does not exist
    Response = gen_server:call(adprep, {rmv_replica, node(), Key}),
    ?assertEqual({error, no_replica}, Response),
    % Clean-up
    stop().

newReplica_test() ->
    % Initialise
    initialise(),
    Key = 'newReplica_test',
    Value = "value",
    % Test - does not exist
    Response = gen_server:call(adprep, {new_replica, node(), Key}),
    ?assertEqual({error, self}, Response),
    Response2 = gen_server:call(adprep, {new_replica, node(), Key, Value}),
    ?assertEqual({error, self}, Response2),
    Response3 = gen_server:call(adprep, {new_replica, any, Key}),
    ?assertEqual({error, does_not_exist}, Response3),
    Response4 = gen_server:call(adprep, {new_replica, any, Key, Value}),
    ?assertEqual({error, does_not_exist}, Response4),
    % Test - exists
    create(Key, Value),
    Response5 = gen_server:call(adprep, {new_replica, node(), Key}),
    ?assertEqual({error, self}, Response5),
    Response6 = gen_server:call(adprep, {new_replica, node(), Key, Value}),
    ?assertEqual({error, self}, Response6),
    % Clean-up
    stop().

update_test() ->
    % Initialise
    initialise(),
    Key = 'update_test',
    Value = "value",
    % Test - does not exist
    Response = gen_server:call(adprep, {update, Key, Value}),
    ?assertEqual({error, no_replica}, Response),
    % Test - exists
    create(Key, Value),
    Response1 = gen_server:call(adprep, {update, Key, Value}),
    ?assertEqual({ok, updated}, Response1),
    % Clean-up
    stop().

forwardDelete_test() ->
    % Initialise
    initialise(),
    Key = 'update_test',
    Value = "value",
    Args = #adpargs{decay_time = 5 * 1000,
                    min_num_replicas = 1,
                    replication_threshold = 2.0,
                    rmv_threshold = 0.0,
                    max_strength = 10.0,
                    decay = 0.5,
                    wdecay = 0.5,
                    rstrength = 1.0,
                    wstrength = 1.5},
    adpreplic:create(Key, Value, adprep, Args),
    % Test
    Response = gen_server:call(adprep, {forward_delete, Key}),
    ?assertEqual(ok, Response),
    % Clean-up
    stop().


%% ============================================================================
initialise() ->
    io:format("~n    (adprep_tests): Initialising servers~n", []),
    {ok, _datastorePid} = datastore:start(),
    {ok, _replicationLayerPid} = adprep:start().

stop() ->
    io:format("    (adprep_tests): Stopping servers~n", []),
    adprep:stop(),
    datastore:stop(),
    erlang:yield().

create(Key, Value) ->
    NextDCFunc = fun(_Rl, _AllDCs, _Args) -> {[], []} end,
    adprep:create(Key, Value, NextDCFunc, []).

checkNewId(_Pid, _Id, _ExpectedId, 0) ->
    ok;
checkNewId(Pid, Id, ExpectedId, NumTimes) when NumTimes > 0 ->
    {reply, 'new_id', Id, Response} = gen_server:call(Pid, {new_id, Id, any}),
    ?assertEqual(Response, ExpectedId),
    checkNewId(Pid, Id, ExpectedId+1, NumTimes-1).
