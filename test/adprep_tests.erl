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
    % Test - altready exists
    Response1 = create(Key, Value),
    ?assertEqual({error, already_exists_replica}, Response1),
    % Clean-up
    stop().

create1_test() ->
    % Initialise
    initialise(),
    Key = 'create1_test',
    Value = "value",
    % Test - does not exist
    Response = adprep:create(Key, Value),
    ?assertEqual(ok, Response),
    % Test - altready exists
    Response1 = create(Key, Value),
    ?assertEqual({error, already_exists_replica}, Response1),
    % Clean-up
    stop().

createNone_test() ->
    % Initialise
    initialise(),
    Key = 'createNone_test',
    % Test - does not exist and cannot be created; missing value an no replica anywhere 
    %         else
    Response = adprep:create(Key),
    ?assertEqual({error, does_not_exist}, Response),
    % Clean-up
    stop().

read_test() ->
    % Initialise
    initialise(),
    Key = 'read_test',
    Value = "value1",
    % Test - does not exist
    Response = adprep:read(Key),
    ?assertEqual({error, timeout}, Response),
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
    ?assertEqual({error, timeout}, Response),
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
    ?assertEqual({error, timeout}, Response1),
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
    % Test - already exist
    create(Key, Value),
    Response = adprep:remove(Key),
    ?assertEqual(ok, Response),
    Response1 = adprep:read(Key),  % should not exists
    ?assertEqual({error, timeout}, Response1),
    Respose2 = create(Key, Value), % should be able to create it again
    ?assertEqual(ok, Respose2),
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
    % Test - already exist
    create(Key, Value),
    VerifyRemove = fun(_Record, _Args) -> false end,
    Response = adprep:remove(Key, VerifyRemove, []),
    ?assertEqual({ok, failed_verification}, Response),
    Response1 = adprep:read(Key),  % should not exists
    ?assertEqual({ok, Value}, Response1),
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

%% ============================================================================
getDCs_test() ->
    % Initialise
    initialise(),
    Key = 'getDCs_test',
    % Test
    ?assertEqual({exists, []}, gen_server:call(adprep, {get_dcs, Key})),
    % Clean-up
    stop().

hasReplica_test() ->
    % Initialise
    initialise(),
    Key = 'hasReplica_test',
    Id = 1,
    Value = "value",
    % Test - data does not exist
    gen_server:cast(adprep, {has_replica, self(), Id, Key}),
    Result = receive
        R ->
            R
    after
        1000 ->
            time_out
    end,
    ?assertEqual(time_out, Result),
    % Test - data exist
    create(Key, Value),
    gen_server:cast(adprep, {has_replica, self(), Id, Key}),
    Result1 = receive
        {reply, has_replica, Id, Response1} ->
            Response1
    after
        1000 ->
            false
    end,
    ?assertEqual({exists, [node()]}, Result1),
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


%% ============================================================================
initialise() ->
    io:format("(adprep_tests): Initialising servers~n", []),
    {ok, _datastorePid} = datastore:start(),
    {ok, _replicationLayerPid} = adprep:start().

stop() ->
    io:format("(adprep_tests): Stopping servers~n", []),
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
