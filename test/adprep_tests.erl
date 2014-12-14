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
-export([]).


-include_lib("eunit/include/eunit.hrl").


%% =============================================================================
%% Internal functions
%% =============================================================================
getNumReplicas_test() ->
    % Initialise
    adprep:start(),
    Key = 'getNumReplicas_test',
    % Test - already running process
    NumReplicas = adprep:getNumReplicas(Key),
    ?assertEqual(0, NumReplicas),
    % Clean-up
    adprep:stop(),
    erlang:yield().

create_test() ->
    % Initialise
    adprep:start(),
    Key = 'create_test',
    Value = "value",
    % Test - does not exist
    Response = create(Key, Value),
    ?assertEqual(ok, Response),
    % Test - altready exists
    Response1 = create(Key, Value),
    ?assertEqual({error, already_exists_replica}, Response1),
    % Clean-up
    adprep:stop(),
    erlang:yield().

create1_test() ->
    % Initialise
    adprep:start(),
    Key = 'create1_test',
    Value = "value",
    % Test - does not exist
    Response = adprep:create(Key, Value),
    ?assertEqual(ok, Response),
    % Test - altready exists
    Response1 = create(Key, Value),
    ?assertEqual({error, already_exists_replica}, Response1),
    % Clean-up
    adprep:stop(),
    erlang:yield().

createNone_test() ->
    % Initialise
    adprep:start(),
    Key = 'createNone_test',
    % Test - does not exist and cannot be created; missing value an no replica anywhere 
    %         else
    Response = adprep:create(Key),
    ?assertEqual({error, does_not_exist}, Response),
    % Clean-up
    adprep:stop(),
    erlang:yield().

read_test() ->
    % Initialise
    adprep:start(),
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
    adprep:stop(),
    erlang:yield().

write_test() ->
    % Initialise
    adprep:start(),
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
    adprep:stop(),
    erlang:yield().

delete_test() ->
    % Initialise
    adprep:start(),
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
    adprep:stop(),
    erlang:yield().

hasAReplica_test() ->
    % Initialise
    adprep:start(),
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
    adprep:stop(),
    erlang:yield().

remove_test() ->
    % Initialise
    adprep:start(),
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
    adprep:stop(),
    erlang:yield().

removeUnexisting_test() ->
    % Initialise
    adprep:start(),
    Key = 'remove_test',
    % Test - does not exist
    Response = adprep:remove(Key),
    ?assertEqual(ok, Response).

removeVerify_test() ->
    % Initialise
    adprep:start(),
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
    adprep:stop(),
    erlang:yield().

%% ============================================================================
getDCs_test() ->
    % Initialise
    adprep:start(),
    Key = 'getDCs_test',
    % Test
    ?assertEqual({exists, []}, gen_server:call(adprep, {get_dcs, Key})),
    % Clean-up
    adprep:stop(),
    erlang:yield().

hasReplica_test() ->
    % Initialise
    adprep:start(),
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
    adprep:stop(),
    erlang:yield().

newId_test() ->
    % Initialise
    adprep:start(),
    Key = 'newId_test',
    Id = adprep:newId(Key),
    ?assertEqual(0, Id),
    Id1 = adprep:newId(),
    ?assertEqual(1, Id1),
    % Clean-up
    adprep:stop(),
    erlang:yield().

newId__test() ->
    % Initialise
    adprep:start(),
    Key = 'newId__test',
    Id = adpreps_:getNewID(Key),
    ?assertEqual(0, Id),
    Id1 = adpreps_:getNewID(),
    ?assertEqual(1, Id1),
    % Clean-up
    adprep:stop(),
    erlang:yield().

handle_info_test() ->
    % Initialise
    adprep:start(),
    Result = adprep ! {unsuported},
    ?assertEqual({unsuported}, Result),
    % Clean-up
    adprep:stop(),
    erlang:yield().

%% ============================================================================
create(Key, Value) ->
    NextDCFunc = fun(_Rl, _AllDCs, _Args) -> {[], []} end,
    adprep:create(Key, Value, NextDCFunc, []).

checkNewId(_Pid, _Id, _ExpectedId, 0) ->
    ok;
checkNewId(Pid, Id, ExpectedId, NumTimes) when NumTimes > 0 ->
    {reply, 'new_id', Id, Response} = gen_server:call(Pid, {new_id, Id, any}),
    ?assertEqual(Response, ExpectedId),
    checkNewId(Pid, Id, ExpectedId+1, NumTimes-1).
