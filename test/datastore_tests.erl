%% =============================================================================
%% EUnits for DC - SyncFree
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

-module(datastore_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


-include_lib("eunit/include/eunit.hrl").


%% ====================================================================
%% Internal test functions
%% ====================================================================
updateNoExisting_test() ->
    % Initialise
    initialise(),
    Key = 'updateNoExisting_test',
    Value = "value",
    % Test
    Result = datastore:update(Key, Value),
    ?assertEqual({error, not_found}, Result),
    % Clean-up
    stop().

removeNoExisting_test() ->
    % Initialise
    initialise(),
    Key = 'removeNoExisting_test',
    % Test
    Result = datastore:remove(Key),
    ?assertEqual(ok, Result),
    % Clean-up
    stop().

invalidMsg_test() ->
    % Initialise
    initialise(),
    Key = 'invalidMsg_test',
    % Test
    try gen_server:call(datastore, Key, 500) of
        R ->
            ?assertEqual(ok, R)
    catch
        exit:{Type,_} ->    
            ?assertEqual(timeout, Type)
    end,
    gen_server:cast(datastore, Key),
    receive
        Response ->
            ?assertEqual([], Response)
    after
        500 ->
            Result = ok,
            ?assertEqual(ok, Result)
    end,
    datastore ! Key,
    % Clean-up
    stop().

tryCreateExisting_test() ->
    % Initialise
    initialise(),
    Key = 'tryCreateExisting_test',
    Value = "value",
%    NewValue = "new_value",
    Result = datastore:create(Key, Value),
    ?assertEqual(ok, Result),
    % Test
%    Result1 = datastore:create(Key, NewValue),
%    ?assertEqual({error, already_created}, Result1),
    % Clean-up
    stop().

code_change_test() ->
    PreviousVersion = "0.00",
    State = none,
    Extra = [],
    Result = datastore:code_change(PreviousVersion, State, Extra),
    ?assertEqual({ok, State}, Result).


%% ============================================================================
initialise() ->
    io:format("(datastore_tests): Initialising servers~n", []),
    {ok, _datastorePid} = datastore:start().

stop() ->
    io:format("(datastore_tests): Stopping servers~n", []),
    datastore:stop(),
    erlang:yield().
