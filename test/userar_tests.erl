%% =============================================================================
%% EUnits for the user interface - SyncFree
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

-module(userar_tests).
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

create_test() ->
    % Initialise
    Key = 'create_test',
    Value = "Value",
    Strategy = adprep,
    Args = #adpargs{decay_time = 5 * 1000,
                    min_num_replicas = 1,
                    replication_threshold = 2.0,
                    rmv_threshold = 0.0,
                    max_strength = 10.0,
                    decay = 0.5,
                    wdecay = 0.5,
                    rstrength = 1.0,
                    wstrength = 1.5},
    adprep:start(),
    erlang:yield(),
    % Test
    Result = userar:create(Key, Value, Strategy, Args),
    ?assertEqual(ok, Result),
    NewValue = "NEWVALUE",
    Result1 = userar:create(Key, NewValue, Strategy, Args),
    ?assertEqual({error, already_exists}, Result1),
    stop(Key).

read_test() ->
    % Initialise
    Key = 'read_test',
    Value = "VALUE",
    initialise(Key, Value),
    % Test
    checkValue(Key, Value),
    stop(Key).

update_test() ->
    % Initialise
    Key = 'update_test',
    Value = "VALUE",
    initialise(Key, Value),
    % Test
    NewValue = "NEWVALUE",
    Result = userar:update(Key, NewValue),
    ?assertEqual(ok, Result),
    checkValue(Key, NewValue),
    stop(Key).

delete_test() ->
    % Initialise
    Key = 'delete_test',
    Value = "VALUE",
    initialise(Key, Value),
    % Test - exits
    Result = userar:delete(Key),
    ?assertEqual(ok, Result),
    erlang:yield(),
    % Test - does not exist
    Result1 = userar:delete(Key),
    ?assertEqual({error, does_not_exist}, Result1),
    stop(Key).


%% =============================================================================
%% Internal functions
%% =============================================================================
create(Key, Value) ->
    % Initialise
    Args = #adpargs{decay_time = 5 * 1000,
                    min_num_replicas = 1,
                    replication_threshold = 2.0,
                    rmv_threshold = 0.0,
                    max_strength = 10.0,
                    decay = 0.5,
                    wdecay = 0.5,
                    rstrength = 1.0,
                    wstrength = 1.5},
    userar:create(Key, Value, adprep, Args).

checkValue(Key, Value) ->
    case userar:read(Key) of 
        {ok, Value1} ->
            ?assertEqual(Value, Value1);
        {error, ErrorCode} ->
            ?assertEqual(Value, ErrorCode);
        Result ->
            ?assertEqual(Value, Result)
    end.


initialise(Key, Value) ->
    adprep:start(),
    erlang:yield(),
    create(Key, Value),
    erlang:yield().

stop(Key) ->
    adpreps_:stop(Key),
    erlang:yield(),
    adprep:stop().
