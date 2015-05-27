%% =============================================================================
%% Adaptive Replication - SyncFree
%% 
%% @author Amadeo Asco, Annette Bieniusa, Adrian Vladu
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%% Unit tests for Strategy module 

-module(datastore_test).
-author(['aas@trifork.co.uk','bieniusa@cs.uni-kl.de', 'adrian.vladu21@gmail.com']).

%% =============================================================================
%% API functions
%% =============================================================================
-compile(export_all).
-export([]).

-include_lib("eunit/include/eunit.hrl").
-include("adprep.hrl").

%% =============================================================================
%% Internal functions
%% =============================================================================

local_datastored_test() ->
    {ok, _} = datastore:start(),
    datastore:create(101, {black,1}),
    {ok, Obj} = datastore:read(101),
    ?assertEqual(Obj, {black,1}),
    ?assertNotEqual(Obj, {black,2}),
    datastore:update(101, {black, 11}),
    datastore:remove(101),
    {error, not_found} = datastore:read(101),
    ok = datastore:stop().

