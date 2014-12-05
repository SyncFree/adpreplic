%% =============================================================================
%% EUnits for Adapive Replications DC - SyncFree
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================


-module(adpreps_tests).
-author('aas@trifork.co.uk').

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
-export([]).


-include_lib("eunit/include/eunit.hrl").


%% ====================================================================
%% Internal functions
%% ====================================================================


flush_test() ->
	?assertEqual(adpreps:flush(""), {ok}).
