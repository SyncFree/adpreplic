%% =============================================================================
%% Adapive Replication User Interface - SyncFree
%%
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%% @doc Provides the user interface to execute requests.
%% 
-module(userar).
-author('aas@trifork.co.uk').

-compile(export_all).
-export([create/5, read/2, write/3]).
%-import(adpreps_, [create/5, read/2, write/3]).


%% =============================================================================
%% User Adaptive Replication support
%% =============================================================================
%% @spec create(Key, Id::integer(), Value, Strategy::atom(), Args::tuple()) -> Result::tuple()
%% 
%% @doc Creates a local replica of the data. The results may have any of the values {ok} or 
%%		{error, ErrorCode}.
create(Key, Id, Value, Strategy, Args) ->
	adpreps_:create(Key, Id, Value, Strategy, Args).

%% @spec read(Key, Id::integer())-> Result::tuple()
%% 
%% @doc Reads the data value for the local replica. The results may have any of the values {ok} or 
%%		{error, ErrorCode}.
read(Key, Id) ->
	adpreps_:read(Key, Id).

%% @spec write(Key, Id::integer(), Value)-> Result::tuple()
%% 
%% @doc Writes the new value for the data. The results may have any of the values {ok} or 
%%		{error, ErrorCode}.
write(Key, Id, Value) ->
	adpreps_:write(Key, Id, Value).
