%% =============================================================================
%% Adapive Replication User Interface - SyncFree
%%
%% Public API, can be called by clients using RPC.
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

-ifdef(EUNIT).
% Unit-test
-compile(export_all).
-else.
% User interface
-export([create/4, delete/1, read/1, update/2]).
-endif.


%% =============================================================================
%% User Adaptive Replication support
%% =============================================================================
%% @spec create(Key::atom(), Value::term(), Strategy::atom(), Args::term()) -> Result::tuple()
%% 
%% @doc Creates a local replica of the data. The results may have any of the values {ok} 
%%      or {error, ErrorCode}.
create(Key, Value, Strategy, Args) ->
    adpreps_:create(Key, Value, Strategy, Args).

%% @spec read(Key::atom())-> Result::tuple()
%% 
%% @doc Reads the data value for the local replica. The results may have any of the 
%%      values {ok, Value::term()} or {error, ErrorCode::term()}.
read(Key) ->
    adpreps_:read(Key).

%% @spec update(Key::atom(), Value::term())-> Result::tuple()
%% 
%% @doc Writes the new value for the data. The results may have any of the values {ok} or 
%%      {error, ErrorCode}.
update(Key, Value) ->
    adpreps_:update(Key, Value).

%% @spec delete(Key::atom())-> Result::tuple()
%% 
%% @doc Delstes the data from every replica. The results may have any of the values {ok} 
%%      or {error, ErrorCode::term()}.
delete(Key) ->
    adpreps_:delete(Key).
