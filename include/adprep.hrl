%% =============================================================================
%% Haedrer for Partial Replication Layer - SyncFree
%%
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

-record(replica,   {key, 
					value, 
					num_replicas = 1, 
					list_dcs_with_replicas}).

-type key() :: term().
-type id() :: integer().
-type value() :: term().
-type strategy() :: atom().
-type args() :: tuple().
-type reason() :: atom().

-export_type([key/0, id/0, value/0, strategy/0, args/0, reason/0]).

