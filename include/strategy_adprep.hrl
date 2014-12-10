%% =============================================================================
%% Haedrer for First Propossed Adaptive Replication Strategy - SyncFree
%%
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

-record(adpargs,   {decay_time :: integer(), %% time in milliseconds between decays
					min_num_replicas = 1 :: integer() , %% the minimum number of replicas at any time
					replication_threshold :: float(), %% the strength threshold to replicat the data locally
					rmv_threshold :: float(),  %% the strength threshold to remove the local replica
					max_strength :: float(), %% the maximum strength of the replication
					decay :: float(), %% the time decay strength
					wdecay :: float(), %% the write decay strength
					rstrength :: float(), %% the read increase on the stregth
					wstrength :: float()}). %% the write increase on the stregth
