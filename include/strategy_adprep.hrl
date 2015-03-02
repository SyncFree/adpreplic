%% =============================================================================
%% Haedrer for First Propossed Adaptive Replication Strategy - SyncFree
%%
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

-record(adpargs,   {decay_time, 
					min_num_replicas = 1, 
					replication_threshold, 
					rmv_threshold, 
					max_strength, 
					decay, 
					wdecay, 
					rstrength, 
					wstrength}).

-record(strategyState,
                    {decay_time, 
                    min_num_replicas = 1, 
                    replication_threshold, 
                    rmv_threshold, 
                    max_strength, 
                    decay, 
                    wdecay, 
                    rstrength, 
                    wstrength}).

