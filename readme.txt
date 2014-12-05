This site contains the work carried out for the Adaptive Replication, which 
includes various types of documentation. Please refer to the documentation in 
the directory docs for more information about this work.

The docs directory contains all the documentation related to this work, 
including the javadoc like documentation under docs/doc.

The source code is Erlang and it is located in the src, test and include directories. 
The Strategies and strategy layer files should have the name starting with strategy_, 
e.g. strategy_adprep.erl, and the name for the replication layer files should start 
with replica_.


To build run
	make

To build javadoc like documentation run
	erl -noshell -run edoc_run packages '[""]' '[{source_path, ["./src" | ["./test" | ["./include"]]]}, {dir,”./docs/doc"}, {private,true}, {todo,true}]'
