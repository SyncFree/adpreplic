#!/bin/bash

time erl -noshell -name avladu@adpreplic-client.com -s \
    run_test test_update "load_test_value.txt" -s init stop
