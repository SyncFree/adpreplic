## Readme
This site contains the work carried out for the Adaptive Replication, which 
includes various types of documentation. Please refer to the documentation in 
the directory docs for more information about this work.

The docs directory contains all the documentation related to this work, 
including the javadoc like documentation under docs/doc.

The source code is Erlang and it is located in the src, test and include directories. 
The Strategies and strategy layer files should have the name starting with strategy_, 
e.g. strategy_adprep.erl, and the name for the replication layer files should start 
with replica_.


## To install Erlang/OTP On Ubuntu 14.04:

    sudo apt-get install build-essential ncurses-dev libssl-dev openjdk-6-jdk xsltproc fop libxml2-utils -y
    # this project works only with OTP version R16B02
    wget http://www.erlang.org/download/otp_src_R16B02.tar.gz
    tar -xvzf otp_src_R16B02.tar.gz
    pushd otp_src_R16B02
    ./configure --prefix=$HOME/erlang-R16B02
    make
    make install
    popd
    # add erlang bin folder $HOME/erlang-R16B02/bin to your path

## To build/run the adpreplic application:

    # compile
    make rel
    # run the app
    ./rel/adpreplic/bin/adpreplic console
    # run an app method
    adpreplic:create().

## To build javadoc like documentation run(not working for now):
    erl -noshell -run edoc_run packages '[""]' '[{source_path, ["./src" | ["./include"]]}, {dir,"./docs/doc"}, {private,true}, {todo,true}]'

## To see the docs in a browser:

    # install a web server like apache2
    # sudo apt-get install apache2 -y
    cp -r docs /var/www/html/
    chmod -R 755 /var/www/html/docs
    # access via a browser http://<IP>/docs

# Resources to understand OTP applications

    http://www.erlang.org/doc/design_principles/applications.html#appl_res_file

# To run in a dsitributed scenario

    Modify the contents of ./rel/files/vm.args -name entry to the hostname of the node.

    Make sure you add in all the other nodes a valid hosts entry if you do not have a DNS.

    The hostnames must be in a FQDN format, like <entry>.local