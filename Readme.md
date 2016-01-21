## Readme
This site contains the work carried out for the Adaptive Replication, which 
includes various types of documentation. Please refer to the documentation in 
the directory docs for more information about this work.

This solution based on an adaptive geo-replication algorithm aims to make a decision without the need of human intervention where and when to replicate a data item while keeping into account the benefits and costs.

This algorithm shall take into account the following variables: available data centers, the replicas number of a data item, number of reads of that data item from a data center, number of writes of that data item from a data center, time spent from the last read or write of that data item in a data center, number of replicas for that data item, minimum number of replicas for that data item, threshold for which data item is added to or removed from a data center.

The docs directory contains all the documentation related to this work, 
including the javadoc like documentation under docs/doc.

The source code is Erlang and it is located in the src, test and include directories. 
The Strategies and strategy layer files should have the name starting with strategy_, 
e.g. strategy_adprep.erl, and the name for the replication layer files should start 
with replica_.

## Automated scripts for Ubuntu 14.04:

    # Make sure the configuration from ./deploy/ubuntu-14-04/etc/adpreplic.config
    # suits your needs before running the following scripts

    # To build install Erlang/OTP R16B02
    ./deploy/ubuntu-14-04/scripts/install-erlang.sh

    # To update the local files from the git repo
    ./deploy/ubuntu-14-04/scripts/update-git-repo.sh

    # To restart adpreplic
    ./deploy/ubuntu-14-04/scripts/restart-adpreplic.sh


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

## To build javadoc like documentation run:
    erl -noshell -run edoc_run packages '[""]' '[{source_path, ["./src" | ["./include"]]}, {dir,"./docs/doc"}, {private,true}, {todo,true}]'

## To see the docs in a browser:

    # install a web server like apache2
    # sudo apt-get install apache2 -y
    cp -r docs /var/www/html/
    chmod -R 755 /var/www/html/docs
    # access via a browser http://<IP>/docs

# Resources to understand OTP applications

    http://www.erlang.org/doc/design_principles/applications.html#appl_res_file

# To run in a distributed scenario

    Modify the contents of ./rel/files/vm.args -name entry to the hostname of the node.

    Make sure you add in all the other nodes a valid hosts entry if you do not have a DNS server.

    The hostnames must be in a FQDN format, like <entry>.local