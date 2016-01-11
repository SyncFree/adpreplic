#!/bin/bash
#set -e

SECRET=$1
ADPREPLIC_NODE=$2
BASEDIR=$(dirname $0)
CONFIG_PATH="${BASEDIR}/../etc/adpreplic.config"

pushd $BASEDIR/../../..
ES=$?
if [ "$ES" -ne 0 ]; then
    echo "Failed to pushd!"
    exit 1
fi

make cleanplt
ES=$?
if [ "$ES" -ne 0 ]; then
    echo "Failed to make cleanplt!"
    exit 1
fi

make rel
ES=$?
if [ "$ES" -ne 0 ]; then
    echo "Failed to make rel first!"
    make rel
    ES=$?
    if [ "$ES" -ne 0 ]; then
        echo "Failed to make rel!"
        exit 1
    fi
fi

./rel/adpreplic/bin/adpreplic stop
ES=$?
if [ "$ES" -ne 0 ]; then
    echo "Failed to stop adpreplic!"
fi

./rel/adpreplic/bin/adpreplic start
ES=$?
if [ "$ES" -ne 0 ]; then
    echo "Failed to start adpreplic!"
    exit 1
fi

sleep 2
./rel/adpreplic/bin/adpreplic getpid
ES=$?
if [ "$ES" -ne 0 ]; then
    echo "Failed to get adpreplic first pid!"
    ./rel/adpreplic/bin/adpreplic getpid
    ES=$?
    if [ "$ES" -ne 0 ]; then
        echo "Failed to get adpreplic pid!"
        exit 1
    fi
fi

echo "Successfully restarted adpreplic"
popd

