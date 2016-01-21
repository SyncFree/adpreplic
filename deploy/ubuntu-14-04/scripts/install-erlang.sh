set -e

sudo apt-get install crudini -y

BASEDIR=$(dirname $0)
CONFIG_PATH="${BASEDIR}/../etc/adpreplic.config"
ERLANG_DIRECTORY=$(crudini --get ${CONFIG_PATH} DEFAULT erlang_dir)
ERLANG_VERSION=$(crudini --get ${CONFIG_PATH} DEFAULT erlang_version)
PATH=$PATH:$ERLANG_DIRECTORY"/bin"

erl_bin=`which erl`
if [ -z "$erl_bin" ]; then
    sudo apt-get install build-essential ncurses-dev libssl-dev openjdk-6-jdk xsltproc fop libxml2-utils -y
    wget http://www.erlang.org/download/otp_src_"$ERLANG_VERSION".tar.gz
    tar -xvzf otp_src_"$ERLANG_VERSION".tar.gz
    pushd otp_src_"$ERLANG_VERSION"
    ./configure --prefix=$ERLANG_DIRECTORY
    make
    make install
    popd
else 
    echo "Erlang/OTP version $ERLANG_VERSION already installed. Skipping..."
fi

bash "$BASEDIR/update-git-repo.sh"
