#!/!bin/bash
set -e

sudo apt-get install crudini -y
BASEDIR=$(dirname $0)
CONFIG_PATH="${BASEDIR}/../etc/adpreplic.config"

ADPREPLIC_DIRECTORY=$(crudini --get ${CONFIG_PATH} DEFAULT adpreplic_dir)
GIT_REPOSITORY=$(crudini --get ${CONFIG_PATH} git repository) 
GIT_BRANCH=$(crudini --get ${CONFIG_PATH} git branch) 
GIT_COMMIT=$(crudini --get ${CONFIG_PATH} git commit)

sudo apt-get install git -y
if [ ! -d "$ADPREPLIC_DIRECTORY" ]; then
  sudo mkdir -p $ADPREPLIC_DIRECTORY
  chmod -R 777 $ADPREPLIC_DIRECTORY
  git clone $GIT_REPOSITORY $ADPREPLIC_DIRECTORY
else
  echo "Adpreplic Directory exists. Skip cloning..."
fi

pushd $ADPREPLIC_DIRECTORY

git checkout $GIT_BRANCH
git pull origin $GIT_BRANCH

if [ "$GIT_COMMIT" = "last" ]; then
    echo "Skipping git reset to commit..."
else
    git reset --hard $GIT_COMMIT
fi

popd

