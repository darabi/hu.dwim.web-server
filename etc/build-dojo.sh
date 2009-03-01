#!/bin/sh

# http://docs.dojocampus.org/build/buildScript
# example usage: ~/workspace/wui/etc/build-dojo.sh ~/workspace/ebr42/etc/ebr42-dojo-profile.js "en-us,hu"

absolutize ()
{
  if [ ! -d "$1" ]; then
    echo
    echo "ERROR: '$1' doesn't exist or not a directory!"
    exit -1
  fi

  cd "$1"
  echo `pwd`
  cd - >/dev/null
}

LOCALE_LIST="en-us"
WUI_HOME="`dirname $0`/.."
DOJO_HOME="`dirname $0`/../../dojo"

WUI_HOME=`absolutize "$WUI_HOME"`
DOJO_HOME=`absolutize "$DOJO_HOME"`
RELEASE_DIR=${WUI_HOME}/wwwroot/

PROFILE="${WUI_HOME}/etc/wui/profile.js"

if [ "$1" != "" ]; then
    PROFILE=$1
fi

if [ "$2" != "" ]; then
    LOCALE_LIST=$2
fi

echo "Will build dojo into ${RELEASE_DIR} now..."
echo "Assuming the following parameters:"
echo "profile - $PROFILE"
echo "locales - $LOCALE_LIST"
echo "wui     - $WUI_HOME"
echo "dojo    - $DOJO_HOME"

if [ ! -d "$DOJO_HOME" -o ! -d "$WUI_HOME" ]; then
    echo Some of the paths are not correct!
    echo Hint:
    echo svn co http://svn.dojotoolkit.org/src/tags/release-1.2.3/ dojo/
    exit -1
fi

echo Starting the dojo build script now...
echo

cd "${DOJO_HOME}/util/buildscripts"
./build.sh profileFile="$PROFILE" releaseDir=${RELEASE_DIR} action="clean,release" copyTests=false layerOptimize=shrinksafe.keepLines localeList=${LOCALE_LIST}
