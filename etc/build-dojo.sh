#!/bin/bash

# http://docs.dojocampus.org/build/buildScript
# example usage: ~/workspace/wui/etc/build-dojo.sh --dojo ~/workspace/dojo.head --dojo-release-dir ~/workspace/ebr42/wwwroot/ --profile ~/workspace/ebr42/etc/ebr42-dojo-profile.js --locales "en-us,hu"

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
DOJO_RELEASE_DIR="${WUI_HOME}/wwwroot/"
DOJO_HOME="`dirname $0`/../../dojo"
DOJO_RELEASE_NAME=`date +"%Y-%m-%dT%H-%M"`
DOJO_PROFILE="${WUI_HOME}/etc/wui/profile.js"

TEMP=`getopt -o h --long help,dojo:,dojo-release-name:,dojo-release-dir:,wui:,profile:,locales: -n "$0" -- "$@"`

# echo $TEMP

if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi

# Note the quotes around `$TEMP': they are essential!
eval set -- "$TEMP"

while true ; do
	case "$1" in
        	-h|--help)	echo TODO usage...
        		        exit 0
        		        ;;
        	--wui) WUI_HOME=$2 ; shift 2
                     DOJO_PROFILE="${WUI_HOME}/etc/wui/profile.js"
                     DOJO_RELEASE_DIR="${WUI_HOME}/wwwroot/"
        	     ;;
        	--dojo) DOJO_HOME=$2 ; shift 2
        	      ;;
        	--dojo-release-name) DOJO_RELEASE_NAME=$2 ; shift 2
        	      ;;
        	--dojo-release-dir) DOJO_RELEASE_DIR=$2 ; shift 2
        	      ;;
        	--locales) LOCALE_LIST=$2 ; shift 2
        	         ;;
        	--profile) DOJO_PROFILE=$2 ; shift 2
	                 ;;
		--) shift ; break ;;
		*) echo "Internal error at $1!" ; exit 1 ;;
	esac
done

#echo "Remaining arguments:"
#for arg do echo '--> '"\`$arg'" ; done

WUI_HOME=`absolutize "$WUI_HOME"`
DOJO_HOME=`absolutize "$DOJO_HOME"`
DOJO_RELEASE_DIR=`absolutize "$DOJO_RELEASE_DIR"`

echo "Will build dojo into ${DOJO_RELEASE_DIR} now..."
echo "Assuming the following parameters:"
echo "profile     - $DOJO_PROFILE"
echo "locales     - $LOCALE_LIST"
echo "wui         - $WUI_HOME"
echo "dojo        - $DOJO_HOME"
echo "release dir - $DOJO_RELEASE_DIR"

if [ ! -d "$DOJO_HOME" -o ! -d "$WUI_HOME" ]; then
    echo Some of the paths are not correct!
    echo Hint:
    echo svn co http://svn.dojotoolkit.org/src/tags/release-1.2.3/ dojo/
    echo svn co http://svn.dojotoolkit.org/src/trunk/ dojo/
    exit -1
fi

echo Starting the dojo build script now...
echo

cd "${DOJO_HOME}/util/buildscripts"
./build.sh action="clean,release" profileFile="$DOJO_PROFILE" releaseDir=${DOJO_RELEASE_DIR} releaseName=dojo-${DOJO_RELEASE_NAME} copyTests=false layerOptimize=shrinksafe.keepLines localeList=${LOCALE_LIST}
