#!/bin/bash

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
PROFILE="${WUI_HOME}/etc/wui/profile.js"

TEMP=`getopt -o h --long help,dojo:,wui:,profile:,locales: -n "$0" -- "$@"`

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
                     PROFILE="${WUI_HOME}/etc/wui/profile.js"
        	     ;;
        	--dojo) DOJO_HOME=$2 ; shift 2
        	      ;;
        	--locales) LOCALE_LIST=$2 ; shift 2
        	         ;;
        	--profile) PROFILE=$2 ; shift 2
	                 ;;
		--) shift ; break ;;
		*) echo "Internal error at $1!" ; exit 1 ;;
	esac
done

#echo "Remaining arguments:"
#for arg do echo '--> '"\`$arg'" ; done

WUI_HOME=`absolutize "$WUI_HOME"`
DOJO_HOME=`absolutize "$DOJO_HOME"`
RELEASE_DIR=${WUI_HOME}/wwwroot/

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
