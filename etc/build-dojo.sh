#!/bin/sh

WUI_HOME="`dirname $0`/.."
DOJO_HOME="`dirname $0`/../../dojo"

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

if [ "$1" != "" ]; then
    DOJO_HOME=$1
fi

WUI_HOME=`absolutize "$WUI_HOME"`
DOJO_HOME=`absolutize "$DOJO_HOME"`

echo "Assuming the following paths:"
echo "wui   - $WUI_HOME"
echo "dojo  - $DOJO_HOME"

if [ ! -d "$DOJO_HOME" -o ! -d "$WUI_HOME" ]; then
    echo Some of the paths are not correct!
    echo Some hints:
    echo svn co http://svn.dojotoolkit.org/src/view/anon/all/trunk dojo
    exit -1
fi

cp "$WUI_HOME/etc/wui.profile.js" "$DOJO_HOME/util/buildscripts/profiles/wui.profile.js"
cd "$DOJO_HOME/util/buildscripts"
./build.sh profile="wui" action="release"

rm -rf "$WUI_HOME/wwwroot/dojo/src/"
rm -f "$WUI_HOME/wwwroot/dijit"

cp -r "$DOJO_HOME/release/dojo/" "$WUI_HOME/wwwroot/"
#cp "$DOJO_HOME/release/dojo/iframe_history.html" "$WUI_HOME/wwwroot/dojo/"
#cp -r "$DOJO_HOME/release/dojo/src/" "$WUI_HOME/wwwroot/dojo/"
#ln -s "$DOJO_HOME/dijit" "$WUI_HOME/wwwroot/dijit"
