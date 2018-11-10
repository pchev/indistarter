#!/bin/bash

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/indistarter
fi

echo Install indistarter to $destdir

install -d -m 755 $destdir
install -d -m 755 $destdir/IndiStarter.app
install -d -m 755 $destdir/IndiStarter.app/Contents
install -d -m 755 $destdir/IndiStarter.app/Contents/MacOS
install -d -m 755 $destdir/IndiStarter.app/Contents/Resources
install -v -m 644 system_integration/MacOSX/pkg/IndiStarter.app/Contents/Info.plist $destdir/IndiStarter.app/Contents/
install -v -m 644 system_integration/MacOSX/pkg/IndiStarter.app/Contents/PkgInfo $destdir/IndiStarter.app/Contents/
install -v -m 755 -s indistarter  $destdir/IndiStarter.app/Contents/MacOS/indistarter
install -v -m 644 system_integration/MacOSX/pkg/IndiStarter.app/Contents/Resources/README.rtf $destdir/IndiStarter.app/Contents/Resources/
install -v -m 644 system_integration/MacOSX/pkg/IndiStarter.app/Contents/Resources/indistarter.icns $destdir/IndiStarter.app/Contents/Resources/

install -v -m 644 doc/indistarter.pdf    $destdir/IndiStarter.app/Contents/Resources/

