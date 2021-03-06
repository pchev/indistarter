#!/bin/bash

destdir=$1

cpu_target=$2

if [ -z "$destdir" ]; then
   export destdir=/tmp/indistarter
fi

echo Install indistarter to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/bin
install -m 755 -d $destdir/share
install -m 755 -d $destdir/share/applications
install -m 755 -d $destdir/share/doc
install -m 755 -d $destdir/share/doc/indistarter
install -m 755 -d $destdir/share/pixmaps
install -m 755 -d $destdir/share/icons
install -m 755 -d $destdir/share/icons/hicolor
install -m 755 -d $destdir/share/icons/hicolor/48x48
install -m 755 -d $destdir/share/icons/hicolor/48x48/apps
install -m 755 -d $destdir/share/metainfo

install -v -m 755 -s indistarter  $destdir/bin/indistarter
install -v -m 755 -s indigui  $destdir/bin/indigui
install -v -m 644 system_integration/Linux/share/applications/indistarter.desktop $destdir/share/applications/indistarter.desktop
install -v -m 644 system_integration/Linux/share/applications/indigui.desktop $destdir/share/applications/indigui.desktop
install -v -m 644 system_integration/Linux/share/metainfo/indistarter.appdata.xml $destdir/share/metainfo/indistarter.appdata.xml
install -v -m 644 system_integration/Linux/share/doc/indistarter/changelog $destdir/share/doc/indistarter/changelog
install -v -m 644 system_integration/Linux/share/doc/indistarter/copyright $destdir/share/doc/indistarter/copyright
install -v -m 644 doc/indistarter.pdf $destdir/share/doc/indistarter/indistarter.pdf
install -v -m 644 system_integration/Linux/share/pixmaps/indistarter.png $destdir/share/pixmaps/indistarter.png
install -v -m 644 system_integration/Linux/share/pixmaps/indigui.png $destdir/share/pixmaps/indigui.png
install -v -m 644 system_integration/Linux/share/icons/hicolor/48x48/apps/indistarter.png $destdir/share/icons/hicolor/48x48/apps/indistarter.png
install -v -m 644 system_integration/Linux/share/icons/hicolor/48x48/apps/indigui.png $destdir/share/icons/hicolor/48x48/apps/indigui.png
