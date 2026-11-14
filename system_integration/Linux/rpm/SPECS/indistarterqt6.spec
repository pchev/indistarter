Summary: A simple program to run a INDI server.
Name: indistarter-qt6
Version: 3
Release: 1
Group: Sciences/Astronomy
License: GPLv3+
URL: http://indistarter.sourceforge.net
Packager: Patrick Chevalley
BuildRoot: %_topdir/%{name}
BuildArch: x86_64
Obsoletes: indistarter
Provides: indistarter
Conflicts: indistarter
Requires: libQt6Pas.so.6()(64bit) libglib-2.0.so.0
AutoReqProv: no

%description
Indistarter is a user interface to run a INDI server.

%files
%defattr(-,root,root)
/usr/bin/indistarter
/usr/bin/indigui
/usr/share/metainfo/indistarter.appdata.xml
/usr/share/applications/indistarter.desktop
/usr/share/applications/indigui.desktop
/usr/share/pixmaps/indistarter.png
/usr/share/pixmaps/indigui.png
/usr/share/icons/hicolor/16x16/apps/net.ap_i.indistarter.png
/usr/share/icons/hicolor/16x16/apps/net.ap_i.indigui.png
/usr/share/icons/hicolor/24x24/apps/net.ap_i.indistarter.png
/usr/share/icons/hicolor/24x24/apps/net.ap_i.indigui.png
/usr/share/icons/hicolor/32x32/apps/net.ap_i.indistarter.png
/usr/share/icons/hicolor/32x32/apps/net.ap_i.indigui.png
/usr/share/icons/hicolor/48x48/apps/net.ap_i.indistarter.png
/usr/share/icons/hicolor/48x48/apps/net.ap_i.indigui.png
/usr/share/icons/hicolor/64x64/apps/net.ap_i.indistarter.png
/usr/share/icons/hicolor/64x64/apps/net.ap_i.indigui.png
/usr/share/icons/hicolor/96x96/apps/net.ap_i.indistarter.png
/usr/share/icons/hicolor/96x96/apps/net.ap_i.indigui.png
/usr/share/icons/hicolor/128x128/apps/net.ap_i.indistarter.png
/usr/share/icons/hicolor/128x128/apps/net.ap_i.indigui.png
/usr/share/icons/hicolor/256x256/apps/net.ap_i.indistarter.png
/usr/share/icons/hicolor/256x256/apps/net.ap_i.indigui.png
/usr/share/doc/indistarter

