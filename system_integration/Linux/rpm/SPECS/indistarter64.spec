Summary: A simple program to run a INDI server.
Name: indistarter
Version: 3
Release: 1
Group: Sciences/Astronomy
License: GPLv3+
URL: http://indistarter.sourceforge.net
Packager: Patrick Chevalley
BuildRoot: %_topdir/%{name}
BuildArch: x86_64
Provides: indistarter
Requires: qt5pas glib2 libjpeg libpng
AutoReqProv: no

%description
Indistarter is a user interface to run a INDI server.

%files
%defattr(-,root,root)
/usr/bin/indistarter
/usr/bin/indistarter
/usr/bin/indigui
/usr/share/metainfo/indistarter.appdata.xml
/usr/share/applications/indistarter.desktop
/usr/share/applications/indigui.desktop
/usr/share/pixmaps/indistarter.png
/usr/share/pixmaps/indigui.png
/usr/share/icons/hicolor/48x48/apps/indistarter.png
/usr/share/icons/hicolor/48x48/apps/indigui.png
/usr/share/doc/indistarter

