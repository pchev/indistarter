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
Requires: gtk2 glib2 pango libjpeg libpng
AutoReqProv: no

%description
Indistarter is a user interface to run a INDI server.

%files
%defattr(-,root,root)
/usr/bin/indistarter
/usr/share/appdata/indistarter.appdata.xml
/usr/share/applications/indistarter.desktop
/usr/share/pixmaps/indistarter.png
/usr/share/icons/hicolor/48x48/apps/indistarter.png
/usr/share/doc/indistarter
