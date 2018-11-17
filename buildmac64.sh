#!/bin/bash 

version=1.3.0

basedir=/tmp/indistarter  # Be sure this is set to a non existent directory, it is removed after the run!

builddir=$basedir/indistarter

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi

wd=`pwd`

currentrev=$(git rev-list --count --first-parent HEAD)

echo $version - $currentrev

# delete old files
  rm indistarter*.dmg
  rm -rf $basedir

# make x86_64 Mac version
  ./configure $configopt prefix=$builddir target=x86_64-darwin
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=x86_64 clean
  make CPU_TARGET=x86_64
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install
  if [[ $? -ne 0 ]]; then exit 1;fi
  # pkg
  sed -i.bak "18s/1.0/$version/"  $builddir/IndiStarter.app/Contents/Info.plist
  rm $builddir/IndiStarter.app/Contents/Info.plist.bak
  cp system_integration/MacOSX/indistarter.pkgproj $basedir
  cp system_integration/MacOSX/readme.txt $basedir
  cd $basedir
  sed -i.bak "s/indistarter_version/$version/g" indistarter.pkgproj 
  rm indistarter.pkgproj.bak
  sed -i.bak "s/indistarter_version/$version/" readme.txt
  rm readme.txt.bak
  packagesbuild -v indistarter.pkgproj
  if [[ $? -ne 0 ]]; then exit 1;fi
  cp readme.txt build/
  hdiutil create -anyowners -volname indistarter-$version-$currentrev-x86_64-macosx -imagekey zlib-level=9 -format UDZO -srcfolder ./build indistarter-$version-$currentrev-x86_64-macosx.dmg
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv indistarter*.dmg $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $basedir

