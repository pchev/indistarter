#!/bin/bash 

version=2.5.1

basedir=/tmp/indistarter  # Be sure this is set to a non existent directory, it is removed after the run!

builddir=$basedir/indistarter

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi

wd=`pwd`

indiversion=$(ls indimac*.tgz| sed 's/indimac-//; s/.tgz//')

currentrev=$(git rev-list --count --first-parent HEAD)

echo $version - $currentrev - $indiversion

# delete old files
  rm indistarter*.dmg
  rm -rf $basedir

# make universal Mac version
  # x86_64
  ./configure $configopt prefix=$builddir target=x86_64-darwin
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=x86_64 clean
  make CPU_TARGET=x86_64
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install CPU_TARGET=x86_64
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/IndiStarter.app/Contents/MacOS/indistarter $builddir/IndiStarter.app/Contents/MacOS/indistarterx86_64
  # arm64
  ./configure $configopt prefix=$builddir target=aarch64-darwin
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=aarch64 clean
  make CPU_TARGET=aarch64
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install CPU_TARGET=aarch64
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/IndiStarter.app/Contents/MacOS/indistarter $builddir/IndiStarter.app/Contents/MacOS/indistarterarm64
  # create universal binary
  cd $builddir/IndiStarter.app/Contents/MacOS/
  lipo -create -output indistarter indistarterarm64 indistarterx86_64
  strip indistarter
  rm indistarterarm64 indistarterx86_64
  cd $wd
  # pkg
  sed -i.bak "18s/1.0/$version/"  $builddir/IndiStarter.app/Contents/Info.plist
  rm $builddir/IndiStarter.app/Contents/Info.plist.bak
  cp system_integration/MacOSX/indistarter.pkgproj $basedir
  cp system_integration/MacOSX/readme.txt $basedir
  cd $basedir
  sed -i.bak "s/indistarter_version/$version/g" indistarter.pkgproj 
  rm indistarter.pkgproj.bak
  sed -i.bak "s/indistarter_version/$version, INDI $indiversion/" readme.txt
  rm readme.txt.bak
  packagesbuild -v indistarter.pkgproj
  if [[ $? -ne 0 ]]; then exit 1;fi
  cp readme.txt build/
  sz=$(du -sk build| cut -f1)
  (( sz=50+(sz/1024) ))
  hdiutil create -size ${sz}M -anyowners -volname indistarter-$version-$currentrev-indi-$indiversion-macos -imagekey zlib-level=9 -format UDZO -srcfolder ./build indistarter-$version-$currentrev-indi-$indiversion-macos.dmg
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv indistarter*.dmg $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $basedir


