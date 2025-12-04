#!/bin/bash 

version=2.5.1

builddir=/tmp/indistarter  # Be sure this is set to a non existent directory, it is removed after the run!

arch=$(arch)

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi

save_PATH=$PATH
wd=`pwd`

currentrev=$(git rev-list --count --first-parent HEAD)

echo $version - $currentrev


# delete old files
  rm indistarter-qt6-*_arm64.tar.bz2
  rm indistarter-qt6_*_arm64.deb
  rm -rf $builddir

# make Linux arm version
  ./configure $configopt prefix=$builddir target=aarch64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=aarch64 OS_TARGET=linux LCL_PLATFORM=qt6 clean
  make CPU_TARGET=aarch64 OS_TARGET=linux LCL_PLATFORM=qt6 opt_target=-k--build-id
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install CPU_TARGET=aarch64
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  cd ..
  tar cvjf indistarter-qt6-$version-$currentrev-linux_arm64.tar.bz2 indistarter
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv indistarter*.tar.bz2 $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn system_integration/Linux/debian $builddir
  cd $builddir
  mkdir debian/indistarterarmqt6/usr/
  mv bin debian/indistarterarmqt6/usr/
  mv share debian/indistarterarmqt6/usr/
  cd debian
  sz=$(du -s indistarterarmqt6/usr | cut -f1)
  sed -i "s/%size%/$sz/" indistarterarmqt6/DEBIAN/control
  sed -i "/Version:/ s/3/$version-$currentrev/" indistarterarmqt6/DEBIAN/control
  fakeroot dpkg-deb -Zxz --build indistarterarmqt6 .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv indistarter*.deb $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir

