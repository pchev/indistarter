#!/bin/bash 

version=2.4.2

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
  rm indistarter-qt6*.xz
  rm indistarter-qt6*.deb
  rm indistarter-qt6*.rpm
  rm -rf $builddir

# make Linux x86_64 version
  ./configure $configopt prefix=$builddir target=x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=x86_64 OS_TARGET=linux LCL_PLATFORM=qt6 clean
  make CPU_TARGET=x86_64 OS_TARGET=linux LCL_PLATFORM=qt6 opt_target=-k--build-id
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  cd ..
  tar cvJf indistarteri-qt6-$version-$currentrev-linux_x86_64.tar.xz indistarter
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv indistarter*.tar.xz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn system_integration/Linux/debian $builddir
  cd $builddir
  mkdir debian/indistarterqt6/usr/
  mv bin debian/indistarterqt6/usr/
  mv share debian/indistarterqt6/usr/
  cd debian
  sz=$(du -s indistarterqt6/usr | cut -f1)
  sed -i "s/%size%/$sz/" indistarterqt6/DEBIAN/control
  sed -i "/Version:/ s/3/$version-$currentrev/" indistarterqt6/DEBIAN/control
  fakeroot dpkg-deb -Zxz --build indistarterqt6 .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv indistarter*.deb $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # rpm
  cd $wd
  rsync -a --exclude=.svn system_integration/Linux/rpm $builddir
  cd $builddir
  mkdir -p rpm/RPMS/x86_64
  mkdir -p rpm/RPMS/i386
  mkdir rpm/SRPMS
  mkdir rpm/tmp
  mkdir -p rpm/indistarter/usr/
  mv debian/indistarterqt6/usr/* rpm/indistarter/usr/
  cd rpm
  sed -i "/Version:/ s/3/$version/"  SPECS/indistarterqt6.spec
  sed -i "/Release:/ s/1/$currentrev/" SPECS/indistarterqt6.spec
# rpm 4.7
  fakeroot rpmbuild  --buildroot "$builddir/rpm/indistarter" --define "_topdir $builddir/rpm/" --define "_binary_payload w7.xzdio"  -bb SPECS/indistarterqt6.spec
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv RPMS/x86_64/indistarter*.rpm $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir


