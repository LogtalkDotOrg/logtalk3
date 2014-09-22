#!/bin/bash

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
## 
##   Logtalk RPM package build script
##   Last updated on September 22, 2013
## 
##   This program is free software: you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, either version 3 of the License, or
##   (at your option) any later version.
##   
##   This program is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##   
##   You should have received a copy of the GNU General Public License
##   along with this program.  If not, see <http://www.gnu.org/licenses/>.
##   
##   Additional licensing terms apply per Section 7 of the GNU General
##   Public License 3. Consult the `LICENSE.txt` file for details.
## 
#############################################################################

version=`cat ../../VERSION.txt`
version_clean=`echo $version | sed -e 's/-stable$//'`
archive=logtalk-$version

directory="$PWD"

cd ../..
tar -cjf ~/rpmbuild/SOURCES/$archive.tar.bz2 .
mkdir -p ~/rpmbuild/RPMS/noarch

cd "$directory"
sed -e 's/LOGTALK_VERSION/'$version_clean'/g' -e 's/LOGTALK_INSTALL_DIRECTORY/'$archive'/g' logtalk.spec.in > logtalk.spec
rpmbuild -ba --target=noarch-*-linux logtalk.spec

cd ~/rpmbuild/RPMS/noarch
echo $PWD
ls -l
