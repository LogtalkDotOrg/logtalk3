#!/bin/bash

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
## 
##   Release build script
##   Last updated on January 8, 2015
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


if [ -z "$1" ]; then
	git clone git://github.com/LogtalkDotOrg/logtalk3.git lgtclone
	version=`cat lgtclone/VERSION.txt | sed -e 's/-stable$//'`

	mv lgtclone logtalk-$version
else
	version="$1"
	git clone git://github.com/LogtalkDotOrg/logtalk3.git logtalk-$version
fi

directory=`PWD`

cd logtalk-$version
chmod a+x scripts/cleandist.sh
scripts/cleandist.sh

cd ..
cp -R logtalk-$version/manuals logtalk-manuals-$version
tar -czf logtalk-manuals-$version.tgz logtalk-manuals-$version
tar -cjf logtalk-$version.tar.bz2 logtalk-$version

rm -rf debian
mkdir -p debian/usr/bin
mkdir -p debian/usr/share/doc/logtalk
mkdir -p debian/usr/share/doc-base
mkdir -p debian/usr/share/menu
mkdir -p debian/DEBIAN
cd logtalk-$version/scripts
./install.sh -p $directory/debian/usr
rm -rf $directory/debian/usr/share/mime
cp debian/logtalk.doc-base $directory/debian/usr/share/doc-base/logtalk-docs
cp debian/menu $directory/debian/usr/share/menu/logtalk
cp ../*.bib $directory/debian/usr/share/doc/logtalk
cp ../*.txt $directory/debian/usr/share/doc/logtalk
cp debian/copyright $directory/debian/usr/share/doc/logtalk
cp debian/changelog $directory/debian/usr/share/doc/logtalk
cp debian/changelog.Debian $directory/debian/usr/share/doc/logtalk
gzip --best $directory/debian/usr/share/doc/logtalk/*.bib 
gzip --best $directory/debian/usr/share/doc/logtalk/*.txt 
gzip --best $directory/debian/usr/share/doc/logtalk/changelog 
gzip --best $directory/debian/usr/share/doc/logtalk/changelog.Debian
cp debian/control $directory/debian/DEBIAN
sudo sed -e "s/^Version:.*/Version: $version/" -i '' $directory/debian/DEBIAN/control
cp debian/postinst $directory/debian/DEBIAN
cp debian/prerm $directory/debian/DEBIAN
cp debian/postrm $directory/debian/DEBIAN
cd $directory
dpkg-deb --build debian logtalk_$version-1_all.deb

sha256="`openssl sha256 -r logtalk-$version.tar.bz2 | xargs -L 1 | sed 's/*.tar.bz2//g'`"
rmd160="`openssl rmd160 -r logtalk-$version.tar.bz2 | xargs -L 1 | sed 's/*.tar.bz2//g'`"
sudo mkdir -p /opt/local/var/macports/distfiles/logtalk
sudo cp -f logtalk-$version.tar.bz2 /opt/local/var/macports/distfiles/logtalk/logtalk-$version.tar.bz2
cd /opt/local/var/macports/sources/rsync.macports.org/release/tarballs/ports/lang/logtalk/
sudo mv -f Portfile Portfile.old
sudo cp $directory/logtalk-$version/scripts/macosx/Portfile .
sudo sed -e "s/^version.*/version $version/" -i '' Portfile
sudo sed -e "s/sha256.*/sha256 $sha256 \\\/" -i '' Portfile
sudo sed -e "s/rmd160.*/rmd160 $rmd160/" -i '' Portfile
sudo port clean logtalk
sudo port destroot logtalk
sudo port pkg logtalk
cp -R work/logtalk-$version.pkg $directory
sudo port clean logtalk

cd $directory
