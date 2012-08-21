#!/bin/bash

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
## 
##   Release build script
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
##   Public License 3. Consult the "LICENSE.txt" file for details.
## 
#############################################################################


dir=`PWD`

svn export http://svn.logtalk.org/logtalk/trunk lgt3000

cd lgt3000
chmod a+x scripts/cleandist.sh
scripts/cleandist.sh

cd ..
cp -R lgt3000/manuals man3000
tar -czf man3000.tgz man3000
tar -cjf lgt3000.tar.bz2 lgt3000

mkdir -p debian/usr/bin
mkdir -p debian/usr/share/doc/logtalk
mkdir -p debian/usr/share/doc-base
mkdir -p debian/usr/share/menu
mkdir -p debian/DEBIAN
cd lgt3000/scripts
./install.sh $dir/debian/usr
rm -rf $dir/debian/usr/share/mime
cp debian/logtalk.doc-base $dir/debian/usr/share/doc-base/logtalk-docs
cp debian/menu $dir/debian/usr/share/menu/logtalk
cp ../*.bib $dir/debian/usr/share/doc/logtalk
cp ../*.txt $dir/debian/usr/share/doc/logtalk
cp debian/copyright $dir/debian/usr/share/doc/logtalk
cp debian/changelog $dir/debian/usr/share/doc/logtalk
cp debian/changelog.Debian $dir/debian/usr/share/doc/logtalk
gzip --best $dir/debian/usr/share/doc/logtalk/*.bib 
gzip --best $dir/debian/usr/share/doc/logtalk/*.txt 
gzip --best $dir/debian/usr/share/doc/logtalk/changelog 
gzip --best $dir/debian/usr/share/doc/logtalk/changelog.Debian
cp debian/control $dir/debian/DEBIAN
cp debian/postinst $dir/debian/DEBIAN
cp debian/prerm $dir/debian/DEBIAN
cp debian/postrm $dir/debian/DEBIAN
cd $dir
dpkg-deb --build debian logtalk_3.00.0-1_all.deb

md5="`md5 -q lgt3000.tar.bz2`"
sha1="`openssl sha1 -r lgt3000.tar.bz2 | xargs -L 1 | sed 's/*lgt3000.tar.bz2//g'`"
rmd160="`openssl rmd160 -r lgt3000.tar.bz2 | xargs -L 1 | sed 's/*lgt3000.tar.bz2//g'`"
sudo mkdir -p /opt/local/var/macports/distfiles/logtalk
sudo cp -f lgt3000.tar.bz2 /opt/local/var/macports/distfiles/logtalk/lgt3000.tar.bz2
cd /opt/local/var/macports/sources/rsync.macports.org/release/ports/lang/logtalk/
sudo mv -f Portfile Portfile.old
sudo cp $dir/lgt3000/scripts/macosx/Portfile .
sudo sed -e 's/^version.*/version 3.00.0/' -i '' Portfile
sudo sed -e "s/sha1.*/sha1 $sha1 \\\/" -i '' Portfile
sudo sed -e "s/rmd160.*/rmd160 $rmd160/" -i '' Portfile
sudo port clean --archive logtalk
sudo port destroot logtalk
sudo port pkg logtalk
cp -R work/logtalk-3.00.0.pkg $dir
sudo port clean logtalk

cd $dir
mkdir manpdf3000
cd man3000/userman
./userman.sh
mv userman.pdf ../../manpdf3000/lgtuserman3000.pdf
cd ../refman
./refman.sh
mv refman.pdf ../../manpdf3000/lgtrefman3000.pdf
cd ../..
tar -czf manpdf3000.tgz manpdf3000
