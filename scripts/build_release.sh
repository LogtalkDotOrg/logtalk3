#!/usr/bin/env bash

#############################################################################
## 
##   Release build script
##   Last updated on January 30, 2025
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
##   SPDX-License-Identifier: Apache-2.0
##   
##   Licensed under the Apache License, Version 2.0 (the "License");
##   you may not use this file except in compliance with the License.
##   You may obtain a copy of the License at
##   
##       http://www.apache.org/licenses/LICENSE-2.0
##   
##   Unless required by applicable law or agreed to in writing, software
##   distributed under the License is distributed on an "AS IS" BASIS,
##   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
##   See the License for the specific language governing permissions and
##   limitations under the License.
## 
#############################################################################


case $(sed --help 2>&1) in
  *GNU*) sed_i () { sed -i "$@"; };;
  *) sed_i () { sed -i '' "$@"; };;
esac

git clone --depth=1 https://github.com/LogtalkDotOrg/logtalk3.git lgtclone
version=$(sed -e 's/-stable$//' < lgtclone/VERSION.txt)
mv lgtclone "logtalk-$version"

directory=$(PWD)

cd "logtalk-$version" || exit 1
chmod a+x scripts/cleandist.sh
scripts/cleandist.sh
cd ..

"logtalk-$version"/manuals/sources/build_manuals.sh
cp -R "logtalk-$version/manuals" "logtalk-manuals-$version"
tar -czf "logtalk-manuals-$version.tgz" "logtalk-manuals-$version"

"logtalk-$version"/scripts/update_html_docs.sh

tar -cjf "logtalk-$version.tar.bz2" "logtalk-$version"

cp -R "logtalk-$version/scripts/pack" "pack-$version"
cp -R "logtalk-$version" "pack-$version/logtalk"
mv "pack-$version/settings.lgt" "pack-$version/logtalk/logtalk-$version"
cd "pack-$version" || exit 1
tar zcvf "logtalk-$version.tgz" logtalk
mv "logtalk-$version.tgz" ..
cd ..

cp -R "logtalk-$version/scripts/pack-experimental" "pack-experimental-$version"
cp -R "logtalk-$version" "pack-experimental-$version/logtalk"
mv "pack-experimental-$version/settings.lgt" "pack-experimental-$version/logtalk/logtalk-$version"
cd "pack-experimental-$version" || exit 1
tar zcvf "logtalk-experimental-$version.tgz" logtalk
mv "logtalk-experimental-$version.tgz" ..
cd ..

rm -rf debian
mkdir -p debian/usr/bin
mkdir -p debian/usr/share/doc/logtalk
mkdir -p debian/usr/share/doc-base
mkdir -p debian/usr/share/menu
mkdir -p debian/DEBIAN
cd "logtalk-$version/scripts" || exit 1
./install.sh -p "$directory/debian/usr"
rm -rf "$directory/debian/usr/share/mime"
cp debian/logtalk.doc-base "$directory/debian/usr/share/doc-base/logtalk-docs"
cp debian/menu "$directory/debian/usr/share/menu/logtalk"
cp ../*.bib "$directory/debian/usr/share/doc/logtalk"
cp ../*.txt "$directory/debian/usr/share/doc/logtalk"
cp debian/copyright "$directory/debian/usr/share/doc/logtalk"
cp debian/changelog "$directory/debian/usr/share/doc/logtalk"
cp debian/changelog.Debian "$directory/debian/usr/share/doc/logtalk"
gzip --best "$directory/debian/usr/share/doc/logtalk"/*.bib
gzip --best "$directory/debian/usr/share/doc/logtalk"/*.txt
gzip --best "$directory/debian/usr/share/doc/logtalk/changelog"
gzip --best "$directory/debian/usr/share/doc/logtalk/changelog.Debian"
cp debian/control "$directory/debian/DEBIAN"
sudo sed_i -e "s/^Version:.*/Version: $version/" "$directory/debian/DEBIAN/control"
cp debian/postinst "$directory/debian/DEBIAN"
cp debian/prerm "$directory/debian/DEBIAN"
cp debian/postrm "$directory/debian/DEBIAN"
cd "$directory" || exit 1
dpkg-deb --build debian "logtalk_$version-1_all.deb"

rmd160="$(openssl dgst -rmd160 "logtalk-$version.tar.bz2" | sed 's/^.* //')"
sha256="$(openssl dgst -sha256 "logtalk-$version.tar.bz2" | sed 's/^.* //')"
size=$(ls -dnL -- "logtalk-$version.tar.bz2" | awk '{print $5;exit}')
sudo mkdir -p /opt/local/var/macports/distfiles/logtalk
sudo cp -f "logtalk-$version.tar.bz2" "/opt/local/var/macports/distfiles/logtalk/logtalk-$version.tar.bz2"
cd /opt/local/var/macports/sources/rsync.macports.org/macports/release/tarballs/ports/lang/logtalk/ || exit 1
sudo mv -f Portfile Portfile.old
sudo cp "$directory/logtalk-$version/scripts/macos/Portfile" .
sudo sed_i -e "s/^version.*/version $version/" Portfile
sudo sed_i -e "s/rmd160.*/rmd160 $rmd160 \\\/" Portfile
sudo sed_i -e "s/sha256.*/sha256 $sha256 \\\/" Portfile
sudo sed_i -e "s/size.*/size $size/" Portfile
sudo port clean logtalk
sudo port destroot logtalk
sudo port pkg logtalk
cp -R "work/logtalk-$version.pkg" "$directory"
sudo port clean logtalk

cd "$directory/logtalk-$version/scripts/linux" || exit 1
./build_rpm.sh
mv "$HOME"/rpmbuild/RPMS/noarch/logtalk-*.rpm "$directory"

cd "$directory" || exit 1

openssl sha256 logtalk-$version.tar.bz2 || true
openssl sha256 logtalk-$version.pkg.zip || true
openssl sha256 logtalk-$version-1.noarch.rpm || true
openssl sha256 logtalk_$version-1_all.deb || true
openssl sha256 logtalk-manuals-$version.tgz || true
openssl sha256 logtalk-$version.exe || true
openssl sha256 logtalk-$version.tgz || true
openssl sha256 logtalk-experimental-$version.tgz || true
