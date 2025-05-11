#!/usr/bin/env bash

#############################################################################
##
##   Release build script
##   Last updated on May 11, 2025
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


# Default values for build options
BUILD_DEB=true
BUILD_RPM=true
BUILD_PKG=true
BUILD_MANUALS=true
BUILD_PACK=true
BUILD_PACK_EXPERIMENTAL=true

# Default git repository URL
GIT_REPO_URL="https://github.com/LogtalkDotOrg/logtalk3.git"

# Function to display help message
show_help() {
    echo "Usage: $0 [options]"
    echo ""
    echo "Options:"
    echo "  --help                   Display this help message"
    echo "  --no-deb                 Skip building the .deb installer (Debian/Ubuntu)"
    echo "  --no-rpm                 Skip building the .rpm installer (Red Hat/Fedora/CentOS)"
    echo "  --no-pkg                 Skip building the .pkg installer (macOS)"
    echo "  --no-manuals             Skip building the manuals archive"
    echo "  --no-pack                Skip building the pack archive"
    echo "  --no-pack-experimental   Skip building the pack-experimental archive"
    echo "  --git-repo-url=URL       Specify the git repository URL to clone from"
    echo "                           (default: $GIT_REPO_URL)"
    echo ""
    echo "By default, all components are built."
    exit 0
}

# Parse command line arguments
while [ $# -gt 0 ]; do
    case "$1" in
        --help)
            show_help
            ;;
        --no-deb)
            BUILD_DEB=false
            ;;
        --no-rpm)
            BUILD_RPM=false
            ;;
        --no-pkg)
            BUILD_PKG=false
            ;;
        --no-manuals)
            BUILD_MANUALS=false
            ;;
        --no-pack)
            BUILD_PACK=false
            ;;
        --no-pack-experimental)
            BUILD_PACK_EXPERIMENTAL=false
            ;;
        --git-repo-url=*)
            GIT_REPO_URL="${1#*=}"
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information."
            exit 1
            ;;
    esac
    shift
done

echo "Cloning from repository: $GIT_REPO_URL"
git clone --depth=1 "$GIT_REPO_URL" lgtclone
version=$(sed -e 's/-stable$//' < lgtclone/VERSION.txt)
mv lgtclone "logtalk-$version"

directory=$(PWD)

cd "logtalk-$version" || exit 1
chmod a+x scripts/cleandist.sh
scripts/cleandist.sh
cd ..

# Ensure that the Handbook and APIs documentation is up-to-date
"logtalk-$version"/docs/handbook/sources/build.sh
"logtalk-$version"/docs/apis/sources/build.sh

# Build manuals archive if enabled
if [ "$BUILD_MANUALS" = true ]; then
    echo "Building manuals archive..."
    cp -R "logtalk-$version/docs" "logtalk-manuals-$version"
    tar -czf "logtalk-manuals-$version.tgz" "logtalk-manuals-$version"
else
    echo "Skipping manuals archive build."
fi

tar -cjf "logtalk-$version.tar.bz2" "logtalk-$version"

# Build pack archive if enabled
if [ "$BUILD_PACK" = true ]; then
    echo "Building pack archive..."
    cp -R "logtalk-$version/scripts/pack" "pack-$version"
    cp -R "logtalk-$version" "pack-$version/logtalk"
    mv "pack-$version/settings.lgt" "pack-$version/logtalk/logtalk-$version"
    cd "pack-$version" || exit 1
    tar zcvf "logtalk-$version.tgz" logtalk
    mv "logtalk-$version.tgz" ..
    cd ..
else
    echo "Skipping pack archive build."
fi

# Build pack-experimental archive if enabled
if [ "$BUILD_PACK_EXPERIMENTAL" = true ]; then
    echo "Building pack-experimental archive..."
    cp -R "logtalk-$version/scripts/pack-experimental" "pack-experimental-$version"
    cp -R "logtalk-$version" "pack-experimental-$version/logtalk"
    mv "pack-experimental-$version/settings.lgt" "pack-experimental-$version/logtalk/logtalk-$version"
    cd "pack-experimental-$version" || exit 1
    tar zcvf "logtalk-experimental-$version.tgz" logtalk
    mv "logtalk-experimental-$version.tgz" ..
    cd ..
else
    echo "Skipping pack-experimental archive build."
fi

# Build .deb installer if enabled
if [ "$BUILD_DEB" = true ]; then
    echo "Building .deb installer..."
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
    # Use appropriate sed syntax based on the system
    case $(sed --help 2>&1) in
        *GNU*) sudo sed -i "s/^Version:.*/Version: $version/" "$directory/debian/DEBIAN/control" ;;
        *) sudo sed -i '' "s/^Version:.*/Version: $version/" "$directory/debian/DEBIAN/control" ;;
    esac
    cp debian/postinst "$directory/debian/DEBIAN"
    cp debian/prerm "$directory/debian/DEBIAN"
    cp debian/postrm "$directory/debian/DEBIAN"
    cd "$directory" || exit 1
    dpkg-deb --build debian "logtalk_$version-1_all.deb"
else
    echo "Skipping .deb installer build."
fi

# Build .pkg installer if enabled
if [ "$BUILD_PKG" = true ]; then
    # Calculate checksums for the tarball (only needed for .pkg installer)
    echo "Computing checksums for the tarball..."
    rmd160="$(openssl dgst -rmd160 "logtalk-$version.tar.bz2" | sed 's/^.* //')"
    sha256="$(openssl dgst -sha256 "logtalk-$version.tar.bz2" | sed 's/^.* //')"
    size=$(ls -dnL -- "logtalk-$version.tar.bz2" | awk '{print $5;exit}')
    echo "Building .pkg installer..."
    sudo mkdir -p /opt/local/var/macports/distfiles/logtalk
    sudo cp -f "logtalk-$version.tar.bz2" "/opt/local/var/macports/distfiles/logtalk/logtalk-$version.tar.bz2"
    cd /opt/local/var/macports/sources/rsync.macports.org/macports/release/tarballs/ports/lang/logtalk/ || exit 1
    sudo mv -f Portfile Portfile.old
    sudo cp "$directory/logtalk-$version/scripts/macos/Portfile" .
    # Use appropriate sed syntax based on the system for Portfile edits
    case $(sed --help 2>&1) in
        *GNU*)
            sudo sed -i "s/^version.*/version $version/" Portfile
            sudo sed -i "s/rmd160.*/rmd160 $rmd160 \\\\/" Portfile
            sudo sed -i "s/sha256.*/sha256 $sha256 \\\\/" Portfile
            sudo sed -i "s/size.*/size $size/" Portfile
            ;;
        *)
            sudo sed -i '' "s/^version.*/version $version/" Portfile
            sudo sed -i '' "s/rmd160.*/rmd160 $rmd160 \\\\/" Portfile
            sudo sed -i '' "s/sha256.*/sha256 $sha256 \\\\/" Portfile
            sudo sed -i '' "s/size.*/size $size/" Portfile
            ;;
    esac
    sudo port clean logtalk
    sudo port destroot logtalk
    sudo port pkg logtalk
    cp -R "work/logtalk-$version.pkg" "$directory"
    sudo port clean logtalk
else
    echo "Skipping .pkg installer build."
fi

# Build .rpm installer if enabled
if [ "$BUILD_RPM" = true ]; then
    echo "Building .rpm installer..."
    cd "$directory/logtalk-$version/scripts/linux" || exit 1
    ./build_rpm.sh
    mv "$HOME"/rpmbuild/RPMS/noarch/logtalk-*.rpm "$directory"
else
    echo "Skipping .rpm installer build."
fi

cd "$directory" || exit 1

openssl sha256 logtalk-$version.tar.bz2 || true
openssl sha256 logtalk-$version.pkg.zip || true
openssl sha256 logtalk-$version-1.noarch.rpm || true
openssl sha256 logtalk_$version-1_all.deb || true
openssl sha256 logtalk-manuals-$version.tgz || true
openssl sha256 logtalk-$version.exe || true
openssl sha256 logtalk-$version.tgz || true
openssl sha256 logtalk-experimental-$version.tgz || true
