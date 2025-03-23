#!/usr/bin/env bash

#############################################################################
##
##   Logtalk script for updating the HTML versions of man pages
##
##   Last updated on March 23, 2025
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


# allow using this script from any directory
cd "$(dirname "$0")" || exit 1

print_version() {
	echo "$(basename "$0") 0.2"
	exit 0
}

usage_help() {
	echo
	echo "This script updates the HTML versions of the man pages."
	echo
	echo "Usage:"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -v print version of $(basename "$0")"
	echo "  -h help"
	echo
}

while getopts "vh" option
do
	case $option in
		v) print_version;;
		h) usage_help;;
		*) usage_help; exit 1;;
	esac
done

if ! [ -x "$(command -v roffit)" ] ; then
	echo "Error: Cannot find the roffit command-line tool!" >&2
	echo "See https://github.com/bagder/roffit for installation instructions." >&2
	exit 1
fi

cd ../man/man1 || exit 1
for file in *.1; do
	roffit < "$file" > "${file%.*}".html
done

exit 0
