#!/usr/bin/env bash

#############################################################################
## 
##   Logtalk script for updating the HTML versions of man pages
## 
##   Last updated on December 27, 2020
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


print_version() {
	echo "$(basename "$0") 0.1"
	exit 0
}

usage_help()
{
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
		h) usage_help; exit;;
		*) usage_help; exit;;
	esac
done

cd ../man/man1 || exit 1
for file in *.1; do
	roffit < "$file" > "${file%.*}".html
done

exit 0
