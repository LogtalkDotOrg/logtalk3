#!/usr/bin/env bash

#############################################################################
##
##   Packs virtual environment script
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


export LC_ALL=C

print_version() {
	echo "$(basename "$0") 0.5"
	exit 0
}

usage_help() {
	echo
	echo "This script creates a packs virtual environment in the current directory or"
	echo "in a specified directory by writing or appending to a .envrc file. Requires"
	echo "direnv to be installed."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-d directory] [-c] [-p packs]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -d directory where to create the virtual environment"
	echo "     (absolute path; default is the current directory)"
	echo "  -c create directory if it does not exist"
	echo "  -p packs sub-directory"
	echo "     (relative path; default is is the current directory)"
	echo "  -v print version"
	echo "  -h help"
	echo
}

# default argument values

base="$PWD"
create='false'

while getopts "vd:cp:h" option; do
	case $option in
		v) print_version;;
		d) d_arg="$OPTARG";;
		c) create='true';;
		p) p_arg="$OPTARG";;
		h) usage_help; exit 0;;
		*) usage_help; exit 1;;
	esac
done

if [ "$d_arg" == "" ] ; then
	directory="$base"
elif [ ! -d "$d_arg" ] ; then
	if [ "$create" == "true" ] ; then
		directory="$d_arg"
		mkdir "$directory"
	else
		echo "Error: directory $d_arg does not exist." >&2
		exit 3
	fi
elif [ ! -w "$d_arg" ] ; then
	echo "Error: directory $d_arg is not writable." >&2
	exit 5
else
	directory="$d_arg"
fi

if [ "$p_arg" == "" ] ; then
	packs="$directory"
elif [ ! -d "$directory/$p_arg" ] ; then
	packs="$directory/$p_arg"
	mkdir -p "$directory/$p_arg"
elif [ ! -w "$directory/$p_arg" ] ; then
	echo "Error: directory $directory/$p_arg is not writable." >&2
	exit 5
else
	packs="$directory/$p_arg"
fi

if ! [ -x "$(command -v direnv)" ]; then
	echo "Error! Cannot find the direnv command-line tool!" >&2
	echo "See https://direnv.net for installation instructions." >&2
	exit 7
fi

echo export LOGTALKPACKS="$packs" >> "$directory"/.envrc
direnv allow "$directory"
exit 0
