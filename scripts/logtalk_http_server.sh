#!/usr/bin/env bash

#############################################################################
##
##   Local HTTP server script
##   Last updated on August 7, 2026
##
##   This file is part of Logtalk <https://logtalk.org/>
##   SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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
	echo "$(basename "$0") 1.0"
	exit 0
}

usage_help() {
	echo
	echo "This script starts a local HTTP server for a directory."
	echo
	echo "Usage:"
	echo "  $(basename "$0") -p prolog [-n port] [directory]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Required arguments:"
	echo "  -p backend Prolog compiler"
	echo "     (valid values are eclipse, sicstus, swi, trealla, and xvm)"
	echo
	echo "Optional arguments:"
	echo "  -n server port (default is 8080)"
	echo "  directory to serve (default is the current directory)"
	echo "  -v print version"
	echo "  -h help"
	echo
}

prolog_atom() {
	printf '%s' "$1" | sed "s/'/''/g"
}

port=8080

while getopts "p:n:vh" option; do
	case $option in
		p) backend="$OPTARG";;
		n) port="$OPTARG";;
		v) print_version;;
		h) usage_help; exit 0;;
		*) usage_help; exit 1;;
	esac
done

shift $((OPTIND - 1))

if [ "$backend" == "" ] ; then
	echo "Error! Backend Prolog compiler not specified!" >&2
	usage_help
	exit 1
fi

if [ "$#" -gt 1 ] ; then
	echo "Error! More than one directory specified!" >&2
	usage_help
	exit 1
fi

if [[ ! "$port" =~ ^[0-9]+$ ]] || [ "$port" -lt 1 ] || [ "$port" -gt 65535 ] ; then
	echo "Error! Invalid server port: $port" >&2
	exit 1
fi

document_root=${1:-$PWD}
if [ ! -d "$document_root" ] ; then
	echo "Error! Directory does not exist: $document_root" >&2
	exit 1
fi

operating_system=$(uname -s)
if [ "${operating_system:0:10}" == "MINGW32_NT" ] || [ "${operating_system:0:10}" == "MINGW64_NT" ] ; then
	extension='.sh'
	document_root=$(cd "$document_root" && pwd -W)
elif [ "$LOGTALKHOME" != "" ] && [ "$LOGTALKUSER" != "" ] && [ "$LOGTALKHOME" == "$LOGTALKUSER" ] ; then
	# assume that we're running Logtalk without using the installer scripts
	extension='.sh'
	document_root=$(cd "$document_root" && pwd -P)
else
	extension=''
	document_root=$(cd "$document_root" && pwd -P)
fi

dot=""
case "$backend" in
	eclipse)  logtalk="eclipselgt$extension";  goal_options=(-e);;
	xvm)      logtalk="xvmlgt$extension";      goal_options=(-q -g); dot=".";;
	sicstus)  logtalk="sicstuslgt$extension";  goal_options=(--noinfo --goal); dot=".";;
	swi)      logtalk="swilgt$extension";      goal_options=(-q -g);;
	trealla)  logtalk="tplgt$extension";       goal_options=(-q -g);;
	*)
		echo "Error! Unsupported backend Prolog compiler: $backend" >&2
		usage_help
		exit 1
		;;
esac

if ! command -v "$logtalk" >/dev/null 2>&1 ; then
	echo "Error! $logtalk integration script not found." >&2
	echo "       Check that its directory is in your execution path." >&2
	exit 1
fi

if [ "$LOGTALKHOME" != "" ] && [ -f "$LOGTALKHOME/scripts/logtalk_http_server.lgt" ] ; then
	driver="$LOGTALKHOME/scripts/logtalk_http_server.lgt"
else
	script_path=$0
	while [ -h "$script_path" ] ; do
		script_directory=$(cd "$(dirname "$script_path")" && pwd -P)
		script_path=$(readlink "$script_path")
		case "$script_path" in
			/*) ;;
			*) script_path="$script_directory/$script_path";;
		esac
	done
	script_directory=$(cd "$(dirname "$script_path")" && pwd -P)
	driver="$script_directory/logtalk_http_server.lgt"
fi

if [ ! -f "$driver" ] ; then
	echo "Error! Logtalk HTTP server driver not found: $driver" >&2
	exit 1
fi

document_root_atom=$(prolog_atom "$document_root")
driver_atom=$(prolog_atom "$driver")
goal="set_logtalk_flag(report,warnings),logtalk_load(http_server(loader)),logtalk_load(http_static_files(loader)),logtalk_load('$driver_atom',[scratch_directory('$LOGTALKUSER/scratch')]),logtalk_http_server::serve($port,'$document_root_atom'),halt$dot"

exec "$logtalk" "${goal_options[@]}" "$goal"
