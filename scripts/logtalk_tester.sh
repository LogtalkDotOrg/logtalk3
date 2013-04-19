#!/bin/bash

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
## 
##   Unit testing automation script
##   Last updated on December 22, 2012
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

# based on a unit test automation script contributed by Parker Jones

print_version() {
	echo "`basename $0` 0.9"
	exit 0
}

if [ "$LOGTALKHOME" != "" ] && [ "$LOGTALKUSER" != "" ] && [ "$LOGTALKHOME" = "$LOGTALKUSER" ] ; then
	# assume that we're running Logtalk without using the installer scripts
	extension='.sh'
else
	extension=''
fi

base="$PWD"
results="$base/tester_results"
backend=yap
prolog='YAP'
logtalk="yaplgt$extension -g"
mode='normal'

versions_goal="logtalk_load(library(tester_versions)),halt"
versions_goal_dot="logtalk_load(library(tester_versions)),halt."

tester_goal="logtalk_load(tester),halt"
tester_goal_dot="logtalk_load(tester),halt."

tester_debug_goal="set_logtalk_flag(debug,on),logtalk_load(tester),halt"
tester_debug_goal_dot="set_logtalk_flag(debug,on),logtalk_load(tester),halt."

usage_help()
{
	echo 
	echo "This script automates running unit tests found on the sub-directories"
	echo "of the directory containing this script."
	echo
	echo "Usage:"
	echo "  `basename $0` [-p prolog] [-m mode] [-d results]"
	echo "  `basename $0` -v"
	echo "  `basename $0` -h"
	echo
	echo "Optional arguments:"
	echo "  -v print version of `basename $0`"
	echo "  -p back-end Prolog compiler (default is $backend)"
	echo "     (possible values are b, cx, eclipse, gnu, lean, qp, sicstus, swi, xsb, xsb64, xsbmt, xsbmt64, and yap)"
	echo "  -m compilation mode (default is $mode)"
	echo "     (possible values are normal, debug, and all)"
	echo "  -d name of the sub-directory to store the test results (default is tester_results)"
	echo "  -h help"
	echo
	exit 0
}

while getopts "vp:m:d:h" option
do
	case $option in
		v) print_version;;
		p) p_arg="$OPTARG";;
		m) m_arg="$OPTARG";;
		d) d_arg="$OPTARG";;
		h) usage_help;;
		*) usage_help;;
	esac
done

if [ "$p_arg" = "b" ] ; then
	prolog='B-Prolog'
	logtalk="bplgt$extension -g"
elif [ "$p_arg" = "cx" ] ; then
	prolog='CxProlog'
	logtalk="cxlgt$extension --goal"
elif [ "$p_arg" = "eclipse" ] ; then
	prolog='ECLiPSe'
	logtalk="eclipselgt$extension -e"
elif [ "$p_arg" = "gnu" ] ; then
	prolog='GNU Prolog'
	logtalk="gplgt$extension --query-goal"
elif [ "$p_arg" = "lean" ] ; then
	prolog='Lean Prolog'
	logtalk="lplgt$extension"
elif [ "$p_arg" = "qp" ] ; then
	prolog='Qu-Prolog'
	logtalk="qplgt$extension -g"
	versions_goal=$versions_goal_dot
	tester_goal=$tester_goal_dot
	tester_debug_goal=$tester_debug_goal_dot
elif [ "$p_arg" = "sicstus" ] ; then
	prolog='SICStus Prolog'
	logtalk="sicstuslgt$extension --goal"
	versions_goal=$versions_goal_dot
	tester_goal=$tester_goal_dot
	tester_debug_goal=$tester_debug_goal_dot
elif [ "$p_arg" = "swi" ] ; then
	prolog='SWI-Prolog'
	logtalk="swilgt$extension -g"
elif [ "$p_arg" = "xsb" ] ; then
	prolog='XSB'
	logtalk="xsblgt$extension -e"
	versions_goal=$versions_goal_dot
	tester_goal=$tester_goal_dot
	tester_debug_goal=$tester_debug_goal_dot
elif [ "$p_arg" = "xsb64" ] ; then
	prolog='XSB 64 bits'
	logtalk="xsb64lgt$extension -e"
	versions_goal=$versions_goal_dot
	tester_goal=$tester_goal_dot
	tester_debug_goal=$tester_debug_goal_dot
elif [ "$p_arg" = "xsbmt" ] ; then
	prolog='XSB-MT'
	logtalk="xsbmtlgt$extension -e"
	versions_goal=$versions_goal_dot
	tester_goal=$tester_goal_dot
	tester_debug_goal=$tester_debug_goal_dot
elif [ "$p_arg" = "xsbmt64" ] ; then
	prolog='XSB-MT 64 bits'
	logtalk="xsbmt64lgt$extension -e"
	versions_goal=$versions_goal_dot
	tester_goal=$tester_goal_dot
	tester_debug_goal=$tester_debug_goal_dot
elif [ "$p_arg" = "yap" ] ; then
	prolog='YAP'
	logtalk="yaplgt$extension -g"
elif [ "$p_arg" != "" ] ; then
	echo "Error! Unsupported back-end Prolog compiler: $p_arg"
	usage_help
	exit 1
elif [ ! `which $backend` ] ; then
    echo "Error! Default back-end Prolog compiler not found: $prolog"
	usage_help
    exit 1
fi

if [ "$m_arg" = "normal" ] ; then
	mode='normal'
elif [ "$m_arg" = "debug" ] ; then
	mode='debug'
elif [ "$m_arg" = "all" ] ; then
	mode='all'
elif [ "$m_arg" != "" ] ; then
	echo "Error! Unknow compilation mode: $m_arg"
	usage_help
	exit 1
fi

if [ "$d_arg" != "" ] ; then
	results="$base/$d_arg"
fi

mkdir -p "$results"
rm -f "$results"/*.results
rm -f "$results"/*.errors
rm -f "$results"/errors.all
rm -f "$results"/tester_versions.txt

date=`eval date \"+%Y-%m-%d %H:%M:%S\"`

echo '*******************************************************************************'
echo "***** Running unit tests"
echo "*****         Date: $date"
$logtalk $versions_goal > "$results"/tester_versions.txt 2> /dev/null
grep "Logtalk version:" "$results"/tester_versions.txt
grep "Prolog version:" "$results"/tester_versions.txt | sed "s/Prolog/$prolog/"
for unit in *
do
	if [ -d $unit ] ; then
		cd $unit
		if [ -e "./tester.lgt" ] ; then
			echo '*******************************************************************************'
			echo "***** Testing $unit"
			name=$(echo $unit|sed 's|/|__|g')
			if [ $mode = 'normal' ] || [ $mode = 'all' ] ; then
				$logtalk $tester_goal > "$results/$name.results" 2> "$results/$name.errors"
				grep 'tests:' "$results/$name.results" | sed 's/%/*****        /'
			fi
			if [ $mode = 'debug' ] || [ $mode = 'all' ] ; then
				$logtalk $tester_debug_goal > "$results/$name.results" 2> "$results/$name.errors"
				grep 'tests:' "$results/$name.results" | sed 's/%/***** (debug)/'
			fi
			grep 'out of' "$results/$name.results" | sed 's/%/*****        /'
			grep 'no coverage information collected' "$results/$name.results" | sed 's/%/*****        /'
			grep '(not applicable)' "$results/$name.results" | sed 's/(/*****         (/'
		fi
		for subunit in *
		do
			if [ -d $subunit ] ; then
				cd $subunit
				if [ -e "./tester.lgt" ] ; then
					echo '*******************************************************************************'
					echo "***** Testing $unit/$subunit"
					subname=$(echo $unit/$subunit|sed 's|/|__|g')
					if [ $mode = 'normal' ] || [ $mode = 'all' ] ; then
						$logtalk $tester_goal > "$results/$subname.results" 2> "$results/$subname.errors"
						grep 'tests:' "$results/$subname.results" | sed 's/%/*****        /'
					fi
					if [ $mode = 'debug' ] || [ $mode = 'all' ] ; then
						$logtalk $tester_debug_goal > "$results/$subname.results" 2> "$results/$subname.errors"
						grep 'tests:' "$results/$subname.results" | sed 's/%/***** (debug)/'
					fi
					grep 'out of' "$results/$subname.results" | sed 's/%/*****        /'
					grep 'no coverage information collected' "$results/$subname.results" | sed 's/%/*****        /'
					grep '(not applicable)' "$results/$subname.results" | sed 's/(/*****         (/'
				fi
				cd ..
			fi
		done
		cd "$base"
	fi
done

echo '*******************************************************************************'
echo "***** Compilation errors and warnings (might be expected depending on the test)"
echo '*******************************************************************************'
cd "$results"
grep -A2 'syntax_error' *.results | sed 's/.results//' | tee errors.all
grep -A2 'syntax_error' *.errors | sed 's/.errors//' | tee -a errors.all
grep -A2 '!     ' *.errors | sed 's/.errors//' | tee -a errors.all
grep -A2 '!     ' *.results | sed 's/.results//' | tee -a errors.all
grep -A2 '*     ' *.errors | sed 's/.errors//' | tee -a errors.all
grep -A2 '*     ' *.results | sed 's/.results//' | tee -a errors.all
echo '*******************************************************************************'
echo "***** Failed tests"
echo '*******************************************************************************'
grep ': failure' *.results | sed 's/: failure/ failed/' | sed 's/.results/:/' | sed 's|__|/|g' | tee -a errors.all
echo '*******************************************************************************'
