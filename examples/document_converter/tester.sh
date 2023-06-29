#!/usr/bin/env bash

#############################################################################
## 
##   Bash script to be sourced by the logtalk_tester script
##   Last updated on June 21, 2023
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


local OPTIND=1
local OPTERR=0
while getopts "p:" option
do
	case $option in
		p) backend=$OPTARG;;
		?) shift;;
	esac
done

if [ "$backend" == "swi" ] ; then
	source set_classpath_swi.sh
	export CLASSPATH
elif [ "$backend" == "swipack" ] ; then
	source set_classpath_swi.sh
	export CLASSPATH
elif [ "$backend" == "yap" ] ; then
	source set_classpath_yap.sh
	export CLASSPATH
elif [ "$backend" == "arriba" ] ; then
	source set_classpath_arriba.sh
	export CLASSPATH
elif [ "$backend" == "lvm" ] ; then
	source set_classpath_lvm.sh
	export CLASSPATH
fi

return 0
