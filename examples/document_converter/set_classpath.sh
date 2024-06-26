#!/usr/bin/env bash

#############################################################################
## 
##   Set CLASSPATH environment variable for XVM
##   Last updated on February 14, 2024
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


cwd="$(dirname "${BASH_SOURCE[0]}")"

for jar in "$cwd"/jars/*.jar; do
	CLASSPATH=$CLASSPATH:"$jar"
done

export CLASSPATH

export TIKA_CONFIG="$cwd"/jars/tika-config.xml 
