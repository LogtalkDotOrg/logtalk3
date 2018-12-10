#!/usr/bin/env bash

#############################################################################
## 
##   Set CLASSPATH environment variable for SWI-Prolog
##   Last updated on October 29, 2018
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


eval $(yap -dump-runtime-variables)
CLASSPATH="$PLBASE/lib/jpl.jar"

for jar in jars/*; do
	CLASSPATH="$PWD/$jar":$CLASSPATH
done

export CLASSPATH
