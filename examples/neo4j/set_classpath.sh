#!/usr/bin/env bash

#############################################################################
## 
##   Set CLASSPATH environment variable
##   Last updated on May 12, 2026
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


NEO4J="$(neo4j status --verbose | grep 'app.home' | sed 's/.*-Dapp.home=\(.*\),.*/\1/')"
CLASSPATH=

# Only use the client-side driver jars. Loading the full server lib directory
# pulls in server classes that may target a newer JVM than the one used by the
# Prolog Java bridge, causing UnsupportedClassVersionError exceptions.
for pattern in \
	'neo4j-java-driver-*.jar' \
	'neo4j-bolt-connection*.jar' \
	'netty-*.jar' \
	'reactive-streams-*.jar' \
	'reactor-*.jar' \
	'slf4j-api-*.jar'
do
	for jar in "$NEO4J"/lib/$pattern; do
		if [ -e "$jar" ]; then
			if [ -n "$CLASSPATH" ]; then
				CLASSPATH="$CLASSPATH:$jar"
			else
				CLASSPATH="$jar"
			fi
		fi
	done
done

export CLASSPATH
