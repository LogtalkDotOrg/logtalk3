#!/usr/bin/env bash

#############################################################################
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

set -eu

if [[ $# -ne 5 ]]; then
	printf 'Usage: %s REPOSITORY COMMIT SOURCE LICENSE OUTPUT\n' "$0" >&2
	exit 1
fi

REPOSITORY=$1
COMMIT=$2
SOURCE=$3
LICENSE_FILE=$4
OUTPUT=$5
INPUT=$SOURCE
CLEANED_SOURCE=

cleanup() {
	if [[ -n "$CLEANED_SOURCE" ]]; then
		rm -f "$CLEANED_SOURCE"
	fi
}

trap cleanup EXIT

if [[ ! "$REPOSITORY" =~ ^stopwords-[a-z0-9-]+$ ]]; then
	printf 'Error: invalid stopwords-iso repository name: %s\n' "$REPOSITORY" >&2
	exit 1
fi

if [[ ! "$COMMIT" =~ ^[0-9a-f]{40}$ ]]; then
	printf 'Error: commit must be a full lowercase SHA-1: %s\n' "$COMMIT" >&2
	exit 1
fi

if ! grep -q '^The MIT License (MIT)$' "$LICENSE_FILE" ||
		! grep -q '^Copyright (c) 2016 Gene Diaz$' "$LICENSE_FILE"; then
	printf 'Error: unsupported source license: %s\n' "$LICENSE_FILE" >&2
	exit 1
fi

if [[ "$REPOSITORY" == stopwords-en ]]; then
	CLEANED_SOURCE=$(mktemp "${TMPDIR:-/tmp}/stopwords-en.XXXXXX")
	LC_ALL=C grep '^[ -~]*$' "$SOURCE" > "$CLEANED_SOURCE"
	INPUT=$CLEANED_SOURCE
	SOURCE_COUNT=$(awk 'END {print NR}' "$SOURCE")
	COUNT=$(awk 'END {print NR}' "$INPUT")
	if [[ $((SOURCE_COUNT - COUNT)) -ne 4 ]]; then
		printf 'Error: expected four malformed non-ASCII English entries in source: %s\n' "$SOURCE" >&2
		exit 1
	fi
	printf 'Removed four malformed non-ASCII English entries\n'
else
	COUNT=$(awk 'END {print NR}' "$INPUT")
fi

if [[ -z "$COUNT" || "$COUNT" -eq 0 ]]; then
	printf 'Error: empty stop-word source: %s\n' "$INPUT" >&2
	exit 1
fi

if grep -q '^$' "$INPUT"; then
	printf 'Error: blank stop word in source: %s\n' "$INPUT" >&2
	exit 1
fi

if LC_ALL=C grep -q '[A-Z]' "$INPUT"; then
	printf 'Error: non-lowercase stop word in source: %s\n' "$INPUT" >&2
	exit 1
fi

if [[ $(LC_ALL=C sort "$INPUT" | uniq -d | wc -l | tr -d ' ') -ne 0 ]]; then
	printf 'Error: duplicate stop words in source: %s\n' "$INPUT" >&2
	exit 1
fi

OBJECT=${REPOSITORY//-/_}
LANGUAGE=${REPOSITORY#stopwords-}

awk -v count="$COUNT" -v repository="$REPOSITORY" -v commit="$COMMIT" \
		-v object="$OBJECT" -v language="$LANGUAGE" -v license_file="$LICENSE_FILE" '
	BEGIN {
		if (language != "en") {
			print ":- encoding(\047UTF-8\047)."
			print ""
		}
		print "/*"
		while ((getline line < license_file) > 0)
			print line
		close(license_file)
		print "*/"
		print ""
		print ""
		printf ":- object(%s,\n", object
		print "\timplements(stop_words_language_protocol))."
		print ""
		print "\t:- info(["
		print "\t\tversion is 1:0:0,"
		print "\t\tauthor is \047Gene Diaz\047,"
		print "\t\tdate is 2026-09-05,"
		printf "\t\tcomment is \047Stop-word facts for the %s language code.\047,\n", language
		print "\t\tcopyright is \047Copyright (c) 2016 Gene Diaz\047,"
		print "\t\tlicense is \047MIT\047,"
		print "\t\tremarks is ["
		printf "\t\t\t\047Source\047 - \047https://github.com/stopwords-iso/%s\047,\n", repository
		printf "\t\t\t\047Source commit\047 - \047%s\047,\n", commit
		if (language == "en")
			print "\t\t\t\047Source corrections\047 - \047Removed four malformed non-ASCII reflexive-pronoun entries.\047,"
		printf "\t\t\t\047Generated entries\047 - \047%d.\047\n", count
		print "\t\t],"
		print "\t\tsee_also is [stop_words(_, _)]"
		print "\t])."
		print ""
	}
	{
		gsub(/\047/, "\047\047")
		printf "\tstop_word(\047%s\047).\n", $0
	}
	END {
		print ""
		print ":- end_object."
	}
' "$INPUT" > "$OUTPUT"

printf 'Generated %s with %s stop words\n' "$OUTPUT" "$COUNT"
