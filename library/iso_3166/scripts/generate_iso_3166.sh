#!/usr/bin/env bash

#############################################################################
##
##   Last updated on May 5, 2026
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

if [ "$#" -ne 2 ]; then
	printf '%s\n' "usage: generate_iso_3166.sh M49_OVERVIEW_HTML TARGET_DIR" >&2
	exit 1
fi

SOURCE_HTML=$1
TARGET_DIR=$2

TMPDIR_PATH=$(mktemp -d "${TMPDIR:-/tmp}/iso_3166.XXXXXX")
trap 'rm -rf "$TMPDIR_PATH"' EXIT HUP INT TERM

FACTS_FILE="$TMPDIR_PATH/country.facts"
TARGET_FILE="$TARGET_DIR/iso_3166_1.lgt"
SOURCE_URL='https://unstats.un.org/unsd/methodology/m49/overview/'
TODAY=$(date +%F)

awk -f - "$SOURCE_HTML" > "$FACTS_FILE" <<'AWK'
function trim(value) {
	gsub(/\r/, "", value)
	gsub(/\n/, " ", value)
	gsub(/[[:space:]]+/, " ", value)
	sub(/^ /, "", value)
	sub(/ $/, "", value)
	return value
}

function utf8(code) {
	if (code < 128) {
		return sprintf("%c", code)
	}
	if (code < 2048) {
		return sprintf("%c%c", int(code / 64) + 192, (code % 64) + 128)
	}
	return sprintf("%c%c%c", int(code / 4096) + 224, int((code % 4096) / 64) + 128, (code % 64) + 128)
}

function decode(value,    code, replacement) {
	gsub(/&nbsp;|&#160;/, " ", value)
	gsub(/&amp;/, "\\&", value)
	gsub(/&quot;/, "\"", value)
	gsub(/&apos;|&#039;|&#39;/, "'", value)
	gsub(/&lt;/, "<", value)
	gsub(/&gt;/, ">", value)
	while (match(value, /&#x[0-9A-Fa-f]+;/)) {
		code = strtonum("0x" substr(value, RSTART + 3, RLENGTH - 4))
		replacement = utf8(code)
		value = substr(value, 1, RSTART - 1) replacement substr(value, RSTART + RLENGTH)
	}
	while (match(value, /&#[0-9]+;/)) {
		code = substr(value, RSTART + 2, RLENGTH - 3) + 0
		replacement = utf8(code)
		value = substr(value, 1, RSTART - 1) replacement substr(value, RSTART + RLENGTH)
	}
	return value
}

function clean(value) {
	gsub(/<br[[:space:]]*\/?>/, "\n", value)
	gsub(/<[^>]*>/, "", value)
	value = decode(value)
	value = trim(value)
	return value
}

function atom(value, copy) {
	copy = value
	gsub(/'/, "''", copy)
	return "'" copy "'"
}

function extract_cells(record, values,    parts, count, cell_index, total) {
	gsub(/\r/, "", record)
	count = split(record, parts, /<\/t[dh]>/)
	total = 0
	for (cell_index = 1; cell_index <= count; cell_index++) {
		if (parts[cell_index] ~ /<t[dh][^>]*>/) {
			sub(/^.*<t[dh][^>]*>/, "", parts[cell_index])
			values[++total] = clean(parts[cell_index])
		}
	}
	return total
}

BEGIN {
	RS = "</tr>"
	ORS = ""
	header_rows = 0
}

{
	count = extract_cells($0, values)
	if (count < 12) {
		next
	}
	if (values[9] == "Country or Area" && values[10] == "M49 Code" && values[11] == "ISO-alpha2 Code" && values[12] == "ISO-alpha3 Code") {
		header_rows++
		if (header_rows > 1) {
			exit
		}
		next
	}
	if (header_rows != 1) {
		next
	}
	name = values[9]
	numeric_text = values[10]
	alpha2 = tolower(values[11])
	alpha3 = tolower(values[12])
	if (name == "" || numeric_text == "" || alpha2 == "" || alpha3 == "") {
		next
	}
	if (alpha2 !~ /^[a-z][a-z]$/ || alpha3 !~ /^[a-z][a-z][a-z]$/ || numeric_text !~ /^[0-9][0-9][0-9]$/) {
		next
	}
	numeric = numeric_text + 0
	key = alpha2 SUBSEP alpha3 SUBSEP numeric SUBSEP name
	if (seen[key]++) {
		next
	}
	printf "\tcountry(%s, %s, %d, %s).\n", atom(alpha2), atom(alpha3), numeric, atom(name)
}
AWK

COUNT=$(awk 'END {print NR + 0}' "$FACTS_FILE")

cat > "$TARGET_FILE" <<EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(iso_3166_1).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is $TODAY,
		comment is 'Generated ISO 3166-1 country facts extracted from the UN M49 overview page, which republishes ISO alpha-2 and alpha-3 codes together with M49 numeric codes.',
		remarks is [
			'Source URL' - '$SOURCE_URL',
			'Generated entries' - '$COUNT'
		]
	]).

	:- public(country/4).
	:- mode(country(?atom, ?atom, ?integer, ?atom), zero_or_more).
	:- info(country/4, [
		comment is 'Generated ISO 3166-1 country fact table.',
		argnames is ['Alpha2', 'Alpha3', 'Numeric', 'Name']
	]).

EOF
cat "$FACTS_FILE" >> "$TARGET_FILE"
printf '\n:- end_object.\n' >> "$TARGET_FILE"

printf '%s\n' "Generated $TARGET_FILE with $COUNT entries from $SOURCE_HTML"
