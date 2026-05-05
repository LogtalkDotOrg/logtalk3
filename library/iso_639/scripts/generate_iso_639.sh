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

if [ "$#" -ne 4 ]; then
	printf '%s\n' "usage: generate_iso_639.sh ISO639_2_HTML ISO639_5_HTML ISO639_3_TAB TARGET_DIR" >&2
	exit 1
fi

ISO639_2_SOURCE=$1
ISO639_5_SOURCE=$2
ISO639_3_SOURCE=$3
TARGET_DIR=$4

TMPDIR_PATH=$(mktemp -d "${TMPDIR:-/tmp}/iso_639.XXXXXX")
trap 'rm -rf "$TMPDIR_PATH"' EXIT HUP INT TERM

ISO639_2_INPUT="$TMPDIR_PATH/iso639_2_input.html"
ISO639_2_ROWS="$TMPDIR_PATH/iso639_2.tsv"
ISO639_3_ROWS="$TMPDIR_PATH/iso639_3.tsv"
ISO639_5_ROWS="$TMPDIR_PATH/iso639_5.tsv"
SCOPE_ROWS="$TMPDIR_PATH/scope.tsv"
PART1_FACTS="$TMPDIR_PATH/iso_639_1.facts"
PART2_FACTS="$TMPDIR_PATH/iso_639_2.facts"
PART3_FACTS="$TMPDIR_PATH/iso_639_3.facts"
PART5_FACTS="$TMPDIR_PATH/iso_639_5.facts"

TODAY=$(date +%F)

if iconv -f ISO-8859-1 -t UTF-8 "$ISO639_2_SOURCE" > "$ISO639_2_INPUT" 2>/dev/null; then
	:
else
	cp "$ISO639_2_SOURCE" "$ISO639_2_INPUT"
fi

UPDATED_2=$(LC_ALL=C tr '\n' ' ' < "$ISO639_2_INPUT" | LC_ALL=C sed -n "s/.*Codes last updated[[:space:]]*<strong>[[:space:]]*\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\).*/\1/p" | LC_ALL=C sed -n '1p')

if [ -z "$UPDATED_2" ]; then
	UPDATED_2=$TODAY
fi

write_header() {
	cat <<'EOF'
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


EOF
}

LC_ALL=C awk -f - "$ISO639_2_INPUT" > "$ISO639_2_ROWS" <<'AWK'
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
}

{
	count = extract_cells($0, values)
	if (count != 5) {
		next
	}
	if (values[1] == "ISO 639-2 Code") {
		next
	}
	if (values[1] != "qaa-qtz" && values[1] !~ /^[a-z]{3}([[:space:]]*\(B\)[[:space:]]*[a-z]{3}[[:space:]]*\(T\))?$/) {
		next
	}
	printf "%s\t%s\t%s\t%s\n", tolower(values[1]), tolower(values[2]), values[3], values[4]
}
AWK

LC_ALL=C awk -f - "$ISO639_5_SOURCE" > "$ISO639_5_ROWS" <<'AWK'
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
}

{
	count = extract_cells($0, values)
	if (count != 6) {
		next
	}
	if (values[1] == "Identifier Indicatif") {
		next
	}
	if (values[1] !~ /^[a-z]{3}$/) {
		next
	}
	printf "%s\t%s\n", tolower(values[1]), values[2]
}
AWK

: > "$SCOPE_ROWS"

LC_ALL=C awk -F '\t' -v scope_file="$SCOPE_ROWS" -f - "$ISO639_3_SOURCE" > "$ISO639_3_ROWS" <<'AWK'
function trim(value) {
	gsub(/\r/, "", value)
	gsub(/[[:space:]]+/, " ", value)
	sub(/^ /, "", value)
	sub(/ $/, "", value)
	return value
}

function scope_atom(value) {
	if (value == "I") return "individual"
	if (value == "M") return "macrolanguage"
	if (value == "C") return "collective"
	return "special"
}

function type_atom(value) {
	if (value == "L") return "living"
	if (value == "E") return "extinct"
	if (value == "A") return "ancient"
	if (value == "C") return "constructed"
	return "special"
}

NR == 1 {
	next
}

{
	alpha3 = tolower(trim($1))
	bibliographic = tolower(trim($2))
	terminologic = tolower(trim($3))
	alpha2 = tolower(trim($4))
	scope = scope_atom(trim($5))
	type = type_atom(trim($6))
	name = trim($7)
	if (alpha3 == "") {
		next
	}
	printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\n", alpha3, bibliographic, terminologic, alpha2, scope, type, name
	if (!(alpha3 in seen_scope)) {
		print alpha3 "\t" scope >> scope_file
		seen_scope[alpha3] = 1
	}
	if (bibliographic != "" && !(bibliographic in seen_scope)) {
		print bibliographic "\t" scope >> scope_file
		seen_scope[bibliographic] = 1
	}
	if (terminologic != "" && !(terminologic in seen_scope)) {
		print terminologic "\t" scope >> scope_file
		seen_scope[terminologic] = 1
	}
}
AWK

: > "$PART1_FACTS"

LC_ALL=C awk -F '\t' -v scope_file="$SCOPE_ROWS" -v part5_file="$ISO639_5_ROWS" -v part1_file="$PART1_FACTS" -f - "$ISO639_2_ROWS" > "$PART2_FACTS" <<'AWK'
function atom(value, copy) {
	copy = value
	gsub(/'/, "''", copy)
	return "'" copy "'"
}

function classify(code_b, code_t, name,    lower_name) {
	if (code_b == "qaa-qtz") {
		return "local_use"
	}
	if (code_b == "mis" || code_b == "mul" || code_b == "und" || code_b == "zxx") {
		return "special"
	}
	if (scope_by_code[code_b] != "") {
		return scope_by_code[code_b]
	}
	if (scope_by_code[code_t] != "") {
		return scope_by_code[code_t]
	}
	if (part5_codes[code_b] || part5_codes[code_t]) {
		return "collective"
	}
	lower_name = tolower(name)
	if (lower_name ~ /languages/ || lower_name ~ /creoles and pidgins/ || name == "Sign Languages") {
		return "collective"
	}
	return "individual"
}

function parse_codes(value, codes,    code_b, code_t) {
	if (value ~ /^[a-z][a-z][a-z][[:space:]]*\(b\)[[:space:]]*[a-z][a-z][a-z][[:space:]]*\(t\)$/) {
		code_b = value
		sub(/[[:space:]]*\(b\)[[:space:]]*.*/, "", code_b)
		code_t = value
		sub(/^.*\(b\)[[:space:]]*/, "", code_t)
		sub(/[[:space:]]*\(t\)$/, "", code_t)
		codes[1] = code_b
		codes[2] = code_t
	} else {
		codes[1] = value
		codes[2] = value
	}
}

BEGIN {
	while ((getline line < scope_file) > 0) {
		split(line, fields, /\t/)
		scope_by_code[fields[1]] = fields[2]
	}
	close(scope_file)
	while ((getline line < part5_file) > 0) {
		split(line, fields, /\t/)
		part5_codes[fields[1]] = 1
	}
	close(part5_file)
}

{
	parse_codes($1, codes)
	bibliographic = codes[1]
	terminologic = codes[2]
	alpha2 = tolower($2)
	name = $3
	class = classify(bibliographic, terminologic, name)
	part2_key = bibliographic SUBSEP terminologic SUBSEP alpha2 SUBSEP name SUBSEP class
	if (!(part2_key in seen_part2)) {
		printf "\tlanguage_code(%s, %s, %s, %s, %s).\n", atom(bibliographic), atom(terminologic), atom(alpha2), atom(name), class
		seen_part2[part2_key] = 1
	}
	if (alpha2 != "") {
		part1_key = alpha2 SUBSEP terminologic SUBSEP name
		if (!(part1_key in seen_part1)) {
			printf "\tlanguage(%s, %s, %s).\n", atom(alpha2), atom(terminologic), atom(name) >> part1_file
			seen_part1[part1_key] = 1
		}
	}
}
AWK

LC_ALL=C awk -F '\t' -f - "$ISO639_3_ROWS" > "$PART3_FACTS" <<'AWK'
function atom(value, copy) {
	copy = value
	gsub(/'/, "''", copy)
	return "'" copy "'"
}

{
	printf "\tlanguage(%s, %s, %s, %s, %s).\n", atom($1), atom($4), $5, $6, atom($7)
}
AWK

LC_ALL=C awk -F '\t' -f - "$ISO639_5_ROWS" > "$PART5_FACTS" <<'AWK'
function atom(value, copy) {
	copy = value
	gsub(/'/, "''", copy)
	return "'" copy "'"
}

{
	printf "\tlanguage_group(%s, %s).\n", atom($1), atom($2)
}
AWK

PART1_COUNT=$(awk 'END {print NR + 0}' "$PART1_FACTS")
PART2_COUNT=$(awk 'END {print NR + 0}' "$PART2_FACTS")
PART3_COUNT=$(awk 'END {print NR + 0}' "$PART3_FACTS")
PART5_COUNT=$(awk 'END {print NR + 0}' "$PART5_FACTS")

write_header > "$TARGET_DIR/iso_639_1.lgt"
cat >> "$TARGET_DIR/iso_639_1.lgt" <<EOF
:- object(iso_639_1).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is $UPDATED_2,
		comment is 'Generated ISO 639-1 fact table.',
		remarks is [
			'Source update' - '$UPDATED_2',
			'Generated entries' - '$PART1_COUNT',
			'Source' - 'Library of Congress ISO 639-2 code list'
		]
	]).

	:- public(language/3).
	:- mode(language(?atom, ?atom, ?atom), zero_or_more).
	:- info(language/3, [
		comment is 'Generated ISO 639-1 fact table.',
		argnames is ['Alpha2', 'Alpha3', 'Name']
	]).

EOF
cat "$PART1_FACTS" >> "$TARGET_DIR/iso_639_1.lgt"
printf '\n:- end_object.\n' >> "$TARGET_DIR/iso_639_1.lgt"

write_header > "$TARGET_DIR/iso_639_2.lgt"
cat >> "$TARGET_DIR/iso_639_2.lgt" <<EOF
:- object(iso_639_2).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is $UPDATED_2,
		comment is 'Generated ISO 639-2 fact table.',
		remarks is [
			'Source update' - '$UPDATED_2',
			'Generated entries' - '$PART2_COUNT',
			'Source' - 'Library of Congress ISO 639-2 code list'
		]
	]).

	:- public(language_code/5).
	:- mode(language_code(?atom, ?atom, ?atom, ?atom, ?atom), zero_or_more).
	:- info(language_code/5, [
		comment is 'Generated ISO 639-2 fact table.',
		argnames is ['Bibliographic', 'Terminologic', 'Alpha2', 'Name', 'Class']
	]).

EOF
cat "$PART2_FACTS" >> "$TARGET_DIR/iso_639_2.lgt"
printf '\n:- end_object.\n' >> "$TARGET_DIR/iso_639_2.lgt"

write_header > "$TARGET_DIR/iso_639_3.lgt"
cat >> "$TARGET_DIR/iso_639_3.lgt" <<EOF
:- object(iso_639_3).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is $TODAY,
		comment is 'Generated ISO 639-3 fact table.',
		remarks is [
			'Source update' - '$TODAY',
			'Generated entries' - '$PART3_COUNT',
			'Source' - 'SIL ISO 639-3 tab-delimited code set'
		]
	]).

	:- public(language/5).
	:- mode(language(?atom, ?atom, ?atom, ?atom, ?atom), zero_or_more).
	:- info(language/5, [
		comment is 'Generated ISO 639-3 fact table.',
		argnames is ['Alpha3', 'Alpha2', 'Scope', 'Type', 'Name']
	]).

EOF
cat "$PART3_FACTS" >> "$TARGET_DIR/iso_639_3.lgt"
printf '\n:- end_object.\n' >> "$TARGET_DIR/iso_639_3.lgt"

write_header > "$TARGET_DIR/iso_639_5.lgt"
cat >> "$TARGET_DIR/iso_639_5.lgt" <<EOF
:- object(iso_639_5).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is $TODAY,
		comment is 'Generated ISO 639-5 fact table.',
		remarks is [
			'Source update' - '$TODAY',
			'Generated entries' - '$PART5_COUNT',
			'Source' - 'Library of Congress ISO 639-5 identifier list'
		]
	]).

	:- public(language_group/2).
	:- mode(language_group(?atom, ?atom), zero_or_more).
	:- info(language_group/2, [
		comment is 'Generated ISO 639-5 fact table.',
		argnames is ['Code', 'Name']
	]).

EOF
cat "$PART5_FACTS" >> "$TARGET_DIR/iso_639_5.lgt"
printf '\n:- end_object.\n' >> "$TARGET_DIR/iso_639_5.lgt"

printf '%s\n' "Generated ISO 639 facts in $TARGET_DIR"
printf '%s\n' "  ISO 639-1 entries: $PART1_COUNT"
printf '%s\n' "  ISO 639-2 entries: $PART2_COUNT"
printf '%s\n' "  ISO 639-3 entries: $PART3_COUNT"
printf '%s\n' "  ISO 639-5 entries: $PART5_COUNT"
