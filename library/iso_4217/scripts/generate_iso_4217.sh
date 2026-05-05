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
	printf '%s\n' "usage: generate_iso_4217.sh SOURCE_XML TARGET_LGT" >&2
	exit 1
fi

SOURCE_XML=$1
TARGET_LGT=$2

TMPDIR_PATH=$(mktemp -d "${TMPDIR:-/tmp}/iso_4217.XXXXXX")
trap 'rm -rf "$TMPDIR_PATH"' EXIT HUP INT TERM

FACTS_FILE="$TMPDIR_PATH/currency.facts"
FUND_FACTS_FILE="$TMPDIR_PATH/fund_currency.facts"
PUBLICATION=$(sed -n 's/.*<ISO_4217 Pblshd="\([0-9-]*\)".*/\1/p' "$SOURCE_XML" | sed -n '1p')

if [ -z "$PUBLICATION" ]; then
	PUBLICATION=$(date +%F)
fi

awk -v facts_file="$FACTS_FILE" -v fund_facts_file="$FUND_FACTS_FILE" -f - "$SOURCE_XML" <<'AWK'
function trim(value) {
	gsub(/\r/, "", value)
	gsub(/\n/, " ", value)
	gsub(/[[:space:]]+/, " ", value)
	sub(/^ /, "", value)
	sub(/ $/, "", value)
	return value
}

function decode(value,    code, replacement) {
	gsub(/&amp;/, "\\&", value)
	gsub(/&quot;/, "\"", value)
	gsub(/&apos;|&#039;|&#39;/, "'", value)
	gsub(/&lt;/, "<", value)
	gsub(/&gt;/, ">", value)
	while (match(value, /&#[0-9]+;/)) {
		code = substr(value, RSTART + 2, RLENGTH - 3) + 0
		replacement = sprintf("%c", code)
		value = substr(value, 1, RSTART - 1) replacement substr(value, RSTART + RLENGTH)
	}
	return value
}

function atom(value, copy) {
	copy = value
	gsub(/'/, "''", copy)
	return "'" copy "'"
}

function xml_text(block, tag,    fragment, parts) {
	fragment = block
	if (fragment !~ "<" tag "([[:space:]][^>]*)?>") {
		return ""
	}
	sub("^.*<" tag "([[:space:]][^>]*)?>", "", fragment)
	split(fragment, parts, "</" tag ">")
	return trim(decode(parts[1]))
}

BEGIN {
	RS = "</CcyNtry>"
	ORS = ""
}

{
	if ($0 !~ /<CcyNtry>/) {
		next
	}
	is_fund = ($0 ~ /<CcyNm[^>]*IsFund="true"/)
	code = xml_text($0, "Ccy")
	if (code == "") {
		next
	}
	numeric = xml_text($0, "CcyNbr") + 0
	minor_unit = xml_text($0, "CcyMnrUnts")
	if (minor_unit == "N.A.") {
		minor_unit = "na"
	} else {
		minor_unit += 0
	}
	name = xml_text($0, "CcyNm")
	entity = xml_text($0, "CtryNm")
	predicate = is_fund ? "fund_currency" : "currency"
	target = is_fund ? fund_facts_file : facts_file
	printf "\t%s(%s, %d, %s, %s, %s).\n", predicate, atom(code), numeric, minor_unit, atom(name), atom(entity) >> target
}
AWK

COUNT=$(awk 'END {print NR + 0}' "$FACTS_FILE")
FUND_COUNT=$(awk 'END {print NR + 0}' "$FUND_FACTS_FILE")

cat > "$TARGET_LGT" <<EOF
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


:- object(iso_4217,
	implements(iso_4217_protocol)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is $PUBLICATION,
		comment is 'Generated ISO 4217 active currency and fund facts extracted from the SIX list-one.xml snapshot.',
		remarks is [
			'Source publication' - '$PUBLICATION',
			'Generated currency entries' - '$COUNT',
			'Generated fund entries' - '$FUND_COUNT'
		]
	]).

	:- public(currency/5).
	:- mode(currency(?atom, ?integer, ?term, ?atom, ?atom), zero_or_more).
	:- info(currency/5, [
		comment is 'Generated ISO 4217 active non-fund currency fact table.',
		argnames is ['Alphabetic', 'Numeric', 'MinorUnit', 'Name', 'Entity']
	]).

	:- public(fund_currency/5).
	:- mode(fund_currency(?atom, ?integer, ?term, ?atom, ?atom), zero_or_more).
	:- info(fund_currency/5, [
		comment is 'Generated ISO 4217 active fund currency fact table.',
		argnames is ['Alphabetic', 'Numeric', 'MinorUnit', 'Name', 'Entity']
	]).

EOF
cat "$FACTS_FILE" >> "$TARGET_LGT"
cat "$FUND_FACTS_FILE" >> "$TARGET_LGT"
printf '\n:- end_object.\n' >> "$TARGET_LGT"

printf '%s\n' "Generated $TARGET_LGT with $COUNT currency entries and $FUND_COUNT fund entries from $SOURCE_XML"
