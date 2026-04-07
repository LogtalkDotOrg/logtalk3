#!/usr/bin/env bash

#############################################################################
##
##   Distribution clean script for packaging
##   Last updated on April 7, 2026
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

VERSION="${1:-2026a}"
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
TMP_DIR=$(mktemp -d "${TMPDIR:-/tmp}/tzif-zone-ids.XXXXXX")
ARCHIVE="$TMP_DIR/tzdata${VERSION}.tar.gz"
CANONICAL="$TMP_DIR/canonical.txt"
ALIASES_RAW="$TMP_DIR/aliases-raw.txt"
ALIASES="$TMP_DIR/aliases.txt"
OUTPUT="$SCRIPT_DIR/tzif_zone_ids.lgt"

cleanup() {
	rm -rf "$TMP_DIR"
}

trap cleanup EXIT HUP INT TERM

curl -fsSLo "$ARCHIVE" "https://data.iana.org/time-zones/releases/tzdata${VERSION}.tar.gz"
tar -xzf "$ARCHIVE" -C "$TMP_DIR"

awk '/^Zone[ 	]/ {print $2}' \
	"$TMP_DIR"/africa \
	"$TMP_DIR"/antarctica \
	"$TMP_DIR"/asia \
	"$TMP_DIR"/australasia \
	"$TMP_DIR"/europe \
	"$TMP_DIR"/northamerica \
	"$TMP_DIR"/southamerica \
	"$TMP_DIR"/etcetera \
	"$TMP_DIR"/factory \
	| LC_ALL=C sort -u > "$CANONICAL"

awk '/^Link[ 	]/ {print $2 "\t" $3}' "$TMP_DIR"/backward \
	| LC_ALL=C sort -u > "$ALIASES_RAW"

awk 'NR == FNR {canonical[$1] = 1; next} !canonical[$2]' "$CANONICAL" "$ALIASES_RAW" > "$ALIASES"

	{
		cat <<EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(tzif_zone_ids).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-07,
		comment is 'Bundled canonical TZDB zone ids and backward-compatible aliases derived from IANA TZDB ${VERSION}.'
	]).

	:- public(tzdb_version/1).
	:- mode(tzdb_version(-atom), one).
	:- info(tzdb_version/1, [
		comment is 'Bundled IANA TZDB release used to derive the zone-id table.',
		argnames is ['Version']
	]).

	:- public(known_zone_id/1).
	:- mode(known_zone_id(+atom), zero_or_one).
	:- info(known_zone_id/1, [
		comment is 'True when the argument is a bundled canonical TZDB zone id or backward-compatible alias.',
		argnames is ['ZoneId']
	]).

	:- public(zone_id_kind/2).
	:- mode(zone_id_kind(+atom, -compound), zero_or_one).
	:- info(zone_id_kind/2, [
		comment is 'Classifies a bundled zone id as canonical or as a backward-compatible alias targeting another zone id.',
		argnames is ['ZoneId', 'Kind']
	]).

	tzdb_version('${VERSION}').

	known_zone_id(ZoneId) :-
		zone_id_kind(ZoneId, _),
		!.

EOF
		awk '{printf "\tzone_id_kind('\''%s'\'', canonical).\n", $0}' "$CANONICAL"
		awk -F '\t' '{printf "\tzone_id_kind('\''%s'\'', alias('\''%s'\'')).\n", $2, $1}' "$ALIASES"
		cat <<EOF

:- end_object.
EOF
	} > "$OUTPUT"

printf 'Generated %s with %s canonical ids and %s aliases from IANA TZDB %s\n' \
	"$OUTPUT" \
	"$(wc -l < "$CANONICAL" | tr -d ' ')" \
	"$(wc -l < "$ALIASES" | tr -d ' ')" \
	"$VERSION"
