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

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
TARGET_DIR="$SCRIPT_DIR/sources"

mkdir -p "$TARGET_DIR"

curl -L --fail --show-error \
	'https://www.loc.gov/standards/iso639-2/php/code_list.php' \
	-o "$TARGET_DIR/iso639-2.html"

curl -L --fail --show-error \
	'https://www.loc.gov/standards/iso639-5/id.php' \
	-o "$TARGET_DIR/iso639-5.html"

curl -L --fail --show-error \
	'https://iso639-3.sil.org/sites/iso639-3/files/downloads/iso-639-3.tab' \
	-o "$TARGET_DIR/iso639-3.tab"

bash "$SCRIPT_DIR/generate_iso_639.sh" \
	"$TARGET_DIR/iso639-2.html" \
	"$TARGET_DIR/iso639-5.html" \
	"$TARGET_DIR/iso639-3.tab" \
	"$SCRIPT_DIR/.."

printf '%s\n' "Saved ISO 639 source snapshots under $TARGET_DIR"
