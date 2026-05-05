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
TARGET_FILE="$TARGET_DIR/list-one.xml"

mkdir -p "$TARGET_DIR"

curl -L --fail --show-error \
	'https://www.six-group.com/dam/download/financial-information/data-center/iso-currrency/lists/list-one.xml' \
	-o "$TARGET_FILE"

bash "$SCRIPT_DIR/generate_iso_4217.sh" "$TARGET_FILE" \
	"$SCRIPT_DIR/../iso_4217.lgt"

printf '%s\n' "Saved $TARGET_FILE"
