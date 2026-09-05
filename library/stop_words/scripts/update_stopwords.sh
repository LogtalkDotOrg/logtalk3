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

if [[ $# -ne 2 ]]; then
	printf 'Usage: %s REPOSITORY COMMIT\n' "$0" >&2
	exit 1
fi

REPOSITORY=$1
COMMIT=$2

if [[ ! "$REPOSITORY" =~ ^stopwords-[a-z0-9-]+$ ]]; then
	printf 'Error: invalid stopwords-iso repository name: %s\n' "$REPOSITORY" >&2
	exit 1
fi

if [[ ! "$COMMIT" =~ ^[0-9a-f]{40}$ ]]; then
	printf 'Error: commit must be a full lowercase SHA-1: %s\n' "$COMMIT" >&2
	exit 1
fi

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
SOURCE_DIR="$SCRIPT_DIR/sources/$REPOSITORY"
OBJECT=${REPOSITORY//-/_}

mkdir -p "$SOURCE_DIR"

curl -L --fail --show-error \
	"https://raw.githubusercontent.com/stopwords-iso/$REPOSITORY/$COMMIT/$REPOSITORY.txt" \
	-o "$SOURCE_DIR/$REPOSITORY.txt"

curl -L --fail --show-error \
	"https://raw.githubusercontent.com/stopwords-iso/$REPOSITORY/$COMMIT/LICENSE" \
	-o "$SOURCE_DIR/LICENSE"

"$SCRIPT_DIR/generate_stopwords.sh" \
	"$REPOSITORY" \
	"$COMMIT" \
	"$SOURCE_DIR/$REPOSITORY.txt" \
	"$SOURCE_DIR/LICENSE" \
	"$SCRIPT_DIR/../languages/$OBJECT.lgt"

printf '%s\n' "Saved the pinned $REPOSITORY source under $SOURCE_DIR"
