#!/bin/sh

# This file is part of Logtalk <https://logtalk.org/>
# SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
# SPDX-License-Identifier: Apache-2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
work_dir=$(mktemp -d "${TMPDIR:-/tmp}/logtalk-unicode-data.XXXXXX")
trap 'rm -rf "$work_dir"' 0
trap 'exit 1' HUP INT TERM
source_dir="$work_dir/sources"
output_dir="$script_dir/.."
test_output_dir="$output_dir/test_files"
unicode_url=https://www.unicode.org/Public/17.0.0/ucd

mkdir -p "$source_dir" "$output_dir" "$test_output_dir"

for file in \
	ArabicShaping.txt BidiMirroring.txt Blocks.txt CJKRadicals.txt CaseFolding.txt \
	CompositionExclusions.txt DerivedAge.txt DerivedCoreProperties.txt \
	DerivedNormalizationProps.txt EastAsianWidth.txt HangulSyllableType.txt \
	IndicPositionalCategory.txt IndicSyllabicCategory.txt Jamo.txt LineBreak.txt \
	NameAliases.txt NormalizationTest.txt PropList.txt ScriptExtensions.txt Scripts.txt \
	SpecialCasing.txt UnicodeData.txt; do
	curl -fLsS "$unicode_url/$file" -o "$source_dir/$file"
done

for file in \
	DerivedBidiClass.txt DerivedCombiningClass.txt DerivedDecompositionType.txt \
	DerivedJoiningGroup.txt DerivedJoiningType.txt DerivedNumericType.txt \
	DerivedNumericValues.txt; do
	curl -fLsS "$unicode_url/extracted/$file" -o "$source_dir/$file"
done

unihan_zip="$source_dir/Unihan.zip"
curl -fLsS "$unicode_url/Unihan.zip" -o "$unihan_zip"
unzip -p "$unihan_zip" Unihan_Variants.txt > "$source_dir/Unihan_Variants.txt"
rm "$unihan_zip"

(cd "$source_dir" && shasum -a 256 -c "$script_dir/checksums.sha256")

python3 "$script_dir/generate_data.py" \
	--source-dir "$source_dir" \
	--output-dir "$output_dir" \
	--test-output-dir "$test_output_dir"
