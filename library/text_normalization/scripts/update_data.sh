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
source_dir="$script_dir/sources"
output_dir="$script_dir/../data"
unicode_url=https://www.unicode.org/Public/17.0.0/ucd

mkdir -p "$source_dir" "$output_dir"

for file in UnicodeData.txt CompositionExclusions.txt DerivedNormalizationProps.txt CaseFolding.txt SpecialCasing.txt DerivedCoreProperties.txt PropList.txt NormalizationTest.txt; do
	curl -fLsS "$unicode_url/$file" -o "$source_dir/$file"
done
curl -fLsS https://html.spec.whatwg.org/entities.json -o "$source_dir/entities.json"

(cd "$source_dir" && shasum -a 256 -c "$script_dir/checksums.sha256")

python3 "$script_dir/generate_data.py" \
	--source-dir "$source_dir" \
	--output-dir "$output_dir" \
	--allowlist "$script_dir/common_entities.txt"
