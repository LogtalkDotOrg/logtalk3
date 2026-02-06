#!/usr/bin/env bash

#############################################################################
##
##   Documentation build script
##   Last updated on February 6, 2026
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


# allow using this script from any directory
cd "$(dirname "$0")" || exit 1

rm -f ../TheLogtalkHandbook*.pdf
rm -f ../TheLogtalkHandbook*.epub
rm -f ../TheLogtalkHandbook*.info
rm -f ../TheLogtalkHandbook*.md
rm -rf ../_sources
rm -rf ../_static
rm -rf ../faq
rm -rf ../refman
rm -rf ../tutorial
rm -rf ../userman
rm -rf ../devtools
rm -rf ../libraries
rm -rf ../ports
rm -rf ../contributions
make clean

sed '1,19d' ../../../tools/NOTES.md | pandoc -f gfm -t rst -o devtools/overview.rst
sed '1,19d' ../../../tools/asdf/NOTES.md | pandoc -f gfm -t rst -o devtools/asdf.rst
sed '1,19d' ../../../tools/assertions/NOTES.md | pandoc -f gfm -t rst -o devtools/assertions.rst
sed '1,19d' ../../../tools/code_metrics/NOTES.md | pandoc -f gfm -t rst -o devtools/code_metrics.rst
sed '1,19d' ../../../tools/dead_code_scanner/NOTES.md | pandoc -f gfm -t rst -o devtools/dead_code_scanner.rst
sed '1,19d' ../../../tools/debug_messages/NOTES.md | pandoc -f gfm -t rst -o devtools/debug_messages.rst
sed '1,19d' ../../../tools/debugger/NOTES.md | pandoc -f gfm -t rst -o devtools/debugger.rst
sed '1,19d' ../../../tools/diagrams/NOTES.md | pandoc -f gfm -t rst -o devtools/diagrams.rst
sed '1,19d' ../../../tools/doclet/NOTES.md | pandoc -f gfm -t rst -o devtools/doclet.rst
sed '1,19d' ../../../tools/help/NOTES.md | pandoc -f gfm -t rst -o devtools/help.rst
sed '1,19d' ../../../tools/issue_creator/NOTES.md | pandoc -f gfm -t rst -o devtools/issue_creator.rst
sed '1,19d' ../../../tools/lgtdoc/NOTES.md | pandoc -f gfm -t rst -o devtools/lgtdoc.rst
sed '1,19d' ../../../tools/lgtunit/NOTES.md | pandoc -f gfm -t rst -o devtools/lgtunit.rst
sed '1,19d' ../../../tools/linter/NOTES.md | pandoc -f gfm -t rst -o devtools/linter.rst
sed '1,19d' ../../../tools/make/NOTES.md | pandoc -f gfm -t rst -o devtools/make.rst
sed '1,19d' ../../../tools/packs/NOTES.md | pandoc -f gfm -t rst -o devtools/packs.rst
sed '1,19d' ../../../tools/ports_profiler/NOTES.md | pandoc -f gfm -t rst -o devtools/ports_profiler.rst
sed '1,19d' ../../../tools/profiler/NOTES.md | pandoc -f gfm -t rst -o devtools/profiler.rst
sed '1,19d' ../../../tools/tutor/NOTES.md | pandoc -f gfm -t rst -o devtools/tutor.rst
sed '1,19d' ../../../tools/wrapper/NOTES.md | pandoc -f gfm -t rst -o devtools/wrapper.rst

for file in devtools/*.rst; do
	base="${file##*/}"
	if [ "$base" != "index.rst" ] && [ "$base" != "overview.rst" ] ; then
		name="${base%.*}"
		echo ".. _library_$name:" > temp0 && echo >> temp0 && cat temp0 "$file" > temp1 && mv temp1 "$file"
	fi
done
rm -f temp0

sed '1,19d' ../../../library/NOTES.md | pandoc -f gfm -t rst -o libraries/overview.rst
sed '1,19d' ../../../library/arbitrary/NOTES.md | pandoc -f gfm -t rst -o libraries/arbitrary.rst
sed '1,19d' ../../../library/assignvars/NOTES.md | pandoc -f gfm -t rst -o libraries/assignvars.rst
sed '1,19d' ../../../library/avro/NOTES.md | pandoc -f gfm -t rst -o libraries/avro.rst
sed '1,19d' ../../../library/base32/NOTES.md | pandoc -f gfm -t rst -o libraries/base32.rst
sed '1,19d' ../../../library/base58/NOTES.md | pandoc -f gfm -t rst -o libraries/base58.rst
sed '1,19d' ../../../library/base64/NOTES.md | pandoc -f gfm -t rst -o libraries/base64.rst
sed '1,19d' ../../../library/base85/NOTES.md | pandoc -f gfm -t rst -o libraries/base85.rst
sed '1,19d' ../../../library/basic_types/NOTES.md | pandoc -f gfm -t rst -o libraries/basic_types.rst
sed '1,19d' ../../../library/coroutining/NOTES.md | pandoc -f gfm -t rst -o libraries/coroutining.rst
sed '1,19d' ../../../library/cbor/NOTES.md | pandoc -f gfm -t rst -o libraries/cbor.rst
sed '1,19d' ../../../library/ccsds/NOTES.md | pandoc -f gfm -t rst -o libraries/ccsds.rst
sed '1,19d' ../../../library/csv/NOTES.md | pandoc -f gfm -t rst -o libraries/csv.rst
sed '1,19d' ../../../library/dates/NOTES.md | pandoc -f gfm -t rst -o libraries/dates.rst
sed '1,19d' ../../../library/dependents/NOTES.md | pandoc -f gfm -t rst -o libraries/dependents.rst
sed '1,19d' ../../../library/dictionaries/NOTES.md | pandoc -f gfm -t rst -o libraries/dictionaries.rst
sed '1,19d' ../../../library/dif/NOTES.md | pandoc -f gfm -t rst -o libraries/dif.rst
sed '1,19d' ../../../library/edcg/NOTES.md | pandoc -f gfm -t rst -o libraries/edcg.rst
sed '1,19d' ../../../library/events/NOTES.md | pandoc -f gfm -t rst -o libraries/events.rst
sed '1,19d' ../../../library/expand_library_alias_paths/NOTES.md | pandoc -f gfm -t rst -o libraries/expand_library_alias_paths.rst
sed '1,19d' ../../../library/expecteds/NOTES.md | pandoc -f gfm -t rst -o libraries/expecteds.rst
sed '1,19d' ../../../library/format/NOTES.md | pandoc -f gfm -t rst -o libraries/format.rst
sed '1,19d' ../../../library/git/NOTES.md | pandoc -f gfm -t rst -o libraries/git.rst
sed '1,19d' ../../../library/grammars/NOTES.md | pandoc -f gfm -t rst -o libraries/grammars.rst
sed '1,19d' ../../../library/gensym/NOTES.md | pandoc -f gfm -t rst -o libraries/gensym.rst
sed '1,19d' ../../../library/genint/NOTES.md | pandoc -f gfm -t rst -o libraries/genint.rst
sed '1,19d' ../../../library/heaps/NOTES.md | pandoc -f gfm -t rst -o libraries/heaps.rst
sed '1,19d' ../../../library/hierarchies/NOTES.md | pandoc -f gfm -t rst -o libraries/hierarchies.rst
sed '1,19d' ../../../library/hook_flows/NOTES.md | pandoc -f gfm -t rst -o libraries/hook_flows.rst
sed '1,19d' ../../../library/hook_objects/NOTES.md | pandoc -f gfm -t rst -o libraries/hook_objects.rst
sed '1,19d' ../../../library/html/NOTES.md | pandoc -f gfm -t rst -o libraries/html.rst
sed '1,19d' ../../../library/ids/NOTES.md | pandoc -f gfm -t rst -o libraries/ids.rst
sed '1,19d' ../../../library/intervals/NOTES.md | pandoc -f gfm -t rst -o libraries/intervals.rst
sed '1,19d' ../../../library/java/NOTES.md | pandoc -f gfm -t rst -o libraries/java.rst
sed '1,19d' ../../../library/json/NOTES.md | pandoc -f gfm -t rst -o libraries/json.rst
sed '1,19d' ../../../library/json_ld/NOTES.md | pandoc -f gfm -t rst -o libraries/json_ld.rst
sed '1,19d' ../../../library/json_lines/NOTES.md | pandoc -f gfm -t rst -o libraries/json_lines.rst
sed '1,19d' ../../../library/json_schema/NOTES.md | pandoc -f gfm -t rst -o libraries/json_schema.rst
sed '1,19d' ../../../library/listing/NOTES.md | pandoc -f gfm -t rst -o libraries/listing.rst
sed '1,19d' ../../../library/logging/NOTES.md | pandoc -f gfm -t rst -o libraries/logging.rst
sed '1,19d' ../../../library/loops/NOTES.md | pandoc -f gfm -t rst -o libraries/loops.rst
sed '1,19d' ../../../library/meta/NOTES.md | pandoc -f gfm -t rst -o libraries/meta.rst
sed '1,19d' ../../../library/meta_compiler/NOTES.md | pandoc -f gfm -t rst -o libraries/meta_compiler.rst
sed '1,19d' ../../../library/mutations/NOTES.md | pandoc -f gfm -t rst -o libraries/mutations.rst
sed '1,19d' ../../../library/nested_dictionaries/NOTES.md | pandoc -f gfm -t rst -o libraries/nested_dictionaries.rst
sed '1,19d' ../../../library/optionals/NOTES.md | pandoc -f gfm -t rst -o libraries/optionals.rst
sed '1,19d' ../../../library/options/NOTES.md | pandoc -f gfm -t rst -o libraries/options.rst
sed '1,19d' ../../../library/os/NOTES.md | pandoc -f gfm -t rst -o libraries/os.rst
sed '1,19d' ../../../library/process/NOTES.md | pandoc -f gfm -t rst -o libraries/process.rst
sed '1,19d' ../../../library/protobuf/NOTES.md | pandoc -f gfm -t rst -o libraries/protobuf.rst
sed '1,19d' ../../../library/queues/NOTES.md | pandoc -f gfm -t rst -o libraries/queues.rst
sed '1,19d' ../../../library/random/NOTES.md | pandoc -f gfm -t rst -o libraries/random.rst
sed '1,19d' ../../../library/reader/NOTES.md | pandoc -f gfm -t rst -o libraries/reader.rst
sed '1,19d' ../../../library/recorded_database/NOTES.md | pandoc -f gfm -t rst -o libraries/recorded_database.rst
sed '1,19d' ../../../library/redis/NOTES.md | pandoc -f gfm -t rst -o libraries/redis.rst
sed '1,19d' ../../../library/sets/NOTES.md | pandoc -f gfm -t rst -o libraries/sets.rst
sed '1,19d' ../../../library/sockets/NOTES.md | pandoc -f gfm -t rst -o libraries/sockets.rst
sed '1,19d' ../../../library/statistics/NOTES.md | pandoc -f gfm -t rst -o libraries/statistics.rst
sed '1,19d' ../../../library/stemming/NOTES.md | pandoc -f gfm -t rst -o libraries/stemming.rst
sed '1,19d' ../../../library/stomp/NOTES.md | pandoc -f gfm -t rst -o libraries/stomp.rst
sed '1,19d' ../../../library/string_distance/NOTES.md | pandoc -f gfm -t rst -o libraries/string_distance.rst
sed '1,19d' ../../../library/strings/NOTES.md | pandoc -f gfm -t rst -o libraries/strings.rst
sed '1,19d' ../../../library/subsequences/NOTES.md | pandoc -f gfm -t rst -o libraries/subsequences.rst
sed '1,19d' ../../../library/term_io/NOTES.md | pandoc -f gfm -t rst -o libraries/term_io.rst
sed '1,19d' ../../../library/timeout/NOTES.md | pandoc -f gfm -t rst -o libraries/timeout.rst
sed '1,19d' ../../../library/toon/NOTES.md | pandoc -f gfm -t rst -o libraries/toon.rst
sed '1,19d' ../../../library/tsv/NOTES.md | pandoc -f gfm -t rst -o libraries/tsv.rst
sed '1,19d' ../../../library/types/NOTES.md | pandoc -f gfm -t rst -o libraries/types.rst
cat ../../../library/unicode_data/README.md | pandoc -f gfm -t rst -o libraries/unicode_data.rst
sed '1,19d' ../../../library/ulid/NOTES.md | pandoc -f gfm -t rst -o libraries/ulid.rst
sed '1,19d' ../../../library/union_find/NOTES.md | pandoc -f gfm -t rst -o libraries/union_find.rst
sed '1,19d' ../../../library/url/NOTES.md | pandoc -f gfm -t rst -o libraries/url.rst
sed '1,19d' ../../../library/uuid/NOTES.md | pandoc -f gfm -t rst -o libraries/uuid.rst
sed '1,19d' ../../../library/yaml/NOTES.md | pandoc -f gfm -t rst -o libraries/yaml.rst
sed '1,19d' ../../../library/zippers/NOTES.md | pandoc -f gfm -t rst -o libraries/zippers.rst

for file in libraries/*.rst; do
	base="${file##*/}"
	if [ "$base" != "index.rst" ] && [ "$base" != "overview.rst" ] && [ "$base" != "core.rst" ] ; then
		name="${base%.*}"
		echo ".. _library_$name:" > temp0 && echo >> temp0 && cat temp0 "$file" > temp1 && mv temp1 "$file"
	fi
done
rm -f temp0

sed '1,24d' ../../../ports/fcube/NOTES.md | pandoc -f gfm -t rst -o ports/fcube.rst
sed '1,35d' ../../../ports/metagol/NOTES.md | pandoc -f gfm -t rst -o ports/metagol.rst
sed '1,22d' ../../../ports/toychr/NOTES.md | pandoc -f gfm -t rst -o ports/toychr.rst

for file in ports/*.rst; do
	base="${file##*/}"
	if [ "$base" != "index.rst" ] && [ "$base" != "overview.rst" ] ; then
		name="${base%.*}"
		echo ".. _library_$name:" > temp0 && echo >> temp0 && cat temp0 "$file" > temp1 && mv temp1 "$file"
	fi
done
rm -f temp0

cat ../../../contributions/flags/NOTES.md | pandoc -f gfm -t rst -o contributions/flags.rst
sed '1,19d' ../../../contributions/iso8601/NOTES.md | pandoc -f gfm -t rst -o contributions/iso8601.rst
cat ../../../contributions/pddl_parser/README.txt | pandoc -f gfm -t rst -o contributions/pddl_parser.rst
cat ../../../contributions/verdi_neruda/README.txt | pandoc -f gfm -t rst -o contributions/verdi_neruda.rst
sed '1,19d' ../../../contributions/xml_parser/NOTES.md | pandoc -f gfm -t rst -o contributions/xml_parser.rst

for file in contributions/*.rst; do
	base="${file##*/}"
	if [ "$base" != "index.rst" ] && [ "$base" != "overview.rst" ] ; then
		name="${base%.*}"
		echo ".. _library_$name:" > temp0 && echo >> temp0 && cat temp0 "$file" > temp1 && mv temp1 "$file"
	fi
done
rm -f temp0

make html
make epub
make info
make latexpdf
make singlehtml
#make linkcheck

# Handle sed differences between GNU and BSD
case $(sed --help 2>&1) in
	*GNU*) sed_i () { sed -i "$@"; };;
	*) sed_i () { sed -i '' "$@"; };;
esac

version_base=$(cat ../../../VERSION.txt | cut -f1 -d"-")
pandoc _build/singlehtml/index.html --wrap=none -t gfm-raw_html -o _build/singlehtml/TheLogtalkHandbook-$version_base.md

# Remove heading link references from the Markdown file
sed_i -E 's/\[.\]\(#[-a-z0-9]+ "Link to this heading"\)//g' _build/singlehtml/TheLogtalkHandbook-$version_base.md
# Remove other links leaving only the text
sed_i -E 's/\[([^]]+)\]\([^)]+\)/\1/g' _build/singlehtml/TheLogtalkHandbook-$version_base.md

rm -f _build/html/index_latexpdf.html
mv -f _build/html/* ../
rm -f ../_sources/index_latexpdf.rst.txt
mv -f _build/latex/TheLogtalkHandbook*.pdf ../
mv -f _build/epub/TheLogtalkHandbook*.epub ../
mv -f _build/texinfo/TheLogtalkHandbook*.info ../
mv -f _build/singlehtml/TheLogtalkHandbook*.md ../

make clean
