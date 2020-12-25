#!/usr/bin/env bash

#############################################################################
## 
##   Documentation build script
##   Last updated on November 12, 2020
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
rm -rf ../_sources
rm -rf ../_static
rm -rf ../faq
rm -rf ../refman
rm -rf ../tutorial
rm -rf ../userman
rm -rf ../devtools
rm -rf ../libraries
make clean

sed '1,18d' ../../tools/NOTES.md | pandoc -f gfm -t rst -o devtools/overview.rst
sed '1,18d' ../../tools/asdf/NOTES.md | pandoc -f gfm -t rst -o devtools/asdf.rst
sed '1,18d' ../../tools/assertions/NOTES.md | pandoc -f gfm -t rst -o devtools/assertions.rst
sed '1,18d' ../../tools/code_metrics/NOTES.md | pandoc -f gfm -t rst -o devtools/code_metrics.rst
sed '1,18d' ../../tools/dead_code_scanner/NOTES.md | pandoc -f gfm -t rst -o devtools/dead_code_scanner.rst
sed '1,18d' ../../tools/debug_messages/NOTES.md | pandoc -f gfm -t rst -o devtools/debug_messages.rst
sed '1,18d' ../../tools/debugger/NOTES.md | pandoc -f gfm -t rst -o devtools/debugger.rst
sed '1,18d' ../../tools/diagrams/NOTES.md | pandoc -f gfm -t rst -o devtools/diagrams.rst
sed '1,18d' ../../tools/doclet/NOTES.md | pandoc -f gfm -t rst -o devtools/doclet.rst
sed '1,18d' ../../tools/help/NOTES.md | pandoc -f gfm -t rst -o devtools/help.rst
sed '1,18d' ../../tools/lgtdoc/NOTES.md | pandoc -f gfm -t rst -o devtools/lgtdoc.rst
sed '1,18d' ../../tools/lgtunit/NOTES.md | pandoc -f gfm -t rst -o devtools/lgtunit.rst
sed '1,18d' ../../tools/linter/NOTES.md | pandoc -f gfm -t rst -o devtools/linter.rst
sed '1,18d' ../../tools/make/NOTES.md | pandoc -f gfm -t rst -o devtools/make.rst
sed '1,18d' ../../tools/ports_profiler/NOTES.md | pandoc -f gfm -t rst -o devtools/ports_profiler.rst
sed '1,18d' ../../tools/profiler/NOTES.md | pandoc -f gfm -t rst -o devtools/profiler.rst
sed '1,18d' ../../tools/tutor/NOTES.md | pandoc -f gfm -t rst -o devtools/tutor.rst
sed '1,18d' ../../tools/wrapper/NOTES.md | pandoc -f gfm -t rst -o devtools/wrapper.rst

sed '1,18d' ../../library/NOTES.md | pandoc -f gfm -t rst -o libraries/overview.rst
sed '1,18d' ../../library/arbitrary/NOTES.md | pandoc -f gfm -t rst -o libraries/arbitrary.rst
sed '1,18d' ../../library/assignvars/NOTES.md | pandoc -f gfm -t rst -o libraries/assignvars.rst
sed '1,18d' ../../library/basic_types/NOTES.md | pandoc -f gfm -t rst -o libraries/basic_types.rst
sed '1,18d' ../../library/coroutining/NOTES.md | pandoc -f gfm -t rst -o libraries/coroutining.rst
sed '1,18d' ../../library/dates/NOTES.md | pandoc -f gfm -t rst -o libraries/dates.rst
sed '1,18d' ../../library/dependents/NOTES.md | pandoc -f gfm -t rst -o libraries/dependents.rst
sed '1,18d' ../../library/dictionaries/NOTES.md | pandoc -f gfm -t rst -o libraries/dictionaries.rst
sed '1,18d' ../../library/edcg/NOTES.md | pandoc -f gfm -t rst -o libraries/edcg.rst
sed '1,18d' ../../library/events/NOTES.md | pandoc -f gfm -t rst -o libraries/events.rst
sed '1,18d' ../../library/expand_library_alias_paths/NOTES.md | pandoc -f gfm -t rst -o libraries/expand_library_alias_paths.rst
sed '1,18d' ../../library/expecteds/NOTES.md | pandoc -f gfm -t rst -o libraries/expecteds.rst
sed '1,18d' ../../library/git/NOTES.md | pandoc -f gfm -t rst -o libraries/git.rst
sed '1,18d' ../../library/gensym/NOTES.md | pandoc -f gfm -t rst -o libraries/gensym.rst
sed '1,18d' ../../library/heaps/NOTES.md | pandoc -f gfm -t rst -o libraries/heaps.rst
sed '1,18d' ../../library/hierarchies/NOTES.md | pandoc -f gfm -t rst -o libraries/hierarchies.rst
sed '1,18d' ../../library/hook_flows/NOTES.md | pandoc -f gfm -t rst -o libraries/hook_flows.rst
sed '1,18d' ../../library/hook_objects/NOTES.md | pandoc -f gfm -t rst -o libraries/hook_objects.rst
sed '1,18d' ../../library/intervals/NOTES.md | pandoc -f gfm -t rst -o libraries/intervals.rst
sed '1,18d' ../../library/java/NOTES.md | pandoc -f gfm -t rst -o libraries/java.rst
sed '1,18d' ../../library/logging/NOTES.md | pandoc -f gfm -t rst -o libraries/logging.rst
sed '1,18d' ../../library/loops/NOTES.md | pandoc -f gfm -t rst -o libraries/loops.rst
sed '1,18d' ../../library/meta/NOTES.md | pandoc -f gfm -t rst -o libraries/meta.rst
sed '1,18d' ../../library/meta_compiler/NOTES.md | pandoc -f gfm -t rst -o libraries/meta_compiler.rst
sed '1,18d' ../../library/optionals/NOTES.md | pandoc -f gfm -t rst -o libraries/optionals.rst
sed '1,18d' ../../library/os/NOTES.md | pandoc -f gfm -t rst -o libraries/os.rst
sed '1,18d' ../../library/queues/NOTES.md | pandoc -f gfm -t rst -o libraries/queues.rst
sed '1,18d' ../../library/random/NOTES.md | pandoc -f gfm -t rst -o libraries/random.rst
sed '1,18d' ../../library/reader/NOTES.md | pandoc -f gfm -t rst -o libraries/reader.rst
sed '1,18d' ../../library/redis/NOTES.md | pandoc -f gfm -t rst -o libraries/redis.rst
sed '1,18d' ../../library/sets/NOTES.md | pandoc -f gfm -t rst -o libraries/sets.rst
sed '1,18d' ../../library/statistics/NOTES.md | pandoc -f gfm -t rst -o libraries/statistics.rst
sed '1,18d' ../../library/timeout/NOTES.md | pandoc -f gfm -t rst -o libraries/timeout.rst
sed '1,18d' ../../library/types/NOTES.md | pandoc -f gfm -t rst -o libraries/types.rst
cat ../../library/unicode_data/README.md | pandoc -f gfm -t rst -o libraries/unicode_data.rst
sed '1,18d' ../../library/zippers/NOTES.md | pandoc -f gfm -t rst -o libraries/zippers.rst

make html
make latexpdf
make epub
make info

sed -e 's|../docs/index.html|../../docs/index.html|g' -i '' _build/html/devtools/index.html
sed -e 's|../docs/index.html|../../docs/index.html|g' -i '' _build/html/faq/index.html
sed -e 's|../docs/index.html|../../docs/index.html|g' -i '' _build/html/libraries/index.html
sed -e 's|../docs/index.html|../../docs/index.html|g' -i '' _build/html/refman/index.html
sed -e 's|../docs/index.html|../../docs/index.html|g' -i '' _build/html/tutorial/index.html
sed -e 's|../docs/index.html|../../docs/index.html|g' -i '' _build/html/userman/index.html

rm -f _build/html/index_latexpdf.html
mv -f _build/html/* ../
rm -f ../_sources/index_latexpdf.rst.txt
mv -f _build/latex/TheLogtalkHandbook*.pdf ../
mv -f _build/epub/TheLogtalkHandbook*.epub ../
mv -f _build/texinfo/TheLogtalkHandbook*.info ../

make clean
