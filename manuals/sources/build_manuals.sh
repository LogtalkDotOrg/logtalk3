#!/usr/bin/env bash

#############################################################################
## 
##   Documentation build script
##   Last updated on July 23, 2019
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
make clean

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
sed '1,18d' ../../tools/make/NOTES.md | pandoc -f gfm -t rst -o devtools/make.rst
sed '1,18d' ../../tools/ports_profiler/NOTES.md | pandoc -f gfm -t rst -o devtools/ports_profiler.rst
sed '1,18d' ../../tools/profiler/NOTES.md | pandoc -f gfm -t rst -o devtools/profiler.rst
sed '1,18d' ../../tools/tutor/NOTES.md | pandoc -f gfm -t rst -o devtools/tutor.rst
sed '1,18d' ../../tools/wrapper/NOTES.md | pandoc -f gfm -t rst -o devtools/wrapper.rst

make html
make latexpdf
make epub
make info

rm -f _build/html/index_latexpdf.html
mv -f _build/html/* ../
rm -f ../_sources/index_latexpdf.rst.txt
mv -f _build/latex/TheLogtalkHandbook*.pdf ../
mv -f _build/epub/TheLogtalkHandbook*.epub ../
mv -f _build/texinfo/TheLogtalkHandbook*.info ../

make clean
