#############################################################################
## 
##   Documentation build script
##   Last updated on September 21, 2022
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


$scriptpath = $MyInvocation.MyCommand.Path
$dir = Split-Path $scriptpath
Push-Location $dir

Remove-Item ../TheLogtalkHandbook*.pdf
Remove-Item ../TheLogtalkHandbook*.epub
Remove-Item ../TheLogtalkHandbook*.info
Remove-Item ../_sources -Recurse
Remove-Item ../_static -Recurse
Remove-Item ../faq -Recurse
Remove-Item ../refman -Recurse
Remove-Item ../tutorial -Recurse
Remove-Item ../userman -Recurse
Remove-Item ../devtools -Recurse
Remove-Item ../libraries -Recurse
Remove-Item ../ports -Recurse
Remove-Item ../contributions -Recurse
.\make.bat clean

(Get-Content ../../tools/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/overview.rst
(Get-Content ../../tools/asdf/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/asdf.rst
(Get-Content ../../tools/assertions/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/assertions.rst
(Get-Content ../../tools/code_metrics/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/code_metrics.rst
(Get-Content ../../tools/dead_code_scanner/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/dead_code_scanner.rst
(Get-Content ../../tools/debug_messages/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/debug_messages.rst
(Get-Content ../../tools/debugger/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/debugger.rst
(Get-Content ../../tools/diagrams/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/diagrams.rst
(Get-Content ../../tools/doclet/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/doclet.rst
(Get-Content ../../tools/help/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/help.rst
(Get-Content ../../tools/issue_creator/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/issue_creator.rst
(Get-Content ../../tools/lgtdoc/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/lgtdoc.rst
(Get-Content ../../tools/lgtunit/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/lgtunit.rst
(Get-Content ../../tools/linter/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/linter.rst
(Get-Content ../../tools/make/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/make.rst
(Get-Content ../../tools/packs/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/packs.rst
(Get-Content ../../tools/ports_profiler/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/ports_profiler.rst
(Get-Content ../../tools/profiler/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/profiler.rst
(Get-Content ../../tools/tutor/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/tutor.rst
(Get-Content ../../tools/wrapper/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o devtools/wrapper.rst

Get-ChildItem -Path devtools/*.rst |
Foreach-Object {
	$file = Split-Path $_ -leaf
	if ( $file -ne "index.rst" -and $file -ne "overview.rst" ) {
		$newline = ".. _library_" + $_.BaseName + ":`n"
		$content = Get-Content $_.FullName
		Set-Content $_.FullName -value $newline, $content
	}
}

(Get-Content ../../library/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/overview.rst
(Get-Content ../../library/arbitrary/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/arbitrary.rst
(Get-Content ../../library/assignvars/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/assignvars.rst
(Get-Content ../../library/base64/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/base64.rst
(Get-Content ../../library/basic_types/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/basic_types.rst
(Get-Content ../../library/coroutining/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/coroutining.rst
(Get-Content ../../library/cbor/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/cbor.rst
(Get-Content ../../library/csv/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/csv.rst
(Get-Content ../../library/dates/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/dates.rst
(Get-Content ../../library/dependents/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/dependents.rst
(Get-Content ../../library/dictionaries/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/dictionaries.rst
(Get-Content ../../library/dif/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/dif.rst
(Get-Content ../../library/edcg/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/edcg.rst
(Get-Content ../../library/events/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/events.rst
(Get-Content ../../library/expand_library_alias_paths/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/expand_library_alias_paths.rst
(Get-Content ../../library/expecteds/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/expecteds.rst
(Get-Content ../../library/format/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/format.rst
(Get-Content ../../library/git/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/git.rst
(Get-Content ../../library/grammars/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/grammars.rst
(Get-Content ../../library/gensym/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/gensym.rst
(Get-Content ../../library/genint/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/genint.rst
(Get-Content ../../library/heaps/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/heaps.rst
(Get-Content ../../library/hierarchies/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/hierarchies.rst
(Get-Content ../../library/hook_flows/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/hook_flows.rst
(Get-Content ../../library/hook_objects/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/hook_objects.rst
(Get-Content ../../library/html/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/html.rst
(Get-Content ../../library/intervals/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/intervals.rst
(Get-Content ../../library/java/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/java.rst
(Get-Content ../../library/json/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/json.rst
(Get-Content ../../library/logging/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/logging.rst
(Get-Content ../../library/loops/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/loops.rst
(Get-Content ../../library/meta/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/meta.rst
(Get-Content ../../library/meta_compiler/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/meta_compiler.rst
(Get-Content ../../library/nested_dictionaries/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/nested_dictionaries.rst
(Get-Content ../../library/optionals/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/optionals.rst
(Get-Content ../../library/options/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/options.rst
(Get-Content ../../library/os/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/os.rst
(Get-Content ../../library/queues/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/queues.rst
(Get-Content ../../library/random/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/random.rst
(Get-Content ../../library/reader/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/reader.rst
(Get-Content ../../library/redis/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/redis.rst
(Get-Content ../../library/sets/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/sets.rst
(Get-Content ../../library/statistics/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/statistics.rst
(Get-Content ../../library/term_io/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/term_io.rst
(Get-Content ../../library/timeout/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/timeout.rst
(Get-Content ../../library/types/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/types.rst
(Get-Content ../../library/unicode_data/README.md) | pandoc -f gfm -t rst -o libraries/unicode_data.rst
(Get-Content ../../library/union_find/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/union_find.rst
(Get-Content ../../library/uuid/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/uuid.rst
(Get-Content ../../library/zippers/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o libraries/zippers.rst

Get-ChildItem -Path libraries/*.rst |
Foreach-Object {
	$file = Split-Path $_ -leaf
	if ( $file -ne "index.rst" -and $file -ne "overview.rst" -and $file -ne "core.rst" ) {
		$newline = ".. _library_" + $_.BaseName + ":`n"
		$content = Get-Content $_.FullName
		Set-Content $_.FullName -value $newline, $content
	}
}

(Get-Content ../../ports/fcube/NOTES.md | Select-Object -Skip 24) | pandoc -f gfm -t rst -o ports/fcube.rst
(Get-Content ../../ports/metagol/NOTES.md | Select-Object -Skip 35) | pandoc -f gfm -t rst -o ports/metagol.rst
(Get-Content ../../ports/toychr/NOTES.md | Select-Object -Skip 22) | pandoc -f gfm -t rst -o ports/toychr.rst

Get-ChildItem -Path ports/*.rst |
Foreach-Object {
	$file = Split-Path $_ -leaf
	if ( $file -ne "index.rst" -and $file -ne "overview.rst" ) {
		$newline = ".. _library_" + $_.BaseName + ":`n"
		$content = Get-Content $_.FullName
		Set-Content $_.FullName -value $newline, $content
	}
}

(Get-Content ../../contributions/flags/NOTES.md) | pandoc -f gfm -t rst -o contributions/flags.rst
(Get-Content ../../contributions/iso8601/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o contributions/iso8601.rst
(Get-Content ../../contributions/pddl_parser/README.txt) | pandoc -f gfm -t rst -o contributions/pddl_parser.rst
(Get-Content ../../contributions/verdi_neruda/README.txt) | pandoc -f gfm -t rst -o contributions/verdi_neruda.rst
(Get-Content ../../contributions/xml_parser/NOTES.md | Select-Object -Skip 19) | pandoc -f gfm -t rst -o contributions/xml_parser.rst

Get-ChildItem -Path contributions/*.rst |
Foreach-Object {
	$file = Split-Path $_ -leaf
	if ( $file -ne "index.rst" -and $file -ne "overview.rst" ) {
		$newline = ".. _library_" + $_.BaseName + ":`n"
		$content = Get-Content $_.FullName
		Set-Content $_.FullName -value $newline, $content
	}
}

.\make.bat html
.\make.bat latexpdf
.\make.bat epub
.\make.bat info

(Get-Content _build/html/contributions/index.html).Replace('../docs/index.html', '../../docs/index.html') | Set-Content _build/html/contributions/index.html
(Get-Content _build/html/devtools/index.html).Replace('../docs/index.html', '../../docs/index.html') | Set-Content _build/html/devtools/index.html
(Get-Content _build/html/faq/index.html).Replace('../docs/index.html', '../../docs/index.html') | Set-Content _build/html/faq/index.html
(Get-Content _build/html/libraries/index.html).Replace('../docs/index.html', '../../docs/index.html') | Set-Content _build/html/libraries/index.html
(Get-Content _build/html/ports/index.html).Replace('../docs/index.html', '../../docs/index.html') | Set-Content _build/html/ports/index.html
(Get-Content _build/html/refman/index.html).Replace('../docs/index.html', '../../docs/index.html') | Set-Content _build/html/refman/index.html
(Get-Content _build/html/tutorial/index.html).Replace('../docs/index.html', '../../docs/index.html') | Set-Content _build/html/tutorial/index.html
(Get-Content _build/html/userman/index.html).Replace('../docs/index.html', '../../docs/index.html') | Set-Content _build/html/userman/index.html

Remove-Item _build/html/index_latexpdf.html
Move-Item -Path _build/html/* -Destination ../ -Force
Remove-Item ../_sources/index_latexpdf.rst.txt
Move-Item -Path _build/latex/TheLogtalkHandbook*.pdf -Destination ../ -Force
Move-Item -Path _build/epub/TheLogtalkHandbook*.epub -Destination ../ -Force
Move-Item -Path _build/texinfo/TheLogtalkHandbook*.info -Destination ../ -Force

.\make.bat clean

Pop-Location
