%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Sample tester file
%  Last updated on May 21, 2017
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


%  This is a sample tester file. Tester files are simply Logtalk source files
%  whose main purpose is to load and run your application unit tests. Usually
%  they contain just an initialization/1 directive wrapping Logtalk goals as
%  exemplified below.
%
%  The provided testing automation shell script, "logtalk_tester", looks for
%  files named "tester.lgt" or "tester.logtalk" in the current directory and
%  its sub-directories when run. Consult its documentation for details.


:- initialization((
	% minimize compilation reports to the essential ones (errors and warnings)
	set_logtalk_flag(report, warnings),
	% load any necessary library files for your application; for example
	logtalk_load(basic_types(loader)),
	% load the unit test tool
	logtalk_load(lgtunit(loader)),
	% load your application files (e.g. "source.lgt") enabling support for
	% code coverage, which requires compilation in debug mode and collecting
	% source data information; if code coverage is not required, remove the
	% "debug(on)" option for faster execution
	logtalk_load(source, [source_data(on), debug(on)]),
	% compile the unit tests file expanding it using "lgtunit" as the hook
	% object to preprocess the tests; if you have failing tests, add the
	% option debug(on) to debug them (see "tools/lgtunit/NOTES.md" for
	% debugging advice)
	logtalk_load(tests, [hook(lgtunit)]),
	% run all the unit tests; assuming your tests object is named "tests"
	tests::run
)).
