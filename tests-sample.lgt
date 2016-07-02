%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Sample tester file
%  Last updated on June 30, 2016
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


%  This is a sample test suite file to help you get started. Test suite files
%  are Logtalk source files defining a set of tests. The tests are usually
%  run with the help of a loader file, usually named "tester.logtalk" or
%  "tester.lgt", that loads the unit testing framework, the code to be tested,
%  and the test suite file. See the "tester-sample.lgt" file for details.

%  The unit testing framework can be found in the "tools/lgtunit" directory.
%  For an overview of its features, see the "tools/lgtunit/NOTES.md" file.
%  For API details, open the "docs/index.html" file in your web browser.


% test suite objets are defined as prototypes extending the
% "lgtunit" object, which implements the unit test framework
:- object(tests,
	extends(lgtunit)).

	% if you want to collect code coverage information, add
	% one clause for the cover/1 predicate for which entity
	% that should be covered
	
	cover(my_object).
	cover(my_other_object).
	...

	% several test idioms are supported with the most simple
	% one being test/1; its argument is the test name, which
	% must be unique in a test suite object; the test goal
	% must succeed for the test to succeed

	test(my_app_1) :-
		% goal that must succeed for the test to pass
		some_goal.

	test(my_app_2) :-
		% another test goal
		another_goal.

	...

	% the "lgtunit" tool supports other, more expressive, idioms
	% allowing e.g. testing for expected failures and errors,
	% determinism, defining setup and cleanup goals, etc; see its
	% documentation for details

:- end_object.
