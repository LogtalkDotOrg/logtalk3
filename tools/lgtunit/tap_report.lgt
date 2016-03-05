%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


:- initialization(logtalk_load(tap_output)).


:- object(tap_report).

	:- info([
		version is 0.2,
		author is 'Paulo Moura',
		date is 2016/03/05,
		comment is 'Intercepts unit test execution messages and generates a tap_report.txt file using the TAP output format in the same directory as the tests object file.'
	]).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		message_hook(Message),
		% allow default processing of the messages
		fail.

	% start
	message_hook(running_tests_from_object_file(_, File)) :-
		logtalk::loaded_file_property(File, directory(Directory)),
		atom_concat(Directory, 'tap_report.txt', ReportFile),
		open(ReportFile, write, _, [alias(tap_report)]),
		set_output(tap_report).
	% stop
	message_hook(tests_end_date_time(_, _, _, _, _, _)) :-
		close(tap_report),
		set_output(user_output).

:- end_object.
