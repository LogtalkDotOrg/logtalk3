%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-11-12,
		comment is 'Unit tests for the "git" library.'
	]).

	:- uses(git, [
		branch/2, commit_log/3,
		commit_author/2, commit_date/2, commit_message/2,
		commit_hash/2, commit_hash_abbreviated/2
	]).

	cover(git).

	% when the directory is not a git repo, the predicates
	% are expected to fail

	test(git_branch_2_01, false) :-
		branch('/', _).

	test(git_commit_log_3_01, false) :-
		commit_log('/', '%h', _).

	test(git_commit_author_2_01, false) :-
		commit_author('/', _).

	test(git_commit_date_2_01, false) :-
		commit_date('/', _).

	test(git_commit_message_2_01, false) :-
		commit_message('/', _).

	test(git_commit_hash_2_01, false) :-
		commit_hash('/', _).

	test(git_commit_hash_abbreviated_2_01, false) :-
		commit_hash_abbreviated('/', _).

:- end_object.
