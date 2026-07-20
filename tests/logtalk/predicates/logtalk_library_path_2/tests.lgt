%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2026-07-20,
		comment is 'Unit tests for the logtalk_library_path/2 built-in predicate.'
	]).

	:- uses(lgtunit, [
		assertion/2
	]).

	% library aliases must be atoms
	test(logtalk_library_path_2_01) :-
		forall(
			logtalk_library_path(Library, Path),
			(	atom(Library),
				ground(Path),
				logtalk::expand_library_path(Library, ExpandedPath),
				assertion(Library, atom(ExpandedPath))
			)
		).

	% library alias paths must end with a slash
	test(logtalk_library_path_2_02) :-
		forall(
			logtalk_library_path(Library, _),
			(	logtalk::expand_library_path(Library, ExpandedPath),
				assertion(Library, sub_atom(ExpandedPath, _, 1, 0, '/'))
			)
		).

	% library alias paths must exist
	:- if(os::operating_system_type(windows)).
		test(logtalk_library_path_2_03) :-
			forall(
				(	logtalk_library_path(Library, _),
					% $LOGTALKUSER/coding is a symbolic link but backends
					% on Windows don't resolve it
					Library \== coding
				),
				(	logtalk::expand_library_path(Library, ExpandedPath),
					assertion(Library, os::directory_exists(ExpandedPath))
				)
			).
	:- else.
		test(logtalk_library_path_2_03) :-
			forall(
				logtalk_library_path(Library, _),
				(	logtalk::expand_library_path(Library, ExpandedPath),
					assertion(Library, os::directory_exists(ExpandedPath))
				)
			).
	:- endif.

:- end_object.
