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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2017/01/05,
		comment is 'Unit tests for the logtalk_library_path/2 built-in predicate.'
	]).

	% library aliases must be atoms
	test(logtalk_library_path_2_01) :-
		forall(
			logtalk_library_path(Library, Path),
			(	atom(Library),
				ground(Path),
				logtalk::expand_library_path(Library, ExpandedPath),
				atom(ExpandedPath)
			)
		).

	% library alias paths must end with a slash
	test(logtalk_library_path_2_02) :-
		forall(
			logtalk_library_path(Library, _),
			(	logtalk::expand_library_path(Library, ExpandedPath),
				sub_atom(ExpandedPath, _, 1, 0, '/')
			)
		).

:- end_object.
