%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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



:- initialization((
	current_logtalk_flag(version_data, logtalk(LogtalkMajor, LogtalkMinor, LogtalkPatch, LogtalkStatus)),
	write('%         Logtalk version: '),
	write(LogtalkMajor), write('.'), write(LogtalkMinor), write('.'), write(LogtalkPatch), write('-'), write(LogtalkStatus), nl,
	write('%         Prolog version: '),
	current_prolog_flag(version_data, VersionData),
	VersionData =.. [_Dialect, PrologMajor, PrologMinor, PrologPatch| PrologOther],
	write(PrologMajor), write('.'), write(PrologMinor), write('.'), write(PrologPatch),
	(	PrologOther == [] ->
		nl
	;	PrologOther == [[]] ->
		nl
	;	PrologOther = [[Data]] ->
		write('-'), write(Data), nl
	;	PrologOther = [Data] ->
		write('-'), write(Data), nl
	;	write('-'), write(PrologOther), nl
	)
)).
