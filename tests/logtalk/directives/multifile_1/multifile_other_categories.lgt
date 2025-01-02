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



:- category(multifile_test_category).

	:- multifile(multifile_primary_object::m1/1).
	multifile_primary_object::m1(4).
	multifile_primary_object::m1(5).

	:- multifile(multifile_primary_object::m3/1).
	multifile_primary_object::m3(G) :-
		call(G).

:- end_category.



:- category(multifile_test_category(_)).

	:- multifile(multifile_primary_object(_)::a/2).
	multifile_primary_object(P)::a(3, P).

	:- multifile(multifile_primary_object(_)::aa/1).
	multifile_primary_object(P)::aa(G) :-
		call(G, P).

:- end_category.
