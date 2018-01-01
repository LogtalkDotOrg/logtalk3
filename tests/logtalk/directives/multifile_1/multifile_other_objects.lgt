%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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



:- object(multifile_other_object,
	imports(multifile_primary_category)).

	:- public(db/2).
	:- dynamic(db/2).

	:- multifile(multifile_primary_object::m2/1).
	:- dynamic(multifile_primary_object::m2/1).
	multifile_primary_object::m2(4).
	multifile_primary_object::m2(5).
	multifile_primary_object::m2(6) :-
		assertz(db(m2, 6)).

	:- multifile(multifile_primary_category::n1/1).
	multifile_primary_category::n1(4).
	multifile_primary_category::n1(5) :-
		assertz(db(n2, 5)).

:- end_object.



:- object(multifile_other_object(P),
	imports(multifile_primary_category(P))).

	:- public(dbp/3).
	:- dynamic(dbp/3).

	:- multifile(multifile_primary_object(_)::a/2).
	multifile_primary_object(P)::a(1, P).
	multifile_primary_object(P)::a(2, P) :-
		assertz(dbp(a, 2, P)).

	:- multifile(multifile_primary_category(_)::b/2).
	multifile_primary_category(P)::b(1, P).
	multifile_primary_category(P)::b(2, P) :-
		assertz(dbp(b, 2, P)).

:- end_object.
