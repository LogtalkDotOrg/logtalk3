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



:- object(multifile_primary_object).

	:- public(m1/1).
	:- multifile(m1/1).
	m1(1).
	m1(2).

	:- public(m2/1).
	:- multifile(m2/1).
	:- dynamic(m2/1).
	m2(1).
	m2(2).

:- end_object.



:- object(multifile_primary_object(_)).

	:- public(a/2).
	:- multifile(a/2).

:- end_object.



:- category(multifile_primary_category).

	:- public(n1/1).
	:- multifile(n1/1).
	n1(1).
	n1(2).

	:- public(n2/1).
	:- multifile(n2/1).

:- end_category.



:- category(multifile_primary_category(_)).

	:- public(b/2).
	:- multifile(b/2).

:- end_category.
