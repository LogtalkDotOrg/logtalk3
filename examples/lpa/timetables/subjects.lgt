%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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


:- object(subjects).

	:- info([
		version is 1.0,
		date is 2005/5/8,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'General attributes & methods for all subjects.'
	]).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print complete timetable from the subject viewpoint.'
	]).

	print :-
		nl, write('SUBJECT TIMETABLE ...'), nl, nl,
		forall(extends_object(Subject, subject), Subject::print),
		nl.

:- end_object.


:- object(subject).

	:- info([
		version is 1.0,
		date is 2005/5/8,
		author is 'Example by LPA; adapted to Logtalk by Paulo Moura.',
		comment is 'General attributes & methods for all subjects.'
	]).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print complete timetable from the subject viewpoint.'
	]).

	print :-
		self(Self),
		write('SUBJECT: '), write(Self), nl,
		forall(extends_object(Period, period), Period::print_subject(Self)),
		nl.

:- end_object.


:- object(maths,
	extends(subject)).

:- end_object.


:- object(music,
	extends(subject)).

:- end_object.


:- object(french,
	extends(subject)).

:- end_object.


:- object(prolog,
	extends(subject)).

:- end_object.


:- object(biology,
	extends(subject)).

:- end_object.


:- object('prolog++',
	extends(subject)).

:- end_object.


:- object(accountancy,
	extends(subject)).

:- end_object.
