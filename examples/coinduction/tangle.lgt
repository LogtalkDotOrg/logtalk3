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


:- object(tangle).

	:- info([
		version is 1.1,
		author is 'Feliks Kluzniak. Adapted to Logtalk by Paulo Moura.',
		date is 2013/03/06,
		comment is 'Coinduction example of a predicate with two starting points and no common solution prefix.'
	]).

	:- public(p/1).

	:- coinductive([
		p/1, q/1, r/1
	]).

	p([a| X]) :- q(X).
	p([c| X]) :- r(X).

	q([b| X]) :- p(X).

	r([d| X]) :- p(X).

:- end_object.
