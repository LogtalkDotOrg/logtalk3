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


/* Who Stole the cake logical puzzle, Copyright (c) Kevin Stone

During a recent police investigation, Chief Inspector Stone was interviewing five local villains to try and identify who stole Mrs Archer's cake from the mid-summers fayre. Below is a summary of their statements:

Arnold:  it wasn't Edward
         it was Brian

Brian:   it wasn't Charlie
         it wasn't Edward

Charlie: it was Edward
         it wasn't Arnold

Derek:   it was Charlie
         it was Brian

Edward:  it was Derek
         it wasn't Arnold

It was well known that each suspect told exactly one lie. Can you determine who stole the cake?

Published on the web:
	http://www.brainbashers.com/showpuzzles.asp?puzzle=ZFSC
*/


:- object(stolen_cake).

	:- info([
		version is 1.0,
		date is 2008/10/28,
		author is 'Paulo Moura',
		comment is 'Who Stole the cake logical puzzle by Kevin Stone',
		source is 'http://www.brainbashers.com/showpuzzles.asp?puzzle=ZFSC']).

	:- public(thief/1).
	:- mode(thief(?atom), zero_or_one).
	:- info(thief/1, [
		comment is 'Thief that stole the cake.',
		argnames is ['Thief']
	]).

	thief(Thief) :-
		villain(Thief),
		% arnold:
		(	Thief \== edward, Thief \== brian
		;	Thief == edward, Thief == brian
		),
		% brian
		(	Thief \== charlie, Thief == edward
		;	Thief == charlie, Thief \== edward
		),
		% charlie
		(	Thief == edward, Thief == arnold
		;	Thief \== edward, Thief \== arnold
		),
		% derek
		(	Thief == charlie, Thief \== brian
		;	Thief \== charlie, Thief == brian
		),
		% edward
		(	Thief == derek, Thief == arnold
		;	Thief \== derek, Thief \== arnold
		).

	villain(arnold).
	villain(brian).
	villain(charlie).
	villain(derek).
	villain(edward).

:- end_object.
