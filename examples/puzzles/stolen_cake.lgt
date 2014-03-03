%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
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
