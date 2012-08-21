%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(descriptors).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/2/18,
		comment is 'Bird descriptors predicates.',
		source is 'Example adapted from an Amzi! Inc Prolog book.']).

	:- public([
		bill/1,
		cheek/1,
		color/1,
		eats/1,
		family/1,
		feed/1,
		feet/1,
		flight/1,
		flight_profile/1,
		head/1,
		live/1,
		neck/1,
		nostrils/1,
		order/1,
		size/1,
		tail/1,
		throat/1,
		voice/1,
		wings/1]).

	:- public(descriptor/1).

	descriptor(bill/1).
	descriptor(cheek/1).
	descriptor(color/1).
	descriptor(eats/1).
	descriptor(feed/1).
	descriptor(feet/1).
	descriptor(flight/1).
	descriptor(flight_profile/1).
	descriptor(head/1).
	descriptor(live/1).
	descriptor(neck/1).
	descriptor(nostrils/1).
	descriptor(size/1).
	descriptor(tail/1).
	descriptor(throat/1).
	descriptor(voice/1).
	descriptor(wings/1).

:- end_category.
