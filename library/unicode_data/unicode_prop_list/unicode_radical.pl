%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 15, 2012
%
%  Original Unicode file header comments follow

/*
# PropList-6.1.0.txt
# Date: 2011-11-30, 01:49:54 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
*/

unicode_radical(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_radical(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_radical(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_radical(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_radical(0x2E80, 0x2E99).	% Radical # So  [26] CJK RADICAL REPEAT..CJK RADICAL RAP
unicode_radical(0x2E9B, 0x2EF3).	% Radical # So  [89] CJK RADICAL CHOKE..CJK RADICAL C-SIMPLIFIED TURTLE
unicode_radical(0x2F00, 0x2FD5).	% Radical # So [214] KANGXI RADICAL ONE..KANGXI RADICAL FLUTE

% Total code points: 329
