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

unicode_quotation_mark(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_quotation_mark(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_quotation_mark(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_quotation_mark(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_quotation_mark(0x0022, 0x0022).	% Quotation_Mark # Po       QUOTATION MARK
unicode_quotation_mark(0x0027, 0x0027).	% Quotation_Mark # Po       APOSTROPHE
unicode_quotation_mark(0x00AB, 0x00AB).	% Quotation_Mark # Pi       LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_quotation_mark(0x00BB, 0x00BB).	% Quotation_Mark # Pf       RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_quotation_mark(0x2018, 0x2018).	% Quotation_Mark # Pi       LEFT SINGLE QUOTATION MARK
unicode_quotation_mark(0x2019, 0x2019).	% Quotation_Mark # Pf       RIGHT SINGLE QUOTATION MARK
unicode_quotation_mark(0x201A, 0x201A).	% Quotation_Mark # Ps       SINGLE LOW-9 QUOTATION MARK
unicode_quotation_mark(0x201B, 0x201C).	% Quotation_Mark # Pi   [2] SINGLE HIGH-REVERSED-9 QUOTATION MARK..LEFT DOUBLE QUOTATION MARK
unicode_quotation_mark(0x201D, 0x201D).	% Quotation_Mark # Pf       RIGHT DOUBLE QUOTATION MARK
unicode_quotation_mark(0x201E, 0x201E).	% Quotation_Mark # Ps       DOUBLE LOW-9 QUOTATION MARK
unicode_quotation_mark(0x201F, 0x201F).	% Quotation_Mark # Pi       DOUBLE HIGH-REVERSED-9 QUOTATION MARK
unicode_quotation_mark(0x2039, 0x2039).	% Quotation_Mark # Pi       SINGLE LEFT-POINTING ANGLE QUOTATION MARK
unicode_quotation_mark(0x203A, 0x203A).	% Quotation_Mark # Pf       SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
unicode_quotation_mark(0x300C, 0x300C).	% Quotation_Mark # Ps       LEFT CORNER BRACKET
unicode_quotation_mark(0x300D, 0x300D).	% Quotation_Mark # Pe       RIGHT CORNER BRACKET
unicode_quotation_mark(0x300E, 0x300E).	% Quotation_Mark # Ps       LEFT WHITE CORNER BRACKET
unicode_quotation_mark(0x300F, 0x300F).	% Quotation_Mark # Pe       RIGHT WHITE CORNER BRACKET
unicode_quotation_mark(0x301D, 0x301D).	% Quotation_Mark # Ps       REVERSED DOUBLE PRIME QUOTATION MARK
unicode_quotation_mark(0x301E, 0x301F).	% Quotation_Mark # Pe   [2] DOUBLE PRIME QUOTATION MARK..LOW DOUBLE PRIME QUOTATION MARK
unicode_quotation_mark(0xFE41, 0xFE41).	% Quotation_Mark # Ps       PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
unicode_quotation_mark(0xFE42, 0xFE42).	% Quotation_Mark # Pe       PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
unicode_quotation_mark(0xFE43, 0xFE43).	% Quotation_Mark # Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
unicode_quotation_mark(0xFE44, 0xFE44).	% Quotation_Mark # Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
unicode_quotation_mark(0xFF02, 0xFF02).	% Quotation_Mark # Po       FULLWIDTH QUOTATION MARK
unicode_quotation_mark(0xFF07, 0xFF07).	% Quotation_Mark # Po       FULLWIDTH APOSTROPHE
unicode_quotation_mark(0xFF62, 0xFF62).	% Quotation_Mark # Ps       HALFWIDTH LEFT CORNER BRACKET
unicode_quotation_mark(0xFF63, 0xFF63).	% Quotation_Mark # Pe       HALFWIDTH RIGHT CORNER BRACKET

% Total code points: 29
