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

unicode_ascii_hex_digit(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_ascii_hex_digit(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_ascii_hex_digit(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_ascii_hex_digit(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_ascii_hex_digit(0x0030, 0x0039).	% ASCII_Hex_Digit # Nd  [10] DIGIT ZERO..DIGIT NINE
unicode_ascii_hex_digit(0x0041, 0x0046).	% ASCII_Hex_Digit # L&   [6] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER F
unicode_ascii_hex_digit(0x0061, 0x0066).	% ASCII_Hex_Digit # L&   [6] LATIN SMALL LETTER A..LATIN SMALL LETTER F

% Total code points: 22
