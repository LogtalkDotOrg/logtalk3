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
# DerivedDecompositionType-6.1.0.txt
# Date: 2011-07-25, 00:54:13 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

# ================================================

# Decomposition_Type (from UnicodeData.txt, field 5: see UAX #44: http://www.unicode.org/reports/tr44/)

#  All code points not explicitly listed for Decomposition_Type
#  have the value None.

# @missing: 0000..10FFFF; None
*/

unicode_sub(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_sub(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_sub(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_sub(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_sub(0x1D62, 0x1D6A).	% Sub Lm   [9] LATIN SUBSCRIPT SMALL LETTER I..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_sub(0x2080, 0x2089).	% Sub No  [10] SUBSCRIPT ZERO..SUBSCRIPT NINE
unicode_sub(0x208A, 0x208C).	% Sub Sm   [3] SUBSCRIPT PLUS SIGN..SUBSCRIPT EQUALS SIGN
unicode_sub(0x208D, 0x208D).	% Sub Ps       SUBSCRIPT LEFT PARENTHESIS
unicode_sub(0x208E, 0x208E).	% Sub Pe       SUBSCRIPT RIGHT PARENTHESIS
unicode_sub(0x2090, 0x209C).	% Sub Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_sub(0x2C7C, 0x2C7C).	% Sub Lm       LATIN SUBSCRIPT SMALL LETTER J

% Total code points: 38
