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

unicode_circle(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_circle(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_circle(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_circle(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_circle(0x2460, 0x2473).	% Circle No  [20] CIRCLED DIGIT ONE..CIRCLED NUMBER TWENTY
unicode_circle(0x24B6, 0x24E9).	% Circle So  [52] CIRCLED LATIN CAPITAL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_circle(0x24EA, 0x24EA).	% Circle No       CIRCLED DIGIT ZERO
unicode_circle(0x3244, 0x3247).	% Circle So   [4] CIRCLED IDEOGRAPH QUESTION..CIRCLED IDEOGRAPH KOTO
unicode_circle(0x3251, 0x325F).	% Circle No  [15] CIRCLED NUMBER TWENTY ONE..CIRCLED NUMBER THIRTY FIVE
unicode_circle(0x3260, 0x327E).	% Circle So  [31] CIRCLED HANGUL KIYEOK..CIRCLED HANGUL IEUNG U
unicode_circle(0x3280, 0x3289).	% Circle No  [10] CIRCLED IDEOGRAPH ONE..CIRCLED IDEOGRAPH TEN
unicode_circle(0x328A, 0x32B0).	% Circle So  [39] CIRCLED IDEOGRAPH MOON..CIRCLED IDEOGRAPH NIGHT
unicode_circle(0x32B1, 0x32BF).	% Circle No  [15] CIRCLED NUMBER THIRTY SIX..CIRCLED NUMBER FIFTY
unicode_circle(0x32D0, 0x32FE).	% Circle So  [47] CIRCLED KATAKANA A..CIRCLED KATAKANA WO
unicode_circle(0x1F12B, 0x1F12E).	% Circle So   [4] CIRCLED ITALIC LATIN CAPITAL LETTER C..CIRCLED WZ
unicode_circle(0x1F250, 0x1F251).	% Circle So   [2] CIRCLED IDEOGRAPH ADVANTAGE..CIRCLED IDEOGRAPH ACCEPT

% Total code points: 240
