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

unicode_wide(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_wide(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_wide(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_wide(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_wide(0x3000, 0x3000).	% Wide Zs       IDEOGRAPHIC SPACE
unicode_wide(0xFF01, 0xFF03).	% Wide Po   [3] FULLWIDTH EXCLAMATION MARK..FULLWIDTH NUMBER SIGN
unicode_wide(0xFF04, 0xFF04).	% Wide Sc       FULLWIDTH DOLLAR SIGN
unicode_wide(0xFF05, 0xFF07).	% Wide Po   [3] FULLWIDTH PERCENT SIGN..FULLWIDTH APOSTROPHE
unicode_wide(0xFF08, 0xFF08).	% Wide Ps       FULLWIDTH LEFT PARENTHESIS
unicode_wide(0xFF09, 0xFF09).	% Wide Pe       FULLWIDTH RIGHT PARENTHESIS
unicode_wide(0xFF0A, 0xFF0A).	% Wide Po       FULLWIDTH ASTERISK
unicode_wide(0xFF0B, 0xFF0B).	% Wide Sm       FULLWIDTH PLUS SIGN
unicode_wide(0xFF0C, 0xFF0C).	% Wide Po       FULLWIDTH COMMA
unicode_wide(0xFF0D, 0xFF0D).	% Wide Pd       FULLWIDTH HYPHEN-MINUS
unicode_wide(0xFF0E, 0xFF0F).	% Wide Po   [2] FULLWIDTH FULL STOP..FULLWIDTH SOLIDUS
unicode_wide(0xFF10, 0xFF19).	% Wide Nd  [10] FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
unicode_wide(0xFF1A, 0xFF1B).	% Wide Po   [2] FULLWIDTH COLON..FULLWIDTH SEMICOLON
unicode_wide(0xFF1C, 0xFF1E).	% Wide Sm   [3] FULLWIDTH LESS-THAN SIGN..FULLWIDTH GREATER-THAN SIGN
unicode_wide(0xFF1F, 0xFF20).	% Wide Po   [2] FULLWIDTH QUESTION MARK..FULLWIDTH COMMERCIAL AT
unicode_wide(0xFF21, 0xFF3A).	% Wide L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_wide(0xFF3B, 0xFF3B).	% Wide Ps       FULLWIDTH LEFT SQUARE BRACKET
unicode_wide(0xFF3C, 0xFF3C).	% Wide Po       FULLWIDTH REVERSE SOLIDUS
unicode_wide(0xFF3D, 0xFF3D).	% Wide Pe       FULLWIDTH RIGHT SQUARE BRACKET
unicode_wide(0xFF3E, 0xFF3E).	% Wide Sk       FULLWIDTH CIRCUMFLEX ACCENT
unicode_wide(0xFF3F, 0xFF3F).	% Wide Pc       FULLWIDTH LOW LINE
unicode_wide(0xFF40, 0xFF40).	% Wide Sk       FULLWIDTH GRAVE ACCENT
unicode_wide(0xFF41, 0xFF5A).	% Wide L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_wide(0xFF5B, 0xFF5B).	% Wide Ps       FULLWIDTH LEFT CURLY BRACKET
unicode_wide(0xFF5C, 0xFF5C).	% Wide Sm       FULLWIDTH VERTICAL LINE
unicode_wide(0xFF5D, 0xFF5D).	% Wide Pe       FULLWIDTH RIGHT CURLY BRACKET
unicode_wide(0xFF5E, 0xFF5E).	% Wide Sm       FULLWIDTH TILDE
unicode_wide(0xFF5F, 0xFF5F).	% Wide Ps       FULLWIDTH LEFT WHITE PARENTHESIS
unicode_wide(0xFF60, 0xFF60).	% Wide Pe       FULLWIDTH RIGHT WHITE PARENTHESIS
unicode_wide(0xFFE0, 0xFFE1).	% Wide Sc   [2] FULLWIDTH CENT SIGN..FULLWIDTH POUND SIGN
unicode_wide(0xFFE2, 0xFFE2).	% Wide Sm       FULLWIDTH NOT SIGN
unicode_wide(0xFFE3, 0xFFE3).	% Wide Sk       FULLWIDTH MACRON
unicode_wide(0xFFE4, 0xFFE4).	% Wide So       FULLWIDTH BROKEN BAR
unicode_wide(0xFFE5, 0xFFE6).	% Wide Sc   [2] FULLWIDTH YEN SIGN..FULLWIDTH WON SIGN

% Total code points: 104
