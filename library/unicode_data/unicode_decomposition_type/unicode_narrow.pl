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

unicode_narrow(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_narrow(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_narrow(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_narrow(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_narrow(0xFF61, 0xFF61).	% Narrow Po       HALFWIDTH IDEOGRAPHIC FULL STOP
unicode_narrow(0xFF62, 0xFF62).	% Narrow Ps       HALFWIDTH LEFT CORNER BRACKET
unicode_narrow(0xFF63, 0xFF63).	% Narrow Pe       HALFWIDTH RIGHT CORNER BRACKET
unicode_narrow(0xFF64, 0xFF65).	% Narrow Po   [2] HALFWIDTH IDEOGRAPHIC COMMA..HALFWIDTH KATAKANA MIDDLE DOT
unicode_narrow(0xFF66, 0xFF6F).	% Narrow Lo  [10] HALFWIDTH KATAKANA LETTER WO..HALFWIDTH KATAKANA LETTER SMALL TU
unicode_narrow(0xFF70, 0xFF70).	% Narrow Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_narrow(0xFF71, 0xFF9D).	% Narrow Lo  [45] HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N
unicode_narrow(0xFF9E, 0xFF9F).	% Narrow Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
unicode_narrow(0xFFA0, 0xFFBE).	% Narrow Lo  [31] HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH
unicode_narrow(0xFFC2, 0xFFC7).	% Narrow Lo   [6] HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E
unicode_narrow(0xFFCA, 0xFFCF).	% Narrow Lo   [6] HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE
unicode_narrow(0xFFD2, 0xFFD7).	% Narrow Lo   [6] HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU
unicode_narrow(0xFFDA, 0xFFDC).	% Narrow Lo   [3] HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I
unicode_narrow(0xFFE8, 0xFFE8).	% Narrow So       HALFWIDTH FORMS LIGHT VERTICAL
unicode_narrow(0xFFE9, 0xFFEC).	% Narrow Sm   [4] HALFWIDTH LEFTWARDS ARROW..HALFWIDTH DOWNWARDS ARROW
unicode_narrow(0xFFED, 0xFFEE).	% Narrow So   [2] HALFWIDTH BLACK SQUARE..HALFWIDTH WHITE CIRCLE

% Total code points: 122
