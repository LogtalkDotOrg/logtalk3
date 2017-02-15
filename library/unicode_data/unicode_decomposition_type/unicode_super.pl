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

unicode_super(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_super(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_super(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_super(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_super(0x00AA, 0x00AA).		% Super Lo       FEMININE ORDINAL INDICATOR
unicode_super(0x00B2, 0x00B3).		% Super No   [2] SUPERSCRIPT TWO..SUPERSCRIPT THREE
unicode_super(0x00B9, 0x00B9).		% Super No       SUPERSCRIPT ONE
unicode_super(0x00BA, 0x00BA).		% Super Lo       MASCULINE ORDINAL INDICATOR
unicode_super(0x02B0, 0x02B8).		% Super Lm   [9] MODIFIER LETTER SMALL H..MODIFIER LETTER SMALL Y
unicode_super(0x02E0, 0x02E4).		% Super Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_super(0x10FC, 0x10FC).		% Super Lm       MODIFIER LETTER GEORGIAN NAR
unicode_super(0x1D2C, 0x1D2E).		% Super Lm   [3] MODIFIER LETTER CAPITAL A..MODIFIER LETTER CAPITAL B
unicode_super(0x1D30, 0x1D3A).		% Super Lm  [11] MODIFIER LETTER CAPITAL D..MODIFIER LETTER CAPITAL N
unicode_super(0x1D3C, 0x1D4D).		% Super Lm  [18] MODIFIER LETTER CAPITAL O..MODIFIER LETTER SMALL G
unicode_super(0x1D4F, 0x1D61).		% Super Lm  [19] MODIFIER LETTER SMALL K..MODIFIER LETTER SMALL CHI
unicode_super(0x1D78, 0x1D78).		% Super Lm       MODIFIER LETTER CYRILLIC EN
unicode_super(0x1D9B, 0x1DBF).		% Super Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_super(0x2070, 0x2070).		% Super No       SUPERSCRIPT ZERO
unicode_super(0x2071, 0x2071).		% Super Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_super(0x2074, 0x2079).		% Super No   [6] SUPERSCRIPT FOUR..SUPERSCRIPT NINE
unicode_super(0x207A, 0x207C).		% Super Sm   [3] SUPERSCRIPT PLUS SIGN..SUPERSCRIPT EQUALS SIGN
unicode_super(0x207D, 0x207D).		% Super Ps       SUPERSCRIPT LEFT PARENTHESIS
unicode_super(0x207E, 0x207E).		% Super Pe       SUPERSCRIPT RIGHT PARENTHESIS
unicode_super(0x207F, 0x207F).		% Super Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_super(0x2120, 0x2120).		% Super So       SERVICE MARK
unicode_super(0x2122, 0x2122).		% Super So       TRADE MARK SIGN
unicode_super(0x2C7D, 0x2C7D).		% Super Lm       MODIFIER LETTER CAPITAL V
unicode_super(0x2D6F, 0x2D6F).		% Super Lm       TIFINAGH MODIFIER LETTER LABIALIZATION MARK
unicode_super(0x3192, 0x3195).		% Super No   [4] IDEOGRAPHIC ANNOTATION ONE MARK..IDEOGRAPHIC ANNOTATION FOUR MARK
unicode_super(0x3196, 0x319F).		% Super So  [10] IDEOGRAPHIC ANNOTATION TOP MARK..IDEOGRAPHIC ANNOTATION MAN MARK
unicode_super(0xA770, 0xA770).		% Super Lm       MODIFIER LETTER US
unicode_super(0xA7F8, 0xA7F9).		% Super Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_super(0x1F16A, 0x1F16B).	% Super So   [2] RAISED MC SIGN..RAISED MD SIGN

% Total code points: 146
