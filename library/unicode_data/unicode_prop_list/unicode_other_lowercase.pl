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

unicode_other_lowercase(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_other_lowercase(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_other_lowercase(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_other_lowercase(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_other_lowercase(0x00AA, 0x00AA).	% Other_Lowercase # Lo       FEMININE ORDINAL INDICATOR
unicode_other_lowercase(0x00BA, 0x00BA).	% Other_Lowercase # Lo       MASCULINE ORDINAL INDICATOR
unicode_other_lowercase(0x02B0, 0x02B8).	% Other_Lowercase # Lm   [9] MODIFIER LETTER SMALL H..MODIFIER LETTER SMALL Y
unicode_other_lowercase(0x02C0, 0x02C1).	% Other_Lowercase # Lm   [2] MODIFIER LETTER GLOTTAL STOP..MODIFIER LETTER REVERSED GLOTTAL STOP
unicode_other_lowercase(0x02E0, 0x02E4).	% Other_Lowercase # Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_other_lowercase(0x0345, 0x0345).	% Other_Lowercase # Mn       COMBINING GREEK YPOGEGRAMMENI
unicode_other_lowercase(0x037A, 0x037A).	% Other_Lowercase # Lm       GREEK YPOGEGRAMMENI
unicode_other_lowercase(0x1D2C, 0x1D6A).	% Other_Lowercase # Lm  [63] MODIFIER LETTER CAPITAL A..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_other_lowercase(0x1D78, 0x1D78).	% Other_Lowercase # Lm       MODIFIER LETTER CYRILLIC EN
unicode_other_lowercase(0x1D9B, 0x1DBF).	% Other_Lowercase # Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_other_lowercase(0x2071, 0x2071).	% Other_Lowercase # Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_other_lowercase(0x207F, 0x207F).	% Other_Lowercase # Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_other_lowercase(0x2090, 0x209C).	% Other_Lowercase # Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_other_lowercase(0x2170, 0x217F).	% Other_Lowercase # Nl  [16] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL ONE THOUSAND
unicode_other_lowercase(0x24D0, 0x24E9).	% Other_Lowercase # So  [26] CIRCLED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_other_lowercase(0x2C7C, 0x2C7D).	% Other_Lowercase # Lm   [2] LATIN SUBSCRIPT SMALL LETTER J..MODIFIER LETTER CAPITAL V
unicode_other_lowercase(0xA770, 0xA770).	% Other_Lowercase # Lm       MODIFIER LETTER US
unicode_other_lowercase(0xA7F8, 0xA7F9).	% Other_Lowercase # Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE

% Total code points: 183
