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

unicode_other_default_ignorable(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_other_default_ignorable(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_other_default_ignorable(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_other_default_ignorable(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_other_default_ignorable(0x034F, 0x034F).	% Other_Default_Ignorable_Code_Point # Mn       COMBINING GRAPHEME JOINER
unicode_other_default_ignorable(0x115F, 0x1160).	% Other_Default_Ignorable_Code_Point # Lo   [2] HANGUL CHOSEONG FILLER..HANGUL JUNGSEONG FILLER
unicode_other_default_ignorable(0x17B4, 0x17B5).	% Other_Default_Ignorable_Code_Point # Mn   [2] KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA
unicode_other_default_ignorable(0x2065, 0x2069).	% Other_Default_Ignorable_Code_Point # Cn   [5] <reserved-2065>..<reserved-2069>
unicode_other_default_ignorable(0x3164, 0x3164).	% Other_Default_Ignorable_Code_Point # Lo       HANGUL FILLER
unicode_other_default_ignorable(0xFFA0, 0xFFA0).	% Other_Default_Ignorable_Code_Point # Lo       HALFWIDTH HANGUL FILLER
unicode_other_default_ignorable(0xFFF0, 0xFFF8).	% Other_Default_Ignorable_Code_Point # Cn   [9] <reserved-FFF0>..<reserved-FFF8>
unicode_other_default_ignorable(0xE0000, 0xE0000).	% Other_Default_Ignorable_Code_Point # Cn       <reserved-E0000>
unicode_other_default_ignorable(0xE0002, 0xE001F).	% Other_Default_Ignorable_Code_Point # Cn  [30] <reserved-E0002>..<reserved-E001F>
unicode_other_default_ignorable(0xE0080, 0xE00FF).	% Other_Default_Ignorable_Code_Point # Cn [128] <reserved-E0080>..<reserved-E00FF>
unicode_other_default_ignorable(0xE01F0, 0xE0FFF).	% Other_Default_Ignorable_Code_Point # Cn [3600] <reserved-E01F0>..<reserved-E0FFF>

% Total code points: 3780
