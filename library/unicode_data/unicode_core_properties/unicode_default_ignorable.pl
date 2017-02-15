%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: September 30, 2012
%
%  Original Unicode file header comments follow

/*
# DerivedCoreProperties-6.1.0.txt
# Date: 2011-12-11, 18:26:55 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

% ================================================
*/

unicode_default_ignorable(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_default_ignorable(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_default_ignorable(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_default_ignorable(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property: Default_Ignorable_Code_Point
%  Generated from
%    Other_Default_Ignorable_Code_Point
%  + Cf (Format characters)
%  + Variation_Selector
%  - White_Space
%  - FFF9..FFFB (Annotation Characters)
%  - 0600..0604, 06DD, 070F, 110BD (exceptional Cf characters that should be visible)

unicode_default_ignorable(0x00AD, 0x00AD).		% Default_Ignorable_Code_Point Cf       SOFT HYPHEN
unicode_default_ignorable(0x034F, 0x034F).		% Default_Ignorable_Code_Point Mn       COMBINING GRAPHEME JOINER
unicode_default_ignorable(0x115F, 0x1160).		% Default_Ignorable_Code_Point Lo   [2] HANGUL CHOSEONG FILLER..HANGUL JUNGSEONG FILLER
unicode_default_ignorable(0x17B4, 0x17B5).		% Default_Ignorable_Code_Point Mn   [2] KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA
unicode_default_ignorable(0x180B, 0x180D).		% Default_Ignorable_Code_Point Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
unicode_default_ignorable(0x200B, 0x200F).		% Default_Ignorable_Code_Point Cf   [5] ZERO WIDTH SPACE..RIGHT-TO-LEFT MARK
unicode_default_ignorable(0x202A, 0x202E).		% Default_Ignorable_Code_Point Cf   [5] LEFT-TO-RIGHT EMBEDDING..RIGHT-TO-LEFT OVERRIDE
unicode_default_ignorable(0x2060, 0x2064).		% Default_Ignorable_Code_Point Cf   [5] WORD JOINER..INVISIBLE PLUS
unicode_default_ignorable(0x2065, 0x2069).		% Default_Ignorable_Code_Point Cn   [5] <reserved-2065>..<reserved-2069>
unicode_default_ignorable(0x206A, 0x206F).		% Default_Ignorable_Code_Point Cf   [6] INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES
unicode_default_ignorable(0x3164, 0x3164).		% Default_Ignorable_Code_Point Lo       HANGUL FILLER
unicode_default_ignorable(0xFE00, 0xFE0F).		% Default_Ignorable_Code_Point Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
unicode_default_ignorable(0xFEFF, 0xFEFF).		% Default_Ignorable_Code_Point Cf       ZERO WIDTH NO-BREAK SPACE
unicode_default_ignorable(0xFFA0, 0xFFA0).		% Default_Ignorable_Code_Point Lo       HALFWIDTH HANGUL FILLER
unicode_default_ignorable(0xFFF0, 0xFFF8).		% Default_Ignorable_Code_Point Cn   [9] <reserved-FFF0>..<reserved-FFF8>
unicode_default_ignorable(0x1D173, 0x1D17A).	% Default_Ignorable_Code_Point Cf   [8] MUSICAL SYMBOL BEGIN BEAM..MUSICAL SYMBOL END PHRASE
unicode_default_ignorable(0xE0000, 0xE0000).	% Default_Ignorable_Code_Point Cn       <reserved-E0000>
unicode_default_ignorable(0xE0001, 0xE0001).	% Default_Ignorable_Code_Point Cf       LANGUAGE TAG
unicode_default_ignorable(0xE0002, 0xE001F).	% Default_Ignorable_Code_Point Cn  [30] <reserved-E0002>..<reserved-E001F>
unicode_default_ignorable(0xE0020, 0xE007F).	% Default_Ignorable_Code_Point Cf  [96] TAG SPACE..CANCEL TAG
unicode_default_ignorable(0xE0080, 0xE00FF).	% Default_Ignorable_Code_Point Cn [128] <reserved-E0080>..<reserved-E00FF>
unicode_default_ignorable(0xE0100, 0xE01EF).	% Default_Ignorable_Code_Point Mn [240] VARIATION SELECTOR-17..VARIATION SELECTOR-256
unicode_default_ignorable(0xE01F0, 0xE0FFF).	% Default_Ignorable_Code_Point Cn [3600] <reserved-E01F0>..<reserved-E0FFF>

% Total code points: 4167
