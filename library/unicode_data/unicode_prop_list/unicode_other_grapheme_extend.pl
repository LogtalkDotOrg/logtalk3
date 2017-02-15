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

unicode_other_grapheme_extend(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_other_grapheme_extend(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_other_grapheme_extend(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_other_grapheme_extend(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_other_grapheme_extend(0x09BE, 0x09BE).		% Other_Grapheme_Extend # Mc       BENGALI VOWEL SIGN AA
unicode_other_grapheme_extend(0x09D7, 0x09D7).		% Other_Grapheme_Extend # Mc       BENGALI AU LENGTH MARK
unicode_other_grapheme_extend(0x0B3E, 0x0B3E).		% Other_Grapheme_Extend # Mc       ORIYA VOWEL SIGN AA
unicode_other_grapheme_extend(0x0B57, 0x0B57).		% Other_Grapheme_Extend # Mc       ORIYA AU LENGTH MARK
unicode_other_grapheme_extend(0x0BBE, 0x0BBE).		% Other_Grapheme_Extend # Mc       TAMIL VOWEL SIGN AA
unicode_other_grapheme_extend(0x0BD7, 0x0BD7).		% Other_Grapheme_Extend # Mc       TAMIL AU LENGTH MARK
unicode_other_grapheme_extend(0x0CC2, 0x0CC2).		% Other_Grapheme_Extend # Mc       KANNADA VOWEL SIGN UU
unicode_other_grapheme_extend(0x0CD5, 0x0CD6).		% Other_Grapheme_Extend # Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
unicode_other_grapheme_extend(0x0D3E, 0x0D3E).		% Other_Grapheme_Extend # Mc       MALAYALAM VOWEL SIGN AA
unicode_other_grapheme_extend(0x0D57, 0x0D57).		% Other_Grapheme_Extend # Mc       MALAYALAM AU LENGTH MARK
unicode_other_grapheme_extend(0x0DCF, 0x0DCF).		% Other_Grapheme_Extend # Mc       SINHALA VOWEL SIGN AELA-PILLA
unicode_other_grapheme_extend(0x0DDF, 0x0DDF).		% Other_Grapheme_Extend # Mc       SINHALA VOWEL SIGN GAYANUKITTA
unicode_other_grapheme_extend(0x200C, 0x200D).		% Other_Grapheme_Extend # Cf   [2] ZERO WIDTH NON-JOINER..ZERO WIDTH JOINER
unicode_other_grapheme_extend(0x302E, 0x302F).		% Other_Grapheme_Extend # Mc   [2] HANGUL SINGLE DOT TONE MARK..HANGUL DOUBLE DOT TONE MARK
unicode_other_grapheme_extend(0xFF9E, 0xFF9F).		% Other_Grapheme_Extend # Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
unicode_other_grapheme_extend(0x1D165, 0x1D165).	% Other_Grapheme_Extend # Mc       MUSICAL SYMBOL COMBINING STEM
unicode_other_grapheme_extend(0x1D16E, 0x1D172).	% Other_Grapheme_Extend # Mc   [5] MUSICAL SYMBOL COMBINING FLAG-1..MUSICAL SYMBOL COMBINING FLAG-5

% Total code points: 25
