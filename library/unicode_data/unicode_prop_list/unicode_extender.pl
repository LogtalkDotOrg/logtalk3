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

unicode_extender(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_extender(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_extender(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_extender(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_extender(0x00B7, 0x00B7).	% Extender # Po       MIDDLE DOT
unicode_extender(0x02D0, 0x02D1).	% Extender # Lm   [2] MODIFIER LETTER TRIANGULAR COLON..MODIFIER LETTER HALF TRIANGULAR COLON
unicode_extender(0x0640, 0x0640).	% Extender # Lm       ARABIC TATWEEL
unicode_extender(0x07FA, 0x07FA).	% Extender # Lm       NKO LAJANYALAN
unicode_extender(0x0E46, 0x0E46).	% Extender # Lm       THAI CHARACTER MAIYAMOK
unicode_extender(0x0EC6, 0x0EC6).	% Extender # Lm       LAO KO LA
unicode_extender(0x180A, 0x180A).	% Extender # Po       MONGOLIAN NIRUGU
unicode_extender(0x1843, 0x1843).	% Extender # Lm       MONGOLIAN LETTER TODO LONG VOWEL SIGN
unicode_extender(0x1AA7, 0x1AA7).	% Extender # Lm       TAI THAM SIGN MAI YAMOK
unicode_extender(0x1C36, 0x1C36).	% Extender # Mn       LEPCHA SIGN RAN
unicode_extender(0x1C7B, 0x1C7B).	% Extender # Lm       OL CHIKI RELAA
unicode_extender(0x3005, 0x3005).	% Extender # Lm       IDEOGRAPHIC ITERATION MARK
unicode_extender(0x3031, 0x3035).	% Extender # Lm   [5] VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF
unicode_extender(0x309D, 0x309E).	% Extender # Lm   [2] HIRAGANA ITERATION MARK..HIRAGANA VOICED ITERATION MARK
unicode_extender(0x30FC, 0x30FE).	% Extender # Lm   [3] KATAKANA-HIRAGANA PROLONGED SOUND MARK..KATAKANA VOICED ITERATION MARK
unicode_extender(0xA015, 0xA015).	% Extender # Lm       YI SYLLABLE WU
unicode_extender(0xA60C, 0xA60C).	% Extender # Lm       VAI SYLLABLE LENGTHENER
unicode_extender(0xA9CF, 0xA9CF).	% Extender # Lm       JAVANESE PANGRANGKEP
unicode_extender(0xAA70, 0xAA70).	% Extender # Lm       MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION
unicode_extender(0xAADD, 0xAADD).	% Extender # Lm       TAI VIET SYMBOL SAM
unicode_extender(0xAAF3, 0xAAF4).	% Extender # Lm   [2] MEETEI MAYEK SYLLABLE REPETITION MARK..MEETEI MAYEK WORD REPETITION MARK
unicode_extender(0xFF70, 0xFF70).	% Extender # Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK

% Total code points: 31
