%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 27, 2012
%
%  Original Unicode file header comments follow

/*
# DerivedNormalizationProps-6.1.0.txt
# Date: 2011-07-26, 04:18:07 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
*/

unicode_nfkc_qc_maybe(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_nfkc_qc_maybe(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_nfkc_qc_maybe(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_nfkc_qc_maybe(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% NFKC_Quick_Check=Maybe

unicode_nfkc_qc_maybe(0x0300, 0x0304).	% Mn   [5] COMBINING GRAVE ACCENT..COMBINING MACRON
unicode_nfkc_qc_maybe(0x0306, 0x030C).	% Mn   [7] COMBINING BREVE..COMBINING CARON
unicode_nfkc_qc_maybe(0x030F, 0x030F).	% Mn       COMBINING DOUBLE GRAVE ACCENT
unicode_nfkc_qc_maybe(0x0311, 0x0311).	% Mn       COMBINING INVERTED BREVE
unicode_nfkc_qc_maybe(0x0313, 0x0314).	% Mn   [2] COMBINING COMMA ABOVE..COMBINING REVERSED COMMA ABOVE
unicode_nfkc_qc_maybe(0x031B, 0x031B).	% Mn       COMBINING HORN
unicode_nfkc_qc_maybe(0x0323, 0x0328).	% Mn   [6] COMBINING DOT BELOW..COMBINING OGONEK
unicode_nfkc_qc_maybe(0x032D, 0x032E).	% Mn   [2] COMBINING CIRCUMFLEX ACCENT BELOW..COMBINING BREVE BELOW
unicode_nfkc_qc_maybe(0x0330, 0x0331).	% Mn   [2] COMBINING TILDE BELOW..COMBINING MACRON BELOW
unicode_nfkc_qc_maybe(0x0338, 0x0338).	% Mn       COMBINING LONG SOLIDUS OVERLAY
unicode_nfkc_qc_maybe(0x0342, 0x0342).	% Mn       COMBINING GREEK PERISPOMENI
unicode_nfkc_qc_maybe(0x0345, 0x0345).	% Mn       COMBINING GREEK YPOGEGRAMMENI
unicode_nfkc_qc_maybe(0x0653, 0x0655).	% Mn   [3] ARABIC MADDAH ABOVE..ARABIC HAMZA BELOW
unicode_nfkc_qc_maybe(0x093C, 0x093C).	% Mn       DEVANAGARI SIGN NUKTA
unicode_nfkc_qc_maybe(0x09BE, 0x09BE).	% Mc       BENGALI VOWEL SIGN AA
unicode_nfkc_qc_maybe(0x09D7, 0x09D7).	% Mc       BENGALI AU LENGTH MARK
unicode_nfkc_qc_maybe(0x0B3E, 0x0B3E).	% Mc       ORIYA VOWEL SIGN AA
unicode_nfkc_qc_maybe(0x0B56, 0x0B56).	% Mn       ORIYA AI LENGTH MARK
unicode_nfkc_qc_maybe(0x0B57, 0x0B57).	% Mc       ORIYA AU LENGTH MARK
unicode_nfkc_qc_maybe(0x0BBE, 0x0BBE).	% Mc       TAMIL VOWEL SIGN AA
unicode_nfkc_qc_maybe(0x0BD7, 0x0BD7).	% Mc       TAMIL AU LENGTH MARK
unicode_nfkc_qc_maybe(0x0C56, 0x0C56).	% Mn       TELUGU AI LENGTH MARK
unicode_nfkc_qc_maybe(0x0CC2, 0x0CC2).	% Mc       KANNADA VOWEL SIGN UU
unicode_nfkc_qc_maybe(0x0CD5, 0x0CD6).	% Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
unicode_nfkc_qc_maybe(0x0D3E, 0x0D3E).	% Mc       MALAYALAM VOWEL SIGN AA
unicode_nfkc_qc_maybe(0x0D57, 0x0D57).	% Mc       MALAYALAM AU LENGTH MARK
unicode_nfkc_qc_maybe(0x0DCA, 0x0DCA).	% Mn       SINHALA SIGN AL-LAKUNA
unicode_nfkc_qc_maybe(0x0DCF, 0x0DCF).	% Mc       SINHALA VOWEL SIGN AELA-PILLA
unicode_nfkc_qc_maybe(0x0DDF, 0x0DDF).	% Mc       SINHALA VOWEL SIGN GAYANUKITTA
unicode_nfkc_qc_maybe(0x102E, 0x102E).	% Mn       MYANMAR VOWEL SIGN II
unicode_nfkc_qc_maybe(0x1161, 0x1175).	% Lo  [21] HANGUL JUNGSEONG A..HANGUL JUNGSEONG I
unicode_nfkc_qc_maybe(0x11A8, 0x11C2).	% Lo  [27] HANGUL JONGSEONG KIYEOK..HANGUL JONGSEONG HIEUH
unicode_nfkc_qc_maybe(0x1B35, 0x1B35).	% Mc       BALINESE VOWEL SIGN TEDUNG
unicode_nfkc_qc_maybe(0x3099, 0x309A).	% Mn   [2] COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK..COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_nfkc_qc_maybe(0x110BA, 0x110BA).	% Mn       KAITHI SIGN NUKTA
unicode_nfkc_qc_maybe(0x11127, 0x11127).	% Mn       CHAKMA VOWEL SIGN A

% Total code points: 104
