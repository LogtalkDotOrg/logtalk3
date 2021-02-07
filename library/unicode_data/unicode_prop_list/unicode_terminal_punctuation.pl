%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of VivoMind Prolog Unicode Resources
%  SPDX-License-Identifier: CC0-1.0
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

unicode_terminal_punctuation(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_terminal_punctuation(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_terminal_punctuation(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_terminal_punctuation(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_terminal_punctuation(0x0021, 0x0021).	% Terminal_Punctuation # Po       EXCLAMATION MARK
unicode_terminal_punctuation(0x002C, 0x002C).	% Terminal_Punctuation # Po       COMMA
unicode_terminal_punctuation(0x002E, 0x002E).	% Terminal_Punctuation # Po       FULL STOP
unicode_terminal_punctuation(0x003A, 0x003B).	% Terminal_Punctuation # Po   [2] COLON..SEMICOLON
unicode_terminal_punctuation(0x003F, 0x003F).	% Terminal_Punctuation # Po       QUESTION MARK
unicode_terminal_punctuation(0x037E, 0x037E).	% Terminal_Punctuation # Po       GREEK QUESTION MARK
unicode_terminal_punctuation(0x0387, 0x0387).	% Terminal_Punctuation # Po       GREEK ANO TELEIA
unicode_terminal_punctuation(0x0589, 0x0589).	% Terminal_Punctuation # Po       ARMENIAN FULL STOP
unicode_terminal_punctuation(0x05C3, 0x05C3).	% Terminal_Punctuation # Po       HEBREW PUNCTUATION SOF PASUQ
unicode_terminal_punctuation(0x060C, 0x060C).	% Terminal_Punctuation # Po       ARABIC COMMA
unicode_terminal_punctuation(0x061B, 0x061B).	% Terminal_Punctuation # Po       ARABIC SEMICOLON
unicode_terminal_punctuation(0x061F, 0x061F).	% Terminal_Punctuation # Po       ARABIC QUESTION MARK
unicode_terminal_punctuation(0x06D4, 0x06D4).	% Terminal_Punctuation # Po       ARABIC FULL STOP
unicode_terminal_punctuation(0x0700, 0x070A).	% Terminal_Punctuation # Po  [11] SYRIAC END OF PARAGRAPH..SYRIAC CONTRACTION
unicode_terminal_punctuation(0x070C, 0x070C).	% Terminal_Punctuation # Po       SYRIAC HARKLEAN METOBELUS
unicode_terminal_punctuation(0x07F8, 0x07F9).	% Terminal_Punctuation # Po   [2] NKO COMMA..NKO EXCLAMATION MARK
unicode_terminal_punctuation(0x0830, 0x083E).	% Terminal_Punctuation # Po  [15] SAMARITAN PUNCTUATION NEQUDAA..SAMARITAN PUNCTUATION ANNAAU
unicode_terminal_punctuation(0x085E, 0x085E).	% Terminal_Punctuation # Po       MANDAIC PUNCTUATION
unicode_terminal_punctuation(0x0964, 0x0965).	% Terminal_Punctuation # Po   [2] DEVANAGARI DANDA..DEVANAGARI DOUBLE DANDA
unicode_terminal_punctuation(0x0E5A, 0x0E5B).	% Terminal_Punctuation # Po   [2] THAI CHARACTER ANGKHANKHU..THAI CHARACTER KHOMUT
unicode_terminal_punctuation(0x0F08, 0x0F08).	% Terminal_Punctuation # Po       TIBETAN MARK SBRUL SHAD
unicode_terminal_punctuation(0x0F0D, 0x0F12).	% Terminal_Punctuation # Po   [6] TIBETAN MARK SHAD..TIBETAN MARK RGYA GRAM SHAD
unicode_terminal_punctuation(0x104A, 0x104B).	% Terminal_Punctuation # Po   [2] MYANMAR SIGN LITTLE SECTION..MYANMAR SIGN SECTION
unicode_terminal_punctuation(0x1361, 0x1368).	% Terminal_Punctuation # Po   [8] ETHIOPIC WORDSPACE..ETHIOPIC PARAGRAPH SEPARATOR
unicode_terminal_punctuation(0x166D, 0x166E).	% Terminal_Punctuation # Po   [2] CANADIAN SYLLABICS CHI SIGN..CANADIAN SYLLABICS FULL STOP
unicode_terminal_punctuation(0x16EB, 0x16ED).	% Terminal_Punctuation # Po   [3] RUNIC SINGLE PUNCTUATION..RUNIC CROSS PUNCTUATION
unicode_terminal_punctuation(0x17D4, 0x17D6).	% Terminal_Punctuation # Po   [3] KHMER SIGN KHAN..KHMER SIGN CAMNUC PII KUUH
unicode_terminal_punctuation(0x17DA, 0x17DA).	% Terminal_Punctuation # Po       KHMER SIGN KOOMUUT
unicode_terminal_punctuation(0x1802, 0x1805).	% Terminal_Punctuation # Po   [4] MONGOLIAN COMMA..MONGOLIAN FOUR DOTS
unicode_terminal_punctuation(0x1808, 0x1809).	% Terminal_Punctuation # Po   [2] MONGOLIAN MANCHU COMMA..MONGOLIAN MANCHU FULL STOP
unicode_terminal_punctuation(0x1944, 0x1945).	% Terminal_Punctuation # Po   [2] LIMBU EXCLAMATION MARK..LIMBU QUESTION MARK
unicode_terminal_punctuation(0x1AA8, 0x1AAB).	% Terminal_Punctuation # Po   [4] TAI THAM SIGN KAAN..TAI THAM SIGN SATKAANKUU
unicode_terminal_punctuation(0x1B5A, 0x1B5B).	% Terminal_Punctuation # Po   [2] BALINESE PANTI..BALINESE PAMADA
unicode_terminal_punctuation(0x1B5D, 0x1B5F).	% Terminal_Punctuation # Po   [3] BALINESE CARIK PAMUNGKAH..BALINESE CARIK PAREREN
unicode_terminal_punctuation(0x1C3B, 0x1C3F).	% Terminal_Punctuation # Po   [5] LEPCHA PUNCTUATION TA-ROL..LEPCHA PUNCTUATION TSHOOK
unicode_terminal_punctuation(0x1C7E, 0x1C7F).	% Terminal_Punctuation # Po   [2] OL CHIKI PUNCTUATION MUCAAD..OL CHIKI PUNCTUATION DOUBLE MUCAAD
unicode_terminal_punctuation(0x203C, 0x203D).	% Terminal_Punctuation # Po   [2] DOUBLE EXCLAMATION MARK..INTERROBANG
unicode_terminal_punctuation(0x2047, 0x2049).	% Terminal_Punctuation # Po   [3] DOUBLE QUESTION MARK..EXCLAMATION QUESTION MARK
unicode_terminal_punctuation(0x2E2E, 0x2E2E).	% Terminal_Punctuation # Po       REVERSED QUESTION MARK
unicode_terminal_punctuation(0x3001, 0x3002).	% Terminal_Punctuation # Po   [2] IDEOGRAPHIC COMMA..IDEOGRAPHIC FULL STOP
unicode_terminal_punctuation(0xA4FE, 0xA4FF).	% Terminal_Punctuation # Po   [2] LISU PUNCTUATION COMMA..LISU PUNCTUATION FULL STOP
unicode_terminal_punctuation(0xA60D, 0xA60F).	% Terminal_Punctuation # Po   [3] VAI COMMA..VAI QUESTION MARK
unicode_terminal_punctuation(0xA6F3, 0xA6F7).	% Terminal_Punctuation # Po   [5] BAMUM FULL STOP..BAMUM QUESTION MARK
unicode_terminal_punctuation(0xA876, 0xA877).	% Terminal_Punctuation # Po   [2] PHAGS-PA MARK SHAD..PHAGS-PA MARK DOUBLE SHAD
unicode_terminal_punctuation(0xA8CE, 0xA8CF).	% Terminal_Punctuation # Po   [2] SAURASHTRA DANDA..SAURASHTRA DOUBLE DANDA
unicode_terminal_punctuation(0xA92F, 0xA92F).	% Terminal_Punctuation # Po       KAYAH LI SIGN SHYA
unicode_terminal_punctuation(0xA9C7, 0xA9C9).	% Terminal_Punctuation # Po   [3] JAVANESE PADA PANGKAT..JAVANESE PADA LUNGSI
unicode_terminal_punctuation(0xAA5D, 0xAA5F).	% Terminal_Punctuation # Po   [3] CHAM PUNCTUATION DANDA..CHAM PUNCTUATION TRIPLE DANDA
unicode_terminal_punctuation(0xAADF, 0xAADF).	% Terminal_Punctuation # Po       TAI VIET SYMBOL KOI KOI
unicode_terminal_punctuation(0xAAF0, 0xAAF1).	% Terminal_Punctuation # Po   [2] MEETEI MAYEK CHEIKHAN..MEETEI MAYEK AHANG KHUDAM
unicode_terminal_punctuation(0xABEB, 0xABEB).	% Terminal_Punctuation # Po       MEETEI MAYEK CHEIKHEI
unicode_terminal_punctuation(0xFE50, 0xFE52).	% Terminal_Punctuation # Po   [3] SMALL COMMA..SMALL FULL STOP
unicode_terminal_punctuation(0xFE54, 0xFE57).	% Terminal_Punctuation # Po   [4] SMALL SEMICOLON..SMALL EXCLAMATION MARK
unicode_terminal_punctuation(0xFF01, 0xFF01).	% Terminal_Punctuation # Po       FULLWIDTH EXCLAMATION MARK
unicode_terminal_punctuation(0xFF0C, 0xFF0C).	% Terminal_Punctuation # Po       FULLWIDTH COMMA
unicode_terminal_punctuation(0xFF0E, 0xFF0E).	% Terminal_Punctuation # Po       FULLWIDTH FULL STOP
unicode_terminal_punctuation(0xFF1A, 0xFF1B).	% Terminal_Punctuation # Po   [2] FULLWIDTH COLON..FULLWIDTH SEMICOLON
unicode_terminal_punctuation(0xFF1F, 0xFF1F).	% Terminal_Punctuation # Po       FULLWIDTH QUESTION MARK
unicode_terminal_punctuation(0xFF61, 0xFF61).	% Terminal_Punctuation # Po       HALFWIDTH IDEOGRAPHIC FULL STOP
unicode_terminal_punctuation(0xFF64, 0xFF64).	% Terminal_Punctuation # Po       HALFWIDTH IDEOGRAPHIC COMMA
unicode_terminal_punctuation(0x1039F, 0x1039F).	% Terminal_Punctuation # Po       UGARITIC WORD DIVIDER
unicode_terminal_punctuation(0x103D0, 0x103D0).	% Terminal_Punctuation # Po       OLD PERSIAN WORD DIVIDER
unicode_terminal_punctuation(0x10857, 0x10857).	% Terminal_Punctuation # Po       IMPERIAL ARAMAIC SECTION SIGN
unicode_terminal_punctuation(0x1091F, 0x1091F).	% Terminal_Punctuation # Po       PHOENICIAN WORD SEPARATOR
unicode_terminal_punctuation(0x10B3A, 0x10B3F).	% Terminal_Punctuation # Po   [6] TINY TWO DOTS OVER ONE DOT PUNCTUATION..LARGE ONE RING OVER TWO RINGS PUNCTUATION
unicode_terminal_punctuation(0x11047, 0x1104D).	% Terminal_Punctuation # Po   [7] BRAHMI DANDA..BRAHMI PUNCTUATION LOTUS
unicode_terminal_punctuation(0x110BE, 0x110C1).	% Terminal_Punctuation # Po   [4] KAITHI SECTION MARK..KAITHI DOUBLE DANDA
unicode_terminal_punctuation(0x11141, 0x11143).	% Terminal_Punctuation # Po   [3] CHAKMA DANDA..CHAKMA QUESTION MARK
unicode_terminal_punctuation(0x111C5, 0x111C6).	% Terminal_Punctuation # Po   [2] SHARADA DANDA..SHARADA DOUBLE DANDA
unicode_terminal_punctuation(0x12470, 0x12473).	% Terminal_Punctuation # Po   [4] CUNEIFORM PUNCTUATION SIGN OLD ASSYRIAN WORD DIVIDER..CUNEIFORM PUNCTUATION SIGN DIAGONAL TRICOLON

% Total code points: 176
