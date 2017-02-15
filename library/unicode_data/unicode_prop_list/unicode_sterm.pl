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

unicode_sterm(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_sterm(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_sterm(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_sterm(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_sterm(0x0021, 0x0021).	% STerm # Po       EXCLAMATION MARK
unicode_sterm(0x002E, 0x002E).	% STerm # Po       FULL STOP
unicode_sterm(0x003F, 0x003F).	% STerm # Po       QUESTION MARK
unicode_sterm(0x055C, 0x055C).	% STerm # Po       ARMENIAN EXCLAMATION MARK
unicode_sterm(0x055E, 0x055E).	% STerm # Po       ARMENIAN QUESTION MARK
unicode_sterm(0x0589, 0x0589).	% STerm # Po       ARMENIAN FULL STOP
unicode_sterm(0x061F, 0x061F).	% STerm # Po       ARABIC QUESTION MARK
unicode_sterm(0x06D4, 0x06D4).	% STerm # Po       ARABIC FULL STOP
unicode_sterm(0x0700, 0x0702).	% STerm # Po   [3] SYRIAC END OF PARAGRAPH..SYRIAC SUBLINEAR FULL STOP
unicode_sterm(0x07F9, 0x07F9).	% STerm # Po       NKO EXCLAMATION MARK
unicode_sterm(0x0964, 0x0965).	% STerm # Po   [2] DEVANAGARI DANDA..DEVANAGARI DOUBLE DANDA
unicode_sterm(0x104A, 0x104B).	% STerm # Po   [2] MYANMAR SIGN LITTLE SECTION..MYANMAR SIGN SECTION
unicode_sterm(0x1362, 0x1362).	% STerm # Po       ETHIOPIC FULL STOP
unicode_sterm(0x1367, 0x1368).	% STerm # Po   [2] ETHIOPIC QUESTION MARK..ETHIOPIC PARAGRAPH SEPARATOR
unicode_sterm(0x166E, 0x166E).	% STerm # Po       CANADIAN SYLLABICS FULL STOP
unicode_sterm(0x1735, 0x1736).	% STerm # Po   [2] PHILIPPINE SINGLE PUNCTUATION..PHILIPPINE DOUBLE PUNCTUATION
unicode_sterm(0x1803, 0x1803).	% STerm # Po       MONGOLIAN FULL STOP
unicode_sterm(0x1809, 0x1809).	% STerm # Po       MONGOLIAN MANCHU FULL STOP
unicode_sterm(0x1944, 0x1945).	% STerm # Po   [2] LIMBU EXCLAMATION MARK..LIMBU QUESTION MARK
unicode_sterm(0x1AA8, 0x1AAB).	% STerm # Po   [4] TAI THAM SIGN KAAN..TAI THAM SIGN SATKAANKUU
unicode_sterm(0x1B5A, 0x1B5B).	% STerm # Po   [2] BALINESE PANTI..BALINESE PAMADA
unicode_sterm(0x1B5E, 0x1B5F).	% STerm # Po   [2] BALINESE CARIK SIKI..BALINESE CARIK PAREREN
unicode_sterm(0x1C3B, 0x1C3C).	% STerm # Po   [2] LEPCHA PUNCTUATION TA-ROL..LEPCHA PUNCTUATION NYET THYOOM TA-ROL
unicode_sterm(0x1C7E, 0x1C7F).	% STerm # Po   [2] OL CHIKI PUNCTUATION MUCAAD..OL CHIKI PUNCTUATION DOUBLE MUCAAD
unicode_sterm(0x203C, 0x203D).	% STerm # Po   [2] DOUBLE EXCLAMATION MARK..INTERROBANG
unicode_sterm(0x2047, 0x2049).	% STerm # Po   [3] DOUBLE QUESTION MARK..EXCLAMATION QUESTION MARK
unicode_sterm(0x2E2E, 0x2E2E).	% STerm # Po       REVERSED QUESTION MARK
unicode_sterm(0x3002, 0x3002).	% STerm # Po       IDEOGRAPHIC FULL STOP
unicode_sterm(0xA4FF, 0xA4FF).	% STerm # Po       LISU PUNCTUATION FULL STOP
unicode_sterm(0xA60E, 0xA60F).	% STerm # Po   [2] VAI FULL STOP..VAI QUESTION MARK
unicode_sterm(0xA6F3, 0xA6F3).	% STerm # Po       BAMUM FULL STOP
unicode_sterm(0xA6F7, 0xA6F7).	% STerm # Po       BAMUM QUESTION MARK
unicode_sterm(0xA876, 0xA877).	% STerm # Po   [2] PHAGS-PA MARK SHAD..PHAGS-PA MARK DOUBLE SHAD
unicode_sterm(0xA8CE, 0xA8CF).	% STerm # Po   [2] SAURASHTRA DANDA..SAURASHTRA DOUBLE DANDA
unicode_sterm(0xA92F, 0xA92F).	% STerm # Po       KAYAH LI SIGN SHYA
unicode_sterm(0xA9C8, 0xA9C9).	% STerm # Po   [2] JAVANESE PADA LINGSA..JAVANESE PADA LUNGSI
unicode_sterm(0xAA5D, 0xAA5F).	% STerm # Po   [3] CHAM PUNCTUATION DANDA..CHAM PUNCTUATION TRIPLE DANDA
unicode_sterm(0xAAF0, 0xAAF1).	% STerm # Po   [2] MEETEI MAYEK CHEIKHAN..MEETEI MAYEK AHANG KHUDAM
unicode_sterm(0xABEB, 0xABEB).	% STerm # Po       MEETEI MAYEK CHEIKHEI
unicode_sterm(0xFE52, 0xFE52).	% STerm # Po       SMALL FULL STOP
unicode_sterm(0xFE56, 0xFE57).	% STerm # Po   [2] SMALL QUESTION MARK..SMALL EXCLAMATION MARK
unicode_sterm(0xFF01, 0xFF01).	% STerm # Po       FULLWIDTH EXCLAMATION MARK
unicode_sterm(0xFF0E, 0xFF0E).	% STerm # Po       FULLWIDTH FULL STOP
unicode_sterm(0xFF1F, 0xFF1F).	% STerm # Po       FULLWIDTH QUESTION MARK
unicode_sterm(0xFF61, 0xFF61).	% STerm # Po       HALFWIDTH IDEOGRAPHIC FULL STOP
unicode_sterm(0x10A56, 0x10A57).	% STerm # Po   [2] KHAROSHTHI PUNCTUATION DANDA..KHAROSHTHI PUNCTUATION DOUBLE DANDA
unicode_sterm(0x11047, 0x11048).	% STerm # Po   [2] BRAHMI DANDA..BRAHMI DOUBLE DANDA
unicode_sterm(0x110BE, 0x110C1).	% STerm # Po   [4] KAITHI SECTION MARK..KAITHI DOUBLE DANDA
unicode_sterm(0x11141, 0x11143).	% STerm # Po   [3] CHAKMA DANDA..CHAKMA QUESTION MARK
unicode_sterm(0x111C5, 0x111C6).	% STerm # Po   [2] SHARADA DANDA..SHARADA DOUBLE DANDA

% Total code points: 83
