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

unicode_compat(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_compat(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_compat(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_compat(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_compat(0x00A8, 0x00A8).	% Compat Sk       DIAERESIS
unicode_compat(0x00AF, 0x00AF).	% Compat Sk       MACRON
unicode_compat(0x00B4, 0x00B4).	% Compat Sk       ACUTE ACCENT
unicode_compat(0x00B5, 0x00B5).	% Compat L&       MICRO SIGN
unicode_compat(0x00B8, 0x00B8).	% Compat Sk       CEDILLA
unicode_compat(0x0132, 0x0133).	% Compat L&   [2] LATIN CAPITAL LIGATURE IJ..LATIN SMALL LIGATURE IJ
unicode_compat(0x013F, 0x0140).	% Compat L&   [2] LATIN CAPITAL LETTER L WITH MIDDLE DOT..LATIN SMALL LETTER L WITH MIDDLE DOT
unicode_compat(0x0149, 0x0149).	% Compat L&       LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
unicode_compat(0x017F, 0x017F).	% Compat L&       LATIN SMALL LETTER LONG S
unicode_compat(0x01C4, 0x01CC).	% Compat L&   [9] LATIN CAPITAL LETTER DZ WITH CARON..LATIN SMALL LETTER NJ
unicode_compat(0x01F1, 0x01F3).	% Compat L&   [3] LATIN CAPITAL LETTER DZ..LATIN SMALL LETTER DZ
unicode_compat(0x02D8, 0x02DD).	% Compat Sk   [6] BREVE..DOUBLE ACUTE ACCENT
unicode_compat(0x037A, 0x037A).	% Compat Lm       GREEK YPOGEGRAMMENI
unicode_compat(0x0384, 0x0384).	% Compat Sk       GREEK TONOS
unicode_compat(0x03D0, 0x03D2).	% Compat L&   [3] GREEK BETA SYMBOL..GREEK UPSILON WITH HOOK SYMBOL
unicode_compat(0x03D5, 0x03D6).	% Compat L&   [2] GREEK PHI SYMBOL..GREEK PI SYMBOL
unicode_compat(0x03F0, 0x03F2).	% Compat L&   [3] GREEK KAPPA SYMBOL..GREEK LUNATE SIGMA SYMBOL
unicode_compat(0x03F4, 0x03F5).	% Compat L&   [2] GREEK CAPITAL THETA SYMBOL..GREEK LUNATE EPSILON SYMBOL
unicode_compat(0x03F9, 0x03F9).	% Compat L&       GREEK CAPITAL LUNATE SIGMA SYMBOL
unicode_compat(0x0587, 0x0587).	% Compat L&       ARMENIAN SMALL LIGATURE ECH YIWN
unicode_compat(0x0675, 0x0678).	% Compat Lo   [4] ARABIC LETTER HIGH HAMZA ALEF..ARABIC LETTER HIGH HAMZA YEH
unicode_compat(0x0E33, 0x0E33).	% Compat Lo       THAI CHARACTER SARA AM
unicode_compat(0x0EB3, 0x0EB3).	% Compat Lo       LAO VOWEL SIGN AM
unicode_compat(0x0EDC, 0x0EDD).	% Compat Lo   [2] LAO HO NO..LAO HO MO
unicode_compat(0x0F77, 0x0F77).	% Compat Mn       TIBETAN VOWEL SIGN VOCALIC RR
unicode_compat(0x0F79, 0x0F79).	% Compat Mn       TIBETAN VOWEL SIGN VOCALIC LL
unicode_compat(0x1E9A, 0x1E9A).	% Compat L&       LATIN SMALL LETTER A WITH RIGHT HALF RING
unicode_compat(0x1FBD, 0x1FBD).	% Compat Sk       GREEK KORONIS
unicode_compat(0x1FBF, 0x1FC0).	% Compat Sk   [2] GREEK PSILI..GREEK PERISPOMENI
unicode_compat(0x1FFE, 0x1FFE).	% Compat Sk       GREEK DASIA
unicode_compat(0x2002, 0x2006).	% Compat Zs   [5] EN SPACE..SIX-PER-EM SPACE
unicode_compat(0x2008, 0x200A).	% Compat Zs   [3] PUNCTUATION SPACE..HAIR SPACE
unicode_compat(0x2017, 0x2017).	% Compat Po       DOUBLE LOW LINE
unicode_compat(0x2024, 0x2026).	% Compat Po   [3] ONE DOT LEADER..HORIZONTAL ELLIPSIS
unicode_compat(0x2033, 0x2034).	% Compat Po   [2] DOUBLE PRIME..TRIPLE PRIME
unicode_compat(0x2036, 0x2037).	% Compat Po   [2] REVERSED DOUBLE PRIME..REVERSED TRIPLE PRIME
unicode_compat(0x203C, 0x203C).	% Compat Po       DOUBLE EXCLAMATION MARK
unicode_compat(0x203E, 0x203E).	% Compat Po       OVERLINE
unicode_compat(0x2047, 0x2049).	% Compat Po   [3] DOUBLE QUESTION MARK..EXCLAMATION QUESTION MARK
unicode_compat(0x2057, 0x2057).	% Compat Po       QUADRUPLE PRIME
unicode_compat(0x205F, 0x205F).	% Compat Zs       MEDIUM MATHEMATICAL SPACE
unicode_compat(0x20A8, 0x20A8).	% Compat Sc       RUPEE SIGN
unicode_compat(0x2100, 0x2101).	% Compat So   [2] ACCOUNT OF..ADDRESSED TO THE SUBJECT
unicode_compat(0x2103, 0x2103).	% Compat So       DEGREE CELSIUS
unicode_compat(0x2105, 0x2106).	% Compat So   [2] CARE OF..CADA UNA
unicode_compat(0x2107, 0x2107).	% Compat L&       EULER CONSTANT
unicode_compat(0x2109, 0x2109).	% Compat So       DEGREE FAHRENHEIT
unicode_compat(0x2116, 0x2116).	% Compat So       NUMERO SIGN
unicode_compat(0x2121, 0x2121).	% Compat So       TELEPHONE SIGN
unicode_compat(0x2135, 0x2138).	% Compat Lo   [4] ALEF SYMBOL..DALET SYMBOL
unicode_compat(0x213B, 0x213B).	% Compat So       FACSIMILE SIGN
unicode_compat(0x2160, 0x217F).	% Compat Nl  [32] ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL ONE THOUSAND
unicode_compat(0x222C, 0x222D).	% Compat Sm   [2] DOUBLE INTEGRAL..TRIPLE INTEGRAL
unicode_compat(0x222F, 0x2230).	% Compat Sm   [2] SURFACE INTEGRAL..VOLUME INTEGRAL
unicode_compat(0x2474, 0x249B).	% Compat No  [40] PARENTHESIZED DIGIT ONE..NUMBER TWENTY FULL STOP
unicode_compat(0x249C, 0x24B5).	% Compat So  [26] PARENTHESIZED LATIN SMALL LETTER A..PARENTHESIZED LATIN SMALL LETTER Z
unicode_compat(0x2A0C, 0x2A0C).	% Compat Sm       QUADRUPLE INTEGRAL OPERATOR
unicode_compat(0x2A74, 0x2A76).	% Compat Sm   [3] DOUBLE COLON EQUAL..THREE CONSECUTIVE EQUALS SIGNS
unicode_compat(0x2E9F, 0x2E9F).	% Compat So       CJK RADICAL MOTHER
unicode_compat(0x2EF3, 0x2EF3).	% Compat So       CJK RADICAL C-SIMPLIFIED TURTLE
unicode_compat(0x2F00, 0x2FD5).	% Compat So [214] KANGXI RADICAL ONE..KANGXI RADICAL FLUTE
unicode_compat(0x3036, 0x3036).	% Compat So       CIRCLED POSTAL MARK
unicode_compat(0x3038, 0x303A).	% Compat Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
unicode_compat(0x309B, 0x309C).	% Compat Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_compat(0x3131, 0x318E).	% Compat Lo  [94] HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE
unicode_compat(0x3200, 0x321E).	% Compat So  [31] PARENTHESIZED HANGUL KIYEOK..PARENTHESIZED KOREAN CHARACTER O HU
unicode_compat(0x3220, 0x3229).	% Compat No  [10] PARENTHESIZED IDEOGRAPH ONE..PARENTHESIZED IDEOGRAPH TEN
unicode_compat(0x322A, 0x3243).	% Compat So  [26] PARENTHESIZED IDEOGRAPH MOON..PARENTHESIZED IDEOGRAPH REACH
unicode_compat(0x32C0, 0x32CB).	% Compat So  [12] IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY..IDEOGRAPHIC TELEGRAPH SYMBOL FOR DECEMBER
unicode_compat(0x3358, 0x3370).	% Compat So  [25] IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ZERO..IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-FOUR
unicode_compat(0x33E0, 0x33FE).	% Compat So  [31] IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY ONE..IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTY-ONE
unicode_compat(0xFB00, 0xFB06).	% Compat L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_compat(0xFB13, 0xFB17).	% Compat L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_compat(0xFB4F, 0xFB4F).	% Compat Lo       HEBREW LIGATURE ALEF LAMED
unicode_compat(0xFE49, 0xFE4C).	% Compat Po   [4] DASHED OVERLINE..DOUBLE WAVY OVERLINE
unicode_compat(0xFE4D, 0xFE4F).	% Compat Pc   [3] DASHED LOW LINE..WAVY LOW LINE
unicode_compat(0x1F100, 0x1F10A).	% Compat No  [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
unicode_compat(0x1F110, 0x1F12A).	% Compat So  [27] PARENTHESIZED LATIN CAPITAL LETTER A..TORTOISE SHELL BRACKETED LATIN CAPITAL LETTER S
unicode_compat(0x1F240, 0x1F248).	% Compat So   [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557

% Total code points: 720
