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

unicode_expands_on_nfkc(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_expands_on_nfkc(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_expands_on_nfkc(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_expands_on_nfkc(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property: Expands_On_NFKC (DEPRECATED as of Unicode 6.0.0)
%   Generated according to UAX #15.
%   Characters whose normalized length is not one.
%   WARNING: Normalization of STRINGS must use the algorithm in UAX #15 because characters may interact.
%            The length of a normalized string is not necessarily the sum of the lengths of the normalized characters!

unicode_expands_on_nfkc(0x00A8, 0x00A8).	% Sk       DIAERESIS
unicode_expands_on_nfkc(0x00AF, 0x00AF).	% Sk       MACRON
unicode_expands_on_nfkc(0x00B4, 0x00B4).	% Sk       ACUTE ACCENT
unicode_expands_on_nfkc(0x00B8, 0x00B8).	% Sk       CEDILLA
unicode_expands_on_nfkc(0x00BC, 0x00BE).	% No   [3] VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS
unicode_expands_on_nfkc(0x0132, 0x0133).	% L&   [2] LATIN CAPITAL LIGATURE IJ..LATIN SMALL LIGATURE IJ
unicode_expands_on_nfkc(0x013F, 0x0140).	% L&   [2] LATIN CAPITAL LETTER L WITH MIDDLE DOT..LATIN SMALL LETTER L WITH MIDDLE DOT
unicode_expands_on_nfkc(0x0149, 0x0149).	% L&       LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
unicode_expands_on_nfkc(0x01C4, 0x01CC).	% L&   [9] LATIN CAPITAL LETTER DZ WITH CARON..LATIN SMALL LETTER NJ
unicode_expands_on_nfkc(0x01F1, 0x01F3).	% L&   [3] LATIN CAPITAL LETTER DZ..LATIN SMALL LETTER DZ
unicode_expands_on_nfkc(0x02D8, 0x02DD).	% Sk   [6] BREVE..DOUBLE ACUTE ACCENT
unicode_expands_on_nfkc(0x0344, 0x0344).	% Mn       COMBINING GREEK DIALYTIKA TONOS
unicode_expands_on_nfkc(0x037A, 0x037A).	% Lm       GREEK YPOGEGRAMMENI
unicode_expands_on_nfkc(0x0384, 0x0385).	% Sk   [2] GREEK TONOS..GREEK DIALYTIKA TONOS
unicode_expands_on_nfkc(0x0587, 0x0587).	% L&       ARMENIAN SMALL LIGATURE ECH YIWN
unicode_expands_on_nfkc(0x0675, 0x0678).	% Lo   [4] ARABIC LETTER HIGH HAMZA ALEF..ARABIC LETTER HIGH HAMZA YEH
unicode_expands_on_nfkc(0x0958, 0x095F).	% Lo   [8] DEVANAGARI LETTER QA..DEVANAGARI LETTER YYA
unicode_expands_on_nfkc(0x09DC, 0x09DD).	% Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_expands_on_nfkc(0x09DF, 0x09DF).	% Lo       BENGALI LETTER YYA
unicode_expands_on_nfkc(0x0A33, 0x0A33).	% Lo       GURMUKHI LETTER LLA
unicode_expands_on_nfkc(0x0A36, 0x0A36).	% Lo       GURMUKHI LETTER SHA
unicode_expands_on_nfkc(0x0A59, 0x0A5B).	% Lo   [3] GURMUKHI LETTER KHHA..GURMUKHI LETTER ZA
unicode_expands_on_nfkc(0x0A5E, 0x0A5E).	% Lo       GURMUKHI LETTER FA
unicode_expands_on_nfkc(0x0B5C, 0x0B5D).	% Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_expands_on_nfkc(0x0E33, 0x0E33).	% Lo       THAI CHARACTER SARA AM
unicode_expands_on_nfkc(0x0EB3, 0x0EB3).	% Lo       LAO VOWEL SIGN AM
unicode_expands_on_nfkc(0x0EDC, 0x0EDD).	% Lo   [2] LAO HO NO..LAO HO MO
unicode_expands_on_nfkc(0x0F43, 0x0F43).	% Lo       TIBETAN LETTER GHA
unicode_expands_on_nfkc(0x0F4D, 0x0F4D).	% Lo       TIBETAN LETTER DDHA
unicode_expands_on_nfkc(0x0F52, 0x0F52).	% Lo       TIBETAN LETTER DHA
unicode_expands_on_nfkc(0x0F57, 0x0F57).	% Lo       TIBETAN LETTER BHA
unicode_expands_on_nfkc(0x0F5C, 0x0F5C).	% Lo       TIBETAN LETTER DZHA
unicode_expands_on_nfkc(0x0F69, 0x0F69).	% Lo       TIBETAN LETTER KSSA
unicode_expands_on_nfkc(0x0F73, 0x0F73).	% Mn       TIBETAN VOWEL SIGN II
unicode_expands_on_nfkc(0x0F75, 0x0F79).	% Mn   [5] TIBETAN VOWEL SIGN UU..TIBETAN VOWEL SIGN VOCALIC LL
unicode_expands_on_nfkc(0x0F81, 0x0F81).	% Mn       TIBETAN VOWEL SIGN REVERSED II
unicode_expands_on_nfkc(0x0F93, 0x0F93).	% Mn       TIBETAN SUBJOINED LETTER GHA
unicode_expands_on_nfkc(0x0F9D, 0x0F9D).	% Mn       TIBETAN SUBJOINED LETTER DDHA
unicode_expands_on_nfkc(0x0FA2, 0x0FA2).	% Mn       TIBETAN SUBJOINED LETTER DHA
unicode_expands_on_nfkc(0x0FA7, 0x0FA7).	% Mn       TIBETAN SUBJOINED LETTER BHA
unicode_expands_on_nfkc(0x0FAC, 0x0FAC).	% Mn       TIBETAN SUBJOINED LETTER DZHA
unicode_expands_on_nfkc(0x0FB9, 0x0FB9).	% Mn       TIBETAN SUBJOINED LETTER KSSA
unicode_expands_on_nfkc(0x1E9A, 0x1E9A).	% L&       LATIN SMALL LETTER A WITH RIGHT HALF RING
unicode_expands_on_nfkc(0x1FBD, 0x1FBD).	% Sk       GREEK KORONIS
unicode_expands_on_nfkc(0x1FBF, 0x1FC1).	% Sk   [3] GREEK PSILI..GREEK DIALYTIKA AND PERISPOMENI
unicode_expands_on_nfkc(0x1FCD, 0x1FCF).	% Sk   [3] GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI
unicode_expands_on_nfkc(0x1FDD, 0x1FDF).	% Sk   [3] GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI
unicode_expands_on_nfkc(0x1FED, 0x1FEE).	% Sk   [2] GREEK DIALYTIKA AND VARIA..GREEK DIALYTIKA AND OXIA
unicode_expands_on_nfkc(0x1FFD, 0x1FFE).	% Sk   [2] GREEK OXIA..GREEK DASIA
unicode_expands_on_nfkc(0x2017, 0x2017).	% Po       DOUBLE LOW LINE
unicode_expands_on_nfkc(0x2025, 0x2026).	% Po   [2] TWO DOT LEADER..HORIZONTAL ELLIPSIS
unicode_expands_on_nfkc(0x2033, 0x2034).	% Po   [2] DOUBLE PRIME..TRIPLE PRIME
unicode_expands_on_nfkc(0x2036, 0x2037).	% Po   [2] REVERSED DOUBLE PRIME..REVERSED TRIPLE PRIME
unicode_expands_on_nfkc(0x203C, 0x203C).	% Po       DOUBLE EXCLAMATION MARK
unicode_expands_on_nfkc(0x203E, 0x203E).	% Po       OVERLINE
unicode_expands_on_nfkc(0x2047, 0x2049).	% Po   [3] DOUBLE QUESTION MARK..EXCLAMATION QUESTION MARK
unicode_expands_on_nfkc(0x2057, 0x2057).	% Po       QUADRUPLE PRIME
unicode_expands_on_nfkc(0x20A8, 0x20A8).	% Sc       RUPEE SIGN
unicode_expands_on_nfkc(0x2100, 0x2101).	% So   [2] ACCOUNT OF..ADDRESSED TO THE SUBJECT
unicode_expands_on_nfkc(0x2103, 0x2103).	% So       DEGREE CELSIUS
unicode_expands_on_nfkc(0x2105, 0x2106).	% So   [2] CARE OF..CADA UNA
unicode_expands_on_nfkc(0x2109, 0x2109).	% So       DEGREE FAHRENHEIT
unicode_expands_on_nfkc(0x2116, 0x2116).	% So       NUMERO SIGN
unicode_expands_on_nfkc(0x2120, 0x2122).	% So   [3] SERVICE MARK..TRADE MARK SIGN
unicode_expands_on_nfkc(0x213B, 0x213B).	% So       FACSIMILE SIGN
unicode_expands_on_nfkc(0x2150, 0x215F).	% No  [16] VULGAR FRACTION ONE SEVENTH..FRACTION NUMERATOR ONE
unicode_expands_on_nfkc(0x2161, 0x2163).	% Nl   [3] ROMAN NUMERAL TWO..ROMAN NUMERAL FOUR
unicode_expands_on_nfkc(0x2165, 0x2168).	% Nl   [4] ROMAN NUMERAL SIX..ROMAN NUMERAL NINE
unicode_expands_on_nfkc(0x216A, 0x216B).	% Nl   [2] ROMAN NUMERAL ELEVEN..ROMAN NUMERAL TWELVE
unicode_expands_on_nfkc(0x2171, 0x2173).	% Nl   [3] SMALL ROMAN NUMERAL TWO..SMALL ROMAN NUMERAL FOUR
unicode_expands_on_nfkc(0x2175, 0x2178).	% Nl   [4] SMALL ROMAN NUMERAL SIX..SMALL ROMAN NUMERAL NINE
unicode_expands_on_nfkc(0x217A, 0x217B).	% Nl   [2] SMALL ROMAN NUMERAL ELEVEN..SMALL ROMAN NUMERAL TWELVE
unicode_expands_on_nfkc(0x2189, 0x2189).	% No       VULGAR FRACTION ZERO THIRDS
unicode_expands_on_nfkc(0x222C, 0x222D).	% Sm   [2] DOUBLE INTEGRAL..TRIPLE INTEGRAL
unicode_expands_on_nfkc(0x222F, 0x2230).	% Sm   [2] SURFACE INTEGRAL..VOLUME INTEGRAL
unicode_expands_on_nfkc(0x2469, 0x249B).	% No  [51] CIRCLED NUMBER TEN..NUMBER TWENTY FULL STOP
unicode_expands_on_nfkc(0x249C, 0x24B5).	% So  [26] PARENTHESIZED LATIN SMALL LETTER A..PARENTHESIZED LATIN SMALL LETTER Z
unicode_expands_on_nfkc(0x2A0C, 0x2A0C).	% Sm       QUADRUPLE INTEGRAL OPERATOR
unicode_expands_on_nfkc(0x2A74, 0x2A76).	% Sm   [3] DOUBLE COLON EQUAL..THREE CONSECUTIVE EQUALS SIGNS
unicode_expands_on_nfkc(0x2ADC, 0x2ADC).	% Sm       FORKING
unicode_expands_on_nfkc(0x309B, 0x309C).	% Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_expands_on_nfkc(0x309F, 0x309F).	% Lo       HIRAGANA DIGRAPH YORI
unicode_expands_on_nfkc(0x30FF, 0x30FF).	% Lo       KATAKANA DIGRAPH KOTO
unicode_expands_on_nfkc(0x3200, 0x321E).	% So  [31] PARENTHESIZED HANGUL KIYEOK..PARENTHESIZED KOREAN CHARACTER O HU
unicode_expands_on_nfkc(0x3220, 0x3229).	% No  [10] PARENTHESIZED IDEOGRAPH ONE..PARENTHESIZED IDEOGRAPH TEN
unicode_expands_on_nfkc(0x322A, 0x3243).	% So  [26] PARENTHESIZED IDEOGRAPH MOON..PARENTHESIZED IDEOGRAPH REACH
unicode_expands_on_nfkc(0x3250, 0x3250).	% So       PARTNERSHIP SIGN
unicode_expands_on_nfkc(0x3251, 0x325F).	% No  [15] CIRCLED NUMBER TWENTY ONE..CIRCLED NUMBER THIRTY FIVE
unicode_expands_on_nfkc(0x327C, 0x327D).	% So   [2] CIRCLED KOREAN CHARACTER CHAMKO..CIRCLED KOREAN CHARACTER JUEUI
unicode_expands_on_nfkc(0x32B1, 0x32BF).	% No  [15] CIRCLED NUMBER THIRTY SIX..CIRCLED NUMBER FIFTY
unicode_expands_on_nfkc(0x32C0, 0x32CF).	% So  [16] IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY..LIMITED LIABILITY SIGN
unicode_expands_on_nfkc(0x3300, 0x33FF).	% So [256] SQUARE APAATO..SQUARE GAL
unicode_expands_on_nfkc(0xFB00, 0xFB06).	% L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_expands_on_nfkc(0xFB13, 0xFB17).	% L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_expands_on_nfkc(0xFB1D, 0xFB1D).	% Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_expands_on_nfkc(0xFB1F, 0xFB1F).	% Lo       HEBREW LIGATURE YIDDISH YOD YOD PATAH
unicode_expands_on_nfkc(0xFB2A, 0xFB36).	% Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_expands_on_nfkc(0xFB38, 0xFB3C).	% Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_expands_on_nfkc(0xFB3E, 0xFB3E).	% Lo       HEBREW LETTER MEM WITH DAGESH
unicode_expands_on_nfkc(0xFB40, 0xFB41).	% Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_expands_on_nfkc(0xFB43, 0xFB44).	% Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_expands_on_nfkc(0xFB46, 0xFB4F).	% Lo  [10] HEBREW LETTER TSADI WITH DAGESH..HEBREW LIGATURE ALEF LAMED
unicode_expands_on_nfkc(0xFBDD, 0xFBDD).	% Lo       ARABIC LETTER U WITH HAMZA ABOVE ISOLATED FORM
unicode_expands_on_nfkc(0xFBEA, 0xFBFB).	% Lo  [18] ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF ISOLATED FORM..ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA INITIAL FORM
unicode_expands_on_nfkc(0xFC00, 0xFD3D).	% Lo [318] ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH JEEM ISOLATED FORM..ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
unicode_expands_on_nfkc(0xFD50, 0xFD8F).	% Lo  [64] ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
unicode_expands_on_nfkc(0xFD92, 0xFDC7).	% Lo  [54] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
unicode_expands_on_nfkc(0xFDF0, 0xFDFB).	% Lo  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
unicode_expands_on_nfkc(0xFDFC, 0xFDFC).	% Sc       RIAL SIGN
unicode_expands_on_nfkc(0xFE19, 0xFE19).	% Po       PRESENTATION FORM FOR VERTICAL HORIZONTAL ELLIPSIS
unicode_expands_on_nfkc(0xFE30, 0xFE30).	% Po       PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
unicode_expands_on_nfkc(0xFE49, 0xFE4C).	% Po   [4] DASHED OVERLINE..DOUBLE WAVY OVERLINE
unicode_expands_on_nfkc(0xFE70, 0xFE72).	% Lo   [3] ARABIC FATHATAN ISOLATED FORM..ARABIC DAMMATAN ISOLATED FORM
unicode_expands_on_nfkc(0xFE74, 0xFE74).	% Lo       ARABIC KASRATAN ISOLATED FORM
unicode_expands_on_nfkc(0xFE76, 0xFE7F).	% Lo  [10] ARABIC FATHA ISOLATED FORM..ARABIC SUKUN MEDIAL FORM
unicode_expands_on_nfkc(0xFEF5, 0xFEFC).	% Lo   [8] ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE ISOLATED FORM..ARABIC LIGATURE LAM WITH ALEF FINAL FORM
unicode_expands_on_nfkc(0xFFE3, 0xFFE3).	% Sk       FULLWIDTH MACRON
unicode_expands_on_nfkc(0x1D15E, 0x1D164).	% So   [7] MUSICAL SYMBOL HALF NOTE..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
unicode_expands_on_nfkc(0x1D1BB, 0x1D1C0).	% So   [6] MUSICAL SYMBOL MINIMA..MUSICAL SYMBOL FUSA BLACK
unicode_expands_on_nfkc(0x1F100, 0x1F10A).	% No  [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
unicode_expands_on_nfkc(0x1F110, 0x1F12A).	% So  [27] PARENTHESIZED LATIN CAPITAL LETTER A..TORTOISE SHELL BRACKETED LATIN CAPITAL LETTER S
unicode_expands_on_nfkc(0x1F12D, 0x1F12E).	% So   [2] CIRCLED CD..CIRCLED WZ
unicode_expands_on_nfkc(0x1F14A, 0x1F14F).	% So   [6] SQUARED HV..SQUARED WC
unicode_expands_on_nfkc(0x1F16A, 0x1F16B).	% So   [2] RAISED MC SIGN..RAISED MD SIGN
unicode_expands_on_nfkc(0x1F190, 0x1F190).	% So       SQUARE DJ
unicode_expands_on_nfkc(0x1F200, 0x1F201).	% So   [2] SQUARE HIRAGANA HOKA..SQUARED KATAKANA KOKO
unicode_expands_on_nfkc(0x1F240, 0x1F248).	% So   [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557

% Total code points: 1235
