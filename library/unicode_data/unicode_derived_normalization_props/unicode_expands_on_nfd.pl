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

unicode_expands_on_nfd(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_expands_on_nfd(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_expands_on_nfd(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_expands_on_nfd(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property: Expands_On_NFD (DEPRECATED as of Unicode 6.0.0)
%   Generated according to UAX #15.
%   Characters whose normalized length is not one.
%   WARNING: Normalization of STRINGS must use the algorithm in UAX #15 because characters may interact.
%            The length of a normalized string is not necessarily the sum of the lengths of the normalized characters!

unicode_expands_on_nfd(0x00C0, 0x00C5).	%  L&   [6] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER A WITH RING ABOVE
unicode_expands_on_nfd(0x00C7, 0x00CF).	%  L&   [9] LATIN CAPITAL LETTER C WITH CEDILLA..LATIN CAPITAL LETTER I WITH DIAERESIS
unicode_expands_on_nfd(0x00D1, 0x00D6).	%  L&   [6] LATIN CAPITAL LETTER N WITH TILDE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_expands_on_nfd(0x00D9, 0x00DD).	%  L&   [5] LATIN CAPITAL LETTER U WITH GRAVE..LATIN CAPITAL LETTER Y WITH ACUTE
unicode_expands_on_nfd(0x00E0, 0x00E5).	%  L&   [6] LATIN SMALL LETTER A WITH GRAVE..LATIN SMALL LETTER A WITH RING ABOVE
unicode_expands_on_nfd(0x00E7, 0x00EF).	%  L&   [9] LATIN SMALL LETTER C WITH CEDILLA..LATIN SMALL LETTER I WITH DIAERESIS
unicode_expands_on_nfd(0x00F1, 0x00F6).	%  L&   [6] LATIN SMALL LETTER N WITH TILDE..LATIN SMALL LETTER O WITH DIAERESIS
unicode_expands_on_nfd(0x00F9, 0x00FD).	%  L&   [5] LATIN SMALL LETTER U WITH GRAVE..LATIN SMALL LETTER Y WITH ACUTE
unicode_expands_on_nfd(0x00FF, 0x010F).	%  L&  [17] LATIN SMALL LETTER Y WITH DIAERESIS..LATIN SMALL LETTER D WITH CARON
unicode_expands_on_nfd(0x0112, 0x0125).	%  L&  [20] LATIN CAPITAL LETTER E WITH MACRON..LATIN SMALL LETTER H WITH CIRCUMFLEX
unicode_expands_on_nfd(0x0128, 0x0130).	%  L&   [9] LATIN CAPITAL LETTER I WITH TILDE..LATIN CAPITAL LETTER I WITH DOT ABOVE
unicode_expands_on_nfd(0x0134, 0x0137).	%  L&   [4] LATIN CAPITAL LETTER J WITH CIRCUMFLEX..LATIN SMALL LETTER K WITH CEDILLA
unicode_expands_on_nfd(0x0139, 0x013E).	%  L&   [6] LATIN CAPITAL LETTER L WITH ACUTE..LATIN SMALL LETTER L WITH CARON
unicode_expands_on_nfd(0x0143, 0x0148).	%  L&   [6] LATIN CAPITAL LETTER N WITH ACUTE..LATIN SMALL LETTER N WITH CARON
unicode_expands_on_nfd(0x014C, 0x0151).	%  L&   [6] LATIN CAPITAL LETTER O WITH MACRON..LATIN SMALL LETTER O WITH DOUBLE ACUTE
unicode_expands_on_nfd(0x0154, 0x0165).	%  L&  [18] LATIN CAPITAL LETTER R WITH ACUTE..LATIN SMALL LETTER T WITH CARON
unicode_expands_on_nfd(0x0168, 0x017E).	%  L&  [23] LATIN CAPITAL LETTER U WITH TILDE..LATIN SMALL LETTER Z WITH CARON
unicode_expands_on_nfd(0x01A0, 0x01A1).	%  L&   [2] LATIN CAPITAL LETTER O WITH HORN..LATIN SMALL LETTER O WITH HORN
unicode_expands_on_nfd(0x01AF, 0x01B0).	%  L&   [2] LATIN CAPITAL LETTER U WITH HORN..LATIN SMALL LETTER U WITH HORN
unicode_expands_on_nfd(0x01CD, 0x01DC).	%  L&  [16] LATIN CAPITAL LETTER A WITH CARON..LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
unicode_expands_on_nfd(0x01DE, 0x01E3).	%  L&   [6] LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON..LATIN SMALL LETTER AE WITH MACRON
unicode_expands_on_nfd(0x01E6, 0x01F0).	%  L&  [11] LATIN CAPITAL LETTER G WITH CARON..LATIN SMALL LETTER J WITH CARON
unicode_expands_on_nfd(0x01F4, 0x01F5).	%  L&   [2] LATIN CAPITAL LETTER G WITH ACUTE..LATIN SMALL LETTER G WITH ACUTE
unicode_expands_on_nfd(0x01F8, 0x021B).	%  L&  [36] LATIN CAPITAL LETTER N WITH GRAVE..LATIN SMALL LETTER T WITH COMMA BELOW
unicode_expands_on_nfd(0x021E, 0x021F).	%  L&   [2] LATIN CAPITAL LETTER H WITH CARON..LATIN SMALL LETTER H WITH CARON
unicode_expands_on_nfd(0x0226, 0x0233).	%  L&  [14] LATIN CAPITAL LETTER A WITH DOT ABOVE..LATIN SMALL LETTER Y WITH MACRON
unicode_expands_on_nfd(0x0344, 0x0344).	%  Mn       COMBINING GREEK DIALYTIKA TONOS
unicode_expands_on_nfd(0x0385, 0x0385).	%  Sk       GREEK DIALYTIKA TONOS
unicode_expands_on_nfd(0x0386, 0x0386).	%  L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_expands_on_nfd(0x0388, 0x038A).	%  L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_expands_on_nfd(0x038C, 0x038C).	%  L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_expands_on_nfd(0x038E, 0x0390).	%  L&   [3] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
unicode_expands_on_nfd(0x03AA, 0x03B0).	%  L&   [7] GREEK CAPITAL LETTER IOTA WITH DIALYTIKA..GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
unicode_expands_on_nfd(0x03CA, 0x03CE).	%  L&   [5] GREEK SMALL LETTER IOTA WITH DIALYTIKA..GREEK SMALL LETTER OMEGA WITH TONOS
unicode_expands_on_nfd(0x03D3, 0x03D4).	%  L&   [2] GREEK UPSILON WITH ACUTE AND HOOK SYMBOL..GREEK UPSILON WITH DIAERESIS AND HOOK SYMBOL
unicode_expands_on_nfd(0x0400, 0x0401).	%  L&   [2] CYRILLIC CAPITAL LETTER IE WITH GRAVE..CYRILLIC CAPITAL LETTER IO
unicode_expands_on_nfd(0x0403, 0x0403).	%  L&       CYRILLIC CAPITAL LETTER GJE
unicode_expands_on_nfd(0x0407, 0x0407).	%  L&       CYRILLIC CAPITAL LETTER YI
unicode_expands_on_nfd(0x040C, 0x040E).	%  L&   [3] CYRILLIC CAPITAL LETTER KJE..CYRILLIC CAPITAL LETTER SHORT U
unicode_expands_on_nfd(0x0419, 0x0419).	%  L&       CYRILLIC CAPITAL LETTER SHORT I
unicode_expands_on_nfd(0x0439, 0x0439).	%  L&       CYRILLIC SMALL LETTER SHORT I
unicode_expands_on_nfd(0x0450, 0x0451).	%  L&   [2] CYRILLIC SMALL LETTER IE WITH GRAVE..CYRILLIC SMALL LETTER IO
unicode_expands_on_nfd(0x0453, 0x0453).	%  L&       CYRILLIC SMALL LETTER GJE
unicode_expands_on_nfd(0x0457, 0x0457).	%  L&       CYRILLIC SMALL LETTER YI
unicode_expands_on_nfd(0x045C, 0x045E).	%  L&   [3] CYRILLIC SMALL LETTER KJE..CYRILLIC SMALL LETTER SHORT U
unicode_expands_on_nfd(0x0476, 0x0477).	%  L&   [2] CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT..CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
unicode_expands_on_nfd(0x04C1, 0x04C2).	%  L&   [2] CYRILLIC CAPITAL LETTER ZHE WITH BREVE..CYRILLIC SMALL LETTER ZHE WITH BREVE
unicode_expands_on_nfd(0x04D0, 0x04D3).	%  L&   [4] CYRILLIC CAPITAL LETTER A WITH BREVE..CYRILLIC SMALL LETTER A WITH DIAERESIS
unicode_expands_on_nfd(0x04D6, 0x04D7).	%  L&   [2] CYRILLIC CAPITAL LETTER IE WITH BREVE..CYRILLIC SMALL LETTER IE WITH BREVE
unicode_expands_on_nfd(0x04DA, 0x04DF).	%  L&   [6] CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS..CYRILLIC SMALL LETTER ZE WITH DIAERESIS
unicode_expands_on_nfd(0x04E2, 0x04E7).	%  L&   [6] CYRILLIC CAPITAL LETTER I WITH MACRON..CYRILLIC SMALL LETTER O WITH DIAERESIS
unicode_expands_on_nfd(0x04EA, 0x04F5).	%  L&  [12] CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS..CYRILLIC SMALL LETTER CHE WITH DIAERESIS
unicode_expands_on_nfd(0x04F8, 0x04F9).	%  L&   [2] CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS..CYRILLIC SMALL LETTER YERU WITH DIAERESIS
unicode_expands_on_nfd(0x0622, 0x0626).	%  Lo   [5] ARABIC LETTER ALEF WITH MADDA ABOVE..ARABIC LETTER YEH WITH HAMZA ABOVE
unicode_expands_on_nfd(0x06C0, 0x06C0).	%  Lo       ARABIC LETTER HEH WITH YEH ABOVE
unicode_expands_on_nfd(0x06C2, 0x06C2).	%  Lo       ARABIC LETTER HEH GOAL WITH HAMZA ABOVE
unicode_expands_on_nfd(0x06D3, 0x06D3).	%  Lo       ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
unicode_expands_on_nfd(0x0929, 0x0929).	%  Lo       DEVANAGARI LETTER NNNA
unicode_expands_on_nfd(0x0931, 0x0931).	%  Lo       DEVANAGARI LETTER RRA
unicode_expands_on_nfd(0x0934, 0x0934).	%  Lo       DEVANAGARI LETTER LLLA
unicode_expands_on_nfd(0x0958, 0x095F).	%  Lo   [8] DEVANAGARI LETTER QA..DEVANAGARI LETTER YYA
unicode_expands_on_nfd(0x09CB, 0x09CC).	%  Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_expands_on_nfd(0x09DC, 0x09DD).	%  Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_expands_on_nfd(0x09DF, 0x09DF).	%  Lo       BENGALI LETTER YYA
unicode_expands_on_nfd(0x0A33, 0x0A33).	%  Lo       GURMUKHI LETTER LLA
unicode_expands_on_nfd(0x0A36, 0x0A36).	%  Lo       GURMUKHI LETTER SHA
unicode_expands_on_nfd(0x0A59, 0x0A5B).	%  Lo   [3] GURMUKHI LETTER KHHA..GURMUKHI LETTER ZA
unicode_expands_on_nfd(0x0A5E, 0x0A5E).	%  Lo       GURMUKHI LETTER FA
unicode_expands_on_nfd(0x0B48, 0x0B48).	%  Mc       ORIYA VOWEL SIGN AI
unicode_expands_on_nfd(0x0B4B, 0x0B4C).	%  Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
unicode_expands_on_nfd(0x0B5C, 0x0B5D).	%  Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_expands_on_nfd(0x0B94, 0x0B94).	%  Lo       TAMIL LETTER AU
unicode_expands_on_nfd(0x0BCA, 0x0BCC).	%  Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_expands_on_nfd(0x0C48, 0x0C48).	%  Mn       TELUGU VOWEL SIGN AI
unicode_expands_on_nfd(0x0CC0, 0x0CC0).	%  Mc       KANNADA VOWEL SIGN II
unicode_expands_on_nfd(0x0CC7, 0x0CC8).	%  Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_expands_on_nfd(0x0CCA, 0x0CCB).	%  Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_expands_on_nfd(0x0D4A, 0x0D4C).	%  Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_expands_on_nfd(0x0DDA, 0x0DDA).	%  Mc       SINHALA VOWEL SIGN DIGA KOMBUVA
unicode_expands_on_nfd(0x0DDC, 0x0DDE).	%  Mc   [3] SINHALA VOWEL SIGN KOMBUVA HAA AELA-PILLA..SINHALA VOWEL SIGN KOMBUVA HAA GAYANUKITTA
unicode_expands_on_nfd(0x0F43, 0x0F43).	%  Lo       TIBETAN LETTER GHA
unicode_expands_on_nfd(0x0F4D, 0x0F4D).	%  Lo       TIBETAN LETTER DDHA
unicode_expands_on_nfd(0x0F52, 0x0F52).	%  Lo       TIBETAN LETTER DHA
unicode_expands_on_nfd(0x0F57, 0x0F57).	%  Lo       TIBETAN LETTER BHA
unicode_expands_on_nfd(0x0F5C, 0x0F5C).	%  Lo       TIBETAN LETTER DZHA
unicode_expands_on_nfd(0x0F69, 0x0F69).	%  Lo       TIBETAN LETTER KSSA
unicode_expands_on_nfd(0x0F73, 0x0F73).	%  Mn       TIBETAN VOWEL SIGN II
unicode_expands_on_nfd(0x0F75, 0x0F76).	%  Mn   [2] TIBETAN VOWEL SIGN UU..TIBETAN VOWEL SIGN VOCALIC R
unicode_expands_on_nfd(0x0F78, 0x0F78).	%  Mn       TIBETAN VOWEL SIGN VOCALIC L
unicode_expands_on_nfd(0x0F81, 0x0F81).	%  Mn       TIBETAN VOWEL SIGN REVERSED II
unicode_expands_on_nfd(0x0F93, 0x0F93).	%  Mn       TIBETAN SUBJOINED LETTER GHA
unicode_expands_on_nfd(0x0F9D, 0x0F9D).	%  Mn       TIBETAN SUBJOINED LETTER DDHA
unicode_expands_on_nfd(0x0FA2, 0x0FA2).	%  Mn       TIBETAN SUBJOINED LETTER DHA
unicode_expands_on_nfd(0x0FA7, 0x0FA7).	%  Mn       TIBETAN SUBJOINED LETTER BHA
unicode_expands_on_nfd(0x0FAC, 0x0FAC).	%  Mn       TIBETAN SUBJOINED LETTER DZHA
unicode_expands_on_nfd(0x0FB9, 0x0FB9).	%  Mn       TIBETAN SUBJOINED LETTER KSSA
unicode_expands_on_nfd(0x1026, 0x1026).	%  Lo       MYANMAR LETTER UU
unicode_expands_on_nfd(0x1B06, 0x1B06).	%  Lo       BALINESE LETTER AKARA TEDUNG
unicode_expands_on_nfd(0x1B08, 0x1B08).	%  Lo       BALINESE LETTER IKARA TEDUNG
unicode_expands_on_nfd(0x1B0A, 0x1B0A).	%  Lo       BALINESE LETTER UKARA TEDUNG
unicode_expands_on_nfd(0x1B0C, 0x1B0C).	%  Lo       BALINESE LETTER RA REPA TEDUNG
unicode_expands_on_nfd(0x1B0E, 0x1B0E).	%  Lo       BALINESE LETTER LA LENGA TEDUNG
unicode_expands_on_nfd(0x1B12, 0x1B12).	%  Lo       BALINESE LETTER OKARA TEDUNG
unicode_expands_on_nfd(0x1B3B, 0x1B3B).	%  Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_expands_on_nfd(0x1B3D, 0x1B3D).	%  Mc       BALINESE VOWEL SIGN LA LENGA TEDUNG
unicode_expands_on_nfd(0x1B40, 0x1B41).	%  Mc   [2] BALINESE VOWEL SIGN TALING TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG
unicode_expands_on_nfd(0x1B43, 0x1B43).	%  Mc       BALINESE VOWEL SIGN PEPET TEDUNG
unicode_expands_on_nfd(0x1E00, 0x1E99).	%  L& [154] LATIN CAPITAL LETTER A WITH RING BELOW..LATIN SMALL LETTER Y WITH RING ABOVE
unicode_expands_on_nfd(0x1E9B, 0x1E9B).	%  L&       LATIN SMALL LETTER LONG S WITH DOT ABOVE
unicode_expands_on_nfd(0x1EA0, 0x1EF9).	%  L&  [90] LATIN CAPITAL LETTER A WITH DOT BELOW..LATIN SMALL LETTER Y WITH TILDE
unicode_expands_on_nfd(0x1F00, 0x1F15).	%  L&  [22] GREEK SMALL LETTER ALPHA WITH PSILI..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_expands_on_nfd(0x1F18, 0x1F1D).	%  L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_expands_on_nfd(0x1F20, 0x1F45).	%  L&  [38] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_expands_on_nfd(0x1F48, 0x1F4D).	%  L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_expands_on_nfd(0x1F50, 0x1F57).	%  L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_expands_on_nfd(0x1F59, 0x1F59).	%  L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_expands_on_nfd(0x1F5B, 0x1F5B).	%  L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_expands_on_nfd(0x1F5D, 0x1F5D).	%  L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_expands_on_nfd(0x1F5F, 0x1F7D).	%  L&  [31] GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_expands_on_nfd(0x1F80, 0x1FB4).	%  L&  [53] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_expands_on_nfd(0x1FB6, 0x1FBC).	%  L&   [7] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_expands_on_nfd(0x1FC1, 0x1FC1).	%  Sk       GREEK DIALYTIKA AND PERISPOMENI
unicode_expands_on_nfd(0x1FC2, 0x1FC4).	%  L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_expands_on_nfd(0x1FC6, 0x1FCC).	%  L&   [7] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_expands_on_nfd(0x1FCD, 0x1FCF).	%  Sk   [3] GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI
unicode_expands_on_nfd(0x1FD0, 0x1FD3).	%  L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_expands_on_nfd(0x1FD6, 0x1FDB).	%  L&   [6] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_expands_on_nfd(0x1FDD, 0x1FDF).	%  Sk   [3] GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI
unicode_expands_on_nfd(0x1FE0, 0x1FEC).	%  L&  [13] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_expands_on_nfd(0x1FED, 0x1FEE).	%  Sk   [2] GREEK DIALYTIKA AND VARIA..GREEK DIALYTIKA AND OXIA
unicode_expands_on_nfd(0x1FF2, 0x1FF4).	%  L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_expands_on_nfd(0x1FF6, 0x1FFC).	%  L&   [7] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_expands_on_nfd(0x212B, 0x212B).	%  L&       ANGSTROM SIGN
unicode_expands_on_nfd(0x219A, 0x219B).	%  Sm   [2] LEFTWARDS ARROW WITH STROKE..RIGHTWARDS ARROW WITH STROKE
unicode_expands_on_nfd(0x21AE, 0x21AE).	%  Sm       LEFT RIGHT ARROW WITH STROKE
unicode_expands_on_nfd(0x21CD, 0x21CD).	%  So       LEFTWARDS DOUBLE ARROW WITH STROKE
unicode_expands_on_nfd(0x21CE, 0x21CF).	%  Sm   [2] LEFT RIGHT DOUBLE ARROW WITH STROKE..RIGHTWARDS DOUBLE ARROW WITH STROKE
unicode_expands_on_nfd(0x2204, 0x2204).	%  Sm       THERE DOES NOT EXIST
unicode_expands_on_nfd(0x2209, 0x2209).	%  Sm       NOT AN ELEMENT OF
unicode_expands_on_nfd(0x220C, 0x220C).	%  Sm       DOES NOT CONTAIN AS MEMBER
unicode_expands_on_nfd(0x2224, 0x2224).	%  Sm       DOES NOT DIVIDE
unicode_expands_on_nfd(0x2226, 0x2226).	%  Sm       NOT PARALLEL TO
unicode_expands_on_nfd(0x2241, 0x2241).	%  Sm       NOT TILDE
unicode_expands_on_nfd(0x2244, 0x2244).	%  Sm       NOT ASYMPTOTICALLY EQUAL TO
unicode_expands_on_nfd(0x2247, 0x2247).	%  Sm       NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
unicode_expands_on_nfd(0x2249, 0x2249).	%  Sm       NOT ALMOST EQUAL TO
unicode_expands_on_nfd(0x2260, 0x2260).	%  Sm       NOT EQUAL TO
unicode_expands_on_nfd(0x2262, 0x2262).	%  Sm       NOT IDENTICAL TO
unicode_expands_on_nfd(0x226D, 0x2271).	%  Sm   [5] NOT EQUIVALENT TO..NEITHER GREATER-THAN NOR EQUAL TO
unicode_expands_on_nfd(0x2274, 0x2275).	%  Sm   [2] NEITHER LESS-THAN NOR EQUIVALENT TO..NEITHER GREATER-THAN NOR EQUIVALENT TO
unicode_expands_on_nfd(0x2278, 0x2279).	%  Sm   [2] NEITHER LESS-THAN NOR GREATER-THAN..NEITHER GREATER-THAN NOR LESS-THAN
unicode_expands_on_nfd(0x2280, 0x2281).	%  Sm   [2] DOES NOT PRECEDE..DOES NOT SUCCEED
unicode_expands_on_nfd(0x2284, 0x2285).	%  Sm   [2] NOT A SUBSET OF..NOT A SUPERSET OF
unicode_expands_on_nfd(0x2288, 0x2289).	%  Sm   [2] NEITHER A SUBSET OF NOR EQUAL TO..NEITHER A SUPERSET OF NOR EQUAL TO
unicode_expands_on_nfd(0x22AC, 0x22AF).	%  Sm   [4] DOES NOT PROVE..NEGATED DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
unicode_expands_on_nfd(0x22E0, 0x22E3).	%  Sm   [4] DOES NOT PRECEDE OR EQUAL..NOT SQUARE ORIGINAL OF OR EQUAL TO
unicode_expands_on_nfd(0x22EA, 0x22ED).	%  Sm   [4] NOT NORMAL SUBGROUP OF..DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
unicode_expands_on_nfd(0x2ADC, 0x2ADC).	%  Sm       FORKING
unicode_expands_on_nfd(0x304C, 0x304C).	%  Lo       HIRAGANA LETTER GA
unicode_expands_on_nfd(0x304E, 0x304E).	%  Lo       HIRAGANA LETTER GI
unicode_expands_on_nfd(0x3050, 0x3050).	%  Lo       HIRAGANA LETTER GU
unicode_expands_on_nfd(0x3052, 0x3052).	%  Lo       HIRAGANA LETTER GE
unicode_expands_on_nfd(0x3054, 0x3054).	%  Lo       HIRAGANA LETTER GO
unicode_expands_on_nfd(0x3056, 0x3056).	%  Lo       HIRAGANA LETTER ZA
unicode_expands_on_nfd(0x3058, 0x3058).	%  Lo       HIRAGANA LETTER ZI
unicode_expands_on_nfd(0x305A, 0x305A).	%  Lo       HIRAGANA LETTER ZU
unicode_expands_on_nfd(0x305C, 0x305C).	%  Lo       HIRAGANA LETTER ZE
unicode_expands_on_nfd(0x305E, 0x305E).	%  Lo       HIRAGANA LETTER ZO
unicode_expands_on_nfd(0x3060, 0x3060).	%  Lo       HIRAGANA LETTER DA
unicode_expands_on_nfd(0x3062, 0x3062).	%  Lo       HIRAGANA LETTER DI
unicode_expands_on_nfd(0x3065, 0x3065).	%  Lo       HIRAGANA LETTER DU
unicode_expands_on_nfd(0x3067, 0x3067).	%  Lo       HIRAGANA LETTER DE
unicode_expands_on_nfd(0x3069, 0x3069).	%  Lo       HIRAGANA LETTER DO
unicode_expands_on_nfd(0x3070, 0x3071).	%  Lo   [2] HIRAGANA LETTER BA..HIRAGANA LETTER PA
unicode_expands_on_nfd(0x3073, 0x3074).	%  Lo   [2] HIRAGANA LETTER BI..HIRAGANA LETTER PI
unicode_expands_on_nfd(0x3076, 0x3077).	%  Lo   [2] HIRAGANA LETTER BU..HIRAGANA LETTER PU
unicode_expands_on_nfd(0x3079, 0x307A).	%  Lo   [2] HIRAGANA LETTER BE..HIRAGANA LETTER PE
unicode_expands_on_nfd(0x307C, 0x307D).	%  Lo   [2] HIRAGANA LETTER BO..HIRAGANA LETTER PO
unicode_expands_on_nfd(0x3094, 0x3094).	%  Lo       HIRAGANA LETTER VU
unicode_expands_on_nfd(0x309E, 0x309E).	%  Lm       HIRAGANA VOICED ITERATION MARK
unicode_expands_on_nfd(0x30AC, 0x30AC).	%  Lo       KATAKANA LETTER GA
unicode_expands_on_nfd(0x30AE, 0x30AE).	%  Lo       KATAKANA LETTER GI
unicode_expands_on_nfd(0x30B0, 0x30B0).	%  Lo       KATAKANA LETTER GU
unicode_expands_on_nfd(0x30B2, 0x30B2).	%  Lo       KATAKANA LETTER GE
unicode_expands_on_nfd(0x30B4, 0x30B4).	%  Lo       KATAKANA LETTER GO
unicode_expands_on_nfd(0x30B6, 0x30B6).	%  Lo       KATAKANA LETTER ZA
unicode_expands_on_nfd(0x30B8, 0x30B8).	%  Lo       KATAKANA LETTER ZI
unicode_expands_on_nfd(0x30BA, 0x30BA).	%  Lo       KATAKANA LETTER ZU
unicode_expands_on_nfd(0x30BC, 0x30BC).	%  Lo       KATAKANA LETTER ZE
unicode_expands_on_nfd(0x30BE, 0x30BE).	%  Lo       KATAKANA LETTER ZO
unicode_expands_on_nfd(0x30C0, 0x30C0).	%  Lo       KATAKANA LETTER DA
unicode_expands_on_nfd(0x30C2, 0x30C2).	%  Lo       KATAKANA LETTER DI
unicode_expands_on_nfd(0x30C5, 0x30C5).	%  Lo       KATAKANA LETTER DU
unicode_expands_on_nfd(0x30C7, 0x30C7).	%  Lo       KATAKANA LETTER DE
unicode_expands_on_nfd(0x30C9, 0x30C9).	%  Lo       KATAKANA LETTER DO
unicode_expands_on_nfd(0x30D0, 0x30D1).	%  Lo   [2] KATAKANA LETTER BA..KATAKANA LETTER PA
unicode_expands_on_nfd(0x30D3, 0x30D4).	%  Lo   [2] KATAKANA LETTER BI..KATAKANA LETTER PI
unicode_expands_on_nfd(0x30D6, 0x30D7).	%  Lo   [2] KATAKANA LETTER BU..KATAKANA LETTER PU
unicode_expands_on_nfd(0x30D9, 0x30DA).	%  Lo   [2] KATAKANA LETTER BE..KATAKANA LETTER PE
unicode_expands_on_nfd(0x30DC, 0x30DD).	%  Lo   [2] KATAKANA LETTER BO..KATAKANA LETTER PO
unicode_expands_on_nfd(0x30F4, 0x30F4).	%  Lo       KATAKANA LETTER VU
unicode_expands_on_nfd(0x30F7, 0x30FA).	%  Lo   [4] KATAKANA LETTER VA..KATAKANA LETTER VO
unicode_expands_on_nfd(0x30FE, 0x30FE).	%  Lm       KATAKANA VOICED ITERATION MARK
unicode_expands_on_nfd(0xAC00, 0xD7A3).	%  Lo [11172] HANGUL SYLLABLE GA..HANGUL SYLLABLE HIH
unicode_expands_on_nfd(0xFB1D, 0xFB1D).	%  Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_expands_on_nfd(0xFB1F, 0xFB1F).	%  Lo       HEBREW LIGATURE YIDDISH YOD YOD PATAH
unicode_expands_on_nfd(0xFB2A, 0xFB36).	%  Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_expands_on_nfd(0xFB38, 0xFB3C).	%  Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_expands_on_nfd(0xFB3E, 0xFB3E).	%  Lo       HEBREW LETTER MEM WITH DAGESH
unicode_expands_on_nfd(0xFB40, 0xFB41).	%  Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_expands_on_nfd(0xFB43, 0xFB44).	%  Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_expands_on_nfd(0xFB46, 0xFB4E).	%  Lo   [9] HEBREW LETTER TSADI WITH DAGESH..HEBREW LETTER PE WITH RAFE
unicode_expands_on_nfd(0x1109A, 0x1109A).	% Lo       KAITHI LETTER DDDHA
unicode_expands_on_nfd(0x1109C, 0x1109C).	% Lo       KAITHI LETTER RHA
unicode_expands_on_nfd(0x110AB, 0x110AB).	% Lo       KAITHI LETTER VA
unicode_expands_on_nfd(0x1112E, 0x1112F).	% Mn   [2] CHAKMA VOWEL SIGN O..CHAKMA VOWEL SIGN AU
unicode_expands_on_nfd(0x1D15E, 0x1D164).	% So   [7] MUSICAL SYMBOL HALF NOTE..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
unicode_expands_on_nfd(0x1D1BB, 0x1D1C0).	% So   [6] MUSICAL SYMBOL MINIMA..MUSICAL SYMBOL FUSA BLACK

% Total code points: 12208
