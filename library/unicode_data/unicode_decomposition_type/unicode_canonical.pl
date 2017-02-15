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

unicode_canonical(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_canonical(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_canonical(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_canonical(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_canonical(0x00C0, 0x00C5).	% Canonical L&   [6] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER A WITH RING ABOVE
unicode_canonical(0x00C7, 0x00CF).	% Canonical L&   [9] LATIN CAPITAL LETTER C WITH CEDILLA..LATIN CAPITAL LETTER I WITH DIAERESIS
unicode_canonical(0x00D1, 0x00D6).	% Canonical L&   [6] LATIN CAPITAL LETTER N WITH TILDE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_canonical(0x00D9, 0x00DD).	% Canonical L&   [5] LATIN CAPITAL LETTER U WITH GRAVE..LATIN CAPITAL LETTER Y WITH ACUTE
unicode_canonical(0x00E0, 0x00E5).	% Canonical L&   [6] LATIN SMALL LETTER A WITH GRAVE..LATIN SMALL LETTER A WITH RING ABOVE
unicode_canonical(0x00E7, 0x00EF).	% Canonical L&   [9] LATIN SMALL LETTER C WITH CEDILLA..LATIN SMALL LETTER I WITH DIAERESIS
unicode_canonical(0x00F1, 0x00F6).	% Canonical L&   [6] LATIN SMALL LETTER N WITH TILDE..LATIN SMALL LETTER O WITH DIAERESIS
unicode_canonical(0x00F9, 0x00FD).	% Canonical L&   [5] LATIN SMALL LETTER U WITH GRAVE..LATIN SMALL LETTER Y WITH ACUTE
unicode_canonical(0x00FF, 0x010F).	% Canonical L&  [17] LATIN SMALL LETTER Y WITH DIAERESIS..LATIN SMALL LETTER D WITH CARON
unicode_canonical(0x0112, 0x0125).	% Canonical L&  [20] LATIN CAPITAL LETTER E WITH MACRON..LATIN SMALL LETTER H WITH CIRCUMFLEX
unicode_canonical(0x0128, 0x0130).	% Canonical L&   [9] LATIN CAPITAL LETTER I WITH TILDE..LATIN CAPITAL LETTER I WITH DOT ABOVE
unicode_canonical(0x0134, 0x0137).	% Canonical L&   [4] LATIN CAPITAL LETTER J WITH CIRCUMFLEX..LATIN SMALL LETTER K WITH CEDILLA
unicode_canonical(0x0139, 0x013E).	% Canonical L&   [6] LATIN CAPITAL LETTER L WITH ACUTE..LATIN SMALL LETTER L WITH CARON
unicode_canonical(0x0143, 0x0148).	% Canonical L&   [6] LATIN CAPITAL LETTER N WITH ACUTE..LATIN SMALL LETTER N WITH CARON
unicode_canonical(0x014C, 0x0151).	% Canonical L&   [6] LATIN CAPITAL LETTER O WITH MACRON..LATIN SMALL LETTER O WITH DOUBLE ACUTE
unicode_canonical(0x0154, 0x0165).	% Canonical L&  [18] LATIN CAPITAL LETTER R WITH ACUTE..LATIN SMALL LETTER T WITH CARON
unicode_canonical(0x0168, 0x017E).	% Canonical L&  [23] LATIN CAPITAL LETTER U WITH TILDE..LATIN SMALL LETTER Z WITH CARON
unicode_canonical(0x01A0, 0x01A1).	% Canonical L&   [2] LATIN CAPITAL LETTER O WITH HORN..LATIN SMALL LETTER O WITH HORN
unicode_canonical(0x01AF, 0x01B0).	% Canonical L&   [2] LATIN CAPITAL LETTER U WITH HORN..LATIN SMALL LETTER U WITH HORN
unicode_canonical(0x01CD, 0x01DC).	% Canonical L&  [16] LATIN CAPITAL LETTER A WITH CARON..LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
unicode_canonical(0x01DE, 0x01E3).	% Canonical L&   [6] LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON..LATIN SMALL LETTER AE WITH MACRON
unicode_canonical(0x01E6, 0x01F0).	% Canonical L&  [11] LATIN CAPITAL LETTER G WITH CARON..LATIN SMALL LETTER J WITH CARON
unicode_canonical(0x01F4, 0x01F5).	% Canonical L&   [2] LATIN CAPITAL LETTER G WITH ACUTE..LATIN SMALL LETTER G WITH ACUTE
unicode_canonical(0x01F8, 0x021B).	% Canonical L&  [36] LATIN CAPITAL LETTER N WITH GRAVE..LATIN SMALL LETTER T WITH COMMA BELOW
unicode_canonical(0x021E, 0x021F).	% Canonical L&   [2] LATIN CAPITAL LETTER H WITH CARON..LATIN SMALL LETTER H WITH CARON
unicode_canonical(0x0226, 0x0233).	% Canonical L&  [14] LATIN CAPITAL LETTER A WITH DOT ABOVE..LATIN SMALL LETTER Y WITH MACRON
unicode_canonical(0x0340, 0x0341).	% Canonical Mn   [2] COMBINING GRAVE TONE MARK..COMBINING ACUTE TONE MARK
unicode_canonical(0x0343, 0x0344).	% Canonical Mn   [2] COMBINING GREEK KORONIS..COMBINING GREEK DIALYTIKA TONOS
unicode_canonical(0x0374, 0x0374).	% Canonical Lm       GREEK NUMERAL SIGN
unicode_canonical(0x037E, 0x037E).	% Canonical Po       GREEK QUESTION MARK
unicode_canonical(0x0385, 0x0385).	% Canonical Sk       GREEK DIALYTIKA TONOS
unicode_canonical(0x0386, 0x0386).	% Canonical L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_canonical(0x0387, 0x0387).	% Canonical Po       GREEK ANO TELEIA
unicode_canonical(0x0388, 0x038A).	% Canonical L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_canonical(0x038C, 0x038C).	% Canonical L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_canonical(0x038E, 0x0390).	% Canonical L&   [3] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
unicode_canonical(0x03AA, 0x03B0).	% Canonical L&   [7] GREEK CAPITAL LETTER IOTA WITH DIALYTIKA..GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
unicode_canonical(0x03CA, 0x03CE).	% Canonical L&   [5] GREEK SMALL LETTER IOTA WITH DIALYTIKA..GREEK SMALL LETTER OMEGA WITH TONOS
unicode_canonical(0x03D3, 0x03D4).	% Canonical L&   [2] GREEK UPSILON WITH ACUTE AND HOOK SYMBOL..GREEK UPSILON WITH DIAERESIS AND HOOK SYMBOL
unicode_canonical(0x0400, 0x0401).	% Canonical L&   [2] CYRILLIC CAPITAL LETTER IE WITH GRAVE..CYRILLIC CAPITAL LETTER IO
unicode_canonical(0x0403, 0x0403).	% Canonical L&       CYRILLIC CAPITAL LETTER GJE
unicode_canonical(0x0407, 0x0407).	% Canonical L&       CYRILLIC CAPITAL LETTER YI
unicode_canonical(0x040C, 0x040E).	% Canonical L&   [3] CYRILLIC CAPITAL LETTER KJE..CYRILLIC CAPITAL LETTER SHORT U
unicode_canonical(0x0419, 0x0419).	% Canonical L&       CYRILLIC CAPITAL LETTER SHORT I
unicode_canonical(0x0439, 0x0439).	% Canonical L&       CYRILLIC SMALL LETTER SHORT I
unicode_canonical(0x0450, 0x0451).	% Canonical L&   [2] CYRILLIC SMALL LETTER IE WITH GRAVE..CYRILLIC SMALL LETTER IO
unicode_canonical(0x0453, 0x0453).	% Canonical L&       CYRILLIC SMALL LETTER GJE
unicode_canonical(0x0457, 0x0457).	% Canonical L&       CYRILLIC SMALL LETTER YI
unicode_canonical(0x045C, 0x045E).	% Canonical L&   [3] CYRILLIC SMALL LETTER KJE..CYRILLIC SMALL LETTER SHORT U
unicode_canonical(0x0476, 0x0477).	% Canonical L&   [2] CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT..CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
unicode_canonical(0x04C1, 0x04C2).	% Canonical L&   [2] CYRILLIC CAPITAL LETTER ZHE WITH BREVE..CYRILLIC SMALL LETTER ZHE WITH BREVE
unicode_canonical(0x04D0, 0x04D3).	% Canonical L&   [4] CYRILLIC CAPITAL LETTER A WITH BREVE..CYRILLIC SMALL LETTER A WITH DIAERESIS
unicode_canonical(0x04D6, 0x04D7).	% Canonical L&   [2] CYRILLIC CAPITAL LETTER IE WITH BREVE..CYRILLIC SMALL LETTER IE WITH BREVE
unicode_canonical(0x04DA, 0x04DF).	% Canonical L&   [6] CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS..CYRILLIC SMALL LETTER ZE WITH DIAERESIS
unicode_canonical(0x04E2, 0x04E7).	% Canonical L&   [6] CYRILLIC CAPITAL LETTER I WITH MACRON..CYRILLIC SMALL LETTER O WITH DIAERESIS
unicode_canonical(0x04EA, 0x04F5).	% Canonical L&  [12] CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS..CYRILLIC SMALL LETTER CHE WITH DIAERESIS
unicode_canonical(0x04F8, 0x04F9).	% Canonical L&   [2] CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS..CYRILLIC SMALL LETTER YERU WITH DIAERESIS
unicode_canonical(0x0622, 0x0626).	% Canonical Lo   [5] ARABIC LETTER ALEF WITH MADDA ABOVE..ARABIC LETTER YEH WITH HAMZA ABOVE
unicode_canonical(0x06C0, 0x06C0).	% Canonical Lo       ARABIC LETTER HEH WITH YEH ABOVE
unicode_canonical(0x06C2, 0x06C2).	% Canonical Lo       ARABIC LETTER HEH GOAL WITH HAMZA ABOVE
unicode_canonical(0x06D3, 0x06D3).	% Canonical Lo       ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
unicode_canonical(0x0929, 0x0929).	% Canonical Lo       DEVANAGARI LETTER NNNA
unicode_canonical(0x0931, 0x0931).	% Canonical Lo       DEVANAGARI LETTER RRA
unicode_canonical(0x0934, 0x0934).	% Canonical Lo       DEVANAGARI LETTER LLLA
unicode_canonical(0x0958, 0x095F).	% Canonical Lo   [8] DEVANAGARI LETTER QA..DEVANAGARI LETTER YYA
unicode_canonical(0x09CB, 0x09CC).	% Canonical Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_canonical(0x09DC, 0x09DD).	% Canonical Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_canonical(0x09DF, 0x09DF).	% Canonical Lo       BENGALI LETTER YYA
unicode_canonical(0x0A33, 0x0A33).	% Canonical Lo       GURMUKHI LETTER LLA
unicode_canonical(0x0A36, 0x0A36).	% Canonical Lo       GURMUKHI LETTER SHA
unicode_canonical(0x0A59, 0x0A5B).	% Canonical Lo   [3] GURMUKHI LETTER KHHA..GURMUKHI LETTER ZA
unicode_canonical(0x0A5E, 0x0A5E).	% Canonical Lo       GURMUKHI LETTER FA
unicode_canonical(0x0B48, 0x0B48).	% Canonical Mc       ORIYA VOWEL SIGN AI
unicode_canonical(0x0B4B, 0x0B4C).	% Canonical Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
unicode_canonical(0x0B5C, 0x0B5D).	% Canonical Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_canonical(0x0B94, 0x0B94).	% Canonical Lo       TAMIL LETTER AU
unicode_canonical(0x0BCA, 0x0BCC).	% Canonical Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_canonical(0x0C48, 0x0C48).	% Canonical Mn       TELUGU VOWEL SIGN AI
unicode_canonical(0x0CC0, 0x0CC0).	% Canonical Mc       KANNADA VOWEL SIGN II
unicode_canonical(0x0CC7, 0x0CC8).	% Canonical Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_canonical(0x0CCA, 0x0CCB).	% Canonical Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_canonical(0x0D4A, 0x0D4C).	% Canonical Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_canonical(0x0DDA, 0x0DDA).	% Canonical Mc       SINHALA VOWEL SIGN DIGA KOMBUVA
unicode_canonical(0x0DDC, 0x0DDE).	% Canonical Mc   [3] SINHALA VOWEL SIGN KOMBUVA HAA AELA-PILLA..SINHALA VOWEL SIGN KOMBUVA HAA GAYANUKITTA
unicode_canonical(0x0F43, 0x0F43).	% Canonical Lo       TIBETAN LETTER GHA
unicode_canonical(0x0F4D, 0x0F4D).	% Canonical Lo       TIBETAN LETTER DDHA
unicode_canonical(0x0F52, 0x0F52).	% Canonical Lo       TIBETAN LETTER DHA
unicode_canonical(0x0F57, 0x0F57).	% Canonical Lo       TIBETAN LETTER BHA
unicode_canonical(0x0F5C, 0x0F5C).	% Canonical Lo       TIBETAN LETTER DZHA
unicode_canonical(0x0F69, 0x0F69).	% Canonical Lo       TIBETAN LETTER KSSA
unicode_canonical(0x0F73, 0x0F73).	% Canonical Mn       TIBETAN VOWEL SIGN II
unicode_canonical(0x0F75, 0x0F76).	% Canonical Mn   [2] TIBETAN VOWEL SIGN UU..TIBETAN VOWEL SIGN VOCALIC R
unicode_canonical(0x0F78, 0x0F78).	% Canonical Mn       TIBETAN VOWEL SIGN VOCALIC L
unicode_canonical(0x0F81, 0x0F81).	% Canonical Mn       TIBETAN VOWEL SIGN REVERSED II
unicode_canonical(0x0F93, 0x0F93).	% Canonical Mn       TIBETAN SUBJOINED LETTER GHA
unicode_canonical(0x0F9D, 0x0F9D).	% Canonical Mn       TIBETAN SUBJOINED LETTER DDHA
unicode_canonical(0x0FA2, 0x0FA2).	% Canonical Mn       TIBETAN SUBJOINED LETTER DHA
unicode_canonical(0x0FA7, 0x0FA7).	% Canonical Mn       TIBETAN SUBJOINED LETTER BHA
unicode_canonical(0x0FAC, 0x0FAC).	% Canonical Mn       TIBETAN SUBJOINED LETTER DZHA
unicode_canonical(0x0FB9, 0x0FB9).	% Canonical Mn       TIBETAN SUBJOINED LETTER KSSA
unicode_canonical(0x1026, 0x1026).	% Canonical Lo       MYANMAR LETTER UU
unicode_canonical(0x1B06, 0x1B06).	% Canonical Lo       BALINESE LETTER AKARA TEDUNG
unicode_canonical(0x1B08, 0x1B08).	% Canonical Lo       BALINESE LETTER IKARA TEDUNG
unicode_canonical(0x1B0A, 0x1B0A).	% Canonical Lo       BALINESE LETTER UKARA TEDUNG
unicode_canonical(0x1B0C, 0x1B0C).	% Canonical Lo       BALINESE LETTER RA REPA TEDUNG
unicode_canonical(0x1B0E, 0x1B0E).	% Canonical Lo       BALINESE LETTER LA LENGA TEDUNG
unicode_canonical(0x1B12, 0x1B12).	% Canonical Lo       BALINESE LETTER OKARA TEDUNG
unicode_canonical(0x1B3B, 0x1B3B).	% Canonical Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_canonical(0x1B3D, 0x1B3D).	% Canonical Mc       BALINESE VOWEL SIGN LA LENGA TEDUNG
unicode_canonical(0x1B40, 0x1B41).	% Canonical Mc   [2] BALINESE VOWEL SIGN TALING TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG
unicode_canonical(0x1B43, 0x1B43).	% Canonical Mc       BALINESE VOWEL SIGN PEPET TEDUNG
unicode_canonical(0x1E00, 0x1E99).	% Canonical L& [154] LATIN CAPITAL LETTER A WITH RING BELOW..LATIN SMALL LETTER Y WITH RING ABOVE
unicode_canonical(0x1E9B, 0x1E9B).	% Canonical L&       LATIN SMALL LETTER LONG S WITH DOT ABOVE
unicode_canonical(0x1EA0, 0x1EF9).	% Canonical L&  [90] LATIN CAPITAL LETTER A WITH DOT BELOW..LATIN SMALL LETTER Y WITH TILDE
unicode_canonical(0x1F00, 0x1F15).	% Canonical L&  [22] GREEK SMALL LETTER ALPHA WITH PSILI..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_canonical(0x1F18, 0x1F1D).	% Canonical L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_canonical(0x1F20, 0x1F45).	% Canonical L&  [38] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_canonical(0x1F48, 0x1F4D).	% Canonical L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_canonical(0x1F50, 0x1F57).	% Canonical L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_canonical(0x1F59, 0x1F59).	% Canonical L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_canonical(0x1F5B, 0x1F5B).	% Canonical L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_canonical(0x1F5D, 0x1F5D).	% Canonical L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_canonical(0x1F5F, 0x1F7D).	% Canonical L&  [31] GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_canonical(0x1F80, 0x1FB4).	% Canonical L&  [53] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_canonical(0x1FB6, 0x1FBC).	% Canonical L&   [7] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_canonical(0x1FBE, 0x1FBE).	% Canonical L&       GREEK PROSGEGRAMMENI
unicode_canonical(0x1FC1, 0x1FC1).	% Canonical Sk       GREEK DIALYTIKA AND PERISPOMENI
unicode_canonical(0x1FC2, 0x1FC4).	% Canonical L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_canonical(0x1FC6, 0x1FCC).	% Canonical L&   [7] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_canonical(0x1FCD, 0x1FCF).	% Canonical Sk   [3] GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI
unicode_canonical(0x1FD0, 0x1FD3).	% Canonical L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_canonical(0x1FD6, 0x1FDB).	% Canonical L&   [6] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_canonical(0x1FDD, 0x1FDF).	% Canonical Sk   [3] GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI
unicode_canonical(0x1FE0, 0x1FEC).	% Canonical L&  [13] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_canonical(0x1FED, 0x1FEF).	% Canonical Sk   [3] GREEK DIALYTIKA AND VARIA..GREEK VARIA
unicode_canonical(0x1FF2, 0x1FF4).	% Canonical L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_canonical(0x1FF6, 0x1FFC).	% Canonical L&   [7] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_canonical(0x1FFD, 0x1FFD).	% Canonical Sk       GREEK OXIA
unicode_canonical(0x2000, 0x2001).	% Canonical Zs   [2] EN QUAD..EM QUAD
unicode_canonical(0x2126, 0x2126).	% Canonical L&       OHM SIGN
unicode_canonical(0x212A, 0x212B).	% Canonical L&   [2] KELVIN SIGN..ANGSTROM SIGN
unicode_canonical(0x219A, 0x219B).	% Canonical Sm   [2] LEFTWARDS ARROW WITH STROKE..RIGHTWARDS ARROW WITH STROKE
unicode_canonical(0x21AE, 0x21AE).	% Canonical Sm       LEFT RIGHT ARROW WITH STROKE
unicode_canonical(0x21CD, 0x21CD).	% Canonical So       LEFTWARDS DOUBLE ARROW WITH STROKE
unicode_canonical(0x21CE, 0x21CF).	% Canonical Sm   [2] LEFT RIGHT DOUBLE ARROW WITH STROKE..RIGHTWARDS DOUBLE ARROW WITH STROKE
unicode_canonical(0x2204, 0x2204).	% Canonical Sm       THERE DOES NOT EXIST
unicode_canonical(0x2209, 0x2209).	% Canonical Sm       NOT AN ELEMENT OF
unicode_canonical(0x220C, 0x220C).	% Canonical Sm       DOES NOT CONTAIN AS MEMBER
unicode_canonical(0x2224, 0x2224).	% Canonical Sm       DOES NOT DIVIDE
unicode_canonical(0x2226, 0x2226).	% Canonical Sm       NOT PARALLEL TO
unicode_canonical(0x2241, 0x2241).	% Canonical Sm       NOT TILDE
unicode_canonical(0x2244, 0x2244).	% Canonical Sm       NOT ASYMPTOTICALLY EQUAL TO
unicode_canonical(0x2247, 0x2247).	% Canonical Sm       NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
unicode_canonical(0x2249, 0x2249).	% Canonical Sm       NOT ALMOST EQUAL TO
unicode_canonical(0x2260, 0x2260).	% Canonical Sm       NOT EQUAL TO
unicode_canonical(0x2262, 0x2262).	% Canonical Sm       NOT IDENTICAL TO
unicode_canonical(0x226D, 0x2271).	% Canonical Sm   [5] NOT EQUIVALENT TO..NEITHER GREATER-THAN NOR EQUAL TO
unicode_canonical(0x2274, 0x2275).	% Canonical Sm   [2] NEITHER LESS-THAN NOR EQUIVALENT TO..NEITHER GREATER-THAN NOR EQUIVALENT TO
unicode_canonical(0x2278, 0x2279).	% Canonical Sm   [2] NEITHER LESS-THAN NOR GREATER-THAN..NEITHER GREATER-THAN NOR LESS-THAN
unicode_canonical(0x2280, 0x2281).	% Canonical Sm   [2] DOES NOT PRECEDE..DOES NOT SUCCEED
unicode_canonical(0x2284, 0x2285).	% Canonical Sm   [2] NOT A SUBSET OF..NOT A SUPERSET OF
unicode_canonical(0x2288, 0x2289).	% Canonical Sm   [2] NEITHER A SUBSET OF NOR EQUAL TO..NEITHER A SUPERSET OF NOR EQUAL TO
unicode_canonical(0x22AC, 0x22AF).	% Canonical Sm   [4] DOES NOT PROVE..NEGATED DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
unicode_canonical(0x22E0, 0x22E3).	% Canonical Sm   [4] DOES NOT PRECEDE OR EQUAL..NOT SQUARE ORIGINAL OF OR EQUAL TO
unicode_canonical(0x22EA, 0x22ED).	% Canonical Sm   [4] NOT NORMAL SUBGROUP OF..DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
unicode_canonical(0x2329, 0x2329).	% Canonical Ps       LEFT-POINTING ANGLE BRACKET
unicode_canonical(0x232A, 0x232A).	% Canonical Pe       RIGHT-POINTING ANGLE BRACKET
unicode_canonical(0x2ADC, 0x2ADC).	% Canonical Sm       FORKING
unicode_canonical(0x304C, 0x304C).	% Canonical Lo       HIRAGANA LETTER GA
unicode_canonical(0x304E, 0x304E).	% Canonical Lo       HIRAGANA LETTER GI
unicode_canonical(0x3050, 0x3050).	% Canonical Lo       HIRAGANA LETTER GU
unicode_canonical(0x3052, 0x3052).	% Canonical Lo       HIRAGANA LETTER GE
unicode_canonical(0x3054, 0x3054).	% Canonical Lo       HIRAGANA LETTER GO
unicode_canonical(0x3056, 0x3056).	% Canonical Lo       HIRAGANA LETTER ZA
unicode_canonical(0x3058, 0x3058).	% Canonical Lo       HIRAGANA LETTER ZI
unicode_canonical(0x305A, 0x305A).	% Canonical Lo       HIRAGANA LETTER ZU
unicode_canonical(0x305C, 0x305C).	% Canonical Lo       HIRAGANA LETTER ZE
unicode_canonical(0x305E, 0x305E).	% Canonical Lo       HIRAGANA LETTER ZO
unicode_canonical(0x3060, 0x3060).	% Canonical Lo       HIRAGANA LETTER DA
unicode_canonical(0x3062, 0x3062).	% Canonical Lo       HIRAGANA LETTER DI
unicode_canonical(0x3065, 0x3065).	% Canonical Lo       HIRAGANA LETTER DU
unicode_canonical(0x3067, 0x3067).	% Canonical Lo       HIRAGANA LETTER DE
unicode_canonical(0x3069, 0x3069).	% Canonical Lo       HIRAGANA LETTER DO
unicode_canonical(0x3070, 0x3071).	% Canonical Lo   [2] HIRAGANA LETTER BA..HIRAGANA LETTER PA
unicode_canonical(0x3073, 0x3074).	% Canonical Lo   [2] HIRAGANA LETTER BI..HIRAGANA LETTER PI
unicode_canonical(0x3076, 0x3077).	% Canonical Lo   [2] HIRAGANA LETTER BU..HIRAGANA LETTER PU
unicode_canonical(0x3079, 0x307A).	% Canonical Lo   [2] HIRAGANA LETTER BE..HIRAGANA LETTER PE
unicode_canonical(0x307C, 0x307D).	% Canonical Lo   [2] HIRAGANA LETTER BO..HIRAGANA LETTER PO
unicode_canonical(0x3094, 0x3094).	% Canonical Lo       HIRAGANA LETTER VU
unicode_canonical(0x309E, 0x309E).	% Canonical Lm       HIRAGANA VOICED ITERATION MARK
unicode_canonical(0x30AC, 0x30AC).	% Canonical Lo       KATAKANA LETTER GA
unicode_canonical(0x30AE, 0x30AE).	% Canonical Lo       KATAKANA LETTER GI
unicode_canonical(0x30B0, 0x30B0).	% Canonical Lo       KATAKANA LETTER GU
unicode_canonical(0x30B2, 0x30B2).	% Canonical Lo       KATAKANA LETTER GE
unicode_canonical(0x30B4, 0x30B4).	% Canonical Lo       KATAKANA LETTER GO
unicode_canonical(0x30B6, 0x30B6).	% Canonical Lo       KATAKANA LETTER ZA
unicode_canonical(0x30B8, 0x30B8).	% Canonical Lo       KATAKANA LETTER ZI
unicode_canonical(0x30BA, 0x30BA).	% Canonical Lo       KATAKANA LETTER ZU
unicode_canonical(0x30BC, 0x30BC).	% Canonical Lo       KATAKANA LETTER ZE
unicode_canonical(0x30BE, 0x30BE).	% Canonical Lo       KATAKANA LETTER ZO
unicode_canonical(0x30C0, 0x30C0).	% Canonical Lo       KATAKANA LETTER DA
unicode_canonical(0x30C2, 0x30C2).	% Canonical Lo       KATAKANA LETTER DI
unicode_canonical(0x30C5, 0x30C5).	% Canonical Lo       KATAKANA LETTER DU
unicode_canonical(0x30C7, 0x30C7).	% Canonical Lo       KATAKANA LETTER DE
unicode_canonical(0x30C9, 0x30C9).	% Canonical Lo       KATAKANA LETTER DO
unicode_canonical(0x30D0, 0x30D1).	% Canonical Lo   [2] KATAKANA LETTER BA..KATAKANA LETTER PA
unicode_canonical(0x30D3, 0x30D4).	% Canonical Lo   [2] KATAKANA LETTER BI..KATAKANA LETTER PI
unicode_canonical(0x30D6, 0x30D7).	% Canonical Lo   [2] KATAKANA LETTER BU..KATAKANA LETTER PU
unicode_canonical(0x30D9, 0x30DA).	% Canonical Lo   [2] KATAKANA LETTER BE..KATAKANA LETTER PE
unicode_canonical(0x30DC, 0x30DD).	% Canonical Lo   [2] KATAKANA LETTER BO..KATAKANA LETTER PO
unicode_canonical(0x30F4, 0x30F4).	% Canonical Lo       KATAKANA LETTER VU
unicode_canonical(0x30F7, 0x30FA).	% Canonical Lo   [4] KATAKANA LETTER VA..KATAKANA LETTER VO
unicode_canonical(0x30FE, 0x30FE).	% Canonical Lm       KATAKANA VOICED ITERATION MARK
unicode_canonical(0xAC00, 0xD7A3).	% Canonical Lo [11172] HANGUL SYLLABLE GA..HANGUL SYLLABLE HIH
unicode_canonical(0xF900, 0xFA0D).	% Canonical Lo [270] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA0D
unicode_canonical(0xFA10, 0xFA10).	% Canonical Lo       CJK COMPATIBILITY IDEOGRAPH-FA10
unicode_canonical(0xFA12, 0xFA12).	% Canonical Lo       CJK COMPATIBILITY IDEOGRAPH-FA12
unicode_canonical(0xFA15, 0xFA1E).	% Canonical Lo  [10] CJK COMPATIBILITY IDEOGRAPH-FA15..CJK COMPATIBILITY IDEOGRAPH-FA1E
unicode_canonical(0xFA20, 0xFA20).	% Canonical Lo       CJK COMPATIBILITY IDEOGRAPH-FA20
unicode_canonical(0xFA22, 0xFA22).	% Canonical Lo       CJK COMPATIBILITY IDEOGRAPH-FA22
unicode_canonical(0xFA25, 0xFA26).	% Canonical Lo   [2] CJK COMPATIBILITY IDEOGRAPH-FA25..CJK COMPATIBILITY IDEOGRAPH-FA26
unicode_canonical(0xFA2A, 0xFA6D).	% Canonical Lo  [68] CJK COMPATIBILITY IDEOGRAPH-FA2A..CJK COMPATIBILITY IDEOGRAPH-FA6D
unicode_canonical(0xFA70, 0xFAD9).	% Canonical Lo [106] CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9
unicode_canonical(0xFB1D, 0xFB1D).	% Canonical Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_canonical(0xFB1F, 0xFB1F).	% Canonical Lo       HEBREW LIGATURE YIDDISH YOD YOD PATAH
unicode_canonical(0xFB2A, 0xFB36).	% Canonical Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_canonical(0xFB38, 0xFB3C).	% Canonical Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_canonical(0xFB3E, 0xFB3E).	% Canonical Lo       HEBREW LETTER MEM WITH DAGESH
unicode_canonical(0xFB40, 0xFB41).	% Canonical Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_canonical(0xFB43, 0xFB44).	% Canonical Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_canonical(0xFB46, 0xFB4E).	% Canonical Lo   [9] HEBREW LETTER TSADI WITH DAGESH..HEBREW LETTER PE WITH RAFE
unicode_canonical(0x1109A, 0x1109A).	% Canonical Lo       KAITHI LETTER DDDHA
unicode_canonical(0x1109C, 0x1109C).	% Canonical Lo       KAITHI LETTER RHA
unicode_canonical(0x110AB, 0x110AB).	% Canonical Lo       KAITHI LETTER VA
unicode_canonical(0x1112E, 0x1112F).	% Canonical Mn   [2] CHAKMA VOWEL SIGN O..CHAKMA VOWEL SIGN AU
unicode_canonical(0x1D15E, 0x1D164).	% Canonical So   [7] MUSICAL SYMBOL HALF NOTE..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
unicode_canonical(0x1D1BB, 0x1D1C0).	% Canonical So   [6] MUSICAL SYMBOL MINIMA..MUSICAL SYMBOL FUSA BLACK
unicode_canonical(0x2F800, 0x2FA1D).	% Canonical Lo [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D

% Total code points: 13225
