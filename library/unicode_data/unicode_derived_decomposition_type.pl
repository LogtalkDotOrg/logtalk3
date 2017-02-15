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

unicode_decomposition_type(CodePoint, Type) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_decomposition_type(CodePointStart, CodePointEnd, Type),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_decomposition_type(CodePoint, _, CodePointType) ->
		Type = CodePointType
	;	% look for a code point range that includes the given code point
		unicode_decomposition_type(CodePointStart, CodePointEnd, CodePointType),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Type = CodePointType
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Type = 'None'
	).

% ================================================

unicode_decomposition_type(0x00C0, 0x00C5, 'Canonical').	% L&   [6] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER A WITH RING ABOVE
unicode_decomposition_type(0x00C7, 0x00CF, 'Canonical').	% L&   [9] LATIN CAPITAL LETTER C WITH CEDILLA..LATIN CAPITAL LETTER I WITH DIAERESIS
unicode_decomposition_type(0x00D1, 0x00D6, 'Canonical').	% L&   [6] LATIN CAPITAL LETTER N WITH TILDE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_decomposition_type(0x00D9, 0x00DD, 'Canonical').	% L&   [5] LATIN CAPITAL LETTER U WITH GRAVE..LATIN CAPITAL LETTER Y WITH ACUTE
unicode_decomposition_type(0x00E0, 0x00E5, 'Canonical').	% L&   [6] LATIN SMALL LETTER A WITH GRAVE..LATIN SMALL LETTER A WITH RING ABOVE
unicode_decomposition_type(0x00E7, 0x00EF, 'Canonical').	% L&   [9] LATIN SMALL LETTER C WITH CEDILLA..LATIN SMALL LETTER I WITH DIAERESIS
unicode_decomposition_type(0x00F1, 0x00F6, 'Canonical').	% L&   [6] LATIN SMALL LETTER N WITH TILDE..LATIN SMALL LETTER O WITH DIAERESIS
unicode_decomposition_type(0x00F9, 0x00FD, 'Canonical').	% L&   [5] LATIN SMALL LETTER U WITH GRAVE..LATIN SMALL LETTER Y WITH ACUTE
unicode_decomposition_type(0x00FF, 0x010F, 'Canonical').	% L&  [17] LATIN SMALL LETTER Y WITH DIAERESIS..LATIN SMALL LETTER D WITH CARON
unicode_decomposition_type(0x0112, 0x0125, 'Canonical').	% L&  [20] LATIN CAPITAL LETTER E WITH MACRON..LATIN SMALL LETTER H WITH CIRCUMFLEX
unicode_decomposition_type(0x0128, 0x0130, 'Canonical').	% L&   [9] LATIN CAPITAL LETTER I WITH TILDE..LATIN CAPITAL LETTER I WITH DOT ABOVE
unicode_decomposition_type(0x0134, 0x0137, 'Canonical').	% L&   [4] LATIN CAPITAL LETTER J WITH CIRCUMFLEX..LATIN SMALL LETTER K WITH CEDILLA
unicode_decomposition_type(0x0139, 0x013E, 'Canonical').	% L&   [6] LATIN CAPITAL LETTER L WITH ACUTE..LATIN SMALL LETTER L WITH CARON
unicode_decomposition_type(0x0143, 0x0148, 'Canonical').	% L&   [6] LATIN CAPITAL LETTER N WITH ACUTE..LATIN SMALL LETTER N WITH CARON
unicode_decomposition_type(0x014C, 0x0151, 'Canonical').	% L&   [6] LATIN CAPITAL LETTER O WITH MACRON..LATIN SMALL LETTER O WITH DOUBLE ACUTE
unicode_decomposition_type(0x0154, 0x0165, 'Canonical').	% L&  [18] LATIN CAPITAL LETTER R WITH ACUTE..LATIN SMALL LETTER T WITH CARON
unicode_decomposition_type(0x0168, 0x017E, 'Canonical').	% L&  [23] LATIN CAPITAL LETTER U WITH TILDE..LATIN SMALL LETTER Z WITH CARON
unicode_decomposition_type(0x01A0, 0x01A1, 'Canonical').	% L&   [2] LATIN CAPITAL LETTER O WITH HORN..LATIN SMALL LETTER O WITH HORN
unicode_decomposition_type(0x01AF, 0x01B0, 'Canonical').	% L&   [2] LATIN CAPITAL LETTER U WITH HORN..LATIN SMALL LETTER U WITH HORN
unicode_decomposition_type(0x01CD, 0x01DC, 'Canonical').	% L&  [16] LATIN CAPITAL LETTER A WITH CARON..LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
unicode_decomposition_type(0x01DE, 0x01E3, 'Canonical').	% L&   [6] LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON..LATIN SMALL LETTER AE WITH MACRON
unicode_decomposition_type(0x01E6, 0x01F0, 'Canonical').	% L&  [11] LATIN CAPITAL LETTER G WITH CARON..LATIN SMALL LETTER J WITH CARON
unicode_decomposition_type(0x01F4, 0x01F5, 'Canonical').	% L&   [2] LATIN CAPITAL LETTER G WITH ACUTE..LATIN SMALL LETTER G WITH ACUTE
unicode_decomposition_type(0x01F8, 0x021B, 'Canonical').	% L&  [36] LATIN CAPITAL LETTER N WITH GRAVE..LATIN SMALL LETTER T WITH COMMA BELOW
unicode_decomposition_type(0x021E, 0x021F, 'Canonical').	% L&   [2] LATIN CAPITAL LETTER H WITH CARON..LATIN SMALL LETTER H WITH CARON
unicode_decomposition_type(0x0226, 0x0233, 'Canonical').	% L&  [14] LATIN CAPITAL LETTER A WITH DOT ABOVE..LATIN SMALL LETTER Y WITH MACRON
unicode_decomposition_type(0x0340, 0x0341, 'Canonical').	% Mn   [2] COMBINING GRAVE TONE MARK..COMBINING ACUTE TONE MARK
unicode_decomposition_type(0x0343, 0x0344, 'Canonical').	% Mn   [2] COMBINING GREEK KORONIS..COMBINING GREEK DIALYTIKA TONOS
unicode_decomposition_type(0x0374, 0x0374, 'Canonical').	% Lm       GREEK NUMERAL SIGN
unicode_decomposition_type(0x037E, 0x037E, 'Canonical').	% Po       GREEK QUESTION MARK
unicode_decomposition_type(0x0385, 0x0385, 'Canonical').	% Sk       GREEK DIALYTIKA TONOS
unicode_decomposition_type(0x0386, 0x0386, 'Canonical').	% L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_decomposition_type(0x0387, 0x0387, 'Canonical').	% Po       GREEK ANO TELEIA
unicode_decomposition_type(0x0388, 0x038A, 'Canonical').	% L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_decomposition_type(0x038C, 0x038C, 'Canonical').	% L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_decomposition_type(0x038E, 0x0390, 'Canonical').	% L&   [3] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
unicode_decomposition_type(0x03AA, 0x03B0, 'Canonical').	% L&   [7] GREEK CAPITAL LETTER IOTA WITH DIALYTIKA..GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
unicode_decomposition_type(0x03CA, 0x03CE, 'Canonical').	% L&   [5] GREEK SMALL LETTER IOTA WITH DIALYTIKA..GREEK SMALL LETTER OMEGA WITH TONOS
unicode_decomposition_type(0x03D3, 0x03D4, 'Canonical').	% L&   [2] GREEK UPSILON WITH ACUTE AND HOOK SYMBOL..GREEK UPSILON WITH DIAERESIS AND HOOK SYMBOL
unicode_decomposition_type(0x0400, 0x0401, 'Canonical').	% L&   [2] CYRILLIC CAPITAL LETTER IE WITH GRAVE..CYRILLIC CAPITAL LETTER IO
unicode_decomposition_type(0x0403, 0x0403, 'Canonical').	% L&       CYRILLIC CAPITAL LETTER GJE
unicode_decomposition_type(0x0407, 0x0407, 'Canonical').	% L&       CYRILLIC CAPITAL LETTER YI
unicode_decomposition_type(0x040C, 0x040E, 'Canonical').	% L&   [3] CYRILLIC CAPITAL LETTER KJE..CYRILLIC CAPITAL LETTER SHORT U
unicode_decomposition_type(0x0419, 0x0419, 'Canonical').	% L&       CYRILLIC CAPITAL LETTER SHORT I
unicode_decomposition_type(0x0439, 0x0439, 'Canonical').	% L&       CYRILLIC SMALL LETTER SHORT I
unicode_decomposition_type(0x0450, 0x0451, 'Canonical').	% L&   [2] CYRILLIC SMALL LETTER IE WITH GRAVE..CYRILLIC SMALL LETTER IO
unicode_decomposition_type(0x0453, 0x0453, 'Canonical').	% L&       CYRILLIC SMALL LETTER GJE
unicode_decomposition_type(0x0457, 0x0457, 'Canonical').	% L&       CYRILLIC SMALL LETTER YI
unicode_decomposition_type(0x045C, 0x045E, 'Canonical').	% L&   [3] CYRILLIC SMALL LETTER KJE..CYRILLIC SMALL LETTER SHORT U
unicode_decomposition_type(0x0476, 0x0477, 'Canonical').	% L&   [2] CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT..CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
unicode_decomposition_type(0x04C1, 0x04C2, 'Canonical').	% L&   [2] CYRILLIC CAPITAL LETTER ZHE WITH BREVE..CYRILLIC SMALL LETTER ZHE WITH BREVE
unicode_decomposition_type(0x04D0, 0x04D3, 'Canonical').	% L&   [4] CYRILLIC CAPITAL LETTER A WITH BREVE..CYRILLIC SMALL LETTER A WITH DIAERESIS
unicode_decomposition_type(0x04D6, 0x04D7, 'Canonical').	% L&   [2] CYRILLIC CAPITAL LETTER IE WITH BREVE..CYRILLIC SMALL LETTER IE WITH BREVE
unicode_decomposition_type(0x04DA, 0x04DF, 'Canonical').	% L&   [6] CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS..CYRILLIC SMALL LETTER ZE WITH DIAERESIS
unicode_decomposition_type(0x04E2, 0x04E7, 'Canonical').	% L&   [6] CYRILLIC CAPITAL LETTER I WITH MACRON..CYRILLIC SMALL LETTER O WITH DIAERESIS
unicode_decomposition_type(0x04EA, 0x04F5, 'Canonical').	% L&  [12] CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS..CYRILLIC SMALL LETTER CHE WITH DIAERESIS
unicode_decomposition_type(0x04F8, 0x04F9, 'Canonical').	% L&   [2] CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS..CYRILLIC SMALL LETTER YERU WITH DIAERESIS
unicode_decomposition_type(0x0622, 0x0626, 'Canonical').	% Lo   [5] ARABIC LETTER ALEF WITH MADDA ABOVE..ARABIC LETTER YEH WITH HAMZA ABOVE
unicode_decomposition_type(0x06C0, 0x06C0, 'Canonical').	% Lo       ARABIC LETTER HEH WITH YEH ABOVE
unicode_decomposition_type(0x06C2, 0x06C2, 'Canonical').	% Lo       ARABIC LETTER HEH GOAL WITH HAMZA ABOVE
unicode_decomposition_type(0x06D3, 0x06D3, 'Canonical').	% Lo       ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
unicode_decomposition_type(0x0929, 0x0929, 'Canonical').	% Lo       DEVANAGARI LETTER NNNA
unicode_decomposition_type(0x0931, 0x0931, 'Canonical').	% Lo       DEVANAGARI LETTER RRA
unicode_decomposition_type(0x0934, 0x0934, 'Canonical').	% Lo       DEVANAGARI LETTER LLLA
unicode_decomposition_type(0x0958, 0x095F, 'Canonical').	% Lo   [8] DEVANAGARI LETTER QA..DEVANAGARI LETTER YYA
unicode_decomposition_type(0x09CB, 0x09CC, 'Canonical').	% Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_decomposition_type(0x09DC, 0x09DD, 'Canonical').	% Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_decomposition_type(0x09DF, 0x09DF, 'Canonical').	% Lo       BENGALI LETTER YYA
unicode_decomposition_type(0x0A33, 0x0A33, 'Canonical').	% Lo       GURMUKHI LETTER LLA
unicode_decomposition_type(0x0A36, 0x0A36, 'Canonical').	% Lo       GURMUKHI LETTER SHA
unicode_decomposition_type(0x0A59, 0x0A5B, 'Canonical').	% Lo   [3] GURMUKHI LETTER KHHA..GURMUKHI LETTER ZA
unicode_decomposition_type(0x0A5E, 0x0A5E, 'Canonical').	% Lo       GURMUKHI LETTER FA
unicode_decomposition_type(0x0B48, 0x0B48, 'Canonical').	% Mc       ORIYA VOWEL SIGN AI
unicode_decomposition_type(0x0B4B, 0x0B4C, 'Canonical').	% Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
unicode_decomposition_type(0x0B5C, 0x0B5D, 'Canonical').	% Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_decomposition_type(0x0B94, 0x0B94, 'Canonical').	% Lo       TAMIL LETTER AU
unicode_decomposition_type(0x0BCA, 0x0BCC, 'Canonical').	% Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_decomposition_type(0x0C48, 0x0C48, 'Canonical').	% Mn       TELUGU VOWEL SIGN AI
unicode_decomposition_type(0x0CC0, 0x0CC0, 'Canonical').	% Mc       KANNADA VOWEL SIGN II
unicode_decomposition_type(0x0CC7, 0x0CC8, 'Canonical').	% Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_decomposition_type(0x0CCA, 0x0CCB, 'Canonical').	% Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_decomposition_type(0x0D4A, 0x0D4C, 'Canonical').	% Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_decomposition_type(0x0DDA, 0x0DDA, 'Canonical').	% Mc       SINHALA VOWEL SIGN DIGA KOMBUVA
unicode_decomposition_type(0x0DDC, 0x0DDE, 'Canonical').	% Mc   [3] SINHALA VOWEL SIGN KOMBUVA HAA AELA-PILLA..SINHALA VOWEL SIGN KOMBUVA HAA GAYANUKITTA
unicode_decomposition_type(0x0F43, 0x0F43, 'Canonical').	% Lo       TIBETAN LETTER GHA
unicode_decomposition_type(0x0F4D, 0x0F4D, 'Canonical').	% Lo       TIBETAN LETTER DDHA
unicode_decomposition_type(0x0F52, 0x0F52, 'Canonical').	% Lo       TIBETAN LETTER DHA
unicode_decomposition_type(0x0F57, 0x0F57, 'Canonical').	% Lo       TIBETAN LETTER BHA
unicode_decomposition_type(0x0F5C, 0x0F5C, 'Canonical').	% Lo       TIBETAN LETTER DZHA
unicode_decomposition_type(0x0F69, 0x0F69, 'Canonical').	% Lo       TIBETAN LETTER KSSA
unicode_decomposition_type(0x0F73, 0x0F73, 'Canonical').	% Mn       TIBETAN VOWEL SIGN II
unicode_decomposition_type(0x0F75, 0x0F76, 'Canonical').	% Mn   [2] TIBETAN VOWEL SIGN UU..TIBETAN VOWEL SIGN VOCALIC R
unicode_decomposition_type(0x0F78, 0x0F78, 'Canonical').	% Mn       TIBETAN VOWEL SIGN VOCALIC L
unicode_decomposition_type(0x0F81, 0x0F81, 'Canonical').	% Mn       TIBETAN VOWEL SIGN REVERSED II
unicode_decomposition_type(0x0F93, 0x0F93, 'Canonical').	% Mn       TIBETAN SUBJOINED LETTER GHA
unicode_decomposition_type(0x0F9D, 0x0F9D, 'Canonical').	% Mn       TIBETAN SUBJOINED LETTER DDHA
unicode_decomposition_type(0x0FA2, 0x0FA2, 'Canonical').	% Mn       TIBETAN SUBJOINED LETTER DHA
unicode_decomposition_type(0x0FA7, 0x0FA7, 'Canonical').	% Mn       TIBETAN SUBJOINED LETTER BHA
unicode_decomposition_type(0x0FAC, 0x0FAC, 'Canonical').	% Mn       TIBETAN SUBJOINED LETTER DZHA
unicode_decomposition_type(0x0FB9, 0x0FB9, 'Canonical').	% Mn       TIBETAN SUBJOINED LETTER KSSA
unicode_decomposition_type(0x1026, 0x1026, 'Canonical').	% Lo       MYANMAR LETTER UU
unicode_decomposition_type(0x1B06, 0x1B06, 'Canonical').	% Lo       BALINESE LETTER AKARA TEDUNG
unicode_decomposition_type(0x1B08, 0x1B08, 'Canonical').	% Lo       BALINESE LETTER IKARA TEDUNG
unicode_decomposition_type(0x1B0A, 0x1B0A, 'Canonical').	% Lo       BALINESE LETTER UKARA TEDUNG
unicode_decomposition_type(0x1B0C, 0x1B0C, 'Canonical').	% Lo       BALINESE LETTER RA REPA TEDUNG
unicode_decomposition_type(0x1B0E, 0x1B0E, 'Canonical').	% Lo       BALINESE LETTER LA LENGA TEDUNG
unicode_decomposition_type(0x1B12, 0x1B12, 'Canonical').	% Lo       BALINESE LETTER OKARA TEDUNG
unicode_decomposition_type(0x1B3B, 0x1B3B, 'Canonical').	% Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_decomposition_type(0x1B3D, 0x1B3D, 'Canonical').	% Mc       BALINESE VOWEL SIGN LA LENGA TEDUNG
unicode_decomposition_type(0x1B40, 0x1B41, 'Canonical').	% Mc   [2] BALINESE VOWEL SIGN TALING TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG
unicode_decomposition_type(0x1B43, 0x1B43, 'Canonical').	% Mc       BALINESE VOWEL SIGN PEPET TEDUNG
unicode_decomposition_type(0x1E00, 0x1E99, 'Canonical').	% L& [154] LATIN CAPITAL LETTER A WITH RING BELOW..LATIN SMALL LETTER Y WITH RING ABOVE
unicode_decomposition_type(0x1E9B, 0x1E9B, 'Canonical').	% L&       LATIN SMALL LETTER LONG S WITH DOT ABOVE
unicode_decomposition_type(0x1EA0, 0x1EF9, 'Canonical').	% L&  [90] LATIN CAPITAL LETTER A WITH DOT BELOW..LATIN SMALL LETTER Y WITH TILDE
unicode_decomposition_type(0x1F00, 0x1F15, 'Canonical').	% L&  [22] GREEK SMALL LETTER ALPHA WITH PSILI..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_decomposition_type(0x1F18, 0x1F1D, 'Canonical').	% L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_decomposition_type(0x1F20, 0x1F45, 'Canonical').	% L&  [38] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_decomposition_type(0x1F48, 0x1F4D, 'Canonical').	% L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_decomposition_type(0x1F50, 0x1F57, 'Canonical').	% L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_decomposition_type(0x1F59, 0x1F59, 'Canonical').	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_decomposition_type(0x1F5B, 0x1F5B, 'Canonical').	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_decomposition_type(0x1F5D, 0x1F5D, 'Canonical').	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_decomposition_type(0x1F5F, 0x1F7D, 'Canonical').	% L&  [31] GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_decomposition_type(0x1F80, 0x1FB4, 'Canonical').	% L&  [53] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_decomposition_type(0x1FB6, 0x1FBC, 'Canonical').	% L&   [7] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_decomposition_type(0x1FBE, 0x1FBE, 'Canonical').	% L&       GREEK PROSGEGRAMMENI
unicode_decomposition_type(0x1FC1, 0x1FC1, 'Canonical').	% Sk       GREEK DIALYTIKA AND PERISPOMENI
unicode_decomposition_type(0x1FC2, 0x1FC4, 'Canonical').	% L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_decomposition_type(0x1FC6, 0x1FCC, 'Canonical').	% L&   [7] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_decomposition_type(0x1FCD, 0x1FCF, 'Canonical').	% Sk   [3] GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI
unicode_decomposition_type(0x1FD0, 0x1FD3, 'Canonical').	% L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_decomposition_type(0x1FD6, 0x1FDB, 'Canonical').	% L&   [6] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_decomposition_type(0x1FDD, 0x1FDF, 'Canonical').	% Sk   [3] GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI
unicode_decomposition_type(0x1FE0, 0x1FEC, 'Canonical').	% L&  [13] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_decomposition_type(0x1FED, 0x1FEF, 'Canonical').	% Sk   [3] GREEK DIALYTIKA AND VARIA..GREEK VARIA
unicode_decomposition_type(0x1FF2, 0x1FF4, 'Canonical').	% L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_decomposition_type(0x1FF6, 0x1FFC, 'Canonical').	% L&   [7] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_decomposition_type(0x1FFD, 0x1FFD, 'Canonical').	% Sk       GREEK OXIA
unicode_decomposition_type(0x2000, 0x2001, 'Canonical').	% Zs   [2] EN QUAD..EM QUAD
unicode_decomposition_type(0x2126, 0x2126, 'Canonical').	% L&       OHM SIGN
unicode_decomposition_type(0x212A, 0x212B, 'Canonical').	% L&   [2] KELVIN SIGN..ANGSTROM SIGN
unicode_decomposition_type(0x219A, 0x219B, 'Canonical').	% Sm   [2] LEFTWARDS ARROW WITH STROKE..RIGHTWARDS ARROW WITH STROKE
unicode_decomposition_type(0x21AE, 0x21AE, 'Canonical').	% Sm       LEFT RIGHT ARROW WITH STROKE
unicode_decomposition_type(0x21CD, 0x21CD, 'Canonical').	% So       LEFTWARDS DOUBLE ARROW WITH STROKE
unicode_decomposition_type(0x21CE, 0x21CF, 'Canonical').	% Sm   [2] LEFT RIGHT DOUBLE ARROW WITH STROKE..RIGHTWARDS DOUBLE ARROW WITH STROKE
unicode_decomposition_type(0x2204, 0x2204, 'Canonical').	% Sm       THERE DOES NOT EXIST
unicode_decomposition_type(0x2209, 0x2209, 'Canonical').	% Sm       NOT AN ELEMENT OF
unicode_decomposition_type(0x220C, 0x220C, 'Canonical').	% Sm       DOES NOT CONTAIN AS MEMBER
unicode_decomposition_type(0x2224, 0x2224, 'Canonical').	% Sm       DOES NOT DIVIDE
unicode_decomposition_type(0x2226, 0x2226, 'Canonical').	% Sm       NOT PARALLEL TO
unicode_decomposition_type(0x2241, 0x2241, 'Canonical').	% Sm       NOT TILDE
unicode_decomposition_type(0x2244, 0x2244, 'Canonical').	% Sm       NOT ASYMPTOTICALLY EQUAL TO
unicode_decomposition_type(0x2247, 0x2247, 'Canonical').	% Sm       NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
unicode_decomposition_type(0x2249, 0x2249, 'Canonical').	% Sm       NOT ALMOST EQUAL TO
unicode_decomposition_type(0x2260, 0x2260, 'Canonical').	% Sm       NOT EQUAL TO
unicode_decomposition_type(0x2262, 0x2262, 'Canonical').	% Sm       NOT IDENTICAL TO
unicode_decomposition_type(0x226D, 0x2271, 'Canonical').	% Sm   [5] NOT EQUIVALENT TO..NEITHER GREATER-THAN NOR EQUAL TO
unicode_decomposition_type(0x2274, 0x2275, 'Canonical').	% Sm   [2] NEITHER LESS-THAN NOR EQUIVALENT TO..NEITHER GREATER-THAN NOR EQUIVALENT TO
unicode_decomposition_type(0x2278, 0x2279, 'Canonical').	% Sm   [2] NEITHER LESS-THAN NOR GREATER-THAN..NEITHER GREATER-THAN NOR LESS-THAN
unicode_decomposition_type(0x2280, 0x2281, 'Canonical').	% Sm   [2] DOES NOT PRECEDE..DOES NOT SUCCEED
unicode_decomposition_type(0x2284, 0x2285, 'Canonical').	% Sm   [2] NOT A SUBSET OF..NOT A SUPERSET OF
unicode_decomposition_type(0x2288, 0x2289, 'Canonical').	% Sm   [2] NEITHER A SUBSET OF NOR EQUAL TO..NEITHER A SUPERSET OF NOR EQUAL TO
unicode_decomposition_type(0x22AC, 0x22AF, 'Canonical').	% Sm   [4] DOES NOT PROVE..NEGATED DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
unicode_decomposition_type(0x22E0, 0x22E3, 'Canonical').	% Sm   [4] DOES NOT PRECEDE OR EQUAL..NOT SQUARE ORIGINAL OF OR EQUAL TO
unicode_decomposition_type(0x22EA, 0x22ED, 'Canonical').	% Sm   [4] NOT NORMAL SUBGROUP OF..DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
unicode_decomposition_type(0x2329, 0x2329, 'Canonical').	% Ps       LEFT-POINTING ANGLE BRACKET
unicode_decomposition_type(0x232A, 0x232A, 'Canonical').	% Pe       RIGHT-POINTING ANGLE BRACKET
unicode_decomposition_type(0x2ADC, 0x2ADC, 'Canonical').	% Sm       FORKING
unicode_decomposition_type(0x304C, 0x304C, 'Canonical').	% Lo       HIRAGANA LETTER GA
unicode_decomposition_type(0x304E, 0x304E, 'Canonical').	% Lo       HIRAGANA LETTER GI
unicode_decomposition_type(0x3050, 0x3050, 'Canonical').	% Lo       HIRAGANA LETTER GU
unicode_decomposition_type(0x3052, 0x3052, 'Canonical').	% Lo       HIRAGANA LETTER GE
unicode_decomposition_type(0x3054, 0x3054, 'Canonical').	% Lo       HIRAGANA LETTER GO
unicode_decomposition_type(0x3056, 0x3056, 'Canonical').	% Lo       HIRAGANA LETTER ZA
unicode_decomposition_type(0x3058, 0x3058, 'Canonical').	% Lo       HIRAGANA LETTER ZI
unicode_decomposition_type(0x305A, 0x305A, 'Canonical').	% Lo       HIRAGANA LETTER ZU
unicode_decomposition_type(0x305C, 0x305C, 'Canonical').	% Lo       HIRAGANA LETTER ZE
unicode_decomposition_type(0x305E, 0x305E, 'Canonical').	% Lo       HIRAGANA LETTER ZO
unicode_decomposition_type(0x3060, 0x3060, 'Canonical').	% Lo       HIRAGANA LETTER DA
unicode_decomposition_type(0x3062, 0x3062, 'Canonical').	% Lo       HIRAGANA LETTER DI
unicode_decomposition_type(0x3065, 0x3065, 'Canonical').	% Lo       HIRAGANA LETTER DU
unicode_decomposition_type(0x3067, 0x3067, 'Canonical').	% Lo       HIRAGANA LETTER DE
unicode_decomposition_type(0x3069, 0x3069, 'Canonical').	% Lo       HIRAGANA LETTER DO
unicode_decomposition_type(0x3070, 0x3071, 'Canonical').	% Lo   [2] HIRAGANA LETTER BA..HIRAGANA LETTER PA
unicode_decomposition_type(0x3073, 0x3074, 'Canonical').	% Lo   [2] HIRAGANA LETTER BI..HIRAGANA LETTER PI
unicode_decomposition_type(0x3076, 0x3077, 'Canonical').	% Lo   [2] HIRAGANA LETTER BU..HIRAGANA LETTER PU
unicode_decomposition_type(0x3079, 0x307A, 'Canonical').	% Lo   [2] HIRAGANA LETTER BE..HIRAGANA LETTER PE
unicode_decomposition_type(0x307C, 0x307D, 'Canonical').	% Lo   [2] HIRAGANA LETTER BO..HIRAGANA LETTER PO
unicode_decomposition_type(0x3094, 0x3094, 'Canonical').	% Lo       HIRAGANA LETTER VU
unicode_decomposition_type(0x309E, 0x309E, 'Canonical').	% Lm       HIRAGANA VOICED ITERATION MARK
unicode_decomposition_type(0x30AC, 0x30AC, 'Canonical').	% Lo       KATAKANA LETTER GA
unicode_decomposition_type(0x30AE, 0x30AE, 'Canonical').	% Lo       KATAKANA LETTER GI
unicode_decomposition_type(0x30B0, 0x30B0, 'Canonical').	% Lo       KATAKANA LETTER GU
unicode_decomposition_type(0x30B2, 0x30B2, 'Canonical').	% Lo       KATAKANA LETTER GE
unicode_decomposition_type(0x30B4, 0x30B4, 'Canonical').	% Lo       KATAKANA LETTER GO
unicode_decomposition_type(0x30B6, 0x30B6, 'Canonical').	% Lo       KATAKANA LETTER ZA
unicode_decomposition_type(0x30B8, 0x30B8, 'Canonical').	% Lo       KATAKANA LETTER ZI
unicode_decomposition_type(0x30BA, 0x30BA, 'Canonical').	% Lo       KATAKANA LETTER ZU
unicode_decomposition_type(0x30BC, 0x30BC, 'Canonical').	% Lo       KATAKANA LETTER ZE
unicode_decomposition_type(0x30BE, 0x30BE, 'Canonical').	% Lo       KATAKANA LETTER ZO
unicode_decomposition_type(0x30C0, 0x30C0, 'Canonical').	% Lo       KATAKANA LETTER DA
unicode_decomposition_type(0x30C2, 0x30C2, 'Canonical').	% Lo       KATAKANA LETTER DI
unicode_decomposition_type(0x30C5, 0x30C5, 'Canonical').	% Lo       KATAKANA LETTER DU
unicode_decomposition_type(0x30C7, 0x30C7, 'Canonical').	% Lo       KATAKANA LETTER DE
unicode_decomposition_type(0x30C9, 0x30C9, 'Canonical').	% Lo       KATAKANA LETTER DO
unicode_decomposition_type(0x30D0, 0x30D1, 'Canonical').	% Lo   [2] KATAKANA LETTER BA..KATAKANA LETTER PA
unicode_decomposition_type(0x30D3, 0x30D4, 'Canonical').	% Lo   [2] KATAKANA LETTER BI..KATAKANA LETTER PI
unicode_decomposition_type(0x30D6, 0x30D7, 'Canonical').	% Lo   [2] KATAKANA LETTER BU..KATAKANA LETTER PU
unicode_decomposition_type(0x30D9, 0x30DA, 'Canonical').	% Lo   [2] KATAKANA LETTER BE..KATAKANA LETTER PE
unicode_decomposition_type(0x30DC, 0x30DD, 'Canonical').	% Lo   [2] KATAKANA LETTER BO..KATAKANA LETTER PO
unicode_decomposition_type(0x30F4, 0x30F4, 'Canonical').	% Lo       KATAKANA LETTER VU
unicode_decomposition_type(0x30F7, 0x30FA, 'Canonical').	% Lo   [4] KATAKANA LETTER VA..KATAKANA LETTER VO
unicode_decomposition_type(0x30FE, 0x30FE, 'Canonical').	% Lm       KATAKANA VOICED ITERATION MARK
unicode_decomposition_type(0xAC00, 0xD7A3, 'Canonical').	% Lo [11172] HANGUL SYLLABLE GA..HANGUL SYLLABLE HIH
unicode_decomposition_type(0xF900, 0xFA0D, 'Canonical').	% Lo [270] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA0D
unicode_decomposition_type(0xFA10, 0xFA10, 'Canonical').	% Lo       CJK COMPATIBILITY IDEOGRAPH-FA10
unicode_decomposition_type(0xFA12, 0xFA12, 'Canonical').	% Lo       CJK COMPATIBILITY IDEOGRAPH-FA12
unicode_decomposition_type(0xFA15, 0xFA1E, 'Canonical').	% Lo  [10] CJK COMPATIBILITY IDEOGRAPH-FA15..CJK COMPATIBILITY IDEOGRAPH-FA1E
unicode_decomposition_type(0xFA20, 0xFA20, 'Canonical').	% Lo       CJK COMPATIBILITY IDEOGRAPH-FA20
unicode_decomposition_type(0xFA22, 0xFA22, 'Canonical').	% Lo       CJK COMPATIBILITY IDEOGRAPH-FA22
unicode_decomposition_type(0xFA25, 0xFA26, 'Canonical').	% Lo   [2] CJK COMPATIBILITY IDEOGRAPH-FA25..CJK COMPATIBILITY IDEOGRAPH-FA26
unicode_decomposition_type(0xFA2A, 0xFA6D, 'Canonical').	% Lo  [68] CJK COMPATIBILITY IDEOGRAPH-FA2A..CJK COMPATIBILITY IDEOGRAPH-FA6D
unicode_decomposition_type(0xFA70, 0xFAD9, 'Canonical').	% Lo [106] CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9
unicode_decomposition_type(0xFB1D, 0xFB1D, 'Canonical').	% Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_decomposition_type(0xFB1F, 0xFB1F, 'Canonical').	% Lo       HEBREW LIGATURE YIDDISH YOD YOD PATAH
unicode_decomposition_type(0xFB2A, 0xFB36, 'Canonical').	% Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_decomposition_type(0xFB38, 0xFB3C, 'Canonical').	% Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_decomposition_type(0xFB3E, 0xFB3E, 'Canonical').	% Lo       HEBREW LETTER MEM WITH DAGESH
unicode_decomposition_type(0xFB40, 0xFB41, 'Canonical').	% Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_decomposition_type(0xFB43, 0xFB44, 'Canonical').	% Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_decomposition_type(0xFB46, 0xFB4E, 'Canonical').	% Lo   [9] HEBREW LETTER TSADI WITH DAGESH..HEBREW LETTER PE WITH RAFE
unicode_decomposition_type(0x1109A, 0x1109A, 'Canonical').	% Lo       KAITHI LETTER DDDHA
unicode_decomposition_type(0x1109C, 0x1109C, 'Canonical').	% Lo       KAITHI LETTER RHA
unicode_decomposition_type(0x110AB, 0x110AB, 'Canonical').	% Lo       KAITHI LETTER VA
unicode_decomposition_type(0x1112E, 0x1112F, 'Canonical').	% Mn   [2] CHAKMA VOWEL SIGN O..CHAKMA VOWEL SIGN AU
unicode_decomposition_type(0x1D15E, 0x1D164, 'Canonical').	% So   [7] MUSICAL SYMBOL HALF NOTE..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
unicode_decomposition_type(0x1D1BB, 0x1D1C0, 'Canonical').	% So   [6] MUSICAL SYMBOL MINIMA..MUSICAL SYMBOL FUSA BLACK
unicode_decomposition_type(0x2F800, 0x2FA1D, 'Canonical').	% Lo [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D

% Total code points: 13225

% ================================================

unicode_decomposition_type(0x00A8, 0x00A8, 'Compat').	% Sk       DIAERESIS
unicode_decomposition_type(0x00AF, 0x00AF, 'Compat').	% Sk       MACRON
unicode_decomposition_type(0x00B4, 0x00B4, 'Compat').	% Sk       ACUTE ACCENT
unicode_decomposition_type(0x00B5, 0x00B5, 'Compat').	% L&       MICRO SIGN
unicode_decomposition_type(0x00B8, 0x00B8, 'Compat').	% Sk       CEDILLA
unicode_decomposition_type(0x0132, 0x0133, 'Compat').	% L&   [2] LATIN CAPITAL LIGATURE IJ..LATIN SMALL LIGATURE IJ
unicode_decomposition_type(0x013F, 0x0140, 'Compat').	% L&   [2] LATIN CAPITAL LETTER L WITH MIDDLE DOT..LATIN SMALL LETTER L WITH MIDDLE DOT
unicode_decomposition_type(0x0149, 0x0149, 'Compat').	% L&       LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
unicode_decomposition_type(0x017F, 0x017F, 'Compat').	% L&       LATIN SMALL LETTER LONG S
unicode_decomposition_type(0x01C4, 0x01CC, 'Compat').	% L&   [9] LATIN CAPITAL LETTER DZ WITH CARON..LATIN SMALL LETTER NJ
unicode_decomposition_type(0x01F1, 0x01F3, 'Compat').	% L&   [3] LATIN CAPITAL LETTER DZ..LATIN SMALL LETTER DZ
unicode_decomposition_type(0x02D8, 0x02DD, 'Compat').	% Sk   [6] BREVE..DOUBLE ACUTE ACCENT
unicode_decomposition_type(0x037A, 0x037A, 'Compat').	% Lm       GREEK YPOGEGRAMMENI
unicode_decomposition_type(0x0384, 0x0384, 'Compat').	% Sk       GREEK TONOS
unicode_decomposition_type(0x03D0, 0x03D2, 'Compat').	% L&   [3] GREEK BETA SYMBOL..GREEK UPSILON WITH HOOK SYMBOL
unicode_decomposition_type(0x03D5, 0x03D6, 'Compat').	% L&   [2] GREEK PHI SYMBOL..GREEK PI SYMBOL
unicode_decomposition_type(0x03F0, 0x03F2, 'Compat').	% L&   [3] GREEK KAPPA SYMBOL..GREEK LUNATE SIGMA SYMBOL
unicode_decomposition_type(0x03F4, 0x03F5, 'Compat').	% L&   [2] GREEK CAPITAL THETA SYMBOL..GREEK LUNATE EPSILON SYMBOL
unicode_decomposition_type(0x03F9, 0x03F9, 'Compat').	% L&       GREEK CAPITAL LUNATE SIGMA SYMBOL
unicode_decomposition_type(0x0587, 0x0587, 'Compat').	% L&       ARMENIAN SMALL LIGATURE ECH YIWN
unicode_decomposition_type(0x0675, 0x0678, 'Compat').	% Lo   [4] ARABIC LETTER HIGH HAMZA ALEF..ARABIC LETTER HIGH HAMZA YEH
unicode_decomposition_type(0x0E33, 0x0E33, 'Compat').	% Lo       THAI CHARACTER SARA AM
unicode_decomposition_type(0x0EB3, 0x0EB3, 'Compat').	% Lo       LAO VOWEL SIGN AM
unicode_decomposition_type(0x0EDC, 0x0EDD, 'Compat').	% Lo   [2] LAO HO NO..LAO HO MO
unicode_decomposition_type(0x0F77, 0x0F77, 'Compat').	% Mn       TIBETAN VOWEL SIGN VOCALIC RR
unicode_decomposition_type(0x0F79, 0x0F79, 'Compat').	% Mn       TIBETAN VOWEL SIGN VOCALIC LL
unicode_decomposition_type(0x1E9A, 0x1E9A, 'Compat').	% L&       LATIN SMALL LETTER A WITH RIGHT HALF RING
unicode_decomposition_type(0x1FBD, 0x1FBD, 'Compat').	% Sk       GREEK KORONIS
unicode_decomposition_type(0x1FBF, 0x1FC0, 'Compat').	% Sk   [2] GREEK PSILI..GREEK PERISPOMENI
unicode_decomposition_type(0x1FFE, 0x1FFE, 'Compat').	% Sk       GREEK DASIA
unicode_decomposition_type(0x2002, 0x2006, 'Compat').	% Zs   [5] EN SPACE..SIX-PER-EM SPACE
unicode_decomposition_type(0x2008, 0x200A, 'Compat').	% Zs   [3] PUNCTUATION SPACE..HAIR SPACE
unicode_decomposition_type(0x2017, 0x2017, 'Compat').	% Po       DOUBLE LOW LINE
unicode_decomposition_type(0x2024, 0x2026, 'Compat').	% Po   [3] ONE DOT LEADER..HORIZONTAL ELLIPSIS
unicode_decomposition_type(0x2033, 0x2034, 'Compat').	% Po   [2] DOUBLE PRIME..TRIPLE PRIME
unicode_decomposition_type(0x2036, 0x2037, 'Compat').	% Po   [2] REVERSED DOUBLE PRIME..REVERSED TRIPLE PRIME
unicode_decomposition_type(0x203C, 0x203C, 'Compat').	% Po       DOUBLE EXCLAMATION MARK
unicode_decomposition_type(0x203E, 0x203E, 'Compat').	% Po       OVERLINE
unicode_decomposition_type(0x2047, 0x2049, 'Compat').	% Po   [3] DOUBLE QUESTION MARK..EXCLAMATION QUESTION MARK
unicode_decomposition_type(0x2057, 0x2057, 'Compat').	% Po       QUADRUPLE PRIME
unicode_decomposition_type(0x205F, 0x205F, 'Compat').	% Zs       MEDIUM MATHEMATICAL SPACE
unicode_decomposition_type(0x20A8, 0x20A8, 'Compat').	% Sc       RUPEE SIGN
unicode_decomposition_type(0x2100, 0x2101, 'Compat').	% So   [2] ACCOUNT OF..ADDRESSED TO THE SUBJECT
unicode_decomposition_type(0x2103, 0x2103, 'Compat').	% So       DEGREE CELSIUS
unicode_decomposition_type(0x2105, 0x2106, 'Compat').	% So   [2] CARE OF..CADA UNA
unicode_decomposition_type(0x2107, 0x2107, 'Compat').	% L&       EULER CONSTANT
unicode_decomposition_type(0x2109, 0x2109, 'Compat').	% So       DEGREE FAHRENHEIT
unicode_decomposition_type(0x2116, 0x2116, 'Compat').	% So       NUMERO SIGN
unicode_decomposition_type(0x2121, 0x2121, 'Compat').	% So       TELEPHONE SIGN
unicode_decomposition_type(0x2135, 0x2138, 'Compat').	% Lo   [4] ALEF SYMBOL..DALET SYMBOL
unicode_decomposition_type(0x213B, 0x213B, 'Compat').	% So       FACSIMILE SIGN
unicode_decomposition_type(0x2160, 0x217F, 'Compat').	% Nl  [32] ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL ONE THOUSAND
unicode_decomposition_type(0x222C, 0x222D, 'Compat').	% Sm   [2] DOUBLE INTEGRAL..TRIPLE INTEGRAL
unicode_decomposition_type(0x222F, 0x2230, 'Compat').	% Sm   [2] SURFACE INTEGRAL..VOLUME INTEGRAL
unicode_decomposition_type(0x2474, 0x249B, 'Compat').	% No  [40] PARENTHESIZED DIGIT ONE..NUMBER TWENTY FULL STOP
unicode_decomposition_type(0x249C, 0x24B5, 'Compat').	% So  [26] PARENTHESIZED LATIN SMALL LETTER A..PARENTHESIZED LATIN SMALL LETTER Z
unicode_decomposition_type(0x2A0C, 0x2A0C, 'Compat').	% Sm       QUADRUPLE INTEGRAL OPERATOR
unicode_decomposition_type(0x2A74, 0x2A76, 'Compat').	% Sm   [3] DOUBLE COLON EQUAL..THREE CONSECUTIVE EQUALS SIGNS
unicode_decomposition_type(0x2E9F, 0x2E9F, 'Compat').	% So       CJK RADICAL MOTHER
unicode_decomposition_type(0x2EF3, 0x2EF3, 'Compat').	% So       CJK RADICAL C-SIMPLIFIED TURTLE
unicode_decomposition_type(0x2F00, 0x2FD5, 'Compat').	% So [214] KANGXI RADICAL ONE..KANGXI RADICAL FLUTE
unicode_decomposition_type(0x3036, 0x3036, 'Compat').	% So       CIRCLED POSTAL MARK
unicode_decomposition_type(0x3038, 0x303A, 'Compat').	% Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
unicode_decomposition_type(0x309B, 0x309C, 'Compat').	% Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_decomposition_type(0x3131, 0x318E, 'Compat').	% Lo  [94] HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE
unicode_decomposition_type(0x3200, 0x321E, 'Compat').	% So  [31] PARENTHESIZED HANGUL KIYEOK..PARENTHESIZED KOREAN CHARACTER O HU
unicode_decomposition_type(0x3220, 0x3229, 'Compat').	% No  [10] PARENTHESIZED IDEOGRAPH ONE..PARENTHESIZED IDEOGRAPH TEN
unicode_decomposition_type(0x322A, 0x3243, 'Compat').	% So  [26] PARENTHESIZED IDEOGRAPH MOON..PARENTHESIZED IDEOGRAPH REACH
unicode_decomposition_type(0x32C0, 0x32CB, 'Compat').	% So  [12] IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY..IDEOGRAPHIC TELEGRAPH SYMBOL FOR DECEMBER
unicode_decomposition_type(0x3358, 0x3370, 'Compat').	% So  [25] IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ZERO..IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-FOUR
unicode_decomposition_type(0x33E0, 0x33FE, 'Compat').	% So  [31] IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY ONE..IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTY-ONE
unicode_decomposition_type(0xFB00, 0xFB06, 'Compat').	% L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_decomposition_type(0xFB13, 0xFB17, 'Compat').	% L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_decomposition_type(0xFB4F, 0xFB4F, 'Compat').	% Lo       HEBREW LIGATURE ALEF LAMED
unicode_decomposition_type(0xFE49, 0xFE4C, 'Compat').	% Po   [4] DASHED OVERLINE..DOUBLE WAVY OVERLINE
unicode_decomposition_type(0xFE4D, 0xFE4F, 'Compat').	% Pc   [3] DASHED LOW LINE..WAVY LOW LINE
unicode_decomposition_type(0x1F100, 0x1F10A, 'Compat').	% No  [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
unicode_decomposition_type(0x1F110, 0x1F12A, 'Compat').	% So  [27] PARENTHESIZED LATIN CAPITAL LETTER A..TORTOISE SHELL BRACKETED LATIN CAPITAL LETTER S
unicode_decomposition_type(0x1F240, 0x1F248, 'Compat').	% So   [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557

% Total code points: 720

% ================================================

unicode_decomposition_type(0x2102, 0x2102, 'Font').	% L&       DOUBLE-STRUCK CAPITAL C
unicode_decomposition_type(0x210A, 0x2113, 'Font').	% L&  [10] SCRIPT SMALL G..SCRIPT SMALL L
unicode_decomposition_type(0x2115, 0x2115, 'Font').	% L&       DOUBLE-STRUCK CAPITAL N
unicode_decomposition_type(0x2119, 0x211D, 'Font').	% L&   [5] DOUBLE-STRUCK CAPITAL P..DOUBLE-STRUCK CAPITAL R
unicode_decomposition_type(0x2124, 0x2124, 'Font').	% L&       DOUBLE-STRUCK CAPITAL Z
unicode_decomposition_type(0x2128, 0x2128, 'Font').	% L&       BLACK-LETTER CAPITAL Z
unicode_decomposition_type(0x212C, 0x212D, 'Font').	% L&   [2] SCRIPT CAPITAL B..BLACK-LETTER CAPITAL C
unicode_decomposition_type(0x212F, 0x2131, 'Font').	% L&   [3] SCRIPT SMALL E..SCRIPT CAPITAL F
unicode_decomposition_type(0x2133, 0x2134, 'Font').	% L&   [2] SCRIPT CAPITAL M..SCRIPT SMALL O
unicode_decomposition_type(0x2139, 0x2139, 'Font').	% L&       INFORMATION SOURCE
unicode_decomposition_type(0x213C, 0x213F, 'Font').	% L&   [4] DOUBLE-STRUCK SMALL PI..DOUBLE-STRUCK CAPITAL PI
unicode_decomposition_type(0x2140, 0x2140, 'Font').	% Sm       DOUBLE-STRUCK N-ARY SUMMATION
unicode_decomposition_type(0x2145, 0x2149, 'Font').	% L&   [5] DOUBLE-STRUCK ITALIC CAPITAL D..DOUBLE-STRUCK ITALIC SMALL J
unicode_decomposition_type(0xFB20, 0xFB28, 'Font').	% Lo   [9] HEBREW LETTER ALTERNATIVE AYIN..HEBREW LETTER WIDE TAV
unicode_decomposition_type(0xFB29, 0xFB29, 'Font').	% Sm       HEBREW LETTER ALTERNATIVE PLUS SIGN
unicode_decomposition_type(0x1D400, 0x1D454, 'Font').	% L&  [85] MATHEMATICAL BOLD CAPITAL A..MATHEMATICAL ITALIC SMALL G
unicode_decomposition_type(0x1D456, 0x1D49C, 'Font').	% L&  [71] MATHEMATICAL ITALIC SMALL I..MATHEMATICAL SCRIPT CAPITAL A
unicode_decomposition_type(0x1D49E, 0x1D49F, 'Font').	% L&   [2] MATHEMATICAL SCRIPT CAPITAL C..MATHEMATICAL SCRIPT CAPITAL D
unicode_decomposition_type(0x1D4A2, 0x1D4A2, 'Font').	% L&       MATHEMATICAL SCRIPT CAPITAL G
unicode_decomposition_type(0x1D4A5, 0x1D4A6, 'Font').	% L&   [2] MATHEMATICAL SCRIPT CAPITAL J..MATHEMATICAL SCRIPT CAPITAL K
unicode_decomposition_type(0x1D4A9, 0x1D4AC, 'Font').	% L&   [4] MATHEMATICAL SCRIPT CAPITAL N..MATHEMATICAL SCRIPT CAPITAL Q
unicode_decomposition_type(0x1D4AE, 0x1D4B9, 'Font').	% L&  [12] MATHEMATICAL SCRIPT CAPITAL S..MATHEMATICAL SCRIPT SMALL D
unicode_decomposition_type(0x1D4BB, 0x1D4BB, 'Font').	% L&       MATHEMATICAL SCRIPT SMALL F
unicode_decomposition_type(0x1D4BD, 0x1D4C3, 'Font').	% L&   [7] MATHEMATICAL SCRIPT SMALL H..MATHEMATICAL SCRIPT SMALL N
unicode_decomposition_type(0x1D4C5, 0x1D505, 'Font').	% L&  [65] MATHEMATICAL SCRIPT SMALL P..MATHEMATICAL FRAKTUR CAPITAL B
unicode_decomposition_type(0x1D507, 0x1D50A, 'Font').	% L&   [4] MATHEMATICAL FRAKTUR CAPITAL D..MATHEMATICAL FRAKTUR CAPITAL G
unicode_decomposition_type(0x1D50D, 0x1D514, 'Font').	% L&   [8] MATHEMATICAL FRAKTUR CAPITAL J..MATHEMATICAL FRAKTUR CAPITAL Q
unicode_decomposition_type(0x1D516, 0x1D51C, 'Font').	% L&   [7] MATHEMATICAL FRAKTUR CAPITAL S..MATHEMATICAL FRAKTUR CAPITAL Y
unicode_decomposition_type(0x1D51E, 0x1D539, 'Font').	% L&  [28] MATHEMATICAL FRAKTUR SMALL A..MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_decomposition_type(0x1D53B, 0x1D53E, 'Font').	% L&   [4] MATHEMATICAL DOUBLE-STRUCK CAPITAL D..MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_decomposition_type(0x1D540, 0x1D544, 'Font').	% L&   [5] MATHEMATICAL DOUBLE-STRUCK CAPITAL I..MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_decomposition_type(0x1D546, 0x1D546, 'Font').	% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_decomposition_type(0x1D54A, 0x1D550, 'Font').	% L&   [7] MATHEMATICAL DOUBLE-STRUCK CAPITAL S..MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_decomposition_type(0x1D552, 0x1D6A5, 'Font').	% L& [340] MATHEMATICAL DOUBLE-STRUCK SMALL A..MATHEMATICAL ITALIC SMALL DOTLESS J
unicode_decomposition_type(0x1D6A8, 0x1D6C0, 'Font').	% L&  [25] MATHEMATICAL BOLD CAPITAL ALPHA..MATHEMATICAL BOLD CAPITAL OMEGA
unicode_decomposition_type(0x1D6C1, 0x1D6C1, 'Font').	% Sm       MATHEMATICAL BOLD NABLA
unicode_decomposition_type(0x1D6C2, 0x1D6DA, 'Font').	% L&  [25] MATHEMATICAL BOLD SMALL ALPHA..MATHEMATICAL BOLD SMALL OMEGA
unicode_decomposition_type(0x1D6DB, 0x1D6DB, 'Font').	% Sm       MATHEMATICAL BOLD PARTIAL DIFFERENTIAL
unicode_decomposition_type(0x1D6DC, 0x1D6FA, 'Font').	% L&  [31] MATHEMATICAL BOLD EPSILON SYMBOL..MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_decomposition_type(0x1D6FB, 0x1D6FB, 'Font').	% Sm       MATHEMATICAL ITALIC NABLA
unicode_decomposition_type(0x1D6FC, 0x1D714, 'Font').	% L&  [25] MATHEMATICAL ITALIC SMALL ALPHA..MATHEMATICAL ITALIC SMALL OMEGA
unicode_decomposition_type(0x1D715, 0x1D715, 'Font').	% Sm       MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL
unicode_decomposition_type(0x1D716, 0x1D734, 'Font').	% L&  [31] MATHEMATICAL ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_decomposition_type(0x1D735, 0x1D735, 'Font').	% Sm       MATHEMATICAL BOLD ITALIC NABLA
unicode_decomposition_type(0x1D736, 0x1D74E, 'Font').	% L&  [25] MATHEMATICAL BOLD ITALIC SMALL ALPHA..MATHEMATICAL BOLD ITALIC SMALL OMEGA
unicode_decomposition_type(0x1D74F, 0x1D74F, 'Font').	% Sm       MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_decomposition_type(0x1D750, 0x1D76E, 'Font').	% L&  [31] MATHEMATICAL BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_decomposition_type(0x1D76F, 0x1D76F, 'Font').	% Sm       MATHEMATICAL SANS-SERIF BOLD NABLA
unicode_decomposition_type(0x1D770, 0x1D788, 'Font').	% L&  [25] MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
unicode_decomposition_type(0x1D789, 0x1D789, 'Font').	% Sm       MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL
unicode_decomposition_type(0x1D78A, 0x1D7A8, 'Font').	% L&  [31] MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_decomposition_type(0x1D7A9, 0x1D7A9, 'Font').	% Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA
unicode_decomposition_type(0x1D7AA, 0x1D7C2, 'Font').	% L&  [25] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
unicode_decomposition_type(0x1D7C3, 0x1D7C3, 'Font').	% Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_decomposition_type(0x1D7C4, 0x1D7CB, 'Font').	% L&   [8] MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD SMALL DIGAMMA
unicode_decomposition_type(0x1D7CE, 0x1D7FF, 'Font').	% Nd  [50] MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
unicode_decomposition_type(0x1EE00, 0x1EE03, 'Font').	% Lo   [4] ARABIC MATHEMATICAL ALEF..ARABIC MATHEMATICAL DAL
unicode_decomposition_type(0x1EE05, 0x1EE1F, 'Font').	% Lo  [27] ARABIC MATHEMATICAL WAW..ARABIC MATHEMATICAL DOTLESS QAF
unicode_decomposition_type(0x1EE21, 0x1EE22, 'Font').	% Lo   [2] ARABIC MATHEMATICAL INITIAL BEH..ARABIC MATHEMATICAL INITIAL JEEM
unicode_decomposition_type(0x1EE24, 0x1EE24, 'Font').	% Lo       ARABIC MATHEMATICAL INITIAL HEH
unicode_decomposition_type(0x1EE27, 0x1EE27, 'Font').	% Lo       ARABIC MATHEMATICAL INITIAL HAH
unicode_decomposition_type(0x1EE29, 0x1EE32, 'Font').	% Lo  [10] ARABIC MATHEMATICAL INITIAL YEH..ARABIC MATHEMATICAL INITIAL QAF
unicode_decomposition_type(0x1EE34, 0x1EE37, 'Font').	% Lo   [4] ARABIC MATHEMATICAL INITIAL SHEEN..ARABIC MATHEMATICAL INITIAL KHAH
unicode_decomposition_type(0x1EE39, 0x1EE39, 'Font').	% Lo       ARABIC MATHEMATICAL INITIAL DAD
unicode_decomposition_type(0x1EE3B, 0x1EE3B, 'Font').	% Lo       ARABIC MATHEMATICAL INITIAL GHAIN
unicode_decomposition_type(0x1EE42, 0x1EE42, 'Font').	% Lo       ARABIC MATHEMATICAL TAILED JEEM
unicode_decomposition_type(0x1EE47, 0x1EE47, 'Font').	% Lo       ARABIC MATHEMATICAL TAILED HAH
unicode_decomposition_type(0x1EE49, 0x1EE49, 'Font').	% Lo       ARABIC MATHEMATICAL TAILED YEH
unicode_decomposition_type(0x1EE4B, 0x1EE4B, 'Font').	% Lo       ARABIC MATHEMATICAL TAILED LAM
unicode_decomposition_type(0x1EE4D, 0x1EE4F, 'Font').	% Lo   [3] ARABIC MATHEMATICAL TAILED NOON..ARABIC MATHEMATICAL TAILED AIN
unicode_decomposition_type(0x1EE51, 0x1EE52, 'Font').	% Lo   [2] ARABIC MATHEMATICAL TAILED SAD..ARABIC MATHEMATICAL TAILED QAF
unicode_decomposition_type(0x1EE54, 0x1EE54, 'Font').	% Lo       ARABIC MATHEMATICAL TAILED SHEEN
unicode_decomposition_type(0x1EE57, 0x1EE57, 'Font').	% Lo       ARABIC MATHEMATICAL TAILED KHAH
unicode_decomposition_type(0x1EE59, 0x1EE59, 'Font').	% Lo       ARABIC MATHEMATICAL TAILED DAD
unicode_decomposition_type(0x1EE5B, 0x1EE5B, 'Font').	% Lo       ARABIC MATHEMATICAL TAILED GHAIN
unicode_decomposition_type(0x1EE5D, 0x1EE5D, 'Font').	% Lo       ARABIC MATHEMATICAL TAILED DOTLESS NOON
unicode_decomposition_type(0x1EE5F, 0x1EE5F, 'Font').	% Lo       ARABIC MATHEMATICAL TAILED DOTLESS QAF
unicode_decomposition_type(0x1EE61, 0x1EE62, 'Font').	% Lo   [2] ARABIC MATHEMATICAL STRETCHED BEH..ARABIC MATHEMATICAL STRETCHED JEEM
unicode_decomposition_type(0x1EE64, 0x1EE64, 'Font').	% Lo       ARABIC MATHEMATICAL STRETCHED HEH
unicode_decomposition_type(0x1EE67, 0x1EE6A, 'Font').	% Lo   [4] ARABIC MATHEMATICAL STRETCHED HAH..ARABIC MATHEMATICAL STRETCHED KAF
unicode_decomposition_type(0x1EE6C, 0x1EE72, 'Font').	% Lo   [7] ARABIC MATHEMATICAL STRETCHED MEEM..ARABIC MATHEMATICAL STRETCHED QAF
unicode_decomposition_type(0x1EE74, 0x1EE77, 'Font').	% Lo   [4] ARABIC MATHEMATICAL STRETCHED SHEEN..ARABIC MATHEMATICAL STRETCHED KHAH
unicode_decomposition_type(0x1EE79, 0x1EE7C, 'Font').	% Lo   [4] ARABIC MATHEMATICAL STRETCHED DAD..ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
unicode_decomposition_type(0x1EE7E, 0x1EE7E, 'Font').	% Lo       ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
unicode_decomposition_type(0x1EE80, 0x1EE89, 'Font').	% Lo  [10] ARABIC MATHEMATICAL LOOPED ALEF..ARABIC MATHEMATICAL LOOPED YEH
unicode_decomposition_type(0x1EE8B, 0x1EE9B, 'Font').	% Lo  [17] ARABIC MATHEMATICAL LOOPED LAM..ARABIC MATHEMATICAL LOOPED GHAIN
unicode_decomposition_type(0x1EEA1, 0x1EEA3, 'Font').	% Lo   [3] ARABIC MATHEMATICAL DOUBLE-STRUCK BEH..ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
unicode_decomposition_type(0x1EEA5, 0x1EEA9, 'Font').	% Lo   [5] ARABIC MATHEMATICAL DOUBLE-STRUCK WAW..ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
unicode_decomposition_type(0x1EEAB, 0x1EEBB, 'Font').	% Lo  [17] ARABIC MATHEMATICAL DOUBLE-STRUCK LAM..ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN

% Total code points: 1184

% ================================================

unicode_decomposition_type(0x00A0, 0x00A0, 'Nobreak').	% Zs       NO-BREAK SPACE
unicode_decomposition_type(0x0F0C, 0x0F0C, 'Nobreak').	% Po       TIBETAN MARK DELIMITER TSHEG BSTAR
unicode_decomposition_type(0x2007, 0x2007, 'Nobreak').	% Zs       FIGURE SPACE
unicode_decomposition_type(0x2011, 0x2011, 'Nobreak').	% Pd       NON-BREAKING HYPHEN
unicode_decomposition_type(0x202F, 0x202F, 'Nobreak').	% Zs       NARROW NO-BREAK SPACE

% Total code points: 5

% ================================================

unicode_decomposition_type(0xFB54, 0xFB54, 'Initial').	% Lo       ARABIC LETTER BEEH INITIAL FORM
unicode_decomposition_type(0xFB58, 0xFB58, 'Initial').	% Lo       ARABIC LETTER PEH INITIAL FORM
unicode_decomposition_type(0xFB5C, 0xFB5C, 'Initial').	% Lo       ARABIC LETTER BEHEH INITIAL FORM
unicode_decomposition_type(0xFB60, 0xFB60, 'Initial').	% Lo       ARABIC LETTER TTEHEH INITIAL FORM
unicode_decomposition_type(0xFB64, 0xFB64, 'Initial').	% Lo       ARABIC LETTER TEHEH INITIAL FORM
unicode_decomposition_type(0xFB68, 0xFB68, 'Initial').	% Lo       ARABIC LETTER TTEH INITIAL FORM
unicode_decomposition_type(0xFB6C, 0xFB6C, 'Initial').	% Lo       ARABIC LETTER VEH INITIAL FORM
unicode_decomposition_type(0xFB70, 0xFB70, 'Initial').	% Lo       ARABIC LETTER PEHEH INITIAL FORM
unicode_decomposition_type(0xFB74, 0xFB74, 'Initial').	% Lo       ARABIC LETTER DYEH INITIAL FORM
unicode_decomposition_type(0xFB78, 0xFB78, 'Initial').	% Lo       ARABIC LETTER NYEH INITIAL FORM
unicode_decomposition_type(0xFB7C, 0xFB7C, 'Initial').	% Lo       ARABIC LETTER TCHEH INITIAL FORM
unicode_decomposition_type(0xFB80, 0xFB80, 'Initial').	% Lo       ARABIC LETTER TCHEHEH INITIAL FORM
unicode_decomposition_type(0xFB90, 0xFB90, 'Initial').	% Lo       ARABIC LETTER KEHEH INITIAL FORM
unicode_decomposition_type(0xFB94, 0xFB94, 'Initial').	% Lo       ARABIC LETTER GAF INITIAL FORM
unicode_decomposition_type(0xFB98, 0xFB98, 'Initial').	% Lo       ARABIC LETTER GUEH INITIAL FORM
unicode_decomposition_type(0xFB9C, 0xFB9C, 'Initial').	% Lo       ARABIC LETTER NGOEH INITIAL FORM
unicode_decomposition_type(0xFBA2, 0xFBA2, 'Initial').	% Lo       ARABIC LETTER RNOON INITIAL FORM
unicode_decomposition_type(0xFBA8, 0xFBA8, 'Initial').	% Lo       ARABIC LETTER HEH GOAL INITIAL FORM
unicode_decomposition_type(0xFBAC, 0xFBAC, 'Initial').	% Lo       ARABIC LETTER HEH DOACHASHMEE INITIAL FORM
unicode_decomposition_type(0xFBD5, 0xFBD5, 'Initial').	% Lo       ARABIC LETTER NG INITIAL FORM
unicode_decomposition_type(0xFBE6, 0xFBE6, 'Initial').	% Lo       ARABIC LETTER E INITIAL FORM
unicode_decomposition_type(0xFBE8, 0xFBE8, 'Initial').	% Lo       ARABIC LETTER UIGHUR KAZAKH KIRGHIZ ALEF MAKSURA INITIAL FORM
unicode_decomposition_type(0xFBF8, 0xFBF8, 'Initial').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E INITIAL FORM
unicode_decomposition_type(0xFBFB, 0xFBFB, 'Initial').	% Lo       ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA INITIAL FORM
unicode_decomposition_type(0xFBFE, 0xFBFE, 'Initial').	% Lo       ARABIC LETTER FARSI YEH INITIAL FORM
unicode_decomposition_type(0xFC97, 0xFCDE, 'Initial').	% Lo  [72] ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH JEEM INITIAL FORM..ARABIC LIGATURE YEH WITH HEH INITIAL FORM
unicode_decomposition_type(0xFD2D, 0xFD33, 'Initial').	% Lo   [7] ARABIC LIGATURE SHEEN WITH JEEM INITIAL FORM..ARABIC LIGATURE TAH WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD50, 0xFD50, 'Initial').	% Lo       ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD52, 0xFD57, 'Initial').	% Lo   [6] ARABIC LIGATURE TEH WITH HAH WITH JEEM INITIAL FORM..ARABIC LIGATURE TEH WITH MEEM WITH KHAH INITIAL FORM
unicode_decomposition_type(0xFD59, 0xFD59 , 'Initial').	% Lo       ARABIC LIGATURE JEEM WITH MEEM WITH HAH INITIAL FORM
unicode_decomposition_type(0xFD5C, 0xFD5D, 'Initial').	% Lo   [2] ARABIC LIGATURE SEEN WITH HAH WITH JEEM INITIAL FORM..ARABIC LIGATURE SEEN WITH JEEM WITH HAH INITIAL FORM
unicode_decomposition_type(0xFD60, 0xFD61, 'Initial').	% Lo   [2] ARABIC LIGATURE SEEN WITH MEEM WITH HAH INITIAL FORM..ARABIC LIGATURE SEEN WITH MEEM WITH JEEM INITIAL FORM
unicode_decomposition_type(0xFD63, 0xFD63, 'Initial').	% Lo       ARABIC LIGATURE SEEN WITH MEEM WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD65, 0xFD65, 'Initial').	% Lo       ARABIC LIGATURE SAD WITH HAH WITH HAH INITIAL FORM
unicode_decomposition_type(0xFD68, 0xFD68, 'Initial').	% Lo       ARABIC LIGATURE SHEEN WITH HAH WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD6B, 0xFD6B, 'Initial').	% Lo       ARABIC LIGATURE SHEEN WITH MEEM WITH KHAH INITIAL FORM
unicode_decomposition_type(0xFD6D, 0xFD6D, 'Initial').	% Lo       ARABIC LIGATURE SHEEN WITH MEEM WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD70, 0xFD70, 'Initial').	% Lo       ARABIC LIGATURE DAD WITH KHAH WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD72, 0xFD73, 'Initial').	% Lo   [2] ARABIC LIGATURE TAH WITH MEEM WITH HAH INITIAL FORM..ARABIC LIGATURE TAH WITH MEEM WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD77, 0xFD77, 'Initial').	% Lo       ARABIC LIGATURE AIN WITH MEEM WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD7D, 0xFD7D, 'Initial').	% Lo       ARABIC LIGATURE FEH WITH KHAH WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD83, 0xFD83, 'Initial').	% Lo       ARABIC LIGATURE LAM WITH JEEM WITH JEEM INITIAL FORM
unicode_decomposition_type(0xFD86, 0xFD86, 'Initial').	% Lo       ARABIC LIGATURE LAM WITH KHAH WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD88, 0xFD8A, 'Initial').	% Lo   [3] ARABIC LIGATURE LAM WITH MEEM WITH HAH INITIAL FORM..ARABIC LIGATURE MEEM WITH HAH WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD8C, 0xFD8F, 'Initial').	% Lo   [4] ARABIC LIGATURE MEEM WITH JEEM WITH HAH INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD92, 0xFD95, 'Initial').	% Lo   [4] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH HAH WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD98, 0xFD98, 'Initial').	% Lo       ARABIC LIGATURE NOON WITH JEEM WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFD9D, 0xFD9D, 'Initial').	% Lo       ARABIC LIGATURE YEH WITH MEEM WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFDB4, 0xFDB5, 'Initial').	% Lo   [2] ARABIC LIGATURE QAF WITH MEEM WITH HAH INITIAL FORM..ARABIC LIGATURE LAM WITH HAH WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFDB8, 0xFDB8, 'Initial').	% Lo       ARABIC LIGATURE NOON WITH JEEM WITH HAH INITIAL FORM
unicode_decomposition_type(0xFDBA, 0xFDBA, 'Initial').	% Lo       ARABIC LIGATURE LAM WITH JEEM WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFDC3, 0xFDC5, 'Initial').	% Lo   [3] ARABIC LIGATURE KAF WITH MEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE SAD WITH MEEM WITH MEEM INITIAL FORM
unicode_decomposition_type(0xFE8B, 0xFE8B, 'Initial').	% Lo       ARABIC LETTER YEH WITH HAMZA ABOVE INITIAL FORM
unicode_decomposition_type(0xFE91, 0xFE91, 'Initial').	% Lo       ARABIC LETTER BEH INITIAL FORM
unicode_decomposition_type(0xFE97, 0xFE97, 'Initial').	% Lo       ARABIC LETTER TEH INITIAL FORM
unicode_decomposition_type(0xFE9B, 0xFE9B, 'Initial').	% Lo       ARABIC LETTER THEH INITIAL FORM
unicode_decomposition_type(0xFE9F, 0xFE9F, 'Initial').	% Lo       ARABIC LETTER JEEM INITIAL FORM
unicode_decomposition_type(0xFEA3, 0xFEA3, 'Initial').	% Lo       ARABIC LETTER HAH INITIAL FORM
unicode_decomposition_type(0xFEA7, 0xFEA7, 'Initial').	% Lo       ARABIC LETTER KHAH INITIAL FORM
unicode_decomposition_type(0xFEB3, 0xFEB3, 'Initial').	% Lo       ARABIC LETTER SEEN INITIAL FORM
unicode_decomposition_type(0xFEB7, 0xFEB7, 'Initial').	% Lo       ARABIC LETTER SHEEN INITIAL FORM
unicode_decomposition_type(0xFEBB, 0xFEBB, 'Initial').	% Lo       ARABIC LETTER SAD INITIAL FORM
unicode_decomposition_type(0xFEBF, 0xFEBF, 'Initial').	% Lo       ARABIC LETTER DAD INITIAL FORM
unicode_decomposition_type(0xFEC3, 0xFEC3, 'Initial').	% Lo       ARABIC LETTER TAH INITIAL FORM
unicode_decomposition_type(0xFEC7, 0xFEC7, 'Initial').	% Lo       ARABIC LETTER ZAH INITIAL FORM
unicode_decomposition_type(0xFECB, 0xFECB, 'Initial').	% Lo       ARABIC LETTER AIN INITIAL FORM
unicode_decomposition_type(0xFECF, 0xFECF, 'Initial').	% Lo       ARABIC LETTER GHAIN INITIAL FORM
unicode_decomposition_type(0xFED3, 0xFED3, 'Initial').	% Lo       ARABIC LETTER FEH INITIAL FORM
unicode_decomposition_type(0xFED7, 0xFED7, 'Initial').	% Lo       ARABIC LETTER QAF INITIAL FORM
unicode_decomposition_type(0xFEDB, 0xFEDB, 'Initial').	% Lo       ARABIC LETTER KAF INITIAL FORM
unicode_decomposition_type(0xFEDF, 0xFEDF, 'Initial').	% Lo       ARABIC LETTER LAM INITIAL FORM
unicode_decomposition_type(0xFEE3, 0xFEE3, 'Initial').	% Lo       ARABIC LETTER MEEM INITIAL FORM
unicode_decomposition_type(0xFEE7, 0xFEE7, 'Initial').	% Lo       ARABIC LETTER NOON INITIAL FORM
unicode_decomposition_type(0xFEEB, 0xFEEB, 'Initial').	% Lo       ARABIC LETTER HEH INITIAL FORM
unicode_decomposition_type(0xFEF3, 0xFEF3, 'Initial').	% Lo       ARABIC LETTER YEH INITIAL FORM

% Total code points: 171

% ================================================

unicode_decomposition_type(0xFB55, 0xFB55, 'Medial').	% Lo       ARABIC LETTER BEEH MEDIAL FORM
unicode_decomposition_type(0xFB59, 0xFB59, 'Medial').	% Lo       ARABIC LETTER PEH MEDIAL FORM
unicode_decomposition_type(0xFB5D, 0xFB5D, 'Medial').	% Lo       ARABIC LETTER BEHEH MEDIAL FORM
unicode_decomposition_type(0xFB61, 0xFB61, 'Medial').	% Lo       ARABIC LETTER TTEHEH MEDIAL FORM
unicode_decomposition_type(0xFB65, 0xFB65, 'Medial').	% Lo       ARABIC LETTER TEHEH MEDIAL FORM
unicode_decomposition_type(0xFB69, 0xFB69, 'Medial').	% Lo       ARABIC LETTER TTEH MEDIAL FORM
unicode_decomposition_type(0xFB6D, 0xFB6D, 'Medial').	% Lo       ARABIC LETTER VEH MEDIAL FORM
unicode_decomposition_type(0xFB71, 0xFB71, 'Medial').	% Lo       ARABIC LETTER PEHEH MEDIAL FORM
unicode_decomposition_type(0xFB75, 0xFB75, 'Medial').	% Lo       ARABIC LETTER DYEH MEDIAL FORM
unicode_decomposition_type(0xFB79, 0xFB79, 'Medial').	% Lo       ARABIC LETTER NYEH MEDIAL FORM
unicode_decomposition_type(0xFB7D, 0xFB7D, 'Medial').	% Lo       ARABIC LETTER TCHEH MEDIAL FORM
unicode_decomposition_type(0xFB81, 0xFB81, 'Medial').	% Lo       ARABIC LETTER TCHEHEH MEDIAL FORM
unicode_decomposition_type(0xFB91, 0xFB91, 'Medial').	% Lo       ARABIC LETTER KEHEH MEDIAL FORM
unicode_decomposition_type(0xFB95, 0xFB95, 'Medial').	% Lo       ARABIC LETTER GAF MEDIAL FORM
unicode_decomposition_type(0xFB99, 0xFB99, 'Medial').	% Lo       ARABIC LETTER GUEH MEDIAL FORM
unicode_decomposition_type(0xFB9D, 0xFB9D, 'Medial').	% Lo       ARABIC LETTER NGOEH MEDIAL FORM
unicode_decomposition_type(0xFBA3, 0xFBA3, 'Medial').	% Lo       ARABIC LETTER RNOON MEDIAL FORM
unicode_decomposition_type(0xFBA9, 0xFBA9, 'Medial').	% Lo       ARABIC LETTER HEH GOAL MEDIAL FORM
unicode_decomposition_type(0xFBAD, 0xFBAD, 'Medial').	% Lo       ARABIC LETTER HEH DOACHASHMEE MEDIAL FORM
unicode_decomposition_type(0xFBD6, 0xFBD6, 'Medial').	% Lo       ARABIC LETTER NG MEDIAL FORM
unicode_decomposition_type(0xFBE7, 0xFBE7, 'Medial').	% Lo       ARABIC LETTER E MEDIAL FORM
unicode_decomposition_type(0xFBE9, 0xFBE9, 'Medial').	% Lo       ARABIC LETTER UIGHUR KAZAKH KIRGHIZ ALEF MAKSURA MEDIAL FORM
unicode_decomposition_type(0xFBFF, 0xFBFF, 'Medial').	% Lo       ARABIC LETTER FARSI YEH MEDIAL FORM
unicode_decomposition_type(0xFCDF, 0xFCF4, 'Medial').	% Lo  [22] ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM MEDIAL FORM..ARABIC LIGATURE SHADDA WITH KASRA MEDIAL FORM
unicode_decomposition_type(0xFD34, 0xFD3B, 'Medial').	% Lo   [8] ARABIC LIGATURE SEEN WITH JEEM MEDIAL FORM..ARABIC LIGATURE ZAH WITH MEEM MEDIAL FORM
unicode_decomposition_type(0xFE71, 0xFE71, 'Medial').	% Lo       ARABIC TATWEEL WITH FATHATAN ABOVE
unicode_decomposition_type(0xFE77, 0xFE77, 'Medial').	% Lo       ARABIC FATHA MEDIAL FORM
unicode_decomposition_type(0xFE79, 0xFE79, 'Medial').	% Lo       ARABIC DAMMA MEDIAL FORM
unicode_decomposition_type(0xFE7B, 0xFE7B, 'Medial').	% Lo       ARABIC KASRA MEDIAL FORM
unicode_decomposition_type(0xFE7D, 0xFE7D, 'Medial').	% Lo       ARABIC SHADDA MEDIAL FORM
unicode_decomposition_type(0xFE7F, 0xFE7F, 'Medial').	% Lo       ARABIC SUKUN MEDIAL FORM
unicode_decomposition_type(0xFE8C, 0xFE8C, 'Medial').	% Lo       ARABIC LETTER YEH WITH HAMZA ABOVE MEDIAL FORM
unicode_decomposition_type(0xFE92, 0xFE92, 'Medial').	% Lo       ARABIC LETTER BEH MEDIAL FORM
unicode_decomposition_type(0xFE98, 0xFE98, 'Medial').	% Lo       ARABIC LETTER TEH MEDIAL FORM
unicode_decomposition_type(0xFE9C, 0xFE9C, 'Medial').	% Lo       ARABIC LETTER THEH MEDIAL FORM
unicode_decomposition_type(0xFEA0, 0xFEA0, 'Medial').	% Lo       ARABIC LETTER JEEM MEDIAL FORM
unicode_decomposition_type(0xFEA4, 0xFEA4, 'Medial').	% Lo       ARABIC LETTER HAH MEDIAL FORM
unicode_decomposition_type(0xFEA8, 0xFEA8, 'Medial').	% Lo       ARABIC LETTER KHAH MEDIAL FORM
unicode_decomposition_type(0xFEB4, 0xFEB4, 'Medial').	% Lo       ARABIC LETTER SEEN MEDIAL FORM
unicode_decomposition_type(0xFEB8, 0xFEB8, 'Medial').	% Lo       ARABIC LETTER SHEEN MEDIAL FORM
unicode_decomposition_type(0xFEBC, 0xFEBC, 'Medial').	% Lo       ARABIC LETTER SAD MEDIAL FORM
unicode_decomposition_type(0xFEC0, 0xFEC0, 'Medial').	% Lo       ARABIC LETTER DAD MEDIAL FORM
unicode_decomposition_type(0xFEC4, 0xFEC4, 'Medial').	% Lo       ARABIC LETTER TAH MEDIAL FORM
unicode_decomposition_type(0xFEC8, 0xFEC8, 'Medial').	% Lo       ARABIC LETTER ZAH MEDIAL FORM
unicode_decomposition_type(0xFECC, 0xFECC, 'Medial').	% Lo       ARABIC LETTER AIN MEDIAL FORM
unicode_decomposition_type(0xFED0, 0xFED0, 'Medial').	% Lo       ARABIC LETTER GHAIN MEDIAL FORM
unicode_decomposition_type(0xFED4, 0xFED4, 'Medial').	% Lo       ARABIC LETTER FEH MEDIAL FORM
unicode_decomposition_type(0xFED8, 0xFED8, 'Medial').	% Lo       ARABIC LETTER QAF MEDIAL FORM
unicode_decomposition_type(0xFEDC, 0xFEDC, 'Medial').	% Lo       ARABIC LETTER KAF MEDIAL FORM
unicode_decomposition_type(0xFEE0, 0xFEE0, 'Medial').	% Lo       ARABIC LETTER LAM MEDIAL FORM
unicode_decomposition_type(0xFEE4, 0xFEE4, 'Medial').	% Lo       ARABIC LETTER MEEM MEDIAL FORM
unicode_decomposition_type(0xFEE8, 0xFEE8, 'Medial').	% Lo       ARABIC LETTER NOON MEDIAL FORM
unicode_decomposition_type(0xFEEC, 0xFEEC, 'Medial').	% Lo       ARABIC LETTER HEH MEDIAL FORM
unicode_decomposition_type(0xFEF4, 0xFEF4, 'Medial').	% Lo       ARABIC LETTER YEH MEDIAL FORM

% Total code points: 82

% ================================================

unicode_decomposition_type(0xFB51, 0xFB51, 'Final').	% Lo       ARABIC LETTER ALEF WASLA FINAL FORM
unicode_decomposition_type(0xFB53, 0xFB53, 'Final').	% Lo       ARABIC LETTER BEEH FINAL FORM
unicode_decomposition_type(0xFB57, 0xFB57, 'Final').	% Lo       ARABIC LETTER PEH FINAL FORM
unicode_decomposition_type(0xFB5B, 0xFB5B, 'Final').	% Lo       ARABIC LETTER BEHEH FINAL FORM
unicode_decomposition_type(0xFB5F, 0xFB5F, 'Final').	% Lo       ARABIC LETTER TTEHEH FINAL FORM
unicode_decomposition_type(0xFB63, 0xFB63, 'Final').	% Lo       ARABIC LETTER TEHEH FINAL FORM
unicode_decomposition_type(0xFB67, 0xFB67, 'Final').	% Lo       ARABIC LETTER TTEH FINAL FORM
unicode_decomposition_type(0xFB6B, 0xFB6B, 'Final').	% Lo       ARABIC LETTER VEH FINAL FORM
unicode_decomposition_type(0xFB6F, 0xFB6F, 'Final').	% Lo       ARABIC LETTER PEHEH FINAL FORM
unicode_decomposition_type(0xFB73, 0xFB73, 'Final').	% Lo       ARABIC LETTER DYEH FINAL FORM
unicode_decomposition_type(0xFB77, 0xFB77, 'Final').	% Lo       ARABIC LETTER NYEH FINAL FORM
unicode_decomposition_type(0xFB7B, 0xFB7B, 'Final').	% Lo       ARABIC LETTER TCHEH FINAL FORM
unicode_decomposition_type(0xFB7F, 0xFB7F, 'Final').	% Lo       ARABIC LETTER TCHEHEH FINAL FORM
unicode_decomposition_type(0xFB83, 0xFB83, 'Final').	% Lo       ARABIC LETTER DDAHAL FINAL FORM
unicode_decomposition_type(0xFB85, 0xFB85, 'Final').	% Lo       ARABIC LETTER DAHAL FINAL FORM
unicode_decomposition_type(0xFB87, 0xFB87, 'Final').	% Lo       ARABIC LETTER DUL FINAL FORM
unicode_decomposition_type(0xFB89, 0xFB89, 'Final').	% Lo       ARABIC LETTER DDAL FINAL FORM
unicode_decomposition_type(0xFB8B, 0xFB8B, 'Final').	% Lo       ARABIC LETTER JEH FINAL FORM
unicode_decomposition_type(0xFB8D, 0xFB8D, 'Final').	% Lo       ARABIC LETTER RREH FINAL FORM
unicode_decomposition_type(0xFB8F, 0xFB8F, 'Final').	% Lo       ARABIC LETTER KEHEH FINAL FORM
unicode_decomposition_type(0xFB93, 0xFB93, 'Final').	% Lo       ARABIC LETTER GAF FINAL FORM
unicode_decomposition_type(0xFB97, 0xFB97, 'Final').	% Lo       ARABIC LETTER GUEH FINAL FORM
unicode_decomposition_type(0xFB9B, 0xFB9B, 'Final').	% Lo       ARABIC LETTER NGOEH FINAL FORM
unicode_decomposition_type(0xFB9F, 0xFB9F, 'Final').	% Lo       ARABIC LETTER NOON GHUNNA FINAL FORM
unicode_decomposition_type(0xFBA1, 0xFBA1, 'Final').	% Lo       ARABIC LETTER RNOON FINAL FORM
unicode_decomposition_type(0xFBA5, 0xFBA5, 'Final').	% Lo       ARABIC LETTER HEH WITH YEH ABOVE FINAL FORM
unicode_decomposition_type(0xFBA7, 0xFBA7, 'Final').	% Lo       ARABIC LETTER HEH GOAL FINAL FORM
unicode_decomposition_type(0xFBAB, 0xFBAB, 'Final').	% Lo       ARABIC LETTER HEH DOACHASHMEE FINAL FORM
unicode_decomposition_type(0xFBAF, 0xFBAF, 'Final').	% Lo       ARABIC LETTER YEH BARREE FINAL FORM
unicode_decomposition_type(0xFBB1, 0xFBB1, 'Final').	% Lo       ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
unicode_decomposition_type(0xFBD4, 0xFBD4, 'Final').	% Lo       ARABIC LETTER NG FINAL FORM
unicode_decomposition_type(0xFBD8, 0xFBD8, 'Final').	% Lo       ARABIC LETTER U FINAL FORM
unicode_decomposition_type(0xFBDA, 0xFBDA, 'Final').	% Lo       ARABIC LETTER OE FINAL FORM
unicode_decomposition_type(0xFBDC, 0xFBDC, 'Final').	% Lo       ARABIC LETTER YU FINAL FORM
unicode_decomposition_type(0xFBDF, 0xFBDF, 'Final').	% Lo       ARABIC LETTER VE FINAL FORM
unicode_decomposition_type(0xFBE1, 0xFBE1, 'Final').	% Lo       ARABIC LETTER KIRGHIZ OE FINAL FORM
unicode_decomposition_type(0xFBE3, 0xFBE3, 'Final').	% Lo       ARABIC LETTER KIRGHIZ YU FINAL FORM
unicode_decomposition_type(0xFBE5, 0xFBE5, 'Final').	% Lo       ARABIC LETTER E FINAL FORM
unicode_decomposition_type(0xFBEB, 0xFBEB, 'Final').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF FINAL FORM
unicode_decomposition_type(0xFBED, 0xFBED, 'Final').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH AE FINAL FORM
unicode_decomposition_type(0xFBEF, 0xFBEF, 'Final').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH WAW FINAL FORM
unicode_decomposition_type(0xFBF1, 0xFBF1, 'Final').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH U FINAL FORM
unicode_decomposition_type(0xFBF3, 0xFBF3, 'Final').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH OE FINAL FORM
unicode_decomposition_type(0xFBF5, 0xFBF5, 'Final').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YU FINAL FORM
unicode_decomposition_type(0xFBF7, 0xFBF7, 'Final').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E FINAL FORM
unicode_decomposition_type(0xFBFA, 0xFBFA, 'Final').	% Lo       ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA FINAL FORM
unicode_decomposition_type(0xFBFD, 0xFBFD, 'Final').	% Lo       ARABIC LETTER FARSI YEH FINAL FORM
unicode_decomposition_type(0xFC64, 0xFC96, 'Final').	% Lo  [51] ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH REH FINAL FORM..ARABIC LIGATURE YEH WITH YEH FINAL FORM
unicode_decomposition_type(0xFD11, 0xFD2C, 'Final').	% Lo  [28] ARABIC LIGATURE TAH WITH ALEF MAKSURA FINAL FORM..ARABIC LIGATURE DAD WITH REH FINAL FORM
unicode_decomposition_type(0xFD3C, 0xFD3C, 'Final').	% Lo       ARABIC LIGATURE ALEF WITH FATHATAN FINAL FORM
unicode_decomposition_type(0xFD51, 0xFD51, 'Final').	% Lo       ARABIC LIGATURE TEH WITH HAH WITH JEEM FINAL FORM
unicode_decomposition_type(0xFD58, 0xFD58, 'Final').	% Lo       ARABIC LIGATURE JEEM WITH MEEM WITH HAH FINAL FORM
unicode_decomposition_type(0xFD5A, 0xFD5B, 'Final').	% Lo   [2] ARABIC LIGATURE HAH WITH MEEM WITH YEH FINAL FORM..ARABIC LIGATURE HAH WITH MEEM WITH ALEF MAKSURA FINAL FORM
unicode_decomposition_type(0xFD5E, 0xFD5F, 'Final').	% Lo   [2] ARABIC LIGATURE SEEN WITH JEEM WITH ALEF MAKSURA FINAL FORM..ARABIC LIGATURE SEEN WITH MEEM WITH HAH FINAL FORM
unicode_decomposition_type(0xFD62, 0xFD62, 'Final').	% Lo       ARABIC LIGATURE SEEN WITH MEEM WITH MEEM FINAL FORM
unicode_decomposition_type(0xFD64, 0xFD64, 'Final').	% Lo       ARABIC LIGATURE SAD WITH HAH WITH HAH FINAL FORM
unicode_decomposition_type(0xFD66, 0xFD67, 'Final').	% Lo   [2] ARABIC LIGATURE SAD WITH MEEM WITH MEEM FINAL FORM..ARABIC LIGATURE SHEEN WITH HAH WITH MEEM FINAL FORM
unicode_decomposition_type(0xFD69, 0xFD6A, 'Final').	% Lo   [2] ARABIC LIGATURE SHEEN WITH JEEM WITH YEH FINAL FORM..ARABIC LIGATURE SHEEN WITH MEEM WITH KHAH FINAL FORM
unicode_decomposition_type(0xFD6C, 0xFD6C, 'Final').	% Lo       ARABIC LIGATURE SHEEN WITH MEEM WITH MEEM FINAL FORM
unicode_decomposition_type(0xFD6E, 0xFD6F, 'Final').	% Lo   [2] ARABIC LIGATURE DAD WITH HAH WITH ALEF MAKSURA FINAL FORM..ARABIC LIGATURE DAD WITH KHAH WITH MEEM FINAL FORM
unicode_decomposition_type(0xFD71, 0xFD71, 'Final').	% Lo       ARABIC LIGATURE TAH WITH MEEM WITH HAH FINAL FORM
unicode_decomposition_type(0xFD74, 0xFD76, 'Final').	% Lo   [3] ARABIC LIGATURE TAH WITH MEEM WITH YEH FINAL FORM..ARABIC LIGATURE AIN WITH MEEM WITH MEEM FINAL FORM
unicode_decomposition_type(0xFD78, 0xFD7C, 'Final').	% Lo   [5] ARABIC LIGATURE AIN WITH MEEM WITH ALEF MAKSURA FINAL FORM..ARABIC LIGATURE FEH WITH KHAH WITH MEEM FINAL FORM
unicode_decomposition_type(0xFD7E, 0xFD82, 'Final').	% Lo   [5] ARABIC LIGATURE QAF WITH MEEM WITH HAH FINAL FORM..ARABIC LIGATURE LAM WITH HAH WITH ALEF MAKSURA FINAL FORM
unicode_decomposition_type(0xFD84, 0xFD85, 'Final').	% Lo   [2] ARABIC LIGATURE LAM WITH JEEM WITH JEEM FINAL FORM..ARABIC LIGATURE LAM WITH KHAH WITH MEEM FINAL FORM
unicode_decomposition_type(0xFD87, 0xFD87, 'Final').	% Lo       ARABIC LIGATURE LAM WITH MEEM WITH HAH FINAL FORM
unicode_decomposition_type(0xFD8B, 0xFD8B, 'Final').	% Lo       ARABIC LIGATURE MEEM WITH HAH WITH YEH FINAL FORM
unicode_decomposition_type(0xFD96, 0xFD97, 'Final').	% Lo   [2] ARABIC LIGATURE NOON WITH HAH WITH ALEF MAKSURA FINAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH MEEM FINAL FORM
unicode_decomposition_type(0xFD99, 0xFD9C, 'Final').	% Lo   [4] ARABIC LIGATURE NOON WITH JEEM WITH ALEF MAKSURA FINAL FORM..ARABIC LIGATURE YEH WITH MEEM WITH MEEM FINAL FORM
unicode_decomposition_type(0xFD9E, 0xFDB3, 'Final').	% Lo  [22] ARABIC LIGATURE BEH WITH KHAH WITH YEH FINAL FORM..ARABIC LIGATURE NOON WITH HAH WITH YEH FINAL FORM
unicode_decomposition_type(0xFDB6, 0xFDB7, 'Final').	% Lo   [2] ARABIC LIGATURE AIN WITH MEEM WITH YEH FINAL FORM..ARABIC LIGATURE KAF WITH MEEM WITH YEH FINAL FORM
unicode_decomposition_type(0xFDB9, 0xFDB9, 'Final').	% Lo       ARABIC LIGATURE MEEM WITH KHAH WITH YEH FINAL FORM
unicode_decomposition_type(0xFDBB, 0xFDC2, 'Final').	% Lo   [8] ARABIC LIGATURE KAF WITH MEEM WITH MEEM FINAL FORM..ARABIC LIGATURE BEH WITH HAH WITH YEH FINAL FORM
unicode_decomposition_type(0xFDC6, 0xFDC7, 'Final').	% Lo   [2] ARABIC LIGATURE SEEN WITH KHAH WITH YEH FINAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
unicode_decomposition_type(0xFE82, 0xFE82, 'Final').	% Lo       ARABIC LETTER ALEF WITH MADDA ABOVE FINAL FORM
unicode_decomposition_type(0xFE84, 0xFE84, 'Final').	% Lo       ARABIC LETTER ALEF WITH HAMZA ABOVE FINAL FORM
unicode_decomposition_type(0xFE86, 0xFE86, 'Final').	% Lo       ARABIC LETTER WAW WITH HAMZA ABOVE FINAL FORM
unicode_decomposition_type(0xFE88, 0xFE88, 'Final').	% Lo       ARABIC LETTER ALEF WITH HAMZA BELOW FINAL FORM
unicode_decomposition_type(0xFE8A, 0xFE8A, 'Final').	% Lo       ARABIC LETTER YEH WITH HAMZA ABOVE FINAL FORM
unicode_decomposition_type(0xFE8E, 0xFE8E, 'Final').	% Lo       ARABIC LETTER ALEF FINAL FORM
unicode_decomposition_type(0xFE90, 0xFE90, 'Final').	% Lo       ARABIC LETTER BEH FINAL FORM
unicode_decomposition_type(0xFE94, 0xFE94, 'Final').	% Lo       ARABIC LETTER TEH MARBUTA FINAL FORM
unicode_decomposition_type(0xFE96, 0xFE96, 'Final').	% Lo       ARABIC LETTER TEH FINAL FORM
unicode_decomposition_type(0xFE9A, 0xFE9A, 'Final').	% Lo       ARABIC LETTER THEH FINAL FORM
unicode_decomposition_type(0xFE9E, 0xFE9E, 'Final').	% Lo       ARABIC LETTER JEEM FINAL FORM
unicode_decomposition_type(0xFEA2, 0xFEA2, 'Final').	% Lo       ARABIC LETTER HAH FINAL FORM
unicode_decomposition_type(0xFEA6, 0xFEA6, 'Final').	% Lo       ARABIC LETTER KHAH FINAL FORM
unicode_decomposition_type(0xFEAA, 0xFEAA, 'Final').	% Lo       ARABIC LETTER DAL FINAL FORM
unicode_decomposition_type(0xFEAC, 0xFEAC, 'Final').	% Lo       ARABIC LETTER THAL FINAL FORM
unicode_decomposition_type(0xFEAE, 0xFEAE, 'Final').	% Lo       ARABIC LETTER REH FINAL FORM
unicode_decomposition_type(0xFEB0, 0xFEB0, 'Final').	% Lo       ARABIC LETTER ZAIN FINAL FORM
unicode_decomposition_type(0xFEB2, 0xFEB2, 'Final').	% Lo       ARABIC LETTER SEEN FINAL FORM
unicode_decomposition_type(0xFEB6, 0xFEB6, 'Final').	% Lo       ARABIC LETTER SHEEN FINAL FORM
unicode_decomposition_type(0xFEBA, 0xFEBA, 'Final').	% Lo       ARABIC LETTER SAD FINAL FORM
unicode_decomposition_type(0xFEBE, 0xFEBE, 'Final').	% Lo       ARABIC LETTER DAD FINAL FORM
unicode_decomposition_type(0xFEC2, 0xFEC2, 'Final').	% Lo       ARABIC LETTER TAH FINAL FORM
unicode_decomposition_type(0xFEC6, 0xFEC6, 'Final').	% Lo       ARABIC LETTER ZAH FINAL FORM
unicode_decomposition_type(0xFECA, 0xFECA, 'Final').	% Lo       ARABIC LETTER AIN FINAL FORM
unicode_decomposition_type(0xFECE, 0xFECE, 'Final').	% Lo       ARABIC LETTER GHAIN FINAL FORM
unicode_decomposition_type(0xFED2, 0xFED2, 'Final').	% Lo       ARABIC LETTER FEH FINAL FORM
unicode_decomposition_type(0xFED6, 0xFED6, 'Final').	% Lo       ARABIC LETTER QAF FINAL FORM
unicode_decomposition_type(0xFEDA, 0xFEDA, 'Final').	% Lo       ARABIC LETTER KAF FINAL FORM
unicode_decomposition_type(0xFEDE, 0xFEDE, 'Final').	% Lo       ARABIC LETTER LAM FINAL FORM
unicode_decomposition_type(0xFEE2, 0xFEE2, 'Final').	% Lo       ARABIC LETTER MEEM FINAL FORM
unicode_decomposition_type(0xFEE6, 0xFEE6, 'Final').	% Lo       ARABIC LETTER NOON FINAL FORM
unicode_decomposition_type(0xFEEA, 0xFEEA, 'Final').	% Lo       ARABIC LETTER HEH FINAL FORM
unicode_decomposition_type(0xFEEE, 0xFEEE, 'Final').	% Lo       ARABIC LETTER WAW FINAL FORM
unicode_decomposition_type(0xFEF0, 0xFEF0, 'Final').	% Lo       ARABIC LETTER ALEF MAKSURA FINAL FORM
unicode_decomposition_type(0xFEF2, 0xFEF2, 'Final').	% Lo       ARABIC LETTER YEH FINAL FORM
unicode_decomposition_type(0xFEF6, 0xFEF6, 'Final').	% Lo       ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE FINAL FORM
unicode_decomposition_type(0xFEF8, 0xFEF8, 'Final').	% Lo       ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE FINAL FORM
unicode_decomposition_type(0xFEFA, 0xFEFA, 'Final').	% Lo       ARABIC LIGATURE LAM WITH ALEF WITH HAMZA BELOW FINAL FORM
unicode_decomposition_type(0xFEFC, 0xFEFC, 'Final').	% Lo       ARABIC LIGATURE LAM WITH ALEF FINAL FORM

% Total code points: 240

% ================================================

unicode_decomposition_type(0xFB50, 0xFB50, 'Isolated').	% Lo       ARABIC LETTER ALEF WASLA ISOLATED FORM
unicode_decomposition_type(0xFB52, 0xFB52, 'Isolated').	% Lo       ARABIC LETTER BEEH ISOLATED FORM
unicode_decomposition_type(0xFB56, 0xFB56, 'Isolated').	% Lo       ARABIC LETTER PEH ISOLATED FORM
unicode_decomposition_type(0xFB5A, 0xFB5A, 'Isolated').	% Lo       ARABIC LETTER BEHEH ISOLATED FORM
unicode_decomposition_type(0xFB5E, 0xFB5E, 'Isolated').	% Lo       ARABIC LETTER TTEHEH ISOLATED FORM
unicode_decomposition_type(0xFB62, 0xFB62, 'Isolated').	% Lo       ARABIC LETTER TEHEH ISOLATED FORM
unicode_decomposition_type(0xFB66, 0xFB66, 'Isolated').	% Lo       ARABIC LETTER TTEH ISOLATED FORM
unicode_decomposition_type(0xFB6A, 0xFB6A, 'Isolated').	% Lo       ARABIC LETTER VEH ISOLATED FORM
unicode_decomposition_type(0xFB6E, 0xFB6E, 'Isolated').	% Lo       ARABIC LETTER PEHEH ISOLATED FORM
unicode_decomposition_type(0xFB72, 0xFB72, 'Isolated').	% Lo       ARABIC LETTER DYEH ISOLATED FORM
unicode_decomposition_type(0xFB76, 0xFB76, 'Isolated').	% Lo       ARABIC LETTER NYEH ISOLATED FORM
unicode_decomposition_type(0xFB7A, 0xFB7A, 'Isolated').	% Lo       ARABIC LETTER TCHEH ISOLATED FORM
unicode_decomposition_type(0xFB7E, 0xFB7E, 'Isolated').	% Lo       ARABIC LETTER TCHEHEH ISOLATED FORM
unicode_decomposition_type(0xFB82, 0xFB82, 'Isolated').	% Lo       ARABIC LETTER DDAHAL ISOLATED FORM
unicode_decomposition_type(0xFB84, 0xFB84, 'Isolated').	% Lo       ARABIC LETTER DAHAL ISOLATED FORM
unicode_decomposition_type(0xFB86, 0xFB86, 'Isolated').	% Lo       ARABIC LETTER DUL ISOLATED FORM
unicode_decomposition_type(0xFB88, 0xFB88, 'Isolated').	% Lo       ARABIC LETTER DDAL ISOLATED FORM
unicode_decomposition_type(0xFB8A, 0xFB8A, 'Isolated').	% Lo       ARABIC LETTER JEH ISOLATED FORM
unicode_decomposition_type(0xFB8C, 0xFB8C, 'Isolated').	% Lo       ARABIC LETTER RREH ISOLATED FORM
unicode_decomposition_type(0xFB8E, 0xFB8E, 'Isolated').	% Lo       ARABIC LETTER KEHEH ISOLATED FORM
unicode_decomposition_type(0xFB92, 0xFB92, 'Isolated').	% Lo       ARABIC LETTER GAF ISOLATED FORM
unicode_decomposition_type(0xFB96, 0xFB96, 'Isolated').	% Lo       ARABIC LETTER GUEH ISOLATED FORM
unicode_decomposition_type(0xFB9A, 0xFB9A, 'Isolated').	% Lo       ARABIC LETTER NGOEH ISOLATED FORM
unicode_decomposition_type(0xFB9E, 0xFB9E, 'Isolated').	% Lo       ARABIC LETTER NOON GHUNNA ISOLATED FORM
unicode_decomposition_type(0xFBA0, 0xFBA0, 'Isolated').	% Lo       ARABIC LETTER RNOON ISOLATED FORM
unicode_decomposition_type(0xFBA4, 0xFBA4, 'Isolated').	% Lo       ARABIC LETTER HEH WITH YEH ABOVE ISOLATED FORM
unicode_decomposition_type(0xFBA6, 0xFBA6, 'Isolated').	% Lo       ARABIC LETTER HEH GOAL ISOLATED FORM
unicode_decomposition_type(0xFBAA, 0xFBAA, 'Isolated').	% Lo       ARABIC LETTER HEH DOACHASHMEE ISOLATED FORM
unicode_decomposition_type(0xFBAE, 0xFBAE, 'Isolated').	% Lo       ARABIC LETTER YEH BARREE ISOLATED FORM
unicode_decomposition_type(0xFBB0, 0xFBB0, 'Isolated').	% Lo       ARABIC LETTER YEH BARREE WITH HAMZA ABOVE ISOLATED FORM
unicode_decomposition_type(0xFBD3, 0xFBD3, 'Isolated').	% Lo       ARABIC LETTER NG ISOLATED FORM
unicode_decomposition_type(0xFBD7, 0xFBD7, 'Isolated').	% Lo       ARABIC LETTER U ISOLATED FORM
unicode_decomposition_type(0xFBD9, 0xFBD9, 'Isolated').	% Lo       ARABIC LETTER OE ISOLATED FORM
unicode_decomposition_type(0xFBDB, 0xFBDB, 'Isolated').	% Lo       ARABIC LETTER YU ISOLATED FORM
unicode_decomposition_type(0xFBDD, 0xFBDE, 'Isolated').	% Lo   [2] ARABIC LETTER U WITH HAMZA ABOVE ISOLATED FORM..ARABIC LETTER VE ISOLATED FORM
unicode_decomposition_type(0xFBE0, 0xFBE0, 'Isolated').	% Lo       ARABIC LETTER KIRGHIZ OE ISOLATED FORM
unicode_decomposition_type(0xFBE2, 0xFBE2, 'Isolated').	% Lo       ARABIC LETTER KIRGHIZ YU ISOLATED FORM
unicode_decomposition_type(0xFBE4, 0xFBE4, 'Isolated').	% Lo       ARABIC LETTER E ISOLATED FORM
unicode_decomposition_type(0xFBEA, 0xFBEA, 'Isolated').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF ISOLATED FORM
unicode_decomposition_type(0xFBEC, 0xFBEC, 'Isolated').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH AE ISOLATED FORM
unicode_decomposition_type(0xFBEE, 0xFBEE, 'Isolated').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH WAW ISOLATED FORM
unicode_decomposition_type(0xFBF0, 0xFBF0, 'Isolated').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH U ISOLATED FORM
unicode_decomposition_type(0xFBF2, 0xFBF2, 'Isolated').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH OE ISOLATED FORM
unicode_decomposition_type(0xFBF4, 0xFBF4, 'Isolated').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YU ISOLATED FORM
unicode_decomposition_type(0xFBF6, 0xFBF6, 'Isolated').	% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E ISOLATED FORM
unicode_decomposition_type(0xFBF9, 0xFBF9, 'Isolated').	% Lo       ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA ISOLATED FORM
unicode_decomposition_type(0xFBFC, 0xFBFC, 'Isolated').	% Lo       ARABIC LETTER FARSI YEH ISOLATED FORM
unicode_decomposition_type(0xFC00, 0xFC63, 'Isolated').	% Lo [100] ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH JEEM ISOLATED FORM..ARABIC LIGATURE SHADDA WITH SUPERSCRIPT ALEF ISOLATED FORM
unicode_decomposition_type(0xFCF5, 0xFD10, 'Isolated').	% Lo  [28] ARABIC LIGATURE TAH WITH ALEF MAKSURA ISOLATED FORM..ARABIC LIGATURE DAD WITH REH ISOLATED FORM
unicode_decomposition_type(0xFD3D, 0xFD3D, 'Isolated').	% Lo       ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
unicode_decomposition_type(0xFDF0, 0xFDFB, 'Isolated').	% Lo  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
unicode_decomposition_type(0xFDFC, 0xFDFC, 'Isolated').	% Sc       RIAL SIGN
unicode_decomposition_type(0xFE70, 0xFE70, 'Isolated').	% Lo       ARABIC FATHATAN ISOLATED FORM
unicode_decomposition_type(0xFE72, 0xFE72, 'Isolated').	% Lo       ARABIC DAMMATAN ISOLATED FORM
unicode_decomposition_type(0xFE74, 0xFE74, 'Isolated').	% Lo       ARABIC KASRATAN ISOLATED FORM
unicode_decomposition_type(0xFE76, 0xFE76, 'Isolated').	% Lo       ARABIC FATHA ISOLATED FORM
unicode_decomposition_type(0xFE78, 0xFE78, 'Isolated').	% Lo       ARABIC DAMMA ISOLATED FORM
unicode_decomposition_type(0xFE7A, 0xFE7A, 'Isolated').	% Lo       ARABIC KASRA ISOLATED FORM
unicode_decomposition_type(0xFE7C, 0xFE7C, 'Isolated').	% Lo       ARABIC SHADDA ISOLATED FORM
unicode_decomposition_type(0xFE7E, 0xFE7E, 'Isolated').	% Lo       ARABIC SUKUN ISOLATED FORM
unicode_decomposition_type(0xFE80, 0xFE81, 'Isolated').	% Lo   [2] ARABIC LETTER HAMZA ISOLATED FORM..ARABIC LETTER ALEF WITH MADDA ABOVE ISOLATED FORM
unicode_decomposition_type(0xFE83, 0xFE83, 'Isolated').	% Lo       ARABIC LETTER ALEF WITH HAMZA ABOVE ISOLATED FORM
unicode_decomposition_type(0xFE85, 0xFE85, 'Isolated').	% Lo       ARABIC LETTER WAW WITH HAMZA ABOVE ISOLATED FORM
unicode_decomposition_type(0xFE87, 0xFE87, 'Isolated').	% Lo       ARABIC LETTER ALEF WITH HAMZA BELOW ISOLATED FORM
unicode_decomposition_type(0xFE89, 0xFE89, 'Isolated').	% Lo       ARABIC LETTER YEH WITH HAMZA ABOVE ISOLATED FORM
unicode_decomposition_type(0xFE8D, 0xFE8D, 'Isolated').	% Lo       ARABIC LETTER ALEF ISOLATED FORM
unicode_decomposition_type(0xFE8F, 0xFE8F, 'Isolated').	% Lo       ARABIC LETTER BEH ISOLATED FORM
unicode_decomposition_type(0xFE93, 0xFE93, 'Isolated').	% Lo       ARABIC LETTER TEH MARBUTA ISOLATED FORM
unicode_decomposition_type(0xFE95, 0xFE95, 'Isolated').	% Lo       ARABIC LETTER TEH ISOLATED FORM
unicode_decomposition_type(0xFE99, 0xFE99, 'Isolated').	% Lo       ARABIC LETTER THEH ISOLATED FORM
unicode_decomposition_type(0xFE9D, 0xFE9D, 'Isolated').	% Lo       ARABIC LETTER JEEM ISOLATED FORM
unicode_decomposition_type(0xFEA1, 0xFEA1, 'Isolated').	% Lo       ARABIC LETTER HAH ISOLATED FORM
unicode_decomposition_type(0xFEA5, 0xFEA5, 'Isolated').	% Lo       ARABIC LETTER KHAH ISOLATED FORM
unicode_decomposition_type(0xFEA9, 0xFEA9, 'Isolated').	% Lo       ARABIC LETTER DAL ISOLATED FORM
unicode_decomposition_type(0xFEAB, 0xFEAB, 'Isolated').	% Lo       ARABIC LETTER THAL ISOLATED FORM
unicode_decomposition_type(0xFEAD, 0xFEAD, 'Isolated').	% Lo       ARABIC LETTER REH ISOLATED FORM
unicode_decomposition_type(0xFEAF, 0xFEAF, 'Isolated').	% Lo       ARABIC LETTER ZAIN ISOLATED FORM
unicode_decomposition_type(0xFEB1, 0xFEB1, 'Isolated').	% Lo       ARABIC LETTER SEEN ISOLATED FORM
unicode_decomposition_type(0xFEB5, 0xFEB5, 'Isolated').	% Lo       ARABIC LETTER SHEEN ISOLATED FORM
unicode_decomposition_type(0xFEB9, 0xFEB9, 'Isolated').	% Lo       ARABIC LETTER SAD ISOLATED FORM
unicode_decomposition_type(0xFEBD, 0xFEBD, 'Isolated').	% Lo       ARABIC LETTER DAD ISOLATED FORM
unicode_decomposition_type(0xFEC1, 0xFEC1, 'Isolated').	% Lo       ARABIC LETTER TAH ISOLATED FORM
unicode_decomposition_type(0xFEC5, 0xFEC5, 'Isolated').	% Lo       ARABIC LETTER ZAH ISOLATED FORM
unicode_decomposition_type(0xFEC9, 0xFEC9, 'Isolated').	% Lo       ARABIC LETTER AIN ISOLATED FORM
unicode_decomposition_type(0xFECD, 0xFECD, 'Isolated').	% Lo       ARABIC LETTER GHAIN ISOLATED FORM
unicode_decomposition_type(0xFED1, 0xFED1, 'Isolated').	% Lo       ARABIC LETTER FEH ISOLATED FORM
unicode_decomposition_type(0xFED5, 0xFED5, 'Isolated').	% Lo       ARABIC LETTER QAF ISOLATED FORM
unicode_decomposition_type(0xFED9, 0xFED9, 'Isolated').	% Lo       ARABIC LETTER KAF ISOLATED FORM
unicode_decomposition_type(0xFEDD, 0xFEDD, 'Isolated').	% Lo       ARABIC LETTER LAM ISOLATED FORM
unicode_decomposition_type(0xFEE1, 0xFEE1, 'Isolated').	% Lo       ARABIC LETTER MEEM ISOLATED FORM
unicode_decomposition_type(0xFEE5, 0xFEE5, 'Isolated').	% Lo       ARABIC LETTER NOON ISOLATED FORM
unicode_decomposition_type(0xFEE9, 0xFEE9, 'Isolated').	% Lo       ARABIC LETTER HEH ISOLATED FORM
unicode_decomposition_type(0xFEED, 0xFEED, 'Isolated').	% Lo       ARABIC LETTER WAW ISOLATED FORM
unicode_decomposition_type(0xFEEF, 0xFEEF, 'Isolated').	% Lo       ARABIC LETTER ALEF MAKSURA ISOLATED FORM
unicode_decomposition_type(0xFEF1, 0xFEF1, 'Isolated').	% Lo       ARABIC LETTER YEH ISOLATED FORM
unicode_decomposition_type(0xFEF5, 0xFEF5, 'Isolated').	% Lo       ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE ISOLATED FORM
unicode_decomposition_type(0xFEF7, 0xFEF7, 'Isolated').	% Lo       ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE ISOLATED FORM
unicode_decomposition_type(0xFEF9, 0xFEF9, 'Isolated').	% Lo       ARABIC LIGATURE LAM WITH ALEF WITH HAMZA BELOW ISOLATED FORM
unicode_decomposition_type(0xFEFB, 0xFEFB, 'Isolated').	% Lo       ARABIC LIGATURE LAM WITH ALEF ISOLATED FORM

% Total code points: 238

% ================================================

unicode_decomposition_type(0x2460, 0x2473, 'Circle').	% No  [20] CIRCLED DIGIT ONE..CIRCLED NUMBER TWENTY
unicode_decomposition_type(0x24B6, 0x24E9, 'Circle').	% So  [52] CIRCLED LATIN CAPITAL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_decomposition_type(0x24EA, 0x24EA, 'Circle').	% No       CIRCLED DIGIT ZERO
unicode_decomposition_type(0x3244, 0x3247, 'Circle').	% So   [4] CIRCLED IDEOGRAPH QUESTION..CIRCLED IDEOGRAPH KOTO
unicode_decomposition_type(0x3251, 0x325F, 'Circle').	% No  [15] CIRCLED NUMBER TWENTY ONE..CIRCLED NUMBER THIRTY FIVE
unicode_decomposition_type(0x3260, 0x327E, 'Circle').	% So  [31] CIRCLED HANGUL KIYEOK..CIRCLED HANGUL IEUNG U
unicode_decomposition_type(0x3280, 0x3289, 'Circle').	% No  [10] CIRCLED IDEOGRAPH ONE..CIRCLED IDEOGRAPH TEN
unicode_decomposition_type(0x328A, 0x32B0, 'Circle').	% So  [39] CIRCLED IDEOGRAPH MOON..CIRCLED IDEOGRAPH NIGHT
unicode_decomposition_type(0x32B1, 0x32BF, 'Circle').	% No  [15] CIRCLED NUMBER THIRTY SIX..CIRCLED NUMBER FIFTY
unicode_decomposition_type(0x32D0, 0x32FE, 'Circle').	% So  [47] CIRCLED KATAKANA A..CIRCLED KATAKANA WO
unicode_decomposition_type(0x1F12B, 0x1F12E, 'Circle').	% So   [4] CIRCLED ITALIC LATIN CAPITAL LETTER C..CIRCLED WZ
unicode_decomposition_type(0x1F250, 0x1F251, 'Circle').	% So   [2] CIRCLED IDEOGRAPH ADVANTAGE..CIRCLED IDEOGRAPH ACCEPT

% Total code points: 240

% ================================================

unicode_decomposition_type(0x00AA, 0x00AA, 'Super').	% Lo       FEMININE ORDINAL INDICATOR
unicode_decomposition_type(0x00B2, 0x00B3, 'Super').	% No   [2] SUPERSCRIPT TWO..SUPERSCRIPT THREE
unicode_decomposition_type(0x00B9, 0x00B9, 'Super').	% No       SUPERSCRIPT ONE
unicode_decomposition_type(0x00BA, 0x00BA, 'Super').	% Lo       MASCULINE ORDINAL INDICATOR
unicode_decomposition_type(0x02B0, 0x02B8, 'Super').	% Lm   [9] MODIFIER LETTER SMALL H..MODIFIER LETTER SMALL Y
unicode_decomposition_type(0x02E0, 0x02E4, 'Super').	% Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_decomposition_type(0x10FC, 0x10FC, 'Super').	% Lm       MODIFIER LETTER GEORGIAN NAR
unicode_decomposition_type(0x1D2C, 0x1D2E, 'Super').	% Lm   [3] MODIFIER LETTER CAPITAL A..MODIFIER LETTER CAPITAL B
unicode_decomposition_type(0x1D30, 0x1D3A, 'Super').	% Lm  [11] MODIFIER LETTER CAPITAL D..MODIFIER LETTER CAPITAL N
unicode_decomposition_type(0x1D3C, 0x1D4D, 'Super').	% Lm  [18] MODIFIER LETTER CAPITAL O..MODIFIER LETTER SMALL G
unicode_decomposition_type(0x1D4F, 0x1D61, 'Super').	% Lm  [19] MODIFIER LETTER SMALL K..MODIFIER LETTER SMALL CHI
unicode_decomposition_type(0x1D78, 0x1D78, 'Super').	% Lm       MODIFIER LETTER CYRILLIC EN
unicode_decomposition_type(0x1D9B, 0x1DBF, 'Super').	% Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_decomposition_type(0x2070, 0x2070, 'Super').	% No       SUPERSCRIPT ZERO
unicode_decomposition_type(0x2071, 0x2071, 'Super').	% Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_decomposition_type(0x2074, 0x2079, 'Super').	% No   [6] SUPERSCRIPT FOUR..SUPERSCRIPT NINE
unicode_decomposition_type(0x207A, 0x207C, 'Super').	% Sm   [3] SUPERSCRIPT PLUS SIGN..SUPERSCRIPT EQUALS SIGN
unicode_decomposition_type(0x207D, 0x207D, 'Super').	% Ps       SUPERSCRIPT LEFT PARENTHESIS
unicode_decomposition_type(0x207E, 0x207E, 'Super').	% Pe       SUPERSCRIPT RIGHT PARENTHESIS
unicode_decomposition_type(0x207F, 0x207F, 'Super').	% Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_decomposition_type(0x2120, 0x2120, 'Super').	% So       SERVICE MARK
unicode_decomposition_type(0x2122, 0x2122, 'Super').	% So       TRADE MARK SIGN
unicode_decomposition_type(0x2C7D, 0x2C7D, 'Super').	% Lm       MODIFIER LETTER CAPITAL V
unicode_decomposition_type(0x2D6F, 0x2D6F, 'Super').	% Lm       TIFINAGH MODIFIER LETTER LABIALIZATION MARK
unicode_decomposition_type(0x3192, 0x3195, 'Super').	% No   [4] IDEOGRAPHIC ANNOTATION ONE MARK..IDEOGRAPHIC ANNOTATION FOUR MARK
unicode_decomposition_type(0x3196, 0x319F, 'Super').	% So  [10] IDEOGRAPHIC ANNOTATION TOP MARK..IDEOGRAPHIC ANNOTATION MAN MARK
unicode_decomposition_type(0xA770, 0xA770, 'Super').	% Lm       MODIFIER LETTER US
unicode_decomposition_type(0xA7F8, 0xA7F9, 'Super').	% Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_decomposition_type(0x1F16A, 0x1F16B, 'Super').	% So   [2] RAISED MC SIGN..RAISED MD SIGN

% Total code points: 146

% ================================================

unicode_decomposition_type(0x1D62, 0x1D6A, 'Sub').	% Lm   [9] LATIN SUBSCRIPT SMALL LETTER I..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_decomposition_type(0x2080, 0x2089, 'Sub').	% No  [10] SUBSCRIPT ZERO..SUBSCRIPT NINE
unicode_decomposition_type(0x208A, 0x208C, 'Sub').	% Sm   [3] SUBSCRIPT PLUS SIGN..SUBSCRIPT EQUALS SIGN
unicode_decomposition_type(0x208D, 0x208D, 'Sub').	% Ps       SUBSCRIPT LEFT PARENTHESIS
unicode_decomposition_type(0x208E, 0x208E, 'Sub').	% Pe       SUBSCRIPT RIGHT PARENTHESIS
unicode_decomposition_type(0x2090, 0x209C, 'Sub').	% Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_decomposition_type(0x2C7C, 0x2C7C, 'Sub').	% Lm       LATIN SUBSCRIPT SMALL LETTER J

% Total code points: 38

% ================================================

unicode_decomposition_type(0x309F, 0x309F, 'Vertical').	% Lo       HIRAGANA DIGRAPH YORI
unicode_decomposition_type(0x30FF, 0x30FF, 'Vertical').	% Lo       KATAKANA DIGRAPH KOTO
unicode_decomposition_type(0xFE10, 0xFE16, 'Vertical').	% Po   [7] PRESENTATION FORM FOR VERTICAL COMMA..PRESENTATION FORM FOR VERTICAL QUESTION MARK
unicode_decomposition_type(0xFE17, 0xFE17, 'Vertical').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE LENTICULAR BRACKET
unicode_decomposition_type(0xFE18, 0xFE18, 'Vertical').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET
unicode_decomposition_type(0xFE19, 0xFE19, 'Vertical').	% Po       PRESENTATION FORM FOR VERTICAL HORIZONTAL ELLIPSIS
unicode_decomposition_type(0xFE30, 0xFE30, 'Vertical').	% Po       PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
unicode_decomposition_type(0xFE31, 0xFE32, 'Vertical').	% Pd   [2] PRESENTATION FORM FOR VERTICAL EM DASH..PRESENTATION FORM FOR VERTICAL EN DASH
unicode_decomposition_type(0xFE33, 0xFE34, 'Vertical').	% Pc   [2] PRESENTATION FORM FOR VERTICAL LOW LINE..PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
unicode_decomposition_type(0xFE35, 0xFE35, 'Vertical').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
unicode_decomposition_type(0xFE36, 0xFE36, 'Vertical').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
unicode_decomposition_type(0xFE37, 0xFE37, 'Vertical').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
unicode_decomposition_type(0xFE38, 0xFE38, 'Vertical').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
unicode_decomposition_type(0xFE39, 0xFE39, 'Vertical').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
unicode_decomposition_type(0xFE3A, 0xFE3A, 'Vertical').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
unicode_decomposition_type(0xFE3B, 0xFE3B, 'Vertical').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
unicode_decomposition_type(0xFE3C, 0xFE3C, 'Vertical').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
unicode_decomposition_type(0xFE3D, 0xFE3D, 'Vertical').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
unicode_decomposition_type(0xFE3E, 0xFE3E, 'Vertical').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
unicode_decomposition_type(0xFE3F, 0xFE3F, 'Vertical').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
unicode_decomposition_type(0xFE40, 0xFE40, 'Vertical').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
unicode_decomposition_type(0xFE41, 0xFE41, 'Vertical').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
unicode_decomposition_type(0xFE42, 0xFE42, 'Vertical').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
unicode_decomposition_type(0xFE43, 0xFE43, 'Vertical').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
unicode_decomposition_type(0xFE44, 0xFE44, 'Vertical').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
unicode_decomposition_type(0xFE47, 0xFE47, 'Vertical').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT SQUARE BRACKET
unicode_decomposition_type(0xFE48, 0xFE48, 'Vertical').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT SQUARE BRACKET

% Total code points: 35

% ================================================

unicode_decomposition_type(0x3000, 0x3000, 'Wide').	% Zs       IDEOGRAPHIC SPACE
unicode_decomposition_type(0xFF01, 0xFF03, 'Wide').	% Po   [3] FULLWIDTH EXCLAMATION MARK..FULLWIDTH NUMBER SIGN
unicode_decomposition_type(0xFF04, 0xFF04, 'Wide').	% Sc       FULLWIDTH DOLLAR SIGN
unicode_decomposition_type(0xFF05, 0xFF07, 'Wide').	% Po   [3] FULLWIDTH PERCENT SIGN..FULLWIDTH APOSTROPHE
unicode_decomposition_type(0xFF08, 0xFF08, 'Wide').	% Ps       FULLWIDTH LEFT PARENTHESIS
unicode_decomposition_type(0xFF09, 0xFF09, 'Wide').	% Pe       FULLWIDTH RIGHT PARENTHESIS
unicode_decomposition_type(0xFF0A, 0xFF0A, 'Wide').	% Po       FULLWIDTH ASTERISK
unicode_decomposition_type(0xFF0B, 0xFF0B, 'Wide').	% Sm       FULLWIDTH PLUS SIGN
unicode_decomposition_type(0xFF0C, 0xFF0C, 'Wide').	% Po       FULLWIDTH COMMA
unicode_decomposition_type(0xFF0D, 0xFF0D, 'Wide').	% Pd       FULLWIDTH HYPHEN-MINUS
unicode_decomposition_type(0xFF0E, 0xFF0F, 'Wide').	% Po   [2] FULLWIDTH FULL STOP..FULLWIDTH SOLIDUS
unicode_decomposition_type(0xFF10, 0xFF19, 'Wide').	% Nd  [10] FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
unicode_decomposition_type(0xFF1A, 0xFF1B, 'Wide').	% Po   [2] FULLWIDTH COLON..FULLWIDTH SEMICOLON
unicode_decomposition_type(0xFF1C, 0xFF1E, 'Wide').	% Sm   [3] FULLWIDTH LESS-THAN SIGN..FULLWIDTH GREATER-THAN SIGN
unicode_decomposition_type(0xFF1F, 0xFF20, 'Wide').	% Po   [2] FULLWIDTH QUESTION MARK..FULLWIDTH COMMERCIAL AT
unicode_decomposition_type(0xFF21, 0xFF3A, 'Wide').	% L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_decomposition_type(0xFF3B, 0xFF3B, 'Wide').	% Ps       FULLWIDTH LEFT SQUARE BRACKET
unicode_decomposition_type(0xFF3C, 0xFF3C, 'Wide').	% Po       FULLWIDTH REVERSE SOLIDUS
unicode_decomposition_type(0xFF3D, 0xFF3D, 'Wide').	% Pe       FULLWIDTH RIGHT SQUARE BRACKET
unicode_decomposition_type(0xFF3E, 0xFF3E, 'Wide').	% Sk       FULLWIDTH CIRCUMFLEX ACCENT
unicode_decomposition_type(0xFF3F, 0xFF3F, 'Wide').	% Pc       FULLWIDTH LOW LINE
unicode_decomposition_type(0xFF40, 0xFF40, 'Wide').	% Sk       FULLWIDTH GRAVE ACCENT
unicode_decomposition_type(0xFF41, 0xFF5A, 'Wide').	% L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_decomposition_type(0xFF5B, 0xFF5B, 'Wide').	% Ps       FULLWIDTH LEFT CURLY BRACKET
unicode_decomposition_type(0xFF5C, 0xFF5C, 'Wide').	% Sm       FULLWIDTH VERTICAL LINE
unicode_decomposition_type(0xFF5D, 0xFF5D, 'Wide').	% Pe       FULLWIDTH RIGHT CURLY BRACKET
unicode_decomposition_type(0xFF5E, 0xFF5E, 'Wide').	% Sm       FULLWIDTH TILDE
unicode_decomposition_type(0xFF5F, 0xFF5F, 'Wide').	% Ps       FULLWIDTH LEFT WHITE PARENTHESIS
unicode_decomposition_type(0xFF60, 0xFF60, 'Wide').	% Pe       FULLWIDTH RIGHT WHITE PARENTHESIS
unicode_decomposition_type(0xFFE0, 0xFFE1, 'Wide').	% Sc   [2] FULLWIDTH CENT SIGN..FULLWIDTH POUND SIGN
unicode_decomposition_type(0xFFE2, 0xFFE2, 'Wide').	% Sm       FULLWIDTH NOT SIGN
unicode_decomposition_type(0xFFE3, 0xFFE3, 'Wide').	% Sk       FULLWIDTH MACRON
unicode_decomposition_type(0xFFE4, 0xFFE4, 'Wide').	% So       FULLWIDTH BROKEN BAR
unicode_decomposition_type(0xFFE5, 0xFFE6, 'Wide').	% Sc   [2] FULLWIDTH YEN SIGN..FULLWIDTH WON SIGN

% Total code points: 104

% ================================================

unicode_decomposition_type(0xFF61, 0xFF61, 'Narrow').	% Po       HALFWIDTH IDEOGRAPHIC FULL STOP
unicode_decomposition_type(0xFF62, 0xFF62, 'Narrow').	% Ps       HALFWIDTH LEFT CORNER BRACKET
unicode_decomposition_type(0xFF63, 0xFF63, 'Narrow').	% Pe       HALFWIDTH RIGHT CORNER BRACKET
unicode_decomposition_type(0xFF64, 0xFF65, 'Narrow').	% Po   [2] HALFWIDTH IDEOGRAPHIC COMMA..HALFWIDTH KATAKANA MIDDLE DOT
unicode_decomposition_type(0xFF66, 0xFF6F, 'Narrow').	% Lo  [10] HALFWIDTH KATAKANA LETTER WO..HALFWIDTH KATAKANA LETTER SMALL TU
unicode_decomposition_type(0xFF70, 0xFF70, 'Narrow').	% Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_decomposition_type(0xFF71, 0xFF9D, 'Narrow').	% Lo  [45] HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N
unicode_decomposition_type(0xFF9E, 0xFF9F, 'Narrow').	% Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
unicode_decomposition_type(0xFFA0, 0xFFBE, 'Narrow').	% Lo  [31] HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH
unicode_decomposition_type(0xFFC2, 0xFFC7, 'Narrow').	% Lo   [6] HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E
unicode_decomposition_type(0xFFCA, 0xFFCF, 'Narrow').	% Lo   [6] HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE
unicode_decomposition_type(0xFFD2, 0xFFD7, 'Narrow').	% Lo   [6] HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU
unicode_decomposition_type(0xFFDA, 0xFFDC, 'Narrow').	% Lo   [3] HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I
unicode_decomposition_type(0xFFE8, 0xFFE8, 'Narrow').	% So       HALFWIDTH FORMS LIGHT VERTICAL
unicode_decomposition_type(0xFFE9, 0xFFEC, 'Narrow').	% Sm   [4] HALFWIDTH LEFTWARDS ARROW..HALFWIDTH DOWNWARDS ARROW
unicode_decomposition_type(0xFFED, 0xFFEE, 'Narrow').	% So   [2] HALFWIDTH BLACK SQUARE..HALFWIDTH WHITE CIRCLE

% Total code points: 122

% ================================================

unicode_decomposition_type(0xFE50, 0xFE52, 'Small').	% Po   [3] SMALL COMMA..SMALL FULL STOP
unicode_decomposition_type(0xFE54, 0xFE57, 'Small').	% Po   [4] SMALL SEMICOLON..SMALL EXCLAMATION MARK
unicode_decomposition_type(0xFE58, 0xFE58, 'Small').	% Pd       SMALL EM DASH
unicode_decomposition_type(0xFE59, 0xFE59, 'Small').	% Ps       SMALL LEFT PARENTHESIS
unicode_decomposition_type(0xFE5A, 0xFE5A, 'Small').	% Pe       SMALL RIGHT PARENTHESIS
unicode_decomposition_type(0xFE5B, 0xFE5B, 'Small').	% Ps       SMALL LEFT CURLY BRACKET
unicode_decomposition_type(0xFE5C, 0xFE5C, 'Small').	% Pe       SMALL RIGHT CURLY BRACKET
unicode_decomposition_type(0xFE5D, 0xFE5D, 'Small').	% Ps       SMALL LEFT TORTOISE SHELL BRACKET
unicode_decomposition_type(0xFE5E, 0xFE5E, 'Small').	% Pe       SMALL RIGHT TORTOISE SHELL BRACKET
unicode_decomposition_type(0xFE5F, 0xFE61, 'Small').	% Po   [3] SMALL NUMBER SIGN..SMALL ASTERISK
unicode_decomposition_type(0xFE62, 0xFE62, 'Small').	% Sm       SMALL PLUS SIGN
unicode_decomposition_type(0xFE63, 0xFE63, 'Small').	% Pd       SMALL HYPHEN-MINUS
unicode_decomposition_type(0xFE64, 0xFE66, 'Small').	% Sm   [3] SMALL LESS-THAN SIGN..SMALL EQUALS SIGN
unicode_decomposition_type(0xFE68, 0xFE68, 'Small').	% Po       SMALL REVERSE SOLIDUS
unicode_decomposition_type(0xFE69, 0xFE69, 'Small').	% Sc       SMALL DOLLAR SIGN
unicode_decomposition_type(0xFE6A, 0xFE6B, 'Small').	% Po   [2] SMALL PERCENT SIGN..SMALL COMMERCIAL AT

% Total code points: 26

% ================================================

unicode_decomposition_type(0x3250, 0x3250, 'Square').	% So       PARTNERSHIP SIGN
unicode_decomposition_type(0x32CC, 0x32CF, 'Square').	% So   [4] SQUARE HG..LIMITED LIABILITY SIGN
unicode_decomposition_type(0x3300, 0x3357, 'Square').	% So  [88] SQUARE APAATO..SQUARE WATTO
unicode_decomposition_type(0x3371, 0x33DF, 'Square').	% So [111] SQUARE HPA..SQUARE A OVER M
unicode_decomposition_type(0x33FF, 0x33FF, 'Square').	% So       SQUARE GAL
unicode_decomposition_type(0x1F130, 0x1F14F, 'Square').	% So  [32] SQUARED LATIN CAPITAL LETTER A..SQUARED WC
unicode_decomposition_type(0x1F190, 0x1F190, 'Square').	% So       SQUARE DJ
unicode_decomposition_type(0x1F200, 0x1F202, 'Square').	% So   [3] SQUARE HIRAGANA HOKA..SQUARED KATAKANA SA
unicode_decomposition_type(0x1F210, 0x1F23A, 'Square').	% So  [43] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-55B6

% Total code points: 284

% ================================================

unicode_decomposition_type(0x00BC, 0x00BE, 'Fraction').	% No   [3] VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS
unicode_decomposition_type(0x2150, 0x215F, 'Fraction').	% No  [16] VULGAR FRACTION ONE SEVENTH..FRACTION NUMERATOR ONE
unicode_decomposition_type(0x2189, 0x2189, 'Fraction').	% No       VULGAR FRACTION ZERO THIRDS

% Total code points: 20

% EOF
