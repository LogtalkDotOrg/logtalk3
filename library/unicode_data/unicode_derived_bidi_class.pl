%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: September 30, 2012
%
%  Original Unicode file header comments follow

/*
# DerivedBidiClass-6.2.0.txt
# Date: 2012-05-20, 00:42:30 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2012 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

# ================================================

# Bidi Class (listing UnicodeData.txt, field 4: see UAX #44: http://www.unicode.org/reports/tr44/)
# Unlike other properties, unassigned code points in blocks
# reserved for right-to-left scripts are given either types R or AL.
#
# The unassigned code points that default to AL are in the ranges:
#     [\u0600-\u07BF \u08A0-\u08FF \uFB50-\uFDCF \uFDF0-\uFDFF \uFE70-\uFEFF \U0001EE00-\U0001EEFF]
#
#     Arabic:            U+0600  -  U+06FF
#     Syriac:            U+0700  -  U+074F
#     Arabic_Supplement: U+0750  -  U+077F
#     Thaana:            U+0780  -  U+07BF
#     Arabic Extended-A: U+08A0  -  U+08FF
#     Arabic_Presentation_Forms_A:
#                        U+FB50  -  U+FDCF
#                        U+FDF0  -  U+FDFF
#     Arabic_Presentation_Forms_B:
#                        U+FE70  -  U+FEFF
#     Arabic Mathematical Alphabetic Symbols:
#                       U+1EE00  - U+1EEFF
#
# The unassigned code points that default to R are in the ranges:
#     [\u0590-\u05FF \u07C0-\u089F \uFB1D-\uFB4F \U00010800-\U00010FFF \U0001E800-\U0001EDFF \U0001EF00-\U0001EFFF]
#
#     Hebrew:            U+0590  -  U+05FF
#     NKo:               U+07C0  -  U+07FF
#     Cypriot_Syllabary: U+10800 - U+1083F
#     Phoenician:        U+10900 - U+1091F
#     Lydian:            U+10920 - U+1093F
#     Meroitic Hieroglyphs:
#                        U+10980 - U+1099F
#     Meroitic Cursive:  U+109A0 - U+109FF
#     Kharoshthi:        U+10A00 - U+10A5F
#     and any others in the ranges:
#                        U+0800  -  U+089F,
#                        U+FB1D  -  U+FB4F,
#                        U+10840 - U+10FFF,
#                        U+1E800 - U+1EDFF,
#                        U+1EF00 - U+1EFFF
#
# For all other cases:

#  All code points not explicitly listed for Bidi_Class
#  have the value Left_To_Right (L).

# @missing: 0000..10FFFF; Left_To_Right

# ================================================
*/

unicode_bidi_class(CodePoint, Class) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_bidi_class(CodePointStart, CodePointEnd, Class),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_bidi_class(CodePoint, _, CodePointClass) ->
		Class = CodePointClass
	;	% look for a code point range that includes the given code point
		unicode_bidi_class(CodePointStart, CodePointEnd, CodePointClass),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Class = CodePointClass
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Class = 'Left_To_Right'
	).

% Bidi_Class=Left_To_Right

unicode_bidi_class(0x0041, 0x005A, 'L'). % L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
unicode_bidi_class(0x0061, 0x007A, 'L'). % L&  [26] LATIN SMALL LETTER A..LATIN SMALL LETTER Z
unicode_bidi_class(0x00AA, 0x00AA, 'L'). % Lo       FEMININE ORDINAL INDICATOR
unicode_bidi_class(0x00B5, 0x00B5, 'L'). % L&       MICRO SIGN
unicode_bidi_class(0x00BA, 0x00BA, 'L'). % Lo       MASCULINE ORDINAL INDICATOR
unicode_bidi_class(0x00C0, 0x00D6, 'L'). % L&  [23] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_bidi_class(0x00D8, 0x00F6, 'L'). % L&  [31] LATIN CAPITAL LETTER O WITH STROKE..LATIN SMALL LETTER O WITH DIAERESIS
unicode_bidi_class(0x00F8, 0x01BA, 'L'). % L& [195] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER EZH WITH TAIL
unicode_bidi_class(0x01BB, 0x01BB, 'L'). % Lo       LATIN LETTER TWO WITH STROKE
unicode_bidi_class(0x01BC, 0x01BF, 'L'). % L&   [4] LATIN CAPITAL LETTER TONE FIVE..LATIN LETTER WYNN
unicode_bidi_class(0x01C0, 0x01C3, 'L'). % Lo   [4] LATIN LETTER DENTAL CLICK..LATIN LETTER RETROFLEX CLICK
unicode_bidi_class(0x01C4, 0x0293, 'L'). % L& [208] LATIN CAPITAL LETTER DZ WITH CARON..LATIN SMALL LETTER EZH WITH CURL
unicode_bidi_class(0x0294, 0x0294, 'L'). % Lo       LATIN LETTER GLOTTAL STOP
unicode_bidi_class(0x0295, 0x02AF, 'L'). % L&  [27] LATIN LETTER PHARYNGEAL VOICED FRICATIVE..LATIN SMALL LETTER TURNED H WITH FISHHOOK AND TAIL
unicode_bidi_class(0x02B0, 0x02B8, 'L'). % Lm   [9] MODIFIER LETTER SMALL H..MODIFIER LETTER SMALL Y
unicode_bidi_class(0x02BB, 0x02C1, 'L'). % Lm   [7] MODIFIER LETTER TURNED COMMA..MODIFIER LETTER REVERSED GLOTTAL STOP
unicode_bidi_class(0x02D0, 0x02D1, 'L'). % Lm   [2] MODIFIER LETTER TRIANGULAR COLON..MODIFIER LETTER HALF TRIANGULAR COLON
unicode_bidi_class(0x02E0, 0x02E4, 'L'). % Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_bidi_class(0x02EE, 0x02EE, 'L'). % Lm       MODIFIER LETTER DOUBLE APOSTROPHE
unicode_bidi_class(0x0370, 0x0373, 'L'). % L&   [4] GREEK CAPITAL LETTER HETA..GREEK SMALL LETTER ARCHAIC SAMPI
unicode_bidi_class(0x0376, 0x0377, 'L'). % L&   [2] GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA..GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
unicode_bidi_class(0x037A, 0x037A, 'L'). % Lm       GREEK YPOGEGRAMMENI
unicode_bidi_class(0x037B, 0x037D, 'L'). % L&   [3] GREEK SMALL REVERSED LUNATE SIGMA SYMBOL..GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
unicode_bidi_class(0x0386, 0x0386, 'L'). % L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_bidi_class(0x0388, 0x038A, 'L'). % L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_bidi_class(0x038C, 0x038C, 'L'). % L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_bidi_class(0x038E, 0x03A1, 'L'). % L&  [20] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK CAPITAL LETTER RHO
unicode_bidi_class(0x03A3, 0x03F5, 'L'). % L&  [83] GREEK CAPITAL LETTER SIGMA..GREEK LUNATE EPSILON SYMBOL
unicode_bidi_class(0x03F7, 0x0481, 'L'). % L& [139] GREEK CAPITAL LETTER SHO..CYRILLIC SMALL LETTER KOPPA
unicode_bidi_class(0x0482, 0x0482, 'L'). % So       CYRILLIC THOUSANDS SIGN
unicode_bidi_class(0x048A, 0x0527, 'L'). % L& [158] CYRILLIC CAPITAL LETTER SHORT I WITH TAIL..CYRILLIC SMALL LETTER SHHA WITH DESCENDER
unicode_bidi_class(0x0531, 0x0556, 'L'). % L&  [38] ARMENIAN CAPITAL LETTER AYB..ARMENIAN CAPITAL LETTER FEH
unicode_bidi_class(0x0559, 0x0559, 'L'). % Lm       ARMENIAN MODIFIER LETTER LEFT HALF RING
unicode_bidi_class(0x055A, 0x055F, 'L'). % Po   [6] ARMENIAN APOSTROPHE..ARMENIAN ABBREVIATION MARK
unicode_bidi_class(0x0561, 0x0587, 'L'). % L&  [39] ARMENIAN SMALL LETTER AYB..ARMENIAN SMALL LIGATURE ECH YIWN
unicode_bidi_class(0x0589, 0x0589, 'L'). % Po       ARMENIAN FULL STOP
unicode_bidi_class(0x0903, 0x0903, 'L'). % Mc       DEVANAGARI SIGN VISARGA
unicode_bidi_class(0x0904, 0x0939, 'L'). % Lo  [54] DEVANAGARI LETTER SHORT A..DEVANAGARI LETTER HA
unicode_bidi_class(0x093B, 0x093B, 'L'). % Mc       DEVANAGARI VOWEL SIGN OOE
unicode_bidi_class(0x093D, 0x093D, 'L'). % Lo       DEVANAGARI SIGN AVAGRAHA
unicode_bidi_class(0x093E, 0x0940, 'L'). % Mc   [3] DEVANAGARI VOWEL SIGN AA..DEVANAGARI VOWEL SIGN II
unicode_bidi_class(0x0949, 0x094C, 'L'). % Mc   [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU
unicode_bidi_class(0x094E, 0x094F, 'L'). % Mc   [2] DEVANAGARI VOWEL SIGN PRISHTHAMATRA E..DEVANAGARI VOWEL SIGN AW
unicode_bidi_class(0x0950, 0x0950, 'L'). % Lo       DEVANAGARI OM
unicode_bidi_class(0x0958, 0x0961, 'L'). % Lo  [10] DEVANAGARI LETTER QA..DEVANAGARI LETTER VOCALIC LL
unicode_bidi_class(0x0964, 0x0965, 'L'). % Po   [2] DEVANAGARI DANDA..DEVANAGARI DOUBLE DANDA
unicode_bidi_class(0x0966, 0x096F, 'L'). % Nd  [10] DEVANAGARI DIGIT ZERO..DEVANAGARI DIGIT NINE
unicode_bidi_class(0x0970, 0x0970, 'L'). % Po       DEVANAGARI ABBREVIATION SIGN
unicode_bidi_class(0x0971, 0x0971, 'L'). % Lm       DEVANAGARI SIGN HIGH SPACING DOT
unicode_bidi_class(0x0972, 0x0977, 'L'). % Lo   [6] DEVANAGARI LETTER CANDRA A..DEVANAGARI LETTER UUE
unicode_bidi_class(0x0979, 0x097F, 'L'). % Lo   [7] DEVANAGARI LETTER ZHA..DEVANAGARI LETTER BBA
unicode_bidi_class(0x0982, 0x0983, 'L'). % Mc   [2] BENGALI SIGN ANUSVARA..BENGALI SIGN VISARGA
unicode_bidi_class(0x0985, 0x098C, 'L'). % Lo   [8] BENGALI LETTER A..BENGALI LETTER VOCALIC L
unicode_bidi_class(0x098F, 0x0990, 'L'). % Lo   [2] BENGALI LETTER E..BENGALI LETTER AI
unicode_bidi_class(0x0993, 0x09A8, 'L'). % Lo  [22] BENGALI LETTER O..BENGALI LETTER NA
unicode_bidi_class(0x09AA, 0x09B0, 'L'). % Lo   [7] BENGALI LETTER PA..BENGALI LETTER RA
unicode_bidi_class(0x09B2, 0x09B2, 'L'). % Lo       BENGALI LETTER LA
unicode_bidi_class(0x09B6, 0x09B9, 'L'). % Lo   [4] BENGALI LETTER SHA..BENGALI LETTER HA
unicode_bidi_class(0x09BD, 0x09BD, 'L'). % Lo       BENGALI SIGN AVAGRAHA
unicode_bidi_class(0x09BE, 0x09C0, 'L'). % Mc   [3] BENGALI VOWEL SIGN AA..BENGALI VOWEL SIGN II
unicode_bidi_class(0x09C7, 0x09C8, 'L'). % Mc   [2] BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI
unicode_bidi_class(0x09CB, 0x09CC, 'L'). % Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_bidi_class(0x09CE, 0x09CE, 'L'). % Lo       BENGALI LETTER KHANDA TA
unicode_bidi_class(0x09D7, 0x09D7, 'L'). % Mc       BENGALI AU LENGTH MARK
unicode_bidi_class(0x09DC, 0x09DD, 'L'). % Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_bidi_class(0x09DF, 0x09E1, 'L'). % Lo   [3] BENGALI LETTER YYA..BENGALI LETTER VOCALIC LL
unicode_bidi_class(0x09E6, 0x09EF, 'L'). % Nd  [10] BENGALI DIGIT ZERO..BENGALI DIGIT NINE
unicode_bidi_class(0x09F0, 0x09F1, 'L'). % Lo   [2] BENGALI LETTER RA WITH MIDDLE DIAGONAL..BENGALI LETTER RA WITH LOWER DIAGONAL
unicode_bidi_class(0x09F4, 0x09F9, 'L'). % No   [6] BENGALI CURRENCY NUMERATOR ONE..BENGALI CURRENCY DENOMINATOR SIXTEEN
unicode_bidi_class(0x09FA, 0x09FA, 'L'). % So       BENGALI ISSHAR
unicode_bidi_class(0x0A03, 0x0A03, 'L'). % Mc       GURMUKHI SIGN VISARGA
unicode_bidi_class(0x0A05, 0x0A0A, 'L'). % Lo   [6] GURMUKHI LETTER A..GURMUKHI LETTER UU
unicode_bidi_class(0x0A0F, 0x0A10, 'L'). % Lo   [2] GURMUKHI LETTER EE..GURMUKHI LETTER AI
unicode_bidi_class(0x0A13, 0x0A28, 'L'). % Lo  [22] GURMUKHI LETTER OO..GURMUKHI LETTER NA
unicode_bidi_class(0x0A2A, 0x0A30, 'L'). % Lo   [7] GURMUKHI LETTER PA..GURMUKHI LETTER RA
unicode_bidi_class(0x0A32, 0x0A33, 'L'). % Lo   [2] GURMUKHI LETTER LA..GURMUKHI LETTER LLA
unicode_bidi_class(0x0A35, 0x0A36, 'L'). % Lo   [2] GURMUKHI LETTER VA..GURMUKHI LETTER SHA
unicode_bidi_class(0x0A38, 0x0A39, 'L'). % Lo   [2] GURMUKHI LETTER SA..GURMUKHI LETTER HA
unicode_bidi_class(0x0A3E, 0x0A40, 'L'). % Mc   [3] GURMUKHI VOWEL SIGN AA..GURMUKHI VOWEL SIGN II
unicode_bidi_class(0x0A59, 0x0A5C, 'L'). % Lo   [4] GURMUKHI LETTER KHHA..GURMUKHI LETTER RRA
unicode_bidi_class(0x0A5E, 0x0A5E, 'L'). % Lo       GURMUKHI LETTER FA
unicode_bidi_class(0x0A66, 0x0A6F, 'L'). % Nd  [10] GURMUKHI DIGIT ZERO..GURMUKHI DIGIT NINE
unicode_bidi_class(0x0A72, 0x0A74, 'L'). % Lo   [3] GURMUKHI IRI..GURMUKHI EK ONKAR
unicode_bidi_class(0x0A83, 0x0A83, 'L'). % Mc       GUJARATI SIGN VISARGA
unicode_bidi_class(0x0A85, 0x0A8D, 'L'). % Lo   [9] GUJARATI LETTER A..GUJARATI VOWEL CANDRA E
unicode_bidi_class(0x0A8F, 0x0A91, 'L'). % Lo   [3] GUJARATI LETTER E..GUJARATI VOWEL CANDRA O
unicode_bidi_class(0x0A93, 0x0AA8, 'L'). % Lo  [22] GUJARATI LETTER O..GUJARATI LETTER NA
unicode_bidi_class(0x0AAA, 0x0AB0, 'L'). % Lo   [7] GUJARATI LETTER PA..GUJARATI LETTER RA
unicode_bidi_class(0x0AB2, 0x0AB3, 'L'). % Lo   [2] GUJARATI LETTER LA..GUJARATI LETTER LLA
unicode_bidi_class(0x0AB5, 0x0AB9, 'L'). % Lo   [5] GUJARATI LETTER VA..GUJARATI LETTER HA
unicode_bidi_class(0x0ABD, 0x0ABD, 'L'). % Lo       GUJARATI SIGN AVAGRAHA
unicode_bidi_class(0x0ABE, 0x0AC0, 'L'). % Mc   [3] GUJARATI VOWEL SIGN AA..GUJARATI VOWEL SIGN II
unicode_bidi_class(0x0AC9, 0x0AC9, 'L'). % Mc       GUJARATI VOWEL SIGN CANDRA O
unicode_bidi_class(0x0ACB, 0x0ACC, 'L'). % Mc   [2] GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU
unicode_bidi_class(0x0AD0, 0x0AD0, 'L'). % Lo       GUJARATI OM
unicode_bidi_class(0x0AE0, 0x0AE1, 'L'). % Lo   [2] GUJARATI LETTER VOCALIC RR..GUJARATI LETTER VOCALIC LL
unicode_bidi_class(0x0AE6, 0x0AEF, 'L'). % Nd  [10] GUJARATI DIGIT ZERO..GUJARATI DIGIT NINE
unicode_bidi_class(0x0AF0, 0x0AF0, 'L'). % Po       GUJARATI ABBREVIATION SIGN
unicode_bidi_class(0x0B02, 0x0B03, 'L'). % Mc   [2] ORIYA SIGN ANUSVARA..ORIYA SIGN VISARGA
unicode_bidi_class(0x0B05, 0x0B0C, 'L'). % Lo   [8] ORIYA LETTER A..ORIYA LETTER VOCALIC L
unicode_bidi_class(0x0B0F, 0x0B10, 'L'). % Lo   [2] ORIYA LETTER E..ORIYA LETTER AI
unicode_bidi_class(0x0B13, 0x0B28, 'L'). % Lo  [22] ORIYA LETTER O..ORIYA LETTER NA
unicode_bidi_class(0x0B2A, 0x0B30, 'L'). % Lo   [7] ORIYA LETTER PA..ORIYA LETTER RA
unicode_bidi_class(0x0B32, 0x0B33, 'L'). % Lo   [2] ORIYA LETTER LA..ORIYA LETTER LLA
unicode_bidi_class(0x0B35, 0x0B39, 'L'). % Lo   [5] ORIYA LETTER VA..ORIYA LETTER HA
unicode_bidi_class(0x0B3D, 0x0B3D, 'L'). % Lo       ORIYA SIGN AVAGRAHA
unicode_bidi_class(0x0B3E, 0x0B3E, 'L'). % Mc       ORIYA VOWEL SIGN AA
unicode_bidi_class(0x0B40, 0x0B40, 'L'). % Mc       ORIYA VOWEL SIGN II
unicode_bidi_class(0x0B47, 0x0B48, 'L'). % Mc   [2] ORIYA VOWEL SIGN E..ORIYA VOWEL SIGN AI
unicode_bidi_class(0x0B4B, 0x0B4C, 'L'). % Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
unicode_bidi_class(0x0B57, 0x0B57, 'L'). % Mc       ORIYA AU LENGTH MARK
unicode_bidi_class(0x0B5C, 0x0B5D, 'L'). % Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_bidi_class(0x0B5F, 0x0B61, 'L'). % Lo   [3] ORIYA LETTER YYA..ORIYA LETTER VOCALIC LL
unicode_bidi_class(0x0B66, 0x0B6F, 'L'). % Nd  [10] ORIYA DIGIT ZERO..ORIYA DIGIT NINE
unicode_bidi_class(0x0B70, 0x0B70, 'L'). % So       ORIYA ISSHAR
unicode_bidi_class(0x0B71, 0x0B71, 'L'). % Lo       ORIYA LETTER WA
unicode_bidi_class(0x0B72, 0x0B77, 'L'). % No   [6] ORIYA FRACTION ONE QUARTER..ORIYA FRACTION THREE SIXTEENTHS
unicode_bidi_class(0x0B83, 0x0B83, 'L'). % Lo       TAMIL SIGN VISARGA
unicode_bidi_class(0x0B85, 0x0B8A, 'L'). % Lo   [6] TAMIL LETTER A..TAMIL LETTER UU
unicode_bidi_class(0x0B8E, 0x0B90, 'L'). % Lo   [3] TAMIL LETTER E..TAMIL LETTER AI
unicode_bidi_class(0x0B92, 0x0B95, 'L'). % Lo   [4] TAMIL LETTER O..TAMIL LETTER KA
unicode_bidi_class(0x0B99, 0x0B9A, 'L'). % Lo   [2] TAMIL LETTER NGA..TAMIL LETTER CA
unicode_bidi_class(0x0B9C, 0x0B9C, 'L'). % Lo       TAMIL LETTER JA
unicode_bidi_class(0x0B9E, 0x0B9F, 'L'). % Lo   [2] TAMIL LETTER NYA..TAMIL LETTER TTA
unicode_bidi_class(0x0BA3, 0x0BA4, 'L'). % Lo   [2] TAMIL LETTER NNA..TAMIL LETTER TA
unicode_bidi_class(0x0BA8, 0x0BAA, 'L'). % Lo   [3] TAMIL LETTER NA..TAMIL LETTER PA
unicode_bidi_class(0x0BAE, 0x0BB9, 'L'). % Lo  [12] TAMIL LETTER MA..TAMIL LETTER HA
unicode_bidi_class(0x0BBE, 0x0BBF, 'L'). % Mc   [2] TAMIL VOWEL SIGN AA..TAMIL VOWEL SIGN I
unicode_bidi_class(0x0BC1, 0x0BC2, 'L'). % Mc   [2] TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU
unicode_bidi_class(0x0BC6, 0x0BC8, 'L'). % Mc   [3] TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI
unicode_bidi_class(0x0BCA, 0x0BCC, 'L'). % Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_bidi_class(0x0BD0, 0x0BD0, 'L'). % Lo       TAMIL OM
unicode_bidi_class(0x0BD7, 0x0BD7, 'L'). % Mc       TAMIL AU LENGTH MARK
unicode_bidi_class(0x0BE6, 0x0BEF, 'L'). % Nd  [10] TAMIL DIGIT ZERO..TAMIL DIGIT NINE
unicode_bidi_class(0x0BF0, 0x0BF2, 'L'). % No   [3] TAMIL NUMBER TEN..TAMIL NUMBER ONE THOUSAND
unicode_bidi_class(0x0C01, 0x0C03, 'L'). % Mc   [3] TELUGU SIGN CANDRABINDU..TELUGU SIGN VISARGA
unicode_bidi_class(0x0C05, 0x0C0C, 'L'). % Lo   [8] TELUGU LETTER A..TELUGU LETTER VOCALIC L
unicode_bidi_class(0x0C0E, 0x0C10, 'L'). % Lo   [3] TELUGU LETTER E..TELUGU LETTER AI
unicode_bidi_class(0x0C12, 0x0C28, 'L'). % Lo  [23] TELUGU LETTER O..TELUGU LETTER NA
unicode_bidi_class(0x0C2A, 0x0C33, 'L'). % Lo  [10] TELUGU LETTER PA..TELUGU LETTER LLA
unicode_bidi_class(0x0C35, 0x0C39, 'L'). % Lo   [5] TELUGU LETTER VA..TELUGU LETTER HA
unicode_bidi_class(0x0C3D, 0x0C3D, 'L'). % Lo       TELUGU SIGN AVAGRAHA
unicode_bidi_class(0x0C41, 0x0C44, 'L'). % Mc   [4] TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR
unicode_bidi_class(0x0C58, 0x0C59, 'L'). % Lo   [2] TELUGU LETTER TSA..TELUGU LETTER DZA
unicode_bidi_class(0x0C60, 0x0C61, 'L'). % Lo   [2] TELUGU LETTER VOCALIC RR..TELUGU LETTER VOCALIC LL
unicode_bidi_class(0x0C66, 0x0C6F, 'L'). % Nd  [10] TELUGU DIGIT ZERO..TELUGU DIGIT NINE
unicode_bidi_class(0x0C7F, 0x0C7F, 'L'). % So       TELUGU SIGN TUUMU
unicode_bidi_class(0x0C82, 0x0C83, 'L'). % Mc   [2] KANNADA SIGN ANUSVARA..KANNADA SIGN VISARGA
unicode_bidi_class(0x0C85, 0x0C8C, 'L'). % Lo   [8] KANNADA LETTER A..KANNADA LETTER VOCALIC L
unicode_bidi_class(0x0C8E, 0x0C90, 'L'). % Lo   [3] KANNADA LETTER E..KANNADA LETTER AI
unicode_bidi_class(0x0C92, 0x0CA8, 'L'). % Lo  [23] KANNADA LETTER O..KANNADA LETTER NA
unicode_bidi_class(0x0CAA, 0x0CB3, 'L'). % Lo  [10] KANNADA LETTER PA..KANNADA LETTER LLA
unicode_bidi_class(0x0CB5, 0x0CB9, 'L'). % Lo   [5] KANNADA LETTER VA..KANNADA LETTER HA
unicode_bidi_class(0x0CBD, 0x0CBD, 'L'). % Lo       KANNADA SIGN AVAGRAHA
unicode_bidi_class(0x0CBE, 0x0CBE, 'L'). % Mc       KANNADA VOWEL SIGN AA
unicode_bidi_class(0x0CBF, 0x0CBF, 'L'). % Mn       KANNADA VOWEL SIGN I
unicode_bidi_class(0x0CC0, 0x0CC4, 'L'). % Mc   [5] KANNADA VOWEL SIGN II..KANNADA VOWEL SIGN VOCALIC RR
unicode_bidi_class(0x0CC6, 0x0CC6, 'L'). % Mn       KANNADA VOWEL SIGN E
unicode_bidi_class(0x0CC7, 0x0CC8, 'L'). % Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_bidi_class(0x0CCA, 0x0CCB, 'L'). % Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_bidi_class(0x0CD5, 0x0CD6, 'L'). % Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
unicode_bidi_class(0x0CDE, 0x0CDE, 'L'). % Lo       KANNADA LETTER FA
unicode_bidi_class(0x0CE0, 0x0CE1, 'L'). % Lo   [2] KANNADA LETTER VOCALIC RR..KANNADA LETTER VOCALIC LL
unicode_bidi_class(0x0CE6, 0x0CEF, 'L'). % Nd  [10] KANNADA DIGIT ZERO..KANNADA DIGIT NINE
unicode_bidi_class(0x0CF1, 0x0CF2, 'L'). % Lo   [2] KANNADA SIGN JIHVAMULIYA..KANNADA SIGN UPADHMANIYA
unicode_bidi_class(0x0D02, 0x0D03, 'L'). % Mc   [2] MALAYALAM SIGN ANUSVARA..MALAYALAM SIGN VISARGA
unicode_bidi_class(0x0D05, 0x0D0C, 'L'). % Lo   [8] MALAYALAM LETTER A..MALAYALAM LETTER VOCALIC L
unicode_bidi_class(0x0D0E, 0x0D10, 'L'). % Lo   [3] MALAYALAM LETTER E..MALAYALAM LETTER AI
unicode_bidi_class(0x0D12, 0x0D3A, 'L'). % Lo  [41] MALAYALAM LETTER O..MALAYALAM LETTER TTTA
unicode_bidi_class(0x0D3D, 0x0D3D, 'L'). % Lo       MALAYALAM SIGN AVAGRAHA
unicode_bidi_class(0x0D3E, 0x0D40, 'L'). % Mc   [3] MALAYALAM VOWEL SIGN AA..MALAYALAM VOWEL SIGN II
unicode_bidi_class(0x0D46, 0x0D48, 'L'). % Mc   [3] MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI
unicode_bidi_class(0x0D4A, 0x0D4C, 'L'). % Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_bidi_class(0x0D4E, 0x0D4E, 'L'). % Lo       MALAYALAM LETTER DOT REPH
unicode_bidi_class(0x0D57, 0x0D57, 'L'). % Mc       MALAYALAM AU LENGTH MARK
unicode_bidi_class(0x0D60, 0x0D61, 'L'). % Lo   [2] MALAYALAM LETTER VOCALIC RR..MALAYALAM LETTER VOCALIC LL
unicode_bidi_class(0x0D66, 0x0D6F, 'L'). % Nd  [10] MALAYALAM DIGIT ZERO..MALAYALAM DIGIT NINE
unicode_bidi_class(0x0D70, 0x0D75, 'L'). % No   [6] MALAYALAM NUMBER TEN..MALAYALAM FRACTION THREE QUARTERS
unicode_bidi_class(0x0D79, 0x0D79, 'L'). % So       MALAYALAM DATE MARK
unicode_bidi_class(0x0D7A, 0x0D7F, 'L'). % Lo   [6] MALAYALAM LETTER CHILLU NN..MALAYALAM LETTER CHILLU K
unicode_bidi_class(0x0D82, 0x0D83, 'L'). % Mc   [2] SINHALA SIGN ANUSVARAYA..SINHALA SIGN VISARGAYA
unicode_bidi_class(0x0D85, 0x0D96, 'L'). % Lo  [18] SINHALA LETTER AYANNA..SINHALA LETTER AUYANNA
unicode_bidi_class(0x0D9A, 0x0DB1, 'L'). % Lo  [24] SINHALA LETTER ALPAPRAANA KAYANNA..SINHALA LETTER DANTAJA NAYANNA
unicode_bidi_class(0x0DB3, 0x0DBB, 'L'). % Lo   [9] SINHALA LETTER SANYAKA DAYANNA..SINHALA LETTER RAYANNA
unicode_bidi_class(0x0DBD, 0x0DBD, 'L'). % Lo       SINHALA LETTER DANTAJA LAYANNA
unicode_bidi_class(0x0DC0, 0x0DC6, 'L'). % Lo   [7] SINHALA LETTER VAYANNA..SINHALA LETTER FAYANNA
unicode_bidi_class(0x0DCF, 0x0DD1, 'L'). % Mc   [3] SINHALA VOWEL SIGN AELA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA
unicode_bidi_class(0x0DD8, 0x0DDF, 'L'). % Mc   [8] SINHALA VOWEL SIGN GAETTA-PILLA..SINHALA VOWEL SIGN GAYANUKITTA
unicode_bidi_class(0x0DF2, 0x0DF3, 'L'). % Mc   [2] SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA
unicode_bidi_class(0x0DF4, 0x0DF4, 'L'). % Po       SINHALA PUNCTUATION KUNDDALIYA
unicode_bidi_class(0x0E01, 0x0E30, 'L'). % Lo  [48] THAI CHARACTER KO KAI..THAI CHARACTER SARA A
unicode_bidi_class(0x0E32, 0x0E33, 'L'). % Lo   [2] THAI CHARACTER SARA AA..THAI CHARACTER SARA AM
unicode_bidi_class(0x0E40, 0x0E45, 'L'). % Lo   [6] THAI CHARACTER SARA E..THAI CHARACTER LAKKHANGYAO
unicode_bidi_class(0x0E46, 0x0E46, 'L'). % Lm       THAI CHARACTER MAIYAMOK
unicode_bidi_class(0x0E4F, 0x0E4F, 'L'). % Po       THAI CHARACTER FONGMAN
unicode_bidi_class(0x0E50, 0x0E59, 'L'). % Nd  [10] THAI DIGIT ZERO..THAI DIGIT NINE
unicode_bidi_class(0x0E5A, 0x0E5B, 'L'). % Po   [2] THAI CHARACTER ANGKHANKHU..THAI CHARACTER KHOMUT
unicode_bidi_class(0x0E81, 0x0E82, 'L'). % Lo   [2] LAO LETTER KO..LAO LETTER KHO SUNG
unicode_bidi_class(0x0E84, 0x0E84, 'L'). % Lo       LAO LETTER KHO TAM
unicode_bidi_class(0x0E87, 0x0E88, 'L'). % Lo   [2] LAO LETTER NGO..LAO LETTER CO
unicode_bidi_class(0x0E8A, 0x0E8A, 'L'). % Lo       LAO LETTER SO TAM
unicode_bidi_class(0x0E8D, 0x0E8D, 'L'). % Lo       LAO LETTER NYO
unicode_bidi_class(0x0E94, 0x0E97, 'L'). % Lo   [4] LAO LETTER DO..LAO LETTER THO TAM
unicode_bidi_class(0x0E99, 0x0E9F, 'L'). % Lo   [7] LAO LETTER NO..LAO LETTER FO SUNG
unicode_bidi_class(0x0EA1, 0x0EA3, 'L'). % Lo   [3] LAO LETTER MO..LAO LETTER LO LING
unicode_bidi_class(0x0EA5, 0x0EA5, 'L'). % Lo       LAO LETTER LO LOOT
unicode_bidi_class(0x0EA7, 0x0EA7, 'L'). % Lo       LAO LETTER WO
unicode_bidi_class(0x0EAA, 0x0EAB, 'L'). % Lo   [2] LAO LETTER SO SUNG..LAO LETTER HO SUNG
unicode_bidi_class(0x0EAD, 0x0EB0, 'L'). % Lo   [4] LAO LETTER O..LAO VOWEL SIGN A
unicode_bidi_class(0x0EB2, 0x0EB3, 'L'). % Lo   [2] LAO VOWEL SIGN AA..LAO VOWEL SIGN AM
unicode_bidi_class(0x0EBD, 0x0EBD, 'L'). % Lo       LAO SEMIVOWEL SIGN NYO
unicode_bidi_class(0x0EC0, 0x0EC4, 'L'). % Lo   [5] LAO VOWEL SIGN E..LAO VOWEL SIGN AI
unicode_bidi_class(0x0EC6, 0x0EC6, 'L'). % Lm       LAO KO LA
unicode_bidi_class(0x0ED0, 0x0ED9, 'L'). % Nd  [10] LAO DIGIT ZERO..LAO DIGIT NINE
unicode_bidi_class(0x0EDC, 0x0EDF, 'L'). % Lo   [4] LAO HO NO..LAO LETTER KHMU NYO
unicode_bidi_class(0x0F00, 0x0F00, 'L'). % Lo       TIBETAN SYLLABLE OM
unicode_bidi_class(0x0F01, 0x0F03, 'L'). % So   [3] TIBETAN MARK GTER YIG MGO TRUNCATED A..TIBETAN MARK GTER YIG MGO -UM GTER TSHEG MA
unicode_bidi_class(0x0F04, 0x0F12, 'L'). % Po  [15] TIBETAN MARK INITIAL YIG MGO MDUN MA..TIBETAN MARK RGYA GRAM SHAD
unicode_bidi_class(0x0F13, 0x0F13, 'L'). % So       TIBETAN MARK CARET -DZUD RTAGS ME LONG CAN
unicode_bidi_class(0x0F14, 0x0F14, 'L'). % Po       TIBETAN MARK GTER TSHEG
unicode_bidi_class(0x0F15, 0x0F17, 'L'). % So   [3] TIBETAN LOGOTYPE SIGN CHAD RTAGS..TIBETAN ASTROLOGICAL SIGN SGRA GCAN -CHAR RTAGS
unicode_bidi_class(0x0F1A, 0x0F1F, 'L'). % So   [6] TIBETAN SIGN RDEL DKAR GCIG..TIBETAN SIGN RDEL DKAR RDEL NAG
unicode_bidi_class(0x0F20, 0x0F29, 'L'). % Nd  [10] TIBETAN DIGIT ZERO..TIBETAN DIGIT NINE
unicode_bidi_class(0x0F2A, 0x0F33, 'L'). % No  [10] TIBETAN DIGIT HALF ONE..TIBETAN DIGIT HALF ZERO
unicode_bidi_class(0x0F34, 0x0F34, 'L'). % So       TIBETAN MARK BSDUS RTAGS
unicode_bidi_class(0x0F36, 0x0F36, 'L'). % So       TIBETAN MARK CARET -DZUD RTAGS BZHI MIG CAN
unicode_bidi_class(0x0F38, 0x0F38, 'L'). % So       TIBETAN MARK CHE MGO
unicode_bidi_class(0x0F3E, 0x0F3F, 'L'). % Mc   [2] TIBETAN SIGN YAR TSHES..TIBETAN SIGN MAR TSHES
unicode_bidi_class(0x0F40, 0x0F47, 'L'). % Lo   [8] TIBETAN LETTER KA..TIBETAN LETTER JA
unicode_bidi_class(0x0F49, 0x0F6C, 'L'). % Lo  [36] TIBETAN LETTER NYA..TIBETAN LETTER RRA
unicode_bidi_class(0x0F7F, 0x0F7F, 'L'). % Mc       TIBETAN SIGN RNAM BCAD
unicode_bidi_class(0x0F85, 0x0F85, 'L'). % Po       TIBETAN MARK PALUTA
unicode_bidi_class(0x0F88, 0x0F8C, 'L'). % Lo   [5] TIBETAN SIGN LCE TSA CAN..TIBETAN SIGN INVERTED MCHU CAN
unicode_bidi_class(0x0FBE, 0x0FC5, 'L'). % So   [8] TIBETAN KU RU KHA..TIBETAN SYMBOL RDO RJE
unicode_bidi_class(0x0FC7, 0x0FCC, 'L'). % So   [6] TIBETAN SYMBOL RDO RJE RGYA GRAM..TIBETAN SYMBOL NOR BU BZHI -KHYIL
unicode_bidi_class(0x0FCE, 0x0FCF, 'L'). % So   [2] TIBETAN SIGN RDEL NAG RDEL DKAR..TIBETAN SIGN RDEL NAG GSUM
unicode_bidi_class(0x0FD0, 0x0FD4, 'L'). % Po   [5] TIBETAN MARK BSKA- SHOG GI MGO RGYAN..TIBETAN MARK CLOSING BRDA RNYING YIG MGO SGAB MA
unicode_bidi_class(0x0FD5, 0x0FD8, 'L'). % So   [4] RIGHT-FACING SVASTI SIGN..LEFT-FACING SVASTI SIGN WITH DOTS
unicode_bidi_class(0x0FD9, 0x0FDA, 'L'). % Po   [2] TIBETAN MARK LEADING MCHAN RTAGS..TIBETAN MARK TRAILING MCHAN RTAGS
unicode_bidi_class(0x1000, 0x102A, 'L'). % Lo  [43] MYANMAR LETTER KA..MYANMAR LETTER AU
unicode_bidi_class(0x102B, 0x102C, 'L'). % Mc   [2] MYANMAR VOWEL SIGN TALL AA..MYANMAR VOWEL SIGN AA
unicode_bidi_class(0x1031, 0x1031, 'L'). % Mc       MYANMAR VOWEL SIGN E
unicode_bidi_class(0x1038, 0x1038, 'L'). % Mc       MYANMAR SIGN VISARGA
unicode_bidi_class(0x103B, 0x103C, 'L'). % Mc   [2] MYANMAR CONSONANT SIGN MEDIAL YA..MYANMAR CONSONANT SIGN MEDIAL RA
unicode_bidi_class(0x103F, 0x103F, 'L'). % Lo       MYANMAR LETTER GREAT SA
unicode_bidi_class(0x1040, 0x1049, 'L'). % Nd  [10] MYANMAR DIGIT ZERO..MYANMAR DIGIT NINE
unicode_bidi_class(0x104A, 0x104F, 'L'). % Po   [6] MYANMAR SIGN LITTLE SECTION..MYANMAR SYMBOL GENITIVE
unicode_bidi_class(0x1050, 0x1055, 'L'). % Lo   [6] MYANMAR LETTER SHA..MYANMAR LETTER VOCALIC LL
unicode_bidi_class(0x1056, 0x1057, 'L'). % Mc   [2] MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR
unicode_bidi_class(0x105A, 0x105D, 'L'). % Lo   [4] MYANMAR LETTER MON NGA..MYANMAR LETTER MON BBE
unicode_bidi_class(0x1061, 0x1061, 'L'). % Lo       MYANMAR LETTER SGAW KAREN SHA
unicode_bidi_class(0x1062, 0x1064, 'L'). % Mc   [3] MYANMAR VOWEL SIGN SGAW KAREN EU..MYANMAR TONE MARK SGAW KAREN KE PHO
unicode_bidi_class(0x1065, 0x1066, 'L'). % Lo   [2] MYANMAR LETTER WESTERN PWO KAREN THA..MYANMAR LETTER WESTERN PWO KAREN PWA
unicode_bidi_class(0x1067, 0x106D, 'L'). % Mc   [7] MYANMAR VOWEL SIGN WESTERN PWO KAREN EU..MYANMAR SIGN WESTERN PWO KAREN TONE-5
unicode_bidi_class(0x106E, 0x1070, 'L'). % Lo   [3] MYANMAR LETTER EASTERN PWO KAREN NNA..MYANMAR LETTER EASTERN PWO KAREN GHWA
unicode_bidi_class(0x1075, 0x1081, 'L'). % Lo  [13] MYANMAR LETTER SHAN KA..MYANMAR LETTER SHAN HA
unicode_bidi_class(0x1083, 0x1084, 'L'). % Mc   [2] MYANMAR VOWEL SIGN SHAN AA..MYANMAR VOWEL SIGN SHAN E
unicode_bidi_class(0x1087, 0x108C, 'L'). % Mc   [6] MYANMAR SIGN SHAN TONE-2..MYANMAR SIGN SHAN COUNCIL TONE-3
unicode_bidi_class(0x108E, 0x108E, 'L'). % Lo       MYANMAR LETTER RUMAI PALAUNG FA
unicode_bidi_class(0x108F, 0x108F, 'L'). % Mc       MYANMAR SIGN RUMAI PALAUNG TONE-5
unicode_bidi_class(0x1090, 0x1099, 'L'). % Nd  [10] MYANMAR SHAN DIGIT ZERO..MYANMAR SHAN DIGIT NINE
unicode_bidi_class(0x109A, 0x109C, 'L'). % Mc   [3] MYANMAR SIGN KHAMTI TONE-1..MYANMAR VOWEL SIGN AITON A
unicode_bidi_class(0x109E, 0x109F, 'L'). % So   [2] MYANMAR SYMBOL SHAN ONE..MYANMAR SYMBOL SHAN EXCLAMATION
unicode_bidi_class(0x10A0, 0x10C5, 'L'). % L&  [38] GEORGIAN CAPITAL LETTER AN..GEORGIAN CAPITAL LETTER HOE
unicode_bidi_class(0x10C7, 0x10C7, 'L'). % L&       GEORGIAN CAPITAL LETTER YN
unicode_bidi_class(0x10CD, 0x10CD, 'L'). % L&       GEORGIAN CAPITAL LETTER AEN
unicode_bidi_class(0x10D0, 0x10FA, 'L'). % Lo  [43] GEORGIAN LETTER AN..GEORGIAN LETTER AIN
unicode_bidi_class(0x10FB, 0x10FB, 'L'). % Po       GEORGIAN PARAGRAPH SEPARATOR
unicode_bidi_class(0x10FC, 0x10FC, 'L'). % Lm       MODIFIER LETTER GEORGIAN NAR
unicode_bidi_class(0x10FD, 0x1248, 'L'). % Lo [332] GEORGIAN LETTER AEN..ETHIOPIC SYLLABLE QWA
unicode_bidi_class(0x124A, 0x124D, 'L'). % Lo   [4] ETHIOPIC SYLLABLE QWI..ETHIOPIC SYLLABLE QWE
unicode_bidi_class(0x1250, 0x1256, 'L'). % Lo   [7] ETHIOPIC SYLLABLE QHA..ETHIOPIC SYLLABLE QHO
unicode_bidi_class(0x1258, 0x1258, 'L'). % Lo       ETHIOPIC SYLLABLE QHWA
unicode_bidi_class(0x125A, 0x125D, 'L'). % Lo   [4] ETHIOPIC SYLLABLE QHWI..ETHIOPIC SYLLABLE QHWE
unicode_bidi_class(0x1260, 0x1288, 'L'). % Lo  [41] ETHIOPIC SYLLABLE BA..ETHIOPIC SYLLABLE XWA
unicode_bidi_class(0x128A, 0x128D, 'L'). % Lo   [4] ETHIOPIC SYLLABLE XWI..ETHIOPIC SYLLABLE XWE
unicode_bidi_class(0x1290, 0x12B0, 'L'). % Lo  [33] ETHIOPIC SYLLABLE NA..ETHIOPIC SYLLABLE KWA
unicode_bidi_class(0x12B2, 0x12B5, 'L'). % Lo   [4] ETHIOPIC SYLLABLE KWI..ETHIOPIC SYLLABLE KWE
unicode_bidi_class(0x12B8, 0x12BE, 'L'). % Lo   [7] ETHIOPIC SYLLABLE KXA..ETHIOPIC SYLLABLE KXO
unicode_bidi_class(0x12C0, 0x12C0, 'L'). % Lo       ETHIOPIC SYLLABLE KXWA
unicode_bidi_class(0x12C2, 0x12C5, 'L'). % Lo   [4] ETHIOPIC SYLLABLE KXWI..ETHIOPIC SYLLABLE KXWE
unicode_bidi_class(0x12C8, 0x12D6, 'L'). % Lo  [15] ETHIOPIC SYLLABLE WA..ETHIOPIC SYLLABLE PHARYNGEAL O
unicode_bidi_class(0x12D8, 0x1310, 'L'). % Lo  [57] ETHIOPIC SYLLABLE ZA..ETHIOPIC SYLLABLE GWA
unicode_bidi_class(0x1312, 0x1315, 'L'). % Lo   [4] ETHIOPIC SYLLABLE GWI..ETHIOPIC SYLLABLE GWE
unicode_bidi_class(0x1318, 0x135A, 'L'). % Lo  [67] ETHIOPIC SYLLABLE GGA..ETHIOPIC SYLLABLE FYA
unicode_bidi_class(0x1360, 0x1368, 'L'). % Po   [9] ETHIOPIC SECTION MARK..ETHIOPIC PARAGRAPH SEPARATOR
unicode_bidi_class(0x1369, 0x137C, 'L'). % No  [20] ETHIOPIC DIGIT ONE..ETHIOPIC NUMBER TEN THOUSAND
unicode_bidi_class(0x1380, 0x138F, 'L'). % Lo  [16] ETHIOPIC SYLLABLE SEBATBEIT MWA..ETHIOPIC SYLLABLE PWE
unicode_bidi_class(0x13A0, 0x13F4, 'L'). % Lo  [85] CHEROKEE LETTER A..CHEROKEE LETTER YV
unicode_bidi_class(0x1401, 0x166C, 'L'). % Lo [620] CANADIAN SYLLABICS E..CANADIAN SYLLABICS CARRIER TTSA
unicode_bidi_class(0x166D, 0x166E, 'L'). % Po   [2] CANADIAN SYLLABICS CHI SIGN..CANADIAN SYLLABICS FULL STOP
unicode_bidi_class(0x166F, 0x167F, 'L'). % Lo  [17] CANADIAN SYLLABICS QAI..CANADIAN SYLLABICS BLACKFOOT W
unicode_bidi_class(0x1681, 0x169A, 'L'). % Lo  [26] OGHAM LETTER BEITH..OGHAM LETTER PEITH
unicode_bidi_class(0x16A0, 0x16EA, 'L'). % Lo  [75] RUNIC LETTER FEHU FEOH FE F..RUNIC LETTER X
unicode_bidi_class(0x16EB, 0x16ED, 'L'). % Po   [3] RUNIC SINGLE PUNCTUATION..RUNIC CROSS PUNCTUATION
unicode_bidi_class(0x16EE, 0x16F0, 'L'). % Nl   [3] RUNIC ARLAUG SYMBOL..RUNIC BELGTHOR SYMBOL
unicode_bidi_class(0x1700, 0x170C, 'L'). % Lo  [13] TAGALOG LETTER A..TAGALOG LETTER YA
unicode_bidi_class(0x170E, 0x1711, 'L'). % Lo   [4] TAGALOG LETTER LA..TAGALOG LETTER HA
unicode_bidi_class(0x1720, 0x1731, 'L'). % Lo  [18] HANUNOO LETTER A..HANUNOO LETTER HA
unicode_bidi_class(0x1735, 0x1736, 'L'). % Po   [2] PHILIPPINE SINGLE PUNCTUATION..PHILIPPINE DOUBLE PUNCTUATION
unicode_bidi_class(0x1740, 0x1751, 'L'). % Lo  [18] BUHID LETTER A..BUHID LETTER HA
unicode_bidi_class(0x1760, 0x176C, 'L'). % Lo  [13] TAGBANWA LETTER A..TAGBANWA LETTER YA
unicode_bidi_class(0x176E, 0x1770, 'L'). % Lo   [3] TAGBANWA LETTER LA..TAGBANWA LETTER SA
unicode_bidi_class(0x1780, 0x17B3, 'L'). % Lo  [52] KHMER LETTER KA..KHMER INDEPENDENT VOWEL QAU
unicode_bidi_class(0x17B6, 0x17B6, 'L'). % Mc       KHMER VOWEL SIGN AA
unicode_bidi_class(0x17BE, 0x17C5, 'L'). % Mc   [8] KHMER VOWEL SIGN OE..KHMER VOWEL SIGN AU
unicode_bidi_class(0x17C7, 0x17C8, 'L'). % Mc   [2] KHMER SIGN REAHMUK..KHMER SIGN YUUKALEAPINTU
unicode_bidi_class(0x17D4, 0x17D6, 'L'). % Po   [3] KHMER SIGN KHAN..KHMER SIGN CAMNUC PII KUUH
unicode_bidi_class(0x17D7, 0x17D7, 'L'). % Lm       KHMER SIGN LEK TOO
unicode_bidi_class(0x17D8, 0x17DA, 'L'). % Po   [3] KHMER SIGN BEYYAL..KHMER SIGN KOOMUUT
unicode_bidi_class(0x17DC, 0x17DC, 'L'). % Lo       KHMER SIGN AVAKRAHASANYA
unicode_bidi_class(0x17E0, 0x17E9, 'L'). % Nd  [10] KHMER DIGIT ZERO..KHMER DIGIT NINE
unicode_bidi_class(0x1810, 0x1819, 'L'). % Nd  [10] MONGOLIAN DIGIT ZERO..MONGOLIAN DIGIT NINE
unicode_bidi_class(0x1820, 0x1842, 'L'). % Lo  [35] MONGOLIAN LETTER A..MONGOLIAN LETTER CHI
unicode_bidi_class(0x1843, 0x1843, 'L'). % Lm       MONGOLIAN LETTER TODO LONG VOWEL SIGN
unicode_bidi_class(0x1844, 0x1877, 'L'). % Lo  [52] MONGOLIAN LETTER TODO E..MONGOLIAN LETTER MANCHU ZHA
unicode_bidi_class(0x1880, 0x18A8, 'L'). % Lo  [41] MONGOLIAN LETTER ALI GALI ANUSVARA ONE..MONGOLIAN LETTER MANCHU ALI GALI BHA
unicode_bidi_class(0x18AA, 0x18AA, 'L'). % Lo       MONGOLIAN LETTER MANCHU ALI GALI LHA
unicode_bidi_class(0x18B0, 0x18F5, 'L'). % Lo  [70] CANADIAN SYLLABICS OY..CANADIAN SYLLABICS CARRIER DENTAL S
unicode_bidi_class(0x1900, 0x191C, 'L'). % Lo  [29] LIMBU VOWEL-CARRIER LETTER..LIMBU LETTER HA
unicode_bidi_class(0x1923, 0x1926, 'L'). % Mc   [4] LIMBU VOWEL SIGN EE..LIMBU VOWEL SIGN AU
unicode_bidi_class(0x1929, 0x192B, 'L'). % Mc   [3] LIMBU SUBJOINED LETTER YA..LIMBU SUBJOINED LETTER WA
unicode_bidi_class(0x1930, 0x1931, 'L'). % Mc   [2] LIMBU SMALL LETTER KA..LIMBU SMALL LETTER NGA
unicode_bidi_class(0x1933, 0x1938, 'L'). % Mc   [6] LIMBU SMALL LETTER TA..LIMBU SMALL LETTER LA
unicode_bidi_class(0x1946, 0x194F, 'L'). % Nd  [10] LIMBU DIGIT ZERO..LIMBU DIGIT NINE
unicode_bidi_class(0x1950, 0x196D, 'L'). % Lo  [30] TAI LE LETTER KA..TAI LE LETTER AI
unicode_bidi_class(0x1970, 0x1974, 'L'). % Lo   [5] TAI LE LETTER TONE-2..TAI LE LETTER TONE-6
unicode_bidi_class(0x1980, 0x19AB, 'L'). % Lo  [44] NEW TAI LUE LETTER HIGH QA..NEW TAI LUE LETTER LOW SUA
unicode_bidi_class(0x19B0, 0x19C0, 'L'). % Mc  [17] NEW TAI LUE VOWEL SIGN VOWEL SHORTENER..NEW TAI LUE VOWEL SIGN IY
unicode_bidi_class(0x19C1, 0x19C7, 'L'). % Lo   [7] NEW TAI LUE LETTER FINAL V..NEW TAI LUE LETTER FINAL B
unicode_bidi_class(0x19C8, 0x19C9, 'L'). % Mc   [2] NEW TAI LUE TONE MARK-1..NEW TAI LUE TONE MARK-2
unicode_bidi_class(0x19D0, 0x19D9, 'L'). % Nd  [10] NEW TAI LUE DIGIT ZERO..NEW TAI LUE DIGIT NINE
unicode_bidi_class(0x19DA, 0x19DA, 'L'). % No       NEW TAI LUE THAM DIGIT ONE
unicode_bidi_class(0x1A00, 0x1A16, 'L'). % Lo  [23] BUGINESE LETTER KA..BUGINESE LETTER HA
unicode_bidi_class(0x1A19, 0x1A1B, 'L'). % Mc   [3] BUGINESE VOWEL SIGN E..BUGINESE VOWEL SIGN AE
unicode_bidi_class(0x1A1E, 0x1A1F, 'L'). % Po   [2] BUGINESE PALLAWA..BUGINESE END OF SECTION
unicode_bidi_class(0x1A20, 0x1A54, 'L'). % Lo  [53] TAI THAM LETTER HIGH KA..TAI THAM LETTER GREAT SA
unicode_bidi_class(0x1A55, 0x1A55, 'L'). % Mc       TAI THAM CONSONANT SIGN MEDIAL RA
unicode_bidi_class(0x1A57, 0x1A57, 'L'). % Mc       TAI THAM CONSONANT SIGN LA TANG LAI
unicode_bidi_class(0x1A61, 0x1A61, 'L'). % Mc       TAI THAM VOWEL SIGN A
unicode_bidi_class(0x1A63, 0x1A64, 'L'). % Mc   [2] TAI THAM VOWEL SIGN AA..TAI THAM VOWEL SIGN TALL AA
unicode_bidi_class(0x1A6D, 0x1A72, 'L'). % Mc   [6] TAI THAM VOWEL SIGN OY..TAI THAM VOWEL SIGN THAM AI
unicode_bidi_class(0x1A80, 0x1A89, 'L'). % Nd  [10] TAI THAM HORA DIGIT ZERO..TAI THAM HORA DIGIT NINE
unicode_bidi_class(0x1A90, 0x1A99, 'L'). % Nd  [10] TAI THAM THAM DIGIT ZERO..TAI THAM THAM DIGIT NINE
unicode_bidi_class(0x1AA0, 0x1AA6, 'L'). % Po   [7] TAI THAM SIGN WIANG..TAI THAM SIGN REVERSED ROTATED RANA
unicode_bidi_class(0x1AA7, 0x1AA7, 'L'). % Lm       TAI THAM SIGN MAI YAMOK
unicode_bidi_class(0x1AA8, 0x1AAD, 'L'). % Po   [6] TAI THAM SIGN KAAN..TAI THAM SIGN CAANG
unicode_bidi_class(0x1B04, 0x1B04, 'L'). % Mc       BALINESE SIGN BISAH
unicode_bidi_class(0x1B05, 0x1B33, 'L'). % Lo  [47] BALINESE LETTER AKARA..BALINESE LETTER HA
unicode_bidi_class(0x1B35, 0x1B35, 'L'). % Mc       BALINESE VOWEL SIGN TEDUNG
unicode_bidi_class(0x1B3B, 0x1B3B, 'L'). % Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_bidi_class(0x1B3D, 0x1B41, 'L'). % Mc   [5] BALINESE VOWEL SIGN LA LENGA TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG
unicode_bidi_class(0x1B43, 0x1B44, 'L'). % Mc   [2] BALINESE VOWEL SIGN PEPET TEDUNG..BALINESE ADEG ADEG
unicode_bidi_class(0x1B45, 0x1B4B, 'L'). % Lo   [7] BALINESE LETTER KAF SASAK..BALINESE LETTER ASYURA SASAK
unicode_bidi_class(0x1B50, 0x1B59, 'L'). % Nd  [10] BALINESE DIGIT ZERO..BALINESE DIGIT NINE
unicode_bidi_class(0x1B5A, 0x1B60, 'L'). % Po   [7] BALINESE PANTI..BALINESE PAMENENG
unicode_bidi_class(0x1B61, 0x1B6A, 'L'). % So  [10] BALINESE MUSICAL SYMBOL DONG..BALINESE MUSICAL SYMBOL DANG GEDE
unicode_bidi_class(0x1B74, 0x1B7C, 'L'). % So   [9] BALINESE MUSICAL SYMBOL RIGHT-HAND OPEN DUG..BALINESE MUSICAL SYMBOL LEFT-HAND OPEN PING
unicode_bidi_class(0x1B82, 0x1B82, 'L'). % Mc       SUNDANESE SIGN PANGWISAD
unicode_bidi_class(0x1B83, 0x1BA0, 'L'). % Lo  [30] SUNDANESE LETTER A..SUNDANESE LETTER HA
unicode_bidi_class(0x1BA1, 0x1BA1, 'L'). % Mc       SUNDANESE CONSONANT SIGN PAMINGKAL
unicode_bidi_class(0x1BA6, 0x1BA7, 'L'). % Mc   [2] SUNDANESE VOWEL SIGN PANAELAENG..SUNDANESE VOWEL SIGN PANOLONG
unicode_bidi_class(0x1BAA, 0x1BAA, 'L'). % Mc       SUNDANESE SIGN PAMAAEH
unicode_bidi_class(0x1BAC, 0x1BAD, 'L'). % Mc   [2] SUNDANESE CONSONANT SIGN PASANGAN MA..SUNDANESE CONSONANT SIGN PASANGAN WA
unicode_bidi_class(0x1BAE, 0x1BAF, 'L'). % Lo   [2] SUNDANESE LETTER KHA..SUNDANESE LETTER SYA
unicode_bidi_class(0x1BB0, 0x1BB9, 'L'). % Nd  [10] SUNDANESE DIGIT ZERO..SUNDANESE DIGIT NINE
unicode_bidi_class(0x1BBA, 0x1BE5, 'L'). % Lo  [44] SUNDANESE AVAGRAHA..BATAK LETTER U
unicode_bidi_class(0x1BE7, 0x1BE7, 'L'). % Mc       BATAK VOWEL SIGN E
unicode_bidi_class(0x1BEA, 0x1BEC, 'L'). % Mc   [3] BATAK VOWEL SIGN I..BATAK VOWEL SIGN O
unicode_bidi_class(0x1BEE, 0x1BEE, 'L'). % Mc       BATAK VOWEL SIGN U
unicode_bidi_class(0x1BF2, 0x1BF3, 'L'). % Mc   [2] BATAK PANGOLAT..BATAK PANONGONAN
unicode_bidi_class(0x1BFC, 0x1BFF, 'L'). % Po   [4] BATAK SYMBOL BINDU NA METEK..BATAK SYMBOL BINDU PANGOLAT
unicode_bidi_class(0x1C00, 0x1C23, 'L'). % Lo  [36] LEPCHA LETTER KA..LEPCHA LETTER A
unicode_bidi_class(0x1C24, 0x1C2B, 'L'). % Mc   [8] LEPCHA SUBJOINED LETTER YA..LEPCHA VOWEL SIGN UU
unicode_bidi_class(0x1C34, 0x1C35, 'L'). % Mc   [2] LEPCHA CONSONANT SIGN NYIN-DO..LEPCHA CONSONANT SIGN KANG
unicode_bidi_class(0x1C3B, 0x1C3F, 'L'). % Po   [5] LEPCHA PUNCTUATION TA-ROL..LEPCHA PUNCTUATION TSHOOK
unicode_bidi_class(0x1C40, 0x1C49, 'L'). % Nd  [10] LEPCHA DIGIT ZERO..LEPCHA DIGIT NINE
unicode_bidi_class(0x1C4D, 0x1C4F, 'L'). % Lo   [3] LEPCHA LETTER TTA..LEPCHA LETTER DDA
unicode_bidi_class(0x1C50, 0x1C59, 'L'). % Nd  [10] OL CHIKI DIGIT ZERO..OL CHIKI DIGIT NINE
unicode_bidi_class(0x1C5A, 0x1C77, 'L'). % Lo  [30] OL CHIKI LETTER LA..OL CHIKI LETTER OH
unicode_bidi_class(0x1C78, 0x1C7D, 'L'). % Lm   [6] OL CHIKI MU TTUDDAG..OL CHIKI AHAD
unicode_bidi_class(0x1C7E, 0x1C7F, 'L'). % Po   [2] OL CHIKI PUNCTUATION MUCAAD..OL CHIKI PUNCTUATION DOUBLE MUCAAD
unicode_bidi_class(0x1CC0, 0x1CC7, 'L'). % Po   [8] SUNDANESE PUNCTUATION BINDU SURYA..SUNDANESE PUNCTUATION BINDU BA SATANGA
unicode_bidi_class(0x1CD3, 0x1CD3, 'L'). % Po       VEDIC SIGN NIHSHVASA
unicode_bidi_class(0x1CE1, 0x1CE1, 'L'). % Mc       VEDIC TONE ATHARVAVEDIC INDEPENDENT SVARITA
unicode_bidi_class(0x1CE9, 0x1CEC, 'L'). % Lo   [4] VEDIC SIGN ANUSVARA ANTARGOMUKHA..VEDIC SIGN ANUSVARA VAMAGOMUKHA WITH TAIL
unicode_bidi_class(0x1CEE, 0x1CF1, 'L'). % Lo   [4] VEDIC SIGN HEXIFORM LONG ANUSVARA..VEDIC SIGN ANUSVARA UBHAYATO MUKHA
unicode_bidi_class(0x1CF2, 0x1CF3, 'L'). % Mc   [2] VEDIC SIGN ARDHAVISARGA..VEDIC SIGN ROTATED ARDHAVISARGA
unicode_bidi_class(0x1CF5, 0x1CF6, 'L'). % Lo   [2] VEDIC SIGN JIHVAMULIYA..VEDIC SIGN UPADHMANIYA
unicode_bidi_class(0x1D00, 0x1D2B, 'L'). % L&  [44] LATIN LETTER SMALL CAPITAL A..CYRILLIC LETTER SMALL CAPITAL EL
unicode_bidi_class(0x1D2C, 0x1D6A, 'L'). % Lm  [63] MODIFIER LETTER CAPITAL A..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_bidi_class(0x1D6B, 0x1D77, 'L'). % L&  [13] LATIN SMALL LETTER UE..LATIN SMALL LETTER TURNED G
unicode_bidi_class(0x1D78, 0x1D78, 'L'). % Lm       MODIFIER LETTER CYRILLIC EN
unicode_bidi_class(0x1D79, 0x1D9A, 'L'). % L&  [34] LATIN SMALL LETTER INSULAR G..LATIN SMALL LETTER EZH WITH RETROFLEX HOOK
unicode_bidi_class(0x1D9B, 0x1DBF, 'L'). % Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_bidi_class(0x1E00, 0x1F15, 'L'). % L& [278] LATIN CAPITAL LETTER A WITH RING BELOW..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_bidi_class(0x1F18, 0x1F1D, 'L'). % L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_bidi_class(0x1F20, 0x1F45, 'L'). % L&  [38] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_bidi_class(0x1F48, 0x1F4D, 'L'). % L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_bidi_class(0x1F50, 0x1F57, 'L'). % L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_bidi_class(0x1F59, 0x1F59, 'L'). % L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_bidi_class(0x1F5B, 0x1F5B, 'L'). % L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_bidi_class(0x1F5D, 0x1F5D, 'L'). % L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_bidi_class(0x1F5F, 0x1F7D, 'L'). % L&  [31] GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_bidi_class(0x1F80, 0x1FB4, 'L'). % L&  [53] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_bidi_class(0x1FB6, 0x1FBC, 'L'). % L&   [7] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_bidi_class(0x1FBE, 0x1FBE, 'L'). % L&       GREEK PROSGEGRAMMENI
unicode_bidi_class(0x1FC2, 0x1FC4, 'L'). % L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_bidi_class(0x1FC6, 0x1FCC, 'L'). % L&   [7] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_bidi_class(0x1FD0, 0x1FD3, 'L'). % L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_bidi_class(0x1FD6, 0x1FDB, 'L'). % L&   [6] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_bidi_class(0x1FE0, 0x1FEC, 'L'). % L&  [13] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_bidi_class(0x1FF2, 0x1FF4, 'L'). % L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_bidi_class(0x1FF6, 0x1FFC, 'L'). % L&   [7] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_bidi_class(0x200E, 0x200E, 'L'). % Cf       LEFT-TO-RIGHT MARK
unicode_bidi_class(0x2071, 0x2071, 'L'). % Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_bidi_class(0x207F, 0x207F, 'L'). % Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_bidi_class(0x2090, 0x209C, 'L'). % Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_bidi_class(0x2102, 0x2102, 'L'). % L&       DOUBLE-STRUCK CAPITAL C
unicode_bidi_class(0x2107, 0x2107, 'L'). % L&       EULER CONSTANT
unicode_bidi_class(0x210A, 0x2113, 'L'). % L&  [10] SCRIPT SMALL G..SCRIPT SMALL L
unicode_bidi_class(0x2115, 0x2115, 'L'). % L&       DOUBLE-STRUCK CAPITAL N
unicode_bidi_class(0x2119, 0x211D, 'L'). % L&   [5] DOUBLE-STRUCK CAPITAL P..DOUBLE-STRUCK CAPITAL R
unicode_bidi_class(0x2124, 0x2124, 'L'). % L&       DOUBLE-STRUCK CAPITAL Z
unicode_bidi_class(0x2126, 0x2126, 'L'). % L&       OHM SIGN
unicode_bidi_class(0x2128, 0x2128, 'L'). % L&       BLACK-LETTER CAPITAL Z
unicode_bidi_class(0x212A, 0x212D, 'L'). % L&   [4] KELVIN SIGN..BLACK-LETTER CAPITAL C
unicode_bidi_class(0x212F, 0x2134, 'L'). % L&   [6] SCRIPT SMALL E..SCRIPT SMALL O
unicode_bidi_class(0x2135, 0x2138, 'L'). % Lo   [4] ALEF SYMBOL..DALET SYMBOL
unicode_bidi_class(0x2139, 0x2139, 'L'). % L&       INFORMATION SOURCE
unicode_bidi_class(0x213C, 0x213F, 'L'). % L&   [4] DOUBLE-STRUCK SMALL PI..DOUBLE-STRUCK CAPITAL PI
unicode_bidi_class(0x2145, 0x2149, 'L'). % L&   [5] DOUBLE-STRUCK ITALIC CAPITAL D..DOUBLE-STRUCK ITALIC SMALL J
unicode_bidi_class(0x214E, 0x214E, 'L'). % L&       TURNED SMALL F
unicode_bidi_class(0x214F, 0x214F, 'L'). % So       SYMBOL FOR SAMARITAN SOURCE
unicode_bidi_class(0x2160, 0x2182, 'L'). % Nl  [35] ROMAN NUMERAL ONE..ROMAN NUMERAL TEN THOUSAND
unicode_bidi_class(0x2183, 0x2184, 'L'). % L&   [2] ROMAN NUMERAL REVERSED ONE HUNDRED..LATIN SMALL LETTER REVERSED C
unicode_bidi_class(0x2185, 0x2188, 'L'). % Nl   [4] ROMAN NUMERAL SIX LATE FORM..ROMAN NUMERAL ONE HUNDRED THOUSAND
unicode_bidi_class(0x2336, 0x237A, 'L'). % So  [69] APL FUNCTIONAL SYMBOL I-BEAM..APL FUNCTIONAL SYMBOL ALPHA
unicode_bidi_class(0x2395, 0x2395, 'L'). % So       APL FUNCTIONAL SYMBOL QUAD
unicode_bidi_class(0x249C, 0x24E9, 'L'). % So  [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_bidi_class(0x26AC, 0x26AC, 'L'). % So       MEDIUM SMALL WHITE CIRCLE
unicode_bidi_class(0x2800, 0x28FF, 'L'). % So [256] BRAILLE PATTERN BLANK..BRAILLE PATTERN DOTS-12345678
unicode_bidi_class(0x2C00, 0x2C2E, 'L'). % L&  [47] GLAGOLITIC CAPITAL LETTER AZU..GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_bidi_class(0x2C30, 0x2C5E, 'L'). % L&  [47] GLAGOLITIC SMALL LETTER AZU..GLAGOLITIC SMALL LETTER LATINATE MYSLITE
unicode_bidi_class(0x2C60, 0x2C7B, 'L'). % L&  [28] LATIN CAPITAL LETTER L WITH DOUBLE BAR..LATIN LETTER SMALL CAPITAL TURNED E
unicode_bidi_class(0x2C7C, 0x2C7D, 'L'). % Lm   [2] LATIN SUBSCRIPT SMALL LETTER J..MODIFIER LETTER CAPITAL V
unicode_bidi_class(0x2C7E, 0x2CE4, 'L'). % L& [103] LATIN CAPITAL LETTER S WITH SWASH TAIL..COPTIC SYMBOL KAI
unicode_bidi_class(0x2CEB, 0x2CEE, 'L'). % L&   [4] COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI..COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
unicode_bidi_class(0x2CF2, 0x2CF3, 'L'). % L&   [2] COPTIC CAPITAL LETTER BOHAIRIC KHEI..COPTIC SMALL LETTER BOHAIRIC KHEI
unicode_bidi_class(0x2D00, 0x2D25, 'L'). % L&  [38] GEORGIAN SMALL LETTER AN..GEORGIAN SMALL LETTER HOE
unicode_bidi_class(0x2D27, 0x2D27, 'L'). % L&       GEORGIAN SMALL LETTER YN
unicode_bidi_class(0x2D2D, 0x2D2D, 'L'). % L&       GEORGIAN SMALL LETTER AEN
unicode_bidi_class(0x2D30, 0x2D67, 'L'). % Lo  [56] TIFINAGH LETTER YA..TIFINAGH LETTER YO
unicode_bidi_class(0x2D6F, 0x2D6F, 'L'). % Lm       TIFINAGH MODIFIER LETTER LABIALIZATION MARK
unicode_bidi_class(0x2D70, 0x2D70, 'L'). % Po       TIFINAGH SEPARATOR MARK
unicode_bidi_class(0x2D80, 0x2D96, 'L'). % Lo  [23] ETHIOPIC SYLLABLE LOA..ETHIOPIC SYLLABLE GGWE
unicode_bidi_class(0x2DA0, 0x2DA6, 'L'). % Lo   [7] ETHIOPIC SYLLABLE SSA..ETHIOPIC SYLLABLE SSO
unicode_bidi_class(0x2DA8, 0x2DAE, 'L'). % Lo   [7] ETHIOPIC SYLLABLE CCA..ETHIOPIC SYLLABLE CCO
unicode_bidi_class(0x2DB0, 0x2DB6, 'L'). % Lo   [7] ETHIOPIC SYLLABLE ZZA..ETHIOPIC SYLLABLE ZZO
unicode_bidi_class(0x2DB8, 0x2DBE, 'L'). % Lo   [7] ETHIOPIC SYLLABLE CCHA..ETHIOPIC SYLLABLE CCHO
unicode_bidi_class(0x2DC0, 0x2DC6, 'L'). % Lo   [7] ETHIOPIC SYLLABLE QYA..ETHIOPIC SYLLABLE QYO
unicode_bidi_class(0x2DC8, 0x2DCE, 'L'). % Lo   [7] ETHIOPIC SYLLABLE KYA..ETHIOPIC SYLLABLE KYO
unicode_bidi_class(0x2DD0, 0x2DD6, 'L'). % Lo   [7] ETHIOPIC SYLLABLE XYA..ETHIOPIC SYLLABLE XYO
unicode_bidi_class(0x2DD8, 0x2DDE, 'L'). % Lo   [7] ETHIOPIC SYLLABLE GYA..ETHIOPIC SYLLABLE GYO
unicode_bidi_class(0x3005, 0x3005, 'L'). % Lm       IDEOGRAPHIC ITERATION MARK
unicode_bidi_class(0x3006, 0x3006, 'L'). % Lo       IDEOGRAPHIC CLOSING MARK
unicode_bidi_class(0x3007, 0x3007, 'L'). % Nl       IDEOGRAPHIC NUMBER ZERO
unicode_bidi_class(0x3021, 0x3029, 'L'). % Nl   [9] HANGZHOU NUMERAL ONE..HANGZHOU NUMERAL NINE
unicode_bidi_class(0x302E, 0x302F, 'L'). % Mc   [2] HANGUL SINGLE DOT TONE MARK..HANGUL DOUBLE DOT TONE MARK
unicode_bidi_class(0x3031, 0x3035, 'L'). % Lm   [5] VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF
unicode_bidi_class(0x3038, 0x303A, 'L'). % Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
unicode_bidi_class(0x303B, 0x303B, 'L'). % Lm       VERTICAL IDEOGRAPHIC ITERATION MARK
unicode_bidi_class(0x303C, 0x303C, 'L'). % Lo       MASU MARK
unicode_bidi_class(0x3041, 0x3096, 'L'). % Lo  [86] HIRAGANA LETTER SMALL A..HIRAGANA LETTER SMALL KE
unicode_bidi_class(0x309D, 0x309E, 'L'). % Lm   [2] HIRAGANA ITERATION MARK..HIRAGANA VOICED ITERATION MARK
unicode_bidi_class(0x309F, 0x309F, 'L'). % Lo       HIRAGANA DIGRAPH YORI
unicode_bidi_class(0x30A1, 0x30FA, 'L'). % Lo  [90] KATAKANA LETTER SMALL A..KATAKANA LETTER VO
unicode_bidi_class(0x30FC, 0x30FE, 'L'). % Lm   [3] KATAKANA-HIRAGANA PROLONGED SOUND MARK..KATAKANA VOICED ITERATION MARK
unicode_bidi_class(0x30FF, 0x30FF, 'L'). % Lo       KATAKANA DIGRAPH KOTO
unicode_bidi_class(0x3105, 0x312D, 'L'). % Lo  [41] BOPOMOFO LETTER B..BOPOMOFO LETTER IH
unicode_bidi_class(0x3131, 0x318E, 'L'). % Lo  [94] HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE
unicode_bidi_class(0x3190, 0x3191, 'L'). % So   [2] IDEOGRAPHIC ANNOTATION LINKING MARK..IDEOGRAPHIC ANNOTATION REVERSE MARK
unicode_bidi_class(0x3192, 0x3195, 'L'). % No   [4] IDEOGRAPHIC ANNOTATION ONE MARK..IDEOGRAPHIC ANNOTATION FOUR MARK
unicode_bidi_class(0x3196, 0x319F, 'L'). % So  [10] IDEOGRAPHIC ANNOTATION TOP MARK..IDEOGRAPHIC ANNOTATION MAN MARK
unicode_bidi_class(0x31A0, 0x31BA, 'L'). % Lo  [27] BOPOMOFO LETTER BU..BOPOMOFO LETTER ZY
unicode_bidi_class(0x31F0, 0x31FF, 'L'). % Lo  [16] KATAKANA LETTER SMALL KU..KATAKANA LETTER SMALL RO
unicode_bidi_class(0x3200, 0x321C, 'L'). % So  [29] PARENTHESIZED HANGUL KIYEOK..PARENTHESIZED HANGUL CIEUC U
unicode_bidi_class(0x3220, 0x3229, 'L'). % No  [10] PARENTHESIZED IDEOGRAPH ONE..PARENTHESIZED IDEOGRAPH TEN
unicode_bidi_class(0x322A, 0x3247, 'L'). % So  [30] PARENTHESIZED IDEOGRAPH MOON..CIRCLED IDEOGRAPH KOTO
unicode_bidi_class(0x3248, 0x324F, 'L'). % No   [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
unicode_bidi_class(0x3260, 0x327B, 'L'). % So  [28] CIRCLED HANGUL KIYEOK..CIRCLED HANGUL HIEUH A
unicode_bidi_class(0x327F, 0x327F, 'L'). % So       KOREAN STANDARD SYMBOL
unicode_bidi_class(0x3280, 0x3289, 'L'). % No  [10] CIRCLED IDEOGRAPH ONE..CIRCLED IDEOGRAPH TEN
unicode_bidi_class(0x328A, 0x32B0, 'L'). % So  [39] CIRCLED IDEOGRAPH MOON..CIRCLED IDEOGRAPH NIGHT
unicode_bidi_class(0x32C0, 0x32CB, 'L'). % So  [12] IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY..IDEOGRAPHIC TELEGRAPH SYMBOL FOR DECEMBER
unicode_bidi_class(0x32D0, 0x32FE, 'L'). % So  [47] CIRCLED KATAKANA A..CIRCLED KATAKANA WO
unicode_bidi_class(0x3300, 0x3376, 'L'). % So [119] SQUARE APAATO..SQUARE PC
unicode_bidi_class(0x337B, 0x33DD, 'L'). % So  [99] SQUARE ERA NAME HEISEI..SQUARE WB
unicode_bidi_class(0x33E0, 0x33FE, 'L'). % So  [31] IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY ONE..IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTY-ONE
unicode_bidi_class(0x3400, 0x4DB5, 'L'). % Lo [6582] CJK UNIFIED IDEOGRAPH-3400..CJK UNIFIED IDEOGRAPH-4DB5
unicode_bidi_class(0x4E00, 0x9FCC, 'L'). % Lo [20941] CJK UNIFIED IDEOGRAPH-4E00..CJK UNIFIED IDEOGRAPH-9FCC
unicode_bidi_class(0xA000, 0xA014, 'L'). % Lo  [21] YI SYLLABLE IT..YI SYLLABLE E
unicode_bidi_class(0xA015, 0xA015, 'L'). % Lm       YI SYLLABLE WU
unicode_bidi_class(0xA016, 0xA48C, 'L'). % Lo [1143] YI SYLLABLE BIT..YI SYLLABLE YYR
unicode_bidi_class(0xA4D0, 0xA4F7, 'L'). % Lo  [40] LISU LETTER BA..LISU LETTER OE
unicode_bidi_class(0xA4F8, 0xA4FD, 'L'). % Lm   [6] LISU LETTER TONE MYA TI..LISU LETTER TONE MYA JEU
unicode_bidi_class(0xA4FE, 0xA4FF, 'L'). % Po   [2] LISU PUNCTUATION COMMA..LISU PUNCTUATION FULL STOP
unicode_bidi_class(0xA500, 0xA60B, 'L'). % Lo [268] VAI SYLLABLE EE..VAI SYLLABLE NG
unicode_bidi_class(0xA60C, 0xA60C, 'L'). % Lm       VAI SYLLABLE LENGTHENER
unicode_bidi_class(0xA610, 0xA61F, 'L'). % Lo  [16] VAI SYLLABLE NDOLE FA..VAI SYMBOL JONG
unicode_bidi_class(0xA620, 0xA629, 'L'). % Nd  [10] VAI DIGIT ZERO..VAI DIGIT NINE
unicode_bidi_class(0xA62A, 0xA62B, 'L'). % Lo   [2] VAI SYLLABLE NDOLE MA..VAI SYLLABLE NDOLE DO
unicode_bidi_class(0xA640, 0xA66D, 'L'). % L&  [46] CYRILLIC CAPITAL LETTER ZEMLYA..CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
unicode_bidi_class(0xA66E, 0xA66E, 'L'). % Lo       CYRILLIC LETTER MULTIOCULAR O
unicode_bidi_class(0xA680, 0xA697, 'L'). % L&  [24] CYRILLIC CAPITAL LETTER DWE..CYRILLIC SMALL LETTER SHWE
unicode_bidi_class(0xA6A0, 0xA6E5, 'L'). % Lo  [70] BAMUM LETTER A..BAMUM LETTER KI
unicode_bidi_class(0xA6E6, 0xA6EF, 'L'). % Nl  [10] BAMUM LETTER MO..BAMUM LETTER KOGHOM
unicode_bidi_class(0xA6F2, 0xA6F7, 'L'). % Po   [6] BAMUM NJAEMLI..BAMUM QUESTION MARK
unicode_bidi_class(0xA722, 0xA76F, 'L'). % L&  [78] LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF..LATIN SMALL LETTER CON
unicode_bidi_class(0xA770, 0xA770, 'L'). % Lm       MODIFIER LETTER US
unicode_bidi_class(0xA771, 0xA787, 'L'). % L&  [23] LATIN SMALL LETTER DUM..LATIN SMALL LETTER INSULAR T
unicode_bidi_class(0xA789, 0xA78A, 'L'). % Sk   [2] MODIFIER LETTER COLON..MODIFIER LETTER SHORT EQUALS SIGN
unicode_bidi_class(0xA78B, 0xA78E, 'L'). % L&   [4] LATIN CAPITAL LETTER SALTILLO..LATIN SMALL LETTER L WITH RETROFLEX HOOK AND BELT
unicode_bidi_class(0xA790, 0xA793, 'L'). % L&   [4] LATIN CAPITAL LETTER N WITH DESCENDER..LATIN SMALL LETTER C WITH BAR
unicode_bidi_class(0xA7A0, 0xA7AA, 'L'). % L&  [11] LATIN CAPITAL LETTER G WITH OBLIQUE STROKE..LATIN CAPITAL LETTER H WITH HOOK
unicode_bidi_class(0xA7F8, 0xA7F9, 'L'). % Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_bidi_class(0xA7FA, 0xA7FA, 'L'). % L&       LATIN LETTER SMALL CAPITAL TURNED M
unicode_bidi_class(0xA7FB, 0xA801, 'L'). % Lo   [7] LATIN EPIGRAPHIC LETTER REVERSED F..SYLOTI NAGRI LETTER I
unicode_bidi_class(0xA803, 0xA805, 'L'). % Lo   [3] SYLOTI NAGRI LETTER U..SYLOTI NAGRI LETTER O
unicode_bidi_class(0xA807, 0xA80A, 'L'). % Lo   [4] SYLOTI NAGRI LETTER KO..SYLOTI NAGRI LETTER GHO
unicode_bidi_class(0xA80C, 0xA822, 'L'). % Lo  [23] SYLOTI NAGRI LETTER CO..SYLOTI NAGRI LETTER HO
unicode_bidi_class(0xA823, 0xA824, 'L'). % Mc   [2] SYLOTI NAGRI VOWEL SIGN A..SYLOTI NAGRI VOWEL SIGN I
unicode_bidi_class(0xA827, 0xA827, 'L'). % Mc       SYLOTI NAGRI VOWEL SIGN OO
unicode_bidi_class(0xA830, 0xA835, 'L'). % No   [6] NORTH INDIC FRACTION ONE QUARTER..NORTH INDIC FRACTION THREE SIXTEENTHS
unicode_bidi_class(0xA836, 0xA837, 'L'). % So   [2] NORTH INDIC QUARTER MARK..NORTH INDIC PLACEHOLDER MARK
unicode_bidi_class(0xA840, 0xA873, 'L'). % Lo  [52] PHAGS-PA LETTER KA..PHAGS-PA LETTER CANDRABINDU
unicode_bidi_class(0xA880, 0xA881, 'L'). % Mc   [2] SAURASHTRA SIGN ANUSVARA..SAURASHTRA SIGN VISARGA
unicode_bidi_class(0xA882, 0xA8B3, 'L'). % Lo  [50] SAURASHTRA LETTER A..SAURASHTRA LETTER LLA
unicode_bidi_class(0xA8B4, 0xA8C3, 'L'). % Mc  [16] SAURASHTRA CONSONANT SIGN HAARU..SAURASHTRA VOWEL SIGN AU
unicode_bidi_class(0xA8CE, 0xA8CF, 'L'). % Po   [2] SAURASHTRA DANDA..SAURASHTRA DOUBLE DANDA
unicode_bidi_class(0xA8D0, 0xA8D9, 'L'). % Nd  [10] SAURASHTRA DIGIT ZERO..SAURASHTRA DIGIT NINE
unicode_bidi_class(0xA8F2, 0xA8F7, 'L'). % Lo   [6] DEVANAGARI SIGN SPACING CANDRABINDU..DEVANAGARI SIGN CANDRABINDU AVAGRAHA
unicode_bidi_class(0xA8F8, 0xA8FA, 'L'). % Po   [3] DEVANAGARI SIGN PUSHPIKA..DEVANAGARI CARET
unicode_bidi_class(0xA8FB, 0xA8FB, 'L'). % Lo       DEVANAGARI HEADSTROKE
unicode_bidi_class(0xA900, 0xA909, 'L'). % Nd  [10] KAYAH LI DIGIT ZERO..KAYAH LI DIGIT NINE
unicode_bidi_class(0xA90A, 0xA925, 'L'). % Lo  [28] KAYAH LI LETTER KA..KAYAH LI LETTER OO
unicode_bidi_class(0xA92E, 0xA92F, 'L'). % Po   [2] KAYAH LI SIGN CWI..KAYAH LI SIGN SHYA
unicode_bidi_class(0xA930, 0xA946, 'L'). % Lo  [23] REJANG LETTER KA..REJANG LETTER A
unicode_bidi_class(0xA952, 0xA953, 'L'). % Mc   [2] REJANG CONSONANT SIGN H..REJANG VIRAMA
unicode_bidi_class(0xA95F, 0xA95F, 'L'). % Po       REJANG SECTION MARK
unicode_bidi_class(0xA960, 0xA97C, 'L'). % Lo  [29] HANGUL CHOSEONG TIKEUT-MIEUM..HANGUL CHOSEONG SSANGYEORINHIEUH
unicode_bidi_class(0xA983, 0xA983, 'L'). % Mc       JAVANESE SIGN WIGNYAN
unicode_bidi_class(0xA984, 0xA9B2, 'L'). % Lo  [47] JAVANESE LETTER A..JAVANESE LETTER HA
unicode_bidi_class(0xA9B4, 0xA9B5, 'L'). % Mc   [2] JAVANESE VOWEL SIGN TARUNG..JAVANESE VOWEL SIGN TOLONG
unicode_bidi_class(0xA9BA, 0xA9BB, 'L'). % Mc   [2] JAVANESE VOWEL SIGN TALING..JAVANESE VOWEL SIGN DIRGA MURE
unicode_bidi_class(0xA9BD, 0xA9C0, 'L'). % Mc   [4] JAVANESE CONSONANT SIGN KERET..JAVANESE PANGKON
unicode_bidi_class(0xA9C1, 0xA9CD, 'L'). % Po  [13] JAVANESE LEFT RERENGGAN..JAVANESE TURNED PADA PISELEH
unicode_bidi_class(0xA9CF, 0xA9CF, 'L'). % Lm       JAVANESE PANGRANGKEP
unicode_bidi_class(0xA9D0, 0xA9D9, 'L'). % Nd  [10] JAVANESE DIGIT ZERO..JAVANESE DIGIT NINE
unicode_bidi_class(0xA9DE, 0xA9DF, 'L'). % Po   [2] JAVANESE PADA TIRTA TUMETES..JAVANESE PADA ISEN-ISEN
unicode_bidi_class(0xAA00, 0xAA28, 'L'). % Lo  [41] CHAM LETTER A..CHAM LETTER HA
unicode_bidi_class(0xAA2F, 0xAA30, 'L'). % Mc   [2] CHAM VOWEL SIGN O..CHAM VOWEL SIGN AI
unicode_bidi_class(0xAA33, 0xAA34, 'L'). % Mc   [2] CHAM CONSONANT SIGN YA..CHAM CONSONANT SIGN RA
unicode_bidi_class(0xAA40, 0xAA42, 'L'). % Lo   [3] CHAM LETTER FINAL K..CHAM LETTER FINAL NG
unicode_bidi_class(0xAA44, 0xAA4B, 'L'). % Lo   [8] CHAM LETTER FINAL CH..CHAM LETTER FINAL SS
unicode_bidi_class(0xAA4D, 0xAA4D, 'L'). % Mc       CHAM CONSONANT SIGN FINAL H
unicode_bidi_class(0xAA50, 0xAA59, 'L'). % Nd  [10] CHAM DIGIT ZERO..CHAM DIGIT NINE
unicode_bidi_class(0xAA5C, 0xAA5F, 'L'). % Po   [4] CHAM PUNCTUATION SPIRAL..CHAM PUNCTUATION TRIPLE DANDA
unicode_bidi_class(0xAA60, 0xAA6F, 'L'). % Lo  [16] MYANMAR LETTER KHAMTI GA..MYANMAR LETTER KHAMTI FA
unicode_bidi_class(0xAA70, 0xAA70, 'L'). % Lm       MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION
unicode_bidi_class(0xAA71, 0xAA76, 'L'). % Lo   [6] MYANMAR LETTER KHAMTI XA..MYANMAR LOGOGRAM KHAMTI HM
unicode_bidi_class(0xAA77, 0xAA79, 'L'). % So   [3] MYANMAR SYMBOL AITON EXCLAMATION..MYANMAR SYMBOL AITON TWO
unicode_bidi_class(0xAA7A, 0xAA7A, 'L'). % Lo       MYANMAR LETTER AITON RA
unicode_bidi_class(0xAA7B, 0xAA7B, 'L'). % Mc       MYANMAR SIGN PAO KAREN TONE
unicode_bidi_class(0xAA80, 0xAAAF, 'L'). % Lo  [48] TAI VIET LETTER LOW KO..TAI VIET LETTER HIGH O
unicode_bidi_class(0xAAB1, 0xAAB1, 'L'). % Lo       TAI VIET VOWEL AA
unicode_bidi_class(0xAAB5, 0xAAB6, 'L'). % Lo   [2] TAI VIET VOWEL E..TAI VIET VOWEL O
unicode_bidi_class(0xAAB9, 0xAABD, 'L'). % Lo   [5] TAI VIET VOWEL UEA..TAI VIET VOWEL AN
unicode_bidi_class(0xAAC0, 0xAAC0, 'L'). % Lo       TAI VIET TONE MAI NUENG
unicode_bidi_class(0xAAC2, 0xAAC2, 'L'). % Lo       TAI VIET TONE MAI SONG
unicode_bidi_class(0xAADB, 0xAADC, 'L'). % Lo   [2] TAI VIET SYMBOL KON..TAI VIET SYMBOL NUENG
unicode_bidi_class(0xAADD, 0xAADD, 'L'). % Lm       TAI VIET SYMBOL SAM
unicode_bidi_class(0xAADE, 0xAADF, 'L'). % Po   [2] TAI VIET SYMBOL HO HOI..TAI VIET SYMBOL KOI KOI
unicode_bidi_class(0xAAE0, 0xAAEA, 'L'). % Lo  [11] MEETEI MAYEK LETTER E..MEETEI MAYEK LETTER SSA
unicode_bidi_class(0xAAEB, 0xAAEB, 'L'). % Mc       MEETEI MAYEK VOWEL SIGN II
unicode_bidi_class(0xAAEE, 0xAAEF, 'L'). % Mc   [2] MEETEI MAYEK VOWEL SIGN AU..MEETEI MAYEK VOWEL SIGN AAU
unicode_bidi_class(0xAAF0, 0xAAF1, 'L'). % Po   [2] MEETEI MAYEK CHEIKHAN..MEETEI MAYEK AHANG KHUDAM
unicode_bidi_class(0xAAF2, 0xAAF2, 'L'). % Lo       MEETEI MAYEK ANJI
unicode_bidi_class(0xAAF3, 0xAAF4, 'L'). % Lm   [2] MEETEI MAYEK SYLLABLE REPETITION MARK..MEETEI MAYEK WORD REPETITION MARK
unicode_bidi_class(0xAAF5, 0xAAF5, 'L'). % Mc       MEETEI MAYEK VOWEL SIGN VISARGA
unicode_bidi_class(0xAB01, 0xAB06, 'L'). % Lo   [6] ETHIOPIC SYLLABLE TTHU..ETHIOPIC SYLLABLE TTHO
unicode_bidi_class(0xAB09, 0xAB0E, 'L'). % Lo   [6] ETHIOPIC SYLLABLE DDHU..ETHIOPIC SYLLABLE DDHO
unicode_bidi_class(0xAB11, 0xAB16, 'L'). % Lo   [6] ETHIOPIC SYLLABLE DZU..ETHIOPIC SYLLABLE DZO
unicode_bidi_class(0xAB20, 0xAB26, 'L'). % Lo   [7] ETHIOPIC SYLLABLE CCHHA..ETHIOPIC SYLLABLE CCHHO
unicode_bidi_class(0xAB28, 0xAB2E, 'L'). % Lo   [7] ETHIOPIC SYLLABLE BBA..ETHIOPIC SYLLABLE BBO
unicode_bidi_class(0xABC0, 0xABE2, 'L'). % Lo  [35] MEETEI MAYEK LETTER KOK..MEETEI MAYEK LETTER I LONSUM
unicode_bidi_class(0xABE3, 0xABE4, 'L'). % Mc   [2] MEETEI MAYEK VOWEL SIGN ONAP..MEETEI MAYEK VOWEL SIGN INAP
unicode_bidi_class(0xABE6, 0xABE7, 'L'). % Mc   [2] MEETEI MAYEK VOWEL SIGN YENAP..MEETEI MAYEK VOWEL SIGN SOUNAP
unicode_bidi_class(0xABE9, 0xABEA, 'L'). % Mc   [2] MEETEI MAYEK VOWEL SIGN CHEINAP..MEETEI MAYEK VOWEL SIGN NUNG
unicode_bidi_class(0xABEB, 0xABEB, 'L'). % Po       MEETEI MAYEK CHEIKHEI
unicode_bidi_class(0xABEC, 0xABEC, 'L'). % Mc       MEETEI MAYEK LUM IYEK
unicode_bidi_class(0xABF0, 0xABF9, 'L'). % Nd  [10] MEETEI MAYEK DIGIT ZERO..MEETEI MAYEK DIGIT NINE
unicode_bidi_class(0xAC00, 0xD7A3, 'L'). % Lo [11172] HANGUL SYLLABLE GA..HANGUL SYLLABLE HIH
unicode_bidi_class(0xD7B0, 0xD7C6, 'L'). % Lo  [23] HANGUL JUNGSEONG O-YEO..HANGUL JUNGSEONG ARAEA-E
unicode_bidi_class(0xD7CB, 0xD7FB, 'L'). % Lo  [49] HANGUL JONGSEONG NIEUN-RIEUL..HANGUL JONGSEONG PHIEUPH-THIEUTH
unicode_bidi_class(0xE000, 0xF8FF, 'L'). % Co [6400] <private-use-E000>..<private-use-F8FF>
unicode_bidi_class(0xF900, 0xFA6D, 'L'). % Lo [366] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA6D
unicode_bidi_class(0xFA70, 0xFAD9, 'L'). % Lo [106] CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9
unicode_bidi_class(0xFB00, 0xFB06, 'L'). % L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_bidi_class(0xFB13, 0xFB17, 'L'). % L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_bidi_class(0xFF21, 0xFF3A, 'L'). % L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_bidi_class(0xFF41, 0xFF5A, 'L'). % L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_bidi_class(0xFF66, 0xFF6F, 'L'). % Lo  [10] HALFWIDTH KATAKANA LETTER WO..HALFWIDTH KATAKANA LETTER SMALL TU
unicode_bidi_class(0xFF70, 0xFF70, 'L'). % Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_bidi_class(0xFF71, 0xFF9D, 'L'). % Lo  [45] HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N
unicode_bidi_class(0xFF9E, 0xFF9F, 'L'). % Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
unicode_bidi_class(0xFFA0, 0xFFBE, 'L'). % Lo  [31] HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH
unicode_bidi_class(0xFFC2, 0xFFC7, 'L'). % Lo   [6] HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E
unicode_bidi_class(0xFFCA, 0xFFCF, 'L'). % Lo   [6] HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE
unicode_bidi_class(0xFFD2, 0xFFD7, 'L'). % Lo   [6] HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU
unicode_bidi_class(0xFFDA, 0xFFDC, 'L'). % Lo   [3] HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I
unicode_bidi_class(0x10000, 0x1000B, 'L'). % Lo  [12] LINEAR B SYLLABLE B008 A..LINEAR B SYLLABLE B046 JE
unicode_bidi_class(0x1000D, 0x10026, 'L'). % Lo  [26] LINEAR B SYLLABLE B036 JO..LINEAR B SYLLABLE B032 QO
unicode_bidi_class(0x10028, 0x1003A, 'L'). % Lo  [19] LINEAR B SYLLABLE B060 RA..LINEAR B SYLLABLE B042 WO
unicode_bidi_class(0x1003C, 0x1003D, 'L'). % Lo   [2] LINEAR B SYLLABLE B017 ZA..LINEAR B SYLLABLE B074 ZE
unicode_bidi_class(0x1003F, 0x1004D, 'L'). % Lo  [15] LINEAR B SYLLABLE B020 ZO..LINEAR B SYLLABLE B091 TWO
unicode_bidi_class(0x10050, 0x1005D, 'L'). % Lo  [14] LINEAR B SYMBOL B018..LINEAR B SYMBOL B089
unicode_bidi_class(0x10080, 0x100FA, 'L'). % Lo [123] LINEAR B IDEOGRAM B100 MAN..LINEAR B IDEOGRAM VESSEL B305
unicode_bidi_class(0x10100, 0x10100, 'L'). % Po       AEGEAN WORD SEPARATOR LINE
unicode_bidi_class(0x10102, 0x10102, 'L'). % Po       AEGEAN CHECK MARK
unicode_bidi_class(0x10107, 0x10133, 'L'). % No  [45] AEGEAN NUMBER ONE..AEGEAN NUMBER NINETY THOUSAND
unicode_bidi_class(0x10137, 0x1013F, 'L'). % So   [9] AEGEAN WEIGHT BASE UNIT..AEGEAN MEASURE THIRD SUBUNIT
unicode_bidi_class(0x101D0, 0x101FC, 'L'). % So  [45] PHAISTOS DISC SIGN PEDESTRIAN..PHAISTOS DISC SIGN WAVY BAND
unicode_bidi_class(0x10280, 0x1029C, 'L'). % Lo  [29] LYCIAN LETTER A..LYCIAN LETTER X
unicode_bidi_class(0x102A0, 0x102D0, 'L'). % Lo  [49] CARIAN LETTER A..CARIAN LETTER UUU3
unicode_bidi_class(0x10300, 0x1031E, 'L'). % Lo  [31] OLD ITALIC LETTER A..OLD ITALIC LETTER UU
unicode_bidi_class(0x10320, 0x10323, 'L'). % No   [4] OLD ITALIC NUMERAL ONE..OLD ITALIC NUMERAL FIFTY
unicode_bidi_class(0x10330, 0x10340, 'L'). % Lo  [17] GOTHIC LETTER AHSA..GOTHIC LETTER PAIRTHRA
unicode_bidi_class(0x10341, 0x10341, 'L'). % Nl       GOTHIC LETTER NINETY
unicode_bidi_class(0x10342, 0x10349, 'L'). % Lo   [8] GOTHIC LETTER RAIDA..GOTHIC LETTER OTHAL
unicode_bidi_class(0x1034A, 0x1034A, 'L'). % Nl       GOTHIC LETTER NINE HUNDRED
unicode_bidi_class(0x10380, 0x1039D, 'L'). % Lo  [30] UGARITIC LETTER ALPA..UGARITIC LETTER SSU
unicode_bidi_class(0x1039F, 0x1039F, 'L'). % Po       UGARITIC WORD DIVIDER
unicode_bidi_class(0x103A0, 0x103C3, 'L'). % Lo  [36] OLD PERSIAN SIGN A..OLD PERSIAN SIGN HA
unicode_bidi_class(0x103C8, 0x103CF, 'L'). % Lo   [8] OLD PERSIAN SIGN AURAMAZDAA..OLD PERSIAN SIGN BUUMISH
unicode_bidi_class(0x103D0, 0x103D0, 'L'). % Po       OLD PERSIAN WORD DIVIDER
unicode_bidi_class(0x103D1, 0x103D5, 'L'). % Nl   [5] OLD PERSIAN NUMBER ONE..OLD PERSIAN NUMBER HUNDRED
unicode_bidi_class(0x10400, 0x1044F, 'L'). % L&  [80] DESERET CAPITAL LETTER LONG I..DESERET SMALL LETTER EW
unicode_bidi_class(0x10450, 0x1049D, 'L'). % Lo  [78] SHAVIAN LETTER PEEP..OSMANYA LETTER OO
unicode_bidi_class(0x104A0, 0x104A9, 'L'). % Nd  [10] OSMANYA DIGIT ZERO..OSMANYA DIGIT NINE
unicode_bidi_class(0x11000, 0x11000, 'L'). % Mc       BRAHMI SIGN CANDRABINDU
unicode_bidi_class(0x11002, 0x11002, 'L'). % Mc       BRAHMI SIGN VISARGA
unicode_bidi_class(0x11003, 0x11037, 'L'). % Lo  [53] BRAHMI SIGN JIHVAMULIYA..BRAHMI LETTER OLD TAMIL NNNA
unicode_bidi_class(0x11047, 0x1104D, 'L'). % Po   [7] BRAHMI DANDA..BRAHMI PUNCTUATION LOTUS
unicode_bidi_class(0x11066, 0x1106F, 'L'). % Nd  [10] BRAHMI DIGIT ZERO..BRAHMI DIGIT NINE
unicode_bidi_class(0x11082, 0x11082, 'L'). % Mc       KAITHI SIGN VISARGA
unicode_bidi_class(0x11083, 0x110AF, 'L'). % Lo  [45] KAITHI LETTER A..KAITHI LETTER HA
unicode_bidi_class(0x110B0, 0x110B2, 'L'). % Mc   [3] KAITHI VOWEL SIGN AA..KAITHI VOWEL SIGN II
unicode_bidi_class(0x110B7, 0x110B8, 'L'). % Mc   [2] KAITHI VOWEL SIGN O..KAITHI VOWEL SIGN AU
unicode_bidi_class(0x110BB, 0x110BC, 'L'). % Po   [2] KAITHI ABBREVIATION SIGN..KAITHI ENUMERATION SIGN
unicode_bidi_class(0x110BD, 0x110BD, 'L'). % Cf       KAITHI NUMBER SIGN
unicode_bidi_class(0x110BE, 0x110C1, 'L'). % Po   [4] KAITHI SECTION MARK..KAITHI DOUBLE DANDA
unicode_bidi_class(0x110D0, 0x110E8, 'L'). % Lo  [25] SORA SOMPENG LETTER SAH..SORA SOMPENG LETTER MAE
unicode_bidi_class(0x110F0, 0x110F9, 'L'). % Nd  [10] SORA SOMPENG DIGIT ZERO..SORA SOMPENG DIGIT NINE
unicode_bidi_class(0x11103, 0x11126, 'L'). % Lo  [36] CHAKMA LETTER AA..CHAKMA LETTER HAA
unicode_bidi_class(0x1112C, 0x1112C, 'L'). % Mc       CHAKMA VOWEL SIGN E
unicode_bidi_class(0x11136, 0x1113F, 'L'). % Nd  [10] CHAKMA DIGIT ZERO..CHAKMA DIGIT NINE
unicode_bidi_class(0x11140, 0x11143, 'L'). % Po   [4] CHAKMA SECTION MARK..CHAKMA QUESTION MARK
unicode_bidi_class(0x11182, 0x11182, 'L'). % Mc       SHARADA SIGN VISARGA
unicode_bidi_class(0x11183, 0x111B2, 'L'). % Lo  [48] SHARADA LETTER A..SHARADA LETTER HA
unicode_bidi_class(0x111B3, 0x111B5, 'L'). % Mc   [3] SHARADA VOWEL SIGN AA..SHARADA VOWEL SIGN II
unicode_bidi_class(0x111BF, 0x111C0, 'L'). % Mc   [2] SHARADA VOWEL SIGN AU..SHARADA SIGN VIRAMA
unicode_bidi_class(0x111C1, 0x111C4, 'L'). % Lo   [4] SHARADA SIGN AVAGRAHA..SHARADA OM
unicode_bidi_class(0x111C5, 0x111C8, 'L'). % Po   [4] SHARADA DANDA..SHARADA SEPARATOR
unicode_bidi_class(0x111D0, 0x111D9, 'L'). % Nd  [10] SHARADA DIGIT ZERO..SHARADA DIGIT NINE
unicode_bidi_class(0x11680, 0x116AA, 'L'). % Lo  [43] TAKRI LETTER A..TAKRI LETTER RRA
unicode_bidi_class(0x116AC, 0x116AC, 'L'). % Mc       TAKRI SIGN VISARGA
unicode_bidi_class(0x116AE, 0x116AF, 'L'). % Mc   [2] TAKRI VOWEL SIGN I..TAKRI VOWEL SIGN II
unicode_bidi_class(0x116B6, 0x116B6, 'L'). % Mc       TAKRI SIGN VIRAMA
unicode_bidi_class(0x116C0, 0x116C9, 'L'). % Nd  [10] TAKRI DIGIT ZERO..TAKRI DIGIT NINE
unicode_bidi_class(0x12000, 0x1236E, 'L'). % Lo [879] CUNEIFORM SIGN A..CUNEIFORM SIGN ZUM
unicode_bidi_class(0x12400, 0x12462, 'L'). % Nl  [99] CUNEIFORM NUMERIC SIGN TWO ASH..CUNEIFORM NUMERIC SIGN OLD ASSYRIAN ONE QUARTER
unicode_bidi_class(0x12470, 0x12473, 'L'). % Po   [4] CUNEIFORM PUNCTUATION SIGN OLD ASSYRIAN WORD DIVIDER..CUNEIFORM PUNCTUATION SIGN DIAGONAL TRICOLON
unicode_bidi_class(0x13000, 0x1342E, 'L'). % Lo [1071] EGYPTIAN HIEROGLYPH A001..EGYPTIAN HIEROGLYPH AA032
unicode_bidi_class(0x16800, 0x16A38, 'L'). % Lo [569] BAMUM LETTER PHASE-A NGKUE MFON..BAMUM LETTER PHASE-F VUEQ
unicode_bidi_class(0x16F00, 0x16F44, 'L'). % Lo  [69] MIAO LETTER PA..MIAO LETTER HHA
unicode_bidi_class(0x16F50, 0x16F50, 'L'). % Lo       MIAO LETTER NASALIZATION
unicode_bidi_class(0x16F51, 0x16F7E, 'L'). % Mc  [46] MIAO SIGN ASPIRATION..MIAO VOWEL SIGN NG
unicode_bidi_class(0x16F93, 0x16F9F, 'L'). % Lm  [13] MIAO LETTER TONE-2..MIAO LETTER REFORMED TONE-8
unicode_bidi_class(0x1B000, 0x1B001, 'L'). % Lo   [2] KATAKANA LETTER ARCHAIC E..HIRAGANA LETTER ARCHAIC YE
unicode_bidi_class(0x1D000, 0x1D0F5, 'L'). % So [246] BYZANTINE MUSICAL SYMBOL PSILI..BYZANTINE MUSICAL SYMBOL GORGON NEO KATO
unicode_bidi_class(0x1D100, 0x1D126, 'L'). % So  [39] MUSICAL SYMBOL SINGLE BARLINE..MUSICAL SYMBOL DRUM CLEF-2
unicode_bidi_class(0x1D129, 0x1D164, 'L'). % So  [60] MUSICAL SYMBOL MULTIPLE MEASURE REST..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
unicode_bidi_class(0x1D165, 0x1D166, 'L'). % Mc   [2] MUSICAL SYMBOL COMBINING STEM..MUSICAL SYMBOL COMBINING SPRECHGESANG STEM
unicode_bidi_class(0x1D16A, 0x1D16C, 'L'). % So   [3] MUSICAL SYMBOL FINGERED TREMOLO-1..MUSICAL SYMBOL FINGERED TREMOLO-3
unicode_bidi_class(0x1D16D, 0x1D172, 'L'). % Mc   [6] MUSICAL SYMBOL COMBINING AUGMENTATION DOT..MUSICAL SYMBOL COMBINING FLAG-5
unicode_bidi_class(0x1D183, 0x1D184, 'L'). % So   [2] MUSICAL SYMBOL ARPEGGIATO UP..MUSICAL SYMBOL ARPEGGIATO DOWN
unicode_bidi_class(0x1D18C, 0x1D1A9, 'L'). % So  [30] MUSICAL SYMBOL RINFORZANDO..MUSICAL SYMBOL DEGREE SLASH
unicode_bidi_class(0x1D1AE, 0x1D1DD, 'L'). % So  [48] MUSICAL SYMBOL PEDAL MARK..MUSICAL SYMBOL PES SUBPUNCTIS
unicode_bidi_class(0x1D360, 0x1D371, 'L'). % No  [18] COUNTING ROD UNIT DIGIT ONE..COUNTING ROD TENS DIGIT NINE
unicode_bidi_class(0x1D400, 0x1D454, 'L'). % L&  [85] MATHEMATICAL BOLD CAPITAL A..MATHEMATICAL ITALIC SMALL G
unicode_bidi_class(0x1D456, 0x1D49C, 'L'). % L&  [71] MATHEMATICAL ITALIC SMALL I..MATHEMATICAL SCRIPT CAPITAL A
unicode_bidi_class(0x1D49E, 0x1D49F, 'L'). % L&   [2] MATHEMATICAL SCRIPT CAPITAL C..MATHEMATICAL SCRIPT CAPITAL D
unicode_bidi_class(0x1D4A2, 0x1D4A2, 'L'). % L&       MATHEMATICAL SCRIPT CAPITAL G
unicode_bidi_class(0x1D4A5, 0x1D4A6, 'L'). % L&   [2] MATHEMATICAL SCRIPT CAPITAL J..MATHEMATICAL SCRIPT CAPITAL K
unicode_bidi_class(0x1D4A9, 0x1D4AC, 'L'). % L&   [4] MATHEMATICAL SCRIPT CAPITAL N..MATHEMATICAL SCRIPT CAPITAL Q
unicode_bidi_class(0x1D4AE, 0x1D4B9, 'L'). % L&  [12] MATHEMATICAL SCRIPT CAPITAL S..MATHEMATICAL SCRIPT SMALL D
unicode_bidi_class(0x1D4BB, 0x1D4BB, 'L'). % L&       MATHEMATICAL SCRIPT SMALL F
unicode_bidi_class(0x1D4BD, 0x1D4C3, 'L'). % L&   [7] MATHEMATICAL SCRIPT SMALL H..MATHEMATICAL SCRIPT SMALL N
unicode_bidi_class(0x1D4C5, 0x1D505, 'L'). % L&  [65] MATHEMATICAL SCRIPT SMALL P..MATHEMATICAL FRAKTUR CAPITAL B
unicode_bidi_class(0x1D507, 0x1D50A, 'L'). % L&   [4] MATHEMATICAL FRAKTUR CAPITAL D..MATHEMATICAL FRAKTUR CAPITAL G
unicode_bidi_class(0x1D50D, 0x1D514, 'L'). % L&   [8] MATHEMATICAL FRAKTUR CAPITAL J..MATHEMATICAL FRAKTUR CAPITAL Q
unicode_bidi_class(0x1D516, 0x1D51C, 'L'). % L&   [7] MATHEMATICAL FRAKTUR CAPITAL S..MATHEMATICAL FRAKTUR CAPITAL Y
unicode_bidi_class(0x1D51E, 0x1D539, 'L'). % L&  [28] MATHEMATICAL FRAKTUR SMALL A..MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_bidi_class(0x1D53B, 0x1D53E, 'L'). % L&   [4] MATHEMATICAL DOUBLE-STRUCK CAPITAL D..MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_bidi_class(0x1D540, 0x1D544, 'L'). % L&   [5] MATHEMATICAL DOUBLE-STRUCK CAPITAL I..MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_bidi_class(0x1D546, 0x1D546, 'L'). % L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_bidi_class(0x1D54A, 0x1D550, 'L'). % L&   [7] MATHEMATICAL DOUBLE-STRUCK CAPITAL S..MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_bidi_class(0x1D552, 0x1D6A5, 'L'). % L& [340] MATHEMATICAL DOUBLE-STRUCK SMALL A..MATHEMATICAL ITALIC SMALL DOTLESS J
unicode_bidi_class(0x1D6A8, 0x1D6C0, 'L'). % L&  [25] MATHEMATICAL BOLD CAPITAL ALPHA..MATHEMATICAL BOLD CAPITAL OMEGA
unicode_bidi_class(0x1D6C1, 0x1D6C1, 'L'). % Sm       MATHEMATICAL BOLD NABLA
unicode_bidi_class(0x1D6C2, 0x1D6DA, 'L'). % L&  [25] MATHEMATICAL BOLD SMALL ALPHA..MATHEMATICAL BOLD SMALL OMEGA
unicode_bidi_class(0x1D6DC, 0x1D6FA, 'L'). % L&  [31] MATHEMATICAL BOLD EPSILON SYMBOL..MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_bidi_class(0x1D6FB, 0x1D6FB, 'L'). % Sm       MATHEMATICAL ITALIC NABLA
unicode_bidi_class(0x1D6FC, 0x1D714, 'L'). % L&  [25] MATHEMATICAL ITALIC SMALL ALPHA..MATHEMATICAL ITALIC SMALL OMEGA
unicode_bidi_class(0x1D716, 0x1D734, 'L'). % L&  [31] MATHEMATICAL ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_bidi_class(0x1D735, 0x1D735, 'L'). % Sm       MATHEMATICAL BOLD ITALIC NABLA
unicode_bidi_class(0x1D736, 0x1D74E, 'L'). % L&  [25] MATHEMATICAL BOLD ITALIC SMALL ALPHA..MATHEMATICAL BOLD ITALIC SMALL OMEGA
unicode_bidi_class(0x1D750, 0x1D76E, 'L'). % L&  [31] MATHEMATICAL BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_bidi_class(0x1D76F, 0x1D76F, 'L'). % Sm       MATHEMATICAL SANS-SERIF BOLD NABLA
unicode_bidi_class(0x1D770, 0x1D788, 'L'). % L&  [25] MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
unicode_bidi_class(0x1D78A, 0x1D7A8, 'L'). % L&  [31] MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_bidi_class(0x1D7A9, 0x1D7A9, 'L'). % Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA
unicode_bidi_class(0x1D7AA, 0x1D7C2, 'L'). % L&  [25] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
unicode_bidi_class(0x1D7C4, 0x1D7CB, 'L'). % L&   [8] MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD SMALL DIGAMMA
unicode_bidi_class(0x1F110, 0x1F12E, 'L'). % So  [31] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED WZ
unicode_bidi_class(0x1F130, 0x1F169, 'L'). % So  [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
unicode_bidi_class(0x1F170, 0x1F19A, 'L'). % So  [43] NEGATIVE SQUARED LATIN CAPITAL LETTER A..SQUARED VS
unicode_bidi_class(0x1F1E6, 0x1F202, 'L'). % So  [29] REGIONAL INDICATOR SYMBOL LETTER A..SQUARED KATAKANA SA
unicode_bidi_class(0x1F210, 0x1F23A, 'L'). % So  [43] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-55B6
unicode_bidi_class(0x1F240, 0x1F248, 'L'). % So   [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
unicode_bidi_class(0x1F250, 0x1F251, 'L'). % So   [2] CIRCLED IDEOGRAPH ADVANTAGE..CIRCLED IDEOGRAPH ACCEPT
unicode_bidi_class(0x20000, 0x2A6D6, 'L'). % Lo [42711] CJK UNIFIED IDEOGRAPH-20000..CJK UNIFIED IDEOGRAPH-2A6D6
unicode_bidi_class(0x2A700, 0x2B734, 'L'). % Lo [4149] CJK UNIFIED IDEOGRAPH-2A700..CJK UNIFIED IDEOGRAPH-2B734
unicode_bidi_class(0x2B740, 0x2B81D, 'L'). % Lo [222] CJK UNIFIED IDEOGRAPH-2B740..CJK UNIFIED IDEOGRAPH-2B81D
unicode_bidi_class(0x2F800, 0x2FA1D, 'L'). % Lo [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D
unicode_bidi_class(0xF0000, 0xFFFFD, 'L'). % Co [65534] <private-use-F0000>..<private-use-FFFFD>
unicode_bidi_class(0x100000, 0x10FFFD, 'L'). % Co [65534] <private-use-100000>..<private-use-10FFFD>

% The above property value applies to 858960 code points not listed here.
% Total code points: 1098531

% ================================================

% Bidi_Class=Right_To_Left

unicode_bidi_class(0x0590, 0x0590, 'R'). % Cn       <reserved-0590>
unicode_bidi_class(0x05BE, 0x05BE, 'R'). % Pd       HEBREW PUNCTUATION MAQAF
unicode_bidi_class(0x05C0, 0x05C0, 'R'). % Po       HEBREW PUNCTUATION PASEQ
unicode_bidi_class(0x05C3, 0x05C3, 'R'). % Po       HEBREW PUNCTUATION SOF PASUQ
unicode_bidi_class(0x05C6, 0x05C6, 'R'). % Po       HEBREW PUNCTUATION NUN HAFUKHA
unicode_bidi_class(0x05C8, 0x05CF, 'R'). % Cn   [8] <reserved-05C8>..<reserved-05CF>
unicode_bidi_class(0x05D0, 0x05EA, 'R'). % Lo  [27] HEBREW LETTER ALEF..HEBREW LETTER TAV
unicode_bidi_class(0x05EB, 0x05EF, 'R'). % Cn   [5] <reserved-05EB>..<reserved-05EF>
unicode_bidi_class(0x05F0, 0x05F2, 'R'). % Lo   [3] HEBREW LIGATURE YIDDISH DOUBLE VAV..HEBREW LIGATURE YIDDISH DOUBLE YOD
unicode_bidi_class(0x05F3, 0x05F4, 'R'). % Po   [2] HEBREW PUNCTUATION GERESH..HEBREW PUNCTUATION GERSHAYIM
unicode_bidi_class(0x05F5, 0x05FF, 'R'). % Cn  [11] <reserved-05F5>..<reserved-05FF>
unicode_bidi_class(0x07C0, 0x07C9, 'R'). % Nd  [10] NKO DIGIT ZERO..NKO DIGIT NINE
unicode_bidi_class(0x07CA, 0x07EA, 'R'). % Lo  [33] NKO LETTER A..NKO LETTER JONA RA
unicode_bidi_class(0x07F4, 0x07F5, 'R'). % Lm   [2] NKO HIGH TONE APOSTROPHE..NKO LOW TONE APOSTROPHE
unicode_bidi_class(0x07FA, 0x07FA, 'R'). % Lm       NKO LAJANYALAN
unicode_bidi_class(0x07FB, 0x07FF, 'R'). % Cn   [5] <reserved-07FB>..<reserved-07FF>
unicode_bidi_class(0x0800, 0x0815, 'R'). % Lo  [22] SAMARITAN LETTER ALAF..SAMARITAN LETTER TAAF
unicode_bidi_class(0x081A, 0x081A, 'R'). % Lm       SAMARITAN MODIFIER LETTER EPENTHETIC YUT
unicode_bidi_class(0x0824, 0x0824, 'R'). % Lm       SAMARITAN MODIFIER LETTER SHORT A
unicode_bidi_class(0x0828, 0x0828, 'R'). % Lm       SAMARITAN MODIFIER LETTER I
unicode_bidi_class(0x082E, 0x082F, 'R'). % Cn   [2] <reserved-082E>..<reserved-082F>
unicode_bidi_class(0x0830, 0x083E, 'R'). % Po  [15] SAMARITAN PUNCTUATION NEQUDAA..SAMARITAN PUNCTUATION ANNAAU
unicode_bidi_class(0x083F, 0x083F, 'R'). % Cn       <reserved-083F>
unicode_bidi_class(0x0840, 0x0858, 'R'). % Lo  [25] MANDAIC LETTER HALQA..MANDAIC LETTER AIN
unicode_bidi_class(0x085C, 0x085D, 'R'). % Cn   [2] <reserved-085C>..<reserved-085D>
unicode_bidi_class(0x085E, 0x085E, 'R'). % Po       MANDAIC PUNCTUATION
unicode_bidi_class(0x085F, 0x089F, 'R'). % Cn  [65] <reserved-085F>..<reserved-089F>
unicode_bidi_class(0x200F, 0x200F, 'R'). % Cf       RIGHT-TO-LEFT MARK
unicode_bidi_class(0xFB1D, 0xFB1D, 'R'). % Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_bidi_class(0xFB1F, 0xFB28, 'R'). % Lo  [10] HEBREW LIGATURE YIDDISH YOD YOD PATAH..HEBREW LETTER WIDE TAV
unicode_bidi_class(0xFB2A, 0xFB36, 'R'). % Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_bidi_class(0xFB37, 0xFB37, 'R'). % Cn       <reserved-FB37>
unicode_bidi_class(0xFB38, 0xFB3C, 'R'). % Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_bidi_class(0xFB3D, 0xFB3D, 'R'). % Cn       <reserved-FB3D>
unicode_bidi_class(0xFB3E, 0xFB3E, 'R'). % Lo       HEBREW LETTER MEM WITH DAGESH
unicode_bidi_class(0xFB3F, 0xFB3F, 'R'). % Cn       <reserved-FB3F>
unicode_bidi_class(0xFB40, 0xFB41, 'R'). % Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_bidi_class(0xFB42, 0xFB42, 'R'). % Cn       <reserved-FB42>
unicode_bidi_class(0xFB43, 0xFB44, 'R'). % Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_bidi_class(0xFB45, 0xFB45, 'R'). % Cn       <reserved-FB45>
unicode_bidi_class(0xFB46, 0xFB4F, 'R'). % Lo  [10] HEBREW LETTER TSADI WITH DAGESH..HEBREW LIGATURE ALEF LAMED
unicode_bidi_class(0x10800, 0x10805, 'R'). % Lo   [6] CYPRIOT SYLLABLE A..CYPRIOT SYLLABLE JA
unicode_bidi_class(0x10806, 0x10807, 'R'). % Cn   [2] <reserved-10806>..<reserved-10807>
unicode_bidi_class(0x10808, 0x10808, 'R'). % Lo       CYPRIOT SYLLABLE JO
unicode_bidi_class(0x10809, 0x10809, 'R'). % Cn       <reserved-10809>
unicode_bidi_class(0x1080A, 0x10835, 'R'). % Lo  [44] CYPRIOT SYLLABLE KA..CYPRIOT SYLLABLE WO
unicode_bidi_class(0x10836, 0x10836, 'R'). % Cn       <reserved-10836>
unicode_bidi_class(0x10837, 0x10838, 'R'). % Lo   [2] CYPRIOT SYLLABLE XA..CYPRIOT SYLLABLE XE
unicode_bidi_class(0x10839, 0x1083B, 'R'). % Cn   [3] <reserved-10839>..<reserved-1083B>
unicode_bidi_class(0x1083C, 0x1083C, 'R'). % Lo       CYPRIOT SYLLABLE ZA
unicode_bidi_class(0x1083D, 0x1083E, 'R'). % Cn   [2] <reserved-1083D>..<reserved-1083E>
unicode_bidi_class(0x1083F, 0x10855, 'R'). % Lo  [23] CYPRIOT SYLLABLE ZO..IMPERIAL ARAMAIC LETTER TAW
unicode_bidi_class(0x10856, 0x10856, 'R'). % Cn       <reserved-10856>
unicode_bidi_class(0x10857, 0x10857, 'R'). % Po       IMPERIAL ARAMAIC SECTION SIGN
unicode_bidi_class(0x10858, 0x1085F, 'R'). % No   [8] IMPERIAL ARAMAIC NUMBER ONE..IMPERIAL ARAMAIC NUMBER TEN THOUSAND
unicode_bidi_class(0x10860, 0x108FF, 'R'). % Cn [160] <reserved-10860>..<reserved-108FF>
unicode_bidi_class(0x10900, 0x10915, 'R'). % Lo  [22] PHOENICIAN LETTER ALF..PHOENICIAN LETTER TAU
unicode_bidi_class(0x10916, 0x1091B, 'R'). % No   [6] PHOENICIAN NUMBER ONE..PHOENICIAN NUMBER THREE
unicode_bidi_class(0x1091C, 0x1091E, 'R'). % Cn   [3] <reserved-1091C>..<reserved-1091E>
unicode_bidi_class(0x10920, 0x10939, 'R'). % Lo  [26] LYDIAN LETTER A..LYDIAN LETTER C
unicode_bidi_class(0x1093A, 0x1093E, 'R'). % Cn   [5] <reserved-1093A>..<reserved-1093E>
unicode_bidi_class(0x1093F, 0x1093F, 'R'). % Po       LYDIAN TRIANGULAR MARK
unicode_bidi_class(0x10940, 0x1097F, 'R'). % Cn  [64] <reserved-10940>..<reserved-1097F>
unicode_bidi_class(0x10980, 0x109B7, 'R'). % Lo  [56] MEROITIC HIEROGLYPHIC LETTER A..MEROITIC CURSIVE LETTER DA
unicode_bidi_class(0x109B8, 0x109BD, 'R'). % Cn   [6] <reserved-109B8>..<reserved-109BD>
unicode_bidi_class(0x109BE, 0x109BF, 'R'). % Lo   [2] MEROITIC CURSIVE LOGOGRAM RMT..MEROITIC CURSIVE LOGOGRAM IMN
unicode_bidi_class(0x109C0, 0x109FF, 'R'). % Cn  [64] <reserved-109C0>..<reserved-109FF>
unicode_bidi_class(0x10A00, 0x10A00, 'R'). % Lo       KHAROSHTHI LETTER A
unicode_bidi_class(0x10A04, 0x10A04, 'R'). % Cn       <reserved-10A04>
unicode_bidi_class(0x10A07, 0x10A0B, 'R'). % Cn   [5] <reserved-10A07>..<reserved-10A0B>
unicode_bidi_class(0x10A10, 0x10A13, 'R'). % Lo   [4] KHAROSHTHI LETTER KA..KHAROSHTHI LETTER GHA
unicode_bidi_class(0x10A14, 0x10A14, 'R'). % Cn       <reserved-10A14>
unicode_bidi_class(0x10A15, 0x10A17, 'R'). % Lo   [3] KHAROSHTHI LETTER CA..KHAROSHTHI LETTER JA
unicode_bidi_class(0x10A18, 0x10A18, 'R'). % Cn       <reserved-10A18>
unicode_bidi_class(0x10A19, 0x10A33, 'R'). % Lo  [27] KHAROSHTHI LETTER NYA..KHAROSHTHI LETTER TTTHA
unicode_bidi_class(0x10A34, 0x10A37, 'R'). % Cn   [4] <reserved-10A34>..<reserved-10A37>
unicode_bidi_class(0x10A3B, 0x10A3E, 'R'). % Cn   [4] <reserved-10A3B>..<reserved-10A3E>
unicode_bidi_class(0x10A40, 0x10A47, 'R'). % No   [8] KHAROSHTHI DIGIT ONE..KHAROSHTHI NUMBER ONE THOUSAND
unicode_bidi_class(0x10A48, 0x10A4F, 'R'). % Cn   [8] <reserved-10A48>..<reserved-10A4F>
unicode_bidi_class(0x10A50, 0x10A58, 'R'). % Po   [9] KHAROSHTHI PUNCTUATION DOT..KHAROSHTHI PUNCTUATION LINES
unicode_bidi_class(0x10A59, 0x10A5F, 'R'). % Cn   [7] <reserved-10A59>..<reserved-10A5F>
unicode_bidi_class(0x10A60, 0x10A7C, 'R'). % Lo  [29] OLD SOUTH ARABIAN LETTER HE..OLD SOUTH ARABIAN LETTER THETH
unicode_bidi_class(0x10A7D, 0x10A7E, 'R'). % No   [2] OLD SOUTH ARABIAN NUMBER ONE..OLD SOUTH ARABIAN NUMBER FIFTY
unicode_bidi_class(0x10A7F, 0x10A7F, 'R'). % Po       OLD SOUTH ARABIAN NUMERIC INDICATOR
unicode_bidi_class(0x10A80, 0x10AFF, 'R'). % Cn [128] <reserved-10A80>..<reserved-10AFF>
unicode_bidi_class(0x10B00, 0x10B35, 'R'). % Lo  [54] AVESTAN LETTER A..AVESTAN LETTER HE
unicode_bidi_class(0x10B36, 0x10B38, 'R'). % Cn   [3] <reserved-10B36>..<reserved-10B38>
unicode_bidi_class(0x10B40, 0x10B55, 'R'). % Lo  [22] INSCRIPTIONAL PARTHIAN LETTER ALEPH..INSCRIPTIONAL PARTHIAN LETTER TAW
unicode_bidi_class(0x10B56, 0x10B57, 'R'). % Cn   [2] <reserved-10B56>..<reserved-10B57>
unicode_bidi_class(0x10B58, 0x10B5F, 'R'). % No   [8] INSCRIPTIONAL PARTHIAN NUMBER ONE..INSCRIPTIONAL PARTHIAN NUMBER ONE THOUSAND
unicode_bidi_class(0x10B60, 0x10B72, 'R'). % Lo  [19] INSCRIPTIONAL PAHLAVI LETTER ALEPH..INSCRIPTIONAL PAHLAVI LETTER TAW
unicode_bidi_class(0x10B73, 0x10B77, 'R'). % Cn   [5] <reserved-10B73>..<reserved-10B77>
unicode_bidi_class(0x10B78, 0x10B7F, 'R'). % No   [8] INSCRIPTIONAL PAHLAVI NUMBER ONE..INSCRIPTIONAL PAHLAVI NUMBER ONE THOUSAND
unicode_bidi_class(0x10B80, 0x10BFF, 'R'). % Cn [128] <reserved-10B80>..<reserved-10BFF>
unicode_bidi_class(0x10C00, 0x10C48, 'R'). % Lo  [73] OLD TURKIC LETTER ORKHON A..OLD TURKIC LETTER ORKHON BASH
unicode_bidi_class(0x10C49, 0x10E5F, 'R'). % Cn [535] <reserved-10C49>..<reserved-10E5F>
unicode_bidi_class(0x10E7F, 0x10FFF, 'R'). % Cn [385] <reserved-10E7F>..<reserved-10FFF>
unicode_bidi_class(0x1E800, 0x1EDFF, 'R'). % Cn [1536] <reserved-1E800>..<reserved-1EDFF>
unicode_bidi_class(0x1EF00, 0x1EFFF, 'R'). % Cn [256] <reserved-1EF00>..<reserved-1EFFF>

% Total code points: 4086

% ================================================

% Bidi_Class=European_Number

unicode_bidi_class(0x0030, 0x0039, 'EN'). % Nd  [10] DIGIT ZERO..DIGIT NINE
unicode_bidi_class(0x00B2, 0x00B3, 'EN'). % No   [2] SUPERSCRIPT TWO..SUPERSCRIPT THREE
unicode_bidi_class(0x00B9, 0x00B9, 'EN'). % No       SUPERSCRIPT ONE
unicode_bidi_class(0x06F0, 0x06F9, 'EN'). % Nd  [10] EXTENDED ARABIC-INDIC DIGIT ZERO..EXTENDED ARABIC-INDIC DIGIT NINE
unicode_bidi_class(0x2070, 0x2070, 'EN'). % No       SUPERSCRIPT ZERO
unicode_bidi_class(0x2074, 0x2079, 'EN'). % No   [6] SUPERSCRIPT FOUR..SUPERSCRIPT NINE
unicode_bidi_class(0x2080, 0x2089, 'EN'). % No  [10] SUBSCRIPT ZERO..SUBSCRIPT NINE
unicode_bidi_class(0x2488, 0x249B, 'EN'). % No  [20] DIGIT ONE FULL STOP..NUMBER TWENTY FULL STOP
unicode_bidi_class(0xFF10, 0xFF19, 'EN'). % Nd  [10] FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
unicode_bidi_class(0x1D7CE, 0x1D7FF, 'EN'). % Nd  [50] MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
unicode_bidi_class(0x1F100, 0x1F10A, 'EN'). % No  [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA

% Total code points: 131

% ================================================

% Bidi_Class=European_Separator

unicode_bidi_class(0x002B, 0x002B, 'ES'). % Sm       PLUS SIGN
unicode_bidi_class(0x002D, 0x002D, 'ES'). % Pd       HYPHEN-MINUS
unicode_bidi_class(0x207A, 0x207B, 'ES'). % Sm   [2] SUPERSCRIPT PLUS SIGN..SUPERSCRIPT MINUS
unicode_bidi_class(0x208A, 0x208B, 'ES'). % Sm   [2] SUBSCRIPT PLUS SIGN..SUBSCRIPT MINUS
unicode_bidi_class(0x2212, 0x2212, 'ES'). % Sm       MINUS SIGN
unicode_bidi_class(0xFB29, 0xFB29, 'ES'). % Sm       HEBREW LETTER ALTERNATIVE PLUS SIGN
unicode_bidi_class(0xFE62, 0xFE62, 'ES'). % Sm       SMALL PLUS SIGN
unicode_bidi_class(0xFE63, 0xFE63, 'ES'). % Pd       SMALL HYPHEN-MINUS
unicode_bidi_class(0xFF0B, 0xFF0B, 'ES'). % Sm       FULLWIDTH PLUS SIGN
unicode_bidi_class(0xFF0D, 0xFF0D, 'ES'). % Pd       FULLWIDTH HYPHEN-MINUS

% Total code points: 12

% ================================================

% Bidi_Class=European_Terminator

unicode_bidi_class(0x0023, 0x0023, 'ET'). % Po       NUMBER SIGN
unicode_bidi_class(0x0024, 0x0024, 'ET'). % Sc       DOLLAR SIGN
unicode_bidi_class(0x0025, 0x0025, 'ET'). % Po       PERCENT SIGN
unicode_bidi_class(0x00A2, 0x00A5, 'ET'). % Sc   [4] CENT SIGN..YEN SIGN
unicode_bidi_class(0x00B0, 0x00B0, 'ET'). % So       DEGREE SIGN
unicode_bidi_class(0x00B1, 0x00B1, 'ET'). % Sm       PLUS-MINUS SIGN
unicode_bidi_class(0x058F, 0x058F, 'ET'). % Sc       ARMENIAN DRAM SIGN
unicode_bidi_class(0x0609, 0x060A, 'ET'). % Po   [2] ARABIC-INDIC PER MILLE SIGN..ARABIC-INDIC PER TEN THOUSAND SIGN
unicode_bidi_class(0x066A, 0x066A, 'ET'). % Po       ARABIC PERCENT SIGN
unicode_bidi_class(0x09F2, 0x09F3, 'ET'). % Sc   [2] BENGALI RUPEE MARK..BENGALI RUPEE SIGN
unicode_bidi_class(0x09FB, 0x09FB, 'ET'). % Sc       BENGALI GANDA MARK
unicode_bidi_class(0x0AF1, 0x0AF1, 'ET'). % Sc       GUJARATI RUPEE SIGN
unicode_bidi_class(0x0BF9, 0x0BF9, 'ET'). % Sc       TAMIL RUPEE SIGN
unicode_bidi_class(0x0E3F, 0x0E3F, 'ET'). % Sc       THAI CURRENCY SYMBOL BAHT
unicode_bidi_class(0x17DB, 0x17DB, 'ET'). % Sc       KHMER CURRENCY SYMBOL RIEL
unicode_bidi_class(0x2030, 0x2034, 'ET'). % Po   [5] PER MILLE SIGN..TRIPLE PRIME
unicode_bidi_class(0x20A0, 0x20BA, 'ET'). % Sc  [27] EURO-CURRENCY SIGN..TURKISH LIRA SIGN
unicode_bidi_class(0x212E, 0x212E, 'ET'). % So       ESTIMATED SYMBOL
unicode_bidi_class(0x2213, 0x2213, 'ET'). % Sm       MINUS-OR-PLUS SIGN
unicode_bidi_class(0xA838, 0xA838, 'ET'). % Sc       NORTH INDIC RUPEE MARK
unicode_bidi_class(0xA839, 0xA839, 'ET'). % So       NORTH INDIC QUANTITY MARK
unicode_bidi_class(0xFE5F, 0xFE5F, 'ET'). % Po       SMALL NUMBER SIGN
unicode_bidi_class(0xFE69, 0xFE69, 'ET'). % Sc       SMALL DOLLAR SIGN
unicode_bidi_class(0xFE6A, 0xFE6A, 'ET'). % Po       SMALL PERCENT SIGN
unicode_bidi_class(0xFF03, 0xFF03, 'ET'). % Po       FULLWIDTH NUMBER SIGN
unicode_bidi_class(0xFF04, 0xFF04, 'ET'). % Sc       FULLWIDTH DOLLAR SIGN
unicode_bidi_class(0xFF05, 0xFF05, 'ET'). % Po       FULLWIDTH PERCENT SIGN
unicode_bidi_class(0xFFE0, 0xFFE1, 'ET'). % Sc   [2] FULLWIDTH CENT SIGN..FULLWIDTH POUND SIGN
unicode_bidi_class(0xFFE5, 0xFFE6, 'ET'). % Sc   [2] FULLWIDTH YEN SIGN..FULLWIDTH WON SIGN

% Total code points: 66

% ================================================

% Bidi_Class=Arabic_Number

unicode_bidi_class(0x0600, 0x0604, 'AN'). % Cf   [5] ARABIC NUMBER SIGN..ARABIC SIGN SAMVAT
unicode_bidi_class(0x0660, 0x0669, 'AN'). % Nd  [10] ARABIC-INDIC DIGIT ZERO..ARABIC-INDIC DIGIT NINE
unicode_bidi_class(0x066B, 0x066C, 'AN'). % Po   [2] ARABIC DECIMAL SEPARATOR..ARABIC THOUSANDS SEPARATOR
unicode_bidi_class(0x06DD, 0x06DD, 'AN'). % Cf       ARABIC END OF AYAH
unicode_bidi_class(0x10E60, 0x10E7E, 'AN'). % No  [31] RUMI DIGIT ONE..RUMI FRACTION TWO THIRDS

% Total code points: 49

% ================================================

% Bidi_Class=Common_Separator

unicode_bidi_class(0x002C, 0x002C, 'CS'). % Po       COMMA
unicode_bidi_class(0x002E, 0x002F, 'CS'). % Po   [2] FULL STOP..SOLIDUS
unicode_bidi_class(0x003A, 0x003A, 'CS'). % Po       COLON
unicode_bidi_class(0x00A0, 0x00A0, 'CS'). % Zs       NO-BREAK SPACE
unicode_bidi_class(0x060C, 0x060C, 'CS'). % Po       ARABIC COMMA
unicode_bidi_class(0x202F, 0x202F, 'CS'). % Zs       NARROW NO-BREAK SPACE
unicode_bidi_class(0x2044, 0x2044, 'CS'). % Sm       FRACTION SLASH
unicode_bidi_class(0xFE50, 0xFE50, 'CS'). % Po       SMALL COMMA
unicode_bidi_class(0xFE52, 0xFE52, 'CS'). % Po       SMALL FULL STOP
unicode_bidi_class(0xFE55, 0xFE55, 'CS'). % Po       SMALL COLON
unicode_bidi_class(0xFF0C, 0xFF0C, 'CS'). % Po       FULLWIDTH COMMA
unicode_bidi_class(0xFF0E, 0xFF0F, 'CS'). % Po   [2] FULLWIDTH FULL STOP..FULLWIDTH SOLIDUS
unicode_bidi_class(0xFF1A, 0xFF1A, 'CS'). % Po       FULLWIDTH COLON

% Total code points: 15

% ================================================

% Bidi_Class=Paragraph_Separator

unicode_bidi_class(0x000A, 0x000A, 'B'). % Cc       <control-000A>
unicode_bidi_class(0x000D, 0x000D, 'B'). % Cc       <control-000D>
unicode_bidi_class(0x001C, 0x001E, 'B'). % Cc   [3] <control-001C>..<control-001E>
unicode_bidi_class(0x0085, 0x0085, 'B'). % Cc       <control-0085>
unicode_bidi_class(0x2029, 0x2029, 'B'). % Zp       PARAGRAPH SEPARATOR

% Total code points: 7

% ================================================

% Bidi_Class=Segment_Separator

unicode_bidi_class(0x0009, 0x0009, 'S'). % Cc       <control-0009>
unicode_bidi_class(0x000B, 0x000B, 'S'). % Cc       <control-000B>
unicode_bidi_class(0x001F, 0x001F, 'S'). % Cc       <control-001F>

% Total code points: 3

% ================================================

% Bidi_Class=White_Space

unicode_bidi_class(0x000C, 0x000C, 'WS'). % Cc       <control-000C>
unicode_bidi_class(0x0020, 0x0020, 'WS'). % Zs       SPACE
unicode_bidi_class(0x1680, 0x1680, 'WS'). % Zs       OGHAM SPACE MARK
unicode_bidi_class(0x180E, 0x180E, 'WS'). % Zs       MONGOLIAN VOWEL SEPARATOR
unicode_bidi_class(0x2000, 0x200A, 'WS'). % Zs  [11] EN QUAD..HAIR SPACE
unicode_bidi_class(0x2028, 0x2028, 'WS'). % Zl       LINE SEPARATOR
unicode_bidi_class(0x205F, 0x205F, 'WS'). % Zs       MEDIUM MATHEMATICAL SPACE
unicode_bidi_class(0x3000, 0x3000, 'WS'). % Zs       IDEOGRAPHIC SPACE

% Total code points: 18

% ================================================

% Bidi_Class=Other_Neutral

unicode_bidi_class(0x0021, 0x0022, 'ON'). % Po   [2] EXCLAMATION MARK..QUOTATION MARK
unicode_bidi_class(0x0026, 0x0027, 'ON'). % Po   [2] AMPERSAND..APOSTROPHE
unicode_bidi_class(0x0028, 0x0028, 'ON'). % Ps       LEFT PARENTHESIS
unicode_bidi_class(0x0029, 0x0029, 'ON'). % Pe       RIGHT PARENTHESIS
unicode_bidi_class(0x002A, 0x002A, 'ON'). % Po       ASTERISK
unicode_bidi_class(0x003B, 0x003B, 'ON'). % Po       SEMICOLON
unicode_bidi_class(0x003C, 0x003E, 'ON'). % Sm   [3] LESS-THAN SIGN..GREATER-THAN SIGN
unicode_bidi_class(0x003F, 0x0040, 'ON'). % Po   [2] QUESTION MARK..COMMERCIAL AT
unicode_bidi_class(0x005B, 0x005B, 'ON'). % Ps       LEFT SQUARE BRACKET
unicode_bidi_class(0x005C, 0x005C, 'ON'). % Po       REVERSE SOLIDUS
unicode_bidi_class(0x005D, 0x005D, 'ON'). % Pe       RIGHT SQUARE BRACKET
unicode_bidi_class(0x005E, 0x005E, 'ON'). % Sk       CIRCUMFLEX ACCENT
unicode_bidi_class(0x005F, 0x005F, 'ON'). % Pc       LOW LINE
unicode_bidi_class(0x0060, 0x0060, 'ON'). % Sk       GRAVE ACCENT
unicode_bidi_class(0x007B, 0x007B, 'ON'). % Ps       LEFT CURLY BRACKET
unicode_bidi_class(0x007C, 0x007C, 'ON'). % Sm       VERTICAL LINE
unicode_bidi_class(0x007D, 0x007D, 'ON'). % Pe       RIGHT CURLY BRACKET
unicode_bidi_class(0x007E, 0x007E, 'ON'). % Sm       TILDE
unicode_bidi_class(0x00A1, 0x00A1, 'ON'). % Po       INVERTED EXCLAMATION MARK
unicode_bidi_class(0x00A6, 0x00A6, 'ON'). % So       BROKEN BAR
unicode_bidi_class(0x00A7, 0x00A7, 'ON'). % Po       SECTION SIGN
unicode_bidi_class(0x00A8, 0x00A8, 'ON'). % Sk       DIAERESIS
unicode_bidi_class(0x00A9, 0x00A9, 'ON'). % So       COPYRIGHT SIGN
unicode_bidi_class(0x00AB, 0x00AB, 'ON'). % Pi       LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_bidi_class(0x00AC, 0x00AC, 'ON'). % Sm       NOT SIGN
unicode_bidi_class(0x00AE, 0x00AE, 'ON'). % So       REGISTERED SIGN
unicode_bidi_class(0x00AF, 0x00AF, 'ON'). % Sk       MACRON
unicode_bidi_class(0x00B4, 0x00B4, 'ON'). % Sk       ACUTE ACCENT
unicode_bidi_class(0x00B6, 0x00B7, 'ON'). % Po   [2] PILCROW SIGN..MIDDLE DOT
unicode_bidi_class(0x00B8, 0x00B8, 'ON'). % Sk       CEDILLA
unicode_bidi_class(0x00BB, 0x00BB, 'ON'). % Pf       RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_bidi_class(0x00BC, 0x00BE, 'ON'). % No   [3] VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS
unicode_bidi_class(0x00BF, 0x00BF, 'ON'). % Po       INVERTED QUESTION MARK
unicode_bidi_class(0x00D7, 0x00D7, 'ON'). % Sm       MULTIPLICATION SIGN
unicode_bidi_class(0x00F7, 0x00F7, 'ON'). % Sm       DIVISION SIGN
unicode_bidi_class(0x02B9, 0x02BA, 'ON'). % Lm   [2] MODIFIER LETTER PRIME..MODIFIER LETTER DOUBLE PRIME
unicode_bidi_class(0x02C2, 0x02C5, 'ON'). % Sk   [4] MODIFIER LETTER LEFT ARROWHEAD..MODIFIER LETTER DOWN ARROWHEAD
unicode_bidi_class(0x02C6, 0x02CF, 'ON'). % Lm  [10] MODIFIER LETTER CIRCUMFLEX ACCENT..MODIFIER LETTER LOW ACUTE ACCENT
unicode_bidi_class(0x02D2, 0x02DF, 'ON'). % Sk  [14] MODIFIER LETTER CENTRED RIGHT HALF RING..MODIFIER LETTER CROSS ACCENT
unicode_bidi_class(0x02E5, 0x02EB, 'ON'). % Sk   [7] MODIFIER LETTER EXTRA-HIGH TONE BAR..MODIFIER LETTER YANG DEPARTING TONE MARK
unicode_bidi_class(0x02EC, 0x02EC, 'ON'). % Lm       MODIFIER LETTER VOICING
unicode_bidi_class(0x02ED, 0x02ED, 'ON'). % Sk       MODIFIER LETTER UNASPIRATED
unicode_bidi_class(0x02EF, 0x02FF, 'ON'). % Sk  [17] MODIFIER LETTER LOW DOWN ARROWHEAD..MODIFIER LETTER LOW LEFT ARROW
unicode_bidi_class(0x0374, 0x0374, 'ON'). % Lm       GREEK NUMERAL SIGN
unicode_bidi_class(0x0375, 0x0375, 'ON'). % Sk       GREEK LOWER NUMERAL SIGN
unicode_bidi_class(0x037E, 0x037E, 'ON'). % Po       GREEK QUESTION MARK
unicode_bidi_class(0x0384, 0x0385, 'ON'). % Sk   [2] GREEK TONOS..GREEK DIALYTIKA TONOS
unicode_bidi_class(0x0387, 0x0387, 'ON'). % Po       GREEK ANO TELEIA
unicode_bidi_class(0x03F6, 0x03F6, 'ON'). % Sm       GREEK REVERSED LUNATE EPSILON SYMBOL
unicode_bidi_class(0x058A, 0x058A, 'ON'). % Pd       ARMENIAN HYPHEN
unicode_bidi_class(0x0606, 0x0607, 'ON'). % Sm   [2] ARABIC-INDIC CUBE ROOT..ARABIC-INDIC FOURTH ROOT
unicode_bidi_class(0x060E, 0x060F, 'ON'). % So   [2] ARABIC POETIC VERSE SIGN..ARABIC SIGN MISRA
unicode_bidi_class(0x06DE, 0x06DE, 'ON'). % So       ARABIC START OF RUB EL HIZB
unicode_bidi_class(0x06E9, 0x06E9, 'ON'). % So       ARABIC PLACE OF SAJDAH
unicode_bidi_class(0x07F6, 0x07F6, 'ON'). % So       NKO SYMBOL OO DENNEN
unicode_bidi_class(0x07F7, 0x07F9, 'ON'). % Po   [3] NKO SYMBOL GBAKURUNEN..NKO EXCLAMATION MARK
unicode_bidi_class(0x0BF3, 0x0BF8, 'ON'). % So   [6] TAMIL DAY SIGN..TAMIL AS ABOVE SIGN
unicode_bidi_class(0x0BFA, 0x0BFA, 'ON'). % So       TAMIL NUMBER SIGN
unicode_bidi_class(0x0C78, 0x0C7E, 'ON'). % No   [7] TELUGU FRACTION DIGIT ZERO FOR ODD POWERS OF FOUR..TELUGU FRACTION DIGIT THREE FOR EVEN POWERS OF FOUR
unicode_bidi_class(0x0F3A, 0x0F3A, 'ON'). % Ps       TIBETAN MARK GUG RTAGS GYON
unicode_bidi_class(0x0F3B, 0x0F3B, 'ON'). % Pe       TIBETAN MARK GUG RTAGS GYAS
unicode_bidi_class(0x0F3C, 0x0F3C, 'ON'). % Ps       TIBETAN MARK ANG KHANG GYON
unicode_bidi_class(0x0F3D, 0x0F3D, 'ON'). % Pe       TIBETAN MARK ANG KHANG GYAS
unicode_bidi_class(0x1390, 0x1399, 'ON'). % So  [10] ETHIOPIC TONAL MARK YIZET..ETHIOPIC TONAL MARK KURT
unicode_bidi_class(0x1400, 0x1400, 'ON'). % Pd       CANADIAN SYLLABICS HYPHEN
unicode_bidi_class(0x169B, 0x169B, 'ON'). % Ps       OGHAM FEATHER MARK
unicode_bidi_class(0x169C, 0x169C, 'ON'). % Pe       OGHAM REVERSED FEATHER MARK
unicode_bidi_class(0x17F0, 0x17F9, 'ON'). % No  [10] KHMER SYMBOL LEK ATTAK SON..KHMER SYMBOL LEK ATTAK PRAM-BUON
unicode_bidi_class(0x1800, 0x1805, 'ON'). % Po   [6] MONGOLIAN BIRGA..MONGOLIAN FOUR DOTS
unicode_bidi_class(0x1806, 0x1806, 'ON'). % Pd       MONGOLIAN TODO SOFT HYPHEN
unicode_bidi_class(0x1807, 0x180A, 'ON'). % Po   [4] MONGOLIAN SIBE SYLLABLE BOUNDARY MARKER..MONGOLIAN NIRUGU
unicode_bidi_class(0x1940, 0x1940, 'ON'). % So       LIMBU SIGN LOO
unicode_bidi_class(0x1944, 0x1945, 'ON'). % Po   [2] LIMBU EXCLAMATION MARK..LIMBU QUESTION MARK
unicode_bidi_class(0x19DE, 0x19FF, 'ON'). % So  [34] NEW TAI LUE SIGN LAE..KHMER SYMBOL DAP-PRAM ROC
unicode_bidi_class(0x1FBD, 0x1FBD, 'ON'). % Sk       GREEK KORONIS
unicode_bidi_class(0x1FBF, 0x1FC1, 'ON'). % Sk   [3] GREEK PSILI..GREEK DIALYTIKA AND PERISPOMENI
unicode_bidi_class(0x1FCD, 0x1FCF, 'ON'). % Sk   [3] GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI
unicode_bidi_class(0x1FDD, 0x1FDF, 'ON'). % Sk   [3] GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI
unicode_bidi_class(0x1FED, 0x1FEF, 'ON'). % Sk   [3] GREEK DIALYTIKA AND VARIA..GREEK VARIA
unicode_bidi_class(0x1FFD, 0x1FFE, 'ON'). % Sk   [2] GREEK OXIA..GREEK DASIA
unicode_bidi_class(0x2010, 0x2015, 'ON'). % Pd   [6] HYPHEN..HORIZONTAL BAR
unicode_bidi_class(0x2016, 0x2017, 'ON'). % Po   [2] DOUBLE VERTICAL LINE..DOUBLE LOW LINE
unicode_bidi_class(0x2018, 0x2018, 'ON'). % Pi       LEFT SINGLE QUOTATION MARK
unicode_bidi_class(0x2019, 0x2019, 'ON'). % Pf       RIGHT SINGLE QUOTATION MARK
unicode_bidi_class(0x201A, 0x201A, 'ON'). % Ps       SINGLE LOW-9 QUOTATION MARK
unicode_bidi_class(0x201B, 0x201C, 'ON'). % Pi   [2] SINGLE HIGH-REVERSED-9 QUOTATION MARK..LEFT DOUBLE QUOTATION MARK
unicode_bidi_class(0x201D, 0x201D, 'ON'). % Pf       RIGHT DOUBLE QUOTATION MARK
unicode_bidi_class(0x201E, 0x201E, 'ON'). % Ps       DOUBLE LOW-9 QUOTATION MARK
unicode_bidi_class(0x201F, 0x201F, 'ON'). % Pi       DOUBLE HIGH-REVERSED-9 QUOTATION MARK
unicode_bidi_class(0x2020, 0x2027, 'ON'). % Po   [8] DAGGER..HYPHENATION POINT
unicode_bidi_class(0x2035, 0x2038, 'ON'). % Po   [4] REVERSED PRIME..CARET
unicode_bidi_class(0x2039, 0x2039, 'ON'). % Pi       SINGLE LEFT-POINTING ANGLE QUOTATION MARK
unicode_bidi_class(0x203A, 0x203A, 'ON'). % Pf       SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
unicode_bidi_class(0x203B, 0x203E, 'ON'). % Po   [4] REFERENCE MARK..OVERLINE
unicode_bidi_class(0x203F, 0x2040, 'ON'). % Pc   [2] UNDERTIE..CHARACTER TIE
unicode_bidi_class(0x2041, 0x2043, 'ON'). % Po   [3] CARET INSERTION POINT..HYPHEN BULLET
unicode_bidi_class(0x2045, 0x2045, 'ON'). % Ps       LEFT SQUARE BRACKET WITH QUILL
unicode_bidi_class(0x2046, 0x2046, 'ON'). % Pe       RIGHT SQUARE BRACKET WITH QUILL
unicode_bidi_class(0x2047, 0x2051, 'ON'). % Po  [11] DOUBLE QUESTION MARK..TWO ASTERISKS ALIGNED VERTICALLY
unicode_bidi_class(0x2052, 0x2052, 'ON'). % Sm       COMMERCIAL MINUS SIGN
unicode_bidi_class(0x2053, 0x2053, 'ON'). % Po       SWUNG DASH
unicode_bidi_class(0x2054, 0x2054, 'ON'). % Pc       INVERTED UNDERTIE
unicode_bidi_class(0x2055, 0x205E, 'ON'). % Po  [10] FLOWER PUNCTUATION MARK..VERTICAL FOUR DOTS
unicode_bidi_class(0x207C, 0x207C, 'ON'). % Sm       SUPERSCRIPT EQUALS SIGN
unicode_bidi_class(0x207D, 0x207D, 'ON'). % Ps       SUPERSCRIPT LEFT PARENTHESIS
unicode_bidi_class(0x207E, 0x207E, 'ON'). % Pe       SUPERSCRIPT RIGHT PARENTHESIS
unicode_bidi_class(0x208C, 0x208C, 'ON'). % Sm       SUBSCRIPT EQUALS SIGN
unicode_bidi_class(0x208D, 0x208D, 'ON'). % Ps       SUBSCRIPT LEFT PARENTHESIS
unicode_bidi_class(0x208E, 0x208E, 'ON'). % Pe       SUBSCRIPT RIGHT PARENTHESIS
unicode_bidi_class(0x2100, 0x2101, 'ON'). % So   [2] ACCOUNT OF..ADDRESSED TO THE SUBJECT
unicode_bidi_class(0x2103, 0x2106, 'ON'). % So   [4] DEGREE CELSIUS..CADA UNA
unicode_bidi_class(0x2108, 0x2109, 'ON'). % So   [2] SCRUPLE..DEGREE FAHRENHEIT
unicode_bidi_class(0x2114, 0x2114, 'ON'). % So       L B BAR SYMBOL
unicode_bidi_class(0x2116, 0x2117, 'ON'). % So   [2] NUMERO SIGN..SOUND RECORDING COPYRIGHT
unicode_bidi_class(0x2118, 0x2118, 'ON'). % Sm       SCRIPT CAPITAL P
unicode_bidi_class(0x211E, 0x2123, 'ON'). % So   [6] PRESCRIPTION TAKE..VERSICLE
unicode_bidi_class(0x2125, 0x2125, 'ON'). % So       OUNCE SIGN
unicode_bidi_class(0x2127, 0x2127, 'ON'). % So       INVERTED OHM SIGN
unicode_bidi_class(0x2129, 0x2129, 'ON'). % So       TURNED GREEK SMALL LETTER IOTA
unicode_bidi_class(0x213A, 0x213B, 'ON'). % So   [2] ROTATED CAPITAL Q..FACSIMILE SIGN
unicode_bidi_class(0x2140, 0x2144, 'ON'). % Sm   [5] DOUBLE-STRUCK N-ARY SUMMATION..TURNED SANS-SERIF CAPITAL Y
unicode_bidi_class(0x214A, 0x214A, 'ON'). % So       PROPERTY LINE
unicode_bidi_class(0x214B, 0x214B, 'ON'). % Sm       TURNED AMPERSAND
unicode_bidi_class(0x214C, 0x214D, 'ON'). % So   [2] PER SIGN..AKTIESELSKAB
unicode_bidi_class(0x2150, 0x215F, 'ON'). % No  [16] VULGAR FRACTION ONE SEVENTH..FRACTION NUMERATOR ONE
unicode_bidi_class(0x2189, 0x2189, 'ON'). % No       VULGAR FRACTION ZERO THIRDS
unicode_bidi_class(0x2190, 0x2194, 'ON'). % Sm   [5] LEFTWARDS ARROW..LEFT RIGHT ARROW
unicode_bidi_class(0x2195, 0x2199, 'ON'). % So   [5] UP DOWN ARROW..SOUTH WEST ARROW
unicode_bidi_class(0x219A, 0x219B, 'ON'). % Sm   [2] LEFTWARDS ARROW WITH STROKE..RIGHTWARDS ARROW WITH STROKE
unicode_bidi_class(0x219C, 0x219F, 'ON'). % So   [4] LEFTWARDS WAVE ARROW..UPWARDS TWO HEADED ARROW
unicode_bidi_class(0x21A0, 0x21A0, 'ON'). % Sm       RIGHTWARDS TWO HEADED ARROW
unicode_bidi_class(0x21A1, 0x21A2, 'ON'). % So   [2] DOWNWARDS TWO HEADED ARROW..LEFTWARDS ARROW WITH TAIL
unicode_bidi_class(0x21A3, 0x21A3, 'ON'). % Sm       RIGHTWARDS ARROW WITH TAIL
unicode_bidi_class(0x21A4, 0x21A5, 'ON'). % So   [2] LEFTWARDS ARROW FROM BAR..UPWARDS ARROW FROM BAR
unicode_bidi_class(0x21A6, 0x21A6, 'ON'). % Sm       RIGHTWARDS ARROW FROM BAR
unicode_bidi_class(0x21A7, 0x21AD, 'ON'). % So   [7] DOWNWARDS ARROW FROM BAR..LEFT RIGHT WAVE ARROW
unicode_bidi_class(0x21AE, 0x21AE, 'ON'). % Sm       LEFT RIGHT ARROW WITH STROKE
unicode_bidi_class(0x21AF, 0x21CD, 'ON'). % So  [31] DOWNWARDS ZIGZAG ARROW..LEFTWARDS DOUBLE ARROW WITH STROKE
unicode_bidi_class(0x21CE, 0x21CF, 'ON'). % Sm   [2] LEFT RIGHT DOUBLE ARROW WITH STROKE..RIGHTWARDS DOUBLE ARROW WITH STROKE
unicode_bidi_class(0x21D0, 0x21D1, 'ON'). % So   [2] LEFTWARDS DOUBLE ARROW..UPWARDS DOUBLE ARROW
unicode_bidi_class(0x21D2, 0x21D2, 'ON'). % Sm       RIGHTWARDS DOUBLE ARROW
unicode_bidi_class(0x21D3, 0x21D3, 'ON'). % So       DOWNWARDS DOUBLE ARROW
unicode_bidi_class(0x21D4, 0x21D4, 'ON'). % Sm       LEFT RIGHT DOUBLE ARROW
unicode_bidi_class(0x21D5, 0x21F3, 'ON'). % So  [31] UP DOWN DOUBLE ARROW..UP DOWN WHITE ARROW
unicode_bidi_class(0x21F4, 0x2211, 'ON'). % Sm  [30] RIGHT ARROW WITH SMALL CIRCLE..N-ARY SUMMATION
unicode_bidi_class(0x2214, 0x22FF, 'ON'). % Sm [236] DOT PLUS..Z NOTATION BAG MEMBERSHIP
unicode_bidi_class(0x2300, 0x2307, 'ON'). % So   [8] DIAMETER SIGN..WAVY LINE
unicode_bidi_class(0x2308, 0x230B, 'ON'). % Sm   [4] LEFT CEILING..RIGHT FLOOR
unicode_bidi_class(0x230C, 0x231F, 'ON'). % So  [20] BOTTOM RIGHT CROP..BOTTOM RIGHT CORNER
unicode_bidi_class(0x2320, 0x2321, 'ON'). % Sm   [2] TOP HALF INTEGRAL..BOTTOM HALF INTEGRAL
unicode_bidi_class(0x2322, 0x2328, 'ON'). % So   [7] FROWN..KEYBOARD
unicode_bidi_class(0x2329, 0x2329, 'ON'). % Ps       LEFT-POINTING ANGLE BRACKET
unicode_bidi_class(0x232A, 0x232A, 'ON'). % Pe       RIGHT-POINTING ANGLE BRACKET
unicode_bidi_class(0x232B, 0x2335, 'ON'). % So  [11] ERASE TO THE LEFT..COUNTERSINK
unicode_bidi_class(0x237B, 0x237B, 'ON'). % So       NOT CHECK MARK
unicode_bidi_class(0x237C, 0x237C, 'ON'). % Sm       RIGHT ANGLE WITH DOWNWARDS ZIGZAG ARROW
unicode_bidi_class(0x237D, 0x2394, 'ON'). % So  [24] SHOULDERED OPEN BOX..SOFTWARE-FUNCTION SYMBOL
unicode_bidi_class(0x2396, 0x239A, 'ON'). % So   [5] DECIMAL SEPARATOR KEY SYMBOL..CLEAR SCREEN SYMBOL
unicode_bidi_class(0x239B, 0x23B3, 'ON'). % Sm  [25] LEFT PARENTHESIS UPPER HOOK..SUMMATION BOTTOM
unicode_bidi_class(0x23B4, 0x23DB, 'ON'). % So  [40] TOP SQUARE BRACKET..FUSE
unicode_bidi_class(0x23DC, 0x23E1, 'ON'). % Sm   [6] TOP PARENTHESIS..BOTTOM TORTOISE SHELL BRACKET
unicode_bidi_class(0x23E2, 0x23F3, 'ON'). % So  [18] WHITE TRAPEZIUM..HOURGLASS WITH FLOWING SAND
unicode_bidi_class(0x2400, 0x2426, 'ON'). % So  [39] SYMBOL FOR NULL..SYMBOL FOR SUBSTITUTE FORM TWO
unicode_bidi_class(0x2440, 0x244A, 'ON'). % So  [11] OCR HOOK..OCR DOUBLE BACKSLASH
unicode_bidi_class(0x2460, 0x2487, 'ON'). % No  [40] CIRCLED DIGIT ONE..PARENTHESIZED NUMBER TWENTY
unicode_bidi_class(0x24EA, 0x24FF, 'ON'). % No  [22] CIRCLED DIGIT ZERO..NEGATIVE CIRCLED DIGIT ZERO
unicode_bidi_class(0x2500, 0x25B6, 'ON'). % So [183] BOX DRAWINGS LIGHT HORIZONTAL..BLACK RIGHT-POINTING TRIANGLE
unicode_bidi_class(0x25B7, 0x25B7, 'ON'). % Sm       WHITE RIGHT-POINTING TRIANGLE
unicode_bidi_class(0x25B8, 0x25C0, 'ON'). % So   [9] BLACK RIGHT-POINTING SMALL TRIANGLE..BLACK LEFT-POINTING TRIANGLE
unicode_bidi_class(0x25C1, 0x25C1, 'ON'). % Sm       WHITE LEFT-POINTING TRIANGLE
unicode_bidi_class(0x25C2, 0x25F7, 'ON'). % So  [54] BLACK LEFT-POINTING SMALL TRIANGLE..WHITE CIRCLE WITH UPPER RIGHT QUADRANT
unicode_bidi_class(0x25F8, 0x25FF, 'ON'). % Sm   [8] UPPER LEFT TRIANGLE..LOWER RIGHT TRIANGLE
unicode_bidi_class(0x2600, 0x266E, 'ON'). % So [111] BLACK SUN WITH RAYS..MUSIC NATURAL SIGN
unicode_bidi_class(0x266F, 0x266F, 'ON'). % Sm       MUSIC SHARP SIGN
unicode_bidi_class(0x2670, 0x26AB, 'ON'). % So  [60] WEST SYRIAC CROSS..MEDIUM BLACK CIRCLE
unicode_bidi_class(0x26AD, 0x26FF, 'ON'). % So  [83] MARRIAGE SYMBOL..WHITE FLAG WITH HORIZONTAL MIDDLE BLACK STRIPE
unicode_bidi_class(0x2701, 0x2767, 'ON'). % So [103] UPPER BLADE SCISSORS..ROTATED FLORAL HEART BULLET
unicode_bidi_class(0x2768, 0x2768, 'ON'). % Ps       MEDIUM LEFT PARENTHESIS ORNAMENT
unicode_bidi_class(0x2769, 0x2769, 'ON'). % Pe       MEDIUM RIGHT PARENTHESIS ORNAMENT
unicode_bidi_class(0x276A, 0x276A, 'ON'). % Ps       MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT
unicode_bidi_class(0x276B, 0x276B, 'ON'). % Pe       MEDIUM FLATTENED RIGHT PARENTHESIS ORNAMENT
unicode_bidi_class(0x276C, 0x276C, 'ON'). % Ps       MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT
unicode_bidi_class(0x276D, 0x276D, 'ON'). % Pe       MEDIUM RIGHT-POINTING ANGLE BRACKET ORNAMENT
unicode_bidi_class(0x276E, 0x276E, 'ON'). % Ps       HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT
unicode_bidi_class(0x276F, 0x276F, 'ON'). % Pe       HEAVY RIGHT-POINTING ANGLE QUOTATION MARK ORNAMENT
unicode_bidi_class(0x2770, 0x2770, 'ON'). % Ps       HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT
unicode_bidi_class(0x2771, 0x2771, 'ON'). % Pe       HEAVY RIGHT-POINTING ANGLE BRACKET ORNAMENT
unicode_bidi_class(0x2772, 0x2772, 'ON'). % Ps       LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT
unicode_bidi_class(0x2773, 0x2773, 'ON'). % Pe       LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT
unicode_bidi_class(0x2774, 0x2774, 'ON'). % Ps       MEDIUM LEFT CURLY BRACKET ORNAMENT
unicode_bidi_class(0x2775, 0x2775, 'ON'). % Pe       MEDIUM RIGHT CURLY BRACKET ORNAMENT
unicode_bidi_class(0x2776, 0x2793, 'ON'). % No  [30] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
unicode_bidi_class(0x2794, 0x27BF, 'ON'). % So  [44] HEAVY WIDE-HEADED RIGHTWARDS ARROW..DOUBLE CURLY LOOP
unicode_bidi_class(0x27C0, 0x27C4, 'ON'). % Sm   [5] THREE DIMENSIONAL ANGLE..OPEN SUPERSET
unicode_bidi_class(0x27C5, 0x27C5, 'ON'). % Ps       LEFT S-SHAPED BAG DELIMITER
unicode_bidi_class(0x27C6, 0x27C6, 'ON'). % Pe       RIGHT S-SHAPED BAG DELIMITER
unicode_bidi_class(0x27C7, 0x27E5, 'ON'). % Sm  [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
unicode_bidi_class(0x27E6, 0x27E6, 'ON'). % Ps       MATHEMATICAL LEFT WHITE SQUARE BRACKET
unicode_bidi_class(0x27E7, 0x27E7, 'ON'). % Pe       MATHEMATICAL RIGHT WHITE SQUARE BRACKET
unicode_bidi_class(0x27E8, 0x27E8, 'ON'). % Ps       MATHEMATICAL LEFT ANGLE BRACKET
unicode_bidi_class(0x27E9, 0x27E9, 'ON'). % Pe       MATHEMATICAL RIGHT ANGLE BRACKET
unicode_bidi_class(0x27EA, 0x27EA, 'ON'). % Ps       MATHEMATICAL LEFT DOUBLE ANGLE BRACKET
unicode_bidi_class(0x27EB, 0x27EB, 'ON'). % Pe       MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET
unicode_bidi_class(0x27EC, 0x27EC, 'ON'). % Ps       MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET
unicode_bidi_class(0x27ED, 0x27ED, 'ON'). % Pe       MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET
unicode_bidi_class(0x27EE, 0x27EE, 'ON'). % Ps       MATHEMATICAL LEFT FLATTENED PARENTHESIS
unicode_bidi_class(0x27EF, 0x27EF, 'ON'). % Pe       MATHEMATICAL RIGHT FLATTENED PARENTHESIS
unicode_bidi_class(0x27F0, 0x27FF, 'ON'). % Sm  [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
unicode_bidi_class(0x2900, 0x2982, 'ON'). % Sm [131] RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE..Z NOTATION TYPE COLON
unicode_bidi_class(0x2983, 0x2983, 'ON'). % Ps       LEFT WHITE CURLY BRACKET
unicode_bidi_class(0x2984, 0x2984, 'ON'). % Pe       RIGHT WHITE CURLY BRACKET
unicode_bidi_class(0x2985, 0x2985, 'ON'). % Ps       LEFT WHITE PARENTHESIS
unicode_bidi_class(0x2986, 0x2986, 'ON'). % Pe       RIGHT WHITE PARENTHESIS
unicode_bidi_class(0x2987, 0x2987, 'ON'). % Ps       Z NOTATION LEFT IMAGE BRACKET
unicode_bidi_class(0x2988, 0x2988, 'ON'). % Pe       Z NOTATION RIGHT IMAGE BRACKET
unicode_bidi_class(0x2989, 0x2989, 'ON'). % Ps       Z NOTATION LEFT BINDING BRACKET
unicode_bidi_class(0x298A, 0x298A, 'ON'). % Pe       Z NOTATION RIGHT BINDING BRACKET
unicode_bidi_class(0x298B, 0x298B, 'ON'). % Ps       LEFT SQUARE BRACKET WITH UNDERBAR
unicode_bidi_class(0x298C, 0x298C, 'ON'). % Pe       RIGHT SQUARE BRACKET WITH UNDERBAR
unicode_bidi_class(0x298D, 0x298D, 'ON'). % Ps       LEFT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_bidi_class(0x298E, 0x298E, 'ON'). % Pe       RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_bidi_class(0x298F, 0x298F, 'ON'). % Ps       LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_bidi_class(0x2990, 0x2990, 'ON'). % Pe       RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_bidi_class(0x2991, 0x2991, 'ON'). % Ps       LEFT ANGLE BRACKET WITH DOT
unicode_bidi_class(0x2992, 0x2992, 'ON'). % Pe       RIGHT ANGLE BRACKET WITH DOT
unicode_bidi_class(0x2993, 0x2993, 'ON'). % Ps       LEFT ARC LESS-THAN BRACKET
unicode_bidi_class(0x2994, 0x2994, 'ON'). % Pe       RIGHT ARC GREATER-THAN BRACKET
unicode_bidi_class(0x2995, 0x2995, 'ON'). % Ps       DOUBLE LEFT ARC GREATER-THAN BRACKET
unicode_bidi_class(0x2996, 0x2996, 'ON'). % Pe       DOUBLE RIGHT ARC LESS-THAN BRACKET
unicode_bidi_class(0x2997, 0x2997, 'ON'). % Ps       LEFT BLACK TORTOISE SHELL BRACKET
unicode_bidi_class(0x2998, 0x2998, 'ON'). % Pe       RIGHT BLACK TORTOISE SHELL BRACKET
unicode_bidi_class(0x2999, 0x29D7, 'ON'). % Sm  [63] DOTTED FENCE..BLACK HOURGLASS
unicode_bidi_class(0x29D8, 0x29D8, 'ON'). % Ps       LEFT WIGGLY FENCE
unicode_bidi_class(0x29D9, 0x29D9, 'ON'). % Pe       RIGHT WIGGLY FENCE
unicode_bidi_class(0x29DA, 0x29DA, 'ON'). % Ps       LEFT DOUBLE WIGGLY FENCE
unicode_bidi_class(0x29DB, 0x29DB, 'ON'). % Pe       RIGHT DOUBLE WIGGLY FENCE
unicode_bidi_class(0x29DC, 0x29FB, 'ON'). % Sm  [32] INCOMPLETE INFINITY..TRIPLE PLUS
unicode_bidi_class(0x29FC, 0x29FC, 'ON'). % Ps       LEFT-POINTING CURVED ANGLE BRACKET
unicode_bidi_class(0x29FD, 0x29FD, 'ON'). % Pe       RIGHT-POINTING CURVED ANGLE BRACKET
unicode_bidi_class(0x29FE, 0x2AFF, 'ON'). % Sm [258] TINY..N-ARY WHITE VERTICAL BAR
unicode_bidi_class(0x2B00, 0x2B2F, 'ON'). % So  [48] NORTH EAST WHITE ARROW..WHITE VERTICAL ELLIPSE
unicode_bidi_class(0x2B30, 0x2B44, 'ON'). % Sm  [21] LEFT ARROW WITH SMALL CIRCLE..RIGHTWARDS ARROW THROUGH SUPERSET
unicode_bidi_class(0x2B45, 0x2B46, 'ON'). % So   [2] LEFTWARDS QUADRUPLE ARROW..RIGHTWARDS QUADRUPLE ARROW
unicode_bidi_class(0x2B47, 0x2B4C, 'ON'). % Sm   [6] REVERSE TILDE OPERATOR ABOVE RIGHTWARDS ARROW..RIGHTWARDS ARROW ABOVE REVERSE TILDE OPERATOR
unicode_bidi_class(0x2B50, 0x2B59, 'ON'). % So  [10] WHITE MEDIUM STAR..HEAVY CIRCLED SALTIRE
unicode_bidi_class(0x2CE5, 0x2CEA, 'ON'). % So   [6] COPTIC SYMBOL MI RO..COPTIC SYMBOL SHIMA SIMA
unicode_bidi_class(0x2CF9, 0x2CFC, 'ON'). % Po   [4] COPTIC OLD NUBIAN FULL STOP..COPTIC OLD NUBIAN VERSE DIVIDER
unicode_bidi_class(0x2CFD, 0x2CFD, 'ON'). % No       COPTIC FRACTION ONE HALF
unicode_bidi_class(0x2CFE, 0x2CFF, 'ON'). % Po   [2] COPTIC FULL STOP..COPTIC MORPHOLOGICAL DIVIDER
unicode_bidi_class(0x2E00, 0x2E01, 'ON'). % Po   [2] RIGHT ANGLE SUBSTITUTION MARKER..RIGHT ANGLE DOTTED SUBSTITUTION MARKER
unicode_bidi_class(0x2E02, 0x2E02, 'ON'). % Pi       LEFT SUBSTITUTION BRACKET
unicode_bidi_class(0x2E03, 0x2E03, 'ON'). % Pf       RIGHT SUBSTITUTION BRACKET
unicode_bidi_class(0x2E04, 0x2E04, 'ON'). % Pi       LEFT DOTTED SUBSTITUTION BRACKET
unicode_bidi_class(0x2E05, 0x2E05, 'ON'). % Pf       RIGHT DOTTED SUBSTITUTION BRACKET
unicode_bidi_class(0x2E06, 0x2E08, 'ON'). % Po   [3] RAISED INTERPOLATION MARKER..DOTTED TRANSPOSITION MARKER
unicode_bidi_class(0x2E09, 0x2E09, 'ON'). % Pi       LEFT TRANSPOSITION BRACKET
unicode_bidi_class(0x2E0A, 0x2E0A, 'ON'). % Pf       RIGHT TRANSPOSITION BRACKET
unicode_bidi_class(0x2E0B, 0x2E0B, 'ON'). % Po       RAISED SQUARE
unicode_bidi_class(0x2E0C, 0x2E0C, 'ON'). % Pi       LEFT RAISED OMISSION BRACKET
unicode_bidi_class(0x2E0D, 0x2E0D, 'ON'). % Pf       RIGHT RAISED OMISSION BRACKET
unicode_bidi_class(0x2E0E, 0x2E16, 'ON'). % Po   [9] EDITORIAL CORONIS..DOTTED RIGHT-POINTING ANGLE
unicode_bidi_class(0x2E17, 0x2E17, 'ON'). % Pd       DOUBLE OBLIQUE HYPHEN
unicode_bidi_class(0x2E18, 0x2E19, 'ON'). % Po   [2] INVERTED INTERROBANG..PALM BRANCH
unicode_bidi_class(0x2E1A, 0x2E1A, 'ON'). % Pd       HYPHEN WITH DIAERESIS
unicode_bidi_class(0x2E1B, 0x2E1B, 'ON'). % Po       TILDE WITH RING ABOVE
unicode_bidi_class(0x2E1C, 0x2E1C, 'ON'). % Pi       LEFT LOW PARAPHRASE BRACKET
unicode_bidi_class(0x2E1D, 0x2E1D, 'ON'). % Pf       RIGHT LOW PARAPHRASE BRACKET
unicode_bidi_class(0x2E1E, 0x2E1F, 'ON'). % Po   [2] TILDE WITH DOT ABOVE..TILDE WITH DOT BELOW
unicode_bidi_class(0x2E20, 0x2E20, 'ON'). % Pi       LEFT VERTICAL BAR WITH QUILL
unicode_bidi_class(0x2E21, 0x2E21, 'ON'). % Pf       RIGHT VERTICAL BAR WITH QUILL
unicode_bidi_class(0x2E22, 0x2E22, 'ON'). % Ps       TOP LEFT HALF BRACKET
unicode_bidi_class(0x2E23, 0x2E23, 'ON'). % Pe       TOP RIGHT HALF BRACKET
unicode_bidi_class(0x2E24, 0x2E24, 'ON'). % Ps       BOTTOM LEFT HALF BRACKET
unicode_bidi_class(0x2E25, 0x2E25, 'ON'). % Pe       BOTTOM RIGHT HALF BRACKET
unicode_bidi_class(0x2E26, 0x2E26, 'ON'). % Ps       LEFT SIDEWAYS U BRACKET
unicode_bidi_class(0x2E27, 0x2E27, 'ON'). % Pe       RIGHT SIDEWAYS U BRACKET
unicode_bidi_class(0x2E28, 0x2E28, 'ON'). % Ps       LEFT DOUBLE PARENTHESIS
unicode_bidi_class(0x2E29, 0x2E29, 'ON'). % Pe       RIGHT DOUBLE PARENTHESIS
unicode_bidi_class(0x2E2A, 0x2E2E, 'ON'). % Po   [5] TWO DOTS OVER ONE DOT PUNCTUATION..REVERSED QUESTION MARK
unicode_bidi_class(0x2E2F, 0x2E2F, 'ON'). % Lm       VERTICAL TILDE
unicode_bidi_class(0x2E30, 0x2E39, 'ON'). % Po  [10] RING POINT..TOP HALF SECTION SIGN
unicode_bidi_class(0x2E3A, 0x2E3B, 'ON'). % Pd   [2] TWO-EM DASH..THREE-EM DASH
unicode_bidi_class(0x2E80, 0x2E99, 'ON'). % So  [26] CJK RADICAL REPEAT..CJK RADICAL RAP
unicode_bidi_class(0x2E9B, 0x2EF3, 'ON'). % So  [89] CJK RADICAL CHOKE..CJK RADICAL C-SIMPLIFIED TURTLE
unicode_bidi_class(0x2F00, 0x2FD5, 'ON'). % So [214] KANGXI RADICAL ONE..KANGXI RADICAL FLUTE
unicode_bidi_class(0x2FF0, 0x2FFB, 'ON'). % So  [12] IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT..IDEOGRAPHIC DESCRIPTION CHARACTER OVERLAID
unicode_bidi_class(0x3001, 0x3003, 'ON'). % Po   [3] IDEOGRAPHIC COMMA..DITTO MARK
unicode_bidi_class(0x3004, 0x3004, 'ON'). % So       JAPANESE INDUSTRIAL STANDARD SYMBOL
unicode_bidi_class(0x3008, 0x3008, 'ON'). % Ps       LEFT ANGLE BRACKET
unicode_bidi_class(0x3009, 0x3009, 'ON'). % Pe       RIGHT ANGLE BRACKET
unicode_bidi_class(0x300A, 0x300A, 'ON'). % Ps       LEFT DOUBLE ANGLE BRACKET
unicode_bidi_class(0x300B, 0x300B, 'ON'). % Pe       RIGHT DOUBLE ANGLE BRACKET
unicode_bidi_class(0x300C, 0x300C, 'ON'). % Ps       LEFT CORNER BRACKET
unicode_bidi_class(0x300D, 0x300D, 'ON'). % Pe       RIGHT CORNER BRACKET
unicode_bidi_class(0x300E, 0x300E, 'ON'). % Ps       LEFT WHITE CORNER BRACKET
unicode_bidi_class(0x300F, 0x300F, 'ON'). % Pe       RIGHT WHITE CORNER BRACKET
unicode_bidi_class(0x3010, 0x3010, 'ON'). % Ps       LEFT BLACK LENTICULAR BRACKET
unicode_bidi_class(0x3011, 0x3011, 'ON'). % Pe       RIGHT BLACK LENTICULAR BRACKET
unicode_bidi_class(0x3012, 0x3013, 'ON'). % So   [2] POSTAL MARK..GETA MARK
unicode_bidi_class(0x3014, 0x3014, 'ON'). % Ps       LEFT TORTOISE SHELL BRACKET
unicode_bidi_class(0x3015, 0x3015, 'ON'). % Pe       RIGHT TORTOISE SHELL BRACKET
unicode_bidi_class(0x3016, 0x3016, 'ON'). % Ps       LEFT WHITE LENTICULAR BRACKET
unicode_bidi_class(0x3017, 0x3017, 'ON'). % Pe       RIGHT WHITE LENTICULAR BRACKET
unicode_bidi_class(0x3018, 0x3018, 'ON'). % Ps       LEFT WHITE TORTOISE SHELL BRACKET
unicode_bidi_class(0x3019, 0x3019, 'ON'). % Pe       RIGHT WHITE TORTOISE SHELL BRACKET
unicode_bidi_class(0x301A, 0x301A, 'ON'). % Ps       LEFT WHITE SQUARE BRACKET
unicode_bidi_class(0x301B, 0x301B, 'ON'). % Pe       RIGHT WHITE SQUARE BRACKET
unicode_bidi_class(0x301C, 0x301C, 'ON'). % Pd       WAVE DASH
unicode_bidi_class(0x301D, 0x301D, 'ON'). % Ps       REVERSED DOUBLE PRIME QUOTATION MARK
unicode_bidi_class(0x301E, 0x301F, 'ON'). % Pe   [2] DOUBLE PRIME QUOTATION MARK..LOW DOUBLE PRIME QUOTATION MARK
unicode_bidi_class(0x3020, 0x3020, 'ON'). % So       POSTAL MARK FACE
unicode_bidi_class(0x3030, 0x3030, 'ON'). % Pd       WAVY DASH
unicode_bidi_class(0x3036, 0x3037, 'ON'). % So   [2] CIRCLED POSTAL MARK..IDEOGRAPHIC TELEGRAPH LINE FEED SEPARATOR SYMBOL
unicode_bidi_class(0x303D, 0x303D, 'ON'). % Po       PART ALTERNATION MARK
unicode_bidi_class(0x303E, 0x303F, 'ON'). % So   [2] IDEOGRAPHIC VARIATION INDICATOR..IDEOGRAPHIC HALF FILL SPACE
unicode_bidi_class(0x309B, 0x309C, 'ON'). % Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_bidi_class(0x30A0, 0x30A0, 'ON'). % Pd       KATAKANA-HIRAGANA DOUBLE HYPHEN
unicode_bidi_class(0x30FB, 0x30FB, 'ON'). % Po       KATAKANA MIDDLE DOT
unicode_bidi_class(0x31C0, 0x31E3, 'ON'). % So  [36] CJK STROKE T..CJK STROKE Q
unicode_bidi_class(0x321D, 0x321E, 'ON'). % So   [2] PARENTHESIZED KOREAN CHARACTER OJEON..PARENTHESIZED KOREAN CHARACTER O HU
unicode_bidi_class(0x3250, 0x3250, 'ON'). % So       PARTNERSHIP SIGN
unicode_bidi_class(0x3251, 0x325F, 'ON'). % No  [15] CIRCLED NUMBER TWENTY ONE..CIRCLED NUMBER THIRTY FIVE
unicode_bidi_class(0x327C, 0x327E, 'ON'). % So   [3] CIRCLED KOREAN CHARACTER CHAMKO..CIRCLED HANGUL IEUNG U
unicode_bidi_class(0x32B1, 0x32BF, 'ON'). % No  [15] CIRCLED NUMBER THIRTY SIX..CIRCLED NUMBER FIFTY
unicode_bidi_class(0x32CC, 0x32CF, 'ON'). % So   [4] SQUARE HG..LIMITED LIABILITY SIGN
unicode_bidi_class(0x3377, 0x337A, 'ON'). % So   [4] SQUARE DM..SQUARE IU
unicode_bidi_class(0x33DE, 0x33DF, 'ON'). % So   [2] SQUARE V OVER M..SQUARE A OVER M
unicode_bidi_class(0x33FF, 0x33FF, 'ON'). % So       SQUARE GAL
unicode_bidi_class(0x4DC0, 0x4DFF, 'ON'). % So  [64] HEXAGRAM FOR THE CREATIVE HEAVEN..HEXAGRAM FOR BEFORE COMPLETION
unicode_bidi_class(0xA490, 0xA4C6, 'ON'). % So  [55] YI RADICAL QOT..YI RADICAL KE
unicode_bidi_class(0xA60D, 0xA60F, 'ON'). % Po   [3] VAI COMMA..VAI QUESTION MARK
unicode_bidi_class(0xA673, 0xA673, 'ON'). % Po       SLAVONIC ASTERISK
unicode_bidi_class(0xA67E, 0xA67E, 'ON'). % Po       CYRILLIC KAVYKA
unicode_bidi_class(0xA67F, 0xA67F, 'ON'). % Lm       CYRILLIC PAYEROK
unicode_bidi_class(0xA700, 0xA716, 'ON'). % Sk  [23] MODIFIER LETTER CHINESE TONE YIN PING..MODIFIER LETTER EXTRA-LOW LEFT-STEM TONE BAR
unicode_bidi_class(0xA717, 0xA71F, 'ON'). % Lm   [9] MODIFIER LETTER DOT VERTICAL BAR..MODIFIER LETTER LOW INVERTED EXCLAMATION MARK
unicode_bidi_class(0xA720, 0xA721, 'ON'). % Sk   [2] MODIFIER LETTER STRESS AND HIGH TONE..MODIFIER LETTER STRESS AND LOW TONE
unicode_bidi_class(0xA788, 0xA788, 'ON'). % Lm       MODIFIER LETTER LOW CIRCUMFLEX ACCENT
unicode_bidi_class(0xA828, 0xA82B, 'ON'). % So   [4] SYLOTI NAGRI POETRY MARK-1..SYLOTI NAGRI POETRY MARK-4
unicode_bidi_class(0xA874, 0xA877, 'ON'). % Po   [4] PHAGS-PA SINGLE HEAD MARK..PHAGS-PA MARK DOUBLE SHAD
unicode_bidi_class(0xFD3E, 0xFD3E, 'ON'). % Ps       ORNATE LEFT PARENTHESIS
unicode_bidi_class(0xFD3F, 0xFD3F, 'ON'). % Pe       ORNATE RIGHT PARENTHESIS
unicode_bidi_class(0xFDFD, 0xFDFD, 'ON'). % So       ARABIC LIGATURE BISMILLAH AR-RAHMAN AR-RAHEEM
unicode_bidi_class(0xFE10, 0xFE16, 'ON'). % Po   [7] PRESENTATION FORM FOR VERTICAL COMMA..PRESENTATION FORM FOR VERTICAL QUESTION MARK
unicode_bidi_class(0xFE17, 0xFE17, 'ON'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE LENTICULAR BRACKET
unicode_bidi_class(0xFE18, 0xFE18, 'ON'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET
unicode_bidi_class(0xFE19, 0xFE19, 'ON'). % Po       PRESENTATION FORM FOR VERTICAL HORIZONTAL ELLIPSIS
unicode_bidi_class(0xFE30, 0xFE30, 'ON'). % Po       PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
unicode_bidi_class(0xFE31, 0xFE32, 'ON'). % Pd   [2] PRESENTATION FORM FOR VERTICAL EM DASH..PRESENTATION FORM FOR VERTICAL EN DASH
unicode_bidi_class(0xFE33, 0xFE34, 'ON'). % Pc   [2] PRESENTATION FORM FOR VERTICAL LOW LINE..PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
unicode_bidi_class(0xFE35, 0xFE35, 'ON'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
unicode_bidi_class(0xFE36, 0xFE36, 'ON'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
unicode_bidi_class(0xFE37, 0xFE37, 'ON'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
unicode_bidi_class(0xFE38, 0xFE38, 'ON'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
unicode_bidi_class(0xFE39, 0xFE39, 'ON'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
unicode_bidi_class(0xFE3A, 0xFE3A, 'ON'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
unicode_bidi_class(0xFE3B, 0xFE3B, 'ON'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
unicode_bidi_class(0xFE3C, 0xFE3C, 'ON'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
unicode_bidi_class(0xFE3D, 0xFE3D, 'ON'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
unicode_bidi_class(0xFE3E, 0xFE3E, 'ON'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
unicode_bidi_class(0xFE3F, 0xFE3F, 'ON'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
unicode_bidi_class(0xFE40, 0xFE40, 'ON'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
unicode_bidi_class(0xFE41, 0xFE41, 'ON'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
unicode_bidi_class(0xFE42, 0xFE42, 'ON'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
unicode_bidi_class(0xFE43, 0xFE43, 'ON'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
unicode_bidi_class(0xFE44, 0xFE44, 'ON'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
unicode_bidi_class(0xFE45, 0xFE46, 'ON'). % Po   [2] SESAME DOT..WHITE SESAME DOT
unicode_bidi_class(0xFE47, 0xFE47, 'ON'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT SQUARE BRACKET
unicode_bidi_class(0xFE48, 0xFE48, 'ON'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT SQUARE BRACKET
unicode_bidi_class(0xFE49, 0xFE4C, 'ON'). % Po   [4] DASHED OVERLINE..DOUBLE WAVY OVERLINE
unicode_bidi_class(0xFE4D, 0xFE4F, 'ON'). % Pc   [3] DASHED LOW LINE..WAVY LOW LINE
unicode_bidi_class(0xFE51, 0xFE51, 'ON'). % Po       SMALL IDEOGRAPHIC COMMA
unicode_bidi_class(0xFE54, 0xFE54, 'ON'). % Po       SMALL SEMICOLON
unicode_bidi_class(0xFE56, 0xFE57, 'ON'). % Po   [2] SMALL QUESTION MARK..SMALL EXCLAMATION MARK
unicode_bidi_class(0xFE58, 0xFE58, 'ON'). % Pd       SMALL EM DASH
unicode_bidi_class(0xFE59, 0xFE59, 'ON'). % Ps       SMALL LEFT PARENTHESIS
unicode_bidi_class(0xFE5A, 0xFE5A, 'ON'). % Pe       SMALL RIGHT PARENTHESIS
unicode_bidi_class(0xFE5B, 0xFE5B, 'ON'). % Ps       SMALL LEFT CURLY BRACKET
unicode_bidi_class(0xFE5C, 0xFE5C, 'ON'). % Pe       SMALL RIGHT CURLY BRACKET
unicode_bidi_class(0xFE5D, 0xFE5D, 'ON'). % Ps       SMALL LEFT TORTOISE SHELL BRACKET
unicode_bidi_class(0xFE5E, 0xFE5E, 'ON'). % Pe       SMALL RIGHT TORTOISE SHELL BRACKET
unicode_bidi_class(0xFE60, 0xFE61, 'ON'). % Po   [2] SMALL AMPERSAND..SMALL ASTERISK
unicode_bidi_class(0xFE64, 0xFE66, 'ON'). % Sm   [3] SMALL LESS-THAN SIGN..SMALL EQUALS SIGN
unicode_bidi_class(0xFE68, 0xFE68, 'ON'). % Po       SMALL REVERSE SOLIDUS
unicode_bidi_class(0xFE6B, 0xFE6B, 'ON'). % Po       SMALL COMMERCIAL AT
unicode_bidi_class(0xFF01, 0xFF02, 'ON'). % Po   [2] FULLWIDTH EXCLAMATION MARK..FULLWIDTH QUOTATION MARK
unicode_bidi_class(0xFF06, 0xFF07, 'ON'). % Po   [2] FULLWIDTH AMPERSAND..FULLWIDTH APOSTROPHE
unicode_bidi_class(0xFF08, 0xFF08, 'ON'). % Ps       FULLWIDTH LEFT PARENTHESIS
unicode_bidi_class(0xFF09, 0xFF09, 'ON'). % Pe       FULLWIDTH RIGHT PARENTHESIS
unicode_bidi_class(0xFF0A, 0xFF0A, 'ON'). % Po       FULLWIDTH ASTERISK
unicode_bidi_class(0xFF1B, 0xFF1B, 'ON'). % Po       FULLWIDTH SEMICOLON
unicode_bidi_class(0xFF1C, 0xFF1E, 'ON'). % Sm   [3] FULLWIDTH LESS-THAN SIGN..FULLWIDTH GREATER-THAN SIGN
unicode_bidi_class(0xFF1F, 0xFF20, 'ON'). % Po   [2] FULLWIDTH QUESTION MARK..FULLWIDTH COMMERCIAL AT
unicode_bidi_class(0xFF3B, 0xFF3B, 'ON'). % Ps       FULLWIDTH LEFT SQUARE BRACKET
unicode_bidi_class(0xFF3C, 0xFF3C, 'ON'). % Po       FULLWIDTH REVERSE SOLIDUS
unicode_bidi_class(0xFF3D, 0xFF3D, 'ON'). % Pe       FULLWIDTH RIGHT SQUARE BRACKET
unicode_bidi_class(0xFF3E, 0xFF3E, 'ON'). % Sk       FULLWIDTH CIRCUMFLEX ACCENT
unicode_bidi_class(0xFF3F, 0xFF3F, 'ON'). % Pc       FULLWIDTH LOW LINE
unicode_bidi_class(0xFF40, 0xFF40, 'ON'). % Sk       FULLWIDTH GRAVE ACCENT
unicode_bidi_class(0xFF5B, 0xFF5B, 'ON'). % Ps       FULLWIDTH LEFT CURLY BRACKET
unicode_bidi_class(0xFF5C, 0xFF5C, 'ON'). % Sm       FULLWIDTH VERTICAL LINE
unicode_bidi_class(0xFF5D, 0xFF5D, 'ON'). % Pe       FULLWIDTH RIGHT CURLY BRACKET
unicode_bidi_class(0xFF5E, 0xFF5E, 'ON'). % Sm       FULLWIDTH TILDE
unicode_bidi_class(0xFF5F, 0xFF5F, 'ON'). % Ps       FULLWIDTH LEFT WHITE PARENTHESIS
unicode_bidi_class(0xFF60, 0xFF60, 'ON'). % Pe       FULLWIDTH RIGHT WHITE PARENTHESIS
unicode_bidi_class(0xFF61, 0xFF61, 'ON'). % Po       HALFWIDTH IDEOGRAPHIC FULL STOP
unicode_bidi_class(0xFF62, 0xFF62, 'ON'). % Ps       HALFWIDTH LEFT CORNER BRACKET
unicode_bidi_class(0xFF63, 0xFF63, 'ON'). % Pe       HALFWIDTH RIGHT CORNER BRACKET
unicode_bidi_class(0xFF64, 0xFF65, 'ON'). % Po   [2] HALFWIDTH IDEOGRAPHIC COMMA..HALFWIDTH KATAKANA MIDDLE DOT
unicode_bidi_class(0xFFE2, 0xFFE2, 'ON'). % Sm       FULLWIDTH NOT SIGN
unicode_bidi_class(0xFFE3, 0xFFE3, 'ON'). % Sk       FULLWIDTH MACRON
unicode_bidi_class(0xFFE4, 0xFFE4, 'ON'). % So       FULLWIDTH BROKEN BAR
unicode_bidi_class(0xFFE8, 0xFFE8, 'ON'). % So       HALFWIDTH FORMS LIGHT VERTICAL
unicode_bidi_class(0xFFE9, 0xFFEC, 'ON'). % Sm   [4] HALFWIDTH LEFTWARDS ARROW..HALFWIDTH DOWNWARDS ARROW
unicode_bidi_class(0xFFED, 0xFFEE, 'ON'). % So   [2] HALFWIDTH BLACK SQUARE..HALFWIDTH WHITE CIRCLE
unicode_bidi_class(0xFFF9, 0xFFFB, 'ON'). % Cf   [3] INTERLINEAR ANNOTATION ANCHOR..INTERLINEAR ANNOTATION TERMINATOR
unicode_bidi_class(0xFFFC, 0xFFFD, 'ON'). % So   [2] OBJECT REPLACEMENT CHARACTER..REPLACEMENT CHARACTER
unicode_bidi_class(0x10101, 0x10101, 'ON'). % Po       AEGEAN WORD SEPARATOR DOT
unicode_bidi_class(0x10140, 0x10174, 'ON'). % Nl  [53] GREEK ACROPHONIC ATTIC ONE QUARTER..GREEK ACROPHONIC STRATIAN FIFTY MNAS
unicode_bidi_class(0x10175, 0x10178, 'ON'). % No   [4] GREEK ONE HALF SIGN..GREEK THREE QUARTERS SIGN
unicode_bidi_class(0x10179, 0x10189, 'ON'). % So  [17] GREEK YEAR SIGN..GREEK TRYBLION BASE SIGN
unicode_bidi_class(0x1018A, 0x1018A, 'ON'). % No       GREEK ZERO SIGN
unicode_bidi_class(0x10190, 0x1019B, 'ON'). % So  [12] ROMAN SEXTANS SIGN..ROMAN CENTURIAL SIGN
unicode_bidi_class(0x1091F, 0x1091F, 'ON'). % Po       PHOENICIAN WORD SEPARATOR
unicode_bidi_class(0x10B39, 0x10B3F, 'ON'). % Po   [7] AVESTAN ABBREVIATION MARK..LARGE ONE RING OVER TWO RINGS PUNCTUATION
unicode_bidi_class(0x11052, 0x11065, 'ON'). % No  [20] BRAHMI NUMBER ONE..BRAHMI NUMBER ONE THOUSAND
unicode_bidi_class(0x1D200, 0x1D241, 'ON'). % So  [66] GREEK VOCAL NOTATION SYMBOL-1..GREEK INSTRUMENTAL NOTATION SYMBOL-54
unicode_bidi_class(0x1D245, 0x1D245, 'ON'). % So       GREEK MUSICAL LEIMMA
unicode_bidi_class(0x1D300, 0x1D356, 'ON'). % So  [87] MONOGRAM FOR EARTH..TETRAGRAM FOR FOSTERING
unicode_bidi_class(0x1D6DB, 0x1D6DB, 'ON'). % Sm       MATHEMATICAL BOLD PARTIAL DIFFERENTIAL
unicode_bidi_class(0x1D715, 0x1D715, 'ON'). % Sm       MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL
unicode_bidi_class(0x1D74F, 0x1D74F, 'ON'). % Sm       MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_bidi_class(0x1D789, 0x1D789, 'ON'). % Sm       MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL
unicode_bidi_class(0x1D7C3, 0x1D7C3, 'ON'). % Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_bidi_class(0x1EEF0, 0x1EEF1, 'ON'). % Sm   [2] ARABIC MATHEMATICAL OPERATOR MEEM WITH HAH WITH TATWEEL..ARABIC MATHEMATICAL OPERATOR HAH WITH DAL
unicode_bidi_class(0x1F000, 0x1F02B, 'ON'). % So  [44] MAHJONG TILE EAST WIND..MAHJONG TILE BACK
unicode_bidi_class(0x1F030, 0x1F093, 'ON'). % So [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
unicode_bidi_class(0x1F0A0, 0x1F0AE, 'ON'). % So  [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
unicode_bidi_class(0x1F0B1, 0x1F0BE, 'ON'). % So  [14] PLAYING CARD ACE OF HEARTS..PLAYING CARD KING OF HEARTS
unicode_bidi_class(0x1F0C1, 0x1F0CF, 'ON'). % So  [15] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD BLACK JOKER
unicode_bidi_class(0x1F0D1, 0x1F0DF, 'ON'). % So  [15] PLAYING CARD ACE OF CLUBS..PLAYING CARD WHITE JOKER
unicode_bidi_class(0x1F16A, 0x1F16B, 'ON'). % So   [2] RAISED MC SIGN..RAISED MD SIGN
unicode_bidi_class(0x1F300, 0x1F320, 'ON'). % So  [33] CYCLONE..SHOOTING STAR
unicode_bidi_class(0x1F330, 0x1F335, 'ON'). % So   [6] CHESTNUT..CACTUS
unicode_bidi_class(0x1F337, 0x1F37C, 'ON'). % So  [70] TULIP..BABY BOTTLE
unicode_bidi_class(0x1F380, 0x1F393, 'ON'). % So  [20] RIBBON..GRADUATION CAP
unicode_bidi_class(0x1F3A0, 0x1F3C4, 'ON'). % So  [37] CAROUSEL HORSE..SURFER
unicode_bidi_class(0x1F3C6, 0x1F3CA, 'ON'). % So   [5] TROPHY..SWIMMER
unicode_bidi_class(0x1F3E0, 0x1F3F0, 'ON'). % So  [17] HOUSE BUILDING..EUROPEAN CASTLE
unicode_bidi_class(0x1F400, 0x1F43E, 'ON'). % So  [63] RAT..PAW PRINTS
unicode_bidi_class(0x1F440, 0x1F440, 'ON'). % So       EYES
unicode_bidi_class(0x1F442, 0x1F4F7, 'ON'). % So [182] EAR..CAMERA
unicode_bidi_class(0x1F4F9, 0x1F4FC, 'ON'). % So   [4] VIDEO CAMERA..VIDEOCASSETTE
unicode_bidi_class(0x1F500, 0x1F53D, 'ON'). % So  [62] TWISTED RIGHTWARDS ARROWS..DOWN-POINTING SMALL RED TRIANGLE
unicode_bidi_class(0x1F540, 0x1F543, 'ON'). % So   [4] CIRCLED CROSS POMMEE..NOTCHED LEFT SEMICIRCLE WITH THREE DOTS
unicode_bidi_class(0x1F550, 0x1F567, 'ON'). % So  [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
unicode_bidi_class(0x1F5FB, 0x1F640, 'ON'). % So  [70] MOUNT FUJI..WEARY CAT FACE
unicode_bidi_class(0x1F645, 0x1F64F, 'ON'). % So  [11] FACE WITH NO GOOD GESTURE..PERSON WITH FOLDED HANDS
unicode_bidi_class(0x1F680, 0x1F6C5, 'ON'). % So  [70] ROCKET..LEFT LUGGAGE
unicode_bidi_class(0x1F700, 0x1F773, 'ON'). % So [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE

% Total code points: 4447

% ================================================

% Bidi_Class=Boundary_Neutral

unicode_bidi_class(0x0000, 0x0008, 'BN'). % Cc   [9] <control-0000>..<control-0008>
unicode_bidi_class(0x000E, 0x001B, 'BN'). % Cc  [14] <control-000E>..<control-001B>
unicode_bidi_class(0x007F, 0x0084, 'BN'). % Cc   [6] <control-007F>..<control-0084>
unicode_bidi_class(0x0086, 0x009F, 'BN'). % Cc  [26] <control-0086>..<control-009F>
unicode_bidi_class(0x00AD, 0x00AD, 'BN'). % Cf       SOFT HYPHEN
unicode_bidi_class(0x200B, 0x200D, 'BN'). % Cf   [3] ZERO WIDTH SPACE..ZERO WIDTH JOINER
unicode_bidi_class(0x2060, 0x2064, 'BN'). % Cf   [5] WORD JOINER..INVISIBLE PLUS
unicode_bidi_class(0x2065, 0x2069, 'BN'). % Cn   [5] <reserved-2065>..<reserved-2069>
unicode_bidi_class(0x206A, 0x206F, 'BN'). % Cf   [6] INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES
unicode_bidi_class(0xFDD0, 0xFDEF, 'BN'). % Cn  [32] <noncharacter-FDD0>..<noncharacter-FDEF>
unicode_bidi_class(0xFEFF, 0xFEFF, 'BN'). % Cf       ZERO WIDTH NO-BREAK SPACE
unicode_bidi_class(0xFFF0, 0xFFF8, 'BN'). % Cn   [9] <reserved-FFF0>..<reserved-FFF8>
unicode_bidi_class(0xFFFE, 0xFFFF, 'BN'). % Cn   [2] <noncharacter-FFFE>..<noncharacter-FFFF>
unicode_bidi_class(0x1D173, 0x1D17A, 'BN'). % Cf   [8] MUSICAL SYMBOL BEGIN BEAM..MUSICAL SYMBOL END PHRASE
unicode_bidi_class(0x1FFFE, 0x1FFFF, 'BN'). % Cn   [2] <noncharacter-1FFFE>..<noncharacter-1FFFF>
unicode_bidi_class(0x2FFFE, 0x2FFFF, 'BN'). % Cn   [2] <noncharacter-2FFFE>..<noncharacter-2FFFF>
unicode_bidi_class(0x3FFFE, 0x3FFFF, 'BN'). % Cn   [2] <noncharacter-3FFFE>..<noncharacter-3FFFF>
unicode_bidi_class(0x4FFFE, 0x4FFFF, 'BN'). % Cn   [2] <noncharacter-4FFFE>..<noncharacter-4FFFF>
unicode_bidi_class(0x5FFFE, 0x5FFFF, 'BN'). % Cn   [2] <noncharacter-5FFFE>..<noncharacter-5FFFF>
unicode_bidi_class(0x6FFFE, 0x6FFFF, 'BN'). % Cn   [2] <noncharacter-6FFFE>..<noncharacter-6FFFF>
unicode_bidi_class(0x7FFFE, 0x7FFFF, 'BN'). % Cn   [2] <noncharacter-7FFFE>..<noncharacter-7FFFF>
unicode_bidi_class(0x8FFFE, 0x8FFFF, 'BN'). % Cn   [2] <noncharacter-8FFFE>..<noncharacter-8FFFF>
unicode_bidi_class(0x9FFFE, 0x9FFFF, 'BN'). % Cn   [2] <noncharacter-9FFFE>..<noncharacter-9FFFF>
unicode_bidi_class(0xAFFFE, 0xAFFFF, 'BN'). % Cn   [2] <noncharacter-AFFFE>..<noncharacter-AFFFF>
unicode_bidi_class(0xBFFFE, 0xBFFFF, 'BN'). % Cn   [2] <noncharacter-BFFFE>..<noncharacter-BFFFF>
unicode_bidi_class(0xCFFFE, 0xCFFFF, 'BN'). % Cn   [2] <noncharacter-CFFFE>..<noncharacter-CFFFF>
unicode_bidi_class(0xDFFFE, 0xE0000, 'BN'). % Cn   [3] <noncharacter-DFFFE>..<reserved-E0000>
unicode_bidi_class(0xE0001, 0xE0001, 'BN'). % Cf       LANGUAGE TAG
unicode_bidi_class(0xE0002, 0xE001F, 'BN'). % Cn  [30] <reserved-E0002>..<reserved-E001F>
unicode_bidi_class(0xE0020, 0xE007F, 'BN'). % Cf  [96] TAG SPACE..CANCEL TAG
unicode_bidi_class(0xE0080, 0xE00FF, 'BN'). % Cn [128] <reserved-E0080>..<reserved-E00FF>
unicode_bidi_class(0xE01F0, 0xE0FFF, 'BN'). % Cn [3600] <reserved-E01F0>..<reserved-E0FFF>
unicode_bidi_class(0xEFFFE, 0xEFFFF, 'BN'). % Cn   [2] <noncharacter-EFFFE>..<noncharacter-EFFFF>
unicode_bidi_class(0xFFFFE, 0xFFFFF, 'BN'). % Cn   [2] <noncharacter-FFFFE>..<noncharacter-FFFFF>
unicode_bidi_class(0x10FFFE, 0x10FFFF, 'BN'). % Cn   [2] <noncharacter-10FFFE>..<noncharacter-10FFFF>

% Total code points: 4015

% ================================================

% Bidi_Class=Nonspacing_Mark

unicode_bidi_class(0x0300, 0x036F, 'NSM'). % Mn [112] COMBINING GRAVE ACCENT..COMBINING LATIN SMALL LETTER X
unicode_bidi_class(0x0483, 0x0487, 'NSM'). % Mn   [5] COMBINING CYRILLIC TITLO..COMBINING CYRILLIC POKRYTIE
unicode_bidi_class(0x0488, 0x0489, 'NSM'). % Me   [2] COMBINING CYRILLIC HUNDRED THOUSANDS SIGN..COMBINING CYRILLIC MILLIONS SIGN
unicode_bidi_class(0x0591, 0x05BD, 'NSM'). % Mn  [45] HEBREW ACCENT ETNAHTA..HEBREW POINT METEG
unicode_bidi_class(0x05BF, 0x05BF, 'NSM'). % Mn       HEBREW POINT RAFE
unicode_bidi_class(0x05C1, 0x05C2, 'NSM'). % Mn   [2] HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT
unicode_bidi_class(0x05C4, 0x05C5, 'NSM'). % Mn   [2] HEBREW MARK UPPER DOT..HEBREW MARK LOWER DOT
unicode_bidi_class(0x05C7, 0x05C7, 'NSM'). % Mn       HEBREW POINT QAMATS QATAN
unicode_bidi_class(0x0610, 0x061A, 'NSM'). % Mn  [11] ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM..ARABIC SMALL KASRA
unicode_bidi_class(0x064B, 0x065F, 'NSM'). % Mn  [21] ARABIC FATHATAN..ARABIC WAVY HAMZA BELOW
unicode_bidi_class(0x0670, 0x0670, 'NSM'). % Mn       ARABIC LETTER SUPERSCRIPT ALEF
unicode_bidi_class(0x06D6, 0x06DC, 'NSM'). % Mn   [7] ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN
unicode_bidi_class(0x06DF, 0x06E4, 'NSM'). % Mn   [6] ARABIC SMALL HIGH ROUNDED ZERO..ARABIC SMALL HIGH MADDA
unicode_bidi_class(0x06E7, 0x06E8, 'NSM'). % Mn   [2] ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON
unicode_bidi_class(0x06EA, 0x06ED, 'NSM'). % Mn   [4] ARABIC EMPTY CENTRE LOW STOP..ARABIC SMALL LOW MEEM
unicode_bidi_class(0x0711, 0x0711, 'NSM'). % Mn       SYRIAC LETTER SUPERSCRIPT ALAPH
unicode_bidi_class(0x0730, 0x074A, 'NSM'). % Mn  [27] SYRIAC PTHAHA ABOVE..SYRIAC BARREKH
unicode_bidi_class(0x07A6, 0x07B0, 'NSM'). % Mn  [11] THAANA ABAFILI..THAANA SUKUN
unicode_bidi_class(0x07EB, 0x07F3, 'NSM'). % Mn   [9] NKO COMBINING SHORT HIGH TONE..NKO COMBINING DOUBLE DOT ABOVE
unicode_bidi_class(0x0816, 0x0819, 'NSM'). % Mn   [4] SAMARITAN MARK IN..SAMARITAN MARK DAGESH
unicode_bidi_class(0x081B, 0x0823, 'NSM'). % Mn   [9] SAMARITAN MARK EPENTHETIC YUT..SAMARITAN VOWEL SIGN A
unicode_bidi_class(0x0825, 0x0827, 'NSM'). % Mn   [3] SAMARITAN VOWEL SIGN SHORT A..SAMARITAN VOWEL SIGN U
unicode_bidi_class(0x0829, 0x082D, 'NSM'). % Mn   [5] SAMARITAN VOWEL SIGN LONG I..SAMARITAN MARK NEQUDAA
unicode_bidi_class(0x0859, 0x085B, 'NSM'). % Mn   [3] MANDAIC AFFRICATION MARK..MANDAIC GEMINATION MARK
unicode_bidi_class(0x08E4, 0x08FE, 'NSM'). % Mn  [27] ARABIC CURLY FATHA..ARABIC DAMMA WITH DOT
unicode_bidi_class(0x0900, 0x0902, 'NSM'). % Mn   [3] DEVANAGARI SIGN INVERTED CANDRABINDU..DEVANAGARI SIGN ANUSVARA
unicode_bidi_class(0x093A, 0x093A, 'NSM'). % Mn       DEVANAGARI VOWEL SIGN OE
unicode_bidi_class(0x093C, 0x093C, 'NSM'). % Mn       DEVANAGARI SIGN NUKTA
unicode_bidi_class(0x0941, 0x0948, 'NSM'). % Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
unicode_bidi_class(0x094D, 0x094D, 'NSM'). % Mn       DEVANAGARI SIGN VIRAMA
unicode_bidi_class(0x0951, 0x0957, 'NSM'). % Mn   [7] DEVANAGARI STRESS SIGN UDATTA..DEVANAGARI VOWEL SIGN UUE
unicode_bidi_class(0x0962, 0x0963, 'NSM'). % Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
unicode_bidi_class(0x0981, 0x0981, 'NSM'). % Mn       BENGALI SIGN CANDRABINDU
unicode_bidi_class(0x09BC, 0x09BC, 'NSM'). % Mn       BENGALI SIGN NUKTA
unicode_bidi_class(0x09C1, 0x09C4, 'NSM'). % Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
unicode_bidi_class(0x09CD, 0x09CD, 'NSM'). % Mn       BENGALI SIGN VIRAMA
unicode_bidi_class(0x09E2, 0x09E3, 'NSM'). % Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
unicode_bidi_class(0x0A01, 0x0A02, 'NSM'). % Mn   [2] GURMUKHI SIGN ADAK BINDI..GURMUKHI SIGN BINDI
unicode_bidi_class(0x0A3C, 0x0A3C, 'NSM'). % Mn       GURMUKHI SIGN NUKTA
unicode_bidi_class(0x0A41, 0x0A42, 'NSM'). % Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
unicode_bidi_class(0x0A47, 0x0A48, 'NSM'). % Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
unicode_bidi_class(0x0A4B, 0x0A4D, 'NSM'). % Mn   [3] GURMUKHI VOWEL SIGN OO..GURMUKHI SIGN VIRAMA
unicode_bidi_class(0x0A51, 0x0A51, 'NSM'). % Mn       GURMUKHI SIGN UDAAT
unicode_bidi_class(0x0A70, 0x0A71, 'NSM'). % Mn   [2] GURMUKHI TIPPI..GURMUKHI ADDAK
unicode_bidi_class(0x0A75, 0x0A75, 'NSM'). % Mn       GURMUKHI SIGN YAKASH
unicode_bidi_class(0x0A81, 0x0A82, 'NSM'). % Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
unicode_bidi_class(0x0ABC, 0x0ABC, 'NSM'). % Mn       GUJARATI SIGN NUKTA
unicode_bidi_class(0x0AC1, 0x0AC5, 'NSM'). % Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
unicode_bidi_class(0x0AC7, 0x0AC8, 'NSM'). % Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
unicode_bidi_class(0x0ACD, 0x0ACD, 'NSM'). % Mn       GUJARATI SIGN VIRAMA
unicode_bidi_class(0x0AE2, 0x0AE3, 'NSM'). % Mn   [2] GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL
unicode_bidi_class(0x0B01, 0x0B01, 'NSM'). % Mn       ORIYA SIGN CANDRABINDU
unicode_bidi_class(0x0B3C, 0x0B3C, 'NSM'). % Mn       ORIYA SIGN NUKTA
unicode_bidi_class(0x0B3F, 0x0B3F, 'NSM'). % Mn       ORIYA VOWEL SIGN I
unicode_bidi_class(0x0B41, 0x0B44, 'NSM'). % Mn   [4] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR
unicode_bidi_class(0x0B4D, 0x0B4D, 'NSM'). % Mn       ORIYA SIGN VIRAMA
unicode_bidi_class(0x0B56, 0x0B56, 'NSM'). % Mn       ORIYA AI LENGTH MARK
unicode_bidi_class(0x0B62, 0x0B63, 'NSM'). % Mn   [2] ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL
unicode_bidi_class(0x0B82, 0x0B82, 'NSM'). % Mn       TAMIL SIGN ANUSVARA
unicode_bidi_class(0x0BC0, 0x0BC0, 'NSM'). % Mn       TAMIL VOWEL SIGN II
unicode_bidi_class(0x0BCD, 0x0BCD, 'NSM'). % Mn       TAMIL SIGN VIRAMA
unicode_bidi_class(0x0C3E, 0x0C40, 'NSM'). % Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
unicode_bidi_class(0x0C46, 0x0C48, 'NSM'). % Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
unicode_bidi_class(0x0C4A, 0x0C4D, 'NSM'). % Mn   [4] TELUGU VOWEL SIGN O..TELUGU SIGN VIRAMA
unicode_bidi_class(0x0C55, 0x0C56, 'NSM'). % Mn   [2] TELUGU LENGTH MARK..TELUGU AI LENGTH MARK
unicode_bidi_class(0x0C62, 0x0C63, 'NSM'). % Mn   [2] TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL
unicode_bidi_class(0x0CBC, 0x0CBC, 'NSM'). % Mn       KANNADA SIGN NUKTA
unicode_bidi_class(0x0CCC, 0x0CCD, 'NSM'). % Mn   [2] KANNADA VOWEL SIGN AU..KANNADA SIGN VIRAMA
unicode_bidi_class(0x0CE2, 0x0CE3, 'NSM'). % Mn   [2] KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL
unicode_bidi_class(0x0D41, 0x0D44, 'NSM'). % Mn   [4] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC RR
unicode_bidi_class(0x0D4D, 0x0D4D, 'NSM'). % Mn       MALAYALAM SIGN VIRAMA
unicode_bidi_class(0x0D62, 0x0D63, 'NSM'). % Mn   [2] MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL
unicode_bidi_class(0x0DCA, 0x0DCA, 'NSM'). % Mn       SINHALA SIGN AL-LAKUNA
unicode_bidi_class(0x0DD2, 0x0DD4, 'NSM'). % Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
unicode_bidi_class(0x0DD6, 0x0DD6, 'NSM'). % Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
unicode_bidi_class(0x0E31, 0x0E31, 'NSM'). % Mn       THAI CHARACTER MAI HAN-AKAT
unicode_bidi_class(0x0E34, 0x0E3A, 'NSM'). % Mn   [7] THAI CHARACTER SARA I..THAI CHARACTER PHINTHU
unicode_bidi_class(0x0E47, 0x0E4E, 'NSM'). % Mn   [8] THAI CHARACTER MAITAIKHU..THAI CHARACTER YAMAKKAN
unicode_bidi_class(0x0EB1, 0x0EB1, 'NSM'). % Mn       LAO VOWEL SIGN MAI KAN
unicode_bidi_class(0x0EB4, 0x0EB9, 'NSM'). % Mn   [6] LAO VOWEL SIGN I..LAO VOWEL SIGN UU
unicode_bidi_class(0x0EBB, 0x0EBC, 'NSM'). % Mn   [2] LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO
unicode_bidi_class(0x0EC8, 0x0ECD, 'NSM'). % Mn   [6] LAO TONE MAI EK..LAO NIGGAHITA
unicode_bidi_class(0x0F18, 0x0F19, 'NSM'). % Mn   [2] TIBETAN ASTROLOGICAL SIGN -KHYUD PA..TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS
unicode_bidi_class(0x0F35, 0x0F35, 'NSM'). % Mn       TIBETAN MARK NGAS BZUNG NYI ZLA
unicode_bidi_class(0x0F37, 0x0F37, 'NSM'). % Mn       TIBETAN MARK NGAS BZUNG SGOR RTAGS
unicode_bidi_class(0x0F39, 0x0F39, 'NSM'). % Mn       TIBETAN MARK TSA -PHRU
unicode_bidi_class(0x0F71, 0x0F7E, 'NSM'). % Mn  [14] TIBETAN VOWEL SIGN AA..TIBETAN SIGN RJES SU NGA RO
unicode_bidi_class(0x0F80, 0x0F84, 'NSM'). % Mn   [5] TIBETAN VOWEL SIGN REVERSED I..TIBETAN MARK HALANTA
unicode_bidi_class(0x0F86, 0x0F87, 'NSM'). % Mn   [2] TIBETAN SIGN LCI RTAGS..TIBETAN SIGN YANG RTAGS
unicode_bidi_class(0x0F8D, 0x0F97, 'NSM'). % Mn  [11] TIBETAN SUBJOINED SIGN LCE TSA CAN..TIBETAN SUBJOINED LETTER JA
unicode_bidi_class(0x0F99, 0x0FBC, 'NSM'). % Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
unicode_bidi_class(0x0FC6, 0x0FC6, 'NSM'). % Mn       TIBETAN SYMBOL PADMA GDAN
unicode_bidi_class(0x102D, 0x1030, 'NSM'). % Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
unicode_bidi_class(0x1032, 0x1037, 'NSM'). % Mn   [6] MYANMAR VOWEL SIGN AI..MYANMAR SIGN DOT BELOW
unicode_bidi_class(0x1039, 0x103A, 'NSM'). % Mn   [2] MYANMAR SIGN VIRAMA..MYANMAR SIGN ASAT
unicode_bidi_class(0x103D, 0x103E, 'NSM'). % Mn   [2] MYANMAR CONSONANT SIGN MEDIAL WA..MYANMAR CONSONANT SIGN MEDIAL HA
unicode_bidi_class(0x1058, 0x1059, 'NSM'). % Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
unicode_bidi_class(0x105E, 0x1060, 'NSM'). % Mn   [3] MYANMAR CONSONANT SIGN MON MEDIAL NA..MYANMAR CONSONANT SIGN MON MEDIAL LA
unicode_bidi_class(0x1071, 0x1074, 'NSM'). % Mn   [4] MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE
unicode_bidi_class(0x1082, 0x1082, 'NSM'). % Mn       MYANMAR CONSONANT SIGN SHAN MEDIAL WA
unicode_bidi_class(0x1085, 0x1086, 'NSM'). % Mn   [2] MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y
unicode_bidi_class(0x108D, 0x108D, 'NSM'). % Mn       MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE
unicode_bidi_class(0x109D, 0x109D, 'NSM'). % Mn       MYANMAR VOWEL SIGN AITON AI
unicode_bidi_class(0x135D, 0x135F, 'NSM'). % Mn   [3] ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK..ETHIOPIC COMBINING GEMINATION MARK
unicode_bidi_class(0x1712, 0x1714, 'NSM'). % Mn   [3] TAGALOG VOWEL SIGN I..TAGALOG SIGN VIRAMA
unicode_bidi_class(0x1732, 0x1734, 'NSM'). % Mn   [3] HANUNOO VOWEL SIGN I..HANUNOO SIGN PAMUDPOD
unicode_bidi_class(0x1752, 0x1753, 'NSM'). % Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
unicode_bidi_class(0x1772, 0x1773, 'NSM'). % Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
unicode_bidi_class(0x17B4, 0x17B5, 'NSM'). % Mn   [2] KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA
unicode_bidi_class(0x17B7, 0x17BD, 'NSM'). % Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
unicode_bidi_class(0x17C6, 0x17C6, 'NSM'). % Mn       KHMER SIGN NIKAHIT
unicode_bidi_class(0x17C9, 0x17D3, 'NSM'). % Mn  [11] KHMER SIGN MUUSIKATOAN..KHMER SIGN BATHAMASAT
unicode_bidi_class(0x17DD, 0x17DD, 'NSM'). % Mn       KHMER SIGN ATTHACAN
unicode_bidi_class(0x180B, 0x180D, 'NSM'). % Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
unicode_bidi_class(0x18A9, 0x18A9, 'NSM'). % Mn       MONGOLIAN LETTER ALI GALI DAGALGA
unicode_bidi_class(0x1920, 0x1922, 'NSM'). % Mn   [3] LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN U
unicode_bidi_class(0x1927, 0x1928, 'NSM'). % Mn   [2] LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O
unicode_bidi_class(0x1932, 0x1932, 'NSM'). % Mn       LIMBU SMALL LETTER ANUSVARA
unicode_bidi_class(0x1939, 0x193B, 'NSM'). % Mn   [3] LIMBU SIGN MUKPHRENG..LIMBU SIGN SA-I
unicode_bidi_class(0x1A17, 0x1A18, 'NSM'). % Mn   [2] BUGINESE VOWEL SIGN I..BUGINESE VOWEL SIGN U
unicode_bidi_class(0x1A56, 0x1A56, 'NSM'). % Mn       TAI THAM CONSONANT SIGN MEDIAL LA
unicode_bidi_class(0x1A58, 0x1A5E, 'NSM'). % Mn   [7] TAI THAM SIGN MAI KANG LAI..TAI THAM CONSONANT SIGN SA
unicode_bidi_class(0x1A60, 0x1A60, 'NSM'). % Mn       TAI THAM SIGN SAKOT
unicode_bidi_class(0x1A62, 0x1A62, 'NSM'). % Mn       TAI THAM VOWEL SIGN MAI SAT
unicode_bidi_class(0x1A65, 0x1A6C, 'NSM'). % Mn   [8] TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN OA BELOW
unicode_bidi_class(0x1A73, 0x1A7C, 'NSM'). % Mn  [10] TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN KHUEN-LUE KARAN
unicode_bidi_class(0x1A7F, 0x1A7F, 'NSM'). % Mn       TAI THAM COMBINING CRYPTOGRAMMIC DOT
unicode_bidi_class(0x1B00, 0x1B03, 'NSM'). % Mn   [4] BALINESE SIGN ULU RICEM..BALINESE SIGN SURANG
unicode_bidi_class(0x1B34, 0x1B34, 'NSM'). % Mn       BALINESE SIGN REREKAN
unicode_bidi_class(0x1B36, 0x1B3A, 'NSM'). % Mn   [5] BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN RA REPA
unicode_bidi_class(0x1B3C, 0x1B3C, 'NSM'). % Mn       BALINESE VOWEL SIGN LA LENGA
unicode_bidi_class(0x1B42, 0x1B42, 'NSM'). % Mn       BALINESE VOWEL SIGN PEPET
unicode_bidi_class(0x1B6B, 0x1B73, 'NSM'). % Mn   [9] BALINESE MUSICAL SYMBOL COMBINING TEGEH..BALINESE MUSICAL SYMBOL COMBINING GONG
unicode_bidi_class(0x1B80, 0x1B81, 'NSM'). % Mn   [2] SUNDANESE SIGN PANYECEK..SUNDANESE SIGN PANGLAYAR
unicode_bidi_class(0x1BA2, 0x1BA5, 'NSM'). % Mn   [4] SUNDANESE CONSONANT SIGN PANYAKRA..SUNDANESE VOWEL SIGN PANYUKU
unicode_bidi_class(0x1BA8, 0x1BA9, 'NSM'). % Mn   [2] SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG
unicode_bidi_class(0x1BAB, 0x1BAB, 'NSM'). % Mn       SUNDANESE SIGN VIRAMA
unicode_bidi_class(0x1BE6, 0x1BE6, 'NSM'). % Mn       BATAK SIGN TOMPI
unicode_bidi_class(0x1BE8, 0x1BE9, 'NSM'). % Mn   [2] BATAK VOWEL SIGN PAKPAK E..BATAK VOWEL SIGN EE
unicode_bidi_class(0x1BED, 0x1BED, 'NSM'). % Mn       BATAK VOWEL SIGN KARO O
unicode_bidi_class(0x1BEF, 0x1BF1, 'NSM'). % Mn   [3] BATAK VOWEL SIGN U FOR SIMALUNGUN SA..BATAK CONSONANT SIGN H
unicode_bidi_class(0x1C2C, 0x1C33, 'NSM'). % Mn   [8] LEPCHA VOWEL SIGN E..LEPCHA CONSONANT SIGN T
unicode_bidi_class(0x1C36, 0x1C37, 'NSM'). % Mn   [2] LEPCHA SIGN RAN..LEPCHA SIGN NUKTA
unicode_bidi_class(0x1CD0, 0x1CD2, 'NSM'). % Mn   [3] VEDIC TONE KARSHANA..VEDIC TONE PRENKHA
unicode_bidi_class(0x1CD4, 0x1CE0, 'NSM'). % Mn  [13] VEDIC SIGN YAJURVEDIC MIDLINE SVARITA..VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA
unicode_bidi_class(0x1CE2, 0x1CE8, 'NSM'). % Mn   [7] VEDIC SIGN VISARGA SVARITA..VEDIC SIGN VISARGA ANUDATTA WITH TAIL
unicode_bidi_class(0x1CED, 0x1CED, 'NSM'). % Mn       VEDIC SIGN TIRYAK
unicode_bidi_class(0x1CF4, 0x1CF4, 'NSM'). % Mn       VEDIC TONE CANDRA ABOVE
unicode_bidi_class(0x1DC0, 0x1DE6, 'NSM'). % Mn  [39] COMBINING DOTTED GRAVE ACCENT..COMBINING LATIN SMALL LETTER Z
unicode_bidi_class(0x1DFC, 0x1DFF, 'NSM'). % Mn   [4] COMBINING DOUBLE INVERTED BREVE BELOW..COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW
unicode_bidi_class(0x20D0, 0x20DC, 'NSM'). % Mn  [13] COMBINING LEFT HARPOON ABOVE..COMBINING FOUR DOTS ABOVE
unicode_bidi_class(0x20DD, 0x20E0, 'NSM'). % Me   [4] COMBINING ENCLOSING CIRCLE..COMBINING ENCLOSING CIRCLE BACKSLASH
unicode_bidi_class(0x20E1, 0x20E1, 'NSM'). % Mn       COMBINING LEFT RIGHT ARROW ABOVE
unicode_bidi_class(0x20E2, 0x20E4, 'NSM'). % Me   [3] COMBINING ENCLOSING SCREEN..COMBINING ENCLOSING UPWARD POINTING TRIANGLE
unicode_bidi_class(0x20E5, 0x20F0, 'NSM'). % Mn  [12] COMBINING REVERSE SOLIDUS OVERLAY..COMBINING ASTERISK ABOVE
unicode_bidi_class(0x2CEF, 0x2CF1, 'NSM'). % Mn   [3] COPTIC COMBINING NI ABOVE..COPTIC COMBINING SPIRITUS LENIS
unicode_bidi_class(0x2D7F, 0x2D7F, 'NSM'). % Mn       TIFINAGH CONSONANT JOINER
unicode_bidi_class(0x2DE0, 0x2DFF, 'NSM'). % Mn  [32] COMBINING CYRILLIC LETTER BE..COMBINING CYRILLIC LETTER IOTIFIED BIG YUS
unicode_bidi_class(0x302A, 0x302D, 'NSM'). % Mn   [4] IDEOGRAPHIC LEVEL TONE MARK..IDEOGRAPHIC ENTERING TONE MARK
unicode_bidi_class(0x3099, 0x309A, 'NSM'). % Mn   [2] COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK..COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_bidi_class(0xA66F, 0xA66F, 'NSM'). % Mn       COMBINING CYRILLIC VZMET
unicode_bidi_class(0xA670, 0xA672, 'NSM'). % Me   [3] COMBINING CYRILLIC TEN MILLIONS SIGN..COMBINING CYRILLIC THOUSAND MILLIONS SIGN
unicode_bidi_class(0xA674, 0xA67D, 'NSM'). % Mn  [10] COMBINING CYRILLIC LETTER UKRAINIAN IE..COMBINING CYRILLIC PAYEROK
unicode_bidi_class(0xA69F, 0xA69F, 'NSM'). % Mn       COMBINING CYRILLIC LETTER IOTIFIED E
unicode_bidi_class(0xA6F0, 0xA6F1, 'NSM'). % Mn   [2] BAMUM COMBINING MARK KOQNDON..BAMUM COMBINING MARK TUKWENTIS
unicode_bidi_class(0xA802, 0xA802, 'NSM'). % Mn       SYLOTI NAGRI SIGN DVISVARA
unicode_bidi_class(0xA806, 0xA806, 'NSM'). % Mn       SYLOTI NAGRI SIGN HASANTA
unicode_bidi_class(0xA80B, 0xA80B, 'NSM'). % Mn       SYLOTI NAGRI SIGN ANUSVARA
unicode_bidi_class(0xA825, 0xA826, 'NSM'). % Mn   [2] SYLOTI NAGRI VOWEL SIGN U..SYLOTI NAGRI VOWEL SIGN E
unicode_bidi_class(0xA8C4, 0xA8C4, 'NSM'). % Mn       SAURASHTRA SIGN VIRAMA
unicode_bidi_class(0xA8E0, 0xA8F1, 'NSM'). % Mn  [18] COMBINING DEVANAGARI DIGIT ZERO..COMBINING DEVANAGARI SIGN AVAGRAHA
unicode_bidi_class(0xA926, 0xA92D, 'NSM'). % Mn   [8] KAYAH LI VOWEL UE..KAYAH LI TONE CALYA PLOPHU
unicode_bidi_class(0xA947, 0xA951, 'NSM'). % Mn  [11] REJANG VOWEL SIGN I..REJANG CONSONANT SIGN R
unicode_bidi_class(0xA980, 0xA982, 'NSM'). % Mn   [3] JAVANESE SIGN PANYANGGA..JAVANESE SIGN LAYAR
unicode_bidi_class(0xA9B3, 0xA9B3, 'NSM'). % Mn       JAVANESE SIGN CECAK TELU
unicode_bidi_class(0xA9B6, 0xA9B9, 'NSM'). % Mn   [4] JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN SUKU MENDUT
unicode_bidi_class(0xA9BC, 0xA9BC, 'NSM'). % Mn       JAVANESE VOWEL SIGN PEPET
unicode_bidi_class(0xAA29, 0xAA2E, 'NSM'). % Mn   [6] CHAM VOWEL SIGN AA..CHAM VOWEL SIGN OE
unicode_bidi_class(0xAA31, 0xAA32, 'NSM'). % Mn   [2] CHAM VOWEL SIGN AU..CHAM VOWEL SIGN UE
unicode_bidi_class(0xAA35, 0xAA36, 'NSM'). % Mn   [2] CHAM CONSONANT SIGN LA..CHAM CONSONANT SIGN WA
unicode_bidi_class(0xAA43, 0xAA43, 'NSM'). % Mn       CHAM CONSONANT SIGN FINAL NG
unicode_bidi_class(0xAA4C, 0xAA4C, 'NSM'). % Mn       CHAM CONSONANT SIGN FINAL M
unicode_bidi_class(0xAAB0, 0xAAB0, 'NSM'). % Mn       TAI VIET MAI KANG
unicode_bidi_class(0xAAB2, 0xAAB4, 'NSM'). % Mn   [3] TAI VIET VOWEL I..TAI VIET VOWEL U
unicode_bidi_class(0xAAB7, 0xAAB8, 'NSM'). % Mn   [2] TAI VIET MAI KHIT..TAI VIET VOWEL IA
unicode_bidi_class(0xAABE, 0xAABF, 'NSM'). % Mn   [2] TAI VIET VOWEL AM..TAI VIET TONE MAI EK
unicode_bidi_class(0xAAC1, 0xAAC1, 'NSM'). % Mn       TAI VIET TONE MAI THO
unicode_bidi_class(0xAAEC, 0xAAED, 'NSM'). % Mn   [2] MEETEI MAYEK VOWEL SIGN UU..MEETEI MAYEK VOWEL SIGN AAI
unicode_bidi_class(0xAAF6, 0xAAF6, 'NSM'). % Mn       MEETEI MAYEK VIRAMA
unicode_bidi_class(0xABE5, 0xABE5, 'NSM'). % Mn       MEETEI MAYEK VOWEL SIGN ANAP
unicode_bidi_class(0xABE8, 0xABE8, 'NSM'). % Mn       MEETEI MAYEK VOWEL SIGN UNAP
unicode_bidi_class(0xABED, 0xABED, 'NSM'). % Mn       MEETEI MAYEK APUN IYEK
unicode_bidi_class(0xFB1E, 0xFB1E, 'NSM'). % Mn       HEBREW POINT JUDEO-SPANISH VARIKA
unicode_bidi_class(0xFE00, 0xFE0F, 'NSM'). % Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
unicode_bidi_class(0xFE20, 0xFE26, 'NSM'). % Mn   [7] COMBINING LIGATURE LEFT HALF..COMBINING CONJOINING MACRON
unicode_bidi_class(0x101FD, 0x101FD, 'NSM'). % Mn       PHAISTOS DISC SIGN COMBINING OBLIQUE STROKE
unicode_bidi_class(0x10A01, 0x10A03, 'NSM'). % Mn   [3] KHAROSHTHI VOWEL SIGN I..KHAROSHTHI VOWEL SIGN VOCALIC R
unicode_bidi_class(0x10A05, 0x10A06, 'NSM'). % Mn   [2] KHAROSHTHI VOWEL SIGN E..KHAROSHTHI VOWEL SIGN O
unicode_bidi_class(0x10A0C, 0x10A0F, 'NSM'). % Mn   [4] KHAROSHTHI VOWEL LENGTH MARK..KHAROSHTHI SIGN VISARGA
unicode_bidi_class(0x10A38, 0x10A3A, 'NSM'). % Mn   [3] KHAROSHTHI SIGN BAR ABOVE..KHAROSHTHI SIGN DOT BELOW
unicode_bidi_class(0x10A3F, 0x10A3F, 'NSM'). % Mn       KHAROSHTHI VIRAMA
unicode_bidi_class(0x11001, 0x11001, 'NSM'). % Mn       BRAHMI SIGN ANUSVARA
unicode_bidi_class(0x11038, 0x11046, 'NSM'). % Mn  [15] BRAHMI VOWEL SIGN AA..BRAHMI VIRAMA
unicode_bidi_class(0x11080, 0x11081, 'NSM'). % Mn   [2] KAITHI SIGN CANDRABINDU..KAITHI SIGN ANUSVARA
unicode_bidi_class(0x110B3, 0x110B6, 'NSM'). % Mn   [4] KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN AI
unicode_bidi_class(0x110B9, 0x110BA, 'NSM'). % Mn   [2] KAITHI SIGN VIRAMA..KAITHI SIGN NUKTA
unicode_bidi_class(0x11100, 0x11102, 'NSM'). % Mn   [3] CHAKMA SIGN CANDRABINDU..CHAKMA SIGN VISARGA
unicode_bidi_class(0x11127, 0x1112B, 'NSM'). % Mn   [5] CHAKMA VOWEL SIGN A..CHAKMA VOWEL SIGN UU
unicode_bidi_class(0x1112D, 0x11134, 'NSM'). % Mn   [8] CHAKMA VOWEL SIGN AI..CHAKMA MAAYYAA
unicode_bidi_class(0x11180, 0x11181, 'NSM'). % Mn   [2] SHARADA SIGN CANDRABINDU..SHARADA SIGN ANUSVARA
unicode_bidi_class(0x111B6, 0x111BE, 'NSM'). % Mn   [9] SHARADA VOWEL SIGN U..SHARADA VOWEL SIGN O
unicode_bidi_class(0x116AB, 0x116AB, 'NSM'). % Mn       TAKRI SIGN ANUSVARA
unicode_bidi_class(0x116AD, 0x116AD, 'NSM'). % Mn       TAKRI VOWEL SIGN AA
unicode_bidi_class(0x116B0, 0x116B5, 'NSM'). % Mn   [6] TAKRI VOWEL SIGN U..TAKRI VOWEL SIGN AU
unicode_bidi_class(0x116B7, 0x116B7, 'NSM'). % Mn       TAKRI SIGN NUKTA
unicode_bidi_class(0x16F8F, 0x16F92, 'NSM'). % Mn   [4] MIAO TONE RIGHT..MIAO TONE BELOW
unicode_bidi_class(0x1D167, 0x1D169, 'NSM'). % Mn   [3] MUSICAL SYMBOL COMBINING TREMOLO-1..MUSICAL SYMBOL COMBINING TREMOLO-3
unicode_bidi_class(0x1D17B, 0x1D182, 'NSM'). % Mn   [8] MUSICAL SYMBOL COMBINING ACCENT..MUSICAL SYMBOL COMBINING LOURE
unicode_bidi_class(0x1D185, 0x1D18B, 'NSM'). % Mn   [7] MUSICAL SYMBOL COMBINING DOIT..MUSICAL SYMBOL COMBINING TRIPLE TONGUE
unicode_bidi_class(0x1D1AA, 0x1D1AD, 'NSM'). % Mn   [4] MUSICAL SYMBOL COMBINING DOWN BOW..MUSICAL SYMBOL COMBINING SNAP PIZZICATO
unicode_bidi_class(0x1D242, 0x1D244, 'NSM'). % Mn   [3] COMBINING GREEK MUSICAL TRISEME..COMBINING GREEK MUSICAL PENTASEME
unicode_bidi_class(0xE0100, 0xE01EF, 'NSM'). % Mn [240] VARIATION SELECTOR-17..VARIATION SELECTOR-256

% Total code points: 1290

% ================================================

% Bidi_Class=Arabic_Letter

unicode_bidi_class(0x0605, 0x0605, 'AL'). % Cn       <reserved-0605>
unicode_bidi_class(0x0608, 0x0608, 'AL'). % Sm       ARABIC RAY
unicode_bidi_class(0x060B, 0x060B, 'AL'). % Sc       AFGHANI SIGN
unicode_bidi_class(0x060D, 0x060D, 'AL'). % Po       ARABIC DATE SEPARATOR
unicode_bidi_class(0x061B, 0x061B, 'AL'). % Po       ARABIC SEMICOLON
unicode_bidi_class(0x061C, 0x061D, 'AL'). % Cn   [2] <reserved-061C>..<reserved-061D>
unicode_bidi_class(0x061E, 0x061F, 'AL'). % Po   [2] ARABIC TRIPLE DOT PUNCTUATION MARK..ARABIC QUESTION MARK
unicode_bidi_class(0x0620, 0x063F, 'AL'). % Lo  [32] ARABIC LETTER KASHMIRI YEH..ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE
unicode_bidi_class(0x0640, 0x0640, 'AL'). % Lm       ARABIC TATWEEL
unicode_bidi_class(0x0641, 0x064A, 'AL'). % Lo  [10] ARABIC LETTER FEH..ARABIC LETTER YEH
unicode_bidi_class(0x066D, 0x066D, 'AL'). % Po       ARABIC FIVE POINTED STAR
unicode_bidi_class(0x066E, 0x066F, 'AL'). % Lo   [2] ARABIC LETTER DOTLESS BEH..ARABIC LETTER DOTLESS QAF
unicode_bidi_class(0x0671, 0x06D3, 'AL'). % Lo  [99] ARABIC LETTER ALEF WASLA..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
unicode_bidi_class(0x06D4, 0x06D4, 'AL'). % Po       ARABIC FULL STOP
unicode_bidi_class(0x06D5, 0x06D5, 'AL'). % Lo       ARABIC LETTER AE
unicode_bidi_class(0x06E5, 0x06E6, 'AL'). % Lm   [2] ARABIC SMALL WAW..ARABIC SMALL YEH
unicode_bidi_class(0x06EE, 0x06EF, 'AL'). % Lo   [2] ARABIC LETTER DAL WITH INVERTED V..ARABIC LETTER REH WITH INVERTED V
unicode_bidi_class(0x06FA, 0x06FC, 'AL'). % Lo   [3] ARABIC LETTER SHEEN WITH DOT BELOW..ARABIC LETTER GHAIN WITH DOT BELOW
unicode_bidi_class(0x06FD, 0x06FE, 'AL'). % So   [2] ARABIC SIGN SINDHI AMPERSAND..ARABIC SIGN SINDHI POSTPOSITION MEN
unicode_bidi_class(0x06FF, 0x06FF, 'AL'). % Lo       ARABIC LETTER HEH WITH INVERTED V
unicode_bidi_class(0x0700, 0x070D, 'AL'). % Po  [14] SYRIAC END OF PARAGRAPH..SYRIAC HARKLEAN ASTERISCUS
unicode_bidi_class(0x070E, 0x070E, 'AL'). % Cn       <reserved-070E>
unicode_bidi_class(0x070F, 0x070F, 'AL'). % Cf       SYRIAC ABBREVIATION MARK
unicode_bidi_class(0x0710, 0x0710, 'AL'). % Lo       SYRIAC LETTER ALAPH
unicode_bidi_class(0x0712, 0x072F, 'AL'). % Lo  [30] SYRIAC LETTER BETH..SYRIAC LETTER PERSIAN DHALATH
unicode_bidi_class(0x074B, 0x074C, 'AL'). % Cn   [2] <reserved-074B>..<reserved-074C>
unicode_bidi_class(0x074D, 0x07A5, 'AL'). % Lo  [89] SYRIAC LETTER SOGDIAN ZHAIN..THAANA LETTER WAAVU
unicode_bidi_class(0x07B1, 0x07B1, 'AL'). % Lo       THAANA LETTER NAA
unicode_bidi_class(0x07B2, 0x07BF, 'AL'). % Cn  [14] <reserved-07B2>..<reserved-07BF>
unicode_bidi_class(0x08A0, 0x08A0, 'AL'). % Lo       ARABIC LETTER BEH WITH SMALL V BELOW
unicode_bidi_class(0x08A1, 0x08A1, 'AL'). % Cn       <reserved-08A1>
unicode_bidi_class(0x08A2, 0x08AC, 'AL'). % Lo  [11] ARABIC LETTER JEEM WITH TWO DOTS ABOVE..ARABIC LETTER ROHINGYA YEH
unicode_bidi_class(0x08AD, 0x08E3, 'AL'). % Cn  [55] <reserved-08AD>..<reserved-08E3>
unicode_bidi_class(0x08FF, 0x08FF, 'AL'). % Cn       <reserved-08FF>
unicode_bidi_class(0xFB50, 0xFBB1, 'AL'). % Lo  [98] ARABIC LETTER ALEF WASLA ISOLATED FORM..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
unicode_bidi_class(0xFBB2, 0xFBC1, 'AL'). % Sk  [16] ARABIC SYMBOL DOT ABOVE..ARABIC SYMBOL SMALL TAH BELOW
unicode_bidi_class(0xFBC2, 0xFBD2, 'AL'). % Cn  [17] <reserved-FBC2>..<reserved-FBD2>
unicode_bidi_class(0xFBD3, 0xFD3D, 'AL'). % Lo [363] ARABIC LETTER NG ISOLATED FORM..ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
unicode_bidi_class(0xFD40, 0xFD4F, 'AL'). % Cn  [16] <reserved-FD40>..<reserved-FD4F>
unicode_bidi_class(0xFD50, 0xFD8F, 'AL'). % Lo  [64] ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
unicode_bidi_class(0xFD90, 0xFD91, 'AL'). % Cn   [2] <reserved-FD90>..<reserved-FD91>
unicode_bidi_class(0xFD92, 0xFDC7, 'AL'). % Lo  [54] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
unicode_bidi_class(0xFDC8, 0xFDCF, 'AL'). % Cn   [8] <reserved-FDC8>..<reserved-FDCF>
unicode_bidi_class(0xFDF0, 0xFDFB, 'AL'). % Lo  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
unicode_bidi_class(0xFDFC, 0xFDFC, 'AL'). % Sc       RIAL SIGN
unicode_bidi_class(0xFDFE, 0xFDFF, 'AL'). % Cn   [2] <reserved-FDFE>..<reserved-FDFF>
unicode_bidi_class(0xFE70, 0xFE74, 'AL'). % Lo   [5] ARABIC FATHATAN ISOLATED FORM..ARABIC KASRATAN ISOLATED FORM
unicode_bidi_class(0xFE75, 0xFE75, 'AL'). % Cn       <reserved-FE75>
unicode_bidi_class(0xFE76, 0xFEFC, 'AL'). % Lo [135] ARABIC FATHA ISOLATED FORM..ARABIC LIGATURE LAM WITH ALEF FINAL FORM
unicode_bidi_class(0xFEFD, 0xFEFE, 'AL'). % Cn   [2] <reserved-FEFD>..<reserved-FEFE>
unicode_bidi_class(0x1EE00, 0x1EE03, 'AL'). % Lo   [4] ARABIC MATHEMATICAL ALEF..ARABIC MATHEMATICAL DAL
unicode_bidi_class(0x1EE04, 0x1EE04, 'AL'). % Cn       <reserved-1EE04>
unicode_bidi_class(0x1EE05, 0x1EE1F, 'AL'). % Lo  [27] ARABIC MATHEMATICAL WAW..ARABIC MATHEMATICAL DOTLESS QAF
unicode_bidi_class(0x1EE20, 0x1EE20, 'AL'). % Cn       <reserved-1EE20>
unicode_bidi_class(0x1EE21, 0x1EE22, 'AL'). % Lo   [2] ARABIC MATHEMATICAL INITIAL BEH..ARABIC MATHEMATICAL INITIAL JEEM
unicode_bidi_class(0x1EE23, 0x1EE23, 'AL'). % Cn       <reserved-1EE23>
unicode_bidi_class(0x1EE24, 0x1EE24, 'AL'). % Lo       ARABIC MATHEMATICAL INITIAL HEH
unicode_bidi_class(0x1EE25, 0x1EE26, 'AL'). % Cn   [2] <reserved-1EE25>..<reserved-1EE26>
unicode_bidi_class(0x1EE27, 0x1EE27, 'AL'). % Lo       ARABIC MATHEMATICAL INITIAL HAH
unicode_bidi_class(0x1EE28, 0x1EE28, 'AL'). % Cn       <reserved-1EE28>
unicode_bidi_class(0x1EE29, 0x1EE32, 'AL'). % Lo  [10] ARABIC MATHEMATICAL INITIAL YEH..ARABIC MATHEMATICAL INITIAL QAF
unicode_bidi_class(0x1EE33, 0x1EE33, 'AL'). % Cn       <reserved-1EE33>
unicode_bidi_class(0x1EE34, 0x1EE37, 'AL'). % Lo   [4] ARABIC MATHEMATICAL INITIAL SHEEN..ARABIC MATHEMATICAL INITIAL KHAH
unicode_bidi_class(0x1EE38, 0x1EE38, 'AL'). % Cn       <reserved-1EE38>
unicode_bidi_class(0x1EE39, 0x1EE39, 'AL'). % Lo       ARABIC MATHEMATICAL INITIAL DAD
unicode_bidi_class(0x1EE3A, 0x1EE3A, 'AL'). % Cn       <reserved-1EE3A>
unicode_bidi_class(0x1EE3B, 0x1EE3B, 'AL'). % Lo       ARABIC MATHEMATICAL INITIAL GHAIN
unicode_bidi_class(0x1EE3C, 0x1EE41, 'AL'). % Cn   [6] <reserved-1EE3C>..<reserved-1EE41>
unicode_bidi_class(0x1EE42, 0x1EE42, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED JEEM
unicode_bidi_class(0x1EE43, 0x1EE46, 'AL'). % Cn   [4] <reserved-1EE43>..<reserved-1EE46>
unicode_bidi_class(0x1EE47, 0x1EE47, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED HAH
unicode_bidi_class(0x1EE48, 0x1EE48, 'AL'). % Cn       <reserved-1EE48>
unicode_bidi_class(0x1EE49, 0x1EE49, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED YEH
unicode_bidi_class(0x1EE4A, 0x1EE4A, 'AL'). % Cn       <reserved-1EE4A>
unicode_bidi_class(0x1EE4B, 0x1EE4B, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED LAM
unicode_bidi_class(0x1EE4C, 0x1EE4C, 'AL'). % Cn       <reserved-1EE4C>
unicode_bidi_class(0x1EE4D, 0x1EE4F, 'AL'). % Lo   [3] ARABIC MATHEMATICAL TAILED NOON..ARABIC MATHEMATICAL TAILED AIN
unicode_bidi_class(0x1EE50, 0x1EE50, 'AL'). % Cn       <reserved-1EE50>
unicode_bidi_class(0x1EE51, 0x1EE52, 'AL'). % Lo   [2] ARABIC MATHEMATICAL TAILED SAD..ARABIC MATHEMATICAL TAILED QAF
unicode_bidi_class(0x1EE53, 0x1EE53, 'AL'). % Cn       <reserved-1EE53>
unicode_bidi_class(0x1EE54, 0x1EE54, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED SHEEN
unicode_bidi_class(0x1EE55, 0x1EE56, 'AL'). % Cn   [2] <reserved-1EE55>..<reserved-1EE56>
unicode_bidi_class(0x1EE57, 0x1EE57, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED KHAH
unicode_bidi_class(0x1EE58, 0x1EE58, 'AL'). % Cn       <reserved-1EE58>
unicode_bidi_class(0x1EE59, 0x1EE59, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED DAD
unicode_bidi_class(0x1EE5A, 0x1EE5A, 'AL'). % Cn       <reserved-1EE5A>
unicode_bidi_class(0x1EE5B, 0x1EE5B, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED GHAIN
unicode_bidi_class(0x1EE5C, 0x1EE5C, 'AL'). % Cn       <reserved-1EE5C>
unicode_bidi_class(0x1EE5D, 0x1EE5D, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED DOTLESS NOON
unicode_bidi_class(0x1EE5E, 0x1EE5E, 'AL'). % Cn       <reserved-1EE5E>
unicode_bidi_class(0x1EE5F, 0x1EE5F, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED DOTLESS QAF
unicode_bidi_class(0x1EE60, 0x1EE60, 'AL'). % Cn       <reserved-1EE60>
unicode_bidi_class(0x1EE61, 0x1EE62, 'AL'). % Lo   [2] ARABIC MATHEMATICAL STRETCHED BEH..ARABIC MATHEMATICAL STRETCHED JEEM
unicode_bidi_class(0x1EE63, 0x1EE63, 'AL'). % Cn       <reserved-1EE63>
unicode_bidi_class(0x1EE64, 0x1EE64, 'AL'). % Lo       ARABIC MATHEMATICAL STRETCHED HEH
unicode_bidi_class(0x1EE65, 0x1EE66, 'AL'). % Cn   [2] <reserved-1EE65>..<reserved-1EE66>
unicode_bidi_class(0x1EE67, 0x1EE6A, 'AL'). % Lo   [4] ARABIC MATHEMATICAL STRETCHED HAH..ARABIC MATHEMATICAL STRETCHED KAF
unicode_bidi_class(0x1EE6B, 0x1EE6B, 'AL'). % Cn       <reserved-1EE6B>
unicode_bidi_class(0x1EE6C, 0x1EE72, 'AL'). % Lo   [7] ARABIC MATHEMATICAL STRETCHED MEEM..ARABIC MATHEMATICAL STRETCHED QAF
unicode_bidi_class(0x1EE73, 0x1EE73, 'AL'). % Cn       <reserved-1EE73>
unicode_bidi_class(0x1EE74, 0x1EE77, 'AL'). % Lo   [4] ARABIC MATHEMATICAL STRETCHED SHEEN..ARABIC MATHEMATICAL STRETCHED KHAH
unicode_bidi_class(0x1EE78, 0x1EE78, 'AL'). % Cn       <reserved-1EE78>
unicode_bidi_class(0x1EE79, 0x1EE7C, 'AL'). % Lo   [4] ARABIC MATHEMATICAL STRETCHED DAD..ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
unicode_bidi_class(0x1EE7D, 0x1EE7D, 'AL'). % Cn       <reserved-1EE7D>
unicode_bidi_class(0x1EE7E, 0x1EE7E, 'AL'). % Lo       ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
unicode_bidi_class(0x1EE7F, 0x1EE7F, 'AL'). % Cn       <reserved-1EE7F>
unicode_bidi_class(0x1EE80, 0x1EE89, 'AL'). % Lo  [10] ARABIC MATHEMATICAL LOOPED ALEF..ARABIC MATHEMATICAL LOOPED YEH
unicode_bidi_class(0x1EE8A, 0x1EE8A, 'AL'). % Cn       <reserved-1EE8A>
unicode_bidi_class(0x1EE8B, 0x1EE9B, 'AL'). % Lo  [17] ARABIC MATHEMATICAL LOOPED LAM..ARABIC MATHEMATICAL LOOPED GHAIN
unicode_bidi_class(0x1EE9C, 0x1EEA0, 'AL'). % Cn   [5] <reserved-1EE9C>..<reserved-1EEA0>
unicode_bidi_class(0x1EEA1, 0x1EEA3, 'AL'). % Lo   [3] ARABIC MATHEMATICAL DOUBLE-STRUCK BEH..ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
unicode_bidi_class(0x1EEA4, 0x1EEA4, 'AL'). % Cn       <reserved-1EEA4>
unicode_bidi_class(0x1EEA5, 0x1EEA9, 'AL'). % Lo   [5] ARABIC MATHEMATICAL DOUBLE-STRUCK WAW..ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
unicode_bidi_class(0x1EEAA, 0x1EEAA, 'AL'). % Cn       <reserved-1EEAA>
unicode_bidi_class(0x1EEAB, 0x1EEBB, 'AL'). % Lo  [17] ARABIC MATHEMATICAL DOUBLE-STRUCK LAM..ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN
unicode_bidi_class(0x1EEBC, 0x1EEEF, 'AL'). % Cn  [52] <reserved-1EEBC>..<reserved-1EEEF>
unicode_bidi_class(0x1EEF2, 0x1EEFF, 'AL'). % Cn  [14] <reserved-1EEF2>..<reserved-1EEFF>

% Total code points: 1438

% ================================================

% Bidi_Class=Left_To_Right_Override

unicode_bidi_class(0x202D, 0x202D, 'LRO'). % Cf       LEFT-TO-RIGHT OVERRIDE

% Total code points: 1

% ================================================

% Bidi_Class=Right_To_Left_Override

unicode_bidi_class(0x202E, 0x202E, 'RLO'). % Cf       RIGHT-TO-LEFT OVERRIDE

% Total code points: 1

% ================================================

% Bidi_Class=Left_To_Right_Embedding

unicode_bidi_class(0x202A, 0x202A, 'LRE'). % Cf       LEFT-TO-RIGHT EMBEDDING

% Total code points: 1

% ================================================

% Bidi_Class=Right_To_Left_Embedding

unicode_bidi_class(0x202B, 0x202B, 'RLE'). % Cf       RIGHT-TO-LEFT EMBEDDING

% Total code points: 1

% ================================================

% Bidi_Class=Pop_Directional_Format

unicode_bidi_class(0x202C, 0x202C, 'PDF'). % Cf       POP DIRECTIONAL FORMATTING

% Total code points: 1

% EOF
