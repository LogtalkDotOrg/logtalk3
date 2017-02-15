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
# DerivedCombiningClass-6.2.0.txt
# Date: 2012-08-13, 19:56:56 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2012 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

# ================================================

# Combining Class (listing UnicodeData.txt, field 3: see UAX #44: http://www.unicode.org/reports/tr44/)

#  All code points not explicitly listed for Canonical_Combining_Class
#  have the value Not_Reordered (0).

# @missing: 0000..10FFFF; Not_Reordered

# ================================================
*/

unicode_combining_class(CodePoint, Class) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_combining_class(CodePointStart, CodePointEnd, Class),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_combining_class(CodePoint, _, CodePointClass) ->
		Class = CodePointClass
	;	% look for a code point range that includes the given code point
		unicode_combining_class(CodePointStart, CodePointEnd, CodePointClass),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Class = CodePointClass
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Class = 0
	).

% Canonical_Combining_Class=Not_Reordered

unicode_combining_class(0x0000, 0x001F, 0).	% Cc  [32] <control-0000>..<control-001F>
unicode_combining_class(0x0020, 0x0020, 0).	% Zs       SPACE
unicode_combining_class(0x0021, 0x0023, 0).	% Po   [3] EXCLAMATION MARK..NUMBER SIGN
unicode_combining_class(0x0024, 0x0024, 0).	% Sc       DOLLAR SIGN
unicode_combining_class(0x0025, 0x0027, 0).	% Po   [3] PERCENT SIGN..APOSTROPHE
unicode_combining_class(0x0028, 0x0028, 0).	% Ps       LEFT PARENTHESIS
unicode_combining_class(0x0029, 0x0029, 0).	% Pe       RIGHT PARENTHESIS
unicode_combining_class(0x002A, 0x002A, 0).	% Po       ASTERISK
unicode_combining_class(0x002B, 0x002B, 0).	% Sm       PLUS SIGN
unicode_combining_class(0x002C, 0x002C, 0).	% Po       COMMA
unicode_combining_class(0x002D, 0x002D, 0).	% Pd       HYPHEN-MINUS
unicode_combining_class(0x002E, 0x002F, 0).	% Po   [2] FULL STOP..SOLIDUS
unicode_combining_class(0x0030, 0x0039, 0).	% Nd  [10] DIGIT ZERO..DIGIT NINE
unicode_combining_class(0x003A, 0x003B, 0).	% Po   [2] COLON..SEMICOLON
unicode_combining_class(0x003C, 0x003E, 0).	% Sm   [3] LESS-THAN SIGN..GREATER-THAN SIGN
unicode_combining_class(0x003F, 0x0040, 0).	% Po   [2] QUESTION MARK..COMMERCIAL AT
unicode_combining_class(0x0041, 0x005A, 0).	% L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
unicode_combining_class(0x005B, 0x005B, 0).	% Ps       LEFT SQUARE BRACKET
unicode_combining_class(0x005C, 0x005C, 0).	% Po       REVERSE SOLIDUS
unicode_combining_class(0x005D, 0x005D, 0).	% Pe       RIGHT SQUARE BRACKET
unicode_combining_class(0x005E, 0x005E, 0).	% Sk       CIRCUMFLEX ACCENT
unicode_combining_class(0x005F, 0x005F, 0).	% Pc       LOW LINE
unicode_combining_class(0x0060, 0x0060, 0).	% Sk       GRAVE ACCENT
unicode_combining_class(0x0061, 0x007A, 0).	% L&  [26] LATIN SMALL LETTER A..LATIN SMALL LETTER Z
unicode_combining_class(0x007B, 0x007B, 0).	% Ps       LEFT CURLY BRACKET
unicode_combining_class(0x007C, 0x007C, 0).	% Sm       VERTICAL LINE
unicode_combining_class(0x007D, 0x007D, 0).	% Pe       RIGHT CURLY BRACKET
unicode_combining_class(0x007E, 0x007E, 0).	% Sm       TILDE
unicode_combining_class(0x007F, 0x009F, 0).	% Cc  [33] <control-007F>..<control-009F>
unicode_combining_class(0x00A0, 0x00A0, 0).	% Zs       NO-BREAK SPACE
unicode_combining_class(0x00A1, 0x00A1, 0).	% Po       INVERTED EXCLAMATION MARK
unicode_combining_class(0x00A2, 0x00A5, 0).	% Sc   [4] CENT SIGN..YEN SIGN
unicode_combining_class(0x00A6, 0x00A6, 0).	% So       BROKEN BAR
unicode_combining_class(0x00A7, 0x00A7, 0).	% Po       SECTION SIGN
unicode_combining_class(0x00A8, 0x00A8, 0).	% Sk       DIAERESIS
unicode_combining_class(0x00A9, 0x00A9, 0).	% So       COPYRIGHT SIGN
unicode_combining_class(0x00AA, 0x00AA, 0).	% Lo       FEMININE ORDINAL INDICATOR
unicode_combining_class(0x00AB, 0x00AB, 0).	% Pi       LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_combining_class(0x00AC, 0x00AC, 0).	% Sm       NOT SIGN
unicode_combining_class(0x00AD, 0x00AD, 0).	% Cf       SOFT HYPHEN
unicode_combining_class(0x00AE, 0x00AE, 0).	% So       REGISTERED SIGN
unicode_combining_class(0x00AF, 0x00AF, 0).	% Sk       MACRON
unicode_combining_class(0x00B0, 0x00B0, 0).	% So       DEGREE SIGN
unicode_combining_class(0x00B1, 0x00B1, 0).	% Sm       PLUS-MINUS SIGN
unicode_combining_class(0x00B2, 0x00B3, 0).	% No   [2] SUPERSCRIPT TWO..SUPERSCRIPT THREE
unicode_combining_class(0x00B4, 0x00B4, 0).	% Sk       ACUTE ACCENT
unicode_combining_class(0x00B5, 0x00B5, 0).	% L&       MICRO SIGN
unicode_combining_class(0x00B6, 0x00B7, 0).	% Po   [2] PILCROW SIGN..MIDDLE DOT
unicode_combining_class(0x00B8, 0x00B8, 0).	% Sk       CEDILLA
unicode_combining_class(0x00B9, 0x00B9, 0).	% No       SUPERSCRIPT ONE
unicode_combining_class(0x00BA, 0x00BA, 0).	% Lo       MASCULINE ORDINAL INDICATOR
unicode_combining_class(0x00BB, 0x00BB, 0).	% Pf       RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_combining_class(0x00BC, 0x00BE, 0).	% No   [3] VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS
unicode_combining_class(0x00BF, 0x00BF, 0).	% Po       INVERTED QUESTION MARK
unicode_combining_class(0x00C0, 0x00D6, 0).	% L&  [23] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_combining_class(0x00D7, 0x00D7, 0).	% Sm       MULTIPLICATION SIGN
unicode_combining_class(0x00D8, 0x00F6, 0).	% L&  [31] LATIN CAPITAL LETTER O WITH STROKE..LATIN SMALL LETTER O WITH DIAERESIS
unicode_combining_class(0x00F7, 0x00F7, 0).	% Sm       DIVISION SIGN
unicode_combining_class(0x00F8, 0x01BA, 0).	% L& [195] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER EZH WITH TAIL
unicode_combining_class(0x01BB, 0x01BB, 0).	% Lo       LATIN LETTER TWO WITH STROKE
unicode_combining_class(0x01BC, 0x01BF, 0).	% L&   [4] LATIN CAPITAL LETTER TONE FIVE..LATIN LETTER WYNN
unicode_combining_class(0x01C0, 0x01C3, 0).	% Lo   [4] LATIN LETTER DENTAL CLICK..LATIN LETTER RETROFLEX CLICK
unicode_combining_class(0x01C4, 0x0293, 0).	% L& [208] LATIN CAPITAL LETTER DZ WITH CARON..LATIN SMALL LETTER EZH WITH CURL
unicode_combining_class(0x0294, 0x0294, 0).	% Lo       LATIN LETTER GLOTTAL STOP
unicode_combining_class(0x0295, 0x02AF, 0).	% L&  [27] LATIN LETTER PHARYNGEAL VOICED FRICATIVE..LATIN SMALL LETTER TURNED H WITH FISHHOOK AND TAIL
unicode_combining_class(0x02B0, 0x02C1, 0).	% Lm  [18] MODIFIER LETTER SMALL H..MODIFIER LETTER REVERSED GLOTTAL STOP
unicode_combining_class(0x02C2, 0x02C5, 0).	% Sk   [4] MODIFIER LETTER LEFT ARROWHEAD..MODIFIER LETTER DOWN ARROWHEAD
unicode_combining_class(0x02C6, 0x02D1, 0).	% Lm  [12] MODIFIER LETTER CIRCUMFLEX ACCENT..MODIFIER LETTER HALF TRIANGULAR COLON
unicode_combining_class(0x02D2, 0x02DF, 0).	% Sk  [14] MODIFIER LETTER CENTRED RIGHT HALF RING..MODIFIER LETTER CROSS ACCENT
unicode_combining_class(0x02E0, 0x02E4, 0).	% Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_combining_class(0x02E5, 0x02EB, 0).	% Sk   [7] MODIFIER LETTER EXTRA-HIGH TONE BAR..MODIFIER LETTER YANG DEPARTING TONE MARK
unicode_combining_class(0x02EC, 0x02EC, 0).	% Lm       MODIFIER LETTER VOICING
unicode_combining_class(0x02ED, 0x02ED, 0).	% Sk       MODIFIER LETTER UNASPIRATED
unicode_combining_class(0x02EE, 0x02EE, 0).	% Lm       MODIFIER LETTER DOUBLE APOSTROPHE
unicode_combining_class(0x02EF, 0x02FF, 0).	% Sk  [17] MODIFIER LETTER LOW DOWN ARROWHEAD..MODIFIER LETTER LOW LEFT ARROW
unicode_combining_class(0x034F, 0x034F, 0).	% Mn       COMBINING GRAPHEME JOINER
unicode_combining_class(0x0370, 0x0373, 0).	% L&   [4] GREEK CAPITAL LETTER HETA..GREEK SMALL LETTER ARCHAIC SAMPI
unicode_combining_class(0x0374, 0x0374, 0).	% Lm       GREEK NUMERAL SIGN
unicode_combining_class(0x0375, 0x0375, 0).	% Sk       GREEK LOWER NUMERAL SIGN
unicode_combining_class(0x0376, 0x0377, 0).	% L&   [2] GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA..GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
unicode_combining_class(0x037A, 0x037A, 0).	% Lm       GREEK YPOGEGRAMMENI
unicode_combining_class(0x037B, 0x037D, 0).	% L&   [3] GREEK SMALL REVERSED LUNATE SIGMA SYMBOL..GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
unicode_combining_class(0x037E, 0x037E, 0).	% Po       GREEK QUESTION MARK
unicode_combining_class(0x0384, 0x0385, 0).	% Sk   [2] GREEK TONOS..GREEK DIALYTIKA TONOS
unicode_combining_class(0x0386, 0x0386, 0).	% L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_combining_class(0x0387, 0x0387, 0).	% Po       GREEK ANO TELEIA
unicode_combining_class(0x0388, 0x038A, 0).	% L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_combining_class(0x038C, 0x038C, 0).	% L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_combining_class(0x038E, 0x03A1, 0).	% L&  [20] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK CAPITAL LETTER RHO
unicode_combining_class(0x03A3, 0x03F5, 0).	% L&  [83] GREEK CAPITAL LETTER SIGMA..GREEK LUNATE EPSILON SYMBOL
unicode_combining_class(0x03F6, 0x03F6, 0).	% Sm       GREEK REVERSED LUNATE EPSILON SYMBOL
unicode_combining_class(0x03F7, 0x0481, 0).	% L& [139] GREEK CAPITAL LETTER SHO..CYRILLIC SMALL LETTER KOPPA
unicode_combining_class(0x0482, 0x0482, 0).	% So       CYRILLIC THOUSANDS SIGN
unicode_combining_class(0x0488, 0x0489, 0).	% Me   [2] COMBINING CYRILLIC HUNDRED THOUSANDS SIGN..COMBINING CYRILLIC MILLIONS SIGN
unicode_combining_class(0x048A, 0x0527, 0).	% L& [158] CYRILLIC CAPITAL LETTER SHORT I WITH TAIL..CYRILLIC SMALL LETTER SHHA WITH DESCENDER
unicode_combining_class(0x0531, 0x0556, 0).	% L&  [38] ARMENIAN CAPITAL LETTER AYB..ARMENIAN CAPITAL LETTER FEH
unicode_combining_class(0x0559, 0x0559, 0).	% Lm       ARMENIAN MODIFIER LETTER LEFT HALF RING
unicode_combining_class(0x055A, 0x055F, 0).	% Po   [6] ARMENIAN APOSTROPHE..ARMENIAN ABBREVIATION MARK
unicode_combining_class(0x0561, 0x0587, 0).	% L&  [39] ARMENIAN SMALL LETTER AYB..ARMENIAN SMALL LIGATURE ECH YIWN
unicode_combining_class(0x0589, 0x0589, 0).	% Po       ARMENIAN FULL STOP
unicode_combining_class(0x058A, 0x058A, 0).	% Pd       ARMENIAN HYPHEN
unicode_combining_class(0x058F, 0x058F, 0).	% Sc       ARMENIAN DRAM SIGN
unicode_combining_class(0x05BE, 0x05BE, 0).	% Pd       HEBREW PUNCTUATION MAQAF
unicode_combining_class(0x05C0, 0x05C0, 0).	% Po       HEBREW PUNCTUATION PASEQ
unicode_combining_class(0x05C3, 0x05C3, 0).	% Po       HEBREW PUNCTUATION SOF PASUQ
unicode_combining_class(0x05C6, 0x05C6, 0).	% Po       HEBREW PUNCTUATION NUN HAFUKHA
unicode_combining_class(0x05D0, 0x05EA, 0).	% Lo  [27] HEBREW LETTER ALEF..HEBREW LETTER TAV
unicode_combining_class(0x05F0, 0x05F2, 0).	% Lo   [3] HEBREW LIGATURE YIDDISH DOUBLE VAV..HEBREW LIGATURE YIDDISH DOUBLE YOD
unicode_combining_class(0x05F3, 0x05F4, 0).	% Po   [2] HEBREW PUNCTUATION GERESH..HEBREW PUNCTUATION GERSHAYIM
unicode_combining_class(0x0600, 0x0604, 0).	% Cf   [5] ARABIC NUMBER SIGN..ARABIC SIGN SAMVAT
unicode_combining_class(0x0606, 0x0608, 0).	% Sm   [3] ARABIC-INDIC CUBE ROOT..ARABIC RAY
unicode_combining_class(0x0609, 0x060A, 0).	% Po   [2] ARABIC-INDIC PER MILLE SIGN..ARABIC-INDIC PER TEN THOUSAND SIGN
unicode_combining_class(0x060B, 0x060B, 0).	% Sc       AFGHANI SIGN
unicode_combining_class(0x060C, 0x060D, 0).	% Po   [2] ARABIC COMMA..ARABIC DATE SEPARATOR
unicode_combining_class(0x060E, 0x060F, 0).	% So   [2] ARABIC POETIC VERSE SIGN..ARABIC SIGN MISRA
unicode_combining_class(0x061B, 0x061B, 0).	% Po       ARABIC SEMICOLON
unicode_combining_class(0x061E, 0x061F, 0).	% Po   [2] ARABIC TRIPLE DOT PUNCTUATION MARK..ARABIC QUESTION MARK
unicode_combining_class(0x0620, 0x063F, 0).	% Lo  [32] ARABIC LETTER KASHMIRI YEH..ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE
unicode_combining_class(0x0640, 0x0640, 0).	% Lm       ARABIC TATWEEL
unicode_combining_class(0x0641, 0x064A, 0).	% Lo  [10] ARABIC LETTER FEH..ARABIC LETTER YEH
unicode_combining_class(0x0660, 0x0669, 0).	% Nd  [10] ARABIC-INDIC DIGIT ZERO..ARABIC-INDIC DIGIT NINE
unicode_combining_class(0x066A, 0x066D, 0).	% Po   [4] ARABIC PERCENT SIGN..ARABIC FIVE POINTED STAR
unicode_combining_class(0x066E, 0x066F, 0).	% Lo   [2] ARABIC LETTER DOTLESS BEH..ARABIC LETTER DOTLESS QAF
unicode_combining_class(0x0671, 0x06D3, 0).	% Lo  [99] ARABIC LETTER ALEF WASLA..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
unicode_combining_class(0x06D4, 0x06D4, 0).	% Po       ARABIC FULL STOP
unicode_combining_class(0x06D5, 0x06D5, 0).	% Lo       ARABIC LETTER AE
unicode_combining_class(0x06DD, 0x06DD, 0).	% Cf       ARABIC END OF AYAH
unicode_combining_class(0x06DE, 0x06DE, 0).	% So       ARABIC START OF RUB EL HIZB
unicode_combining_class(0x06E5, 0x06E6, 0).	% Lm   [2] ARABIC SMALL WAW..ARABIC SMALL YEH
unicode_combining_class(0x06E9, 0x06E9, 0).	% So       ARABIC PLACE OF SAJDAH
unicode_combining_class(0x06EE, 0x06EF, 0).	% Lo   [2] ARABIC LETTER DAL WITH INVERTED V..ARABIC LETTER REH WITH INVERTED V
unicode_combining_class(0x06F0, 0x06F9, 0).	% Nd  [10] EXTENDED ARABIC-INDIC DIGIT ZERO..EXTENDED ARABIC-INDIC DIGIT NINE
unicode_combining_class(0x06FA, 0x06FC, 0).	% Lo   [3] ARABIC LETTER SHEEN WITH DOT BELOW..ARABIC LETTER GHAIN WITH DOT BELOW
unicode_combining_class(0x06FD, 0x06FE, 0).	% So   [2] ARABIC SIGN SINDHI AMPERSAND..ARABIC SIGN SINDHI POSTPOSITION MEN
unicode_combining_class(0x06FF, 0x06FF, 0).	% Lo       ARABIC LETTER HEH WITH INVERTED V
unicode_combining_class(0x0700, 0x070D, 0).	% Po  [14] SYRIAC END OF PARAGRAPH..SYRIAC HARKLEAN ASTERISCUS
unicode_combining_class(0x070F, 0x070F, 0).	% Cf       SYRIAC ABBREVIATION MARK
unicode_combining_class(0x0710, 0x0710, 0).	% Lo       SYRIAC LETTER ALAPH
unicode_combining_class(0x0712, 0x072F, 0).	% Lo  [30] SYRIAC LETTER BETH..SYRIAC LETTER PERSIAN DHALATH
unicode_combining_class(0x074D, 0x07A5, 0).	% Lo  [89] SYRIAC LETTER SOGDIAN ZHAIN..THAANA LETTER WAAVU
unicode_combining_class(0x07A6, 0x07B0, 0).	% Mn  [11] THAANA ABAFILI..THAANA SUKUN
unicode_combining_class(0x07B1, 0x07B1, 0).	% Lo       THAANA LETTER NAA
unicode_combining_class(0x07C0, 0x07C9, 0).	% Nd  [10] NKO DIGIT ZERO..NKO DIGIT NINE
unicode_combining_class(0x07CA, 0x07EA, 0).	% Lo  [33] NKO LETTER A..NKO LETTER JONA RA
unicode_combining_class(0x07F4, 0x07F5, 0).	% Lm   [2] NKO HIGH TONE APOSTROPHE..NKO LOW TONE APOSTROPHE
unicode_combining_class(0x07F6, 0x07F6, 0).	% So       NKO SYMBOL OO DENNEN
unicode_combining_class(0x07F7, 0x07F9, 0).	% Po   [3] NKO SYMBOL GBAKURUNEN..NKO EXCLAMATION MARK
unicode_combining_class(0x07FA, 0x07FA, 0).	% Lm       NKO LAJANYALAN
unicode_combining_class(0x0800, 0x0815, 0).	% Lo  [22] SAMARITAN LETTER ALAF..SAMARITAN LETTER TAAF
unicode_combining_class(0x081A, 0x081A, 0).	% Lm       SAMARITAN MODIFIER LETTER EPENTHETIC YUT
unicode_combining_class(0x0824, 0x0824, 0).	% Lm       SAMARITAN MODIFIER LETTER SHORT A
unicode_combining_class(0x0828, 0x0828, 0).	% Lm       SAMARITAN MODIFIER LETTER I
unicode_combining_class(0x0830, 0x083E, 0).	% Po  [15] SAMARITAN PUNCTUATION NEQUDAA..SAMARITAN PUNCTUATION ANNAAU
unicode_combining_class(0x0840, 0x0858, 0).	% Lo  [25] MANDAIC LETTER HALQA..MANDAIC LETTER AIN
unicode_combining_class(0x085E, 0x085E, 0).	% Po       MANDAIC PUNCTUATION
unicode_combining_class(0x08A0, 0x08A0, 0).	% Lo       ARABIC LETTER BEH WITH SMALL V BELOW
unicode_combining_class(0x08A2, 0x08AC, 0).	% Lo  [11] ARABIC LETTER JEEM WITH TWO DOTS ABOVE..ARABIC LETTER ROHINGYA YEH
unicode_combining_class(0x0900, 0x0902, 0).	% Mn   [3] DEVANAGARI SIGN INVERTED CANDRABINDU..DEVANAGARI SIGN ANUSVARA
unicode_combining_class(0x0903, 0x0903, 0).	% Mc       DEVANAGARI SIGN VISARGA
unicode_combining_class(0x0904, 0x0939, 0).	% Lo  [54] DEVANAGARI LETTER SHORT A..DEVANAGARI LETTER HA
unicode_combining_class(0x093A, 0x093A, 0).	% Mn       DEVANAGARI VOWEL SIGN OE
unicode_combining_class(0x093B, 0x093B, 0).	% Mc       DEVANAGARI VOWEL SIGN OOE
unicode_combining_class(0x093D, 0x093D, 0).	% Lo       DEVANAGARI SIGN AVAGRAHA
unicode_combining_class(0x093E, 0x0940, 0).	% Mc   [3] DEVANAGARI VOWEL SIGN AA..DEVANAGARI VOWEL SIGN II
unicode_combining_class(0x0941, 0x0948, 0).	% Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
unicode_combining_class(0x0949, 0x094C, 0).	% Mc   [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU
unicode_combining_class(0x094E, 0x094F, 0).	% Mc   [2] DEVANAGARI VOWEL SIGN PRISHTHAMATRA E..DEVANAGARI VOWEL SIGN AW
unicode_combining_class(0x0950, 0x0950, 0).	% Lo       DEVANAGARI OM
unicode_combining_class(0x0955, 0x0957, 0).	% Mn   [3] DEVANAGARI VOWEL SIGN CANDRA LONG E..DEVANAGARI VOWEL SIGN UUE
unicode_combining_class(0x0958, 0x0961, 0).	% Lo  [10] DEVANAGARI LETTER QA..DEVANAGARI LETTER VOCALIC LL
unicode_combining_class(0x0962, 0x0963, 0).	% Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
unicode_combining_class(0x0964, 0x0965, 0).	% Po   [2] DEVANAGARI DANDA..DEVANAGARI DOUBLE DANDA
unicode_combining_class(0x0966, 0x096F, 0).	% Nd  [10] DEVANAGARI DIGIT ZERO..DEVANAGARI DIGIT NINE
unicode_combining_class(0x0970, 0x0970, 0).	% Po       DEVANAGARI ABBREVIATION SIGN
unicode_combining_class(0x0971, 0x0971, 0).	% Lm       DEVANAGARI SIGN HIGH SPACING DOT
unicode_combining_class(0x0972, 0x0977, 0).	% Lo   [6] DEVANAGARI LETTER CANDRA A..DEVANAGARI LETTER UUE
unicode_combining_class(0x0979, 0x097F, 0).	% Lo   [7] DEVANAGARI LETTER ZHA..DEVANAGARI LETTER BBA
unicode_combining_class(0x0981, 0x0981, 0).	% Mn       BENGALI SIGN CANDRABINDU
unicode_combining_class(0x0982, 0x0983, 0).	% Mc   [2] BENGALI SIGN ANUSVARA..BENGALI SIGN VISARGA
unicode_combining_class(0x0985, 0x098C, 0).	% Lo   [8] BENGALI LETTER A..BENGALI LETTER VOCALIC L
unicode_combining_class(0x098F, 0x0990, 0).	% Lo   [2] BENGALI LETTER E..BENGALI LETTER AI
unicode_combining_class(0x0993, 0x09A8, 0).	% Lo  [22] BENGALI LETTER O..BENGALI LETTER NA
unicode_combining_class(0x09AA, 0x09B0, 0).	% Lo   [7] BENGALI LETTER PA..BENGALI LETTER RA
unicode_combining_class(0x09B2, 0x09B2, 0).	% Lo       BENGALI LETTER LA
unicode_combining_class(0x09B6, 0x09B9, 0).	% Lo   [4] BENGALI LETTER SHA..BENGALI LETTER HA
unicode_combining_class(0x09BD, 0x09BD, 0).	% Lo       BENGALI SIGN AVAGRAHA
unicode_combining_class(0x09BE, 0x09C0, 0).	% Mc   [3] BENGALI VOWEL SIGN AA..BENGALI VOWEL SIGN II
unicode_combining_class(0x09C1, 0x09C4, 0).	% Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
unicode_combining_class(0x09C7, 0x09C8, 0).	% Mc   [2] BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI
unicode_combining_class(0x09CB, 0x09CC, 0).	% Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_combining_class(0x09CE, 0x09CE, 0).	% Lo       BENGALI LETTER KHANDA TA
unicode_combining_class(0x09D7, 0x09D7, 0).	% Mc       BENGALI AU LENGTH MARK
unicode_combining_class(0x09DC, 0x09DD, 0).	% Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_combining_class(0x09DF, 0x09E1, 0).	% Lo   [3] BENGALI LETTER YYA..BENGALI LETTER VOCALIC LL
unicode_combining_class(0x09E2, 0x09E3, 0).	% Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
unicode_combining_class(0x09E6, 0x09EF, 0).	% Nd  [10] BENGALI DIGIT ZERO..BENGALI DIGIT NINE
unicode_combining_class(0x09F0, 0x09F1, 0).	% Lo   [2] BENGALI LETTER RA WITH MIDDLE DIAGONAL..BENGALI LETTER RA WITH LOWER DIAGONAL
unicode_combining_class(0x09F2, 0x09F3, 0).	% Sc   [2] BENGALI RUPEE MARK..BENGALI RUPEE SIGN
unicode_combining_class(0x09F4, 0x09F9, 0).	% No   [6] BENGALI CURRENCY NUMERATOR ONE..BENGALI CURRENCY DENOMINATOR SIXTEEN
unicode_combining_class(0x09FA, 0x09FA, 0).	% So       BENGALI ISSHAR
unicode_combining_class(0x09FB, 0x09FB, 0).	% Sc       BENGALI GANDA MARK
unicode_combining_class(0x0A01, 0x0A02, 0).	% Mn   [2] GURMUKHI SIGN ADAK BINDI..GURMUKHI SIGN BINDI
unicode_combining_class(0x0A03, 0x0A03, 0).	% Mc       GURMUKHI SIGN VISARGA
unicode_combining_class(0x0A05, 0x0A0A, 0).	% Lo   [6] GURMUKHI LETTER A..GURMUKHI LETTER UU
unicode_combining_class(0x0A0F, 0x0A10, 0).	% Lo   [2] GURMUKHI LETTER EE..GURMUKHI LETTER AI
unicode_combining_class(0x0A13, 0x0A28, 0).	% Lo  [22] GURMUKHI LETTER OO..GURMUKHI LETTER NA
unicode_combining_class(0x0A2A, 0x0A30, 0).	% Lo   [7] GURMUKHI LETTER PA..GURMUKHI LETTER RA
unicode_combining_class(0x0A32, 0x0A33, 0).	% Lo   [2] GURMUKHI LETTER LA..GURMUKHI LETTER LLA
unicode_combining_class(0x0A35, 0x0A36, 0).	% Lo   [2] GURMUKHI LETTER VA..GURMUKHI LETTER SHA
unicode_combining_class(0x0A38, 0x0A39, 0).	% Lo   [2] GURMUKHI LETTER SA..GURMUKHI LETTER HA
unicode_combining_class(0x0A3E, 0x0A40, 0).	% Mc   [3] GURMUKHI VOWEL SIGN AA..GURMUKHI VOWEL SIGN II
unicode_combining_class(0x0A41, 0x0A42, 0).	% Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
unicode_combining_class(0x0A47, 0x0A48, 0).	% Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
unicode_combining_class(0x0A4B, 0x0A4C, 0).	% Mn   [2] GURMUKHI VOWEL SIGN OO..GURMUKHI VOWEL SIGN AU
unicode_combining_class(0x0A51, 0x0A51, 0).	% Mn       GURMUKHI SIGN UDAAT
unicode_combining_class(0x0A59, 0x0A5C, 0).	% Lo   [4] GURMUKHI LETTER KHHA..GURMUKHI LETTER RRA
unicode_combining_class(0x0A5E, 0x0A5E, 0).	% Lo       GURMUKHI LETTER FA
unicode_combining_class(0x0A66, 0x0A6F, 0).	% Nd  [10] GURMUKHI DIGIT ZERO..GURMUKHI DIGIT NINE
unicode_combining_class(0x0A70, 0x0A71, 0).	% Mn   [2] GURMUKHI TIPPI..GURMUKHI ADDAK
unicode_combining_class(0x0A72, 0x0A74, 0).	% Lo   [3] GURMUKHI IRI..GURMUKHI EK ONKAR
unicode_combining_class(0x0A75, 0x0A75, 0).	% Mn       GURMUKHI SIGN YAKASH
unicode_combining_class(0x0A81, 0x0A82, 0).	% Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
unicode_combining_class(0x0A83, 0x0A83, 0).	% Mc       GUJARATI SIGN VISARGA
unicode_combining_class(0x0A85, 0x0A8D, 0).	% Lo   [9] GUJARATI LETTER A..GUJARATI VOWEL CANDRA E
unicode_combining_class(0x0A8F, 0x0A91, 0).	% Lo   [3] GUJARATI LETTER E..GUJARATI VOWEL CANDRA O
unicode_combining_class(0x0A93, 0x0AA8, 0).	% Lo  [22] GUJARATI LETTER O..GUJARATI LETTER NA
unicode_combining_class(0x0AAA, 0x0AB0, 0).	% Lo   [7] GUJARATI LETTER PA..GUJARATI LETTER RA
unicode_combining_class(0x0AB2, 0x0AB3, 0).	% Lo   [2] GUJARATI LETTER LA..GUJARATI LETTER LLA
unicode_combining_class(0x0AB5, 0x0AB9, 0).	% Lo   [5] GUJARATI LETTER VA..GUJARATI LETTER HA
unicode_combining_class(0x0ABD, 0x0ABD, 0).	% Lo       GUJARATI SIGN AVAGRAHA
unicode_combining_class(0x0ABE, 0x0AC0, 0).	% Mc   [3] GUJARATI VOWEL SIGN AA..GUJARATI VOWEL SIGN II
unicode_combining_class(0x0AC1, 0x0AC5, 0).	% Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
unicode_combining_class(0x0AC7, 0x0AC8, 0).	% Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
unicode_combining_class(0x0AC9, 0x0AC9, 0).	% Mc       GUJARATI VOWEL SIGN CANDRA O
unicode_combining_class(0x0ACB, 0x0ACC, 0).	% Mc   [2] GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU
unicode_combining_class(0x0AD0, 0x0AD0, 0).	% Lo       GUJARATI OM
unicode_combining_class(0x0AE0, 0x0AE1, 0).	% Lo   [2] GUJARATI LETTER VOCALIC RR..GUJARATI LETTER VOCALIC LL
unicode_combining_class(0x0AE2, 0x0AE3, 0).	% Mn   [2] GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL
unicode_combining_class(0x0AE6, 0x0AEF, 0).	% Nd  [10] GUJARATI DIGIT ZERO..GUJARATI DIGIT NINE
unicode_combining_class(0x0AF0, 0x0AF0, 0).	% Po       GUJARATI ABBREVIATION SIGN
unicode_combining_class(0x0AF1, 0x0AF1, 0).	% Sc       GUJARATI RUPEE SIGN
unicode_combining_class(0x0B01, 0x0B01, 0).	% Mn       ORIYA SIGN CANDRABINDU
unicode_combining_class(0x0B02, 0x0B03, 0).	% Mc   [2] ORIYA SIGN ANUSVARA..ORIYA SIGN VISARGA
unicode_combining_class(0x0B05, 0x0B0C, 0).	% Lo   [8] ORIYA LETTER A..ORIYA LETTER VOCALIC L
unicode_combining_class(0x0B0F, 0x0B10, 0).	% Lo   [2] ORIYA LETTER E..ORIYA LETTER AI
unicode_combining_class(0x0B13, 0x0B28, 0).	% Lo  [22] ORIYA LETTER O..ORIYA LETTER NA
unicode_combining_class(0x0B2A, 0x0B30, 0).	% Lo   [7] ORIYA LETTER PA..ORIYA LETTER RA
unicode_combining_class(0x0B32, 0x0B33, 0).	% Lo   [2] ORIYA LETTER LA..ORIYA LETTER LLA
unicode_combining_class(0x0B35, 0x0B39, 0).	% Lo   [5] ORIYA LETTER VA..ORIYA LETTER HA
unicode_combining_class(0x0B3D, 0x0B3D, 0).	% Lo       ORIYA SIGN AVAGRAHA
unicode_combining_class(0x0B3E, 0x0B3E, 0).	% Mc       ORIYA VOWEL SIGN AA
unicode_combining_class(0x0B3F, 0x0B3F, 0).	% Mn       ORIYA VOWEL SIGN I
unicode_combining_class(0x0B40, 0x0B40, 0).	% Mc       ORIYA VOWEL SIGN II
unicode_combining_class(0x0B41, 0x0B44, 0).	% Mn   [4] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR
unicode_combining_class(0x0B47, 0x0B48, 0).	% Mc   [2] ORIYA VOWEL SIGN E..ORIYA VOWEL SIGN AI
unicode_combining_class(0x0B4B, 0x0B4C, 0).	% Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
unicode_combining_class(0x0B56, 0x0B56, 0).	% Mn       ORIYA AI LENGTH MARK
unicode_combining_class(0x0B57, 0x0B57, 0).	% Mc       ORIYA AU LENGTH MARK
unicode_combining_class(0x0B5C, 0x0B5D, 0).	% Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_combining_class(0x0B5F, 0x0B61, 0).	% Lo   [3] ORIYA LETTER YYA..ORIYA LETTER VOCALIC LL
unicode_combining_class(0x0B62, 0x0B63, 0).	% Mn   [2] ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL
unicode_combining_class(0x0B66, 0x0B6F, 0).	% Nd  [10] ORIYA DIGIT ZERO..ORIYA DIGIT NINE
unicode_combining_class(0x0B70, 0x0B70, 0).	% So       ORIYA ISSHAR
unicode_combining_class(0x0B71, 0x0B71, 0).	% Lo       ORIYA LETTER WA
unicode_combining_class(0x0B72, 0x0B77, 0).	% No   [6] ORIYA FRACTION ONE QUARTER..ORIYA FRACTION THREE SIXTEENTHS
unicode_combining_class(0x0B82, 0x0B82, 0).	% Mn       TAMIL SIGN ANUSVARA
unicode_combining_class(0x0B83, 0x0B83, 0).	% Lo       TAMIL SIGN VISARGA
unicode_combining_class(0x0B85, 0x0B8A, 0).	% Lo   [6] TAMIL LETTER A..TAMIL LETTER UU
unicode_combining_class(0x0B8E, 0x0B90, 0).	% Lo   [3] TAMIL LETTER E..TAMIL LETTER AI
unicode_combining_class(0x0B92, 0x0B95, 0).	% Lo   [4] TAMIL LETTER O..TAMIL LETTER KA
unicode_combining_class(0x0B99, 0x0B9A, 0).	% Lo   [2] TAMIL LETTER NGA..TAMIL LETTER CA
unicode_combining_class(0x0B9C, 0x0B9C, 0).	% Lo       TAMIL LETTER JA
unicode_combining_class(0x0B9E, 0x0B9F, 0).	% Lo   [2] TAMIL LETTER NYA..TAMIL LETTER TTA
unicode_combining_class(0x0BA3, 0x0BA4, 0).	% Lo   [2] TAMIL LETTER NNA..TAMIL LETTER TA
unicode_combining_class(0x0BA8, 0x0BAA, 0).	% Lo   [3] TAMIL LETTER NA..TAMIL LETTER PA
unicode_combining_class(0x0BAE, 0x0BB9, 0).	% Lo  [12] TAMIL LETTER MA..TAMIL LETTER HA
unicode_combining_class(0x0BBE, 0x0BBF, 0).	% Mc   [2] TAMIL VOWEL SIGN AA..TAMIL VOWEL SIGN I
unicode_combining_class(0x0BC0, 0x0BC0, 0).	% Mn       TAMIL VOWEL SIGN II
unicode_combining_class(0x0BC1, 0x0BC2, 0).	% Mc   [2] TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU
unicode_combining_class(0x0BC6, 0x0BC8, 0).	% Mc   [3] TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI
unicode_combining_class(0x0BCA, 0x0BCC, 0).	% Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_combining_class(0x0BD0, 0x0BD0, 0).	% Lo       TAMIL OM
unicode_combining_class(0x0BD7, 0x0BD7, 0).	% Mc       TAMIL AU LENGTH MARK
unicode_combining_class(0x0BE6, 0x0BEF, 0).	% Nd  [10] TAMIL DIGIT ZERO..TAMIL DIGIT NINE
unicode_combining_class(0x0BF0, 0x0BF2, 0).	% No   [3] TAMIL NUMBER TEN..TAMIL NUMBER ONE THOUSAND
unicode_combining_class(0x0BF3, 0x0BF8, 0).	% So   [6] TAMIL DAY SIGN..TAMIL AS ABOVE SIGN
unicode_combining_class(0x0BF9, 0x0BF9, 0).	% Sc       TAMIL RUPEE SIGN
unicode_combining_class(0x0BFA, 0x0BFA, 0).	% So       TAMIL NUMBER SIGN
unicode_combining_class(0x0C01, 0x0C03, 0).	% Mc   [3] TELUGU SIGN CANDRABINDU..TELUGU SIGN VISARGA
unicode_combining_class(0x0C05, 0x0C0C, 0).	% Lo   [8] TELUGU LETTER A..TELUGU LETTER VOCALIC L
unicode_combining_class(0x0C0E, 0x0C10, 0).	% Lo   [3] TELUGU LETTER E..TELUGU LETTER AI
unicode_combining_class(0x0C12, 0x0C28, 0).	% Lo  [23] TELUGU LETTER O..TELUGU LETTER NA
unicode_combining_class(0x0C2A, 0x0C33, 0).	% Lo  [10] TELUGU LETTER PA..TELUGU LETTER LLA
unicode_combining_class(0x0C35, 0x0C39, 0).	% Lo   [5] TELUGU LETTER VA..TELUGU LETTER HA
unicode_combining_class(0x0C3D, 0x0C3D, 0).	% Lo       TELUGU SIGN AVAGRAHA
unicode_combining_class(0x0C3E, 0x0C40, 0).	% Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
unicode_combining_class(0x0C41, 0x0C44, 0).	% Mc   [4] TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR
unicode_combining_class(0x0C46, 0x0C48, 0).	% Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
unicode_combining_class(0x0C4A, 0x0C4C, 0).	% Mn   [3] TELUGU VOWEL SIGN O..TELUGU VOWEL SIGN AU
unicode_combining_class(0x0C58, 0x0C59, 0).	% Lo   [2] TELUGU LETTER TSA..TELUGU LETTER DZA
unicode_combining_class(0x0C60, 0x0C61, 0).	% Lo   [2] TELUGU LETTER VOCALIC RR..TELUGU LETTER VOCALIC LL
unicode_combining_class(0x0C62, 0x0C63, 0).	% Mn   [2] TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL
unicode_combining_class(0x0C66, 0x0C6F, 0).	% Nd  [10] TELUGU DIGIT ZERO..TELUGU DIGIT NINE
unicode_combining_class(0x0C78, 0x0C7E, 0).	% No   [7] TELUGU FRACTION DIGIT ZERO FOR ODD POWERS OF FOUR..TELUGU FRACTION DIGIT THREE FOR EVEN POWERS OF FOUR
unicode_combining_class(0x0C7F, 0x0C7F, 0).	% So       TELUGU SIGN TUUMU
unicode_combining_class(0x0C82, 0x0C83, 0).	% Mc   [2] KANNADA SIGN ANUSVARA..KANNADA SIGN VISARGA
unicode_combining_class(0x0C85, 0x0C8C, 0).	% Lo   [8] KANNADA LETTER A..KANNADA LETTER VOCALIC L
unicode_combining_class(0x0C8E, 0x0C90, 0).	% Lo   [3] KANNADA LETTER E..KANNADA LETTER AI
unicode_combining_class(0x0C92, 0x0CA8, 0).	% Lo  [23] KANNADA LETTER O..KANNADA LETTER NA
unicode_combining_class(0x0CAA, 0x0CB3, 0).	% Lo  [10] KANNADA LETTER PA..KANNADA LETTER LLA
unicode_combining_class(0x0CB5, 0x0CB9, 0).	% Lo   [5] KANNADA LETTER VA..KANNADA LETTER HA
unicode_combining_class(0x0CBD, 0x0CBD, 0).	% Lo       KANNADA SIGN AVAGRAHA
unicode_combining_class(0x0CBE, 0x0CBE, 0).	% Mc       KANNADA VOWEL SIGN AA
unicode_combining_class(0x0CBF, 0x0CBF, 0).	% Mn       KANNADA VOWEL SIGN I
unicode_combining_class(0x0CC0, 0x0CC4, 0).	% Mc   [5] KANNADA VOWEL SIGN II..KANNADA VOWEL SIGN VOCALIC RR
unicode_combining_class(0x0CC6, 0x0CC6, 0).	% Mn       KANNADA VOWEL SIGN E
unicode_combining_class(0x0CC7, 0x0CC8, 0).	% Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_combining_class(0x0CCA, 0x0CCB, 0).	% Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_combining_class(0x0CCC, 0x0CCC, 0).	% Mn       KANNADA VOWEL SIGN AU
unicode_combining_class(0x0CD5, 0x0CD6, 0).	% Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
unicode_combining_class(0x0CDE, 0x0CDE, 0).	% Lo       KANNADA LETTER FA
unicode_combining_class(0x0CE0, 0x0CE1, 0).	% Lo   [2] KANNADA LETTER VOCALIC RR..KANNADA LETTER VOCALIC LL
unicode_combining_class(0x0CE2, 0x0CE3, 0).	% Mn   [2] KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL
unicode_combining_class(0x0CE6, 0x0CEF, 0).	% Nd  [10] KANNADA DIGIT ZERO..KANNADA DIGIT NINE
unicode_combining_class(0x0CF1, 0x0CF2, 0).	% Lo   [2] KANNADA SIGN JIHVAMULIYA..KANNADA SIGN UPADHMANIYA
unicode_combining_class(0x0D02, 0x0D03, 0).	% Mc   [2] MALAYALAM SIGN ANUSVARA..MALAYALAM SIGN VISARGA
unicode_combining_class(0x0D05, 0x0D0C, 0).	% Lo   [8] MALAYALAM LETTER A..MALAYALAM LETTER VOCALIC L
unicode_combining_class(0x0D0E, 0x0D10, 0).	% Lo   [3] MALAYALAM LETTER E..MALAYALAM LETTER AI
unicode_combining_class(0x0D12, 0x0D3A, 0).	% Lo  [41] MALAYALAM LETTER O..MALAYALAM LETTER TTTA
unicode_combining_class(0x0D3D, 0x0D3D, 0).	% Lo       MALAYALAM SIGN AVAGRAHA
unicode_combining_class(0x0D3E, 0x0D40, 0).	% Mc   [3] MALAYALAM VOWEL SIGN AA..MALAYALAM VOWEL SIGN II
unicode_combining_class(0x0D41, 0x0D44, 0).	% Mn   [4] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC RR
unicode_combining_class(0x0D46, 0x0D48, 0).	% Mc   [3] MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI
unicode_combining_class(0x0D4A, 0x0D4C, 0).	% Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_combining_class(0x0D4E, 0x0D4E, 0).	% Lo       MALAYALAM LETTER DOT REPH
unicode_combining_class(0x0D57, 0x0D57, 0).	% Mc       MALAYALAM AU LENGTH MARK
unicode_combining_class(0x0D60, 0x0D61, 0).	% Lo   [2] MALAYALAM LETTER VOCALIC RR..MALAYALAM LETTER VOCALIC LL
unicode_combining_class(0x0D62, 0x0D63, 0).	% Mn   [2] MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL
unicode_combining_class(0x0D66, 0x0D6F, 0).	% Nd  [10] MALAYALAM DIGIT ZERO..MALAYALAM DIGIT NINE
unicode_combining_class(0x0D70, 0x0D75, 0).	% No   [6] MALAYALAM NUMBER TEN..MALAYALAM FRACTION THREE QUARTERS
unicode_combining_class(0x0D79, 0x0D79, 0).	% So       MALAYALAM DATE MARK
unicode_combining_class(0x0D7A, 0x0D7F, 0).	% Lo   [6] MALAYALAM LETTER CHILLU NN..MALAYALAM LETTER CHILLU K
unicode_combining_class(0x0D82, 0x0D83, 0).	% Mc   [2] SINHALA SIGN ANUSVARAYA..SINHALA SIGN VISARGAYA
unicode_combining_class(0x0D85, 0x0D96, 0).	% Lo  [18] SINHALA LETTER AYANNA..SINHALA LETTER AUYANNA
unicode_combining_class(0x0D9A, 0x0DB1, 0).	% Lo  [24] SINHALA LETTER ALPAPRAANA KAYANNA..SINHALA LETTER DANTAJA NAYANNA
unicode_combining_class(0x0DB3, 0x0DBB, 0).	% Lo   [9] SINHALA LETTER SANYAKA DAYANNA..SINHALA LETTER RAYANNA
unicode_combining_class(0x0DBD, 0x0DBD, 0).	% Lo       SINHALA LETTER DANTAJA LAYANNA
unicode_combining_class(0x0DC0, 0x0DC6, 0).	% Lo   [7] SINHALA LETTER VAYANNA..SINHALA LETTER FAYANNA
unicode_combining_class(0x0DCF, 0x0DD1, 0).	% Mc   [3] SINHALA VOWEL SIGN AELA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA
unicode_combining_class(0x0DD2, 0x0DD4, 0).	% Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
unicode_combining_class(0x0DD6, 0x0DD6, 0).	% Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
unicode_combining_class(0x0DD8, 0x0DDF, 0).	% Mc   [8] SINHALA VOWEL SIGN GAETTA-PILLA..SINHALA VOWEL SIGN GAYANUKITTA
unicode_combining_class(0x0DF2, 0x0DF3, 0).	% Mc   [2] SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA
unicode_combining_class(0x0DF4, 0x0DF4, 0).	% Po       SINHALA PUNCTUATION KUNDDALIYA
unicode_combining_class(0x0E01, 0x0E30, 0).	% Lo  [48] THAI CHARACTER KO KAI..THAI CHARACTER SARA A
unicode_combining_class(0x0E31, 0x0E31, 0).	% Mn       THAI CHARACTER MAI HAN-AKAT
unicode_combining_class(0x0E32, 0x0E33, 0).	% Lo   [2] THAI CHARACTER SARA AA..THAI CHARACTER SARA AM
unicode_combining_class(0x0E34, 0x0E37, 0).	% Mn   [4] THAI CHARACTER SARA I..THAI CHARACTER SARA UEE
unicode_combining_class(0x0E3F, 0x0E3F, 0).	% Sc       THAI CURRENCY SYMBOL BAHT
unicode_combining_class(0x0E40, 0x0E45, 0).	% Lo   [6] THAI CHARACTER SARA E..THAI CHARACTER LAKKHANGYAO
unicode_combining_class(0x0E46, 0x0E46, 0).	% Lm       THAI CHARACTER MAIYAMOK
unicode_combining_class(0x0E47, 0x0E47, 0).	% Mn       THAI CHARACTER MAITAIKHU
unicode_combining_class(0x0E4C, 0x0E4E, 0).	% Mn   [3] THAI CHARACTER THANTHAKHAT..THAI CHARACTER YAMAKKAN
unicode_combining_class(0x0E4F, 0x0E4F, 0).	% Po       THAI CHARACTER FONGMAN
unicode_combining_class(0x0E50, 0x0E59, 0).	% Nd  [10] THAI DIGIT ZERO..THAI DIGIT NINE
unicode_combining_class(0x0E5A, 0x0E5B, 0).	% Po   [2] THAI CHARACTER ANGKHANKHU..THAI CHARACTER KHOMUT
unicode_combining_class(0x0E81, 0x0E82, 0).	% Lo   [2] LAO LETTER KO..LAO LETTER KHO SUNG
unicode_combining_class(0x0E84, 0x0E84, 0).	% Lo       LAO LETTER KHO TAM
unicode_combining_class(0x0E87, 0x0E88, 0).	% Lo   [2] LAO LETTER NGO..LAO LETTER CO
unicode_combining_class(0x0E8A, 0x0E8A, 0).	% Lo       LAO LETTER SO TAM
unicode_combining_class(0x0E8D, 0x0E8D, 0).	% Lo       LAO LETTER NYO
unicode_combining_class(0x0E94, 0x0E97, 0).	% Lo   [4] LAO LETTER DO..LAO LETTER THO TAM
unicode_combining_class(0x0E99, 0x0E9F, 0).	% Lo   [7] LAO LETTER NO..LAO LETTER FO SUNG
unicode_combining_class(0x0EA1, 0x0EA3, 0).	% Lo   [3] LAO LETTER MO..LAO LETTER LO LING
unicode_combining_class(0x0EA5, 0x0EA5, 0).	% Lo       LAO LETTER LO LOOT
unicode_combining_class(0x0EA7, 0x0EA7, 0).	% Lo       LAO LETTER WO
unicode_combining_class(0x0EAA, 0x0EAB, 0).	% Lo   [2] LAO LETTER SO SUNG..LAO LETTER HO SUNG
unicode_combining_class(0x0EAD, 0x0EB0, 0).	% Lo   [4] LAO LETTER O..LAO VOWEL SIGN A
unicode_combining_class(0x0EB1, 0x0EB1, 0).	% Mn       LAO VOWEL SIGN MAI KAN
unicode_combining_class(0x0EB2, 0x0EB3, 0).	% Lo   [2] LAO VOWEL SIGN AA..LAO VOWEL SIGN AM
unicode_combining_class(0x0EB4, 0x0EB7, 0).	% Mn   [4] LAO VOWEL SIGN I..LAO VOWEL SIGN YY
unicode_combining_class(0x0EBB, 0x0EBC, 0).	% Mn   [2] LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO
unicode_combining_class(0x0EBD, 0x0EBD, 0).	% Lo       LAO SEMIVOWEL SIGN NYO
unicode_combining_class(0x0EC0, 0x0EC4, 0).	% Lo   [5] LAO VOWEL SIGN E..LAO VOWEL SIGN AI
unicode_combining_class(0x0EC6, 0x0EC6, 0).	% Lm       LAO KO LA
unicode_combining_class(0x0ECC, 0x0ECD, 0).	% Mn   [2] LAO CANCELLATION MARK..LAO NIGGAHITA
unicode_combining_class(0x0ED0, 0x0ED9, 0).	% Nd  [10] LAO DIGIT ZERO..LAO DIGIT NINE
unicode_combining_class(0x0EDC, 0x0EDF, 0).	% Lo   [4] LAO HO NO..LAO LETTER KHMU NYO
unicode_combining_class(0x0F00, 0x0F00, 0).	% Lo       TIBETAN SYLLABLE OM
unicode_combining_class(0x0F01, 0x0F03, 0).	% So   [3] TIBETAN MARK GTER YIG MGO TRUNCATED A..TIBETAN MARK GTER YIG MGO -UM GTER TSHEG MA
unicode_combining_class(0x0F04, 0x0F12, 0).	% Po  [15] TIBETAN MARK INITIAL YIG MGO MDUN MA..TIBETAN MARK RGYA GRAM SHAD
unicode_combining_class(0x0F13, 0x0F13, 0).	% So       TIBETAN MARK CARET -DZUD RTAGS ME LONG CAN
unicode_combining_class(0x0F14, 0x0F14, 0).	% Po       TIBETAN MARK GTER TSHEG
unicode_combining_class(0x0F15, 0x0F17, 0).	% So   [3] TIBETAN LOGOTYPE SIGN CHAD RTAGS..TIBETAN ASTROLOGICAL SIGN SGRA GCAN -CHAR RTAGS
unicode_combining_class(0x0F1A, 0x0F1F, 0).	% So   [6] TIBETAN SIGN RDEL DKAR GCIG..TIBETAN SIGN RDEL DKAR RDEL NAG
unicode_combining_class(0x0F20, 0x0F29, 0).	% Nd  [10] TIBETAN DIGIT ZERO..TIBETAN DIGIT NINE
unicode_combining_class(0x0F2A, 0x0F33, 0).	% No  [10] TIBETAN DIGIT HALF ONE..TIBETAN DIGIT HALF ZERO
unicode_combining_class(0x0F34, 0x0F34, 0).	% So       TIBETAN MARK BSDUS RTAGS
unicode_combining_class(0x0F36, 0x0F36, 0).	% So       TIBETAN MARK CARET -DZUD RTAGS BZHI MIG CAN
unicode_combining_class(0x0F38, 0x0F38, 0).	% So       TIBETAN MARK CHE MGO
unicode_combining_class(0x0F3A, 0x0F3A, 0).	% Ps       TIBETAN MARK GUG RTAGS GYON
unicode_combining_class(0x0F3B, 0x0F3B, 0).	% Pe       TIBETAN MARK GUG RTAGS GYAS
unicode_combining_class(0x0F3C, 0x0F3C, 0).	% Ps       TIBETAN MARK ANG KHANG GYON
unicode_combining_class(0x0F3D, 0x0F3D, 0).	% Pe       TIBETAN MARK ANG KHANG GYAS
unicode_combining_class(0x0F3E, 0x0F3F, 0).	% Mc   [2] TIBETAN SIGN YAR TSHES..TIBETAN SIGN MAR TSHES
unicode_combining_class(0x0F40, 0x0F47, 0).	% Lo   [8] TIBETAN LETTER KA..TIBETAN LETTER JA
unicode_combining_class(0x0F49, 0x0F6C, 0).	% Lo  [36] TIBETAN LETTER NYA..TIBETAN LETTER RRA
unicode_combining_class(0x0F73, 0x0F73, 0).	% Mn       TIBETAN VOWEL SIGN II
unicode_combining_class(0x0F75, 0x0F79, 0).	% Mn   [5] TIBETAN VOWEL SIGN UU..TIBETAN VOWEL SIGN VOCALIC LL
unicode_combining_class(0x0F7E, 0x0F7E, 0).	% Mn       TIBETAN SIGN RJES SU NGA RO
unicode_combining_class(0x0F7F, 0x0F7F, 0).	% Mc       TIBETAN SIGN RNAM BCAD
unicode_combining_class(0x0F81, 0x0F81, 0).	% Mn       TIBETAN VOWEL SIGN REVERSED II
unicode_combining_class(0x0F85, 0x0F85, 0).	% Po       TIBETAN MARK PALUTA
unicode_combining_class(0x0F88, 0x0F8C, 0).	% Lo   [5] TIBETAN SIGN LCE TSA CAN..TIBETAN SIGN INVERTED MCHU CAN
unicode_combining_class(0x0F8D, 0x0F97, 0).	% Mn  [11] TIBETAN SUBJOINED SIGN LCE TSA CAN..TIBETAN SUBJOINED LETTER JA
unicode_combining_class(0x0F99, 0x0FBC, 0).	% Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
unicode_combining_class(0x0FBE, 0x0FC5, 0).	% So   [8] TIBETAN KU RU KHA..TIBETAN SYMBOL RDO RJE
unicode_combining_class(0x0FC7, 0x0FCC, 0).	% So   [6] TIBETAN SYMBOL RDO RJE RGYA GRAM..TIBETAN SYMBOL NOR BU BZHI -KHYIL
unicode_combining_class(0x0FCE, 0x0FCF, 0).	% So   [2] TIBETAN SIGN RDEL NAG RDEL DKAR..TIBETAN SIGN RDEL NAG GSUM
unicode_combining_class(0x0FD0, 0x0FD4, 0).	% Po   [5] TIBETAN MARK BSKA- SHOG GI MGO RGYAN..TIBETAN MARK CLOSING BRDA RNYING YIG MGO SGAB MA
unicode_combining_class(0x0FD5, 0x0FD8, 0).	% So   [4] RIGHT-FACING SVASTI SIGN..LEFT-FACING SVASTI SIGN WITH DOTS
unicode_combining_class(0x0FD9, 0x0FDA, 0).	% Po   [2] TIBETAN MARK LEADING MCHAN RTAGS..TIBETAN MARK TRAILING MCHAN RTAGS
unicode_combining_class(0x1000, 0x102A, 0).	% Lo  [43] MYANMAR LETTER KA..MYANMAR LETTER AU
unicode_combining_class(0x102B, 0x102C, 0).	% Mc   [2] MYANMAR VOWEL SIGN TALL AA..MYANMAR VOWEL SIGN AA
unicode_combining_class(0x102D, 0x1030, 0).	% Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
unicode_combining_class(0x1031, 0x1031, 0).	% Mc       MYANMAR VOWEL SIGN E
unicode_combining_class(0x1032, 0x1036, 0).	% Mn   [5] MYANMAR VOWEL SIGN AI..MYANMAR SIGN ANUSVARA
unicode_combining_class(0x1038, 0x1038, 0).	% Mc       MYANMAR SIGN VISARGA
unicode_combining_class(0x103B, 0x103C, 0).	% Mc   [2] MYANMAR CONSONANT SIGN MEDIAL YA..MYANMAR CONSONANT SIGN MEDIAL RA
unicode_combining_class(0x103D, 0x103E, 0).	% Mn   [2] MYANMAR CONSONANT SIGN MEDIAL WA..MYANMAR CONSONANT SIGN MEDIAL HA
unicode_combining_class(0x103F, 0x103F, 0).	% Lo       MYANMAR LETTER GREAT SA
unicode_combining_class(0x1040, 0x1049, 0).	% Nd  [10] MYANMAR DIGIT ZERO..MYANMAR DIGIT NINE
unicode_combining_class(0x104A, 0x104F, 0).	% Po   [6] MYANMAR SIGN LITTLE SECTION..MYANMAR SYMBOL GENITIVE
unicode_combining_class(0x1050, 0x1055, 0).	% Lo   [6] MYANMAR LETTER SHA..MYANMAR LETTER VOCALIC LL
unicode_combining_class(0x1056, 0x1057, 0).	% Mc   [2] MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR
unicode_combining_class(0x1058, 0x1059, 0).	% Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
unicode_combining_class(0x105A, 0x105D, 0).	% Lo   [4] MYANMAR LETTER MON NGA..MYANMAR LETTER MON BBE
unicode_combining_class(0x105E, 0x1060, 0).	% Mn   [3] MYANMAR CONSONANT SIGN MON MEDIAL NA..MYANMAR CONSONANT SIGN MON MEDIAL LA
unicode_combining_class(0x1061, 0x1061, 0).	% Lo       MYANMAR LETTER SGAW KAREN SHA
unicode_combining_class(0x1062, 0x1064, 0).	% Mc   [3] MYANMAR VOWEL SIGN SGAW KAREN EU..MYANMAR TONE MARK SGAW KAREN KE PHO
unicode_combining_class(0x1065, 0x1066, 0).	% Lo   [2] MYANMAR LETTER WESTERN PWO KAREN THA..MYANMAR LETTER WESTERN PWO KAREN PWA
unicode_combining_class(0x1067, 0x106D, 0).	% Mc   [7] MYANMAR VOWEL SIGN WESTERN PWO KAREN EU..MYANMAR SIGN WESTERN PWO KAREN TONE-5
unicode_combining_class(0x106E, 0x1070, 0).	% Lo   [3] MYANMAR LETTER EASTERN PWO KAREN NNA..MYANMAR LETTER EASTERN PWO KAREN GHWA
unicode_combining_class(0x1071, 0x1074, 0).	% Mn   [4] MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE
unicode_combining_class(0x1075, 0x1081, 0).	% Lo  [13] MYANMAR LETTER SHAN KA..MYANMAR LETTER SHAN HA
unicode_combining_class(0x1082, 0x1082, 0).	% Mn       MYANMAR CONSONANT SIGN SHAN MEDIAL WA
unicode_combining_class(0x1083, 0x1084, 0).	% Mc   [2] MYANMAR VOWEL SIGN SHAN AA..MYANMAR VOWEL SIGN SHAN E
unicode_combining_class(0x1085, 0x1086, 0).	% Mn   [2] MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y
unicode_combining_class(0x1087, 0x108C, 0).	% Mc   [6] MYANMAR SIGN SHAN TONE-2..MYANMAR SIGN SHAN COUNCIL TONE-3
unicode_combining_class(0x108E, 0x108E, 0).	% Lo       MYANMAR LETTER RUMAI PALAUNG FA
unicode_combining_class(0x108F, 0x108F, 0).	% Mc       MYANMAR SIGN RUMAI PALAUNG TONE-5
unicode_combining_class(0x1090, 0x1099, 0).	% Nd  [10] MYANMAR SHAN DIGIT ZERO..MYANMAR SHAN DIGIT NINE
unicode_combining_class(0x109A, 0x109C, 0).	% Mc   [3] MYANMAR SIGN KHAMTI TONE-1..MYANMAR VOWEL SIGN AITON A
unicode_combining_class(0x109D, 0x109D, 0).	% Mn       MYANMAR VOWEL SIGN AITON AI
unicode_combining_class(0x109E, 0x109F, 0).	% So   [2] MYANMAR SYMBOL SHAN ONE..MYANMAR SYMBOL SHAN EXCLAMATION
unicode_combining_class(0x10A0, 0x10C5, 0).	% L&  [38] GEORGIAN CAPITAL LETTER AN..GEORGIAN CAPITAL LETTER HOE
unicode_combining_class(0x10C7, 0x10C7, 0).	% L&       GEORGIAN CAPITAL LETTER YN
unicode_combining_class(0x10CD, 0x10CD, 0).	% L&       GEORGIAN CAPITAL LETTER AEN
unicode_combining_class(0x10D0, 0x10FA, 0).	% Lo  [43] GEORGIAN LETTER AN..GEORGIAN LETTER AIN
unicode_combining_class(0x10FB, 0x10FB, 0).	% Po       GEORGIAN PARAGRAPH SEPARATOR
unicode_combining_class(0x10FC, 0x10FC, 0).	% Lm       MODIFIER LETTER GEORGIAN NAR
unicode_combining_class(0x10FD, 0x1248, 0).	% Lo [332] GEORGIAN LETTER AEN..ETHIOPIC SYLLABLE QWA
unicode_combining_class(0x124A, 0x124D, 0).	% Lo   [4] ETHIOPIC SYLLABLE QWI..ETHIOPIC SYLLABLE QWE
unicode_combining_class(0x1250, 0x1256, 0).	% Lo   [7] ETHIOPIC SYLLABLE QHA..ETHIOPIC SYLLABLE QHO
unicode_combining_class(0x1258, 0x1258, 0).	% Lo       ETHIOPIC SYLLABLE QHWA
unicode_combining_class(0x125A, 0x125D, 0).	% Lo   [4] ETHIOPIC SYLLABLE QHWI..ETHIOPIC SYLLABLE QHWE
unicode_combining_class(0x1260, 0x1288, 0).	% Lo  [41] ETHIOPIC SYLLABLE BA..ETHIOPIC SYLLABLE XWA
unicode_combining_class(0x128A, 0x128D, 0).	% Lo   [4] ETHIOPIC SYLLABLE XWI..ETHIOPIC SYLLABLE XWE
unicode_combining_class(0x1290, 0x12B0, 0).	% Lo  [33] ETHIOPIC SYLLABLE NA..ETHIOPIC SYLLABLE KWA
unicode_combining_class(0x12B2, 0x12B5, 0).	% Lo   [4] ETHIOPIC SYLLABLE KWI..ETHIOPIC SYLLABLE KWE
unicode_combining_class(0x12B8, 0x12BE, 0).	% Lo   [7] ETHIOPIC SYLLABLE KXA..ETHIOPIC SYLLABLE KXO
unicode_combining_class(0x12C0, 0x12C0, 0).	% Lo       ETHIOPIC SYLLABLE KXWA
unicode_combining_class(0x12C2, 0x12C5, 0).	% Lo   [4] ETHIOPIC SYLLABLE KXWI..ETHIOPIC SYLLABLE KXWE
unicode_combining_class(0x12C8, 0x12D6, 0).	% Lo  [15] ETHIOPIC SYLLABLE WA..ETHIOPIC SYLLABLE PHARYNGEAL O
unicode_combining_class(0x12D8, 0x1310, 0).	% Lo  [57] ETHIOPIC SYLLABLE ZA..ETHIOPIC SYLLABLE GWA
unicode_combining_class(0x1312, 0x1315, 0).	% Lo   [4] ETHIOPIC SYLLABLE GWI..ETHIOPIC SYLLABLE GWE
unicode_combining_class(0x1318, 0x135A, 0).	% Lo  [67] ETHIOPIC SYLLABLE GGA..ETHIOPIC SYLLABLE FYA
unicode_combining_class(0x1360, 0x1368, 0).	% Po   [9] ETHIOPIC SECTION MARK..ETHIOPIC PARAGRAPH SEPARATOR
unicode_combining_class(0x1369, 0x137C, 0).	% No  [20] ETHIOPIC DIGIT ONE..ETHIOPIC NUMBER TEN THOUSAND
unicode_combining_class(0x1380, 0x138F, 0).	% Lo  [16] ETHIOPIC SYLLABLE SEBATBEIT MWA..ETHIOPIC SYLLABLE PWE
unicode_combining_class(0x1390, 0x1399, 0).	% So  [10] ETHIOPIC TONAL MARK YIZET..ETHIOPIC TONAL MARK KURT
unicode_combining_class(0x13A0, 0x13F4, 0).	% Lo  [85] CHEROKEE LETTER A..CHEROKEE LETTER YV
unicode_combining_class(0x1400, 0x1400, 0).	% Pd       CANADIAN SYLLABICS HYPHEN
unicode_combining_class(0x1401, 0x166C, 0).	% Lo [620] CANADIAN SYLLABICS E..CANADIAN SYLLABICS CARRIER TTSA
unicode_combining_class(0x166D, 0x166E, 0).	% Po   [2] CANADIAN SYLLABICS CHI SIGN..CANADIAN SYLLABICS FULL STOP
unicode_combining_class(0x166F, 0x167F, 0).	% Lo  [17] CANADIAN SYLLABICS QAI..CANADIAN SYLLABICS BLACKFOOT W
unicode_combining_class(0x1680, 0x1680, 0).	% Zs       OGHAM SPACE MARK
unicode_combining_class(0x1681, 0x169A, 0).	% Lo  [26] OGHAM LETTER BEITH..OGHAM LETTER PEITH
unicode_combining_class(0x169B, 0x169B, 0).	% Ps       OGHAM FEATHER MARK
unicode_combining_class(0x169C, 0x169C, 0).	% Pe       OGHAM REVERSED FEATHER MARK
unicode_combining_class(0x16A0, 0x16EA, 0).	% Lo  [75] RUNIC LETTER FEHU FEOH FE F..RUNIC LETTER X
unicode_combining_class(0x16EB, 0x16ED, 0).	% Po   [3] RUNIC SINGLE PUNCTUATION..RUNIC CROSS PUNCTUATION
unicode_combining_class(0x16EE, 0x16F0, 0).	% Nl   [3] RUNIC ARLAUG SYMBOL..RUNIC BELGTHOR SYMBOL
unicode_combining_class(0x1700, 0x170C, 0).	% Lo  [13] TAGALOG LETTER A..TAGALOG LETTER YA
unicode_combining_class(0x170E, 0x1711, 0).	% Lo   [4] TAGALOG LETTER LA..TAGALOG LETTER HA
unicode_combining_class(0x1712, 0x1713, 0).	% Mn   [2] TAGALOG VOWEL SIGN I..TAGALOG VOWEL SIGN U
unicode_combining_class(0x1720, 0x1731, 0).	% Lo  [18] HANUNOO LETTER A..HANUNOO LETTER HA
unicode_combining_class(0x1732, 0x1733, 0).	% Mn   [2] HANUNOO VOWEL SIGN I..HANUNOO VOWEL SIGN U
unicode_combining_class(0x1735, 0x1736, 0).	% Po   [2] PHILIPPINE SINGLE PUNCTUATION..PHILIPPINE DOUBLE PUNCTUATION
unicode_combining_class(0x1740, 0x1751, 0).	% Lo  [18] BUHID LETTER A..BUHID LETTER HA
unicode_combining_class(0x1752, 0x1753, 0).	% Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
unicode_combining_class(0x1760, 0x176C, 0).	% Lo  [13] TAGBANWA LETTER A..TAGBANWA LETTER YA
unicode_combining_class(0x176E, 0x1770, 0).	% Lo   [3] TAGBANWA LETTER LA..TAGBANWA LETTER SA
unicode_combining_class(0x1772, 0x1773, 0).	% Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
unicode_combining_class(0x1780, 0x17B3, 0).	% Lo  [52] KHMER LETTER KA..KHMER INDEPENDENT VOWEL QAU
unicode_combining_class(0x17B4, 0x17B5, 0).	% Mn   [2] KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA
unicode_combining_class(0x17B6, 0x17B6, 0).	% Mc       KHMER VOWEL SIGN AA
unicode_combining_class(0x17B7, 0x17BD, 0).	% Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
unicode_combining_class(0x17BE, 0x17C5, 0).	% Mc   [8] KHMER VOWEL SIGN OE..KHMER VOWEL SIGN AU
unicode_combining_class(0x17C6, 0x17C6, 0).	% Mn       KHMER SIGN NIKAHIT
unicode_combining_class(0x17C7, 0x17C8, 0).	% Mc   [2] KHMER SIGN REAHMUK..KHMER SIGN YUUKALEAPINTU
unicode_combining_class(0x17C9, 0x17D1, 0).	% Mn   [9] KHMER SIGN MUUSIKATOAN..KHMER SIGN VIRIAM
unicode_combining_class(0x17D3, 0x17D3, 0).	% Mn       KHMER SIGN BATHAMASAT
unicode_combining_class(0x17D4, 0x17D6, 0).	% Po   [3] KHMER SIGN KHAN..KHMER SIGN CAMNUC PII KUUH
unicode_combining_class(0x17D7, 0x17D7, 0).	% Lm       KHMER SIGN LEK TOO
unicode_combining_class(0x17D8, 0x17DA, 0).	% Po   [3] KHMER SIGN BEYYAL..KHMER SIGN KOOMUUT
unicode_combining_class(0x17DB, 0x17DB, 0).	% Sc       KHMER CURRENCY SYMBOL RIEL
unicode_combining_class(0x17DC, 0x17DC, 0).	% Lo       KHMER SIGN AVAKRAHASANYA
unicode_combining_class(0x17E0, 0x17E9, 0).	% Nd  [10] KHMER DIGIT ZERO..KHMER DIGIT NINE
unicode_combining_class(0x17F0, 0x17F9, 0).	% No  [10] KHMER SYMBOL LEK ATTAK SON..KHMER SYMBOL LEK ATTAK PRAM-BUON
unicode_combining_class(0x1800, 0x1805, 0).	% Po   [6] MONGOLIAN BIRGA..MONGOLIAN FOUR DOTS
unicode_combining_class(0x1806, 0x1806, 0).	% Pd       MONGOLIAN TODO SOFT HYPHEN
unicode_combining_class(0x1807, 0x180A, 0).	% Po   [4] MONGOLIAN SIBE SYLLABLE BOUNDARY MARKER..MONGOLIAN NIRUGU
unicode_combining_class(0x180B, 0x180D, 0).	% Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
unicode_combining_class(0x180E, 0x180E, 0).	% Zs       MONGOLIAN VOWEL SEPARATOR
unicode_combining_class(0x1810, 0x1819, 0).	% Nd  [10] MONGOLIAN DIGIT ZERO..MONGOLIAN DIGIT NINE
unicode_combining_class(0x1820, 0x1842, 0).	% Lo  [35] MONGOLIAN LETTER A..MONGOLIAN LETTER CHI
unicode_combining_class(0x1843, 0x1843, 0).	% Lm       MONGOLIAN LETTER TODO LONG VOWEL SIGN
unicode_combining_class(0x1844, 0x1877, 0).	% Lo  [52] MONGOLIAN LETTER TODO E..MONGOLIAN LETTER MANCHU ZHA
unicode_combining_class(0x1880, 0x18A8, 0).	% Lo  [41] MONGOLIAN LETTER ALI GALI ANUSVARA ONE..MONGOLIAN LETTER MANCHU ALI GALI BHA
unicode_combining_class(0x18AA, 0x18AA, 0).	% Lo       MONGOLIAN LETTER MANCHU ALI GALI LHA
unicode_combining_class(0x18B0, 0x18F5, 0).	% Lo  [70] CANADIAN SYLLABICS OY..CANADIAN SYLLABICS CARRIER DENTAL S
unicode_combining_class(0x1900, 0x191C, 0).	% Lo  [29] LIMBU VOWEL-CARRIER LETTER..LIMBU LETTER HA
unicode_combining_class(0x1920, 0x1922, 0).	% Mn   [3] LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN U
unicode_combining_class(0x1923, 0x1926, 0).	% Mc   [4] LIMBU VOWEL SIGN EE..LIMBU VOWEL SIGN AU
unicode_combining_class(0x1927, 0x1928, 0).	% Mn   [2] LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O
unicode_combining_class(0x1929, 0x192B, 0).	% Mc   [3] LIMBU SUBJOINED LETTER YA..LIMBU SUBJOINED LETTER WA
unicode_combining_class(0x1930, 0x1931, 0).	% Mc   [2] LIMBU SMALL LETTER KA..LIMBU SMALL LETTER NGA
unicode_combining_class(0x1932, 0x1932, 0).	% Mn       LIMBU SMALL LETTER ANUSVARA
unicode_combining_class(0x1933, 0x1938, 0).	% Mc   [6] LIMBU SMALL LETTER TA..LIMBU SMALL LETTER LA
unicode_combining_class(0x1940, 0x1940, 0).	% So       LIMBU SIGN LOO
unicode_combining_class(0x1944, 0x1945, 0).	% Po   [2] LIMBU EXCLAMATION MARK..LIMBU QUESTION MARK
unicode_combining_class(0x1946, 0x194F, 0).	% Nd  [10] LIMBU DIGIT ZERO..LIMBU DIGIT NINE
unicode_combining_class(0x1950, 0x196D, 0).	% Lo  [30] TAI LE LETTER KA..TAI LE LETTER AI
unicode_combining_class(0x1970, 0x1974, 0).	% Lo   [5] TAI LE LETTER TONE-2..TAI LE LETTER TONE-6
unicode_combining_class(0x1980, 0x19AB, 0).	% Lo  [44] NEW TAI LUE LETTER HIGH QA..NEW TAI LUE LETTER LOW SUA
unicode_combining_class(0x19B0, 0x19C0, 0).	% Mc  [17] NEW TAI LUE VOWEL SIGN VOWEL SHORTENER..NEW TAI LUE VOWEL SIGN IY
unicode_combining_class(0x19C1, 0x19C7, 0).	% Lo   [7] NEW TAI LUE LETTER FINAL V..NEW TAI LUE LETTER FINAL B
unicode_combining_class(0x19C8, 0x19C9, 0).	% Mc   [2] NEW TAI LUE TONE MARK-1..NEW TAI LUE TONE MARK-2
unicode_combining_class(0x19D0, 0x19D9, 0).	% Nd  [10] NEW TAI LUE DIGIT ZERO..NEW TAI LUE DIGIT NINE
unicode_combining_class(0x19DA, 0x19DA, 0).	% No       NEW TAI LUE THAM DIGIT ONE
unicode_combining_class(0x19DE, 0x19FF, 0).	% So  [34] NEW TAI LUE SIGN LAE..KHMER SYMBOL DAP-PRAM ROC
unicode_combining_class(0x1A00, 0x1A16, 0).	% Lo  [23] BUGINESE LETTER KA..BUGINESE LETTER HA
unicode_combining_class(0x1A19, 0x1A1B, 0).	% Mc   [3] BUGINESE VOWEL SIGN E..BUGINESE VOWEL SIGN AE
unicode_combining_class(0x1A1E, 0x1A1F, 0).	% Po   [2] BUGINESE PALLAWA..BUGINESE END OF SECTION
unicode_combining_class(0x1A20, 0x1A54, 0).	% Lo  [53] TAI THAM LETTER HIGH KA..TAI THAM LETTER GREAT SA
unicode_combining_class(0x1A55, 0x1A55, 0).	% Mc       TAI THAM CONSONANT SIGN MEDIAL RA
unicode_combining_class(0x1A56, 0x1A56, 0).	% Mn       TAI THAM CONSONANT SIGN MEDIAL LA
unicode_combining_class(0x1A57, 0x1A57, 0).	% Mc       TAI THAM CONSONANT SIGN LA TANG LAI
unicode_combining_class(0x1A58, 0x1A5E, 0).	% Mn   [7] TAI THAM SIGN MAI KANG LAI..TAI THAM CONSONANT SIGN SA
unicode_combining_class(0x1A61, 0x1A61, 0).	% Mc       TAI THAM VOWEL SIGN A
unicode_combining_class(0x1A62, 0x1A62, 0).	% Mn       TAI THAM VOWEL SIGN MAI SAT
unicode_combining_class(0x1A63, 0x1A64, 0).	% Mc   [2] TAI THAM VOWEL SIGN AA..TAI THAM VOWEL SIGN TALL AA
unicode_combining_class(0x1A65, 0x1A6C, 0).	% Mn   [8] TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN OA BELOW
unicode_combining_class(0x1A6D, 0x1A72, 0).	% Mc   [6] TAI THAM VOWEL SIGN OY..TAI THAM VOWEL SIGN THAM AI
unicode_combining_class(0x1A73, 0x1A74, 0).	% Mn   [2] TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN MAI KANG
unicode_combining_class(0x1A80, 0x1A89, 0).	% Nd  [10] TAI THAM HORA DIGIT ZERO..TAI THAM HORA DIGIT NINE
unicode_combining_class(0x1A90, 0x1A99, 0).	% Nd  [10] TAI THAM THAM DIGIT ZERO..TAI THAM THAM DIGIT NINE
unicode_combining_class(0x1AA0, 0x1AA6, 0).	% Po   [7] TAI THAM SIGN WIANG..TAI THAM SIGN REVERSED ROTATED RANA
unicode_combining_class(0x1AA7, 0x1AA7, 0).	% Lm       TAI THAM SIGN MAI YAMOK
unicode_combining_class(0x1AA8, 0x1AAD, 0).	% Po   [6] TAI THAM SIGN KAAN..TAI THAM SIGN CAANG
unicode_combining_class(0x1B00, 0x1B03, 0).	% Mn   [4] BALINESE SIGN ULU RICEM..BALINESE SIGN SURANG
unicode_combining_class(0x1B04, 0x1B04, 0).	% Mc       BALINESE SIGN BISAH
unicode_combining_class(0x1B05, 0x1B33, 0).	% Lo  [47] BALINESE LETTER AKARA..BALINESE LETTER HA
unicode_combining_class(0x1B35, 0x1B35, 0).	% Mc       BALINESE VOWEL SIGN TEDUNG
unicode_combining_class(0x1B36, 0x1B3A, 0).	% Mn   [5] BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN RA REPA
unicode_combining_class(0x1B3B, 0x1B3B, 0).	% Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_combining_class(0x1B3C, 0x1B3C, 0).	% Mn       BALINESE VOWEL SIGN LA LENGA
unicode_combining_class(0x1B3D, 0x1B41, 0).	% Mc   [5] BALINESE VOWEL SIGN LA LENGA TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG
unicode_combining_class(0x1B42, 0x1B42, 0).	% Mn       BALINESE VOWEL SIGN PEPET
unicode_combining_class(0x1B43, 0x1B43, 0).	% Mc       BALINESE VOWEL SIGN PEPET TEDUNG
unicode_combining_class(0x1B45, 0x1B4B, 0).	% Lo   [7] BALINESE LETTER KAF SASAK..BALINESE LETTER ASYURA SASAK
unicode_combining_class(0x1B50, 0x1B59, 0).	% Nd  [10] BALINESE DIGIT ZERO..BALINESE DIGIT NINE
unicode_combining_class(0x1B5A, 0x1B60, 0).	% Po   [7] BALINESE PANTI..BALINESE PAMENENG
unicode_combining_class(0x1B61, 0x1B6A, 0).	% So  [10] BALINESE MUSICAL SYMBOL DONG..BALINESE MUSICAL SYMBOL DANG GEDE
unicode_combining_class(0x1B74, 0x1B7C, 0).	% So   [9] BALINESE MUSICAL SYMBOL RIGHT-HAND OPEN DUG..BALINESE MUSICAL SYMBOL LEFT-HAND OPEN PING
unicode_combining_class(0x1B80, 0x1B81, 0).	% Mn   [2] SUNDANESE SIGN PANYECEK..SUNDANESE SIGN PANGLAYAR
unicode_combining_class(0x1B82, 0x1B82, 0).	% Mc       SUNDANESE SIGN PANGWISAD
unicode_combining_class(0x1B83, 0x1BA0, 0).	% Lo  [30] SUNDANESE LETTER A..SUNDANESE LETTER HA
unicode_combining_class(0x1BA1, 0x1BA1, 0).	% Mc       SUNDANESE CONSONANT SIGN PAMINGKAL
unicode_combining_class(0x1BA2, 0x1BA5, 0).	% Mn   [4] SUNDANESE CONSONANT SIGN PANYAKRA..SUNDANESE VOWEL SIGN PANYUKU
unicode_combining_class(0x1BA6, 0x1BA7, 0).	% Mc   [2] SUNDANESE VOWEL SIGN PANAELAENG..SUNDANESE VOWEL SIGN PANOLONG
unicode_combining_class(0x1BA8, 0x1BA9, 0).	% Mn   [2] SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG
unicode_combining_class(0x1BAC, 0x1BAD, 0).	% Mc   [2] SUNDANESE CONSONANT SIGN PASANGAN MA..SUNDANESE CONSONANT SIGN PASANGAN WA
unicode_combining_class(0x1BAE, 0x1BAF, 0).	% Lo   [2] SUNDANESE LETTER KHA..SUNDANESE LETTER SYA
unicode_combining_class(0x1BB0, 0x1BB9, 0).	% Nd  [10] SUNDANESE DIGIT ZERO..SUNDANESE DIGIT NINE
unicode_combining_class(0x1BBA, 0x1BE5, 0).	% Lo  [44] SUNDANESE AVAGRAHA..BATAK LETTER U
unicode_combining_class(0x1BE7, 0x1BE7, 0).	% Mc       BATAK VOWEL SIGN E
unicode_combining_class(0x1BE8, 0x1BE9, 0).	% Mn   [2] BATAK VOWEL SIGN PAKPAK E..BATAK VOWEL SIGN EE
unicode_combining_class(0x1BEA, 0x1BEC, 0).	% Mc   [3] BATAK VOWEL SIGN I..BATAK VOWEL SIGN O
unicode_combining_class(0x1BED, 0x1BED, 0).	% Mn       BATAK VOWEL SIGN KARO O
unicode_combining_class(0x1BEE, 0x1BEE, 0).	% Mc       BATAK VOWEL SIGN U
unicode_combining_class(0x1BEF, 0x1BF1, 0).	% Mn   [3] BATAK VOWEL SIGN U FOR SIMALUNGUN SA..BATAK CONSONANT SIGN H
unicode_combining_class(0x1BFC, 0x1BFF, 0).	% Po   [4] BATAK SYMBOL BINDU NA METEK..BATAK SYMBOL BINDU PANGOLAT
unicode_combining_class(0x1C00, 0x1C23, 0).	% Lo  [36] LEPCHA LETTER KA..LEPCHA LETTER A
unicode_combining_class(0x1C24, 0x1C2B, 0).	% Mc   [8] LEPCHA SUBJOINED LETTER YA..LEPCHA VOWEL SIGN UU
unicode_combining_class(0x1C2C, 0x1C33, 0).	% Mn   [8] LEPCHA VOWEL SIGN E..LEPCHA CONSONANT SIGN T
unicode_combining_class(0x1C34, 0x1C35, 0).	% Mc   [2] LEPCHA CONSONANT SIGN NYIN-DO..LEPCHA CONSONANT SIGN KANG
unicode_combining_class(0x1C36, 0x1C36, 0).	% Mn       LEPCHA SIGN RAN
unicode_combining_class(0x1C3B, 0x1C3F, 0).	% Po   [5] LEPCHA PUNCTUATION TA-ROL..LEPCHA PUNCTUATION TSHOOK
unicode_combining_class(0x1C40, 0x1C49, 0).	% Nd  [10] LEPCHA DIGIT ZERO..LEPCHA DIGIT NINE
unicode_combining_class(0x1C4D, 0x1C4F, 0).	% Lo   [3] LEPCHA LETTER TTA..LEPCHA LETTER DDA
unicode_combining_class(0x1C50, 0x1C59, 0).	% Nd  [10] OL CHIKI DIGIT ZERO..OL CHIKI DIGIT NINE
unicode_combining_class(0x1C5A, 0x1C77, 0).	% Lo  [30] OL CHIKI LETTER LA..OL CHIKI LETTER OH
unicode_combining_class(0x1C78, 0x1C7D, 0).	% Lm   [6] OL CHIKI MU TTUDDAG..OL CHIKI AHAD
unicode_combining_class(0x1C7E, 0x1C7F, 0).	% Po   [2] OL CHIKI PUNCTUATION MUCAAD..OL CHIKI PUNCTUATION DOUBLE MUCAAD
unicode_combining_class(0x1CC0, 0x1CC7, 0).	% Po   [8] SUNDANESE PUNCTUATION BINDU SURYA..SUNDANESE PUNCTUATION BINDU BA SATANGA
unicode_combining_class(0x1CD3, 0x1CD3, 0).	% Po       VEDIC SIGN NIHSHVASA
unicode_combining_class(0x1CE1, 0x1CE1, 0).	% Mc       VEDIC TONE ATHARVAVEDIC INDEPENDENT SVARITA
unicode_combining_class(0x1CE9, 0x1CEC, 0).	% Lo   [4] VEDIC SIGN ANUSVARA ANTARGOMUKHA..VEDIC SIGN ANUSVARA VAMAGOMUKHA WITH TAIL
unicode_combining_class(0x1CEE, 0x1CF1, 0).	% Lo   [4] VEDIC SIGN HEXIFORM LONG ANUSVARA..VEDIC SIGN ANUSVARA UBHAYATO MUKHA
unicode_combining_class(0x1CF2, 0x1CF3, 0).	% Mc   [2] VEDIC SIGN ARDHAVISARGA..VEDIC SIGN ROTATED ARDHAVISARGA
unicode_combining_class(0x1CF5, 0x1CF6, 0).	% Lo   [2] VEDIC SIGN JIHVAMULIYA..VEDIC SIGN UPADHMANIYA
unicode_combining_class(0x1D00, 0x1D2B, 0).	% L&  [44] LATIN LETTER SMALL CAPITAL A..CYRILLIC LETTER SMALL CAPITAL EL
unicode_combining_class(0x1D2C, 0x1D6A, 0).	% Lm  [63] MODIFIER LETTER CAPITAL A..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_combining_class(0x1D6B, 0x1D77, 0).	% L&  [13] LATIN SMALL LETTER UE..LATIN SMALL LETTER TURNED G
unicode_combining_class(0x1D78, 0x1D78, 0).	% Lm       MODIFIER LETTER CYRILLIC EN
unicode_combining_class(0x1D79, 0x1D9A, 0).	% L&  [34] LATIN SMALL LETTER INSULAR G..LATIN SMALL LETTER EZH WITH RETROFLEX HOOK
unicode_combining_class(0x1D9B, 0x1DBF, 0).	% Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_combining_class(0x1E00, 0x1F15, 0).	% L& [278] LATIN CAPITAL LETTER A WITH RING BELOW..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_combining_class(0x1F18, 0x1F1D, 0).	% L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_combining_class(0x1F20, 0x1F45, 0).	% L&  [38] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_combining_class(0x1F48, 0x1F4D, 0).	% L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_combining_class(0x1F50, 0x1F57, 0).	% L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_combining_class(0x1F59, 0x1F59, 0).	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_combining_class(0x1F5B, 0x1F5B, 0).	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_combining_class(0x1F5D, 0x1F5D, 0).	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_combining_class(0x1F5F, 0x1F7D, 0).	% L&  [31] GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_combining_class(0x1F80, 0x1FB4, 0).	% L&  [53] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_combining_class(0x1FB6, 0x1FBC, 0).	% L&   [7] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_combining_class(0x1FBD, 0x1FBD, 0).	% Sk       GREEK KORONIS
unicode_combining_class(0x1FBE, 0x1FBE, 0).	% L&       GREEK PROSGEGRAMMENI
unicode_combining_class(0x1FBF, 0x1FC1, 0).	% Sk   [3] GREEK PSILI..GREEK DIALYTIKA AND PERISPOMENI
unicode_combining_class(0x1FC2, 0x1FC4, 0).	% L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_combining_class(0x1FC6, 0x1FCC, 0).	% L&   [7] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_combining_class(0x1FCD, 0x1FCF, 0).	% Sk   [3] GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI
unicode_combining_class(0x1FD0, 0x1FD3, 0).	% L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_combining_class(0x1FD6, 0x1FDB, 0).	% L&   [6] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_combining_class(0x1FDD, 0x1FDF, 0).	% Sk   [3] GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI
unicode_combining_class(0x1FE0, 0x1FEC, 0).	% L&  [13] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_combining_class(0x1FED, 0x1FEF, 0).	% Sk   [3] GREEK DIALYTIKA AND VARIA..GREEK VARIA
unicode_combining_class(0x1FF2, 0x1FF4, 0).	% L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_combining_class(0x1FF6, 0x1FFC, 0).	% L&   [7] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_combining_class(0x1FFD, 0x1FFE, 0).	% Sk   [2] GREEK OXIA..GREEK DASIA
unicode_combining_class(0x2000, 0x200A, 0).	% Zs  [11] EN QUAD..HAIR SPACE
unicode_combining_class(0x200B, 0x200F, 0).	% Cf   [5] ZERO WIDTH SPACE..RIGHT-TO-LEFT MARK
unicode_combining_class(0x2010, 0x2015, 0).	% Pd   [6] HYPHEN..HORIZONTAL BAR
unicode_combining_class(0x2016, 0x2017, 0).	% Po   [2] DOUBLE VERTICAL LINE..DOUBLE LOW LINE
unicode_combining_class(0x2018, 0x2018, 0).	% Pi       LEFT SINGLE QUOTATION MARK
unicode_combining_class(0x2019, 0x2019, 0).	% Pf       RIGHT SINGLE QUOTATION MARK
unicode_combining_class(0x201A, 0x201A, 0).	% Ps       SINGLE LOW-9 QUOTATION MARK
unicode_combining_class(0x201B, 0x201C, 0).	% Pi   [2] SINGLE HIGH-REVERSED-9 QUOTATION MARK..LEFT DOUBLE QUOTATION MARK
unicode_combining_class(0x201D, 0x201D, 0).	% Pf       RIGHT DOUBLE QUOTATION MARK
unicode_combining_class(0x201E, 0x201E, 0).	% Ps       DOUBLE LOW-9 QUOTATION MARK
unicode_combining_class(0x201F, 0x201F, 0).	% Pi       DOUBLE HIGH-REVERSED-9 QUOTATION MARK
unicode_combining_class(0x2020, 0x2027, 0).	% Po   [8] DAGGER..HYPHENATION POINT
unicode_combining_class(0x2028, 0x2028, 0).	% Zl       LINE SEPARATOR
unicode_combining_class(0x2029, 0x2029, 0).	% Zp       PARAGRAPH SEPARATOR
unicode_combining_class(0x202A, 0x202E, 0).	% Cf   [5] LEFT-TO-RIGHT EMBEDDING..RIGHT-TO-LEFT OVERRIDE
unicode_combining_class(0x202F, 0x202F, 0).	% Zs       NARROW NO-BREAK SPACE
unicode_combining_class(0x2030, 0x2038, 0).	% Po   [9] PER MILLE SIGN..CARET
unicode_combining_class(0x2039, 0x2039, 0).	% Pi       SINGLE LEFT-POINTING ANGLE QUOTATION MARK
unicode_combining_class(0x203A, 0x203A, 0).	% Pf       SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
unicode_combining_class(0x203B, 0x203E, 0).	% Po   [4] REFERENCE MARK..OVERLINE
unicode_combining_class(0x203F, 0x2040, 0).	% Pc   [2] UNDERTIE..CHARACTER TIE
unicode_combining_class(0x2041, 0x2043, 0).	% Po   [3] CARET INSERTION POINT..HYPHEN BULLET
unicode_combining_class(0x2044, 0x2044, 0).	% Sm       FRACTION SLASH
unicode_combining_class(0x2045, 0x2045, 0).	% Ps       LEFT SQUARE BRACKET WITH QUILL
unicode_combining_class(0x2046, 0x2046, 0).	% Pe       RIGHT SQUARE BRACKET WITH QUILL
unicode_combining_class(0x2047, 0x2051, 0).	% Po  [11] DOUBLE QUESTION MARK..TWO ASTERISKS ALIGNED VERTICALLY
unicode_combining_class(0x2052, 0x2052, 0).	% Sm       COMMERCIAL MINUS SIGN
unicode_combining_class(0x2053, 0x2053, 0).	% Po       SWUNG DASH
unicode_combining_class(0x2054, 0x2054, 0).	% Pc       INVERTED UNDERTIE
unicode_combining_class(0x2055, 0x205E, 0).	% Po  [10] FLOWER PUNCTUATION MARK..VERTICAL FOUR DOTS
unicode_combining_class(0x205F, 0x205F, 0).	% Zs       MEDIUM MATHEMATICAL SPACE
unicode_combining_class(0x2060, 0x2064, 0).	% Cf   [5] WORD JOINER..INVISIBLE PLUS
unicode_combining_class(0x206A, 0x206F, 0).	% Cf   [6] INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES
unicode_combining_class(0x2070, 0x2070, 0).	% No       SUPERSCRIPT ZERO
unicode_combining_class(0x2071, 0x2071, 0).	% Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_combining_class(0x2074, 0x2079, 0).	% No   [6] SUPERSCRIPT FOUR..SUPERSCRIPT NINE
unicode_combining_class(0x207A, 0x207C, 0).	% Sm   [3] SUPERSCRIPT PLUS SIGN..SUPERSCRIPT EQUALS SIGN
unicode_combining_class(0x207D, 0x207D, 0).	% Ps       SUPERSCRIPT LEFT PARENTHESIS
unicode_combining_class(0x207E, 0x207E, 0).	% Pe       SUPERSCRIPT RIGHT PARENTHESIS
unicode_combining_class(0x207F, 0x207F, 0).	% Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_combining_class(0x2080, 0x2089, 0).	% No  [10] SUBSCRIPT ZERO..SUBSCRIPT NINE
unicode_combining_class(0x208A, 0x208C, 0).	% Sm   [3] SUBSCRIPT PLUS SIGN..SUBSCRIPT EQUALS SIGN
unicode_combining_class(0x208D, 0x208D, 0).	% Ps       SUBSCRIPT LEFT PARENTHESIS
unicode_combining_class(0x208E, 0x208E, 0).	% Pe       SUBSCRIPT RIGHT PARENTHESIS
unicode_combining_class(0x2090, 0x209C, 0).	% Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_combining_class(0x20A0, 0x20BA, 0).	% Sc  [27] EURO-CURRENCY SIGN..TURKISH LIRA SIGN
unicode_combining_class(0x20DD, 0x20E0, 0).	% Me   [4] COMBINING ENCLOSING CIRCLE..COMBINING ENCLOSING CIRCLE BACKSLASH
unicode_combining_class(0x20E2, 0x20E4, 0).	% Me   [3] COMBINING ENCLOSING SCREEN..COMBINING ENCLOSING UPWARD POINTING TRIANGLE
unicode_combining_class(0x2100, 0x2101, 0).	% So   [2] ACCOUNT OF..ADDRESSED TO THE SUBJECT
unicode_combining_class(0x2102, 0x2102, 0).	% L&       DOUBLE-STRUCK CAPITAL C
unicode_combining_class(0x2103, 0x2106, 0).	% So   [4] DEGREE CELSIUS..CADA UNA
unicode_combining_class(0x2107, 0x2107, 0).	% L&       EULER CONSTANT
unicode_combining_class(0x2108, 0x2109, 0).	% So   [2] SCRUPLE..DEGREE FAHRENHEIT
unicode_combining_class(0x210A, 0x2113, 0).	% L&  [10] SCRIPT SMALL G..SCRIPT SMALL L
unicode_combining_class(0x2114, 0x2114, 0).	% So       L B BAR SYMBOL
unicode_combining_class(0x2115, 0x2115, 0).	% L&       DOUBLE-STRUCK CAPITAL N
unicode_combining_class(0x2116, 0x2117, 0).	% So   [2] NUMERO SIGN..SOUND RECORDING COPYRIGHT
unicode_combining_class(0x2118, 0x2118, 0).	% Sm       SCRIPT CAPITAL P
unicode_combining_class(0x2119, 0x211D, 0).	% L&   [5] DOUBLE-STRUCK CAPITAL P..DOUBLE-STRUCK CAPITAL R
unicode_combining_class(0x211E, 0x2123, 0).	% So   [6] PRESCRIPTION TAKE..VERSICLE
unicode_combining_class(0x2124, 0x2124, 0).	% L&       DOUBLE-STRUCK CAPITAL Z
unicode_combining_class(0x2125, 0x2125, 0).	% So       OUNCE SIGN
unicode_combining_class(0x2126, 0x2126, 0).	% L&       OHM SIGN
unicode_combining_class(0x2127, 0x2127, 0).	% So       INVERTED OHM SIGN
unicode_combining_class(0x2128, 0x2128, 0).	% L&       BLACK-LETTER CAPITAL Z
unicode_combining_class(0x2129, 0x2129, 0).	% So       TURNED GREEK SMALL LETTER IOTA
unicode_combining_class(0x212A, 0x212D, 0).	% L&   [4] KELVIN SIGN..BLACK-LETTER CAPITAL C
unicode_combining_class(0x212E, 0x212E, 0).	% So       ESTIMATED SYMBOL
unicode_combining_class(0x212F, 0x2134, 0).	% L&   [6] SCRIPT SMALL E..SCRIPT SMALL O
unicode_combining_class(0x2135, 0x2138, 0).	% Lo   [4] ALEF SYMBOL..DALET SYMBOL
unicode_combining_class(0x2139, 0x2139, 0).	% L&       INFORMATION SOURCE
unicode_combining_class(0x213A, 0x213B, 0).	% So   [2] ROTATED CAPITAL Q..FACSIMILE SIGN
unicode_combining_class(0x213C, 0x213F, 0).	% L&   [4] DOUBLE-STRUCK SMALL PI..DOUBLE-STRUCK CAPITAL PI
unicode_combining_class(0x2140, 0x2144, 0).	% Sm   [5] DOUBLE-STRUCK N-ARY SUMMATION..TURNED SANS-SERIF CAPITAL Y
unicode_combining_class(0x2145, 0x2149, 0).	% L&   [5] DOUBLE-STRUCK ITALIC CAPITAL D..DOUBLE-STRUCK ITALIC SMALL J
unicode_combining_class(0x214A, 0x214A, 0).	% So       PROPERTY LINE
unicode_combining_class(0x214B, 0x214B, 0).	% Sm       TURNED AMPERSAND
unicode_combining_class(0x214C, 0x214D, 0).	% So   [2] PER SIGN..AKTIESELSKAB
unicode_combining_class(0x214E, 0x214E, 0).	% L&       TURNED SMALL F
unicode_combining_class(0x214F, 0x214F, 0).	% So       SYMBOL FOR SAMARITAN SOURCE
unicode_combining_class(0x2150, 0x215F, 0).	% No  [16] VULGAR FRACTION ONE SEVENTH..FRACTION NUMERATOR ONE
unicode_combining_class(0x2160, 0x2182, 0).	% Nl  [35] ROMAN NUMERAL ONE..ROMAN NUMERAL TEN THOUSAND
unicode_combining_class(0x2183, 0x2184, 0).	% L&   [2] ROMAN NUMERAL REVERSED ONE HUNDRED..LATIN SMALL LETTER REVERSED C
unicode_combining_class(0x2185, 0x2188, 0).	% Nl   [4] ROMAN NUMERAL SIX LATE FORM..ROMAN NUMERAL ONE HUNDRED THOUSAND
unicode_combining_class(0x2189, 0x2189, 0).	% No       VULGAR FRACTION ZERO THIRDS
unicode_combining_class(0x2190, 0x2194, 0).	% Sm   [5] LEFTWARDS ARROW..LEFT RIGHT ARROW
unicode_combining_class(0x2195, 0x2199, 0).	% So   [5] UP DOWN ARROW..SOUTH WEST ARROW
unicode_combining_class(0x219A, 0x219B, 0).	% Sm   [2] LEFTWARDS ARROW WITH STROKE..RIGHTWARDS ARROW WITH STROKE
unicode_combining_class(0x219C, 0x219F, 0).	% So   [4] LEFTWARDS WAVE ARROW..UPWARDS TWO HEADED ARROW
unicode_combining_class(0x21A0, 0x21A0, 0).	% Sm       RIGHTWARDS TWO HEADED ARROW
unicode_combining_class(0x21A1, 0x21A2, 0).	% So   [2] DOWNWARDS TWO HEADED ARROW..LEFTWARDS ARROW WITH TAIL
unicode_combining_class(0x21A3, 0x21A3, 0).	% Sm       RIGHTWARDS ARROW WITH TAIL
unicode_combining_class(0x21A4, 0x21A5, 0).	% So   [2] LEFTWARDS ARROW FROM BAR..UPWARDS ARROW FROM BAR
unicode_combining_class(0x21A6, 0x21A6, 0).	% Sm       RIGHTWARDS ARROW FROM BAR
unicode_combining_class(0x21A7, 0x21AD, 0).	% So   [7] DOWNWARDS ARROW FROM BAR..LEFT RIGHT WAVE ARROW
unicode_combining_class(0x21AE, 0x21AE, 0).	% Sm       LEFT RIGHT ARROW WITH STROKE
unicode_combining_class(0x21AF, 0x21CD, 0).	% So  [31] DOWNWARDS ZIGZAG ARROW..LEFTWARDS DOUBLE ARROW WITH STROKE
unicode_combining_class(0x21CE, 0x21CF, 0).	% Sm   [2] LEFT RIGHT DOUBLE ARROW WITH STROKE..RIGHTWARDS DOUBLE ARROW WITH STROKE
unicode_combining_class(0x21D0, 0x21D1, 0).	% So   [2] LEFTWARDS DOUBLE ARROW..UPWARDS DOUBLE ARROW
unicode_combining_class(0x21D2, 0x21D2, 0).	% Sm       RIGHTWARDS DOUBLE ARROW
unicode_combining_class(0x21D3, 0x21D3, 0).	% So       DOWNWARDS DOUBLE ARROW
unicode_combining_class(0x21D4, 0x21D4, 0).	% Sm       LEFT RIGHT DOUBLE ARROW
unicode_combining_class(0x21D5, 0x21F3, 0).	% So  [31] UP DOWN DOUBLE ARROW..UP DOWN WHITE ARROW
unicode_combining_class(0x21F4, 0x22FF, 0).	% Sm [268] RIGHT ARROW WITH SMALL CIRCLE..Z NOTATION BAG MEMBERSHIP
unicode_combining_class(0x2300, 0x2307, 0).	% So   [8] DIAMETER SIGN..WAVY LINE
unicode_combining_class(0x2308, 0x230B, 0).	% Sm   [4] LEFT CEILING..RIGHT FLOOR
unicode_combining_class(0x230C, 0x231F, 0).	% So  [20] BOTTOM RIGHT CROP..BOTTOM RIGHT CORNER
unicode_combining_class(0x2320, 0x2321, 0).	% Sm   [2] TOP HALF INTEGRAL..BOTTOM HALF INTEGRAL
unicode_combining_class(0x2322, 0x2328, 0).	% So   [7] FROWN..KEYBOARD
unicode_combining_class(0x2329, 0x2329, 0).	% Ps       LEFT-POINTING ANGLE BRACKET
unicode_combining_class(0x232A, 0x232A, 0).	% Pe       RIGHT-POINTING ANGLE BRACKET
unicode_combining_class(0x232B, 0x237B, 0).	% So  [81] ERASE TO THE LEFT..NOT CHECK MARK
unicode_combining_class(0x237C, 0x237C, 0).	% Sm       RIGHT ANGLE WITH DOWNWARDS ZIGZAG ARROW
unicode_combining_class(0x237D, 0x239A, 0).	% So  [30] SHOULDERED OPEN BOX..CLEAR SCREEN SYMBOL
unicode_combining_class(0x239B, 0x23B3, 0).	% Sm  [25] LEFT PARENTHESIS UPPER HOOK..SUMMATION BOTTOM
unicode_combining_class(0x23B4, 0x23DB, 0).	% So  [40] TOP SQUARE BRACKET..FUSE
unicode_combining_class(0x23DC, 0x23E1, 0).	% Sm   [6] TOP PARENTHESIS..BOTTOM TORTOISE SHELL BRACKET
unicode_combining_class(0x23E2, 0x23F3, 0).	% So  [18] WHITE TRAPEZIUM..HOURGLASS WITH FLOWING SAND
unicode_combining_class(0x2400, 0x2426, 0).	% So  [39] SYMBOL FOR NULL..SYMBOL FOR SUBSTITUTE FORM TWO
unicode_combining_class(0x2440, 0x244A, 0).	% So  [11] OCR HOOK..OCR DOUBLE BACKSLASH
unicode_combining_class(0x2460, 0x249B, 0).	% No  [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
unicode_combining_class(0x249C, 0x24E9, 0).	% So  [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_combining_class(0x24EA, 0x24FF, 0).	% No  [22] CIRCLED DIGIT ZERO..NEGATIVE CIRCLED DIGIT ZERO
unicode_combining_class(0x2500, 0x25B6, 0).	% So [183] BOX DRAWINGS LIGHT HORIZONTAL..BLACK RIGHT-POINTING TRIANGLE
unicode_combining_class(0x25B7, 0x25B7, 0).	% Sm       WHITE RIGHT-POINTING TRIANGLE
unicode_combining_class(0x25B8, 0x25C0, 0).	% So   [9] BLACK RIGHT-POINTING SMALL TRIANGLE..BLACK LEFT-POINTING TRIANGLE
unicode_combining_class(0x25C1, 0x25C1, 0).	% Sm       WHITE LEFT-POINTING TRIANGLE
unicode_combining_class(0x25C2, 0x25F7, 0).	% So  [54] BLACK LEFT-POINTING SMALL TRIANGLE..WHITE CIRCLE WITH UPPER RIGHT QUADRANT
unicode_combining_class(0x25F8, 0x25FF, 0).	% Sm   [8] UPPER LEFT TRIANGLE..LOWER RIGHT TRIANGLE
unicode_combining_class(0x2600, 0x266E, 0).	% So [111] BLACK SUN WITH RAYS..MUSIC NATURAL SIGN
unicode_combining_class(0x266F, 0x266F, 0).	% Sm       MUSIC SHARP SIGN
unicode_combining_class(0x2670, 0x26FF, 0).	% So [144] WEST SYRIAC CROSS..WHITE FLAG WITH HORIZONTAL MIDDLE BLACK STRIPE
unicode_combining_class(0x2701, 0x2767, 0).	% So [103] UPPER BLADE SCISSORS..ROTATED FLORAL HEART BULLET
unicode_combining_class(0x2768, 0x2768, 0).	% Ps       MEDIUM LEFT PARENTHESIS ORNAMENT
unicode_combining_class(0x2769, 0x2769, 0).	% Pe       MEDIUM RIGHT PARENTHESIS ORNAMENT
unicode_combining_class(0x276A, 0x276A, 0).	% Ps       MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT
unicode_combining_class(0x276B, 0x276B, 0).	% Pe       MEDIUM FLATTENED RIGHT PARENTHESIS ORNAMENT
unicode_combining_class(0x276C, 0x276C, 0).	% Ps       MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT
unicode_combining_class(0x276D, 0x276D, 0).	% Pe       MEDIUM RIGHT-POINTING ANGLE BRACKET ORNAMENT
unicode_combining_class(0x276E, 0x276E, 0).	% Ps       HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT
unicode_combining_class(0x276F, 0x276F, 0).	% Pe       HEAVY RIGHT-POINTING ANGLE QUOTATION MARK ORNAMENT
unicode_combining_class(0x2770, 0x2770, 0).	% Ps       HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT
unicode_combining_class(0x2771, 0x2771, 0).	% Pe       HEAVY RIGHT-POINTING ANGLE BRACKET ORNAMENT
unicode_combining_class(0x2772, 0x2772, 0).	% Ps       LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT
unicode_combining_class(0x2773, 0x2773, 0).	% Pe       LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT
unicode_combining_class(0x2774, 0x2774, 0).	% Ps       MEDIUM LEFT CURLY BRACKET ORNAMENT
unicode_combining_class(0x2775, 0x2775, 0).	% Pe       MEDIUM RIGHT CURLY BRACKET ORNAMENT
unicode_combining_class(0x2776, 0x2793, 0).	% No  [30] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
unicode_combining_class(0x2794, 0x27BF, 0).	% So  [44] HEAVY WIDE-HEADED RIGHTWARDS ARROW..DOUBLE CURLY LOOP
unicode_combining_class(0x27C0, 0x27C4, 0).	% Sm   [5] THREE DIMENSIONAL ANGLE..OPEN SUPERSET
unicode_combining_class(0x27C5, 0x27C5, 0).	% Ps       LEFT S-SHAPED BAG DELIMITER
unicode_combining_class(0x27C6, 0x27C6, 0).	% Pe       RIGHT S-SHAPED BAG DELIMITER
unicode_combining_class(0x27C7, 0x27E5, 0).	% Sm  [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
unicode_combining_class(0x27E6, 0x27E6, 0).	% Ps       MATHEMATICAL LEFT WHITE SQUARE BRACKET
unicode_combining_class(0x27E7, 0x27E7, 0).	% Pe       MATHEMATICAL RIGHT WHITE SQUARE BRACKET
unicode_combining_class(0x27E8, 0x27E8, 0).	% Ps       MATHEMATICAL LEFT ANGLE BRACKET
unicode_combining_class(0x27E9, 0x27E9, 0).	% Pe       MATHEMATICAL RIGHT ANGLE BRACKET
unicode_combining_class(0x27EA, 0x27EA, 0).	% Ps       MATHEMATICAL LEFT DOUBLE ANGLE BRACKET
unicode_combining_class(0x27EB, 0x27EB, 0).	% Pe       MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET
unicode_combining_class(0x27EC, 0x27EC, 0).	% Ps       MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET
unicode_combining_class(0x27ED, 0x27ED, 0).	% Pe       MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET
unicode_combining_class(0x27EE, 0x27EE, 0).	% Ps       MATHEMATICAL LEFT FLATTENED PARENTHESIS
unicode_combining_class(0x27EF, 0x27EF, 0).	% Pe       MATHEMATICAL RIGHT FLATTENED PARENTHESIS
unicode_combining_class(0x27F0, 0x27FF, 0).	% Sm  [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
unicode_combining_class(0x2800, 0x28FF, 0).	% So [256] BRAILLE PATTERN BLANK..BRAILLE PATTERN DOTS-12345678
unicode_combining_class(0x2900, 0x2982, 0).	% Sm [131] RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE..Z NOTATION TYPE COLON
unicode_combining_class(0x2983, 0x2983, 0).	% Ps       LEFT WHITE CURLY BRACKET
unicode_combining_class(0x2984, 0x2984, 0).	% Pe       RIGHT WHITE CURLY BRACKET
unicode_combining_class(0x2985, 0x2985, 0).	% Ps       LEFT WHITE PARENTHESIS
unicode_combining_class(0x2986, 0x2986, 0).	% Pe       RIGHT WHITE PARENTHESIS
unicode_combining_class(0x2987, 0x2987, 0).	% Ps       Z NOTATION LEFT IMAGE BRACKET
unicode_combining_class(0x2988, 0x2988, 0).	% Pe       Z NOTATION RIGHT IMAGE BRACKET
unicode_combining_class(0x2989, 0x2989, 0).	% Ps       Z NOTATION LEFT BINDING BRACKET
unicode_combining_class(0x298A, 0x298A, 0).	% Pe       Z NOTATION RIGHT BINDING BRACKET
unicode_combining_class(0x298B, 0x298B, 0).	% Ps       LEFT SQUARE BRACKET WITH UNDERBAR
unicode_combining_class(0x298C, 0x298C, 0).	% Pe       RIGHT SQUARE BRACKET WITH UNDERBAR
unicode_combining_class(0x298D, 0x298D, 0).	% Ps       LEFT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_combining_class(0x298E, 0x298E, 0).	% Pe       RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_combining_class(0x298F, 0x298F, 0).	% Ps       LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_combining_class(0x2990, 0x2990, 0).	% Pe       RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_combining_class(0x2991, 0x2991, 0).	% Ps       LEFT ANGLE BRACKET WITH DOT
unicode_combining_class(0x2992, 0x2992, 0).	% Pe       RIGHT ANGLE BRACKET WITH DOT
unicode_combining_class(0x2993, 0x2993, 0).	% Ps       LEFT ARC LESS-THAN BRACKET
unicode_combining_class(0x2994, 0x2994, 0).	% Pe       RIGHT ARC GREATER-THAN BRACKET
unicode_combining_class(0x2995, 0x2995, 0).	% Ps       DOUBLE LEFT ARC GREATER-THAN BRACKET
unicode_combining_class(0x2996, 0x2996, 0).	% Pe       DOUBLE RIGHT ARC LESS-THAN BRACKET
unicode_combining_class(0x2997, 0x2997, 0).	% Ps       LEFT BLACK TORTOISE SHELL BRACKET
unicode_combining_class(0x2998, 0x2998, 0).	% Pe       RIGHT BLACK TORTOISE SHELL BRACKET
unicode_combining_class(0x2999, 0x29D7, 0).	% Sm  [63] DOTTED FENCE..BLACK HOURGLASS
unicode_combining_class(0x29D8, 0x29D8, 0).	% Ps       LEFT WIGGLY FENCE
unicode_combining_class(0x29D9, 0x29D9, 0).	% Pe       RIGHT WIGGLY FENCE
unicode_combining_class(0x29DA, 0x29DA, 0).	% Ps       LEFT DOUBLE WIGGLY FENCE
unicode_combining_class(0x29DB, 0x29DB, 0).	% Pe       RIGHT DOUBLE WIGGLY FENCE
unicode_combining_class(0x29DC, 0x29FB, 0).	% Sm  [32] INCOMPLETE INFINITY..TRIPLE PLUS
unicode_combining_class(0x29FC, 0x29FC, 0).	% Ps       LEFT-POINTING CURVED ANGLE BRACKET
unicode_combining_class(0x29FD, 0x29FD, 0).	% Pe       RIGHT-POINTING CURVED ANGLE BRACKET
unicode_combining_class(0x29FE, 0x2AFF, 0).	% Sm [258] TINY..N-ARY WHITE VERTICAL BAR
unicode_combining_class(0x2B00, 0x2B2F, 0).	% So  [48] NORTH EAST WHITE ARROW..WHITE VERTICAL ELLIPSE
unicode_combining_class(0x2B30, 0x2B44, 0).	% Sm  [21] LEFT ARROW WITH SMALL CIRCLE..RIGHTWARDS ARROW THROUGH SUPERSET
unicode_combining_class(0x2B45, 0x2B46, 0).	% So   [2] LEFTWARDS QUADRUPLE ARROW..RIGHTWARDS QUADRUPLE ARROW
unicode_combining_class(0x2B47, 0x2B4C, 0).	% Sm   [6] REVERSE TILDE OPERATOR ABOVE RIGHTWARDS ARROW..RIGHTWARDS ARROW ABOVE REVERSE TILDE OPERATOR
unicode_combining_class(0x2B50, 0x2B59, 0).	% So  [10] WHITE MEDIUM STAR..HEAVY CIRCLED SALTIRE
unicode_combining_class(0x2C00, 0x2C2E, 0).	% L&  [47] GLAGOLITIC CAPITAL LETTER AZU..GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_combining_class(0x2C30, 0x2C5E, 0).	% L&  [47] GLAGOLITIC SMALL LETTER AZU..GLAGOLITIC SMALL LETTER LATINATE MYSLITE
unicode_combining_class(0x2C60, 0x2C7B, 0).	% L&  [28] LATIN CAPITAL LETTER L WITH DOUBLE BAR..LATIN LETTER SMALL CAPITAL TURNED E
unicode_combining_class(0x2C7C, 0x2C7D, 0).	% Lm   [2] LATIN SUBSCRIPT SMALL LETTER J..MODIFIER LETTER CAPITAL V
unicode_combining_class(0x2C7E, 0x2CE4, 0).	% L& [103] LATIN CAPITAL LETTER S WITH SWASH TAIL..COPTIC SYMBOL KAI
unicode_combining_class(0x2CE5, 0x2CEA, 0).	% So   [6] COPTIC SYMBOL MI RO..COPTIC SYMBOL SHIMA SIMA
unicode_combining_class(0x2CEB, 0x2CEE, 0).	% L&   [4] COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI..COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
unicode_combining_class(0x2CF2, 0x2CF3, 0).	% L&   [2] COPTIC CAPITAL LETTER BOHAIRIC KHEI..COPTIC SMALL LETTER BOHAIRIC KHEI
unicode_combining_class(0x2CF9, 0x2CFC, 0).	% Po   [4] COPTIC OLD NUBIAN FULL STOP..COPTIC OLD NUBIAN VERSE DIVIDER
unicode_combining_class(0x2CFD, 0x2CFD, 0).	% No       COPTIC FRACTION ONE HALF
unicode_combining_class(0x2CFE, 0x2CFF, 0).	% Po   [2] COPTIC FULL STOP..COPTIC MORPHOLOGICAL DIVIDER
unicode_combining_class(0x2D00, 0x2D25, 0).	% L&  [38] GEORGIAN SMALL LETTER AN..GEORGIAN SMALL LETTER HOE
unicode_combining_class(0x2D27, 0x2D27, 0).	% L&       GEORGIAN SMALL LETTER YN
unicode_combining_class(0x2D2D, 0x2D2D, 0).	% L&       GEORGIAN SMALL LETTER AEN
unicode_combining_class(0x2D30, 0x2D67, 0).	% Lo  [56] TIFINAGH LETTER YA..TIFINAGH LETTER YO
unicode_combining_class(0x2D6F, 0x2D6F, 0).	% Lm       TIFINAGH MODIFIER LETTER LABIALIZATION MARK
unicode_combining_class(0x2D70, 0x2D70, 0).	% Po       TIFINAGH SEPARATOR MARK
unicode_combining_class(0x2D80, 0x2D96, 0).	% Lo  [23] ETHIOPIC SYLLABLE LOA..ETHIOPIC SYLLABLE GGWE
unicode_combining_class(0x2DA0, 0x2DA6, 0).	% Lo   [7] ETHIOPIC SYLLABLE SSA..ETHIOPIC SYLLABLE SSO
unicode_combining_class(0x2DA8, 0x2DAE, 0).	% Lo   [7] ETHIOPIC SYLLABLE CCA..ETHIOPIC SYLLABLE CCO
unicode_combining_class(0x2DB0, 0x2DB6, 0).	% Lo   [7] ETHIOPIC SYLLABLE ZZA..ETHIOPIC SYLLABLE ZZO
unicode_combining_class(0x2DB8, 0x2DBE, 0).	% Lo   [7] ETHIOPIC SYLLABLE CCHA..ETHIOPIC SYLLABLE CCHO
unicode_combining_class(0x2DC0, 0x2DC6, 0).	% Lo   [7] ETHIOPIC SYLLABLE QYA..ETHIOPIC SYLLABLE QYO
unicode_combining_class(0x2DC8, 0x2DCE, 0).	% Lo   [7] ETHIOPIC SYLLABLE KYA..ETHIOPIC SYLLABLE KYO
unicode_combining_class(0x2DD0, 0x2DD6, 0).	% Lo   [7] ETHIOPIC SYLLABLE XYA..ETHIOPIC SYLLABLE XYO
unicode_combining_class(0x2DD8, 0x2DDE, 0).	% Lo   [7] ETHIOPIC SYLLABLE GYA..ETHIOPIC SYLLABLE GYO
unicode_combining_class(0x2E00, 0x2E01, 0).	% Po   [2] RIGHT ANGLE SUBSTITUTION MARKER..RIGHT ANGLE DOTTED SUBSTITUTION MARKER
unicode_combining_class(0x2E02, 0x2E02, 0).	% Pi       LEFT SUBSTITUTION BRACKET
unicode_combining_class(0x2E03, 0x2E03, 0).	% Pf       RIGHT SUBSTITUTION BRACKET
unicode_combining_class(0x2E04, 0x2E04, 0).	% Pi       LEFT DOTTED SUBSTITUTION BRACKET
unicode_combining_class(0x2E05, 0x2E05, 0).	% Pf       RIGHT DOTTED SUBSTITUTION BRACKET
unicode_combining_class(0x2E06, 0x2E08, 0).	% Po   [3] RAISED INTERPOLATION MARKER..DOTTED TRANSPOSITION MARKER
unicode_combining_class(0x2E09, 0x2E09, 0).	% Pi       LEFT TRANSPOSITION BRACKET
unicode_combining_class(0x2E0A, 0x2E0A, 0).	% Pf       RIGHT TRANSPOSITION BRACKET
unicode_combining_class(0x2E0B, 0x2E0B, 0).	% Po       RAISED SQUARE
unicode_combining_class(0x2E0C, 0x2E0C, 0).	% Pi       LEFT RAISED OMISSION BRACKET
unicode_combining_class(0x2E0D, 0x2E0D, 0).	% Pf       RIGHT RAISED OMISSION BRACKET
unicode_combining_class(0x2E0E, 0x2E16, 0).	% Po   [9] EDITORIAL CORONIS..DOTTED RIGHT-POINTING ANGLE
unicode_combining_class(0x2E17, 0x2E17, 0).	% Pd       DOUBLE OBLIQUE HYPHEN
unicode_combining_class(0x2E18, 0x2E19, 0).	% Po   [2] INVERTED INTERROBANG..PALM BRANCH
unicode_combining_class(0x2E1A, 0x2E1A, 0).	% Pd       HYPHEN WITH DIAERESIS
unicode_combining_class(0x2E1B, 0x2E1B, 0).	% Po       TILDE WITH RING ABOVE
unicode_combining_class(0x2E1C, 0x2E1C, 0).	% Pi       LEFT LOW PARAPHRASE BRACKET
unicode_combining_class(0x2E1D, 0x2E1D, 0).	% Pf       RIGHT LOW PARAPHRASE BRACKET
unicode_combining_class(0x2E1E, 0x2E1F, 0).	% Po   [2] TILDE WITH DOT ABOVE..TILDE WITH DOT BELOW
unicode_combining_class(0x2E20, 0x2E20, 0).	% Pi       LEFT VERTICAL BAR WITH QUILL
unicode_combining_class(0x2E21, 0x2E21, 0).	% Pf       RIGHT VERTICAL BAR WITH QUILL
unicode_combining_class(0x2E22, 0x2E22, 0).	% Ps       TOP LEFT HALF BRACKET
unicode_combining_class(0x2E23, 0x2E23, 0).	% Pe       TOP RIGHT HALF BRACKET
unicode_combining_class(0x2E24, 0x2E24, 0).	% Ps       BOTTOM LEFT HALF BRACKET
unicode_combining_class(0x2E25, 0x2E25, 0).	% Pe       BOTTOM RIGHT HALF BRACKET
unicode_combining_class(0x2E26, 0x2E26, 0).	% Ps       LEFT SIDEWAYS U BRACKET
unicode_combining_class(0x2E27, 0x2E27, 0).	% Pe       RIGHT SIDEWAYS U BRACKET
unicode_combining_class(0x2E28, 0x2E28, 0).	% Ps       LEFT DOUBLE PARENTHESIS
unicode_combining_class(0x2E29, 0x2E29, 0).	% Pe       RIGHT DOUBLE PARENTHESIS
unicode_combining_class(0x2E2A, 0x2E2E, 0).	% Po   [5] TWO DOTS OVER ONE DOT PUNCTUATION..REVERSED QUESTION MARK
unicode_combining_class(0x2E2F, 0x2E2F, 0).	% Lm       VERTICAL TILDE
unicode_combining_class(0x2E30, 0x2E39, 0).	% Po  [10] RING POINT..TOP HALF SECTION SIGN
unicode_combining_class(0x2E3A, 0x2E3B, 0).	% Pd   [2] TWO-EM DASH..THREE-EM DASH
unicode_combining_class(0x2E80, 0x2E99, 0).	% So  [26] CJK RADICAL REPEAT..CJK RADICAL RAP
unicode_combining_class(0x2E9B, 0x2EF3, 0).	% So  [89] CJK RADICAL CHOKE..CJK RADICAL C-SIMPLIFIED TURTLE
unicode_combining_class(0x2F00, 0x2FD5, 0).	% So [214] KANGXI RADICAL ONE..KANGXI RADICAL FLUTE
unicode_combining_class(0x2FF0, 0x2FFB, 0).	% So  [12] IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT..IDEOGRAPHIC DESCRIPTION CHARACTER OVERLAID
unicode_combining_class(0x3000, 0x3000, 0).	% Zs       IDEOGRAPHIC SPACE
unicode_combining_class(0x3001, 0x3003, 0).	% Po   [3] IDEOGRAPHIC COMMA..DITTO MARK
unicode_combining_class(0x3004, 0x3004, 0).	% So       JAPANESE INDUSTRIAL STANDARD SYMBOL
unicode_combining_class(0x3005, 0x3005, 0).	% Lm       IDEOGRAPHIC ITERATION MARK
unicode_combining_class(0x3006, 0x3006, 0).	% Lo       IDEOGRAPHIC CLOSING MARK
unicode_combining_class(0x3007, 0x3007, 0).	% Nl       IDEOGRAPHIC NUMBER ZERO
unicode_combining_class(0x3008, 0x3008, 0).	% Ps       LEFT ANGLE BRACKET
unicode_combining_class(0x3009, 0x3009, 0).	% Pe       RIGHT ANGLE BRACKET
unicode_combining_class(0x300A, 0x300A, 0).	% Ps       LEFT DOUBLE ANGLE BRACKET
unicode_combining_class(0x300B, 0x300B, 0).	% Pe       RIGHT DOUBLE ANGLE BRACKET
unicode_combining_class(0x300C, 0x300C, 0).	% Ps       LEFT CORNER BRACKET
unicode_combining_class(0x300D, 0x300D, 0).	% Pe       RIGHT CORNER BRACKET
unicode_combining_class(0x300E, 0x300E, 0).	% Ps       LEFT WHITE CORNER BRACKET
unicode_combining_class(0x300F, 0x300F, 0).	% Pe       RIGHT WHITE CORNER BRACKET
unicode_combining_class(0x3010, 0x3010, 0).	% Ps       LEFT BLACK LENTICULAR BRACKET
unicode_combining_class(0x3011, 0x3011, 0).	% Pe       RIGHT BLACK LENTICULAR BRACKET
unicode_combining_class(0x3012, 0x3013, 0).	% So   [2] POSTAL MARK..GETA MARK
unicode_combining_class(0x3014, 0x3014, 0).	% Ps       LEFT TORTOISE SHELL BRACKET
unicode_combining_class(0x3015, 0x3015, 0).	% Pe       RIGHT TORTOISE SHELL BRACKET
unicode_combining_class(0x3016, 0x3016, 0).	% Ps       LEFT WHITE LENTICULAR BRACKET
unicode_combining_class(0x3017, 0x3017, 0).	% Pe       RIGHT WHITE LENTICULAR BRACKET
unicode_combining_class(0x3018, 0x3018, 0).	% Ps       LEFT WHITE TORTOISE SHELL BRACKET
unicode_combining_class(0x3019, 0x3019, 0).	% Pe       RIGHT WHITE TORTOISE SHELL BRACKET
unicode_combining_class(0x301A, 0x301A, 0).	% Ps       LEFT WHITE SQUARE BRACKET
unicode_combining_class(0x301B, 0x301B, 0).	% Pe       RIGHT WHITE SQUARE BRACKET
unicode_combining_class(0x301C, 0x301C, 0).	% Pd       WAVE DASH
unicode_combining_class(0x301D, 0x301D, 0).	% Ps       REVERSED DOUBLE PRIME QUOTATION MARK
unicode_combining_class(0x301E, 0x301F, 0).	% Pe   [2] DOUBLE PRIME QUOTATION MARK..LOW DOUBLE PRIME QUOTATION MARK
unicode_combining_class(0x3020, 0x3020, 0).	% So       POSTAL MARK FACE
unicode_combining_class(0x3021, 0x3029, 0).	% Nl   [9] HANGZHOU NUMERAL ONE..HANGZHOU NUMERAL NINE
unicode_combining_class(0x3030, 0x3030, 0).	% Pd       WAVY DASH
unicode_combining_class(0x3031, 0x3035, 0).	% Lm   [5] VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF
unicode_combining_class(0x3036, 0x3037, 0).	% So   [2] CIRCLED POSTAL MARK..IDEOGRAPHIC TELEGRAPH LINE FEED SEPARATOR SYMBOL
unicode_combining_class(0x3038, 0x303A, 0).	% Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
unicode_combining_class(0x303B, 0x303B, 0).	% Lm       VERTICAL IDEOGRAPHIC ITERATION MARK
unicode_combining_class(0x303C, 0x303C, 0).	% Lo       MASU MARK
unicode_combining_class(0x303D, 0x303D, 0).	% Po       PART ALTERNATION MARK
unicode_combining_class(0x303E, 0x303F, 0).	% So   [2] IDEOGRAPHIC VARIATION INDICATOR..IDEOGRAPHIC HALF FILL SPACE
unicode_combining_class(0x3041, 0x3096, 0).	% Lo  [86] HIRAGANA LETTER SMALL A..HIRAGANA LETTER SMALL KE
unicode_combining_class(0x309B, 0x309C, 0).	% Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_combining_class(0x309D, 0x309E, 0).	% Lm   [2] HIRAGANA ITERATION MARK..HIRAGANA VOICED ITERATION MARK
unicode_combining_class(0x309F, 0x309F, 0).	% Lo       HIRAGANA DIGRAPH YORI
unicode_combining_class(0x30A0, 0x30A0, 0).	% Pd       KATAKANA-HIRAGANA DOUBLE HYPHEN
unicode_combining_class(0x30A1, 0x30FA, 0).	% Lo  [90] KATAKANA LETTER SMALL A..KATAKANA LETTER VO
unicode_combining_class(0x30FB, 0x30FB, 0).	% Po       KATAKANA MIDDLE DOT
unicode_combining_class(0x30FC, 0x30FE, 0).	% Lm   [3] KATAKANA-HIRAGANA PROLONGED SOUND MARK..KATAKANA VOICED ITERATION MARK
unicode_combining_class(0x30FF, 0x30FF, 0).	% Lo       KATAKANA DIGRAPH KOTO
unicode_combining_class(0x3105, 0x312D, 0).	% Lo  [41] BOPOMOFO LETTER B..BOPOMOFO LETTER IH
unicode_combining_class(0x3131, 0x318E, 0).	% Lo  [94] HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE
unicode_combining_class(0x3190, 0x3191, 0).	% So   [2] IDEOGRAPHIC ANNOTATION LINKING MARK..IDEOGRAPHIC ANNOTATION REVERSE MARK
unicode_combining_class(0x3192, 0x3195, 0).	% No   [4] IDEOGRAPHIC ANNOTATION ONE MARK..IDEOGRAPHIC ANNOTATION FOUR MARK
unicode_combining_class(0x3196, 0x319F, 0).	% So  [10] IDEOGRAPHIC ANNOTATION TOP MARK..IDEOGRAPHIC ANNOTATION MAN MARK
unicode_combining_class(0x31A0, 0x31BA, 0).	% Lo  [27] BOPOMOFO LETTER BU..BOPOMOFO LETTER ZY
unicode_combining_class(0x31C0, 0x31E3, 0).	% So  [36] CJK STROKE T..CJK STROKE Q
unicode_combining_class(0x31F0, 0x31FF, 0).	% Lo  [16] KATAKANA LETTER SMALL KU..KATAKANA LETTER SMALL RO
unicode_combining_class(0x3200, 0x321E, 0).	% So  [31] PARENTHESIZED HANGUL KIYEOK..PARENTHESIZED KOREAN CHARACTER O HU
unicode_combining_class(0x3220, 0x3229, 0).	% No  [10] PARENTHESIZED IDEOGRAPH ONE..PARENTHESIZED IDEOGRAPH TEN
unicode_combining_class(0x322A, 0x3247, 0).	% So  [30] PARENTHESIZED IDEOGRAPH MOON..CIRCLED IDEOGRAPH KOTO
unicode_combining_class(0x3248, 0x324F, 0).	% No   [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
unicode_combining_class(0x3250, 0x3250, 0).	% So       PARTNERSHIP SIGN
unicode_combining_class(0x3251, 0x325F, 0).	% No  [15] CIRCLED NUMBER TWENTY ONE..CIRCLED NUMBER THIRTY FIVE
unicode_combining_class(0x3260, 0x327F, 0).	% So  [32] CIRCLED HANGUL KIYEOK..KOREAN STANDARD SYMBOL
unicode_combining_class(0x3280, 0x3289, 0).	% No  [10] CIRCLED IDEOGRAPH ONE..CIRCLED IDEOGRAPH TEN
unicode_combining_class(0x328A, 0x32B0, 0).	% So  [39] CIRCLED IDEOGRAPH MOON..CIRCLED IDEOGRAPH NIGHT
unicode_combining_class(0x32B1, 0x32BF, 0).	% No  [15] CIRCLED NUMBER THIRTY SIX..CIRCLED NUMBER FIFTY
unicode_combining_class(0x32C0, 0x32FE, 0).	% So  [63] IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY..CIRCLED KATAKANA WO
unicode_combining_class(0x3300, 0x33FF, 0).	% So [256] SQUARE APAATO..SQUARE GAL
unicode_combining_class(0x3400, 0x4DB5, 0).	% Lo [6582] CJK UNIFIED IDEOGRAPH-3400..CJK UNIFIED IDEOGRAPH-4DB5
unicode_combining_class(0x4DC0, 0x4DFF, 0).	% So  [64] HEXAGRAM FOR THE CREATIVE HEAVEN..HEXAGRAM FOR BEFORE COMPLETION
unicode_combining_class(0x4E00, 0x9FCC, 0).	% Lo [20941] CJK UNIFIED IDEOGRAPH-4E00..CJK UNIFIED IDEOGRAPH-9FCC
unicode_combining_class(0xA000, 0xA014, 0).	% Lo  [21] YI SYLLABLE IT..YI SYLLABLE E
unicode_combining_class(0xA015, 0xA015, 0).	% Lm       YI SYLLABLE WU
unicode_combining_class(0xA016, 0xA48C, 0).	% Lo [1143] YI SYLLABLE BIT..YI SYLLABLE YYR
unicode_combining_class(0xA490, 0xA4C6, 0).	% So  [55] YI RADICAL QOT..YI RADICAL KE
unicode_combining_class(0xA4D0, 0xA4F7, 0).	% Lo  [40] LISU LETTER BA..LISU LETTER OE
unicode_combining_class(0xA4F8, 0xA4FD, 0).	% Lm   [6] LISU LETTER TONE MYA TI..LISU LETTER TONE MYA JEU
unicode_combining_class(0xA4FE, 0xA4FF, 0).	% Po   [2] LISU PUNCTUATION COMMA..LISU PUNCTUATION FULL STOP
unicode_combining_class(0xA500, 0xA60B, 0).	% Lo [268] VAI SYLLABLE EE..VAI SYLLABLE NG
unicode_combining_class(0xA60C, 0xA60C, 0).	% Lm       VAI SYLLABLE LENGTHENER
unicode_combining_class(0xA60D, 0xA60F, 0).	% Po   [3] VAI COMMA..VAI QUESTION MARK
unicode_combining_class(0xA610, 0xA61F, 0).	% Lo  [16] VAI SYLLABLE NDOLE FA..VAI SYMBOL JONG
unicode_combining_class(0xA620, 0xA629, 0).	% Nd  [10] VAI DIGIT ZERO..VAI DIGIT NINE
unicode_combining_class(0xA62A, 0xA62B, 0).	% Lo   [2] VAI SYLLABLE NDOLE MA..VAI SYLLABLE NDOLE DO
unicode_combining_class(0xA640, 0xA66D, 0).	% L&  [46] CYRILLIC CAPITAL LETTER ZEMLYA..CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
unicode_combining_class(0xA66E, 0xA66E, 0).	% Lo       CYRILLIC LETTER MULTIOCULAR O
unicode_combining_class(0xA670, 0xA672, 0).	% Me   [3] COMBINING CYRILLIC TEN MILLIONS SIGN..COMBINING CYRILLIC THOUSAND MILLIONS SIGN
unicode_combining_class(0xA673, 0xA673, 0).	% Po       SLAVONIC ASTERISK
unicode_combining_class(0xA67E, 0xA67E, 0).	% Po       CYRILLIC KAVYKA
unicode_combining_class(0xA67F, 0xA67F, 0).	% Lm       CYRILLIC PAYEROK
unicode_combining_class(0xA680, 0xA697, 0).	% L&  [24] CYRILLIC CAPITAL LETTER DWE..CYRILLIC SMALL LETTER SHWE
unicode_combining_class(0xA6A0, 0xA6E5, 0).	% Lo  [70] BAMUM LETTER A..BAMUM LETTER KI
unicode_combining_class(0xA6E6, 0xA6EF, 0).	% Nl  [10] BAMUM LETTER MO..BAMUM LETTER KOGHOM
unicode_combining_class(0xA6F2, 0xA6F7, 0).	% Po   [6] BAMUM NJAEMLI..BAMUM QUESTION MARK
unicode_combining_class(0xA700, 0xA716, 0).	% Sk  [23] MODIFIER LETTER CHINESE TONE YIN PING..MODIFIER LETTER EXTRA-LOW LEFT-STEM TONE BAR
unicode_combining_class(0xA717, 0xA71F, 0).	% Lm   [9] MODIFIER LETTER DOT VERTICAL BAR..MODIFIER LETTER LOW INVERTED EXCLAMATION MARK
unicode_combining_class(0xA720, 0xA721, 0).	% Sk   [2] MODIFIER LETTER STRESS AND HIGH TONE..MODIFIER LETTER STRESS AND LOW TONE
unicode_combining_class(0xA722, 0xA76F, 0).	% L&  [78] LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF..LATIN SMALL LETTER CON
unicode_combining_class(0xA770, 0xA770, 0).	% Lm       MODIFIER LETTER US
unicode_combining_class(0xA771, 0xA787, 0).	% L&  [23] LATIN SMALL LETTER DUM..LATIN SMALL LETTER INSULAR T
unicode_combining_class(0xA788, 0xA788, 0).	% Lm       MODIFIER LETTER LOW CIRCUMFLEX ACCENT
unicode_combining_class(0xA789, 0xA78A, 0).	% Sk   [2] MODIFIER LETTER COLON..MODIFIER LETTER SHORT EQUALS SIGN
unicode_combining_class(0xA78B, 0xA78E, 0).	% L&   [4] LATIN CAPITAL LETTER SALTILLO..LATIN SMALL LETTER L WITH RETROFLEX HOOK AND BELT
unicode_combining_class(0xA790, 0xA793, 0).	% L&   [4] LATIN CAPITAL LETTER N WITH DESCENDER..LATIN SMALL LETTER C WITH BAR
unicode_combining_class(0xA7A0, 0xA7AA, 0).	% L&  [11] LATIN CAPITAL LETTER G WITH OBLIQUE STROKE..LATIN CAPITAL LETTER H WITH HOOK
unicode_combining_class(0xA7F8, 0xA7F9, 0).	% Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_combining_class(0xA7FA, 0xA7FA, 0).	% L&       LATIN LETTER SMALL CAPITAL TURNED M
unicode_combining_class(0xA7FB, 0xA801, 0).	% Lo   [7] LATIN EPIGRAPHIC LETTER REVERSED F..SYLOTI NAGRI LETTER I
unicode_combining_class(0xA802, 0xA802, 0).	% Mn       SYLOTI NAGRI SIGN DVISVARA
unicode_combining_class(0xA803, 0xA805, 0).	% Lo   [3] SYLOTI NAGRI LETTER U..SYLOTI NAGRI LETTER O
unicode_combining_class(0xA807, 0xA80A, 0).	% Lo   [4] SYLOTI NAGRI LETTER KO..SYLOTI NAGRI LETTER GHO
unicode_combining_class(0xA80B, 0xA80B, 0).	% Mn       SYLOTI NAGRI SIGN ANUSVARA
unicode_combining_class(0xA80C, 0xA822, 0).	% Lo  [23] SYLOTI NAGRI LETTER CO..SYLOTI NAGRI LETTER HO
unicode_combining_class(0xA823, 0xA824, 0).	% Mc   [2] SYLOTI NAGRI VOWEL SIGN A..SYLOTI NAGRI VOWEL SIGN I
unicode_combining_class(0xA825, 0xA826, 0).	% Mn   [2] SYLOTI NAGRI VOWEL SIGN U..SYLOTI NAGRI VOWEL SIGN E
unicode_combining_class(0xA827, 0xA827, 0).	% Mc       SYLOTI NAGRI VOWEL SIGN OO
unicode_combining_class(0xA828, 0xA82B, 0).	% So   [4] SYLOTI NAGRI POETRY MARK-1..SYLOTI NAGRI POETRY MARK-4
unicode_combining_class(0xA830, 0xA835, 0).	% No   [6] NORTH INDIC FRACTION ONE QUARTER..NORTH INDIC FRACTION THREE SIXTEENTHS
unicode_combining_class(0xA836, 0xA837, 0).	% So   [2] NORTH INDIC QUARTER MARK..NORTH INDIC PLACEHOLDER MARK
unicode_combining_class(0xA838, 0xA838, 0).	% Sc       NORTH INDIC RUPEE MARK
unicode_combining_class(0xA839, 0xA839, 0).	% So       NORTH INDIC QUANTITY MARK
unicode_combining_class(0xA840, 0xA873, 0).	% Lo  [52] PHAGS-PA LETTER KA..PHAGS-PA LETTER CANDRABINDU
unicode_combining_class(0xA874, 0xA877, 0).	% Po   [4] PHAGS-PA SINGLE HEAD MARK..PHAGS-PA MARK DOUBLE SHAD
unicode_combining_class(0xA880, 0xA881, 0).	% Mc   [2] SAURASHTRA SIGN ANUSVARA..SAURASHTRA SIGN VISARGA
unicode_combining_class(0xA882, 0xA8B3, 0).	% Lo  [50] SAURASHTRA LETTER A..SAURASHTRA LETTER LLA
unicode_combining_class(0xA8B4, 0xA8C3, 0).	% Mc  [16] SAURASHTRA CONSONANT SIGN HAARU..SAURASHTRA VOWEL SIGN AU
unicode_combining_class(0xA8CE, 0xA8CF, 0).	% Po   [2] SAURASHTRA DANDA..SAURASHTRA DOUBLE DANDA
unicode_combining_class(0xA8D0, 0xA8D9, 0).	% Nd  [10] SAURASHTRA DIGIT ZERO..SAURASHTRA DIGIT NINE
unicode_combining_class(0xA8F2, 0xA8F7, 0).	% Lo   [6] DEVANAGARI SIGN SPACING CANDRABINDU..DEVANAGARI SIGN CANDRABINDU AVAGRAHA
unicode_combining_class(0xA8F8, 0xA8FA, 0).	% Po   [3] DEVANAGARI SIGN PUSHPIKA..DEVANAGARI CARET
unicode_combining_class(0xA8FB, 0xA8FB, 0).	% Lo       DEVANAGARI HEADSTROKE
unicode_combining_class(0xA900, 0xA909, 0).	% Nd  [10] KAYAH LI DIGIT ZERO..KAYAH LI DIGIT NINE
unicode_combining_class(0xA90A, 0xA925, 0).	% Lo  [28] KAYAH LI LETTER KA..KAYAH LI LETTER OO
unicode_combining_class(0xA926, 0xA92A, 0).	% Mn   [5] KAYAH LI VOWEL UE..KAYAH LI VOWEL O
unicode_combining_class(0xA92E, 0xA92F, 0).	% Po   [2] KAYAH LI SIGN CWI..KAYAH LI SIGN SHYA
unicode_combining_class(0xA930, 0xA946, 0).	% Lo  [23] REJANG LETTER KA..REJANG LETTER A
unicode_combining_class(0xA947, 0xA951, 0).	% Mn  [11] REJANG VOWEL SIGN I..REJANG CONSONANT SIGN R
unicode_combining_class(0xA952, 0xA952, 0).	% Mc       REJANG CONSONANT SIGN H
unicode_combining_class(0xA95F, 0xA95F, 0).	% Po       REJANG SECTION MARK
unicode_combining_class(0xA960, 0xA97C, 0).	% Lo  [29] HANGUL CHOSEONG TIKEUT-MIEUM..HANGUL CHOSEONG SSANGYEORINHIEUH
unicode_combining_class(0xA980, 0xA982, 0).	% Mn   [3] JAVANESE SIGN PANYANGGA..JAVANESE SIGN LAYAR
unicode_combining_class(0xA983, 0xA983, 0).	% Mc       JAVANESE SIGN WIGNYAN
unicode_combining_class(0xA984, 0xA9B2, 0).	% Lo  [47] JAVANESE LETTER A..JAVANESE LETTER HA
unicode_combining_class(0xA9B4, 0xA9B5, 0).	% Mc   [2] JAVANESE VOWEL SIGN TARUNG..JAVANESE VOWEL SIGN TOLONG
unicode_combining_class(0xA9B6, 0xA9B9, 0).	% Mn   [4] JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN SUKU MENDUT
unicode_combining_class(0xA9BA, 0xA9BB, 0).	% Mc   [2] JAVANESE VOWEL SIGN TALING..JAVANESE VOWEL SIGN DIRGA MURE
unicode_combining_class(0xA9BC, 0xA9BC, 0).	% Mn       JAVANESE VOWEL SIGN PEPET
unicode_combining_class(0xA9BD, 0xA9BF, 0).	% Mc   [3] JAVANESE CONSONANT SIGN KERET..JAVANESE CONSONANT SIGN CAKRA
unicode_combining_class(0xA9C1, 0xA9CD, 0).	% Po  [13] JAVANESE LEFT RERENGGAN..JAVANESE TURNED PADA PISELEH
unicode_combining_class(0xA9CF, 0xA9CF, 0).	% Lm       JAVANESE PANGRANGKEP
unicode_combining_class(0xA9D0, 0xA9D9, 0).	% Nd  [10] JAVANESE DIGIT ZERO..JAVANESE DIGIT NINE
unicode_combining_class(0xA9DE, 0xA9DF, 0).	% Po   [2] JAVANESE PADA TIRTA TUMETES..JAVANESE PADA ISEN-ISEN
unicode_combining_class(0xAA00, 0xAA28, 0).	% Lo  [41] CHAM LETTER A..CHAM LETTER HA
unicode_combining_class(0xAA29, 0xAA2E, 0).	% Mn   [6] CHAM VOWEL SIGN AA..CHAM VOWEL SIGN OE
unicode_combining_class(0xAA2F, 0xAA30, 0).	% Mc   [2] CHAM VOWEL SIGN O..CHAM VOWEL SIGN AI
unicode_combining_class(0xAA31, 0xAA32, 0).	% Mn   [2] CHAM VOWEL SIGN AU..CHAM VOWEL SIGN UE
unicode_combining_class(0xAA33, 0xAA34, 0).	% Mc   [2] CHAM CONSONANT SIGN YA..CHAM CONSONANT SIGN RA
unicode_combining_class(0xAA35, 0xAA36, 0).	% Mn   [2] CHAM CONSONANT SIGN LA..CHAM CONSONANT SIGN WA
unicode_combining_class(0xAA40, 0xAA42, 0).	% Lo   [3] CHAM LETTER FINAL K..CHAM LETTER FINAL NG
unicode_combining_class(0xAA43, 0xAA43, 0).	% Mn       CHAM CONSONANT SIGN FINAL NG
unicode_combining_class(0xAA44, 0xAA4B, 0).	% Lo   [8] CHAM LETTER FINAL CH..CHAM LETTER FINAL SS
unicode_combining_class(0xAA4C, 0xAA4C, 0).	% Mn       CHAM CONSONANT SIGN FINAL M
unicode_combining_class(0xAA4D, 0xAA4D, 0).	% Mc       CHAM CONSONANT SIGN FINAL H
unicode_combining_class(0xAA50, 0xAA59, 0).	% Nd  [10] CHAM DIGIT ZERO..CHAM DIGIT NINE
unicode_combining_class(0xAA5C, 0xAA5F, 0).	% Po   [4] CHAM PUNCTUATION SPIRAL..CHAM PUNCTUATION TRIPLE DANDA
unicode_combining_class(0xAA60, 0xAA6F, 0).	% Lo  [16] MYANMAR LETTER KHAMTI GA..MYANMAR LETTER KHAMTI FA
unicode_combining_class(0xAA70, 0xAA70, 0).	% Lm       MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION
unicode_combining_class(0xAA71, 0xAA76, 0).	% Lo   [6] MYANMAR LETTER KHAMTI XA..MYANMAR LOGOGRAM KHAMTI HM
unicode_combining_class(0xAA77, 0xAA79, 0).	% So   [3] MYANMAR SYMBOL AITON EXCLAMATION..MYANMAR SYMBOL AITON TWO
unicode_combining_class(0xAA7A, 0xAA7A, 0).	% Lo       MYANMAR LETTER AITON RA
unicode_combining_class(0xAA7B, 0xAA7B, 0).	% Mc       MYANMAR SIGN PAO KAREN TONE
unicode_combining_class(0xAA80, 0xAAAF, 0).	% Lo  [48] TAI VIET LETTER LOW KO..TAI VIET LETTER HIGH O
unicode_combining_class(0xAAB1, 0xAAB1, 0).	% Lo       TAI VIET VOWEL AA
unicode_combining_class(0xAAB5, 0xAAB6, 0).	% Lo   [2] TAI VIET VOWEL E..TAI VIET VOWEL O
unicode_combining_class(0xAAB9, 0xAABD, 0).	% Lo   [5] TAI VIET VOWEL UEA..TAI VIET VOWEL AN
unicode_combining_class(0xAAC0, 0xAAC0, 0).	% Lo       TAI VIET TONE MAI NUENG
unicode_combining_class(0xAAC2, 0xAAC2, 0).	% Lo       TAI VIET TONE MAI SONG
unicode_combining_class(0xAADB, 0xAADC, 0).	% Lo   [2] TAI VIET SYMBOL KON..TAI VIET SYMBOL NUENG
unicode_combining_class(0xAADD, 0xAADD, 0).	% Lm       TAI VIET SYMBOL SAM
unicode_combining_class(0xAADE, 0xAADF, 0).	% Po   [2] TAI VIET SYMBOL HO HOI..TAI VIET SYMBOL KOI KOI
unicode_combining_class(0xAAE0, 0xAAEA, 0).	% Lo  [11] MEETEI MAYEK LETTER E..MEETEI MAYEK LETTER SSA
unicode_combining_class(0xAAEB, 0xAAEB, 0).	% Mc       MEETEI MAYEK VOWEL SIGN II
unicode_combining_class(0xAAEC, 0xAAED, 0).	% Mn   [2] MEETEI MAYEK VOWEL SIGN UU..MEETEI MAYEK VOWEL SIGN AAI
unicode_combining_class(0xAAEE, 0xAAEF, 0).	% Mc   [2] MEETEI MAYEK VOWEL SIGN AU..MEETEI MAYEK VOWEL SIGN AAU
unicode_combining_class(0xAAF0, 0xAAF1, 0).	% Po   [2] MEETEI MAYEK CHEIKHAN..MEETEI MAYEK AHANG KHUDAM
unicode_combining_class(0xAAF2, 0xAAF2, 0).	% Lo       MEETEI MAYEK ANJI
unicode_combining_class(0xAAF3, 0xAAF4, 0).	% Lm   [2] MEETEI MAYEK SYLLABLE REPETITION MARK..MEETEI MAYEK WORD REPETITION MARK
unicode_combining_class(0xAAF5, 0xAAF5, 0).	% Mc       MEETEI MAYEK VOWEL SIGN VISARGA
unicode_combining_class(0xAB01, 0xAB06, 0).	% Lo   [6] ETHIOPIC SYLLABLE TTHU..ETHIOPIC SYLLABLE TTHO
unicode_combining_class(0xAB09, 0xAB0E, 0).	% Lo   [6] ETHIOPIC SYLLABLE DDHU..ETHIOPIC SYLLABLE DDHO
unicode_combining_class(0xAB11, 0xAB16, 0).	% Lo   [6] ETHIOPIC SYLLABLE DZU..ETHIOPIC SYLLABLE DZO
unicode_combining_class(0xAB20, 0xAB26, 0).	% Lo   [7] ETHIOPIC SYLLABLE CCHHA..ETHIOPIC SYLLABLE CCHHO
unicode_combining_class(0xAB28, 0xAB2E, 0).	% Lo   [7] ETHIOPIC SYLLABLE BBA..ETHIOPIC SYLLABLE BBO
unicode_combining_class(0xABC0, 0xABE2, 0).	% Lo  [35] MEETEI MAYEK LETTER KOK..MEETEI MAYEK LETTER I LONSUM
unicode_combining_class(0xABE3, 0xABE4, 0).	% Mc   [2] MEETEI MAYEK VOWEL SIGN ONAP..MEETEI MAYEK VOWEL SIGN INAP
unicode_combining_class(0xABE5, 0xABE5, 0).	% Mn       MEETEI MAYEK VOWEL SIGN ANAP
unicode_combining_class(0xABE6, 0xABE7, 0).	% Mc   [2] MEETEI MAYEK VOWEL SIGN YENAP..MEETEI MAYEK VOWEL SIGN SOUNAP
unicode_combining_class(0xABE8, 0xABE8, 0).	% Mn       MEETEI MAYEK VOWEL SIGN UNAP
unicode_combining_class(0xABE9, 0xABEA, 0).	% Mc   [2] MEETEI MAYEK VOWEL SIGN CHEINAP..MEETEI MAYEK VOWEL SIGN NUNG
unicode_combining_class(0xABEB, 0xABEB, 0).	% Po       MEETEI MAYEK CHEIKHEI
unicode_combining_class(0xABEC, 0xABEC, 0).	% Mc       MEETEI MAYEK LUM IYEK
unicode_combining_class(0xABF0, 0xABF9, 0).	% Nd  [10] MEETEI MAYEK DIGIT ZERO..MEETEI MAYEK DIGIT NINE
unicode_combining_class(0xAC00, 0xD7A3, 0).	% Lo [11172] HANGUL SYLLABLE GA..HANGUL SYLLABLE HIH
unicode_combining_class(0xD7B0, 0xD7C6, 0).	% Lo  [23] HANGUL JUNGSEONG O-YEO..HANGUL JUNGSEONG ARAEA-E
unicode_combining_class(0xD7CB, 0xD7FB, 0).	% Lo  [49] HANGUL JONGSEONG NIEUN-RIEUL..HANGUL JONGSEONG PHIEUPH-THIEUTH
unicode_combining_class(0xE000, 0xF8FF, 0).	% Co [6400] <private-use-E000>..<private-use-F8FF>
unicode_combining_class(0xF900, 0xFA6D, 0).	% Lo [366] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA6D
unicode_combining_class(0xFA70, 0xFAD9, 0).	% Lo [106] CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9
unicode_combining_class(0xFB00, 0xFB06, 0).	% L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_combining_class(0xFB13, 0xFB17, 0).	% L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_combining_class(0xFB1D, 0xFB1D, 0).	% Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_combining_class(0xFB1F, 0xFB28, 0).	% Lo  [10] HEBREW LIGATURE YIDDISH YOD YOD PATAH..HEBREW LETTER WIDE TAV
unicode_combining_class(0xFB29, 0xFB29, 0).	% Sm       HEBREW LETTER ALTERNATIVE PLUS SIGN
unicode_combining_class(0xFB2A, 0xFB36, 0).	% Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_combining_class(0xFB38, 0xFB3C, 0).	% Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_combining_class(0xFB3E, 0xFB3E, 0).	% Lo       HEBREW LETTER MEM WITH DAGESH
unicode_combining_class(0xFB40, 0xFB41, 0).	% Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_combining_class(0xFB43, 0xFB44, 0).	% Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_combining_class(0xFB46, 0xFBB1, 0).	% Lo [108] HEBREW LETTER TSADI WITH DAGESH..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
unicode_combining_class(0xFBB2, 0xFBC1, 0).	% Sk  [16] ARABIC SYMBOL DOT ABOVE..ARABIC SYMBOL SMALL TAH BELOW
unicode_combining_class(0xFBD3, 0xFD3D, 0).	% Lo [363] ARABIC LETTER NG ISOLATED FORM..ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
unicode_combining_class(0xFD3E, 0xFD3E, 0).	% Ps       ORNATE LEFT PARENTHESIS
unicode_combining_class(0xFD3F, 0xFD3F, 0).	% Pe       ORNATE RIGHT PARENTHESIS
unicode_combining_class(0xFD50, 0xFD8F, 0).	% Lo  [64] ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
unicode_combining_class(0xFD92, 0xFDC7, 0).	% Lo  [54] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
unicode_combining_class(0xFDF0, 0xFDFB, 0).	% Lo  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
unicode_combining_class(0xFDFC, 0xFDFC, 0).	% Sc       RIAL SIGN
unicode_combining_class(0xFDFD, 0xFDFD, 0).	% So       ARABIC LIGATURE BISMILLAH AR-RAHMAN AR-RAHEEM
unicode_combining_class(0xFE00, 0xFE0F, 0).	% Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
unicode_combining_class(0xFE10, 0xFE16, 0).	% Po   [7] PRESENTATION FORM FOR VERTICAL COMMA..PRESENTATION FORM FOR VERTICAL QUESTION MARK
unicode_combining_class(0xFE17, 0xFE17, 0).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE LENTICULAR BRACKET
unicode_combining_class(0xFE18, 0xFE18, 0).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET
unicode_combining_class(0xFE19, 0xFE19, 0).	% Po       PRESENTATION FORM FOR VERTICAL HORIZONTAL ELLIPSIS
unicode_combining_class(0xFE30, 0xFE30, 0).	% Po       PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
unicode_combining_class(0xFE31, 0xFE32, 0).	% Pd   [2] PRESENTATION FORM FOR VERTICAL EM DASH..PRESENTATION FORM FOR VERTICAL EN DASH
unicode_combining_class(0xFE33, 0xFE34, 0).	% Pc   [2] PRESENTATION FORM FOR VERTICAL LOW LINE..PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
unicode_combining_class(0xFE35, 0xFE35, 0).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
unicode_combining_class(0xFE36, 0xFE36, 0).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
unicode_combining_class(0xFE37, 0xFE37, 0).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
unicode_combining_class(0xFE38, 0xFE38, 0).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
unicode_combining_class(0xFE39, 0xFE39, 0).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
unicode_combining_class(0xFE3A, 0xFE3A, 0).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
unicode_combining_class(0xFE3B, 0xFE3B, 0).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
unicode_combining_class(0xFE3C, 0xFE3C, 0).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
unicode_combining_class(0xFE3D, 0xFE3D, 0).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
unicode_combining_class(0xFE3E, 0xFE3E, 0).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
unicode_combining_class(0xFE3F, 0xFE3F, 0).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
unicode_combining_class(0xFE40, 0xFE40, 0).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
unicode_combining_class(0xFE41, 0xFE41, 0).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
unicode_combining_class(0xFE42, 0xFE42, 0).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
unicode_combining_class(0xFE43, 0xFE43, 0).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
unicode_combining_class(0xFE44, 0xFE44, 0).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
unicode_combining_class(0xFE45, 0xFE46, 0).	% Po   [2] SESAME DOT..WHITE SESAME DOT
unicode_combining_class(0xFE47, 0xFE47, 0).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT SQUARE BRACKET
unicode_combining_class(0xFE48, 0xFE48, 0).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT SQUARE BRACKET
unicode_combining_class(0xFE49, 0xFE4C, 0).	% Po   [4] DASHED OVERLINE..DOUBLE WAVY OVERLINE
unicode_combining_class(0xFE4D, 0xFE4F, 0).	% Pc   [3] DASHED LOW LINE..WAVY LOW LINE
unicode_combining_class(0xFE50, 0xFE52, 0).	% Po   [3] SMALL COMMA..SMALL FULL STOP
unicode_combining_class(0xFE54, 0xFE57, 0).	% Po   [4] SMALL SEMICOLON..SMALL EXCLAMATION MARK
unicode_combining_class(0xFE58, 0xFE58, 0).	% Pd       SMALL EM DASH
unicode_combining_class(0xFE59, 0xFE59, 0).	% Ps       SMALL LEFT PARENTHESIS
unicode_combining_class(0xFE5A, 0xFE5A, 0).	% Pe       SMALL RIGHT PARENTHESIS
unicode_combining_class(0xFE5B, 0xFE5B, 0).	% Ps       SMALL LEFT CURLY BRACKET
unicode_combining_class(0xFE5C, 0xFE5C, 0).	% Pe       SMALL RIGHT CURLY BRACKET
unicode_combining_class(0xFE5D, 0xFE5D, 0).	% Ps       SMALL LEFT TORTOISE SHELL BRACKET
unicode_combining_class(0xFE5E, 0xFE5E, 0).	% Pe       SMALL RIGHT TORTOISE SHELL BRACKET
unicode_combining_class(0xFE5F, 0xFE61, 0).	% Po   [3] SMALL NUMBER SIGN..SMALL ASTERISK
unicode_combining_class(0xFE62, 0xFE62, 0).	% Sm       SMALL PLUS SIGN
unicode_combining_class(0xFE63, 0xFE63, 0).	% Pd       SMALL HYPHEN-MINUS
unicode_combining_class(0xFE64, 0xFE66, 0).	% Sm   [3] SMALL LESS-THAN SIGN..SMALL EQUALS SIGN
unicode_combining_class(0xFE68, 0xFE68, 0).	% Po       SMALL REVERSE SOLIDUS
unicode_combining_class(0xFE69, 0xFE69, 0).	% Sc       SMALL DOLLAR SIGN
unicode_combining_class(0xFE6A, 0xFE6B, 0).	% Po   [2] SMALL PERCENT SIGN..SMALL COMMERCIAL AT
unicode_combining_class(0xFE70, 0xFE74, 0).	% Lo   [5] ARABIC FATHATAN ISOLATED FORM..ARABIC KASRATAN ISOLATED FORM
unicode_combining_class(0xFE76, 0xFEFC, 0).	% Lo [135] ARABIC FATHA ISOLATED FORM..ARABIC LIGATURE LAM WITH ALEF FINAL FORM
unicode_combining_class(0xFEFF, 0xFEFF, 0).	% Cf       ZERO WIDTH NO-BREAK SPACE
unicode_combining_class(0xFF01, 0xFF03, 0).	% Po   [3] FULLWIDTH EXCLAMATION MARK..FULLWIDTH NUMBER SIGN
unicode_combining_class(0xFF04, 0xFF04, 0).	% Sc       FULLWIDTH DOLLAR SIGN
unicode_combining_class(0xFF05, 0xFF07, 0).	% Po   [3] FULLWIDTH PERCENT SIGN..FULLWIDTH APOSTROPHE
unicode_combining_class(0xFF08, 0xFF08, 0).	% Ps       FULLWIDTH LEFT PARENTHESIS
unicode_combining_class(0xFF09, 0xFF09, 0).	% Pe       FULLWIDTH RIGHT PARENTHESIS
unicode_combining_class(0xFF0A, 0xFF0A, 0).	% Po       FULLWIDTH ASTERISK
unicode_combining_class(0xFF0B, 0xFF0B, 0).	% Sm       FULLWIDTH PLUS SIGN
unicode_combining_class(0xFF0C, 0xFF0C, 0).	% Po       FULLWIDTH COMMA
unicode_combining_class(0xFF0D, 0xFF0D, 0).	% Pd       FULLWIDTH HYPHEN-MINUS
unicode_combining_class(0xFF0E, 0xFF0F, 0).	% Po   [2] FULLWIDTH FULL STOP..FULLWIDTH SOLIDUS
unicode_combining_class(0xFF10, 0xFF19, 0).	% Nd  [10] FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
unicode_combining_class(0xFF1A, 0xFF1B, 0).	% Po   [2] FULLWIDTH COLON..FULLWIDTH SEMICOLON
unicode_combining_class(0xFF1C, 0xFF1E, 0).	% Sm   [3] FULLWIDTH LESS-THAN SIGN..FULLWIDTH GREATER-THAN SIGN
unicode_combining_class(0xFF1F, 0xFF20, 0).	% Po   [2] FULLWIDTH QUESTION MARK..FULLWIDTH COMMERCIAL AT
unicode_combining_class(0xFF21, 0xFF3A, 0).	% L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_combining_class(0xFF3B, 0xFF3B, 0).	% Ps       FULLWIDTH LEFT SQUARE BRACKET
unicode_combining_class(0xFF3C, 0xFF3C, 0).	% Po       FULLWIDTH REVERSE SOLIDUS
unicode_combining_class(0xFF3D, 0xFF3D, 0).	% Pe       FULLWIDTH RIGHT SQUARE BRACKET
unicode_combining_class(0xFF3E, 0xFF3E, 0).	% Sk       FULLWIDTH CIRCUMFLEX ACCENT
unicode_combining_class(0xFF3F, 0xFF3F, 0).	% Pc       FULLWIDTH LOW LINE
unicode_combining_class(0xFF40, 0xFF40, 0).	% Sk       FULLWIDTH GRAVE ACCENT
unicode_combining_class(0xFF41, 0xFF5A, 0).	% L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_combining_class(0xFF5B, 0xFF5B, 0).	% Ps       FULLWIDTH LEFT CURLY BRACKET
unicode_combining_class(0xFF5C, 0xFF5C, 0).	% Sm       FULLWIDTH VERTICAL LINE
unicode_combining_class(0xFF5D, 0xFF5D, 0).	% Pe       FULLWIDTH RIGHT CURLY BRACKET
unicode_combining_class(0xFF5E, 0xFF5E, 0).	% Sm       FULLWIDTH TILDE
unicode_combining_class(0xFF5F, 0xFF5F, 0).	% Ps       FULLWIDTH LEFT WHITE PARENTHESIS
unicode_combining_class(0xFF60, 0xFF60, 0).	% Pe       FULLWIDTH RIGHT WHITE PARENTHESIS
unicode_combining_class(0xFF61, 0xFF61, 0).	% Po       HALFWIDTH IDEOGRAPHIC FULL STOP
unicode_combining_class(0xFF62, 0xFF62, 0).	% Ps       HALFWIDTH LEFT CORNER BRACKET
unicode_combining_class(0xFF63, 0xFF63, 0).	% Pe       HALFWIDTH RIGHT CORNER BRACKET
unicode_combining_class(0xFF64, 0xFF65, 0).	% Po   [2] HALFWIDTH IDEOGRAPHIC COMMA..HALFWIDTH KATAKANA MIDDLE DOT
unicode_combining_class(0xFF66, 0xFF6F, 0).	% Lo  [10] HALFWIDTH KATAKANA LETTER WO..HALFWIDTH KATAKANA LETTER SMALL TU
unicode_combining_class(0xFF70, 0xFF70, 0).	% Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_combining_class(0xFF71, 0xFF9D, 0).	% Lo  [45] HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N
unicode_combining_class(0xFF9E, 0xFF9F, 0).	% Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
unicode_combining_class(0xFFA0, 0xFFBE, 0).	% Lo  [31] HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH
unicode_combining_class(0xFFC2, 0xFFC7, 0).	% Lo   [6] HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E
unicode_combining_class(0xFFCA, 0xFFCF, 0).	% Lo   [6] HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE
unicode_combining_class(0xFFD2, 0xFFD7, 0).	% Lo   [6] HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU
unicode_combining_class(0xFFDA, 0xFFDC, 0).	% Lo   [3] HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I
unicode_combining_class(0xFFE0, 0xFFE1, 0).	% Sc   [2] FULLWIDTH CENT SIGN..FULLWIDTH POUND SIGN
unicode_combining_class(0xFFE2, 0xFFE2, 0).	% Sm       FULLWIDTH NOT SIGN
unicode_combining_class(0xFFE3, 0xFFE3, 0).	% Sk       FULLWIDTH MACRON
unicode_combining_class(0xFFE4, 0xFFE4, 0).	% So       FULLWIDTH BROKEN BAR
unicode_combining_class(0xFFE5, 0xFFE6, 0).	% Sc   [2] FULLWIDTH YEN SIGN..FULLWIDTH WON SIGN
unicode_combining_class(0xFFE8, 0xFFE8, 0).	% So       HALFWIDTH FORMS LIGHT VERTICAL
unicode_combining_class(0xFFE9, 0xFFEC, 0).	% Sm   [4] HALFWIDTH LEFTWARDS ARROW..HALFWIDTH DOWNWARDS ARROW
unicode_combining_class(0xFFED, 0xFFEE, 0).	% So   [2] HALFWIDTH BLACK SQUARE..HALFWIDTH WHITE CIRCLE
unicode_combining_class(0xFFF9, 0xFFFB, 0).	% Cf   [3] INTERLINEAR ANNOTATION ANCHOR..INTERLINEAR ANNOTATION TERMINATOR
unicode_combining_class(0xFFFC, 0xFFFD, 0).	% So   [2] OBJECT REPLACEMENT CHARACTER..REPLACEMENT CHARACTER
unicode_combining_class(0x10000, 0x1000B, 0).	% Lo  [12] LINEAR B SYLLABLE B008 A..LINEAR B SYLLABLE B046 JE
unicode_combining_class(0x1000D, 0x10026, 0).	% Lo  [26] LINEAR B SYLLABLE B036 JO..LINEAR B SYLLABLE B032 QO
unicode_combining_class(0x10028, 0x1003A, 0).	% Lo  [19] LINEAR B SYLLABLE B060 RA..LINEAR B SYLLABLE B042 WO
unicode_combining_class(0x1003C, 0x1003D, 0).	% Lo   [2] LINEAR B SYLLABLE B017 ZA..LINEAR B SYLLABLE B074 ZE
unicode_combining_class(0x1003F, 0x1004D, 0).	% Lo  [15] LINEAR B SYLLABLE B020 ZO..LINEAR B SYLLABLE B091 TWO
unicode_combining_class(0x10050, 0x1005D, 0).	% Lo  [14] LINEAR B SYMBOL B018..LINEAR B SYMBOL B089
unicode_combining_class(0x10080, 0x100FA, 0).	% Lo [123] LINEAR B IDEOGRAM B100 MAN..LINEAR B IDEOGRAM VESSEL B305
unicode_combining_class(0x10100, 0x10102, 0).	% Po   [3] AEGEAN WORD SEPARATOR LINE..AEGEAN CHECK MARK
unicode_combining_class(0x10107, 0x10133, 0).	% No  [45] AEGEAN NUMBER ONE..AEGEAN NUMBER NINETY THOUSAND
unicode_combining_class(0x10137, 0x1013F, 0).	% So   [9] AEGEAN WEIGHT BASE UNIT..AEGEAN MEASURE THIRD SUBUNIT
unicode_combining_class(0x10140, 0x10174, 0).	% Nl  [53] GREEK ACROPHONIC ATTIC ONE QUARTER..GREEK ACROPHONIC STRATIAN FIFTY MNAS
unicode_combining_class(0x10175, 0x10178, 0).	% No   [4] GREEK ONE HALF SIGN..GREEK THREE QUARTERS SIGN
unicode_combining_class(0x10179, 0x10189, 0).	% So  [17] GREEK YEAR SIGN..GREEK TRYBLION BASE SIGN
unicode_combining_class(0x1018A, 0x1018A, 0).	% No       GREEK ZERO SIGN
unicode_combining_class(0x10190, 0x1019B, 0).	% So  [12] ROMAN SEXTANS SIGN..ROMAN CENTURIAL SIGN
unicode_combining_class(0x101D0, 0x101FC, 0).	% So  [45] PHAISTOS DISC SIGN PEDESTRIAN..PHAISTOS DISC SIGN WAVY BAND
unicode_combining_class(0x10280, 0x1029C, 0).	% Lo  [29] LYCIAN LETTER A..LYCIAN LETTER X
unicode_combining_class(0x102A0, 0x102D0, 0).	% Lo  [49] CARIAN LETTER A..CARIAN LETTER UUU3
unicode_combining_class(0x10300, 0x1031E, 0).	% Lo  [31] OLD ITALIC LETTER A..OLD ITALIC LETTER UU
unicode_combining_class(0x10320, 0x10323, 0).	% No   [4] OLD ITALIC NUMERAL ONE..OLD ITALIC NUMERAL FIFTY
unicode_combining_class(0x10330, 0x10340, 0).	% Lo  [17] GOTHIC LETTER AHSA..GOTHIC LETTER PAIRTHRA
unicode_combining_class(0x10341, 0x10341, 0).	% Nl       GOTHIC LETTER NINETY
unicode_combining_class(0x10342, 0x10349, 0).	% Lo   [8] GOTHIC LETTER RAIDA..GOTHIC LETTER OTHAL
unicode_combining_class(0x1034A, 0x1034A, 0).	% Nl       GOTHIC LETTER NINE HUNDRED
unicode_combining_class(0x10380, 0x1039D, 0).	% Lo  [30] UGARITIC LETTER ALPA..UGARITIC LETTER SSU
unicode_combining_class(0x1039F, 0x1039F, 0).	% Po       UGARITIC WORD DIVIDER
unicode_combining_class(0x103A0, 0x103C3, 0).	% Lo  [36] OLD PERSIAN SIGN A..OLD PERSIAN SIGN HA
unicode_combining_class(0x103C8, 0x103CF, 0).	% Lo   [8] OLD PERSIAN SIGN AURAMAZDAA..OLD PERSIAN SIGN BUUMISH
unicode_combining_class(0x103D0, 0x103D0, 0).	% Po       OLD PERSIAN WORD DIVIDER
unicode_combining_class(0x103D1, 0x103D5, 0).	% Nl   [5] OLD PERSIAN NUMBER ONE..OLD PERSIAN NUMBER HUNDRED
unicode_combining_class(0x10400, 0x1044F, 0).	% L&  [80] DESERET CAPITAL LETTER LONG I..DESERET SMALL LETTER EW
unicode_combining_class(0x10450, 0x1049D, 0).	% Lo  [78] SHAVIAN LETTER PEEP..OSMANYA LETTER OO
unicode_combining_class(0x104A0, 0x104A9, 0).	% Nd  [10] OSMANYA DIGIT ZERO..OSMANYA DIGIT NINE
unicode_combining_class(0x10800, 0x10805, 0).	% Lo   [6] CYPRIOT SYLLABLE A..CYPRIOT SYLLABLE JA
unicode_combining_class(0x10808, 0x10808, 0).	% Lo       CYPRIOT SYLLABLE JO
unicode_combining_class(0x1080A, 0x10835, 0).	% Lo  [44] CYPRIOT SYLLABLE KA..CYPRIOT SYLLABLE WO
unicode_combining_class(0x10837, 0x10838, 0).	% Lo   [2] CYPRIOT SYLLABLE XA..CYPRIOT SYLLABLE XE
unicode_combining_class(0x1083C, 0x1083C, 0).	% Lo       CYPRIOT SYLLABLE ZA
unicode_combining_class(0x1083F, 0x10855, 0).	% Lo  [23] CYPRIOT SYLLABLE ZO..IMPERIAL ARAMAIC LETTER TAW
unicode_combining_class(0x10857, 0x10857, 0).	% Po       IMPERIAL ARAMAIC SECTION SIGN
unicode_combining_class(0x10858, 0x1085F, 0).	% No   [8] IMPERIAL ARAMAIC NUMBER ONE..IMPERIAL ARAMAIC NUMBER TEN THOUSAND
unicode_combining_class(0x10900, 0x10915, 0).	% Lo  [22] PHOENICIAN LETTER ALF..PHOENICIAN LETTER TAU
unicode_combining_class(0x10916, 0x1091B, 0).	% No   [6] PHOENICIAN NUMBER ONE..PHOENICIAN NUMBER THREE
unicode_combining_class(0x1091F, 0x1091F, 0).	% Po       PHOENICIAN WORD SEPARATOR
unicode_combining_class(0x10920, 0x10939, 0).	% Lo  [26] LYDIAN LETTER A..LYDIAN LETTER C
unicode_combining_class(0x1093F, 0x1093F, 0).	% Po       LYDIAN TRIANGULAR MARK
unicode_combining_class(0x10980, 0x109B7, 0).	% Lo  [56] MEROITIC HIEROGLYPHIC LETTER A..MEROITIC CURSIVE LETTER DA
unicode_combining_class(0x109BE, 0x109BF, 0).	% Lo   [2] MEROITIC CURSIVE LOGOGRAM RMT..MEROITIC CURSIVE LOGOGRAM IMN
unicode_combining_class(0x10A00, 0x10A00, 0).	% Lo       KHAROSHTHI LETTER A
unicode_combining_class(0x10A01, 0x10A03, 0).	% Mn   [3] KHAROSHTHI VOWEL SIGN I..KHAROSHTHI VOWEL SIGN VOCALIC R
unicode_combining_class(0x10A05, 0x10A06, 0).	% Mn   [2] KHAROSHTHI VOWEL SIGN E..KHAROSHTHI VOWEL SIGN O
unicode_combining_class(0x10A0C, 0x10A0C, 0).	% Mn       KHAROSHTHI VOWEL LENGTH MARK
unicode_combining_class(0x10A0E, 0x10A0E, 0).	% Mn       KHAROSHTHI SIGN ANUSVARA
unicode_combining_class(0x10A10, 0x10A13, 0).	% Lo   [4] KHAROSHTHI LETTER KA..KHAROSHTHI LETTER GHA
unicode_combining_class(0x10A15, 0x10A17, 0).	% Lo   [3] KHAROSHTHI LETTER CA..KHAROSHTHI LETTER JA
unicode_combining_class(0x10A19, 0x10A33, 0).	% Lo  [27] KHAROSHTHI LETTER NYA..KHAROSHTHI LETTER TTTHA
unicode_combining_class(0x10A40, 0x10A47, 0).	% No   [8] KHAROSHTHI DIGIT ONE..KHAROSHTHI NUMBER ONE THOUSAND
unicode_combining_class(0x10A50, 0x10A58, 0).	% Po   [9] KHAROSHTHI PUNCTUATION DOT..KHAROSHTHI PUNCTUATION LINES
unicode_combining_class(0x10A60, 0x10A7C, 0).	% Lo  [29] OLD SOUTH ARABIAN LETTER HE..OLD SOUTH ARABIAN LETTER THETH
unicode_combining_class(0x10A7D, 0x10A7E, 0).	% No   [2] OLD SOUTH ARABIAN NUMBER ONE..OLD SOUTH ARABIAN NUMBER FIFTY
unicode_combining_class(0x10A7F, 0x10A7F, 0).	% Po       OLD SOUTH ARABIAN NUMERIC INDICATOR
unicode_combining_class(0x10B00, 0x10B35, 0).	% Lo  [54] AVESTAN LETTER A..AVESTAN LETTER HE
unicode_combining_class(0x10B39, 0x10B3F, 0).	% Po   [7] AVESTAN ABBREVIATION MARK..LARGE ONE RING OVER TWO RINGS PUNCTUATION
unicode_combining_class(0x10B40, 0x10B55, 0).	% Lo  [22] INSCRIPTIONAL PARTHIAN LETTER ALEPH..INSCRIPTIONAL PARTHIAN LETTER TAW
unicode_combining_class(0x10B58, 0x10B5F, 0).	% No   [8] INSCRIPTIONAL PARTHIAN NUMBER ONE..INSCRIPTIONAL PARTHIAN NUMBER ONE THOUSAND
unicode_combining_class(0x10B60, 0x10B72, 0).	% Lo  [19] INSCRIPTIONAL PAHLAVI LETTER ALEPH..INSCRIPTIONAL PAHLAVI LETTER TAW
unicode_combining_class(0x10B78, 0x10B7F, 0).	% No   [8] INSCRIPTIONAL PAHLAVI NUMBER ONE..INSCRIPTIONAL PAHLAVI NUMBER ONE THOUSAND
unicode_combining_class(0x10C00, 0x10C48, 0).	% Lo  [73] OLD TURKIC LETTER ORKHON A..OLD TURKIC LETTER ORKHON BASH
unicode_combining_class(0x10E60, 0x10E7E, 0).	% No  [31] RUMI DIGIT ONE..RUMI FRACTION TWO THIRDS
unicode_combining_class(0x11000, 0x11000, 0).	% Mc       BRAHMI SIGN CANDRABINDU
unicode_combining_class(0x11001, 0x11001, 0).	% Mn       BRAHMI SIGN ANUSVARA
unicode_combining_class(0x11002, 0x11002, 0).	% Mc       BRAHMI SIGN VISARGA
unicode_combining_class(0x11003, 0x11037, 0).	% Lo  [53] BRAHMI SIGN JIHVAMULIYA..BRAHMI LETTER OLD TAMIL NNNA
unicode_combining_class(0x11038, 0x11045, 0).	% Mn  [14] BRAHMI VOWEL SIGN AA..BRAHMI VOWEL SIGN AU
unicode_combining_class(0x11047, 0x1104D, 0).	% Po   [7] BRAHMI DANDA..BRAHMI PUNCTUATION LOTUS
unicode_combining_class(0x11052, 0x11065, 0).	% No  [20] BRAHMI NUMBER ONE..BRAHMI NUMBER ONE THOUSAND
unicode_combining_class(0x11066, 0x1106F, 0).	% Nd  [10] BRAHMI DIGIT ZERO..BRAHMI DIGIT NINE
unicode_combining_class(0x11080, 0x11081, 0).	% Mn   [2] KAITHI SIGN CANDRABINDU..KAITHI SIGN ANUSVARA
unicode_combining_class(0x11082, 0x11082, 0).	% Mc       KAITHI SIGN VISARGA
unicode_combining_class(0x11083, 0x110AF, 0).	% Lo  [45] KAITHI LETTER A..KAITHI LETTER HA
unicode_combining_class(0x110B0, 0x110B2, 0).	% Mc   [3] KAITHI VOWEL SIGN AA..KAITHI VOWEL SIGN II
unicode_combining_class(0x110B3, 0x110B6, 0).	% Mn   [4] KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN AI
unicode_combining_class(0x110B7, 0x110B8, 0).	% Mc   [2] KAITHI VOWEL SIGN O..KAITHI VOWEL SIGN AU
unicode_combining_class(0x110BB, 0x110BC, 0).	% Po   [2] KAITHI ABBREVIATION SIGN..KAITHI ENUMERATION SIGN
unicode_combining_class(0x110BD, 0x110BD, 0).	% Cf       KAITHI NUMBER SIGN
unicode_combining_class(0x110BE, 0x110C1, 0).	% Po   [4] KAITHI SECTION MARK..KAITHI DOUBLE DANDA
unicode_combining_class(0x110D0, 0x110E8, 0).	% Lo  [25] SORA SOMPENG LETTER SAH..SORA SOMPENG LETTER MAE
unicode_combining_class(0x110F0, 0x110F9, 0).	% Nd  [10] SORA SOMPENG DIGIT ZERO..SORA SOMPENG DIGIT NINE
unicode_combining_class(0x11103, 0x11126, 0).	% Lo  [36] CHAKMA LETTER AA..CHAKMA LETTER HAA
unicode_combining_class(0x11127, 0x1112B, 0).	% Mn   [5] CHAKMA VOWEL SIGN A..CHAKMA VOWEL SIGN UU
unicode_combining_class(0x1112C, 0x1112C, 0).	% Mc       CHAKMA VOWEL SIGN E
unicode_combining_class(0x1112D, 0x11132, 0).	% Mn   [6] CHAKMA VOWEL SIGN AI..CHAKMA AU MARK
unicode_combining_class(0x11136, 0x1113F, 0).	% Nd  [10] CHAKMA DIGIT ZERO..CHAKMA DIGIT NINE
unicode_combining_class(0x11140, 0x11143, 0).	% Po   [4] CHAKMA SECTION MARK..CHAKMA QUESTION MARK
unicode_combining_class(0x11180, 0x11181, 0).	% Mn   [2] SHARADA SIGN CANDRABINDU..SHARADA SIGN ANUSVARA
unicode_combining_class(0x11182, 0x11182, 0).	% Mc       SHARADA SIGN VISARGA
unicode_combining_class(0x11183, 0x111B2, 0).	% Lo  [48] SHARADA LETTER A..SHARADA LETTER HA
unicode_combining_class(0x111B3, 0x111B5, 0).	% Mc   [3] SHARADA VOWEL SIGN AA..SHARADA VOWEL SIGN II
unicode_combining_class(0x111B6, 0x111BE, 0).	% Mn   [9] SHARADA VOWEL SIGN U..SHARADA VOWEL SIGN O
unicode_combining_class(0x111BF, 0x111BF, 0).	% Mc       SHARADA VOWEL SIGN AU
unicode_combining_class(0x111C1, 0x111C4, 0).	% Lo   [4] SHARADA SIGN AVAGRAHA..SHARADA OM
unicode_combining_class(0x111C5, 0x111C8, 0).	% Po   [4] SHARADA DANDA..SHARADA SEPARATOR
unicode_combining_class(0x111D0, 0x111D9, 0).	% Nd  [10] SHARADA DIGIT ZERO..SHARADA DIGIT NINE
unicode_combining_class(0x11680, 0x116AA, 0).	% Lo  [43] TAKRI LETTER A..TAKRI LETTER RRA
unicode_combining_class(0x116AB, 0x116AB, 0).	% Mn       TAKRI SIGN ANUSVARA
unicode_combining_class(0x116AC, 0x116AC, 0).	% Mc       TAKRI SIGN VISARGA
unicode_combining_class(0x116AD, 0x116AD, 0).	% Mn       TAKRI VOWEL SIGN AA
unicode_combining_class(0x116AE, 0x116AF, 0).	% Mc   [2] TAKRI VOWEL SIGN I..TAKRI VOWEL SIGN II
unicode_combining_class(0x116B0, 0x116B5, 0).	% Mn   [6] TAKRI VOWEL SIGN U..TAKRI VOWEL SIGN AU
unicode_combining_class(0x116C0, 0x116C9, 0).	% Nd  [10] TAKRI DIGIT ZERO..TAKRI DIGIT NINE
unicode_combining_class(0x12000, 0x1236E, 0).	% Lo [879] CUNEIFORM SIGN A..CUNEIFORM SIGN ZUM
unicode_combining_class(0x12400, 0x12462, 0).	% Nl  [99] CUNEIFORM NUMERIC SIGN TWO ASH..CUNEIFORM NUMERIC SIGN OLD ASSYRIAN ONE QUARTER
unicode_combining_class(0x12470, 0x12473, 0).	% Po   [4] CUNEIFORM PUNCTUATION SIGN OLD ASSYRIAN WORD DIVIDER..CUNEIFORM PUNCTUATION SIGN DIAGONAL TRICOLON
unicode_combining_class(0x13000, 0x1342E, 0).	% Lo [1071] EGYPTIAN HIEROGLYPH A001..EGYPTIAN HIEROGLYPH AA032
unicode_combining_class(0x16800, 0x16A38, 0).	% Lo [569] BAMUM LETTER PHASE-A NGKUE MFON..BAMUM LETTER PHASE-F VUEQ
unicode_combining_class(0x16F00, 0x16F44, 0).	% Lo  [69] MIAO LETTER PA..MIAO LETTER HHA
unicode_combining_class(0x16F50, 0x16F50, 0).	% Lo       MIAO LETTER NASALIZATION
unicode_combining_class(0x16F51, 0x16F7E, 0).	% Mc  [46] MIAO SIGN ASPIRATION..MIAO VOWEL SIGN NG
unicode_combining_class(0x16F8F, 0x16F92, 0).	% Mn   [4] MIAO TONE RIGHT..MIAO TONE BELOW
unicode_combining_class(0x16F93, 0x16F9F, 0).	% Lm  [13] MIAO LETTER TONE-2..MIAO LETTER REFORMED TONE-8
unicode_combining_class(0x1B000, 0x1B001, 0).	% Lo   [2] KATAKANA LETTER ARCHAIC E..HIRAGANA LETTER ARCHAIC YE
unicode_combining_class(0x1D000, 0x1D0F5, 0).	% So [246] BYZANTINE MUSICAL SYMBOL PSILI..BYZANTINE MUSICAL SYMBOL GORGON NEO KATO
unicode_combining_class(0x1D100, 0x1D126, 0).	% So  [39] MUSICAL SYMBOL SINGLE BARLINE..MUSICAL SYMBOL DRUM CLEF-2
unicode_combining_class(0x1D129, 0x1D164, 0).	% So  [60] MUSICAL SYMBOL MULTIPLE MEASURE REST..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
unicode_combining_class(0x1D16A, 0x1D16C, 0).	% So   [3] MUSICAL SYMBOL FINGERED TREMOLO-1..MUSICAL SYMBOL FINGERED TREMOLO-3
unicode_combining_class(0x1D173, 0x1D17A, 0).	% Cf   [8] MUSICAL SYMBOL BEGIN BEAM..MUSICAL SYMBOL END PHRASE
unicode_combining_class(0x1D183, 0x1D184, 0).	% So   [2] MUSICAL SYMBOL ARPEGGIATO UP..MUSICAL SYMBOL ARPEGGIATO DOWN
unicode_combining_class(0x1D18C, 0x1D1A9, 0).	% So  [30] MUSICAL SYMBOL RINFORZANDO..MUSICAL SYMBOL DEGREE SLASH
unicode_combining_class(0x1D1AE, 0x1D1DD, 0).	% So  [48] MUSICAL SYMBOL PEDAL MARK..MUSICAL SYMBOL PES SUBPUNCTIS
unicode_combining_class(0x1D200, 0x1D241, 0).	% So  [66] GREEK VOCAL NOTATION SYMBOL-1..GREEK INSTRUMENTAL NOTATION SYMBOL-54
unicode_combining_class(0x1D245, 0x1D245, 0).	% So       GREEK MUSICAL LEIMMA
unicode_combining_class(0x1D300, 0x1D356, 0).	% So  [87] MONOGRAM FOR EARTH..TETRAGRAM FOR FOSTERING
unicode_combining_class(0x1D360, 0x1D371, 0).	% No  [18] COUNTING ROD UNIT DIGIT ONE..COUNTING ROD TENS DIGIT NINE
unicode_combining_class(0x1D400, 0x1D454, 0).	% L&  [85] MATHEMATICAL BOLD CAPITAL A..MATHEMATICAL ITALIC SMALL G
unicode_combining_class(0x1D456, 0x1D49C, 0).	% L&  [71] MATHEMATICAL ITALIC SMALL I..MATHEMATICAL SCRIPT CAPITAL A
unicode_combining_class(0x1D49E, 0x1D49F, 0).	% L&   [2] MATHEMATICAL SCRIPT CAPITAL C..MATHEMATICAL SCRIPT CAPITAL D
unicode_combining_class(0x1D4A2, 0x1D4A2, 0).	% L&       MATHEMATICAL SCRIPT CAPITAL G
unicode_combining_class(0x1D4A5, 0x1D4A6, 0).	% L&   [2] MATHEMATICAL SCRIPT CAPITAL J..MATHEMATICAL SCRIPT CAPITAL K
unicode_combining_class(0x1D4A9, 0x1D4AC, 0).	% L&   [4] MATHEMATICAL SCRIPT CAPITAL N..MATHEMATICAL SCRIPT CAPITAL Q
unicode_combining_class(0x1D4AE, 0x1D4B9, 0).	% L&  [12] MATHEMATICAL SCRIPT CAPITAL S..MATHEMATICAL SCRIPT SMALL D
unicode_combining_class(0x1D4BB, 0x1D4BB, 0).	% L&       MATHEMATICAL SCRIPT SMALL F
unicode_combining_class(0x1D4BD, 0x1D4C3, 0).	% L&   [7] MATHEMATICAL SCRIPT SMALL H..MATHEMATICAL SCRIPT SMALL N
unicode_combining_class(0x1D4C5, 0x1D505, 0).	% L&  [65] MATHEMATICAL SCRIPT SMALL P..MATHEMATICAL FRAKTUR CAPITAL B
unicode_combining_class(0x1D507, 0x1D50A, 0).	% L&   [4] MATHEMATICAL FRAKTUR CAPITAL D..MATHEMATICAL FRAKTUR CAPITAL G
unicode_combining_class(0x1D50D, 0x1D514, 0).	% L&   [8] MATHEMATICAL FRAKTUR CAPITAL J..MATHEMATICAL FRAKTUR CAPITAL Q
unicode_combining_class(0x1D516, 0x1D51C, 0).	% L&   [7] MATHEMATICAL FRAKTUR CAPITAL S..MATHEMATICAL FRAKTUR CAPITAL Y
unicode_combining_class(0x1D51E, 0x1D539, 0).	% L&  [28] MATHEMATICAL FRAKTUR SMALL A..MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_combining_class(0x1D53B, 0x1D53E, 0).	% L&   [4] MATHEMATICAL DOUBLE-STRUCK CAPITAL D..MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_combining_class(0x1D540, 0x1D544, 0).	% L&   [5] MATHEMATICAL DOUBLE-STRUCK CAPITAL I..MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_combining_class(0x1D546, 0x1D546, 0).	% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_combining_class(0x1D54A, 0x1D550, 0).	% L&   [7] MATHEMATICAL DOUBLE-STRUCK CAPITAL S..MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_combining_class(0x1D552, 0x1D6A5, 0).	% L& [340] MATHEMATICAL DOUBLE-STRUCK SMALL A..MATHEMATICAL ITALIC SMALL DOTLESS J
unicode_combining_class(0x1D6A8, 0x1D6C0, 0).	% L&  [25] MATHEMATICAL BOLD CAPITAL ALPHA..MATHEMATICAL BOLD CAPITAL OMEGA
unicode_combining_class(0x1D6C1, 0x1D6C1, 0).	% Sm       MATHEMATICAL BOLD NABLA
unicode_combining_class(0x1D6C2, 0x1D6DA, 0).	% L&  [25] MATHEMATICAL BOLD SMALL ALPHA..MATHEMATICAL BOLD SMALL OMEGA
unicode_combining_class(0x1D6DB, 0x1D6DB, 0).	% Sm       MATHEMATICAL BOLD PARTIAL DIFFERENTIAL
unicode_combining_class(0x1D6DC, 0x1D6FA, 0).	% L&  [31] MATHEMATICAL BOLD EPSILON SYMBOL..MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_combining_class(0x1D6FB, 0x1D6FB, 0).	% Sm       MATHEMATICAL ITALIC NABLA
unicode_combining_class(0x1D6FC, 0x1D714, 0).	% L&  [25] MATHEMATICAL ITALIC SMALL ALPHA..MATHEMATICAL ITALIC SMALL OMEGA
unicode_combining_class(0x1D715, 0x1D715, 0).	% Sm       MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL
unicode_combining_class(0x1D716, 0x1D734, 0).	% L&  [31] MATHEMATICAL ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_combining_class(0x1D735, 0x1D735, 0).	% Sm       MATHEMATICAL BOLD ITALIC NABLA
unicode_combining_class(0x1D736, 0x1D74E, 0).	% L&  [25] MATHEMATICAL BOLD ITALIC SMALL ALPHA..MATHEMATICAL BOLD ITALIC SMALL OMEGA
unicode_combining_class(0x1D74F, 0x1D74F, 0).	% Sm       MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_combining_class(0x1D750, 0x1D76E, 0).	% L&  [31] MATHEMATICAL BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_combining_class(0x1D76F, 0x1D76F, 0).	% Sm       MATHEMATICAL SANS-SERIF BOLD NABLA
unicode_combining_class(0x1D770, 0x1D788, 0).	% L&  [25] MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
unicode_combining_class(0x1D789, 0x1D789, 0).	% Sm       MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL
unicode_combining_class(0x1D78A, 0x1D7A8, 0).	% L&  [31] MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_combining_class(0x1D7A9, 0x1D7A9, 0).	% Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA
unicode_combining_class(0x1D7AA, 0x1D7C2, 0).	% L&  [25] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
unicode_combining_class(0x1D7C3, 0x1D7C3, 0).	% Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_combining_class(0x1D7C4, 0x1D7CB, 0).	% L&   [8] MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD SMALL DIGAMMA
unicode_combining_class(0x1D7CE, 0x1D7FF, 0).	% Nd  [50] MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
unicode_combining_class(0x1EE00, 0x1EE03, 0).	% Lo   [4] ARABIC MATHEMATICAL ALEF..ARABIC MATHEMATICAL DAL
unicode_combining_class(0x1EE05, 0x1EE1F, 0).	% Lo  [27] ARABIC MATHEMATICAL WAW..ARABIC MATHEMATICAL DOTLESS QAF
unicode_combining_class(0x1EE21, 0x1EE22, 0).	% Lo   [2] ARABIC MATHEMATICAL INITIAL BEH..ARABIC MATHEMATICAL INITIAL JEEM
unicode_combining_class(0x1EE24, 0x1EE24, 0).	% Lo       ARABIC MATHEMATICAL INITIAL HEH
unicode_combining_class(0x1EE27, 0x1EE27, 0).	% Lo       ARABIC MATHEMATICAL INITIAL HAH
unicode_combining_class(0x1EE29, 0x1EE32, 0).	% Lo  [10] ARABIC MATHEMATICAL INITIAL YEH..ARABIC MATHEMATICAL INITIAL QAF
unicode_combining_class(0x1EE34, 0x1EE37, 0).	% Lo   [4] ARABIC MATHEMATICAL INITIAL SHEEN..ARABIC MATHEMATICAL INITIAL KHAH
unicode_combining_class(0x1EE39, 0x1EE39, 0).	% Lo       ARABIC MATHEMATICAL INITIAL DAD
unicode_combining_class(0x1EE3B, 0x1EE3B, 0).	% Lo       ARABIC MATHEMATICAL INITIAL GHAIN
unicode_combining_class(0x1EE42, 0x1EE42, 0).	% Lo       ARABIC MATHEMATICAL TAILED JEEM
unicode_combining_class(0x1EE47, 0x1EE47, 0).	% Lo       ARABIC MATHEMATICAL TAILED HAH
unicode_combining_class(0x1EE49, 0x1EE49, 0).	% Lo       ARABIC MATHEMATICAL TAILED YEH
unicode_combining_class(0x1EE4B, 0x1EE4B, 0).	% Lo       ARABIC MATHEMATICAL TAILED LAM
unicode_combining_class(0x1EE4D, 0x1EE4F, 0).	% Lo   [3] ARABIC MATHEMATICAL TAILED NOON..ARABIC MATHEMATICAL TAILED AIN
unicode_combining_class(0x1EE51, 0x1EE52, 0).	% Lo   [2] ARABIC MATHEMATICAL TAILED SAD..ARABIC MATHEMATICAL TAILED QAF
unicode_combining_class(0x1EE54, 0x1EE54, 0).	% Lo       ARABIC MATHEMATICAL TAILED SHEEN
unicode_combining_class(0x1EE57, 0x1EE57, 0).	% Lo       ARABIC MATHEMATICAL TAILED KHAH
unicode_combining_class(0x1EE59, 0x1EE59, 0).	% Lo       ARABIC MATHEMATICAL TAILED DAD
unicode_combining_class(0x1EE5B, 0x1EE5B, 0).	% Lo       ARABIC MATHEMATICAL TAILED GHAIN
unicode_combining_class(0x1EE5D, 0x1EE5D, 0).	% Lo       ARABIC MATHEMATICAL TAILED DOTLESS NOON
unicode_combining_class(0x1EE5F, 0x1EE5F, 0).	% Lo       ARABIC MATHEMATICAL TAILED DOTLESS QAF
unicode_combining_class(0x1EE61, 0x1EE62, 0).	% Lo   [2] ARABIC MATHEMATICAL STRETCHED BEH..ARABIC MATHEMATICAL STRETCHED JEEM
unicode_combining_class(0x1EE64, 0x1EE64, 0).	% Lo       ARABIC MATHEMATICAL STRETCHED HEH
unicode_combining_class(0x1EE67, 0x1EE6A, 0).	% Lo   [4] ARABIC MATHEMATICAL STRETCHED HAH..ARABIC MATHEMATICAL STRETCHED KAF
unicode_combining_class(0x1EE6C, 0x1EE72, 0).	% Lo   [7] ARABIC MATHEMATICAL STRETCHED MEEM..ARABIC MATHEMATICAL STRETCHED QAF
unicode_combining_class(0x1EE74, 0x1EE77, 0).	% Lo   [4] ARABIC MATHEMATICAL STRETCHED SHEEN..ARABIC MATHEMATICAL STRETCHED KHAH
unicode_combining_class(0x1EE79, 0x1EE7C, 0).	% Lo   [4] ARABIC MATHEMATICAL STRETCHED DAD..ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
unicode_combining_class(0x1EE7E, 0x1EE7E, 0).	% Lo       ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
unicode_combining_class(0x1EE80, 0x1EE89, 0).	% Lo  [10] ARABIC MATHEMATICAL LOOPED ALEF..ARABIC MATHEMATICAL LOOPED YEH
unicode_combining_class(0x1EE8B, 0x1EE9B, 0).	% Lo  [17] ARABIC MATHEMATICAL LOOPED LAM..ARABIC MATHEMATICAL LOOPED GHAIN
unicode_combining_class(0x1EEA1, 0x1EEA3, 0).	% Lo   [3] ARABIC MATHEMATICAL DOUBLE-STRUCK BEH..ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
unicode_combining_class(0x1EEA5, 0x1EEA9, 0).	% Lo   [5] ARABIC MATHEMATICAL DOUBLE-STRUCK WAW..ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
unicode_combining_class(0x1EEAB, 0x1EEBB, 0).	% Lo  [17] ARABIC MATHEMATICAL DOUBLE-STRUCK LAM..ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN
unicode_combining_class(0x1EEF0, 0x1EEF1, 0).	% Sm   [2] ARABIC MATHEMATICAL OPERATOR MEEM WITH HAH WITH TATWEEL..ARABIC MATHEMATICAL OPERATOR HAH WITH DAL
unicode_combining_class(0x1F000, 0x1F02B, 0).	% So  [44] MAHJONG TILE EAST WIND..MAHJONG TILE BACK
unicode_combining_class(0x1F030, 0x1F093, 0).	% So [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
unicode_combining_class(0x1F0A0, 0x1F0AE, 0).	% So  [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
unicode_combining_class(0x1F0B1, 0x1F0BE, 0).	% So  [14] PLAYING CARD ACE OF HEARTS..PLAYING CARD KING OF HEARTS
unicode_combining_class(0x1F0C1, 0x1F0CF, 0).	% So  [15] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD BLACK JOKER
unicode_combining_class(0x1F0D1, 0x1F0DF, 0).	% So  [15] PLAYING CARD ACE OF CLUBS..PLAYING CARD WHITE JOKER
unicode_combining_class(0x1F100, 0x1F10A, 0).	% No  [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
unicode_combining_class(0x1F110, 0x1F12E, 0).	% So  [31] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED WZ
unicode_combining_class(0x1F130, 0x1F16B, 0).	% So  [60] SQUARED LATIN CAPITAL LETTER A..RAISED MD SIGN
unicode_combining_class(0x1F170, 0x1F19A, 0).	% So  [43] NEGATIVE SQUARED LATIN CAPITAL LETTER A..SQUARED VS
unicode_combining_class(0x1F1E6, 0x1F202, 0).	% So  [29] REGIONAL INDICATOR SYMBOL LETTER A..SQUARED KATAKANA SA
unicode_combining_class(0x1F210, 0x1F23A, 0).	% So  [43] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-55B6
unicode_combining_class(0x1F240, 0x1F248, 0).	% So   [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
unicode_combining_class(0x1F250, 0x1F251, 0).	% So   [2] CIRCLED IDEOGRAPH ADVANTAGE..CIRCLED IDEOGRAPH ACCEPT
unicode_combining_class(0x1F300, 0x1F320, 0).	% So  [33] CYCLONE..SHOOTING STAR
unicode_combining_class(0x1F330, 0x1F335, 0).	% So   [6] CHESTNUT..CACTUS
unicode_combining_class(0x1F337, 0x1F37C, 0).	% So  [70] TULIP..BABY BOTTLE
unicode_combining_class(0x1F380, 0x1F393, 0).	% So  [20] RIBBON..GRADUATION CAP
unicode_combining_class(0x1F3A0, 0x1F3C4, 0).	% So  [37] CAROUSEL HORSE..SURFER
unicode_combining_class(0x1F3C6, 0x1F3CA, 0).	% So   [5] TROPHY..SWIMMER
unicode_combining_class(0x1F3E0, 0x1F3F0, 0).	% So  [17] HOUSE BUILDING..EUROPEAN CASTLE
unicode_combining_class(0x1F400, 0x1F43E, 0).	% So  [63] RAT..PAW PRINTS
unicode_combining_class(0x1F440, 0x1F440, 0).	% So       EYES
unicode_combining_class(0x1F442, 0x1F4F7, 0).	% So [182] EAR..CAMERA
unicode_combining_class(0x1F4F9, 0x1F4FC, 0).	% So   [4] VIDEO CAMERA..VIDEOCASSETTE
unicode_combining_class(0x1F500, 0x1F53D, 0).	% So  [62] TWISTED RIGHTWARDS ARROWS..DOWN-POINTING SMALL RED TRIANGLE
unicode_combining_class(0x1F540, 0x1F543, 0).	% So   [4] CIRCLED CROSS POMMEE..NOTCHED LEFT SEMICIRCLE WITH THREE DOTS
unicode_combining_class(0x1F550, 0x1F567, 0).	% So  [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
unicode_combining_class(0x1F5FB, 0x1F640, 0).	% So  [70] MOUNT FUJI..WEARY CAT FACE
unicode_combining_class(0x1F645, 0x1F64F, 0).	% So  [11] FACE WITH NO GOOD GESTURE..PERSON WITH FOLDED HANDS
unicode_combining_class(0x1F680, 0x1F6C5, 0).	% So  [70] ROCKET..LEFT LUGGAGE
unicode_combining_class(0x1F700, 0x1F773, 0).	% So [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
unicode_combining_class(0x20000, 0x2A6D6, 0).	% Lo [42711] CJK UNIFIED IDEOGRAPH-20000..CJK UNIFIED IDEOGRAPH-2A6D6
unicode_combining_class(0x2A700, 0x2B734, 0).	% Lo [4149] CJK UNIFIED IDEOGRAPH-2A700..CJK UNIFIED IDEOGRAPH-2B734
unicode_combining_class(0x2B740, 0x2B81D, 0).	% Lo [222] CJK UNIFIED IDEOGRAPH-2B740..CJK UNIFIED IDEOGRAPH-2B81D
unicode_combining_class(0x2F800, 0x2FA1D, 0).	% Lo [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D
unicode_combining_class(0xE0001, 0xE0001, 0).	% Cf       LANGUAGE TAG
unicode_combining_class(0xE0020, 0xE007F, 0).	% Cf  [96] TAG SPACE..CANCEL TAG
unicode_combining_class(0xE0100, 0xE01EF, 0).	% Mn [240] VARIATION SELECTOR-17..VARIATION SELECTOR-256
unicode_combining_class(0xF0000, 0xFFFFD, 0).	% Co [65534] <private-use-F0000>..<private-use-FFFFD>
unicode_combining_class(0x100000, 0x10FFFD, 0).	% Co [65534] <private-use-100000>..<private-use-10FFFD>

% The above property value applies to 866463 code points not listed here.
% Total code points: 1113460

% ================================================

% Canonical_Combining_Class=Overlay

unicode_combining_class(0x0334, 0x0338, 1).	% Mn   [5] COMBINING TILDE OVERLAY..COMBINING LONG SOLIDUS OVERLAY
unicode_combining_class(0x1CD4, 0x1CD4, 1).	% Mn       VEDIC SIGN YAJURVEDIC MIDLINE SVARITA
unicode_combining_class(0x1CE2, 0x1CE8, 1).	% Mn   [7] VEDIC SIGN VISARGA SVARITA..VEDIC SIGN VISARGA ANUDATTA WITH TAIL
unicode_combining_class(0x20D2, 0x20D3, 1).	% Mn   [2] COMBINING LONG VERTICAL LINE OVERLAY..COMBINING SHORT VERTICAL LINE OVERLAY
unicode_combining_class(0x20D8, 0x20DA, 1).	% Mn   [3] COMBINING RING OVERLAY..COMBINING ANTICLOCKWISE RING OVERLAY
unicode_combining_class(0x20E5, 0x20E6, 1).	% Mn   [2] COMBINING REVERSE SOLIDUS OVERLAY..COMBINING DOUBLE VERTICAL STROKE OVERLAY
unicode_combining_class(0x20EA, 0x20EB, 1).	% Mn   [2] COMBINING LEFTWARDS ARROW OVERLAY..COMBINING LONG DOUBLE SOLIDUS OVERLAY
unicode_combining_class(0x10A39, 0x10A39, 1).	% Mn       KHAROSHTHI SIGN CAUDA
unicode_combining_class(0x1D167, 0x1D169, 1).	% Mn   [3] MUSICAL SYMBOL COMBINING TREMOLO-1..MUSICAL SYMBOL COMBINING TREMOLO-3

% Total code points: 26

% ================================================

% Canonical_Combining_Class=Nukta

unicode_combining_class(0x093C, 0x093C, 7).	% Mn       DEVANAGARI SIGN NUKTA
unicode_combining_class(0x09BC, 0x09BC, 7).	% Mn       BENGALI SIGN NUKTA
unicode_combining_class(0x0A3C, 0x0A3C, 7).	% Mn       GURMUKHI SIGN NUKTA
unicode_combining_class(0x0ABC, 0x0ABC, 7).	% Mn       GUJARATI SIGN NUKTA
unicode_combining_class(0x0B3C, 0x0B3C, 7).	% Mn       ORIYA SIGN NUKTA
unicode_combining_class(0x0CBC, 0x0CBC, 7).	% Mn       KANNADA SIGN NUKTA
unicode_combining_class(0x1037, 0x1037, 7).	% Mn       MYANMAR SIGN DOT BELOW
unicode_combining_class(0x1B34, 0x1B34, 7).	% Mn       BALINESE SIGN REREKAN
unicode_combining_class(0x1BE6, 0x1BE6, 7).	% Mn       BATAK SIGN TOMPI
unicode_combining_class(0x1C37, 0x1C37, 7).	% Mn       LEPCHA SIGN NUKTA
unicode_combining_class(0xA9B3, 0xA9B3, 7).	% Mn       JAVANESE SIGN CECAK TELU
unicode_combining_class(0x110BA, 0x110BA, 7).	% Mn       KAITHI SIGN NUKTA
unicode_combining_class(0x116B7, 0x116B7, 7).	% Mn       TAKRI SIGN NUKTA

% Total code points: 13

% ================================================

% Canonical_Combining_Class=Kana_Voicing

unicode_combining_class(0x3099, 0x309A, 8).	% Mn   [2] COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK..COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK

% Total code points: 2

% ================================================

% Canonical_Combining_Class=Virama

unicode_combining_class(0x094D, 0x094D, 9).	% Mn       DEVANAGARI SIGN VIRAMA
unicode_combining_class(0x09CD, 0x09CD, 9).	% Mn       BENGALI SIGN VIRAMA
unicode_combining_class(0x0A4D, 0x0A4D, 9).	% Mn       GURMUKHI SIGN VIRAMA
unicode_combining_class(0x0ACD, 0x0ACD, 9).	% Mn       GUJARATI SIGN VIRAMA
unicode_combining_class(0x0B4D, 0x0B4D, 9).	% Mn       ORIYA SIGN VIRAMA
unicode_combining_class(0x0BCD, 0x0BCD, 9).	% Mn       TAMIL SIGN VIRAMA
unicode_combining_class(0x0C4D, 0x0C4D, 9).	% Mn       TELUGU SIGN VIRAMA
unicode_combining_class(0x0CCD, 0x0CCD, 9).	% Mn       KANNADA SIGN VIRAMA
unicode_combining_class(0x0D4D, 0x0D4D, 9).	% Mn       MALAYALAM SIGN VIRAMA
unicode_combining_class(0x0DCA, 0x0DCA, 9).	% Mn       SINHALA SIGN AL-LAKUNA
unicode_combining_class(0x0E3A, 0x0E3A, 9).	% Mn       THAI CHARACTER PHINTHU
unicode_combining_class(0x0F84, 0x0F84, 9).	% Mn       TIBETAN MARK HALANTA
unicode_combining_class(0x1039, 0x103A, 9).	% Mn   [2] MYANMAR SIGN VIRAMA..MYANMAR SIGN ASAT
unicode_combining_class(0x1714, 0x1714, 9).	% Mn       TAGALOG SIGN VIRAMA
unicode_combining_class(0x1734, 0x1734, 9).	% Mn       HANUNOO SIGN PAMUDPOD
unicode_combining_class(0x17D2, 0x17D2, 9).	% Mn       KHMER SIGN COENG
unicode_combining_class(0x1A60, 0x1A60, 9).	% Mn       TAI THAM SIGN SAKOT
unicode_combining_class(0x1B44, 0x1B44, 9).	% Mc       BALINESE ADEG ADEG
unicode_combining_class(0x1BAA, 0x1BAA, 9).	% Mc       SUNDANESE SIGN PAMAAEH
unicode_combining_class(0x1BAB, 0x1BAB, 9).	% Mn       SUNDANESE SIGN VIRAMA
unicode_combining_class(0x1BF2, 0x1BF3, 9).	% Mc   [2] BATAK PANGOLAT..BATAK PANONGONAN
unicode_combining_class(0x2D7F, 0x2D7F, 9).	% Mn       TIFINAGH CONSONANT JOINER
unicode_combining_class(0xA806, 0xA806, 9).	% Mn       SYLOTI NAGRI SIGN HASANTA
unicode_combining_class(0xA8C4, 0xA8C4, 9).	% Mn       SAURASHTRA SIGN VIRAMA
unicode_combining_class(0xA953, 0xA953, 9).	% Mc       REJANG VIRAMA
unicode_combining_class(0xA9C0, 0xA9C0, 9).	% Mc       JAVANESE PANGKON
unicode_combining_class(0xAAF6, 0xAAF6, 9).	% Mn       MEETEI MAYEK VIRAMA
unicode_combining_class(0xABED, 0xABED, 9).	% Mn       MEETEI MAYEK APUN IYEK
unicode_combining_class(0x10A3F, 0x10A3F, 9).	% Mn       KHAROSHTHI VIRAMA
unicode_combining_class(0x11046, 0x11046, 9).	% Mn       BRAHMI VIRAMA
unicode_combining_class(0x110B9, 0x110B9, 9).	% Mn       KAITHI SIGN VIRAMA
unicode_combining_class(0x11133, 0x11134, 9).	% Mn   [2] CHAKMA VIRAMA..CHAKMA MAAYYAA
unicode_combining_class(0x111C0, 0x111C0, 9).	% Mc       SHARADA SIGN VIRAMA
unicode_combining_class(0x116B6, 0x116B6, 9).	% Mc       TAKRI SIGN VIRAMA

% Total code points: 37

% ================================================

% Canonical_Combining_Class=CCC10

unicode_combining_class(0x05B0, 0x05B0, 10).	% Mn       HEBREW POINT SHEVA

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC11

unicode_combining_class(0x05B1, 0x05B1, 11).	% Mn       HEBREW POINT HATAF SEGOL

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC12

unicode_combining_class(0x05B2, 0x05B2, 12).	% Mn       HEBREW POINT HATAF PATAH

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC13

unicode_combining_class(0x05B3, 0x05B3, 13).	% Mn       HEBREW POINT HATAF QAMATS

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC14

unicode_combining_class(0x05B4, 0x05B4, 14).	% Mn       HEBREW POINT HIRIQ

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC15

unicode_combining_class(0x05B5, 0x05B5, 15).	% Mn       HEBREW POINT TSERE

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC16

unicode_combining_class(0x05B6, 0x05B6, 16).	% Mn       HEBREW POINT SEGOL

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC17

unicode_combining_class(0x05B7, 0x05B7, 17).	% Mn       HEBREW POINT PATAH

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC18

unicode_combining_class(0x05B8, 0x05B8, 18).	% Mn       HEBREW POINT QAMATS
unicode_combining_class(0x05C7, 0x05C7, 18).	% Mn       HEBREW POINT QAMATS QATAN

% Total code points: 2

% ================================================

% Canonical_Combining_Class=CCC19

unicode_combining_class(0x05B9, 0x05BA, 19).	% Mn   [2] HEBREW POINT HOLAM..HEBREW POINT HOLAM HASER FOR VAV

% Total code points: 2

% ================================================

% Canonical_Combining_Class=CCC20

unicode_combining_class(0x05BB, 0x05BB, 20).	% Mn       HEBREW POINT QUBUTS

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC21

unicode_combining_class(0x05BC, 0x05BC, 21).	% Mn       HEBREW POINT DAGESH OR MAPIQ

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC22

unicode_combining_class(0x05BD, 0x05BD, 22).	% Mn       HEBREW POINT METEG

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC23

unicode_combining_class(0x05BF, 0x05BF, 23).	% Mn       HEBREW POINT RAFE

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC24

unicode_combining_class(0x05C1, 0x05C1, 24).	% Mn       HEBREW POINT SHIN DOT

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC25

unicode_combining_class(0x05C2, 0x05C2, 25).	% Mn       HEBREW POINT SIN DOT

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC26

unicode_combining_class(0xFB1E, 0xFB1E, 26).	% Mn       HEBREW POINT JUDEO-SPANISH VARIKA

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC27

unicode_combining_class(0x064B, 0x064B, 27).	% Mn       ARABIC FATHATAN
unicode_combining_class(0x08F0, 0x08F0, 27).	% Mn       ARABIC OPEN FATHATAN

% Total code points: 2

% ================================================

% Canonical_Combining_Class=CCC28

unicode_combining_class(0x064C, 0x064C, 28).	% Mn       ARABIC DAMMATAN
unicode_combining_class(0x08F1, 0x08F1, 28).	% Mn       ARABIC OPEN DAMMATAN

% Total code points: 2

% ================================================

% Canonical_Combining_Class=CCC29

unicode_combining_class(0x064D, 0x064D, 29).	% Mn       ARABIC KASRATAN
unicode_combining_class(0x08F2, 0x08F2, 29).	% Mn       ARABIC OPEN KASRATAN

% Total code points: 2

% ================================================

% Canonical_Combining_Class=CCC30

unicode_combining_class(0x0618, 0x0618, 30).	% Mn       ARABIC SMALL FATHA
unicode_combining_class(0x064E, 0x064E, 30).	% Mn       ARABIC FATHA

% Total code points: 2

% ================================================

% Canonical_Combining_Class=CCC31

unicode_combining_class(0x0619, 0x0619, 31).	% Mn       ARABIC SMALL DAMMA
unicode_combining_class(0x064F, 0x064F, 31).	% Mn       ARABIC DAMMA

% Total code points: 2

% ================================================

% Canonical_Combining_Class=CCC32

unicode_combining_class(0x061A, 0x061A, 32).	% Mn       ARABIC SMALL KASRA
unicode_combining_class(0x0650, 0x0650, 32).	% Mn       ARABIC KASRA

% Total code points: 2

% ================================================

% Canonical_Combining_Class=CCC33

unicode_combining_class(0x0651, 0x0651, 33).	% Mn       ARABIC SHADDA

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC34

unicode_combining_class(0x0652, 0x0652, 34).	% Mn       ARABIC SUKUN

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC35

unicode_combining_class(0x0670, 0x0670, 35).	% Mn       ARABIC LETTER SUPERSCRIPT ALEF

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC36

unicode_combining_class(0x0711, 0x0711, 36).	% Mn       SYRIAC LETTER SUPERSCRIPT ALAPH

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC84

unicode_combining_class(0x0C55, 0x0C55, 84).	% Mn       TELUGU LENGTH MARK

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC91

unicode_combining_class(0x0C56, 0x0C56, 91).	% Mn       TELUGU AI LENGTH MARK

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC103

unicode_combining_class(0x0E38, 0x0E39, 103).	% Mn   [2] THAI CHARACTER SARA U..THAI CHARACTER SARA UU

% Total code points: 2

% ================================================

% Canonical_Combining_Class=CCC107

unicode_combining_class(0x0E48, 0x0E4B, 107).	% Mn   [4] THAI CHARACTER MAI EK..THAI CHARACTER MAI CHATTAWA

% Total code points: 4

% ================================================

% Canonical_Combining_Class=CCC118

unicode_combining_class(0x0EB8, 0x0EB9, 118).	% Mn   [2] LAO VOWEL SIGN U..LAO VOWEL SIGN UU

% Total code points: 2

% ================================================

% Canonical_Combining_Class=CCC122

unicode_combining_class(0x0EC8, 0x0ECB, 122).	% Mn   [4] LAO TONE MAI EK..LAO TONE MAI CATAWA

% Total code points: 4

% ================================================

% Canonical_Combining_Class=CCC129

unicode_combining_class(0x0F71, 0x0F71, 129).	% Mn       TIBETAN VOWEL SIGN AA

% Total code points: 1

% ================================================

% Canonical_Combining_Class=CCC130

unicode_combining_class(0x0F72, 0x0F72, 130).	% Mn       TIBETAN VOWEL SIGN I
unicode_combining_class(0x0F7A, 0x0F7D, 130).	% Mn   [4] TIBETAN VOWEL SIGN E..TIBETAN VOWEL SIGN OO
unicode_combining_class(0x0F80, 0x0F80, 130).	% Mn       TIBETAN VOWEL SIGN REVERSED I

% Total code points: 6

% ================================================

% Canonical_Combining_Class=CCC133

unicode_combining_class(0x0F74, 0x0F74, 132).	% Mn       TIBETAN VOWEL SIGN U

% Total code points: 1

% ================================================

% Canonical_Combining_Class=Attached_Below

unicode_combining_class(0x0321, 0x0322, 202).	% Mn   [2] COMBINING PALATALIZED HOOK BELOW..COMBINING RETROFLEX HOOK BELOW
unicode_combining_class(0x0327, 0x0328, 202).	% Mn   [2] COMBINING CEDILLA..COMBINING OGONEK
unicode_combining_class(0x1DD0, 0x1DD0, 202).	% Mn       COMBINING IS BELOW

% Total code points: 5

% ================================================

% Canonical_Combining_Class=Attached_Above

unicode_combining_class(0x1DCE, 0x1DCE, 214).	% Mn       COMBINING OGONEK ABOVE

% Total code points: 1

% ================================================

% Canonical_Combining_Class=Attached_Above_Right

unicode_combining_class(0x031B, 0x031B, 216).	% Mn       COMBINING HORN
unicode_combining_class(0x0F39, 0x0F39, 216).	% Mn       TIBETAN MARK TSA -PHRU
unicode_combining_class(0x1D165, 0x1D166, 216).	% Mc   [2] MUSICAL SYMBOL COMBINING STEM..MUSICAL SYMBOL COMBINING SPRECHGESANG STEM
unicode_combining_class(0x1D16E, 0x1D172, 216).	% Mc   [5] MUSICAL SYMBOL COMBINING FLAG-1..MUSICAL SYMBOL COMBINING FLAG-5

% Total code points: 9

% ================================================

% Canonical_Combining_Class=Below_Left

unicode_combining_class(0x302A, 0x302A, 218).	% Mn       IDEOGRAPHIC LEVEL TONE MARK

% Total code points: 1

% ================================================

% Canonical_Combining_Class=Below

unicode_combining_class(0x0316, 0x0319, 220).	% Mn   [4] COMBINING GRAVE ACCENT BELOW..COMBINING RIGHT TACK BELOW
unicode_combining_class(0x031C, 0x0320, 220).	% Mn   [5] COMBINING LEFT HALF RING BELOW..COMBINING MINUS SIGN BELOW
unicode_combining_class(0x0323, 0x0326, 220).	% Mn   [4] COMBINING DOT BELOW..COMBINING COMMA BELOW
unicode_combining_class(0x0329, 0x0333, 220).	% Mn  [11] COMBINING VERTICAL LINE BELOW..COMBINING DOUBLE LOW LINE
unicode_combining_class(0x0339, 0x033C, 220).	% Mn   [4] COMBINING RIGHT HALF RING BELOW..COMBINING SEAGULL BELOW
unicode_combining_class(0x0347, 0x0349, 220).	% Mn   [3] COMBINING EQUALS SIGN BELOW..COMBINING LEFT ANGLE BELOW
unicode_combining_class(0x034D, 0x034E, 220).	% Mn   [2] COMBINING LEFT RIGHT ARROW BELOW..COMBINING UPWARDS ARROW BELOW
unicode_combining_class(0x0353, 0x0356, 220).	% Mn   [4] COMBINING X BELOW..COMBINING RIGHT ARROWHEAD AND UP ARROWHEAD BELOW
unicode_combining_class(0x0359, 0x035A, 220).	% Mn   [2] COMBINING ASTERISK BELOW..COMBINING DOUBLE RING BELOW
unicode_combining_class(0x0591, 0x0591, 220).	% Mn       HEBREW ACCENT ETNAHTA
unicode_combining_class(0x0596, 0x0596, 220).	% Mn       HEBREW ACCENT TIPEHA
unicode_combining_class(0x059B, 0x059B, 220).	% Mn       HEBREW ACCENT TEVIR
unicode_combining_class(0x05A2, 0x05A7, 220).	% Mn   [6] HEBREW ACCENT ATNAH HAFUKH..HEBREW ACCENT DARGA
unicode_combining_class(0x05AA, 0x05AA, 220).	% Mn       HEBREW ACCENT YERAH BEN YOMO
unicode_combining_class(0x05C5, 0x05C5, 220).	% Mn       HEBREW MARK LOWER DOT
unicode_combining_class(0x0655, 0x0656, 220).	% Mn   [2] ARABIC HAMZA BELOW..ARABIC SUBSCRIPT ALEF
unicode_combining_class(0x065C, 0x065C, 220).	% Mn       ARABIC VOWEL SIGN DOT BELOW
unicode_combining_class(0x065F, 0x065F, 220).	% Mn       ARABIC WAVY HAMZA BELOW
unicode_combining_class(0x06E3, 0x06E3, 220).	% Mn       ARABIC SMALL LOW SEEN
unicode_combining_class(0x06EA, 0x06EA, 220).	% Mn       ARABIC EMPTY CENTRE LOW STOP
unicode_combining_class(0x06ED, 0x06ED, 220).	% Mn       ARABIC SMALL LOW MEEM
unicode_combining_class(0x0731, 0x0731, 220).	% Mn       SYRIAC PTHAHA BELOW
unicode_combining_class(0x0734, 0x0734, 220).	% Mn       SYRIAC ZQAPHA BELOW
unicode_combining_class(0x0737, 0x0739, 220).	% Mn   [3] SYRIAC RBASA BELOW..SYRIAC DOTTED ZLAMA ANGULAR
unicode_combining_class(0x073B, 0x073C, 220).	% Mn   [2] SYRIAC HBASA BELOW..SYRIAC HBASA-ESASA DOTTED
unicode_combining_class(0x073E, 0x073E, 220).	% Mn       SYRIAC ESASA BELOW
unicode_combining_class(0x0742, 0x0742, 220).	% Mn       SYRIAC RUKKAKHA
unicode_combining_class(0x0744, 0x0744, 220).	% Mn       SYRIAC TWO VERTICAL DOTS BELOW
unicode_combining_class(0x0746, 0x0746, 220).	% Mn       SYRIAC THREE DOTS BELOW
unicode_combining_class(0x0748, 0x0748, 220).	% Mn       SYRIAC OBLIQUE LINE BELOW
unicode_combining_class(0x07F2, 0x07F2, 220).	% Mn       NKO COMBINING NASALIZATION MARK
unicode_combining_class(0x0859, 0x085B, 220).	% Mn   [3] MANDAIC AFFRICATION MARK..MANDAIC GEMINATION MARK
unicode_combining_class(0x08E6, 0x08E6, 220).	% Mn       ARABIC CURLY KASRA
unicode_combining_class(0x08E9, 0x08E9, 220).	% Mn       ARABIC CURLY KASRATAN
unicode_combining_class(0x08ED, 0x08EF, 220).	% Mn   [3] ARABIC TONE ONE DOT BELOW..ARABIC TONE LOOP BELOW
unicode_combining_class(0x08F6, 0x08F6, 220).	% Mn       ARABIC KASRA WITH DOT BELOW
unicode_combining_class(0x08F9, 0x08FA, 220).	% Mn   [2] ARABIC LEFT ARROWHEAD BELOW..ARABIC RIGHT ARROWHEAD BELOW
unicode_combining_class(0x0952, 0x0952, 220).	% Mn       DEVANAGARI STRESS SIGN ANUDATTA
unicode_combining_class(0x0F18, 0x0F19, 220).	% Mn   [2] TIBETAN ASTROLOGICAL SIGN -KHYUD PA..TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS
unicode_combining_class(0x0F35, 0x0F35, 220).	% Mn       TIBETAN MARK NGAS BZUNG NYI ZLA
unicode_combining_class(0x0F37, 0x0F37, 220).	% Mn       TIBETAN MARK NGAS BZUNG SGOR RTAGS
unicode_combining_class(0x0FC6, 0x0FC6, 220).	% Mn       TIBETAN SYMBOL PADMA GDAN
unicode_combining_class(0x108D, 0x108D, 220).	% Mn       MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE
unicode_combining_class(0x193B, 0x193B, 220).	% Mn       LIMBU SIGN SA-I
unicode_combining_class(0x1A18, 0x1A18, 220).	% Mn       BUGINESE VOWEL SIGN U
unicode_combining_class(0x1A7F, 0x1A7F, 220).	% Mn       TAI THAM COMBINING CRYPTOGRAMMIC DOT
unicode_combining_class(0x1B6C, 0x1B6C, 220).	% Mn       BALINESE MUSICAL SYMBOL COMBINING ENDEP
unicode_combining_class(0x1CD5, 0x1CD9, 220).	% Mn   [5] VEDIC TONE YAJURVEDIC AGGRAVATED INDEPENDENT SVARITA..VEDIC TONE YAJURVEDIC KATHAKA INDEPENDENT SVARITA SCHROEDER
unicode_combining_class(0x1CDC, 0x1CDF, 220).	% Mn   [4] VEDIC TONE KATHAKA ANUDATTA..VEDIC TONE THREE DOTS BELOW
unicode_combining_class(0x1CED, 0x1CED, 220).	% Mn       VEDIC SIGN TIRYAK
unicode_combining_class(0x1DC2, 0x1DC2, 220).	% Mn       COMBINING SNAKE BELOW
unicode_combining_class(0x1DCA, 0x1DCA, 220).	% Mn       COMBINING LATIN SMALL LETTER R BELOW
unicode_combining_class(0x1DCF, 0x1DCF, 220).	% Mn       COMBINING ZIGZAG BELOW
unicode_combining_class(0x1DFD, 0x1DFD, 220).	% Mn       COMBINING ALMOST EQUAL TO BELOW
unicode_combining_class(0x1DFF, 0x1DFF, 220).	% Mn       COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW
unicode_combining_class(0x20E8, 0x20E8, 220).	% Mn       COMBINING TRIPLE UNDERDOT
unicode_combining_class(0x20EC, 0x20EF, 220).	% Mn   [4] COMBINING RIGHTWARDS HARPOON WITH BARB DOWNWARDS..COMBINING RIGHT ARROW BELOW
unicode_combining_class(0xA92B, 0xA92D, 220).	% Mn   [3] KAYAH LI TONE PLOPHU..KAYAH LI TONE CALYA PLOPHU
unicode_combining_class(0xAAB4, 0xAAB4, 220).	% Mn       TAI VIET VOWEL U
unicode_combining_class(0x101FD, 0x101FD, 220).	% Mn       PHAISTOS DISC SIGN COMBINING OBLIQUE STROKE
unicode_combining_class(0x10A0D, 0x10A0D, 220).	% Mn       KHAROSHTHI SIGN DOUBLE RING BELOW
unicode_combining_class(0x10A3A, 0x10A3A, 220).	% Mn       KHAROSHTHI SIGN DOT BELOW
unicode_combining_class(0x1D17B, 0x1D182, 220).	% Mn   [8] MUSICAL SYMBOL COMBINING ACCENT..MUSICAL SYMBOL COMBINING LOURE
unicode_combining_class(0x1D18A, 0x1D18B, 220).	% Mn   [2] MUSICAL SYMBOL COMBINING DOUBLE TONGUE..MUSICAL SYMBOL COMBINING TRIPLE TONGUE

% Total code points: 129

% ================================================

% Canonical_Combining_Class=Below_Right

unicode_combining_class(0x059A, 0x059A, 222).	% Mn       HEBREW ACCENT YETIV
unicode_combining_class(0x05AD, 0x05AD, 222).	% Mn       HEBREW ACCENT DEHI
unicode_combining_class(0x1939, 0x1939, 222).	% Mn       LIMBU SIGN MUKPHRENG
unicode_combining_class(0x302D, 0x302D, 222).	% Mn       IDEOGRAPHIC ENTERING TONE MARK

% Total code points: 4

% ================================================

% Canonical_Combining_Class=Left

unicode_combining_class(0x302E, 0x302F, 224).	% Mc   [2] HANGUL SINGLE DOT TONE MARK..HANGUL DOUBLE DOT TONE MARK

% Total code points: 2

% ================================================

% Canonical_Combining_Class=Right

unicode_combining_class(0x1D16D, 0x1D16D, 226).	% Mc       MUSICAL SYMBOL COMBINING AUGMENTATION DOT

% Total code points: 1

% ================================================

% Canonical_Combining_Class=Above_Left

unicode_combining_class(0x05AE, 0x05AE, 228).	% Mn       HEBREW ACCENT ZINOR
unicode_combining_class(0x18A9, 0x18A9, 228).	% Mn       MONGOLIAN LETTER ALI GALI DAGALGA
unicode_combining_class(0x302B, 0x302B, 228).	% Mn       IDEOGRAPHIC RISING TONE MARK

% Total code points: 3

% ================================================

% Canonical_Combining_Class=Above

unicode_combining_class(0x0300, 0x0314, 230).	% Mn  [21] COMBINING GRAVE ACCENT..COMBINING REVERSED COMMA ABOVE
unicode_combining_class(0x033D, 0x0344, 230).	% Mn   [8] COMBINING X ABOVE..COMBINING GREEK DIALYTIKA TONOS
unicode_combining_class(0x0346, 0x0346, 230).	% Mn       COMBINING BRIDGE ABOVE
unicode_combining_class(0x034A, 0x034C, 230).	% Mn   [3] COMBINING NOT TILDE ABOVE..COMBINING ALMOST EQUAL TO ABOVE
unicode_combining_class(0x0350, 0x0352, 230).	% Mn   [3] COMBINING RIGHT ARROWHEAD ABOVE..COMBINING FERMATA
unicode_combining_class(0x0357, 0x0357, 230).	% Mn       COMBINING RIGHT HALF RING ABOVE
unicode_combining_class(0x035B, 0x035B, 230).	% Mn       COMBINING ZIGZAG ABOVE
unicode_combining_class(0x0363, 0x036F, 230).	% Mn  [13] COMBINING LATIN SMALL LETTER A..COMBINING LATIN SMALL LETTER X
unicode_combining_class(0x0483, 0x0487, 230).	% Mn   [5] COMBINING CYRILLIC TITLO..COMBINING CYRILLIC POKRYTIE
unicode_combining_class(0x0592, 0x0595, 230).	% Mn   [4] HEBREW ACCENT SEGOL..HEBREW ACCENT ZAQEF GADOL
unicode_combining_class(0x0597, 0x0599, 230).	% Mn   [3] HEBREW ACCENT REVIA..HEBREW ACCENT PASHTA
unicode_combining_class(0x059C, 0x05A1, 230).	% Mn   [6] HEBREW ACCENT GERESH..HEBREW ACCENT PAZER
unicode_combining_class(0x05A8, 0x05A9, 230).	% Mn   [2] HEBREW ACCENT QADMA..HEBREW ACCENT TELISHA QETANA
unicode_combining_class(0x05AB, 0x05AC, 230).	% Mn   [2] HEBREW ACCENT OLE..HEBREW ACCENT ILUY
unicode_combining_class(0x05AF, 0x05AF, 230).	% Mn       HEBREW MARK MASORA CIRCLE
unicode_combining_class(0x05C4, 0x05C4, 230).	% Mn       HEBREW MARK UPPER DOT
unicode_combining_class(0x0610, 0x0617, 230).	% Mn   [8] ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM..ARABIC SMALL HIGH ZAIN
unicode_combining_class(0x0653, 0x0654, 230).	% Mn   [2] ARABIC MADDAH ABOVE..ARABIC HAMZA ABOVE
unicode_combining_class(0x0657, 0x065B, 230).	% Mn   [5] ARABIC INVERTED DAMMA..ARABIC VOWEL SIGN INVERTED SMALL V ABOVE
unicode_combining_class(0x065D, 0x065E, 230).	% Mn   [2] ARABIC REVERSED DAMMA..ARABIC FATHA WITH TWO DOTS
unicode_combining_class(0x06D6, 0x06DC, 230).	% Mn   [7] ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN
unicode_combining_class(0x06DF, 0x06E2, 230).	% Mn   [4] ARABIC SMALL HIGH ROUNDED ZERO..ARABIC SMALL HIGH MEEM ISOLATED FORM
unicode_combining_class(0x06E4, 0x06E4, 230).	% Mn       ARABIC SMALL HIGH MADDA
unicode_combining_class(0x06E7, 0x06E8, 230).	% Mn   [2] ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON
unicode_combining_class(0x06EB, 0x06EC, 230).	% Mn   [2] ARABIC EMPTY CENTRE HIGH STOP..ARABIC ROUNDED HIGH STOP WITH FILLED CENTRE
unicode_combining_class(0x0730, 0x0730, 230).	% Mn       SYRIAC PTHAHA ABOVE
unicode_combining_class(0x0732, 0x0733, 230).	% Mn   [2] SYRIAC PTHAHA DOTTED..SYRIAC ZQAPHA ABOVE
unicode_combining_class(0x0735, 0x0736, 230).	% Mn   [2] SYRIAC ZQAPHA DOTTED..SYRIAC RBASA ABOVE
unicode_combining_class(0x073A, 0x073A, 230).	% Mn       SYRIAC HBASA ABOVE
unicode_combining_class(0x073D, 0x073D, 230).	% Mn       SYRIAC ESASA ABOVE
unicode_combining_class(0x073F, 0x0741, 230).	% Mn   [3] SYRIAC RWAHA..SYRIAC QUSHSHAYA
unicode_combining_class(0x0743, 0x0743, 230).	% Mn       SYRIAC TWO VERTICAL DOTS ABOVE
unicode_combining_class(0x0745, 0x0745, 230).	% Mn       SYRIAC THREE DOTS ABOVE
unicode_combining_class(0x0747, 0x0747, 230).	% Mn       SYRIAC OBLIQUE LINE ABOVE
unicode_combining_class(0x0749, 0x074A, 230).	% Mn   [2] SYRIAC MUSIC..SYRIAC BARREKH
unicode_combining_class(0x07EB, 0x07F1, 230).	% Mn   [7] NKO COMBINING SHORT HIGH TONE..NKO COMBINING LONG RISING TONE
unicode_combining_class(0x07F3, 0x07F3, 230).	% Mn       NKO COMBINING DOUBLE DOT ABOVE
unicode_combining_class(0x0816, 0x0819, 230).	% Mn   [4] SAMARITAN MARK IN..SAMARITAN MARK DAGESH
unicode_combining_class(0x081B, 0x0823, 230).	% Mn   [9] SAMARITAN MARK EPENTHETIC YUT..SAMARITAN VOWEL SIGN A
unicode_combining_class(0x0825, 0x0827, 230).	% Mn   [3] SAMARITAN VOWEL SIGN SHORT A..SAMARITAN VOWEL SIGN U
unicode_combining_class(0x0829, 0x082D, 230).	% Mn   [5] SAMARITAN VOWEL SIGN LONG I..SAMARITAN MARK NEQUDAA
unicode_combining_class(0x08E4, 0x08E5, 230).	% Mn   [2] ARABIC CURLY FATHA..ARABIC CURLY DAMMA
unicode_combining_class(0x08E7, 0x08E8, 230).	% Mn   [2] ARABIC CURLY FATHATAN..ARABIC CURLY DAMMATAN
unicode_combining_class(0x08EA, 0x08EC, 230).	% Mn   [3] ARABIC TONE ONE DOT ABOVE..ARABIC TONE LOOP ABOVE
unicode_combining_class(0x08F3, 0x08F5, 230).	% Mn   [3] ARABIC SMALL HIGH WAW..ARABIC FATHA WITH DOT ABOVE
unicode_combining_class(0x08F7, 0x08F8, 230).	% Mn   [2] ARABIC LEFT ARROWHEAD ABOVE..ARABIC RIGHT ARROWHEAD ABOVE
unicode_combining_class(0x08FB, 0x08FE, 230).	% Mn   [4] ARABIC DOUBLE RIGHT ARROWHEAD ABOVE..ARABIC DAMMA WITH DOT
unicode_combining_class(0x0951, 0x0951, 230).	% Mn       DEVANAGARI STRESS SIGN UDATTA
unicode_combining_class(0x0953, 0x0954, 230).	% Mn   [2] DEVANAGARI GRAVE ACCENT..DEVANAGARI ACUTE ACCENT
unicode_combining_class(0x0F82, 0x0F83, 230).	% Mn   [2] TIBETAN SIGN NYI ZLA NAA DA..TIBETAN SIGN SNA LDAN
unicode_combining_class(0x0F86, 0x0F87, 230).	% Mn   [2] TIBETAN SIGN LCI RTAGS..TIBETAN SIGN YANG RTAGS
unicode_combining_class(0x135D, 0x135F, 230).	% Mn   [3] ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK..ETHIOPIC COMBINING GEMINATION MARK
unicode_combining_class(0x17DD, 0x17DD, 230).	% Mn       KHMER SIGN ATTHACAN
unicode_combining_class(0x193A, 0x193A, 230).	% Mn       LIMBU SIGN KEMPHRENG
unicode_combining_class(0x1A17, 0x1A17, 230).	% Mn       BUGINESE VOWEL SIGN I
unicode_combining_class(0x1A75, 0x1A7C, 230).	% Mn   [8] TAI THAM SIGN TONE-1..TAI THAM SIGN KHUEN-LUE KARAN
unicode_combining_class(0x1B6B, 0x1B6B, 230).	% Mn       BALINESE MUSICAL SYMBOL COMBINING TEGEH
unicode_combining_class(0x1B6D, 0x1B73, 230).	% Mn   [7] BALINESE MUSICAL SYMBOL COMBINING KEMPUL..BALINESE MUSICAL SYMBOL COMBINING GONG
unicode_combining_class(0x1CD0, 0x1CD2, 230).	% Mn   [3] VEDIC TONE KARSHANA..VEDIC TONE PRENKHA
unicode_combining_class(0x1CDA, 0x1CDB, 230).	% Mn   [2] VEDIC TONE DOUBLE SVARITA..VEDIC TONE TRIPLE SVARITA
unicode_combining_class(0x1CE0, 0x1CE0, 230).	% Mn       VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA
unicode_combining_class(0x1CF4, 0x1CF4, 230).	% Mn       VEDIC TONE CANDRA ABOVE
unicode_combining_class(0x1DC0, 0x1DC1, 230).	% Mn   [2] COMBINING DOTTED GRAVE ACCENT..COMBINING DOTTED ACUTE ACCENT
unicode_combining_class(0x1DC3, 0x1DC9, 230).	% Mn   [7] COMBINING SUSPENSION MARK..COMBINING ACUTE-GRAVE-ACUTE
unicode_combining_class(0x1DCB, 0x1DCC, 230).	% Mn   [2] COMBINING BREVE-MACRON..COMBINING MACRON-BREVE
unicode_combining_class(0x1DD1, 0x1DE6, 230).	% Mn  [22] COMBINING UR ABOVE..COMBINING LATIN SMALL LETTER Z
unicode_combining_class(0x1DFE, 0x1DFE, 230).	% Mn       COMBINING LEFT ARROWHEAD ABOVE
unicode_combining_class(0x20D0, 0x20D1, 230).	% Mn   [2] COMBINING LEFT HARPOON ABOVE..COMBINING RIGHT HARPOON ABOVE
unicode_combining_class(0x20D4, 0x20D7, 230).	% Mn   [4] COMBINING ANTICLOCKWISE ARROW ABOVE..COMBINING RIGHT ARROW ABOVE
unicode_combining_class(0x20DB, 0x20DC, 230).	% Mn   [2] COMBINING THREE DOTS ABOVE..COMBINING FOUR DOTS ABOVE
unicode_combining_class(0x20E1, 0x20E1, 230).	% Mn       COMBINING LEFT RIGHT ARROW ABOVE
unicode_combining_class(0x20E7, 0x20E7, 230).	% Mn       COMBINING ANNUITY SYMBOL
unicode_combining_class(0x20E9, 0x20E9, 230).	% Mn       COMBINING WIDE BRIDGE ABOVE
unicode_combining_class(0x20F0, 0x20F0, 230).	% Mn       COMBINING ASTERISK ABOVE
unicode_combining_class(0x2CEF, 0x2CF1, 230).	% Mn   [3] COPTIC COMBINING NI ABOVE..COPTIC COMBINING SPIRITUS LENIS
unicode_combining_class(0x2DE0, 0x2DFF, 230).	% Mn  [32] COMBINING CYRILLIC LETTER BE..COMBINING CYRILLIC LETTER IOTIFIED BIG YUS
unicode_combining_class(0xA66F, 0xA66F, 230).	% Mn       COMBINING CYRILLIC VZMET
unicode_combining_class(0xA674, 0xA67D, 230).	% Mn  [10] COMBINING CYRILLIC LETTER UKRAINIAN IE..COMBINING CYRILLIC PAYEROK
unicode_combining_class(0xA69F, 0xA69F, 230).	% Mn       COMBINING CYRILLIC LETTER IOTIFIED E
unicode_combining_class(0xA6F0, 0xA6F1, 230).	% Mn   [2] BAMUM COMBINING MARK KOQNDON..BAMUM COMBINING MARK TUKWENTIS
unicode_combining_class(0xA8E0, 0xA8F1, 230).	% Mn  [18] COMBINING DEVANAGARI DIGIT ZERO..COMBINING DEVANAGARI SIGN AVAGRAHA
unicode_combining_class(0xAAB0, 0xAAB0, 230).	% Mn       TAI VIET MAI KANG
unicode_combining_class(0xAAB2, 0xAAB3, 230).	% Mn   [2] TAI VIET VOWEL I..TAI VIET VOWEL UE
unicode_combining_class(0xAAB7, 0xAAB8, 230).	% Mn   [2] TAI VIET MAI KHIT..TAI VIET VOWEL IA
unicode_combining_class(0xAABE, 0xAABF, 230).	% Mn   [2] TAI VIET VOWEL AM..TAI VIET TONE MAI EK
unicode_combining_class(0xAAC1, 0xAAC1, 230).	% Mn       TAI VIET TONE MAI THO
unicode_combining_class(0xFE20, 0xFE26, 230).	% Mn   [7] COMBINING LIGATURE LEFT HALF..COMBINING CONJOINING MACRON
unicode_combining_class(0x10A0F, 0x10A0F, 230).	% Mn       KHAROSHTHI SIGN VISARGA
unicode_combining_class(0x10A38, 0x10A38, 230).	% Mn       KHAROSHTHI SIGN BAR ABOVE
unicode_combining_class(0x11100, 0x11102, 230).	% Mn   [3] CHAKMA SIGN CANDRABINDU..CHAKMA SIGN VISARGA
unicode_combining_class(0x1D185, 0x1D189, 230).	% Mn   [5] MUSICAL SYMBOL COMBINING DOIT..MUSICAL SYMBOL COMBINING BEND
unicode_combining_class(0x1D1AA, 0x1D1AD, 230).	% Mn   [4] MUSICAL SYMBOL COMBINING DOWN BOW..MUSICAL SYMBOL COMBINING SNAP PIZZICATO
unicode_combining_class(0x1D242, 0x1D244, 230).	% Mn   [3] COMBINING GREEK MUSICAL TRISEME..COMBINING GREEK MUSICAL PENTASEME

% Total code points: 349

% ================================================

% Canonical_Combining_Class=Above_Right

unicode_combining_class(0x0315, 0x0315, 232).	% Mn       COMBINING COMMA ABOVE RIGHT
unicode_combining_class(0x031A, 0x031A, 232).	% Mn       COMBINING LEFT ANGLE ABOVE
unicode_combining_class(0x0358, 0x0358, 232).	% Mn       COMBINING DOT ABOVE RIGHT
unicode_combining_class(0x302C, 0x302C, 232).	% Mn       IDEOGRAPHIC DEPARTING TONE MARK

% Total code points: 4

% ================================================

% Canonical_Combining_Class=Double_Below

unicode_combining_class(0x035C, 0x035C, 233).	% Mn       COMBINING DOUBLE BREVE BELOW
unicode_combining_class(0x035F, 0x035F, 233).	% Mn       COMBINING DOUBLE MACRON BELOW
unicode_combining_class(0x0362, 0x0362, 233).	% Mn       COMBINING DOUBLE RIGHTWARDS ARROW BELOW
unicode_combining_class(0x1DFC, 0x1DFC, 233).	% Mn       COMBINING DOUBLE INVERTED BREVE BELOW

% Total code points: 4

% ================================================

% Canonical_Combining_Class=Double_Above

unicode_combining_class(0x035D, 0x035E, 234).	% Mn   [2] COMBINING DOUBLE BREVE..COMBINING DOUBLE MACRON
unicode_combining_class(0x0360, 0x0361, 234).	% Mn   [2] COMBINING DOUBLE TILDE..COMBINING DOUBLE INVERTED BREVE
unicode_combining_class(0x1DCD, 0x1DCD, 234).	% Mn       COMBINING DOUBLE CIRCUMFLEX ABOVE

% Total code points: 5

% ================================================

% Canonical_Combining_Class=Iota_Subscript

unicode_combining_class(0x0345, 0x0345, 240).	% Mn       COMBINING GREEK YPOGEGRAMMENI

% Total code points: 1

% EOF
