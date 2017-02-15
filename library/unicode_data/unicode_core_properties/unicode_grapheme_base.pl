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
# DerivedCoreProperties-6.1.0.txt
# Date: 2011-12-11, 18:26:55 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

% ================================================
*/

unicode_grapheme_base(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_grapheme_base(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_grapheme_base(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_grapheme_base(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property: Grapheme_Base
%  Generated from: [0..10FFFF] - Cc - Cf - Cs - Co - Cn - Zl - Zp - Grapheme_Extend
%  Note: depending on an application's interpretation of Co (private use),
%  they may be either in Grapheme_Base, or in Grapheme_Extend, or in neither.

unicode_grapheme_base(0x0020, 0x0020).	% Grapheme_Base Zs       SPACE
unicode_grapheme_base(0x0021, 0x0023).	% Grapheme_Base Po   [3] EXCLAMATION MARK..NUMBER SIGN
unicode_grapheme_base(0x0024, 0x0024).	% Grapheme_Base Sc       DOLLAR SIGN
unicode_grapheme_base(0x0025, 0x0027).	% Grapheme_Base Po   [3] PERCENT SIGN..APOSTROPHE
unicode_grapheme_base(0x0028, 0x0028).	% Grapheme_Base Ps       LEFT PARENTHESIS
unicode_grapheme_base(0x0029, 0x0029).	% Grapheme_Base Pe       RIGHT PARENTHESIS
unicode_grapheme_base(0x002A, 0x002A).	% Grapheme_Base Po       ASTERISK
unicode_grapheme_base(0x002B, 0x002B).	% Grapheme_Base Sm       PLUS SIGN
unicode_grapheme_base(0x002C, 0x002C).	% Grapheme_Base Po       COMMA
unicode_grapheme_base(0x002D, 0x002D).	% Grapheme_Base Pd       HYPHEN-MINUS
unicode_grapheme_base(0x002E, 0x002F).	% Grapheme_Base Po   [2] FULL STOP..SOLIDUS
unicode_grapheme_base(0x0030, 0x0039).	% Grapheme_Base Nd  [10] DIGIT ZERO..DIGIT NINE
unicode_grapheme_base(0x003A, 0x003B).	% Grapheme_Base Po   [2] COLON..SEMICOLON
unicode_grapheme_base(0x003C, 0x003E).	% Grapheme_Base Sm   [3] LESS-THAN SIGN..GREATER-THAN SIGN
unicode_grapheme_base(0x003F, 0x0040).	% Grapheme_Base Po   [2] QUESTION MARK..COMMERCIAL AT
unicode_grapheme_base(0x0041, 0x005A).	% Grapheme_Base L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
unicode_grapheme_base(0x005B, 0x005B).	% Grapheme_Base Ps       LEFT SQUARE BRACKET
unicode_grapheme_base(0x005C, 0x005C).	% Grapheme_Base Po       REVERSE SOLIDUS
unicode_grapheme_base(0x005D, 0x005D).	% Grapheme_Base Pe       RIGHT SQUARE BRACKET
unicode_grapheme_base(0x005E, 0x005E).	% Grapheme_Base Sk       CIRCUMFLEX ACCENT
unicode_grapheme_base(0x005F, 0x005F).	% Grapheme_Base Pc       LOW LINE
unicode_grapheme_base(0x0060, 0x0060).	% Grapheme_Base Sk       GRAVE ACCENT
unicode_grapheme_base(0x0061, 0x007A).	% Grapheme_Base L&  [26] LATIN SMALL LETTER A..LATIN SMALL LETTER Z
unicode_grapheme_base(0x007B, 0x007B).	% Grapheme_Base Ps       LEFT CURLY BRACKET
unicode_grapheme_base(0x007C, 0x007C).	% Grapheme_Base Sm       VERTICAL LINE
unicode_grapheme_base(0x007D, 0x007D).	% Grapheme_Base Pe       RIGHT CURLY BRACKET
unicode_grapheme_base(0x007E, 0x007E).	% Grapheme_Base Sm       TILDE
unicode_grapheme_base(0x00A0, 0x00A0).	% Grapheme_Base Zs       NO-BREAK SPACE
unicode_grapheme_base(0x00A1, 0x00A1).	% Grapheme_Base Po       INVERTED EXCLAMATION MARK
unicode_grapheme_base(0x00A2, 0x00A5).	% Grapheme_Base Sc   [4] CENT SIGN..YEN SIGN
unicode_grapheme_base(0x00A6, 0x00A6).	% Grapheme_Base So       BROKEN BAR
unicode_grapheme_base(0x00A7, 0x00A7).	% Grapheme_Base Po       SECTION SIGN
unicode_grapheme_base(0x00A8, 0x00A8).	% Grapheme_Base Sk       DIAERESIS
unicode_grapheme_base(0x00A9, 0x00A9).	% Grapheme_Base So       COPYRIGHT SIGN
unicode_grapheme_base(0x00AA, 0x00AA).	% Grapheme_Base Lo       FEMININE ORDINAL INDICATOR
unicode_grapheme_base(0x00AB, 0x00AB).	% Grapheme_Base Pi       LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_grapheme_base(0x00AC, 0x00AC).	% Grapheme_Base Sm       NOT SIGN
unicode_grapheme_base(0x00AE, 0x00AE).	% Grapheme_Base So       REGISTERED SIGN
unicode_grapheme_base(0x00AF, 0x00AF).	% Grapheme_Base Sk       MACRON
unicode_grapheme_base(0x00B0, 0x00B0).	% Grapheme_Base So       DEGREE SIGN
unicode_grapheme_base(0x00B1, 0x00B1).	% Grapheme_Base Sm       PLUS-MINUS SIGN
unicode_grapheme_base(0x00B2, 0x00B3).	% Grapheme_Base No   [2] SUPERSCRIPT TWO..SUPERSCRIPT THREE
unicode_grapheme_base(0x00B4, 0x00B4).	% Grapheme_Base Sk       ACUTE ACCENT
unicode_grapheme_base(0x00B5, 0x00B5).	% Grapheme_Base L&       MICRO SIGN
unicode_grapheme_base(0x00B6, 0x00B7).	% Grapheme_Base Po   [2] PILCROW SIGN..MIDDLE DOT
unicode_grapheme_base(0x00B8, 0x00B8).	% Grapheme_Base Sk       CEDILLA
unicode_grapheme_base(0x00B9, 0x00B9).	% Grapheme_Base No       SUPERSCRIPT ONE
unicode_grapheme_base(0x00BA, 0x00BA).	% Grapheme_Base Lo       MASCULINE ORDINAL INDICATOR
unicode_grapheme_base(0x00BB, 0x00BB).	% Grapheme_Base Pf       RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_grapheme_base(0x00BC, 0x00BE).	% Grapheme_Base No   [3] VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS
unicode_grapheme_base(0x00BF, 0x00BF).	% Grapheme_Base Po       INVERTED QUESTION MARK
unicode_grapheme_base(0x00C0, 0x00D6).	% Grapheme_Base L&  [23] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_grapheme_base(0x00D7, 0x00D7).	% Grapheme_Base Sm       MULTIPLICATION SIGN
unicode_grapheme_base(0x00D8, 0x00F6).	% Grapheme_Base L&  [31] LATIN CAPITAL LETTER O WITH STROKE..LATIN SMALL LETTER O WITH DIAERESIS
unicode_grapheme_base(0x00F7, 0x00F7).	% Grapheme_Base Sm       DIVISION SIGN
unicode_grapheme_base(0x00F8, 0x01BA).	% Grapheme_Base L& [195] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER EZH WITH TAIL
unicode_grapheme_base(0x01BB, 0x01BB).	% Grapheme_Base Lo       LATIN LETTER TWO WITH STROKE
unicode_grapheme_base(0x01BC, 0x01BF).	% Grapheme_Base L&   [4] LATIN CAPITAL LETTER TONE FIVE..LATIN LETTER WYNN
unicode_grapheme_base(0x01C0, 0x01C3).	% Grapheme_Base Lo   [4] LATIN LETTER DENTAL CLICK..LATIN LETTER RETROFLEX CLICK
unicode_grapheme_base(0x01C4, 0x0293).	% Grapheme_Base L& [208] LATIN CAPITAL LETTER DZ WITH CARON..LATIN SMALL LETTER EZH WITH CURL
unicode_grapheme_base(0x0294, 0x0294).	% Grapheme_Base Lo       LATIN LETTER GLOTTAL STOP
unicode_grapheme_base(0x0295, 0x02AF).	% Grapheme_Base L&  [27] LATIN LETTER PHARYNGEAL VOICED FRICATIVE..LATIN SMALL LETTER TURNED H WITH FISHHOOK AND TAIL
unicode_grapheme_base(0x02B0, 0x02C1).	% Grapheme_Base Lm  [18] MODIFIER LETTER SMALL H..MODIFIER LETTER REVERSED GLOTTAL STOP
unicode_grapheme_base(0x02C2, 0x02C5).	% Grapheme_Base Sk   [4] MODIFIER LETTER LEFT ARROWHEAD..MODIFIER LETTER DOWN ARROWHEAD
unicode_grapheme_base(0x02C6, 0x02D1).	% Grapheme_Base Lm  [12] MODIFIER LETTER CIRCUMFLEX ACCENT..MODIFIER LETTER HALF TRIANGULAR COLON
unicode_grapheme_base(0x02D2, 0x02DF).	% Grapheme_Base Sk  [14] MODIFIER LETTER CENTRED RIGHT HALF RING..MODIFIER LETTER CROSS ACCENT
unicode_grapheme_base(0x02E0, 0x02E4).	% Grapheme_Base Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_grapheme_base(0x02E5, 0x02EB).	% Grapheme_Base Sk   [7] MODIFIER LETTER EXTRA-HIGH TONE BAR..MODIFIER LETTER YANG DEPARTING TONE MARK
unicode_grapheme_base(0x02EC, 0x02EC).	% Grapheme_Base Lm       MODIFIER LETTER VOICING
unicode_grapheme_base(0x02ED, 0x02ED).	% Grapheme_Base Sk       MODIFIER LETTER UNASPIRATED
unicode_grapheme_base(0x02EE, 0x02EE).	% Grapheme_Base Lm       MODIFIER LETTER DOUBLE APOSTROPHE
unicode_grapheme_base(0x02EF, 0x02FF).	% Grapheme_Base Sk  [17] MODIFIER LETTER LOW DOWN ARROWHEAD..MODIFIER LETTER LOW LEFT ARROW
unicode_grapheme_base(0x0370, 0x0373).	% Grapheme_Base L&   [4] GREEK CAPITAL LETTER HETA..GREEK SMALL LETTER ARCHAIC SAMPI
unicode_grapheme_base(0x0374, 0x0374).	% Grapheme_Base Lm       GREEK NUMERAL SIGN
unicode_grapheme_base(0x0375, 0x0375).	% Grapheme_Base Sk       GREEK LOWER NUMERAL SIGN
unicode_grapheme_base(0x0376, 0x0377).	% Grapheme_Base L&   [2] GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA..GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
unicode_grapheme_base(0x037A, 0x037A).	% Grapheme_Base Lm       GREEK YPOGEGRAMMENI
unicode_grapheme_base(0x037B, 0x037D).	% Grapheme_Base L&   [3] GREEK SMALL REVERSED LUNATE SIGMA SYMBOL..GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
unicode_grapheme_base(0x037E, 0x037E).	% Grapheme_Base Po       GREEK QUESTION MARK
unicode_grapheme_base(0x0384, 0x0385).	% Grapheme_Base Sk   [2] GREEK TONOS..GREEK DIALYTIKA TONOS
unicode_grapheme_base(0x0386, 0x0386).	% Grapheme_Base L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_grapheme_base(0x0387, 0x0387).	% Grapheme_Base Po       GREEK ANO TELEIA
unicode_grapheme_base(0x0388, 0x038A).	% Grapheme_Base L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_grapheme_base(0x038C, 0x038C).	% Grapheme_Base L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_grapheme_base(0x038E, 0x03A1).	% Grapheme_Base L&  [20] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK CAPITAL LETTER RHO
unicode_grapheme_base(0x03A3, 0x03F5).	% Grapheme_Base L&  [83] GREEK CAPITAL LETTER SIGMA..GREEK LUNATE EPSILON SYMBOL
unicode_grapheme_base(0x03F6, 0x03F6).	% Grapheme_Base Sm       GREEK REVERSED LUNATE EPSILON SYMBOL
unicode_grapheme_base(0x03F7, 0x0481).	% Grapheme_Base L& [139] GREEK CAPITAL LETTER SHO..CYRILLIC SMALL LETTER KOPPA
unicode_grapheme_base(0x0482, 0x0482).	% Grapheme_Base So       CYRILLIC THOUSANDS SIGN
unicode_grapheme_base(0x048A, 0x0527).	% Grapheme_Base L& [158] CYRILLIC CAPITAL LETTER SHORT I WITH TAIL..CYRILLIC SMALL LETTER SHHA WITH DESCENDER
unicode_grapheme_base(0x0531, 0x0556).	% Grapheme_Base L&  [38] ARMENIAN CAPITAL LETTER AYB..ARMENIAN CAPITAL LETTER FEH
unicode_grapheme_base(0x0559, 0x0559).	% Grapheme_Base Lm       ARMENIAN MODIFIER LETTER LEFT HALF RING
unicode_grapheme_base(0x055A, 0x055F).	% Grapheme_Base Po   [6] ARMENIAN APOSTROPHE..ARMENIAN ABBREVIATION MARK
unicode_grapheme_base(0x0561, 0x0587).	% Grapheme_Base L&  [39] ARMENIAN SMALL LETTER AYB..ARMENIAN SMALL LIGATURE ECH YIWN
unicode_grapheme_base(0x0589, 0x0589).	% Grapheme_Base Po       ARMENIAN FULL STOP
unicode_grapheme_base(0x058A, 0x058A).	% Grapheme_Base Pd       ARMENIAN HYPHEN
unicode_grapheme_base(0x058F, 0x058F).	% Grapheme_Base Sc       ARMENIAN DRAM SIGN
unicode_grapheme_base(0x05BE, 0x05BE).	% Grapheme_Base Pd       HEBREW PUNCTUATION MAQAF
unicode_grapheme_base(0x05C0, 0x05C0).	% Grapheme_Base Po       HEBREW PUNCTUATION PASEQ
unicode_grapheme_base(0x05C3, 0x05C3).	% Grapheme_Base Po       HEBREW PUNCTUATION SOF PASUQ
unicode_grapheme_base(0x05C6, 0x05C6).	% Grapheme_Base Po       HEBREW PUNCTUATION NUN HAFUKHA
unicode_grapheme_base(0x05D0, 0x05EA).	% Grapheme_Base Lo  [27] HEBREW LETTER ALEF..HEBREW LETTER TAV
unicode_grapheme_base(0x05F0, 0x05F2).	% Grapheme_Base Lo   [3] HEBREW LIGATURE YIDDISH DOUBLE VAV..HEBREW LIGATURE YIDDISH DOUBLE YOD
unicode_grapheme_base(0x05F3, 0x05F4).	% Grapheme_Base Po   [2] HEBREW PUNCTUATION GERESH..HEBREW PUNCTUATION GERSHAYIM
unicode_grapheme_base(0x0606, 0x0608).	% Grapheme_Base Sm   [3] ARABIC-INDIC CUBE ROOT..ARABIC RAY
unicode_grapheme_base(0x0609, 0x060A).	% Grapheme_Base Po   [2] ARABIC-INDIC PER MILLE SIGN..ARABIC-INDIC PER TEN THOUSAND SIGN
unicode_grapheme_base(0x060B, 0x060B).	% Grapheme_Base Sc       AFGHANI SIGN
unicode_grapheme_base(0x060C, 0x060D).	% Grapheme_Base Po   [2] ARABIC COMMA..ARABIC DATE SEPARATOR
unicode_grapheme_base(0x060E, 0x060F).	% Grapheme_Base So   [2] ARABIC POETIC VERSE SIGN..ARABIC SIGN MISRA
unicode_grapheme_base(0x061B, 0x061B).	% Grapheme_Base Po       ARABIC SEMICOLON
unicode_grapheme_base(0x061E, 0x061F).	% Grapheme_Base Po   [2] ARABIC TRIPLE DOT PUNCTUATION MARK..ARABIC QUESTION MARK
unicode_grapheme_base(0x0620, 0x063F).	% Grapheme_Base Lo  [32] ARABIC LETTER KASHMIRI YEH..ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE
unicode_grapheme_base(0x0640, 0x0640).	% Grapheme_Base Lm       ARABIC TATWEEL
unicode_grapheme_base(0x0641, 0x064A).	% Grapheme_Base Lo  [10] ARABIC LETTER FEH..ARABIC LETTER YEH
unicode_grapheme_base(0x0660, 0x0669).	% Grapheme_Base Nd  [10] ARABIC-INDIC DIGIT ZERO..ARABIC-INDIC DIGIT NINE
unicode_grapheme_base(0x066A, 0x066D).	% Grapheme_Base Po   [4] ARABIC PERCENT SIGN..ARABIC FIVE POINTED STAR
unicode_grapheme_base(0x066E, 0x066F).	% Grapheme_Base Lo   [2] ARABIC LETTER DOTLESS BEH..ARABIC LETTER DOTLESS QAF
unicode_grapheme_base(0x0671, 0x06D3).	% Grapheme_Base Lo  [99] ARABIC LETTER ALEF WASLA..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
unicode_grapheme_base(0x06D4, 0x06D4).	% Grapheme_Base Po       ARABIC FULL STOP
unicode_grapheme_base(0x06D5, 0x06D5).	% Grapheme_Base Lo       ARABIC LETTER AE
unicode_grapheme_base(0x06DE, 0x06DE).	% Grapheme_Base So       ARABIC START OF RUB EL HIZB
unicode_grapheme_base(0x06E5, 0x06E6).	% Grapheme_Base Lm   [2] ARABIC SMALL WAW..ARABIC SMALL YEH
unicode_grapheme_base(0x06E9, 0x06E9).	% Grapheme_Base So       ARABIC PLACE OF SAJDAH
unicode_grapheme_base(0x06EE, 0x06EF).	% Grapheme_Base Lo   [2] ARABIC LETTER DAL WITH INVERTED V..ARABIC LETTER REH WITH INVERTED V
unicode_grapheme_base(0x06F0, 0x06F9).	% Grapheme_Base Nd  [10] EXTENDED ARABIC-INDIC DIGIT ZERO..EXTENDED ARABIC-INDIC DIGIT NINE
unicode_grapheme_base(0x06FA, 0x06FC).	% Grapheme_Base Lo   [3] ARABIC LETTER SHEEN WITH DOT BELOW..ARABIC LETTER GHAIN WITH DOT BELOW
unicode_grapheme_base(0x06FD, 0x06FE).	% Grapheme_Base So   [2] ARABIC SIGN SINDHI AMPERSAND..ARABIC SIGN SINDHI POSTPOSITION MEN
unicode_grapheme_base(0x06FF, 0x06FF).	% Grapheme_Base Lo       ARABIC LETTER HEH WITH INVERTED V
unicode_grapheme_base(0x0700, 0x070D).	% Grapheme_Base Po  [14] SYRIAC END OF PARAGRAPH..SYRIAC HARKLEAN ASTERISCUS
unicode_grapheme_base(0x0710, 0x0710).	% Grapheme_Base Lo       SYRIAC LETTER ALAPH
unicode_grapheme_base(0x0712, 0x072F).	% Grapheme_Base Lo  [30] SYRIAC LETTER BETH..SYRIAC LETTER PERSIAN DHALATH
unicode_grapheme_base(0x074D, 0x07A5).	% Grapheme_Base Lo  [89] SYRIAC LETTER SOGDIAN ZHAIN..THAANA LETTER WAAVU
unicode_grapheme_base(0x07B1, 0x07B1).	% Grapheme_Base Lo       THAANA LETTER NAA
unicode_grapheme_base(0x07C0, 0x07C9).	% Grapheme_Base Nd  [10] NKO DIGIT ZERO..NKO DIGIT NINE
unicode_grapheme_base(0x07CA, 0x07EA).	% Grapheme_Base Lo  [33] NKO LETTER A..NKO LETTER JONA RA
unicode_grapheme_base(0x07F4, 0x07F5).	% Grapheme_Base Lm   [2] NKO HIGH TONE APOSTROPHE..NKO LOW TONE APOSTROPHE
unicode_grapheme_base(0x07F6, 0x07F6).	% Grapheme_Base So       NKO SYMBOL OO DENNEN
unicode_grapheme_base(0x07F7, 0x07F9).	% Grapheme_Base Po   [3] NKO SYMBOL GBAKURUNEN..NKO EXCLAMATION MARK
unicode_grapheme_base(0x07FA, 0x07FA).	% Grapheme_Base Lm       NKO LAJANYALAN
unicode_grapheme_base(0x0800, 0x0815).	% Grapheme_Base Lo  [22] SAMARITAN LETTER ALAF..SAMARITAN LETTER TAAF
unicode_grapheme_base(0x081A, 0x081A).	% Grapheme_Base Lm       SAMARITAN MODIFIER LETTER EPENTHETIC YUT
unicode_grapheme_base(0x0824, 0x0824).	% Grapheme_Base Lm       SAMARITAN MODIFIER LETTER SHORT A
unicode_grapheme_base(0x0828, 0x0828).	% Grapheme_Base Lm       SAMARITAN MODIFIER LETTER I
unicode_grapheme_base(0x0830, 0x083E).	% Grapheme_Base Po  [15] SAMARITAN PUNCTUATION NEQUDAA..SAMARITAN PUNCTUATION ANNAAU
unicode_grapheme_base(0x0840, 0x0858).	% Grapheme_Base Lo  [25] MANDAIC LETTER HALQA..MANDAIC LETTER AIN
unicode_grapheme_base(0x085E, 0x085E).	% Grapheme_Base Po       MANDAIC PUNCTUATION
unicode_grapheme_base(0x08A0, 0x08A0).	% Grapheme_Base Lo       ARABIC LETTER BEH WITH SMALL V BELOW
unicode_grapheme_base(0x08A2, 0x08AC).	% Grapheme_Base Lo  [11] ARABIC LETTER JEEM WITH TWO DOTS ABOVE..ARABIC LETTER ROHINGYA YEH
unicode_grapheme_base(0x0903, 0x0903).	% Grapheme_Base Mc       DEVANAGARI SIGN VISARGA
unicode_grapheme_base(0x0904, 0x0939).	% Grapheme_Base Lo  [54] DEVANAGARI LETTER SHORT A..DEVANAGARI LETTER HA
unicode_grapheme_base(0x093B, 0x093B).	% Grapheme_Base Mc       DEVANAGARI VOWEL SIGN OOE
unicode_grapheme_base(0x093D, 0x093D).	% Grapheme_Base Lo       DEVANAGARI SIGN AVAGRAHA
unicode_grapheme_base(0x093E, 0x0940).	% Grapheme_Base Mc   [3] DEVANAGARI VOWEL SIGN AA..DEVANAGARI VOWEL SIGN II
unicode_grapheme_base(0x0949, 0x094C).	% Grapheme_Base Mc   [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU
unicode_grapheme_base(0x094E, 0x094F).	% Grapheme_Base Mc   [2] DEVANAGARI VOWEL SIGN PRISHTHAMATRA E..DEVANAGARI VOWEL SIGN AW
unicode_grapheme_base(0x0950, 0x0950).	% Grapheme_Base Lo       DEVANAGARI OM
unicode_grapheme_base(0x0958, 0x0961).	% Grapheme_Base Lo  [10] DEVANAGARI LETTER QA..DEVANAGARI LETTER VOCALIC LL
unicode_grapheme_base(0x0964, 0x0965).	% Grapheme_Base Po   [2] DEVANAGARI DANDA..DEVANAGARI DOUBLE DANDA
unicode_grapheme_base(0x0966, 0x096F).	% Grapheme_Base Nd  [10] DEVANAGARI DIGIT ZERO..DEVANAGARI DIGIT NINE
unicode_grapheme_base(0x0970, 0x0970).	% Grapheme_Base Po       DEVANAGARI ABBREVIATION SIGN
unicode_grapheme_base(0x0971, 0x0971).	% Grapheme_Base Lm       DEVANAGARI SIGN HIGH SPACING DOT
unicode_grapheme_base(0x0972, 0x0977).	% Grapheme_Base Lo   [6] DEVANAGARI LETTER CANDRA A..DEVANAGARI LETTER UUE
unicode_grapheme_base(0x0979, 0x097F).	% Grapheme_Base Lo   [7] DEVANAGARI LETTER ZHA..DEVANAGARI LETTER BBA
unicode_grapheme_base(0x0982, 0x0983).	% Grapheme_Base Mc   [2] BENGALI SIGN ANUSVARA..BENGALI SIGN VISARGA
unicode_grapheme_base(0x0985, 0x098C).	% Grapheme_Base Lo   [8] BENGALI LETTER A..BENGALI LETTER VOCALIC L
unicode_grapheme_base(0x098F, 0x0990).	% Grapheme_Base Lo   [2] BENGALI LETTER E..BENGALI LETTER AI
unicode_grapheme_base(0x0993, 0x09A8).	% Grapheme_Base Lo  [22] BENGALI LETTER O..BENGALI LETTER NA
unicode_grapheme_base(0x09AA, 0x09B0).	% Grapheme_Base Lo   [7] BENGALI LETTER PA..BENGALI LETTER RA
unicode_grapheme_base(0x09B2, 0x09B2).	% Grapheme_Base Lo       BENGALI LETTER LA
unicode_grapheme_base(0x09B6, 0x09B9).	% Grapheme_Base Lo   [4] BENGALI LETTER SHA..BENGALI LETTER HA
unicode_grapheme_base(0x09BD, 0x09BD).	% Grapheme_Base Lo       BENGALI SIGN AVAGRAHA
unicode_grapheme_base(0x09BF, 0x09C0).	% Grapheme_Base Mc   [2] BENGALI VOWEL SIGN I..BENGALI VOWEL SIGN II
unicode_grapheme_base(0x09C7, 0x09C8).	% Grapheme_Base Mc   [2] BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI
unicode_grapheme_base(0x09CB, 0x09CC).	% Grapheme_Base Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_grapheme_base(0x09CE, 0x09CE).	% Grapheme_Base Lo       BENGALI LETTER KHANDA TA
unicode_grapheme_base(0x09DC, 0x09DD).	% Grapheme_Base Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_grapheme_base(0x09DF, 0x09E1).	% Grapheme_Base Lo   [3] BENGALI LETTER YYA..BENGALI LETTER VOCALIC LL
unicode_grapheme_base(0x09E6, 0x09EF).	% Grapheme_Base Nd  [10] BENGALI DIGIT ZERO..BENGALI DIGIT NINE
unicode_grapheme_base(0x09F0, 0x09F1).	% Grapheme_Base Lo   [2] BENGALI LETTER RA WITH MIDDLE DIAGONAL..BENGALI LETTER RA WITH LOWER DIAGONAL
unicode_grapheme_base(0x09F2, 0x09F3).	% Grapheme_Base Sc   [2] BENGALI RUPEE MARK..BENGALI RUPEE SIGN
unicode_grapheme_base(0x09F4, 0x09F9).	% Grapheme_Base No   [6] BENGALI CURRENCY NUMERATOR ONE..BENGALI CURRENCY DENOMINATOR SIXTEEN
unicode_grapheme_base(0x09FA, 0x09FA).	% Grapheme_Base So       BENGALI ISSHAR
unicode_grapheme_base(0x09FB, 0x09FB).	% Grapheme_Base Sc       BENGALI GANDA MARK
unicode_grapheme_base(0x0A03, 0x0A03).	% Grapheme_Base Mc       GURMUKHI SIGN VISARGA
unicode_grapheme_base(0x0A05, 0x0A0A).	% Grapheme_Base Lo   [6] GURMUKHI LETTER A..GURMUKHI LETTER UU
unicode_grapheme_base(0x0A0F, 0x0A10).	% Grapheme_Base Lo   [2] GURMUKHI LETTER EE..GURMUKHI LETTER AI
unicode_grapheme_base(0x0A13, 0x0A28).	% Grapheme_Base Lo  [22] GURMUKHI LETTER OO..GURMUKHI LETTER NA
unicode_grapheme_base(0x0A2A, 0x0A30).	% Grapheme_Base Lo   [7] GURMUKHI LETTER PA..GURMUKHI LETTER RA
unicode_grapheme_base(0x0A32, 0x0A33).	% Grapheme_Base Lo   [2] GURMUKHI LETTER LA..GURMUKHI LETTER LLA
unicode_grapheme_base(0x0A35, 0x0A36).	% Grapheme_Base Lo   [2] GURMUKHI LETTER VA..GURMUKHI LETTER SHA
unicode_grapheme_base(0x0A38, 0x0A39).	% Grapheme_Base Lo   [2] GURMUKHI LETTER SA..GURMUKHI LETTER HA
unicode_grapheme_base(0x0A3E, 0x0A40).	% Grapheme_Base Mc   [3] GURMUKHI VOWEL SIGN AA..GURMUKHI VOWEL SIGN II
unicode_grapheme_base(0x0A59, 0x0A5C).	% Grapheme_Base Lo   [4] GURMUKHI LETTER KHHA..GURMUKHI LETTER RRA
unicode_grapheme_base(0x0A5E, 0x0A5E).	% Grapheme_Base Lo       GURMUKHI LETTER FA
unicode_grapheme_base(0x0A66, 0x0A6F).	% Grapheme_Base Nd  [10] GURMUKHI DIGIT ZERO..GURMUKHI DIGIT NINE
unicode_grapheme_base(0x0A72, 0x0A74).	% Grapheme_Base Lo   [3] GURMUKHI IRI..GURMUKHI EK ONKAR
unicode_grapheme_base(0x0A83, 0x0A83).	% Grapheme_Base Mc       GUJARATI SIGN VISARGA
unicode_grapheme_base(0x0A85, 0x0A8D).	% Grapheme_Base Lo   [9] GUJARATI LETTER A..GUJARATI VOWEL CANDRA E
unicode_grapheme_base(0x0A8F, 0x0A91).	% Grapheme_Base Lo   [3] GUJARATI LETTER E..GUJARATI VOWEL CANDRA O
unicode_grapheme_base(0x0A93, 0x0AA8).	% Grapheme_Base Lo  [22] GUJARATI LETTER O..GUJARATI LETTER NA
unicode_grapheme_base(0x0AAA, 0x0AB0).	% Grapheme_Base Lo   [7] GUJARATI LETTER PA..GUJARATI LETTER RA
unicode_grapheme_base(0x0AB2, 0x0AB3).	% Grapheme_Base Lo   [2] GUJARATI LETTER LA..GUJARATI LETTER LLA
unicode_grapheme_base(0x0AB5, 0x0AB9).	% Grapheme_Base Lo   [5] GUJARATI LETTER VA..GUJARATI LETTER HA
unicode_grapheme_base(0x0ABD, 0x0ABD).	% Grapheme_Base Lo       GUJARATI SIGN AVAGRAHA
unicode_grapheme_base(0x0ABE, 0x0AC0).	% Grapheme_Base Mc   [3] GUJARATI VOWEL SIGN AA..GUJARATI VOWEL SIGN II
unicode_grapheme_base(0x0AC9, 0x0AC9).	% Grapheme_Base Mc       GUJARATI VOWEL SIGN CANDRA O
unicode_grapheme_base(0x0ACB, 0x0ACC).	% Grapheme_Base Mc   [2] GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU
unicode_grapheme_base(0x0AD0, 0x0AD0).	% Grapheme_Base Lo       GUJARATI OM
unicode_grapheme_base(0x0AE0, 0x0AE1).	% Grapheme_Base Lo   [2] GUJARATI LETTER VOCALIC RR..GUJARATI LETTER VOCALIC LL
unicode_grapheme_base(0x0AE6, 0x0AEF).	% Grapheme_Base Nd  [10] GUJARATI DIGIT ZERO..GUJARATI DIGIT NINE
unicode_grapheme_base(0x0AF0, 0x0AF0).	% Grapheme_Base Po       GUJARATI ABBREVIATION SIGN
unicode_grapheme_base(0x0AF1, 0x0AF1).	% Grapheme_Base Sc       GUJARATI RUPEE SIGN
unicode_grapheme_base(0x0B02, 0x0B03).	% Grapheme_Base Mc   [2] ORIYA SIGN ANUSVARA..ORIYA SIGN VISARGA
unicode_grapheme_base(0x0B05, 0x0B0C).	% Grapheme_Base Lo   [8] ORIYA LETTER A..ORIYA LETTER VOCALIC L
unicode_grapheme_base(0x0B0F, 0x0B10).	% Grapheme_Base Lo   [2] ORIYA LETTER E..ORIYA LETTER AI
unicode_grapheme_base(0x0B13, 0x0B28).	% Grapheme_Base Lo  [22] ORIYA LETTER O..ORIYA LETTER NA
unicode_grapheme_base(0x0B2A, 0x0B30).	% Grapheme_Base Lo   [7] ORIYA LETTER PA..ORIYA LETTER RA
unicode_grapheme_base(0x0B32, 0x0B33).	% Grapheme_Base Lo   [2] ORIYA LETTER LA..ORIYA LETTER LLA
unicode_grapheme_base(0x0B35, 0x0B39).	% Grapheme_Base Lo   [5] ORIYA LETTER VA..ORIYA LETTER HA
unicode_grapheme_base(0x0B3D, 0x0B3D).	% Grapheme_Base Lo       ORIYA SIGN AVAGRAHA
unicode_grapheme_base(0x0B40, 0x0B40).	% Grapheme_Base Mc       ORIYA VOWEL SIGN II
unicode_grapheme_base(0x0B47, 0x0B48).	% Grapheme_Base Mc   [2] ORIYA VOWEL SIGN E..ORIYA VOWEL SIGN AI
unicode_grapheme_base(0x0B4B, 0x0B4C).	% Grapheme_Base Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
unicode_grapheme_base(0x0B5C, 0x0B5D).	% Grapheme_Base Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_grapheme_base(0x0B5F, 0x0B61).	% Grapheme_Base Lo   [3] ORIYA LETTER YYA..ORIYA LETTER VOCALIC LL
unicode_grapheme_base(0x0B66, 0x0B6F).	% Grapheme_Base Nd  [10] ORIYA DIGIT ZERO..ORIYA DIGIT NINE
unicode_grapheme_base(0x0B70, 0x0B70).	% Grapheme_Base So       ORIYA ISSHAR
unicode_grapheme_base(0x0B71, 0x0B71).	% Grapheme_Base Lo       ORIYA LETTER WA
unicode_grapheme_base(0x0B72, 0x0B77).	% Grapheme_Base No   [6] ORIYA FRACTION ONE QUARTER..ORIYA FRACTION THREE SIXTEENTHS
unicode_grapheme_base(0x0B83, 0x0B83).	% Grapheme_Base Lo       TAMIL SIGN VISARGA
unicode_grapheme_base(0x0B85, 0x0B8A).	% Grapheme_Base Lo   [6] TAMIL LETTER A..TAMIL LETTER UU
unicode_grapheme_base(0x0B8E, 0x0B90).	% Grapheme_Base Lo   [3] TAMIL LETTER E..TAMIL LETTER AI
unicode_grapheme_base(0x0B92, 0x0B95).	% Grapheme_Base Lo   [4] TAMIL LETTER O..TAMIL LETTER KA
unicode_grapheme_base(0x0B99, 0x0B9A).	% Grapheme_Base Lo   [2] TAMIL LETTER NGA..TAMIL LETTER CA
unicode_grapheme_base(0x0B9C, 0x0B9C).	% Grapheme_Base Lo       TAMIL LETTER JA
unicode_grapheme_base(0x0B9E, 0x0B9F).	% Grapheme_Base Lo   [2] TAMIL LETTER NYA..TAMIL LETTER TTA
unicode_grapheme_base(0x0BA3, 0x0BA4).	% Grapheme_Base Lo   [2] TAMIL LETTER NNA..TAMIL LETTER TA
unicode_grapheme_base(0x0BA8, 0x0BAA).	% Grapheme_Base Lo   [3] TAMIL LETTER NA..TAMIL LETTER PA
unicode_grapheme_base(0x0BAE, 0x0BB9).	% Grapheme_Base Lo  [12] TAMIL LETTER MA..TAMIL LETTER HA
unicode_grapheme_base(0x0BBF, 0x0BBF).	% Grapheme_Base Mc       TAMIL VOWEL SIGN I
unicode_grapheme_base(0x0BC1, 0x0BC2).	% Grapheme_Base Mc   [2] TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU
unicode_grapheme_base(0x0BC6, 0x0BC8).	% Grapheme_Base Mc   [3] TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI
unicode_grapheme_base(0x0BCA, 0x0BCC).	% Grapheme_Base Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_grapheme_base(0x0BD0, 0x0BD0).	% Grapheme_Base Lo       TAMIL OM
unicode_grapheme_base(0x0BE6, 0x0BEF).	% Grapheme_Base Nd  [10] TAMIL DIGIT ZERO..TAMIL DIGIT NINE
unicode_grapheme_base(0x0BF0, 0x0BF2).	% Grapheme_Base No   [3] TAMIL NUMBER TEN..TAMIL NUMBER ONE THOUSAND
unicode_grapheme_base(0x0BF3, 0x0BF8).	% Grapheme_Base So   [6] TAMIL DAY SIGN..TAMIL AS ABOVE SIGN
unicode_grapheme_base(0x0BF9, 0x0BF9).	% Grapheme_Base Sc       TAMIL RUPEE SIGN
unicode_grapheme_base(0x0BFA, 0x0BFA).	% Grapheme_Base So       TAMIL NUMBER SIGN
unicode_grapheme_base(0x0C01, 0x0C03).	% Grapheme_Base Mc   [3] TELUGU SIGN CANDRABINDU..TELUGU SIGN VISARGA
unicode_grapheme_base(0x0C05, 0x0C0C).	% Grapheme_Base Lo   [8] TELUGU LETTER A..TELUGU LETTER VOCALIC L
unicode_grapheme_base(0x0C0E, 0x0C10).	% Grapheme_Base Lo   [3] TELUGU LETTER E..TELUGU LETTER AI
unicode_grapheme_base(0x0C12, 0x0C28).	% Grapheme_Base Lo  [23] TELUGU LETTER O..TELUGU LETTER NA
unicode_grapheme_base(0x0C2A, 0x0C33).	% Grapheme_Base Lo  [10] TELUGU LETTER PA..TELUGU LETTER LLA
unicode_grapheme_base(0x0C35, 0x0C39).	% Grapheme_Base Lo   [5] TELUGU LETTER VA..TELUGU LETTER HA
unicode_grapheme_base(0x0C3D, 0x0C3D).	% Grapheme_Base Lo       TELUGU SIGN AVAGRAHA
unicode_grapheme_base(0x0C41, 0x0C44).	% Grapheme_Base Mc   [4] TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR
unicode_grapheme_base(0x0C58, 0x0C59).	% Grapheme_Base Lo   [2] TELUGU LETTER TSA..TELUGU LETTER DZA
unicode_grapheme_base(0x0C60, 0x0C61).	% Grapheme_Base Lo   [2] TELUGU LETTER VOCALIC RR..TELUGU LETTER VOCALIC LL
unicode_grapheme_base(0x0C66, 0x0C6F).	% Grapheme_Base Nd  [10] TELUGU DIGIT ZERO..TELUGU DIGIT NINE
unicode_grapheme_base(0x0C78, 0x0C7E).	% Grapheme_Base No   [7] TELUGU FRACTION DIGIT ZERO FOR ODD POWERS OF FOUR..TELUGU FRACTION DIGIT THREE FOR EVEN POWERS OF FOUR
unicode_grapheme_base(0x0C7F, 0x0C7F).	% Grapheme_Base So       TELUGU SIGN TUUMU
unicode_grapheme_base(0x0C82, 0x0C83).	% Grapheme_Base Mc   [2] KANNADA SIGN ANUSVARA..KANNADA SIGN VISARGA
unicode_grapheme_base(0x0C85, 0x0C8C).	% Grapheme_Base Lo   [8] KANNADA LETTER A..KANNADA LETTER VOCALIC L
unicode_grapheme_base(0x0C8E, 0x0C90).	% Grapheme_Base Lo   [3] KANNADA LETTER E..KANNADA LETTER AI
unicode_grapheme_base(0x0C92, 0x0CA8).	% Grapheme_Base Lo  [23] KANNADA LETTER O..KANNADA LETTER NA
unicode_grapheme_base(0x0CAA, 0x0CB3).	% Grapheme_Base Lo  [10] KANNADA LETTER PA..KANNADA LETTER LLA
unicode_grapheme_base(0x0CB5, 0x0CB9).	% Grapheme_Base Lo   [5] KANNADA LETTER VA..KANNADA LETTER HA
unicode_grapheme_base(0x0CBD, 0x0CBD).	% Grapheme_Base Lo       KANNADA SIGN AVAGRAHA
unicode_grapheme_base(0x0CBE, 0x0CBE).	% Grapheme_Base Mc       KANNADA VOWEL SIGN AA
unicode_grapheme_base(0x0CC0, 0x0CC1).	% Grapheme_Base Mc   [2] KANNADA VOWEL SIGN II..KANNADA VOWEL SIGN U
unicode_grapheme_base(0x0CC3, 0x0CC4).	% Grapheme_Base Mc   [2] KANNADA VOWEL SIGN VOCALIC R..KANNADA VOWEL SIGN VOCALIC RR
unicode_grapheme_base(0x0CC7, 0x0CC8).	% Grapheme_Base Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_grapheme_base(0x0CCA, 0x0CCB).	% Grapheme_Base Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_grapheme_base(0x0CDE, 0x0CDE).	% Grapheme_Base Lo       KANNADA LETTER FA
unicode_grapheme_base(0x0CE0, 0x0CE1).	% Grapheme_Base Lo   [2] KANNADA LETTER VOCALIC RR..KANNADA LETTER VOCALIC LL
unicode_grapheme_base(0x0CE6, 0x0CEF).	% Grapheme_Base Nd  [10] KANNADA DIGIT ZERO..KANNADA DIGIT NINE
unicode_grapheme_base(0x0CF1, 0x0CF2).	% Grapheme_Base Lo   [2] KANNADA SIGN JIHVAMULIYA..KANNADA SIGN UPADHMANIYA
unicode_grapheme_base(0x0D02, 0x0D03).	% Grapheme_Base Mc   [2] MALAYALAM SIGN ANUSVARA..MALAYALAM SIGN VISARGA
unicode_grapheme_base(0x0D05, 0x0D0C).	% Grapheme_Base Lo   [8] MALAYALAM LETTER A..MALAYALAM LETTER VOCALIC L
unicode_grapheme_base(0x0D0E, 0x0D10).	% Grapheme_Base Lo   [3] MALAYALAM LETTER E..MALAYALAM LETTER AI
unicode_grapheme_base(0x0D12, 0x0D3A).	% Grapheme_Base Lo  [41] MALAYALAM LETTER O..MALAYALAM LETTER TTTA
unicode_grapheme_base(0x0D3D, 0x0D3D).	% Grapheme_Base Lo       MALAYALAM SIGN AVAGRAHA
unicode_grapheme_base(0x0D3F, 0x0D40).	% Grapheme_Base Mc   [2] MALAYALAM VOWEL SIGN I..MALAYALAM VOWEL SIGN II
unicode_grapheme_base(0x0D46, 0x0D48).	% Grapheme_Base Mc   [3] MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI
unicode_grapheme_base(0x0D4A, 0x0D4C).	% Grapheme_Base Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_grapheme_base(0x0D4E, 0x0D4E).	% Grapheme_Base Lo       MALAYALAM LETTER DOT REPH
unicode_grapheme_base(0x0D60, 0x0D61).	% Grapheme_Base Lo   [2] MALAYALAM LETTER VOCALIC RR..MALAYALAM LETTER VOCALIC LL
unicode_grapheme_base(0x0D66, 0x0D6F).	% Grapheme_Base Nd  [10] MALAYALAM DIGIT ZERO..MALAYALAM DIGIT NINE
unicode_grapheme_base(0x0D70, 0x0D75).	% Grapheme_Base No   [6] MALAYALAM NUMBER TEN..MALAYALAM FRACTION THREE QUARTERS
unicode_grapheme_base(0x0D79, 0x0D79).	% Grapheme_Base So       MALAYALAM DATE MARK
unicode_grapheme_base(0x0D7A, 0x0D7F).	% Grapheme_Base Lo   [6] MALAYALAM LETTER CHILLU NN..MALAYALAM LETTER CHILLU K
unicode_grapheme_base(0x0D82, 0x0D83).	% Grapheme_Base Mc   [2] SINHALA SIGN ANUSVARAYA..SINHALA SIGN VISARGAYA
unicode_grapheme_base(0x0D85, 0x0D96).	% Grapheme_Base Lo  [18] SINHALA LETTER AYANNA..SINHALA LETTER AUYANNA
unicode_grapheme_base(0x0D9A, 0x0DB1).	% Grapheme_Base Lo  [24] SINHALA LETTER ALPAPRAANA KAYANNA..SINHALA LETTER DANTAJA NAYANNA
unicode_grapheme_base(0x0DB3, 0x0DBB).	% Grapheme_Base Lo   [9] SINHALA LETTER SANYAKA DAYANNA..SINHALA LETTER RAYANNA
unicode_grapheme_base(0x0DBD, 0x0DBD).	% Grapheme_Base Lo       SINHALA LETTER DANTAJA LAYANNA
unicode_grapheme_base(0x0DC0, 0x0DC6).	% Grapheme_Base Lo   [7] SINHALA LETTER VAYANNA..SINHALA LETTER FAYANNA
unicode_grapheme_base(0x0DD0, 0x0DD1).	% Grapheme_Base Mc   [2] SINHALA VOWEL SIGN KETTI AEDA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA
unicode_grapheme_base(0x0DD8, 0x0DDE).	% Grapheme_Base Mc   [7] SINHALA VOWEL SIGN GAETTA-PILLA..SINHALA VOWEL SIGN KOMBUVA HAA GAYANUKITTA
unicode_grapheme_base(0x0DF2, 0x0DF3).	% Grapheme_Base Mc   [2] SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA
unicode_grapheme_base(0x0DF4, 0x0DF4).	% Grapheme_Base Po       SINHALA PUNCTUATION KUNDDALIYA
unicode_grapheme_base(0x0E01, 0x0E30).	% Grapheme_Base Lo  [48] THAI CHARACTER KO KAI..THAI CHARACTER SARA A
unicode_grapheme_base(0x0E32, 0x0E33).	% Grapheme_Base Lo   [2] THAI CHARACTER SARA AA..THAI CHARACTER SARA AM
unicode_grapheme_base(0x0E3F, 0x0E3F).	% Grapheme_Base Sc       THAI CURRENCY SYMBOL BAHT
unicode_grapheme_base(0x0E40, 0x0E45).	% Grapheme_Base Lo   [6] THAI CHARACTER SARA E..THAI CHARACTER LAKKHANGYAO
unicode_grapheme_base(0x0E46, 0x0E46).	% Grapheme_Base Lm       THAI CHARACTER MAIYAMOK
unicode_grapheme_base(0x0E4F, 0x0E4F).	% Grapheme_Base Po       THAI CHARACTER FONGMAN
unicode_grapheme_base(0x0E50, 0x0E59).	% Grapheme_Base Nd  [10] THAI DIGIT ZERO..THAI DIGIT NINE
unicode_grapheme_base(0x0E5A, 0x0E5B).	% Grapheme_Base Po   [2] THAI CHARACTER ANGKHANKHU..THAI CHARACTER KHOMUT
unicode_grapheme_base(0x0E81, 0x0E82).	% Grapheme_Base Lo   [2] LAO LETTER KO..LAO LETTER KHO SUNG
unicode_grapheme_base(0x0E84, 0x0E84).	% Grapheme_Base Lo       LAO LETTER KHO TAM
unicode_grapheme_base(0x0E87, 0x0E88).	% Grapheme_Base Lo   [2] LAO LETTER NGO..LAO LETTER CO
unicode_grapheme_base(0x0E8A, 0x0E8A).	% Grapheme_Base Lo       LAO LETTER SO TAM
unicode_grapheme_base(0x0E8D, 0x0E8D).	% Grapheme_Base Lo       LAO LETTER NYO
unicode_grapheme_base(0x0E94, 0x0E97).	% Grapheme_Base Lo   [4] LAO LETTER DO..LAO LETTER THO TAM
unicode_grapheme_base(0x0E99, 0x0E9F).	% Grapheme_Base Lo   [7] LAO LETTER NO..LAO LETTER FO SUNG
unicode_grapheme_base(0x0EA1, 0x0EA3).	% Grapheme_Base Lo   [3] LAO LETTER MO..LAO LETTER LO LING
unicode_grapheme_base(0x0EA5, 0x0EA5).	% Grapheme_Base Lo       LAO LETTER LO LOOT
unicode_grapheme_base(0x0EA7, 0x0EA7).	% Grapheme_Base Lo       LAO LETTER WO
unicode_grapheme_base(0x0EAA, 0x0EAB).	% Grapheme_Base Lo   [2] LAO LETTER SO SUNG..LAO LETTER HO SUNG
unicode_grapheme_base(0x0EAD, 0x0EB0).	% Grapheme_Base Lo   [4] LAO LETTER O..LAO VOWEL SIGN A
unicode_grapheme_base(0x0EB2, 0x0EB3).	% Grapheme_Base Lo   [2] LAO VOWEL SIGN AA..LAO VOWEL SIGN AM
unicode_grapheme_base(0x0EBD, 0x0EBD).	% Grapheme_Base Lo       LAO SEMIVOWEL SIGN NYO
unicode_grapheme_base(0x0EC0, 0x0EC4).	% Grapheme_Base Lo   [5] LAO VOWEL SIGN E..LAO VOWEL SIGN AI
unicode_grapheme_base(0x0EC6, 0x0EC6).	% Grapheme_Base Lm       LAO KO LA
unicode_grapheme_base(0x0ED0, 0x0ED9).	% Grapheme_Base Nd  [10] LAO DIGIT ZERO..LAO DIGIT NINE
unicode_grapheme_base(0x0EDC, 0x0EDF).	% Grapheme_Base Lo   [4] LAO HO NO..LAO LETTER KHMU NYO
unicode_grapheme_base(0x0F00, 0x0F00).	% Grapheme_Base Lo       TIBETAN SYLLABLE OM
unicode_grapheme_base(0x0F01, 0x0F03).	% Grapheme_Base So   [3] TIBETAN MARK GTER YIG MGO TRUNCATED A..TIBETAN MARK GTER YIG MGO -UM GTER TSHEG MA
unicode_grapheme_base(0x0F04, 0x0F12).	% Grapheme_Base Po  [15] TIBETAN MARK INITIAL YIG MGO MDUN MA..TIBETAN MARK RGYA GRAM SHAD
unicode_grapheme_base(0x0F13, 0x0F13).	% Grapheme_Base So       TIBETAN MARK CARET -DZUD RTAGS ME LONG CAN
unicode_grapheme_base(0x0F14, 0x0F14).	% Grapheme_Base Po       TIBETAN MARK GTER TSHEG
unicode_grapheme_base(0x0F15, 0x0F17).	% Grapheme_Base So   [3] TIBETAN LOGOTYPE SIGN CHAD RTAGS..TIBETAN ASTROLOGICAL SIGN SGRA GCAN -CHAR RTAGS
unicode_grapheme_base(0x0F1A, 0x0F1F).	% Grapheme_Base So   [6] TIBETAN SIGN RDEL DKAR GCIG..TIBETAN SIGN RDEL DKAR RDEL NAG
unicode_grapheme_base(0x0F20, 0x0F29).	% Grapheme_Base Nd  [10] TIBETAN DIGIT ZERO..TIBETAN DIGIT NINE
unicode_grapheme_base(0x0F2A, 0x0F33).	% Grapheme_Base No  [10] TIBETAN DIGIT HALF ONE..TIBETAN DIGIT HALF ZERO
unicode_grapheme_base(0x0F34, 0x0F34).	% Grapheme_Base So       TIBETAN MARK BSDUS RTAGS
unicode_grapheme_base(0x0F36, 0x0F36).	% Grapheme_Base So       TIBETAN MARK CARET -DZUD RTAGS BZHI MIG CAN
unicode_grapheme_base(0x0F38, 0x0F38).	% Grapheme_Base So       TIBETAN MARK CHE MGO
unicode_grapheme_base(0x0F3A, 0x0F3A).	% Grapheme_Base Ps       TIBETAN MARK GUG RTAGS GYON
unicode_grapheme_base(0x0F3B, 0x0F3B).	% Grapheme_Base Pe       TIBETAN MARK GUG RTAGS GYAS
unicode_grapheme_base(0x0F3C, 0x0F3C).	% Grapheme_Base Ps       TIBETAN MARK ANG KHANG GYON
unicode_grapheme_base(0x0F3D, 0x0F3D).	% Grapheme_Base Pe       TIBETAN MARK ANG KHANG GYAS
unicode_grapheme_base(0x0F3E, 0x0F3F).	% Grapheme_Base Mc   [2] TIBETAN SIGN YAR TSHES..TIBETAN SIGN MAR TSHES
unicode_grapheme_base(0x0F40, 0x0F47).	% Grapheme_Base Lo   [8] TIBETAN LETTER KA..TIBETAN LETTER JA
unicode_grapheme_base(0x0F49, 0x0F6C).	% Grapheme_Base Lo  [36] TIBETAN LETTER NYA..TIBETAN LETTER RRA
unicode_grapheme_base(0x0F7F, 0x0F7F).	% Grapheme_Base Mc       TIBETAN SIGN RNAM BCAD
unicode_grapheme_base(0x0F85, 0x0F85).	% Grapheme_Base Po       TIBETAN MARK PALUTA
unicode_grapheme_base(0x0F88, 0x0F8C).	% Grapheme_Base Lo   [5] TIBETAN SIGN LCE TSA CAN..TIBETAN SIGN INVERTED MCHU CAN
unicode_grapheme_base(0x0FBE, 0x0FC5).	% Grapheme_Base So   [8] TIBETAN KU RU KHA..TIBETAN SYMBOL RDO RJE
unicode_grapheme_base(0x0FC7, 0x0FCC).	% Grapheme_Base So   [6] TIBETAN SYMBOL RDO RJE RGYA GRAM..TIBETAN SYMBOL NOR BU BZHI -KHYIL
unicode_grapheme_base(0x0FCE, 0x0FCF).	% Grapheme_Base So   [2] TIBETAN SIGN RDEL NAG RDEL DKAR..TIBETAN SIGN RDEL NAG GSUM
unicode_grapheme_base(0x0FD0, 0x0FD4).	% Grapheme_Base Po   [5] TIBETAN MARK BSKA- SHOG GI MGO RGYAN..TIBETAN MARK CLOSING BRDA RNYING YIG MGO SGAB MA
unicode_grapheme_base(0x0FD5, 0x0FD8).	% Grapheme_Base So   [4] RIGHT-FACING SVASTI SIGN..LEFT-FACING SVASTI SIGN WITH DOTS
unicode_grapheme_base(0x0FD9, 0x0FDA).	% Grapheme_Base Po   [2] TIBETAN MARK LEADING MCHAN RTAGS..TIBETAN MARK TRAILING MCHAN RTAGS
unicode_grapheme_base(0x1000, 0x102A).	% Grapheme_Base Lo  [43] MYANMAR LETTER KA..MYANMAR LETTER AU
unicode_grapheme_base(0x102B, 0x102C).	% Grapheme_Base Mc   [2] MYANMAR VOWEL SIGN TALL AA..MYANMAR VOWEL SIGN AA
unicode_grapheme_base(0x1031, 0x1031).	% Grapheme_Base Mc       MYANMAR VOWEL SIGN E
unicode_grapheme_base(0x1038, 0x1038).	% Grapheme_Base Mc       MYANMAR SIGN VISARGA
unicode_grapheme_base(0x103B, 0x103C).	% Grapheme_Base Mc   [2] MYANMAR CONSONANT SIGN MEDIAL YA..MYANMAR CONSONANT SIGN MEDIAL RA
unicode_grapheme_base(0x103F, 0x103F).	% Grapheme_Base Lo       MYANMAR LETTER GREAT SA
unicode_grapheme_base(0x1040, 0x1049).	% Grapheme_Base Nd  [10] MYANMAR DIGIT ZERO..MYANMAR DIGIT NINE
unicode_grapheme_base(0x104A, 0x104F).	% Grapheme_Base Po   [6] MYANMAR SIGN LITTLE SECTION..MYANMAR SYMBOL GENITIVE
unicode_grapheme_base(0x1050, 0x1055).	% Grapheme_Base Lo   [6] MYANMAR LETTER SHA..MYANMAR LETTER VOCALIC LL
unicode_grapheme_base(0x1056, 0x1057).	% Grapheme_Base Mc   [2] MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR
unicode_grapheme_base(0x105A, 0x105D).	% Grapheme_Base Lo   [4] MYANMAR LETTER MON NGA..MYANMAR LETTER MON BBE
unicode_grapheme_base(0x1061, 0x1061).	% Grapheme_Base Lo       MYANMAR LETTER SGAW KAREN SHA
unicode_grapheme_base(0x1062, 0x1064).	% Grapheme_Base Mc   [3] MYANMAR VOWEL SIGN SGAW KAREN EU..MYANMAR TONE MARK SGAW KAREN KE PHO
unicode_grapheme_base(0x1065, 0x1066).	% Grapheme_Base Lo   [2] MYANMAR LETTER WESTERN PWO KAREN THA..MYANMAR LETTER WESTERN PWO KAREN PWA
unicode_grapheme_base(0x1067, 0x106D).	% Grapheme_Base Mc   [7] MYANMAR VOWEL SIGN WESTERN PWO KAREN EU..MYANMAR SIGN WESTERN PWO KAREN TONE-5
unicode_grapheme_base(0x106E, 0x1070).	% Grapheme_Base Lo   [3] MYANMAR LETTER EASTERN PWO KAREN NNA..MYANMAR LETTER EASTERN PWO KAREN GHWA
unicode_grapheme_base(0x1075, 0x1081).	% Grapheme_Base Lo  [13] MYANMAR LETTER SHAN KA..MYANMAR LETTER SHAN HA
unicode_grapheme_base(0x1083, 0x1084).	% Grapheme_Base Mc   [2] MYANMAR VOWEL SIGN SHAN AA..MYANMAR VOWEL SIGN SHAN E
unicode_grapheme_base(0x1087, 0x108C).	% Grapheme_Base Mc   [6] MYANMAR SIGN SHAN TONE-2..MYANMAR SIGN SHAN COUNCIL TONE-3
unicode_grapheme_base(0x108E, 0x108E).	% Grapheme_Base Lo       MYANMAR LETTER RUMAI PALAUNG FA
unicode_grapheme_base(0x108F, 0x108F).	% Grapheme_Base Mc       MYANMAR SIGN RUMAI PALAUNG TONE-5
unicode_grapheme_base(0x1090, 0x1099).	% Grapheme_Base Nd  [10] MYANMAR SHAN DIGIT ZERO..MYANMAR SHAN DIGIT NINE
unicode_grapheme_base(0x109A, 0x109C).	% Grapheme_Base Mc   [3] MYANMAR SIGN KHAMTI TONE-1..MYANMAR VOWEL SIGN AITON A
unicode_grapheme_base(0x109E, 0x109F).	% Grapheme_Base So   [2] MYANMAR SYMBOL SHAN ONE..MYANMAR SYMBOL SHAN EXCLAMATION
unicode_grapheme_base(0x10A0, 0x10C5).	% Grapheme_Base L&  [38] GEORGIAN CAPITAL LETTER AN..GEORGIAN CAPITAL LETTER HOE
unicode_grapheme_base(0x10C7, 0x10C7).	% Grapheme_Base L&       GEORGIAN CAPITAL LETTER YN
unicode_grapheme_base(0x10CD, 0x10CD).	% Grapheme_Base L&       GEORGIAN CAPITAL LETTER AEN
unicode_grapheme_base(0x10D0, 0x10FA).	% Grapheme_Base Lo  [43] GEORGIAN LETTER AN..GEORGIAN LETTER AIN
unicode_grapheme_base(0x10FB, 0x10FB).	% Grapheme_Base Po       GEORGIAN PARAGRAPH SEPARATOR
unicode_grapheme_base(0x10FC, 0x10FC).	% Grapheme_Base Lm       MODIFIER LETTER GEORGIAN NAR
unicode_grapheme_base(0x10FD, 0x1248).	% Grapheme_Base Lo [332] GEORGIAN LETTER AEN..ETHIOPIC SYLLABLE QWA
unicode_grapheme_base(0x124A, 0x124D).	% Grapheme_Base Lo   [4] ETHIOPIC SYLLABLE QWI..ETHIOPIC SYLLABLE QWE
unicode_grapheme_base(0x1250, 0x1256).	% Grapheme_Base Lo   [7] ETHIOPIC SYLLABLE QHA..ETHIOPIC SYLLABLE QHO
unicode_grapheme_base(0x1258, 0x1258).	% Grapheme_Base Lo       ETHIOPIC SYLLABLE QHWA
unicode_grapheme_base(0x125A, 0x125D).	% Grapheme_Base Lo   [4] ETHIOPIC SYLLABLE QHWI..ETHIOPIC SYLLABLE QHWE
unicode_grapheme_base(0x1260, 0x1288).	% Grapheme_Base Lo  [41] ETHIOPIC SYLLABLE BA..ETHIOPIC SYLLABLE XWA
unicode_grapheme_base(0x128A, 0x128D).	% Grapheme_Base Lo   [4] ETHIOPIC SYLLABLE XWI..ETHIOPIC SYLLABLE XWE
unicode_grapheme_base(0x1290, 0x12B0).	% Grapheme_Base Lo  [33] ETHIOPIC SYLLABLE NA..ETHIOPIC SYLLABLE KWA
unicode_grapheme_base(0x12B2, 0x12B5).	% Grapheme_Base Lo   [4] ETHIOPIC SYLLABLE KWI..ETHIOPIC SYLLABLE KWE
unicode_grapheme_base(0x12B8, 0x12BE).	% Grapheme_Base Lo   [7] ETHIOPIC SYLLABLE KXA..ETHIOPIC SYLLABLE KXO
unicode_grapheme_base(0x12C0, 0x12C0).	% Grapheme_Base Lo       ETHIOPIC SYLLABLE KXWA
unicode_grapheme_base(0x12C2, 0x12C5).	% Grapheme_Base Lo   [4] ETHIOPIC SYLLABLE KXWI..ETHIOPIC SYLLABLE KXWE
unicode_grapheme_base(0x12C8, 0x12D6).	% Grapheme_Base Lo  [15] ETHIOPIC SYLLABLE WA..ETHIOPIC SYLLABLE PHARYNGEAL O
unicode_grapheme_base(0x12D8, 0x1310).	% Grapheme_Base Lo  [57] ETHIOPIC SYLLABLE ZA..ETHIOPIC SYLLABLE GWA
unicode_grapheme_base(0x1312, 0x1315).	% Grapheme_Base Lo   [4] ETHIOPIC SYLLABLE GWI..ETHIOPIC SYLLABLE GWE
unicode_grapheme_base(0x1318, 0x135A).	% Grapheme_Base Lo  [67] ETHIOPIC SYLLABLE GGA..ETHIOPIC SYLLABLE FYA
unicode_grapheme_base(0x1360, 0x1368).	% Grapheme_Base Po   [9] ETHIOPIC SECTION MARK..ETHIOPIC PARAGRAPH SEPARATOR
unicode_grapheme_base(0x1369, 0x137C).	% Grapheme_Base No  [20] ETHIOPIC DIGIT ONE..ETHIOPIC NUMBER TEN THOUSAND
unicode_grapheme_base(0x1380, 0x138F).	% Grapheme_Base Lo  [16] ETHIOPIC SYLLABLE SEBATBEIT MWA..ETHIOPIC SYLLABLE PWE
unicode_grapheme_base(0x1390, 0x1399).	% Grapheme_Base So  [10] ETHIOPIC TONAL MARK YIZET..ETHIOPIC TONAL MARK KURT
unicode_grapheme_base(0x13A0, 0x13F4).	% Grapheme_Base Lo  [85] CHEROKEE LETTER A..CHEROKEE LETTER YV
unicode_grapheme_base(0x1400, 0x1400).	% Grapheme_Base Pd       CANADIAN SYLLABICS HYPHEN
unicode_grapheme_base(0x1401, 0x166C).	% Grapheme_Base Lo [620] CANADIAN SYLLABICS E..CANADIAN SYLLABICS CARRIER TTSA
unicode_grapheme_base(0x166D, 0x166E).	% Grapheme_Base Po   [2] CANADIAN SYLLABICS CHI SIGN..CANADIAN SYLLABICS FULL STOP
unicode_grapheme_base(0x166F, 0x167F).	% Grapheme_Base Lo  [17] CANADIAN SYLLABICS QAI..CANADIAN SYLLABICS BLACKFOOT W
unicode_grapheme_base(0x1680, 0x1680).	% Grapheme_Base Zs       OGHAM SPACE MARK
unicode_grapheme_base(0x1681, 0x169A).	% Grapheme_Base Lo  [26] OGHAM LETTER BEITH..OGHAM LETTER PEITH
unicode_grapheme_base(0x169B, 0x169B).	% Grapheme_Base Ps       OGHAM FEATHER MARK
unicode_grapheme_base(0x169C, 0x169C).	% Grapheme_Base Pe       OGHAM REVERSED FEATHER MARK
unicode_grapheme_base(0x16A0, 0x16EA).	% Grapheme_Base Lo  [75] RUNIC LETTER FEHU FEOH FE F..RUNIC LETTER X
unicode_grapheme_base(0x16EB, 0x16ED).	% Grapheme_Base Po   [3] RUNIC SINGLE PUNCTUATION..RUNIC CROSS PUNCTUATION
unicode_grapheme_base(0x16EE, 0x16F0).	% Grapheme_Base Nl   [3] RUNIC ARLAUG SYMBOL..RUNIC BELGTHOR SYMBOL
unicode_grapheme_base(0x1700, 0x170C).	% Grapheme_Base Lo  [13] TAGALOG LETTER A..TAGALOG LETTER YA
unicode_grapheme_base(0x170E, 0x1711).	% Grapheme_Base Lo   [4] TAGALOG LETTER LA..TAGALOG LETTER HA
unicode_grapheme_base(0x1720, 0x1731).	% Grapheme_Base Lo  [18] HANUNOO LETTER A..HANUNOO LETTER HA
unicode_grapheme_base(0x1735, 0x1736).	% Grapheme_Base Po   [2] PHILIPPINE SINGLE PUNCTUATION..PHILIPPINE DOUBLE PUNCTUATION
unicode_grapheme_base(0x1740, 0x1751).	% Grapheme_Base Lo  [18] BUHID LETTER A..BUHID LETTER HA
unicode_grapheme_base(0x1760, 0x176C).	% Grapheme_Base Lo  [13] TAGBANWA LETTER A..TAGBANWA LETTER YA
unicode_grapheme_base(0x176E, 0x1770).	% Grapheme_Base Lo   [3] TAGBANWA LETTER LA..TAGBANWA LETTER SA
unicode_grapheme_base(0x1780, 0x17B3).	% Grapheme_Base Lo  [52] KHMER LETTER KA..KHMER INDEPENDENT VOWEL QAU
unicode_grapheme_base(0x17B6, 0x17B6).	% Grapheme_Base Mc       KHMER VOWEL SIGN AA
unicode_grapheme_base(0x17BE, 0x17C5).	% Grapheme_Base Mc   [8] KHMER VOWEL SIGN OE..KHMER VOWEL SIGN AU
unicode_grapheme_base(0x17C7, 0x17C8).	% Grapheme_Base Mc   [2] KHMER SIGN REAHMUK..KHMER SIGN YUUKALEAPINTU
unicode_grapheme_base(0x17D4, 0x17D6).	% Grapheme_Base Po   [3] KHMER SIGN KHAN..KHMER SIGN CAMNUC PII KUUH
unicode_grapheme_base(0x17D7, 0x17D7).	% Grapheme_Base Lm       KHMER SIGN LEK TOO
unicode_grapheme_base(0x17D8, 0x17DA).	% Grapheme_Base Po   [3] KHMER SIGN BEYYAL..KHMER SIGN KOOMUUT
unicode_grapheme_base(0x17DB, 0x17DB).	% Grapheme_Base Sc       KHMER CURRENCY SYMBOL RIEL
unicode_grapheme_base(0x17DC, 0x17DC).	% Grapheme_Base Lo       KHMER SIGN AVAKRAHASANYA
unicode_grapheme_base(0x17E0, 0x17E9).	% Grapheme_Base Nd  [10] KHMER DIGIT ZERO..KHMER DIGIT NINE
unicode_grapheme_base(0x17F0, 0x17F9).	% Grapheme_Base No  [10] KHMER SYMBOL LEK ATTAK SON..KHMER SYMBOL LEK ATTAK PRAM-BUON
unicode_grapheme_base(0x1800, 0x1805).	% Grapheme_Base Po   [6] MONGOLIAN BIRGA..MONGOLIAN FOUR DOTS
unicode_grapheme_base(0x1806, 0x1806).	% Grapheme_Base Pd       MONGOLIAN TODO SOFT HYPHEN
unicode_grapheme_base(0x1807, 0x180A).	% Grapheme_Base Po   [4] MONGOLIAN SIBE SYLLABLE BOUNDARY MARKER..MONGOLIAN NIRUGU
unicode_grapheme_base(0x180E, 0x180E).	% Grapheme_Base Zs       MONGOLIAN VOWEL SEPARATOR
unicode_grapheme_base(0x1810, 0x1819).	% Grapheme_Base Nd  [10] MONGOLIAN DIGIT ZERO..MONGOLIAN DIGIT NINE
unicode_grapheme_base(0x1820, 0x1842).	% Grapheme_Base Lo  [35] MONGOLIAN LETTER A..MONGOLIAN LETTER CHI
unicode_grapheme_base(0x1843, 0x1843).	% Grapheme_Base Lm       MONGOLIAN LETTER TODO LONG VOWEL SIGN
unicode_grapheme_base(0x1844, 0x1877).	% Grapheme_Base Lo  [52] MONGOLIAN LETTER TODO E..MONGOLIAN LETTER MANCHU ZHA
unicode_grapheme_base(0x1880, 0x18A8).	% Grapheme_Base Lo  [41] MONGOLIAN LETTER ALI GALI ANUSVARA ONE..MONGOLIAN LETTER MANCHU ALI GALI BHA
unicode_grapheme_base(0x18AA, 0x18AA).	% Grapheme_Base Lo       MONGOLIAN LETTER MANCHU ALI GALI LHA
unicode_grapheme_base(0x18B0, 0x18F5).	% Grapheme_Base Lo  [70] CANADIAN SYLLABICS OY..CANADIAN SYLLABICS CARRIER DENTAL S
unicode_grapheme_base(0x1900, 0x191C).	% Grapheme_Base Lo  [29] LIMBU VOWEL-CARRIER LETTER..LIMBU LETTER HA
unicode_grapheme_base(0x1923, 0x1926).	% Grapheme_Base Mc   [4] LIMBU VOWEL SIGN EE..LIMBU VOWEL SIGN AU
unicode_grapheme_base(0x1929, 0x192B).	% Grapheme_Base Mc   [3] LIMBU SUBJOINED LETTER YA..LIMBU SUBJOINED LETTER WA
unicode_grapheme_base(0x1930, 0x1931).	% Grapheme_Base Mc   [2] LIMBU SMALL LETTER KA..LIMBU SMALL LETTER NGA
unicode_grapheme_base(0x1933, 0x1938).	% Grapheme_Base Mc   [6] LIMBU SMALL LETTER TA..LIMBU SMALL LETTER LA
unicode_grapheme_base(0x1940, 0x1940).	% Grapheme_Base So       LIMBU SIGN LOO
unicode_grapheme_base(0x1944, 0x1945).	% Grapheme_Base Po   [2] LIMBU EXCLAMATION MARK..LIMBU QUESTION MARK
unicode_grapheme_base(0x1946, 0x194F).	% Grapheme_Base Nd  [10] LIMBU DIGIT ZERO..LIMBU DIGIT NINE
unicode_grapheme_base(0x1950, 0x196D).	% Grapheme_Base Lo  [30] TAI LE LETTER KA..TAI LE LETTER AI
unicode_grapheme_base(0x1970, 0x1974).	% Grapheme_Base Lo   [5] TAI LE LETTER TONE-2..TAI LE LETTER TONE-6
unicode_grapheme_base(0x1980, 0x19AB).	% Grapheme_Base Lo  [44] NEW TAI LUE LETTER HIGH QA..NEW TAI LUE LETTER LOW SUA
unicode_grapheme_base(0x19B0, 0x19C0).	% Grapheme_Base Mc  [17] NEW TAI LUE VOWEL SIGN VOWEL SHORTENER..NEW TAI LUE VOWEL SIGN IY
unicode_grapheme_base(0x19C1, 0x19C7).	% Grapheme_Base Lo   [7] NEW TAI LUE LETTER FINAL V..NEW TAI LUE LETTER FINAL B
unicode_grapheme_base(0x19C8, 0x19C9).	% Grapheme_Base Mc   [2] NEW TAI LUE TONE MARK-1..NEW TAI LUE TONE MARK-2
unicode_grapheme_base(0x19D0, 0x19D9).	% Grapheme_Base Nd  [10] NEW TAI LUE DIGIT ZERO..NEW TAI LUE DIGIT NINE
unicode_grapheme_base(0x19DA, 0x19DA).	% Grapheme_Base No       NEW TAI LUE THAM DIGIT ONE
unicode_grapheme_base(0x19DE, 0x19FF).	% Grapheme_Base So  [34] NEW TAI LUE SIGN LAE..KHMER SYMBOL DAP-PRAM ROC
unicode_grapheme_base(0x1A00, 0x1A16).	% Grapheme_Base Lo  [23] BUGINESE LETTER KA..BUGINESE LETTER HA
unicode_grapheme_base(0x1A19, 0x1A1B).	% Grapheme_Base Mc   [3] BUGINESE VOWEL SIGN E..BUGINESE VOWEL SIGN AE
unicode_grapheme_base(0x1A1E, 0x1A1F).	% Grapheme_Base Po   [2] BUGINESE PALLAWA..BUGINESE END OF SECTION
unicode_grapheme_base(0x1A20, 0x1A54).	% Grapheme_Base Lo  [53] TAI THAM LETTER HIGH KA..TAI THAM LETTER GREAT SA
unicode_grapheme_base(0x1A55, 0x1A55).	% Grapheme_Base Mc       TAI THAM CONSONANT SIGN MEDIAL RA
unicode_grapheme_base(0x1A57, 0x1A57).	% Grapheme_Base Mc       TAI THAM CONSONANT SIGN LA TANG LAI
unicode_grapheme_base(0x1A61, 0x1A61).	% Grapheme_Base Mc       TAI THAM VOWEL SIGN A
unicode_grapheme_base(0x1A63, 0x1A64).	% Grapheme_Base Mc   [2] TAI THAM VOWEL SIGN AA..TAI THAM VOWEL SIGN TALL AA
unicode_grapheme_base(0x1A6D, 0x1A72).	% Grapheme_Base Mc   [6] TAI THAM VOWEL SIGN OY..TAI THAM VOWEL SIGN THAM AI
unicode_grapheme_base(0x1A80, 0x1A89).	% Grapheme_Base Nd  [10] TAI THAM HORA DIGIT ZERO..TAI THAM HORA DIGIT NINE
unicode_grapheme_base(0x1A90, 0x1A99).	% Grapheme_Base Nd  [10] TAI THAM THAM DIGIT ZERO..TAI THAM THAM DIGIT NINE
unicode_grapheme_base(0x1AA0, 0x1AA6).	% Grapheme_Base Po   [7] TAI THAM SIGN WIANG..TAI THAM SIGN REVERSED ROTATED RANA
unicode_grapheme_base(0x1AA7, 0x1AA7).	% Grapheme_Base Lm       TAI THAM SIGN MAI YAMOK
unicode_grapheme_base(0x1AA8, 0x1AAD).	% Grapheme_Base Po   [6] TAI THAM SIGN KAAN..TAI THAM SIGN CAANG
unicode_grapheme_base(0x1B04, 0x1B04).	% Grapheme_Base Mc       BALINESE SIGN BISAH
unicode_grapheme_base(0x1B05, 0x1B33).	% Grapheme_Base Lo  [47] BALINESE LETTER AKARA..BALINESE LETTER HA
unicode_grapheme_base(0x1B35, 0x1B35).	% Grapheme_Base Mc       BALINESE VOWEL SIGN TEDUNG
unicode_grapheme_base(0x1B3B, 0x1B3B).	% Grapheme_Base Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_grapheme_base(0x1B3D, 0x1B41).	% Grapheme_Base Mc   [5] BALINESE VOWEL SIGN LA LENGA TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG
unicode_grapheme_base(0x1B43, 0x1B44).	% Grapheme_Base Mc   [2] BALINESE VOWEL SIGN PEPET TEDUNG..BALINESE ADEG ADEG
unicode_grapheme_base(0x1B45, 0x1B4B).	% Grapheme_Base Lo   [7] BALINESE LETTER KAF SASAK..BALINESE LETTER ASYURA SASAK
unicode_grapheme_base(0x1B50, 0x1B59).	% Grapheme_Base Nd  [10] BALINESE DIGIT ZERO..BALINESE DIGIT NINE
unicode_grapheme_base(0x1B5A, 0x1B60).	% Grapheme_Base Po   [7] BALINESE PANTI..BALINESE PAMENENG
unicode_grapheme_base(0x1B61, 0x1B6A).	% Grapheme_Base So  [10] BALINESE MUSICAL SYMBOL DONG..BALINESE MUSICAL SYMBOL DANG GEDE
unicode_grapheme_base(0x1B74, 0x1B7C).	% Grapheme_Base So   [9] BALINESE MUSICAL SYMBOL RIGHT-HAND OPEN DUG..BALINESE MUSICAL SYMBOL LEFT-HAND OPEN PING
unicode_grapheme_base(0x1B82, 0x1B82).	% Grapheme_Base Mc       SUNDANESE SIGN PANGWISAD
unicode_grapheme_base(0x1B83, 0x1BA0).	% Grapheme_Base Lo  [30] SUNDANESE LETTER A..SUNDANESE LETTER HA
unicode_grapheme_base(0x1BA1, 0x1BA1).	% Grapheme_Base Mc       SUNDANESE CONSONANT SIGN PAMINGKAL
unicode_grapheme_base(0x1BA6, 0x1BA7).	% Grapheme_Base Mc   [2] SUNDANESE VOWEL SIGN PANAELAENG..SUNDANESE VOWEL SIGN PANOLONG
unicode_grapheme_base(0x1BAA, 0x1BAA).	% Grapheme_Base Mc       SUNDANESE SIGN PAMAAEH
unicode_grapheme_base(0x1BAC, 0x1BAD).	% Grapheme_Base Mc   [2] SUNDANESE CONSONANT SIGN PASANGAN MA..SUNDANESE CONSONANT SIGN PASANGAN WA
unicode_grapheme_base(0x1BAE, 0x1BAF).	% Grapheme_Base Lo   [2] SUNDANESE LETTER KHA..SUNDANESE LETTER SYA
unicode_grapheme_base(0x1BB0, 0x1BB9).	% Grapheme_Base Nd  [10] SUNDANESE DIGIT ZERO..SUNDANESE DIGIT NINE
unicode_grapheme_base(0x1BBA, 0x1BE5).	% Grapheme_Base Lo  [44] SUNDANESE AVAGRAHA..BATAK LETTER U
unicode_grapheme_base(0x1BE7, 0x1BE7).	% Grapheme_Base Mc       BATAK VOWEL SIGN E
unicode_grapheme_base(0x1BEA, 0x1BEC).	% Grapheme_Base Mc   [3] BATAK VOWEL SIGN I..BATAK VOWEL SIGN O
unicode_grapheme_base(0x1BEE, 0x1BEE).	% Grapheme_Base Mc       BATAK VOWEL SIGN U
unicode_grapheme_base(0x1BF2, 0x1BF3).	% Grapheme_Base Mc   [2] BATAK PANGOLAT..BATAK PANONGONAN
unicode_grapheme_base(0x1BFC, 0x1BFF).	% Grapheme_Base Po   [4] BATAK SYMBOL BINDU NA METEK..BATAK SYMBOL BINDU PANGOLAT
unicode_grapheme_base(0x1C00, 0x1C23).	% Grapheme_Base Lo  [36] LEPCHA LETTER KA..LEPCHA LETTER A
unicode_grapheme_base(0x1C24, 0x1C2B).	% Grapheme_Base Mc   [8] LEPCHA SUBJOINED LETTER YA..LEPCHA VOWEL SIGN UU
unicode_grapheme_base(0x1C34, 0x1C35).	% Grapheme_Base Mc   [2] LEPCHA CONSONANT SIGN NYIN-DO..LEPCHA CONSONANT SIGN KANG
unicode_grapheme_base(0x1C3B, 0x1C3F).	% Grapheme_Base Po   [5] LEPCHA PUNCTUATION TA-ROL..LEPCHA PUNCTUATION TSHOOK
unicode_grapheme_base(0x1C40, 0x1C49).	% Grapheme_Base Nd  [10] LEPCHA DIGIT ZERO..LEPCHA DIGIT NINE
unicode_grapheme_base(0x1C4D, 0x1C4F).	% Grapheme_Base Lo   [3] LEPCHA LETTER TTA..LEPCHA LETTER DDA
unicode_grapheme_base(0x1C50, 0x1C59).	% Grapheme_Base Nd  [10] OL CHIKI DIGIT ZERO..OL CHIKI DIGIT NINE
unicode_grapheme_base(0x1C5A, 0x1C77).	% Grapheme_Base Lo  [30] OL CHIKI LETTER LA..OL CHIKI LETTER OH
unicode_grapheme_base(0x1C78, 0x1C7D).	% Grapheme_Base Lm   [6] OL CHIKI MU TTUDDAG..OL CHIKI AHAD
unicode_grapheme_base(0x1C7E, 0x1C7F).	% Grapheme_Base Po   [2] OL CHIKI PUNCTUATION MUCAAD..OL CHIKI PUNCTUATION DOUBLE MUCAAD
unicode_grapheme_base(0x1CC0, 0x1CC7).	% Grapheme_Base Po   [8] SUNDANESE PUNCTUATION BINDU SURYA..SUNDANESE PUNCTUATION BINDU BA SATANGA
unicode_grapheme_base(0x1CD3, 0x1CD3).	% Grapheme_Base Po       VEDIC SIGN NIHSHVASA
unicode_grapheme_base(0x1CE1, 0x1CE1).	% Grapheme_Base Mc       VEDIC TONE ATHARVAVEDIC INDEPENDENT SVARITA
unicode_grapheme_base(0x1CE9, 0x1CEC).	% Grapheme_Base Lo   [4] VEDIC SIGN ANUSVARA ANTARGOMUKHA..VEDIC SIGN ANUSVARA VAMAGOMUKHA WITH TAIL
unicode_grapheme_base(0x1CEE, 0x1CF1).	% Grapheme_Base Lo   [4] VEDIC SIGN HEXIFORM LONG ANUSVARA..VEDIC SIGN ANUSVARA UBHAYATO MUKHA
unicode_grapheme_base(0x1CF2, 0x1CF3).	% Grapheme_Base Mc   [2] VEDIC SIGN ARDHAVISARGA..VEDIC SIGN ROTATED ARDHAVISARGA
unicode_grapheme_base(0x1CF5, 0x1CF6).	% Grapheme_Base Lo   [2] VEDIC SIGN JIHVAMULIYA..VEDIC SIGN UPADHMANIYA
unicode_grapheme_base(0x1D00, 0x1D2B).	% Grapheme_Base L&  [44] LATIN LETTER SMALL CAPITAL A..CYRILLIC LETTER SMALL CAPITAL EL
unicode_grapheme_base(0x1D2C, 0x1D6A).	% Grapheme_Base Lm  [63] MODIFIER LETTER CAPITAL A..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_grapheme_base(0x1D6B, 0x1D77).	% Grapheme_Base L&  [13] LATIN SMALL LETTER UE..LATIN SMALL LETTER TURNED G
unicode_grapheme_base(0x1D78, 0x1D78).	% Grapheme_Base Lm       MODIFIER LETTER CYRILLIC EN
unicode_grapheme_base(0x1D79, 0x1D9A).	% Grapheme_Base L&  [34] LATIN SMALL LETTER INSULAR G..LATIN SMALL LETTER EZH WITH RETROFLEX HOOK
unicode_grapheme_base(0x1D9B, 0x1DBF).	% Grapheme_Base Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_grapheme_base(0x1E00, 0x1F15).	% Grapheme_Base L& [278] LATIN CAPITAL LETTER A WITH RING BELOW..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_grapheme_base(0x1F18, 0x1F1D).	% Grapheme_Base L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_grapheme_base(0x1F20, 0x1F45).	% Grapheme_Base L&  [38] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_grapheme_base(0x1F48, 0x1F4D).	% Grapheme_Base L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_grapheme_base(0x1F50, 0x1F57).	% Grapheme_Base L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_grapheme_base(0x1F59, 0x1F59).	% Grapheme_Base L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_grapheme_base(0x1F5B, 0x1F5B).	% Grapheme_Base L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_grapheme_base(0x1F5D, 0x1F5D).	% Grapheme_Base L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_grapheme_base(0x1F5F, 0x1F7D).	% Grapheme_Base L&  [31] GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_grapheme_base(0x1F80, 0x1FB4).	% Grapheme_Base L&  [53] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_grapheme_base(0x1FB6, 0x1FBC).	% Grapheme_Base L&   [7] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_grapheme_base(0x1FBD, 0x1FBD).	% Grapheme_Base Sk       GREEK KORONIS
unicode_grapheme_base(0x1FBE, 0x1FBE).	% Grapheme_Base L&       GREEK PROSGEGRAMMENI
unicode_grapheme_base(0x1FBF, 0x1FC1).	% Grapheme_Base Sk   [3] GREEK PSILI..GREEK DIALYTIKA AND PERISPOMENI
unicode_grapheme_base(0x1FC2, 0x1FC4).	% Grapheme_Base L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_grapheme_base(0x1FC6, 0x1FCC).	% Grapheme_Base L&   [7] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_grapheme_base(0x1FCD, 0x1FCF).	% Grapheme_Base Sk   [3] GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI
unicode_grapheme_base(0x1FD0, 0x1FD3).	% Grapheme_Base L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_grapheme_base(0x1FD6, 0x1FDB).	% Grapheme_Base L&   [6] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_grapheme_base(0x1FDD, 0x1FDF).	% Grapheme_Base Sk   [3] GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI
unicode_grapheme_base(0x1FE0, 0x1FEC).	% Grapheme_Base L&  [13] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_grapheme_base(0x1FED, 0x1FEF).	% Grapheme_Base Sk   [3] GREEK DIALYTIKA AND VARIA..GREEK VARIA
unicode_grapheme_base(0x1FF2, 0x1FF4).	% Grapheme_Base L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_grapheme_base(0x1FF6, 0x1FFC).	% Grapheme_Base L&   [7] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_grapheme_base(0x1FFD, 0x1FFE).	% Grapheme_Base Sk   [2] GREEK OXIA..GREEK DASIA
unicode_grapheme_base(0x2000, 0x200A).	% Grapheme_Base Zs  [11] EN QUAD..HAIR SPACE
unicode_grapheme_base(0x2010, 0x2015).	% Grapheme_Base Pd   [6] HYPHEN..HORIZONTAL BAR
unicode_grapheme_base(0x2016, 0x2017).	% Grapheme_Base Po   [2] DOUBLE VERTICAL LINE..DOUBLE LOW LINE
unicode_grapheme_base(0x2018, 0x2018).	% Grapheme_Base Pi       LEFT SINGLE QUOTATION MARK
unicode_grapheme_base(0x2019, 0x2019).	% Grapheme_Base Pf       RIGHT SINGLE QUOTATION MARK
unicode_grapheme_base(0x201A, 0x201A).	% Grapheme_Base Ps       SINGLE LOW-9 QUOTATION MARK
unicode_grapheme_base(0x201B, 0x201C).	% Grapheme_Base Pi   [2] SINGLE HIGH-REVERSED-9 QUOTATION MARK..LEFT DOUBLE QUOTATION MARK
unicode_grapheme_base(0x201D, 0x201D).	% Grapheme_Base Pf       RIGHT DOUBLE QUOTATION MARK
unicode_grapheme_base(0x201E, 0x201E).	% Grapheme_Base Ps       DOUBLE LOW-9 QUOTATION MARK
unicode_grapheme_base(0x201F, 0x201F).	% Grapheme_Base Pi       DOUBLE HIGH-REVERSED-9 QUOTATION MARK
unicode_grapheme_base(0x2020, 0x2027).	% Grapheme_Base Po   [8] DAGGER..HYPHENATION POINT
unicode_grapheme_base(0x202F, 0x202F).	% Grapheme_Base Zs       NARROW NO-BREAK SPACE
unicode_grapheme_base(0x2030, 0x2038).	% Grapheme_Base Po   [9] PER MILLE SIGN..CARET
unicode_grapheme_base(0x2039, 0x2039).	% Grapheme_Base Pi       SINGLE LEFT-POINTING ANGLE QUOTATION MARK
unicode_grapheme_base(0x203A, 0x203A).	% Grapheme_Base Pf       SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
unicode_grapheme_base(0x203B, 0x203E).	% Grapheme_Base Po   [4] REFERENCE MARK..OVERLINE
unicode_grapheme_base(0x203F, 0x2040).	% Grapheme_Base Pc   [2] UNDERTIE..CHARACTER TIE
unicode_grapheme_base(0x2041, 0x2043).	% Grapheme_Base Po   [3] CARET INSERTION POINT..HYPHEN BULLET
unicode_grapheme_base(0x2044, 0x2044).	% Grapheme_Base Sm       FRACTION SLASH
unicode_grapheme_base(0x2045, 0x2045).	% Grapheme_Base Ps       LEFT SQUARE BRACKET WITH QUILL
unicode_grapheme_base(0x2046, 0x2046).	% Grapheme_Base Pe       RIGHT SQUARE BRACKET WITH QUILL
unicode_grapheme_base(0x2047, 0x2051).	% Grapheme_Base Po  [11] DOUBLE QUESTION MARK..TWO ASTERISKS ALIGNED VERTICALLY
unicode_grapheme_base(0x2052, 0x2052).	% Grapheme_Base Sm       COMMERCIAL MINUS SIGN
unicode_grapheme_base(0x2053, 0x2053).	% Grapheme_Base Po       SWUNG DASH
unicode_grapheme_base(0x2054, 0x2054).	% Grapheme_Base Pc       INVERTED UNDERTIE
unicode_grapheme_base(0x2055, 0x205E).	% Grapheme_Base Po  [10] FLOWER PUNCTUATION MARK..VERTICAL FOUR DOTS
unicode_grapheme_base(0x205F, 0x205F).	% Grapheme_Base Zs       MEDIUM MATHEMATICAL SPACE
unicode_grapheme_base(0x2070, 0x2070).	% Grapheme_Base No       SUPERSCRIPT ZERO
unicode_grapheme_base(0x2071, 0x2071).	% Grapheme_Base Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_grapheme_base(0x2074, 0x2079).	% Grapheme_Base No   [6] SUPERSCRIPT FOUR..SUPERSCRIPT NINE
unicode_grapheme_base(0x207A, 0x207C).	% Grapheme_Base Sm   [3] SUPERSCRIPT PLUS SIGN..SUPERSCRIPT EQUALS SIGN
unicode_grapheme_base(0x207D, 0x207D).	% Grapheme_Base Ps       SUPERSCRIPT LEFT PARENTHESIS
unicode_grapheme_base(0x207E, 0x207E).	% Grapheme_Base Pe       SUPERSCRIPT RIGHT PARENTHESIS
unicode_grapheme_base(0x207F, 0x207F).	% Grapheme_Base Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_grapheme_base(0x2080, 0x2089).	% Grapheme_Base No  [10] SUBSCRIPT ZERO..SUBSCRIPT NINE
unicode_grapheme_base(0x208A, 0x208C).	% Grapheme_Base Sm   [3] SUBSCRIPT PLUS SIGN..SUBSCRIPT EQUALS SIGN
unicode_grapheme_base(0x208D, 0x208D).	% Grapheme_Base Ps       SUBSCRIPT LEFT PARENTHESIS
unicode_grapheme_base(0x208E, 0x208E).	% Grapheme_Base Pe       SUBSCRIPT RIGHT PARENTHESIS
unicode_grapheme_base(0x2090, 0x209C).	% Grapheme_Base Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_grapheme_base(0x20A0, 0x20BA).	% Grapheme_Base Sc  [27] EURO-CURRENCY SIGN..TURKISH LIRA SIGN
unicode_grapheme_base(0x2100, 0x2101).	% Grapheme_Base So   [2] ACCOUNT OF..ADDRESSED TO THE SUBJECT
unicode_grapheme_base(0x2102, 0x2102).	% Grapheme_Base L&       DOUBLE-STRUCK CAPITAL C
unicode_grapheme_base(0x2103, 0x2106).	% Grapheme_Base So   [4] DEGREE CELSIUS..CADA UNA
unicode_grapheme_base(0x2107, 0x2107).	% Grapheme_Base L&       EULER CONSTANT
unicode_grapheme_base(0x2108, 0x2109).	% Grapheme_Base So   [2] SCRUPLE..DEGREE FAHRENHEIT
unicode_grapheme_base(0x210A, 0x2113).	% Grapheme_Base L&  [10] SCRIPT SMALL G..SCRIPT SMALL L
unicode_grapheme_base(0x2114, 0x2114).	% Grapheme_Base So       L B BAR SYMBOL
unicode_grapheme_base(0x2115, 0x2115).	% Grapheme_Base L&       DOUBLE-STRUCK CAPITAL N
unicode_grapheme_base(0x2116, 0x2117).	% Grapheme_Base So   [2] NUMERO SIGN..SOUND RECORDING COPYRIGHT
unicode_grapheme_base(0x2118, 0x2118).	% Grapheme_Base Sm       SCRIPT CAPITAL P
unicode_grapheme_base(0x2119, 0x211D).	% Grapheme_Base L&   [5] DOUBLE-STRUCK CAPITAL P..DOUBLE-STRUCK CAPITAL R
unicode_grapheme_base(0x211E, 0x2123).	% Grapheme_Base So   [6] PRESCRIPTION TAKE..VERSICLE
unicode_grapheme_base(0x2124, 0x2124).	% Grapheme_Base L&       DOUBLE-STRUCK CAPITAL Z
unicode_grapheme_base(0x2125, 0x2125).	% Grapheme_Base So       OUNCE SIGN
unicode_grapheme_base(0x2126, 0x2126).	% Grapheme_Base L&       OHM SIGN
unicode_grapheme_base(0x2127, 0x2127).	% Grapheme_Base So       INVERTED OHM SIGN
unicode_grapheme_base(0x2128, 0x2128).	% Grapheme_Base L&       BLACK-LETTER CAPITAL Z
unicode_grapheme_base(0x2129, 0x2129).	% Grapheme_Base So       TURNED GREEK SMALL LETTER IOTA
unicode_grapheme_base(0x212A, 0x212D).	% Grapheme_Base L&   [4] KELVIN SIGN..BLACK-LETTER CAPITAL C
unicode_grapheme_base(0x212E, 0x212E).	% Grapheme_Base So       ESTIMATED SYMBOL
unicode_grapheme_base(0x212F, 0x2134).	% Grapheme_Base L&   [6] SCRIPT SMALL E..SCRIPT SMALL O
unicode_grapheme_base(0x2135, 0x2138).	% Grapheme_Base Lo   [4] ALEF SYMBOL..DALET SYMBOL
unicode_grapheme_base(0x2139, 0x2139).	% Grapheme_Base L&       INFORMATION SOURCE
unicode_grapheme_base(0x213A, 0x213B).	% Grapheme_Base So   [2] ROTATED CAPITAL Q..FACSIMILE SIGN
unicode_grapheme_base(0x213C, 0x213F).	% Grapheme_Base L&   [4] DOUBLE-STRUCK SMALL PI..DOUBLE-STRUCK CAPITAL PI
unicode_grapheme_base(0x2140, 0x2144).	% Grapheme_Base Sm   [5] DOUBLE-STRUCK N-ARY SUMMATION..TURNED SANS-SERIF CAPITAL Y
unicode_grapheme_base(0x2145, 0x2149).	% Grapheme_Base L&   [5] DOUBLE-STRUCK ITALIC CAPITAL D..DOUBLE-STRUCK ITALIC SMALL J
unicode_grapheme_base(0x214A, 0x214A).	% Grapheme_Base So       PROPERTY LINE
unicode_grapheme_base(0x214B, 0x214B).	% Grapheme_Base Sm       TURNED AMPERSAND
unicode_grapheme_base(0x214C, 0x214D).	% Grapheme_Base So   [2] PER SIGN..AKTIESELSKAB
unicode_grapheme_base(0x214E, 0x214E).	% Grapheme_Base L&       TURNED SMALL F
unicode_grapheme_base(0x214F, 0x214F).	% Grapheme_Base So       SYMBOL FOR SAMARITAN SOURCE
unicode_grapheme_base(0x2150, 0x215F).	% Grapheme_Base No  [16] VULGAR FRACTION ONE SEVENTH..FRACTION NUMERATOR ONE
unicode_grapheme_base(0x2160, 0x2182).	% Grapheme_Base Nl  [35] ROMAN NUMERAL ONE..ROMAN NUMERAL TEN THOUSAND
unicode_grapheme_base(0x2183, 0x2184).	% Grapheme_Base L&   [2] ROMAN NUMERAL REVERSED ONE HUNDRED..LATIN SMALL LETTER REVERSED C
unicode_grapheme_base(0x2185, 0x2188).	% Grapheme_Base Nl   [4] ROMAN NUMERAL SIX LATE FORM..ROMAN NUMERAL ONE HUNDRED THOUSAND
unicode_grapheme_base(0x2189, 0x2189).	% Grapheme_Base No       VULGAR FRACTION ZERO THIRDS
unicode_grapheme_base(0x2190, 0x2194).	% Grapheme_Base Sm   [5] LEFTWARDS ARROW..LEFT RIGHT ARROW
unicode_grapheme_base(0x2195, 0x2199).	% Grapheme_Base So   [5] UP DOWN ARROW..SOUTH WEST ARROW
unicode_grapheme_base(0x219A, 0x219B).	% Grapheme_Base Sm   [2] LEFTWARDS ARROW WITH STROKE..RIGHTWARDS ARROW WITH STROKE
unicode_grapheme_base(0x219C, 0x219F).	% Grapheme_Base So   [4] LEFTWARDS WAVE ARROW..UPWARDS TWO HEADED ARROW
unicode_grapheme_base(0x21A0, 0x21A0).	% Grapheme_Base Sm       RIGHTWARDS TWO HEADED ARROW
unicode_grapheme_base(0x21A1, 0x21A2).	% Grapheme_Base So   [2] DOWNWARDS TWO HEADED ARROW..LEFTWARDS ARROW WITH TAIL
unicode_grapheme_base(0x21A3, 0x21A3).	% Grapheme_Base Sm       RIGHTWARDS ARROW WITH TAIL
unicode_grapheme_base(0x21A4, 0x21A5).	% Grapheme_Base So   [2] LEFTWARDS ARROW FROM BAR..UPWARDS ARROW FROM BAR
unicode_grapheme_base(0x21A6, 0x21A6).	% Grapheme_Base Sm       RIGHTWARDS ARROW FROM BAR
unicode_grapheme_base(0x21A7, 0x21AD).	% Grapheme_Base So   [7] DOWNWARDS ARROW FROM BAR..LEFT RIGHT WAVE ARROW
unicode_grapheme_base(0x21AE, 0x21AE).	% Grapheme_Base Sm       LEFT RIGHT ARROW WITH STROKE
unicode_grapheme_base(0x21AF, 0x21CD).	% Grapheme_Base So  [31] DOWNWARDS ZIGZAG ARROW..LEFTWARDS DOUBLE ARROW WITH STROKE
unicode_grapheme_base(0x21CE, 0x21CF).	% Grapheme_Base Sm   [2] LEFT RIGHT DOUBLE ARROW WITH STROKE..RIGHTWARDS DOUBLE ARROW WITH STROKE
unicode_grapheme_base(0x21D0, 0x21D1).	% Grapheme_Base So   [2] LEFTWARDS DOUBLE ARROW..UPWARDS DOUBLE ARROW
unicode_grapheme_base(0x21D2, 0x21D2).	% Grapheme_Base Sm       RIGHTWARDS DOUBLE ARROW
unicode_grapheme_base(0x21D3, 0x21D3).	% Grapheme_Base So       DOWNWARDS DOUBLE ARROW
unicode_grapheme_base(0x21D4, 0x21D4).	% Grapheme_Base Sm       LEFT RIGHT DOUBLE ARROW
unicode_grapheme_base(0x21D5, 0x21F3).	% Grapheme_Base So  [31] UP DOWN DOUBLE ARROW..UP DOWN WHITE ARROW
unicode_grapheme_base(0x21F4, 0x22FF).	% Grapheme_Base Sm [268] RIGHT ARROW WITH SMALL CIRCLE..Z NOTATION BAG MEMBERSHIP
unicode_grapheme_base(0x2300, 0x2307).	% Grapheme_Base So   [8] DIAMETER SIGN..WAVY LINE
unicode_grapheme_base(0x2308, 0x230B).	% Grapheme_Base Sm   [4] LEFT CEILING..RIGHT FLOOR
unicode_grapheme_base(0x230C, 0x231F).	% Grapheme_Base So  [20] BOTTOM RIGHT CROP..BOTTOM RIGHT CORNER
unicode_grapheme_base(0x2320, 0x2321).	% Grapheme_Base Sm   [2] TOP HALF INTEGRAL..BOTTOM HALF INTEGRAL
unicode_grapheme_base(0x2322, 0x2328).	% Grapheme_Base So   [7] FROWN..KEYBOARD
unicode_grapheme_base(0x2329, 0x2329).	% Grapheme_Base Ps       LEFT-POINTING ANGLE BRACKET
unicode_grapheme_base(0x232A, 0x232A).	% Grapheme_Base Pe       RIGHT-POINTING ANGLE BRACKET
unicode_grapheme_base(0x232B, 0x237B).	% Grapheme_Base So  [81] ERASE TO THE LEFT..NOT CHECK MARK
unicode_grapheme_base(0x237C, 0x237C).	% Grapheme_Base Sm       RIGHT ANGLE WITH DOWNWARDS ZIGZAG ARROW
unicode_grapheme_base(0x237D, 0x239A).	% Grapheme_Base So  [30] SHOULDERED OPEN BOX..CLEAR SCREEN SYMBOL
unicode_grapheme_base(0x239B, 0x23B3).	% Grapheme_Base Sm  [25] LEFT PARENTHESIS UPPER HOOK..SUMMATION BOTTOM
unicode_grapheme_base(0x23B4, 0x23DB).	% Grapheme_Base So  [40] TOP SQUARE BRACKET..FUSE
unicode_grapheme_base(0x23DC, 0x23E1).	% Grapheme_Base Sm   [6] TOP PARENTHESIS..BOTTOM TORTOISE SHELL BRACKET
unicode_grapheme_base(0x23E2, 0x23F3).	% Grapheme_Base So  [18] WHITE TRAPEZIUM..HOURGLASS WITH FLOWING SAND
unicode_grapheme_base(0x2400, 0x2426).	% Grapheme_Base So  [39] SYMBOL FOR NULL..SYMBOL FOR SUBSTITUTE FORM TWO
unicode_grapheme_base(0x2440, 0x244A).	% Grapheme_Base So  [11] OCR HOOK..OCR DOUBLE BACKSLASH
unicode_grapheme_base(0x2460, 0x249B).	% Grapheme_Base No  [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
unicode_grapheme_base(0x249C, 0x24E9).	% Grapheme_Base So  [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_grapheme_base(0x24EA, 0x24FF).	% Grapheme_Base No  [22] CIRCLED DIGIT ZERO..NEGATIVE CIRCLED DIGIT ZERO
unicode_grapheme_base(0x2500, 0x25B6).	% Grapheme_Base So [183] BOX DRAWINGS LIGHT HORIZONTAL..BLACK RIGHT-POINTING TRIANGLE
unicode_grapheme_base(0x25B7, 0x25B7).	% Grapheme_Base Sm       WHITE RIGHT-POINTING TRIANGLE
unicode_grapheme_base(0x25B8, 0x25C0).	% Grapheme_Base So   [9] BLACK RIGHT-POINTING SMALL TRIANGLE..BLACK LEFT-POINTING TRIANGLE
unicode_grapheme_base(0x25C1, 0x25C1).	% Grapheme_Base Sm       WHITE LEFT-POINTING TRIANGLE
unicode_grapheme_base(0x25C2, 0x25F7).	% Grapheme_Base So  [54] BLACK LEFT-POINTING SMALL TRIANGLE..WHITE CIRCLE WITH UPPER RIGHT QUADRANT
unicode_grapheme_base(0x25F8, 0x25FF).	% Grapheme_Base Sm   [8] UPPER LEFT TRIANGLE..LOWER RIGHT TRIANGLE
unicode_grapheme_base(0x2600, 0x266E).	% Grapheme_Base So [111] BLACK SUN WITH RAYS..MUSIC NATURAL SIGN
unicode_grapheme_base(0x266F, 0x266F).	% Grapheme_Base Sm       MUSIC SHARP SIGN
unicode_grapheme_base(0x2670, 0x26FF).	% Grapheme_Base So [144] WEST SYRIAC CROSS..WHITE FLAG WITH HORIZONTAL MIDDLE BLACK STRIPE
unicode_grapheme_base(0x2701, 0x2767).	% Grapheme_Base So [103] UPPER BLADE SCISSORS..ROTATED FLORAL HEART BULLET
unicode_grapheme_base(0x2768, 0x2768).	% Grapheme_Base Ps       MEDIUM LEFT PARENTHESIS ORNAMENT
unicode_grapheme_base(0x2769, 0x2769).	% Grapheme_Base Pe       MEDIUM RIGHT PARENTHESIS ORNAMENT
unicode_grapheme_base(0x276A, 0x276A).	% Grapheme_Base Ps       MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT
unicode_grapheme_base(0x276B, 0x276B).	% Grapheme_Base Pe       MEDIUM FLATTENED RIGHT PARENTHESIS ORNAMENT
unicode_grapheme_base(0x276C, 0x276C).	% Grapheme_Base Ps       MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT
unicode_grapheme_base(0x276D, 0x276D).	% Grapheme_Base Pe       MEDIUM RIGHT-POINTING ANGLE BRACKET ORNAMENT
unicode_grapheme_base(0x276E, 0x276E).	% Grapheme_Base Ps       HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT
unicode_grapheme_base(0x276F, 0x276F).	% Grapheme_Base Pe       HEAVY RIGHT-POINTING ANGLE QUOTATION MARK ORNAMENT
unicode_grapheme_base(0x2770, 0x2770).	% Grapheme_Base Ps       HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT
unicode_grapheme_base(0x2771, 0x2771).	% Grapheme_Base Pe       HEAVY RIGHT-POINTING ANGLE BRACKET ORNAMENT
unicode_grapheme_base(0x2772, 0x2772).	% Grapheme_Base Ps       LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT
unicode_grapheme_base(0x2773, 0x2773).	% Grapheme_Base Pe       LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT
unicode_grapheme_base(0x2774, 0x2774).	% Grapheme_Base Ps       MEDIUM LEFT CURLY BRACKET ORNAMENT
unicode_grapheme_base(0x2775, 0x2775).	% Grapheme_Base Pe       MEDIUM RIGHT CURLY BRACKET ORNAMENT
unicode_grapheme_base(0x2776, 0x2793).	% Grapheme_Base No  [30] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
unicode_grapheme_base(0x2794, 0x27BF).	% Grapheme_Base So  [44] HEAVY WIDE-HEADED RIGHTWARDS ARROW..DOUBLE CURLY LOOP
unicode_grapheme_base(0x27C0, 0x27C4).	% Grapheme_Base Sm   [5] THREE DIMENSIONAL ANGLE..OPEN SUPERSET
unicode_grapheme_base(0x27C5, 0x27C5).	% Grapheme_Base Ps       LEFT S-SHAPED BAG DELIMITER
unicode_grapheme_base(0x27C6, 0x27C6).	% Grapheme_Base Pe       RIGHT S-SHAPED BAG DELIMITER
unicode_grapheme_base(0x27C7, 0x27E5).	% Grapheme_Base Sm  [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
unicode_grapheme_base(0x27E6, 0x27E6).	% Grapheme_Base Ps       MATHEMATICAL LEFT WHITE SQUARE BRACKET
unicode_grapheme_base(0x27E7, 0x27E7).	% Grapheme_Base Pe       MATHEMATICAL RIGHT WHITE SQUARE BRACKET
unicode_grapheme_base(0x27E8, 0x27E8).	% Grapheme_Base Ps       MATHEMATICAL LEFT ANGLE BRACKET
unicode_grapheme_base(0x27E9, 0x27E9).	% Grapheme_Base Pe       MATHEMATICAL RIGHT ANGLE BRACKET
unicode_grapheme_base(0x27EA, 0x27EA).	% Grapheme_Base Ps       MATHEMATICAL LEFT DOUBLE ANGLE BRACKET
unicode_grapheme_base(0x27EB, 0x27EB).	% Grapheme_Base Pe       MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET
unicode_grapheme_base(0x27EC, 0x27EC).	% Grapheme_Base Ps       MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET
unicode_grapheme_base(0x27ED, 0x27ED).	% Grapheme_Base Pe       MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET
unicode_grapheme_base(0x27EE, 0x27EE).	% Grapheme_Base Ps       MATHEMATICAL LEFT FLATTENED PARENTHESIS
unicode_grapheme_base(0x27EF, 0x27EF).	% Grapheme_Base Pe       MATHEMATICAL RIGHT FLATTENED PARENTHESIS
unicode_grapheme_base(0x27F0, 0x27FF).	% Grapheme_Base Sm  [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
unicode_grapheme_base(0x2800, 0x28FF).	% Grapheme_Base So [256] BRAILLE PATTERN BLANK..BRAILLE PATTERN DOTS-12345678
unicode_grapheme_base(0x2900, 0x2982).	% Grapheme_Base Sm [131] RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE..Z NOTATION TYPE COLON
unicode_grapheme_base(0x2983, 0x2983).	% Grapheme_Base Ps       LEFT WHITE CURLY BRACKET
unicode_grapheme_base(0x2984, 0x2984).	% Grapheme_Base Pe       RIGHT WHITE CURLY BRACKET
unicode_grapheme_base(0x2985, 0x2985).	% Grapheme_Base Ps       LEFT WHITE PARENTHESIS
unicode_grapheme_base(0x2986, 0x2986).	% Grapheme_Base Pe       RIGHT WHITE PARENTHESIS
unicode_grapheme_base(0x2987, 0x2987).	% Grapheme_Base Ps       Z NOTATION LEFT IMAGE BRACKET
unicode_grapheme_base(0x2988, 0x2988).	% Grapheme_Base Pe       Z NOTATION RIGHT IMAGE BRACKET
unicode_grapheme_base(0x2989, 0x2989).	% Grapheme_Base Ps       Z NOTATION LEFT BINDING BRACKET
unicode_grapheme_base(0x298A, 0x298A).	% Grapheme_Base Pe       Z NOTATION RIGHT BINDING BRACKET
unicode_grapheme_base(0x298B, 0x298B).	% Grapheme_Base Ps       LEFT SQUARE BRACKET WITH UNDERBAR
unicode_grapheme_base(0x298C, 0x298C).	% Grapheme_Base Pe       RIGHT SQUARE BRACKET WITH UNDERBAR
unicode_grapheme_base(0x298D, 0x298D).	% Grapheme_Base Ps       LEFT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_grapheme_base(0x298E, 0x298E).	% Grapheme_Base Pe       RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_grapheme_base(0x298F, 0x298F).	% Grapheme_Base Ps       LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_grapheme_base(0x2990, 0x2990).	% Grapheme_Base Pe       RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_grapheme_base(0x2991, 0x2991).	% Grapheme_Base Ps       LEFT ANGLE BRACKET WITH DOT
unicode_grapheme_base(0x2992, 0x2992).	% Grapheme_Base Pe       RIGHT ANGLE BRACKET WITH DOT
unicode_grapheme_base(0x2993, 0x2993).	% Grapheme_Base Ps       LEFT ARC LESS-THAN BRACKET
unicode_grapheme_base(0x2994, 0x2994).	% Grapheme_Base Pe       RIGHT ARC GREATER-THAN BRACKET
unicode_grapheme_base(0x2995, 0x2995).	% Grapheme_Base Ps       DOUBLE LEFT ARC GREATER-THAN BRACKET
unicode_grapheme_base(0x2996, 0x2996).	% Grapheme_Base Pe       DOUBLE RIGHT ARC LESS-THAN BRACKET
unicode_grapheme_base(0x2997, 0x2997).	% Grapheme_Base Ps       LEFT BLACK TORTOISE SHELL BRACKET
unicode_grapheme_base(0x2998, 0x2998).	% Grapheme_Base Pe       RIGHT BLACK TORTOISE SHELL BRACKET
unicode_grapheme_base(0x2999, 0x29D7).	% Grapheme_Base Sm  [63] DOTTED FENCE..BLACK HOURGLASS
unicode_grapheme_base(0x29D8, 0x29D8).	% Grapheme_Base Ps       LEFT WIGGLY FENCE
unicode_grapheme_base(0x29D9, 0x29D9).	% Grapheme_Base Pe       RIGHT WIGGLY FENCE
unicode_grapheme_base(0x29DA, 0x29DA).	% Grapheme_Base Ps       LEFT DOUBLE WIGGLY FENCE
unicode_grapheme_base(0x29DB, 0x29DB).	% Grapheme_Base Pe       RIGHT DOUBLE WIGGLY FENCE
unicode_grapheme_base(0x29DC, 0x29FB).	% Grapheme_Base Sm  [32] INCOMPLETE INFINITY..TRIPLE PLUS
unicode_grapheme_base(0x29FC, 0x29FC).	% Grapheme_Base Ps       LEFT-POINTING CURVED ANGLE BRACKET
unicode_grapheme_base(0x29FD, 0x29FD).	% Grapheme_Base Pe       RIGHT-POINTING CURVED ANGLE BRACKET
unicode_grapheme_base(0x29FE, 0x2AFF).	% Grapheme_Base Sm [258] TINY..N-ARY WHITE VERTICAL BAR
unicode_grapheme_base(0x2B00, 0x2B2F).	% Grapheme_Base So  [48] NORTH EAST WHITE ARROW..WHITE VERTICAL ELLIPSE
unicode_grapheme_base(0x2B30, 0x2B44).	% Grapheme_Base Sm  [21] LEFT ARROW WITH SMALL CIRCLE..RIGHTWARDS ARROW THROUGH SUPERSET
unicode_grapheme_base(0x2B45, 0x2B46).	% Grapheme_Base So   [2] LEFTWARDS QUADRUPLE ARROW..RIGHTWARDS QUADRUPLE ARROW
unicode_grapheme_base(0x2B47, 0x2B4C).	% Grapheme_Base Sm   [6] REVERSE TILDE OPERATOR ABOVE RIGHTWARDS ARROW..RIGHTWARDS ARROW ABOVE REVERSE TILDE OPERATOR
unicode_grapheme_base(0x2B50, 0x2B59).	% Grapheme_Base So  [10] WHITE MEDIUM STAR..HEAVY CIRCLED SALTIRE
unicode_grapheme_base(0x2C00, 0x2C2E).	% Grapheme_Base L&  [47] GLAGOLITIC CAPITAL LETTER AZU..GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_grapheme_base(0x2C30, 0x2C5E).	% Grapheme_Base L&  [47] GLAGOLITIC SMALL LETTER AZU..GLAGOLITIC SMALL LETTER LATINATE MYSLITE
unicode_grapheme_base(0x2C60, 0x2C7B).	% Grapheme_Base L&  [28] LATIN CAPITAL LETTER L WITH DOUBLE BAR..LATIN LETTER SMALL CAPITAL TURNED E
unicode_grapheme_base(0x2C7C, 0x2C7D).	% Grapheme_Base Lm   [2] LATIN SUBSCRIPT SMALL LETTER J..MODIFIER LETTER CAPITAL V
unicode_grapheme_base(0x2C7E, 0x2CE4).	% Grapheme_Base L& [103] LATIN CAPITAL LETTER S WITH SWASH TAIL..COPTIC SYMBOL KAI
unicode_grapheme_base(0x2CE5, 0x2CEA).	% Grapheme_Base So   [6] COPTIC SYMBOL MI RO..COPTIC SYMBOL SHIMA SIMA
unicode_grapheme_base(0x2CEB, 0x2CEE).	% Grapheme_Base L&   [4] COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI..COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
unicode_grapheme_base(0x2CF2, 0x2CF3).	% Grapheme_Base L&   [2] COPTIC CAPITAL LETTER BOHAIRIC KHEI..COPTIC SMALL LETTER BOHAIRIC KHEI
unicode_grapheme_base(0x2CF9, 0x2CFC).	% Grapheme_Base Po   [4] COPTIC OLD NUBIAN FULL STOP..COPTIC OLD NUBIAN VERSE DIVIDER
unicode_grapheme_base(0x2CFD, 0x2CFD).	% Grapheme_Base No       COPTIC FRACTION ONE HALF
unicode_grapheme_base(0x2CFE, 0x2CFF).	% Grapheme_Base Po   [2] COPTIC FULL STOP..COPTIC MORPHOLOGICAL DIVIDER
unicode_grapheme_base(0x2D00, 0x2D25).	% Grapheme_Base L&  [38] GEORGIAN SMALL LETTER AN..GEORGIAN SMALL LETTER HOE
unicode_grapheme_base(0x2D27, 0x2D27).	% Grapheme_Base L&       GEORGIAN SMALL LETTER YN
unicode_grapheme_base(0x2D2D, 0x2D2D).	% Grapheme_Base L&       GEORGIAN SMALL LETTER AEN
unicode_grapheme_base(0x2D30, 0x2D67).	% Grapheme_Base Lo  [56] TIFINAGH LETTER YA..TIFINAGH LETTER YO
unicode_grapheme_base(0x2D6F, 0x2D6F).	% Grapheme_Base Lm       TIFINAGH MODIFIER LETTER LABIALIZATION MARK
unicode_grapheme_base(0x2D70, 0x2D70).	% Grapheme_Base Po       TIFINAGH SEPARATOR MARK
unicode_grapheme_base(0x2D80, 0x2D96).	% Grapheme_Base Lo  [23] ETHIOPIC SYLLABLE LOA..ETHIOPIC SYLLABLE GGWE
unicode_grapheme_base(0x2DA0, 0x2DA6).	% Grapheme_Base Lo   [7] ETHIOPIC SYLLABLE SSA..ETHIOPIC SYLLABLE SSO
unicode_grapheme_base(0x2DA8, 0x2DAE).	% Grapheme_Base Lo   [7] ETHIOPIC SYLLABLE CCA..ETHIOPIC SYLLABLE CCO
unicode_grapheme_base(0x2DB0, 0x2DB6).	% Grapheme_Base Lo   [7] ETHIOPIC SYLLABLE ZZA..ETHIOPIC SYLLABLE ZZO
unicode_grapheme_base(0x2DB8, 0x2DBE).	% Grapheme_Base Lo   [7] ETHIOPIC SYLLABLE CCHA..ETHIOPIC SYLLABLE CCHO
unicode_grapheme_base(0x2DC0, 0x2DC6).	% Grapheme_Base Lo   [7] ETHIOPIC SYLLABLE QYA..ETHIOPIC SYLLABLE QYO
unicode_grapheme_base(0x2DC8, 0x2DCE).	% Grapheme_Base Lo   [7] ETHIOPIC SYLLABLE KYA..ETHIOPIC SYLLABLE KYO
unicode_grapheme_base(0x2DD0, 0x2DD6).	% Grapheme_Base Lo   [7] ETHIOPIC SYLLABLE XYA..ETHIOPIC SYLLABLE XYO
unicode_grapheme_base(0x2DD8, 0x2DDE).	% Grapheme_Base Lo   [7] ETHIOPIC SYLLABLE GYA..ETHIOPIC SYLLABLE GYO
unicode_grapheme_base(0x2E00, 0x2E01).	% Grapheme_Base Po   [2] RIGHT ANGLE SUBSTITUTION MARKER..RIGHT ANGLE DOTTED SUBSTITUTION MARKER
unicode_grapheme_base(0x2E02, 0x2E02).	% Grapheme_Base Pi       LEFT SUBSTITUTION BRACKET
unicode_grapheme_base(0x2E03, 0x2E03).	% Grapheme_Base Pf       RIGHT SUBSTITUTION BRACKET
unicode_grapheme_base(0x2E04, 0x2E04).	% Grapheme_Base Pi       LEFT DOTTED SUBSTITUTION BRACKET
unicode_grapheme_base(0x2E05, 0x2E05).	% Grapheme_Base Pf       RIGHT DOTTED SUBSTITUTION BRACKET
unicode_grapheme_base(0x2E06, 0x2E08).	% Grapheme_Base Po   [3] RAISED INTERPOLATION MARKER..DOTTED TRANSPOSITION MARKER
unicode_grapheme_base(0x2E09, 0x2E09).	% Grapheme_Base Pi       LEFT TRANSPOSITION BRACKET
unicode_grapheme_base(0x2E0A, 0x2E0A).	% Grapheme_Base Pf       RIGHT TRANSPOSITION BRACKET
unicode_grapheme_base(0x2E0B, 0x2E0B).	% Grapheme_Base Po       RAISED SQUARE
unicode_grapheme_base(0x2E0C, 0x2E0C).	% Grapheme_Base Pi       LEFT RAISED OMISSION BRACKET
unicode_grapheme_base(0x2E0D, 0x2E0D).	% Grapheme_Base Pf       RIGHT RAISED OMISSION BRACKET
unicode_grapheme_base(0x2E0E, 0x2E16).	% Grapheme_Base Po   [9] EDITORIAL CORONIS..DOTTED RIGHT-POINTING ANGLE
unicode_grapheme_base(0x2E17, 0x2E17).	% Grapheme_Base Pd       DOUBLE OBLIQUE HYPHEN
unicode_grapheme_base(0x2E18, 0x2E19).	% Grapheme_Base Po   [2] INVERTED INTERROBANG..PALM BRANCH
unicode_grapheme_base(0x2E1A, 0x2E1A).	% Grapheme_Base Pd       HYPHEN WITH DIAERESIS
unicode_grapheme_base(0x2E1B, 0x2E1B).	% Grapheme_Base Po       TILDE WITH RING ABOVE
unicode_grapheme_base(0x2E1C, 0x2E1C).	% Grapheme_Base Pi       LEFT LOW PARAPHRASE BRACKET
unicode_grapheme_base(0x2E1D, 0x2E1D).	% Grapheme_Base Pf       RIGHT LOW PARAPHRASE BRACKET
unicode_grapheme_base(0x2E1E, 0x2E1F).	% Grapheme_Base Po   [2] TILDE WITH DOT ABOVE..TILDE WITH DOT BELOW
unicode_grapheme_base(0x2E20, 0x2E20).	% Grapheme_Base Pi       LEFT VERTICAL BAR WITH QUILL
unicode_grapheme_base(0x2E21, 0x2E21).	% Grapheme_Base Pf       RIGHT VERTICAL BAR WITH QUILL
unicode_grapheme_base(0x2E22, 0x2E22).	% Grapheme_Base Ps       TOP LEFT HALF BRACKET
unicode_grapheme_base(0x2E23, 0x2E23).	% Grapheme_Base Pe       TOP RIGHT HALF BRACKET
unicode_grapheme_base(0x2E24, 0x2E24).	% Grapheme_Base Ps       BOTTOM LEFT HALF BRACKET
unicode_grapheme_base(0x2E25, 0x2E25).	% Grapheme_Base Pe       BOTTOM RIGHT HALF BRACKET
unicode_grapheme_base(0x2E26, 0x2E26).	% Grapheme_Base Ps       LEFT SIDEWAYS U BRACKET
unicode_grapheme_base(0x2E27, 0x2E27).	% Grapheme_Base Pe       RIGHT SIDEWAYS U BRACKET
unicode_grapheme_base(0x2E28, 0x2E28).	% Grapheme_Base Ps       LEFT DOUBLE PARENTHESIS
unicode_grapheme_base(0x2E29, 0x2E29).	% Grapheme_Base Pe       RIGHT DOUBLE PARENTHESIS
unicode_grapheme_base(0x2E2A, 0x2E2E).	% Grapheme_Base Po   [5] TWO DOTS OVER ONE DOT PUNCTUATION..REVERSED QUESTION MARK
unicode_grapheme_base(0x2E2F, 0x2E2F).	% Grapheme_Base Lm       VERTICAL TILDE
unicode_grapheme_base(0x2E30, 0x2E39).	% Grapheme_Base Po  [10] RING POINT..TOP HALF SECTION SIGN
unicode_grapheme_base(0x2E3A, 0x2E3B).	% Grapheme_Base Pd   [2] TWO-EM DASH..THREE-EM DASH
unicode_grapheme_base(0x2E80, 0x2E99).	% Grapheme_Base So  [26] CJK RADICAL REPEAT..CJK RADICAL RAP
unicode_grapheme_base(0x2E9B, 0x2EF3).	% Grapheme_Base So  [89] CJK RADICAL CHOKE..CJK RADICAL C-SIMPLIFIED TURTLE
unicode_grapheme_base(0x2F00, 0x2FD5).	% Grapheme_Base So [214] KANGXI RADICAL ONE..KANGXI RADICAL FLUTE
unicode_grapheme_base(0x2FF0, 0x2FFB).	% Grapheme_Base So  [12] IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT..IDEOGRAPHIC DESCRIPTION CHARACTER OVERLAID
unicode_grapheme_base(0x3000, 0x3000).	% Grapheme_Base Zs       IDEOGRAPHIC SPACE
unicode_grapheme_base(0x3001, 0x3003).	% Grapheme_Base Po   [3] IDEOGRAPHIC COMMA..DITTO MARK
unicode_grapheme_base(0x3004, 0x3004).	% Grapheme_Base So       JAPANESE INDUSTRIAL STANDARD SYMBOL
unicode_grapheme_base(0x3005, 0x3005).	% Grapheme_Base Lm       IDEOGRAPHIC ITERATION MARK
unicode_grapheme_base(0x3006, 0x3006).	% Grapheme_Base Lo       IDEOGRAPHIC CLOSING MARK
unicode_grapheme_base(0x3007, 0x3007).	% Grapheme_Base Nl       IDEOGRAPHIC NUMBER ZERO
unicode_grapheme_base(0x3008, 0x3008).	% Grapheme_Base Ps       LEFT ANGLE BRACKET
unicode_grapheme_base(0x3009, 0x3009).	% Grapheme_Base Pe       RIGHT ANGLE BRACKET
unicode_grapheme_base(0x300A, 0x300A).	% Grapheme_Base Ps       LEFT DOUBLE ANGLE BRACKET
unicode_grapheme_base(0x300B, 0x300B).	% Grapheme_Base Pe       RIGHT DOUBLE ANGLE BRACKET
unicode_grapheme_base(0x300C, 0x300C).	% Grapheme_Base Ps       LEFT CORNER BRACKET
unicode_grapheme_base(0x300D, 0x300D).	% Grapheme_Base Pe       RIGHT CORNER BRACKET
unicode_grapheme_base(0x300E, 0x300E).	% Grapheme_Base Ps       LEFT WHITE CORNER BRACKET
unicode_grapheme_base(0x300F, 0x300F).	% Grapheme_Base Pe       RIGHT WHITE CORNER BRACKET
unicode_grapheme_base(0x3010, 0x3010).	% Grapheme_Base Ps       LEFT BLACK LENTICULAR BRACKET
unicode_grapheme_base(0x3011, 0x3011).	% Grapheme_Base Pe       RIGHT BLACK LENTICULAR BRACKET
unicode_grapheme_base(0x3012, 0x3013).	% Grapheme_Base So   [2] POSTAL MARK..GETA MARK
unicode_grapheme_base(0x3014, 0x3014).	% Grapheme_Base Ps       LEFT TORTOISE SHELL BRACKET
unicode_grapheme_base(0x3015, 0x3015).	% Grapheme_Base Pe       RIGHT TORTOISE SHELL BRACKET
unicode_grapheme_base(0x3016, 0x3016).	% Grapheme_Base Ps       LEFT WHITE LENTICULAR BRACKET
unicode_grapheme_base(0x3017, 0x3017).	% Grapheme_Base Pe       RIGHT WHITE LENTICULAR BRACKET
unicode_grapheme_base(0x3018, 0x3018).	% Grapheme_Base Ps       LEFT WHITE TORTOISE SHELL BRACKET
unicode_grapheme_base(0x3019, 0x3019).	% Grapheme_Base Pe       RIGHT WHITE TORTOISE SHELL BRACKET
unicode_grapheme_base(0x301A, 0x301A).	% Grapheme_Base Ps       LEFT WHITE SQUARE BRACKET
unicode_grapheme_base(0x301B, 0x301B).	% Grapheme_Base Pe       RIGHT WHITE SQUARE BRACKET
unicode_grapheme_base(0x301C, 0x301C).	% Grapheme_Base Pd       WAVE DASH
unicode_grapheme_base(0x301D, 0x301D).	% Grapheme_Base Ps       REVERSED DOUBLE PRIME QUOTATION MARK
unicode_grapheme_base(0x301E, 0x301F).	% Grapheme_Base Pe   [2] DOUBLE PRIME QUOTATION MARK..LOW DOUBLE PRIME QUOTATION MARK
unicode_grapheme_base(0x3020, 0x3020).	% Grapheme_Base So       POSTAL MARK FACE
unicode_grapheme_base(0x3021, 0x3029).	% Grapheme_Base Nl   [9] HANGZHOU NUMERAL ONE..HANGZHOU NUMERAL NINE
unicode_grapheme_base(0x3030, 0x3030).	% Grapheme_Base Pd       WAVY DASH
unicode_grapheme_base(0x3031, 0x3035).	% Grapheme_Base Lm   [5] VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF
unicode_grapheme_base(0x3036, 0x3037).	% Grapheme_Base So   [2] CIRCLED POSTAL MARK..IDEOGRAPHIC TELEGRAPH LINE FEED SEPARATOR SYMBOL
unicode_grapheme_base(0x3038, 0x303A).	% Grapheme_Base Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
unicode_grapheme_base(0x303B, 0x303B).	% Grapheme_Base Lm       VERTICAL IDEOGRAPHIC ITERATION MARK
unicode_grapheme_base(0x303C, 0x303C).	% Grapheme_Base Lo       MASU MARK
unicode_grapheme_base(0x303D, 0x303D).	% Grapheme_Base Po       PART ALTERNATION MARK
unicode_grapheme_base(0x303E, 0x303F).	% Grapheme_Base So   [2] IDEOGRAPHIC VARIATION INDICATOR..IDEOGRAPHIC HALF FILL SPACE
unicode_grapheme_base(0x3041, 0x3096).	% Grapheme_Base Lo  [86] HIRAGANA LETTER SMALL A..HIRAGANA LETTER SMALL KE
unicode_grapheme_base(0x309B, 0x309C).	% Grapheme_Base Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_grapheme_base(0x309D, 0x309E).	% Grapheme_Base Lm   [2] HIRAGANA ITERATION MARK..HIRAGANA VOICED ITERATION MARK
unicode_grapheme_base(0x309F, 0x309F).	% Grapheme_Base Lo       HIRAGANA DIGRAPH YORI
unicode_grapheme_base(0x30A0, 0x30A0).	% Grapheme_Base Pd       KATAKANA-HIRAGANA DOUBLE HYPHEN
unicode_grapheme_base(0x30A1, 0x30FA).	% Grapheme_Base Lo  [90] KATAKANA LETTER SMALL A..KATAKANA LETTER VO
unicode_grapheme_base(0x30FB, 0x30FB).	% Grapheme_Base Po       KATAKANA MIDDLE DOT
unicode_grapheme_base(0x30FC, 0x30FE).	% Grapheme_Base Lm   [3] KATAKANA-HIRAGANA PROLONGED SOUND MARK..KATAKANA VOICED ITERATION MARK
unicode_grapheme_base(0x30FF, 0x30FF).	% Grapheme_Base Lo       KATAKANA DIGRAPH KOTO
unicode_grapheme_base(0x3105, 0x312D).	% Grapheme_Base Lo  [41] BOPOMOFO LETTER B..BOPOMOFO LETTER IH
unicode_grapheme_base(0x3131, 0x318E).	% Grapheme_Base Lo  [94] HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE
unicode_grapheme_base(0x3190, 0x3191).	% Grapheme_Base So   [2] IDEOGRAPHIC ANNOTATION LINKING MARK..IDEOGRAPHIC ANNOTATION REVERSE MARK
unicode_grapheme_base(0x3192, 0x3195).	% Grapheme_Base No   [4] IDEOGRAPHIC ANNOTATION ONE MARK..IDEOGRAPHIC ANNOTATION FOUR MARK
unicode_grapheme_base(0x3196, 0x319F).	% Grapheme_Base So  [10] IDEOGRAPHIC ANNOTATION TOP MARK..IDEOGRAPHIC ANNOTATION MAN MARK
unicode_grapheme_base(0x31A0, 0x31BA).	% Grapheme_Base Lo  [27] BOPOMOFO LETTER BU..BOPOMOFO LETTER ZY
unicode_grapheme_base(0x31C0, 0x31E3).	% Grapheme_Base So  [36] CJK STROKE T..CJK STROKE Q
unicode_grapheme_base(0x31F0, 0x31FF).	% Grapheme_Base Lo  [16] KATAKANA LETTER SMALL KU..KATAKANA LETTER SMALL RO
unicode_grapheme_base(0x3200, 0x321E).	% Grapheme_Base So  [31] PARENTHESIZED HANGUL KIYEOK..PARENTHESIZED KOREAN CHARACTER O HU
unicode_grapheme_base(0x3220, 0x3229).	% Grapheme_Base No  [10] PARENTHESIZED IDEOGRAPH ONE..PARENTHESIZED IDEOGRAPH TEN
unicode_grapheme_base(0x322A, 0x3247).	% Grapheme_Base So  [30] PARENTHESIZED IDEOGRAPH MOON..CIRCLED IDEOGRAPH KOTO
unicode_grapheme_base(0x3248, 0x324F).	% Grapheme_Base No   [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
unicode_grapheme_base(0x3250, 0x3250).	% Grapheme_Base So       PARTNERSHIP SIGN
unicode_grapheme_base(0x3251, 0x325F).	% Grapheme_Base No  [15] CIRCLED NUMBER TWENTY ONE..CIRCLED NUMBER THIRTY FIVE
unicode_grapheme_base(0x3260, 0x327F).	% Grapheme_Base So  [32] CIRCLED HANGUL KIYEOK..KOREAN STANDARD SYMBOL
unicode_grapheme_base(0x3280, 0x3289).	% Grapheme_Base No  [10] CIRCLED IDEOGRAPH ONE..CIRCLED IDEOGRAPH TEN
unicode_grapheme_base(0x328A, 0x32B0).	% Grapheme_Base So  [39] CIRCLED IDEOGRAPH MOON..CIRCLED IDEOGRAPH NIGHT
unicode_grapheme_base(0x32B1, 0x32BF).	% Grapheme_Base No  [15] CIRCLED NUMBER THIRTY SIX..CIRCLED NUMBER FIFTY
unicode_grapheme_base(0x32C0, 0x32FE).	% Grapheme_Base So  [63] IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY..CIRCLED KATAKANA WO
unicode_grapheme_base(0x3300, 0x33FF).	% Grapheme_Base So [256] SQUARE APAATO..SQUARE GAL
unicode_grapheme_base(0x3400, 0x4DB5).	% Grapheme_Base Lo [6582] CJK UNIFIED IDEOGRAPH-3400..CJK UNIFIED IDEOGRAPH-4DB5
unicode_grapheme_base(0x4DC0, 0x4DFF).	% Grapheme_Base So  [64] HEXAGRAM FOR THE CREATIVE HEAVEN..HEXAGRAM FOR BEFORE COMPLETION
unicode_grapheme_base(0x4E00, 0x9FCC).	% Grapheme_Base Lo [20941] CJK UNIFIED IDEOGRAPH-4E00..CJK UNIFIED IDEOGRAPH-9FCC
unicode_grapheme_base(0xA000, 0xA014).	% Grapheme_Base Lo  [21] YI SYLLABLE IT..YI SYLLABLE E
unicode_grapheme_base(0xA015, 0xA015).	% Grapheme_Base Lm       YI SYLLABLE WU
unicode_grapheme_base(0xA016, 0xA48C).	% Grapheme_Base Lo [1143] YI SYLLABLE BIT..YI SYLLABLE YYR
unicode_grapheme_base(0xA490, 0xA4C6).	% Grapheme_Base So  [55] YI RADICAL QOT..YI RADICAL KE
unicode_grapheme_base(0xA4D0, 0xA4F7).	% Grapheme_Base Lo  [40] LISU LETTER BA..LISU LETTER OE
unicode_grapheme_base(0xA4F8, 0xA4FD).	% Grapheme_Base Lm   [6] LISU LETTER TONE MYA TI..LISU LETTER TONE MYA JEU
unicode_grapheme_base(0xA4FE, 0xA4FF).	% Grapheme_Base Po   [2] LISU PUNCTUATION COMMA..LISU PUNCTUATION FULL STOP
unicode_grapheme_base(0xA500, 0xA60B).	% Grapheme_Base Lo [268] VAI SYLLABLE EE..VAI SYLLABLE NG
unicode_grapheme_base(0xA60C, 0xA60C).	% Grapheme_Base Lm       VAI SYLLABLE LENGTHENER
unicode_grapheme_base(0xA60D, 0xA60F).	% Grapheme_Base Po   [3] VAI COMMA..VAI QUESTION MARK
unicode_grapheme_base(0xA610, 0xA61F).	% Grapheme_Base Lo  [16] VAI SYLLABLE NDOLE FA..VAI SYMBOL JONG
unicode_grapheme_base(0xA620, 0xA629).	% Grapheme_Base Nd  [10] VAI DIGIT ZERO..VAI DIGIT NINE
unicode_grapheme_base(0xA62A, 0xA62B).	% Grapheme_Base Lo   [2] VAI SYLLABLE NDOLE MA..VAI SYLLABLE NDOLE DO
unicode_grapheme_base(0xA640, 0xA66D).	% Grapheme_Base L&  [46] CYRILLIC CAPITAL LETTER ZEMLYA..CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
unicode_grapheme_base(0xA66E, 0xA66E).	% Grapheme_Base Lo       CYRILLIC LETTER MULTIOCULAR O
unicode_grapheme_base(0xA673, 0xA673).	% Grapheme_Base Po       SLAVONIC ASTERISK
unicode_grapheme_base(0xA67E, 0xA67E).	% Grapheme_Base Po       CYRILLIC KAVYKA
unicode_grapheme_base(0xA67F, 0xA67F).	% Grapheme_Base Lm       CYRILLIC PAYEROK
unicode_grapheme_base(0xA680, 0xA697).	% Grapheme_Base L&  [24] CYRILLIC CAPITAL LETTER DWE..CYRILLIC SMALL LETTER SHWE
unicode_grapheme_base(0xA6A0, 0xA6E5).	% Grapheme_Base Lo  [70] BAMUM LETTER A..BAMUM LETTER KI
unicode_grapheme_base(0xA6E6, 0xA6EF).	% Grapheme_Base Nl  [10] BAMUM LETTER MO..BAMUM LETTER KOGHOM
unicode_grapheme_base(0xA6F2, 0xA6F7).	% Grapheme_Base Po   [6] BAMUM NJAEMLI..BAMUM QUESTION MARK
unicode_grapheme_base(0xA700, 0xA716).	% Grapheme_Base Sk  [23] MODIFIER LETTER CHINESE TONE YIN PING..MODIFIER LETTER EXTRA-LOW LEFT-STEM TONE BAR
unicode_grapheme_base(0xA717, 0xA71F).	% Grapheme_Base Lm   [9] MODIFIER LETTER DOT VERTICAL BAR..MODIFIER LETTER LOW INVERTED EXCLAMATION MARK
unicode_grapheme_base(0xA720, 0xA721).	% Grapheme_Base Sk   [2] MODIFIER LETTER STRESS AND HIGH TONE..MODIFIER LETTER STRESS AND LOW TONE
unicode_grapheme_base(0xA722, 0xA76F).	% Grapheme_Base L&  [78] LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF..LATIN SMALL LETTER CON
unicode_grapheme_base(0xA770, 0xA770).	% Grapheme_Base Lm       MODIFIER LETTER US
unicode_grapheme_base(0xA771, 0xA787).	% Grapheme_Base L&  [23] LATIN SMALL LETTER DUM..LATIN SMALL LETTER INSULAR T
unicode_grapheme_base(0xA788, 0xA788).	% Grapheme_Base Lm       MODIFIER LETTER LOW CIRCUMFLEX ACCENT
unicode_grapheme_base(0xA789, 0xA78A).	% Grapheme_Base Sk   [2] MODIFIER LETTER COLON..MODIFIER LETTER SHORT EQUALS SIGN
unicode_grapheme_base(0xA78B, 0xA78E).	% Grapheme_Base L&   [4] LATIN CAPITAL LETTER SALTILLO..LATIN SMALL LETTER L WITH RETROFLEX HOOK AND BELT
unicode_grapheme_base(0xA790, 0xA793).	% Grapheme_Base L&   [4] LATIN CAPITAL LETTER N WITH DESCENDER..LATIN SMALL LETTER C WITH BAR
unicode_grapheme_base(0xA7A0, 0xA7AA).	% Grapheme_Base L&  [11] LATIN CAPITAL LETTER G WITH OBLIQUE STROKE..LATIN CAPITAL LETTER H WITH HOOK
unicode_grapheme_base(0xA7F8, 0xA7F9).	% Grapheme_Base Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_grapheme_base(0xA7FA, 0xA7FA).	% Grapheme_Base L&       LATIN LETTER SMALL CAPITAL TURNED M
unicode_grapheme_base(0xA7FB, 0xA801).	% Grapheme_Base Lo   [7] LATIN EPIGRAPHIC LETTER REVERSED F..SYLOTI NAGRI LETTER I
unicode_grapheme_base(0xA803, 0xA805).	% Grapheme_Base Lo   [3] SYLOTI NAGRI LETTER U..SYLOTI NAGRI LETTER O
unicode_grapheme_base(0xA807, 0xA80A).	% Grapheme_Base Lo   [4] SYLOTI NAGRI LETTER KO..SYLOTI NAGRI LETTER GHO
unicode_grapheme_base(0xA80C, 0xA822).	% Grapheme_Base Lo  [23] SYLOTI NAGRI LETTER CO..SYLOTI NAGRI LETTER HO
unicode_grapheme_base(0xA823, 0xA824).	% Grapheme_Base Mc   [2] SYLOTI NAGRI VOWEL SIGN A..SYLOTI NAGRI VOWEL SIGN I
unicode_grapheme_base(0xA827, 0xA827).	% Grapheme_Base Mc       SYLOTI NAGRI VOWEL SIGN OO
unicode_grapheme_base(0xA828, 0xA82B).	% Grapheme_Base So   [4] SYLOTI NAGRI POETRY MARK-1..SYLOTI NAGRI POETRY MARK-4
unicode_grapheme_base(0xA830, 0xA835).	% Grapheme_Base No   [6] NORTH INDIC FRACTION ONE QUARTER..NORTH INDIC FRACTION THREE SIXTEENTHS
unicode_grapheme_base(0xA836, 0xA837).	% Grapheme_Base So   [2] NORTH INDIC QUARTER MARK..NORTH INDIC PLACEHOLDER MARK
unicode_grapheme_base(0xA838, 0xA838).	% Grapheme_Base Sc       NORTH INDIC RUPEE MARK
unicode_grapheme_base(0xA839, 0xA839).	% Grapheme_Base So       NORTH INDIC QUANTITY MARK
unicode_grapheme_base(0xA840, 0xA873).	% Grapheme_Base Lo  [52] PHAGS-PA LETTER KA..PHAGS-PA LETTER CANDRABINDU
unicode_grapheme_base(0xA874, 0xA877).	% Grapheme_Base Po   [4] PHAGS-PA SINGLE HEAD MARK..PHAGS-PA MARK DOUBLE SHAD
unicode_grapheme_base(0xA880, 0xA881).	% Grapheme_Base Mc   [2] SAURASHTRA SIGN ANUSVARA..SAURASHTRA SIGN VISARGA
unicode_grapheme_base(0xA882, 0xA8B3).	% Grapheme_Base Lo  [50] SAURASHTRA LETTER A..SAURASHTRA LETTER LLA
unicode_grapheme_base(0xA8B4, 0xA8C3).	% Grapheme_Base Mc  [16] SAURASHTRA CONSONANT SIGN HAARU..SAURASHTRA VOWEL SIGN AU
unicode_grapheme_base(0xA8CE, 0xA8CF).	% Grapheme_Base Po   [2] SAURASHTRA DANDA..SAURASHTRA DOUBLE DANDA
unicode_grapheme_base(0xA8D0, 0xA8D9).	% Grapheme_Base Nd  [10] SAURASHTRA DIGIT ZERO..SAURASHTRA DIGIT NINE
unicode_grapheme_base(0xA8F2, 0xA8F7).	% Grapheme_Base Lo   [6] DEVANAGARI SIGN SPACING CANDRABINDU..DEVANAGARI SIGN CANDRABINDU AVAGRAHA
unicode_grapheme_base(0xA8F8, 0xA8FA).	% Grapheme_Base Po   [3] DEVANAGARI SIGN PUSHPIKA..DEVANAGARI CARET
unicode_grapheme_base(0xA8FB, 0xA8FB).	% Grapheme_Base Lo       DEVANAGARI HEADSTROKE
unicode_grapheme_base(0xA900, 0xA909).	% Grapheme_Base Nd  [10] KAYAH LI DIGIT ZERO..KAYAH LI DIGIT NINE
unicode_grapheme_base(0xA90A, 0xA925).	% Grapheme_Base Lo  [28] KAYAH LI LETTER KA..KAYAH LI LETTER OO
unicode_grapheme_base(0xA92E, 0xA92F).	% Grapheme_Base Po   [2] KAYAH LI SIGN CWI..KAYAH LI SIGN SHYA
unicode_grapheme_base(0xA930, 0xA946).	% Grapheme_Base Lo  [23] REJANG LETTER KA..REJANG LETTER A
unicode_grapheme_base(0xA952, 0xA953).	% Grapheme_Base Mc   [2] REJANG CONSONANT SIGN H..REJANG VIRAMA
unicode_grapheme_base(0xA95F, 0xA95F).	% Grapheme_Base Po       REJANG SECTION MARK
unicode_grapheme_base(0xA960, 0xA97C).	% Grapheme_Base Lo  [29] HANGUL CHOSEONG TIKEUT-MIEUM..HANGUL CHOSEONG SSANGYEORINHIEUH
unicode_grapheme_base(0xA983, 0xA983).	% Grapheme_Base Mc       JAVANESE SIGN WIGNYAN
unicode_grapheme_base(0xA984, 0xA9B2).	% Grapheme_Base Lo  [47] JAVANESE LETTER A..JAVANESE LETTER HA
unicode_grapheme_base(0xA9B4, 0xA9B5).	% Grapheme_Base Mc   [2] JAVANESE VOWEL SIGN TARUNG..JAVANESE VOWEL SIGN TOLONG
unicode_grapheme_base(0xA9BA, 0xA9BB).	% Grapheme_Base Mc   [2] JAVANESE VOWEL SIGN TALING..JAVANESE VOWEL SIGN DIRGA MURE
unicode_grapheme_base(0xA9BD, 0xA9C0).	% Grapheme_Base Mc   [4] JAVANESE CONSONANT SIGN KERET..JAVANESE PANGKON
unicode_grapheme_base(0xA9C1, 0xA9CD).	% Grapheme_Base Po  [13] JAVANESE LEFT RERENGGAN..JAVANESE TURNED PADA PISELEH
unicode_grapheme_base(0xA9CF, 0xA9CF).	% Grapheme_Base Lm       JAVANESE PANGRANGKEP
unicode_grapheme_base(0xA9D0, 0xA9D9).	% Grapheme_Base Nd  [10] JAVANESE DIGIT ZERO..JAVANESE DIGIT NINE
unicode_grapheme_base(0xA9DE, 0xA9DF).	% Grapheme_Base Po   [2] JAVANESE PADA TIRTA TUMETES..JAVANESE PADA ISEN-ISEN
unicode_grapheme_base(0xAA00, 0xAA28).	% Grapheme_Base Lo  [41] CHAM LETTER A..CHAM LETTER HA
unicode_grapheme_base(0xAA2F, 0xAA30).	% Grapheme_Base Mc   [2] CHAM VOWEL SIGN O..CHAM VOWEL SIGN AI
unicode_grapheme_base(0xAA33, 0xAA34).	% Grapheme_Base Mc   [2] CHAM CONSONANT SIGN YA..CHAM CONSONANT SIGN RA
unicode_grapheme_base(0xAA40, 0xAA42).	% Grapheme_Base Lo   [3] CHAM LETTER FINAL K..CHAM LETTER FINAL NG
unicode_grapheme_base(0xAA44, 0xAA4B).	% Grapheme_Base Lo   [8] CHAM LETTER FINAL CH..CHAM LETTER FINAL SS
unicode_grapheme_base(0xAA4D, 0xAA4D).	% Grapheme_Base Mc       CHAM CONSONANT SIGN FINAL H
unicode_grapheme_base(0xAA50, 0xAA59).	% Grapheme_Base Nd  [10] CHAM DIGIT ZERO..CHAM DIGIT NINE
unicode_grapheme_base(0xAA5C, 0xAA5F).	% Grapheme_Base Po   [4] CHAM PUNCTUATION SPIRAL..CHAM PUNCTUATION TRIPLE DANDA
unicode_grapheme_base(0xAA60, 0xAA6F).	% Grapheme_Base Lo  [16] MYANMAR LETTER KHAMTI GA..MYANMAR LETTER KHAMTI FA
unicode_grapheme_base(0xAA70, 0xAA70).	% Grapheme_Base Lm       MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION
unicode_grapheme_base(0xAA71, 0xAA76).	% Grapheme_Base Lo   [6] MYANMAR LETTER KHAMTI XA..MYANMAR LOGOGRAM KHAMTI HM
unicode_grapheme_base(0xAA77, 0xAA79).	% Grapheme_Base So   [3] MYANMAR SYMBOL AITON EXCLAMATION..MYANMAR SYMBOL AITON TWO
unicode_grapheme_base(0xAA7A, 0xAA7A).	% Grapheme_Base Lo       MYANMAR LETTER AITON RA
unicode_grapheme_base(0xAA7B, 0xAA7B).	% Grapheme_Base Mc       MYANMAR SIGN PAO KAREN TONE
unicode_grapheme_base(0xAA80, 0xAAAF).	% Grapheme_Base Lo  [48] TAI VIET LETTER LOW KO..TAI VIET LETTER HIGH O
unicode_grapheme_base(0xAAB1, 0xAAB1).	% Grapheme_Base Lo       TAI VIET VOWEL AA
unicode_grapheme_base(0xAAB5, 0xAAB6).	% Grapheme_Base Lo   [2] TAI VIET VOWEL E..TAI VIET VOWEL O
unicode_grapheme_base(0xAAB9, 0xAABD).	% Grapheme_Base Lo   [5] TAI VIET VOWEL UEA..TAI VIET VOWEL AN
unicode_grapheme_base(0xAAC0, 0xAAC0).	% Grapheme_Base Lo       TAI VIET TONE MAI NUENG
unicode_grapheme_base(0xAAC2, 0xAAC2).	% Grapheme_Base Lo       TAI VIET TONE MAI SONG
unicode_grapheme_base(0xAADB, 0xAADC).	% Grapheme_Base Lo   [2] TAI VIET SYMBOL KON..TAI VIET SYMBOL NUENG
unicode_grapheme_base(0xAADD, 0xAADD).	% Grapheme_Base Lm       TAI VIET SYMBOL SAM
unicode_grapheme_base(0xAADE, 0xAADF).	% Grapheme_Base Po   [2] TAI VIET SYMBOL HO HOI..TAI VIET SYMBOL KOI KOI
unicode_grapheme_base(0xAAE0, 0xAAEA).	% Grapheme_Base Lo  [11] MEETEI MAYEK LETTER E..MEETEI MAYEK LETTER SSA
unicode_grapheme_base(0xAAEB, 0xAAEB).	% Grapheme_Base Mc       MEETEI MAYEK VOWEL SIGN II
unicode_grapheme_base(0xAAEE, 0xAAEF).	% Grapheme_Base Mc   [2] MEETEI MAYEK VOWEL SIGN AU..MEETEI MAYEK VOWEL SIGN AAU
unicode_grapheme_base(0xAAF0, 0xAAF1).	% Grapheme_Base Po   [2] MEETEI MAYEK CHEIKHAN..MEETEI MAYEK AHANG KHUDAM
unicode_grapheme_base(0xAAF2, 0xAAF2).	% Grapheme_Base Lo       MEETEI MAYEK ANJI
unicode_grapheme_base(0xAAF3, 0xAAF4).	% Grapheme_Base Lm   [2] MEETEI MAYEK SYLLABLE REPETITION MARK..MEETEI MAYEK WORD REPETITION MARK
unicode_grapheme_base(0xAAF5, 0xAAF5).	% Grapheme_Base Mc       MEETEI MAYEK VOWEL SIGN VISARGA
unicode_grapheme_base(0xAB01, 0xAB06).	% Grapheme_Base Lo   [6] ETHIOPIC SYLLABLE TTHU..ETHIOPIC SYLLABLE TTHO
unicode_grapheme_base(0xAB09, 0xAB0E).	% Grapheme_Base Lo   [6] ETHIOPIC SYLLABLE DDHU..ETHIOPIC SYLLABLE DDHO
unicode_grapheme_base(0xAB11, 0xAB16).	% Grapheme_Base Lo   [6] ETHIOPIC SYLLABLE DZU..ETHIOPIC SYLLABLE DZO
unicode_grapheme_base(0xAB20, 0xAB26).	% Grapheme_Base Lo   [7] ETHIOPIC SYLLABLE CCHHA..ETHIOPIC SYLLABLE CCHHO
unicode_grapheme_base(0xAB28, 0xAB2E).	% Grapheme_Base Lo   [7] ETHIOPIC SYLLABLE BBA..ETHIOPIC SYLLABLE BBO
unicode_grapheme_base(0xABC0, 0xABE2).	% Grapheme_Base Lo  [35] MEETEI MAYEK LETTER KOK..MEETEI MAYEK LETTER I LONSUM
unicode_grapheme_base(0xABE3, 0xABE4).	% Grapheme_Base Mc   [2] MEETEI MAYEK VOWEL SIGN ONAP..MEETEI MAYEK VOWEL SIGN INAP
unicode_grapheme_base(0xABE6, 0xABE7).	% Grapheme_Base Mc   [2] MEETEI MAYEK VOWEL SIGN YENAP..MEETEI MAYEK VOWEL SIGN SOUNAP
unicode_grapheme_base(0xABE9, 0xABEA).	% Grapheme_Base Mc   [2] MEETEI MAYEK VOWEL SIGN CHEINAP..MEETEI MAYEK VOWEL SIGN NUNG
unicode_grapheme_base(0xABEB, 0xABEB).	% Grapheme_Base Po       MEETEI MAYEK CHEIKHEI
unicode_grapheme_base(0xABEC, 0xABEC).	% Grapheme_Base Mc       MEETEI MAYEK LUM IYEK
unicode_grapheme_base(0xABF0, 0xABF9).	% Grapheme_Base Nd  [10] MEETEI MAYEK DIGIT ZERO..MEETEI MAYEK DIGIT NINE
unicode_grapheme_base(0xAC00, 0xD7A3).	% Grapheme_Base Lo [11172] HANGUL SYLLABLE GA..HANGUL SYLLABLE HIH
unicode_grapheme_base(0xD7B0, 0xD7C6).	% Grapheme_Base Lo  [23] HANGUL JUNGSEONG O-YEO..HANGUL JUNGSEONG ARAEA-E
unicode_grapheme_base(0xD7CB, 0xD7FB).	% Grapheme_Base Lo  [49] HANGUL JONGSEONG NIEUN-RIEUL..HANGUL JONGSEONG PHIEUPH-THIEUTH
unicode_grapheme_base(0xF900, 0xFA6D).	% Grapheme_Base Lo [366] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA6D
unicode_grapheme_base(0xFA70, 0xFAD9).	% Grapheme_Base Lo [106] CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9
unicode_grapheme_base(0xFB00, 0xFB06).	% Grapheme_Base L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_grapheme_base(0xFB13, 0xFB17).	% Grapheme_Base L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_grapheme_base(0xFB1D, 0xFB1D).	% Grapheme_Base Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_grapheme_base(0xFB1F, 0xFB28).	% Grapheme_Base Lo  [10] HEBREW LIGATURE YIDDISH YOD YOD PATAH..HEBREW LETTER WIDE TAV
unicode_grapheme_base(0xFB29, 0xFB29).	% Grapheme_Base Sm       HEBREW LETTER ALTERNATIVE PLUS SIGN
unicode_grapheme_base(0xFB2A, 0xFB36).	% Grapheme_Base Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_grapheme_base(0xFB38, 0xFB3C).	% Grapheme_Base Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_grapheme_base(0xFB3E, 0xFB3E).	% Grapheme_Base Lo       HEBREW LETTER MEM WITH DAGESH
unicode_grapheme_base(0xFB40, 0xFB41).	% Grapheme_Base Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_grapheme_base(0xFB43, 0xFB44).	% Grapheme_Base Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_grapheme_base(0xFB46, 0xFBB1).	% Grapheme_Base Lo [108] HEBREW LETTER TSADI WITH DAGESH..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
unicode_grapheme_base(0xFBB2, 0xFBC1).	% Grapheme_Base Sk  [16] ARABIC SYMBOL DOT ABOVE..ARABIC SYMBOL SMALL TAH BELOW
unicode_grapheme_base(0xFBD3, 0xFD3D).	% Grapheme_Base Lo [363] ARABIC LETTER NG ISOLATED FORM..ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
unicode_grapheme_base(0xFD3E, 0xFD3E).	% Grapheme_Base Ps       ORNATE LEFT PARENTHESIS
unicode_grapheme_base(0xFD3F, 0xFD3F).	% Grapheme_Base Pe       ORNATE RIGHT PARENTHESIS
unicode_grapheme_base(0xFD50, 0xFD8F).	% Grapheme_Base Lo  [64] ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
unicode_grapheme_base(0xFD92, 0xFDC7).	% Grapheme_Base Lo  [54] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
unicode_grapheme_base(0xFDF0, 0xFDFB).	% Grapheme_Base Lo  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
unicode_grapheme_base(0xFDFC, 0xFDFC).	% Grapheme_Base Sc       RIAL SIGN
unicode_grapheme_base(0xFDFD, 0xFDFD).	% Grapheme_Base So       ARABIC LIGATURE BISMILLAH AR-RAHMAN AR-RAHEEM
unicode_grapheme_base(0xFE10, 0xFE16).	% Grapheme_Base Po   [7] PRESENTATION FORM FOR VERTICAL COMMA..PRESENTATION FORM FOR VERTICAL QUESTION MARK
unicode_grapheme_base(0xFE17, 0xFE17).	% Grapheme_Base Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE LENTICULAR BRACKET
unicode_grapheme_base(0xFE18, 0xFE18).	% Grapheme_Base Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET
unicode_grapheme_base(0xFE19, 0xFE19).	% Grapheme_Base Po       PRESENTATION FORM FOR VERTICAL HORIZONTAL ELLIPSIS
unicode_grapheme_base(0xFE30, 0xFE30).	% Grapheme_Base Po       PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
unicode_grapheme_base(0xFE31, 0xFE32).	% Grapheme_Base Pd   [2] PRESENTATION FORM FOR VERTICAL EM DASH..PRESENTATION FORM FOR VERTICAL EN DASH
unicode_grapheme_base(0xFE33, 0xFE34).	% Grapheme_Base Pc   [2] PRESENTATION FORM FOR VERTICAL LOW LINE..PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
unicode_grapheme_base(0xFE35, 0xFE35).	% Grapheme_Base Ps       PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
unicode_grapheme_base(0xFE36, 0xFE36).	% Grapheme_Base Pe       PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
unicode_grapheme_base(0xFE37, 0xFE37).	% Grapheme_Base Ps       PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
unicode_grapheme_base(0xFE38, 0xFE38).	% Grapheme_Base Pe       PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
unicode_grapheme_base(0xFE39, 0xFE39).	% Grapheme_Base Ps       PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
unicode_grapheme_base(0xFE3A, 0xFE3A).	% Grapheme_Base Pe       PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
unicode_grapheme_base(0xFE3B, 0xFE3B).	% Grapheme_Base Ps       PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
unicode_grapheme_base(0xFE3C, 0xFE3C).	% Grapheme_Base Pe       PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
unicode_grapheme_base(0xFE3D, 0xFE3D).	% Grapheme_Base Ps       PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
unicode_grapheme_base(0xFE3E, 0xFE3E).	% Grapheme_Base Pe       PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
unicode_grapheme_base(0xFE3F, 0xFE3F).	% Grapheme_Base Ps       PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
unicode_grapheme_base(0xFE40, 0xFE40).	% Grapheme_Base Pe       PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
unicode_grapheme_base(0xFE41, 0xFE41).	% Grapheme_Base Ps       PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
unicode_grapheme_base(0xFE42, 0xFE42).	% Grapheme_Base Pe       PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
unicode_grapheme_base(0xFE43, 0xFE43).	% Grapheme_Base Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
unicode_grapheme_base(0xFE44, 0xFE44).	% Grapheme_Base Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
unicode_grapheme_base(0xFE45, 0xFE46).	% Grapheme_Base Po   [2] SESAME DOT..WHITE SESAME DOT
unicode_grapheme_base(0xFE47, 0xFE47).	% Grapheme_Base Ps       PRESENTATION FORM FOR VERTICAL LEFT SQUARE BRACKET
unicode_grapheme_base(0xFE48, 0xFE48).	% Grapheme_Base Pe       PRESENTATION FORM FOR VERTICAL RIGHT SQUARE BRACKET
unicode_grapheme_base(0xFE49, 0xFE4C).	% Grapheme_Base Po   [4] DASHED OVERLINE..DOUBLE WAVY OVERLINE
unicode_grapheme_base(0xFE4D, 0xFE4F).	% Grapheme_Base Pc   [3] DASHED LOW LINE..WAVY LOW LINE
unicode_grapheme_base(0xFE50, 0xFE52).	% Grapheme_Base Po   [3] SMALL COMMA..SMALL FULL STOP
unicode_grapheme_base(0xFE54, 0xFE57).	% Grapheme_Base Po   [4] SMALL SEMICOLON..SMALL EXCLAMATION MARK
unicode_grapheme_base(0xFE58, 0xFE58).	% Grapheme_Base Pd       SMALL EM DASH
unicode_grapheme_base(0xFE59, 0xFE59).	% Grapheme_Base Ps       SMALL LEFT PARENTHESIS
unicode_grapheme_base(0xFE5A, 0xFE5A).	% Grapheme_Base Pe       SMALL RIGHT PARENTHESIS
unicode_grapheme_base(0xFE5B, 0xFE5B).	% Grapheme_Base Ps       SMALL LEFT CURLY BRACKET
unicode_grapheme_base(0xFE5C, 0xFE5C).	% Grapheme_Base Pe       SMALL RIGHT CURLY BRACKET
unicode_grapheme_base(0xFE5D, 0xFE5D).	% Grapheme_Base Ps       SMALL LEFT TORTOISE SHELL BRACKET
unicode_grapheme_base(0xFE5E, 0xFE5E).	% Grapheme_Base Pe       SMALL RIGHT TORTOISE SHELL BRACKET
unicode_grapheme_base(0xFE5F, 0xFE61).	% Grapheme_Base Po   [3] SMALL NUMBER SIGN..SMALL ASTERISK
unicode_grapheme_base(0xFE62, 0xFE62).	% Grapheme_Base Sm       SMALL PLUS SIGN
unicode_grapheme_base(0xFE63, 0xFE63).	% Grapheme_Base Pd       SMALL HYPHEN-MINUS
unicode_grapheme_base(0xFE64, 0xFE66).	% Grapheme_Base Sm   [3] SMALL LESS-THAN SIGN..SMALL EQUALS SIGN
unicode_grapheme_base(0xFE68, 0xFE68).	% Grapheme_Base Po       SMALL REVERSE SOLIDUS
unicode_grapheme_base(0xFE69, 0xFE69).	% Grapheme_Base Sc       SMALL DOLLAR SIGN
unicode_grapheme_base(0xFE6A, 0xFE6B).	% Grapheme_Base Po   [2] SMALL PERCENT SIGN..SMALL COMMERCIAL AT
unicode_grapheme_base(0xFE70, 0xFE74).	% Grapheme_Base Lo   [5] ARABIC FATHATAN ISOLATED FORM..ARABIC KASRATAN ISOLATED FORM
unicode_grapheme_base(0xFE76, 0xFEFC).	% Grapheme_Base Lo [135] ARABIC FATHA ISOLATED FORM..ARABIC LIGATURE LAM WITH ALEF FINAL FORM
unicode_grapheme_base(0xFF01, 0xFF03).	% Grapheme_Base Po   [3] FULLWIDTH EXCLAMATION MARK..FULLWIDTH NUMBER SIGN
unicode_grapheme_base(0xFF04, 0xFF04).	% Grapheme_Base Sc       FULLWIDTH DOLLAR SIGN
unicode_grapheme_base(0xFF05, 0xFF07).	% Grapheme_Base Po   [3] FULLWIDTH PERCENT SIGN..FULLWIDTH APOSTROPHE
unicode_grapheme_base(0xFF08, 0xFF08).	% Grapheme_Base Ps       FULLWIDTH LEFT PARENTHESIS
unicode_grapheme_base(0xFF09, 0xFF09).	% Grapheme_Base Pe       FULLWIDTH RIGHT PARENTHESIS
unicode_grapheme_base(0xFF0A, 0xFF0A).	% Grapheme_Base Po       FULLWIDTH ASTERISK
unicode_grapheme_base(0xFF0B, 0xFF0B).	% Grapheme_Base Sm       FULLWIDTH PLUS SIGN
unicode_grapheme_base(0xFF0C, 0xFF0C).	% Grapheme_Base Po       FULLWIDTH COMMA
unicode_grapheme_base(0xFF0D, 0xFF0D).	% Grapheme_Base Pd       FULLWIDTH HYPHEN-MINUS
unicode_grapheme_base(0xFF0E, 0xFF0F).	% Grapheme_Base Po   [2] FULLWIDTH FULL STOP..FULLWIDTH SOLIDUS
unicode_grapheme_base(0xFF10, 0xFF19).	% Grapheme_Base Nd  [10] FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
unicode_grapheme_base(0xFF1A, 0xFF1B).	% Grapheme_Base Po   [2] FULLWIDTH COLON..FULLWIDTH SEMICOLON
unicode_grapheme_base(0xFF1C, 0xFF1E).	% Grapheme_Base Sm   [3] FULLWIDTH LESS-THAN SIGN..FULLWIDTH GREATER-THAN SIGN
unicode_grapheme_base(0xFF1F, 0xFF20).	% Grapheme_Base Po   [2] FULLWIDTH QUESTION MARK..FULLWIDTH COMMERCIAL AT
unicode_grapheme_base(0xFF21, 0xFF3A).	% Grapheme_Base L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_grapheme_base(0xFF3B, 0xFF3B).	% Grapheme_Base Ps       FULLWIDTH LEFT SQUARE BRACKET
unicode_grapheme_base(0xFF3C, 0xFF3C).	% Grapheme_Base Po       FULLWIDTH REVERSE SOLIDUS
unicode_grapheme_base(0xFF3D, 0xFF3D).	% Grapheme_Base Pe       FULLWIDTH RIGHT SQUARE BRACKET
unicode_grapheme_base(0xFF3E, 0xFF3E).	% Grapheme_Base Sk       FULLWIDTH CIRCUMFLEX ACCENT
unicode_grapheme_base(0xFF3F, 0xFF3F).	% Grapheme_Base Pc       FULLWIDTH LOW LINE
unicode_grapheme_base(0xFF40, 0xFF40).	% Grapheme_Base Sk       FULLWIDTH GRAVE ACCENT
unicode_grapheme_base(0xFF41, 0xFF5A).	% Grapheme_Base L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_grapheme_base(0xFF5B, 0xFF5B).	% Grapheme_Base Ps       FULLWIDTH LEFT CURLY BRACKET
unicode_grapheme_base(0xFF5C, 0xFF5C).	% Grapheme_Base Sm       FULLWIDTH VERTICAL LINE
unicode_grapheme_base(0xFF5D, 0xFF5D).	% Grapheme_Base Pe       FULLWIDTH RIGHT CURLY BRACKET
unicode_grapheme_base(0xFF5E, 0xFF5E).	% Grapheme_Base Sm       FULLWIDTH TILDE
unicode_grapheme_base(0xFF5F, 0xFF5F).	% Grapheme_Base Ps       FULLWIDTH LEFT WHITE PARENTHESIS
unicode_grapheme_base(0xFF60, 0xFF60).	% Grapheme_Base Pe       FULLWIDTH RIGHT WHITE PARENTHESIS
unicode_grapheme_base(0xFF61, 0xFF61).	% Grapheme_Base Po       HALFWIDTH IDEOGRAPHIC FULL STOP
unicode_grapheme_base(0xFF62, 0xFF62).	% Grapheme_Base Ps       HALFWIDTH LEFT CORNER BRACKET
unicode_grapheme_base(0xFF63, 0xFF63).	% Grapheme_Base Pe       HALFWIDTH RIGHT CORNER BRACKET
unicode_grapheme_base(0xFF64, 0xFF65).	% Grapheme_Base Po   [2] HALFWIDTH IDEOGRAPHIC COMMA..HALFWIDTH KATAKANA MIDDLE DOT
unicode_grapheme_base(0xFF66, 0xFF6F).	% Grapheme_Base Lo  [10] HALFWIDTH KATAKANA LETTER WO..HALFWIDTH KATAKANA LETTER SMALL TU
unicode_grapheme_base(0xFF70, 0xFF70).	% Grapheme_Base Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_grapheme_base(0xFF71, 0xFF9D).	% Grapheme_Base Lo  [45] HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N
unicode_grapheme_base(0xFFA0, 0xFFBE).	% Grapheme_Base Lo  [31] HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH
unicode_grapheme_base(0xFFC2, 0xFFC7).	% Grapheme_Base Lo   [6] HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E
unicode_grapheme_base(0xFFCA, 0xFFCF).	% Grapheme_Base Lo   [6] HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE
unicode_grapheme_base(0xFFD2, 0xFFD7).	% Grapheme_Base Lo   [6] HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU
unicode_grapheme_base(0xFFDA, 0xFFDC).	% Grapheme_Base Lo   [3] HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I
unicode_grapheme_base(0xFFE0, 0xFFE1).	% Grapheme_Base Sc   [2] FULLWIDTH CENT SIGN..FULLWIDTH POUND SIGN
unicode_grapheme_base(0xFFE2, 0xFFE2).	% Grapheme_Base Sm       FULLWIDTH NOT SIGN
unicode_grapheme_base(0xFFE3, 0xFFE3).	% Grapheme_Base Sk       FULLWIDTH MACRON
unicode_grapheme_base(0xFFE4, 0xFFE4).	% Grapheme_Base So       FULLWIDTH BROKEN BAR
unicode_grapheme_base(0xFFE5, 0xFFE6).	% Grapheme_Base Sc   [2] FULLWIDTH YEN SIGN..FULLWIDTH WON SIGN
unicode_grapheme_base(0xFFE8, 0xFFE8).	% Grapheme_Base So       HALFWIDTH FORMS LIGHT VERTICAL
unicode_grapheme_base(0xFFE9, 0xFFEC).	% Grapheme_Base Sm   [4] HALFWIDTH LEFTWARDS ARROW..HALFWIDTH DOWNWARDS ARROW
unicode_grapheme_base(0xFFED, 0xFFEE).	% Grapheme_Base So   [2] HALFWIDTH BLACK SQUARE..HALFWIDTH WHITE CIRCLE
unicode_grapheme_base(0xFFFC, 0xFFFD).	% Grapheme_Base So   [2] OBJECT REPLACEMENT CHARACTER..REPLACEMENT CHARACTER
unicode_grapheme_base(0x10000, 0x1000B).	% Grapheme_Base Lo  [12] LINEAR B SYLLABLE B008 A..LINEAR B SYLLABLE B046 JE
unicode_grapheme_base(0x1000D, 0x10026).	% Grapheme_Base Lo  [26] LINEAR B SYLLABLE B036 JO..LINEAR B SYLLABLE B032 QO
unicode_grapheme_base(0x10028, 0x1003A).	% Grapheme_Base Lo  [19] LINEAR B SYLLABLE B060 RA..LINEAR B SYLLABLE B042 WO
unicode_grapheme_base(0x1003C, 0x1003D).	% Grapheme_Base Lo   [2] LINEAR B SYLLABLE B017 ZA..LINEAR B SYLLABLE B074 ZE
unicode_grapheme_base(0x1003F, 0x1004D).	% Grapheme_Base Lo  [15] LINEAR B SYLLABLE B020 ZO..LINEAR B SYLLABLE B091 TWO
unicode_grapheme_base(0x10050, 0x1005D).	% Grapheme_Base Lo  [14] LINEAR B SYMBOL B018..LINEAR B SYMBOL B089
unicode_grapheme_base(0x10080, 0x100FA).	% Grapheme_Base Lo [123] LINEAR B IDEOGRAM B100 MAN..LINEAR B IDEOGRAM VESSEL B305
unicode_grapheme_base(0x10100, 0x10102).	% Grapheme_Base Po   [3] AEGEAN WORD SEPARATOR LINE..AEGEAN CHECK MARK
unicode_grapheme_base(0x10107, 0x10133).	% Grapheme_Base No  [45] AEGEAN NUMBER ONE..AEGEAN NUMBER NINETY THOUSAND
unicode_grapheme_base(0x10137, 0x1013F).	% Grapheme_Base So   [9] AEGEAN WEIGHT BASE UNIT..AEGEAN MEASURE THIRD SUBUNIT
unicode_grapheme_base(0x10140, 0x10174).	% Grapheme_Base Nl  [53] GREEK ACROPHONIC ATTIC ONE QUARTER..GREEK ACROPHONIC STRATIAN FIFTY MNAS
unicode_grapheme_base(0x10175, 0x10178).	% Grapheme_Base No   [4] GREEK ONE HALF SIGN..GREEK THREE QUARTERS SIGN
unicode_grapheme_base(0x10179, 0x10189).	% Grapheme_Base So  [17] GREEK YEAR SIGN..GREEK TRYBLION BASE SIGN
unicode_grapheme_base(0x1018A, 0x1018A).	% Grapheme_Base No       GREEK ZERO SIGN
unicode_grapheme_base(0x10190, 0x1019B).	% Grapheme_Base So  [12] ROMAN SEXTANS SIGN..ROMAN CENTURIAL SIGN
unicode_grapheme_base(0x101D0, 0x101FC).	% Grapheme_Base So  [45] PHAISTOS DISC SIGN PEDESTRIAN..PHAISTOS DISC SIGN WAVY BAND
unicode_grapheme_base(0x10280, 0x1029C).	% Grapheme_Base Lo  [29] LYCIAN LETTER A..LYCIAN LETTER X
unicode_grapheme_base(0x102A0, 0x102D0).	% Grapheme_Base Lo  [49] CARIAN LETTER A..CARIAN LETTER UUU3
unicode_grapheme_base(0x10300, 0x1031E).	% Grapheme_Base Lo  [31] OLD ITALIC LETTER A..OLD ITALIC LETTER UU
unicode_grapheme_base(0x10320, 0x10323).	% Grapheme_Base No   [4] OLD ITALIC NUMERAL ONE..OLD ITALIC NUMERAL FIFTY
unicode_grapheme_base(0x10330, 0x10340).	% Grapheme_Base Lo  [17] GOTHIC LETTER AHSA..GOTHIC LETTER PAIRTHRA
unicode_grapheme_base(0x10341, 0x10341).	% Grapheme_Base Nl       GOTHIC LETTER NINETY
unicode_grapheme_base(0x10342, 0x10349).	% Grapheme_Base Lo   [8] GOTHIC LETTER RAIDA..GOTHIC LETTER OTHAL
unicode_grapheme_base(0x1034A, 0x1034A).	% Grapheme_Base Nl       GOTHIC LETTER NINE HUNDRED
unicode_grapheme_base(0x10380, 0x1039D).	% Grapheme_Base Lo  [30] UGARITIC LETTER ALPA..UGARITIC LETTER SSU
unicode_grapheme_base(0x1039F, 0x1039F).	% Grapheme_Base Po       UGARITIC WORD DIVIDER
unicode_grapheme_base(0x103A0, 0x103C3).	% Grapheme_Base Lo  [36] OLD PERSIAN SIGN A..OLD PERSIAN SIGN HA
unicode_grapheme_base(0x103C8, 0x103CF).	% Grapheme_Base Lo   [8] OLD PERSIAN SIGN AURAMAZDAA..OLD PERSIAN SIGN BUUMISH
unicode_grapheme_base(0x103D0, 0x103D0).	% Grapheme_Base Po       OLD PERSIAN WORD DIVIDER
unicode_grapheme_base(0x103D1, 0x103D5).	% Grapheme_Base Nl   [5] OLD PERSIAN NUMBER ONE..OLD PERSIAN NUMBER HUNDRED
unicode_grapheme_base(0x10400, 0x1044F).	% Grapheme_Base L&  [80] DESERET CAPITAL LETTER LONG I..DESERET SMALL LETTER EW
unicode_grapheme_base(0x10450, 0x1049D).	% Grapheme_Base Lo  [78] SHAVIAN LETTER PEEP..OSMANYA LETTER OO
unicode_grapheme_base(0x104A0, 0x104A9).	% Grapheme_Base Nd  [10] OSMANYA DIGIT ZERO..OSMANYA DIGIT NINE
unicode_grapheme_base(0x10800, 0x10805).	% Grapheme_Base Lo   [6] CYPRIOT SYLLABLE A..CYPRIOT SYLLABLE JA
unicode_grapheme_base(0x10808, 0x10808).	% Grapheme_Base Lo       CYPRIOT SYLLABLE JO
unicode_grapheme_base(0x1080A, 0x10835).	% Grapheme_Base Lo  [44] CYPRIOT SYLLABLE KA..CYPRIOT SYLLABLE WO
unicode_grapheme_base(0x10837, 0x10838).	% Grapheme_Base Lo   [2] CYPRIOT SYLLABLE XA..CYPRIOT SYLLABLE XE
unicode_grapheme_base(0x1083C, 0x1083C).	% Grapheme_Base Lo       CYPRIOT SYLLABLE ZA
unicode_grapheme_base(0x1083F, 0x10855).	% Grapheme_Base Lo  [23] CYPRIOT SYLLABLE ZO..IMPERIAL ARAMAIC LETTER TAW
unicode_grapheme_base(0x10857, 0x10857).	% Grapheme_Base Po       IMPERIAL ARAMAIC SECTION SIGN
unicode_grapheme_base(0x10858, 0x1085F).	% Grapheme_Base No   [8] IMPERIAL ARAMAIC NUMBER ONE..IMPERIAL ARAMAIC NUMBER TEN THOUSAND
unicode_grapheme_base(0x10900, 0x10915).	% Grapheme_Base Lo  [22] PHOENICIAN LETTER ALF..PHOENICIAN LETTER TAU
unicode_grapheme_base(0x10916, 0x1091B).	% Grapheme_Base No   [6] PHOENICIAN NUMBER ONE..PHOENICIAN NUMBER THREE
unicode_grapheme_base(0x1091F, 0x1091F).	% Grapheme_Base Po       PHOENICIAN WORD SEPARATOR
unicode_grapheme_base(0x10920, 0x10939).	% Grapheme_Base Lo  [26] LYDIAN LETTER A..LYDIAN LETTER C
unicode_grapheme_base(0x1093F, 0x1093F).	% Grapheme_Base Po       LYDIAN TRIANGULAR MARK
unicode_grapheme_base(0x10980, 0x109B7).	% Grapheme_Base Lo  [56] MEROITIC HIEROGLYPHIC LETTER A..MEROITIC CURSIVE LETTER DA
unicode_grapheme_base(0x109BE, 0x109BF).	% Grapheme_Base Lo   [2] MEROITIC CURSIVE LOGOGRAM RMT..MEROITIC CURSIVE LOGOGRAM IMN
unicode_grapheme_base(0x10A00, 0x10A00).	% Grapheme_Base Lo       KHAROSHTHI LETTER A
unicode_grapheme_base(0x10A10, 0x10A13).	% Grapheme_Base Lo   [4] KHAROSHTHI LETTER KA..KHAROSHTHI LETTER GHA
unicode_grapheme_base(0x10A15, 0x10A17).	% Grapheme_Base Lo   [3] KHAROSHTHI LETTER CA..KHAROSHTHI LETTER JA
unicode_grapheme_base(0x10A19, 0x10A33).	% Grapheme_Base Lo  [27] KHAROSHTHI LETTER NYA..KHAROSHTHI LETTER TTTHA
unicode_grapheme_base(0x10A40, 0x10A47).	% Grapheme_Base No   [8] KHAROSHTHI DIGIT ONE..KHAROSHTHI NUMBER ONE THOUSAND
unicode_grapheme_base(0x10A50, 0x10A58).	% Grapheme_Base Po   [9] KHAROSHTHI PUNCTUATION DOT..KHAROSHTHI PUNCTUATION LINES
unicode_grapheme_base(0x10A60, 0x10A7C).	% Grapheme_Base Lo  [29] OLD SOUTH ARABIAN LETTER HE..OLD SOUTH ARABIAN LETTER THETH
unicode_grapheme_base(0x10A7D, 0x10A7E).	% Grapheme_Base No   [2] OLD SOUTH ARABIAN NUMBER ONE..OLD SOUTH ARABIAN NUMBER FIFTY
unicode_grapheme_base(0x10A7F, 0x10A7F).	% Grapheme_Base Po       OLD SOUTH ARABIAN NUMERIC INDICATOR
unicode_grapheme_base(0x10B00, 0x10B35).	% Grapheme_Base Lo  [54] AVESTAN LETTER A..AVESTAN LETTER HE
unicode_grapheme_base(0x10B39, 0x10B3F).	% Grapheme_Base Po   [7] AVESTAN ABBREVIATION MARK..LARGE ONE RING OVER TWO RINGS PUNCTUATION
unicode_grapheme_base(0x10B40, 0x10B55).	% Grapheme_Base Lo  [22] INSCRIPTIONAL PARTHIAN LETTER ALEPH..INSCRIPTIONAL PARTHIAN LETTER TAW
unicode_grapheme_base(0x10B58, 0x10B5F).	% Grapheme_Base No   [8] INSCRIPTIONAL PARTHIAN NUMBER ONE..INSCRIPTIONAL PARTHIAN NUMBER ONE THOUSAND
unicode_grapheme_base(0x10B60, 0x10B72).	% Grapheme_Base Lo  [19] INSCRIPTIONAL PAHLAVI LETTER ALEPH..INSCRIPTIONAL PAHLAVI LETTER TAW
unicode_grapheme_base(0x10B78, 0x10B7F).	% Grapheme_Base No   [8] INSCRIPTIONAL PAHLAVI NUMBER ONE..INSCRIPTIONAL PAHLAVI NUMBER ONE THOUSAND
unicode_grapheme_base(0x10C00, 0x10C48).	% Grapheme_Base Lo  [73] OLD TURKIC LETTER ORKHON A..OLD TURKIC LETTER ORKHON BASH
unicode_grapheme_base(0x10E60, 0x10E7E).	% Grapheme_Base No  [31] RUMI DIGIT ONE..RUMI FRACTION TWO THIRDS
unicode_grapheme_base(0x11000, 0x11000).	% Grapheme_Base Mc       BRAHMI SIGN CANDRABINDU
unicode_grapheme_base(0x11002, 0x11002).	% Grapheme_Base Mc       BRAHMI SIGN VISARGA
unicode_grapheme_base(0x11003, 0x11037).	% Grapheme_Base Lo  [53] BRAHMI SIGN JIHVAMULIYA..BRAHMI LETTER OLD TAMIL NNNA
unicode_grapheme_base(0x11047, 0x1104D).	% Grapheme_Base Po   [7] BRAHMI DANDA..BRAHMI PUNCTUATION LOTUS
unicode_grapheme_base(0x11052, 0x11065).	% Grapheme_Base No  [20] BRAHMI NUMBER ONE..BRAHMI NUMBER ONE THOUSAND
unicode_grapheme_base(0x11066, 0x1106F).	% Grapheme_Base Nd  [10] BRAHMI DIGIT ZERO..BRAHMI DIGIT NINE
unicode_grapheme_base(0x11082, 0x11082).	% Grapheme_Base Mc       KAITHI SIGN VISARGA
unicode_grapheme_base(0x11083, 0x110AF).	% Grapheme_Base Lo  [45] KAITHI LETTER A..KAITHI LETTER HA
unicode_grapheme_base(0x110B0, 0x110B2).	% Grapheme_Base Mc   [3] KAITHI VOWEL SIGN AA..KAITHI VOWEL SIGN II
unicode_grapheme_base(0x110B7, 0x110B8).	% Grapheme_Base Mc   [2] KAITHI VOWEL SIGN O..KAITHI VOWEL SIGN AU
unicode_grapheme_base(0x110BB, 0x110BC).	% Grapheme_Base Po   [2] KAITHI ABBREVIATION SIGN..KAITHI ENUMERATION SIGN
unicode_grapheme_base(0x110BE, 0x110C1).	% Grapheme_Base Po   [4] KAITHI SECTION MARK..KAITHI DOUBLE DANDA
unicode_grapheme_base(0x110D0, 0x110E8).	% Grapheme_Base Lo  [25] SORA SOMPENG LETTER SAH..SORA SOMPENG LETTER MAE
unicode_grapheme_base(0x110F0, 0x110F9).	% Grapheme_Base Nd  [10] SORA SOMPENG DIGIT ZERO..SORA SOMPENG DIGIT NINE
unicode_grapheme_base(0x11103, 0x11126).	% Grapheme_Base Lo  [36] CHAKMA LETTER AA..CHAKMA LETTER HAA
unicode_grapheme_base(0x1112C, 0x1112C).	% Grapheme_Base Mc       CHAKMA VOWEL SIGN E
unicode_grapheme_base(0x11136, 0x1113F).	% Grapheme_Base Nd  [10] CHAKMA DIGIT ZERO..CHAKMA DIGIT NINE
unicode_grapheme_base(0x11140, 0x11143).	% Grapheme_Base Po   [4] CHAKMA SECTION MARK..CHAKMA QUESTION MARK
unicode_grapheme_base(0x11182, 0x11182).	% Grapheme_Base Mc       SHARADA SIGN VISARGA
unicode_grapheme_base(0x11183, 0x111B2).	% Grapheme_Base Lo  [48] SHARADA LETTER A..SHARADA LETTER HA
unicode_grapheme_base(0x111B3, 0x111B5).	% Grapheme_Base Mc   [3] SHARADA VOWEL SIGN AA..SHARADA VOWEL SIGN II
unicode_grapheme_base(0x111BF, 0x111C0).	% Grapheme_Base Mc   [2] SHARADA VOWEL SIGN AU..SHARADA SIGN VIRAMA
unicode_grapheme_base(0x111C1, 0x111C4).	% Grapheme_Base Lo   [4] SHARADA SIGN AVAGRAHA..SHARADA OM
unicode_grapheme_base(0x111C5, 0x111C8).	% Grapheme_Base Po   [4] SHARADA DANDA..SHARADA SEPARATOR
unicode_grapheme_base(0x111D0, 0x111D9).	% Grapheme_Base Nd  [10] SHARADA DIGIT ZERO..SHARADA DIGIT NINE
unicode_grapheme_base(0x11680, 0x116AA).	% Grapheme_Base Lo  [43] TAKRI LETTER A..TAKRI LETTER RRA
unicode_grapheme_base(0x116AC, 0x116AC).	% Grapheme_Base Mc       TAKRI SIGN VISARGA
unicode_grapheme_base(0x116AE, 0x116AF).	% Grapheme_Base Mc   [2] TAKRI VOWEL SIGN I..TAKRI VOWEL SIGN II
unicode_grapheme_base(0x116B6, 0x116B6).	% Grapheme_Base Mc       TAKRI SIGN VIRAMA
unicode_grapheme_base(0x116C0, 0x116C9).	% Grapheme_Base Nd  [10] TAKRI DIGIT ZERO..TAKRI DIGIT NINE
unicode_grapheme_base(0x12000, 0x1236E).	% Grapheme_Base Lo [879] CUNEIFORM SIGN A..CUNEIFORM SIGN ZUM
unicode_grapheme_base(0x12400, 0x12462).	% Grapheme_Base Nl  [99] CUNEIFORM NUMERIC SIGN TWO ASH..CUNEIFORM NUMERIC SIGN OLD ASSYRIAN ONE QUARTER
unicode_grapheme_base(0x12470, 0x12473).	% Grapheme_Base Po   [4] CUNEIFORM PUNCTUATION SIGN OLD ASSYRIAN WORD DIVIDER..CUNEIFORM PUNCTUATION SIGN DIAGONAL TRICOLON
unicode_grapheme_base(0x13000, 0x1342E).	% Grapheme_Base Lo [1071] EGYPTIAN HIEROGLYPH A001..EGYPTIAN HIEROGLYPH AA032
unicode_grapheme_base(0x16800, 0x16A38).	% Grapheme_Base Lo [569] BAMUM LETTER PHASE-A NGKUE MFON..BAMUM LETTER PHASE-F VUEQ
unicode_grapheme_base(0x16F00, 0x16F44).	% Grapheme_Base Lo  [69] MIAO LETTER PA..MIAO LETTER HHA
unicode_grapheme_base(0x16F50, 0x16F50).	% Grapheme_Base Lo       MIAO LETTER NASALIZATION
unicode_grapheme_base(0x16F51, 0x16F7E).	% Grapheme_Base Mc  [46] MIAO SIGN ASPIRATION..MIAO VOWEL SIGN NG
unicode_grapheme_base(0x16F93, 0x16F9F).	% Grapheme_Base Lm  [13] MIAO LETTER TONE-2..MIAO LETTER REFORMED TONE-8
unicode_grapheme_base(0x1B000, 0x1B001).	% Grapheme_Base Lo   [2] KATAKANA LETTER ARCHAIC E..HIRAGANA LETTER ARCHAIC YE
unicode_grapheme_base(0x1D000, 0x1D0F5).	% Grapheme_Base So [246] BYZANTINE MUSICAL SYMBOL PSILI..BYZANTINE MUSICAL SYMBOL GORGON NEO KATO
unicode_grapheme_base(0x1D100, 0x1D126).	% Grapheme_Base So  [39] MUSICAL SYMBOL SINGLE BARLINE..MUSICAL SYMBOL DRUM CLEF-2
unicode_grapheme_base(0x1D129, 0x1D164).	% Grapheme_Base So  [60] MUSICAL SYMBOL MULTIPLE MEASURE REST..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
unicode_grapheme_base(0x1D166, 0x1D166).	% Grapheme_Base Mc       MUSICAL SYMBOL COMBINING SPRECHGESANG STEM
unicode_grapheme_base(0x1D16A, 0x1D16C).	% Grapheme_Base So   [3] MUSICAL SYMBOL FINGERED TREMOLO-1..MUSICAL SYMBOL FINGERED TREMOLO-3
unicode_grapheme_base(0x1D16D, 0x1D16D).	% Grapheme_Base Mc       MUSICAL SYMBOL COMBINING AUGMENTATION DOT
unicode_grapheme_base(0x1D183, 0x1D184).	% Grapheme_Base So   [2] MUSICAL SYMBOL ARPEGGIATO UP..MUSICAL SYMBOL ARPEGGIATO DOWN
unicode_grapheme_base(0x1D18C, 0x1D1A9).	% Grapheme_Base So  [30] MUSICAL SYMBOL RINFORZANDO..MUSICAL SYMBOL DEGREE SLASH
unicode_grapheme_base(0x1D1AE, 0x1D1DD).	% Grapheme_Base So  [48] MUSICAL SYMBOL PEDAL MARK..MUSICAL SYMBOL PES SUBPUNCTIS
unicode_grapheme_base(0x1D200, 0x1D241).	% Grapheme_Base So  [66] GREEK VOCAL NOTATION SYMBOL-1..GREEK INSTRUMENTAL NOTATION SYMBOL-54
unicode_grapheme_base(0x1D245, 0x1D245).	% Grapheme_Base So       GREEK MUSICAL LEIMMA
unicode_grapheme_base(0x1D300, 0x1D356).	% Grapheme_Base So  [87] MONOGRAM FOR EARTH..TETRAGRAM FOR FOSTERING
unicode_grapheme_base(0x1D360, 0x1D371).	% Grapheme_Base No  [18] COUNTING ROD UNIT DIGIT ONE..COUNTING ROD TENS DIGIT NINE
unicode_grapheme_base(0x1D400, 0x1D454).	% Grapheme_Base L&  [85] MATHEMATICAL BOLD CAPITAL A..MATHEMATICAL ITALIC SMALL G
unicode_grapheme_base(0x1D456, 0x1D49C).	% Grapheme_Base L&  [71] MATHEMATICAL ITALIC SMALL I..MATHEMATICAL SCRIPT CAPITAL A
unicode_grapheme_base(0x1D49E, 0x1D49F).	% Grapheme_Base L&   [2] MATHEMATICAL SCRIPT CAPITAL C..MATHEMATICAL SCRIPT CAPITAL D
unicode_grapheme_base(0x1D4A2, 0x1D4A2).	% Grapheme_Base L&       MATHEMATICAL SCRIPT CAPITAL G
unicode_grapheme_base(0x1D4A5, 0x1D4A6).	% Grapheme_Base L&   [2] MATHEMATICAL SCRIPT CAPITAL J..MATHEMATICAL SCRIPT CAPITAL K
unicode_grapheme_base(0x1D4A9, 0x1D4AC).	% Grapheme_Base L&   [4] MATHEMATICAL SCRIPT CAPITAL N..MATHEMATICAL SCRIPT CAPITAL Q
unicode_grapheme_base(0x1D4AE, 0x1D4B9).	% Grapheme_Base L&  [12] MATHEMATICAL SCRIPT CAPITAL S..MATHEMATICAL SCRIPT SMALL D
unicode_grapheme_base(0x1D4BB, 0x1D4BB).	% Grapheme_Base L&       MATHEMATICAL SCRIPT SMALL F
unicode_grapheme_base(0x1D4BD, 0x1D4C3).	% Grapheme_Base L&   [7] MATHEMATICAL SCRIPT SMALL H..MATHEMATICAL SCRIPT SMALL N
unicode_grapheme_base(0x1D4C5, 0x1D505).	% Grapheme_Base L&  [65] MATHEMATICAL SCRIPT SMALL P..MATHEMATICAL FRAKTUR CAPITAL B
unicode_grapheme_base(0x1D507, 0x1D50A).	% Grapheme_Base L&   [4] MATHEMATICAL FRAKTUR CAPITAL D..MATHEMATICAL FRAKTUR CAPITAL G
unicode_grapheme_base(0x1D50D, 0x1D514).	% Grapheme_Base L&   [8] MATHEMATICAL FRAKTUR CAPITAL J..MATHEMATICAL FRAKTUR CAPITAL Q
unicode_grapheme_base(0x1D516, 0x1D51C).	% Grapheme_Base L&   [7] MATHEMATICAL FRAKTUR CAPITAL S..MATHEMATICAL FRAKTUR CAPITAL Y
unicode_grapheme_base(0x1D51E, 0x1D539).	% Grapheme_Base L&  [28] MATHEMATICAL FRAKTUR SMALL A..MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_grapheme_base(0x1D53B, 0x1D53E).	% Grapheme_Base L&   [4] MATHEMATICAL DOUBLE-STRUCK CAPITAL D..MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_grapheme_base(0x1D540, 0x1D544).	% Grapheme_Base L&   [5] MATHEMATICAL DOUBLE-STRUCK CAPITAL I..MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_grapheme_base(0x1D546, 0x1D546).	% Grapheme_Base L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_grapheme_base(0x1D54A, 0x1D550).	% Grapheme_Base L&   [7] MATHEMATICAL DOUBLE-STRUCK CAPITAL S..MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_grapheme_base(0x1D552, 0x1D6A5).	% Grapheme_Base L& [340] MATHEMATICAL DOUBLE-STRUCK SMALL A..MATHEMATICAL ITALIC SMALL DOTLESS J
unicode_grapheme_base(0x1D6A8, 0x1D6C0).	% Grapheme_Base L&  [25] MATHEMATICAL BOLD CAPITAL ALPHA..MATHEMATICAL BOLD CAPITAL OMEGA
unicode_grapheme_base(0x1D6C1, 0x1D6C1).	% Grapheme_Base Sm       MATHEMATICAL BOLD NABLA
unicode_grapheme_base(0x1D6C2, 0x1D6DA).	% Grapheme_Base L&  [25] MATHEMATICAL BOLD SMALL ALPHA..MATHEMATICAL BOLD SMALL OMEGA
unicode_grapheme_base(0x1D6DB, 0x1D6DB).	% Grapheme_Base Sm       MATHEMATICAL BOLD PARTIAL DIFFERENTIAL
unicode_grapheme_base(0x1D6DC, 0x1D6FA).	% Grapheme_Base L&  [31] MATHEMATICAL BOLD EPSILON SYMBOL..MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_grapheme_base(0x1D6FB, 0x1D6FB).	% Grapheme_Base Sm       MATHEMATICAL ITALIC NABLA
unicode_grapheme_base(0x1D6FC, 0x1D714).	% Grapheme_Base L&  [25] MATHEMATICAL ITALIC SMALL ALPHA..MATHEMATICAL ITALIC SMALL OMEGA
unicode_grapheme_base(0x1D715, 0x1D715).	% Grapheme_Base Sm       MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL
unicode_grapheme_base(0x1D716, 0x1D734).	% Grapheme_Base L&  [31] MATHEMATICAL ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_grapheme_base(0x1D735, 0x1D735).	% Grapheme_Base Sm       MATHEMATICAL BOLD ITALIC NABLA
unicode_grapheme_base(0x1D736, 0x1D74E).	% Grapheme_Base L&  [25] MATHEMATICAL BOLD ITALIC SMALL ALPHA..MATHEMATICAL BOLD ITALIC SMALL OMEGA
unicode_grapheme_base(0x1D74F, 0x1D74F).	% Grapheme_Base Sm       MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_grapheme_base(0x1D750, 0x1D76E).	% Grapheme_Base L&  [31] MATHEMATICAL BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_grapheme_base(0x1D76F, 0x1D76F).	% Grapheme_Base Sm       MATHEMATICAL SANS-SERIF BOLD NABLA
unicode_grapheme_base(0x1D770, 0x1D788).	% Grapheme_Base L&  [25] MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
unicode_grapheme_base(0x1D789, 0x1D789).	% Grapheme_Base Sm       MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL
unicode_grapheme_base(0x1D78A, 0x1D7A8).	% Grapheme_Base L&  [31] MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_grapheme_base(0x1D7A9, 0x1D7A9).	% Grapheme_Base Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA
unicode_grapheme_base(0x1D7AA, 0x1D7C2).	% Grapheme_Base L&  [25] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
unicode_grapheme_base(0x1D7C3, 0x1D7C3).	% Grapheme_Base Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_grapheme_base(0x1D7C4, 0x1D7CB).	% Grapheme_Base L&   [8] MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD SMALL DIGAMMA
unicode_grapheme_base(0x1D7CE, 0x1D7FF).	% Grapheme_Base Nd  [50] MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
unicode_grapheme_base(0x1EE00, 0x1EE03).	% Grapheme_Base Lo   [4] ARABIC MATHEMATICAL ALEF..ARABIC MATHEMATICAL DAL
unicode_grapheme_base(0x1EE05, 0x1EE1F).	% Grapheme_Base Lo  [27] ARABIC MATHEMATICAL WAW..ARABIC MATHEMATICAL DOTLESS QAF
unicode_grapheme_base(0x1EE21, 0x1EE22).	% Grapheme_Base Lo   [2] ARABIC MATHEMATICAL INITIAL BEH..ARABIC MATHEMATICAL INITIAL JEEM
unicode_grapheme_base(0x1EE24, 0x1EE24).	% Grapheme_Base Lo       ARABIC MATHEMATICAL INITIAL HEH
unicode_grapheme_base(0x1EE27, 0x1EE27).	% Grapheme_Base Lo       ARABIC MATHEMATICAL INITIAL HAH
unicode_grapheme_base(0x1EE29, 0x1EE32).	% Grapheme_Base Lo  [10] ARABIC MATHEMATICAL INITIAL YEH..ARABIC MATHEMATICAL INITIAL QAF
unicode_grapheme_base(0x1EE34, 0x1EE37).	% Grapheme_Base Lo   [4] ARABIC MATHEMATICAL INITIAL SHEEN..ARABIC MATHEMATICAL INITIAL KHAH
unicode_grapheme_base(0x1EE39, 0x1EE39).	% Grapheme_Base Lo       ARABIC MATHEMATICAL INITIAL DAD
unicode_grapheme_base(0x1EE3B, 0x1EE3B).	% Grapheme_Base Lo       ARABIC MATHEMATICAL INITIAL GHAIN
unicode_grapheme_base(0x1EE42, 0x1EE42).	% Grapheme_Base Lo       ARABIC MATHEMATICAL TAILED JEEM
unicode_grapheme_base(0x1EE47, 0x1EE47).	% Grapheme_Base Lo       ARABIC MATHEMATICAL TAILED HAH
unicode_grapheme_base(0x1EE49, 0x1EE49).	% Grapheme_Base Lo       ARABIC MATHEMATICAL TAILED YEH
unicode_grapheme_base(0x1EE4B, 0x1EE4B).	% Grapheme_Base Lo       ARABIC MATHEMATICAL TAILED LAM
unicode_grapheme_base(0x1EE4D, 0x1EE4F).	% Grapheme_Base Lo   [3] ARABIC MATHEMATICAL TAILED NOON..ARABIC MATHEMATICAL TAILED AIN
unicode_grapheme_base(0x1EE51, 0x1EE52).	% Grapheme_Base Lo   [2] ARABIC MATHEMATICAL TAILED SAD..ARABIC MATHEMATICAL TAILED QAF
unicode_grapheme_base(0x1EE54, 0x1EE54).	% Grapheme_Base Lo       ARABIC MATHEMATICAL TAILED SHEEN
unicode_grapheme_base(0x1EE57, 0x1EE57).	% Grapheme_Base Lo       ARABIC MATHEMATICAL TAILED KHAH
unicode_grapheme_base(0x1EE59, 0x1EE59).	% Grapheme_Base Lo       ARABIC MATHEMATICAL TAILED DAD
unicode_grapheme_base(0x1EE5B, 0x1EE5B).	% Grapheme_Base Lo       ARABIC MATHEMATICAL TAILED GHAIN
unicode_grapheme_base(0x1EE5D, 0x1EE5D).	% Grapheme_Base Lo       ARABIC MATHEMATICAL TAILED DOTLESS NOON
unicode_grapheme_base(0x1EE5F, 0x1EE5F).	% Grapheme_Base Lo       ARABIC MATHEMATICAL TAILED DOTLESS QAF
unicode_grapheme_base(0x1EE61, 0x1EE62).	% Grapheme_Base Lo   [2] ARABIC MATHEMATICAL STRETCHED BEH..ARABIC MATHEMATICAL STRETCHED JEEM
unicode_grapheme_base(0x1EE64, 0x1EE64).	% Grapheme_Base Lo       ARABIC MATHEMATICAL STRETCHED HEH
unicode_grapheme_base(0x1EE67, 0x1EE6A).	% Grapheme_Base Lo   [4] ARABIC MATHEMATICAL STRETCHED HAH..ARABIC MATHEMATICAL STRETCHED KAF
unicode_grapheme_base(0x1EE6C, 0x1EE72).	% Grapheme_Base Lo   [7] ARABIC MATHEMATICAL STRETCHED MEEM..ARABIC MATHEMATICAL STRETCHED QAF
unicode_grapheme_base(0x1EE74, 0x1EE77).	% Grapheme_Base Lo   [4] ARABIC MATHEMATICAL STRETCHED SHEEN..ARABIC MATHEMATICAL STRETCHED KHAH
unicode_grapheme_base(0x1EE79, 0x1EE7C).	% Grapheme_Base Lo   [4] ARABIC MATHEMATICAL STRETCHED DAD..ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
unicode_grapheme_base(0x1EE7E, 0x1EE7E).	% Grapheme_Base Lo       ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
unicode_grapheme_base(0x1EE80, 0x1EE89).	% Grapheme_Base Lo  [10] ARABIC MATHEMATICAL LOOPED ALEF..ARABIC MATHEMATICAL LOOPED YEH
unicode_grapheme_base(0x1EE8B, 0x1EE9B).	% Grapheme_Base Lo  [17] ARABIC MATHEMATICAL LOOPED LAM..ARABIC MATHEMATICAL LOOPED GHAIN
unicode_grapheme_base(0x1EEA1, 0x1EEA3).	% Grapheme_Base Lo   [3] ARABIC MATHEMATICAL DOUBLE-STRUCK BEH..ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
unicode_grapheme_base(0x1EEA5, 0x1EEA9).	% Grapheme_Base Lo   [5] ARABIC MATHEMATICAL DOUBLE-STRUCK WAW..ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
unicode_grapheme_base(0x1EEAB, 0x1EEBB).	% Grapheme_Base Lo  [17] ARABIC MATHEMATICAL DOUBLE-STRUCK LAM..ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN
unicode_grapheme_base(0x1EEF0, 0x1EEF1).	% Grapheme_Base Sm   [2] ARABIC MATHEMATICAL OPERATOR MEEM WITH HAH WITH TATWEEL..ARABIC MATHEMATICAL OPERATOR HAH WITH DAL
unicode_grapheme_base(0x1F000, 0x1F02B).	% Grapheme_Base So  [44] MAHJONG TILE EAST WIND..MAHJONG TILE BACK
unicode_grapheme_base(0x1F030, 0x1F093).	% Grapheme_Base So [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
unicode_grapheme_base(0x1F0A0, 0x1F0AE).	% Grapheme_Base So  [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
unicode_grapheme_base(0x1F0B1, 0x1F0BE).	% Grapheme_Base So  [14] PLAYING CARD ACE OF HEARTS..PLAYING CARD KING OF HEARTS
unicode_grapheme_base(0x1F0C1, 0x1F0CF).	% Grapheme_Base So  [15] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD BLACK JOKER
unicode_grapheme_base(0x1F0D1, 0x1F0DF).	% Grapheme_Base So  [15] PLAYING CARD ACE OF CLUBS..PLAYING CARD WHITE JOKER
unicode_grapheme_base(0x1F100, 0x1F10A).	% Grapheme_Base No  [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
unicode_grapheme_base(0x1F110, 0x1F12E).	% Grapheme_Base So  [31] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED WZ
unicode_grapheme_base(0x1F130, 0x1F16B).	% Grapheme_Base So  [60] SQUARED LATIN CAPITAL LETTER A..RAISED MD SIGN
unicode_grapheme_base(0x1F170, 0x1F19A).	% Grapheme_Base So  [43] NEGATIVE SQUARED LATIN CAPITAL LETTER A..SQUARED VS
unicode_grapheme_base(0x1F1E6, 0x1F202).	% Grapheme_Base So  [29] REGIONAL INDICATOR SYMBOL LETTER A..SQUARED KATAKANA SA
unicode_grapheme_base(0x1F210, 0x1F23A).	% Grapheme_Base So  [43] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-55B6
unicode_grapheme_base(0x1F240, 0x1F248).	% Grapheme_Base So   [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
unicode_grapheme_base(0x1F250, 0x1F251).	% Grapheme_Base So   [2] CIRCLED IDEOGRAPH ADVANTAGE..CIRCLED IDEOGRAPH ACCEPT
unicode_grapheme_base(0x1F300, 0x1F320).	% Grapheme_Base So  [33] CYCLONE..SHOOTING STAR
unicode_grapheme_base(0x1F330, 0x1F335).	% Grapheme_Base So   [6] CHESTNUT..CACTUS
unicode_grapheme_base(0x1F337, 0x1F37C).	% Grapheme_Base So  [70] TULIP..BABY BOTTLE
unicode_grapheme_base(0x1F380, 0x1F393).	% Grapheme_Base So  [20] RIBBON..GRADUATION CAP
unicode_grapheme_base(0x1F3A0, 0x1F3C4).	% Grapheme_Base So  [37] CAROUSEL HORSE..SURFER
unicode_grapheme_base(0x1F3C6, 0x1F3CA).	% Grapheme_Base So   [5] TROPHY..SWIMMER
unicode_grapheme_base(0x1F3E0, 0x1F3F0).	% Grapheme_Base So  [17] HOUSE BUILDING..EUROPEAN CASTLE
unicode_grapheme_base(0x1F400, 0x1F43E).	% Grapheme_Base So  [63] RAT..PAW PRINTS
unicode_grapheme_base(0x1F440, 0x1F440).	% Grapheme_Base So       EYES
unicode_grapheme_base(0x1F442, 0x1F4F7).	% Grapheme_Base So [182] EAR..CAMERA
unicode_grapheme_base(0x1F4F9, 0x1F4FC).	% Grapheme_Base So   [4] VIDEO CAMERA..VIDEOCASSETTE
unicode_grapheme_base(0x1F500, 0x1F53D).	% Grapheme_Base So  [62] TWISTED RIGHTWARDS ARROWS..DOWN-POINTING SMALL RED TRIANGLE
unicode_grapheme_base(0x1F540, 0x1F543).	% Grapheme_Base So   [4] CIRCLED CROSS POMMEE..NOTCHED LEFT SEMICIRCLE WITH THREE DOTS
unicode_grapheme_base(0x1F550, 0x1F567).	% Grapheme_Base So  [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
unicode_grapheme_base(0x1F5FB, 0x1F640).	% Grapheme_Base So  [70] MOUNT FUJI..WEARY CAT FACE
unicode_grapheme_base(0x1F645, 0x1F64F).	% Grapheme_Base So  [11] FACE WITH NO GOOD GESTURE..PERSON WITH FOLDED HANDS
unicode_grapheme_base(0x1F680, 0x1F6C5).	% Grapheme_Base So  [70] ROCKET..LEFT LUGGAGE
unicode_grapheme_base(0x1F700, 0x1F773).	% Grapheme_Base So [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
unicode_grapheme_base(0x20000, 0x2A6D6).	% Grapheme_Base Lo [42711] CJK UNIFIED IDEOGRAPH-20000..CJK UNIFIED IDEOGRAPH-2A6D6
unicode_grapheme_base(0x2A700, 0x2B734).	% Grapheme_Base Lo [4149] CJK UNIFIED IDEOGRAPH-2A700..CJK UNIFIED IDEOGRAPH-2B734
unicode_grapheme_base(0x2B740, 0x2B81D).	% Grapheme_Base Lo [222] CJK UNIFIED IDEOGRAPH-2B740..CJK UNIFIED IDEOGRAPH-2B81D
unicode_grapheme_base(0x2F800, 0x2FA1D).	% Grapheme_Base Lo [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D

% Total code points: 108661
