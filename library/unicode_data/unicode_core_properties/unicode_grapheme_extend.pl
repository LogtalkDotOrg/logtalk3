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

unicode_grapheme_extend(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_grapheme_extend(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_grapheme_extend(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_grapheme_extend(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property: Grapheme_Extend
%  Generated from: Me + Mn + Other_Grapheme_Extend
%  Note: depending on an application's interpretation of Co (private use),
%  they may be either in Grapheme_Base, or in Grapheme_Extend, or in neither.

unicode_grapheme_extend(0x0300, 0x036F).	% Grapheme_Extend Mn [112] COMBINING GRAVE ACCENT..COMBINING LATIN SMALL LETTER X
unicode_grapheme_extend(0x0483, 0x0487).	% Grapheme_Extend Mn   [5] COMBINING CYRILLIC TITLO..COMBINING CYRILLIC POKRYTIE
unicode_grapheme_extend(0x0488, 0x0489).	% Grapheme_Extend Me   [2] COMBINING CYRILLIC HUNDRED THOUSANDS SIGN..COMBINING CYRILLIC MILLIONS SIGN
unicode_grapheme_extend(0x0591, 0x05BD).	% Grapheme_Extend Mn  [45] HEBREW ACCENT ETNAHTA..HEBREW POINT METEG
unicode_grapheme_extend(0x05BF, 0x05BF).	% Grapheme_Extend Mn       HEBREW POINT RAFE
unicode_grapheme_extend(0x05C1, 0x05C2).	% Grapheme_Extend Mn   [2] HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT
unicode_grapheme_extend(0x05C4, 0x05C5).	% Grapheme_Extend Mn   [2] HEBREW MARK UPPER DOT..HEBREW MARK LOWER DOT
unicode_grapheme_extend(0x05C7, 0x05C7).	% Grapheme_Extend Mn       HEBREW POINT QAMATS QATAN
unicode_grapheme_extend(0x0610, 0x061A).	% Grapheme_Extend Mn  [11] ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM..ARABIC SMALL KASRA
unicode_grapheme_extend(0x064B, 0x065F).	% Grapheme_Extend Mn  [21] ARABIC FATHATAN..ARABIC WAVY HAMZA BELOW
unicode_grapheme_extend(0x0670, 0x0670).	% Grapheme_Extend Mn       ARABIC LETTER SUPERSCRIPT ALEF
unicode_grapheme_extend(0x06D6, 0x06DC).	% Grapheme_Extend Mn   [7] ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN
unicode_grapheme_extend(0x06DF, 0x06E4).	% Grapheme_Extend Mn   [6] ARABIC SMALL HIGH ROUNDED ZERO..ARABIC SMALL HIGH MADDA
unicode_grapheme_extend(0x06E7, 0x06E8).	% Grapheme_Extend Mn   [2] ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON
unicode_grapheme_extend(0x06EA, 0x06ED).	% Grapheme_Extend Mn   [4] ARABIC EMPTY CENTRE LOW STOP..ARABIC SMALL LOW MEEM
unicode_grapheme_extend(0x0711, 0x0711).	% Grapheme_Extend Mn       SYRIAC LETTER SUPERSCRIPT ALAPH
unicode_grapheme_extend(0x0730, 0x074A).	% Grapheme_Extend Mn  [27] SYRIAC PTHAHA ABOVE..SYRIAC BARREKH
unicode_grapheme_extend(0x07A6, 0x07B0).	% Grapheme_Extend Mn  [11] THAANA ABAFILI..THAANA SUKUN
unicode_grapheme_extend(0x07EB, 0x07F3).	% Grapheme_Extend Mn   [9] NKO COMBINING SHORT HIGH TONE..NKO COMBINING DOUBLE DOT ABOVE
unicode_grapheme_extend(0x0816, 0x0819).	% Grapheme_Extend Mn   [4] SAMARITAN MARK IN..SAMARITAN MARK DAGESH
unicode_grapheme_extend(0x081B, 0x0823).	% Grapheme_Extend Mn   [9] SAMARITAN MARK EPENTHETIC YUT..SAMARITAN VOWEL SIGN A
unicode_grapheme_extend(0x0825, 0x0827).	% Grapheme_Extend Mn   [3] SAMARITAN VOWEL SIGN SHORT A..SAMARITAN VOWEL SIGN U
unicode_grapheme_extend(0x0829, 0x082D).	% Grapheme_Extend Mn   [5] SAMARITAN VOWEL SIGN LONG I..SAMARITAN MARK NEQUDAA
unicode_grapheme_extend(0x0859, 0x085B).	% Grapheme_Extend Mn   [3] MANDAIC AFFRICATION MARK..MANDAIC GEMINATION MARK
unicode_grapheme_extend(0x08E4, 0x08FE).	% Grapheme_Extend Mn  [27] ARABIC CURLY FATHA..ARABIC DAMMA WITH DOT
unicode_grapheme_extend(0x0900, 0x0902).	% Grapheme_Extend Mn   [3] DEVANAGARI SIGN INVERTED CANDRABINDU..DEVANAGARI SIGN ANUSVARA
unicode_grapheme_extend(0x093A, 0x093A).	% Grapheme_Extend Mn       DEVANAGARI VOWEL SIGN OE
unicode_grapheme_extend(0x093C, 0x093C).	% Grapheme_Extend Mn       DEVANAGARI SIGN NUKTA
unicode_grapheme_extend(0x0941, 0x0948).	% Grapheme_Extend Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
unicode_grapheme_extend(0x094D, 0x094D).	% Grapheme_Extend Mn       DEVANAGARI SIGN VIRAMA
unicode_grapheme_extend(0x0951, 0x0957).	% Grapheme_Extend Mn   [7] DEVANAGARI STRESS SIGN UDATTA..DEVANAGARI VOWEL SIGN UUE
unicode_grapheme_extend(0x0962, 0x0963).	% Grapheme_Extend Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
unicode_grapheme_extend(0x0981, 0x0981).	% Grapheme_Extend Mn       BENGALI SIGN CANDRABINDU
unicode_grapheme_extend(0x09BC, 0x09BC).	% Grapheme_Extend Mn       BENGALI SIGN NUKTA
unicode_grapheme_extend(0x09BE, 0x09BE).	% Grapheme_Extend Mc       BENGALI VOWEL SIGN AA
unicode_grapheme_extend(0x09C1, 0x09C4).	% Grapheme_Extend Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
unicode_grapheme_extend(0x09CD, 0x09CD).	% Grapheme_Extend Mn       BENGALI SIGN VIRAMA
unicode_grapheme_extend(0x09D7, 0x09D7).	% Grapheme_Extend Mc       BENGALI AU LENGTH MARK
unicode_grapheme_extend(0x09E2, 0x09E3).	% Grapheme_Extend Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
unicode_grapheme_extend(0x0A01, 0x0A02).	% Grapheme_Extend Mn   [2] GURMUKHI SIGN ADAK BINDI..GURMUKHI SIGN BINDI
unicode_grapheme_extend(0x0A3C, 0x0A3C).	% Grapheme_Extend Mn       GURMUKHI SIGN NUKTA
unicode_grapheme_extend(0x0A41, 0x0A42).	% Grapheme_Extend Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
unicode_grapheme_extend(0x0A47, 0x0A48).	% Grapheme_Extend Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
unicode_grapheme_extend(0x0A4B, 0x0A4D).	% Grapheme_Extend Mn   [3] GURMUKHI VOWEL SIGN OO..GURMUKHI SIGN VIRAMA
unicode_grapheme_extend(0x0A51, 0x0A51).	% Grapheme_Extend Mn       GURMUKHI SIGN UDAAT
unicode_grapheme_extend(0x0A70, 0x0A71).	% Grapheme_Extend Mn   [2] GURMUKHI TIPPI..GURMUKHI ADDAK
unicode_grapheme_extend(0x0A75, 0x0A75).	% Grapheme_Extend Mn       GURMUKHI SIGN YAKASH
unicode_grapheme_extend(0x0A81, 0x0A82).	% Grapheme_Extend Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
unicode_grapheme_extend(0x0ABC, 0x0ABC).	% Grapheme_Extend Mn       GUJARATI SIGN NUKTA
unicode_grapheme_extend(0x0AC1, 0x0AC5).	% Grapheme_Extend Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
unicode_grapheme_extend(0x0AC7, 0x0AC8).	% Grapheme_Extend Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
unicode_grapheme_extend(0x0ACD, 0x0ACD).	% Grapheme_Extend Mn       GUJARATI SIGN VIRAMA
unicode_grapheme_extend(0x0AE2, 0x0AE3).	% Grapheme_Extend Mn   [2] GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL
unicode_grapheme_extend(0x0B01, 0x0B01).	% Grapheme_Extend Mn       ORIYA SIGN CANDRABINDU
unicode_grapheme_extend(0x0B3C, 0x0B3C).	% Grapheme_Extend Mn       ORIYA SIGN NUKTA
unicode_grapheme_extend(0x0B3E, 0x0B3E).	% Grapheme_Extend Mc       ORIYA VOWEL SIGN AA
unicode_grapheme_extend(0x0B3F, 0x0B3F).	% Grapheme_Extend Mn       ORIYA VOWEL SIGN I
unicode_grapheme_extend(0x0B41, 0x0B44).	% Grapheme_Extend Mn   [4] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR
unicode_grapheme_extend(0x0B4D, 0x0B4D).	% Grapheme_Extend Mn       ORIYA SIGN VIRAMA
unicode_grapheme_extend(0x0B56, 0x0B56).	% Grapheme_Extend Mn       ORIYA AI LENGTH MARK
unicode_grapheme_extend(0x0B57, 0x0B57).	% Grapheme_Extend Mc       ORIYA AU LENGTH MARK
unicode_grapheme_extend(0x0B62, 0x0B63).	% Grapheme_Extend Mn   [2] ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL
unicode_grapheme_extend(0x0B82, 0x0B82).	% Grapheme_Extend Mn       TAMIL SIGN ANUSVARA
unicode_grapheme_extend(0x0BBE, 0x0BBE).	% Grapheme_Extend Mc       TAMIL VOWEL SIGN AA
unicode_grapheme_extend(0x0BC0, 0x0BC0).	% Grapheme_Extend Mn       TAMIL VOWEL SIGN II
unicode_grapheme_extend(0x0BCD, 0x0BCD).	% Grapheme_Extend Mn       TAMIL SIGN VIRAMA
unicode_grapheme_extend(0x0BD7, 0x0BD7).	% Grapheme_Extend Mc       TAMIL AU LENGTH MARK
unicode_grapheme_extend(0x0C3E, 0x0C40).	% Grapheme_Extend Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
unicode_grapheme_extend(0x0C46, 0x0C48).	% Grapheme_Extend Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
unicode_grapheme_extend(0x0C4A, 0x0C4D).	% Grapheme_Extend Mn   [4] TELUGU VOWEL SIGN O..TELUGU SIGN VIRAMA
unicode_grapheme_extend(0x0C55, 0x0C56).	% Grapheme_Extend Mn   [2] TELUGU LENGTH MARK..TELUGU AI LENGTH MARK
unicode_grapheme_extend(0x0C62, 0x0C63).	% Grapheme_Extend Mn   [2] TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL
unicode_grapheme_extend(0x0CBC, 0x0CBC).	% Grapheme_Extend Mn       KANNADA SIGN NUKTA
unicode_grapheme_extend(0x0CBF, 0x0CBF).	% Grapheme_Extend Mn       KANNADA VOWEL SIGN I
unicode_grapheme_extend(0x0CC2, 0x0CC2).	% Grapheme_Extend Mc       KANNADA VOWEL SIGN UU
unicode_grapheme_extend(0x0CC6, 0x0CC6).	% Grapheme_Extend Mn       KANNADA VOWEL SIGN E
unicode_grapheme_extend(0x0CCC, 0x0CCD).	% Grapheme_Extend Mn   [2] KANNADA VOWEL SIGN AU..KANNADA SIGN VIRAMA
unicode_grapheme_extend(0x0CD5, 0x0CD6).	% Grapheme_Extend Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
unicode_grapheme_extend(0x0CE2, 0x0CE3).	% Grapheme_Extend Mn   [2] KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL
unicode_grapheme_extend(0x0D3E, 0x0D3E).	% Grapheme_Extend Mc       MALAYALAM VOWEL SIGN AA
unicode_grapheme_extend(0x0D41, 0x0D44).	% Grapheme_Extend Mn   [4] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC RR
unicode_grapheme_extend(0x0D4D, 0x0D4D).	% Grapheme_Extend Mn       MALAYALAM SIGN VIRAMA
unicode_grapheme_extend(0x0D57, 0x0D57).	% Grapheme_Extend Mc       MALAYALAM AU LENGTH MARK
unicode_grapheme_extend(0x0D62, 0x0D63).	% Grapheme_Extend Mn   [2] MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL
unicode_grapheme_extend(0x0DCA, 0x0DCA).	% Grapheme_Extend Mn       SINHALA SIGN AL-LAKUNA
unicode_grapheme_extend(0x0DCF, 0x0DCF).	% Grapheme_Extend Mc       SINHALA VOWEL SIGN AELA-PILLA
unicode_grapheme_extend(0x0DD2, 0x0DD4).	% Grapheme_Extend Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
unicode_grapheme_extend(0x0DD6, 0x0DD6).	% Grapheme_Extend Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
unicode_grapheme_extend(0x0DDF, 0x0DDF).	% Grapheme_Extend Mc       SINHALA VOWEL SIGN GAYANUKITTA
unicode_grapheme_extend(0x0E31, 0x0E31).	% Grapheme_Extend Mn       THAI CHARACTER MAI HAN-AKAT
unicode_grapheme_extend(0x0E34, 0x0E3A).	% Grapheme_Extend Mn   [7] THAI CHARACTER SARA I..THAI CHARACTER PHINTHU
unicode_grapheme_extend(0x0E47, 0x0E4E).	% Grapheme_Extend Mn   [8] THAI CHARACTER MAITAIKHU..THAI CHARACTER YAMAKKAN
unicode_grapheme_extend(0x0EB1, 0x0EB1).	% Grapheme_Extend Mn       LAO VOWEL SIGN MAI KAN
unicode_grapheme_extend(0x0EB4, 0x0EB9).	% Grapheme_Extend Mn   [6] LAO VOWEL SIGN I..LAO VOWEL SIGN UU
unicode_grapheme_extend(0x0EBB, 0x0EBC).	% Grapheme_Extend Mn   [2] LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO
unicode_grapheme_extend(0x0EC8, 0x0ECD).	% Grapheme_Extend Mn   [6] LAO TONE MAI EK..LAO NIGGAHITA
unicode_grapheme_extend(0x0F18, 0x0F19).	% Grapheme_Extend Mn   [2] TIBETAN ASTROLOGICAL SIGN -KHYUD PA..TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS
unicode_grapheme_extend(0x0F35, 0x0F35).	% Grapheme_Extend Mn       TIBETAN MARK NGAS BZUNG NYI ZLA
unicode_grapheme_extend(0x0F37, 0x0F37).	% Grapheme_Extend Mn       TIBETAN MARK NGAS BZUNG SGOR RTAGS
unicode_grapheme_extend(0x0F39, 0x0F39).	% Grapheme_Extend Mn       TIBETAN MARK TSA -PHRU
unicode_grapheme_extend(0x0F71, 0x0F7E).	% Grapheme_Extend Mn  [14] TIBETAN VOWEL SIGN AA..TIBETAN SIGN RJES SU NGA RO
unicode_grapheme_extend(0x0F80, 0x0F84).	% Grapheme_Extend Mn   [5] TIBETAN VOWEL SIGN REVERSED I..TIBETAN MARK HALANTA
unicode_grapheme_extend(0x0F86, 0x0F87).	% Grapheme_Extend Mn   [2] TIBETAN SIGN LCI RTAGS..TIBETAN SIGN YANG RTAGS
unicode_grapheme_extend(0x0F8D, 0x0F97).	% Grapheme_Extend Mn  [11] TIBETAN SUBJOINED SIGN LCE TSA CAN..TIBETAN SUBJOINED LETTER JA
unicode_grapheme_extend(0x0F99, 0x0FBC).	% Grapheme_Extend Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
unicode_grapheme_extend(0x0FC6, 0x0FC6).	% Grapheme_Extend Mn       TIBETAN SYMBOL PADMA GDAN
unicode_grapheme_extend(0x102D, 0x1030).	% Grapheme_Extend Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
unicode_grapheme_extend(0x1032, 0x1037).	% Grapheme_Extend Mn   [6] MYANMAR VOWEL SIGN AI..MYANMAR SIGN DOT BELOW
unicode_grapheme_extend(0x1039, 0x103A).	% Grapheme_Extend Mn   [2] MYANMAR SIGN VIRAMA..MYANMAR SIGN ASAT
unicode_grapheme_extend(0x103D, 0x103E).	% Grapheme_Extend Mn   [2] MYANMAR CONSONANT SIGN MEDIAL WA..MYANMAR CONSONANT SIGN MEDIAL HA
unicode_grapheme_extend(0x1058, 0x1059).	% Grapheme_Extend Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
unicode_grapheme_extend(0x105E, 0x1060).	% Grapheme_Extend Mn   [3] MYANMAR CONSONANT SIGN MON MEDIAL NA..MYANMAR CONSONANT SIGN MON MEDIAL LA
unicode_grapheme_extend(0x1071, 0x1074).	% Grapheme_Extend Mn   [4] MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE
unicode_grapheme_extend(0x1082, 0x1082).	% Grapheme_Extend Mn       MYANMAR CONSONANT SIGN SHAN MEDIAL WA
unicode_grapheme_extend(0x1085, 0x1086).	% Grapheme_Extend Mn   [2] MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y
unicode_grapheme_extend(0x108D, 0x108D).	% Grapheme_Extend Mn       MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE
unicode_grapheme_extend(0x109D, 0x109D).	% Grapheme_Extend Mn       MYANMAR VOWEL SIGN AITON AI
unicode_grapheme_extend(0x135D, 0x135F).	% Grapheme_Extend Mn   [3] ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK..ETHIOPIC COMBINING GEMINATION MARK
unicode_grapheme_extend(0x1712, 0x1714).	% Grapheme_Extend Mn   [3] TAGALOG VOWEL SIGN I..TAGALOG SIGN VIRAMA
unicode_grapheme_extend(0x1732, 0x1734).	% Grapheme_Extend Mn   [3] HANUNOO VOWEL SIGN I..HANUNOO SIGN PAMUDPOD
unicode_grapheme_extend(0x1752, 0x1753).	% Grapheme_Extend Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
unicode_grapheme_extend(0x1772, 0x1773).	% Grapheme_Extend Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
unicode_grapheme_extend(0x17B4, 0x17B5).	% Grapheme_Extend Mn   [2] KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA
unicode_grapheme_extend(0x17B7, 0x17BD).	% Grapheme_Extend Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
unicode_grapheme_extend(0x17C6, 0x17C6).	% Grapheme_Extend Mn       KHMER SIGN NIKAHIT
unicode_grapheme_extend(0x17C9, 0x17D3).	% Grapheme_Extend Mn  [11] KHMER SIGN MUUSIKATOAN..KHMER SIGN BATHAMASAT
unicode_grapheme_extend(0x17DD, 0x17DD).	% Grapheme_Extend Mn       KHMER SIGN ATTHACAN
unicode_grapheme_extend(0x180B, 0x180D).	% Grapheme_Extend Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
unicode_grapheme_extend(0x18A9, 0x18A9).	% Grapheme_Extend Mn       MONGOLIAN LETTER ALI GALI DAGALGA
unicode_grapheme_extend(0x1920, 0x1922).	% Grapheme_Extend Mn   [3] LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN U
unicode_grapheme_extend(0x1927, 0x1928).	% Grapheme_Extend Mn   [2] LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O
unicode_grapheme_extend(0x1932, 0x1932).	% Grapheme_Extend Mn       LIMBU SMALL LETTER ANUSVARA
unicode_grapheme_extend(0x1939, 0x193B).	% Grapheme_Extend Mn   [3] LIMBU SIGN MUKPHRENG..LIMBU SIGN SA-I
unicode_grapheme_extend(0x1A17, 0x1A18).	% Grapheme_Extend Mn   [2] BUGINESE VOWEL SIGN I..BUGINESE VOWEL SIGN U
unicode_grapheme_extend(0x1A56, 0x1A56).	% Grapheme_Extend Mn       TAI THAM CONSONANT SIGN MEDIAL LA
unicode_grapheme_extend(0x1A58, 0x1A5E).	% Grapheme_Extend Mn   [7] TAI THAM SIGN MAI KANG LAI..TAI THAM CONSONANT SIGN SA
unicode_grapheme_extend(0x1A60, 0x1A60).	% Grapheme_Extend Mn       TAI THAM SIGN SAKOT
unicode_grapheme_extend(0x1A62, 0x1A62).	% Grapheme_Extend Mn       TAI THAM VOWEL SIGN MAI SAT
unicode_grapheme_extend(0x1A65, 0x1A6C).	% Grapheme_Extend Mn   [8] TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN OA BELOW
unicode_grapheme_extend(0x1A73, 0x1A7C).	% Grapheme_Extend Mn  [10] TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN KHUEN-LUE KARAN
unicode_grapheme_extend(0x1A7F, 0x1A7F).	% Grapheme_Extend Mn       TAI THAM COMBINING CRYPTOGRAMMIC DOT
unicode_grapheme_extend(0x1B00, 0x1B03).	% Grapheme_Extend Mn   [4] BALINESE SIGN ULU RICEM..BALINESE SIGN SURANG
unicode_grapheme_extend(0x1B34, 0x1B34).	% Grapheme_Extend Mn       BALINESE SIGN REREKAN
unicode_grapheme_extend(0x1B36, 0x1B3A).	% Grapheme_Extend Mn   [5] BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN RA REPA
unicode_grapheme_extend(0x1B3C, 0x1B3C).	% Grapheme_Extend Mn       BALINESE VOWEL SIGN LA LENGA
unicode_grapheme_extend(0x1B42, 0x1B42).	% Grapheme_Extend Mn       BALINESE VOWEL SIGN PEPET
unicode_grapheme_extend(0x1B6B, 0x1B73).	% Grapheme_Extend Mn   [9] BALINESE MUSICAL SYMBOL COMBINING TEGEH..BALINESE MUSICAL SYMBOL COMBINING GONG
unicode_grapheme_extend(0x1B80, 0x1B81).	% Grapheme_Extend Mn   [2] SUNDANESE SIGN PANYECEK..SUNDANESE SIGN PANGLAYAR
unicode_grapheme_extend(0x1BA2, 0x1BA5).	% Grapheme_Extend Mn   [4] SUNDANESE CONSONANT SIGN PANYAKRA..SUNDANESE VOWEL SIGN PANYUKU
unicode_grapheme_extend(0x1BA8, 0x1BA9).	% Grapheme_Extend Mn   [2] SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG
unicode_grapheme_extend(0x1BAB, 0x1BAB).	% Grapheme_Extend Mn       SUNDANESE SIGN VIRAMA
unicode_grapheme_extend(0x1BE6, 0x1BE6).	% Grapheme_Extend Mn       BATAK SIGN TOMPI
unicode_grapheme_extend(0x1BE8, 0x1BE9).	% Grapheme_Extend Mn   [2] BATAK VOWEL SIGN PAKPAK E..BATAK VOWEL SIGN EE
unicode_grapheme_extend(0x1BED, 0x1BED).	% Grapheme_Extend Mn       BATAK VOWEL SIGN KARO O
unicode_grapheme_extend(0x1BEF, 0x1BF1).	% Grapheme_Extend Mn   [3] BATAK VOWEL SIGN U FOR SIMALUNGUN SA..BATAK CONSONANT SIGN H
unicode_grapheme_extend(0x1C2C, 0x1C33).	% Grapheme_Extend Mn   [8] LEPCHA VOWEL SIGN E..LEPCHA CONSONANT SIGN T
unicode_grapheme_extend(0x1C36, 0x1C37).	% Grapheme_Extend Mn   [2] LEPCHA SIGN RAN..LEPCHA SIGN NUKTA
unicode_grapheme_extend(0x1CD0, 0x1CD2).	% Grapheme_Extend Mn   [3] VEDIC TONE KARSHANA..VEDIC TONE PRENKHA
unicode_grapheme_extend(0x1CD4, 0x1CE0).	% Grapheme_Extend Mn  [13] VEDIC SIGN YAJURVEDIC MIDLINE SVARITA..VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA
unicode_grapheme_extend(0x1CE2, 0x1CE8).	% Grapheme_Extend Mn   [7] VEDIC SIGN VISARGA SVARITA..VEDIC SIGN VISARGA ANUDATTA WITH TAIL
unicode_grapheme_extend(0x1CED, 0x1CED).	% Grapheme_Extend Mn       VEDIC SIGN TIRYAK
unicode_grapheme_extend(0x1CF4, 0x1CF4).	% Grapheme_Extend Mn       VEDIC TONE CANDRA ABOVE
unicode_grapheme_extend(0x1DC0, 0x1DE6).	% Grapheme_Extend Mn  [39] COMBINING DOTTED GRAVE ACCENT..COMBINING LATIN SMALL LETTER Z
unicode_grapheme_extend(0x1DFC, 0x1DFF).	% Grapheme_Extend Mn   [4] COMBINING DOUBLE INVERTED BREVE BELOW..COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW
unicode_grapheme_extend(0x200C, 0x200D).	% Grapheme_Extend Cf   [2] ZERO WIDTH NON-JOINER..ZERO WIDTH JOINER
unicode_grapheme_extend(0x20D0, 0x20DC).	% Grapheme_Extend Mn  [13] COMBINING LEFT HARPOON ABOVE..COMBINING FOUR DOTS ABOVE
unicode_grapheme_extend(0x20DD, 0x20E0).	% Grapheme_Extend Me   [4] COMBINING ENCLOSING CIRCLE..COMBINING ENCLOSING CIRCLE BACKSLASH
unicode_grapheme_extend(0x20E1, 0x20E1).	% Grapheme_Extend Mn       COMBINING LEFT RIGHT ARROW ABOVE
unicode_grapheme_extend(0x20E2, 0x20E4).	% Grapheme_Extend Me   [3] COMBINING ENCLOSING SCREEN..COMBINING ENCLOSING UPWARD POINTING TRIANGLE
unicode_grapheme_extend(0x20E5, 0x20F0).	% Grapheme_Extend Mn  [12] COMBINING REVERSE SOLIDUS OVERLAY..COMBINING ASTERISK ABOVE
unicode_grapheme_extend(0x2CEF, 0x2CF1).	% Grapheme_Extend Mn   [3] COPTIC COMBINING NI ABOVE..COPTIC COMBINING SPIRITUS LENIS
unicode_grapheme_extend(0x2D7F, 0x2D7F).	% Grapheme_Extend Mn       TIFINAGH CONSONANT JOINER
unicode_grapheme_extend(0x2DE0, 0x2DFF).	% Grapheme_Extend Mn  [32] COMBINING CYRILLIC LETTER BE..COMBINING CYRILLIC LETTER IOTIFIED BIG YUS
unicode_grapheme_extend(0x302A, 0x302D).	% Grapheme_Extend Mn   [4] IDEOGRAPHIC LEVEL TONE MARK..IDEOGRAPHIC ENTERING TONE MARK
unicode_grapheme_extend(0x302E, 0x302F).	% Grapheme_Extend Mc   [2] HANGUL SINGLE DOT TONE MARK..HANGUL DOUBLE DOT TONE MARK
unicode_grapheme_extend(0x3099, 0x309A).	% Grapheme_Extend Mn   [2] COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK..COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_grapheme_extend(0xA66F, 0xA66F).	% Grapheme_Extend Mn       COMBINING CYRILLIC VZMET
unicode_grapheme_extend(0xA670, 0xA672).	% Grapheme_Extend Me   [3] COMBINING CYRILLIC TEN MILLIONS SIGN..COMBINING CYRILLIC THOUSAND MILLIONS SIGN
unicode_grapheme_extend(0xA674, 0xA67D).	% Grapheme_Extend Mn  [10] COMBINING CYRILLIC LETTER UKRAINIAN IE..COMBINING CYRILLIC PAYEROK
unicode_grapheme_extend(0xA69F, 0xA69F).	% Grapheme_Extend Mn       COMBINING CYRILLIC LETTER IOTIFIED E
unicode_grapheme_extend(0xA6F0, 0xA6F1).	% Grapheme_Extend Mn   [2] BAMUM COMBINING MARK KOQNDON..BAMUM COMBINING MARK TUKWENTIS
unicode_grapheme_extend(0xA802, 0xA802).	% Grapheme_Extend Mn       SYLOTI NAGRI SIGN DVISVARA
unicode_grapheme_extend(0xA806, 0xA806).	% Grapheme_Extend Mn       SYLOTI NAGRI SIGN HASANTA
unicode_grapheme_extend(0xA80B, 0xA80B).	% Grapheme_Extend Mn       SYLOTI NAGRI SIGN ANUSVARA
unicode_grapheme_extend(0xA825, 0xA826).	% Grapheme_Extend Mn   [2] SYLOTI NAGRI VOWEL SIGN U..SYLOTI NAGRI VOWEL SIGN E
unicode_grapheme_extend(0xA8C4, 0xA8C4).	% Grapheme_Extend Mn       SAURASHTRA SIGN VIRAMA
unicode_grapheme_extend(0xA8E0, 0xA8F1).	% Grapheme_Extend Mn  [18] COMBINING DEVANAGARI DIGIT ZERO..COMBINING DEVANAGARI SIGN AVAGRAHA
unicode_grapheme_extend(0xA926, 0xA92D).	% Grapheme_Extend Mn   [8] KAYAH LI VOWEL UE..KAYAH LI TONE CALYA PLOPHU
unicode_grapheme_extend(0xA947, 0xA951).	% Grapheme_Extend Mn  [11] REJANG VOWEL SIGN I..REJANG CONSONANT SIGN R
unicode_grapheme_extend(0xA980, 0xA982).	% Grapheme_Extend Mn   [3] JAVANESE SIGN PANYANGGA..JAVANESE SIGN LAYAR
unicode_grapheme_extend(0xA9B3, 0xA9B3).	% Grapheme_Extend Mn       JAVANESE SIGN CECAK TELU
unicode_grapheme_extend(0xA9B6, 0xA9B9).	% Grapheme_Extend Mn   [4] JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN SUKU MENDUT
unicode_grapheme_extend(0xA9BC, 0xA9BC).	% Grapheme_Extend Mn       JAVANESE VOWEL SIGN PEPET
unicode_grapheme_extend(0xAA29, 0xAA2E).	% Grapheme_Extend Mn   [6] CHAM VOWEL SIGN AA..CHAM VOWEL SIGN OE
unicode_grapheme_extend(0xAA31, 0xAA32).	% Grapheme_Extend Mn   [2] CHAM VOWEL SIGN AU..CHAM VOWEL SIGN UE
unicode_grapheme_extend(0xAA35, 0xAA36).	% Grapheme_Extend Mn   [2] CHAM CONSONANT SIGN LA..CHAM CONSONANT SIGN WA
unicode_grapheme_extend(0xAA43, 0xAA43).	% Grapheme_Extend Mn       CHAM CONSONANT SIGN FINAL NG
unicode_grapheme_extend(0xAA4C, 0xAA4C).	% Grapheme_Extend Mn       CHAM CONSONANT SIGN FINAL M
unicode_grapheme_extend(0xAAB0, 0xAAB0).	% Grapheme_Extend Mn       TAI VIET MAI KANG
unicode_grapheme_extend(0xAAB2, 0xAAB4).	% Grapheme_Extend Mn   [3] TAI VIET VOWEL I..TAI VIET VOWEL U
unicode_grapheme_extend(0xAAB7, 0xAAB8).	% Grapheme_Extend Mn   [2] TAI VIET MAI KHIT..TAI VIET VOWEL IA
unicode_grapheme_extend(0xAABE, 0xAABF).	% Grapheme_Extend Mn   [2] TAI VIET VOWEL AM..TAI VIET TONE MAI EK
unicode_grapheme_extend(0xAAC1, 0xAAC1).	% Grapheme_Extend Mn       TAI VIET TONE MAI THO
unicode_grapheme_extend(0xAAEC, 0xAAED).	% Grapheme_Extend Mn   [2] MEETEI MAYEK VOWEL SIGN UU..MEETEI MAYEK VOWEL SIGN AAI
unicode_grapheme_extend(0xAAF6, 0xAAF6).	% Grapheme_Extend Mn       MEETEI MAYEK VIRAMA
unicode_grapheme_extend(0xABE5, 0xABE5).	% Grapheme_Extend Mn       MEETEI MAYEK VOWEL SIGN ANAP
unicode_grapheme_extend(0xABE8, 0xABE8).	% Grapheme_Extend Mn       MEETEI MAYEK VOWEL SIGN UNAP
unicode_grapheme_extend(0xABED, 0xABED).	% Grapheme_Extend Mn       MEETEI MAYEK APUN IYEK
unicode_grapheme_extend(0xFB1E, 0xFB1E).	% Grapheme_Extend Mn       HEBREW POINT JUDEO-SPANISH VARIKA
unicode_grapheme_extend(0xFE00, 0xFE0F).	% Grapheme_Extend Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
unicode_grapheme_extend(0xFE20, 0xFE26).	% Grapheme_Extend Mn   [7] COMBINING LIGATURE LEFT HALF..COMBINING CONJOINING MACRON
unicode_grapheme_extend(0xFF9E, 0xFF9F).	% Grapheme_Extend Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
unicode_grapheme_extend(0x101FD, 0x101FD).	% Grapheme_Extend Mn       PHAISTOS DISC SIGN COMBINING OBLIQUE STROKE
unicode_grapheme_extend(0x10A01, 0x10A03).	% Grapheme_Extend Mn   [3] KHAROSHTHI VOWEL SIGN I..KHAROSHTHI VOWEL SIGN VOCALIC R
unicode_grapheme_extend(0x10A05, 0x10A06).	% Grapheme_Extend Mn   [2] KHAROSHTHI VOWEL SIGN E..KHAROSHTHI VOWEL SIGN O
unicode_grapheme_extend(0x10A0C, 0x10A0F).	% Grapheme_Extend Mn   [4] KHAROSHTHI VOWEL LENGTH MARK..KHAROSHTHI SIGN VISARGA
unicode_grapheme_extend(0x10A38, 0x10A3A).	% Grapheme_Extend Mn   [3] KHAROSHTHI SIGN BAR ABOVE..KHAROSHTHI SIGN DOT BELOW
unicode_grapheme_extend(0x10A3F, 0x10A3F).	% Grapheme_Extend Mn       KHAROSHTHI VIRAMA
unicode_grapheme_extend(0x11001, 0x11001).	% Grapheme_Extend Mn       BRAHMI SIGN ANUSVARA
unicode_grapheme_extend(0x11038, 0x11046).	% Grapheme_Extend Mn  [15] BRAHMI VOWEL SIGN AA..BRAHMI VIRAMA
unicode_grapheme_extend(0x11080, 0x11081).	% Grapheme_Extend Mn   [2] KAITHI SIGN CANDRABINDU..KAITHI SIGN ANUSVARA
unicode_grapheme_extend(0x110B3, 0x110B6).	% Grapheme_Extend Mn   [4] KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN AI
unicode_grapheme_extend(0x110B9, 0x110BA).	% Grapheme_Extend Mn   [2] KAITHI SIGN VIRAMA..KAITHI SIGN NUKTA
unicode_grapheme_extend(0x11100, 0x11102).	% Grapheme_Extend Mn   [3] CHAKMA SIGN CANDRABINDU..CHAKMA SIGN VISARGA
unicode_grapheme_extend(0x11127, 0x1112B).	% Grapheme_Extend Mn   [5] CHAKMA VOWEL SIGN A..CHAKMA VOWEL SIGN UU
unicode_grapheme_extend(0x1112D, 0x11134).	% Grapheme_Extend Mn   [8] CHAKMA VOWEL SIGN AI..CHAKMA MAAYYAA
unicode_grapheme_extend(0x11180, 0x11181).	% Grapheme_Extend Mn   [2] SHARADA SIGN CANDRABINDU..SHARADA SIGN ANUSVARA
unicode_grapheme_extend(0x111B6, 0x111BE).	% Grapheme_Extend Mn   [9] SHARADA VOWEL SIGN U..SHARADA VOWEL SIGN O
unicode_grapheme_extend(0x116AB, 0x116AB).	% Grapheme_Extend Mn       TAKRI SIGN ANUSVARA
unicode_grapheme_extend(0x116AD, 0x116AD).	% Grapheme_Extend Mn       TAKRI VOWEL SIGN AA
unicode_grapheme_extend(0x116B0, 0x116B5).	% Grapheme_Extend Mn   [6] TAKRI VOWEL SIGN U..TAKRI VOWEL SIGN AU
unicode_grapheme_extend(0x116B7, 0x116B7).	% Grapheme_Extend Mn       TAKRI SIGN NUKTA
unicode_grapheme_extend(0x16F8F, 0x16F92).	% Grapheme_Extend Mn   [4] MIAO TONE RIGHT..MIAO TONE BELOW
unicode_grapheme_extend(0x1D165, 0x1D165).	% Grapheme_Extend Mc       MUSICAL SYMBOL COMBINING STEM
unicode_grapheme_extend(0x1D167, 0x1D169).	% Grapheme_Extend Mn   [3] MUSICAL SYMBOL COMBINING TREMOLO-1..MUSICAL SYMBOL COMBINING TREMOLO-3
unicode_grapheme_extend(0x1D16E, 0x1D172).	% Grapheme_Extend Mc   [5] MUSICAL SYMBOL COMBINING FLAG-1..MUSICAL SYMBOL COMBINING FLAG-5
unicode_grapheme_extend(0x1D17B, 0x1D182).	% Grapheme_Extend Mn   [8] MUSICAL SYMBOL COMBINING ACCENT..MUSICAL SYMBOL COMBINING LOURE
unicode_grapheme_extend(0x1D185, 0x1D18B).	% Grapheme_Extend Mn   [7] MUSICAL SYMBOL COMBINING DOIT..MUSICAL SYMBOL COMBINING TRIPLE TONGUE
unicode_grapheme_extend(0x1D1AA, 0x1D1AD).	% Grapheme_Extend Mn   [4] MUSICAL SYMBOL COMBINING DOWN BOW..MUSICAL SYMBOL COMBINING SNAP PIZZICATO
unicode_grapheme_extend(0x1D242, 0x1D244).	% Grapheme_Extend Mn   [3] COMBINING GREEK MUSICAL TRISEME..COMBINING GREEK MUSICAL PENTASEME
unicode_grapheme_extend(0xE0100, 0xE01EF).	% Grapheme_Extend Mn [240] VARIATION SELECTOR-17..VARIATION SELECTOR-256

% Total code points: 1317
