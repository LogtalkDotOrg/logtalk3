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

unicode_case_ignorable(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_case_ignorable(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_case_ignorable(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_case_ignorable(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property:   Case_Ignorable (CI)
%  As defined by Unicode Standard Definition D121
%  C is defined to be case-ignorable if
%    Word_Break(C) = MidLetter or MidNumLet, or
%    General_Category(C) = Nonspacing_Mark (Mn), Enclosing_Mark (Me), Format (Cf), Modifier_Letter (Lm), or Modifier_Symbol (Sk).

unicode_case_ignorable(0x0027, 0x0027).	% Case_Ignorable Po       APOSTROPHE
unicode_case_ignorable(0x002E, 0x002E).	% Case_Ignorable Po       FULL STOP
unicode_case_ignorable(0x003A, 0x003A).	% Case_Ignorable Po       COLON
unicode_case_ignorable(0x005E, 0x005E).	% Case_Ignorable Sk       CIRCUMFLEX ACCENT
unicode_case_ignorable(0x0060, 0x0060).	% Case_Ignorable Sk       GRAVE ACCENT
unicode_case_ignorable(0x00A8, 0x00A8).	% Case_Ignorable Sk       DIAERESIS
unicode_case_ignorable(0x00AD, 0x00AD).	% Case_Ignorable Cf       SOFT HYPHEN
unicode_case_ignorable(0x00AF, 0x00AF).	% Case_Ignorable Sk       MACRON
unicode_case_ignorable(0x00B4, 0x00B4).	% Case_Ignorable Sk       ACUTE ACCENT
unicode_case_ignorable(0x00B7, 0x00B7).	% Case_Ignorable Po       MIDDLE DOT
unicode_case_ignorable(0x00B8, 0x00B8).	% Case_Ignorable Sk       CEDILLA
unicode_case_ignorable(0x02B0, 0x02C1).	% Case_Ignorable Lm  [18] MODIFIER LETTER SMALL H..MODIFIER LETTER REVERSED GLOTTAL STOP
unicode_case_ignorable(0x02C2, 0x02C5).	% Case_Ignorable Sk   [4] MODIFIER LETTER LEFT ARROWHEAD..MODIFIER LETTER DOWN ARROWHEAD
unicode_case_ignorable(0x02C6, 0x02D1).	% Case_Ignorable Lm  [12] MODIFIER LETTER CIRCUMFLEX ACCENT..MODIFIER LETTER HALF TRIANGULAR COLON
unicode_case_ignorable(0x02D2, 0x02DF).	% Case_Ignorable Sk  [14] MODIFIER LETTER CENTRED RIGHT HALF RING..MODIFIER LETTER CROSS ACCENT
unicode_case_ignorable(0x02E0, 0x02E4).	% Case_Ignorable Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_case_ignorable(0x02E5, 0x02EB).	% Case_Ignorable Sk   [7] MODIFIER LETTER EXTRA-HIGH TONE BAR..MODIFIER LETTER YANG DEPARTING TONE MARK
unicode_case_ignorable(0x02EC, 0x02EC).	% Case_Ignorable Lm       MODIFIER LETTER VOICING
unicode_case_ignorable(0x02ED, 0x02ED).	% Case_Ignorable Sk       MODIFIER LETTER UNASPIRATED
unicode_case_ignorable(0x02EE, 0x02EE).	% Case_Ignorable Lm       MODIFIER LETTER DOUBLE APOSTROPHE
unicode_case_ignorable(0x02EF, 0x02FF).	% Case_Ignorable Sk  [17] MODIFIER LETTER LOW DOWN ARROWHEAD..MODIFIER LETTER LOW LEFT ARROW
unicode_case_ignorable(0x0300, 0x036F).	% Case_Ignorable Mn [112] COMBINING GRAVE ACCENT..COMBINING LATIN SMALL LETTER X
unicode_case_ignorable(0x0374, 0x0374).	% Case_Ignorable Lm       GREEK NUMERAL SIGN
unicode_case_ignorable(0x0375, 0x0375).	% Case_Ignorable Sk       GREEK LOWER NUMERAL SIGN
unicode_case_ignorable(0x037A, 0x037A).	% Case_Ignorable Lm       GREEK YPOGEGRAMMENI
unicode_case_ignorable(0x0384, 0x0385).	% Case_Ignorable Sk   [2] GREEK TONOS..GREEK DIALYTIKA TONOS
unicode_case_ignorable(0x0387, 0x0387).	% Case_Ignorable Po       GREEK ANO TELEIA
unicode_case_ignorable(0x0483, 0x0487).	% Case_Ignorable Mn   [5] COMBINING CYRILLIC TITLO..COMBINING CYRILLIC POKRYTIE
unicode_case_ignorable(0x0488, 0x0489).	% Case_Ignorable Me   [2] COMBINING CYRILLIC HUNDRED THOUSANDS SIGN..COMBINING CYRILLIC MILLIONS SIGN
unicode_case_ignorable(0x0559, 0x0559).	% Case_Ignorable Lm       ARMENIAN MODIFIER LETTER LEFT HALF RING
unicode_case_ignorable(0x0591, 0x05BD).	% Case_Ignorable Mn  [45] HEBREW ACCENT ETNAHTA..HEBREW POINT METEG
unicode_case_ignorable(0x05BF, 0x05BF).	% Case_Ignorable Mn       HEBREW POINT RAFE
unicode_case_ignorable(0x05C1, 0x05C2).	% Case_Ignorable Mn   [2] HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT
unicode_case_ignorable(0x05C4, 0x05C5).	% Case_Ignorable Mn   [2] HEBREW MARK UPPER DOT..HEBREW MARK LOWER DOT
unicode_case_ignorable(0x05C7, 0x05C7).	% Case_Ignorable Mn       HEBREW POINT QAMATS QATAN
unicode_case_ignorable(0x05F4, 0x05F4).	% Case_Ignorable Po       HEBREW PUNCTUATION GERSHAYIM
unicode_case_ignorable(0x0600, 0x0604).	% Case_Ignorable Cf   [5] ARABIC NUMBER SIGN..ARABIC SIGN SAMVAT
unicode_case_ignorable(0x0610, 0x061A).	% Case_Ignorable Mn  [11] ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM..ARABIC SMALL KASRA
unicode_case_ignorable(0x0640, 0x0640).	% Case_Ignorable Lm       ARABIC TATWEEL
unicode_case_ignorable(0x064B, 0x065F).	% Case_Ignorable Mn  [21] ARABIC FATHATAN..ARABIC WAVY HAMZA BELOW
unicode_case_ignorable(0x0670, 0x0670).	% Case_Ignorable Mn       ARABIC LETTER SUPERSCRIPT ALEF
unicode_case_ignorable(0x06D6, 0x06DC).	% Case_Ignorable Mn   [7] ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN
unicode_case_ignorable(0x06DD, 0x06DD).	% Case_Ignorable Cf       ARABIC END OF AYAH
unicode_case_ignorable(0x06DF, 0x06E4).	% Case_Ignorable Mn   [6] ARABIC SMALL HIGH ROUNDED ZERO..ARABIC SMALL HIGH MADDA
unicode_case_ignorable(0x06E5, 0x06E6).	% Case_Ignorable Lm   [2] ARABIC SMALL WAW..ARABIC SMALL YEH
unicode_case_ignorable(0x06E7, 0x06E8).	% Case_Ignorable Mn   [2] ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON
unicode_case_ignorable(0x06EA, 0x06ED).	% Case_Ignorable Mn   [4] ARABIC EMPTY CENTRE LOW STOP..ARABIC SMALL LOW MEEM
unicode_case_ignorable(0x070F, 0x070F).	% Case_Ignorable Cf       SYRIAC ABBREVIATION MARK
unicode_case_ignorable(0x0711, 0x0711).	% Case_Ignorable Mn       SYRIAC LETTER SUPERSCRIPT ALAPH
unicode_case_ignorable(0x0730, 0x074A).	% Case_Ignorable Mn  [27] SYRIAC PTHAHA ABOVE..SYRIAC BARREKH
unicode_case_ignorable(0x07A6, 0x07B0).	% Case_Ignorable Mn  [11] THAANA ABAFILI..THAANA SUKUN
unicode_case_ignorable(0x07EB, 0x07F3).	% Case_Ignorable Mn   [9] NKO COMBINING SHORT HIGH TONE..NKO COMBINING DOUBLE DOT ABOVE
unicode_case_ignorable(0x07F4, 0x07F5).	% Case_Ignorable Lm   [2] NKO HIGH TONE APOSTROPHE..NKO LOW TONE APOSTROPHE
unicode_case_ignorable(0x07FA, 0x07FA).	% Case_Ignorable Lm       NKO LAJANYALAN
unicode_case_ignorable(0x0816, 0x0819).	% Case_Ignorable Mn   [4] SAMARITAN MARK IN..SAMARITAN MARK DAGESH
unicode_case_ignorable(0x081A, 0x07FA).	% Case_Ignorable Lm       SAMARITAN MODIFIER LETTER EPENTHETIC YUT
unicode_case_ignorable(0x081B, 0x0823).	% Case_Ignorable Mn   [9] SAMARITAN MARK EPENTHETIC YUT..SAMARITAN VOWEL SIGN A
unicode_case_ignorable(0x0824, 0x0824).	% Case_Ignorable Lm       SAMARITAN MODIFIER LETTER SHORT A
unicode_case_ignorable(0x0825, 0x0827).	% Case_Ignorable Mn   [3] SAMARITAN VOWEL SIGN SHORT A..SAMARITAN VOWEL SIGN U
unicode_case_ignorable(0x0828, 0x0828).	% Case_Ignorable Lm       SAMARITAN MODIFIER LETTER I
unicode_case_ignorable(0x0829, 0x082D).	% Case_Ignorable Mn   [5] SAMARITAN VOWEL SIGN LONG I..SAMARITAN MARK NEQUDAA
unicode_case_ignorable(0x0859, 0x085B).	% Case_Ignorable Mn   [3] MANDAIC AFFRICATION MARK..MANDAIC GEMINATION MARK
unicode_case_ignorable(0x08E4, 0x08FE).	% Case_Ignorable Mn  [27] ARABIC CURLY FATHA..ARABIC DAMMA WITH DOT
unicode_case_ignorable(0x0900, 0x0902).	% Case_Ignorable Mn   [3] DEVANAGARI SIGN INVERTED CANDRABINDU..DEVANAGARI SIGN ANUSVARA
unicode_case_ignorable(0x093A, 0x093A).	% Case_Ignorable Mn       DEVANAGARI VOWEL SIGN OE
unicode_case_ignorable(0x093C, 0x093C).	% Case_Ignorable Mn       DEVANAGARI SIGN NUKTA
unicode_case_ignorable(0x0941, 0x0948).	% Case_Ignorable Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
unicode_case_ignorable(0x094D, 0x094D).	% Case_Ignorable Mn       DEVANAGARI SIGN VIRAMA
unicode_case_ignorable(0x0951, 0x0957).	% Case_Ignorable Mn   [7] DEVANAGARI STRESS SIGN UDATTA..DEVANAGARI VOWEL SIGN UUE
unicode_case_ignorable(0x0962, 0x0963).	% Case_Ignorable Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
unicode_case_ignorable(0x0971, 0x0971).	% Case_Ignorable Lm       DEVANAGARI SIGN HIGH SPACING DOT
unicode_case_ignorable(0x0981, 0x0981).	% Case_Ignorable Mn       BENGALI SIGN CANDRABINDU
unicode_case_ignorable(0x09BC, 0x09BC).	% Case_Ignorable Mn       BENGALI SIGN NUKTA
unicode_case_ignorable(0x09C1, 0x09C4).	% Case_Ignorable Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
unicode_case_ignorable(0x09CD, 0x09CD).	% Case_Ignorable Mn       BENGALI SIGN VIRAMA
unicode_case_ignorable(0x09E2, 0x09E3).	% Case_Ignorable Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
unicode_case_ignorable(0x0A01, 0x0A02).	% Case_Ignorable Mn   [2] GURMUKHI SIGN ADAK BINDI..GURMUKHI SIGN BINDI
unicode_case_ignorable(0x0A3C, 0x0A3C).	% Case_Ignorable Mn       GURMUKHI SIGN NUKTA
unicode_case_ignorable(0x0A41, 0x0A42).	% Case_Ignorable Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
unicode_case_ignorable(0x0A47, 0x0A48).	% Case_Ignorable Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
unicode_case_ignorable(0x0A4B, 0x0A4D).	% Case_Ignorable Mn   [3] GURMUKHI VOWEL SIGN OO..GURMUKHI SIGN VIRAMA
unicode_case_ignorable(0x0A51, 0x0A51).	% Case_Ignorable Mn       GURMUKHI SIGN UDAAT
unicode_case_ignorable(0x0A70, 0x0A71).	% Case_Ignorable Mn   [2] GURMUKHI TIPPI..GURMUKHI ADDAK
unicode_case_ignorable(0x0A75, 0x0A75).	% Case_Ignorable Mn       GURMUKHI SIGN YAKASH
unicode_case_ignorable(0x0A81, 0x0A82).	% Case_Ignorable Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
unicode_case_ignorable(0x0ABC, 0x0ABC).	% Case_Ignorable Mn       GUJARATI SIGN NUKTA
unicode_case_ignorable(0x0AC1, 0x0AC5).	% Case_Ignorable Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
unicode_case_ignorable(0x0AC7, 0x0AC8).	% Case_Ignorable Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
unicode_case_ignorable(0x0ACD, 0x0ACD).	% Case_Ignorable Mn       GUJARATI SIGN VIRAMA
unicode_case_ignorable(0x0AE2, 0x0AE3).	% Case_Ignorable Mn   [2] GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL
unicode_case_ignorable(0x0B01, 0x0B01).	% Case_Ignorable Mn       ORIYA SIGN CANDRABINDU
unicode_case_ignorable(0x0B3C, 0x0B3C).	% Case_Ignorable Mn       ORIYA SIGN NUKTA
unicode_case_ignorable(0x0B3F, 0x0B3F).	% Case_Ignorable Mn       ORIYA VOWEL SIGN I
unicode_case_ignorable(0x0B41, 0x0B44).	% Case_Ignorable Mn   [4] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR
unicode_case_ignorable(0x0B4D, 0x0B4D).	% Case_Ignorable Mn       ORIYA SIGN VIRAMA
unicode_case_ignorable(0x0B56, 0x0B56).	% Case_Ignorable Mn       ORIYA AI LENGTH MARK
unicode_case_ignorable(0x0B62, 0x0B63).	% Case_Ignorable Mn   [2] ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL
unicode_case_ignorable(0x0B82, 0x0B82).	% Case_Ignorable Mn       TAMIL SIGN ANUSVARA
unicode_case_ignorable(0x0BC0, 0x0BC0).	% Case_Ignorable Mn       TAMIL VOWEL SIGN II
unicode_case_ignorable(0x0BCD, 0x0BCD).	% Case_Ignorable Mn       TAMIL SIGN VIRAMA
unicode_case_ignorable(0x0C3E, 0x0C40).	% Case_Ignorable Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
unicode_case_ignorable(0x0C46, 0x0C48).	% Case_Ignorable Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
unicode_case_ignorable(0x0C4A, 0x0C4D).	% Case_Ignorable Mn   [4] TELUGU VOWEL SIGN O..TELUGU SIGN VIRAMA
unicode_case_ignorable(0x0C55, 0x0C56).	% Case_Ignorable Mn   [2] TELUGU LENGTH MARK..TELUGU AI LENGTH MARK
unicode_case_ignorable(0x0C62, 0x0C63).	% Case_Ignorable Mn   [2] TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL
unicode_case_ignorable(0x0CBC, 0x0CBC).	% Case_Ignorable Mn       KANNADA SIGN NUKTA
unicode_case_ignorable(0x0CBF, 0x0CBF).	% Case_Ignorable Mn       KANNADA VOWEL SIGN I
unicode_case_ignorable(0x0CC6, 0x0CC6).	% Case_Ignorable Mn       KANNADA VOWEL SIGN E
unicode_case_ignorable(0x0CCC, 0x0CCD).	% Case_Ignorable Mn   [2] KANNADA VOWEL SIGN AU..KANNADA SIGN VIRAMA
unicode_case_ignorable(0x0CE2, 0x0CE3).	% Case_Ignorable Mn   [2] KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL
unicode_case_ignorable(0x0D41, 0x0D44).	% Case_Ignorable Mn   [4] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC RR
unicode_case_ignorable(0x0D4D, 0x0D4D).	% Case_Ignorable Mn       MALAYALAM SIGN VIRAMA
unicode_case_ignorable(0x0D62, 0x0D63).	% Case_Ignorable Mn   [2] MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL
unicode_case_ignorable(0x0DCA, 0x0DCA).	% Case_Ignorable Mn       SINHALA SIGN AL-LAKUNA
unicode_case_ignorable(0x0DD2, 0x0DD4).	% Case_Ignorable Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
unicode_case_ignorable(0x0DD6, 0x0DD6).	% Case_Ignorable Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
unicode_case_ignorable(0x0E31, 0x0E31).	% Case_Ignorable Mn       THAI CHARACTER MAI HAN-AKAT
unicode_case_ignorable(0x0E34, 0x0E3A).	% Case_Ignorable Mn   [7] THAI CHARACTER SARA I..THAI CHARACTER PHINTHU
unicode_case_ignorable(0x0E46, 0x0E46).	% Case_Ignorable Lm       THAI CHARACTER MAIYAMOK
unicode_case_ignorable(0x0E47, 0x0E4E).	% Case_Ignorable Mn   [8] THAI CHARACTER MAITAIKHU..THAI CHARACTER YAMAKKAN
unicode_case_ignorable(0x0EB1, 0x0EB1).	% Case_Ignorable Mn       LAO VOWEL SIGN MAI KAN
unicode_case_ignorable(0x0EB4, 0x0EB9).	% Case_Ignorable Mn   [6] LAO VOWEL SIGN I..LAO VOWEL SIGN UU
unicode_case_ignorable(0x0EBB, 0x0EBC).	% Case_Ignorable Mn   [2] LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO
unicode_case_ignorable(0x0EC6, 0x0EC6).	% Case_Ignorable Lm       LAO KO LA
unicode_case_ignorable(0x0EC8, 0x0ECD).	% Case_Ignorable Mn   [6] LAO TONE MAI EK..LAO NIGGAHITA
unicode_case_ignorable(0x0F18, 0x0F19).	% Case_Ignorable Mn   [2] TIBETAN ASTROLOGICAL SIGN -KHYUD PA..TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS
unicode_case_ignorable(0x0F35, 0x0F35).	% Case_Ignorable Mn       TIBETAN MARK NGAS BZUNG NYI ZLA
unicode_case_ignorable(0x0F37, 0x0F37).	% Case_Ignorable Mn       TIBETAN MARK NGAS BZUNG SGOR RTAGS
unicode_case_ignorable(0x0F39, 0x0F39).	% Case_Ignorable Mn       TIBETAN MARK TSA -PHRU
unicode_case_ignorable(0x0F71, 0x0F7E).	% Case_Ignorable Mn  [14] TIBETAN VOWEL SIGN AA..TIBETAN SIGN RJES SU NGA RO
unicode_case_ignorable(0x0F80, 0x0F84).	% Case_Ignorable Mn   [5] TIBETAN VOWEL SIGN REVERSED I..TIBETAN MARK HALANTA
unicode_case_ignorable(0x0F86, 0x0F87).	% Case_Ignorable Mn   [2] TIBETAN SIGN LCI RTAGS..TIBETAN SIGN YANG RTAGS
unicode_case_ignorable(0x0F8D, 0x0F97).	% Case_Ignorable Mn  [11] TIBETAN SUBJOINED SIGN LCE TSA CAN..TIBETAN SUBJOINED LETTER JA
unicode_case_ignorable(0x0F99, 0x0FBC).	% Case_Ignorable Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
unicode_case_ignorable(0x0FC6, 0x0FC6).	% Case_Ignorable Mn       TIBETAN SYMBOL PADMA GDAN
unicode_case_ignorable(0x102D, 0x1030).	% Case_Ignorable Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
unicode_case_ignorable(0x1032, 0x1037).	% Case_Ignorable Mn   [6] MYANMAR VOWEL SIGN AI..MYANMAR SIGN DOT BELOW
unicode_case_ignorable(0x1039, 0x103A).	% Case_Ignorable Mn   [2] MYANMAR SIGN VIRAMA..MYANMAR SIGN ASAT
unicode_case_ignorable(0x103D, 0x103E).	% Case_Ignorable Mn   [2] MYANMAR CONSONANT SIGN MEDIAL WA..MYANMAR CONSONANT SIGN MEDIAL HA
unicode_case_ignorable(0x1058, 0x1059).	% Case_Ignorable Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
unicode_case_ignorable(0x105E, 0x1060).	% Case_Ignorable Mn   [3] MYANMAR CONSONANT SIGN MON MEDIAL NA..MYANMAR CONSONANT SIGN MON MEDIAL LA
unicode_case_ignorable(0x1071, 0x1074).	% Case_Ignorable Mn   [4] MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE
unicode_case_ignorable(0x1082, 0x1082).	% Case_Ignorable Mn       MYANMAR CONSONANT SIGN SHAN MEDIAL WA
unicode_case_ignorable(0x1085, 0x1086).	% Case_Ignorable Mn   [2] MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y
unicode_case_ignorable(0x108D, 0x108D).	% Case_Ignorable Mn       MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE
unicode_case_ignorable(0x109D, 0x109D).	% Case_Ignorable Mn       MYANMAR VOWEL SIGN AITON AI
unicode_case_ignorable(0x10FC, 0x10FC).	% Case_Ignorable Lm       MODIFIER LETTER GEORGIAN NAR
unicode_case_ignorable(0x135D, 0x135F).	% Case_Ignorable Mn   [3] ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK..ETHIOPIC COMBINING GEMINATION MARK
unicode_case_ignorable(0x1712, 0x1714).	% Case_Ignorable Mn   [3] TAGALOG VOWEL SIGN I..TAGALOG SIGN VIRAMA
unicode_case_ignorable(0x1732, 0x1734).	% Case_Ignorable Mn   [3] HANUNOO VOWEL SIGN I..HANUNOO SIGN PAMUDPOD
unicode_case_ignorable(0x1752, 0x1753).	% Case_Ignorable Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
unicode_case_ignorable(0x1772, 0x1773).	% Case_Ignorable Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
unicode_case_ignorable(0x17B4, 0x17B5).	% Case_Ignorable Mn   [2] KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA
unicode_case_ignorable(0x17B7, 0x17BD).	% Case_Ignorable Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
unicode_case_ignorable(0x17C6, 0x17C6).	% Case_Ignorable Mn       KHMER SIGN NIKAHIT
unicode_case_ignorable(0x17C9, 0x17D3).	% Case_Ignorable Mn  [11] KHMER SIGN MUUSIKATOAN..KHMER SIGN BATHAMASAT
unicode_case_ignorable(0x17D7, 0x17D7).	% Case_Ignorable Lm       KHMER SIGN LEK TOO
unicode_case_ignorable(0x17DD, 0x17DD).	% Case_Ignorable Mn       KHMER SIGN ATTHACAN
unicode_case_ignorable(0x180B, 0x180D).	% Case_Ignorable Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
unicode_case_ignorable(0x1843, 0x1843).	% Case_Ignorable Lm       MONGOLIAN LETTER TODO LONG VOWEL SIGN
unicode_case_ignorable(0x18A9, 0x18A9).	% Case_Ignorable Mn       MONGOLIAN LETTER ALI GALI DAGALGA
unicode_case_ignorable(0x1920, 0x1922).	% Case_Ignorable Mn   [3] LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN U
unicode_case_ignorable(0x1927, 0x1928).	% Case_Ignorable Mn   [2] LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O
unicode_case_ignorable(0x1932, 0x1932).	% Case_Ignorable Mn       LIMBU SMALL LETTER ANUSVARA
unicode_case_ignorable(0x1939, 0x193B).	% Case_Ignorable Mn   [3] LIMBU SIGN MUKPHRENG..LIMBU SIGN SA-I
unicode_case_ignorable(0x1A17, 0x1A18).	% Case_Ignorable Mn   [2] BUGINESE VOWEL SIGN I..BUGINESE VOWEL SIGN U
unicode_case_ignorable(0x1A56, 0x1A56).	% Case_Ignorable Mn       TAI THAM CONSONANT SIGN MEDIAL LA
unicode_case_ignorable(0x1A58, 0x1A5E).	% Case_Ignorable Mn   [7] TAI THAM SIGN MAI KANG LAI..TAI THAM CONSONANT SIGN SA
unicode_case_ignorable(0x1A60, 0x1A60).	% Case_Ignorable Mn       TAI THAM SIGN SAKOT
unicode_case_ignorable(0x1A62, 0x1A62).	% Case_Ignorable Mn       TAI THAM VOWEL SIGN MAI SAT
unicode_case_ignorable(0x1A65, 0x1A6C).	% Case_Ignorable Mn   [8] TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN OA BELOW
unicode_case_ignorable(0x1A73, 0x1A7C).	% Case_Ignorable Mn  [10] TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN KHUEN-LUE KARAN
unicode_case_ignorable(0x1A7F, 0x1A7F).	% Case_Ignorable Mn       TAI THAM COMBINING CRYPTOGRAMMIC DOT
unicode_case_ignorable(0x1AA7, 0x1AA7).	% Case_Ignorable Lm       TAI THAM SIGN MAI YAMOK
unicode_case_ignorable(0x1B00, 0x1B03).	% Case_Ignorable Mn   [4] BALINESE SIGN ULU RICEM..BALINESE SIGN SURANG
unicode_case_ignorable(0x1B34, 0x1B34).	% Case_Ignorable Mn       BALINESE SIGN REREKAN
unicode_case_ignorable(0x1B36, 0x1B3A).	% Case_Ignorable Mn   [5] BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN RA REPA
unicode_case_ignorable(0x1B3C, 0x1B3C).	% Case_Ignorable Mn       BALINESE VOWEL SIGN LA LENGA
unicode_case_ignorable(0x1B42, 0x1B42).	% Case_Ignorable Mn       BALINESE VOWEL SIGN PEPET
unicode_case_ignorable(0x1B6B, 0x1B73).	% Case_Ignorable Mn   [9] BALINESE MUSICAL SYMBOL COMBINING TEGEH..BALINESE MUSICAL SYMBOL COMBINING GONG
unicode_case_ignorable(0x1B80, 0x1B81).	% Case_Ignorable Mn   [2] SUNDANESE SIGN PANYECEK..SUNDANESE SIGN PANGLAYAR
unicode_case_ignorable(0x1BA2, 0x1BA5).	% Case_Ignorable Mn   [4] SUNDANESE CONSONANT SIGN PANYAKRA..SUNDANESE VOWEL SIGN PANYUKU
unicode_case_ignorable(0x1BA8, 0x1BA9).	% Case_Ignorable Mn   [2] SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG
unicode_case_ignorable(0x1BAB, 0x1BAB).	% Case_Ignorable Mn       SUNDANESE SIGN VIRAMA
unicode_case_ignorable(0x1BE6, 0x1BE6).	% Case_Ignorable Mn       BATAK SIGN TOMPI
unicode_case_ignorable(0x1BE8, 0x1BE9).	% Case_Ignorable Mn   [2] BATAK VOWEL SIGN PAKPAK E..BATAK VOWEL SIGN EE
unicode_case_ignorable(0x1BED, 0x1BED).	% Case_Ignorable Mn       BATAK VOWEL SIGN KARO O
unicode_case_ignorable(0x1BEF, 0x1BF1).	% Case_Ignorable Mn   [3] BATAK VOWEL SIGN U FOR SIMALUNGUN SA..BATAK CONSONANT SIGN H
unicode_case_ignorable(0x1C2C, 0x1C33).	% Case_Ignorable Mn   [8] LEPCHA VOWEL SIGN E..LEPCHA CONSONANT SIGN T
unicode_case_ignorable(0x1C36, 0x1C37).	% Case_Ignorable Mn   [2] LEPCHA SIGN RAN..LEPCHA SIGN NUKTA
unicode_case_ignorable(0x1C78, 0x1C7D).	% Case_Ignorable Lm   [6] OL CHIKI MU TTUDDAG..OL CHIKI AHAD
unicode_case_ignorable(0x1CD0, 0x1CD2).	% Case_Ignorable Mn   [3] VEDIC TONE KARSHANA..VEDIC TONE PRENKHA
unicode_case_ignorable(0x1CD4, 0x1CE0).	% Case_Ignorable Mn  [13] VEDIC SIGN YAJURVEDIC MIDLINE SVARITA..VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA
unicode_case_ignorable(0x1CE2, 0x1CE8).	% Case_Ignorable Mn   [7] VEDIC SIGN VISARGA SVARITA..VEDIC SIGN VISARGA ANUDATTA WITH TAIL
unicode_case_ignorable(0x1CED, 0x1CED).	% Case_Ignorable Mn       VEDIC SIGN TIRYAK
unicode_case_ignorable(0x1CF4, 0x1CF4).	% Case_Ignorable Mn       VEDIC TONE CANDRA ABOVE
unicode_case_ignorable(0x1D2C, 0x1D6A).	% Case_Ignorable Lm  [63] MODIFIER LETTER CAPITAL A..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_case_ignorable(0x1D78, 0x1D78).	% Case_Ignorable Lm       MODIFIER LETTER CYRILLIC EN
unicode_case_ignorable(0x1D9B, 0x1DBF).	% Case_Ignorable Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_case_ignorable(0x1DC0, 0x1DE6).	% Case_Ignorable Mn  [39] COMBINING DOTTED GRAVE ACCENT..COMBINING LATIN SMALL LETTER Z
unicode_case_ignorable(0x1DFC, 0x1DFF).	% Case_Ignorable Mn   [4] COMBINING DOUBLE INVERTED BREVE BELOW..COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW
unicode_case_ignorable(0x1FBD, 0x1FBD).	% Case_Ignorable Sk       GREEK KORONIS
unicode_case_ignorable(0x1FBF, 0x1FC1).	% Case_Ignorable Sk   [3] GREEK PSILI..GREEK DIALYTIKA AND PERISPOMENI
unicode_case_ignorable(0x1FCD, 0x1FCF).	% Case_Ignorable Sk   [3] GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI
unicode_case_ignorable(0x1FDD, 0x1FDF).	% Case_Ignorable Sk   [3] GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI
unicode_case_ignorable(0x1FED, 0x1FEF).	% Case_Ignorable Sk   [3] GREEK DIALYTIKA AND VARIA..GREEK VARIA
unicode_case_ignorable(0x1FFD, 0x1FFE).	% Case_Ignorable Sk   [2] GREEK OXIA..GREEK DASIA
unicode_case_ignorable(0x200B, 0x200F).	% Case_Ignorable Cf   [5] ZERO WIDTH SPACE..RIGHT-TO-LEFT MARK
unicode_case_ignorable(0x2018, 0x2018).	% Case_Ignorable Pi       LEFT SINGLE QUOTATION MARK
unicode_case_ignorable(0x2019, 0x2019).	% Case_Ignorable Pf       RIGHT SINGLE QUOTATION MARK
unicode_case_ignorable(0x2024, 0x2024).	% Case_Ignorable Po       ONE DOT LEADER
unicode_case_ignorable(0x2027, 0x2027).	% Case_Ignorable Po       HYPHENATION POINT
unicode_case_ignorable(0x202A, 0x202E).	% Case_Ignorable Cf   [5] LEFT-TO-RIGHT EMBEDDING..RIGHT-TO-LEFT OVERRIDE
unicode_case_ignorable(0x2060, 0x2064).	% Case_Ignorable Cf   [5] WORD JOINER..INVISIBLE PLUS
unicode_case_ignorable(0x206A, 0x206F).	% Case_Ignorable Cf   [6] INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES
unicode_case_ignorable(0x2071, 0x2071).	% Case_Ignorable Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_case_ignorable(0x207F, 0x207F).	% Case_Ignorable Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_case_ignorable(0x2090, 0x209C).	% Case_Ignorable Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_case_ignorable(0x20D0, 0x20DC).	% Case_Ignorable Mn  [13] COMBINING LEFT HARPOON ABOVE..COMBINING FOUR DOTS ABOVE
unicode_case_ignorable(0x20DD, 0x20E0).	% Case_Ignorable Me   [4] COMBINING ENCLOSING CIRCLE..COMBINING ENCLOSING CIRCLE BACKSLASH
unicode_case_ignorable(0x20E1, 0x20E1).	% Case_Ignorable Mn       COMBINING LEFT RIGHT ARROW ABOVE
unicode_case_ignorable(0x20E2, 0x20E4).	% Case_Ignorable Me   [3] COMBINING ENCLOSING SCREEN..COMBINING ENCLOSING UPWARD POINTING TRIANGLE
unicode_case_ignorable(0x20E5, 0x20F0).	% Case_Ignorable Mn  [12] COMBINING REVERSE SOLIDUS OVERLAY..COMBINING ASTERISK ABOVE
unicode_case_ignorable(0x2C7C, 0x2C7D).	% Case_Ignorable Lm   [2] LATIN SUBSCRIPT SMALL LETTER J..MODIFIER LETTER CAPITAL V
unicode_case_ignorable(0x2CEF, 0x2CF1).	% Case_Ignorable Mn   [3] COPTIC COMBINING NI ABOVE..COPTIC COMBINING SPIRITUS LENIS
unicode_case_ignorable(0x2D6F, 0x2D6F).	% Case_Ignorable Lm       TIFINAGH MODIFIER LETTER LABIALIZATION MARK
unicode_case_ignorable(0x2D7F, 0x2D7F).	% Case_Ignorable Mn       TIFINAGH CONSONANT JOINER
unicode_case_ignorable(0x2DE0, 0x2DFF).	% Case_Ignorable Mn  [32] COMBINING CYRILLIC LETTER BE..COMBINING CYRILLIC LETTER IOTIFIED BIG YUS
unicode_case_ignorable(0x2E2F, 0x2E2F).	% Case_Ignorable Lm       VERTICAL TILDE
unicode_case_ignorable(0x3005, 0x3005).	% Case_Ignorable Lm       IDEOGRAPHIC ITERATION MARK
unicode_case_ignorable(0x302A, 0x302D).	% Case_Ignorable Mn   [4] IDEOGRAPHIC LEVEL TONE MARK..IDEOGRAPHIC ENTERING TONE MARK
unicode_case_ignorable(0x3031, 0x3035).	% Case_Ignorable Lm   [5] VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF
unicode_case_ignorable(0x303B, 0x303B).	% Case_Ignorable Lm       VERTICAL IDEOGRAPHIC ITERATION MARK
unicode_case_ignorable(0x3099, 0x309A).	% Case_Ignorable Mn   [2] COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK..COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_case_ignorable(0x309B, 0x309C).	% Case_Ignorable Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_case_ignorable(0x309D, 0x309E).	% Case_Ignorable Lm   [2] HIRAGANA ITERATION MARK..HIRAGANA VOICED ITERATION MARK
unicode_case_ignorable(0x30FC, 0x30FE).	% Case_Ignorable Lm   [3] KATAKANA-HIRAGANA PROLONGED SOUND MARK..KATAKANA VOICED ITERATION MARK
unicode_case_ignorable(0xA015, 0xA015).	% Case_Ignorable Lm       YI SYLLABLE WU
unicode_case_ignorable(0xA4F8, 0xA4FD).	% Case_Ignorable Lm   [6] LISU LETTER TONE MYA TI..LISU LETTER TONE MYA JEU
unicode_case_ignorable(0xA60C, 0xA60C).	% Case_Ignorable Lm       VAI SYLLABLE LENGTHENER
unicode_case_ignorable(0xA66F, 0xA66F).	% Case_Ignorable Mn       COMBINING CYRILLIC VZMET
unicode_case_ignorable(0xA670, 0xA672).	% Case_Ignorable Me   [3] COMBINING CYRILLIC TEN MILLIONS SIGN..COMBINING CYRILLIC THOUSAND MILLIONS SIGN
unicode_case_ignorable(0xA674, 0xA67D).	% Case_Ignorable Mn  [10] COMBINING CYRILLIC LETTER UKRAINIAN IE..COMBINING CYRILLIC PAYEROK
unicode_case_ignorable(0xA67F, 0xA67F).	% Case_Ignorable Lm       CYRILLIC PAYEROK
unicode_case_ignorable(0xA69F, 0xA69F).	% Case_Ignorable Mn       COMBINING CYRILLIC LETTER IOTIFIED E
unicode_case_ignorable(0xA6F0, 0xA6F1).	% Case_Ignorable Mn   [2] BAMUM COMBINING MARK KOQNDON..BAMUM COMBINING MARK TUKWENTIS
unicode_case_ignorable(0xA700, 0xA716).	% Case_Ignorable Sk  [23] MODIFIER LETTER CHINESE TONE YIN PING..MODIFIER LETTER EXTRA-LOW LEFT-STEM TONE BAR
unicode_case_ignorable(0xA717, 0xA71F).	% Case_Ignorable Lm   [9] MODIFIER LETTER DOT VERTICAL BAR..MODIFIER LETTER LOW INVERTED EXCLAMATION MARK
unicode_case_ignorable(0xA720, 0xA721).	% Case_Ignorable Sk   [2] MODIFIER LETTER STRESS AND HIGH TONE..MODIFIER LETTER STRESS AND LOW TONE
unicode_case_ignorable(0xA770, 0xA770).	% Case_Ignorable Lm       MODIFIER LETTER US
unicode_case_ignorable(0xA788, 0xA788).	% Case_Ignorable Lm       MODIFIER LETTER LOW CIRCUMFLEX ACCENT
unicode_case_ignorable(0xA789, 0xA78A).	% Case_Ignorable Sk   [2] MODIFIER LETTER COLON..MODIFIER LETTER SHORT EQUALS SIGN
unicode_case_ignorable(0xA7F8, 0xA7F9).	% Case_Ignorable Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_case_ignorable(0xA802, 0xA802).	% Case_Ignorable Mn       SYLOTI NAGRI SIGN DVISVARA
unicode_case_ignorable(0xA806, 0xA806).	% Case_Ignorable Mn       SYLOTI NAGRI SIGN HASANTA
unicode_case_ignorable(0xA80B, 0xA80B).	% Case_Ignorable Mn       SYLOTI NAGRI SIGN ANUSVARA
unicode_case_ignorable(0xA825, 0xA826).	% Case_Ignorable Mn   [2] SYLOTI NAGRI VOWEL SIGN U..SYLOTI NAGRI VOWEL SIGN E
unicode_case_ignorable(0xA8C4, 0xA8C4).	% Case_Ignorable Mn       SAURASHTRA SIGN VIRAMA
unicode_case_ignorable(0xA8E0, 0xA8F1).	% Case_Ignorable Mn  [18] COMBINING DEVANAGARI DIGIT ZERO..COMBINING DEVANAGARI SIGN AVAGRAHA
unicode_case_ignorable(0xA926, 0xA92D).	% Case_Ignorable Mn   [8] KAYAH LI VOWEL UE..KAYAH LI TONE CALYA PLOPHU
unicode_case_ignorable(0xA947, 0xA951).	% Case_Ignorable Mn  [11] REJANG VOWEL SIGN I..REJANG CONSONANT SIGN R
unicode_case_ignorable(0xA980, 0xA982).	% Case_Ignorable Mn   [3] JAVANESE SIGN PANYANGGA..JAVANESE SIGN LAYAR
unicode_case_ignorable(0xA9B3, 0xA9B3).	% Case_Ignorable Mn       JAVANESE SIGN CECAK TELU
unicode_case_ignorable(0xA9B6, 0xA9B9).	% Case_Ignorable Mn   [4] JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN SUKU MENDUT
unicode_case_ignorable(0xA9BC, 0xA9BC).	% Case_Ignorable Mn       JAVANESE VOWEL SIGN PEPET
unicode_case_ignorable(0xA9CF, 0xA9CF).	% Case_Ignorable Lm       JAVANESE PANGRANGKEP
unicode_case_ignorable(0xAA29, 0xAA2E).	% Case_Ignorable Mn   [6] CHAM VOWEL SIGN AA..CHAM VOWEL SIGN OE
unicode_case_ignorable(0xAA31, 0xAA32).	% Case_Ignorable Mn   [2] CHAM VOWEL SIGN AU..CHAM VOWEL SIGN UE
unicode_case_ignorable(0xAA35, 0xAA36).	% Case_Ignorable Mn   [2] CHAM CONSONANT SIGN LA..CHAM CONSONANT SIGN WA
unicode_case_ignorable(0xAA43, 0xAA43).	% Case_Ignorable Mn       CHAM CONSONANT SIGN FINAL NG
unicode_case_ignorable(0xAA4C, 0xAA4C).	% Case_Ignorable Mn       CHAM CONSONANT SIGN FINAL M
unicode_case_ignorable(0xAA70, 0xAA70).	% Case_Ignorable Lm       MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION
unicode_case_ignorable(0xAAB0, 0xAAB0).	% Case_Ignorable Mn       TAI VIET MAI KANG
unicode_case_ignorable(0xAAB2, 0xAAB4).	% Case_Ignorable Mn   [3] TAI VIET VOWEL I..TAI VIET VOWEL U
unicode_case_ignorable(0xAAB7, 0xAAB8).	% Case_Ignorable Mn   [2] TAI VIET MAI KHIT..TAI VIET VOWEL IA
unicode_case_ignorable(0xAABE, 0xAABF).	% Case_Ignorable Mn   [2] TAI VIET VOWEL AM..TAI VIET TONE MAI EK
unicode_case_ignorable(0xAAC1, 0xAAC1).	% Case_Ignorable Mn       TAI VIET TONE MAI THO
unicode_case_ignorable(0xAADD, 0xAADD).	% Case_Ignorable Lm       TAI VIET SYMBOL SAM
unicode_case_ignorable(0xAAEC, 0xAAED).	% Case_Ignorable Mn   [2] MEETEI MAYEK VOWEL SIGN UU..MEETEI MAYEK VOWEL SIGN AAI
unicode_case_ignorable(0xAAF3, 0xAAF4).	% Case_Ignorable Lm   [2] MEETEI MAYEK SYLLABLE REPETITION MARK..MEETEI MAYEK WORD REPETITION MARK
unicode_case_ignorable(0xAAF6, 0xAAF6).	% Case_Ignorable Mn       MEETEI MAYEK VIRAMA
unicode_case_ignorable(0xABE5, 0xABE5).	% Case_Ignorable Mn       MEETEI MAYEK VOWEL SIGN ANAP
unicode_case_ignorable(0xABE8, 0xABE8).	% Case_Ignorable Mn       MEETEI MAYEK VOWEL SIGN UNAP
unicode_case_ignorable(0xABED, 0xABED).	% Case_Ignorable Mn       MEETEI MAYEK APUN IYEK
unicode_case_ignorable(0xFB1E, 0xFB1E).	% Case_Ignorable Mn       HEBREW POINT JUDEO-SPANISH VARIKA
unicode_case_ignorable(0xFBB2, 0xFBC1).	% Case_Ignorable Sk  [16] ARABIC SYMBOL DOT ABOVE..ARABIC SYMBOL SMALL TAH BELOW
unicode_case_ignorable(0xFE00, 0xFE0F).	% Case_Ignorable Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
unicode_case_ignorable(0xFE13, 0xFE13).	% Case_Ignorable Po       PRESENTATION FORM FOR VERTICAL COLON
unicode_case_ignorable(0xFE20, 0xFE26).	% Case_Ignorable Mn   [7] COMBINING LIGATURE LEFT HALF..COMBINING CONJOINING MACRON
unicode_case_ignorable(0xFE52, 0xFE52).	% Case_Ignorable Po       SMALL FULL STOP
unicode_case_ignorable(0xFE55, 0xFE55).	% Case_Ignorable Po       SMALL COLON
unicode_case_ignorable(0xFEFF, 0xFEFF).	% Case_Ignorable Cf       ZERO WIDTH NO-BREAK SPACE
unicode_case_ignorable(0xFF07, 0xFF07).	% Case_Ignorable Po       FULLWIDTH APOSTROPHE
unicode_case_ignorable(0xFF0E, 0xFF0E).	% Case_Ignorable Po       FULLWIDTH FULL STOP
unicode_case_ignorable(0xFF1A, 0xFF1A).	% Case_Ignorable Po       FULLWIDTH COLON
unicode_case_ignorable(0xFF3E, 0xFF3E).	% Case_Ignorable Sk       FULLWIDTH CIRCUMFLEX ACCENT
unicode_case_ignorable(0xFF40, 0xFF40).	% Case_Ignorable Sk       FULLWIDTH GRAVE ACCENT
unicode_case_ignorable(0xFF70, 0xFF70).	% Case_Ignorable Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_case_ignorable(0xFF9E, 0xFF9F).	% Case_Ignorable Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
unicode_case_ignorable(0xFFE3, 0xFFE3).	% Case_Ignorable Sk       FULLWIDTH MACRON
unicode_case_ignorable(0xFFF9, 0xFFFB).	% Case_Ignorable Cf   [3] INTERLINEAR ANNOTATION ANCHOR..INTERLINEAR ANNOTATION TERMINATOR
unicode_case_ignorable(0x101FD, 0x101FD).	% Case_Ignorable Mn       PHAISTOS DISC SIGN COMBINING OBLIQUE STROKE
unicode_case_ignorable(0x10A01, 0x10A03).	% Case_Ignorable Mn   [3] KHAROSHTHI VOWEL SIGN I..KHAROSHTHI VOWEL SIGN VOCALIC R
unicode_case_ignorable(0x10A05, 0x10A06).	% Case_Ignorable Mn   [2] KHAROSHTHI VOWEL SIGN E..KHAROSHTHI VOWEL SIGN O
unicode_case_ignorable(0x10A0C, 0x10A0F).	% Case_Ignorable Mn   [4] KHAROSHTHI VOWEL LENGTH MARK..KHAROSHTHI SIGN VISARGA
unicode_case_ignorable(0x10A38, 0x10A3A).	% Case_Ignorable Mn   [3] KHAROSHTHI SIGN BAR ABOVE..KHAROSHTHI SIGN DOT BELOW
unicode_case_ignorable(0x10A3F, 0x10A3F).	% Case_Ignorable Mn       KHAROSHTHI VIRAMA
unicode_case_ignorable(0x11001, 0x11001).	% Case_Ignorable Mn       BRAHMI SIGN ANUSVARA
unicode_case_ignorable(0x11038, 0x11046).	% Case_Ignorable Mn  [15] BRAHMI VOWEL SIGN AA..BRAHMI VIRAMA
unicode_case_ignorable(0x11080, 0x11081).	% Case_Ignorable Mn   [2] KAITHI SIGN CANDRABINDU..KAITHI SIGN ANUSVARA
unicode_case_ignorable(0x110B3, 0x110B6).	% Case_Ignorable Mn   [4] KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN AI
unicode_case_ignorable(0x110B9, 0x110BA).	% Case_Ignorable Mn   [2] KAITHI SIGN VIRAMA..KAITHI SIGN NUKTA
unicode_case_ignorable(0x110BD, 0x110BD).	% Case_Ignorable Cf       KAITHI NUMBER SIGN
unicode_case_ignorable(0x11100, 0x11102).	% Case_Ignorable Mn   [3] CHAKMA SIGN CANDRABINDU..CHAKMA SIGN VISARGA
unicode_case_ignorable(0x11127, 0x1112B).	% Case_Ignorable Mn   [5] CHAKMA VOWEL SIGN A..CHAKMA VOWEL SIGN UU
unicode_case_ignorable(0x1112D, 0x11134).	% Case_Ignorable Mn   [8] CHAKMA VOWEL SIGN AI..CHAKMA MAAYYAA
unicode_case_ignorable(0x11180, 0x11181).	% Case_Ignorable Mn   [2] SHARADA SIGN CANDRABINDU..SHARADA SIGN ANUSVARA
unicode_case_ignorable(0x111B6, 0x111BE).	% Case_Ignorable Mn   [9] SHARADA VOWEL SIGN U..SHARADA VOWEL SIGN O
unicode_case_ignorable(0x116AB, 0x116AB).	% Case_Ignorable Mn       TAKRI SIGN ANUSVARA
unicode_case_ignorable(0x116AD, 0x116AD).	% Case_Ignorable Mn       TAKRI VOWEL SIGN AA
unicode_case_ignorable(0x116B0, 0x116B5).	% Case_Ignorable Mn   [6] TAKRI VOWEL SIGN U..TAKRI VOWEL SIGN AU
unicode_case_ignorable(0x116B7, 0x116B7).	% Case_Ignorable Mn       TAKRI SIGN NUKTA
unicode_case_ignorable(0x16F8F, 0x16F92).	% Case_Ignorable Mn   [4] MIAO TONE RIGHT..MIAO TONE BELOW
unicode_case_ignorable(0x16F93, 0x16F9F).	% Case_Ignorable Lm  [13] MIAO LETTER TONE-2..MIAO LETTER REFORMED TONE-8
unicode_case_ignorable(0x1D167, 0x1D169).	% Case_Ignorable Mn   [3] MUSICAL SYMBOL COMBINING TREMOLO-1..MUSICAL SYMBOL COMBINING TREMOLO-3
unicode_case_ignorable(0x1D173, 0x1D17A).	% Case_Ignorable Cf   [8] MUSICAL SYMBOL BEGIN BEAM..MUSICAL SYMBOL END PHRASE
unicode_case_ignorable(0x1D17B, 0x1D182).	% Case_Ignorable Mn   [8] MUSICAL SYMBOL COMBINING ACCENT..MUSICAL SYMBOL COMBINING LOURE
unicode_case_ignorable(0x1D185, 0x1D18B).	% Case_Ignorable Mn   [7] MUSICAL SYMBOL COMBINING DOIT..MUSICAL SYMBOL COMBINING TRIPLE TONGUE
unicode_case_ignorable(0x1D1AA, 0x1D1AD).	% Case_Ignorable Mn   [4] MUSICAL SYMBOL COMBINING DOWN BOW..MUSICAL SYMBOL COMBINING SNAP PIZZICATO
unicode_case_ignorable(0x1D242, 0x1D244).	% Case_Ignorable Mn   [3] COMBINING GREEK MUSICAL TRISEME..COMBINING GREEK MUSICAL PENTASEME
unicode_case_ignorable(0xE0001, 0xE0001).	% Case_Ignorable Cf       LANGUAGE TAG
unicode_case_ignorable(0xE0020, 0xE007F).	% Case_Ignorable Cf  [96] TAG SPACE..CANCEL TAG
unicode_case_ignorable(0xE0100, 0xE01EF).	% Case_Ignorable Mn [240] VARIATION SELECTOR-17..VARIATION SELECTOR-256

% Total code points: 1799
