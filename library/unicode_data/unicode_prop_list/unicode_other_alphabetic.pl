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
# PropList-6.1.0.txt
# Date: 2011-11-30, 01:49:54 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
*/

unicode_other_alphabetic(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_other_alphabetic(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_other_alphabetic(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_other_alphabetic(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_other_alphabetic(0x0345, 0x0345).	% Other_Alphabetic # Mn       COMBINING GREEK YPOGEGRAMMENI
unicode_other_alphabetic(0x05B0, 0x05BD).	% Other_Alphabetic # Mn  [14] HEBREW POINT SHEVA..HEBREW POINT METEG
unicode_other_alphabetic(0x05BF, 0x05BF).	% Other_Alphabetic # Mn       HEBREW POINT RAFE
unicode_other_alphabetic(0x05C1, 0x05C2).	% Other_Alphabetic # Mn   [2] HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT
unicode_other_alphabetic(0x05C4, 0x05C5).	% Other_Alphabetic # Mn   [2] HEBREW MARK UPPER DOT..HEBREW MARK LOWER DOT
unicode_other_alphabetic(0x05C7, 0x05C7).	% Other_Alphabetic # Mn       HEBREW POINT QAMATS QATAN
unicode_other_alphabetic(0x0610, 0x061A).	% Other_Alphabetic # Mn  [11] ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM..ARABIC SMALL KASRA
unicode_other_alphabetic(0x064B, 0x0657).	% Other_Alphabetic # Mn  [13] ARABIC FATHATAN..ARABIC INVERTED DAMMA
unicode_other_alphabetic(0x0659, 0x065F).	% Other_Alphabetic # Mn   [7] ARABIC ZWARAKAY..ARABIC WAVY HAMZA BELOW
unicode_other_alphabetic(0x0670, 0x0670).	% Other_Alphabetic # Mn       ARABIC LETTER SUPERSCRIPT ALEF
unicode_other_alphabetic(0x06D6, 0x06DC).	% Other_Alphabetic # Mn   [7] ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN
unicode_other_alphabetic(0x06E1, 0x06E4).	% Other_Alphabetic # Mn   [4] ARABIC SMALL HIGH DOTLESS HEAD OF KHAH..ARABIC SMALL HIGH MADDA
unicode_other_alphabetic(0x06E7, 0x06E8).	% Other_Alphabetic # Mn   [2] ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON
unicode_other_alphabetic(0x06ED, 0x06ED).	% Other_Alphabetic # Mn       ARABIC SMALL LOW MEEM
unicode_other_alphabetic(0x0711, 0x0711).	% Other_Alphabetic # Mn       SYRIAC LETTER SUPERSCRIPT ALAPH
unicode_other_alphabetic(0x0730, 0x073F).	% Other_Alphabetic # Mn  [16] SYRIAC PTHAHA ABOVE..SYRIAC RWAHA
unicode_other_alphabetic(0x07A6, 0x07B0).	% Other_Alphabetic # Mn  [11] THAANA ABAFILI..THAANA SUKUN
unicode_other_alphabetic(0x0816, 0x0817).	% Other_Alphabetic # Mn   [2] SAMARITAN MARK IN..SAMARITAN MARK IN-ALAF
unicode_other_alphabetic(0x081B, 0x0823).	% Other_Alphabetic # Mn   [9] SAMARITAN MARK EPENTHETIC YUT..SAMARITAN VOWEL SIGN A
unicode_other_alphabetic(0x0825, 0x0827).	% Other_Alphabetic # Mn   [3] SAMARITAN VOWEL SIGN SHORT A..SAMARITAN VOWEL SIGN U
unicode_other_alphabetic(0x0829, 0x082C).	% Other_Alphabetic # Mn   [4] SAMARITAN VOWEL SIGN LONG I..SAMARITAN VOWEL SIGN SUKUN
unicode_other_alphabetic(0x08E4, 0x08E9).	% Other_Alphabetic # Mn   [6] ARABIC CURLY FATHA..ARABIC CURLY KASRATAN
unicode_other_alphabetic(0x08F0, 0x08FE).	% Other_Alphabetic # Mn  [15] ARABIC OPEN FATHATAN..ARABIC DAMMA WITH DOT
unicode_other_alphabetic(0x0900, 0x0902).	% Other_Alphabetic # Mn   [3] DEVANAGARI SIGN INVERTED CANDRABINDU..DEVANAGARI SIGN ANUSVARA
unicode_other_alphabetic(0x0903, 0x0903).	% Other_Alphabetic # Mc       DEVANAGARI SIGN VISARGA
unicode_other_alphabetic(0x093A, 0x093A).	% Other_Alphabetic # Mn       DEVANAGARI VOWEL SIGN OE
unicode_other_alphabetic(0x093B, 0x093B).	% Other_Alphabetic # Mc       DEVANAGARI VOWEL SIGN OOE
unicode_other_alphabetic(0x093E, 0x0940).	% Other_Alphabetic # Mc   [3] DEVANAGARI VOWEL SIGN AA..DEVANAGARI VOWEL SIGN II
unicode_other_alphabetic(0x0941, 0x0948).	% Other_Alphabetic # Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
unicode_other_alphabetic(0x0949, 0x094C).	% Other_Alphabetic # Mc   [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU
unicode_other_alphabetic(0x094E, 0x094F).	% Other_Alphabetic # Mc   [2] DEVANAGARI VOWEL SIGN PRISHTHAMATRA E..DEVANAGARI VOWEL SIGN AW
unicode_other_alphabetic(0x0955, 0x0957).	% Other_Alphabetic # Mn   [3] DEVANAGARI VOWEL SIGN CANDRA LONG E..DEVANAGARI VOWEL SIGN UUE
unicode_other_alphabetic(0x0962, 0x0963).	% Other_Alphabetic # Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
unicode_other_alphabetic(0x0981, 0x0981).	% Other_Alphabetic # Mn       BENGALI SIGN CANDRABINDU
unicode_other_alphabetic(0x0982, 0x0983).	% Other_Alphabetic # Mc   [2] BENGALI SIGN ANUSVARA..BENGALI SIGN VISARGA
unicode_other_alphabetic(0x09BE, 0x09C0).	% Other_Alphabetic # Mc   [3] BENGALI VOWEL SIGN AA..BENGALI VOWEL SIGN II
unicode_other_alphabetic(0x09C1, 0x09C4).	% Other_Alphabetic # Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
unicode_other_alphabetic(0x09C7, 0x09C8).	% Other_Alphabetic # Mc   [2] BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI
unicode_other_alphabetic(0x09CB, 0x09CC).	% Other_Alphabetic # Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_other_alphabetic(0x09D7, 0x09D7).	% Other_Alphabetic # Mc       BENGALI AU LENGTH MARK
unicode_other_alphabetic(0x09E2, 0x09E3).	% Other_Alphabetic # Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
unicode_other_alphabetic(0x0A01, 0x0A02).	% Other_Alphabetic # Mn   [2] GURMUKHI SIGN ADAK BINDI..GURMUKHI SIGN BINDI
unicode_other_alphabetic(0x0A03, 0x0A03).	% Other_Alphabetic # Mc       GURMUKHI SIGN VISARGA
unicode_other_alphabetic(0x0A3E, 0x0A40).	% Other_Alphabetic # Mc   [3] GURMUKHI VOWEL SIGN AA..GURMUKHI VOWEL SIGN II
unicode_other_alphabetic(0x0A41, 0x0A42).	% Other_Alphabetic # Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
unicode_other_alphabetic(0x0A47, 0x0A48).	% Other_Alphabetic # Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
unicode_other_alphabetic(0x0A4B, 0x0A4C).	% Other_Alphabetic # Mn   [2] GURMUKHI VOWEL SIGN OO..GURMUKHI VOWEL SIGN AU
unicode_other_alphabetic(0x0A51, 0x0A51).	% Other_Alphabetic # Mn       GURMUKHI SIGN UDAAT
unicode_other_alphabetic(0x0A70, 0x0A71).	% Other_Alphabetic # Mn   [2] GURMUKHI TIPPI..GURMUKHI ADDAK
unicode_other_alphabetic(0x0A75, 0x0A75).	% Other_Alphabetic # Mn       GURMUKHI SIGN YAKASH
unicode_other_alphabetic(0x0A81, 0x0A82).	% Other_Alphabetic # Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
unicode_other_alphabetic(0x0A83, 0x0A83).	% Other_Alphabetic # Mc       GUJARATI SIGN VISARGA
unicode_other_alphabetic(0x0ABE, 0x0AC0).	% Other_Alphabetic # Mc   [3] GUJARATI VOWEL SIGN AA..GUJARATI VOWEL SIGN II
unicode_other_alphabetic(0x0AC1, 0x0AC5).	% Other_Alphabetic # Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
unicode_other_alphabetic(0x0AC7, 0x0AC8).	% Other_Alphabetic # Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
unicode_other_alphabetic(0x0AC9, 0x0AC9).	% Other_Alphabetic # Mc       GUJARATI VOWEL SIGN CANDRA O
unicode_other_alphabetic(0x0ACB, 0x0ACC).	% Other_Alphabetic # Mc   [2] GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU
unicode_other_alphabetic(0x0AE2, 0x0AE3).	% Other_Alphabetic # Mn   [2] GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL
unicode_other_alphabetic(0x0B01, 0x0B01).	% Other_Alphabetic # Mn       ORIYA SIGN CANDRABINDU
unicode_other_alphabetic(0x0B02, 0x0B03).	% Other_Alphabetic # Mc   [2] ORIYA SIGN ANUSVARA..ORIYA SIGN VISARGA
unicode_other_alphabetic(0x0B3E, 0x0B3E).	% Other_Alphabetic # Mc       ORIYA VOWEL SIGN AA
unicode_other_alphabetic(0x0B3F, 0x0B3F).	% Other_Alphabetic # Mn       ORIYA VOWEL SIGN I
unicode_other_alphabetic(0x0B40, 0x0B40).	% Other_Alphabetic # Mc       ORIYA VOWEL SIGN II
unicode_other_alphabetic(0x0B41, 0x0B44).	% Other_Alphabetic # Mn   [4] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR
unicode_other_alphabetic(0x0B47, 0x0B48).	% Other_Alphabetic # Mc   [2] ORIYA VOWEL SIGN E..ORIYA VOWEL SIGN AI
unicode_other_alphabetic(0x0B4B, 0x0B4C).	% Other_Alphabetic # Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
unicode_other_alphabetic(0x0B56, 0x0B56).	% Other_Alphabetic # Mn       ORIYA AI LENGTH MARK
unicode_other_alphabetic(0x0B57, 0x0B57).	% Other_Alphabetic # Mc       ORIYA AU LENGTH MARK
unicode_other_alphabetic(0x0B62, 0x0B63).	% Other_Alphabetic # Mn   [2] ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL
unicode_other_alphabetic(0x0B82, 0x0B82).	% Other_Alphabetic # Mn       TAMIL SIGN ANUSVARA
unicode_other_alphabetic(0x0BBE, 0x0BBF).	% Other_Alphabetic # Mc   [2] TAMIL VOWEL SIGN AA..TAMIL VOWEL SIGN I
unicode_other_alphabetic(0x0BC0, 0x0BC0).	% Other_Alphabetic # Mn       TAMIL VOWEL SIGN II
unicode_other_alphabetic(0x0BC1, 0x0BC2).	% Other_Alphabetic # Mc   [2] TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU
unicode_other_alphabetic(0x0BC6, 0x0BC8).	% Other_Alphabetic # Mc   [3] TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI
unicode_other_alphabetic(0x0BCA, 0x0BCC).	% Other_Alphabetic # Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_other_alphabetic(0x0BD7, 0x0BD7).	% Other_Alphabetic # Mc       TAMIL AU LENGTH MARK
unicode_other_alphabetic(0x0C01, 0x0C03).	% Other_Alphabetic # Mc   [3] TELUGU SIGN CANDRABINDU..TELUGU SIGN VISARGA
unicode_other_alphabetic(0x0C3E, 0x0C40).	% Other_Alphabetic # Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
unicode_other_alphabetic(0x0C41, 0x0C44).	% Other_Alphabetic # Mc   [4] TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR
unicode_other_alphabetic(0x0C46, 0x0C48).	% Other_Alphabetic # Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
unicode_other_alphabetic(0x0C4A, 0x0C4C).	% Other_Alphabetic # Mn   [3] TELUGU VOWEL SIGN O..TELUGU VOWEL SIGN AU
unicode_other_alphabetic(0x0C55, 0x0C56).	% Other_Alphabetic # Mn   [2] TELUGU LENGTH MARK..TELUGU AI LENGTH MARK
unicode_other_alphabetic(0x0C62, 0x0C63).	% Other_Alphabetic # Mn   [2] TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL
unicode_other_alphabetic(0x0C82, 0x0C83).	% Other_Alphabetic # Mc   [2] KANNADA SIGN ANUSVARA..KANNADA SIGN VISARGA
unicode_other_alphabetic(0x0CBE, 0x0CBE).	% Other_Alphabetic # Mc       KANNADA VOWEL SIGN AA
unicode_other_alphabetic(0x0CBF, 0x0CBF).	% Other_Alphabetic # Mn       KANNADA VOWEL SIGN I
unicode_other_alphabetic(0x0CC0, 0x0CC4).	% Other_Alphabetic # Mc   [5] KANNADA VOWEL SIGN II..KANNADA VOWEL SIGN VOCALIC RR
unicode_other_alphabetic(0x0CC6, 0x0CC6).	% Other_Alphabetic # Mn       KANNADA VOWEL SIGN E
unicode_other_alphabetic(0x0CC7, 0x0CC8).	% Other_Alphabetic # Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_other_alphabetic(0x0CCA, 0x0CCB).	% Other_Alphabetic # Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_other_alphabetic(0x0CCC, 0x0CCC).	% Other_Alphabetic # Mn       KANNADA VOWEL SIGN AU
unicode_other_alphabetic(0x0CD5, 0x0CD6).	% Other_Alphabetic # Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
unicode_other_alphabetic(0x0CE2, 0x0CE3).	% Other_Alphabetic # Mn   [2] KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL
unicode_other_alphabetic(0x0D02, 0x0D03).	% Other_Alphabetic # Mc   [2] MALAYALAM SIGN ANUSVARA..MALAYALAM SIGN VISARGA
unicode_other_alphabetic(0x0D3E, 0x0D40).	% Other_Alphabetic # Mc   [3] MALAYALAM VOWEL SIGN AA..MALAYALAM VOWEL SIGN II
unicode_other_alphabetic(0x0D41, 0x0D44).	% Other_Alphabetic # Mn   [4] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC RR
unicode_other_alphabetic(0x0D46, 0x0D48).	% Other_Alphabetic # Mc   [3] MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI
unicode_other_alphabetic(0x0D4A, 0x0D4C).	% Other_Alphabetic # Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_other_alphabetic(0x0D57, 0x0D57).	% Other_Alphabetic # Mc       MALAYALAM AU LENGTH MARK
unicode_other_alphabetic(0x0D62, 0x0D63).	% Other_Alphabetic # Mn   [2] MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL
unicode_other_alphabetic(0x0D82, 0x0D83).	% Other_Alphabetic # Mc   [2] SINHALA SIGN ANUSVARAYA..SINHALA SIGN VISARGAYA
unicode_other_alphabetic(0x0DCF, 0x0DD1).	% Other_Alphabetic # Mc   [3] SINHALA VOWEL SIGN AELA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA
unicode_other_alphabetic(0x0DD2, 0x0DD4).	% Other_Alphabetic # Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
unicode_other_alphabetic(0x0DD6, 0x0DD6).	% Other_Alphabetic # Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
unicode_other_alphabetic(0x0DD8, 0x0DDF).	% Other_Alphabetic # Mc   [8] SINHALA VOWEL SIGN GAETTA-PILLA..SINHALA VOWEL SIGN GAYANUKITTA
unicode_other_alphabetic(0x0DF2, 0x0DF3).	% Other_Alphabetic # Mc   [2] SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA
unicode_other_alphabetic(0x0E31, 0x0E31).	% Other_Alphabetic # Mn       THAI CHARACTER MAI HAN-AKAT
unicode_other_alphabetic(0x0E34, 0x0E3A).	% Other_Alphabetic # Mn   [7] THAI CHARACTER SARA I..THAI CHARACTER PHINTHU
unicode_other_alphabetic(0x0E4D, 0x0E4D).	% Other_Alphabetic # Mn       THAI CHARACTER NIKHAHIT
unicode_other_alphabetic(0x0EB1, 0x0EB1).	% Other_Alphabetic # Mn       LAO VOWEL SIGN MAI KAN
unicode_other_alphabetic(0x0EB4, 0x0EB9).	% Other_Alphabetic # Mn   [6] LAO VOWEL SIGN I..LAO VOWEL SIGN UU
unicode_other_alphabetic(0x0EBB, 0x0EBC).	% Other_Alphabetic # Mn   [2] LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO
unicode_other_alphabetic(0x0ECD, 0x0ECD).	% Other_Alphabetic # Mn       LAO NIGGAHITA
unicode_other_alphabetic(0x0F71, 0x0F7E).	% Other_Alphabetic # Mn  [14] TIBETAN VOWEL SIGN AA..TIBETAN SIGN RJES SU NGA RO
unicode_other_alphabetic(0x0F7F, 0x0F7F).	% Other_Alphabetic # Mc       TIBETAN SIGN RNAM BCAD
unicode_other_alphabetic(0x0F80, 0x0F81).	% Other_Alphabetic # Mn   [2] TIBETAN VOWEL SIGN REVERSED I..TIBETAN VOWEL SIGN REVERSED II
unicode_other_alphabetic(0x0F8D, 0x0F97).	% Other_Alphabetic # Mn  [11] TIBETAN SUBJOINED SIGN LCE TSA CAN..TIBETAN SUBJOINED LETTER JA
unicode_other_alphabetic(0x0F99, 0x0FBC).	% Other_Alphabetic # Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
unicode_other_alphabetic(0x102B, 0x102C).	% Other_Alphabetic # Mc   [2] MYANMAR VOWEL SIGN TALL AA..MYANMAR VOWEL SIGN AA
unicode_other_alphabetic(0x102D, 0x1030).	% Other_Alphabetic # Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
unicode_other_alphabetic(0x1031, 0x1031).	% Other_Alphabetic # Mc       MYANMAR VOWEL SIGN E
unicode_other_alphabetic(0x1032, 0x1036).	% Other_Alphabetic # Mn   [5] MYANMAR VOWEL SIGN AI..MYANMAR SIGN ANUSVARA
unicode_other_alphabetic(0x1038, 0x1038).	% Other_Alphabetic # Mc       MYANMAR SIGN VISARGA
unicode_other_alphabetic(0x103B, 0x103C).	% Other_Alphabetic # Mc   [2] MYANMAR CONSONANT SIGN MEDIAL YA..MYANMAR CONSONANT SIGN MEDIAL RA
unicode_other_alphabetic(0x103D, 0x103E).	% Other_Alphabetic # Mn   [2] MYANMAR CONSONANT SIGN MEDIAL WA..MYANMAR CONSONANT SIGN MEDIAL HA
unicode_other_alphabetic(0x1056, 0x1057).	% Other_Alphabetic # Mc   [2] MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR
unicode_other_alphabetic(0x1058, 0x1059).	% Other_Alphabetic # Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
unicode_other_alphabetic(0x105E, 0x1060).	% Other_Alphabetic # Mn   [3] MYANMAR CONSONANT SIGN MON MEDIAL NA..MYANMAR CONSONANT SIGN MON MEDIAL LA
unicode_other_alphabetic(0x1062, 0x1062).	% Other_Alphabetic # Mc       MYANMAR VOWEL SIGN SGAW KAREN EU
unicode_other_alphabetic(0x1067, 0x1068).	% Other_Alphabetic # Mc   [2] MYANMAR VOWEL SIGN WESTERN PWO KAREN EU..MYANMAR VOWEL SIGN WESTERN PWO KAREN UE
unicode_other_alphabetic(0x1071, 0x1074).	% Other_Alphabetic # Mn   [4] MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE
unicode_other_alphabetic(0x1082, 0x1082).	% Other_Alphabetic # Mn       MYANMAR CONSONANT SIGN SHAN MEDIAL WA
unicode_other_alphabetic(0x1083, 0x1084).	% Other_Alphabetic # Mc   [2] MYANMAR VOWEL SIGN SHAN AA..MYANMAR VOWEL SIGN SHAN E
unicode_other_alphabetic(0x1085, 0x1086).	% Other_Alphabetic # Mn   [2] MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y
unicode_other_alphabetic(0x109C, 0x109C).	% Other_Alphabetic # Mc       MYANMAR VOWEL SIGN AITON A
unicode_other_alphabetic(0x109D, 0x109D).	% Other_Alphabetic # Mn       MYANMAR VOWEL SIGN AITON AI
unicode_other_alphabetic(0x135F, 0x135F).	% Other_Alphabetic # Mn       ETHIOPIC COMBINING GEMINATION MARK
unicode_other_alphabetic(0x1712, 0x1713).	% Other_Alphabetic # Mn   [2] TAGALOG VOWEL SIGN I..TAGALOG VOWEL SIGN U
unicode_other_alphabetic(0x1732, 0x1733).	% Other_Alphabetic # Mn   [2] HANUNOO VOWEL SIGN I..HANUNOO VOWEL SIGN U
unicode_other_alphabetic(0x1752, 0x1753).	% Other_Alphabetic # Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
unicode_other_alphabetic(0x1772, 0x1773).	% Other_Alphabetic # Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
unicode_other_alphabetic(0x17B6, 0x17B6).	% Other_Alphabetic # Mc       KHMER VOWEL SIGN AA
unicode_other_alphabetic(0x17B7, 0x17BD).	% Other_Alphabetic # Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
unicode_other_alphabetic(0x17BE, 0x17C5).	% Other_Alphabetic # Mc   [8] KHMER VOWEL SIGN OE..KHMER VOWEL SIGN AU
unicode_other_alphabetic(0x17C6, 0x17C6).	% Other_Alphabetic # Mn       KHMER SIGN NIKAHIT
unicode_other_alphabetic(0x17C7, 0x17C8).	% Other_Alphabetic # Mc   [2] KHMER SIGN REAHMUK..KHMER SIGN YUUKALEAPINTU
unicode_other_alphabetic(0x18A9, 0x18A9).	% Other_Alphabetic # Mn       MONGOLIAN LETTER ALI GALI DAGALGA
unicode_other_alphabetic(0x1920, 0x1922).	% Other_Alphabetic # Mn   [3] LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN U
unicode_other_alphabetic(0x1923, 0x1926).	% Other_Alphabetic # Mc   [4] LIMBU VOWEL SIGN EE..LIMBU VOWEL SIGN AU
unicode_other_alphabetic(0x1927, 0x1928).	% Other_Alphabetic # Mn   [2] LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O
unicode_other_alphabetic(0x1929, 0x192B).	% Other_Alphabetic # Mc   [3] LIMBU SUBJOINED LETTER YA..LIMBU SUBJOINED LETTER WA
unicode_other_alphabetic(0x1930, 0x1931).	% Other_Alphabetic # Mc   [2] LIMBU SMALL LETTER KA..LIMBU SMALL LETTER NGA
unicode_other_alphabetic(0x1932, 0x1932).	% Other_Alphabetic # Mn       LIMBU SMALL LETTER ANUSVARA
unicode_other_alphabetic(0x1933, 0x1938).	% Other_Alphabetic # Mc   [6] LIMBU SMALL LETTER TA..LIMBU SMALL LETTER LA
unicode_other_alphabetic(0x19B0, 0x19C0).	% Other_Alphabetic # Mc  [17] NEW TAI LUE VOWEL SIGN VOWEL SHORTENER..NEW TAI LUE VOWEL SIGN IY
unicode_other_alphabetic(0x19C8, 0x19C9).	% Other_Alphabetic # Mc   [2] NEW TAI LUE TONE MARK-1..NEW TAI LUE TONE MARK-2
unicode_other_alphabetic(0x1A17, 0x1A18).	% Other_Alphabetic # Mn   [2] BUGINESE VOWEL SIGN I..BUGINESE VOWEL SIGN U
unicode_other_alphabetic(0x1A19, 0x1A1B).	% Other_Alphabetic # Mc   [3] BUGINESE VOWEL SIGN E..BUGINESE VOWEL SIGN AE
unicode_other_alphabetic(0x1A55, 0x1A55).	% Other_Alphabetic # Mc       TAI THAM CONSONANT SIGN MEDIAL RA
unicode_other_alphabetic(0x1A56, 0x1A56).	% Other_Alphabetic # Mn       TAI THAM CONSONANT SIGN MEDIAL LA
unicode_other_alphabetic(0x1A57, 0x1A57).	% Other_Alphabetic # Mc       TAI THAM CONSONANT SIGN LA TANG LAI
unicode_other_alphabetic(0x1A58, 0x1A5E).	% Other_Alphabetic # Mn   [7] TAI THAM SIGN MAI KANG LAI..TAI THAM CONSONANT SIGN SA
unicode_other_alphabetic(0x1A61, 0x1A61).	% Other_Alphabetic # Mc       TAI THAM VOWEL SIGN A
unicode_other_alphabetic(0x1A62, 0x1A62).	% Other_Alphabetic # Mn       TAI THAM VOWEL SIGN MAI SAT
unicode_other_alphabetic(0x1A63, 0x1A64).	% Other_Alphabetic # Mc   [2] TAI THAM VOWEL SIGN AA..TAI THAM VOWEL SIGN TALL AA
unicode_other_alphabetic(0x1A65, 0x1A6C).	% Other_Alphabetic # Mn   [8] TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN OA BELOW
unicode_other_alphabetic(0x1A6D, 0x1A72).	% Other_Alphabetic # Mc   [6] TAI THAM VOWEL SIGN OY..TAI THAM VOWEL SIGN THAM AI
unicode_other_alphabetic(0x1A73, 0x1A74).	% Other_Alphabetic # Mn   [2] TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN MAI KANG
unicode_other_alphabetic(0x1B00, 0x1B03).	% Other_Alphabetic # Mn   [4] BALINESE SIGN ULU RICEM..BALINESE SIGN SURANG
unicode_other_alphabetic(0x1B04, 0x1B04).	% Other_Alphabetic # Mc       BALINESE SIGN BISAH
unicode_other_alphabetic(0x1B35, 0x1B35).	% Other_Alphabetic # Mc       BALINESE VOWEL SIGN TEDUNG
unicode_other_alphabetic(0x1B36, 0x1B3A).	% Other_Alphabetic # Mn   [5] BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN RA REPA
unicode_other_alphabetic(0x1B3B, 0x1B3B).	% Other_Alphabetic # Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_other_alphabetic(0x1B3C, 0x1B3C).	% Other_Alphabetic # Mn       BALINESE VOWEL SIGN LA LENGA
unicode_other_alphabetic(0x1B3D, 0x1B41).	% Other_Alphabetic # Mc   [5] BALINESE VOWEL SIGN LA LENGA TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG
unicode_other_alphabetic(0x1B42, 0x1B42).	% Other_Alphabetic # Mn       BALINESE VOWEL SIGN PEPET
unicode_other_alphabetic(0x1B43, 0x1B43).	% Other_Alphabetic # Mc       BALINESE VOWEL SIGN PEPET TEDUNG
unicode_other_alphabetic(0x1B80, 0x1B81).	% Other_Alphabetic # Mn   [2] SUNDANESE SIGN PANYECEK..SUNDANESE SIGN PANGLAYAR
unicode_other_alphabetic(0x1B82, 0x1B82).	% Other_Alphabetic # Mc       SUNDANESE SIGN PANGWISAD
unicode_other_alphabetic(0x1BA1, 0x1BA1).	% Other_Alphabetic # Mc       SUNDANESE CONSONANT SIGN PAMINGKAL
unicode_other_alphabetic(0x1BA2, 0x1BA5).	% Other_Alphabetic # Mn   [4] SUNDANESE CONSONANT SIGN PANYAKRA..SUNDANESE VOWEL SIGN PANYUKU
unicode_other_alphabetic(0x1BA6, 0x1BA7).	% Other_Alphabetic # Mc   [2] SUNDANESE VOWEL SIGN PANAELAENG..SUNDANESE VOWEL SIGN PANOLONG
unicode_other_alphabetic(0x1BA8, 0x1BA9).	% Other_Alphabetic # Mn   [2] SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG
unicode_other_alphabetic(0x1BAC, 0x1BAD).	% Other_Alphabetic # Mc   [2] SUNDANESE CONSONANT SIGN PASANGAN MA..SUNDANESE CONSONANT SIGN PASANGAN WA
unicode_other_alphabetic(0x1BE7, 0x1BE7).	% Other_Alphabetic # Mc       BATAK VOWEL SIGN E
unicode_other_alphabetic(0x1BE8, 0x1BE9).	% Other_Alphabetic # Mn   [2] BATAK VOWEL SIGN PAKPAK E..BATAK VOWEL SIGN EE
unicode_other_alphabetic(0x1BEA, 0x1BEC).	% Other_Alphabetic # Mc   [3] BATAK VOWEL SIGN I..BATAK VOWEL SIGN O
unicode_other_alphabetic(0x1BED, 0x1BED).	% Other_Alphabetic # Mn       BATAK VOWEL SIGN KARO O
unicode_other_alphabetic(0x1BEE, 0x1BEE).	% Other_Alphabetic # Mc       BATAK VOWEL SIGN U
unicode_other_alphabetic(0x1BEF, 0x1BF1).	% Other_Alphabetic # Mn   [3] BATAK VOWEL SIGN U FOR SIMALUNGUN SA..BATAK CONSONANT SIGN H
unicode_other_alphabetic(0x1C24, 0x1C2B).	% Other_Alphabetic # Mc   [8] LEPCHA SUBJOINED LETTER YA..LEPCHA VOWEL SIGN UU
unicode_other_alphabetic(0x1C2C, 0x1C33).	% Other_Alphabetic # Mn   [8] LEPCHA VOWEL SIGN E..LEPCHA CONSONANT SIGN T
unicode_other_alphabetic(0x1C34, 0x1C35).	% Other_Alphabetic # Mc   [2] LEPCHA CONSONANT SIGN NYIN-DO..LEPCHA CONSONANT SIGN KANG
unicode_other_alphabetic(0x1CF2, 0x1CF3).	% Other_Alphabetic # Mc   [2] VEDIC SIGN ARDHAVISARGA..VEDIC SIGN ROTATED ARDHAVISARGA
unicode_other_alphabetic(0x24B6, 0x24E9).	% Other_Alphabetic # So  [52] CIRCLED LATIN CAPITAL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_other_alphabetic(0x2DE0, 0x2DFF).	% Other_Alphabetic # Mn  [32] COMBINING CYRILLIC LETTER BE..COMBINING CYRILLIC LETTER IOTIFIED BIG YUS
unicode_other_alphabetic(0xA674, 0xA67B).	% Other_Alphabetic # Mn   [8] COMBINING CYRILLIC LETTER UKRAINIAN IE..COMBINING CYRILLIC LETTER OMEGA
unicode_other_alphabetic(0xA69F, 0xA69F).	% Other_Alphabetic # Mn       COMBINING CYRILLIC LETTER IOTIFIED E
unicode_other_alphabetic(0xA823, 0xA824).	% Other_Alphabetic # Mc   [2] SYLOTI NAGRI VOWEL SIGN A..SYLOTI NAGRI VOWEL SIGN I
unicode_other_alphabetic(0xA825, 0xA826).	% Other_Alphabetic # Mn   [2] SYLOTI NAGRI VOWEL SIGN U..SYLOTI NAGRI VOWEL SIGN E
unicode_other_alphabetic(0xA827, 0xA827).	% Other_Alphabetic # Mc       SYLOTI NAGRI VOWEL SIGN OO
unicode_other_alphabetic(0xA880, 0xA881).	% Other_Alphabetic # Mc   [2] SAURASHTRA SIGN ANUSVARA..SAURASHTRA SIGN VISARGA
unicode_other_alphabetic(0xA8B4, 0xA8C3).	% Other_Alphabetic # Mc  [16] SAURASHTRA CONSONANT SIGN HAARU..SAURASHTRA VOWEL SIGN AU
unicode_other_alphabetic(0xA926, 0xA92A).	% Other_Alphabetic # Mn   [5] KAYAH LI VOWEL UE..KAYAH LI VOWEL O
unicode_other_alphabetic(0xA947, 0xA951).	% Other_Alphabetic # Mn  [11] REJANG VOWEL SIGN I..REJANG CONSONANT SIGN R
unicode_other_alphabetic(0xA952, 0xA952).	% Other_Alphabetic # Mc       REJANG CONSONANT SIGN H
unicode_other_alphabetic(0xA980, 0xA982).	% Other_Alphabetic # Mn   [3] JAVANESE SIGN PANYANGGA..JAVANESE SIGN LAYAR
unicode_other_alphabetic(0xA983, 0xA983).	% Other_Alphabetic # Mc       JAVANESE SIGN WIGNYAN
unicode_other_alphabetic(0xA9B4, 0xA9B5).	% Other_Alphabetic # Mc   [2] JAVANESE VOWEL SIGN TARUNG..JAVANESE VOWEL SIGN TOLONG
unicode_other_alphabetic(0xA9B6, 0xA9B9).	% Other_Alphabetic # Mn   [4] JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN SUKU MENDUT
unicode_other_alphabetic(0xA9BA, 0xA9BB).	% Other_Alphabetic # Mc   [2] JAVANESE VOWEL SIGN TALING..JAVANESE VOWEL SIGN DIRGA MURE
unicode_other_alphabetic(0xA9BC, 0xA9BC).	% Other_Alphabetic # Mn       JAVANESE VOWEL SIGN PEPET
unicode_other_alphabetic(0xA9BD, 0xA9BF).	% Other_Alphabetic # Mc   [3] JAVANESE CONSONANT SIGN KERET..JAVANESE CONSONANT SIGN CAKRA
unicode_other_alphabetic(0xAA29, 0xAA2E).	% Other_Alphabetic # Mn   [6] CHAM VOWEL SIGN AA..CHAM VOWEL SIGN OE
unicode_other_alphabetic(0xAA2F, 0xAA30).	% Other_Alphabetic # Mc   [2] CHAM VOWEL SIGN O..CHAM VOWEL SIGN AI
unicode_other_alphabetic(0xAA31, 0xAA32).	% Other_Alphabetic # Mn   [2] CHAM VOWEL SIGN AU..CHAM VOWEL SIGN UE
unicode_other_alphabetic(0xAA33, 0xAA34).	% Other_Alphabetic # Mc   [2] CHAM CONSONANT SIGN YA..CHAM CONSONANT SIGN RA
unicode_other_alphabetic(0xAA35, 0xAA36).	% Other_Alphabetic # Mn   [2] CHAM CONSONANT SIGN LA..CHAM CONSONANT SIGN WA
unicode_other_alphabetic(0xAA43, 0xAA43).	% Other_Alphabetic # Mn       CHAM CONSONANT SIGN FINAL NG
unicode_other_alphabetic(0xAA4C, 0xAA4C).	% Other_Alphabetic # Mn       CHAM CONSONANT SIGN FINAL M
unicode_other_alphabetic(0xAA4D, 0xAA4D).	% Other_Alphabetic # Mc       CHAM CONSONANT SIGN FINAL H
unicode_other_alphabetic(0xAAB0, 0xAAB0).	% Other_Alphabetic # Mn       TAI VIET MAI KANG
unicode_other_alphabetic(0xAAB2, 0xAAB4).	% Other_Alphabetic # Mn   [3] TAI VIET VOWEL I..TAI VIET VOWEL U
unicode_other_alphabetic(0xAAB7, 0xAAB8).	% Other_Alphabetic # Mn   [2] TAI VIET MAI KHIT..TAI VIET VOWEL IA
unicode_other_alphabetic(0xAABE, 0xAABE).	% Other_Alphabetic # Mn       TAI VIET VOWEL AM
unicode_other_alphabetic(0xAAEB, 0xAAEB).	% Other_Alphabetic # Mc       MEETEI MAYEK VOWEL SIGN II
unicode_other_alphabetic(0xAAEC, 0xAAED).	% Other_Alphabetic # Mn   [2] MEETEI MAYEK VOWEL SIGN UU..MEETEI MAYEK VOWEL SIGN AAI
unicode_other_alphabetic(0xAAEE, 0xAAEF).	% Other_Alphabetic # Mc   [2] MEETEI MAYEK VOWEL SIGN AU..MEETEI MAYEK VOWEL SIGN AAU
unicode_other_alphabetic(0xAAF5, 0xAAF5).	% Other_Alphabetic # Mc       MEETEI MAYEK VOWEL SIGN VISARGA
unicode_other_alphabetic(0xABE3, 0xABE4).	% Other_Alphabetic # Mc   [2] MEETEI MAYEK VOWEL SIGN ONAP..MEETEI MAYEK VOWEL SIGN INAP
unicode_other_alphabetic(0xABE5, 0xABE5).	% Other_Alphabetic # Mn       MEETEI MAYEK VOWEL SIGN ANAP
unicode_other_alphabetic(0xABE6, 0xABE7).	% Other_Alphabetic # Mc   [2] MEETEI MAYEK VOWEL SIGN YENAP..MEETEI MAYEK VOWEL SIGN SOUNAP
unicode_other_alphabetic(0xABE8, 0xABE8).	% Other_Alphabetic # Mn       MEETEI MAYEK VOWEL SIGN UNAP
unicode_other_alphabetic(0xABE9, 0xABEA).	% Other_Alphabetic # Mc   [2] MEETEI MAYEK VOWEL SIGN CHEINAP..MEETEI MAYEK VOWEL SIGN NUNG
unicode_other_alphabetic(0xFB1E, 0xFB1E).	% Other_Alphabetic # Mn       HEBREW POINT JUDEO-SPANISH VARIKA
unicode_other_alphabetic(0x10A01, 0x10A03).	% Other_Alphabetic # Mn   [3] KHAROSHTHI VOWEL SIGN I..KHAROSHTHI VOWEL SIGN VOCALIC R
unicode_other_alphabetic(0x10A05, 0x10A06).	% Other_Alphabetic # Mn   [2] KHAROSHTHI VOWEL SIGN E..KHAROSHTHI VOWEL SIGN O
unicode_other_alphabetic(0x10A0C, 0x10A0F).	% Other_Alphabetic # Mn   [4] KHAROSHTHI VOWEL LENGTH MARK..KHAROSHTHI SIGN VISARGA
unicode_other_alphabetic(0x11000, 0x11000).	% Other_Alphabetic # Mc       BRAHMI SIGN CANDRABINDU
unicode_other_alphabetic(0x11001, 0x11001).	% Other_Alphabetic # Mn       BRAHMI SIGN ANUSVARA
unicode_other_alphabetic(0x11002, 0x11002).	% Other_Alphabetic # Mc       BRAHMI SIGN VISARGA
unicode_other_alphabetic(0x11038, 0x11045).	% Other_Alphabetic # Mn  [14] BRAHMI VOWEL SIGN AA..BRAHMI VOWEL SIGN AU
unicode_other_alphabetic(0x11082, 0x11082).	% Other_Alphabetic # Mc       KAITHI SIGN VISARGA
unicode_other_alphabetic(0x110B0, 0x110B2).	% Other_Alphabetic # Mc   [3] KAITHI VOWEL SIGN AA..KAITHI VOWEL SIGN II
unicode_other_alphabetic(0x110B3, 0x110B6).	% Other_Alphabetic # Mn   [4] KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN AI
unicode_other_alphabetic(0x110B7, 0x110B8).	% Other_Alphabetic # Mc   [2] KAITHI VOWEL SIGN O..KAITHI VOWEL SIGN AU
unicode_other_alphabetic(0x11100, 0x11102).	% Other_Alphabetic # Mn   [3] CHAKMA SIGN CANDRABINDU..CHAKMA SIGN VISARGA
unicode_other_alphabetic(0x11127, 0x1112B).	% Other_Alphabetic # Mn   [5] CHAKMA VOWEL SIGN A..CHAKMA VOWEL SIGN UU
unicode_other_alphabetic(0x1112C, 0x1112C).	% Other_Alphabetic # Mc       CHAKMA VOWEL SIGN E
unicode_other_alphabetic(0x1112D, 0x11132).	% Other_Alphabetic # Mn   [6] CHAKMA VOWEL SIGN AI..CHAKMA AU MARK
unicode_other_alphabetic(0x11180, 0x11181).	% Other_Alphabetic # Mn   [2] SHARADA SIGN CANDRABINDU..SHARADA SIGN ANUSVARA
unicode_other_alphabetic(0x11182, 0x11182).	% Other_Alphabetic # Mc       SHARADA SIGN VISARGA
unicode_other_alphabetic(0x111B3, 0x111B5).	% Other_Alphabetic # Mc   [3] SHARADA VOWEL SIGN AA..SHARADA VOWEL SIGN II
unicode_other_alphabetic(0x111B6, 0x111BE).	% Other_Alphabetic # Mn   [9] SHARADA VOWEL SIGN U..SHARADA VOWEL SIGN O
unicode_other_alphabetic(0x111BF, 0x111BF).	% Other_Alphabetic # Mc       SHARADA VOWEL SIGN AU
unicode_other_alphabetic(0x116AB, 0x116AB).	% Other_Alphabetic # Mn       TAKRI SIGN ANUSVARA
unicode_other_alphabetic(0x116AC, 0x116AC).	% Other_Alphabetic # Mc       TAKRI SIGN VISARGA
unicode_other_alphabetic(0x116AD, 0x116AD).	% Other_Alphabetic # Mn       TAKRI VOWEL SIGN AA
unicode_other_alphabetic(0x116AE, 0x116AF).	% Other_Alphabetic # Mc   [2] TAKRI VOWEL SIGN I..TAKRI VOWEL SIGN II
unicode_other_alphabetic(0x116B0, 0x116B5).	% Other_Alphabetic # Mn   [6] TAKRI VOWEL SIGN U..TAKRI VOWEL SIGN AU
unicode_other_alphabetic(0x16F51, 0x16F7E).	% Other_Alphabetic # Mc  [46] MIAO SIGN ASPIRATION..MIAO VOWEL SIGN NG

% Total code points: 922
