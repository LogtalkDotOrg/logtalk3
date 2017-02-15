%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 28, 2012
%
%  Original Unicode file header comments follow

/*
# DerivedJoiningType-6.1.0.txt
# Date: 2011-11-27, 05:10:23 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

# ================================================

#	Type T is derived, as described in ArabicShaping.txt

#  All code points not explicitly listed for Joining_Type
#  have the value Non_Joining (U).

# @missing: 0000..10FFFF; Non_Joining
*/

% ================================================

unicode_joining_type(CodePoint, Type) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_joining_type(CodePointStart, CodePointEnd, Type),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_joining_type(CodePoint, _, CodePointType) ->
		Type = CodePointType
	;	% look for a code point range that includes the given code point
		unicode_joining_type(CodePointStart, CodePointEnd, CodePointType),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Type = CodePointType
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Type = 'U'
	).

% Joining_Type=Join_Causing

unicode_joining_type(0x0640, 0x0640, 'C'). % Lm       ARABIC TATWEEL
unicode_joining_type(0x07FA, 0x07FA, 'C'). % Lm       NKO LAJANYALAN
unicode_joining_type(0x200D, 0x200D, 'C'). % Cf       ZERO WIDTH JOINER

% Total code points: 3

% ================================================

% Joining_Type=Dual_Joining

unicode_joining_type(0x0620, 0x0620, 'D'). % Lo       ARABIC LETTER KASHMIRI YEH
unicode_joining_type(0x0626, 0x0626, 'D'). % Lo       ARABIC LETTER YEH WITH HAMZA ABOVE
unicode_joining_type(0x0628, 0x0628, 'D'). % Lo       ARABIC LETTER BEH
unicode_joining_type(0x062A, 0x062E, 'D'). % Lo   [5] ARABIC LETTER TEH..ARABIC LETTER KHAH
unicode_joining_type(0x0633, 0x063F, 'D'). % Lo  [13] ARABIC LETTER SEEN..ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE
unicode_joining_type(0x0641, 0x0647, 'D'). % Lo   [7] ARABIC LETTER FEH..ARABIC LETTER HEH
unicode_joining_type(0x0649, 0x064A, 'D'). % Lo   [2] ARABIC LETTER ALEF MAKSURA..ARABIC LETTER YEH
unicode_joining_type(0x066E, 0x066F, 'D'). % Lo   [2] ARABIC LETTER DOTLESS BEH..ARABIC LETTER DOTLESS QAF
unicode_joining_type(0x0678, 0x0687, 'D'). % Lo  [16] ARABIC LETTER HIGH HAMZA YEH..ARABIC LETTER TCHEHEH
unicode_joining_type(0x069A, 0x06BF, 'D'). % Lo  [38] ARABIC LETTER SEEN WITH DOT BELOW AND DOT ABOVE..ARABIC LETTER TCHEH WITH DOT ABOVE
unicode_joining_type(0x06C1, 0x06C2, 'D'). % Lo   [2] ARABIC LETTER HEH GOAL..ARABIC LETTER HEH GOAL WITH HAMZA ABOVE
unicode_joining_type(0x06CC, 0x06CC, 'D'). % Lo       ARABIC LETTER FARSI YEH
unicode_joining_type(0x06CE, 0x06CE, 'D'). % Lo       ARABIC LETTER YEH WITH SMALL V
unicode_joining_type(0x06D0, 0x06D1, 'D'). % Lo   [2] ARABIC LETTER E..ARABIC LETTER YEH WITH THREE DOTS BELOW
unicode_joining_type(0x06FA, 0x06FC, 'D'). % Lo   [3] ARABIC LETTER SHEEN WITH DOT BELOW..ARABIC LETTER GHAIN WITH DOT BELOW
unicode_joining_type(0x06FF, 0x06FF, 'D'). % Lo       ARABIC LETTER HEH WITH INVERTED V
unicode_joining_type(0x0712, 0x0714, 'D'). % Lo   [3] SYRIAC LETTER BETH..SYRIAC LETTER GAMAL GARSHUNI
unicode_joining_type(0x071A, 0x071D, 'D'). % Lo   [4] SYRIAC LETTER HETH..SYRIAC LETTER YUDH
unicode_joining_type(0x071F, 0x0727, 'D'). % Lo   [9] SYRIAC LETTER KAPH..SYRIAC LETTER REVERSED PE
unicode_joining_type(0x0729, 0x0729, 'D'). % Lo       SYRIAC LETTER QAPH
unicode_joining_type(0x072B, 0x072B, 'D'). % Lo       SYRIAC LETTER SHIN
unicode_joining_type(0x072D, 0x072E, 'D'). % Lo   [2] SYRIAC LETTER PERSIAN BHETH..SYRIAC LETTER PERSIAN GHAMAL
unicode_joining_type(0x074E, 0x0758, 'D'). % Lo  [11] SYRIAC LETTER SOGDIAN KHAPH..ARABIC LETTER HAH WITH THREE DOTS POINTING UPWARDS BELOW
unicode_joining_type(0x075C, 0x076A, 'D'). % Lo  [15] ARABIC LETTER SEEN WITH FOUR DOTS ABOVE..ARABIC LETTER LAM WITH BAR
unicode_joining_type(0x076D, 0x0770, 'D'). % Lo   [4] ARABIC LETTER SEEN WITH TWO DOTS VERTICALLY ABOVE..ARABIC LETTER SEEN WITH SMALL ARABIC LETTER TAH AND TWO DOTS
unicode_joining_type(0x0772, 0x0772, 'D'). % Lo       ARABIC LETTER HAH WITH SMALL ARABIC LETTER TAH ABOVE
unicode_joining_type(0x0775, 0x0777, 'D'). % Lo   [3] ARABIC LETTER FARSI YEH WITH EXTENDED ARABIC-INDIC DIGIT TWO ABOVE..ARABIC LETTER FARSI YEH WITH EXTENDED ARABIC-INDIC DIGIT FOUR BELOW
unicode_joining_type(0x077A, 0x077F, 'D'). % Lo   [6] ARABIC LETTER YEH BARREE WITH EXTENDED ARABIC-INDIC DIGIT TWO ABOVE..ARABIC LETTER KAF WITH TWO DOTS ABOVE
unicode_joining_type(0x07CA, 0x07EA, 'D'). % Lo  [33] NKO LETTER A..NKO LETTER JONA RA
unicode_joining_type(0x0841, 0x0845, 'D'). % Lo   [5] MANDAIC LETTER AB..MANDAIC LETTER USHENNA
unicode_joining_type(0x0847, 0x0848, 'D'). % Lo   [2] MANDAIC LETTER IT..MANDAIC LETTER ATT
unicode_joining_type(0x084A, 0x084E, 'D'). % Lo   [5] MANDAIC LETTER AK..MANDAIC LETTER AS
unicode_joining_type(0x0850, 0x0853, 'D'). % Lo   [4] MANDAIC LETTER AP..MANDAIC LETTER AR
unicode_joining_type(0x0855, 0x0855, 'D'). % Lo       MANDAIC LETTER AT
unicode_joining_type(0x08A0, 0x08A0, 'D'). % Lo       ARABIC LETTER BEH WITH SMALL V BELOW
unicode_joining_type(0x08A2, 0x08A9, 'D'). % Lo   [8] ARABIC LETTER JEEM WITH TWO DOTS ABOVE..ARABIC LETTER YEH WITH TWO DOTS BELOW AND DOT ABOVE

% Total code points: 215

% ================================================

% Joining_Type=Right_Joining

unicode_joining_type(0x0622, 0x0625, 'R'). % Lo   [4] ARABIC LETTER ALEF WITH MADDA ABOVE..ARABIC LETTER ALEF WITH HAMZA BELOW
unicode_joining_type(0x0627, 0x0627, 'R'). % Lo       ARABIC LETTER ALEF
unicode_joining_type(0x0629, 0x0629, 'R'). % Lo       ARABIC LETTER TEH MARBUTA
unicode_joining_type(0x062F, 0x0632, 'R'). % Lo   [4] ARABIC LETTER DAL..ARABIC LETTER ZAIN
unicode_joining_type(0x0648, 0x0648, 'R'). % Lo       ARABIC LETTER WAW
unicode_joining_type(0x0671, 0x0673, 'R'). % Lo   [3] ARABIC LETTER ALEF WASLA..ARABIC LETTER ALEF WITH WAVY HAMZA BELOW
unicode_joining_type(0x0675, 0x0677, 'R'). % Lo   [3] ARABIC LETTER HIGH HAMZA ALEF..ARABIC LETTER U WITH HAMZA ABOVE
unicode_joining_type(0x0688, 0x0699, 'R'). % Lo  [18] ARABIC LETTER DDAL..ARABIC LETTER REH WITH FOUR DOTS ABOVE
unicode_joining_type(0x06C0, 0x06C0, 'R'). % Lo       ARABIC LETTER HEH WITH YEH ABOVE
unicode_joining_type(0x06C3, 0x06CB, 'R'). % Lo   [9] ARABIC LETTER TEH MARBUTA GOAL..ARABIC LETTER VE
unicode_joining_type(0x06CD, 0x06CD, 'R'). % Lo       ARABIC LETTER YEH WITH TAIL
unicode_joining_type(0x06CF, 0x06CF, 'R'). % Lo       ARABIC LETTER WAW WITH DOT ABOVE
unicode_joining_type(0x06D2, 0x06D3, 'R'). % Lo   [2] ARABIC LETTER YEH BARREE..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
unicode_joining_type(0x06D5, 0x06D5, 'R'). % Lo       ARABIC LETTER AE
unicode_joining_type(0x06EE, 0x06EF, 'R'). % Lo   [2] ARABIC LETTER DAL WITH INVERTED V..ARABIC LETTER REH WITH INVERTED V
unicode_joining_type(0x0710, 0x0710, 'R'). % Lo       SYRIAC LETTER ALAPH
unicode_joining_type(0x0715, 0x0719, 'R'). % Lo   [5] SYRIAC LETTER DALATH..SYRIAC LETTER ZAIN
unicode_joining_type(0x071E, 0x071E, 'R'). % Lo       SYRIAC LETTER YUDH HE
unicode_joining_type(0x0728, 0x0728, 'R'). % Lo       SYRIAC LETTER SADHE
unicode_joining_type(0x072A, 0x072A, 'R'). % Lo       SYRIAC LETTER RISH
unicode_joining_type(0x072C, 0x072C, 'R'). % Lo       SYRIAC LETTER TAW
unicode_joining_type(0x072F, 0x072F, 'R'). % Lo       SYRIAC LETTER PERSIAN DHALATH
unicode_joining_type(0x074D, 0x074D, 'R'). % Lo       SYRIAC LETTER SOGDIAN ZHAIN
unicode_joining_type(0x0759, 0x075B, 'R'). % Lo   [3] ARABIC LETTER DAL WITH TWO DOTS VERTICALLY BELOW AND SMALL TAH..ARABIC LETTER REH WITH STROKE
unicode_joining_type(0x076B, 0x076C, 'R'). % Lo   [2] ARABIC LETTER REH WITH TWO DOTS VERTICALLY ABOVE..ARABIC LETTER REH WITH HAMZA ABOVE
unicode_joining_type(0x0771, 0x0771, 'R'). % Lo       ARABIC LETTER REH WITH SMALL ARABIC LETTER TAH AND TWO DOTS
unicode_joining_type(0x0773, 0x0774, 'R'). % Lo   [2] ARABIC LETTER ALEF WITH EXTENDED ARABIC-INDIC DIGIT TWO ABOVE..ARABIC LETTER ALEF WITH EXTENDED ARABIC-INDIC DIGIT THREE ABOVE
unicode_joining_type(0x0778, 0x0779, 'R'). % Lo   [2] ARABIC LETTER WAW WITH EXTENDED ARABIC-INDIC DIGIT TWO ABOVE..ARABIC LETTER WAW WITH EXTENDED ARABIC-INDIC DIGIT THREE ABOVE
unicode_joining_type(0x0840, 0x0840, 'R'). % Lo       MANDAIC LETTER HALQA
unicode_joining_type(0x0846, 0x0846, 'R'). % Lo       MANDAIC LETTER AZ
unicode_joining_type(0x0849, 0x0849, 'R'). % Lo       MANDAIC LETTER AKSA
unicode_joining_type(0x084F, 0x084F, 'R'). % Lo       MANDAIC LETTER IN
unicode_joining_type(0x0854, 0x0854, 'R'). % Lo       MANDAIC LETTER ASH
unicode_joining_type(0x08AA, 0x08AC, 'R'). % Lo   [3] ARABIC LETTER REH WITH LOOP..ARABIC LETTER ROHINGYA YEH

% Total code points: 82

% ================================================

% Joining_Type=Transparent

unicode_joining_type(0x00AD, 0x00AD, 'T'). % Cf       SOFT HYPHEN
unicode_joining_type(0x0300, 0x036F, 'T'). % Mn [112] COMBINING GRAVE ACCENT..COMBINING LATIN SMALL LETTER X
unicode_joining_type(0x0483, 0x0487, 'T'). % Mn   [5] COMBINING CYRILLIC TITLO..COMBINING CYRILLIC POKRYTIE
unicode_joining_type(0x0488, 0x0489, 'T'). % Me   [2] COMBINING CYRILLIC HUNDRED THOUSANDS SIGN..COMBINING CYRILLIC MILLIONS SIGN
unicode_joining_type(0x0591, 0x05BD, 'T'). % Mn  [45] HEBREW ACCENT ETNAHTA..HEBREW POINT METEG
unicode_joining_type(0x05BF, 0x05BF, 'T'). % Mn       HEBREW POINT RAFE
unicode_joining_type(0x05C1, 0x05C2, 'T'). % Mn   [2] HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT
unicode_joining_type(0x05C4, 0x05C5, 'T'). % Mn   [2] HEBREW MARK UPPER DOT..HEBREW MARK LOWER DOT
unicode_joining_type(0x05C7, 0x05C7, 'T'). % Mn       HEBREW POINT QAMATS QATAN
unicode_joining_type(0x0610, 0x061A, 'T'). % Mn  [11] ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM..ARABIC SMALL KASRA
unicode_joining_type(0x064B, 0x065F, 'T'). % Mn  [21] ARABIC FATHATAN..ARABIC WAVY HAMZA BELOW
unicode_joining_type(0x0670, 0x0670, 'T'). % Mn       ARABIC LETTER SUPERSCRIPT ALEF
unicode_joining_type(0x06D6, 0x06DC, 'T'). % Mn   [7] ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN
unicode_joining_type(0x06DF, 0x06E4, 'T'). % Mn   [6] ARABIC SMALL HIGH ROUNDED ZERO..ARABIC SMALL HIGH MADDA
unicode_joining_type(0x06E7, 0x06E8, 'T'). % Mn   [2] ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON
unicode_joining_type(0x06EA, 0x06ED, 'T'). % Mn   [4] ARABIC EMPTY CENTRE LOW STOP..ARABIC SMALL LOW MEEM
unicode_joining_type(0x070F, 0x070F, 'T'). % Cf       SYRIAC ABBREVIATION MARK
unicode_joining_type(0x0711, 0x0711, 'T'). % Mn       SYRIAC LETTER SUPERSCRIPT ALAPH
unicode_joining_type(0x0730, 0x074A, 'T'). % Mn  [27] SYRIAC PTHAHA ABOVE..SYRIAC BARREKH
unicode_joining_type(0x07A6, 0x07B0, 'T'). % Mn  [11] THAANA ABAFILI..THAANA SUKUN
unicode_joining_type(0x07EB, 0x07F3, 'T'). % Mn   [9] NKO COMBINING SHORT HIGH TONE..NKO COMBINING DOUBLE DOT ABOVE
unicode_joining_type(0x0816, 0x0819, 'T'). % Mn   [4] SAMARITAN MARK IN..SAMARITAN MARK DAGESH
unicode_joining_type(0x081B, 0x0823, 'T'). % Mn   [9] SAMARITAN MARK EPENTHETIC YUT..SAMARITAN VOWEL SIGN A
unicode_joining_type(0x0825, 0x0827, 'T'). % Mn   [3] SAMARITAN VOWEL SIGN SHORT A..SAMARITAN VOWEL SIGN U
unicode_joining_type(0x0829, 0x082D, 'T'). % Mn   [5] SAMARITAN VOWEL SIGN LONG I..SAMARITAN MARK NEQUDAA
unicode_joining_type(0x0859, 0x085B, 'T'). % Mn   [3] MANDAIC AFFRICATION MARK..MANDAIC GEMINATION MARK
unicode_joining_type(0x08E4, 0x08FE, 'T'). % Mn  [27] ARABIC CURLY FATHA..ARABIC DAMMA WITH DOT
unicode_joining_type(0x0900, 0x0902, 'T'). % Mn   [3] DEVANAGARI SIGN INVERTED CANDRABINDU..DEVANAGARI SIGN ANUSVARA
unicode_joining_type(0x093A, 0x093A, 'T'). % Mn       DEVANAGARI VOWEL SIGN OE
unicode_joining_type(0x093C, 0x093C, 'T'). % Mn       DEVANAGARI SIGN NUKTA
unicode_joining_type(0x0941, 0x0948, 'T'). % Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
unicode_joining_type(0x094D, 0x094D, 'T'). % Mn       DEVANAGARI SIGN VIRAMA
unicode_joining_type(0x0951, 0x0957, 'T'). % Mn   [7] DEVANAGARI STRESS SIGN UDATTA..DEVANAGARI VOWEL SIGN UUE
unicode_joining_type(0x0962, 0x0963, 'T'). % Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
unicode_joining_type(0x0981, 0x0981, 'T'). % Mn       BENGALI SIGN CANDRABINDU
unicode_joining_type(0x09BC, 0x09BC, 'T'). % Mn       BENGALI SIGN NUKTA
unicode_joining_type(0x09C1, 0x09C4, 'T'). % Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
unicode_joining_type(0x09CD, 0x09CD, 'T'). % Mn       BENGALI SIGN VIRAMA
unicode_joining_type(0x09E2, 0x09E3, 'T'). % Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
unicode_joining_type(0x0A01, 0x0A02, 'T'). % Mn   [2] GURMUKHI SIGN ADAK BINDI..GURMUKHI SIGN BINDI
unicode_joining_type(0x0A3C, 0x0A3C, 'T'). % Mn       GURMUKHI SIGN NUKTA
unicode_joining_type(0x0A41, 0x0A42, 'T'). % Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
unicode_joining_type(0x0A47, 0x0A48, 'T'). % Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
unicode_joining_type(0x0A4B, 0x0A4D, 'T'). % Mn   [3] GURMUKHI VOWEL SIGN OO..GURMUKHI SIGN VIRAMA
unicode_joining_type(0x0A51, 0x0A51, 'T'). % Mn       GURMUKHI SIGN UDAAT
unicode_joining_type(0x0A70, 0x0A71, 'T'). % Mn   [2] GURMUKHI TIPPI..GURMUKHI ADDAK
unicode_joining_type(0x0A75, 0x0A75, 'T'). % Mn       GURMUKHI SIGN YAKASH
unicode_joining_type(0x0A81, 0x0A82, 'T'). % Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
unicode_joining_type(0x0ABC, 0x0ABC, 'T'). % Mn       GUJARATI SIGN NUKTA
unicode_joining_type(0x0AC1, 0x0AC5, 'T'). % Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
unicode_joining_type(0x0AC7, 0x0AC8, 'T'). % Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
unicode_joining_type(0x0ACD, 0x0ACD, 'T'). % Mn       GUJARATI SIGN VIRAMA
unicode_joining_type(0x0AE2, 0x0AE3, 'T'). % Mn   [2] GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL
unicode_joining_type(0x0B01, 0x0B01, 'T'). % Mn       ORIYA SIGN CANDRABINDU
unicode_joining_type(0x0B3C, 0x0B3C, 'T'). % Mn       ORIYA SIGN NUKTA
unicode_joining_type(0x0B3F, 0x0B3F, 'T'). % Mn       ORIYA VOWEL SIGN I
unicode_joining_type(0x0B41, 0x0B44, 'T'). % Mn   [4] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR
unicode_joining_type(0x0B4D, 0x0B4D, 'T'). % Mn       ORIYA SIGN VIRAMA
unicode_joining_type(0x0B56, 0x0B56, 'T'). % Mn       ORIYA AI LENGTH MARK
unicode_joining_type(0x0B62, 0x0B63, 'T'). % Mn   [2] ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL
unicode_joining_type(0x0B82, 0x0B82, 'T'). % Mn       TAMIL SIGN ANUSVARA
unicode_joining_type(0x0BC0, 0x0BC0, 'T'). % Mn       TAMIL VOWEL SIGN II
unicode_joining_type(0x0BCD, 0x0BCD, 'T'). % Mn       TAMIL SIGN VIRAMA
unicode_joining_type(0x0C3E, 0x0C40, 'T'). % Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
unicode_joining_type(0x0C46, 0x0C48, 'T'). % Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
unicode_joining_type(0x0C4A, 0x0C4D, 'T'). % Mn   [4] TELUGU VOWEL SIGN O..TELUGU SIGN VIRAMA
unicode_joining_type(0x0C55, 0x0C56, 'T'). % Mn   [2] TELUGU LENGTH MARK..TELUGU AI LENGTH MARK
unicode_joining_type(0x0C62, 0x0C63, 'T'). % Mn   [2] TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL
unicode_joining_type(0x0CBC, 0x0CBC, 'T'). % Mn       KANNADA SIGN NUKTA
unicode_joining_type(0x0CBF, 0x0CBF, 'T'). % Mn       KANNADA VOWEL SIGN I
unicode_joining_type(0x0CC6, 0x0CC6, 'T'). % Mn       KANNADA VOWEL SIGN E
unicode_joining_type(0x0CCC, 0x0CCD, 'T'). % Mn   [2] KANNADA VOWEL SIGN AU..KANNADA SIGN VIRAMA
unicode_joining_type(0x0CE2, 0x0CE3, 'T'). % Mn   [2] KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL
unicode_joining_type(0x0D41, 0x0D44, 'T'). % Mn   [4] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC RR
unicode_joining_type(0x0D4D, 0x0D4D, 'T'). % Mn       MALAYALAM SIGN VIRAMA
unicode_joining_type(0x0D62, 0x0D63, 'T'). % Mn   [2] MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL
unicode_joining_type(0x0DCA, 0x0DCA, 'T'). % Mn       SINHALA SIGN AL-LAKUNA
unicode_joining_type(0x0DD2, 0x0DD4, 'T'). % Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
unicode_joining_type(0x0DD6, 0x0DD6, 'T'). % Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
unicode_joining_type(0x0E31, 0x0E31, 'T'). % Mn       THAI CHARACTER MAI HAN-AKAT
unicode_joining_type(0x0E34, 0x0E3A, 'T'). % Mn   [7] THAI CHARACTER SARA I..THAI CHARACTER PHINTHU
unicode_joining_type(0x0E47, 0x0E4E, 'T'). % Mn   [8] THAI CHARACTER MAITAIKHU..THAI CHARACTER YAMAKKAN
unicode_joining_type(0x0EB1, 0x0EB1, 'T'). % Mn       LAO VOWEL SIGN MAI KAN
unicode_joining_type(0x0EB4, 0x0EB9, 'T'). % Mn   [6] LAO VOWEL SIGN I..LAO VOWEL SIGN UU
unicode_joining_type(0x0EBB, 0x0EBC, 'T'). % Mn   [2] LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO
unicode_joining_type(0x0EC8, 0x0ECD, 'T'). % Mn   [6] LAO TONE MAI EK..LAO NIGGAHITA
unicode_joining_type(0x0F18, 0x0F19, 'T'). % Mn   [2] TIBETAN ASTROLOGICAL SIGN -KHYUD PA..TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS
unicode_joining_type(0x0F35, 0x0F35, 'T'). % Mn       TIBETAN MARK NGAS BZUNG NYI ZLA
unicode_joining_type(0x0F37, 0x0F37, 'T'). % Mn       TIBETAN MARK NGAS BZUNG SGOR RTAGS
unicode_joining_type(0x0F39, 0x0F39, 'T'). % Mn       TIBETAN MARK TSA -PHRU
unicode_joining_type(0x0F71, 0x0F7E, 'T'). % Mn  [14] TIBETAN VOWEL SIGN AA..TIBETAN SIGN RJES SU NGA RO
unicode_joining_type(0x0F80, 0x0F84, 'T'). % Mn   [5] TIBETAN VOWEL SIGN REVERSED I..TIBETAN MARK HALANTA
unicode_joining_type(0x0F86, 0x0F87, 'T'). % Mn   [2] TIBETAN SIGN LCI RTAGS..TIBETAN SIGN YANG RTAGS
unicode_joining_type(0x0F8D, 0x0F97, 'T'). % Mn  [11] TIBETAN SUBJOINED SIGN LCE TSA CAN..TIBETAN SUBJOINED LETTER JA
unicode_joining_type(0x0F99, 0x0FBC, 'T'). % Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
unicode_joining_type(0x0FC6, 0x0FC6, 'T'). % Mn       TIBETAN SYMBOL PADMA GDAN
unicode_joining_type(0x102D, 0x1030, 'T'). % Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
unicode_joining_type(0x1032, 0x1037, 'T'). % Mn   [6] MYANMAR VOWEL SIGN AI..MYANMAR SIGN DOT BELOW
unicode_joining_type(0x1039, 0x103A, 'T'). % Mn   [2] MYANMAR SIGN VIRAMA..MYANMAR SIGN ASAT
unicode_joining_type(0x103D, 0x103E, 'T'). % Mn   [2] MYANMAR CONSONANT SIGN MEDIAL WA..MYANMAR CONSONANT SIGN MEDIAL HA
unicode_joining_type(0x1058, 0x1059, 'T'). % Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
unicode_joining_type(0x105E, 0x1060, 'T'). % Mn   [3] MYANMAR CONSONANT SIGN MON MEDIAL NA..MYANMAR CONSONANT SIGN MON MEDIAL LA
unicode_joining_type(0x1071, 0x1074, 'T'). % Mn   [4] MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE
unicode_joining_type(0x1082, 0x1082, 'T'). % Mn       MYANMAR CONSONANT SIGN SHAN MEDIAL WA
unicode_joining_type(0x1085, 0x1086, 'T'). % Mn   [2] MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y
unicode_joining_type(0x108D, 0x108D, 'T'). % Mn       MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE
unicode_joining_type(0x109D, 0x109D, 'T'). % Mn       MYANMAR VOWEL SIGN AITON AI
unicode_joining_type(0x135D, 0x135F, 'T'). % Mn   [3] ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK..ETHIOPIC COMBINING GEMINATION MARK
unicode_joining_type(0x1712, 0x1714, 'T'). % Mn   [3] TAGALOG VOWEL SIGN I..TAGALOG SIGN VIRAMA
unicode_joining_type(0x1732, 0x1734, 'T'). % Mn   [3] HANUNOO VOWEL SIGN I..HANUNOO SIGN PAMUDPOD
unicode_joining_type(0x1752, 0x1753, 'T'). % Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
unicode_joining_type(0x1772, 0x1773, 'T'). % Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
unicode_joining_type(0x17B4, 0x17B5, 'T'). % Mn   [2] KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA
unicode_joining_type(0x17B7, 0x17BD, 'T'). % Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
unicode_joining_type(0x17C6, 0x17C6, 'T'). % Mn       KHMER SIGN NIKAHIT
unicode_joining_type(0x17C9, 0x17D3, 'T'). % Mn  [11] KHMER SIGN MUUSIKATOAN..KHMER SIGN BATHAMASAT
unicode_joining_type(0x17DD, 0x17DD, 'T'). % Mn       KHMER SIGN ATTHACAN
unicode_joining_type(0x180B, 0x180D, 'T'). % Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
unicode_joining_type(0x18A9, 0x18A9, 'T'). % Mn       MONGOLIAN LETTER ALI GALI DAGALGA
unicode_joining_type(0x1920, 0x1922, 'T'). % Mn   [3] LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN U
unicode_joining_type(0x1927, 0x1928, 'T'). % Mn   [2] LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O
unicode_joining_type(0x1932, 0x1932, 'T'). % Mn       LIMBU SMALL LETTER ANUSVARA
unicode_joining_type(0x1939, 0x193B, 'T'). % Mn   [3] LIMBU SIGN MUKPHRENG..LIMBU SIGN SA-I
unicode_joining_type(0x1A17, 0x1A18, 'T'). % Mn   [2] BUGINESE VOWEL SIGN I..BUGINESE VOWEL SIGN U
unicode_joining_type(0x1A56, 0x1A56, 'T'). % Mn       TAI THAM CONSONANT SIGN MEDIAL LA
unicode_joining_type(0x1A58, 0x1A5E, 'T'). % Mn   [7] TAI THAM SIGN MAI KANG LAI..TAI THAM CONSONANT SIGN SA
unicode_joining_type(0x1A60, 0x1A60, 'T'). % Mn       TAI THAM SIGN SAKOT
unicode_joining_type(0x1A62, 0x1A62, 'T'). % Mn       TAI THAM VOWEL SIGN MAI SAT
unicode_joining_type(0x1A65, 0x1A6C, 'T'). % Mn   [8] TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN OA BELOW
unicode_joining_type(0x1A73, 0x1A7C, 'T'). % Mn  [10] TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN KHUEN-LUE KARAN
unicode_joining_type(0x1A7F, 0x1A7F, 'T'). % Mn       TAI THAM COMBINING CRYPTOGRAMMIC DOT
unicode_joining_type(0x1B00, 0x1B03, 'T'). % Mn   [4] BALINESE SIGN ULU RICEM..BALINESE SIGN SURANG
unicode_joining_type(0x1B34, 0x1B34, 'T'). % Mn       BALINESE SIGN REREKAN
unicode_joining_type(0x1B36, 0x1B3A, 'T'). % Mn   [5] BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN RA REPA
unicode_joining_type(0x1B3C, 0x1B3C, 'T'). % Mn       BALINESE VOWEL SIGN LA LENGA
unicode_joining_type(0x1B42, 0x1B42, 'T'). % Mn       BALINESE VOWEL SIGN PEPET
unicode_joining_type(0x1B6B, 0x1B73, 'T'). % Mn   [9] BALINESE MUSICAL SYMBOL COMBINING TEGEH..BALINESE MUSICAL SYMBOL COMBINING GONG
unicode_joining_type(0x1B80, 0x1B81, 'T'). % Mn   [2] SUNDANESE SIGN PANYECEK..SUNDANESE SIGN PANGLAYAR
unicode_joining_type(0x1BA2, 0x1BA5, 'T'). % Mn   [4] SUNDANESE CONSONANT SIGN PANYAKRA..SUNDANESE VOWEL SIGN PANYUKU
unicode_joining_type(0x1BA8, 0x1BA9, 'T'). % Mn   [2] SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG
unicode_joining_type(0x1BAB, 0x1BAB, 'T'). % Mn       SUNDANESE SIGN VIRAMA
unicode_joining_type(0x1BE6, 0x1BE6, 'T'). % Mn       BATAK SIGN TOMPI
unicode_joining_type(0x1BE8, 0x1BE9, 'T'). % Mn   [2] BATAK VOWEL SIGN PAKPAK E..BATAK VOWEL SIGN EE
unicode_joining_type(0x1BED, 0x1BED, 'T'). % Mn       BATAK VOWEL SIGN KARO O
unicode_joining_type(0x1BEF, 0x1BF1, 'T'). % Mn   [3] BATAK VOWEL SIGN U FOR SIMALUNGUN SA..BATAK CONSONANT SIGN H
unicode_joining_type(0x1C2C, 0x1C33, 'T'). % Mn   [8] LEPCHA VOWEL SIGN E..LEPCHA CONSONANT SIGN T
unicode_joining_type(0x1C36, 0x1C37, 'T'). % Mn   [2] LEPCHA SIGN RAN..LEPCHA SIGN NUKTA
unicode_joining_type(0x1CD0, 0x1CD2, 'T'). % Mn   [3] VEDIC TONE KARSHANA..VEDIC TONE PRENKHA
unicode_joining_type(0x1CD4, 0x1CE0, 'T'). % Mn  [13] VEDIC SIGN YAJURVEDIC MIDLINE SVARITA..VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA
unicode_joining_type(0x1CE2, 0x1CE8, 'T'). % Mn   [7] VEDIC SIGN VISARGA SVARITA..VEDIC SIGN VISARGA ANUDATTA WITH TAIL
unicode_joining_type(0x1CED, 0x1CED, 'T'). % Mn       VEDIC SIGN TIRYAK
unicode_joining_type(0x1CF4, 0x1CF4, 'T'). % Mn       VEDIC TONE CANDRA ABOVE
unicode_joining_type(0x1DC0, 0x1DE6, 'T'). % Mn  [39] COMBINING DOTTED GRAVE ACCENT..COMBINING LATIN SMALL LETTER Z
unicode_joining_type(0x1DFC, 0x1DFF, 'T'). % Mn   [4] COMBINING DOUBLE INVERTED BREVE BELOW..COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW
unicode_joining_type(0x200B, 0x200B, 'T'). % Cf       ZERO WIDTH SPACE
unicode_joining_type(0x200E, 0x200F, 'T'). % Cf   [2] LEFT-TO-RIGHT MARK..RIGHT-TO-LEFT MARK
unicode_joining_type(0x202A, 0x202E, 'T'). % Cf   [5] LEFT-TO-RIGHT EMBEDDING..RIGHT-TO-LEFT OVERRIDE
unicode_joining_type(0x2060, 0x2064, 'T'). % Cf   [5] WORD JOINER..INVISIBLE PLUS
unicode_joining_type(0x206A, 0x206F, 'T'). % Cf   [6] INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES
unicode_joining_type(0x20D0, 0x20DC, 'T'). % Mn  [13] COMBINING LEFT HARPOON ABOVE..COMBINING FOUR DOTS ABOVE
unicode_joining_type(0x20DD, 0x20E0, 'T'). % Me   [4] COMBINING ENCLOSING CIRCLE..COMBINING ENCLOSING CIRCLE BACKSLASH
unicode_joining_type(0x20E1, 0x20E1, 'T'). % Mn       COMBINING LEFT RIGHT ARROW ABOVE
unicode_joining_type(0x20E2, 0x20E4, 'T'). % Me   [3] COMBINING ENCLOSING SCREEN..COMBINING ENCLOSING UPWARD POINTING TRIANGLE
unicode_joining_type(0x20E5, 0x20F0, 'T'). % Mn  [12] COMBINING REVERSE SOLIDUS OVERLAY..COMBINING ASTERISK ABOVE
unicode_joining_type(0x2CEF, 0x2CF1, 'T'). % Mn   [3] COPTIC COMBINING NI ABOVE..COPTIC COMBINING SPIRITUS LENIS
unicode_joining_type(0x2D7F, 0x2D7F, 'T'). % Mn       TIFINAGH CONSONANT JOINER
unicode_joining_type(0x2DE0, 0x2DFF, 'T'). % Mn  [32] COMBINING CYRILLIC LETTER BE..COMBINING CYRILLIC LETTER IOTIFIED BIG YUS
unicode_joining_type(0x302A, 0x302D, 'T'). % Mn   [4] IDEOGRAPHIC LEVEL TONE MARK..IDEOGRAPHIC ENTERING TONE MARK
unicode_joining_type(0x3099, 0x309A, 'T'). % Mn   [2] COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK..COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_joining_type(0xA66F, 0xA66F, 'T'). % Mn       COMBINING CYRILLIC VZMET
unicode_joining_type(0xA670, 0xA672, 'T'). % Me   [3] COMBINING CYRILLIC TEN MILLIONS SIGN..COMBINING CYRILLIC THOUSAND MILLIONS SIGN
unicode_joining_type(0xA674, 0xA67D, 'T'). % Mn  [10] COMBINING CYRILLIC LETTER UKRAINIAN IE..COMBINING CYRILLIC PAYEROK
unicode_joining_type(0xA69F, 0xA69F, 'T'). % Mn       COMBINING CYRILLIC LETTER IOTIFIED E
unicode_joining_type(0xA6F0, 0xA6F1, 'T'). % Mn   [2] BAMUM COMBINING MARK KOQNDON..BAMUM COMBINING MARK TUKWENTIS
unicode_joining_type(0xA802, 0xA802, 'T'). % Mn       SYLOTI NAGRI SIGN DVISVARA
unicode_joining_type(0xA806, 0xA806, 'T'). % Mn       SYLOTI NAGRI SIGN HASANTA
unicode_joining_type(0xA80B, 0xA80B, 'T'). % Mn       SYLOTI NAGRI SIGN ANUSVARA
unicode_joining_type(0xA825, 0xA826, 'T'). % Mn   [2] SYLOTI NAGRI VOWEL SIGN U..SYLOTI NAGRI VOWEL SIGN E
unicode_joining_type(0xA8C4, 0xA8C4, 'T'). % Mn       SAURASHTRA SIGN VIRAMA
unicode_joining_type(0xA8E0, 0xA8F1, 'T'). % Mn  [18] COMBINING DEVANAGARI DIGIT ZERO..COMBINING DEVANAGARI SIGN AVAGRAHA
unicode_joining_type(0xA926, 0xA92D, 'T'). % Mn   [8] KAYAH LI VOWEL UE..KAYAH LI TONE CALYA PLOPHU
unicode_joining_type(0xA947, 0xA951, 'T'). % Mn  [11] REJANG VOWEL SIGN I..REJANG CONSONANT SIGN R
unicode_joining_type(0xA980, 0xA982, 'T'). % Mn   [3] JAVANESE SIGN PANYANGGA..JAVANESE SIGN LAYAR
unicode_joining_type(0xA9B3, 0xA9B3, 'T'). % Mn       JAVANESE SIGN CECAK TELU
unicode_joining_type(0xA9B6, 0xA9B9, 'T'). % Mn   [4] JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN SUKU MENDUT
unicode_joining_type(0xA9BC, 0xA9BC, 'T'). % Mn       JAVANESE VOWEL SIGN PEPET
unicode_joining_type(0xAA29, 0xAA2E, 'T'). % Mn   [6] CHAM VOWEL SIGN AA..CHAM VOWEL SIGN OE
unicode_joining_type(0xAA31, 0xAA32, 'T'). % Mn   [2] CHAM VOWEL SIGN AU..CHAM VOWEL SIGN UE
unicode_joining_type(0xAA35, 0xAA36, 'T'). % Mn   [2] CHAM CONSONANT SIGN LA..CHAM CONSONANT SIGN WA
unicode_joining_type(0xAA43, 0xAA43, 'T'). % Mn       CHAM CONSONANT SIGN FINAL NG
unicode_joining_type(0xAA4C, 0xAA4C, 'T'). % Mn       CHAM CONSONANT SIGN FINAL M
unicode_joining_type(0xAAB0, 0xAAB0, 'T'). % Mn       TAI VIET MAI KANG
unicode_joining_type(0xAAB2, 0xAAB4, 'T'). % Mn   [3] TAI VIET VOWEL I..TAI VIET VOWEL U
unicode_joining_type(0xAAB7, 0xAAB8, 'T'). % Mn   [2] TAI VIET MAI KHIT..TAI VIET VOWEL IA
unicode_joining_type(0xAABE, 0xAABF, 'T'). % Mn   [2] TAI VIET VOWEL AM..TAI VIET TONE MAI EK
unicode_joining_type(0xAAC1, 0xAAC1, 'T'). % Mn       TAI VIET TONE MAI THO
unicode_joining_type(0xAAEC, 0xAAED, 'T'). % Mn   [2] MEETEI MAYEK VOWEL SIGN UU..MEETEI MAYEK VOWEL SIGN AAI
unicode_joining_type(0xAAF6, 0xAAF6, 'T'). % Mn       MEETEI MAYEK VIRAMA
unicode_joining_type(0xABE5, 0xABE5, 'T'). % Mn       MEETEI MAYEK VOWEL SIGN ANAP
unicode_joining_type(0xABE8, 0xABE8, 'T'). % Mn       MEETEI MAYEK VOWEL SIGN UNAP
unicode_joining_type(0xABED, 0xABED, 'T'). % Mn       MEETEI MAYEK APUN IYEK
unicode_joining_type(0xFB1E, 0xFB1E, 'T'). % Mn       HEBREW POINT JUDEO-SPANISH VARIKA
unicode_joining_type(0xFE00, 0xFE0F, 'T'). % Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
unicode_joining_type(0xFE20, 0xFE26, 'T'). % Mn   [7] COMBINING LIGATURE LEFT HALF..COMBINING CONJOINING MACRON
unicode_joining_type(0xFEFF, 0xFEFF, 'T'). % Cf       ZERO WIDTH NO-BREAK SPACE
unicode_joining_type(0xFFF9, 0xFFFB, 'T'). % Cf   [3] INTERLINEAR ANNOTATION ANCHOR..INTERLINEAR ANNOTATION TERMINATOR
unicode_joining_type(0x101FD, 0x101FD, 'T'). % Mn       PHAISTOS DISC SIGN COMBINING OBLIQUE STROKE
unicode_joining_type(0x10A01, 0x10A03, 'T'). % Mn   [3] KHAROSHTHI VOWEL SIGN I..KHAROSHTHI VOWEL SIGN VOCALIC R
unicode_joining_type(0x10A05, 0x10A06, 'T'). % Mn   [2] KHAROSHTHI VOWEL SIGN E..KHAROSHTHI VOWEL SIGN O
unicode_joining_type(0x10A0C, 0x10A0F, 'T'). % Mn   [4] KHAROSHTHI VOWEL LENGTH MARK..KHAROSHTHI SIGN VISARGA
unicode_joining_type(0x10A38, 0x10A3A, 'T'). % Mn   [3] KHAROSHTHI SIGN BAR ABOVE..KHAROSHTHI SIGN DOT BELOW
unicode_joining_type(0x10A3F, 0x10A3F, 'T'). % Mn       KHAROSHTHI VIRAMA
unicode_joining_type(0x11001, 0x11001, 'T'). % Mn       BRAHMI SIGN ANUSVARA
unicode_joining_type(0x11038, 0x11046, 'T'). % Mn  [15] BRAHMI VOWEL SIGN AA..BRAHMI VIRAMA
unicode_joining_type(0x11080, 0x11081, 'T'). % Mn   [2] KAITHI SIGN CANDRABINDU..KAITHI SIGN ANUSVARA
unicode_joining_type(0x110B3, 0x110B6, 'T'). % Mn   [4] KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN AI
unicode_joining_type(0x110B9, 0x110BA, 'T'). % Mn   [2] KAITHI SIGN VIRAMA..KAITHI SIGN NUKTA
unicode_joining_type(0x110BD, 0x110BD, 'T'). % Cf       KAITHI NUMBER SIGN
unicode_joining_type(0x11100, 0x11102, 'T'). % Mn   [3] CHAKMA SIGN CANDRABINDU..CHAKMA SIGN VISARGA
unicode_joining_type(0x11127, 0x1112B, 'T'). % Mn   [5] CHAKMA VOWEL SIGN A..CHAKMA VOWEL SIGN UU
unicode_joining_type(0x1112D, 0x11134, 'T'). % Mn   [8] CHAKMA VOWEL SIGN AI..CHAKMA MAAYYAA
unicode_joining_type(0x11180, 0x11181, 'T'). % Mn   [2] SHARADA SIGN CANDRABINDU..SHARADA SIGN ANUSVARA
unicode_joining_type(0x111B6, 0x111BE, 'T'). % Mn   [9] SHARADA VOWEL SIGN U..SHARADA VOWEL SIGN O
unicode_joining_type(0x116AB, 0x116AB, 'T'). % Mn       TAKRI SIGN ANUSVARA
unicode_joining_type(0x116AD, 0x116AD, 'T'). % Mn       TAKRI VOWEL SIGN AA
unicode_joining_type(0x116B0, 0x116B5, 'T'). % Mn   [6] TAKRI VOWEL SIGN U..TAKRI VOWEL SIGN AU
unicode_joining_type(0x116B7, 0x116B7, 'T'). % Mn       TAKRI SIGN NUKTA
unicode_joining_type(0x16F8F, 0x16F92, 'T'). % Mn   [4] MIAO TONE RIGHT..MIAO TONE BELOW
unicode_joining_type(0x1D167, 0x1D169, 'T'). % Mn   [3] MUSICAL SYMBOL COMBINING TREMOLO-1..MUSICAL SYMBOL COMBINING TREMOLO-3
unicode_joining_type(0x1D173, 0x1D17A, 'T'). % Cf   [8] MUSICAL SYMBOL BEGIN BEAM..MUSICAL SYMBOL END PHRASE
unicode_joining_type(0x1D17B, 0x1D182, 'T'). % Mn   [8] MUSICAL SYMBOL COMBINING ACCENT..MUSICAL SYMBOL COMBINING LOURE
unicode_joining_type(0x1D185, 0x1D18B, 'T'). % Mn   [7] MUSICAL SYMBOL COMBINING DOIT..MUSICAL SYMBOL COMBINING TRIPLE TONGUE
unicode_joining_type(0x1D1AA, 0x1D1AD, 'T'). % Mn   [4] MUSICAL SYMBOL COMBINING DOWN BOW..MUSICAL SYMBOL COMBINING SNAP PIZZICATO
unicode_joining_type(0x1D242, 0x1D244, 'T'). % Mn   [3] COMBINING GREEK MUSICAL TRISEME..COMBINING GREEK MUSICAL PENTASEME
unicode_joining_type(0xE0001, 0xE0001, 'T'). % Cf       LANGUAGE TAG
unicode_joining_type(0xE0020, 0xE007F, 'T'). % Cf  [96] TAG SPACE..CANCEL TAG
unicode_joining_type(0xE0100, 0xE01EF, 'T'). % Mn [240] VARIATION SELECTOR-17..VARIATION SELECTOR-256

% Total code points: 1423

% EOF
