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

unicode_diacritic(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_diacritic(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_diacritic(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_diacritic(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_diacritic(0x005E, 0x005E).	% Diacritic # Sk       CIRCUMFLEX ACCENT
unicode_diacritic(0x0060, 0x0060).	% Diacritic # Sk       GRAVE ACCENT
unicode_diacritic(0x00A8, 0x00A8).	% Diacritic # Sk       DIAERESIS
unicode_diacritic(0x00AF, 0x00AF).	% Diacritic # Sk       MACRON
unicode_diacritic(0x00B4, 0x00B4).	% Diacritic # Sk       ACUTE ACCENT
unicode_diacritic(0x00B7, 0x00B7).	% Diacritic # Po       MIDDLE DOT
unicode_diacritic(0x00B8, 0x00B8).	% Diacritic # Sk       CEDILLA
unicode_diacritic(0x02B0, 0x02C1).	% Diacritic # Lm  [18] MODIFIER LETTER SMALL H..MODIFIER LETTER REVERSED GLOTTAL STOP
unicode_diacritic(0x02C2, 0x02C5).	% Diacritic # Sk   [4] MODIFIER LETTER LEFT ARROWHEAD..MODIFIER LETTER DOWN ARROWHEAD
unicode_diacritic(0x02C6, 0x02D1).	% Diacritic # Lm  [12] MODIFIER LETTER CIRCUMFLEX ACCENT..MODIFIER LETTER HALF TRIANGULAR COLON
unicode_diacritic(0x02D2, 0x02DF).	% Diacritic # Sk  [14] MODIFIER LETTER CENTRED RIGHT HALF RING..MODIFIER LETTER CROSS ACCENT
unicode_diacritic(0x02E0, 0x02E4).	% Diacritic # Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_diacritic(0x02E5, 0x02EB).	% Diacritic # Sk   [7] MODIFIER LETTER EXTRA-HIGH TONE BAR..MODIFIER LETTER YANG DEPARTING TONE MARK
unicode_diacritic(0x02EC, 0x02EC).	% Diacritic # Lm       MODIFIER LETTER VOICING
unicode_diacritic(0x02ED, 0x02ED).	% Diacritic # Sk       MODIFIER LETTER UNASPIRATED
unicode_diacritic(0x02EE, 0x02EE).	% Diacritic # Lm       MODIFIER LETTER DOUBLE APOSTROPHE
unicode_diacritic(0x02EF, 0x02FF).	% Diacritic # Sk  [17] MODIFIER LETTER LOW DOWN ARROWHEAD..MODIFIER LETTER LOW LEFT ARROW
unicode_diacritic(0x0300, 0x034E).	% Diacritic # Mn  [79] COMBINING GRAVE ACCENT..COMBINING UPWARDS ARROW BELOW
unicode_diacritic(0x0350, 0x0357).	% Diacritic # Mn   [8] COMBINING RIGHT ARROWHEAD ABOVE..COMBINING RIGHT HALF RING ABOVE
unicode_diacritic(0x035D, 0x0362).	% Diacritic # Mn   [6] COMBINING DOUBLE BREVE..COMBINING DOUBLE RIGHTWARDS ARROW BELOW
unicode_diacritic(0x0374, 0x0374).	% Diacritic # Lm       GREEK NUMERAL SIGN
unicode_diacritic(0x0375, 0x0375).	% Diacritic # Sk       GREEK LOWER NUMERAL SIGN
unicode_diacritic(0x037A, 0x037A).	% Diacritic # Lm       GREEK YPOGEGRAMMENI
unicode_diacritic(0x0384, 0x0385).	% Diacritic # Sk   [2] GREEK TONOS..GREEK DIALYTIKA TONOS
unicode_diacritic(0x0483, 0x0487).	% Diacritic # Mn   [5] COMBINING CYRILLIC TITLO..COMBINING CYRILLIC POKRYTIE
unicode_diacritic(0x0559, 0x0559).	% Diacritic # Lm       ARMENIAN MODIFIER LETTER LEFT HALF RING
unicode_diacritic(0x0591, 0x05A1).	% Diacritic # Mn  [17] HEBREW ACCENT ETNAHTA..HEBREW ACCENT PAZER
unicode_diacritic(0x05A3, 0x05BD).	% Diacritic # Mn  [27] HEBREW ACCENT MUNAH..HEBREW POINT METEG
unicode_diacritic(0x05BF, 0x05BF).	% Diacritic # Mn       HEBREW POINT RAFE
unicode_diacritic(0x05C1, 0x05C2).	% Diacritic # Mn   [2] HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT
unicode_diacritic(0x05C4, 0x05C4).	% Diacritic # Mn       HEBREW MARK UPPER DOT
unicode_diacritic(0x064B, 0x0652).	% Diacritic # Mn   [8] ARABIC FATHATAN..ARABIC SUKUN
unicode_diacritic(0x0657, 0x0658).	% Diacritic # Mn   [2] ARABIC INVERTED DAMMA..ARABIC MARK NOON GHUNNA
unicode_diacritic(0x06DF, 0x06E0).	% Diacritic # Mn   [2] ARABIC SMALL HIGH ROUNDED ZERO..ARABIC SMALL HIGH UPRIGHT RECTANGULAR ZERO
unicode_diacritic(0x06E5, 0x06E6).	% Diacritic # Lm   [2] ARABIC SMALL WAW..ARABIC SMALL YEH
unicode_diacritic(0x06EA, 0x06EC).	% Diacritic # Mn   [3] ARABIC EMPTY CENTRE LOW STOP..ARABIC ROUNDED HIGH STOP WITH FILLED CENTRE
unicode_diacritic(0x0730, 0x074A).	% Diacritic # Mn  [27] SYRIAC PTHAHA ABOVE..SYRIAC BARREKH
unicode_diacritic(0x07A6, 0x07B0).	% Diacritic # Mn  [11] THAANA ABAFILI..THAANA SUKUN
unicode_diacritic(0x07EB, 0x07F3).	% Diacritic # Mn   [9] NKO COMBINING SHORT HIGH TONE..NKO COMBINING DOUBLE DOT ABOVE
unicode_diacritic(0x07F4, 0x07F5).	% Diacritic # Lm   [2] NKO HIGH TONE APOSTROPHE..NKO LOW TONE APOSTROPHE
unicode_diacritic(0x0818, 0x0819).	% Diacritic # Mn   [2] SAMARITAN MARK OCCLUSION..SAMARITAN MARK DAGESH
unicode_diacritic(0x08E4, 0x08FE).	% Diacritic # Mn  [27] ARABIC CURLY FATHA..ARABIC DAMMA WITH DOT
unicode_diacritic(0x093C, 0x093C).	% Diacritic # Mn       DEVANAGARI SIGN NUKTA
unicode_diacritic(0x094D, 0x094D).	% Diacritic # Mn       DEVANAGARI SIGN VIRAMA
unicode_diacritic(0x0951, 0x0954).	% Diacritic # Mn   [4] DEVANAGARI STRESS SIGN UDATTA..DEVANAGARI ACUTE ACCENT
unicode_diacritic(0x0971, 0x0971).	% Diacritic # Lm       DEVANAGARI SIGN HIGH SPACING DOT
unicode_diacritic(0x09BC, 0x09BC).	% Diacritic # Mn       BENGALI SIGN NUKTA
unicode_diacritic(0x09CD, 0x09CD).	% Diacritic # Mn       BENGALI SIGN VIRAMA
unicode_diacritic(0x0A3C, 0x0A3C).	% Diacritic # Mn       GURMUKHI SIGN NUKTA
unicode_diacritic(0x0A4D, 0x0A4D).	% Diacritic # Mn       GURMUKHI SIGN VIRAMA
unicode_diacritic(0x0ABC, 0x0ABC).	% Diacritic # Mn       GUJARATI SIGN NUKTA
unicode_diacritic(0x0ACD, 0x0ACD).	% Diacritic # Mn       GUJARATI SIGN VIRAMA
unicode_diacritic(0x0B3C, 0x0B3C).	% Diacritic # Mn       ORIYA SIGN NUKTA
unicode_diacritic(0x0B4D, 0x0B4D).	% Diacritic # Mn       ORIYA SIGN VIRAMA
unicode_diacritic(0x0BCD, 0x0BCD).	% Diacritic # Mn       TAMIL SIGN VIRAMA
unicode_diacritic(0x0C4D, 0x0C4D).	% Diacritic # Mn       TELUGU SIGN VIRAMA
unicode_diacritic(0x0CBC, 0x0CBC).	% Diacritic # Mn       KANNADA SIGN NUKTA
unicode_diacritic(0x0CCD, 0x0CCD).	% Diacritic # Mn       KANNADA SIGN VIRAMA
unicode_diacritic(0x0D4D, 0x0D4D).	% Diacritic # Mn       MALAYALAM SIGN VIRAMA
unicode_diacritic(0x0DCA, 0x0DCA).	% Diacritic # Mn       SINHALA SIGN AL-LAKUNA
unicode_diacritic(0x0E47, 0x0E4C).	% Diacritic # Mn   [6] THAI CHARACTER MAITAIKHU..THAI CHARACTER THANTHAKHAT
unicode_diacritic(0x0E4E, 0x0E4E).	% Diacritic # Mn       THAI CHARACTER YAMAKKAN
unicode_diacritic(0x0EC8, 0x0ECC).	% Diacritic # Mn   [5] LAO TONE MAI EK..LAO CANCELLATION MARK
unicode_diacritic(0x0F18, 0x0F19).	% Diacritic # Mn   [2] TIBETAN ASTROLOGICAL SIGN -KHYUD PA..TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS
unicode_diacritic(0x0F35, 0x0F35).	% Diacritic # Mn       TIBETAN MARK NGAS BZUNG NYI ZLA
unicode_diacritic(0x0F37, 0x0F37).	% Diacritic # Mn       TIBETAN MARK NGAS BZUNG SGOR RTAGS
unicode_diacritic(0x0F39, 0x0F39).	% Diacritic # Mn       TIBETAN MARK TSA -PHRU
unicode_diacritic(0x0F3E, 0x0F3F).	% Diacritic # Mc   [2] TIBETAN SIGN YAR TSHES..TIBETAN SIGN MAR TSHES
unicode_diacritic(0x0F82, 0x0F84).	% Diacritic # Mn   [3] TIBETAN SIGN NYI ZLA NAA DA..TIBETAN MARK HALANTA
unicode_diacritic(0x0F86, 0x0F87).	% Diacritic # Mn   [2] TIBETAN SIGN LCI RTAGS..TIBETAN SIGN YANG RTAGS
unicode_diacritic(0x0FC6, 0x0FC6).	% Diacritic # Mn       TIBETAN SYMBOL PADMA GDAN
unicode_diacritic(0x1037, 0x1037).	% Diacritic # Mn       MYANMAR SIGN DOT BELOW
unicode_diacritic(0x1039, 0x103A).	% Diacritic # Mn   [2] MYANMAR SIGN VIRAMA..MYANMAR SIGN ASAT
unicode_diacritic(0x1087, 0x108C).	% Diacritic # Mc   [6] MYANMAR SIGN SHAN TONE-2..MYANMAR SIGN SHAN COUNCIL TONE-3
unicode_diacritic(0x108D, 0x108D).	% Diacritic # Mn       MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE
unicode_diacritic(0x108F, 0x108F).	% Diacritic # Mc       MYANMAR SIGN RUMAI PALAUNG TONE-5
unicode_diacritic(0x109A, 0x109B).	% Diacritic # Mc   [2] MYANMAR SIGN KHAMTI TONE-1..MYANMAR SIGN KHAMTI TONE-3
unicode_diacritic(0x17C9, 0x17D3).	% Diacritic # Mn  [11] KHMER SIGN MUUSIKATOAN..KHMER SIGN BATHAMASAT
unicode_diacritic(0x17DD, 0x17DD).	% Diacritic # Mn       KHMER SIGN ATTHACAN
unicode_diacritic(0x1939, 0x193B).	% Diacritic # Mn   [3] LIMBU SIGN MUKPHRENG..LIMBU SIGN SA-I
unicode_diacritic(0x1A75, 0x1A7C).	% Diacritic # Mn   [8] TAI THAM SIGN TONE-1..TAI THAM SIGN KHUEN-LUE KARAN
unicode_diacritic(0x1A7F, 0x1A7F).	% Diacritic # Mn       TAI THAM COMBINING CRYPTOGRAMMIC DOT
unicode_diacritic(0x1B34, 0x1B34).	% Diacritic # Mn       BALINESE SIGN REREKAN
unicode_diacritic(0x1B44, 0x1B44).	% Diacritic # Mc       BALINESE ADEG ADEG
unicode_diacritic(0x1B6B, 0x1B73).	% Diacritic # Mn   [9] BALINESE MUSICAL SYMBOL COMBINING TEGEH..BALINESE MUSICAL SYMBOL COMBINING GONG
unicode_diacritic(0x1BAA, 0x1BAA).	% Diacritic # Mc       SUNDANESE SIGN PAMAAEH
unicode_diacritic(0x1BAB, 0x1BAB).	% Diacritic # Mn       SUNDANESE SIGN VIRAMA
unicode_diacritic(0x1C36, 0x1C37).	% Diacritic # Mn   [2] LEPCHA SIGN RAN..LEPCHA SIGN NUKTA
unicode_diacritic(0x1C78, 0x1C7D).	% Diacritic # Lm   [6] OL CHIKI MU TTUDDAG..OL CHIKI AHAD
unicode_diacritic(0x1CD0, 0x1CD2).	% Diacritic # Mn   [3] VEDIC TONE KARSHANA..VEDIC TONE PRENKHA
unicode_diacritic(0x1CD3, 0x1CD3).	% Diacritic # Po       VEDIC SIGN NIHSHVASA
unicode_diacritic(0x1CD4, 0x1CE0).	% Diacritic # Mn  [13] VEDIC SIGN YAJURVEDIC MIDLINE SVARITA..VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA
unicode_diacritic(0x1CE1, 0x1CE1).	% Diacritic # Mc       VEDIC TONE ATHARVAVEDIC INDEPENDENT SVARITA
unicode_diacritic(0x1CE2, 0x1CE8).	% Diacritic # Mn   [7] VEDIC SIGN VISARGA SVARITA..VEDIC SIGN VISARGA ANUDATTA WITH TAIL
unicode_diacritic(0x1CED, 0x1CED).	% Diacritic # Mn       VEDIC SIGN TIRYAK
unicode_diacritic(0x1CF4, 0x1CF4).	% Diacritic # Mn       VEDIC TONE CANDRA ABOVE
unicode_diacritic(0x1D2C, 0x1D6A).	% Diacritic # Lm  [63] MODIFIER LETTER CAPITAL A..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_diacritic(0x1DC4, 0x1DCF).	% Diacritic # Mn  [12] COMBINING MACRON-ACUTE..COMBINING ZIGZAG BELOW
unicode_diacritic(0x1DFD, 0x1DFF).	% Diacritic # Mn   [3] COMBINING ALMOST EQUAL TO BELOW..COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW
unicode_diacritic(0x1FBD, 0x1FBD).	% Diacritic # Sk       GREEK KORONIS
unicode_diacritic(0x1FBF, 0x1FC1).	% Diacritic # Sk   [3] GREEK PSILI..GREEK DIALYTIKA AND PERISPOMENI
unicode_diacritic(0x1FCD, 0x1FCF).	% Diacritic # Sk   [3] GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI
unicode_diacritic(0x1FDD, 0x1FDF).	% Diacritic # Sk   [3] GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI
unicode_diacritic(0x1FED, 0x1FEF).	% Diacritic # Sk   [3] GREEK DIALYTIKA AND VARIA..GREEK VARIA
unicode_diacritic(0x1FFD, 0x1FFE).	% Diacritic # Sk   [2] GREEK OXIA..GREEK DASIA
unicode_diacritic(0x2CEF, 0x2CF1).	% Diacritic # Mn   [3] COPTIC COMBINING NI ABOVE..COPTIC COMBINING SPIRITUS LENIS
unicode_diacritic(0x2E2F, 0x2E2F).	% Diacritic # Lm       VERTICAL TILDE
unicode_diacritic(0x302A, 0x302D).	% Diacritic # Mn   [4] IDEOGRAPHIC LEVEL TONE MARK..IDEOGRAPHIC ENTERING TONE MARK
unicode_diacritic(0x302E, 0x302F).	% Diacritic # Mc   [2] HANGUL SINGLE DOT TONE MARK..HANGUL DOUBLE DOT TONE MARK
unicode_diacritic(0x3099, 0x309A).	% Diacritic # Mn   [2] COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK..COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_diacritic(0x309B, 0x309C).	% Diacritic # Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_diacritic(0x30FC, 0x30FC).	% Diacritic # Lm       KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_diacritic(0xA66F, 0xA66F).	% Diacritic # Mn       COMBINING CYRILLIC VZMET
unicode_diacritic(0xA67C, 0xA67D).	% Diacritic # Mn   [2] COMBINING CYRILLIC KAVYKA..COMBINING CYRILLIC PAYEROK
unicode_diacritic(0xA67F, 0xA67F).	% Diacritic # Lm       CYRILLIC PAYEROK
unicode_diacritic(0xA6F0, 0xA6F1).	% Diacritic # Mn   [2] BAMUM COMBINING MARK KOQNDON..BAMUM COMBINING MARK TUKWENTIS
unicode_diacritic(0xA717, 0xA71F).	% Diacritic # Lm   [9] MODIFIER LETTER DOT VERTICAL BAR..MODIFIER LETTER LOW INVERTED EXCLAMATION MARK
unicode_diacritic(0xA720, 0xA721).	% Diacritic # Sk   [2] MODIFIER LETTER STRESS AND HIGH TONE..MODIFIER LETTER STRESS AND LOW TONE
unicode_diacritic(0xA788, 0xA788).	% Diacritic # Lm       MODIFIER LETTER LOW CIRCUMFLEX ACCENT
unicode_diacritic(0xA7F8, 0xA7F9).	% Diacritic # Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_diacritic(0xA8C4, 0xA8C4).	% Diacritic # Mn       SAURASHTRA SIGN VIRAMA
unicode_diacritic(0xA8E0, 0xA8F1).	% Diacritic # Mn  [18] COMBINING DEVANAGARI DIGIT ZERO..COMBINING DEVANAGARI SIGN AVAGRAHA
unicode_diacritic(0xA92B, 0xA92D).	% Diacritic # Mn   [3] KAYAH LI TONE PLOPHU..KAYAH LI TONE CALYA PLOPHU
unicode_diacritic(0xA92E, 0xA92E).	% Diacritic # Po       KAYAH LI SIGN CWI
unicode_diacritic(0xA953, 0xA953).	% Diacritic # Mc       REJANG VIRAMA
unicode_diacritic(0xA9B3, 0xA9B3).	% Diacritic # Mn       JAVANESE SIGN CECAK TELU
unicode_diacritic(0xA9C0, 0xA9C0).	% Diacritic # Mc       JAVANESE PANGKON
unicode_diacritic(0xAA7B, 0xAA7B).	% Diacritic # Mc       MYANMAR SIGN PAO KAREN TONE
unicode_diacritic(0xAABF, 0xAABF).	% Diacritic # Mn       TAI VIET TONE MAI EK
unicode_diacritic(0xAAC0, 0xAAC0).	% Diacritic # Lo       TAI VIET TONE MAI NUENG
unicode_diacritic(0xAAC1, 0xAAC1).	% Diacritic # Mn       TAI VIET TONE MAI THO
unicode_diacritic(0xAAC2, 0xAAC2).	% Diacritic # Lo       TAI VIET TONE MAI SONG
unicode_diacritic(0xAAF6, 0xAAF6).	% Diacritic # Mn       MEETEI MAYEK VIRAMA
unicode_diacritic(0xABEC, 0xABEC).	% Diacritic # Mc       MEETEI MAYEK LUM IYEK
unicode_diacritic(0xABED, 0xABED).	% Diacritic # Mn       MEETEI MAYEK APUN IYEK
unicode_diacritic(0xFB1E, 0xFB1E).	% Diacritic # Mn       HEBREW POINT JUDEO-SPANISH VARIKA
unicode_diacritic(0xFE20, 0xFE26).	% Diacritic # Mn   [7] COMBINING LIGATURE LEFT HALF..COMBINING CONJOINING MACRON
unicode_diacritic(0xFF3E, 0xFF3E).	% Diacritic # Sk       FULLWIDTH CIRCUMFLEX ACCENT
unicode_diacritic(0xFF40, 0xFF40).	% Diacritic # Sk       FULLWIDTH GRAVE ACCENT
unicode_diacritic(0xFF70, 0xFF70).	% Diacritic # Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_diacritic(0xFF9E, 0xFF9F).	% Diacritic # Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
unicode_diacritic(0xFFE3, 0xFFE3).	% Diacritic # Sk       FULLWIDTH MACRON
unicode_diacritic(0x110B9, 0x110BA).	% Diacritic # Mn   [2] KAITHI SIGN VIRAMA..KAITHI SIGN NUKTA
unicode_diacritic(0x11133, 0x11134).	% Diacritic # Mn   [2] CHAKMA VIRAMA..CHAKMA MAAYYAA
unicode_diacritic(0x111C0, 0x111C0).	% Diacritic # Mc       SHARADA SIGN VIRAMA
unicode_diacritic(0x116B6, 0x116B6).	% Diacritic # Mc       TAKRI SIGN VIRAMA
unicode_diacritic(0x116B7, 0x116B7).	% Diacritic # Mn       TAKRI SIGN NUKTA
unicode_diacritic(0x16F8F, 0x16F92).	% Diacritic # Mn   [4] MIAO TONE RIGHT..MIAO TONE BELOW
unicode_diacritic(0x16F93, 0x16F9F).	% Diacritic # Lm  [13] MIAO LETTER TONE-2..MIAO LETTER REFORMED TONE-8
unicode_diacritic(0x1D167, 0x1D169).	% Diacritic # Mn   [3] MUSICAL SYMBOL COMBINING TREMOLO-1..MUSICAL SYMBOL COMBINING TREMOLO-3
unicode_diacritic(0x1D16D, 0x1D172).	% Diacritic # Mc   [6] MUSICAL SYMBOL COMBINING AUGMENTATION DOT..MUSICAL SYMBOL COMBINING FLAG-5
unicode_diacritic(0x1D17B, 0x1D182).	% Diacritic # Mn   [8] MUSICAL SYMBOL COMBINING ACCENT..MUSICAL SYMBOL COMBINING LOURE
unicode_diacritic(0x1D185, 0x1D18B).	% Diacritic # Mn   [7] MUSICAL SYMBOL COMBINING DOIT..MUSICAL SYMBOL COMBINING TRIPLE TONGUE
unicode_diacritic(0x1D1AA, 0x1D1AD).	% Diacritic # Mn   [4] MUSICAL SYMBOL COMBINING DOWN BOW..MUSICAL SYMBOL COMBINING SNAP PIZZICATO

% Total code points: 693
