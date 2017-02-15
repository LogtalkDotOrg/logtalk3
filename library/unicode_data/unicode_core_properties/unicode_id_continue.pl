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

unicode_id_continue(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_id_continue(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_id_continue(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_id_continue(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property: ID_Continue
%  Characters that can continue an identifier.
%  Generated from:
%      ID_Start
%    + Mn + Mc + Nd + Pc
%    + Other_ID_Continue
%    - Pattern_Syntax
%    - Pattern_White_Space
%  NOTE: See UAX #31 for more information

unicode_id_continue(0x0030, 0x0039).	% ID_Continue Nd  [10] DIGIT ZERO..DIGIT NINE
unicode_id_continue(0x0041, 0x005A).	% ID_Continue L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
unicode_id_continue(0x005F, 0x005F).	% ID_Continue Pc       LOW LINE
unicode_id_continue(0x0061, 0x007A).	% ID_Continue L&  [26] LATIN SMALL LETTER A..LATIN SMALL LETTER Z
unicode_id_continue(0x00AA, 0x00AA).	% ID_Continue Lo       FEMININE ORDINAL INDICATOR
unicode_id_continue(0x00B5, 0x00B5).	% ID_Continue L&       MICRO SIGN
unicode_id_continue(0x00B7, 0x00B7).	% ID_Continue Po       MIDDLE DOT
unicode_id_continue(0x00BA, 0x00BA).	% ID_Continue Lo       MASCULINE ORDINAL INDICATOR
unicode_id_continue(0x00C0, 0x00D6).	% ID_Continue L&  [23] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_id_continue(0x00D8, 0x00F6).	% ID_Continue L&  [31] LATIN CAPITAL LETTER O WITH STROKE..LATIN SMALL LETTER O WITH DIAERESIS
unicode_id_continue(0x00F8, 0x01BA).	% ID_Continue L& [195] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER EZH WITH TAIL
unicode_id_continue(0x01BB, 0x01BB).	% ID_Continue Lo       LATIN LETTER TWO WITH STROKE
unicode_id_continue(0x01BC, 0x01BF).	% ID_Continue L&   [4] LATIN CAPITAL LETTER TONE FIVE..LATIN LETTER WYNN
unicode_id_continue(0x01C0, 0x01C3).	% ID_Continue Lo   [4] LATIN LETTER DENTAL CLICK..LATIN LETTER RETROFLEX CLICK
unicode_id_continue(0x01C4, 0x0293).	% ID_Continue L& [208] LATIN CAPITAL LETTER DZ WITH CARON..LATIN SMALL LETTER EZH WITH CURL
unicode_id_continue(0x0294, 0x0294).	% ID_Continue Lo       LATIN LETTER GLOTTAL STOP
unicode_id_continue(0x0295, 0x02AF).	% ID_Continue L&  [27] LATIN LETTER PHARYNGEAL VOICED FRICATIVE..LATIN SMALL LETTER TURNED H WITH FISHHOOK AND TAIL
unicode_id_continue(0x02B0, 0x02C1).	% ID_Continue Lm  [18] MODIFIER LETTER SMALL H..MODIFIER LETTER REVERSED GLOTTAL STOP
unicode_id_continue(0x02C6, 0x02D1).	% ID_Continue Lm  [12] MODIFIER LETTER CIRCUMFLEX ACCENT..MODIFIER LETTER HALF TRIANGULAR COLON
unicode_id_continue(0x02E0, 0x02E4).	% ID_Continue Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_id_continue(0x02EC, 0x02EC).	% ID_Continue Lm       MODIFIER LETTER VOICING
unicode_id_continue(0x02EE, 0x02EE).	% ID_Continue Lm       MODIFIER LETTER DOUBLE APOSTROPHE
unicode_id_continue(0x0300, 0x036F).	% ID_Continue Mn [112] COMBINING GRAVE ACCENT..COMBINING LATIN SMALL LETTER X
unicode_id_continue(0x0370, 0x0373).	% ID_Continue L&   [4] GREEK CAPITAL LETTER HETA..GREEK SMALL LETTER ARCHAIC SAMPI
unicode_id_continue(0x0374, 0x0374).	% ID_Continue Lm       GREEK NUMERAL SIGN
unicode_id_continue(0x0376, 0x0377).	% ID_Continue L&   [2] GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA..GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
unicode_id_continue(0x037A, 0x037A).	% ID_Continue Lm       GREEK YPOGEGRAMMENI
unicode_id_continue(0x037B, 0x037D).	% ID_Continue L&   [3] GREEK SMALL REVERSED LUNATE SIGMA SYMBOL..GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
unicode_id_continue(0x0386, 0x0386).	% ID_Continue L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_id_continue(0x0387, 0x0387).	% ID_Continue Po       GREEK ANO TELEIA
unicode_id_continue(0x0388, 0x038A).	% ID_Continue L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_id_continue(0x038C, 0x038C).	% ID_Continue L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_id_continue(0x038E, 0x03A1).	% ID_Continue L&  [20] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK CAPITAL LETTER RHO
unicode_id_continue(0x03A3, 0x03F5).	% ID_Continue L&  [83] GREEK CAPITAL LETTER SIGMA..GREEK LUNATE EPSILON SYMBOL
unicode_id_continue(0x03F7, 0x0481).	% ID_Continue L& [139] GREEK CAPITAL LETTER SHO..CYRILLIC SMALL LETTER KOPPA
unicode_id_continue(0x0483, 0x0487).	% ID_Continue Mn   [5] COMBINING CYRILLIC TITLO..COMBINING CYRILLIC POKRYTIE
unicode_id_continue(0x048A, 0x0527).	% ID_Continue L& [158] CYRILLIC CAPITAL LETTER SHORT I WITH TAIL..CYRILLIC SMALL LETTER SHHA WITH DESCENDER
unicode_id_continue(0x0531, 0x0556).	% ID_Continue L&  [38] ARMENIAN CAPITAL LETTER AYB..ARMENIAN CAPITAL LETTER FEH
unicode_id_continue(0x0559, 0x0559).	% ID_Continue Lm       ARMENIAN MODIFIER LETTER LEFT HALF RING
unicode_id_continue(0x0561, 0x0587).	% ID_Continue L&  [39] ARMENIAN SMALL LETTER AYB..ARMENIAN SMALL LIGATURE ECH YIWN
unicode_id_continue(0x0591, 0x05BD).	% ID_Continue Mn  [45] HEBREW ACCENT ETNAHTA..HEBREW POINT METEG
unicode_id_continue(0x05BF, 0x05BF).	% ID_Continue Mn       HEBREW POINT RAFE
unicode_id_continue(0x05C1, 0x05C2).	% ID_Continue Mn   [2] HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT
unicode_id_continue(0x05C4, 0x05C5).	% ID_Continue Mn   [2] HEBREW MARK UPPER DOT..HEBREW MARK LOWER DOT
unicode_id_continue(0x05C7, 0x05C7).	% ID_Continue Mn       HEBREW POINT QAMATS QATAN
unicode_id_continue(0x05D0, 0x05EA).	% ID_Continue Lo  [27] HEBREW LETTER ALEF..HEBREW LETTER TAV
unicode_id_continue(0x05F0, 0x05F2).	% ID_Continue Lo   [3] HEBREW LIGATURE YIDDISH DOUBLE VAV..HEBREW LIGATURE YIDDISH DOUBLE YOD
unicode_id_continue(0x0610, 0x061A).	% ID_Continue Mn  [11] ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM..ARABIC SMALL KASRA
unicode_id_continue(0x0620, 0x063F).	% ID_Continue Lo  [32] ARABIC LETTER KASHMIRI YEH..ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE
unicode_id_continue(0x0640, 0x0640).	% ID_Continue Lm       ARABIC TATWEEL
unicode_id_continue(0x0641, 0x064A).	% ID_Continue Lo  [10] ARABIC LETTER FEH..ARABIC LETTER YEH
unicode_id_continue(0x064B, 0x065F).	% ID_Continue Mn  [21] ARABIC FATHATAN..ARABIC WAVY HAMZA BELOW
unicode_id_continue(0x0660, 0x0669).	% ID_Continue Nd  [10] ARABIC-INDIC DIGIT ZERO..ARABIC-INDIC DIGIT NINE
unicode_id_continue(0x066E, 0x066F).	% ID_Continue Lo   [2] ARABIC LETTER DOTLESS BEH..ARABIC LETTER DOTLESS QAF
unicode_id_continue(0x0670, 0x0670).	% ID_Continue Mn       ARABIC LETTER SUPERSCRIPT ALEF
unicode_id_continue(0x0671, 0x06D3).	% ID_Continue Lo  [99] ARABIC LETTER ALEF WASLA..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
unicode_id_continue(0x06D5, 0x06D5).	% ID_Continue Lo       ARABIC LETTER AE
unicode_id_continue(0x06D6, 0x06DC).	% ID_Continue Mn   [7] ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN
unicode_id_continue(0x06DF, 0x06E4).	% ID_Continue Mn   [6] ARABIC SMALL HIGH ROUNDED ZERO..ARABIC SMALL HIGH MADDA
unicode_id_continue(0x06E5, 0x06E6).	% ID_Continue Lm   [2] ARABIC SMALL WAW..ARABIC SMALL YEH
unicode_id_continue(0x06E7, 0x06E8).	% ID_Continue Mn   [2] ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON
unicode_id_continue(0x06EA, 0x06ED).	% ID_Continue Mn   [4] ARABIC EMPTY CENTRE LOW STOP..ARABIC SMALL LOW MEEM
unicode_id_continue(0x06EE, 0x06EF).	% ID_Continue Lo   [2] ARABIC LETTER DAL WITH INVERTED V..ARABIC LETTER REH WITH INVERTED V
unicode_id_continue(0x06F0, 0x06F9).	% ID_Continue Nd  [10] EXTENDED ARABIC-INDIC DIGIT ZERO..EXTENDED ARABIC-INDIC DIGIT NINE
unicode_id_continue(0x06FA, 0x06FC).	% ID_Continue Lo   [3] ARABIC LETTER SHEEN WITH DOT BELOW..ARABIC LETTER GHAIN WITH DOT BELOW
unicode_id_continue(0x06FF, 0x06FF).	% ID_Continue Lo       ARABIC LETTER HEH WITH INVERTED V
unicode_id_continue(0x0710, 0x0710).	% ID_Continue Lo       SYRIAC LETTER ALAPH
unicode_id_continue(0x0711, 0x0711).	% ID_Continue Mn       SYRIAC LETTER SUPERSCRIPT ALAPH
unicode_id_continue(0x0712, 0x072F).	% ID_Continue Lo  [30] SYRIAC LETTER BETH..SYRIAC LETTER PERSIAN DHALATH
unicode_id_continue(0x0730, 0x074A).	% ID_Continue Mn  [27] SYRIAC PTHAHA ABOVE..SYRIAC BARREKH
unicode_id_continue(0x074D, 0x07A5).	% ID_Continue Lo  [89] SYRIAC LETTER SOGDIAN ZHAIN..THAANA LETTER WAAVU
unicode_id_continue(0x07A6, 0x07B0).	% ID_Continue Mn  [11] THAANA ABAFILI..THAANA SUKUN
unicode_id_continue(0x07B1, 0x07B1).	% ID_Continue Lo       THAANA LETTER NAA
unicode_id_continue(0x07C0, 0x07C9).	% ID_Continue Nd  [10] NKO DIGIT ZERO..NKO DIGIT NINE
unicode_id_continue(0x07CA, 0x07EA).	% ID_Continue Lo  [33] NKO LETTER A..NKO LETTER JONA RA
unicode_id_continue(0x07EB, 0x07F3).	% ID_Continue Mn   [9] NKO COMBINING SHORT HIGH TONE..NKO COMBINING DOUBLE DOT ABOVE
unicode_id_continue(0x07F4, 0x07F5).	% ID_Continue Lm   [2] NKO HIGH TONE APOSTROPHE..NKO LOW TONE APOSTROPHE
unicode_id_continue(0x07FA, 0x07FA).	% ID_Continue Lm       NKO LAJANYALAN
unicode_id_continue(0x0800, 0x0815).	% ID_Continue Lo  [22] SAMARITAN LETTER ALAF..SAMARITAN LETTER TAAF
unicode_id_continue(0x0816, 0x0819).	% ID_Continue Mn   [4] SAMARITAN MARK IN..SAMARITAN MARK DAGESH
unicode_id_continue(0x081A, 0x081A).	% ID_Continue Lm       SAMARITAN MODIFIER LETTER EPENTHETIC YUT
unicode_id_continue(0x081B, 0x0823).	% ID_Continue Mn   [9] SAMARITAN MARK EPENTHETIC YUT..SAMARITAN VOWEL SIGN A
unicode_id_continue(0x0824, 0x0824).	% ID_Continue Lm       SAMARITAN MODIFIER LETTER SHORT A
unicode_id_continue(0x0825, 0x0827).	% ID_Continue Mn   [3] SAMARITAN VOWEL SIGN SHORT A..SAMARITAN VOWEL SIGN U
unicode_id_continue(0x0828, 0x0828).	% ID_Continue Lm       SAMARITAN MODIFIER LETTER I
unicode_id_continue(0x0829, 0x082D).	% ID_Continue Mn   [5] SAMARITAN VOWEL SIGN LONG I..SAMARITAN MARK NEQUDAA
unicode_id_continue(0x0840, 0x0858).	% ID_Continue Lo  [25] MANDAIC LETTER HALQA..MANDAIC LETTER AIN
unicode_id_continue(0x0859, 0x085B).	% ID_Continue Mn   [3] MANDAIC AFFRICATION MARK..MANDAIC GEMINATION MARK
unicode_id_continue(0x08A0, 0x08A0).	% ID_Continue Lo       ARABIC LETTER BEH WITH SMALL V BELOW
unicode_id_continue(0x08A2, 0x08AC).	% ID_Continue Lo  [11] ARABIC LETTER JEEM WITH TWO DOTS ABOVE..ARABIC LETTER ROHINGYA YEH
unicode_id_continue(0x08E4, 0x08FE).	% ID_Continue Mn  [27] ARABIC CURLY FATHA..ARABIC DAMMA WITH DOT
unicode_id_continue(0x0900, 0x0902).	% ID_Continue Mn   [3] DEVANAGARI SIGN INVERTED CANDRABINDU..DEVANAGARI SIGN ANUSVARA
unicode_id_continue(0x0903, 0x0903).	% ID_Continue Mc       DEVANAGARI SIGN VISARGA
unicode_id_continue(0x0904, 0x0939).	% ID_Continue Lo  [54] DEVANAGARI LETTER SHORT A..DEVANAGARI LETTER HA
unicode_id_continue(0x093A, 0x093A).	% ID_Continue Mn       DEVANAGARI VOWEL SIGN OE
unicode_id_continue(0x093B, 0x093B).	% ID_Continue Mc       DEVANAGARI VOWEL SIGN OOE
unicode_id_continue(0x093C, 0x093C).	% ID_Continue Mn       DEVANAGARI SIGN NUKTA
unicode_id_continue(0x093D, 0x093D).	% ID_Continue Lo       DEVANAGARI SIGN AVAGRAHA
unicode_id_continue(0x093E, 0x0940).	% ID_Continue Mc   [3] DEVANAGARI VOWEL SIGN AA..DEVANAGARI VOWEL SIGN II
unicode_id_continue(0x0941, 0x0948).	% ID_Continue Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
unicode_id_continue(0x0949, 0x094C).	% ID_Continue Mc   [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU
unicode_id_continue(0x094D, 0x094D).	% ID_Continue Mn       DEVANAGARI SIGN VIRAMA
unicode_id_continue(0x094E, 0x094F).	% ID_Continue Mc   [2] DEVANAGARI VOWEL SIGN PRISHTHAMATRA E..DEVANAGARI VOWEL SIGN AW
unicode_id_continue(0x0950, 0x0950).	% ID_Continue Lo       DEVANAGARI OM
unicode_id_continue(0x0951, 0x0957).	% ID_Continue Mn   [7] DEVANAGARI STRESS SIGN UDATTA..DEVANAGARI VOWEL SIGN UUE
unicode_id_continue(0x0958, 0x0961).	% ID_Continue Lo  [10] DEVANAGARI LETTER QA..DEVANAGARI LETTER VOCALIC LL
unicode_id_continue(0x0962, 0x0963).	% ID_Continue Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
unicode_id_continue(0x0966, 0x096F).	% ID_Continue Nd  [10] DEVANAGARI DIGIT ZERO..DEVANAGARI DIGIT NINE
unicode_id_continue(0x0971, 0x0971).	% ID_Continue Lm       DEVANAGARI SIGN HIGH SPACING DOT
unicode_id_continue(0x0972, 0x0977).	% ID_Continue Lo   [6] DEVANAGARI LETTER CANDRA A..DEVANAGARI LETTER UUE
unicode_id_continue(0x0979, 0x097F).	% ID_Continue Lo   [7] DEVANAGARI LETTER ZHA..DEVANAGARI LETTER BBA
unicode_id_continue(0x0981, 0x0981).	% ID_Continue Mn       BENGALI SIGN CANDRABINDU
unicode_id_continue(0x0982, 0x0983).	% ID_Continue Mc   [2] BENGALI SIGN ANUSVARA..BENGALI SIGN VISARGA
unicode_id_continue(0x0985, 0x098C).	% ID_Continue Lo   [8] BENGALI LETTER A..BENGALI LETTER VOCALIC L
unicode_id_continue(0x098F, 0x0990).	% ID_Continue Lo   [2] BENGALI LETTER E..BENGALI LETTER AI
unicode_id_continue(0x0993, 0x09A8).	% ID_Continue Lo  [22] BENGALI LETTER O..BENGALI LETTER NA
unicode_id_continue(0x09AA, 0x09B0).	% ID_Continue Lo   [7] BENGALI LETTER PA..BENGALI LETTER RA
unicode_id_continue(0x09B2, 0x09B2).	% ID_Continue Lo       BENGALI LETTER LA
unicode_id_continue(0x09B6, 0x09B9).	% ID_Continue Lo   [4] BENGALI LETTER SHA..BENGALI LETTER HA
unicode_id_continue(0x09BC, 0x09BC).	% ID_Continue Mn       BENGALI SIGN NUKTA
unicode_id_continue(0x09BD, 0x09BD).	% ID_Continue Lo       BENGALI SIGN AVAGRAHA
unicode_id_continue(0x09BE, 0x09C0).	% ID_Continue Mc   [3] BENGALI VOWEL SIGN AA..BENGALI VOWEL SIGN II
unicode_id_continue(0x09C1, 0x09C4).	% ID_Continue Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
unicode_id_continue(0x09C7, 0x09C8).	% ID_Continue Mc   [2] BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI
unicode_id_continue(0x09CB, 0x09CC).	% ID_Continue Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_id_continue(0x09CD, 0x09CD).	% ID_Continue Mn       BENGALI SIGN VIRAMA
unicode_id_continue(0x09CE, 0x09CE).	% ID_Continue Lo       BENGALI LETTER KHANDA TA
unicode_id_continue(0x09D7, 0x09D7).	% ID_Continue Mc       BENGALI AU LENGTH MARK
unicode_id_continue(0x09DC, 0x09DD).	% ID_Continue Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_id_continue(0x09DF, 0x09E1).	% ID_Continue Lo   [3] BENGALI LETTER YYA..BENGALI LETTER VOCALIC LL
unicode_id_continue(0x09E2, 0x09E3).	% ID_Continue Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
unicode_id_continue(0x09E6, 0x09EF).	% ID_Continue Nd  [10] BENGALI DIGIT ZERO..BENGALI DIGIT NINE
unicode_id_continue(0x09F0, 0x09F1).	% ID_Continue Lo   [2] BENGALI LETTER RA WITH MIDDLE DIAGONAL..BENGALI LETTER RA WITH LOWER DIAGONAL
unicode_id_continue(0x0A01, 0x0A02).	% ID_Continue Mn   [2] GURMUKHI SIGN ADAK BINDI..GURMUKHI SIGN BINDI
unicode_id_continue(0x0A03, 0x0A03).	% ID_Continue Mc       GURMUKHI SIGN VISARGA
unicode_id_continue(0x0A05, 0x0A0A).	% ID_Continue Lo   [6] GURMUKHI LETTER A..GURMUKHI LETTER UU
unicode_id_continue(0x0A0F, 0x0A10).	% ID_Continue Lo   [2] GURMUKHI LETTER EE..GURMUKHI LETTER AI
unicode_id_continue(0x0A13, 0x0A28).	% ID_Continue Lo  [22] GURMUKHI LETTER OO..GURMUKHI LETTER NA
unicode_id_continue(0x0A2A, 0x0A30).	% ID_Continue Lo   [7] GURMUKHI LETTER PA..GURMUKHI LETTER RA
unicode_id_continue(0x0A32, 0x0A33).	% ID_Continue Lo   [2] GURMUKHI LETTER LA..GURMUKHI LETTER LLA
unicode_id_continue(0x0A35, 0x0A36).	% ID_Continue Lo   [2] GURMUKHI LETTER VA..GURMUKHI LETTER SHA
unicode_id_continue(0x0A38, 0x0A39).	% ID_Continue Lo   [2] GURMUKHI LETTER SA..GURMUKHI LETTER HA
unicode_id_continue(0x0A3C, 0x0A3C).	% ID_Continue Mn       GURMUKHI SIGN NUKTA
unicode_id_continue(0x0A3E, 0x0A40).	% ID_Continue Mc   [3] GURMUKHI VOWEL SIGN AA..GURMUKHI VOWEL SIGN II
unicode_id_continue(0x0A41, 0x0A42).	% ID_Continue Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
unicode_id_continue(0x0A47, 0x0A48).	% ID_Continue Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
unicode_id_continue(0x0A4B, 0x0A4D).	% ID_Continue Mn   [3] GURMUKHI VOWEL SIGN OO..GURMUKHI SIGN VIRAMA
unicode_id_continue(0x0A51, 0x0A51).	% ID_Continue Mn       GURMUKHI SIGN UDAAT
unicode_id_continue(0x0A59, 0x0A5C).	% ID_Continue Lo   [4] GURMUKHI LETTER KHHA..GURMUKHI LETTER RRA
unicode_id_continue(0x0A5E, 0x0A5E).	% ID_Continue Lo       GURMUKHI LETTER FA
unicode_id_continue(0x0A66, 0x0A6F).	% ID_Continue Nd  [10] GURMUKHI DIGIT ZERO..GURMUKHI DIGIT NINE
unicode_id_continue(0x0A70, 0x0A71).	% ID_Continue Mn   [2] GURMUKHI TIPPI..GURMUKHI ADDAK
unicode_id_continue(0x0A72, 0x0A74).	% ID_Continue Lo   [3] GURMUKHI IRI..GURMUKHI EK ONKAR
unicode_id_continue(0x0A75, 0x0A75).	% ID_Continue Mn       GURMUKHI SIGN YAKASH
unicode_id_continue(0x0A81, 0x0A82).	% ID_Continue Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
unicode_id_continue(0x0A83, 0x0A83).	% ID_Continue Mc       GUJARATI SIGN VISARGA
unicode_id_continue(0x0A85, 0x0A8D).	% ID_Continue Lo   [9] GUJARATI LETTER A..GUJARATI VOWEL CANDRA E
unicode_id_continue(0x0A8F, 0x0A91).	% ID_Continue Lo   [3] GUJARATI LETTER E..GUJARATI VOWEL CANDRA O
unicode_id_continue(0x0A93, 0x0AA8).	% ID_Continue Lo  [22] GUJARATI LETTER O..GUJARATI LETTER NA
unicode_id_continue(0x0AAA, 0x0AB0).	% ID_Continue Lo   [7] GUJARATI LETTER PA..GUJARATI LETTER RA
unicode_id_continue(0x0AB2, 0x0AB3).	% ID_Continue Lo   [2] GUJARATI LETTER LA..GUJARATI LETTER LLA
unicode_id_continue(0x0AB5, 0x0AB9).	% ID_Continue Lo   [5] GUJARATI LETTER VA..GUJARATI LETTER HA
unicode_id_continue(0x0ABC, 0x0ABC).	% ID_Continue Mn       GUJARATI SIGN NUKTA
unicode_id_continue(0x0ABD, 0x0ABD).	% ID_Continue Lo       GUJARATI SIGN AVAGRAHA
unicode_id_continue(0x0ABE, 0x0AC0).	% ID_Continue Mc   [3] GUJARATI VOWEL SIGN AA..GUJARATI VOWEL SIGN II
unicode_id_continue(0x0AC1, 0x0AC5).	% ID_Continue Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
unicode_id_continue(0x0AC7, 0x0AC8).	% ID_Continue Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
unicode_id_continue(0x0AC9, 0x0AC9).	% ID_Continue Mc       GUJARATI VOWEL SIGN CANDRA O
unicode_id_continue(0x0ACB, 0x0ACC).	% ID_Continue Mc   [2] GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU
unicode_id_continue(0x0ACD, 0x0ACD).	% ID_Continue Mn       GUJARATI SIGN VIRAMA
unicode_id_continue(0x0AD0, 0x0AD0).	% ID_Continue Lo       GUJARATI OM
unicode_id_continue(0x0AE0, 0x0AE1).	% ID_Continue Lo   [2] GUJARATI LETTER VOCALIC RR..GUJARATI LETTER VOCALIC LL
unicode_id_continue(0x0AE2, 0x0AE3).	% ID_Continue Mn   [2] GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL
unicode_id_continue(0x0AE6, 0x0AEF).	% ID_Continue Nd  [10] GUJARATI DIGIT ZERO..GUJARATI DIGIT NINE
unicode_id_continue(0x0B01, 0x0B01).	% ID_Continue Mn       ORIYA SIGN CANDRABINDU
unicode_id_continue(0x0B02, 0x0B03).	% ID_Continue Mc   [2] ORIYA SIGN ANUSVARA..ORIYA SIGN VISARGA
unicode_id_continue(0x0B05, 0x0B0C).	% ID_Continue Lo   [8] ORIYA LETTER A..ORIYA LETTER VOCALIC L
unicode_id_continue(0x0B0F, 0x0B10).	% ID_Continue Lo   [2] ORIYA LETTER E..ORIYA LETTER AI
unicode_id_continue(0x0B13, 0x0B28).	% ID_Continue Lo  [22] ORIYA LETTER O..ORIYA LETTER NA
unicode_id_continue(0x0B2A, 0x0B30).	% ID_Continue Lo   [7] ORIYA LETTER PA..ORIYA LETTER RA
unicode_id_continue(0x0B32, 0x0B33).	% ID_Continue Lo   [2] ORIYA LETTER LA..ORIYA LETTER LLA
unicode_id_continue(0x0B35, 0x0B39).	% ID_Continue Lo   [5] ORIYA LETTER VA..ORIYA LETTER HA
unicode_id_continue(0x0B3C, 0x0B3C).	% ID_Continue Mn       ORIYA SIGN NUKTA
unicode_id_continue(0x0B3D, 0x0B3D).	% ID_Continue Lo       ORIYA SIGN AVAGRAHA
unicode_id_continue(0x0B3E, 0x0B3E).	% ID_Continue Mc       ORIYA VOWEL SIGN AA
unicode_id_continue(0x0B3F, 0x0B3F).	% ID_Continue Mn       ORIYA VOWEL SIGN I
unicode_id_continue(0x0B40, 0x0B40).	% ID_Continue Mc       ORIYA VOWEL SIGN II
unicode_id_continue(0x0B41, 0x0B44).	% ID_Continue Mn   [4] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR
unicode_id_continue(0x0B47, 0x0B48).	% ID_Continue Mc   [2] ORIYA VOWEL SIGN E..ORIYA VOWEL SIGN AI
unicode_id_continue(0x0B4B, 0x0B4C).	% ID_Continue Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
unicode_id_continue(0x0B4D, 0x0B4D).	% ID_Continue Mn       ORIYA SIGN VIRAMA
unicode_id_continue(0x0B56, 0x0B56).	% ID_Continue Mn       ORIYA AI LENGTH MARK
unicode_id_continue(0x0B57, 0x0B57).	% ID_Continue Mc       ORIYA AU LENGTH MARK
unicode_id_continue(0x0B5C, 0x0B5D).	% ID_Continue Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_id_continue(0x0B5F, 0x0B61).	% ID_Continue Lo   [3] ORIYA LETTER YYA..ORIYA LETTER VOCALIC LL
unicode_id_continue(0x0B62, 0x0B63).	% ID_Continue Mn   [2] ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL
unicode_id_continue(0x0B66, 0x0B6F).	% ID_Continue Nd  [10] ORIYA DIGIT ZERO..ORIYA DIGIT NINE
unicode_id_continue(0x0B71, 0x0B71).	% ID_Continue Lo       ORIYA LETTER WA
unicode_id_continue(0x0B82, 0x0B82).	% ID_Continue Mn       TAMIL SIGN ANUSVARA
unicode_id_continue(0x0B83, 0x0B83).	% ID_Continue Lo       TAMIL SIGN VISARGA
unicode_id_continue(0x0B85, 0x0B8A).	% ID_Continue Lo   [6] TAMIL LETTER A..TAMIL LETTER UU
unicode_id_continue(0x0B8E, 0x0B90).	% ID_Continue Lo   [3] TAMIL LETTER E..TAMIL LETTER AI
unicode_id_continue(0x0B92, 0x0B95).	% ID_Continue Lo   [4] TAMIL LETTER O..TAMIL LETTER KA
unicode_id_continue(0x0B99, 0x0B9A).	% ID_Continue Lo   [2] TAMIL LETTER NGA..TAMIL LETTER CA
unicode_id_continue(0x0B9C, 0x0B9C).	% ID_Continue Lo       TAMIL LETTER JA
unicode_id_continue(0x0B9E, 0x0B9F).	% ID_Continue Lo   [2] TAMIL LETTER NYA..TAMIL LETTER TTA
unicode_id_continue(0x0BA3, 0x0BA4).	% ID_Continue Lo   [2] TAMIL LETTER NNA..TAMIL LETTER TA
unicode_id_continue(0x0BA8, 0x0BAA).	% ID_Continue Lo   [3] TAMIL LETTER NA..TAMIL LETTER PA
unicode_id_continue(0x0BAE, 0x0BB9).	% ID_Continue Lo  [12] TAMIL LETTER MA..TAMIL LETTER HA
unicode_id_continue(0x0BBE, 0x0BBF).	% ID_Continue Mc   [2] TAMIL VOWEL SIGN AA..TAMIL VOWEL SIGN I
unicode_id_continue(0x0BC0, 0x0BC0).	% ID_Continue Mn       TAMIL VOWEL SIGN II
unicode_id_continue(0x0BC1, 0x0BC2).	% ID_Continue Mc   [2] TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU
unicode_id_continue(0x0BC6, 0x0BC8).	% ID_Continue Mc   [3] TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI
unicode_id_continue(0x0BCA, 0x0BCC).	% ID_Continue Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_id_continue(0x0BCD, 0x0BCD).	% ID_Continue Mn       TAMIL SIGN VIRAMA
unicode_id_continue(0x0BD0, 0x0BD0).	% ID_Continue Lo       TAMIL OM
unicode_id_continue(0x0BD7, 0x0BD7).	% ID_Continue Mc       TAMIL AU LENGTH MARK
unicode_id_continue(0x0BE6, 0x0BEF).	% ID_Continue Nd  [10] TAMIL DIGIT ZERO..TAMIL DIGIT NINE
unicode_id_continue(0x0C01, 0x0C03).	% ID_Continue Mc   [3] TELUGU SIGN CANDRABINDU..TELUGU SIGN VISARGA
unicode_id_continue(0x0C05, 0x0C0C).	% ID_Continue Lo   [8] TELUGU LETTER A..TELUGU LETTER VOCALIC L
unicode_id_continue(0x0C0E, 0x0C10).	% ID_Continue Lo   [3] TELUGU LETTER E..TELUGU LETTER AI
unicode_id_continue(0x0C12, 0x0C28).	% ID_Continue Lo  [23] TELUGU LETTER O..TELUGU LETTER NA
unicode_id_continue(0x0C2A, 0x0C33).	% ID_Continue Lo  [10] TELUGU LETTER PA..TELUGU LETTER LLA
unicode_id_continue(0x0C35, 0x0C39).	% ID_Continue Lo   [5] TELUGU LETTER VA..TELUGU LETTER HA
unicode_id_continue(0x0C3D, 0x0C3D).	% ID_Continue Lo       TELUGU SIGN AVAGRAHA
unicode_id_continue(0x0C3E, 0x0C40).	% ID_Continue Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
unicode_id_continue(0x0C41, 0x0C44).	% ID_Continue Mc   [4] TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR
unicode_id_continue(0x0C46, 0x0C48).	% ID_Continue Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
unicode_id_continue(0x0C4A, 0x0C4D).	% ID_Continue Mn   [4] TELUGU VOWEL SIGN O..TELUGU SIGN VIRAMA
unicode_id_continue(0x0C55, 0x0C56).	% ID_Continue Mn   [2] TELUGU LENGTH MARK..TELUGU AI LENGTH MARK
unicode_id_continue(0x0C58, 0x0C59).	% ID_Continue Lo   [2] TELUGU LETTER TSA..TELUGU LETTER DZA
unicode_id_continue(0x0C60, 0x0C61).	% ID_Continue Lo   [2] TELUGU LETTER VOCALIC RR..TELUGU LETTER VOCALIC LL
unicode_id_continue(0x0C62, 0x0C63).	% ID_Continue Mn   [2] TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL
unicode_id_continue(0x0C66, 0x0C6F).	% ID_Continue Nd  [10] TELUGU DIGIT ZERO..TELUGU DIGIT NINE
unicode_id_continue(0x0C82, 0x0C83).	% ID_Continue Mc   [2] KANNADA SIGN ANUSVARA..KANNADA SIGN VISARGA
unicode_id_continue(0x0C85, 0x0C8C).	% ID_Continue Lo   [8] KANNADA LETTER A..KANNADA LETTER VOCALIC L
unicode_id_continue(0x0C8E, 0x0C90).	% ID_Continue Lo   [3] KANNADA LETTER E..KANNADA LETTER AI
unicode_id_continue(0x0C92, 0x0CA8).	% ID_Continue Lo  [23] KANNADA LETTER O..KANNADA LETTER NA
unicode_id_continue(0x0CAA, 0x0CB3).	% ID_Continue Lo  [10] KANNADA LETTER PA..KANNADA LETTER LLA
unicode_id_continue(0x0CB5, 0x0CB9).	% ID_Continue Lo   [5] KANNADA LETTER VA..KANNADA LETTER HA
unicode_id_continue(0x0CBC, 0x0CBC).	% ID_Continue Mn       KANNADA SIGN NUKTA
unicode_id_continue(0x0CBD, 0x0CBD).	% ID_Continue Lo       KANNADA SIGN AVAGRAHA
unicode_id_continue(0x0CBE, 0x0CBE).	% ID_Continue Mc       KANNADA VOWEL SIGN AA
unicode_id_continue(0x0CBF, 0x0CBF).	% ID_Continue Mn       KANNADA VOWEL SIGN I
unicode_id_continue(0x0CC0, 0x0CC4).	% ID_Continue Mc   [5] KANNADA VOWEL SIGN II..KANNADA VOWEL SIGN VOCALIC RR
unicode_id_continue(0x0CC6, 0x0CC6).	% ID_Continue Mn       KANNADA VOWEL SIGN E
unicode_id_continue(0x0CC7, 0x0CC8).	% ID_Continue Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_id_continue(0x0CCA, 0x0CCB).	% ID_Continue Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_id_continue(0x0CCC, 0x0CCD).	% ID_Continue Mn   [2] KANNADA VOWEL SIGN AU..KANNADA SIGN VIRAMA
unicode_id_continue(0x0CD5, 0x0CD6).	% ID_Continue Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
unicode_id_continue(0x0CDE, 0x0CDE).	% ID_Continue Lo       KANNADA LETTER FA
unicode_id_continue(0x0CE0, 0x0CE1).	% ID_Continue Lo   [2] KANNADA LETTER VOCALIC RR..KANNADA LETTER VOCALIC LL
unicode_id_continue(0x0CE2, 0x0CE3).	% ID_Continue Mn   [2] KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL
unicode_id_continue(0x0CE6, 0x0CEF).	% ID_Continue Nd  [10] KANNADA DIGIT ZERO..KANNADA DIGIT NINE
unicode_id_continue(0x0CF1, 0x0CF2).	% ID_Continue Lo   [2] KANNADA SIGN JIHVAMULIYA..KANNADA SIGN UPADHMANIYA
unicode_id_continue(0x0D02, 0x0D03).	% ID_Continue Mc   [2] MALAYALAM SIGN ANUSVARA..MALAYALAM SIGN VISARGA
unicode_id_continue(0x0D05, 0x0D0C).	% ID_Continue Lo   [8] MALAYALAM LETTER A..MALAYALAM LETTER VOCALIC L
unicode_id_continue(0x0D0E, 0x0D10).	% ID_Continue Lo   [3] MALAYALAM LETTER E..MALAYALAM LETTER AI
unicode_id_continue(0x0D12, 0x0D3A).	% ID_Continue Lo  [41] MALAYALAM LETTER O..MALAYALAM LETTER TTTA
unicode_id_continue(0x0D3D, 0x0D3D).	% ID_Continue Lo       MALAYALAM SIGN AVAGRAHA
unicode_id_continue(0x0D3E, 0x0D40).	% ID_Continue Mc   [3] MALAYALAM VOWEL SIGN AA..MALAYALAM VOWEL SIGN II
unicode_id_continue(0x0D41, 0x0D44).	% ID_Continue Mn   [4] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC RR
unicode_id_continue(0x0D46, 0x0D48).	% ID_Continue Mc   [3] MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI
unicode_id_continue(0x0D4A, 0x0D4C).	% ID_Continue Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_id_continue(0x0D4D, 0x0D4D).	% ID_Continue Mn       MALAYALAM SIGN VIRAMA
unicode_id_continue(0x0D4E, 0x0D4E).	% ID_Continue Lo       MALAYALAM LETTER DOT REPH
unicode_id_continue(0x0D57, 0x0D57).	% ID_Continue Mc       MALAYALAM AU LENGTH MARK
unicode_id_continue(0x0D60, 0x0D61).	% ID_Continue Lo   [2] MALAYALAM LETTER VOCALIC RR..MALAYALAM LETTER VOCALIC LL
unicode_id_continue(0x0D62, 0x0D63).	% ID_Continue Mn   [2] MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL
unicode_id_continue(0x0D66, 0x0D6F).	% ID_Continue Nd  [10] MALAYALAM DIGIT ZERO..MALAYALAM DIGIT NINE
unicode_id_continue(0x0D7A, 0x0D7F).	% ID_Continue Lo   [6] MALAYALAM LETTER CHILLU NN..MALAYALAM LETTER CHILLU K
unicode_id_continue(0x0D82, 0x0D83).	% ID_Continue Mc   [2] SINHALA SIGN ANUSVARAYA..SINHALA SIGN VISARGAYA
unicode_id_continue(0x0D85, 0x0D96).	% ID_Continue Lo  [18] SINHALA LETTER AYANNA..SINHALA LETTER AUYANNA
unicode_id_continue(0x0D9A, 0x0DB1).	% ID_Continue Lo  [24] SINHALA LETTER ALPAPRAANA KAYANNA..SINHALA LETTER DANTAJA NAYANNA
unicode_id_continue(0x0DB3, 0x0DBB).	% ID_Continue Lo   [9] SINHALA LETTER SANYAKA DAYANNA..SINHALA LETTER RAYANNA
unicode_id_continue(0x0DBD, 0x0DBD).	% ID_Continue Lo       SINHALA LETTER DANTAJA LAYANNA
unicode_id_continue(0x0DC0, 0x0DC6).	% ID_Continue Lo   [7] SINHALA LETTER VAYANNA..SINHALA LETTER FAYANNA
unicode_id_continue(0x0DCA, 0x0DCA).	% ID_Continue Mn       SINHALA SIGN AL-LAKUNA
unicode_id_continue(0x0DCF, 0x0DD1).	% ID_Continue Mc   [3] SINHALA VOWEL SIGN AELA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA
unicode_id_continue(0x0DD2, 0x0DD4).	% ID_Continue Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
unicode_id_continue(0x0DD6, 0x0DD6).	% ID_Continue Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
unicode_id_continue(0x0DD8, 0x0DDF).	% ID_Continue Mc   [8] SINHALA VOWEL SIGN GAETTA-PILLA..SINHALA VOWEL SIGN GAYANUKITTA
unicode_id_continue(0x0DF2, 0x0DF3).	% ID_Continue Mc   [2] SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA
unicode_id_continue(0x0E01, 0x0E30).	% ID_Continue Lo  [48] THAI CHARACTER KO KAI..THAI CHARACTER SARA A
unicode_id_continue(0x0E31, 0x0E31).	% ID_Continue Mn       THAI CHARACTER MAI HAN-AKAT
unicode_id_continue(0x0E32, 0x0E33).	% ID_Continue Lo   [2] THAI CHARACTER SARA AA..THAI CHARACTER SARA AM
unicode_id_continue(0x0E34, 0x0E3A).	% ID_Continue Mn   [7] THAI CHARACTER SARA I..THAI CHARACTER PHINTHU
unicode_id_continue(0x0E40, 0x0E45).	% ID_Continue Lo   [6] THAI CHARACTER SARA E..THAI CHARACTER LAKKHANGYAO
unicode_id_continue(0x0E46, 0x0E46).	% ID_Continue Lm       THAI CHARACTER MAIYAMOK
unicode_id_continue(0x0E47, 0x0E4E).	% ID_Continue Mn   [8] THAI CHARACTER MAITAIKHU..THAI CHARACTER YAMAKKAN
unicode_id_continue(0x0E50, 0x0E59).	% ID_Continue Nd  [10] THAI DIGIT ZERO..THAI DIGIT NINE
unicode_id_continue(0x0E81, 0x0E82).	% ID_Continue Lo   [2] LAO LETTER KO..LAO LETTER KHO SUNG
unicode_id_continue(0x0E84, 0x0E84).	% ID_Continue Lo       LAO LETTER KHO TAM
unicode_id_continue(0x0E87, 0x0E88).	% ID_Continue Lo   [2] LAO LETTER NGO..LAO LETTER CO
unicode_id_continue(0x0E8A, 0x0E8A).	% ID_Continue Lo       LAO LETTER SO TAM
unicode_id_continue(0x0E8D, 0x0E8D).	% ID_Continue Lo       LAO LETTER NYO
unicode_id_continue(0x0E94, 0x0E97).	% ID_Continue Lo   [4] LAO LETTER DO..LAO LETTER THO TAM
unicode_id_continue(0x0E99, 0x0E9F).	% ID_Continue Lo   [7] LAO LETTER NO..LAO LETTER FO SUNG
unicode_id_continue(0x0EA1, 0x0EA3).	% ID_Continue Lo   [3] LAO LETTER MO..LAO LETTER LO LING
unicode_id_continue(0x0EA5, 0x0EA5).	% ID_Continue Lo       LAO LETTER LO LOOT
unicode_id_continue(0x0EA7, 0x0EA7).	% ID_Continue Lo       LAO LETTER WO
unicode_id_continue(0x0EAA, 0x0EAB).	% ID_Continue Lo   [2] LAO LETTER SO SUNG..LAO LETTER HO SUNG
unicode_id_continue(0x0EAD, 0x0EB0).	% ID_Continue Lo   [4] LAO LETTER O..LAO VOWEL SIGN A
unicode_id_continue(0x0EB1, 0x0EB1).	% ID_Continue Mn       LAO VOWEL SIGN MAI KAN
unicode_id_continue(0x0EB2, 0x0EB3).	% ID_Continue Lo   [2] LAO VOWEL SIGN AA..LAO VOWEL SIGN AM
unicode_id_continue(0x0EB4, 0x0EB9).	% ID_Continue Mn   [6] LAO VOWEL SIGN I..LAO VOWEL SIGN UU
unicode_id_continue(0x0EBB, 0x0EBC).	% ID_Continue Mn   [2] LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO
unicode_id_continue(0x0EBD, 0x0EBD).	% ID_Continue Lo       LAO SEMIVOWEL SIGN NYO
unicode_id_continue(0x0EC0, 0x0EC4).	% ID_Continue Lo   [5] LAO VOWEL SIGN E..LAO VOWEL SIGN AI
unicode_id_continue(0x0EC6, 0x0EC6).	% ID_Continue Lm       LAO KO LA
unicode_id_continue(0x0EC8, 0x0ECD).	% ID_Continue Mn   [6] LAO TONE MAI EK..LAO NIGGAHITA
unicode_id_continue(0x0ED0, 0x0ED9).	% ID_Continue Nd  [10] LAO DIGIT ZERO..LAO DIGIT NINE
unicode_id_continue(0x0EDC, 0x0EDF).	% ID_Continue Lo   [4] LAO HO NO..LAO LETTER KHMU NYO
unicode_id_continue(0x0F00, 0x0F00).	% ID_Continue Lo       TIBETAN SYLLABLE OM
unicode_id_continue(0x0F18, 0x0F19).	% ID_Continue Mn   [2] TIBETAN ASTROLOGICAL SIGN -KHYUD PA..TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS
unicode_id_continue(0x0F20, 0x0F29).	% ID_Continue Nd  [10] TIBETAN DIGIT ZERO..TIBETAN DIGIT NINE
unicode_id_continue(0x0F35, 0x0F35).	% ID_Continue Mn       TIBETAN MARK NGAS BZUNG NYI ZLA
unicode_id_continue(0x0F37, 0x0F37).	% ID_Continue Mn       TIBETAN MARK NGAS BZUNG SGOR RTAGS
unicode_id_continue(0x0F39, 0x0F39).	% ID_Continue Mn       TIBETAN MARK TSA -PHRU
unicode_id_continue(0x0F3E, 0x0F3F).	% ID_Continue Mc   [2] TIBETAN SIGN YAR TSHES..TIBETAN SIGN MAR TSHES
unicode_id_continue(0x0F40, 0x0F47).	% ID_Continue Lo   [8] TIBETAN LETTER KA..TIBETAN LETTER JA
unicode_id_continue(0x0F49, 0x0F6C).	% ID_Continue Lo  [36] TIBETAN LETTER NYA..TIBETAN LETTER RRA
unicode_id_continue(0x0F71, 0x0F7E).	% ID_Continue Mn  [14] TIBETAN VOWEL SIGN AA..TIBETAN SIGN RJES SU NGA RO
unicode_id_continue(0x0F7F, 0x0F7F).	% ID_Continue Mc       TIBETAN SIGN RNAM BCAD
unicode_id_continue(0x0F80, 0x0F84).	% ID_Continue Mn   [5] TIBETAN VOWEL SIGN REVERSED I..TIBETAN MARK HALANTA
unicode_id_continue(0x0F86, 0x0F87).	% ID_Continue Mn   [2] TIBETAN SIGN LCI RTAGS..TIBETAN SIGN YANG RTAGS
unicode_id_continue(0x0F88, 0x0F8C).	% ID_Continue Lo   [5] TIBETAN SIGN LCE TSA CAN..TIBETAN SIGN INVERTED MCHU CAN
unicode_id_continue(0x0F8D, 0x0F97).	% ID_Continue Mn  [11] TIBETAN SUBJOINED SIGN LCE TSA CAN..TIBETAN SUBJOINED LETTER JA
unicode_id_continue(0x0F99, 0x0FBC).	% ID_Continue Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
unicode_id_continue(0x0FC6, 0x0FC6).	% ID_Continue Mn       TIBETAN SYMBOL PADMA GDAN
unicode_id_continue(0x1000, 0x102A).	% ID_Continue Lo  [43] MYANMAR LETTER KA..MYANMAR LETTER AU
unicode_id_continue(0x102B, 0x102C).	% ID_Continue Mc   [2] MYANMAR VOWEL SIGN TALL AA..MYANMAR VOWEL SIGN AA
unicode_id_continue(0x102D, 0x1030).	% ID_Continue Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
unicode_id_continue(0x1031, 0x1031).	% ID_Continue Mc       MYANMAR VOWEL SIGN E
unicode_id_continue(0x1032, 0x1037).	% ID_Continue Mn   [6] MYANMAR VOWEL SIGN AI..MYANMAR SIGN DOT BELOW
unicode_id_continue(0x1038, 0x1038).	% ID_Continue Mc       MYANMAR SIGN VISARGA
unicode_id_continue(0x1039, 0x103A).	% ID_Continue Mn   [2] MYANMAR SIGN VIRAMA..MYANMAR SIGN ASAT
unicode_id_continue(0x103B, 0x103C).	% ID_Continue Mc   [2] MYANMAR CONSONANT SIGN MEDIAL YA..MYANMAR CONSONANT SIGN MEDIAL RA
unicode_id_continue(0x103D, 0x103E).	% ID_Continue Mn   [2] MYANMAR CONSONANT SIGN MEDIAL WA..MYANMAR CONSONANT SIGN MEDIAL HA
unicode_id_continue(0x103F, 0x103F).	% ID_Continue Lo       MYANMAR LETTER GREAT SA
unicode_id_continue(0x1040, 0x1049).	% ID_Continue Nd  [10] MYANMAR DIGIT ZERO..MYANMAR DIGIT NINE
unicode_id_continue(0x1050, 0x1055).	% ID_Continue Lo   [6] MYANMAR LETTER SHA..MYANMAR LETTER VOCALIC LL
unicode_id_continue(0x1056, 0x1057).	% ID_Continue Mc   [2] MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR
unicode_id_continue(0x1058, 0x1059).	% ID_Continue Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
unicode_id_continue(0x105A, 0x105D).	% ID_Continue Lo   [4] MYANMAR LETTER MON NGA..MYANMAR LETTER MON BBE
unicode_id_continue(0x105E, 0x1060).	% ID_Continue Mn   [3] MYANMAR CONSONANT SIGN MON MEDIAL NA..MYANMAR CONSONANT SIGN MON MEDIAL LA
unicode_id_continue(0x1061, 0x1061).	% ID_Continue Lo       MYANMAR LETTER SGAW KAREN SHA
unicode_id_continue(0x1062, 0x1064).	% ID_Continue Mc   [3] MYANMAR VOWEL SIGN SGAW KAREN EU..MYANMAR TONE MARK SGAW KAREN KE PHO
unicode_id_continue(0x1065, 0x1066).	% ID_Continue Lo   [2] MYANMAR LETTER WESTERN PWO KAREN THA..MYANMAR LETTER WESTERN PWO KAREN PWA
unicode_id_continue(0x1067, 0x106D).	% ID_Continue Mc   [7] MYANMAR VOWEL SIGN WESTERN PWO KAREN EU..MYANMAR SIGN WESTERN PWO KAREN TONE-5
unicode_id_continue(0x106E, 0x1070).	% ID_Continue Lo   [3] MYANMAR LETTER EASTERN PWO KAREN NNA..MYANMAR LETTER EASTERN PWO KAREN GHWA
unicode_id_continue(0x1071, 0x1074).	% ID_Continue Mn   [4] MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE
unicode_id_continue(0x1075, 0x1081).	% ID_Continue Lo  [13] MYANMAR LETTER SHAN KA..MYANMAR LETTER SHAN HA
unicode_id_continue(0x1082, 0x1082).	% ID_Continue Mn       MYANMAR CONSONANT SIGN SHAN MEDIAL WA
unicode_id_continue(0x1083, 0x1084).	% ID_Continue Mc   [2] MYANMAR VOWEL SIGN SHAN AA..MYANMAR VOWEL SIGN SHAN E
unicode_id_continue(0x1085, 0x1086).	% ID_Continue Mn   [2] MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y
unicode_id_continue(0x1087, 0x108C).	% ID_Continue Mc   [6] MYANMAR SIGN SHAN TONE-2..MYANMAR SIGN SHAN COUNCIL TONE-3
unicode_id_continue(0x108D, 0x108D).	% ID_Continue Mn       MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE
unicode_id_continue(0x108E, 0x108E).	% ID_Continue Lo       MYANMAR LETTER RUMAI PALAUNG FA
unicode_id_continue(0x108F, 0x108F).	% ID_Continue Mc       MYANMAR SIGN RUMAI PALAUNG TONE-5
unicode_id_continue(0x1090, 0x1099).	% ID_Continue Nd  [10] MYANMAR SHAN DIGIT ZERO..MYANMAR SHAN DIGIT NINE
unicode_id_continue(0x109A, 0x109C).	% ID_Continue Mc   [3] MYANMAR SIGN KHAMTI TONE-1..MYANMAR VOWEL SIGN AITON A
unicode_id_continue(0x109D, 0x109D).	% ID_Continue Mn       MYANMAR VOWEL SIGN AITON AI
unicode_id_continue(0x10A0, 0x10C5).	% ID_Continue L&  [38] GEORGIAN CAPITAL LETTER AN..GEORGIAN CAPITAL LETTER HOE
unicode_id_continue(0x10C7, 0x10C7).	% ID_Continue L&       GEORGIAN CAPITAL LETTER YN
unicode_id_continue(0x10CD, 0x10CD).	% ID_Continue L&       GEORGIAN CAPITAL LETTER AEN
unicode_id_continue(0x10D0, 0x10FA).	% ID_Continue Lo  [43] GEORGIAN LETTER AN..GEORGIAN LETTER AIN
unicode_id_continue(0x10FC, 0x10FC).	% ID_Continue Lm       MODIFIER LETTER GEORGIAN NAR
unicode_id_continue(0x10FD, 0x1248).	% ID_Continue Lo [332] GEORGIAN LETTER AEN..ETHIOPIC SYLLABLE QWA
unicode_id_continue(0x124A, 0x124D).	% ID_Continue Lo   [4] ETHIOPIC SYLLABLE QWI..ETHIOPIC SYLLABLE QWE
unicode_id_continue(0x1250, 0x1256).	% ID_Continue Lo   [7] ETHIOPIC SYLLABLE QHA..ETHIOPIC SYLLABLE QHO
unicode_id_continue(0x1258, 0x1258).	% ID_Continue Lo       ETHIOPIC SYLLABLE QHWA
unicode_id_continue(0x125A, 0x125D).	% ID_Continue Lo   [4] ETHIOPIC SYLLABLE QHWI..ETHIOPIC SYLLABLE QHWE
unicode_id_continue(0x1260, 0x1288).	% ID_Continue Lo  [41] ETHIOPIC SYLLABLE BA..ETHIOPIC SYLLABLE XWA
unicode_id_continue(0x128A, 0x128D).	% ID_Continue Lo   [4] ETHIOPIC SYLLABLE XWI..ETHIOPIC SYLLABLE XWE
unicode_id_continue(0x1290, 0x12B0).	% ID_Continue Lo  [33] ETHIOPIC SYLLABLE NA..ETHIOPIC SYLLABLE KWA
unicode_id_continue(0x12B2, 0x12B5).	% ID_Continue Lo   [4] ETHIOPIC SYLLABLE KWI..ETHIOPIC SYLLABLE KWE
unicode_id_continue(0x12B8, 0x12BE).	% ID_Continue Lo   [7] ETHIOPIC SYLLABLE KXA..ETHIOPIC SYLLABLE KXO
unicode_id_continue(0x12C0, 0x12C0).	% ID_Continue Lo       ETHIOPIC SYLLABLE KXWA
unicode_id_continue(0x12C2, 0x12C5).	% ID_Continue Lo   [4] ETHIOPIC SYLLABLE KXWI..ETHIOPIC SYLLABLE KXWE
unicode_id_continue(0x12C8, 0x12D6).	% ID_Continue Lo  [15] ETHIOPIC SYLLABLE WA..ETHIOPIC SYLLABLE PHARYNGEAL O
unicode_id_continue(0x12D8, 0x1310).	% ID_Continue Lo  [57] ETHIOPIC SYLLABLE ZA..ETHIOPIC SYLLABLE GWA
unicode_id_continue(0x1312, 0x1315).	% ID_Continue Lo   [4] ETHIOPIC SYLLABLE GWI..ETHIOPIC SYLLABLE GWE
unicode_id_continue(0x1318, 0x135A).	% ID_Continue Lo  [67] ETHIOPIC SYLLABLE GGA..ETHIOPIC SYLLABLE FYA
unicode_id_continue(0x135D, 0x135F).	% ID_Continue Mn   [3] ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK..ETHIOPIC COMBINING GEMINATION MARK
unicode_id_continue(0x1369, 0x1371).	% ID_Continue No   [9] ETHIOPIC DIGIT ONE..ETHIOPIC DIGIT NINE
unicode_id_continue(0x1380, 0x138F).	% ID_Continue Lo  [16] ETHIOPIC SYLLABLE SEBATBEIT MWA..ETHIOPIC SYLLABLE PWE
unicode_id_continue(0x13A0, 0x13F4).	% ID_Continue Lo  [85] CHEROKEE LETTER A..CHEROKEE LETTER YV
unicode_id_continue(0x1401, 0x166C).	% ID_Continue Lo [620] CANADIAN SYLLABICS E..CANADIAN SYLLABICS CARRIER TTSA
unicode_id_continue(0x166F, 0x167F).	% ID_Continue Lo  [17] CANADIAN SYLLABICS QAI..CANADIAN SYLLABICS BLACKFOOT W
unicode_id_continue(0x1681, 0x169A).	% ID_Continue Lo  [26] OGHAM LETTER BEITH..OGHAM LETTER PEITH
unicode_id_continue(0x16A0, 0x16EA).	% ID_Continue Lo  [75] RUNIC LETTER FEHU FEOH FE F..RUNIC LETTER X
unicode_id_continue(0x16EE, 0x16F0).	% ID_Continue Nl   [3] RUNIC ARLAUG SYMBOL..RUNIC BELGTHOR SYMBOL
unicode_id_continue(0x1700, 0x170C).	% ID_Continue Lo  [13] TAGALOG LETTER A..TAGALOG LETTER YA
unicode_id_continue(0x170E, 0x1711).	% ID_Continue Lo   [4] TAGALOG LETTER LA..TAGALOG LETTER HA
unicode_id_continue(0x1712, 0x1714).	% ID_Continue Mn   [3] TAGALOG VOWEL SIGN I..TAGALOG SIGN VIRAMA
unicode_id_continue(0x1720, 0x1731).	% ID_Continue Lo  [18] HANUNOO LETTER A..HANUNOO LETTER HA
unicode_id_continue(0x1732, 0x1734).	% ID_Continue Mn   [3] HANUNOO VOWEL SIGN I..HANUNOO SIGN PAMUDPOD
unicode_id_continue(0x1740, 0x1751).	% ID_Continue Lo  [18] BUHID LETTER A..BUHID LETTER HA
unicode_id_continue(0x1752, 0x1753).	% ID_Continue Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
unicode_id_continue(0x1760, 0x176C).	% ID_Continue Lo  [13] TAGBANWA LETTER A..TAGBANWA LETTER YA
unicode_id_continue(0x176E, 0x1770).	% ID_Continue Lo   [3] TAGBANWA LETTER LA..TAGBANWA LETTER SA
unicode_id_continue(0x1772, 0x1773).	% ID_Continue Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
unicode_id_continue(0x1780, 0x17B3).	% ID_Continue Lo  [52] KHMER LETTER KA..KHMER INDEPENDENT VOWEL QAU
unicode_id_continue(0x17B4, 0x17B5).	% ID_Continue Mn   [2] KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA
unicode_id_continue(0x17B6, 0x17B6).	% ID_Continue Mc       KHMER VOWEL SIGN AA
unicode_id_continue(0x17B7, 0x17BD).	% ID_Continue Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
unicode_id_continue(0x17BE, 0x17C5).	% ID_Continue Mc   [8] KHMER VOWEL SIGN OE..KHMER VOWEL SIGN AU
unicode_id_continue(0x17C6, 0x17C6).	% ID_Continue Mn       KHMER SIGN NIKAHIT
unicode_id_continue(0x17C7, 0x17C8).	% ID_Continue Mc   [2] KHMER SIGN REAHMUK..KHMER SIGN YUUKALEAPINTU
unicode_id_continue(0x17C9, 0x17D3).	% ID_Continue Mn  [11] KHMER SIGN MUUSIKATOAN..KHMER SIGN BATHAMASAT
unicode_id_continue(0x17D7, 0x17D7).	% ID_Continue Lm       KHMER SIGN LEK TOO
unicode_id_continue(0x17DC, 0x17DC).	% ID_Continue Lo       KHMER SIGN AVAKRAHASANYA
unicode_id_continue(0x17DD, 0x17DD).	% ID_Continue Mn       KHMER SIGN ATTHACAN
unicode_id_continue(0x17E0, 0x17E9).	% ID_Continue Nd  [10] KHMER DIGIT ZERO..KHMER DIGIT NINE
unicode_id_continue(0x180B, 0x180D).	% ID_Continue Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
unicode_id_continue(0x1810, 0x1819).	% ID_Continue Nd  [10] MONGOLIAN DIGIT ZERO..MONGOLIAN DIGIT NINE
unicode_id_continue(0x1820, 0x1842).	% ID_Continue Lo  [35] MONGOLIAN LETTER A..MONGOLIAN LETTER CHI
unicode_id_continue(0x1843, 0x1843).	% ID_Continue Lm       MONGOLIAN LETTER TODO LONG VOWEL SIGN
unicode_id_continue(0x1844, 0x1877).	% ID_Continue Lo  [52] MONGOLIAN LETTER TODO E..MONGOLIAN LETTER MANCHU ZHA
unicode_id_continue(0x1880, 0x18A8).	% ID_Continue Lo  [41] MONGOLIAN LETTER ALI GALI ANUSVARA ONE..MONGOLIAN LETTER MANCHU ALI GALI BHA
unicode_id_continue(0x18A9, 0x18A9).	% ID_Continue Mn       MONGOLIAN LETTER ALI GALI DAGALGA
unicode_id_continue(0x18AA, 0x18AA).	% ID_Continue Lo       MONGOLIAN LETTER MANCHU ALI GALI LHA
unicode_id_continue(0x18B0, 0x18F5).	% ID_Continue Lo  [70] CANADIAN SYLLABICS OY..CANADIAN SYLLABICS CARRIER DENTAL S
unicode_id_continue(0x1900, 0x191C).	% ID_Continue Lo  [29] LIMBU VOWEL-CARRIER LETTER..LIMBU LETTER HA
unicode_id_continue(0x1920, 0x1922).	% ID_Continue Mn   [3] LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN U
unicode_id_continue(0x1923, 0x1926).	% ID_Continue Mc   [4] LIMBU VOWEL SIGN EE..LIMBU VOWEL SIGN AU
unicode_id_continue(0x1927, 0x1928).	% ID_Continue Mn   [2] LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O
unicode_id_continue(0x1929, 0x192B).	% ID_Continue Mc   [3] LIMBU SUBJOINED LETTER YA..LIMBU SUBJOINED LETTER WA
unicode_id_continue(0x1930, 0x1931).	% ID_Continue Mc   [2] LIMBU SMALL LETTER KA..LIMBU SMALL LETTER NGA
unicode_id_continue(0x1932, 0x1932).	% ID_Continue Mn       LIMBU SMALL LETTER ANUSVARA
unicode_id_continue(0x1933, 0x1938).	% ID_Continue Mc   [6] LIMBU SMALL LETTER TA..LIMBU SMALL LETTER LA
unicode_id_continue(0x1939, 0x193B).	% ID_Continue Mn   [3] LIMBU SIGN MUKPHRENG..LIMBU SIGN SA-I
unicode_id_continue(0x1946, 0x194F).	% ID_Continue Nd  [10] LIMBU DIGIT ZERO..LIMBU DIGIT NINE
unicode_id_continue(0x1950, 0x196D).	% ID_Continue Lo  [30] TAI LE LETTER KA..TAI LE LETTER AI
unicode_id_continue(0x1970, 0x1974).	% ID_Continue Lo   [5] TAI LE LETTER TONE-2..TAI LE LETTER TONE-6
unicode_id_continue(0x1980, 0x19AB).	% ID_Continue Lo  [44] NEW TAI LUE LETTER HIGH QA..NEW TAI LUE LETTER LOW SUA
unicode_id_continue(0x19B0, 0x19C0).	% ID_Continue Mc  [17] NEW TAI LUE VOWEL SIGN VOWEL SHORTENER..NEW TAI LUE VOWEL SIGN IY
unicode_id_continue(0x19C1, 0x19C7).	% ID_Continue Lo   [7] NEW TAI LUE LETTER FINAL V..NEW TAI LUE LETTER FINAL B
unicode_id_continue(0x19C8, 0x19C9).	% ID_Continue Mc   [2] NEW TAI LUE TONE MARK-1..NEW TAI LUE TONE MARK-2
unicode_id_continue(0x19D0, 0x19D9).	% ID_Continue Nd  [10] NEW TAI LUE DIGIT ZERO..NEW TAI LUE DIGIT NINE
unicode_id_continue(0x19DA, 0x19DA).	% ID_Continue No       NEW TAI LUE THAM DIGIT ONE
unicode_id_continue(0x1A00, 0x1A16).	% ID_Continue Lo  [23] BUGINESE LETTER KA..BUGINESE LETTER HA
unicode_id_continue(0x1A17, 0x1A18).	% ID_Continue Mn   [2] BUGINESE VOWEL SIGN I..BUGINESE VOWEL SIGN U
unicode_id_continue(0x1A19, 0x1A1B).	% ID_Continue Mc   [3] BUGINESE VOWEL SIGN E..BUGINESE VOWEL SIGN AE
unicode_id_continue(0x1A20, 0x1A54).	% ID_Continue Lo  [53] TAI THAM LETTER HIGH KA..TAI THAM LETTER GREAT SA
unicode_id_continue(0x1A55, 0x1A55).	% ID_Continue Mc       TAI THAM CONSONANT SIGN MEDIAL RA
unicode_id_continue(0x1A56, 0x1A56).	% ID_Continue Mn       TAI THAM CONSONANT SIGN MEDIAL LA
unicode_id_continue(0x1A57, 0x1A57).	% ID_Continue Mc       TAI THAM CONSONANT SIGN LA TANG LAI
unicode_id_continue(0x1A58, 0x1A5E).	% ID_Continue Mn   [7] TAI THAM SIGN MAI KANG LAI..TAI THAM CONSONANT SIGN SA
unicode_id_continue(0x1A60, 0x1A60).	% ID_Continue Mn       TAI THAM SIGN SAKOT
unicode_id_continue(0x1A61, 0x1A61).	% ID_Continue Mc       TAI THAM VOWEL SIGN A
unicode_id_continue(0x1A62, 0x1A62).	% ID_Continue Mn       TAI THAM VOWEL SIGN MAI SAT
unicode_id_continue(0x1A63, 0x1A64).	% ID_Continue Mc   [2] TAI THAM VOWEL SIGN AA..TAI THAM VOWEL SIGN TALL AA
unicode_id_continue(0x1A65, 0x1A6C).	% ID_Continue Mn   [8] TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN OA BELOW
unicode_id_continue(0x1A6D, 0x1A72).	% ID_Continue Mc   [6] TAI THAM VOWEL SIGN OY..TAI THAM VOWEL SIGN THAM AI
unicode_id_continue(0x1A73, 0x1A7C).	% ID_Continue Mn  [10] TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN KHUEN-LUE KARAN
unicode_id_continue(0x1A7F, 0x1A7F).	% ID_Continue Mn       TAI THAM COMBINING CRYPTOGRAMMIC DOT
unicode_id_continue(0x1A80, 0x1A89).	% ID_Continue Nd  [10] TAI THAM HORA DIGIT ZERO..TAI THAM HORA DIGIT NINE
unicode_id_continue(0x1A90, 0x1A99).	% ID_Continue Nd  [10] TAI THAM THAM DIGIT ZERO..TAI THAM THAM DIGIT NINE
unicode_id_continue(0x1AA7, 0x1AA7).	% ID_Continue Lm       TAI THAM SIGN MAI YAMOK
unicode_id_continue(0x1B00, 0x1B03).	% ID_Continue Mn   [4] BALINESE SIGN ULU RICEM..BALINESE SIGN SURANG
unicode_id_continue(0x1B04, 0x1B04).	% ID_Continue Mc       BALINESE SIGN BISAH
unicode_id_continue(0x1B05, 0x1B33).	% ID_Continue Lo  [47] BALINESE LETTER AKARA..BALINESE LETTER HA
unicode_id_continue(0x1B34, 0x1B34).	% ID_Continue Mn       BALINESE SIGN REREKAN
unicode_id_continue(0x1B35, 0x1B35).	% ID_Continue Mc       BALINESE VOWEL SIGN TEDUNG
unicode_id_continue(0x1B36, 0x1B3A).	% ID_Continue Mn   [5] BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN RA REPA
unicode_id_continue(0x1B3B, 0x1B3B).	% ID_Continue Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_id_continue(0x1B3C, 0x1B3C).	% ID_Continue Mn       BALINESE VOWEL SIGN LA LENGA
unicode_id_continue(0x1B3D, 0x1B41).	% ID_Continue Mc   [5] BALINESE VOWEL SIGN LA LENGA TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG
unicode_id_continue(0x1B42, 0x1B42).	% ID_Continue Mn       BALINESE VOWEL SIGN PEPET
unicode_id_continue(0x1B43, 0x1B44).	% ID_Continue Mc   [2] BALINESE VOWEL SIGN PEPET TEDUNG..BALINESE ADEG ADEG
unicode_id_continue(0x1B45, 0x1B4B).	% ID_Continue Lo   [7] BALINESE LETTER KAF SASAK..BALINESE LETTER ASYURA SASAK
unicode_id_continue(0x1B50, 0x1B59).	% ID_Continue Nd  [10] BALINESE DIGIT ZERO..BALINESE DIGIT NINE
unicode_id_continue(0x1B6B, 0x1B73).	% ID_Continue Mn   [9] BALINESE MUSICAL SYMBOL COMBINING TEGEH..BALINESE MUSICAL SYMBOL COMBINING GONG
unicode_id_continue(0x1B80, 0x1B81).	% ID_Continue Mn   [2] SUNDANESE SIGN PANYECEK..SUNDANESE SIGN PANGLAYAR
unicode_id_continue(0x1B82, 0x1B82).	% ID_Continue Mc       SUNDANESE SIGN PANGWISAD
unicode_id_continue(0x1B83, 0x1BA0).	% ID_Continue Lo  [30] SUNDANESE LETTER A..SUNDANESE LETTER HA
unicode_id_continue(0x1BA1, 0x1BA1).	% ID_Continue Mc       SUNDANESE CONSONANT SIGN PAMINGKAL
unicode_id_continue(0x1BA2, 0x1BA5).	% ID_Continue Mn   [4] SUNDANESE CONSONANT SIGN PANYAKRA..SUNDANESE VOWEL SIGN PANYUKU
unicode_id_continue(0x1BA6, 0x1BA7).	% ID_Continue Mc   [2] SUNDANESE VOWEL SIGN PANAELAENG..SUNDANESE VOWEL SIGN PANOLONG
unicode_id_continue(0x1BA8, 0x1BA9).	% ID_Continue Mn   [2] SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG
unicode_id_continue(0x1BAA, 0x1BAA).	% ID_Continue Mc       SUNDANESE SIGN PAMAAEH
unicode_id_continue(0x1BAB, 0x1BAB).	% ID_Continue Mn       SUNDANESE SIGN VIRAMA
unicode_id_continue(0x1BAC, 0x1BAD).	% ID_Continue Mc   [2] SUNDANESE CONSONANT SIGN PASANGAN MA..SUNDANESE CONSONANT SIGN PASANGAN WA
unicode_id_continue(0x1BAE, 0x1BAF).	% ID_Continue Lo   [2] SUNDANESE LETTER KHA..SUNDANESE LETTER SYA
unicode_id_continue(0x1BB0, 0x1BB9).	% ID_Continue Nd  [10] SUNDANESE DIGIT ZERO..SUNDANESE DIGIT NINE
unicode_id_continue(0x1BBA, 0x1BE5).	% ID_Continue Lo  [44] SUNDANESE AVAGRAHA..BATAK LETTER U
unicode_id_continue(0x1BE6, 0x1BE6).	% ID_Continue Mn       BATAK SIGN TOMPI
unicode_id_continue(0x1BE7, 0x1BE7).	% ID_Continue Mc       BATAK VOWEL SIGN E
unicode_id_continue(0x1BE8, 0x1BE9).	% ID_Continue Mn   [2] BATAK VOWEL SIGN PAKPAK E..BATAK VOWEL SIGN EE
unicode_id_continue(0x1BEA, 0x1BEC).	% ID_Continue Mc   [3] BATAK VOWEL SIGN I..BATAK VOWEL SIGN O
unicode_id_continue(0x1BED, 0x1BED).	% ID_Continue Mn       BATAK VOWEL SIGN KARO O
unicode_id_continue(0x1BEE, 0x1BEE).	% ID_Continue Mc       BATAK VOWEL SIGN U
unicode_id_continue(0x1BEF, 0x1BF1).	% ID_Continue Mn   [3] BATAK VOWEL SIGN U FOR SIMALUNGUN SA..BATAK CONSONANT SIGN H
unicode_id_continue(0x1BF2, 0x1BF3).	% ID_Continue Mc   [2] BATAK PANGOLAT..BATAK PANONGONAN
unicode_id_continue(0x1C00, 0x1C23).	% ID_Continue Lo  [36] LEPCHA LETTER KA..LEPCHA LETTER A
unicode_id_continue(0x1C24, 0x1C2B).	% ID_Continue Mc   [8] LEPCHA SUBJOINED LETTER YA..LEPCHA VOWEL SIGN UU
unicode_id_continue(0x1C2C, 0x1C33).	% ID_Continue Mn   [8] LEPCHA VOWEL SIGN E..LEPCHA CONSONANT SIGN T
unicode_id_continue(0x1C34, 0x1C35).	% ID_Continue Mc   [2] LEPCHA CONSONANT SIGN NYIN-DO..LEPCHA CONSONANT SIGN KANG
unicode_id_continue(0x1C36, 0x1C37).	% ID_Continue Mn   [2] LEPCHA SIGN RAN..LEPCHA SIGN NUKTA
unicode_id_continue(0x1C40, 0x1C49).	% ID_Continue Nd  [10] LEPCHA DIGIT ZERO..LEPCHA DIGIT NINE
unicode_id_continue(0x1C4D, 0x1C4F).	% ID_Continue Lo   [3] LEPCHA LETTER TTA..LEPCHA LETTER DDA
unicode_id_continue(0x1C50, 0x1C59).	% ID_Continue Nd  [10] OL CHIKI DIGIT ZERO..OL CHIKI DIGIT NINE
unicode_id_continue(0x1C5A, 0x1C77).	% ID_Continue Lo  [30] OL CHIKI LETTER LA..OL CHIKI LETTER OH
unicode_id_continue(0x1C78, 0x1C7D).	% ID_Continue Lm   [6] OL CHIKI MU TTUDDAG..OL CHIKI AHAD
unicode_id_continue(0x1CD0, 0x1CD2).	% ID_Continue Mn   [3] VEDIC TONE KARSHANA..VEDIC TONE PRENKHA
unicode_id_continue(0x1CD4, 0x1CE0).	% ID_Continue Mn  [13] VEDIC SIGN YAJURVEDIC MIDLINE SVARITA..VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA
unicode_id_continue(0x1CE1, 0x1CE1).	% ID_Continue Mc       VEDIC TONE ATHARVAVEDIC INDEPENDENT SVARITA
unicode_id_continue(0x1CE2, 0x1CE8).	% ID_Continue Mn   [7] VEDIC SIGN VISARGA SVARITA..VEDIC SIGN VISARGA ANUDATTA WITH TAIL
unicode_id_continue(0x1CE9, 0x1CEC).	% ID_Continue Lo   [4] VEDIC SIGN ANUSVARA ANTARGOMUKHA..VEDIC SIGN ANUSVARA VAMAGOMUKHA WITH TAIL
unicode_id_continue(0x1CED, 0x1CED).	% ID_Continue Mn       VEDIC SIGN TIRYAK
unicode_id_continue(0x1CEE, 0x1CF1).	% ID_Continue Lo   [4] VEDIC SIGN HEXIFORM LONG ANUSVARA..VEDIC SIGN ANUSVARA UBHAYATO MUKHA
unicode_id_continue(0x1CF2, 0x1CF3).	% ID_Continue Mc   [2] VEDIC SIGN ARDHAVISARGA..VEDIC SIGN ROTATED ARDHAVISARGA
unicode_id_continue(0x1CF4, 0x1CF4).	% ID_Continue Mn       VEDIC TONE CANDRA ABOVE
unicode_id_continue(0x1CF5, 0x1CF6).	% ID_Continue Lo   [2] VEDIC SIGN JIHVAMULIYA..VEDIC SIGN UPADHMANIYA
unicode_id_continue(0x1D00, 0x1D2B).	% ID_Continue L&  [44] LATIN LETTER SMALL CAPITAL A..CYRILLIC LETTER SMALL CAPITAL EL
unicode_id_continue(0x1D2C, 0x1D6A).	% ID_Continue Lm  [63] MODIFIER LETTER CAPITAL A..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_id_continue(0x1D6B, 0x1D77).	% ID_Continue L&  [13] LATIN SMALL LETTER UE..LATIN SMALL LETTER TURNED G
unicode_id_continue(0x1D78, 0x1D78).	% ID_Continue Lm       MODIFIER LETTER CYRILLIC EN
unicode_id_continue(0x1D79, 0x1D9A).	% ID_Continue L&  [34] LATIN SMALL LETTER INSULAR G..LATIN SMALL LETTER EZH WITH RETROFLEX HOOK
unicode_id_continue(0x1D9B, 0x1DBF).	% ID_Continue Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_id_continue(0x1DC0, 0x1DE6).	% ID_Continue Mn  [39] COMBINING DOTTED GRAVE ACCENT..COMBINING LATIN SMALL LETTER Z
unicode_id_continue(0x1DFC, 0x1DFF).	% ID_Continue Mn   [4] COMBINING DOUBLE INVERTED BREVE BELOW..COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW
unicode_id_continue(0x1E00, 0x1F15).	% ID_Continue L& [278] LATIN CAPITAL LETTER A WITH RING BELOW..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_id_continue(0x1F18, 0x1F1D).	% ID_Continue L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_id_continue(0x1F20, 0x1F45).	% ID_Continue L&  [38] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_id_continue(0x1F48, 0x1F4D).	% ID_Continue L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_id_continue(0x1F50, 0x1F57).	% ID_Continue L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_id_continue(0x1F59, 0x1F59).	% ID_Continue L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_id_continue(0x1F5B, 0x1F5B).	% ID_Continue L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_id_continue(0x1F5D, 0x1F5D).	% ID_Continue L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_id_continue(0x1F5F, 0x1F7D).	% ID_Continue L&  [31] GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_id_continue(0x1F80, 0x1FB4).	% ID_Continue L&  [53] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_id_continue(0x1FB6, 0x1FBC).	% ID_Continue L&   [7] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_id_continue(0x1FBE, 0x1FBE).	% ID_Continue L&       GREEK PROSGEGRAMMENI
unicode_id_continue(0x1FC2, 0x1FC4).	% ID_Continue L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_id_continue(0x1FC6, 0x1FCC).	% ID_Continue L&   [7] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_id_continue(0x1FD0, 0x1FD3).	% ID_Continue L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_id_continue(0x1FD6, 0x1FDB).	% ID_Continue L&   [6] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_id_continue(0x1FE0, 0x1FEC).	% ID_Continue L&  [13] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_id_continue(0x1FF2, 0x1FF4).	% ID_Continue L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_id_continue(0x1FF6, 0x1FFC).	% ID_Continue L&   [7] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_id_continue(0x203F, 0x2040).	% ID_Continue Pc   [2] UNDERTIE..CHARACTER TIE
unicode_id_continue(0x2054, 0x2054).	% ID_Continue Pc       INVERTED UNDERTIE
unicode_id_continue(0x2071, 0x2071).	% ID_Continue Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_id_continue(0x207F, 0x207F).	% ID_Continue Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_id_continue(0x2090, 0x209C).	% ID_Continue Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_id_continue(0x20D0, 0x20DC).	% ID_Continue Mn  [13] COMBINING LEFT HARPOON ABOVE..COMBINING FOUR DOTS ABOVE
unicode_id_continue(0x20E1, 0x20E1).	% ID_Continue Mn       COMBINING LEFT RIGHT ARROW ABOVE
unicode_id_continue(0x20E5, 0x20F0).	% ID_Continue Mn  [12] COMBINING REVERSE SOLIDUS OVERLAY..COMBINING ASTERISK ABOVE
unicode_id_continue(0x2102, 0x2102).	% ID_Continue L&       DOUBLE-STRUCK CAPITAL C
unicode_id_continue(0x2107, 0x2107).	% ID_Continue L&       EULER CONSTANT
unicode_id_continue(0x210A, 0x2113).	% ID_Continue L&  [10] SCRIPT SMALL G..SCRIPT SMALL L
unicode_id_continue(0x2115, 0x2115).	% ID_Continue L&       DOUBLE-STRUCK CAPITAL N
unicode_id_continue(0x2118, 0x2118).	% ID_Continue Sm       SCRIPT CAPITAL P
unicode_id_continue(0x2119, 0x211D).	% ID_Continue L&   [5] DOUBLE-STRUCK CAPITAL P..DOUBLE-STRUCK CAPITAL R
unicode_id_continue(0x2124, 0x2124).	% ID_Continue L&       DOUBLE-STRUCK CAPITAL Z
unicode_id_continue(0x2126, 0x2126).	% ID_Continue L&       OHM SIGN
unicode_id_continue(0x2128, 0x2128).	% ID_Continue L&       BLACK-LETTER CAPITAL Z
unicode_id_continue(0x212A, 0x212D).	% ID_Continue L&   [4] KELVIN SIGN..BLACK-LETTER CAPITAL C
unicode_id_continue(0x212E, 0x212E).	% ID_Continue So       ESTIMATED SYMBOL
unicode_id_continue(0x212F, 0x2134).	% ID_Continue L&   [6] SCRIPT SMALL E..SCRIPT SMALL O
unicode_id_continue(0x2135, 0x2138).	% ID_Continue Lo   [4] ALEF SYMBOL..DALET SYMBOL
unicode_id_continue(0x2139, 0x2139).	% ID_Continue L&       INFORMATION SOURCE
unicode_id_continue(0x213C, 0x213F).	% ID_Continue L&   [4] DOUBLE-STRUCK SMALL PI..DOUBLE-STRUCK CAPITAL PI
unicode_id_continue(0x2145, 0x2149).	% ID_Continue L&   [5] DOUBLE-STRUCK ITALIC CAPITAL D..DOUBLE-STRUCK ITALIC SMALL J
unicode_id_continue(0x214E, 0x214E).	% ID_Continue L&       TURNED SMALL F
unicode_id_continue(0x2160, 0x2182).	% ID_Continue Nl  [35] ROMAN NUMERAL ONE..ROMAN NUMERAL TEN THOUSAND
unicode_id_continue(0x2183, 0x2184).	% ID_Continue L&   [2] ROMAN NUMERAL REVERSED ONE HUNDRED..LATIN SMALL LETTER REVERSED C
unicode_id_continue(0x2185, 0x2188).	% ID_Continue Nl   [4] ROMAN NUMERAL SIX LATE FORM..ROMAN NUMERAL ONE HUNDRED THOUSAND
unicode_id_continue(0x2C00, 0x2C2E).	% ID_Continue L&  [47] GLAGOLITIC CAPITAL LETTER AZU..GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_id_continue(0x2C30, 0x2C5E).	% ID_Continue L&  [47] GLAGOLITIC SMALL LETTER AZU..GLAGOLITIC SMALL LETTER LATINATE MYSLITE
unicode_id_continue(0x2C60, 0x2C7B).	% ID_Continue L&  [28] LATIN CAPITAL LETTER L WITH DOUBLE BAR..LATIN LETTER SMALL CAPITAL TURNED E
unicode_id_continue(0x2C7C, 0x2C7D).	% ID_Continue Lm   [2] LATIN SUBSCRIPT SMALL LETTER J..MODIFIER LETTER CAPITAL V
unicode_id_continue(0x2C7E, 0x2CE4).	% ID_Continue L& [103] LATIN CAPITAL LETTER S WITH SWASH TAIL..COPTIC SYMBOL KAI
unicode_id_continue(0x2CEB, 0x2CEE).	% ID_Continue L&   [4] COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI..COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
unicode_id_continue(0x2CEF, 0x2CF1).	% ID_Continue Mn   [3] COPTIC COMBINING NI ABOVE..COPTIC COMBINING SPIRITUS LENIS
unicode_id_continue(0x2CF2, 0x2CF3).	% ID_Continue L&   [2] COPTIC CAPITAL LETTER BOHAIRIC KHEI..COPTIC SMALL LETTER BOHAIRIC KHEI
unicode_id_continue(0x2D00, 0x2D25).	% ID_Continue L&  [38] GEORGIAN SMALL LETTER AN..GEORGIAN SMALL LETTER HOE
unicode_id_continue(0x2D27, 0x2D27).	% ID_Continue L&       GEORGIAN SMALL LETTER YN
unicode_id_continue(0x2D2D, 0x2D2D).	% ID_Continue L&       GEORGIAN SMALL LETTER AEN
unicode_id_continue(0x2D30, 0x2D67).	% ID_Continue Lo  [56] TIFINAGH LETTER YA..TIFINAGH LETTER YO
unicode_id_continue(0x2D6F, 0x2D6F).	% ID_Continue Lm       TIFINAGH MODIFIER LETTER LABIALIZATION MARK
unicode_id_continue(0x2D7F, 0x2D7F).	% ID_Continue Mn       TIFINAGH CONSONANT JOINER
unicode_id_continue(0x2D80, 0x2D96).	% ID_Continue Lo  [23] ETHIOPIC SYLLABLE LOA..ETHIOPIC SYLLABLE GGWE
unicode_id_continue(0x2DA0, 0x2DA6).	% ID_Continue Lo   [7] ETHIOPIC SYLLABLE SSA..ETHIOPIC SYLLABLE SSO
unicode_id_continue(0x2DA8, 0x2DAE).	% ID_Continue Lo   [7] ETHIOPIC SYLLABLE CCA..ETHIOPIC SYLLABLE CCO
unicode_id_continue(0x2DB0, 0x2DB6).	% ID_Continue Lo   [7] ETHIOPIC SYLLABLE ZZA..ETHIOPIC SYLLABLE ZZO
unicode_id_continue(0x2DB8, 0x2DBE).	% ID_Continue Lo   [7] ETHIOPIC SYLLABLE CCHA..ETHIOPIC SYLLABLE CCHO
unicode_id_continue(0x2DC0, 0x2DC6).	% ID_Continue Lo   [7] ETHIOPIC SYLLABLE QYA..ETHIOPIC SYLLABLE QYO
unicode_id_continue(0x2DC8, 0x2DCE).	% ID_Continue Lo   [7] ETHIOPIC SYLLABLE KYA..ETHIOPIC SYLLABLE KYO
unicode_id_continue(0x2DD0, 0x2DD6).	% ID_Continue Lo   [7] ETHIOPIC SYLLABLE XYA..ETHIOPIC SYLLABLE XYO
unicode_id_continue(0x2DD8, 0x2DDE).	% ID_Continue Lo   [7] ETHIOPIC SYLLABLE GYA..ETHIOPIC SYLLABLE GYO
unicode_id_continue(0x2DE0, 0x2DFF).	% ID_Continue Mn  [32] COMBINING CYRILLIC LETTER BE..COMBINING CYRILLIC LETTER IOTIFIED BIG YUS
unicode_id_continue(0x3005, 0x3005).	% ID_Continue Lm       IDEOGRAPHIC ITERATION MARK
unicode_id_continue(0x3006, 0x3006).	% ID_Continue Lo       IDEOGRAPHIC CLOSING MARK
unicode_id_continue(0x3007, 0x3007).	% ID_Continue Nl       IDEOGRAPHIC NUMBER ZERO
unicode_id_continue(0x3021, 0x3029).	% ID_Continue Nl   [9] HANGZHOU NUMERAL ONE..HANGZHOU NUMERAL NINE
unicode_id_continue(0x302A, 0x302D).	% ID_Continue Mn   [4] IDEOGRAPHIC LEVEL TONE MARK..IDEOGRAPHIC ENTERING TONE MARK
unicode_id_continue(0x302E, 0x302F).	% ID_Continue Mc   [2] HANGUL SINGLE DOT TONE MARK..HANGUL DOUBLE DOT TONE MARK
unicode_id_continue(0x3031, 0x3035).	% ID_Continue Lm   [5] VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF
unicode_id_continue(0x3038, 0x303A).	% ID_Continue Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
unicode_id_continue(0x303B, 0x303B).	% ID_Continue Lm       VERTICAL IDEOGRAPHIC ITERATION MARK
unicode_id_continue(0x303C, 0x303C).	% ID_Continue Lo       MASU MARK
unicode_id_continue(0x3041, 0x3096).	% ID_Continue Lo  [86] HIRAGANA LETTER SMALL A..HIRAGANA LETTER SMALL KE
unicode_id_continue(0x3099, 0x309A).	% ID_Continue Mn   [2] COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK..COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_id_continue(0x309B, 0x309C).	% ID_Continue Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_id_continue(0x309D, 0x309E).	% ID_Continue Lm   [2] HIRAGANA ITERATION MARK..HIRAGANA VOICED ITERATION MARK
unicode_id_continue(0x309F, 0x309F).	% ID_Continue Lo       HIRAGANA DIGRAPH YORI
unicode_id_continue(0x30A1, 0x30FA).	% ID_Continue Lo  [90] KATAKANA LETTER SMALL A..KATAKANA LETTER VO
unicode_id_continue(0x30FC, 0x30FE).	% ID_Continue Lm   [3] KATAKANA-HIRAGANA PROLONGED SOUND MARK..KATAKANA VOICED ITERATION MARK
unicode_id_continue(0x30FF, 0x30FF).	% ID_Continue Lo       KATAKANA DIGRAPH KOTO
unicode_id_continue(0x3105, 0x312D).	% ID_Continue Lo  [41] BOPOMOFO LETTER B..BOPOMOFO LETTER IH
unicode_id_continue(0x3131, 0x318E).	% ID_Continue Lo  [94] HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE
unicode_id_continue(0x31A0, 0x31BA).	% ID_Continue Lo  [27] BOPOMOFO LETTER BU..BOPOMOFO LETTER ZY
unicode_id_continue(0x31F0, 0x31FF).	% ID_Continue Lo  [16] KATAKANA LETTER SMALL KU..KATAKANA LETTER SMALL RO
unicode_id_continue(0x3400, 0x4DB5).	% ID_Continue Lo [6582] CJK UNIFIED IDEOGRAPH-3400..CJK UNIFIED IDEOGRAPH-4DB5
unicode_id_continue(0x4E00, 0x9FCC).	% ID_Continue Lo [20941] CJK UNIFIED IDEOGRAPH-4E00..CJK UNIFIED IDEOGRAPH-9FCC
unicode_id_continue(0xA000, 0xA014).	% ID_Continue Lo  [21] YI SYLLABLE IT..YI SYLLABLE E
unicode_id_continue(0xA015, 0xA015).	% ID_Continue Lm       YI SYLLABLE WU
unicode_id_continue(0xA016, 0xA48C).	% ID_Continue Lo [1143] YI SYLLABLE BIT..YI SYLLABLE YYR
unicode_id_continue(0xA4D0, 0xA4F7).	% ID_Continue Lo  [40] LISU LETTER BA..LISU LETTER OE
unicode_id_continue(0xA4F8, 0xA4FD).	% ID_Continue Lm   [6] LISU LETTER TONE MYA TI..LISU LETTER TONE MYA JEU
unicode_id_continue(0xA500, 0xA60B).	% ID_Continue Lo [268] VAI SYLLABLE EE..VAI SYLLABLE NG
unicode_id_continue(0xA60C, 0xA60C).	% ID_Continue Lm       VAI SYLLABLE LENGTHENER
unicode_id_continue(0xA610, 0xA61F).	% ID_Continue Lo  [16] VAI SYLLABLE NDOLE FA..VAI SYMBOL JONG
unicode_id_continue(0xA620, 0xA629).	% ID_Continue Nd  [10] VAI DIGIT ZERO..VAI DIGIT NINE
unicode_id_continue(0xA62A, 0xA62B).	% ID_Continue Lo   [2] VAI SYLLABLE NDOLE MA..VAI SYLLABLE NDOLE DO
unicode_id_continue(0xA640, 0xA66D).	% ID_Continue L&  [46] CYRILLIC CAPITAL LETTER ZEMLYA..CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
unicode_id_continue(0xA66E, 0xA66E).	% ID_Continue Lo       CYRILLIC LETTER MULTIOCULAR O
unicode_id_continue(0xA66F, 0xA66F).	% ID_Continue Mn       COMBINING CYRILLIC VZMET
unicode_id_continue(0xA674, 0xA67D).	% ID_Continue Mn  [10] COMBINING CYRILLIC LETTER UKRAINIAN IE..COMBINING CYRILLIC PAYEROK
unicode_id_continue(0xA67F, 0xA67F).	% ID_Continue Lm       CYRILLIC PAYEROK
unicode_id_continue(0xA680, 0xA697).	% ID_Continue L&  [24] CYRILLIC CAPITAL LETTER DWE..CYRILLIC SMALL LETTER SHWE
unicode_id_continue(0xA69F, 0xA69F).	% ID_Continue Mn       COMBINING CYRILLIC LETTER IOTIFIED E
unicode_id_continue(0xA6A0, 0xA6E5).	% ID_Continue Lo  [70] BAMUM LETTER A..BAMUM LETTER KI
unicode_id_continue(0xA6E6, 0xA6EF).	% ID_Continue Nl  [10] BAMUM LETTER MO..BAMUM LETTER KOGHOM
unicode_id_continue(0xA6F0, 0xA6F1).	% ID_Continue Mn   [2] BAMUM COMBINING MARK KOQNDON..BAMUM COMBINING MARK TUKWENTIS
unicode_id_continue(0xA717, 0xA71F).	% ID_Continue Lm   [9] MODIFIER LETTER DOT VERTICAL BAR..MODIFIER LETTER LOW INVERTED EXCLAMATION MARK
unicode_id_continue(0xA722, 0xA76F).	% ID_Continue L&  [78] LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF..LATIN SMALL LETTER CON
unicode_id_continue(0xA770, 0xA770).	% ID_Continue Lm       MODIFIER LETTER US
unicode_id_continue(0xA771, 0xA787).	% ID_Continue L&  [23] LATIN SMALL LETTER DUM..LATIN SMALL LETTER INSULAR T
unicode_id_continue(0xA788, 0xA788).	% ID_Continue Lm       MODIFIER LETTER LOW CIRCUMFLEX ACCENT
unicode_id_continue(0xA78B, 0xA78E).	% ID_Continue L&   [4] LATIN CAPITAL LETTER SALTILLO..LATIN SMALL LETTER L WITH RETROFLEX HOOK AND BELT
unicode_id_continue(0xA790, 0xA793).	% ID_Continue L&   [4] LATIN CAPITAL LETTER N WITH DESCENDER..LATIN SMALL LETTER C WITH BAR
unicode_id_continue(0xA7A0, 0xA7AA).	% ID_Continue L&  [11] LATIN CAPITAL LETTER G WITH OBLIQUE STROKE..LATIN CAPITAL LETTER H WITH HOOK
unicode_id_continue(0xA7F8, 0xA7F9).	% ID_Continue Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_id_continue(0xA7FA, 0xA7FA).	% ID_Continue L&       LATIN LETTER SMALL CAPITAL TURNED M
unicode_id_continue(0xA7FB, 0xA801).	% ID_Continue Lo   [7] LATIN EPIGRAPHIC LETTER REVERSED F..SYLOTI NAGRI LETTER I
unicode_id_continue(0xA802, 0xA802).	% ID_Continue Mn       SYLOTI NAGRI SIGN DVISVARA
unicode_id_continue(0xA803, 0xA805).	% ID_Continue Lo   [3] SYLOTI NAGRI LETTER U..SYLOTI NAGRI LETTER O
unicode_id_continue(0xA806, 0xA806).	% ID_Continue Mn       SYLOTI NAGRI SIGN HASANTA
unicode_id_continue(0xA807, 0xA80A).	% ID_Continue Lo   [4] SYLOTI NAGRI LETTER KO..SYLOTI NAGRI LETTER GHO
unicode_id_continue(0xA80B, 0xA80B).	% ID_Continue Mn       SYLOTI NAGRI SIGN ANUSVARA
unicode_id_continue(0xA80C, 0xA822).	% ID_Continue Lo  [23] SYLOTI NAGRI LETTER CO..SYLOTI NAGRI LETTER HO
unicode_id_continue(0xA823, 0xA824).	% ID_Continue Mc   [2] SYLOTI NAGRI VOWEL SIGN A..SYLOTI NAGRI VOWEL SIGN I
unicode_id_continue(0xA825, 0xA826).	% ID_Continue Mn   [2] SYLOTI NAGRI VOWEL SIGN U..SYLOTI NAGRI VOWEL SIGN E
unicode_id_continue(0xA827, 0xA827).	% ID_Continue Mc       SYLOTI NAGRI VOWEL SIGN OO
unicode_id_continue(0xA840, 0xA873).	% ID_Continue Lo  [52] PHAGS-PA LETTER KA..PHAGS-PA LETTER CANDRABINDU
unicode_id_continue(0xA880, 0xA881).	% ID_Continue Mc   [2] SAURASHTRA SIGN ANUSVARA..SAURASHTRA SIGN VISARGA
unicode_id_continue(0xA882, 0xA8B3).	% ID_Continue Lo  [50] SAURASHTRA LETTER A..SAURASHTRA LETTER LLA
unicode_id_continue(0xA8B4, 0xA8C3).	% ID_Continue Mc  [16] SAURASHTRA CONSONANT SIGN HAARU..SAURASHTRA VOWEL SIGN AU
unicode_id_continue(0xA8C4, 0xA8C4).	% ID_Continue Mn       SAURASHTRA SIGN VIRAMA
unicode_id_continue(0xA8D0, 0xA8D9).	% ID_Continue Nd  [10] SAURASHTRA DIGIT ZERO..SAURASHTRA DIGIT NINE
unicode_id_continue(0xA8E0, 0xA8F1).	% ID_Continue Mn  [18] COMBINING DEVANAGARI DIGIT ZERO..COMBINING DEVANAGARI SIGN AVAGRAHA
unicode_id_continue(0xA8F2, 0xA8F7).	% ID_Continue Lo   [6] DEVANAGARI SIGN SPACING CANDRABINDU..DEVANAGARI SIGN CANDRABINDU AVAGRAHA
unicode_id_continue(0xA8FB, 0xA8FB).	% ID_Continue Lo       DEVANAGARI HEADSTROKE
unicode_id_continue(0xA900, 0xA909).	% ID_Continue Nd  [10] KAYAH LI DIGIT ZERO..KAYAH LI DIGIT NINE
unicode_id_continue(0xA90A, 0xA925).	% ID_Continue Lo  [28] KAYAH LI LETTER KA..KAYAH LI LETTER OO
unicode_id_continue(0xA926, 0xA92D).	% ID_Continue Mn   [8] KAYAH LI VOWEL UE..KAYAH LI TONE CALYA PLOPHU
unicode_id_continue(0xA930, 0xA946).	% ID_Continue Lo  [23] REJANG LETTER KA..REJANG LETTER A
unicode_id_continue(0xA947, 0xA951).	% ID_Continue Mn  [11] REJANG VOWEL SIGN I..REJANG CONSONANT SIGN R
unicode_id_continue(0xA952, 0xA953).	% ID_Continue Mc   [2] REJANG CONSONANT SIGN H..REJANG VIRAMA
unicode_id_continue(0xA960, 0xA97C).	% ID_Continue Lo  [29] HANGUL CHOSEONG TIKEUT-MIEUM..HANGUL CHOSEONG SSANGYEORINHIEUH
unicode_id_continue(0xA980, 0xA982).	% ID_Continue Mn   [3] JAVANESE SIGN PANYANGGA..JAVANESE SIGN LAYAR
unicode_id_continue(0xA983, 0xA983).	% ID_Continue Mc       JAVANESE SIGN WIGNYAN
unicode_id_continue(0xA984, 0xA9B2).	% ID_Continue Lo  [47] JAVANESE LETTER A..JAVANESE LETTER HA
unicode_id_continue(0xA9B3, 0xA9B3).	% ID_Continue Mn       JAVANESE SIGN CECAK TELU
unicode_id_continue(0xA9B4, 0xA9B5).	% ID_Continue Mc   [2] JAVANESE VOWEL SIGN TARUNG..JAVANESE VOWEL SIGN TOLONG
unicode_id_continue(0xA9B6, 0xA9B9).	% ID_Continue Mn   [4] JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN SUKU MENDUT
unicode_id_continue(0xA9BA, 0xA9BB).	% ID_Continue Mc   [2] JAVANESE VOWEL SIGN TALING..JAVANESE VOWEL SIGN DIRGA MURE
unicode_id_continue(0xA9BC, 0xA9BC).	% ID_Continue Mn       JAVANESE VOWEL SIGN PEPET
unicode_id_continue(0xA9BD, 0xA9C0).	% ID_Continue Mc   [4] JAVANESE CONSONANT SIGN KERET..JAVANESE PANGKON
unicode_id_continue(0xA9CF, 0xA9CF).	% ID_Continue Lm       JAVANESE PANGRANGKEP
unicode_id_continue(0xA9D0, 0xA9D9).	% ID_Continue Nd  [10] JAVANESE DIGIT ZERO..JAVANESE DIGIT NINE
unicode_id_continue(0xAA00, 0xAA28).	% ID_Continue Lo  [41] CHAM LETTER A..CHAM LETTER HA
unicode_id_continue(0xAA29, 0xAA2E).	% ID_Continue Mn   [6] CHAM VOWEL SIGN AA..CHAM VOWEL SIGN OE
unicode_id_continue(0xAA2F, 0xAA30).	% ID_Continue Mc   [2] CHAM VOWEL SIGN O..CHAM VOWEL SIGN AI
unicode_id_continue(0xAA31, 0xAA32).	% ID_Continue Mn   [2] CHAM VOWEL SIGN AU..CHAM VOWEL SIGN UE
unicode_id_continue(0xAA33, 0xAA34).	% ID_Continue Mc   [2] CHAM CONSONANT SIGN YA..CHAM CONSONANT SIGN RA
unicode_id_continue(0xAA35, 0xAA36).	% ID_Continue Mn   [2] CHAM CONSONANT SIGN LA..CHAM CONSONANT SIGN WA
unicode_id_continue(0xAA40, 0xAA42).	% ID_Continue Lo   [3] CHAM LETTER FINAL K..CHAM LETTER FINAL NG
unicode_id_continue(0xAA43, 0xAA43).	% ID_Continue Mn       CHAM CONSONANT SIGN FINAL NG
unicode_id_continue(0xAA44, 0xAA4B).	% ID_Continue Lo   [8] CHAM LETTER FINAL CH..CHAM LETTER FINAL SS
unicode_id_continue(0xAA4C, 0xAA4C).	% ID_Continue Mn       CHAM CONSONANT SIGN FINAL M
unicode_id_continue(0xAA4D, 0xAA4D).	% ID_Continue Mc       CHAM CONSONANT SIGN FINAL H
unicode_id_continue(0xAA50, 0xAA59).	% ID_Continue Nd  [10] CHAM DIGIT ZERO..CHAM DIGIT NINE
unicode_id_continue(0xAA60, 0xAA6F).	% ID_Continue Lo  [16] MYANMAR LETTER KHAMTI GA..MYANMAR LETTER KHAMTI FA
unicode_id_continue(0xAA70, 0xAA70).	% ID_Continue Lm       MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION
unicode_id_continue(0xAA71, 0xAA76).	% ID_Continue Lo   [6] MYANMAR LETTER KHAMTI XA..MYANMAR LOGOGRAM KHAMTI HM
unicode_id_continue(0xAA7A, 0xAA7A).	% ID_Continue Lo       MYANMAR LETTER AITON RA
unicode_id_continue(0xAA7B, 0xAA7B).	% ID_Continue Mc       MYANMAR SIGN PAO KAREN TONE
unicode_id_continue(0xAA80, 0xAAAF).	% ID_Continue Lo  [48] TAI VIET LETTER LOW KO..TAI VIET LETTER HIGH O
unicode_id_continue(0xAAB0, 0xAAB0).	% ID_Continue Mn       TAI VIET MAI KANG
unicode_id_continue(0xAAB1, 0xAAB1).	% ID_Continue Lo       TAI VIET VOWEL AA
unicode_id_continue(0xAAB2, 0xAAB4).	% ID_Continue Mn   [3] TAI VIET VOWEL I..TAI VIET VOWEL U
unicode_id_continue(0xAAB5, 0xAAB6).	% ID_Continue Lo   [2] TAI VIET VOWEL E..TAI VIET VOWEL O
unicode_id_continue(0xAAB7, 0xAAB8).	% ID_Continue Mn   [2] TAI VIET MAI KHIT..TAI VIET VOWEL IA
unicode_id_continue(0xAAB9, 0xAABD).	% ID_Continue Lo   [5] TAI VIET VOWEL UEA..TAI VIET VOWEL AN
unicode_id_continue(0xAABE, 0xAABF).	% ID_Continue Mn   [2] TAI VIET VOWEL AM..TAI VIET TONE MAI EK
unicode_id_continue(0xAAC0, 0xAAC0).	% ID_Continue Lo       TAI VIET TONE MAI NUENG
unicode_id_continue(0xAAC1, 0xAAC1).	% ID_Continue Mn       TAI VIET TONE MAI THO
unicode_id_continue(0xAAC2, 0xAAC2).	% ID_Continue Lo       TAI VIET TONE MAI SONG
unicode_id_continue(0xAADB, 0xAADC).	% ID_Continue Lo   [2] TAI VIET SYMBOL KON..TAI VIET SYMBOL NUENG
unicode_id_continue(0xAADD, 0xAADD).	% ID_Continue Lm       TAI VIET SYMBOL SAM
unicode_id_continue(0xAAE0, 0xAAEA).	% ID_Continue Lo  [11] MEETEI MAYEK LETTER E..MEETEI MAYEK LETTER SSA
unicode_id_continue(0xAAEB, 0xAAEB).	% ID_Continue Mc       MEETEI MAYEK VOWEL SIGN II
unicode_id_continue(0xAAEC, 0xAAED).	% ID_Continue Mn   [2] MEETEI MAYEK VOWEL SIGN UU..MEETEI MAYEK VOWEL SIGN AAI
unicode_id_continue(0xAAEE, 0xAAEF).	% ID_Continue Mc   [2] MEETEI MAYEK VOWEL SIGN AU..MEETEI MAYEK VOWEL SIGN AAU
unicode_id_continue(0xAAF2, 0xAAF2).	% ID_Continue Lo       MEETEI MAYEK ANJI
unicode_id_continue(0xAAF3, 0xAAF4).	% ID_Continue Lm   [2] MEETEI MAYEK SYLLABLE REPETITION MARK..MEETEI MAYEK WORD REPETITION MARK
unicode_id_continue(0xAAF5, 0xAAF5).	% ID_Continue Mc       MEETEI MAYEK VOWEL SIGN VISARGA
unicode_id_continue(0xAAF6, 0xAAF6).	% ID_Continue Mn       MEETEI MAYEK VIRAMA
unicode_id_continue(0xAB01, 0xAB06).	% ID_Continue Lo   [6] ETHIOPIC SYLLABLE TTHU..ETHIOPIC SYLLABLE TTHO
unicode_id_continue(0xAB09, 0xAB0E).	% ID_Continue Lo   [6] ETHIOPIC SYLLABLE DDHU..ETHIOPIC SYLLABLE DDHO
unicode_id_continue(0xAB11, 0xAB16).	% ID_Continue Lo   [6] ETHIOPIC SYLLABLE DZU..ETHIOPIC SYLLABLE DZO
unicode_id_continue(0xAB20, 0xAB26).	% ID_Continue Lo   [7] ETHIOPIC SYLLABLE CCHHA..ETHIOPIC SYLLABLE CCHHO
unicode_id_continue(0xAB28, 0xAB2E).	% ID_Continue Lo   [7] ETHIOPIC SYLLABLE BBA..ETHIOPIC SYLLABLE BBO
unicode_id_continue(0xABC0, 0xABE2).	% ID_Continue Lo  [35] MEETEI MAYEK LETTER KOK..MEETEI MAYEK LETTER I LONSUM
unicode_id_continue(0xABE3, 0xABE4).	% ID_Continue Mc   [2] MEETEI MAYEK VOWEL SIGN ONAP..MEETEI MAYEK VOWEL SIGN INAP
unicode_id_continue(0xABE5, 0xABE5).	% ID_Continue Mn       MEETEI MAYEK VOWEL SIGN ANAP
unicode_id_continue(0xABE6, 0xABE7).	% ID_Continue Mc   [2] MEETEI MAYEK VOWEL SIGN YENAP..MEETEI MAYEK VOWEL SIGN SOUNAP
unicode_id_continue(0xABE8, 0xABE8).	% ID_Continue Mn       MEETEI MAYEK VOWEL SIGN UNAP
unicode_id_continue(0xABE9, 0xABEA).	% ID_Continue Mc   [2] MEETEI MAYEK VOWEL SIGN CHEINAP..MEETEI MAYEK VOWEL SIGN NUNG
unicode_id_continue(0xABEC, 0xABEC).	% ID_Continue Mc       MEETEI MAYEK LUM IYEK
unicode_id_continue(0xABED, 0xABED).	% ID_Continue Mn       MEETEI MAYEK APUN IYEK
unicode_id_continue(0xABF0, 0xABF9).	% ID_Continue Nd  [10] MEETEI MAYEK DIGIT ZERO..MEETEI MAYEK DIGIT NINE
unicode_id_continue(0xAC00, 0xD7A3).	% ID_Continue Lo [11172] HANGUL SYLLABLE GA..HANGUL SYLLABLE HIH
unicode_id_continue(0xD7B0, 0xD7C6).	% ID_Continue Lo  [23] HANGUL JUNGSEONG O-YEO..HANGUL JUNGSEONG ARAEA-E
unicode_id_continue(0xD7CB, 0xD7FB).	% ID_Continue Lo  [49] HANGUL JONGSEONG NIEUN-RIEUL..HANGUL JONGSEONG PHIEUPH-THIEUTH
unicode_id_continue(0xF900, 0xFA6D).	% ID_Continue Lo [366] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA6D
unicode_id_continue(0xFA70, 0xFAD9).	% ID_Continue Lo [106] CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9
unicode_id_continue(0xFB00, 0xFB06).	% ID_Continue L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_id_continue(0xFB13, 0xFB17).	% ID_Continue L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_id_continue(0xFB1D, 0xFB1D).	% ID_Continue Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_id_continue(0xFB1E, 0xFB1E).	% ID_Continue Mn       HEBREW POINT JUDEO-SPANISH VARIKA
unicode_id_continue(0xFB1F, 0xFB28).	% ID_Continue Lo  [10] HEBREW LIGATURE YIDDISH YOD YOD PATAH..HEBREW LETTER WIDE TAV
unicode_id_continue(0xFB2A, 0xFB36).	% ID_Continue Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_id_continue(0xFB38, 0xFB3C).	% ID_Continue Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_id_continue(0xFB3E, 0xFB3E).	% ID_Continue Lo       HEBREW LETTER MEM WITH DAGESH
unicode_id_continue(0xFB40, 0xFB41).	% ID_Continue Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_id_continue(0xFB43, 0xFB44).	% ID_Continue Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_id_continue(0xFB46, 0xFBB1).	% ID_Continue Lo [108] HEBREW LETTER TSADI WITH DAGESH..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
unicode_id_continue(0xFBD3, 0xFD3D).	% ID_Continue Lo [363] ARABIC LETTER NG ISOLATED FORM..ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
unicode_id_continue(0xFD50, 0xFD8F).	% ID_Continue Lo  [64] ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
unicode_id_continue(0xFD92, 0xFDC7).	% ID_Continue Lo  [54] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
unicode_id_continue(0xFDF0, 0xFDFB).	% ID_Continue Lo  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
unicode_id_continue(0xFE00, 0xFE0F).	% ID_Continue Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
unicode_id_continue(0xFE20, 0xFE26).	% ID_Continue Mn   [7] COMBINING LIGATURE LEFT HALF..COMBINING CONJOINING MACRON
unicode_id_continue(0xFE33, 0xFE34).	% ID_Continue Pc   [2] PRESENTATION FORM FOR VERTICAL LOW LINE..PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
unicode_id_continue(0xFE4D, 0xFE4F).	% ID_Continue Pc   [3] DASHED LOW LINE..WAVY LOW LINE
unicode_id_continue(0xFE70, 0xFE74).	% ID_Continue Lo   [5] ARABIC FATHATAN ISOLATED FORM..ARABIC KASRATAN ISOLATED FORM
unicode_id_continue(0xFE76, 0xFEFC).	% ID_Continue Lo [135] ARABIC FATHA ISOLATED FORM..ARABIC LIGATURE LAM WITH ALEF FINAL FORM
unicode_id_continue(0xFF10, 0xFF19).	% ID_Continue Nd  [10] FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
unicode_id_continue(0xFF21, 0xFF3A).	% ID_Continue L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_id_continue(0xFF3F, 0xFF3F).	% ID_Continue Pc       FULLWIDTH LOW LINE
unicode_id_continue(0xFF41, 0xFF5A).	% ID_Continue L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_id_continue(0xFF66, 0xFF6F).	% ID_Continue Lo  [10] HALFWIDTH KATAKANA LETTER WO..HALFWIDTH KATAKANA LETTER SMALL TU
unicode_id_continue(0xFF70, 0xFF70).	% ID_Continue Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_id_continue(0xFF71, 0xFF9D).	% ID_Continue Lo  [45] HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N
unicode_id_continue(0xFF9E, 0xFF9F).	% ID_Continue Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
unicode_id_continue(0xFFA0, 0xFFBE).	% ID_Continue Lo  [31] HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH
unicode_id_continue(0xFFC2, 0xFFC7).	% ID_Continue Lo   [6] HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E
unicode_id_continue(0xFFCA, 0xFFCF).	% ID_Continue Lo   [6] HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE
unicode_id_continue(0xFFD2, 0xFFD7).	% ID_Continue Lo   [6] HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU
unicode_id_continue(0xFFDA, 0xFFDC).	% ID_Continue Lo   [3] HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I
unicode_id_continue(0x10000, 0x1000B).	% ID_Continue Lo  [12] LINEAR B SYLLABLE B008 A..LINEAR B SYLLABLE B046 JE
unicode_id_continue(0x1000D, 0x10026).	% ID_Continue Lo  [26] LINEAR B SYLLABLE B036 JO..LINEAR B SYLLABLE B032 QO
unicode_id_continue(0x10028, 0x1003A).	% ID_Continue Lo  [19] LINEAR B SYLLABLE B060 RA..LINEAR B SYLLABLE B042 WO
unicode_id_continue(0x1003C, 0x1003D).	% ID_Continue Lo   [2] LINEAR B SYLLABLE B017 ZA..LINEAR B SYLLABLE B074 ZE
unicode_id_continue(0x1003F, 0x1004D).	% ID_Continue Lo  [15] LINEAR B SYLLABLE B020 ZO..LINEAR B SYLLABLE B091 TWO
unicode_id_continue(0x10050, 0x1005D).	% ID_Continue Lo  [14] LINEAR B SYMBOL B018..LINEAR B SYMBOL B089
unicode_id_continue(0x10080, 0x100FA).	% ID_Continue Lo [123] LINEAR B IDEOGRAM B100 MAN..LINEAR B IDEOGRAM VESSEL B305
unicode_id_continue(0x10140, 0x10174).	% ID_Continue Nl  [53] GREEK ACROPHONIC ATTIC ONE QUARTER..GREEK ACROPHONIC STRATIAN FIFTY MNAS
unicode_id_continue(0x101FD, 0x101FD).	% ID_Continue Mn       PHAISTOS DISC SIGN COMBINING OBLIQUE STROKE
unicode_id_continue(0x10280, 0x1029C).	% ID_Continue Lo  [29] LYCIAN LETTER A..LYCIAN LETTER X
unicode_id_continue(0x102A0, 0x102D0).	% ID_Continue Lo  [49] CARIAN LETTER A..CARIAN LETTER UUU3
unicode_id_continue(0x10300, 0x1031E).	% ID_Continue Lo  [31] OLD ITALIC LETTER A..OLD ITALIC LETTER UU
unicode_id_continue(0x10330, 0x10340).	% ID_Continue Lo  [17] GOTHIC LETTER AHSA..GOTHIC LETTER PAIRTHRA
unicode_id_continue(0x10341, 0x10341).	% ID_Continue Nl       GOTHIC LETTER NINETY
unicode_id_continue(0x10342, 0x10349).	% ID_Continue Lo   [8] GOTHIC LETTER RAIDA..GOTHIC LETTER OTHAL
unicode_id_continue(0x1034A, 0x1034A).	% ID_Continue Nl       GOTHIC LETTER NINE HUNDRED
unicode_id_continue(0x10380, 0x1039D).	% ID_Continue Lo  [30] UGARITIC LETTER ALPA..UGARITIC LETTER SSU
unicode_id_continue(0x103A0, 0x103C3).	% ID_Continue Lo  [36] OLD PERSIAN SIGN A..OLD PERSIAN SIGN HA
unicode_id_continue(0x103C8, 0x103CF).	% ID_Continue Lo   [8] OLD PERSIAN SIGN AURAMAZDAA..OLD PERSIAN SIGN BUUMISH
unicode_id_continue(0x103D1, 0x103D5).	% ID_Continue Nl   [5] OLD PERSIAN NUMBER ONE..OLD PERSIAN NUMBER HUNDRED
unicode_id_continue(0x10400, 0x1044F).	% ID_Continue L&  [80] DESERET CAPITAL LETTER LONG I..DESERET SMALL LETTER EW
unicode_id_continue(0x10450, 0x1049D).	% ID_Continue Lo  [78] SHAVIAN LETTER PEEP..OSMANYA LETTER OO
unicode_id_continue(0x104A0, 0x104A9).	% ID_Continue Nd  [10] OSMANYA DIGIT ZERO..OSMANYA DIGIT NINE
unicode_id_continue(0x10800, 0x10805).	% ID_Continue Lo   [6] CYPRIOT SYLLABLE A..CYPRIOT SYLLABLE JA
unicode_id_continue(0x10808, 0x10808).	% ID_Continue Lo       CYPRIOT SYLLABLE JO
unicode_id_continue(0x1080A, 0x10835).	% ID_Continue Lo  [44] CYPRIOT SYLLABLE KA..CYPRIOT SYLLABLE WO
unicode_id_continue(0x10837, 0x10838).	% ID_Continue Lo   [2] CYPRIOT SYLLABLE XA..CYPRIOT SYLLABLE XE
unicode_id_continue(0x1083C, 0x1083C).	% ID_Continue Lo       CYPRIOT SYLLABLE ZA
unicode_id_continue(0x1083F, 0x10855).	% ID_Continue Lo  [23] CYPRIOT SYLLABLE ZO..IMPERIAL ARAMAIC LETTER TAW
unicode_id_continue(0x10900, 0x10915).	% ID_Continue Lo  [22] PHOENICIAN LETTER ALF..PHOENICIAN LETTER TAU
unicode_id_continue(0x10920, 0x10939).	% ID_Continue Lo  [26] LYDIAN LETTER A..LYDIAN LETTER C
unicode_id_continue(0x10980, 0x109B7).	% ID_Continue Lo  [56] MEROITIC HIEROGLYPHIC LETTER A..MEROITIC CURSIVE LETTER DA
unicode_id_continue(0x109BE, 0x109BF).	% ID_Continue Lo   [2] MEROITIC CURSIVE LOGOGRAM RMT..MEROITIC CURSIVE LOGOGRAM IMN
unicode_id_continue(0x10A00, 0x10A00).	% ID_Continue Lo       KHAROSHTHI LETTER A
unicode_id_continue(0x10A01, 0x10A03).	% ID_Continue Mn   [3] KHAROSHTHI VOWEL SIGN I..KHAROSHTHI VOWEL SIGN VOCALIC R
unicode_id_continue(0x10A05, 0x10A06).	% ID_Continue Mn   [2] KHAROSHTHI VOWEL SIGN E..KHAROSHTHI VOWEL SIGN O
unicode_id_continue(0x10A0C, 0x10A0F).	% ID_Continue Mn   [4] KHAROSHTHI VOWEL LENGTH MARK..KHAROSHTHI SIGN VISARGA
unicode_id_continue(0x10A10, 0x10A13).	% ID_Continue Lo   [4] KHAROSHTHI LETTER KA..KHAROSHTHI LETTER GHA
unicode_id_continue(0x10A15, 0x10A17).	% ID_Continue Lo   [3] KHAROSHTHI LETTER CA..KHAROSHTHI LETTER JA
unicode_id_continue(0x10A19, 0x10A33).	% ID_Continue Lo  [27] KHAROSHTHI LETTER NYA..KHAROSHTHI LETTER TTTHA
unicode_id_continue(0x10A38, 0x10A3A).	% ID_Continue Mn   [3] KHAROSHTHI SIGN BAR ABOVE..KHAROSHTHI SIGN DOT BELOW
unicode_id_continue(0x10A3F, 0x10A3F).	% ID_Continue Mn       KHAROSHTHI VIRAMA
unicode_id_continue(0x10A60, 0x10A7C).	% ID_Continue Lo  [29] OLD SOUTH ARABIAN LETTER HE..OLD SOUTH ARABIAN LETTER THETH
unicode_id_continue(0x10B00, 0x10B35).	% ID_Continue Lo  [54] AVESTAN LETTER A..AVESTAN LETTER HE
unicode_id_continue(0x10B40, 0x10B55).	% ID_Continue Lo  [22] INSCRIPTIONAL PARTHIAN LETTER ALEPH..INSCRIPTIONAL PARTHIAN LETTER TAW
unicode_id_continue(0x10B60, 0x10B72).	% ID_Continue Lo  [19] INSCRIPTIONAL PAHLAVI LETTER ALEPH..INSCRIPTIONAL PAHLAVI LETTER TAW
unicode_id_continue(0x10C00, 0x10C48).	% ID_Continue Lo  [73] OLD TURKIC LETTER ORKHON A..OLD TURKIC LETTER ORKHON BASH
unicode_id_continue(0x11000, 0x11000).	% ID_Continue Mc       BRAHMI SIGN CANDRABINDU
unicode_id_continue(0x11001, 0x11001).	% ID_Continue Mn       BRAHMI SIGN ANUSVARA
unicode_id_continue(0x11002, 0x11002).	% ID_Continue Mc       BRAHMI SIGN VISARGA
unicode_id_continue(0x11003, 0x11037).	% ID_Continue Lo  [53] BRAHMI SIGN JIHVAMULIYA..BRAHMI LETTER OLD TAMIL NNNA
unicode_id_continue(0x11038, 0x11046).	% ID_Continue Mn  [15] BRAHMI VOWEL SIGN AA..BRAHMI VIRAMA
unicode_id_continue(0x11066, 0x1106F).	% ID_Continue Nd  [10] BRAHMI DIGIT ZERO..BRAHMI DIGIT NINE
unicode_id_continue(0x11080, 0x11081).	% ID_Continue Mn   [2] KAITHI SIGN CANDRABINDU..KAITHI SIGN ANUSVARA
unicode_id_continue(0x11082, 0x11082).	% ID_Continue Mc       KAITHI SIGN VISARGA
unicode_id_continue(0x11083, 0x110AF).	% ID_Continue Lo  [45] KAITHI LETTER A..KAITHI LETTER HA
unicode_id_continue(0x110B0, 0x110B2).	% ID_Continue Mc   [3] KAITHI VOWEL SIGN AA..KAITHI VOWEL SIGN II
unicode_id_continue(0x110B3, 0x110B6).	% ID_Continue Mn   [4] KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN AI
unicode_id_continue(0x110B7, 0x110B8).	% ID_Continue Mc   [2] KAITHI VOWEL SIGN O..KAITHI VOWEL SIGN AU
unicode_id_continue(0x110B9, 0x110BA).	% ID_Continue Mn   [2] KAITHI SIGN VIRAMA..KAITHI SIGN NUKTA
unicode_id_continue(0x110D0, 0x110E8).	% ID_Continue Lo  [25] SORA SOMPENG LETTER SAH..SORA SOMPENG LETTER MAE
unicode_id_continue(0x110F0, 0x110F9).	% ID_Continue Nd  [10] SORA SOMPENG DIGIT ZERO..SORA SOMPENG DIGIT NINE
unicode_id_continue(0x11100, 0x11102).	% ID_Continue Mn   [3] CHAKMA SIGN CANDRABINDU..CHAKMA SIGN VISARGA
unicode_id_continue(0x11103, 0x11126).	% ID_Continue Lo  [36] CHAKMA LETTER AA..CHAKMA LETTER HAA
unicode_id_continue(0x11127, 0x1112B).	% ID_Continue Mn   [5] CHAKMA VOWEL SIGN A..CHAKMA VOWEL SIGN UU
unicode_id_continue(0x1112C, 0x1112C).	% ID_Continue Mc       CHAKMA VOWEL SIGN E
unicode_id_continue(0x1112D, 0x11134).	% ID_Continue Mn   [8] CHAKMA VOWEL SIGN AI..CHAKMA MAAYYAA
unicode_id_continue(0x11136, 0x1113F).	% ID_Continue Nd  [10] CHAKMA DIGIT ZERO..CHAKMA DIGIT NINE
unicode_id_continue(0x11180, 0x11181).	% ID_Continue Mn   [2] SHARADA SIGN CANDRABINDU..SHARADA SIGN ANUSVARA
unicode_id_continue(0x11182, 0x11182).	% ID_Continue Mc       SHARADA SIGN VISARGA
unicode_id_continue(0x11183, 0x111B2).	% ID_Continue Lo  [48] SHARADA LETTER A..SHARADA LETTER HA
unicode_id_continue(0x111B3, 0x111B5).	% ID_Continue Mc   [3] SHARADA VOWEL SIGN AA..SHARADA VOWEL SIGN II
unicode_id_continue(0x111B6, 0x111BE).	% ID_Continue Mn   [9] SHARADA VOWEL SIGN U..SHARADA VOWEL SIGN O
unicode_id_continue(0x111BF, 0x111C0).	% ID_Continue Mc   [2] SHARADA VOWEL SIGN AU..SHARADA SIGN VIRAMA
unicode_id_continue(0x111C1, 0x111C4).	% ID_Continue Lo   [4] SHARADA SIGN AVAGRAHA..SHARADA OM
unicode_id_continue(0x111D0, 0x111D9).	% ID_Continue Nd  [10] SHARADA DIGIT ZERO..SHARADA DIGIT NINE
unicode_id_continue(0x11680, 0x116AA).	% ID_Continue Lo  [43] TAKRI LETTER A..TAKRI LETTER RRA
unicode_id_continue(0x116AB, 0x116AB).	% ID_Continue Mn       TAKRI SIGN ANUSVARA
unicode_id_continue(0x116AC, 0x116AC).	% ID_Continue Mc       TAKRI SIGN VISARGA
unicode_id_continue(0x116AD, 0x116AD).	% ID_Continue Mn       TAKRI VOWEL SIGN AA
unicode_id_continue(0x116AE, 0x116AF).	% ID_Continue Mc   [2] TAKRI VOWEL SIGN I..TAKRI VOWEL SIGN II
unicode_id_continue(0x116B0, 0x116B5).	% ID_Continue Mn   [6] TAKRI VOWEL SIGN U..TAKRI VOWEL SIGN AU
unicode_id_continue(0x116B6, 0x116B6).	% ID_Continue Mc       TAKRI SIGN VIRAMA
unicode_id_continue(0x116B7, 0x116B7).	% ID_Continue Mn       TAKRI SIGN NUKTA
unicode_id_continue(0x116C0, 0x116C9).	% ID_Continue Nd  [10] TAKRI DIGIT ZERO..TAKRI DIGIT NINE
unicode_id_continue(0x12000, 0x1236E).	% ID_Continue Lo [879] CUNEIFORM SIGN A..CUNEIFORM SIGN ZUM
unicode_id_continue(0x12400, 0x12462).	% ID_Continue Nl  [99] CUNEIFORM NUMERIC SIGN TWO ASH..CUNEIFORM NUMERIC SIGN OLD ASSYRIAN ONE QUARTER
unicode_id_continue(0x13000, 0x1342E).	% ID_Continue Lo [1071] EGYPTIAN HIEROGLYPH A001..EGYPTIAN HIEROGLYPH AA032
unicode_id_continue(0x16800, 0x16A38).	% ID_Continue Lo [569] BAMUM LETTER PHASE-A NGKUE MFON..BAMUM LETTER PHASE-F VUEQ
unicode_id_continue(0x16F00, 0x16F44).	% ID_Continue Lo  [69] MIAO LETTER PA..MIAO LETTER HHA
unicode_id_continue(0x16F50, 0x16F50).	% ID_Continue Lo       MIAO LETTER NASALIZATION
unicode_id_continue(0x16F51, 0x16F7E).	% ID_Continue Mc  [46] MIAO SIGN ASPIRATION..MIAO VOWEL SIGN NG
unicode_id_continue(0x16F8F, 0x16F92).	% ID_Continue Mn   [4] MIAO TONE RIGHT..MIAO TONE BELOW
unicode_id_continue(0x16F93, 0x16F9F).	% ID_Continue Lm  [13] MIAO LETTER TONE-2..MIAO LETTER REFORMED TONE-8
unicode_id_continue(0x1B000, 0x1B001).	% ID_Continue Lo   [2] KATAKANA LETTER ARCHAIC E..HIRAGANA LETTER ARCHAIC YE
unicode_id_continue(0x1D165, 0x1D166).	% ID_Continue Mc   [2] MUSICAL SYMBOL COMBINING STEM..MUSICAL SYMBOL COMBINING SPRECHGESANG STEM
unicode_id_continue(0x1D167, 0x1D169).	% ID_Continue Mn   [3] MUSICAL SYMBOL COMBINING TREMOLO-1..MUSICAL SYMBOL COMBINING TREMOLO-3
unicode_id_continue(0x1D16D, 0x1D172).	% ID_Continue Mc   [6] MUSICAL SYMBOL COMBINING AUGMENTATION DOT..MUSICAL SYMBOL COMBINING FLAG-5
unicode_id_continue(0x1D17B, 0x1D182).	% ID_Continue Mn   [8] MUSICAL SYMBOL COMBINING ACCENT..MUSICAL SYMBOL COMBINING LOURE
unicode_id_continue(0x1D185, 0x1D18B).	% ID_Continue Mn   [7] MUSICAL SYMBOL COMBINING DOIT..MUSICAL SYMBOL COMBINING TRIPLE TONGUE
unicode_id_continue(0x1D1AA, 0x1D1AD).	% ID_Continue Mn   [4] MUSICAL SYMBOL COMBINING DOWN BOW..MUSICAL SYMBOL COMBINING SNAP PIZZICATO
unicode_id_continue(0x1D242, 0x1D244).	% ID_Continue Mn   [3] COMBINING GREEK MUSICAL TRISEME..COMBINING GREEK MUSICAL PENTASEME
unicode_id_continue(0x1D400, 0x1D454).	% ID_Continue L&  [85] MATHEMATICAL BOLD CAPITAL A..MATHEMATICAL ITALIC SMALL G
unicode_id_continue(0x1D456, 0x1D49C).	% ID_Continue L&  [71] MATHEMATICAL ITALIC SMALL I..MATHEMATICAL SCRIPT CAPITAL A
unicode_id_continue(0x1D49E, 0x1D49F).	% ID_Continue L&   [2] MATHEMATICAL SCRIPT CAPITAL C..MATHEMATICAL SCRIPT CAPITAL D
unicode_id_continue(0x1D4A2, 0x1D4A2).	% ID_Continue L&       MATHEMATICAL SCRIPT CAPITAL G
unicode_id_continue(0x1D4A5, 0x1D4A6).	% ID_Continue L&   [2] MATHEMATICAL SCRIPT CAPITAL J..MATHEMATICAL SCRIPT CAPITAL K
unicode_id_continue(0x1D4A9, 0x1D4AC).	% ID_Continue L&   [4] MATHEMATICAL SCRIPT CAPITAL N..MATHEMATICAL SCRIPT CAPITAL Q
unicode_id_continue(0x1D4AE, 0x1D4B9).	% ID_Continue L&  [12] MATHEMATICAL SCRIPT CAPITAL S..MATHEMATICAL SCRIPT SMALL D
unicode_id_continue(0x1D4BB, 0x1D4BB).	% ID_Continue L&       MATHEMATICAL SCRIPT SMALL F
unicode_id_continue(0x1D4BD, 0x1D4C3).	% ID_Continue L&   [7] MATHEMATICAL SCRIPT SMALL H..MATHEMATICAL SCRIPT SMALL N
unicode_id_continue(0x1D4C5, 0x1D505).	% ID_Continue L&  [65] MATHEMATICAL SCRIPT SMALL P..MATHEMATICAL FRAKTUR CAPITAL B
unicode_id_continue(0x1D507, 0x1D50A).	% ID_Continue L&   [4] MATHEMATICAL FRAKTUR CAPITAL D..MATHEMATICAL FRAKTUR CAPITAL G
unicode_id_continue(0x1D50D, 0x1D514).	% ID_Continue L&   [8] MATHEMATICAL FRAKTUR CAPITAL J..MATHEMATICAL FRAKTUR CAPITAL Q
unicode_id_continue(0x1D516, 0x1D51C).	% ID_Continue L&   [7] MATHEMATICAL FRAKTUR CAPITAL S..MATHEMATICAL FRAKTUR CAPITAL Y
unicode_id_continue(0x1D51E, 0x1D539).	% ID_Continue L&  [28] MATHEMATICAL FRAKTUR SMALL A..MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_id_continue(0x1D53B, 0x1D53E).	% ID_Continue L&   [4] MATHEMATICAL DOUBLE-STRUCK CAPITAL D..MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_id_continue(0x1D540, 0x1D544).	% ID_Continue L&   [5] MATHEMATICAL DOUBLE-STRUCK CAPITAL I..MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_id_continue(0x1D546, 0x1D546).	% ID_Continue L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_id_continue(0x1D54A, 0x1D550).	% ID_Continue L&   [7] MATHEMATICAL DOUBLE-STRUCK CAPITAL S..MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_id_continue(0x1D552, 0x1D6A5).	% ID_Continue L& [340] MATHEMATICAL DOUBLE-STRUCK SMALL A..MATHEMATICAL ITALIC SMALL DOTLESS J
unicode_id_continue(0x1D6A8, 0x1D6C0).	% ID_Continue L&  [25] MATHEMATICAL BOLD CAPITAL ALPHA..MATHEMATICAL BOLD CAPITAL OMEGA
unicode_id_continue(0x1D6C2, 0x1D6DA).	% ID_Continue L&  [25] MATHEMATICAL BOLD SMALL ALPHA..MATHEMATICAL BOLD SMALL OMEGA
unicode_id_continue(0x1D6DC, 0x1D6FA).	% ID_Continue L&  [31] MATHEMATICAL BOLD EPSILON SYMBOL..MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_id_continue(0x1D6FC, 0x1D714).	% ID_Continue L&  [25] MATHEMATICAL ITALIC SMALL ALPHA..MATHEMATICAL ITALIC SMALL OMEGA
unicode_id_continue(0x1D716, 0x1D734).	% ID_Continue L&  [31] MATHEMATICAL ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_id_continue(0x1D736, 0x1D74E).	% ID_Continue L&  [25] MATHEMATICAL BOLD ITALIC SMALL ALPHA..MATHEMATICAL BOLD ITALIC SMALL OMEGA
unicode_id_continue(0x1D750, 0x1D76E).	% ID_Continue L&  [31] MATHEMATICAL BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_id_continue(0x1D770, 0x1D788).	% ID_Continue L&  [25] MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
unicode_id_continue(0x1D78A, 0x1D7A8).	% ID_Continue L&  [31] MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_id_continue(0x1D7AA, 0x1D7C2).	% ID_Continue L&  [25] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
unicode_id_continue(0x1D7C4, 0x1D7CB).	% ID_Continue L&   [8] MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD SMALL DIGAMMA
unicode_id_continue(0x1D7CE, 0x1D7FF).	% ID_Continue Nd  [50] MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
unicode_id_continue(0x1EE00, 0x1EE03).	% ID_Continue Lo   [4] ARABIC MATHEMATICAL ALEF..ARABIC MATHEMATICAL DAL
unicode_id_continue(0x1EE05, 0x1EE1F).	% ID_Continue Lo  [27] ARABIC MATHEMATICAL WAW..ARABIC MATHEMATICAL DOTLESS QAF
unicode_id_continue(0x1EE21, 0x1EE22).	% ID_Continue Lo   [2] ARABIC MATHEMATICAL INITIAL BEH..ARABIC MATHEMATICAL INITIAL JEEM
unicode_id_continue(0x1EE24, 0x1EE24).	% ID_Continue Lo       ARABIC MATHEMATICAL INITIAL HEH
unicode_id_continue(0x1EE27, 0x1EE27).	% ID_Continue Lo       ARABIC MATHEMATICAL INITIAL HAH
unicode_id_continue(0x1EE29, 0x1EE32).	% ID_Continue Lo  [10] ARABIC MATHEMATICAL INITIAL YEH..ARABIC MATHEMATICAL INITIAL QAF
unicode_id_continue(0x1EE34, 0x1EE37).	% ID_Continue Lo   [4] ARABIC MATHEMATICAL INITIAL SHEEN..ARABIC MATHEMATICAL INITIAL KHAH
unicode_id_continue(0x1EE39, 0x1EE39).	% ID_Continue Lo       ARABIC MATHEMATICAL INITIAL DAD
unicode_id_continue(0x1EE3B, 0x1EE3B).	% ID_Continue Lo       ARABIC MATHEMATICAL INITIAL GHAIN
unicode_id_continue(0x1EE42, 0x1EE42).	% ID_Continue Lo       ARABIC MATHEMATICAL TAILED JEEM
unicode_id_continue(0x1EE47, 0x1EE47).	% ID_Continue Lo       ARABIC MATHEMATICAL TAILED HAH
unicode_id_continue(0x1EE49, 0x1EE49).	% ID_Continue Lo       ARABIC MATHEMATICAL TAILED YEH
unicode_id_continue(0x1EE4B, 0x1EE4B).	% ID_Continue Lo       ARABIC MATHEMATICAL TAILED LAM
unicode_id_continue(0x1EE4D, 0x1EE4F).	% ID_Continue Lo   [3] ARABIC MATHEMATICAL TAILED NOON..ARABIC MATHEMATICAL TAILED AIN
unicode_id_continue(0x1EE51, 0x1EE52).	% ID_Continue Lo   [2] ARABIC MATHEMATICAL TAILED SAD..ARABIC MATHEMATICAL TAILED QAF
unicode_id_continue(0x1EE54, 0x1EE54).	% ID_Continue Lo       ARABIC MATHEMATICAL TAILED SHEEN
unicode_id_continue(0x1EE57, 0x1EE57).	% ID_Continue Lo       ARABIC MATHEMATICAL TAILED KHAH
unicode_id_continue(0x1EE59, 0x1EE59).	% ID_Continue Lo       ARABIC MATHEMATICAL TAILED DAD
unicode_id_continue(0x1EE5B, 0x1EE5B).	% ID_Continue Lo       ARABIC MATHEMATICAL TAILED GHAIN
unicode_id_continue(0x1EE5D, 0x1EE5D).	% ID_Continue Lo       ARABIC MATHEMATICAL TAILED DOTLESS NOON
unicode_id_continue(0x1EE5F, 0x1EE5F).	% ID_Continue Lo       ARABIC MATHEMATICAL TAILED DOTLESS QAF
unicode_id_continue(0x1EE61, 0x1EE62).	% ID_Continue Lo   [2] ARABIC MATHEMATICAL STRETCHED BEH..ARABIC MATHEMATICAL STRETCHED JEEM
unicode_id_continue(0x1EE64, 0x1EE64).	% ID_Continue Lo       ARABIC MATHEMATICAL STRETCHED HEH
unicode_id_continue(0x1EE67, 0x1EE6A).	% ID_Continue Lo   [4] ARABIC MATHEMATICAL STRETCHED HAH..ARABIC MATHEMATICAL STRETCHED KAF
unicode_id_continue(0x1EE6C, 0x1EE72).	% ID_Continue Lo   [7] ARABIC MATHEMATICAL STRETCHED MEEM..ARABIC MATHEMATICAL STRETCHED QAF
unicode_id_continue(0x1EE74, 0x1EE77).	% ID_Continue Lo   [4] ARABIC MATHEMATICAL STRETCHED SHEEN..ARABIC MATHEMATICAL STRETCHED KHAH
unicode_id_continue(0x1EE79, 0x1EE7C).	% ID_Continue Lo   [4] ARABIC MATHEMATICAL STRETCHED DAD..ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
unicode_id_continue(0x1EE7E, 0x1EE7E).	% ID_Continue Lo       ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
unicode_id_continue(0x1EE80, 0x1EE89).	% ID_Continue Lo  [10] ARABIC MATHEMATICAL LOOPED ALEF..ARABIC MATHEMATICAL LOOPED YEH
unicode_id_continue(0x1EE8B, 0x1EE9B).	% ID_Continue Lo  [17] ARABIC MATHEMATICAL LOOPED LAM..ARABIC MATHEMATICAL LOOPED GHAIN
unicode_id_continue(0x1EEA1, 0x1EEA3).	% ID_Continue Lo   [3] ARABIC MATHEMATICAL DOUBLE-STRUCK BEH..ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
unicode_id_continue(0x1EEA5, 0x1EEA9).	% ID_Continue Lo   [5] ARABIC MATHEMATICAL DOUBLE-STRUCK WAW..ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
unicode_id_continue(0x1EEAB, 0x1EEBB).	% ID_Continue Lo  [17] ARABIC MATHEMATICAL DOUBLE-STRUCK LAM..ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN
unicode_id_continue(0x20000, 0x2A6D6).	% ID_Continue Lo [42711] CJK UNIFIED IDEOGRAPH-20000..CJK UNIFIED IDEOGRAPH-2A6D6
unicode_id_continue(0x2A700, 0x2B734).	% ID_Continue Lo [4149] CJK UNIFIED IDEOGRAPH-2A700..CJK UNIFIED IDEOGRAPH-2B734
unicode_id_continue(0x2B740, 0x2B81D).	% ID_Continue Lo [222] CJK UNIFIED IDEOGRAPH-2B740..CJK UNIFIED IDEOGRAPH-2B81D
unicode_id_continue(0x2F800, 0x2FA1D).	% ID_Continue Lo [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D
unicode_id_continue(0xE0100, 0xE01EF).	% ID_Continue Mn [240] VARIATION SELECTOR-17..VARIATION SELECTOR-256

% Total code points: 103355
