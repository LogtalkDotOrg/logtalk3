%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: June 20, 2013
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

% use the unicode_alphabetic/1 predicate defined in the derived
% file "unicode_range_alphabetic.pl" for better performance
%
%unicode_alphabetic(CodePoint) :-
%	(	var(CodePoint) ->
%		% generate code point pairs
%		unicode_alphabetic(CodePointStart, CodePointEnd),
%		between(CodePointStart, CodePointEnd, CodePoint)
%	;	% try first-argument indexing first
%		unicode_alphabetic(CodePoint, _) ->
%		true
%	;	% look for a code point range that includes the given code point
%		unicode_alphabetic(CodePointStart, CodePointEnd),
%		between(CodePointStart, CodePointEnd, CodePoint) ->
%		true
%	).

% ================================================

% Derived Property: Alphabetic
%  Generated from: Lu+Ll+Lt+Lm+Lo+Nl + Other_Alphabetic

unicode_alphabetic(0x0041, 0x005A).	% Alphabetic L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
unicode_alphabetic(0x0061, 0x007A).	% Alphabetic L&  [26] LATIN SMALL LETTER A..LATIN SMALL LETTER Z
unicode_alphabetic(0x00AA, 0x00AA).	% Alphabetic Lo       FEMININE ORDINAL INDICATOR
unicode_alphabetic(0x00B5, 0x00B5).	% Alphabetic L&       MICRO SIGN
unicode_alphabetic(0x00BA, 0x00BA).	% Alphabetic Lo       MASCULINE ORDINAL INDICATOR
unicode_alphabetic(0x00C0, 0x00D6).	% Alphabetic L&  [23] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_alphabetic(0x00D8, 0x00F6).	% Alphabetic L&  [31] LATIN CAPITAL LETTER O WITH STROKE..LATIN SMALL LETTER O WITH DIAERESIS
unicode_alphabetic(0x00F8, 0x01BA).	% Alphabetic L& [195] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER EZH WITH TAIL
unicode_alphabetic(0x01BB, 0x01BB).	% Alphabetic Lo       LATIN LETTER TWO WITH STROKE
unicode_alphabetic(0x01BC, 0x01BF).	% Alphabetic L&   [4] LATIN CAPITAL LETTER TONE FIVE..LATIN LETTER WYNN
unicode_alphabetic(0x01C0, 0x01C3).	% Alphabetic Lo   [4] LATIN LETTER DENTAL CLICK..LATIN LETTER RETROFLEX CLICK
unicode_alphabetic(0x01C4, 0x0293).	% Alphabetic L& [208] LATIN CAPITAL LETTER DZ WITH CARON..LATIN SMALL LETTER EZH WITH CURL
unicode_alphabetic(0x0294, 0x0294).	% Alphabetic Lo       LATIN LETTER GLOTTAL STOP
unicode_alphabetic(0x0295, 0x02AF).	% Alphabetic L&  [27] LATIN LETTER PHARYNGEAL VOICED FRICATIVE..LATIN SMALL LETTER TURNED H WITH FISHHOOK AND TAIL
unicode_alphabetic(0x02B0, 0x02C1).	% Alphabetic Lm  [18] MODIFIER LETTER SMALL H..MODIFIER LETTER REVERSED GLOTTAL STOP
unicode_alphabetic(0x02C6, 0x02D1).	% Alphabetic Lm  [12] MODIFIER LETTER CIRCUMFLEX ACCENT..MODIFIER LETTER HALF TRIANGULAR COLON
unicode_alphabetic(0x02E0, 0x02E4).	% Alphabetic Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_alphabetic(0x02EC, 0x02EC).	% Alphabetic Lm       MODIFIER LETTER VOICING
unicode_alphabetic(0x02EE, 0x02EE).	% Alphabetic Lm       MODIFIER LETTER DOUBLE APOSTROPHE
unicode_alphabetic(0x0345, 0x0345).	% Alphabetic Mn       COMBINING GREEK YPOGEGRAMMENI
unicode_alphabetic(0x0370, 0x0373).	% Alphabetic L&   [4] GREEK CAPITAL LETTER HETA..GREEK SMALL LETTER ARCHAIC SAMPI
unicode_alphabetic(0x0374, 0x0374).	% Alphabetic Lm       GREEK NUMERAL SIGN
unicode_alphabetic(0x0376, 0x0377).	% Alphabetic L&   [2] GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA..GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
unicode_alphabetic(0x037A, 0x037A).	% Alphabetic Lm       GREEK YPOGEGRAMMENI
unicode_alphabetic(0x037B, 0x037D).	% Alphabetic L&   [3] GREEK SMALL REVERSED LUNATE SIGMA SYMBOL..GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
unicode_alphabetic(0x0386, 0x0386).	% Alphabetic L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_alphabetic(0x0388, 0x038A).	% Alphabetic L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_alphabetic(0x038C, 0x038C).	% Alphabetic L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_alphabetic(0x038E, 0x03A1).	% Alphabetic L&  [20] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK CAPITAL LETTER RHO
unicode_alphabetic(0x03A3, 0x03F5).	% Alphabetic L&  [83] GREEK CAPITAL LETTER SIGMA..GREEK LUNATE EPSILON SYMBOL
unicode_alphabetic(0x03F7, 0x0481).	% Alphabetic L& [139] GREEK CAPITAL LETTER SHO..CYRILLIC SMALL LETTER KOPPA
unicode_alphabetic(0x048A, 0x0527).	% Alphabetic L& [158] CYRILLIC CAPITAL LETTER SHORT I WITH TAIL..CYRILLIC SMALL LETTER SHHA WITH DESCENDER
unicode_alphabetic(0x0531, 0x0556).	% Alphabetic L&  [38] ARMENIAN CAPITAL LETTER AYB..ARMENIAN CAPITAL LETTER FEH
unicode_alphabetic(0x0559, 0x0559).	% Alphabetic Lm       ARMENIAN MODIFIER LETTER LEFT HALF RING
unicode_alphabetic(0x0561, 0x0587).	% Alphabetic L&  [39] ARMENIAN SMALL LETTER AYB..ARMENIAN SMALL LIGATURE ECH YIWN
unicode_alphabetic(0x05B0, 0x05BD).	% Alphabetic Mn  [14] HEBREW POINT SHEVA..HEBREW POINT METEG
unicode_alphabetic(0x05BF, 0x05BF).	% Alphabetic Mn       HEBREW POINT RAFE
unicode_alphabetic(0x05C1, 0x05C2).	% Alphabetic Mn   [2] HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT
unicode_alphabetic(0x05C4, 0x05C5).	% Alphabetic Mn   [2] HEBREW MARK UPPER DOT..HEBREW MARK LOWER DOT
unicode_alphabetic(0x05C7, 0x05C7).	% Alphabetic Mn       HEBREW POINT QAMATS QATAN
unicode_alphabetic(0x05D0, 0x05EA).	% Alphabetic Lo  [27] HEBREW LETTER ALEF..HEBREW LETTER TAV
unicode_alphabetic(0x05F0, 0x05F2).	% Alphabetic Lo   [3] HEBREW LIGATURE YIDDISH DOUBLE VAV..HEBREW LIGATURE YIDDISH DOUBLE YOD
unicode_alphabetic(0x0610, 0x061A).	% Alphabetic Mn  [11] ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM..ARABIC SMALL KASRA
unicode_alphabetic(0x0620, 0x063F).	% Alphabetic Lo  [32] ARABIC LETTER KASHMIRI YEH..ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE
unicode_alphabetic(0x0640, 0x0640).	% Alphabetic Lm       ARABIC TATWEEL
unicode_alphabetic(0x0641, 0x064A).	% Alphabetic Lo  [10] ARABIC LETTER FEH..ARABIC LETTER YEH
unicode_alphabetic(0x064B, 0x0657).	% Alphabetic Mn  [13] ARABIC FATHATAN..ARABIC INVERTED DAMMA
unicode_alphabetic(0x0659, 0x065F).	% Alphabetic Mn   [7] ARABIC ZWARAKAY..ARABIC WAVY HAMZA BELOW
unicode_alphabetic(0x066E, 0x066F).	% Alphabetic Lo   [2] ARABIC LETTER DOTLESS BEH..ARABIC LETTER DOTLESS QAF
unicode_alphabetic(0x0670, 0x0670).	% Alphabetic Mn       ARABIC LETTER SUPERSCRIPT ALEF
unicode_alphabetic(0x0671, 0x06D3).	% Alphabetic Lo  [99] ARABIC LETTER ALEF WASLA..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
unicode_alphabetic(0x06D5, 0x06D5).	% Alphabetic Lo       ARABIC LETTER AE
unicode_alphabetic(0x06D6, 0x06DC).	% Alphabetic Mn   [7] ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN
unicode_alphabetic(0x06E1, 0x06E4).	% Alphabetic Mn   [4] ARABIC SMALL HIGH DOTLESS HEAD OF KHAH..ARABIC SMALL HIGH MADDA
unicode_alphabetic(0x06E5, 0x06E6).	% Alphabetic Lm   [2] ARABIC SMALL WAW..ARABIC SMALL YEH
unicode_alphabetic(0x06E7, 0x06E8).	% Alphabetic Mn   [2] ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON
unicode_alphabetic(0x06ED, 0x06ED).	% Alphabetic Mn       ARABIC SMALL LOW MEEM
unicode_alphabetic(0x06EE, 0x06EF).	% Alphabetic Lo   [2] ARABIC LETTER DAL WITH INVERTED V..ARABIC LETTER REH WITH INVERTED V
unicode_alphabetic(0x06FA, 0x06FC).	% Alphabetic Lo   [3] ARABIC LETTER SHEEN WITH DOT BELOW..ARABIC LETTER GHAIN WITH DOT BELOW
unicode_alphabetic(0x06FF, 0x06FF).	% Alphabetic Lo       ARABIC LETTER HEH WITH INVERTED V
unicode_alphabetic(0x0710, 0x0710).	% Alphabetic Lo       SYRIAC LETTER ALAPH
unicode_alphabetic(0x0711, 0x0711).	% Alphabetic Mn       SYRIAC LETTER SUPERSCRIPT ALAPH
unicode_alphabetic(0x0712, 0x072F).	% Alphabetic Lo  [30] SYRIAC LETTER BETH..SYRIAC LETTER PERSIAN DHALATH
unicode_alphabetic(0x0730, 0x073F).	% Alphabetic Mn  [16] SYRIAC PTHAHA ABOVE..SYRIAC RWAHA
unicode_alphabetic(0x074D, 0x07A5).	% Alphabetic Lo  [89] SYRIAC LETTER SOGDIAN ZHAIN..THAANA LETTER WAAVU
unicode_alphabetic(0x07A6, 0x07B0).	% Alphabetic Mn  [11] THAANA ABAFILI..THAANA SUKUN
unicode_alphabetic(0x07B1, 0x07B1).	% Alphabetic Lo       THAANA LETTER NAA
unicode_alphabetic(0x07CA, 0x07EA).	% Alphabetic Lo  [33] NKO LETTER A..NKO LETTER JONA RA
unicode_alphabetic(0x07F4, 0x07F5).	% Alphabetic Lm   [2] NKO HIGH TONE APOSTROPHE..NKO LOW TONE APOSTROPHE
unicode_alphabetic(0x07FA, 0x07FA).	% Alphabetic Lm       NKO LAJANYALAN
unicode_alphabetic(0x0800, 0x0815).	% Alphabetic Lo  [22] SAMARITAN LETTER ALAF..SAMARITAN LETTER TAAF
unicode_alphabetic(0x0816, 0x0817).	% Alphabetic Mn   [2] SAMARITAN MARK IN..SAMARITAN MARK IN-ALAF
unicode_alphabetic(0x081A, 0x081A).	% Alphabetic Lm       SAMARITAN MODIFIER LETTER EPENTHETIC YUT
unicode_alphabetic(0x081B, 0x0823).	% Alphabetic Mn   [9] SAMARITAN MARK EPENTHETIC YUT..SAMARITAN VOWEL SIGN A
unicode_alphabetic(0x0824, 0x0824).	% Alphabetic Lm       SAMARITAN MODIFIER LETTER SHORT A
unicode_alphabetic(0x0825, 0x0827).	% Alphabetic Mn   [3] SAMARITAN VOWEL SIGN SHORT A..SAMARITAN VOWEL SIGN U
unicode_alphabetic(0x0828, 0x0828).	% Alphabetic Lm       SAMARITAN MODIFIER LETTER I
unicode_alphabetic(0x0829, 0x082C).	% Alphabetic Mn   [4] SAMARITAN VOWEL SIGN LONG I..SAMARITAN VOWEL SIGN SUKUN
unicode_alphabetic(0x0840, 0x0858).	% Alphabetic Lo  [25] MANDAIC LETTER HALQA..MANDAIC LETTER AIN
unicode_alphabetic(0x08A0, 0x08A0).	% Alphabetic Lo       ARABIC LETTER BEH WITH SMALL V BELOW
unicode_alphabetic(0x08A2, 0x08AC).	% Alphabetic Lo  [11] ARABIC LETTER JEEM WITH TWO DOTS ABOVE..ARABIC LETTER ROHINGYA YEH
unicode_alphabetic(0x08E4, 0x08E9).	% Alphabetic Mn   [6] ARABIC CURLY FATHA..ARABIC CURLY KASRATAN
unicode_alphabetic(0x08F0, 0x08FE).	% Alphabetic Mn  [15] ARABIC OPEN FATHATAN..ARABIC DAMMA WITH DOT
unicode_alphabetic(0x0900, 0x0902).	% Alphabetic Mn   [3] DEVANAGARI SIGN INVERTED CANDRABINDU..DEVANAGARI SIGN ANUSVARA
unicode_alphabetic(0x0903, 0x0903).	% Alphabetic Mc       DEVANAGARI SIGN VISARGA
unicode_alphabetic(0x0904, 0x0939).	% Alphabetic Lo  [54] DEVANAGARI LETTER SHORT A..DEVANAGARI LETTER HA
unicode_alphabetic(0x093A, 0x093A).	% Alphabetic Mn       DEVANAGARI VOWEL SIGN OE
unicode_alphabetic(0x093B, 0x093B).	% Alphabetic Mc       DEVANAGARI VOWEL SIGN OOE
unicode_alphabetic(0x093D, 0x093D).	% Alphabetic Lo       DEVANAGARI SIGN AVAGRAHA
unicode_alphabetic(0x093E, 0x0940).	% Alphabetic Mc   [3] DEVANAGARI VOWEL SIGN AA..DEVANAGARI VOWEL SIGN II
unicode_alphabetic(0x0941, 0x0948).	% Alphabetic Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
unicode_alphabetic(0x0949, 0x094C).	% Alphabetic Mc   [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU
unicode_alphabetic(0x094E, 0x094F).	% Alphabetic Mc   [2] DEVANAGARI VOWEL SIGN PRISHTHAMATRA E..DEVANAGARI VOWEL SIGN AW
unicode_alphabetic(0x0950, 0x0950).	% Alphabetic Lo       DEVANAGARI OM
unicode_alphabetic(0x0955, 0x0957).	% Alphabetic Mn   [3] DEVANAGARI VOWEL SIGN CANDRA LONG E..DEVANAGARI VOWEL SIGN UUE
unicode_alphabetic(0x0958, 0x0961).	% Alphabetic Lo  [10] DEVANAGARI LETTER QA..DEVANAGARI LETTER VOCALIC LL
unicode_alphabetic(0x0962, 0x0963).	% Alphabetic Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
unicode_alphabetic(0x0971, 0x0971).	% Alphabetic Lm       DEVANAGARI SIGN HIGH SPACING DOT
unicode_alphabetic(0x0972, 0x0977).	% Alphabetic Lo   [6] DEVANAGARI LETTER CANDRA A..DEVANAGARI LETTER UUE
unicode_alphabetic(0x0979, 0x097F).	% Alphabetic Lo   [7] DEVANAGARI LETTER ZHA..DEVANAGARI LETTER BBA
unicode_alphabetic(0x0981, 0x0981).	% Alphabetic Mn       BENGALI SIGN CANDRABINDU
unicode_alphabetic(0x0982, 0x0983).	% Alphabetic Mc   [2] BENGALI SIGN ANUSVARA..BENGALI SIGN VISARGA
unicode_alphabetic(0x0985, 0x098C).	% Alphabetic Lo   [8] BENGALI LETTER A..BENGALI LETTER VOCALIC L
unicode_alphabetic(0x098F, 0x0990).	% Alphabetic Lo   [2] BENGALI LETTER E..BENGALI LETTER AI
unicode_alphabetic(0x0993, 0x09A8).	% Alphabetic Lo  [22] BENGALI LETTER O..BENGALI LETTER NA
unicode_alphabetic(0x09AA, 0x09B0).	% Alphabetic Lo   [7] BENGALI LETTER PA..BENGALI LETTER RA
unicode_alphabetic(0x09B2, 0x09B2).	% Alphabetic Lo       BENGALI LETTER LA
unicode_alphabetic(0x09B6, 0x09B9).	% Alphabetic Lo   [4] BENGALI LETTER SHA..BENGALI LETTER HA
unicode_alphabetic(0x09BD, 0x09BD).	% Alphabetic Lo       BENGALI SIGN AVAGRAHA
unicode_alphabetic(0x09BE, 0x09C0).	% Alphabetic Mc   [3] BENGALI VOWEL SIGN AA..BENGALI VOWEL SIGN II
unicode_alphabetic(0x09C1, 0x09C4).	% Alphabetic Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
unicode_alphabetic(0x09C7, 0x09C8).	% Alphabetic Mc   [2] BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI
unicode_alphabetic(0x09CB, 0x09CC).	% Alphabetic Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_alphabetic(0x09CE, 0x09CE).	% Alphabetic Lo       BENGALI LETTER KHANDA TA
unicode_alphabetic(0x09D7, 0x09D7).	% Alphabetic Mc       BENGALI AU LENGTH MARK
unicode_alphabetic(0x09DC, 0x09DD).	% Alphabetic Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_alphabetic(0x09DF, 0x09E1).	% Alphabetic Lo   [3] BENGALI LETTER YYA..BENGALI LETTER VOCALIC LL
unicode_alphabetic(0x09E2, 0x09E3).	% Alphabetic Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
unicode_alphabetic(0x09F0, 0x09F1).	% Alphabetic Lo   [2] BENGALI LETTER RA WITH MIDDLE DIAGONAL..BENGALI LETTER RA WITH LOWER DIAGONAL
unicode_alphabetic(0x0A01, 0x0A02).	% Alphabetic Mn   [2] GURMUKHI SIGN ADAK BINDI..GURMUKHI SIGN BINDI
unicode_alphabetic(0x0A03, 0x0A03).	% Alphabetic Mc       GURMUKHI SIGN VISARGA
unicode_alphabetic(0x0A05, 0x0A0A).	% Alphabetic Lo   [6] GURMUKHI LETTER A..GURMUKHI LETTER UU
unicode_alphabetic(0x0A0F, 0x0A10).	% Alphabetic Lo   [2] GURMUKHI LETTER EE..GURMUKHI LETTER AI
unicode_alphabetic(0x0A13, 0x0A28).	% Alphabetic Lo  [22] GURMUKHI LETTER OO..GURMUKHI LETTER NA
unicode_alphabetic(0x0A2A, 0x0A30).	% Alphabetic Lo   [7] GURMUKHI LETTER PA..GURMUKHI LETTER RA
unicode_alphabetic(0x0A32, 0x0A33).	% Alphabetic Lo   [2] GURMUKHI LETTER LA..GURMUKHI LETTER LLA
unicode_alphabetic(0x0A35, 0x0A36).	% Alphabetic Lo   [2] GURMUKHI LETTER VA..GURMUKHI LETTER SHA
unicode_alphabetic(0x0A38, 0x0A39).	% Alphabetic Lo   [2] GURMUKHI LETTER SA..GURMUKHI LETTER HA
unicode_alphabetic(0x0A3E, 0x0A40).	% Alphabetic Mc   [3] GURMUKHI VOWEL SIGN AA..GURMUKHI VOWEL SIGN II
unicode_alphabetic(0x0A41, 0x0A42).	% Alphabetic Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
unicode_alphabetic(0x0A47, 0x0A48).	% Alphabetic Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
unicode_alphabetic(0x0A4B, 0x0A4C).	% Alphabetic Mn   [2] GURMUKHI VOWEL SIGN OO..GURMUKHI VOWEL SIGN AU
unicode_alphabetic(0x0A51, 0x0A51).	% Alphabetic Mn       GURMUKHI SIGN UDAAT
unicode_alphabetic(0x0A59, 0x0A5C).	% Alphabetic Lo   [4] GURMUKHI LETTER KHHA..GURMUKHI LETTER RRA
unicode_alphabetic(0x0A5E, 0x0A5E).	% Alphabetic Lo       GURMUKHI LETTER FA
unicode_alphabetic(0x0A70, 0x0A71).	% Alphabetic Mn   [2] GURMUKHI TIPPI..GURMUKHI ADDAK
unicode_alphabetic(0x0A72, 0x0A74).	% Alphabetic Lo   [3] GURMUKHI IRI..GURMUKHI EK ONKAR
unicode_alphabetic(0x0A75, 0x0A75).	% Alphabetic Mn       GURMUKHI SIGN YAKASH
unicode_alphabetic(0x0A81, 0x0A82).	% Alphabetic Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
unicode_alphabetic(0x0A83, 0x0A83).	% Alphabetic Mc       GUJARATI SIGN VISARGA
unicode_alphabetic(0x0A85, 0x0A8D).	% Alphabetic Lo   [9] GUJARATI LETTER A..GUJARATI VOWEL CANDRA E
unicode_alphabetic(0x0A8F, 0x0A91).	% Alphabetic Lo   [3] GUJARATI LETTER E..GUJARATI VOWEL CANDRA O
unicode_alphabetic(0x0A93, 0x0AA8).	% Alphabetic Lo  [22] GUJARATI LETTER O..GUJARATI LETTER NA
unicode_alphabetic(0x0AAA, 0x0AB0).	% Alphabetic Lo   [7] GUJARATI LETTER PA..GUJARATI LETTER RA
unicode_alphabetic(0x0AB2, 0x0AB3).	% Alphabetic Lo   [2] GUJARATI LETTER LA..GUJARATI LETTER LLA
unicode_alphabetic(0x0AB5, 0x0AB9).	% Alphabetic Lo   [5] GUJARATI LETTER VA..GUJARATI LETTER HA
unicode_alphabetic(0x0ABD, 0x0ABD).	% Alphabetic Lo       GUJARATI SIGN AVAGRAHA
unicode_alphabetic(0x0ABE, 0x0AC0).	% Alphabetic Mc   [3] GUJARATI VOWEL SIGN AA..GUJARATI VOWEL SIGN II
unicode_alphabetic(0x0AC1, 0x0AC5).	% Alphabetic Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
unicode_alphabetic(0x0AC7, 0x0AC8).	% Alphabetic Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
unicode_alphabetic(0x0AC9, 0x0AC9).	% Alphabetic Mc       GUJARATI VOWEL SIGN CANDRA O
unicode_alphabetic(0x0ACB, 0x0ACC).	% Alphabetic Mc   [2] GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU
unicode_alphabetic(0x0AD0, 0x0AD0).	% Alphabetic Lo       GUJARATI OM
unicode_alphabetic(0x0AE0, 0x0AE1).	% Alphabetic Lo   [2] GUJARATI LETTER VOCALIC RR..GUJARATI LETTER VOCALIC LL
unicode_alphabetic(0x0AE2, 0x0AE3).	% Alphabetic Mn   [2] GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL
unicode_alphabetic(0x0B01, 0x0B01).	% Alphabetic Mn       ORIYA SIGN CANDRABINDU
unicode_alphabetic(0x0B02, 0x0B03).	% Alphabetic Mc   [2] ORIYA SIGN ANUSVARA..ORIYA SIGN VISARGA
unicode_alphabetic(0x0B05, 0x0B0C).	% Alphabetic Lo   [8] ORIYA LETTER A..ORIYA LETTER VOCALIC L
unicode_alphabetic(0x0B0F, 0x0B10).	% Alphabetic Lo   [2] ORIYA LETTER E..ORIYA LETTER AI
unicode_alphabetic(0x0B13, 0x0B28).	% Alphabetic Lo  [22] ORIYA LETTER O..ORIYA LETTER NA
unicode_alphabetic(0x0B2A, 0x0B30).	% Alphabetic Lo   [7] ORIYA LETTER PA..ORIYA LETTER RA
unicode_alphabetic(0x0B32, 0x0B33).	% Alphabetic Lo   [2] ORIYA LETTER LA..ORIYA LETTER LLA
unicode_alphabetic(0x0B35, 0x0B39).	% Alphabetic Lo   [5] ORIYA LETTER VA..ORIYA LETTER HA
unicode_alphabetic(0x0B3D, 0x0B3D).	% Alphabetic Lo       ORIYA SIGN AVAGRAHA
unicode_alphabetic(0x0B3E, 0x0B3E).	% Alphabetic Mc       ORIYA VOWEL SIGN AA
unicode_alphabetic(0x0B3F, 0x0B3F).	% Alphabetic Mn       ORIYA VOWEL SIGN I
unicode_alphabetic(0x0B40, 0x0B40).	% Alphabetic Mc       ORIYA VOWEL SIGN II
unicode_alphabetic(0x0B41, 0x0B44).	% Alphabetic Mn   [4] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR
unicode_alphabetic(0x0B47, 0x0B48).	% Alphabetic Mc   [2] ORIYA VOWEL SIGN E..ORIYA VOWEL SIGN AI
unicode_alphabetic(0x0B4B, 0x0B4C).	% Alphabetic Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
unicode_alphabetic(0x0B56, 0x0B56).	% Alphabetic Mn       ORIYA AI LENGTH MARK
unicode_alphabetic(0x0B57, 0x0B57).	% Alphabetic Mc       ORIYA AU LENGTH MARK
unicode_alphabetic(0x0B5C, 0x0B5D).	% Alphabetic Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_alphabetic(0x0B5F, 0x0B61).	% Alphabetic Lo   [3] ORIYA LETTER YYA..ORIYA LETTER VOCALIC LL
unicode_alphabetic(0x0B62, 0x0B63).	% Alphabetic Mn   [2] ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL
unicode_alphabetic(0x0B71, 0x0B71).	% Alphabetic Lo       ORIYA LETTER WA
unicode_alphabetic(0x0B82, 0x0B82).	% Alphabetic Mn       TAMIL SIGN ANUSVARA
unicode_alphabetic(0x0B83, 0x0B83).	% Alphabetic Lo       TAMIL SIGN VISARGA
unicode_alphabetic(0x0B85, 0x0B8A).	% Alphabetic Lo   [6] TAMIL LETTER A..TAMIL LETTER UU
unicode_alphabetic(0x0B8E, 0x0B90).	% Alphabetic Lo   [3] TAMIL LETTER E..TAMIL LETTER AI
unicode_alphabetic(0x0B92, 0x0B95).	% Alphabetic Lo   [4] TAMIL LETTER O..TAMIL LETTER KA
unicode_alphabetic(0x0B99, 0x0B9A).	% Alphabetic Lo   [2] TAMIL LETTER NGA..TAMIL LETTER CA
unicode_alphabetic(0x0B9C, 0x0B9C).	% Alphabetic Lo       TAMIL LETTER JA
unicode_alphabetic(0x0B9E, 0x0B9F).	% Alphabetic Lo   [2] TAMIL LETTER NYA..TAMIL LETTER TTA
unicode_alphabetic(0x0BA3, 0x0BA4).	% Alphabetic Lo   [2] TAMIL LETTER NNA..TAMIL LETTER TA
unicode_alphabetic(0x0BA8, 0x0BAA).	% Alphabetic Lo   [3] TAMIL LETTER NA..TAMIL LETTER PA
unicode_alphabetic(0x0BAE, 0x0BB9).	% Alphabetic Lo  [12] TAMIL LETTER MA..TAMIL LETTER HA
unicode_alphabetic(0x0BBE, 0x0BBF).	% Alphabetic Mc   [2] TAMIL VOWEL SIGN AA..TAMIL VOWEL SIGN I
unicode_alphabetic(0x0BC0, 0x0BC0).	% Alphabetic Mn       TAMIL VOWEL SIGN II
unicode_alphabetic(0x0BC1, 0x0BC2).	% Alphabetic Mc   [2] TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU
unicode_alphabetic(0x0BC6, 0x0BC8).	% Alphabetic Mc   [3] TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI
unicode_alphabetic(0x0BCA, 0x0BCC).	% Alphabetic Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_alphabetic(0x0BD0, 0x0BD0).	% Alphabetic Lo       TAMIL OM
unicode_alphabetic(0x0BD7, 0x0BD7).	% Alphabetic Mc       TAMIL AU LENGTH MARK
unicode_alphabetic(0x0C01, 0x0C03).	% Alphabetic Mc   [3] TELUGU SIGN CANDRABINDU..TELUGU SIGN VISARGA
unicode_alphabetic(0x0C05, 0x0C0C).	% Alphabetic Lo   [8] TELUGU LETTER A..TELUGU LETTER VOCALIC L
unicode_alphabetic(0x0C0E, 0x0C10).	% Alphabetic Lo   [3] TELUGU LETTER E..TELUGU LETTER AI
unicode_alphabetic(0x0C12, 0x0C28).	% Alphabetic Lo  [23] TELUGU LETTER O..TELUGU LETTER NA
unicode_alphabetic(0x0C2A, 0x0C33).	% Alphabetic Lo  [10] TELUGU LETTER PA..TELUGU LETTER LLA
unicode_alphabetic(0x0C35, 0x0C39).	% Alphabetic Lo   [5] TELUGU LETTER VA..TELUGU LETTER HA
unicode_alphabetic(0x0C3D, 0x0C3D).	% Alphabetic Lo       TELUGU SIGN AVAGRAHA
unicode_alphabetic(0x0C3E, 0x0C40).	% Alphabetic Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
unicode_alphabetic(0x0C41, 0x0C44).	% Alphabetic Mc   [4] TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR
unicode_alphabetic(0x0C46, 0x0C48).	% Alphabetic Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
unicode_alphabetic(0x0C4A, 0x0C4C).	% Alphabetic Mn   [3] TELUGU VOWEL SIGN O..TELUGU VOWEL SIGN AU
unicode_alphabetic(0x0C55, 0x0C56).	% Alphabetic Mn   [2] TELUGU LENGTH MARK..TELUGU AI LENGTH MARK
unicode_alphabetic(0x0C58, 0x0C59).	% Alphabetic Lo   [2] TELUGU LETTER TSA..TELUGU LETTER DZA
unicode_alphabetic(0x0C60, 0x0C61).	% Alphabetic Lo   [2] TELUGU LETTER VOCALIC RR..TELUGU LETTER VOCALIC LL
unicode_alphabetic(0x0C62, 0x0C63).	% Alphabetic Mn   [2] TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL
unicode_alphabetic(0x0C82, 0x0C83).	% Alphabetic Mc   [2] KANNADA SIGN ANUSVARA..KANNADA SIGN VISARGA
unicode_alphabetic(0x0C85, 0x0C8C).	% Alphabetic Lo   [8] KANNADA LETTER A..KANNADA LETTER VOCALIC L
unicode_alphabetic(0x0C8E, 0x0C90).	% Alphabetic Lo   [3] KANNADA LETTER E..KANNADA LETTER AI
unicode_alphabetic(0x0C92, 0x0CA8).	% Alphabetic Lo  [23] KANNADA LETTER O..KANNADA LETTER NA
unicode_alphabetic(0x0CAA, 0x0CB3).	% Alphabetic Lo  [10] KANNADA LETTER PA..KANNADA LETTER LLA
unicode_alphabetic(0x0CB5, 0x0CB9).	% Alphabetic Lo   [5] KANNADA LETTER VA..KANNADA LETTER HA
unicode_alphabetic(0x0CBD, 0x0CBD).	% Alphabetic Lo       KANNADA SIGN AVAGRAHA
unicode_alphabetic(0x0CBE, 0x0CBE).	% Alphabetic Mc       KANNADA VOWEL SIGN AA
unicode_alphabetic(0x0CBF, 0x0CBF).	% Alphabetic Mn       KANNADA VOWEL SIGN I
unicode_alphabetic(0x0CC0, 0x0CC4).	% Alphabetic Mc   [5] KANNADA VOWEL SIGN II..KANNADA VOWEL SIGN VOCALIC RR
unicode_alphabetic(0x0CC6, 0x0CC6).	% Alphabetic Mn       KANNADA VOWEL SIGN E
unicode_alphabetic(0x0CC7, 0x0CC8).	% Alphabetic Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_alphabetic(0x0CCA, 0x0CCB).	% Alphabetic Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_alphabetic(0x0CCC, 0x0CCC).	% Alphabetic Mn       KANNADA VOWEL SIGN AU
unicode_alphabetic(0x0CD5, 0x0CD6).	% Alphabetic Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
unicode_alphabetic(0x0CDE, 0x0CDE).	% Alphabetic Lo       KANNADA LETTER FA
unicode_alphabetic(0x0CE0, 0x0CE1).	% Alphabetic Lo   [2] KANNADA LETTER VOCALIC RR..KANNADA LETTER VOCALIC LL
unicode_alphabetic(0x0CE2, 0x0CE3).	% Alphabetic Mn   [2] KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL
unicode_alphabetic(0x0CF1, 0x0CF2).	% Alphabetic Lo   [2] KANNADA SIGN JIHVAMULIYA..KANNADA SIGN UPADHMANIYA
unicode_alphabetic(0x0D02, 0x0D03).	% Alphabetic Mc   [2] MALAYALAM SIGN ANUSVARA..MALAYALAM SIGN VISARGA
unicode_alphabetic(0x0D05, 0x0D0C).	% Alphabetic Lo   [8] MALAYALAM LETTER A..MALAYALAM LETTER VOCALIC L
unicode_alphabetic(0x0D0E, 0x0D10).	% Alphabetic Lo   [3] MALAYALAM LETTER E..MALAYALAM LETTER AI
unicode_alphabetic(0x0D12, 0x0D3A).	% Alphabetic Lo  [41] MALAYALAM LETTER O..MALAYALAM LETTER TTTA
unicode_alphabetic(0x0D3D, 0x0D3D).	% Alphabetic Lo       MALAYALAM SIGN AVAGRAHA
unicode_alphabetic(0x0D3E, 0x0D40).	% Alphabetic Mc   [3] MALAYALAM VOWEL SIGN AA..MALAYALAM VOWEL SIGN II
unicode_alphabetic(0x0D41, 0x0D44).	% Alphabetic Mn   [4] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC RR
unicode_alphabetic(0x0D46, 0x0D48).	% Alphabetic Mc   [3] MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI
unicode_alphabetic(0x0D4A, 0x0D4C).	% Alphabetic Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_alphabetic(0x0D4E, 0x0D4E).	% Alphabetic Lo       MALAYALAM LETTER DOT REPH
unicode_alphabetic(0x0D57, 0x0D57).	% Alphabetic Mc       MALAYALAM AU LENGTH MARK
unicode_alphabetic(0x0D60, 0x0D61).	% Alphabetic Lo   [2] MALAYALAM LETTER VOCALIC RR..MALAYALAM LETTER VOCALIC LL
unicode_alphabetic(0x0D62, 0x0D63).	% Alphabetic Mn   [2] MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL
unicode_alphabetic(0x0D7A, 0x0D7F).	% Alphabetic Lo   [6] MALAYALAM LETTER CHILLU NN..MALAYALAM LETTER CHILLU K
unicode_alphabetic(0x0D82, 0x0D83).	% Alphabetic Mc   [2] SINHALA SIGN ANUSVARAYA..SINHALA SIGN VISARGAYA
unicode_alphabetic(0x0D85, 0x0D96).	% Alphabetic Lo  [18] SINHALA LETTER AYANNA..SINHALA LETTER AUYANNA
unicode_alphabetic(0x0D9A, 0x0DB1).	% Alphabetic Lo  [24] SINHALA LETTER ALPAPRAANA KAYANNA..SINHALA LETTER DANTAJA NAYANNA
unicode_alphabetic(0x0DB3, 0x0DBB).	% Alphabetic Lo   [9] SINHALA LETTER SANYAKA DAYANNA..SINHALA LETTER RAYANNA
unicode_alphabetic(0x0DBD, 0x0DBD).	% Alphabetic Lo       SINHALA LETTER DANTAJA LAYANNA
unicode_alphabetic(0x0DC0, 0x0DC6).	% Alphabetic Lo   [7] SINHALA LETTER VAYANNA..SINHALA LETTER FAYANNA
unicode_alphabetic(0x0DCF, 0x0DD1).	% Alphabetic Mc   [3] SINHALA VOWEL SIGN AELA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA
unicode_alphabetic(0x0DD2, 0x0DD4).	% Alphabetic Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
unicode_alphabetic(0x0DD6, 0x0DD6).	% Alphabetic Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
unicode_alphabetic(0x0DD8, 0x0DDF).	% Alphabetic Mc   [8] SINHALA VOWEL SIGN GAETTA-PILLA..SINHALA VOWEL SIGN GAYANUKITTA
unicode_alphabetic(0x0DF2, 0x0DF3).	% Alphabetic Mc   [2] SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA
unicode_alphabetic(0x0E01, 0x0E30).	% Alphabetic Lo  [48] THAI CHARACTER KO KAI..THAI CHARACTER SARA A
unicode_alphabetic(0x0E31, 0x0E31).	% Alphabetic Mn       THAI CHARACTER MAI HAN-AKAT
unicode_alphabetic(0x0E32, 0x0E33).	% Alphabetic Lo   [2] THAI CHARACTER SARA AA..THAI CHARACTER SARA AM
unicode_alphabetic(0x0E34, 0x0E3A).	% Alphabetic Mn   [7] THAI CHARACTER SARA I..THAI CHARACTER PHINTHU
unicode_alphabetic(0x0E40, 0x0E45).	% Alphabetic Lo   [6] THAI CHARACTER SARA E..THAI CHARACTER LAKKHANGYAO
unicode_alphabetic(0x0E46, 0x0E46).	% Alphabetic Lm       THAI CHARACTER MAIYAMOK
unicode_alphabetic(0x0E4D, 0x0E4D).	% Alphabetic Mn       THAI CHARACTER NIKHAHIT
unicode_alphabetic(0x0E81, 0x0E82).	% Alphabetic Lo   [2] LAO LETTER KO..LAO LETTER KHO SUNG
unicode_alphabetic(0x0E84, 0x0E84).	% Alphabetic Lo       LAO LETTER KHO TAM
unicode_alphabetic(0x0E87, 0x0E88).	% Alphabetic Lo   [2] LAO LETTER NGO..LAO LETTER CO
unicode_alphabetic(0x0E8A, 0x0E8A).	% Alphabetic Lo       LAO LETTER SO TAM
unicode_alphabetic(0x0E8D, 0x0E8D).	% Alphabetic Lo       LAO LETTER NYO
unicode_alphabetic(0x0E94, 0x0E97).	% Alphabetic Lo   [4] LAO LETTER DO..LAO LETTER THO TAM
unicode_alphabetic(0x0E99, 0x0E9F).	% Alphabetic Lo   [7] LAO LETTER NO..LAO LETTER FO SUNG
unicode_alphabetic(0x0EA1, 0x0EA3).	% Alphabetic Lo   [3] LAO LETTER MO..LAO LETTER LO LING
unicode_alphabetic(0x0EA5, 0x0EA5).	% Alphabetic Lo       LAO LETTER LO LOOT
unicode_alphabetic(0x0EA7, 0x0EA7).	% Alphabetic Lo       LAO LETTER WO
unicode_alphabetic(0x0EAA, 0x0EAB).	% Alphabetic Lo   [2] LAO LETTER SO SUNG..LAO LETTER HO SUNG
unicode_alphabetic(0x0EAD, 0x0EB0).	% Alphabetic Lo   [4] LAO LETTER O..LAO VOWEL SIGN A
unicode_alphabetic(0x0EB1, 0x0EB1).	% Alphabetic Mn       LAO VOWEL SIGN MAI KAN
unicode_alphabetic(0x0EB2, 0x0EB3).	% Alphabetic Lo   [2] LAO VOWEL SIGN AA..LAO VOWEL SIGN AM
unicode_alphabetic(0x0EB4, 0x0EB9).	% Alphabetic Mn   [6] LAO VOWEL SIGN I..LAO VOWEL SIGN UU
unicode_alphabetic(0x0EBB, 0x0EBC).	% Alphabetic Mn   [2] LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO
unicode_alphabetic(0x0EBD, 0x0EBD).	% Alphabetic Lo       LAO SEMIVOWEL SIGN NYO
unicode_alphabetic(0x0EC0, 0x0EC4).	% Alphabetic Lo   [5] LAO VOWEL SIGN E..LAO VOWEL SIGN AI
unicode_alphabetic(0x0EC6, 0x0EC6).	% Alphabetic Lm       LAO KO LA
unicode_alphabetic(0x0ECD, 0x0ECD).	% Alphabetic Mn       LAO NIGGAHITA
unicode_alphabetic(0x0EDC, 0x0EDF).	% Alphabetic Lo   [4] LAO HO NO..LAO LETTER KHMU NYO
unicode_alphabetic(0x0F00, 0x0F00).	% Alphabetic Lo       TIBETAN SYLLABLE OM
unicode_alphabetic(0x0F40, 0x0F47).	% Alphabetic Lo   [8] TIBETAN LETTER KA..TIBETAN LETTER JA
unicode_alphabetic(0x0F49, 0x0F6C).	% Alphabetic Lo  [36] TIBETAN LETTER NYA..TIBETAN LETTER RRA
unicode_alphabetic(0x0F71, 0x0F7E).	% Alphabetic Mn  [14] TIBETAN VOWEL SIGN AA..TIBETAN SIGN RJES SU NGA RO
unicode_alphabetic(0x0F7F, 0x0F7F).	% Alphabetic Mc       TIBETAN SIGN RNAM BCAD
unicode_alphabetic(0x0F80, 0x0F81).	% Alphabetic Mn   [2] TIBETAN VOWEL SIGN REVERSED I..TIBETAN VOWEL SIGN REVERSED II
unicode_alphabetic(0x0F88, 0x0F8C).	% Alphabetic Lo   [5] TIBETAN SIGN LCE TSA CAN..TIBETAN SIGN INVERTED MCHU CAN
unicode_alphabetic(0x0F8D, 0x0F97).	% Alphabetic Mn  [11] TIBETAN SUBJOINED SIGN LCE TSA CAN..TIBETAN SUBJOINED LETTER JA
unicode_alphabetic(0x0F99, 0x0FBC).	% Alphabetic Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
unicode_alphabetic(0x1000, 0x102A).	% Alphabetic Lo  [43] MYANMAR LETTER KA..MYANMAR LETTER AU
unicode_alphabetic(0x102B, 0x102C).	% Alphabetic Mc   [2] MYANMAR VOWEL SIGN TALL AA..MYANMAR VOWEL SIGN AA
unicode_alphabetic(0x102D, 0x1030).	% Alphabetic Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
unicode_alphabetic(0x1031, 0x1031).	% Alphabetic Mc       MYANMAR VOWEL SIGN E
unicode_alphabetic(0x1032, 0x1036).	% Alphabetic Mn   [5] MYANMAR VOWEL SIGN AI..MYANMAR SIGN ANUSVARA
unicode_alphabetic(0x1038, 0x1038).	% Alphabetic Mc       MYANMAR SIGN VISARGA
unicode_alphabetic(0x103B, 0x103C).	% Alphabetic Mc   [2] MYANMAR CONSONANT SIGN MEDIAL YA..MYANMAR CONSONANT SIGN MEDIAL RA
unicode_alphabetic(0x103D, 0x103E).	% Alphabetic Mn   [2] MYANMAR CONSONANT SIGN MEDIAL WA..MYANMAR CONSONANT SIGN MEDIAL HA
unicode_alphabetic(0x103F, 0x103F).	% Alphabetic Lo       MYANMAR LETTER GREAT SA
unicode_alphabetic(0x1050, 0x1055).	% Alphabetic Lo   [6] MYANMAR LETTER SHA..MYANMAR LETTER VOCALIC LL
unicode_alphabetic(0x1056, 0x1057).	% Alphabetic Mc   [2] MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR
unicode_alphabetic(0x1058, 0x1059).	% Alphabetic Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
unicode_alphabetic(0x105A, 0x105D).	% Alphabetic Lo   [4] MYANMAR LETTER MON NGA..MYANMAR LETTER MON BBE
unicode_alphabetic(0x105E, 0x1060).	% Alphabetic Mn   [3] MYANMAR CONSONANT SIGN MON MEDIAL NA..MYANMAR CONSONANT SIGN MON MEDIAL LA
unicode_alphabetic(0x1061, 0x1061).	% Alphabetic Lo       MYANMAR LETTER SGAW KAREN SHA
unicode_alphabetic(0x1062, 0x1062).	% Alphabetic Mc       MYANMAR VOWEL SIGN SGAW KAREN EU
unicode_alphabetic(0x1065, 0x1066).	% Alphabetic Lo   [2] MYANMAR LETTER WESTERN PWO KAREN THA..MYANMAR LETTER WESTERN PWO KAREN PWA
unicode_alphabetic(0x1067, 0x1068).	% Alphabetic Mc   [2] MYANMAR VOWEL SIGN WESTERN PWO KAREN EU..MYANMAR VOWEL SIGN WESTERN PWO KAREN UE
unicode_alphabetic(0x106E, 0x1070).	% Alphabetic Lo   [3] MYANMAR LETTER EASTERN PWO KAREN NNA..MYANMAR LETTER EASTERN PWO KAREN GHWA
unicode_alphabetic(0x1071, 0x1074).	% Alphabetic Mn   [4] MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE
unicode_alphabetic(0x1075, 0x1081).	% Alphabetic Lo  [13] MYANMAR LETTER SHAN KA..MYANMAR LETTER SHAN HA
unicode_alphabetic(0x1082, 0x1082).	% Alphabetic Mn       MYANMAR CONSONANT SIGN SHAN MEDIAL WA
unicode_alphabetic(0x1083, 0x1084).	% Alphabetic Mc   [2] MYANMAR VOWEL SIGN SHAN AA..MYANMAR VOWEL SIGN SHAN E
unicode_alphabetic(0x1085, 0x1086).	% Alphabetic Mn   [2] MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y
unicode_alphabetic(0x108E, 0x108E).	% Alphabetic Lo       MYANMAR LETTER RUMAI PALAUNG FA
unicode_alphabetic(0x109C, 0x109C).	% Alphabetic Mc       MYANMAR VOWEL SIGN AITON A
unicode_alphabetic(0x109D, 0x109D).	% Alphabetic Mn       MYANMAR VOWEL SIGN AITON AI
unicode_alphabetic(0x10A0, 0x10C5).	% Alphabetic L&  [38] GEORGIAN CAPITAL LETTER AN..GEORGIAN CAPITAL LETTER HOE
unicode_alphabetic(0x10C7, 0x10C7).	% Alphabetic L&       GEORGIAN CAPITAL LETTER YN
unicode_alphabetic(0x10CD, 0x10CD).	% Alphabetic L&       GEORGIAN CAPITAL LETTER AEN
unicode_alphabetic(0x10D0, 0x10FA).	% Alphabetic Lo  [43] GEORGIAN LETTER AN..GEORGIAN LETTER AIN
unicode_alphabetic(0x10FC, 0x10FC).	% Alphabetic Lm       MODIFIER LETTER GEORGIAN NAR
unicode_alphabetic(0x10FD, 0x1248).	% Alphabetic Lo [332] GEORGIAN LETTER AEN..ETHIOPIC SYLLABLE QWA
unicode_alphabetic(0x124A, 0x124D).	% Alphabetic Lo   [4] ETHIOPIC SYLLABLE QWI..ETHIOPIC SYLLABLE QWE
unicode_alphabetic(0x1250, 0x1256).	% Alphabetic Lo   [7] ETHIOPIC SYLLABLE QHA..ETHIOPIC SYLLABLE QHO
unicode_alphabetic(0x1258, 0x1258).	% Alphabetic Lo       ETHIOPIC SYLLABLE QHWA
unicode_alphabetic(0x125A, 0x125D).	% Alphabetic Lo   [4] ETHIOPIC SYLLABLE QHWI..ETHIOPIC SYLLABLE QHWE
unicode_alphabetic(0x1260, 0x1288).	% Alphabetic Lo  [41] ETHIOPIC SYLLABLE BA..ETHIOPIC SYLLABLE XWA
unicode_alphabetic(0x128A, 0x128D).	% Alphabetic Lo   [4] ETHIOPIC SYLLABLE XWI..ETHIOPIC SYLLABLE XWE
unicode_alphabetic(0x1290, 0x12B0).	% Alphabetic Lo  [33] ETHIOPIC SYLLABLE NA..ETHIOPIC SYLLABLE KWA
unicode_alphabetic(0x12B2, 0x12B5).	% Alphabetic Lo   [4] ETHIOPIC SYLLABLE KWI..ETHIOPIC SYLLABLE KWE
unicode_alphabetic(0x12B8, 0x12BE).	% Alphabetic Lo   [7] ETHIOPIC SYLLABLE KXA..ETHIOPIC SYLLABLE KXO
unicode_alphabetic(0x12C0, 0x12C0).	% Alphabetic Lo       ETHIOPIC SYLLABLE KXWA
unicode_alphabetic(0x12C2, 0x12C5).	% Alphabetic Lo   [4] ETHIOPIC SYLLABLE KXWI..ETHIOPIC SYLLABLE KXWE
unicode_alphabetic(0x12C8, 0x12D6).	% Alphabetic Lo  [15] ETHIOPIC SYLLABLE WA..ETHIOPIC SYLLABLE PHARYNGEAL O
unicode_alphabetic(0x12D8, 0x1310).	% Alphabetic Lo  [57] ETHIOPIC SYLLABLE ZA..ETHIOPIC SYLLABLE GWA
unicode_alphabetic(0x1312, 0x1315).	% Alphabetic Lo   [4] ETHIOPIC SYLLABLE GWI..ETHIOPIC SYLLABLE GWE
unicode_alphabetic(0x1318, 0x135A).	% Alphabetic Lo  [67] ETHIOPIC SYLLABLE GGA..ETHIOPIC SYLLABLE FYA
unicode_alphabetic(0x135F, 0x135F).	% Alphabetic Mn       ETHIOPIC COMBINING GEMINATION MARK
unicode_alphabetic(0x1380, 0x138F).	% Alphabetic Lo  [16] ETHIOPIC SYLLABLE SEBATBEIT MWA..ETHIOPIC SYLLABLE PWE
unicode_alphabetic(0x13A0, 0x13F4).	% Alphabetic Lo  [85] CHEROKEE LETTER A..CHEROKEE LETTER YV
unicode_alphabetic(0x1401, 0x166C).	% Alphabetic Lo [620] CANADIAN SYLLABICS E..CANADIAN SYLLABICS CARRIER TTSA
unicode_alphabetic(0x166F, 0x167F).	% Alphabetic Lo  [17] CANADIAN SYLLABICS QAI..CANADIAN SYLLABICS BLACKFOOT W
unicode_alphabetic(0x1681, 0x169A).	% Alphabetic Lo  [26] OGHAM LETTER BEITH..OGHAM LETTER PEITH
unicode_alphabetic(0x16A0, 0x16EA).	% Alphabetic Lo  [75] RUNIC LETTER FEHU FEOH FE F..RUNIC LETTER X
unicode_alphabetic(0x16EE, 0x16F0).	% Alphabetic Nl   [3] RUNIC ARLAUG SYMBOL..RUNIC BELGTHOR SYMBOL
unicode_alphabetic(0x1700, 0x170C).	% Alphabetic Lo  [13] TAGALOG LETTER A..TAGALOG LETTER YA
unicode_alphabetic(0x170E, 0x1711).	% Alphabetic Lo   [4] TAGALOG LETTER LA..TAGALOG LETTER HA
unicode_alphabetic(0x1712, 0x1713).	% Alphabetic Mn   [2] TAGALOG VOWEL SIGN I..TAGALOG VOWEL SIGN U
unicode_alphabetic(0x1720, 0x1731).	% Alphabetic Lo  [18] HANUNOO LETTER A..HANUNOO LETTER HA
unicode_alphabetic(0x1732, 0x1733).	% Alphabetic Mn   [2] HANUNOO VOWEL SIGN I..HANUNOO VOWEL SIGN U
unicode_alphabetic(0x1740, 0x1751).	% Alphabetic Lo  [18] BUHID LETTER A..BUHID LETTER HA
unicode_alphabetic(0x1752, 0x1753).	% Alphabetic Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
unicode_alphabetic(0x1760, 0x176C).	% Alphabetic Lo  [13] TAGBANWA LETTER A..TAGBANWA LETTER YA
unicode_alphabetic(0x176E, 0x1770).	% Alphabetic Lo   [3] TAGBANWA LETTER LA..TAGBANWA LETTER SA
unicode_alphabetic(0x1772, 0x1773).	% Alphabetic Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
unicode_alphabetic(0x1780, 0x17B3).	% Alphabetic Lo  [52] KHMER LETTER KA..KHMER INDEPENDENT VOWEL QAU
unicode_alphabetic(0x17B6, 0x17B6).	% Alphabetic Mc       KHMER VOWEL SIGN AA
unicode_alphabetic(0x17B7, 0x17BD).	% Alphabetic Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
unicode_alphabetic(0x17BE, 0x17C5).	% Alphabetic Mc   [8] KHMER VOWEL SIGN OE..KHMER VOWEL SIGN AU
unicode_alphabetic(0x17C6, 0x17C6).	% Alphabetic Mn       KHMER SIGN NIKAHIT
unicode_alphabetic(0x17C7, 0x17C8).	% Alphabetic Mc   [2] KHMER SIGN REAHMUK..KHMER SIGN YUUKALEAPINTU
unicode_alphabetic(0x17D7, 0x17D7).	% Alphabetic Lm       KHMER SIGN LEK TOO
unicode_alphabetic(0x17DC, 0x17DC).	% Alphabetic Lo       KHMER SIGN AVAKRAHASANYA
unicode_alphabetic(0x1820, 0x1842).	% Alphabetic Lo  [35] MONGOLIAN LETTER A..MONGOLIAN LETTER CHI
unicode_alphabetic(0x1843, 0x1843).	% Alphabetic Lm       MONGOLIAN LETTER TODO LONG VOWEL SIGN
unicode_alphabetic(0x1844, 0x1877).	% Alphabetic Lo  [52] MONGOLIAN LETTER TODO E..MONGOLIAN LETTER MANCHU ZHA
unicode_alphabetic(0x1880, 0x18A8).	% Alphabetic Lo  [41] MONGOLIAN LETTER ALI GALI ANUSVARA ONE..MONGOLIAN LETTER MANCHU ALI GALI BHA
unicode_alphabetic(0x18A9, 0x18A9).	% Alphabetic Mn       MONGOLIAN LETTER ALI GALI DAGALGA
unicode_alphabetic(0x18AA, 0x18AA).	% Alphabetic Lo       MONGOLIAN LETTER MANCHU ALI GALI LHA
unicode_alphabetic(0x18B0, 0x18F5).	% Alphabetic Lo  [70] CANADIAN SYLLABICS OY..CANADIAN SYLLABICS CARRIER DENTAL S
unicode_alphabetic(0x1900, 0x191C).	% Alphabetic Lo  [29] LIMBU VOWEL-CARRIER LETTER..LIMBU LETTER HA
unicode_alphabetic(0x1920, 0x1922).	% Alphabetic Mn   [3] LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN U
unicode_alphabetic(0x1923, 0x1926).	% Alphabetic Mc   [4] LIMBU VOWEL SIGN EE..LIMBU VOWEL SIGN AU
unicode_alphabetic(0x1927, 0x1928).	% Alphabetic Mn   [2] LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O
unicode_alphabetic(0x1929, 0x192B).	% Alphabetic Mc   [3] LIMBU SUBJOINED LETTER YA..LIMBU SUBJOINED LETTER WA
unicode_alphabetic(0x1930, 0x1931).	% Alphabetic Mc   [2] LIMBU SMALL LETTER KA..LIMBU SMALL LETTER NGA
unicode_alphabetic(0x1932, 0x1932).	% Alphabetic Mn       LIMBU SMALL LETTER ANUSVARA
unicode_alphabetic(0x1933, 0x1938).	% Alphabetic Mc   [6] LIMBU SMALL LETTER TA..LIMBU SMALL LETTER LA
unicode_alphabetic(0x1950, 0x196D).	% Alphabetic Lo  [30] TAI LE LETTER KA..TAI LE LETTER AI
unicode_alphabetic(0x1970, 0x1974).	% Alphabetic Lo   [5] TAI LE LETTER TONE-2..TAI LE LETTER TONE-6
unicode_alphabetic(0x1980, 0x19AB).	% Alphabetic Lo  [44] NEW TAI LUE LETTER HIGH QA..NEW TAI LUE LETTER LOW SUA
unicode_alphabetic(0x19B0, 0x19C0).	% Alphabetic Mc  [17] NEW TAI LUE VOWEL SIGN VOWEL SHORTENER..NEW TAI LUE VOWEL SIGN IY
unicode_alphabetic(0x19C1, 0x19C7).	% Alphabetic Lo   [7] NEW TAI LUE LETTER FINAL V..NEW TAI LUE LETTER FINAL B
unicode_alphabetic(0x19C8, 0x19C9).	% Alphabetic Mc   [2] NEW TAI LUE TONE MARK-1..NEW TAI LUE TONE MARK-2
unicode_alphabetic(0x1A00, 0x1A16).	% Alphabetic Lo  [23] BUGINESE LETTER KA..BUGINESE LETTER HA
unicode_alphabetic(0x1A17, 0x1A18).	% Alphabetic Mn   [2] BUGINESE VOWEL SIGN I..BUGINESE VOWEL SIGN U
unicode_alphabetic(0x1A19, 0x1A1B).	% Alphabetic Mc   [3] BUGINESE VOWEL SIGN E..BUGINESE VOWEL SIGN AE
unicode_alphabetic(0x1A20, 0x1A54).	% Alphabetic Lo  [53] TAI THAM LETTER HIGH KA..TAI THAM LETTER GREAT SA
unicode_alphabetic(0x1A55, 0x1A55).	% Alphabetic Mc       TAI THAM CONSONANT SIGN MEDIAL RA
unicode_alphabetic(0x1A56, 0x1A56).	% Alphabetic Mn       TAI THAM CONSONANT SIGN MEDIAL LA
unicode_alphabetic(0x1A57, 0x1A57).	% Alphabetic Mc       TAI THAM CONSONANT SIGN LA TANG LAI
unicode_alphabetic(0x1A58, 0x1A5E).	% Alphabetic Mn   [7] TAI THAM SIGN MAI KANG LAI..TAI THAM CONSONANT SIGN SA
unicode_alphabetic(0x1A61, 0x1A61).	% Alphabetic Mc       TAI THAM VOWEL SIGN A
unicode_alphabetic(0x1A62, 0x1A62).	% Alphabetic Mn       TAI THAM VOWEL SIGN MAI SAT
unicode_alphabetic(0x1A63, 0x1A64).	% Alphabetic Mc   [2] TAI THAM VOWEL SIGN AA..TAI THAM VOWEL SIGN TALL AA
unicode_alphabetic(0x1A65, 0x1A6C).	% Alphabetic Mn   [8] TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN OA BELOW
unicode_alphabetic(0x1A6D, 0x1A72).	% Alphabetic Mc   [6] TAI THAM VOWEL SIGN OY..TAI THAM VOWEL SIGN THAM AI
unicode_alphabetic(0x1A73, 0x1A74).	% Alphabetic Mn   [2] TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN MAI KANG
unicode_alphabetic(0x1AA7, 0x1AA7).	% Alphabetic Lm       TAI THAM SIGN MAI YAMOK
unicode_alphabetic(0x1B00, 0x1B03).	% Alphabetic Mn   [4] BALINESE SIGN ULU RICEM..BALINESE SIGN SURANG
unicode_alphabetic(0x1B04, 0x1B04).	% Alphabetic Mc       BALINESE SIGN BISAH
unicode_alphabetic(0x1B05, 0x1B33).	% Alphabetic Lo  [47] BALINESE LETTER AKARA..BALINESE LETTER HA
unicode_alphabetic(0x1B35, 0x1B35).	% Alphabetic Mc       BALINESE VOWEL SIGN TEDUNG
unicode_alphabetic(0x1B36, 0x1B3A).	% Alphabetic Mn   [5] BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN RA REPA
unicode_alphabetic(0x1B3B, 0x1B3B).	% Alphabetic Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_alphabetic(0x1B3C, 0x1B3C).	% Alphabetic Mn       BALINESE VOWEL SIGN LA LENGA
unicode_alphabetic(0x1B3D, 0x1B41).	% Alphabetic Mc   [5] BALINESE VOWEL SIGN LA LENGA TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG
unicode_alphabetic(0x1B42, 0x1B42).	% Alphabetic Mn       BALINESE VOWEL SIGN PEPET
unicode_alphabetic(0x1B43, 0x1B43).	% Alphabetic Mc       BALINESE VOWEL SIGN PEPET TEDUNG
unicode_alphabetic(0x1B45, 0x1B4B).	% Alphabetic Lo   [7] BALINESE LETTER KAF SASAK..BALINESE LETTER ASYURA SASAK
unicode_alphabetic(0x1B80, 0x1B81).	% Alphabetic Mn   [2] SUNDANESE SIGN PANYECEK..SUNDANESE SIGN PANGLAYAR
unicode_alphabetic(0x1B82, 0x1B82).	% Alphabetic Mc       SUNDANESE SIGN PANGWISAD
unicode_alphabetic(0x1B83, 0x1BA0).	% Alphabetic Lo  [30] SUNDANESE LETTER A..SUNDANESE LETTER HA
unicode_alphabetic(0x1BA1, 0x1BA1).	% Alphabetic Mc       SUNDANESE CONSONANT SIGN PAMINGKAL
unicode_alphabetic(0x1BA2, 0x1BA5).	% Alphabetic Mn   [4] SUNDANESE CONSONANT SIGN PANYAKRA..SUNDANESE VOWEL SIGN PANYUKU
unicode_alphabetic(0x1BA6, 0x1BA7).	% Alphabetic Mc   [2] SUNDANESE VOWEL SIGN PANAELAENG..SUNDANESE VOWEL SIGN PANOLONG
unicode_alphabetic(0x1BA8, 0x1BA9).	% Alphabetic Mn   [2] SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG
unicode_alphabetic(0x1BAC, 0x1BAD).	% Alphabetic Mc   [2] SUNDANESE CONSONANT SIGN PASANGAN MA..SUNDANESE CONSONANT SIGN PASANGAN WA
unicode_alphabetic(0x1BAE, 0x1BAF).	% Alphabetic Lo   [2] SUNDANESE LETTER KHA..SUNDANESE LETTER SYA
unicode_alphabetic(0x1BBA, 0x1BE5).	% Alphabetic Lo  [44] SUNDANESE AVAGRAHA..BATAK LETTER U
unicode_alphabetic(0x1BE7, 0x1BE7).	% Alphabetic Mc       BATAK VOWEL SIGN E
unicode_alphabetic(0x1BE8, 0x1BE9).	% Alphabetic Mn   [2] BATAK VOWEL SIGN PAKPAK E..BATAK VOWEL SIGN EE
unicode_alphabetic(0x1BEA, 0x1BEC).	% Alphabetic Mc   [3] BATAK VOWEL SIGN I..BATAK VOWEL SIGN O
unicode_alphabetic(0x1BED, 0x1BED).	% Alphabetic Mn       BATAK VOWEL SIGN KARO O
unicode_alphabetic(0x1BEE, 0x1BEE).	% Alphabetic Mc       BATAK VOWEL SIGN U
unicode_alphabetic(0x1BEF, 0x1BF1).	% Alphabetic Mn   [3] BATAK VOWEL SIGN U FOR SIMALUNGUN SA..BATAK CONSONANT SIGN H
unicode_alphabetic(0x1C00, 0x1C23).	% Alphabetic Lo  [36] LEPCHA LETTER KA..LEPCHA LETTER A
unicode_alphabetic(0x1C24, 0x1C2B).	% Alphabetic Mc   [8] LEPCHA SUBJOINED LETTER YA..LEPCHA VOWEL SIGN UU
unicode_alphabetic(0x1C2C, 0x1C33).	% Alphabetic Mn   [8] LEPCHA VOWEL SIGN E..LEPCHA CONSONANT SIGN T
unicode_alphabetic(0x1C34, 0x1C35).	% Alphabetic Mc   [2] LEPCHA CONSONANT SIGN NYIN-DO..LEPCHA CONSONANT SIGN KANG
unicode_alphabetic(0x1C4D, 0x1C4F).	% Alphabetic Lo   [3] LEPCHA LETTER TTA..LEPCHA LETTER DDA
unicode_alphabetic(0x1C5A, 0x1C77).	% Alphabetic Lo  [30] OL CHIKI LETTER LA..OL CHIKI LETTER OH
unicode_alphabetic(0x1C78, 0x1C7D).	% Alphabetic Lm   [6] OL CHIKI MU TTUDDAG..OL CHIKI AHAD
unicode_alphabetic(0x1CE9, 0x1CEC).	% Alphabetic Lo   [4] VEDIC SIGN ANUSVARA ANTARGOMUKHA..VEDIC SIGN ANUSVARA VAMAGOMUKHA WITH TAIL
unicode_alphabetic(0x1CEE, 0x1CF1).	% Alphabetic Lo   [4] VEDIC SIGN HEXIFORM LONG ANUSVARA..VEDIC SIGN ANUSVARA UBHAYATO MUKHA
unicode_alphabetic(0x1CF2, 0x1CF3).	% Alphabetic Mc   [2] VEDIC SIGN ARDHAVISARGA..VEDIC SIGN ROTATED ARDHAVISARGA
unicode_alphabetic(0x1CF5, 0x1CF6).	% Alphabetic Lo   [2] VEDIC SIGN JIHVAMULIYA..VEDIC SIGN UPADHMANIYA
unicode_alphabetic(0x1D00, 0x1D2B).	% Alphabetic L&  [44] LATIN LETTER SMALL CAPITAL A..CYRILLIC LETTER SMALL CAPITAL EL
unicode_alphabetic(0x1D2C, 0x1D6A).	% Alphabetic Lm  [63] MODIFIER LETTER CAPITAL A..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_alphabetic(0x1D6B, 0x1D77).	% Alphabetic L&  [13] LATIN SMALL LETTER UE..LATIN SMALL LETTER TURNED G
unicode_alphabetic(0x1D78, 0x1D78).	% Alphabetic Lm       MODIFIER LETTER CYRILLIC EN
unicode_alphabetic(0x1D79, 0x1D9A).	% Alphabetic L&  [34] LATIN SMALL LETTER INSULAR G..LATIN SMALL LETTER EZH WITH RETROFLEX HOOK
unicode_alphabetic(0x1D9B, 0x1DBF).	% Alphabetic Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_alphabetic(0x1E00, 0x1F15).	% Alphabetic L& [278] LATIN CAPITAL LETTER A WITH RING BELOW..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_alphabetic(0x1F18, 0x1F1D).	% Alphabetic L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_alphabetic(0x1F20, 0x1F45).	% Alphabetic L&  [38] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_alphabetic(0x1F48, 0x1F4D).	% Alphabetic L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_alphabetic(0x1F50, 0x1F57).	% Alphabetic L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_alphabetic(0x1F59, 0x1F59).	% Alphabetic L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_alphabetic(0x1F5B, 0x1F5B).	% Alphabetic L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_alphabetic(0x1F5D, 0x1F5D).	% Alphabetic L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_alphabetic(0x1F5F, 0x1F7D).	% Alphabetic L&  [31] GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_alphabetic(0x1F80, 0x1FB4).	% Alphabetic L&  [53] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_alphabetic(0x1FB6, 0x1FBC).	% Alphabetic L&   [7] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_alphabetic(0x1FBE, 0x1FBE).	% Alphabetic L&       GREEK PROSGEGRAMMENI
unicode_alphabetic(0x1FC2, 0x1FC4).	% Alphabetic L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_alphabetic(0x1FC6, 0x1FCC).	% Alphabetic L&   [7] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_alphabetic(0x1FD0, 0x1FD3).	% Alphabetic L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_alphabetic(0x1FD6, 0x1FDB).	% Alphabetic L&   [6] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_alphabetic(0x1FE0, 0x1FEC).	% Alphabetic L&  [13] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_alphabetic(0x1FF2, 0x1FF4).	% Alphabetic L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_alphabetic(0x1FF6, 0x1FFC).	% Alphabetic L&   [7] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_alphabetic(0x2071, 0x2071).	% Alphabetic Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_alphabetic(0x207F, 0x207F).	% Alphabetic Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_alphabetic(0x2090, 0x209C).	% Alphabetic Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_alphabetic(0x2102, 0x2102).	% Alphabetic L&       DOUBLE-STRUCK CAPITAL C
unicode_alphabetic(0x2107, 0x2107).	% Alphabetic L&       EULER CONSTANT
unicode_alphabetic(0x210A, 0x2113).	% Alphabetic L&  [10] SCRIPT SMALL G..SCRIPT SMALL L
unicode_alphabetic(0x2115, 0x2115).	% Alphabetic L&       DOUBLE-STRUCK CAPITAL N
unicode_alphabetic(0x2119, 0x211D).	% Alphabetic L&   [5] DOUBLE-STRUCK CAPITAL P..DOUBLE-STRUCK CAPITAL R
unicode_alphabetic(0x2124, 0x2124).	% Alphabetic L&       DOUBLE-STRUCK CAPITAL Z
unicode_alphabetic(0x2126, 0x2126).	% Alphabetic L&       OHM SIGN
unicode_alphabetic(0x2128, 0x2128).	% Alphabetic L&       BLACK-LETTER CAPITAL Z
unicode_alphabetic(0x212A, 0x212D).	% Alphabetic L&   [4] KELVIN SIGN..BLACK-LETTER CAPITAL C
unicode_alphabetic(0x212F, 0x2134).	% Alphabetic L&   [6] SCRIPT SMALL E..SCRIPT SMALL O
unicode_alphabetic(0x2135, 0x2138).	% Alphabetic Lo   [4] ALEF SYMBOL..DALET SYMBOL
unicode_alphabetic(0x2139, 0x2139).	% Alphabetic L&       INFORMATION SOURCE
unicode_alphabetic(0x213C, 0x213F).	% Alphabetic L&   [4] DOUBLE-STRUCK SMALL PI..DOUBLE-STRUCK CAPITAL PI
unicode_alphabetic(0x2145, 0x2149).	% Alphabetic L&   [5] DOUBLE-STRUCK ITALIC CAPITAL D..DOUBLE-STRUCK ITALIC SMALL J
unicode_alphabetic(0x214E, 0x214E).	% Alphabetic L&       TURNED SMALL F
unicode_alphabetic(0x2160, 0x2182).	% Alphabetic Nl  [35] ROMAN NUMERAL ONE..ROMAN NUMERAL TEN THOUSAND
unicode_alphabetic(0x2183, 0x2184).	% Alphabetic L&   [2] ROMAN NUMERAL REVERSED ONE HUNDRED..LATIN SMALL LETTER REVERSED C
unicode_alphabetic(0x2185, 0x2188).	% Alphabetic Nl   [4] ROMAN NUMERAL SIX LATE FORM..ROMAN NUMERAL ONE HUNDRED THOUSAND
unicode_alphabetic(0x24B6, 0x24E9).	% Alphabetic So  [52] CIRCLED LATIN CAPITAL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_alphabetic(0x2C00, 0x2C2E).	% Alphabetic L&  [47] GLAGOLITIC CAPITAL LETTER AZU..GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_alphabetic(0x2C30, 0x2C5E).	% Alphabetic L&  [47] GLAGOLITIC SMALL LETTER AZU..GLAGOLITIC SMALL LETTER LATINATE MYSLITE
unicode_alphabetic(0x2C60, 0x2C7B).	% Alphabetic L&  [28] LATIN CAPITAL LETTER L WITH DOUBLE BAR..LATIN LETTER SMALL CAPITAL TURNED E
unicode_alphabetic(0x2C7C, 0x2C7D).	% Alphabetic Lm   [2] LATIN SUBSCRIPT SMALL LETTER J..MODIFIER LETTER CAPITAL V
unicode_alphabetic(0x2C7E, 0x2CE4).	% Alphabetic L& [103] LATIN CAPITAL LETTER S WITH SWASH TAIL..COPTIC SYMBOL KAI
unicode_alphabetic(0x2CEB, 0x2CEE).	% Alphabetic L&   [4] COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI..COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
unicode_alphabetic(0x2CF2, 0x2CF3).	% Alphabetic L&   [2] COPTIC CAPITAL LETTER BOHAIRIC KHEI..COPTIC SMALL LETTER BOHAIRIC KHEI
unicode_alphabetic(0x2D00, 0x2D25).	% Alphabetic L&  [38] GEORGIAN SMALL LETTER AN..GEORGIAN SMALL LETTER HOE
unicode_alphabetic(0x2D27, 0x2D27).	% Alphabetic L&       GEORGIAN SMALL LETTER YN
unicode_alphabetic(0x2D2D, 0x2D2D).	% Alphabetic L&       GEORGIAN SMALL LETTER AEN
unicode_alphabetic(0x2D30, 0x2D67).	% Alphabetic Lo  [56] TIFINAGH LETTER YA..TIFINAGH LETTER YO
unicode_alphabetic(0x2D6F, 0x2D6F).	% Alphabetic Lm       TIFINAGH MODIFIER LETTER LABIALIZATION MARK
unicode_alphabetic(0x2D80, 0x2D96).	% Alphabetic Lo  [23] ETHIOPIC SYLLABLE LOA..ETHIOPIC SYLLABLE GGWE
unicode_alphabetic(0x2DA0, 0x2DA6).	% Alphabetic Lo   [7] ETHIOPIC SYLLABLE SSA..ETHIOPIC SYLLABLE SSO
unicode_alphabetic(0x2DA8, 0x2DAE).	% Alphabetic Lo   [7] ETHIOPIC SYLLABLE CCA..ETHIOPIC SYLLABLE CCO
unicode_alphabetic(0x2DB0, 0x2DB6).	% Alphabetic Lo   [7] ETHIOPIC SYLLABLE ZZA..ETHIOPIC SYLLABLE ZZO
unicode_alphabetic(0x2DB8, 0x2DBE).	% Alphabetic Lo   [7] ETHIOPIC SYLLABLE CCHA..ETHIOPIC SYLLABLE CCHO
unicode_alphabetic(0x2DC0, 0x2DC6).	% Alphabetic Lo   [7] ETHIOPIC SYLLABLE QYA..ETHIOPIC SYLLABLE QYO
unicode_alphabetic(0x2DC8, 0x2DCE).	% Alphabetic Lo   [7] ETHIOPIC SYLLABLE KYA..ETHIOPIC SYLLABLE KYO
unicode_alphabetic(0x2DD0, 0x2DD6).	% Alphabetic Lo   [7] ETHIOPIC SYLLABLE XYA..ETHIOPIC SYLLABLE XYO
unicode_alphabetic(0x2DD8, 0x2DDE).	% Alphabetic Lo   [7] ETHIOPIC SYLLABLE GYA..ETHIOPIC SYLLABLE GYO
unicode_alphabetic(0x2DE0, 0x2DFF).	% Alphabetic Mn  [32] COMBINING CYRILLIC LETTER BE..COMBINING CYRILLIC LETTER IOTIFIED BIG YUS
unicode_alphabetic(0x2E2F, 0x2E2F).	% Alphabetic Lm       VERTICAL TILDE
unicode_alphabetic(0x3005, 0x3005).	% Alphabetic Lm       IDEOGRAPHIC ITERATION MARK
unicode_alphabetic(0x3006, 0x3006).	% Alphabetic Lo       IDEOGRAPHIC CLOSING MARK
unicode_alphabetic(0x3007, 0x3007).	% Alphabetic Nl       IDEOGRAPHIC NUMBER ZERO
unicode_alphabetic(0x3021, 0x3029).	% Alphabetic Nl   [9] HANGZHOU NUMERAL ONE..HANGZHOU NUMERAL NINE
unicode_alphabetic(0x3031, 0x3035).	% Alphabetic Lm   [5] VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF
unicode_alphabetic(0x3038, 0x303A).	% Alphabetic Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
unicode_alphabetic(0x303B, 0x303B).	% Alphabetic Lm       VERTICAL IDEOGRAPHIC ITERATION MARK
unicode_alphabetic(0x303C, 0x303C).	% Alphabetic Lo       MASU MARK
unicode_alphabetic(0x3041, 0x3096).	% Alphabetic Lo  [86] HIRAGANA LETTER SMALL A..HIRAGANA LETTER SMALL KE
unicode_alphabetic(0x309D, 0x309E).	% Alphabetic Lm   [2] HIRAGANA ITERATION MARK..HIRAGANA VOICED ITERATION MARK
unicode_alphabetic(0x309F, 0x309F).	% Alphabetic Lo       HIRAGANA DIGRAPH YORI
unicode_alphabetic(0x30A1, 0x30FA).	% Alphabetic Lo  [90] KATAKANA LETTER SMALL A..KATAKANA LETTER VO
unicode_alphabetic(0x30FC, 0x30FE).	% Alphabetic Lm   [3] KATAKANA-HIRAGANA PROLONGED SOUND MARK..KATAKANA VOICED ITERATION MARK
unicode_alphabetic(0x30FF, 0x30FF).	% Alphabetic Lo       KATAKANA DIGRAPH KOTO
unicode_alphabetic(0x3105, 0x312D).	% Alphabetic Lo  [41] BOPOMOFO LETTER B..BOPOMOFO LETTER IH
unicode_alphabetic(0x3131, 0x318E).	% Alphabetic Lo  [94] HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE
unicode_alphabetic(0x31A0, 0x31BA).	% Alphabetic Lo  [27] BOPOMOFO LETTER BU..BOPOMOFO LETTER ZY
unicode_alphabetic(0x31F0, 0x31FF).	% Alphabetic Lo  [16] KATAKANA LETTER SMALL KU..KATAKANA LETTER SMALL RO
unicode_alphabetic(0x3400, 0x4DB5).	% Alphabetic Lo [6582] CJK UNIFIED IDEOGRAPH-3400..CJK UNIFIED IDEOGRAPH-4DB5
unicode_alphabetic(0x4E00, 0x9FCC).	% Alphabetic Lo [20941] CJK UNIFIED IDEOGRAPH-4E00..CJK UNIFIED IDEOGRAPH-9FCC
unicode_alphabetic(0xA000, 0xA014).	% Alphabetic Lo  [21] YI SYLLABLE IT..YI SYLLABLE E
unicode_alphabetic(0xA015, 0xA015).	% Alphabetic Lm       YI SYLLABLE WU
unicode_alphabetic(0xA016, 0xA48C).	% Alphabetic Lo [1143] YI SYLLABLE BIT..YI SYLLABLE YYR
unicode_alphabetic(0xA4D0, 0xA4F7).	% Alphabetic Lo  [40] LISU LETTER BA..LISU LETTER OE
unicode_alphabetic(0xA4F8, 0xA4FD).	% Alphabetic Lm   [6] LISU LETTER TONE MYA TI..LISU LETTER TONE MYA JEU
unicode_alphabetic(0xA500, 0xA60B).	% Alphabetic Lo [268] VAI SYLLABLE EE..VAI SYLLABLE NG
unicode_alphabetic(0xA60C, 0xA60C).	% Alphabetic Lm       VAI SYLLABLE LENGTHENER
unicode_alphabetic(0xA610, 0xA61F).	% Alphabetic Lo  [16] VAI SYLLABLE NDOLE FA..VAI SYMBOL JONG
unicode_alphabetic(0xA62A, 0xA62B).	% Alphabetic Lo   [2] VAI SYLLABLE NDOLE MA..VAI SYLLABLE NDOLE DO
unicode_alphabetic(0xA640, 0xA66D).	% Alphabetic L&  [46] CYRILLIC CAPITAL LETTER ZEMLYA..CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
unicode_alphabetic(0xA66E, 0xA66E).	% Alphabetic Lo       CYRILLIC LETTER MULTIOCULAR O
unicode_alphabetic(0xA674, 0xA67B).	% Alphabetic Mn   [8] COMBINING CYRILLIC LETTER UKRAINIAN IE..COMBINING CYRILLIC LETTER OMEGA
unicode_alphabetic(0xA67F, 0xA67F).	% Alphabetic Lm       CYRILLIC PAYEROK
unicode_alphabetic(0xA680, 0xA697).	% Alphabetic L&  [24] CYRILLIC CAPITAL LETTER DWE..CYRILLIC SMALL LETTER SHWE
unicode_alphabetic(0xA69F, 0xA69F).	% Alphabetic Mn       COMBINING CYRILLIC LETTER IOTIFIED E
unicode_alphabetic(0xA6A0, 0xA6E5).	% Alphabetic Lo  [70] BAMUM LETTER A..BAMUM LETTER KI
unicode_alphabetic(0xA6E6, 0xA6EF).	% Alphabetic Nl  [10] BAMUM LETTER MO..BAMUM LETTER KOGHOM
unicode_alphabetic(0xA717, 0xA71F).	% Alphabetic Lm   [9] MODIFIER LETTER DOT VERTICAL BAR..MODIFIER LETTER LOW INVERTED EXCLAMATION MARK
unicode_alphabetic(0xA722, 0xA76F).	% Alphabetic L&  [78] LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF..LATIN SMALL LETTER CON
unicode_alphabetic(0xA770, 0xA770).	% Alphabetic Lm       MODIFIER LETTER US
unicode_alphabetic(0xA771, 0xA787).	% Alphabetic L&  [23] LATIN SMALL LETTER DUM..LATIN SMALL LETTER INSULAR T
unicode_alphabetic(0xA788, 0xA788).	% Alphabetic Lm       MODIFIER LETTER LOW CIRCUMFLEX ACCENT
unicode_alphabetic(0xA78B, 0xA78E).	% Alphabetic L&   [4] LATIN CAPITAL LETTER SALTILLO..LATIN SMALL LETTER L WITH RETROFLEX HOOK AND BELT
unicode_alphabetic(0xA790, 0xA793).	% Alphabetic L&   [4] LATIN CAPITAL LETTER N WITH DESCENDER..LATIN SMALL LETTER C WITH BAR
unicode_alphabetic(0xA7A0, 0xA7AA).	% Alphabetic L&  [11] LATIN CAPITAL LETTER G WITH OBLIQUE STROKE..LATIN CAPITAL LETTER H WITH HOOK
unicode_alphabetic(0xA7F8, 0xA7F9).	% Alphabetic Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_alphabetic(0xA7FA, 0xA7FA).	% Alphabetic L&       LATIN LETTER SMALL CAPITAL TURNED M
unicode_alphabetic(0xA7FB, 0xA801).	% Alphabetic Lo   [7] LATIN EPIGRAPHIC LETTER REVERSED F..SYLOTI NAGRI LETTER I
unicode_alphabetic(0xA803, 0xA805).	% Alphabetic Lo   [3] SYLOTI NAGRI LETTER U..SYLOTI NAGRI LETTER O
unicode_alphabetic(0xA807, 0xA80A).	% Alphabetic Lo   [4] SYLOTI NAGRI LETTER KO..SYLOTI NAGRI LETTER GHO
unicode_alphabetic(0xA80C, 0xA822).	% Alphabetic Lo  [23] SYLOTI NAGRI LETTER CO..SYLOTI NAGRI LETTER HO
unicode_alphabetic(0xA823, 0xA824).	% Alphabetic Mc   [2] SYLOTI NAGRI VOWEL SIGN A..SYLOTI NAGRI VOWEL SIGN I
unicode_alphabetic(0xA825, 0xA826).	% Alphabetic Mn   [2] SYLOTI NAGRI VOWEL SIGN U..SYLOTI NAGRI VOWEL SIGN E
unicode_alphabetic(0xA827, 0xA827).	% Alphabetic Mc       SYLOTI NAGRI VOWEL SIGN OO
unicode_alphabetic(0xA840, 0xA873).	% Alphabetic Lo  [52] PHAGS-PA LETTER KA..PHAGS-PA LETTER CANDRABINDU
unicode_alphabetic(0xA880, 0xA881).	% Alphabetic Mc   [2] SAURASHTRA SIGN ANUSVARA..SAURASHTRA SIGN VISARGA
unicode_alphabetic(0xA882, 0xA8B3).	% Alphabetic Lo  [50] SAURASHTRA LETTER A..SAURASHTRA LETTER LLA
unicode_alphabetic(0xA8B4, 0xA8C3).	% Alphabetic Mc  [16] SAURASHTRA CONSONANT SIGN HAARU..SAURASHTRA VOWEL SIGN AU
unicode_alphabetic(0xA8F2, 0xA8F7).	% Alphabetic Lo   [6] DEVANAGARI SIGN SPACING CANDRABINDU..DEVANAGARI SIGN CANDRABINDU AVAGRAHA
unicode_alphabetic(0xA8FB, 0xA8FB).	% Alphabetic Lo       DEVANAGARI HEADSTROKE
unicode_alphabetic(0xA90A, 0xA925).	% Alphabetic Lo  [28] KAYAH LI LETTER KA..KAYAH LI LETTER OO
unicode_alphabetic(0xA926, 0xA92A).	% Alphabetic Mn   [5] KAYAH LI VOWEL UE..KAYAH LI VOWEL O
unicode_alphabetic(0xA930, 0xA946).	% Alphabetic Lo  [23] REJANG LETTER KA..REJANG LETTER A
unicode_alphabetic(0xA947, 0xA951).	% Alphabetic Mn  [11] REJANG VOWEL SIGN I..REJANG CONSONANT SIGN R
unicode_alphabetic(0xA952, 0xA952).	% Alphabetic Mc       REJANG CONSONANT SIGN H
unicode_alphabetic(0xA960, 0xA97C).	% Alphabetic Lo  [29] HANGUL CHOSEONG TIKEUT-MIEUM..HANGUL CHOSEONG SSANGYEORINHIEUH
unicode_alphabetic(0xA980, 0xA982).	% Alphabetic Mn   [3] JAVANESE SIGN PANYANGGA..JAVANESE SIGN LAYAR
unicode_alphabetic(0xA983, 0xA983).	% Alphabetic Mc       JAVANESE SIGN WIGNYAN
unicode_alphabetic(0xA984, 0xA9B2).	% Alphabetic Lo  [47] JAVANESE LETTER A..JAVANESE LETTER HA
unicode_alphabetic(0xA9B4, 0xA9B5).	% Alphabetic Mc   [2] JAVANESE VOWEL SIGN TARUNG..JAVANESE VOWEL SIGN TOLONG
unicode_alphabetic(0xA9B6, 0xA9B9).	% Alphabetic Mn   [4] JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN SUKU MENDUT
unicode_alphabetic(0xA9BA, 0xA9BB).	% Alphabetic Mc   [2] JAVANESE VOWEL SIGN TALING..JAVANESE VOWEL SIGN DIRGA MURE
unicode_alphabetic(0xA9BC, 0xA9BC).	% Alphabetic Mn       JAVANESE VOWEL SIGN PEPET
unicode_alphabetic(0xA9BD, 0xA9BF).	% Alphabetic Mc   [3] JAVANESE CONSONANT SIGN KERET..JAVANESE CONSONANT SIGN CAKRA
unicode_alphabetic(0xA9CF, 0xA9CF).	% Alphabetic Lm       JAVANESE PANGRANGKEP
unicode_alphabetic(0xAA00, 0xAA28).	% Alphabetic Lo  [41] CHAM LETTER A..CHAM LETTER HA
unicode_alphabetic(0xAA29, 0xAA2E).	% Alphabetic Mn   [6] CHAM VOWEL SIGN AA..CHAM VOWEL SIGN OE
unicode_alphabetic(0xAA2F, 0xAA30).	% Alphabetic Mc   [2] CHAM VOWEL SIGN O..CHAM VOWEL SIGN AI
unicode_alphabetic(0xAA31, 0xAA32).	% Alphabetic Mn   [2] CHAM VOWEL SIGN AU..CHAM VOWEL SIGN UE
unicode_alphabetic(0xAA33, 0xAA34).	% Alphabetic Mc   [2] CHAM CONSONANT SIGN YA..CHAM CONSONANT SIGN RA
unicode_alphabetic(0xAA35, 0xAA36).	% Alphabetic Mn   [2] CHAM CONSONANT SIGN LA..CHAM CONSONANT SIGN WA
unicode_alphabetic(0xAA40, 0xAA42).	% Alphabetic Lo   [3] CHAM LETTER FINAL K..CHAM LETTER FINAL NG
unicode_alphabetic(0xAA43, 0xAA43).	% Alphabetic Mn       CHAM CONSONANT SIGN FINAL NG
unicode_alphabetic(0xAA44, 0xAA4B).	% Alphabetic Lo   [8] CHAM LETTER FINAL CH..CHAM LETTER FINAL SS
unicode_alphabetic(0xAA4C, 0xAA4C).	% Alphabetic Mn       CHAM CONSONANT SIGN FINAL M
unicode_alphabetic(0xAA4D, 0xAA4D).	% Alphabetic Mc       CHAM CONSONANT SIGN FINAL H
unicode_alphabetic(0xAA60, 0xAA6F).	% Alphabetic Lo  [16] MYANMAR LETTER KHAMTI GA..MYANMAR LETTER KHAMTI FA
unicode_alphabetic(0xAA70, 0xAA70).	% Alphabetic Lm       MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION
unicode_alphabetic(0xAA71, 0xAA76).	% Alphabetic Lo   [6] MYANMAR LETTER KHAMTI XA..MYANMAR LOGOGRAM KHAMTI HM
unicode_alphabetic(0xAA7A, 0xAA7A).	% Alphabetic Lo       MYANMAR LETTER AITON RA
unicode_alphabetic(0xAA80, 0xAAAF).	% Alphabetic Lo  [48] TAI VIET LETTER LOW KO..TAI VIET LETTER HIGH O
unicode_alphabetic(0xAAB0, 0xAAB0).	% Alphabetic Mn       TAI VIET MAI KANG
unicode_alphabetic(0xAAB1, 0xAAB1).	% Alphabetic Lo       TAI VIET VOWEL AA
unicode_alphabetic(0xAAB2, 0xAAB4).	% Alphabetic Mn   [3] TAI VIET VOWEL I..TAI VIET VOWEL U
unicode_alphabetic(0xAAB5, 0xAAB6).	% Alphabetic Lo   [2] TAI VIET VOWEL E..TAI VIET VOWEL O
unicode_alphabetic(0xAAB7, 0xAAB8).	% Alphabetic Mn   [2] TAI VIET MAI KHIT..TAI VIET VOWEL IA
unicode_alphabetic(0xAAB9, 0xAABD).	% Alphabetic Lo   [5] TAI VIET VOWEL UEA..TAI VIET VOWEL AN
unicode_alphabetic(0xAABE, 0xAABE).	% Alphabetic Mn       TAI VIET VOWEL AM
unicode_alphabetic(0xAAC0, 0xAAC0).	% Alphabetic Lo       TAI VIET TONE MAI NUENG
unicode_alphabetic(0xAAC2, 0xAAC2).	% Alphabetic Lo       TAI VIET TONE MAI SONG
unicode_alphabetic(0xAADB, 0xAADC).	% Alphabetic Lo   [2] TAI VIET SYMBOL KON..TAI VIET SYMBOL NUENG
unicode_alphabetic(0xAADD, 0xAADD).	% Alphabetic Lm       TAI VIET SYMBOL SAM
unicode_alphabetic(0xAAE0, 0xAAEA).	% Alphabetic Lo  [11] MEETEI MAYEK LETTER E..MEETEI MAYEK LETTER SSA
unicode_alphabetic(0xAAEB, 0xAAEB).	% Alphabetic Mc       MEETEI MAYEK VOWEL SIGN II
unicode_alphabetic(0xAAEC, 0xAAED).	% Alphabetic Mn   [2] MEETEI MAYEK VOWEL SIGN UU..MEETEI MAYEK VOWEL SIGN AAI
unicode_alphabetic(0xAAEE, 0xAAEF).	% Alphabetic Mc   [2] MEETEI MAYEK VOWEL SIGN AU..MEETEI MAYEK VOWEL SIGN AAU
unicode_alphabetic(0xAAF2, 0xAAF2).	% Alphabetic Lo       MEETEI MAYEK ANJI
unicode_alphabetic(0xAAF3, 0xAAF4).	% Alphabetic Lm   [2] MEETEI MAYEK SYLLABLE REPETITION MARK..MEETEI MAYEK WORD REPETITION MARK
unicode_alphabetic(0xAAF5, 0xAAF5).	% Alphabetic Mc       MEETEI MAYEK VOWEL SIGN VISARGA
unicode_alphabetic(0xAB01, 0xAB06).	% Alphabetic Lo   [6] ETHIOPIC SYLLABLE TTHU..ETHIOPIC SYLLABLE TTHO
unicode_alphabetic(0xAB09, 0xAB0E).	% Alphabetic Lo   [6] ETHIOPIC SYLLABLE DDHU..ETHIOPIC SYLLABLE DDHO
unicode_alphabetic(0xAB11, 0xAB16).	% Alphabetic Lo   [6] ETHIOPIC SYLLABLE DZU..ETHIOPIC SYLLABLE DZO
unicode_alphabetic(0xAB20, 0xAB26).	% Alphabetic Lo   [7] ETHIOPIC SYLLABLE CCHHA..ETHIOPIC SYLLABLE CCHHO
unicode_alphabetic(0xAB28, 0xAB2E).	% Alphabetic Lo   [7] ETHIOPIC SYLLABLE BBA..ETHIOPIC SYLLABLE BBO
unicode_alphabetic(0xABC0, 0xABE2).	% Alphabetic Lo  [35] MEETEI MAYEK LETTER KOK..MEETEI MAYEK LETTER I LONSUM
unicode_alphabetic(0xABE3, 0xABE4).	% Alphabetic Mc   [2] MEETEI MAYEK VOWEL SIGN ONAP..MEETEI MAYEK VOWEL SIGN INAP
unicode_alphabetic(0xABE5, 0xABE5).	% Alphabetic Mn       MEETEI MAYEK VOWEL SIGN ANAP
unicode_alphabetic(0xABE6, 0xABE7).	% Alphabetic Mc   [2] MEETEI MAYEK VOWEL SIGN YENAP..MEETEI MAYEK VOWEL SIGN SOUNAP
unicode_alphabetic(0xABE8, 0xABE8).	% Alphabetic Mn       MEETEI MAYEK VOWEL SIGN UNAP
unicode_alphabetic(0xABE9, 0xABEA).	% Alphabetic Mc   [2] MEETEI MAYEK VOWEL SIGN CHEINAP..MEETEI MAYEK VOWEL SIGN NUNG
unicode_alphabetic(0xAC00, 0xD7A3).	% Alphabetic Lo [11172] HANGUL SYLLABLE GA..HANGUL SYLLABLE HIH
unicode_alphabetic(0xD7B0, 0xD7C6).	% Alphabetic Lo  [23] HANGUL JUNGSEONG O-YEO..HANGUL JUNGSEONG ARAEA-E
unicode_alphabetic(0xD7CB, 0xD7FB).	% Alphabetic Lo  [49] HANGUL JONGSEONG NIEUN-RIEUL..HANGUL JONGSEONG PHIEUPH-THIEUTH
unicode_alphabetic(0xF900, 0xFA6D).	% Alphabetic Lo [366] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA6D
unicode_alphabetic(0xFA70, 0xFAD9).	% Alphabetic Lo [106] CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9
unicode_alphabetic(0xFB00, 0xFB06).	% Alphabetic L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_alphabetic(0xFB13, 0xFB17).	% Alphabetic L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_alphabetic(0xFB1D, 0xFB1D).	% Alphabetic Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_alphabetic(0xFB1E, 0xFB1E).	% Alphabetic Mn       HEBREW POINT JUDEO-SPANISH VARIKA
unicode_alphabetic(0xFB1F, 0xFB28).	% Alphabetic Lo  [10] HEBREW LIGATURE YIDDISH YOD YOD PATAH..HEBREW LETTER WIDE TAV
unicode_alphabetic(0xFB2A, 0xFB36).	% Alphabetic Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_alphabetic(0xFB38, 0xFB3C).	% Alphabetic Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_alphabetic(0xFB3E, 0xFB3E).	% Alphabetic Lo       HEBREW LETTER MEM WITH DAGESH
unicode_alphabetic(0xFB40, 0xFB41).	% Alphabetic Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_alphabetic(0xFB43, 0xFB44).	% Alphabetic Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_alphabetic(0xFB46, 0xFBB1).	% Alphabetic Lo [108] HEBREW LETTER TSADI WITH DAGESH..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
unicode_alphabetic(0xFBD3, 0xFD3D).	% Alphabetic Lo [363] ARABIC LETTER NG ISOLATED FORM..ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
unicode_alphabetic(0xFD50, 0xFD8F).	% Alphabetic Lo  [64] ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
unicode_alphabetic(0xFD92, 0xFDC7).	% Alphabetic Lo  [54] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
unicode_alphabetic(0xFDF0, 0xFDFB).	% Alphabetic Lo  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
unicode_alphabetic(0xFE70, 0xFE74).	% Alphabetic Lo   [5] ARABIC FATHATAN ISOLATED FORM..ARABIC KASRATAN ISOLATED FORM
unicode_alphabetic(0xFE76, 0xFEFC).	% Alphabetic Lo [135] ARABIC FATHA ISOLATED FORM..ARABIC LIGATURE LAM WITH ALEF FINAL FORM
unicode_alphabetic(0xFF21, 0xFF3A).	% Alphabetic L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_alphabetic(0xFF41, 0xFF5A).	% Alphabetic L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_alphabetic(0xFF66, 0xFF6F).	% Alphabetic Lo  [10] HALFWIDTH KATAKANA LETTER WO..HALFWIDTH KATAKANA LETTER SMALL TU
unicode_alphabetic(0xFF70, 0xFF70).	% Alphabetic Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_alphabetic(0xFF71, 0xFF9D).	% Alphabetic Lo  [45] HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N
unicode_alphabetic(0xFF9E, 0xFF9F).	% Alphabetic Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
unicode_alphabetic(0xFFA0, 0xFFBE).	% Alphabetic Lo  [31] HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH
unicode_alphabetic(0xFFC2, 0xFFC7).	% Alphabetic Lo   [6] HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E
unicode_alphabetic(0xFFCA, 0xFFCF).	% Alphabetic Lo   [6] HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE
unicode_alphabetic(0xFFD2, 0xFFD7).	% Alphabetic Lo   [6] HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU
unicode_alphabetic(0xFFDA, 0xFFDC).	% Alphabetic Lo   [3] HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I
unicode_alphabetic(0x10000, 0x1000B).	% Alphabetic Lo  [12] LINEAR B SYLLABLE B008 A..LINEAR B SYLLABLE B046 JE
unicode_alphabetic(0x1000D, 0x10026).	% Alphabetic Lo  [26] LINEAR B SYLLABLE B036 JO..LINEAR B SYLLABLE B032 QO
unicode_alphabetic(0x10028, 0x1003A).	% Alphabetic Lo  [19] LINEAR B SYLLABLE B060 RA..LINEAR B SYLLABLE B042 WO
unicode_alphabetic(0x1003C, 0x1003D).	% Alphabetic Lo   [2] LINEAR B SYLLABLE B017 ZA..LINEAR B SYLLABLE B074 ZE
unicode_alphabetic(0x1003F, 0x1004D).	% Alphabetic Lo  [15] LINEAR B SYLLABLE B020 ZO..LINEAR B SYLLABLE B091 TWO
unicode_alphabetic(0x10050, 0x1005D).	% Alphabetic Lo  [14] LINEAR B SYMBOL B018..LINEAR B SYMBOL B089
unicode_alphabetic(0x10080, 0x100FA).	% Alphabetic Lo [123] LINEAR B IDEOGRAM B100 MAN..LINEAR B IDEOGRAM VESSEL B305
unicode_alphabetic(0x10140, 0x10174).	% Alphabetic Nl  [53] GREEK ACROPHONIC ATTIC ONE QUARTER..GREEK ACROPHONIC STRATIAN FIFTY MNAS
unicode_alphabetic(0x10280, 0x1029C).	% Alphabetic Lo  [29] LYCIAN LETTER A..LYCIAN LETTER X
unicode_alphabetic(0x102A0, 0x102D0).	% Alphabetic Lo  [49] CARIAN LETTER A..CARIAN LETTER UUU3
unicode_alphabetic(0x10300, 0x1031E).	% Alphabetic Lo  [31] OLD ITALIC LETTER A..OLD ITALIC LETTER UU
unicode_alphabetic(0x10330, 0x10340).	% Alphabetic Lo  [17] GOTHIC LETTER AHSA..GOTHIC LETTER PAIRTHRA
unicode_alphabetic(0x10341, 0x10341).	% Alphabetic Nl       GOTHIC LETTER NINETY
unicode_alphabetic(0x10342, 0x10349).	% Alphabetic Lo   [8] GOTHIC LETTER RAIDA..GOTHIC LETTER OTHAL
unicode_alphabetic(0x1034A, 0x1034A).	% Alphabetic Nl       GOTHIC LETTER NINE HUNDRED
unicode_alphabetic(0x10380, 0x1039D).	% Alphabetic Lo  [30] UGARITIC LETTER ALPA..UGARITIC LETTER SSU
unicode_alphabetic(0x103A0, 0x103C3).	% Alphabetic Lo  [36] OLD PERSIAN SIGN A..OLD PERSIAN SIGN HA
unicode_alphabetic(0x103C8, 0x103CF).	% Alphabetic Lo   [8] OLD PERSIAN SIGN AURAMAZDAA..OLD PERSIAN SIGN BUUMISH
unicode_alphabetic(0x103D1, 0x103D5).	% Alphabetic Nl   [5] OLD PERSIAN NUMBER ONE..OLD PERSIAN NUMBER HUNDRED
unicode_alphabetic(0x10400, 0x1044F).	% Alphabetic L&  [80] DESERET CAPITAL LETTER LONG I..DESERET SMALL LETTER EW
unicode_alphabetic(0x10450, 0x1049D).	% Alphabetic Lo  [78] SHAVIAN LETTER PEEP..OSMANYA LETTER OO
unicode_alphabetic(0x10800, 0x10805).	% Alphabetic Lo   [6] CYPRIOT SYLLABLE A..CYPRIOT SYLLABLE JA
unicode_alphabetic(0x10808, 0x10808).	% Alphabetic Lo       CYPRIOT SYLLABLE JO
unicode_alphabetic(0x1080A, 0x10835).	% Alphabetic Lo  [44] CYPRIOT SYLLABLE KA..CYPRIOT SYLLABLE WO
unicode_alphabetic(0x10837, 0x10838).	% Alphabetic Lo   [2] CYPRIOT SYLLABLE XA..CYPRIOT SYLLABLE XE
unicode_alphabetic(0x1083C, 0x1083C).	% Alphabetic Lo       CYPRIOT SYLLABLE ZA
unicode_alphabetic(0x1083F, 0x10855).	% Alphabetic Lo  [23] CYPRIOT SYLLABLE ZO..IMPERIAL ARAMAIC LETTER TAW
unicode_alphabetic(0x10900, 0x10915).	% Alphabetic Lo  [22] PHOENICIAN LETTER ALF..PHOENICIAN LETTER TAU
unicode_alphabetic(0x10920, 0x10939).	% Alphabetic Lo  [26] LYDIAN LETTER A..LYDIAN LETTER C
unicode_alphabetic(0x10980, 0x109B7).	% Alphabetic Lo  [56] MEROITIC HIEROGLYPHIC LETTER A..MEROITIC CURSIVE LETTER DA
unicode_alphabetic(0x109BE, 0x109BF).	% Alphabetic Lo   [2] MEROITIC CURSIVE LOGOGRAM RMT..MEROITIC CURSIVE LOGOGRAM IMN
unicode_alphabetic(0x10A00, 0x10A00).	% Alphabetic Lo       KHAROSHTHI LETTER A
unicode_alphabetic(0x10A01, 0x10A03).	% Alphabetic Mn   [3] KHAROSHTHI VOWEL SIGN I..KHAROSHTHI VOWEL SIGN VOCALIC R
unicode_alphabetic(0x10A05, 0x10A06).	% Alphabetic Mn   [2] KHAROSHTHI VOWEL SIGN E..KHAROSHTHI VOWEL SIGN O
unicode_alphabetic(0x10A0C, 0x10A0F).	% Alphabetic Mn   [4] KHAROSHTHI VOWEL LENGTH MARK..KHAROSHTHI SIGN VISARGA
unicode_alphabetic(0x10A10, 0x10A13).	% Alphabetic Lo   [4] KHAROSHTHI LETTER KA..KHAROSHTHI LETTER GHA
unicode_alphabetic(0x10A15, 0x10A17).	% Alphabetic Lo   [3] KHAROSHTHI LETTER CA..KHAROSHTHI LETTER JA
unicode_alphabetic(0x10A19, 0x10A33).	% Alphabetic Lo  [27] KHAROSHTHI LETTER NYA..KHAROSHTHI LETTER TTTHA
unicode_alphabetic(0x10A60, 0x10A7C).	% Alphabetic Lo  [29] OLD SOUTH ARABIAN LETTER HE..OLD SOUTH ARABIAN LETTER THETH
unicode_alphabetic(0x10B00, 0x10B35).	% Alphabetic Lo  [54] AVESTAN LETTER A..AVESTAN LETTER HE
unicode_alphabetic(0x10B40, 0x10B55).	% Alphabetic Lo  [22] INSCRIPTIONAL PARTHIAN LETTER ALEPH..INSCRIPTIONAL PARTHIAN LETTER TAW
unicode_alphabetic(0x10B60, 0x10B72).	% Alphabetic Lo  [19] INSCRIPTIONAL PAHLAVI LETTER ALEPH..INSCRIPTIONAL PAHLAVI LETTER TAW
unicode_alphabetic(0x10C00, 0x10C48).	% Alphabetic Lo  [73] OLD TURKIC LETTER ORKHON A..OLD TURKIC LETTER ORKHON BASH
unicode_alphabetic(0x11000, 0x11000).	% Alphabetic Mc       BRAHMI SIGN CANDRABINDU
unicode_alphabetic(0x11001, 0x11001).	% Alphabetic Mn       BRAHMI SIGN ANUSVARA
unicode_alphabetic(0x11002, 0x11002).	% Alphabetic Mc       BRAHMI SIGN VISARGA
unicode_alphabetic(0x11003, 0x11037).	% Alphabetic Lo  [53] BRAHMI SIGN JIHVAMULIYA..BRAHMI LETTER OLD TAMIL NNNA
unicode_alphabetic(0x11038, 0x11045).	% Alphabetic Mn  [14] BRAHMI VOWEL SIGN AA..BRAHMI VOWEL SIGN AU
unicode_alphabetic(0x11082, 0x11082).	% Alphabetic Mc       KAITHI SIGN VISARGA
unicode_alphabetic(0x11083, 0x110AF).	% Alphabetic Lo  [45] KAITHI LETTER A..KAITHI LETTER HA
unicode_alphabetic(0x110B0, 0x110B2).	% Alphabetic Mc   [3] KAITHI VOWEL SIGN AA..KAITHI VOWEL SIGN II
unicode_alphabetic(0x110B3, 0x110B6).	% Alphabetic Mn   [4] KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN AI
unicode_alphabetic(0x110B7, 0x110B8).	% Alphabetic Mc   [2] KAITHI VOWEL SIGN O..KAITHI VOWEL SIGN AU
unicode_alphabetic(0x110D0, 0x110E8).	% Alphabetic Lo  [25] SORA SOMPENG LETTER SAH..SORA SOMPENG LETTER MAE
unicode_alphabetic(0x11100, 0x11102).	% Alphabetic Mn   [3] CHAKMA SIGN CANDRABINDU..CHAKMA SIGN VISARGA
unicode_alphabetic(0x11103, 0x11126).	% Alphabetic Lo  [36] CHAKMA LETTER AA..CHAKMA LETTER HAA
unicode_alphabetic(0x11127, 0x1112B).	% Alphabetic Mn   [5] CHAKMA VOWEL SIGN A..CHAKMA VOWEL SIGN UU
unicode_alphabetic(0x1112C, 0x1112C).	% Alphabetic Mc       CHAKMA VOWEL SIGN E
unicode_alphabetic(0x1112D, 0x11132).	% Alphabetic Mn   [6] CHAKMA VOWEL SIGN AI..CHAKMA AU MARK
unicode_alphabetic(0x11180, 0x11181).	% Alphabetic Mn   [2] SHARADA SIGN CANDRABINDU..SHARADA SIGN ANUSVARA
unicode_alphabetic(0x11182, 0x11182).	% Alphabetic Mc       SHARADA SIGN VISARGA
unicode_alphabetic(0x11183, 0x111B2).	% Alphabetic Lo  [48] SHARADA LETTER A..SHARADA LETTER HA
unicode_alphabetic(0x111B3, 0x111B5).	% Alphabetic Mc   [3] SHARADA VOWEL SIGN AA..SHARADA VOWEL SIGN II
unicode_alphabetic(0x111B6, 0x111BE).	% Alphabetic Mn   [9] SHARADA VOWEL SIGN U..SHARADA VOWEL SIGN O
unicode_alphabetic(0x111BF, 0x111BF).	% Alphabetic Mc       SHARADA VOWEL SIGN AU
unicode_alphabetic(0x111C1, 0x111C4).	% Alphabetic Lo   [4] SHARADA SIGN AVAGRAHA..SHARADA OM
unicode_alphabetic(0x11680, 0x116AA).	% Alphabetic Lo  [43] TAKRI LETTER A..TAKRI LETTER RRA
unicode_alphabetic(0x116AB, 0x116AB).	% Alphabetic Mn       TAKRI SIGN ANUSVARA
unicode_alphabetic(0x116AC, 0x116AC).	% Alphabetic Mc       TAKRI SIGN VISARGA
unicode_alphabetic(0x116AD, 0x116AD).	% Alphabetic Mn       TAKRI VOWEL SIGN AA
unicode_alphabetic(0x116AE, 0x116AF).	% Alphabetic Mc   [2] TAKRI VOWEL SIGN I..TAKRI VOWEL SIGN II
unicode_alphabetic(0x116B0, 0x116B5).	% Alphabetic Mn   [6] TAKRI VOWEL SIGN U..TAKRI VOWEL SIGN AU
unicode_alphabetic(0x12000, 0x1236E).	% Alphabetic Lo [879] CUNEIFORM SIGN A..CUNEIFORM SIGN ZUM
unicode_alphabetic(0x12400, 0x12462).	% Alphabetic Nl  [99] CUNEIFORM NUMERIC SIGN TWO ASH..CUNEIFORM NUMERIC SIGN OLD ASSYRIAN ONE QUARTER
unicode_alphabetic(0x13000, 0x1342E).	% Alphabetic Lo [1071] EGYPTIAN HIEROGLYPH A001..EGYPTIAN HIEROGLYPH AA032
unicode_alphabetic(0x16800, 0x16A38).	% Alphabetic Lo [569] BAMUM LETTER PHASE-A NGKUE MFON..BAMUM LETTER PHASE-F VUEQ
unicode_alphabetic(0x16F00, 0x16F44).	% Alphabetic Lo  [69] MIAO LETTER PA..MIAO LETTER HHA
unicode_alphabetic(0x16F50, 0x16F50).	% Alphabetic Lo       MIAO LETTER NASALIZATION
unicode_alphabetic(0x16F51, 0x16F7E).	% Alphabetic Mc  [46] MIAO SIGN ASPIRATION..MIAO VOWEL SIGN NG
unicode_alphabetic(0x16F93, 0x16F9F).	% Alphabetic Lm  [13] MIAO LETTER TONE-2..MIAO LETTER REFORMED TONE-8
unicode_alphabetic(0x1B000, 0x1B001).	% Alphabetic Lo   [2] KATAKANA LETTER ARCHAIC E..HIRAGANA LETTER ARCHAIC YE
unicode_alphabetic(0x1D400, 0x1D454).	% Alphabetic L&  [85] MATHEMATICAL BOLD CAPITAL A..MATHEMATICAL ITALIC SMALL G
unicode_alphabetic(0x1D456, 0x1D49C).	% Alphabetic L&  [71] MATHEMATICAL ITALIC SMALL I..MATHEMATICAL SCRIPT CAPITAL A
unicode_alphabetic(0x1D49E, 0x1D49F).	% Alphabetic L&   [2] MATHEMATICAL SCRIPT CAPITAL C..MATHEMATICAL SCRIPT CAPITAL D
unicode_alphabetic(0x1D4A2, 0x1D4A2).	% Alphabetic L&       MATHEMATICAL SCRIPT CAPITAL G
unicode_alphabetic(0x1D4A5, 0x1D4A6).	% Alphabetic L&   [2] MATHEMATICAL SCRIPT CAPITAL J..MATHEMATICAL SCRIPT CAPITAL K
unicode_alphabetic(0x1D4A9, 0x1D4AC).	% Alphabetic L&   [4] MATHEMATICAL SCRIPT CAPITAL N..MATHEMATICAL SCRIPT CAPITAL Q
unicode_alphabetic(0x1D4AE, 0x1D4B9).	% Alphabetic L&  [12] MATHEMATICAL SCRIPT CAPITAL S..MATHEMATICAL SCRIPT SMALL D
unicode_alphabetic(0x1D4BB, 0x1D4BB).	% Alphabetic L&       MATHEMATICAL SCRIPT SMALL F
unicode_alphabetic(0x1D4BD, 0x1D4C3).	% Alphabetic L&   [7] MATHEMATICAL SCRIPT SMALL H..MATHEMATICAL SCRIPT SMALL N
unicode_alphabetic(0x1D4C5, 0x1D505).	% Alphabetic L&  [65] MATHEMATICAL SCRIPT SMALL P..MATHEMATICAL FRAKTUR CAPITAL B
unicode_alphabetic(0x1D507, 0x1D50A).	% Alphabetic L&   [4] MATHEMATICAL FRAKTUR CAPITAL D..MATHEMATICAL FRAKTUR CAPITAL G
unicode_alphabetic(0x1D50D, 0x1D514).	% Alphabetic L&   [8] MATHEMATICAL FRAKTUR CAPITAL J..MATHEMATICAL FRAKTUR CAPITAL Q
unicode_alphabetic(0x1D516, 0x1D51C).	% Alphabetic L&   [7] MATHEMATICAL FRAKTUR CAPITAL S..MATHEMATICAL FRAKTUR CAPITAL Y
unicode_alphabetic(0x1D51E, 0x1D539).	% Alphabetic L&  [28] MATHEMATICAL FRAKTUR SMALL A..MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_alphabetic(0x1D53B, 0x1D53E).	% Alphabetic L&   [4] MATHEMATICAL DOUBLE-STRUCK CAPITAL D..MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_alphabetic(0x1D540, 0x1D544).	% Alphabetic L&   [5] MATHEMATICAL DOUBLE-STRUCK CAPITAL I..MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_alphabetic(0x1D546, 0x1D546).	% Alphabetic L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_alphabetic(0x1D54A, 0x1D550).	% Alphabetic L&   [7] MATHEMATICAL DOUBLE-STRUCK CAPITAL S..MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_alphabetic(0x1D552, 0x1D6A5).	% Alphabetic L& [340] MATHEMATICAL DOUBLE-STRUCK SMALL A..MATHEMATICAL ITALIC SMALL DOTLESS J
unicode_alphabetic(0x1D6A8, 0x1D6C0).	% Alphabetic L&  [25] MATHEMATICAL BOLD CAPITAL ALPHA..MATHEMATICAL BOLD CAPITAL OMEGA
unicode_alphabetic(0x1D6C2, 0x1D6DA).	% Alphabetic L&  [25] MATHEMATICAL BOLD SMALL ALPHA..MATHEMATICAL BOLD SMALL OMEGA
unicode_alphabetic(0x1D6DC, 0x1D6FA).	% Alphabetic L&  [31] MATHEMATICAL BOLD EPSILON SYMBOL..MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_alphabetic(0x1D6FC, 0x1D714).	% Alphabetic L&  [25] MATHEMATICAL ITALIC SMALL ALPHA..MATHEMATICAL ITALIC SMALL OMEGA
unicode_alphabetic(0x1D716, 0x1D734).	% Alphabetic L&  [31] MATHEMATICAL ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_alphabetic(0x1D736, 0x1D74E).	% Alphabetic L&  [25] MATHEMATICAL BOLD ITALIC SMALL ALPHA..MATHEMATICAL BOLD ITALIC SMALL OMEGA
unicode_alphabetic(0x1D750, 0x1D76E).	% Alphabetic L&  [31] MATHEMATICAL BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_alphabetic(0x1D770, 0x1D788).	% Alphabetic L&  [25] MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
unicode_alphabetic(0x1D78A, 0x1D7A8).	% Alphabetic L&  [31] MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_alphabetic(0x1D7AA, 0x1D7C2).	% Alphabetic L&  [25] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
unicode_alphabetic(0x1D7C4, 0x1D7CB).	% Alphabetic L&   [8] MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD SMALL DIGAMMA
unicode_alphabetic(0x1EE00, 0x1EE03).	% Alphabetic Lo   [4] ARABIC MATHEMATICAL ALEF..ARABIC MATHEMATICAL DAL
unicode_alphabetic(0x1EE05, 0x1EE1F).	% Alphabetic Lo  [27] ARABIC MATHEMATICAL WAW..ARABIC MATHEMATICAL DOTLESS QAF
unicode_alphabetic(0x1EE21, 0x1EE22).	% Alphabetic Lo   [2] ARABIC MATHEMATICAL INITIAL BEH..ARABIC MATHEMATICAL INITIAL JEEM
unicode_alphabetic(0x1EE24, 0x1EE24).	% Alphabetic Lo       ARABIC MATHEMATICAL INITIAL HEH
unicode_alphabetic(0x1EE27, 0x1EE27).	% Alphabetic Lo       ARABIC MATHEMATICAL INITIAL HAH
unicode_alphabetic(0x1EE29, 0x1EE32).	% Alphabetic Lo  [10] ARABIC MATHEMATICAL INITIAL YEH..ARABIC MATHEMATICAL INITIAL QAF
unicode_alphabetic(0x1EE34, 0x1EE37).	% Alphabetic Lo   [4] ARABIC MATHEMATICAL INITIAL SHEEN..ARABIC MATHEMATICAL INITIAL KHAH
unicode_alphabetic(0x1EE39, 0x1EE39).	% Alphabetic Lo       ARABIC MATHEMATICAL INITIAL DAD
unicode_alphabetic(0x1EE3B, 0x1EE3B).	% Alphabetic Lo       ARABIC MATHEMATICAL INITIAL GHAIN
unicode_alphabetic(0x1EE42, 0x1EE42).	% Alphabetic Lo       ARABIC MATHEMATICAL TAILED JEEM
unicode_alphabetic(0x1EE47, 0x1EE47).	% Alphabetic Lo       ARABIC MATHEMATICAL TAILED HAH
unicode_alphabetic(0x1EE49, 0x1EE49).	% Alphabetic Lo       ARABIC MATHEMATICAL TAILED YEH
unicode_alphabetic(0x1EE4B, 0x1EE4B).	% Alphabetic Lo       ARABIC MATHEMATICAL TAILED LAM
unicode_alphabetic(0x1EE4D, 0x1EE4F).	% Alphabetic Lo   [3] ARABIC MATHEMATICAL TAILED NOON..ARABIC MATHEMATICAL TAILED AIN
unicode_alphabetic(0x1EE51, 0x1EE52).	% Alphabetic Lo   [2] ARABIC MATHEMATICAL TAILED SAD..ARABIC MATHEMATICAL TAILED QAF
unicode_alphabetic(0x1EE54, 0x1EE54).	% Alphabetic Lo       ARABIC MATHEMATICAL TAILED SHEEN
unicode_alphabetic(0x1EE57, 0x1EE57).	% Alphabetic Lo       ARABIC MATHEMATICAL TAILED KHAH
unicode_alphabetic(0x1EE59, 0x1EE59).	% Alphabetic Lo       ARABIC MATHEMATICAL TAILED DAD
unicode_alphabetic(0x1EE5B, 0x1EE5B).	% Alphabetic Lo       ARABIC MATHEMATICAL TAILED GHAIN
unicode_alphabetic(0x1EE5D, 0x1EE5D).	% Alphabetic Lo       ARABIC MATHEMATICAL TAILED DOTLESS NOON
unicode_alphabetic(0x1EE5F, 0x1EE5F).	% Alphabetic Lo       ARABIC MATHEMATICAL TAILED DOTLESS QAF
unicode_alphabetic(0x1EE61, 0x1EE62).	% Alphabetic Lo   [2] ARABIC MATHEMATICAL STRETCHED BEH..ARABIC MATHEMATICAL STRETCHED JEEM
unicode_alphabetic(0x1EE64, 0x1EE64).	% Alphabetic Lo       ARABIC MATHEMATICAL STRETCHED HEH
unicode_alphabetic(0x1EE67, 0x1EE6A).	% Alphabetic Lo   [4] ARABIC MATHEMATICAL STRETCHED HAH..ARABIC MATHEMATICAL STRETCHED KAF
unicode_alphabetic(0x1EE6C, 0x1EE72).	% Alphabetic Lo   [7] ARABIC MATHEMATICAL STRETCHED MEEM..ARABIC MATHEMATICAL STRETCHED QAF
unicode_alphabetic(0x1EE74, 0x1EE77).	% Alphabetic Lo   [4] ARABIC MATHEMATICAL STRETCHED SHEEN..ARABIC MATHEMATICAL STRETCHED KHAH
unicode_alphabetic(0x1EE79, 0x1EE7C).	% Alphabetic Lo   [4] ARABIC MATHEMATICAL STRETCHED DAD..ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
unicode_alphabetic(0x1EE7E, 0x1EE7E).	% Alphabetic Lo       ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
unicode_alphabetic(0x1EE80, 0x1EE89).	% Alphabetic Lo  [10] ARABIC MATHEMATICAL LOOPED ALEF..ARABIC MATHEMATICAL LOOPED YEH
unicode_alphabetic(0x1EE8B, 0x1EE9B).	% Alphabetic Lo  [17] ARABIC MATHEMATICAL LOOPED LAM..ARABIC MATHEMATICAL LOOPED GHAIN
unicode_alphabetic(0x1EEA1, 0x1EEA3).	% Alphabetic Lo   [3] ARABIC MATHEMATICAL DOUBLE-STRUCK BEH..ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
unicode_alphabetic(0x1EEA5, 0x1EEA9).	% Alphabetic Lo   [5] ARABIC MATHEMATICAL DOUBLE-STRUCK WAW..ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
unicode_alphabetic(0x1EEAB, 0x1EEBB).	% Alphabetic Lo  [17] ARABIC MATHEMATICAL DOUBLE-STRUCK LAM..ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN
unicode_alphabetic(0x20000, 0x2A6D6).	% Alphabetic Lo [42711] CJK UNIFIED IDEOGRAPH-20000..CJK UNIFIED IDEOGRAPH-2A6D6
unicode_alphabetic(0x2A700, 0x2B734).	% Alphabetic Lo [4149] CJK UNIFIED IDEOGRAPH-2A700..CJK UNIFIED IDEOGRAPH-2B734
unicode_alphabetic(0x2B740, 0x2B81D).	% Alphabetic Lo [222] CJK UNIFIED IDEOGRAPH-2B740..CJK UNIFIED IDEOGRAPH-2B81D
unicode_alphabetic(0x2F800, 0x2FA1D).	% Alphabetic Lo [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D

% Total code points: 102159
