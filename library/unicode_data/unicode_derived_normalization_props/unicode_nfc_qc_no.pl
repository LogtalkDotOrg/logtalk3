%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 27, 2012
%
%  Original Unicode file header comments follow

/*
# DerivedNormalizationProps-6.1.0.txt
# Date: 2011-07-26, 04:18:07 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
*/

unicode_nfc_qc_no(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_nfc_qc_no(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_nfc_qc_no(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_nfc_qc_no(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Property:	NFC_Quick_Check

%  All code points not explicitly listed for NFC_Quick_Check
%  have the value Yes (Y).

% @missing: 0000..10FFFF; NFC_QC; Yes

% ================================================

% NFC_Quick_Check=No

unicode_nfc_qc_no(0x0340, 0x0341).	% Mn   [2] COMBINING GRAVE TONE MARK..COMBINING ACUTE TONE MARK
unicode_nfc_qc_no(0x0343, 0x0344).	% Mn   [2] COMBINING GREEK KORONIS..COMBINING GREEK DIALYTIKA TONOS
unicode_nfc_qc_no(0x0374, 0x0374).	% Lm       GREEK NUMERAL SIGN
unicode_nfc_qc_no(0x037E, 0x037E).	% Po       GREEK QUESTION MARK
unicode_nfc_qc_no(0x0387, 0x0387).	% Po       GREEK ANO TELEIA
unicode_nfc_qc_no(0x0958, 0x095F).	% Lo   [8] DEVANAGARI LETTER QA..DEVANAGARI LETTER YYA
unicode_nfc_qc_no(0x09DC, 0x09DD).	% Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_nfc_qc_no(0x09DF, 0x09DF).	% Lo       BENGALI LETTER YYA
unicode_nfc_qc_no(0x0A33, 0x0A33).	% Lo       GURMUKHI LETTER LLA
unicode_nfc_qc_no(0x0A36, 0x0A36).	% Lo       GURMUKHI LETTER SHA
unicode_nfc_qc_no(0x0A59, 0x0A5B).	% Lo   [3] GURMUKHI LETTER KHHA..GURMUKHI LETTER ZA
unicode_nfc_qc_no(0x0A5E, 0x0A5E).	% Lo       GURMUKHI LETTER FA
unicode_nfc_qc_no(0x0B5C, 0x0B5D).	% Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_nfc_qc_no(0x0F43, 0x0F43).	% Lo       TIBETAN LETTER GHA
unicode_nfc_qc_no(0x0F4D, 0x0F4D).	% Lo       TIBETAN LETTER DDHA
unicode_nfc_qc_no(0x0F52, 0x0F52).	% Lo       TIBETAN LETTER DHA
unicode_nfc_qc_no(0x0F57, 0x0F57).	% Lo       TIBETAN LETTER BHA
unicode_nfc_qc_no(0x0F5C, 0x0F5C).	% Lo       TIBETAN LETTER DZHA
unicode_nfc_qc_no(0x0F69, 0x0F69).	% Lo       TIBETAN LETTER KSSA
unicode_nfc_qc_no(0x0F73, 0x0F73).	% Mn       TIBETAN VOWEL SIGN II
unicode_nfc_qc_no(0x0F75, 0x0F76).	% Mn   [2] TIBETAN VOWEL SIGN UU..TIBETAN VOWEL SIGN VOCALIC R
unicode_nfc_qc_no(0x0F78, 0x0F78).	% Mn       TIBETAN VOWEL SIGN VOCALIC L
unicode_nfc_qc_no(0x0F81, 0x0F81).	% Mn       TIBETAN VOWEL SIGN REVERSED II
unicode_nfc_qc_no(0x0F93, 0x0F93).	% Mn       TIBETAN SUBJOINED LETTER GHA
unicode_nfc_qc_no(0x0F9D, 0x0F9D).	% Mn       TIBETAN SUBJOINED LETTER DDHA
unicode_nfc_qc_no(0x0FA2, 0x0FA2).	% Mn       TIBETAN SUBJOINED LETTER DHA
unicode_nfc_qc_no(0x0FA7, 0x0FA7).	% Mn       TIBETAN SUBJOINED LETTER BHA
unicode_nfc_qc_no(0x0FAC, 0x0FAC).	% Mn       TIBETAN SUBJOINED LETTER DZHA
unicode_nfc_qc_no(0x0FB9, 0x0FB9).	% Mn       TIBETAN SUBJOINED LETTER KSSA
unicode_nfc_qc_no(0x1F71, 0x1F71).	% L&       GREEK SMALL LETTER ALPHA WITH OXIA
unicode_nfc_qc_no(0x1F73, 0x1F73).	% L&       GREEK SMALL LETTER EPSILON WITH OXIA
unicode_nfc_qc_no(0x1F75, 0x1F75).	% L&       GREEK SMALL LETTER ETA WITH OXIA
unicode_nfc_qc_no(0x1F77, 0x1F77).	% L&       GREEK SMALL LETTER IOTA WITH OXIA
unicode_nfc_qc_no(0x1F79, 0x1F79).	% L&       GREEK SMALL LETTER OMICRON WITH OXIA
unicode_nfc_qc_no(0x1F7B, 0x1F7B).	% L&       GREEK SMALL LETTER UPSILON WITH OXIA
unicode_nfc_qc_no(0x1F7D, 0x1F7D).	% L&       GREEK SMALL LETTER OMEGA WITH OXIA
unicode_nfc_qc_no(0x1FBB, 0x1FBB).	% L&       GREEK CAPITAL LETTER ALPHA WITH OXIA
unicode_nfc_qc_no(0x1FBE, 0x1FBE).	% L&       GREEK PROSGEGRAMMENI
unicode_nfc_qc_no(0x1FC9, 0x1FC9).	% L&       GREEK CAPITAL LETTER EPSILON WITH OXIA
unicode_nfc_qc_no(0x1FCB, 0x1FCB).	% L&       GREEK CAPITAL LETTER ETA WITH OXIA
unicode_nfc_qc_no(0x1FD3, 0x1FD3).	% L&       GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_nfc_qc_no(0x1FDB, 0x1FDB).	% L&       GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_nfc_qc_no(0x1FE3, 0x1FE3).	% L&       GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
unicode_nfc_qc_no(0x1FEB, 0x1FEB).	% L&       GREEK CAPITAL LETTER UPSILON WITH OXIA
unicode_nfc_qc_no(0x1FEE, 0x1FEF).	% Sk   [2] GREEK DIALYTIKA AND OXIA..GREEK VARIA
unicode_nfc_qc_no(0x1FF9, 0x1FF9).	% L&       GREEK CAPITAL LETTER OMICRON WITH OXIA
unicode_nfc_qc_no(0x1FFB, 0x1FFB).	% L&       GREEK CAPITAL LETTER OMEGA WITH OXIA
unicode_nfc_qc_no(0x1FFD, 0x1FFD).	% Sk       GREEK OXIA
unicode_nfc_qc_no(0x2000, 0x2001).	% Zs   [2] EN QUAD..EM QUAD
unicode_nfc_qc_no(0x2126, 0x2126).	% L&       OHM SIGN
unicode_nfc_qc_no(0x212A, 0x212B).	% L&   [2] KELVIN SIGN..ANGSTROM SIGN
unicode_nfc_qc_no(0x2329, 0x2329).	% Ps       LEFT-POINTING ANGLE BRACKET
unicode_nfc_qc_no(0x232A, 0x232A).	% Pe       RIGHT-POINTING ANGLE BRACKET
unicode_nfc_qc_no(0x2ADC, 0x2ADC).	% Sm       FORKING
unicode_nfc_qc_no(0xF900, 0xFA0D).	% Lo [270] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA0D
unicode_nfc_qc_no(0xFA10, 0xFA10).	% Lo       CJK COMPATIBILITY IDEOGRAPH-FA10
unicode_nfc_qc_no(0xFA12, 0xFA12).	% Lo       CJK COMPATIBILITY IDEOGRAPH-FA12
unicode_nfc_qc_no(0xFA15, 0xFA1E).	% Lo  [10] CJK COMPATIBILITY IDEOGRAPH-FA15..CJK COMPATIBILITY IDEOGRAPH-FA1E
unicode_nfc_qc_no(0xFA20, 0xFA20).	% Lo       CJK COMPATIBILITY IDEOGRAPH-FA20
unicode_nfc_qc_no(0xFA22, 0xFA22).	% Lo       CJK COMPATIBILITY IDEOGRAPH-FA22
unicode_nfc_qc_no(0xFA25, 0xFA26).	% Lo   [2] CJK COMPATIBILITY IDEOGRAPH-FA25..CJK COMPATIBILITY IDEOGRAPH-FA26
unicode_nfc_qc_no(0xFA2A, 0xFA6D).	% Lo  [68] CJK COMPATIBILITY IDEOGRAPH-FA2A..CJK COMPATIBILITY IDEOGRAPH-FA6D
unicode_nfc_qc_no(0xFA70, 0xFAD9).	% Lo [106] CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9
unicode_nfc_qc_no(0xFB1D, 0xFB1D).	% Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_nfc_qc_no(0xFB1F, 0xFB1F).	% Lo       HEBREW LIGATURE YIDDISH YOD YOD PATAH
unicode_nfc_qc_no(0xFB2A, 0xFB36).	% Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_nfc_qc_no(0xFB38, 0xFB3C).	% Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_nfc_qc_no(0xFB3E, 0xFB3E).	% Lo       HEBREW LETTER MEM WITH DAGESH
unicode_nfc_qc_no(0xFB40, 0xFB41).	% Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_nfc_qc_no(0xFB43, 0xFB44).	% Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_nfc_qc_no(0xFB46, 0xFB4E).	% Lo   [9] HEBREW LETTER TSADI WITH DAGESH..HEBREW LETTER PE WITH RAFE
unicode_nfc_qc_no(0x1D15E, 0x1D164).	% So   [7] MUSICAL SYMBOL HALF NOTE..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
unicode_nfc_qc_no(0x1D1BB, 0x1D1C0).	% So   [6] MUSICAL SYMBOL MINIMA..MUSICAL SYMBOL FUSA BLACK
unicode_nfc_qc_no(0x2F800, 0x2FA1D).	% Lo [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D

% Total code points: 1120
