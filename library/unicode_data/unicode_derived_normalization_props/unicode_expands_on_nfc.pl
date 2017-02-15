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

unicode_expands_on_nfc(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_expands_on_nfc(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_expands_on_nfc(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_expands_on_nfc(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property: Expands_On_NFC (DEPRECATED as of Unicode 6.0.0)
%   Generated according to UAX #15.
%   Characters whose normalized length is not one.
%   WARNING: Normalization of STRINGS must use the algorithm in UAX #15 because characters may interact.
%            The length of a normalized string is not necessarily the sum of the lengths of the normalized characters!

unicode_expands_on_nfc(0x0344, 0x0344).	% Mn       COMBINING GREEK DIALYTIKA TONOS
unicode_expands_on_nfc(0x0958, 0x095F).	% Lo   [8] DEVANAGARI LETTER QA..DEVANAGARI LETTER YYA
unicode_expands_on_nfc(0x09DC, 0x09DD).	% Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_expands_on_nfc(0x09DF, 0x09DF).	% Lo       BENGALI LETTER YYA
unicode_expands_on_nfc(0x0A33, 0x0A33).	% Lo       GURMUKHI LETTER LLA
unicode_expands_on_nfc(0x0A36, 0x0A36).	% Lo       GURMUKHI LETTER SHA
unicode_expands_on_nfc(0x0A59, 0x0A5B).	% Lo   [3] GURMUKHI LETTER KHHA..GURMUKHI LETTER ZA
unicode_expands_on_nfc(0x0A5E, 0x0A5E).	% Lo       GURMUKHI LETTER FA
unicode_expands_on_nfc(0x0B5C, 0x0B5D).	% Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_expands_on_nfc(0x0F43, 0x0F43).	% Lo       TIBETAN LETTER GHA
unicode_expands_on_nfc(0x0F4D, 0x0F4D).	% Lo       TIBETAN LETTER DDHA
unicode_expands_on_nfc(0x0F52, 0x0F52).	% Lo       TIBETAN LETTER DHA
unicode_expands_on_nfc(0x0F57, 0x0F57).	% Lo       TIBETAN LETTER BHA
unicode_expands_on_nfc(0x0F5C, 0x0F5C).	% Lo       TIBETAN LETTER DZHA
unicode_expands_on_nfc(0x0F69, 0x0F69).	% Lo       TIBETAN LETTER KSSA
unicode_expands_on_nfc(0x0F73, 0x0F73).	% Mn       TIBETAN VOWEL SIGN II
unicode_expands_on_nfc(0x0F75, 0x0F76).	% Mn   [2] TIBETAN VOWEL SIGN UU..TIBETAN VOWEL SIGN VOCALIC R
unicode_expands_on_nfc(0x0F78, 0x0F78).	% Mn       TIBETAN VOWEL SIGN VOCALIC L
unicode_expands_on_nfc(0x0F81, 0x0F81).	% Mn       TIBETAN VOWEL SIGN REVERSED II
unicode_expands_on_nfc(0x0F93, 0x0F93).	% Mn       TIBETAN SUBJOINED LETTER GHA
unicode_expands_on_nfc(0x0F9D, 0x0F9D).	% Mn       TIBETAN SUBJOINED LETTER DDHA
unicode_expands_on_nfc(0x0FA2, 0x0FA2).	% Mn       TIBETAN SUBJOINED LETTER DHA
unicode_expands_on_nfc(0x0FA7, 0x0FA7).	% Mn       TIBETAN SUBJOINED LETTER BHA
unicode_expands_on_nfc(0x0FAC, 0x0FAC).	% Mn       TIBETAN SUBJOINED LETTER DZHA
unicode_expands_on_nfc(0x0FB9, 0x0FB9).	% Mn       TIBETAN SUBJOINED LETTER KSSA
unicode_expands_on_nfc(0x2ADC, 0x2ADC).	% Sm       FORKING
unicode_expands_on_nfc(0xFB1D, 0xFB1D).	% Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_expands_on_nfc(0xFB1F, 0xFB1F).	% Lo       HEBREW LIGATURE YIDDISH YOD YOD PATAH
unicode_expands_on_nfc(0xFB2A, 0xFB36).	% Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_expands_on_nfc(0xFB38, 0xFB3C).	% Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_expands_on_nfc(0xFB3E, 0xFB3E).	% Lo       HEBREW LETTER MEM WITH DAGESH
unicode_expands_on_nfc(0xFB40, 0xFB41).	% Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_expands_on_nfc(0xFB43, 0xFB44).	% Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_expands_on_nfc(0xFB46, 0xFB4E).	% Lo   [9] HEBREW LETTER TSADI WITH DAGESH..HEBREW LETTER PE WITH RAFE
unicode_expands_on_nfc(0x1D15E, 0x1D164).	% So   [7] MUSICAL SYMBOL HALF NOTE..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
unicode_expands_on_nfc(0x1D1BB, 0x1D1C0).	% So   [6] MUSICAL SYMBOL MINIMA..MUSICAL SYMBOL FUSA BLACK

% Total code points: 85
