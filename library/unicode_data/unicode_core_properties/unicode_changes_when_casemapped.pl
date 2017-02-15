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

unicode_changes_when_casemapped(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_changes_when_casemapped(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_changes_when_casemapped(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_changes_when_casemapped(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).
% ================================================

% Derived Property:   Changes_When_Casemapped (CWCM)
%  Characters whose normalized forms are not stable under case mapping.
%  For more information, see D128 in Section 3.13, "Default Case Algorithms".
%  Changes_When_Casemapped(X) is true when CWL(X), or CWT(X), or CWU(X)

unicode_changes_when_casemapped(0x0041, 0x005A).	% Changes_When_Casemapped L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
unicode_changes_when_casemapped(0x0061, 0x007A).	% Changes_When_Casemapped L&  [26] LATIN SMALL LETTER A..LATIN SMALL LETTER Z
unicode_changes_when_casemapped(0x00B5, 0x00B5).	% Changes_When_Casemapped L&       MICRO SIGN
unicode_changes_when_casemapped(0x00C0, 0x00D6).	% Changes_When_Casemapped L&  [23] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_changes_when_casemapped(0x00D8, 0x00F6).	% Changes_When_Casemapped L&  [31] LATIN CAPITAL LETTER O WITH STROKE..LATIN SMALL LETTER O WITH DIAERESIS
unicode_changes_when_casemapped(0x00F8, 0x0137).	% Changes_When_Casemapped L&  [64] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER K WITH CEDILLA
unicode_changes_when_casemapped(0x0139, 0x018C).	% Changes_When_Casemapped L&  [84] LATIN CAPITAL LETTER L WITH ACUTE..LATIN SMALL LETTER D WITH TOPBAR
unicode_changes_when_casemapped(0x018E, 0x019A).	% Changes_When_Casemapped L&  [13] LATIN CAPITAL LETTER REVERSED E..LATIN SMALL LETTER L WITH BAR
unicode_changes_when_casemapped(0x019C, 0x01A9).	% Changes_When_Casemapped L&  [14] LATIN CAPITAL LETTER TURNED M..LATIN CAPITAL LETTER ESH
unicode_changes_when_casemapped(0x01AC, 0x01B9).	% Changes_When_Casemapped L&  [14] LATIN CAPITAL LETTER T WITH HOOK..LATIN SMALL LETTER EZH REVERSED
unicode_changes_when_casemapped(0x01BC, 0x01BD).	% Changes_When_Casemapped L&   [2] LATIN CAPITAL LETTER TONE FIVE..LATIN SMALL LETTER TONE FIVE
unicode_changes_when_casemapped(0x01BF, 0x01BF).	% Changes_When_Casemapped L&       LATIN LETTER WYNN
unicode_changes_when_casemapped(0x01C4, 0x0220).	% Changes_When_Casemapped L&  [93] LATIN CAPITAL LETTER DZ WITH CARON..LATIN CAPITAL LETTER N WITH LONG RIGHT LEG
unicode_changes_when_casemapped(0x0222, 0x0233).	% Changes_When_Casemapped L&  [18] LATIN CAPITAL LETTER OU..LATIN SMALL LETTER Y WITH MACRON
unicode_changes_when_casemapped(0x023A, 0x0254).	% Changes_When_Casemapped L&  [27] LATIN CAPITAL LETTER A WITH STROKE..LATIN SMALL LETTER OPEN O
unicode_changes_when_casemapped(0x0256, 0x0257).	% Changes_When_Casemapped L&   [2] LATIN SMALL LETTER D WITH TAIL..LATIN SMALL LETTER D WITH HOOK
unicode_changes_when_casemapped(0x0259, 0x0259).	% Changes_When_Casemapped L&       LATIN SMALL LETTER SCHWA
unicode_changes_when_casemapped(0x025B, 0x025B).	% Changes_When_Casemapped L&       LATIN SMALL LETTER OPEN E
unicode_changes_when_casemapped(0x0260, 0x0260).	% Changes_When_Casemapped L&       LATIN SMALL LETTER G WITH HOOK
unicode_changes_when_casemapped(0x0263, 0x0263).	% Changes_When_Casemapped L&       LATIN SMALL LETTER GAMMA
unicode_changes_when_casemapped(0x0265, 0x0266).	% Changes_When_Casemapped L&   [2] LATIN SMALL LETTER TURNED H..LATIN SMALL LETTER H WITH HOOK
unicode_changes_when_casemapped(0x0268, 0x0269).	% Changes_When_Casemapped L&   [2] LATIN SMALL LETTER I WITH STROKE..LATIN SMALL LETTER IOTA
unicode_changes_when_casemapped(0x026B, 0x026B).	% Changes_When_Casemapped L&       LATIN SMALL LETTER L WITH MIDDLE TILDE
unicode_changes_when_casemapped(0x026F, 0x026F).	% Changes_When_Casemapped L&       LATIN SMALL LETTER TURNED M
unicode_changes_when_casemapped(0x0271, 0x0272).	% Changes_When_Casemapped L&   [2] LATIN SMALL LETTER M WITH HOOK..LATIN SMALL LETTER N WITH LEFT HOOK
unicode_changes_when_casemapped(0x0275, 0x0275).	% Changes_When_Casemapped L&       LATIN SMALL LETTER BARRED O
unicode_changes_when_casemapped(0x027D, 0x027D).	% Changes_When_Casemapped L&       LATIN SMALL LETTER R WITH TAIL
unicode_changes_when_casemapped(0x0280, 0x0280).	% Changes_When_Casemapped L&       LATIN LETTER SMALL CAPITAL R
unicode_changes_when_casemapped(0x0283, 0x0283).	% Changes_When_Casemapped L&       LATIN SMALL LETTER ESH
unicode_changes_when_casemapped(0x0288, 0x028C).	% Changes_When_Casemapped L&   [5] LATIN SMALL LETTER T WITH RETROFLEX HOOK..LATIN SMALL LETTER TURNED V
unicode_changes_when_casemapped(0x0292, 0x0292).	% Changes_When_Casemapped L&       LATIN SMALL LETTER EZH
unicode_changes_when_casemapped(0x0345, 0x0345).	% Changes_When_Casemapped Mn       COMBINING GREEK YPOGEGRAMMENI
unicode_changes_when_casemapped(0x0370, 0x0373).	% Changes_When_Casemapped L&   [4] GREEK CAPITAL LETTER HETA..GREEK SMALL LETTER ARCHAIC SAMPI
unicode_changes_when_casemapped(0x0376, 0x0377).	% Changes_When_Casemapped L&   [2] GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA..GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
unicode_changes_when_casemapped(0x037B, 0x037D).	% Changes_When_Casemapped L&   [3] GREEK SMALL REVERSED LUNATE SIGMA SYMBOL..GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
unicode_changes_when_casemapped(0x0386, 0x0386).	% Changes_When_Casemapped L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_changes_when_casemapped(0x0388, 0x038A).	% Changes_When_Casemapped L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_changes_when_casemapped(0x038C, 0x038C).	% Changes_When_Casemapped L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_changes_when_casemapped(0x038E, 0x03A1).	% Changes_When_Casemapped L&  [20] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK CAPITAL LETTER RHO
unicode_changes_when_casemapped(0x03A3, 0x03D1).	% Changes_When_Casemapped L&  [47] GREEK CAPITAL LETTER SIGMA..GREEK THETA SYMBOL
unicode_changes_when_casemapped(0x03D5, 0x03F2).	% Changes_When_Casemapped L&  [30] GREEK PHI SYMBOL..GREEK LUNATE SIGMA SYMBOL
unicode_changes_when_casemapped(0x03F4, 0x03F5).	% Changes_When_Casemapped L&   [2] GREEK CAPITAL THETA SYMBOL..GREEK LUNATE EPSILON SYMBOL
unicode_changes_when_casemapped(0x03F7, 0x03FB).	% Changes_When_Casemapped L&   [5] GREEK CAPITAL LETTER SHO..GREEK SMALL LETTER SAN
unicode_changes_when_casemapped(0x03FD, 0x0481).	% Changes_When_Casemapped L& [133] GREEK CAPITAL REVERSED LUNATE SIGMA SYMBOL..CYRILLIC SMALL LETTER KOPPA
unicode_changes_when_casemapped(0x048A, 0x0527).	% Changes_When_Casemapped L& [158] CYRILLIC CAPITAL LETTER SHORT I WITH TAIL..CYRILLIC SMALL LETTER SHHA WITH DESCENDER
unicode_changes_when_casemapped(0x0531, 0x0556).	% Changes_When_Casemapped L&  [38] ARMENIAN CAPITAL LETTER AYB..ARMENIAN CAPITAL LETTER FEH
unicode_changes_when_casemapped(0x0561, 0x0587).	% Changes_When_Casemapped L&  [39] ARMENIAN SMALL LETTER AYB..ARMENIAN SMALL LIGATURE ECH YIWN
unicode_changes_when_casemapped(0x10A0, 0x10C5).	% Changes_When_Casemapped L&  [38] GEORGIAN CAPITAL LETTER AN..GEORGIAN CAPITAL LETTER HOE
unicode_changes_when_casemapped(0x10C7, 0x10C7).	% Changes_When_Casemapped L&       GEORGIAN CAPITAL LETTER YN
unicode_changes_when_casemapped(0x10CD, 0x10CD).	% Changes_When_Casemapped L&       GEORGIAN CAPITAL LETTER AEN
unicode_changes_when_casemapped(0x1D79, 0x1D79).	% Changes_When_Casemapped L&       LATIN SMALL LETTER INSULAR G
unicode_changes_when_casemapped(0x1D7D, 0x1D7D).	% Changes_When_Casemapped L&       LATIN SMALL LETTER P WITH STROKE
unicode_changes_when_casemapped(0x1E00, 0x1E9B).	% Changes_When_Casemapped L& [156] LATIN CAPITAL LETTER A WITH RING BELOW..LATIN SMALL LETTER LONG S WITH DOT ABOVE
unicode_changes_when_casemapped(0x1E9E, 0x1E9E).	% Changes_When_Casemapped L&       LATIN CAPITAL LETTER SHARP S
unicode_changes_when_casemapped(0x1EA0, 0x1F15).	% Changes_When_Casemapped L& [118] LATIN CAPITAL LETTER A WITH DOT BELOW..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_changes_when_casemapped(0x1F18, 0x1F1D).	% Changes_When_Casemapped L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_changes_when_casemapped(0x1F20, 0x1F45).	% Changes_When_Casemapped L&  [38] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_changes_when_casemapped(0x1F48, 0x1F4D).	% Changes_When_Casemapped L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_changes_when_casemapped(0x1F50, 0x1F57).	% Changes_When_Casemapped L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_changes_when_casemapped(0x1F59, 0x1F59).	% Changes_When_Casemapped L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_changes_when_casemapped(0x1F5B, 0x1F5B).	% Changes_When_Casemapped L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_changes_when_casemapped(0x1F5D, 0x1F5D).	% Changes_When_Casemapped L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_changes_when_casemapped(0x1F5F, 0x1F7D).	% Changes_When_Casemapped L&  [31] GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_changes_when_casemapped(0x1F80, 0x1FB4).	% Changes_When_Casemapped L&  [53] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_changes_when_casemapped(0x1FB6, 0x1FBC).	% Changes_When_Casemapped L&   [7] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_changes_when_casemapped(0x1FBE, 0x1FBE).	% Changes_When_Casemapped L&       GREEK PROSGEGRAMMENI
unicode_changes_when_casemapped(0x1FC2, 0x1FC4).	% Changes_When_Casemapped L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_changes_when_casemapped(0x1FC6, 0x1FCC).	% Changes_When_Casemapped L&   [7] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_changes_when_casemapped(0x1FD0, 0x1FD3).	% Changes_When_Casemapped L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_changes_when_casemapped(0x1FD6, 0x1FDB).	% Changes_When_Casemapped L&   [6] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_changes_when_casemapped(0x1FE0, 0x1FEC).	% Changes_When_Casemapped L&  [13] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_changes_when_casemapped(0x1FF2, 0x1FF4).	% Changes_When_Casemapped L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_changes_when_casemapped(0x1FF6, 0x1FFC).	% Changes_When_Casemapped L&   [7] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_changes_when_casemapped(0x2126, 0x2126).	% Changes_When_Casemapped L&       OHM SIGN
unicode_changes_when_casemapped(0x212A, 0x212B).	% Changes_When_Casemapped L&   [2] KELVIN SIGN..ANGSTROM SIGN
unicode_changes_when_casemapped(0x2132, 0x2132).	% Changes_When_Casemapped L&       TURNED CAPITAL F
unicode_changes_when_casemapped(0x214E, 0x214E).	% Changes_When_Casemapped L&       TURNED SMALL F
unicode_changes_when_casemapped(0x2160, 0x217F).	% Changes_When_Casemapped Nl  [32] ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL ONE THOUSAND
unicode_changes_when_casemapped(0x2183, 0x2184).	% Changes_When_Casemapped L&   [2] ROMAN NUMERAL REVERSED ONE HUNDRED..LATIN SMALL LETTER REVERSED C
unicode_changes_when_casemapped(0x24B6, 0x24E9).	% Changes_When_Casemapped So  [52] CIRCLED LATIN CAPITAL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_changes_when_casemapped(0x2C00, 0x2C2E).	% Changes_When_Casemapped L&  [47] GLAGOLITIC CAPITAL LETTER AZU..GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_changes_when_casemapped(0x2C30, 0x2C5E).	% Changes_When_Casemapped L&  [47] GLAGOLITIC SMALL LETTER AZU..GLAGOLITIC SMALL LETTER LATINATE MYSLITE
unicode_changes_when_casemapped(0x2C60, 0x2C70).	% Changes_When_Casemapped L&  [17] LATIN CAPITAL LETTER L WITH DOUBLE BAR..LATIN CAPITAL LETTER TURNED ALPHA
unicode_changes_when_casemapped(0x2C72, 0x2C73).	% Changes_When_Casemapped L&   [2] LATIN CAPITAL LETTER W WITH HOOK..LATIN SMALL LETTER W WITH HOOK
unicode_changes_when_casemapped(0x2C75, 0x2C76).	% Changes_When_Casemapped L&   [2] LATIN CAPITAL LETTER HALF H..LATIN SMALL LETTER HALF H
unicode_changes_when_casemapped(0x2C7E, 0x2CE3).	% Changes_When_Casemapped L& [102] LATIN CAPITAL LETTER S WITH SWASH TAIL..COPTIC SMALL LETTER OLD NUBIAN WAU
unicode_changes_when_casemapped(0x2CEB, 0x2CEE).	% Changes_When_Casemapped L&   [4] COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI..COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
unicode_changes_when_casemapped(0x2CF2, 0x2CF3).	% Changes_When_Casemapped L&   [2] COPTIC CAPITAL LETTER BOHAIRIC KHEI..COPTIC SMALL LETTER BOHAIRIC KHEI
unicode_changes_when_casemapped(0x2D00, 0x2D25).	% Changes_When_Casemapped L&  [38] GEORGIAN SMALL LETTER AN..GEORGIAN SMALL LETTER HOE
unicode_changes_when_casemapped(0x2D27, 0x2D27).	% Changes_When_Casemapped L&       GEORGIAN SMALL LETTER YN
unicode_changes_when_casemapped(0x2D2D, 0x2D2D).	% Changes_When_Casemapped L&       GEORGIAN SMALL LETTER AEN
unicode_changes_when_casemapped(0xA640, 0xA66D).	% Changes_When_Casemapped L&  [46] CYRILLIC CAPITAL LETTER ZEMLYA..CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
unicode_changes_when_casemapped(0xA680, 0xA697).	% Changes_When_Casemapped L&  [24] CYRILLIC CAPITAL LETTER DWE..CYRILLIC SMALL LETTER SHWE
unicode_changes_when_casemapped(0xA722, 0xA72F).	% Changes_When_Casemapped L&  [14] LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF..LATIN SMALL LETTER CUATRILLO WITH COMMA
unicode_changes_when_casemapped(0xA732, 0xA76F).	% Changes_When_Casemapped L&  [62] LATIN CAPITAL LETTER AA..LATIN SMALL LETTER CON
unicode_changes_when_casemapped(0xA779, 0xA787).	% Changes_When_Casemapped L&  [15] LATIN CAPITAL LETTER INSULAR D..LATIN SMALL LETTER INSULAR T
unicode_changes_when_casemapped(0xA78B, 0xA78D).	% Changes_When_Casemapped L&   [3] LATIN CAPITAL LETTER SALTILLO..LATIN CAPITAL LETTER TURNED H
unicode_changes_when_casemapped(0xA790, 0xA793).	% Changes_When_Casemapped L&   [4] LATIN CAPITAL LETTER N WITH DESCENDER..LATIN SMALL LETTER C WITH BAR
unicode_changes_when_casemapped(0xA7A0, 0xA7AA).	% Changes_When_Casemapped L&  [11] LATIN CAPITAL LETTER G WITH OBLIQUE STROKE..LATIN CAPITAL LETTER H WITH HOOK
unicode_changes_when_casemapped(0xFB00, 0xFB06).	% Changes_When_Casemapped L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_changes_when_casemapped(0xFB13, 0xFB17).	% Changes_When_Casemapped L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_changes_when_casemapped(0xFF21, 0xFF3A).	% Changes_When_Casemapped L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_changes_when_casemapped(0xFF41, 0xFF5A).	% Changes_When_Casemapped L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_changes_when_casemapped(0x10400, 0x1044F).	% Changes_When_Casemapped L&  [80] DESERET CAPITAL LETTER LONG I..DESERET SMALL LETTER EW

% Total code points: 2138
