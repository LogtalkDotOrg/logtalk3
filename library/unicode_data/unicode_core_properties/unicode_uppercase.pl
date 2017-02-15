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

unicode_uppercase(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_uppercase(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_uppercase(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_uppercase(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property: Uppercase
%  Generated from: Lu + Other_Uppercase

unicode_uppercase(0x0041, 0x005A).	% Uppercase L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
unicode_uppercase(0x00C0, 0x00D6).	% Uppercase L&  [23] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_uppercase(0x00D8, 0x00DE).	% Uppercase L&   [7] LATIN CAPITAL LETTER O WITH STROKE..LATIN CAPITAL LETTER THORN
unicode_uppercase(0x0100, 0x0100).	% Uppercase L&       LATIN CAPITAL LETTER A WITH MACRON
unicode_uppercase(0x0102, 0x0102).	% Uppercase L&       LATIN CAPITAL LETTER A WITH BREVE
unicode_uppercase(0x0104, 0x0104).	% Uppercase L&       LATIN CAPITAL LETTER A WITH OGONEK
unicode_uppercase(0x0106, 0x0106).	% Uppercase L&       LATIN CAPITAL LETTER C WITH ACUTE
unicode_uppercase(0x0108, 0x0108).	% Uppercase L&       LATIN CAPITAL LETTER C WITH CIRCUMFLEX
unicode_uppercase(0x010A, 0x010A).	% Uppercase L&       LATIN CAPITAL LETTER C WITH DOT ABOVE
unicode_uppercase(0x010C, 0x010C).	% Uppercase L&       LATIN CAPITAL LETTER C WITH CARON
unicode_uppercase(0x010E, 0x010E).	% Uppercase L&       LATIN CAPITAL LETTER D WITH CARON
unicode_uppercase(0x0110, 0x0110).	% Uppercase L&       LATIN CAPITAL LETTER D WITH STROKE
unicode_uppercase(0x0112, 0x0112).	% Uppercase L&       LATIN CAPITAL LETTER E WITH MACRON
unicode_uppercase(0x0114, 0x0114).	% Uppercase L&       LATIN CAPITAL LETTER E WITH BREVE
unicode_uppercase(0x0116, 0x0116).	% Uppercase L&       LATIN CAPITAL LETTER E WITH DOT ABOVE
unicode_uppercase(0x0118, 0x0118).	% Uppercase L&       LATIN CAPITAL LETTER E WITH OGONEK
unicode_uppercase(0x011A, 0x011A).	% Uppercase L&       LATIN CAPITAL LETTER E WITH CARON
unicode_uppercase(0x011C, 0x011C).	% Uppercase L&       LATIN CAPITAL LETTER G WITH CIRCUMFLEX
unicode_uppercase(0x011E, 0x011E).	% Uppercase L&       LATIN CAPITAL LETTER G WITH BREVE
unicode_uppercase(0x0120, 0x0120).	% Uppercase L&       LATIN CAPITAL LETTER G WITH DOT ABOVE
unicode_uppercase(0x0122, 0x0122).	% Uppercase L&       LATIN CAPITAL LETTER G WITH CEDILLA
unicode_uppercase(0x0124, 0x0124).	% Uppercase L&       LATIN CAPITAL LETTER H WITH CIRCUMFLEX
unicode_uppercase(0x0126, 0x0126).	% Uppercase L&       LATIN CAPITAL LETTER H WITH STROKE
unicode_uppercase(0x0128, 0x0128).	% Uppercase L&       LATIN CAPITAL LETTER I WITH TILDE
unicode_uppercase(0x012A, 0x012A).	% Uppercase L&       LATIN CAPITAL LETTER I WITH MACRON
unicode_uppercase(0x012C, 0x012C).	% Uppercase L&       LATIN CAPITAL LETTER I WITH BREVE
unicode_uppercase(0x012E, 0x012E).	% Uppercase L&       LATIN CAPITAL LETTER I WITH OGONEK
unicode_uppercase(0x0130, 0x0130).	% Uppercase L&       LATIN CAPITAL LETTER I WITH DOT ABOVE
unicode_uppercase(0x0132, 0x0132).	% Uppercase L&       LATIN CAPITAL LIGATURE IJ
unicode_uppercase(0x0134, 0x0134).	% Uppercase L&       LATIN CAPITAL LETTER J WITH CIRCUMFLEX
unicode_uppercase(0x0136, 0x0136).	% Uppercase L&       LATIN CAPITAL LETTER K WITH CEDILLA
unicode_uppercase(0x0139, 0x0139).	% Uppercase L&       LATIN CAPITAL LETTER L WITH ACUTE
unicode_uppercase(0x013B, 0x013B).	% Uppercase L&       LATIN CAPITAL LETTER L WITH CEDILLA
unicode_uppercase(0x013D, 0x013D).	% Uppercase L&       LATIN CAPITAL LETTER L WITH CARON
unicode_uppercase(0x013F, 0x013F).	% Uppercase L&       LATIN CAPITAL LETTER L WITH MIDDLE DOT
unicode_uppercase(0x0141, 0x0141).	% Uppercase L&       LATIN CAPITAL LETTER L WITH STROKE
unicode_uppercase(0x0143, 0x0143).	% Uppercase L&       LATIN CAPITAL LETTER N WITH ACUTE
unicode_uppercase(0x0145, 0x0145).	% Uppercase L&       LATIN CAPITAL LETTER N WITH CEDILLA
unicode_uppercase(0x0147, 0x0147).	% Uppercase L&       LATIN CAPITAL LETTER N WITH CARON
unicode_uppercase(0x014A, 0x014A).	% Uppercase L&       LATIN CAPITAL LETTER ENG
unicode_uppercase(0x014C, 0x014C).	% Uppercase L&       LATIN CAPITAL LETTER O WITH MACRON
unicode_uppercase(0x014E, 0x014E).	% Uppercase L&       LATIN CAPITAL LETTER O WITH BREVE
unicode_uppercase(0x0150, 0x0150).	% Uppercase L&       LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
unicode_uppercase(0x0152, 0x0152).	% Uppercase L&       LATIN CAPITAL LIGATURE OE
unicode_uppercase(0x0154, 0x0154).	% Uppercase L&       LATIN CAPITAL LETTER R WITH ACUTE
unicode_uppercase(0x0156, 0x0156).	% Uppercase L&       LATIN CAPITAL LETTER R WITH CEDILLA
unicode_uppercase(0x0158, 0x0158).	% Uppercase L&       LATIN CAPITAL LETTER R WITH CARON
unicode_uppercase(0x015A, 0x015A).	% Uppercase L&       LATIN CAPITAL LETTER S WITH ACUTE
unicode_uppercase(0x015C, 0x015C).	% Uppercase L&       LATIN CAPITAL LETTER S WITH CIRCUMFLEX
unicode_uppercase(0x015E, 0x015E).	% Uppercase L&       LATIN CAPITAL LETTER S WITH CEDILLA
unicode_uppercase(0x0160, 0x0160).	% Uppercase L&       LATIN CAPITAL LETTER S WITH CARON
unicode_uppercase(0x0162, 0x0162).	% Uppercase L&       LATIN CAPITAL LETTER T WITH CEDILLA
unicode_uppercase(0x0164, 0x0164).	% Uppercase L&       LATIN CAPITAL LETTER T WITH CARON
unicode_uppercase(0x0166, 0x0166).	% Uppercase L&       LATIN CAPITAL LETTER T WITH STROKE
unicode_uppercase(0x0168, 0x0168).	% Uppercase L&       LATIN CAPITAL LETTER U WITH TILDE
unicode_uppercase(0x016A, 0x016A).	% Uppercase L&       LATIN CAPITAL LETTER U WITH MACRON
unicode_uppercase(0x016C, 0x016C).	% Uppercase L&       LATIN CAPITAL LETTER U WITH BREVE
unicode_uppercase(0x016E, 0x016E).	% Uppercase L&       LATIN CAPITAL LETTER U WITH RING ABOVE
unicode_uppercase(0x0170, 0x0170).	% Uppercase L&       LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
unicode_uppercase(0x0172, 0x0172).	% Uppercase L&       LATIN CAPITAL LETTER U WITH OGONEK
unicode_uppercase(0x0174, 0x0174).	% Uppercase L&       LATIN CAPITAL LETTER W WITH CIRCUMFLEX
unicode_uppercase(0x0176, 0x0176).	% Uppercase L&       LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
unicode_uppercase(0x0178, 0x0179).	% Uppercase L&   [2] LATIN CAPITAL LETTER Y WITH DIAERESIS..LATIN CAPITAL LETTER Z WITH ACUTE
unicode_uppercase(0x017B, 0x017B).	% Uppercase L&       LATIN CAPITAL LETTER Z WITH DOT ABOVE
unicode_uppercase(0x017D, 0x017D).	% Uppercase L&       LATIN CAPITAL LETTER Z WITH CARON
unicode_uppercase(0x0181, 0x0182).	% Uppercase L&   [2] LATIN CAPITAL LETTER B WITH HOOK..LATIN CAPITAL LETTER B WITH TOPBAR
unicode_uppercase(0x0184, 0x0184).	% Uppercase L&       LATIN CAPITAL LETTER TONE SIX
unicode_uppercase(0x0186, 0x0187).	% Uppercase L&   [2] LATIN CAPITAL LETTER OPEN O..LATIN CAPITAL LETTER C WITH HOOK
unicode_uppercase(0x0189, 0x018B).	% Uppercase L&   [3] LATIN CAPITAL LETTER AFRICAN D..LATIN CAPITAL LETTER D WITH TOPBAR
unicode_uppercase(0x018E, 0x0191).	% Uppercase L&   [4] LATIN CAPITAL LETTER REVERSED E..LATIN CAPITAL LETTER F WITH HOOK
unicode_uppercase(0x0193, 0x0194).	% Uppercase L&   [2] LATIN CAPITAL LETTER G WITH HOOK..LATIN CAPITAL LETTER GAMMA
unicode_uppercase(0x0196, 0x0198).	% Uppercase L&   [3] LATIN CAPITAL LETTER IOTA..LATIN CAPITAL LETTER K WITH HOOK
unicode_uppercase(0x019C, 0x019D).	% Uppercase L&   [2] LATIN CAPITAL LETTER TURNED M..LATIN CAPITAL LETTER N WITH LEFT HOOK
unicode_uppercase(0x019F, 0x01A0).	% Uppercase L&   [2] LATIN CAPITAL LETTER O WITH MIDDLE TILDE..LATIN CAPITAL LETTER O WITH HORN
unicode_uppercase(0x01A2, 0x01A2).	% Uppercase L&       LATIN CAPITAL LETTER OI
unicode_uppercase(0x01A4, 0x01A4).	% Uppercase L&       LATIN CAPITAL LETTER P WITH HOOK
unicode_uppercase(0x01A6, 0x01A7).	% Uppercase L&   [2] LATIN LETTER YR..LATIN CAPITAL LETTER TONE TWO
unicode_uppercase(0x01A9, 0x01A9).	% Uppercase L&       LATIN CAPITAL LETTER ESH
unicode_uppercase(0x01AC, 0x01AC).	% Uppercase L&       LATIN CAPITAL LETTER T WITH HOOK
unicode_uppercase(0x01AE, 0x01AF).	% Uppercase L&   [2] LATIN CAPITAL LETTER T WITH RETROFLEX HOOK..LATIN CAPITAL LETTER U WITH HORN
unicode_uppercase(0x01B1, 0x01B3).	% Uppercase L&   [3] LATIN CAPITAL LETTER UPSILON..LATIN CAPITAL LETTER Y WITH HOOK
unicode_uppercase(0x01B5, 0x01B5).	% Uppercase L&       LATIN CAPITAL LETTER Z WITH STROKE
unicode_uppercase(0x01B7, 0x01B8).	% Uppercase L&   [2] LATIN CAPITAL LETTER EZH..LATIN CAPITAL LETTER EZH REVERSED
unicode_uppercase(0x01BC, 0x01BC).	% Uppercase L&       LATIN CAPITAL LETTER TONE FIVE
unicode_uppercase(0x01C4, 0x01C4).	% Uppercase L&       LATIN CAPITAL LETTER DZ WITH CARON
unicode_uppercase(0x01C7, 0x01C7).	% Uppercase L&       LATIN CAPITAL LETTER LJ
unicode_uppercase(0x01CA, 0x01CA).	% Uppercase L&       LATIN CAPITAL LETTER NJ
unicode_uppercase(0x01CD, 0x01CD).	% Uppercase L&       LATIN CAPITAL LETTER A WITH CARON
unicode_uppercase(0x01CF, 0x01CF).	% Uppercase L&       LATIN CAPITAL LETTER I WITH CARON
unicode_uppercase(0x01D1, 0x01D1).	% Uppercase L&       LATIN CAPITAL LETTER O WITH CARON
unicode_uppercase(0x01D3, 0x01D3).	% Uppercase L&       LATIN CAPITAL LETTER U WITH CARON
unicode_uppercase(0x01D5, 0x01D5).	% Uppercase L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
unicode_uppercase(0x01D7, 0x01D7).	% Uppercase L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
unicode_uppercase(0x01D9, 0x01D9).	% Uppercase L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
unicode_uppercase(0x01DB, 0x01DB).	% Uppercase L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
unicode_uppercase(0x01DE, 0x01DE).	% Uppercase L&       LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
unicode_uppercase(0x01E0, 0x01E0).	% Uppercase L&       LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
unicode_uppercase(0x01E2, 0x01E2).	% Uppercase L&       LATIN CAPITAL LETTER AE WITH MACRON
unicode_uppercase(0x01E4, 0x01E4).	% Uppercase L&       LATIN CAPITAL LETTER G WITH STROKE
unicode_uppercase(0x01E6, 0x01E6).	% Uppercase L&       LATIN CAPITAL LETTER G WITH CARON
unicode_uppercase(0x01E8, 0x01E8).	% Uppercase L&       LATIN CAPITAL LETTER K WITH CARON
unicode_uppercase(0x01EA, 0x01EA).	% Uppercase L&       LATIN CAPITAL LETTER O WITH OGONEK
unicode_uppercase(0x01EC, 0x01EC).	% Uppercase L&       LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
unicode_uppercase(0x01EE, 0x01EE).	% Uppercase L&       LATIN CAPITAL LETTER EZH WITH CARON
unicode_uppercase(0x01F1, 0x01F1).	% Uppercase L&       LATIN CAPITAL LETTER DZ
unicode_uppercase(0x01F4, 0x01F4).	% Uppercase L&       LATIN CAPITAL LETTER G WITH ACUTE
unicode_uppercase(0x01F6, 0x01F8).	% Uppercase L&   [3] LATIN CAPITAL LETTER HWAIR..LATIN CAPITAL LETTER N WITH GRAVE
unicode_uppercase(0x01FA, 0x01FA).	% Uppercase L&       LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
unicode_uppercase(0x01FC, 0x01FC).	% Uppercase L&       LATIN CAPITAL LETTER AE WITH ACUTE
unicode_uppercase(0x01FE, 0x01FE).	% Uppercase L&       LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
unicode_uppercase(0x0200, 0x0200).	% Uppercase L&       LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
unicode_uppercase(0x0202, 0x0202).	% Uppercase L&       LATIN CAPITAL LETTER A WITH INVERTED BREVE
unicode_uppercase(0x0204, 0x0204).	% Uppercase L&       LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
unicode_uppercase(0x0206, 0x0206).	% Uppercase L&       LATIN CAPITAL LETTER E WITH INVERTED BREVE
unicode_uppercase(0x0208, 0x0208).	% Uppercase L&       LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
unicode_uppercase(0x020A, 0x020A).	% Uppercase L&       LATIN CAPITAL LETTER I WITH INVERTED BREVE
unicode_uppercase(0x020C, 0x020C).	% Uppercase L&       LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
unicode_uppercase(0x020E, 0x020E).	% Uppercase L&       LATIN CAPITAL LETTER O WITH INVERTED BREVE
unicode_uppercase(0x0210, 0x0210).	% Uppercase L&       LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
unicode_uppercase(0x0212, 0x0212).	% Uppercase L&       LATIN CAPITAL LETTER R WITH INVERTED BREVE
unicode_uppercase(0x0214, 0x0214).	% Uppercase L&       LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
unicode_uppercase(0x0216, 0x0216).	% Uppercase L&       LATIN CAPITAL LETTER U WITH INVERTED BREVE
unicode_uppercase(0x0218, 0x0218).	% Uppercase L&       LATIN CAPITAL LETTER S WITH COMMA BELOW
unicode_uppercase(0x021A, 0x021A).	% Uppercase L&       LATIN CAPITAL LETTER T WITH COMMA BELOW
unicode_uppercase(0x021C, 0x021C).	% Uppercase L&       LATIN CAPITAL LETTER YOGH
unicode_uppercase(0x021E, 0x021E).	% Uppercase L&       LATIN CAPITAL LETTER H WITH CARON
unicode_uppercase(0x0220, 0x0220).	% Uppercase L&       LATIN CAPITAL LETTER N WITH LONG RIGHT LEG
unicode_uppercase(0x0222, 0x0222).	% Uppercase L&       LATIN CAPITAL LETTER OU
unicode_uppercase(0x0224, 0x0224).	% Uppercase L&       LATIN CAPITAL LETTER Z WITH HOOK
unicode_uppercase(0x0226, 0x0226).	% Uppercase L&       LATIN CAPITAL LETTER A WITH DOT ABOVE
unicode_uppercase(0x0228, 0x0228).	% Uppercase L&       LATIN CAPITAL LETTER E WITH CEDILLA
unicode_uppercase(0x022A, 0x022A).	% Uppercase L&       LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
unicode_uppercase(0x022C, 0x022C).	% Uppercase L&       LATIN CAPITAL LETTER O WITH TILDE AND MACRON
unicode_uppercase(0x022E, 0x022E).	% Uppercase L&       LATIN CAPITAL LETTER O WITH DOT ABOVE
unicode_uppercase(0x0230, 0x0230).	% Uppercase L&       LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
unicode_uppercase(0x0232, 0x0232).	% Uppercase L&       LATIN CAPITAL LETTER Y WITH MACRON
unicode_uppercase(0x023A, 0x023B).	% Uppercase L&   [2] LATIN CAPITAL LETTER A WITH STROKE..LATIN CAPITAL LETTER C WITH STROKE
unicode_uppercase(0x023D, 0x023E).	% Uppercase L&   [2] LATIN CAPITAL LETTER L WITH BAR..LATIN CAPITAL LETTER T WITH DIAGONAL STROKE
unicode_uppercase(0x0241, 0x0241).	% Uppercase L&       LATIN CAPITAL LETTER GLOTTAL STOP
unicode_uppercase(0x0243, 0x0246).	% Uppercase L&   [4] LATIN CAPITAL LETTER B WITH STROKE..LATIN CAPITAL LETTER E WITH STROKE
unicode_uppercase(0x0248, 0x0248).	% Uppercase L&       LATIN CAPITAL LETTER J WITH STROKE
unicode_uppercase(0x024A, 0x024A).	% Uppercase L&       LATIN CAPITAL LETTER SMALL Q WITH HOOK TAIL
unicode_uppercase(0x024C, 0x024C).	% Uppercase L&       LATIN CAPITAL LETTER R WITH STROKE
unicode_uppercase(0x024E, 0x024E).	% Uppercase L&       LATIN CAPITAL LETTER Y WITH STROKE
unicode_uppercase(0x0370, 0x0370).	% Uppercase L&       GREEK CAPITAL LETTER HETA
unicode_uppercase(0x0372, 0x0372).	% Uppercase L&       GREEK CAPITAL LETTER ARCHAIC SAMPI
unicode_uppercase(0x0376, 0x0376).	% Uppercase L&       GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA
unicode_uppercase(0x0386, 0x0386).	% Uppercase L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_uppercase(0x0388, 0x038A).	% Uppercase L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_uppercase(0x038C, 0x038C).	% Uppercase L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_uppercase(0x038E, 0x038F).	% Uppercase L&   [2] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK CAPITAL LETTER OMEGA WITH TONOS
unicode_uppercase(0x0391, 0x03A1).	% Uppercase L&  [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
unicode_uppercase(0x03A3, 0x03AB).	% Uppercase L&   [9] GREEK CAPITAL LETTER SIGMA..GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
unicode_uppercase(0x03CF, 0x03CF).	% Uppercase L&       GREEK CAPITAL KAI SYMBOL
unicode_uppercase(0x03D2, 0x03D4).	% Uppercase L&   [3] GREEK UPSILON WITH HOOK SYMBOL..GREEK UPSILON WITH DIAERESIS AND HOOK SYMBOL
unicode_uppercase(0x03D8, 0x03D8).	% Uppercase L&       GREEK LETTER ARCHAIC KOPPA
unicode_uppercase(0x03DA, 0x03DA).	% Uppercase L&       GREEK LETTER STIGMA
unicode_uppercase(0x03DC, 0x03DC).	% Uppercase L&       GREEK LETTER DIGAMMA
unicode_uppercase(0x03DE, 0x03DE).	% Uppercase L&       GREEK LETTER KOPPA
unicode_uppercase(0x03E0, 0x03E0).	% Uppercase L&       GREEK LETTER SAMPI
unicode_uppercase(0x03E2, 0x03E2).	% Uppercase L&       COPTIC CAPITAL LETTER SHEI
unicode_uppercase(0x03E4, 0x03E4).	% Uppercase L&       COPTIC CAPITAL LETTER FEI
unicode_uppercase(0x03E6, 0x03E6).	% Uppercase L&       COPTIC CAPITAL LETTER KHEI
unicode_uppercase(0x03E8, 0x03E8).	% Uppercase L&       COPTIC CAPITAL LETTER HORI
unicode_uppercase(0x03EA, 0x03EA).	% Uppercase L&       COPTIC CAPITAL LETTER GANGIA
unicode_uppercase(0x03EC, 0x03EC).	% Uppercase L&       COPTIC CAPITAL LETTER SHIMA
unicode_uppercase(0x03EE, 0x03EE).	% Uppercase L&       COPTIC CAPITAL LETTER DEI
unicode_uppercase(0x03F4, 0x03F4).	% Uppercase L&       GREEK CAPITAL THETA SYMBOL
unicode_uppercase(0x03F7, 0x03F7).	% Uppercase L&       GREEK CAPITAL LETTER SHO
unicode_uppercase(0x03F9, 0x03FA).	% Uppercase L&   [2] GREEK CAPITAL LUNATE SIGMA SYMBOL..GREEK CAPITAL LETTER SAN
unicode_uppercase(0x03FD, 0x042F).	% Uppercase L&  [51] GREEK CAPITAL REVERSED LUNATE SIGMA SYMBOL..CYRILLIC CAPITAL LETTER YA
unicode_uppercase(0x0460, 0x0460).	% Uppercase L&       CYRILLIC CAPITAL LETTER OMEGA
unicode_uppercase(0x0462, 0x0462).	% Uppercase L&       CYRILLIC CAPITAL LETTER YAT
unicode_uppercase(0x0464, 0x0464).	% Uppercase L&       CYRILLIC CAPITAL LETTER IOTIFIED E
unicode_uppercase(0x0466, 0x0466).	% Uppercase L&       CYRILLIC CAPITAL LETTER LITTLE YUS
unicode_uppercase(0x0468, 0x0468).	% Uppercase L&       CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
unicode_uppercase(0x046A, 0x046A).	% Uppercase L&       CYRILLIC CAPITAL LETTER BIG YUS
unicode_uppercase(0x046C, 0x046C).	% Uppercase L&       CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
unicode_uppercase(0x046E, 0x046E).	% Uppercase L&       CYRILLIC CAPITAL LETTER KSI
unicode_uppercase(0x0470, 0x0470).	% Uppercase L&       CYRILLIC CAPITAL LETTER PSI
unicode_uppercase(0x0472, 0x0472).	% Uppercase L&       CYRILLIC CAPITAL LETTER FITA
unicode_uppercase(0x0474, 0x0474).	% Uppercase L&       CYRILLIC CAPITAL LETTER IZHITSA
unicode_uppercase(0x0476, 0x0476).	% Uppercase L&       CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
unicode_uppercase(0x0478, 0x0478).	% Uppercase L&       CYRILLIC CAPITAL LETTER UK
unicode_uppercase(0x047A, 0x047A).	% Uppercase L&       CYRILLIC CAPITAL LETTER ROUND OMEGA
unicode_uppercase(0x047C, 0x047C).	% Uppercase L&       CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
unicode_uppercase(0x047E, 0x047E).	% Uppercase L&       CYRILLIC CAPITAL LETTER OT
unicode_uppercase(0x0480, 0x0480).	% Uppercase L&       CYRILLIC CAPITAL LETTER KOPPA
unicode_uppercase(0x048A, 0x048A).	% Uppercase L&       CYRILLIC CAPITAL LETTER SHORT I WITH TAIL
unicode_uppercase(0x048C, 0x048C).	% Uppercase L&       CYRILLIC CAPITAL LETTER SEMISOFT SIGN
unicode_uppercase(0x048E, 0x048E).	% Uppercase L&       CYRILLIC CAPITAL LETTER ER WITH TICK
unicode_uppercase(0x0490, 0x0490).	% Uppercase L&       CYRILLIC CAPITAL LETTER GHE WITH UPTURN
unicode_uppercase(0x0492, 0x0492).	% Uppercase L&       CYRILLIC CAPITAL LETTER GHE WITH STROKE
unicode_uppercase(0x0494, 0x0494).	% Uppercase L&       CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
unicode_uppercase(0x0496, 0x0496).	% Uppercase L&       CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
unicode_uppercase(0x0498, 0x0498).	% Uppercase L&       CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
unicode_uppercase(0x049A, 0x049A).	% Uppercase L&       CYRILLIC CAPITAL LETTER KA WITH DESCENDER
unicode_uppercase(0x049C, 0x049C).	% Uppercase L&       CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
unicode_uppercase(0x049E, 0x049E).	% Uppercase L&       CYRILLIC CAPITAL LETTER KA WITH STROKE
unicode_uppercase(0x04A0, 0x04A0).	% Uppercase L&       CYRILLIC CAPITAL LETTER BASHKIR KA
unicode_uppercase(0x04A2, 0x04A2).	% Uppercase L&       CYRILLIC CAPITAL LETTER EN WITH DESCENDER
unicode_uppercase(0x04A4, 0x04A4).	% Uppercase L&       CYRILLIC CAPITAL LIGATURE EN GHE
unicode_uppercase(0x04A6, 0x04A6).	% Uppercase L&       CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK
unicode_uppercase(0x04A8, 0x04A8).	% Uppercase L&       CYRILLIC CAPITAL LETTER ABKHASIAN HA
unicode_uppercase(0x04AA, 0x04AA).	% Uppercase L&       CYRILLIC CAPITAL LETTER ES WITH DESCENDER
unicode_uppercase(0x04AC, 0x04AC).	% Uppercase L&       CYRILLIC CAPITAL LETTER TE WITH DESCENDER
unicode_uppercase(0x04AE, 0x04AE).	% Uppercase L&       CYRILLIC CAPITAL LETTER STRAIGHT U
unicode_uppercase(0x04B0, 0x04B0).	% Uppercase L&       CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
unicode_uppercase(0x04B2, 0x04B2).	% Uppercase L&       CYRILLIC CAPITAL LETTER HA WITH DESCENDER
unicode_uppercase(0x04B4, 0x04B4).	% Uppercase L&       CYRILLIC CAPITAL LIGATURE TE TSE
unicode_uppercase(0x04B6, 0x04B6).	% Uppercase L&       CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
unicode_uppercase(0x04B8, 0x04B8).	% Uppercase L&       CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
unicode_uppercase(0x04BA, 0x04BA).	% Uppercase L&       CYRILLIC CAPITAL LETTER SHHA
unicode_uppercase(0x04BC, 0x04BC).	% Uppercase L&       CYRILLIC CAPITAL LETTER ABKHASIAN CHE
unicode_uppercase(0x04BE, 0x04BE).	% Uppercase L&       CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
unicode_uppercase(0x04C0, 0x04C1).	% Uppercase L&   [2] CYRILLIC LETTER PALOCHKA..CYRILLIC CAPITAL LETTER ZHE WITH BREVE
unicode_uppercase(0x04C3, 0x04C3).	% Uppercase L&       CYRILLIC CAPITAL LETTER KA WITH HOOK
unicode_uppercase(0x04C5, 0x04C5).	% Uppercase L&       CYRILLIC CAPITAL LETTER EL WITH TAIL
unicode_uppercase(0x04C7, 0x04C7).	% Uppercase L&       CYRILLIC CAPITAL LETTER EN WITH HOOK
unicode_uppercase(0x04C9, 0x04C9).	% Uppercase L&       CYRILLIC CAPITAL LETTER EN WITH TAIL
unicode_uppercase(0x04CB, 0x04CB).	% Uppercase L&       CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
unicode_uppercase(0x04CD, 0x04CD).	% Uppercase L&       CYRILLIC CAPITAL LETTER EM WITH TAIL
unicode_uppercase(0x04D0, 0x04D0).	% Uppercase L&       CYRILLIC CAPITAL LETTER A WITH BREVE
unicode_uppercase(0x04D2, 0x04D2).	% Uppercase L&       CYRILLIC CAPITAL LETTER A WITH DIAERESIS
unicode_uppercase(0x04D4, 0x04D4).	% Uppercase L&       CYRILLIC CAPITAL LIGATURE A IE
unicode_uppercase(0x04D6, 0x04D6).	% Uppercase L&       CYRILLIC CAPITAL LETTER IE WITH BREVE
unicode_uppercase(0x04D8, 0x04D8).	% Uppercase L&       CYRILLIC CAPITAL LETTER SCHWA
unicode_uppercase(0x04DA, 0x04DA).	% Uppercase L&       CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
unicode_uppercase(0x04DC, 0x04DC).	% Uppercase L&       CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
unicode_uppercase(0x04DE, 0x04DE).	% Uppercase L&       CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
unicode_uppercase(0x04E0, 0x04E0).	% Uppercase L&       CYRILLIC CAPITAL LETTER ABKHASIAN DZE
unicode_uppercase(0x04E2, 0x04E2).	% Uppercase L&       CYRILLIC CAPITAL LETTER I WITH MACRON
unicode_uppercase(0x04E4, 0x04E4).	% Uppercase L&       CYRILLIC CAPITAL LETTER I WITH DIAERESIS
unicode_uppercase(0x04E6, 0x04E6).	% Uppercase L&       CYRILLIC CAPITAL LETTER O WITH DIAERESIS
unicode_uppercase(0x04E8, 0x04E8).	% Uppercase L&       CYRILLIC CAPITAL LETTER BARRED O
unicode_uppercase(0x04EA, 0x04EA).	% Uppercase L&       CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
unicode_uppercase(0x04EC, 0x04EC).	% Uppercase L&       CYRILLIC CAPITAL LETTER E WITH DIAERESIS
unicode_uppercase(0x04EE, 0x04EE).	% Uppercase L&       CYRILLIC CAPITAL LETTER U WITH MACRON
unicode_uppercase(0x04F0, 0x04F0).	% Uppercase L&       CYRILLIC CAPITAL LETTER U WITH DIAERESIS
unicode_uppercase(0x04F2, 0x04F2).	% Uppercase L&       CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
unicode_uppercase(0x04F4, 0x04F4).	% Uppercase L&       CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
unicode_uppercase(0x04F6, 0x04F6).	% Uppercase L&       CYRILLIC CAPITAL LETTER GHE WITH DESCENDER
unicode_uppercase(0x04F8, 0x04F8).	% Uppercase L&       CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
unicode_uppercase(0x04FA, 0x04FA).	% Uppercase L&       CYRILLIC CAPITAL LETTER GHE WITH STROKE AND HOOK
unicode_uppercase(0x04FC, 0x04FC).	% Uppercase L&       CYRILLIC CAPITAL LETTER HA WITH HOOK
unicode_uppercase(0x04FE, 0x04FE).	% Uppercase L&       CYRILLIC CAPITAL LETTER HA WITH STROKE
unicode_uppercase(0x0500, 0x0500).	% Uppercase L&       CYRILLIC CAPITAL LETTER KOMI DE
unicode_uppercase(0x0502, 0x0502).	% Uppercase L&       CYRILLIC CAPITAL LETTER KOMI DJE
unicode_uppercase(0x0504, 0x0504).	% Uppercase L&       CYRILLIC CAPITAL LETTER KOMI ZJE
unicode_uppercase(0x0506, 0x0506).	% Uppercase L&       CYRILLIC CAPITAL LETTER KOMI DZJE
unicode_uppercase(0x0508, 0x0508).	% Uppercase L&       CYRILLIC CAPITAL LETTER KOMI LJE
unicode_uppercase(0x050A, 0x050A).	% Uppercase L&       CYRILLIC CAPITAL LETTER KOMI NJE
unicode_uppercase(0x050C, 0x050C).	% Uppercase L&       CYRILLIC CAPITAL LETTER KOMI SJE
unicode_uppercase(0x050E, 0x050E).	% Uppercase L&       CYRILLIC CAPITAL LETTER KOMI TJE
unicode_uppercase(0x0510, 0x0510).	% Uppercase L&       CYRILLIC CAPITAL LETTER REVERSED ZE
unicode_uppercase(0x0512, 0x0512).	% Uppercase L&       CYRILLIC CAPITAL LETTER EL WITH HOOK
unicode_uppercase(0x0514, 0x0514).	% Uppercase L&       CYRILLIC CAPITAL LETTER LHA
unicode_uppercase(0x0516, 0x0516).	% Uppercase L&       CYRILLIC CAPITAL LETTER RHA
unicode_uppercase(0x0518, 0x0518).	% Uppercase L&       CYRILLIC CAPITAL LETTER YAE
unicode_uppercase(0x051A, 0x051A).	% Uppercase L&       CYRILLIC CAPITAL LETTER QA
unicode_uppercase(0x051C, 0x051C).	% Uppercase L&       CYRILLIC CAPITAL LETTER WE
unicode_uppercase(0x051E, 0x051E).	% Uppercase L&       CYRILLIC CAPITAL LETTER ALEUT KA
unicode_uppercase(0x0520, 0x0520).	% Uppercase L&       CYRILLIC CAPITAL LETTER EL WITH MIDDLE HOOK
unicode_uppercase(0x0522, 0x0522).	% Uppercase L&       CYRILLIC CAPITAL LETTER EN WITH MIDDLE HOOK
unicode_uppercase(0x0524, 0x0524).	% Uppercase L&       CYRILLIC CAPITAL LETTER PE WITH DESCENDER
unicode_uppercase(0x0526, 0x0526).	% Uppercase L&       CYRILLIC CAPITAL LETTER SHHA WITH DESCENDER
unicode_uppercase(0x0531, 0x0556).	% Uppercase L&  [38] ARMENIAN CAPITAL LETTER AYB..ARMENIAN CAPITAL LETTER FEH
unicode_uppercase(0x10A0, 0x10C5).	% Uppercase L&  [38] GEORGIAN CAPITAL LETTER AN..GEORGIAN CAPITAL LETTER HOE
unicode_uppercase(0x10C7, 0x10C7).	% Uppercase L&       GEORGIAN CAPITAL LETTER YN
unicode_uppercase(0x10CD, 0x10CD).	% Uppercase L&       GEORGIAN CAPITAL LETTER AEN
unicode_uppercase(0x1E00, 0x1E00).	% Uppercase L&       LATIN CAPITAL LETTER A WITH RING BELOW
unicode_uppercase(0x1E02, 0x1E02).	% Uppercase L&       LATIN CAPITAL LETTER B WITH DOT ABOVE
unicode_uppercase(0x1E04, 0x1E04).	% Uppercase L&       LATIN CAPITAL LETTER B WITH DOT BELOW
unicode_uppercase(0x1E06, 0x1E06).	% Uppercase L&       LATIN CAPITAL LETTER B WITH LINE BELOW
unicode_uppercase(0x1E08, 0x1E08).	% Uppercase L&       LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
unicode_uppercase(0x1E0A, 0x1E0A).	% Uppercase L&       LATIN CAPITAL LETTER D WITH DOT ABOVE
unicode_uppercase(0x1E0C, 0x1E0C).	% Uppercase L&       LATIN CAPITAL LETTER D WITH DOT BELOW
unicode_uppercase(0x1E0E, 0x1E0E).	% Uppercase L&       LATIN CAPITAL LETTER D WITH LINE BELOW
unicode_uppercase(0x1E10, 0x1E10).	% Uppercase L&       LATIN CAPITAL LETTER D WITH CEDILLA
unicode_uppercase(0x1E12, 0x1E12).	% Uppercase L&       LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
unicode_uppercase(0x1E14, 0x1E14).	% Uppercase L&       LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
unicode_uppercase(0x1E16, 0x1E16).	% Uppercase L&       LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
unicode_uppercase(0x1E18, 0x1E18).	% Uppercase L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
unicode_uppercase(0x1E1A, 0x1E1A).	% Uppercase L&       LATIN CAPITAL LETTER E WITH TILDE BELOW
unicode_uppercase(0x1E1C, 0x1E1C).	% Uppercase L&       LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
unicode_uppercase(0x1E1E, 0x1E1E).	% Uppercase L&       LATIN CAPITAL LETTER F WITH DOT ABOVE
unicode_uppercase(0x1E20, 0x1E20).	% Uppercase L&       LATIN CAPITAL LETTER G WITH MACRON
unicode_uppercase(0x1E22, 0x1E22).	% Uppercase L&       LATIN CAPITAL LETTER H WITH DOT ABOVE
unicode_uppercase(0x1E24, 0x1E24).	% Uppercase L&       LATIN CAPITAL LETTER H WITH DOT BELOW
unicode_uppercase(0x1E26, 0x1E26).	% Uppercase L&       LATIN CAPITAL LETTER H WITH DIAERESIS
unicode_uppercase(0x1E28, 0x1E28).	% Uppercase L&       LATIN CAPITAL LETTER H WITH CEDILLA
unicode_uppercase(0x1E2A, 0x1E2A).	% Uppercase L&       LATIN CAPITAL LETTER H WITH BREVE BELOW
unicode_uppercase(0x1E2C, 0x1E2C).	% Uppercase L&       LATIN CAPITAL LETTER I WITH TILDE BELOW
unicode_uppercase(0x1E2E, 0x1E2E).	% Uppercase L&       LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
unicode_uppercase(0x1E30, 0x1E30).	% Uppercase L&       LATIN CAPITAL LETTER K WITH ACUTE
unicode_uppercase(0x1E32, 0x1E32).	% Uppercase L&       LATIN CAPITAL LETTER K WITH DOT BELOW
unicode_uppercase(0x1E34, 0x1E34).	% Uppercase L&       LATIN CAPITAL LETTER K WITH LINE BELOW
unicode_uppercase(0x1E36, 0x1E36).	% Uppercase L&       LATIN CAPITAL LETTER L WITH DOT BELOW
unicode_uppercase(0x1E38, 0x1E38).	% Uppercase L&       LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
unicode_uppercase(0x1E3A, 0x1E3A).	% Uppercase L&       LATIN CAPITAL LETTER L WITH LINE BELOW
unicode_uppercase(0x1E3C, 0x1E3C).	% Uppercase L&       LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
unicode_uppercase(0x1E3E, 0x1E3E).	% Uppercase L&       LATIN CAPITAL LETTER M WITH ACUTE
unicode_uppercase(0x1E40, 0x1E40).	% Uppercase L&       LATIN CAPITAL LETTER M WITH DOT ABOVE
unicode_uppercase(0x1E42, 0x1E42).	% Uppercase L&       LATIN CAPITAL LETTER M WITH DOT BELOW
unicode_uppercase(0x1E44, 0x1E44).	% Uppercase L&       LATIN CAPITAL LETTER N WITH DOT ABOVE
unicode_uppercase(0x1E46, 0x1E46).	% Uppercase L&       LATIN CAPITAL LETTER N WITH DOT BELOW
unicode_uppercase(0x1E48, 0x1E48).	% Uppercase L&       LATIN CAPITAL LETTER N WITH LINE BELOW
unicode_uppercase(0x1E4A, 0x1E4A).	% Uppercase L&       LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
unicode_uppercase(0x1E4C, 0x1E4C).	% Uppercase L&       LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
unicode_uppercase(0x1E4E, 0x1E4E).	% Uppercase L&       LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
unicode_uppercase(0x1E50, 0x1E50).	% Uppercase L&       LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
unicode_uppercase(0x1E52, 0x1E52).	% Uppercase L&       LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
unicode_uppercase(0x1E54, 0x1E54).	% Uppercase L&       LATIN CAPITAL LETTER P WITH ACUTE
unicode_uppercase(0x1E56, 0x1E56).	% Uppercase L&       LATIN CAPITAL LETTER P WITH DOT ABOVE
unicode_uppercase(0x1E58, 0x1E58).	% Uppercase L&       LATIN CAPITAL LETTER R WITH DOT ABOVE
unicode_uppercase(0x1E5A, 0x1E5A).	% Uppercase L&       LATIN CAPITAL LETTER R WITH DOT BELOW
unicode_uppercase(0x1E5C, 0x1E5C).	% Uppercase L&       LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
unicode_uppercase(0x1E5E, 0x1E5E).	% Uppercase L&       LATIN CAPITAL LETTER R WITH LINE BELOW
unicode_uppercase(0x1E60, 0x1E60).	% Uppercase L&       LATIN CAPITAL LETTER S WITH DOT ABOVE
unicode_uppercase(0x1E62, 0x1E62).	% Uppercase L&       LATIN CAPITAL LETTER S WITH DOT BELOW
unicode_uppercase(0x1E64, 0x1E64).	% Uppercase L&       LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
unicode_uppercase(0x1E66, 0x1E66).	% Uppercase L&       LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
unicode_uppercase(0x1E68, 0x1E68).	% Uppercase L&       LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
unicode_uppercase(0x1E6A, 0x1E6A).	% Uppercase L&       LATIN CAPITAL LETTER T WITH DOT ABOVE
unicode_uppercase(0x1E6C, 0x1E6C).	% Uppercase L&       LATIN CAPITAL LETTER T WITH DOT BELOW
unicode_uppercase(0x1E6E, 0x1E6E).	% Uppercase L&       LATIN CAPITAL LETTER T WITH LINE BELOW
unicode_uppercase(0x1E70, 0x1E70).	% Uppercase L&       LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
unicode_uppercase(0x1E72, 0x1E72).	% Uppercase L&       LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
unicode_uppercase(0x1E74, 0x1E74).	% Uppercase L&       LATIN CAPITAL LETTER U WITH TILDE BELOW
unicode_uppercase(0x1E76, 0x1E76).	% Uppercase L&       LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
unicode_uppercase(0x1E78, 0x1E78).	% Uppercase L&       LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
unicode_uppercase(0x1E7A, 0x1E7A).	% Uppercase L&       LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
unicode_uppercase(0x1E7C, 0x1E7C).	% Uppercase L&       LATIN CAPITAL LETTER V WITH TILDE
unicode_uppercase(0x1E7E, 0x1E7E).	% Uppercase L&       LATIN CAPITAL LETTER V WITH DOT BELOW
unicode_uppercase(0x1E80, 0x1E80).	% Uppercase L&       LATIN CAPITAL LETTER W WITH GRAVE
unicode_uppercase(0x1E82, 0x1E82).	% Uppercase L&       LATIN CAPITAL LETTER W WITH ACUTE
unicode_uppercase(0x1E84, 0x1E84).	% Uppercase L&       LATIN CAPITAL LETTER W WITH DIAERESIS
unicode_uppercase(0x1E86, 0x1E86).	% Uppercase L&       LATIN CAPITAL LETTER W WITH DOT ABOVE
unicode_uppercase(0x1E88, 0x1E88).	% Uppercase L&       LATIN CAPITAL LETTER W WITH DOT BELOW
unicode_uppercase(0x1E8A, 0x1E8A).	% Uppercase L&       LATIN CAPITAL LETTER X WITH DOT ABOVE
unicode_uppercase(0x1E8C, 0x1E8C).	% Uppercase L&       LATIN CAPITAL LETTER X WITH DIAERESIS
unicode_uppercase(0x1E8E, 0x1E8E).	% Uppercase L&       LATIN CAPITAL LETTER Y WITH DOT ABOVE
unicode_uppercase(0x1E90, 0x1E90).	% Uppercase L&       LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
unicode_uppercase(0x1E92, 0x1E92).	% Uppercase L&       LATIN CAPITAL LETTER Z WITH DOT BELOW
unicode_uppercase(0x1E94, 0x1E94).	% Uppercase L&       LATIN CAPITAL LETTER Z WITH LINE BELOW
unicode_uppercase(0x1E9E, 0x1E9E).	% Uppercase L&       LATIN CAPITAL LETTER SHARP S
unicode_uppercase(0x1EA0, 0x1EA0).	% Uppercase L&       LATIN CAPITAL LETTER A WITH DOT BELOW
unicode_uppercase(0x1EA2, 0x1EA2).	% Uppercase L&       LATIN CAPITAL LETTER A WITH HOOK ABOVE
unicode_uppercase(0x1EA4, 0x1EA4).	% Uppercase L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
unicode_uppercase(0x1EA6, 0x1EA6).	% Uppercase L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
unicode_uppercase(0x1EA8, 0x1EA8).	% Uppercase L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
unicode_uppercase(0x1EAA, 0x1EAA).	% Uppercase L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
unicode_uppercase(0x1EAC, 0x1EAC).	% Uppercase L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
unicode_uppercase(0x1EAE, 0x1EAE).	% Uppercase L&       LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
unicode_uppercase(0x1EB0, 0x1EB0).	% Uppercase L&       LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
unicode_uppercase(0x1EB2, 0x1EB2).	% Uppercase L&       LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
unicode_uppercase(0x1EB4, 0x1EB4).	% Uppercase L&       LATIN CAPITAL LETTER A WITH BREVE AND TILDE
unicode_uppercase(0x1EB6, 0x1EB6).	% Uppercase L&       LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
unicode_uppercase(0x1EB8, 0x1EB8).	% Uppercase L&       LATIN CAPITAL LETTER E WITH DOT BELOW
unicode_uppercase(0x1EBA, 0x1EBA).	% Uppercase L&       LATIN CAPITAL LETTER E WITH HOOK ABOVE
unicode_uppercase(0x1EBC, 0x1EBC).	% Uppercase L&       LATIN CAPITAL LETTER E WITH TILDE
unicode_uppercase(0x1EBE, 0x1EBE).	% Uppercase L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
unicode_uppercase(0x1EC0, 0x1EC0).	% Uppercase L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
unicode_uppercase(0x1EC2, 0x1EC2).	% Uppercase L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
unicode_uppercase(0x1EC4, 0x1EC4).	% Uppercase L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
unicode_uppercase(0x1EC6, 0x1EC6).	% Uppercase L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
unicode_uppercase(0x1EC8, 0x1EC8).	% Uppercase L&       LATIN CAPITAL LETTER I WITH HOOK ABOVE
unicode_uppercase(0x1ECA, 0x1ECA).	% Uppercase L&       LATIN CAPITAL LETTER I WITH DOT BELOW
unicode_uppercase(0x1ECC, 0x1ECC).	% Uppercase L&       LATIN CAPITAL LETTER O WITH DOT BELOW
unicode_uppercase(0x1ECE, 0x1ECE).	% Uppercase L&       LATIN CAPITAL LETTER O WITH HOOK ABOVE
unicode_uppercase(0x1ED0, 0x1ED0).	% Uppercase L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
unicode_uppercase(0x1ED2, 0x1ED2).	% Uppercase L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
unicode_uppercase(0x1ED4, 0x1ED4).	% Uppercase L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
unicode_uppercase(0x1ED6, 0x1ED6).	% Uppercase L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
unicode_uppercase(0x1ED8, 0x1ED8).	% Uppercase L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
unicode_uppercase(0x1EDA, 0x1EDA).	% Uppercase L&       LATIN CAPITAL LETTER O WITH HORN AND ACUTE
unicode_uppercase(0x1EDC, 0x1EDC).	% Uppercase L&       LATIN CAPITAL LETTER O WITH HORN AND GRAVE
unicode_uppercase(0x1EDE, 0x1EDE).	% Uppercase L&       LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
unicode_uppercase(0x1EE0, 0x1EE0).	% Uppercase L&       LATIN CAPITAL LETTER O WITH HORN AND TILDE
unicode_uppercase(0x1EE2, 0x1EE2).	% Uppercase L&       LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
unicode_uppercase(0x1EE4, 0x1EE4).	% Uppercase L&       LATIN CAPITAL LETTER U WITH DOT BELOW
unicode_uppercase(0x1EE6, 0x1EE6).	% Uppercase L&       LATIN CAPITAL LETTER U WITH HOOK ABOVE
unicode_uppercase(0x1EE8, 0x1EE8).	% Uppercase L&       LATIN CAPITAL LETTER U WITH HORN AND ACUTE
unicode_uppercase(0x1EEA, 0x1EEA).	% Uppercase L&       LATIN CAPITAL LETTER U WITH HORN AND GRAVE
unicode_uppercase(0x1EEC, 0x1EEC).	% Uppercase L&       LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
unicode_uppercase(0x1EEE, 0x1EEE).	% Uppercase L&       LATIN CAPITAL LETTER U WITH HORN AND TILDE
unicode_uppercase(0x1EF0, 0x1EF0).	% Uppercase L&       LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
unicode_uppercase(0x1EF2, 0x1EF2).	% Uppercase L&       LATIN CAPITAL LETTER Y WITH GRAVE
unicode_uppercase(0x1EF4, 0x1EF4).	% Uppercase L&       LATIN CAPITAL LETTER Y WITH DOT BELOW
unicode_uppercase(0x1EF6, 0x1EF6).	% Uppercase L&       LATIN CAPITAL LETTER Y WITH HOOK ABOVE
unicode_uppercase(0x1EF8, 0x1EF8).	% Uppercase L&       LATIN CAPITAL LETTER Y WITH TILDE
unicode_uppercase(0x1EFA, 0x1EFA).	% Uppercase L&       LATIN CAPITAL LETTER MIDDLE-WELSH LL
unicode_uppercase(0x1EFC, 0x1EFC).	% Uppercase L&       LATIN CAPITAL LETTER MIDDLE-WELSH V
unicode_uppercase(0x1EFE, 0x1EFE).	% Uppercase L&       LATIN CAPITAL LETTER Y WITH LOOP
unicode_uppercase(0x1F08, 0x1F0F).	% Uppercase L&   [8] GREEK CAPITAL LETTER ALPHA WITH PSILI..GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
unicode_uppercase(0x1F18, 0x1F1D).	% Uppercase L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_uppercase(0x1F28, 0x1F2F).	% Uppercase L&   [8] GREEK CAPITAL LETTER ETA WITH PSILI..GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
unicode_uppercase(0x1F38, 0x1F3F).	% Uppercase L&   [8] GREEK CAPITAL LETTER IOTA WITH PSILI..GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
unicode_uppercase(0x1F48, 0x1F4D).	% Uppercase L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_uppercase(0x1F59, 0x1F59).	% Uppercase L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_uppercase(0x1F5B, 0x1F5B).	% Uppercase L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_uppercase(0x1F5D, 0x1F5D).	% Uppercase L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_uppercase(0x1F5F, 0x1F5F).	% Uppercase L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_uppercase(0x1F68, 0x1F6F).	% Uppercase L&   [8] GREEK CAPITAL LETTER OMEGA WITH PSILI..GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
unicode_uppercase(0x1FB8, 0x1FBB).	% Uppercase L&   [4] GREEK CAPITAL LETTER ALPHA WITH VRACHY..GREEK CAPITAL LETTER ALPHA WITH OXIA
unicode_uppercase(0x1FC8, 0x1FCB).	% Uppercase L&   [4] GREEK CAPITAL LETTER EPSILON WITH VARIA..GREEK CAPITAL LETTER ETA WITH OXIA
unicode_uppercase(0x1FD8, 0x1FDB).	% Uppercase L&   [4] GREEK CAPITAL LETTER IOTA WITH VRACHY..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_uppercase(0x1FE8, 0x1FEC).	% Uppercase L&   [5] GREEK CAPITAL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_uppercase(0x1FF8, 0x1FFB).	% Uppercase L&   [4] GREEK CAPITAL LETTER OMICRON WITH VARIA..GREEK CAPITAL LETTER OMEGA WITH OXIA
unicode_uppercase(0x2102, 0x2102).	% Uppercase L&       DOUBLE-STRUCK CAPITAL C
unicode_uppercase(0x2107, 0x2107).	% Uppercase L&       EULER CONSTANT
unicode_uppercase(0x210B, 0x210D).	% Uppercase L&   [3] SCRIPT CAPITAL H..DOUBLE-STRUCK CAPITAL H
unicode_uppercase(0x2110, 0x2112).	% Uppercase L&   [3] SCRIPT CAPITAL I..SCRIPT CAPITAL L
unicode_uppercase(0x2115, 0x2115).	% Uppercase L&       DOUBLE-STRUCK CAPITAL N
unicode_uppercase(0x2119, 0x211D).	% Uppercase L&   [5] DOUBLE-STRUCK CAPITAL P..DOUBLE-STRUCK CAPITAL R
unicode_uppercase(0x2124, 0x2124).	% Uppercase L&       DOUBLE-STRUCK CAPITAL Z
unicode_uppercase(0x2126, 0x2126).	% Uppercase L&       OHM SIGN
unicode_uppercase(0x2128, 0x2128).	% Uppercase L&       BLACK-LETTER CAPITAL Z
unicode_uppercase(0x212A, 0x212D).	% Uppercase L&   [4] KELVIN SIGN..BLACK-LETTER CAPITAL C
unicode_uppercase(0x2130, 0x2133).	% Uppercase L&   [4] SCRIPT CAPITAL E..SCRIPT CAPITAL M
unicode_uppercase(0x213E, 0x213F).	% Uppercase L&   [2] DOUBLE-STRUCK CAPITAL GAMMA..DOUBLE-STRUCK CAPITAL PI
unicode_uppercase(0x2145, 0x2145).	% Uppercase L&       DOUBLE-STRUCK ITALIC CAPITAL D
unicode_uppercase(0x2160, 0x216F).	% Uppercase Nl  [16] ROMAN NUMERAL ONE..ROMAN NUMERAL ONE THOUSAND
unicode_uppercase(0x2183, 0x2183).	% Uppercase L&       ROMAN NUMERAL REVERSED ONE HUNDRED
unicode_uppercase(0x24B6, 0x24CF).	% Uppercase So  [26] CIRCLED LATIN CAPITAL LETTER A..CIRCLED LATIN CAPITAL LETTER Z
unicode_uppercase(0x2C00, 0x2C2E).	% Uppercase L&  [47] GLAGOLITIC CAPITAL LETTER AZU..GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_uppercase(0x2C60, 0x2C60).	% Uppercase L&       LATIN CAPITAL LETTER L WITH DOUBLE BAR
unicode_uppercase(0x2C62, 0x2C64).	% Uppercase L&   [3] LATIN CAPITAL LETTER L WITH MIDDLE TILDE..LATIN CAPITAL LETTER R WITH TAIL
unicode_uppercase(0x2C67, 0x2C67).	% Uppercase L&       LATIN CAPITAL LETTER H WITH DESCENDER
unicode_uppercase(0x2C69, 0x2C69).	% Uppercase L&       LATIN CAPITAL LETTER K WITH DESCENDER
unicode_uppercase(0x2C6B, 0x2C6B).	% Uppercase L&       LATIN CAPITAL LETTER Z WITH DESCENDER
unicode_uppercase(0x2C6D, 0x2C70).	% Uppercase L&   [4] LATIN CAPITAL LETTER ALPHA..LATIN CAPITAL LETTER TURNED ALPHA
unicode_uppercase(0x2C72, 0x2C72).	% Uppercase L&       LATIN CAPITAL LETTER W WITH HOOK
unicode_uppercase(0x2C75, 0x2C75).	% Uppercase L&       LATIN CAPITAL LETTER HALF H
unicode_uppercase(0x2C7E, 0x2C80).	% Uppercase L&   [3] LATIN CAPITAL LETTER S WITH SWASH TAIL..COPTIC CAPITAL LETTER ALFA
unicode_uppercase(0x2C82, 0x2C82).	% Uppercase L&       COPTIC CAPITAL LETTER VIDA
unicode_uppercase(0x2C84, 0x2C84).	% Uppercase L&       COPTIC CAPITAL LETTER GAMMA
unicode_uppercase(0x2C86, 0x2C86).	% Uppercase L&       COPTIC CAPITAL LETTER DALDA
unicode_uppercase(0x2C88, 0x2C88).	% Uppercase L&       COPTIC CAPITAL LETTER EIE
unicode_uppercase(0x2C8A, 0x2C8A).	% Uppercase L&       COPTIC CAPITAL LETTER SOU
unicode_uppercase(0x2C8C, 0x2C8C).	% Uppercase L&       COPTIC CAPITAL LETTER ZATA
unicode_uppercase(0x2C8E, 0x2C8E).	% Uppercase L&       COPTIC CAPITAL LETTER HATE
unicode_uppercase(0x2C90, 0x2C90).	% Uppercase L&       COPTIC CAPITAL LETTER THETHE
unicode_uppercase(0x2C92, 0x2C92).	% Uppercase L&       COPTIC CAPITAL LETTER IAUDA
unicode_uppercase(0x2C94, 0x2C94).	% Uppercase L&       COPTIC CAPITAL LETTER KAPA
unicode_uppercase(0x2C96, 0x2C96).	% Uppercase L&       COPTIC CAPITAL LETTER LAULA
unicode_uppercase(0x2C98, 0x2C98).	% Uppercase L&       COPTIC CAPITAL LETTER MI
unicode_uppercase(0x2C9A, 0x2C9A).	% Uppercase L&       COPTIC CAPITAL LETTER NI
unicode_uppercase(0x2C9C, 0x2C9C).	% Uppercase L&       COPTIC CAPITAL LETTER KSI
unicode_uppercase(0x2C9E, 0x2C9E).	% Uppercase L&       COPTIC CAPITAL LETTER O
unicode_uppercase(0x2CA0, 0x2CA0).	% Uppercase L&       COPTIC CAPITAL LETTER PI
unicode_uppercase(0x2CA2, 0x2CA2).	% Uppercase L&       COPTIC CAPITAL LETTER RO
unicode_uppercase(0x2CA4, 0x2CA4).	% Uppercase L&       COPTIC CAPITAL LETTER SIMA
unicode_uppercase(0x2CA6, 0x2CA6).	% Uppercase L&       COPTIC CAPITAL LETTER TAU
unicode_uppercase(0x2CA8, 0x2CA8).	% Uppercase L&       COPTIC CAPITAL LETTER UA
unicode_uppercase(0x2CAA, 0x2CAA).	% Uppercase L&       COPTIC CAPITAL LETTER FI
unicode_uppercase(0x2CAC, 0x2CAC).	% Uppercase L&       COPTIC CAPITAL LETTER KHI
unicode_uppercase(0x2CAE, 0x2CAE).	% Uppercase L&       COPTIC CAPITAL LETTER PSI
unicode_uppercase(0x2CB0, 0x2CB0).	% Uppercase L&       COPTIC CAPITAL LETTER OOU
unicode_uppercase(0x2CB2, 0x2CB2).	% Uppercase L&       COPTIC CAPITAL LETTER DIALECT-P ALEF
unicode_uppercase(0x2CB4, 0x2CB4).	% Uppercase L&       COPTIC CAPITAL LETTER OLD COPTIC AIN
unicode_uppercase(0x2CB6, 0x2CB6).	% Uppercase L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC EIE
unicode_uppercase(0x2CB8, 0x2CB8).	% Uppercase L&       COPTIC CAPITAL LETTER DIALECT-P KAPA
unicode_uppercase(0x2CBA, 0x2CBA).	% Uppercase L&       COPTIC CAPITAL LETTER DIALECT-P NI
unicode_uppercase(0x2CBC, 0x2CBC).	% Uppercase L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC NI
unicode_uppercase(0x2CBE, 0x2CBE).	% Uppercase L&       COPTIC CAPITAL LETTER OLD COPTIC OOU
unicode_uppercase(0x2CC0, 0x2CC0).	% Uppercase L&       COPTIC CAPITAL LETTER SAMPI
unicode_uppercase(0x2CC2, 0x2CC2).	% Uppercase L&       COPTIC CAPITAL LETTER CROSSED SHEI
unicode_uppercase(0x2CC4, 0x2CC4).	% Uppercase L&       COPTIC CAPITAL LETTER OLD COPTIC SHEI
unicode_uppercase(0x2CC6, 0x2CC6).	% Uppercase L&       COPTIC CAPITAL LETTER OLD COPTIC ESH
unicode_uppercase(0x2CC8, 0x2CC8).	% Uppercase L&       COPTIC CAPITAL LETTER AKHMIMIC KHEI
unicode_uppercase(0x2CCA, 0x2CCA).	% Uppercase L&       COPTIC CAPITAL LETTER DIALECT-P HORI
unicode_uppercase(0x2CCC, 0x2CCC).	% Uppercase L&       COPTIC CAPITAL LETTER OLD COPTIC HORI
unicode_uppercase(0x2CCE, 0x2CCE).	% Uppercase L&       COPTIC CAPITAL LETTER OLD COPTIC HA
unicode_uppercase(0x2CD0, 0x2CD0).	% Uppercase L&       COPTIC CAPITAL LETTER L-SHAPED HA
unicode_uppercase(0x2CD2, 0x2CD2).	% Uppercase L&       COPTIC CAPITAL LETTER OLD COPTIC HEI
unicode_uppercase(0x2CD4, 0x2CD4).	% Uppercase L&       COPTIC CAPITAL LETTER OLD COPTIC HAT
unicode_uppercase(0x2CD6, 0x2CD6).	% Uppercase L&       COPTIC CAPITAL LETTER OLD COPTIC GANGIA
unicode_uppercase(0x2CD8, 0x2CD8).	% Uppercase L&       COPTIC CAPITAL LETTER OLD COPTIC DJA
unicode_uppercase(0x2CDA, 0x2CDA).	% Uppercase L&       COPTIC CAPITAL LETTER OLD COPTIC SHIMA
unicode_uppercase(0x2CDC, 0x2CDC).	% Uppercase L&       COPTIC CAPITAL LETTER OLD NUBIAN SHIMA
unicode_uppercase(0x2CDE, 0x2CDE).	% Uppercase L&       COPTIC CAPITAL LETTER OLD NUBIAN NGI
unicode_uppercase(0x2CE0, 0x2CE0).	% Uppercase L&       COPTIC CAPITAL LETTER OLD NUBIAN NYI
unicode_uppercase(0x2CE2, 0x2CE2).	% Uppercase L&       COPTIC CAPITAL LETTER OLD NUBIAN WAU
unicode_uppercase(0x2CEB, 0x2CEB).	% Uppercase L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI
unicode_uppercase(0x2CED, 0x2CED).	% Uppercase L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC GANGIA
unicode_uppercase(0x2CF2, 0x2CF2).	% Uppercase L&       COPTIC CAPITAL LETTER BOHAIRIC KHEI
unicode_uppercase(0xA640, 0xA640).	% Uppercase L&       CYRILLIC CAPITAL LETTER ZEMLYA
unicode_uppercase(0xA642, 0xA642).	% Uppercase L&       CYRILLIC CAPITAL LETTER DZELO
unicode_uppercase(0xA644, 0xA644).	% Uppercase L&       CYRILLIC CAPITAL LETTER REVERSED DZE
unicode_uppercase(0xA646, 0xA646).	% Uppercase L&       CYRILLIC CAPITAL LETTER IOTA
unicode_uppercase(0xA648, 0xA648).	% Uppercase L&       CYRILLIC CAPITAL LETTER DJERV
unicode_uppercase(0xA64A, 0xA64A).	% Uppercase L&       CYRILLIC CAPITAL LETTER MONOGRAPH UK
unicode_uppercase(0xA64C, 0xA64C).	% Uppercase L&       CYRILLIC CAPITAL LETTER BROAD OMEGA
unicode_uppercase(0xA64E, 0xA64E).	% Uppercase L&       CYRILLIC CAPITAL LETTER NEUTRAL YER
unicode_uppercase(0xA650, 0xA650).	% Uppercase L&       CYRILLIC CAPITAL LETTER YERU WITH BACK YER
unicode_uppercase(0xA652, 0xA652).	% Uppercase L&       CYRILLIC CAPITAL LETTER IOTIFIED YAT
unicode_uppercase(0xA654, 0xA654).	% Uppercase L&       CYRILLIC CAPITAL LETTER REVERSED YU
unicode_uppercase(0xA656, 0xA656).	% Uppercase L&       CYRILLIC CAPITAL LETTER IOTIFIED A
unicode_uppercase(0xA658, 0xA658).	% Uppercase L&       CYRILLIC CAPITAL LETTER CLOSED LITTLE YUS
unicode_uppercase(0xA65A, 0xA65A).	% Uppercase L&       CYRILLIC CAPITAL LETTER BLENDED YUS
unicode_uppercase(0xA65C, 0xA65C).	% Uppercase L&       CYRILLIC CAPITAL LETTER IOTIFIED CLOSED LITTLE YUS
unicode_uppercase(0xA65E, 0xA65E).	% Uppercase L&       CYRILLIC CAPITAL LETTER YN
unicode_uppercase(0xA660, 0xA660).	% Uppercase L&       CYRILLIC CAPITAL LETTER REVERSED TSE
unicode_uppercase(0xA662, 0xA662).	% Uppercase L&       CYRILLIC CAPITAL LETTER SOFT DE
unicode_uppercase(0xA664, 0xA664).	% Uppercase L&       CYRILLIC CAPITAL LETTER SOFT EL
unicode_uppercase(0xA666, 0xA666).	% Uppercase L&       CYRILLIC CAPITAL LETTER SOFT EM
unicode_uppercase(0xA668, 0xA668).	% Uppercase L&       CYRILLIC CAPITAL LETTER MONOCULAR O
unicode_uppercase(0xA66A, 0xA66A).	% Uppercase L&       CYRILLIC CAPITAL LETTER BINOCULAR O
unicode_uppercase(0xA66C, 0xA66C).	% Uppercase L&       CYRILLIC CAPITAL LETTER DOUBLE MONOCULAR O
unicode_uppercase(0xA680, 0xA680).	% Uppercase L&       CYRILLIC CAPITAL LETTER DWE
unicode_uppercase(0xA682, 0xA682).	% Uppercase L&       CYRILLIC CAPITAL LETTER DZWE
unicode_uppercase(0xA684, 0xA684).	% Uppercase L&       CYRILLIC CAPITAL LETTER ZHWE
unicode_uppercase(0xA686, 0xA686).	% Uppercase L&       CYRILLIC CAPITAL LETTER CCHE
unicode_uppercase(0xA688, 0xA688).	% Uppercase L&       CYRILLIC CAPITAL LETTER DZZE
unicode_uppercase(0xA68A, 0xA68A).	% Uppercase L&       CYRILLIC CAPITAL LETTER TE WITH MIDDLE HOOK
unicode_uppercase(0xA68C, 0xA68C).	% Uppercase L&       CYRILLIC CAPITAL LETTER TWE
unicode_uppercase(0xA68E, 0xA68E).	% Uppercase L&       CYRILLIC CAPITAL LETTER TSWE
unicode_uppercase(0xA690, 0xA690).	% Uppercase L&       CYRILLIC CAPITAL LETTER TSSE
unicode_uppercase(0xA692, 0xA692).	% Uppercase L&       CYRILLIC CAPITAL LETTER TCHE
unicode_uppercase(0xA694, 0xA694).	% Uppercase L&       CYRILLIC CAPITAL LETTER HWE
unicode_uppercase(0xA696, 0xA696).	% Uppercase L&       CYRILLIC CAPITAL LETTER SHWE
unicode_uppercase(0xA722, 0xA722).	% Uppercase L&       LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF
unicode_uppercase(0xA724, 0xA724).	% Uppercase L&       LATIN CAPITAL LETTER EGYPTOLOGICAL AIN
unicode_uppercase(0xA726, 0xA726).	% Uppercase L&       LATIN CAPITAL LETTER HENG
unicode_uppercase(0xA728, 0xA728).	% Uppercase L&       LATIN CAPITAL LETTER TZ
unicode_uppercase(0xA72A, 0xA72A).	% Uppercase L&       LATIN CAPITAL LETTER TRESILLO
unicode_uppercase(0xA72C, 0xA72C).	% Uppercase L&       LATIN CAPITAL LETTER CUATRILLO
unicode_uppercase(0xA72E, 0xA72E).	% Uppercase L&       LATIN CAPITAL LETTER CUATRILLO WITH COMMA
unicode_uppercase(0xA732, 0xA732).	% Uppercase L&       LATIN CAPITAL LETTER AA
unicode_uppercase(0xA734, 0xA734).	% Uppercase L&       LATIN CAPITAL LETTER AO
unicode_uppercase(0xA736, 0xA736).	% Uppercase L&       LATIN CAPITAL LETTER AU
unicode_uppercase(0xA738, 0xA738).	% Uppercase L&       LATIN CAPITAL LETTER AV
unicode_uppercase(0xA73A, 0xA73A).	% Uppercase L&       LATIN CAPITAL LETTER AV WITH HORIZONTAL BAR
unicode_uppercase(0xA73C, 0xA73C).	% Uppercase L&       LATIN CAPITAL LETTER AY
unicode_uppercase(0xA73E, 0xA73E).	% Uppercase L&       LATIN CAPITAL LETTER REVERSED C WITH DOT
unicode_uppercase(0xA740, 0xA740).	% Uppercase L&       LATIN CAPITAL LETTER K WITH STROKE
unicode_uppercase(0xA742, 0xA742).	% Uppercase L&       LATIN CAPITAL LETTER K WITH DIAGONAL STROKE
unicode_uppercase(0xA744, 0xA744).	% Uppercase L&       LATIN CAPITAL LETTER K WITH STROKE AND DIAGONAL STROKE
unicode_uppercase(0xA746, 0xA746).	% Uppercase L&       LATIN CAPITAL LETTER BROKEN L
unicode_uppercase(0xA748, 0xA748).	% Uppercase L&       LATIN CAPITAL LETTER L WITH HIGH STROKE
unicode_uppercase(0xA74A, 0xA74A).	% Uppercase L&       LATIN CAPITAL LETTER O WITH LONG STROKE OVERLAY
unicode_uppercase(0xA74C, 0xA74C).	% Uppercase L&       LATIN CAPITAL LETTER O WITH LOOP
unicode_uppercase(0xA74E, 0xA74E).	% Uppercase L&       LATIN CAPITAL LETTER OO
unicode_uppercase(0xA750, 0xA750).	% Uppercase L&       LATIN CAPITAL LETTER P WITH STROKE THROUGH DESCENDER
unicode_uppercase(0xA752, 0xA752).	% Uppercase L&       LATIN CAPITAL LETTER P WITH FLOURISH
unicode_uppercase(0xA754, 0xA754).	% Uppercase L&       LATIN CAPITAL LETTER P WITH SQUIRREL TAIL
unicode_uppercase(0xA756, 0xA756).	% Uppercase L&       LATIN CAPITAL LETTER Q WITH STROKE THROUGH DESCENDER
unicode_uppercase(0xA758, 0xA758).	% Uppercase L&       LATIN CAPITAL LETTER Q WITH DIAGONAL STROKE
unicode_uppercase(0xA75A, 0xA75A).	% Uppercase L&       LATIN CAPITAL LETTER R ROTUNDA
unicode_uppercase(0xA75C, 0xA75C).	% Uppercase L&       LATIN CAPITAL LETTER RUM ROTUNDA
unicode_uppercase(0xA75E, 0xA75E).	% Uppercase L&       LATIN CAPITAL LETTER V WITH DIAGONAL STROKE
unicode_uppercase(0xA760, 0xA760).	% Uppercase L&       LATIN CAPITAL LETTER VY
unicode_uppercase(0xA762, 0xA762).	% Uppercase L&       LATIN CAPITAL LETTER VISIGOTHIC Z
unicode_uppercase(0xA764, 0xA764).	% Uppercase L&       LATIN CAPITAL LETTER THORN WITH STROKE
unicode_uppercase(0xA766, 0xA766).	% Uppercase L&       LATIN CAPITAL LETTER THORN WITH STROKE THROUGH DESCENDER
unicode_uppercase(0xA768, 0xA768).	% Uppercase L&       LATIN CAPITAL LETTER VEND
unicode_uppercase(0xA76A, 0xA76A).	% Uppercase L&       LATIN CAPITAL LETTER ET
unicode_uppercase(0xA76C, 0xA76C).	% Uppercase L&       LATIN CAPITAL LETTER IS
unicode_uppercase(0xA76E, 0xA76E).	% Uppercase L&       LATIN CAPITAL LETTER CON
unicode_uppercase(0xA779, 0xA779).	% Uppercase L&       LATIN CAPITAL LETTER INSULAR D
unicode_uppercase(0xA77B, 0xA77B).	% Uppercase L&       LATIN CAPITAL LETTER INSULAR F
unicode_uppercase(0xA77D, 0xA77E).	% Uppercase L&   [2] LATIN CAPITAL LETTER INSULAR G..LATIN CAPITAL LETTER TURNED INSULAR G
unicode_uppercase(0xA780, 0xA780).	% Uppercase L&       LATIN CAPITAL LETTER TURNED L
unicode_uppercase(0xA782, 0xA782).	% Uppercase L&       LATIN CAPITAL LETTER INSULAR R
unicode_uppercase(0xA784, 0xA784).	% Uppercase L&       LATIN CAPITAL LETTER INSULAR S
unicode_uppercase(0xA786, 0xA786).	% Uppercase L&       LATIN CAPITAL LETTER INSULAR T
unicode_uppercase(0xA78B, 0xA78B).	% Uppercase L&       LATIN CAPITAL LETTER SALTILLO
unicode_uppercase(0xA78D, 0xA78D).	% Uppercase L&       LATIN CAPITAL LETTER TURNED H
unicode_uppercase(0xA790, 0xA790).	% Uppercase L&       LATIN CAPITAL LETTER N WITH DESCENDER
unicode_uppercase(0xA792, 0xA792).	% Uppercase L&       LATIN CAPITAL LETTER C WITH BAR
unicode_uppercase(0xA7A0, 0xA7A0).	% Uppercase L&       LATIN CAPITAL LETTER G WITH OBLIQUE STROKE
unicode_uppercase(0xA7A2, 0xA7A2).	% Uppercase L&       LATIN CAPITAL LETTER K WITH OBLIQUE STROKE
unicode_uppercase(0xA7A4, 0xA7A4).	% Uppercase L&       LATIN CAPITAL LETTER N WITH OBLIQUE STROKE
unicode_uppercase(0xA7A6, 0xA7A6).	% Uppercase L&       LATIN CAPITAL LETTER R WITH OBLIQUE STROKE
unicode_uppercase(0xA7A8, 0xA7A8).	% Uppercase L&       LATIN CAPITAL LETTER S WITH OBLIQUE STROKE
unicode_uppercase(0xA7AA, 0xA7AA).	% Uppercase L&       LATIN CAPITAL LETTER H WITH HOOK
unicode_uppercase(0xFF21, 0xFF3A).	% Uppercase L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_uppercase(0x10400, 0x10427).	% Uppercase L&  [40] DESERET CAPITAL LETTER LONG I..DESERET CAPITAL LETTER EW
unicode_uppercase(0x1D400, 0x1D419).	% Uppercase L&  [26] MATHEMATICAL BOLD CAPITAL A..MATHEMATICAL BOLD CAPITAL Z
unicode_uppercase(0x1D434, 0x1D44D).	% Uppercase L&  [26] MATHEMATICAL ITALIC CAPITAL A..MATHEMATICAL ITALIC CAPITAL Z
unicode_uppercase(0x1D468, 0x1D481).	% Uppercase L&  [26] MATHEMATICAL BOLD ITALIC CAPITAL A..MATHEMATICAL BOLD ITALIC CAPITAL Z
unicode_uppercase(0x1D49C, 0x1D49C).	% Uppercase L&       MATHEMATICAL SCRIPT CAPITAL A
unicode_uppercase(0x1D49E, 0x1D49F).	% Uppercase L&   [2] MATHEMATICAL SCRIPT CAPITAL C..MATHEMATICAL SCRIPT CAPITAL D
unicode_uppercase(0x1D4A2, 0x1D4A2).	% Uppercase L&       MATHEMATICAL SCRIPT CAPITAL G
unicode_uppercase(0x1D4A5, 0x1D4A6).	% Uppercase L&   [2] MATHEMATICAL SCRIPT CAPITAL J..MATHEMATICAL SCRIPT CAPITAL K
unicode_uppercase(0x1D4A9, 0x1D4AC).	% Uppercase L&   [4] MATHEMATICAL SCRIPT CAPITAL N..MATHEMATICAL SCRIPT CAPITAL Q
unicode_uppercase(0x1D4AE, 0x1D4B5).	% Uppercase L&   [8] MATHEMATICAL SCRIPT CAPITAL S..MATHEMATICAL SCRIPT CAPITAL Z
unicode_uppercase(0x1D4D0, 0x1D4E9).	% Uppercase L&  [26] MATHEMATICAL BOLD SCRIPT CAPITAL A..MATHEMATICAL BOLD SCRIPT CAPITAL Z
unicode_uppercase(0x1D504, 0x1D505).	% Uppercase L&   [2] MATHEMATICAL FRAKTUR CAPITAL A..MATHEMATICAL FRAKTUR CAPITAL B
unicode_uppercase(0x1D507, 0x1D50A).	% Uppercase L&   [4] MATHEMATICAL FRAKTUR CAPITAL D..MATHEMATICAL FRAKTUR CAPITAL G
unicode_uppercase(0x1D50D, 0x1D514).	% Uppercase L&   [8] MATHEMATICAL FRAKTUR CAPITAL J..MATHEMATICAL FRAKTUR CAPITAL Q
unicode_uppercase(0x1D516, 0x1D51C).	% Uppercase L&   [7] MATHEMATICAL FRAKTUR CAPITAL S..MATHEMATICAL FRAKTUR CAPITAL Y
unicode_uppercase(0x1D538, 0x1D539).	% Uppercase L&   [2] MATHEMATICAL DOUBLE-STRUCK CAPITAL A..MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_uppercase(0x1D53B, 0x1D53E).	% Uppercase L&   [4] MATHEMATICAL DOUBLE-STRUCK CAPITAL D..MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_uppercase(0x1D540, 0x1D544).	% Uppercase L&   [5] MATHEMATICAL DOUBLE-STRUCK CAPITAL I..MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_uppercase(0x1D546, 0x1D546).	% Uppercase L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_uppercase(0x1D54A, 0x1D550).	% Uppercase L&   [7] MATHEMATICAL DOUBLE-STRUCK CAPITAL S..MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_uppercase(0x1D56C, 0x1D585).	% Uppercase L&  [26] MATHEMATICAL BOLD FRAKTUR CAPITAL A..MATHEMATICAL BOLD FRAKTUR CAPITAL Z
unicode_uppercase(0x1D5A0, 0x1D5B9).	% Uppercase L&  [26] MATHEMATICAL SANS-SERIF CAPITAL A..MATHEMATICAL SANS-SERIF CAPITAL Z
unicode_uppercase(0x1D5D4, 0x1D5ED).	% Uppercase L&  [26] MATHEMATICAL SANS-SERIF BOLD CAPITAL A..MATHEMATICAL SANS-SERIF BOLD CAPITAL Z
unicode_uppercase(0x1D608, 0x1D621).	% Uppercase L&  [26] MATHEMATICAL SANS-SERIF ITALIC CAPITAL A..MATHEMATICAL SANS-SERIF ITALIC CAPITAL Z
unicode_uppercase(0x1D63C, 0x1D655).	% Uppercase L&  [26] MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL A..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Z
unicode_uppercase(0x1D670, 0x1D689).	% Uppercase L&  [26] MATHEMATICAL MONOSPACE CAPITAL A..MATHEMATICAL MONOSPACE CAPITAL Z
unicode_uppercase(0x1D6A8, 0x1D6C0).	% Uppercase L&  [25] MATHEMATICAL BOLD CAPITAL ALPHA..MATHEMATICAL BOLD CAPITAL OMEGA
unicode_uppercase(0x1D6E2, 0x1D6FA).	% Uppercase L&  [25] MATHEMATICAL ITALIC CAPITAL ALPHA..MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_uppercase(0x1D71C, 0x1D734).	% Uppercase L&  [25] MATHEMATICAL BOLD ITALIC CAPITAL ALPHA..MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_uppercase(0x1D756, 0x1D76E).	% Uppercase L&  [25] MATHEMATICAL SANS-SERIF BOLD CAPITAL ALPHA..MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_uppercase(0x1D790, 0x1D7A8).	% Uppercase L&  [25] MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_uppercase(0x1D7CA, 0x1D7CA).	% Uppercase L&       MATHEMATICAL BOLD CAPITAL DIGAMMA

% Total code points: 1483
