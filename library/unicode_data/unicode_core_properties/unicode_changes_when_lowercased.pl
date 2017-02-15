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

unicode_changes_when_lowercased(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_changes_when_lowercased(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_changes_when_lowercased(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_changes_when_lowercased(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property:   Changes_When_Lowercased (CWL)
%  Characters whose normalized forms are not stable under a toLowercase mapping.
%  For more information, see D124 in Section 3.13, "Default Case Algorithms".
%  Changes_When_Lowercased(X) is true when toLowercase(toNFD(X)) != toNFD(X)

unicode_changes_when_lowercased(0x0041, 0x005A).	% Changes_When_Lowercased L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
unicode_changes_when_lowercased(0x00C0, 0x00D6).	% Changes_When_Lowercased L&  [23] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_changes_when_lowercased(0x00D8, 0x00DE).	% Changes_When_Lowercased L&   [7] LATIN CAPITAL LETTER O WITH STROKE..LATIN CAPITAL LETTER THORN
unicode_changes_when_lowercased(0x0100, 0x0100).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH MACRON
unicode_changes_when_lowercased(0x0102, 0x0102).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH BREVE
unicode_changes_when_lowercased(0x0104, 0x0104).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH OGONEK
unicode_changes_when_lowercased(0x0106, 0x0106).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER C WITH ACUTE
unicode_changes_when_lowercased(0x0108, 0x0108).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER C WITH CIRCUMFLEX
unicode_changes_when_lowercased(0x010A, 0x010A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER C WITH DOT ABOVE
unicode_changes_when_lowercased(0x010C, 0x010C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER C WITH CARON
unicode_changes_when_lowercased(0x010E, 0x010E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER D WITH CARON
unicode_changes_when_lowercased(0x0110, 0x0110).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER D WITH STROKE
unicode_changes_when_lowercased(0x0112, 0x0112).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH MACRON
unicode_changes_when_lowercased(0x0114, 0x0114).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH BREVE
unicode_changes_when_lowercased(0x0116, 0x0116).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH DOT ABOVE
unicode_changes_when_lowercased(0x0118, 0x0118).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH OGONEK
unicode_changes_when_lowercased(0x011A, 0x011A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH CARON
unicode_changes_when_lowercased(0x011C, 0x011C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER G WITH CIRCUMFLEX
unicode_changes_when_lowercased(0x011E, 0x011E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER G WITH BREVE
unicode_changes_when_lowercased(0x0120, 0x0120).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER G WITH DOT ABOVE
unicode_changes_when_lowercased(0x0122, 0x0122).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER G WITH CEDILLA
unicode_changes_when_lowercased(0x0124, 0x0124).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER H WITH CIRCUMFLEX
unicode_changes_when_lowercased(0x0126, 0x0126).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER H WITH STROKE
unicode_changes_when_lowercased(0x0128, 0x0128).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER I WITH TILDE
unicode_changes_when_lowercased(0x012A, 0x012A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER I WITH MACRON
unicode_changes_when_lowercased(0x012C, 0x012C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER I WITH BREVE
unicode_changes_when_lowercased(0x012E, 0x012E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER I WITH OGONEK
unicode_changes_when_lowercased(0x0130, 0x0130).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER I WITH DOT ABOVE
unicode_changes_when_lowercased(0x0132, 0x0132).	% Changes_When_Lowercased L&       LATIN CAPITAL LIGATURE IJ
unicode_changes_when_lowercased(0x0134, 0x0134).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER J WITH CIRCUMFLEX
unicode_changes_when_lowercased(0x0136, 0x0136).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER K WITH CEDILLA
unicode_changes_when_lowercased(0x0139, 0x0139).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER L WITH ACUTE
unicode_changes_when_lowercased(0x013B, 0x013B).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER L WITH CEDILLA
unicode_changes_when_lowercased(0x013D, 0x013D).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER L WITH CARON
unicode_changes_when_lowercased(0x013F, 0x013F).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER L WITH MIDDLE DOT
unicode_changes_when_lowercased(0x0141, 0x0141).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER L WITH STROKE
unicode_changes_when_lowercased(0x0143, 0x0143).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER N WITH ACUTE
unicode_changes_when_lowercased(0x0145, 0x0145).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER N WITH CEDILLA
unicode_changes_when_lowercased(0x0147, 0x0147).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER N WITH CARON
unicode_changes_when_lowercased(0x014A, 0x014A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER ENG
unicode_changes_when_lowercased(0x014C, 0x014C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH MACRON
unicode_changes_when_lowercased(0x014E, 0x014E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH BREVE
unicode_changes_when_lowercased(0x0150, 0x0150).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
unicode_changes_when_lowercased(0x0152, 0x0152).	% Changes_When_Lowercased L&       LATIN CAPITAL LIGATURE OE
unicode_changes_when_lowercased(0x0154, 0x0154).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER R WITH ACUTE
unicode_changes_when_lowercased(0x0156, 0x0156).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER R WITH CEDILLA
unicode_changes_when_lowercased(0x0158, 0x0158).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER R WITH CARON
unicode_changes_when_lowercased(0x015A, 0x015A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER S WITH ACUTE
unicode_changes_when_lowercased(0x015C, 0x015C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER S WITH CIRCUMFLEX
unicode_changes_when_lowercased(0x015E, 0x015E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER S WITH CEDILLA
unicode_changes_when_lowercased(0x0160, 0x0160).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER S WITH CARON
unicode_changes_when_lowercased(0x0162, 0x0162).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER T WITH CEDILLA
unicode_changes_when_lowercased(0x0164, 0x0164).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER T WITH CARON
unicode_changes_when_lowercased(0x0166, 0x0166).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER T WITH STROKE
unicode_changes_when_lowercased(0x0168, 0x0168).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH TILDE
unicode_changes_when_lowercased(0x016A, 0x016A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH MACRON
unicode_changes_when_lowercased(0x016C, 0x016C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH BREVE
unicode_changes_when_lowercased(0x016E, 0x016E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH RING ABOVE
unicode_changes_when_lowercased(0x0170, 0x0170).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
unicode_changes_when_lowercased(0x0172, 0x0172).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH OGONEK
unicode_changes_when_lowercased(0x0174, 0x0174).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER W WITH CIRCUMFLEX
unicode_changes_when_lowercased(0x0176, 0x0176).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
unicode_changes_when_lowercased(0x0178, 0x0179).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER Y WITH DIAERESIS..LATIN CAPITAL LETTER Z WITH ACUTE
unicode_changes_when_lowercased(0x017B, 0x017B).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Z WITH DOT ABOVE
unicode_changes_when_lowercased(0x017D, 0x017D).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Z WITH CARON
unicode_changes_when_lowercased(0x0181, 0x0182).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER B WITH HOOK..LATIN CAPITAL LETTER B WITH TOPBAR
unicode_changes_when_lowercased(0x0184, 0x0184).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER TONE SIX
unicode_changes_when_lowercased(0x0186, 0x0187).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER OPEN O..LATIN CAPITAL LETTER C WITH HOOK
unicode_changes_when_lowercased(0x0189, 0x018B).	% Changes_When_Lowercased L&   [3] LATIN CAPITAL LETTER AFRICAN D..LATIN CAPITAL LETTER D WITH TOPBAR
unicode_changes_when_lowercased(0x018E, 0x0191).	% Changes_When_Lowercased L&   [4] LATIN CAPITAL LETTER REVERSED E..LATIN CAPITAL LETTER F WITH HOOK
unicode_changes_when_lowercased(0x0193, 0x0194).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER G WITH HOOK..LATIN CAPITAL LETTER GAMMA
unicode_changes_when_lowercased(0x0196, 0x0198).	% Changes_When_Lowercased L&   [3] LATIN CAPITAL LETTER IOTA..LATIN CAPITAL LETTER K WITH HOOK
unicode_changes_when_lowercased(0x019C, 0x019D).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER TURNED M..LATIN CAPITAL LETTER N WITH LEFT HOOK
unicode_changes_when_lowercased(0x019F, 0x01A0).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER O WITH MIDDLE TILDE..LATIN CAPITAL LETTER O WITH HORN
unicode_changes_when_lowercased(0x01A2, 0x01A2).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER OI
unicode_changes_when_lowercased(0x01A4, 0x01A4).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER P WITH HOOK
unicode_changes_when_lowercased(0x01A6, 0x01A7).	% Changes_When_Lowercased L&   [2] LATIN LETTER YR..LATIN CAPITAL LETTER TONE TWO
unicode_changes_when_lowercased(0x01A9, 0x01A9).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER ESH
unicode_changes_when_lowercased(0x01AC, 0x01AC).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER T WITH HOOK
unicode_changes_when_lowercased(0x01AE, 0x01AF).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER T WITH RETROFLEX HOOK..LATIN CAPITAL LETTER U WITH HORN
unicode_changes_when_lowercased(0x01B1, 0x01B3).	% Changes_When_Lowercased L&   [3] LATIN CAPITAL LETTER UPSILON..LATIN CAPITAL LETTER Y WITH HOOK
unicode_changes_when_lowercased(0x01B5, 0x01B5).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Z WITH STROKE
unicode_changes_when_lowercased(0x01B7, 0x01B8).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER EZH..LATIN CAPITAL LETTER EZH REVERSED
unicode_changes_when_lowercased(0x01BC, 0x01BC).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER TONE FIVE
unicode_changes_when_lowercased(0x01C4, 0x01C5).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER DZ WITH CARON..LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
unicode_changes_when_lowercased(0x01C7, 0x01C8).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER LJ..LATIN CAPITAL LETTER L WITH SMALL LETTER J
unicode_changes_when_lowercased(0x01CA, 0x01CB).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER NJ..LATIN CAPITAL LETTER N WITH SMALL LETTER J
unicode_changes_when_lowercased(0x01CD, 0x01CD).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH CARON
unicode_changes_when_lowercased(0x01CF, 0x01CF).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER I WITH CARON
unicode_changes_when_lowercased(0x01D1, 0x01D1).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH CARON
unicode_changes_when_lowercased(0x01D3, 0x01D3).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH CARON
unicode_changes_when_lowercased(0x01D5, 0x01D5).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
unicode_changes_when_lowercased(0x01D7, 0x01D7).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
unicode_changes_when_lowercased(0x01D9, 0x01D9).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
unicode_changes_when_lowercased(0x01DB, 0x01DB).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
unicode_changes_when_lowercased(0x01DE, 0x01DE).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
unicode_changes_when_lowercased(0x01E0, 0x01E0).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
unicode_changes_when_lowercased(0x01E2, 0x01E2).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER AE WITH MACRON
unicode_changes_when_lowercased(0x01E4, 0x01E4).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER G WITH STROKE
unicode_changes_when_lowercased(0x01E6, 0x01E6).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER G WITH CARON
unicode_changes_when_lowercased(0x01E8, 0x01E8).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER K WITH CARON
unicode_changes_when_lowercased(0x01EA, 0x01EA).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH OGONEK
unicode_changes_when_lowercased(0x01EC, 0x01EC).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
unicode_changes_when_lowercased(0x01EE, 0x01EE).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER EZH WITH CARON
unicode_changes_when_lowercased(0x01F1, 0x01F2).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER DZ..LATIN CAPITAL LETTER D WITH SMALL LETTER Z
unicode_changes_when_lowercased(0x01F4, 0x01F4).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER G WITH ACUTE
unicode_changes_when_lowercased(0x01F6, 0x01F8).	% Changes_When_Lowercased L&   [3] LATIN CAPITAL LETTER HWAIR..LATIN CAPITAL LETTER N WITH GRAVE
unicode_changes_when_lowercased(0x01FA, 0x01FA).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
unicode_changes_when_lowercased(0x01FC, 0x01FC).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER AE WITH ACUTE
unicode_changes_when_lowercased(0x01FE, 0x01FE).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
unicode_changes_when_lowercased(0x0200, 0x0200).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
unicode_changes_when_lowercased(0x0202, 0x0202).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH INVERTED BREVE
unicode_changes_when_lowercased(0x0204, 0x0204).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
unicode_changes_when_lowercased(0x0206, 0x0206).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH INVERTED BREVE
unicode_changes_when_lowercased(0x0208, 0x0208).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
unicode_changes_when_lowercased(0x020A, 0x020A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER I WITH INVERTED BREVE
unicode_changes_when_lowercased(0x020C, 0x020C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
unicode_changes_when_lowercased(0x020E, 0x020E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH INVERTED BREVE
unicode_changes_when_lowercased(0x0210, 0x0210).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
unicode_changes_when_lowercased(0x0212, 0x0212).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER R WITH INVERTED BREVE
unicode_changes_when_lowercased(0x0214, 0x0214).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
unicode_changes_when_lowercased(0x0216, 0x0216).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH INVERTED BREVE
unicode_changes_when_lowercased(0x0218, 0x0218).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER S WITH COMMA BELOW
unicode_changes_when_lowercased(0x021A, 0x021A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER T WITH COMMA BELOW
unicode_changes_when_lowercased(0x021C, 0x021C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER YOGH
unicode_changes_when_lowercased(0x021E, 0x021E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER H WITH CARON
unicode_changes_when_lowercased(0x0220, 0x0220).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER N WITH LONG RIGHT LEG
unicode_changes_when_lowercased(0x0222, 0x0222).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER OU
unicode_changes_when_lowercased(0x0224, 0x0224).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Z WITH HOOK
unicode_changes_when_lowercased(0x0226, 0x0226).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH DOT ABOVE
unicode_changes_when_lowercased(0x0228, 0x0228).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH CEDILLA
unicode_changes_when_lowercased(0x022A, 0x022A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
unicode_changes_when_lowercased(0x022C, 0x022C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH TILDE AND MACRON
unicode_changes_when_lowercased(0x022E, 0x022E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH DOT ABOVE
unicode_changes_when_lowercased(0x0230, 0x0230).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
unicode_changes_when_lowercased(0x0232, 0x0232).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Y WITH MACRON
unicode_changes_when_lowercased(0x023A, 0x023B).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER A WITH STROKE..LATIN CAPITAL LETTER C WITH STROKE
unicode_changes_when_lowercased(0x023D, 0x023E).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER L WITH BAR..LATIN CAPITAL LETTER T WITH DIAGONAL STROKE
unicode_changes_when_lowercased(0x0241, 0x0241).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER GLOTTAL STOP
unicode_changes_when_lowercased(0x0243, 0x0246).	% Changes_When_Lowercased L&   [4] LATIN CAPITAL LETTER B WITH STROKE..LATIN CAPITAL LETTER E WITH STROKE
unicode_changes_when_lowercased(0x0248, 0x0248).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER J WITH STROKE
unicode_changes_when_lowercased(0x024A, 0x024A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER SMALL Q WITH HOOK TAIL
unicode_changes_when_lowercased(0x024C, 0x024C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER R WITH STROKE
unicode_changes_when_lowercased(0x024E, 0x024E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Y WITH STROKE
unicode_changes_when_lowercased(0x0370, 0x0370).	% Changes_When_Lowercased L&       GREEK CAPITAL LETTER HETA
unicode_changes_when_lowercased(0x0372, 0x0372).	% Changes_When_Lowercased L&       GREEK CAPITAL LETTER ARCHAIC SAMPI
unicode_changes_when_lowercased(0x0376, 0x0376).	% Changes_When_Lowercased L&       GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA
unicode_changes_when_lowercased(0x0386, 0x0386).	% Changes_When_Lowercased L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_changes_when_lowercased(0x0388, 0x038A).	% Changes_When_Lowercased L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_changes_when_lowercased(0x038C, 0x038C).	% Changes_When_Lowercased L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_changes_when_lowercased(0x038E, 0x038F).	% Changes_When_Lowercased L&   [2] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK CAPITAL LETTER OMEGA WITH TONOS
unicode_changes_when_lowercased(0x0391, 0x03A1).	% Changes_When_Lowercased L&  [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
unicode_changes_when_lowercased(0x03A3, 0x03AB).	% Changes_When_Lowercased L&   [9] GREEK CAPITAL LETTER SIGMA..GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
unicode_changes_when_lowercased(0x03CF, 0x03CF).	% Changes_When_Lowercased L&       GREEK CAPITAL KAI SYMBOL
unicode_changes_when_lowercased(0x03D8, 0x03D8).	% Changes_When_Lowercased L&       GREEK LETTER ARCHAIC KOPPA
unicode_changes_when_lowercased(0x03DA, 0x03DA).	% Changes_When_Lowercased L&       GREEK LETTER STIGMA
unicode_changes_when_lowercased(0x03DC, 0x03DC).	% Changes_When_Lowercased L&       GREEK LETTER DIGAMMA
unicode_changes_when_lowercased(0x03DE, 0x03DE).	% Changes_When_Lowercased L&       GREEK LETTER KOPPA
unicode_changes_when_lowercased(0x03E0, 0x03E0).	% Changes_When_Lowercased L&       GREEK LETTER SAMPI
unicode_changes_when_lowercased(0x03E2, 0x03E2).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER SHEI
unicode_changes_when_lowercased(0x03E4, 0x03E4).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER FEI
unicode_changes_when_lowercased(0x03E6, 0x03E6).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER KHEI
unicode_changes_when_lowercased(0x03E8, 0x03E8).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER HORI
unicode_changes_when_lowercased(0x03EA, 0x03EA).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER GANGIA
unicode_changes_when_lowercased(0x03EC, 0x03EC).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER SHIMA
unicode_changes_when_lowercased(0x03EE, 0x03EE).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER DEI
unicode_changes_when_lowercased(0x03F4, 0x03F4).	% Changes_When_Lowercased L&       GREEK CAPITAL THETA SYMBOL
unicode_changes_when_lowercased(0x03F7, 0x03F7).	% Changes_When_Lowercased L&       GREEK CAPITAL LETTER SHO
unicode_changes_when_lowercased(0x03F9, 0x03FA).	% Changes_When_Lowercased L&   [2] GREEK CAPITAL LUNATE SIGMA SYMBOL..GREEK CAPITAL LETTER SAN
unicode_changes_when_lowercased(0x03FD, 0x042F).	% Changes_When_Lowercased L&  [51] GREEK CAPITAL REVERSED LUNATE SIGMA SYMBOL..CYRILLIC CAPITAL LETTER YA
unicode_changes_when_lowercased(0x0460, 0x0460).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER OMEGA
unicode_changes_when_lowercased(0x0462, 0x0462).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER YAT
unicode_changes_when_lowercased(0x0464, 0x0464).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER IOTIFIED E
unicode_changes_when_lowercased(0x0466, 0x0466).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER LITTLE YUS
unicode_changes_when_lowercased(0x0468, 0x0468).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
unicode_changes_when_lowercased(0x046A, 0x046A).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER BIG YUS
unicode_changes_when_lowercased(0x046C, 0x046C).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
unicode_changes_when_lowercased(0x046E, 0x046E).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KSI
unicode_changes_when_lowercased(0x0470, 0x0470).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER PSI
unicode_changes_when_lowercased(0x0472, 0x0472).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER FITA
unicode_changes_when_lowercased(0x0474, 0x0474).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER IZHITSA
unicode_changes_when_lowercased(0x0476, 0x0476).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
unicode_changes_when_lowercased(0x0478, 0x0478).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER UK
unicode_changes_when_lowercased(0x047A, 0x047A).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ROUND OMEGA
unicode_changes_when_lowercased(0x047C, 0x047C).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
unicode_changes_when_lowercased(0x047E, 0x047E).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER OT
unicode_changes_when_lowercased(0x0480, 0x0480).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KOPPA
unicode_changes_when_lowercased(0x048A, 0x048A).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER SHORT I WITH TAIL
unicode_changes_when_lowercased(0x048C, 0x048C).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER SEMISOFT SIGN
unicode_changes_when_lowercased(0x048E, 0x048E).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ER WITH TICK
unicode_changes_when_lowercased(0x0490, 0x0490).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER GHE WITH UPTURN
unicode_changes_when_lowercased(0x0492, 0x0492).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER GHE WITH STROKE
unicode_changes_when_lowercased(0x0494, 0x0494).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
unicode_changes_when_lowercased(0x0496, 0x0496).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
unicode_changes_when_lowercased(0x0498, 0x0498).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
unicode_changes_when_lowercased(0x049A, 0x049A).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KA WITH DESCENDER
unicode_changes_when_lowercased(0x049C, 0x049C).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
unicode_changes_when_lowercased(0x049E, 0x049E).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KA WITH STROKE
unicode_changes_when_lowercased(0x04A0, 0x04A0).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER BASHKIR KA
unicode_changes_when_lowercased(0x04A2, 0x04A2).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER EN WITH DESCENDER
unicode_changes_when_lowercased(0x04A4, 0x04A4).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LIGATURE EN GHE
unicode_changes_when_lowercased(0x04A6, 0x04A6).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK
unicode_changes_when_lowercased(0x04A8, 0x04A8).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ABKHASIAN HA
unicode_changes_when_lowercased(0x04AA, 0x04AA).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ES WITH DESCENDER
unicode_changes_when_lowercased(0x04AC, 0x04AC).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER TE WITH DESCENDER
unicode_changes_when_lowercased(0x04AE, 0x04AE).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER STRAIGHT U
unicode_changes_when_lowercased(0x04B0, 0x04B0).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
unicode_changes_when_lowercased(0x04B2, 0x04B2).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER HA WITH DESCENDER
unicode_changes_when_lowercased(0x04B4, 0x04B4).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LIGATURE TE TSE
unicode_changes_when_lowercased(0x04B6, 0x04B6).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
unicode_changes_when_lowercased(0x04B8, 0x04B8).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
unicode_changes_when_lowercased(0x04BA, 0x04BA).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER SHHA
unicode_changes_when_lowercased(0x04BC, 0x04BC).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ABKHASIAN CHE
unicode_changes_when_lowercased(0x04BE, 0x04BE).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
unicode_changes_when_lowercased(0x04C0, 0x04C1).	% Changes_When_Lowercased L&   [2] CYRILLIC LETTER PALOCHKA..CYRILLIC CAPITAL LETTER ZHE WITH BREVE
unicode_changes_when_lowercased(0x04C3, 0x04C3).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KA WITH HOOK
unicode_changes_when_lowercased(0x04C5, 0x04C5).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER EL WITH TAIL
unicode_changes_when_lowercased(0x04C7, 0x04C7).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER EN WITH HOOK
unicode_changes_when_lowercased(0x04C9, 0x04C9).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER EN WITH TAIL
unicode_changes_when_lowercased(0x04CB, 0x04CB).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
unicode_changes_when_lowercased(0x04CD, 0x04CD).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER EM WITH TAIL
unicode_changes_when_lowercased(0x04D0, 0x04D0).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER A WITH BREVE
unicode_changes_when_lowercased(0x04D2, 0x04D2).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER A WITH DIAERESIS
unicode_changes_when_lowercased(0x04D4, 0x04D4).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LIGATURE A IE
unicode_changes_when_lowercased(0x04D6, 0x04D6).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER IE WITH BREVE
unicode_changes_when_lowercased(0x04D8, 0x04D8).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER SCHWA
unicode_changes_when_lowercased(0x04DA, 0x04DA).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
unicode_changes_when_lowercased(0x04DC, 0x04DC).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
unicode_changes_when_lowercased(0x04DE, 0x04DE).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
unicode_changes_when_lowercased(0x04E0, 0x04E0).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ABKHASIAN DZE
unicode_changes_when_lowercased(0x04E2, 0x04E2).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER I WITH MACRON
unicode_changes_when_lowercased(0x04E4, 0x04E4).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER I WITH DIAERESIS
unicode_changes_when_lowercased(0x04E6, 0x04E6).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER O WITH DIAERESIS
unicode_changes_when_lowercased(0x04E8, 0x04E8).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER BARRED O
unicode_changes_when_lowercased(0x04EA, 0x04EA).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
unicode_changes_when_lowercased(0x04EC, 0x04EC).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER E WITH DIAERESIS
unicode_changes_when_lowercased(0x04EE, 0x04EE).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER U WITH MACRON
unicode_changes_when_lowercased(0x04F0, 0x04F0).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER U WITH DIAERESIS
unicode_changes_when_lowercased(0x04F2, 0x04F2).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
unicode_changes_when_lowercased(0x04F4, 0x04F4).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
unicode_changes_when_lowercased(0x04F6, 0x04F6).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER GHE WITH DESCENDER
unicode_changes_when_lowercased(0x04F8, 0x04F8).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
unicode_changes_when_lowercased(0x04FA, 0x04FA).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER GHE WITH STROKE AND HOOK
unicode_changes_when_lowercased(0x04FC, 0x04FC).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER HA WITH HOOK
unicode_changes_when_lowercased(0x04FE, 0x04FE).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER HA WITH STROKE
unicode_changes_when_lowercased(0x0500, 0x0500).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KOMI DE
unicode_changes_when_lowercased(0x0502, 0x0502).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KOMI DJE
unicode_changes_when_lowercased(0x0504, 0x0504).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KOMI ZJE
unicode_changes_when_lowercased(0x0506, 0x0506).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KOMI DZJE
unicode_changes_when_lowercased(0x0508, 0x0508).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KOMI LJE
unicode_changes_when_lowercased(0x050A, 0x050A).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KOMI NJE
unicode_changes_when_lowercased(0x050C, 0x050C).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KOMI SJE
unicode_changes_when_lowercased(0x050E, 0x050E).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER KOMI TJE
unicode_changes_when_lowercased(0x0510, 0x0510).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER REVERSED ZE
unicode_changes_when_lowercased(0x0512, 0x0512).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER EL WITH HOOK
unicode_changes_when_lowercased(0x0514, 0x0514).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER LHA
unicode_changes_when_lowercased(0x0516, 0x0516).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER RHA
unicode_changes_when_lowercased(0x0518, 0x0518).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER YAE
unicode_changes_when_lowercased(0x051A, 0x051A).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER QA
unicode_changes_when_lowercased(0x051C, 0x051C).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER WE
unicode_changes_when_lowercased(0x051E, 0x051E).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ALEUT KA
unicode_changes_when_lowercased(0x0520, 0x0520).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER EL WITH MIDDLE HOOK
unicode_changes_when_lowercased(0x0522, 0x0522).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER EN WITH MIDDLE HOOK
unicode_changes_when_lowercased(0x0524, 0x0524).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER PE WITH DESCENDER
unicode_changes_when_lowercased(0x0526, 0x0526).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER SHHA WITH DESCENDER
unicode_changes_when_lowercased(0x0531, 0x0556).	% Changes_When_Lowercased L&  [38] ARMENIAN CAPITAL LETTER AYB..ARMENIAN CAPITAL LETTER FEH
unicode_changes_when_lowercased(0x10A0, 0x10C5).	% Changes_When_Lowercased L&  [38] GEORGIAN CAPITAL LETTER AN..GEORGIAN CAPITAL LETTER HOE
unicode_changes_when_lowercased(0x10C7, 0x10C7).	% Changes_When_Lowercased L&       GEORGIAN CAPITAL LETTER YN
unicode_changes_when_lowercased(0x10CD, 0x10CD).	% Changes_When_Lowercased L&       GEORGIAN CAPITAL LETTER AEN
unicode_changes_when_lowercased(0x1E00, 0x1E00).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH RING BELOW
unicode_changes_when_lowercased(0x1E02, 0x1E02).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER B WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E04, 0x1E04).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER B WITH DOT BELOW
unicode_changes_when_lowercased(0x1E06, 0x1E06).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER B WITH LINE BELOW
unicode_changes_when_lowercased(0x1E08, 0x1E08).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
unicode_changes_when_lowercased(0x1E0A, 0x1E0A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER D WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E0C, 0x1E0C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER D WITH DOT BELOW
unicode_changes_when_lowercased(0x1E0E, 0x1E0E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER D WITH LINE BELOW
unicode_changes_when_lowercased(0x1E10, 0x1E10).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER D WITH CEDILLA
unicode_changes_when_lowercased(0x1E12, 0x1E12).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
unicode_changes_when_lowercased(0x1E14, 0x1E14).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
unicode_changes_when_lowercased(0x1E16, 0x1E16).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
unicode_changes_when_lowercased(0x1E18, 0x1E18).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
unicode_changes_when_lowercased(0x1E1A, 0x1E1A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH TILDE BELOW
unicode_changes_when_lowercased(0x1E1C, 0x1E1C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
unicode_changes_when_lowercased(0x1E1E, 0x1E1E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER F WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E20, 0x1E20).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER G WITH MACRON
unicode_changes_when_lowercased(0x1E22, 0x1E22).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER H WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E24, 0x1E24).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER H WITH DOT BELOW
unicode_changes_when_lowercased(0x1E26, 0x1E26).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER H WITH DIAERESIS
unicode_changes_when_lowercased(0x1E28, 0x1E28).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER H WITH CEDILLA
unicode_changes_when_lowercased(0x1E2A, 0x1E2A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER H WITH BREVE BELOW
unicode_changes_when_lowercased(0x1E2C, 0x1E2C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER I WITH TILDE BELOW
unicode_changes_when_lowercased(0x1E2E, 0x1E2E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
unicode_changes_when_lowercased(0x1E30, 0x1E30).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER K WITH ACUTE
unicode_changes_when_lowercased(0x1E32, 0x1E32).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER K WITH DOT BELOW
unicode_changes_when_lowercased(0x1E34, 0x1E34).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER K WITH LINE BELOW
unicode_changes_when_lowercased(0x1E36, 0x1E36).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER L WITH DOT BELOW
unicode_changes_when_lowercased(0x1E38, 0x1E38).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
unicode_changes_when_lowercased(0x1E3A, 0x1E3A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER L WITH LINE BELOW
unicode_changes_when_lowercased(0x1E3C, 0x1E3C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
unicode_changes_when_lowercased(0x1E3E, 0x1E3E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER M WITH ACUTE
unicode_changes_when_lowercased(0x1E40, 0x1E40).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER M WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E42, 0x1E42).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER M WITH DOT BELOW
unicode_changes_when_lowercased(0x1E44, 0x1E44).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER N WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E46, 0x1E46).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER N WITH DOT BELOW
unicode_changes_when_lowercased(0x1E48, 0x1E48).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER N WITH LINE BELOW
unicode_changes_when_lowercased(0x1E4A, 0x1E4A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
unicode_changes_when_lowercased(0x1E4C, 0x1E4C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
unicode_changes_when_lowercased(0x1E4E, 0x1E4E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
unicode_changes_when_lowercased(0x1E50, 0x1E50).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
unicode_changes_when_lowercased(0x1E52, 0x1E52).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
unicode_changes_when_lowercased(0x1E54, 0x1E54).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER P WITH ACUTE
unicode_changes_when_lowercased(0x1E56, 0x1E56).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER P WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E58, 0x1E58).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER R WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E5A, 0x1E5A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER R WITH DOT BELOW
unicode_changes_when_lowercased(0x1E5C, 0x1E5C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
unicode_changes_when_lowercased(0x1E5E, 0x1E5E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER R WITH LINE BELOW
unicode_changes_when_lowercased(0x1E60, 0x1E60).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER S WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E62, 0x1E62).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER S WITH DOT BELOW
unicode_changes_when_lowercased(0x1E64, 0x1E64).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
unicode_changes_when_lowercased(0x1E66, 0x1E66).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
unicode_changes_when_lowercased(0x1E68, 0x1E68).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
unicode_changes_when_lowercased(0x1E6A, 0x1E6A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER T WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E6C, 0x1E6C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER T WITH DOT BELOW
unicode_changes_when_lowercased(0x1E6E, 0x1E6E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER T WITH LINE BELOW
unicode_changes_when_lowercased(0x1E70, 0x1E70).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
unicode_changes_when_lowercased(0x1E72, 0x1E72).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
unicode_changes_when_lowercased(0x1E74, 0x1E74).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH TILDE BELOW
unicode_changes_when_lowercased(0x1E76, 0x1E76).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
unicode_changes_when_lowercased(0x1E78, 0x1E78).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
unicode_changes_when_lowercased(0x1E7A, 0x1E7A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
unicode_changes_when_lowercased(0x1E7C, 0x1E7C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER V WITH TILDE
unicode_changes_when_lowercased(0x1E7E, 0x1E7E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER V WITH DOT BELOW
unicode_changes_when_lowercased(0x1E80, 0x1E80).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER W WITH GRAVE
unicode_changes_when_lowercased(0x1E82, 0x1E82).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER W WITH ACUTE
unicode_changes_when_lowercased(0x1E84, 0x1E84).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER W WITH DIAERESIS
unicode_changes_when_lowercased(0x1E86, 0x1E86).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER W WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E88, 0x1E88).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER W WITH DOT BELOW
unicode_changes_when_lowercased(0x1E8A, 0x1E8A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER X WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E8C, 0x1E8C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER X WITH DIAERESIS
unicode_changes_when_lowercased(0x1E8E, 0x1E8E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Y WITH DOT ABOVE
unicode_changes_when_lowercased(0x1E90, 0x1E90).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
unicode_changes_when_lowercased(0x1E92, 0x1E92).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Z WITH DOT BELOW
unicode_changes_when_lowercased(0x1E94, 0x1E94).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Z WITH LINE BELOW
unicode_changes_when_lowercased(0x1E9E, 0x1E9E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER SHARP S
unicode_changes_when_lowercased(0x1EA0, 0x1EA0).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH DOT BELOW
unicode_changes_when_lowercased(0x1EA2, 0x1EA2).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH HOOK ABOVE
unicode_changes_when_lowercased(0x1EA4, 0x1EA4).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
unicode_changes_when_lowercased(0x1EA6, 0x1EA6).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
unicode_changes_when_lowercased(0x1EA8, 0x1EA8).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
unicode_changes_when_lowercased(0x1EAA, 0x1EAA).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
unicode_changes_when_lowercased(0x1EAC, 0x1EAC).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
unicode_changes_when_lowercased(0x1EAE, 0x1EAE).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
unicode_changes_when_lowercased(0x1EB0, 0x1EB0).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
unicode_changes_when_lowercased(0x1EB2, 0x1EB2).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
unicode_changes_when_lowercased(0x1EB4, 0x1EB4).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH BREVE AND TILDE
unicode_changes_when_lowercased(0x1EB6, 0x1EB6).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
unicode_changes_when_lowercased(0x1EB8, 0x1EB8).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH DOT BELOW
unicode_changes_when_lowercased(0x1EBA, 0x1EBA).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH HOOK ABOVE
unicode_changes_when_lowercased(0x1EBC, 0x1EBC).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH TILDE
unicode_changes_when_lowercased(0x1EBE, 0x1EBE).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
unicode_changes_when_lowercased(0x1EC0, 0x1EC0).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
unicode_changes_when_lowercased(0x1EC2, 0x1EC2).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
unicode_changes_when_lowercased(0x1EC4, 0x1EC4).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
unicode_changes_when_lowercased(0x1EC6, 0x1EC6).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
unicode_changes_when_lowercased(0x1EC8, 0x1EC8).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER I WITH HOOK ABOVE
unicode_changes_when_lowercased(0x1ECA, 0x1ECA).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER I WITH DOT BELOW
unicode_changes_when_lowercased(0x1ECC, 0x1ECC).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH DOT BELOW
unicode_changes_when_lowercased(0x1ECE, 0x1ECE).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH HOOK ABOVE
unicode_changes_when_lowercased(0x1ED0, 0x1ED0).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
unicode_changes_when_lowercased(0x1ED2, 0x1ED2).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
unicode_changes_when_lowercased(0x1ED4, 0x1ED4).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
unicode_changes_when_lowercased(0x1ED6, 0x1ED6).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
unicode_changes_when_lowercased(0x1ED8, 0x1ED8).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
unicode_changes_when_lowercased(0x1EDA, 0x1EDA).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH HORN AND ACUTE
unicode_changes_when_lowercased(0x1EDC, 0x1EDC).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH HORN AND GRAVE
unicode_changes_when_lowercased(0x1EDE, 0x1EDE).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
unicode_changes_when_lowercased(0x1EE0, 0x1EE0).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH HORN AND TILDE
unicode_changes_when_lowercased(0x1EE2, 0x1EE2).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
unicode_changes_when_lowercased(0x1EE4, 0x1EE4).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH DOT BELOW
unicode_changes_when_lowercased(0x1EE6, 0x1EE6).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH HOOK ABOVE
unicode_changes_when_lowercased(0x1EE8, 0x1EE8).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH HORN AND ACUTE
unicode_changes_when_lowercased(0x1EEA, 0x1EEA).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH HORN AND GRAVE
unicode_changes_when_lowercased(0x1EEC, 0x1EEC).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
unicode_changes_when_lowercased(0x1EEE, 0x1EEE).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH HORN AND TILDE
unicode_changes_when_lowercased(0x1EF0, 0x1EF0).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
unicode_changes_when_lowercased(0x1EF2, 0x1EF2).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Y WITH GRAVE
unicode_changes_when_lowercased(0x1EF4, 0x1EF4).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Y WITH DOT BELOW
unicode_changes_when_lowercased(0x1EF6, 0x1EF6).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Y WITH HOOK ABOVE
unicode_changes_when_lowercased(0x1EF8, 0x1EF8).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Y WITH TILDE
unicode_changes_when_lowercased(0x1EFA, 0x1EFA).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER MIDDLE-WELSH LL
unicode_changes_when_lowercased(0x1EFC, 0x1EFC).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER MIDDLE-WELSH V
unicode_changes_when_lowercased(0x1EFE, 0x1EFE).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Y WITH LOOP
unicode_changes_when_lowercased(0x1F08, 0x1F0F).	% Changes_When_Lowercased L&   [8] GREEK CAPITAL LETTER ALPHA WITH PSILI..GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
unicode_changes_when_lowercased(0x1F18, 0x1F1D).	% Changes_When_Lowercased L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_changes_when_lowercased(0x1F28, 0x1F2F).	% Changes_When_Lowercased L&   [8] GREEK CAPITAL LETTER ETA WITH PSILI..GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
unicode_changes_when_lowercased(0x1F38, 0x1F3F).	% Changes_When_Lowercased L&   [8] GREEK CAPITAL LETTER IOTA WITH PSILI..GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
unicode_changes_when_lowercased(0x1F48, 0x1F4D).	% Changes_When_Lowercased L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_changes_when_lowercased(0x1F59, 0x1F59).	% Changes_When_Lowercased L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_changes_when_lowercased(0x1F5B, 0x1F5B).	% Changes_When_Lowercased L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_changes_when_lowercased(0x1F5D, 0x1F5D).	% Changes_When_Lowercased L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_changes_when_lowercased(0x1F5F, 0x1F5F).	% Changes_When_Lowercased L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_changes_when_lowercased(0x1F68, 0x1F6F).	% Changes_When_Lowercased L&   [8] GREEK CAPITAL LETTER OMEGA WITH PSILI..GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
unicode_changes_when_lowercased(0x1F88, 0x1F8F).	% Changes_When_Lowercased L&   [8] GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI..GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_changes_when_lowercased(0x1F98, 0x1F9F).	% Changes_When_Lowercased L&   [8] GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI..GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_changes_when_lowercased(0x1FA8, 0x1FAF).	% Changes_When_Lowercased L&   [8] GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI..GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_changes_when_lowercased(0x1FB8, 0x1FBC).	% Changes_When_Lowercased L&   [5] GREEK CAPITAL LETTER ALPHA WITH VRACHY..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_changes_when_lowercased(0x1FC8, 0x1FCC).	% Changes_When_Lowercased L&   [5] GREEK CAPITAL LETTER EPSILON WITH VARIA..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_changes_when_lowercased(0x1FD8, 0x1FDB).	% Changes_When_Lowercased L&   [4] GREEK CAPITAL LETTER IOTA WITH VRACHY..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_changes_when_lowercased(0x1FE8, 0x1FEC).	% Changes_When_Lowercased L&   [5] GREEK CAPITAL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_changes_when_lowercased(0x1FF8, 0x1FFC).	% Changes_When_Lowercased L&   [5] GREEK CAPITAL LETTER OMICRON WITH VARIA..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_changes_when_lowercased(0x2126, 0x2126).	% Changes_When_Lowercased L&       OHM SIGN
unicode_changes_when_lowercased(0x212A, 0x212B).	% Changes_When_Lowercased L&   [2] KELVIN SIGN..ANGSTROM SIGN
unicode_changes_when_lowercased(0x2132, 0x2132).	% Changes_When_Lowercased L&       TURNED CAPITAL F
unicode_changes_when_lowercased(0x2160, 0x216F).	% Changes_When_Lowercased Nl  [16] ROMAN NUMERAL ONE..ROMAN NUMERAL ONE THOUSAND
unicode_changes_when_lowercased(0x2183, 0x2183).	% Changes_When_Lowercased L&       ROMAN NUMERAL REVERSED ONE HUNDRED
unicode_changes_when_lowercased(0x24B6, 0x24CF).	% Changes_When_Lowercased So  [26] CIRCLED LATIN CAPITAL LETTER A..CIRCLED LATIN CAPITAL LETTER Z
unicode_changes_when_lowercased(0x2C00, 0x2C2E).	% Changes_When_Lowercased L&  [47] GLAGOLITIC CAPITAL LETTER AZU..GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_changes_when_lowercased(0x2C60, 0x2C60).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER L WITH DOUBLE BAR
unicode_changes_when_lowercased(0x2C62, 0x2C64).	% Changes_When_Lowercased L&   [3] LATIN CAPITAL LETTER L WITH MIDDLE TILDE..LATIN CAPITAL LETTER R WITH TAIL
unicode_changes_when_lowercased(0x2C67, 0x2C67).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER H WITH DESCENDER
unicode_changes_when_lowercased(0x2C69, 0x2C69).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER K WITH DESCENDER
unicode_changes_when_lowercased(0x2C6B, 0x2C6B).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Z WITH DESCENDER
unicode_changes_when_lowercased(0x2C6D, 0x2C70).	% Changes_When_Lowercased L&   [4] LATIN CAPITAL LETTER ALPHA..LATIN CAPITAL LETTER TURNED ALPHA
unicode_changes_when_lowercased(0x2C72, 0x2C72).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER W WITH HOOK
unicode_changes_when_lowercased(0x2C75, 0x2C75).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER HALF H
unicode_changes_when_lowercased(0x2C7E, 0x2C80).	% Changes_When_Lowercased L&   [3] LATIN CAPITAL LETTER S WITH SWASH TAIL..COPTIC CAPITAL LETTER ALFA
unicode_changes_when_lowercased(0x2C82, 0x2C82).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER VIDA
unicode_changes_when_lowercased(0x2C84, 0x2C84).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER GAMMA
unicode_changes_when_lowercased(0x2C86, 0x2C86).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER DALDA
unicode_changes_when_lowercased(0x2C88, 0x2C88).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER EIE
unicode_changes_when_lowercased(0x2C8A, 0x2C8A).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER SOU
unicode_changes_when_lowercased(0x2C8C, 0x2C8C).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER ZATA
unicode_changes_when_lowercased(0x2C8E, 0x2C8E).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER HATE
unicode_changes_when_lowercased(0x2C90, 0x2C90).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER THETHE
unicode_changes_when_lowercased(0x2C92, 0x2C92).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER IAUDA
unicode_changes_when_lowercased(0x2C94, 0x2C94).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER KAPA
unicode_changes_when_lowercased(0x2C96, 0x2C96).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER LAULA
unicode_changes_when_lowercased(0x2C98, 0x2C98).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER MI
unicode_changes_when_lowercased(0x2C9A, 0x2C9A).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER NI
unicode_changes_when_lowercased(0x2C9C, 0x2C9C).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER KSI
unicode_changes_when_lowercased(0x2C9E, 0x2C9E).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER O
unicode_changes_when_lowercased(0x2CA0, 0x2CA0).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER PI
unicode_changes_when_lowercased(0x2CA2, 0x2CA2).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER RO
unicode_changes_when_lowercased(0x2CA4, 0x2CA4).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER SIMA
unicode_changes_when_lowercased(0x2CA6, 0x2CA6).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER TAU
unicode_changes_when_lowercased(0x2CA8, 0x2CA8).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER UA
unicode_changes_when_lowercased(0x2CAA, 0x2CAA).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER FI
unicode_changes_when_lowercased(0x2CAC, 0x2CAC).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER KHI
unicode_changes_when_lowercased(0x2CAE, 0x2CAE).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER PSI
unicode_changes_when_lowercased(0x2CB0, 0x2CB0).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OOU
unicode_changes_when_lowercased(0x2CB2, 0x2CB2).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER DIALECT-P ALEF
unicode_changes_when_lowercased(0x2CB4, 0x2CB4).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD COPTIC AIN
unicode_changes_when_lowercased(0x2CB6, 0x2CB6).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC EIE
unicode_changes_when_lowercased(0x2CB8, 0x2CB8).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER DIALECT-P KAPA
unicode_changes_when_lowercased(0x2CBA, 0x2CBA).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER DIALECT-P NI
unicode_changes_when_lowercased(0x2CBC, 0x2CBC).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC NI
unicode_changes_when_lowercased(0x2CBE, 0x2CBE).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD COPTIC OOU
unicode_changes_when_lowercased(0x2CC0, 0x2CC0).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER SAMPI
unicode_changes_when_lowercased(0x2CC2, 0x2CC2).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER CROSSED SHEI
unicode_changes_when_lowercased(0x2CC4, 0x2CC4).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD COPTIC SHEI
unicode_changes_when_lowercased(0x2CC6, 0x2CC6).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD COPTIC ESH
unicode_changes_when_lowercased(0x2CC8, 0x2CC8).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER AKHMIMIC KHEI
unicode_changes_when_lowercased(0x2CCA, 0x2CCA).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER DIALECT-P HORI
unicode_changes_when_lowercased(0x2CCC, 0x2CCC).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD COPTIC HORI
unicode_changes_when_lowercased(0x2CCE, 0x2CCE).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD COPTIC HA
unicode_changes_when_lowercased(0x2CD0, 0x2CD0).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER L-SHAPED HA
unicode_changes_when_lowercased(0x2CD2, 0x2CD2).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD COPTIC HEI
unicode_changes_when_lowercased(0x2CD4, 0x2CD4).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD COPTIC HAT
unicode_changes_when_lowercased(0x2CD6, 0x2CD6).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD COPTIC GANGIA
unicode_changes_when_lowercased(0x2CD8, 0x2CD8).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD COPTIC DJA
unicode_changes_when_lowercased(0x2CDA, 0x2CDA).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD COPTIC SHIMA
unicode_changes_when_lowercased(0x2CDC, 0x2CDC).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD NUBIAN SHIMA
unicode_changes_when_lowercased(0x2CDE, 0x2CDE).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD NUBIAN NGI
unicode_changes_when_lowercased(0x2CE0, 0x2CE0).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD NUBIAN NYI
unicode_changes_when_lowercased(0x2CE2, 0x2CE2).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER OLD NUBIAN WAU
unicode_changes_when_lowercased(0x2CEB, 0x2CEB).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI
unicode_changes_when_lowercased(0x2CED, 0x2CED).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC GANGIA
unicode_changes_when_lowercased(0x2CF2, 0x2CF2).	% Changes_When_Lowercased L&       COPTIC CAPITAL LETTER BOHAIRIC KHEI
unicode_changes_when_lowercased(0xA640, 0xA640).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ZEMLYA
unicode_changes_when_lowercased(0xA642, 0xA642).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER DZELO
unicode_changes_when_lowercased(0xA644, 0xA644).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER REVERSED DZE
unicode_changes_when_lowercased(0xA646, 0xA646).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER IOTA
unicode_changes_when_lowercased(0xA648, 0xA648).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER DJERV
unicode_changes_when_lowercased(0xA64A, 0xA64A).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER MONOGRAPH UK
unicode_changes_when_lowercased(0xA64C, 0xA64C).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER BROAD OMEGA
unicode_changes_when_lowercased(0xA64E, 0xA64E).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER NEUTRAL YER
unicode_changes_when_lowercased(0xA650, 0xA650).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER YERU WITH BACK YER
unicode_changes_when_lowercased(0xA652, 0xA652).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER IOTIFIED YAT
unicode_changes_when_lowercased(0xA654, 0xA654).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER REVERSED YU
unicode_changes_when_lowercased(0xA656, 0xA656).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER IOTIFIED A
unicode_changes_when_lowercased(0xA658, 0xA658).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER CLOSED LITTLE YUS
unicode_changes_when_lowercased(0xA65A, 0xA65A).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER BLENDED YUS
unicode_changes_when_lowercased(0xA65C, 0xA65C).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER IOTIFIED CLOSED LITTLE YUS
unicode_changes_when_lowercased(0xA65E, 0xA65E).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER YN
unicode_changes_when_lowercased(0xA660, 0xA660).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER REVERSED TSE
unicode_changes_when_lowercased(0xA662, 0xA662).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER SOFT DE
unicode_changes_when_lowercased(0xA664, 0xA664).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER SOFT EL
unicode_changes_when_lowercased(0xA666, 0xA666).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER SOFT EM
unicode_changes_when_lowercased(0xA668, 0xA668).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER MONOCULAR O
unicode_changes_when_lowercased(0xA66A, 0xA66A).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER BINOCULAR O
unicode_changes_when_lowercased(0xA66C, 0xA66C).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER DOUBLE MONOCULAR O
unicode_changes_when_lowercased(0xA680, 0xA680).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER DWE
unicode_changes_when_lowercased(0xA682, 0xA682).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER DZWE
unicode_changes_when_lowercased(0xA684, 0xA684).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER ZHWE
unicode_changes_when_lowercased(0xA686, 0xA686).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER CCHE
unicode_changes_when_lowercased(0xA688, 0xA688).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER DZZE
unicode_changes_when_lowercased(0xA68A, 0xA68A).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER TE WITH MIDDLE HOOK
unicode_changes_when_lowercased(0xA68C, 0xA68C).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER TWE
unicode_changes_when_lowercased(0xA68E, 0xA68E).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER TSWE
unicode_changes_when_lowercased(0xA690, 0xA690).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER TSSE
unicode_changes_when_lowercased(0xA692, 0xA692).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER TCHE
unicode_changes_when_lowercased(0xA694, 0xA694).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER HWE
unicode_changes_when_lowercased(0xA696, 0xA696).	% Changes_When_Lowercased L&       CYRILLIC CAPITAL LETTER SHWE
unicode_changes_when_lowercased(0xA722, 0xA722).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF
unicode_changes_when_lowercased(0xA724, 0xA724).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER EGYPTOLOGICAL AIN
unicode_changes_when_lowercased(0xA726, 0xA726).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER HENG
unicode_changes_when_lowercased(0xA728, 0xA728).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER TZ
unicode_changes_when_lowercased(0xA72A, 0xA72A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER TRESILLO
unicode_changes_when_lowercased(0xA72C, 0xA72C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER CUATRILLO
unicode_changes_when_lowercased(0xA72E, 0xA72E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER CUATRILLO WITH COMMA
unicode_changes_when_lowercased(0xA732, 0xA732).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER AA
unicode_changes_when_lowercased(0xA734, 0xA734).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER AO
unicode_changes_when_lowercased(0xA736, 0xA736).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER AU
unicode_changes_when_lowercased(0xA738, 0xA738).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER AV
unicode_changes_when_lowercased(0xA73A, 0xA73A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER AV WITH HORIZONTAL BAR
unicode_changes_when_lowercased(0xA73C, 0xA73C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER AY
unicode_changes_when_lowercased(0xA73E, 0xA73E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER REVERSED C WITH DOT
unicode_changes_when_lowercased(0xA740, 0xA740).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER K WITH STROKE
unicode_changes_when_lowercased(0xA742, 0xA742).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER K WITH DIAGONAL STROKE
unicode_changes_when_lowercased(0xA744, 0xA744).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER K WITH STROKE AND DIAGONAL STROKE
unicode_changes_when_lowercased(0xA746, 0xA746).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER BROKEN L
unicode_changes_when_lowercased(0xA748, 0xA748).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER L WITH HIGH STROKE
unicode_changes_when_lowercased(0xA74A, 0xA74A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH LONG STROKE OVERLAY
unicode_changes_when_lowercased(0xA74C, 0xA74C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER O WITH LOOP
unicode_changes_when_lowercased(0xA74E, 0xA74E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER OO
unicode_changes_when_lowercased(0xA750, 0xA750).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER P WITH STROKE THROUGH DESCENDER
unicode_changes_when_lowercased(0xA752, 0xA752).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER P WITH FLOURISH
unicode_changes_when_lowercased(0xA754, 0xA754).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER P WITH SQUIRREL TAIL
unicode_changes_when_lowercased(0xA756, 0xA756).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Q WITH STROKE THROUGH DESCENDER
unicode_changes_when_lowercased(0xA758, 0xA758).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER Q WITH DIAGONAL STROKE
unicode_changes_when_lowercased(0xA75A, 0xA75A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER R ROTUNDA
unicode_changes_when_lowercased(0xA75C, 0xA75C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER RUM ROTUNDA
unicode_changes_when_lowercased(0xA75E, 0xA75E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER V WITH DIAGONAL STROKE
unicode_changes_when_lowercased(0xA760, 0xA760).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER VY
unicode_changes_when_lowercased(0xA762, 0xA762).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER VISIGOTHIC Z
unicode_changes_when_lowercased(0xA764, 0xA764).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER THORN WITH STROKE
unicode_changes_when_lowercased(0xA766, 0xA766).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER THORN WITH STROKE THROUGH DESCENDER
unicode_changes_when_lowercased(0xA768, 0xA768).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER VEND
unicode_changes_when_lowercased(0xA76A, 0xA76A).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER ET
unicode_changes_when_lowercased(0xA76C, 0xA76C).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER IS
unicode_changes_when_lowercased(0xA76E, 0xA76E).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER CON
unicode_changes_when_lowercased(0xA779, 0xA779).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER INSULAR D
unicode_changes_when_lowercased(0xA77B, 0xA77B).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER INSULAR F
unicode_changes_when_lowercased(0xA77D, 0xA77E).	% Changes_When_Lowercased L&   [2] LATIN CAPITAL LETTER INSULAR G..LATIN CAPITAL LETTER TURNED INSULAR G
unicode_changes_when_lowercased(0xA780, 0xA780).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER TURNED L
unicode_changes_when_lowercased(0xA782, 0xA782).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER INSULAR R
unicode_changes_when_lowercased(0xA784, 0xA784).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER INSULAR S
unicode_changes_when_lowercased(0xA786, 0xA786).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER INSULAR T
unicode_changes_when_lowercased(0xA78B, 0xA78B).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER SALTILLO
unicode_changes_when_lowercased(0xA78D, 0xA78D).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER TURNED H
unicode_changes_when_lowercased(0xA790, 0xA790).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER N WITH DESCENDER
unicode_changes_when_lowercased(0xA792, 0xA792).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER C WITH BAR
unicode_changes_when_lowercased(0xA7A0, 0xA7A0).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER G WITH OBLIQUE STROKE
unicode_changes_when_lowercased(0xA7A2, 0xA7A2).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER K WITH OBLIQUE STROKE
unicode_changes_when_lowercased(0xA7A4, 0xA7A4).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER N WITH OBLIQUE STROKE
unicode_changes_when_lowercased(0xA7A6, 0xA7A6).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER R WITH OBLIQUE STROKE
unicode_changes_when_lowercased(0xA7A8, 0xA7A8).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER S WITH OBLIQUE STROKE
unicode_changes_when_lowercased(0xA7AA, 0xA7AA).	% Changes_When_Lowercased L&       LATIN CAPITAL LETTER H WITH HOOK
unicode_changes_when_lowercased(0xFF21, 0xFF3A).	% Changes_When_Lowercased L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_changes_when_lowercased(0x10400, 0x10427).	% Changes_When_Lowercased L&  [40] DESERET CAPITAL LETTER LONG I..DESERET CAPITAL LETTER EW

% Total code points: 1043
