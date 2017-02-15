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

unicode_lowercase(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_lowercase(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_lowercase(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_lowercase(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property: Lowercase
%  Generated from: Ll + Other_Lowercase

unicode_lowercase(0x0061, 0x007A).	% Lowercase L&  [26] LATIN SMALL LETTER A..LATIN SMALL LETTER Z
unicode_lowercase(0x00AA, 0x00AA).	% Lowercase Lo       FEMININE ORDINAL INDICATOR
unicode_lowercase(0x00B5, 0x00B5).	% Lowercase L&       MICRO SIGN
unicode_lowercase(0x00BA, 0x00BA).	% Lowercase Lo       MASCULINE ORDINAL INDICATOR
unicode_lowercase(0x00DF, 0x00F6).	% Lowercase L&  [24] LATIN SMALL LETTER SHARP S..LATIN SMALL LETTER O WITH DIAERESIS
unicode_lowercase(0x00F8, 0x00FF).	% Lowercase L&   [8] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER Y WITH DIAERESIS
unicode_lowercase(0x0101, 0x0101).	% Lowercase L&       LATIN SMALL LETTER A WITH MACRON
unicode_lowercase(0x0103, 0x0103).	% Lowercase L&       LATIN SMALL LETTER A WITH BREVE
unicode_lowercase(0x0105, 0x0105).	% Lowercase L&       LATIN SMALL LETTER A WITH OGONEK
unicode_lowercase(0x0107, 0x0107).	% Lowercase L&       LATIN SMALL LETTER C WITH ACUTE
unicode_lowercase(0x0109, 0x0109).	% Lowercase L&       LATIN SMALL LETTER C WITH CIRCUMFLEX
unicode_lowercase(0x010B, 0x010B).	% Lowercase L&       LATIN SMALL LETTER C WITH DOT ABOVE
unicode_lowercase(0x010D, 0x010D).	% Lowercase L&       LATIN SMALL LETTER C WITH CARON
unicode_lowercase(0x010F, 0x010F).	% Lowercase L&       LATIN SMALL LETTER D WITH CARON
unicode_lowercase(0x0111, 0x0111).	% Lowercase L&       LATIN SMALL LETTER D WITH STROKE
unicode_lowercase(0x0113, 0x0113).	% Lowercase L&       LATIN SMALL LETTER E WITH MACRON
unicode_lowercase(0x0115, 0x0115).	% Lowercase L&       LATIN SMALL LETTER E WITH BREVE
unicode_lowercase(0x0117, 0x0117).	% Lowercase L&       LATIN SMALL LETTER E WITH DOT ABOVE
unicode_lowercase(0x0119, 0x0119).	% Lowercase L&       LATIN SMALL LETTER E WITH OGONEK
unicode_lowercase(0x011B, 0x011B).	% Lowercase L&       LATIN SMALL LETTER E WITH CARON
unicode_lowercase(0x011D, 0x011D).	% Lowercase L&       LATIN SMALL LETTER G WITH CIRCUMFLEX
unicode_lowercase(0x011F, 0x011F).	% Lowercase L&       LATIN SMALL LETTER G WITH BREVE
unicode_lowercase(0x0121, 0x0121).	% Lowercase L&       LATIN SMALL LETTER G WITH DOT ABOVE
unicode_lowercase(0x0123, 0x0123).	% Lowercase L&       LATIN SMALL LETTER G WITH CEDILLA
unicode_lowercase(0x0125, 0x0125).	% Lowercase L&       LATIN SMALL LETTER H WITH CIRCUMFLEX
unicode_lowercase(0x0127, 0x0127).	% Lowercase L&       LATIN SMALL LETTER H WITH STROKE
unicode_lowercase(0x0129, 0x0129).	% Lowercase L&       LATIN SMALL LETTER I WITH TILDE
unicode_lowercase(0x012B, 0x012B).	% Lowercase L&       LATIN SMALL LETTER I WITH MACRON
unicode_lowercase(0x012D, 0x012D).	% Lowercase L&       LATIN SMALL LETTER I WITH BREVE
unicode_lowercase(0x012F, 0x012F).	% Lowercase L&       LATIN SMALL LETTER I WITH OGONEK
unicode_lowercase(0x0131, 0x0131).	% Lowercase L&       LATIN SMALL LETTER DOTLESS I
unicode_lowercase(0x0133, 0x0133).	% Lowercase L&       LATIN SMALL LIGATURE IJ
unicode_lowercase(0x0135, 0x0135).	% Lowercase L&       LATIN SMALL LETTER J WITH CIRCUMFLEX
unicode_lowercase(0x0137, 0x0138).	% Lowercase L&   [2] LATIN SMALL LETTER K WITH CEDILLA..LATIN SMALL LETTER KRA
unicode_lowercase(0x013A, 0x013A).	% Lowercase L&       LATIN SMALL LETTER L WITH ACUTE
unicode_lowercase(0x013C, 0x013C).	% Lowercase L&       LATIN SMALL LETTER L WITH CEDILLA
unicode_lowercase(0x013E, 0x013E).	% Lowercase L&       LATIN SMALL LETTER L WITH CARON
unicode_lowercase(0x0140, 0x0140).	% Lowercase L&       LATIN SMALL LETTER L WITH MIDDLE DOT
unicode_lowercase(0x0142, 0x0142).	% Lowercase L&       LATIN SMALL LETTER L WITH STROKE
unicode_lowercase(0x0144, 0x0144).	% Lowercase L&       LATIN SMALL LETTER N WITH ACUTE
unicode_lowercase(0x0146, 0x0146).	% Lowercase L&       LATIN SMALL LETTER N WITH CEDILLA
unicode_lowercase(0x0148, 0x0149).	% Lowercase L&   [2] LATIN SMALL LETTER N WITH CARON..LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
unicode_lowercase(0x014B, 0x014B).	% Lowercase L&       LATIN SMALL LETTER ENG
unicode_lowercase(0x014D, 0x014D).	% Lowercase L&       LATIN SMALL LETTER O WITH MACRON
unicode_lowercase(0x014F, 0x014F).	% Lowercase L&       LATIN SMALL LETTER O WITH BREVE
unicode_lowercase(0x0151, 0x0151).	% Lowercase L&       LATIN SMALL LETTER O WITH DOUBLE ACUTE
unicode_lowercase(0x0153, 0x0153).	% Lowercase L&       LATIN SMALL LIGATURE OE
unicode_lowercase(0x0155, 0x0155).	% Lowercase L&       LATIN SMALL LETTER R WITH ACUTE
unicode_lowercase(0x0157, 0x0157).	% Lowercase L&       LATIN SMALL LETTER R WITH CEDILLA
unicode_lowercase(0x0159, 0x0159).	% Lowercase L&       LATIN SMALL LETTER R WITH CARON
unicode_lowercase(0x015B, 0x015B).	% Lowercase L&       LATIN SMALL LETTER S WITH ACUTE
unicode_lowercase(0x015D, 0x015D).	% Lowercase L&       LATIN SMALL LETTER S WITH CIRCUMFLEX
unicode_lowercase(0x015F, 0x015F).	% Lowercase L&       LATIN SMALL LETTER S WITH CEDILLA
unicode_lowercase(0x0161, 0x0161).	% Lowercase L&       LATIN SMALL LETTER S WITH CARON
unicode_lowercase(0x0163, 0x0163).	% Lowercase L&       LATIN SMALL LETTER T WITH CEDILLA
unicode_lowercase(0x0165, 0x0165).	% Lowercase L&       LATIN SMALL LETTER T WITH CARON
unicode_lowercase(0x0167, 0x0167).	% Lowercase L&       LATIN SMALL LETTER T WITH STROKE
unicode_lowercase(0x0169, 0x0169).	% Lowercase L&       LATIN SMALL LETTER U WITH TILDE
unicode_lowercase(0x016B, 0x016B).	% Lowercase L&       LATIN SMALL LETTER U WITH MACRON
unicode_lowercase(0x016D, 0x016D).	% Lowercase L&       LATIN SMALL LETTER U WITH BREVE
unicode_lowercase(0x016F, 0x016F).	% Lowercase L&       LATIN SMALL LETTER U WITH RING ABOVE
unicode_lowercase(0x0171, 0x0171).	% Lowercase L&       LATIN SMALL LETTER U WITH DOUBLE ACUTE
unicode_lowercase(0x0173, 0x0173).	% Lowercase L&       LATIN SMALL LETTER U WITH OGONEK
unicode_lowercase(0x0175, 0x0175).	% Lowercase L&       LATIN SMALL LETTER W WITH CIRCUMFLEX
unicode_lowercase(0x0177, 0x0177).	% Lowercase L&       LATIN SMALL LETTER Y WITH CIRCUMFLEX
unicode_lowercase(0x017A, 0x017A).	% Lowercase L&       LATIN SMALL LETTER Z WITH ACUTE
unicode_lowercase(0x017C, 0x017C).	% Lowercase L&       LATIN SMALL LETTER Z WITH DOT ABOVE
unicode_lowercase(0x017E, 0x0180).	% Lowercase L&   [3] LATIN SMALL LETTER Z WITH CARON..LATIN SMALL LETTER B WITH STROKE
unicode_lowercase(0x0183, 0x0183).	% Lowercase L&       LATIN SMALL LETTER B WITH TOPBAR
unicode_lowercase(0x0185, 0x0185).	% Lowercase L&       LATIN SMALL LETTER TONE SIX
unicode_lowercase(0x0188, 0x0188).	% Lowercase L&       LATIN SMALL LETTER C WITH HOOK
unicode_lowercase(0x018C, 0x018D).	% Lowercase L&   [2] LATIN SMALL LETTER D WITH TOPBAR..LATIN SMALL LETTER TURNED DELTA
unicode_lowercase(0x0192, 0x0192).	% Lowercase L&       LATIN SMALL LETTER F WITH HOOK
unicode_lowercase(0x0195, 0x0195).	% Lowercase L&       LATIN SMALL LETTER HV
unicode_lowercase(0x0199, 0x019B).	% Lowercase L&   [3] LATIN SMALL LETTER K WITH HOOK..LATIN SMALL LETTER LAMBDA WITH STROKE
unicode_lowercase(0x019E, 0x019E).	% Lowercase L&       LATIN SMALL LETTER N WITH LONG RIGHT LEG
unicode_lowercase(0x01A1, 0x01A1).	% Lowercase L&       LATIN SMALL LETTER O WITH HORN
unicode_lowercase(0x01A3, 0x01A3).	% Lowercase L&       LATIN SMALL LETTER OI
unicode_lowercase(0x01A5, 0x01A5).	% Lowercase L&       LATIN SMALL LETTER P WITH HOOK
unicode_lowercase(0x01A8, 0x01A8).	% Lowercase L&       LATIN SMALL LETTER TONE TWO
unicode_lowercase(0x01AA, 0x01AB).	% Lowercase L&   [2] LATIN LETTER REVERSED ESH LOOP..LATIN SMALL LETTER T WITH PALATAL HOOK
unicode_lowercase(0x01AD, 0x01AD).	% Lowercase L&       LATIN SMALL LETTER T WITH HOOK
unicode_lowercase(0x01B0, 0x01B0).	% Lowercase L&       LATIN SMALL LETTER U WITH HORN
unicode_lowercase(0x01B4, 0x01B4).	% Lowercase L&       LATIN SMALL LETTER Y WITH HOOK
unicode_lowercase(0x01B6, 0x01B6).	% Lowercase L&       LATIN SMALL LETTER Z WITH STROKE
unicode_lowercase(0x01B9, 0x01BA).	% Lowercase L&   [2] LATIN SMALL LETTER EZH REVERSED..LATIN SMALL LETTER EZH WITH TAIL
unicode_lowercase(0x01BD, 0x01BF).	% Lowercase L&   [3] LATIN SMALL LETTER TONE FIVE..LATIN LETTER WYNN
unicode_lowercase(0x01C6, 0x01C6).	% Lowercase L&       LATIN SMALL LETTER DZ WITH CARON
unicode_lowercase(0x01C9, 0x01C9).	% Lowercase L&       LATIN SMALL LETTER LJ
unicode_lowercase(0x01CC, 0x01CC).	% Lowercase L&       LATIN SMALL LETTER NJ
unicode_lowercase(0x01CE, 0x01CE).	% Lowercase L&       LATIN SMALL LETTER A WITH CARON
unicode_lowercase(0x01D0, 0x01D0).	% Lowercase L&       LATIN SMALL LETTER I WITH CARON
unicode_lowercase(0x01D2, 0x01D2).	% Lowercase L&       LATIN SMALL LETTER O WITH CARON
unicode_lowercase(0x01D4, 0x01D4).	% Lowercase L&       LATIN SMALL LETTER U WITH CARON
unicode_lowercase(0x01D6, 0x01D6).	% Lowercase L&       LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
unicode_lowercase(0x01D8, 0x01D8).	% Lowercase L&       LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
unicode_lowercase(0x01DA, 0x01DA).	% Lowercase L&       LATIN SMALL LETTER U WITH DIAERESIS AND CARON
unicode_lowercase(0x01DC, 0x01DD).	% Lowercase L&   [2] LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE..LATIN SMALL LETTER TURNED E
unicode_lowercase(0x01DF, 0x01DF).	% Lowercase L&       LATIN SMALL LETTER A WITH DIAERESIS AND MACRON
unicode_lowercase(0x01E1, 0x01E1).	% Lowercase L&       LATIN SMALL LETTER A WITH DOT ABOVE AND MACRON
unicode_lowercase(0x01E3, 0x01E3).	% Lowercase L&       LATIN SMALL LETTER AE WITH MACRON
unicode_lowercase(0x01E5, 0x01E5).	% Lowercase L&       LATIN SMALL LETTER G WITH STROKE
unicode_lowercase(0x01E7, 0x01E7).	% Lowercase L&       LATIN SMALL LETTER G WITH CARON
unicode_lowercase(0x01E9, 0x01E9).	% Lowercase L&       LATIN SMALL LETTER K WITH CARON
unicode_lowercase(0x01EB, 0x01EB).	% Lowercase L&       LATIN SMALL LETTER O WITH OGONEK
unicode_lowercase(0x01ED, 0x01ED).	% Lowercase L&       LATIN SMALL LETTER O WITH OGONEK AND MACRON
unicode_lowercase(0x01EF, 0x01F0).	% Lowercase L&   [2] LATIN SMALL LETTER EZH WITH CARON..LATIN SMALL LETTER J WITH CARON
unicode_lowercase(0x01F3, 0x01F3).	% Lowercase L&       LATIN SMALL LETTER DZ
unicode_lowercase(0x01F5, 0x01F5).	% Lowercase L&       LATIN SMALL LETTER G WITH ACUTE
unicode_lowercase(0x01F9, 0x01F9).	% Lowercase L&       LATIN SMALL LETTER N WITH GRAVE
unicode_lowercase(0x01FB, 0x01FB).	% Lowercase L&       LATIN SMALL LETTER A WITH RING ABOVE AND ACUTE
unicode_lowercase(0x01FD, 0x01FD).	% Lowercase L&       LATIN SMALL LETTER AE WITH ACUTE
unicode_lowercase(0x01FF, 0x01FF).	% Lowercase L&       LATIN SMALL LETTER O WITH STROKE AND ACUTE
unicode_lowercase(0x0201, 0x0201).	% Lowercase L&       LATIN SMALL LETTER A WITH DOUBLE GRAVE
unicode_lowercase(0x0203, 0x0203).	% Lowercase L&       LATIN SMALL LETTER A WITH INVERTED BREVE
unicode_lowercase(0x0205, 0x0205).	% Lowercase L&       LATIN SMALL LETTER E WITH DOUBLE GRAVE
unicode_lowercase(0x0207, 0x0207).	% Lowercase L&       LATIN SMALL LETTER E WITH INVERTED BREVE
unicode_lowercase(0x0209, 0x0209).	% Lowercase L&       LATIN SMALL LETTER I WITH DOUBLE GRAVE
unicode_lowercase(0x020B, 0x020B).	% Lowercase L&       LATIN SMALL LETTER I WITH INVERTED BREVE
unicode_lowercase(0x020D, 0x020D).	% Lowercase L&       LATIN SMALL LETTER O WITH DOUBLE GRAVE
unicode_lowercase(0x020F, 0x020F).	% Lowercase L&       LATIN SMALL LETTER O WITH INVERTED BREVE
unicode_lowercase(0x0211, 0x0211).	% Lowercase L&       LATIN SMALL LETTER R WITH DOUBLE GRAVE
unicode_lowercase(0x0213, 0x0213).	% Lowercase L&       LATIN SMALL LETTER R WITH INVERTED BREVE
unicode_lowercase(0x0215, 0x0215).	% Lowercase L&       LATIN SMALL LETTER U WITH DOUBLE GRAVE
unicode_lowercase(0x0217, 0x0217).	% Lowercase L&       LATIN SMALL LETTER U WITH INVERTED BREVE
unicode_lowercase(0x0219, 0x0219).	% Lowercase L&       LATIN SMALL LETTER S WITH COMMA BELOW
unicode_lowercase(0x021B, 0x021B).	% Lowercase L&       LATIN SMALL LETTER T WITH COMMA BELOW
unicode_lowercase(0x021D, 0x021D).	% Lowercase L&       LATIN SMALL LETTER YOGH
unicode_lowercase(0x021F, 0x021F).	% Lowercase L&       LATIN SMALL LETTER H WITH CARON
unicode_lowercase(0x0221, 0x0221).	% Lowercase L&       LATIN SMALL LETTER D WITH CURL
unicode_lowercase(0x0223, 0x0223).	% Lowercase L&       LATIN SMALL LETTER OU
unicode_lowercase(0x0225, 0x0225).	% Lowercase L&       LATIN SMALL LETTER Z WITH HOOK
unicode_lowercase(0x0227, 0x0227).	% Lowercase L&       LATIN SMALL LETTER A WITH DOT ABOVE
unicode_lowercase(0x0229, 0x0229).	% Lowercase L&       LATIN SMALL LETTER E WITH CEDILLA
unicode_lowercase(0x022B, 0x022B).	% Lowercase L&       LATIN SMALL LETTER O WITH DIAERESIS AND MACRON
unicode_lowercase(0x022D, 0x022D).	% Lowercase L&       LATIN SMALL LETTER O WITH TILDE AND MACRON
unicode_lowercase(0x022F, 0x022F).	% Lowercase L&       LATIN SMALL LETTER O WITH DOT ABOVE
unicode_lowercase(0x0231, 0x0231).	% Lowercase L&       LATIN SMALL LETTER O WITH DOT ABOVE AND MACRON
unicode_lowercase(0x0233, 0x0239).	% Lowercase L&   [7] LATIN SMALL LETTER Y WITH MACRON..LATIN SMALL LETTER QP DIGRAPH
unicode_lowercase(0x023C, 0x023C).	% Lowercase L&       LATIN SMALL LETTER C WITH STROKE
unicode_lowercase(0x023F, 0x0240).	% Lowercase L&   [2] LATIN SMALL LETTER S WITH SWASH TAIL..LATIN SMALL LETTER Z WITH SWASH TAIL
unicode_lowercase(0x0242, 0x0242).	% Lowercase L&       LATIN SMALL LETTER GLOTTAL STOP
unicode_lowercase(0x0247, 0x0247).	% Lowercase L&       LATIN SMALL LETTER E WITH STROKE
unicode_lowercase(0x0249, 0x0249).	% Lowercase L&       LATIN SMALL LETTER J WITH STROKE
unicode_lowercase(0x024B, 0x024B).	% Lowercase L&       LATIN SMALL LETTER Q WITH HOOK TAIL
unicode_lowercase(0x024D, 0x024D).	% Lowercase L&       LATIN SMALL LETTER R WITH STROKE
unicode_lowercase(0x024F, 0x0293).	% Lowercase L&  [69] LATIN SMALL LETTER Y WITH STROKE..LATIN SMALL LETTER EZH WITH CURL
unicode_lowercase(0x0295, 0x02AF).	% Lowercase L&  [27] LATIN LETTER PHARYNGEAL VOICED FRICATIVE..LATIN SMALL LETTER TURNED H WITH FISHHOOK AND TAIL
unicode_lowercase(0x02B0, 0x02B8).	% Lowercase Lm   [9] MODIFIER LETTER SMALL H..MODIFIER LETTER SMALL Y
unicode_lowercase(0x02C0, 0x02C1).	% Lowercase Lm   [2] MODIFIER LETTER GLOTTAL STOP..MODIFIER LETTER REVERSED GLOTTAL STOP
unicode_lowercase(0x02E0, 0x02E4).	% Lowercase Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_lowercase(0x0345, 0x0345).	% Lowercase Mn       COMBINING GREEK YPOGEGRAMMENI
unicode_lowercase(0x0371, 0x0371).	% Lowercase L&       GREEK SMALL LETTER HETA
unicode_lowercase(0x0373, 0x0373).	% Lowercase L&       GREEK SMALL LETTER ARCHAIC SAMPI
unicode_lowercase(0x0377, 0x0377).	% Lowercase L&       GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
unicode_lowercase(0x037A, 0x037A).	% Lowercase Lm       GREEK YPOGEGRAMMENI
unicode_lowercase(0x037B, 0x037D).	% Lowercase L&   [3] GREEK SMALL REVERSED LUNATE SIGMA SYMBOL..GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
unicode_lowercase(0x0390, 0x0390).	% Lowercase L&       GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
unicode_lowercase(0x03AC, 0x03CE).	% Lowercase L&  [35] GREEK SMALL LETTER ALPHA WITH TONOS..GREEK SMALL LETTER OMEGA WITH TONOS
unicode_lowercase(0x03D0, 0x03D1).	% Lowercase L&   [2] GREEK BETA SYMBOL..GREEK THETA SYMBOL
unicode_lowercase(0x03D5, 0x03D7).	% Lowercase L&   [3] GREEK PHI SYMBOL..GREEK KAI SYMBOL
unicode_lowercase(0x03D9, 0x03D9).	% Lowercase L&       GREEK SMALL LETTER ARCHAIC KOPPA
unicode_lowercase(0x03DB, 0x03DB).	% Lowercase L&       GREEK SMALL LETTER STIGMA
unicode_lowercase(0x03DD, 0x03DD).	% Lowercase L&       GREEK SMALL LETTER DIGAMMA
unicode_lowercase(0x03DF, 0x03DF).	% Lowercase L&       GREEK SMALL LETTER KOPPA
unicode_lowercase(0x03E1, 0x03E1).	% Lowercase L&       GREEK SMALL LETTER SAMPI
unicode_lowercase(0x03E3, 0x03E3).	% Lowercase L&       COPTIC SMALL LETTER SHEI
unicode_lowercase(0x03E5, 0x03E5).	% Lowercase L&       COPTIC SMALL LETTER FEI
unicode_lowercase(0x03E7, 0x03E7).	% Lowercase L&       COPTIC SMALL LETTER KHEI
unicode_lowercase(0x03E9, 0x03E9).	% Lowercase L&       COPTIC SMALL LETTER HORI
unicode_lowercase(0x03EB, 0x03EB).	% Lowercase L&       COPTIC SMALL LETTER GANGIA
unicode_lowercase(0x03ED, 0x03ED).	% Lowercase L&       COPTIC SMALL LETTER SHIMA
unicode_lowercase(0x03EF, 0x03F3).	% Lowercase L&   [5] COPTIC SMALL LETTER DEI..GREEK LETTER YOT
unicode_lowercase(0x03F5, 0x03F5).	% Lowercase L&       GREEK LUNATE EPSILON SYMBOL
unicode_lowercase(0x03F8, 0x03F8).	% Lowercase L&       GREEK SMALL LETTER SHO
unicode_lowercase(0x03FB, 0x03FC).	% Lowercase L&   [2] GREEK SMALL LETTER SAN..GREEK RHO WITH STROKE SYMBOL
unicode_lowercase(0x0430, 0x045F).	% Lowercase L&  [48] CYRILLIC SMALL LETTER A..CYRILLIC SMALL LETTER DZHE
unicode_lowercase(0x0461, 0x0461).	% Lowercase L&       CYRILLIC SMALL LETTER OMEGA
unicode_lowercase(0x0463, 0x0463).	% Lowercase L&       CYRILLIC SMALL LETTER YAT
unicode_lowercase(0x0465, 0x0465).	% Lowercase L&       CYRILLIC SMALL LETTER IOTIFIED E
unicode_lowercase(0x0467, 0x0467).	% Lowercase L&       CYRILLIC SMALL LETTER LITTLE YUS
unicode_lowercase(0x0469, 0x0469).	% Lowercase L&       CYRILLIC SMALL LETTER IOTIFIED LITTLE YUS
unicode_lowercase(0x046B, 0x046B).	% Lowercase L&       CYRILLIC SMALL LETTER BIG YUS
unicode_lowercase(0x046D, 0x046D).	% Lowercase L&       CYRILLIC SMALL LETTER IOTIFIED BIG YUS
unicode_lowercase(0x046F, 0x046F).	% Lowercase L&       CYRILLIC SMALL LETTER KSI
unicode_lowercase(0x0471, 0x0471).	% Lowercase L&       CYRILLIC SMALL LETTER PSI
unicode_lowercase(0x0473, 0x0473).	% Lowercase L&       CYRILLIC SMALL LETTER FITA
unicode_lowercase(0x0475, 0x0475).	% Lowercase L&       CYRILLIC SMALL LETTER IZHITSA
unicode_lowercase(0x0477, 0x0477).	% Lowercase L&       CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
unicode_lowercase(0x0479, 0x0479).	% Lowercase L&       CYRILLIC SMALL LETTER UK
unicode_lowercase(0x047B, 0x047B).	% Lowercase L&       CYRILLIC SMALL LETTER ROUND OMEGA
unicode_lowercase(0x047D, 0x047D).	% Lowercase L&       CYRILLIC SMALL LETTER OMEGA WITH TITLO
unicode_lowercase(0x047F, 0x047F).	% Lowercase L&       CYRILLIC SMALL LETTER OT
unicode_lowercase(0x0481, 0x0481).	% Lowercase L&       CYRILLIC SMALL LETTER KOPPA
unicode_lowercase(0x048B, 0x048B).	% Lowercase L&       CYRILLIC SMALL LETTER SHORT I WITH TAIL
unicode_lowercase(0x048D, 0x048D).	% Lowercase L&       CYRILLIC SMALL LETTER SEMISOFT SIGN
unicode_lowercase(0x048F, 0x048F).	% Lowercase L&       CYRILLIC SMALL LETTER ER WITH TICK
unicode_lowercase(0x0491, 0x0491).	% Lowercase L&       CYRILLIC SMALL LETTER GHE WITH UPTURN
unicode_lowercase(0x0493, 0x0493).	% Lowercase L&       CYRILLIC SMALL LETTER GHE WITH STROKE
unicode_lowercase(0x0495, 0x0495).	% Lowercase L&       CYRILLIC SMALL LETTER GHE WITH MIDDLE HOOK
unicode_lowercase(0x0497, 0x0497).	% Lowercase L&       CYRILLIC SMALL LETTER ZHE WITH DESCENDER
unicode_lowercase(0x0499, 0x0499).	% Lowercase L&       CYRILLIC SMALL LETTER ZE WITH DESCENDER
unicode_lowercase(0x049B, 0x049B).	% Lowercase L&       CYRILLIC SMALL LETTER KA WITH DESCENDER
unicode_lowercase(0x049D, 0x049D).	% Lowercase L&       CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
unicode_lowercase(0x049F, 0x049F).	% Lowercase L&       CYRILLIC SMALL LETTER KA WITH STROKE
unicode_lowercase(0x04A1, 0x04A1).	% Lowercase L&       CYRILLIC SMALL LETTER BASHKIR KA
unicode_lowercase(0x04A3, 0x04A3).	% Lowercase L&       CYRILLIC SMALL LETTER EN WITH DESCENDER
unicode_lowercase(0x04A5, 0x04A5).	% Lowercase L&       CYRILLIC SMALL LIGATURE EN GHE
unicode_lowercase(0x04A7, 0x04A7).	% Lowercase L&       CYRILLIC SMALL LETTER PE WITH MIDDLE HOOK
unicode_lowercase(0x04A9, 0x04A9).	% Lowercase L&       CYRILLIC SMALL LETTER ABKHASIAN HA
unicode_lowercase(0x04AB, 0x04AB).	% Lowercase L&       CYRILLIC SMALL LETTER ES WITH DESCENDER
unicode_lowercase(0x04AD, 0x04AD).	% Lowercase L&       CYRILLIC SMALL LETTER TE WITH DESCENDER
unicode_lowercase(0x04AF, 0x04AF).	% Lowercase L&       CYRILLIC SMALL LETTER STRAIGHT U
unicode_lowercase(0x04B1, 0x04B1).	% Lowercase L&       CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
unicode_lowercase(0x04B3, 0x04B3).	% Lowercase L&       CYRILLIC SMALL LETTER HA WITH DESCENDER
unicode_lowercase(0x04B5, 0x04B5).	% Lowercase L&       CYRILLIC SMALL LIGATURE TE TSE
unicode_lowercase(0x04B7, 0x04B7).	% Lowercase L&       CYRILLIC SMALL LETTER CHE WITH DESCENDER
unicode_lowercase(0x04B9, 0x04B9).	% Lowercase L&       CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
unicode_lowercase(0x04BB, 0x04BB).	% Lowercase L&       CYRILLIC SMALL LETTER SHHA
unicode_lowercase(0x04BD, 0x04BD).	% Lowercase L&       CYRILLIC SMALL LETTER ABKHASIAN CHE
unicode_lowercase(0x04BF, 0x04BF).	% Lowercase L&       CYRILLIC SMALL LETTER ABKHASIAN CHE WITH DESCENDER
unicode_lowercase(0x04C2, 0x04C2).	% Lowercase L&       CYRILLIC SMALL LETTER ZHE WITH BREVE
unicode_lowercase(0x04C4, 0x04C4).	% Lowercase L&       CYRILLIC SMALL LETTER KA WITH HOOK
unicode_lowercase(0x04C6, 0x04C6).	% Lowercase L&       CYRILLIC SMALL LETTER EL WITH TAIL
unicode_lowercase(0x04C8, 0x04C8).	% Lowercase L&       CYRILLIC SMALL LETTER EN WITH HOOK
unicode_lowercase(0x04CA, 0x04CA).	% Lowercase L&       CYRILLIC SMALL LETTER EN WITH TAIL
unicode_lowercase(0x04CC, 0x04CC).	% Lowercase L&       CYRILLIC SMALL LETTER KHAKASSIAN CHE
unicode_lowercase(0x04CE, 0x04CF).	% Lowercase L&   [2] CYRILLIC SMALL LETTER EM WITH TAIL..CYRILLIC SMALL LETTER PALOCHKA
unicode_lowercase(0x04D1, 0x04D1).	% Lowercase L&       CYRILLIC SMALL LETTER A WITH BREVE
unicode_lowercase(0x04D3, 0x04D3).	% Lowercase L&       CYRILLIC SMALL LETTER A WITH DIAERESIS
unicode_lowercase(0x04D5, 0x04D5).	% Lowercase L&       CYRILLIC SMALL LIGATURE A IE
unicode_lowercase(0x04D7, 0x04D7).	% Lowercase L&       CYRILLIC SMALL LETTER IE WITH BREVE
unicode_lowercase(0x04D9, 0x04D9).	% Lowercase L&       CYRILLIC SMALL LETTER SCHWA
unicode_lowercase(0x04DB, 0x04DB).	% Lowercase L&       CYRILLIC SMALL LETTER SCHWA WITH DIAERESIS
unicode_lowercase(0x04DD, 0x04DD).	% Lowercase L&       CYRILLIC SMALL LETTER ZHE WITH DIAERESIS
unicode_lowercase(0x04DF, 0x04DF).	% Lowercase L&       CYRILLIC SMALL LETTER ZE WITH DIAERESIS
unicode_lowercase(0x04E1, 0x04E1).	% Lowercase L&       CYRILLIC SMALL LETTER ABKHASIAN DZE
unicode_lowercase(0x04E3, 0x04E3).	% Lowercase L&       CYRILLIC SMALL LETTER I WITH MACRON
unicode_lowercase(0x04E5, 0x04E5).	% Lowercase L&       CYRILLIC SMALL LETTER I WITH DIAERESIS
unicode_lowercase(0x04E7, 0x04E7).	% Lowercase L&       CYRILLIC SMALL LETTER O WITH DIAERESIS
unicode_lowercase(0x04E9, 0x04E9).	% Lowercase L&       CYRILLIC SMALL LETTER BARRED O
unicode_lowercase(0x04EB, 0x04EB).	% Lowercase L&       CYRILLIC SMALL LETTER BARRED O WITH DIAERESIS
unicode_lowercase(0x04ED, 0x04ED).	% Lowercase L&       CYRILLIC SMALL LETTER E WITH DIAERESIS
unicode_lowercase(0x04EF, 0x04EF).	% Lowercase L&       CYRILLIC SMALL LETTER U WITH MACRON
unicode_lowercase(0x04F1, 0x04F1).	% Lowercase L&       CYRILLIC SMALL LETTER U WITH DIAERESIS
unicode_lowercase(0x04F3, 0x04F3).	% Lowercase L&       CYRILLIC SMALL LETTER U WITH DOUBLE ACUTE
unicode_lowercase(0x04F5, 0x04F5).	% Lowercase L&       CYRILLIC SMALL LETTER CHE WITH DIAERESIS
unicode_lowercase(0x04F7, 0x04F7).	% Lowercase L&       CYRILLIC SMALL LETTER GHE WITH DESCENDER
unicode_lowercase(0x04F9, 0x04F9).	% Lowercase L&       CYRILLIC SMALL LETTER YERU WITH DIAERESIS
unicode_lowercase(0x04FB, 0x04FB).	% Lowercase L&       CYRILLIC SMALL LETTER GHE WITH STROKE AND HOOK
unicode_lowercase(0x04FD, 0x04FD).	% Lowercase L&       CYRILLIC SMALL LETTER HA WITH HOOK
unicode_lowercase(0x04FF, 0x04FF).	% Lowercase L&       CYRILLIC SMALL LETTER HA WITH STROKE
unicode_lowercase(0x0501, 0x0501).	% Lowercase L&       CYRILLIC SMALL LETTER KOMI DE
unicode_lowercase(0x0503, 0x0503).	% Lowercase L&       CYRILLIC SMALL LETTER KOMI DJE
unicode_lowercase(0x0505, 0x0505).	% Lowercase L&       CYRILLIC SMALL LETTER KOMI ZJE
unicode_lowercase(0x0507, 0x0507).	% Lowercase L&       CYRILLIC SMALL LETTER KOMI DZJE
unicode_lowercase(0x0509, 0x0509).	% Lowercase L&       CYRILLIC SMALL LETTER KOMI LJE
unicode_lowercase(0x050B, 0x050B).	% Lowercase L&       CYRILLIC SMALL LETTER KOMI NJE
unicode_lowercase(0x050D, 0x050D).	% Lowercase L&       CYRILLIC SMALL LETTER KOMI SJE
unicode_lowercase(0x050F, 0x050F).	% Lowercase L&       CYRILLIC SMALL LETTER KOMI TJE
unicode_lowercase(0x0511, 0x0511).	% Lowercase L&       CYRILLIC SMALL LETTER REVERSED ZE
unicode_lowercase(0x0513, 0x0513).	% Lowercase L&       CYRILLIC SMALL LETTER EL WITH HOOK
unicode_lowercase(0x0515, 0x0515).	% Lowercase L&       CYRILLIC SMALL LETTER LHA
unicode_lowercase(0x0517, 0x0517).	% Lowercase L&       CYRILLIC SMALL LETTER RHA
unicode_lowercase(0x0519, 0x0519).	% Lowercase L&       CYRILLIC SMALL LETTER YAE
unicode_lowercase(0x051B, 0x051B).	% Lowercase L&       CYRILLIC SMALL LETTER QA
unicode_lowercase(0x051D, 0x051D).	% Lowercase L&       CYRILLIC SMALL LETTER WE
unicode_lowercase(0x051F, 0x051F).	% Lowercase L&       CYRILLIC SMALL LETTER ALEUT KA
unicode_lowercase(0x0521, 0x0521).	% Lowercase L&       CYRILLIC SMALL LETTER EL WITH MIDDLE HOOK
unicode_lowercase(0x0523, 0x0523).	% Lowercase L&       CYRILLIC SMALL LETTER EN WITH MIDDLE HOOK
unicode_lowercase(0x0525, 0x0525).	% Lowercase L&       CYRILLIC SMALL LETTER PE WITH DESCENDER
unicode_lowercase(0x0527, 0x0527).	% Lowercase L&       CYRILLIC SMALL LETTER SHHA WITH DESCENDER
unicode_lowercase(0x0561, 0x0587).	% Lowercase L&  [39] ARMENIAN SMALL LETTER AYB..ARMENIAN SMALL LIGATURE ECH YIWN
unicode_lowercase(0x1D00, 0x1D2B).	% Lowercase L&  [44] LATIN LETTER SMALL CAPITAL A..CYRILLIC LETTER SMALL CAPITAL EL
unicode_lowercase(0x1D2C, 0x1D6A).	% Lowercase Lm  [63] MODIFIER LETTER CAPITAL A..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_lowercase(0x1D6B, 0x1D77).	% Lowercase L&  [13] LATIN SMALL LETTER UE..LATIN SMALL LETTER TURNED G
unicode_lowercase(0x1D78, 0x1D78).	% Lowercase Lm       MODIFIER LETTER CYRILLIC EN
unicode_lowercase(0x1D79, 0x1D9A).	% Lowercase L&  [34] LATIN SMALL LETTER INSULAR G..LATIN SMALL LETTER EZH WITH RETROFLEX HOOK
unicode_lowercase(0x1D9B, 0x1DBF).	% Lowercase Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_lowercase(0x1E01, 0x1E01).	% Lowercase L&       LATIN SMALL LETTER A WITH RING BELOW
unicode_lowercase(0x1E03, 0x1E03).	% Lowercase L&       LATIN SMALL LETTER B WITH DOT ABOVE
unicode_lowercase(0x1E05, 0x1E05).	% Lowercase L&       LATIN SMALL LETTER B WITH DOT BELOW
unicode_lowercase(0x1E07, 0x1E07).	% Lowercase L&       LATIN SMALL LETTER B WITH LINE BELOW
unicode_lowercase(0x1E09, 0x1E09).	% Lowercase L&       LATIN SMALL LETTER C WITH CEDILLA AND ACUTE
unicode_lowercase(0x1E0B, 0x1E0B).	% Lowercase L&       LATIN SMALL LETTER D WITH DOT ABOVE
unicode_lowercase(0x1E0D, 0x1E0D).	% Lowercase L&       LATIN SMALL LETTER D WITH DOT BELOW
unicode_lowercase(0x1E0F, 0x1E0F).	% Lowercase L&       LATIN SMALL LETTER D WITH LINE BELOW
unicode_lowercase(0x1E11, 0x1E11).	% Lowercase L&       LATIN SMALL LETTER D WITH CEDILLA
unicode_lowercase(0x1E13, 0x1E13).	% Lowercase L&       LATIN SMALL LETTER D WITH CIRCUMFLEX BELOW
unicode_lowercase(0x1E15, 0x1E15).	% Lowercase L&       LATIN SMALL LETTER E WITH MACRON AND GRAVE
unicode_lowercase(0x1E17, 0x1E17).	% Lowercase L&       LATIN SMALL LETTER E WITH MACRON AND ACUTE
unicode_lowercase(0x1E19, 0x1E19).	% Lowercase L&       LATIN SMALL LETTER E WITH CIRCUMFLEX BELOW
unicode_lowercase(0x1E1B, 0x1E1B).	% Lowercase L&       LATIN SMALL LETTER E WITH TILDE BELOW
unicode_lowercase(0x1E1D, 0x1E1D).	% Lowercase L&       LATIN SMALL LETTER E WITH CEDILLA AND BREVE
unicode_lowercase(0x1E1F, 0x1E1F).	% Lowercase L&       LATIN SMALL LETTER F WITH DOT ABOVE
unicode_lowercase(0x1E21, 0x1E21).	% Lowercase L&       LATIN SMALL LETTER G WITH MACRON
unicode_lowercase(0x1E23, 0x1E23).	% Lowercase L&       LATIN SMALL LETTER H WITH DOT ABOVE
unicode_lowercase(0x1E25, 0x1E25).	% Lowercase L&       LATIN SMALL LETTER H WITH DOT BELOW
unicode_lowercase(0x1E27, 0x1E27).	% Lowercase L&       LATIN SMALL LETTER H WITH DIAERESIS
unicode_lowercase(0x1E29, 0x1E29).	% Lowercase L&       LATIN SMALL LETTER H WITH CEDILLA
unicode_lowercase(0x1E2B, 0x1E2B).	% Lowercase L&       LATIN SMALL LETTER H WITH BREVE BELOW
unicode_lowercase(0x1E2D, 0x1E2D).	% Lowercase L&       LATIN SMALL LETTER I WITH TILDE BELOW
unicode_lowercase(0x1E2F, 0x1E2F).	% Lowercase L&       LATIN SMALL LETTER I WITH DIAERESIS AND ACUTE
unicode_lowercase(0x1E31, 0x1E31).	% Lowercase L&       LATIN SMALL LETTER K WITH ACUTE
unicode_lowercase(0x1E33, 0x1E33).	% Lowercase L&       LATIN SMALL LETTER K WITH DOT BELOW
unicode_lowercase(0x1E35, 0x1E35).	% Lowercase L&       LATIN SMALL LETTER K WITH LINE BELOW
unicode_lowercase(0x1E37, 0x1E37).	% Lowercase L&       LATIN SMALL LETTER L WITH DOT BELOW
unicode_lowercase(0x1E39, 0x1E39).	% Lowercase L&       LATIN SMALL LETTER L WITH DOT BELOW AND MACRON
unicode_lowercase(0x1E3B, 0x1E3B).	% Lowercase L&       LATIN SMALL LETTER L WITH LINE BELOW
unicode_lowercase(0x1E3D, 0x1E3D).	% Lowercase L&       LATIN SMALL LETTER L WITH CIRCUMFLEX BELOW
unicode_lowercase(0x1E3F, 0x1E3F).	% Lowercase L&       LATIN SMALL LETTER M WITH ACUTE
unicode_lowercase(0x1E41, 0x1E41).	% Lowercase L&       LATIN SMALL LETTER M WITH DOT ABOVE
unicode_lowercase(0x1E43, 0x1E43).	% Lowercase L&       LATIN SMALL LETTER M WITH DOT BELOW
unicode_lowercase(0x1E45, 0x1E45).	% Lowercase L&       LATIN SMALL LETTER N WITH DOT ABOVE
unicode_lowercase(0x1E47, 0x1E47).	% Lowercase L&       LATIN SMALL LETTER N WITH DOT BELOW
unicode_lowercase(0x1E49, 0x1E49).	% Lowercase L&       LATIN SMALL LETTER N WITH LINE BELOW
unicode_lowercase(0x1E4B, 0x1E4B).	% Lowercase L&       LATIN SMALL LETTER N WITH CIRCUMFLEX BELOW
unicode_lowercase(0x1E4D, 0x1E4D).	% Lowercase L&       LATIN SMALL LETTER O WITH TILDE AND ACUTE
unicode_lowercase(0x1E4F, 0x1E4F).	% Lowercase L&       LATIN SMALL LETTER O WITH TILDE AND DIAERESIS
unicode_lowercase(0x1E51, 0x1E51).	% Lowercase L&       LATIN SMALL LETTER O WITH MACRON AND GRAVE
unicode_lowercase(0x1E53, 0x1E53).	% Lowercase L&       LATIN SMALL LETTER O WITH MACRON AND ACUTE
unicode_lowercase(0x1E55, 0x1E55).	% Lowercase L&       LATIN SMALL LETTER P WITH ACUTE
unicode_lowercase(0x1E57, 0x1E57).	% Lowercase L&       LATIN SMALL LETTER P WITH DOT ABOVE
unicode_lowercase(0x1E59, 0x1E59).	% Lowercase L&       LATIN SMALL LETTER R WITH DOT ABOVE
unicode_lowercase(0x1E5B, 0x1E5B).	% Lowercase L&       LATIN SMALL LETTER R WITH DOT BELOW
unicode_lowercase(0x1E5D, 0x1E5D).	% Lowercase L&       LATIN SMALL LETTER R WITH DOT BELOW AND MACRON
unicode_lowercase(0x1E5F, 0x1E5F).	% Lowercase L&       LATIN SMALL LETTER R WITH LINE BELOW
unicode_lowercase(0x1E61, 0x1E61).	% Lowercase L&       LATIN SMALL LETTER S WITH DOT ABOVE
unicode_lowercase(0x1E63, 0x1E63).	% Lowercase L&       LATIN SMALL LETTER S WITH DOT BELOW
unicode_lowercase(0x1E65, 0x1E65).	% Lowercase L&       LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE
unicode_lowercase(0x1E67, 0x1E67).	% Lowercase L&       LATIN SMALL LETTER S WITH CARON AND DOT ABOVE
unicode_lowercase(0x1E69, 0x1E69).	% Lowercase L&       LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
unicode_lowercase(0x1E6B, 0x1E6B).	% Lowercase L&       LATIN SMALL LETTER T WITH DOT ABOVE
unicode_lowercase(0x1E6D, 0x1E6D).	% Lowercase L&       LATIN SMALL LETTER T WITH DOT BELOW
unicode_lowercase(0x1E6F, 0x1E6F).	% Lowercase L&       LATIN SMALL LETTER T WITH LINE BELOW
unicode_lowercase(0x1E71, 0x1E71).	% Lowercase L&       LATIN SMALL LETTER T WITH CIRCUMFLEX BELOW
unicode_lowercase(0x1E73, 0x1E73).	% Lowercase L&       LATIN SMALL LETTER U WITH DIAERESIS BELOW
unicode_lowercase(0x1E75, 0x1E75).	% Lowercase L&       LATIN SMALL LETTER U WITH TILDE BELOW
unicode_lowercase(0x1E77, 0x1E77).	% Lowercase L&       LATIN SMALL LETTER U WITH CIRCUMFLEX BELOW
unicode_lowercase(0x1E79, 0x1E79).	% Lowercase L&       LATIN SMALL LETTER U WITH TILDE AND ACUTE
unicode_lowercase(0x1E7B, 0x1E7B).	% Lowercase L&       LATIN SMALL LETTER U WITH MACRON AND DIAERESIS
unicode_lowercase(0x1E7D, 0x1E7D).	% Lowercase L&       LATIN SMALL LETTER V WITH TILDE
unicode_lowercase(0x1E7F, 0x1E7F).	% Lowercase L&       LATIN SMALL LETTER V WITH DOT BELOW
unicode_lowercase(0x1E81, 0x1E81).	% Lowercase L&       LATIN SMALL LETTER W WITH GRAVE
unicode_lowercase(0x1E83, 0x1E83).	% Lowercase L&       LATIN SMALL LETTER W WITH ACUTE
unicode_lowercase(0x1E85, 0x1E85).	% Lowercase L&       LATIN SMALL LETTER W WITH DIAERESIS
unicode_lowercase(0x1E87, 0x1E87).	% Lowercase L&       LATIN SMALL LETTER W WITH DOT ABOVE
unicode_lowercase(0x1E89, 0x1E89).	% Lowercase L&       LATIN SMALL LETTER W WITH DOT BELOW
unicode_lowercase(0x1E8B, 0x1E8B).	% Lowercase L&       LATIN SMALL LETTER X WITH DOT ABOVE
unicode_lowercase(0x1E8D, 0x1E8D).	% Lowercase L&       LATIN SMALL LETTER X WITH DIAERESIS
unicode_lowercase(0x1E8F, 0x1E8F).	% Lowercase L&       LATIN SMALL LETTER Y WITH DOT ABOVE
unicode_lowercase(0x1E91, 0x1E91).	% Lowercase L&       LATIN SMALL LETTER Z WITH CIRCUMFLEX
unicode_lowercase(0x1E93, 0x1E93).	% Lowercase L&       LATIN SMALL LETTER Z WITH DOT BELOW
unicode_lowercase(0x1E95, 0x1E9D).	% Lowercase L&   [9] LATIN SMALL LETTER Z WITH LINE BELOW..LATIN SMALL LETTER LONG S WITH HIGH STROKE
unicode_lowercase(0x1E9F, 0x1E9F).	% Lowercase L&       LATIN SMALL LETTER DELTA
unicode_lowercase(0x1EA1, 0x1EA1).	% Lowercase L&       LATIN SMALL LETTER A WITH DOT BELOW
unicode_lowercase(0x1EA3, 0x1EA3).	% Lowercase L&       LATIN SMALL LETTER A WITH HOOK ABOVE
unicode_lowercase(0x1EA5, 0x1EA5).	% Lowercase L&       LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
unicode_lowercase(0x1EA7, 0x1EA7).	% Lowercase L&       LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
unicode_lowercase(0x1EA9, 0x1EA9).	% Lowercase L&       LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
unicode_lowercase(0x1EAB, 0x1EAB).	% Lowercase L&       LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
unicode_lowercase(0x1EAD, 0x1EAD).	% Lowercase L&       LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
unicode_lowercase(0x1EAF, 0x1EAF).	% Lowercase L&       LATIN SMALL LETTER A WITH BREVE AND ACUTE
unicode_lowercase(0x1EB1, 0x1EB1).	% Lowercase L&       LATIN SMALL LETTER A WITH BREVE AND GRAVE
unicode_lowercase(0x1EB3, 0x1EB3).	% Lowercase L&       LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
unicode_lowercase(0x1EB5, 0x1EB5).	% Lowercase L&       LATIN SMALL LETTER A WITH BREVE AND TILDE
unicode_lowercase(0x1EB7, 0x1EB7).	% Lowercase L&       LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
unicode_lowercase(0x1EB9, 0x1EB9).	% Lowercase L&       LATIN SMALL LETTER E WITH DOT BELOW
unicode_lowercase(0x1EBB, 0x1EBB).	% Lowercase L&       LATIN SMALL LETTER E WITH HOOK ABOVE
unicode_lowercase(0x1EBD, 0x1EBD).	% Lowercase L&       LATIN SMALL LETTER E WITH TILDE
unicode_lowercase(0x1EBF, 0x1EBF).	% Lowercase L&       LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
unicode_lowercase(0x1EC1, 0x1EC1).	% Lowercase L&       LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
unicode_lowercase(0x1EC3, 0x1EC3).	% Lowercase L&       LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
unicode_lowercase(0x1EC5, 0x1EC5).	% Lowercase L&       LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
unicode_lowercase(0x1EC7, 0x1EC7).	% Lowercase L&       LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
unicode_lowercase(0x1EC9, 0x1EC9).	% Lowercase L&       LATIN SMALL LETTER I WITH HOOK ABOVE
unicode_lowercase(0x1ECB, 0x1ECB).	% Lowercase L&       LATIN SMALL LETTER I WITH DOT BELOW
unicode_lowercase(0x1ECD, 0x1ECD).	% Lowercase L&       LATIN SMALL LETTER O WITH DOT BELOW
unicode_lowercase(0x1ECF, 0x1ECF).	% Lowercase L&       LATIN SMALL LETTER O WITH HOOK ABOVE
unicode_lowercase(0x1ED1, 0x1ED1).	% Lowercase L&       LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
unicode_lowercase(0x1ED3, 0x1ED3).	% Lowercase L&       LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
unicode_lowercase(0x1ED5, 0x1ED5).	% Lowercase L&       LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
unicode_lowercase(0x1ED7, 0x1ED7).	% Lowercase L&       LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
unicode_lowercase(0x1ED9, 0x1ED9).	% Lowercase L&       LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
unicode_lowercase(0x1EDB, 0x1EDB).	% Lowercase L&       LATIN SMALL LETTER O WITH HORN AND ACUTE
unicode_lowercase(0x1EDD, 0x1EDD).	% Lowercase L&       LATIN SMALL LETTER O WITH HORN AND GRAVE
unicode_lowercase(0x1EDF, 0x1EDF).	% Lowercase L&       LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
unicode_lowercase(0x1EE1, 0x1EE1).	% Lowercase L&       LATIN SMALL LETTER O WITH HORN AND TILDE
unicode_lowercase(0x1EE3, 0x1EE3).	% Lowercase L&       LATIN SMALL LETTER O WITH HORN AND DOT BELOW
unicode_lowercase(0x1EE5, 0x1EE5).	% Lowercase L&       LATIN SMALL LETTER U WITH DOT BELOW
unicode_lowercase(0x1EE7, 0x1EE7).	% Lowercase L&       LATIN SMALL LETTER U WITH HOOK ABOVE
unicode_lowercase(0x1EE9, 0x1EE9).	% Lowercase L&       LATIN SMALL LETTER U WITH HORN AND ACUTE
unicode_lowercase(0x1EEB, 0x1EEB).	% Lowercase L&       LATIN SMALL LETTER U WITH HORN AND GRAVE
unicode_lowercase(0x1EED, 0x1EED).	% Lowercase L&       LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
unicode_lowercase(0x1EEF, 0x1EEF).	% Lowercase L&       LATIN SMALL LETTER U WITH HORN AND TILDE
unicode_lowercase(0x1EF1, 0x1EF1).	% Lowercase L&       LATIN SMALL LETTER U WITH HORN AND DOT BELOW
unicode_lowercase(0x1EF3, 0x1EF3).	% Lowercase L&       LATIN SMALL LETTER Y WITH GRAVE
unicode_lowercase(0x1EF5, 0x1EF5).	% Lowercase L&       LATIN SMALL LETTER Y WITH DOT BELOW
unicode_lowercase(0x1EF7, 0x1EF7).	% Lowercase L&       LATIN SMALL LETTER Y WITH HOOK ABOVE
unicode_lowercase(0x1EF9, 0x1EF9).	% Lowercase L&       LATIN SMALL LETTER Y WITH TILDE
unicode_lowercase(0x1EFB, 0x1EFB).	% Lowercase L&       LATIN SMALL LETTER MIDDLE-WELSH LL
unicode_lowercase(0x1EFD, 0x1EFD).	% Lowercase L&       LATIN SMALL LETTER MIDDLE-WELSH V
unicode_lowercase(0x1EFF, 0x1F07).	% Lowercase L&   [9] LATIN SMALL LETTER Y WITH LOOP..GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
unicode_lowercase(0x1F10, 0x1F15).	% Lowercase L&   [6] GREEK SMALL LETTER EPSILON WITH PSILI..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_lowercase(0x1F20, 0x1F27).	% Lowercase L&   [8] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI
unicode_lowercase(0x1F30, 0x1F37).	% Lowercase L&   [8] GREEK SMALL LETTER IOTA WITH PSILI..GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI
unicode_lowercase(0x1F40, 0x1F45).	% Lowercase L&   [6] GREEK SMALL LETTER OMICRON WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_lowercase(0x1F50, 0x1F57).	% Lowercase L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_lowercase(0x1F60, 0x1F67).	% Lowercase L&   [8] GREEK SMALL LETTER OMEGA WITH PSILI..GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI
unicode_lowercase(0x1F70, 0x1F7D).	% Lowercase L&  [14] GREEK SMALL LETTER ALPHA WITH VARIA..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_lowercase(0x1F80, 0x1F87).	% Lowercase L&   [8] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_lowercase(0x1F90, 0x1F97).	% Lowercase L&   [8] GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_lowercase(0x1FA0, 0x1FA7).	% Lowercase L&   [8] GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_lowercase(0x1FB0, 0x1FB4).	% Lowercase L&   [5] GREEK SMALL LETTER ALPHA WITH VRACHY..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_lowercase(0x1FB6, 0x1FB7).	% Lowercase L&   [2] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_lowercase(0x1FBE, 0x1FBE).	% Lowercase L&       GREEK PROSGEGRAMMENI
unicode_lowercase(0x1FC2, 0x1FC4).	% Lowercase L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_lowercase(0x1FC6, 0x1FC7).	% Lowercase L&   [2] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_lowercase(0x1FD0, 0x1FD3).	% Lowercase L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_lowercase(0x1FD6, 0x1FD7).	% Lowercase L&   [2] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
unicode_lowercase(0x1FE0, 0x1FE7).	% Lowercase L&   [8] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
unicode_lowercase(0x1FF2, 0x1FF4).	% Lowercase L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_lowercase(0x1FF6, 0x1FF7).	% Lowercase L&   [2] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_lowercase(0x2071, 0x2071).	% Lowercase Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_lowercase(0x207F, 0x207F).	% Lowercase Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_lowercase(0x2090, 0x209C).	% Lowercase Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_lowercase(0x210A, 0x210A).	% Lowercase L&       SCRIPT SMALL G
unicode_lowercase(0x210E, 0x210F).	% Lowercase L&   [2] PLANCK CONSTANT..PLANCK CONSTANT OVER TWO PI
unicode_lowercase(0x2113, 0x2113).	% Lowercase L&       SCRIPT SMALL L
unicode_lowercase(0x212F, 0x212F).	% Lowercase L&       SCRIPT SMALL E
unicode_lowercase(0x2134, 0x2134).	% Lowercase L&       SCRIPT SMALL O
unicode_lowercase(0x2139, 0x2139).	% Lowercase L&       INFORMATION SOURCE
unicode_lowercase(0x213C, 0x213D).	% Lowercase L&   [2] DOUBLE-STRUCK SMALL PI..DOUBLE-STRUCK SMALL GAMMA
unicode_lowercase(0x2146, 0x2149).	% Lowercase L&   [4] DOUBLE-STRUCK ITALIC SMALL D..DOUBLE-STRUCK ITALIC SMALL J
unicode_lowercase(0x214E, 0x214E).	% Lowercase L&       TURNED SMALL F
unicode_lowercase(0x2170, 0x217F).	% Lowercase Nl  [16] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL ONE THOUSAND
unicode_lowercase(0x2184, 0x2184).	% Lowercase L&       LATIN SMALL LETTER REVERSED C
unicode_lowercase(0x24D0, 0x24E9).	% Lowercase So  [26] CIRCLED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_lowercase(0x2C30, 0x2C5E).	% Lowercase L&  [47] GLAGOLITIC SMALL LETTER AZU..GLAGOLITIC SMALL LETTER LATINATE MYSLITE
unicode_lowercase(0x2C61, 0x2C61).	% Lowercase L&       LATIN SMALL LETTER L WITH DOUBLE BAR
unicode_lowercase(0x2C65, 0x2C66).	% Lowercase L&   [2] LATIN SMALL LETTER A WITH STROKE..LATIN SMALL LETTER T WITH DIAGONAL STROKE
unicode_lowercase(0x2C68, 0x2C68).	% Lowercase L&       LATIN SMALL LETTER H WITH DESCENDER
unicode_lowercase(0x2C6A, 0x2C6A).	% Lowercase L&       LATIN SMALL LETTER K WITH DESCENDER
unicode_lowercase(0x2C6C, 0x2C6C).	% Lowercase L&       LATIN SMALL LETTER Z WITH DESCENDER
unicode_lowercase(0x2C71, 0x2C71).	% Lowercase L&       LATIN SMALL LETTER V WITH RIGHT HOOK
unicode_lowercase(0x2C73, 0x2C74).	% Lowercase L&   [2] LATIN SMALL LETTER W WITH HOOK..LATIN SMALL LETTER V WITH CURL
unicode_lowercase(0x2C76, 0x2C7B).	% Lowercase L&   [6] LATIN SMALL LETTER HALF H..LATIN LETTER SMALL CAPITAL TURNED E
unicode_lowercase(0x2C7C, 0x2C7D).	% Lowercase Lm   [2] LATIN SUBSCRIPT SMALL LETTER J..MODIFIER LETTER CAPITAL V
unicode_lowercase(0x2C81, 0x2C81).	% Lowercase L&       COPTIC SMALL LETTER ALFA
unicode_lowercase(0x2C83, 0x2C83).	% Lowercase L&       COPTIC SMALL LETTER VIDA
unicode_lowercase(0x2C85, 0x2C85).	% Lowercase L&       COPTIC SMALL LETTER GAMMA
unicode_lowercase(0x2C87, 0x2C87).	% Lowercase L&       COPTIC SMALL LETTER DALDA
unicode_lowercase(0x2C89, 0x2C89).	% Lowercase L&       COPTIC SMALL LETTER EIE
unicode_lowercase(0x2C8B, 0x2C8B).	% Lowercase L&       COPTIC SMALL LETTER SOU
unicode_lowercase(0x2C8D, 0x2C8D).	% Lowercase L&       COPTIC SMALL LETTER ZATA
unicode_lowercase(0x2C8F, 0x2C8F).	% Lowercase L&       COPTIC SMALL LETTER HATE
unicode_lowercase(0x2C91, 0x2C91).	% Lowercase L&       COPTIC SMALL LETTER THETHE
unicode_lowercase(0x2C93, 0x2C93).	% Lowercase L&       COPTIC SMALL LETTER IAUDA
unicode_lowercase(0x2C95, 0x2C95).	% Lowercase L&       COPTIC SMALL LETTER KAPA
unicode_lowercase(0x2C97, 0x2C97).	% Lowercase L&       COPTIC SMALL LETTER LAULA
unicode_lowercase(0x2C99, 0x2C99).	% Lowercase L&       COPTIC SMALL LETTER MI
unicode_lowercase(0x2C9B, 0x2C9B).	% Lowercase L&       COPTIC SMALL LETTER NI
unicode_lowercase(0x2C9D, 0x2C9D).	% Lowercase L&       COPTIC SMALL LETTER KSI
unicode_lowercase(0x2C9F, 0x2C9F).	% Lowercase L&       COPTIC SMALL LETTER O
unicode_lowercase(0x2CA1, 0x2CA1).	% Lowercase L&       COPTIC SMALL LETTER PI
unicode_lowercase(0x2CA3, 0x2CA3).	% Lowercase L&       COPTIC SMALL LETTER RO
unicode_lowercase(0x2CA5, 0x2CA5).	% Lowercase L&       COPTIC SMALL LETTER SIMA
unicode_lowercase(0x2CA7, 0x2CA7).	% Lowercase L&       COPTIC SMALL LETTER TAU
unicode_lowercase(0x2CA9, 0x2CA9).	% Lowercase L&       COPTIC SMALL LETTER UA
unicode_lowercase(0x2CAB, 0x2CAB).	% Lowercase L&       COPTIC SMALL LETTER FI
unicode_lowercase(0x2CAD, 0x2CAD).	% Lowercase L&       COPTIC SMALL LETTER KHI
unicode_lowercase(0x2CAF, 0x2CAF).	% Lowercase L&       COPTIC SMALL LETTER PSI
unicode_lowercase(0x2CB1, 0x2CB1).	% Lowercase L&       COPTIC SMALL LETTER OOU
unicode_lowercase(0x2CB3, 0x2CB3).	% Lowercase L&       COPTIC SMALL LETTER DIALECT-P ALEF
unicode_lowercase(0x2CB5, 0x2CB5).	% Lowercase L&       COPTIC SMALL LETTER OLD COPTIC AIN
unicode_lowercase(0x2CB7, 0x2CB7).	% Lowercase L&       COPTIC SMALL LETTER CRYPTOGRAMMIC EIE
unicode_lowercase(0x2CB9, 0x2CB9).	% Lowercase L&       COPTIC SMALL LETTER DIALECT-P KAPA
unicode_lowercase(0x2CBB, 0x2CBB).	% Lowercase L&       COPTIC SMALL LETTER DIALECT-P NI
unicode_lowercase(0x2CBD, 0x2CBD).	% Lowercase L&       COPTIC SMALL LETTER CRYPTOGRAMMIC NI
unicode_lowercase(0x2CBF, 0x2CBF).	% Lowercase L&       COPTIC SMALL LETTER OLD COPTIC OOU
unicode_lowercase(0x2CC1, 0x2CC1).	% Lowercase L&       COPTIC SMALL LETTER SAMPI
unicode_lowercase(0x2CC3, 0x2CC3).	% Lowercase L&       COPTIC SMALL LETTER CROSSED SHEI
unicode_lowercase(0x2CC5, 0x2CC5).	% Lowercase L&       COPTIC SMALL LETTER OLD COPTIC SHEI
unicode_lowercase(0x2CC7, 0x2CC7).	% Lowercase L&       COPTIC SMALL LETTER OLD COPTIC ESH
unicode_lowercase(0x2CC9, 0x2CC9).	% Lowercase L&       COPTIC SMALL LETTER AKHMIMIC KHEI
unicode_lowercase(0x2CCB, 0x2CCB).	% Lowercase L&       COPTIC SMALL LETTER DIALECT-P HORI
unicode_lowercase(0x2CCD, 0x2CCD).	% Lowercase L&       COPTIC SMALL LETTER OLD COPTIC HORI
unicode_lowercase(0x2CCF, 0x2CCF).	% Lowercase L&       COPTIC SMALL LETTER OLD COPTIC HA
unicode_lowercase(0x2CD1, 0x2CD1).	% Lowercase L&       COPTIC SMALL LETTER L-SHAPED HA
unicode_lowercase(0x2CD3, 0x2CD3).	% Lowercase L&       COPTIC SMALL LETTER OLD COPTIC HEI
unicode_lowercase(0x2CD5, 0x2CD5).	% Lowercase L&       COPTIC SMALL LETTER OLD COPTIC HAT
unicode_lowercase(0x2CD7, 0x2CD7).	% Lowercase L&       COPTIC SMALL LETTER OLD COPTIC GANGIA
unicode_lowercase(0x2CD9, 0x2CD9).	% Lowercase L&       COPTIC SMALL LETTER OLD COPTIC DJA
unicode_lowercase(0x2CDB, 0x2CDB).	% Lowercase L&       COPTIC SMALL LETTER OLD COPTIC SHIMA
unicode_lowercase(0x2CDD, 0x2CDD).	% Lowercase L&       COPTIC SMALL LETTER OLD NUBIAN SHIMA
unicode_lowercase(0x2CDF, 0x2CDF).	% Lowercase L&       COPTIC SMALL LETTER OLD NUBIAN NGI
unicode_lowercase(0x2CE1, 0x2CE1).	% Lowercase L&       COPTIC SMALL LETTER OLD NUBIAN NYI
unicode_lowercase(0x2CE3, 0x2CE4).	% Lowercase L&   [2] COPTIC SMALL LETTER OLD NUBIAN WAU..COPTIC SYMBOL KAI
unicode_lowercase(0x2CEC, 0x2CEC).	% Lowercase L&       COPTIC SMALL LETTER CRYPTOGRAMMIC SHEI
unicode_lowercase(0x2CEE, 0x2CEE).	% Lowercase L&       COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
unicode_lowercase(0x2CF3, 0x2CF3).	% Lowercase L&       COPTIC SMALL LETTER BOHAIRIC KHEI
unicode_lowercase(0x2D00, 0x2D25).	% Lowercase L&  [38] GEORGIAN SMALL LETTER AN..GEORGIAN SMALL LETTER HOE
unicode_lowercase(0x2D27, 0x2D27).	% Lowercase L&       GEORGIAN SMALL LETTER YN
unicode_lowercase(0x2D2D, 0x2D2D).	% Lowercase L&       GEORGIAN SMALL LETTER AEN
unicode_lowercase(0xA641, 0xA641).	% Lowercase L&       CYRILLIC SMALL LETTER ZEMLYA
unicode_lowercase(0xA643, 0xA643).	% Lowercase L&       CYRILLIC SMALL LETTER DZELO
unicode_lowercase(0xA645, 0xA645).	% Lowercase L&       CYRILLIC SMALL LETTER REVERSED DZE
unicode_lowercase(0xA647, 0xA647).	% Lowercase L&       CYRILLIC SMALL LETTER IOTA
unicode_lowercase(0xA649, 0xA649).	% Lowercase L&       CYRILLIC SMALL LETTER DJERV
unicode_lowercase(0xA64B, 0xA64B).	% Lowercase L&       CYRILLIC SMALL LETTER MONOGRAPH UK
unicode_lowercase(0xA64D, 0xA64D).	% Lowercase L&       CYRILLIC SMALL LETTER BROAD OMEGA
unicode_lowercase(0xA64F, 0xA64F).	% Lowercase L&       CYRILLIC SMALL LETTER NEUTRAL YER
unicode_lowercase(0xA651, 0xA651).	% Lowercase L&       CYRILLIC SMALL LETTER YERU WITH BACK YER
unicode_lowercase(0xA653, 0xA653).	% Lowercase L&       CYRILLIC SMALL LETTER IOTIFIED YAT
unicode_lowercase(0xA655, 0xA655).	% Lowercase L&       CYRILLIC SMALL LETTER REVERSED YU
unicode_lowercase(0xA657, 0xA657).	% Lowercase L&       CYRILLIC SMALL LETTER IOTIFIED A
unicode_lowercase(0xA659, 0xA659).	% Lowercase L&       CYRILLIC SMALL LETTER CLOSED LITTLE YUS
unicode_lowercase(0xA65B, 0xA65B).	% Lowercase L&       CYRILLIC SMALL LETTER BLENDED YUS
unicode_lowercase(0xA65D, 0xA65D).	% Lowercase L&       CYRILLIC SMALL LETTER IOTIFIED CLOSED LITTLE YUS
unicode_lowercase(0xA65F, 0xA65F).	% Lowercase L&       CYRILLIC SMALL LETTER YN
unicode_lowercase(0xA661, 0xA661).	% Lowercase L&       CYRILLIC SMALL LETTER REVERSED TSE
unicode_lowercase(0xA663, 0xA663).	% Lowercase L&       CYRILLIC SMALL LETTER SOFT DE
unicode_lowercase(0xA665, 0xA665).	% Lowercase L&       CYRILLIC SMALL LETTER SOFT EL
unicode_lowercase(0xA667, 0xA667).	% Lowercase L&       CYRILLIC SMALL LETTER SOFT EM
unicode_lowercase(0xA669, 0xA669).	% Lowercase L&       CYRILLIC SMALL LETTER MONOCULAR O
unicode_lowercase(0xA66B, 0xA66B).	% Lowercase L&       CYRILLIC SMALL LETTER BINOCULAR O
unicode_lowercase(0xA66D, 0xA66D).	% Lowercase L&       CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
unicode_lowercase(0xA681, 0xA681).	% Lowercase L&       CYRILLIC SMALL LETTER DWE
unicode_lowercase(0xA683, 0xA683).	% Lowercase L&       CYRILLIC SMALL LETTER DZWE
unicode_lowercase(0xA685, 0xA685).	% Lowercase L&       CYRILLIC SMALL LETTER ZHWE
unicode_lowercase(0xA687, 0xA687).	% Lowercase L&       CYRILLIC SMALL LETTER CCHE
unicode_lowercase(0xA689, 0xA689).	% Lowercase L&       CYRILLIC SMALL LETTER DZZE
unicode_lowercase(0xA68B, 0xA68B).	% Lowercase L&       CYRILLIC SMALL LETTER TE WITH MIDDLE HOOK
unicode_lowercase(0xA68D, 0xA68D).	% Lowercase L&       CYRILLIC SMALL LETTER TWE
unicode_lowercase(0xA68F, 0xA68F).	% Lowercase L&       CYRILLIC SMALL LETTER TSWE
unicode_lowercase(0xA691, 0xA691).	% Lowercase L&       CYRILLIC SMALL LETTER TSSE
unicode_lowercase(0xA693, 0xA693).	% Lowercase L&       CYRILLIC SMALL LETTER TCHE
unicode_lowercase(0xA695, 0xA695).	% Lowercase L&       CYRILLIC SMALL LETTER HWE
unicode_lowercase(0xA697, 0xA697).	% Lowercase L&       CYRILLIC SMALL LETTER SHWE
unicode_lowercase(0xA723, 0xA723).	% Lowercase L&       LATIN SMALL LETTER EGYPTOLOGICAL ALEF
unicode_lowercase(0xA725, 0xA725).	% Lowercase L&       LATIN SMALL LETTER EGYPTOLOGICAL AIN
unicode_lowercase(0xA727, 0xA727).	% Lowercase L&       LATIN SMALL LETTER HENG
unicode_lowercase(0xA729, 0xA729).	% Lowercase L&       LATIN SMALL LETTER TZ
unicode_lowercase(0xA72B, 0xA72B).	% Lowercase L&       LATIN SMALL LETTER TRESILLO
unicode_lowercase(0xA72D, 0xA72D).	% Lowercase L&       LATIN SMALL LETTER CUATRILLO
unicode_lowercase(0xA72F, 0xA731).	% Lowercase L&   [3] LATIN SMALL LETTER CUATRILLO WITH COMMA..LATIN LETTER SMALL CAPITAL S
unicode_lowercase(0xA733, 0xA733).	% Lowercase L&       LATIN SMALL LETTER AA
unicode_lowercase(0xA735, 0xA735).	% Lowercase L&       LATIN SMALL LETTER AO
unicode_lowercase(0xA737, 0xA737).	% Lowercase L&       LATIN SMALL LETTER AU
unicode_lowercase(0xA739, 0xA739).	% Lowercase L&       LATIN SMALL LETTER AV
unicode_lowercase(0xA73B, 0xA73B).	% Lowercase L&       LATIN SMALL LETTER AV WITH HORIZONTAL BAR
unicode_lowercase(0xA73D, 0xA73D).	% Lowercase L&       LATIN SMALL LETTER AY
unicode_lowercase(0xA73F, 0xA73F).	% Lowercase L&       LATIN SMALL LETTER REVERSED C WITH DOT
unicode_lowercase(0xA741, 0xA741).	% Lowercase L&       LATIN SMALL LETTER K WITH STROKE
unicode_lowercase(0xA743, 0xA743).	% Lowercase L&       LATIN SMALL LETTER K WITH DIAGONAL STROKE
unicode_lowercase(0xA745, 0xA745).	% Lowercase L&       LATIN SMALL LETTER K WITH STROKE AND DIAGONAL STROKE
unicode_lowercase(0xA747, 0xA747).	% Lowercase L&       LATIN SMALL LETTER BROKEN L
unicode_lowercase(0xA749, 0xA749).	% Lowercase L&       LATIN SMALL LETTER L WITH HIGH STROKE
unicode_lowercase(0xA74B, 0xA74B).	% Lowercase L&       LATIN SMALL LETTER O WITH LONG STROKE OVERLAY
unicode_lowercase(0xA74D, 0xA74D).	% Lowercase L&       LATIN SMALL LETTER O WITH LOOP
unicode_lowercase(0xA74F, 0xA74F).	% Lowercase L&       LATIN SMALL LETTER OO
unicode_lowercase(0xA751, 0xA751).	% Lowercase L&       LATIN SMALL LETTER P WITH STROKE THROUGH DESCENDER
unicode_lowercase(0xA753, 0xA753).	% Lowercase L&       LATIN SMALL LETTER P WITH FLOURISH
unicode_lowercase(0xA755, 0xA755).	% Lowercase L&       LATIN SMALL LETTER P WITH SQUIRREL TAIL
unicode_lowercase(0xA757, 0xA757).	% Lowercase L&       LATIN SMALL LETTER Q WITH STROKE THROUGH DESCENDER
unicode_lowercase(0xA759, 0xA759).	% Lowercase L&       LATIN SMALL LETTER Q WITH DIAGONAL STROKE
unicode_lowercase(0xA75B, 0xA75B).	% Lowercase L&       LATIN SMALL LETTER R ROTUNDA
unicode_lowercase(0xA75D, 0xA75D).	% Lowercase L&       LATIN SMALL LETTER RUM ROTUNDA
unicode_lowercase(0xA75F, 0xA75F).	% Lowercase L&       LATIN SMALL LETTER V WITH DIAGONAL STROKE
unicode_lowercase(0xA761, 0xA761).	% Lowercase L&       LATIN SMALL LETTER VY
unicode_lowercase(0xA763, 0xA763).	% Lowercase L&       LATIN SMALL LETTER VISIGOTHIC Z
unicode_lowercase(0xA765, 0xA765).	% Lowercase L&       LATIN SMALL LETTER THORN WITH STROKE
unicode_lowercase(0xA767, 0xA767).	% Lowercase L&       LATIN SMALL LETTER THORN WITH STROKE THROUGH DESCENDER
unicode_lowercase(0xA769, 0xA769).	% Lowercase L&       LATIN SMALL LETTER VEND
unicode_lowercase(0xA76B, 0xA76B).	% Lowercase L&       LATIN SMALL LETTER ET
unicode_lowercase(0xA76D, 0xA76D).	% Lowercase L&       LATIN SMALL LETTER IS
unicode_lowercase(0xA76F, 0xA76F).	% Lowercase L&       LATIN SMALL LETTER CON
unicode_lowercase(0xA770, 0xA770).	% Lowercase Lm       MODIFIER LETTER US
unicode_lowercase(0xA771, 0xA778).	% Lowercase L&   [8] LATIN SMALL LETTER DUM..LATIN SMALL LETTER UM
unicode_lowercase(0xA77A, 0xA77A).	% Lowercase L&       LATIN SMALL LETTER INSULAR D
unicode_lowercase(0xA77C, 0xA77C).	% Lowercase L&       LATIN SMALL LETTER INSULAR F
unicode_lowercase(0xA77F, 0xA77F).	% Lowercase L&       LATIN SMALL LETTER TURNED INSULAR G
unicode_lowercase(0xA781, 0xA781).	% Lowercase L&       LATIN SMALL LETTER TURNED L
unicode_lowercase(0xA783, 0xA783).	% Lowercase L&       LATIN SMALL LETTER INSULAR R
unicode_lowercase(0xA785, 0xA785).	% Lowercase L&       LATIN SMALL LETTER INSULAR S
unicode_lowercase(0xA787, 0xA787).	% Lowercase L&       LATIN SMALL LETTER INSULAR T
unicode_lowercase(0xA78C, 0xA78C).	% Lowercase L&       LATIN SMALL LETTER SALTILLO
unicode_lowercase(0xA78E, 0xA78E).	% Lowercase L&       LATIN SMALL LETTER L WITH RETROFLEX HOOK AND BELT
unicode_lowercase(0xA791, 0xA791).	% Lowercase L&       LATIN SMALL LETTER N WITH DESCENDER
unicode_lowercase(0xA793, 0xA793).	% Lowercase L&       LATIN SMALL LETTER C WITH BAR
unicode_lowercase(0xA7A1, 0xA7A1).	% Lowercase L&       LATIN SMALL LETTER G WITH OBLIQUE STROKE
unicode_lowercase(0xA7A3, 0xA7A3).	% Lowercase L&       LATIN SMALL LETTER K WITH OBLIQUE STROKE
unicode_lowercase(0xA7A5, 0xA7A5).	% Lowercase L&       LATIN SMALL LETTER N WITH OBLIQUE STROKE
unicode_lowercase(0xA7A7, 0xA7A7).	% Lowercase L&       LATIN SMALL LETTER R WITH OBLIQUE STROKE
unicode_lowercase(0xA7A9, 0xA7A9).	% Lowercase L&       LATIN SMALL LETTER S WITH OBLIQUE STROKE
unicode_lowercase(0xA7F8, 0xA7F9).	% Lowercase Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_lowercase(0xA7FA, 0xA7FA).	% Lowercase L&       LATIN LETTER SMALL CAPITAL TURNED M
unicode_lowercase(0xFB00, 0xFB06).	% Lowercase L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_lowercase(0xFB13, 0xFB17).	% Lowercase L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_lowercase(0xFF41, 0xFF5A).	% Lowercase L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_lowercase(0x10428, 0x1044F).	% Lowercase L&  [40] DESERET SMALL LETTER LONG I..DESERET SMALL LETTER EW
unicode_lowercase(0x1D41A, 0x1D433).	% Lowercase L&  [26] MATHEMATICAL BOLD SMALL A..MATHEMATICAL BOLD SMALL Z
unicode_lowercase(0x1D44E, 0x1D454).	% Lowercase L&   [7] MATHEMATICAL ITALIC SMALL A..MATHEMATICAL ITALIC SMALL G
unicode_lowercase(0x1D456, 0x1D467).	% Lowercase L&  [18] MATHEMATICAL ITALIC SMALL I..MATHEMATICAL ITALIC SMALL Z
unicode_lowercase(0x1D482, 0x1D49B).	% Lowercase L&  [26] MATHEMATICAL BOLD ITALIC SMALL A..MATHEMATICAL BOLD ITALIC SMALL Z
unicode_lowercase(0x1D4B6, 0x1D4B9).	% Lowercase L&   [4] MATHEMATICAL SCRIPT SMALL A..MATHEMATICAL SCRIPT SMALL D
unicode_lowercase(0x1D4BB, 0x1D4BB).	% Lowercase L&       MATHEMATICAL SCRIPT SMALL F
unicode_lowercase(0x1D4BD, 0x1D4C3).	% Lowercase L&   [7] MATHEMATICAL SCRIPT SMALL H..MATHEMATICAL SCRIPT SMALL N
unicode_lowercase(0x1D4C5, 0x1D4CF).	% Lowercase L&  [11] MATHEMATICAL SCRIPT SMALL P..MATHEMATICAL SCRIPT SMALL Z
unicode_lowercase(0x1D4EA, 0x1D503).	% Lowercase L&  [26] MATHEMATICAL BOLD SCRIPT SMALL A..MATHEMATICAL BOLD SCRIPT SMALL Z
unicode_lowercase(0x1D51E, 0x1D537).	% Lowercase L&  [26] MATHEMATICAL FRAKTUR SMALL A..MATHEMATICAL FRAKTUR SMALL Z
unicode_lowercase(0x1D552, 0x1D56B).	% Lowercase L&  [26] MATHEMATICAL DOUBLE-STRUCK SMALL A..MATHEMATICAL DOUBLE-STRUCK SMALL Z
unicode_lowercase(0x1D586, 0x1D59F).	% Lowercase L&  [26] MATHEMATICAL BOLD FRAKTUR SMALL A..MATHEMATICAL BOLD FRAKTUR SMALL Z
unicode_lowercase(0x1D5BA, 0x1D5D3).	% Lowercase L&  [26] MATHEMATICAL SANS-SERIF SMALL A..MATHEMATICAL SANS-SERIF SMALL Z
unicode_lowercase(0x1D5EE, 0x1D607).	% Lowercase L&  [26] MATHEMATICAL SANS-SERIF BOLD SMALL A..MATHEMATICAL SANS-SERIF BOLD SMALL Z
unicode_lowercase(0x1D622, 0x1D63B).	% Lowercase L&  [26] MATHEMATICAL SANS-SERIF ITALIC SMALL A..MATHEMATICAL SANS-SERIF ITALIC SMALL Z
unicode_lowercase(0x1D656, 0x1D66F).	% Lowercase L&  [26] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL A..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Z
unicode_lowercase(0x1D68A, 0x1D6A5).	% Lowercase L&  [28] MATHEMATICAL MONOSPACE SMALL A..MATHEMATICAL ITALIC SMALL DOTLESS J
unicode_lowercase(0x1D6C2, 0x1D6DA).	% Lowercase L&  [25] MATHEMATICAL BOLD SMALL ALPHA..MATHEMATICAL BOLD SMALL OMEGA
unicode_lowercase(0x1D6DC, 0x1D6E1).	% Lowercase L&   [6] MATHEMATICAL BOLD EPSILON SYMBOL..MATHEMATICAL BOLD PI SYMBOL
unicode_lowercase(0x1D6FC, 0x1D714).	% Lowercase L&  [25] MATHEMATICAL ITALIC SMALL ALPHA..MATHEMATICAL ITALIC SMALL OMEGA
unicode_lowercase(0x1D716, 0x1D71B).	% Lowercase L&   [6] MATHEMATICAL ITALIC EPSILON SYMBOL..MATHEMATICAL ITALIC PI SYMBOL
unicode_lowercase(0x1D736, 0x1D74E).	% Lowercase L&  [25] MATHEMATICAL BOLD ITALIC SMALL ALPHA..MATHEMATICAL BOLD ITALIC SMALL OMEGA
unicode_lowercase(0x1D750, 0x1D755).	% Lowercase L&   [6] MATHEMATICAL BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD ITALIC PI SYMBOL
unicode_lowercase(0x1D770, 0x1D788).	% Lowercase L&  [25] MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
unicode_lowercase(0x1D78A, 0x1D78F).	% Lowercase L&   [6] MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD PI SYMBOL
unicode_lowercase(0x1D7AA, 0x1D7C2).	% Lowercase L&  [25] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
unicode_lowercase(0x1D7C4, 0x1D7C9).	% Lowercase L&   [6] MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD ITALIC PI SYMBOL
unicode_lowercase(0x1D7CB, 0x1D7CB).	% Lowercase L&       MATHEMATICAL BOLD SMALL DIGAMMA

% Total code points: 1934
