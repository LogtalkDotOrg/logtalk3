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

unicode_changes_when_titlecased(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_changes_when_titlecased(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_changes_when_titlecased(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_changes_when_titlecased(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property:   Changes_When_Titlecased (CWT)
%  Characters whose normalized forms are not stable under a toTitlecase mapping.
%  For more information, see D126 in Section 3.13, "Default Case Algorithms".
%  Changes_When_Titlecased(X) is true when toTitlecase(toNFD(X)) != toNFD(X)

unicode_changes_when_titlecased(0x0061, 0x007A).	% Changes_When_Titlecased L&  [26] LATIN SMALL LETTER A..LATIN SMALL LETTER Z
unicode_changes_when_titlecased(0x00B5, 0x00B5).	% Changes_When_Titlecased L&       MICRO SIGN
unicode_changes_when_titlecased(0x00DF, 0x00F6).	% Changes_When_Titlecased L&  [24] LATIN SMALL LETTER SHARP S..LATIN SMALL LETTER O WITH DIAERESIS
unicode_changes_when_titlecased(0x00F8, 0x00FF).	% Changes_When_Titlecased L&   [8] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER Y WITH DIAERESIS
unicode_changes_when_titlecased(0x0101, 0x0101).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH MACRON
unicode_changes_when_titlecased(0x0103, 0x0103).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH BREVE
unicode_changes_when_titlecased(0x0105, 0x0105).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH OGONEK
unicode_changes_when_titlecased(0x0107, 0x0107).	% Changes_When_Titlecased L&       LATIN SMALL LETTER C WITH ACUTE
unicode_changes_when_titlecased(0x0109, 0x0109).	% Changes_When_Titlecased L&       LATIN SMALL LETTER C WITH CIRCUMFLEX
unicode_changes_when_titlecased(0x010B, 0x010B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER C WITH DOT ABOVE
unicode_changes_when_titlecased(0x010D, 0x010D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER C WITH CARON
unicode_changes_when_titlecased(0x010F, 0x010F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER D WITH CARON
unicode_changes_when_titlecased(0x0111, 0x0111).	% Changes_When_Titlecased L&       LATIN SMALL LETTER D WITH STROKE
unicode_changes_when_titlecased(0x0113, 0x0113).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH MACRON
unicode_changes_when_titlecased(0x0115, 0x0115).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH BREVE
unicode_changes_when_titlecased(0x0117, 0x0117).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH DOT ABOVE
unicode_changes_when_titlecased(0x0119, 0x0119).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH OGONEK
unicode_changes_when_titlecased(0x011B, 0x011B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH CARON
unicode_changes_when_titlecased(0x011D, 0x011D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER G WITH CIRCUMFLEX
unicode_changes_when_titlecased(0x011F, 0x011F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER G WITH BREVE
unicode_changes_when_titlecased(0x0121, 0x0121).	% Changes_When_Titlecased L&       LATIN SMALL LETTER G WITH DOT ABOVE
unicode_changes_when_titlecased(0x0123, 0x0123).	% Changes_When_Titlecased L&       LATIN SMALL LETTER G WITH CEDILLA
unicode_changes_when_titlecased(0x0125, 0x0125).	% Changes_When_Titlecased L&       LATIN SMALL LETTER H WITH CIRCUMFLEX
unicode_changes_when_titlecased(0x0127, 0x0127).	% Changes_When_Titlecased L&       LATIN SMALL LETTER H WITH STROKE
unicode_changes_when_titlecased(0x0129, 0x0129).	% Changes_When_Titlecased L&       LATIN SMALL LETTER I WITH TILDE
unicode_changes_when_titlecased(0x012B, 0x012B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER I WITH MACRON
unicode_changes_when_titlecased(0x012D, 0x012D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER I WITH BREVE
unicode_changes_when_titlecased(0x012F, 0x012F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER I WITH OGONEK
unicode_changes_when_titlecased(0x0131, 0x0131).	% Changes_When_Titlecased L&       LATIN SMALL LETTER DOTLESS I
unicode_changes_when_titlecased(0x0133, 0x0133).	% Changes_When_Titlecased L&       LATIN SMALL LIGATURE IJ
unicode_changes_when_titlecased(0x0135, 0x0135).	% Changes_When_Titlecased L&       LATIN SMALL LETTER J WITH CIRCUMFLEX
unicode_changes_when_titlecased(0x0137, 0x0137).	% Changes_When_Titlecased L&       LATIN SMALL LETTER K WITH CEDILLA
unicode_changes_when_titlecased(0x013A, 0x013A).	% Changes_When_Titlecased L&       LATIN SMALL LETTER L WITH ACUTE
unicode_changes_when_titlecased(0x013C, 0x013C).	% Changes_When_Titlecased L&       LATIN SMALL LETTER L WITH CEDILLA
unicode_changes_when_titlecased(0x013E, 0x013E).	% Changes_When_Titlecased L&       LATIN SMALL LETTER L WITH CARON
unicode_changes_when_titlecased(0x0140, 0x0140).	% Changes_When_Titlecased L&       LATIN SMALL LETTER L WITH MIDDLE DOT
unicode_changes_when_titlecased(0x0142, 0x0142).	% Changes_When_Titlecased L&       LATIN SMALL LETTER L WITH STROKE
unicode_changes_when_titlecased(0x0144, 0x0144).	% Changes_When_Titlecased L&       LATIN SMALL LETTER N WITH ACUTE
unicode_changes_when_titlecased(0x0146, 0x0146).	% Changes_When_Titlecased L&       LATIN SMALL LETTER N WITH CEDILLA
unicode_changes_when_titlecased(0x0148, 0x0149).	% Changes_When_Titlecased L&   [2] LATIN SMALL LETTER N WITH CARON..LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
unicode_changes_when_titlecased(0x014B, 0x014B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER ENG
unicode_changes_when_titlecased(0x014D, 0x014D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH MACRON
unicode_changes_when_titlecased(0x014F, 0x014F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH BREVE
unicode_changes_when_titlecased(0x0151, 0x0151).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH DOUBLE ACUTE
unicode_changes_when_titlecased(0x0153, 0x0153).	% Changes_When_Titlecased L&       LATIN SMALL LIGATURE OE
unicode_changes_when_titlecased(0x0155, 0x0155).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R WITH ACUTE
unicode_changes_when_titlecased(0x0157, 0x0157).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R WITH CEDILLA
unicode_changes_when_titlecased(0x0159, 0x0159).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R WITH CARON
unicode_changes_when_titlecased(0x015B, 0x015B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER S WITH ACUTE
unicode_changes_when_titlecased(0x015D, 0x015D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER S WITH CIRCUMFLEX
unicode_changes_when_titlecased(0x015F, 0x015F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER S WITH CEDILLA
unicode_changes_when_titlecased(0x0161, 0x0161).	% Changes_When_Titlecased L&       LATIN SMALL LETTER S WITH CARON
unicode_changes_when_titlecased(0x0163, 0x0163).	% Changes_When_Titlecased L&       LATIN SMALL LETTER T WITH CEDILLA
unicode_changes_when_titlecased(0x0165, 0x0165).	% Changes_When_Titlecased L&       LATIN SMALL LETTER T WITH CARON
unicode_changes_when_titlecased(0x0167, 0x0167).	% Changes_When_Titlecased L&       LATIN SMALL LETTER T WITH STROKE
unicode_changes_when_titlecased(0x0169, 0x0169).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH TILDE
unicode_changes_when_titlecased(0x016B, 0x016B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH MACRON
unicode_changes_when_titlecased(0x016D, 0x016D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH BREVE
unicode_changes_when_titlecased(0x016F, 0x016F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH RING ABOVE
unicode_changes_when_titlecased(0x0171, 0x0171).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH DOUBLE ACUTE
unicode_changes_when_titlecased(0x0173, 0x0173).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH OGONEK
unicode_changes_when_titlecased(0x0175, 0x0175).	% Changes_When_Titlecased L&       LATIN SMALL LETTER W WITH CIRCUMFLEX
unicode_changes_when_titlecased(0x0177, 0x0177).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Y WITH CIRCUMFLEX
unicode_changes_when_titlecased(0x017A, 0x017A).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Z WITH ACUTE
unicode_changes_when_titlecased(0x017C, 0x017C).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Z WITH DOT ABOVE
unicode_changes_when_titlecased(0x017E, 0x0180).	% Changes_When_Titlecased L&   [3] LATIN SMALL LETTER Z WITH CARON..LATIN SMALL LETTER B WITH STROKE
unicode_changes_when_titlecased(0x0183, 0x0183).	% Changes_When_Titlecased L&       LATIN SMALL LETTER B WITH TOPBAR
unicode_changes_when_titlecased(0x0185, 0x0185).	% Changes_When_Titlecased L&       LATIN SMALL LETTER TONE SIX
unicode_changes_when_titlecased(0x0188, 0x0188).	% Changes_When_Titlecased L&       LATIN SMALL LETTER C WITH HOOK
unicode_changes_when_titlecased(0x018C, 0x018C).	% Changes_When_Titlecased L&       LATIN SMALL LETTER D WITH TOPBAR
unicode_changes_when_titlecased(0x0192, 0x0192).	% Changes_When_Titlecased L&       LATIN SMALL LETTER F WITH HOOK
unicode_changes_when_titlecased(0x0195, 0x0195).	% Changes_When_Titlecased L&       LATIN SMALL LETTER HV
unicode_changes_when_titlecased(0x0199, 0x019A).	% Changes_When_Titlecased L&   [2] LATIN SMALL LETTER K WITH HOOK..LATIN SMALL LETTER L WITH BAR
unicode_changes_when_titlecased(0x019E, 0x019E).	% Changes_When_Titlecased L&       LATIN SMALL LETTER N WITH LONG RIGHT LEG
unicode_changes_when_titlecased(0x01A1, 0x01A1).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH HORN
unicode_changes_when_titlecased(0x01A3, 0x01A3).	% Changes_When_Titlecased L&       LATIN SMALL LETTER OI
unicode_changes_when_titlecased(0x01A5, 0x01A5).	% Changes_When_Titlecased L&       LATIN SMALL LETTER P WITH HOOK
unicode_changes_when_titlecased(0x01A8, 0x01A8).	% Changes_When_Titlecased L&       LATIN SMALL LETTER TONE TWO
unicode_changes_when_titlecased(0x01AD, 0x01AD).	% Changes_When_Titlecased L&       LATIN SMALL LETTER T WITH HOOK
unicode_changes_when_titlecased(0x01B0, 0x01B0).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH HORN
unicode_changes_when_titlecased(0x01B4, 0x01B4).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Y WITH HOOK
unicode_changes_when_titlecased(0x01B6, 0x01B6).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Z WITH STROKE
unicode_changes_when_titlecased(0x01B9, 0x01B9).	% Changes_When_Titlecased L&       LATIN SMALL LETTER EZH REVERSED
unicode_changes_when_titlecased(0x01BD, 0x01BD).	% Changes_When_Titlecased L&       LATIN SMALL LETTER TONE FIVE
unicode_changes_when_titlecased(0x01BF, 0x01BF).	% Changes_When_Titlecased L&       LATIN LETTER WYNN
unicode_changes_when_titlecased(0x01C4, 0x01C4).	% Changes_When_Titlecased L&       LATIN CAPITAL LETTER DZ WITH CARON
unicode_changes_when_titlecased(0x01C6, 0x01C7).	% Changes_When_Titlecased L&   [2] LATIN SMALL LETTER DZ WITH CARON..LATIN CAPITAL LETTER LJ
unicode_changes_when_titlecased(0x01C9, 0x01CA).	% Changes_When_Titlecased L&   [2] LATIN SMALL LETTER LJ..LATIN CAPITAL LETTER NJ
unicode_changes_when_titlecased(0x01CC, 0x01CC).	% Changes_When_Titlecased L&       LATIN SMALL LETTER NJ
unicode_changes_when_titlecased(0x01CE, 0x01CE).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH CARON
unicode_changes_when_titlecased(0x01D0, 0x01D0).	% Changes_When_Titlecased L&       LATIN SMALL LETTER I WITH CARON
unicode_changes_when_titlecased(0x01D2, 0x01D2).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH CARON
unicode_changes_when_titlecased(0x01D4, 0x01D4).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH CARON
unicode_changes_when_titlecased(0x01D6, 0x01D6).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
unicode_changes_when_titlecased(0x01D8, 0x01D8).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
unicode_changes_when_titlecased(0x01DA, 0x01DA).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH DIAERESIS AND CARON
unicode_changes_when_titlecased(0x01DC, 0x01DD).	% Changes_When_Titlecased L&   [2] LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE..LATIN SMALL LETTER TURNED E
unicode_changes_when_titlecased(0x01DF, 0x01DF).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH DIAERESIS AND MACRON
unicode_changes_when_titlecased(0x01E1, 0x01E1).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH DOT ABOVE AND MACRON
unicode_changes_when_titlecased(0x01E3, 0x01E3).	% Changes_When_Titlecased L&       LATIN SMALL LETTER AE WITH MACRON
unicode_changes_when_titlecased(0x01E5, 0x01E5).	% Changes_When_Titlecased L&       LATIN SMALL LETTER G WITH STROKE
unicode_changes_when_titlecased(0x01E7, 0x01E7).	% Changes_When_Titlecased L&       LATIN SMALL LETTER G WITH CARON
unicode_changes_when_titlecased(0x01E9, 0x01E9).	% Changes_When_Titlecased L&       LATIN SMALL LETTER K WITH CARON
unicode_changes_when_titlecased(0x01EB, 0x01EB).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH OGONEK
unicode_changes_when_titlecased(0x01ED, 0x01ED).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH OGONEK AND MACRON
unicode_changes_when_titlecased(0x01EF, 0x01F1).	% Changes_When_Titlecased L&   [3] LATIN SMALL LETTER EZH WITH CARON..LATIN CAPITAL LETTER DZ
unicode_changes_when_titlecased(0x01F3, 0x01F3).	% Changes_When_Titlecased L&       LATIN SMALL LETTER DZ
unicode_changes_when_titlecased(0x01F5, 0x01F5).	% Changes_When_Titlecased L&       LATIN SMALL LETTER G WITH ACUTE
unicode_changes_when_titlecased(0x01F9, 0x01F9).	% Changes_When_Titlecased L&       LATIN SMALL LETTER N WITH GRAVE
unicode_changes_when_titlecased(0x01FB, 0x01FB).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH RING ABOVE AND ACUTE
unicode_changes_when_titlecased(0x01FD, 0x01FD).	% Changes_When_Titlecased L&       LATIN SMALL LETTER AE WITH ACUTE
unicode_changes_when_titlecased(0x01FF, 0x01FF).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH STROKE AND ACUTE
unicode_changes_when_titlecased(0x0201, 0x0201).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH DOUBLE GRAVE
unicode_changes_when_titlecased(0x0203, 0x0203).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH INVERTED BREVE
unicode_changes_when_titlecased(0x0205, 0x0205).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH DOUBLE GRAVE
unicode_changes_when_titlecased(0x0207, 0x0207).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH INVERTED BREVE
unicode_changes_when_titlecased(0x0209, 0x0209).	% Changes_When_Titlecased L&       LATIN SMALL LETTER I WITH DOUBLE GRAVE
unicode_changes_when_titlecased(0x020B, 0x020B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER I WITH INVERTED BREVE
unicode_changes_when_titlecased(0x020D, 0x020D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH DOUBLE GRAVE
unicode_changes_when_titlecased(0x020F, 0x020F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH INVERTED BREVE
unicode_changes_when_titlecased(0x0211, 0x0211).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R WITH DOUBLE GRAVE
unicode_changes_when_titlecased(0x0213, 0x0213).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R WITH INVERTED BREVE
unicode_changes_when_titlecased(0x0215, 0x0215).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH DOUBLE GRAVE
unicode_changes_when_titlecased(0x0217, 0x0217).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH INVERTED BREVE
unicode_changes_when_titlecased(0x0219, 0x0219).	% Changes_When_Titlecased L&       LATIN SMALL LETTER S WITH COMMA BELOW
unicode_changes_when_titlecased(0x021B, 0x021B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER T WITH COMMA BELOW
unicode_changes_when_titlecased(0x021D, 0x021D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER YOGH
unicode_changes_when_titlecased(0x021F, 0x021F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER H WITH CARON
unicode_changes_when_titlecased(0x0223, 0x0223).	% Changes_When_Titlecased L&       LATIN SMALL LETTER OU
unicode_changes_when_titlecased(0x0225, 0x0225).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Z WITH HOOK
unicode_changes_when_titlecased(0x0227, 0x0227).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH DOT ABOVE
unicode_changes_when_titlecased(0x0229, 0x0229).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH CEDILLA
unicode_changes_when_titlecased(0x022B, 0x022B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH DIAERESIS AND MACRON
unicode_changes_when_titlecased(0x022D, 0x022D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH TILDE AND MACRON
unicode_changes_when_titlecased(0x022F, 0x022F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH DOT ABOVE
unicode_changes_when_titlecased(0x0231, 0x0231).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH DOT ABOVE AND MACRON
unicode_changes_when_titlecased(0x0233, 0x0233).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Y WITH MACRON
unicode_changes_when_titlecased(0x023C, 0x023C).	% Changes_When_Titlecased L&       LATIN SMALL LETTER C WITH STROKE
unicode_changes_when_titlecased(0x023F, 0x0240).	% Changes_When_Titlecased L&   [2] LATIN SMALL LETTER S WITH SWASH TAIL..LATIN SMALL LETTER Z WITH SWASH TAIL
unicode_changes_when_titlecased(0x0242, 0x0242).	% Changes_When_Titlecased L&       LATIN SMALL LETTER GLOTTAL STOP
unicode_changes_when_titlecased(0x0247, 0x0247).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH STROKE
unicode_changes_when_titlecased(0x0249, 0x0249).	% Changes_When_Titlecased L&       LATIN SMALL LETTER J WITH STROKE
unicode_changes_when_titlecased(0x024B, 0x024B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Q WITH HOOK TAIL
unicode_changes_when_titlecased(0x024D, 0x024D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R WITH STROKE
unicode_changes_when_titlecased(0x024F, 0x0254).	% Changes_When_Titlecased L&   [6] LATIN SMALL LETTER Y WITH STROKE..LATIN SMALL LETTER OPEN O
unicode_changes_when_titlecased(0x0256, 0x0257).	% Changes_When_Titlecased L&   [2] LATIN SMALL LETTER D WITH TAIL..LATIN SMALL LETTER D WITH HOOK
unicode_changes_when_titlecased(0x0259, 0x0259).	% Changes_When_Titlecased L&       LATIN SMALL LETTER SCHWA
unicode_changes_when_titlecased(0x025B, 0x025B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER OPEN E
unicode_changes_when_titlecased(0x0260, 0x0260).	% Changes_When_Titlecased L&       LATIN SMALL LETTER G WITH HOOK
unicode_changes_when_titlecased(0x0263, 0x0263).	% Changes_When_Titlecased L&       LATIN SMALL LETTER GAMMA
unicode_changes_when_titlecased(0x0265, 0x0266).	% Changes_When_Titlecased L&   [2] LATIN SMALL LETTER TURNED H..LATIN SMALL LETTER H WITH HOOK
unicode_changes_when_titlecased(0x0268, 0x0269).	% Changes_When_Titlecased L&   [2] LATIN SMALL LETTER I WITH STROKE..LATIN SMALL LETTER IOTA
unicode_changes_when_titlecased(0x026B, 0x026B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER L WITH MIDDLE TILDE
unicode_changes_when_titlecased(0x026F, 0x026F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER TURNED M
unicode_changes_when_titlecased(0x0271, 0x0272).	% Changes_When_Titlecased L&   [2] LATIN SMALL LETTER M WITH HOOK..LATIN SMALL LETTER N WITH LEFT HOOK
unicode_changes_when_titlecased(0x0275, 0x0275).	% Changes_When_Titlecased L&       LATIN SMALL LETTER BARRED O
unicode_changes_when_titlecased(0x027D, 0x027D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R WITH TAIL
unicode_changes_when_titlecased(0x0280, 0x0280).	% Changes_When_Titlecased L&       LATIN LETTER SMALL CAPITAL R
unicode_changes_when_titlecased(0x0283, 0x0283).	% Changes_When_Titlecased L&       LATIN SMALL LETTER ESH
unicode_changes_when_titlecased(0x0288, 0x028C).	% Changes_When_Titlecased L&   [5] LATIN SMALL LETTER T WITH RETROFLEX HOOK..LATIN SMALL LETTER TURNED V
unicode_changes_when_titlecased(0x0292, 0x0292).	% Changes_When_Titlecased L&       LATIN SMALL LETTER EZH
unicode_changes_when_titlecased(0x0345, 0x0345).	% Changes_When_Titlecased Mn       COMBINING GREEK YPOGEGRAMMENI
unicode_changes_when_titlecased(0x0371, 0x0371).	% Changes_When_Titlecased L&       GREEK SMALL LETTER HETA
unicode_changes_when_titlecased(0x0373, 0x0373).	% Changes_When_Titlecased L&       GREEK SMALL LETTER ARCHAIC SAMPI
unicode_changes_when_titlecased(0x0377, 0x0377).	% Changes_When_Titlecased L&       GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
unicode_changes_when_titlecased(0x037B, 0x037D).	% Changes_When_Titlecased L&   [3] GREEK SMALL REVERSED LUNATE SIGMA SYMBOL..GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
unicode_changes_when_titlecased(0x0390, 0x0390).	% Changes_When_Titlecased L&       GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
unicode_changes_when_titlecased(0x03AC, 0x03CE).	% Changes_When_Titlecased L&  [35] GREEK SMALL LETTER ALPHA WITH TONOS..GREEK SMALL LETTER OMEGA WITH TONOS
unicode_changes_when_titlecased(0x03D0, 0x03D1).	% Changes_When_Titlecased L&   [2] GREEK BETA SYMBOL..GREEK THETA SYMBOL
unicode_changes_when_titlecased(0x03D5, 0x03D7).	% Changes_When_Titlecased L&   [3] GREEK PHI SYMBOL..GREEK KAI SYMBOL
unicode_changes_when_titlecased(0x03D9, 0x03D9).	% Changes_When_Titlecased L&       GREEK SMALL LETTER ARCHAIC KOPPA
unicode_changes_when_titlecased(0x03DB, 0x03DB).	% Changes_When_Titlecased L&       GREEK SMALL LETTER STIGMA
unicode_changes_when_titlecased(0x03DD, 0x03DD).	% Changes_When_Titlecased L&       GREEK SMALL LETTER DIGAMMA
unicode_changes_when_titlecased(0x03DF, 0x03DF).	% Changes_When_Titlecased L&       GREEK SMALL LETTER KOPPA
unicode_changes_when_titlecased(0x03E1, 0x03E1).	% Changes_When_Titlecased L&       GREEK SMALL LETTER SAMPI
unicode_changes_when_titlecased(0x03E3, 0x03E3).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER SHEI
unicode_changes_when_titlecased(0x03E5, 0x03E5).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER FEI
unicode_changes_when_titlecased(0x03E7, 0x03E7).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER KHEI
unicode_changes_when_titlecased(0x03E9, 0x03E9).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER HORI
unicode_changes_when_titlecased(0x03EB, 0x03EB).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER GANGIA
unicode_changes_when_titlecased(0x03ED, 0x03ED).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER SHIMA
unicode_changes_when_titlecased(0x03EF, 0x03F2).	% Changes_When_Titlecased L&   [4] COPTIC SMALL LETTER DEI..GREEK LUNATE SIGMA SYMBOL
unicode_changes_when_titlecased(0x03F5, 0x03F5).	% Changes_When_Titlecased L&       GREEK LUNATE EPSILON SYMBOL
unicode_changes_when_titlecased(0x03F8, 0x03F8).	% Changes_When_Titlecased L&       GREEK SMALL LETTER SHO
unicode_changes_when_titlecased(0x03FB, 0x03FB).	% Changes_When_Titlecased L&       GREEK SMALL LETTER SAN
unicode_changes_when_titlecased(0x0430, 0x045F).	% Changes_When_Titlecased L&  [48] CYRILLIC SMALL LETTER A..CYRILLIC SMALL LETTER DZHE
unicode_changes_when_titlecased(0x0461, 0x0461).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER OMEGA
unicode_changes_when_titlecased(0x0463, 0x0463).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER YAT
unicode_changes_when_titlecased(0x0465, 0x0465).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER IOTIFIED E
unicode_changes_when_titlecased(0x0467, 0x0467).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER LITTLE YUS
unicode_changes_when_titlecased(0x0469, 0x0469).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER IOTIFIED LITTLE YUS
unicode_changes_when_titlecased(0x046B, 0x046B).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER BIG YUS
unicode_changes_when_titlecased(0x046D, 0x046D).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER IOTIFIED BIG YUS
unicode_changes_when_titlecased(0x046F, 0x046F).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KSI
unicode_changes_when_titlecased(0x0471, 0x0471).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER PSI
unicode_changes_when_titlecased(0x0473, 0x0473).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER FITA
unicode_changes_when_titlecased(0x0475, 0x0475).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER IZHITSA
unicode_changes_when_titlecased(0x0477, 0x0477).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
unicode_changes_when_titlecased(0x0479, 0x0479).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER UK
unicode_changes_when_titlecased(0x047B, 0x047B).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ROUND OMEGA
unicode_changes_when_titlecased(0x047D, 0x047D).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER OMEGA WITH TITLO
unicode_changes_when_titlecased(0x047F, 0x047F).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER OT
unicode_changes_when_titlecased(0x0481, 0x0481).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KOPPA
unicode_changes_when_titlecased(0x048B, 0x048B).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER SHORT I WITH TAIL
unicode_changes_when_titlecased(0x048D, 0x048D).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER SEMISOFT SIGN
unicode_changes_when_titlecased(0x048F, 0x048F).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ER WITH TICK
unicode_changes_when_titlecased(0x0491, 0x0491).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER GHE WITH UPTURN
unicode_changes_when_titlecased(0x0493, 0x0493).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER GHE WITH STROKE
unicode_changes_when_titlecased(0x0495, 0x0495).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER GHE WITH MIDDLE HOOK
unicode_changes_when_titlecased(0x0497, 0x0497).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ZHE WITH DESCENDER
unicode_changes_when_titlecased(0x0499, 0x0499).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ZE WITH DESCENDER
unicode_changes_when_titlecased(0x049B, 0x049B).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KA WITH DESCENDER
unicode_changes_when_titlecased(0x049D, 0x049D).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
unicode_changes_when_titlecased(0x049F, 0x049F).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KA WITH STROKE
unicode_changes_when_titlecased(0x04A1, 0x04A1).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER BASHKIR KA
unicode_changes_when_titlecased(0x04A3, 0x04A3).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER EN WITH DESCENDER
unicode_changes_when_titlecased(0x04A5, 0x04A5).	% Changes_When_Titlecased L&       CYRILLIC SMALL LIGATURE EN GHE
unicode_changes_when_titlecased(0x04A7, 0x04A7).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER PE WITH MIDDLE HOOK
unicode_changes_when_titlecased(0x04A9, 0x04A9).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ABKHASIAN HA
unicode_changes_when_titlecased(0x04AB, 0x04AB).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ES WITH DESCENDER
unicode_changes_when_titlecased(0x04AD, 0x04AD).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER TE WITH DESCENDER
unicode_changes_when_titlecased(0x04AF, 0x04AF).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER STRAIGHT U
unicode_changes_when_titlecased(0x04B1, 0x04B1).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
unicode_changes_when_titlecased(0x04B3, 0x04B3).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER HA WITH DESCENDER
unicode_changes_when_titlecased(0x04B5, 0x04B5).	% Changes_When_Titlecased L&       CYRILLIC SMALL LIGATURE TE TSE
unicode_changes_when_titlecased(0x04B7, 0x04B7).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER CHE WITH DESCENDER
unicode_changes_when_titlecased(0x04B9, 0x04B9).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
unicode_changes_when_titlecased(0x04BB, 0x04BB).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER SHHA
unicode_changes_when_titlecased(0x04BD, 0x04BD).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ABKHASIAN CHE
unicode_changes_when_titlecased(0x04BF, 0x04BF).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ABKHASIAN CHE WITH DESCENDER
unicode_changes_when_titlecased(0x04C2, 0x04C2).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ZHE WITH BREVE
unicode_changes_when_titlecased(0x04C4, 0x04C4).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KA WITH HOOK
unicode_changes_when_titlecased(0x04C6, 0x04C6).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER EL WITH TAIL
unicode_changes_when_titlecased(0x04C8, 0x04C8).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER EN WITH HOOK
unicode_changes_when_titlecased(0x04CA, 0x04CA).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER EN WITH TAIL
unicode_changes_when_titlecased(0x04CC, 0x04CC).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KHAKASSIAN CHE
unicode_changes_when_titlecased(0x04CE, 0x04CF).	% Changes_When_Titlecased L&   [2] CYRILLIC SMALL LETTER EM WITH TAIL..CYRILLIC SMALL LETTER PALOCHKA
unicode_changes_when_titlecased(0x04D1, 0x04D1).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER A WITH BREVE
unicode_changes_when_titlecased(0x04D3, 0x04D3).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER A WITH DIAERESIS
unicode_changes_when_titlecased(0x04D5, 0x04D5).	% Changes_When_Titlecased L&       CYRILLIC SMALL LIGATURE A IE
unicode_changes_when_titlecased(0x04D7, 0x04D7).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER IE WITH BREVE
unicode_changes_when_titlecased(0x04D9, 0x04D9).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER SCHWA
unicode_changes_when_titlecased(0x04DB, 0x04DB).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER SCHWA WITH DIAERESIS
unicode_changes_when_titlecased(0x04DD, 0x04DD).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ZHE WITH DIAERESIS
unicode_changes_when_titlecased(0x04DF, 0x04DF).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ZE WITH DIAERESIS
unicode_changes_when_titlecased(0x04E1, 0x04E1).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ABKHASIAN DZE
unicode_changes_when_titlecased(0x04E3, 0x04E3).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER I WITH MACRON
unicode_changes_when_titlecased(0x04E5, 0x04E5).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER I WITH DIAERESIS
unicode_changes_when_titlecased(0x04E7, 0x04E7).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER O WITH DIAERESIS
unicode_changes_when_titlecased(0x04E9, 0x04E9).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER BARRED O
unicode_changes_when_titlecased(0x04EB, 0x04EB).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER BARRED O WITH DIAERESIS
unicode_changes_when_titlecased(0x04ED, 0x04ED).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER E WITH DIAERESIS
unicode_changes_when_titlecased(0x04EF, 0x04EF).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER U WITH MACRON
unicode_changes_when_titlecased(0x04F1, 0x04F1).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER U WITH DIAERESIS
unicode_changes_when_titlecased(0x04F3, 0x04F3).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER U WITH DOUBLE ACUTE
unicode_changes_when_titlecased(0x04F5, 0x04F5).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER CHE WITH DIAERESIS
unicode_changes_when_titlecased(0x04F7, 0x04F7).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER GHE WITH DESCENDER
unicode_changes_when_titlecased(0x04F9, 0x04F9).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER YERU WITH DIAERESIS
unicode_changes_when_titlecased(0x04FB, 0x04FB).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER GHE WITH STROKE AND HOOK
unicode_changes_when_titlecased(0x04FD, 0x04FD).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER HA WITH HOOK
unicode_changes_when_titlecased(0x04FF, 0x04FF).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER HA WITH STROKE
unicode_changes_when_titlecased(0x0501, 0x0501).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KOMI DE
unicode_changes_when_titlecased(0x0503, 0x0503).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KOMI DJE
unicode_changes_when_titlecased(0x0505, 0x0505).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KOMI ZJE
unicode_changes_when_titlecased(0x0507, 0x0507).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KOMI DZJE
unicode_changes_when_titlecased(0x0509, 0x0509).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KOMI LJE
unicode_changes_when_titlecased(0x050B, 0x050B).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KOMI NJE
unicode_changes_when_titlecased(0x050D, 0x050D).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KOMI SJE
unicode_changes_when_titlecased(0x050F, 0x050F).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER KOMI TJE
unicode_changes_when_titlecased(0x0511, 0x0511).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER REVERSED ZE
unicode_changes_when_titlecased(0x0513, 0x0513).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER EL WITH HOOK
unicode_changes_when_titlecased(0x0515, 0x0515).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER LHA
unicode_changes_when_titlecased(0x0517, 0x0517).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER RHA
unicode_changes_when_titlecased(0x0519, 0x0519).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER YAE
unicode_changes_when_titlecased(0x051B, 0x051B).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER QA
unicode_changes_when_titlecased(0x051D, 0x051D).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER WE
unicode_changes_when_titlecased(0x051F, 0x051F).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ALEUT KA
unicode_changes_when_titlecased(0x0521, 0x0521).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER EL WITH MIDDLE HOOK
unicode_changes_when_titlecased(0x0523, 0x0523).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER EN WITH MIDDLE HOOK
unicode_changes_when_titlecased(0x0525, 0x0525).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER PE WITH DESCENDER
unicode_changes_when_titlecased(0x0527, 0x0527).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER SHHA WITH DESCENDER
unicode_changes_when_titlecased(0x0561, 0x0587).	% Changes_When_Titlecased L&  [39] ARMENIAN SMALL LETTER AYB..ARMENIAN SMALL LIGATURE ECH YIWN
unicode_changes_when_titlecased(0x1D79, 0x1D79).	% Changes_When_Titlecased L&       LATIN SMALL LETTER INSULAR G
unicode_changes_when_titlecased(0x1D7D, 0x1D7D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER P WITH STROKE
unicode_changes_when_titlecased(0x1E01, 0x1E01).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH RING BELOW
unicode_changes_when_titlecased(0x1E03, 0x1E03).	% Changes_When_Titlecased L&       LATIN SMALL LETTER B WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E05, 0x1E05).	% Changes_When_Titlecased L&       LATIN SMALL LETTER B WITH DOT BELOW
unicode_changes_when_titlecased(0x1E07, 0x1E07).	% Changes_When_Titlecased L&       LATIN SMALL LETTER B WITH LINE BELOW
unicode_changes_when_titlecased(0x1E09, 0x1E09).	% Changes_When_Titlecased L&       LATIN SMALL LETTER C WITH CEDILLA AND ACUTE
unicode_changes_when_titlecased(0x1E0B, 0x1E0B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER D WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E0D, 0x1E0D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER D WITH DOT BELOW
unicode_changes_when_titlecased(0x1E0F, 0x1E0F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER D WITH LINE BELOW
unicode_changes_when_titlecased(0x1E11, 0x1E11).	% Changes_When_Titlecased L&       LATIN SMALL LETTER D WITH CEDILLA
unicode_changes_when_titlecased(0x1E13, 0x1E13).	% Changes_When_Titlecased L&       LATIN SMALL LETTER D WITH CIRCUMFLEX BELOW
unicode_changes_when_titlecased(0x1E15, 0x1E15).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH MACRON AND GRAVE
unicode_changes_when_titlecased(0x1E17, 0x1E17).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH MACRON AND ACUTE
unicode_changes_when_titlecased(0x1E19, 0x1E19).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH CIRCUMFLEX BELOW
unicode_changes_when_titlecased(0x1E1B, 0x1E1B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH TILDE BELOW
unicode_changes_when_titlecased(0x1E1D, 0x1E1D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH CEDILLA AND BREVE
unicode_changes_when_titlecased(0x1E1F, 0x1E1F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER F WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E21, 0x1E21).	% Changes_When_Titlecased L&       LATIN SMALL LETTER G WITH MACRON
unicode_changes_when_titlecased(0x1E23, 0x1E23).	% Changes_When_Titlecased L&       LATIN SMALL LETTER H WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E25, 0x1E25).	% Changes_When_Titlecased L&       LATIN SMALL LETTER H WITH DOT BELOW
unicode_changes_when_titlecased(0x1E27, 0x1E27).	% Changes_When_Titlecased L&       LATIN SMALL LETTER H WITH DIAERESIS
unicode_changes_when_titlecased(0x1E29, 0x1E29).	% Changes_When_Titlecased L&       LATIN SMALL LETTER H WITH CEDILLA
unicode_changes_when_titlecased(0x1E2B, 0x1E2B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER H WITH BREVE BELOW
unicode_changes_when_titlecased(0x1E2D, 0x1E2D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER I WITH TILDE BELOW
unicode_changes_when_titlecased(0x1E2F, 0x1E2F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER I WITH DIAERESIS AND ACUTE
unicode_changes_when_titlecased(0x1E31, 0x1E31).	% Changes_When_Titlecased L&       LATIN SMALL LETTER K WITH ACUTE
unicode_changes_when_titlecased(0x1E33, 0x1E33).	% Changes_When_Titlecased L&       LATIN SMALL LETTER K WITH DOT BELOW
unicode_changes_when_titlecased(0x1E35, 0x1E35).	% Changes_When_Titlecased L&       LATIN SMALL LETTER K WITH LINE BELOW
unicode_changes_when_titlecased(0x1E37, 0x1E37).	% Changes_When_Titlecased L&       LATIN SMALL LETTER L WITH DOT BELOW
unicode_changes_when_titlecased(0x1E39, 0x1E39).	% Changes_When_Titlecased L&       LATIN SMALL LETTER L WITH DOT BELOW AND MACRON
unicode_changes_when_titlecased(0x1E3B, 0x1E3B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER L WITH LINE BELOW
unicode_changes_when_titlecased(0x1E3D, 0x1E3D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER L WITH CIRCUMFLEX BELOW
unicode_changes_when_titlecased(0x1E3F, 0x1E3F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER M WITH ACUTE
unicode_changes_when_titlecased(0x1E41, 0x1E41).	% Changes_When_Titlecased L&       LATIN SMALL LETTER M WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E43, 0x1E43).	% Changes_When_Titlecased L&       LATIN SMALL LETTER M WITH DOT BELOW
unicode_changes_when_titlecased(0x1E45, 0x1E45).	% Changes_When_Titlecased L&       LATIN SMALL LETTER N WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E47, 0x1E47).	% Changes_When_Titlecased L&       LATIN SMALL LETTER N WITH DOT BELOW
unicode_changes_when_titlecased(0x1E49, 0x1E49).	% Changes_When_Titlecased L&       LATIN SMALL LETTER N WITH LINE BELOW
unicode_changes_when_titlecased(0x1E4B, 0x1E4B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER N WITH CIRCUMFLEX BELOW
unicode_changes_when_titlecased(0x1E4D, 0x1E4D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH TILDE AND ACUTE
unicode_changes_when_titlecased(0x1E4F, 0x1E4F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH TILDE AND DIAERESIS
unicode_changes_when_titlecased(0x1E51, 0x1E51).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH MACRON AND GRAVE
unicode_changes_when_titlecased(0x1E53, 0x1E53).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH MACRON AND ACUTE
unicode_changes_when_titlecased(0x1E55, 0x1E55).	% Changes_When_Titlecased L&       LATIN SMALL LETTER P WITH ACUTE
unicode_changes_when_titlecased(0x1E57, 0x1E57).	% Changes_When_Titlecased L&       LATIN SMALL LETTER P WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E59, 0x1E59).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E5B, 0x1E5B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R WITH DOT BELOW
unicode_changes_when_titlecased(0x1E5D, 0x1E5D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R WITH DOT BELOW AND MACRON
unicode_changes_when_titlecased(0x1E5F, 0x1E5F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R WITH LINE BELOW
unicode_changes_when_titlecased(0x1E61, 0x1E61).	% Changes_When_Titlecased L&       LATIN SMALL LETTER S WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E63, 0x1E63).	% Changes_When_Titlecased L&       LATIN SMALL LETTER S WITH DOT BELOW
unicode_changes_when_titlecased(0x1E65, 0x1E65).	% Changes_When_Titlecased L&       LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE
unicode_changes_when_titlecased(0x1E67, 0x1E67).	% Changes_When_Titlecased L&       LATIN SMALL LETTER S WITH CARON AND DOT ABOVE
unicode_changes_when_titlecased(0x1E69, 0x1E69).	% Changes_When_Titlecased L&       LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
unicode_changes_when_titlecased(0x1E6B, 0x1E6B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER T WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E6D, 0x1E6D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER T WITH DOT BELOW
unicode_changes_when_titlecased(0x1E6F, 0x1E6F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER T WITH LINE BELOW
unicode_changes_when_titlecased(0x1E71, 0x1E71).	% Changes_When_Titlecased L&       LATIN SMALL LETTER T WITH CIRCUMFLEX BELOW
unicode_changes_when_titlecased(0x1E73, 0x1E73).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH DIAERESIS BELOW
unicode_changes_when_titlecased(0x1E75, 0x1E75).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH TILDE BELOW
unicode_changes_when_titlecased(0x1E77, 0x1E77).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH CIRCUMFLEX BELOW
unicode_changes_when_titlecased(0x1E79, 0x1E79).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH TILDE AND ACUTE
unicode_changes_when_titlecased(0x1E7B, 0x1E7B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH MACRON AND DIAERESIS
unicode_changes_when_titlecased(0x1E7D, 0x1E7D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER V WITH TILDE
unicode_changes_when_titlecased(0x1E7F, 0x1E7F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER V WITH DOT BELOW
unicode_changes_when_titlecased(0x1E81, 0x1E81).	% Changes_When_Titlecased L&       LATIN SMALL LETTER W WITH GRAVE
unicode_changes_when_titlecased(0x1E83, 0x1E83).	% Changes_When_Titlecased L&       LATIN SMALL LETTER W WITH ACUTE
unicode_changes_when_titlecased(0x1E85, 0x1E85).	% Changes_When_Titlecased L&       LATIN SMALL LETTER W WITH DIAERESIS
unicode_changes_when_titlecased(0x1E87, 0x1E87).	% Changes_When_Titlecased L&       LATIN SMALL LETTER W WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E89, 0x1E89).	% Changes_When_Titlecased L&       LATIN SMALL LETTER W WITH DOT BELOW
unicode_changes_when_titlecased(0x1E8B, 0x1E8B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER X WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E8D, 0x1E8D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER X WITH DIAERESIS
unicode_changes_when_titlecased(0x1E8F, 0x1E8F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Y WITH DOT ABOVE
unicode_changes_when_titlecased(0x1E91, 0x1E91).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Z WITH CIRCUMFLEX
unicode_changes_when_titlecased(0x1E93, 0x1E93).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Z WITH DOT BELOW
unicode_changes_when_titlecased(0x1E95, 0x1E9B).	% Changes_When_Titlecased L&   [7] LATIN SMALL LETTER Z WITH LINE BELOW..LATIN SMALL LETTER LONG S WITH DOT ABOVE
unicode_changes_when_titlecased(0x1EA1, 0x1EA1).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH DOT BELOW
unicode_changes_when_titlecased(0x1EA3, 0x1EA3).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH HOOK ABOVE
unicode_changes_when_titlecased(0x1EA5, 0x1EA5).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
unicode_changes_when_titlecased(0x1EA7, 0x1EA7).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
unicode_changes_when_titlecased(0x1EA9, 0x1EA9).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
unicode_changes_when_titlecased(0x1EAB, 0x1EAB).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
unicode_changes_when_titlecased(0x1EAD, 0x1EAD).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
unicode_changes_when_titlecased(0x1EAF, 0x1EAF).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH BREVE AND ACUTE
unicode_changes_when_titlecased(0x1EB1, 0x1EB1).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH BREVE AND GRAVE
unicode_changes_when_titlecased(0x1EB3, 0x1EB3).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
unicode_changes_when_titlecased(0x1EB5, 0x1EB5).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH BREVE AND TILDE
unicode_changes_when_titlecased(0x1EB7, 0x1EB7).	% Changes_When_Titlecased L&       LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
unicode_changes_when_titlecased(0x1EB9, 0x1EB9).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH DOT BELOW
unicode_changes_when_titlecased(0x1EBB, 0x1EBB).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH HOOK ABOVE
unicode_changes_when_titlecased(0x1EBD, 0x1EBD).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH TILDE
unicode_changes_when_titlecased(0x1EBF, 0x1EBF).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
unicode_changes_when_titlecased(0x1EC1, 0x1EC1).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
unicode_changes_when_titlecased(0x1EC3, 0x1EC3).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
unicode_changes_when_titlecased(0x1EC5, 0x1EC5).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
unicode_changes_when_titlecased(0x1EC7, 0x1EC7).	% Changes_When_Titlecased L&       LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
unicode_changes_when_titlecased(0x1EC9, 0x1EC9).	% Changes_When_Titlecased L&       LATIN SMALL LETTER I WITH HOOK ABOVE
unicode_changes_when_titlecased(0x1ECB, 0x1ECB).	% Changes_When_Titlecased L&       LATIN SMALL LETTER I WITH DOT BELOW
unicode_changes_when_titlecased(0x1ECD, 0x1ECD).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH DOT BELOW
unicode_changes_when_titlecased(0x1ECF, 0x1ECF).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH HOOK ABOVE
unicode_changes_when_titlecased(0x1ED1, 0x1ED1).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
unicode_changes_when_titlecased(0x1ED3, 0x1ED3).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
unicode_changes_when_titlecased(0x1ED5, 0x1ED5).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
unicode_changes_when_titlecased(0x1ED7, 0x1ED7).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
unicode_changes_when_titlecased(0x1ED9, 0x1ED9).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
unicode_changes_when_titlecased(0x1EDB, 0x1EDB).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH HORN AND ACUTE
unicode_changes_when_titlecased(0x1EDD, 0x1EDD).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH HORN AND GRAVE
unicode_changes_when_titlecased(0x1EDF, 0x1EDF).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
unicode_changes_when_titlecased(0x1EE1, 0x1EE1).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH HORN AND TILDE
unicode_changes_when_titlecased(0x1EE3, 0x1EE3).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH HORN AND DOT BELOW
unicode_changes_when_titlecased(0x1EE5, 0x1EE5).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH DOT BELOW
unicode_changes_when_titlecased(0x1EE7, 0x1EE7).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH HOOK ABOVE
unicode_changes_when_titlecased(0x1EE9, 0x1EE9).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH HORN AND ACUTE
unicode_changes_when_titlecased(0x1EEB, 0x1EEB).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH HORN AND GRAVE
unicode_changes_when_titlecased(0x1EED, 0x1EED).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
unicode_changes_when_titlecased(0x1EEF, 0x1EEF).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH HORN AND TILDE
unicode_changes_when_titlecased(0x1EF1, 0x1EF1).	% Changes_When_Titlecased L&       LATIN SMALL LETTER U WITH HORN AND DOT BELOW
unicode_changes_when_titlecased(0x1EF3, 0x1EF3).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Y WITH GRAVE
unicode_changes_when_titlecased(0x1EF5, 0x1EF5).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Y WITH DOT BELOW
unicode_changes_when_titlecased(0x1EF7, 0x1EF7).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Y WITH HOOK ABOVE
unicode_changes_when_titlecased(0x1EF9, 0x1EF9).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Y WITH TILDE
unicode_changes_when_titlecased(0x1EFB, 0x1EFB).	% Changes_When_Titlecased L&       LATIN SMALL LETTER MIDDLE-WELSH LL
unicode_changes_when_titlecased(0x1EFD, 0x1EFD).	% Changes_When_Titlecased L&       LATIN SMALL LETTER MIDDLE-WELSH V
unicode_changes_when_titlecased(0x1EFF, 0x1F07).	% Changes_When_Titlecased L&   [9] LATIN SMALL LETTER Y WITH LOOP..GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
unicode_changes_when_titlecased(0x1F10, 0x1F15).	% Changes_When_Titlecased L&   [6] GREEK SMALL LETTER EPSILON WITH PSILI..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_changes_when_titlecased(0x1F20, 0x1F27).	% Changes_When_Titlecased L&   [8] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI
unicode_changes_when_titlecased(0x1F30, 0x1F37).	% Changes_When_Titlecased L&   [8] GREEK SMALL LETTER IOTA WITH PSILI..GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI
unicode_changes_when_titlecased(0x1F40, 0x1F45).	% Changes_When_Titlecased L&   [6] GREEK SMALL LETTER OMICRON WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_changes_when_titlecased(0x1F50, 0x1F57).	% Changes_When_Titlecased L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_changes_when_titlecased(0x1F60, 0x1F67).	% Changes_When_Titlecased L&   [8] GREEK SMALL LETTER OMEGA WITH PSILI..GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI
unicode_changes_when_titlecased(0x1F70, 0x1F7D).	% Changes_When_Titlecased L&  [14] GREEK SMALL LETTER ALPHA WITH VARIA..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_changes_when_titlecased(0x1F80, 0x1F87).	% Changes_When_Titlecased L&   [8] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_changes_when_titlecased(0x1F90, 0x1F97).	% Changes_When_Titlecased L&   [8] GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_changes_when_titlecased(0x1FA0, 0x1FA7).	% Changes_When_Titlecased L&   [8] GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_changes_when_titlecased(0x1FB0, 0x1FB4).	% Changes_When_Titlecased L&   [5] GREEK SMALL LETTER ALPHA WITH VRACHY..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_changes_when_titlecased(0x1FB6, 0x1FB7).	% Changes_When_Titlecased L&   [2] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_changes_when_titlecased(0x1FBE, 0x1FBE).	% Changes_When_Titlecased L&       GREEK PROSGEGRAMMENI
unicode_changes_when_titlecased(0x1FC2, 0x1FC4).	% Changes_When_Titlecased L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_changes_when_titlecased(0x1FC6, 0x1FC7).	% Changes_When_Titlecased L&   [2] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_changes_when_titlecased(0x1FD0, 0x1FD3).	% Changes_When_Titlecased L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_changes_when_titlecased(0x1FD6, 0x1FD7).	% Changes_When_Titlecased L&   [2] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
unicode_changes_when_titlecased(0x1FE0, 0x1FE7).	% Changes_When_Titlecased L&   [8] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
unicode_changes_when_titlecased(0x1FF2, 0x1FF4).	% Changes_When_Titlecased L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_changes_when_titlecased(0x1FF6, 0x1FF7).	% Changes_When_Titlecased L&   [2] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_changes_when_titlecased(0x214E, 0x214E).	% Changes_When_Titlecased L&       TURNED SMALL F
unicode_changes_when_titlecased(0x2170, 0x217F).	% Changes_When_Titlecased Nl  [16] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL ONE THOUSAND
unicode_changes_when_titlecased(0x2184, 0x2184).	% Changes_When_Titlecased L&       LATIN SMALL LETTER REVERSED C
unicode_changes_when_titlecased(0x24D0, 0x24E9).	% Changes_When_Titlecased So  [26] CIRCLED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_changes_when_titlecased(0x2C30, 0x2C5E).	% Changes_When_Titlecased L&  [47] GLAGOLITIC SMALL LETTER AZU..GLAGOLITIC SMALL LETTER LATINATE MYSLITE
unicode_changes_when_titlecased(0x2C61, 0x2C61).	% Changes_When_Titlecased L&       LATIN SMALL LETTER L WITH DOUBLE BAR
unicode_changes_when_titlecased(0x2C65, 0x2C66).	% Changes_When_Titlecased L&   [2] LATIN SMALL LETTER A WITH STROKE..LATIN SMALL LETTER T WITH DIAGONAL STROKE
unicode_changes_when_titlecased(0x2C68, 0x2C68).	% Changes_When_Titlecased L&       LATIN SMALL LETTER H WITH DESCENDER
unicode_changes_when_titlecased(0x2C6A, 0x2C6A).	% Changes_When_Titlecased L&       LATIN SMALL LETTER K WITH DESCENDER
unicode_changes_when_titlecased(0x2C6C, 0x2C6C).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Z WITH DESCENDER
unicode_changes_when_titlecased(0x2C73, 0x2C73).	% Changes_When_Titlecased L&       LATIN SMALL LETTER W WITH HOOK
unicode_changes_when_titlecased(0x2C76, 0x2C76).	% Changes_When_Titlecased L&       LATIN SMALL LETTER HALF H
unicode_changes_when_titlecased(0x2C81, 0x2C81).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER ALFA
unicode_changes_when_titlecased(0x2C83, 0x2C83).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER VIDA
unicode_changes_when_titlecased(0x2C85, 0x2C85).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER GAMMA
unicode_changes_when_titlecased(0x2C87, 0x2C87).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER DALDA
unicode_changes_when_titlecased(0x2C89, 0x2C89).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER EIE
unicode_changes_when_titlecased(0x2C8B, 0x2C8B).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER SOU
unicode_changes_when_titlecased(0x2C8D, 0x2C8D).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER ZATA
unicode_changes_when_titlecased(0x2C8F, 0x2C8F).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER HATE
unicode_changes_when_titlecased(0x2C91, 0x2C91).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER THETHE
unicode_changes_when_titlecased(0x2C93, 0x2C93).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER IAUDA
unicode_changes_when_titlecased(0x2C95, 0x2C95).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER KAPA
unicode_changes_when_titlecased(0x2C97, 0x2C97).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER LAULA
unicode_changes_when_titlecased(0x2C99, 0x2C99).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER MI
unicode_changes_when_titlecased(0x2C9B, 0x2C9B).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER NI
unicode_changes_when_titlecased(0x2C9D, 0x2C9D).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER KSI
unicode_changes_when_titlecased(0x2C9F, 0x2C9F).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER O
unicode_changes_when_titlecased(0x2CA1, 0x2CA1).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER PI
unicode_changes_when_titlecased(0x2CA3, 0x2CA3).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER RO
unicode_changes_when_titlecased(0x2CA5, 0x2CA5).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER SIMA
unicode_changes_when_titlecased(0x2CA7, 0x2CA7).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER TAU
unicode_changes_when_titlecased(0x2CA9, 0x2CA9).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER UA
unicode_changes_when_titlecased(0x2CAB, 0x2CAB).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER FI
unicode_changes_when_titlecased(0x2CAD, 0x2CAD).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER KHI
unicode_changes_when_titlecased(0x2CAF, 0x2CAF).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER PSI
unicode_changes_when_titlecased(0x2CB1, 0x2CB1).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OOU
unicode_changes_when_titlecased(0x2CB3, 0x2CB3).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER DIALECT-P ALEF
unicode_changes_when_titlecased(0x2CB5, 0x2CB5).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD COPTIC AIN
unicode_changes_when_titlecased(0x2CB7, 0x2CB7).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER CRYPTOGRAMMIC EIE
unicode_changes_when_titlecased(0x2CB9, 0x2CB9).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER DIALECT-P KAPA
unicode_changes_when_titlecased(0x2CBB, 0x2CBB).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER DIALECT-P NI
unicode_changes_when_titlecased(0x2CBD, 0x2CBD).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER CRYPTOGRAMMIC NI
unicode_changes_when_titlecased(0x2CBF, 0x2CBF).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD COPTIC OOU
unicode_changes_when_titlecased(0x2CC1, 0x2CC1).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER SAMPI
unicode_changes_when_titlecased(0x2CC3, 0x2CC3).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER CROSSED SHEI
unicode_changes_when_titlecased(0x2CC5, 0x2CC5).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD COPTIC SHEI
unicode_changes_when_titlecased(0x2CC7, 0x2CC7).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD COPTIC ESH
unicode_changes_when_titlecased(0x2CC9, 0x2CC9).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER AKHMIMIC KHEI
unicode_changes_when_titlecased(0x2CCB, 0x2CCB).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER DIALECT-P HORI
unicode_changes_when_titlecased(0x2CCD, 0x2CCD).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD COPTIC HORI
unicode_changes_when_titlecased(0x2CCF, 0x2CCF).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD COPTIC HA
unicode_changes_when_titlecased(0x2CD1, 0x2CD1).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER L-SHAPED HA
unicode_changes_when_titlecased(0x2CD3, 0x2CD3).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD COPTIC HEI
unicode_changes_when_titlecased(0x2CD5, 0x2CD5).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD COPTIC HAT
unicode_changes_when_titlecased(0x2CD7, 0x2CD7).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD COPTIC GANGIA
unicode_changes_when_titlecased(0x2CD9, 0x2CD9).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD COPTIC DJA
unicode_changes_when_titlecased(0x2CDB, 0x2CDB).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD COPTIC SHIMA
unicode_changes_when_titlecased(0x2CDD, 0x2CDD).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD NUBIAN SHIMA
unicode_changes_when_titlecased(0x2CDF, 0x2CDF).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD NUBIAN NGI
unicode_changes_when_titlecased(0x2CE1, 0x2CE1).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD NUBIAN NYI
unicode_changes_when_titlecased(0x2CE3, 0x2CE3).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER OLD NUBIAN WAU
unicode_changes_when_titlecased(0x2CEC, 0x2CEC).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER CRYPTOGRAMMIC SHEI
unicode_changes_when_titlecased(0x2CEE, 0x2CEE).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
unicode_changes_when_titlecased(0x2CF3, 0x2CF3).	% Changes_When_Titlecased L&       COPTIC SMALL LETTER BOHAIRIC KHEI
unicode_changes_when_titlecased(0x2D00, 0x2D25).	% Changes_When_Titlecased L&  [38] GEORGIAN SMALL LETTER AN..GEORGIAN SMALL LETTER HOE
unicode_changes_when_titlecased(0x2D27, 0x2D27).	% Changes_When_Titlecased L&       GEORGIAN SMALL LETTER YN
unicode_changes_when_titlecased(0x2D2D, 0x2D2D).	% Changes_When_Titlecased L&       GEORGIAN SMALL LETTER AEN
unicode_changes_when_titlecased(0xA641, 0xA641).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ZEMLYA
unicode_changes_when_titlecased(0xA643, 0xA643).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER DZELO
unicode_changes_when_titlecased(0xA645, 0xA645).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER REVERSED DZE
unicode_changes_when_titlecased(0xA647, 0xA647).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER IOTA
unicode_changes_when_titlecased(0xA649, 0xA649).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER DJERV
unicode_changes_when_titlecased(0xA64B, 0xA64B).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER MONOGRAPH UK
unicode_changes_when_titlecased(0xA64D, 0xA64D).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER BROAD OMEGA
unicode_changes_when_titlecased(0xA64F, 0xA64F).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER NEUTRAL YER
unicode_changes_when_titlecased(0xA651, 0xA651).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER YERU WITH BACK YER
unicode_changes_when_titlecased(0xA653, 0xA653).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER IOTIFIED YAT
unicode_changes_when_titlecased(0xA655, 0xA655).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER REVERSED YU
unicode_changes_when_titlecased(0xA657, 0xA657).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER IOTIFIED A
unicode_changes_when_titlecased(0xA659, 0xA659).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER CLOSED LITTLE YUS
unicode_changes_when_titlecased(0xA65B, 0xA65B).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER BLENDED YUS
unicode_changes_when_titlecased(0xA65D, 0xA65D).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER IOTIFIED CLOSED LITTLE YUS
unicode_changes_when_titlecased(0xA65F, 0xA65F).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER YN
unicode_changes_when_titlecased(0xA661, 0xA661).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER REVERSED TSE
unicode_changes_when_titlecased(0xA663, 0xA663).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER SOFT DE
unicode_changes_when_titlecased(0xA665, 0xA665).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER SOFT EL
unicode_changes_when_titlecased(0xA667, 0xA667).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER SOFT EM
unicode_changes_when_titlecased(0xA669, 0xA669).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER MONOCULAR O
unicode_changes_when_titlecased(0xA66B, 0xA66B).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER BINOCULAR O
unicode_changes_when_titlecased(0xA66D, 0xA66D).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
unicode_changes_when_titlecased(0xA681, 0xA681).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER DWE
unicode_changes_when_titlecased(0xA683, 0xA683).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER DZWE
unicode_changes_when_titlecased(0xA685, 0xA685).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER ZHWE
unicode_changes_when_titlecased(0xA687, 0xA687).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER CCHE
unicode_changes_when_titlecased(0xA689, 0xA689).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER DZZE
unicode_changes_when_titlecased(0xA68B, 0xA68B).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER TE WITH MIDDLE HOOK
unicode_changes_when_titlecased(0xA68D, 0xA68D).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER TWE
unicode_changes_when_titlecased(0xA68F, 0xA68F).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER TSWE
unicode_changes_when_titlecased(0xA691, 0xA691).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER TSSE
unicode_changes_when_titlecased(0xA693, 0xA693).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER TCHE
unicode_changes_when_titlecased(0xA695, 0xA695).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER HWE
unicode_changes_when_titlecased(0xA697, 0xA697).	% Changes_When_Titlecased L&       CYRILLIC SMALL LETTER SHWE
unicode_changes_when_titlecased(0xA723, 0xA723).	% Changes_When_Titlecased L&       LATIN SMALL LETTER EGYPTOLOGICAL ALEF
unicode_changes_when_titlecased(0xA725, 0xA725).	% Changes_When_Titlecased L&       LATIN SMALL LETTER EGYPTOLOGICAL AIN
unicode_changes_when_titlecased(0xA727, 0xA727).	% Changes_When_Titlecased L&       LATIN SMALL LETTER HENG
unicode_changes_when_titlecased(0xA729, 0xA729).	% Changes_When_Titlecased L&       LATIN SMALL LETTER TZ
unicode_changes_when_titlecased(0xA72B, 0xA72B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER TRESILLO
unicode_changes_when_titlecased(0xA72D, 0xA72D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER CUATRILLO
unicode_changes_when_titlecased(0xA72F, 0xA72F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER CUATRILLO WITH COMMA
unicode_changes_when_titlecased(0xA733, 0xA733).	% Changes_When_Titlecased L&       LATIN SMALL LETTER AA
unicode_changes_when_titlecased(0xA735, 0xA735).	% Changes_When_Titlecased L&       LATIN SMALL LETTER AO
unicode_changes_when_titlecased(0xA737, 0xA737).	% Changes_When_Titlecased L&       LATIN SMALL LETTER AU
unicode_changes_when_titlecased(0xA739, 0xA739).	% Changes_When_Titlecased L&       LATIN SMALL LETTER AV
unicode_changes_when_titlecased(0xA73B, 0xA73B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER AV WITH HORIZONTAL BAR
unicode_changes_when_titlecased(0xA73D, 0xA73D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER AY
unicode_changes_when_titlecased(0xA73F, 0xA73F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER REVERSED C WITH DOT
unicode_changes_when_titlecased(0xA741, 0xA741).	% Changes_When_Titlecased L&       LATIN SMALL LETTER K WITH STROKE
unicode_changes_when_titlecased(0xA743, 0xA743).	% Changes_When_Titlecased L&       LATIN SMALL LETTER K WITH DIAGONAL STROKE
unicode_changes_when_titlecased(0xA745, 0xA745).	% Changes_When_Titlecased L&       LATIN SMALL LETTER K WITH STROKE AND DIAGONAL STROKE
unicode_changes_when_titlecased(0xA747, 0xA747).	% Changes_When_Titlecased L&       LATIN SMALL LETTER BROKEN L
unicode_changes_when_titlecased(0xA749, 0xA749).	% Changes_When_Titlecased L&       LATIN SMALL LETTER L WITH HIGH STROKE
unicode_changes_when_titlecased(0xA74B, 0xA74B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH LONG STROKE OVERLAY
unicode_changes_when_titlecased(0xA74D, 0xA74D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER O WITH LOOP
unicode_changes_when_titlecased(0xA74F, 0xA74F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER OO
unicode_changes_when_titlecased(0xA751, 0xA751).	% Changes_When_Titlecased L&       LATIN SMALL LETTER P WITH STROKE THROUGH DESCENDER
unicode_changes_when_titlecased(0xA753, 0xA753).	% Changes_When_Titlecased L&       LATIN SMALL LETTER P WITH FLOURISH
unicode_changes_when_titlecased(0xA755, 0xA755).	% Changes_When_Titlecased L&       LATIN SMALL LETTER P WITH SQUIRREL TAIL
unicode_changes_when_titlecased(0xA757, 0xA757).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Q WITH STROKE THROUGH DESCENDER
unicode_changes_when_titlecased(0xA759, 0xA759).	% Changes_When_Titlecased L&       LATIN SMALL LETTER Q WITH DIAGONAL STROKE
unicode_changes_when_titlecased(0xA75B, 0xA75B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R ROTUNDA
unicode_changes_when_titlecased(0xA75D, 0xA75D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER RUM ROTUNDA
unicode_changes_when_titlecased(0xA75F, 0xA75F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER V WITH DIAGONAL STROKE
unicode_changes_when_titlecased(0xA761, 0xA761).	% Changes_When_Titlecased L&       LATIN SMALL LETTER VY
unicode_changes_when_titlecased(0xA763, 0xA763).	% Changes_When_Titlecased L&       LATIN SMALL LETTER VISIGOTHIC Z
unicode_changes_when_titlecased(0xA765, 0xA765).	% Changes_When_Titlecased L&       LATIN SMALL LETTER THORN WITH STROKE
unicode_changes_when_titlecased(0xA767, 0xA767).	% Changes_When_Titlecased L&       LATIN SMALL LETTER THORN WITH STROKE THROUGH DESCENDER
unicode_changes_when_titlecased(0xA769, 0xA769).	% Changes_When_Titlecased L&       LATIN SMALL LETTER VEND
unicode_changes_when_titlecased(0xA76B, 0xA76B).	% Changes_When_Titlecased L&       LATIN SMALL LETTER ET
unicode_changes_when_titlecased(0xA76D, 0xA76D).	% Changes_When_Titlecased L&       LATIN SMALL LETTER IS
unicode_changes_when_titlecased(0xA76F, 0xA76F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER CON
unicode_changes_when_titlecased(0xA77A, 0xA77A).	% Changes_When_Titlecased L&       LATIN SMALL LETTER INSULAR D
unicode_changes_when_titlecased(0xA77C, 0xA77C).	% Changes_When_Titlecased L&       LATIN SMALL LETTER INSULAR F
unicode_changes_when_titlecased(0xA77F, 0xA77F).	% Changes_When_Titlecased L&       LATIN SMALL LETTER TURNED INSULAR G
unicode_changes_when_titlecased(0xA781, 0xA781).	% Changes_When_Titlecased L&       LATIN SMALL LETTER TURNED L
unicode_changes_when_titlecased(0xA783, 0xA783).	% Changes_When_Titlecased L&       LATIN SMALL LETTER INSULAR R
unicode_changes_when_titlecased(0xA785, 0xA785).	% Changes_When_Titlecased L&       LATIN SMALL LETTER INSULAR S
unicode_changes_when_titlecased(0xA787, 0xA787).	% Changes_When_Titlecased L&       LATIN SMALL LETTER INSULAR T
unicode_changes_when_titlecased(0xA78C, 0xA78C).	% Changes_When_Titlecased L&       LATIN SMALL LETTER SALTILLO
unicode_changes_when_titlecased(0xA791, 0xA791).	% Changes_When_Titlecased L&       LATIN SMALL LETTER N WITH DESCENDER
unicode_changes_when_titlecased(0xA793, 0xA793).	% Changes_When_Titlecased L&       LATIN SMALL LETTER C WITH BAR
unicode_changes_when_titlecased(0xA7A1, 0xA7A1).	% Changes_When_Titlecased L&       LATIN SMALL LETTER G WITH OBLIQUE STROKE
unicode_changes_when_titlecased(0xA7A3, 0xA7A3).	% Changes_When_Titlecased L&       LATIN SMALL LETTER K WITH OBLIQUE STROKE
unicode_changes_when_titlecased(0xA7A5, 0xA7A5).	% Changes_When_Titlecased L&       LATIN SMALL LETTER N WITH OBLIQUE STROKE
unicode_changes_when_titlecased(0xA7A7, 0xA7A7).	% Changes_When_Titlecased L&       LATIN SMALL LETTER R WITH OBLIQUE STROKE
unicode_changes_when_titlecased(0xA7A9, 0xA7A9).	% Changes_When_Titlecased L&       LATIN SMALL LETTER S WITH OBLIQUE STROKE
unicode_changes_when_titlecased(0xFB00, 0xFB06).	% Changes_When_Titlecased L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_changes_when_titlecased(0xFB13, 0xFB17).	% Changes_When_Titlecased L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_changes_when_titlecased(0xFF41, 0xFF5A).	% Changes_When_Titlecased L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_changes_when_titlecased(0x10428, 0x1044F).	% Changes_When_Titlecased L&  [40] DESERET SMALL LETTER LONG I..DESERET SMALL LETTER EW

% Total code points: 1099
