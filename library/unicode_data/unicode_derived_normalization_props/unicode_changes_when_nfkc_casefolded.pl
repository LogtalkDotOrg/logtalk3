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

unicode_changes_when_nfkc_casefolded(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_changes_when_nfkc_casefolded(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_changes_when_nfkc_casefolded(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_changes_when_nfkc_casefolded(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property:   Changes_When_NFKC_Casefolded (CWKCF)
%  Characters that are not stable under an NFKC_Casefold mapping.
%  As defined by cp != NFKC_Casefold(cp)

unicode_changes_when_nfkc_casefolded(0x0041, 0x005A).	% L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
unicode_changes_when_nfkc_casefolded(0x00A0, 0x00A0).	% Zs       NO-BREAK SPACE
unicode_changes_when_nfkc_casefolded(0x00A8, 0x00A8).	% Sk       DIAERESIS
unicode_changes_when_nfkc_casefolded(0x00AA, 0x00AA).	% Lo       FEMININE ORDINAL INDICATOR
unicode_changes_when_nfkc_casefolded(0x00AD, 0x00AD).	% Cf       SOFT HYPHEN
unicode_changes_when_nfkc_casefolded(0x00AF, 0x00AF).	% Sk       MACRON
unicode_changes_when_nfkc_casefolded(0x00B2, 0x00B3).	% No   [2] SUPERSCRIPT TWO..SUPERSCRIPT THREE
unicode_changes_when_nfkc_casefolded(0x00B4, 0x00B4).	% Sk       ACUTE ACCENT
unicode_changes_when_nfkc_casefolded(0x00B5, 0x00B5).	% L&       MICRO SIGN
unicode_changes_when_nfkc_casefolded(0x00B8, 0x00B8).	% Sk       CEDILLA
unicode_changes_when_nfkc_casefolded(0x00B9, 0x00B9).	% No       SUPERSCRIPT ONE
unicode_changes_when_nfkc_casefolded(0x00BA, 0x00BA).	% Lo       MASCULINE ORDINAL INDICATOR
unicode_changes_when_nfkc_casefolded(0x00BC, 0x00BE).	% No   [3] VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS
unicode_changes_when_nfkc_casefolded(0x00C0, 0x00D6).	% L&  [23] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x00D8, 0x00DF).	% L&   [8] LATIN CAPITAL LETTER O WITH STROKE..LATIN SMALL LETTER SHARP S
unicode_changes_when_nfkc_casefolded(0x0100, 0x0100).	% L&       LATIN CAPITAL LETTER A WITH MACRON
unicode_changes_when_nfkc_casefolded(0x0102, 0x0102).	% L&       LATIN CAPITAL LETTER A WITH BREVE
unicode_changes_when_nfkc_casefolded(0x0104, 0x0104).	% L&       LATIN CAPITAL LETTER A WITH OGONEK
unicode_changes_when_nfkc_casefolded(0x0106, 0x0106).	% L&       LATIN CAPITAL LETTER C WITH ACUTE
unicode_changes_when_nfkc_casefolded(0x0108, 0x0108).	% L&       LATIN CAPITAL LETTER C WITH CIRCUMFLEX
unicode_changes_when_nfkc_casefolded(0x010A, 0x010A).	% L&       LATIN CAPITAL LETTER C WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x010C, 0x010C).	% L&       LATIN CAPITAL LETTER C WITH CARON
unicode_changes_when_nfkc_casefolded(0x010E, 0x010E).	% L&       LATIN CAPITAL LETTER D WITH CARON
unicode_changes_when_nfkc_casefolded(0x0110, 0x0110).	% L&       LATIN CAPITAL LETTER D WITH STROKE
unicode_changes_when_nfkc_casefolded(0x0112, 0x0112).	% L&       LATIN CAPITAL LETTER E WITH MACRON
unicode_changes_when_nfkc_casefolded(0x0114, 0x0114).	% L&       LATIN CAPITAL LETTER E WITH BREVE
unicode_changes_when_nfkc_casefolded(0x0116, 0x0116).	% L&       LATIN CAPITAL LETTER E WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x0118, 0x0118).	% L&       LATIN CAPITAL LETTER E WITH OGONEK
unicode_changes_when_nfkc_casefolded(0x011A, 0x011A).	% L&       LATIN CAPITAL LETTER E WITH CARON
unicode_changes_when_nfkc_casefolded(0x011C, 0x011C).	% L&       LATIN CAPITAL LETTER G WITH CIRCUMFLEX
unicode_changes_when_nfkc_casefolded(0x011E, 0x011E).	% L&       LATIN CAPITAL LETTER G WITH BREVE
unicode_changes_when_nfkc_casefolded(0x0120, 0x0120).	% L&       LATIN CAPITAL LETTER G WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x0122, 0x0122).	% L&       LATIN CAPITAL LETTER G WITH CEDILLA
unicode_changes_when_nfkc_casefolded(0x0124, 0x0124).	% L&       LATIN CAPITAL LETTER H WITH CIRCUMFLEX
unicode_changes_when_nfkc_casefolded(0x0126, 0x0126).	% L&       LATIN CAPITAL LETTER H WITH STROKE
unicode_changes_when_nfkc_casefolded(0x0128, 0x0128).	% L&       LATIN CAPITAL LETTER I WITH TILDE
unicode_changes_when_nfkc_casefolded(0x012A, 0x012A).	% L&       LATIN CAPITAL LETTER I WITH MACRON
unicode_changes_when_nfkc_casefolded(0x012C, 0x012C).	% L&       LATIN CAPITAL LETTER I WITH BREVE
unicode_changes_when_nfkc_casefolded(0x012E, 0x012E).	% L&       LATIN CAPITAL LETTER I WITH OGONEK
unicode_changes_when_nfkc_casefolded(0x0130, 0x0130).	% L&       LATIN CAPITAL LETTER I WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x0132, 0x0134).	% L&   [3] LATIN CAPITAL LIGATURE IJ..LATIN CAPITAL LETTER J WITH CIRCUMFLEX
unicode_changes_when_nfkc_casefolded(0x0136, 0x0136).	% L&       LATIN CAPITAL LETTER K WITH CEDILLA
unicode_changes_when_nfkc_casefolded(0x0139, 0x0139).	% L&       LATIN CAPITAL LETTER L WITH ACUTE
unicode_changes_when_nfkc_casefolded(0x013B, 0x013B).	% L&       LATIN CAPITAL LETTER L WITH CEDILLA
unicode_changes_when_nfkc_casefolded(0x013D, 0x013D).	% L&       LATIN CAPITAL LETTER L WITH CARON
unicode_changes_when_nfkc_casefolded(0x013F, 0x0141).	% L&   [3] LATIN CAPITAL LETTER L WITH MIDDLE DOT..LATIN CAPITAL LETTER L WITH STROKE
unicode_changes_when_nfkc_casefolded(0x0143, 0x0143).	% L&       LATIN CAPITAL LETTER N WITH ACUTE
unicode_changes_when_nfkc_casefolded(0x0145, 0x0145).	% L&       LATIN CAPITAL LETTER N WITH CEDILLA
unicode_changes_when_nfkc_casefolded(0x0147, 0x0147).	% L&       LATIN CAPITAL LETTER N WITH CARON
unicode_changes_when_nfkc_casefolded(0x0149, 0x014A).	% L&   [2] LATIN SMALL LETTER N PRECEDED BY APOSTROPHE..LATIN CAPITAL LETTER ENG
unicode_changes_when_nfkc_casefolded(0x014C, 0x014C).	% L&       LATIN CAPITAL LETTER O WITH MACRON
unicode_changes_when_nfkc_casefolded(0x014E, 0x014E).	% L&       LATIN CAPITAL LETTER O WITH BREVE
unicode_changes_when_nfkc_casefolded(0x0150, 0x0150).	% L&       LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
unicode_changes_when_nfkc_casefolded(0x0152, 0x0152).	% L&       LATIN CAPITAL LIGATURE OE
unicode_changes_when_nfkc_casefolded(0x0154, 0x0154).	% L&       LATIN CAPITAL LETTER R WITH ACUTE
unicode_changes_when_nfkc_casefolded(0x0156, 0x0156).	% L&       LATIN CAPITAL LETTER R WITH CEDILLA
unicode_changes_when_nfkc_casefolded(0x0158, 0x0158).	% L&       LATIN CAPITAL LETTER R WITH CARON
unicode_changes_when_nfkc_casefolded(0x015A, 0x015A).	% L&       LATIN CAPITAL LETTER S WITH ACUTE
unicode_changes_when_nfkc_casefolded(0x015C, 0x015C).	% L&       LATIN CAPITAL LETTER S WITH CIRCUMFLEX
unicode_changes_when_nfkc_casefolded(0x015E, 0x015E).	% L&       LATIN CAPITAL LETTER S WITH CEDILLA
unicode_changes_when_nfkc_casefolded(0x0160, 0x0160).	% L&       LATIN CAPITAL LETTER S WITH CARON
unicode_changes_when_nfkc_casefolded(0x0162, 0x0162).	% L&       LATIN CAPITAL LETTER T WITH CEDILLA
unicode_changes_when_nfkc_casefolded(0x0164, 0x0164).	% L&       LATIN CAPITAL LETTER T WITH CARON
unicode_changes_when_nfkc_casefolded(0x0166, 0x0166).	% L&       LATIN CAPITAL LETTER T WITH STROKE
unicode_changes_when_nfkc_casefolded(0x0168, 0x0168).	% L&       LATIN CAPITAL LETTER U WITH TILDE
unicode_changes_when_nfkc_casefolded(0x016A, 0x016A).	% L&       LATIN CAPITAL LETTER U WITH MACRON
unicode_changes_when_nfkc_casefolded(0x016C, 0x016C).	% L&       LATIN CAPITAL LETTER U WITH BREVE
unicode_changes_when_nfkc_casefolded(0x016E, 0x016E).	% L&       LATIN CAPITAL LETTER U WITH RING ABOVE
unicode_changes_when_nfkc_casefolded(0x0170, 0x0170).	% L&       LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
unicode_changes_when_nfkc_casefolded(0x0172, 0x0172).	% L&       LATIN CAPITAL LETTER U WITH OGONEK
unicode_changes_when_nfkc_casefolded(0x0174, 0x0174).	% L&       LATIN CAPITAL LETTER W WITH CIRCUMFLEX
unicode_changes_when_nfkc_casefolded(0x0176, 0x0176).	% L&       LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
unicode_changes_when_nfkc_casefolded(0x0178, 0x0179).	% L&   [2] LATIN CAPITAL LETTER Y WITH DIAERESIS..LATIN CAPITAL LETTER Z WITH ACUTE
unicode_changes_when_nfkc_casefolded(0x017B, 0x017B).	% L&       LATIN CAPITAL LETTER Z WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x017D, 0x017D).	% L&       LATIN CAPITAL LETTER Z WITH CARON
unicode_changes_when_nfkc_casefolded(0x017F, 0x017F).	% L&       LATIN SMALL LETTER LONG S
unicode_changes_when_nfkc_casefolded(0x0181, 0x0182).	% L&   [2] LATIN CAPITAL LETTER B WITH HOOK..LATIN CAPITAL LETTER B WITH TOPBAR
unicode_changes_when_nfkc_casefolded(0x0184, 0x0184).	% L&       LATIN CAPITAL LETTER TONE SIX
unicode_changes_when_nfkc_casefolded(0x0186, 0x0187).	% L&   [2] LATIN CAPITAL LETTER OPEN O..LATIN CAPITAL LETTER C WITH HOOK
unicode_changes_when_nfkc_casefolded(0x0189, 0x018B).	% L&   [3] LATIN CAPITAL LETTER AFRICAN D..LATIN CAPITAL LETTER D WITH TOPBAR
unicode_changes_when_nfkc_casefolded(0x018E, 0x0191).	% L&   [4] LATIN CAPITAL LETTER REVERSED E..LATIN CAPITAL LETTER F WITH HOOK
unicode_changes_when_nfkc_casefolded(0x0193, 0x0194).	% L&   [2] LATIN CAPITAL LETTER G WITH HOOK..LATIN CAPITAL LETTER GAMMA
unicode_changes_when_nfkc_casefolded(0x0196, 0x0198).	% L&   [3] LATIN CAPITAL LETTER IOTA..LATIN CAPITAL LETTER K WITH HOOK
unicode_changes_when_nfkc_casefolded(0x019C, 0x019D).	% L&   [2] LATIN CAPITAL LETTER TURNED M..LATIN CAPITAL LETTER N WITH LEFT HOOK
unicode_changes_when_nfkc_casefolded(0x019F, 0x01A0).	% L&   [2] LATIN CAPITAL LETTER O WITH MIDDLE TILDE..LATIN CAPITAL LETTER O WITH HORN
unicode_changes_when_nfkc_casefolded(0x01A2, 0x01A2).	% L&       LATIN CAPITAL LETTER OI
unicode_changes_when_nfkc_casefolded(0x01A4, 0x01A4).	% L&       LATIN CAPITAL LETTER P WITH HOOK
unicode_changes_when_nfkc_casefolded(0x01A6, 0x01A7).	% L&   [2] LATIN LETTER YR..LATIN CAPITAL LETTER TONE TWO
unicode_changes_when_nfkc_casefolded(0x01A9, 0x01A9).	% L&       LATIN CAPITAL LETTER ESH
unicode_changes_when_nfkc_casefolded(0x01AC, 0x01AC).	% L&       LATIN CAPITAL LETTER T WITH HOOK
unicode_changes_when_nfkc_casefolded(0x01AE, 0x01AF).	% L&   [2] LATIN CAPITAL LETTER T WITH RETROFLEX HOOK..LATIN CAPITAL LETTER U WITH HORN
unicode_changes_when_nfkc_casefolded(0x01B1, 0x01B3).	% L&   [3] LATIN CAPITAL LETTER UPSILON..LATIN CAPITAL LETTER Y WITH HOOK
unicode_changes_when_nfkc_casefolded(0x01B5, 0x01B5).	% L&       LATIN CAPITAL LETTER Z WITH STROKE
unicode_changes_when_nfkc_casefolded(0x01B7, 0x01B8).	% L&   [2] LATIN CAPITAL LETTER EZH..LATIN CAPITAL LETTER EZH REVERSED
unicode_changes_when_nfkc_casefolded(0x01BC, 0x01BC).	% L&       LATIN CAPITAL LETTER TONE FIVE
unicode_changes_when_nfkc_casefolded(0x01C4, 0x01CD).	% L&  [10] LATIN CAPITAL LETTER DZ WITH CARON..LATIN CAPITAL LETTER A WITH CARON
unicode_changes_when_nfkc_casefolded(0x01CF, 0x01CF).	% L&       LATIN CAPITAL LETTER I WITH CARON
unicode_changes_when_nfkc_casefolded(0x01D1, 0x01D1).	% L&       LATIN CAPITAL LETTER O WITH CARON
unicode_changes_when_nfkc_casefolded(0x01D3, 0x01D3).	% L&       LATIN CAPITAL LETTER U WITH CARON
unicode_changes_when_nfkc_casefolded(0x01D5, 0x01D5).	% L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
unicode_changes_when_nfkc_casefolded(0x01D7, 0x01D7).	% L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
unicode_changes_when_nfkc_casefolded(0x01D9, 0x01D9).	% L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
unicode_changes_when_nfkc_casefolded(0x01DB, 0x01DB).	% L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
unicode_changes_when_nfkc_casefolded(0x01DE, 0x01DE).	% L&       LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
unicode_changes_when_nfkc_casefolded(0x01E0, 0x01E0).	% L&       LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
unicode_changes_when_nfkc_casefolded(0x01E2, 0x01E2).	% L&       LATIN CAPITAL LETTER AE WITH MACRON
unicode_changes_when_nfkc_casefolded(0x01E4, 0x01E4).	% L&       LATIN CAPITAL LETTER G WITH STROKE
unicode_changes_when_nfkc_casefolded(0x01E6, 0x01E6).	% L&       LATIN CAPITAL LETTER G WITH CARON
unicode_changes_when_nfkc_casefolded(0x01E8, 0x01E8).	% L&       LATIN CAPITAL LETTER K WITH CARON
unicode_changes_when_nfkc_casefolded(0x01EA, 0x01EA).	% L&       LATIN CAPITAL LETTER O WITH OGONEK
unicode_changes_when_nfkc_casefolded(0x01EC, 0x01EC).	% L&       LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
unicode_changes_when_nfkc_casefolded(0x01EE, 0x01EE).	% L&       LATIN CAPITAL LETTER EZH WITH CARON
unicode_changes_when_nfkc_casefolded(0x01F1, 0x01F4).	% L&   [4] LATIN CAPITAL LETTER DZ..LATIN CAPITAL LETTER G WITH ACUTE
unicode_changes_when_nfkc_casefolded(0x01F6, 0x01F8).	% L&   [3] LATIN CAPITAL LETTER HWAIR..LATIN CAPITAL LETTER N WITH GRAVE
unicode_changes_when_nfkc_casefolded(0x01FA, 0x01FA).	% L&       LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
unicode_changes_when_nfkc_casefolded(0x01FC, 0x01FC).	% L&       LATIN CAPITAL LETTER AE WITH ACUTE
unicode_changes_when_nfkc_casefolded(0x01FE, 0x01FE).	% L&       LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
unicode_changes_when_nfkc_casefolded(0x0200, 0x0200).	% L&       LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
unicode_changes_when_nfkc_casefolded(0x0202, 0x0202).	% L&       LATIN CAPITAL LETTER A WITH INVERTED BREVE
unicode_changes_when_nfkc_casefolded(0x0204, 0x0204).	% L&       LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
unicode_changes_when_nfkc_casefolded(0x0206, 0x0206).	% L&       LATIN CAPITAL LETTER E WITH INVERTED BREVE
unicode_changes_when_nfkc_casefolded(0x0208, 0x0208).	% L&       LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
unicode_changes_when_nfkc_casefolded(0x020A, 0x020A).	% L&       LATIN CAPITAL LETTER I WITH INVERTED BREVE
unicode_changes_when_nfkc_casefolded(0x020C, 0x020C).	% L&       LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
unicode_changes_when_nfkc_casefolded(0x020E, 0x020E).	% L&       LATIN CAPITAL LETTER O WITH INVERTED BREVE
unicode_changes_when_nfkc_casefolded(0x0210, 0x0210).	% L&       LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
unicode_changes_when_nfkc_casefolded(0x0212, 0x0212).	% L&       LATIN CAPITAL LETTER R WITH INVERTED BREVE
unicode_changes_when_nfkc_casefolded(0x0214, 0x0214).	% L&       LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
unicode_changes_when_nfkc_casefolded(0x0216, 0x0216).	% L&       LATIN CAPITAL LETTER U WITH INVERTED BREVE
unicode_changes_when_nfkc_casefolded(0x0218, 0x0218).	% L&       LATIN CAPITAL LETTER S WITH COMMA BELOW
unicode_changes_when_nfkc_casefolded(0x021A, 0x021A).	% L&       LATIN CAPITAL LETTER T WITH COMMA BELOW
unicode_changes_when_nfkc_casefolded(0x021C, 0x021C).	% L&       LATIN CAPITAL LETTER YOGH
unicode_changes_when_nfkc_casefolded(0x021E, 0x021E).	% L&       LATIN CAPITAL LETTER H WITH CARON
unicode_changes_when_nfkc_casefolded(0x0220, 0x0220).	% L&       LATIN CAPITAL LETTER N WITH LONG RIGHT LEG
unicode_changes_when_nfkc_casefolded(0x0222, 0x0222).	% L&       LATIN CAPITAL LETTER OU
unicode_changes_when_nfkc_casefolded(0x0224, 0x0224).	% L&       LATIN CAPITAL LETTER Z WITH HOOK
unicode_changes_when_nfkc_casefolded(0x0226, 0x0226).	% L&       LATIN CAPITAL LETTER A WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x0228, 0x0228).	% L&       LATIN CAPITAL LETTER E WITH CEDILLA
unicode_changes_when_nfkc_casefolded(0x022A, 0x022A).	% L&       LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
unicode_changes_when_nfkc_casefolded(0x022C, 0x022C).	% L&       LATIN CAPITAL LETTER O WITH TILDE AND MACRON
unicode_changes_when_nfkc_casefolded(0x022E, 0x022E).	% L&       LATIN CAPITAL LETTER O WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x0230, 0x0230).	% L&       LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
unicode_changes_when_nfkc_casefolded(0x0232, 0x0232).	% L&       LATIN CAPITAL LETTER Y WITH MACRON
unicode_changes_when_nfkc_casefolded(0x023A, 0x023B).	% L&   [2] LATIN CAPITAL LETTER A WITH STROKE..LATIN CAPITAL LETTER C WITH STROKE
unicode_changes_when_nfkc_casefolded(0x023D, 0x023E).	% L&   [2] LATIN CAPITAL LETTER L WITH BAR..LATIN CAPITAL LETTER T WITH DIAGONAL STROKE
unicode_changes_when_nfkc_casefolded(0x0241, 0x0241).	% L&       LATIN CAPITAL LETTER GLOTTAL STOP
unicode_changes_when_nfkc_casefolded(0x0243, 0x0246).	% L&   [4] LATIN CAPITAL LETTER B WITH STROKE..LATIN CAPITAL LETTER E WITH STROKE
unicode_changes_when_nfkc_casefolded(0x0248, 0x0248).	% L&       LATIN CAPITAL LETTER J WITH STROKE
unicode_changes_when_nfkc_casefolded(0x024A, 0x024A).	% L&       LATIN CAPITAL LETTER SMALL Q WITH HOOK TAIL
unicode_changes_when_nfkc_casefolded(0x024C, 0x024C).	% L&       LATIN CAPITAL LETTER R WITH STROKE
unicode_changes_when_nfkc_casefolded(0x024E, 0x024E).	% L&       LATIN CAPITAL LETTER Y WITH STROKE
unicode_changes_when_nfkc_casefolded(0x02B0, 0x02B8).	% Lm   [9] MODIFIER LETTER SMALL H..MODIFIER LETTER SMALL Y
unicode_changes_when_nfkc_casefolded(0x02D8, 0x02DD).	% Sk   [6] BREVE..DOUBLE ACUTE ACCENT
unicode_changes_when_nfkc_casefolded(0x02E0, 0x02E4).	% Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_changes_when_nfkc_casefolded(0x0340, 0x0341).	% Mn   [2] COMBINING GRAVE TONE MARK..COMBINING ACUTE TONE MARK
unicode_changes_when_nfkc_casefolded(0x0343, 0x0345).	% Mn   [3] COMBINING GREEK KORONIS..COMBINING GREEK YPOGEGRAMMENI
unicode_changes_when_nfkc_casefolded(0x034F, 0x034F).	% Mn       COMBINING GRAPHEME JOINER
unicode_changes_when_nfkc_casefolded(0x0370, 0x0370).	% L&       GREEK CAPITAL LETTER HETA
unicode_changes_when_nfkc_casefolded(0x0372, 0x0372).	% L&       GREEK CAPITAL LETTER ARCHAIC SAMPI
unicode_changes_when_nfkc_casefolded(0x0374, 0x0374).	% Lm       GREEK NUMERAL SIGN
unicode_changes_when_nfkc_casefolded(0x0376, 0x0376).	% L&       GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA
unicode_changes_when_nfkc_casefolded(0x037A, 0x037A).	% Lm       GREEK YPOGEGRAMMENI
unicode_changes_when_nfkc_casefolded(0x037E, 0x037E).	% Po       GREEK QUESTION MARK
unicode_changes_when_nfkc_casefolded(0x0384, 0x0385).	% Sk   [2] GREEK TONOS..GREEK DIALYTIKA TONOS
unicode_changes_when_nfkc_casefolded(0x0386, 0x0386).	% L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_changes_when_nfkc_casefolded(0x0387, 0x0387).	% Po       GREEK ANO TELEIA
unicode_changes_when_nfkc_casefolded(0x0388, 0x038A).	% L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_changes_when_nfkc_casefolded(0x038C, 0x038C).	% L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_changes_when_nfkc_casefolded(0x038E, 0x038F).	% L&   [2] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK CAPITAL LETTER OMEGA WITH TONOS
unicode_changes_when_nfkc_casefolded(0x0391, 0x03A1).	% L&  [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
unicode_changes_when_nfkc_casefolded(0x03A3, 0x03AB).	% L&   [9] GREEK CAPITAL LETTER SIGMA..GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
unicode_changes_when_nfkc_casefolded(0x03C2, 0x03C2).	% L&       GREEK SMALL LETTER FINAL SIGMA
unicode_changes_when_nfkc_casefolded(0x03CF, 0x03D6).	% L&   [8] GREEK CAPITAL KAI SYMBOL..GREEK PI SYMBOL
unicode_changes_when_nfkc_casefolded(0x03D8, 0x03D8).	% L&       GREEK LETTER ARCHAIC KOPPA
unicode_changes_when_nfkc_casefolded(0x03DA, 0x03DA).	% L&       GREEK LETTER STIGMA
unicode_changes_when_nfkc_casefolded(0x03DC, 0x03DC).	% L&       GREEK LETTER DIGAMMA
unicode_changes_when_nfkc_casefolded(0x03DE, 0x03DE).	% L&       GREEK LETTER KOPPA
unicode_changes_when_nfkc_casefolded(0x03E0, 0x03E0).	% L&       GREEK LETTER SAMPI
unicode_changes_when_nfkc_casefolded(0x03E2, 0x03E2).	% L&       COPTIC CAPITAL LETTER SHEI
unicode_changes_when_nfkc_casefolded(0x03E4, 0x03E4).	% L&       COPTIC CAPITAL LETTER FEI
unicode_changes_when_nfkc_casefolded(0x03E6, 0x03E6).	% L&       COPTIC CAPITAL LETTER KHEI
unicode_changes_when_nfkc_casefolded(0x03E8, 0x03E8).	% L&       COPTIC CAPITAL LETTER HORI
unicode_changes_when_nfkc_casefolded(0x03EA, 0x03EA).	% L&       COPTIC CAPITAL LETTER GANGIA
unicode_changes_when_nfkc_casefolded(0x03EC, 0x03EC).	% L&       COPTIC CAPITAL LETTER SHIMA
unicode_changes_when_nfkc_casefolded(0x03EE, 0x03EE).	% L&       COPTIC CAPITAL LETTER DEI
unicode_changes_when_nfkc_casefolded(0x03F0, 0x03F2).	% L&   [3] GREEK KAPPA SYMBOL..GREEK LUNATE SIGMA SYMBOL
unicode_changes_when_nfkc_casefolded(0x03F4, 0x03F5).	% L&   [2] GREEK CAPITAL THETA SYMBOL..GREEK LUNATE EPSILON SYMBOL
unicode_changes_when_nfkc_casefolded(0x03F7, 0x03F7).	% L&       GREEK CAPITAL LETTER SHO
unicode_changes_when_nfkc_casefolded(0x03F9, 0x03FA).	% L&   [2] GREEK CAPITAL LUNATE SIGMA SYMBOL..GREEK CAPITAL LETTER SAN
unicode_changes_when_nfkc_casefolded(0x03FD, 0x042F).	% L&  [51] GREEK CAPITAL REVERSED LUNATE SIGMA SYMBOL..CYRILLIC CAPITAL LETTER YA
unicode_changes_when_nfkc_casefolded(0x0460, 0x0460).	% L&       CYRILLIC CAPITAL LETTER OMEGA
unicode_changes_when_nfkc_casefolded(0x0462, 0x0462).	% L&       CYRILLIC CAPITAL LETTER YAT
unicode_changes_when_nfkc_casefolded(0x0464, 0x0464).	% L&       CYRILLIC CAPITAL LETTER IOTIFIED E
unicode_changes_when_nfkc_casefolded(0x0466, 0x0466).	% L&       CYRILLIC CAPITAL LETTER LITTLE YUS
unicode_changes_when_nfkc_casefolded(0x0468, 0x0468).	% L&       CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
unicode_changes_when_nfkc_casefolded(0x046A, 0x046A).	% L&       CYRILLIC CAPITAL LETTER BIG YUS
unicode_changes_when_nfkc_casefolded(0x046C, 0x046C).	% L&       CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
unicode_changes_when_nfkc_casefolded(0x046E, 0x046E).	% L&       CYRILLIC CAPITAL LETTER KSI
unicode_changes_when_nfkc_casefolded(0x0470, 0x0470).	% L&       CYRILLIC CAPITAL LETTER PSI
unicode_changes_when_nfkc_casefolded(0x0472, 0x0472).	% L&       CYRILLIC CAPITAL LETTER FITA
unicode_changes_when_nfkc_casefolded(0x0474, 0x0474).	% L&       CYRILLIC CAPITAL LETTER IZHITSA
unicode_changes_when_nfkc_casefolded(0x0476, 0x0476).	% L&       CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
unicode_changes_when_nfkc_casefolded(0x0478, 0x0478).	% L&       CYRILLIC CAPITAL LETTER UK
unicode_changes_when_nfkc_casefolded(0x047A, 0x047A).	% L&       CYRILLIC CAPITAL LETTER ROUND OMEGA
unicode_changes_when_nfkc_casefolded(0x047C, 0x047C).	% L&       CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
unicode_changes_when_nfkc_casefolded(0x047E, 0x047E).	% L&       CYRILLIC CAPITAL LETTER OT
unicode_changes_when_nfkc_casefolded(0x0480, 0x0480).	% L&       CYRILLIC CAPITAL LETTER KOPPA
unicode_changes_when_nfkc_casefolded(0x048A, 0x048A).	% L&       CYRILLIC CAPITAL LETTER SHORT I WITH TAIL
unicode_changes_when_nfkc_casefolded(0x048C, 0x048C).	% L&       CYRILLIC CAPITAL LETTER SEMISOFT SIGN
unicode_changes_when_nfkc_casefolded(0x048E, 0x048E).	% L&       CYRILLIC CAPITAL LETTER ER WITH TICK
unicode_changes_when_nfkc_casefolded(0x0490, 0x0490).	% L&       CYRILLIC CAPITAL LETTER GHE WITH UPTURN
unicode_changes_when_nfkc_casefolded(0x0492, 0x0492).	% L&       CYRILLIC CAPITAL LETTER GHE WITH STROKE
unicode_changes_when_nfkc_casefolded(0x0494, 0x0494).	% L&       CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
unicode_changes_when_nfkc_casefolded(0x0496, 0x0496).	% L&       CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x0498, 0x0498).	% L&       CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x049A, 0x049A).	% L&       CYRILLIC CAPITAL LETTER KA WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x049C, 0x049C).	% L&       CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
unicode_changes_when_nfkc_casefolded(0x049E, 0x049E).	% L&       CYRILLIC CAPITAL LETTER KA WITH STROKE
unicode_changes_when_nfkc_casefolded(0x04A0, 0x04A0).	% L&       CYRILLIC CAPITAL LETTER BASHKIR KA
unicode_changes_when_nfkc_casefolded(0x04A2, 0x04A2).	% L&       CYRILLIC CAPITAL LETTER EN WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x04A4, 0x04A4).	% L&       CYRILLIC CAPITAL LIGATURE EN GHE
unicode_changes_when_nfkc_casefolded(0x04A6, 0x04A6).	% L&       CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK
unicode_changes_when_nfkc_casefolded(0x04A8, 0x04A8).	% L&       CYRILLIC CAPITAL LETTER ABKHASIAN HA
unicode_changes_when_nfkc_casefolded(0x04AA, 0x04AA).	% L&       CYRILLIC CAPITAL LETTER ES WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x04AC, 0x04AC).	% L&       CYRILLIC CAPITAL LETTER TE WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x04AE, 0x04AE).	% L&       CYRILLIC CAPITAL LETTER STRAIGHT U
unicode_changes_when_nfkc_casefolded(0x04B0, 0x04B0).	% L&       CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
unicode_changes_when_nfkc_casefolded(0x04B2, 0x04B2).	% L&       CYRILLIC CAPITAL LETTER HA WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x04B4, 0x04B4).	% L&       CYRILLIC CAPITAL LIGATURE TE TSE
unicode_changes_when_nfkc_casefolded(0x04B6, 0x04B6).	% L&       CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x04B8, 0x04B8).	% L&       CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
unicode_changes_when_nfkc_casefolded(0x04BA, 0x04BA).	% L&       CYRILLIC CAPITAL LETTER SHHA
unicode_changes_when_nfkc_casefolded(0x04BC, 0x04BC).	% L&       CYRILLIC CAPITAL LETTER ABKHASIAN CHE
unicode_changes_when_nfkc_casefolded(0x04BE, 0x04BE).	% L&       CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x04C0, 0x04C1).	% L&   [2] CYRILLIC LETTER PALOCHKA..CYRILLIC CAPITAL LETTER ZHE WITH BREVE
unicode_changes_when_nfkc_casefolded(0x04C3, 0x04C3).	% L&       CYRILLIC CAPITAL LETTER KA WITH HOOK
unicode_changes_when_nfkc_casefolded(0x04C5, 0x04C5).	% L&       CYRILLIC CAPITAL LETTER EL WITH TAIL
unicode_changes_when_nfkc_casefolded(0x04C7, 0x04C7).	% L&       CYRILLIC CAPITAL LETTER EN WITH HOOK
unicode_changes_when_nfkc_casefolded(0x04C9, 0x04C9).	% L&       CYRILLIC CAPITAL LETTER EN WITH TAIL
unicode_changes_when_nfkc_casefolded(0x04CB, 0x04CB).	% L&       CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
unicode_changes_when_nfkc_casefolded(0x04CD, 0x04CD).	% L&       CYRILLIC CAPITAL LETTER EM WITH TAIL
unicode_changes_when_nfkc_casefolded(0x04D0, 0x04D0).	% L&       CYRILLIC CAPITAL LETTER A WITH BREVE
unicode_changes_when_nfkc_casefolded(0x04D2, 0x04D2).	% L&       CYRILLIC CAPITAL LETTER A WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x04D4, 0x04D4).	% L&       CYRILLIC CAPITAL LIGATURE A IE
unicode_changes_when_nfkc_casefolded(0x04D6, 0x04D6).	% L&       CYRILLIC CAPITAL LETTER IE WITH BREVE
unicode_changes_when_nfkc_casefolded(0x04D8, 0x04D8).	% L&       CYRILLIC CAPITAL LETTER SCHWA
unicode_changes_when_nfkc_casefolded(0x04DA, 0x04DA).	% L&       CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x04DC, 0x04DC).	% L&       CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x04DE, 0x04DE).	% L&       CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x04E0, 0x04E0).	% L&       CYRILLIC CAPITAL LETTER ABKHASIAN DZE
unicode_changes_when_nfkc_casefolded(0x04E2, 0x04E2).	% L&       CYRILLIC CAPITAL LETTER I WITH MACRON
unicode_changes_when_nfkc_casefolded(0x04E4, 0x04E4).	% L&       CYRILLIC CAPITAL LETTER I WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x04E6, 0x04E6).	% L&       CYRILLIC CAPITAL LETTER O WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x04E8, 0x04E8).	% L&       CYRILLIC CAPITAL LETTER BARRED O
unicode_changes_when_nfkc_casefolded(0x04EA, 0x04EA).	% L&       CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x04EC, 0x04EC).	% L&       CYRILLIC CAPITAL LETTER E WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x04EE, 0x04EE).	% L&       CYRILLIC CAPITAL LETTER U WITH MACRON
unicode_changes_when_nfkc_casefolded(0x04F0, 0x04F0).	% L&       CYRILLIC CAPITAL LETTER U WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x04F2, 0x04F2).	% L&       CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
unicode_changes_when_nfkc_casefolded(0x04F4, 0x04F4).	% L&       CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x04F6, 0x04F6).	% L&       CYRILLIC CAPITAL LETTER GHE WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x04F8, 0x04F8).	% L&       CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x04FA, 0x04FA).	% L&       CYRILLIC CAPITAL LETTER GHE WITH STROKE AND HOOK
unicode_changes_when_nfkc_casefolded(0x04FC, 0x04FC).	% L&       CYRILLIC CAPITAL LETTER HA WITH HOOK
unicode_changes_when_nfkc_casefolded(0x04FE, 0x04FE).	% L&       CYRILLIC CAPITAL LETTER HA WITH STROKE
unicode_changes_when_nfkc_casefolded(0x0500, 0x0500).	% L&       CYRILLIC CAPITAL LETTER KOMI DE
unicode_changes_when_nfkc_casefolded(0x0502, 0x0502).	% L&       CYRILLIC CAPITAL LETTER KOMI DJE
unicode_changes_when_nfkc_casefolded(0x0504, 0x0504).	% L&       CYRILLIC CAPITAL LETTER KOMI ZJE
unicode_changes_when_nfkc_casefolded(0x0506, 0x0506).	% L&       CYRILLIC CAPITAL LETTER KOMI DZJE
unicode_changes_when_nfkc_casefolded(0x0508, 0x0508).	% L&       CYRILLIC CAPITAL LETTER KOMI LJE
unicode_changes_when_nfkc_casefolded(0x050A, 0x050A).	% L&       CYRILLIC CAPITAL LETTER KOMI NJE
unicode_changes_when_nfkc_casefolded(0x050C, 0x050C).	% L&       CYRILLIC CAPITAL LETTER KOMI SJE
unicode_changes_when_nfkc_casefolded(0x050E, 0x050E).	% L&       CYRILLIC CAPITAL LETTER KOMI TJE
unicode_changes_when_nfkc_casefolded(0x0510, 0x0510).	% L&       CYRILLIC CAPITAL LETTER REVERSED ZE
unicode_changes_when_nfkc_casefolded(0x0512, 0x0512).	% L&       CYRILLIC CAPITAL LETTER EL WITH HOOK
unicode_changes_when_nfkc_casefolded(0x0514, 0x0514).	% L&       CYRILLIC CAPITAL LETTER LHA
unicode_changes_when_nfkc_casefolded(0x0516, 0x0516).	% L&       CYRILLIC CAPITAL LETTER RHA
unicode_changes_when_nfkc_casefolded(0x0518, 0x0518).	% L&       CYRILLIC CAPITAL LETTER YAE
unicode_changes_when_nfkc_casefolded(0x051A, 0x051A).	% L&       CYRILLIC CAPITAL LETTER QA
unicode_changes_when_nfkc_casefolded(0x051C, 0x051C).	% L&       CYRILLIC CAPITAL LETTER WE
unicode_changes_when_nfkc_casefolded(0x051E, 0x051E).	% L&       CYRILLIC CAPITAL LETTER ALEUT KA
unicode_changes_when_nfkc_casefolded(0x0520, 0x0520).	% L&       CYRILLIC CAPITAL LETTER EL WITH MIDDLE HOOK
unicode_changes_when_nfkc_casefolded(0x0522, 0x0522).	% L&       CYRILLIC CAPITAL LETTER EN WITH MIDDLE HOOK
unicode_changes_when_nfkc_casefolded(0x0524, 0x0524).	% L&       CYRILLIC CAPITAL LETTER PE WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x0526, 0x0526).	% L&       CYRILLIC CAPITAL LETTER SHHA WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x0531, 0x0556).	% L&  [38] ARMENIAN CAPITAL LETTER AYB..ARMENIAN CAPITAL LETTER FEH
unicode_changes_when_nfkc_casefolded(0x0587, 0x0587).	% L&       ARMENIAN SMALL LIGATURE ECH YIWN
unicode_changes_when_nfkc_casefolded(0x0675, 0x0678).	% Lo   [4] ARABIC LETTER HIGH HAMZA ALEF..ARABIC LETTER HIGH HAMZA YEH
unicode_changes_when_nfkc_casefolded(0x0958, 0x095F).	% Lo   [8] DEVANAGARI LETTER QA..DEVANAGARI LETTER YYA
unicode_changes_when_nfkc_casefolded(0x09DC, 0x09DD).	% Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_changes_when_nfkc_casefolded(0x09DF, 0x09DF).	% Lo       BENGALI LETTER YYA
unicode_changes_when_nfkc_casefolded(0x0A33, 0x0A33).	% Lo       GURMUKHI LETTER LLA
unicode_changes_when_nfkc_casefolded(0x0A36, 0x0A36).	% Lo       GURMUKHI LETTER SHA
unicode_changes_when_nfkc_casefolded(0x0A59, 0x0A5B).	% Lo   [3] GURMUKHI LETTER KHHA..GURMUKHI LETTER ZA
unicode_changes_when_nfkc_casefolded(0x0A5E, 0x0A5E).	% Lo       GURMUKHI LETTER FA
unicode_changes_when_nfkc_casefolded(0x0B5C, 0x0B5D).	% Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_changes_when_nfkc_casefolded(0x0E33, 0x0E33).	% Lo       THAI CHARACTER SARA AM
unicode_changes_when_nfkc_casefolded(0x0EB3, 0x0EB3).	% Lo       LAO VOWEL SIGN AM
unicode_changes_when_nfkc_casefolded(0x0EDC, 0x0EDD).	% Lo   [2] LAO HO NO..LAO HO MO
unicode_changes_when_nfkc_casefolded(0x0F0C, 0x0F0C).	% Po       TIBETAN MARK DELIMITER TSHEG BSTAR
unicode_changes_when_nfkc_casefolded(0x0F43, 0x0F43).	% Lo       TIBETAN LETTER GHA
unicode_changes_when_nfkc_casefolded(0x0F4D, 0x0F4D).	% Lo       TIBETAN LETTER DDHA
unicode_changes_when_nfkc_casefolded(0x0F52, 0x0F52).	% Lo       TIBETAN LETTER DHA
unicode_changes_when_nfkc_casefolded(0x0F57, 0x0F57).	% Lo       TIBETAN LETTER BHA
unicode_changes_when_nfkc_casefolded(0x0F5C, 0x0F5C).	% Lo       TIBETAN LETTER DZHA
unicode_changes_when_nfkc_casefolded(0x0F69, 0x0F69).	% Lo       TIBETAN LETTER KSSA
unicode_changes_when_nfkc_casefolded(0x0F73, 0x0F73).	% Mn       TIBETAN VOWEL SIGN II
unicode_changes_when_nfkc_casefolded(0x0F75, 0x0F79).	% Mn   [5] TIBETAN VOWEL SIGN UU..TIBETAN VOWEL SIGN VOCALIC LL
unicode_changes_when_nfkc_casefolded(0x0F81, 0x0F81).	% Mn       TIBETAN VOWEL SIGN REVERSED II
unicode_changes_when_nfkc_casefolded(0x0F93, 0x0F93).	% Mn       TIBETAN SUBJOINED LETTER GHA
unicode_changes_when_nfkc_casefolded(0x0F9D, 0x0F9D).	% Mn       TIBETAN SUBJOINED LETTER DDHA
unicode_changes_when_nfkc_casefolded(0x0FA2, 0x0FA2).	% Mn       TIBETAN SUBJOINED LETTER DHA
unicode_changes_when_nfkc_casefolded(0x0FA7, 0x0FA7).	% Mn       TIBETAN SUBJOINED LETTER BHA
unicode_changes_when_nfkc_casefolded(0x0FAC, 0x0FAC).	% Mn       TIBETAN SUBJOINED LETTER DZHA
unicode_changes_when_nfkc_casefolded(0x0FB9, 0x0FB9).	% Mn       TIBETAN SUBJOINED LETTER KSSA
unicode_changes_when_nfkc_casefolded(0x10A0, 0x10C5).	% L&  [38] GEORGIAN CAPITAL LETTER AN..GEORGIAN CAPITAL LETTER HOE
unicode_changes_when_nfkc_casefolded(0x10C7, 0x10C7).	% L&       GEORGIAN CAPITAL LETTER YN
unicode_changes_when_nfkc_casefolded(0x10CD, 0x10CD).	% L&       GEORGIAN CAPITAL LETTER AEN
unicode_changes_when_nfkc_casefolded(0x10FC, 0x10FC).	% Lm       MODIFIER LETTER GEORGIAN NAR
unicode_changes_when_nfkc_casefolded(0x115F, 0x1160).	% Lo   [2] HANGUL CHOSEONG FILLER..HANGUL JUNGSEONG FILLER
unicode_changes_when_nfkc_casefolded(0x17B4, 0x17B5).	% Mn   [2] KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA
unicode_changes_when_nfkc_casefolded(0x180B, 0x180D).	% Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
unicode_changes_when_nfkc_casefolded(0x1D2C, 0x1D2E).	% Lm   [3] MODIFIER LETTER CAPITAL A..MODIFIER LETTER CAPITAL B
unicode_changes_when_nfkc_casefolded(0x1D30, 0x1D3A).	% Lm  [11] MODIFIER LETTER CAPITAL D..MODIFIER LETTER CAPITAL N
unicode_changes_when_nfkc_casefolded(0x1D3C, 0x1D4D).	% Lm  [18] MODIFIER LETTER CAPITAL O..MODIFIER LETTER SMALL G
unicode_changes_when_nfkc_casefolded(0x1D4F, 0x1D6A).	% Lm  [28] MODIFIER LETTER SMALL K..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_changes_when_nfkc_casefolded(0x1D78, 0x1D78).	% Lm       MODIFIER LETTER CYRILLIC EN
unicode_changes_when_nfkc_casefolded(0x1D9B, 0x1DBF).	% Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_changes_when_nfkc_casefolded(0x1E00, 0x1E00).	% L&       LATIN CAPITAL LETTER A WITH RING BELOW
unicode_changes_when_nfkc_casefolded(0x1E02, 0x1E02).	% L&       LATIN CAPITAL LETTER B WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E04, 0x1E04).	% L&       LATIN CAPITAL LETTER B WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E06, 0x1E06).	% L&       LATIN CAPITAL LETTER B WITH LINE BELOW
unicode_changes_when_nfkc_casefolded(0x1E08, 0x1E08).	% L&       LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
unicode_changes_when_nfkc_casefolded(0x1E0A, 0x1E0A).	% L&       LATIN CAPITAL LETTER D WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E0C, 0x1E0C).	% L&       LATIN CAPITAL LETTER D WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E0E, 0x1E0E).	% L&       LATIN CAPITAL LETTER D WITH LINE BELOW
unicode_changes_when_nfkc_casefolded(0x1E10, 0x1E10).	% L&       LATIN CAPITAL LETTER D WITH CEDILLA
unicode_changes_when_nfkc_casefolded(0x1E12, 0x1E12).	% L&       LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
unicode_changes_when_nfkc_casefolded(0x1E14, 0x1E14).	% L&       LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
unicode_changes_when_nfkc_casefolded(0x1E16, 0x1E16).	% L&       LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
unicode_changes_when_nfkc_casefolded(0x1E18, 0x1E18).	% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
unicode_changes_when_nfkc_casefolded(0x1E1A, 0x1E1A).	% L&       LATIN CAPITAL LETTER E WITH TILDE BELOW
unicode_changes_when_nfkc_casefolded(0x1E1C, 0x1E1C).	% L&       LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
unicode_changes_when_nfkc_casefolded(0x1E1E, 0x1E1E).	% L&       LATIN CAPITAL LETTER F WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E20, 0x1E20).	% L&       LATIN CAPITAL LETTER G WITH MACRON
unicode_changes_when_nfkc_casefolded(0x1E22, 0x1E22).	% L&       LATIN CAPITAL LETTER H WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E24, 0x1E24).	% L&       LATIN CAPITAL LETTER H WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E26, 0x1E26).	% L&       LATIN CAPITAL LETTER H WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x1E28, 0x1E28).	% L&       LATIN CAPITAL LETTER H WITH CEDILLA
unicode_changes_when_nfkc_casefolded(0x1E2A, 0x1E2A).	% L&       LATIN CAPITAL LETTER H WITH BREVE BELOW
unicode_changes_when_nfkc_casefolded(0x1E2C, 0x1E2C).	% L&       LATIN CAPITAL LETTER I WITH TILDE BELOW
unicode_changes_when_nfkc_casefolded(0x1E2E, 0x1E2E).	% L&       LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
unicode_changes_when_nfkc_casefolded(0x1E30, 0x1E30).	% L&       LATIN CAPITAL LETTER K WITH ACUTE
unicode_changes_when_nfkc_casefolded(0x1E32, 0x1E32).	% L&       LATIN CAPITAL LETTER K WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E34, 0x1E34).	% L&       LATIN CAPITAL LETTER K WITH LINE BELOW
unicode_changes_when_nfkc_casefolded(0x1E36, 0x1E36).	% L&       LATIN CAPITAL LETTER L WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E38, 0x1E38).	% L&       LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
unicode_changes_when_nfkc_casefolded(0x1E3A, 0x1E3A).	% L&       LATIN CAPITAL LETTER L WITH LINE BELOW
unicode_changes_when_nfkc_casefolded(0x1E3C, 0x1E3C).	% L&       LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
unicode_changes_when_nfkc_casefolded(0x1E3E, 0x1E3E).	% L&       LATIN CAPITAL LETTER M WITH ACUTE
unicode_changes_when_nfkc_casefolded(0x1E40, 0x1E40).	% L&       LATIN CAPITAL LETTER M WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E42, 0x1E42).	% L&       LATIN CAPITAL LETTER M WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E44, 0x1E44).	% L&       LATIN CAPITAL LETTER N WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E46, 0x1E46).	% L&       LATIN CAPITAL LETTER N WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E48, 0x1E48).	% L&       LATIN CAPITAL LETTER N WITH LINE BELOW
unicode_changes_when_nfkc_casefolded(0x1E4A, 0x1E4A).	% L&       LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
unicode_changes_when_nfkc_casefolded(0x1E4C, 0x1E4C).	% L&       LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
unicode_changes_when_nfkc_casefolded(0x1E4E, 0x1E4E).	% L&       LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
unicode_changes_when_nfkc_casefolded(0x1E50, 0x1E50).	% L&       LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
unicode_changes_when_nfkc_casefolded(0x1E52, 0x1E52).	% L&       LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
unicode_changes_when_nfkc_casefolded(0x1E54, 0x1E54).	% L&       LATIN CAPITAL LETTER P WITH ACUTE
unicode_changes_when_nfkc_casefolded(0x1E56, 0x1E56).	% L&       LATIN CAPITAL LETTER P WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E58, 0x1E58).	% L&       LATIN CAPITAL LETTER R WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E5A, 0x1E5A).	% L&       LATIN CAPITAL LETTER R WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E5C, 0x1E5C).	% L&       LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
unicode_changes_when_nfkc_casefolded(0x1E5E, 0x1E5E).	% L&       LATIN CAPITAL LETTER R WITH LINE BELOW
unicode_changes_when_nfkc_casefolded(0x1E60, 0x1E60).	% L&       LATIN CAPITAL LETTER S WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E62, 0x1E62).	% L&       LATIN CAPITAL LETTER S WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E64, 0x1E64).	% L&       LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E66, 0x1E66).	% L&       LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E68, 0x1E68).	% L&       LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E6A, 0x1E6A).	% L&       LATIN CAPITAL LETTER T WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E6C, 0x1E6C).	% L&       LATIN CAPITAL LETTER T WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E6E, 0x1E6E).	% L&       LATIN CAPITAL LETTER T WITH LINE BELOW
unicode_changes_when_nfkc_casefolded(0x1E70, 0x1E70).	% L&       LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
unicode_changes_when_nfkc_casefolded(0x1E72, 0x1E72).	% L&       LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
unicode_changes_when_nfkc_casefolded(0x1E74, 0x1E74).	% L&       LATIN CAPITAL LETTER U WITH TILDE BELOW
unicode_changes_when_nfkc_casefolded(0x1E76, 0x1E76).	% L&       LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
unicode_changes_when_nfkc_casefolded(0x1E78, 0x1E78).	% L&       LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
unicode_changes_when_nfkc_casefolded(0x1E7A, 0x1E7A).	% L&       LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
unicode_changes_when_nfkc_casefolded(0x1E7C, 0x1E7C).	% L&       LATIN CAPITAL LETTER V WITH TILDE
unicode_changes_when_nfkc_casefolded(0x1E7E, 0x1E7E).	% L&       LATIN CAPITAL LETTER V WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E80, 0x1E80).	% L&       LATIN CAPITAL LETTER W WITH GRAVE
unicode_changes_when_nfkc_casefolded(0x1E82, 0x1E82).	% L&       LATIN CAPITAL LETTER W WITH ACUTE
unicode_changes_when_nfkc_casefolded(0x1E84, 0x1E84).	% L&       LATIN CAPITAL LETTER W WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x1E86, 0x1E86).	% L&       LATIN CAPITAL LETTER W WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E88, 0x1E88).	% L&       LATIN CAPITAL LETTER W WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E8A, 0x1E8A).	% L&       LATIN CAPITAL LETTER X WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E8C, 0x1E8C).	% L&       LATIN CAPITAL LETTER X WITH DIAERESIS
unicode_changes_when_nfkc_casefolded(0x1E8E, 0x1E8E).	% L&       LATIN CAPITAL LETTER Y WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E90, 0x1E90).	% L&       LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
unicode_changes_when_nfkc_casefolded(0x1E92, 0x1E92).	% L&       LATIN CAPITAL LETTER Z WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1E94, 0x1E94).	% L&       LATIN CAPITAL LETTER Z WITH LINE BELOW
unicode_changes_when_nfkc_casefolded(0x1E9A, 0x1E9B).	% L&   [2] LATIN SMALL LETTER A WITH RIGHT HALF RING..LATIN SMALL LETTER LONG S WITH DOT ABOVE
unicode_changes_when_nfkc_casefolded(0x1E9E, 0x1E9E).	% L&       LATIN CAPITAL LETTER SHARP S
unicode_changes_when_nfkc_casefolded(0x1EA0, 0x1EA0).	% L&       LATIN CAPITAL LETTER A WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1EA2, 0x1EA2).	% L&       LATIN CAPITAL LETTER A WITH HOOK ABOVE
unicode_changes_when_nfkc_casefolded(0x1EA4, 0x1EA4).	% L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
unicode_changes_when_nfkc_casefolded(0x1EA6, 0x1EA6).	% L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
unicode_changes_when_nfkc_casefolded(0x1EA8, 0x1EA8).	% L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
unicode_changes_when_nfkc_casefolded(0x1EAA, 0x1EAA).	% L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
unicode_changes_when_nfkc_casefolded(0x1EAC, 0x1EAC).	% L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1EAE, 0x1EAE).	% L&       LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
unicode_changes_when_nfkc_casefolded(0x1EB0, 0x1EB0).	% L&       LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
unicode_changes_when_nfkc_casefolded(0x1EB2, 0x1EB2).	% L&       LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
unicode_changes_when_nfkc_casefolded(0x1EB4, 0x1EB4).	% L&       LATIN CAPITAL LETTER A WITH BREVE AND TILDE
unicode_changes_when_nfkc_casefolded(0x1EB6, 0x1EB6).	% L&       LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1EB8, 0x1EB8).	% L&       LATIN CAPITAL LETTER E WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1EBA, 0x1EBA).	% L&       LATIN CAPITAL LETTER E WITH HOOK ABOVE
unicode_changes_when_nfkc_casefolded(0x1EBC, 0x1EBC).	% L&       LATIN CAPITAL LETTER E WITH TILDE
unicode_changes_when_nfkc_casefolded(0x1EBE, 0x1EBE).	% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
unicode_changes_when_nfkc_casefolded(0x1EC0, 0x1EC0).	% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
unicode_changes_when_nfkc_casefolded(0x1EC2, 0x1EC2).	% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
unicode_changes_when_nfkc_casefolded(0x1EC4, 0x1EC4).	% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
unicode_changes_when_nfkc_casefolded(0x1EC6, 0x1EC6).	% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1EC8, 0x1EC8).	% L&       LATIN CAPITAL LETTER I WITH HOOK ABOVE
unicode_changes_when_nfkc_casefolded(0x1ECA, 0x1ECA).	% L&       LATIN CAPITAL LETTER I WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1ECC, 0x1ECC).	% L&       LATIN CAPITAL LETTER O WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1ECE, 0x1ECE).	% L&       LATIN CAPITAL LETTER O WITH HOOK ABOVE
unicode_changes_when_nfkc_casefolded(0x1ED0, 0x1ED0).	% L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
unicode_changes_when_nfkc_casefolded(0x1ED2, 0x1ED2).	% L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
unicode_changes_when_nfkc_casefolded(0x1ED4, 0x1ED4).	% L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
unicode_changes_when_nfkc_casefolded(0x1ED6, 0x1ED6).	% L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
unicode_changes_when_nfkc_casefolded(0x1ED8, 0x1ED8).	% L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1EDA, 0x1EDA).	% L&       LATIN CAPITAL LETTER O WITH HORN AND ACUTE
unicode_changes_when_nfkc_casefolded(0x1EDC, 0x1EDC).	% L&       LATIN CAPITAL LETTER O WITH HORN AND GRAVE
unicode_changes_when_nfkc_casefolded(0x1EDE, 0x1EDE).	% L&       LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
unicode_changes_when_nfkc_casefolded(0x1EE0, 0x1EE0).	% L&       LATIN CAPITAL LETTER O WITH HORN AND TILDE
unicode_changes_when_nfkc_casefolded(0x1EE2, 0x1EE2).	% L&       LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1EE4, 0x1EE4).	% L&       LATIN CAPITAL LETTER U WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1EE6, 0x1EE6).	% L&       LATIN CAPITAL LETTER U WITH HOOK ABOVE
unicode_changes_when_nfkc_casefolded(0x1EE8, 0x1EE8).	% L&       LATIN CAPITAL LETTER U WITH HORN AND ACUTE
unicode_changes_when_nfkc_casefolded(0x1EEA, 0x1EEA).	% L&       LATIN CAPITAL LETTER U WITH HORN AND GRAVE
unicode_changes_when_nfkc_casefolded(0x1EEC, 0x1EEC).	% L&       LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
unicode_changes_when_nfkc_casefolded(0x1EEE, 0x1EEE).	% L&       LATIN CAPITAL LETTER U WITH HORN AND TILDE
unicode_changes_when_nfkc_casefolded(0x1EF0, 0x1EF0).	% L&       LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1EF2, 0x1EF2).	% L&       LATIN CAPITAL LETTER Y WITH GRAVE
unicode_changes_when_nfkc_casefolded(0x1EF4, 0x1EF4).	% L&       LATIN CAPITAL LETTER Y WITH DOT BELOW
unicode_changes_when_nfkc_casefolded(0x1EF6, 0x1EF6).	% L&       LATIN CAPITAL LETTER Y WITH HOOK ABOVE
unicode_changes_when_nfkc_casefolded(0x1EF8, 0x1EF8).	% L&       LATIN CAPITAL LETTER Y WITH TILDE
unicode_changes_when_nfkc_casefolded(0x1EFA, 0x1EFA).	% L&       LATIN CAPITAL LETTER MIDDLE-WELSH LL
unicode_changes_when_nfkc_casefolded(0x1EFC, 0x1EFC).	% L&       LATIN CAPITAL LETTER MIDDLE-WELSH V
unicode_changes_when_nfkc_casefolded(0x1EFE, 0x1EFE).	% L&       LATIN CAPITAL LETTER Y WITH LOOP
unicode_changes_when_nfkc_casefolded(0x1F08, 0x1F0F).	% L&   [8] GREEK CAPITAL LETTER ALPHA WITH PSILI..GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
unicode_changes_when_nfkc_casefolded(0x1F18, 0x1F1D).	% L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_changes_when_nfkc_casefolded(0x1F28, 0x1F2F).	% L&   [8] GREEK CAPITAL LETTER ETA WITH PSILI..GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
unicode_changes_when_nfkc_casefolded(0x1F38, 0x1F3F).	% L&   [8] GREEK CAPITAL LETTER IOTA WITH PSILI..GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
unicode_changes_when_nfkc_casefolded(0x1F48, 0x1F4D).	% L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_changes_when_nfkc_casefolded(0x1F59, 0x1F59).	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_changes_when_nfkc_casefolded(0x1F5B, 0x1F5B).	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_changes_when_nfkc_casefolded(0x1F5D, 0x1F5D).	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_changes_when_nfkc_casefolded(0x1F5F, 0x1F5F).	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_changes_when_nfkc_casefolded(0x1F68, 0x1F6F).	% L&   [8] GREEK CAPITAL LETTER OMEGA WITH PSILI..GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
unicode_changes_when_nfkc_casefolded(0x1F71, 0x1F71).	% L&       GREEK SMALL LETTER ALPHA WITH OXIA
unicode_changes_when_nfkc_casefolded(0x1F73, 0x1F73).	% L&       GREEK SMALL LETTER EPSILON WITH OXIA
unicode_changes_when_nfkc_casefolded(0x1F75, 0x1F75).	% L&       GREEK SMALL LETTER ETA WITH OXIA
unicode_changes_when_nfkc_casefolded(0x1F77, 0x1F77).	% L&       GREEK SMALL LETTER IOTA WITH OXIA
unicode_changes_when_nfkc_casefolded(0x1F79, 0x1F79).	% L&       GREEK SMALL LETTER OMICRON WITH OXIA
unicode_changes_when_nfkc_casefolded(0x1F7B, 0x1F7B).	% L&       GREEK SMALL LETTER UPSILON WITH OXIA
unicode_changes_when_nfkc_casefolded(0x1F7D, 0x1F7D).	% L&       GREEK SMALL LETTER OMEGA WITH OXIA
unicode_changes_when_nfkc_casefolded(0x1F80, 0x1FAF).	% L&  [48] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_changes_when_nfkc_casefolded(0x1FB2, 0x1FB4).	% L&   [3] GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_changes_when_nfkc_casefolded(0x1FB7, 0x1FBC).	% L&   [6] GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_changes_when_nfkc_casefolded(0x1FBD, 0x1FBD).	% Sk       GREEK KORONIS
unicode_changes_when_nfkc_casefolded(0x1FBE, 0x1FBE).	% L&       GREEK PROSGEGRAMMENI
unicode_changes_when_nfkc_casefolded(0x1FBF, 0x1FC1).	% Sk   [3] GREEK PSILI..GREEK DIALYTIKA AND PERISPOMENI
unicode_changes_when_nfkc_casefolded(0x1FC2, 0x1FC4).	% L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_changes_when_nfkc_casefolded(0x1FC7, 0x1FCC).	% L&   [6] GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_changes_when_nfkc_casefolded(0x1FCD, 0x1FCF).	% Sk   [3] GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI
unicode_changes_when_nfkc_casefolded(0x1FD3, 0x1FD3).	% L&       GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_changes_when_nfkc_casefolded(0x1FD8, 0x1FDB).	% L&   [4] GREEK CAPITAL LETTER IOTA WITH VRACHY..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_changes_when_nfkc_casefolded(0x1FDD, 0x1FDF).	% Sk   [3] GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI
unicode_changes_when_nfkc_casefolded(0x1FE3, 0x1FE3).	% L&       GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
unicode_changes_when_nfkc_casefolded(0x1FE8, 0x1FEC).	% L&   [5] GREEK CAPITAL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_changes_when_nfkc_casefolded(0x1FED, 0x1FEF).	% Sk   [3] GREEK DIALYTIKA AND VARIA..GREEK VARIA
unicode_changes_when_nfkc_casefolded(0x1FF2, 0x1FF4).	% L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_changes_when_nfkc_casefolded(0x1FF7, 0x1FFC).	% L&   [6] GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_changes_when_nfkc_casefolded(0x1FFD, 0x1FFE).	% Sk   [2] GREEK OXIA..GREEK DASIA
unicode_changes_when_nfkc_casefolded(0x2000, 0x200A).	% Zs  [11] EN QUAD..HAIR SPACE
unicode_changes_when_nfkc_casefolded(0x200B, 0x200F).	% Cf   [5] ZERO WIDTH SPACE..RIGHT-TO-LEFT MARK
unicode_changes_when_nfkc_casefolded(0x2011, 0x2011).	% Pd       NON-BREAKING HYPHEN
unicode_changes_when_nfkc_casefolded(0x2017, 0x2017).	% Po       DOUBLE LOW LINE
unicode_changes_when_nfkc_casefolded(0x2024, 0x2026).	% Po   [3] ONE DOT LEADER..HORIZONTAL ELLIPSIS
unicode_changes_when_nfkc_casefolded(0x202A, 0x202E).	% Cf   [5] LEFT-TO-RIGHT EMBEDDING..RIGHT-TO-LEFT OVERRIDE
unicode_changes_when_nfkc_casefolded(0x202F, 0x202F).	% Zs       NARROW NO-BREAK SPACE
unicode_changes_when_nfkc_casefolded(0x2033, 0x2034).	% Po   [2] DOUBLE PRIME..TRIPLE PRIME
unicode_changes_when_nfkc_casefolded(0x2036, 0x2037).	% Po   [2] REVERSED DOUBLE PRIME..REVERSED TRIPLE PRIME
unicode_changes_when_nfkc_casefolded(0x203C, 0x203C).	% Po       DOUBLE EXCLAMATION MARK
unicode_changes_when_nfkc_casefolded(0x203E, 0x203E).	% Po       OVERLINE
unicode_changes_when_nfkc_casefolded(0x2047, 0x2049).	% Po   [3] DOUBLE QUESTION MARK..EXCLAMATION QUESTION MARK
unicode_changes_when_nfkc_casefolded(0x2057, 0x2057).	% Po       QUADRUPLE PRIME
unicode_changes_when_nfkc_casefolded(0x205F, 0x205F).	% Zs       MEDIUM MATHEMATICAL SPACE
unicode_changes_when_nfkc_casefolded(0x2060, 0x2064).	% Cf   [5] WORD JOINER..INVISIBLE PLUS
unicode_changes_when_nfkc_casefolded(0x2065, 0x2069).	% Cn   [5] <reserved-2065>..<reserved-2069>
unicode_changes_when_nfkc_casefolded(0x206A, 0x206F).	% Cf   [6] INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES
unicode_changes_when_nfkc_casefolded(0x2070, 0x2070).	% No       SUPERSCRIPT ZERO
unicode_changes_when_nfkc_casefolded(0x2071, 0x2071).	% Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_changes_when_nfkc_casefolded(0x2074, 0x2079).	% No   [6] SUPERSCRIPT FOUR..SUPERSCRIPT NINE
unicode_changes_when_nfkc_casefolded(0x207A, 0x207C).	% Sm   [3] SUPERSCRIPT PLUS SIGN..SUPERSCRIPT EQUALS SIGN
unicode_changes_when_nfkc_casefolded(0x207D, 0x207D).	% Ps       SUPERSCRIPT LEFT PARENTHESIS
unicode_changes_when_nfkc_casefolded(0x207E, 0x207E).	% Pe       SUPERSCRIPT RIGHT PARENTHESIS
unicode_changes_when_nfkc_casefolded(0x207F, 0x207F).	% Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_changes_when_nfkc_casefolded(0x2080, 0x2089).	% No  [10] SUBSCRIPT ZERO..SUBSCRIPT NINE
unicode_changes_when_nfkc_casefolded(0x208A, 0x208C).	% Sm   [3] SUBSCRIPT PLUS SIGN..SUBSCRIPT EQUALS SIGN
unicode_changes_when_nfkc_casefolded(0x208D, 0x208D).	% Ps       SUBSCRIPT LEFT PARENTHESIS
unicode_changes_when_nfkc_casefolded(0x208E, 0x208E).	% Pe       SUBSCRIPT RIGHT PARENTHESIS
unicode_changes_when_nfkc_casefolded(0x2090, 0x209C).	% Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_changes_when_nfkc_casefolded(0x20A8, 0x20A8).	% Sc       RUPEE SIGN
unicode_changes_when_nfkc_casefolded(0x2100, 0x2101).	% So   [2] ACCOUNT OF..ADDRESSED TO THE SUBJECT
unicode_changes_when_nfkc_casefolded(0x2102, 0x2102).	% L&       DOUBLE-STRUCK CAPITAL C
unicode_changes_when_nfkc_casefolded(0x2103, 0x2103).	% So       DEGREE CELSIUS
unicode_changes_when_nfkc_casefolded(0x2105, 0x2106).	% So   [2] CARE OF..CADA UNA
unicode_changes_when_nfkc_casefolded(0x2107, 0x2107).	% L&       EULER CONSTANT
unicode_changes_when_nfkc_casefolded(0x2109, 0x2109).	% So       DEGREE FAHRENHEIT
unicode_changes_when_nfkc_casefolded(0x210A, 0x2113).	% L&  [10] SCRIPT SMALL G..SCRIPT SMALL L
unicode_changes_when_nfkc_casefolded(0x2115, 0x2115).	% L&       DOUBLE-STRUCK CAPITAL N
unicode_changes_when_nfkc_casefolded(0x2116, 0x2116).	% So       NUMERO SIGN
unicode_changes_when_nfkc_casefolded(0x2119, 0x211D).	% L&   [5] DOUBLE-STRUCK CAPITAL P..DOUBLE-STRUCK CAPITAL R
unicode_changes_when_nfkc_casefolded(0x2120, 0x2122).	% So   [3] SERVICE MARK..TRADE MARK SIGN
unicode_changes_when_nfkc_casefolded(0x2124, 0x2124).	% L&       DOUBLE-STRUCK CAPITAL Z
unicode_changes_when_nfkc_casefolded(0x2126, 0x2126).	% L&       OHM SIGN
unicode_changes_when_nfkc_casefolded(0x2128, 0x2128).	% L&       BLACK-LETTER CAPITAL Z
unicode_changes_when_nfkc_casefolded(0x212A, 0x212D).	% L&   [4] KELVIN SIGN..BLACK-LETTER CAPITAL C
unicode_changes_when_nfkc_casefolded(0x212F, 0x2134).	% L&   [6] SCRIPT SMALL E..SCRIPT SMALL O
unicode_changes_when_nfkc_casefolded(0x2135, 0x2138).	% Lo   [4] ALEF SYMBOL..DALET SYMBOL
unicode_changes_when_nfkc_casefolded(0x2139, 0x2139).	% L&       INFORMATION SOURCE
unicode_changes_when_nfkc_casefolded(0x213B, 0x213B).	% So       FACSIMILE SIGN
unicode_changes_when_nfkc_casefolded(0x213C, 0x213F).	% L&   [4] DOUBLE-STRUCK SMALL PI..DOUBLE-STRUCK CAPITAL PI
unicode_changes_when_nfkc_casefolded(0x2140, 0x2140).	% Sm       DOUBLE-STRUCK N-ARY SUMMATION
unicode_changes_when_nfkc_casefolded(0x2145, 0x2149).	% L&   [5] DOUBLE-STRUCK ITALIC CAPITAL D..DOUBLE-STRUCK ITALIC SMALL J
unicode_changes_when_nfkc_casefolded(0x2150, 0x215F).	% No  [16] VULGAR FRACTION ONE SEVENTH..FRACTION NUMERATOR ONE
unicode_changes_when_nfkc_casefolded(0x2160, 0x217F).	% Nl  [32] ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL ONE THOUSAND
unicode_changes_when_nfkc_casefolded(0x2183, 0x2183).	% L&       ROMAN NUMERAL REVERSED ONE HUNDRED
unicode_changes_when_nfkc_casefolded(0x2189, 0x2189).	% No       VULGAR FRACTION ZERO THIRDS
unicode_changes_when_nfkc_casefolded(0x222C, 0x222D).	% Sm   [2] DOUBLE INTEGRAL..TRIPLE INTEGRAL
unicode_changes_when_nfkc_casefolded(0x222F, 0x2230).	% Sm   [2] SURFACE INTEGRAL..VOLUME INTEGRAL
unicode_changes_when_nfkc_casefolded(0x2329, 0x2329).	% Ps       LEFT-POINTING ANGLE BRACKET
unicode_changes_when_nfkc_casefolded(0x232A, 0x232A).	% Pe       RIGHT-POINTING ANGLE BRACKET
unicode_changes_when_nfkc_casefolded(0x2460, 0x249B).	% No  [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
unicode_changes_when_nfkc_casefolded(0x249C, 0x24E9).	% So  [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_changes_when_nfkc_casefolded(0x24EA, 0x24EA).	% No       CIRCLED DIGIT ZERO
unicode_changes_when_nfkc_casefolded(0x2A0C, 0x2A0C).	% Sm       QUADRUPLE INTEGRAL OPERATOR
unicode_changes_when_nfkc_casefolded(0x2A74, 0x2A76).	% Sm   [3] DOUBLE COLON EQUAL..THREE CONSECUTIVE EQUALS SIGNS
unicode_changes_when_nfkc_casefolded(0x2ADC, 0x2ADC).	% Sm       FORKING
unicode_changes_when_nfkc_casefolded(0x2C00, 0x2C2E).	% L&  [47] GLAGOLITIC CAPITAL LETTER AZU..GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_changes_when_nfkc_casefolded(0x2C60, 0x2C60).	% L&       LATIN CAPITAL LETTER L WITH DOUBLE BAR
unicode_changes_when_nfkc_casefolded(0x2C62, 0x2C64).	% L&   [3] LATIN CAPITAL LETTER L WITH MIDDLE TILDE..LATIN CAPITAL LETTER R WITH TAIL
unicode_changes_when_nfkc_casefolded(0x2C67, 0x2C67).	% L&       LATIN CAPITAL LETTER H WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x2C69, 0x2C69).	% L&       LATIN CAPITAL LETTER K WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x2C6B, 0x2C6B).	% L&       LATIN CAPITAL LETTER Z WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0x2C6D, 0x2C70).	% L&   [4] LATIN CAPITAL LETTER ALPHA..LATIN CAPITAL LETTER TURNED ALPHA
unicode_changes_when_nfkc_casefolded(0x2C72, 0x2C72).	% L&       LATIN CAPITAL LETTER W WITH HOOK
unicode_changes_when_nfkc_casefolded(0x2C75, 0x2C75).	% L&       LATIN CAPITAL LETTER HALF H
unicode_changes_when_nfkc_casefolded(0x2C7C, 0x2C7D).	% Lm   [2] LATIN SUBSCRIPT SMALL LETTER J..MODIFIER LETTER CAPITAL V
unicode_changes_when_nfkc_casefolded(0x2C7E, 0x2C80).	% L&   [3] LATIN CAPITAL LETTER S WITH SWASH TAIL..COPTIC CAPITAL LETTER ALFA
unicode_changes_when_nfkc_casefolded(0x2C82, 0x2C82).	% L&       COPTIC CAPITAL LETTER VIDA
unicode_changes_when_nfkc_casefolded(0x2C84, 0x2C84).	% L&       COPTIC CAPITAL LETTER GAMMA
unicode_changes_when_nfkc_casefolded(0x2C86, 0x2C86).	% L&       COPTIC CAPITAL LETTER DALDA
unicode_changes_when_nfkc_casefolded(0x2C88, 0x2C88).	% L&       COPTIC CAPITAL LETTER EIE
unicode_changes_when_nfkc_casefolded(0x2C8A, 0x2C8A).	% L&       COPTIC CAPITAL LETTER SOU
unicode_changes_when_nfkc_casefolded(0x2C8C, 0x2C8C).	% L&       COPTIC CAPITAL LETTER ZATA
unicode_changes_when_nfkc_casefolded(0x2C8E, 0x2C8E).	% L&       COPTIC CAPITAL LETTER HATE
unicode_changes_when_nfkc_casefolded(0x2C90, 0x2C90).	% L&       COPTIC CAPITAL LETTER THETHE
unicode_changes_when_nfkc_casefolded(0x2C92, 0x2C92).	% L&       COPTIC CAPITAL LETTER IAUDA
unicode_changes_when_nfkc_casefolded(0x2C94, 0x2C94).	% L&       COPTIC CAPITAL LETTER KAPA
unicode_changes_when_nfkc_casefolded(0x2C96, 0x2C96).	% L&       COPTIC CAPITAL LETTER LAULA
unicode_changes_when_nfkc_casefolded(0x2C98, 0x2C98).	% L&       COPTIC CAPITAL LETTER MI
unicode_changes_when_nfkc_casefolded(0x2C9A, 0x2C9A).	% L&       COPTIC CAPITAL LETTER NI
unicode_changes_when_nfkc_casefolded(0x2C9C, 0x2C9C).	% L&       COPTIC CAPITAL LETTER KSI
unicode_changes_when_nfkc_casefolded(0x2C9E, 0x2C9E).	% L&       COPTIC CAPITAL LETTER O
unicode_changes_when_nfkc_casefolded(0x2CA0, 0x2CA0).	% L&       COPTIC CAPITAL LETTER PI
unicode_changes_when_nfkc_casefolded(0x2CA2, 0x2CA2).	% L&       COPTIC CAPITAL LETTER RO
unicode_changes_when_nfkc_casefolded(0x2CA4, 0x2CA4).	% L&       COPTIC CAPITAL LETTER SIMA
unicode_changes_when_nfkc_casefolded(0x2CA6, 0x2CA6).	% L&       COPTIC CAPITAL LETTER TAU
unicode_changes_when_nfkc_casefolded(0x2CA8, 0x2CA8).	% L&       COPTIC CAPITAL LETTER UA
unicode_changes_when_nfkc_casefolded(0x2CAA, 0x2CAA).	% L&       COPTIC CAPITAL LETTER FI
unicode_changes_when_nfkc_casefolded(0x2CAC, 0x2CAC).	% L&       COPTIC CAPITAL LETTER KHI
unicode_changes_when_nfkc_casefolded(0x2CAE, 0x2CAE).	% L&       COPTIC CAPITAL LETTER PSI
unicode_changes_when_nfkc_casefolded(0x2CB0, 0x2CB0).	% L&       COPTIC CAPITAL LETTER OOU
unicode_changes_when_nfkc_casefolded(0x2CB2, 0x2CB2).	% L&       COPTIC CAPITAL LETTER DIALECT-P ALEF
unicode_changes_when_nfkc_casefolded(0x2CB4, 0x2CB4).	% L&       COPTIC CAPITAL LETTER OLD COPTIC AIN
unicode_changes_when_nfkc_casefolded(0x2CB6, 0x2CB6).	% L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC EIE
unicode_changes_when_nfkc_casefolded(0x2CB8, 0x2CB8).	% L&       COPTIC CAPITAL LETTER DIALECT-P KAPA
unicode_changes_when_nfkc_casefolded(0x2CBA, 0x2CBA).	% L&       COPTIC CAPITAL LETTER DIALECT-P NI
unicode_changes_when_nfkc_casefolded(0x2CBC, 0x2CBC).	% L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC NI
unicode_changes_when_nfkc_casefolded(0x2CBE, 0x2CBE).	% L&       COPTIC CAPITAL LETTER OLD COPTIC OOU
unicode_changes_when_nfkc_casefolded(0x2CC0, 0x2CC0).	% L&       COPTIC CAPITAL LETTER SAMPI
unicode_changes_when_nfkc_casefolded(0x2CC2, 0x2CC2).	% L&       COPTIC CAPITAL LETTER CROSSED SHEI
unicode_changes_when_nfkc_casefolded(0x2CC4, 0x2CC4).	% L&       COPTIC CAPITAL LETTER OLD COPTIC SHEI
unicode_changes_when_nfkc_casefolded(0x2CC6, 0x2CC6).	% L&       COPTIC CAPITAL LETTER OLD COPTIC ESH
unicode_changes_when_nfkc_casefolded(0x2CC8, 0x2CC8).	% L&       COPTIC CAPITAL LETTER AKHMIMIC KHEI
unicode_changes_when_nfkc_casefolded(0x2CCA, 0x2CCA).	% L&       COPTIC CAPITAL LETTER DIALECT-P HORI
unicode_changes_when_nfkc_casefolded(0x2CCC, 0x2CCC).	% L&       COPTIC CAPITAL LETTER OLD COPTIC HORI
unicode_changes_when_nfkc_casefolded(0x2CCE, 0x2CCE).	% L&       COPTIC CAPITAL LETTER OLD COPTIC HA
unicode_changes_when_nfkc_casefolded(0x2CD0, 0x2CD0).	% L&       COPTIC CAPITAL LETTER L-SHAPED HA
unicode_changes_when_nfkc_casefolded(0x2CD2, 0x2CD2).	% L&       COPTIC CAPITAL LETTER OLD COPTIC HEI
unicode_changes_when_nfkc_casefolded(0x2CD4, 0x2CD4).	% L&       COPTIC CAPITAL LETTER OLD COPTIC HAT
unicode_changes_when_nfkc_casefolded(0x2CD6, 0x2CD6).	% L&       COPTIC CAPITAL LETTER OLD COPTIC GANGIA
unicode_changes_when_nfkc_casefolded(0x2CD8, 0x2CD8).	% L&       COPTIC CAPITAL LETTER OLD COPTIC DJA
unicode_changes_when_nfkc_casefolded(0x2CDA, 0x2CDA).	% L&       COPTIC CAPITAL LETTER OLD COPTIC SHIMA
unicode_changes_when_nfkc_casefolded(0x2CDC, 0x2CDC).	% L&       COPTIC CAPITAL LETTER OLD NUBIAN SHIMA
unicode_changes_when_nfkc_casefolded(0x2CDE, 0x2CDE).	% L&       COPTIC CAPITAL LETTER OLD NUBIAN NGI
unicode_changes_when_nfkc_casefolded(0x2CE0, 0x2CE0).	% L&       COPTIC CAPITAL LETTER OLD NUBIAN NYI
unicode_changes_when_nfkc_casefolded(0x2CE2, 0x2CE2).	% L&       COPTIC CAPITAL LETTER OLD NUBIAN WAU
unicode_changes_when_nfkc_casefolded(0x2CEB, 0x2CEB).	% L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI
unicode_changes_when_nfkc_casefolded(0x2CED, 0x2CED).	% L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC GANGIA
unicode_changes_when_nfkc_casefolded(0x2CF2, 0x2CF2).	% L&       COPTIC CAPITAL LETTER BOHAIRIC KHEI
unicode_changes_when_nfkc_casefolded(0x2D6F, 0x2D6F).	% Lm       TIFINAGH MODIFIER LETTER LABIALIZATION MARK
unicode_changes_when_nfkc_casefolded(0x2E9F, 0x2E9F).	% So       CJK RADICAL MOTHER
unicode_changes_when_nfkc_casefolded(0x2EF3, 0x2EF3).	% So       CJK RADICAL C-SIMPLIFIED TURTLE
unicode_changes_when_nfkc_casefolded(0x2F00, 0x2FD5).	% So [214] KANGXI RADICAL ONE..KANGXI RADICAL FLUTE
unicode_changes_when_nfkc_casefolded(0x3000, 0x3000).	% Zs       IDEOGRAPHIC SPACE
unicode_changes_when_nfkc_casefolded(0x3036, 0x3036).	% So       CIRCLED POSTAL MARK
unicode_changes_when_nfkc_casefolded(0x3038, 0x303A).	% Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
unicode_changes_when_nfkc_casefolded(0x309B, 0x309C).	% Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_changes_when_nfkc_casefolded(0x309F, 0x309F).	% Lo       HIRAGANA DIGRAPH YORI
unicode_changes_when_nfkc_casefolded(0x30FF, 0x30FF).	% Lo       KATAKANA DIGRAPH KOTO
unicode_changes_when_nfkc_casefolded(0x3131, 0x318E).	% Lo  [94] HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE
unicode_changes_when_nfkc_casefolded(0x3192, 0x3195).	% No   [4] IDEOGRAPHIC ANNOTATION ONE MARK..IDEOGRAPHIC ANNOTATION FOUR MARK
unicode_changes_when_nfkc_casefolded(0x3196, 0x319F).	% So  [10] IDEOGRAPHIC ANNOTATION TOP MARK..IDEOGRAPHIC ANNOTATION MAN MARK
unicode_changes_when_nfkc_casefolded(0x3200, 0x321E).	% So  [31] PARENTHESIZED HANGUL KIYEOK..PARENTHESIZED KOREAN CHARACTER O HU
unicode_changes_when_nfkc_casefolded(0x3220, 0x3229).	% No  [10] PARENTHESIZED IDEOGRAPH ONE..PARENTHESIZED IDEOGRAPH TEN
unicode_changes_when_nfkc_casefolded(0x322A, 0x3247).	% So  [30] PARENTHESIZED IDEOGRAPH MOON..CIRCLED IDEOGRAPH KOTO
unicode_changes_when_nfkc_casefolded(0x3250, 0x3250).	% So       PARTNERSHIP SIGN
unicode_changes_when_nfkc_casefolded(0x3251, 0x325F).	% No  [15] CIRCLED NUMBER TWENTY ONE..CIRCLED NUMBER THIRTY FIVE
unicode_changes_when_nfkc_casefolded(0x3260, 0x327E).	% So  [31] CIRCLED HANGUL KIYEOK..CIRCLED HANGUL IEUNG U
unicode_changes_when_nfkc_casefolded(0x3280, 0x3289).	% No  [10] CIRCLED IDEOGRAPH ONE..CIRCLED IDEOGRAPH TEN
unicode_changes_when_nfkc_casefolded(0x328A, 0x32B0).	% So  [39] CIRCLED IDEOGRAPH MOON..CIRCLED IDEOGRAPH NIGHT
unicode_changes_when_nfkc_casefolded(0x32B1, 0x32BF).	% No  [15] CIRCLED NUMBER THIRTY SIX..CIRCLED NUMBER FIFTY
unicode_changes_when_nfkc_casefolded(0x32C0, 0x32FE).	% So  [63] IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY..CIRCLED KATAKANA WO
unicode_changes_when_nfkc_casefolded(0x3300, 0x33FF).	% So [256] SQUARE APAATO..SQUARE GAL
unicode_changes_when_nfkc_casefolded(0xA640, 0xA640).	% L&       CYRILLIC CAPITAL LETTER ZEMLYA
unicode_changes_when_nfkc_casefolded(0xA642, 0xA642).	% L&       CYRILLIC CAPITAL LETTER DZELO
unicode_changes_when_nfkc_casefolded(0xA644, 0xA644).	% L&       CYRILLIC CAPITAL LETTER REVERSED DZE
unicode_changes_when_nfkc_casefolded(0xA646, 0xA646).	% L&       CYRILLIC CAPITAL LETTER IOTA
unicode_changes_when_nfkc_casefolded(0xA648, 0xA648).	% L&       CYRILLIC CAPITAL LETTER DJERV
unicode_changes_when_nfkc_casefolded(0xA64A, 0xA64A).	% L&       CYRILLIC CAPITAL LETTER MONOGRAPH UK
unicode_changes_when_nfkc_casefolded(0xA64C, 0xA64C).	% L&       CYRILLIC CAPITAL LETTER BROAD OMEGA
unicode_changes_when_nfkc_casefolded(0xA64E, 0xA64E).	% L&       CYRILLIC CAPITAL LETTER NEUTRAL YER
unicode_changes_when_nfkc_casefolded(0xA650, 0xA650).	% L&       CYRILLIC CAPITAL LETTER YERU WITH BACK YER
unicode_changes_when_nfkc_casefolded(0xA652, 0xA652).	% L&       CYRILLIC CAPITAL LETTER IOTIFIED YAT
unicode_changes_when_nfkc_casefolded(0xA654, 0xA654).	% L&       CYRILLIC CAPITAL LETTER REVERSED YU
unicode_changes_when_nfkc_casefolded(0xA656, 0xA656).	% L&       CYRILLIC CAPITAL LETTER IOTIFIED A
unicode_changes_when_nfkc_casefolded(0xA658, 0xA658).	% L&       CYRILLIC CAPITAL LETTER CLOSED LITTLE YUS
unicode_changes_when_nfkc_casefolded(0xA65A, 0xA65A).	% L&       CYRILLIC CAPITAL LETTER BLENDED YUS
unicode_changes_when_nfkc_casefolded(0xA65C, 0xA65C).	% L&       CYRILLIC CAPITAL LETTER IOTIFIED CLOSED LITTLE YUS
unicode_changes_when_nfkc_casefolded(0xA65E, 0xA65E).	% L&       CYRILLIC CAPITAL LETTER YN
unicode_changes_when_nfkc_casefolded(0xA660, 0xA660).	% L&       CYRILLIC CAPITAL LETTER REVERSED TSE
unicode_changes_when_nfkc_casefolded(0xA662, 0xA662).	% L&       CYRILLIC CAPITAL LETTER SOFT DE
unicode_changes_when_nfkc_casefolded(0xA664, 0xA664).	% L&       CYRILLIC CAPITAL LETTER SOFT EL
unicode_changes_when_nfkc_casefolded(0xA666, 0xA666).	% L&       CYRILLIC CAPITAL LETTER SOFT EM
unicode_changes_when_nfkc_casefolded(0xA668, 0xA668).	% L&       CYRILLIC CAPITAL LETTER MONOCULAR O
unicode_changes_when_nfkc_casefolded(0xA66A, 0xA66A).	% L&       CYRILLIC CAPITAL LETTER BINOCULAR O
unicode_changes_when_nfkc_casefolded(0xA66C, 0xA66C).	% L&       CYRILLIC CAPITAL LETTER DOUBLE MONOCULAR O
unicode_changes_when_nfkc_casefolded(0xA680, 0xA680).	% L&       CYRILLIC CAPITAL LETTER DWE
unicode_changes_when_nfkc_casefolded(0xA682, 0xA682).	% L&       CYRILLIC CAPITAL LETTER DZWE
unicode_changes_when_nfkc_casefolded(0xA684, 0xA684).	% L&       CYRILLIC CAPITAL LETTER ZHWE
unicode_changes_when_nfkc_casefolded(0xA686, 0xA686).	% L&       CYRILLIC CAPITAL LETTER CCHE
unicode_changes_when_nfkc_casefolded(0xA688, 0xA688).	% L&       CYRILLIC CAPITAL LETTER DZZE
unicode_changes_when_nfkc_casefolded(0xA68A, 0xA68A).	% L&       CYRILLIC CAPITAL LETTER TE WITH MIDDLE HOOK
unicode_changes_when_nfkc_casefolded(0xA68C, 0xA68C).	% L&       CYRILLIC CAPITAL LETTER TWE
unicode_changes_when_nfkc_casefolded(0xA68E, 0xA68E).	% L&       CYRILLIC CAPITAL LETTER TSWE
unicode_changes_when_nfkc_casefolded(0xA690, 0xA690).	% L&       CYRILLIC CAPITAL LETTER TSSE
unicode_changes_when_nfkc_casefolded(0xA692, 0xA692).	% L&       CYRILLIC CAPITAL LETTER TCHE
unicode_changes_when_nfkc_casefolded(0xA694, 0xA694).	% L&       CYRILLIC CAPITAL LETTER HWE
unicode_changes_when_nfkc_casefolded(0xA696, 0xA696).	% L&       CYRILLIC CAPITAL LETTER SHWE
unicode_changes_when_nfkc_casefolded(0xA722, 0xA722).	% L&       LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF
unicode_changes_when_nfkc_casefolded(0xA724, 0xA724).	% L&       LATIN CAPITAL LETTER EGYPTOLOGICAL AIN
unicode_changes_when_nfkc_casefolded(0xA726, 0xA726).	% L&       LATIN CAPITAL LETTER HENG
unicode_changes_when_nfkc_casefolded(0xA728, 0xA728).	% L&       LATIN CAPITAL LETTER TZ
unicode_changes_when_nfkc_casefolded(0xA72A, 0xA72A).	% L&       LATIN CAPITAL LETTER TRESILLO
unicode_changes_when_nfkc_casefolded(0xA72C, 0xA72C).	% L&       LATIN CAPITAL LETTER CUATRILLO
unicode_changes_when_nfkc_casefolded(0xA72E, 0xA72E).	% L&       LATIN CAPITAL LETTER CUATRILLO WITH COMMA
unicode_changes_when_nfkc_casefolded(0xA732, 0xA732).	% L&       LATIN CAPITAL LETTER AA
unicode_changes_when_nfkc_casefolded(0xA734, 0xA734).	% L&       LATIN CAPITAL LETTER AO
unicode_changes_when_nfkc_casefolded(0xA736, 0xA736).	% L&       LATIN CAPITAL LETTER AU
unicode_changes_when_nfkc_casefolded(0xA738, 0xA738).	% L&       LATIN CAPITAL LETTER AV
unicode_changes_when_nfkc_casefolded(0xA73A, 0xA73A).	% L&       LATIN CAPITAL LETTER AV WITH HORIZONTAL BAR
unicode_changes_when_nfkc_casefolded(0xA73C, 0xA73C).	% L&       LATIN CAPITAL LETTER AY
unicode_changes_when_nfkc_casefolded(0xA73E, 0xA73E).	% L&       LATIN CAPITAL LETTER REVERSED C WITH DOT
unicode_changes_when_nfkc_casefolded(0xA740, 0xA740).	% L&       LATIN CAPITAL LETTER K WITH STROKE
unicode_changes_when_nfkc_casefolded(0xA742, 0xA742).	% L&       LATIN CAPITAL LETTER K WITH DIAGONAL STROKE
unicode_changes_when_nfkc_casefolded(0xA744, 0xA744).	% L&       LATIN CAPITAL LETTER K WITH STROKE AND DIAGONAL STROKE
unicode_changes_when_nfkc_casefolded(0xA746, 0xA746).	% L&       LATIN CAPITAL LETTER BROKEN L
unicode_changes_when_nfkc_casefolded(0xA748, 0xA748).	% L&       LATIN CAPITAL LETTER L WITH HIGH STROKE
unicode_changes_when_nfkc_casefolded(0xA74A, 0xA74A).	% L&       LATIN CAPITAL LETTER O WITH LONG STROKE OVERLAY
unicode_changes_when_nfkc_casefolded(0xA74C, 0xA74C).	% L&       LATIN CAPITAL LETTER O WITH LOOP
unicode_changes_when_nfkc_casefolded(0xA74E, 0xA74E).	% L&       LATIN CAPITAL LETTER OO
unicode_changes_when_nfkc_casefolded(0xA750, 0xA750).	% L&       LATIN CAPITAL LETTER P WITH STROKE THROUGH DESCENDER
unicode_changes_when_nfkc_casefolded(0xA752, 0xA752).	% L&       LATIN CAPITAL LETTER P WITH FLOURISH
unicode_changes_when_nfkc_casefolded(0xA754, 0xA754).	% L&       LATIN CAPITAL LETTER P WITH SQUIRREL TAIL
unicode_changes_when_nfkc_casefolded(0xA756, 0xA756).	% L&       LATIN CAPITAL LETTER Q WITH STROKE THROUGH DESCENDER
unicode_changes_when_nfkc_casefolded(0xA758, 0xA758).	% L&       LATIN CAPITAL LETTER Q WITH DIAGONAL STROKE
unicode_changes_when_nfkc_casefolded(0xA75A, 0xA75A).	% L&       LATIN CAPITAL LETTER R ROTUNDA
unicode_changes_when_nfkc_casefolded(0xA75C, 0xA75C).	% L&       LATIN CAPITAL LETTER RUM ROTUNDA
unicode_changes_when_nfkc_casefolded(0xA75E, 0xA75E).	% L&       LATIN CAPITAL LETTER V WITH DIAGONAL STROKE
unicode_changes_when_nfkc_casefolded(0xA760, 0xA760).	% L&       LATIN CAPITAL LETTER VY
unicode_changes_when_nfkc_casefolded(0xA762, 0xA762).	% L&       LATIN CAPITAL LETTER VISIGOTHIC Z
unicode_changes_when_nfkc_casefolded(0xA764, 0xA764).	% L&       LATIN CAPITAL LETTER THORN WITH STROKE
unicode_changes_when_nfkc_casefolded(0xA766, 0xA766).	% L&       LATIN CAPITAL LETTER THORN WITH STROKE THROUGH DESCENDER
unicode_changes_when_nfkc_casefolded(0xA768, 0xA768).	% L&       LATIN CAPITAL LETTER VEND
unicode_changes_when_nfkc_casefolded(0xA76A, 0xA76A).	% L&       LATIN CAPITAL LETTER ET
unicode_changes_when_nfkc_casefolded(0xA76C, 0xA76C).	% L&       LATIN CAPITAL LETTER IS
unicode_changes_when_nfkc_casefolded(0xA76E, 0xA76E).	% L&       LATIN CAPITAL LETTER CON
unicode_changes_when_nfkc_casefolded(0xA770, 0xA770).	% Lm       MODIFIER LETTER US
unicode_changes_when_nfkc_casefolded(0xA779, 0xA779).	% L&       LATIN CAPITAL LETTER INSULAR D
unicode_changes_when_nfkc_casefolded(0xA77B, 0xA77B).	% L&       LATIN CAPITAL LETTER INSULAR F
unicode_changes_when_nfkc_casefolded(0xA77D, 0xA77E).	% L&   [2] LATIN CAPITAL LETTER INSULAR G..LATIN CAPITAL LETTER TURNED INSULAR G
unicode_changes_when_nfkc_casefolded(0xA780, 0xA780).	% L&       LATIN CAPITAL LETTER TURNED L
unicode_changes_when_nfkc_casefolded(0xA782, 0xA782).	% L&       LATIN CAPITAL LETTER INSULAR R
unicode_changes_when_nfkc_casefolded(0xA784, 0xA784).	% L&       LATIN CAPITAL LETTER INSULAR S
unicode_changes_when_nfkc_casefolded(0xA786, 0xA786).	% L&       LATIN CAPITAL LETTER INSULAR T
unicode_changes_when_nfkc_casefolded(0xA78B, 0xA78B).	% L&       LATIN CAPITAL LETTER SALTILLO
unicode_changes_when_nfkc_casefolded(0xA78D, 0xA78D).	% L&       LATIN CAPITAL LETTER TURNED H
unicode_changes_when_nfkc_casefolded(0xA790, 0xA790).	% L&       LATIN CAPITAL LETTER N WITH DESCENDER
unicode_changes_when_nfkc_casefolded(0xA792, 0xA792).	% L&       LATIN CAPITAL LETTER C WITH BAR
unicode_changes_when_nfkc_casefolded(0xA7A0, 0xA7A0).	% L&       LATIN CAPITAL LETTER G WITH OBLIQUE STROKE
unicode_changes_when_nfkc_casefolded(0xA7A2, 0xA7A2).	% L&       LATIN CAPITAL LETTER K WITH OBLIQUE STROKE
unicode_changes_when_nfkc_casefolded(0xA7A4, 0xA7A4).	% L&       LATIN CAPITAL LETTER N WITH OBLIQUE STROKE
unicode_changes_when_nfkc_casefolded(0xA7A6, 0xA7A6).	% L&       LATIN CAPITAL LETTER R WITH OBLIQUE STROKE
unicode_changes_when_nfkc_casefolded(0xA7A8, 0xA7A8).	% L&       LATIN CAPITAL LETTER S WITH OBLIQUE STROKE
unicode_changes_when_nfkc_casefolded(0xA7AA, 0xA7AA).	% L&       LATIN CAPITAL LETTER H WITH HOOK
unicode_changes_when_nfkc_casefolded(0xA7F8, 0xA7F9).	% Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_changes_when_nfkc_casefolded(0xF900, 0xFA0D).	% Lo [270] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA0D
unicode_changes_when_nfkc_casefolded(0xFA10, 0xFA10).	% Lo       CJK COMPATIBILITY IDEOGRAPH-FA10
unicode_changes_when_nfkc_casefolded(0xFA12, 0xFA12).	% Lo       CJK COMPATIBILITY IDEOGRAPH-FA12
unicode_changes_when_nfkc_casefolded(0xFA15, 0xFA1E).	% Lo  [10] CJK COMPATIBILITY IDEOGRAPH-FA15..CJK COMPATIBILITY IDEOGRAPH-FA1E
unicode_changes_when_nfkc_casefolded(0xFA20, 0xFA20).	% Lo       CJK COMPATIBILITY IDEOGRAPH-FA20
unicode_changes_when_nfkc_casefolded(0xFA22, 0xFA22).	% Lo       CJK COMPATIBILITY IDEOGRAPH-FA22
unicode_changes_when_nfkc_casefolded(0xFA25, 0xFA26).	% Lo   [2] CJK COMPATIBILITY IDEOGRAPH-FA25..CJK COMPATIBILITY IDEOGRAPH-FA26
unicode_changes_when_nfkc_casefolded(0xFA2A, 0xFA6D).	% Lo  [68] CJK COMPATIBILITY IDEOGRAPH-FA2A..CJK COMPATIBILITY IDEOGRAPH-FA6D
unicode_changes_when_nfkc_casefolded(0xFA70, 0xFAD9).	% Lo [106] CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9
unicode_changes_when_nfkc_casefolded(0xFB00, 0xFB06).	% L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_changes_when_nfkc_casefolded(0xFB13, 0xFB17).	% L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_changes_when_nfkc_casefolded(0xFB1D, 0xFB1D).	% Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_changes_when_nfkc_casefolded(0xFB1F, 0xFB28).	% Lo  [10] HEBREW LIGATURE YIDDISH YOD YOD PATAH..HEBREW LETTER WIDE TAV
unicode_changes_when_nfkc_casefolded(0xFB29, 0xFB29).	% Sm       HEBREW LETTER ALTERNATIVE PLUS SIGN
unicode_changes_when_nfkc_casefolded(0xFB2A, 0xFB36).	% Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_changes_when_nfkc_casefolded(0xFB38, 0xFB3C).	% Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_changes_when_nfkc_casefolded(0xFB3E, 0xFB3E).	% Lo       HEBREW LETTER MEM WITH DAGESH
unicode_changes_when_nfkc_casefolded(0xFB40, 0xFB41).	% Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_changes_when_nfkc_casefolded(0xFB43, 0xFB44).	% Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_changes_when_nfkc_casefolded(0xFB46, 0xFBB1).	% Lo [108] HEBREW LETTER TSADI WITH DAGESH..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
unicode_changes_when_nfkc_casefolded(0xFBD3, 0xFD3D).	% Lo [363] ARABIC LETTER NG ISOLATED FORM..ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
unicode_changes_when_nfkc_casefolded(0xFD50, 0xFD8F).	% Lo  [64] ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
unicode_changes_when_nfkc_casefolded(0xFD92, 0xFDC7).	% Lo  [54] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
unicode_changes_when_nfkc_casefolded(0xFDF0, 0xFDFB).	% Lo  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
unicode_changes_when_nfkc_casefolded(0xFDFC, 0xFDFC).	% Sc       RIAL SIGN
unicode_changes_when_nfkc_casefolded(0xFE00, 0xFE0F).	% Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
unicode_changes_when_nfkc_casefolded(0xFE10, 0xFE16).	% Po   [7] PRESENTATION FORM FOR VERTICAL COMMA..PRESENTATION FORM FOR VERTICAL QUESTION MARK
unicode_changes_when_nfkc_casefolded(0xFE17, 0xFE17).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE LENTICULAR BRACKET
unicode_changes_when_nfkc_casefolded(0xFE18, 0xFE18).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET
unicode_changes_when_nfkc_casefolded(0xFE19, 0xFE19).	% Po       PRESENTATION FORM FOR VERTICAL HORIZONTAL ELLIPSIS
unicode_changes_when_nfkc_casefolded(0xFE30, 0xFE30).	% Po       PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
unicode_changes_when_nfkc_casefolded(0xFE31, 0xFE32).	% Pd   [2] PRESENTATION FORM FOR VERTICAL EM DASH..PRESENTATION FORM FOR VERTICAL EN DASH
unicode_changes_when_nfkc_casefolded(0xFE33, 0xFE34).	% Pc   [2] PRESENTATION FORM FOR VERTICAL LOW LINE..PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
unicode_changes_when_nfkc_casefolded(0xFE35, 0xFE35).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
unicode_changes_when_nfkc_casefolded(0xFE36, 0xFE36).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
unicode_changes_when_nfkc_casefolded(0xFE37, 0xFE37).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
unicode_changes_when_nfkc_casefolded(0xFE38, 0xFE38).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
unicode_changes_when_nfkc_casefolded(0xFE39, 0xFE39).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
unicode_changes_when_nfkc_casefolded(0xFE3A, 0xFE3A).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
unicode_changes_when_nfkc_casefolded(0xFE3B, 0xFE3B).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
unicode_changes_when_nfkc_casefolded(0xFE3C, 0xFE3C).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
unicode_changes_when_nfkc_casefolded(0xFE3D, 0xFE3D).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
unicode_changes_when_nfkc_casefolded(0xFE3E, 0xFE3E).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
unicode_changes_when_nfkc_casefolded(0xFE3F, 0xFE3F).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
unicode_changes_when_nfkc_casefolded(0xFE40, 0xFE40).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
unicode_changes_when_nfkc_casefolded(0xFE41, 0xFE41).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
unicode_changes_when_nfkc_casefolded(0xFE42, 0xFE42).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
unicode_changes_when_nfkc_casefolded(0xFE43, 0xFE43).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
unicode_changes_when_nfkc_casefolded(0xFE44, 0xFE44).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
unicode_changes_when_nfkc_casefolded(0xFE47, 0xFE47).	% Ps       PRESENTATION FORM FOR VERTICAL LEFT SQUARE BRACKET
unicode_changes_when_nfkc_casefolded(0xFE48, 0xFE48).	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT SQUARE BRACKET
unicode_changes_when_nfkc_casefolded(0xFE49, 0xFE4C).	% Po   [4] DASHED OVERLINE..DOUBLE WAVY OVERLINE
unicode_changes_when_nfkc_casefolded(0xFE4D, 0xFE4F).	% Pc   [3] DASHED LOW LINE..WAVY LOW LINE
unicode_changes_when_nfkc_casefolded(0xFE50, 0xFE52).	% Po   [3] SMALL COMMA..SMALL FULL STOP
unicode_changes_when_nfkc_casefolded(0xFE54, 0xFE57).	% Po   [4] SMALL SEMICOLON..SMALL EXCLAMATION MARK
unicode_changes_when_nfkc_casefolded(0xFE58, 0xFE58).	% Pd       SMALL EM DASH
unicode_changes_when_nfkc_casefolded(0xFE59, 0xFE59).	% Ps       SMALL LEFT PARENTHESIS
unicode_changes_when_nfkc_casefolded(0xFE5A, 0xFE5A).	% Pe       SMALL RIGHT PARENTHESIS
unicode_changes_when_nfkc_casefolded(0xFE5B, 0xFE5B).	% Ps       SMALL LEFT CURLY BRACKET
unicode_changes_when_nfkc_casefolded(0xFE5C, 0xFE5C).	% Pe       SMALL RIGHT CURLY BRACKET
unicode_changes_when_nfkc_casefolded(0xFE5D, 0xFE5D).	% Ps       SMALL LEFT TORTOISE SHELL BRACKET
unicode_changes_when_nfkc_casefolded(0xFE5E, 0xFE5E).	% Pe       SMALL RIGHT TORTOISE SHELL BRACKET
unicode_changes_when_nfkc_casefolded(0xFE5F, 0xFE61).	% Po   [3] SMALL NUMBER SIGN..SMALL ASTERISK
unicode_changes_when_nfkc_casefolded(0xFE62, 0xFE62).	% Sm       SMALL PLUS SIGN
unicode_changes_when_nfkc_casefolded(0xFE63, 0xFE63).	% Pd       SMALL HYPHEN-MINUS
unicode_changes_when_nfkc_casefolded(0xFE64, 0xFE66).	% Sm   [3] SMALL LESS-THAN SIGN..SMALL EQUALS SIGN
unicode_changes_when_nfkc_casefolded(0xFE68, 0xFE68).	% Po       SMALL REVERSE SOLIDUS
unicode_changes_when_nfkc_casefolded(0xFE69, 0xFE69).	% Sc       SMALL DOLLAR SIGN
unicode_changes_when_nfkc_casefolded(0xFE6A, 0xFE6B).	% Po   [2] SMALL PERCENT SIGN..SMALL COMMERCIAL AT
unicode_changes_when_nfkc_casefolded(0xFE70, 0xFE72).	% Lo   [3] ARABIC FATHATAN ISOLATED FORM..ARABIC DAMMATAN ISOLATED FORM
unicode_changes_when_nfkc_casefolded(0xFE74, 0xFE74).	% Lo       ARABIC KASRATAN ISOLATED FORM
unicode_changes_when_nfkc_casefolded(0xFE76, 0xFEFC).	% Lo [135] ARABIC FATHA ISOLATED FORM..ARABIC LIGATURE LAM WITH ALEF FINAL FORM
unicode_changes_when_nfkc_casefolded(0xFEFF, 0xFEFF).	% Cf       ZERO WIDTH NO-BREAK SPACE
unicode_changes_when_nfkc_casefolded(0xFF01, 0xFF03).	% Po   [3] FULLWIDTH EXCLAMATION MARK..FULLWIDTH NUMBER SIGN
unicode_changes_when_nfkc_casefolded(0xFF04, 0xFF04).	% Sc       FULLWIDTH DOLLAR SIGN
unicode_changes_when_nfkc_casefolded(0xFF05, 0xFF07).	% Po   [3] FULLWIDTH PERCENT SIGN..FULLWIDTH APOSTROPHE
unicode_changes_when_nfkc_casefolded(0xFF08, 0xFF08).	% Ps       FULLWIDTH LEFT PARENTHESIS
unicode_changes_when_nfkc_casefolded(0xFF09, 0xFF09).	% Pe       FULLWIDTH RIGHT PARENTHESIS
unicode_changes_when_nfkc_casefolded(0xFF0A, 0xFF0A).	% Po       FULLWIDTH ASTERISK
unicode_changes_when_nfkc_casefolded(0xFF0B, 0xFF0B).	% Sm       FULLWIDTH PLUS SIGN
unicode_changes_when_nfkc_casefolded(0xFF0C, 0xFF0C).	% Po       FULLWIDTH COMMA
unicode_changes_when_nfkc_casefolded(0xFF0D, 0xFF0D).	% Pd       FULLWIDTH HYPHEN-MINUS
unicode_changes_when_nfkc_casefolded(0xFF0E, 0xFF0F).	% Po   [2] FULLWIDTH FULL STOP..FULLWIDTH SOLIDUS
unicode_changes_when_nfkc_casefolded(0xFF10, 0xFF19).	% Nd  [10] FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
unicode_changes_when_nfkc_casefolded(0xFF1A, 0xFF1B).	% Po   [2] FULLWIDTH COLON..FULLWIDTH SEMICOLON
unicode_changes_when_nfkc_casefolded(0xFF1C, 0xFF1E).	% Sm   [3] FULLWIDTH LESS-THAN SIGN..FULLWIDTH GREATER-THAN SIGN
unicode_changes_when_nfkc_casefolded(0xFF1F, 0xFF20).	% Po   [2] FULLWIDTH QUESTION MARK..FULLWIDTH COMMERCIAL AT
unicode_changes_when_nfkc_casefolded(0xFF21, 0xFF3A).	% L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_changes_when_nfkc_casefolded(0xFF3B, 0xFF3B).	% Ps       FULLWIDTH LEFT SQUARE BRACKET
unicode_changes_when_nfkc_casefolded(0xFF3C, 0xFF3C).	% Po       FULLWIDTH REVERSE SOLIDUS
unicode_changes_when_nfkc_casefolded(0xFF3D, 0xFF3D).	% Pe       FULLWIDTH RIGHT SQUARE BRACKET
unicode_changes_when_nfkc_casefolded(0xFF3E, 0xFF3E).	% Sk       FULLWIDTH CIRCUMFLEX ACCENT
unicode_changes_when_nfkc_casefolded(0xFF3F, 0xFF3F).	% Pc       FULLWIDTH LOW LINE
unicode_changes_when_nfkc_casefolded(0xFF40, 0xFF40).	% Sk       FULLWIDTH GRAVE ACCENT
unicode_changes_when_nfkc_casefolded(0xFF41, 0xFF5A).	% L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_changes_when_nfkc_casefolded(0xFF5B, 0xFF5B).	% Ps       FULLWIDTH LEFT CURLY BRACKET
unicode_changes_when_nfkc_casefolded(0xFF5C, 0xFF5C).	% Sm       FULLWIDTH VERTICAL LINE
unicode_changes_when_nfkc_casefolded(0xFF5D, 0xFF5D).	% Pe       FULLWIDTH RIGHT CURLY BRACKET
unicode_changes_when_nfkc_casefolded(0xFF5E, 0xFF5E).	% Sm       FULLWIDTH TILDE
unicode_changes_when_nfkc_casefolded(0xFF5F, 0xFF5F).	% Ps       FULLWIDTH LEFT WHITE PARENTHESIS
unicode_changes_when_nfkc_casefolded(0xFF60, 0xFF60).	% Pe       FULLWIDTH RIGHT WHITE PARENTHESIS
unicode_changes_when_nfkc_casefolded(0xFF61, 0xFF61).	% Po       HALFWIDTH IDEOGRAPHIC FULL STOP
unicode_changes_when_nfkc_casefolded(0xFF62, 0xFF62).	% Ps       HALFWIDTH LEFT CORNER BRACKET
unicode_changes_when_nfkc_casefolded(0xFF63, 0xFF63).	% Pe       HALFWIDTH RIGHT CORNER BRACKET
unicode_changes_when_nfkc_casefolded(0xFF64, 0xFF65).	% Po   [2] HALFWIDTH IDEOGRAPHIC COMMA..HALFWIDTH KATAKANA MIDDLE DOT
unicode_changes_when_nfkc_casefolded(0xFF66, 0xFF6F).	% Lo  [10] HALFWIDTH KATAKANA LETTER WO..HALFWIDTH KATAKANA LETTER SMALL TU
unicode_changes_when_nfkc_casefolded(0xFF70, 0xFF70).	% Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_changes_when_nfkc_casefolded(0xFF71, 0xFF9D).	% Lo  [45] HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N
unicode_changes_when_nfkc_casefolded(0xFF9E, 0xFF9F).	% Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
unicode_changes_when_nfkc_casefolded(0xFFA0, 0xFFBE).	% Lo  [31] HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH
unicode_changes_when_nfkc_casefolded(0xFFC2, 0xFFC7).	% Lo   [6] HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E
unicode_changes_when_nfkc_casefolded(0xFFCA, 0xFFCF).	% Lo   [6] HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE
unicode_changes_when_nfkc_casefolded(0xFFD2, 0xFFD7).	% Lo   [6] HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU
unicode_changes_when_nfkc_casefolded(0xFFDA, 0xFFDC).	% Lo   [3] HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I
unicode_changes_when_nfkc_casefolded(0xFFE0, 0xFFE1).	% Sc   [2] FULLWIDTH CENT SIGN..FULLWIDTH POUND SIGN
unicode_changes_when_nfkc_casefolded(0xFFE2, 0xFFE2).	% Sm       FULLWIDTH NOT SIGN
unicode_changes_when_nfkc_casefolded(0xFFE3, 0xFFE3).	% Sk       FULLWIDTH MACRON
unicode_changes_when_nfkc_casefolded(0xFFE4, 0xFFE4).	% So       FULLWIDTH BROKEN BAR
unicode_changes_when_nfkc_casefolded(0xFFE5, 0xFFE6).	% Sc   [2] FULLWIDTH YEN SIGN..FULLWIDTH WON SIGN
unicode_changes_when_nfkc_casefolded(0xFFE8, 0xFFE8).	% So       HALFWIDTH FORMS LIGHT VERTICAL
unicode_changes_when_nfkc_casefolded(0xFFE9, 0xFFEC).	% Sm   [4] HALFWIDTH LEFTWARDS ARROW..HALFWIDTH DOWNWARDS ARROW
unicode_changes_when_nfkc_casefolded(0xFFED, 0xFFEE).	% So   [2] HALFWIDTH BLACK SQUARE..HALFWIDTH WHITE CIRCLE
unicode_changes_when_nfkc_casefolded(0xFFF0, 0xFFF8).	% Cn   [9] <reserved-FFF0>..<reserved-FFF8>
unicode_changes_when_nfkc_casefolded(0x10400, 0x10427).	% L&  [40] DESERET CAPITAL LETTER LONG I..DESERET CAPITAL LETTER EW
unicode_changes_when_nfkc_casefolded(0x1D15E, 0x1D164).	% So   [7] MUSICAL SYMBOL HALF NOTE..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
unicode_changes_when_nfkc_casefolded(0x1D173, 0x1D17A).	% Cf   [8] MUSICAL SYMBOL BEGIN BEAM..MUSICAL SYMBOL END PHRASE
unicode_changes_when_nfkc_casefolded(0x1D1BB, 0x1D1C0).	% So   [6] MUSICAL SYMBOL MINIMA..MUSICAL SYMBOL FUSA BLACK
unicode_changes_when_nfkc_casefolded(0x1D400, 0x1D454).	% L&  [85] MATHEMATICAL BOLD CAPITAL A..MATHEMATICAL ITALIC SMALL G
unicode_changes_when_nfkc_casefolded(0x1D456, 0x1D49C).	% L&  [71] MATHEMATICAL ITALIC SMALL I..MATHEMATICAL SCRIPT CAPITAL A
unicode_changes_when_nfkc_casefolded(0x1D49E, 0x1D49F).	% L&   [2] MATHEMATICAL SCRIPT CAPITAL C..MATHEMATICAL SCRIPT CAPITAL D
unicode_changes_when_nfkc_casefolded(0x1D4A2, 0x1D4A2).	% L&       MATHEMATICAL SCRIPT CAPITAL G
unicode_changes_when_nfkc_casefolded(0x1D4A5, 0x1D4A6).	% L&   [2] MATHEMATICAL SCRIPT CAPITAL J..MATHEMATICAL SCRIPT CAPITAL K
unicode_changes_when_nfkc_casefolded(0x1D4A9, 0x1D4AC).	% L&   [4] MATHEMATICAL SCRIPT CAPITAL N..MATHEMATICAL SCRIPT CAPITAL Q
unicode_changes_when_nfkc_casefolded(0x1D4AE, 0x1D4B9).	% L&  [12] MATHEMATICAL SCRIPT CAPITAL S..MATHEMATICAL SCRIPT SMALL D
unicode_changes_when_nfkc_casefolded(0x1D4BB, 0x1D4BB).	% L&       MATHEMATICAL SCRIPT SMALL F
unicode_changes_when_nfkc_casefolded(0x1D4BD, 0x1D4C3).	% L&   [7] MATHEMATICAL SCRIPT SMALL H..MATHEMATICAL SCRIPT SMALL N
unicode_changes_when_nfkc_casefolded(0x1D4C5, 0x1D505).	% L&  [65] MATHEMATICAL SCRIPT SMALL P..MATHEMATICAL FRAKTUR CAPITAL B
unicode_changes_when_nfkc_casefolded(0x1D507, 0x1D50A).	% L&   [4] MATHEMATICAL FRAKTUR CAPITAL D..MATHEMATICAL FRAKTUR CAPITAL G
unicode_changes_when_nfkc_casefolded(0x1D50D, 0x1D514).	% L&   [8] MATHEMATICAL FRAKTUR CAPITAL J..MATHEMATICAL FRAKTUR CAPITAL Q
unicode_changes_when_nfkc_casefolded(0x1D516, 0x1D51C).	% L&   [7] MATHEMATICAL FRAKTUR CAPITAL S..MATHEMATICAL FRAKTUR CAPITAL Y
unicode_changes_when_nfkc_casefolded(0x1D51E, 0x1D539).	% L&  [28] MATHEMATICAL FRAKTUR SMALL A..MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_changes_when_nfkc_casefolded(0x1D53B, 0x1D53E).	% L&   [4] MATHEMATICAL DOUBLE-STRUCK CAPITAL D..MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_changes_when_nfkc_casefolded(0x1D540, 0x1D544).	% L&   [5] MATHEMATICAL DOUBLE-STRUCK CAPITAL I..MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_changes_when_nfkc_casefolded(0x1D546, 0x1D546).	% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_changes_when_nfkc_casefolded(0x1D54A, 0x1D550).	% L&   [7] MATHEMATICAL DOUBLE-STRUCK CAPITAL S..MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_changes_when_nfkc_casefolded(0x1D552, 0x1D6A5).	% L& [340] MATHEMATICAL DOUBLE-STRUCK SMALL A..MATHEMATICAL ITALIC SMALL DOTLESS J
unicode_changes_when_nfkc_casefolded(0x1D6A8, 0x1D6C0).	% L&  [25] MATHEMATICAL BOLD CAPITAL ALPHA..MATHEMATICAL BOLD CAPITAL OMEGA
unicode_changes_when_nfkc_casefolded(0x1D6C1, 0x1D6C1).	% Sm       MATHEMATICAL BOLD NABLA
unicode_changes_when_nfkc_casefolded(0x1D6C2, 0x1D6DA).	% L&  [25] MATHEMATICAL BOLD SMALL ALPHA..MATHEMATICAL BOLD SMALL OMEGA
unicode_changes_when_nfkc_casefolded(0x1D6DB, 0x1D6DB).	% Sm       MATHEMATICAL BOLD PARTIAL DIFFERENTIAL
unicode_changes_when_nfkc_casefolded(0x1D6DC, 0x1D6FA).	% L&  [31] MATHEMATICAL BOLD EPSILON SYMBOL..MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_changes_when_nfkc_casefolded(0x1D6FB, 0x1D6FB).	% Sm       MATHEMATICAL ITALIC NABLA
unicode_changes_when_nfkc_casefolded(0x1D6FC, 0x1D714).	% L&  [25] MATHEMATICAL ITALIC SMALL ALPHA..MATHEMATICAL ITALIC SMALL OMEGA
unicode_changes_when_nfkc_casefolded(0x1D715, 0x1D715).	% Sm       MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL
unicode_changes_when_nfkc_casefolded(0x1D716, 0x1D734).	% L&  [31] MATHEMATICAL ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_changes_when_nfkc_casefolded(0x1D735, 0x1D735).	% Sm       MATHEMATICAL BOLD ITALIC NABLA
unicode_changes_when_nfkc_casefolded(0x1D736, 0x1D74E).	% L&  [25] MATHEMATICAL BOLD ITALIC SMALL ALPHA..MATHEMATICAL BOLD ITALIC SMALL OMEGA
unicode_changes_when_nfkc_casefolded(0x1D74F, 0x1D74F).	% Sm       MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_changes_when_nfkc_casefolded(0x1D750, 0x1D76E).	% L&  [31] MATHEMATICAL BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_changes_when_nfkc_casefolded(0x1D76F, 0x1D76F).	% Sm       MATHEMATICAL SANS-SERIF BOLD NABLA
unicode_changes_when_nfkc_casefolded(0x1D770, 0x1D788).	% L&  [25] MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
unicode_changes_when_nfkc_casefolded(0x1D789, 0x1D789).	% Sm       MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL
unicode_changes_when_nfkc_casefolded(0x1D78A, 0x1D7A8).	% L&  [31] MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_changes_when_nfkc_casefolded(0x1D7A9, 0x1D7A9).	% Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA
unicode_changes_when_nfkc_casefolded(0x1D7AA, 0x1D7C2).	% L&  [25] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
unicode_changes_when_nfkc_casefolded(0x1D7C3, 0x1D7C3).	% Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_changes_when_nfkc_casefolded(0x1D7C4, 0x1D7CB).	% L&   [8] MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD SMALL DIGAMMA
unicode_changes_when_nfkc_casefolded(0x1D7CE, 0x1D7FF).	% Nd  [50] MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
unicode_changes_when_nfkc_casefolded(0x1EE00, 0x1EE03).	% Lo   [4] ARABIC MATHEMATICAL ALEF..ARABIC MATHEMATICAL DAL
unicode_changes_when_nfkc_casefolded(0x1EE05, 0x1EE1F).	% Lo  [27] ARABIC MATHEMATICAL WAW..ARABIC MATHEMATICAL DOTLESS QAF
unicode_changes_when_nfkc_casefolded(0x1EE21, 0x1EE22).	% Lo   [2] ARABIC MATHEMATICAL INITIAL BEH..ARABIC MATHEMATICAL INITIAL JEEM
unicode_changes_when_nfkc_casefolded(0x1EE24, 0x1EE24).	% Lo       ARABIC MATHEMATICAL INITIAL HEH
unicode_changes_when_nfkc_casefolded(0x1EE27, 0x1EE27).	% Lo       ARABIC MATHEMATICAL INITIAL HAH
unicode_changes_when_nfkc_casefolded(0x1EE29, 0x1EE32).	% Lo  [10] ARABIC MATHEMATICAL INITIAL YEH..ARABIC MATHEMATICAL INITIAL QAF
unicode_changes_when_nfkc_casefolded(0x1EE34, 0x1EE37).	% Lo   [4] ARABIC MATHEMATICAL INITIAL SHEEN..ARABIC MATHEMATICAL INITIAL KHAH
unicode_changes_when_nfkc_casefolded(0x1EE39, 0x1EE39).	% Lo       ARABIC MATHEMATICAL INITIAL DAD
unicode_changes_when_nfkc_casefolded(0x1EE3B, 0x1EE3B).	% Lo       ARABIC MATHEMATICAL INITIAL GHAIN
unicode_changes_when_nfkc_casefolded(0x1EE42, 0x1EE42).	% Lo       ARABIC MATHEMATICAL TAILED JEEM
unicode_changes_when_nfkc_casefolded(0x1EE47, 0x1EE47).	% Lo       ARABIC MATHEMATICAL TAILED HAH
unicode_changes_when_nfkc_casefolded(0x1EE49, 0x1EE49).	% Lo       ARABIC MATHEMATICAL TAILED YEH
unicode_changes_when_nfkc_casefolded(0x1EE4B, 0x1EE4B).	% Lo       ARABIC MATHEMATICAL TAILED LAM
unicode_changes_when_nfkc_casefolded(0x1EE4D, 0x1EE4F).	% Lo   [3] ARABIC MATHEMATICAL TAILED NOON..ARABIC MATHEMATICAL TAILED AIN
unicode_changes_when_nfkc_casefolded(0x1EE51, 0x1EE52).	% Lo   [2] ARABIC MATHEMATICAL TAILED SAD..ARABIC MATHEMATICAL TAILED QAF
unicode_changes_when_nfkc_casefolded(0x1EE54, 0x1EE54).	% Lo       ARABIC MATHEMATICAL TAILED SHEEN
unicode_changes_when_nfkc_casefolded(0x1EE57, 0x1EE57).	% Lo       ARABIC MATHEMATICAL TAILED KHAH
unicode_changes_when_nfkc_casefolded(0x1EE59, 0x1EE59).	% Lo       ARABIC MATHEMATICAL TAILED DAD
unicode_changes_when_nfkc_casefolded(0x1EE5B, 0x1EE5B).	% Lo       ARABIC MATHEMATICAL TAILED GHAIN
unicode_changes_when_nfkc_casefolded(0x1EE5D, 0x1EE5D).	% Lo       ARABIC MATHEMATICAL TAILED DOTLESS NOON
unicode_changes_when_nfkc_casefolded(0x1EE5F, 0x1EE5F).	% Lo       ARABIC MATHEMATICAL TAILED DOTLESS QAF
unicode_changes_when_nfkc_casefolded(0x1EE61, 0x1EE62).	% Lo   [2] ARABIC MATHEMATICAL STRETCHED BEH..ARABIC MATHEMATICAL STRETCHED JEEM
unicode_changes_when_nfkc_casefolded(0x1EE64, 0x1EE64).	% Lo       ARABIC MATHEMATICAL STRETCHED HEH
unicode_changes_when_nfkc_casefolded(0x1EE67, 0x1EE6A).	% Lo   [4] ARABIC MATHEMATICAL STRETCHED HAH..ARABIC MATHEMATICAL STRETCHED KAF
unicode_changes_when_nfkc_casefolded(0x1EE6C, 0x1EE72).	% Lo   [7] ARABIC MATHEMATICAL STRETCHED MEEM..ARABIC MATHEMATICAL STRETCHED QAF
unicode_changes_when_nfkc_casefolded(0x1EE74, 0x1EE77).	% Lo   [4] ARABIC MATHEMATICAL STRETCHED SHEEN..ARABIC MATHEMATICAL STRETCHED KHAH
unicode_changes_when_nfkc_casefolded(0x1EE79, 0x1EE7C).	% Lo   [4] ARABIC MATHEMATICAL STRETCHED DAD..ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
unicode_changes_when_nfkc_casefolded(0x1EE7E, 0x1EE7E).	% Lo       ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
unicode_changes_when_nfkc_casefolded(0x1EE80, 0x1EE89).	% Lo  [10] ARABIC MATHEMATICAL LOOPED ALEF..ARABIC MATHEMATICAL LOOPED YEH
unicode_changes_when_nfkc_casefolded(0x1EE8B, 0x1EE9B).	% Lo  [17] ARABIC MATHEMATICAL LOOPED LAM..ARABIC MATHEMATICAL LOOPED GHAIN
unicode_changes_when_nfkc_casefolded(0x1EEA1, 0x1EEA3).	% Lo   [3] ARABIC MATHEMATICAL DOUBLE-STRUCK BEH..ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
unicode_changes_when_nfkc_casefolded(0x1EEA5, 0x1EEA9).	% Lo   [5] ARABIC MATHEMATICAL DOUBLE-STRUCK WAW..ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
unicode_changes_when_nfkc_casefolded(0x1EEAB, 0x1EEBB).	% Lo  [17] ARABIC MATHEMATICAL DOUBLE-STRUCK LAM..ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN
unicode_changes_when_nfkc_casefolded(0x1F100, 0x1F10A).	% No  [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
unicode_changes_when_nfkc_casefolded(0x1F110, 0x1F12E).	% So  [31] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED WZ
unicode_changes_when_nfkc_casefolded(0x1F130, 0x1F14F).	% So  [32] SQUARED LATIN CAPITAL LETTER A..SQUARED WC
unicode_changes_when_nfkc_casefolded(0x1F16A, 0x1F16B).	% So   [2] RAISED MC SIGN..RAISED MD SIGN
unicode_changes_when_nfkc_casefolded(0x1F190, 0x1F190).	% So       SQUARE DJ
unicode_changes_when_nfkc_casefolded(0x1F200, 0x1F202).	% So   [3] SQUARE HIRAGANA HOKA..SQUARED KATAKANA SA
unicode_changes_when_nfkc_casefolded(0x1F210, 0x1F23A).	% So  [43] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-55B6
unicode_changes_when_nfkc_casefolded(0x1F240, 0x1F248).	% So   [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
unicode_changes_when_nfkc_casefolded(0x1F250, 0x1F251).	% So   [2] CIRCLED IDEOGRAPH ADVANTAGE..CIRCLED IDEOGRAPH ACCEPT
unicode_changes_when_nfkc_casefolded(0x2F800, 0x2FA1D).	% Lo [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D
unicode_changes_when_nfkc_casefolded(0xE0000, 0xE0000).	% Cn       <reserved-E0000>
unicode_changes_when_nfkc_casefolded(0xE0001, 0xE0001).	% Cf       LANGUAGE TAG
unicode_changes_when_nfkc_casefolded(0xE0002, 0xE001F).	% Cn  [30] <reserved-E0002>..<reserved-E001F>
unicode_changes_when_nfkc_casefolded(0xE0020, 0xE007F).	% Cf  [96] TAG SPACE..CANCEL TAG
unicode_changes_when_nfkc_casefolded(0xE0080, 0xE00FF).	% Cn [128] <reserved-E0080>..<reserved-E00FF>
unicode_changes_when_nfkc_casefolded(0xE0100, 0xE01EF).	% Mn [240] VARIATION SELECTOR-17..VARIATION SELECTOR-256
unicode_changes_when_nfkc_casefolded(0xE01F0, 0xE0FFF).	% Cn [3600] <reserved-E01F0>..<reserved-E0FFF>

% Total code points: 9944
