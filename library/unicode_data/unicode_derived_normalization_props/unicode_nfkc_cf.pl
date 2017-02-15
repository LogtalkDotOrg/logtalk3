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

% ================================================

% Derived Property:   NFKC_Casefold (NFKC_CF)
%   This property removes certain variations from characters: case, compatibility, and default-ignorables.
%   It is used for loose matching and certain types of identifiers.
%   It is constructed by applying NFKC, CaseFolding, and removal of Default_Ignorable_Code_Points.
%   The process of applying these transformations is repeated until a stable result is produced.
%   WARNING: Application to STRINGS must apply NFC after mapping each character, because characters may interact.
%            For more information, see [http://www.unicode.org/reports/tr44/]
% Omitted code points are unchanged by this mapping.
% @missing: 0000..10FFFF; NFKC_CF; <code point>

%  All code points not explicitly listed for NFKC_Casefold
%  have the value <codepoint>.

unicode_nfkc_cf(0x0041, [0x0061]).						% L&       LATIN CAPITAL LETTER A
unicode_nfkc_cf(0x0042, [0x0062]).						% L&       LATIN CAPITAL LETTER B
unicode_nfkc_cf(0x0043, [0x0063]).						% L&       LATIN CAPITAL LETTER C
unicode_nfkc_cf(0x0044, [0x0064]).						% L&       LATIN CAPITAL LETTER D
unicode_nfkc_cf(0x0045, [0x0065]).						% L&       LATIN CAPITAL LETTER E
unicode_nfkc_cf(0x0046, [0x0066]).						% L&       LATIN CAPITAL LETTER F
unicode_nfkc_cf(0x0047, [0x0067]).						% L&       LATIN CAPITAL LETTER G
unicode_nfkc_cf(0x0048, [0x0068]).						% L&       LATIN CAPITAL LETTER H
unicode_nfkc_cf(0x0049, [0x0069]).						% L&       LATIN CAPITAL LETTER I
unicode_nfkc_cf(0x004A, [0x006A]).						% L&       LATIN CAPITAL LETTER J
unicode_nfkc_cf(0x004B, [0x006B]).						% L&       LATIN CAPITAL LETTER K
unicode_nfkc_cf(0x004C, [0x006C]).						% L&       LATIN CAPITAL LETTER L
unicode_nfkc_cf(0x004D, [0x006D]).						% L&       LATIN CAPITAL LETTER M
unicode_nfkc_cf(0x004E, [0x006E]).						% L&       LATIN CAPITAL LETTER N
unicode_nfkc_cf(0x004F, [0x006F]).						% L&       LATIN CAPITAL LETTER O
unicode_nfkc_cf(0x0050, [0x0070]).						% L&       LATIN CAPITAL LETTER P
unicode_nfkc_cf(0x0051, [0x0071]).						% L&       LATIN CAPITAL LETTER Q
unicode_nfkc_cf(0x0052, [0x0072]).						% L&       LATIN CAPITAL LETTER R
unicode_nfkc_cf(0x0053, [0x0073]).						% L&       LATIN CAPITAL LETTER S
unicode_nfkc_cf(0x0054, [0x0074]).						% L&       LATIN CAPITAL LETTER T
unicode_nfkc_cf(0x0055, [0x0075]).						% L&       LATIN CAPITAL LETTER U
unicode_nfkc_cf(0x0056, [0x0076]).						% L&       LATIN CAPITAL LETTER V
unicode_nfkc_cf(0x0057, [0x0077]).						% L&       LATIN CAPITAL LETTER W
unicode_nfkc_cf(0x0058, [0x0078]).						% L&       LATIN CAPITAL LETTER X
unicode_nfkc_cf(0x0059, [0x0079]).						% L&       LATIN CAPITAL LETTER Y
unicode_nfkc_cf(0x005A, [0x007A]).						% L&       LATIN CAPITAL LETTER Z
unicode_nfkc_cf(0x00A0, [0x0020]).						% Zs       NO-BREAK SPACE
unicode_nfkc_cf(0x00A8, [0x0020, 0x0308]).				% Sk       DIAERESIS
unicode_nfkc_cf(0x00AA, [0x0061]).						% Lo       FEMININE ORDINAL INDICATOR
%unicode_nfkc_cf(0x00AD, 0x        ; NFKC_CF;       	            # Cf       SOFT HYPHEN
unicode_nfkc_cf(0x00AF, [0x0020, 0x0304]).				% Sk       MACRON
unicode_nfkc_cf(0x00B2, [0x0032]).						% No       SUPERSCRIPT TWO
unicode_nfkc_cf(0x00B3, [0x0033]).						% No       SUPERSCRIPT THREE
unicode_nfkc_cf(0x00B4, [0x0020, 0x0301]).				% Sk       ACUTE ACCENT
unicode_nfkc_cf(0x00B5, [0x03BC]).						% L&       MICRO SIGN
unicode_nfkc_cf(0x00B8, [0x0020, 0x0327]).				% Sk       CEDILLA
unicode_nfkc_cf(0x00B9, [0x0031]).						% No       SUPERSCRIPT ONE
unicode_nfkc_cf(0x00BA, [0x006F]).						% Lo       MASCULINE ORDINAL INDICATOR
unicode_nfkc_cf(0x00BC, [0x0031, 0x2044, 0x0034]).		% No       VULGAR FRACTION ONE QUARTER
unicode_nfkc_cf(0x00BD, [0x0031, 0x2044, 0x0032]).		% No       VULGAR FRACTION ONE HALF
unicode_nfkc_cf(0x00BE, [0x0033, 0x2044, 0x0034]).		% No       VULGAR FRACTION THREE QUARTERS
unicode_nfkc_cf(0x00C0, [0x00E0]).						% L&       LATIN CAPITAL LETTER A WITH GRAVE
unicode_nfkc_cf(0x00C1, [0x00E1]).						% L&       LATIN CAPITAL LETTER A WITH ACUTE
unicode_nfkc_cf(0x00C2, [0x00E2]).						% L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX
unicode_nfkc_cf(0x00C3, [0x00E3]).						% L&       LATIN CAPITAL LETTER A WITH TILDE
unicode_nfkc_cf(0x00C4, [0x00E4]).						% L&       LATIN CAPITAL LETTER A WITH DIAERESIS
unicode_nfkc_cf(0x00C5, [0x00E5]).						% L&       LATIN CAPITAL LETTER A WITH RING ABOVE
unicode_nfkc_cf(0x00C6, [0x00E6]).						% L&       LATIN CAPITAL LETTER AE
unicode_nfkc_cf(0x00C7, [0x00E7]).						% L&       LATIN CAPITAL LETTER C WITH CEDILLA
unicode_nfkc_cf(0x00C8, [0x00E8]).						% L&       LATIN CAPITAL LETTER E WITH GRAVE
unicode_nfkc_cf(0x00C9, [0x00E9]).						% L&       LATIN CAPITAL LETTER E WITH ACUTE
unicode_nfkc_cf(0x00CA, [0x00EA]).						% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX
unicode_nfkc_cf(0x00CB, [0x00EB]).						% L&       LATIN CAPITAL LETTER E WITH DIAERESIS
unicode_nfkc_cf(0x00CC, [0x00EC]).						% L&       LATIN CAPITAL LETTER I WITH GRAVE
unicode_nfkc_cf(0x00CD, [0x00ED]).						% L&       LATIN CAPITAL LETTER I WITH ACUTE
unicode_nfkc_cf(0x00CE, [0x00EE]).						% L&       LATIN CAPITAL LETTER I WITH CIRCUMFLEX
unicode_nfkc_cf(0x00CF, [0x00EF]).						% L&       LATIN CAPITAL LETTER I WITH DIAERESIS
unicode_nfkc_cf(0x00D0, [0x00F0]).						% L&       LATIN CAPITAL LETTER ETH
unicode_nfkc_cf(0x00D1, [0x00F1]).						% L&       LATIN CAPITAL LETTER N WITH TILDE
unicode_nfkc_cf(0x00D2, [0x00F2]).						% L&       LATIN CAPITAL LETTER O WITH GRAVE
unicode_nfkc_cf(0x00D3, [0x00F3]).						% L&       LATIN CAPITAL LETTER O WITH ACUTE
unicode_nfkc_cf(0x00D4, [0x00F4]).						% L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX
unicode_nfkc_cf(0x00D5, [0x00F5]).						% L&       LATIN CAPITAL LETTER O WITH TILDE
unicode_nfkc_cf(0x00D6, [0x00F6]).						% L&       LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_nfkc_cf(0x00D8, [0x00F8]).						% L&       LATIN CAPITAL LETTER O WITH STROKE
unicode_nfkc_cf(0x00D9, [0x00F9]).						% L&       LATIN CAPITAL LETTER U WITH GRAVE
unicode_nfkc_cf(0x00DA, [0x00FA]).						% L&       LATIN CAPITAL LETTER U WITH ACUTE
unicode_nfkc_cf(0x00DB, [0x00FB]).						% L&       LATIN CAPITAL LETTER U WITH CIRCUMFLEX
unicode_nfkc_cf(0x00DC, [0x00FC]).						% L&       LATIN CAPITAL LETTER U WITH DIAERESIS
unicode_nfkc_cf(0x00DD, [0x00FD]).						% L&       LATIN CAPITAL LETTER Y WITH ACUTE
unicode_nfkc_cf(0x00DE, [0x00FE]).						% L&       LATIN CAPITAL LETTER THORN
unicode_nfkc_cf(0x00DF, [0x0073, 0x0073]).				% L&       LATIN SMALL LETTER SHARP S
unicode_nfkc_cf(0x0100, [0x0101]).						% L&       LATIN CAPITAL LETTER A WITH MACRON
unicode_nfkc_cf(0x0102, [0x0103]).						% L&       LATIN CAPITAL LETTER A WITH BREVE
unicode_nfkc_cf(0x0104, [0x0105]).						% L&       LATIN CAPITAL LETTER A WITH OGONEK
unicode_nfkc_cf(0x0106, [0x0107]).						% L&       LATIN CAPITAL LETTER C WITH ACUTE
unicode_nfkc_cf(0x0108, [0x0109]).						% L&       LATIN CAPITAL LETTER C WITH CIRCUMFLEX
unicode_nfkc_cf(0x010A, [0x010B]).						% L&       LATIN CAPITAL LETTER C WITH DOT ABOVE
unicode_nfkc_cf(0x010C, [0x010D]).						% L&       LATIN CAPITAL LETTER C WITH CARON
unicode_nfkc_cf(0x010E, [0x010F]).						% L&       LATIN CAPITAL LETTER D WITH CARON
unicode_nfkc_cf(0x0110, [0x0111]).						% L&       LATIN CAPITAL LETTER D WITH STROKE
unicode_nfkc_cf(0x0112, [0x0113]).						% L&       LATIN CAPITAL LETTER E WITH MACRON
unicode_nfkc_cf(0x0114, [0x0115]).						% L&       LATIN CAPITAL LETTER E WITH BREVE
unicode_nfkc_cf(0x0116, [0x0117]).						% L&       LATIN CAPITAL LETTER E WITH DOT ABOVE
unicode_nfkc_cf(0x0118, [0x0119]).						% L&       LATIN CAPITAL LETTER E WITH OGONEK
unicode_nfkc_cf(0x011A, [0x011B]).						% L&       LATIN CAPITAL LETTER E WITH CARON
unicode_nfkc_cf(0x011C, [0x011D]).						% L&       LATIN CAPITAL LETTER G WITH CIRCUMFLEX
unicode_nfkc_cf(0x011E, [0x011F]).						% L&       LATIN CAPITAL LETTER G WITH BREVE
unicode_nfkc_cf(0x0120, [0x0121]).						% L&       LATIN CAPITAL LETTER G WITH DOT ABOVE
unicode_nfkc_cf(0x0122, [0x0123]).						% L&       LATIN CAPITAL LETTER G WITH CEDILLA
unicode_nfkc_cf(0x0124, [0x0125]).						% L&       LATIN CAPITAL LETTER H WITH CIRCUMFLEX
unicode_nfkc_cf(0x0126, [0x0127]).						% L&       LATIN CAPITAL LETTER H WITH STROKE
unicode_nfkc_cf(0x0128, [0x0129]).						% L&       LATIN CAPITAL LETTER I WITH TILDE
unicode_nfkc_cf(0x012A, [0x012B]).						% L&       LATIN CAPITAL LETTER I WITH MACRON
unicode_nfkc_cf(0x012C, [0x012D]).						% L&       LATIN CAPITAL LETTER I WITH BREVE
unicode_nfkc_cf(0x012E, [0x012F]).						% L&       LATIN CAPITAL LETTER I WITH OGONEK
unicode_nfkc_cf(0x0130, [0x0069, 0x0307]).				% L&       LATIN CAPITAL LETTER I WITH DOT ABOVE
unicode_nfkc_cf(0x0132, [0x0069, 0x006A]).				% L&       LATIN CAPITAL LIGATURE IJ
unicode_nfkc_cf(0x0133, [0x0069, 0x006A]).				% L&       LATIN SMALL LIGATURE IJ
unicode_nfkc_cf(0x0134, [0x0135]).						% L&       LATIN CAPITAL LETTER J WITH CIRCUMFLEX
unicode_nfkc_cf(0x0136, [0x0137]).						% L&       LATIN CAPITAL LETTER K WITH CEDILLA
unicode_nfkc_cf(0x0139, [0x013A]).						% L&       LATIN CAPITAL LETTER L WITH ACUTE
unicode_nfkc_cf(0x013B, [0x013C]).						% L&       LATIN CAPITAL LETTER L WITH CEDILLA
unicode_nfkc_cf(0x013D, [0x013E]).						% L&       LATIN CAPITAL LETTER L WITH CARON
unicode_nfkc_cf(0x013F, [0x006C, 0x00B7]).				% L&       LATIN CAPITAL LETTER L WITH MIDDLE DOT
unicode_nfkc_cf(0x0140, [0x006C, 0x00B7]).				% L&       LATIN SMALL LETTER L WITH MIDDLE DOT
unicode_nfkc_cf(0x0141, [0x0142]).						% L&       LATIN CAPITAL LETTER L WITH STROKE
unicode_nfkc_cf(0x0143, [0x0144]).						% L&       LATIN CAPITAL LETTER N WITH ACUTE
unicode_nfkc_cf(0x0145, [0x0146]).						% L&       LATIN CAPITAL LETTER N WITH CEDILLA
unicode_nfkc_cf(0x0147, [0x0148]).						% L&       LATIN CAPITAL LETTER N WITH CARON
unicode_nfkc_cf(0x0149, [0x02BC, 0x006E]).				% L&       LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
unicode_nfkc_cf(0x014A, [0x014B]).						% L&       LATIN CAPITAL LETTER ENG
unicode_nfkc_cf(0x014C, [0x014D]).						% L&       LATIN CAPITAL LETTER O WITH MACRON
unicode_nfkc_cf(0x014E, [0x014F]).						% L&       LATIN CAPITAL LETTER O WITH BREVE
unicode_nfkc_cf(0x0150, [0x0151]).						% L&       LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
unicode_nfkc_cf(0x0152, [0x0153]).						% L&       LATIN CAPITAL LIGATURE OE
unicode_nfkc_cf(0x0154, [0x0155]).						% L&       LATIN CAPITAL LETTER R WITH ACUTE
unicode_nfkc_cf(0x0156, [0x0157]).						% L&       LATIN CAPITAL LETTER R WITH CEDILLA
unicode_nfkc_cf(0x0158, [0x0159]).						% L&       LATIN CAPITAL LETTER R WITH CARON
unicode_nfkc_cf(0x015A, [0x015B]).						% L&       LATIN CAPITAL LETTER S WITH ACUTE
unicode_nfkc_cf(0x015C, [0x015D]).						% L&       LATIN CAPITAL LETTER S WITH CIRCUMFLEX
unicode_nfkc_cf(0x015E, [0x015F]).						% L&       LATIN CAPITAL LETTER S WITH CEDILLA
unicode_nfkc_cf(0x0160, [0x0161]).						% L&       LATIN CAPITAL LETTER S WITH CARON
unicode_nfkc_cf(0x0162, [0x0163]).						% L&       LATIN CAPITAL LETTER T WITH CEDILLA
unicode_nfkc_cf(0x0164, [0x0165]).						% L&       LATIN CAPITAL LETTER T WITH CARON
unicode_nfkc_cf(0x0166, [0x0167]).						% L&       LATIN CAPITAL LETTER T WITH STROKE
unicode_nfkc_cf(0x0168, [0x0169]).						% L&       LATIN CAPITAL LETTER U WITH TILDE
unicode_nfkc_cf(0x016A, [0x016B]).						% L&       LATIN CAPITAL LETTER U WITH MACRON
unicode_nfkc_cf(0x016C, [0x016D]).						% L&       LATIN CAPITAL LETTER U WITH BREVE
unicode_nfkc_cf(0x016E, [0x016F]).						% L&       LATIN CAPITAL LETTER U WITH RING ABOVE
unicode_nfkc_cf(0x0170, [0x0171]).						% L&       LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
unicode_nfkc_cf(0x0172, [0x0173]).						% L&       LATIN CAPITAL LETTER U WITH OGONEK
unicode_nfkc_cf(0x0174, [0x0175]).						% L&       LATIN CAPITAL LETTER W WITH CIRCUMFLEX
unicode_nfkc_cf(0x0176, [0x0177]).						% L&       LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
unicode_nfkc_cf(0x0178, [0x00FF]).						% L&       LATIN CAPITAL LETTER Y WITH DIAERESIS
unicode_nfkc_cf(0x0179, [0x017A]).						% L&       LATIN CAPITAL LETTER Z WITH ACUTE
unicode_nfkc_cf(0x017B, [0x017C]).						% L&       LATIN CAPITAL LETTER Z WITH DOT ABOVE
unicode_nfkc_cf(0x017D, [0x017E]).						% L&       LATIN CAPITAL LETTER Z WITH CARON
unicode_nfkc_cf(0x017F, [0x0073]).						% L&       LATIN SMALL LETTER LONG S
unicode_nfkc_cf(0x0181, [0x0253]).						% L&       LATIN CAPITAL LETTER B WITH HOOK
unicode_nfkc_cf(0x0182, [0x0183]).						% L&       LATIN CAPITAL LETTER B WITH TOPBAR
unicode_nfkc_cf(0x0184, [0x0185]).						% L&       LATIN CAPITAL LETTER TONE SIX
unicode_nfkc_cf(0x0186, [0x0254]).						% L&       LATIN CAPITAL LETTER OPEN O
unicode_nfkc_cf(0x0187, [0x0188]).						% L&       LATIN CAPITAL LETTER C WITH HOOK
unicode_nfkc_cf(0x0189, [0x0256]).						% L&       LATIN CAPITAL LETTER AFRICAN D
unicode_nfkc_cf(0x018A, [0x0257]).						% L&       LATIN CAPITAL LETTER D WITH HOOK
unicode_nfkc_cf(0x018B, [0x018C]).						% L&       LATIN CAPITAL LETTER D WITH TOPBAR
unicode_nfkc_cf(0x018E, [0x01DD]).						% L&       LATIN CAPITAL LETTER REVERSED E
unicode_nfkc_cf(0x018F, [0x0259]).						% L&       LATIN CAPITAL LETTER SCHWA
unicode_nfkc_cf(0x0190, [0x025B]).						% L&       LATIN CAPITAL LETTER OPEN E
unicode_nfkc_cf(0x0191, [0x0192]).						% L&       LATIN CAPITAL LETTER F WITH HOOK
unicode_nfkc_cf(0x0193, [0x0260]).						% L&       LATIN CAPITAL LETTER G WITH HOOK
unicode_nfkc_cf(0x0194, [0x0263]).						% L&       LATIN CAPITAL LETTER GAMMA
unicode_nfkc_cf(0x0196, [0x0269]).						% L&       LATIN CAPITAL LETTER IOTA
unicode_nfkc_cf(0x0197, [0x0268]).						% L&       LATIN CAPITAL LETTER I WITH STROKE
unicode_nfkc_cf(0x0198, [0x0199]).						% L&       LATIN CAPITAL LETTER K WITH HOOK
unicode_nfkc_cf(0x019C, [0x026F]).						% L&       LATIN CAPITAL LETTER TURNED M
unicode_nfkc_cf(0x019D, [0x0272]).						% L&       LATIN CAPITAL LETTER N WITH LEFT HOOK
unicode_nfkc_cf(0x019F, [0x0275]).						% L&       LATIN CAPITAL LETTER O WITH MIDDLE TILDE
unicode_nfkc_cf(0x01A0, [0x01A1]).						% L&       LATIN CAPITAL LETTER O WITH HORN
unicode_nfkc_cf(0x01A2, [0x01A3]).						% L&       LATIN CAPITAL LETTER OI
unicode_nfkc_cf(0x01A4, [0x01A5]).						% L&       LATIN CAPITAL LETTER P WITH HOOK
unicode_nfkc_cf(0x01A6, [0x0280]).						% L&       LATIN LETTER YR
unicode_nfkc_cf(0x01A7, [0x01A8]).						% L&       LATIN CAPITAL LETTER TONE TWO
unicode_nfkc_cf(0x01A9, [0x0283]).						% L&       LATIN CAPITAL LETTER ESH
unicode_nfkc_cf(0x01AC, [0x01AD]).						% L&       LATIN CAPITAL LETTER T WITH HOOK
unicode_nfkc_cf(0x01AE, [0x0288]).						% L&       LATIN CAPITAL LETTER T WITH RETROFLEX HOOK
unicode_nfkc_cf(0x01AF, [0x01B0]).						% L&       LATIN CAPITAL LETTER U WITH HORN
unicode_nfkc_cf(0x01B1, [0x028A]).						% L&       LATIN CAPITAL LETTER UPSILON
unicode_nfkc_cf(0x01B2, [0x028B]).						% L&       LATIN CAPITAL LETTER V WITH HOOK
unicode_nfkc_cf(0x01B3, [0x01B4]).						% L&       LATIN CAPITAL LETTER Y WITH HOOK
unicode_nfkc_cf(0x01B5, [0x01B6]).						% L&       LATIN CAPITAL LETTER Z WITH STROKE
unicode_nfkc_cf(0x01B7, [0x0292]).						% L&       LATIN CAPITAL LETTER EZH
unicode_nfkc_cf(0x01B8, [0x01B9]).						% L&       LATIN CAPITAL LETTER EZH REVERSED
unicode_nfkc_cf(0x01BC, [0x01BD]).						% L&       LATIN CAPITAL LETTER TONE FIVE
unicode_nfkc_cf(0x01C4, [0x0064, 0x017E]).				% L&       LATIN CAPITAL LETTER DZ WITH CARON..
unicode_nfkc_cf(0x01C5, [0x0064, 0x017E]).				% L&       ..
unicode_nfkc_cf(0x01C6, [0x0064, 0x017E]).				% L&       LATIN SMALL LETTER DZ WITH CARON
unicode_nfkc_cf(0x01C7, [0x006C, 0x006A]).				% L&       LATIN CAPITAL LETTER LJ
unicode_nfkc_cf(0x01C8, [0x006C, 0x006A]).				% L&       ..
unicode_nfkc_cf(0x01C9, [0x006C, 0x006A]).				% L&       LATIN SMALL LETTER LJ
unicode_nfkc_cf(0x01CA, [0x006E, 0x006A]).				% L&       LATIN CAPITAL LETTER NJ
unicode_nfkc_cf(0x01CB, [0x006E, 0x006A]).				% L&       ..
unicode_nfkc_cf(0x01CC, [0x006E, 0x006A]).				% L&       LATIN SMALL LETTER NJ
unicode_nfkc_cf(0x01CD, [0x01CE]).						% L&       LATIN CAPITAL LETTER A WITH CARON
unicode_nfkc_cf(0x01CF, [0x01D0]).						% L&       LATIN CAPITAL LETTER I WITH CARON
unicode_nfkc_cf(0x01D1, [0x01D2]).						% L&       LATIN CAPITAL LETTER O WITH CARON
unicode_nfkc_cf(0x01D3, [0x01D4]).						% L&       LATIN CAPITAL LETTER U WITH CARON
unicode_nfkc_cf(0x01D5, [0x01D6]).						% L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
unicode_nfkc_cf(0x01D7, [0x01D8]).						% L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
unicode_nfkc_cf(0x01D9, [0x01DA]).						% L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
unicode_nfkc_cf(0x01DB, [0x01DC]).						% L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
unicode_nfkc_cf(0x01DE, [0x01DF]).						% L&       LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
unicode_nfkc_cf(0x01E0, [0x01E1]).						% L&       LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
unicode_nfkc_cf(0x01E2, [0x01E3]).						% L&       LATIN CAPITAL LETTER AE WITH MACRON
unicode_nfkc_cf(0x01E4, [0x01E5]).						% L&       LATIN CAPITAL LETTER G WITH STROKE
unicode_nfkc_cf(0x01E6, [0x01E7]).						% L&       LATIN CAPITAL LETTER G WITH CARON
unicode_nfkc_cf(0x01E8, [0x01E9]).						% L&       LATIN CAPITAL LETTER K WITH CARON
unicode_nfkc_cf(0x01EA, [0x01EB]).						% L&       LATIN CAPITAL LETTER O WITH OGONEK
unicode_nfkc_cf(0x01EC, [0x01ED]).						% L&       LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
unicode_nfkc_cf(0x01EE, [0x01EF]).						% L&       LATIN CAPITAL LETTER EZH WITH CARON
unicode_nfkc_cf(0x01F1, [0x0064, 0x007A]).				% L&       LATIN CAPITAL LETTER DZ
unicode_nfkc_cf(0x01F2, [0x0064, 0x007A]).				% L&       ..
unicode_nfkc_cf(0x01F3, [0x0064, 0x007A]).				% L&       LATIN SMALL LETTER DZ
unicode_nfkc_cf(0x01F4, [0x01F5]).						% L&       LATIN CAPITAL LETTER G WITH ACUTE
unicode_nfkc_cf(0x01F6, [0x0195]).						% L&       LATIN CAPITAL LETTER HWAIR
unicode_nfkc_cf(0x01F7, [0x01BF]).						% L&       LATIN CAPITAL LETTER WYNN
unicode_nfkc_cf(0x01F8, [0x01F9]).						% L&       LATIN CAPITAL LETTER N WITH GRAVE
unicode_nfkc_cf(0x01FA, [0x01FB]).						% L&       LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
unicode_nfkc_cf(0x01FC, [0x01FD]).						% L&       LATIN CAPITAL LETTER AE WITH ACUTE
unicode_nfkc_cf(0x01FE, [0x01FF]).						% L&       LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
unicode_nfkc_cf(0x0200, [0x0201]).						% L&       LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
unicode_nfkc_cf(0x0202, [0x0203]).						% L&       LATIN CAPITAL LETTER A WITH INVERTED BREVE
unicode_nfkc_cf(0x0204, [0x0205]).						% L&       LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
unicode_nfkc_cf(0x0206, [0x0207]).						% L&       LATIN CAPITAL LETTER E WITH INVERTED BREVE
unicode_nfkc_cf(0x0208, [0x0209]).						% L&       LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
unicode_nfkc_cf(0x020A, [0x020B]).						% L&       LATIN CAPITAL LETTER I WITH INVERTED BREVE
unicode_nfkc_cf(0x020C, [0x020D]).						% L&       LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
unicode_nfkc_cf(0x020E, [0x020F]).						% L&       LATIN CAPITAL LETTER O WITH INVERTED BREVE
unicode_nfkc_cf(0x0210, [0x0211]).						% L&       LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
unicode_nfkc_cf(0x0212, [0x0213]).						% L&       LATIN CAPITAL LETTER R WITH INVERTED BREVE
unicode_nfkc_cf(0x0214, [0x0215]).						% L&       LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
unicode_nfkc_cf(0x0216, [0x0217]).						% L&       LATIN CAPITAL LETTER U WITH INVERTED BREVE
unicode_nfkc_cf(0x0218, [0x0219]).						% L&       LATIN CAPITAL LETTER S WITH COMMA BELOW
unicode_nfkc_cf(0x021A, [0x021B]).						% L&       LATIN CAPITAL LETTER T WITH COMMA BELOW
unicode_nfkc_cf(0x021C, [0x021D]).						% L&       LATIN CAPITAL LETTER YOGH
unicode_nfkc_cf(0x021E, [0x021F]).						% L&       LATIN CAPITAL LETTER H WITH CARON
unicode_nfkc_cf(0x0220, [0x019E]).						% L&       LATIN CAPITAL LETTER N WITH LONG RIGHT LEG
unicode_nfkc_cf(0x0222, [0x0223]).						% L&       LATIN CAPITAL LETTER OU
unicode_nfkc_cf(0x0224, [0x0225]).						% L&       LATIN CAPITAL LETTER Z WITH HOOK
unicode_nfkc_cf(0x0226, [0x0227]).						% L&       LATIN CAPITAL LETTER A WITH DOT ABOVE
unicode_nfkc_cf(0x0228, [0x0229]).						% L&       LATIN CAPITAL LETTER E WITH CEDILLA
unicode_nfkc_cf(0x022A, [0x022B]).						% L&       LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
unicode_nfkc_cf(0x022C, [0x022D]).						% L&       LATIN CAPITAL LETTER O WITH TILDE AND MACRON
unicode_nfkc_cf(0x022E, [0x022F]).						% L&       LATIN CAPITAL LETTER O WITH DOT ABOVE
unicode_nfkc_cf(0x0230, [0x0231]).						% L&       LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
unicode_nfkc_cf(0x0232, [0x0233]).						% L&       LATIN CAPITAL LETTER Y WITH MACRON
unicode_nfkc_cf(0x023A, [0x2C65]).						% L&       LATIN CAPITAL LETTER A WITH STROKE
unicode_nfkc_cf(0x023B, [0x023C]).						% L&       LATIN CAPITAL LETTER C WITH STROKE
unicode_nfkc_cf(0x023D, [0x019A]).						% L&       LATIN CAPITAL LETTER L WITH BAR
unicode_nfkc_cf(0x023E, [0x2C66]).						% L&       LATIN CAPITAL LETTER T WITH DIAGONAL STROKE
unicode_nfkc_cf(0x0241, [0x0242]).						% L&       LATIN CAPITAL LETTER GLOTTAL STOP
unicode_nfkc_cf(0x0243, [0x0180]).						% L&       LATIN CAPITAL LETTER B WITH STROKE
unicode_nfkc_cf(0x0244, [0x0289]).						% L&       LATIN CAPITAL LETTER U BAR
unicode_nfkc_cf(0x0245, [0x028C]).						% L&       LATIN CAPITAL LETTER TURNED V
unicode_nfkc_cf(0x0246, [0x0247]).						% L&       LATIN CAPITAL LETTER E WITH STROKE
unicode_nfkc_cf(0x0248, [0x0249]).						% L&       LATIN CAPITAL LETTER J WITH STROKE
unicode_nfkc_cf(0x024A, [0x024B]).						% L&       LATIN CAPITAL LETTER SMALL Q WITH HOOK TAIL
unicode_nfkc_cf(0x024C, [0x024D]).						% L&       LATIN CAPITAL LETTER R WITH STROKE
unicode_nfkc_cf(0x024E, [0x024F]).						% L&       LATIN CAPITAL LETTER Y WITH STROKE
unicode_nfkc_cf(0x02B0, [0x0068]).						% Lm       MODIFIER LETTER SMALL H
unicode_nfkc_cf(0x02B1, [0x0266]).						% Lm       MODIFIER LETTER SMALL H WITH HOOK
unicode_nfkc_cf(0x02B2, [0x006A]).						% Lm       MODIFIER LETTER SMALL J
unicode_nfkc_cf(0x02B3, [0x0072]).						% Lm       MODIFIER LETTER SMALL R
unicode_nfkc_cf(0x02B4, [0x0279]).						% Lm       MODIFIER LETTER SMALL TURNED R
unicode_nfkc_cf(0x02B5, [0x027B]).						% Lm       MODIFIER LETTER SMALL TURNED R WITH HOOK
unicode_nfkc_cf(0x02B6, [0x0281]).						% Lm       MODIFIER LETTER SMALL CAPITAL INVERTED R
unicode_nfkc_cf(0x02B7, [0x0077]).						% Lm       MODIFIER LETTER SMALL W
unicode_nfkc_cf(0x02B8, [0x0079]).						% Lm       MODIFIER LETTER SMALL Y
unicode_nfkc_cf(0x02D8, [0x0020, 0x0306]).				% Sk       BREVE
unicode_nfkc_cf(0x02D9, [0x0020, 0x0307]).				% Sk       DOT ABOVE
unicode_nfkc_cf(0x02DA, [0x0020, 0x030A]).				% Sk       RING ABOVE
unicode_nfkc_cf(0x02DB, [0x0020, 0x0328]).				% Sk       OGONEK
unicode_nfkc_cf(0x02DC, [0x0020, 0x0303]).				% Sk       SMALL TILDE
unicode_nfkc_cf(0x02DD, [0x0020, 0x030B]).				% Sk       DOUBLE ACUTE ACCENT
unicode_nfkc_cf(0x02E0, [0x0263]).						% Lm       MODIFIER LETTER SMALL GAMMA
unicode_nfkc_cf(0x02E1, [0x006C]).						% Lm       MODIFIER LETTER SMALL L
unicode_nfkc_cf(0x02E2, [0x0073]).						% Lm       MODIFIER LETTER SMALL S
unicode_nfkc_cf(0x02E3, [0x0078]).						% Lm       MODIFIER LETTER SMALL X
unicode_nfkc_cf(0x02E4, [0x0295]).						% Lm       MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_nfkc_cf(0x0340, [0x0300]).						% Mn       COMBINING GRAVE TONE MARK
unicode_nfkc_cf(0x0341, [0x0301]).						% Mn       COMBINING ACUTE TONE MARK
unicode_nfkc_cf(0x0343, [0x0313]).						% Mn       COMBINING GREEK KORONIS
unicode_nfkc_cf(0x0344, [0x0308, 0x0301]).				% Mn       COMBINING GREEK DIALYTIKA TONOS
unicode_nfkc_cf(0x0345, [0x03B9]).						% Mn       COMBINING GREEK YPOGEGRAMMENI
%unicode_nfkc_cf(0x034,                    # Mn     	  COMBINING GRAPHEME JOINER
unicode_nfkc_cf(0x0370, [0x0371]).						% L&       GREEK CAPITAL LETTER HETA
unicode_nfkc_cf(0x0372, [0x0373]).						% L&       GREEK CAPITAL LETTER ARCHAIC SAMPI
unicode_nfkc_cf(0x0374, [0x02B9]).						% Lm       GREEK NUMERAL SIGN
unicode_nfkc_cf(0x0376, [0x0377]).						% L&       GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA
unicode_nfkc_cf(0x037A, [0x0020, 0x03B9]).				% Lm       GREEK YPOGEGRAMMENI
unicode_nfkc_cf(0x037E, [0x003B]).						% Po       GREEK QUESTION MARK
unicode_nfkc_cf(0x0384, [0x0020, 0x0301]).				% Sk       GREEK TONOS
unicode_nfkc_cf(0x0385, [0x0020, 0x0308, 0x0301]).		% Sk       GREEK DIALYTIKA TONOS
unicode_nfkc_cf(0x0386, [0x03AC]).						% L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_nfkc_cf(0x0387, [0x00B7]).						% Po       GREEK ANO TELEIA
unicode_nfkc_cf(0x0388, [0x03AD]).						% L&       GREEK CAPITAL LETTER EPSILON WITH TONOS
unicode_nfkc_cf(0x0389, [0x03AE]).						% L&       GREEK CAPITAL LETTER ETA WITH TONOS
unicode_nfkc_cf(0x038A, [0x03AF]).						% L&       GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_nfkc_cf(0x038C, [0x03CC]).						% L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_nfkc_cf(0x038E, [0x03CD]).						% L&       GREEK CAPITAL LETTER UPSILON WITH TONOS
unicode_nfkc_cf(0x038F, [0x03CE]).						% L&       GREEK CAPITAL LETTER OMEGA WITH TONOS
unicode_nfkc_cf(0x0391, [0x03B1]).						% L&       GREEK CAPITAL LETTER ALPHA
unicode_nfkc_cf(0x0392, [0x03B2]).						% L&       GREEK CAPITAL LETTER BETA
unicode_nfkc_cf(0x0393, [0x03B3]).						% L&       GREEK CAPITAL LETTER GAMMA
unicode_nfkc_cf(0x0394, [0x03B4]).						% L&       GREEK CAPITAL LETTER DELTA
unicode_nfkc_cf(0x0395, [0x03B5]).						% L&       GREEK CAPITAL LETTER EPSILON
unicode_nfkc_cf(0x0396, [0x03B6]).						% L&       GREEK CAPITAL LETTER ZETA
unicode_nfkc_cf(0x0397, [0x03B7]).						% L&       GREEK CAPITAL LETTER ETA
unicode_nfkc_cf(0x0398, [0x03B8]).						% L&       GREEK CAPITAL LETTER THETA
unicode_nfkc_cf(0x0399, [0x03B9]).						% L&       GREEK CAPITAL LETTER IOTA
unicode_nfkc_cf(0x039A, [0x03BA]).						% L&       GREEK CAPITAL LETTER KAPPA
unicode_nfkc_cf(0x039B, [0x03BB]).						% L&       GREEK CAPITAL LETTER LAMDA
unicode_nfkc_cf(0x039C, [0x03BC]).						% L&       GREEK CAPITAL LETTER MU
unicode_nfkc_cf(0x039D, [0x03BD]).						% L&       GREEK CAPITAL LETTER NU
unicode_nfkc_cf(0x039E, [0x03BE]).						% L&       GREEK CAPITAL LETTER XI
unicode_nfkc_cf(0x039F, [0x03BF]).						% L&       GREEK CAPITAL LETTER OMICRON
unicode_nfkc_cf(0x03A0, [0x03C0]).						% L&       GREEK CAPITAL LETTER PI
unicode_nfkc_cf(0x03A1, [0x03C1]).						% L&       GREEK CAPITAL LETTER RHO
unicode_nfkc_cf(0x03A3, [0x03C3]).						% L&       GREEK CAPITAL LETTER SIGMA
unicode_nfkc_cf(0x03A4, [0x03C4]).						% L&       GREEK CAPITAL LETTER TAU
unicode_nfkc_cf(0x03A5, [0x03C5]).						% L&       GREEK CAPITAL LETTER UPSILON
unicode_nfkc_cf(0x03A6, [0x03C6]).						% L&       GREEK CAPITAL LETTER PHI
unicode_nfkc_cf(0x03A7, [0x03C7]).						% L&       GREEK CAPITAL LETTER CHI
unicode_nfkc_cf(0x03A8, [0x03C8]).						% L&       GREEK CAPITAL LETTER PSI
unicode_nfkc_cf(0x03A9, [0x03C9]).						% L&       GREEK CAPITAL LETTER OMEGA
unicode_nfkc_cf(0x03AA, [0x03CA]).						% L&       GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
unicode_nfkc_cf(0x03AB, [0x03CB]).						% L&       GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
unicode_nfkc_cf(0x03C2, [0x03C3]).						% L&       GREEK SMALL LETTER FINAL SIGMA
unicode_nfkc_cf(0x03CF, [0x03D7]).						% L&       GREEK CAPITAL KAI SYMBOL
unicode_nfkc_cf(0x03D0, [0x03B2]).						% L&       GREEK BETA SYMBOL
unicode_nfkc_cf(0x03D1, [0x03B8]).						% L&       GREEK THETA SYMBOL
unicode_nfkc_cf(0x03D2, [0x03C5]).						% L&       GREEK UPSILON WITH HOOK SYMBOL
unicode_nfkc_cf(0x03D3, [0x03CD]).						% L&       GREEK UPSILON WITH ACUTE AND HOOK SYMBOL
unicode_nfkc_cf(0x03D4, [0x03CB]).						% L&       GREEK UPSILON WITH DIAERESIS AND HOOK SYMBOL
unicode_nfkc_cf(0x03D5, [0x03C6]).						% L&       GREEK PHI SYMBOL
unicode_nfkc_cf(0x03D6, [0x03C0]).						% L&       GREEK PI SYMBOL
unicode_nfkc_cf(0x03D8, [0x03D9]).						% L&       GREEK LETTER ARCHAIC KOPPA
unicode_nfkc_cf(0x03DA, [0x03DB]).						% L&       GREEK LETTER STIGMA
unicode_nfkc_cf(0x03DC, [0x03DD]).						% L&       GREEK LETTER DIGAMMA
unicode_nfkc_cf(0x03DE, [0x03DF]).						% L&       GREEK LETTER KOPPA
unicode_nfkc_cf(0x03E0, [0x03E1]).						% L&       GREEK LETTER SAMPI
unicode_nfkc_cf(0x03E2, [0x03E3]).						% L&       COPTIC CAPITAL LETTER SHEI
unicode_nfkc_cf(0x03E4, [0x03E5]).						% L&       COPTIC CAPITAL LETTER FEI
unicode_nfkc_cf(0x03E6, [0x03E7]).						% L&       COPTIC CAPITAL LETTER KHEI
unicode_nfkc_cf(0x03E8, [0x03E9]).						% L&       COPTIC CAPITAL LETTER HORI
unicode_nfkc_cf(0x03EA, [0x03EB]).						% L&       COPTIC CAPITAL LETTER GANGIA
unicode_nfkc_cf(0x03EC, [0x03ED]).						% L&       COPTIC CAPITAL LETTER SHIMA
unicode_nfkc_cf(0x03EE, [0x03EF]).						% L&       COPTIC CAPITAL LETTER DEI
unicode_nfkc_cf(0x03F0, [0x03BA]).						% L&       GREEK KAPPA SYMBOL
unicode_nfkc_cf(0x03F1, [0x03C1]).						% L&       GREEK RHO SYMBOL
unicode_nfkc_cf(0x03F2, [0x03C3]).						% L&       GREEK LUNATE SIGMA SYMBOL
unicode_nfkc_cf(0x03F4, [0x03B8]).						% L&       GREEK CAPITAL THETA SYMBOL
unicode_nfkc_cf(0x03F5, [0x03B5]).						% L&       GREEK LUNATE EPSILON SYMBOL
unicode_nfkc_cf(0x03F7, [0x03F8]).						% L&       GREEK CAPITAL LETTER SHO
unicode_nfkc_cf(0x03F9, [0x03C3]).						% L&       GREEK CAPITAL LUNATE SIGMA SYMBOL
unicode_nfkc_cf(0x03FA, [0x03FB]).						% L&       GREEK CAPITAL LETTER SAN
unicode_nfkc_cf(0x03FD, [0x037B]).						% L&       GREEK CAPITAL REVERSED LUNATE SIGMA SYMBOL
unicode_nfkc_cf(0x03FE, [0x037C]).						% L&       GREEK CAPITAL DOTTED LUNATE SIGMA SYMBOL
unicode_nfkc_cf(0x03FF, [0x037D]).						% L&       GREEK CAPITAL REVERSED DOTTED LUNATE SIGMA SYMBOL
unicode_nfkc_cf(0x0400, [0x0450]).						% L&       CYRILLIC CAPITAL LETTER IE WITH GRAVE
unicode_nfkc_cf(0x0401, [0x0451]).						% L&       CYRILLIC CAPITAL LETTER IO
unicode_nfkc_cf(0x0402, [0x0452]).						% L&       CYRILLIC CAPITAL LETTER DJE
unicode_nfkc_cf(0x0403, [0x0453]).						% L&       CYRILLIC CAPITAL LETTER GJE
unicode_nfkc_cf(0x0404, [0x0454]).						% L&       CYRILLIC CAPITAL LETTER UKRAINIAN IE
unicode_nfkc_cf(0x0405, [0x0455]).						% L&       CYRILLIC CAPITAL LETTER DZE
unicode_nfkc_cf(0x0406, [0x0456]).						% L&       CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
unicode_nfkc_cf(0x0407, [0x0457]).						% L&       CYRILLIC CAPITAL LETTER YI
unicode_nfkc_cf(0x0408, [0x0458]).						% L&       CYRILLIC CAPITAL LETTER JE
unicode_nfkc_cf(0x0409, [0x0459]).						% L&       CYRILLIC CAPITAL LETTER LJE
unicode_nfkc_cf(0x040A, [0x045A]).						% L&       CYRILLIC CAPITAL LETTER NJE
unicode_nfkc_cf(0x040B, [0x045B]).						% L&       CYRILLIC CAPITAL LETTER TSHE
unicode_nfkc_cf(0x040C, [0x045C]).						% L&       CYRILLIC CAPITAL LETTER KJE
unicode_nfkc_cf(0x040D, [0x045D]).						% L&       CYRILLIC CAPITAL LETTER I WITH GRAVE
unicode_nfkc_cf(0x040E, [0x045E]).						% L&       CYRILLIC CAPITAL LETTER SHORT U
unicode_nfkc_cf(0x040F, [0x045F]).						% L&       CYRILLIC CAPITAL LETTER DZHE
unicode_nfkc_cf(0x0410, [0x0430]).						% L&       CYRILLIC CAPITAL LETTER A
unicode_nfkc_cf(0x0411, [0x0431]).						% L&       CYRILLIC CAPITAL LETTER BE
unicode_nfkc_cf(0x0412, [0x0432]).						% L&       CYRILLIC CAPITAL LETTER VE
unicode_nfkc_cf(0x0413, [0x0433]).						% L&       CYRILLIC CAPITAL LETTER GHE
unicode_nfkc_cf(0x0414, [0x0434]).						% L&       CYRILLIC CAPITAL LETTER DE
unicode_nfkc_cf(0x0415, [0x0435]).						% L&       CYRILLIC CAPITAL LETTER IE
unicode_nfkc_cf(0x0416, [0x0436]).						% L&       CYRILLIC CAPITAL LETTER ZHE
unicode_nfkc_cf(0x0417, [0x0437]).						% L&       CYRILLIC CAPITAL LETTER ZE
unicode_nfkc_cf(0x0418, [0x0438]).						% L&       CYRILLIC CAPITAL LETTER I
unicode_nfkc_cf(0x0419, [0x0439]).						% L&       CYRILLIC CAPITAL LETTER SHORT I
unicode_nfkc_cf(0x041A, [0x043A]).						% L&       CYRILLIC CAPITAL LETTER KA
unicode_nfkc_cf(0x041B, [0x043B]).						% L&       CYRILLIC CAPITAL LETTER EL
unicode_nfkc_cf(0x041C, [0x043C]).						% L&       CYRILLIC CAPITAL LETTER EM
unicode_nfkc_cf(0x041D, [0x043D]).						% L&       CYRILLIC CAPITAL LETTER EN
unicode_nfkc_cf(0x041E, [0x043E]).						% L&       CYRILLIC CAPITAL LETTER O
unicode_nfkc_cf(0x041F, [0x043F]).						% L&       CYRILLIC CAPITAL LETTER PE
unicode_nfkc_cf(0x0420, [0x0440]).						% L&       CYRILLIC CAPITAL LETTER ER
unicode_nfkc_cf(0x0421, [0x0441]).						% L&       CYRILLIC CAPITAL LETTER ES
unicode_nfkc_cf(0x0422, [0x0442]).						% L&       CYRILLIC CAPITAL LETTER TE
unicode_nfkc_cf(0x0423, [0x0443]).						% L&       CYRILLIC CAPITAL LETTER U
unicode_nfkc_cf(0x0424, [0x0444]).						% L&       CYRILLIC CAPITAL LETTER EF
unicode_nfkc_cf(0x0425, [0x0445]).						% L&       CYRILLIC CAPITAL LETTER HA
unicode_nfkc_cf(0x0426, [0x0446]).						% L&       CYRILLIC CAPITAL LETTER TSE
unicode_nfkc_cf(0x0427, [0x0447]).						% L&       CYRILLIC CAPITAL LETTER CHE
unicode_nfkc_cf(0x0428, [0x0448]).						% L&       CYRILLIC CAPITAL LETTER SHA
unicode_nfkc_cf(0x0429, [0x0449]).						% L&       CYRILLIC CAPITAL LETTER SHCHA
unicode_nfkc_cf(0x042A, [0x044A]).						% L&       CYRILLIC CAPITAL LETTER HARD SIGN
unicode_nfkc_cf(0x042B, [0x044B]).						% L&       CYRILLIC CAPITAL LETTER YERU
unicode_nfkc_cf(0x042C, [0x044C]).						% L&       CYRILLIC CAPITAL LETTER SOFT SIGN
unicode_nfkc_cf(0x042D, [0x044D]).						% L&       CYRILLIC CAPITAL LETTER E
unicode_nfkc_cf(0x042E, [0x044E]).						% L&       CYRILLIC CAPITAL LETTER YU
unicode_nfkc_cf(0x042F, [0x044F]).						% L&       CYRILLIC CAPITAL LETTER YA
unicode_nfkc_cf(0x0460, [0x0461]).						% L&       CYRILLIC CAPITAL LETTER OMEGA
unicode_nfkc_cf(0x0462, [0x0463]).						% L&       CYRILLIC CAPITAL LETTER YAT
unicode_nfkc_cf(0x0464, [0x0465]).						% L&       CYRILLIC CAPITAL LETTER IOTIFIED E
unicode_nfkc_cf(0x0466, [0x0467]).						% L&       CYRILLIC CAPITAL LETTER LITTLE YUS
unicode_nfkc_cf(0x0468, [0x0469]).						% L&       CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
unicode_nfkc_cf(0x046A, [0x046B]).						% L&       CYRILLIC CAPITAL LETTER BIG YUS
unicode_nfkc_cf(0x046C, [0x046D]).						% L&       CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
unicode_nfkc_cf(0x046E, [0x046F]).						% L&       CYRILLIC CAPITAL LETTER KSI
unicode_nfkc_cf(0x0470, [0x0471]).						% L&       CYRILLIC CAPITAL LETTER PSI
unicode_nfkc_cf(0x0472, [0x0473]).						% L&       CYRILLIC CAPITAL LETTER FITA
unicode_nfkc_cf(0x0474, [0x0475]).						% L&       CYRILLIC CAPITAL LETTER IZHITSA
unicode_nfkc_cf(0x0476, [0x0477]).						% L&       CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
unicode_nfkc_cf(0x0478, [0x0479]).						% L&       CYRILLIC CAPITAL LETTER UK
unicode_nfkc_cf(0x047A, [0x047B]).						% L&       CYRILLIC CAPITAL LETTER ROUND OMEGA
unicode_nfkc_cf(0x047C, [0x047D]).						% L&       CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
unicode_nfkc_cf(0x047E, [0x047F]).						% L&       CYRILLIC CAPITAL LETTER OT
unicode_nfkc_cf(0x0480, [0x0481]).						% L&       CYRILLIC CAPITAL LETTER KOPPA
unicode_nfkc_cf(0x048A, [0x048B]).						% L&       CYRILLIC CAPITAL LETTER SHORT I WITH TAIL
unicode_nfkc_cf(0x048C, [0x048D]).						% L&       CYRILLIC CAPITAL LETTER SEMISOFT SIGN
unicode_nfkc_cf(0x048E, [0x048F]).						% L&       CYRILLIC CAPITAL LETTER ER WITH TICK
unicode_nfkc_cf(0x0490, [0x0491]).						% L&       CYRILLIC CAPITAL LETTER GHE WITH UPTURN
unicode_nfkc_cf(0x0492, [0x0493]).						% L&       CYRILLIC CAPITAL LETTER GHE WITH STROKE
unicode_nfkc_cf(0x0494, [0x0495]).						% L&       CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
unicode_nfkc_cf(0x0496, [0x0497]).						% L&       CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
unicode_nfkc_cf(0x0498, [0x0499]).						% L&       CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
unicode_nfkc_cf(0x049A, [0x049B]).						% L&       CYRILLIC CAPITAL LETTER KA WITH DESCENDER
unicode_nfkc_cf(0x049C, [0x049D]).						% L&       CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
unicode_nfkc_cf(0x049E, [0x049F]).						% L&       CYRILLIC CAPITAL LETTER KA WITH STROKE
unicode_nfkc_cf(0x04A0, [0x04A1]).						% L&       CYRILLIC CAPITAL LETTER BASHKIR KA
unicode_nfkc_cf(0x04A2, [0x04A3]).						% L&       CYRILLIC CAPITAL LETTER EN WITH DESCENDER
unicode_nfkc_cf(0x04A4, [0x04A5]).						% L&       CYRILLIC CAPITAL LIGATURE EN GHE
unicode_nfkc_cf(0x04A6, [0x04A7]).						% L&       CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK
unicode_nfkc_cf(0x04A8, [0x04A9]).						% L&       CYRILLIC CAPITAL LETTER ABKHASIAN HA
unicode_nfkc_cf(0x04AA, [0x04AB]).						% L&       CYRILLIC CAPITAL LETTER ES WITH DESCENDER
unicode_nfkc_cf(0x04AC, [0x04AD]).						% L&       CYRILLIC CAPITAL LETTER TE WITH DESCENDER
unicode_nfkc_cf(0x04AE, [0x04AF]).						% L&       CYRILLIC CAPITAL LETTER STRAIGHT U
unicode_nfkc_cf(0x04B0, [0x04B1]).						% L&       CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
unicode_nfkc_cf(0x04B2, [0x04B3]).						% L&       CYRILLIC CAPITAL LETTER HA WITH DESCENDER
unicode_nfkc_cf(0x04B4, [0x04B5]).						% L&       CYRILLIC CAPITAL LIGATURE TE TSE
unicode_nfkc_cf(0x04B6, [0x04B7]).						% L&       CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
unicode_nfkc_cf(0x04B8, [0x04B9]).						% L&       CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
unicode_nfkc_cf(0x04BA, [0x04BB]).						% L&       CYRILLIC CAPITAL LETTER SHHA
unicode_nfkc_cf(0x04BC, [0x04BD]).						% L&       CYRILLIC CAPITAL LETTER ABKHASIAN CHE
unicode_nfkc_cf(0x04BE, [0x04BF]).						% L&       CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
unicode_nfkc_cf(0x04C0, [0x04CF]).						% L&       CYRILLIC LETTER PALOCHKA
unicode_nfkc_cf(0x04C1, [0x04C2]).						% L&       CYRILLIC CAPITAL LETTER ZHE WITH BREVE
unicode_nfkc_cf(0x04C3, [0x04C4]).						% L&       CYRILLIC CAPITAL LETTER KA WITH HOOK
unicode_nfkc_cf(0x04C5, [0x04C6]).						% L&       CYRILLIC CAPITAL LETTER EL WITH TAIL
unicode_nfkc_cf(0x04C7, [0x04C8]).						% L&       CYRILLIC CAPITAL LETTER EN WITH HOOK
unicode_nfkc_cf(0x04C9, [0x04CA]).						% L&       CYRILLIC CAPITAL LETTER EN WITH TAIL
unicode_nfkc_cf(0x04CB, [0x04CC]).						% L&       CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
unicode_nfkc_cf(0x04CD, [0x04CE]).						% L&       CYRILLIC CAPITAL LETTER EM WITH TAIL
unicode_nfkc_cf(0x04D0, [0x04D1]).						% L&       CYRILLIC CAPITAL LETTER A WITH BREVE
unicode_nfkc_cf(0x04D2, [0x04D3]).						% L&       CYRILLIC CAPITAL LETTER A WITH DIAERESIS
unicode_nfkc_cf(0x04D4, [0x04D5]).						% L&       CYRILLIC CAPITAL LIGATURE A IE
unicode_nfkc_cf(0x04D6, [0x04D7]).						% L&       CYRILLIC CAPITAL LETTER IE WITH BREVE
unicode_nfkc_cf(0x04D8, [0x04D9]).						% L&       CYRILLIC CAPITAL LETTER SCHWA
unicode_nfkc_cf(0x04DA, [0x04DB]).						% L&       CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
unicode_nfkc_cf(0x04DC, [0x04DD]).						% L&       CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
unicode_nfkc_cf(0x04DE, [0x04DF]).						% L&       CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
unicode_nfkc_cf(0x04E0, [0x04E1]).						% L&       CYRILLIC CAPITAL LETTER ABKHASIAN DZE
unicode_nfkc_cf(0x04E2, [0x04E3]).						% L&       CYRILLIC CAPITAL LETTER I WITH MACRON
unicode_nfkc_cf(0x04E4, [0x04E5]).						% L&       CYRILLIC CAPITAL LETTER I WITH DIAERESIS
unicode_nfkc_cf(0x04E6, [0x04E7]).						% L&       CYRILLIC CAPITAL LETTER O WITH DIAERESIS
unicode_nfkc_cf(0x04E8, [0x04E9]).						% L&       CYRILLIC CAPITAL LETTER BARRED O
unicode_nfkc_cf(0x04EA, [0x04EB]).						% L&       CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
unicode_nfkc_cf(0x04EC, [0x04ED]).						% L&       CYRILLIC CAPITAL LETTER E WITH DIAERESIS
unicode_nfkc_cf(0x04EE, [0x04EF]).						% L&       CYRILLIC CAPITAL LETTER U WITH MACRON
unicode_nfkc_cf(0x04F0, [0x04F1]).						% L&       CYRILLIC CAPITAL LETTER U WITH DIAERESIS
unicode_nfkc_cf(0x04F2, [0x04F3]).						% L&       CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
unicode_nfkc_cf(0x04F4, [0x04F5]).						% L&       CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
unicode_nfkc_cf(0x04F6, [0x04F7]).						% L&       CYRILLIC CAPITAL LETTER GHE WITH DESCENDER
unicode_nfkc_cf(0x04F8, [0x04F9]).						% L&       CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
unicode_nfkc_cf(0x04FA, [0x04FB]).						% L&       CYRILLIC CAPITAL LETTER GHE WITH STROKE AND HOOK
unicode_nfkc_cf(0x04FC, [0x04FD]).						% L&       CYRILLIC CAPITAL LETTER HA WITH HOOK
unicode_nfkc_cf(0x04FE, [0x04FF]).						% L&       CYRILLIC CAPITAL LETTER HA WITH STROKE
unicode_nfkc_cf(0x0500, [0x0501]).						% L&       CYRILLIC CAPITAL LETTER KOMI DE
unicode_nfkc_cf(0x0502, [0x0503]).						% L&       CYRILLIC CAPITAL LETTER KOMI DJE
unicode_nfkc_cf(0x0504, [0x0505]).						% L&       CYRILLIC CAPITAL LETTER KOMI ZJE
unicode_nfkc_cf(0x0506, [0x0507]).						% L&       CYRILLIC CAPITAL LETTER KOMI DZJE
unicode_nfkc_cf(0x0508, [0x0509]).						% L&       CYRILLIC CAPITAL LETTER KOMI LJE
unicode_nfkc_cf(0x050A, [0x050B]).						% L&       CYRILLIC CAPITAL LETTER KOMI NJE
unicode_nfkc_cf(0x050C, [0x050D]).						% L&       CYRILLIC CAPITAL LETTER KOMI SJE
unicode_nfkc_cf(0x050E, [0x050F]).						% L&       CYRILLIC CAPITAL LETTER KOMI TJE
unicode_nfkc_cf(0x0510, [0x0511]).						% L&       CYRILLIC CAPITAL LETTER REVERSED ZE
unicode_nfkc_cf(0x0512, [0x0513]).						% L&       CYRILLIC CAPITAL LETTER EL WITH HOOK
unicode_nfkc_cf(0x0514, [0x0515]).						% L&       CYRILLIC CAPITAL LETTER LHA
unicode_nfkc_cf(0x0516, [0x0517]).						% L&       CYRILLIC CAPITAL LETTER RHA
unicode_nfkc_cf(0x0518, [0x0519]).						% L&       CYRILLIC CAPITAL LETTER YAE
unicode_nfkc_cf(0x051A, [0x051B]).						% L&       CYRILLIC CAPITAL LETTER QA
unicode_nfkc_cf(0x051C, [0x051D]).						% L&       CYRILLIC CAPITAL LETTER WE
unicode_nfkc_cf(0x051E, [0x051F]).						% L&       CYRILLIC CAPITAL LETTER ALEUT KA
unicode_nfkc_cf(0x0520, [0x0521]).						% L&       CYRILLIC CAPITAL LETTER EL WITH MIDDLE HOOK
unicode_nfkc_cf(0x0522, [0x0523]).						% L&       CYRILLIC CAPITAL LETTER EN WITH MIDDLE HOOK
unicode_nfkc_cf(0x0524, [0x0525]).						% L&       CYRILLIC CAPITAL LETTER PE WITH DESCENDER
unicode_nfkc_cf(0x0526, [0x0527]).						% L&       CYRILLIC CAPITAL LETTER SHHA WITH DESCENDER
unicode_nfkc_cf(0x0531, [0x0561]).						% L&       ARMENIAN CAPITAL LETTER AYB
unicode_nfkc_cf(0x0532, [0x0562]).						% L&       ARMENIAN CAPITAL LETTER BEN
unicode_nfkc_cf(0x0533, [0x0563]).						% L&       ARMENIAN CAPITAL LETTER GIM
unicode_nfkc_cf(0x0534, [0x0564]).						% L&       ARMENIAN CAPITAL LETTER DA
unicode_nfkc_cf(0x0535, [0x0565]).						% L&       ARMENIAN CAPITAL LETTER ECH
unicode_nfkc_cf(0x0536, [0x0566]).						% L&       ARMENIAN CAPITAL LETTER ZA
unicode_nfkc_cf(0x0537, [0x0567]).						% L&       ARMENIAN CAPITAL LETTER EH
unicode_nfkc_cf(0x0538, [0x0568]).						% L&       ARMENIAN CAPITAL LETTER ET
unicode_nfkc_cf(0x0539, [0x0569]).						% L&       ARMENIAN CAPITAL LETTER TO
unicode_nfkc_cf(0x053A, [0x056A]).						% L&       ARMENIAN CAPITAL LETTER ZHE
unicode_nfkc_cf(0x053B, [0x056B]).						% L&       ARMENIAN CAPITAL LETTER INI
unicode_nfkc_cf(0x053C, [0x056C]).						% L&       ARMENIAN CAPITAL LETTER LIWN
unicode_nfkc_cf(0x053D, [0x056D]).						% L&       ARMENIAN CAPITAL LETTER XEH
unicode_nfkc_cf(0x053E, [0x056E]).						% L&       ARMENIAN CAPITAL LETTER CA
unicode_nfkc_cf(0x053F, [0x056F]).						% L&       ARMENIAN CAPITAL LETTER KEN
unicode_nfkc_cf(0x0540, [0x0570]).						% L&       ARMENIAN CAPITAL LETTER HO
unicode_nfkc_cf(0x0541, [0x0571]).						% L&       ARMENIAN CAPITAL LETTER JA
unicode_nfkc_cf(0x0542, [0x0572]).						% L&       ARMENIAN CAPITAL LETTER GHAD
unicode_nfkc_cf(0x0543, [0x0573]).						% L&       ARMENIAN CAPITAL LETTER CHEH
unicode_nfkc_cf(0x0544, [0x0574]).						% L&       ARMENIAN CAPITAL LETTER MEN
unicode_nfkc_cf(0x0545, [0x0575]).						% L&       ARMENIAN CAPITAL LETTER YI
unicode_nfkc_cf(0x0546, [0x0576]).						% L&       ARMENIAN CAPITAL LETTER NOW
unicode_nfkc_cf(0x0547, [0x0577]).						% L&       ARMENIAN CAPITAL LETTER SHA
unicode_nfkc_cf(0x0548, [0x0578]).						% L&       ARMENIAN CAPITAL LETTER VO
unicode_nfkc_cf(0x0549, [0x0579]).						% L&       ARMENIAN CAPITAL LETTER CHA
unicode_nfkc_cf(0x054A, [0x057A]).						% L&       ARMENIAN CAPITAL LETTER PEH
unicode_nfkc_cf(0x054B, [0x057B]).						% L&       ARMENIAN CAPITAL LETTER JHEH
unicode_nfkc_cf(0x054C, [0x057C]).						% L&       ARMENIAN CAPITAL LETTER RA
unicode_nfkc_cf(0x054D, [0x057D]).						% L&       ARMENIAN CAPITAL LETTER SEH
unicode_nfkc_cf(0x054E, [0x057E]).						% L&       ARMENIAN CAPITAL LETTER VEW
unicode_nfkc_cf(0x054F, [0x057F]).						% L&       ARMENIAN CAPITAL LETTER TIWN
unicode_nfkc_cf(0x0550, [0x0580]).						% L&       ARMENIAN CAPITAL LETTER REH
unicode_nfkc_cf(0x0551, [0x0581]).						% L&       ARMENIAN CAPITAL LETTER CO
unicode_nfkc_cf(0x0552, [0x0582]).						% L&       ARMENIAN CAPITAL LETTER YIWN
unicode_nfkc_cf(0x0553, [0x0583]).						% L&       ARMENIAN CAPITAL LETTER PIWR
unicode_nfkc_cf(0x0554, [0x0584]).						% L&       ARMENIAN CAPITAL LETTER KEH
unicode_nfkc_cf(0x0555, [0x0585]).						% L&       ARMENIAN CAPITAL LETTER OH
unicode_nfkc_cf(0x0556, [0x0586]).						% L&       ARMENIAN CAPITAL LETTER FEH
unicode_nfkc_cf(0x0587, [0x0565, 0x0582]).				% L&       ARMENIAN SMALL LIGATURE ECH YIWN
unicode_nfkc_cf(0x0675, [0x0627, 0x0674]).				% Lo       ARABIC LETTER HIGH HAMZA ALEF
unicode_nfkc_cf(0x0676, [0x0648, 0x0674]).				% Lo       ARABIC LETTER HIGH HAMZA WAW
unicode_nfkc_cf(0x0677, [0x06C7, 0x0674]).				% Lo       ARABIC LETTER U WITH HAMZA ABOVE
unicode_nfkc_cf(0x0678, [0x064A, 0x0674]).				% Lo       ARABIC LETTER HIGH HAMZA YEH
unicode_nfkc_cf(0x0958, [0x0915, 0x093C]).				% Lo       DEVANAGARI LETTER QA
unicode_nfkc_cf(0x0959, [0x0916, 0x093C]).				% Lo       DEVANAGARI LETTER KHHA
unicode_nfkc_cf(0x095A, [0x0917, 0x093C]).				% Lo       DEVANAGARI LETTER GHHA
unicode_nfkc_cf(0x095B, [0x091C, 0x093C]).				% Lo       DEVANAGARI LETTER ZA
unicode_nfkc_cf(0x095C, [0x0921, 0x093C]).				% Lo       DEVANAGARI LETTER DDDHA
unicode_nfkc_cf(0x095D, [0x0922, 0x093C]).				% Lo       DEVANAGARI LETTER RHA
unicode_nfkc_cf(0x095E, [0x092B, 0x093C]).				% Lo       DEVANAGARI LETTER FA
unicode_nfkc_cf(0x095F, [0x092F, 0x093C]).				% Lo       DEVANAGARI LETTER YYA
unicode_nfkc_cf(0x09DC, [0x09A1, 0x09BC]).				% Lo       BENGALI LETTER RRA
unicode_nfkc_cf(0x09DD, [0x09A2, 0x09BC]).				% Lo       BENGALI LETTER RHA
unicode_nfkc_cf(0x09DF, [0x09AF, 0x09BC]).				% Lo       BENGALI LETTER YYA
unicode_nfkc_cf(0x0A33, [0x0A32, 0x0A3C]).				% Lo       GURMUKHI LETTER LLA
unicode_nfkc_cf(0x0A36, [0x0A38, 0x0A3C]).				% Lo       GURMUKHI LETTER SHA
unicode_nfkc_cf(0x0A59, [0x0A16, 0x0A3C]).				% Lo       GURMUKHI LETTER KHHA
unicode_nfkc_cf(0x0A5A, [0x0A17, 0x0A3C]).				% Lo       GURMUKHI LETTER GHHA
unicode_nfkc_cf(0x0A5B, [0x0A1C, 0x0A3C]).				% Lo       GURMUKHI LETTER ZA
unicode_nfkc_cf(0x0A5E, [0x0A2B, 0x0A3C]).				% Lo       GURMUKHI LETTER FA
unicode_nfkc_cf(0x0B5C, [0x0B21, 0x0B3C]).				% Lo       ORIYA LETTER RRA
unicode_nfkc_cf(0x0B5D, [0x0B22, 0x0B3C]).				% Lo       ORIYA LETTER RHA
unicode_nfkc_cf(0x0E33, [0x0E4D, 0x0E32]).				% Lo       THAI CHARACTER SARA AM
unicode_nfkc_cf(0x0EB3, [0x0ECD, 0x0EB2]).				% Lo       LAO VOWEL SIGN AM
unicode_nfkc_cf(0x0EDC, [0x0EAB, 0x0E99]).				% Lo       LAO HO NO
unicode_nfkc_cf(0x0EDD, [0x0EAB, 0x0EA1]).				% Lo       LAO HO MO
unicode_nfkc_cf(0x0F0C, [0x0F0B]).						% Po       TIBETAN MARK DELIMITER TSHEG BSTAR
unicode_nfkc_cf(0x0F43, [0x0F42, 0x0FB7]).				% Lo       TIBETAN LETTER GHA
unicode_nfkc_cf(0x0F4D, [0x0F4C, 0x0FB7]).				% Lo       TIBETAN LETTER DDHA
unicode_nfkc_cf(0x0F52, [0x0F51, 0x0FB7]).				% Lo       TIBETAN LETTER DHA
unicode_nfkc_cf(0x0F57, [0x0F56, 0x0FB7]).				% Lo       TIBETAN LETTER BHA
unicode_nfkc_cf(0x0F5C, [0x0F5B, 0x0FB7]).				% Lo       TIBETAN LETTER DZHA
unicode_nfkc_cf(0x0F69, [0x0F40, 0x0FB5]).				% Lo       TIBETAN LETTER KSSA
unicode_nfkc_cf(0x0F73, [0x0F71, 0x0F72]).				% Mn       TIBETAN VOWEL SIGN II
unicode_nfkc_cf(0x0F75, [0x0F71, 0x0F74]).				% Mn       TIBETAN VOWEL SIGN UU
unicode_nfkc_cf(0x0F76, [0x0FB2, 0x0F80]).				% Mn       TIBETAN VOWEL SIGN VOCALIC R
unicode_nfkc_cf(0x0F77, [0x0FB2, 0x0F71, 0x0F80]).		% Mn       TIBETAN VOWEL SIGN VOCALIC RR
unicode_nfkc_cf(0x0F78, [0x0FB3, 0x0F80]). 				% Mn       TIBETAN VOWEL SIGN VOCALIC L
unicode_nfkc_cf(0x0F79, [0x0FB3, 0x0F71, 0x0F80]).		% Mn       TIBETAN VOWEL SIGN VOCALIC LL
unicode_nfkc_cf(0x0F81, [0x0F71, 0x0F80]).				% Mn       TIBETAN VOWEL SIGN REVERSED II
unicode_nfkc_cf(0x0F93, [0x0F92, 0x0FB7]).				% Mn       TIBETAN SUBJOINED LETTER GHA
unicode_nfkc_cf(0x0F9D, [0x0F9C, 0x0FB7]).				% Mn       TIBETAN SUBJOINED LETTER DDHA
unicode_nfkc_cf(0x0FA2, [0x0FA1, 0x0FB7]).				% Mn       TIBETAN SUBJOINED LETTER DHA
unicode_nfkc_cf(0x0FA7, [0x0FA6, 0x0FB7]).				% Mn       TIBETAN SUBJOINED LETTER BHA
unicode_nfkc_cf(0x0FAC, [0x0FAB, 0x0FB7]).				% Mn       TIBETAN SUBJOINED LETTER DZHA
unicode_nfkc_cf(0x0FB9, [0x0F90, 0x0FB5]).				% Mn       TIBETAN SUBJOINED LETTER KSSA
unicode_nfkc_cf(0x10A0, [0x2D00]).						% L&       GEORGIAN CAPITAL LETTER AN
unicode_nfkc_cf(0x10A1, [0x2D01]).						% L&       GEORGIAN CAPITAL LETTER BAN
unicode_nfkc_cf(0x10A2, [0x2D02]).						% L&       GEORGIAN CAPITAL LETTER GAN
unicode_nfkc_cf(0x10A3, [0x2D03]).						% L&       GEORGIAN CAPITAL LETTER DON
unicode_nfkc_cf(0x10A4, [0x2D04]).						% L&       GEORGIAN CAPITAL LETTER EN
unicode_nfkc_cf(0x10A5, [0x2D05]).						% L&       GEORGIAN CAPITAL LETTER VIN
unicode_nfkc_cf(0x10A6, [0x2D06]).						% L&       GEORGIAN CAPITAL LETTER ZEN
unicode_nfkc_cf(0x10A7, [0x2D07]).						% L&       GEORGIAN CAPITAL LETTER TAN
unicode_nfkc_cf(0x10A8, [0x2D08]).						% L&       GEORGIAN CAPITAL LETTER IN
unicode_nfkc_cf(0x10A9, [0x2D09]).						% L&       GEORGIAN CAPITAL LETTER KAN
unicode_nfkc_cf(0x10AA, [0x2D0A]).						% L&       GEORGIAN CAPITAL LETTER LAS
unicode_nfkc_cf(0x10AB, [0x2D0B]).						% L&       GEORGIAN CAPITAL LETTER MAN
unicode_nfkc_cf(0x10AC, [0x2D0C]).						% L&       GEORGIAN CAPITAL LETTER NAR
unicode_nfkc_cf(0x10AD, [0x2D0D]).						% L&       GEORGIAN CAPITAL LETTER ON
unicode_nfkc_cf(0x10AE, [0x2D0E]).						% L&       GEORGIAN CAPITAL LETTER PAR
unicode_nfkc_cf(0x10AF, [0x2D0F]).						% L&       GEORGIAN CAPITAL LETTER ZHAR
unicode_nfkc_cf(0x10B0, [0x2D10]).						% L&       GEORGIAN CAPITAL LETTER RAE
unicode_nfkc_cf(0x10B1, [0x2D11]).						% L&       GEORGIAN CAPITAL LETTER SAN
unicode_nfkc_cf(0x10B2, [0x2D12]).						% L&       GEORGIAN CAPITAL LETTER TAR
unicode_nfkc_cf(0x10B3, [0x2D13]).						% L&       GEORGIAN CAPITAL LETTER UN
unicode_nfkc_cf(0x10B4, [0x2D14]).						% L&       GEORGIAN CAPITAL LETTER PHAR
unicode_nfkc_cf(0x10B5, [0x2D15]).						% L&       GEORGIAN CAPITAL LETTER KHAR
unicode_nfkc_cf(0x10B6, [0x2D16]).						% L&       GEORGIAN CAPITAL LETTER GHAN
unicode_nfkc_cf(0x10B7, [0x2D17]).						% L&       GEORGIAN CAPITAL LETTER QAR
unicode_nfkc_cf(0x10B8, [0x2D18]).						% L&       GEORGIAN CAPITAL LETTER SHIN
unicode_nfkc_cf(0x10B9, [0x2D19]).						% L&       GEORGIAN CAPITAL LETTER CHIN
unicode_nfkc_cf(0x10BA, [0x2D1A]).						% L&       GEORGIAN CAPITAL LETTER CAN
unicode_nfkc_cf(0x10BB, [0x2D1B]).						% L&       GEORGIAN CAPITAL LETTER JIL
unicode_nfkc_cf(0x10BC, [0x2D1C]).						% L&       GEORGIAN CAPITAL LETTER CIL
unicode_nfkc_cf(0x10BD, [0x2D1D]).						% L&       GEORGIAN CAPITAL LETTER CHAR
unicode_nfkc_cf(0x10BE, [0x2D1E]).						% L&       GEORGIAN CAPITAL LETTER XAN
unicode_nfkc_cf(0x10BF, [0x2D1F]).						% L&       GEORGIAN CAPITAL LETTER JHAN
unicode_nfkc_cf(0x10C0, [0x2D20]).						% L&       GEORGIAN CAPITAL LETTER HAE
unicode_nfkc_cf(0x10C1, [0x2D21]).						% L&       GEORGIAN CAPITAL LETTER HE
unicode_nfkc_cf(0x10C2, [0x2D22]).						% L&       GEORGIAN CAPITAL LETTER HIE
unicode_nfkc_cf(0x10C3, [0x2D23]).						% L&       GEORGIAN CAPITAL LETTER WE
unicode_nfkc_cf(0x10C4, [0x2D24]).						% L&       GEORGIAN CAPITAL LETTER HAR
unicode_nfkc_cf(0x10C5, [0x2D25]).						% L&       GEORGIAN CAPITAL LETTER HOE
unicode_nfkc_cf(0x10C7, [0x2D27]).						% L&       GEORGIAN CAPITAL LETTER YN
unicode_nfkc_cf(0x10CD, [0x2D2D]).						% L&       GEORGIAN CAPITAL LETTER AEN
unicode_nfkc_cf(0x10FC, [0x10DC]).						% Lm       MODIFIER LETTER GEORGIAN NAR
%unicode_nfkc_cf(0x115F, 0x1160    ; NFKC_CF;       	         # Lo   [2] HANGUL CHOSEONG FILLER..HANGUL JUNGSEONG FILLER
%unicode_nfkc_cf(0x17B4, 0x17B5    ; NFKC_CF;       	         # Mn   [2] KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA
%unicode_nfkc_cf(0x180B, 0x180D    ; NFKC_CF;       	         # Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
unicode_nfkc_cf(0x1D2C, [0x0061]).						% Lm       MODIFIER LETTER CAPITAL A
unicode_nfkc_cf(0x1D2D, [0x00E6]).						% Lm       MODIFIER LETTER CAPITAL AE
unicode_nfkc_cf(0x1D2E, [0x0062]).						% Lm       MODIFIER LETTER CAPITAL B
unicode_nfkc_cf(0x1D30, [0x0064]).						% Lm       MODIFIER LETTER CAPITAL D
unicode_nfkc_cf(0x1D31, [0x0065]).						% Lm       MODIFIER LETTER CAPITAL E
unicode_nfkc_cf(0x1D32, [0x01DD]).						% Lm       MODIFIER LETTER CAPITAL REVERSED E
unicode_nfkc_cf(0x1D33, [0x0067]).						% Lm       MODIFIER LETTER CAPITAL G
unicode_nfkc_cf(0x1D34, [0x0068]).						% Lm       MODIFIER LETTER CAPITAL H
unicode_nfkc_cf(0x1D35, [0x0069]).						% Lm       MODIFIER LETTER CAPITAL I
unicode_nfkc_cf(0x1D36, [0x006A]).						% Lm       MODIFIER LETTER CAPITAL J
unicode_nfkc_cf(0x1D37, [0x006B]).						% Lm       MODIFIER LETTER CAPITAL K
unicode_nfkc_cf(0x1D38, [0x006C]).						% Lm       MODIFIER LETTER CAPITAL L
unicode_nfkc_cf(0x1D39, [0x006D]).						% Lm       MODIFIER LETTER CAPITAL M
unicode_nfkc_cf(0x1D3A, [0x006E]).						% Lm       MODIFIER LETTER CAPITAL N
unicode_nfkc_cf(0x1D3C, [0x006F]).						% Lm       MODIFIER LETTER CAPITAL O
unicode_nfkc_cf(0x1D3D, [0x0223]).						% Lm       MODIFIER LETTER CAPITAL OU
unicode_nfkc_cf(0x1D3E, [0x0070]).						% Lm       MODIFIER LETTER CAPITAL P
unicode_nfkc_cf(0x1D3F, [0x0072]).						% Lm       MODIFIER LETTER CAPITAL R
unicode_nfkc_cf(0x1D40, [0x0074]).						% Lm       MODIFIER LETTER CAPITAL T
unicode_nfkc_cf(0x1D41, [0x0075]).						% Lm       MODIFIER LETTER CAPITAL U
unicode_nfkc_cf(0x1D42, [0x0077]).						% Lm       MODIFIER LETTER CAPITAL W
unicode_nfkc_cf(0x1D43, [0x0061]).						% Lm       MODIFIER LETTER SMALL A
unicode_nfkc_cf(0x1D44, [0x0250]).						% Lm       MODIFIER LETTER SMALL TURNED A
unicode_nfkc_cf(0x1D45, [0x0251]).						% Lm       MODIFIER LETTER SMALL ALPHA
unicode_nfkc_cf(0x1D46, [0x1D02]).						% Lm       MODIFIER LETTER SMALL TURNED AE
unicode_nfkc_cf(0x1D47, [0x0062]).						% Lm       MODIFIER LETTER SMALL B
unicode_nfkc_cf(0x1D48, [0x0064]).						% Lm       MODIFIER LETTER SMALL D
unicode_nfkc_cf(0x1D49, [0x0065]).						% Lm       MODIFIER LETTER SMALL E
unicode_nfkc_cf(0x1D4A, [0x0259]).						% Lm       MODIFIER LETTER SMALL SCHWA
unicode_nfkc_cf(0x1D4B, [0x025B]).						% Lm       MODIFIER LETTER SMALL OPEN E
unicode_nfkc_cf(0x1D4C, [0x025C]).						% Lm       MODIFIER LETTER SMALL TURNED OPEN E
unicode_nfkc_cf(0x1D4D, [0x0067]).						% Lm       MODIFIER LETTER SMALL G
unicode_nfkc_cf(0x1D4F, [0x006B]).						% Lm       MODIFIER LETTER SMALL K
unicode_nfkc_cf(0x1D50, [0x006D]).						% Lm       MODIFIER LETTER SMALL M
unicode_nfkc_cf(0x1D51, [0x014B]).						% Lm       MODIFIER LETTER SMALL ENG
unicode_nfkc_cf(0x1D52, [0x006F]).						% Lm       MODIFIER LETTER SMALL O
unicode_nfkc_cf(0x1D53, [0x0254]).						% Lm       MODIFIER LETTER SMALL OPEN O
unicode_nfkc_cf(0x1D54, [0x1D16]).						% Lm       MODIFIER LETTER SMALL TOP HALF O
unicode_nfkc_cf(0x1D55, [0x1D17]).						% Lm       MODIFIER LETTER SMALL BOTTOM HALF O
unicode_nfkc_cf(0x1D56, [0x0070]).						% Lm       MODIFIER LETTER SMALL P
unicode_nfkc_cf(0x1D57, [0x0074]).						% Lm       MODIFIER LETTER SMALL T
unicode_nfkc_cf(0x1D58, [0x0075]).						% Lm       MODIFIER LETTER SMALL U
unicode_nfkc_cf(0x1D59, [0x1D1D]).						% Lm       MODIFIER LETTER SMALL SIDEWAYS U
unicode_nfkc_cf(0x1D5A, [0x026F]).						% Lm       MODIFIER LETTER SMALL TURNED M
unicode_nfkc_cf(0x1D5B, [0x0076]).						% Lm       MODIFIER LETTER SMALL V
unicode_nfkc_cf(0x1D5C, [0x1D25]).						% Lm       MODIFIER LETTER SMALL AIN
unicode_nfkc_cf(0x1D5D, [0x03B2]).						% Lm       MODIFIER LETTER SMALL BETA
unicode_nfkc_cf(0x1D5E, [0x03B3]).						% Lm       MODIFIER LETTER SMALL GREEK GAMMA
unicode_nfkc_cf(0x1D5F, [0x03B4]).						% Lm       MODIFIER LETTER SMALL DELTA
unicode_nfkc_cf(0x1D60, [0x03C6]).						% Lm       MODIFIER LETTER SMALL GREEK PHI
unicode_nfkc_cf(0x1D61, [0x03C7]).						% Lm       MODIFIER LETTER SMALL CHI
unicode_nfkc_cf(0x1D62, [0x0069]).						% Lm       LATIN SUBSCRIPT SMALL LETTER I
unicode_nfkc_cf(0x1D63, [0x0072]).						% Lm       LATIN SUBSCRIPT SMALL LETTER R
unicode_nfkc_cf(0x1D64, [0x0075]).						% Lm       LATIN SUBSCRIPT SMALL LETTER U
unicode_nfkc_cf(0x1D65, [0x0076]).						% Lm       LATIN SUBSCRIPT SMALL LETTER V
unicode_nfkc_cf(0x1D66, [0x03B2]).						% Lm       GREEK SUBSCRIPT SMALL LETTER BETA
unicode_nfkc_cf(0x1D67, [0x03B3]).						% Lm       GREEK SUBSCRIPT SMALL LETTER GAMMA
unicode_nfkc_cf(0x1D68, [0x03C1]).						% Lm       GREEK SUBSCRIPT SMALL LETTER RHO
unicode_nfkc_cf(0x1D69, [0x03C6]).						% Lm       GREEK SUBSCRIPT SMALL LETTER PHI
unicode_nfkc_cf(0x1D6A, [0x03C7]).						% Lm       GREEK SUBSCRIPT SMALL LETTER CHI
unicode_nfkc_cf(0x1D78, [0x043D]).						% Lm       MODIFIER LETTER CYRILLIC EN
unicode_nfkc_cf(0x1D9B, [0x0252]).						% Lm       MODIFIER LETTER SMALL TURNED ALPHA
unicode_nfkc_cf(0x1D9C, [0x0063]).						% Lm       MODIFIER LETTER SMALL C
unicode_nfkc_cf(0x1D9D, [0x0255]).						% Lm       MODIFIER LETTER SMALL C WITH CURL
unicode_nfkc_cf(0x1D9E, [0x00F0]).						% Lm       MODIFIER LETTER SMALL ETH
unicode_nfkc_cf(0x1D9F, [0x025C]).						% Lm       MODIFIER LETTER SMALL REVERSED OPEN E
unicode_nfkc_cf(0x1DA0, [0x0066]).						% Lm       MODIFIER LETTER SMALL F
unicode_nfkc_cf(0x1DA1, [0x025F]).						% Lm       MODIFIER LETTER SMALL DOTLESS J WITH STROKE
unicode_nfkc_cf(0x1DA2, [0x0261]).						% Lm       MODIFIER LETTER SMALL SCRIPT G
unicode_nfkc_cf(0x1DA3, [0x0265]).						% Lm       MODIFIER LETTER SMALL TURNED H
unicode_nfkc_cf(0x1DA4, [0x0268]).						% Lm       MODIFIER LETTER SMALL I WITH STROKE
unicode_nfkc_cf(0x1DA5, [0x0269]).						% Lm       MODIFIER LETTER SMALL IOTA
unicode_nfkc_cf(0x1DA6, [0x026A]).						% Lm       MODIFIER LETTER SMALL CAPITAL I
unicode_nfkc_cf(0x1DA7, [0x1D7B]).						% Lm       MODIFIER LETTER SMALL CAPITAL I WITH STROKE
unicode_nfkc_cf(0x1DA8, [0x029D]).						% Lm       MODIFIER LETTER SMALL J WITH CROSSED-TAIL
unicode_nfkc_cf(0x1DA9, [0x026D]).						% Lm       MODIFIER LETTER SMALL L WITH RETROFLEX HOOK
unicode_nfkc_cf(0x1DAA, [0x1D85]).						% Lm       MODIFIER LETTER SMALL L WITH PALATAL HOOK
unicode_nfkc_cf(0x1DAB, [0x029F]).						% Lm       MODIFIER LETTER SMALL CAPITAL L
unicode_nfkc_cf(0x1DAC, [0x0271]).						% Lm       MODIFIER LETTER SMALL M WITH HOOK
unicode_nfkc_cf(0x1DAD, [0x0270]).						% Lm       MODIFIER LETTER SMALL TURNED M WITH LONG LEG
unicode_nfkc_cf(0x1DAE, [0x0272]).						% Lm       MODIFIER LETTER SMALL N WITH LEFT HOOK
unicode_nfkc_cf(0x1DAF, [0x0273]).						% Lm       MODIFIER LETTER SMALL N WITH RETROFLEX HOOK
unicode_nfkc_cf(0x1DB0, [0x0274]).						% Lm       MODIFIER LETTER SMALL CAPITAL N
unicode_nfkc_cf(0x1DB1, [0x0275]).						% Lm       MODIFIER LETTER SMALL BARRED O
unicode_nfkc_cf(0x1DB2, [0x0278]).						% Lm       MODIFIER LETTER SMALL PHI
unicode_nfkc_cf(0x1DB3, [0x0282]).						% Lm       MODIFIER LETTER SMALL S WITH HOOK
unicode_nfkc_cf(0x1DB4, [0x0283]).						% Lm       MODIFIER LETTER SMALL ESH
unicode_nfkc_cf(0x1DB5, [0x01AB]).						% Lm       MODIFIER LETTER SMALL T WITH PALATAL HOOK
unicode_nfkc_cf(0x1DB6, [0x0289]).						% Lm       MODIFIER LETTER SMALL U BAR
unicode_nfkc_cf(0x1DB7, [0x028A]).						% Lm       MODIFIER LETTER SMALL UPSILON
unicode_nfkc_cf(0x1DB8, [0x1D1C]).						% Lm       MODIFIER LETTER SMALL CAPITAL U
unicode_nfkc_cf(0x1DB9, [0x028B]).						% Lm       MODIFIER LETTER SMALL V WITH HOOK
unicode_nfkc_cf(0x1DBA, [0x028C]).						% Lm       MODIFIER LETTER SMALL TURNED V
unicode_nfkc_cf(0x1DBB, [0x007A]).						% Lm       MODIFIER LETTER SMALL Z
unicode_nfkc_cf(0x1DBC, [0x0290]).						% Lm       MODIFIER LETTER SMALL Z WITH RETROFLEX HOOK
unicode_nfkc_cf(0x1DBD, [0x0291]).						% Lm       MODIFIER LETTER SMALL Z WITH CURL
unicode_nfkc_cf(0x1DBE, [0x0292]).						% Lm       MODIFIER LETTER SMALL EZH
unicode_nfkc_cf(0x1DBF, [0x03B8]).						% Lm       MODIFIER LETTER SMALL THETA
unicode_nfkc_cf(0x1E00, [0x1E01]).						% L&       LATIN CAPITAL LETTER A WITH RING BELOW
unicode_nfkc_cf(0x1E02, [0x1E03]).						% L&       LATIN CAPITAL LETTER B WITH DOT ABOVE
unicode_nfkc_cf(0x1E04, [0x1E05]).						% L&       LATIN CAPITAL LETTER B WITH DOT BELOW
unicode_nfkc_cf(0x1E06, [0x1E07]).						% L&       LATIN CAPITAL LETTER B WITH LINE BELOW
unicode_nfkc_cf(0x1E08, [0x1E09]).						% L&       LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
unicode_nfkc_cf(0x1E0A, [0x1E0B]).						% L&       LATIN CAPITAL LETTER D WITH DOT ABOVE
unicode_nfkc_cf(0x1E0C, [0x1E0D]).						% L&       LATIN CAPITAL LETTER D WITH DOT BELOW
unicode_nfkc_cf(0x1E0E, [0x1E0F]).						% L&       LATIN CAPITAL LETTER D WITH LINE BELOW
unicode_nfkc_cf(0x1E10, [0x1E11]).						% L&       LATIN CAPITAL LETTER D WITH CEDILLA
unicode_nfkc_cf(0x1E12, [0x1E13]).						% L&       LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
unicode_nfkc_cf(0x1E14, [0x1E15]).						% L&       LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
unicode_nfkc_cf(0x1E16, [0x1E17]).						% L&       LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
unicode_nfkc_cf(0x1E18, [0x1E19]).						% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
unicode_nfkc_cf(0x1E1A, [0x1E1B]).						% L&       LATIN CAPITAL LETTER E WITH TILDE BELOW
unicode_nfkc_cf(0x1E1C, [0x1E1D]).						% L&       LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
unicode_nfkc_cf(0x1E1E, [0x1E1F]).						% L&       LATIN CAPITAL LETTER F WITH DOT ABOVE
unicode_nfkc_cf(0x1E20, [0x1E21]).						% L&       LATIN CAPITAL LETTER G WITH MACRON
unicode_nfkc_cf(0x1E22, [0x1E23]).						% L&       LATIN CAPITAL LETTER H WITH DOT ABOVE
unicode_nfkc_cf(0x1E24, [0x1E25]).						% L&       LATIN CAPITAL LETTER H WITH DOT BELOW
unicode_nfkc_cf(0x1E26, [0x1E27]).						% L&       LATIN CAPITAL LETTER H WITH DIAERESIS
unicode_nfkc_cf(0x1E28, [0x1E29]).						% L&       LATIN CAPITAL LETTER H WITH CEDILLA
unicode_nfkc_cf(0x1E2A, [0x1E2B]).						% L&       LATIN CAPITAL LETTER H WITH BREVE BELOW
unicode_nfkc_cf(0x1E2C, [0x1E2D]).						% L&       LATIN CAPITAL LETTER I WITH TILDE BELOW
unicode_nfkc_cf(0x1E2E, [0x1E2F]).						% L&       LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
unicode_nfkc_cf(0x1E30, [0x1E31]).						% L&       LATIN CAPITAL LETTER K WITH ACUTE
unicode_nfkc_cf(0x1E32, [0x1E33]).						% L&       LATIN CAPITAL LETTER K WITH DOT BELOW
unicode_nfkc_cf(0x1E34, [0x1E35]).						% L&       LATIN CAPITAL LETTER K WITH LINE BELOW
unicode_nfkc_cf(0x1E36, [0x1E37]).						% L&       LATIN CAPITAL LETTER L WITH DOT BELOW
unicode_nfkc_cf(0x1E38, [0x1E39]).						% L&       LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
unicode_nfkc_cf(0x1E3A, [0x1E3B]).						% L&       LATIN CAPITAL LETTER L WITH LINE BELOW
unicode_nfkc_cf(0x1E3C, [0x1E3D]).						% L&       LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
unicode_nfkc_cf(0x1E3E, [0x1E3F]).						% L&       LATIN CAPITAL LETTER M WITH ACUTE
unicode_nfkc_cf(0x1E40, [0x1E41]).						% L&       LATIN CAPITAL LETTER M WITH DOT ABOVE
unicode_nfkc_cf(0x1E42, [0x1E43]).						% L&       LATIN CAPITAL LETTER M WITH DOT BELOW
unicode_nfkc_cf(0x1E44, [0x1E45]).						% L&       LATIN CAPITAL LETTER N WITH DOT ABOVE
unicode_nfkc_cf(0x1E46, [0x1E47]).						% L&       LATIN CAPITAL LETTER N WITH DOT BELOW
unicode_nfkc_cf(0x1E48, [0x1E49]).						% L&       LATIN CAPITAL LETTER N WITH LINE BELOW
unicode_nfkc_cf(0x1E4A, [0x1E4B]).						% L&       LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
unicode_nfkc_cf(0x1E4C, [0x1E4D]).						% L&       LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
unicode_nfkc_cf(0x1E4E, [0x1E4F]).						% L&       LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
unicode_nfkc_cf(0x1E50, [0x1E51]).						% L&       LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
unicode_nfkc_cf(0x1E52, [0x1E53]).						% L&       LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
unicode_nfkc_cf(0x1E54, [0x1E55]).						% L&       LATIN CAPITAL LETTER P WITH ACUTE
unicode_nfkc_cf(0x1E56, [0x1E57]).						% L&       LATIN CAPITAL LETTER P WITH DOT ABOVE
unicode_nfkc_cf(0x1E58, [0x1E59]).						% L&       LATIN CAPITAL LETTER R WITH DOT ABOVE
unicode_nfkc_cf(0x1E5A, [0x1E5B]).						% L&       LATIN CAPITAL LETTER R WITH DOT BELOW
unicode_nfkc_cf(0x1E5C, [0x1E5D]).						% L&       LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
unicode_nfkc_cf(0x1E5E, [0x1E5F]).						% L&       LATIN CAPITAL LETTER R WITH LINE BELOW
unicode_nfkc_cf(0x1E60, [0x1E61]).						% L&       LATIN CAPITAL LETTER S WITH DOT ABOVE
unicode_nfkc_cf(0x1E62, [0x1E63]).						% L&       LATIN CAPITAL LETTER S WITH DOT BELOW
unicode_nfkc_cf(0x1E64, [0x1E65]).						% L&       LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
unicode_nfkc_cf(0x1E66, [0x1E67]).						% L&       LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
unicode_nfkc_cf(0x1E68, [0x1E69]).						% L&       LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
unicode_nfkc_cf(0x1E6A, [0x1E6B]).						% L&       LATIN CAPITAL LETTER T WITH DOT ABOVE
unicode_nfkc_cf(0x1E6C, [0x1E6D]).						% L&       LATIN CAPITAL LETTER T WITH DOT BELOW
unicode_nfkc_cf(0x1E6E, [0x1E6F]).						% L&       LATIN CAPITAL LETTER T WITH LINE BELOW
unicode_nfkc_cf(0x1E70, [0x1E71]).						% L&       LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
unicode_nfkc_cf(0x1E72, [0x1E73]).						% L&       LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
unicode_nfkc_cf(0x1E74, [0x1E75]).						% L&       LATIN CAPITAL LETTER U WITH TILDE BELOW
unicode_nfkc_cf(0x1E76, [0x1E77]).						% L&       LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
unicode_nfkc_cf(0x1E78, [0x1E79]).						% L&       LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
unicode_nfkc_cf(0x1E7A, [0x1E7B]).						% L&       LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
unicode_nfkc_cf(0x1E7C, [0x1E7D]).						% L&       LATIN CAPITAL LETTER V WITH TILDE
unicode_nfkc_cf(0x1E7E, [0x1E7F]).						% L&       LATIN CAPITAL LETTER V WITH DOT BELOW
unicode_nfkc_cf(0x1E80, [0x1E81]).						% L&       LATIN CAPITAL LETTER W WITH GRAVE
unicode_nfkc_cf(0x1E82, [0x1E83]).						% L&       LATIN CAPITAL LETTER W WITH ACUTE
unicode_nfkc_cf(0x1E84, [0x1E85]).						% L&       LATIN CAPITAL LETTER W WITH DIAERESIS
unicode_nfkc_cf(0x1E86, [0x1E87]).						% L&       LATIN CAPITAL LETTER W WITH DOT ABOVE
unicode_nfkc_cf(0x1E88, [0x1E89]).						% L&       LATIN CAPITAL LETTER W WITH DOT BELOW
unicode_nfkc_cf(0x1E8A, [0x1E8B]).						% L&       LATIN CAPITAL LETTER X WITH DOT ABOVE
unicode_nfkc_cf(0x1E8C, [0x1E8D]).						% L&       LATIN CAPITAL LETTER X WITH DIAERESIS
unicode_nfkc_cf(0x1E8E, [0x1E8F]).						% L&       LATIN CAPITAL LETTER Y WITH DOT ABOVE
unicode_nfkc_cf(0x1E90, [0x1E91]).						% L&       LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
unicode_nfkc_cf(0x1E92, [0x1E93]).						% L&       LATIN CAPITAL LETTER Z WITH DOT BELOW
unicode_nfkc_cf(0x1E94, [0x1E95]).						% L&       LATIN CAPITAL LETTER Z WITH LINE BELOW
unicode_nfkc_cf(0x1E9A, [0x0061, 0x02BE]).				% L&       LATIN SMALL LETTER A WITH RIGHT HALF RING
unicode_nfkc_cf(0x1E9B, [0x1E61]).						% L&       LATIN SMALL LETTER LONG S WITH DOT ABOVE
unicode_nfkc_cf(0x1E9E, [0x0073, 0x0073]).				% L&       LATIN CAPITAL LETTER SHARP S
unicode_nfkc_cf(0x1EA0, [0x1EA1]).						% L&       LATIN CAPITAL LETTER A WITH DOT BELOW
unicode_nfkc_cf(0x1EA2, [0x1EA3]).						% L&       LATIN CAPITAL LETTER A WITH HOOK ABOVE
unicode_nfkc_cf(0x1EA4, [0x1EA5]).						% L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
unicode_nfkc_cf(0x1EA6, [0x1EA7]).						% L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
unicode_nfkc_cf(0x1EA8, [0x1EA9]).						% L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
unicode_nfkc_cf(0x1EAA, [0x1EAB]).						% L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
unicode_nfkc_cf(0x1EAC, [0x1EAD]).						% L&       LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
unicode_nfkc_cf(0x1EAE, [0x1EAF]).						% L&       LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
unicode_nfkc_cf(0x1EB0, [0x1EB1]).						% L&       LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
unicode_nfkc_cf(0x1EB2, [0x1EB3]).						% L&       LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
unicode_nfkc_cf(0x1EB4, [0x1EB5]).						% L&       LATIN CAPITAL LETTER A WITH BREVE AND TILDE
unicode_nfkc_cf(0x1EB6, [0x1EB7]).						% L&       LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
unicode_nfkc_cf(0x1EB8, [0x1EB9]).						% L&       LATIN CAPITAL LETTER E WITH DOT BELOW
unicode_nfkc_cf(0x1EBA, [0x1EBB]).						% L&       LATIN CAPITAL LETTER E WITH HOOK ABOVE
unicode_nfkc_cf(0x1EBC, [0x1EBD]).						% L&       LATIN CAPITAL LETTER E WITH TILDE
unicode_nfkc_cf(0x1EBE, [0x1EBF]).						% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
unicode_nfkc_cf(0x1EC0, [0x1EC1]).						% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
unicode_nfkc_cf(0x1EC2, [0x1EC3]).						% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
unicode_nfkc_cf(0x1EC4, [0x1EC5]).						% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
unicode_nfkc_cf(0x1EC6, [0x1EC7]).						% L&       LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
unicode_nfkc_cf(0x1EC8, [0x1EC9]).						% L&       LATIN CAPITAL LETTER I WITH HOOK ABOVE
unicode_nfkc_cf(0x1ECA, [0x1ECB]).						% L&       LATIN CAPITAL LETTER I WITH DOT BELOW
unicode_nfkc_cf(0x1ECC, [0x1ECD]).						% L&       LATIN CAPITAL LETTER O WITH DOT BELOW
unicode_nfkc_cf(0x1ECE, [0x1ECF]).						% L&       LATIN CAPITAL LETTER O WITH HOOK ABOVE
unicode_nfkc_cf(0x1ED0, [0x1ED1]).						% L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
unicode_nfkc_cf(0x1ED2, [0x1ED3]).						% L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
unicode_nfkc_cf(0x1ED4, [0x1ED5]).						% L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
unicode_nfkc_cf(0x1ED6, [0x1ED7]).						% L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
unicode_nfkc_cf(0x1ED8, [0x1ED9]).						% L&       LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
unicode_nfkc_cf(0x1EDA, [0x1EDB]).						% L&       LATIN CAPITAL LETTER O WITH HORN AND ACUTE
unicode_nfkc_cf(0x1EDC, [0x1EDD]).						% L&       LATIN CAPITAL LETTER O WITH HORN AND GRAVE
unicode_nfkc_cf(0x1EDE, [0x1EDF]).						% L&       LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
unicode_nfkc_cf(0x1EE0, [0x1EE1]).						% L&       LATIN CAPITAL LETTER O WITH HORN AND TILDE
unicode_nfkc_cf(0x1EE2, [0x1EE3]).						% L&       LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
unicode_nfkc_cf(0x1EE4, [0x1EE5]).						% L&       LATIN CAPITAL LETTER U WITH DOT BELOW
unicode_nfkc_cf(0x1EE6, [0x1EE7]).						% L&       LATIN CAPITAL LETTER U WITH HOOK ABOVE
unicode_nfkc_cf(0x1EE8, [0x1EE9]).						% L&       LATIN CAPITAL LETTER U WITH HORN AND ACUTE
unicode_nfkc_cf(0x1EEA, [0x1EEB]).						% L&       LATIN CAPITAL LETTER U WITH HORN AND GRAVE
unicode_nfkc_cf(0x1EEC, [0x1EED]).						% L&       LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
unicode_nfkc_cf(0x1EEE, [0x1EEF]).						% L&       LATIN CAPITAL LETTER U WITH HORN AND TILDE
unicode_nfkc_cf(0x1EF0, [0x1EF1]).						% L&       LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
unicode_nfkc_cf(0x1EF2, [0x1EF3]).						% L&       LATIN CAPITAL LETTER Y WITH GRAVE
unicode_nfkc_cf(0x1EF4, [0x1EF5]).						% L&       LATIN CAPITAL LETTER Y WITH DOT BELOW
unicode_nfkc_cf(0x1EF6, [0x1EF7]).						% L&       LATIN CAPITAL LETTER Y WITH HOOK ABOVE
unicode_nfkc_cf(0x1EF8, [0x1EF9]).						% L&       LATIN CAPITAL LETTER Y WITH TILDE
unicode_nfkc_cf(0x1EFA, [0x1EFB]).						% L&       LATIN CAPITAL LETTER MIDDLE-WELSH LL
unicode_nfkc_cf(0x1EFC, [0x1EFD]).						% L&       LATIN CAPITAL LETTER MIDDLE-WELSH V
unicode_nfkc_cf(0x1EFE, [0x1EFF]).						% L&       LATIN CAPITAL LETTER Y WITH LOOP
unicode_nfkc_cf(0x1F08, [0x1F00]).						% L&       GREEK CAPITAL LETTER ALPHA WITH PSILI
unicode_nfkc_cf(0x1F09, [0x1F01]).						% L&       GREEK CAPITAL LETTER ALPHA WITH DASIA
unicode_nfkc_cf(0x1F0A, [0x1F02]).						% L&       GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
unicode_nfkc_cf(0x1F0B, [0x1F03]).						% L&       GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
unicode_nfkc_cf(0x1F0C, [0x1F04]).						% L&       GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
unicode_nfkc_cf(0x1F0D, [0x1F05]).						% L&       GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
unicode_nfkc_cf(0x1F0E, [0x1F06]).						% L&       GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
unicode_nfkc_cf(0x1F0F, [0x1F07]).						% L&       GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
unicode_nfkc_cf(0x1F18, [0x1F10]).						% L&       GREEK CAPITAL LETTER EPSILON WITH PSILI
unicode_nfkc_cf(0x1F19, [0x1F11]).						% L&       GREEK CAPITAL LETTER EPSILON WITH DASIA
unicode_nfkc_cf(0x1F1A, [0x1F12]).						% L&       GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
unicode_nfkc_cf(0x1F1B, [0x1F13]).						% L&       GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
unicode_nfkc_cf(0x1F1C, [0x1F14]).						% L&       GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
unicode_nfkc_cf(0x1F1D, [0x1F15]).						% L&       GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_nfkc_cf(0x1F28, [0x1F20]).						% L&       GREEK CAPITAL LETTER ETA WITH PSILI
unicode_nfkc_cf(0x1F29, [0x1F21]).						% L&       GREEK CAPITAL LETTER ETA WITH DASIA
unicode_nfkc_cf(0x1F2A, [0x1F22]).						% L&       GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
unicode_nfkc_cf(0x1F2B, [0x1F23]).						% L&       GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
unicode_nfkc_cf(0x1F2C, [0x1F24]).						% L&       GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
unicode_nfkc_cf(0x1F2D, [0x1F25]).						% L&       GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
unicode_nfkc_cf(0x1F2E, [0x1F26]).						% L&       GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
unicode_nfkc_cf(0x1F2F, [0x1F27]).						% L&       GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
unicode_nfkc_cf(0x1F38, [0x1F30]).						% L&       GREEK CAPITAL LETTER IOTA WITH PSILI
unicode_nfkc_cf(0x1F39, [0x1F31]).						% L&       GREEK CAPITAL LETTER IOTA WITH DASIA
unicode_nfkc_cf(0x1F3A, [0x1F32]).						% L&       GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
unicode_nfkc_cf(0x1F3B, [0x1F33]).						% L&       GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
unicode_nfkc_cf(0x1F3C, [0x1F34]).						% L&       GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
unicode_nfkc_cf(0x1F3D, [0x1F35]).						% L&       GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
unicode_nfkc_cf(0x1F3E, [0x1F36]).						% L&       GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
unicode_nfkc_cf(0x1F3F, [0x1F37]).						% L&       GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
unicode_nfkc_cf(0x1F48, [0x1F40]).						% L&       GREEK CAPITAL LETTER OMICRON WITH PSILI
unicode_nfkc_cf(0x1F49, [0x1F41]).						% L&       GREEK CAPITAL LETTER OMICRON WITH DASIA
unicode_nfkc_cf(0x1F4A, [0x1F42]).						% L&       GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
unicode_nfkc_cf(0x1F4B, [0x1F43]).						% L&       GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
unicode_nfkc_cf(0x1F4C, [0x1F44]).						% L&       GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
unicode_nfkc_cf(0x1F4D, [0x1F45]).						% L&       GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_nfkc_cf(0x1F59, [0x1F51]).						% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_nfkc_cf(0x1F5B, [0x1F53]).						% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_nfkc_cf(0x1F5D, [0x1F55]).						% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_nfkc_cf(0x1F5F, [0x1F57]).						% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_nfkc_cf(0x1F68, [0x1F60]).						% L&       GREEK CAPITAL LETTER OMEGA WITH PSILI
unicode_nfkc_cf(0x1F69, [0x1F61]).						% L&       GREEK CAPITAL LETTER OMEGA WITH DASIA
unicode_nfkc_cf(0x1F6A, [0x1F62]).						% L&       GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
unicode_nfkc_cf(0x1F6B, [0x1F63]).						% L&       GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
unicode_nfkc_cf(0x1F6C, [0x1F64]).						% L&       GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
unicode_nfkc_cf(0x1F6D, [0x1F65]).						% L&       GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
unicode_nfkc_cf(0x1F6E, [0x1F66]).						% L&       GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
unicode_nfkc_cf(0x1F6F, [0x1F67]).						% L&       GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
unicode_nfkc_cf(0x1F71, [0x03AC]).						% L&       GREEK SMALL LETTER ALPHA WITH OXIA
unicode_nfkc_cf(0x1F73, [0x03AD]).						% L&       GREEK SMALL LETTER EPSILON WITH OXIA
unicode_nfkc_cf(0x1F75, [0x03AE]).						% L&       GREEK SMALL LETTER ETA WITH OXIA
unicode_nfkc_cf(0x1F77, [0x03AF]).						% L&       GREEK SMALL LETTER IOTA WITH OXIA
unicode_nfkc_cf(0x1F79, [0x03CC]).						% L&       GREEK SMALL LETTER OMICRON WITH OXIA
unicode_nfkc_cf(0x1F7B, [0x03CD]).						% L&       GREEK SMALL LETTER UPSILON WITH OXIA
unicode_nfkc_cf(0x1F7D, [0x03CE]).						% L&       GREEK SMALL LETTER OMEGA WITH OXIA
unicode_nfkc_cf(0x1F80, [0x1F00, 0x03B9]).				% L&       GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F81, [0x1F01, 0x03B9]).				% L&       GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F82, [0x1F02, 0x03B9]).				% L&       GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F83, [0x1F03, 0x03B9]).				% L&       GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F84, [0x1F04, 0x03B9]).				% L&       GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F85, [0x1F05, 0x03B9]).				% L&       GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F86, [0x1F06, 0x03B9]).				% L&       GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F87, [0x1F07, 0x03B9]).				% L&       GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F88, [0x1F00, 0x03B9]).				% L&       GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F89, [0x1F01, 0x03B9]).				% L&       GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F8A, [0x1F02, 0x03B9]).				% L&       GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F8B, [0x1F03, 0x03B9]).				% L&       GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F8C, [0x1F04, 0x03B9]).				% L&       GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F8D, [0x1F05, 0x03B9]).				% L&       GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F8E, [0x1F06, 0x03B9]).				% L&       GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F8F, [0x1F07, 0x03B9]).				% L&       GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F90, [0x1F20, 0x03B9]).				% L&       GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F91, [0x1F21, 0x03B9]).				% L&       GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F92, [0x1F22, 0x03B9]).				% L&       GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F93, [0x1F23, 0x03B9]).				% L&       GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F94, [0x1F24, 0x03B9]).				% L&       GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F95, [0x1F25, 0x03B9]).				% L&       GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F96, [0x1F26, 0x03B9]).				% L&       GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F97, [0x1F27, 0x03B9]).				% L&       GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1F98, [0x1F20, 0x03B9]).				% L&       GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F99, [0x1F21, 0x03B9]).				% L&       GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F9A, [0x1F22, 0x03B9]).				% L&       GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F9B, [0x1F23, 0x03B9]).				% L&       GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F9C, [0x1F24, 0x03B9]).				% L&       GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F9D, [0x1F25, 0x03B9]).				% L&       GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F9E, [0x1F26, 0x03B9]).				% L&       GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1F9F, [0x1F27, 0x03B9]).				% L&       GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1FA0, [0x1F60, 0x03B9]).				% L&       GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FA1, [0x1F61, 0x03B9]).				% L&       GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FA2, [0x1F62, 0x03B9]).				% L&       GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FA3, [0x1F63, 0x03B9]).				% L&       GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FA4, [0x1F64, 0x03B9]).				% L&       GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FA5, [0x1F65, 0x03B9]).				% L&       GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FA6, [0x1F66, 0x03B9]).				% L&       GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FA7, [0x1F67, 0x03B9]).				% L&       GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FA8, [0x1F60, 0x03B9]).				% L&       GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1FA9, [0x1F61, 0x03B9]).				% L&       GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1FAA, [0x1F62, 0x03B9]).				% L&       GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1FAB, [0x1F63, 0x03B9]).				% L&       GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1FAC, [0x1F64, 0x03B9]).				% L&       GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1FAD, [0x1F65, 0x03B9]).				% L&       GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1FAE, [0x1F66, 0x03B9]).				% L&       GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1FAF, [0x1F67, 0x03B9]).				% L&       GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_nfkc_cf(0x1FB2, [0x1F70, 0x03B9]).				% L&       GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FB3, [0x03B1, 0x03B9]).				% L&       GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
unicode_nfkc_cf(0x1FB4, [0x03AC, 0x03B9]).				% L&       GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FB7, [0x1FB6, 0x03B9]).				% L&       GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FB8, [0x1FB0]).						% L&       GREEK CAPITAL LETTER ALPHA WITH VRACHY
unicode_nfkc_cf(0x1FB9, [0x1FB1]).						% L&       GREEK CAPITAL LETTER ALPHA WITH MACRON
unicode_nfkc_cf(0x1FBA, [0x1F70]).						% L&       GREEK CAPITAL LETTER ALPHA WITH VARIA
unicode_nfkc_cf(0x1FBB, [0x03AC]).						% L&       GREEK CAPITAL LETTER ALPHA WITH OXIA
unicode_nfkc_cf(0x1FBC, [0x03B1, 0x03B9]).				% L&       GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_nfkc_cf(0x1FBD, [0x0020, 0x0313]).				% Sk       GREEK KORONIS
unicode_nfkc_cf(0x1FBE, [0x03B9]).						% L&       GREEK PROSGEGRAMMENI
unicode_nfkc_cf(0x1FBF, [0x0020, 0x0313]).				% Sk       GREEK PSILI
unicode_nfkc_cf(0x1FC0, [0x0020, 0x0342]).				% Sk       GREEK PERISPOMENI
unicode_nfkc_cf(0x1FC1, [0x0020, 0x0308, 0x0342]).		% Sk       GREEK DIALYTIKA AND PERISPOMENI
unicode_nfkc_cf(0x1FC2, [0x1F74, 0x03B9]).				% L&       GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FC3, [0x03B7, 0x03B9]).				% L&       GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
unicode_nfkc_cf(0x1FC4, [0x03AE, 0x03B9]).				% L&       GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FC7, [0x1FC6, 0x03B9]).				% L&       GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FC8, [0x1F72]).						% L&       GREEK CAPITAL LETTER EPSILON WITH VARIA
unicode_nfkc_cf(0x1FC9, [0x03AD]).						% L&       GREEK CAPITAL LETTER EPSILON WITH OXIA
unicode_nfkc_cf(0x1FCA, [0x1F74]).						% L&       GREEK CAPITAL LETTER ETA WITH VARIA
unicode_nfkc_cf(0x1FCB, [0x03AE]).						% L&       GREEK CAPITAL LETTER ETA WITH OXIA
unicode_nfkc_cf(0x1FCC, [0x03B7, 0x03B9]).				% L&       GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_nfkc_cf(0x1FCD, [0x0020, 0x0313, 0x0300]).		% Sk       GREEK PSILI AND VARIA
unicode_nfkc_cf(0x1FCE, [0x0020, 0x0313, 0x0301]).		% Sk       GREEK PSILI AND OXIA
unicode_nfkc_cf(0x1FCF, [0x0020, 0x0313, 0x0342]).		% Sk       GREEK PSILI AND PERISPOMENI
unicode_nfkc_cf(0x1FD3, [0x0390]).						% L&       GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_nfkc_cf(0x1FD8, [0x1FD0]).						% L&       GREEK CAPITAL LETTER IOTA WITH VRACHY
unicode_nfkc_cf(0x1FD9, [0x1FD1]).						% L&       GREEK CAPITAL LETTER IOTA WITH MACRON
unicode_nfkc_cf(0x1FDA, [0x1F76]).						% L&       GREEK CAPITAL LETTER IOTA WITH VARIA
unicode_nfkc_cf(0x1FDB, [0x03AF]).						% L&       GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_nfkc_cf(0x1FDD, [0x0020, 0x0314, 0x0300]).		% Sk       GREEK DASIA AND VARIA
unicode_nfkc_cf(0x1FDE, [0x0020, 0x0314, 0x0301]).		% Sk       GREEK DASIA AND OXIA
unicode_nfkc_cf(0x1FDF, [0x0020, 0x0314, 0x0342]).		% Sk       GREEK DASIA AND PERISPOMENI
unicode_nfkc_cf(0x1FE3, [0x03B0]).						% L&       GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
unicode_nfkc_cf(0x1FE8, [0x1FE0]).						% L&       GREEK CAPITAL LETTER UPSILON WITH VRACHY
unicode_nfkc_cf(0x1FE9, [0x1FE1]).						% L&       GREEK CAPITAL LETTER UPSILON WITH MACRON
unicode_nfkc_cf(0x1FEA, [0x1F7A]).						% L&       GREEK CAPITAL LETTER UPSILON WITH VARIA
unicode_nfkc_cf(0x1FEB, [0x03CD]).						% L&       GREEK CAPITAL LETTER UPSILON WITH OXIA
unicode_nfkc_cf(0x1FEC, [0x1FE5]).						% L&       GREEK CAPITAL LETTER RHO WITH DASIA
unicode_nfkc_cf(0x1FED, [0x0020, 0x0308, 0x0300]).		% Sk       GREEK DIALYTIKA AND VARIA
unicode_nfkc_cf(0x1FEE, [0x0020, 0x0308, 0x0301]).		% Sk       GREEK DIALYTIKA AND OXIA
unicode_nfkc_cf(0x1FEF, [0x0060]).						% Sk       GREEK VARIA
unicode_nfkc_cf(0x1FF2, [0x1F7C, 0x03B9]).				% L&       GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FF3, [0x03C9, 0x03B9]).				% L&       GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
unicode_nfkc_cf(0x1FF4, [0x03CE, 0x03B9]).				% L&       GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FF7, [0x1FF6, 0x03B9]).				% L&       GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_nfkc_cf(0x1FF8, [0x1F78]).						% L&       GREEK CAPITAL LETTER OMICRON WITH VARIA
unicode_nfkc_cf(0x1FF9, [0x03CC]).						% L&       GREEK CAPITAL LETTER OMICRON WITH OXIA
unicode_nfkc_cf(0x1FFA, [0x1F7C]).						% L&       GREEK CAPITAL LETTER OMEGA WITH VARIA
unicode_nfkc_cf(0x1FFB, [0x03CE]).						% L&       GREEK CAPITAL LETTER OMEGA WITH OXIA
unicode_nfkc_cf(0x1FFC, [0x03C9, 0x03B9]).				% L&       GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_nfkc_cf(0x1FFD, [0x0020, 0x0301]).				% Sk       GREEK OXIA
unicode_nfkc_cf(0x1FFE, [0x0020, 0x0314]).				% Sk       GREEK DASIA
unicode_nfkc_cf(0x2000, [0x0020]).						% Zs       EN QUAD..
unicode_nfkc_cf(0x2001, [0x0020]).						% Zs       ..
unicode_nfkc_cf(0x2002, [0x0020]).						% Zs       ..
unicode_nfkc_cf(0x2003, [0x0020]).						% Zs       ..
unicode_nfkc_cf(0x2004, [0x0020]).						% Zs       ..
unicode_nfkc_cf(0x2005, [0x0020]).						% Zs       ..
unicode_nfkc_cf(0x2006, [0x0020]).						% Zs       ..
unicode_nfkc_cf(0x2007, [0x0020]).						% Zs       ..
unicode_nfkc_cf(0x2008, [0x0020]).						% Zs       ..
unicode_nfkc_cf(0x2009, [0x0020]).						% Zs       ..
unicode_nfkc_cf(0x200A, [0x0020]).						% Zs       ..HAIR SPACE
%unicode_nfkc_cf(0x200B, 0x200F    ; NFKC_CF;       	         # Cf   [5] ZERO WIDTH SPACE..RIGHT-TO-LEFT MARK
unicode_nfkc_cf(0x2011, [0x2010]).						% Pd       NON-BREAKING HYPHEN
unicode_nfkc_cf(0x2017, [0x0020, 0x0333]).				% Po       DOUBLE LOW LINE
unicode_nfkc_cf(0x2024, [0x002E]).						% Po       ONE DOT LEADER
unicode_nfkc_cf(0x2025, [0x002E, 0x002E]).				% Po       TWO DOT LEADER
unicode_nfkc_cf(0x2026, [0x002E, 0x002E, 0x002E]).		% Po       HORIZONTAL ELLIPSIS
%unicode_nfkc_cf(0x202A, 0x202E    ; NFKC_CF;       	         # Cf   [5] LEFT-TO-RIGHT EMBEDDING..RIGHT-TO-LEFT OVERRIDE
unicode_nfkc_cf(0x202F, [0x0020]).						% Zs       NARROW NO-BREAK SPACE
unicode_nfkc_cf(0x2033, [0x2032, 0x2032]).				% Po       DOUBLE PRIME
unicode_nfkc_cf(0x2034, [0x2032, 0x2032, 0x2032]).		% Po       TRIPLE PRIME
unicode_nfkc_cf(0x2036, [0x2035, 0x2035]).				% Po       REVERSED DOUBLE PRIME
unicode_nfkc_cf(0x2037, [0x2035, 0x2035, 0x2035]).		% Po       REVERSED TRIPLE PRIME
unicode_nfkc_cf(0x203C, [0x0021, 0x0021]).				% Po       DOUBLE EXCLAMATION MARK
unicode_nfkc_cf(0x203E, [0x0020, 0x0305]).				% Po       OVERLINE
unicode_nfkc_cf(0x2047, [0x003F, 0x003F]).				% Po       DOUBLE QUESTION MARK
unicode_nfkc_cf(0x2048, [0x003F, 0x0021]).				% Po       QUESTION EXCLAMATION MARK
unicode_nfkc_cf(0x2049, [0x0021, 0x003F]).				% Po       EXCLAMATION QUESTION MARK
unicode_nfkc_cf(0x2057, [0x2032, 0x2032, 0x2032, 0x2032]).	% Po   QUADRUPLE PRIME
unicode_nfkc_cf(0x205F, [0x0020]).						% Zs       MEDIUM MATHEMATICAL SPACE
%unicode_nfkc_cf(0x2060, 0x2064    ; NFKC_CF;       	         # Cf   [5] WORD JOINER..INVISIBLE PLUS
%unicode_nfkc_cf(0x2065, 0x2069    ; NFKC_CF;       	         # Cn   [5] <reserved-2065>..<reserved-2069>
%unicode_nfkc_cf(0x206A, 0x206F    ; NFKC_CF;       	         # Cf   [6] INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES
unicode_nfkc_cf(0x2070, [0x0030]).						% No       SUPERSCRIPT ZERO
unicode_nfkc_cf(0x2071, [0x0069]).						% Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_nfkc_cf(0x2074, [0x0034]).						% No       SUPERSCRIPT FOUR
unicode_nfkc_cf(0x2075, [0x0035]).						% No       SUPERSCRIPT FIVE
unicode_nfkc_cf(0x2076, [0x0036]).						% No       SUPERSCRIPT SIX
unicode_nfkc_cf(0x2077, [0x0037]).						% No       SUPERSCRIPT SEVEN
unicode_nfkc_cf(0x2078, [0x0038]).						% No       SUPERSCRIPT EIGHT
unicode_nfkc_cf(0x2079, [0x0039]).						% No       SUPERSCRIPT NINE
unicode_nfkc_cf(0x207A, [0x002B]).						% Sm       SUPERSCRIPT PLUS SIGN
unicode_nfkc_cf(0x207B, [0x2212]).						% Sm       SUPERSCRIPT MINUS
unicode_nfkc_cf(0x207C, [0x003D]).						% Sm       SUPERSCRIPT EQUALS SIGN
unicode_nfkc_cf(0x207D, [0x0028]).						% Ps       SUPERSCRIPT LEFT PARENTHESIS
unicode_nfkc_cf(0x207E, [0x0029]).						% Pe       SUPERSCRIPT RIGHT PARENTHESIS
unicode_nfkc_cf(0x207F, [0x006E]).						% Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_nfkc_cf(0x2080, [0x0030]).						% No       SUBSCRIPT ZERO
unicode_nfkc_cf(0x2081, [0x0031]).						% No       SUBSCRIPT ONE
unicode_nfkc_cf(0x2082, [0x0032]).						% No       SUBSCRIPT TWO
unicode_nfkc_cf(0x2083, [0x0033]).						% No       SUBSCRIPT THREE
unicode_nfkc_cf(0x2084, [0x0034]).						% No       SUBSCRIPT FOUR
unicode_nfkc_cf(0x2085, [0x0035]).						% No       SUBSCRIPT FIVE
unicode_nfkc_cf(0x2086, [0x0036]).						% No       SUBSCRIPT SIX
unicode_nfkc_cf(0x2087, [0x0037]).						% No       SUBSCRIPT SEVEN
unicode_nfkc_cf(0x2088, [0x0038]).						% No       SUBSCRIPT EIGHT
unicode_nfkc_cf(0x2089, [0x0039]).						% No       SUBSCRIPT NINE
unicode_nfkc_cf(0x208A, [0x002B]).						% Sm       SUBSCRIPT PLUS SIGN
unicode_nfkc_cf(0x208B, [0x2212]).						% Sm       SUBSCRIPT MINUS
unicode_nfkc_cf(0x208C, [0x003D]).						% Sm       SUBSCRIPT EQUALS SIGN
unicode_nfkc_cf(0x208D, [0x0028]).						% Ps       SUBSCRIPT LEFT PARENTHESIS
unicode_nfkc_cf(0x208E, [0x0029]).						% Pe       SUBSCRIPT RIGHT PARENTHESIS
unicode_nfkc_cf(0x2090, [0x0061]).						% Lm       LATIN SUBSCRIPT SMALL LETTER A
unicode_nfkc_cf(0x2091, [0x0065]).						% Lm       LATIN SUBSCRIPT SMALL LETTER E
unicode_nfkc_cf(0x2092, [0x006F]).						% Lm       LATIN SUBSCRIPT SMALL LETTER O
unicode_nfkc_cf(0x2093, [0x0078]).						% Lm       LATIN SUBSCRIPT SMALL LETTER X
unicode_nfkc_cf(0x2094, [0x0259]).						% Lm       LATIN SUBSCRIPT SMALL LETTER SCHWA
unicode_nfkc_cf(0x2095, [0x0068]).						% Lm       LATIN SUBSCRIPT SMALL LETTER H
unicode_nfkc_cf(0x2096, [0x006B]).						% Lm       LATIN SUBSCRIPT SMALL LETTER K
unicode_nfkc_cf(0x2097, [0x006C]).						% Lm       LATIN SUBSCRIPT SMALL LETTER L
unicode_nfkc_cf(0x2098, [0x006D]).						% Lm       LATIN SUBSCRIPT SMALL LETTER M
unicode_nfkc_cf(0x2099, [0x006E]).						% Lm       LATIN SUBSCRIPT SMALL LETTER N
unicode_nfkc_cf(0x209A, [0x0070]).						% Lm       LATIN SUBSCRIPT SMALL LETTER P
unicode_nfkc_cf(0x209B, [0x0073]).						% Lm       LATIN SUBSCRIPT SMALL LETTER S
unicode_nfkc_cf(0x209C, [0x0074]).						% Lm       LATIN SUBSCRIPT SMALL LETTER T
unicode_nfkc_cf(0x20A8, [0x0072, 0x0073]).				% Sc       RUPEE SIGN
unicode_nfkc_cf(0x2100, [0x0061, 0x002F, 0x0063]).		% So       ACCOUNT OF
unicode_nfkc_cf(0x2101, [0x0061, 0x002F, 0x0073]).		% So       ADDRESSED TO THE SUBJECT
unicode_nfkc_cf(0x2102, [0x0063]).						% L&       DOUBLE-STRUCK CAPITAL C
unicode_nfkc_cf(0x2103, [0x00B0, 0x0063]).				% So       DEGREE CELSIUS
unicode_nfkc_cf(0x2105, [0x0063, 0x002F, 0x006F]).		% So       CARE OF
unicode_nfkc_cf(0x2106, [0x0063, 0x002F, 0x0075]).		% So       CADA UNA
unicode_nfkc_cf(0x2107, [0x025B]).						% L&       EULER CONSTANT
unicode_nfkc_cf(0x2109, [0x00B0, 0x0066]).				% So       DEGREE FAHRENHEIT
unicode_nfkc_cf(0x210A, [0x0067]).						% L&       SCRIPT SMALL G
unicode_nfkc_cf(0x210B, [0x0068]).						% L&       SCRIPT CAPITAL H
unicode_nfkc_cf(0x210C, [0x0068]).						% L&       ..
unicode_nfkc_cf(0x210D, [0x0068]).						% L&       ..
unicode_nfkc_cf(0x210E, [0x0068]).						% L&       PLANCK CONSTANT
unicode_nfkc_cf(0x210F, [0x0127]).						% L&       PLANCK CONSTANT OVER TWO PI
unicode_nfkc_cf(0x2110, [0x0069]).						% L&       SCRIPT CAPITAL I
unicode_nfkc_cf(0x2111, [0x0069]).						% L&       BLACK-LETTER CAPITAL I
unicode_nfkc_cf(0x2112, [0x006C]).						% L&       SCRIPT CAPITAL L
unicode_nfkc_cf(0x2113, [0x006C]).						% L&       SCRIPT SMALL L
unicode_nfkc_cf(0x2115, [0x006E]).						% L&       DOUBLE-STRUCK CAPITAL N
unicode_nfkc_cf(0x2116, [0x006E, 0x006F]).				% So       NUMERO SIGN
unicode_nfkc_cf(0x2119, [0x0070]).						% L&       DOUBLE-STRUCK CAPITAL P
unicode_nfkc_cf(0x211A, [0x0071]).						% L&       DOUBLE-STRUCK CAPITAL Q
unicode_nfkc_cf(0x211B, [0x0072]).						% L&       SCRIPT CAPITAL R..DOUBLE-STRUCK CAPITAL R
unicode_nfkc_cf(0x211C, [0x0072]).						% L&       SCRIPT CAPITAL R..DOUBLE-STRUCK CAPITAL R
unicode_nfkc_cf(0x211D, [0x0072]).						% L&       SCRIPT CAPITAL R..DOUBLE-STRUCK CAPITAL R
unicode_nfkc_cf(0x2120, [0x0073, 0x006D]).				% So       SERVICE MARK
unicode_nfkc_cf(0x2121, [0x0074, 0x0065, 0x006C]).		% So       TELEPHONE SIGN
unicode_nfkc_cf(0x2122, [0x0074, 0x006D]).				% So       TRADE MARK SIGN
unicode_nfkc_cf(0x2124, [0x007A]).						% L&       DOUBLE-STRUCK CAPITAL Z
unicode_nfkc_cf(0x2126, [0x03C9]).						% L&       OHM SIGN
unicode_nfkc_cf(0x2128, [0x007A]).						% L&       BLACK-LETTER CAPITAL Z
unicode_nfkc_cf(0x212A, [0x006B]).						% L&       KELVIN SIGN
unicode_nfkc_cf(0x212B, [0x00E5]).						% L&       ANGSTROM SIGN
unicode_nfkc_cf(0x212C, [0x0062]).						% L&       SCRIPT CAPITAL B
unicode_nfkc_cf(0x212D, [0x0063]).						% L&       BLACK-LETTER CAPITAL C
unicode_nfkc_cf(0x212F, [0x0065]).						% L&       SCRIPT SMALL E
unicode_nfkc_cf(0x2130, [0x0065]).						% L&       SCRIPT CAPITAL E
unicode_nfkc_cf(0x2131, [0x0066]).						% L&       SCRIPT CAPITAL F
unicode_nfkc_cf(0x2132, [0x214E]).						% L&       TURNED CAPITAL F
unicode_nfkc_cf(0x2133, [0x006D]).						% L&       SCRIPT CAPITAL M
unicode_nfkc_cf(0x2134, [0x006F]).						% L&       SCRIPT SMALL O
unicode_nfkc_cf(0x2135, [0x05D0]).						% Lo       ALEF SYMBOL
unicode_nfkc_cf(0x2136, [0x05D1]).						% Lo       BET SYMBOL
unicode_nfkc_cf(0x2137, [0x05D2]).						% Lo       GIMEL SYMBOL
unicode_nfkc_cf(0x2138, [0x05D3]).						% Lo       DALET SYMBOL
unicode_nfkc_cf(0x2139, [0x0069]).						% L&       INFORMATION SOURCE
unicode_nfkc_cf(0x213B, [0x0066, 0x0061, 0x0078]).		% So       FACSIMILE SIGN
unicode_nfkc_cf(0x213C, [0x03C0]).						% L&       DOUBLE-STRUCK SMALL PI
unicode_nfkc_cf(0x213D, [0x03B3]).						% L&       DOUBLE-STRUCK SMALL GAMMA
unicode_nfkc_cf(0x213E, [0x03B3]).						% L&       DOUBLE-STRUCK CAPITAL GAMMA
unicode_nfkc_cf(0x213F, [0x03C0]).						% L&       DOUBLE-STRUCK CAPITAL PI
unicode_nfkc_cf(0x2140, [0x2211]).						% Sm       DOUBLE-STRUCK N-ARY SUMMATION
unicode_nfkc_cf(0x2145, [0x0064]).						% L&       DOUBLE-STRUCK ITALIC CAPITAL D
unicode_nfkc_cf(0x2146, [0x0064]).						% L&       DOUBLE-STRUCK ITALIC SMALL D
unicode_nfkc_cf(0x2147, [0x0065]).						% L&       DOUBLE-STRUCK ITALIC SMALL E
unicode_nfkc_cf(0x2148, [0x0069]).						% L&       DOUBLE-STRUCK ITALIC SMALL I
unicode_nfkc_cf(0x2149, [0x006A]).						% L&       DOUBLE-STRUCK ITALIC SMALL J
unicode_nfkc_cf(0x2150, [0x0031, 0x2044, 0x0037]).		% No       VULGAR FRACTION ONE SEVENTH
unicode_nfkc_cf(0x2151, [0x0031, 0x2044, 0x0039]).		% No       VULGAR FRACTION ONE NINTH
unicode_nfkc_cf(0x2152, [0x0031, 0x2044, 0x0031, 0x0030]).	% No   VULGAR FRACTION ONE TENTH
unicode_nfkc_cf(0x2153, [0x0031, 0x2044, 0x0033]).		% No       VULGAR FRACTION ONE THIRD
unicode_nfkc_cf(0x2154, [0x0032, 0x2044, 0x0033]).		% No       VULGAR FRACTION TWO THIRDS
unicode_nfkc_cf(0x2155, [0x0031, 0x2044, 0x0035]).		% No       VULGAR FRACTION ONE FIFTH
unicode_nfkc_cf(0x2156, [0x0032, 0x2044, 0x0035]).		% No       VULGAR FRACTION TWO FIFTHS
unicode_nfkc_cf(0x2157, [0x0033, 0x2044, 0x0035]).		% No       VULGAR FRACTION THREE FIFTHS
unicode_nfkc_cf(0x2158, [0x0034, 0x2044, 0x0035]).		% No       VULGAR FRACTION FOUR FIFTHS
unicode_nfkc_cf(0x2159, [0x0031, 0x2044, 0x0036]).		% No       VULGAR FRACTION ONE SIXTH
unicode_nfkc_cf(0x215A, [0x0035, 0x2044, 0x0036]).		% No       VULGAR FRACTION FIVE SIXTHS
unicode_nfkc_cf(0x215B, [0x0031, 0x2044, 0x0038]).		% No       VULGAR FRACTION ONE EIGHTH
unicode_nfkc_cf(0x215C, [0x0033, 0x2044, 0x0038]).		% No       VULGAR FRACTION THREE EIGHTHS
unicode_nfkc_cf(0x215D, [0x0035, 0x2044, 0x0038]).		% No       VULGAR FRACTION FIVE EIGHTHS
unicode_nfkc_cf(0x215E, [0x0037, 0x2044, 0x0038]).		% No       VULGAR FRACTION SEVEN EIGHTHS
unicode_nfkc_cf(0x215F, [0x0031, 0x2044]).				% No       FRACTION NUMERATOR ONE
unicode_nfkc_cf(0x2160, [0x0069]).						% Nl       ROMAN NUMERAL ONE
unicode_nfkc_cf(0x2161, [0x0069, 0x0069]).				% Nl       ROMAN NUMERAL TWO
unicode_nfkc_cf(0x2162, [0x0069, 0x0069, 0x0069]).		% Nl       ROMAN NUMERAL THREE
unicode_nfkc_cf(0x2163, [0x0069, 0x0076]).				% Nl       ROMAN NUMERAL FOUR
unicode_nfkc_cf(0x2164, [0x0076]).						% Nl       ROMAN NUMERAL FIVE
unicode_nfkc_cf(0x2165, [0x0076, 0x0069]).				% Nl       ROMAN NUMERAL SIX
unicode_nfkc_cf(0x2166, [0x0076, 0x0069, 0x0069]).		% Nl       ROMAN NUMERAL SEVEN
unicode_nfkc_cf(0x2167, [0x0076, 0x0069, 0x0069, 0x0069]).	% Nl   ROMAN NUMERAL EIGHT
unicode_nfkc_cf(0x2168, [0x0069, 0x0078]).				% Nl       ROMAN NUMERAL NINE
unicode_nfkc_cf(0x2169, [0x0078]).						% Nl       ROMAN NUMERAL TEN
unicode_nfkc_cf(0x216A, [0x0078, 0x0069]).   			% Nl       ROMAN NUMERAL ELEVEN
unicode_nfkc_cf(0x216B, [0x0078, 0x0069, 0x0069]).		% Nl       ROMAN NUMERAL TWELVE
unicode_nfkc_cf(0x216C, [0x006C]).						% Nl       ROMAN NUMERAL FIFTY
unicode_nfkc_cf(0x216D, [0x0063]).						% Nl       ROMAN NUMERAL ONE HUNDRED
unicode_nfkc_cf(0x216E, [0x0064]).						% Nl       ROMAN NUMERAL FIVE HUNDRED
unicode_nfkc_cf(0x216F, [0x006D]).						% Nl       ROMAN NUMERAL ONE THOUSAND
unicode_nfkc_cf(0x2170, [0x0069]).						% Nl       SMALL ROMAN NUMERAL ONE
unicode_nfkc_cf(0x2171, [0x0069, 0x0069]).				% Nl       SMALL ROMAN NUMERAL TWO
unicode_nfkc_cf(0x2172, [0x0069, 0x0069, 0x0069]).		% Nl       SMALL ROMAN NUMERAL THREE
unicode_nfkc_cf(0x2173, [0x0069, 0x0076]).				% Nl       SMALL ROMAN NUMERAL FOUR
unicode_nfkc_cf(0x2174, [0x0076]).						% Nl       SMALL ROMAN NUMERAL FIVE
unicode_nfkc_cf(0x2175, [0x0076, 0x0069]).				% Nl       SMALL ROMAN NUMERAL SIX
unicode_nfkc_cf(0x2176, [0x0076, 0x0069, 0x0069]).		% Nl       SMALL ROMAN NUMERAL SEVEN
unicode_nfkc_cf(0x2177, [0x0076, 0x0069, 0x0069, 0x0069]).	% Nl   SMALL ROMAN NUMERAL EIGHT
unicode_nfkc_cf(0x2178, [0x0069, 0x0078]).				% Nl       SMALL ROMAN NUMERAL NINE
unicode_nfkc_cf(0x2179, [0x0078]).						% Nl       SMALL ROMAN NUMERAL TEN
unicode_nfkc_cf(0x217A, [0x0078, 0x0069]).				% Nl       SMALL ROMAN NUMERAL ELEVEN
unicode_nfkc_cf(0x217B, [0x0078, 0x0069, 0x0069]).		% Nl       SMALL ROMAN NUMERAL TWELVE
unicode_nfkc_cf(0x217C, [0x006C]).						% Nl       SMALL ROMAN NUMERAL FIFTY
unicode_nfkc_cf(0x217D, [0x0063]).						% Nl       SMALL ROMAN NUMERAL ONE HUNDRED
unicode_nfkc_cf(0x217E, [0x0064]).						% Nl       SMALL ROMAN NUMERAL FIVE HUNDRED
unicode_nfkc_cf(0x217F, [0x006D]).						% Nl       SMALL ROMAN NUMERAL ONE THOUSAND
unicode_nfkc_cf(0x2183, [0x2184]).						% L&       ROMAN NUMERAL REVERSED ONE HUNDRED
unicode_nfkc_cf(0x2189, [0x0030, 0x2044, 0x0033]).		% No       VULGAR FRACTION ZERO THIRDS
unicode_nfkc_cf(0x222C, [0x222B, 0x222B]).				% Sm       DOUBLE INTEGRAL
unicode_nfkc_cf(0x222D, [0x222B, 0x222B, 0x222B]).		% Sm       TRIPLE INTEGRAL
unicode_nfkc_cf(0x222F, [0x222E, 0x222E]).				% Sm       SURFACE INTEGRAL
unicode_nfkc_cf(0x2230, [0x222E, 0x222E, 0x222E]).		% Sm       VOLUME INTEGRAL
unicode_nfkc_cf(0x2329, [0x3008]).						% Ps       LEFT-POINTING ANGLE BRACKET
unicode_nfkc_cf(0x232A, [0x3009]).						% Pe       RIGHT-POINTING ANGLE BRACKET
unicode_nfkc_cf(0x2460, [0x0031]).						% No       CIRCLED DIGIT ONE
unicode_nfkc_cf(0x2461, [0x0032]).						% No       CIRCLED DIGIT TWO
unicode_nfkc_cf(0x2462, [0x0033]).						% No       CIRCLED DIGIT THREE
unicode_nfkc_cf(0x2463, [0x0034]).						% No       CIRCLED DIGIT FOUR
unicode_nfkc_cf(0x2464, [0x0035]).						% No       CIRCLED DIGIT FIVE
unicode_nfkc_cf(0x2465, [0x0036]).						% No       CIRCLED DIGIT SIX
unicode_nfkc_cf(0x2466, [0x0037]).						% No       CIRCLED DIGIT SEVEN
unicode_nfkc_cf(0x2467, [0x0038]).						% No       CIRCLED DIGIT EIGHT
unicode_nfkc_cf(0x2468, [0x0039]).						% No       CIRCLED DIGIT NINE
unicode_nfkc_cf(0x2469, [0x0031, 0x0030]).				% No       CIRCLED NUMBER TEN
unicode_nfkc_cf(0x246A, [0x0031, 0x0031]).				% No       CIRCLED NUMBER ELEVEN
unicode_nfkc_cf(0x246B, [0x0031, 0x0032]).				% No       CIRCLED NUMBER TWELVE
unicode_nfkc_cf(0x246C, [0x0031, 0x0033]).				% No       CIRCLED NUMBER THIRTEEN
unicode_nfkc_cf(0x246D, [0x0031, 0x0034]).				% No       CIRCLED NUMBER FOURTEEN
unicode_nfkc_cf(0x246E, [0x0031, 0x0035]).				% No       CIRCLED NUMBER FIFTEEN
unicode_nfkc_cf(0x246F, [0x0031, 0x0036]).				% No       CIRCLED NUMBER SIXTEEN
unicode_nfkc_cf(0x2470, [0x0031, 0x0037]).				% No       CIRCLED NUMBER SEVENTEEN
unicode_nfkc_cf(0x2471, [0x0031, 0x0038]).				% No       CIRCLED NUMBER EIGHTEEN
unicode_nfkc_cf(0x2472, [0x0031, 0x0039]).				% No       CIRCLED NUMBER NINETEEN
unicode_nfkc_cf(0x2473, [0x0032, 0x0030]).				% No       CIRCLED NUMBER TWENTY
unicode_nfkc_cf(0x2474, [0x0028, 0x0031, 0x0029]).		% No       PARENTHESIZED DIGIT ONE
unicode_nfkc_cf(0x2475, [0x0028, 0x0032, 0x0029]).		% No       PARENTHESIZED DIGIT TWO
unicode_nfkc_cf(0x2476, [0x0028, 0x0033, 0x0029]).		% No       PARENTHESIZED DIGIT THREE
unicode_nfkc_cf(0x2477, [0x0028, 0x0034, 0x0029]).		% No       PARENTHESIZED DIGIT FOUR
unicode_nfkc_cf(0x2478, [0x0028, 0x0035, 0x0029]).		% No       PARENTHESIZED DIGIT FIVE
unicode_nfkc_cf(0x2479, [0x0028, 0x0036, 0x0029]).		% No       PARENTHESIZED DIGIT SIX
unicode_nfkc_cf(0x247A, [0x0028, 0x0037, 0x0029]).		% No       PARENTHESIZED DIGIT SEVEN
unicode_nfkc_cf(0x247B, [0x0028, 0x0038, 0x0029]).		% No       PARENTHESIZED DIGIT EIGHT
unicode_nfkc_cf(0x247C, [0x0028, 0x0039, 0x0029]).		% No       PARENTHESIZED DIGIT NINE
unicode_nfkc_cf(0x247D, [0x0028, 0x0031, 0x0030, 0x0029]).	% No   PARENTHESIZED NUMBER TEN
unicode_nfkc_cf(0x247E, [0x0028, 0x0031, 0x0031, 0x0029]).	% No   PARENTHESIZED NUMBER ELEVEN
unicode_nfkc_cf(0x247F, [0x0028, 0x0031, 0x0032, 0x0029]).	% No   PARENTHESIZED NUMBER TWELVE
unicode_nfkc_cf(0x2480, [0x0028, 0x0031, 0x0033, 0x0029]).	% No   PARENTHESIZED NUMBER THIRTEEN
unicode_nfkc_cf(0x2481, [0x0028, 0x0031, 0x0034, 0x0029]).	% No   PARENTHESIZED NUMBER FOURTEEN
unicode_nfkc_cf(0x2482, [0x0028, 0x0031, 0x0035, 0x0029]).	% No   PARENTHESIZED NUMBER FIFTEEN
unicode_nfkc_cf(0x2483, [0x0028, 0x0031, 0x0036, 0x0029]).	% No   PARENTHESIZED NUMBER SIXTEEN
unicode_nfkc_cf(0x2484, [0x0028, 0x0031, 0x0037, 0x0029]).	% No   PARENTHESIZED NUMBER SEVENTEEN
unicode_nfkc_cf(0x2485, [0x0028, 0x0031, 0x0038, 0x0029]).	% No   PARENTHESIZED NUMBER EIGHTEEN
unicode_nfkc_cf(0x2486, [0x0028, 0x0031, 0x0039, 0x0029]).	% No   PARENTHESIZED NUMBER NINETEEN
unicode_nfkc_cf(0x2487, [0x0028, 0x0032, 0x0030, 0x0029]).	% No   PARENTHESIZED NUMBER TWENTY
unicode_nfkc_cf(0x2488, [0x0031, 0x002E]).   			% No       DIGIT ONE FULL STOP
unicode_nfkc_cf(0x2489, [0x0032, 0x002E]).   			% No       DIGIT TWO FULL STOP
unicode_nfkc_cf(0x248A, [0x0033, 0x002E]).   			% No       DIGIT THREE FULL STOP
unicode_nfkc_cf(0x248B, [0x0034, 0x002E]).   			% No       DIGIT FOUR FULL STOP
unicode_nfkc_cf(0x248C, [0x0035, 0x002E]).   			% No       DIGIT FIVE FULL STOP
unicode_nfkc_cf(0x248D, [0x0036, 0x002E]).   			% No       DIGIT SIX FULL STOP
unicode_nfkc_cf(0x248E, [0x0037, 0x002E]).   			% No       DIGIT SEVEN FULL STOP
unicode_nfkc_cf(0x248F, [0x0038, 0x002E]).   			% No       DIGIT EIGHT FULL STOP
unicode_nfkc_cf(0x2490, [0x0039, 0x002E]).   			% No       DIGIT NINE FULL STOP
unicode_nfkc_cf(0x2491, [0x0031, 0x0030, 0x002E]).		% No       NUMBER TEN FULL STOP
unicode_nfkc_cf(0x2492, [0x0031, 0x0031, 0x002E]).		% No       NUMBER ELEVEN FULL STOP
unicode_nfkc_cf(0x2493, [0x0031, 0x0032, 0x002E]).		% No       NUMBER TWELVE FULL STOP
unicode_nfkc_cf(0x2494, [0x0031, 0x0033, 0x002E]).		% No       NUMBER THIRTEEN FULL STOP
unicode_nfkc_cf(0x2495, [0x0031, 0x0034, 0x002E]).		% No       NUMBER FOURTEEN FULL STOP
unicode_nfkc_cf(0x2496, [0x0031, 0x0035, 0x002E]).		% No       NUMBER FIFTEEN FULL STOP
unicode_nfkc_cf(0x2497, [0x0031, 0x0036, 0x002E]).		% No       NUMBER SIXTEEN FULL STOP
unicode_nfkc_cf(0x2498, [0x0031, 0x0037, 0x002E]).		% No       NUMBER SEVENTEEN FULL STOP
unicode_nfkc_cf(0x2499, [0x0031, 0x0038, 0x002E]).		% No       NUMBER EIGHTEEN FULL STOP
unicode_nfkc_cf(0x249A, [0x0031, 0x0039, 0x002E]).		% No       NUMBER NINETEEN FULL STOP
unicode_nfkc_cf(0x249B, [0x0032, 0x0030, 0x002E]).		% No       NUMBER TWENTY FULL STOP
unicode_nfkc_cf(0x249C, [0x0028, 0x0061, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER A
unicode_nfkc_cf(0x249D, [0x0028, 0x0062, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER B
unicode_nfkc_cf(0x249E, [0x0028, 0x0063, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER C
unicode_nfkc_cf(0x249F, [0x0028, 0x0064, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER D
unicode_nfkc_cf(0x24A0, [0x0028, 0x0065, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER E
unicode_nfkc_cf(0x24A1, [0x0028, 0x0066, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER F
unicode_nfkc_cf(0x24A2, [0x0028, 0x0067, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER G
unicode_nfkc_cf(0x24A3, [0x0028, 0x0068, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER H
unicode_nfkc_cf(0x24A4, [0x0028, 0x0069, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER I
unicode_nfkc_cf(0x24A5, [0x0028, 0x006A, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER J
unicode_nfkc_cf(0x24A6, [0x0028, 0x006B, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER K
unicode_nfkc_cf(0x24A7, [0x0028, 0x006C, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER L
unicode_nfkc_cf(0x24A8, [0x0028, 0x006D, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER M
unicode_nfkc_cf(0x24A9, [0x0028, 0x006E, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER N
unicode_nfkc_cf(0x24AA, [0x0028, 0x006F, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER O
unicode_nfkc_cf(0x24AB, [0x0028, 0x0070, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER P
unicode_nfkc_cf(0x24AC, [0x0028, 0x0071, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER Q
unicode_nfkc_cf(0x24AD, [0x0028, 0x0072, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER R
unicode_nfkc_cf(0x24AE, [0x0028, 0x0073, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER S
unicode_nfkc_cf(0x24AF, [0x0028, 0x0074, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER T
unicode_nfkc_cf(0x24B0, [0x0028, 0x0075, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER U
unicode_nfkc_cf(0x24B1, [0x0028, 0x0076, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER V
unicode_nfkc_cf(0x24B2, [0x0028, 0x0077, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER W
unicode_nfkc_cf(0x24B3, [0x0028, 0x0078, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER X
unicode_nfkc_cf(0x24B4, [0x0028, 0x0079, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER Y
unicode_nfkc_cf(0x24B5, [0x0028, 0x007A, 0x0029]).		% So       PARENTHESIZED LATIN SMALL LETTER Z
unicode_nfkc_cf(0x24B6, [0x0061]).						% So       CIRCLED LATIN CAPITAL LETTER A
unicode_nfkc_cf(0x24B7, [0x0062]).						% So       CIRCLED LATIN CAPITAL LETTER B
unicode_nfkc_cf(0x24B8, [0x0063]).						% So       CIRCLED LATIN CAPITAL LETTER C
unicode_nfkc_cf(0x24B9, [0x0064]).						% So       CIRCLED LATIN CAPITAL LETTER D
unicode_nfkc_cf(0x24BA, [0x0065]).						% So       CIRCLED LATIN CAPITAL LETTER E
unicode_nfkc_cf(0x24BB, [0x0066]).						% So       CIRCLED LATIN CAPITAL LETTER F
unicode_nfkc_cf(0x24BC, [0x0067]).						% So       CIRCLED LATIN CAPITAL LETTER G
unicode_nfkc_cf(0x24BD, [0x0068]).						% So       CIRCLED LATIN CAPITAL LETTER H
unicode_nfkc_cf(0x24BE, [0x0069]).						% So       CIRCLED LATIN CAPITAL LETTER I
unicode_nfkc_cf(0x24BF, [0x006A]).						% So       CIRCLED LATIN CAPITAL LETTER J
unicode_nfkc_cf(0x24C0, [0x006B]).						% So       CIRCLED LATIN CAPITAL LETTER K
unicode_nfkc_cf(0x24C1, [0x006C]).						% So       CIRCLED LATIN CAPITAL LETTER L
unicode_nfkc_cf(0x24C2, [0x006D]).						% So       CIRCLED LATIN CAPITAL LETTER M
unicode_nfkc_cf(0x24C3, [0x006E]).						% So       CIRCLED LATIN CAPITAL LETTER N
unicode_nfkc_cf(0x24C4, [0x006F]).						% So       CIRCLED LATIN CAPITAL LETTER O
unicode_nfkc_cf(0x24C5, [0x0070]).						% So       CIRCLED LATIN CAPITAL LETTER P
unicode_nfkc_cf(0x24C6, [0x0071]).						% So       CIRCLED LATIN CAPITAL LETTER Q
unicode_nfkc_cf(0x24C7, [0x0072]).						% So       CIRCLED LATIN CAPITAL LETTER R
unicode_nfkc_cf(0x24C8, [0x0073]).						% So       CIRCLED LATIN CAPITAL LETTER S
unicode_nfkc_cf(0x24C9, [0x0074]).						% So       CIRCLED LATIN CAPITAL LETTER T
unicode_nfkc_cf(0x24CA, [0x0075]).						% So       CIRCLED LATIN CAPITAL LETTER U
unicode_nfkc_cf(0x24CB, [0x0076]).						% So       CIRCLED LATIN CAPITAL LETTER V
unicode_nfkc_cf(0x24CC, [0x0077]).						% So       CIRCLED LATIN CAPITAL LETTER W
unicode_nfkc_cf(0x24CD, [0x0078]).						% So       CIRCLED LATIN CAPITAL LETTER X
unicode_nfkc_cf(0x24CE, [0x0079]).						% So       CIRCLED LATIN CAPITAL LETTER Y
unicode_nfkc_cf(0x24CF, [0x007A]).						% So       CIRCLED LATIN CAPITAL LETTER Z
unicode_nfkc_cf(0x24D0, [0x0061]).						% So       CIRCLED LATIN SMALL LETTER A
unicode_nfkc_cf(0x24D1, [0x0062]).						% So       CIRCLED LATIN SMALL LETTER B
unicode_nfkc_cf(0x24D2, [0x0063]).						% So       CIRCLED LATIN SMALL LETTER C
unicode_nfkc_cf(0x24D3, [0x0064]).						% So       CIRCLED LATIN SMALL LETTER D
unicode_nfkc_cf(0x24D4, [0x0065]).						% So       CIRCLED LATIN SMALL LETTER E
unicode_nfkc_cf(0x24D5, [0x0066]).						% So       CIRCLED LATIN SMALL LETTER F
unicode_nfkc_cf(0x24D6, [0x0067]).						% So       CIRCLED LATIN SMALL LETTER G
unicode_nfkc_cf(0x24D7, [0x0068]).						% So       CIRCLED LATIN SMALL LETTER H
unicode_nfkc_cf(0x24D8, [0x0069]).						% So       CIRCLED LATIN SMALL LETTER I
unicode_nfkc_cf(0x24D9, [0x006A]).						% So       CIRCLED LATIN SMALL LETTER J
unicode_nfkc_cf(0x24DA, [0x006B]).						% So       CIRCLED LATIN SMALL LETTER K
unicode_nfkc_cf(0x24DB, [0x006C]).						% So       CIRCLED LATIN SMALL LETTER L
unicode_nfkc_cf(0x24DC, [0x006D]).						% So       CIRCLED LATIN SMALL LETTER M
unicode_nfkc_cf(0x24DD, [0x006E]).						% So       CIRCLED LATIN SMALL LETTER N
unicode_nfkc_cf(0x24DE, [0x006F]).						% So       CIRCLED LATIN SMALL LETTER O
unicode_nfkc_cf(0x24DF, [0x0070]).						% So       CIRCLED LATIN SMALL LETTER P
unicode_nfkc_cf(0x24E0, [0x0071]).						% So       CIRCLED LATIN SMALL LETTER Q
unicode_nfkc_cf(0x24E1, [0x0072]).						% So       CIRCLED LATIN SMALL LETTER R
unicode_nfkc_cf(0x24E2, [0x0073]).						% So       CIRCLED LATIN SMALL LETTER S
unicode_nfkc_cf(0x24E3, [0x0074]).						% So       CIRCLED LATIN SMALL LETTER T
unicode_nfkc_cf(0x24E4, [0x0075]).						% So       CIRCLED LATIN SMALL LETTER U
unicode_nfkc_cf(0x24E5, [0x0076]).						% So       CIRCLED LATIN SMALL LETTER V
unicode_nfkc_cf(0x24E6, [0x0077]).						% So       CIRCLED LATIN SMALL LETTER W
unicode_nfkc_cf(0x24E7, [0x0078]).						% So       CIRCLED LATIN SMALL LETTER X
unicode_nfkc_cf(0x24E8, [0x0079]).						% So       CIRCLED LATIN SMALL LETTER Y
unicode_nfkc_cf(0x24E9, [0x007A]).						% So       CIRCLED LATIN SMALL LETTER Z
unicode_nfkc_cf(0x24EA, [0x0030]).						% No       CIRCLED DIGIT ZERO
unicode_nfkc_cf(0x2A0C, [0x222B, 0x222B, 0x222B, 0x222B]).	% Sm   QUADRUPLE INTEGRAL OPERATOR
unicode_nfkc_cf(0x2A74, [0x003A, 0x003A, 0x003D]).		% Sm       DOUBLE COLON EQUAL
unicode_nfkc_cf(0x2A75, [0x003D, 0x003D]).				% Sm       TWO CONSECUTIVE EQUALS SIGNS
unicode_nfkc_cf(0x2A76, [0x003D, 0x003D, 0x003D]).		% Sm       THREE CONSECUTIVE EQUALS SIGNS
unicode_nfkc_cf(0x2ADC, [0x2ADD, 0x0338]).				% Sm       FORKING
unicode_nfkc_cf(0x2C00, [0x2C30]).						% L&       GLAGOLITIC CAPITAL LETTER AZU
unicode_nfkc_cf(0x2C01, [0x2C31]).						% L&       GLAGOLITIC CAPITAL LETTER BUKY
unicode_nfkc_cf(0x2C02, [0x2C32]).						% L&       GLAGOLITIC CAPITAL LETTER VEDE
unicode_nfkc_cf(0x2C03, [0x2C33]).						% L&       GLAGOLITIC CAPITAL LETTER GLAGOLI
unicode_nfkc_cf(0x2C04, [0x2C34]).						% L&       GLAGOLITIC CAPITAL LETTER DOBRO
unicode_nfkc_cf(0x2C05, [0x2C35]).						% L&       GLAGOLITIC CAPITAL LETTER YESTU
unicode_nfkc_cf(0x2C06, [0x2C36]).						% L&       GLAGOLITIC CAPITAL LETTER ZHIVETE
unicode_nfkc_cf(0x2C07, [0x2C37]).						% L&       GLAGOLITIC CAPITAL LETTER DZELO
unicode_nfkc_cf(0x2C08, [0x2C38]).						% L&       GLAGOLITIC CAPITAL LETTER ZEMLJA
unicode_nfkc_cf(0x2C09, [0x2C39]).						% L&       GLAGOLITIC CAPITAL LETTER IZHE
unicode_nfkc_cf(0x2C0A, [0x2C3A]).						% L&       GLAGOLITIC CAPITAL LETTER INITIAL IZHE
unicode_nfkc_cf(0x2C0B, [0x2C3B]).						% L&       GLAGOLITIC CAPITAL LETTER I
unicode_nfkc_cf(0x2C0C, [0x2C3C]).						% L&       GLAGOLITIC CAPITAL LETTER DJERVI
unicode_nfkc_cf(0x2C0D, [0x2C3D]).						% L&       GLAGOLITIC CAPITAL LETTER KAKO
unicode_nfkc_cf(0x2C0E, [0x2C3E]).						% L&       GLAGOLITIC CAPITAL LETTER LJUDIJE
unicode_nfkc_cf(0x2C0F, [0x2C3F]).						% L&       GLAGOLITIC CAPITAL LETTER MYSLITE
unicode_nfkc_cf(0x2C10, [0x2C40]).						% L&       GLAGOLITIC CAPITAL LETTER NASHI
unicode_nfkc_cf(0x2C11, [0x2C41]).						% L&       GLAGOLITIC CAPITAL LETTER ONU
unicode_nfkc_cf(0x2C12, [0x2C42]).						% L&       GLAGOLITIC CAPITAL LETTER POKOJI
unicode_nfkc_cf(0x2C13, [0x2C43]).						% L&       GLAGOLITIC CAPITAL LETTER RITSI
unicode_nfkc_cf(0x2C14, [0x2C44]).						% L&       GLAGOLITIC CAPITAL LETTER SLOVO
unicode_nfkc_cf(0x2C15, [0x2C45]).						% L&       GLAGOLITIC CAPITAL LETTER TVRIDO
unicode_nfkc_cf(0x2C16, [0x2C46]).						% L&       GLAGOLITIC CAPITAL LETTER UKU
unicode_nfkc_cf(0x2C17, [0x2C47]).						% L&       GLAGOLITIC CAPITAL LETTER FRITU
unicode_nfkc_cf(0x2C18, [0x2C48]).						% L&       GLAGOLITIC CAPITAL LETTER HERU
unicode_nfkc_cf(0x2C19, [0x2C49]).						% L&       GLAGOLITIC CAPITAL LETTER OTU
unicode_nfkc_cf(0x2C1A, [0x2C4A]).						% L&       GLAGOLITIC CAPITAL LETTER PE
unicode_nfkc_cf(0x2C1B, [0x2C4B]).						% L&       GLAGOLITIC CAPITAL LETTER SHTA
unicode_nfkc_cf(0x2C1C, [0x2C4C]).						% L&       GLAGOLITIC CAPITAL LETTER TSI
unicode_nfkc_cf(0x2C1D, [0x2C4D]).						% L&       GLAGOLITIC CAPITAL LETTER CHRIVI
unicode_nfkc_cf(0x2C1E, [0x2C4E]).						% L&       GLAGOLITIC CAPITAL LETTER SHA
unicode_nfkc_cf(0x2C1F, [0x2C4F]).						% L&       GLAGOLITIC CAPITAL LETTER YERU
unicode_nfkc_cf(0x2C20, [0x2C50]).						% L&       GLAGOLITIC CAPITAL LETTER YERI
unicode_nfkc_cf(0x2C21, [0x2C51]).						% L&       GLAGOLITIC CAPITAL LETTER YATI
unicode_nfkc_cf(0x2C22, [0x2C52]).						% L&       GLAGOLITIC CAPITAL LETTER SPIDERY HA
unicode_nfkc_cf(0x2C23, [0x2C53]).						% L&       GLAGOLITIC CAPITAL LETTER YU
unicode_nfkc_cf(0x2C24, [0x2C54]).						% L&       GLAGOLITIC CAPITAL LETTER SMALL YUS
unicode_nfkc_cf(0x2C25, [0x2C55]).						% L&       GLAGOLITIC CAPITAL LETTER SMALL YUS WITH TAIL
unicode_nfkc_cf(0x2C26, [0x2C56]).						% L&       GLAGOLITIC CAPITAL LETTER YO
unicode_nfkc_cf(0x2C27, [0x2C57]).						% L&       GLAGOLITIC CAPITAL LETTER IOTATED SMALL YUS
unicode_nfkc_cf(0x2C28, [0x2C58]).						% L&       GLAGOLITIC CAPITAL LETTER BIG YUS
unicode_nfkc_cf(0x2C29, [0x2C59]).						% L&       GLAGOLITIC CAPITAL LETTER IOTATED BIG YUS
unicode_nfkc_cf(0x2C2A, [0x2C5A]).						% L&       GLAGOLITIC CAPITAL LETTER FITA
unicode_nfkc_cf(0x2C2B, [0x2C5B]).						% L&       GLAGOLITIC CAPITAL LETTER IZHITSA
unicode_nfkc_cf(0x2C2C, [0x2C5C]).						% L&       GLAGOLITIC CAPITAL LETTER SHTAPIC
unicode_nfkc_cf(0x2C2D, [0x2C5D]).						% L&       GLAGOLITIC CAPITAL LETTER TROKUTASTI A
unicode_nfkc_cf(0x2C2E, [0x2C5E]).						% L&       GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_nfkc_cf(0x2C60, [0x2C61]).						% L&       LATIN CAPITAL LETTER L WITH DOUBLE BAR
unicode_nfkc_cf(0x2C62, [0x026B]).						% L&       LATIN CAPITAL LETTER L WITH MIDDLE TILDE
unicode_nfkc_cf(0x2C63, [0x1D7D]).						% L&       LATIN CAPITAL LETTER P WITH STROKE
unicode_nfkc_cf(0x2C64, [0x027D]).						% L&       LATIN CAPITAL LETTER R WITH TAIL
unicode_nfkc_cf(0x2C67, [0x2C68]).						% L&       LATIN CAPITAL LETTER H WITH DESCENDER
unicode_nfkc_cf(0x2C69, [0x2C6A]).						% L&       LATIN CAPITAL LETTER K WITH DESCENDER
unicode_nfkc_cf(0x2C6B, [0x2C6C]).						% L&       LATIN CAPITAL LETTER Z WITH DESCENDER
unicode_nfkc_cf(0x2C6D, [0x0251]).						% L&       LATIN CAPITAL LETTER ALPHA
unicode_nfkc_cf(0x2C6E, [0x0271]).						% L&       LATIN CAPITAL LETTER M WITH HOOK
unicode_nfkc_cf(0x2C6F, [0x0250]).						% L&       LATIN CAPITAL LETTER TURNED A
unicode_nfkc_cf(0x2C70, [0x0252]).						% L&       LATIN CAPITAL LETTER TURNED ALPHA
unicode_nfkc_cf(0x2C72, [0x2C73]).						% L&       LATIN CAPITAL LETTER W WITH HOOK
unicode_nfkc_cf(0x2C75, [0x2C76]).						% L&       LATIN CAPITAL LETTER HALF H
unicode_nfkc_cf(0x2C7C, [0x006A]).						% Lm       LATIN SUBSCRIPT SMALL LETTER J
unicode_nfkc_cf(0x2C7D, [0x0076]).						% Lm       MODIFIER LETTER CAPITAL V
unicode_nfkc_cf(0x2C7E, [0x023F]).						% L&       LATIN CAPITAL LETTER S WITH SWASH TAIL
unicode_nfkc_cf(0x2C7F, [0x0240]).						% L&       LATIN CAPITAL LETTER Z WITH SWASH TAIL
unicode_nfkc_cf(0x2C80, [0x2C81]).						% L&       COPTIC CAPITAL LETTER ALFA
unicode_nfkc_cf(0x2C82, [0x2C83]).						% L&       COPTIC CAPITAL LETTER VIDA
unicode_nfkc_cf(0x2C84, [0x2C85]).						% L&       COPTIC CAPITAL LETTER GAMMA
unicode_nfkc_cf(0x2C86, [0x2C87]).						% L&       COPTIC CAPITAL LETTER DALDA
unicode_nfkc_cf(0x2C88, [0x2C89]).						% L&       COPTIC CAPITAL LETTER EIE
unicode_nfkc_cf(0x2C8A, [0x2C8B]).						% L&       COPTIC CAPITAL LETTER SOU
unicode_nfkc_cf(0x2C8C, [0x2C8D]).						% L&       COPTIC CAPITAL LETTER ZATA
unicode_nfkc_cf(0x2C8E, [0x2C8F]).						% L&       COPTIC CAPITAL LETTER HATE
unicode_nfkc_cf(0x2C90, [0x2C91]).						% L&       COPTIC CAPITAL LETTER THETHE
unicode_nfkc_cf(0x2C92, [0x2C93]).						% L&       COPTIC CAPITAL LETTER IAUDA
unicode_nfkc_cf(0x2C94, [0x2C95]).						% L&       COPTIC CAPITAL LETTER KAPA
unicode_nfkc_cf(0x2C96, [0x2C97]).						% L&       COPTIC CAPITAL LETTER LAULA
unicode_nfkc_cf(0x2C98, [0x2C99]).						% L&       COPTIC CAPITAL LETTER MI
unicode_nfkc_cf(0x2C9A, [0x2C9B]).						% L&       COPTIC CAPITAL LETTER NI
unicode_nfkc_cf(0x2C9C, [0x2C9D]).						% L&       COPTIC CAPITAL LETTER KSI
unicode_nfkc_cf(0x2C9E, [0x2C9F]).						% L&       COPTIC CAPITAL LETTER O
unicode_nfkc_cf(0x2CA0, [0x2CA1]).						% L&       COPTIC CAPITAL LETTER PI
unicode_nfkc_cf(0x2CA2, [0x2CA3]).						% L&       COPTIC CAPITAL LETTER RO
unicode_nfkc_cf(0x2CA4, [0x2CA5]).						% L&       COPTIC CAPITAL LETTER SIMA
unicode_nfkc_cf(0x2CA6, [0x2CA7]).						% L&       COPTIC CAPITAL LETTER TAU
unicode_nfkc_cf(0x2CA8, [0x2CA9]).						% L&       COPTIC CAPITAL LETTER UA
unicode_nfkc_cf(0x2CAA, [0x2CAB]).						% L&       COPTIC CAPITAL LETTER FI
unicode_nfkc_cf(0x2CAC, [0x2CAD]).						% L&       COPTIC CAPITAL LETTER KHI
unicode_nfkc_cf(0x2CAE, [0x2CAF]).						% L&       COPTIC CAPITAL LETTER PSI
unicode_nfkc_cf(0x2CB0, [0x2CB1]).						% L&       COPTIC CAPITAL LETTER OOU
unicode_nfkc_cf(0x2CB2, [0x2CB3]).						% L&       COPTIC CAPITAL LETTER DIALECT-P ALEF
unicode_nfkc_cf(0x2CB4, [0x2CB5]).						% L&       COPTIC CAPITAL LETTER OLD COPTIC AIN
unicode_nfkc_cf(0x2CB6, [0x2CB7]).						% L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC EIE
unicode_nfkc_cf(0x2CB8, [0x2CB9]).						% L&       COPTIC CAPITAL LETTER DIALECT-P KAPA
unicode_nfkc_cf(0x2CBA, [0x2CBB]).						% L&       COPTIC CAPITAL LETTER DIALECT-P NI
unicode_nfkc_cf(0x2CBC, [0x2CBD]).						% L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC NI
unicode_nfkc_cf(0x2CBE, [0x2CBF]).						% L&       COPTIC CAPITAL LETTER OLD COPTIC OOU
unicode_nfkc_cf(0x2CC0, [0x2CC1]).						% L&       COPTIC CAPITAL LETTER SAMPI
unicode_nfkc_cf(0x2CC2, [0x2CC3]).						% L&       COPTIC CAPITAL LETTER CROSSED SHEI
unicode_nfkc_cf(0x2CC4, [0x2CC5]).						% L&       COPTIC CAPITAL LETTER OLD COPTIC SHEI
unicode_nfkc_cf(0x2CC6, [0x2CC7]).						% L&       COPTIC CAPITAL LETTER OLD COPTIC ESH
unicode_nfkc_cf(0x2CC8, [0x2CC9]).						% L&       COPTIC CAPITAL LETTER AKHMIMIC KHEI
unicode_nfkc_cf(0x2CCA, [0x2CCB]).						% L&       COPTIC CAPITAL LETTER DIALECT-P HORI
unicode_nfkc_cf(0x2CCC, [0x2CCD]).						% L&       COPTIC CAPITAL LETTER OLD COPTIC HORI
unicode_nfkc_cf(0x2CCE, [0x2CCF]).						% L&       COPTIC CAPITAL LETTER OLD COPTIC HA
unicode_nfkc_cf(0x2CD0, [0x2CD1]).						% L&       COPTIC CAPITAL LETTER L-SHAPED HA
unicode_nfkc_cf(0x2CD2, [0x2CD3]).						% L&       COPTIC CAPITAL LETTER OLD COPTIC HEI
unicode_nfkc_cf(0x2CD4, [0x2CD5]).						% L&       COPTIC CAPITAL LETTER OLD COPTIC HAT
unicode_nfkc_cf(0x2CD6, [0x2CD7]).						% L&       COPTIC CAPITAL LETTER OLD COPTIC GANGIA
unicode_nfkc_cf(0x2CD8, [0x2CD9]).						% L&       COPTIC CAPITAL LETTER OLD COPTIC DJA
unicode_nfkc_cf(0x2CDA, [0x2CDB]).						% L&       COPTIC CAPITAL LETTER OLD COPTIC SHIMA
unicode_nfkc_cf(0x2CDC, [0x2CDD]).						% L&       COPTIC CAPITAL LETTER OLD NUBIAN SHIMA
unicode_nfkc_cf(0x2CDE, [0x2CDF]).						% L&       COPTIC CAPITAL LETTER OLD NUBIAN NGI
unicode_nfkc_cf(0x2CE0, [0x2CE1]).						% L&       COPTIC CAPITAL LETTER OLD NUBIAN NYI
unicode_nfkc_cf(0x2CE2, [0x2CE3]).						% L&       COPTIC CAPITAL LETTER OLD NUBIAN WAU
unicode_nfkc_cf(0x2CEB, [0x2CEC]).						% L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI
unicode_nfkc_cf(0x2CED, [0x2CEE]).						% L&       COPTIC CAPITAL LETTER CRYPTOGRAMMIC GANGIA
unicode_nfkc_cf(0x2CF2, [0x2CF3]).						% L&       COPTIC CAPITAL LETTER BOHAIRIC KHEI
unicode_nfkc_cf(0x2D6F, [0x2D61]).						% Lm       TIFINAGH MODIFIER LETTER LABIALIZATION MARK
unicode_nfkc_cf(0x2E9F, [0x6BCD]).						% So       CJK RADICAL MOTHER
unicode_nfkc_cf(0x2EF3, [0x9F9F]).						% So       CJK RADICAL C-SIMPLIFIED TURTLE
unicode_nfkc_cf(0x2F00, [0x4E00]).						% So       KANGXI RADICAL ONE
unicode_nfkc_cf(0x2F01, [0x4E28]).						% So       KANGXI RADICAL LINE
unicode_nfkc_cf(0x2F02, [0x4E36]).						% So       KANGXI RADICAL DOT
unicode_nfkc_cf(0x2F03, [0x4E3F]).						% So       KANGXI RADICAL SLASH
unicode_nfkc_cf(0x2F04, [0x4E59]).						% So       KANGXI RADICAL SECOND
unicode_nfkc_cf(0x2F05, [0x4E85]).						% So       KANGXI RADICAL HOOK
unicode_nfkc_cf(0x2F06, [0x4E8C]).						% So       KANGXI RADICAL TWO
unicode_nfkc_cf(0x2F07, [0x4EA0]).						% So       KANGXI RADICAL LID
unicode_nfkc_cf(0x2F08, [0x4EBA]).						% So       KANGXI RADICAL MAN
unicode_nfkc_cf(0x2F09, [0x513F]).						% So       KANGXI RADICAL LEGS
unicode_nfkc_cf(0x2F0A, [0x5165]).						% So       KANGXI RADICAL ENTER
unicode_nfkc_cf(0x2F0B, [0x516B]).						% So       KANGXI RADICAL EIGHT
unicode_nfkc_cf(0x2F0C, [0x5182]).						% So       KANGXI RADICAL DOWN BOX
unicode_nfkc_cf(0x2F0D, [0x5196]).						% So       KANGXI RADICAL COVER
unicode_nfkc_cf(0x2F0E, [0x51AB]).						% So       KANGXI RADICAL ICE
unicode_nfkc_cf(0x2F0F, [0x51E0]).						% So       KANGXI RADICAL TABLE
unicode_nfkc_cf(0x2F10, [0x51F5]).						% So       KANGXI RADICAL OPEN BOX
unicode_nfkc_cf(0x2F11, [0x5200]).						% So       KANGXI RADICAL KNIFE
unicode_nfkc_cf(0x2F12, [0x529B]).						% So       KANGXI RADICAL POWER
unicode_nfkc_cf(0x2F13, [0x52F9]).						% So       KANGXI RADICAL WRAP
unicode_nfkc_cf(0x2F14, [0x5315]).						% So       KANGXI RADICAL SPOON
unicode_nfkc_cf(0x2F15, [0x531A]).						% So       KANGXI RADICAL RIGHT OPEN BOX
unicode_nfkc_cf(0x2F16, [0x5338]).						% So       KANGXI RADICAL HIDING ENCLOSURE
unicode_nfkc_cf(0x2F17, [0x5341]).						% So       KANGXI RADICAL TEN
unicode_nfkc_cf(0x2F18, [0x535C]).						% So       KANGXI RADICAL DIVINATION
unicode_nfkc_cf(0x2F19, [0x5369]).						% So       KANGXI RADICAL SEAL
unicode_nfkc_cf(0x2F1A, [0x5382]).						% So       KANGXI RADICAL CLIFF
unicode_nfkc_cf(0x2F1B, [0x53B6]).						% So       KANGXI RADICAL PRIVATE
unicode_nfkc_cf(0x2F1C, [0x53C8]).						% So       KANGXI RADICAL AGAIN
unicode_nfkc_cf(0x2F1D, [0x53E3]).						% So       KANGXI RADICAL MOUTH
unicode_nfkc_cf(0x2F1E, [0x56D7]).						% So       KANGXI RADICAL ENCLOSURE
unicode_nfkc_cf(0x2F1F, [0x571F]).						% So       KANGXI RADICAL EARTH
unicode_nfkc_cf(0x2F20, [0x58EB]).						% So       KANGXI RADICAL SCHOLAR
unicode_nfkc_cf(0x2F21, [0x5902]).						% So       KANGXI RADICAL GO
unicode_nfkc_cf(0x2F22, [0x590A]).						% So       KANGXI RADICAL GO SLOWLY
unicode_nfkc_cf(0x2F23, [0x5915]).						% So       KANGXI RADICAL EVENING
unicode_nfkc_cf(0x2F24, [0x5927]).						% So       KANGXI RADICAL BIG
unicode_nfkc_cf(0x2F25, [0x5973]).						% So       KANGXI RADICAL WOMAN
unicode_nfkc_cf(0x2F26, [0x5B50]).						% So       KANGXI RADICAL CHILD
unicode_nfkc_cf(0x2F27, [0x5B80]).						% So       KANGXI RADICAL ROOF
unicode_nfkc_cf(0x2F28, [0x5BF8]).						% So       KANGXI RADICAL INCH
unicode_nfkc_cf(0x2F29, [0x5C0F]).						% So       KANGXI RADICAL SMALL
unicode_nfkc_cf(0x2F2A, [0x5C22]).						% So       KANGXI RADICAL LAME
unicode_nfkc_cf(0x2F2B, [0x5C38]).						% So       KANGXI RADICAL CORPSE
unicode_nfkc_cf(0x2F2C, [0x5C6E]).						% So       KANGXI RADICAL SPROUT
unicode_nfkc_cf(0x2F2D, [0x5C71]).						% So       KANGXI RADICAL MOUNTAIN
unicode_nfkc_cf(0x2F2E, [0x5DDB]).						% So       KANGXI RADICAL RIVER
unicode_nfkc_cf(0x2F2F, [0x5DE5]).						% So       KANGXI RADICAL WORK
unicode_nfkc_cf(0x2F30, [0x5DF1]).						% So       KANGXI RADICAL ONESELF
unicode_nfkc_cf(0x2F31, [0x5DFE]).						% So       KANGXI RADICAL TURBAN
unicode_nfkc_cf(0x2F32, [0x5E72]).						% So       KANGXI RADICAL DRY
unicode_nfkc_cf(0x2F33, [0x5E7A]).						% So       KANGXI RADICAL SHORT THREAD
unicode_nfkc_cf(0x2F34, [0x5E7F]).						% So       KANGXI RADICAL DOTTED CLIFF
unicode_nfkc_cf(0x2F35, [0x5EF4]).						% So       KANGXI RADICAL LONG STRIDE
unicode_nfkc_cf(0x2F36, [0x5EFE]).						% So       KANGXI RADICAL TWO HANDS
unicode_nfkc_cf(0x2F37, [0x5F0B]).						% So       KANGXI RADICAL SHOOT
unicode_nfkc_cf(0x2F38, [0x5F13]).						% So       KANGXI RADICAL BOW
unicode_nfkc_cf(0x2F39, [0x5F50]).						% So       KANGXI RADICAL SNOUT
unicode_nfkc_cf(0x2F3A, [0x5F61]).						% So       KANGXI RADICAL BRISTLE
unicode_nfkc_cf(0x2F3B, [0x5F73]).						% So       KANGXI RADICAL STEP
unicode_nfkc_cf(0x2F3C, [0x5FC3]).						% So       KANGXI RADICAL HEART
unicode_nfkc_cf(0x2F3D, [0x6208]).						% So       KANGXI RADICAL HALBERD
unicode_nfkc_cf(0x2F3E, [0x6236]).						% So       KANGXI RADICAL DOOR
unicode_nfkc_cf(0x2F3F, [0x624B]).						% So       KANGXI RADICAL HAND
unicode_nfkc_cf(0x2F40, [0x652F]).						% So       KANGXI RADICAL BRANCH
unicode_nfkc_cf(0x2F41, [0x6534]).						% So       KANGXI RADICAL RAP
unicode_nfkc_cf(0x2F42, [0x6587]).						% So       KANGXI RADICAL SCRIPT
unicode_nfkc_cf(0x2F43, [0x6597]).						% So       KANGXI RADICAL DIPPER
unicode_nfkc_cf(0x2F44, [0x65A4]).						% So       KANGXI RADICAL AXE
unicode_nfkc_cf(0x2F45, [0x65B9]).						% So       KANGXI RADICAL SQUARE
unicode_nfkc_cf(0x2F46, [0x65E0]).						% So       KANGXI RADICAL NOT
unicode_nfkc_cf(0x2F47, [0x65E5]).						% So       KANGXI RADICAL SUN
unicode_nfkc_cf(0x2F48, [0x66F0]).						% So       KANGXI RADICAL SAY
unicode_nfkc_cf(0x2F49, [0x6708]).						% So       KANGXI RADICAL MOON
unicode_nfkc_cf(0x2F4A, [0x6728]).						% So       KANGXI RADICAL TREE
unicode_nfkc_cf(0x2F4B, [0x6B20]).						% So       KANGXI RADICAL LACK
unicode_nfkc_cf(0x2F4C, [0x6B62]).						% So       KANGXI RADICAL STOP
unicode_nfkc_cf(0x2F4D, [0x6B79]).						% So       KANGXI RADICAL DEATH
unicode_nfkc_cf(0x2F4E, [0x6BB3]).						% So       KANGXI RADICAL WEAPON
unicode_nfkc_cf(0x2F4F, [0x6BCB]).						% So       KANGXI RADICAL DO NOT
unicode_nfkc_cf(0x2F50, [0x6BD4]).						% So       KANGXI RADICAL COMPARE
unicode_nfkc_cf(0x2F51, [0x6BDB]).						% So       KANGXI RADICAL FUR
unicode_nfkc_cf(0x2F52, [0x6C0F]).						% So       KANGXI RADICAL CLAN
unicode_nfkc_cf(0x2F53, [0x6C14]).						% So       KANGXI RADICAL STEAM
unicode_nfkc_cf(0x2F54, [0x6C34]).						% So       KANGXI RADICAL WATER
unicode_nfkc_cf(0x2F55, [0x706B]).						% So       KANGXI RADICAL FIRE
unicode_nfkc_cf(0x2F56, [0x722A]).						% So       KANGXI RADICAL CLAW
unicode_nfkc_cf(0x2F57, [0x7236]).						% So       KANGXI RADICAL FATHER
unicode_nfkc_cf(0x2F58, [0x723B]).						% So       KANGXI RADICAL DOUBLE X
unicode_nfkc_cf(0x2F59, [0x723F]).						% So       KANGXI RADICAL HALF TREE TRUNK
unicode_nfkc_cf(0x2F5A, [0x7247]).						% So       KANGXI RADICAL SLICE
unicode_nfkc_cf(0x2F5B, [0x7259]).						% So       KANGXI RADICAL FANG
unicode_nfkc_cf(0x2F5C, [0x725B]).						% So       KANGXI RADICAL COW
unicode_nfkc_cf(0x2F5D, [0x72AC]).						% So       KANGXI RADICAL DOG
unicode_nfkc_cf(0x2F5E, [0x7384]).						% So       KANGXI RADICAL PROFOUND
unicode_nfkc_cf(0x2F5F, [0x7389]).						% So       KANGXI RADICAL JADE
unicode_nfkc_cf(0x2F60, [0x74DC]).						% So       KANGXI RADICAL MELON
unicode_nfkc_cf(0x2F61, [0x74E6]).						% So       KANGXI RADICAL TILE
unicode_nfkc_cf(0x2F62, [0x7518]).						% So       KANGXI RADICAL SWEET
unicode_nfkc_cf(0x2F63, [0x751F]).						% So       KANGXI RADICAL LIFE
unicode_nfkc_cf(0x2F64, [0x7528]).						% So       KANGXI RADICAL USE
unicode_nfkc_cf(0x2F65, [0x7530]).						% So       KANGXI RADICAL FIELD
unicode_nfkc_cf(0x2F66, [0x758B]).						% So       KANGXI RADICAL BOLT OF CLOTH
unicode_nfkc_cf(0x2F67, [0x7592]).						% So       KANGXI RADICAL SICKNESS
unicode_nfkc_cf(0x2F68, [0x7676]).						% So       KANGXI RADICAL DOTTED TENT
unicode_nfkc_cf(0x2F69, [0x767D]).						% So       KANGXI RADICAL WHITE
unicode_nfkc_cf(0x2F6A, [0x76AE]).						% So       KANGXI RADICAL SKIN
unicode_nfkc_cf(0x2F6B, [0x76BF]).						% So       KANGXI RADICAL DISH
unicode_nfkc_cf(0x2F6C, [0x76EE]).						% So       KANGXI RADICAL EYE
unicode_nfkc_cf(0x2F6D, [0x77DB]).						% So       KANGXI RADICAL SPEAR
unicode_nfkc_cf(0x2F6E, [0x77E2]).						% So       KANGXI RADICAL ARROW
unicode_nfkc_cf(0x2F6F, [0x77F3]).						% So       KANGXI RADICAL STONE
unicode_nfkc_cf(0x2F70, [0x793A]).						% So       KANGXI RADICAL SPIRIT
unicode_nfkc_cf(0x2F71, [0x79B8]).						% So       KANGXI RADICAL TRACK
unicode_nfkc_cf(0x2F72, [0x79BE]).						% So       KANGXI RADICAL GRAIN
unicode_nfkc_cf(0x2F73, [0x7A74]).						% So       KANGXI RADICAL CAVE
unicode_nfkc_cf(0x2F74, [0x7ACB]).						% So       KANGXI RADICAL STAND
unicode_nfkc_cf(0x2F75, [0x7AF9]).						% So       KANGXI RADICAL BAMBOO
unicode_nfkc_cf(0x2F76, [0x7C73]).						% So       KANGXI RADICAL RICE
unicode_nfkc_cf(0x2F77, [0x7CF8]).						% So       KANGXI RADICAL SILK
unicode_nfkc_cf(0x2F78, [0x7F36]).						% So       KANGXI RADICAL JAR
unicode_nfkc_cf(0x2F79, [0x7F51]).						% So       KANGXI RADICAL NET
unicode_nfkc_cf(0x2F7A, [0x7F8A]).						% So       KANGXI RADICAL SHEEP
unicode_nfkc_cf(0x2F7B, [0x7FBD]).						% So       KANGXI RADICAL FEATHER
unicode_nfkc_cf(0x2F7C, [0x8001]).						% So       KANGXI RADICAL OLD
unicode_nfkc_cf(0x2F7D, [0x800C]).						% So       KANGXI RADICAL AND
unicode_nfkc_cf(0x2F7E, [0x8012]).						% So       KANGXI RADICAL PLOW
unicode_nfkc_cf(0x2F7F, [0x8033]).						% So       KANGXI RADICAL EAR
unicode_nfkc_cf(0x2F80, [0x807F]).						% So       KANGXI RADICAL BRUSH
unicode_nfkc_cf(0x2F81, [0x8089]).						% So       KANGXI RADICAL MEAT
unicode_nfkc_cf(0x2F82, [0x81E3]).						% So       KANGXI RADICAL MINISTER
unicode_nfkc_cf(0x2F83, [0x81EA]).						% So       KANGXI RADICAL SELF
unicode_nfkc_cf(0x2F84, [0x81F3]).						% So       KANGXI RADICAL ARRIVE
unicode_nfkc_cf(0x2F85, [0x81FC]).						% So       KANGXI RADICAL MORTAR
unicode_nfkc_cf(0x2F86, [0x820C]).						% So       KANGXI RADICAL TONGUE
unicode_nfkc_cf(0x2F87, [0x821B]).						% So       KANGXI RADICAL OPPOSE
unicode_nfkc_cf(0x2F88, [0x821F]).						% So       KANGXI RADICAL BOAT
unicode_nfkc_cf(0x2F89, [0x826E]).						% So       KANGXI RADICAL STOPPING
unicode_nfkc_cf(0x2F8A, [0x8272]).						% So       KANGXI RADICAL COLOR
unicode_nfkc_cf(0x2F8B, [0x8278]).						% So       KANGXI RADICAL GRASS
unicode_nfkc_cf(0x2F8C, [0x864D]).						% So       KANGXI RADICAL TIGER
unicode_nfkc_cf(0x2F8D, [0x866B]).						% So       KANGXI RADICAL INSECT
unicode_nfkc_cf(0x2F8E, [0x8840]).						% So       KANGXI RADICAL BLOOD
unicode_nfkc_cf(0x2F8F, [0x884C]).						% So       KANGXI RADICAL WALK ENCLOSURE
unicode_nfkc_cf(0x2F90, [0x8863]).						% So       KANGXI RADICAL CLOTHES
unicode_nfkc_cf(0x2F91, [0x897E]).						% So       KANGXI RADICAL WEST
unicode_nfkc_cf(0x2F92, [0x898B]).						% So       KANGXI RADICAL SEE
unicode_nfkc_cf(0x2F93, [0x89D2]).						% So       KANGXI RADICAL HORN
unicode_nfkc_cf(0x2F94, [0x8A00]).						% So       KANGXI RADICAL SPEECH
unicode_nfkc_cf(0x2F95, [0x8C37]).						% So       KANGXI RADICAL VALLEY
unicode_nfkc_cf(0x2F96, [0x8C46]).						% So       KANGXI RADICAL BEAN
unicode_nfkc_cf(0x2F97, [0x8C55]).						% So       KANGXI RADICAL PIG
unicode_nfkc_cf(0x2F98, [0x8C78]).						% So       KANGXI RADICAL BADGER
unicode_nfkc_cf(0x2F99, [0x8C9D]).						% So       KANGXI RADICAL SHELL
unicode_nfkc_cf(0x2F9A, [0x8D64]).						% So       KANGXI RADICAL RED
unicode_nfkc_cf(0x2F9B, [0x8D70]).						% So       KANGXI RADICAL RUN
unicode_nfkc_cf(0x2F9C, [0x8DB3]).						% So       KANGXI RADICAL FOOT
unicode_nfkc_cf(0x2F9D, [0x8EAB]).						% So       KANGXI RADICAL BODY
unicode_nfkc_cf(0x2F9E, [0x8ECA]).						% So       KANGXI RADICAL CART
unicode_nfkc_cf(0x2F9F, [0x8F9B]).						% So       KANGXI RADICAL BITTER
unicode_nfkc_cf(0x2FA0, [0x8FB0]).						% So       KANGXI RADICAL MORNING
unicode_nfkc_cf(0x2FA1, [0x8FB5]).						% So       KANGXI RADICAL WALK
unicode_nfkc_cf(0x2FA2, [0x9091]).						% So       KANGXI RADICAL CITY
unicode_nfkc_cf(0x2FA3, [0x9149]).						% So       KANGXI RADICAL WINE
unicode_nfkc_cf(0x2FA4, [0x91C6]).						% So       KANGXI RADICAL DISTINGUISH
unicode_nfkc_cf(0x2FA5, [0x91CC]).						% So       KANGXI RADICAL VILLAGE
unicode_nfkc_cf(0x2FA6, [0x91D1]).						% So       KANGXI RADICAL GOLD
unicode_nfkc_cf(0x2FA7, [0x9577]).						% So       KANGXI RADICAL LONG
unicode_nfkc_cf(0x2FA8, [0x9580]).						% So       KANGXI RADICAL GATE
unicode_nfkc_cf(0x2FA9, [0x961C]).						% So       KANGXI RADICAL MOUND
unicode_nfkc_cf(0x2FAA, [0x96B6]).						% So       KANGXI RADICAL SLAVE
unicode_nfkc_cf(0x2FAB, [0x96B9]).						% So       KANGXI RADICAL SHORT TAILED BIRD
unicode_nfkc_cf(0x2FAC, [0x96E8]).						% So       KANGXI RADICAL RAIN
unicode_nfkc_cf(0x2FAD, [0x9751]).						% So       KANGXI RADICAL BLUE
unicode_nfkc_cf(0x2FAE, [0x975E]).						% So       KANGXI RADICAL WRONG
unicode_nfkc_cf(0x2FAF, [0x9762]).						% So       KANGXI RADICAL FACE
unicode_nfkc_cf(0x2FB0, [0x9769]).						% So       KANGXI RADICAL LEATHER
unicode_nfkc_cf(0x2FB1, [0x97CB]).						% So       KANGXI RADICAL TANNED LEATHER
unicode_nfkc_cf(0x2FB2, [0x97ED]).						% So       KANGXI RADICAL LEEK
unicode_nfkc_cf(0x2FB3, [0x97F3]).						% So       KANGXI RADICAL SOUND
unicode_nfkc_cf(0x2FB4, [0x9801]).						% So       KANGXI RADICAL LEAF
unicode_nfkc_cf(0x2FB5, [0x98A8]).						% So       KANGXI RADICAL WIND
unicode_nfkc_cf(0x2FB6, [0x98DB]).						% So       KANGXI RADICAL FLY
unicode_nfkc_cf(0x2FB7, [0x98DF]).						% So       KANGXI RADICAL EAT
unicode_nfkc_cf(0x2FB8, [0x9996]).						% So       KANGXI RADICAL HEAD
unicode_nfkc_cf(0x2FB9, [0x9999]).						% So       KANGXI RADICAL FRAGRANT
unicode_nfkc_cf(0x2FBA, [0x99AC]).						% So       KANGXI RADICAL HORSE
unicode_nfkc_cf(0x2FBB, [0x9AA8]).						% So       KANGXI RADICAL BONE
unicode_nfkc_cf(0x2FBC, [0x9AD8]).						% So       KANGXI RADICAL TALL
unicode_nfkc_cf(0x2FBD, [0x9ADF]).						% So       KANGXI RADICAL HAIR
unicode_nfkc_cf(0x2FBE, [0x9B25]).						% So       KANGXI RADICAL FIGHT
unicode_nfkc_cf(0x2FBF, [0x9B2F]).						% So       KANGXI RADICAL SACRIFICIAL WINE
unicode_nfkc_cf(0x2FC0, [0x9B32]).						% So       KANGXI RADICAL CAULDRON
unicode_nfkc_cf(0x2FC1, [0x9B3C]).						% So       KANGXI RADICAL GHOST
unicode_nfkc_cf(0x2FC2, [0x9B5A]).						% So       KANGXI RADICAL FISH
unicode_nfkc_cf(0x2FC3, [0x9CE5]).						% So       KANGXI RADICAL BIRD
unicode_nfkc_cf(0x2FC4, [0x9E75]).						% So       KANGXI RADICAL SALT
unicode_nfkc_cf(0x2FC5, [0x9E7F]).						% So       KANGXI RADICAL DEER
unicode_nfkc_cf(0x2FC6, [0x9EA5]).						% So       KANGXI RADICAL WHEAT
unicode_nfkc_cf(0x2FC7, [0x9EBB]).						% So       KANGXI RADICAL HEMP
unicode_nfkc_cf(0x2FC8, [0x9EC3]).						% So       KANGXI RADICAL YELLOW
unicode_nfkc_cf(0x2FC9, [0x9ECD]).						% So       KANGXI RADICAL MILLET
unicode_nfkc_cf(0x2FCA, [0x9ED1]).						% So       KANGXI RADICAL BLACK
unicode_nfkc_cf(0x2FCB, [0x9EF9]).						% So       KANGXI RADICAL EMBROIDERY
unicode_nfkc_cf(0x2FCC, [0x9EFD]).						% So       KANGXI RADICAL FROG
unicode_nfkc_cf(0x2FCD, [0x9F0E]).						% So       KANGXI RADICAL TRIPOD
unicode_nfkc_cf(0x2FCE, [0x9F13]).						% So       KANGXI RADICAL DRUM
unicode_nfkc_cf(0x2FCF, [0x9F20]).						% So       KANGXI RADICAL RAT
unicode_nfkc_cf(0x2FD0, [0x9F3B]).						% So       KANGXI RADICAL NOSE
unicode_nfkc_cf(0x2FD1, [0x9F4A]).						% So       KANGXI RADICAL EVEN
unicode_nfkc_cf(0x2FD2, [0x9F52]).						% So       KANGXI RADICAL TOOTH
unicode_nfkc_cf(0x2FD3, [0x9F8D]).						% So       KANGXI RADICAL DRAGON
unicode_nfkc_cf(0x2FD4, [0x9F9C]).						% So       KANGXI RADICAL TURTLE
unicode_nfkc_cf(0x2FD5, [0x9FA0]).						% So       KANGXI RADICAL FLUTE
unicode_nfkc_cf(0x3000, [0x0020]).						% Zs       IDEOGRAPHIC SPACE
unicode_nfkc_cf(0x3036, [0x3012]).						% So       CIRCLED POSTAL MARK
unicode_nfkc_cf(0x3038, [0x5341]).						% Nl       HANGZHOU NUMERAL TEN
unicode_nfkc_cf(0x3039, [0x5344]).						% Nl       HANGZHOU NUMERAL TWENTY
unicode_nfkc_cf(0x303A, [0x5345]).						% Nl       HANGZHOU NUMERAL THIRTY
unicode_nfkc_cf(0x309B, [0x0020, 0x3099]).				% Sk       KATAKANA-HIRAGANA VOICED SOUND MARK
unicode_nfkc_cf(0x309C, [0x0020, 0x309A]).				% Sk       KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_nfkc_cf(0x309F, [0x3088, 0x308A]).				% Lo       HIRAGANA DIGRAPH YORI
unicode_nfkc_cf(0x30FF, [0x30B3, 0x30C8]).				% Lo       KATAKANA DIGRAPH KOTO
unicode_nfkc_cf(0x3131, [0x1100]).						% Lo       HANGUL LETTER KIYEOK
unicode_nfkc_cf(0x3132, [0x1101]).						% Lo       HANGUL LETTER SSANGKIYEOK
unicode_nfkc_cf(0x3133, [0x11AA]).						% Lo       HANGUL LETTER KIYEOK-SIOS
unicode_nfkc_cf(0x3134, [0x1102]).						% Lo       HANGUL LETTER NIEUN
unicode_nfkc_cf(0x3135, [0x11AC]).						% Lo       HANGUL LETTER NIEUN-CIEUC
unicode_nfkc_cf(0x3136, [0x11AD]).						% Lo       HANGUL LETTER NIEUN-HIEUH
unicode_nfkc_cf(0x3137, [0x1103]).						% Lo       HANGUL LETTER TIKEUT
unicode_nfkc_cf(0x3138, [0x1104]).						% Lo       HANGUL LETTER SSANGTIKEUT
unicode_nfkc_cf(0x3139, [0x1105]).						% Lo       HANGUL LETTER RIEUL
unicode_nfkc_cf(0x313A, [0x11B0]).						% Lo       HANGUL LETTER RIEUL-KIYEOK
unicode_nfkc_cf(0x313B, [0x11B1]).						% Lo       HANGUL LETTER RIEUL-MIEUM
unicode_nfkc_cf(0x313C, [0x11B2]).						% Lo       HANGUL LETTER RIEUL-PIEUP
unicode_nfkc_cf(0x313D, [0x11B3]).						% Lo       HANGUL LETTER RIEUL-SIOS
unicode_nfkc_cf(0x313E, [0x11B4]).						% Lo       HANGUL LETTER RIEUL-THIEUTH
unicode_nfkc_cf(0x313F, [0x11B5]).						% Lo       HANGUL LETTER RIEUL-PHIEUPH
unicode_nfkc_cf(0x3140, [0x111A]).						% Lo       HANGUL LETTER RIEUL-HIEUH
unicode_nfkc_cf(0x3141, [0x1106]).						% Lo       HANGUL LETTER MIEUM
unicode_nfkc_cf(0x3142, [0x1107]).						% Lo       HANGUL LETTER PIEUP
unicode_nfkc_cf(0x3143, [0x1108]).						% Lo       HANGUL LETTER SSANGPIEUP
unicode_nfkc_cf(0x3144, [0x1121]).						% Lo       HANGUL LETTER PIEUP-SIOS
unicode_nfkc_cf(0x3145, [0x1109]).						% Lo       HANGUL LETTER SIOS
unicode_nfkc_cf(0x3146, [0x110A]).						% Lo       HANGUL LETTER SSANGSIOS
unicode_nfkc_cf(0x3147, [0x110B]).						% Lo       HANGUL LETTER IEUNG
unicode_nfkc_cf(0x3148, [0x110C]).						% Lo       HANGUL LETTER CIEUC
unicode_nfkc_cf(0x3149, [0x110D]).						% Lo       HANGUL LETTER SSANGCIEUC
unicode_nfkc_cf(0x314A, [0x110E]).						% Lo       HANGUL LETTER CHIEUCH
unicode_nfkc_cf(0x314B, [0x110F]).						% Lo       HANGUL LETTER KHIEUKH
unicode_nfkc_cf(0x314C, [0x1110]).						% Lo       HANGUL LETTER THIEUTH
unicode_nfkc_cf(0x314D, [0x1111]).						% Lo       HANGUL LETTER PHIEUPH
unicode_nfkc_cf(0x314E, [0x1112]).						% Lo       HANGUL LETTER HIEUH
unicode_nfkc_cf(0x314F, [0x1161]).						% Lo       HANGUL LETTER A
unicode_nfkc_cf(0x3150, [0x1162]).						% Lo       HANGUL LETTER AE
unicode_nfkc_cf(0x3151, [0x1163]).						% Lo       HANGUL LETTER YA
unicode_nfkc_cf(0x3152, [0x1164]).						% Lo       HANGUL LETTER YAE
unicode_nfkc_cf(0x3153, [0x1165]).						% Lo       HANGUL LETTER EO
unicode_nfkc_cf(0x3154, [0x1166]).						% Lo       HANGUL LETTER E
unicode_nfkc_cf(0x3155, [0x1167]).						% Lo       HANGUL LETTER YEO
unicode_nfkc_cf(0x3156, [0x1168]).						% Lo       HANGUL LETTER YE
unicode_nfkc_cf(0x3157, [0x1169]).						% Lo       HANGUL LETTER O
unicode_nfkc_cf(0x3158, [0x116A]).						% Lo       HANGUL LETTER WA
unicode_nfkc_cf(0x3159, [0x116B]).						% Lo       HANGUL LETTER WAE
unicode_nfkc_cf(0x315A, [0x116C]).						% Lo       HANGUL LETTER OE
unicode_nfkc_cf(0x315B, [0x116D]).						% Lo       HANGUL LETTER YO
unicode_nfkc_cf(0x315C, [0x116E]).						% Lo       HANGUL LETTER U
unicode_nfkc_cf(0x315D, [0x116F]).						% Lo       HANGUL LETTER WEO
unicode_nfkc_cf(0x315E, [0x1170]).						% Lo       HANGUL LETTER WE
unicode_nfkc_cf(0x315F, [0x1171]).						% Lo       HANGUL LETTER WI
unicode_nfkc_cf(0x3160, [0x1172]).						% Lo       HANGUL LETTER YU
unicode_nfkc_cf(0x3161, [0x1173]).						% Lo       HANGUL LETTER EU
unicode_nfkc_cf(0x3162, [0x1174]).						% Lo       HANGUL LETTER YI
unicode_nfkc_cf(0x3163, [0x1175]).						% Lo       HANGUL LETTER I
%unicode_nfkc_cf(0x3164,                      # Lo  	     HANGUL FILLER
unicode_nfkc_cf(0x3165, [0x1114]).						% Lo       HANGUL LETTER SSANGNIEUN
unicode_nfkc_cf(0x3166, [0x1115]).						% Lo       HANGUL LETTER NIEUN-TIKEUT
unicode_nfkc_cf(0x3167, [0x11C7]).						% Lo       HANGUL LETTER NIEUN-SIOS
unicode_nfkc_cf(0x3168, [0x11C8]).						% Lo       HANGUL LETTER NIEUN-PANSIOS
unicode_nfkc_cf(0x3169, [0x11CC]).						% Lo       HANGUL LETTER RIEUL-KIYEOK-SIOS
unicode_nfkc_cf(0x316A, [0x11CE]).						% Lo       HANGUL LETTER RIEUL-TIKEUT
unicode_nfkc_cf(0x316B, [0x11D3]).						% Lo       HANGUL LETTER RIEUL-PIEUP-SIOS
unicode_nfkc_cf(0x316C, [0x11D7]).						% Lo       HANGUL LETTER RIEUL-PANSIOS
unicode_nfkc_cf(0x316D, [0x11D9]).						% Lo       HANGUL LETTER RIEUL-YEORINHIEUH
unicode_nfkc_cf(0x316E, [0x111C]).						% Lo       HANGUL LETTER MIEUM-PIEUP
unicode_nfkc_cf(0x316F, [0x11DD]).						% Lo       HANGUL LETTER MIEUM-SIOS
unicode_nfkc_cf(0x3170, [0x11DF]).						% Lo       HANGUL LETTER MIEUM-PANSIOS
unicode_nfkc_cf(0x3171, [0x111D]).						% Lo       HANGUL LETTER KAPYEOUNMIEUM
unicode_nfkc_cf(0x3172, [0x111E]).						% Lo       HANGUL LETTER PIEUP-KIYEOK
unicode_nfkc_cf(0x3173, [0x1120]).						% Lo       HANGUL LETTER PIEUP-TIKEUT
unicode_nfkc_cf(0x3174, [0x1122]).						% Lo       HANGUL LETTER PIEUP-SIOS-KIYEOK
unicode_nfkc_cf(0x3175, [0x1123]).						% Lo       HANGUL LETTER PIEUP-SIOS-TIKEUT
unicode_nfkc_cf(0x3176, [0x1127]).						% Lo       HANGUL LETTER PIEUP-CIEUC
unicode_nfkc_cf(0x3177, [0x1129]).						% Lo       HANGUL LETTER PIEUP-THIEUTH
unicode_nfkc_cf(0x3178, [0x112B]).						% Lo       HANGUL LETTER KAPYEOUNPIEUP
unicode_nfkc_cf(0x3179, [0x112C]).						% Lo       HANGUL LETTER KAPYEOUNSSANGPIEUP
unicode_nfkc_cf(0x317A, [0x112D]).						% Lo       HANGUL LETTER SIOS-KIYEOK
unicode_nfkc_cf(0x317B, [0x112E]).						% Lo       HANGUL LETTER SIOS-NIEUN
unicode_nfkc_cf(0x317C, [0x112F]).						% Lo       HANGUL LETTER SIOS-TIKEUT
unicode_nfkc_cf(0x317D, [0x1132]).						% Lo       HANGUL LETTER SIOS-PIEUP
unicode_nfkc_cf(0x317E, [0x1136]).						% Lo       HANGUL LETTER SIOS-CIEUC
unicode_nfkc_cf(0x317F, [0x1140]).						% Lo       HANGUL LETTER PANSIOS
unicode_nfkc_cf(0x3180, [0x1147]).						% Lo       HANGUL LETTER SSANGIEUNG
unicode_nfkc_cf(0x3181, [0x114C]).						% Lo       HANGUL LETTER YESIEUNG
unicode_nfkc_cf(0x3182, [0x11F1]).						% Lo       HANGUL LETTER YESIEUNG-SIOS
unicode_nfkc_cf(0x3183, [0x11F2]).						% Lo       HANGUL LETTER YESIEUNG-PANSIOS
unicode_nfkc_cf(0x3184, [0x1157]).						% Lo       HANGUL LETTER KAPYEOUNPHIEUPH
unicode_nfkc_cf(0x3185, [0x1158]).						% Lo       HANGUL LETTER SSANGHIEUH
unicode_nfkc_cf(0x3186, [0x1159]).						% Lo       HANGUL LETTER YEORINHIEUH
unicode_nfkc_cf(0x3187, [0x1184]).						% Lo       HANGUL LETTER YO-YA
unicode_nfkc_cf(0x3188, [0x1185]).						% Lo       HANGUL LETTER YO-YAE
unicode_nfkc_cf(0x3189, [0x1188]).						% Lo       HANGUL LETTER YO-I
unicode_nfkc_cf(0x318A, [0x1191]).						% Lo       HANGUL LETTER YU-YEO
unicode_nfkc_cf(0x318B, [0x1192]).						% Lo       HANGUL LETTER YU-YE
unicode_nfkc_cf(0x318C, [0x1194]).						% Lo       HANGUL LETTER YU-I
unicode_nfkc_cf(0x318D, [0x119E]).						% Lo       HANGUL LETTER ARAEA
unicode_nfkc_cf(0x318E, [0x11A1]).						% Lo       HANGUL LETTER ARAEAE
unicode_nfkc_cf(0x3192, [0x4E00]).						% No       IDEOGRAPHIC ANNOTATION ONE MARK
unicode_nfkc_cf(0x3193, [0x4E8C]).						% No       IDEOGRAPHIC ANNOTATION TWO MARK
unicode_nfkc_cf(0x3194, [0x4E09]).						% No       IDEOGRAPHIC ANNOTATION THREE MARK
unicode_nfkc_cf(0x3195, [0x56DB]).						% No       IDEOGRAPHIC ANNOTATION FOUR MARK
unicode_nfkc_cf(0x3196, [0x4E0A]).						% So       IDEOGRAPHIC ANNOTATION TOP MARK
unicode_nfkc_cf(0x3197, [0x4E2D]).						% So       IDEOGRAPHIC ANNOTATION MIDDLE MARK
unicode_nfkc_cf(0x3198, [0x4E0B]).						% So       IDEOGRAPHIC ANNOTATION BOTTOM MARK
unicode_nfkc_cf(0x3199, [0x7532]).						% So       IDEOGRAPHIC ANNOTATION FIRST MARK
unicode_nfkc_cf(0x319A, [0x4E59]).						% So       IDEOGRAPHIC ANNOTATION SECOND MARK
unicode_nfkc_cf(0x319B, [0x4E19]).						% So       IDEOGRAPHIC ANNOTATION THIRD MARK
unicode_nfkc_cf(0x319C, [0x4E01]).						% So       IDEOGRAPHIC ANNOTATION FOURTH MARK
unicode_nfkc_cf(0x319D, [0x5929]).						% So       IDEOGRAPHIC ANNOTATION HEAVEN MARK
unicode_nfkc_cf(0x319E, [0x5730]).						% So       IDEOGRAPHIC ANNOTATION EARTH MARK
unicode_nfkc_cf(0x319F, [0x4EBA]).						% So       IDEOGRAPHIC ANNOTATION MAN MARK
unicode_nfkc_cf(0x3200, [0x0028, 0x1100, 0x0029]).		% So       PARENTHESIZED HANGUL KIYEOK
unicode_nfkc_cf(0x3201, [0x0028, 0x1102, 0x0029]).		% So       PARENTHESIZED HANGUL NIEUN
unicode_nfkc_cf(0x3202, [0x0028, 0x1103, 0x0029]).		% So       PARENTHESIZED HANGUL TIKEUT
unicode_nfkc_cf(0x3203, [0x0028, 0x1105, 0x0029]).		% So       PARENTHESIZED HANGUL RIEUL
unicode_nfkc_cf(0x3204, [0x0028, 0x1106, 0x0029]).		% So       PARENTHESIZED HANGUL MIEUM
unicode_nfkc_cf(0x3205, [0x0028, 0x1107, 0x0029]).		% So       PARENTHESIZED HANGUL PIEUP
unicode_nfkc_cf(0x3206, [0x0028, 0x1109, 0x0029]).		% So       PARENTHESIZED HANGUL SIOS
unicode_nfkc_cf(0x3207, [0x0028, 0x110B, 0x0029]).		% So       PARENTHESIZED HANGUL IEUNG
unicode_nfkc_cf(0x3208, [0x0028, 0x110C, 0x0029]).		% So       PARENTHESIZED HANGUL CIEUC
unicode_nfkc_cf(0x3209, [0x0028, 0x110E, 0x0029]).		% So       PARENTHESIZED HANGUL CHIEUCH
unicode_nfkc_cf(0x320A, [0x0028, 0x110F, 0x0029]).		% So       PARENTHESIZED HANGUL KHIEUKH
unicode_nfkc_cf(0x320B, [0x0028, 0x1110, 0x0029]).		% So       PARENTHESIZED HANGUL THIEUTH
unicode_nfkc_cf(0x320C, [0x0028, 0x1111, 0x0029]).		% So       PARENTHESIZED HANGUL PHIEUPH
unicode_nfkc_cf(0x320D, [0x0028, 0x1112, 0x0029]).		% So       PARENTHESIZED HANGUL HIEUH
unicode_nfkc_cf(0x320E, [0x0028, 0xAC00, 0x0029]).		% So       PARENTHESIZED HANGUL KIYEOK A
unicode_nfkc_cf(0x320F, [0x0028, 0xB098, 0x0029]).		% So       PARENTHESIZED HANGUL NIEUN A
unicode_nfkc_cf(0x3210, [0x0028, 0xB2E4, 0x0029]).		% So       PARENTHESIZED HANGUL TIKEUT A
unicode_nfkc_cf(0x3211, [0x0028, 0xB77C, 0x0029]).		% So       PARENTHESIZED HANGUL RIEUL A
unicode_nfkc_cf(0x3212, [0x0028, 0xB9C8, 0x0029]).		% So       PARENTHESIZED HANGUL MIEUM A
unicode_nfkc_cf(0x3213, [0x0028, 0xBC14, 0x0029]).		% So       PARENTHESIZED HANGUL PIEUP A
unicode_nfkc_cf(0x3214, [0x0028, 0xC0AC, 0x0029]).		% So       PARENTHESIZED HANGUL SIOS A
unicode_nfkc_cf(0x3215, [0x0028, 0xC544, 0x0029]).		% So       PARENTHESIZED HANGUL IEUNG A
unicode_nfkc_cf(0x3216, [0x0028, 0xC790, 0x0029]).		% So       PARENTHESIZED HANGUL CIEUC A
unicode_nfkc_cf(0x3217, [0x0028, 0xCC28, 0x0029]).		% So       PARENTHESIZED HANGUL CHIEUCH A
unicode_nfkc_cf(0x3218, [0x0028, 0xCE74, 0x0029]).		% So       PARENTHESIZED HANGUL KHIEUKH A
unicode_nfkc_cf(0x3219, [0x0028, 0xD0C0, 0x0029]).		% So       PARENTHESIZED HANGUL THIEUTH A
unicode_nfkc_cf(0x321A, [0x0028, 0xD30C, 0x0029]).		% So       PARENTHESIZED HANGUL PHIEUPH A
unicode_nfkc_cf(0x321B, [0x0028, 0xD558, 0x0029]).		% So       PARENTHESIZED HANGUL HIEUH A
unicode_nfkc_cf(0x321C, [0x0028, 0xC8FC, 0x0029]).		% So       PARENTHESIZED HANGUL CIEUC U
unicode_nfkc_cf(0x321D, [0x0028, 0xC624, 0xC804, 0x0029]).	% So   PARENTHESIZED KOREAN CHARACTER OJEON
unicode_nfkc_cf(0x321E, [0x0028, 0xC624, 0xD6C4, 0x0029]).	% So   PARENTHESIZED KOREAN CHARACTER O HU
unicode_nfkc_cf(0x3220, [0x0028, 0x4E00, 0x0029]).		% No       PARENTHESIZED IDEOGRAPH ONE
unicode_nfkc_cf(0x3221, [0x0028, 0x4E8C, 0x0029]).		% No       PARENTHESIZED IDEOGRAPH TWO
unicode_nfkc_cf(0x3222, [0x0028, 0x4E09, 0x0029]).		% No       PARENTHESIZED IDEOGRAPH THREE
unicode_nfkc_cf(0x3223, [0x0028, 0x56DB, 0x0029]).		% No       PARENTHESIZED IDEOGRAPH FOUR
unicode_nfkc_cf(0x3224, [0x0028, 0x4E94, 0x0029]).		% No       PARENTHESIZED IDEOGRAPH FIVE
unicode_nfkc_cf(0x3225, [0x0028, 0x516D, 0x0029]).		% No       PARENTHESIZED IDEOGRAPH SIX
unicode_nfkc_cf(0x3226, [0x0028, 0x4E03, 0x0029]).		% No       PARENTHESIZED IDEOGRAPH SEVEN
unicode_nfkc_cf(0x3227, [0x0028, 0x516B, 0x0029]).		% No       PARENTHESIZED IDEOGRAPH EIGHT
unicode_nfkc_cf(0x3228, [0x0028, 0x4E5D, 0x0029]).		% No       PARENTHESIZED IDEOGRAPH NINE
unicode_nfkc_cf(0x3229, [0x0028, 0x5341, 0x0029]).		% No       PARENTHESIZED IDEOGRAPH TEN
unicode_nfkc_cf(0x322A, [0x0028, 0x6708, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH MOON
unicode_nfkc_cf(0x322B, [0x0028, 0x706B, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH FIRE
unicode_nfkc_cf(0x322C, [0x0028, 0x6C34, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH WATER
unicode_nfkc_cf(0x322D, [0x0028, 0x6728, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH WOOD
unicode_nfkc_cf(0x322E, [0x0028, 0x91D1, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH METAL
unicode_nfkc_cf(0x322F, [0x0028, 0x571F, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH EARTH
unicode_nfkc_cf(0x3230, [0x0028, 0x65E5, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH SUN
unicode_nfkc_cf(0x3231, [0x0028, 0x682A, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH STOCK
unicode_nfkc_cf(0x3232, [0x0028, 0x6709, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH HAVE
unicode_nfkc_cf(0x3233, [0x0028, 0x793E, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH SOCIETY
unicode_nfkc_cf(0x3234, [0x0028, 0x540D, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH NAME
unicode_nfkc_cf(0x3235, [0x0028, 0x7279, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH SPECIAL
unicode_nfkc_cf(0x3236, [0x0028, 0x8CA1, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH FINANCIAL
unicode_nfkc_cf(0x3237, [0x0028, 0x795D, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH CONGRATULATION
unicode_nfkc_cf(0x3238, [0x0028, 0x52B4, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH LABOR
unicode_nfkc_cf(0x3239, [0x0028, 0x4EE3, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH REPRESENT
unicode_nfkc_cf(0x323A, [0x0028, 0x547C, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH CALL
unicode_nfkc_cf(0x323B, [0x0028, 0x5B66, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH STUDY
unicode_nfkc_cf(0x323C, [0x0028, 0x76E3, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH SUPERVISE
unicode_nfkc_cf(0x323D, [0x0028, 0x4F01, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH ENTERPRISE
unicode_nfkc_cf(0x323E, [0x0028, 0x8CC7, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH RESOURCE
unicode_nfkc_cf(0x323F, [0x0028, 0x5354, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH ALLIANCE
unicode_nfkc_cf(0x3240, [0x0028, 0x796D, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH FESTIVAL
unicode_nfkc_cf(0x3241, [0x0028, 0x4F11, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH REST
unicode_nfkc_cf(0x3242, [0x0028, 0x81EA, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH SELF
unicode_nfkc_cf(0x3243, [0x0028, 0x81F3, 0x0029]).		% So       PARENTHESIZED IDEOGRAPH REACH
unicode_nfkc_cf(0x3244, [0x554F]).						% So       CIRCLED IDEOGRAPH QUESTION
unicode_nfkc_cf(0x3245, [0x5E7C]).						% So       CIRCLED IDEOGRAPH KINDERGARTEN
unicode_nfkc_cf(0x3246, [0x6587]).						% So       CIRCLED IDEOGRAPH SCHOOL
unicode_nfkc_cf(0x3247, [0x7B8F]).						% So       CIRCLED IDEOGRAPH KOTO
unicode_nfkc_cf(0x3250, [0x0070, 0x0074, 0x0065]).		% So       PARTNERSHIP SIGN
unicode_nfkc_cf(0x3251, [0x0032, 0x0031]).				% No       CIRCLED NUMBER TWENTY ONE
unicode_nfkc_cf(0x3252, [0x0032, 0x0032]).				% No       CIRCLED NUMBER TWENTY TWO
unicode_nfkc_cf(0x3253, [0x0032, 0x0033]).				% No       CIRCLED NUMBER TWENTY THREE
unicode_nfkc_cf(0x3254, [0x0032, 0x0034]).				% No       CIRCLED NUMBER TWENTY FOUR
unicode_nfkc_cf(0x3255, [0x0032, 0x0035]).				% No       CIRCLED NUMBER TWENTY FIVE
unicode_nfkc_cf(0x3256, [0x0032, 0x0036]).				% No       CIRCLED NUMBER TWENTY SIX
unicode_nfkc_cf(0x3257, [0x0032, 0x0037]).				% No       CIRCLED NUMBER TWENTY SEVEN
unicode_nfkc_cf(0x3258, [0x0032, 0x0038]).				% No       CIRCLED NUMBER TWENTY EIGHT
unicode_nfkc_cf(0x3259, [0x0032, 0x0039]).				% No       CIRCLED NUMBER TWENTY NINE
unicode_nfkc_cf(0x325A, [0x0033, 0x0030]).				% No       CIRCLED NUMBER THIRTY
unicode_nfkc_cf(0x325B, [0x0033, 0x0031]).				% No       CIRCLED NUMBER THIRTY ONE
unicode_nfkc_cf(0x325C, [0x0033, 0x0032]).				% No       CIRCLED NUMBER THIRTY TWO
unicode_nfkc_cf(0x325D, [0x0033, 0x0033]).				% No       CIRCLED NUMBER THIRTY THREE
unicode_nfkc_cf(0x325E, [0x0033, 0x0034]).				% No       CIRCLED NUMBER THIRTY FOUR
unicode_nfkc_cf(0x325F, [0x0033, 0x0035]).				% No       CIRCLED NUMBER THIRTY FIVE
unicode_nfkc_cf(0x3260, [0x1100]).						% So       CIRCLED HANGUL KIYEOK
unicode_nfkc_cf(0x3261, [0x1102]).						% So       CIRCLED HANGUL NIEUN
unicode_nfkc_cf(0x3262, [0x1103]).						% So       CIRCLED HANGUL TIKEUT
unicode_nfkc_cf(0x3263, [0x1105]).						% So       CIRCLED HANGUL RIEUL
unicode_nfkc_cf(0x3264, [0x1106]).						% So       CIRCLED HANGUL MIEUM
unicode_nfkc_cf(0x3265, [0x1107]).						% So       CIRCLED HANGUL PIEUP
unicode_nfkc_cf(0x3266, [0x1109]).						% So       CIRCLED HANGUL SIOS
unicode_nfkc_cf(0x3267, [0x110B]).						% So       CIRCLED HANGUL IEUNG
unicode_nfkc_cf(0x3268, [0x110C]).						% So       CIRCLED HANGUL CIEUC
unicode_nfkc_cf(0x3269, [0x110E]).						% So       CIRCLED HANGUL CHIEUCH
unicode_nfkc_cf(0x326A, [0x110F]).						% So       CIRCLED HANGUL KHIEUKH
unicode_nfkc_cf(0x326B, [0x1110]).						% So       CIRCLED HANGUL THIEUTH
unicode_nfkc_cf(0x326C, [0x1111]).						% So       CIRCLED HANGUL PHIEUPH
unicode_nfkc_cf(0x326D, [0x1112]).						% So       CIRCLED HANGUL HIEUH
unicode_nfkc_cf(0x326E, [0xAC00]).						% So       CIRCLED HANGUL KIYEOK A
unicode_nfkc_cf(0x326F, [0xB098]).						% So       CIRCLED HANGUL NIEUN A
unicode_nfkc_cf(0x3270, [0xB2E4]).						% So       CIRCLED HANGUL TIKEUT A
unicode_nfkc_cf(0x3271, [0xB77C]).						% So       CIRCLED HANGUL RIEUL A
unicode_nfkc_cf(0x3272, [0xB9C8]).						% So       CIRCLED HANGUL MIEUM A
unicode_nfkc_cf(0x3273, [0xBC14]).						% So       CIRCLED HANGUL PIEUP A
unicode_nfkc_cf(0x3274, [0xC0AC]).						% So       CIRCLED HANGUL SIOS A
unicode_nfkc_cf(0x3275, [0xC544]).						% So       CIRCLED HANGUL IEUNG A
unicode_nfkc_cf(0x3276, [0xC790]).						% So       CIRCLED HANGUL CIEUC A
unicode_nfkc_cf(0x3277, [0xCC28]).						% So       CIRCLED HANGUL CHIEUCH A
unicode_nfkc_cf(0x3278, [0xCE74]).						% So       CIRCLED HANGUL KHIEUKH A
unicode_nfkc_cf(0x3279, [0xD0C0]).						% So       CIRCLED HANGUL THIEUTH A
unicode_nfkc_cf(0x327A, [0xD30C]).						% So       CIRCLED HANGUL PHIEUPH A
unicode_nfkc_cf(0x327B, [0xD558]).						% So       CIRCLED HANGUL HIEUH A
unicode_nfkc_cf(0x327C, [0xCC38, 0xACE0]).				% So       CIRCLED KOREAN CHARACTER CHAMKO
unicode_nfkc_cf(0x327D, [0xC8FC, 0xC758]).				% So       CIRCLED KOREAN CHARACTER JUEUI
unicode_nfkc_cf(0x327E, [0xC6B0]).						% So       CIRCLED HANGUL IEUNG U
unicode_nfkc_cf(0x3280, [0x4E00]).						% No       CIRCLED IDEOGRAPH ONE
unicode_nfkc_cf(0x3281, [0x4E8C]).						% No       CIRCLED IDEOGRAPH TWO
unicode_nfkc_cf(0x3282, [0x4E09]).						% No       CIRCLED IDEOGRAPH THREE
unicode_nfkc_cf(0x3283, [0x56DB]).						% No       CIRCLED IDEOGRAPH FOUR
unicode_nfkc_cf(0x3284, [0x4E94]).						% No       CIRCLED IDEOGRAPH FIVE
unicode_nfkc_cf(0x3285, [0x516D]).						% No       CIRCLED IDEOGRAPH SIX
unicode_nfkc_cf(0x3286, [0x4E03]).						% No       CIRCLED IDEOGRAPH SEVEN
unicode_nfkc_cf(0x3287, [0x516B]).						% No       CIRCLED IDEOGRAPH EIGHT
unicode_nfkc_cf(0x3288, [0x4E5D]).						% No       CIRCLED IDEOGRAPH NINE
unicode_nfkc_cf(0x3289, [0x5341]).						% No       CIRCLED IDEOGRAPH TEN
unicode_nfkc_cf(0x328A, [0x6708]).						% So       CIRCLED IDEOGRAPH MOON
unicode_nfkc_cf(0x328B, [0x706B]).						% So       CIRCLED IDEOGRAPH FIRE
unicode_nfkc_cf(0x328C, [0x6C34]).						% So       CIRCLED IDEOGRAPH WATER
unicode_nfkc_cf(0x328D, [0x6728]).						% So       CIRCLED IDEOGRAPH WOOD
unicode_nfkc_cf(0x328E, [0x91D1]).						% So       CIRCLED IDEOGRAPH METAL
unicode_nfkc_cf(0x328F, [0x571F]).						% So       CIRCLED IDEOGRAPH EARTH
unicode_nfkc_cf(0x3290, [0x65E5]).						% So       CIRCLED IDEOGRAPH SUN
unicode_nfkc_cf(0x3291, [0x682A]).						% So       CIRCLED IDEOGRAPH STOCK
unicode_nfkc_cf(0x3292, [0x6709]).						% So       CIRCLED IDEOGRAPH HAVE
unicode_nfkc_cf(0x3293, [0x793E]).						% So       CIRCLED IDEOGRAPH SOCIETY
unicode_nfkc_cf(0x3294, [0x540D]).						% So       CIRCLED IDEOGRAPH NAME
unicode_nfkc_cf(0x3295, [0x7279]).						% So       CIRCLED IDEOGRAPH SPECIAL
unicode_nfkc_cf(0x3296, [0x8CA1]).						% So       CIRCLED IDEOGRAPH FINANCIAL
unicode_nfkc_cf(0x3297, [0x795D]).						% So       CIRCLED IDEOGRAPH CONGRATULATION
unicode_nfkc_cf(0x3298, [0x52B4]).						% So       CIRCLED IDEOGRAPH LABOR
unicode_nfkc_cf(0x3299, [0x79D8]).						% So       CIRCLED IDEOGRAPH SECRET
unicode_nfkc_cf(0x329A, [0x7537]).						% So       CIRCLED IDEOGRAPH MALE
unicode_nfkc_cf(0x329B, [0x5973]).						% So       CIRCLED IDEOGRAPH FEMALE
unicode_nfkc_cf(0x329C, [0x9069]).						% So       CIRCLED IDEOGRAPH SUITABLE
unicode_nfkc_cf(0x329D, [0x512A]).						% So       CIRCLED IDEOGRAPH EXCELLENT
unicode_nfkc_cf(0x329E, [0x5370]).						% So       CIRCLED IDEOGRAPH PRINT
unicode_nfkc_cf(0x329F, [0x6CE8]).						% So       CIRCLED IDEOGRAPH ATTENTION
unicode_nfkc_cf(0x32A0, [0x9805]).						% So       CIRCLED IDEOGRAPH ITEM
unicode_nfkc_cf(0x32A1, [0x4F11]).						% So       CIRCLED IDEOGRAPH REST
unicode_nfkc_cf(0x32A2, [0x5199]).						% So       CIRCLED IDEOGRAPH COPY
unicode_nfkc_cf(0x32A3, [0x6B63]).						% So       CIRCLED IDEOGRAPH CORRECT
unicode_nfkc_cf(0x32A4, [0x4E0A]).						% So       CIRCLED IDEOGRAPH HIGH
unicode_nfkc_cf(0x32A5, [0x4E2D]).						% So       CIRCLED IDEOGRAPH CENTRE
unicode_nfkc_cf(0x32A6, [0x4E0B]).						% So       CIRCLED IDEOGRAPH LOW
unicode_nfkc_cf(0x32A7, [0x5DE6]).						% So       CIRCLED IDEOGRAPH LEFT
unicode_nfkc_cf(0x32A8, [0x53F3]).						% So       CIRCLED IDEOGRAPH RIGHT
unicode_nfkc_cf(0x32A9, [0x533B]).						% So       CIRCLED IDEOGRAPH MEDICINE
unicode_nfkc_cf(0x32AA, [0x5B97]).						% So       CIRCLED IDEOGRAPH RELIGION
unicode_nfkc_cf(0x32AB, [0x5B66]).						% So       CIRCLED IDEOGRAPH STUDY
unicode_nfkc_cf(0x32AC, [0x76E3]).						% So       CIRCLED IDEOGRAPH SUPERVISE
unicode_nfkc_cf(0x32AD, [0x4F01]).						% So       CIRCLED IDEOGRAPH ENTERPRISE
unicode_nfkc_cf(0x32AE, [0x8CC7]).						% So       CIRCLED IDEOGRAPH RESOURCE
unicode_nfkc_cf(0x32AF, [0x5354]).						% So       CIRCLED IDEOGRAPH ALLIANCE
unicode_nfkc_cf(0x32B0, [0x591C]).						% So       CIRCLED IDEOGRAPH NIGHT
unicode_nfkc_cf(0x32B1, [0x0033, 0x0036]).				% No       CIRCLED NUMBER THIRTY SIX
unicode_nfkc_cf(0x32B2, [0x0033, 0x0037]).				% No       CIRCLED NUMBER THIRTY SEVEN
unicode_nfkc_cf(0x32B3, [0x0033, 0x0038]).				% No       CIRCLED NUMBER THIRTY EIGHT
unicode_nfkc_cf(0x32B4, [0x0033, 0x0039]).				% No       CIRCLED NUMBER THIRTY NINE
unicode_nfkc_cf(0x32B5, [0x0034, 0x0030]).				% No       CIRCLED NUMBER FORTY
unicode_nfkc_cf(0x32B6, [0x0034, 0x0031]).				% No       CIRCLED NUMBER FORTY ONE
unicode_nfkc_cf(0x32B7, [0x0034, 0x0032]).				% No       CIRCLED NUMBER FORTY TWO
unicode_nfkc_cf(0x32B8, [0x0034, 0x0033]).				% No       CIRCLED NUMBER FORTY THREE
unicode_nfkc_cf(0x32B9, [0x0034, 0x0034]).				% No       CIRCLED NUMBER FORTY FOUR
unicode_nfkc_cf(0x32BA, [0x0034, 0x0035]).				% No       CIRCLED NUMBER FORTY FIVE
unicode_nfkc_cf(0x32BB, [0x0034, 0x0036]).				% No       CIRCLED NUMBER FORTY SIX
unicode_nfkc_cf(0x32BC, [0x0034, 0x0037]).				% No       CIRCLED NUMBER FORTY SEVEN
unicode_nfkc_cf(0x32BD, [0x0034, 0x0038]).				% No       CIRCLED NUMBER FORTY EIGHT
unicode_nfkc_cf(0x32BE, [0x0034, 0x0039]).				% No       CIRCLED NUMBER FORTY NINE
unicode_nfkc_cf(0x32BF, [0x0035, 0x0030]).				% No       CIRCLED NUMBER FIFTY
unicode_nfkc_cf(0x32C0, [0x0031, 0x6708]).				% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY
unicode_nfkc_cf(0x32C1, [0x0032, 0x6708]).				% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR FEBRUARY
unicode_nfkc_cf(0x32C2, [0x0033, 0x6708]).				% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR MARCH
unicode_nfkc_cf(0x32C3, [0x0034, 0x6708]).				% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR APRIL
unicode_nfkc_cf(0x32C4, [0x0035, 0x6708]).				% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR MAY
unicode_nfkc_cf(0x32C5, [0x0036, 0x6708]).				% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR JUNE
unicode_nfkc_cf(0x32C6, [0x0037, 0x6708]).				% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR JULY
unicode_nfkc_cf(0x32C7, [0x0038, 0x6708]).				% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR AUGUST
unicode_nfkc_cf(0x32C8, [0x0039, 0x6708]).				% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR SEPTEMBER
unicode_nfkc_cf(0x32C9, [0x0031, 0x0030, 0x6708]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR OCTOBER
unicode_nfkc_cf(0x32CA, [0x0031, 0x0031, 0x6708]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR NOVEMBER
unicode_nfkc_cf(0x32CB, [0x0031, 0x0032, 0x6708]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DECEMBER
unicode_nfkc_cf(0x32CC, [0x0068, 0x0067]).				% So       SQUARE HG
unicode_nfkc_cf(0x32CD, [0x0065, 0x0072, 0x0067]).		% So       SQUARE ERG
unicode_nfkc_cf(0x32CE, [0x0065, 0x0076]).				% So       SQUARE EV
unicode_nfkc_cf(0x32CF, [0x006C, 0x0074, 0x0064]).		% So       LIMITED LIABILITY SIGN
unicode_nfkc_cf(0x32D0, [0x30A2]).						% So       CIRCLED KATAKANA A
unicode_nfkc_cf(0x32D1, [0x30A4]).						% So       CIRCLED KATAKANA I
unicode_nfkc_cf(0x32D2, [0x30A6]).						% So       CIRCLED KATAKANA U
unicode_nfkc_cf(0x32D3, [0x30A8]).						% So       CIRCLED KATAKANA E
unicode_nfkc_cf(0x32D4, [0x30AA]).						% So       CIRCLED KATAKANA O
unicode_nfkc_cf(0x32D5, [0x30AB]).						% So       CIRCLED KATAKANA KA
unicode_nfkc_cf(0x32D6, [0x30AD]).						% So       CIRCLED KATAKANA KI
unicode_nfkc_cf(0x32D7, [0x30AF]).						% So       CIRCLED KATAKANA KU
unicode_nfkc_cf(0x32D8, [0x30B1]).						% So       CIRCLED KATAKANA KE
unicode_nfkc_cf(0x32D9, [0x30B3]).						% So       CIRCLED KATAKANA KO
unicode_nfkc_cf(0x32DA, [0x30B5]).						% So       CIRCLED KATAKANA SA
unicode_nfkc_cf(0x32DB, [0x30B7]).						% So       CIRCLED KATAKANA SI
unicode_nfkc_cf(0x32DC, [0x30B9]).						% So       CIRCLED KATAKANA SU
unicode_nfkc_cf(0x32DD, [0x30BB]).						% So       CIRCLED KATAKANA SE
unicode_nfkc_cf(0x32DE, [0x30BD]).						% So       CIRCLED KATAKANA SO
unicode_nfkc_cf(0x32DF, [0x30BF]).						% So       CIRCLED KATAKANA TA
unicode_nfkc_cf(0x32E0, [0x30C1]).						% So       CIRCLED KATAKANA TI
unicode_nfkc_cf(0x32E1, [0x30C4]).						% So       CIRCLED KATAKANA TU
unicode_nfkc_cf(0x32E2, [0x30C6]).						% So       CIRCLED KATAKANA TE
unicode_nfkc_cf(0x32E3, [0x30C8]).						% So       CIRCLED KATAKANA TO
unicode_nfkc_cf(0x32E4, [0x30CA]).						% So       CIRCLED KATAKANA NA
unicode_nfkc_cf(0x32E5, [0x30CB]).						% So       CIRCLED KATAKANA NI
unicode_nfkc_cf(0x32E6, [0x30CC]).						% So       CIRCLED KATAKANA NU
unicode_nfkc_cf(0x32E7, [0x30CD]).						% So       CIRCLED KATAKANA NE
unicode_nfkc_cf(0x32E8, [0x30CE]).						% So       CIRCLED KATAKANA NO
unicode_nfkc_cf(0x32E9, [0x30CF]).						% So       CIRCLED KATAKANA HA
unicode_nfkc_cf(0x32EA, [0x30D2]).						% So       CIRCLED KATAKANA HI
unicode_nfkc_cf(0x32EB, [0x30D5]).						% So       CIRCLED KATAKANA HU
unicode_nfkc_cf(0x32EC, [0x30D8]).						% So       CIRCLED KATAKANA HE
unicode_nfkc_cf(0x32ED, [0x30DB]).						% So       CIRCLED KATAKANA HO
unicode_nfkc_cf(0x32EE, [0x30DE]).						% So       CIRCLED KATAKANA MA
unicode_nfkc_cf(0x32EF, [0x30DF]).						% So       CIRCLED KATAKANA MI
unicode_nfkc_cf(0x32F0, [0x30E0]).						% So       CIRCLED KATAKANA MU
unicode_nfkc_cf(0x32F1, [0x30E1]).						% So       CIRCLED KATAKANA ME
unicode_nfkc_cf(0x32F2, [0x30E2]).						% So       CIRCLED KATAKANA MO
unicode_nfkc_cf(0x32F3, [0x30E4]).						% So       CIRCLED KATAKANA YA
unicode_nfkc_cf(0x32F4, [0x30E6]).						% So       CIRCLED KATAKANA YU
unicode_nfkc_cf(0x32F5, [0x30E8]).						% So       CIRCLED KATAKANA YO
unicode_nfkc_cf(0x32F6, [0x30E9]).						% So       CIRCLED KATAKANA RA
unicode_nfkc_cf(0x32F7, [0x30EA]).						% So       CIRCLED KATAKANA RI
unicode_nfkc_cf(0x32F8, [0x30EB]).						% So       CIRCLED KATAKANA RU
unicode_nfkc_cf(0x32F9, [0x30EC]).						% So       CIRCLED KATAKANA RE
unicode_nfkc_cf(0x32FA, [0x30ED]).						% So       CIRCLED KATAKANA RO
unicode_nfkc_cf(0x32FB, [0x30EF]).						% So       CIRCLED KATAKANA WA
unicode_nfkc_cf(0x32FC, [0x30F0]).						% So       CIRCLED KATAKANA WI
unicode_nfkc_cf(0x32FD, [0x30F1]).						% So       CIRCLED KATAKANA WE
unicode_nfkc_cf(0x32FE, [0x30F2]).						% So       CIRCLED KATAKANA WO
unicode_nfkc_cf(0x3300, [0x30A2, 0x30D1, 0x30FC, 0x30C8]).	% So   SQUARE APAATO
unicode_nfkc_cf(0x3301, [0x30A2, 0x30EB, 0x30D5, 0x30A1]).	% So   SQUARE ARUHUA
unicode_nfkc_cf(0x3302, [0x30A2, 0x30F3, 0x30DA, 0x30A2]).	% So   SQUARE ANPEA
unicode_nfkc_cf(0x3303, [0x30A2, 0x30FC, 0x30EB]).		% So       SQUARE AARU
unicode_nfkc_cf(0x3304, [0x30A4, 0x30CB, 0x30F3, 0x30B0]).	% So   SQUARE ININGU
unicode_nfkc_cf(0x3305, [0x30A4, 0x30F3, 0x30C1]).		% So       SQUARE INTI
unicode_nfkc_cf(0x3306, [0x30A6, 0x30A9, 0x30F3]).		% So       SQUARE UON
unicode_nfkc_cf(0x3307, [0x30A8, 0x30B9, 0x30AF, 0x30FC, 0x30C9]).	% So SQUARE ESUKUUDO
unicode_nfkc_cf(0x3308, [0x30A8, 0x30FC, 0x30AB, 0x30FC]).	% So   SQUARE EEKAA
unicode_nfkc_cf(0x3309, [0x30AA, 0x30F3, 0x30B9]).		% So       SQUARE ONSU
unicode_nfkc_cf(0x330A, [0x30AA, 0x30FC, 0x30E0]).		% So       SQUARE OOMU
unicode_nfkc_cf(0x330B, [0x30AB, 0x30A4, 0x30EA]).		% So       SQUARE KAIRI
unicode_nfkc_cf(0x330C, [0x30AB, 0x30E9, 0x30C3, 0x30C8]).	% So   SQUARE KARATTO
unicode_nfkc_cf(0x330D, [0x30AB, 0x30ED, 0x30EA, 0x30FC]).	% So   SQUARE KARORII
unicode_nfkc_cf(0x330E, [0x30AC, 0x30ED, 0x30F3]).		% So       SQUARE GARON
unicode_nfkc_cf(0x330F, [0x30AC, 0x30F3, 0x30DE]).		% So       SQUARE GANMA
unicode_nfkc_cf(0x3310, [0x30AE, 0x30AC]).      		% So       SQUARE GIGA
unicode_nfkc_cf(0x3311, [0x30AE, 0x30CB, 0x30FC]).		% So       SQUARE GINII
unicode_nfkc_cf(0x3312, [0x30AD, 0x30E5, 0x30EA, 0x30FC]).	% So   SQUARE KYURII
unicode_nfkc_cf(0x3313, [0x30AE, 0x30EB, 0x30C0, 0x30FC]).	% So   SQUARE GIRUDAA
unicode_nfkc_cf(0x3314, [0x30AD, 0x30ED]).      		% So       SQUARE KIRO
unicode_nfkc_cf(0x3315, [0x30AD, 0x30ED, 0x30B0, 0x30E9, 0x30E0]).	% So SQUARE KIROGURAMU
unicode_nfkc_cf(0x3316, [0x30AD, 0x30ED, 0x30E1, 0x30FC, 0x30C8, 0x30EB]).	% So SQUARE KIROMEETORU
unicode_nfkc_cf(0x3317, [0x30AD, 0x30ED, 0x30EF, 0x30C3, 0x30C8]).	% So SQUARE KIROWATTO
unicode_nfkc_cf(0x3318, [0x30B0, 0x30E9, 0x30E0]).		% So       SQUARE GURAMU
unicode_nfkc_cf(0x3319, [0x30B0, 0x30E9, 0x30E0, 0x30C8, 0x30F3]).	% So SQUARE GURAMUTON
unicode_nfkc_cf(0x331A, [0x30AF, 0x30EB, 0x30BC, 0x30A4, 0x30ED]).	% So SQUARE KURUZEIRO
unicode_nfkc_cf(0x331B, [0x30AF, 0x30ED, 0x30FC, 0x30CD]).	% So   SQUARE KUROONE
unicode_nfkc_cf(0x331C, [0x30B1, 0x30FC, 0x30B9]).		% So       SQUARE KEESU
unicode_nfkc_cf(0x331D, [0x30B3, 0x30EB, 0x30CA]).		% So       SQUARE KORUNA
unicode_nfkc_cf(0x331E, [0x30B3, 0x30FC, 0x30DD]).		% So       SQUARE KOOPO
unicode_nfkc_cf(0x331F, [0x30B5, 0x30A4, 0x30AF, 0x30EB]).	% So   SQUARE SAIKURU
unicode_nfkc_cf(0x3320, [0x30B5, 0x30F3, 0x30C1, 0x30FC, 0x30E0]).	% So SQUARE SANTIIMU
unicode_nfkc_cf(0x3321, [0x30B7, 0x30EA, 0x30F3, 0x30B0]).	% So   SQUARE SIRINGU
unicode_nfkc_cf(0x3322, [0x30BB, 0x30F3, 0x30C1]).		% So       SQUARE SENTI
unicode_nfkc_cf(0x3323, [0x30BB, 0x30F3, 0x30C8]).		% So       SQUARE SENTO
unicode_nfkc_cf(0x3324, [0x30C0, 0x30FC, 0x30B9]).		% So       SQUARE DAASU
unicode_nfkc_cf(0x3325, [0x30C7, 0x30B7]).   			% So       SQUARE DESI
unicode_nfkc_cf(0x3326, [0x30C9, 0x30EB]).   			% So       SQUARE DORU
unicode_nfkc_cf(0x3327, [0x30C8, 0x30F3]).   			% So       SQUARE TON
unicode_nfkc_cf(0x3328, [0x30CA, 0x30CE]).   			% So       SQUARE NANO
unicode_nfkc_cf(0x3329, [0x30CE, 0x30C3, 0x30C8]).		% So       SQUARE NOTTO
unicode_nfkc_cf(0x332A, [0x30CF, 0x30A4, 0x30C4]).		% So       SQUARE HAITU
unicode_nfkc_cf(0x332B, [0x30D1, 0x30FC, 0x30BB, 0x30F3, 0x30C8]).	% So SQUARE PAASENTO
unicode_nfkc_cf(0x332C, [0x30D1, 0x30FC, 0x30C4]).		% So       SQUARE PAATU
unicode_nfkc_cf(0x332D, [0x30D0, 0x30FC, 0x30EC, 0x30EB]).	% So   SQUARE BAARERU
unicode_nfkc_cf(0x332E, [0x30D4, 0x30A2, 0x30B9, 0x30C8, 0x30EB]).	% So SQUARE PIASUTORU
unicode_nfkc_cf(0x332F, [0x30D4, 0x30AF, 0x30EB]).		% So       SQUARE PIKURU
unicode_nfkc_cf(0x3330, [0x30D4, 0x30B3]).   			% So       SQUARE PIKO
unicode_nfkc_cf(0x3331, [0x30D3, 0x30EB]).   			% So       SQUARE BIRU
unicode_nfkc_cf(0x3332, [0x30D5, 0x30A1, 0x30E9, 0x30C3, 0x30C9]).	% So SQUARE HUARADDO
unicode_nfkc_cf(0x3333, [0x30D5, 0x30A3, 0x30FC, 0x30C8]).	% So   SQUARE HUIITO
unicode_nfkc_cf(0x3334, [0x30D6, 0x30C3, 0x30B7, 0x30A7, 0x30EB]).	% So SQUARE BUSSYERU
unicode_nfkc_cf(0x3335, [0x30D5, 0x30E9, 0x30F3]).		% So       SQUARE HURAN
unicode_nfkc_cf(0x3336, [0x30D8, 0x30AF, 0x30BF, 0x30FC, 0x30EB]).	% So SQUARE HEKUTAARU
unicode_nfkc_cf(0x3337, [0x30DA, 0x30BD]).   			% So       SQUARE PESO
unicode_nfkc_cf(0x3338, [0x30DA, 0x30CB, 0x30D2]).		% So       SQUARE PENIHI
unicode_nfkc_cf(0x3339, [0x30D8, 0x30EB, 0x30C4]).		% So       SQUARE HERUTU
unicode_nfkc_cf(0x333A, [0x30DA, 0x30F3, 0x30B9]).		% So       SQUARE PENSU
unicode_nfkc_cf(0x333B, [0x30DA, 0x30FC, 0x30B8]).		% So       SQUARE PEEZI
unicode_nfkc_cf(0x333C, [0x30D9, 0x30FC, 0x30BF]).		% So       SQUARE BEETA
unicode_nfkc_cf(0x333D, [0x30DD, 0x30A4, 0x30F3, 0x30C8]).	% So   SQUARE POINTO
unicode_nfkc_cf(0x333E, [0x30DC, 0x30EB, 0x30C8]).		% So       SQUARE BORUTO
unicode_nfkc_cf(0x333F, [0x30DB, 0x30F3]).   			% So       SQUARE HON
unicode_nfkc_cf(0x3340, [0x30DD, 0x30F3, 0x30C9]).		% So       SQUARE PONDO
unicode_nfkc_cf(0x3341, [0x30DB, 0x30FC, 0x30EB]).		% So       SQUARE HOORU
unicode_nfkc_cf(0x3342, [0x30DB, 0x30FC, 0x30F3]).		% So       SQUARE HOON
unicode_nfkc_cf(0x3343, [0x30DE, 0x30A4, 0x30AF, 0x30ED]).	% So   SQUARE MAIKURO
unicode_nfkc_cf(0x3344, [0x30DE, 0x30A4, 0x30EB]).		% So       SQUARE MAIRU
unicode_nfkc_cf(0x3345, [0x30DE, 0x30C3, 0x30CF]).		% So       SQUARE MAHHA
unicode_nfkc_cf(0x3346, [0x30DE, 0x30EB, 0x30AF]).		% So       SQUARE MARUKU
unicode_nfkc_cf(0x3347, [0x30DE, 0x30F3, 0x30B7, 0x30E7, 0x30F3]).	% So SQUARE MANSYON
unicode_nfkc_cf(0x3348, [0x30DF, 0x30AF, 0x30ED, 0x30F3]).		% So   SQUARE MIKURON
unicode_nfkc_cf(0x3349, [0x30DF, 0x30EA]).   			% So       SQUARE MIRI
unicode_nfkc_cf(0x334A, [0x30DF, 0x30EA, 0x30D0, 0x30FC, 0x30EB]).		% So SQUARE MIRIBAARU
unicode_nfkc_cf(0x334B, [0x30E1, 0x30AC]).   			% So       SQUARE MEGA
unicode_nfkc_cf(0x334C, [0x30E1, 0x30AC, 0x30C8, 0x30F3]).	% So   SQUARE MEGATON
unicode_nfkc_cf(0x334D, [0x30E1, 0x30FC, 0x30C8, 0x30EB]).	% So   SQUARE MEETORU
unicode_nfkc_cf(0x334E, [0x30E4, 0x30FC, 0x30C9]).		% So       SQUARE YAADO
unicode_nfkc_cf(0x334F, [0x30E4, 0x30FC, 0x30EB]).		% So       SQUARE YAARU
unicode_nfkc_cf(0x3350, [0x30E6, 0x30A2, 0x30F3]).		% So       SQUARE YUAN
unicode_nfkc_cf(0x3351, [0x30EA, 0x30C3, 0x30C8, 0x30EB]).	% So   SQUARE RITTORU
unicode_nfkc_cf(0x3352, [0x30EA, 0x30E9]).   			% So       SQUARE RIRA
unicode_nfkc_cf(0x3353, [0x30EB, 0x30D4, 0x30FC]).		% So       SQUARE RUPII
unicode_nfkc_cf(0x3354, [0x30EB, 0x30FC, 0x30D6, 0x30EB]).	% So   SQUARE RUUBURU
unicode_nfkc_cf(0x3355, [0x30EC, 0x30E0]).   			% So       SQUARE REMU
unicode_nfkc_cf(0x3356, [0x30EC, 0x30F3, 0x30C8, 0x30B2, 0x30F3]).	% So SQUARE RENTOGEN
unicode_nfkc_cf(0x3357, [0x30EF, 0x30C3, 0x30C8]).		% So       SQUARE WATTO
unicode_nfkc_cf(0x3358, [0x0030, 0x70B9]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ZERO
unicode_nfkc_cf(0x3359, [0x0031, 0x70B9]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ONE
unicode_nfkc_cf(0x335A, [0x0032, 0x70B9]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWO
unicode_nfkc_cf(0x335B, [0x0033, 0x70B9]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR THREE
unicode_nfkc_cf(0x335C, [0x0034, 0x70B9]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR FOUR
unicode_nfkc_cf(0x335D, [0x0035, 0x70B9]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR FIVE
unicode_nfkc_cf(0x335E, [0x0036, 0x70B9]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR SIX
unicode_nfkc_cf(0x335F, [0x0037, 0x70B9]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR SEVEN
unicode_nfkc_cf(0x3360, [0x0038, 0x70B9]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR EIGHT
unicode_nfkc_cf(0x3361, [0x0039, 0x70B9]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR NINE
unicode_nfkc_cf(0x3362, [0x0031, 0x0030, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TEN
unicode_nfkc_cf(0x3363, [0x0031, 0x0031, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ELEVEN
unicode_nfkc_cf(0x3364, [0x0031, 0x0032, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWELVE
unicode_nfkc_cf(0x3365, [0x0031, 0x0033, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR THIRTEEN
unicode_nfkc_cf(0x3366, [0x0031, 0x0034, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR FOURTEEN
unicode_nfkc_cf(0x3367, [0x0031, 0x0035, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR FIFTEEN
unicode_nfkc_cf(0x3368, [0x0031, 0x0036, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR SIXTEEN
unicode_nfkc_cf(0x3369, [0x0031, 0x0037, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR SEVENTEEN
unicode_nfkc_cf(0x336A, [0x0031, 0x0038, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR EIGHTEEN
unicode_nfkc_cf(0x336B, [0x0031, 0x0039, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR NINETEEN
unicode_nfkc_cf(0x336C, [0x0032, 0x0030, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY
unicode_nfkc_cf(0x336D, [0x0032, 0x0031, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-ONE
unicode_nfkc_cf(0x336E, [0x0032, 0x0032, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-TWO
unicode_nfkc_cf(0x336F, [0x0032, 0x0033, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-THREE
unicode_nfkc_cf(0x3370, [0x0032, 0x0034, 0x70B9]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-FOUR
unicode_nfkc_cf(0x3371, [0x0068, 0x0070, 0x0061]).		% So       SQUARE HPA
unicode_nfkc_cf(0x3372, [0x0064, 0x0061]).   			% So       SQUARE DA
unicode_nfkc_cf(0x3373, [0x0061, 0x0075]).   			% So       SQUARE AU
unicode_nfkc_cf(0x3374, [0x0062, 0x0061, 0x0072]).		% So       SQUARE BAR
unicode_nfkc_cf(0x3375, [0x006F, 0x0076]).   			% So       SQUARE OV
unicode_nfkc_cf(0x3376, [0x0070, 0x0063]).   			% So       SQUARE PC
unicode_nfkc_cf(0x3377, [0x0064, 0x006D]).   			% So       SQUARE DM
unicode_nfkc_cf(0x3378, [0x0064, 0x006D, 0x0032]).		% So       SQUARE DM SQUARED
unicode_nfkc_cf(0x3379, [0x0064, 0x006D, 0x0033]).		% So       SQUARE DM CUBED
unicode_nfkc_cf(0x337A, [0x0069, 0x0075]).   			% So       SQUARE IU
unicode_nfkc_cf(0x337B, [0x5E73, 0x6210]).   			% So       SQUARE ERA NAME HEISEI
unicode_nfkc_cf(0x337C, [0x662D, 0x548C]).   			% So       SQUARE ERA NAME SYOUWA
unicode_nfkc_cf(0x337D, [0x5927, 0x6B63]).   			% So       SQUARE ERA NAME TAISYOU
unicode_nfkc_cf(0x337E, [0x660E, 0x6CBB]).   			% So       SQUARE ERA NAME MEIZI
unicode_nfkc_cf(0x337F, [0x682A, 0x5F0F, 0x4F1A, 0x793E]).	% So   SQUARE CORPORATION
unicode_nfkc_cf(0x3380, [0x0070, 0x0061]).   			% So       SQUARE PA AMPS
unicode_nfkc_cf(0x3381, [0x006E, 0x0061]).   			% So       SQUARE NA
unicode_nfkc_cf(0x3382, [0x03BC, 0x0061]).   			% So       SQUARE MU A
unicode_nfkc_cf(0x3383, [0x006D, 0x0061]).   			% So       SQUARE MA
unicode_nfkc_cf(0x3384, [0x006B, 0x0061]).   			% So       SQUARE KA
unicode_nfkc_cf(0x3385, [0x006B, 0x0062]).   			% So       SQUARE KB
unicode_nfkc_cf(0x3386, [0x006D, 0x0062]).   			% So       SQUARE MB
unicode_nfkc_cf(0x3387, [0x0067, 0x0062]).   			% So       SQUARE GB
unicode_nfkc_cf(0x3388, [0x0063, 0x0061, 0x006C]).		% So       SQUARE CAL
unicode_nfkc_cf(0x3389, [0x006B, 0x0063, 0x0061, 0x006C]).	% So   SQUARE KCAL
unicode_nfkc_cf(0x338A, [0x0070, 0x0066]).   			% So       SQUARE PF
unicode_nfkc_cf(0x338B, [0x006E, 0x0066]).   			% So       SQUARE NF
unicode_nfkc_cf(0x338C, [0x03BC, 0x0066]).   			% So       SQUARE MU F
unicode_nfkc_cf(0x338D, [0x03BC, 0x0067]).   			% So       SQUARE MU G
unicode_nfkc_cf(0x338E, [0x006D, 0x0067]).   			% So       SQUARE MG
unicode_nfkc_cf(0x338F, [0x006B, 0x0067]).   			% So       SQUARE KG
unicode_nfkc_cf(0x3390, [0x0068, 0x007A]).   			% So       SQUARE HZ
unicode_nfkc_cf(0x3391, [0x006B, 0x0068, 0x007A]).		% So       SQUARE KHZ
unicode_nfkc_cf(0x3392, [0x006D, 0x0068, 0x007A]).		% So       SQUARE MHZ
unicode_nfkc_cf(0x3393, [0x0067, 0x0068, 0x007A]).		% So       SQUARE GHZ
unicode_nfkc_cf(0x3394, [0x0074, 0x0068, 0x007A]).		% So       SQUARE THZ
unicode_nfkc_cf(0x3395, [0x03BC, 0x006C]).   			% So       SQUARE MU L
unicode_nfkc_cf(0x3396, [0x006D, 0x006C]).   			% So       SQUARE ML
unicode_nfkc_cf(0x3397, [0x0064, 0x006C]).   			% So       SQUARE DL
unicode_nfkc_cf(0x3398, [0x006B, 0x006C]).   			% So       SQUARE KL
unicode_nfkc_cf(0x3399, [0x0066, 0x006D]).   			% So       SQUARE FM
unicode_nfkc_cf(0x339A, [0x006E, 0x006D]).   			% So       SQUARE NM
unicode_nfkc_cf(0x339B, [0x03BC, 0x006D]).   			% So       SQUARE MU M
unicode_nfkc_cf(0x339C, [0x006D, 0x006D]).   			% So       SQUARE MM
unicode_nfkc_cf(0x339D, [0x0063, 0x006D]).   			% So       SQUARE CM
unicode_nfkc_cf(0x339E, [0x006B, 0x006D]).   			% So       SQUARE KM
unicode_nfkc_cf(0x339F, [0x006D, 0x006D, 0x0032]).		% So       SQUARE MM SQUARED
unicode_nfkc_cf(0x33A0, [0x0063, 0x006D, 0x0032]).		% So       SQUARE CM SQUARED
unicode_nfkc_cf(0x33A1, [0x006D, 0x0032]).   			% So       SQUARE M SQUARED
unicode_nfkc_cf(0x33A2, [0x006B, 0x006D, 0x0032]).		% So       SQUARE KM SQUARED
unicode_nfkc_cf(0x33A3, [0x006D, 0x006D, 0x0033]).		% So       SQUARE MM CUBED
unicode_nfkc_cf(0x33A4, [0x0063, 0x006D, 0x0033]).		% So       SQUARE CM CUBED
unicode_nfkc_cf(0x33A5, [0x006D, 0x0033]).   			% So       SQUARE M CUBED
unicode_nfkc_cf(0x33A6, [0x006B, 0x006D, 0x0033]).		% So       SQUARE KM CUBED
unicode_nfkc_cf(0x33A7, [0x006D, 0x2215, 0x0073]).		% So       SQUARE M OVER S
unicode_nfkc_cf(0x33A8, [0x006D, 0x2215, 0x0073, 0x0032]).	% So   SQUARE M OVER S SQUARED
unicode_nfkc_cf(0x33A9, [0x0070, 0x0061]).   			% So       SQUARE PA
unicode_nfkc_cf(0x33AA, [0x006B, 0x0070, 0x0061]).		% So       SQUARE KPA
unicode_nfkc_cf(0x33AB, [0x006D, 0x0070, 0x0061]).		% So       SQUARE MPA
unicode_nfkc_cf(0x33AC, [0x0067, 0x0070, 0x0061]).		% So       SQUARE GPA
unicode_nfkc_cf(0x33AD, [0x0072, 0x0061, 0x0064]).		% So       SQUARE RAD
unicode_nfkc_cf(0x33AE, [0x0072, 0x0061, 0x0064, 0x2215, 0x0073]).	% So SQUARE RAD OVER S
unicode_nfkc_cf(0x33AF, [0x0072, 0x0061, 0x0064, 0x2215, 0x0073, 0x0032]).	% So SQUARE RAD OVER S SQUARED
unicode_nfkc_cf(0x33B0, [0x0070, 0x0073]).   			% So       SQUARE PS
unicode_nfkc_cf(0x33B1, [0x006E, 0x0073]).   			% So       SQUARE NS
unicode_nfkc_cf(0x33B2, [0x03BC, 0x0073]).   			% So       SQUARE MU S
unicode_nfkc_cf(0x33B3, [0x006D, 0x0073]).   			% So       SQUARE MS
unicode_nfkc_cf(0x33B4, [0x0070, 0x0076]).   			% So       SQUARE PV
unicode_nfkc_cf(0x33B5, [0x006E, 0x0076]).   			% So       SQUARE NV
unicode_nfkc_cf(0x33B6, [0x03BC, 0x0076]).   			% So       SQUARE MU V
unicode_nfkc_cf(0x33B7, [0x006D, 0x0076]).   			% So       SQUARE MV
unicode_nfkc_cf(0x33B8, [0x006B, 0x0076]).   			% So       SQUARE KV
unicode_nfkc_cf(0x33B9, [0x006D, 0x0076]).   			% So       SQUARE MV MEGA
unicode_nfkc_cf(0x33BA, [0x0070, 0x0077]).   			% So       SQUARE PW
unicode_nfkc_cf(0x33BB, [0x006E, 0x0077]).   			% So       SQUARE NW
unicode_nfkc_cf(0x33BC, [0x03BC, 0x0077]).   			% So       SQUARE MU W
unicode_nfkc_cf(0x33BD, [0x006D, 0x0077]).   			% So       SQUARE MW
unicode_nfkc_cf(0x33BE, [0x006B, 0x0077]).   			% So       SQUARE KW
unicode_nfkc_cf(0x33BF, [0x006D, 0x0077]).   			% So       SQUARE MW MEGA
unicode_nfkc_cf(0x33C0, [0x006B, 0x03C9]).   			% So       SQUARE K OHM
unicode_nfkc_cf(0x33C1, [0x006D, 0x03C9]).   			% So       SQUARE M OHM
unicode_nfkc_cf(0x33C2, [0x0061, 0x002E, 0x006D, 0x002E]).	% So   SQUARE AM
unicode_nfkc_cf(0x33C3, [0x0062, 0x0071]).   			% So       SQUARE BQ
unicode_nfkc_cf(0x33C4, [0x0063, 0x0063]).   			% So       SQUARE CC
unicode_nfkc_cf(0x33C5, [0x0063, 0x0064]).   			% So       SQUARE CD
unicode_nfkc_cf(0x33C6, [0x0063, 0x2215, 0x006B, 0x0067]).	% So   SQUARE C OVER KG
unicode_nfkc_cf(0x33C7, [0x0063, 0x006F, 0x002E]).		% So       SQUARE CO
unicode_nfkc_cf(0x33C8, [0x0064, 0x0062]).   			% So       SQUARE DB
unicode_nfkc_cf(0x33C9, [0x0067, 0x0079]).   			% So       SQUARE GY
unicode_nfkc_cf(0x33CA, [0x0068, 0x0061]).   			% So       SQUARE HA
unicode_nfkc_cf(0x33CB, [0x0068, 0x0070]).   			% So       SQUARE HP
unicode_nfkc_cf(0x33CC, [0x0069, 0x006E]).   			% So       SQUARE IN
unicode_nfkc_cf(0x33CD, [0x006B, 0x006B]).   			% So       SQUARE KK
unicode_nfkc_cf(0x33CE, [0x006B, 0x006D]).   			% So       SQUARE KM CAPITAL
unicode_nfkc_cf(0x33CF, [0x006B, 0x0074]).   			% So       SQUARE KT
unicode_nfkc_cf(0x33D0, [0x006C, 0x006D]).   			% So       SQUARE LM
unicode_nfkc_cf(0x33D1, [0x006C, 0x006E]).   			% So       SQUARE LN
unicode_nfkc_cf(0x33D2, [0x006C, 0x006F, 0x0067]).		% So       SQUARE LOG
unicode_nfkc_cf(0x33D3, [0x006C, 0x0078]).   			% So       SQUARE LX
unicode_nfkc_cf(0x33D4, [0x006D, 0x0062]).   			% So       SQUARE MB SMALL
unicode_nfkc_cf(0x33D5, [0x006D, 0x0069, 0x006C]).		% So       SQUARE MIL
unicode_nfkc_cf(0x33D6, [0x006D, 0x006F, 0x006C]).		% So       SQUARE MOL
unicode_nfkc_cf(0x33D7, [0x0070, 0x0068]).   			% So       SQUARE PH
unicode_nfkc_cf(0x33D8, [0x0070, 0x002E, 0x006D, 0x002E]).	% So   SQUARE PM
unicode_nfkc_cf(0x33D9, [0x0070, 0x0070, 0x006D]).		% So       SQUARE PPM
unicode_nfkc_cf(0x33DA, [0x0070, 0x0072]).   			% So       SQUARE PR
unicode_nfkc_cf(0x33DB, [0x0073, 0x0072]).   			% So       SQUARE SR
unicode_nfkc_cf(0x33DC, [0x0073, 0x0076]).   			% So       SQUARE SV
unicode_nfkc_cf(0x33DD, [0x0077, 0x0062]).   			% So       SQUARE WB
unicode_nfkc_cf(0x33DE, [0x0076, 0x2215, 0x006D]).		% So       SQUARE V OVER M
unicode_nfkc_cf(0x33DF, [0x0061, 0x2215, 0x006D]).		% So       SQUARE A OVER M
unicode_nfkc_cf(0x33E0, [0x0031, 0x65E5]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY ONE
unicode_nfkc_cf(0x33E1, [0x0032, 0x65E5]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWO
unicode_nfkc_cf(0x33E2, [0x0033, 0x65E5]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THREE
unicode_nfkc_cf(0x33E3, [0x0034, 0x65E5]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY FOUR
unicode_nfkc_cf(0x33E4, [0x0035, 0x65E5]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY FIVE
unicode_nfkc_cf(0x33E5, [0x0036, 0x65E5]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY SIX
unicode_nfkc_cf(0x33E6, [0x0037, 0x65E5]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY SEVEN
unicode_nfkc_cf(0x33E7, [0x0038, 0x65E5]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY EIGHT
unicode_nfkc_cf(0x33E8, [0x0039, 0x65E5]).   			% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY NINE
unicode_nfkc_cf(0x33E9, [0x0031, 0x0030, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TEN
unicode_nfkc_cf(0x33EA, [0x0031, 0x0031, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY ELEVEN
unicode_nfkc_cf(0x33EB, [0x0031, 0x0032, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWELVE
unicode_nfkc_cf(0x33EC, [0x0031, 0x0033, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTEEN
unicode_nfkc_cf(0x33ED, [0x0031, 0x0034, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY FOURTEEN
unicode_nfkc_cf(0x33EE, [0x0031, 0x0035, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY FIFTEEN
unicode_nfkc_cf(0x33EF, [0x0031, 0x0036, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY SIXTEEN
unicode_nfkc_cf(0x33F0, [0x0031, 0x0037, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY SEVENTEEN
unicode_nfkc_cf(0x33F1, [0x0031, 0x0038, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY EIGHTEEN
unicode_nfkc_cf(0x33F2, [0x0031, 0x0039, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY NINETEEN
unicode_nfkc_cf(0x33F3, [0x0032, 0x0030, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY
unicode_nfkc_cf(0x33F4, [0x0032, 0x0031, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-ONE
unicode_nfkc_cf(0x33F5, [0x0032, 0x0032, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-TWO
unicode_nfkc_cf(0x33F6, [0x0032, 0x0033, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-THREE
unicode_nfkc_cf(0x33F7, [0x0032, 0x0034, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-FOUR
unicode_nfkc_cf(0x33F8, [0x0032, 0x0035, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-FIVE
unicode_nfkc_cf(0x33F9, [0x0032, 0x0036, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-SIX
unicode_nfkc_cf(0x33FA, [0x0032, 0x0037, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-SEVEN
unicode_nfkc_cf(0x33FB, [0x0032, 0x0038, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-EIGHT
unicode_nfkc_cf(0x33FC, [0x0032, 0x0039, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-NINE
unicode_nfkc_cf(0x33FD, [0x0033, 0x0030, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTY
unicode_nfkc_cf(0x33FE, [0x0033, 0x0031, 0x65E5]).		% So       IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTY-ONE
unicode_nfkc_cf(0x33FF, [0x0067, 0x0061, 0x006C]).		% So       SQUARE GAL
unicode_nfkc_cf(0xA640, [0xA641]).						% L&       CYRILLIC CAPITAL LETTER ZEMLYA
unicode_nfkc_cf(0xA642, [0xA643]).						% L&       CYRILLIC CAPITAL LETTER DZELO
unicode_nfkc_cf(0xA644, [0xA645]).						% L&       CYRILLIC CAPITAL LETTER REVERSED DZE
unicode_nfkc_cf(0xA646, [0xA647]).						% L&       CYRILLIC CAPITAL LETTER IOTA
unicode_nfkc_cf(0xA648, [0xA649]).						% L&       CYRILLIC CAPITAL LETTER DJERV
unicode_nfkc_cf(0xA64A, [0xA64B]).						% L&       CYRILLIC CAPITAL LETTER MONOGRAPH UK
unicode_nfkc_cf(0xA64C, [0xA64D]).						% L&       CYRILLIC CAPITAL LETTER BROAD OMEGA
unicode_nfkc_cf(0xA64E, [0xA64F]).						% L&       CYRILLIC CAPITAL LETTER NEUTRAL YER
unicode_nfkc_cf(0xA650, [0xA651]).						% L&       CYRILLIC CAPITAL LETTER YERU WITH BACK YER
unicode_nfkc_cf(0xA652, [0xA653]).						% L&       CYRILLIC CAPITAL LETTER IOTIFIED YAT
unicode_nfkc_cf(0xA654, [0xA655]).						% L&       CYRILLIC CAPITAL LETTER REVERSED YU
unicode_nfkc_cf(0xA656, [0xA657]).						% L&       CYRILLIC CAPITAL LETTER IOTIFIED A
unicode_nfkc_cf(0xA658, [0xA659]).						% L&       CYRILLIC CAPITAL LETTER CLOSED LITTLE YUS
unicode_nfkc_cf(0xA65A, [0xA65B]).						% L&       CYRILLIC CAPITAL LETTER BLENDED YUS
unicode_nfkc_cf(0xA65C, [0xA65D]).						% L&       CYRILLIC CAPITAL LETTER IOTIFIED CLOSED LITTLE YUS
unicode_nfkc_cf(0xA65E, [0xA65F]).						% L&       CYRILLIC CAPITAL LETTER YN
unicode_nfkc_cf(0xA660, [0xA661]).						% L&       CYRILLIC CAPITAL LETTER REVERSED TSE
unicode_nfkc_cf(0xA662, [0xA663]).						% L&       CYRILLIC CAPITAL LETTER SOFT DE
unicode_nfkc_cf(0xA664, [0xA665]).						% L&       CYRILLIC CAPITAL LETTER SOFT EL
unicode_nfkc_cf(0xA666, [0xA667]).						% L&       CYRILLIC CAPITAL LETTER SOFT EM
unicode_nfkc_cf(0xA668, [0xA669]).						% L&       CYRILLIC CAPITAL LETTER MONOCULAR O
unicode_nfkc_cf(0xA66A, [0xA66B]).						% L&       CYRILLIC CAPITAL LETTER BINOCULAR O
unicode_nfkc_cf(0xA66C, [0xA66D]).						% L&       CYRILLIC CAPITAL LETTER DOUBLE MONOCULAR O
unicode_nfkc_cf(0xA680, [0xA681]).						% L&       CYRILLIC CAPITAL LETTER DWE
unicode_nfkc_cf(0xA682, [0xA683]).						% L&       CYRILLIC CAPITAL LETTER DZWE
unicode_nfkc_cf(0xA684, [0xA685]).						% L&       CYRILLIC CAPITAL LETTER ZHWE
unicode_nfkc_cf(0xA686, [0xA687]).						% L&       CYRILLIC CAPITAL LETTER CCHE
unicode_nfkc_cf(0xA688, [0xA689]).						% L&       CYRILLIC CAPITAL LETTER DZZE
unicode_nfkc_cf(0xA68A, [0xA68B]).						% L&       CYRILLIC CAPITAL LETTER TE WITH MIDDLE HOOK
unicode_nfkc_cf(0xA68C, [0xA68D]).						% L&       CYRILLIC CAPITAL LETTER TWE
unicode_nfkc_cf(0xA68E, [0xA68F]).						% L&       CYRILLIC CAPITAL LETTER TSWE
unicode_nfkc_cf(0xA690, [0xA691]).						% L&       CYRILLIC CAPITAL LETTER TSSE
unicode_nfkc_cf(0xA692, [0xA693]).						% L&       CYRILLIC CAPITAL LETTER TCHE
unicode_nfkc_cf(0xA694, [0xA695]).						% L&       CYRILLIC CAPITAL LETTER HWE
unicode_nfkc_cf(0xA696, [0xA697]).						% L&       CYRILLIC CAPITAL LETTER SHWE
unicode_nfkc_cf(0xA722, [0xA723]).						% L&       LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF
unicode_nfkc_cf(0xA724, [0xA725]).						% L&       LATIN CAPITAL LETTER EGYPTOLOGICAL AIN
unicode_nfkc_cf(0xA726, [0xA727]).						% L&       LATIN CAPITAL LETTER HENG
unicode_nfkc_cf(0xA728, [0xA729]).						% L&       LATIN CAPITAL LETTER TZ
unicode_nfkc_cf(0xA72A, [0xA72B]).						% L&       LATIN CAPITAL LETTER TRESILLO
unicode_nfkc_cf(0xA72C, [0xA72D]).						% L&       LATIN CAPITAL LETTER CUATRILLO
unicode_nfkc_cf(0xA72E, [0xA72F]).						% L&       LATIN CAPITAL LETTER CUATRILLO WITH COMMA
unicode_nfkc_cf(0xA732, [0xA733]).						% L&       LATIN CAPITAL LETTER AA
unicode_nfkc_cf(0xA734, [0xA735]).						% L&       LATIN CAPITAL LETTER AO
unicode_nfkc_cf(0xA736, [0xA737]).						% L&       LATIN CAPITAL LETTER AU
unicode_nfkc_cf(0xA738, [0xA739]).						% L&       LATIN CAPITAL LETTER AV
unicode_nfkc_cf(0xA73A, [0xA73B]).						% L&       LATIN CAPITAL LETTER AV WITH HORIZONTAL BAR
unicode_nfkc_cf(0xA73C, [0xA73D]).						% L&       LATIN CAPITAL LETTER AY
unicode_nfkc_cf(0xA73E, [0xA73F]).						% L&       LATIN CAPITAL LETTER REVERSED C WITH DOT
unicode_nfkc_cf(0xA740, [0xA741]).						% L&       LATIN CAPITAL LETTER K WITH STROKE
unicode_nfkc_cf(0xA742, [0xA743]).						% L&       LATIN CAPITAL LETTER K WITH DIAGONAL STROKE
unicode_nfkc_cf(0xA744, [0xA745]).						% L&       LATIN CAPITAL LETTER K WITH STROKE AND DIAGONAL STROKE
unicode_nfkc_cf(0xA746, [0xA747]).						% L&       LATIN CAPITAL LETTER BROKEN L
unicode_nfkc_cf(0xA748, [0xA749]).						% L&       LATIN CAPITAL LETTER L WITH HIGH STROKE
unicode_nfkc_cf(0xA74A, [0xA74B]).						% L&       LATIN CAPITAL LETTER O WITH LONG STROKE OVERLAY
unicode_nfkc_cf(0xA74C, [0xA74D]).						% L&       LATIN CAPITAL LETTER O WITH LOOP
unicode_nfkc_cf(0xA74E, [0xA74F]).						% L&       LATIN CAPITAL LETTER OO
unicode_nfkc_cf(0xA750, [0xA751]).						% L&       LATIN CAPITAL LETTER P WITH STROKE THROUGH DESCENDER
unicode_nfkc_cf(0xA752, [0xA753]).						% L&       LATIN CAPITAL LETTER P WITH FLOURISH
unicode_nfkc_cf(0xA754, [0xA755]).						% L&       LATIN CAPITAL LETTER P WITH SQUIRREL TAIL
unicode_nfkc_cf(0xA756, [0xA757]).						% L&       LATIN CAPITAL LETTER Q WITH STROKE THROUGH DESCENDER
unicode_nfkc_cf(0xA758, [0xA759]).						% L&       LATIN CAPITAL LETTER Q WITH DIAGONAL STROKE
unicode_nfkc_cf(0xA75A, [0xA75B]).						% L&       LATIN CAPITAL LETTER R ROTUNDA
unicode_nfkc_cf(0xA75C, [0xA75D]).						% L&       LATIN CAPITAL LETTER RUM ROTUNDA
unicode_nfkc_cf(0xA75E, [0xA75F]).						% L&       LATIN CAPITAL LETTER V WITH DIAGONAL STROKE
unicode_nfkc_cf(0xA760, [0xA761]).						% L&       LATIN CAPITAL LETTER VY
unicode_nfkc_cf(0xA762, [0xA763]).						% L&       LATIN CAPITAL LETTER VISIGOTHIC Z
unicode_nfkc_cf(0xA764, [0xA765]).						% L&       LATIN CAPITAL LETTER THORN WITH STROKE
unicode_nfkc_cf(0xA766, [0xA767]).						% L&       LATIN CAPITAL LETTER THORN WITH STROKE THROUGH DESCENDER
unicode_nfkc_cf(0xA768, [0xA769]).						% L&       LATIN CAPITAL LETTER VEND
unicode_nfkc_cf(0xA76A, [0xA76B]).						% L&       LATIN CAPITAL LETTER ET
unicode_nfkc_cf(0xA76C, [0xA76D]).						% L&       LATIN CAPITAL LETTER IS
unicode_nfkc_cf(0xA76E, [0xA76F]).						% L&       LATIN CAPITAL LETTER CON
unicode_nfkc_cf(0xA770, [0xA76F]).						% Lm       MODIFIER LETTER US
unicode_nfkc_cf(0xA779, [0xA77A]).						% L&       LATIN CAPITAL LETTER INSULAR D
unicode_nfkc_cf(0xA77B, [0xA77C]).						% L&       LATIN CAPITAL LETTER INSULAR F
unicode_nfkc_cf(0xA77D, [0x1D79]).						% L&       LATIN CAPITAL LETTER INSULAR G
unicode_nfkc_cf(0xA77E, [0xA77F]).						% L&       LATIN CAPITAL LETTER TURNED INSULAR G
unicode_nfkc_cf(0xA780, [0xA781]).						% L&       LATIN CAPITAL LETTER TURNED L
unicode_nfkc_cf(0xA782, [0xA783]).						% L&       LATIN CAPITAL LETTER INSULAR R
unicode_nfkc_cf(0xA784, [0xA785]).						% L&       LATIN CAPITAL LETTER INSULAR S
unicode_nfkc_cf(0xA786, [0xA787]).						% L&       LATIN CAPITAL LETTER INSULAR T
unicode_nfkc_cf(0xA78B, [0xA78C]).						% L&       LATIN CAPITAL LETTER SALTILLO
unicode_nfkc_cf(0xA78D, [0x0265]).						% L&       LATIN CAPITAL LETTER TURNED H
unicode_nfkc_cf(0xA790, [0xA791]).						% L&       LATIN CAPITAL LETTER N WITH DESCENDER
unicode_nfkc_cf(0xA792, [0xA793]).						% L&       LATIN CAPITAL LETTER C WITH BAR
unicode_nfkc_cf(0xA7A0, [0xA7A1]).						% L&       LATIN CAPITAL LETTER G WITH OBLIQUE STROKE
unicode_nfkc_cf(0xA7A2, [0xA7A3]).						% L&       LATIN CAPITAL LETTER K WITH OBLIQUE STROKE
unicode_nfkc_cf(0xA7A4, [0xA7A5]).						% L&       LATIN CAPITAL LETTER N WITH OBLIQUE STROKE
unicode_nfkc_cf(0xA7A6, [0xA7A7]).						% L&       LATIN CAPITAL LETTER R WITH OBLIQUE STROKE
unicode_nfkc_cf(0xA7A8, [0xA7A9]).						% L&       LATIN CAPITAL LETTER S WITH OBLIQUE STROKE
unicode_nfkc_cf(0xA7AA, [0x0266]).						% L&       LATIN CAPITAL LETTER H WITH HOOK
unicode_nfkc_cf(0xA7F8, [0x0127]).						% Lm       MODIFIER LETTER CAPITAL H WITH STROKE
unicode_nfkc_cf(0xA7F9, [0x0153]).						% Lm       MODIFIER LETTER SMALL LIGATURE OE
unicode_nfkc_cf(0xF900, [0x8C48]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F900
unicode_nfkc_cf(0xF901, [0x66F4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F901
unicode_nfkc_cf(0xF902, [0x8ECA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F902
unicode_nfkc_cf(0xF903, [0x8CC8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F903
unicode_nfkc_cf(0xF904, [0x6ED1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F904
unicode_nfkc_cf(0xF905, [0x4E32]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F905
unicode_nfkc_cf(0xF906, [0x53E5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F906
unicode_nfkc_cf(0xF907, [0x9F9C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F907
unicode_nfkc_cf(0xF908, [0x9F9C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F908
unicode_nfkc_cf(0xF909, [0x5951]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F909
unicode_nfkc_cf(0xF90A, [0x91D1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F90A
unicode_nfkc_cf(0xF90B, [0x5587]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F90B
unicode_nfkc_cf(0xF90C, [0x5948]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F90C
unicode_nfkc_cf(0xF90D, [0x61F6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F90D
unicode_nfkc_cf(0xF90E, [0x7669]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F90E
unicode_nfkc_cf(0xF90F, [0x7F85]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F90F
unicode_nfkc_cf(0xF910, [0x863F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F910
unicode_nfkc_cf(0xF911, [0x87BA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F911
unicode_nfkc_cf(0xF912, [0x88F8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F912
unicode_nfkc_cf(0xF913, [0x908F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F913
unicode_nfkc_cf(0xF914, [0x6A02]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F914
unicode_nfkc_cf(0xF915, [0x6D1B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F915
unicode_nfkc_cf(0xF916, [0x70D9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F916
unicode_nfkc_cf(0xF917, [0x73DE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F917
unicode_nfkc_cf(0xF918, [0x843D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F918
unicode_nfkc_cf(0xF919, [0x916A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F919
unicode_nfkc_cf(0xF91A, [0x99F1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F91A
unicode_nfkc_cf(0xF91B, [0x4E82]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F91B
unicode_nfkc_cf(0xF91C, [0x5375]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F91C
unicode_nfkc_cf(0xF91D, [0x6B04]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F91D
unicode_nfkc_cf(0xF91E, [0x721B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F91E
unicode_nfkc_cf(0xF91F, [0x862D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F91F
unicode_nfkc_cf(0xF920, [0x9E1E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F920
unicode_nfkc_cf(0xF921, [0x5D50]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F921
unicode_nfkc_cf(0xF922, [0x6FEB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F922
unicode_nfkc_cf(0xF923, [0x85CD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F923
unicode_nfkc_cf(0xF924, [0x8964]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F924
unicode_nfkc_cf(0xF925, [0x62C9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F925
unicode_nfkc_cf(0xF926, [0x81D8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F926
unicode_nfkc_cf(0xF927, [0x881F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F927
unicode_nfkc_cf(0xF928, [0x5ECA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F928
unicode_nfkc_cf(0xF929, [0x6717]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F929
unicode_nfkc_cf(0xF92A, [0x6D6A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F92A
unicode_nfkc_cf(0xF92B, [0x72FC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F92B
unicode_nfkc_cf(0xF92C, [0x90CE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F92C
unicode_nfkc_cf(0xF92D, [0x4F86]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F92D
unicode_nfkc_cf(0xF92E, [0x51B7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F92E
unicode_nfkc_cf(0xF92F, [0x52DE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F92F
unicode_nfkc_cf(0xF930, [0x64C4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F930
unicode_nfkc_cf(0xF931, [0x6AD3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F931
unicode_nfkc_cf(0xF932, [0x7210]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F932
unicode_nfkc_cf(0xF933, [0x76E7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F933
unicode_nfkc_cf(0xF934, [0x8001]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F934
unicode_nfkc_cf(0xF935, [0x8606]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F935
unicode_nfkc_cf(0xF936, [0x865C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F936
unicode_nfkc_cf(0xF937, [0x8DEF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F937
unicode_nfkc_cf(0xF938, [0x9732]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F938
unicode_nfkc_cf(0xF939, [0x9B6F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F939
unicode_nfkc_cf(0xF93A, [0x9DFA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F93A
unicode_nfkc_cf(0xF93B, [0x788C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F93B
unicode_nfkc_cf(0xF93C, [0x797F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F93C
unicode_nfkc_cf(0xF93D, [0x7DA0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F93D
unicode_nfkc_cf(0xF93E, [0x83C9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F93E
unicode_nfkc_cf(0xF93F, [0x9304]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F93F
unicode_nfkc_cf(0xF940, [0x9E7F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F940
unicode_nfkc_cf(0xF941, [0x8AD6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F941
unicode_nfkc_cf(0xF942, [0x58DF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F942
unicode_nfkc_cf(0xF943, [0x5F04]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F943
unicode_nfkc_cf(0xF944, [0x7C60]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F944
unicode_nfkc_cf(0xF945, [0x807E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F945
unicode_nfkc_cf(0xF946, [0x7262]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F946
unicode_nfkc_cf(0xF947, [0x78CA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F947
unicode_nfkc_cf(0xF948, [0x8CC2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F948
unicode_nfkc_cf(0xF949, [0x96F7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F949
unicode_nfkc_cf(0xF94A, [0x58D8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F94A
unicode_nfkc_cf(0xF94B, [0x5C62]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F94B
unicode_nfkc_cf(0xF94C, [0x6A13]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F94C
unicode_nfkc_cf(0xF94D, [0x6DDA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F94D
unicode_nfkc_cf(0xF94E, [0x6F0F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F94E
unicode_nfkc_cf(0xF94F, [0x7D2F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F94F
unicode_nfkc_cf(0xF950, [0x7E37]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F950
unicode_nfkc_cf(0xF951, [0x964B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F951
unicode_nfkc_cf(0xF952, [0x52D2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F952
unicode_nfkc_cf(0xF953, [0x808B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F953
unicode_nfkc_cf(0xF954, [0x51DC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F954
unicode_nfkc_cf(0xF955, [0x51CC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F955
unicode_nfkc_cf(0xF956, [0x7A1C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F956
unicode_nfkc_cf(0xF957, [0x7DBE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F957
unicode_nfkc_cf(0xF958, [0x83F1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F958
unicode_nfkc_cf(0xF959, [0x9675]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F959
unicode_nfkc_cf(0xF95A, [0x8B80]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F95A
unicode_nfkc_cf(0xF95B, [0x62CF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F95B
unicode_nfkc_cf(0xF95C, [0x6A02]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F95C
unicode_nfkc_cf(0xF95D, [0x8AFE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F95D
unicode_nfkc_cf(0xF95E, [0x4E39]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F95E
unicode_nfkc_cf(0xF95F, [0x5BE7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F95F
unicode_nfkc_cf(0xF960, [0x6012]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F960
unicode_nfkc_cf(0xF961, [0x7387]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F961
unicode_nfkc_cf(0xF962, [0x7570]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F962
unicode_nfkc_cf(0xF963, [0x5317]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F963
unicode_nfkc_cf(0xF964, [0x78FB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F964
unicode_nfkc_cf(0xF965, [0x4FBF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F965
unicode_nfkc_cf(0xF966, [0x5FA9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F966
unicode_nfkc_cf(0xF967, [0x4E0D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F967
unicode_nfkc_cf(0xF968, [0x6CCC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F968
unicode_nfkc_cf(0xF969, [0x6578]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F969
unicode_nfkc_cf(0xF96A, [0x7D22]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F96A
unicode_nfkc_cf(0xF96B, [0x53C3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F96B
unicode_nfkc_cf(0xF96C, [0x585E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F96C
unicode_nfkc_cf(0xF96D, [0x7701]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F96D
unicode_nfkc_cf(0xF96E, [0x8449]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F96E
unicode_nfkc_cf(0xF96F, [0x8AAA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F96F
unicode_nfkc_cf(0xF970, [0x6BBA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F970
unicode_nfkc_cf(0xF971, [0x8FB0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F971
unicode_nfkc_cf(0xF972, [0x6C88]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F972
unicode_nfkc_cf(0xF973, [0x62FE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F973
unicode_nfkc_cf(0xF974, [0x82E5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F974
unicode_nfkc_cf(0xF975, [0x63A0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F975
unicode_nfkc_cf(0xF976, [0x7565]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F976
unicode_nfkc_cf(0xF977, [0x4EAE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F977
unicode_nfkc_cf(0xF978, [0x5169]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F978
unicode_nfkc_cf(0xF979, [0x51C9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F979
unicode_nfkc_cf(0xF97A, [0x6881]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F97A
unicode_nfkc_cf(0xF97B, [0x7CE7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F97B
unicode_nfkc_cf(0xF97C, [0x826F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F97C
unicode_nfkc_cf(0xF97D, [0x8AD2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F97D
unicode_nfkc_cf(0xF97E, [0x91CF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F97E
unicode_nfkc_cf(0xF97F, [0x52F5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F97F
unicode_nfkc_cf(0xF980, [0x5442]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F980
unicode_nfkc_cf(0xF981, [0x5973]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F981
unicode_nfkc_cf(0xF982, [0x5EEC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F982
unicode_nfkc_cf(0xF983, [0x65C5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F983
unicode_nfkc_cf(0xF984, [0x6FFE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F984
unicode_nfkc_cf(0xF985, [0x792A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F985
unicode_nfkc_cf(0xF986, [0x95AD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F986
unicode_nfkc_cf(0xF987, [0x9A6A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F987
unicode_nfkc_cf(0xF988, [0x9E97]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F988
unicode_nfkc_cf(0xF989, [0x9ECE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F989
unicode_nfkc_cf(0xF98A, [0x529B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F98A
unicode_nfkc_cf(0xF98B, [0x66C6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F98B
unicode_nfkc_cf(0xF98C, [0x6B77]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F98C
unicode_nfkc_cf(0xF98D, [0x8F62]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F98D
unicode_nfkc_cf(0xF98E, [0x5E74]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F98E
unicode_nfkc_cf(0xF98F, [0x6190]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F98F
unicode_nfkc_cf(0xF990, [0x6200]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F990
unicode_nfkc_cf(0xF991, [0x649A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F991
unicode_nfkc_cf(0xF992, [0x6F23]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F992
unicode_nfkc_cf(0xF993, [0x7149]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F993
unicode_nfkc_cf(0xF994, [0x7489]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F994
unicode_nfkc_cf(0xF995, [0x79CA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F995
unicode_nfkc_cf(0xF996, [0x7DF4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F996
unicode_nfkc_cf(0xF997, [0x806F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F997
unicode_nfkc_cf(0xF998, [0x8F26]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F998
unicode_nfkc_cf(0xF999, [0x84EE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F999
unicode_nfkc_cf(0xF99A, [0x9023]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F99A
unicode_nfkc_cf(0xF99B, [0x934A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F99B
unicode_nfkc_cf(0xF99C, [0x5217]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F99C
unicode_nfkc_cf(0xF99D, [0x52A3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F99D
unicode_nfkc_cf(0xF99E, [0x54BD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F99E
unicode_nfkc_cf(0xF99F, [0x70C8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F99F
unicode_nfkc_cf(0xF9A0, [0x88C2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9A0
unicode_nfkc_cf(0xF9A1, [0x8AAA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9A1
unicode_nfkc_cf(0xF9A2, [0x5EC9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9A2
unicode_nfkc_cf(0xF9A3, [0x5FF5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9A3
unicode_nfkc_cf(0xF9A4, [0x637B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9A4
unicode_nfkc_cf(0xF9A5, [0x6BAE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9A5
unicode_nfkc_cf(0xF9A6, [0x7C3E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9A6
unicode_nfkc_cf(0xF9A7, [0x7375]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9A7
unicode_nfkc_cf(0xF9A8, [0x4EE4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9A8
unicode_nfkc_cf(0xF9A9, [0x56F9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9A9
unicode_nfkc_cf(0xF9AA, [0x5BE7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9AA
unicode_nfkc_cf(0xF9AB, [0x5DBA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9AB
unicode_nfkc_cf(0xF9AC, [0x601C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9AC
unicode_nfkc_cf(0xF9AD, [0x73B2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9AD
unicode_nfkc_cf(0xF9AE, [0x7469]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9AE
unicode_nfkc_cf(0xF9AF, [0x7F9A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9AF
unicode_nfkc_cf(0xF9B0, [0x8046]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9B0
unicode_nfkc_cf(0xF9B1, [0x9234]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9B1
unicode_nfkc_cf(0xF9B2, [0x96F6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9B2
unicode_nfkc_cf(0xF9B3, [0x9748]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9B3
unicode_nfkc_cf(0xF9B4, [0x9818]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9B4
unicode_nfkc_cf(0xF9B5, [0x4F8B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9B5
unicode_nfkc_cf(0xF9B6, [0x79AE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9B6
unicode_nfkc_cf(0xF9B7, [0x91B4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9B7
unicode_nfkc_cf(0xF9B8, [0x96B8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9B8
unicode_nfkc_cf(0xF9B9, [0x60E1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9B9
unicode_nfkc_cf(0xF9BA, [0x4E86]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9BA
unicode_nfkc_cf(0xF9BB, [0x50DA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9BB
unicode_nfkc_cf(0xF9BC, [0x5BEE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9BC
unicode_nfkc_cf(0xF9BD, [0x5C3F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9BD
unicode_nfkc_cf(0xF9BE, [0x6599]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9BE
unicode_nfkc_cf(0xF9BF, [0x6A02]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9BF
unicode_nfkc_cf(0xF9C0, [0x71CE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9C0
unicode_nfkc_cf(0xF9C1, [0x7642]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9C1
unicode_nfkc_cf(0xF9C2, [0x84FC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9C2
unicode_nfkc_cf(0xF9C3, [0x907C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9C3
unicode_nfkc_cf(0xF9C4, [0x9F8D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9C4
unicode_nfkc_cf(0xF9C5, [0x6688]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9C5
unicode_nfkc_cf(0xF9C6, [0x962E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9C6
unicode_nfkc_cf(0xF9C7, [0x5289]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9C7
unicode_nfkc_cf(0xF9C8, [0x677B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9C8
unicode_nfkc_cf(0xF9C9, [0x67F3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9C9
unicode_nfkc_cf(0xF9CA, [0x6D41]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9CA
unicode_nfkc_cf(0xF9CB, [0x6E9C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9CB
unicode_nfkc_cf(0xF9CC, [0x7409]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9CC
unicode_nfkc_cf(0xF9CD, [0x7559]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9CD
unicode_nfkc_cf(0xF9CE, [0x786B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9CE
unicode_nfkc_cf(0xF9CF, [0x7D10]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9CF
unicode_nfkc_cf(0xF9D0, [0x985E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D0
unicode_nfkc_cf(0xF9D1, [0x516D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D1
unicode_nfkc_cf(0xF9D2, [0x622E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D2
unicode_nfkc_cf(0xF9D3, [0x9678]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D3
unicode_nfkc_cf(0xF9D4, [0x502B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D4
unicode_nfkc_cf(0xF9D5, [0x5D19]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D5
unicode_nfkc_cf(0xF9D6, [0x6DEA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D6
unicode_nfkc_cf(0xF9D7, [0x8F2A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D7
unicode_nfkc_cf(0xF9D8, [0x5F8B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D8
unicode_nfkc_cf(0xF9D9, [0x6144]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D9
unicode_nfkc_cf(0xF9DA, [0x6817]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9DA
unicode_nfkc_cf(0xF9DB, [0x7387]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9DB
unicode_nfkc_cf(0xF9DC, [0x9686]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9DC
unicode_nfkc_cf(0xF9DD, [0x5229]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9DD
unicode_nfkc_cf(0xF9DE, [0x540F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9DE
unicode_nfkc_cf(0xF9DF, [0x5C65]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9DF
unicode_nfkc_cf(0xF9E0, [0x6613]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9E0
unicode_nfkc_cf(0xF9E1, [0x674E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9E1
unicode_nfkc_cf(0xF9E2, [0x68A8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9E2
unicode_nfkc_cf(0xF9E3, [0x6CE5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9E3
unicode_nfkc_cf(0xF9E4, [0x7406]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9E4
unicode_nfkc_cf(0xF9E5, [0x75E2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9E5
unicode_nfkc_cf(0xF9E6, [0x7F79]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9E6
unicode_nfkc_cf(0xF9E7, [0x88CF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9E7
unicode_nfkc_cf(0xF9E8, [0x88E1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9E8
unicode_nfkc_cf(0xF9E9, [0x91CC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9E9
unicode_nfkc_cf(0xF9EA, [0x96E2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9EA
unicode_nfkc_cf(0xF9EB, [0x533F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9EB
unicode_nfkc_cf(0xF9EC, [0x6EBA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9EC
unicode_nfkc_cf(0xF9ED, [0x541D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9ED
unicode_nfkc_cf(0xF9EE, [0x71D0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9EE
unicode_nfkc_cf(0xF9EF, [0x7498]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9EF
unicode_nfkc_cf(0xF9F0, [0x85FA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9F0
unicode_nfkc_cf(0xF9F1, [0x96A3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9F1
unicode_nfkc_cf(0xF9F2, [0x9C57]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9F2
unicode_nfkc_cf(0xF9F3, [0x9E9F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9F3
unicode_nfkc_cf(0xF9F4, [0x6797]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9F4
unicode_nfkc_cf(0xF9F5, [0x6DCB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9F5
unicode_nfkc_cf(0xF9F6, [0x81E8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9F6
unicode_nfkc_cf(0xF9F7, [0x7ACB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9F7
unicode_nfkc_cf(0xF9F8, [0x7B20]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9F8
unicode_nfkc_cf(0xF9F9, [0x7C92]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9F9
unicode_nfkc_cf(0xF9FA, [0x72C0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9FA
unicode_nfkc_cf(0xF9FB, [0x7099]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9FB
unicode_nfkc_cf(0xF9FC, [0x8B58]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9FC
unicode_nfkc_cf(0xF9FD, [0x4EC0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9FD
unicode_nfkc_cf(0xF9FE, [0x8336]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9FE
unicode_nfkc_cf(0xF9FF, [0x523A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-F9FF
unicode_nfkc_cf(0xFA00, [0x5207]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA00
unicode_nfkc_cf(0xFA01, [0x5EA6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA01
unicode_nfkc_cf(0xFA02, [0x62D3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA02
unicode_nfkc_cf(0xFA03, [0x7CD6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA03
unicode_nfkc_cf(0xFA04, [0x5B85]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA04
unicode_nfkc_cf(0xFA05, [0x6D1E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA05
unicode_nfkc_cf(0xFA06, [0x66B4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA06
unicode_nfkc_cf(0xFA07, [0x8F3B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA07
unicode_nfkc_cf(0xFA08, [0x884C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA08
unicode_nfkc_cf(0xFA09, [0x964D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA09
unicode_nfkc_cf(0xFA0A, [0x898B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA0A
unicode_nfkc_cf(0xFA0B, [0x5ED3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA0B
unicode_nfkc_cf(0xFA0C, [0x5140]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA0C
unicode_nfkc_cf(0xFA0D, [0x55C0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA0D
unicode_nfkc_cf(0xFA10, [0x585A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA10
unicode_nfkc_cf(0xFA12, [0x6674]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA12
unicode_nfkc_cf(0xFA15, [0x51DE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA15
unicode_nfkc_cf(0xFA16, [0x732A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA16
unicode_nfkc_cf(0xFA17, [0x76CA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA17
unicode_nfkc_cf(0xFA18, [0x793C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA18
unicode_nfkc_cf(0xFA19, [0x795E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA19
unicode_nfkc_cf(0xFA1A, [0x7965]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA1A
unicode_nfkc_cf(0xFA1B, [0x798F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA1B
unicode_nfkc_cf(0xFA1C, [0x9756]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA1C
unicode_nfkc_cf(0xFA1D, [0x7CBE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA1D
unicode_nfkc_cf(0xFA1E, [0x7FBD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA1E
unicode_nfkc_cf(0xFA20, [0x8612]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA20
unicode_nfkc_cf(0xFA22, [0x8AF8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA22
unicode_nfkc_cf(0xFA25, [0x9038]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA25
unicode_nfkc_cf(0xFA26, [0x90FD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA26
unicode_nfkc_cf(0xFA2A, [0x98EF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA2A
unicode_nfkc_cf(0xFA2B, [0x98FC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA2B
unicode_nfkc_cf(0xFA2C, [0x9928]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA2C
unicode_nfkc_cf(0xFA2D, [0x9DB4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA2D
unicode_nfkc_cf(0xFA2E, [0x90DE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA2E
unicode_nfkc_cf(0xFA2F, [0x96B7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA2F
unicode_nfkc_cf(0xFA30, [0x4FAE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA30
unicode_nfkc_cf(0xFA31, [0x50E7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA31
unicode_nfkc_cf(0xFA32, [0x514D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA32
unicode_nfkc_cf(0xFA33, [0x52C9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA33
unicode_nfkc_cf(0xFA34, [0x52E4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA34
unicode_nfkc_cf(0xFA35, [0x5351]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA35
unicode_nfkc_cf(0xFA36, [0x559D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA36
unicode_nfkc_cf(0xFA37, [0x5606]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA37
unicode_nfkc_cf(0xFA38, [0x5668]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA38
unicode_nfkc_cf(0xFA39, [0x5840]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA39
unicode_nfkc_cf(0xFA3A, [0x58A8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA3A
unicode_nfkc_cf(0xFA3B, [0x5C64]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA3B
unicode_nfkc_cf(0xFA3C, [0x5C6E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA3C
unicode_nfkc_cf(0xFA3D, [0x6094]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA3D
unicode_nfkc_cf(0xFA3E, [0x6168]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA3E
unicode_nfkc_cf(0xFA3F, [0x618E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA3F
unicode_nfkc_cf(0xFA40, [0x61F2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA40
unicode_nfkc_cf(0xFA41, [0x654F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA41
unicode_nfkc_cf(0xFA42, [0x65E2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA42
unicode_nfkc_cf(0xFA43, [0x6691]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA43
unicode_nfkc_cf(0xFA44, [0x6885]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA44
unicode_nfkc_cf(0xFA45, [0x6D77]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA45
unicode_nfkc_cf(0xFA46, [0x6E1A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA46
unicode_nfkc_cf(0xFA47, [0x6F22]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA47
unicode_nfkc_cf(0xFA48, [0x716E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA48
unicode_nfkc_cf(0xFA49, [0x722B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA49
unicode_nfkc_cf(0xFA4A, [0x7422]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA4A
unicode_nfkc_cf(0xFA4B, [0x7891]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA4B
unicode_nfkc_cf(0xFA4C, [0x793E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA4C
unicode_nfkc_cf(0xFA4D, [0x7949]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA4D
unicode_nfkc_cf(0xFA4E, [0x7948]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA4E
unicode_nfkc_cf(0xFA4F, [0x7950]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA4F
unicode_nfkc_cf(0xFA50, [0x7956]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA50
unicode_nfkc_cf(0xFA51, [0x795D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA51
unicode_nfkc_cf(0xFA52, [0x798D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA52
unicode_nfkc_cf(0xFA53, [0x798E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA53
unicode_nfkc_cf(0xFA54, [0x7A40]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA54
unicode_nfkc_cf(0xFA55, [0x7A81]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA55
unicode_nfkc_cf(0xFA56, [0x7BC0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA56
unicode_nfkc_cf(0xFA57, [0x7DF4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA57
unicode_nfkc_cf(0xFA58, [0x7E09]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA58
unicode_nfkc_cf(0xFA59, [0x7E41]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA59
unicode_nfkc_cf(0xFA5A, [0x7F72]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA5A
unicode_nfkc_cf(0xFA5B, [0x8005]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA5B
unicode_nfkc_cf(0xFA5C, [0x81ED]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA5C
unicode_nfkc_cf(0xFA5D, [0x8279]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA5D
unicode_nfkc_cf(0xFA5E, [0x8279]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA5E
unicode_nfkc_cf(0xFA5F, [0x8457]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA5F
unicode_nfkc_cf(0xFA60, [0x8910]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA60
unicode_nfkc_cf(0xFA61, [0x8996]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA61
unicode_nfkc_cf(0xFA62, [0x8B01]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA62
unicode_nfkc_cf(0xFA63, [0x8B39]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA63
unicode_nfkc_cf(0xFA64, [0x8CD3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA64
unicode_nfkc_cf(0xFA65, [0x8D08]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA65
unicode_nfkc_cf(0xFA66, [0x8FB6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA66
unicode_nfkc_cf(0xFA67, [0x9038]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA67
unicode_nfkc_cf(0xFA68, [0x96E3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA68
unicode_nfkc_cf(0xFA69, [0x97FF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA69
unicode_nfkc_cf(0xFA6A, [0x983B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA6A
unicode_nfkc_cf(0xFA6B, [0x6075]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA6B
unicode_nfkc_cf(0xFA6C, [0x242EE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA6C
unicode_nfkc_cf(0xFA6D, [0x8218]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA6D
unicode_nfkc_cf(0xFA70, [0x4E26]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA70
unicode_nfkc_cf(0xFA71, [0x51B5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA71
unicode_nfkc_cf(0xFA72, [0x5168]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA72
unicode_nfkc_cf(0xFA73, [0x4F80]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA73
unicode_nfkc_cf(0xFA74, [0x5145]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA74
unicode_nfkc_cf(0xFA75, [0x5180]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA75
unicode_nfkc_cf(0xFA76, [0x52C7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA76
unicode_nfkc_cf(0xFA77, [0x52FA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA77
unicode_nfkc_cf(0xFA78, [0x559D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA78
unicode_nfkc_cf(0xFA79, [0x5555]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA79
unicode_nfkc_cf(0xFA7A, [0x5599]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA7A
unicode_nfkc_cf(0xFA7B, [0x55E2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA7B
unicode_nfkc_cf(0xFA7C, [0x585A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA7C
unicode_nfkc_cf(0xFA7D, [0x58B3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA7D
unicode_nfkc_cf(0xFA7E, [0x5944]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA7E
unicode_nfkc_cf(0xFA7F, [0x5954]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA7F
unicode_nfkc_cf(0xFA80, [0x5A62]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA80
unicode_nfkc_cf(0xFA81, [0x5B28]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA81
unicode_nfkc_cf(0xFA82, [0x5ED2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA82
unicode_nfkc_cf(0xFA83, [0x5ED9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA83
unicode_nfkc_cf(0xFA84, [0x5F69]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA84
unicode_nfkc_cf(0xFA85, [0x5FAD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA85
unicode_nfkc_cf(0xFA86, [0x60D8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA86
unicode_nfkc_cf(0xFA87, [0x614E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA87
unicode_nfkc_cf(0xFA88, [0x6108]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA88
unicode_nfkc_cf(0xFA89, [0x618E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA89
unicode_nfkc_cf(0xFA8A, [0x6160]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA8A
unicode_nfkc_cf(0xFA8B, [0x61F2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA8B
unicode_nfkc_cf(0xFA8C, [0x6234]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA8C
unicode_nfkc_cf(0xFA8D, [0x63C4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA8D
unicode_nfkc_cf(0xFA8E, [0x641C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA8E
unicode_nfkc_cf(0xFA8F, [0x6452]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA8F
unicode_nfkc_cf(0xFA90, [0x6556]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA90
unicode_nfkc_cf(0xFA91, [0x6674]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA91
unicode_nfkc_cf(0xFA92, [0x6717]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA92
unicode_nfkc_cf(0xFA93, [0x671B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA93
unicode_nfkc_cf(0xFA94, [0x6756]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA94
unicode_nfkc_cf(0xFA95, [0x6B79]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA95
unicode_nfkc_cf(0xFA96, [0x6BBA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA96
unicode_nfkc_cf(0xFA97, [0x6D41]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA97
unicode_nfkc_cf(0xFA98, [0x6EDB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA98
unicode_nfkc_cf(0xFA99, [0x6ECB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA99
unicode_nfkc_cf(0xFA9A, [0x6F22]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA9A
unicode_nfkc_cf(0xFA9B, [0x701E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA9B
unicode_nfkc_cf(0xFA9C, [0x716E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA9C
unicode_nfkc_cf(0xFA9D, [0x77A7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA9D
unicode_nfkc_cf(0xFA9E, [0x7235]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA9E
unicode_nfkc_cf(0xFA9F, [0x72AF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FA9F
unicode_nfkc_cf(0xFAA0, [0x732A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAA0
unicode_nfkc_cf(0xFAA1, [0x7471]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAA1
unicode_nfkc_cf(0xFAA2, [0x7506]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAA2
unicode_nfkc_cf(0xFAA3, [0x753B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAA3
unicode_nfkc_cf(0xFAA4, [0x761D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAA4
unicode_nfkc_cf(0xFAA5, [0x761F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAA5
unicode_nfkc_cf(0xFAA6, [0x76CA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAA6
unicode_nfkc_cf(0xFAA7, [0x76DB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAA7
unicode_nfkc_cf(0xFAA8, [0x76F4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAA8
unicode_nfkc_cf(0xFAA9, [0x774A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAA9
unicode_nfkc_cf(0xFAAA, [0x7740]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAAA
unicode_nfkc_cf(0xFAAB, [0x78CC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAAB
unicode_nfkc_cf(0xFAAC, [0x7AB1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAAC
unicode_nfkc_cf(0xFAAD, [0x7BC0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAAD
unicode_nfkc_cf(0xFAAE, [0x7C7B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAAE
unicode_nfkc_cf(0xFAAF, [0x7D5B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAAF
unicode_nfkc_cf(0xFAB0, [0x7DF4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAB0
unicode_nfkc_cf(0xFAB1, [0x7F3E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAB1
unicode_nfkc_cf(0xFAB2, [0x8005]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAB2
unicode_nfkc_cf(0xFAB3, [0x8352]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAB3
unicode_nfkc_cf(0xFAB4, [0x83EF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAB4
unicode_nfkc_cf(0xFAB5, [0x8779]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAB5
unicode_nfkc_cf(0xFAB6, [0x8941]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAB6
unicode_nfkc_cf(0xFAB7, [0x8986]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAB7
unicode_nfkc_cf(0xFAB8, [0x8996]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAB8
unicode_nfkc_cf(0xFAB9, [0x8ABF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAB9
unicode_nfkc_cf(0xFABA, [0x8AF8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FABA
unicode_nfkc_cf(0xFABB, [0x8ACB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FABB
unicode_nfkc_cf(0xFABC, [0x8B01]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FABC
unicode_nfkc_cf(0xFABD, [0x8AFE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FABD
unicode_nfkc_cf(0xFABE, [0x8AED]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FABE
unicode_nfkc_cf(0xFABF, [0x8B39]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FABF
unicode_nfkc_cf(0xFAC0, [0x8B8A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAC0
unicode_nfkc_cf(0xFAC1, [0x8D08]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAC1
unicode_nfkc_cf(0xFAC2, [0x8F38]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAC2
unicode_nfkc_cf(0xFAC3, [0x9072]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAC3
unicode_nfkc_cf(0xFAC4, [0x9199]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAC4
unicode_nfkc_cf(0xFAC5, [0x9276]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAC5
unicode_nfkc_cf(0xFAC6, [0x967C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAC6
unicode_nfkc_cf(0xFAC7, [0x96E3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAC7
unicode_nfkc_cf(0xFAC8, [0x9756]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAC8
unicode_nfkc_cf(0xFAC9, [0x97DB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAC9
unicode_nfkc_cf(0xFACA, [0x97FF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FACA
unicode_nfkc_cf(0xFACB, [0x980B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FACB
unicode_nfkc_cf(0xFACC, [0x983B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FACC
unicode_nfkc_cf(0xFACD, [0x9B12]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FACD
unicode_nfkc_cf(0xFACE, [0x9F9C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FACE
unicode_nfkc_cf(0xFACF, [0x2284A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FACF
unicode_nfkc_cf(0xFAD0, [0x22844]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAD0
unicode_nfkc_cf(0xFAD1, [0x233D5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAD1
unicode_nfkc_cf(0xFAD2, [0x3B9D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAD2
unicode_nfkc_cf(0xFAD3, [0x4018]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAD3
unicode_nfkc_cf(0xFAD4, [0x4039]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAD4
unicode_nfkc_cf(0xFAD5, [0x25249]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAD5
unicode_nfkc_cf(0xFAD6, [0x25CD0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAD6
unicode_nfkc_cf(0xFAD7, [0x27ED3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAD7
unicode_nfkc_cf(0xFAD8, [0x9F43]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAD8
unicode_nfkc_cf(0xFAD9, [0x9F8E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-FAD9
unicode_nfkc_cf(0xFB00, [0x0066, 0x0066]).				% L&       LATIN SMALL LIGATURE FF
unicode_nfkc_cf(0xFB01, [0x0066, 0x0069]).				% L&       LATIN SMALL LIGATURE FI
unicode_nfkc_cf(0xFB02, [0x0066, 0x006C]).				% L&       LATIN SMALL LIGATURE FL
unicode_nfkc_cf(0xFB03, [0x0066, 0x0066, 0x0069]).		% L&       LATIN SMALL LIGATURE FFI
unicode_nfkc_cf(0xFB04, [0x0066, 0x0066, 0x006C]).		% L&       LATIN SMALL LIGATURE FFL
unicode_nfkc_cf(0xFB05, [0x0073, 0x0074]).				% L&       LATIN SMALL LIGATURE LONG S T
unicode_nfkc_cf(0xFB06, [0x0073, 0x0074]).				% L&       LATIN SMALL LIGATURE ST
unicode_nfkc_cf(0xFB13, [0x0574, 0x0576]).				% L&       ARMENIAN SMALL LIGATURE MEN NOW
unicode_nfkc_cf(0xFB14, [0x0574, 0x0565]).				% L&       ARMENIAN SMALL LIGATURE MEN ECH
unicode_nfkc_cf(0xFB15, [0x0574, 0x056B]).				% L&       ARMENIAN SMALL LIGATURE MEN INI
unicode_nfkc_cf(0xFB16, [0x057E, 0x0576]).				% L&       ARMENIAN SMALL LIGATURE VEW NOW
unicode_nfkc_cf(0xFB17, [0x0574, 0x056D]).				% L&       ARMENIAN SMALL LIGATURE MEN XEH
unicode_nfkc_cf(0xFB1D, [0x05D9, 0x05B4]).				% Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_nfkc_cf(0xFB1F, [0x05F2, 0x05B7]).				% Lo       HEBREW LIGATURE YIDDISH YOD YOD PATAH
unicode_nfkc_cf(0xFB20, [0x05E2]).						% Lo       HEBREW LETTER ALTERNATIVE AYIN
unicode_nfkc_cf(0xFB21, [0x05D0]).						% Lo       HEBREW LETTER WIDE ALEF
unicode_nfkc_cf(0xFB22, [0x05D3]).						% Lo       HEBREW LETTER WIDE DALET
unicode_nfkc_cf(0xFB23, [0x05D4]).						% Lo       HEBREW LETTER WIDE HE
unicode_nfkc_cf(0xFB24, [0x05DB]).						% Lo       HEBREW LETTER WIDE KAF
unicode_nfkc_cf(0xFB25, [0x05DC]).						% Lo       HEBREW LETTER WIDE LAMED
unicode_nfkc_cf(0xFB26, [0x05DD]).						% Lo       HEBREW LETTER WIDE FINAL MEM
unicode_nfkc_cf(0xFB27, [0x05E8]).						% Lo       HEBREW LETTER WIDE RESH
unicode_nfkc_cf(0xFB28, [0x05EA]).						% Lo       HEBREW LETTER WIDE TAV
unicode_nfkc_cf(0xFB29, [0x002B]).						% Sm       HEBREW LETTER ALTERNATIVE PLUS SIGN
unicode_nfkc_cf(0xFB2A, [0x05E9, 0x05C1]).				% Lo       HEBREW LETTER SHIN WITH SHIN DOT
unicode_nfkc_cf(0xFB2B, [0x05E9, 0x05C2]).				% Lo       HEBREW LETTER SHIN WITH SIN DOT
unicode_nfkc_cf(0xFB2C, [0x05E9, 0x05BC, 0x05C1]).		% Lo       HEBREW LETTER SHIN WITH DAGESH AND SHIN DOT
unicode_nfkc_cf(0xFB2D, [0x05E9, 0x05BC, 0x05C2]).		% Lo       HEBREW LETTER SHIN WITH DAGESH AND SIN DOT
unicode_nfkc_cf(0xFB2E, [0x05D0, 0x05B7]).				% Lo       HEBREW LETTER ALEF WITH PATAH
unicode_nfkc_cf(0xFB2F, [0x05D0, 0x05B8]).				% Lo       HEBREW LETTER ALEF WITH QAMATS
unicode_nfkc_cf(0xFB30, [0x05D0, 0x05BC]).				% Lo       HEBREW LETTER ALEF WITH MAPIQ
unicode_nfkc_cf(0xFB31, [0x05D1, 0x05BC]).				% Lo       HEBREW LETTER BET WITH DAGESH
unicode_nfkc_cf(0xFB32, [0x05D2, 0x05BC]).				% Lo       HEBREW LETTER GIMEL WITH DAGESH
unicode_nfkc_cf(0xFB33, [0x05D3, 0x05BC]).				% Lo       HEBREW LETTER DALET WITH DAGESH
unicode_nfkc_cf(0xFB34, [0x05D4, 0x05BC]).				% Lo       HEBREW LETTER HE WITH MAPIQ
unicode_nfkc_cf(0xFB35, [0x05D5, 0x05BC]).				% Lo       HEBREW LETTER VAV WITH DAGESH
unicode_nfkc_cf(0xFB36, [0x05D6, 0x05BC]).				% Lo       HEBREW LETTER ZAYIN WITH DAGESH
unicode_nfkc_cf(0xFB38, [0x05D8, 0x05BC]).				% Lo       HEBREW LETTER TET WITH DAGESH
unicode_nfkc_cf(0xFB39, [0x05D9, 0x05BC]).				% Lo       HEBREW LETTER YOD WITH DAGESH
unicode_nfkc_cf(0xFB3A, [0x05DA, 0x05BC]).				% Lo       HEBREW LETTER FINAL KAF WITH DAGESH
unicode_nfkc_cf(0xFB3B, [0x05DB, 0x05BC]).				% Lo       HEBREW LETTER KAF WITH DAGESH
unicode_nfkc_cf(0xFB3C, [0x05DC, 0x05BC]).				% Lo       HEBREW LETTER LAMED WITH DAGESH
unicode_nfkc_cf(0xFB3E, [0x05DE, 0x05BC]).				% Lo       HEBREW LETTER MEM WITH DAGESH
unicode_nfkc_cf(0xFB40, [0x05E0, 0x05BC]).				% Lo       HEBREW LETTER NUN WITH DAGESH
unicode_nfkc_cf(0xFB41, [0x05E1, 0x05BC]).				% Lo       HEBREW LETTER SAMEKH WITH DAGESH
unicode_nfkc_cf(0xFB43, [0x05E3, 0x05BC]).				% Lo       HEBREW LETTER FINAL PE WITH DAGESH
unicode_nfkc_cf(0xFB44, [0x05E4, 0x05BC]).				% Lo       HEBREW LETTER PE WITH DAGESH
unicode_nfkc_cf(0xFB46, [0x05E6, 0x05BC]).				% Lo       HEBREW LETTER TSADI WITH DAGESH
unicode_nfkc_cf(0xFB47, [0x05E7, 0x05BC]).				% Lo       HEBREW LETTER QOF WITH DAGESH
unicode_nfkc_cf(0xFB48, [0x05E8, 0x05BC]).				% Lo       HEBREW LETTER RESH WITH DAGESH
unicode_nfkc_cf(0xFB49, [0x05E9, 0x05BC]).				% Lo       HEBREW LETTER SHIN WITH DAGESH
unicode_nfkc_cf(0xFB4A, [0x05EA, 0x05BC]).				% Lo       HEBREW LETTER TAV WITH DAGESH
unicode_nfkc_cf(0xFB4B, [0x05D5, 0x05B9]).				% Lo       HEBREW LETTER VAV WITH HOLAM
unicode_nfkc_cf(0xFB4C, [0x05D1, 0x05BF]).				% Lo       HEBREW LETTER BET WITH RAFE
unicode_nfkc_cf(0xFB4D, [0x05DB, 0x05BF]).				% Lo       HEBREW LETTER KAF WITH RAFE
unicode_nfkc_cf(0xFB4E, [0x05E4, 0x05BF]).				% Lo       HEBREW LETTER PE WITH RAFE
unicode_nfkc_cf(0xFB4F, [0x05D0, 0x05DC]).				% Lo       HEBREW LIGATURE ALEF LAMED
unicode_nfkc_cf(0xFB50, [0x0671]).						% Lo       ARABIC LETTER ALEF WASLA ISOLATED FORM
unicode_nfkc_cf(0xFB51, [0x0671]).						% Lo       ARABIC LETTER ALEF WASLA FINAL FORM
unicode_nfkc_cf(0xFB52, [0x067B]).						% Lo       ARABIC LETTER BEEH ISOLATED FORM..
unicode_nfkc_cf(0xFB53, [0x067B]).						% Lo       ..
unicode_nfkc_cf(0xFB54, [0x067B]).						% Lo       ..
unicode_nfkc_cf(0xFB55, [0x067B]).						% Lo       ..ARABIC LETTER BEEH MEDIAL FORM
unicode_nfkc_cf(0xFB56, [0x067E]).						% Lo       ARABIC LETTER PEH ISOLATED FORM..
unicode_nfkc_cf(0xFB57, [0x067E]).						% Lo       ..
unicode_nfkc_cf(0xFB58, [0x067E]).						% Lo       ..
unicode_nfkc_cf(0xFB59, [0x067E]).						% Lo       ..ARABIC LETTER PEH MEDIAL FORM
unicode_nfkc_cf(0xFB5A, [0x0680]).						% Lo       ARABIC LETTER BEHEH ISOLATED FORM..
unicode_nfkc_cf(0xFB5B, [0x0680]).						% Lo       ..
unicode_nfkc_cf(0xFB5C, [0x0680]).						% Lo       ..
unicode_nfkc_cf(0xFB5D, [0x0680]).						% Lo       ..ARABIC LETTER BEHEH MEDIAL FORM
unicode_nfkc_cf(0xFB5E, [0x067A]).						% Lo       ARABIC LETTER TTEHEH ISOLATED FORM..
unicode_nfkc_cf(0xFB5F, [0x067A]).						% Lo       ..
unicode_nfkc_cf(0xFB60, [0x067A]).						% Lo       ..
unicode_nfkc_cf(0xFB61, [0x067A]).						% Lo       ..ARABIC LETTER TTEHEH MEDIAL FORM
unicode_nfkc_cf(0xFB62, [0x067F]).						% Lo       ARABIC LETTER TEHEH ISOLATED FORM..
unicode_nfkc_cf(0xFB63, [0x067F]).						% Lo       ..
unicode_nfkc_cf(0xFB64, [0x067F]).						% Lo       ..
unicode_nfkc_cf(0xFB65, [0x067F]).						% Lo       ..ARABIC LETTER TEHEH MEDIAL FORM
unicode_nfkc_cf(0xFB66, [0x0679]).						% Lo       ARABIC LETTER TTEH ISOLATED FORM..
unicode_nfkc_cf(0xFB67, [0x0679]).						% Lo       ..
unicode_nfkc_cf(0xFB68, [0x0679]).						% Lo       ..
unicode_nfkc_cf(0xFB69, [0x0679]).						% Lo       ..ARABIC LETTER TTEH MEDIAL FORM
unicode_nfkc_cf(0xFB6A, [0x06A4]).						% Lo       ARABIC LETTER VEH ISOLATED FORM..
unicode_nfkc_cf(0xFB6B, [0x06A4]).						% Lo       ..
unicode_nfkc_cf(0xFB6C, [0x06A4]).						% Lo       ..
unicode_nfkc_cf(0xFB6D, [0x06A4]).						% Lo       ..ARABIC LETTER VEH MEDIAL FORM
unicode_nfkc_cf(0xFB6E, [0x06A6]).						% Lo       ARABIC LETTER PEHEH ISOLATED FORM..
unicode_nfkc_cf(0xFB6F, [0x06A6]).						% Lo       ..
unicode_nfkc_cf(0xFB70, [0x06A6]).						% Lo       ..
unicode_nfkc_cf(0xFB71, [0x06A6]).						% Lo       ..ARABIC LETTER PEHEH MEDIAL FORM
unicode_nfkc_cf(0xFB72, [0x0684]).						% Lo       ARABIC LETTER DYEH ISOLATED FORM..
unicode_nfkc_cf(0xFB73, [0x0684]).						% Lo       ..
unicode_nfkc_cf(0xFB74, [0x0684]).						% Lo       ..
unicode_nfkc_cf(0xFB75, [0x0684]).						% Lo       ..ARABIC LETTER DYEH MEDIAL FORM
unicode_nfkc_cf(0xFB76, [0x0683]).						% Lo       ARABIC LETTER NYEH ISOLATED FORM..
unicode_nfkc_cf(0xFB77, [0x0683]).						% Lo       ..
unicode_nfkc_cf(0xFB78, [0x0683]).						% Lo       ..
unicode_nfkc_cf(0xFB79, [0x0683]).						% Lo       ..ARABIC LETTER NYEH MEDIAL FORM
unicode_nfkc_cf(0xFB7A, [0x0686]).						% Lo       ARABIC LETTER TCHEH ISOLATED FORM..
unicode_nfkc_cf(0xFB7B, [0x0686]).						% Lo       ..
unicode_nfkc_cf(0xFB7C, [0x0686]).						% Lo       ..
unicode_nfkc_cf(0xFB7D, [0x0686]).						% Lo       ..ARABIC LETTER TCHEH MEDIAL FORM
unicode_nfkc_cf(0xFB7E, [0x0687]).						% Lo       ARABIC LETTER TCHEHEH ISOLATED FORM..
unicode_nfkc_cf(0xFB7F, [0x0687]).						% Lo       ..
unicode_nfkc_cf(0xFB80, [0x0687]).						% Lo       ..
unicode_nfkc_cf(0xFB81, [0x0687]).						% Lo       ..ARABIC LETTER TCHEHEH MEDIAL FORM
unicode_nfkc_cf(0xFB82, [0x068D]).						% Lo       ARABIC LETTER DDAHAL ISOLATED FORM..
unicode_nfkc_cf(0xFB83, [0x068D]).						% Lo       ..ARABIC LETTER DDAHAL FINAL FORM
unicode_nfkc_cf(0xFB84, [0x068C]).						% Lo       ARABIC LETTER DAHAL ISOLATED FORM..
unicode_nfkc_cf(0xFB85, [0x068C]).						% Lo       ..ARABIC LETTER DAHAL FINAL FORM
unicode_nfkc_cf(0xFB86, [0x068E]).						% Lo       ARABIC LETTER DUL ISOLATED FORM..
unicode_nfkc_cf(0xFB87, [0x068E]).						% Lo       ..ARABIC LETTER DUL FINAL FORM
unicode_nfkc_cf(0xFB88, [0x0688]).						% Lo       ARABIC LETTER DDAL ISOLATED FORM..
unicode_nfkc_cf(0xFB89, [0x0688]).						% Lo       ..ARABIC LETTER DDAL FINAL FORM
unicode_nfkc_cf(0xFB8A, [0x0698]).						% Lo       ARABIC LETTER JEH ISOLATED FORM..
unicode_nfkc_cf(0xFB8B, [0x0698]).						% Lo       ..ARABIC LETTER JEH FINAL FORM
unicode_nfkc_cf(0xFB8C, [0x0691]).						% Lo       ARABIC LETTER RREH ISOLATED FORM..
unicode_nfkc_cf(0xFB8D, [0x0691]).						% Lo       ..ARABIC LETTER RREH FINAL FORM
unicode_nfkc_cf(0xFB8E, [0x06A9]).						% Lo       ARABIC LETTER KEHEH ISOLATED FORM..
unicode_nfkc_cf(0xFB8F, [0x06A9]).						% Lo       ..
unicode_nfkc_cf(0xFB90, [0x06A9]).						% Lo       ..
unicode_nfkc_cf(0xFB91, [0x06A9]).						% Lo       ..ARABIC LETTER KEHEH MEDIAL FORM
unicode_nfkc_cf(0xFB92, [0x06AF]).						% Lo       ARABIC LETTER GAF ISOLATED FORM..
unicode_nfkc_cf(0xFB93, [0x06AF]).						% Lo       ..
unicode_nfkc_cf(0xFB94, [0x06AF]).						% Lo       ..
unicode_nfkc_cf(0xFB95, [0x06AF]).						% Lo       ..ARABIC LETTER GAF MEDIAL FORM
unicode_nfkc_cf(0xFB96, [0x06B3]).						% Lo       ARABIC LETTER GUEH ISOLATED FORM..
unicode_nfkc_cf(0xFB97, [0x06B3]).						% Lo       ..
unicode_nfkc_cf(0xFB98, [0x06B3]).						% Lo       ..
unicode_nfkc_cf(0xFB99, [0x06B3]).						% Lo       ..ARABIC LETTER GUEH MEDIAL FORM
unicode_nfkc_cf(0xFB9A, [0x06B1]).						% Lo       ARABIC LETTER NGOEH ISOLATED FORM..
unicode_nfkc_cf(0xFB9B, [0x06B1]).						% Lo       ..
unicode_nfkc_cf(0xFB9C, [0x06B1]).						% Lo       ..
unicode_nfkc_cf(0xFB9D, [0x06B1]).						% Lo       ..ARABIC LETTER NGOEH MEDIAL FORM
unicode_nfkc_cf(0xFB9E, [0x06BA]).						% Lo       ARABIC LETTER NOON GHUNNA ISOLATED FORM..
unicode_nfkc_cf(0xFB9F, [0x06BA]).						% Lo       ..ARABIC LETTER NOON GHUNNA FINAL FORM
unicode_nfkc_cf(0xFBA0, [0x06BB]).						% Lo       ARABIC LETTER RNOON ISOLATED FORM..
unicode_nfkc_cf(0xFBA1, [0x06BB]).						% Lo       ..
unicode_nfkc_cf(0xFBA2, [0x06BB]).						% Lo       ..
unicode_nfkc_cf(0xFBA3, [0x06BB]).						% Lo       ..ARABIC LETTER RNOON MEDIAL FORM
unicode_nfkc_cf(0xFBA4, [0x06C0]).						% Lo       ARABIC LETTER HEH WITH YEH ABOVE ISOLATED FORM..
unicode_nfkc_cf(0xFBA5, [0x06C0]).						% Lo       ..ARABIC LETTER HEH WITH YEH ABOVE FINAL FORM
unicode_nfkc_cf(0xFBA6, [0x06C1]).						% Lo       ARABIC LETTER HEH GOAL ISOLATED FORM..
unicode_nfkc_cf(0xFBA7, [0x06C1]).						% Lo       ..
unicode_nfkc_cf(0xFBA8, [0x06C1]).						% Lo       ..
unicode_nfkc_cf(0xFBA9, [0x06C1]).						% Lo       ..ARABIC LETTER HEH GOAL MEDIAL FORM
unicode_nfkc_cf(0xFBAA, [0x06BE]).						% Lo       ARABIC LETTER HEH DOACHASHMEE ISOLATED FORM..
unicode_nfkc_cf(0xFBAB, [0x06BE]).						% Lo       ..
unicode_nfkc_cf(0xFBAC, [0x06BE]).						% Lo       ..
unicode_nfkc_cf(0xFBAD, [0x06BE]).						% Lo       ..ARABIC LETTER HEH DOACHASHMEE MEDIAL FORM
unicode_nfkc_cf(0xFBAE, [0x06D2]).						% Lo       ARABIC LETTER YEH BARREE ISOLATED FORM..
unicode_nfkc_cf(0xFBAF, [0x06D2]).						% Lo       ..ARABIC LETTER YEH BARREE FINAL FORM
unicode_nfkc_cf(0xFBB0, [0x06D3]).						% Lo       ARABIC LETTER YEH BARREE WITH HAMZA ABOVE ISOLATED FORM..
unicode_nfkc_cf(0xFBB1, [0x06D3]).						% Lo       ..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
unicode_nfkc_cf(0xFBD3, [0x06AD]).						% Lo       ARABIC LETTER NG ISOLATED FORM..
unicode_nfkc_cf(0xFBD4, [0x06AD]).						% Lo       ..
unicode_nfkc_cf(0xFBD5, [0x06AD]).						% Lo       ..
unicode_nfkc_cf(0xFBD6, [0x06AD]).						% Lo       ..ARABIC LETTER NG MEDIAL FORM
unicode_nfkc_cf(0xFBD7, [0x06C7]).						% Lo       ARABIC LETTER U ISOLATED FORM..
unicode_nfkc_cf(0xFBD8, [0x06C7]).						% Lo       ..ARABIC LETTER U FINAL FORM
unicode_nfkc_cf(0xFBD9, [0x06C6]).						% Lo       ARABIC LETTER OE ISOLATED FORM..
unicode_nfkc_cf(0xFBDA, [0x06C6]).						% Lo       ..ARABIC LETTER OE FINAL FORM
unicode_nfkc_cf(0xFBDB, [0x06C8]).						% Lo       ARABIC LETTER YU ISOLATED FORM..
unicode_nfkc_cf(0xFBDC, [0x06C8]).						% Lo       ..ARABIC LETTER YU FINAL FORM
unicode_nfkc_cf(0xFBDD, [0x06C7, 0x0674]).				% Lo       ARABIC LETTER U WITH HAMZA ABOVE ISOLATED FORM
unicode_nfkc_cf(0xFBDE, [0x06CB]).						% Lo       ARABIC LETTER VE ISOLATED FORM..
unicode_nfkc_cf(0xFBDF, [0x06CB]).						% Lo       ..ARABIC LETTER VE FINAL FORM
unicode_nfkc_cf(0xFBE0, [0x06C5]).						% Lo       ARABIC LETTER KIRGHIZ OE ISOLATED FORM..
unicode_nfkc_cf(0xFBE1, [0x06C5]).						% Lo       ..ARABIC LETTER KIRGHIZ OE FINAL FORM
unicode_nfkc_cf(0xFBE2, [0x06C9]).						% Lo       ARABIC LETTER KIRGHIZ YU ISOLATED FORM..
unicode_nfkc_cf(0xFBE3, [0x06C9]).						% Lo       ..ARABIC LETTER KIRGHIZ YU FINAL FORM
unicode_nfkc_cf(0xFBE4, [0x06D0]).						% Lo       ARABIC LETTER E ISOLATED FORM..
unicode_nfkc_cf(0xFBE5, [0x06D0]).						% Lo       ..
unicode_nfkc_cf(0xFBE6, [0x06D0]).						% Lo       ..
unicode_nfkc_cf(0xFBE7, [0x06D0]).						% Lo       ..ARABIC LETTER E MEDIAL FORM
unicode_nfkc_cf(0xFBE8, [0x0649]).						% Lo       ARABIC LETTER UIGHUR KAZAKH KIRGHIZ ALEF MAKSURA INITIAL FORM
unicode_nfkc_cf(0xFBE9, [0x0649]).						% Lo       ARABIC LETTER UIGHUR KAZAKH KIRGHIZ ALEF MAKSURA MEDIAL FORM
unicode_nfkc_cf(0xFBEA, [0x0626, 0x0627]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF ISOLATED FORM
unicode_nfkc_cf(0xFBEB, [0x0626, 0x0627]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF FINAL FORM
unicode_nfkc_cf(0xFBEC, [0x0626, 0x06D5]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH AE ISOLATED FORM
unicode_nfkc_cf(0xFBED, [0x0626, 0x06D5]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH AE FINAL FORM
unicode_nfkc_cf(0xFBEE, [0x0626, 0x0648]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH WAW ISOLATED FORM
unicode_nfkc_cf(0xFBEF, [0x0626, 0x0648]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH WAW FINAL FORM
unicode_nfkc_cf(0xFBF0, [0x0626, 0x06C7]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH U ISOLATED FORM
unicode_nfkc_cf(0xFBF1, [0x0626, 0x06C7]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH U FINAL FORM
unicode_nfkc_cf(0xFBF2, [0x0626, 0x06C6]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH OE ISOLATED FORM
unicode_nfkc_cf(0xFBF3, [0x0626, 0x06C6]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH OE FINAL FORM
unicode_nfkc_cf(0xFBF4, [0x0626, 0x06C8]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YU ISOLATED FORM
unicode_nfkc_cf(0xFBF5, [0x0626, 0x06C8]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YU FINAL FORM
unicode_nfkc_cf(0xFBF6, [0x0626, 0x06D0]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E ISOLATED FORM
unicode_nfkc_cf(0xFBF7, [0x0626, 0x06D0]).				% Lo       ..
unicode_nfkc_cf(0xFBF8, [0x0626, 0x06D0]).				% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E INITIAL FORM
unicode_nfkc_cf(0xFBF9, [0x0626, 0x0649]).				% Lo       ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFBFA, [0x0626, 0x0649]).				% Lo       ..
unicode_nfkc_cf(0xFBFB, [0x0626, 0x0649]).				% Lo       ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA INITIAL FORM
unicode_nfkc_cf(0xFBFC, [0x06CC]).   					% Lo       ARABIC LETTER FARSI YEH ISOLATED FORM..
unicode_nfkc_cf(0xFBFD, [0x06CC]).   					% Lo       ..
unicode_nfkc_cf(0xFBFE, [0x06CC]).   					% Lo       ..
unicode_nfkc_cf(0xFBFF, [0x06CC]).   					% Lo       ARABIC LETTER FARSI YEH MEDIAL FORM
unicode_nfkc_cf(0xFC00, [0x0626, 0x062C]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC01, [0x0626, 0x062D]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC02, [0x0626, 0x0645]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC03, [0x0626, 0x0649]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFC04, [0x0626, 0x064A]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFC05, [0x0628, 0x062C]).   			% Lo       ARABIC LIGATURE BEH WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC06, [0x0628, 0x062D]).   			% Lo       ARABIC LIGATURE BEH WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC07, [0x0628, 0x062E]).   			% Lo       ARABIC LIGATURE BEH WITH KHAH ISOLATED FORM
unicode_nfkc_cf(0xFC08, [0x0628, 0x0645]).   			% Lo       ARABIC LIGATURE BEH WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC09, [0x0628, 0x0649]).   			% Lo       ARABIC LIGATURE BEH WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFC0A, [0x0628, 0x064A]).   			% Lo       ARABIC LIGATURE BEH WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFC0B, [0x062A, 0x062C]).   			% Lo       ARABIC LIGATURE TEH WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC0C, [0x062A, 0x062D]).   			% Lo       ARABIC LIGATURE TEH WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC0D, [0x062A, 0x062E]).   			% Lo       ARABIC LIGATURE TEH WITH KHAH ISOLATED FORM
unicode_nfkc_cf(0xFC0E, [0x062A, 0x0645]).   			% Lo       ARABIC LIGATURE TEH WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC0F, [0x062A, 0x0649]).   			% Lo       ARABIC LIGATURE TEH WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFC10, [0x062A, 0x064A]).   			% Lo       ARABIC LIGATURE TEH WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFC11, [0x062B, 0x062C]).   			% Lo       ARABIC LIGATURE THEH WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC12, [0x062B, 0x0645]).   			% Lo       ARABIC LIGATURE THEH WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC13, [0x062B, 0x0649]).   			% Lo       ARABIC LIGATURE THEH WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFC14, [0x062B, 0x064A]).   			% Lo       ARABIC LIGATURE THEH WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFC15, [0x062C, 0x062D]).   			% Lo       ARABIC LIGATURE JEEM WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC16, [0x062C, 0x0645]).   			% Lo       ARABIC LIGATURE JEEM WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC17, [0x062D, 0x062C]).   			% Lo       ARABIC LIGATURE HAH WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC18, [0x062D, 0x0645]).   			% Lo       ARABIC LIGATURE HAH WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC19, [0x062E, 0x062C]).   			% Lo       ARABIC LIGATURE KHAH WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC1A, [0x062E, 0x062D]).   			% Lo       ARABIC LIGATURE KHAH WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC1B, [0x062E, 0x0645]).   			% Lo       ARABIC LIGATURE KHAH WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC1C, [0x0633, 0x062C]).   			% Lo       ARABIC LIGATURE SEEN WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC1D, [0x0633, 0x062D]).   			% Lo       ARABIC LIGATURE SEEN WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC1E, [0x0633, 0x062E]).   			% Lo       ARABIC LIGATURE SEEN WITH KHAH ISOLATED FORM
unicode_nfkc_cf(0xFC1F, [0x0633, 0x0645]).   			% Lo       ARABIC LIGATURE SEEN WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC20, [0x0635, 0x062D]).   			% Lo       ARABIC LIGATURE SAD WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC21, [0x0635, 0x0645]).   			% Lo       ARABIC LIGATURE SAD WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC22, [0x0636, 0x062C]).   			% Lo       ARABIC LIGATURE DAD WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC23, [0x0636, 0x062D]).   			% Lo       ARABIC LIGATURE DAD WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC24, [0x0636, 0x062E]).   			% Lo       ARABIC LIGATURE DAD WITH KHAH ISOLATED FORM
unicode_nfkc_cf(0xFC25, [0x0636, 0x0645]).   			% Lo       ARABIC LIGATURE DAD WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC26, [0x0637, 0x062D]).   			% Lo       ARABIC LIGATURE TAH WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC27, [0x0637, 0x0645]).   			% Lo       ARABIC LIGATURE TAH WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC28, [0x0638, 0x0645]).   			% Lo       ARABIC LIGATURE ZAH WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC29, [0x0639, 0x062C]).   			% Lo       ARABIC LIGATURE AIN WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC2A, [0x0639, 0x0645]).   			% Lo       ARABIC LIGATURE AIN WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC2B, [0x063A, 0x062C]).   			% Lo       ARABIC LIGATURE GHAIN WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC2C, [0x063A, 0x0645]).   			% Lo       ARABIC LIGATURE GHAIN WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC2D, [0x0641, 0x062C]).   			% Lo       ARABIC LIGATURE FEH WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC2E, [0x0641, 0x062D]).   			% Lo       ARABIC LIGATURE FEH WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC2F, [0x0641, 0x062E]).   			% Lo       ARABIC LIGATURE FEH WITH KHAH ISOLATED FORM
unicode_nfkc_cf(0xFC30, [0x0641, 0x0645]).   			% Lo       ARABIC LIGATURE FEH WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC31, [0x0641, 0x0649]).   			% Lo       ARABIC LIGATURE FEH WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFC32, [0x0641, 0x064A]).   			% Lo       ARABIC LIGATURE FEH WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFC33, [0x0642, 0x062D]).   			% Lo       ARABIC LIGATURE QAF WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC34, [0x0642, 0x0645]).   			% Lo       ARABIC LIGATURE QAF WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC35, [0x0642, 0x0649]).   			% Lo       ARABIC LIGATURE QAF WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFC36, [0x0642, 0x064A]).   			% Lo       ARABIC LIGATURE QAF WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFC37, [0x0643, 0x0627]).   			% Lo       ARABIC LIGATURE KAF WITH ALEF ISOLATED FORM
unicode_nfkc_cf(0xFC38, [0x0643, 0x062C]).   			% Lo       ARABIC LIGATURE KAF WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC39, [0x0643, 0x062D]).   			% Lo       ARABIC LIGATURE KAF WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC3A, [0x0643, 0x062E]).   			% Lo       ARABIC LIGATURE KAF WITH KHAH ISOLATED FORM
unicode_nfkc_cf(0xFC3B, [0x0643, 0x0644]).   			% Lo       ARABIC LIGATURE KAF WITH LAM ISOLATED FORM
unicode_nfkc_cf(0xFC3C, [0x0643, 0x0645]).   			% Lo       ARABIC LIGATURE KAF WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC3D, [0x0643, 0x0649]).   			% Lo       ARABIC LIGATURE KAF WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFC3E, [0x0643, 0x064A]).   			% Lo       ARABIC LIGATURE KAF WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFC3F, [0x0644, 0x062C]).   			% Lo       ARABIC LIGATURE LAM WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC40, [0x0644, 0x062D]).   			% Lo       ARABIC LIGATURE LAM WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC41, [0x0644, 0x062E]).   			% Lo       ARABIC LIGATURE LAM WITH KHAH ISOLATED FORM
unicode_nfkc_cf(0xFC42, [0x0644, 0x0645]).   			% Lo       ARABIC LIGATURE LAM WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC43, [0x0644, 0x0649]).   			% Lo       ARABIC LIGATURE LAM WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFC44, [0x0644, 0x064A]).   			% Lo       ARABIC LIGATURE LAM WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFC45, [0x0645, 0x062C]).   			% Lo       ARABIC LIGATURE MEEM WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC46, [0x0645, 0x062D]).   			% Lo       ARABIC LIGATURE MEEM WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC47, [0x0645, 0x062E]).   			% Lo       ARABIC LIGATURE MEEM WITH KHAH ISOLATED FORM
unicode_nfkc_cf(0xFC48, [0x0645, 0x0645]).   			% Lo       ARABIC LIGATURE MEEM WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC49, [0x0645, 0x0649]).   			% Lo       ARABIC LIGATURE MEEM WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFC4A, [0x0645, 0x064A]).   			% Lo       ARABIC LIGATURE MEEM WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFC4B, [0x0646, 0x062C]).   			% Lo       ARABIC LIGATURE NOON WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC4C, [0x0646, 0x062D]).   			% Lo       ARABIC LIGATURE NOON WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC4D, [0x0646, 0x062E]).   			% Lo       ARABIC LIGATURE NOON WITH KHAH ISOLATED FORM
unicode_nfkc_cf(0xFC4E, [0x0646, 0x0645]).   			% Lo       ARABIC LIGATURE NOON WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC4F, [0x0646, 0x0649]).   			% Lo       ARABIC LIGATURE NOON WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFC50, [0x0646, 0x064A]).   			% Lo       ARABIC LIGATURE NOON WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFC51, [0x0647, 0x062C]).   			% Lo       ARABIC LIGATURE HEH WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC52, [0x0647, 0x0645]).   			% Lo       ARABIC LIGATURE HEH WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC53, [0x0647, 0x0649]).   			% Lo       ARABIC LIGATURE HEH WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFC54, [0x0647, 0x064A]).   			% Lo       ARABIC LIGATURE HEH WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFC55, [0x064A, 0x062C]).   			% Lo       ARABIC LIGATURE YEH WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFC56, [0x064A, 0x062D]).   			% Lo       ARABIC LIGATURE YEH WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFC57, [0x064A, 0x062E]).   			% Lo       ARABIC LIGATURE YEH WITH KHAH ISOLATED FORM
unicode_nfkc_cf(0xFC58, [0x064A, 0x0645]).   			% Lo       ARABIC LIGATURE YEH WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFC59, [0x064A, 0x0649]).   			% Lo       ARABIC LIGATURE YEH WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFC5A, [0x064A, 0x064A]).   			% Lo       ARABIC LIGATURE YEH WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFC5B, [0x0630, 0x0670]).   			% Lo       ARABIC LIGATURE THAL WITH SUPERSCRIPT ALEF ISOLATED FORM
unicode_nfkc_cf(0xFC5C, [0x0631, 0x0670]).   			% Lo       ARABIC LIGATURE REH WITH SUPERSCRIPT ALEF ISOLATED FORM
unicode_nfkc_cf(0xFC5D, [0x0649, 0x0670]).   			% Lo       ARABIC LIGATURE ALEF MAKSURA WITH SUPERSCRIPT ALEF ISOLATED FORM
unicode_nfkc_cf(0xFC5E, [0x0020, 0x064C, 0x0651]).		% Lo       ARABIC LIGATURE SHADDA WITH DAMMATAN ISOLATED FORM
unicode_nfkc_cf(0xFC5F, [0x0020, 0x064D, 0x0651]).		% Lo       ARABIC LIGATURE SHADDA WITH KASRATAN ISOLATED FORM
unicode_nfkc_cf(0xFC60, [0x0020, 0x064E, 0x0651]).		% Lo       ARABIC LIGATURE SHADDA WITH FATHA ISOLATED FORM
unicode_nfkc_cf(0xFC61, [0x0020, 0x064F, 0x0651]).		% Lo       ARABIC LIGATURE SHADDA WITH DAMMA ISOLATED FORM
unicode_nfkc_cf(0xFC62, [0x0020, 0x0650, 0x0651]).		% Lo       ARABIC LIGATURE SHADDA WITH KASRA ISOLATED FORM
unicode_nfkc_cf(0xFC63, [0x0020, 0x0651, 0x0670]).		% Lo       ARABIC LIGATURE SHADDA WITH SUPERSCRIPT ALEF ISOLATED FORM
unicode_nfkc_cf(0xFC64, [0x0626, 0x0631]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH REH FINAL FORM
unicode_nfkc_cf(0xFC65, [0x0626, 0x0632]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ZAIN FINAL FORM
unicode_nfkc_cf(0xFC66, [0x0626, 0x0645]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFC67, [0x0626, 0x0646]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH NOON FINAL FORM
unicode_nfkc_cf(0xFC68, [0x0626, 0x0649]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFC69, [0x0626, 0x064A]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YEH FINAL FORM
unicode_nfkc_cf(0xFC6A, [0x0628, 0x0631]).   			% Lo       ARABIC LIGATURE BEH WITH REH FINAL FORM
unicode_nfkc_cf(0xFC6B, [0x0628, 0x0632]).   			% Lo       ARABIC LIGATURE BEH WITH ZAIN FINAL FORM
unicode_nfkc_cf(0xFC6C, [0x0628, 0x0645]).   			% Lo       ARABIC LIGATURE BEH WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFC6D, [0x0628, 0x0646]).   			% Lo       ARABIC LIGATURE BEH WITH NOON FINAL FORM
unicode_nfkc_cf(0xFC6E, [0x0628, 0x0649]).   			% Lo       ARABIC LIGATURE BEH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFC6F, [0x0628, 0x064A]).   			% Lo       ARABIC LIGATURE BEH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFC70, [0x062A, 0x0631]).   			% Lo       ARABIC LIGATURE TEH WITH REH FINAL FORM
unicode_nfkc_cf(0xFC71, [0x062A, 0x0632]).   			% Lo       ARABIC LIGATURE TEH WITH ZAIN FINAL FORM
unicode_nfkc_cf(0xFC72, [0x062A, 0x0645]).   			% Lo       ARABIC LIGATURE TEH WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFC73, [0x062A, 0x0646]).   			% Lo       ARABIC LIGATURE TEH WITH NOON FINAL FORM
unicode_nfkc_cf(0xFC74, [0x062A, 0x0649]).   			% Lo       ARABIC LIGATURE TEH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFC75, [0x062A, 0x064A]).   			% Lo       ARABIC LIGATURE TEH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFC76, [0x062B, 0x0631]).   			% Lo       ARABIC LIGATURE THEH WITH REH FINAL FORM
unicode_nfkc_cf(0xFC77, [0x062B, 0x0632]).   			% Lo       ARABIC LIGATURE THEH WITH ZAIN FINAL FORM
unicode_nfkc_cf(0xFC78, [0x062B, 0x0645]).   			% Lo       ARABIC LIGATURE THEH WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFC79, [0x062B, 0x0646]).   			% Lo       ARABIC LIGATURE THEH WITH NOON FINAL FORM
unicode_nfkc_cf(0xFC7A, [0x062B, 0x0649]).   			% Lo       ARABIC LIGATURE THEH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFC7B, [0x062B, 0x064A]).   			% Lo       ARABIC LIGATURE THEH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFC7C, [0x0641, 0x0649]).   			% Lo       ARABIC LIGATURE FEH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFC7D, [0x0641, 0x064A]).   			% Lo       ARABIC LIGATURE FEH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFC7E, [0x0642, 0x0649]).   			% Lo       ARABIC LIGATURE QAF WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFC7F, [0x0642, 0x064A]).   			% Lo       ARABIC LIGATURE QAF WITH YEH FINAL FORM
unicode_nfkc_cf(0xFC80, [0x0643, 0x0627]).   			% Lo       ARABIC LIGATURE KAF WITH ALEF FINAL FORM
unicode_nfkc_cf(0xFC81, [0x0643, 0x0644]).   			% Lo       ARABIC LIGATURE KAF WITH LAM FINAL FORM
unicode_nfkc_cf(0xFC82, [0x0643, 0x0645]).   			% Lo       ARABIC LIGATURE KAF WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFC83, [0x0643, 0x0649]).   			% Lo       ARABIC LIGATURE KAF WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFC84, [0x0643, 0x064A]).   			% Lo       ARABIC LIGATURE KAF WITH YEH FINAL FORM
unicode_nfkc_cf(0xFC85, [0x0644, 0x0645]).   			% Lo       ARABIC LIGATURE LAM WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFC86, [0x0644, 0x0649]).   			% Lo       ARABIC LIGATURE LAM WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFC87, [0x0644, 0x064A]).   			% Lo       ARABIC LIGATURE LAM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFC88, [0x0645, 0x0627]).   			% Lo       ARABIC LIGATURE MEEM WITH ALEF FINAL FORM
unicode_nfkc_cf(0xFC89, [0x0645, 0x0645]).   			% Lo       ARABIC LIGATURE MEEM WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFC8A, [0x0646, 0x0631]).   			% Lo       ARABIC LIGATURE NOON WITH REH FINAL FORM
unicode_nfkc_cf(0xFC8B, [0x0646, 0x0632]).   			% Lo       ARABIC LIGATURE NOON WITH ZAIN FINAL FORM
unicode_nfkc_cf(0xFC8C, [0x0646, 0x0645]).   			% Lo       ARABIC LIGATURE NOON WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFC8D, [0x0646, 0x0646]).   			% Lo       ARABIC LIGATURE NOON WITH NOON FINAL FORM
unicode_nfkc_cf(0xFC8E, [0x0646, 0x0649]).   			% Lo       ARABIC LIGATURE NOON WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFC8F, [0x0646, 0x064A]).   			% Lo       ARABIC LIGATURE NOON WITH YEH FINAL FORM
unicode_nfkc_cf(0xFC90, [0x0649, 0x0670]).   			% Lo       ARABIC LIGATURE ALEF MAKSURA WITH SUPERSCRIPT ALEF FINAL FORM
unicode_nfkc_cf(0xFC91, [0x064A, 0x0631]).   			% Lo       ARABIC LIGATURE YEH WITH REH FINAL FORM
unicode_nfkc_cf(0xFC92, [0x064A, 0x0632]).   			% Lo       ARABIC LIGATURE YEH WITH ZAIN FINAL FORM
unicode_nfkc_cf(0xFC93, [0x064A, 0x0645]).   			% Lo       ARABIC LIGATURE YEH WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFC94, [0x064A, 0x0646]).   			% Lo       ARABIC LIGATURE YEH WITH NOON FINAL FORM
unicode_nfkc_cf(0xFC95, [0x064A, 0x0649]).   			% Lo       ARABIC LIGATURE YEH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFC96, [0x064A, 0x064A]).   			% Lo       ARABIC LIGATURE YEH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFC97, [0x0626, 0x062C]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFC98, [0x0626, 0x062D]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFC99, [0x0626, 0x062E]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFC9A, [0x0626, 0x0645]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFC9B, [0x0626, 0x0647]).   			% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH HEH INITIAL FORM
unicode_nfkc_cf(0xFC9C, [0x0628, 0x062C]).   			% Lo       ARABIC LIGATURE BEH WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFC9D, [0x0628, 0x062D]).   			% Lo       ARABIC LIGATURE BEH WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFC9E, [0x0628, 0x062E]).   			% Lo       ARABIC LIGATURE BEH WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFC9F, [0x0628, 0x0645]).   			% Lo       ARABIC LIGATURE BEH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCA0, [0x0628, 0x0647]).   			% Lo       ARABIC LIGATURE BEH WITH HEH INITIAL FORM
unicode_nfkc_cf(0xFCA1, [0x062A, 0x062C]).   			% Lo       ARABIC LIGATURE TEH WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCA2, [0x062A, 0x062D]).   			% Lo       ARABIC LIGATURE TEH WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCA3, [0x062A, 0x062E]).   			% Lo       ARABIC LIGATURE TEH WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFCA4, [0x062A, 0x0645]).   			% Lo       ARABIC LIGATURE TEH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCA5, [0x062A, 0x0647]).   			% Lo       ARABIC LIGATURE TEH WITH HEH INITIAL FORM
unicode_nfkc_cf(0xFCA6, [0x062B, 0x0645]).   			% Lo       ARABIC LIGATURE THEH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCA7, [0x062C, 0x062D]).   			% Lo       ARABIC LIGATURE JEEM WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCA8, [0x062C, 0x0645]).   			% Lo       ARABIC LIGATURE JEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCA9, [0x062D, 0x062C]).   			% Lo       ARABIC LIGATURE HAH WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCAA, [0x062D, 0x0645]).   			% Lo       ARABIC LIGATURE HAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCAB, [0x062E, 0x062C]).   			% Lo       ARABIC LIGATURE KHAH WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCAC, [0x062E, 0x0645]).   			% Lo       ARABIC LIGATURE KHAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCAD, [0x0633, 0x062C]).   			% Lo       ARABIC LIGATURE SEEN WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCAE, [0x0633, 0x062D]).   			% Lo       ARABIC LIGATURE SEEN WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCAF, [0x0633, 0x062E]).   			% Lo       ARABIC LIGATURE SEEN WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFCB0, [0x0633, 0x0645]).   			% Lo       ARABIC LIGATURE SEEN WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCB1, [0x0635, 0x062D]).   			% Lo       ARABIC LIGATURE SAD WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCB2, [0x0635, 0x062E]).   			% Lo       ARABIC LIGATURE SAD WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFCB3, [0x0635, 0x0645]).   			% Lo       ARABIC LIGATURE SAD WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCB4, [0x0636, 0x062C]).   			% Lo       ARABIC LIGATURE DAD WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCB5, [0x0636, 0x062D]).   			% Lo       ARABIC LIGATURE DAD WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCB6, [0x0636, 0x062E]).   			% Lo       ARABIC LIGATURE DAD WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFCB7, [0x0636, 0x0645]).   			% Lo       ARABIC LIGATURE DAD WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCB8, [0x0637, 0x062D]).   			% Lo       ARABIC LIGATURE TAH WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCB9, [0x0638, 0x0645]).   			% Lo       ARABIC LIGATURE ZAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCBA, [0x0639, 0x062C]).   			% Lo       ARABIC LIGATURE AIN WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCBB, [0x0639, 0x0645]).   			% Lo       ARABIC LIGATURE AIN WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCBC, [0x063A, 0x062C]).   			% Lo       ARABIC LIGATURE GHAIN WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCBD, [0x063A, 0x0645]).   			% Lo       ARABIC LIGATURE GHAIN WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCBE, [0x0641, 0x062C]).   			% Lo       ARABIC LIGATURE FEH WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCBF, [0x0641, 0x062D]).   			% Lo       ARABIC LIGATURE FEH WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCC0, [0x0641, 0x062E]).   			% Lo       ARABIC LIGATURE FEH WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFCC1, [0x0641, 0x0645]).   			% Lo       ARABIC LIGATURE FEH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCC2, [0x0642, 0x062D]).   			% Lo       ARABIC LIGATURE QAF WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCC3, [0x0642, 0x0645]).   			% Lo       ARABIC LIGATURE QAF WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCC4, [0x0643, 0x062C]).   			% Lo       ARABIC LIGATURE KAF WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCC5, [0x0643, 0x062D]).   			% Lo       ARABIC LIGATURE KAF WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCC6, [0x0643, 0x062E]).   			% Lo       ARABIC LIGATURE KAF WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFCC7, [0x0643, 0x0644]).   			% Lo       ARABIC LIGATURE KAF WITH LAM INITIAL FORM
unicode_nfkc_cf(0xFCC8, [0x0643, 0x0645]).   			% Lo       ARABIC LIGATURE KAF WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCC9, [0x0644, 0x062C]).   			% Lo       ARABIC LIGATURE LAM WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCCA, [0x0644, 0x062D]).   			% Lo       ARABIC LIGATURE LAM WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCCB, [0x0644, 0x062E]).   			% Lo       ARABIC LIGATURE LAM WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFCCC, [0x0644, 0x0645]).   			% Lo       ARABIC LIGATURE LAM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCCD, [0x0644, 0x0647]).   			% Lo       ARABIC LIGATURE LAM WITH HEH INITIAL FORM
unicode_nfkc_cf(0xFCCE, [0x0645, 0x062C]).   			% Lo       ARABIC LIGATURE MEEM WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCCF, [0x0645, 0x062D]).   			% Lo       ARABIC LIGATURE MEEM WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCD0, [0x0645, 0x062E]).   			% Lo       ARABIC LIGATURE MEEM WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFCD1, [0x0645, 0x0645]).   			% Lo       ARABIC LIGATURE MEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCD2, [0x0646, 0x062C]).   			% Lo       ARABIC LIGATURE NOON WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCD3, [0x0646, 0x062D]).   			% Lo       ARABIC LIGATURE NOON WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCD4, [0x0646, 0x062E]).   			% Lo       ARABIC LIGATURE NOON WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFCD5, [0x0646, 0x0645]).   			% Lo       ARABIC LIGATURE NOON WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCD6, [0x0646, 0x0647]).   	  		% Lo       ARABIC LIGATURE NOON WITH HEH INITIAL FORM
unicode_nfkc_cf(0xFCD7, [0x0647, 0x062C]).   	  		% Lo       ARABIC LIGATURE HEH WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCD8, [0x0647, 0x0645]).   	  		% Lo       ARABIC LIGATURE HEH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCD9, [0x0647, 0x0670]).   	  		% Lo       ARABIC LIGATURE HEH WITH SUPERSCRIPT ALEF INITIAL FORM
unicode_nfkc_cf(0xFCDA, [0x064A, 0x062C]).   	  		% Lo       ARABIC LIGATURE YEH WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFCDB, [0x064A, 0x062D]).   	  		% Lo       ARABIC LIGATURE YEH WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFCDC, [0x064A, 0x062E]).   	  		% Lo       ARABIC LIGATURE YEH WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFCDD, [0x064A, 0x0645]).   	  		% Lo       ARABIC LIGATURE YEH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFCDE, [0x064A, 0x0647]).   	  		% Lo       ARABIC LIGATURE YEH WITH HEH INITIAL FORM
unicode_nfkc_cf(0xFCDF, [0x0626, 0x0645]).   	  		% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM MEDIAL FORM
unicode_nfkc_cf(0xFCE0, [0x0626, 0x0647]).   	  		% Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH HEH MEDIAL FORM
unicode_nfkc_cf(0xFCE1, [0x0628, 0x0645]).   	  		% Lo       ARABIC LIGATURE BEH WITH MEEM MEDIAL FORM
unicode_nfkc_cf(0xFCE2, [0x0628, 0x0647]).   	  		% Lo       ARABIC LIGATURE BEH WITH HEH MEDIAL FORM
unicode_nfkc_cf(0xFCE3, [0x062A, 0x0645]).   	  		% Lo       ARABIC LIGATURE TEH WITH MEEM MEDIAL FORM
unicode_nfkc_cf(0xFCE4, [0x062A, 0x0647]).   	  		% Lo       ARABIC LIGATURE TEH WITH HEH MEDIAL FORM
unicode_nfkc_cf(0xFCE5, [0x062B, 0x0645]).   	  		% Lo       ARABIC LIGATURE THEH WITH MEEM MEDIAL FORM
unicode_nfkc_cf(0xFCE6, [0x062B, 0x0647]).   	  		% Lo       ARABIC LIGATURE THEH WITH HEH MEDIAL FORM
unicode_nfkc_cf(0xFCE7, [0x0633, 0x0645]).   	  		% Lo       ARABIC LIGATURE SEEN WITH MEEM MEDIAL FORM
unicode_nfkc_cf(0xFCE8, [0x0633, 0x0647]).   	  		% Lo       ARABIC LIGATURE SEEN WITH HEH MEDIAL FORM
unicode_nfkc_cf(0xFCE9, [0x0634, 0x0645]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH MEEM MEDIAL FORM
unicode_nfkc_cf(0xFCEA, [0x0634, 0x0647]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH HEH MEDIAL FORM
unicode_nfkc_cf(0xFCEB, [0x0643, 0x0644]).   	  		% Lo       ARABIC LIGATURE KAF WITH LAM MEDIAL FORM
unicode_nfkc_cf(0xFCEC, [0x0643, 0x0645]).   	  		% Lo       ARABIC LIGATURE KAF WITH MEEM MEDIAL FORM
unicode_nfkc_cf(0xFCED, [0x0644, 0x0645]).   	  		% Lo       ARABIC LIGATURE LAM WITH MEEM MEDIAL FORM
unicode_nfkc_cf(0xFCEE, [0x0646, 0x0645]).   	  		% Lo       ARABIC LIGATURE NOON WITH MEEM MEDIAL FORM
unicode_nfkc_cf(0xFCEF, [0x0646, 0x0647]).   	  		% Lo       ARABIC LIGATURE NOON WITH HEH MEDIAL FORM
unicode_nfkc_cf(0xFCF0, [0x064A, 0x0645]).   	  		% Lo       ARABIC LIGATURE YEH WITH MEEM MEDIAL FORM
unicode_nfkc_cf(0xFCF1, [0x064A, 0x0647]).   	  		% Lo       ARABIC LIGATURE YEH WITH HEH MEDIAL FORM
unicode_nfkc_cf(0xFCF2, [0x0640, 0x064E, 0x0651]).		% Lo       ARABIC LIGATURE SHADDA WITH FATHA MEDIAL FORM
unicode_nfkc_cf(0xFCF3, [0x0640, 0x064F, 0x0651]).		% Lo       ARABIC LIGATURE SHADDA WITH DAMMA MEDIAL FORM
unicode_nfkc_cf(0xFCF4, [0x0640, 0x0650, 0x0651]).		% Lo       ARABIC LIGATURE SHADDA WITH KASRA MEDIAL FORM
unicode_nfkc_cf(0xFCF5, [0x0637, 0x0649]).   	  		% Lo       ARABIC LIGATURE TAH WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFCF6, [0x0637, 0x064A]).   	  		% Lo       ARABIC LIGATURE TAH WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFCF7, [0x0639, 0x0649]).   	  		% Lo       ARABIC LIGATURE AIN WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFCF8, [0x0639, 0x064A]).   	  		% Lo       ARABIC LIGATURE AIN WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFCF9, [0x063A, 0x0649]).   	  		% Lo       ARABIC LIGATURE GHAIN WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFCFA, [0x063A, 0x064A]).   	  		% Lo       ARABIC LIGATURE GHAIN WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFCFB, [0x0633, 0x0649]).   	  		% Lo       ARABIC LIGATURE SEEN WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFCFC, [0x0633, 0x064A]).   	  		% Lo       ARABIC LIGATURE SEEN WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFCFD, [0x0634, 0x0649]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFCFE, [0x0634, 0x064A]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFCFF, [0x062D, 0x0649]).   	  		% Lo       ARABIC LIGATURE HAH WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFD00, [0x062D, 0x064A]).   	  		% Lo       ARABIC LIGATURE HAH WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFD01, [0x062C, 0x0649]).   	  		% Lo       ARABIC LIGATURE JEEM WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFD02, [0x062C, 0x064A]).   	  		% Lo       ARABIC LIGATURE JEEM WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFD03, [0x062E, 0x0649]).   	  		% Lo       ARABIC LIGATURE KHAH WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFD04, [0x062E, 0x064A]).   	  		% Lo       ARABIC LIGATURE KHAH WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFD05, [0x0635, 0x0649]).   	  		% Lo       ARABIC LIGATURE SAD WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFD06, [0x0635, 0x064A]).   	  		% Lo       ARABIC LIGATURE SAD WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFD07, [0x0636, 0x0649]).   	  		% Lo       ARABIC LIGATURE DAD WITH ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFD08, [0x0636, 0x064A]).   	  		% Lo       ARABIC LIGATURE DAD WITH YEH ISOLATED FORM
unicode_nfkc_cf(0xFD09, [0x0634, 0x062C]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH JEEM ISOLATED FORM
unicode_nfkc_cf(0xFD0A, [0x0634, 0x062D]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH HAH ISOLATED FORM
unicode_nfkc_cf(0xFD0B, [0x0634, 0x062E]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH KHAH ISOLATED FORM
unicode_nfkc_cf(0xFD0C, [0x0634, 0x0645]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH MEEM ISOLATED FORM
unicode_nfkc_cf(0xFD0D, [0x0634, 0x0631]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH REH ISOLATED FORM
unicode_nfkc_cf(0xFD0E, [0x0633, 0x0631]).   	  		% Lo       ARABIC LIGATURE SEEN WITH REH ISOLATED FORM
unicode_nfkc_cf(0xFD0F, [0x0635, 0x0631]).   	  		% Lo       ARABIC LIGATURE SAD WITH REH ISOLATED FORM
unicode_nfkc_cf(0xFD10, [0x0636, 0x0631]).   	  		% Lo       ARABIC LIGATURE DAD WITH REH ISOLATED FORM
unicode_nfkc_cf(0xFD11, [0x0637, 0x0649]).   	  		% Lo       ARABIC LIGATURE TAH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD12, [0x0637, 0x064A]).   	  		% Lo       ARABIC LIGATURE TAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD13, [0x0639, 0x0649]).   	  		% Lo       ARABIC LIGATURE AIN WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD14, [0x0639, 0x064A]).   	  		% Lo       ARABIC LIGATURE AIN WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD15, [0x063A, 0x0649]).   	  		% Lo       ARABIC LIGATURE GHAIN WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD16, [0x063A, 0x064A]).   	  		% Lo       ARABIC LIGATURE GHAIN WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD17, [0x0633, 0x0649]).   	  		% Lo       ARABIC LIGATURE SEEN WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD18, [0x0633, 0x064A]).   	  		% Lo       ARABIC LIGATURE SEEN WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD19, [0x0634, 0x0649]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD1A, [0x0634, 0x064A]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD1B, [0x062D, 0x0649]).   	  		% Lo       ARABIC LIGATURE HAH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD1C, [0x062D, 0x064A]).   	  		% Lo       ARABIC LIGATURE HAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD1D, [0x062C, 0x0649]).   	  		% Lo       ARABIC LIGATURE JEEM WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD1E, [0x062C, 0x064A]).   	  		% Lo       ARABIC LIGATURE JEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD1F, [0x062E, 0x0649]).   	  		% Lo       ARABIC LIGATURE KHAH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD20, [0x062E, 0x064A]).   	  		% Lo       ARABIC LIGATURE KHAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD21, [0x0635, 0x0649]).   	  		% Lo       ARABIC LIGATURE SAD WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD22, [0x0635, 0x064A]).   	  		% Lo       ARABIC LIGATURE SAD WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD23, [0x0636, 0x0649]).   	  		% Lo       ARABIC LIGATURE DAD WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD24, [0x0636, 0x064A]).   	  		% Lo       ARABIC LIGATURE DAD WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD25, [0x0634, 0x062C]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH JEEM FINAL FORM
unicode_nfkc_cf(0xFD26, [0x0634, 0x062D]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH HAH FINAL FORM
unicode_nfkc_cf(0xFD27, [0x0634, 0x062E]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH KHAH FINAL FORM
unicode_nfkc_cf(0xFD28, [0x0634, 0x0645]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD29, [0x0634, 0x0631]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH REH FINAL FORM
unicode_nfkc_cf(0xFD2A, [0x0633, 0x0631]).   	  		% Lo       ARABIC LIGATURE SEEN WITH REH FINAL FORM
unicode_nfkc_cf(0xFD2B, [0x0635, 0x0631]).   	  		% Lo       ARABIC LIGATURE SAD WITH REH FINAL FORM
unicode_nfkc_cf(0xFD2C, [0x0636, 0x0631]).   	  		% Lo       ARABIC LIGATURE DAD WITH REH FINAL FORM
unicode_nfkc_cf(0xFD2D, [0x0634, 0x062C]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFD2E, [0x0634, 0x062D]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFD2F, [0x0634, 0x062E]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFD30, [0x0634, 0x0645]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD31, [0x0633, 0x0647]).   	  		% Lo       ARABIC LIGATURE SEEN WITH HEH INITIAL FORM
unicode_nfkc_cf(0xFD32, [0x0634, 0x0647]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH HEH INITIAL FORM
unicode_nfkc_cf(0xFD33, [0x0637, 0x0645]).   	  		% Lo       ARABIC LIGATURE TAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD34, [0x0633, 0x062C]).   	  		% Lo       ARABIC LIGATURE SEEN WITH JEEM MEDIAL FORM
unicode_nfkc_cf(0xFD35, [0x0633, 0x062D]).   	  		% Lo       ARABIC LIGATURE SEEN WITH HAH MEDIAL FORM
unicode_nfkc_cf(0xFD36, [0x0633, 0x062E]).   	  		% Lo       ARABIC LIGATURE SEEN WITH KHAH MEDIAL FORM
unicode_nfkc_cf(0xFD37, [0x0634, 0x062C]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH JEEM MEDIAL FORM
unicode_nfkc_cf(0xFD38, [0x0634, 0x062D]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH HAH MEDIAL FORM
unicode_nfkc_cf(0xFD39, [0x0634, 0x062E]).   	  		% Lo       ARABIC LIGATURE SHEEN WITH KHAH MEDIAL FORM
unicode_nfkc_cf(0xFD3A, [0x0637, 0x0645]).   	  		% Lo       ARABIC LIGATURE TAH WITH MEEM MEDIAL FORM
unicode_nfkc_cf(0xFD3B, [0x0638, 0x0645]).   	  		% Lo       ARABIC LIGATURE ZAH WITH MEEM MEDIAL FORM
unicode_nfkc_cf(0xFD3C, [0x0627, 0x064B]).   	  		% Lo       ARABIC LIGATURE ALEF WITH FATHATAN FINAL FORM
unicode_nfkc_cf(0xFD3D, [0x0627, 0x064B]).   	  		% Lo       ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
unicode_nfkc_cf(0xFD50, [0x062A, 0x062C, 0x0645]).		% Lo    ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD51, [0x062A, 0x062D, 0x062C]).		% Lo    ARABIC LIGATURE TEH WITH HAH WITH JEEM FINAL FORM
unicode_nfkc_cf(0xFD52, [0x062A, 0x062D, 0x062C]).		% Lo    ARABIC LIGATURE TEH WITH HAH WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFD53, [0x062A, 0x062D, 0x0645]).		% Lo    ARABIC LIGATURE TEH WITH HAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD54, [0x062A, 0x062E, 0x0645]).		% Lo    ARABIC LIGATURE TEH WITH KHAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD55, [0x062A, 0x0645, 0x062C]).		% Lo    ARABIC LIGATURE TEH WITH MEEM WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFD56, [0x062A, 0x0645, 0x062D]).		% Lo    ARABIC LIGATURE TEH WITH MEEM WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFD57, [0x062A, 0x0645, 0x062E]).		% Lo    ARABIC LIGATURE TEH WITH MEEM WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFD58, [0x062C, 0x0645, 0x062D]).		% Lo    ARABIC LIGATURE JEEM WITH MEEM WITH HAH FINAL FORM
unicode_nfkc_cf(0xFD59, [0x062C, 0x0645, 0x062D]).		% Lo    ARABIC LIGATURE JEEM WITH MEEM WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFD5A, [0x062D, 0x0645, 0x064A]).		% Lo    ARABIC LIGATURE HAH WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD5B, [0x062D, 0x0645, 0x0649]).		% Lo    ARABIC LIGATURE HAH WITH MEEM WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD5C, [0x0633, 0x062D, 0x062C]).		% Lo    ARABIC LIGATURE SEEN WITH HAH WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFD5D, [0x0633, 0x062C, 0x062D]).		% Lo    ARABIC LIGATURE SEEN WITH JEEM WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFD5E, [0x0633, 0x062C, 0x0649]).		% Lo    ARABIC LIGATURE SEEN WITH JEEM WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD5F, [0x0633, 0x0645, 0x062D]).		% Lo    ARABIC LIGATURE SEEN WITH MEEM WITH HAH FINAL FORM
unicode_nfkc_cf(0xFD60, [0x0633, 0x0645, 0x062D]).		% Lo    ARABIC LIGATURE SEEN WITH MEEM WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFD61, [0x0633, 0x0645, 0x062C]).		% Lo    ARABIC LIGATURE SEEN WITH MEEM WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFD62, [0x0633, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE SEEN WITH MEEM WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD63, [0x0633, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE SEEN WITH MEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD64, [0x0635, 0x062D, 0x062D]).		% Lo       ARABIC LIGATURE SAD WITH HAH WITH HAH FINAL FORM
unicode_nfkc_cf(0xFD65, [0x0635, 0x062D, 0x062D]).		% Lo       ARABIC LIGATURE SAD WITH HAH WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFD66, [0x0635, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE SAD WITH MEEM WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD67, [0x0634, 0x062D, 0x0645]).		% Lo       ARABIC LIGATURE SHEEN WITH HAH WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD68, [0x0634, 0x062D, 0x0645]).		% Lo       ARABIC LIGATURE SHEEN WITH HAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD69, [0x0634, 0x062C, 0x064A]).		% Lo       ARABIC LIGATURE SHEEN WITH JEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD6A, [0x0634, 0x0645, 0x062E]).		% Lo       ARABIC LIGATURE SHEEN WITH MEEM WITH KHAH FINAL FORM
unicode_nfkc_cf(0xFD6B, [0x0634, 0x0645, 0x062E]).		% Lo       ARABIC LIGATURE SHEEN WITH MEEM WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFD6C, [0x0634, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE SHEEN WITH MEEM WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD6D, [0x0634, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE SHEEN WITH MEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD6E, [0x0636, 0x062D, 0x0649]).		% Lo       ARABIC LIGATURE DAD WITH HAH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD6F, [0x0636, 0x062E, 0x0645]).		% Lo       ARABIC LIGATURE DAD WITH KHAH WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD70, [0x0636, 0x062E, 0x0645]).		% Lo       ARABIC LIGATURE DAD WITH KHAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD71, [0x0637, 0x0645, 0x062D]).		% Lo       ARABIC LIGATURE TAH WITH MEEM WITH HAH FINAL FORM
unicode_nfkc_cf(0xFD72, [0x0637, 0x0645, 0x062D]).		% Lo       ARABIC LIGATURE TAH WITH MEEM WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFD73, [0x0637, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE TAH WITH MEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD74, [0x0637, 0x0645, 0x064A]).		% Lo       ARABIC LIGATURE TAH WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD75, [0x0639, 0x062C, 0x0645]).		% Lo       ARABIC LIGATURE AIN WITH JEEM WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD76, [0x0639, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE AIN WITH MEEM WITH MEEM FINAL FORM..
unicode_nfkc_cf(0xFD77, [0x0639, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE AIN WITH MEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD78, [0x0639, 0x0645, 0x0649]).		% Lo       ARABIC LIGATURE AIN WITH MEEM WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD79, [0x063A, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE GHAIN WITH MEEM WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD7A, [0x063A, 0x0645, 0x064A]).		% Lo       ARABIC LIGATURE GHAIN WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD7B, [0x063A, 0x0645, 0x0649]).		% Lo       ARABIC LIGATURE GHAIN WITH MEEM WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD7C, [0x0641, 0x062E, 0x0645]).		% Lo       ARABIC LIGATURE FEH WITH KHAH WITH MEEM FINAL FORM..
unicode_nfkc_cf(0xFD7D, [0x0641, 0x062E, 0x0645]).		% Lo       ARABIC LIGATURE FEH WITH KHAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD7E, [0x0642, 0x0645, 0x062D]).		% Lo       ARABIC LIGATURE QAF WITH MEEM WITH HAH FINAL FORM
unicode_nfkc_cf(0xFD7F, [0x0642, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE QAF WITH MEEM WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD80, [0x0644, 0x062D, 0x0645]).		% Lo       ARABIC LIGATURE LAM WITH HAH WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD81, [0x0644, 0x062D, 0x064A]).		% Lo       ARABIC LIGATURE LAM WITH HAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD82, [0x0644, 0x062D, 0x0649]).		% Lo       ARABIC LIGATURE LAM WITH HAH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD83, [0x0644, 0x062C, 0x062C]).		% Lo       ARABIC LIGATURE LAM WITH JEEM WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFD84, [0x0644, 0x062C, 0x062C]).		% Lo       ARABIC LIGATURE LAM WITH JEEM WITH JEEM FINAL FORM
unicode_nfkc_cf(0xFD85, [0x0644, 0x062E, 0x0645]).		% Lo       ARABIC LIGATURE LAM WITH KHAH WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD86, [0x0644, 0x062E, 0x0645]).		% Lo       ARABIC LIGATURE LAM WITH KHAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD87, [0x0644, 0x0645, 0x062D]).		% Lo       ARABIC LIGATURE LAM WITH MEEM WITH HAH FINAL FORM
unicode_nfkc_cf(0xFD88, [0x0644, 0x0645, 0x062D]).		% Lo       ARABIC LIGATURE LAM WITH MEEM WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFD89, [0x0645, 0x062D, 0x062C]).		% Lo       ARABIC LIGATURE MEEM WITH HAH WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFD8A, [0x0645, 0x062D, 0x0645]).		% Lo       ARABIC LIGATURE MEEM WITH HAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD8B, [0x0645, 0x062D, 0x064A]).		% Lo       ARABIC LIGATURE MEEM WITH HAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD8C, [0x0645, 0x062C, 0x062D]).		% Lo       ARABIC LIGATURE MEEM WITH JEEM WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFD8D, [0x0645, 0x062C, 0x0645]).		% Lo       ARABIC LIGATURE MEEM WITH JEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD8E, [0x0645, 0x062E, 0x062C]).		% Lo       ARABIC LIGATURE MEEM WITH KHAH WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFD8F, [0x0645, 0x062E, 0x0645]).		% Lo       ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD92, [0x0645, 0x062C, 0x062E]).		% Lo       ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM
unicode_nfkc_cf(0xFD93, [0x0647, 0x0645, 0x062C]).		% Lo       ARABIC LIGATURE HEH WITH MEEM WITH JEEM INITIAL FORM
unicode_nfkc_cf(0xFD94, [0x0647, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE HEH WITH MEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD95, [0x0646, 0x062D, 0x0645]).		% Lo       ARABIC LIGATURE NOON WITH HAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD96, [0x0646, 0x062D, 0x0649]).		% Lo       ARABIC LIGATURE NOON WITH HAH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD97, [0x0646, 0x062C, 0x0645]).		% Lo       ARABIC LIGATURE NOON WITH JEEM WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD98, [0x0646, 0x062C, 0x0645]).		% Lo       ARABIC LIGATURE NOON WITH JEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD99, [0x0646, 0x062C, 0x0649]).		% Lo       ARABIC LIGATURE NOON WITH JEEM WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD9A, [0x0646, 0x0645, 0x064A]).		% Lo       ARABIC LIGATURE NOON WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD9B, [0x0646, 0x0645, 0x0649]).		% Lo       ARABIC LIGATURE NOON WITH MEEM WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFD9C, [0x064A, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE YEH WITH MEEM WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFD9D, [0x064A, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE YEH WITH MEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFD9E, [0x0628, 0x062E, 0x064A]).		% Lo       ARABIC LIGATURE BEH WITH KHAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFD9F, [0x062A, 0x062C, 0x064A]).		% Lo       ARABIC LIGATURE TEH WITH JEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDA0, [0x062A, 0x062C, 0x0649]).		% Lo       ARABIC LIGATURE TEH WITH JEEM WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFDA1, [0x062A, 0x062E, 0x064A]).		% Lo       ARABIC LIGATURE TEH WITH KHAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDA2, [0x062A, 0x062E, 0x0649]).		% Lo       ARABIC LIGATURE TEH WITH KHAH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFDA3, [0x062A, 0x0645, 0x064A]).		% Lo       ARABIC LIGATURE TEH WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDA4, [0x062A, 0x0645, 0x0649]).		% Lo       ARABIC LIGATURE TEH WITH MEEM WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFDA5, [0x062C, 0x0645, 0x064A]).		% Lo       ARABIC LIGATURE JEEM WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDA6, [0x062C, 0x062D, 0x0649]).		% Lo       ARABIC LIGATURE JEEM WITH HAH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFDA7, [0x062C, 0x0645, 0x0649]).		% Lo       ARABIC LIGATURE JEEM WITH MEEM WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFDA8, [0x0633, 0x062E, 0x0649]).		% Lo       ARABIC LIGATURE SEEN WITH KHAH WITH ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFDA9, [0x0635, 0x062D, 0x064A]).		% Lo       ARABIC LIGATURE SAD WITH HAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDAA, [0x0634, 0x062D, 0x064A]).		% Lo       ARABIC LIGATURE SHEEN WITH HAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDAB, [0x0636, 0x062D, 0x064A]).		% Lo       ARABIC LIGATURE DAD WITH HAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDAC, [0x0644, 0x062C, 0x064A]).		% Lo       ARABIC LIGATURE LAM WITH JEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDAD, [0x0644, 0x0645, 0x064A]).		% Lo       ARABIC LIGATURE LAM WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDAE, [0x064A, 0x062D, 0x064A]).		% Lo       ARABIC LIGATURE YEH WITH HAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDAF, [0x064A, 0x062C, 0x064A]).		% Lo       ARABIC LIGATURE YEH WITH JEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDB0, [0x064A, 0x0645, 0x064A]).		% Lo       ARABIC LIGATURE YEH WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDB1, [0x0645, 0x0645, 0x064A]).		% Lo       ARABIC LIGATURE MEEM WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDB2, [0x0642, 0x0645, 0x064A]).		% Lo       ARABIC LIGATURE QAF WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDB3, [0x0646, 0x062D, 0x064A]).		% Lo       ARABIC LIGATURE NOON WITH HAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDB4, [0x0642, 0x0645, 0x062D]).		% Lo       ARABIC LIGATURE QAF WITH MEEM WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFDB5, [0x0644, 0x062D, 0x0645]).		% Lo       ARABIC LIGATURE LAM WITH HAH WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFDB6, [0x0639, 0x0645, 0x064A]).		% Lo       ARABIC LIGATURE AIN WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDB7, [0x0643, 0x0645, 0x064A]).		% Lo       ARABIC LIGATURE KAF WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDB8, [0x0646, 0x062C, 0x062D]).		% Lo       ARABIC LIGATURE NOON WITH JEEM WITH HAH INITIAL FORM
unicode_nfkc_cf(0xFDB9, [0x0645, 0x062E, 0x064A]).		% Lo       ARABIC LIGATURE MEEM WITH KHAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDBA, [0x0644, 0x062C, 0x0645]).		% Lo       ARABIC LIGATURE LAM WITH JEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFDBB, [0x0643, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE KAF WITH MEEM WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFDBC, [0x0644, 0x062C, 0x0645]).		% Lo       ARABIC LIGATURE LAM WITH JEEM WITH MEEM FINAL FORM
unicode_nfkc_cf(0xFDBD, [0x0646, 0x062C, 0x062D]).		% Lo       ARABIC LIGATURE NOON WITH JEEM WITH HAH FINAL FORM
unicode_nfkc_cf(0xFDBE, [0x062C, 0x062D, 0x064A]).		% Lo       ARABIC LIGATURE JEEM WITH HAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDBF, [0x062D, 0x062C, 0x064A]).		% Lo       ARABIC LIGATURE HAH WITH JEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDC0, [0x0645, 0x062C, 0x064A]).		% Lo       ARABIC LIGATURE MEEM WITH JEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDC1, [0x0641, 0x0645, 0x064A]).		% Lo       ARABIC LIGATURE FEH WITH MEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDC2, [0x0628, 0x062D, 0x064A]).		% Lo       ARABIC LIGATURE BEH WITH HAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDC3, [0x0643, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE KAF WITH MEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFDC4, [0x0639, 0x062C, 0x0645]).		% Lo       ARABIC LIGATURE AIN WITH JEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFDC5, [0x0635, 0x0645, 0x0645]).		% Lo       ARABIC LIGATURE SAD WITH MEEM WITH MEEM INITIAL FORM
unicode_nfkc_cf(0xFDC6, [0x0633, 0x062E, 0x064A]).		% Lo       ARABIC LIGATURE SEEN WITH KHAH WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDC7, [0x0646, 0x062C, 0x064A]).		% Lo       ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
unicode_nfkc_cf(0xFDF0, [0x0635, 0x0644, 0x06D2]).		% Lo       ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM
unicode_nfkc_cf(0xFDF1, [0x0642, 0x0644, 0x06D2]).		% Lo       ARABIC LIGATURE QALA USED AS KORANIC STOP SIGN ISOLATED FORM
unicode_nfkc_cf(0xFDF2, [0x0627, 0x0644, 0x0644, 0x0647]).	% Lo   ARABIC LIGATURE ALLAH ISOLATED FORM
unicode_nfkc_cf(0xFDF3, [0x0627, 0x0643, 0x0628, 0x0631]).	% Lo   ARABIC LIGATURE AKBAR ISOLATED FORM
unicode_nfkc_cf(0xFDF4, [0x0645, 0x062D, 0x0645, 0x062F]).	% Lo   ARABIC LIGATURE MOHAMMAD ISOLATED FORM
unicode_nfkc_cf(0xFDF5, [0x0635, 0x0644, 0x0639, 0x0645]).	% Lo   ARABIC LIGATURE SALAM ISOLATED FORM
unicode_nfkc_cf(0xFDF6, [0x0631, 0x0633, 0x0648, 0x0644]).	% Lo   ARABIC LIGATURE RASOUL ISOLATED FORM
unicode_nfkc_cf(0xFDF7, [0x0639, 0x0644, 0x064A, 0x0647]).	% Lo   ARABIC LIGATURE ALAYHE ISOLATED FORM
unicode_nfkc_cf(0xFDF8, [0x0648, 0x0633, 0x0644, 0x0645]).	% Lo   ARABIC LIGATURE WASALLAM ISOLATED FORM
unicode_nfkc_cf(0xFDF9, [0x0635, 0x0644, 0x0649]).		% Lo       ARABIC LIGATURE SALLA ISOLATED FORM
unicode_nfkc_cf(0xFDFA, [0x0635, 0x0644, 0x0649, 0x0020, 0x0627, 0x0644, 0x0644, 0x0647, 0x0020, 0x0639, 0x0644, 0x064A, 0x0647, 0x0020, 0x0648, 0x0633, 0x0644, 0x0645]).	% Lo ARABIC LIGATURE SALLALLAHOU ALAYHE WASALLAM
unicode_nfkc_cf(0xFDFB, [0x062C, 0x0644, 0x0020, 0x062C, 0x0644, 0x0627, 0x0644, 0x0647]).	% Lo ARABIC LIGATURE JALLAJALALOUHOU
unicode_nfkc_cf(0xFDFC, [0x0631, 0x06CC, 0x0627, 0x0644]).	% Sc   RIAL SIGN
%unicode_nfkc_cf(0xFE00, 0xFE0F    ; NFKC_CF;                # Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
unicode_nfkc_cf(0xFE10, [0x002C]).						% Po       PRESENTATION FORM FOR VERTICAL COMMA
unicode_nfkc_cf(0xFE11, [0x3001]).						% Po       PRESENTATION FORM FOR VERTICAL IDEOGRAPHIC COMMA
unicode_nfkc_cf(0xFE12, [0x3002]).						% Po       PRESENTATION FORM FOR VERTICAL IDEOGRAPHIC FULL STOP
unicode_nfkc_cf(0xFE13, [0x003A]).						% Po       PRESENTATION FORM FOR VERTICAL COLON
unicode_nfkc_cf(0xFE14, [0x003B]).						% Po       PRESENTATION FORM FOR VERTICAL SEMICOLON
unicode_nfkc_cf(0xFE15, [0x0021]).						% Po       PRESENTATION FORM FOR VERTICAL EXCLAMATION MARK
unicode_nfkc_cf(0xFE16, [0x003F]).						% Po       PRESENTATION FORM FOR VERTICAL QUESTION MARK
unicode_nfkc_cf(0xFE17, [0x3016]).						% Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE LENTICULAR BRACKET
unicode_nfkc_cf(0xFE18, [0x3017]).						% Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET
unicode_nfkc_cf(0xFE19, [0x002E, 0x002E, 0x002E]).		% Po       PRESENTATION FORM FOR VERTICAL HORIZONTAL ELLIPSIS
unicode_nfkc_cf(0xFE30, [0x002E, 0x002E]).				% Po       PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
unicode_nfkc_cf(0xFE31, [0x2014]).						% Pd       PRESENTATION FORM FOR VERTICAL EM DASH
unicode_nfkc_cf(0xFE32, [0x2013]).						% Pd       PRESENTATION FORM FOR VERTICAL EN DASH
unicode_nfkc_cf(0xFE33, [0x005F]).						% Pc       PRESENTATION FORM FOR VERTICAL LOW LINE
unicode_nfkc_cf(0xFE34, [0x005F]).						% Pc       PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
unicode_nfkc_cf(0xFE35, [0x0028]).						% Ps       PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
unicode_nfkc_cf(0xFE36, [0x0029]).						% Pe       PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
unicode_nfkc_cf(0xFE37, [0x007B]).						% Ps       PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
unicode_nfkc_cf(0xFE38, [0x007D]).						% Pe       PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
unicode_nfkc_cf(0xFE39, [0x3014]).						% Ps       PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
unicode_nfkc_cf(0xFE3A, [0x3015]).						% Pe       PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
unicode_nfkc_cf(0xFE3B, [0x3010]).						% Ps       PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
unicode_nfkc_cf(0xFE3C, [0x3011]).						% Pe       PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
unicode_nfkc_cf(0xFE3D, [0x300A]).						% Ps       PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
unicode_nfkc_cf(0xFE3E, [0x300B]).						% Pe       PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
unicode_nfkc_cf(0xFE3F, [0x3008]).						% Ps       PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
unicode_nfkc_cf(0xFE40, [0x3009]).						% Pe       PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
unicode_nfkc_cf(0xFE41, [0x300C]).						% Ps       PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
unicode_nfkc_cf(0xFE42, [0x300D]).						% Pe       PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
unicode_nfkc_cf(0xFE43, [0x300E]).						% Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
unicode_nfkc_cf(0xFE44, [0x300F]).						% Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
unicode_nfkc_cf(0xFE47, [0x005B]).						% Ps       PRESENTATION FORM FOR VERTICAL LEFT SQUARE BRACKET
unicode_nfkc_cf(0xFE48, [0x005D]).						% Pe       PRESENTATION FORM FOR VERTICAL RIGHT SQUARE BRACKET
unicode_nfkc_cf(0xFE49, [0x0020, 0x0305]).				% Po       DASHED OVERLINE
unicode_nfkc_cf(0xFE4A, [0x0020, 0x0305]).				% Po       ..
unicode_nfkc_cf(0xFE4B, [0x0020, 0x0305]).				% Po       ..
unicode_nfkc_cf(0xFE4C, [0x0020, 0x0305]).				% Po       DOUBLE WAVY OVERLINE
unicode_nfkc_cf(0xFE4D, [0x005F]).						% Pc       DASHED LOW LINE
unicode_nfkc_cf(0xFE4E, [0x005F]).						% Pc       ..
unicode_nfkc_cf(0xFE4F, [0x005F]).						% Pc       WAVY LOW LINE
unicode_nfkc_cf(0xFE50, [0x002C]).						% Po       SMALL COMMA
unicode_nfkc_cf(0xFE51, [0x3001]).						% Po       SMALL IDEOGRAPHIC COMMA
unicode_nfkc_cf(0xFE52, [0x002E]).						% Po       SMALL FULL STOP
unicode_nfkc_cf(0xFE54, [0x003B]).						% Po       SMALL SEMICOLON
unicode_nfkc_cf(0xFE55, [0x003A]).						% Po       SMALL COLON
unicode_nfkc_cf(0xFE56, [0x003F]).						% Po       SMALL QUESTION MARK
unicode_nfkc_cf(0xFE57, [0x0021]).						% Po       SMALL EXCLAMATION MARK
unicode_nfkc_cf(0xFE58, [0x2014]).						% Pd       SMALL EM DASH
unicode_nfkc_cf(0xFE59, [0x0028]).						% Ps       SMALL LEFT PARENTHESIS
unicode_nfkc_cf(0xFE5A, [0x0029]).						% Pe       SMALL RIGHT PARENTHESIS
unicode_nfkc_cf(0xFE5B, [0x007B]).						% Ps       SMALL LEFT CURLY BRACKET
unicode_nfkc_cf(0xFE5C, [0x007D]).						% Pe       SMALL RIGHT CURLY BRACKET
unicode_nfkc_cf(0xFE5D, [0x3014]).						% Ps       SMALL LEFT TORTOISE SHELL BRACKET
unicode_nfkc_cf(0xFE5E, [0x3015]).						% Pe       SMALL RIGHT TORTOISE SHELL BRACKET
unicode_nfkc_cf(0xFE5F, [0x0023]).						% Po       SMALL NUMBER SIGN
unicode_nfkc_cf(0xFE60, [0x0026]).						% Po       SMALL AMPERSAND
unicode_nfkc_cf(0xFE61, [0x002A]).						% Po       SMALL ASTERISK
unicode_nfkc_cf(0xFE62, [0x002B]).						% Sm       SMALL PLUS SIGN
unicode_nfkc_cf(0xFE63, [0x002D]).						% Pd       SMALL HYPHEN-MINUS
unicode_nfkc_cf(0xFE64, [0x003C]).						% Sm       SMALL LESS-THAN SIGN
unicode_nfkc_cf(0xFE65, [0x003E]).						% Sm       SMALL GREATER-THAN SIGN
unicode_nfkc_cf(0xFE66, [0x003D]).						% Sm       SMALL EQUALS SIGN
unicode_nfkc_cf(0xFE68, [0x005C]).						% Po       SMALL REVERSE SOLIDUS
unicode_nfkc_cf(0xFE69, [0x0024]).						% Sc       SMALL DOLLAR SIGN
unicode_nfkc_cf(0xFE6A, [0x0025]).						% Po       SMALL PERCENT SIGN
unicode_nfkc_cf(0xFE6B, [0x0040]).						% Po       SMALL COMMERCIAL AT
unicode_nfkc_cf(0xFE70, [0x0020, 0x064B]).				% Lo       ARABIC FATHATAN ISOLATED FORM
unicode_nfkc_cf(0xFE71, [0x0640, 0x064B]).				% Lo       ARABIC TATWEEL WITH FATHATAN ABOVE
unicode_nfkc_cf(0xFE72, [0x0020, 0x064C]).				% Lo       ARABIC DAMMATAN ISOLATED FORM
unicode_nfkc_cf(0xFE74, [0x0020, 0x064D]).				% Lo       ARABIC KASRATAN ISOLATED FORM
unicode_nfkc_cf(0xFE76, [0x0020, 0x064E]).				% Lo       ARABIC FATHA ISOLATED FORM
unicode_nfkc_cf(0xFE77, [0x0640, 0x064E]).				% Lo       ARABIC FATHA MEDIAL FORM
unicode_nfkc_cf(0xFE78, [0x0020, 0x064F]).				% Lo       ARABIC DAMMA ISOLATED FORM
unicode_nfkc_cf(0xFE79, [0x0640, 0x064F]).				% Lo       ARABIC DAMMA MEDIAL FORM
unicode_nfkc_cf(0xFE7A, [0x0020, 0x0650]).				% Lo       ARABIC KASRA ISOLATED FORM
unicode_nfkc_cf(0xFE7B, [0x0640, 0x0650]).				% Lo       ARABIC KASRA MEDIAL FORM
unicode_nfkc_cf(0xFE7C, [0x0020, 0x0651]).				% Lo       ARABIC SHADDA ISOLATED FORM
unicode_nfkc_cf(0xFE7D, [0x0640, 0x0651]).				% Lo       ARABIC SHADDA MEDIAL FORM
unicode_nfkc_cf(0xFE7E, [0x0020, 0x0652]).				% Lo       ARABIC SUKUN ISOLATED FORM
unicode_nfkc_cf(0xFE7F, [0x0640, 0x0652]).				% Lo       ARABIC SUKUN MEDIAL FORM
unicode_nfkc_cf(0xFE80, [0x0621]).						% Lo       ARABIC LETTER HAMZA ISOLATED FORM
unicode_nfkc_cf(0xFE81, [0x0622]).						% Lo       ARABIC LETTER ALEF WITH MADDA ABOVE ISOLATED FORM
unicode_nfkc_cf(0xFE82, [0x0622]).						% Lo       ARABIC LETTER ALEF WITH MADDA ABOVE FINAL FORM
unicode_nfkc_cf(0xFE83, [0x0623]).						% Lo       ARABIC LETTER ALEF WITH HAMZA ABOVE ISOLATED FORM
unicode_nfkc_cf(0xFE84, [0x0623]).						% Lo       ARABIC LETTER ALEF WITH HAMZA ABOVE FINAL FORM
unicode_nfkc_cf(0xFE85, [0x0624]).						% Lo       ARABIC LETTER WAW WITH HAMZA ABOVE ISOLATED FORM
unicode_nfkc_cf(0xFE86, [0x0624]).						% Lo       ARABIC LETTER WAW WITH HAMZA ABOVE FINAL FORM
unicode_nfkc_cf(0xFE87, [0x0625]).						% Lo       ARABIC LETTER ALEF WITH HAMZA BELOW ISOLATED FORM
unicode_nfkc_cf(0xFE88, [0x0625]).						% Lo       ARABIC LETTER ALEF WITH HAMZA BELOW FINAL FORM
unicode_nfkc_cf(0xFE89, [0x0626]).						% Lo       ARABIC LETTER YEH WITH HAMZA ABOVE ISOLATED FORM..
unicode_nfkc_cf(0xFE8A, [0x0626]).						% Lo       ..
unicode_nfkc_cf(0xFE8B, [0x0626]).						% Lo       ..
unicode_nfkc_cf(0xFE8C, [0x0626]).						% Lo       ..ARABIC LETTER YEH WITH HAMZA ABOVE MEDIAL FORM
unicode_nfkc_cf(0xFE8D, [0x0627]).						% Lo       ARABIC LETTER ALEF ISOLATED FORM..
unicode_nfkc_cf(0xFE8E, [0x0627]).						% Lo       ..ARABIC LETTER ALEF FINAL FORM
unicode_nfkc_cf(0xFE8F, [0x0628]).						% Lo       ARABIC LETTER BEH ISOLATED FORM..
unicode_nfkc_cf(0xFE90, [0x0628]).						% Lo       ..
unicode_nfkc_cf(0xFE91, [0x0628]).						% Lo       ..
unicode_nfkc_cf(0xFE92, [0x0628]).						% Lo       ..ARABIC LETTER BEH MEDIAL FORM
unicode_nfkc_cf(0xFE93, [0x0629]).						% Lo       ARABIC LETTER TEH MARBUTA ISOLATED FORM..
unicode_nfkc_cf(0xFE94, [0x0629]).						% Lo       ..ARABIC LETTER TEH MARBUTA FINAL FORM
unicode_nfkc_cf(0xFE95, [0x062A]).						% Lo       ARABIC LETTER TEH ISOLATED FORM..
unicode_nfkc_cf(0xFE96, [0x062A]).						% Lo       ..
unicode_nfkc_cf(0xFE97, [0x062A]).						% Lo       ..
unicode_nfkc_cf(0xFE98, [0x062A]).						% Lo       ..ARABIC LETTER TEH MEDIAL FORM
unicode_nfkc_cf(0xFE99, [0x062B]).						% Lo       ARABIC LETTER THEH ISOLATED FORM..
unicode_nfkc_cf(0xFE9A, [0x062B]).						% Lo       ..
unicode_nfkc_cf(0xFE9B, [0x062B]).						% Lo       ..
unicode_nfkc_cf(0xFE9C, [0x062B]).						% Lo       ..ARABIC LETTER THEH MEDIAL FORM
unicode_nfkc_cf(0xFE9D, [0x062C]).						% Lo       ARABIC LETTER JEEM ISOLATED FORM..
unicode_nfkc_cf(0xFE9E, [0x062C]).						% Lo       ..
unicode_nfkc_cf(0xFE9F, [0x062C]).						% Lo       ..
unicode_nfkc_cf(0xFEA0, [0x062C]).						% Lo       ..ARABIC LETTER JEEM MEDIAL FORM
unicode_nfkc_cf(0xFEA1, [0x062D]).						% Lo       ARABIC LETTER HAH ISOLATED FORM..
unicode_nfkc_cf(0xFEA2, [0x062D]).						% Lo       ..
unicode_nfkc_cf(0xFEA3, [0x062D]).						% Lo       ..
unicode_nfkc_cf(0xFEA4, [0x062D]).						% Lo       ..ARABIC LETTER HAH MEDIAL FORM
unicode_nfkc_cf(0xFEA5, [0x062E]).						% Lo       ARABIC LETTER KHAH ISOLATED FORM..
unicode_nfkc_cf(0xFEA6, [0x062E]).						% Lo       ..
unicode_nfkc_cf(0xFEA7, [0x062E]).						% Lo       ..
unicode_nfkc_cf(0xFEA8, [0x062E]).						% Lo       ..ARABIC LETTER KHAH MEDIAL FORM
unicode_nfkc_cf(0xFEA9, [0x062F]).						% Lo       ARABIC LETTER DAL ISOLATED FORM..
unicode_nfkc_cf(0xFEAA, [0x062F]).						% Lo       ..ARABIC LETTER DAL FINAL FORM
unicode_nfkc_cf(0xFEAB, [0x0630]).						% Lo       ARABIC LETTER THAL ISOLATED FORM..
unicode_nfkc_cf(0xFEAC, [0x0630]).						% Lo       ..ARABIC LETTER THAL FINAL FORM
unicode_nfkc_cf(0xFEAD, [0x0631]).						% Lo       ARABIC LETTER REH ISOLATED FORM..
unicode_nfkc_cf(0xFEAE, [0x0631]).						% Lo       ..ARABIC LETTER REH FINAL FORM
unicode_nfkc_cf(0xFEAF, [0x0632]).						% Lo       ARABIC LETTER ZAIN ISOLATED FORM..
unicode_nfkc_cf(0xFEB0, [0x0632]).						% Lo       ..ARABIC LETTER ZAIN FINAL FORM
unicode_nfkc_cf(0xFEB1, [0x0633]).						% Lo       ARABIC LETTER SEEN ISOLATED FORM..
unicode_nfkc_cf(0xFEB2, [0x0633]).						% Lo       ..
unicode_nfkc_cf(0xFEB3, [0x0633]).						% Lo       ..
unicode_nfkc_cf(0xFEB4, [0x0633]).						% Lo       ..ARABIC LETTER SEEN MEDIAL FORM
unicode_nfkc_cf(0xFEB5, [0x0634]).						% Lo       ARABIC LETTER SHEEN ISOLATED FORM..
unicode_nfkc_cf(0xFEB6, [0x0634]).						% Lo       ..
unicode_nfkc_cf(0xFEB7, [0x0634]).						% Lo       ..
unicode_nfkc_cf(0xFEB8, [0x0634]).						% Lo       ..ARABIC LETTER SHEEN MEDIAL FORM
unicode_nfkc_cf(0xFEB9, [0x0635]).						% Lo       ARABIC LETTER SAD ISOLATED FORM..
unicode_nfkc_cf(0xFEBA, [0x0635]).						% Lo       ..
unicode_nfkc_cf(0xFEBB, [0x0635]).						% Lo       ..
unicode_nfkc_cf(0xFEBC, [0x0635]).						% Lo       ..ARABIC LETTER SAD MEDIAL FORM
unicode_nfkc_cf(0xFEBD, [0x0636]).						% Lo       ARABIC LETTER DAD ISOLATED FORM..
unicode_nfkc_cf(0xFEBE, [0x0636]).						% Lo       ..
unicode_nfkc_cf(0xFEBF, [0x0636]).						% Lo       ..
unicode_nfkc_cf(0xFEC0, [0x0636]).						% Lo       ..ARABIC LETTER DAD MEDIAL FORM
unicode_nfkc_cf(0xFEC1, [0x0637]).						% Lo       ARABIC LETTER TAH ISOLATED FORM..
unicode_nfkc_cf(0xFEC2, [0x0637]).						% Lo       ..
unicode_nfkc_cf(0xFEC3, [0x0637]).						% Lo       ..
unicode_nfkc_cf(0xFEC4, [0x0637]).						% Lo       ..ARABIC LETTER TAH MEDIAL FORM
unicode_nfkc_cf(0xFEC5, [0x0638]).						% Lo       ARABIC LETTER ZAH ISOLATED FORM..
unicode_nfkc_cf(0xFEC6, [0x0638]).						% Lo       ..
unicode_nfkc_cf(0xFEC7, [0x0638]).						% Lo       ..
unicode_nfkc_cf(0xFEC8, [0x0638]).						% Lo       ..ARABIC LETTER ZAH MEDIAL FORM
unicode_nfkc_cf(0xFEC9, [0x0639]).						% Lo       ARABIC LETTER AIN ISOLATED FORM..
unicode_nfkc_cf(0xFECA, [0x0639]).						% Lo       ..
unicode_nfkc_cf(0xFECB, [0x0639]).						% Lo       ..
unicode_nfkc_cf(0xFECC, [0x0639]).						% Lo       ..ARABIC LETTER AIN MEDIAL FORM
unicode_nfkc_cf(0xFECD, [0x063A]).						% Lo       ARABIC LETTER GHAIN ISOLATED FORM..
unicode_nfkc_cf(0xFECE, [0x063A]).						% Lo       ..
unicode_nfkc_cf(0xFECF, [0x063A]).						% Lo       ..
unicode_nfkc_cf(0xFED0, [0x063A]).						% Lo       ..ARABIC LETTER GHAIN MEDIAL FORM
unicode_nfkc_cf(0xFED1, [0x0641]).						% Lo       ARABIC LETTER FEH ISOLATED FORM..
unicode_nfkc_cf(0xFED2, [0x0641]).						% Lo       ..
unicode_nfkc_cf(0xFED3, [0x0641]).						% Lo       ..
unicode_nfkc_cf(0xFED4, [0x0641]).						% Lo       ..ARABIC LETTER FEH MEDIAL FORM
unicode_nfkc_cf(0xFED5, [0x0642]).						% Lo       ARABIC LETTER QAF ISOLATED FORM..
unicode_nfkc_cf(0xFED6, [0x0642]).						% Lo       ..
unicode_nfkc_cf(0xFED7, [0x0642]).						% Lo       ..
unicode_nfkc_cf(0xFED8, [0x0642]).						% Lo       ..ARABIC LETTER QAF MEDIAL FORM
unicode_nfkc_cf(0xFED9, [0x0643]).						% Lo       ARABIC LETTER KAF ISOLATED FORM..
unicode_nfkc_cf(0xFEDA, [0x0643]).						% Lo       ..
unicode_nfkc_cf(0xFEDB, [0x0643]).						% Lo       ..
unicode_nfkc_cf(0xFEDC, [0x0643]).						% Lo       ..ARABIC LETTER KAF MEDIAL FORM
unicode_nfkc_cf(0xFEDD, [0x0644]).						% Lo       ARABIC LETTER LAM ISOLATED FORM..
unicode_nfkc_cf(0xFEDE, [0x0644]).						% Lo       ..
unicode_nfkc_cf(0xFEDF, [0x0644]).						% Lo       ..
unicode_nfkc_cf(0xFEE0, [0x0644]).						% Lo       ..ARABIC LETTER LAM MEDIAL FORM
unicode_nfkc_cf(0xFEE1, [0x0645]).						% Lo       ARABIC LETTER MEEM ISOLATED FORM
unicode_nfkc_cf(0xFEE2, [0x0645]).						% Lo       ..
unicode_nfkc_cf(0xFEE3, [0x0645]).						% Lo       ..
unicode_nfkc_cf(0xFEE4, [0x0645]).						% Lo       ARABIC LETTER MEEM MEDIAL FORM
unicode_nfkc_cf(0xFEE5, [0x0646]).						% Lo       ARABIC LETTER NOON ISOLATED FORM
unicode_nfkc_cf(0xFEE6, [0x0646]).						% Lo       ..
unicode_nfkc_cf(0xFEE7, [0x0646]).						% Lo       ..
unicode_nfkc_cf(0xFEE8, [0x0646]).						% Lo       ARABIC LETTER NOON MEDIAL FORM
unicode_nfkc_cf(0xFEE9, [0x0647]).						% Lo       ARABIC LETTER HEH ISOLATED FORM
unicode_nfkc_cf(0xFEEA, [0x0647]).						% Lo       ..
unicode_nfkc_cf(0xFEEB, [0x0647]).						% Lo       ..
unicode_nfkc_cf(0xFEEC, [0x0647]).						% Lo       ARABIC LETTER HEH MEDIAL FORM
unicode_nfkc_cf(0xFEED, [0x0648]).						% Lo       ARABIC LETTER WAW ISOLATED FORM
unicode_nfkc_cf(0xFEEE, [0x0648]).						% Lo       ARABIC LETTER WAW FINAL FORM
unicode_nfkc_cf(0xFEEF, [0x0649]).						% Lo       ARABIC LETTER ALEF MAKSURA ISOLATED FORM
unicode_nfkc_cf(0xFEF0, [0x0649]).						% Lo       ARABIC LETTER ALEF MAKSURA FINAL FORM
unicode_nfkc_cf(0xFEF1, [0x064A]).						% Lo       ARABIC LETTER YEH ISOLATED FORM
unicode_nfkc_cf(0xFEF2, [0x064A]).						% Lo       ..
unicode_nfkc_cf(0xFEF3, [0x064A]).						% Lo       ..
unicode_nfkc_cf(0xFEF4, [0x064A]).						% Lo       ARABIC LETTER YEH MEDIAL FORM
unicode_nfkc_cf(0xFEF5, [0x0644, 0x0622]).				% Lo       ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE ISOLATED FORM.
unicode_nfkc_cf(0xFEF6, [0x0644, 0x0622]).				% Lo       ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE FINAL FORM
unicode_nfkc_cf(0xFEF7, [0x0644, 0x0623]).				% Lo       ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE ISOLATED FORM
unicode_nfkc_cf(0xFEF8, [0x0644, 0x0623]).				% Lo       ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE FINAL FORM
unicode_nfkc_cf(0xFEF9, [0x0644, 0x0625]).				% Lo       ARABIC LIGATURE LAM WITH ALEF WITH HAMZA BELOW ISOLATED FORM
unicode_nfkc_cf(0xFEFA, [0x0644, 0x0625]).				% Lo       ARABIC LIGATURE LAM WITH ALEF WITH HAMZA BELOW FINAL FORM
unicode_nfkc_cf(0xFEFB, [0x0644, 0x0627]).				% Lo       ARABIC LIGATURE LAM WITH ALEF ISOLATED FORM
unicode_nfkc_cf(0xFEFC, [0x0644, 0x0627]).				% Lo       ARABIC LIGATURE LAM WITH ALEF FINAL FORM
%unicode_nfkc_cf(0xFEFF           ; NFKC_CF;                   # Cf       ZERO WIDTH NO-BREAK SPACE
unicode_nfkc_cf(0xFF01, [0x0021]).						% Po       FULLWIDTH EXCLAMATION MARK
unicode_nfkc_cf(0xFF02, [0x0022]).						% Po       FULLWIDTH QUOTATION MARK
unicode_nfkc_cf(0xFF03, [0x0023]).						% Po       FULLWIDTH NUMBER SIGN
unicode_nfkc_cf(0xFF04, [0x0024]).						% Sc       FULLWIDTH DOLLAR SIGN
unicode_nfkc_cf(0xFF05, [0x0025]).						% Po       FULLWIDTH PERCENT SIGN
unicode_nfkc_cf(0xFF06, [0x0026]).						% Po       FULLWIDTH AMPERSAND
unicode_nfkc_cf(0xFF07, [0x0027]).						% Po       FULLWIDTH APOSTROPHE
unicode_nfkc_cf(0xFF08, [0x0028]).						% Ps       FULLWIDTH LEFT PARENTHESIS
unicode_nfkc_cf(0xFF09, [0x0029]).						% Pe       FULLWIDTH RIGHT PARENTHESIS
unicode_nfkc_cf(0xFF0A, [0x002A]).						% Po       FULLWIDTH ASTERISK
unicode_nfkc_cf(0xFF0B, [0x002B]).						% Sm       FULLWIDTH PLUS SIGN
unicode_nfkc_cf(0xFF0C, [0x002C]).						% Po       FULLWIDTH COMMA
unicode_nfkc_cf(0xFF0D, [0x002D]).						% Pd       FULLWIDTH HYPHEN-MINUS
unicode_nfkc_cf(0xFF0E, [0x002E]).						% Po       FULLWIDTH FULL STOP
unicode_nfkc_cf(0xFF0F, [0x002F]).						% Po       FULLWIDTH SOLIDUS
unicode_nfkc_cf(0xFF10, [0x0030]).						% Nd       FULLWIDTH DIGIT ZERO
unicode_nfkc_cf(0xFF11, [0x0031]).						% Nd       FULLWIDTH DIGIT ONE
unicode_nfkc_cf(0xFF12, [0x0032]).						% Nd       FULLWIDTH DIGIT TWO
unicode_nfkc_cf(0xFF13, [0x0033]).						% Nd       FULLWIDTH DIGIT THREE
unicode_nfkc_cf(0xFF14, [0x0034]).						% Nd       FULLWIDTH DIGIT FOUR
unicode_nfkc_cf(0xFF15, [0x0035]).						% Nd       FULLWIDTH DIGIT FIVE
unicode_nfkc_cf(0xFF16, [0x0036]).						% Nd       FULLWIDTH DIGIT SIX
unicode_nfkc_cf(0xFF17, [0x0037]).						% Nd       FULLWIDTH DIGIT SEVEN
unicode_nfkc_cf(0xFF18, [0x0038]).						% Nd       FULLWIDTH DIGIT EIGHT
unicode_nfkc_cf(0xFF19, [0x0039]).						% Nd       FULLWIDTH DIGIT NINE
unicode_nfkc_cf(0xFF1A, [0x003A]).						% Po       FULLWIDTH COLON
unicode_nfkc_cf(0xFF1B, [0x003B]).						% Po       FULLWIDTH SEMICOLON
unicode_nfkc_cf(0xFF1C, [0x003C]).						% Sm       FULLWIDTH LESS-THAN SIGN
unicode_nfkc_cf(0xFF1D, [0x003D]).						% Sm       FULLWIDTH EQUALS SIGN
unicode_nfkc_cf(0xFF1E, [0x003E]).						% Sm       FULLWIDTH GREATER-THAN SIGN
unicode_nfkc_cf(0xFF1F, [0x003F]).						% Po       FULLWIDTH QUESTION MARK
unicode_nfkc_cf(0xFF20, [0x0040]).						% Po       FULLWIDTH COMMERCIAL AT
unicode_nfkc_cf(0xFF21, [0x0061]).						% L&       FULLWIDTH LATIN CAPITAL LETTER A
unicode_nfkc_cf(0xFF22, [0x0062]).						% L&       FULLWIDTH LATIN CAPITAL LETTER B
unicode_nfkc_cf(0xFF23, [0x0063]).						% L&       FULLWIDTH LATIN CAPITAL LETTER C
unicode_nfkc_cf(0xFF24, [0x0064]).						% L&       FULLWIDTH LATIN CAPITAL LETTER D
unicode_nfkc_cf(0xFF25, [0x0065]).						% L&       FULLWIDTH LATIN CAPITAL LETTER E
unicode_nfkc_cf(0xFF26, [0x0066]).						% L&       FULLWIDTH LATIN CAPITAL LETTER F
unicode_nfkc_cf(0xFF27, [0x0067]).						% L&       FULLWIDTH LATIN CAPITAL LETTER G
unicode_nfkc_cf(0xFF28, [0x0068]).						% L&       FULLWIDTH LATIN CAPITAL LETTER H
unicode_nfkc_cf(0xFF29, [0x0069]).						% L&       FULLWIDTH LATIN CAPITAL LETTER I
unicode_nfkc_cf(0xFF2A, [0x006A]).						% L&       FULLWIDTH LATIN CAPITAL LETTER J
unicode_nfkc_cf(0xFF2B, [0x006B]).						% L&       FULLWIDTH LATIN CAPITAL LETTER K
unicode_nfkc_cf(0xFF2C, [0x006C]).						% L&       FULLWIDTH LATIN CAPITAL LETTER L
unicode_nfkc_cf(0xFF2D, [0x006D]).						% L&       FULLWIDTH LATIN CAPITAL LETTER M
unicode_nfkc_cf(0xFF2E, [0x006E]).						% L&       FULLWIDTH LATIN CAPITAL LETTER N
unicode_nfkc_cf(0xFF2F, [0x006F]).						% L&       FULLWIDTH LATIN CAPITAL LETTER O
unicode_nfkc_cf(0xFF30, [0x0070]).						% L&       FULLWIDTH LATIN CAPITAL LETTER P
unicode_nfkc_cf(0xFF31, [0x0071]).						% L&       FULLWIDTH LATIN CAPITAL LETTER Q
unicode_nfkc_cf(0xFF32, [0x0072]).						% L&       FULLWIDTH LATIN CAPITAL LETTER R
unicode_nfkc_cf(0xFF33, [0x0073]).						% L&       FULLWIDTH LATIN CAPITAL LETTER S
unicode_nfkc_cf(0xFF34, [0x0074]).						% L&       FULLWIDTH LATIN CAPITAL LETTER T
unicode_nfkc_cf(0xFF35, [0x0075]).						% L&       FULLWIDTH LATIN CAPITAL LETTER U
unicode_nfkc_cf(0xFF36, [0x0076]).						% L&       FULLWIDTH LATIN CAPITAL LETTER V
unicode_nfkc_cf(0xFF37, [0x0077]).						% L&       FULLWIDTH LATIN CAPITAL LETTER W
unicode_nfkc_cf(0xFF38, [0x0078]).						% L&       FULLWIDTH LATIN CAPITAL LETTER X
unicode_nfkc_cf(0xFF39, [0x0079]).						% L&       FULLWIDTH LATIN CAPITAL LETTER Y
unicode_nfkc_cf(0xFF3A, [0x007A]).						% L&       FULLWIDTH LATIN CAPITAL LETTER Z
unicode_nfkc_cf(0xFF3B, [0x005B]).						% Ps       FULLWIDTH LEFT SQUARE BRACKET
unicode_nfkc_cf(0xFF3C, [0x005C]).						% Po       FULLWIDTH REVERSE SOLIDUS
unicode_nfkc_cf(0xFF3D, [0x005D]).						% Pe       FULLWIDTH RIGHT SQUARE BRACKET
unicode_nfkc_cf(0xFF3E, [0x005E]).						% Sk       FULLWIDTH CIRCUMFLEX ACCENT
unicode_nfkc_cf(0xFF3F, [0x005F]).						% Pc       FULLWIDTH LOW LINE
unicode_nfkc_cf(0xFF40, [0x0060]).						% Sk       FULLWIDTH GRAVE ACCENT
unicode_nfkc_cf(0xFF41, [0x0061]).						% L&       FULLWIDTH LATIN SMALL LETTER A
unicode_nfkc_cf(0xFF42, [0x0062]).						% L&       FULLWIDTH LATIN SMALL LETTER B
unicode_nfkc_cf(0xFF43, [0x0063]).						% L&       FULLWIDTH LATIN SMALL LETTER C
unicode_nfkc_cf(0xFF44, [0x0064]).						% L&       FULLWIDTH LATIN SMALL LETTER D
unicode_nfkc_cf(0xFF45, [0x0065]).						% L&       FULLWIDTH LATIN SMALL LETTER E
unicode_nfkc_cf(0xFF46, [0x0066]).						% L&       FULLWIDTH LATIN SMALL LETTER F
unicode_nfkc_cf(0xFF47, [0x0067]).						% L&       FULLWIDTH LATIN SMALL LETTER G
unicode_nfkc_cf(0xFF48, [0x0068]).						% L&       FULLWIDTH LATIN SMALL LETTER H
unicode_nfkc_cf(0xFF49, [0x0069]).						% L&       FULLWIDTH LATIN SMALL LETTER I
unicode_nfkc_cf(0xFF4A, [0x006A]).						% L&       FULLWIDTH LATIN SMALL LETTER J
unicode_nfkc_cf(0xFF4B, [0x006B]).						% L&       FULLWIDTH LATIN SMALL LETTER K
unicode_nfkc_cf(0xFF4C, [0x006C]).						% L&       FULLWIDTH LATIN SMALL LETTER L
unicode_nfkc_cf(0xFF4D, [0x006D]).						% L&       FULLWIDTH LATIN SMALL LETTER M
unicode_nfkc_cf(0xFF4E, [0x006E]).						% L&       FULLWIDTH LATIN SMALL LETTER N
unicode_nfkc_cf(0xFF4F, [0x006F]).						% L&       FULLWIDTH LATIN SMALL LETTER O
unicode_nfkc_cf(0xFF50, [0x0070]).						% L&       FULLWIDTH LATIN SMALL LETTER P
unicode_nfkc_cf(0xFF51, [0x0071]).						% L&       FULLWIDTH LATIN SMALL LETTER Q
unicode_nfkc_cf(0xFF52, [0x0072]).						% L&       FULLWIDTH LATIN SMALL LETTER R
unicode_nfkc_cf(0xFF53, [0x0073]).						% L&       FULLWIDTH LATIN SMALL LETTER S
unicode_nfkc_cf(0xFF54, [0x0074]).						% L&       FULLWIDTH LATIN SMALL LETTER T
unicode_nfkc_cf(0xFF55, [0x0075]).						% L&       FULLWIDTH LATIN SMALL LETTER U
unicode_nfkc_cf(0xFF56, [0x0076]).						% L&       FULLWIDTH LATIN SMALL LETTER V
unicode_nfkc_cf(0xFF57, [0x0077]).						% L&       FULLWIDTH LATIN SMALL LETTER W
unicode_nfkc_cf(0xFF58, [0x0078]).						% L&       FULLWIDTH LATIN SMALL LETTER X
unicode_nfkc_cf(0xFF59, [0x0079]).						% L&       FULLWIDTH LATIN SMALL LETTER Y
unicode_nfkc_cf(0xFF5A, [0x007A]).						% L&       FULLWIDTH LATIN SMALL LETTER Z
unicode_nfkc_cf(0xFF5B, [0x007B]).						% Ps       FULLWIDTH LEFT CURLY BRACKET
unicode_nfkc_cf(0xFF5C, [0x007C]).						% Sm       FULLWIDTH VERTICAL LINE
unicode_nfkc_cf(0xFF5D, [0x007D]).						% Pe       FULLWIDTH RIGHT CURLY BRACKET
unicode_nfkc_cf(0xFF5E, [0x007E]).						% Sm       FULLWIDTH TILDE
unicode_nfkc_cf(0xFF5F, [0x2985]).						% Ps       FULLWIDTH LEFT WHITE PARENTHESIS
unicode_nfkc_cf(0xFF60, [0x2986]).						% Pe       FULLWIDTH RIGHT WHITE PARENTHESIS
unicode_nfkc_cf(0xFF61, [0x3002]).						% Po       HALFWIDTH IDEOGRAPHIC FULL STOP
unicode_nfkc_cf(0xFF62, [0x300C]).						% Ps       HALFWIDTH LEFT CORNER BRACKET
unicode_nfkc_cf(0xFF63, [0x300D]).						% Pe       HALFWIDTH RIGHT CORNER BRACKET
unicode_nfkc_cf(0xFF64, [0x3001]).						% Po       HALFWIDTH IDEOGRAPHIC COMMA
unicode_nfkc_cf(0xFF65, [0x30FB]).						% Po       HALFWIDTH KATAKANA MIDDLE DOT
unicode_nfkc_cf(0xFF66, [0x30F2]).						% Lo       HALFWIDTH KATAKANA LETTER WO
unicode_nfkc_cf(0xFF67, [0x30A1]).						% Lo       HALFWIDTH KATAKANA LETTER SMALL A
unicode_nfkc_cf(0xFF68, [0x30A3]).						% Lo       HALFWIDTH KATAKANA LETTER SMALL I
unicode_nfkc_cf(0xFF69, [0x30A5]).						% Lo       HALFWIDTH KATAKANA LETTER SMALL U
unicode_nfkc_cf(0xFF6A, [0x30A7]).						% Lo       HALFWIDTH KATAKANA LETTER SMALL E
unicode_nfkc_cf(0xFF6B, [0x30A9]).						% Lo       HALFWIDTH KATAKANA LETTER SMALL O
unicode_nfkc_cf(0xFF6C, [0x30E3]).						% Lo       HALFWIDTH KATAKANA LETTER SMALL YA
unicode_nfkc_cf(0xFF6D, [0x30E5]).						% Lo       HALFWIDTH KATAKANA LETTER SMALL YU
unicode_nfkc_cf(0xFF6E, [0x30E7]).						% Lo       HALFWIDTH KATAKANA LETTER SMALL YO
unicode_nfkc_cf(0xFF6F, [0x30C3]).						% Lo       HALFWIDTH KATAKANA LETTER SMALL TU
unicode_nfkc_cf(0xFF70, [0x30FC]).						% Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_nfkc_cf(0xFF71, [0x30A2]).						% Lo       HALFWIDTH KATAKANA LETTER A
unicode_nfkc_cf(0xFF72, [0x30A4]).						% Lo       HALFWIDTH KATAKANA LETTER I
unicode_nfkc_cf(0xFF73, [0x30A6]).						% Lo       HALFWIDTH KATAKANA LETTER U
unicode_nfkc_cf(0xFF74, [0x30A8]).						% Lo       HALFWIDTH KATAKANA LETTER E
unicode_nfkc_cf(0xFF75, [0x30AA]).						% Lo       HALFWIDTH KATAKANA LETTER O
unicode_nfkc_cf(0xFF76, [0x30AB]).						% Lo       HALFWIDTH KATAKANA LETTER KA
unicode_nfkc_cf(0xFF77, [0x30AD]).						% Lo       HALFWIDTH KATAKANA LETTER KI
unicode_nfkc_cf(0xFF78, [0x30AF]).						% Lo       HALFWIDTH KATAKANA LETTER KU
unicode_nfkc_cf(0xFF79, [0x30B1]).						% Lo       HALFWIDTH KATAKANA LETTER KE
unicode_nfkc_cf(0xFF7A, [0x30B3]).						% Lo       HALFWIDTH KATAKANA LETTER KO
unicode_nfkc_cf(0xFF7B, [0x30B5]).						% Lo       HALFWIDTH KATAKANA LETTER SA
unicode_nfkc_cf(0xFF7C, [0x30B7]).						% Lo       HALFWIDTH KATAKANA LETTER SI
unicode_nfkc_cf(0xFF7D, [0x30B9]).						% Lo       HALFWIDTH KATAKANA LETTER SU
unicode_nfkc_cf(0xFF7E, [0x30BB]).						% Lo       HALFWIDTH KATAKANA LETTER SE
unicode_nfkc_cf(0xFF7F, [0x30BD]).						% Lo       HALFWIDTH KATAKANA LETTER SO
unicode_nfkc_cf(0xFF80, [0x30BF]).						% Lo       HALFWIDTH KATAKANA LETTER TA
unicode_nfkc_cf(0xFF81, [0x30C1]).						% Lo       HALFWIDTH KATAKANA LETTER TI
unicode_nfkc_cf(0xFF82, [0x30C4]).						% Lo       HALFWIDTH KATAKANA LETTER TU
unicode_nfkc_cf(0xFF83, [0x30C6]).						% Lo       HALFWIDTH KATAKANA LETTER TE
unicode_nfkc_cf(0xFF84, [0x30C8]).						% Lo       HALFWIDTH KATAKANA LETTER TO
unicode_nfkc_cf(0xFF85, [0x30CA]).						% Lo       HALFWIDTH KATAKANA LETTER NA
unicode_nfkc_cf(0xFF86, [0x30CB]).						% Lo       HALFWIDTH KATAKANA LETTER NI
unicode_nfkc_cf(0xFF87, [0x30CC]).						% Lo       HALFWIDTH KATAKANA LETTER NU
unicode_nfkc_cf(0xFF88, [0x30CD]).						% Lo       HALFWIDTH KATAKANA LETTER NE
unicode_nfkc_cf(0xFF89, [0x30CE]).						% Lo       HALFWIDTH KATAKANA LETTER NO
unicode_nfkc_cf(0xFF8A, [0x30CF]).						% Lo       HALFWIDTH KATAKANA LETTER HA
unicode_nfkc_cf(0xFF8B, [0x30D2]).						% Lo       HALFWIDTH KATAKANA LETTER HI
unicode_nfkc_cf(0xFF8C, [0x30D5]).						% Lo       HALFWIDTH KATAKANA LETTER HU
unicode_nfkc_cf(0xFF8D, [0x30D8]).						% Lo       HALFWIDTH KATAKANA LETTER HE
unicode_nfkc_cf(0xFF8E, [0x30DB]).						% Lo       HALFWIDTH KATAKANA LETTER HO
unicode_nfkc_cf(0xFF8F, [0x30DE]).						% Lo       HALFWIDTH KATAKANA LETTER MA
unicode_nfkc_cf(0xFF90, [0x30DF]).						% Lo       HALFWIDTH KATAKANA LETTER MI
unicode_nfkc_cf(0xFF91, [0x30E0]).						% Lo       HALFWIDTH KATAKANA LETTER MU
unicode_nfkc_cf(0xFF92, [0x30E1]).						% Lo       HALFWIDTH KATAKANA LETTER ME
unicode_nfkc_cf(0xFF93, [0x30E2]).						% Lo       HALFWIDTH KATAKANA LETTER MO
unicode_nfkc_cf(0xFF94, [0x30E4]).						% Lo       HALFWIDTH KATAKANA LETTER YA
unicode_nfkc_cf(0xFF95, [0x30E6]).						% Lo       HALFWIDTH KATAKANA LETTER YU
unicode_nfkc_cf(0xFF96, [0x30E8]).						% Lo       HALFWIDTH KATAKANA LETTER YO
unicode_nfkc_cf(0xFF97, [0x30E9]).						% Lo       HALFWIDTH KATAKANA LETTER RA
unicode_nfkc_cf(0xFF98, [0x30EA]).						% Lo       HALFWIDTH KATAKANA LETTER RI
unicode_nfkc_cf(0xFF99, [0x30EB]).						% Lo       HALFWIDTH KATAKANA LETTER RU
unicode_nfkc_cf(0xFF9A, [0x30EC]).						% Lo       HALFWIDTH KATAKANA LETTER RE
unicode_nfkc_cf(0xFF9B, [0x30ED]).						% Lo       HALFWIDTH KATAKANA LETTER RO
unicode_nfkc_cf(0xFF9C, [0x30EF]).						% Lo       HALFWIDTH KATAKANA LETTER WA
unicode_nfkc_cf(0xFF9D, [0x30F3]).						% Lo       HALFWIDTH KATAKANA LETTER N
unicode_nfkc_cf(0xFF9E, [0x3099]).						% Lm       HALFWIDTH KATAKANA VOICED SOUND MARK
unicode_nfkc_cf(0xFF9F, [0x309A]).						% Lm       HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
%unicode_nfkc_cf(0xFFA0                       # Lo       HALFWIDTH HANGUL FILLER
unicode_nfkc_cf(0xFFA1, [0x1100]).						% Lo       HALFWIDTH HANGUL LETTER KIYEOK
unicode_nfkc_cf(0xFFA2, [0x1101]).						% Lo       HALFWIDTH HANGUL LETTER SSANGKIYEOK
unicode_nfkc_cf(0xFFA3, [0x11AA]).						% Lo       HALFWIDTH HANGUL LETTER KIYEOK-SIOS
unicode_nfkc_cf(0xFFA4, [0x1102]).						% Lo       HALFWIDTH HANGUL LETTER NIEUN
unicode_nfkc_cf(0xFFA5, [0x11AC]).						% Lo       HALFWIDTH HANGUL LETTER NIEUN-CIEUC
unicode_nfkc_cf(0xFFA6, [0x11AD]).						% Lo       HALFWIDTH HANGUL LETTER NIEUN-HIEUH
unicode_nfkc_cf(0xFFA7, [0x1103]).						% Lo       HALFWIDTH HANGUL LETTER TIKEUT
unicode_nfkc_cf(0xFFA8, [0x1104]).						% Lo       HALFWIDTH HANGUL LETTER SSANGTIKEUT
unicode_nfkc_cf(0xFFA9, [0x1105]).						% Lo       HALFWIDTH HANGUL LETTER RIEUL
unicode_nfkc_cf(0xFFAA, [0x11B0]).						% Lo       HALFWIDTH HANGUL LETTER RIEUL-KIYEOK
unicode_nfkc_cf(0xFFAB, [0x11B1]).						% Lo       HALFWIDTH HANGUL LETTER RIEUL-MIEUM
unicode_nfkc_cf(0xFFAC, [0x11B2]).						% Lo       HALFWIDTH HANGUL LETTER RIEUL-PIEUP
unicode_nfkc_cf(0xFFAD, [0x11B3]).						% Lo       HALFWIDTH HANGUL LETTER RIEUL-SIOS
unicode_nfkc_cf(0xFFAE, [0x11B4]).						% Lo       HALFWIDTH HANGUL LETTER RIEUL-THIEUTH
unicode_nfkc_cf(0xFFAF, [0x11B5]).						% Lo       HALFWIDTH HANGUL LETTER RIEUL-PHIEUPH
unicode_nfkc_cf(0xFFB0, [0x111A]).						% Lo       HALFWIDTH HANGUL LETTER RIEUL-HIEUH
unicode_nfkc_cf(0xFFB1, [0x1106]).						% Lo       HALFWIDTH HANGUL LETTER MIEUM
unicode_nfkc_cf(0xFFB2, [0x1107]).						% Lo       HALFWIDTH HANGUL LETTER PIEUP
unicode_nfkc_cf(0xFFB3, [0x1108]).						% Lo       HALFWIDTH HANGUL LETTER SSANGPIEUP
unicode_nfkc_cf(0xFFB4, [0x1121]).						% Lo       HALFWIDTH HANGUL LETTER PIEUP-SIOS
unicode_nfkc_cf(0xFFB5, [0x1109]).						% Lo       HALFWIDTH HANGUL LETTER SIOS
unicode_nfkc_cf(0xFFB6, [0x110A]).						% Lo       HALFWIDTH HANGUL LETTER SSANGSIOS
unicode_nfkc_cf(0xFFB7, [0x110B]).						% Lo       HALFWIDTH HANGUL LETTER IEUNG
unicode_nfkc_cf(0xFFB8, [0x110C]).						% Lo       HALFWIDTH HANGUL LETTER CIEUC
unicode_nfkc_cf(0xFFB9, [0x110D]).						% Lo       HALFWIDTH HANGUL LETTER SSANGCIEUC
unicode_nfkc_cf(0xFFBA, [0x110E]).						% Lo       HALFWIDTH HANGUL LETTER CHIEUCH
unicode_nfkc_cf(0xFFBB, [0x110F]).						% Lo       HALFWIDTH HANGUL LETTER KHIEUKH
unicode_nfkc_cf(0xFFBC, [0x1110]).						% Lo       HALFWIDTH HANGUL LETTER THIEUTH
unicode_nfkc_cf(0xFFBD, [0x1111]).						% Lo       HALFWIDTH HANGUL LETTER PHIEUPH
unicode_nfkc_cf(0xFFBE, [0x1112]).						% Lo       HALFWIDTH HANGUL LETTER HIEUH
unicode_nfkc_cf(0xFFC2, [0x1161]).						% Lo       HALFWIDTH HANGUL LETTER A
unicode_nfkc_cf(0xFFC3, [0x1162]).						% Lo       HALFWIDTH HANGUL LETTER AE
unicode_nfkc_cf(0xFFC4, [0x1163]).						% Lo       HALFWIDTH HANGUL LETTER YA
unicode_nfkc_cf(0xFFC5, [0x1164]).						% Lo       HALFWIDTH HANGUL LETTER YAE
unicode_nfkc_cf(0xFFC6, [0x1165]).						% Lo       HALFWIDTH HANGUL LETTER EO
unicode_nfkc_cf(0xFFC7, [0x1166]).						% Lo       HALFWIDTH HANGUL LETTER E
unicode_nfkc_cf(0xFFCA, [0x1167]).						% Lo       HALFWIDTH HANGUL LETTER YEO
unicode_nfkc_cf(0xFFCB, [0x1168]).						% Lo       HALFWIDTH HANGUL LETTER YE
unicode_nfkc_cf(0xFFCC, [0x1169]).						% Lo       HALFWIDTH HANGUL LETTER O
unicode_nfkc_cf(0xFFCD, [0x116A]).						% Lo       HALFWIDTH HANGUL LETTER WA
unicode_nfkc_cf(0xFFCE, [0x116B]).						% Lo       HALFWIDTH HANGUL LETTER WAE
unicode_nfkc_cf(0xFFCF, [0x116C]).						% Lo       HALFWIDTH HANGUL LETTER OE
unicode_nfkc_cf(0xFFD2, [0x116D]).						% Lo       HALFWIDTH HANGUL LETTER YO
unicode_nfkc_cf(0xFFD3, [0x116E]).						% Lo       HALFWIDTH HANGUL LETTER U
unicode_nfkc_cf(0xFFD4, [0x116F]).						% Lo       HALFWIDTH HANGUL LETTER WEO
unicode_nfkc_cf(0xFFD5, [0x1170]).						% Lo       HALFWIDTH HANGUL LETTER WE
unicode_nfkc_cf(0xFFD6, [0x1171]).						% Lo       HALFWIDTH HANGUL LETTER WI
unicode_nfkc_cf(0xFFD7, [0x1172]).						% Lo       HALFWIDTH HANGUL LETTER YU
unicode_nfkc_cf(0xFFDA, [0x1173]).						% Lo       HALFWIDTH HANGUL LETTER EU
unicode_nfkc_cf(0xFFDB, [0x1174]).						% Lo       HALFWIDTH HANGUL LETTER YI
unicode_nfkc_cf(0xFFDC, [0x1175]).						% Lo       HALFWIDTH HANGUL LETTER I
unicode_nfkc_cf(0xFFE0, [0x00A2]).						% Sc       FULLWIDTH CENT SIGN
unicode_nfkc_cf(0xFFE1, [0x00A3]).						% Sc       FULLWIDTH POUND SIGN
unicode_nfkc_cf(0xFFE2, [0x00AC]).						% Sm       FULLWIDTH NOT SIGN
unicode_nfkc_cf(0xFFE3, [0x0020, 0x0304]).				% Sk       FULLWIDTH MACRON
unicode_nfkc_cf(0xFFE4, [0x00A6]).						% So       FULLWIDTH BROKEN BAR
unicode_nfkc_cf(0xFFE5, [0x00A5]).						% Sc       FULLWIDTH YEN SIGN
unicode_nfkc_cf(0xFFE6, [0x20A9]).						% Sc       FULLWIDTH WON SIGN
unicode_nfkc_cf(0xFFE8, [0x2502]).						% So       HALFWIDTH FORMS LIGHT VERTICAL
unicode_nfkc_cf(0xFFE9, [0x2190]).						% Sm       HALFWIDTH LEFTWARDS ARROW
unicode_nfkc_cf(0xFFEA, [0x2191]).						% Sm       HALFWIDTH UPWARDS ARROW
unicode_nfkc_cf(0xFFEB, [0x2192]).						% Sm       HALFWIDTH RIGHTWARDS ARROW
unicode_nfkc_cf(0xFFEC, [0x2193]).						% Sm       HALFWIDTH DOWNWARDS ARROW
unicode_nfkc_cf(0xFFED, [0x25A0]).						% So       HALFWIDTH BLACK SQUARE
unicode_nfkc_cf(0xFFEE, [0x25CB]).						% So       HALFWIDTH WHITE CIRCLE
%unicode_nfkc_cf(0xFFF0, 0xFFF8    ; NFKC_CF;                   # Cn   [9] <reserved-FFF0>..<reserved-FFF8>
unicode_nfkc_cf(0x10400, [0x10428]).					% L&       DESERET CAPITAL LETTER LONG I
unicode_nfkc_cf(0x10401, [0x10429]).					% L&       DESERET CAPITAL LETTER LONG E
unicode_nfkc_cf(0x10402, [0x1042A]).					% L&       DESERET CAPITAL LETTER LONG A
unicode_nfkc_cf(0x10403, [0x1042B]).					% L&       DESERET CAPITAL LETTER LONG AH
unicode_nfkc_cf(0x10404, [0x1042C]).					% L&       DESERET CAPITAL LETTER LONG O
unicode_nfkc_cf(0x10405, [0x1042D]).					% L&       DESERET CAPITAL LETTER LONG OO
unicode_nfkc_cf(0x10406, [0x1042E]).					% L&       DESERET CAPITAL LETTER SHORT I
unicode_nfkc_cf(0x10407, [0x1042F]).					% L&       DESERET CAPITAL LETTER SHORT E
unicode_nfkc_cf(0x10408, [0x10430]).					% L&       DESERET CAPITAL LETTER SHORT A
unicode_nfkc_cf(0x10409, [0x10431]).					% L&       DESERET CAPITAL LETTER SHORT AH
unicode_nfkc_cf(0x1040A, [0x10432]).					% L&       DESERET CAPITAL LETTER SHORT O
unicode_nfkc_cf(0x1040B, [0x10433]).					% L&       DESERET CAPITAL LETTER SHORT OO
unicode_nfkc_cf(0x1040C, [0x10434]).					% L&       DESERET CAPITAL LETTER AY
unicode_nfkc_cf(0x1040D, [0x10435]).					% L&       DESERET CAPITAL LETTER OW
unicode_nfkc_cf(0x1040E, [0x10436]).					% L&       DESERET CAPITAL LETTER WU
unicode_nfkc_cf(0x1040F, [0x10437]).					% L&       DESERET CAPITAL LETTER YEE
unicode_nfkc_cf(0x10410, [0x10438]).					% L&       DESERET CAPITAL LETTER H
unicode_nfkc_cf(0x10411, [0x10439]).					% L&       DESERET CAPITAL LETTER PEE
unicode_nfkc_cf(0x10412, [0x1043A]).					% L&       DESERET CAPITAL LETTER BEE
unicode_nfkc_cf(0x10413, [0x1043B]).					% L&       DESERET CAPITAL LETTER TEE
unicode_nfkc_cf(0x10414, [0x1043C]).					% L&       DESERET CAPITAL LETTER DEE
unicode_nfkc_cf(0x10415, [0x1043D]).					% L&       DESERET CAPITAL LETTER CHEE
unicode_nfkc_cf(0x10416, [0x1043E]).					% L&       DESERET CAPITAL LETTER JEE
unicode_nfkc_cf(0x10417, [0x1043F]).					% L&       DESERET CAPITAL LETTER KAY
unicode_nfkc_cf(0x10418, [0x10440]).					% L&       DESERET CAPITAL LETTER GAY
unicode_nfkc_cf(0x10419, [0x10441]).					% L&       DESERET CAPITAL LETTER EF
unicode_nfkc_cf(0x1041A, [0x10442]).					% L&       DESERET CAPITAL LETTER VEE
unicode_nfkc_cf(0x1041B, [0x10443]).					% L&       DESERET CAPITAL LETTER ETH
unicode_nfkc_cf(0x1041C, [0x10444]).					% L&       DESERET CAPITAL LETTER THEE
unicode_nfkc_cf(0x1041D, [0x10445]).					% L&       DESERET CAPITAL LETTER ES
unicode_nfkc_cf(0x1041E, [0x10446]).					% L&       DESERET CAPITAL LETTER ZEE
unicode_nfkc_cf(0x1041F, [0x10447]).					% L&       DESERET CAPITAL LETTER ESH
unicode_nfkc_cf(0x10420, [0x10448]).					% L&       DESERET CAPITAL LETTER ZHEE
unicode_nfkc_cf(0x10421, [0x10449]).					% L&       DESERET CAPITAL LETTER ER
unicode_nfkc_cf(0x10422, [0x1044A]).					% L&       DESERET CAPITAL LETTER EL
unicode_nfkc_cf(0x10423, [0x1044B]).					% L&       DESERET CAPITAL LETTER EM
unicode_nfkc_cf(0x10424, [0x1044C]).					% L&       DESERET CAPITAL LETTER EN
unicode_nfkc_cf(0x10425, [0x1044D]).					% L&       DESERET CAPITAL LETTER ENG
unicode_nfkc_cf(0x10426, [0x1044E]).					% L&       DESERET CAPITAL LETTER OI
unicode_nfkc_cf(0x10427, [0x1044F]).					% L&       DESERET CAPITAL LETTER EW
unicode_nfkc_cf(0x1D15E, [0x1D157, 0x1D165]).			% So       MUSICAL SYMBOL HALF NOTE
unicode_nfkc_cf(0x1D15F, [0x1D158, 0x1D165]).			% So       MUSICAL SYMBOL QUARTER NOTE
unicode_nfkc_cf(0x1D160, [0x1D158, 0x1D165, 0x1D16E]).	% So     MUSICAL SYMBOL EIGHTH NOTE
unicode_nfkc_cf(0x1D161, [0x1D158, 0x1D165, 0x1D16F]).	% So     MUSICAL SYMBOL SIXTEENTH NOTE
unicode_nfkc_cf(0x1D162, [0x1D158, 0x1D165, 0x1D170]).	% So     MUSICAL SYMBOL THIRTY-SECOND NOTE
unicode_nfkc_cf(0x1D163, [0x1D158, 0x1D165, 0x1D171]).	% So     MUSICAL SYMBOL SIXTY-FOURTH NOTE
unicode_nfkc_cf(0x1D164, [0x1D158, 0x1D165, 0x1D172]).	% So     MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
%unicode_nfkc_cf(0x1D173, 0x1D17A  ; NFKC_CF;                   # Cf   [8] MUSICAL SYMBOL BEGIN BEAM..MUSICAL SYMBOL END PHRASE
unicode_nfkc_cf(0x1D1BB, [0x1D1B9, 0x1D165]).			% So       MUSICAL SYMBOL MINIMA
unicode_nfkc_cf(0x1D1BC, [0x1D1BA, 0x1D165]).			% So       MUSICAL SYMBOL MINIMA BLACK
unicode_nfkc_cf(0x1D1BD, [0x1D1B9, 0x1D165, 0x1D16E]).	% So     MUSICAL SYMBOL SEMIMINIMA WHITE
unicode_nfkc_cf(0x1D1BE, [0x1D1BA, 0x1D165, 0x1D16E]).	% So     MUSICAL SYMBOL SEMIMINIMA BLACK
unicode_nfkc_cf(0x1D1BF, [0x1D1B9, 0x1D165, 0x1D16F]).	% So     MUSICAL SYMBOL FUSA WHITE
unicode_nfkc_cf(0x1D1C0, [0x1D1BA, 0x1D165, 0x1D16F]).	% So     MUSICAL SYMBOL FUSA BLACK
unicode_nfkc_cf(0x1D400, [0x0061]).						% L&       MATHEMATICAL BOLD CAPITAL A
unicode_nfkc_cf(0x1D401, [0x0062]).						% L&       MATHEMATICAL BOLD CAPITAL B
unicode_nfkc_cf(0x1D402, [0x0063]).						% L&       MATHEMATICAL BOLD CAPITAL C
unicode_nfkc_cf(0x1D403, [0x0064]).						% L&       MATHEMATICAL BOLD CAPITAL D
unicode_nfkc_cf(0x1D404, [0x0065]).						% L&       MATHEMATICAL BOLD CAPITAL E
unicode_nfkc_cf(0x1D405, [0x0066]).						% L&       MATHEMATICAL BOLD CAPITAL F
unicode_nfkc_cf(0x1D406, [0x0067]).						% L&       MATHEMATICAL BOLD CAPITAL G
unicode_nfkc_cf(0x1D407, [0x0068]).						% L&       MATHEMATICAL BOLD CAPITAL H
unicode_nfkc_cf(0x1D408, [0x0069]).						% L&       MATHEMATICAL BOLD CAPITAL I
unicode_nfkc_cf(0x1D409, [0x006A]).						% L&       MATHEMATICAL BOLD CAPITAL J
unicode_nfkc_cf(0x1D40A, [0x006B]).						% L&       MATHEMATICAL BOLD CAPITAL K
unicode_nfkc_cf(0x1D40B, [0x006C]).						% L&       MATHEMATICAL BOLD CAPITAL L
unicode_nfkc_cf(0x1D40C, [0x006D]).						% L&       MATHEMATICAL BOLD CAPITAL M
unicode_nfkc_cf(0x1D40D, [0x006E]).						% L&       MATHEMATICAL BOLD CAPITAL N
unicode_nfkc_cf(0x1D40E, [0x006F]).						% L&       MATHEMATICAL BOLD CAPITAL O
unicode_nfkc_cf(0x1D40F, [0x0070]).						% L&       MATHEMATICAL BOLD CAPITAL P
unicode_nfkc_cf(0x1D410, [0x0071]).						% L&       MATHEMATICAL BOLD CAPITAL Q
unicode_nfkc_cf(0x1D411, [0x0072]).						% L&       MATHEMATICAL BOLD CAPITAL R
unicode_nfkc_cf(0x1D412, [0x0073]).						% L&       MATHEMATICAL BOLD CAPITAL S
unicode_nfkc_cf(0x1D413, [0x0074]).						% L&       MATHEMATICAL BOLD CAPITAL T
unicode_nfkc_cf(0x1D414, [0x0075]).						% L&       MATHEMATICAL BOLD CAPITAL U
unicode_nfkc_cf(0x1D415, [0x0076]).						% L&       MATHEMATICAL BOLD CAPITAL V
unicode_nfkc_cf(0x1D416, [0x0077]).						% L&       MATHEMATICAL BOLD CAPITAL W
unicode_nfkc_cf(0x1D417, [0x0078]).						% L&       MATHEMATICAL BOLD CAPITAL X
unicode_nfkc_cf(0x1D418, [0x0079]).						% L&       MATHEMATICAL BOLD CAPITAL Y
unicode_nfkc_cf(0x1D419, [0x007A]).						% L&       MATHEMATICAL BOLD CAPITAL Z
unicode_nfkc_cf(0x1D41A, [0x0061]).						% L&       MATHEMATICAL BOLD SMALL A
unicode_nfkc_cf(0x1D41B, [0x0062]).						% L&       MATHEMATICAL BOLD SMALL B
unicode_nfkc_cf(0x1D41C, [0x0063]).						% L&       MATHEMATICAL BOLD SMALL C
unicode_nfkc_cf(0x1D41D, [0x0064]).						% L&       MATHEMATICAL BOLD SMALL D
unicode_nfkc_cf(0x1D41E, [0x0065]).						% L&       MATHEMATICAL BOLD SMALL E
unicode_nfkc_cf(0x1D41F, [0x0066]).						% L&       MATHEMATICAL BOLD SMALL F
unicode_nfkc_cf(0x1D420, [0x0067]).						% L&       MATHEMATICAL BOLD SMALL G
unicode_nfkc_cf(0x1D421, [0x0068]).						% L&       MATHEMATICAL BOLD SMALL H
unicode_nfkc_cf(0x1D422, [0x0069]).						% L&       MATHEMATICAL BOLD SMALL I
unicode_nfkc_cf(0x1D423, [0x006A]).						% L&       MATHEMATICAL BOLD SMALL J
unicode_nfkc_cf(0x1D424, [0x006B]).						% L&       MATHEMATICAL BOLD SMALL K
unicode_nfkc_cf(0x1D425, [0x006C]).						% L&       MATHEMATICAL BOLD SMALL L
unicode_nfkc_cf(0x1D426, [0x006D]).						% L&       MATHEMATICAL BOLD SMALL M
unicode_nfkc_cf(0x1D427, [0x006E]).						% L&       MATHEMATICAL BOLD SMALL N
unicode_nfkc_cf(0x1D428, [0x006F]).						% L&       MATHEMATICAL BOLD SMALL O
unicode_nfkc_cf(0x1D429, [0x0070]).						% L&       MATHEMATICAL BOLD SMALL P
unicode_nfkc_cf(0x1D42A, [0x0071]).						% L&       MATHEMATICAL BOLD SMALL Q
unicode_nfkc_cf(0x1D42B, [0x0072]).						% L&       MATHEMATICAL BOLD SMALL R
unicode_nfkc_cf(0x1D42C, [0x0073]).						% L&       MATHEMATICAL BOLD SMALL S
unicode_nfkc_cf(0x1D42D, [0x0074]).						% L&       MATHEMATICAL BOLD SMALL T
unicode_nfkc_cf(0x1D42E, [0x0075]).						% L&       MATHEMATICAL BOLD SMALL U
unicode_nfkc_cf(0x1D42F, [0x0076]).						% L&       MATHEMATICAL BOLD SMALL V
unicode_nfkc_cf(0x1D430, [0x0077]).						% L&       MATHEMATICAL BOLD SMALL W
unicode_nfkc_cf(0x1D431, [0x0078]).						% L&       MATHEMATICAL BOLD SMALL X
unicode_nfkc_cf(0x1D432, [0x0079]).						% L&       MATHEMATICAL BOLD SMALL Y
unicode_nfkc_cf(0x1D433, [0x007A]).						% L&       MATHEMATICAL BOLD SMALL Z
unicode_nfkc_cf(0x1D434, [0x0061]).						% L&       MATHEMATICAL ITALIC CAPITAL A
unicode_nfkc_cf(0x1D435, [0x0062]).						% L&       MATHEMATICAL ITALIC CAPITAL B
unicode_nfkc_cf(0x1D436, [0x0063]).						% L&       MATHEMATICAL ITALIC CAPITAL C
unicode_nfkc_cf(0x1D437, [0x0064]).						% L&       MATHEMATICAL ITALIC CAPITAL D
unicode_nfkc_cf(0x1D438, [0x0065]).						% L&       MATHEMATICAL ITALIC CAPITAL E
unicode_nfkc_cf(0x1D439, [0x0066]).						% L&       MATHEMATICAL ITALIC CAPITAL F
unicode_nfkc_cf(0x1D43A, [0x0067]).						% L&       MATHEMATICAL ITALIC CAPITAL G
unicode_nfkc_cf(0x1D43B, [0x0068]).						% L&       MATHEMATICAL ITALIC CAPITAL H
unicode_nfkc_cf(0x1D43C, [0x0069]).						% L&       MATHEMATICAL ITALIC CAPITAL I
unicode_nfkc_cf(0x1D43D, [0x006A]).						% L&       MATHEMATICAL ITALIC CAPITAL J
unicode_nfkc_cf(0x1D43E, [0x006B]).						% L&       MATHEMATICAL ITALIC CAPITAL K
unicode_nfkc_cf(0x1D43F, [0x006C]).						% L&       MATHEMATICAL ITALIC CAPITAL L
unicode_nfkc_cf(0x1D440, [0x006D]).						% L&       MATHEMATICAL ITALIC CAPITAL M
unicode_nfkc_cf(0x1D441, [0x006E]).						% L&       MATHEMATICAL ITALIC CAPITAL N
unicode_nfkc_cf(0x1D442, [0x006F]).						% L&       MATHEMATICAL ITALIC CAPITAL O
unicode_nfkc_cf(0x1D443, [0x0070]).						% L&       MATHEMATICAL ITALIC CAPITAL P
unicode_nfkc_cf(0x1D444, [0x0071]).						% L&       MATHEMATICAL ITALIC CAPITAL Q
unicode_nfkc_cf(0x1D445, [0x0072]).						% L&       MATHEMATICAL ITALIC CAPITAL R
unicode_nfkc_cf(0x1D446, [0x0073]).						% L&       MATHEMATICAL ITALIC CAPITAL S
unicode_nfkc_cf(0x1D447, [0x0074]).						% L&       MATHEMATICAL ITALIC CAPITAL T
unicode_nfkc_cf(0x1D448, [0x0075]).						% L&       MATHEMATICAL ITALIC CAPITAL U
unicode_nfkc_cf(0x1D449, [0x0076]).						% L&       MATHEMATICAL ITALIC CAPITAL V
unicode_nfkc_cf(0x1D44A, [0x0077]).						% L&       MATHEMATICAL ITALIC CAPITAL W
unicode_nfkc_cf(0x1D44B, [0x0078]).						% L&       MATHEMATICAL ITALIC CAPITAL X
unicode_nfkc_cf(0x1D44C, [0x0079]).						% L&       MATHEMATICAL ITALIC CAPITAL Y
unicode_nfkc_cf(0x1D44D, [0x007A]).						% L&       MATHEMATICAL ITALIC CAPITAL Z
unicode_nfkc_cf(0x1D44E, [0x0061]).						% L&       MATHEMATICAL ITALIC SMALL A
unicode_nfkc_cf(0x1D44F, [0x0062]).						% L&       MATHEMATICAL ITALIC SMALL B
unicode_nfkc_cf(0x1D450, [0x0063]).						% L&       MATHEMATICAL ITALIC SMALL C
unicode_nfkc_cf(0x1D451, [0x0064]).						% L&       MATHEMATICAL ITALIC SMALL D
unicode_nfkc_cf(0x1D452, [0x0065]).						% L&       MATHEMATICAL ITALIC SMALL E
unicode_nfkc_cf(0x1D453, [0x0066]).						% L&       MATHEMATICAL ITALIC SMALL F
unicode_nfkc_cf(0x1D454, [0x0067]).						% L&       MATHEMATICAL ITALIC SMALL G
unicode_nfkc_cf(0x1D456, [0x0069]).						% L&       MATHEMATICAL ITALIC SMALL I
unicode_nfkc_cf(0x1D457, [0x006A]).						% L&       MATHEMATICAL ITALIC SMALL J
unicode_nfkc_cf(0x1D458, [0x006B]).						% L&       MATHEMATICAL ITALIC SMALL K
unicode_nfkc_cf(0x1D459, [0x006C]).						% L&       MATHEMATICAL ITALIC SMALL L
unicode_nfkc_cf(0x1D45A, [0x006D]).						% L&       MATHEMATICAL ITALIC SMALL M
unicode_nfkc_cf(0x1D45B, [0x006E]).						% L&       MATHEMATICAL ITALIC SMALL N
unicode_nfkc_cf(0x1D45C, [0x006F]).						% L&       MATHEMATICAL ITALIC SMALL O
unicode_nfkc_cf(0x1D45D, [0x0070]).						% L&       MATHEMATICAL ITALIC SMALL P
unicode_nfkc_cf(0x1D45E, [0x0071]).						% L&       MATHEMATICAL ITALIC SMALL Q
unicode_nfkc_cf(0x1D45F, [0x0072]).						% L&       MATHEMATICAL ITALIC SMALL R
unicode_nfkc_cf(0x1D460, [0x0073]).						% L&       MATHEMATICAL ITALIC SMALL S
unicode_nfkc_cf(0x1D461, [0x0074]).						% L&       MATHEMATICAL ITALIC SMALL T
unicode_nfkc_cf(0x1D462, [0x0075]).						% L&       MATHEMATICAL ITALIC SMALL U
unicode_nfkc_cf(0x1D463, [0x0076]).						% L&       MATHEMATICAL ITALIC SMALL V
unicode_nfkc_cf(0x1D464, [0x0077]).						% L&       MATHEMATICAL ITALIC SMALL W
unicode_nfkc_cf(0x1D465, [0x0078]).						% L&       MATHEMATICAL ITALIC SMALL X
unicode_nfkc_cf(0x1D466, [0x0079]).						% L&       MATHEMATICAL ITALIC SMALL Y
unicode_nfkc_cf(0x1D467, [0x007A]).						% L&       MATHEMATICAL ITALIC SMALL Z
unicode_nfkc_cf(0x1D468, [0x0061]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL A
unicode_nfkc_cf(0x1D469, [0x0062]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL B
unicode_nfkc_cf(0x1D46A, [0x0063]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL C
unicode_nfkc_cf(0x1D46B, [0x0064]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL D
unicode_nfkc_cf(0x1D46C, [0x0065]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL E
unicode_nfkc_cf(0x1D46D, [0x0066]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL F
unicode_nfkc_cf(0x1D46E, [0x0067]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL G
unicode_nfkc_cf(0x1D46F, [0x0068]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL H
unicode_nfkc_cf(0x1D470, [0x0069]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL I
unicode_nfkc_cf(0x1D471, [0x006A]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL J
unicode_nfkc_cf(0x1D472, [0x006B]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL K
unicode_nfkc_cf(0x1D473, [0x006C]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL L
unicode_nfkc_cf(0x1D474, [0x006D]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL M
unicode_nfkc_cf(0x1D475, [0x006E]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL N
unicode_nfkc_cf(0x1D476, [0x006F]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL O
unicode_nfkc_cf(0x1D477, [0x0070]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL P
unicode_nfkc_cf(0x1D478, [0x0071]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL Q
unicode_nfkc_cf(0x1D479, [0x0072]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL R
unicode_nfkc_cf(0x1D47A, [0x0073]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL S
unicode_nfkc_cf(0x1D47B, [0x0074]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL T
unicode_nfkc_cf(0x1D47C, [0x0075]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL U
unicode_nfkc_cf(0x1D47D, [0x0076]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL V
unicode_nfkc_cf(0x1D47E, [0x0077]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL W
unicode_nfkc_cf(0x1D47F, [0x0078]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL X
unicode_nfkc_cf(0x1D480, [0x0079]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL Y
unicode_nfkc_cf(0x1D481, [0x007A]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL Z
unicode_nfkc_cf(0x1D482, [0x0061]).						% L&       MATHEMATICAL BOLD ITALIC SMALL A
unicode_nfkc_cf(0x1D483, [0x0062]).						% L&       MATHEMATICAL BOLD ITALIC SMALL B
unicode_nfkc_cf(0x1D484, [0x0063]).						% L&       MATHEMATICAL BOLD ITALIC SMALL C
unicode_nfkc_cf(0x1D485, [0x0064]).						% L&       MATHEMATICAL BOLD ITALIC SMALL D
unicode_nfkc_cf(0x1D486, [0x0065]).						% L&       MATHEMATICAL BOLD ITALIC SMALL E
unicode_nfkc_cf(0x1D487, [0x0066]).						% L&       MATHEMATICAL BOLD ITALIC SMALL F
unicode_nfkc_cf(0x1D488, [0x0067]).						% L&       MATHEMATICAL BOLD ITALIC SMALL G
unicode_nfkc_cf(0x1D489, [0x0068]).						% L&       MATHEMATICAL BOLD ITALIC SMALL H
unicode_nfkc_cf(0x1D48A, [0x0069]).						% L&       MATHEMATICAL BOLD ITALIC SMALL I
unicode_nfkc_cf(0x1D48B, [0x006A]).						% L&       MATHEMATICAL BOLD ITALIC SMALL J
unicode_nfkc_cf(0x1D48C, [0x006B]).						% L&       MATHEMATICAL BOLD ITALIC SMALL K
unicode_nfkc_cf(0x1D48D, [0x006C]).						% L&       MATHEMATICAL BOLD ITALIC SMALL L
unicode_nfkc_cf(0x1D48E, [0x006D]).						% L&       MATHEMATICAL BOLD ITALIC SMALL M
unicode_nfkc_cf(0x1D48F, [0x006E]).						% L&       MATHEMATICAL BOLD ITALIC SMALL N
unicode_nfkc_cf(0x1D490, [0x006F]).						% L&       MATHEMATICAL BOLD ITALIC SMALL O
unicode_nfkc_cf(0x1D491, [0x0070]).						% L&       MATHEMATICAL BOLD ITALIC SMALL P
unicode_nfkc_cf(0x1D492, [0x0071]).						% L&       MATHEMATICAL BOLD ITALIC SMALL Q
unicode_nfkc_cf(0x1D493, [0x0072]).						% L&       MATHEMATICAL BOLD ITALIC SMALL R
unicode_nfkc_cf(0x1D494, [0x0073]).						% L&       MATHEMATICAL BOLD ITALIC SMALL S
unicode_nfkc_cf(0x1D495, [0x0074]).						% L&       MATHEMATICAL BOLD ITALIC SMALL T
unicode_nfkc_cf(0x1D496, [0x0075]).						% L&       MATHEMATICAL BOLD ITALIC SMALL U
unicode_nfkc_cf(0x1D497, [0x0076]).						% L&       MATHEMATICAL BOLD ITALIC SMALL V
unicode_nfkc_cf(0x1D498, [0x0077]).						% L&       MATHEMATICAL BOLD ITALIC SMALL W
unicode_nfkc_cf(0x1D499, [0x0078]).						% L&       MATHEMATICAL BOLD ITALIC SMALL X
unicode_nfkc_cf(0x1D49A, [0x0079]).						% L&       MATHEMATICAL BOLD ITALIC SMALL Y
unicode_nfkc_cf(0x1D49B, [0x007A]).						% L&       MATHEMATICAL BOLD ITALIC SMALL Z
unicode_nfkc_cf(0x1D49C, [0x0061]).						% L&       MATHEMATICAL SCRIPT CAPITAL A
unicode_nfkc_cf(0x1D49E, [0x0063]).						% L&       MATHEMATICAL SCRIPT CAPITAL C
unicode_nfkc_cf(0x1D49F, [0x0064]).						% L&       MATHEMATICAL SCRIPT CAPITAL D
unicode_nfkc_cf(0x1D4A2, [0x0067]).						% L&       MATHEMATICAL SCRIPT CAPITAL G
unicode_nfkc_cf(0x1D4A5, [0x006A]).						% L&       MATHEMATICAL SCRIPT CAPITAL J
unicode_nfkc_cf(0x1D4A6, [0x006B]).						% L&       MATHEMATICAL SCRIPT CAPITAL K
unicode_nfkc_cf(0x1D4A9, [0x006E]).						% L&       MATHEMATICAL SCRIPT CAPITAL N
unicode_nfkc_cf(0x1D4AA, [0x006F]).						% L&       MATHEMATICAL SCRIPT CAPITAL O
unicode_nfkc_cf(0x1D4AB, [0x0070]).						% L&       MATHEMATICAL SCRIPT CAPITAL P
unicode_nfkc_cf(0x1D4AC, [0x0071]).						% L&       MATHEMATICAL SCRIPT CAPITAL Q
unicode_nfkc_cf(0x1D4AE, [0x0073]).						% L&       MATHEMATICAL SCRIPT CAPITAL S
unicode_nfkc_cf(0x1D4AF, [0x0074]).						% L&       MATHEMATICAL SCRIPT CAPITAL T
unicode_nfkc_cf(0x1D4B0, [0x0075]).						% L&       MATHEMATICAL SCRIPT CAPITAL U
unicode_nfkc_cf(0x1D4B1, [0x0076]).						% L&       MATHEMATICAL SCRIPT CAPITAL V
unicode_nfkc_cf(0x1D4B2, [0x0077]).						% L&       MATHEMATICAL SCRIPT CAPITAL W
unicode_nfkc_cf(0x1D4B3, [0x0078]).						% L&       MATHEMATICAL SCRIPT CAPITAL X
unicode_nfkc_cf(0x1D4B4, [0x0079]).						% L&       MATHEMATICAL SCRIPT CAPITAL Y
unicode_nfkc_cf(0x1D4B5, [0x007A]).						% L&       MATHEMATICAL SCRIPT CAPITAL Z
unicode_nfkc_cf(0x1D4B6, [0x0061]).						% L&       MATHEMATICAL SCRIPT SMALL A
unicode_nfkc_cf(0x1D4B7, [0x0062]).						% L&       MATHEMATICAL SCRIPT SMALL B
unicode_nfkc_cf(0x1D4B8, [0x0063]).						% L&       MATHEMATICAL SCRIPT SMALL C
unicode_nfkc_cf(0x1D4B9, [0x0064]).						% L&       MATHEMATICAL SCRIPT SMALL D
unicode_nfkc_cf(0x1D4BB, [0x0066]).						% L&       MATHEMATICAL SCRIPT SMALL F
unicode_nfkc_cf(0x1D4BD, [0x0068]).						% L&       MATHEMATICAL SCRIPT SMALL H
unicode_nfkc_cf(0x1D4BE, [0x0069]).						% L&       MATHEMATICAL SCRIPT SMALL I
unicode_nfkc_cf(0x1D4BF, [0x006A]).						% L&       MATHEMATICAL SCRIPT SMALL J
unicode_nfkc_cf(0x1D4C0, [0x006B]).						% L&       MATHEMATICAL SCRIPT SMALL K
unicode_nfkc_cf(0x1D4C1, [0x006C]).						% L&       MATHEMATICAL SCRIPT SMALL L
unicode_nfkc_cf(0x1D4C2, [0x006D]).						% L&       MATHEMATICAL SCRIPT SMALL M
unicode_nfkc_cf(0x1D4C3, [0x006E]).						% L&       MATHEMATICAL SCRIPT SMALL N
unicode_nfkc_cf(0x1D4C5, [0x0070]).						% L&       MATHEMATICAL SCRIPT SMALL P
unicode_nfkc_cf(0x1D4C6, [0x0071]).						% L&       MATHEMATICAL SCRIPT SMALL Q
unicode_nfkc_cf(0x1D4C7, [0x0072]).						% L&       MATHEMATICAL SCRIPT SMALL R
unicode_nfkc_cf(0x1D4C8, [0x0073]).						% L&       MATHEMATICAL SCRIPT SMALL S
unicode_nfkc_cf(0x1D4C9, [0x0074]).						% L&       MATHEMATICAL SCRIPT SMALL T
unicode_nfkc_cf(0x1D4CA, [0x0075]).						% L&       MATHEMATICAL SCRIPT SMALL U
unicode_nfkc_cf(0x1D4CB, [0x0076]).						% L&       MATHEMATICAL SCRIPT SMALL V
unicode_nfkc_cf(0x1D4CC, [0x0077]).						% L&       MATHEMATICAL SCRIPT SMALL W
unicode_nfkc_cf(0x1D4CD, [0x0078]).						% L&       MATHEMATICAL SCRIPT SMALL X
unicode_nfkc_cf(0x1D4CE, [0x0079]).						% L&       MATHEMATICAL SCRIPT SMALL Y
unicode_nfkc_cf(0x1D4CF, [0x007A]).						% L&       MATHEMATICAL SCRIPT SMALL Z
unicode_nfkc_cf(0x1D4D0, [0x0061]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL A
unicode_nfkc_cf(0x1D4D1, [0x0062]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL B
unicode_nfkc_cf(0x1D4D2, [0x0063]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL C
unicode_nfkc_cf(0x1D4D3, [0x0064]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL D
unicode_nfkc_cf(0x1D4D4, [0x0065]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL E
unicode_nfkc_cf(0x1D4D5, [0x0066]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL F
unicode_nfkc_cf(0x1D4D6, [0x0067]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL G
unicode_nfkc_cf(0x1D4D7, [0x0068]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL H
unicode_nfkc_cf(0x1D4D8, [0x0069]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL I
unicode_nfkc_cf(0x1D4D9, [0x006A]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL J
unicode_nfkc_cf(0x1D4DA, [0x006B]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL K
unicode_nfkc_cf(0x1D4DB, [0x006C]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL L
unicode_nfkc_cf(0x1D4DC, [0x006D]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL M
unicode_nfkc_cf(0x1D4DD, [0x006E]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL N
unicode_nfkc_cf(0x1D4DE, [0x006F]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL O
unicode_nfkc_cf(0x1D4DF, [0x0070]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL P
unicode_nfkc_cf(0x1D4E0, [0x0071]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL Q
unicode_nfkc_cf(0x1D4E1, [0x0072]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL R
unicode_nfkc_cf(0x1D4E2, [0x0073]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL S
unicode_nfkc_cf(0x1D4E3, [0x0074]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL T
unicode_nfkc_cf(0x1D4E4, [0x0075]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL U
unicode_nfkc_cf(0x1D4E5, [0x0076]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL V
unicode_nfkc_cf(0x1D4E6, [0x0077]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL W
unicode_nfkc_cf(0x1D4E7, [0x0078]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL X
unicode_nfkc_cf(0x1D4E8, [0x0079]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL Y
unicode_nfkc_cf(0x1D4E9, [0x007A]).						% L&       MATHEMATICAL BOLD SCRIPT CAPITAL Z
unicode_nfkc_cf(0x1D4EA, [0x0061]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL A
unicode_nfkc_cf(0x1D4EB, [0x0062]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL B
unicode_nfkc_cf(0x1D4EC, [0x0063]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL C
unicode_nfkc_cf(0x1D4ED, [0x0064]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL D
unicode_nfkc_cf(0x1D4EE, [0x0065]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL E
unicode_nfkc_cf(0x1D4EF, [0x0066]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL F
unicode_nfkc_cf(0x1D4F0, [0x0067]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL G
unicode_nfkc_cf(0x1D4F1, [0x0068]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL H
unicode_nfkc_cf(0x1D4F2, [0x0069]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL I
unicode_nfkc_cf(0x1D4F3, [0x006A]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL J
unicode_nfkc_cf(0x1D4F4, [0x006B]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL K
unicode_nfkc_cf(0x1D4F5, [0x006C]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL L
unicode_nfkc_cf(0x1D4F6, [0x006D]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL M
unicode_nfkc_cf(0x1D4F7, [0x006E]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL N
unicode_nfkc_cf(0x1D4F8, [0x006F]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL O
unicode_nfkc_cf(0x1D4F9, [0x0070]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL P
unicode_nfkc_cf(0x1D4FA, [0x0071]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL Q
unicode_nfkc_cf(0x1D4FB, [0x0072]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL R
unicode_nfkc_cf(0x1D4FC, [0x0073]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL S
unicode_nfkc_cf(0x1D4FD, [0x0074]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL T
unicode_nfkc_cf(0x1D4FE, [0x0075]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL U
unicode_nfkc_cf(0x1D4FF, [0x0076]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL V
unicode_nfkc_cf(0x1D500, [0x0077]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL W
unicode_nfkc_cf(0x1D501, [0x0078]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL X
unicode_nfkc_cf(0x1D502, [0x0079]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL Y
unicode_nfkc_cf(0x1D503, [0x007A]).						% L&       MATHEMATICAL BOLD SCRIPT SMALL Z
unicode_nfkc_cf(0x1D504, [0x0061]).						% L&       MATHEMATICAL FRAKTUR CAPITAL A
unicode_nfkc_cf(0x1D505, [0x0062]).						% L&       MATHEMATICAL FRAKTUR CAPITAL B
unicode_nfkc_cf(0x1D507, [0x0064]).						% L&       MATHEMATICAL FRAKTUR CAPITAL D
unicode_nfkc_cf(0x1D508, [0x0065]).						% L&       MATHEMATICAL FRAKTUR CAPITAL E
unicode_nfkc_cf(0x1D509, [0x0066]).						% L&       MATHEMATICAL FRAKTUR CAPITAL F
unicode_nfkc_cf(0x1D50A, [0x0067]).						% L&       MATHEMATICAL FRAKTUR CAPITAL G
unicode_nfkc_cf(0x1D50D, [0x006A]).						% L&       MATHEMATICAL FRAKTUR CAPITAL J
unicode_nfkc_cf(0x1D50E, [0x006B]).						% L&       MATHEMATICAL FRAKTUR CAPITAL K
unicode_nfkc_cf(0x1D50F, [0x006C]).						% L&       MATHEMATICAL FRAKTUR CAPITAL L
unicode_nfkc_cf(0x1D510, [0x006D]).						% L&       MATHEMATICAL FRAKTUR CAPITAL M
unicode_nfkc_cf(0x1D511, [0x006E]).						% L&       MATHEMATICAL FRAKTUR CAPITAL N
unicode_nfkc_cf(0x1D512, [0x006F]).						% L&       MATHEMATICAL FRAKTUR CAPITAL O
unicode_nfkc_cf(0x1D513, [0x0070]).						% L&       MATHEMATICAL FRAKTUR CAPITAL P
unicode_nfkc_cf(0x1D514, [0x0071]).						% L&       MATHEMATICAL FRAKTUR CAPITAL Q
unicode_nfkc_cf(0x1D516, [0x0073]).						% L&       MATHEMATICAL FRAKTUR CAPITAL S
unicode_nfkc_cf(0x1D517, [0x0074]).						% L&       MATHEMATICAL FRAKTUR CAPITAL T
unicode_nfkc_cf(0x1D518, [0x0075]).						% L&       MATHEMATICAL FRAKTUR CAPITAL U
unicode_nfkc_cf(0x1D519, [0x0076]).						% L&       MATHEMATICAL FRAKTUR CAPITAL V
unicode_nfkc_cf(0x1D51A, [0x0077]).						% L&       MATHEMATICAL FRAKTUR CAPITAL W
unicode_nfkc_cf(0x1D51B, [0x0078]).						% L&       MATHEMATICAL FRAKTUR CAPITAL X
unicode_nfkc_cf(0x1D51C, [0x0079]).						% L&       MATHEMATICAL FRAKTUR CAPITAL Y
unicode_nfkc_cf(0x1D51E, [0x0061]).						% L&       MATHEMATICAL FRAKTUR SMALL A
unicode_nfkc_cf(0x1D51F, [0x0062]).						% L&       MATHEMATICAL FRAKTUR SMALL B
unicode_nfkc_cf(0x1D520, [0x0063]).						% L&       MATHEMATICAL FRAKTUR SMALL C
unicode_nfkc_cf(0x1D521, [0x0064]).						% L&       MATHEMATICAL FRAKTUR SMALL D
unicode_nfkc_cf(0x1D522, [0x0065]).						% L&       MATHEMATICAL FRAKTUR SMALL E
unicode_nfkc_cf(0x1D523, [0x0066]).						% L&       MATHEMATICAL FRAKTUR SMALL F
unicode_nfkc_cf(0x1D524, [0x0067]).						% L&       MATHEMATICAL FRAKTUR SMALL G
unicode_nfkc_cf(0x1D525, [0x0068]).						% L&       MATHEMATICAL FRAKTUR SMALL H
unicode_nfkc_cf(0x1D526, [0x0069]).						% L&       MATHEMATICAL FRAKTUR SMALL I
unicode_nfkc_cf(0x1D527, [0x006A]).						% L&       MATHEMATICAL FRAKTUR SMALL J
unicode_nfkc_cf(0x1D528, [0x006B]).						% L&       MATHEMATICAL FRAKTUR SMALL K
unicode_nfkc_cf(0x1D529, [0x006C]).						% L&       MATHEMATICAL FRAKTUR SMALL L
unicode_nfkc_cf(0x1D52A, [0x006D]).						% L&       MATHEMATICAL FRAKTUR SMALL M
unicode_nfkc_cf(0x1D52B, [0x006E]).						% L&       MATHEMATICAL FRAKTUR SMALL N
unicode_nfkc_cf(0x1D52C, [0x006F]).						% L&       MATHEMATICAL FRAKTUR SMALL O
unicode_nfkc_cf(0x1D52D, [0x0070]).						% L&       MATHEMATICAL FRAKTUR SMALL P
unicode_nfkc_cf(0x1D52E, [0x0071]).						% L&       MATHEMATICAL FRAKTUR SMALL Q
unicode_nfkc_cf(0x1D52F, [0x0072]).						% L&       MATHEMATICAL FRAKTUR SMALL R
unicode_nfkc_cf(0x1D530, [0x0073]).						% L&       MATHEMATICAL FRAKTUR SMALL S
unicode_nfkc_cf(0x1D531, [0x0074]).						% L&       MATHEMATICAL FRAKTUR SMALL T
unicode_nfkc_cf(0x1D532, [0x0075]).						% L&       MATHEMATICAL FRAKTUR SMALL U
unicode_nfkc_cf(0x1D533, [0x0076]).						% L&       MATHEMATICAL FRAKTUR SMALL V
unicode_nfkc_cf(0x1D534, [0x0077]).						% L&       MATHEMATICAL FRAKTUR SMALL W
unicode_nfkc_cf(0x1D535, [0x0078]).						% L&       MATHEMATICAL FRAKTUR SMALL X
unicode_nfkc_cf(0x1D536, [0x0079]).						% L&       MATHEMATICAL FRAKTUR SMALL Y
unicode_nfkc_cf(0x1D537, [0x007A]).						% L&       MATHEMATICAL FRAKTUR SMALL Z
unicode_nfkc_cf(0x1D538, [0x0061]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL A
unicode_nfkc_cf(0x1D539, [0x0062]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_nfkc_cf(0x1D53B, [0x0064]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL D
unicode_nfkc_cf(0x1D53C, [0x0065]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL E
unicode_nfkc_cf(0x1D53D, [0x0066]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL F
unicode_nfkc_cf(0x1D53E, [0x0067]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_nfkc_cf(0x1D540, [0x0069]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL I
unicode_nfkc_cf(0x1D541, [0x006A]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL J
unicode_nfkc_cf(0x1D542, [0x006B]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL K
unicode_nfkc_cf(0x1D543, [0x006C]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL L
unicode_nfkc_cf(0x1D544, [0x006D]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_nfkc_cf(0x1D546, [0x006F]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_nfkc_cf(0x1D54A, [0x0073]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL S
unicode_nfkc_cf(0x1D54B, [0x0074]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL T
unicode_nfkc_cf(0x1D54C, [0x0075]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL U
unicode_nfkc_cf(0x1D54D, [0x0076]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL V
unicode_nfkc_cf(0x1D54E, [0x0077]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL W
unicode_nfkc_cf(0x1D54F, [0x0078]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL X
unicode_nfkc_cf(0x1D550, [0x0079]).						% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_nfkc_cf(0x1D552, [0x0061]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL A
unicode_nfkc_cf(0x1D553, [0x0062]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL B
unicode_nfkc_cf(0x1D554, [0x0063]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL C
unicode_nfkc_cf(0x1D555, [0x0064]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL D
unicode_nfkc_cf(0x1D556, [0x0065]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL E
unicode_nfkc_cf(0x1D557, [0x0066]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL F
unicode_nfkc_cf(0x1D558, [0x0067]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL G
unicode_nfkc_cf(0x1D559, [0x0068]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL H
unicode_nfkc_cf(0x1D55A, [0x0069]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL I
unicode_nfkc_cf(0x1D55B, [0x006A]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL J
unicode_nfkc_cf(0x1D55C, [0x006B]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL K
unicode_nfkc_cf(0x1D55D, [0x006C]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL L
unicode_nfkc_cf(0x1D55E, [0x006D]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL M
unicode_nfkc_cf(0x1D55F, [0x006E]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL N
unicode_nfkc_cf(0x1D560, [0x006F]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL O
unicode_nfkc_cf(0x1D561, [0x0070]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL P
unicode_nfkc_cf(0x1D562, [0x0071]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL Q
unicode_nfkc_cf(0x1D563, [0x0072]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL R
unicode_nfkc_cf(0x1D564, [0x0073]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL S
unicode_nfkc_cf(0x1D565, [0x0074]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL T
unicode_nfkc_cf(0x1D566, [0x0075]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL U
unicode_nfkc_cf(0x1D567, [0x0076]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL V
unicode_nfkc_cf(0x1D568, [0x0077]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL W
unicode_nfkc_cf(0x1D569, [0x0078]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL X
unicode_nfkc_cf(0x1D56A, [0x0079]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL Y
unicode_nfkc_cf(0x1D56B, [0x007A]).						% L&       MATHEMATICAL DOUBLE-STRUCK SMALL Z
unicode_nfkc_cf(0x1D56C, [0x0061]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL A
unicode_nfkc_cf(0x1D56D, [0x0062]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL B
unicode_nfkc_cf(0x1D56E, [0x0063]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL C
unicode_nfkc_cf(0x1D56F, [0x0064]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL D
unicode_nfkc_cf(0x1D570, [0x0065]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL E
unicode_nfkc_cf(0x1D571, [0x0066]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL F
unicode_nfkc_cf(0x1D572, [0x0067]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL G
unicode_nfkc_cf(0x1D573, [0x0068]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL H
unicode_nfkc_cf(0x1D574, [0x0069]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL I
unicode_nfkc_cf(0x1D575, [0x006A]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL J
unicode_nfkc_cf(0x1D576, [0x006B]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL K
unicode_nfkc_cf(0x1D577, [0x006C]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL L
unicode_nfkc_cf(0x1D578, [0x006D]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL M
unicode_nfkc_cf(0x1D579, [0x006E]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL N
unicode_nfkc_cf(0x1D57A, [0x006F]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL O
unicode_nfkc_cf(0x1D57B, [0x0070]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL P
unicode_nfkc_cf(0x1D57C, [0x0071]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL Q
unicode_nfkc_cf(0x1D57D, [0x0072]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL R
unicode_nfkc_cf(0x1D57E, [0x0073]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL S
unicode_nfkc_cf(0x1D57F, [0x0074]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL T
unicode_nfkc_cf(0x1D580, [0x0075]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL U
unicode_nfkc_cf(0x1D581, [0x0076]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL V
unicode_nfkc_cf(0x1D582, [0x0077]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL W
unicode_nfkc_cf(0x1D583, [0x0078]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL X
unicode_nfkc_cf(0x1D584, [0x0079]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL Y
unicode_nfkc_cf(0x1D585, [0x007A]).						% L&       MATHEMATICAL BOLD FRAKTUR CAPITAL Z
unicode_nfkc_cf(0x1D586, [0x0061]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL A
unicode_nfkc_cf(0x1D587, [0x0062]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL B
unicode_nfkc_cf(0x1D588, [0x0063]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL C
unicode_nfkc_cf(0x1D589, [0x0064]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL D
unicode_nfkc_cf(0x1D58A, [0x0065]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL E
unicode_nfkc_cf(0x1D58B, [0x0066]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL F
unicode_nfkc_cf(0x1D58C, [0x0067]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL G
unicode_nfkc_cf(0x1D58D, [0x0068]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL H
unicode_nfkc_cf(0x1D58E, [0x0069]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL I
unicode_nfkc_cf(0x1D58F, [0x006A]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL J
unicode_nfkc_cf(0x1D590, [0x006B]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL K
unicode_nfkc_cf(0x1D591, [0x006C]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL L
unicode_nfkc_cf(0x1D592, [0x006D]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL M
unicode_nfkc_cf(0x1D593, [0x006E]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL N
unicode_nfkc_cf(0x1D594, [0x006F]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL O
unicode_nfkc_cf(0x1D595, [0x0070]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL P
unicode_nfkc_cf(0x1D596, [0x0071]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL Q
unicode_nfkc_cf(0x1D597, [0x0072]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL R
unicode_nfkc_cf(0x1D598, [0x0073]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL S
unicode_nfkc_cf(0x1D599, [0x0074]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL T
unicode_nfkc_cf(0x1D59A, [0x0075]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL U
unicode_nfkc_cf(0x1D59B, [0x0076]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL V
unicode_nfkc_cf(0x1D59C, [0x0077]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL W
unicode_nfkc_cf(0x1D59D, [0x0078]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL X
unicode_nfkc_cf(0x1D59E, [0x0079]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL Y
unicode_nfkc_cf(0x1D59F, [0x007A]).						% L&       MATHEMATICAL BOLD FRAKTUR SMALL Z
unicode_nfkc_cf(0x1D5A0, [0x0061]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL A
unicode_nfkc_cf(0x1D5A1, [0x0062]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL B
unicode_nfkc_cf(0x1D5A2, [0x0063]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL C
unicode_nfkc_cf(0x1D5A3, [0x0064]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL D
unicode_nfkc_cf(0x1D5A4, [0x0065]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL E
unicode_nfkc_cf(0x1D5A5, [0x0066]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL F
unicode_nfkc_cf(0x1D5A6, [0x0067]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL G
unicode_nfkc_cf(0x1D5A7, [0x0068]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL H
unicode_nfkc_cf(0x1D5A8, [0x0069]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL I
unicode_nfkc_cf(0x1D5A9, [0x006A]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL J
unicode_nfkc_cf(0x1D5AA, [0x006B]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL K
unicode_nfkc_cf(0x1D5AB, [0x006C]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL L
unicode_nfkc_cf(0x1D5AC, [0x006D]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL M
unicode_nfkc_cf(0x1D5AD, [0x006E]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL N
unicode_nfkc_cf(0x1D5AE, [0x006F]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL O
unicode_nfkc_cf(0x1D5AF, [0x0070]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL P
unicode_nfkc_cf(0x1D5B0, [0x0071]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL Q
unicode_nfkc_cf(0x1D5B1, [0x0072]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL R
unicode_nfkc_cf(0x1D5B2, [0x0073]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL S
unicode_nfkc_cf(0x1D5B3, [0x0074]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL T
unicode_nfkc_cf(0x1D5B4, [0x0075]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL U
unicode_nfkc_cf(0x1D5B5, [0x0076]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL V
unicode_nfkc_cf(0x1D5B6, [0x0077]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL W
unicode_nfkc_cf(0x1D5B7, [0x0078]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL X
unicode_nfkc_cf(0x1D5B8, [0x0079]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL Y
unicode_nfkc_cf(0x1D5B9, [0x007A]).						% L&       MATHEMATICAL SANS-SERIF CAPITAL Z
unicode_nfkc_cf(0x1D5BA, [0x0061]).						% L&       MATHEMATICAL SANS-SERIF SMALL A
unicode_nfkc_cf(0x1D5BB, [0x0062]).						% L&       MATHEMATICAL SANS-SERIF SMALL B
unicode_nfkc_cf(0x1D5BC, [0x0063]).						% L&       MATHEMATICAL SANS-SERIF SMALL C
unicode_nfkc_cf(0x1D5BD, [0x0064]).						% L&       MATHEMATICAL SANS-SERIF SMALL D
unicode_nfkc_cf(0x1D5BE, [0x0065]).						% L&       MATHEMATICAL SANS-SERIF SMALL E
unicode_nfkc_cf(0x1D5BF, [0x0066]).						% L&       MATHEMATICAL SANS-SERIF SMALL F
unicode_nfkc_cf(0x1D5C0, [0x0067]).						% L&       MATHEMATICAL SANS-SERIF SMALL G
unicode_nfkc_cf(0x1D5C1, [0x0068]).						% L&       MATHEMATICAL SANS-SERIF SMALL H
unicode_nfkc_cf(0x1D5C2, [0x0069]).						% L&       MATHEMATICAL SANS-SERIF SMALL I
unicode_nfkc_cf(0x1D5C3, [0x006A]).						% L&       MATHEMATICAL SANS-SERIF SMALL J
unicode_nfkc_cf(0x1D5C4, [0x006B]).						% L&       MATHEMATICAL SANS-SERIF SMALL K
unicode_nfkc_cf(0x1D5C5, [0x006C]).						% L&       MATHEMATICAL SANS-SERIF SMALL L
unicode_nfkc_cf(0x1D5C6, [0x006D]).						% L&       MATHEMATICAL SANS-SERIF SMALL M
unicode_nfkc_cf(0x1D5C7, [0x006E]).						% L&       MATHEMATICAL SANS-SERIF SMALL N
unicode_nfkc_cf(0x1D5C8, [0x006F]).						% L&       MATHEMATICAL SANS-SERIF SMALL O
unicode_nfkc_cf(0x1D5C9, [0x0070]).						% L&       MATHEMATICAL SANS-SERIF SMALL P
unicode_nfkc_cf(0x1D5CA, [0x0071]).						% L&       MATHEMATICAL SANS-SERIF SMALL Q
unicode_nfkc_cf(0x1D5CB, [0x0072]).						% L&       MATHEMATICAL SANS-SERIF SMALL R
unicode_nfkc_cf(0x1D5CC, [0x0073]).						% L&       MATHEMATICAL SANS-SERIF SMALL S
unicode_nfkc_cf(0x1D5CD, [0x0074]).						% L&       MATHEMATICAL SANS-SERIF SMALL T
unicode_nfkc_cf(0x1D5CE, [0x0075]).						% L&       MATHEMATICAL SANS-SERIF SMALL U
unicode_nfkc_cf(0x1D5CF, [0x0076]).						% L&       MATHEMATICAL SANS-SERIF SMALL V
unicode_nfkc_cf(0x1D5D0, [0x0077]).						% L&       MATHEMATICAL SANS-SERIF SMALL W
unicode_nfkc_cf(0x1D5D1, [0x0078]).						% L&       MATHEMATICAL SANS-SERIF SMALL X
unicode_nfkc_cf(0x1D5D2, [0x0079]).						% L&       MATHEMATICAL SANS-SERIF SMALL Y
unicode_nfkc_cf(0x1D5D3, [0x007A]).						% L&       MATHEMATICAL SANS-SERIF SMALL Z
unicode_nfkc_cf(0x1D5D4, [0x0061]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL A
unicode_nfkc_cf(0x1D5D5, [0x0062]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL B
unicode_nfkc_cf(0x1D5D6, [0x0063]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL C
unicode_nfkc_cf(0x1D5D7, [0x0064]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL D
unicode_nfkc_cf(0x1D5D8, [0x0065]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL E
unicode_nfkc_cf(0x1D5D9, [0x0066]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL F
unicode_nfkc_cf(0x1D5DA, [0x0067]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL G
unicode_nfkc_cf(0x1D5DB, [0x0068]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL H
unicode_nfkc_cf(0x1D5DC, [0x0069]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL I
unicode_nfkc_cf(0x1D5DD, [0x006A]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL J
unicode_nfkc_cf(0x1D5DE, [0x006B]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL K
unicode_nfkc_cf(0x1D5DF, [0x006C]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL L
unicode_nfkc_cf(0x1D5E0, [0x006D]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL M
unicode_nfkc_cf(0x1D5E1, [0x006E]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL N
unicode_nfkc_cf(0x1D5E2, [0x006F]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL O
unicode_nfkc_cf(0x1D5E3, [0x0070]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL P
unicode_nfkc_cf(0x1D5E4, [0x0071]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL Q
unicode_nfkc_cf(0x1D5E5, [0x0072]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL R
unicode_nfkc_cf(0x1D5E6, [0x0073]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL S
unicode_nfkc_cf(0x1D5E7, [0x0074]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL T
unicode_nfkc_cf(0x1D5E8, [0x0075]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL U
unicode_nfkc_cf(0x1D5E9, [0x0076]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL V
unicode_nfkc_cf(0x1D5EA, [0x0077]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL W
unicode_nfkc_cf(0x1D5EB, [0x0078]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL X
unicode_nfkc_cf(0x1D5EC, [0x0079]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL Y
unicode_nfkc_cf(0x1D5ED, [0x007A]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL Z
unicode_nfkc_cf(0x1D5EE, [0x0061]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL A
unicode_nfkc_cf(0x1D5EF, [0x0062]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL B
unicode_nfkc_cf(0x1D5F0, [0x0063]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL C
unicode_nfkc_cf(0x1D5F1, [0x0064]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL D
unicode_nfkc_cf(0x1D5F2, [0x0065]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL E
unicode_nfkc_cf(0x1D5F3, [0x0066]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL F
unicode_nfkc_cf(0x1D5F4, [0x0067]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL G
unicode_nfkc_cf(0x1D5F5, [0x0068]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL H
unicode_nfkc_cf(0x1D5F6, [0x0069]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL I
unicode_nfkc_cf(0x1D5F7, [0x006A]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL J
unicode_nfkc_cf(0x1D5F8, [0x006B]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL K
unicode_nfkc_cf(0x1D5F9, [0x006C]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL L
unicode_nfkc_cf(0x1D5FA, [0x006D]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL M
unicode_nfkc_cf(0x1D5FB, [0x006E]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL N
unicode_nfkc_cf(0x1D5FC, [0x006F]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL O
unicode_nfkc_cf(0x1D5FD, [0x0070]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL P
unicode_nfkc_cf(0x1D5FE, [0x0071]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL Q
unicode_nfkc_cf(0x1D5FF, [0x0072]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL R
unicode_nfkc_cf(0x1D600, [0x0073]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL S
unicode_nfkc_cf(0x1D601, [0x0074]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL T
unicode_nfkc_cf(0x1D602, [0x0075]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL U
unicode_nfkc_cf(0x1D603, [0x0076]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL V
unicode_nfkc_cf(0x1D604, [0x0077]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL W
unicode_nfkc_cf(0x1D605, [0x0078]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL X
unicode_nfkc_cf(0x1D606, [0x0079]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL Y
unicode_nfkc_cf(0x1D607, [0x007A]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL Z
unicode_nfkc_cf(0x1D608, [0x0061]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL A
unicode_nfkc_cf(0x1D609, [0x0062]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL B
unicode_nfkc_cf(0x1D60A, [0x0063]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL C
unicode_nfkc_cf(0x1D60B, [0x0064]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL D
unicode_nfkc_cf(0x1D60C, [0x0065]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL E
unicode_nfkc_cf(0x1D60D, [0x0066]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL F
unicode_nfkc_cf(0x1D60E, [0x0067]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL G
unicode_nfkc_cf(0x1D60F, [0x0068]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL H
unicode_nfkc_cf(0x1D610, [0x0069]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL I
unicode_nfkc_cf(0x1D611, [0x006A]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL J
unicode_nfkc_cf(0x1D612, [0x006B]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL K
unicode_nfkc_cf(0x1D613, [0x006C]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL L
unicode_nfkc_cf(0x1D614, [0x006D]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL M
unicode_nfkc_cf(0x1D615, [0x006E]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL N
unicode_nfkc_cf(0x1D616, [0x006F]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL O
unicode_nfkc_cf(0x1D617, [0x0070]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL P
unicode_nfkc_cf(0x1D618, [0x0071]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL Q
unicode_nfkc_cf(0x1D619, [0x0072]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL R
unicode_nfkc_cf(0x1D61A, [0x0073]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL S
unicode_nfkc_cf(0x1D61B, [0x0074]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL T
unicode_nfkc_cf(0x1D61C, [0x0075]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL U
unicode_nfkc_cf(0x1D61D, [0x0076]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL V
unicode_nfkc_cf(0x1D61E, [0x0077]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL W
unicode_nfkc_cf(0x1D61F, [0x0078]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL X
unicode_nfkc_cf(0x1D620, [0x0079]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL Y
unicode_nfkc_cf(0x1D621, [0x007A]).						% L&       MATHEMATICAL SANS-SERIF ITALIC CAPITAL Z
unicode_nfkc_cf(0x1D622, [0x0061]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL A
unicode_nfkc_cf(0x1D623, [0x0062]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL B
unicode_nfkc_cf(0x1D624, [0x0063]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL C
unicode_nfkc_cf(0x1D625, [0x0064]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL D
unicode_nfkc_cf(0x1D626, [0x0065]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL E
unicode_nfkc_cf(0x1D627, [0x0066]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL F
unicode_nfkc_cf(0x1D628, [0x0067]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL G
unicode_nfkc_cf(0x1D629, [0x0068]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL H
unicode_nfkc_cf(0x1D62A, [0x0069]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL I
unicode_nfkc_cf(0x1D62B, [0x006A]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL J
unicode_nfkc_cf(0x1D62C, [0x006B]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL K
unicode_nfkc_cf(0x1D62D, [0x006C]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL L
unicode_nfkc_cf(0x1D62E, [0x006D]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL M
unicode_nfkc_cf(0x1D62F, [0x006E]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL N
unicode_nfkc_cf(0x1D630, [0x006F]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL O
unicode_nfkc_cf(0x1D631, [0x0070]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL P
unicode_nfkc_cf(0x1D632, [0x0071]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL Q
unicode_nfkc_cf(0x1D633, [0x0072]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL R
unicode_nfkc_cf(0x1D634, [0x0073]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL S
unicode_nfkc_cf(0x1D635, [0x0074]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL T
unicode_nfkc_cf(0x1D636, [0x0075]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL U
unicode_nfkc_cf(0x1D637, [0x0076]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL V
unicode_nfkc_cf(0x1D638, [0x0077]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL W
unicode_nfkc_cf(0x1D639, [0x0078]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL X
unicode_nfkc_cf(0x1D63A, [0x0079]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL Y
unicode_nfkc_cf(0x1D63B, [0x007A]).						% L&       MATHEMATICAL SANS-SERIF ITALIC SMALL Z
unicode_nfkc_cf(0x1D63C, [0x0061]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL A
unicode_nfkc_cf(0x1D63D, [0x0062]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL B
unicode_nfkc_cf(0x1D63E, [0x0063]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL C
unicode_nfkc_cf(0x1D63F, [0x0064]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL D
unicode_nfkc_cf(0x1D640, [0x0065]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL E
unicode_nfkc_cf(0x1D641, [0x0066]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL F
unicode_nfkc_cf(0x1D642, [0x0067]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL G
unicode_nfkc_cf(0x1D643, [0x0068]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL H
unicode_nfkc_cf(0x1D644, [0x0069]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL I
unicode_nfkc_cf(0x1D645, [0x006A]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL J
unicode_nfkc_cf(0x1D646, [0x006B]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL K
unicode_nfkc_cf(0x1D647, [0x006C]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL L
unicode_nfkc_cf(0x1D648, [0x006D]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL M
unicode_nfkc_cf(0x1D649, [0x006E]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL N
unicode_nfkc_cf(0x1D64A, [0x006F]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL O
unicode_nfkc_cf(0x1D64B, [0x0070]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL P
unicode_nfkc_cf(0x1D64C, [0x0071]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Q
unicode_nfkc_cf(0x1D64D, [0x0072]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL R
unicode_nfkc_cf(0x1D64E, [0x0073]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL S
unicode_nfkc_cf(0x1D64F, [0x0074]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL T
unicode_nfkc_cf(0x1D650, [0x0075]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL U
unicode_nfkc_cf(0x1D651, [0x0076]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL V
unicode_nfkc_cf(0x1D652, [0x0077]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL W
unicode_nfkc_cf(0x1D653, [0x0078]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL X
unicode_nfkc_cf(0x1D654, [0x0079]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Y
unicode_nfkc_cf(0x1D655, [0x007A]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Z
unicode_nfkc_cf(0x1D656, [0x0061]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL A
unicode_nfkc_cf(0x1D657, [0x0062]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL B
unicode_nfkc_cf(0x1D658, [0x0063]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL C
unicode_nfkc_cf(0x1D659, [0x0064]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL D
unicode_nfkc_cf(0x1D65A, [0x0065]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL E
unicode_nfkc_cf(0x1D65B, [0x0066]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL F
unicode_nfkc_cf(0x1D65C, [0x0067]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL G
unicode_nfkc_cf(0x1D65D, [0x0068]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL H
unicode_nfkc_cf(0x1D65E, [0x0069]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL I
unicode_nfkc_cf(0x1D65F, [0x006A]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL J
unicode_nfkc_cf(0x1D660, [0x006B]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL K
unicode_nfkc_cf(0x1D661, [0x006C]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL L
unicode_nfkc_cf(0x1D662, [0x006D]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL M
unicode_nfkc_cf(0x1D663, [0x006E]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL N
unicode_nfkc_cf(0x1D664, [0x006F]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL O
unicode_nfkc_cf(0x1D665, [0x0070]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL P
unicode_nfkc_cf(0x1D666, [0x0071]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Q
unicode_nfkc_cf(0x1D667, [0x0072]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL R
unicode_nfkc_cf(0x1D668, [0x0073]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL S
unicode_nfkc_cf(0x1D669, [0x0074]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL T
unicode_nfkc_cf(0x1D66A, [0x0075]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL U
unicode_nfkc_cf(0x1D66B, [0x0076]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL V
unicode_nfkc_cf(0x1D66C, [0x0077]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL W
unicode_nfkc_cf(0x1D66D, [0x0078]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL X
unicode_nfkc_cf(0x1D66E, [0x0079]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Y
unicode_nfkc_cf(0x1D66F, [0x007A]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Z
unicode_nfkc_cf(0x1D670, [0x0061]).						% L&       MATHEMATICAL MONOSPACE CAPITAL A
unicode_nfkc_cf(0x1D671, [0x0062]).						% L&       MATHEMATICAL MONOSPACE CAPITAL B
unicode_nfkc_cf(0x1D672, [0x0063]).						% L&       MATHEMATICAL MONOSPACE CAPITAL C
unicode_nfkc_cf(0x1D673, [0x0064]).						% L&       MATHEMATICAL MONOSPACE CAPITAL D
unicode_nfkc_cf(0x1D674, [0x0065]).						% L&       MATHEMATICAL MONOSPACE CAPITAL E
unicode_nfkc_cf(0x1D675, [0x0066]).						% L&       MATHEMATICAL MONOSPACE CAPITAL F
unicode_nfkc_cf(0x1D676, [0x0067]).						% L&       MATHEMATICAL MONOSPACE CAPITAL G
unicode_nfkc_cf(0x1D677, [0x0068]).						% L&       MATHEMATICAL MONOSPACE CAPITAL H
unicode_nfkc_cf(0x1D678, [0x0069]).						% L&       MATHEMATICAL MONOSPACE CAPITAL I
unicode_nfkc_cf(0x1D679, [0x006A]).						% L&       MATHEMATICAL MONOSPACE CAPITAL J
unicode_nfkc_cf(0x1D67A, [0x006B]).						% L&       MATHEMATICAL MONOSPACE CAPITAL K
unicode_nfkc_cf(0x1D67B, [0x006C]).						% L&       MATHEMATICAL MONOSPACE CAPITAL L
unicode_nfkc_cf(0x1D67C, [0x006D]).						% L&       MATHEMATICAL MONOSPACE CAPITAL M
unicode_nfkc_cf(0x1D67D, [0x006E]).						% L&       MATHEMATICAL MONOSPACE CAPITAL N
unicode_nfkc_cf(0x1D67E, [0x006F]).						% L&       MATHEMATICAL MONOSPACE CAPITAL O
unicode_nfkc_cf(0x1D67F, [0x0070]).						% L&       MATHEMATICAL MONOSPACE CAPITAL P
unicode_nfkc_cf(0x1D680, [0x0071]).						% L&       MATHEMATICAL MONOSPACE CAPITAL Q
unicode_nfkc_cf(0x1D681, [0x0072]).						% L&       MATHEMATICAL MONOSPACE CAPITAL R
unicode_nfkc_cf(0x1D682, [0x0073]).						% L&       MATHEMATICAL MONOSPACE CAPITAL S
unicode_nfkc_cf(0x1D683, [0x0074]).						% L&       MATHEMATICAL MONOSPACE CAPITAL T
unicode_nfkc_cf(0x1D684, [0x0075]).						% L&       MATHEMATICAL MONOSPACE CAPITAL U
unicode_nfkc_cf(0x1D685, [0x0076]).						% L&       MATHEMATICAL MONOSPACE CAPITAL V
unicode_nfkc_cf(0x1D686, [0x0077]).						% L&       MATHEMATICAL MONOSPACE CAPITAL W
unicode_nfkc_cf(0x1D687, [0x0078]).						% L&       MATHEMATICAL MONOSPACE CAPITAL X
unicode_nfkc_cf(0x1D688, [0x0079]).						% L&       MATHEMATICAL MONOSPACE CAPITAL Y
unicode_nfkc_cf(0x1D689, [0x007A]).						% L&       MATHEMATICAL MONOSPACE CAPITAL Z
unicode_nfkc_cf(0x1D68A, [0x0061]).						% L&       MATHEMATICAL MONOSPACE SMALL A
unicode_nfkc_cf(0x1D68B, [0x0062]).						% L&       MATHEMATICAL MONOSPACE SMALL B
unicode_nfkc_cf(0x1D68C, [0x0063]).						% L&       MATHEMATICAL MONOSPACE SMALL C
unicode_nfkc_cf(0x1D68D, [0x0064]).						% L&       MATHEMATICAL MONOSPACE SMALL D
unicode_nfkc_cf(0x1D68E, [0x0065]).						% L&       MATHEMATICAL MONOSPACE SMALL E
unicode_nfkc_cf(0x1D68F, [0x0066]).						% L&       MATHEMATICAL MONOSPACE SMALL F
unicode_nfkc_cf(0x1D690, [0x0067]).						% L&       MATHEMATICAL MONOSPACE SMALL G
unicode_nfkc_cf(0x1D691, [0x0068]).						% L&       MATHEMATICAL MONOSPACE SMALL H
unicode_nfkc_cf(0x1D692, [0x0069]).						% L&       MATHEMATICAL MONOSPACE SMALL I
unicode_nfkc_cf(0x1D693, [0x006A]).						% L&       MATHEMATICAL MONOSPACE SMALL J
unicode_nfkc_cf(0x1D694, [0x006B]).						% L&       MATHEMATICAL MONOSPACE SMALL K
unicode_nfkc_cf(0x1D695, [0x006C]).						% L&       MATHEMATICAL MONOSPACE SMALL L
unicode_nfkc_cf(0x1D696, [0x006D]).						% L&       MATHEMATICAL MONOSPACE SMALL M
unicode_nfkc_cf(0x1D697, [0x006E]).						% L&       MATHEMATICAL MONOSPACE SMALL N
unicode_nfkc_cf(0x1D698, [0x006F]).						% L&       MATHEMATICAL MONOSPACE SMALL O
unicode_nfkc_cf(0x1D699, [0x0070]).						% L&       MATHEMATICAL MONOSPACE SMALL P
unicode_nfkc_cf(0x1D69A, [0x0071]).						% L&       MATHEMATICAL MONOSPACE SMALL Q
unicode_nfkc_cf(0x1D69B, [0x0072]).						% L&       MATHEMATICAL MONOSPACE SMALL R
unicode_nfkc_cf(0x1D69C, [0x0073]).						% L&       MATHEMATICAL MONOSPACE SMALL S
unicode_nfkc_cf(0x1D69D, [0x0074]).						% L&       MATHEMATICAL MONOSPACE SMALL T
unicode_nfkc_cf(0x1D69E, [0x0075]).						% L&       MATHEMATICAL MONOSPACE SMALL U
unicode_nfkc_cf(0x1D69F, [0x0076]).						% L&       MATHEMATICAL MONOSPACE SMALL V
unicode_nfkc_cf(0x1D6A0, [0x0077]).						% L&       MATHEMATICAL MONOSPACE SMALL W
unicode_nfkc_cf(0x1D6A1, [0x0078]).						% L&       MATHEMATICAL MONOSPACE SMALL X
unicode_nfkc_cf(0x1D6A2, [0x0079]).						% L&       MATHEMATICAL MONOSPACE SMALL Y
unicode_nfkc_cf(0x1D6A3, [0x007A]).						% L&       MATHEMATICAL MONOSPACE SMALL Z
unicode_nfkc_cf(0x1D6A4, [0x0131]).						% L&       MATHEMATICAL ITALIC SMALL DOTLESS I
unicode_nfkc_cf(0x1D6A5, [0x0237]).						% L&       MATHEMATICAL ITALIC SMALL DOTLESS J
unicode_nfkc_cf(0x1D6A8, [0x03B1]).						% L&       MATHEMATICAL BOLD CAPITAL ALPHA
unicode_nfkc_cf(0x1D6A9, [0x03B2]).						% L&       MATHEMATICAL BOLD CAPITAL BETA
unicode_nfkc_cf(0x1D6AA, [0x03B3]).						% L&       MATHEMATICAL BOLD CAPITAL GAMMA
unicode_nfkc_cf(0x1D6AB, [0x03B4]).						% L&       MATHEMATICAL BOLD CAPITAL DELTA
unicode_nfkc_cf(0x1D6AC, [0x03B5]).						% L&       MATHEMATICAL BOLD CAPITAL EPSILON
unicode_nfkc_cf(0x1D6AD, [0x03B6]).						% L&       MATHEMATICAL BOLD CAPITAL ZETA
unicode_nfkc_cf(0x1D6AE, [0x03B7]).						% L&       MATHEMATICAL BOLD CAPITAL ETA
unicode_nfkc_cf(0x1D6AF, [0x03B8]).						% L&       MATHEMATICAL BOLD CAPITAL THETA
unicode_nfkc_cf(0x1D6B0, [0x03B9]).						% L&       MATHEMATICAL BOLD CAPITAL IOTA
unicode_nfkc_cf(0x1D6B1, [0x03BA]).						% L&       MATHEMATICAL BOLD CAPITAL KAPPA
unicode_nfkc_cf(0x1D6B2, [0x03BB]).						% L&       MATHEMATICAL BOLD CAPITAL LAMDA
unicode_nfkc_cf(0x1D6B3, [0x03BC]).						% L&       MATHEMATICAL BOLD CAPITAL MU
unicode_nfkc_cf(0x1D6B4, [0x03BD]).						% L&       MATHEMATICAL BOLD CAPITAL NU
unicode_nfkc_cf(0x1D6B5, [0x03BE]).						% L&       MATHEMATICAL BOLD CAPITAL XI
unicode_nfkc_cf(0x1D6B6, [0x03BF]).						% L&       MATHEMATICAL BOLD CAPITAL OMICRON
unicode_nfkc_cf(0x1D6B7, [0x03C0]).						% L&       MATHEMATICAL BOLD CAPITAL PI
unicode_nfkc_cf(0x1D6B8, [0x03C1]).						% L&       MATHEMATICAL BOLD CAPITAL RHO
unicode_nfkc_cf(0x1D6B9, [0x03B8]).						% L&       MATHEMATICAL BOLD CAPITAL THETA SYMBOL
unicode_nfkc_cf(0x1D6BA, [0x03C3]).						% L&       MATHEMATICAL BOLD CAPITAL SIGMA
unicode_nfkc_cf(0x1D6BB, [0x03C4]).						% L&       MATHEMATICAL BOLD CAPITAL TAU
unicode_nfkc_cf(0x1D6BC, [0x03C5]).						% L&       MATHEMATICAL BOLD CAPITAL UPSILON
unicode_nfkc_cf(0x1D6BD, [0x03C6]).						% L&       MATHEMATICAL BOLD CAPITAL PHI
unicode_nfkc_cf(0x1D6BE, [0x03C7]).						% L&       MATHEMATICAL BOLD CAPITAL CHI
unicode_nfkc_cf(0x1D6BF, [0x03C8]).						% L&       MATHEMATICAL BOLD CAPITAL PSI
unicode_nfkc_cf(0x1D6C0, [0x03C9]).						% L&       MATHEMATICAL BOLD CAPITAL OMEGA
unicode_nfkc_cf(0x1D6C1, [0x2207]).						% Sm       MATHEMATICAL BOLD NABLA
unicode_nfkc_cf(0x1D6C2, [0x03B1]).						% L&       MATHEMATICAL BOLD SMALL ALPHA
unicode_nfkc_cf(0x1D6C3, [0x03B2]).						% L&       MATHEMATICAL BOLD SMALL BETA
unicode_nfkc_cf(0x1D6C4, [0x03B3]).						% L&       MATHEMATICAL BOLD SMALL GAMMA
unicode_nfkc_cf(0x1D6C5, [0x03B4]).						% L&       MATHEMATICAL BOLD SMALL DELTA
unicode_nfkc_cf(0x1D6C6, [0x03B5]).						% L&       MATHEMATICAL BOLD SMALL EPSILON
unicode_nfkc_cf(0x1D6C7, [0x03B6]).						% L&       MATHEMATICAL BOLD SMALL ZETA
unicode_nfkc_cf(0x1D6C8, [0x03B7]).						% L&       MATHEMATICAL BOLD SMALL ETA
unicode_nfkc_cf(0x1D6C9, [0x03B8]).						% L&       MATHEMATICAL BOLD SMALL THETA
unicode_nfkc_cf(0x1D6CA, [0x03B9]).						% L&       MATHEMATICAL BOLD SMALL IOTA
unicode_nfkc_cf(0x1D6CB, [0x03BA]).						% L&       MATHEMATICAL BOLD SMALL KAPPA
unicode_nfkc_cf(0x1D6CC, [0x03BB]).						% L&       MATHEMATICAL BOLD SMALL LAMDA
unicode_nfkc_cf(0x1D6CD, [0x03BC]).						% L&       MATHEMATICAL BOLD SMALL MU
unicode_nfkc_cf(0x1D6CE, [0x03BD]).						% L&       MATHEMATICAL BOLD SMALL NU
unicode_nfkc_cf(0x1D6CF, [0x03BE]).						% L&       MATHEMATICAL BOLD SMALL XI
unicode_nfkc_cf(0x1D6D0, [0x03BF]).						% L&       MATHEMATICAL BOLD SMALL OMICRON
unicode_nfkc_cf(0x1D6D1, [0x03C0]).						% L&       MATHEMATICAL BOLD SMALL PI
unicode_nfkc_cf(0x1D6D2, [0x03C1]).						% L&       MATHEMATICAL BOLD SMALL RHO
unicode_nfkc_cf(0x1D6D3, [0x03C3]).						% L&       MATHEMATICAL BOLD SMALL FINAL SIGMA
unicode_nfkc_cf(0x1D6D4, [0x03C3]).						% L&       MATHEMATICAL BOLD SMALL SIGMA
unicode_nfkc_cf(0x1D6D5, [0x03C4]).						% L&       MATHEMATICAL BOLD SMALL TAU
unicode_nfkc_cf(0x1D6D6, [0x03C5]).						% L&       MATHEMATICAL BOLD SMALL UPSILON
unicode_nfkc_cf(0x1D6D7, [0x03C6]).						% L&       MATHEMATICAL BOLD SMALL PHI
unicode_nfkc_cf(0x1D6D8, [0x03C7]).						% L&       MATHEMATICAL BOLD SMALL CHI
unicode_nfkc_cf(0x1D6D9, [0x03C8]).						% L&       MATHEMATICAL BOLD SMALL PSI
unicode_nfkc_cf(0x1D6DA, [0x03C9]).						% L&       MATHEMATICAL BOLD SMALL OMEGA
unicode_nfkc_cf(0x1D6DB, [0x2202]).						% Sm       MATHEMATICAL BOLD PARTIAL DIFFERENTIAL
unicode_nfkc_cf(0x1D6DC, [0x03B5]).						% L&       MATHEMATICAL BOLD EPSILON SYMBOL
unicode_nfkc_cf(0x1D6DD, [0x03B8]).						% L&       MATHEMATICAL BOLD THETA SYMBOL
unicode_nfkc_cf(0x1D6DE, [0x03BA]).						% L&       MATHEMATICAL BOLD KAPPA SYMBOL
unicode_nfkc_cf(0x1D6DF, [0x03C6]).						% L&       MATHEMATICAL BOLD PHI SYMBOL
unicode_nfkc_cf(0x1D6E0, [0x03C1]).						% L&       MATHEMATICAL BOLD RHO SYMBOL
unicode_nfkc_cf(0x1D6E1, [0x03C0]).						% L&       MATHEMATICAL BOLD PI SYMBOL
unicode_nfkc_cf(0x1D6E2, [0x03B1]).						% L&       MATHEMATICAL ITALIC CAPITAL ALPHA
unicode_nfkc_cf(0x1D6E3, [0x03B2]).						% L&       MATHEMATICAL ITALIC CAPITAL BETA
unicode_nfkc_cf(0x1D6E4, [0x03B3]).						% L&       MATHEMATICAL ITALIC CAPITAL GAMMA
unicode_nfkc_cf(0x1D6E5, [0x03B4]).						% L&       MATHEMATICAL ITALIC CAPITAL DELTA
unicode_nfkc_cf(0x1D6E6, [0x03B5]).						% L&       MATHEMATICAL ITALIC CAPITAL EPSILON
unicode_nfkc_cf(0x1D6E7, [0x03B6]).						% L&       MATHEMATICAL ITALIC CAPITAL ZETA
unicode_nfkc_cf(0x1D6E8, [0x03B7]).						% L&       MATHEMATICAL ITALIC CAPITAL ETA
unicode_nfkc_cf(0x1D6E9, [0x03B8]).						% L&       MATHEMATICAL ITALIC CAPITAL THETA
unicode_nfkc_cf(0x1D6EA, [0x03B9]).						% L&       MATHEMATICAL ITALIC CAPITAL IOTA
unicode_nfkc_cf(0x1D6EB, [0x03BA]).						% L&       MATHEMATICAL ITALIC CAPITAL KAPPA
unicode_nfkc_cf(0x1D6EC, [0x03BB]).						% L&       MATHEMATICAL ITALIC CAPITAL LAMDA
unicode_nfkc_cf(0x1D6ED, [0x03BC]).						% L&       MATHEMATICAL ITALIC CAPITAL MU
unicode_nfkc_cf(0x1D6EE, [0x03BD]).						% L&       MATHEMATICAL ITALIC CAPITAL NU
unicode_nfkc_cf(0x1D6EF, [0x03BE]).						% L&       MATHEMATICAL ITALIC CAPITAL XI
unicode_nfkc_cf(0x1D6F0, [0x03BF]).						% L&       MATHEMATICAL ITALIC CAPITAL OMICRON
unicode_nfkc_cf(0x1D6F1, [0x03C0]).						% L&       MATHEMATICAL ITALIC CAPITAL PI
unicode_nfkc_cf(0x1D6F2, [0x03C1]).						% L&       MATHEMATICAL ITALIC CAPITAL RHO
unicode_nfkc_cf(0x1D6F3, [0x03B8]).						% L&       MATHEMATICAL ITALIC CAPITAL THETA SYMBOL
unicode_nfkc_cf(0x1D6F4, [0x03C3]).						% L&       MATHEMATICAL ITALIC CAPITAL SIGMA
unicode_nfkc_cf(0x1D6F5, [0x03C4]).						% L&       MATHEMATICAL ITALIC CAPITAL TAU
unicode_nfkc_cf(0x1D6F6, [0x03C5]).						% L&       MATHEMATICAL ITALIC CAPITAL UPSILON
unicode_nfkc_cf(0x1D6F7, [0x03C6]).						% L&       MATHEMATICAL ITALIC CAPITAL PHI
unicode_nfkc_cf(0x1D6F8, [0x03C7]).						% L&       MATHEMATICAL ITALIC CAPITAL CHI
unicode_nfkc_cf(0x1D6F9, [0x03C8]).						% L&       MATHEMATICAL ITALIC CAPITAL PSI
unicode_nfkc_cf(0x1D6FA, [0x03C9]).						% L&       MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_nfkc_cf(0x1D6FB, [0x2207]).						% Sm       MATHEMATICAL ITALIC NABLA
unicode_nfkc_cf(0x1D6FC, [0x03B1]).						% L&       MATHEMATICAL ITALIC SMALL ALPHA
unicode_nfkc_cf(0x1D6FD, [0x03B2]).						% L&       MATHEMATICAL ITALIC SMALL BETA
unicode_nfkc_cf(0x1D6FE, [0x03B3]).						% L&       MATHEMATICAL ITALIC SMALL GAMMA
unicode_nfkc_cf(0x1D6FF, [0x03B4]).						% L&       MATHEMATICAL ITALIC SMALL DELTA
unicode_nfkc_cf(0x1D700, [0x03B5]).						% L&       MATHEMATICAL ITALIC SMALL EPSILON
unicode_nfkc_cf(0x1D701, [0x03B6]).						% L&       MATHEMATICAL ITALIC SMALL ZETA
unicode_nfkc_cf(0x1D702, [0x03B7]).						% L&       MATHEMATICAL ITALIC SMALL ETA
unicode_nfkc_cf(0x1D703, [0x03B8]).						% L&       MATHEMATICAL ITALIC SMALL THETA
unicode_nfkc_cf(0x1D704, [0x03B9]).						% L&       MATHEMATICAL ITALIC SMALL IOTA
unicode_nfkc_cf(0x1D705, [0x03BA]).						% L&       MATHEMATICAL ITALIC SMALL KAPPA
unicode_nfkc_cf(0x1D706, [0x03BB]).						% L&       MATHEMATICAL ITALIC SMALL LAMDA
unicode_nfkc_cf(0x1D707, [0x03BC]).						% L&       MATHEMATICAL ITALIC SMALL MU
unicode_nfkc_cf(0x1D708, [0x03BD]).						% L&       MATHEMATICAL ITALIC SMALL NU
unicode_nfkc_cf(0x1D709, [0x03BE]).						% L&       MATHEMATICAL ITALIC SMALL XI
unicode_nfkc_cf(0x1D70A, [0x03BF]).						% L&       MATHEMATICAL ITALIC SMALL OMICRON
unicode_nfkc_cf(0x1D70B, [0x03C0]).						% L&       MATHEMATICAL ITALIC SMALL PI
unicode_nfkc_cf(0x1D70C, [0x03C1]).						% L&       MATHEMATICAL ITALIC SMALL RHO
unicode_nfkc_cf(0x1D70D, [0x03C3]).						% L&       MATHEMATICAL ITALIC SMALL FINAL SIGMA
unicode_nfkc_cf(0x1D70E, [0x03C3]).						% L&       MATHEMATICAL ITALIC SMALL SIGMA
unicode_nfkc_cf(0x1D70F, [0x03C4]).						% L&       MATHEMATICAL ITALIC SMALL TAU
unicode_nfkc_cf(0x1D710, [0x03C5]).						% L&       MATHEMATICAL ITALIC SMALL UPSILON
unicode_nfkc_cf(0x1D711, [0x03C6]).						% L&       MATHEMATICAL ITALIC SMALL PHI
unicode_nfkc_cf(0x1D712, [0x03C7]).						% L&       MATHEMATICAL ITALIC SMALL CHI
unicode_nfkc_cf(0x1D713, [0x03C8]).						% L&       MATHEMATICAL ITALIC SMALL PSI
unicode_nfkc_cf(0x1D714, [0x03C9]).						% L&       MATHEMATICAL ITALIC SMALL OMEGA
unicode_nfkc_cf(0x1D715, [0x2202]).						% Sm       MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL
unicode_nfkc_cf(0x1D716, [0x03B5]).						% L&       MATHEMATICAL ITALIC EPSILON SYMBOL
unicode_nfkc_cf(0x1D717, [0x03B8]).						% L&       MATHEMATICAL ITALIC THETA SYMBOL
unicode_nfkc_cf(0x1D718, [0x03BA]).						% L&       MATHEMATICAL ITALIC KAPPA SYMBOL
unicode_nfkc_cf(0x1D719, [0x03C6]).						% L&       MATHEMATICAL ITALIC PHI SYMBOL
unicode_nfkc_cf(0x1D71A, [0x03C1]).						% L&       MATHEMATICAL ITALIC RHO SYMBOL
unicode_nfkc_cf(0x1D71B, [0x03C0]).						% L&       MATHEMATICAL ITALIC PI SYMBOL
unicode_nfkc_cf(0x1D71C, [0x03B1]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL ALPHA
unicode_nfkc_cf(0x1D71D, [0x03B2]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL BETA
unicode_nfkc_cf(0x1D71E, [0x03B3]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL GAMMA
unicode_nfkc_cf(0x1D71F, [0x03B4]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL DELTA
unicode_nfkc_cf(0x1D720, [0x03B5]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL EPSILON
unicode_nfkc_cf(0x1D721, [0x03B6]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL ZETA
unicode_nfkc_cf(0x1D722, [0x03B7]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL ETA
unicode_nfkc_cf(0x1D723, [0x03B8]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL THETA
unicode_nfkc_cf(0x1D724, [0x03B9]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL IOTA
unicode_nfkc_cf(0x1D725, [0x03BA]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL KAPPA
unicode_nfkc_cf(0x1D726, [0x03BB]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL LAMDA
unicode_nfkc_cf(0x1D727, [0x03BC]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL MU
unicode_nfkc_cf(0x1D728, [0x03BD]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL NU
unicode_nfkc_cf(0x1D729, [0x03BE]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL XI
unicode_nfkc_cf(0x1D72A, [0x03BF]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL OMICRON
unicode_nfkc_cf(0x1D72B, [0x03C0]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL PI
unicode_nfkc_cf(0x1D72C, [0x03C1]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL RHO
unicode_nfkc_cf(0x1D72D, [0x03B8]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL THETA SYMBOL
unicode_nfkc_cf(0x1D72E, [0x03C3]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL SIGMA
unicode_nfkc_cf(0x1D72F, [0x03C4]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL TAU
unicode_nfkc_cf(0x1D730, [0x03C5]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL UPSILON
unicode_nfkc_cf(0x1D731, [0x03C6]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL PHI
unicode_nfkc_cf(0x1D732, [0x03C7]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL CHI
unicode_nfkc_cf(0x1D733, [0x03C8]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL PSI
unicode_nfkc_cf(0x1D734, [0x03C9]).						% L&       MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_nfkc_cf(0x1D735, [0x2207]).						% Sm       MATHEMATICAL BOLD ITALIC NABLA
unicode_nfkc_cf(0x1D736, [0x03B1]).						% L&       MATHEMATICAL BOLD ITALIC SMALL ALPHA
unicode_nfkc_cf(0x1D737, [0x03B2]).						% L&       MATHEMATICAL BOLD ITALIC SMALL BETA
unicode_nfkc_cf(0x1D738, [0x03B3]).						% L&       MATHEMATICAL BOLD ITALIC SMALL GAMMA
unicode_nfkc_cf(0x1D739, [0x03B4]).						% L&       MATHEMATICAL BOLD ITALIC SMALL DELTA
unicode_nfkc_cf(0x1D73A, [0x03B5]).						% L&       MATHEMATICAL BOLD ITALIC SMALL EPSILON
unicode_nfkc_cf(0x1D73B, [0x03B6]).						% L&       MATHEMATICAL BOLD ITALIC SMALL ZETA
unicode_nfkc_cf(0x1D73C, [0x03B7]).						% L&       MATHEMATICAL BOLD ITALIC SMALL ETA
unicode_nfkc_cf(0x1D73D, [0x03B8]).						% L&       MATHEMATICAL BOLD ITALIC SMALL THETA
unicode_nfkc_cf(0x1D73E, [0x03B9]).						% L&       MATHEMATICAL BOLD ITALIC SMALL IOTA
unicode_nfkc_cf(0x1D73F, [0x03BA]).						% L&       MATHEMATICAL BOLD ITALIC SMALL KAPPA
unicode_nfkc_cf(0x1D740, [0x03BB]).						% L&       MATHEMATICAL BOLD ITALIC SMALL LAMDA
unicode_nfkc_cf(0x1D741, [0x03BC]).						% L&       MATHEMATICAL BOLD ITALIC SMALL MU
unicode_nfkc_cf(0x1D742, [0x03BD]).						% L&       MATHEMATICAL BOLD ITALIC SMALL NU
unicode_nfkc_cf(0x1D743, [0x03BE]).						% L&       MATHEMATICAL BOLD ITALIC SMALL XI
unicode_nfkc_cf(0x1D744, [0x03BF]).						% L&       MATHEMATICAL BOLD ITALIC SMALL OMICRON
unicode_nfkc_cf(0x1D745, [0x03C0]).						% L&       MATHEMATICAL BOLD ITALIC SMALL PI
unicode_nfkc_cf(0x1D746, [0x03C1]).						% L&       MATHEMATICAL BOLD ITALIC SMALL RHO
unicode_nfkc_cf(0x1D747, [0x03C3]).						% L&       MATHEMATICAL BOLD ITALIC SMALL FINAL SIGMA
unicode_nfkc_cf(0x1D748, [0x03C3]).						% L&       MATHEMATICAL BOLD ITALIC SMALL SIGMA
unicode_nfkc_cf(0x1D749, [0x03C4]).						% L&       MATHEMATICAL BOLD ITALIC SMALL TAU
unicode_nfkc_cf(0x1D74A, [0x03C5]).						% L&       MATHEMATICAL BOLD ITALIC SMALL UPSILON
unicode_nfkc_cf(0x1D74B, [0x03C6]).						% L&       MATHEMATICAL BOLD ITALIC SMALL PHI
unicode_nfkc_cf(0x1D74C, [0x03C7]).						% L&       MATHEMATICAL BOLD ITALIC SMALL CHI
unicode_nfkc_cf(0x1D74D, [0x03C8]).						% L&       MATHEMATICAL BOLD ITALIC SMALL PSI
unicode_nfkc_cf(0x1D74E, [0x03C9]).						% L&       MATHEMATICAL BOLD ITALIC SMALL OMEGA
unicode_nfkc_cf(0x1D74F, [0x2202]).						% Sm       MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_nfkc_cf(0x1D750, [0x03B5]).						% L&       MATHEMATICAL BOLD ITALIC EPSILON SYMBOL
unicode_nfkc_cf(0x1D751, [0x03B8]).						% L&       MATHEMATICAL BOLD ITALIC THETA SYMBOL
unicode_nfkc_cf(0x1D752, [0x03BA]).						% L&       MATHEMATICAL BOLD ITALIC KAPPA SYMBOL
unicode_nfkc_cf(0x1D753, [0x03C6]).						% L&       MATHEMATICAL BOLD ITALIC PHI SYMBOL
unicode_nfkc_cf(0x1D754, [0x03C1]).						% L&       MATHEMATICAL BOLD ITALIC RHO SYMBOL
unicode_nfkc_cf(0x1D755, [0x03C0]).						% L&       MATHEMATICAL BOLD ITALIC PI SYMBOL
unicode_nfkc_cf(0x1D756, [0x03B1]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL ALPHA
unicode_nfkc_cf(0x1D757, [0x03B2]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL BETA
unicode_nfkc_cf(0x1D758, [0x03B3]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL GAMMA
unicode_nfkc_cf(0x1D759, [0x03B4]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL DELTA
unicode_nfkc_cf(0x1D75A, [0x03B5]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL EPSILON
unicode_nfkc_cf(0x1D75B, [0x03B6]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL ZETA
unicode_nfkc_cf(0x1D75C, [0x03B7]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL ETA
unicode_nfkc_cf(0x1D75D, [0x03B8]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA
unicode_nfkc_cf(0x1D75E, [0x03B9]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL IOTA
unicode_nfkc_cf(0x1D75F, [0x03BA]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL KAPPA
unicode_nfkc_cf(0x1D760, [0x03BB]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL LAMDA
unicode_nfkc_cf(0x1D761, [0x03BC]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL MU
unicode_nfkc_cf(0x1D762, [0x03BD]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL NU
unicode_nfkc_cf(0x1D763, [0x03BE]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL XI
unicode_nfkc_cf(0x1D764, [0x03BF]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL OMICRON
unicode_nfkc_cf(0x1D765, [0x03C0]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL PI
unicode_nfkc_cf(0x1D766, [0x03C1]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL RHO
unicode_nfkc_cf(0x1D767, [0x03B8]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA SYMBOL
unicode_nfkc_cf(0x1D768, [0x03C3]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL SIGMA
unicode_nfkc_cf(0x1D769, [0x03C4]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL TAU
unicode_nfkc_cf(0x1D76A, [0x03C5]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL UPSILON
unicode_nfkc_cf(0x1D76B, [0x03C6]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL PHI
unicode_nfkc_cf(0x1D76C, [0x03C7]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL CHI
unicode_nfkc_cf(0x1D76D, [0x03C8]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL PSI
unicode_nfkc_cf(0x1D76E, [0x03C9]).						% L&       MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_nfkc_cf(0x1D76F, [0x2207]).						% Sm       MATHEMATICAL SANS-SERIF BOLD NABLA
unicode_nfkc_cf(0x1D770, [0x03B1]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA
unicode_nfkc_cf(0x1D771, [0x03B2]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL BETA
unicode_nfkc_cf(0x1D772, [0x03B3]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL GAMMA
unicode_nfkc_cf(0x1D773, [0x03B4]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL DELTA
unicode_nfkc_cf(0x1D774, [0x03B5]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL EPSILON
unicode_nfkc_cf(0x1D775, [0x03B6]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL ZETA
unicode_nfkc_cf(0x1D776, [0x03B7]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL ETA
unicode_nfkc_cf(0x1D777, [0x03B8]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL THETA
unicode_nfkc_cf(0x1D778, [0x03B9]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL IOTA
unicode_nfkc_cf(0x1D779, [0x03BA]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL KAPPA
unicode_nfkc_cf(0x1D77A, [0x03BB]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL LAMDA
unicode_nfkc_cf(0x1D77B, [0x03BC]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL MU
unicode_nfkc_cf(0x1D77C, [0x03BD]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL NU
unicode_nfkc_cf(0x1D77D, [0x03BE]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL XI
unicode_nfkc_cf(0x1D77E, [0x03BF]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL OMICRON
unicode_nfkc_cf(0x1D77F, [0x03C0]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL PI
unicode_nfkc_cf(0x1D780, [0x03C1]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL RHO
unicode_nfkc_cf(0x1D781, [0x03C3]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL FINAL SIGMA
unicode_nfkc_cf(0x1D782, [0x03C3]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL SIGMA
unicode_nfkc_cf(0x1D783, [0x03C4]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL TAU
unicode_nfkc_cf(0x1D784, [0x03C5]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL UPSILON
unicode_nfkc_cf(0x1D785, [0x03C6]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL PHI
unicode_nfkc_cf(0x1D786, [0x03C7]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL CHI
unicode_nfkc_cf(0x1D787, [0x03C8]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL PSI
unicode_nfkc_cf(0x1D788, [0x03C9]).						% L&       MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
unicode_nfkc_cf(0x1D789, [0x2202]).						% Sm       MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL
unicode_nfkc_cf(0x1D78A, [0x03B5]).						% L&       MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL
unicode_nfkc_cf(0x1D78B, [0x03B8]).						% L&       MATHEMATICAL SANS-SERIF BOLD THETA SYMBOL
unicode_nfkc_cf(0x1D78C, [0x03BA]).						% L&       MATHEMATICAL SANS-SERIF BOLD KAPPA SYMBOL
unicode_nfkc_cf(0x1D78D, [0x03C6]).						% L&       MATHEMATICAL SANS-SERIF BOLD PHI SYMBOL
unicode_nfkc_cf(0x1D78E, [0x03C1]).						% L&       MATHEMATICAL SANS-SERIF BOLD RHO SYMBOL
unicode_nfkc_cf(0x1D78F, [0x03C0]).						% L&       MATHEMATICAL SANS-SERIF BOLD PI SYMBOL
unicode_nfkc_cf(0x1D790, [0x03B1]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ALPHA
unicode_nfkc_cf(0x1D791, [0x03B2]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL BETA
unicode_nfkc_cf(0x1D792, [0x03B3]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL GAMMA
unicode_nfkc_cf(0x1D793, [0x03B4]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL DELTA
unicode_nfkc_cf(0x1D794, [0x03B5]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL EPSILON
unicode_nfkc_cf(0x1D795, [0x03B6]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ZETA
unicode_nfkc_cf(0x1D796, [0x03B7]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ETA
unicode_nfkc_cf(0x1D797, [0x03B8]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA
unicode_nfkc_cf(0x1D798, [0x03B9]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL IOTA
unicode_nfkc_cf(0x1D799, [0x03BA]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL KAPPA
unicode_nfkc_cf(0x1D79A, [0x03BB]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL LAMDA
unicode_nfkc_cf(0x1D79B, [0x03BC]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL MU
unicode_nfkc_cf(0x1D79C, [0x03BD]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL NU
unicode_nfkc_cf(0x1D79D, [0x03BE]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL XI
unicode_nfkc_cf(0x1D79E, [0x03BF]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMICRON
unicode_nfkc_cf(0x1D79F, [0x03C0]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PI
unicode_nfkc_cf(0x1D7A0, [0x03C1]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL RHO
unicode_nfkc_cf(0x1D7A1, [0x03B8]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA SYMBOL
unicode_nfkc_cf(0x1D7A2, [0x03C3]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL SIGMA
unicode_nfkc_cf(0x1D7A3, [0x03C4]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL TAU
unicode_nfkc_cf(0x1D7A4, [0x03C5]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL UPSILON
unicode_nfkc_cf(0x1D7A5, [0x03C6]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PHI
unicode_nfkc_cf(0x1D7A6, [0x03C7]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL CHI
unicode_nfkc_cf(0x1D7A7, [0x03C8]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PSI
unicode_nfkc_cf(0x1D7A8, [0x03C9]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_nfkc_cf(0x1D7A9, [0x2207]).						% Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA
unicode_nfkc_cf(0x1D7AA, [0x03B1]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA
unicode_nfkc_cf(0x1D7AB, [0x03B2]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL BETA
unicode_nfkc_cf(0x1D7AC, [0x03B3]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL GAMMA
unicode_nfkc_cf(0x1D7AD, [0x03B4]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL DELTA
unicode_nfkc_cf(0x1D7AE, [0x03B5]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL EPSILON
unicode_nfkc_cf(0x1D7AF, [0x03B6]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ZETA
unicode_nfkc_cf(0x1D7B0, [0x03B7]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ETA
unicode_nfkc_cf(0x1D7B1, [0x03B8]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL THETA
unicode_nfkc_cf(0x1D7B2, [0x03B9]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL IOTA
unicode_nfkc_cf(0x1D7B3, [0x03BA]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL KAPPA
unicode_nfkc_cf(0x1D7B4, [0x03BB]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL LAMDA
unicode_nfkc_cf(0x1D7B5, [0x03BC]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL MU
unicode_nfkc_cf(0x1D7B6, [0x03BD]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL NU
unicode_nfkc_cf(0x1D7B7, [0x03BE]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL XI
unicode_nfkc_cf(0x1D7B8, [0x03BF]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMICRON
unicode_nfkc_cf(0x1D7B9, [0x03C0]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PI
unicode_nfkc_cf(0x1D7BA, [0x03C1]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL RHO
unicode_nfkc_cf(0x1D7BB, [0x03C3]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL FINAL SIGMA
unicode_nfkc_cf(0x1D7BC, [0x03C3]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL SIGMA
unicode_nfkc_cf(0x1D7BD, [0x03C4]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL TAU
unicode_nfkc_cf(0x1D7BE, [0x03C5]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL UPSILON
unicode_nfkc_cf(0x1D7BF, [0x03C6]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PHI
unicode_nfkc_cf(0x1D7C0, [0x03C7]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL CHI
unicode_nfkc_cf(0x1D7C1, [0x03C8]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PSI
unicode_nfkc_cf(0x1D7C2, [0x03C9]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
unicode_nfkc_cf(0x1D7C3, [0x2202]).						% Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_nfkc_cf(0x1D7C4, [0x03B5]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL
unicode_nfkc_cf(0x1D7C5, [0x03B8]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC THETA SYMBOL
unicode_nfkc_cf(0x1D7C6, [0x03BA]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC KAPPA SYMBOL
unicode_nfkc_cf(0x1D7C7, [0x03C6]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC PHI SYMBOL
unicode_nfkc_cf(0x1D7C8, [0x03C1]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC RHO SYMBOL
unicode_nfkc_cf(0x1D7C9, [0x03C0]).						% L&       MATHEMATICAL SANS-SERIF BOLD ITALIC PI SYMBOL
unicode_nfkc_cf(0x1D7CA, [0x03DD]).						% L&       MATHEMATICAL BOLD CAPITAL DIGAMMA
unicode_nfkc_cf(0x1D7CB, [0x03DD]).						% L&       MATHEMATICAL BOLD SMALL DIGAMMA
unicode_nfkc_cf(0x1D7CE, [0x0030]).						% Nd       MATHEMATICAL BOLD DIGIT ZERO
unicode_nfkc_cf(0x1D7CF, [0x0031]).						% Nd       MATHEMATICAL BOLD DIGIT ONE
unicode_nfkc_cf(0x1D7D0, [0x0032]).						% Nd       MATHEMATICAL BOLD DIGIT TWO
unicode_nfkc_cf(0x1D7D1, [0x0033]).						% Nd       MATHEMATICAL BOLD DIGIT THREE
unicode_nfkc_cf(0x1D7D2, [0x0034]).						% Nd       MATHEMATICAL BOLD DIGIT FOUR
unicode_nfkc_cf(0x1D7D3, [0x0035]).						% Nd       MATHEMATICAL BOLD DIGIT FIVE
unicode_nfkc_cf(0x1D7D4, [0x0036]).						% Nd       MATHEMATICAL BOLD DIGIT SIX
unicode_nfkc_cf(0x1D7D5, [0x0037]).						% Nd       MATHEMATICAL BOLD DIGIT SEVEN
unicode_nfkc_cf(0x1D7D6, [0x0038]).						% Nd       MATHEMATICAL BOLD DIGIT EIGHT
unicode_nfkc_cf(0x1D7D7, [0x0039]).						% Nd       MATHEMATICAL BOLD DIGIT NINE
unicode_nfkc_cf(0x1D7D8, [0x0030]).						% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO
unicode_nfkc_cf(0x1D7D9, [0x0031]).						% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT ONE
unicode_nfkc_cf(0x1D7DA, [0x0032]).						% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT TWO
unicode_nfkc_cf(0x1D7DB, [0x0033]).						% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT THREE
unicode_nfkc_cf(0x1D7DC, [0x0034]).						% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT FOUR
unicode_nfkc_cf(0x1D7DD, [0x0035]).						% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT FIVE
unicode_nfkc_cf(0x1D7DE, [0x0036]).						% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT SIX
unicode_nfkc_cf(0x1D7DF, [0x0037]).						% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT SEVEN
unicode_nfkc_cf(0x1D7E0, [0x0038]).						% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT EIGHT
unicode_nfkc_cf(0x1D7E1, [0x0039]).						% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT NINE
unicode_nfkc_cf(0x1D7E2, [0x0030]).						% Nd       MATHEMATICAL SANS-SERIF DIGIT ZERO
unicode_nfkc_cf(0x1D7E3, [0x0031]).						% Nd       MATHEMATICAL SANS-SERIF DIGIT ONE
unicode_nfkc_cf(0x1D7E4, [0x0032]).						% Nd       MATHEMATICAL SANS-SERIF DIGIT TWO
unicode_nfkc_cf(0x1D7E5, [0x0033]).						% Nd       MATHEMATICAL SANS-SERIF DIGIT THREE
unicode_nfkc_cf(0x1D7E6, [0x0034]).						% Nd       MATHEMATICAL SANS-SERIF DIGIT FOUR
unicode_nfkc_cf(0x1D7E7, [0x0035]).						% Nd       MATHEMATICAL SANS-SERIF DIGIT FIVE
unicode_nfkc_cf(0x1D7E8, [0x0036]).						% Nd       MATHEMATICAL SANS-SERIF DIGIT SIX
unicode_nfkc_cf(0x1D7E9, [0x0037]).						% Nd       MATHEMATICAL SANS-SERIF DIGIT SEVEN
unicode_nfkc_cf(0x1D7EA, [0x0038]).						% Nd       MATHEMATICAL SANS-SERIF DIGIT EIGHT
unicode_nfkc_cf(0x1D7EB, [0x0039]).						% Nd       MATHEMATICAL SANS-SERIF DIGIT NINE
unicode_nfkc_cf(0x1D7EC, [0x0030]).						% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT ZERO
unicode_nfkc_cf(0x1D7ED, [0x0031]).						% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT ONE
unicode_nfkc_cf(0x1D7EE, [0x0032]).						% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT TWO
unicode_nfkc_cf(0x1D7EF, [0x0033]).						% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT THREE
unicode_nfkc_cf(0x1D7F0, [0x0034]).						% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT FOUR
unicode_nfkc_cf(0x1D7F1, [0x0035]).						% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT FIVE
unicode_nfkc_cf(0x1D7F2, [0x0036]).						% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT SIX
unicode_nfkc_cf(0x1D7F3, [0x0037]).						% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT SEVEN
unicode_nfkc_cf(0x1D7F4, [0x0038]).						% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT EIGHT
unicode_nfkc_cf(0x1D7F5, [0x0039]).						% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT NINE
unicode_nfkc_cf(0x1D7F6, [0x0030]).						% Nd       MATHEMATICAL MONOSPACE DIGIT ZERO
unicode_nfkc_cf(0x1D7F7, [0x0031]).						% Nd       MATHEMATICAL MONOSPACE DIGIT ONE
unicode_nfkc_cf(0x1D7F8, [0x0032]).						% Nd       MATHEMATICAL MONOSPACE DIGIT TWO
unicode_nfkc_cf(0x1D7F9, [0x0033]).						% Nd       MATHEMATICAL MONOSPACE DIGIT THREE
unicode_nfkc_cf(0x1D7FA, [0x0034]).						% Nd       MATHEMATICAL MONOSPACE DIGIT FOUR
unicode_nfkc_cf(0x1D7FB, [0x0035]).						% Nd       MATHEMATICAL MONOSPACE DIGIT FIVE
unicode_nfkc_cf(0x1D7FC, [0x0036]).						% Nd       MATHEMATICAL MONOSPACE DIGIT SIX
unicode_nfkc_cf(0x1D7FD, [0x0037]).						% Nd       MATHEMATICAL MONOSPACE DIGIT SEVEN
unicode_nfkc_cf(0x1D7FE, [0x0038]).						% Nd       MATHEMATICAL MONOSPACE DIGIT EIGHT
unicode_nfkc_cf(0x1D7FF, [0x0039]).						% Nd       MATHEMATICAL MONOSPACE DIGIT NINE
unicode_nfkc_cf(0x1EE00, [0x0627]).						% Lo       ARABIC MATHEMATICAL ALEF
unicode_nfkc_cf(0x1EE01, [0x0628]).						% Lo       ARABIC MATHEMATICAL BEH
unicode_nfkc_cf(0x1EE02, [0x062C]).						% Lo       ARABIC MATHEMATICAL JEEM
unicode_nfkc_cf(0x1EE03, [0x062F]).						% Lo       ARABIC MATHEMATICAL DAL
unicode_nfkc_cf(0x1EE05, [0x0648]).						% Lo       ARABIC MATHEMATICAL WAW
unicode_nfkc_cf(0x1EE06, [0x0632]).						% Lo       ARABIC MATHEMATICAL ZAIN
unicode_nfkc_cf(0x1EE07, [0x062D]).						% Lo       ARABIC MATHEMATICAL HAH
unicode_nfkc_cf(0x1EE08, [0x0637]).						% Lo       ARABIC MATHEMATICAL TAH
unicode_nfkc_cf(0x1EE09, [0x064A]).						% Lo       ARABIC MATHEMATICAL YEH
unicode_nfkc_cf(0x1EE0A, [0x0643]).						% Lo       ARABIC MATHEMATICAL KAF
unicode_nfkc_cf(0x1EE0B, [0x0644]).						% Lo       ARABIC MATHEMATICAL LAM
unicode_nfkc_cf(0x1EE0C, [0x0645]).						% Lo       ARABIC MATHEMATICAL MEEM
unicode_nfkc_cf(0x1EE0D, [0x0646]).						% Lo       ARABIC MATHEMATICAL NOON
unicode_nfkc_cf(0x1EE0E, [0x0633]).						% Lo       ARABIC MATHEMATICAL SEEN
unicode_nfkc_cf(0x1EE0F, [0x0639]).						% Lo       ARABIC MATHEMATICAL AIN
unicode_nfkc_cf(0x1EE10, [0x0641]).						% Lo       ARABIC MATHEMATICAL FEH
unicode_nfkc_cf(0x1EE11, [0x0635]).						% Lo       ARABIC MATHEMATICAL SAD
unicode_nfkc_cf(0x1EE12, [0x0642]).						% Lo       ARABIC MATHEMATICAL QAF
unicode_nfkc_cf(0x1EE13, [0x0631]).						% Lo       ARABIC MATHEMATICAL REH
unicode_nfkc_cf(0x1EE14, [0x0634]).						% Lo       ARABIC MATHEMATICAL SHEEN
unicode_nfkc_cf(0x1EE15, [0x062A]).						% Lo       ARABIC MATHEMATICAL TEH
unicode_nfkc_cf(0x1EE16, [0x062B]).						% Lo       ARABIC MATHEMATICAL THEH
unicode_nfkc_cf(0x1EE17, [0x062E]).						% Lo       ARABIC MATHEMATICAL KHAH
unicode_nfkc_cf(0x1EE18, [0x0630]).						% Lo       ARABIC MATHEMATICAL THAL
unicode_nfkc_cf(0x1EE19, [0x0636]).						% Lo       ARABIC MATHEMATICAL DAD
unicode_nfkc_cf(0x1EE1A, [0x0638]).						% Lo       ARABIC MATHEMATICAL ZAH
unicode_nfkc_cf(0x1EE1B, [0x063A]).						% Lo       ARABIC MATHEMATICAL GHAIN
unicode_nfkc_cf(0x1EE1C, [0x066E]).						% Lo       ARABIC MATHEMATICAL DOTLESS BEH
unicode_nfkc_cf(0x1EE1D, [0x06BA]).						% Lo       ARABIC MATHEMATICAL DOTLESS NOON
unicode_nfkc_cf(0x1EE1E, [0x06A1]).						% Lo       ARABIC MATHEMATICAL DOTLESS FEH
unicode_nfkc_cf(0x1EE1F, [0x066F]).						% Lo       ARABIC MATHEMATICAL DOTLESS QAF
unicode_nfkc_cf(0x1EE21, [0x0628]).						% Lo       ARABIC MATHEMATICAL INITIAL BEH
unicode_nfkc_cf(0x1EE22, [0x062C]).						% Lo       ARABIC MATHEMATICAL INITIAL JEEM
unicode_nfkc_cf(0x1EE24, [0x0647]).						% Lo       ARABIC MATHEMATICAL INITIAL HEH
unicode_nfkc_cf(0x1EE27, [0x062D]).						% Lo       ARABIC MATHEMATICAL INITIAL HAH
unicode_nfkc_cf(0x1EE29, [0x064A]).						% Lo       ARABIC MATHEMATICAL INITIAL YEH
unicode_nfkc_cf(0x1EE2A, [0x0643]).						% Lo       ARABIC MATHEMATICAL INITIAL KAF
unicode_nfkc_cf(0x1EE2B, [0x0644]).						% Lo       ARABIC MATHEMATICAL INITIAL LAM
unicode_nfkc_cf(0x1EE2C, [0x0645]).						% Lo       ARABIC MATHEMATICAL INITIAL MEEM
unicode_nfkc_cf(0x1EE2D, [0x0646]).						% Lo       ARABIC MATHEMATICAL INITIAL NOON
unicode_nfkc_cf(0x1EE2E, [0x0633]).						% Lo       ARABIC MATHEMATICAL INITIAL SEEN
unicode_nfkc_cf(0x1EE2F, [0x0639]).						% Lo       ARABIC MATHEMATICAL INITIAL AIN
unicode_nfkc_cf(0x1EE30, [0x0641]).						% Lo       ARABIC MATHEMATICAL INITIAL FEH
unicode_nfkc_cf(0x1EE31, [0x0635]).						% Lo       ARABIC MATHEMATICAL INITIAL SAD
unicode_nfkc_cf(0x1EE32, [0x0642]).						% Lo       ARABIC MATHEMATICAL INITIAL QAF
unicode_nfkc_cf(0x1EE34, [0x0634]).						% Lo       ARABIC MATHEMATICAL INITIAL SHEEN
unicode_nfkc_cf(0x1EE35, [0x062A]).						% Lo       ARABIC MATHEMATICAL INITIAL TEH
unicode_nfkc_cf(0x1EE36, [0x062B]).						% Lo       ARABIC MATHEMATICAL INITIAL THEH
unicode_nfkc_cf(0x1EE37, [0x062E]).						% Lo       ARABIC MATHEMATICAL INITIAL KHAH
unicode_nfkc_cf(0x1EE39, [0x0636]).						% Lo       ARABIC MATHEMATICAL INITIAL DAD
unicode_nfkc_cf(0x1EE3B, [0x063A]).						% Lo       ARABIC MATHEMATICAL INITIAL GHAIN
unicode_nfkc_cf(0x1EE42, [0x062C]).						% Lo       ARABIC MATHEMATICAL TAILED JEEM
unicode_nfkc_cf(0x1EE47, [0x062D]).						% Lo       ARABIC MATHEMATICAL TAILED HAH
unicode_nfkc_cf(0x1EE49, [0x064A]).						% Lo       ARABIC MATHEMATICAL TAILED YEH
unicode_nfkc_cf(0x1EE4B, [0x0644]).						% Lo       ARABIC MATHEMATICAL TAILED LAM
unicode_nfkc_cf(0x1EE4D, [0x0646]).						% Lo       ARABIC MATHEMATICAL TAILED NOON
unicode_nfkc_cf(0x1EE4E, [0x0633]).						% Lo       ARABIC MATHEMATICAL TAILED SEEN
unicode_nfkc_cf(0x1EE4F, [0x0639]).						% Lo       ARABIC MATHEMATICAL TAILED AIN
unicode_nfkc_cf(0x1EE51, [0x0635]).						% Lo       ARABIC MATHEMATICAL TAILED SAD
unicode_nfkc_cf(0x1EE52, [0x0642]).						% Lo       ARABIC MATHEMATICAL TAILED QAF
unicode_nfkc_cf(0x1EE54, [0x0634]).						% Lo       ARABIC MATHEMATICAL TAILED SHEEN
unicode_nfkc_cf(0x1EE57, [0x062E]).						% Lo       ARABIC MATHEMATICAL TAILED KHAH
unicode_nfkc_cf(0x1EE59, [0x0636]).						% Lo       ARABIC MATHEMATICAL TAILED DAD
unicode_nfkc_cf(0x1EE5B, [0x063A]).						% Lo       ARABIC MATHEMATICAL TAILED GHAIN
unicode_nfkc_cf(0x1EE5D, [0x06BA]).						% Lo       ARABIC MATHEMATICAL TAILED DOTLESS NOON
unicode_nfkc_cf(0x1EE5F, [0x066F]).						% Lo       ARABIC MATHEMATICAL TAILED DOTLESS QAF
unicode_nfkc_cf(0x1EE61, [0x0628]).						% Lo       ARABIC MATHEMATICAL STRETCHED BEH
unicode_nfkc_cf(0x1EE62, [0x062C]).						% Lo       ARABIC MATHEMATICAL STRETCHED JEEM
unicode_nfkc_cf(0x1EE64, [0x0647]).						% Lo       ARABIC MATHEMATICAL STRETCHED HEH
unicode_nfkc_cf(0x1EE67, [0x062D]).						% Lo       ARABIC MATHEMATICAL STRETCHED HAH
unicode_nfkc_cf(0x1EE68, [0x0637]).						% Lo       ARABIC MATHEMATICAL STRETCHED TAH
unicode_nfkc_cf(0x1EE69, [0x064A]).						% Lo       ARABIC MATHEMATICAL STRETCHED YEH
unicode_nfkc_cf(0x1EE6A, [0x0643]).						% Lo       ARABIC MATHEMATICAL STRETCHED KAF
unicode_nfkc_cf(0x1EE6C, [0x0645]).						% Lo       ARABIC MATHEMATICAL STRETCHED MEEM
unicode_nfkc_cf(0x1EE6D, [0x0646]).						% Lo       ARABIC MATHEMATICAL STRETCHED NOON
unicode_nfkc_cf(0x1EE6E, [0x0633]).						% Lo       ARABIC MATHEMATICAL STRETCHED SEEN
unicode_nfkc_cf(0x1EE6F, [0x0639]).						% Lo       ARABIC MATHEMATICAL STRETCHED AIN
unicode_nfkc_cf(0x1EE70, [0x0641]).						% Lo       ARABIC MATHEMATICAL STRETCHED FEH
unicode_nfkc_cf(0x1EE71, [0x0635]).						% Lo       ARABIC MATHEMATICAL STRETCHED SAD
unicode_nfkc_cf(0x1EE72, [0x0642]).						% Lo       ARABIC MATHEMATICAL STRETCHED QAF
unicode_nfkc_cf(0x1EE74, [0x0634]).						% Lo       ARABIC MATHEMATICAL STRETCHED SHEEN
unicode_nfkc_cf(0x1EE75, [0x062A]).						% Lo       ARABIC MATHEMATICAL STRETCHED TEH
unicode_nfkc_cf(0x1EE76, [0x062B]).						% Lo       ARABIC MATHEMATICAL STRETCHED THEH
unicode_nfkc_cf(0x1EE77, [0x062E]).						% Lo       ARABIC MATHEMATICAL STRETCHED KHAH
unicode_nfkc_cf(0x1EE79, [0x0636]).						% Lo       ARABIC MATHEMATICAL STRETCHED DAD
unicode_nfkc_cf(0x1EE7A, [0x0638]).						% Lo       ARABIC MATHEMATICAL STRETCHED ZAH
unicode_nfkc_cf(0x1EE7B, [0x063A]).						% Lo       ARABIC MATHEMATICAL STRETCHED GHAIN
unicode_nfkc_cf(0x1EE7C, [0x066E]).						% Lo       ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
unicode_nfkc_cf(0x1EE7E, [0x06A1]).						% Lo       ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
unicode_nfkc_cf(0x1EE80, [0x0627]).						% Lo       ARABIC MATHEMATICAL LOOPED ALEF
unicode_nfkc_cf(0x1EE81, [0x0628]).						% Lo       ARABIC MATHEMATICAL LOOPED BEH
unicode_nfkc_cf(0x1EE82, [0x062C]).						% Lo       ARABIC MATHEMATICAL LOOPED JEEM
unicode_nfkc_cf(0x1EE83, [0x062F]).						% Lo       ARABIC MATHEMATICAL LOOPED DAL
unicode_nfkc_cf(0x1EE84, [0x0647]).						% Lo       ARABIC MATHEMATICAL LOOPED HEH
unicode_nfkc_cf(0x1EE85, [0x0648]).						% Lo       ARABIC MATHEMATICAL LOOPED WAW
unicode_nfkc_cf(0x1EE86, [0x0632]).						% Lo       ARABIC MATHEMATICAL LOOPED ZAIN
unicode_nfkc_cf(0x1EE87, [0x062D]).						% Lo       ARABIC MATHEMATICAL LOOPED HAH
unicode_nfkc_cf(0x1EE88, [0x0637]).						% Lo       ARABIC MATHEMATICAL LOOPED TAH
unicode_nfkc_cf(0x1EE89, [0x064A]).						% Lo       ARABIC MATHEMATICAL LOOPED YEH
unicode_nfkc_cf(0x1EE8B, [0x0644]).						% Lo       ARABIC MATHEMATICAL LOOPED LAM
unicode_nfkc_cf(0x1EE8C, [0x0645]).						% Lo       ARABIC MATHEMATICAL LOOPED MEEM
unicode_nfkc_cf(0x1EE8D, [0x0646]).						% Lo       ARABIC MATHEMATICAL LOOPED NOON
unicode_nfkc_cf(0x1EE8E, [0x0633]).						% Lo       ARABIC MATHEMATICAL LOOPED SEEN
unicode_nfkc_cf(0x1EE8F, [0x0639]).						% Lo       ARABIC MATHEMATICAL LOOPED AIN
unicode_nfkc_cf(0x1EE90, [0x0641]).						% Lo       ARABIC MATHEMATICAL LOOPED FEH
unicode_nfkc_cf(0x1EE91, [0x0635]).						% Lo       ARABIC MATHEMATICAL LOOPED SAD
unicode_nfkc_cf(0x1EE92, [0x0642]).						% Lo       ARABIC MATHEMATICAL LOOPED QAF
unicode_nfkc_cf(0x1EE93, [0x0631]).						% Lo       ARABIC MATHEMATICAL LOOPED REH
unicode_nfkc_cf(0x1EE94, [0x0634]).						% Lo       ARABIC MATHEMATICAL LOOPED SHEEN
unicode_nfkc_cf(0x1EE95, [0x062A]).						% Lo       ARABIC MATHEMATICAL LOOPED TEH
unicode_nfkc_cf(0x1EE96, [0x062B]).						% Lo       ARABIC MATHEMATICAL LOOPED THEH
unicode_nfkc_cf(0x1EE97, [0x062E]).						% Lo       ARABIC MATHEMATICAL LOOPED KHAH
unicode_nfkc_cf(0x1EE98, [0x0630]).						% Lo       ARABIC MATHEMATICAL LOOPED THAL
unicode_nfkc_cf(0x1EE99, [0x0636]).						% Lo       ARABIC MATHEMATICAL LOOPED DAD
unicode_nfkc_cf(0x1EE9A, [0x0638]).						% Lo       ARABIC MATHEMATICAL LOOPED ZAH
unicode_nfkc_cf(0x1EE9B, [0x063A]).						% Lo       ARABIC MATHEMATICAL LOOPED GHAIN
unicode_nfkc_cf(0x1EEA1, [0x0628]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK BEH
unicode_nfkc_cf(0x1EEA2, [0x062C]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK JEEM
unicode_nfkc_cf(0x1EEA3, [0x062F]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
unicode_nfkc_cf(0x1EEA5, [0x0648]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK WAW
unicode_nfkc_cf(0x1EEA6, [0x0632]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK ZAIN
unicode_nfkc_cf(0x1EEA7, [0x062D]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK HAH
unicode_nfkc_cf(0x1EEA8, [0x0637]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK TAH
unicode_nfkc_cf(0x1EEA9, [0x064A]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
unicode_nfkc_cf(0x1EEAB, [0x0644]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK LAM
unicode_nfkc_cf(0x1EEAC, [0x0645]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK MEEM
unicode_nfkc_cf(0x1EEAD, [0x0646]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK NOON
unicode_nfkc_cf(0x1EEAE, [0x0633]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK SEEN
unicode_nfkc_cf(0x1EEAF, [0x0639]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK AIN
unicode_nfkc_cf(0x1EEB0, [0x0641]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK FEH
unicode_nfkc_cf(0x1EEB1, [0x0635]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK SAD
unicode_nfkc_cf(0x1EEB2, [0x0642]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK QAF
unicode_nfkc_cf(0x1EEB3, [0x0631]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK REH
unicode_nfkc_cf(0x1EEB4, [0x0634]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK SHEEN
unicode_nfkc_cf(0x1EEB5, [0x062A]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK TEH
unicode_nfkc_cf(0x1EEB6, [0x062B]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK THEH
unicode_nfkc_cf(0x1EEB7, [0x062E]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK KHAH
unicode_nfkc_cf(0x1EEB8, [0x0630]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK THAL
unicode_nfkc_cf(0x1EEB9, [0x0636]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK DAD
unicode_nfkc_cf(0x1EEBA, [0x0638]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK ZAH
unicode_nfkc_cf(0x1EEBB, [0x063A]).						% Lo       ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN
unicode_nfkc_cf(0x1F100, [0x0030, 0x002E]).				% No       DIGIT ZERO FULL STOP
unicode_nfkc_cf(0x1F101, [0x0030, 0x002C]).				% No       DIGIT ZERO COMMA
unicode_nfkc_cf(0x1F102, [0x0031, 0x002C]).				% No       DIGIT ONE COMMA
unicode_nfkc_cf(0x1F103, [0x0032, 0x002C]).				% No       DIGIT TWO COMMA
unicode_nfkc_cf(0x1F104, [0x0033, 0x002C]).				% No       DIGIT THREE COMMA
unicode_nfkc_cf(0x1F105, [0x0034, 0x002C]).				% No       DIGIT FOUR COMMA
unicode_nfkc_cf(0x1F106, [0x0035, 0x002C]).				% No       DIGIT FIVE COMMA
unicode_nfkc_cf(0x1F107, [0x0036, 0x002C]).				% No       DIGIT SIX COMMA
unicode_nfkc_cf(0x1F108, [0x0037, 0x002C]).				% No       DIGIT SEVEN COMMA
unicode_nfkc_cf(0x1F109, [0x0038, 0x002C]).				% No       DIGIT EIGHT COMMA
unicode_nfkc_cf(0x1F10A, [0x0039, 0x002C]).				% No       DIGIT NINE COMMA
unicode_nfkc_cf(0x1F110, [0x0028, 0x0061, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER A
unicode_nfkc_cf(0x1F111, [0x0028, 0x0062, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER B
unicode_nfkc_cf(0x1F112, [0x0028, 0x0063, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER C
unicode_nfkc_cf(0x1F113, [0x0028, 0x0064, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER D
unicode_nfkc_cf(0x1F114, [0x0028, 0x0065, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER E
unicode_nfkc_cf(0x1F115, [0x0028, 0x0066, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER F
unicode_nfkc_cf(0x1F116, [0x0028, 0x0067, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER G
unicode_nfkc_cf(0x1F117, [0x0028, 0x0068, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER H
unicode_nfkc_cf(0x1F118, [0x0028, 0x0069, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER I
unicode_nfkc_cf(0x1F119, [0x0028, 0x006A, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER J
unicode_nfkc_cf(0x1F11A, [0x0028, 0x006B, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER K
unicode_nfkc_cf(0x1F11B, [0x0028, 0x006C, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER L
unicode_nfkc_cf(0x1F11C, [0x0028, 0x006D, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER M
unicode_nfkc_cf(0x1F11D, [0x0028, 0x006E, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER N
unicode_nfkc_cf(0x1F11E, [0x0028, 0x006F, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER O
unicode_nfkc_cf(0x1F11F, [0x0028, 0x0070, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER P
unicode_nfkc_cf(0x1F120, [0x0028, 0x0071, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER Q
unicode_nfkc_cf(0x1F121, [0x0028, 0x0072, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER R
unicode_nfkc_cf(0x1F122, [0x0028, 0x0073, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER S
unicode_nfkc_cf(0x1F123, [0x0028, 0x0074, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER T
unicode_nfkc_cf(0x1F124, [0x0028, 0x0075, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER U
unicode_nfkc_cf(0x1F125, [0x0028, 0x0076, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER V
unicode_nfkc_cf(0x1F126, [0x0028, 0x0077, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER W
unicode_nfkc_cf(0x1F127, [0x0028, 0x0078, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER X
unicode_nfkc_cf(0x1F128, [0x0028, 0x0079, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER Y
unicode_nfkc_cf(0x1F129, [0x0028, 0x007A, 0x0029]).		% So       PARENTHESIZED LATIN CAPITAL LETTER Z
unicode_nfkc_cf(0x1F12A, [0x3014, 0x0073, 0x3015]).		% So       TORTOISE SHELL BRACKETED LATIN CAPITAL LETTER S
unicode_nfkc_cf(0x1F12B, [0x0063]).						% So       CIRCLED ITALIC LATIN CAPITAL LETTER C
unicode_nfkc_cf(0x1F12C, [0x0072]).						% So       CIRCLED ITALIC LATIN CAPITAL LETTER R
unicode_nfkc_cf(0x1F12D, [0x0063, 0x0064]).				% So       CIRCLED CD
unicode_nfkc_cf(0x1F12E, [0x0077, 0x007A]).				% So       CIRCLED WZ
unicode_nfkc_cf(0x1F130, [0x0061]).						% So       SQUARED LATIN CAPITAL LETTER A
unicode_nfkc_cf(0x1F131, [0x0062]).						% So       SQUARED LATIN CAPITAL LETTER B
unicode_nfkc_cf(0x1F132, [0x0063]).						% So       SQUARED LATIN CAPITAL LETTER C
unicode_nfkc_cf(0x1F133, [0x0064]).						% So       SQUARED LATIN CAPITAL LETTER D
unicode_nfkc_cf(0x1F134, [0x0065]).						% So       SQUARED LATIN CAPITAL LETTER E
unicode_nfkc_cf(0x1F135, [0x0066]).						% So       SQUARED LATIN CAPITAL LETTER F
unicode_nfkc_cf(0x1F136, [0x0067]).						% So       SQUARED LATIN CAPITAL LETTER G
unicode_nfkc_cf(0x1F137, [0x0068]).						% So       SQUARED LATIN CAPITAL LETTER H
unicode_nfkc_cf(0x1F138, [0x0069]).						% So       SQUARED LATIN CAPITAL LETTER I
unicode_nfkc_cf(0x1F139, [0x006A]).						% So       SQUARED LATIN CAPITAL LETTER J
unicode_nfkc_cf(0x1F13A, [0x006B]).						% So       SQUARED LATIN CAPITAL LETTER K
unicode_nfkc_cf(0x1F13B, [0x006C]).						% So       SQUARED LATIN CAPITAL LETTER L
unicode_nfkc_cf(0x1F13C, [0x006D]).						% So       SQUARED LATIN CAPITAL LETTER M
unicode_nfkc_cf(0x1F13D, [0x006E]).						% So       SQUARED LATIN CAPITAL LETTER N
unicode_nfkc_cf(0x1F13E, [0x006F]).						% So       SQUARED LATIN CAPITAL LETTER O
unicode_nfkc_cf(0x1F13F, [0x0070]).						% So       SQUARED LATIN CAPITAL LETTER P
unicode_nfkc_cf(0x1F140, [0x0071]).						% So       SQUARED LATIN CAPITAL LETTER Q
unicode_nfkc_cf(0x1F141, [0x0072]).						% So       SQUARED LATIN CAPITAL LETTER R
unicode_nfkc_cf(0x1F142, [0x0073]).						% So       SQUARED LATIN CAPITAL LETTER S
unicode_nfkc_cf(0x1F143, [0x0074]).						% So       SQUARED LATIN CAPITAL LETTER T
unicode_nfkc_cf(0x1F144, [0x0075]).						% So       SQUARED LATIN CAPITAL LETTER U
unicode_nfkc_cf(0x1F145, [0x0076]).						% So       SQUARED LATIN CAPITAL LETTER V
unicode_nfkc_cf(0x1F146, [0x0077]).						% So       SQUARED LATIN CAPITAL LETTER W
unicode_nfkc_cf(0x1F147, [0x0078]).						% So       SQUARED LATIN CAPITAL LETTER X
unicode_nfkc_cf(0x1F148, [0x0079]).						% So       SQUARED LATIN CAPITAL LETTER Y
unicode_nfkc_cf(0x1F149, [0x007A]).						% So       SQUARED LATIN CAPITAL LETTER Z
unicode_nfkc_cf(0x1F14A, [0x0068, 0x0076]).				% So       SQUARED HV
unicode_nfkc_cf(0x1F14B, [0x006D, 0x0076]).				% So       SQUARED MV
unicode_nfkc_cf(0x1F14C, [0x0073, 0x0064]).				% So       SQUARED SD
unicode_nfkc_cf(0x1F14D, [0x0073, 0x0073]).				% So       SQUARED SS
unicode_nfkc_cf(0x1F14E, [0x0070, 0x0070, 0x0076]).		% So       SQUARED PPV
unicode_nfkc_cf(0x1F14F, [0x0077, 0x0063]).				% So       SQUARED WC
unicode_nfkc_cf(0x1F16A, [0x006D, 0x0063]).				% So       RAISED MC SIGN
unicode_nfkc_cf(0x1F16B, [0x006D, 0x0064]).				% So       RAISED MD SIGN
unicode_nfkc_cf(0x1F190, [0x0064, 0x006A]).				% So       SQUARE DJ
unicode_nfkc_cf(0x1F200, [0x307B, 0x304B]).				% So       SQUARE HIRAGANA HOKA
unicode_nfkc_cf(0x1F201, [0x30B3, 0x30B3]).				% So       SQUARED KATAKANA KOKO
unicode_nfkc_cf(0x1F202, [0x30B5]).						% So       SQUARED KATAKANA SA
unicode_nfkc_cf(0x1F210, [0x624B]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-624B
unicode_nfkc_cf(0x1F211, [0x5B57]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-5B57
unicode_nfkc_cf(0x1F212, [0x53CC]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-53CC
unicode_nfkc_cf(0x1F213, [0x30C7]).						% So       SQUARED KATAKANA DE
unicode_nfkc_cf(0x1F214, [0x4E8C]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-4E8C
unicode_nfkc_cf(0x1F215, [0x591A]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-591A
unicode_nfkc_cf(0x1F216, [0x89E3]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-89E3
unicode_nfkc_cf(0x1F217, [0x5929]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-5929
unicode_nfkc_cf(0x1F218, [0x4EA4]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-4EA4
unicode_nfkc_cf(0x1F219, [0x6620]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-6620
unicode_nfkc_cf(0x1F21A, [0x7121]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-7121
unicode_nfkc_cf(0x1F21B, [0x6599]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-6599
unicode_nfkc_cf(0x1F21C, [0x524D]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-524D
unicode_nfkc_cf(0x1F21D, [0x5F8C]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-5F8C
unicode_nfkc_cf(0x1F21E, [0x518D]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-518D
unicode_nfkc_cf(0x1F21F, [0x65B0]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-65B0
unicode_nfkc_cf(0x1F220, [0x521D]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-521D
unicode_nfkc_cf(0x1F221, [0x7D42]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-7D42
unicode_nfkc_cf(0x1F222, [0x751F]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-751F
unicode_nfkc_cf(0x1F223, [0x8CA9]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-8CA9
unicode_nfkc_cf(0x1F224, [0x58F0]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-58F0
unicode_nfkc_cf(0x1F225, [0x5439]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-5439
unicode_nfkc_cf(0x1F226, [0x6F14]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-6F14
unicode_nfkc_cf(0x1F227, [0x6295]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-6295
unicode_nfkc_cf(0x1F228, [0x6355]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-6355
unicode_nfkc_cf(0x1F229, [0x4E00]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-4E00
unicode_nfkc_cf(0x1F22A, [0x4E09]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-4E09
unicode_nfkc_cf(0x1F22B, [0x904A]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-904A
unicode_nfkc_cf(0x1F22C, [0x5DE6]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-5DE6
unicode_nfkc_cf(0x1F22D, [0x4E2D]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-4E2D
unicode_nfkc_cf(0x1F22E, [0x53F3]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-53F3
unicode_nfkc_cf(0x1F22F, [0x6307]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-6307
unicode_nfkc_cf(0x1F230, [0x8D70]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-8D70
unicode_nfkc_cf(0x1F231, [0x6253]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-6253
unicode_nfkc_cf(0x1F232, [0x7981]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-7981
unicode_nfkc_cf(0x1F233, [0x7A7A]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-7A7A
unicode_nfkc_cf(0x1F234, [0x5408]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-5408
unicode_nfkc_cf(0x1F235, [0x6E80]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-6E80
unicode_nfkc_cf(0x1F236, [0x6709]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-6709
unicode_nfkc_cf(0x1F237, [0x6708]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-6708
unicode_nfkc_cf(0x1F238, [0x7533]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-7533
unicode_nfkc_cf(0x1F239, [0x5272]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-5272
unicode_nfkc_cf(0x1F23A, [0x55B6]).						% So       SQUARED CJK UNIFIED IDEOGRAPH-55B6
unicode_nfkc_cf(0x1F240, [0x3014, 0x672C, 0x3015]).		% So       TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C
unicode_nfkc_cf(0x1F241, [0x3014, 0x4E09, 0x3015]).		% So       TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-4E09
unicode_nfkc_cf(0x1F242, [0x3014, 0x4E8C, 0x3015]).		% So       TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-4E8C
unicode_nfkc_cf(0x1F243, [0x3014, 0x5B89, 0x3015]).		% So       TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-5B89
unicode_nfkc_cf(0x1F244, [0x3014, 0x70B9, 0x3015]).		% So       TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-70B9
unicode_nfkc_cf(0x1F245, [0x3014, 0x6253, 0x3015]).		% So       TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6253
unicode_nfkc_cf(0x1F246, [0x3014, 0x76D7, 0x3015]).		% So       TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-76D7
unicode_nfkc_cf(0x1F247, [0x3014, 0x52DD, 0x3015]).		% So       TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-52DD
unicode_nfkc_cf(0x1F248, [0x3014, 0x6557, 0x3015]).		% So       TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
unicode_nfkc_cf(0x1F250, [0x5F97]).						% So       CIRCLED IDEOGRAPH ADVANTAGE
unicode_nfkc_cf(0x1F251, [0x53EF]).						% So       CIRCLED IDEOGRAPH ACCEPT
unicode_nfkc_cf(0x2F800, [0x4E3D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F800
unicode_nfkc_cf(0x2F801, [0x4E38]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F801
unicode_nfkc_cf(0x2F802, [0x4E41]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F802
unicode_nfkc_cf(0x2F803, [0x20122]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F803
unicode_nfkc_cf(0x2F804, [0x4F60]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F804
unicode_nfkc_cf(0x2F805, [0x4FAE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F805
unicode_nfkc_cf(0x2F806, [0x4FBB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F806
unicode_nfkc_cf(0x2F807, [0x5002]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F807
unicode_nfkc_cf(0x2F808, [0x507A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F808
unicode_nfkc_cf(0x2F809, [0x5099]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F809
unicode_nfkc_cf(0x2F80A, [0x50E7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F80A
unicode_nfkc_cf(0x2F80B, [0x50CF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F80B
unicode_nfkc_cf(0x2F80C, [0x349E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F80C
unicode_nfkc_cf(0x2F80D, [0x2063A]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F80D
unicode_nfkc_cf(0x2F80E, [0x514D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F80E
unicode_nfkc_cf(0x2F80F, [0x5154]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F80F
unicode_nfkc_cf(0x2F810, [0x5164]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F810
unicode_nfkc_cf(0x2F811, [0x5177]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F811
unicode_nfkc_cf(0x2F812, [0x2051C]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F812
unicode_nfkc_cf(0x2F813, [0x34B9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F813
unicode_nfkc_cf(0x2F814, [0x5167]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F814
unicode_nfkc_cf(0x2F815, [0x518D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F815
unicode_nfkc_cf(0x2F816, [0x2054B]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F816
unicode_nfkc_cf(0x2F817, [0x5197]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F817
unicode_nfkc_cf(0x2F818, [0x51A4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F818
unicode_nfkc_cf(0x2F819, [0x4ECC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F819
unicode_nfkc_cf(0x2F81A, [0x51AC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F81A
unicode_nfkc_cf(0x2F81B, [0x51B5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F81B
unicode_nfkc_cf(0x2F81C, [0x291DF]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F81C
unicode_nfkc_cf(0x2F81D, [0x51F5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F81D
unicode_nfkc_cf(0x2F81E, [0x5203]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F81E
unicode_nfkc_cf(0x2F81F, [0x34DF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F81F
unicode_nfkc_cf(0x2F820, [0x523B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F820
unicode_nfkc_cf(0x2F821, [0x5246]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F821
unicode_nfkc_cf(0x2F822, [0x5272]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F822
unicode_nfkc_cf(0x2F823, [0x5277]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F823
unicode_nfkc_cf(0x2F824, [0x3515]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F824
unicode_nfkc_cf(0x2F825, [0x52C7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F825
unicode_nfkc_cf(0x2F826, [0x52C9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F826
unicode_nfkc_cf(0x2F827, [0x52E4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F827
unicode_nfkc_cf(0x2F828, [0x52FA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F828
unicode_nfkc_cf(0x2F829, [0x5305]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F829
unicode_nfkc_cf(0x2F82A, [0x5306]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F82A
unicode_nfkc_cf(0x2F82B, [0x5317]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F82B
unicode_nfkc_cf(0x2F82C, [0x5349]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F82C
unicode_nfkc_cf(0x2F82D, [0x5351]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F82D
unicode_nfkc_cf(0x2F82E, [0x535A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F82E
unicode_nfkc_cf(0x2F82F, [0x5373]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F82F
unicode_nfkc_cf(0x2F830, [0x537D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F830
unicode_nfkc_cf(0x2F831, [0x537F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F831
unicode_nfkc_cf(0x2F832, [0x537F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F832
unicode_nfkc_cf(0x2F833, [0x537F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F833
unicode_nfkc_cf(0x2F834, [0x20A2C]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F834
unicode_nfkc_cf(0x2F835, [0x7070]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F835
unicode_nfkc_cf(0x2F836, [0x53CA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F836
unicode_nfkc_cf(0x2F837, [0x53DF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F837
unicode_nfkc_cf(0x2F838, [0x20B63]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F838
unicode_nfkc_cf(0x2F839, [0x53EB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F839
unicode_nfkc_cf(0x2F83A, [0x53F1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F83A
unicode_nfkc_cf(0x2F83B, [0x5406]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F83B
unicode_nfkc_cf(0x2F83C, [0x549E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F83C
unicode_nfkc_cf(0x2F83D, [0x5438]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F83D
unicode_nfkc_cf(0x2F83E, [0x5448]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F83E
unicode_nfkc_cf(0x2F83F, [0x5468]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F83F
unicode_nfkc_cf(0x2F840, [0x54A2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F840
unicode_nfkc_cf(0x2F841, [0x54F6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F841
unicode_nfkc_cf(0x2F842, [0x5510]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F842
unicode_nfkc_cf(0x2F843, [0x5553]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F843
unicode_nfkc_cf(0x2F844, [0x5563]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F844
unicode_nfkc_cf(0x2F845, [0x5584]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F845
unicode_nfkc_cf(0x2F846, [0x5584]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F846
unicode_nfkc_cf(0x2F847, [0x5599]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F847
unicode_nfkc_cf(0x2F848, [0x55AB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F848
unicode_nfkc_cf(0x2F849, [0x55B3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F849
unicode_nfkc_cf(0x2F84A, [0x55C2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F84A
unicode_nfkc_cf(0x2F84B, [0x5716]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F84B
unicode_nfkc_cf(0x2F84C, [0x5606]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F84C
unicode_nfkc_cf(0x2F84D, [0x5717]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F84D
unicode_nfkc_cf(0x2F84E, [0x5651]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F84E
unicode_nfkc_cf(0x2F84F, [0x5674]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F84F
unicode_nfkc_cf(0x2F850, [0x5207]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F850
unicode_nfkc_cf(0x2F851, [0x58EE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F851
unicode_nfkc_cf(0x2F852, [0x57CE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F852
unicode_nfkc_cf(0x2F853, [0x57F4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F853
unicode_nfkc_cf(0x2F854, [0x580D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F854
unicode_nfkc_cf(0x2F855, [0x578B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F855
unicode_nfkc_cf(0x2F856, [0x5832]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F856
unicode_nfkc_cf(0x2F857, [0x5831]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F857
unicode_nfkc_cf(0x2F858, [0x58AC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F858
unicode_nfkc_cf(0x2F859, [0x214E4]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F859
unicode_nfkc_cf(0x2F85A, [0x58F2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F85A
unicode_nfkc_cf(0x2F85B, [0x58F7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F85B
unicode_nfkc_cf(0x2F85C, [0x5906]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F85C
unicode_nfkc_cf(0x2F85D, [0x591A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F85D
unicode_nfkc_cf(0x2F85E, [0x5922]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F85E
unicode_nfkc_cf(0x2F85F, [0x5962]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F85F
unicode_nfkc_cf(0x2F860, [0x216A8]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F860
unicode_nfkc_cf(0x2F861, [0x216EA]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F861
unicode_nfkc_cf(0x2F862, [0x59EC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F862
unicode_nfkc_cf(0x2F863, [0x5A1B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F863
unicode_nfkc_cf(0x2F864, [0x5A27]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F864
unicode_nfkc_cf(0x2F865, [0x59D8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F865
unicode_nfkc_cf(0x2F866, [0x5A66]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F866
unicode_nfkc_cf(0x2F867, [0x36EE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F867
unicode_nfkc_cf(0x2F868, [0x36FC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F868
unicode_nfkc_cf(0x2F869, [0x5B08]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F869
unicode_nfkc_cf(0x2F86A, [0x5B3E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F86A
unicode_nfkc_cf(0x2F86B, [0x5B3E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F86B
unicode_nfkc_cf(0x2F86C, [0x219C8]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F86C
unicode_nfkc_cf(0x2F86D, [0x5BC3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F86D
unicode_nfkc_cf(0x2F86E, [0x5BD8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F86E
unicode_nfkc_cf(0x2F86F, [0x5BE7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F86F
unicode_nfkc_cf(0x2F870, [0x5BF3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F870
unicode_nfkc_cf(0x2F871, [0x21B18]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F871
unicode_nfkc_cf(0x2F872, [0x5BFF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F872
unicode_nfkc_cf(0x2F873, [0x5C06]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F873
unicode_nfkc_cf(0x2F874, [0x5F53]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F874
unicode_nfkc_cf(0x2F875, [0x5C22]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F875
unicode_nfkc_cf(0x2F876, [0x3781]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F876
unicode_nfkc_cf(0x2F877, [0x5C60]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F877
unicode_nfkc_cf(0x2F878, [0x5C6E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F878
unicode_nfkc_cf(0x2F879, [0x5CC0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F879
unicode_nfkc_cf(0x2F87A, [0x5C8D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F87A
unicode_nfkc_cf(0x2F87B, [0x21DE4]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F87B
unicode_nfkc_cf(0x2F87C, [0x5D43]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F87C
unicode_nfkc_cf(0x2F87D, [0x21DE6]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F87D
unicode_nfkc_cf(0x2F87E, [0x5D6E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F87E
unicode_nfkc_cf(0x2F87F, [0x5D6B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F87F
unicode_nfkc_cf(0x2F880, [0x5D7C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F880
unicode_nfkc_cf(0x2F881, [0x5DE1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F881
unicode_nfkc_cf(0x2F882, [0x5DE2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F882
unicode_nfkc_cf(0x2F883, [0x382F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F883
unicode_nfkc_cf(0x2F884, [0x5DFD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F884
unicode_nfkc_cf(0x2F885, [0x5E28]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F885
unicode_nfkc_cf(0x2F886, [0x5E3D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F886
unicode_nfkc_cf(0x2F887, [0x5E69]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F887
unicode_nfkc_cf(0x2F888, [0x3862]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F888
unicode_nfkc_cf(0x2F889, [0x22183]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F889
unicode_nfkc_cf(0x2F88A, [0x387C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F88A
unicode_nfkc_cf(0x2F88B, [0x5EB0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F88B
unicode_nfkc_cf(0x2F88C, [0x5EB3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F88C
unicode_nfkc_cf(0x2F88D, [0x5EB6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F88D
unicode_nfkc_cf(0x2F88E, [0x5ECA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F88E
unicode_nfkc_cf(0x2F88F, [0x2A392]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F88F
unicode_nfkc_cf(0x2F890, [0x5EFE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F890
unicode_nfkc_cf(0x2F891, [0x22331]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F891
unicode_nfkc_cf(0x2F892, [0x22331]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F892
unicode_nfkc_cf(0x2F893, [0x8201]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F893
unicode_nfkc_cf(0x2F894, [0x5F22]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F894
unicode_nfkc_cf(0x2F895, [0x5F22]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F895
unicode_nfkc_cf(0x2F896, [0x38C7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F896
unicode_nfkc_cf(0x2F897, [0x232B8]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F897
unicode_nfkc_cf(0x2F898, [0x261DA]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F898
unicode_nfkc_cf(0x2F899, [0x5F62]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F899
unicode_nfkc_cf(0x2F89A, [0x5F6B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F89A
unicode_nfkc_cf(0x2F89B, [0x38E3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F89B
unicode_nfkc_cf(0x2F89C, [0x5F9A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F89C
unicode_nfkc_cf(0x2F89D, [0x5FCD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F89D
unicode_nfkc_cf(0x2F89E, [0x5FD7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F89E
unicode_nfkc_cf(0x2F89F, [0x5FF9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F89F
unicode_nfkc_cf(0x2F8A0, [0x6081]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8A0
unicode_nfkc_cf(0x2F8A1, [0x393A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8A1
unicode_nfkc_cf(0x2F8A2, [0x391C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8A2
unicode_nfkc_cf(0x2F8A3, [0x6094]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8A3
unicode_nfkc_cf(0x2F8A4, [0x226D4]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8A4
unicode_nfkc_cf(0x2F8A5, [0x60C7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8A5
unicode_nfkc_cf(0x2F8A6, [0x6148]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8A6
unicode_nfkc_cf(0x2F8A7, [0x614C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8A7
unicode_nfkc_cf(0x2F8A8, [0x614E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8A8
unicode_nfkc_cf(0x2F8A9, [0x614C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8A9
unicode_nfkc_cf(0x2F8AA, [0x617A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8AA
unicode_nfkc_cf(0x2F8AB, [0x618E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8AB
unicode_nfkc_cf(0x2F8AC, [0x61B2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8AC
unicode_nfkc_cf(0x2F8AD, [0x61A4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8AD
unicode_nfkc_cf(0x2F8AE, [0x61AF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8AE
unicode_nfkc_cf(0x2F8AF, [0x61DE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8AF
unicode_nfkc_cf(0x2F8B0, [0x61F2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8B0
unicode_nfkc_cf(0x2F8B1, [0x61F6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8B1
unicode_nfkc_cf(0x2F8B2, [0x6210]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8B2
unicode_nfkc_cf(0x2F8B3, [0x621B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8B3
unicode_nfkc_cf(0x2F8B4, [0x625D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8B4
unicode_nfkc_cf(0x2F8B5, [0x62B1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8B5
unicode_nfkc_cf(0x2F8B6, [0x62D4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8B6
unicode_nfkc_cf(0x2F8B7, [0x6350]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8B7
unicode_nfkc_cf(0x2F8B8, [0x22B0C]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8B8
unicode_nfkc_cf(0x2F8B9, [0x633D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8B9
unicode_nfkc_cf(0x2F8BA, [0x62FC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8BA
unicode_nfkc_cf(0x2F8BB, [0x6368]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8BB
unicode_nfkc_cf(0x2F8BC, [0x6383]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8BC
unicode_nfkc_cf(0x2F8BD, [0x63E4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8BD
unicode_nfkc_cf(0x2F8BE, [0x22BF1]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8BE
unicode_nfkc_cf(0x2F8BF, [0x6422]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8BF
unicode_nfkc_cf(0x2F8C0, [0x63C5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8C0
unicode_nfkc_cf(0x2F8C1, [0x63A9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8C1
unicode_nfkc_cf(0x2F8C2, [0x3A2E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8C2
unicode_nfkc_cf(0x2F8C3, [0x6469]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8C3
unicode_nfkc_cf(0x2F8C4, [0x647E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8C4
unicode_nfkc_cf(0x2F8C5, [0x649D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8C5
unicode_nfkc_cf(0x2F8C6, [0x6477]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8C6
unicode_nfkc_cf(0x2F8C7, [0x3A6C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8C7
unicode_nfkc_cf(0x2F8C8, [0x654F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8C8
unicode_nfkc_cf(0x2F8C9, [0x656C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8C9
unicode_nfkc_cf(0x2F8CA, [0x2300A]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8CA
unicode_nfkc_cf(0x2F8CB, [0x65E3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8CB
unicode_nfkc_cf(0x2F8CC, [0x66F8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8CC
unicode_nfkc_cf(0x2F8CD, [0x6649]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8CD
unicode_nfkc_cf(0x2F8CE, [0x3B19]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8CE
unicode_nfkc_cf(0x2F8CF, [0x6691]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8CF
unicode_nfkc_cf(0x2F8D0, [0x3B08]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8D0
unicode_nfkc_cf(0x2F8D1, [0x3AE4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8D1
unicode_nfkc_cf(0x2F8D2, [0x5192]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8D2
unicode_nfkc_cf(0x2F8D3, [0x5195]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8D3
unicode_nfkc_cf(0x2F8D4, [0x6700]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8D4
unicode_nfkc_cf(0x2F8D5, [0x669C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8D5
unicode_nfkc_cf(0x2F8D6, [0x80AD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8D6
unicode_nfkc_cf(0x2F8D7, [0x43D9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8D7
unicode_nfkc_cf(0x2F8D8, [0x6717]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8D8
unicode_nfkc_cf(0x2F8D9, [0x671B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8D9
unicode_nfkc_cf(0x2F8DA, [0x6721]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8DA
unicode_nfkc_cf(0x2F8DB, [0x675E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8DB
unicode_nfkc_cf(0x2F8DC, [0x6753]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8DC
unicode_nfkc_cf(0x2F8DD, [0x233C3]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8DD
unicode_nfkc_cf(0x2F8DE, [0x3B49]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8DE
unicode_nfkc_cf(0x2F8DF, [0x67FA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8DF
unicode_nfkc_cf(0x2F8E0, [0x6785]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8E0
unicode_nfkc_cf(0x2F8E1, [0x6852]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8E1
unicode_nfkc_cf(0x2F8E2, [0x6885]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8E2
unicode_nfkc_cf(0x2F8E3, [0x2346D]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8E3
unicode_nfkc_cf(0x2F8E4, [0x688E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8E4
unicode_nfkc_cf(0x2F8E5, [0x681F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8E5
unicode_nfkc_cf(0x2F8E6, [0x6914]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8E6
unicode_nfkc_cf(0x2F8E7, [0x3B9D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8E7
unicode_nfkc_cf(0x2F8E8, [0x6942]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8E8
unicode_nfkc_cf(0x2F8E9, [0x69A3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8E9
unicode_nfkc_cf(0x2F8EA, [0x69EA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8EA
unicode_nfkc_cf(0x2F8EB, [0x6AA8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8EB
unicode_nfkc_cf(0x2F8EC, [0x236A3]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8EC
unicode_nfkc_cf(0x2F8ED, [0x6ADB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8ED
unicode_nfkc_cf(0x2F8EE, [0x3C18]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8EE
unicode_nfkc_cf(0x2F8EF, [0x6B21]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8EF
unicode_nfkc_cf(0x2F8F0, [0x238A7]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8F0
unicode_nfkc_cf(0x2F8F1, [0x6B54]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8F1
unicode_nfkc_cf(0x2F8F2, [0x3C4E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8F2
unicode_nfkc_cf(0x2F8F3, [0x6B72]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8F3
unicode_nfkc_cf(0x2F8F4, [0x6B9F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8F4
unicode_nfkc_cf(0x2F8F5, [0x6BBA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8F5
unicode_nfkc_cf(0x2F8F6, [0x6BBB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8F6
unicode_nfkc_cf(0x2F8F7, [0x23A8D]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8F7
unicode_nfkc_cf(0x2F8F8, [0x21D0B]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8F8
unicode_nfkc_cf(0x2F8F9, [0x23AFA]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8F9
unicode_nfkc_cf(0x2F8FA, [0x6C4E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8FA
unicode_nfkc_cf(0x2F8FB, [0x23CBC]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8FB
unicode_nfkc_cf(0x2F8FC, [0x6CBF]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8FC
unicode_nfkc_cf(0x2F8FD, [0x6CCD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8FD
unicode_nfkc_cf(0x2F8FE, [0x6C67]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8FE
unicode_nfkc_cf(0x2F8FF, [0x6D16]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F8FF
unicode_nfkc_cf(0x2F900, [0x6D3E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F900
unicode_nfkc_cf(0x2F901, [0x6D77]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F901
unicode_nfkc_cf(0x2F902, [0x6D41]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F902
unicode_nfkc_cf(0x2F903, [0x6D69]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F903
unicode_nfkc_cf(0x2F904, [0x6D78]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F904
unicode_nfkc_cf(0x2F905, [0x6D85]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F905
unicode_nfkc_cf(0x2F906, [0x23D1E]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F906
unicode_nfkc_cf(0x2F907, [0x6D34]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F907
unicode_nfkc_cf(0x2F908, [0x6E2F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F908
unicode_nfkc_cf(0x2F909, [0x6E6E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F909
unicode_nfkc_cf(0x2F90A, [0x3D33]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F90A
unicode_nfkc_cf(0x2F90B, [0x6ECB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F90B
unicode_nfkc_cf(0x2F90C, [0x6EC7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F90C
unicode_nfkc_cf(0x2F90D, [0x23ED1]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F90D
unicode_nfkc_cf(0x2F90E, [0x6DF9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F90E
unicode_nfkc_cf(0x2F90F, [0x6F6E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F90F
unicode_nfkc_cf(0x2F910, [0x23F5E]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F910
unicode_nfkc_cf(0x2F911, [0x23F8E]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F911
unicode_nfkc_cf(0x2F912, [0x6FC6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F912
unicode_nfkc_cf(0x2F913, [0x7039]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F913
unicode_nfkc_cf(0x2F914, [0x701E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F914
unicode_nfkc_cf(0x2F915, [0x701B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F915
unicode_nfkc_cf(0x2F916, [0x3D96]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F916
unicode_nfkc_cf(0x2F917, [0x704A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F917
unicode_nfkc_cf(0x2F918, [0x707D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F918
unicode_nfkc_cf(0x2F919, [0x7077]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F919
unicode_nfkc_cf(0x2F91A, [0x70AD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F91A
unicode_nfkc_cf(0x2F91B, [0x20525]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F91B
unicode_nfkc_cf(0x2F91C, [0x7145]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F91C
unicode_nfkc_cf(0x2F91D, [0x24263]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F91D
unicode_nfkc_cf(0x2F91E, [0x719C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F91E
unicode_nfkc_cf(0x2F91F, [0x243AB]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F91F
unicode_nfkc_cf(0x2F920, [0x7228]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F920
unicode_nfkc_cf(0x2F921, [0x7235]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F921
unicode_nfkc_cf(0x2F922, [0x7250]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F922
unicode_nfkc_cf(0x2F923, [0x24608]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F923
unicode_nfkc_cf(0x2F924, [0x7280]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F924
unicode_nfkc_cf(0x2F925, [0x7295]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F925
unicode_nfkc_cf(0x2F926, [0x24735]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F926
unicode_nfkc_cf(0x2F927, [0x24814]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F927
unicode_nfkc_cf(0x2F928, [0x737A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F928
unicode_nfkc_cf(0x2F929, [0x738B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F929
unicode_nfkc_cf(0x2F92A, [0x3EAC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F92A
unicode_nfkc_cf(0x2F92B, [0x73A5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F92B
unicode_nfkc_cf(0x2F92C, [0x3EB8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F92C
unicode_nfkc_cf(0x2F92D, [0x3EB8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F92D
unicode_nfkc_cf(0x2F92E, [0x7447]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F92E
unicode_nfkc_cf(0x2F92F, [0x745C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F92F
unicode_nfkc_cf(0x2F930, [0x7471]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F930
unicode_nfkc_cf(0x2F931, [0x7485]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F931
unicode_nfkc_cf(0x2F932, [0x74CA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F932
unicode_nfkc_cf(0x2F933, [0x3F1B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F933
unicode_nfkc_cf(0x2F934, [0x7524]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F934
unicode_nfkc_cf(0x2F935, [0x24C36]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F935
unicode_nfkc_cf(0x2F936, [0x753E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F936
unicode_nfkc_cf(0x2F937, [0x24C92]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F937
unicode_nfkc_cf(0x2F938, [0x7570]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F938
unicode_nfkc_cf(0x2F939, [0x2219F]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F939
unicode_nfkc_cf(0x2F93A, [0x7610]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F93A
unicode_nfkc_cf(0x2F93B, [0x24FA1]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F93B
unicode_nfkc_cf(0x2F93C, [0x24FB8]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F93C
unicode_nfkc_cf(0x2F93D, [0x25044]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F93D
unicode_nfkc_cf(0x2F93E, [0x3FFC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F93E
unicode_nfkc_cf(0x2F93F, [0x4008]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F93F
unicode_nfkc_cf(0x2F940, [0x76F4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F940
unicode_nfkc_cf(0x2F941, [0x250F3]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F941
unicode_nfkc_cf(0x2F942, [0x250F2]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F942
unicode_nfkc_cf(0x2F943, [0x25119]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F943
unicode_nfkc_cf(0x2F944, [0x25133]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F944
unicode_nfkc_cf(0x2F945, [0x771E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F945
unicode_nfkc_cf(0x2F946, [0x771F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F946
unicode_nfkc_cf(0x2F947, [0x771F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F947
unicode_nfkc_cf(0x2F948, [0x774A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F948
unicode_nfkc_cf(0x2F949, [0x4039]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F949
unicode_nfkc_cf(0x2F94A, [0x778B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F94A
unicode_nfkc_cf(0x2F94B, [0x4046]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F94B
unicode_nfkc_cf(0x2F94C, [0x4096]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F94C
unicode_nfkc_cf(0x2F94D, [0x2541D]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F94D
unicode_nfkc_cf(0x2F94E, [0x784E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F94E
unicode_nfkc_cf(0x2F94F, [0x788C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F94F
unicode_nfkc_cf(0x2F950, [0x78CC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F950
unicode_nfkc_cf(0x2F951, [0x40E3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F951
unicode_nfkc_cf(0x2F952, [0x25626]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F952
unicode_nfkc_cf(0x2F953, [0x7956]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F953
unicode_nfkc_cf(0x2F954, [0x2569A]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F954
unicode_nfkc_cf(0x2F955, [0x256C5]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F955
unicode_nfkc_cf(0x2F956, [0x798F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F956
unicode_nfkc_cf(0x2F957, [0x79EB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F957
unicode_nfkc_cf(0x2F958, [0x412F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F958
unicode_nfkc_cf(0x2F959, [0x7A40]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F959
unicode_nfkc_cf(0x2F95A, [0x7A4A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F95A
unicode_nfkc_cf(0x2F95B, [0x7A4F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F95B
unicode_nfkc_cf(0x2F95C, [0x2597C]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F95C
unicode_nfkc_cf(0x2F95D, [0x25AA7]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F95D
unicode_nfkc_cf(0x2F95E, [0x25AA7]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F95E
unicode_nfkc_cf(0x2F95F, [0x7AEE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F95F
unicode_nfkc_cf(0x2F960, [0x4202]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F960
unicode_nfkc_cf(0x2F961, [0x25BAB]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F961
unicode_nfkc_cf(0x2F962, [0x7BC6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F962
unicode_nfkc_cf(0x2F963, [0x7BC9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F963
unicode_nfkc_cf(0x2F964, [0x4227]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F964
unicode_nfkc_cf(0x2F965, [0x25C80]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F965
unicode_nfkc_cf(0x2F966, [0x7CD2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F966
unicode_nfkc_cf(0x2F967, [0x42A0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F967
unicode_nfkc_cf(0x2F968, [0x7CE8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F968
unicode_nfkc_cf(0x2F969, [0x7CE3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F969
unicode_nfkc_cf(0x2F96A, [0x7D00]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F96A
unicode_nfkc_cf(0x2F96B, [0x25F86]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F96B
unicode_nfkc_cf(0x2F96C, [0x7D63]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F96C
unicode_nfkc_cf(0x2F96D, [0x4301]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F96D
unicode_nfkc_cf(0x2F96E, [0x7DC7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F96E
unicode_nfkc_cf(0x2F96F, [0x7E02]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F96F
unicode_nfkc_cf(0x2F970, [0x7E45]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F970
unicode_nfkc_cf(0x2F971, [0x4334]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F971
unicode_nfkc_cf(0x2F972, [0x26228]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F972
unicode_nfkc_cf(0x2F973, [0x26247]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F973
unicode_nfkc_cf(0x2F974, [0x4359]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F974
unicode_nfkc_cf(0x2F975, [0x262D9]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F975
unicode_nfkc_cf(0x2F976, [0x7F7A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F976
unicode_nfkc_cf(0x2F977, [0x2633E]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F977
unicode_nfkc_cf(0x2F978, [0x7F95]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F978
unicode_nfkc_cf(0x2F979, [0x7FFA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F979
unicode_nfkc_cf(0x2F97A, [0x8005]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F97A
unicode_nfkc_cf(0x2F97B, [0x264DA]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F97B
unicode_nfkc_cf(0x2F97C, [0x26523]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F97C
unicode_nfkc_cf(0x2F97D, [0x8060]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F97D
unicode_nfkc_cf(0x2F97E, [0x265A8]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F97E
unicode_nfkc_cf(0x2F97F, [0x8070]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F97F
unicode_nfkc_cf(0x2F980, [0x2335F]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F980
unicode_nfkc_cf(0x2F981, [0x43D5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F981
unicode_nfkc_cf(0x2F982, [0x80B2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F982
unicode_nfkc_cf(0x2F983, [0x8103]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F983
unicode_nfkc_cf(0x2F984, [0x440B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F984
unicode_nfkc_cf(0x2F985, [0x813E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F985
unicode_nfkc_cf(0x2F986, [0x5AB5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F986
unicode_nfkc_cf(0x2F987, [0x267A7]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F987
unicode_nfkc_cf(0x2F988, [0x267B5]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F988
unicode_nfkc_cf(0x2F989, [0x23393]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F989
unicode_nfkc_cf(0x2F98A, [0x2339C]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F98A
unicode_nfkc_cf(0x2F98B, [0x8201]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F98B
unicode_nfkc_cf(0x2F98C, [0x8204]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F98C
unicode_nfkc_cf(0x2F98D, [0x8F9E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F98D
unicode_nfkc_cf(0x2F98E, [0x446B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F98E
unicode_nfkc_cf(0x2F98F, [0x8291]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F98F
unicode_nfkc_cf(0x2F990, [0x828B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F990
unicode_nfkc_cf(0x2F991, [0x829D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F991
unicode_nfkc_cf(0x2F992, [0x52B3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F992
unicode_nfkc_cf(0x2F993, [0x82B1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F993
unicode_nfkc_cf(0x2F994, [0x82B3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F994
unicode_nfkc_cf(0x2F995, [0x82BD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F995
unicode_nfkc_cf(0x2F996, [0x82E6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F996
unicode_nfkc_cf(0x2F997, [0x26B3C]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F997
unicode_nfkc_cf(0x2F998, [0x82E5]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F998
unicode_nfkc_cf(0x2F999, [0x831D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F999
unicode_nfkc_cf(0x2F99A, [0x8363]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F99A
unicode_nfkc_cf(0x2F99B, [0x83AD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F99B
unicode_nfkc_cf(0x2F99C, [0x8323]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F99C
unicode_nfkc_cf(0x2F99D, [0x83BD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F99D
unicode_nfkc_cf(0x2F99E, [0x83E7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F99E
unicode_nfkc_cf(0x2F99F, [0x8457]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F99F
unicode_nfkc_cf(0x2F9A0, [0x8353]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9A0
unicode_nfkc_cf(0x2F9A1, [0x83CA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9A1
unicode_nfkc_cf(0x2F9A2, [0x83CC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9A2
unicode_nfkc_cf(0x2F9A3, [0x83DC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9A3
unicode_nfkc_cf(0x2F9A4, [0x26C36]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9A4
unicode_nfkc_cf(0x2F9A5, [0x26D6B]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9A5
unicode_nfkc_cf(0x2F9A6, [0x26CD5]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9A6
unicode_nfkc_cf(0x2F9A7, [0x452B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9A7
unicode_nfkc_cf(0x2F9A8, [0x84F1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9A8
unicode_nfkc_cf(0x2F9A9, [0x84F3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9A9
unicode_nfkc_cf(0x2F9AA, [0x8516]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9AA
unicode_nfkc_cf(0x2F9AB, [0x273CA]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9AB
unicode_nfkc_cf(0x2F9AC, [0x8564]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9AC
unicode_nfkc_cf(0x2F9AD, [0x26F2C]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9AD
unicode_nfkc_cf(0x2F9AE, [0x455D]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9AE
unicode_nfkc_cf(0x2F9AF, [0x4561]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9AF
unicode_nfkc_cf(0x2F9B0, [0x26FB1]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9B0
unicode_nfkc_cf(0x2F9B1, [0x270D2]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9B1
unicode_nfkc_cf(0x2F9B2, [0x456B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9B2
unicode_nfkc_cf(0x2F9B3, [0x8650]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9B3
unicode_nfkc_cf(0x2F9B4, [0x865C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9B4
unicode_nfkc_cf(0x2F9B5, [0x8667]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9B5
unicode_nfkc_cf(0x2F9B6, [0x8669]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9B6
unicode_nfkc_cf(0x2F9B7, [0x86A9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9B7
unicode_nfkc_cf(0x2F9B8, [0x8688]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9B8
unicode_nfkc_cf(0x2F9B9, [0x870E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9B9
unicode_nfkc_cf(0x2F9BA, [0x86E2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9BA
unicode_nfkc_cf(0x2F9BB, [0x8779]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9BB
unicode_nfkc_cf(0x2F9BC, [0x8728]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9BC
unicode_nfkc_cf(0x2F9BD, [0x876B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9BD
unicode_nfkc_cf(0x2F9BE, [0x8786]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9BE
unicode_nfkc_cf(0x2F9BF, [0x45D7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9BF
unicode_nfkc_cf(0x2F9C0, [0x87E1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9C0
unicode_nfkc_cf(0x2F9C1, [0x8801]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9C1
unicode_nfkc_cf(0x2F9C2, [0x45F9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9C2
unicode_nfkc_cf(0x2F9C3, [0x8860]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9C3
unicode_nfkc_cf(0x2F9C4, [0x8863]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9C4
unicode_nfkc_cf(0x2F9C5, [0x27667]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9C5
unicode_nfkc_cf(0x2F9C6, [0x88D7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9C6
unicode_nfkc_cf(0x2F9C7, [0x88DE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9C7
unicode_nfkc_cf(0x2F9C8, [0x4635]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9C8
unicode_nfkc_cf(0x2F9C9, [0x88FA]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9C9
unicode_nfkc_cf(0x2F9CA, [0x34BB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9CA
unicode_nfkc_cf(0x2F9CB, [0x278AE]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9CB
unicode_nfkc_cf(0x2F9CC, [0x27966]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9CC
unicode_nfkc_cf(0x2F9CD, [0x46BE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9CD
unicode_nfkc_cf(0x2F9CE, [0x46C7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9CE
unicode_nfkc_cf(0x2F9CF, [0x8AA0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9CF
unicode_nfkc_cf(0x2F9D0, [0x8AED]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9D0
unicode_nfkc_cf(0x2F9D1, [0x8B8A]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9D1
unicode_nfkc_cf(0x2F9D2, [0x8C55]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9D2
unicode_nfkc_cf(0x2F9D3, [0x27CA8]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9D3
unicode_nfkc_cf(0x2F9D4, [0x8CAB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9D4
unicode_nfkc_cf(0x2F9D5, [0x8CC1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9D5
unicode_nfkc_cf(0x2F9D6, [0x8D1B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9D6
unicode_nfkc_cf(0x2F9D7, [0x8D77]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9D7
unicode_nfkc_cf(0x2F9D8, [0x27F2F]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9D8
unicode_nfkc_cf(0x2F9D9, [0x20804]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9D9
unicode_nfkc_cf(0x2F9DA, [0x8DCB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9DA
unicode_nfkc_cf(0x2F9DB, [0x8DBC]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9DB
unicode_nfkc_cf(0x2F9DC, [0x8DF0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9DC
unicode_nfkc_cf(0x2F9DD, [0x208DE]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9DD
unicode_nfkc_cf(0x2F9DE, [0x8ED4]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9DE
unicode_nfkc_cf(0x2F9DF, [0x8F38]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9DF
unicode_nfkc_cf(0x2F9E0, [0x285D2]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9E0
unicode_nfkc_cf(0x2F9E1, [0x285ED]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9E1
unicode_nfkc_cf(0x2F9E2, [0x9094]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9E2
unicode_nfkc_cf(0x2F9E3, [0x90F1]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9E3
unicode_nfkc_cf(0x2F9E4, [0x9111]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9E4
unicode_nfkc_cf(0x2F9E5, [0x2872E]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9E5
unicode_nfkc_cf(0x2F9E6, [0x911B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9E6
unicode_nfkc_cf(0x2F9E7, [0x9238]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9E7
unicode_nfkc_cf(0x2F9E8, [0x92D7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9E8
unicode_nfkc_cf(0x2F9E9, [0x92D8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9E9
unicode_nfkc_cf(0x2F9EA, [0x927C]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9EA
unicode_nfkc_cf(0x2F9EB, [0x93F9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9EB
unicode_nfkc_cf(0x2F9EC, [0x9415]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9EC
unicode_nfkc_cf(0x2F9ED, [0x28BFA]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9ED
unicode_nfkc_cf(0x2F9EE, [0x958B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9EE
unicode_nfkc_cf(0x2F9EF, [0x4995]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9EF
unicode_nfkc_cf(0x2F9F0, [0x95B7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9F0
unicode_nfkc_cf(0x2F9F1, [0x28D77]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9F1
unicode_nfkc_cf(0x2F9F2, [0x49E6]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9F2
unicode_nfkc_cf(0x2F9F3, [0x96C3]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9F3
unicode_nfkc_cf(0x2F9F4, [0x5DB2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9F4
unicode_nfkc_cf(0x2F9F5, [0x9723]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9F5
unicode_nfkc_cf(0x2F9F6, [0x29145]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9F6
unicode_nfkc_cf(0x2F9F7, [0x2921A]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9F7
unicode_nfkc_cf(0x2F9F8, [0x4A6E]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9F8
unicode_nfkc_cf(0x2F9F9, [0x4A76]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9F9
unicode_nfkc_cf(0x2F9FA, [0x97E0]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9FA
unicode_nfkc_cf(0x2F9FB, [0x2940A]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9FB
unicode_nfkc_cf(0x2F9FC, [0x4AB2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9FC
unicode_nfkc_cf(0x2F9FD, [0x29496]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9FD
unicode_nfkc_cf(0x2F9FE, [0x980B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9FE
unicode_nfkc_cf(0x2F9FF, [0x980B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2F9FF
unicode_nfkc_cf(0x2FA00, [0x9829]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA00
unicode_nfkc_cf(0x2FA01, [0x295B6]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA01
unicode_nfkc_cf(0x2FA02, [0x98E2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA02
unicode_nfkc_cf(0x2FA03, [0x4B33]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA03
unicode_nfkc_cf(0x2FA04, [0x9929]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA04
unicode_nfkc_cf(0x2FA05, [0x99A7]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA05
unicode_nfkc_cf(0x2FA06, [0x99C2]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA06
unicode_nfkc_cf(0x2FA07, [0x99FE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA07
unicode_nfkc_cf(0x2FA08, [0x4BCE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA08
unicode_nfkc_cf(0x2FA09, [0x29B30]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA09
unicode_nfkc_cf(0x2FA0A, [0x9B12]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA0A
unicode_nfkc_cf(0x2FA0B, [0x9C40]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA0B
unicode_nfkc_cf(0x2FA0C, [0x9CFD]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA0C
unicode_nfkc_cf(0x2FA0D, [0x4CCE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA0D
unicode_nfkc_cf(0x2FA0E, [0x4CED]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA0E
unicode_nfkc_cf(0x2FA0F, [0x9D67]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA0F
unicode_nfkc_cf(0x2FA10, [0x2A0CE]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA10
unicode_nfkc_cf(0x2FA11, [0x4CF8]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA11
unicode_nfkc_cf(0x2FA12, [0x2A105]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA12
unicode_nfkc_cf(0x2FA13, [0x2A20E]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA13
unicode_nfkc_cf(0x2FA14, [0x2A291]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA14
unicode_nfkc_cf(0x2FA15, [0x9EBB]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA15
unicode_nfkc_cf(0x2FA16, [0x4D56]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA16
unicode_nfkc_cf(0x2FA17, [0x9EF9]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA17
unicode_nfkc_cf(0x2FA18, [0x9EFE]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA18
unicode_nfkc_cf(0x2FA19, [0x9F05]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA19
unicode_nfkc_cf(0x2FA1A, [0x9F0F]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA1A
unicode_nfkc_cf(0x2FA1B, [0x9F16]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA1B
unicode_nfkc_cf(0x2FA1C, [0x9F3B]).						% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA1C
unicode_nfkc_cf(0x2FA1D, [0x2A600]).					% Lo       CJK COMPATIBILITY IDEOGRAPH-2FA1D
%unicode_nfkc_cf(0xE0000, 0xE0000  ; NFKC_CF;                   # Cn       <reserved-E0000>
%unicode_nfkc_cf(0xE0001, 0xE0001  ; NFKC_CF;                   # Cf       LANGUAGE TAG
%unicode_nfkc_cf(0xE0002, 0xE001F  ; NFKC_CF;                   # Cn  [30] <reserved-E0002>..<reserved-E001F>
%unicode_nfkc_cf(0xE0020, 0xE007F  ; NFKC_CF;                   # Cf  [96] TAG SPACE..CANCEL TAG
%unicode_nfkc_cf(0xE0080, 0xE00FF  ; NFKC_CF;                   # Cn [128] <reserved-E0080>..<reserved-E00FF>
%unicode_nfkc_cf(0xE0100, 0xE01EF  ; NFKC_CF;                   # Mn [240] VARIATION SELECTOR-17..VARIATION SELECTOR-256
%unicode_nfkc_cf(0xE01F0, 0xE0FFF  ; NFKC_CF;                   # Cn [3600] <reserved-E01F0>..<reserved-E0FFF>

% Total code points: 9944
