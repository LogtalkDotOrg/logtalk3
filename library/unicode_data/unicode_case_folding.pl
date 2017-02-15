%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 19, 2012
%
%  Original Unicode file header comments follow

/*
# CaseFolding-6.1.0.txt
# Date: 2011-07-25, 21:21:56 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
#
# Case Folding Properties
#
# This file is a supplement to the UnicodeData file.
# It provides a case folding mapping generated from the Unicode Character Database.
# If all characters are mapped according to the full mapping below, then
# case differences (according to UnicodeData.txt and SpecialCasing.txt)
# are eliminated.
#
# The data supports both implementations that require simple case foldings
# (where string lengths don't change), and implementations that allow full case folding
# (where string lengths may grow). Note that where they can be supported, the
# full case foldings are superior: for example, they allow "MASSE" and "Maße" to match.
#
# All code points not listed in this file map to themselves.
#
# NOTE: case folding does not preserve normalization formats!
#
# For information on case folding, including how to have case folding 
# preserve normalization formats, see Section 3.13 Default Case Algorithms in
# The Unicode Standard, Version 5.0.
#
# ================================================================================
# Format
# ================================================================================
# The entries in this file are in the following machine-readable format:
#
# <code>; <status>; <mapping>).	% <name>
#
# The status field is:
# C: common case folding, common mappings shared by both simple and full mappings.
# F: full case folding, mappings that cause strings to grow in length. Multiple characters are separated by spaces.
# S: simple case folding, mappings to single characters where different from F.
# T: special case for uppercase I and dotted uppercase I
#    - For non-Turkic languages, this mapping is normally not used.
#    - For Turkic languages (tr, az), this mapping can be used instead of the normal mapping for these characters.
#      Note that the Turkic mappings do not maintain canonical equivalence without additional processing.
#      See the discussions of case mapping in the Unicode Standard for more information.
#
# Usage:
#  A. To do a simple case folding, use the mappings with status C + S.
#  B. To do a full case folding, use the mappings with status C + F.
#
#    The mappings with status T can be used or omitted depending on the desired case-folding
#    behavior. (The default option is to exclude them.)
#
# =================================================================

# Property: Case_Folding

#  All code points not explicitly listed for Case_Folding
#  have the value C for the status field, and the code point itself for the mapping field.

# @missing: 0000..10FFFF, c, 0x<code point>

# =================================================================
*/

unicode_case_folding(0x0041, 'C', [0x0061]).					% LATIN CAPITAL LETTER A
unicode_case_folding(0x0042, 'C', [0x0062]).					% LATIN CAPITAL LETTER B
unicode_case_folding(0x0043, 'C', [0x0063]).					% LATIN CAPITAL LETTER C
unicode_case_folding(0x0044, 'C', [0x0064]).					% LATIN CAPITAL LETTER D
unicode_case_folding(0x0045, 'C', [0x0065]).					% LATIN CAPITAL LETTER E
unicode_case_folding(0x0046, 'C', [0x0066]).					% LATIN CAPITAL LETTER F
unicode_case_folding(0x0047, 'C', [0x0067]).					% LATIN CAPITAL LETTER G
unicode_case_folding(0x0048, 'C', [0x0068]).					% LATIN CAPITAL LETTER H
unicode_case_folding(0x0049, 'C', [0x0069]).					% LATIN CAPITAL LETTER I
unicode_case_folding(0x0049, 'T', [0x0131]).					% LATIN CAPITAL LETTER I
unicode_case_folding(0x004A, 'C', [0x006A]).					% LATIN CAPITAL LETTER J
unicode_case_folding(0x004B, 'C', [0x006B]).					% LATIN CAPITAL LETTER K
unicode_case_folding(0x004C, 'C', [0x006C]).					% LATIN CAPITAL LETTER L
unicode_case_folding(0x004D, 'C', [0x006D]).					% LATIN CAPITAL LETTER M
unicode_case_folding(0x004E, 'C', [0x006E]).					% LATIN CAPITAL LETTER N
unicode_case_folding(0x004F, 'C', [0x006F]).					% LATIN CAPITAL LETTER O
unicode_case_folding(0x0050, 'C', [0x0070]).					% LATIN CAPITAL LETTER P
unicode_case_folding(0x0051, 'C', [0x0071]).					% LATIN CAPITAL LETTER Q
unicode_case_folding(0x0052, 'C', [0x0072]).					% LATIN CAPITAL LETTER R
unicode_case_folding(0x0053, 'C', [0x0073]).					% LATIN CAPITAL LETTER S
unicode_case_folding(0x0054, 'C', [0x0074]).					% LATIN CAPITAL LETTER T
unicode_case_folding(0x0055, 'C', [0x0075]).					% LATIN CAPITAL LETTER U
unicode_case_folding(0x0056, 'C', [0x0076]).					% LATIN CAPITAL LETTER V
unicode_case_folding(0x0057, 'C', [0x0077]).					% LATIN CAPITAL LETTER W
unicode_case_folding(0x0058, 'C', [0x0078]).					% LATIN CAPITAL LETTER X
unicode_case_folding(0x0059, 'C', [0x0079]).					% LATIN CAPITAL LETTER Y
unicode_case_folding(0x005A, 'C', [0x007A]).					% LATIN CAPITAL LETTER Z
unicode_case_folding(0x00B5, 'C', [0x03BC]).					% MICRO SIGN
unicode_case_folding(0x00C0, 'C', [0x00E0]).					% LATIN CAPITAL LETTER A WITH GRAVE
unicode_case_folding(0x00C1, 'C', [0x00E1]).					% LATIN CAPITAL LETTER A WITH ACUTE
unicode_case_folding(0x00C2, 'C', [0x00E2]).					% LATIN CAPITAL LETTER A WITH CIRCUMFLEX
unicode_case_folding(0x00C3, 'C', [0x00E3]).					% LATIN CAPITAL LETTER A WITH TILDE
unicode_case_folding(0x00C4, 'C', [0x00E4]).					% LATIN CAPITAL LETTER A WITH DIAERESIS
unicode_case_folding(0x00C5, 'C', [0x00E5]).					% LATIN CAPITAL LETTER A WITH RING ABOVE
unicode_case_folding(0x00C6, 'C', [0x00E6]).					% LATIN CAPITAL LETTER AE
unicode_case_folding(0x00C7, 'C', [0x00E7]).					% LATIN CAPITAL LETTER C WITH CEDILLA
unicode_case_folding(0x00C8, 'C', [0x00E8]).					% LATIN CAPITAL LETTER E WITH GRAVE
unicode_case_folding(0x00C9, 'C', [0x00E9]).					% LATIN CAPITAL LETTER E WITH ACUTE
unicode_case_folding(0x00CA, 'C', [0x00EA]).					% LATIN CAPITAL LETTER E WITH CIRCUMFLEX
unicode_case_folding(0x00CB, 'C', [0x00EB]).					% LATIN CAPITAL LETTER E WITH DIAERESIS
unicode_case_folding(0x00CC, 'C', [0x00EC]).					% LATIN CAPITAL LETTER I WITH GRAVE
unicode_case_folding(0x00CD, 'C', [0x00ED]).					% LATIN CAPITAL LETTER I WITH ACUTE
unicode_case_folding(0x00CE, 'C', [0x00EE]).					% LATIN CAPITAL LETTER I WITH CIRCUMFLEX
unicode_case_folding(0x00CF, 'C', [0x00EF]).					% LATIN CAPITAL LETTER I WITH DIAERESIS
unicode_case_folding(0x00D0, 'C', [0x00F0]).					% LATIN CAPITAL LETTER ETH
unicode_case_folding(0x00D1, 'C', [0x00F1]).					% LATIN CAPITAL LETTER N WITH TILDE
unicode_case_folding(0x00D2, 'C', [0x00F2]).					% LATIN CAPITAL LETTER O WITH GRAVE
unicode_case_folding(0x00D3, 'C', [0x00F3]).					% LATIN CAPITAL LETTER O WITH ACUTE
unicode_case_folding(0x00D4, 'C', [0x00F4]).					% LATIN CAPITAL LETTER O WITH CIRCUMFLEX
unicode_case_folding(0x00D5, 'C', [0x00F5]).					% LATIN CAPITAL LETTER O WITH TILDE
unicode_case_folding(0x00D6, 'C', [0x00F6]).					% LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_case_folding(0x00D8, 'C', [0x00F8]).					% LATIN CAPITAL LETTER O WITH STROKE
unicode_case_folding(0x00D9, 'C', [0x00F9]).					% LATIN CAPITAL LETTER U WITH GRAVE
unicode_case_folding(0x00DA, 'C', [0x00FA]).					% LATIN CAPITAL LETTER U WITH ACUTE
unicode_case_folding(0x00DB, 'C', [0x00FB]).					% LATIN CAPITAL LETTER U WITH CIRCUMFLEX
unicode_case_folding(0x00DC, 'C', [0x00FC]).					% LATIN CAPITAL LETTER U WITH DIAERESIS
unicode_case_folding(0x00DD, 'C', [0x00FD]).					% LATIN CAPITAL LETTER Y WITH ACUTE
unicode_case_folding(0x00DE, 'C', [0x00FE]).					% LATIN CAPITAL LETTER THORN
unicode_case_folding(0x00DF, 'F', [0x0073, 0x0073]).			% LATIN SMALL LETTER SHARP S
unicode_case_folding(0x0100, 'C', [0x0101]).					% LATIN CAPITAL LETTER A WITH MACRON
unicode_case_folding(0x0102, 'C', [0x0103]).					% LATIN CAPITAL LETTER A WITH BREVE
unicode_case_folding(0x0104, 'C', [0x0105]).					% LATIN CAPITAL LETTER A WITH OGONEK
unicode_case_folding(0x0106, 'C', [0x0107]).					% LATIN CAPITAL LETTER C WITH ACUTE
unicode_case_folding(0x0108, 'C', [0x0109]).					% LATIN CAPITAL LETTER C WITH CIRCUMFLEX
unicode_case_folding(0x010A, 'C', [0x010B]).					% LATIN CAPITAL LETTER C WITH DOT ABOVE
unicode_case_folding(0x010C, 'C', [0x010D]).					% LATIN CAPITAL LETTER C WITH CARON
unicode_case_folding(0x010E, 'C', [0x010F]).					% LATIN CAPITAL LETTER D WITH CARON
unicode_case_folding(0x0110, 'C', [0x0111]).					% LATIN CAPITAL LETTER D WITH STROKE
unicode_case_folding(0x0112, 'C', [0x0113]).					% LATIN CAPITAL LETTER E WITH MACRON
unicode_case_folding(0x0114, 'C', [0x0115]).					% LATIN CAPITAL LETTER E WITH BREVE
unicode_case_folding(0x0116, 'C', [0x0117]).					% LATIN CAPITAL LETTER E WITH DOT ABOVE
unicode_case_folding(0x0118, 'C', [0x0119]).					% LATIN CAPITAL LETTER E WITH OGONEK
unicode_case_folding(0x011A, 'C', [0x011B]).					% LATIN CAPITAL LETTER E WITH CARON
unicode_case_folding(0x011C, 'C', [0x011D]).					% LATIN CAPITAL LETTER G WITH CIRCUMFLEX
unicode_case_folding(0x011E, 'C', [0x011F]).					% LATIN CAPITAL LETTER G WITH BREVE
unicode_case_folding(0x0120, 'C', [0x0121]).					% LATIN CAPITAL LETTER G WITH DOT ABOVE
unicode_case_folding(0x0122, 'C', [0x0123]).					% LATIN CAPITAL LETTER G WITH CEDILLA
unicode_case_folding(0x0124, 'C', [0x0125]).					% LATIN CAPITAL LETTER H WITH CIRCUMFLEX
unicode_case_folding(0x0126, 'C', [0x0127]).					% LATIN CAPITAL LETTER H WITH STROKE
unicode_case_folding(0x0128, 'C', [0x0129]).					% LATIN CAPITAL LETTER I WITH TILDE
unicode_case_folding(0x012A, 'C', [0x012B]).					% LATIN CAPITAL LETTER I WITH MACRON
unicode_case_folding(0x012C, 'C', [0x012D]).					% LATIN CAPITAL LETTER I WITH BREVE
unicode_case_folding(0x012E, 'C', [0x012F]).					% LATIN CAPITAL LETTER I WITH OGONEK
unicode_case_folding(0x0130, 'F', [0x0069, 0x0307]).			% LATIN CAPITAL LETTER I WITH DOT ABOVE
unicode_case_folding(0x0130, 'T', [0x0069]).					% LATIN CAPITAL LETTER I WITH DOT ABOVE
unicode_case_folding(0x0132, 'C', [0x0133]).					% LATIN CAPITAL LIGATURE IJ
unicode_case_folding(0x0134, 'C', [0x0135]).					% LATIN CAPITAL LETTER J WITH CIRCUMFLEX
unicode_case_folding(0x0136, 'C', [0x0137]).					% LATIN CAPITAL LETTER K WITH CEDILLA
unicode_case_folding(0x0139, 'C', [0x013A]).					% LATIN CAPITAL LETTER L WITH ACUTE
unicode_case_folding(0x013B, 'C', [0x013C]).					% LATIN CAPITAL LETTER L WITH CEDILLA
unicode_case_folding(0x013D, 'C', [0x013E]).					% LATIN CAPITAL LETTER L WITH CARON
unicode_case_folding(0x013F, 'C', [0x0140]).					% LATIN CAPITAL LETTER L WITH MIDDLE DOT
unicode_case_folding(0x0141, 'C', [0x0142]).					% LATIN CAPITAL LETTER L WITH STROKE
unicode_case_folding(0x0143, 'C', [0x0144]).					% LATIN CAPITAL LETTER N WITH ACUTE
unicode_case_folding(0x0145, 'C', [0x0146]).					% LATIN CAPITAL LETTER N WITH CEDILLA
unicode_case_folding(0x0147, 'C', [0x0148]).					% LATIN CAPITAL LETTER N WITH CARON
unicode_case_folding(0x0149, 'F', [0x02BC, 0x006E]).			% LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
unicode_case_folding(0x014A, 'C', [0x014B]).					% LATIN CAPITAL LETTER ENG
unicode_case_folding(0x014C, 'C', [0x014D]).					% LATIN CAPITAL LETTER O WITH MACRON
unicode_case_folding(0x014E, 'C', [0x014F]).					% LATIN CAPITAL LETTER O WITH BREVE
unicode_case_folding(0x0150, 'C', [0x0151]).					% LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
unicode_case_folding(0x0152, 'C', [0x0153]).					% LATIN CAPITAL LIGATURE OE
unicode_case_folding(0x0154, 'C', [0x0155]).					% LATIN CAPITAL LETTER R WITH ACUTE
unicode_case_folding(0x0156, 'C', [0x0157]).					% LATIN CAPITAL LETTER R WITH CEDILLA
unicode_case_folding(0x0158, 'C', [0x0159]).					% LATIN CAPITAL LETTER R WITH CARON
unicode_case_folding(0x015A, 'C', [0x015B]).					% LATIN CAPITAL LETTER S WITH ACUTE
unicode_case_folding(0x015C, 'C', [0x015D]).					% LATIN CAPITAL LETTER S WITH CIRCUMFLEX
unicode_case_folding(0x015E, 'C', [0x015F]).					% LATIN CAPITAL LETTER S WITH CEDILLA
unicode_case_folding(0x0160, 'C', [0x0161]).					% LATIN CAPITAL LETTER S WITH CARON
unicode_case_folding(0x0162, 'C', [0x0163]).					% LATIN CAPITAL LETTER T WITH CEDILLA
unicode_case_folding(0x0164, 'C', [0x0165]).					% LATIN CAPITAL LETTER T WITH CARON
unicode_case_folding(0x0166, 'C', [0x0167]).					% LATIN CAPITAL LETTER T WITH STROKE
unicode_case_folding(0x0168, 'C', [0x0169]).					% LATIN CAPITAL LETTER U WITH TILDE
unicode_case_folding(0x016A, 'C', [0x016B]).					% LATIN CAPITAL LETTER U WITH MACRON
unicode_case_folding(0x016C, 'C', [0x016D]).					% LATIN CAPITAL LETTER U WITH BREVE
unicode_case_folding(0x016E, 'C', [0x016F]).					% LATIN CAPITAL LETTER U WITH RING ABOVE
unicode_case_folding(0x0170, 'C', [0x0171]).					% LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
unicode_case_folding(0x0172, 'C', [0x0173]).					% LATIN CAPITAL LETTER U WITH OGONEK
unicode_case_folding(0x0174, 'C', [0x0175]).					% LATIN CAPITAL LETTER W WITH CIRCUMFLEX
unicode_case_folding(0x0176, 'C', [0x0177]).					% LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
unicode_case_folding(0x0178, 'C', [0x00FF]).					% LATIN CAPITAL LETTER Y WITH DIAERESIS
unicode_case_folding(0x0179, 'C', [0x017A]).					% LATIN CAPITAL LETTER Z WITH ACUTE
unicode_case_folding(0x017B, 'C', [0x017C]).					% LATIN CAPITAL LETTER Z WITH DOT ABOVE
unicode_case_folding(0x017D, 'C', [0x017E]).					% LATIN CAPITAL LETTER Z WITH CARON
unicode_case_folding(0x017F, 'C', [0x0073]).					% LATIN SMALL LETTER LONG S
unicode_case_folding(0x0181, 'C', [0x0253]).					% LATIN CAPITAL LETTER B WITH HOOK
unicode_case_folding(0x0182, 'C', [0x0183]).					% LATIN CAPITAL LETTER B WITH TOPBAR
unicode_case_folding(0x0184, 'C', [0x0185]).					% LATIN CAPITAL LETTER TONE SIX
unicode_case_folding(0x0186, 'C', [0x0254]).					% LATIN CAPITAL LETTER OPEN O
unicode_case_folding(0x0187, 'C', [0x0188]).					% LATIN CAPITAL LETTER C WITH HOOK
unicode_case_folding(0x0189, 'C', [0x0256]).					% LATIN CAPITAL LETTER AFRICAN D
unicode_case_folding(0x018A, 'C', [0x0257]).					% LATIN CAPITAL LETTER D WITH HOOK
unicode_case_folding(0x018B, 'C', [0x018C]).					% LATIN CAPITAL LETTER D WITH TOPBAR
unicode_case_folding(0x018E, 'C', [0x01DD]).					% LATIN CAPITAL LETTER REVERSED E
unicode_case_folding(0x018F, 'C', [0x0259]).					% LATIN CAPITAL LETTER SCHWA
unicode_case_folding(0x0190, 'C', [0x025B]).					% LATIN CAPITAL LETTER OPEN E
unicode_case_folding(0x0191, 'C', [0x0192]).					% LATIN CAPITAL LETTER F WITH HOOK
unicode_case_folding(0x0193, 'C', [0x0260]).					% LATIN CAPITAL LETTER G WITH HOOK
unicode_case_folding(0x0194, 'C', [0x0263]).					% LATIN CAPITAL LETTER GAMMA
unicode_case_folding(0x0196, 'C', [0x0269]).					% LATIN CAPITAL LETTER IOTA
unicode_case_folding(0x0197, 'C', [0x0268]).					% LATIN CAPITAL LETTER I WITH STROKE
unicode_case_folding(0x0198, 'C', [0x0199]).					% LATIN CAPITAL LETTER K WITH HOOK
unicode_case_folding(0x019C, 'C', [0x026F]).					% LATIN CAPITAL LETTER TURNED M
unicode_case_folding(0x019D, 'C', [0x0272]).					% LATIN CAPITAL LETTER N WITH LEFT HOOK
unicode_case_folding(0x019F, 'C', [0x0275]).					% LATIN CAPITAL LETTER O WITH MIDDLE TILDE
unicode_case_folding(0x01A0, 'C', [0x01A1]).					% LATIN CAPITAL LETTER O WITH HORN
unicode_case_folding(0x01A2, 'C', [0x01A3]).					% LATIN CAPITAL LETTER OI
unicode_case_folding(0x01A4, 'C', [0x01A5]).					% LATIN CAPITAL LETTER P WITH HOOK
unicode_case_folding(0x01A6, 'C', [0x0280]).					% LATIN LETTER YR
unicode_case_folding(0x01A7, 'C', [0x01A8]).					% LATIN CAPITAL LETTER TONE TWO
unicode_case_folding(0x01A9, 'C', [0x0283]).					% LATIN CAPITAL LETTER ESH
unicode_case_folding(0x01AC, 'C', [0x01AD]).					% LATIN CAPITAL LETTER T WITH HOOK
unicode_case_folding(0x01AE, 'C', [0x0288]).					% LATIN CAPITAL LETTER T WITH RETROFLEX HOOK
unicode_case_folding(0x01AF, 'C', [0x01B0]).					% LATIN CAPITAL LETTER U WITH HORN
unicode_case_folding(0x01B1, 'C', [0x028A]).					% LATIN CAPITAL LETTER UPSILON
unicode_case_folding(0x01B2, 'C', [0x028B]).					% LATIN CAPITAL LETTER V WITH HOOK
unicode_case_folding(0x01B3, 'C', [0x01B4]).					% LATIN CAPITAL LETTER Y WITH HOOK
unicode_case_folding(0x01B5, 'C', [0x01B6]).					% LATIN CAPITAL LETTER Z WITH STROKE
unicode_case_folding(0x01B7, 'C', [0x0292]).					% LATIN CAPITAL LETTER EZH
unicode_case_folding(0x01B8, 'C', [0x01B9]).					% LATIN CAPITAL LETTER EZH REVERSED
unicode_case_folding(0x01BC, 'C', [0x01BD]).					% LATIN CAPITAL LETTER TONE FIVE
unicode_case_folding(0x01C4, 'C', [0x01C6]).					% LATIN CAPITAL LETTER DZ WITH CARON
unicode_case_folding(0x01C5, 'C', [0x01C6]).					% LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
unicode_case_folding(0x01C7, 'C', [0x01C9]).					% LATIN CAPITAL LETTER LJ
unicode_case_folding(0x01C8, 'C', [0x01C9]).					% LATIN CAPITAL LETTER L WITH SMALL LETTER J
unicode_case_folding(0x01CA, 'C', [0x01CC]).					% LATIN CAPITAL LETTER NJ
unicode_case_folding(0x01CB, 'C', [0x01CC]).					% LATIN CAPITAL LETTER N WITH SMALL LETTER J
unicode_case_folding(0x01CD, 'C', [0x01CE]).					% LATIN CAPITAL LETTER A WITH CARON
unicode_case_folding(0x01CF, 'C', [0x01D0]).					% LATIN CAPITAL LETTER I WITH CARON
unicode_case_folding(0x01D1, 'C', [0x01D2]).					% LATIN CAPITAL LETTER O WITH CARON
unicode_case_folding(0x01D3, 'C', [0x01D4]).					% LATIN CAPITAL LETTER U WITH CARON
unicode_case_folding(0x01D5, 'C', [0x01D6]).					% LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
unicode_case_folding(0x01D7, 'C', [0x01D8]).					% LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
unicode_case_folding(0x01D9, 'C', [0x01DA]).					% LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
unicode_case_folding(0x01DB, 'C', [0x01DC]).					% LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
unicode_case_folding(0x01DE, 'C', [0x01DF]).					% LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
unicode_case_folding(0x01E0, 'C', [0x01E1]).					% LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
unicode_case_folding(0x01E2, 'C', [0x01E3]).					% LATIN CAPITAL LETTER AE WITH MACRON
unicode_case_folding(0x01E4, 'C', [0x01E5]).					% LATIN CAPITAL LETTER G WITH STROKE
unicode_case_folding(0x01E6, 'C', [0x01E7]).					% LATIN CAPITAL LETTER G WITH CARON
unicode_case_folding(0x01E8, 'C', [0x01E9]).					% LATIN CAPITAL LETTER K WITH CARON
unicode_case_folding(0x01EA, 'C', [0x01EB]).					% LATIN CAPITAL LETTER O WITH OGONEK
unicode_case_folding(0x01EC, 'C', [0x01ED]).					% LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
unicode_case_folding(0x01EE, 'C', [0x01EF]).					% LATIN CAPITAL LETTER EZH WITH CARON
unicode_case_folding(0x01F0, 'F', [0x006A, 0x030C]).			% LATIN SMALL LETTER J WITH CARON
unicode_case_folding(0x01F1, 'C', [0x01F3]).					% LATIN CAPITAL LETTER DZ
unicode_case_folding(0x01F2, 'C', [0x01F3]).					% LATIN CAPITAL LETTER D WITH SMALL LETTER Z
unicode_case_folding(0x01F4, 'C', [0x01F5]).					% LATIN CAPITAL LETTER G WITH ACUTE
unicode_case_folding(0x01F6, 'C', [0x0195]).					% LATIN CAPITAL LETTER HWAIR
unicode_case_folding(0x01F7, 'C', [0x01BF]).					% LATIN CAPITAL LETTER WYNN
unicode_case_folding(0x01F8, 'C', [0x01F9]).					% LATIN CAPITAL LETTER N WITH GRAVE
unicode_case_folding(0x01FA, 'C', [0x01FB]).					% LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
unicode_case_folding(0x01FC, 'C', [0x01FD]).					% LATIN CAPITAL LETTER AE WITH ACUTE
unicode_case_folding(0x01FE, 'C', [0x01FF]).					% LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
unicode_case_folding(0x0200, 'C', [0x0201]).					% LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
unicode_case_folding(0x0202, 'C', [0x0203]).					% LATIN CAPITAL LETTER A WITH INVERTED BREVE
unicode_case_folding(0x0204, 'C', [0x0205]).					% LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
unicode_case_folding(0x0206, 'C', [0x0207]).					% LATIN CAPITAL LETTER E WITH INVERTED BREVE
unicode_case_folding(0x0208, 'C', [0x0209]).					% LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
unicode_case_folding(0x020A, 'C', [0x020B]).					% LATIN CAPITAL LETTER I WITH INVERTED BREVE
unicode_case_folding(0x020C, 'C', [0x020D]).					% LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
unicode_case_folding(0x020E, 'C', [0x020F]).					% LATIN CAPITAL LETTER O WITH INVERTED BREVE
unicode_case_folding(0x0210, 'C', [0x0211]).					% LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
unicode_case_folding(0x0212, 'C', [0x0213]).					% LATIN CAPITAL LETTER R WITH INVERTED BREVE
unicode_case_folding(0x0214, 'C', [0x0215]).					% LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
unicode_case_folding(0x0216, 'C', [0x0217]).					% LATIN CAPITAL LETTER U WITH INVERTED BREVE
unicode_case_folding(0x0218, 'C', [0x0219]).					% LATIN CAPITAL LETTER S WITH COMMA BELOW
unicode_case_folding(0x021A, 'C', [0x021B]).					% LATIN CAPITAL LETTER T WITH COMMA BELOW
unicode_case_folding(0x021C, 'C', [0x021D]).					% LATIN CAPITAL LETTER YOGH
unicode_case_folding(0x021E, 'C', [0x021F]).					% LATIN CAPITAL LETTER H WITH CARON
unicode_case_folding(0x0220, 'C', [0x019E]).					% LATIN CAPITAL LETTER N WITH LONG RIGHT LEG
unicode_case_folding(0x0222, 'C', [0x0223]).					% LATIN CAPITAL LETTER OU
unicode_case_folding(0x0224, 'C', [0x0225]).					% LATIN CAPITAL LETTER Z WITH HOOK
unicode_case_folding(0x0226, 'C', [0x0227]).					% LATIN CAPITAL LETTER A WITH DOT ABOVE
unicode_case_folding(0x0228, 'C', [0x0229]).					% LATIN CAPITAL LETTER E WITH CEDILLA
unicode_case_folding(0x022A, 'C', [0x022B]).					% LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
unicode_case_folding(0x022C, 'C', [0x022D]).					% LATIN CAPITAL LETTER O WITH TILDE AND MACRON
unicode_case_folding(0x022E, 'C', [0x022F]).					% LATIN CAPITAL LETTER O WITH DOT ABOVE
unicode_case_folding(0x0230, 'C', [0x0231]).					% LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
unicode_case_folding(0x0232, 'C', [0x0233]).					% LATIN CAPITAL LETTER Y WITH MACRON
unicode_case_folding(0x023A, 'C', [0x2C65]).					% LATIN CAPITAL LETTER A WITH STROKE
unicode_case_folding(0x023B, 'C', [0x023C]).					% LATIN CAPITAL LETTER C WITH STROKE
unicode_case_folding(0x023D, 'C', [0x019A]).					% LATIN CAPITAL LETTER L WITH BAR
unicode_case_folding(0x023E, 'C', [0x2C66]).					% LATIN CAPITAL LETTER T WITH DIAGONAL STROKE
unicode_case_folding(0x0241, 'C', [0x0242]).					% LATIN CAPITAL LETTER GLOTTAL STOP
unicode_case_folding(0x0243, 'C', [0x0180]).					% LATIN CAPITAL LETTER B WITH STROKE
unicode_case_folding(0x0244, 'C', [0x0289]).					% LATIN CAPITAL LETTER U BAR
unicode_case_folding(0x0245, 'C', [0x028C]).					% LATIN CAPITAL LETTER TURNED V
unicode_case_folding(0x0246, 'C', [0x0247]).					% LATIN CAPITAL LETTER E WITH STROKE
unicode_case_folding(0x0248, 'C', [0x0249]).					% LATIN CAPITAL LETTER J WITH STROKE
unicode_case_folding(0x024A, 'C', [0x024B]).					% LATIN CAPITAL LETTER SMALL Q WITH HOOK TAIL
unicode_case_folding(0x024C, 'C', [0x024D]).					% LATIN CAPITAL LETTER R WITH STROKE
unicode_case_folding(0x024E, 'C', [0x024F]).					% LATIN CAPITAL LETTER Y WITH STROKE
unicode_case_folding(0x0345, 'C', [0x03B9]).					% COMBINING GREEK YPOGEGRAMMENI
unicode_case_folding(0x0370, 'C', [0x0371]).					% GREEK CAPITAL LETTER HETA
unicode_case_folding(0x0372, 'C', [0x0373]).					% GREEK CAPITAL LETTER ARCHAIC SAMPI
unicode_case_folding(0x0376, 'C', [0x0377]).					% GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA
unicode_case_folding(0x0386, 'C', [0x03AC]).					% GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_case_folding(0x0388, 'C', [0x03AD]).					% GREEK CAPITAL LETTER EPSILON WITH TONOS
unicode_case_folding(0x0389, 'C', [0x03AE]).					% GREEK CAPITAL LETTER ETA WITH TONOS
unicode_case_folding(0x038A, 'C', [0x03AF]).					% GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_case_folding(0x038C, 'C', [0x03CC]).					% GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_case_folding(0x038E, 'C', [0x03CD]).					% GREEK CAPITAL LETTER UPSILON WITH TONOS
unicode_case_folding(0x038F, 'C', [0x03CE]).					% GREEK CAPITAL LETTER OMEGA WITH TONOS
unicode_case_folding(0x0390, 'F', [0x03B9, 0x0308, 0x0301]).	% GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
unicode_case_folding(0x0391, 'C', [0x03B1]).					% GREEK CAPITAL LETTER ALPHA
unicode_case_folding(0x0392, 'C', [0x03B2]).					% GREEK CAPITAL LETTER BETA
unicode_case_folding(0x0393, 'C', [0x03B3]).					% GREEK CAPITAL LETTER GAMMA
unicode_case_folding(0x0394, 'C', [0x03B4]).					% GREEK CAPITAL LETTER DELTA
unicode_case_folding(0x0395, 'C', [0x03B5]).					% GREEK CAPITAL LETTER EPSILON
unicode_case_folding(0x0396, 'C', [0x03B6]).					% GREEK CAPITAL LETTER ZETA
unicode_case_folding(0x0397, 'C', [0x03B7]).					% GREEK CAPITAL LETTER ETA
unicode_case_folding(0x0398, 'C', [0x03B8]).					% GREEK CAPITAL LETTER THETA
unicode_case_folding(0x0399, 'C', [0x03B9]).					% GREEK CAPITAL LETTER IOTA
unicode_case_folding(0x039A, 'C', [0x03BA]).					% GREEK CAPITAL LETTER KAPPA
unicode_case_folding(0x039B, 'C', [0x03BB]).					% GREEK CAPITAL LETTER LAMDA
unicode_case_folding(0x039C, 'C', [0x03BC]).					% GREEK CAPITAL LETTER MU
unicode_case_folding(0x039D, 'C', [0x03BD]).					% GREEK CAPITAL LETTER NU
unicode_case_folding(0x039E, 'C', [0x03BE]).					% GREEK CAPITAL LETTER XI
unicode_case_folding(0x039F, 'C', [0x03BF]).					% GREEK CAPITAL LETTER OMICRON
unicode_case_folding(0x03A0, 'C', [0x03C0]).					% GREEK CAPITAL LETTER PI
unicode_case_folding(0x03A1, 'C', [0x03C1]).					% GREEK CAPITAL LETTER RHO
unicode_case_folding(0x03A3, 'C', [0x03C3]).					% GREEK CAPITAL LETTER SIGMA
unicode_case_folding(0x03A4, 'C', [0x03C4]).					% GREEK CAPITAL LETTER TAU
unicode_case_folding(0x03A5, 'C', [0x03C5]).					% GREEK CAPITAL LETTER UPSILON
unicode_case_folding(0x03A6, 'C', [0x03C6]).					% GREEK CAPITAL LETTER PHI
unicode_case_folding(0x03A7, 'C', [0x03C7]).					% GREEK CAPITAL LETTER CHI
unicode_case_folding(0x03A8, 'C', [0x03C8]).					% GREEK CAPITAL LETTER PSI
unicode_case_folding(0x03A9, 'C', [0x03C9]).					% GREEK CAPITAL LETTER OMEGA
unicode_case_folding(0x03AA, 'C', [0x03CA]).					% GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
unicode_case_folding(0x03AB, 'C', [0x03CB]).					% GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
unicode_case_folding(0x03B0, 'F', [0x03C5, 0x0308, 0x0301]).	% GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
unicode_case_folding(0x03C2, 'C', [0x03C3]).					% GREEK SMALL LETTER FINAL SIGMA
unicode_case_folding(0x03CF, 'C', [0x03D7]).					% GREEK CAPITAL KAI SYMBOL
unicode_case_folding(0x03D0, 'C', [0x03B2]).					% GREEK BETA SYMBOL
unicode_case_folding(0x03D1, 'C', [0x03B8]).					% GREEK THETA SYMBOL
unicode_case_folding(0x03D5, 'C', [0x03C6]).					% GREEK PHI SYMBOL
unicode_case_folding(0x03D6, 'C', [0x03C0]).					% GREEK PI SYMBOL
unicode_case_folding(0x03D8, 'C', [0x03D9]).					% GREEK LETTER ARCHAIC KOPPA
unicode_case_folding(0x03DA, 'C', [0x03DB]).					% GREEK LETTER STIGMA
unicode_case_folding(0x03DC, 'C', [0x03DD]).					% GREEK LETTER DIGAMMA
unicode_case_folding(0x03DE, 'C', [0x03DF]).					% GREEK LETTER KOPPA
unicode_case_folding(0x03E0, 'C', [0x03E1]).					% GREEK LETTER SAMPI
unicode_case_folding(0x03E2, 'C', [0x03E3]).					% COPTIC CAPITAL LETTER SHEI
unicode_case_folding(0x03E4, 'C', [0x03E5]).					% COPTIC CAPITAL LETTER FEI
unicode_case_folding(0x03E6, 'C', [0x03E7]).					% COPTIC CAPITAL LETTER KHEI
unicode_case_folding(0x03E8, 'C', [0x03E9]).					% COPTIC CAPITAL LETTER HORI
unicode_case_folding(0x03EA, 'C', [0x03EB]).					% COPTIC CAPITAL LETTER GANGIA
unicode_case_folding(0x03EC, 'C', [0x03ED]).					% COPTIC CAPITAL LETTER SHIMA
unicode_case_folding(0x03EE, 'C', [0x03EF]).					% COPTIC CAPITAL LETTER DEI
unicode_case_folding(0x03F0, 'C', [0x03BA]).					% GREEK KAPPA SYMBOL
unicode_case_folding(0x03F1, 'C', [0x03C1]).					% GREEK RHO SYMBOL
unicode_case_folding(0x03F4, 'C', [0x03B8]).					% GREEK CAPITAL THETA SYMBOL
unicode_case_folding(0x03F5, 'C', [0x03B5]).					% GREEK LUNATE EPSILON SYMBOL
unicode_case_folding(0x03F7, 'C', [0x03F8]).					% GREEK CAPITAL LETTER SHO
unicode_case_folding(0x03F9, 'C', [0x03F2]).					% GREEK CAPITAL LUNATE SIGMA SYMBOL
unicode_case_folding(0x03FA, 'C', [0x03FB]).					% GREEK CAPITAL LETTER SAN
unicode_case_folding(0x03FD, 'C', [0x037B]).					% GREEK CAPITAL REVERSED LUNATE SIGMA SYMBOL
unicode_case_folding(0x03FE, 'C', [0x037C]).					% GREEK CAPITAL DOTTED LUNATE SIGMA SYMBOL
unicode_case_folding(0x03FF, 'C', [0x037D]).					% GREEK CAPITAL REVERSED DOTTED LUNATE SIGMA SYMBOL
unicode_case_folding(0x0400, 'C', [0x0450]).					% CYRILLIC CAPITAL LETTER IE WITH GRAVE
unicode_case_folding(0x0401, 'C', [0x0451]).					% CYRILLIC CAPITAL LETTER IO
unicode_case_folding(0x0402, 'C', [0x0452]).					% CYRILLIC CAPITAL LETTER DJE
unicode_case_folding(0x0403, 'C', [0x0453]).					% CYRILLIC CAPITAL LETTER GJE
unicode_case_folding(0x0404, 'C', [0x0454]).					% CYRILLIC CAPITAL LETTER UKRAINIAN IE
unicode_case_folding(0x0405, 'C', [0x0455]).					% CYRILLIC CAPITAL LETTER DZE
unicode_case_folding(0x0406, 'C', [0x0456]).					% CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
unicode_case_folding(0x0407, 'C', [0x0457]).					% CYRILLIC CAPITAL LETTER YI
unicode_case_folding(0x0408, 'C', [0x0458]).					% CYRILLIC CAPITAL LETTER JE
unicode_case_folding(0x0409, 'C', [0x0459]).					% CYRILLIC CAPITAL LETTER LJE
unicode_case_folding(0x040A, 'C', [0x045A]).					% CYRILLIC CAPITAL LETTER NJE
unicode_case_folding(0x040B, 'C', [0x045B]).					% CYRILLIC CAPITAL LETTER TSHE
unicode_case_folding(0x040C, 'C', [0x045C]).					% CYRILLIC CAPITAL LETTER KJE
unicode_case_folding(0x040D, 'C', [0x045D]).					% CYRILLIC CAPITAL LETTER I WITH GRAVE
unicode_case_folding(0x040E, 'C', [0x045E]).					% CYRILLIC CAPITAL LETTER SHORT U
unicode_case_folding(0x040F, 'C', [0x045F]).					% CYRILLIC CAPITAL LETTER DZHE
unicode_case_folding(0x0410, 'C', [0x0430]).					% CYRILLIC CAPITAL LETTER A
unicode_case_folding(0x0411, 'C', [0x0431]).					% CYRILLIC CAPITAL LETTER BE
unicode_case_folding(0x0412, 'C', [0x0432]).					% CYRILLIC CAPITAL LETTER VE
unicode_case_folding(0x0413, 'C', [0x0433]).					% CYRILLIC CAPITAL LETTER GHE
unicode_case_folding(0x0414, 'C', [0x0434]).					% CYRILLIC CAPITAL LETTER DE
unicode_case_folding(0x0415, 'C', [0x0435]).					% CYRILLIC CAPITAL LETTER IE
unicode_case_folding(0x0416, 'C', [0x0436]).					% CYRILLIC CAPITAL LETTER ZHE
unicode_case_folding(0x0417, 'C', [0x0437]).					% CYRILLIC CAPITAL LETTER ZE
unicode_case_folding(0x0418, 'C', [0x0438]).					% CYRILLIC CAPITAL LETTER I
unicode_case_folding(0x0419, 'C', [0x0439]).					% CYRILLIC CAPITAL LETTER SHORT I
unicode_case_folding(0x041A, 'C', [0x043A]).					% CYRILLIC CAPITAL LETTER KA
unicode_case_folding(0x041B, 'C', [0x043B]).					% CYRILLIC CAPITAL LETTER EL
unicode_case_folding(0x041C, 'C', [0x043C]).					% CYRILLIC CAPITAL LETTER EM
unicode_case_folding(0x041D, 'C', [0x043D]).					% CYRILLIC CAPITAL LETTER EN
unicode_case_folding(0x041E, 'C', [0x043E]).					% CYRILLIC CAPITAL LETTER O
unicode_case_folding(0x041F, 'C', [0x043F]).					% CYRILLIC CAPITAL LETTER PE
unicode_case_folding(0x0420, 'C', [0x0440]).					% CYRILLIC CAPITAL LETTER ER
unicode_case_folding(0x0421, 'C', [0x0441]).					% CYRILLIC CAPITAL LETTER ES
unicode_case_folding(0x0422, 'C', [0x0442]).					% CYRILLIC CAPITAL LETTER TE
unicode_case_folding(0x0423, 'C', [0x0443]).					% CYRILLIC CAPITAL LETTER U
unicode_case_folding(0x0424, 'C', [0x0444]).					% CYRILLIC CAPITAL LETTER EF
unicode_case_folding(0x0425, 'C', [0x0445]).					% CYRILLIC CAPITAL LETTER HA
unicode_case_folding(0x0426, 'C', [0x0446]).					% CYRILLIC CAPITAL LETTER TSE
unicode_case_folding(0x0427, 'C', [0x0447]).					% CYRILLIC CAPITAL LETTER CHE
unicode_case_folding(0x0428, 'C', [0x0448]).					% CYRILLIC CAPITAL LETTER SHA
unicode_case_folding(0x0429, 'C', [0x0449]).					% CYRILLIC CAPITAL LETTER SHCHA
unicode_case_folding(0x042A, 'C', [0x044A]).					% CYRILLIC CAPITAL LETTER HARD SIGN
unicode_case_folding(0x042B, 'C', [0x044B]).					% CYRILLIC CAPITAL LETTER YERU
unicode_case_folding(0x042C, 'C', [0x044C]).					% CYRILLIC CAPITAL LETTER SOFT SIGN
unicode_case_folding(0x042D, 'C', [0x044D]).					% CYRILLIC CAPITAL LETTER E
unicode_case_folding(0x042E, 'C', [0x044E]).					% CYRILLIC CAPITAL LETTER YU
unicode_case_folding(0x042F, 'C', [0x044F]).					% CYRILLIC CAPITAL LETTER YA
unicode_case_folding(0x0460, 'C', [0x0461]).					% CYRILLIC CAPITAL LETTER OMEGA
unicode_case_folding(0x0462, 'C', [0x0463]).					% CYRILLIC CAPITAL LETTER YAT
unicode_case_folding(0x0464, 'C', [0x0465]).					% CYRILLIC CAPITAL LETTER IOTIFIED E
unicode_case_folding(0x0466, 'C', [0x0467]).					% CYRILLIC CAPITAL LETTER LITTLE YUS
unicode_case_folding(0x0468, 'C', [0x0469]).					% CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
unicode_case_folding(0x046A, 'C', [0x046B]).					% CYRILLIC CAPITAL LETTER BIG YUS
unicode_case_folding(0x046C, 'C', [0x046D]).					% CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
unicode_case_folding(0x046E, 'C', [0x046F]).					% CYRILLIC CAPITAL LETTER KSI
unicode_case_folding(0x0470, 'C', [0x0471]).					% CYRILLIC CAPITAL LETTER PSI
unicode_case_folding(0x0472, 'C', [0x0473]).					% CYRILLIC CAPITAL LETTER FITA
unicode_case_folding(0x0474, 'C', [0x0475]).					% CYRILLIC CAPITAL LETTER IZHITSA
unicode_case_folding(0x0476, 'C', [0x0477]).					% CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
unicode_case_folding(0x0478, 'C', [0x0479]).					% CYRILLIC CAPITAL LETTER UK
unicode_case_folding(0x047A, 'C', [0x047B]).					% CYRILLIC CAPITAL LETTER ROUND OMEGA
unicode_case_folding(0x047C, 'C', [0x047D]).					% CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
unicode_case_folding(0x047E, 'C', [0x047F]).					% CYRILLIC CAPITAL LETTER OT
unicode_case_folding(0x0480, 'C', [0x0481]).					% CYRILLIC CAPITAL LETTER KOPPA
unicode_case_folding(0x048A, 'C', [0x048B]).					% CYRILLIC CAPITAL LETTER SHORT I WITH TAIL
unicode_case_folding(0x048C, 'C', [0x048D]).					% CYRILLIC CAPITAL LETTER SEMISOFT SIGN
unicode_case_folding(0x048E, 'C', [0x048F]).					% CYRILLIC CAPITAL LETTER ER WITH TICK
unicode_case_folding(0x0490, 'C', [0x0491]).					% CYRILLIC CAPITAL LETTER GHE WITH UPTURN
unicode_case_folding(0x0492, 'C', [0x0493]).					% CYRILLIC CAPITAL LETTER GHE WITH STROKE
unicode_case_folding(0x0494, 'C', [0x0495]).					% CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
unicode_case_folding(0x0496, 'C', [0x0497]).					% CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
unicode_case_folding(0x0498, 'C', [0x0499]).					% CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
unicode_case_folding(0x049A, 'C', [0x049B]).					% CYRILLIC CAPITAL LETTER KA WITH DESCENDER
unicode_case_folding(0x049C, 'C', [0x049D]).					% CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
unicode_case_folding(0x049E, 'C', [0x049F]).					% CYRILLIC CAPITAL LETTER KA WITH STROKE
unicode_case_folding(0x04A0, 'C', [0x04A1]).					% CYRILLIC CAPITAL LETTER BASHKIR KA
unicode_case_folding(0x04A2, 'C', [0x04A3]).					% CYRILLIC CAPITAL LETTER EN WITH DESCENDER
unicode_case_folding(0x04A4, 'C', [0x04A5]).					% CYRILLIC CAPITAL LIGATURE EN GHE
unicode_case_folding(0x04A6, 'C', [0x04A7]).					% CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK
unicode_case_folding(0x04A8, 'C', [0x04A9]).					% CYRILLIC CAPITAL LETTER ABKHASIAN HA
unicode_case_folding(0x04AA, 'C', [0x04AB]).					% CYRILLIC CAPITAL LETTER ES WITH DESCENDER
unicode_case_folding(0x04AC, 'C', [0x04AD]).					% CYRILLIC CAPITAL LETTER TE WITH DESCENDER
unicode_case_folding(0x04AE, 'C', [0x04AF]).					% CYRILLIC CAPITAL LETTER STRAIGHT U
unicode_case_folding(0x04B0, 'C', [0x04B1]).					% CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
unicode_case_folding(0x04B2, 'C', [0x04B3]).					% CYRILLIC CAPITAL LETTER HA WITH DESCENDER
unicode_case_folding(0x04B4, 'C', [0x04B5]).					% CYRILLIC CAPITAL LIGATURE TE TSE
unicode_case_folding(0x04B6, 'C', [0x04B7]).					% CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
unicode_case_folding(0x04B8, 'C', [0x04B9]).					% CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
unicode_case_folding(0x04BA, 'C', [0x04BB]).					% CYRILLIC CAPITAL LETTER SHHA
unicode_case_folding(0x04BC, 'C', [0x04BD]).					% CYRILLIC CAPITAL LETTER ABKHASIAN CHE
unicode_case_folding(0x04BE, 'C', [0x04BF]).					% CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
unicode_case_folding(0x04C0, 'C', [0x04CF]).					% CYRILLIC LETTER PALOCHKA
unicode_case_folding(0x04C1, 'C', [0x04C2]).					% CYRILLIC CAPITAL LETTER ZHE WITH BREVE
unicode_case_folding(0x04C3, 'C', [0x04C4]).					% CYRILLIC CAPITAL LETTER KA WITH HOOK
unicode_case_folding(0x04C5, 'C', [0x04C6]).					% CYRILLIC CAPITAL LETTER EL WITH TAIL
unicode_case_folding(0x04C7, 'C', [0x04C8]).					% CYRILLIC CAPITAL LETTER EN WITH HOOK
unicode_case_folding(0x04C9, 'C', [0x04CA]).					% CYRILLIC CAPITAL LETTER EN WITH TAIL
unicode_case_folding(0x04CB, 'C', [0x04CC]).					% CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
unicode_case_folding(0x04CD, 'C', [0x04CE]).					% CYRILLIC CAPITAL LETTER EM WITH TAIL
unicode_case_folding(0x04D0, 'C', [0x04D1]).					% CYRILLIC CAPITAL LETTER A WITH BREVE
unicode_case_folding(0x04D2, 'C', [0x04D3]).					% CYRILLIC CAPITAL LETTER A WITH DIAERESIS
unicode_case_folding(0x04D4, 'C', [0x04D5]).					% CYRILLIC CAPITAL LIGATURE A IE
unicode_case_folding(0x04D6, 'C', [0x04D7]).					% CYRILLIC CAPITAL LETTER IE WITH BREVE
unicode_case_folding(0x04D8, 'C', [0x04D9]).					% CYRILLIC CAPITAL LETTER SCHWA
unicode_case_folding(0x04DA, 'C', [0x04DB]).					% CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
unicode_case_folding(0x04DC, 'C', [0x04DD]).					% CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
unicode_case_folding(0x04DE, 'C', [0x04DF]).					% CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
unicode_case_folding(0x04E0, 'C', [0x04E1]).					% CYRILLIC CAPITAL LETTER ABKHASIAN DZE
unicode_case_folding(0x04E2, 'C', [0x04E3]).					% CYRILLIC CAPITAL LETTER I WITH MACRON
unicode_case_folding(0x04E4, 'C', [0x04E5]).					% CYRILLIC CAPITAL LETTER I WITH DIAERESIS
unicode_case_folding(0x04E6, 'C', [0x04E7]).					% CYRILLIC CAPITAL LETTER O WITH DIAERESIS
unicode_case_folding(0x04E8, 'C', [0x04E9]).					% CYRILLIC CAPITAL LETTER BARRED O
unicode_case_folding(0x04EA, 'C', [0x04EB]).					% CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
unicode_case_folding(0x04EC, 'C', [0x04ED]).					% CYRILLIC CAPITAL LETTER E WITH DIAERESIS
unicode_case_folding(0x04EE, 'C', [0x04EF]).					% CYRILLIC CAPITAL LETTER U WITH MACRON
unicode_case_folding(0x04F0, 'C', [0x04F1]).					% CYRILLIC CAPITAL LETTER U WITH DIAERESIS
unicode_case_folding(0x04F2, 'C', [0x04F3]).					% CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
unicode_case_folding(0x04F4, 'C', [0x04F5]).					% CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
unicode_case_folding(0x04F6, 'C', [0x04F7]).					% CYRILLIC CAPITAL LETTER GHE WITH DESCENDER
unicode_case_folding(0x04F8, 'C', [0x04F9]).					% CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
unicode_case_folding(0x04FA, 'C', [0x04FB]).					% CYRILLIC CAPITAL LETTER GHE WITH STROKE AND HOOK
unicode_case_folding(0x04FC, 'C', [0x04FD]).					% CYRILLIC CAPITAL LETTER HA WITH HOOK
unicode_case_folding(0x04FE, 'C', [0x04FF]).					% CYRILLIC CAPITAL LETTER HA WITH STROKE
unicode_case_folding(0x0500, 'C', [0x0501]).					% CYRILLIC CAPITAL LETTER KOMI DE
unicode_case_folding(0x0502, 'C', [0x0503]).					% CYRILLIC CAPITAL LETTER KOMI DJE
unicode_case_folding(0x0504, 'C', [0x0505]).					% CYRILLIC CAPITAL LETTER KOMI ZJE
unicode_case_folding(0x0506, 'C', [0x0507]).					% CYRILLIC CAPITAL LETTER KOMI DZJE
unicode_case_folding(0x0508, 'C', [0x0509]).					% CYRILLIC CAPITAL LETTER KOMI LJE
unicode_case_folding(0x050A, 'C', [0x050B]).					% CYRILLIC CAPITAL LETTER KOMI NJE
unicode_case_folding(0x050C, 'C', [0x050D]).					% CYRILLIC CAPITAL LETTER KOMI SJE
unicode_case_folding(0x050E, 'C', [0x050F]).					% CYRILLIC CAPITAL LETTER KOMI TJE
unicode_case_folding(0x0510, 'C', [0x0511]).					% CYRILLIC CAPITAL LETTER REVERSED ZE
unicode_case_folding(0x0512, 'C', [0x0513]).					% CYRILLIC CAPITAL LETTER EL WITH HOOK
unicode_case_folding(0x0514, 'C', [0x0515]).					% CYRILLIC CAPITAL LETTER LHA
unicode_case_folding(0x0516, 'C', [0x0517]).					% CYRILLIC CAPITAL LETTER RHA
unicode_case_folding(0x0518, 'C', [0x0519]).					% CYRILLIC CAPITAL LETTER YAE
unicode_case_folding(0x051A, 'C', [0x051B]).					% CYRILLIC CAPITAL LETTER QA
unicode_case_folding(0x051C, 'C', [0x051D]).					% CYRILLIC CAPITAL LETTER WE
unicode_case_folding(0x051E, 'C', [0x051F]).					% CYRILLIC CAPITAL LETTER ALEUT KA
unicode_case_folding(0x0520, 'C', [0x0521]).					% CYRILLIC CAPITAL LETTER EL WITH MIDDLE HOOK
unicode_case_folding(0x0522, 'C', [0x0523]).					% CYRILLIC CAPITAL LETTER EN WITH MIDDLE HOOK
unicode_case_folding(0x0524, 'C', [0x0525]).					% CYRILLIC CAPITAL LETTER PE WITH DESCENDER
unicode_case_folding(0x0526, 'C', [0x0527]).					% CYRILLIC CAPITAL LETTER SHHA WITH DESCENDER
unicode_case_folding(0x0531, 'C', [0x0561]).					% ARMENIAN CAPITAL LETTER AYB
unicode_case_folding(0x0532, 'C', [0x0562]).					% ARMENIAN CAPITAL LETTER BEN
unicode_case_folding(0x0533, 'C', [0x0563]).					% ARMENIAN CAPITAL LETTER GIM
unicode_case_folding(0x0534, 'C', [0x0564]).					% ARMENIAN CAPITAL LETTER DA
unicode_case_folding(0x0535, 'C', [0x0565]).					% ARMENIAN CAPITAL LETTER ECH
unicode_case_folding(0x0536, 'C', [0x0566]).					% ARMENIAN CAPITAL LETTER ZA
unicode_case_folding(0x0537, 'C', [0x0567]).					% ARMENIAN CAPITAL LETTER EH
unicode_case_folding(0x0538, 'C', [0x0568]).					% ARMENIAN CAPITAL LETTER ET
unicode_case_folding(0x0539, 'C', [0x0569]).					% ARMENIAN CAPITAL LETTER TO
unicode_case_folding(0x053A, 'C', [0x056A]).					% ARMENIAN CAPITAL LETTER ZHE
unicode_case_folding(0x053B, 'C', [0x056B]).					% ARMENIAN CAPITAL LETTER INI
unicode_case_folding(0x053C, 'C', [0x056C]).					% ARMENIAN CAPITAL LETTER LIWN
unicode_case_folding(0x053D, 'C', [0x056D]).					% ARMENIAN CAPITAL LETTER XEH
unicode_case_folding(0x053E, 'C', [0x056E]).					% ARMENIAN CAPITAL LETTER CA
unicode_case_folding(0x053F, 'C', [0x056F]).					% ARMENIAN CAPITAL LETTER KEN
unicode_case_folding(0x0540, 'C', [0x0570]).					% ARMENIAN CAPITAL LETTER HO
unicode_case_folding(0x0541, 'C', [0x0571]).					% ARMENIAN CAPITAL LETTER JA
unicode_case_folding(0x0542, 'C', [0x0572]).					% ARMENIAN CAPITAL LETTER GHAD
unicode_case_folding(0x0543, 'C', [0x0573]).					% ARMENIAN CAPITAL LETTER CHEH
unicode_case_folding(0x0544, 'C', [0x0574]).					% ARMENIAN CAPITAL LETTER MEN
unicode_case_folding(0x0545, 'C', [0x0575]).					% ARMENIAN CAPITAL LETTER YI
unicode_case_folding(0x0546, 'C', [0x0576]).					% ARMENIAN CAPITAL LETTER NOW
unicode_case_folding(0x0547, 'C', [0x0577]).					% ARMENIAN CAPITAL LETTER SHA
unicode_case_folding(0x0548, 'C', [0x0578]).					% ARMENIAN CAPITAL LETTER VO
unicode_case_folding(0x0549, 'C', [0x0579]).					% ARMENIAN CAPITAL LETTER CHA
unicode_case_folding(0x054A, 'C', [0x057A]).					% ARMENIAN CAPITAL LETTER PEH
unicode_case_folding(0x054B, 'C', [0x057B]).					% ARMENIAN CAPITAL LETTER JHEH
unicode_case_folding(0x054C, 'C', [0x057C]).					% ARMENIAN CAPITAL LETTER RA
unicode_case_folding(0x054D, 'C', [0x057D]).					% ARMENIAN CAPITAL LETTER SEH
unicode_case_folding(0x054E, 'C', [0x057E]).					% ARMENIAN CAPITAL LETTER VEW
unicode_case_folding(0x054F, 'C', [0x057F]).					% ARMENIAN CAPITAL LETTER TIWN
unicode_case_folding(0x0550, 'C', [0x0580]).					% ARMENIAN CAPITAL LETTER REH
unicode_case_folding(0x0551, 'C', [0x0581]).					% ARMENIAN CAPITAL LETTER CO
unicode_case_folding(0x0552, 'C', [0x0582]).					% ARMENIAN CAPITAL LETTER YIWN
unicode_case_folding(0x0553, 'C', [0x0583]).					% ARMENIAN CAPITAL LETTER PIWR
unicode_case_folding(0x0554, 'C', [0x0584]).					% ARMENIAN CAPITAL LETTER KEH
unicode_case_folding(0x0555, 'C', [0x0585]).					% ARMENIAN CAPITAL LETTER OH
unicode_case_folding(0x0556, 'C', [0x0586]).					% ARMENIAN CAPITAL LETTER FEH
unicode_case_folding(0x0587, 'F', [0x0565, 0x0582]).			% ARMENIAN SMALL LIGATURE ECH YIWN
unicode_case_folding(0x10A0, 'C', [0x2D00]).					% GEORGIAN CAPITAL LETTER AN
unicode_case_folding(0x10A1, 'C', [0x2D01]).					% GEORGIAN CAPITAL LETTER BAN
unicode_case_folding(0x10A2, 'C', [0x2D02]).					% GEORGIAN CAPITAL LETTER GAN
unicode_case_folding(0x10A3, 'C', [0x2D03]).					% GEORGIAN CAPITAL LETTER DON
unicode_case_folding(0x10A4, 'C', [0x2D04]).					% GEORGIAN CAPITAL LETTER EN
unicode_case_folding(0x10A5, 'C', [0x2D05]).					% GEORGIAN CAPITAL LETTER VIN
unicode_case_folding(0x10A6, 'C', [0x2D06]).					% GEORGIAN CAPITAL LETTER ZEN
unicode_case_folding(0x10A7, 'C', [0x2D07]).					% GEORGIAN CAPITAL LETTER TAN
unicode_case_folding(0x10A8, 'C', [0x2D08]).					% GEORGIAN CAPITAL LETTER IN
unicode_case_folding(0x10A9, 'C', [0x2D09]).					% GEORGIAN CAPITAL LETTER KAN
unicode_case_folding(0x10AA, 'C', [0x2D0A]).					% GEORGIAN CAPITAL LETTER LAS
unicode_case_folding(0x10AB, 'C', [0x2D0B]).					% GEORGIAN CAPITAL LETTER MAN
unicode_case_folding(0x10AC, 'C', [0x2D0C]).					% GEORGIAN CAPITAL LETTER NAR
unicode_case_folding(0x10AD, 'C', [0x2D0D]).					% GEORGIAN CAPITAL LETTER ON
unicode_case_folding(0x10AE, 'C', [0x2D0E]).					% GEORGIAN CAPITAL LETTER PAR
unicode_case_folding(0x10AF, 'C', [0x2D0F]).					% GEORGIAN CAPITAL LETTER ZHAR
unicode_case_folding(0x10B0, 'C', [0x2D10]).					% GEORGIAN CAPITAL LETTER RAE
unicode_case_folding(0x10B1, 'C', [0x2D11]).					% GEORGIAN CAPITAL LETTER SAN
unicode_case_folding(0x10B2, 'C', [0x2D12]).					% GEORGIAN CAPITAL LETTER TAR
unicode_case_folding(0x10B3, 'C', [0x2D13]).					% GEORGIAN CAPITAL LETTER UN
unicode_case_folding(0x10B4, 'C', [0x2D14]).					% GEORGIAN CAPITAL LETTER PHAR
unicode_case_folding(0x10B5, 'C', [0x2D15]).					% GEORGIAN CAPITAL LETTER KHAR
unicode_case_folding(0x10B6, 'C', [0x2D16]).					% GEORGIAN CAPITAL LETTER GHAN
unicode_case_folding(0x10B7, 'C', [0x2D17]).					% GEORGIAN CAPITAL LETTER QAR
unicode_case_folding(0x10B8, 'C', [0x2D18]).					% GEORGIAN CAPITAL LETTER SHIN
unicode_case_folding(0x10B9, 'C', [0x2D19]).					% GEORGIAN CAPITAL LETTER CHIN
unicode_case_folding(0x10BA, 'C', [0x2D1A]).					% GEORGIAN CAPITAL LETTER CAN
unicode_case_folding(0x10BB, 'C', [0x2D1B]).					% GEORGIAN CAPITAL LETTER JIL
unicode_case_folding(0x10BC, 'C', [0x2D1C]).					% GEORGIAN CAPITAL LETTER CIL
unicode_case_folding(0x10BD, 'C', [0x2D1D]).					% GEORGIAN CAPITAL LETTER CHAR
unicode_case_folding(0x10BE, 'C', [0x2D1E]).					% GEORGIAN CAPITAL LETTER XAN
unicode_case_folding(0x10BF, 'C', [0x2D1F]).					% GEORGIAN CAPITAL LETTER JHAN
unicode_case_folding(0x10C0, 'C', [0x2D20]).					% GEORGIAN CAPITAL LETTER HAE
unicode_case_folding(0x10C1, 'C', [0x2D21]).					% GEORGIAN CAPITAL LETTER HE
unicode_case_folding(0x10C2, 'C', [0x2D22]).					% GEORGIAN CAPITAL LETTER HIE
unicode_case_folding(0x10C3, 'C', [0x2D23]).					% GEORGIAN CAPITAL LETTER WE
unicode_case_folding(0x10C4, 'C', [0x2D24]).					% GEORGIAN CAPITAL LETTER HAR
unicode_case_folding(0x10C5, 'C', [0x2D25]).					% GEORGIAN CAPITAL LETTER HOE
unicode_case_folding(0x10C7, 'C', [0x2D27]).					% GEORGIAN CAPITAL LETTER YN
unicode_case_folding(0x10CD, 'C', [0x2D2D]).					% GEORGIAN CAPITAL LETTER AEN
unicode_case_folding(0x1E00, 'C', [0x1E01]).					% LATIN CAPITAL LETTER A WITH RING BELOW
unicode_case_folding(0x1E02, 'C', [0x1E03]).					% LATIN CAPITAL LETTER B WITH DOT ABOVE
unicode_case_folding(0x1E04, 'C', [0x1E05]).					% LATIN CAPITAL LETTER B WITH DOT BELOW
unicode_case_folding(0x1E06, 'C', [0x1E07]).					% LATIN CAPITAL LETTER B WITH LINE BELOW
unicode_case_folding(0x1E08, 'C', [0x1E09]).					% LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
unicode_case_folding(0x1E0A, 'C', [0x1E0B]).					% LATIN CAPITAL LETTER D WITH DOT ABOVE
unicode_case_folding(0x1E0C, 'C', [0x1E0D]).					% LATIN CAPITAL LETTER D WITH DOT BELOW
unicode_case_folding(0x1E0E, 'C', [0x1E0F]).					% LATIN CAPITAL LETTER D WITH LINE BELOW
unicode_case_folding(0x1E10, 'C', [0x1E11]).					% LATIN CAPITAL LETTER D WITH CEDILLA
unicode_case_folding(0x1E12, 'C', [0x1E13]).					% LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
unicode_case_folding(0x1E14, 'C', [0x1E15]).					% LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
unicode_case_folding(0x1E16, 'C', [0x1E17]).					% LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
unicode_case_folding(0x1E18, 'C', [0x1E19]).					% LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
unicode_case_folding(0x1E1A, 'C', [0x1E1B]).					% LATIN CAPITAL LETTER E WITH TILDE BELOW
unicode_case_folding(0x1E1C, 'C', [0x1E1D]).					% LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
unicode_case_folding(0x1E1E, 'C', [0x1E1F]).					% LATIN CAPITAL LETTER F WITH DOT ABOVE
unicode_case_folding(0x1E20, 'C', [0x1E21]).					% LATIN CAPITAL LETTER G WITH MACRON
unicode_case_folding(0x1E22, 'C', [0x1E23]).					% LATIN CAPITAL LETTER H WITH DOT ABOVE
unicode_case_folding(0x1E24, 'C', [0x1E25]).					% LATIN CAPITAL LETTER H WITH DOT BELOW
unicode_case_folding(0x1E26, 'C', [0x1E27]).					% LATIN CAPITAL LETTER H WITH DIAERESIS
unicode_case_folding(0x1E28, 'C', [0x1E29]).					% LATIN CAPITAL LETTER H WITH CEDILLA
unicode_case_folding(0x1E2A, 'C', [0x1E2B]).					% LATIN CAPITAL LETTER H WITH BREVE BELOW
unicode_case_folding(0x1E2C, 'C', [0x1E2D]).					% LATIN CAPITAL LETTER I WITH TILDE BELOW
unicode_case_folding(0x1E2E, 'C', [0x1E2F]).					% LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
unicode_case_folding(0x1E30, 'C', [0x1E31]).					% LATIN CAPITAL LETTER K WITH ACUTE
unicode_case_folding(0x1E32, 'C', [0x1E33]).					% LATIN CAPITAL LETTER K WITH DOT BELOW
unicode_case_folding(0x1E34, 'C', [0x1E35]).					% LATIN CAPITAL LETTER K WITH LINE BELOW
unicode_case_folding(0x1E36, 'C', [0x1E37]).					% LATIN CAPITAL LETTER L WITH DOT BELOW
unicode_case_folding(0x1E38, 'C', [0x1E39]).					% LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
unicode_case_folding(0x1E3A, 'C', [0x1E3B]).					% LATIN CAPITAL LETTER L WITH LINE BELOW
unicode_case_folding(0x1E3C, 'C', [0x1E3D]).					% LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
unicode_case_folding(0x1E3E, 'C', [0x1E3F]).					% LATIN CAPITAL LETTER M WITH ACUTE
unicode_case_folding(0x1E40, 'C', [0x1E41]).					% LATIN CAPITAL LETTER M WITH DOT ABOVE
unicode_case_folding(0x1E42, 'C', [0x1E43]).					% LATIN CAPITAL LETTER M WITH DOT BELOW
unicode_case_folding(0x1E44, 'C', [0x1E45]).					% LATIN CAPITAL LETTER N WITH DOT ABOVE
unicode_case_folding(0x1E46, 'C', [0x1E47]).					% LATIN CAPITAL LETTER N WITH DOT BELOW
unicode_case_folding(0x1E48, 'C', [0x1E49]).					% LATIN CAPITAL LETTER N WITH LINE BELOW
unicode_case_folding(0x1E4A, 'C', [0x1E4B]).					% LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
unicode_case_folding(0x1E4C, 'C', [0x1E4D]).					% LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
unicode_case_folding(0x1E4E, 'C', [0x1E4F]).					% LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
unicode_case_folding(0x1E50, 'C', [0x1E51]).					% LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
unicode_case_folding(0x1E52, 'C', [0x1E53]).					% LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
unicode_case_folding(0x1E54, 'C', [0x1E55]).					% LATIN CAPITAL LETTER P WITH ACUTE
unicode_case_folding(0x1E56, 'C', [0x1E57]).					% LATIN CAPITAL LETTER P WITH DOT ABOVE
unicode_case_folding(0x1E58, 'C', [0x1E59]).					% LATIN CAPITAL LETTER R WITH DOT ABOVE
unicode_case_folding(0x1E5A, 'C', [0x1E5B]).					% LATIN CAPITAL LETTER R WITH DOT BELOW
unicode_case_folding(0x1E5C, 'C', [0x1E5D]).					% LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
unicode_case_folding(0x1E5E, 'C', [0x1E5F]).					% LATIN CAPITAL LETTER R WITH LINE BELOW
unicode_case_folding(0x1E60, 'C', [0x1E61]).					% LATIN CAPITAL LETTER S WITH DOT ABOVE
unicode_case_folding(0x1E62, 'C', [0x1E63]).					% LATIN CAPITAL LETTER S WITH DOT BELOW
unicode_case_folding(0x1E64, 'C', [0x1E65]).					% LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
unicode_case_folding(0x1E66, 'C', [0x1E67]).					% LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
unicode_case_folding(0x1E68, 'C', [0x1E69]).					% LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
unicode_case_folding(0x1E6A, 'C', [0x1E6B]).					% LATIN CAPITAL LETTER T WITH DOT ABOVE
unicode_case_folding(0x1E6C, 'C', [0x1E6D]).					% LATIN CAPITAL LETTER T WITH DOT BELOW
unicode_case_folding(0x1E6E, 'C', [0x1E6F]).					% LATIN CAPITAL LETTER T WITH LINE BELOW
unicode_case_folding(0x1E70, 'C', [0x1E71]).					% LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
unicode_case_folding(0x1E72, 'C', [0x1E73]).					% LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
unicode_case_folding(0x1E74, 'C', [0x1E75]).					% LATIN CAPITAL LETTER U WITH TILDE BELOW
unicode_case_folding(0x1E76, 'C', [0x1E77]).					% LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
unicode_case_folding(0x1E78, 'C', [0x1E79]).					% LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
unicode_case_folding(0x1E7A, 'C', [0x1E7B]).					% LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
unicode_case_folding(0x1E7C, 'C', [0x1E7D]).					% LATIN CAPITAL LETTER V WITH TILDE
unicode_case_folding(0x1E7E, 'C', [0x1E7F]).					% LATIN CAPITAL LETTER V WITH DOT BELOW
unicode_case_folding(0x1E80, 'C', [0x1E81]).					% LATIN CAPITAL LETTER W WITH GRAVE
unicode_case_folding(0x1E82, 'C', [0x1E83]).					% LATIN CAPITAL LETTER W WITH ACUTE
unicode_case_folding(0x1E84, 'C', [0x1E85]).					% LATIN CAPITAL LETTER W WITH DIAERESIS
unicode_case_folding(0x1E86, 'C', [0x1E87]).					% LATIN CAPITAL LETTER W WITH DOT ABOVE
unicode_case_folding(0x1E88, 'C', [0x1E89]).					% LATIN CAPITAL LETTER W WITH DOT BELOW
unicode_case_folding(0x1E8A, 'C', [0x1E8B]).					% LATIN CAPITAL LETTER X WITH DOT ABOVE
unicode_case_folding(0x1E8C, 'C', [0x1E8D]).					% LATIN CAPITAL LETTER X WITH DIAERESIS
unicode_case_folding(0x1E8E, 'C', [0x1E8F]).					% LATIN CAPITAL LETTER Y WITH DOT ABOVE
unicode_case_folding(0x1E90, 'C', [0x1E91]).					% LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
unicode_case_folding(0x1E92, 'C', [0x1E93]).					% LATIN CAPITAL LETTER Z WITH DOT BELOW
unicode_case_folding(0x1E94, 'C', [0x1E95]).					% LATIN CAPITAL LETTER Z WITH LINE BELOW
unicode_case_folding(0x1E96, 'F', [0x0068, 0x0331]).			% LATIN SMALL LETTER H WITH LINE BELOW
unicode_case_folding(0x1E97, 'F', [0x0074, 0x0308]).			% LATIN SMALL LETTER T WITH DIAERESIS
unicode_case_folding(0x1E98, 'F', [0x0077, 0x030A]).			% LATIN SMALL LETTER W WITH RING ABOVE
unicode_case_folding(0x1E99, 'F', [0x0079, 0x030A]).			% LATIN SMALL LETTER Y WITH RING ABOVE
unicode_case_folding(0x1E9A, 'F', [0x0061, 0x02BE]).			% LATIN SMALL LETTER A WITH RIGHT HALF RING
unicode_case_folding(0x1E9B, 'C', [0x1E61]).					% LATIN SMALL LETTER LONG S WITH DOT ABOVE
unicode_case_folding(0x1E9E, 'F', [0x0073, 0x0073]).			% LATIN CAPITAL LETTER SHARP S
unicode_case_folding(0x1E9E, 'S', [0x00DF]).					% LATIN CAPITAL LETTER SHARP S
unicode_case_folding(0x1EA0, 'C', [0x1EA1]).					% LATIN CAPITAL LETTER A WITH DOT BELOW
unicode_case_folding(0x1EA2, 'C', [0x1EA3]).					% LATIN CAPITAL LETTER A WITH HOOK ABOVE
unicode_case_folding(0x1EA4, 'C', [0x1EA5]).					% LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
unicode_case_folding(0x1EA6, 'C', [0x1EA7]).					% LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
unicode_case_folding(0x1EA8, 'C', [0x1EA9]).					% LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
unicode_case_folding(0x1EAA, 'C', [0x1EAB]).					% LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
unicode_case_folding(0x1EAC, 'C', [0x1EAD]).					% LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
unicode_case_folding(0x1EAE, 'C', [0x1EAF]).					% LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
unicode_case_folding(0x1EB0, 'C', [0x1EB1]).					% LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
unicode_case_folding(0x1EB2, 'C', [0x1EB3]).					% LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
unicode_case_folding(0x1EB4, 'C', [0x1EB5]).					% LATIN CAPITAL LETTER A WITH BREVE AND TILDE
unicode_case_folding(0x1EB6, 'C', [0x1EB7]).					% LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
unicode_case_folding(0x1EB8, 'C', [0x1EB9]).					% LATIN CAPITAL LETTER E WITH DOT BELOW
unicode_case_folding(0x1EBA, 'C', [0x1EBB]).					% LATIN CAPITAL LETTER E WITH HOOK ABOVE
unicode_case_folding(0x1EBC, 'C', [0x1EBD]).					% LATIN CAPITAL LETTER E WITH TILDE
unicode_case_folding(0x1EBE, 'C', [0x1EBF]).					% LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
unicode_case_folding(0x1EC0, 'C', [0x1EC1]).					% LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
unicode_case_folding(0x1EC2, 'C', [0x1EC3]).					% LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
unicode_case_folding(0x1EC4, 'C', [0x1EC5]).					% LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
unicode_case_folding(0x1EC6, 'C', [0x1EC7]).					% LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
unicode_case_folding(0x1EC8, 'C', [0x1EC9]).					% LATIN CAPITAL LETTER I WITH HOOK ABOVE
unicode_case_folding(0x1ECA, 'C', [0x1ECB]).					% LATIN CAPITAL LETTER I WITH DOT BELOW
unicode_case_folding(0x1ECC, 'C', [0x1ECD]).					% LATIN CAPITAL LETTER O WITH DOT BELOW
unicode_case_folding(0x1ECE, 'C', [0x1ECF]).					% LATIN CAPITAL LETTER O WITH HOOK ABOVE
unicode_case_folding(0x1ED0, 'C', [0x1ED1]).					% LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
unicode_case_folding(0x1ED2, 'C', [0x1ED3]).					% LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
unicode_case_folding(0x1ED4, 'C', [0x1ED5]).					% LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
unicode_case_folding(0x1ED6, 'C', [0x1ED7]).					% LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
unicode_case_folding(0x1ED8, 'C', [0x1ED9]).					% LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
unicode_case_folding(0x1EDA, 'C', [0x1EDB]).					% LATIN CAPITAL LETTER O WITH HORN AND ACUTE
unicode_case_folding(0x1EDC, 'C', [0x1EDD]).					% LATIN CAPITAL LETTER O WITH HORN AND GRAVE
unicode_case_folding(0x1EDE, 'C', [0x1EDF]).					% LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
unicode_case_folding(0x1EE0, 'C', [0x1EE1]).					% LATIN CAPITAL LETTER O WITH HORN AND TILDE
unicode_case_folding(0x1EE2, 'C', [0x1EE3]).					% LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
unicode_case_folding(0x1EE4, 'C', [0x1EE5]).					% LATIN CAPITAL LETTER U WITH DOT BELOW
unicode_case_folding(0x1EE6, 'C', [0x1EE7]).					% LATIN CAPITAL LETTER U WITH HOOK ABOVE
unicode_case_folding(0x1EE8, 'C', [0x1EE9]).					% LATIN CAPITAL LETTER U WITH HORN AND ACUTE
unicode_case_folding(0x1EEA, 'C', [0x1EEB]).					% LATIN CAPITAL LETTER U WITH HORN AND GRAVE
unicode_case_folding(0x1EEC, 'C', [0x1EED]).					% LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
unicode_case_folding(0x1EEE, 'C', [0x1EEF]).					% LATIN CAPITAL LETTER U WITH HORN AND TILDE
unicode_case_folding(0x1EF0, 'C', [0x1EF1]).					% LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
unicode_case_folding(0x1EF2, 'C', [0x1EF3]).					% LATIN CAPITAL LETTER Y WITH GRAVE
unicode_case_folding(0x1EF4, 'C', [0x1EF5]).					% LATIN CAPITAL LETTER Y WITH DOT BELOW
unicode_case_folding(0x1EF6, 'C', [0x1EF7]).					% LATIN CAPITAL LETTER Y WITH HOOK ABOVE
unicode_case_folding(0x1EF8, 'C', [0x1EF9]).					% LATIN CAPITAL LETTER Y WITH TILDE
unicode_case_folding(0x1EFA, 'C', [0x1EFB]).					% LATIN CAPITAL LETTER MIDDLE-WELSH LL
unicode_case_folding(0x1EFC, 'C', [0x1EFD]).					% LATIN CAPITAL LETTER MIDDLE-WELSH V
unicode_case_folding(0x1EFE, 'C', [0x1EFF]).					% LATIN CAPITAL LETTER Y WITH LOOP
unicode_case_folding(0x1F08, 'C', [0x1F00]).					% GREEK CAPITAL LETTER ALPHA WITH PSILI
unicode_case_folding(0x1F09, 'C', [0x1F01]).					% GREEK CAPITAL LETTER ALPHA WITH DASIA
unicode_case_folding(0x1F0A, 'C', [0x1F02]).					% GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
unicode_case_folding(0x1F0B, 'C', [0x1F03]).					% GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
unicode_case_folding(0x1F0C, 'C', [0x1F04]).					% GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
unicode_case_folding(0x1F0D, 'C', [0x1F05]).					% GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
unicode_case_folding(0x1F0E, 'C', [0x1F06]).					% GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
unicode_case_folding(0x1F0F, 'C', [0x1F07]).					% GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
unicode_case_folding(0x1F18, 'C', [0x1F10]).					% GREEK CAPITAL LETTER EPSILON WITH PSILI
unicode_case_folding(0x1F19, 'C', [0x1F11]).					% GREEK CAPITAL LETTER EPSILON WITH DASIA
unicode_case_folding(0x1F1A, 'C', [0x1F12]).					% GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
unicode_case_folding(0x1F1B, 'C', [0x1F13]).					% GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
unicode_case_folding(0x1F1C, 'C', [0x1F14]).					% GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
unicode_case_folding(0x1F1D, 'C', [0x1F15]).					% GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_case_folding(0x1F28, 'C', [0x1F20]).					% GREEK CAPITAL LETTER ETA WITH PSILI
unicode_case_folding(0x1F29, 'C', [0x1F21]).					% GREEK CAPITAL LETTER ETA WITH DASIA
unicode_case_folding(0x1F2A, 'C', [0x1F22]).					% GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
unicode_case_folding(0x1F2B, 'C', [0x1F23]).					% GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
unicode_case_folding(0x1F2C, 'C', [0x1F24]).					% GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
unicode_case_folding(0x1F2D, 'C', [0x1F25]).					% GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
unicode_case_folding(0x1F2E, 'C', [0x1F26]).					% GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
unicode_case_folding(0x1F2F, 'C', [0x1F27]).					% GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
unicode_case_folding(0x1F38, 'C', [0x1F30]).					% GREEK CAPITAL LETTER IOTA WITH PSILI
unicode_case_folding(0x1F39, 'C', [0x1F31]).					% GREEK CAPITAL LETTER IOTA WITH DASIA
unicode_case_folding(0x1F3A, 'C', [0x1F32]).					% GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
unicode_case_folding(0x1F3B, 'C', [0x1F33]).					% GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
unicode_case_folding(0x1F3C, 'C', [0x1F34]).					% GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
unicode_case_folding(0x1F3D, 'C', [0x1F35]).					% GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
unicode_case_folding(0x1F3E, 'C', [0x1F36]).					% GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
unicode_case_folding(0x1F3F, 'C', [0x1F37]).					% GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
unicode_case_folding(0x1F48, 'C', [0x1F40]).					% GREEK CAPITAL LETTER OMICRON WITH PSILI
unicode_case_folding(0x1F49, 'C', [0x1F41]).					% GREEK CAPITAL LETTER OMICRON WITH DASIA
unicode_case_folding(0x1F4A, 'C', [0x1F42]).					% GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
unicode_case_folding(0x1F4B, 'C', [0x1F43]).					% GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
unicode_case_folding(0x1F4C, 'C', [0x1F44]).					% GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
unicode_case_folding(0x1F4D, 'C', [0x1F45]).					% GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_case_folding(0x1F50, 'F', [0x03C5, 0x0313]).			% GREEK SMALL LETTER UPSILON WITH PSILI
unicode_case_folding(0x1F52, 'F', [0x03C5, 0x0313, 0x0300]).	% GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
unicode_case_folding(0x1F54, 'F', [0x03C5, 0x0313, 0x0301]).	% GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
unicode_case_folding(0x1F56, 'F', [0x03C5, 0x0313, 0x0342]).	% GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
unicode_case_folding(0x1F59, 'C', [0x1F51]).					% GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_case_folding(0x1F5B, 'C', [0x1F53]).					% GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_case_folding(0x1F5D, 'C', [0x1F55]).					% GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_case_folding(0x1F5F, 'C', [0x1F57]).					% GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_case_folding(0x1F68, 'C', [0x1F60]).					% GREEK CAPITAL LETTER OMEGA WITH PSILI
unicode_case_folding(0x1F69, 'C', [0x1F61]).					% GREEK CAPITAL LETTER OMEGA WITH DASIA
unicode_case_folding(0x1F6A, 'C', [0x1F62]).					% GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
unicode_case_folding(0x1F6B, 'C', [0x1F63]).					% GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
unicode_case_folding(0x1F6C, 'C', [0x1F64]).					% GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
unicode_case_folding(0x1F6D, 'C', [0x1F65]).					% GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
unicode_case_folding(0x1F6E, 'C', [0x1F66]).					% GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
unicode_case_folding(0x1F6F, 'C', [0x1F67]).					% GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
unicode_case_folding(0x1F80, 'F', [0x1F00, 0x03B9]).			% GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
unicode_case_folding(0x1F81, 'F', [0x1F01, 0x03B9]).			% GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
unicode_case_folding(0x1F82, 'F', [0x1F02, 0x03B9]).			% GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
unicode_case_folding(0x1F83, 'F', [0x1F03, 0x03B9]).			% GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
unicode_case_folding(0x1F84, 'F', [0x1F04, 0x03B9]).			% GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
unicode_case_folding(0x1F85, 'F', [0x1F05, 0x03B9]).			% GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
unicode_case_folding(0x1F86, 'F', [0x1F06, 0x03B9]).			% GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
unicode_case_folding(0x1F87, 'F', [0x1F07, 0x03B9]).			% GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_case_folding(0x1F88, 'F', [0x1F00, 0x03B9]).			% GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
unicode_case_folding(0x1F88, 'S', [0x1F80]).					% GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
unicode_case_folding(0x1F89, 'F', [0x1F01, 0x03B9]).			% GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F89, 'S', [0x1F81]).					% GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F8A, 'F', [0x1F02, 0x03B9]).			% GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F8A, 'S', [0x1F82]).					% GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F8B, 'F', [0x1F03, 0x03B9]).			% GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F8B, 'S', [0x1F83]).					% GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F8C, 'F', [0x1F04, 0x03B9]).			% GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F8C, 'S', [0x1F84]).					% GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F8D, 'F', [0x1F05, 0x03B9]).			% GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F8D, 'S', [0x1F85]).					% GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F8E, 'F', [0x1F06, 0x03B9]).			% GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
unicode_case_folding(0x1F8E, 'S', [0x1F86]).					% GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
unicode_case_folding(0x1F8F, 'F', [0x1F07, 0x03B9]).			% GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_case_folding(0x1F8F, 'S', [0x1F87]).					% GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_case_folding(0x1F90, 'F', [0x1F20, 0x03B9]).			% GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
unicode_case_folding(0x1F91, 'F', [0x1F21, 0x03B9]).			% GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
unicode_case_folding(0x1F92, 'F', [0x1F22, 0x03B9]).			% GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
unicode_case_folding(0x1F93, 'F', [0x1F23, 0x03B9]).			% GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
unicode_case_folding(0x1F94, 'F', [0x1F24, 0x03B9]).			% GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
unicode_case_folding(0x1F95, 'F', [0x1F25, 0x03B9]).			% GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
unicode_case_folding(0x1F96, 'F', [0x1F26, 0x03B9]).			% GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
unicode_case_folding(0x1F97, 'F', [0x1F27, 0x03B9]).			% GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_case_folding(0x1F98, 'F', [0x1F20, 0x03B9]).			% GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
unicode_case_folding(0x1F98, 'S', [0x1F90]).					% GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
unicode_case_folding(0x1F99, 'F', [0x1F21, 0x03B9]).			% GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F99, 'S', [0x1F91]).					% GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F9A, 'F', [0x1F22, 0x03B9]).			% GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F9A, 'S', [0x1F92]).					% GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F9B, 'F', [0x1F23, 0x03B9]).			% GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F9B, 'S', [0x1F93]).					% GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F9C, 'F', [0x1F24, 0x03B9]).			% GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F9C, 'S', [0x1F94]).					% GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F9D, 'F', [0x1F25, 0x03B9]).			% GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F9D, 'S', [0x1F95]).					% GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
unicode_case_folding(0x1F9E, 'F', [0x1F26, 0x03B9]).			% GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
unicode_case_folding(0x1F9E, 'S', [0x1F96]).					% GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
unicode_case_folding(0x1F9F, 'F', [0x1F27, 0x03B9]).			% GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_case_folding(0x1F9F, 'S', [0x1F97]).					% GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_case_folding(0x1FA0, 'F', [0x1F60, 0x03B9]).			% GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
unicode_case_folding(0x1FA1, 'F', [0x1F61, 0x03B9]).			% GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
unicode_case_folding(0x1FA2, 'F', [0x1F62, 0x03B9]).			% GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
unicode_case_folding(0x1FA3, 'F', [0x1F63, 0x03B9]).			% GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
unicode_case_folding(0x1FA4, 'F', [0x1F64, 0x03B9]).			% GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
unicode_case_folding(0x1FA5, 'F', [0x1F65, 0x03B9]).			% GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
unicode_case_folding(0x1FA6, 'F', [0x1F66, 0x03B9]).			% GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
unicode_case_folding(0x1FA7, 'F', [0x1F67, 0x03B9]).			% GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_case_folding(0x1FA8, 'F', [0x1F60, 0x03B9]).			% GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
unicode_case_folding(0x1FA8, 'S', [0x1FA0]).					% GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
unicode_case_folding(0x1FA9, 'F', [0x1F61, 0x03B9]).			% GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
unicode_case_folding(0x1FA9, 'S', [0x1FA1]).					% GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
unicode_case_folding(0x1FAA, 'F', [0x1F62, 0x03B9]).			% GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
unicode_case_folding(0x1FAA, 'S', [0x1FA2]).					% GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
unicode_case_folding(0x1FAB, 'F', [0x1F63, 0x03B9]).			% GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
unicode_case_folding(0x1FAB, 'S', [0x1FA3]).					% GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
unicode_case_folding(0x1FAC, 'F', [0x1F64, 0x03B9]).			% GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
unicode_case_folding(0x1FAC, 'S', [0x1FA4]).					% GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
unicode_case_folding(0x1FAD, 'F', [0x1F65, 0x03B9]).			% GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
unicode_case_folding(0x1FAD, 'S', [0x1FA5]).					% GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
unicode_case_folding(0x1FAE, 'F', [0x1F66, 0x03B9]).			% GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
unicode_case_folding(0x1FAE, 'S', [0x1FA6]).					% GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
unicode_case_folding(0x1FAF, 'F', [0x1F67, 0x03B9]).			% GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_case_folding(0x1FAF, 'S', [0x1FA7]).					% GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_case_folding(0x1FB2, 'F', [0x1F70, 0x03B9]).			% GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
unicode_case_folding(0x1FB3, 'F', [0x03B1, 0x03B9]).			% GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
unicode_case_folding(0x1FB4, 'F', [0x03AC, 0x03B9]).			% GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_case_folding(0x1FB6, 'F', [0x03B1, 0x0342]).			% GREEK SMALL LETTER ALPHA WITH PERISPOMENI
unicode_case_folding(0x1FB7, 'F', [0x03B1, 0x0342, 0x03B9]).	% GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_case_folding(0x1FB8, 'C', [0x1FB0]).					% GREEK CAPITAL LETTER ALPHA WITH VRACHY
unicode_case_folding(0x1FB9, 'C', [0x1FB1]).					% GREEK CAPITAL LETTER ALPHA WITH MACRON
unicode_case_folding(0x1FBA, 'C', [0x1F70]).					% GREEK CAPITAL LETTER ALPHA WITH VARIA
unicode_case_folding(0x1FBB, 'C', [0x1F71]).					% GREEK CAPITAL LETTER ALPHA WITH OXIA
unicode_case_folding(0x1FBC, 'F', [0x03B1, 0x03B9]).			% GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_case_folding(0x1FBC, 'S', [0x1FB3]).					% GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_case_folding(0x1FBE, 'C', [0x03B9]).					% GREEK PROSGEGRAMMENI
unicode_case_folding(0x1FC2, 'F', [0x1F74, 0x03B9]).			% GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
unicode_case_folding(0x1FC3, 'F', [0x03B7, 0x03B9]).			% GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
unicode_case_folding(0x1FC4, 'F', [0x03AE, 0x03B9]).			% GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_case_folding(0x1FC6, 'F', [0x03B7, 0x0342]).			% GREEK SMALL LETTER ETA WITH PERISPOMENI
unicode_case_folding(0x1FC7, 'F', [0x03B7, 0x0342, 0x03B9]).	% GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_case_folding(0x1FC8, 'C', [0x1F72]).					% GREEK CAPITAL LETTER EPSILON WITH VARIA
unicode_case_folding(0x1FC9, 'C', [0x1F73]).					% GREEK CAPITAL LETTER EPSILON WITH OXIA
unicode_case_folding(0x1FCA, 'C', [0x1F74]).					% GREEK CAPITAL LETTER ETA WITH VARIA
unicode_case_folding(0x1FCB, 'C', [0x1F75]).					% GREEK CAPITAL LETTER ETA WITH OXIA
unicode_case_folding(0x1FCC, 'F', [0x03B7, 0x03B9]).			% GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_case_folding(0x1FCC, 'S', [0x1FC3]).					% GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_case_folding(0x1FD2, 'F', [0x03B9, 0x0308, 0x0300]).	% GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
unicode_case_folding(0x1FD3, 'F', [0x03B9, 0x0308, 0x0301]).	% GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_case_folding(0x1FD6, 'F', [0x03B9, 0x0342]).			% GREEK SMALL LETTER IOTA WITH PERISPOMENI
unicode_case_folding(0x1FD7, 'F', [0x03B9, 0x0308, 0x0342]).	% GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
unicode_case_folding(0x1FD8, 'C', [0x1FD0]).					% GREEK CAPITAL LETTER IOTA WITH VRACHY
unicode_case_folding(0x1FD9, 'C', [0x1FD1]).					% GREEK CAPITAL LETTER IOTA WITH MACRON
unicode_case_folding(0x1FDA, 'C', [0x1F76]).					% GREEK CAPITAL LETTER IOTA WITH VARIA
unicode_case_folding(0x1FDB, 'C', [0x1F77]).					% GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_case_folding(0x1FE2, 'F', [0x03C5, 0x0308, 0x0300]).	% GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
unicode_case_folding(0x1FE3, 'F', [0x03C5, 0x0308, 0x0301]).	% GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
unicode_case_folding(0x1FE4, 'F', [0x03C1, 0x0313]).			% GREEK SMALL LETTER RHO WITH PSILI
unicode_case_folding(0x1FE6, 'F', [0x03C5, 0x0342]).			% GREEK SMALL LETTER UPSILON WITH PERISPOMENI
unicode_case_folding(0x1FE7, 'F', [0x03C5, 0x0308, 0x0342]).	% GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
unicode_case_folding(0x1FE8, 'C', [0x1FE0]).					% GREEK CAPITAL LETTER UPSILON WITH VRACHY
unicode_case_folding(0x1FE9, 'C', [0x1FE1]).					% GREEK CAPITAL LETTER UPSILON WITH MACRON
unicode_case_folding(0x1FEA, 'C', [0x1F7A]).					% GREEK CAPITAL LETTER UPSILON WITH VARIA
unicode_case_folding(0x1FEB, 'C', [0x1F7B]).					% GREEK CAPITAL LETTER UPSILON WITH OXIA
unicode_case_folding(0x1FEC, 'C', [0x1FE5]).					% GREEK CAPITAL LETTER RHO WITH DASIA
unicode_case_folding(0x1FF2, 'F', [0x1F7C, 0x03B9]).			% GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
unicode_case_folding(0x1FF3, 'F', [0x03C9, 0x03B9]).			% GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
unicode_case_folding(0x1FF4, 'F', [0x03CE, 0x03B9]).			% GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_case_folding(0x1FF6, 'F', [0x03C9, 0x0342]).			% GREEK SMALL LETTER OMEGA WITH PERISPOMENI
unicode_case_folding(0x1FF7, 'F', [0x03C9, 0x0342, 0x03B9]).	% GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_case_folding(0x1FF8, 'C', [0x1F78]).					% GREEK CAPITAL LETTER OMICRON WITH VARIA
unicode_case_folding(0x1FF9, 'C', [0x1F79]).					% GREEK CAPITAL LETTER OMICRON WITH OXIA
unicode_case_folding(0x1FFA, 'C', [0x1F7C]).					% GREEK CAPITAL LETTER OMEGA WITH VARIA
unicode_case_folding(0x1FFB, 'C', [0x1F7D]).					% GREEK CAPITAL LETTER OMEGA WITH OXIA
unicode_case_folding(0x1FFC, 'F', [0x03C9, 0x03B9]).			% GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_case_folding(0x1FFC, 'S', [0x1FF3]).					% GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_case_folding(0x2126, 'C', [0x03C9]).					% OHM SIGN
unicode_case_folding(0x212A, 'C', [0x006B]).					% KELVIN SIGN
unicode_case_folding(0x212B, 'C', [0x00E5]).					% ANGSTROM SIGN
unicode_case_folding(0x2132, 'C', [0x214E]).					% TURNED CAPITAL F
unicode_case_folding(0x2160, 'C', [0x2170]).					% ROMAN NUMERAL ONE
unicode_case_folding(0x2161, 'C', [0x2171]).					% ROMAN NUMERAL TWO
unicode_case_folding(0x2162, 'C', [0x2172]).					% ROMAN NUMERAL THREE
unicode_case_folding(0x2163, 'C', [0x2173]).					% ROMAN NUMERAL FOUR
unicode_case_folding(0x2164, 'C', [0x2174]).					% ROMAN NUMERAL FIVE
unicode_case_folding(0x2165, 'C', [0x2175]).					% ROMAN NUMERAL SIX
unicode_case_folding(0x2166, 'C', [0x2176]).					% ROMAN NUMERAL SEVEN
unicode_case_folding(0x2167, 'C', [0x2177]).					% ROMAN NUMERAL EIGHT
unicode_case_folding(0x2168, 'C', [0x2178]).					% ROMAN NUMERAL NINE
unicode_case_folding(0x2169, 'C', [0x2179]).					% ROMAN NUMERAL TEN
unicode_case_folding(0x216A, 'C', [0x217A]).					% ROMAN NUMERAL ELEVEN
unicode_case_folding(0x216B, 'C', [0x217B]).					% ROMAN NUMERAL TWELVE
unicode_case_folding(0x216C, 'C', [0x217C]).					% ROMAN NUMERAL FIFTY
unicode_case_folding(0x216D, 'C', [0x217D]).					% ROMAN NUMERAL ONE HUNDRED
unicode_case_folding(0x216E, 'C', [0x217E]).					% ROMAN NUMERAL FIVE HUNDRED
unicode_case_folding(0x216F, 'C', [0x217F]).					% ROMAN NUMERAL ONE THOUSAND
unicode_case_folding(0x2183, 'C', [0x2184]).					% ROMAN NUMERAL REVERSED ONE HUNDRED
unicode_case_folding(0x24B6, 'C', [0x24D0]).					% CIRCLED LATIN CAPITAL LETTER A
unicode_case_folding(0x24B7, 'C', [0x24D1]).					% CIRCLED LATIN CAPITAL LETTER B
unicode_case_folding(0x24B8, 'C', [0x24D2]).					% CIRCLED LATIN CAPITAL LETTER C
unicode_case_folding(0x24B9, 'C', [0x24D3]).					% CIRCLED LATIN CAPITAL LETTER D
unicode_case_folding(0x24BA, 'C', [0x24D4]).					% CIRCLED LATIN CAPITAL LETTER E
unicode_case_folding(0x24BB, 'C', [0x24D5]).					% CIRCLED LATIN CAPITAL LETTER F
unicode_case_folding(0x24BC, 'C', [0x24D6]).					% CIRCLED LATIN CAPITAL LETTER G
unicode_case_folding(0x24BD, 'C', [0x24D7]).					% CIRCLED LATIN CAPITAL LETTER H
unicode_case_folding(0x24BE, 'C', [0x24D8]).					% CIRCLED LATIN CAPITAL LETTER I
unicode_case_folding(0x24BF, 'C', [0x24D9]).					% CIRCLED LATIN CAPITAL LETTER J
unicode_case_folding(0x24C0, 'C', [0x24DA]).					% CIRCLED LATIN CAPITAL LETTER K
unicode_case_folding(0x24C1, 'C', [0x24DB]).					% CIRCLED LATIN CAPITAL LETTER L
unicode_case_folding(0x24C2, 'C', [0x24DC]).					% CIRCLED LATIN CAPITAL LETTER M
unicode_case_folding(0x24C3, 'C', [0x24DD]).					% CIRCLED LATIN CAPITAL LETTER N
unicode_case_folding(0x24C4, 'C', [0x24DE]).					% CIRCLED LATIN CAPITAL LETTER O
unicode_case_folding(0x24C5, 'C', [0x24DF]).					% CIRCLED LATIN CAPITAL LETTER P
unicode_case_folding(0x24C6, 'C', [0x24E0]).					% CIRCLED LATIN CAPITAL LETTER Q
unicode_case_folding(0x24C7, 'C', [0x24E1]).					% CIRCLED LATIN CAPITAL LETTER R
unicode_case_folding(0x24C8, 'C', [0x24E2]).					% CIRCLED LATIN CAPITAL LETTER S
unicode_case_folding(0x24C9, 'C', [0x24E3]).					% CIRCLED LATIN CAPITAL LETTER T
unicode_case_folding(0x24CA, 'C', [0x24E4]).					% CIRCLED LATIN CAPITAL LETTER U
unicode_case_folding(0x24CB, 'C', [0x24E5]).					% CIRCLED LATIN CAPITAL LETTER V
unicode_case_folding(0x24CC, 'C', [0x24E6]).					% CIRCLED LATIN CAPITAL LETTER W
unicode_case_folding(0x24CD, 'C', [0x24E7]).					% CIRCLED LATIN CAPITAL LETTER X
unicode_case_folding(0x24CE, 'C', [0x24E8]).					% CIRCLED LATIN CAPITAL LETTER Y
unicode_case_folding(0x24CF, 'C', [0x24E9]).					% CIRCLED LATIN CAPITAL LETTER Z
unicode_case_folding(0x2C00, 'C', [0x2C30]).					% GLAGOLITIC CAPITAL LETTER AZU
unicode_case_folding(0x2C01, 'C', [0x2C31]).					% GLAGOLITIC CAPITAL LETTER BUKY
unicode_case_folding(0x2C02, 'C', [0x2C32]).					% GLAGOLITIC CAPITAL LETTER VEDE
unicode_case_folding(0x2C03, 'C', [0x2C33]).					% GLAGOLITIC CAPITAL LETTER GLAGOLI
unicode_case_folding(0x2C04, 'C', [0x2C34]).					% GLAGOLITIC CAPITAL LETTER DOBRO
unicode_case_folding(0x2C05, 'C', [0x2C35]).					% GLAGOLITIC CAPITAL LETTER YESTU
unicode_case_folding(0x2C06, 'C', [0x2C36]).					% GLAGOLITIC CAPITAL LETTER ZHIVETE
unicode_case_folding(0x2C07, 'C', [0x2C37]).					% GLAGOLITIC CAPITAL LETTER DZELO
unicode_case_folding(0x2C08, 'C', [0x2C38]).					% GLAGOLITIC CAPITAL LETTER ZEMLJA
unicode_case_folding(0x2C09, 'C', [0x2C39]).					% GLAGOLITIC CAPITAL LETTER IZHE
unicode_case_folding(0x2C0A, 'C', [0x2C3A]).					% GLAGOLITIC CAPITAL LETTER INITIAL IZHE
unicode_case_folding(0x2C0B, 'C', [0x2C3B]).					% GLAGOLITIC CAPITAL LETTER I
unicode_case_folding(0x2C0C, 'C', [0x2C3C]).					% GLAGOLITIC CAPITAL LETTER DJERVI
unicode_case_folding(0x2C0D, 'C', [0x2C3D]).					% GLAGOLITIC CAPITAL LETTER KAKO
unicode_case_folding(0x2C0E, 'C', [0x2C3E]).					% GLAGOLITIC CAPITAL LETTER LJUDIJE
unicode_case_folding(0x2C0F, 'C', [0x2C3F]).					% GLAGOLITIC CAPITAL LETTER MYSLITE
unicode_case_folding(0x2C10, 'C', [0x2C40]).					% GLAGOLITIC CAPITAL LETTER NASHI
unicode_case_folding(0x2C11, 'C', [0x2C41]).					% GLAGOLITIC CAPITAL LETTER ONU
unicode_case_folding(0x2C12, 'C', [0x2C42]).					% GLAGOLITIC CAPITAL LETTER POKOJI
unicode_case_folding(0x2C13, 'C', [0x2C43]).					% GLAGOLITIC CAPITAL LETTER RITSI
unicode_case_folding(0x2C14, 'C', [0x2C44]).					% GLAGOLITIC CAPITAL LETTER SLOVO
unicode_case_folding(0x2C15, 'C', [0x2C45]).					% GLAGOLITIC CAPITAL LETTER TVRIDO
unicode_case_folding(0x2C16, 'C', [0x2C46]).					% GLAGOLITIC CAPITAL LETTER UKU
unicode_case_folding(0x2C17, 'C', [0x2C47]).					% GLAGOLITIC CAPITAL LETTER FRITU
unicode_case_folding(0x2C18, 'C', [0x2C48]).					% GLAGOLITIC CAPITAL LETTER HERU
unicode_case_folding(0x2C19, 'C', [0x2C49]).					% GLAGOLITIC CAPITAL LETTER OTU
unicode_case_folding(0x2C1A, 'C', [0x2C4A]).					% GLAGOLITIC CAPITAL LETTER PE
unicode_case_folding(0x2C1B, 'C', [0x2C4B]).					% GLAGOLITIC CAPITAL LETTER SHTA
unicode_case_folding(0x2C1C, 'C', [0x2C4C]).					% GLAGOLITIC CAPITAL LETTER TSI
unicode_case_folding(0x2C1D, 'C', [0x2C4D]).					% GLAGOLITIC CAPITAL LETTER CHRIVI
unicode_case_folding(0x2C1E, 'C', [0x2C4E]).					% GLAGOLITIC CAPITAL LETTER SHA
unicode_case_folding(0x2C1F, 'C', [0x2C4F]).					% GLAGOLITIC CAPITAL LETTER YERU
unicode_case_folding(0x2C20, 'C', [0x2C50]).					% GLAGOLITIC CAPITAL LETTER YERI
unicode_case_folding(0x2C21, 'C', [0x2C51]).					% GLAGOLITIC CAPITAL LETTER YATI
unicode_case_folding(0x2C22, 'C', [0x2C52]).					% GLAGOLITIC CAPITAL LETTER SPIDERY HA
unicode_case_folding(0x2C23, 'C', [0x2C53]).					% GLAGOLITIC CAPITAL LETTER YU
unicode_case_folding(0x2C24, 'C', [0x2C54]).					% GLAGOLITIC CAPITAL LETTER SMALL YUS
unicode_case_folding(0x2C25, 'C', [0x2C55]).					% GLAGOLITIC CAPITAL LETTER SMALL YUS WITH TAIL
unicode_case_folding(0x2C26, 'C', [0x2C56]).					% GLAGOLITIC CAPITAL LETTER YO
unicode_case_folding(0x2C27, 'C', [0x2C57]).					% GLAGOLITIC CAPITAL LETTER IOTATED SMALL YUS
unicode_case_folding(0x2C28, 'C', [0x2C58]).					% GLAGOLITIC CAPITAL LETTER BIG YUS
unicode_case_folding(0x2C29, 'C', [0x2C59]).					% GLAGOLITIC CAPITAL LETTER IOTATED BIG YUS
unicode_case_folding(0x2C2A, 'C', [0x2C5A]).					% GLAGOLITIC CAPITAL LETTER FITA
unicode_case_folding(0x2C2B, 'C', [0x2C5B]).					% GLAGOLITIC CAPITAL LETTER IZHITSA
unicode_case_folding(0x2C2C, 'C', [0x2C5C]).					% GLAGOLITIC CAPITAL LETTER SHTAPIC
unicode_case_folding(0x2C2D, 'C', [0x2C5D]).					% GLAGOLITIC CAPITAL LETTER TROKUTASTI A
unicode_case_folding(0x2C2E, 'C', [0x2C5E]).					% GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_case_folding(0x2C60, 'C', [0x2C61]).					% LATIN CAPITAL LETTER L WITH DOUBLE BAR
unicode_case_folding(0x2C62, 'C', [0x026B]).					% LATIN CAPITAL LETTER L WITH MIDDLE TILDE
unicode_case_folding(0x2C63, 'C', [0x1D7D]).					% LATIN CAPITAL LETTER P WITH STROKE
unicode_case_folding(0x2C64, 'C', [0x027D]).					% LATIN CAPITAL LETTER R WITH TAIL
unicode_case_folding(0x2C67, 'C', [0x2C68]).					% LATIN CAPITAL LETTER H WITH DESCENDER
unicode_case_folding(0x2C69, 'C', [0x2C6A]).					% LATIN CAPITAL LETTER K WITH DESCENDER
unicode_case_folding(0x2C6B, 'C', [0x2C6C]).					% LATIN CAPITAL LETTER Z WITH DESCENDER
unicode_case_folding(0x2C6D, 'C', [0x0251]).					% LATIN CAPITAL LETTER ALPHA
unicode_case_folding(0x2C6E, 'C', [0x0271]).					% LATIN CAPITAL LETTER M WITH HOOK
unicode_case_folding(0x2C6F, 'C', [0x0250]).					% LATIN CAPITAL LETTER TURNED A
unicode_case_folding(0x2C70, 'C', [0x0252]).					% LATIN CAPITAL LETTER TURNED ALPHA
unicode_case_folding(0x2C72, 'C', [0x2C73]).					% LATIN CAPITAL LETTER W WITH HOOK
unicode_case_folding(0x2C75, 'C', [0x2C76]).					% LATIN CAPITAL LETTER HALF H
unicode_case_folding(0x2C7E, 'C', [0x023F]).					% LATIN CAPITAL LETTER S WITH SWASH TAIL
unicode_case_folding(0x2C7F, 'C', [0x0240]).					% LATIN CAPITAL LETTER Z WITH SWASH TAIL
unicode_case_folding(0x2C80, 'C', [0x2C81]).					% COPTIC CAPITAL LETTER ALFA
unicode_case_folding(0x2C82, 'C', [0x2C83]).					% COPTIC CAPITAL LETTER VIDA
unicode_case_folding(0x2C84, 'C', [0x2C85]).					% COPTIC CAPITAL LETTER GAMMA
unicode_case_folding(0x2C86, 'C', [0x2C87]).					% COPTIC CAPITAL LETTER DALDA
unicode_case_folding(0x2C88, 'C', [0x2C89]).					% COPTIC CAPITAL LETTER EIE
unicode_case_folding(0x2C8A, 'C', [0x2C8B]).					% COPTIC CAPITAL LETTER SOU
unicode_case_folding(0x2C8C, 'C', [0x2C8D]).					% COPTIC CAPITAL LETTER ZATA
unicode_case_folding(0x2C8E, 'C', [0x2C8F]).					% COPTIC CAPITAL LETTER HATE
unicode_case_folding(0x2C90, 'C', [0x2C91]).					% COPTIC CAPITAL LETTER THETHE
unicode_case_folding(0x2C92, 'C', [0x2C93]).					% COPTIC CAPITAL LETTER IAUDA
unicode_case_folding(0x2C94, 'C', [0x2C95]).					% COPTIC CAPITAL LETTER KAPA
unicode_case_folding(0x2C96, 'C', [0x2C97]).					% COPTIC CAPITAL LETTER LAULA
unicode_case_folding(0x2C98, 'C', [0x2C99]).					% COPTIC CAPITAL LETTER MI
unicode_case_folding(0x2C9A, 'C', [0x2C9B]).					% COPTIC CAPITAL LETTER NI
unicode_case_folding(0x2C9C, 'C', [0x2C9D]).					% COPTIC CAPITAL LETTER KSI
unicode_case_folding(0x2C9E, 'C', [0x2C9F]).					% COPTIC CAPITAL LETTER O
unicode_case_folding(0x2CA0, 'C', [0x2CA1]).					% COPTIC CAPITAL LETTER PI
unicode_case_folding(0x2CA2, 'C', [0x2CA3]).					% COPTIC CAPITAL LETTER RO
unicode_case_folding(0x2CA4, 'C', [0x2CA5]).					% COPTIC CAPITAL LETTER SIMA
unicode_case_folding(0x2CA6, 'C', [0x2CA7]).					% COPTIC CAPITAL LETTER TAU
unicode_case_folding(0x2CA8, 'C', [0x2CA9]).					% COPTIC CAPITAL LETTER UA
unicode_case_folding(0x2CAA, 'C', [0x2CAB]).					% COPTIC CAPITAL LETTER FI
unicode_case_folding(0x2CAC, 'C', [0x2CAD]).					% COPTIC CAPITAL LETTER KHI
unicode_case_folding(0x2CAE, 'C', [0x2CAF]).					% COPTIC CAPITAL LETTER PSI
unicode_case_folding(0x2CB0, 'C', [0x2CB1]).					% COPTIC CAPITAL LETTER OOU
unicode_case_folding(0x2CB2, 'C', [0x2CB3]).					% COPTIC CAPITAL LETTER DIALECT-P ALEF
unicode_case_folding(0x2CB4, 'C', [0x2CB5]).					% COPTIC CAPITAL LETTER OLD COPTIC AIN
unicode_case_folding(0x2CB6, 'C', [0x2CB7]).					% COPTIC CAPITAL LETTER CRYPTOGRAMMIC EIE
unicode_case_folding(0x2CB8, 'C', [0x2CB9]).					% COPTIC CAPITAL LETTER DIALECT-P KAPA
unicode_case_folding(0x2CBA, 'C', [0x2CBB]).					% COPTIC CAPITAL LETTER DIALECT-P NI
unicode_case_folding(0x2CBC, 'C', [0x2CBD]).					% COPTIC CAPITAL LETTER CRYPTOGRAMMIC NI
unicode_case_folding(0x2CBE, 'C', [0x2CBF]).					% COPTIC CAPITAL LETTER OLD COPTIC OOU
unicode_case_folding(0x2CC0, 'C', [0x2CC1]).					% COPTIC CAPITAL LETTER SAMPI
unicode_case_folding(0x2CC2, 'C', [0x2CC3]).					% COPTIC CAPITAL LETTER CROSSED SHEI
unicode_case_folding(0x2CC4, 'C', [0x2CC5]).					% COPTIC CAPITAL LETTER OLD COPTIC SHEI
unicode_case_folding(0x2CC6, 'C', [0x2CC7]).					% COPTIC CAPITAL LETTER OLD COPTIC ESH
unicode_case_folding(0x2CC8, 'C', [0x2CC9]).					% COPTIC CAPITAL LETTER AKHMIMIC KHEI
unicode_case_folding(0x2CCA, 'C', [0x2CCB]).					% COPTIC CAPITAL LETTER DIALECT-P HORI
unicode_case_folding(0x2CCC, 'C', [0x2CCD]).					% COPTIC CAPITAL LETTER OLD COPTIC HORI
unicode_case_folding(0x2CCE, 'C', [0x2CCF]).					% COPTIC CAPITAL LETTER OLD COPTIC HA
unicode_case_folding(0x2CD0, 'C', [0x2CD1]).					% COPTIC CAPITAL LETTER L-SHAPED HA
unicode_case_folding(0x2CD2, 'C', [0x2CD3]).					% COPTIC CAPITAL LETTER OLD COPTIC HEI
unicode_case_folding(0x2CD4, 'C', [0x2CD5]).					% COPTIC CAPITAL LETTER OLD COPTIC HAT
unicode_case_folding(0x2CD6, 'C', [0x2CD7]).					% COPTIC CAPITAL LETTER OLD COPTIC GANGIA
unicode_case_folding(0x2CD8, 'C', [0x2CD9]).					% COPTIC CAPITAL LETTER OLD COPTIC DJA
unicode_case_folding(0x2CDA, 'C', [0x2CDB]).					% COPTIC CAPITAL LETTER OLD COPTIC SHIMA
unicode_case_folding(0x2CDC, 'C', [0x2CDD]).					% COPTIC CAPITAL LETTER OLD NUBIAN SHIMA
unicode_case_folding(0x2CDE, 'C', [0x2CDF]).					% COPTIC CAPITAL LETTER OLD NUBIAN NGI
unicode_case_folding(0x2CE0, 'C', [0x2CE1]).					% COPTIC CAPITAL LETTER OLD NUBIAN NYI
unicode_case_folding(0x2CE2, 'C', [0x2CE3]).					% COPTIC CAPITAL LETTER OLD NUBIAN WAU
unicode_case_folding(0x2CEB, 'C', [0x2CEC]).					% COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI
unicode_case_folding(0x2CED, 'C', [0x2CEE]).					% COPTIC CAPITAL LETTER CRYPTOGRAMMIC GANGIA
unicode_case_folding(0x2CF2, 'C', [0x2CF3]).					% COPTIC CAPITAL LETTER BOHAIRIC KHEI
unicode_case_folding(0xA640, 'C', [0xA641]).					% CYRILLIC CAPITAL LETTER ZEMLYA
unicode_case_folding(0xA642, 'C', [0xA643]).					% CYRILLIC CAPITAL LETTER DZELO
unicode_case_folding(0xA644, 'C', [0xA645]).					% CYRILLIC CAPITAL LETTER REVERSED DZE
unicode_case_folding(0xA646, 'C', [0xA647]).					% CYRILLIC CAPITAL LETTER IOTA
unicode_case_folding(0xA648, 'C', [0xA649]).					% CYRILLIC CAPITAL LETTER DJERV
unicode_case_folding(0xA64A, 'C', [0xA64B]).					% CYRILLIC CAPITAL LETTER MONOGRAPH UK
unicode_case_folding(0xA64C, 'C', [0xA64D]).					% CYRILLIC CAPITAL LETTER BROAD OMEGA
unicode_case_folding(0xA64E, 'C', [0xA64F]).					% CYRILLIC CAPITAL LETTER NEUTRAL YER
unicode_case_folding(0xA650, 'C', [0xA651]).					% CYRILLIC CAPITAL LETTER YERU WITH BACK YER
unicode_case_folding(0xA652, 'C', [0xA653]).					% CYRILLIC CAPITAL LETTER IOTIFIED YAT
unicode_case_folding(0xA654, 'C', [0xA655]).					% CYRILLIC CAPITAL LETTER REVERSED YU
unicode_case_folding(0xA656, 'C', [0xA657]).					% CYRILLIC CAPITAL LETTER IOTIFIED A
unicode_case_folding(0xA658, 'C', [0xA659]).					% CYRILLIC CAPITAL LETTER CLOSED LITTLE YUS
unicode_case_folding(0xA65A, 'C', [0xA65B]).					% CYRILLIC CAPITAL LETTER BLENDED YUS
unicode_case_folding(0xA65C, 'C', [0xA65D]).					% CYRILLIC CAPITAL LETTER IOTIFIED CLOSED LITTLE YUS
unicode_case_folding(0xA65E, 'C', [0xA65F]).					% CYRILLIC CAPITAL LETTER YN
unicode_case_folding(0xA660, 'C', [0xA661]).					% CYRILLIC CAPITAL LETTER REVERSED TSE
unicode_case_folding(0xA662, 'C', [0xA663]).					% CYRILLIC CAPITAL LETTER SOFT DE
unicode_case_folding(0xA664, 'C', [0xA665]).					% CYRILLIC CAPITAL LETTER SOFT EL
unicode_case_folding(0xA666, 'C', [0xA667]).					% CYRILLIC CAPITAL LETTER SOFT EM
unicode_case_folding(0xA668, 'C', [0xA669]).					% CYRILLIC CAPITAL LETTER MONOCULAR O
unicode_case_folding(0xA66A, 'C', [0xA66B]).					% CYRILLIC CAPITAL LETTER BINOCULAR O
unicode_case_folding(0xA66C, 'C', [0xA66D]).					% CYRILLIC CAPITAL LETTER DOUBLE MONOCULAR O
unicode_case_folding(0xA680, 'C', [0xA681]).					% CYRILLIC CAPITAL LETTER DWE
unicode_case_folding(0xA682, 'C', [0xA683]).					% CYRILLIC CAPITAL LETTER DZWE
unicode_case_folding(0xA684, 'C', [0xA685]).					% CYRILLIC CAPITAL LETTER ZHWE
unicode_case_folding(0xA686, 'C', [0xA687]).					% CYRILLIC CAPITAL LETTER CCHE
unicode_case_folding(0xA688, 'C', [0xA689]).					% CYRILLIC CAPITAL LETTER DZZE
unicode_case_folding(0xA68A, 'C', [0xA68B]).					% CYRILLIC CAPITAL LETTER TE WITH MIDDLE HOOK
unicode_case_folding(0xA68C, 'C', [0xA68D]).					% CYRILLIC CAPITAL LETTER TWE
unicode_case_folding(0xA68E, 'C', [0xA68F]).					% CYRILLIC CAPITAL LETTER TSWE
unicode_case_folding(0xA690, 'C', [0xA691]).					% CYRILLIC CAPITAL LETTER TSSE
unicode_case_folding(0xA692, 'C', [0xA693]).					% CYRILLIC CAPITAL LETTER TCHE
unicode_case_folding(0xA694, 'C', [0xA695]).					% CYRILLIC CAPITAL LETTER HWE
unicode_case_folding(0xA696, 'C', [0xA697]).					% CYRILLIC CAPITAL LETTER SHWE
unicode_case_folding(0xA722, 'C', [0xA723]).					% LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF
unicode_case_folding(0xA724, 'C', [0xA725]).					% LATIN CAPITAL LETTER EGYPTOLOGICAL AIN
unicode_case_folding(0xA726, 'C', [0xA727]).					% LATIN CAPITAL LETTER HENG
unicode_case_folding(0xA728, 'C', [0xA729]).					% LATIN CAPITAL LETTER TZ
unicode_case_folding(0xA72A, 'C', [0xA72B]).					% LATIN CAPITAL LETTER TRESILLO
unicode_case_folding(0xA72C, 'C', [0xA72D]).					% LATIN CAPITAL LETTER CUATRILLO
unicode_case_folding(0xA72E, 'C', [0xA72F]).					% LATIN CAPITAL LETTER CUATRILLO WITH COMMA
unicode_case_folding(0xA732, 'C', [0xA733]).					% LATIN CAPITAL LETTER AA
unicode_case_folding(0xA734, 'C', [0xA735]).					% LATIN CAPITAL LETTER AO
unicode_case_folding(0xA736, 'C', [0xA737]).					% LATIN CAPITAL LETTER AU
unicode_case_folding(0xA738, 'C', [0xA739]).					% LATIN CAPITAL LETTER AV
unicode_case_folding(0xA73A, 'C', [0xA73B]).					% LATIN CAPITAL LETTER AV WITH HORIZONTAL BAR
unicode_case_folding(0xA73C, 'C', [0xA73D]).					% LATIN CAPITAL LETTER AY
unicode_case_folding(0xA73E, 'C', [0xA73F]).					% LATIN CAPITAL LETTER REVERSED C WITH DOT
unicode_case_folding(0xA740, 'C', [0xA741]).					% LATIN CAPITAL LETTER K WITH STROKE
unicode_case_folding(0xA742, 'C', [0xA743]).					% LATIN CAPITAL LETTER K WITH DIAGONAL STROKE
unicode_case_folding(0xA744, 'C', [0xA745]).					% LATIN CAPITAL LETTER K WITH STROKE AND DIAGONAL STROKE
unicode_case_folding(0xA746, 'C', [0xA747]).					% LATIN CAPITAL LETTER BROKEN L
unicode_case_folding(0xA748, 'C', [0xA749]).					% LATIN CAPITAL LETTER L WITH HIGH STROKE
unicode_case_folding(0xA74A, 'C', [0xA74B]).					% LATIN CAPITAL LETTER O WITH LONG STROKE OVERLAY
unicode_case_folding(0xA74C, 'C', [0xA74D]).					% LATIN CAPITAL LETTER O WITH LOOP
unicode_case_folding(0xA74E, 'C', [0xA74F]).					% LATIN CAPITAL LETTER OO
unicode_case_folding(0xA750, 'C', [0xA751]).					% LATIN CAPITAL LETTER P WITH STROKE THROUGH DESCENDER
unicode_case_folding(0xA752, 'C', [0xA753]).					% LATIN CAPITAL LETTER P WITH FLOURISH
unicode_case_folding(0xA754, 'C', [0xA755]).					% LATIN CAPITAL LETTER P WITH SQUIRREL TAIL
unicode_case_folding(0xA756, 'C', [0xA757]).					% LATIN CAPITAL LETTER Q WITH STROKE THROUGH DESCENDER
unicode_case_folding(0xA758, 'C', [0xA759]).					% LATIN CAPITAL LETTER Q WITH DIAGONAL STROKE
unicode_case_folding(0xA75A, 'C', [0xA75B]).					% LATIN CAPITAL LETTER R ROTUNDA
unicode_case_folding(0xA75C, 'C', [0xA75D]).					% LATIN CAPITAL LETTER RUM ROTUNDA
unicode_case_folding(0xA75E, 'C', [0xA75F]).					% LATIN CAPITAL LETTER V WITH DIAGONAL STROKE
unicode_case_folding(0xA760, 'C', [0xA761]).					% LATIN CAPITAL LETTER VY
unicode_case_folding(0xA762, 'C', [0xA763]).					% LATIN CAPITAL LETTER VISIGOTHIC Z
unicode_case_folding(0xA764, 'C', [0xA765]).					% LATIN CAPITAL LETTER THORN WITH STROKE
unicode_case_folding(0xA766, 'C', [0xA767]).					% LATIN CAPITAL LETTER THORN WITH STROKE THROUGH DESCENDER
unicode_case_folding(0xA768, 'C', [0xA769]).					% LATIN CAPITAL LETTER VEND
unicode_case_folding(0xA76A, 'C', [0xA76B]).					% LATIN CAPITAL LETTER ET
unicode_case_folding(0xA76C, 'C', [0xA76D]).					% LATIN CAPITAL LETTER IS
unicode_case_folding(0xA76E, 'C', [0xA76F]).					% LATIN CAPITAL LETTER CON
unicode_case_folding(0xA779, 'C', [0xA77A]).					% LATIN CAPITAL LETTER INSULAR D
unicode_case_folding(0xA77B, 'C', [0xA77C]).					% LATIN CAPITAL LETTER INSULAR F
unicode_case_folding(0xA77D, 'C', [0x1D79]).					% LATIN CAPITAL LETTER INSULAR G
unicode_case_folding(0xA77E, 'C', [0xA77F]).					% LATIN CAPITAL LETTER TURNED INSULAR G
unicode_case_folding(0xA780, 'C', [0xA781]).					% LATIN CAPITAL LETTER TURNED L
unicode_case_folding(0xA782, 'C', [0xA783]).					% LATIN CAPITAL LETTER INSULAR R
unicode_case_folding(0xA784, 'C', [0xA785]).					% LATIN CAPITAL LETTER INSULAR S
unicode_case_folding(0xA786, 'C', [0xA787]).					% LATIN CAPITAL LETTER INSULAR T
unicode_case_folding(0xA78B, 'C', [0xA78C]).					% LATIN CAPITAL LETTER SALTILLO
unicode_case_folding(0xA78D, 'C', [0x0265]).					% LATIN CAPITAL LETTER TURNED H
unicode_case_folding(0xA790, 'C', [0xA791]).					% LATIN CAPITAL LETTER N WITH DESCENDER
unicode_case_folding(0xA792, 'C', [0xA793]).					% LATIN CAPITAL LETTER C WITH BAR
unicode_case_folding(0xA7A0, 'C', [0xA7A1]).					% LATIN CAPITAL LETTER G WITH OBLIQUE STROKE
unicode_case_folding(0xA7A2, 'C', [0xA7A3]).					% LATIN CAPITAL LETTER K WITH OBLIQUE STROKE
unicode_case_folding(0xA7A4, 'C', [0xA7A5]).					% LATIN CAPITAL LETTER N WITH OBLIQUE STROKE
unicode_case_folding(0xA7A6, 'C', [0xA7A7]).					% LATIN CAPITAL LETTER R WITH OBLIQUE STROKE
unicode_case_folding(0xA7A8, 'C', [0xA7A9]).					% LATIN CAPITAL LETTER S WITH OBLIQUE STROKE
unicode_case_folding(0xA7AA, 'C', [0x0266]).					% LATIN CAPITAL LETTER H WITH HOOK
unicode_case_folding(0xFB00, 'F', [0x0066, 0x0066]).			% LATIN SMALL LIGATURE FF
unicode_case_folding(0xFB01, 'F', [0x0066, 0x0069]).			% LATIN SMALL LIGATURE FI
unicode_case_folding(0xFB02, 'F', [0x0066, 0x006C]).			% LATIN SMALL LIGATURE FL
unicode_case_folding(0xFB03, 'F', [0x0066, 0x0066, 0x0069]).	% LATIN SMALL LIGATURE FFI
unicode_case_folding(0xFB04, 'F', [0x0066, 0x0066, 0x006C]).	% LATIN SMALL LIGATURE FFL
unicode_case_folding(0xFB05, 'F', [0x0073, 0x0074]).			% LATIN SMALL LIGATURE LONG S T
unicode_case_folding(0xFB06, 'F', [0x0073, 0x0074]).			% LATIN SMALL LIGATURE ST
unicode_case_folding(0xFB13, 'F', [0x0574, 0x0576]).			% ARMENIAN SMALL LIGATURE MEN NOW
unicode_case_folding(0xFB14, 'F', [0x0574, 0x0565]).			% ARMENIAN SMALL LIGATURE MEN ECH
unicode_case_folding(0xFB15, 'F', [0x0574, 0x056B]).			% ARMENIAN SMALL LIGATURE MEN INI
unicode_case_folding(0xFB16, 'F', [0x057E, 0x0576]).			% ARMENIAN SMALL LIGATURE VEW NOW
unicode_case_folding(0xFB17, 'F', [0x0574, 0x056D]).			% ARMENIAN SMALL LIGATURE MEN XEH
unicode_case_folding(0xFF21, 'C', [0xFF41]).					% FULLWIDTH LATIN CAPITAL LETTER A
unicode_case_folding(0xFF22, 'C', [0xFF42]).					% FULLWIDTH LATIN CAPITAL LETTER B
unicode_case_folding(0xFF23, 'C', [0xFF43]).					% FULLWIDTH LATIN CAPITAL LETTER C
unicode_case_folding(0xFF24, 'C', [0xFF44]).					% FULLWIDTH LATIN CAPITAL LETTER D
unicode_case_folding(0xFF25, 'C', [0xFF45]).					% FULLWIDTH LATIN CAPITAL LETTER E
unicode_case_folding(0xFF26, 'C', [0xFF46]).					% FULLWIDTH LATIN CAPITAL LETTER F
unicode_case_folding(0xFF27, 'C', [0xFF47]).					% FULLWIDTH LATIN CAPITAL LETTER G
unicode_case_folding(0xFF28, 'C', [0xFF48]).					% FULLWIDTH LATIN CAPITAL LETTER H
unicode_case_folding(0xFF29, 'C', [0xFF49]).					% FULLWIDTH LATIN CAPITAL LETTER I
unicode_case_folding(0xFF2A, 'C', [0xFF4A]).					% FULLWIDTH LATIN CAPITAL LETTER J
unicode_case_folding(0xFF2B, 'C', [0xFF4B]).					% FULLWIDTH LATIN CAPITAL LETTER K
unicode_case_folding(0xFF2C, 'C', [0xFF4C]).					% FULLWIDTH LATIN CAPITAL LETTER L
unicode_case_folding(0xFF2D, 'C', [0xFF4D]).					% FULLWIDTH LATIN CAPITAL LETTER M
unicode_case_folding(0xFF2E, 'C', [0xFF4E]).					% FULLWIDTH LATIN CAPITAL LETTER N
unicode_case_folding(0xFF2F, 'C', [0xFF4F]).					% FULLWIDTH LATIN CAPITAL LETTER O
unicode_case_folding(0xFF30, 'C', [0xFF50]).					% FULLWIDTH LATIN CAPITAL LETTER P
unicode_case_folding(0xFF31, 'C', [0xFF51]).					% FULLWIDTH LATIN CAPITAL LETTER Q
unicode_case_folding(0xFF32, 'C', [0xFF52]).					% FULLWIDTH LATIN CAPITAL LETTER R
unicode_case_folding(0xFF33, 'C', [0xFF53]).					% FULLWIDTH LATIN CAPITAL LETTER S
unicode_case_folding(0xFF34, 'C', [0xFF54]).					% FULLWIDTH LATIN CAPITAL LETTER T
unicode_case_folding(0xFF35, 'C', [0xFF55]).					% FULLWIDTH LATIN CAPITAL LETTER U
unicode_case_folding(0xFF36, 'C', [0xFF56]).					% FULLWIDTH LATIN CAPITAL LETTER V
unicode_case_folding(0xFF37, 'C', [0xFF57]).					% FULLWIDTH LATIN CAPITAL LETTER W
unicode_case_folding(0xFF38, 'C', [0xFF58]).					% FULLWIDTH LATIN CAPITAL LETTER X
unicode_case_folding(0xFF39, 'C', [0xFF59]).					% FULLWIDTH LATIN CAPITAL LETTER Y
unicode_case_folding(0xFF3A, 'C', [0xFF5A]).					% FULLWIDTH LATIN CAPITAL LETTER Z
unicode_case_folding(0x10400, 'C', [0x10428]).				% DESERET CAPITAL LETTER LONG I
unicode_case_folding(0x10401, 'C', [0x10429]).				% DESERET CAPITAL LETTER LONG E
unicode_case_folding(0x10402, 'C', [0x1042A]).				% DESERET CAPITAL LETTER LONG A
unicode_case_folding(0x10403, 'C', [0x1042B]).				% DESERET CAPITAL LETTER LONG AH
unicode_case_folding(0x10404, 'C', [0x1042C]).				% DESERET CAPITAL LETTER LONG O
unicode_case_folding(0x10405, 'C', [0x1042D]).				% DESERET CAPITAL LETTER LONG OO
unicode_case_folding(0x10406, 'C', [0x1042E]).				% DESERET CAPITAL LETTER SHORT I
unicode_case_folding(0x10407, 'C', [0x1042F]).				% DESERET CAPITAL LETTER SHORT E
unicode_case_folding(0x10408, 'C', [0x10430]).				% DESERET CAPITAL LETTER SHORT A
unicode_case_folding(0x10409, 'C', [0x10431]).				% DESERET CAPITAL LETTER SHORT AH
unicode_case_folding(0x1040A, 'C', [0x10432]).				% DESERET CAPITAL LETTER SHORT O
unicode_case_folding(0x1040B, 'C', [0x10433]).				% DESERET CAPITAL LETTER SHORT OO
unicode_case_folding(0x1040C, 'C', [0x10434]).				% DESERET CAPITAL LETTER AY
unicode_case_folding(0x1040D, 'C', [0x10435]).				% DESERET CAPITAL LETTER OW
unicode_case_folding(0x1040E, 'C', [0x10436]).				% DESERET CAPITAL LETTER WU
unicode_case_folding(0x1040F, 'C', [0x10437]).				% DESERET CAPITAL LETTER YEE
unicode_case_folding(0x10410, 'C', [0x10438]).				% DESERET CAPITAL LETTER H
unicode_case_folding(0x10411, 'C', [0x10439]).				% DESERET CAPITAL LETTER PEE
unicode_case_folding(0x10412, 'C', [0x1043A]).				% DESERET CAPITAL LETTER BEE
unicode_case_folding(0x10413, 'C', [0x1043B]).				% DESERET CAPITAL LETTER TEE
unicode_case_folding(0x10414, 'C', [0x1043C]).				% DESERET CAPITAL LETTER DEE
unicode_case_folding(0x10415, 'C', [0x1043D]).				% DESERET CAPITAL LETTER CHEE
unicode_case_folding(0x10416, 'C', [0x1043E]).				% DESERET CAPITAL LETTER JEE
unicode_case_folding(0x10417, 'C', [0x1043F]).				% DESERET CAPITAL LETTER KAY
unicode_case_folding(0x10418, 'C', [0x10440]).				% DESERET CAPITAL LETTER GAY
unicode_case_folding(0x10419, 'C', [0x10441]).				% DESERET CAPITAL LETTER EF
unicode_case_folding(0x1041A, 'C', [0x10442]).				% DESERET CAPITAL LETTER VEE
unicode_case_folding(0x1041B, 'C', [0x10443]).				% DESERET CAPITAL LETTER ETH
unicode_case_folding(0x1041C, 'C', [0x10444]).				% DESERET CAPITAL LETTER THEE
unicode_case_folding(0x1041D, 'C', [0x10445]).				% DESERET CAPITAL LETTER ES
unicode_case_folding(0x1041E, 'C', [0x10446]).				% DESERET CAPITAL LETTER ZEE
unicode_case_folding(0x1041F, 'C', [0x10447]).				% DESERET CAPITAL LETTER ESH
unicode_case_folding(0x10420, 'C', [0x10448]).				% DESERET CAPITAL LETTER ZHEE
unicode_case_folding(0x10421, 'C', [0x10449]).				% DESERET CAPITAL LETTER ER
unicode_case_folding(0x10422, 'C', [0x1044A]).				% DESERET CAPITAL LETTER EL
unicode_case_folding(0x10423, 'C', [0x1044B]).				% DESERET CAPITAL LETTER EM
unicode_case_folding(0x10424, 'C', [0x1044C]).				% DESERET CAPITAL LETTER EN
unicode_case_folding(0x10425, 'C', [0x1044D]).				% DESERET CAPITAL LETTER ENG
unicode_case_folding(0x10426, 'C', [0x1044E]).				% DESERET CAPITAL LETTER OI
unicode_case_folding(0x10427, 'C', [0x1044F]).				% DESERET CAPITAL LETTER EW
