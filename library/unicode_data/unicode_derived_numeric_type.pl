%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 29, 2012
%
%  Original Unicode file header comments follow

/*
# DerivedNumericType-6.1.0.txt
# Date: 2011-08-23, 00:47:14 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

% ================================================

# Derived Property:   Numeric_Type
#  The values are based on fields 6-8 of UnicodeData.txt, plus the fields
#  kAccountingNumeric, kOtherNumeric, kPrimaryNumeric in the Unicode Han Database (Unihan).
#  The derivations for these values are as follows.
#   Numeric_Type=Decimal: When there is a value in field 6.
#   Numeric_Type=Digit:   When there is a value in field 7, but not in field 6.
#   Numeric_Type=Numeric: When there are values for kAccountingNumeric, kOtherNumeric, kPrimaryNumeric,
#                         or there is a value in field 8, but not in field 7.
#   Numeric_Type=None:    Otherwise

#  All code points not explicitly listed for Numeric_Type
#  have the value None.

# @missing: 0000..10FFFF; None
*/

% ================================================

unicode_numeric_type(CodePoint, Type) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_numeric_type(CodePointStart, CodePointEnd, Type),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_numeric_type(CodePoint, _, CodePointType) ->
		Type = CodePointType
	;	% look for a code point range that includes the given code point
		unicode_numeric_type(CodePointStart, CodePointEnd, CodePointType),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Type = CodePointType
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Type = 'None'
	).

unicode_numeric_type(0x00BC, 0x00BE, 'Numeric').	% No   [3] VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS
unicode_numeric_type(0x09F4, 0x09F9, 'Numeric').	% No   [6] BENGALI CURRENCY NUMERATOR ONE..BENGALI CURRENCY DENOMINATOR SIXTEEN
unicode_numeric_type(0x0B72, 0x0B77, 'Numeric').	% No   [6] ORIYA FRACTION ONE QUARTER..ORIYA FRACTION THREE SIXTEENTHS
unicode_numeric_type(0x0BF0, 0x0BF2, 'Numeric').	% No   [3] TAMIL NUMBER TEN..TAMIL NUMBER ONE THOUSAND
unicode_numeric_type(0x0C78, 0x0C7E, 'Numeric').	% No   [7] TELUGU FRACTION DIGIT ZERO FOR ODD POWERS OF FOUR..TELUGU FRACTION DIGIT THREE FOR EVEN POWERS OF FOUR
unicode_numeric_type(0x0D70, 0x0D75, 'Numeric').	% No   [6] MALAYALAM NUMBER TEN..MALAYALAM FRACTION THREE QUARTERS
unicode_numeric_type(0x0F2A, 0x0F33, 'Numeric').	% No  [10] TIBETAN DIGIT HALF ONE..TIBETAN DIGIT HALF ZERO
unicode_numeric_type(0x1372, 0x137C, 'Numeric').	% No  [11] ETHIOPIC NUMBER TEN..ETHIOPIC NUMBER TEN THOUSAND
unicode_numeric_type(0x16EE, 0x16F0, 'Numeric').	% Nl   [3] RUNIC ARLAUG SYMBOL..RUNIC BELGTHOR SYMBOL
unicode_numeric_type(0x17F0, 0x17F9, 'Numeric').	% No  [10] KHMER SYMBOL LEK ATTAK SON..KHMER SYMBOL LEK ATTAK PRAM-BUON
unicode_numeric_type(0x2150, 0x215F, 'Numeric').	% No  [16] VULGAR FRACTION ONE SEVENTH..FRACTION NUMERATOR ONE
unicode_numeric_type(0x2160, 0x2182, 'Numeric').	% Nl  [35] ROMAN NUMERAL ONE..ROMAN NUMERAL TEN THOUSAND
unicode_numeric_type(0x2185, 0x2188, 'Numeric').	% Nl   [4] ROMAN NUMERAL SIX LATE FORM..ROMAN NUMERAL ONE HUNDRED THOUSAND
unicode_numeric_type(0x2189, 0x2189, 'Numeric').	% No       VULGAR FRACTION ZERO THIRDS
unicode_numeric_type(0x2469, 0x2473, 'Numeric').	% No  [11] CIRCLED NUMBER TEN..CIRCLED NUMBER TWENTY
unicode_numeric_type(0x247D, 0x2487, 'Numeric').	% No  [11] PARENTHESIZED NUMBER TEN..PARENTHESIZED NUMBER TWENTY
unicode_numeric_type(0x2491, 0x249B, 'Numeric').	% No  [11] NUMBER TEN FULL STOP..NUMBER TWENTY FULL STOP
unicode_numeric_type(0x24EB, 0x24F4, 'Numeric').	% No  [10] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED NUMBER TWENTY
unicode_numeric_type(0x24FE, 0x24FE, 'Numeric').	% No       DOUBLE CIRCLED NUMBER TEN
unicode_numeric_type(0x277F, 0x277F, 'Numeric').	% No       DINGBAT NEGATIVE CIRCLED NUMBER TEN
unicode_numeric_type(0x2789, 0x2789, 'Numeric').	% No       DINGBAT CIRCLED SANS-SERIF NUMBER TEN
unicode_numeric_type(0x2793, 0x2793, 'Numeric').	% No       DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
unicode_numeric_type(0x2CFD, 0x2CFD, 'Numeric').	% No       COPTIC FRACTION ONE HALF
unicode_numeric_type(0x3007, 0x3007, 'Numeric').	% Nl       IDEOGRAPHIC NUMBER ZERO
unicode_numeric_type(0x3021, 0x3029, 'Numeric').	% Nl   [9] HANGZHOU NUMERAL ONE..HANGZHOU NUMERAL NINE
unicode_numeric_type(0x3038, 0x303A, 'Numeric').	% Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
unicode_numeric_type(0x3192, 0x3195, 'Numeric').	% No   [4] IDEOGRAPHIC ANNOTATION ONE MARK..IDEOGRAPHIC ANNOTATION FOUR MARK
unicode_numeric_type(0x3220, 0x3229, 'Numeric').	% No  [10] PARENTHESIZED IDEOGRAPH ONE..PARENTHESIZED IDEOGRAPH TEN
unicode_numeric_type(0x3248, 0x324F, 'Numeric').	% No   [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
unicode_numeric_type(0x3251, 0x325F, 'Numeric').	% No  [15] CIRCLED NUMBER TWENTY ONE..CIRCLED NUMBER THIRTY FIVE
unicode_numeric_type(0x3280, 0x3289, 'Numeric').	% No  [10] CIRCLED IDEOGRAPH ONE..CIRCLED IDEOGRAPH TEN
unicode_numeric_type(0x32B1, 0x32BF, 'Numeric').	% No  [15] CIRCLED NUMBER THIRTY SIX..CIRCLED NUMBER FIFTY
unicode_numeric_type(0x3405, 0x3405, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-3405
unicode_numeric_type(0x3483, 0x3483, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-3483
unicode_numeric_type(0x382A, 0x382A, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-382A
unicode_numeric_type(0x3B4D, 0x3B4D, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-3B4D
unicode_numeric_type(0x4E00, 0x4E00, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-4E00
unicode_numeric_type(0x4E03, 0x4E03, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-4E03
unicode_numeric_type(0x4E07, 0x4E07, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-4E07
unicode_numeric_type(0x4E09, 0x4E09, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-4E09
unicode_numeric_type(0x4E5D, 0x4E5D, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-4E5D
unicode_numeric_type(0x4E8C, 0x4E8C, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-4E8C
unicode_numeric_type(0x4E94, 0x4E94, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-4E94
unicode_numeric_type(0x4E96, 0x4E96, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-4E96
unicode_numeric_type(0x4EBF, 0x4EC0, 'Numeric').	% Lo   [2] CJK UNIFIED IDEOGRAPH-4EBF..CJK UNIFIED IDEOGRAPH-4EC0
unicode_numeric_type(0x4EDF, 0x4EDF, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-4EDF
unicode_numeric_type(0x4EE8, 0x4EE8, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-4EE8
unicode_numeric_type(0x4F0D, 0x4F0D, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-4F0D
unicode_numeric_type(0x4F70, 0x4F70, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-4F70
unicode_numeric_type(0x5104, 0x5104, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-5104
unicode_numeric_type(0x5146, 0x5146, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-5146
unicode_numeric_type(0x5169, 0x5169, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-5169
unicode_numeric_type(0x516B, 0x516B, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-516B
unicode_numeric_type(0x516D, 0x516D, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-516D
unicode_numeric_type(0x5341, 0x5341, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-5341
unicode_numeric_type(0x5343, 0x5345, 'Numeric').	% Lo   [3] CJK UNIFIED IDEOGRAPH-5343..CJK UNIFIED IDEOGRAPH-5345
unicode_numeric_type(0x534C, 0x534C, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-534C
unicode_numeric_type(0x53C1, 0x53C4, 'Numeric').	% Lo   [4] CJK UNIFIED IDEOGRAPH-53C1..CJK UNIFIED IDEOGRAPH-53C4
unicode_numeric_type(0x56DB, 0x56DB, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-56DB
unicode_numeric_type(0x58F1, 0x58F1, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-58F1
unicode_numeric_type(0x58F9, 0x58F9, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-58F9
unicode_numeric_type(0x5E7A, 0x5E7A, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-5E7A
unicode_numeric_type(0x5EFE, 0x5EFF, 'Numeric').	% Lo   [2] CJK UNIFIED IDEOGRAPH-5EFE..CJK UNIFIED IDEOGRAPH-5EFF
unicode_numeric_type(0x5F0C, 0x5F0E, 'Numeric').	% Lo   [3] CJK UNIFIED IDEOGRAPH-5F0C..CJK UNIFIED IDEOGRAPH-5F0E
unicode_numeric_type(0x5F10, 0x5F10, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-5F10
unicode_numeric_type(0x62FE, 0x62FE, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-62FE
unicode_numeric_type(0x634C, 0x634C, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-634C
unicode_numeric_type(0x67D2, 0x67D2, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-67D2
unicode_numeric_type(0x6F06, 0x6F06, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-6F06
unicode_numeric_type(0x7396, 0x7396, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-7396
unicode_numeric_type(0x767E, 0x767E, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-767E
unicode_numeric_type(0x8086, 0x8086, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-8086
unicode_numeric_type(0x842C, 0x842C, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-842C
unicode_numeric_type(0x8CAE, 0x8CAE, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-8CAE
unicode_numeric_type(0x8CB3, 0x8CB3, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-8CB3
unicode_numeric_type(0x8D30, 0x8D30, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-8D30
unicode_numeric_type(0x9621, 0x9621, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-9621
unicode_numeric_type(0x9646, 0x9646, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-9646
unicode_numeric_type(0x964C, 0x964C, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-964C
unicode_numeric_type(0x9678, 0x9678, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-9678
unicode_numeric_type(0x96F6, 0x96F6, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-96F6
unicode_numeric_type(0xA6E6, 0xA6EF, 'Numeric').	% Nl  [10] BAMUM LETTER MO..BAMUM LETTER KOGHOM
unicode_numeric_type(0xA830, 0xA835, 'Numeric').	% No   [6] NORTH INDIC FRACTION ONE QUARTER..NORTH INDIC FRACTION THREE SIXTEENTHS
unicode_numeric_type(0xF96B, 0xF96B, 'Numeric').	% Lo       CJK COMPATIBILITY IDEOGRAPH-F96B
unicode_numeric_type(0xF973, 0xF973, 'Numeric').	% Lo       CJK COMPATIBILITY IDEOGRAPH-F973
unicode_numeric_type(0xF978, 0xF978, 'Numeric').	% Lo       CJK COMPATIBILITY IDEOGRAPH-F978
unicode_numeric_type(0xF9B2, 0xF9B2, 'Numeric').	% Lo       CJK COMPATIBILITY IDEOGRAPH-F9B2
unicode_numeric_type(0xF9D1, 0xF9D1, 'Numeric').	% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D1
unicode_numeric_type(0xF9D3, 0xF9D3, 'Numeric').	% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D3
unicode_numeric_type(0xF9FD, 0xF9FD, 'Numeric').	% Lo       CJK COMPATIBILITY IDEOGRAPH-F9FD
unicode_numeric_type(0x10107, 0x10133, 'Numeric').	% No  [45] AEGEAN NUMBER ONE..AEGEAN NUMBER NINETY THOUSAND
unicode_numeric_type(0x10140, 0x10174, 'Numeric').	% Nl  [53] GREEK ACROPHONIC ATTIC ONE QUARTER..GREEK ACROPHONIC STRATIAN FIFTY MNAS
unicode_numeric_type(0x10175, 0x10178, 'Numeric').	% No   [4] GREEK ONE HALF SIGN..GREEK THREE QUARTERS SIGN
unicode_numeric_type(0x1018A, 0x1018A, 'Numeric').	% No       GREEK ZERO SIGN
unicode_numeric_type(0x10320, 0x10323, 'Numeric').	% No   [4] OLD ITALIC NUMERAL ONE..OLD ITALIC NUMERAL FIFTY
unicode_numeric_type(0x10341, 0x10341, 'Numeric').	% Nl       GOTHIC LETTER NINETY
unicode_numeric_type(0x1034A, 0x1034A, 'Numeric').	% Nl       GOTHIC LETTER NINE HUNDRED
unicode_numeric_type(0x103D1, 0x103D5, 'Numeric').	% Nl   [5] OLD PERSIAN NUMBER ONE..OLD PERSIAN NUMBER HUNDRED
unicode_numeric_type(0x10858, 0x1085F, 'Numeric').	% No   [8] IMPERIAL ARAMAIC NUMBER ONE..IMPERIAL ARAMAIC NUMBER TEN THOUSAND
unicode_numeric_type(0x10916, 0x1091B, 'Numeric').	% No   [6] PHOENICIAN NUMBER ONE..PHOENICIAN NUMBER THREE
unicode_numeric_type(0x10A44, 0x10A47, 'Numeric').	% No   [4] KHAROSHTHI NUMBER TEN..KHAROSHTHI NUMBER ONE THOUSAND
unicode_numeric_type(0x10A7D, 0x10A7E, 'Numeric').	% No   [2] OLD SOUTH ARABIAN NUMBER ONE..OLD SOUTH ARABIAN NUMBER FIFTY
unicode_numeric_type(0x10B58, 0x10B5F, 'Numeric').	% No   [8] INSCRIPTIONAL PARTHIAN NUMBER ONE..INSCRIPTIONAL PARTHIAN NUMBER ONE THOUSAND
unicode_numeric_type(0x10B78, 0x10B7F, 'Numeric').	% No   [8] INSCRIPTIONAL PAHLAVI NUMBER ONE..INSCRIPTIONAL PAHLAVI NUMBER ONE THOUSAND
unicode_numeric_type(0x10E69, 0x10E7E, 'Numeric').	% No  [22] RUMI NUMBER TEN..RUMI FRACTION TWO THIRDS
unicode_numeric_type(0x1105B, 0x11065, 'Numeric').	% No  [11] BRAHMI NUMBER TEN..BRAHMI NUMBER ONE THOUSAND
unicode_numeric_type(0x12400, 0x12431, 'Numeric').	% Nl  [50] CUNEIFORM NUMERIC SIGN TWO ASH..CUNEIFORM NUMERIC SIGN FIVE SHARU
unicode_numeric_type(0x12434, 0x12455, 'Numeric').	% Nl  [34] CUNEIFORM NUMERIC SIGN ONE BURU..CUNEIFORM NUMERIC SIGN FIVE BAN2 VARIANT FORM
unicode_numeric_type(0x12458, 0x12462, 'Numeric').	% Nl  [11] CUNEIFORM NUMERIC SIGN ONE ESHE3..CUNEIFORM NUMERIC SIGN OLD ASSYRIAN ONE QUARTER
unicode_numeric_type(0x1D360, 0x1D371, 'Numeric').	% No  [18] COUNTING ROD UNIT DIGIT ONE..COUNTING ROD TENS DIGIT NINE
unicode_numeric_type(0x20001, 0x20001, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-20001
unicode_numeric_type(0x20064, 0x20064, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-20064
unicode_numeric_type(0x200E2, 0x200E2, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-200E2
unicode_numeric_type(0x20121, 0x20121, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-20121
unicode_numeric_type(0x2092A, 0x2092A, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-2092A
unicode_numeric_type(0x20983, 0x20983, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-20983
unicode_numeric_type(0x2098C, 0x2098C, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-2098C
unicode_numeric_type(0x2099C, 0x2099C, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-2099C
unicode_numeric_type(0x20AEA, 0x20AEA, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-20AEA
unicode_numeric_type(0x20AFD, 0x20AFD, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-20AFD
unicode_numeric_type(0x20B19, 0x20B19, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-20B19
unicode_numeric_type(0x22390, 0x22390, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-22390
unicode_numeric_type(0x22998, 0x22998, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-22998
unicode_numeric_type(0x23B1B, 0x23B1B, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-23B1B
unicode_numeric_type(0x2626D, 0x2626D, 'Numeric').	% Lo       CJK UNIFIED IDEOGRAPH-2626D
unicode_numeric_type(0x2F890, 0x2F890, 'Numeric').	% Lo       CJK COMPATIBILITY IDEOGRAPH-2F890

% Total code points: 637

% ================================================

unicode_numeric_type(0x00B2, 0x00B3, 'Digit').	% No   [2] SUPERSCRIPT TWO..SUPERSCRIPT THREE
unicode_numeric_type(0x00B9, 0x00B9, 'Digit').	% No       SUPERSCRIPT ONE
unicode_numeric_type(0x1369, 0x1371, 'Digit').	% No   [9] ETHIOPIC DIGIT ONE..ETHIOPIC DIGIT NINE
unicode_numeric_type(0x19DA, 0x19DA, 'Digit').	% No       NEW TAI LUE THAM DIGIT ONE
unicode_numeric_type(0x2070, 0x2070, 'Digit').	% No       SUPERSCRIPT ZERO
unicode_numeric_type(0x2074, 0x2079, 'Digit').	% No   [6] SUPERSCRIPT FOUR..SUPERSCRIPT NINE
unicode_numeric_type(0x2080, 0x2089, 'Digit').	% No  [10] SUBSCRIPT ZERO..SUBSCRIPT NINE
unicode_numeric_type(0x2460, 0x2468, 'Digit').	% No   [9] CIRCLED DIGIT ONE..CIRCLED DIGIT NINE
unicode_numeric_type(0x2474, 0x247C, 'Digit').	% No   [9] PARENTHESIZED DIGIT ONE..PARENTHESIZED DIGIT NINE
unicode_numeric_type(0x2488, 0x2490, 'Digit').	% No   [9] DIGIT ONE FULL STOP..DIGIT NINE FULL STOP
unicode_numeric_type(0x24EA, 0x24EA, 'Digit').	% No       CIRCLED DIGIT ZERO
unicode_numeric_type(0x24F5, 0x24FD, 'Digit').	% No   [9] DOUBLE CIRCLED DIGIT ONE..DOUBLE CIRCLED DIGIT NINE
unicode_numeric_type(0x24FF, 0x24FF, 'Digit').	% No       NEGATIVE CIRCLED DIGIT ZERO
unicode_numeric_type(0x2776, 0x277E, 'Digit').	% No   [9] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED DIGIT NINE
unicode_numeric_type(0x2780, 0x2788, 'Digit').	% No   [9] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT CIRCLED SANS-SERIF DIGIT NINE
unicode_numeric_type(0x278A, 0x2792, 'Digit').	% No   [9] DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT NINE
unicode_numeric_type(0x10A40, 0x10A43, 'Digit').	% No   [4] KHAROSHTHI DIGIT ONE..KHAROSHTHI DIGIT FOUR
unicode_numeric_type(0x10E60, 0x10E68, 'Digit').	% No   [9] RUMI DIGIT ONE..RUMI DIGIT NINE
unicode_numeric_type(0x11052, 0x1105A, 'Digit').	% No   [9] BRAHMI NUMBER ONE..BRAHMI NUMBER NINE
unicode_numeric_type(0x1F100, 0x1F10A, 'Digit').	% No  [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA

% Total code points: 128

% ================================================

unicode_numeric_type(0x0030, 0x0039, 'Decimal').	% Nd  [10] DIGIT ZERO..DIGIT NINE
unicode_numeric_type(0x0660, 0x0669, 'Decimal').	% Nd  [10] ARABIC-INDIC DIGIT ZERO..ARABIC-INDIC DIGIT NINE
unicode_numeric_type(0x06F0, 0x06F9, 'Decimal').	% Nd  [10] EXTENDED ARABIC-INDIC DIGIT ZERO..EXTENDED ARABIC-INDIC DIGIT NINE
unicode_numeric_type(0x07C0, 0x07C9, 'Decimal').	% Nd  [10] NKO DIGIT ZERO..NKO DIGIT NINE
unicode_numeric_type(0x0966, 0x096F, 'Decimal').	% Nd  [10] DEVANAGARI DIGIT ZERO..DEVANAGARI DIGIT NINE
unicode_numeric_type(0x09E6, 0x09EF, 'Decimal').	% Nd  [10] BENGALI DIGIT ZERO..BENGALI DIGIT NINE
unicode_numeric_type(0x0A66, 0x0A6F, 'Decimal').	% Nd  [10] GURMUKHI DIGIT ZERO..GURMUKHI DIGIT NINE
unicode_numeric_type(0x0AE6, 0x0AEF, 'Decimal').	% Nd  [10] GUJARATI DIGIT ZERO..GUJARATI DIGIT NINE
unicode_numeric_type(0x0B66, 0x0B6F, 'Decimal').	% Nd  [10] ORIYA DIGIT ZERO..ORIYA DIGIT NINE
unicode_numeric_type(0x0BE6, 0x0BEF, 'Decimal').	% Nd  [10] TAMIL DIGIT ZERO..TAMIL DIGIT NINE
unicode_numeric_type(0x0C66, 0x0C6F, 'Decimal').	% Nd  [10] TELUGU DIGIT ZERO..TELUGU DIGIT NINE
unicode_numeric_type(0x0CE6, 0x0CEF, 'Decimal').	% Nd  [10] KANNADA DIGIT ZERO..KANNADA DIGIT NINE
unicode_numeric_type(0x0D66, 0x0D6F, 'Decimal').	% Nd  [10] MALAYALAM DIGIT ZERO..MALAYALAM DIGIT NINE
unicode_numeric_type(0x0E50, 0x0E59, 'Decimal').	% Nd  [10] THAI DIGIT ZERO..THAI DIGIT NINE
unicode_numeric_type(0x0ED0, 0x0ED9, 'Decimal').	% Nd  [10] LAO DIGIT ZERO..LAO DIGIT NINE
unicode_numeric_type(0x0F20, 0x0F29, 'Decimal').	% Nd  [10] TIBETAN DIGIT ZERO..TIBETAN DIGIT NINE
unicode_numeric_type(0x1040, 0x1049, 'Decimal').	% Nd  [10] MYANMAR DIGIT ZERO..MYANMAR DIGIT NINE
unicode_numeric_type(0x1090, 0x1099, 'Decimal').	% Nd  [10] MYANMAR SHAN DIGIT ZERO..MYANMAR SHAN DIGIT NINE
unicode_numeric_type(0x17E0, 0x17E9, 'Decimal').	% Nd  [10] KHMER DIGIT ZERO..KHMER DIGIT NINE
unicode_numeric_type(0x1810, 0x1819, 'Decimal').	% Nd  [10] MONGOLIAN DIGIT ZERO..MONGOLIAN DIGIT NINE
unicode_numeric_type(0x1946, 0x194F, 'Decimal').	% Nd  [10] LIMBU DIGIT ZERO..LIMBU DIGIT NINE
unicode_numeric_type(0x19D0, 0x19D9, 'Decimal').	% Nd  [10] NEW TAI LUE DIGIT ZERO..NEW TAI LUE DIGIT NINE
unicode_numeric_type(0x1A80, 0x1A89, 'Decimal').	% Nd  [10] TAI THAM HORA DIGIT ZERO..TAI THAM HORA DIGIT NINE
unicode_numeric_type(0x1A90, 0x1A99, 'Decimal').	% Nd  [10] TAI THAM THAM DIGIT ZERO..TAI THAM THAM DIGIT NINE
unicode_numeric_type(0x1B50, 0x1B59, 'Decimal').	% Nd  [10] BALINESE DIGIT ZERO..BALINESE DIGIT NINE
unicode_numeric_type(0x1BB0, 0x1BB9, 'Decimal').	% Nd  [10] SUNDANESE DIGIT ZERO..SUNDANESE DIGIT NINE
unicode_numeric_type(0x1C40, 0x1C49, 'Decimal').	% Nd  [10] LEPCHA DIGIT ZERO..LEPCHA DIGIT NINE
unicode_numeric_type(0x1C50, 0x1C59, 'Decimal').	% Nd  [10] OL CHIKI DIGIT ZERO..OL CHIKI DIGIT NINE
unicode_numeric_type(0xA620, 0xA629, 'Decimal').	% Nd  [10] VAI DIGIT ZERO..VAI DIGIT NINE
unicode_numeric_type(0xA8D0, 0xA8D9, 'Decimal').	% Nd  [10] SAURASHTRA DIGIT ZERO..SAURASHTRA DIGIT NINE
unicode_numeric_type(0xA900, 0xA909, 'Decimal').	% Nd  [10] KAYAH LI DIGIT ZERO..KAYAH LI DIGIT NINE
unicode_numeric_type(0xA9D0, 0xA9D9, 'Decimal').	% Nd  [10] JAVANESE DIGIT ZERO..JAVANESE DIGIT NINE
unicode_numeric_type(0xAA50, 0xAA59, 'Decimal').	% Nd  [10] CHAM DIGIT ZERO..CHAM DIGIT NINE
unicode_numeric_type(0xABF0, 0xABF9, 'Decimal').	% Nd  [10] MEETEI MAYEK DIGIT ZERO..MEETEI MAYEK DIGIT NINE
unicode_numeric_type(0xFF10, 0xFF19, 'Decimal').	% Nd  [10] FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
unicode_numeric_type(0x104A0, 0x104A9, 'Decimal').	% Nd  [10] OSMANYA DIGIT ZERO..OSMANYA DIGIT NINE
unicode_numeric_type(0x11066, 0x1106F, 'Decimal').	% Nd  [10] BRAHMI DIGIT ZERO..BRAHMI DIGIT NINE
unicode_numeric_type(0x110F0, 0x110F9, 'Decimal').	% Nd  [10] SORA SOMPENG DIGIT ZERO..SORA SOMPENG DIGIT NINE
unicode_numeric_type(0x11136, 0x1113F, 'Decimal').	% Nd  [10] CHAKMA DIGIT ZERO..CHAKMA DIGIT NINE
unicode_numeric_type(0x111D0, 0x111D9, 'Decimal').	% Nd  [10] SHARADA DIGIT ZERO..SHARADA DIGIT NINE
unicode_numeric_type(0x116C0, 0x116C9, 'Decimal').	% Nd  [10] TAKRI DIGIT ZERO..TAKRI DIGIT NINE
unicode_numeric_type(0x1D7CE, 0x1D7FF, 'Decimal').	% Nd  [50] MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE

% Total code points: 460

% EOF
