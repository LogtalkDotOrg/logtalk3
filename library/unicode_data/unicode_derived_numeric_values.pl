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
# DerivedNumericValues-6.2.0.txt
# Date: 2012-08-13, 19:20:22 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2012 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

# ================================================

# Derived Property:   Numeric_Value
#  The values are based on field 8 of UnicodeData.txt, plus the fields
#  kAccountingNumeric, kOtherNumeric, kPrimaryNumeric in the Unicode Han Database (Unihan).
#  The derivations for these values are as follows.
#     Numeric_Value = the value of kAccountingNumeric, kOtherNumeric, or kPrimaryNumeric, if they exist; otherwise
#     Numeric_Value = the value of field 8, if it exists; otherwise
#     Numeric_Value = NaN
#
# WARNING: Certain values, such as 0.16666667, are repeating fractions
# Although they are only printed with a limited number of decimal places
# in this file, they should be expressed to the limits of the precision
# available when used.
#
# The third field is empty; it used to be a copy of the numeric type.
#
# A fourth field was added to this extracted data as of
# Unicode 5.1.0, expressing the same numeric value either as
# a whole integer where possible or as a rational fraction, e.g. "1/6".
#
# @missing: 0000..10FFFF; NaN; ; NaN
*/

% ================================================

unicode_numerical_value(0x0F33, -0.5, -1/2).	% No       TIBETAN DIGIT HALF ZERO

% Total code points: 1

% ================================================

unicode_numerical_value(0x0030, 0.0, 0).	% Nd       DIGIT ZERO
unicode_numerical_value(0x0660, 0.0, 0).	% Nd       ARABIC-INDIC DIGIT ZERO
unicode_numerical_value(0x06F0, 0.0, 0).	% Nd       EXTENDED ARABIC-INDIC DIGIT ZERO
unicode_numerical_value(0x07C0, 0.0, 0).	% Nd       NKO DIGIT ZERO
unicode_numerical_value(0x0966, 0.0, 0).	% Nd       DEVANAGARI DIGIT ZERO
unicode_numerical_value(0x09E6, 0.0, 0).	% Nd       BENGALI DIGIT ZERO
unicode_numerical_value(0x0A66, 0.0, 0).	% Nd       GURMUKHI DIGIT ZERO
unicode_numerical_value(0x0AE6, 0.0, 0).	% Nd       GUJARATI DIGIT ZERO
unicode_numerical_value(0x0B66, 0.0, 0).	% Nd       ORIYA DIGIT ZERO
unicode_numerical_value(0x0BE6, 0.0, 0).	% Nd       TAMIL DIGIT ZERO
unicode_numerical_value(0x0C66, 0.0, 0).	% Nd       TELUGU DIGIT ZERO
unicode_numerical_value(0x0C78, 0.0, 0).	% No       TELUGU FRACTION DIGIT ZERO FOR ODD POWERS OF FOUR
unicode_numerical_value(0x0CE6, 0.0, 0).	% Nd       KANNADA DIGIT ZERO
unicode_numerical_value(0x0D66, 0.0, 0).	% Nd       MALAYALAM DIGIT ZERO
unicode_numerical_value(0x0E50, 0.0, 0).	% Nd       THAI DIGIT ZERO
unicode_numerical_value(0x0ED0, 0.0, 0).	% Nd       LAO DIGIT ZERO
unicode_numerical_value(0x0F20, 0.0, 0).	% Nd       TIBETAN DIGIT ZERO
unicode_numerical_value(0x1040, 0.0, 0).	% Nd       MYANMAR DIGIT ZERO
unicode_numerical_value(0x1090, 0.0, 0).	% Nd       MYANMAR SHAN DIGIT ZERO
unicode_numerical_value(0x17E0, 0.0, 0).	% Nd       KHMER DIGIT ZERO
unicode_numerical_value(0x17F0, 0.0, 0).	% No       KHMER SYMBOL LEK ATTAK SON
unicode_numerical_value(0x1810, 0.0, 0).	% Nd       MONGOLIAN DIGIT ZERO
unicode_numerical_value(0x1946, 0.0, 0).	% Nd       LIMBU DIGIT ZERO
unicode_numerical_value(0x19D0, 0.0, 0).	% Nd       NEW TAI LUE DIGIT ZERO
unicode_numerical_value(0x1A80, 0.0, 0).	% Nd       TAI THAM HORA DIGIT ZERO
unicode_numerical_value(0x1A90, 0.0, 0).	% Nd       TAI THAM THAM DIGIT ZERO
unicode_numerical_value(0x1B50, 0.0, 0).	% Nd       BALINESE DIGIT ZERO
unicode_numerical_value(0x1BB0, 0.0, 0).	% Nd       SUNDANESE DIGIT ZERO
unicode_numerical_value(0x1C40, 0.0, 0).	% Nd       LEPCHA DIGIT ZERO
unicode_numerical_value(0x1C50, 0.0, 0).	% Nd       OL CHIKI DIGIT ZERO
unicode_numerical_value(0x2070, 0.0, 0).	% No       SUPERSCRIPT ZERO
unicode_numerical_value(0x2080, 0.0, 0).	% No       SUBSCRIPT ZERO
unicode_numerical_value(0x2189, 0.0, 0).	% No       VULGAR FRACTION ZERO THIRDS
unicode_numerical_value(0x24EA, 0.0, 0).	% No       CIRCLED DIGIT ZERO
unicode_numerical_value(0x24FF, 0.0, 0).	% No       NEGATIVE CIRCLED DIGIT ZERO
unicode_numerical_value(0x3007, 0.0, 0).	% Nl       IDEOGRAPHIC NUMBER ZERO
unicode_numerical_value(0x96F6, 0.0, 0).	% Lo       CJK UNIFIED IDEOGRAPH-96F6
unicode_numerical_value(0xA620, 0.0, 0).	% Nd       VAI DIGIT ZERO
unicode_numerical_value(0xA6EF, 0.0, 0).	% Nl       BAMUM LETTER KOGHOM
unicode_numerical_value(0xA8D0, 0.0, 0).	% Nd       SAURASHTRA DIGIT ZERO
unicode_numerical_value(0xA900, 0.0, 0).	% Nd       KAYAH LI DIGIT ZERO
unicode_numerical_value(0xA9D0, 0.0, 0).	% Nd       JAVANESE DIGIT ZERO
unicode_numerical_value(0xAA50, 0.0, 0).	% Nd       CHAM DIGIT ZERO
unicode_numerical_value(0xABF0, 0.0, 0).	% Nd       MEETEI MAYEK DIGIT ZERO
unicode_numerical_value(0xF9B2, 0.0, 0).	% Lo       CJK COMPATIBILITY IDEOGRAPH-F9B2
unicode_numerical_value(0xFF10, 0.0, 0).	% Nd       FULLWIDTH DIGIT ZERO
unicode_numerical_value(0x1018A, 0.0, 0).	% No       GREEK ZERO SIGN
unicode_numerical_value(0x104A0, 0.0, 0).	% Nd       OSMANYA DIGIT ZERO
unicode_numerical_value(0x11066, 0.0, 0).	% Nd       BRAHMI DIGIT ZERO
unicode_numerical_value(0x110F0, 0.0, 0).	% Nd       SORA SOMPENG DIGIT ZERO
unicode_numerical_value(0x11136, 0.0, 0).	% Nd       CHAKMA DIGIT ZERO
unicode_numerical_value(0x111D0, 0.0, 0).	% Nd       SHARADA DIGIT ZERO
unicode_numerical_value(0x116C0, 0.0, 0).	% Nd       TAKRI DIGIT ZERO
unicode_numerical_value(0x1D7CE, 0.0, 0).	% Nd       MATHEMATICAL BOLD DIGIT ZERO
unicode_numerical_value(0x1D7D8, 0.0, 0).	% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO
unicode_numerical_value(0x1D7E2, 0.0, 0).	% Nd       MATHEMATICAL SANS-SERIF DIGIT ZERO
unicode_numerical_value(0x1D7EC, 0.0, 0).	% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT ZERO
unicode_numerical_value(0x1D7F6, 0.0, 0).	% Nd       MATHEMATICAL MONOSPACE DIGIT ZERO
%0x1F100..1F101  ; 0.0, 0).	% No   [2] DIGIT ZERO FULL STOP..DIGIT ZERO COMMA
unicode_numerical_value(0x1F100, 0.0, 0).	% No   DIGIT ZERO FULL STOP
unicode_numerical_value(0x1F101, 0.0, 0).	% No   DIGIT ZERO COMMA

% Total code points: 60

% ================================================

unicode_numerical_value(0x09F4, 0.0625, 1/16).	% No       BENGALI CURRENCY NUMERATOR ONE
unicode_numerical_value(0x0B75, 0.0625, 1/16).	% No       ORIYA FRACTION ONE SIXTEENTH
unicode_numerical_value(0xA833, 0.0625, 1/16).	% No       NORTH INDIC FRACTION ONE SIXTEENTH

% Total code points: 3

% ================================================

unicode_numerical_value(0x2152, 0.1, 1/10).	% No       VULGAR FRACTION ONE TENTH

% Total code points: 1

% ================================================

unicode_numerical_value(0x2151, 0.11111111, 1/9).	% No       VULGAR FRACTION ONE NINTH

% Total code points: 1

% ================================================

unicode_numerical_value(0x09F5, 0.125, 1/8).	% No       BENGALI CURRENCY NUMERATOR TWO
unicode_numerical_value(0x0B76, 0.125, 1/8).	% No       ORIYA FRACTION ONE EIGHTH
unicode_numerical_value(0x215B, 0.125, 1/8).	% No       VULGAR FRACTION ONE EIGHTH
unicode_numerical_value(0xA834, 0.125, 1/8).	% No       NORTH INDIC FRACTION ONE EIGHTH
unicode_numerical_value(0x1245F, 0.125, 1/8).	% Nl       CUNEIFORM NUMERIC SIGN ONE EIGHTH ASH

% Total code points: 5

% ================================================

unicode_numerical_value(0x2150, 0.14285714, 1/7).	% No       VULGAR FRACTION ONE SEVENTH

% Total code points: 1

% ================================================

unicode_numerical_value(0x2159, 0.16666667, 1/6).	% No       VULGAR FRACTION ONE SIXTH
unicode_numerical_value(0x12461, 0.16666667, 1/6).	% Nl       CUNEIFORM NUMERIC SIGN OLD ASSYRIAN ONE SIXTH

% Total code points: 2

% ================================================

unicode_numerical_value(0x09F6, 0.1875, 3/16).	% No       BENGALI CURRENCY NUMERATOR THREE
unicode_numerical_value(0x0B77, 0.1875, 3/16).	% No       ORIYA FRACTION THREE SIXTEENTHS
unicode_numerical_value(0xA835, 0.1875, 3/16).	% No       NORTH INDIC FRACTION THREE SIXTEENTHS

% Total code points: 3

% ================================================

unicode_numerical_value(0x2155, 0.2, 1/5).	% No       VULGAR FRACTION ONE FIFTH

% Total code points: 1

% ================================================

unicode_numerical_value(0x00BC, 0.25, 1/4).	% No       VULGAR FRACTION ONE QUARTER
unicode_numerical_value(0x09F7, 0.25, 1/4).	% No       BENGALI CURRENCY NUMERATOR FOUR
unicode_numerical_value(0x0B72, 0.25, 1/4).	% No       ORIYA FRACTION ONE QUARTER
unicode_numerical_value(0x0D73, 0.25, 1/4).	% No       MALAYALAM FRACTION ONE QUARTER
unicode_numerical_value(0xA830, 0.25, 1/4).	% No       NORTH INDIC FRACTION ONE QUARTER
unicode_numerical_value(0x10140, 0.25, 1/4).	% Nl       GREEK ACROPHONIC ATTIC ONE QUARTER
unicode_numerical_value(0x10E7C, 0.25, 1/4).	% No       RUMI FRACTION ONE QUARTER
unicode_numerical_value(0x12460, 0.25, 1/4).	% Nl       CUNEIFORM NUMERIC SIGN ONE QUARTER ASH
unicode_numerical_value(0x12462, 0.25, 1/4).	% Nl       CUNEIFORM NUMERIC SIGN OLD ASSYRIAN ONE QUARTER

% Total code points: 9

% ================================================

unicode_numerical_value(0x2153, 0.33333333, 1/3).	% No       VULGAR FRACTION ONE THIRD
unicode_numerical_value(0x10E7D, 0.33333333, 1/3).	% No       RUMI FRACTION ONE THIRD
unicode_numerical_value(0x1245A, 0.33333333, 1/3).	% Nl       CUNEIFORM NUMERIC SIGN ONE THIRD DISH
unicode_numerical_value(0x1245D, 0.33333333, 1/3).	% Nl       CUNEIFORM NUMERIC SIGN ONE THIRD VARIANT FORM A

% Total code points: 4

% ================================================

unicode_numerical_value(0x215C, 0.375, 3/8).	% No       VULGAR FRACTION THREE EIGHTHS

% Total code points: 1

% ================================================

unicode_numerical_value(0x2156, 0.4, 2/5).	% No       VULGAR FRACTION TWO FIFTHS

% Total code points: 1

% ================================================

unicode_numerical_value(0x00BD, 0.5, 1/2).	% No       VULGAR FRACTION ONE HALF
unicode_numerical_value(0x0B73, 0.5, 1/2).	% No       ORIYA FRACTION ONE HALF
unicode_numerical_value(0x0D74, 0.5, 1/2).	% No       MALAYALAM FRACTION ONE HALF
unicode_numerical_value(0x0F2A, 0.5, 1/2).	% No       TIBETAN DIGIT HALF ONE
unicode_numerical_value(0x2CFD, 0.5, 1/2).	% No       COPTIC FRACTION ONE HALF
unicode_numerical_value(0xA831, 0.5, 1/2).	% No       NORTH INDIC FRACTION ONE HALF
unicode_numerical_value(0x10141, 0.5, 1/2).	% Nl       GREEK ACROPHONIC ATTIC ONE HALF
%10175..10176  ; 0.5, 1/2).	% No   [2] GREEK ONE HALF SIGN..GREEK ONE HALF SIGN ALTERNATE FORM
unicode_numerical_value(0x10175, 0.5, 1/2).	% No       GREEK ONE HALF SIGN
unicode_numerical_value(0x10176, 0.5, 1/2).	% No       GREEK ONE HALF SIGN ALTERNATE FORM
unicode_numerical_value(0x10E7B, 0.5, 1/2).	% No       RUMI FRACTION ONE HALF

% Total code points: 10

% ================================================

unicode_numerical_value(0x2157, 0.6, 3/5).	% No       VULGAR FRACTION THREE FIFTHS

% Total code points: 1

% ================================================

unicode_numerical_value(0x215D, 0.625, 5/8).	% No       VULGAR FRACTION FIVE EIGHTHS

% Total code points: 1

% ================================================

unicode_numerical_value(0x2154, 0.66666667, 2/3).	% No       VULGAR FRACTION TWO THIRDS
unicode_numerical_value(0x10177, 0.66666667, 2/3).	% No       GREEK TWO THIRDS SIGN
unicode_numerical_value(0x10E7E, 0.66666667, 2/3).	% No       RUMI FRACTION TWO THIRDS
unicode_numerical_value(0x1245B, 0.66666667, 2/3).	% Nl       CUNEIFORM NUMERIC SIGN TWO THIRDS DISH
unicode_numerical_value(0x1245E, 0.66666667, 2/3).	% Nl       CUNEIFORM NUMERIC SIGN TWO THIRDS VARIANT FORM A

% Total code points: 5

% ================================================

unicode_numerical_value(0x00BE, 0.75, 3/4).	% No       VULGAR FRACTION THREE QUARTERS
unicode_numerical_value(0x09F8, 0.75, 3/4).	% No       BENGALI CURRENCY NUMERATOR ONE LESS THAN THE DENOMINATOR
unicode_numerical_value(0x0B74, 0.75, 3/4).	% No       ORIYA FRACTION THREE QUARTERS
unicode_numerical_value(0x0D75, 0.75, 3/4).	% No       MALAYALAM FRACTION THREE QUARTERS
unicode_numerical_value(0xA832, 0.75, 3/4).	% No       NORTH INDIC FRACTION THREE QUARTERS
unicode_numerical_value(0x10178, 0.75, 3/4).	% No       GREEK THREE QUARTERS SIGN

% Total code points: 6

% ================================================

unicode_numerical_value(0x2158, 0.8, 4/5).	% No       VULGAR FRACTION FOUR FIFTHS

% Total code points: 1

% ================================================

unicode_numerical_value(0x215A, 0.83333333, 5/6).	% No       VULGAR FRACTION FIVE SIXTHS
unicode_numerical_value(0x1245C, 0.83333333, 5/6).	% Nl       CUNEIFORM NUMERIC SIGN FIVE SIXTHS DISH

% Total code points: 2

% ================================================

unicode_numerical_value(0x215E, 0.875, 7/8).	% No       VULGAR FRACTION SEVEN EIGHTHS

% Total code points: 1

% ================================================

unicode_numerical_value(0x0031, 1.0, 1).	% Nd       DIGIT ONE
unicode_numerical_value(0x00B9, 1.0, 1).	% No       SUPERSCRIPT ONE
unicode_numerical_value(0x0661, 1.0, 1).	% Nd       ARABIC-INDIC DIGIT ONE
unicode_numerical_value(0x06F1, 1.0, 1).	% Nd       EXTENDED ARABIC-INDIC DIGIT ONE
unicode_numerical_value(0x07C1, 1.0, 1).	% Nd       NKO DIGIT ONE
unicode_numerical_value(0x0967, 1.0, 1).	% Nd       DEVANAGARI DIGIT ONE
unicode_numerical_value(0x09E7, 1.0, 1).	% Nd       BENGALI DIGIT ONE
unicode_numerical_value(0x0A67, 1.0, 1).	% Nd       GURMUKHI DIGIT ONE
unicode_numerical_value(0x0AE7, 1.0, 1).	% Nd       GUJARATI DIGIT ONE
unicode_numerical_value(0x0B67, 1.0, 1).	% Nd       ORIYA DIGIT ONE
unicode_numerical_value(0x0BE7, 1.0, 1).	% Nd       TAMIL DIGIT ONE
unicode_numerical_value(0x0C67, 1.0, 1).	% Nd       TELUGU DIGIT ONE
unicode_numerical_value(0x0C79, 1.0, 1).	% No       TELUGU FRACTION DIGIT ONE FOR ODD POWERS OF FOUR
unicode_numerical_value(0x0C7C, 1.0, 1).	% No       TELUGU FRACTION DIGIT ONE FOR EVEN POWERS OF FOUR
unicode_numerical_value(0x0CE7, 1.0, 1).	% Nd       KANNADA DIGIT ONE
unicode_numerical_value(0x0D67, 1.0, 1).	% Nd       MALAYALAM DIGIT ONE
unicode_numerical_value(0x0E51, 1.0, 1).	% Nd       THAI DIGIT ONE
unicode_numerical_value(0x0ED1, 1.0, 1).	% Nd       LAO DIGIT ONE
unicode_numerical_value(0x0F21, 1.0, 1).	% Nd       TIBETAN DIGIT ONE
unicode_numerical_value(0x1041, 1.0, 1).	% Nd       MYANMAR DIGIT ONE
unicode_numerical_value(0x1091, 1.0, 1).	% Nd       MYANMAR SHAN DIGIT ONE
unicode_numerical_value(0x1369, 1.0, 1).	% No       ETHIOPIC DIGIT ONE
unicode_numerical_value(0x17E1, 1.0, 1).	% Nd       KHMER DIGIT ONE
unicode_numerical_value(0x17F1, 1.0, 1).	% No       KHMER SYMBOL LEK ATTAK MUOY
unicode_numerical_value(0x1811, 1.0, 1).	% Nd       MONGOLIAN DIGIT ONE
unicode_numerical_value(0x1947, 1.0, 1).	% Nd       LIMBU DIGIT ONE
unicode_numerical_value(0x19D1, 1.0, 1).	% Nd       NEW TAI LUE DIGIT ONE
unicode_numerical_value(0x19DA, 1.0, 1).	% No       NEW TAI LUE THAM DIGIT ONE
unicode_numerical_value(0x1A81, 1.0, 1).	% Nd       TAI THAM HORA DIGIT ONE
unicode_numerical_value(0x1A91, 1.0, 1).	% Nd       TAI THAM THAM DIGIT ONE
unicode_numerical_value(0x1B51, 1.0, 1).	% Nd       BALINESE DIGIT ONE
unicode_numerical_value(0x1BB1, 1.0, 1).	% Nd       SUNDANESE DIGIT ONE
unicode_numerical_value(0x1C41, 1.0, 1).	% Nd       LEPCHA DIGIT ONE
unicode_numerical_value(0x1C51, 1.0, 1).	% Nd       OL CHIKI DIGIT ONE
unicode_numerical_value(0x2081, 1.0, 1).	% No       SUBSCRIPT ONE
unicode_numerical_value(0x215F, 1.0, 1).	% No       FRACTION NUMERATOR ONE
unicode_numerical_value(0x2160, 1.0, 1).	% Nl       ROMAN NUMERAL ONE
unicode_numerical_value(0x2170, 1.0, 1).	% Nl       SMALL ROMAN NUMERAL ONE
unicode_numerical_value(0x2460, 1.0, 1).	% No       CIRCLED DIGIT ONE
unicode_numerical_value(0x2474, 1.0, 1).	% No       PARENTHESIZED DIGIT ONE
unicode_numerical_value(0x2488, 1.0, 1).	% No       DIGIT ONE FULL STOP
unicode_numerical_value(0x24F5, 1.0, 1).	% No       DOUBLE CIRCLED DIGIT ONE
unicode_numerical_value(0x2776, 1.0, 1).	% No       DINGBAT NEGATIVE CIRCLED DIGIT ONE
unicode_numerical_value(0x2780, 1.0, 1).	% No       DINGBAT CIRCLED SANS-SERIF DIGIT ONE
unicode_numerical_value(0x278A, 1.0, 1).	% No       DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT ONE
unicode_numerical_value(0x3021, 1.0, 1).	% Nl       HANGZHOU NUMERAL ONE
unicode_numerical_value(0x3192, 1.0, 1).	% No       IDEOGRAPHIC ANNOTATION ONE MARK
unicode_numerical_value(0x3220, 1.0, 1).	% No       PARENTHESIZED IDEOGRAPH ONE
unicode_numerical_value(0x3280, 1.0, 1).	% No       CIRCLED IDEOGRAPH ONE
unicode_numerical_value(0x4E00, 1.0, 1).	% Lo       CJK UNIFIED IDEOGRAPH-4E00
unicode_numerical_value(0x58F1, 1.0, 1).	% Lo       CJK UNIFIED IDEOGRAPH-58F1
unicode_numerical_value(0x58F9, 1.0, 1).	% Lo       CJK UNIFIED IDEOGRAPH-58F9
unicode_numerical_value(0x5E7A, 1.0, 1).	% Lo       CJK UNIFIED IDEOGRAPH-5E7A
unicode_numerical_value(0x5F0C, 1.0, 1).	% Lo       CJK UNIFIED IDEOGRAPH-5F0C
unicode_numerical_value(0xA621, 1.0, 1).	% Nd       VAI DIGIT ONE
unicode_numerical_value(0xA6E6, 1.0, 1).	% Nl       BAMUM LETTER MO
unicode_numerical_value(0xA8D1, 1.0, 1).	% Nd       SAURASHTRA DIGIT ONE
unicode_numerical_value(0xA901, 1.0, 1).	% Nd       KAYAH LI DIGIT ONE
unicode_numerical_value(0xA9D1, 1.0, 1).	% Nd       JAVANESE DIGIT ONE
unicode_numerical_value(0xAA51, 1.0, 1).	% Nd       CHAM DIGIT ONE
unicode_numerical_value(0xABF1, 1.0, 1).	% Nd       MEETEI MAYEK DIGIT ONE
unicode_numerical_value(0xFF11, 1.0, 1).	% Nd       FULLWIDTH DIGIT ONE
unicode_numerical_value(0x10107, 1.0, 1).	% No       AEGEAN NUMBER ONE
unicode_numerical_value(0x10142, 1.0, 1).	% Nl       GREEK ACROPHONIC ATTIC ONE DRACHMA
%10158..1015A  ; 1.0, 1).	% Nl   [3] GREEK ACROPHONIC HERAEUM ONE PLETHRON..GREEK ACROPHONIC HERMIONIAN ONE
unicode_numerical_value(0x10158, 1.0, 1).	% Nl       GREEK ACROPHONIC HERAEUM ONE PLETHRON
unicode_numerical_value(0x10159, 1.0, 1).	% Nl       GREEK ACROPHONIC THESPIAN ONE 
unicode_numerical_value(0x1015A, 1.0, 1).	% Nl       GREEK ACROPHONIC HERMIONIAN ONE
unicode_numerical_value(0x10320, 1.0, 1).	% No       OLD ITALIC NUMERAL ONE
unicode_numerical_value(0x103D1, 1.0, 1).	% Nl       OLD PERSIAN NUMBER ONE
unicode_numerical_value(0x104A1, 1.0, 1).	% Nd       OSMANYA DIGIT ONE
unicode_numerical_value(0x10858, 1.0, 1).	% No       IMPERIAL ARAMAIC NUMBER ONE
unicode_numerical_value(0x10916, 1.0, 1).	% No       PHOENICIAN NUMBER ONE
unicode_numerical_value(0x10A40, 1.0, 1).	% No       KHAROSHTHI DIGIT ONE
unicode_numerical_value(0x10A7D, 1.0, 1).	% No       OLD SOUTH ARABIAN NUMBER ONE
unicode_numerical_value(0x10B58, 1.0, 1).	% No       INSCRIPTIONAL PARTHIAN NUMBER ONE
unicode_numerical_value(0x10B78, 1.0, 1).	% No       INSCRIPTIONAL PAHLAVI NUMBER ONE
unicode_numerical_value(0x10E60, 1.0, 1).	% No       RUMI DIGIT ONE
unicode_numerical_value(0x11052, 1.0, 1).	% No       BRAHMI NUMBER ONE
unicode_numerical_value(0x11067, 1.0, 1).	% Nd       BRAHMI DIGIT ONE
unicode_numerical_value(0x110F1, 1.0, 1).	% Nd       SORA SOMPENG DIGIT ONE
unicode_numerical_value(0x11137, 1.0, 1).	% Nd       CHAKMA DIGIT ONE
unicode_numerical_value(0x111D1, 1.0, 1).	% Nd       SHARADA DIGIT ONE
unicode_numerical_value(0x116C1, 1.0, 1).	% Nd       TAKRI DIGIT ONE
unicode_numerical_value(0x12415, 1.0, 1).	% Nl       CUNEIFORM NUMERIC SIGN ONE GESH2
unicode_numerical_value(0x1241E, 1.0, 1).	% Nl       CUNEIFORM NUMERIC SIGN ONE GESHU
unicode_numerical_value(0x1242C, 1.0, 1).	% Nl       CUNEIFORM NUMERIC SIGN ONE SHARU
unicode_numerical_value(0x12434, 1.0, 1).	% Nl       CUNEIFORM NUMERIC SIGN ONE BURU
unicode_numerical_value(0x1244F, 1.0, 1).	% Nl       CUNEIFORM NUMERIC SIGN ONE BAN2
unicode_numerical_value(0x12458, 1.0, 1).	% Nl       CUNEIFORM NUMERIC SIGN ONE ESHE3
unicode_numerical_value(0x1D360, 1.0, 1).	% No       COUNTING ROD UNIT DIGIT ONE
unicode_numerical_value(0x1D7CF, 1.0, 1).	% Nd       MATHEMATICAL BOLD DIGIT ONE
unicode_numerical_value(0x1D7D9, 1.0, 1).	% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT ONE
unicode_numerical_value(0x1D7E3, 1.0, 1).	% Nd       MATHEMATICAL SANS-SERIF DIGIT ONE
unicode_numerical_value(0x1D7ED, 1.0, 1).	% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT ONE
unicode_numerical_value(0x1D7F7, 1.0, 1).	% Nd       MATHEMATICAL MONOSPACE DIGIT ONE
unicode_numerical_value(0x1F102, 1.0, 1).	% No       DIGIT ONE COMMA
unicode_numerical_value(0x2092A, 1.0, 1).	% Lo       CJK UNIFIED IDEOGRAPH-2092A

% Total code points: 97

% ================================================

unicode_numerical_value(0x0F2B, 1.5, 3/2).	% No       TIBETAN DIGIT HALF TWO

% Total code points: 1

% ================================================

unicode_numerical_value(0x0032, 2.0, 2).	% Nd       DIGIT TWO
unicode_numerical_value(0x00B2, 2.0, 2).	% No       SUPERSCRIPT TWO
unicode_numerical_value(0x0662, 2.0, 2).	% Nd       ARABIC-INDIC DIGIT TWO
unicode_numerical_value(0x06F2, 2.0, 2).	% Nd       EXTENDED ARABIC-INDIC DIGIT TWO
unicode_numerical_value(0x07C2, 2.0, 2).	% Nd       NKO DIGIT TWO
unicode_numerical_value(0x0968, 2.0, 2).	% Nd       DEVANAGARI DIGIT TWO
unicode_numerical_value(0x09E8, 2.0, 2).	% Nd       BENGALI DIGIT TWO
unicode_numerical_value(0x0A68, 2.0, 2).	% Nd       GURMUKHI DIGIT TWO
unicode_numerical_value(0x0AE8, 2.0, 2).	% Nd       GUJARATI DIGIT TWO
unicode_numerical_value(0x0B68, 2.0, 2).	% Nd       ORIYA DIGIT TWO
unicode_numerical_value(0x0BE8, 2.0, 2).	% Nd       TAMIL DIGIT TWO
unicode_numerical_value(0x0C68, 2.0, 2).	% Nd       TELUGU DIGIT TWO
unicode_numerical_value(0x0C7A, 2.0, 2).	% No       TELUGU FRACTION DIGIT TWO FOR ODD POWERS OF FOUR
unicode_numerical_value(0x0C7D, 2.0, 2).	% No       TELUGU FRACTION DIGIT TWO FOR EVEN POWERS OF FOUR
unicode_numerical_value(0x0CE8, 2.0, 2).	% Nd       KANNADA DIGIT TWO
unicode_numerical_value(0x0D68, 2.0, 2).	% Nd       MALAYALAM DIGIT TWO
unicode_numerical_value(0x0E52, 2.0, 2).	% Nd       THAI DIGIT TWO
unicode_numerical_value(0x0ED2, 2.0, 2).	% Nd       LAO DIGIT TWO
unicode_numerical_value(0x0F22, 2.0, 2).	% Nd       TIBETAN DIGIT TWO
unicode_numerical_value(0x1042, 2.0, 2).	% Nd       MYANMAR DIGIT TWO
unicode_numerical_value(0x1092, 2.0, 2).	% Nd       MYANMAR SHAN DIGIT TWO
unicode_numerical_value(0x136A, 2.0, 2).	% No       ETHIOPIC DIGIT TWO
unicode_numerical_value(0x17E2, 2.0, 2).	% Nd       KHMER DIGIT TWO
unicode_numerical_value(0x17F2, 2.0, 2).	% No       KHMER SYMBOL LEK ATTAK PII
unicode_numerical_value(0x1812, 2.0, 2).	% Nd       MONGOLIAN DIGIT TWO
unicode_numerical_value(0x1948, 2.0, 2).	% Nd       LIMBU DIGIT TWO
unicode_numerical_value(0x19D2, 2.0, 2).	% Nd       NEW TAI LUE DIGIT TWO
unicode_numerical_value(0x1A82, 2.0, 2).	% Nd       TAI THAM HORA DIGIT TWO
unicode_numerical_value(0x1A92, 2.0, 2).	% Nd       TAI THAM THAM DIGIT TWO
unicode_numerical_value(0x1B52, 2.0, 2).	% Nd       BALINESE DIGIT TWO
unicode_numerical_value(0x1BB2, 2.0, 2).	% Nd       SUNDANESE DIGIT TWO
unicode_numerical_value(0x1C42, 2.0, 2).	% Nd       LEPCHA DIGIT TWO
unicode_numerical_value(0x1C52, 2.0, 2).	% Nd       OL CHIKI DIGIT TWO
unicode_numerical_value(0x2082, 2.0, 2).	% No       SUBSCRIPT TWO
unicode_numerical_value(0x2161, 2.0, 2).	% Nl       ROMAN NUMERAL TWO
unicode_numerical_value(0x2171, 2.0, 2).	% Nl       SMALL ROMAN NUMERAL TWO
unicode_numerical_value(0x2461, 2.0, 2).	% No       CIRCLED DIGIT TWO
unicode_numerical_value(0x2475, 2.0, 2).	% No       PARENTHESIZED DIGIT TWO
unicode_numerical_value(0x2489, 2.0, 2).	% No       DIGIT TWO FULL STOP
unicode_numerical_value(0x24F6, 2.0, 2).	% No       DOUBLE CIRCLED DIGIT TWO
unicode_numerical_value(0x2777, 2.0, 2).	% No       DINGBAT NEGATIVE CIRCLED DIGIT TWO
unicode_numerical_value(0x2781, 2.0, 2).	% No       DINGBAT CIRCLED SANS-SERIF DIGIT TWO
unicode_numerical_value(0x278B, 2.0, 2).	% No       DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT TWO
unicode_numerical_value(0x3022, 2.0, 2).	% Nl       HANGZHOU NUMERAL TWO
unicode_numerical_value(0x3193, 2.0, 2).	% No       IDEOGRAPHIC ANNOTATION TWO MARK
unicode_numerical_value(0x3221, 2.0, 2).	% No       PARENTHESIZED IDEOGRAPH TWO
unicode_numerical_value(0x3281, 2.0, 2).	% No       CIRCLED IDEOGRAPH TWO
unicode_numerical_value(0x3483, 2.0, 2).	% Lo       CJK UNIFIED IDEOGRAPH-3483
unicode_numerical_value(0x4E8C, 2.0, 2).	% Lo       CJK UNIFIED IDEOGRAPH-4E8C
unicode_numerical_value(0x5169, 2.0, 2).	% Lo       CJK UNIFIED IDEOGRAPH-5169
unicode_numerical_value(0x5F0D, 2.0, 2).	% Lo       CJK UNIFIED IDEOGRAPH-5F0D
unicode_numerical_value(0x5F10, 2.0, 2).	% Lo       CJK UNIFIED IDEOGRAPH-5F10
unicode_numerical_value(0x8CAE, 2.0, 2).	% Lo       CJK UNIFIED IDEOGRAPH-8CAE
unicode_numerical_value(0x8CB3, 2.0, 2).	% Lo       CJK UNIFIED IDEOGRAPH-8CB3
unicode_numerical_value(0x8D30, 2.0, 2).	% Lo       CJK UNIFIED IDEOGRAPH-8D30
unicode_numerical_value(0xA622, 2.0, 2).	% Nd       VAI DIGIT TWO
unicode_numerical_value(0xA6E7, 2.0, 2).	% Nl       BAMUM LETTER MBAA
unicode_numerical_value(0xA8D2, 2.0, 2).	% Nd       SAURASHTRA DIGIT TWO
unicode_numerical_value(0xA902, 2.0, 2).	% Nd       KAYAH LI DIGIT TWO
unicode_numerical_value(0xA9D2, 2.0, 2).	% Nd       JAVANESE DIGIT TWO
unicode_numerical_value(0xAA52, 2.0, 2).	% Nd       CHAM DIGIT TWO
unicode_numerical_value(0xABF2, 2.0, 2).	% Nd       MEETEI MAYEK DIGIT TWO
unicode_numerical_value(0xF978, 2.0, 2).	% Lo       CJK COMPATIBILITY IDEOGRAPH-F978
unicode_numerical_value(0xFF12, 2.0, 2).	% Nd       FULLWIDTH DIGIT TWO
unicode_numerical_value(0x10108, 2.0, 2).	% No       AEGEAN NUMBER TWO
%1015B..1015E  ; 2.0, 2).	% Nl   [4] GREEK ACROPHONIC EPIDAUREAN TWO..GREEK ACROPHONIC EPIDAUREAN TWO DRACHMAS
unicode_numerical_value(0x1015B, 2.0, 2).	% Nl       GREEK ACROPHONIC EPIDAUREAN TWO
unicode_numerical_value(0x1015C, 2.0, 2).	% Nl       GREEK ACROPHONIC THESPIAN TWO
unicode_numerical_value(0x1015D, 2.0, 2).	% Nl       GREEK ACROPHONIC CYRENAIC TWO DRACHMAS
unicode_numerical_value(0x1015E, 2.0, 2).	% Nl       GREEK ACROPHONIC EPIDAUREAN TWO DRACHMAS
unicode_numerical_value(0x103D2, 2.0, 2).	% Nl       OLD PERSIAN NUMBER TWO
unicode_numerical_value(0x104A2, 2.0, 2).	% Nd       OSMANYA DIGIT TWO
unicode_numerical_value(0x10859, 2.0, 2).	% No       IMPERIAL ARAMAIC NUMBER TWO
unicode_numerical_value(0x1091A, 2.0, 2).	% No       PHOENICIAN NUMBER TWO
unicode_numerical_value(0x10A41, 2.0, 2).	% No       KHAROSHTHI DIGIT TWO
unicode_numerical_value(0x10B59, 2.0, 2).	% No       INSCRIPTIONAL PARTHIAN NUMBER TWO
unicode_numerical_value(0x10B79, 2.0, 2).	% No       INSCRIPTIONAL PAHLAVI NUMBER TWO
unicode_numerical_value(0x10E61, 2.0, 2).	% No       RUMI DIGIT TWO
unicode_numerical_value(0x11053, 2.0, 2).	% No       BRAHMI NUMBER TWO
unicode_numerical_value(0x11068, 2.0, 2).	% Nd       BRAHMI DIGIT TWO
unicode_numerical_value(0x110F2, 2.0, 2).	% Nd       SORA SOMPENG DIGIT TWO
unicode_numerical_value(0x11138, 2.0, 2).	% Nd       CHAKMA DIGIT TWO
unicode_numerical_value(0x111D2, 2.0, 2).	% Nd       SHARADA DIGIT TWO
unicode_numerical_value(0x116C2, 2.0, 2).	% Nd       TAKRI DIGIT TWO
unicode_numerical_value(0x12400, 2.0, 2).	% Nl       CUNEIFORM NUMERIC SIGN TWO ASH
unicode_numerical_value(0x12416, 2.0, 2).	% Nl       CUNEIFORM NUMERIC SIGN TWO GESH2
unicode_numerical_value(0x1241F, 2.0, 2).	% Nl       CUNEIFORM NUMERIC SIGN TWO GESHU
unicode_numerical_value(0x12423, 2.0, 2).	% Nl       CUNEIFORM NUMERIC SIGN TWO SHAR2
unicode_numerical_value(0x1242D, 2.0, 2).	% Nl       CUNEIFORM NUMERIC SIGN TWO SHARU
unicode_numerical_value(0x12435, 2.0, 2).	% Nl       CUNEIFORM NUMERIC SIGN TWO BURU
unicode_numerical_value(0x1244A, 2.0, 2).	% Nl       CUNEIFORM NUMERIC SIGN TWO ASH TENU
unicode_numerical_value(0x12450, 2.0, 2).	% Nl       CUNEIFORM NUMERIC SIGN TWO BAN2
unicode_numerical_value(0x12459, 2.0, 2).	% Nl       CUNEIFORM NUMERIC SIGN TWO ESHE3
unicode_numerical_value(0x1D361, 2.0, 2).	% No       COUNTING ROD UNIT DIGIT TWO
unicode_numerical_value(0x1D7D0, 2.0, 2).	% Nd       MATHEMATICAL BOLD DIGIT TWO
unicode_numerical_value(0x1D7DA, 2.0, 2).	% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT TWO
unicode_numerical_value(0x1D7E4, 2.0, 2).	% Nd       MATHEMATICAL SANS-SERIF DIGIT TWO
unicode_numerical_value(0x1D7EE, 2.0, 2).	% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT TWO
unicode_numerical_value(0x1D7F8, 2.0, 2).	% Nd       MATHEMATICAL MONOSPACE DIGIT TWO
unicode_numerical_value(0x1F103, 2.0, 2).	% No       DIGIT TWO COMMA
unicode_numerical_value(0x22390, 2.0, 2).	% Lo       CJK UNIFIED IDEOGRAPH-22390

% Total code points: 100

% ================================================

unicode_numerical_value(0x0F2C, 2.5, 5/2).	% No       TIBETAN DIGIT HALF THREE

% Total code points: 1

% ================================================

unicode_numerical_value(0x0033, 3.0, 3).	% Nd       DIGIT THREE
unicode_numerical_value(0x00B3, 3.0, 3).	% No       SUPERSCRIPT THREE
unicode_numerical_value(0x0663, 3.0, 3).	% Nd       ARABIC-INDIC DIGIT THREE
unicode_numerical_value(0x06F3, 3.0, 3).	% Nd       EXTENDED ARABIC-INDIC DIGIT THREE
unicode_numerical_value(0x07C3, 3.0, 3).	% Nd       NKO DIGIT THREE
unicode_numerical_value(0x0969, 3.0, 3).	% Nd       DEVANAGARI DIGIT THREE
unicode_numerical_value(0x09E9, 3.0, 3).	% Nd       BENGALI DIGIT THREE
unicode_numerical_value(0x0A69, 3.0, 3).	% Nd       GURMUKHI DIGIT THREE
unicode_numerical_value(0x0AE9, 3.0, 3).	% Nd       GUJARATI DIGIT THREE
unicode_numerical_value(0x0B69, 3.0, 3).	% Nd       ORIYA DIGIT THREE
unicode_numerical_value(0x0BE9, 3.0, 3).	% Nd       TAMIL DIGIT THREE
unicode_numerical_value(0x0C69, 3.0, 3).	% Nd       TELUGU DIGIT THREE
unicode_numerical_value(0x0C7B, 3.0, 3).	% No       TELUGU FRACTION DIGIT THREE FOR ODD POWERS OF FOUR
unicode_numerical_value(0x0C7E, 3.0, 3).	% No       TELUGU FRACTION DIGIT THREE FOR EVEN POWERS OF FOUR
unicode_numerical_value(0x0CE9, 3.0, 3).	% Nd       KANNADA DIGIT THREE
unicode_numerical_value(0x0D69, 3.0, 3).	% Nd       MALAYALAM DIGIT THREE
unicode_numerical_value(0x0E53, 3.0, 3).	% Nd       THAI DIGIT THREE
unicode_numerical_value(0x0ED3, 3.0, 3).	% Nd       LAO DIGIT THREE
unicode_numerical_value(0x0F23, 3.0, 3).	% Nd       TIBETAN DIGIT THREE
unicode_numerical_value(0x1043, 3.0, 3).	% Nd       MYANMAR DIGIT THREE
unicode_numerical_value(0x1093, 3.0, 3).	% Nd       MYANMAR SHAN DIGIT THREE
unicode_numerical_value(0x136B, 3.0, 3).	% No       ETHIOPIC DIGIT THREE
unicode_numerical_value(0x17E3, 3.0, 3).	% Nd       KHMER DIGIT THREE
unicode_numerical_value(0x17F3, 3.0, 3).	% No       KHMER SYMBOL LEK ATTAK BEI
unicode_numerical_value(0x1813, 3.0, 3).	% Nd       MONGOLIAN DIGIT THREE
unicode_numerical_value(0x1949, 3.0, 3).	% Nd       LIMBU DIGIT THREE
unicode_numerical_value(0x19D3, 3.0, 3).	% Nd       NEW TAI LUE DIGIT THREE
unicode_numerical_value(0x1A83, 3.0, 3).	% Nd       TAI THAM HORA DIGIT THREE
unicode_numerical_value(0x1A93, 3.0, 3).	% Nd       TAI THAM THAM DIGIT THREE
unicode_numerical_value(0x1B53, 3.0, 3).	% Nd       BALINESE DIGIT THREE
unicode_numerical_value(0x1BB3, 3.0, 3).	% Nd       SUNDANESE DIGIT THREE
unicode_numerical_value(0x1C43, 3.0, 3).	% Nd       LEPCHA DIGIT THREE
unicode_numerical_value(0x1C53, 3.0, 3).	% Nd       OL CHIKI DIGIT THREE
unicode_numerical_value(0x2083, 3.0, 3).	% No       SUBSCRIPT THREE
unicode_numerical_value(0x2162, 3.0, 3).	% Nl       ROMAN NUMERAL THREE
unicode_numerical_value(0x2172, 3.0, 3).	% Nl       SMALL ROMAN NUMERAL THREE
unicode_numerical_value(0x2462, 3.0, 3).	% No       CIRCLED DIGIT THREE
unicode_numerical_value(0x2476, 3.0, 3).	% No       PARENTHESIZED DIGIT THREE
unicode_numerical_value(0x248A, 3.0, 3).	% No       DIGIT THREE FULL STOP
unicode_numerical_value(0x24F7, 3.0, 3).	% No       DOUBLE CIRCLED DIGIT THREE
unicode_numerical_value(0x2778, 3.0, 3).	% No       DINGBAT NEGATIVE CIRCLED DIGIT THREE
unicode_numerical_value(0x2782, 3.0, 3).	% No       DINGBAT CIRCLED SANS-SERIF DIGIT THREE
unicode_numerical_value(0x278C, 3.0, 3).	% No       DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT THREE
unicode_numerical_value(0x3023, 3.0, 3).	% Nl       HANGZHOU NUMERAL THREE
unicode_numerical_value(0x3194, 3.0, 3).	% No       IDEOGRAPHIC ANNOTATION THREE MARK
unicode_numerical_value(0x3222, 3.0, 3).	% No       PARENTHESIZED IDEOGRAPH THREE
unicode_numerical_value(0x3282, 3.0, 3).	% No       CIRCLED IDEOGRAPH THREE
unicode_numerical_value(0x4E09, 3.0, 3).	% Lo       CJK UNIFIED IDEOGRAPH-4E09
unicode_numerical_value(0x4EE8, 3.0, 3).	% Lo       CJK UNIFIED IDEOGRAPH-4EE8
%53C1..53C4    ; 3.0, 3).	% Lo   [4] CJK UNIFIED IDEOGRAPH-53C1..CJK UNIFIED IDEOGRAPH-53C4
unicode_numerical_value(0x53C1, 3.0, 3).	% Lo       CJK UNIFIED IDEOGRAPH-53C1
unicode_numerical_value(0x53C2, 3.0, 3).	% Lo       CJK UNIFIED IDEOGRAPH-53C2
unicode_numerical_value(0x53C3, 3.0, 3).	% Lo       CJK UNIFIED IDEOGRAPH-53C3
unicode_numerical_value(0x53C4, 3.0, 3).	% Lo       CJK UNIFIED IDEOGRAPH-53C4
unicode_numerical_value(0x5F0E, 3.0, 3).	% Lo       CJK UNIFIED IDEOGRAPH-5F0E
unicode_numerical_value(0xA623, 3.0, 3).	% Nd       VAI DIGIT THREE
unicode_numerical_value(0xA6E8, 3.0, 3).	% Nl       BAMUM LETTER TET
unicode_numerical_value(0xA8D3, 3.0, 3).	% Nd       SAURASHTRA DIGIT THREE
unicode_numerical_value(0xA903, 3.0, 3).	% Nd       KAYAH LI DIGIT THREE
unicode_numerical_value(0xA9D3, 3.0, 3).	% Nd       JAVANESE DIGIT THREE
unicode_numerical_value(0xAA53, 3.0, 3).	% Nd       CHAM DIGIT THREE
unicode_numerical_value(0xABF3, 3.0, 3).	% Nd       MEETEI MAYEK DIGIT THREE
unicode_numerical_value(0xF96B, 3.0, 3).	% Lo       CJK COMPATIBILITY IDEOGRAPH-F96B
unicode_numerical_value(0xFF13, 3.0, 3).	% Nd       FULLWIDTH DIGIT THREE
unicode_numerical_value(0x10109, 3.0, 3).	% No       AEGEAN NUMBER THREE
unicode_numerical_value(0x104A3, 3.0, 3).	% Nd       OSMANYA DIGIT THREE
unicode_numerical_value(0x1085A, 3.0, 3).	% No       IMPERIAL ARAMAIC NUMBER THREE
unicode_numerical_value(0x1091B, 3.0, 3).	% No       PHOENICIAN NUMBER THREE
unicode_numerical_value(0x10A42, 3.0, 3).	% No       KHAROSHTHI DIGIT THREE
unicode_numerical_value(0x10B5A, 3.0, 3).	% No       INSCRIPTIONAL PARTHIAN NUMBER THREE
unicode_numerical_value(0x10B7A, 3.0, 3).	% No       INSCRIPTIONAL PAHLAVI NUMBER THREE
unicode_numerical_value(0x10E62, 3.0, 3).	% No       RUMI DIGIT THREE
unicode_numerical_value(0x11054, 3.0, 3).	% No       BRAHMI NUMBER THREE
unicode_numerical_value(0x11069, 3.0, 3).	% Nd       BRAHMI DIGIT THREE
unicode_numerical_value(0x110F3, 3.0, 3).	% Nd       SORA SOMPENG DIGIT THREE
unicode_numerical_value(0x11139, 3.0, 3).	% Nd       CHAKMA DIGIT THREE
unicode_numerical_value(0x111D3, 3.0, 3).	% Nd       SHARADA DIGIT THREE
unicode_numerical_value(0x116C3, 3.0, 3).	% Nd       TAKRI DIGIT THREE
unicode_numerical_value(0x12401, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE ASH
unicode_numerical_value(0x12408, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE DISH
unicode_numerical_value(0x12417, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE GESH2
unicode_numerical_value(0x12420, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE GESHU
%12424..12425  ; 3.0, 3).	% Nl   [2] CUNEIFORM NUMERIC SIGN THREE SHAR2..CUNEIFORM NUMERIC SIGN THREE SHAR2 VARIANT FORM
unicode_numerical_value(0x12424, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE SHAR2
unicode_numerical_value(0x12425, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE SHAR2 VARIANT FORM
%1242E..1242F  ; 3.0, 3).	% Nl   [2] CUNEIFORM NUMERIC SIGN THREE SHARU..CUNEIFORM NUMERIC SIGN THREE SHARU VARIANT FORM
unicode_numerical_value(0x1242E, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE SHARU
unicode_numerical_value(0x1242F, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE SHARU VARIANT FORM
%12436..12437  ; 3.0, 3).	% Nl   [2] CUNEIFORM NUMERIC SIGN THREE BURU..CUNEIFORM NUMERIC SIGN THREE BURU VARIANT FORM
unicode_numerical_value(0x12436, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE BURU
unicode_numerical_value(0x12437, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE BURU VARIANT FORM
%1243A..1243B  ; 3.0, 3).	% Nl   [2] CUNEIFORM NUMERIC SIGN THREE VARIANT FORM ESH16..CUNEIFORM NUMERIC SIGN THREE VARIANT FORM ESH21
unicode_numerical_value(0x1243A, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE VARIANT FORM ESH16
unicode_numerical_value(0x1243B, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE VARIANT FORM ESH21
unicode_numerical_value(0x1244B, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE ASH TENU
unicode_numerical_value(0x12451, 3.0, 3).	% Nl       CUNEIFORM NUMERIC SIGN THREE BAN2
unicode_numerical_value(0x1D362, 3.0, 3).	% No       COUNTING ROD UNIT DIGIT THREE
unicode_numerical_value(0x1D7D1, 3.0, 3).	% Nd       MATHEMATICAL BOLD DIGIT THREE
unicode_numerical_value(0x1D7DB, 3.0, 3).	% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT THREE
unicode_numerical_value(0x1D7E5, 3.0, 3).	% Nd       MATHEMATICAL SANS-SERIF DIGIT THREE
unicode_numerical_value(0x1D7EF, 3.0, 3).	% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT THREE
unicode_numerical_value(0x1D7F9, 3.0, 3).	% Nd       MATHEMATICAL MONOSPACE DIGIT THREE
unicode_numerical_value(0x1F104, 3.0, 3).	% No       DIGIT THREE COMMA
unicode_numerical_value(0x20AFD, 3.0, 3).	% Lo       CJK UNIFIED IDEOGRAPH-20AFD
unicode_numerical_value(0x20B19, 3.0, 3).	% Lo       CJK UNIFIED IDEOGRAPH-20B19
unicode_numerical_value(0x22998, 3.0, 3).	% Lo       CJK UNIFIED IDEOGRAPH-22998
unicode_numerical_value(0x23B1B, 3.0, 3).	% Lo       CJK UNIFIED IDEOGRAPH-23B1B

% Total code points: 102

% ================================================

unicode_numerical_value(0x0F2D, 3.5, 7/2).	% No       TIBETAN DIGIT HALF FOUR

% Total code points: 1

% ================================================

unicode_numerical_value(0x0034, 4.0, 4).	% Nd       DIGIT FOUR
unicode_numerical_value(0x0664, 4.0, 4).	% Nd       ARABIC-INDIC DIGIT FOUR
unicode_numerical_value(0x06F4, 4.0, 4).	% Nd       EXTENDED ARABIC-INDIC DIGIT FOUR
unicode_numerical_value(0x07C4, 4.0, 4).	% Nd       NKO DIGIT FOUR
unicode_numerical_value(0x096A, 4.0, 4).	% Nd       DEVANAGARI DIGIT FOUR
unicode_numerical_value(0x09EA, 4.0, 4).	% Nd       BENGALI DIGIT FOUR
unicode_numerical_value(0x0A6A, 4.0, 4).	% Nd       GURMUKHI DIGIT FOUR
unicode_numerical_value(0x0AEA, 4.0, 4).	% Nd       GUJARATI DIGIT FOUR
unicode_numerical_value(0x0B6A, 4.0, 4).	% Nd       ORIYA DIGIT FOUR
unicode_numerical_value(0x0BEA, 4.0, 4).	% Nd       TAMIL DIGIT FOUR
unicode_numerical_value(0x0C6A, 4.0, 4).	% Nd       TELUGU DIGIT FOUR
unicode_numerical_value(0x0CEA, 4.0, 4).	% Nd       KANNADA DIGIT FOUR
unicode_numerical_value(0x0D6A, 4.0, 4).	% Nd       MALAYALAM DIGIT FOUR
unicode_numerical_value(0x0E54, 4.0, 4).	% Nd       THAI DIGIT FOUR
unicode_numerical_value(0x0ED4, 4.0, 4).	% Nd       LAO DIGIT FOUR
unicode_numerical_value(0x0F24, 4.0, 4).	% Nd       TIBETAN DIGIT FOUR
unicode_numerical_value(0x1044, 4.0, 4).	% Nd       MYANMAR DIGIT FOUR
unicode_numerical_value(0x1094, 4.0, 4).	% Nd       MYANMAR SHAN DIGIT FOUR
unicode_numerical_value(0x136C, 4.0, 4).	% No       ETHIOPIC DIGIT FOUR
unicode_numerical_value(0x17E4, 4.0, 4).	% Nd       KHMER DIGIT FOUR
unicode_numerical_value(0x17F4, 4.0, 4).	% No       KHMER SYMBOL LEK ATTAK BUON
unicode_numerical_value(0x1814, 4.0, 4).	% Nd       MONGOLIAN DIGIT FOUR
unicode_numerical_value(0x194A, 4.0, 4).	% Nd       LIMBU DIGIT FOUR
unicode_numerical_value(0x19D4, 4.0, 4).	% Nd       NEW TAI LUE DIGIT FOUR
unicode_numerical_value(0x1A84, 4.0, 4).	% Nd       TAI THAM HORA DIGIT FOUR
unicode_numerical_value(0x1A94, 4.0, 4).	% Nd       TAI THAM THAM DIGIT FOUR
unicode_numerical_value(0x1B54, 4.0, 4).	% Nd       BALINESE DIGIT FOUR
unicode_numerical_value(0x1BB4, 4.0, 4).	% Nd       SUNDANESE DIGIT FOUR
unicode_numerical_value(0x1C44, 4.0, 4).	% Nd       LEPCHA DIGIT FOUR
unicode_numerical_value(0x1C54, 4.0, 4).	% Nd       OL CHIKI DIGIT FOUR
unicode_numerical_value(0x2074, 4.0, 4).	% No       SUPERSCRIPT FOUR
unicode_numerical_value(0x2084, 4.0, 4).	% No       SUBSCRIPT FOUR
unicode_numerical_value(0x2163, 4.0, 4).	% Nl       ROMAN NUMERAL FOUR
unicode_numerical_value(0x2173, 4.0, 4).	% Nl       SMALL ROMAN NUMERAL FOUR
unicode_numerical_value(0x2463, 4.0, 4).	% No       CIRCLED DIGIT FOUR
unicode_numerical_value(0x2477, 4.0, 4).	% No       PARENTHESIZED DIGIT FOUR
unicode_numerical_value(0x248B, 4.0, 4).	% No       DIGIT FOUR FULL STOP
unicode_numerical_value(0x24F8, 4.0, 4).	% No       DOUBLE CIRCLED DIGIT FOUR
unicode_numerical_value(0x2779, 4.0, 4).	% No       DINGBAT NEGATIVE CIRCLED DIGIT FOUR
unicode_numerical_value(0x2783, 4.0, 4).	% No       DINGBAT CIRCLED SANS-SERIF DIGIT FOUR
unicode_numerical_value(0x278D, 4.0, 4).	% No       DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT FOUR
unicode_numerical_value(0x3024, 4.0, 4).	% Nl       HANGZHOU NUMERAL FOUR
unicode_numerical_value(0x3195, 4.0, 4).	% No       IDEOGRAPHIC ANNOTATION FOUR MARK
unicode_numerical_value(0x3223, 4.0, 4).	% No       PARENTHESIZED IDEOGRAPH FOUR
unicode_numerical_value(0x3283, 4.0, 4).	% No       CIRCLED IDEOGRAPH FOUR
unicode_numerical_value(0x4E96, 4.0, 4).	% Lo       CJK UNIFIED IDEOGRAPH-4E96
unicode_numerical_value(0x56DB, 4.0, 4).	% Lo       CJK UNIFIED IDEOGRAPH-56DB
unicode_numerical_value(0x8086, 4.0, 4).	% Lo       CJK UNIFIED IDEOGRAPH-8086
unicode_numerical_value(0xA624, 4.0, 4).	% Nd       VAI DIGIT FOUR
unicode_numerical_value(0xA6E9, 4.0, 4).	% Nl       BAMUM LETTER KPA
unicode_numerical_value(0xA8D4, 4.0, 4).	% Nd       SAURASHTRA DIGIT FOUR
unicode_numerical_value(0xA904, 4.0, 4).	% Nd       KAYAH LI DIGIT FOUR
unicode_numerical_value(0xA9D4, 4.0, 4).	% Nd       JAVANESE DIGIT FOUR
unicode_numerical_value(0xAA54, 4.0, 4).	% Nd       CHAM DIGIT FOUR
unicode_numerical_value(0xABF4, 4.0, 4).	% Nd       MEETEI MAYEK DIGIT FOUR
unicode_numerical_value(0xFF14, 4.0, 4).	% Nd       FULLWIDTH DIGIT FOUR
unicode_numerical_value(0x1010A, 4.0, 4).	% No       AEGEAN NUMBER FOUR
unicode_numerical_value(0x104A4, 4.0, 4).	% Nd       OSMANYA DIGIT FOUR
unicode_numerical_value(0x10A43, 4.0, 4).	% No       KHAROSHTHI DIGIT FOUR
unicode_numerical_value(0x10B5B, 4.0, 4).	% No       INSCRIPTIONAL PARTHIAN NUMBER FOUR
unicode_numerical_value(0x10B7B, 4.0, 4).	% No       INSCRIPTIONAL PAHLAVI NUMBER FOUR
unicode_numerical_value(0x10E63, 4.0, 4).	% No       RUMI DIGIT FOUR
unicode_numerical_value(0x11055, 4.0, 4).	% No       BRAHMI NUMBER FOUR
unicode_numerical_value(0x1106A, 4.0, 4).	% Nd       BRAHMI DIGIT FOUR
unicode_numerical_value(0x110F4, 4.0, 4).	% Nd       SORA SOMPENG DIGIT FOUR
unicode_numerical_value(0x1113A, 4.0, 4).	% Nd       CHAKMA DIGIT FOUR
unicode_numerical_value(0x111D4, 4.0, 4).	% Nd       SHARADA DIGIT FOUR
unicode_numerical_value(0x116C4, 4.0, 4).	% Nd       TAKRI DIGIT FOUR
unicode_numerical_value(0x12402, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR ASH
unicode_numerical_value(0x12409, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR DISH
unicode_numerical_value(0x1240F, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR U
unicode_numerical_value(0x12418, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR GESH2
unicode_numerical_value(0x12421, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR GESHU
unicode_numerical_value(0x12426, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR SHAR2
unicode_numerical_value(0x12430, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR SHARU
unicode_numerical_value(0x12438, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR BURU
%1243C..1243F  ; 4.0, 4).	% Nl   [4] CUNEIFORM NUMERIC SIGN FOUR VARIANT FORM LIMMU..CUNEIFORM NUMERIC SIGN FOUR VARIANT FORM LIMMU B
unicode_numerical_value(0x1243C, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR VARIANT FORM LIMMU
unicode_numerical_value(0x1243D, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR VARIANT FORM LIMMU4
unicode_numerical_value(0x1243E, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR VARIANT FORM LIMMU A
unicode_numerical_value(0x1243F, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR VARIANT FORM LIMMU B
unicode_numerical_value(0x1244C, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR ASH TENU
%12452..12453  ; 4.0, 4).	% Nl   [2] CUNEIFORM NUMERIC SIGN FOUR BAN2..CUNEIFORM NUMERIC SIGN FOUR BAN2 VARIANT FORM
unicode_numerical_value(0x12452, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR BAN2
unicode_numerical_value(0x12453, 4.0, 4).	% Nl       CUNEIFORM NUMERIC SIGN FOUR BAN2 VARIANT FORM
unicode_numerical_value(0x1D363, 4.0, 4).	% No       COUNTING ROD UNIT DIGIT FOUR
unicode_numerical_value(0x1D7D2, 4.0, 4).	% Nd       MATHEMATICAL BOLD DIGIT FOUR
unicode_numerical_value(0x1D7DC, 4.0, 4).	% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT FOUR
unicode_numerical_value(0x1D7E6, 4.0, 4).	% Nd       MATHEMATICAL SANS-SERIF DIGIT FOUR
unicode_numerical_value(0x1D7F0, 4.0, 4).	% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT FOUR
unicode_numerical_value(0x1D7FA, 4.0, 4).	% Nd       MATHEMATICAL MONOSPACE DIGIT FOUR
unicode_numerical_value(0x1F105, 4.0, 4).	% No       DIGIT FOUR COMMA
unicode_numerical_value(0x20064, 4.0, 4).	% Lo       CJK UNIFIED IDEOGRAPH-20064
unicode_numerical_value(0x200E2, 4.0, 4).	% Lo       CJK UNIFIED IDEOGRAPH-200E2
unicode_numerical_value(0x2626D, 4.0, 4).	% Lo       CJK UNIFIED IDEOGRAPH-2626D

% Total code points: 93

% ================================================

unicode_numerical_value(0x0F2E, 4.5, 9/2).	% No       TIBETAN DIGIT HALF FIVE

% Total code points: 1

% ================================================

unicode_numerical_value(0x0035, 5.0, 5).	% Nd       DIGIT FIVE
unicode_numerical_value(0x0665, 5.0, 5).	% Nd       ARABIC-INDIC DIGIT FIVE
unicode_numerical_value(0x06F5, 5.0, 5).	% Nd       EXTENDED ARABIC-INDIC DIGIT FIVE
unicode_numerical_value(0x07C5, 5.0, 5).	% Nd       NKO DIGIT FIVE
unicode_numerical_value(0x096B, 5.0, 5).	% Nd       DEVANAGARI DIGIT FIVE
unicode_numerical_value(0x09EB, 5.0, 5).	% Nd       BENGALI DIGIT FIVE
unicode_numerical_value(0x0A6B, 5.0, 5).	% Nd       GURMUKHI DIGIT FIVE
unicode_numerical_value(0x0AEB, 5.0, 5).	% Nd       GUJARATI DIGIT FIVE
unicode_numerical_value(0x0B6B, 5.0, 5).	% Nd       ORIYA DIGIT FIVE
unicode_numerical_value(0x0BEB, 5.0, 5).	% Nd       TAMIL DIGIT FIVE
unicode_numerical_value(0x0C6B, 5.0, 5).	% Nd       TELUGU DIGIT FIVE
unicode_numerical_value(0x0CEB, 5.0, 5).	% Nd       KANNADA DIGIT FIVE
unicode_numerical_value(0x0D6B, 5.0, 5).	% Nd       MALAYALAM DIGIT FIVE
unicode_numerical_value(0x0E55, 5.0, 5).	% Nd       THAI DIGIT FIVE
unicode_numerical_value(0x0ED5, 5.0, 5).	% Nd       LAO DIGIT FIVE
unicode_numerical_value(0x0F25, 5.0, 5).	% Nd       TIBETAN DIGIT FIVE
unicode_numerical_value(0x1045, 5.0, 5).	% Nd       MYANMAR DIGIT FIVE
unicode_numerical_value(0x1095, 5.0, 5).	% Nd       MYANMAR SHAN DIGIT FIVE
unicode_numerical_value(0x136D, 5.0, 5).	% No       ETHIOPIC DIGIT FIVE
unicode_numerical_value(0x17E5, 5.0, 5).	% Nd       KHMER DIGIT FIVE
unicode_numerical_value(0x17F5, 5.0, 5).	% No       KHMER SYMBOL LEK ATTAK PRAM
unicode_numerical_value(0x1815, 5.0, 5).	% Nd       MONGOLIAN DIGIT FIVE
unicode_numerical_value(0x194B, 5.0, 5).	% Nd       LIMBU DIGIT FIVE
unicode_numerical_value(0x19D5, 5.0, 5).	% Nd       NEW TAI LUE DIGIT FIVE
unicode_numerical_value(0x1A85, 5.0, 5).	% Nd       TAI THAM HORA DIGIT FIVE
unicode_numerical_value(0x1A95, 5.0, 5).	% Nd       TAI THAM THAM DIGIT FIVE
unicode_numerical_value(0x1B55, 5.0, 5).	% Nd       BALINESE DIGIT FIVE
unicode_numerical_value(0x1BB5, 5.0, 5).	% Nd       SUNDANESE DIGIT FIVE
unicode_numerical_value(0x1C45, 5.0, 5).	% Nd       LEPCHA DIGIT FIVE
unicode_numerical_value(0x1C55, 5.0, 5).	% Nd       OL CHIKI DIGIT FIVE
unicode_numerical_value(0x2075, 5.0, 5).	% No       SUPERSCRIPT FIVE
unicode_numerical_value(0x2085, 5.0, 5).	% No       SUBSCRIPT FIVE
unicode_numerical_value(0x2164, 5.0, 5).	% Nl       ROMAN NUMERAL FIVE
unicode_numerical_value(0x2174, 5.0, 5).	% Nl       SMALL ROMAN NUMERAL FIVE
unicode_numerical_value(0x2464, 5.0, 5).	% No       CIRCLED DIGIT FIVE
unicode_numerical_value(0x2478, 5.0, 5).	% No       PARENTHESIZED DIGIT FIVE
unicode_numerical_value(0x248C, 5.0, 5).	% No       DIGIT FIVE FULL STOP
unicode_numerical_value(0x24F9, 5.0, 5).	% No       DOUBLE CIRCLED DIGIT FIVE
unicode_numerical_value(0x277A, 5.0, 5).	% No       DINGBAT NEGATIVE CIRCLED DIGIT FIVE
unicode_numerical_value(0x2784, 5.0, 5).	% No       DINGBAT CIRCLED SANS-SERIF DIGIT FIVE
unicode_numerical_value(0x278E, 5.0, 5).	% No       DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT FIVE
unicode_numerical_value(0x3025, 5.0, 5).	% Nl       HANGZHOU NUMERAL FIVE
unicode_numerical_value(0x3224, 5.0, 5).	% No       PARENTHESIZED IDEOGRAPH FIVE
unicode_numerical_value(0x3284, 5.0, 5).	% No       CIRCLED IDEOGRAPH FIVE
unicode_numerical_value(0x3405, 5.0, 5).	% Lo       CJK UNIFIED IDEOGRAPH-3405
unicode_numerical_value(0x382A, 5.0, 5).	% Lo       CJK UNIFIED IDEOGRAPH-382A
unicode_numerical_value(0x4E94, 5.0, 5).	% Lo       CJK UNIFIED IDEOGRAPH-4E94
unicode_numerical_value(0x4F0D, 5.0, 5).	% Lo       CJK UNIFIED IDEOGRAPH-4F0D
unicode_numerical_value(0xA625, 5.0, 5).	% Nd       VAI DIGIT FIVE
unicode_numerical_value(0xA6EA, 5.0, 5).	% Nl       BAMUM LETTER TEN
unicode_numerical_value(0xA8D5, 5.0, 5).	% Nd       SAURASHTRA DIGIT FIVE
unicode_numerical_value(0xA905, 5.0, 5).	% Nd       KAYAH LI DIGIT FIVE
unicode_numerical_value(0xA9D5, 5.0, 5).	% Nd       JAVANESE DIGIT FIVE
unicode_numerical_value(0xAA55, 5.0, 5).	% Nd       CHAM DIGIT FIVE
unicode_numerical_value(0xABF5, 5.0, 5).	% Nd       MEETEI MAYEK DIGIT FIVE
unicode_numerical_value(0xFF15, 5.0, 5).	% Nd       FULLWIDTH DIGIT FIVE
unicode_numerical_value(0x1010B, 5.0, 5).	% No       AEGEAN NUMBER FIVE
unicode_numerical_value(0x10143, 5.0, 5).	% Nl       GREEK ACROPHONIC ATTIC FIVE
unicode_numerical_value(0x10148, 5.0, 5).	% Nl       GREEK ACROPHONIC ATTIC FIVE TALENTS
unicode_numerical_value(0x1014F, 5.0, 5).	% Nl       GREEK ACROPHONIC ATTIC FIVE STATERS
unicode_numerical_value(0x1015F, 5.0, 5).	% Nl       GREEK ACROPHONIC TROEZENIAN FIVE
unicode_numerical_value(0x10173, 5.0, 5).	% Nl       GREEK ACROPHONIC DELPHIC FIVE MNAS
unicode_numerical_value(0x10321, 5.0, 5).	% No       OLD ITALIC NUMERAL FIVE
unicode_numerical_value(0x104A5, 5.0, 5).	% Nd       OSMANYA DIGIT FIVE
unicode_numerical_value(0x10E64, 5.0, 5).	% No       RUMI DIGIT FIVE
unicode_numerical_value(0x11056, 5.0, 5).	% No       BRAHMI NUMBER FIVE
unicode_numerical_value(0x1106B, 5.0, 5).	% Nd       BRAHMI DIGIT FIVE
unicode_numerical_value(0x110F5, 5.0, 5).	% Nd       SORA SOMPENG DIGIT FIVE
unicode_numerical_value(0x1113B, 5.0, 5).	% Nd       CHAKMA DIGIT FIVE
unicode_numerical_value(0x111D5, 5.0, 5).	% Nd       SHARADA DIGIT FIVE
unicode_numerical_value(0x116C5, 5.0, 5).	% Nd       TAKRI DIGIT FIVE
unicode_numerical_value(0x12403, 5.0, 5).	% Nl       CUNEIFORM NUMERIC SIGN FIVE ASH
unicode_numerical_value(0x1240A, 5.0, 5).	% Nl       CUNEIFORM NUMERIC SIGN FIVE DISH
unicode_numerical_value(0x12410, 5.0, 5).	% Nl       CUNEIFORM NUMERIC SIGN FIVE U
unicode_numerical_value(0x12419, 5.0, 5).	% Nl       CUNEIFORM NUMERIC SIGN FIVE GESH2
unicode_numerical_value(0x12422, 5.0, 5).	% Nl       CUNEIFORM NUMERIC SIGN FIVE GESHU
unicode_numerical_value(0x12427, 5.0, 5).	% Nl       CUNEIFORM NUMERIC SIGN FIVE SHAR2
unicode_numerical_value(0x12431, 5.0, 5).	% Nl       CUNEIFORM NUMERIC SIGN FIVE SHARU
unicode_numerical_value(0x12439, 5.0, 5).	% Nl       CUNEIFORM NUMERIC SIGN FIVE BURU
unicode_numerical_value(0x1244D, 5.0, 5).	% Nl       CUNEIFORM NUMERIC SIGN FIVE ASH TENU
%12454..12455  ; 5.0, 5).	% Nl   [2] CUNEIFORM NUMERIC SIGN FIVE BAN2..CUNEIFORM NUMERIC SIGN FIVE BAN2 VARIANT FORM
unicode_numerical_value(0x12454, 5.0, 5).	% Nl       CUNEIFORM NUMERIC SIGN FIVE BAN2
unicode_numerical_value(0x12455, 5.0, 5).	% Nl       CUNEIFORM NUMERIC SIGN FIVE BAN2 VARIANT FORM
unicode_numerical_value(0x1D364, 5.0, 5).	% No       COUNTING ROD UNIT DIGIT FIVE
unicode_numerical_value(0x1D7D3, 5.0, 5).	% Nd       MATHEMATICAL BOLD DIGIT FIVE
unicode_numerical_value(0x1D7DD, 5.0, 5).	% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT FIVE
unicode_numerical_value(0x1D7E7, 5.0, 5).	% Nd       MATHEMATICAL SANS-SERIF DIGIT FIVE
unicode_numerical_value(0x1D7F1, 5.0, 5).	% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT FIVE
unicode_numerical_value(0x1D7FB, 5.0, 5).	% Nd       MATHEMATICAL MONOSPACE DIGIT FIVE
unicode_numerical_value(0x1F106, 5.0, 5).	% No       DIGIT FIVE COMMA
unicode_numerical_value(0x20121, 5.0, 5).	% Lo       CJK UNIFIED IDEOGRAPH-20121

% Total code points: 90

% ================================================

unicode_numerical_value(0x0F2F, 5.5, 11/2).	% No       TIBETAN DIGIT HALF SIX

% Total code points: 1

% ================================================

unicode_numerical_value(0x0036, 6.0, 6).	% Nd       DIGIT SIX
unicode_numerical_value(0x0666, 6.0, 6).	% Nd       ARABIC-INDIC DIGIT SIX
unicode_numerical_value(0x06F6, 6.0, 6).	% Nd       EXTENDED ARABIC-INDIC DIGIT SIX
unicode_numerical_value(0x07C6, 6.0, 6).	% Nd       NKO DIGIT SIX
unicode_numerical_value(0x096C, 6.0, 6).	% Nd       DEVANAGARI DIGIT SIX
unicode_numerical_value(0x09EC, 6.0, 6).	% Nd       BENGALI DIGIT SIX
unicode_numerical_value(0x0A6C, 6.0, 6).	% Nd       GURMUKHI DIGIT SIX
unicode_numerical_value(0x0AEC, 6.0, 6).	% Nd       GUJARATI DIGIT SIX
unicode_numerical_value(0x0B6C, 6.0, 6).	% Nd       ORIYA DIGIT SIX
unicode_numerical_value(0x0BEC, 6.0, 6).	% Nd       TAMIL DIGIT SIX
unicode_numerical_value(0x0C6C, 6.0, 6).	% Nd       TELUGU DIGIT SIX
unicode_numerical_value(0x0CEC, 6.0, 6).	% Nd       KANNADA DIGIT SIX
unicode_numerical_value(0x0D6C, 6.0, 6).	% Nd       MALAYALAM DIGIT SIX
unicode_numerical_value(0x0E56, 6.0, 6).	% Nd       THAI DIGIT SIX
unicode_numerical_value(0x0ED6, 6.0, 6).	% Nd       LAO DIGIT SIX
unicode_numerical_value(0x0F26, 6.0, 6).	% Nd       TIBETAN DIGIT SIX
unicode_numerical_value(0x1046, 6.0, 6).	% Nd       MYANMAR DIGIT SIX
unicode_numerical_value(0x1096, 6.0, 6).	% Nd       MYANMAR SHAN DIGIT SIX
unicode_numerical_value(0x136E, 6.0, 6).	% No       ETHIOPIC DIGIT SIX
unicode_numerical_value(0x17E6, 6.0, 6).	% Nd       KHMER DIGIT SIX
unicode_numerical_value(0x17F6, 6.0, 6).	% No       KHMER SYMBOL LEK ATTAK PRAM-MUOY
unicode_numerical_value(0x1816, 6.0, 6).	% Nd       MONGOLIAN DIGIT SIX
unicode_numerical_value(0x194C, 6.0, 6).	% Nd       LIMBU DIGIT SIX
unicode_numerical_value(0x19D6, 6.0, 6).	% Nd       NEW TAI LUE DIGIT SIX
unicode_numerical_value(0x1A86, 6.0, 6).	% Nd       TAI THAM HORA DIGIT SIX
unicode_numerical_value(0x1A96, 6.0, 6).	% Nd       TAI THAM THAM DIGIT SIX
unicode_numerical_value(0x1B56, 6.0, 6).	% Nd       BALINESE DIGIT SIX
unicode_numerical_value(0x1BB6, 6.0, 6).	% Nd       SUNDANESE DIGIT SIX
unicode_numerical_value(0x1C46, 6.0, 6).	% Nd       LEPCHA DIGIT SIX
unicode_numerical_value(0x1C56, 6.0, 6).	% Nd       OL CHIKI DIGIT SIX
unicode_numerical_value(0x2076, 6.0, 6).	% No       SUPERSCRIPT SIX
unicode_numerical_value(0x2086, 6.0, 6).	% No       SUBSCRIPT SIX
unicode_numerical_value(0x2165, 6.0, 6).	% Nl       ROMAN NUMERAL SIX
unicode_numerical_value(0x2175, 6.0, 6).	% Nl       SMALL ROMAN NUMERAL SIX
unicode_numerical_value(0x2185, 6.0, 6).	% Nl       ROMAN NUMERAL SIX LATE FORM
unicode_numerical_value(0x2465, 6.0, 6).	% No       CIRCLED DIGIT SIX
unicode_numerical_value(0x2479, 6.0, 6).	% No       PARENTHESIZED DIGIT SIX
unicode_numerical_value(0x248D, 6.0, 6).	% No       DIGIT SIX FULL STOP
unicode_numerical_value(0x24FA, 6.0, 6).	% No       DOUBLE CIRCLED DIGIT SIX
unicode_numerical_value(0x277B, 6.0, 6).	% No       DINGBAT NEGATIVE CIRCLED DIGIT SIX
unicode_numerical_value(0x2785, 6.0, 6).	% No       DINGBAT CIRCLED SANS-SERIF DIGIT SIX
unicode_numerical_value(0x278F, 6.0, 6).	% No       DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT SIX
unicode_numerical_value(0x3026, 6.0, 6).	% Nl       HANGZHOU NUMERAL SIX
unicode_numerical_value(0x3225, 6.0, 6).	% No       PARENTHESIZED IDEOGRAPH SIX
unicode_numerical_value(0x3285, 6.0, 6).	% No       CIRCLED IDEOGRAPH SIX
unicode_numerical_value(0x516D, 6.0, 6).	% Lo       CJK UNIFIED IDEOGRAPH-516D
unicode_numerical_value(0x9646, 6.0, 6).	% Lo       CJK UNIFIED IDEOGRAPH-9646
unicode_numerical_value(0x9678, 6.0, 6).	% Lo       CJK UNIFIED IDEOGRAPH-9678
unicode_numerical_value(0xA626, 6.0, 6).	% Nd       VAI DIGIT SIX
unicode_numerical_value(0xA6EB, 6.0, 6).	% Nl       BAMUM LETTER NTUU
unicode_numerical_value(0xA8D6, 6.0, 6).	% Nd       SAURASHTRA DIGIT SIX
unicode_numerical_value(0xA906, 6.0, 6).	% Nd       KAYAH LI DIGIT SIX
unicode_numerical_value(0xA9D6, 6.0, 6).	% Nd       JAVANESE DIGIT SIX
unicode_numerical_value(0xAA56, 6.0, 6).	% Nd       CHAM DIGIT SIX
unicode_numerical_value(0xABF6, 6.0, 6).	% Nd       MEETEI MAYEK DIGIT SIX
unicode_numerical_value(0xF9D1, 6.0, 6).	% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D1
unicode_numerical_value(0xF9D3, 6.0, 6).	% Lo       CJK COMPATIBILITY IDEOGRAPH-F9D3
unicode_numerical_value(0xFF16, 6.0, 6).	% Nd       FULLWIDTH DIGIT SIX
unicode_numerical_value(0x1010C, 6.0, 6).	% No       AEGEAN NUMBER SIX
unicode_numerical_value(0x104A6, 6.0, 6).	% Nd       OSMANYA DIGIT SIX
unicode_numerical_value(0x10E65, 6.0, 6).	% No       RUMI DIGIT SIX
unicode_numerical_value(0x11057, 6.0, 6).	% No       BRAHMI NUMBER SIX
unicode_numerical_value(0x1106C, 6.0, 6).	% Nd       BRAHMI DIGIT SIX
unicode_numerical_value(0x110F6, 6.0, 6).	% Nd       SORA SOMPENG DIGIT SIX
unicode_numerical_value(0x1113C, 6.0, 6).	% Nd       CHAKMA DIGIT SIX
unicode_numerical_value(0x111D6, 6.0, 6).	% Nd       SHARADA DIGIT SIX
unicode_numerical_value(0x116C6, 6.0, 6).	% Nd       TAKRI DIGIT SIX
unicode_numerical_value(0x12404, 6.0, 6).	% Nl       CUNEIFORM NUMERIC SIGN SIX ASH
unicode_numerical_value(0x1240B, 6.0, 6).	% Nl       CUNEIFORM NUMERIC SIGN SIX DISH
unicode_numerical_value(0x12411, 6.0, 6).	% Nl       CUNEIFORM NUMERIC SIGN SIX U
unicode_numerical_value(0x1241A, 6.0, 6).	% Nl       CUNEIFORM NUMERIC SIGN SIX GESH2
unicode_numerical_value(0x12428, 6.0, 6).	% Nl       CUNEIFORM NUMERIC SIGN SIX SHAR2
unicode_numerical_value(0x12440, 6.0, 6).	% Nl       CUNEIFORM NUMERIC SIGN SIX VARIANT FORM ASH9
unicode_numerical_value(0x1244E, 6.0, 6).	% Nl       CUNEIFORM NUMERIC SIGN SIX ASH TENU
unicode_numerical_value(0x1D365, 6.0, 6).	% No       COUNTING ROD UNIT DIGIT SIX
unicode_numerical_value(0x1D7D4, 6.0, 6).	% Nd       MATHEMATICAL BOLD DIGIT SIX
unicode_numerical_value(0x1D7DE, 6.0, 6).	% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT SIX
unicode_numerical_value(0x1D7E8, 6.0, 6).	% Nd       MATHEMATICAL SANS-SERIF DIGIT SIX
unicode_numerical_value(0x1D7F2, 6.0, 6).	% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT SIX
unicode_numerical_value(0x1D7FC, 6.0, 6).	% Nd       MATHEMATICAL MONOSPACE DIGIT SIX
unicode_numerical_value(0x1F107, 6.0, 6).	% No       DIGIT SIX COMMA
unicode_numerical_value(0x20AEA, 6.0, 6).	% Lo       CJK UNIFIED IDEOGRAPH-20AEA

% Total code points: 82

% ================================================

unicode_numerical_value(0x0F30, 6.5, 13/2).	% No       TIBETAN DIGIT HALF SEVEN

% Total code points: 1

% ================================================

unicode_numerical_value(0x0037, 7.0, 7).	% Nd       DIGIT SEVEN
unicode_numerical_value(0x0667, 7.0, 7).	% Nd       ARABIC-INDIC DIGIT SEVEN
unicode_numerical_value(0x06F7, 7.0, 7).	% Nd       EXTENDED ARABIC-INDIC DIGIT SEVEN
unicode_numerical_value(0x07C7, 7.0, 7).	% Nd       NKO DIGIT SEVEN
unicode_numerical_value(0x096D, 7.0, 7).	% Nd       DEVANAGARI DIGIT SEVEN
unicode_numerical_value(0x09ED, 7.0, 7).	% Nd       BENGALI DIGIT SEVEN
unicode_numerical_value(0x0A6D, 7.0, 7).	% Nd       GURMUKHI DIGIT SEVEN
unicode_numerical_value(0x0AED, 7.0, 7).	% Nd       GUJARATI DIGIT SEVEN
unicode_numerical_value(0x0B6D, 7.0, 7).	% Nd       ORIYA DIGIT SEVEN
unicode_numerical_value(0x0BED, 7.0, 7).	% Nd       TAMIL DIGIT SEVEN
unicode_numerical_value(0x0C6D, 7.0, 7).	% Nd       TELUGU DIGIT SEVEN
unicode_numerical_value(0x0CED, 7.0, 7).	% Nd       KANNADA DIGIT SEVEN
unicode_numerical_value(0x0D6D, 7.0, 7).	% Nd       MALAYALAM DIGIT SEVEN
unicode_numerical_value(0x0E57, 7.0, 7).	% Nd       THAI DIGIT SEVEN
unicode_numerical_value(0x0ED7, 7.0, 7).	% Nd       LAO DIGIT SEVEN
unicode_numerical_value(0x0F27, 7.0, 7).	% Nd       TIBETAN DIGIT SEVEN
unicode_numerical_value(0x1047, 7.0, 7).	% Nd       MYANMAR DIGIT SEVEN
unicode_numerical_value(0x1097, 7.0, 7).	% Nd       MYANMAR SHAN DIGIT SEVEN
unicode_numerical_value(0x136F, 7.0, 7).	% No       ETHIOPIC DIGIT SEVEN
unicode_numerical_value(0x17E7, 7.0, 7).	% Nd       KHMER DIGIT SEVEN
unicode_numerical_value(0x17F7, 7.0, 7).	% No       KHMER SYMBOL LEK ATTAK PRAM-PII
unicode_numerical_value(0x1817, 7.0, 7).	% Nd       MONGOLIAN DIGIT SEVEN
unicode_numerical_value(0x194D, 7.0, 7).	% Nd       LIMBU DIGIT SEVEN
unicode_numerical_value(0x19D7, 7.0, 7).	% Nd       NEW TAI LUE DIGIT SEVEN
unicode_numerical_value(0x1A87, 7.0, 7).	% Nd       TAI THAM HORA DIGIT SEVEN
unicode_numerical_value(0x1A97, 7.0, 7).	% Nd       TAI THAM THAM DIGIT SEVEN
unicode_numerical_value(0x1B57, 7.0, 7).	% Nd       BALINESE DIGIT SEVEN
unicode_numerical_value(0x1BB7, 7.0, 7).	% Nd       SUNDANESE DIGIT SEVEN
unicode_numerical_value(0x1C47, 7.0, 7).	% Nd       LEPCHA DIGIT SEVEN
unicode_numerical_value(0x1C57, 7.0, 7).	% Nd       OL CHIKI DIGIT SEVEN
unicode_numerical_value(0x2077, 7.0, 7).	% No       SUPERSCRIPT SEVEN
unicode_numerical_value(0x2087, 7.0, 7).	% No       SUBSCRIPT SEVEN
unicode_numerical_value(0x2166, 7.0, 7).	% Nl       ROMAN NUMERAL SEVEN
unicode_numerical_value(0x2176, 7.0, 7).	% Nl       SMALL ROMAN NUMERAL SEVEN
unicode_numerical_value(0x2466, 7.0, 7).	% No       CIRCLED DIGIT SEVEN
unicode_numerical_value(0x247A, 7.0, 7).	% No       PARENTHESIZED DIGIT SEVEN
unicode_numerical_value(0x248E, 7.0, 7).	% No       DIGIT SEVEN FULL STOP
unicode_numerical_value(0x24FB, 7.0, 7).	% No       DOUBLE CIRCLED DIGIT SEVEN
unicode_numerical_value(0x277C, 7.0, 7).	% No       DINGBAT NEGATIVE CIRCLED DIGIT SEVEN
unicode_numerical_value(0x2786, 7.0, 7).	% No       DINGBAT CIRCLED SANS-SERIF DIGIT SEVEN
unicode_numerical_value(0x2790, 7.0, 7).	% No       DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT SEVEN
unicode_numerical_value(0x3027, 7.0, 7).	% Nl       HANGZHOU NUMERAL SEVEN
unicode_numerical_value(0x3226, 7.0, 7).	% No       PARENTHESIZED IDEOGRAPH SEVEN
unicode_numerical_value(0x3286, 7.0, 7).	% No       CIRCLED IDEOGRAPH SEVEN
unicode_numerical_value(0x3B4D, 7.0, 7).	% Lo       CJK UNIFIED IDEOGRAPH-3B4D
unicode_numerical_value(0x4E03, 7.0, 7).	% Lo       CJK UNIFIED IDEOGRAPH-4E03
unicode_numerical_value(0x67D2, 7.0, 7).	% Lo       CJK UNIFIED IDEOGRAPH-67D2
unicode_numerical_value(0x6F06, 7.0, 7).	% Lo       CJK UNIFIED IDEOGRAPH-6F06
unicode_numerical_value(0xA627, 7.0, 7).	% Nd       VAI DIGIT SEVEN
unicode_numerical_value(0xA6EC, 7.0, 7).	% Nl       BAMUM LETTER SAMBA
unicode_numerical_value(0xA8D7, 7.0, 7).	% Nd       SAURASHTRA DIGIT SEVEN
unicode_numerical_value(0xA907, 7.0, 7).	% Nd       KAYAH LI DIGIT SEVEN
unicode_numerical_value(0xA9D7, 7.0, 7).	% Nd       JAVANESE DIGIT SEVEN
unicode_numerical_value(0xAA57, 7.0, 7).	% Nd       CHAM DIGIT SEVEN
unicode_numerical_value(0xABF7, 7.0, 7).	% Nd       MEETEI MAYEK DIGIT SEVEN
unicode_numerical_value(0xFF17, 7.0, 7).	% Nd       FULLWIDTH DIGIT SEVEN
unicode_numerical_value(0x1010D, 7.0, 7).	% No       AEGEAN NUMBER SEVEN
unicode_numerical_value(0x104A7, 7.0, 7).	% Nd       OSMANYA DIGIT SEVEN
unicode_numerical_value(0x10E66, 7.0, 7).	% No       RUMI DIGIT SEVEN
unicode_numerical_value(0x11058, 7.0, 7).	% No       BRAHMI NUMBER SEVEN
unicode_numerical_value(0x1106D, 7.0, 7).	% Nd       BRAHMI DIGIT SEVEN
unicode_numerical_value(0x110F7, 7.0, 7).	% Nd       SORA SOMPENG DIGIT SEVEN
unicode_numerical_value(0x1113D, 7.0, 7).	% Nd       CHAKMA DIGIT SEVEN
unicode_numerical_value(0x111D7, 7.0, 7).	% Nd       SHARADA DIGIT SEVEN
unicode_numerical_value(0x116C7, 7.0, 7).	% Nd       TAKRI DIGIT SEVEN
unicode_numerical_value(0x12405, 7.0, 7).	% Nl       CUNEIFORM NUMERIC SIGN SEVEN ASH
unicode_numerical_value(0x1240C, 7.0, 7).	% Nl       CUNEIFORM NUMERIC SIGN SEVEN DISH
unicode_numerical_value(0x12412, 7.0, 7).	% Nl       CUNEIFORM NUMERIC SIGN SEVEN U
unicode_numerical_value(0x1241B, 7.0, 7).	% Nl       CUNEIFORM NUMERIC SIGN SEVEN GESH2
unicode_numerical_value(0x12429, 7.0, 7).	% Nl       CUNEIFORM NUMERIC SIGN SEVEN SHAR2
%12441..12443  ; 7.0, 7).	% Nl   [3] CUNEIFORM NUMERIC SIGN SEVEN VARIANT FORM IMIN3..CUNEIFORM NUMERIC SIGN SEVEN VARIANT FORM IMIN B
unicode_numerical_value(0x12441, 7.0, 7).	% Nl       CUNEIFORM NUMERIC SIGN SEVEN VARIANT FORM IMIN3
unicode_numerical_value(0x12442, 7.0, 7).	% Nl       CUNEIFORM NUMERIC SIGN SEVEN VARIANT FORM IMIN A
unicode_numerical_value(0x12443, 7.0, 7).	% Nl       CUNEIFORM NUMERIC SIGN SEVEN VARIANT FORM IMIN B
unicode_numerical_value(0x1D366, 7.0, 7).	% No       COUNTING ROD UNIT DIGIT SEVEN
unicode_numerical_value(0x1D7D5, 7.0, 7).	% Nd       MATHEMATICAL BOLD DIGIT SEVEN
unicode_numerical_value(0x1D7DF, 7.0, 7).	% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT SEVEN
unicode_numerical_value(0x1D7E9, 7.0, 7).	% Nd       MATHEMATICAL SANS-SERIF DIGIT SEVEN
unicode_numerical_value(0x1D7F3, 7.0, 7).	% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT SEVEN
unicode_numerical_value(0x1D7FD, 7.0, 7).	% Nd       MATHEMATICAL MONOSPACE DIGIT SEVEN
unicode_numerical_value(0x1F108, 7.0, 7).	% No       DIGIT SEVEN COMMA
unicode_numerical_value(0x20001, 7.0, 7).	% Lo       CJK UNIFIED IDEOGRAPH-20001

% Total code points: 81

% ================================================

unicode_numerical_value(0x0F31, 7.5, 15/2).	% No       TIBETAN DIGIT HALF EIGHT

% Total code points: 1

% ================================================

unicode_numerical_value(0x0038, 8.0, 8).	% Nd       DIGIT EIGHT
unicode_numerical_value(0x0668, 8.0, 8).	% Nd       ARABIC-INDIC DIGIT EIGHT
unicode_numerical_value(0x06F8, 8.0, 8).	% Nd       EXTENDED ARABIC-INDIC DIGIT EIGHT
unicode_numerical_value(0x07C8, 8.0, 8).	% Nd       NKO DIGIT EIGHT
unicode_numerical_value(0x096E, 8.0, 8).	% Nd       DEVANAGARI DIGIT EIGHT
unicode_numerical_value(0x09EE, 8.0, 8).	% Nd       BENGALI DIGIT EIGHT
unicode_numerical_value(0x0A6E, 8.0, 8).	% Nd       GURMUKHI DIGIT EIGHT
unicode_numerical_value(0x0AEE, 8.0, 8).	% Nd       GUJARATI DIGIT EIGHT
unicode_numerical_value(0x0B6E, 8.0, 8).	% Nd       ORIYA DIGIT EIGHT
unicode_numerical_value(0x0BEE, 8.0, 8).	% Nd       TAMIL DIGIT EIGHT
unicode_numerical_value(0x0C6E, 8.0, 8).	% Nd       TELUGU DIGIT EIGHT
unicode_numerical_value(0x0CEE, 8.0, 8).	% Nd       KANNADA DIGIT EIGHT
unicode_numerical_value(0x0D6E, 8.0, 8).	% Nd       MALAYALAM DIGIT EIGHT
unicode_numerical_value(0x0E58, 8.0, 8).	% Nd       THAI DIGIT EIGHT
unicode_numerical_value(0x0ED8, 8.0, 8).	% Nd       LAO DIGIT EIGHT
unicode_numerical_value(0x0F28, 8.0, 8).	% Nd       TIBETAN DIGIT EIGHT
unicode_numerical_value(0x1048, 8.0, 8).	% Nd       MYANMAR DIGIT EIGHT
unicode_numerical_value(0x1098, 8.0, 8).	% Nd       MYANMAR SHAN DIGIT EIGHT
unicode_numerical_value(0x1370, 8.0, 8).	% No       ETHIOPIC DIGIT EIGHT
unicode_numerical_value(0x17E8, 8.0, 8).	% Nd       KHMER DIGIT EIGHT
unicode_numerical_value(0x17F8, 8.0, 8).	% No       KHMER SYMBOL LEK ATTAK PRAM-BEI
unicode_numerical_value(0x1818, 8.0, 8).	% Nd       MONGOLIAN DIGIT EIGHT
unicode_numerical_value(0x194E, 8.0, 8).	% Nd       LIMBU DIGIT EIGHT
unicode_numerical_value(0x19D8, 8.0, 8).	% Nd       NEW TAI LUE DIGIT EIGHT
unicode_numerical_value(0x1A88, 8.0, 8).	% Nd       TAI THAM HORA DIGIT EIGHT
unicode_numerical_value(0x1A98, 8.0, 8).	% Nd       TAI THAM THAM DIGIT EIGHT
unicode_numerical_value(0x1B58, 8.0, 8).	% Nd       BALINESE DIGIT EIGHT
unicode_numerical_value(0x1BB8, 8.0, 8).	% Nd       SUNDANESE DIGIT EIGHT
unicode_numerical_value(0x1C48, 8.0, 8).	% Nd       LEPCHA DIGIT EIGHT
unicode_numerical_value(0x1C58, 8.0, 8).	% Nd       OL CHIKI DIGIT EIGHT
unicode_numerical_value(0x2078, 8.0, 8).	% No       SUPERSCRIPT EIGHT
unicode_numerical_value(0x2088, 8.0, 8).	% No       SUBSCRIPT EIGHT
unicode_numerical_value(0x2167, 8.0, 8).	% Nl       ROMAN NUMERAL EIGHT
unicode_numerical_value(0x2177, 8.0, 8).	% Nl       SMALL ROMAN NUMERAL EIGHT
unicode_numerical_value(0x2467, 8.0, 8).	% No       CIRCLED DIGIT EIGHT
unicode_numerical_value(0x247B, 8.0, 8).	% No       PARENTHESIZED DIGIT EIGHT
unicode_numerical_value(0x248F, 8.0, 8).	% No       DIGIT EIGHT FULL STOP
unicode_numerical_value(0x24FC, 8.0, 8).	% No       DOUBLE CIRCLED DIGIT EIGHT
unicode_numerical_value(0x277D, 8.0, 8).	% No       DINGBAT NEGATIVE CIRCLED DIGIT EIGHT
unicode_numerical_value(0x2787, 8.0, 8).	% No       DINGBAT CIRCLED SANS-SERIF DIGIT EIGHT
unicode_numerical_value(0x2791, 8.0, 8).	% No       DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT EIGHT
unicode_numerical_value(0x3028, 8.0, 8).	% Nl       HANGZHOU NUMERAL EIGHT
unicode_numerical_value(0x3227, 8.0, 8).	% No       PARENTHESIZED IDEOGRAPH EIGHT
unicode_numerical_value(0x3287, 8.0, 8).	% No       CIRCLED IDEOGRAPH EIGHT
unicode_numerical_value(0x516B, 8.0, 8).	% Lo       CJK UNIFIED IDEOGRAPH-516B
unicode_numerical_value(0x634C, 8.0, 8).	% Lo       CJK UNIFIED IDEOGRAPH-634C
unicode_numerical_value(0xA628, 8.0, 8).	% Nd       VAI DIGIT EIGHT
unicode_numerical_value(0xA6ED, 8.0, 8).	% Nl       BAMUM LETTER FAAMAE
unicode_numerical_value(0xA8D8, 8.0, 8).	% Nd       SAURASHTRA DIGIT EIGHT
unicode_numerical_value(0xA908, 8.0, 8).	% Nd       KAYAH LI DIGIT EIGHT
unicode_numerical_value(0xA9D8, 8.0, 8).	% Nd       JAVANESE DIGIT EIGHT
unicode_numerical_value(0xAA58, 8.0, 8).	% Nd       CHAM DIGIT EIGHT
unicode_numerical_value(0xABF8, 8.0, 8).	% Nd       MEETEI MAYEK DIGIT EIGHT
unicode_numerical_value(0xFF18, 8.0, 8).	% Nd       FULLWIDTH DIGIT EIGHT
unicode_numerical_value(0x1010E, 8.0, 8).	% No       AEGEAN NUMBER EIGHT
unicode_numerical_value(0x104A8, 8.0, 8).	% Nd       OSMANYA DIGIT EIGHT
unicode_numerical_value(0x10E67, 8.0, 8).	% No       RUMI DIGIT EIGHT
unicode_numerical_value(0x11059, 8.0, 8).	% No       BRAHMI NUMBER EIGHT
unicode_numerical_value(0x1106E, 8.0, 8).	% Nd       BRAHMI DIGIT EIGHT
unicode_numerical_value(0x110F8, 8.0, 8).	% Nd       SORA SOMPENG DIGIT EIGHT
unicode_numerical_value(0x1113E, 8.0, 8).	% Nd       CHAKMA DIGIT EIGHT
unicode_numerical_value(0x111D8, 8.0, 8).	% Nd       SHARADA DIGIT EIGHT
unicode_numerical_value(0x116C8, 8.0, 8).	% Nd       TAKRI DIGIT EIGHT
unicode_numerical_value(0x12406, 8.0, 8).	% Nl       CUNEIFORM NUMERIC SIGN EIGHT ASH
unicode_numerical_value(0x1240D, 8.0, 8).	% Nl       CUNEIFORM NUMERIC SIGN EIGHT DISH
unicode_numerical_value(0x12413, 8.0, 8).	% Nl       CUNEIFORM NUMERIC SIGN EIGHT U
unicode_numerical_value(0x1241C, 8.0, 8).	% Nl       CUNEIFORM NUMERIC SIGN EIGHT GESH2
unicode_numerical_value(0x1242A, 8.0, 8).	% Nl       CUNEIFORM NUMERIC SIGN EIGHT SHAR2
%12444..12445  ; 8.0, 8).	% Nl   [2] CUNEIFORM NUMERIC SIGN EIGHT VARIANT FORM USSU..CUNEIFORM NUMERIC SIGN EIGHT VARIANT FORM USSU3
unicode_numerical_value(0x12444, 8.0, 8).	% Nl       CUNEIFORM NUMERIC SIGN EIGHT VARIANT FORM USSU
unicode_numerical_value(0x12445, 8.0, 8).	% Nl       CUNEIFORM NUMERIC SIGN EIGHT VARIANT FORM USSU3
unicode_numerical_value(0x1D367, 8.0, 8).	% No       COUNTING ROD UNIT DIGIT EIGHT
unicode_numerical_value(0x1D7D6, 8.0, 8).	% Nd       MATHEMATICAL BOLD DIGIT EIGHT
unicode_numerical_value(0x1D7E0, 8.0, 8).	% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT EIGHT
unicode_numerical_value(0x1D7EA, 8.0, 8).	% Nd       MATHEMATICAL SANS-SERIF DIGIT EIGHT
unicode_numerical_value(0x1D7F4, 8.0, 8).	% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT EIGHT
unicode_numerical_value(0x1D7FE, 8.0, 8).	% Nd       MATHEMATICAL MONOSPACE DIGIT EIGHT
unicode_numerical_value(0x1F109, 8.0, 8).	% No       DIGIT EIGHT COMMA

% Total code points: 77

% ================================================

unicode_numerical_value(0x0F32, 8.5, 17/2).	% No       TIBETAN DIGIT HALF NINE

% Total code points: 1

% ================================================

unicode_numerical_value(0x0039, 9.0, 9).	% Nd       DIGIT NINE
unicode_numerical_value(0x0669, 9.0, 9).	% Nd       ARABIC-INDIC DIGIT NINE
unicode_numerical_value(0x06F9, 9.0, 9).	% Nd       EXTENDED ARABIC-INDIC DIGIT NINE
unicode_numerical_value(0x07C9, 9.0, 9).	% Nd       NKO DIGIT NINE
unicode_numerical_value(0x096F, 9.0, 9).	% Nd       DEVANAGARI DIGIT NINE
unicode_numerical_value(0x09EF, 9.0, 9).	% Nd       BENGALI DIGIT NINE
unicode_numerical_value(0x0A6F, 9.0, 9).	% Nd       GURMUKHI DIGIT NINE
unicode_numerical_value(0x0AEF, 9.0, 9).	% Nd       GUJARATI DIGIT NINE
unicode_numerical_value(0x0B6F, 9.0, 9).	% Nd       ORIYA DIGIT NINE
unicode_numerical_value(0x0BEF, 9.0, 9).	% Nd       TAMIL DIGIT NINE
unicode_numerical_value(0x0C6F, 9.0, 9).	% Nd       TELUGU DIGIT NINE
unicode_numerical_value(0x0CEF, 9.0, 9).	% Nd       KANNADA DIGIT NINE
unicode_numerical_value(0x0D6F, 9.0, 9).	% Nd       MALAYALAM DIGIT NINE
unicode_numerical_value(0x0E59, 9.0, 9).	% Nd       THAI DIGIT NINE
unicode_numerical_value(0x0ED9, 9.0, 9).	% Nd       LAO DIGIT NINE
unicode_numerical_value(0x0F29, 9.0, 9).	% Nd       TIBETAN DIGIT NINE
unicode_numerical_value(0x1049, 9.0, 9).	% Nd       MYANMAR DIGIT NINE
unicode_numerical_value(0x1099, 9.0, 9).	% Nd       MYANMAR SHAN DIGIT NINE
unicode_numerical_value(0x1371, 9.0, 9).	% No       ETHIOPIC DIGIT NINE
unicode_numerical_value(0x17E9, 9.0, 9).	% Nd       KHMER DIGIT NINE
unicode_numerical_value(0x17F9, 9.0, 9).	% No       KHMER SYMBOL LEK ATTAK PRAM-BUON
unicode_numerical_value(0x1819, 9.0, 9).	% Nd       MONGOLIAN DIGIT NINE
unicode_numerical_value(0x194F, 9.0, 9).	% Nd       LIMBU DIGIT NINE
unicode_numerical_value(0x19D9, 9.0, 9).	% Nd       NEW TAI LUE DIGIT NINE
unicode_numerical_value(0x1A89, 9.0, 9).	% Nd       TAI THAM HORA DIGIT NINE
unicode_numerical_value(0x1A99, 9.0, 9).	% Nd       TAI THAM THAM DIGIT NINE
unicode_numerical_value(0x1B59, 9.0, 9).	% Nd       BALINESE DIGIT NINE
unicode_numerical_value(0x1BB9, 9.0, 9).	% Nd       SUNDANESE DIGIT NINE
unicode_numerical_value(0x1C49, 9.0, 9).	% Nd       LEPCHA DIGIT NINE
unicode_numerical_value(0x1C59, 9.0, 9).	% Nd       OL CHIKI DIGIT NINE
unicode_numerical_value(0x2079, 9.0, 9).	% No       SUPERSCRIPT NINE
unicode_numerical_value(0x2089, 9.0, 9).	% No       SUBSCRIPT NINE
unicode_numerical_value(0x2168, 9.0, 9).	% Nl       ROMAN NUMERAL NINE
unicode_numerical_value(0x2178, 9.0, 9).	% Nl       SMALL ROMAN NUMERAL NINE
unicode_numerical_value(0x2468, 9.0, 9).	% No       CIRCLED DIGIT NINE
unicode_numerical_value(0x247C, 9.0, 9).	% No       PARENTHESIZED DIGIT NINE
unicode_numerical_value(0x2490, 9.0, 9).	% No       DIGIT NINE FULL STOP
unicode_numerical_value(0x24FD, 9.0, 9).	% No       DOUBLE CIRCLED DIGIT NINE
unicode_numerical_value(0x277E, 9.0, 9).	% No       DINGBAT NEGATIVE CIRCLED DIGIT NINE
unicode_numerical_value(0x2788, 9.0, 9).	% No       DINGBAT CIRCLED SANS-SERIF DIGIT NINE
unicode_numerical_value(0x2792, 9.0, 9).	% No       DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT NINE
unicode_numerical_value(0x3029, 9.0, 9).	% Nl       HANGZHOU NUMERAL NINE
unicode_numerical_value(0x3228, 9.0, 9).	% No       PARENTHESIZED IDEOGRAPH NINE
unicode_numerical_value(0x3288, 9.0, 9).	% No       CIRCLED IDEOGRAPH NINE
unicode_numerical_value(0x4E5D, 9.0, 9).	% Lo       CJK UNIFIED IDEOGRAPH-4E5D
unicode_numerical_value(0x5EFE, 9.0, 9).	% Lo       CJK UNIFIED IDEOGRAPH-5EFE
unicode_numerical_value(0x7396, 9.0, 9).	% Lo       CJK UNIFIED IDEOGRAPH-7396
unicode_numerical_value(0xA629, 9.0, 9).	% Nd       VAI DIGIT NINE
unicode_numerical_value(0xA6EE, 9.0, 9).	% Nl       BAMUM LETTER KOVUU
unicode_numerical_value(0xA8D9, 9.0, 9).	% Nd       SAURASHTRA DIGIT NINE
unicode_numerical_value(0xA909, 9.0, 9).	% Nd       KAYAH LI DIGIT NINE
unicode_numerical_value(0xA9D9, 9.0, 9).	% Nd       JAVANESE DIGIT NINE
unicode_numerical_value(0xAA59, 9.0, 9).	% Nd       CHAM DIGIT NINE
unicode_numerical_value(0xABF9, 9.0, 9).	% Nd       MEETEI MAYEK DIGIT NINE
unicode_numerical_value(0xFF19, 9.0, 9).	% Nd       FULLWIDTH DIGIT NINE
unicode_numerical_value(0x1010F, 9.0, 9).	% No       AEGEAN NUMBER NINE
unicode_numerical_value(0x104A9, 9.0, 9).	% Nd       OSMANYA DIGIT NINE
unicode_numerical_value(0x10E68, 9.0, 9).	% No       RUMI DIGIT NINE
unicode_numerical_value(0x1105A, 9.0, 9).	% No       BRAHMI NUMBER NINE
unicode_numerical_value(0x1106F, 9.0, 9).	% Nd       BRAHMI DIGIT NINE
unicode_numerical_value(0x110F9, 9.0, 9).	% Nd       SORA SOMPENG DIGIT NINE
unicode_numerical_value(0x1113F, 9.0, 9).	% Nd       CHAKMA DIGIT NINE
unicode_numerical_value(0x111D9, 9.0, 9).	% Nd       SHARADA DIGIT NINE
unicode_numerical_value(0x116C9, 9.0, 9).	% Nd       TAKRI DIGIT NINE
unicode_numerical_value(0x12407, 9.0, 9).	% Nl       CUNEIFORM NUMERIC SIGN NINE ASH
unicode_numerical_value(0x1240E, 9.0, 9).	% Nl       CUNEIFORM NUMERIC SIGN NINE DISH
unicode_numerical_value(0x12414, 9.0, 9).	% Nl       CUNEIFORM NUMERIC SIGN NINE U
unicode_numerical_value(0x1241D, 9.0, 9).	% Nl       CUNEIFORM NUMERIC SIGN NINE GESH2
unicode_numerical_value(0x1242B, 9.0, 9).	% Nl       CUNEIFORM NUMERIC SIGN NINE SHAR2
%12446..12449  ; 9.0, 9).	% Nl   [4] CUNEIFORM NUMERIC SIGN NINE VARIANT FORM ILIMMU..CUNEIFORM NUMERIC SIGN NINE VARIANT FORM ILIMMU A
unicode_numerical_value(0x12446, 9.0, 9).	% Nl       CUNEIFORM NUMERIC SIGN NINE VARIANT FORM ILIMMU
unicode_numerical_value(0x12447, 9.0, 9).	% Nl       CUNEIFORM NUMERIC SIGN NINE VARIANT FORM ILIMMU3
unicode_numerical_value(0x12448, 9.0, 9).	% Nl       CUNEIFORM NUMERIC SIGN NINE VARIANT FORM ILIMMU4
unicode_numerical_value(0x12449, 9.0, 9).	% Nl       CUNEIFORM NUMERIC SIGN NINE VARIANT FORM ILIMMU A
unicode_numerical_value(0x1D368, 9.0, 9).	% No       COUNTING ROD UNIT DIGIT NINE
unicode_numerical_value(0x1D7D7, 9.0, 9).	% Nd       MATHEMATICAL BOLD DIGIT NINE
unicode_numerical_value(0x1D7E1, 9.0, 9).	% Nd       MATHEMATICAL DOUBLE-STRUCK DIGIT NINE
unicode_numerical_value(0x1D7EB, 9.0, 9).	% Nd       MATHEMATICAL SANS-SERIF DIGIT NINE
unicode_numerical_value(0x1D7F5, 9.0, 9).	% Nd       MATHEMATICAL SANS-SERIF BOLD DIGIT NINE
unicode_numerical_value(0x1D7FF, 9.0, 9).	% Nd       MATHEMATICAL MONOSPACE DIGIT NINE
unicode_numerical_value(0x1F10A, 9.0, 9).	% No       DIGIT NINE COMMA
unicode_numerical_value(0x2F890, 9.0, 9).	% Lo       CJK COMPATIBILITY IDEOGRAPH-2F890

% Total code points: 81

% ================================================

unicode_numerical_value(0x0BF0, 10.0, 10).	% No       TAMIL NUMBER TEN
unicode_numerical_value(0x0D70, 10.0, 10).	% No       MALAYALAM NUMBER TEN
unicode_numerical_value(0x1372, 10.0, 10).	% No       ETHIOPIC NUMBER TEN
unicode_numerical_value(0x2169, 10.0, 10).	% Nl       ROMAN NUMERAL TEN
unicode_numerical_value(0x2179, 10.0, 10).	% Nl       SMALL ROMAN NUMERAL TEN
unicode_numerical_value(0x2469, 10.0, 10).	% No       CIRCLED NUMBER TEN
unicode_numerical_value(0x247D, 10.0, 10).	% No       PARENTHESIZED NUMBER TEN
unicode_numerical_value(0x2491, 10.0, 10).	% No       NUMBER TEN FULL STOP
unicode_numerical_value(0x24FE, 10.0, 10).	% No       DOUBLE CIRCLED NUMBER TEN
unicode_numerical_value(0x277F, 10.0, 10).	% No       DINGBAT NEGATIVE CIRCLED NUMBER TEN
unicode_numerical_value(0x2789, 10.0, 10).	% No       DINGBAT CIRCLED SANS-SERIF NUMBER TEN
unicode_numerical_value(0x2793, 10.0, 10).	% No       DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
unicode_numerical_value(0x3038, 10.0, 10).	% Nl       HANGZHOU NUMERAL TEN
unicode_numerical_value(0x3229, 10.0, 10).	% No       PARENTHESIZED IDEOGRAPH TEN
unicode_numerical_value(0x3248, 10.0, 10).	% No       CIRCLED NUMBER TEN ON BLACK SQUARE
unicode_numerical_value(0x3289, 10.0, 10).	% No       CIRCLED IDEOGRAPH TEN
unicode_numerical_value(0x4EC0, 10.0, 10).	% Lo       CJK UNIFIED IDEOGRAPH-4EC0
unicode_numerical_value(0x5341, 10.0, 10).	% Lo       CJK UNIFIED IDEOGRAPH-5341
unicode_numerical_value(0x62FE, 10.0, 10).	% Lo       CJK UNIFIED IDEOGRAPH-62FE
unicode_numerical_value(0xF973, 10.0, 10).	% Lo       CJK COMPATIBILITY IDEOGRAPH-F973
unicode_numerical_value(0xF9FD, 10.0, 10).	% Lo       CJK COMPATIBILITY IDEOGRAPH-F9FD
unicode_numerical_value(0x10110, 10.0, 10).	% No       AEGEAN NUMBER TEN
unicode_numerical_value(0x10149, 10.0, 10).	% Nl       GREEK ACROPHONIC ATTIC TEN TALENTS
unicode_numerical_value(0x10150, 10.0, 10).	% Nl       GREEK ACROPHONIC ATTIC TEN STATERS
unicode_numerical_value(0x10157, 10.0, 10).	% Nl       GREEK ACROPHONIC ATTIC TEN MNAS
%10160..10164  ; 10.0, 10).	% Nl   [5] GREEK ACROPHONIC TROEZENIAN TEN..GREEK ACROPHONIC THESPIAN TEN
unicode_numerical_value(0x10160, 10.0, 10).	% Nl       GREEK ACROPHONIC TROEZENIAN TEN
unicode_numerical_value(0x10161, 10.0, 10).	% Nl       GREEK ACROPHONIC TROEZENIAN TEN ALTERNATE FORM
unicode_numerical_value(0x10162, 10.0, 10).	% Nl       GREEK ACROPHONIC HERMIONIAN TEN
unicode_numerical_value(0x10163, 10.0, 10).	% Nl       GREEK ACROPHONIC MESSENIAN TEN
unicode_numerical_value(0x10164, 10.0, 10).	% Nl       GREEK ACROPHONIC THESPIAN TEN
unicode_numerical_value(0x10322, 10.0, 10).	% No       OLD ITALIC NUMERAL TEN
unicode_numerical_value(0x103D3, 10.0, 10).	% Nl       OLD PERSIAN NUMBER TEN
unicode_numerical_value(0x1085B, 10.0, 10).	% No       IMPERIAL ARAMAIC NUMBER TEN
unicode_numerical_value(0x10917, 10.0, 10).	% No       PHOENICIAN NUMBER TEN
unicode_numerical_value(0x10A44, 10.0, 10).	% No       KHAROSHTHI NUMBER TEN
unicode_numerical_value(0x10B5C, 10.0, 10).	% No       INSCRIPTIONAL PARTHIAN NUMBER TEN
unicode_numerical_value(0x10B7C, 10.0, 10).	% No       INSCRIPTIONAL PAHLAVI NUMBER TEN
unicode_numerical_value(0x10E69, 10.0, 10).	% No       RUMI NUMBER TEN
unicode_numerical_value(0x1105B, 10.0, 10).	% No       BRAHMI NUMBER TEN
unicode_numerical_value(0x1D369, 10.0, 10).	% No       COUNTING ROD TENS DIGIT ONE

% Total code points: 40

% ================================================

unicode_numerical_value(0x216A, 11.0, 11).	% Nl       ROMAN NUMERAL ELEVEN
unicode_numerical_value(0x217A, 11.0, 11).	% Nl       SMALL ROMAN NUMERAL ELEVEN
unicode_numerical_value(0x246A, 11.0, 11).	% No       CIRCLED NUMBER ELEVEN
unicode_numerical_value(0x247E, 11.0, 11).	% No       PARENTHESIZED NUMBER ELEVEN
unicode_numerical_value(0x2492, 11.0, 11).	% No       NUMBER ELEVEN FULL STOP
unicode_numerical_value(0x24EB, 11.0, 11).	% No       NEGATIVE CIRCLED NUMBER ELEVEN

% Total code points: 6

% ================================================

unicode_numerical_value(0x216B, 12.0, 12).	% Nl       ROMAN NUMERAL TWELVE
unicode_numerical_value(0x217B, 12.0, 12).	% Nl       SMALL ROMAN NUMERAL TWELVE
unicode_numerical_value(0x246B, 12.0, 12).	% No       CIRCLED NUMBER TWELVE
unicode_numerical_value(0x247F, 12.0, 12).	% No       PARENTHESIZED NUMBER TWELVE
unicode_numerical_value(0x2493, 12.0, 12).	% No       NUMBER TWELVE FULL STOP
unicode_numerical_value(0x24EC, 12.0, 12).	% No       NEGATIVE CIRCLED NUMBER TWELVE

% Total code points: 6

% ================================================

unicode_numerical_value(0x246C, 13.0, 13).	% No       CIRCLED NUMBER THIRTEEN
unicode_numerical_value(0x2480, 13.0, 13).	% No       PARENTHESIZED NUMBER THIRTEEN
unicode_numerical_value(0x2494, 13.0, 13).	% No       NUMBER THIRTEEN FULL STOP
unicode_numerical_value(0x24ED, 13.0, 13).	% No       NEGATIVE CIRCLED NUMBER THIRTEEN

% Total code points: 4

% ================================================

unicode_numerical_value(0x246D, 14.0, 14).	% No       CIRCLED NUMBER FOURTEEN
unicode_numerical_value(0x2481, 14.0, 14).	% No       PARENTHESIZED NUMBER FOURTEEN
unicode_numerical_value(0x2495, 14.0, 14).	% No       NUMBER FOURTEEN FULL STOP
unicode_numerical_value(0x24EE, 14.0, 14).	% No       NEGATIVE CIRCLED NUMBER FOURTEEN

% Total code points: 4

% ================================================

unicode_numerical_value(0x246E, 15.0, 15).	% No       CIRCLED NUMBER FIFTEEN
unicode_numerical_value(0x2482, 15.0, 15).	% No       PARENTHESIZED NUMBER FIFTEEN
unicode_numerical_value(0x2496, 15.0, 15).	% No       NUMBER FIFTEEN FULL STOP
unicode_numerical_value(0x24EF, 15.0, 15).	% No       NEGATIVE CIRCLED NUMBER FIFTEEN

% Total code points: 4

% ================================================

unicode_numerical_value(0x09F9, 16.0, 16).	% No       BENGALI CURRENCY DENOMINATOR SIXTEEN
unicode_numerical_value(0x246F, 16.0, 16).	% No       CIRCLED NUMBER SIXTEEN
unicode_numerical_value(0x2483, 16.0, 16).	% No       PARENTHESIZED NUMBER SIXTEEN
unicode_numerical_value(0x2497, 16.0, 16).	% No       NUMBER SIXTEEN FULL STOP
unicode_numerical_value(0x24F0, 16.0, 16).	% No       NEGATIVE CIRCLED NUMBER SIXTEEN

% Total code points: 5

% ================================================

unicode_numerical_value(0x16EE, 17.0, 17).	% Nl       RUNIC ARLAUG SYMBOL
unicode_numerical_value(0x2470, 17.0, 17).	% No       CIRCLED NUMBER SEVENTEEN
unicode_numerical_value(0x2484, 17.0, 17).	% No       PARENTHESIZED NUMBER SEVENTEEN
unicode_numerical_value(0x2498, 17.0, 17).	% No       NUMBER SEVENTEEN FULL STOP
unicode_numerical_value(0x24F1, 17.0, 17).	% No       NEGATIVE CIRCLED NUMBER SEVENTEEN

% Total code points: 5

% ================================================

unicode_numerical_value(0x16EF, 18.0, 18).	% Nl       RUNIC TVIMADUR SYMBOL
unicode_numerical_value(0x2471, 18.0, 18).	% No       CIRCLED NUMBER EIGHTEEN
unicode_numerical_value(0x2485, 18.0, 18).	% No       PARENTHESIZED NUMBER EIGHTEEN
unicode_numerical_value(0x2499, 18.0, 18).	% No       NUMBER EIGHTEEN FULL STOP
unicode_numerical_value(0x24F2, 18.0, 18).	% No       NEGATIVE CIRCLED NUMBER EIGHTEEN

% Total code points: 5

% ================================================

unicode_numerical_value(0x16F0, 19.0, 19).	% Nl       RUNIC BELGTHOR SYMBOL
unicode_numerical_value(0x2472, 19.0, 19).	% No       CIRCLED NUMBER NINETEEN
unicode_numerical_value(0x2486, 19.0, 19).	% No       PARENTHESIZED NUMBER NINETEEN
unicode_numerical_value(0x249A, 19.0, 19).	% No       NUMBER NINETEEN FULL STOP
unicode_numerical_value(0x24F3, 19.0, 19).	% No       NEGATIVE CIRCLED NUMBER NINETEEN

% Total code points: 5

% ================================================

unicode_numerical_value(0x1373, 20.0, 20).	% No       ETHIOPIC NUMBER TWENTY
unicode_numerical_value(0x2473, 20.0, 20).	% No       CIRCLED NUMBER TWENTY
unicode_numerical_value(0x2487, 20.0, 20).	% No       PARENTHESIZED NUMBER TWENTY
unicode_numerical_value(0x249B, 20.0, 20).	% No       NUMBER TWENTY FULL STOP
unicode_numerical_value(0x24F4, 20.0, 20).	% No       NEGATIVE CIRCLED NUMBER TWENTY
unicode_numerical_value(0x3039, 20.0, 20).	% Nl       HANGZHOU NUMERAL TWENTY
unicode_numerical_value(0x3249, 20.0, 20).	% No       CIRCLED NUMBER TWENTY ON BLACK SQUARE
unicode_numerical_value(0x5344, 20.0, 20).	% Lo       CJK UNIFIED IDEOGRAPH-5344
unicode_numerical_value(0x5EFF, 20.0, 20).	% Lo       CJK UNIFIED IDEOGRAPH-5EFF
unicode_numerical_value(0x10111, 20.0, 20).	% No       AEGEAN NUMBER TWENTY
unicode_numerical_value(0x103D4, 20.0, 20).	% Nl       OLD PERSIAN NUMBER TWENTY
unicode_numerical_value(0x1085C, 20.0, 20).	% No       IMPERIAL ARAMAIC NUMBER TWENTY
unicode_numerical_value(0x10918, 20.0, 20).	% No       PHOENICIAN NUMBER TWENTY
unicode_numerical_value(0x10A45, 20.0, 20).	% No       KHAROSHTHI NUMBER TWENTY
unicode_numerical_value(0x10B5D, 20.0, 20).	% No       INSCRIPTIONAL PARTHIAN NUMBER TWENTY
unicode_numerical_value(0x10B7D, 20.0, 20).	% No       INSCRIPTIONAL PAHLAVI NUMBER TWENTY
unicode_numerical_value(0x10E6A, 20.0, 20).	% No       RUMI NUMBER TWENTY
unicode_numerical_value(0x1105C, 20.0, 20).	% No       BRAHMI NUMBER TWENTY
unicode_numerical_value(0x1D36A, 20.0, 20).	% No       COUNTING ROD TENS DIGIT TWO

% Total code points: 19

% ================================================

unicode_numerical_value(0x3251, 21.0, 21).	% No       CIRCLED NUMBER TWENTY ONE

% Total code points: 1

% ================================================

unicode_numerical_value(0x3252, 22.0, 22).	% No       CIRCLED NUMBER TWENTY TWO

% Total code points: 1

% ================================================

unicode_numerical_value(0x3253, 23.0, 23).	% No       CIRCLED NUMBER TWENTY THREE

% Total code points: 1

% ================================================

unicode_numerical_value(0x3254, 24.0, 24).	% No       CIRCLED NUMBER TWENTY FOUR

% Total code points: 1

% ================================================

unicode_numerical_value(0x3255, 25.0, 25).	% No       CIRCLED NUMBER TWENTY FIVE

% Total code points: 1

% ================================================

unicode_numerical_value(0x3256, 26.0, 26).	% No       CIRCLED NUMBER TWENTY SIX

% Total code points: 1

% ================================================

unicode_numerical_value(0x3257, 27.0, 27).	% No       CIRCLED NUMBER TWENTY SEVEN

% Total code points: 1

% ================================================

unicode_numerical_value(0x3258, 28.0, 28).	% No       CIRCLED NUMBER TWENTY EIGHT

% Total code points: 1

% ================================================

unicode_numerical_value(0x3259, 29.0, 29).	% No       CIRCLED NUMBER TWENTY NINE

% Total code points: 1

% ================================================

unicode_numerical_value(0x1374, 30.0, 30).	% No       ETHIOPIC NUMBER THIRTY
unicode_numerical_value(0x303A, 30.0, 30).	% Nl       HANGZHOU NUMERAL THIRTY
unicode_numerical_value(0x324A, 30.0, 30).	% No       CIRCLED NUMBER THIRTY ON BLACK SQUARE
unicode_numerical_value(0x325A, 30.0, 30).	% No       CIRCLED NUMBER THIRTY
unicode_numerical_value(0x5345, 30.0, 30).	% Lo       CJK UNIFIED IDEOGRAPH-5345
unicode_numerical_value(0x10112, 30.0, 30).	% No       AEGEAN NUMBER THIRTY
unicode_numerical_value(0x10165, 30.0, 30).	% Nl       GREEK ACROPHONIC THESPIAN THIRTY
unicode_numerical_value(0x10E6B, 30.0, 30).	% No       RUMI NUMBER THIRTY
unicode_numerical_value(0x1105D, 30.0, 30).	% No       BRAHMI NUMBER THIRTY
unicode_numerical_value(0x1D36B, 30.0, 30).	% No       COUNTING ROD TENS DIGIT THREE
unicode_numerical_value(0x20983, 30.0, 30).	% Lo       CJK UNIFIED IDEOGRAPH-20983

% Total code points: 11

% ================================================

unicode_numerical_value(0x325B, 31.0, 31).	% No       CIRCLED NUMBER THIRTY ONE

% Total code points: 1

% ================================================

unicode_numerical_value(0x325C, 32.0, 32).	% No       CIRCLED NUMBER THIRTY TWO

% Total code points: 1

% ================================================

unicode_numerical_value(0x325D, 33.0, 33).	% No       CIRCLED NUMBER THIRTY THREE

% Total code points: 1

% ================================================

unicode_numerical_value(0x325E, 34.0, 34).	% No       CIRCLED NUMBER THIRTY FOUR

% Total code points: 1

% ================================================

unicode_numerical_value(0x325F, 35.0, 35).	% No       CIRCLED NUMBER THIRTY FIVE

% Total code points: 1

% ================================================

unicode_numerical_value(0x32B1, 36.0, 36).	% No       CIRCLED NUMBER THIRTY SIX

% Total code points: 1

% ================================================

unicode_numerical_value(0x32B2, 37.0, 37).	% No       CIRCLED NUMBER THIRTY SEVEN

% Total code points: 1

% ================================================

unicode_numerical_value(0x32B3, 38.0, 38).	% No       CIRCLED NUMBER THIRTY EIGHT

% Total code points: 1

% ================================================

unicode_numerical_value(0x32B4, 39.0, 39).	% No       CIRCLED NUMBER THIRTY NINE

% Total code points: 1

% ================================================

unicode_numerical_value(0x1375, 40.0, 40).	% No       ETHIOPIC NUMBER FORTY
unicode_numerical_value(0x324B, 40.0, 40).	% No       CIRCLED NUMBER FORTY ON BLACK SQUARE
unicode_numerical_value(0x32B5, 40.0, 40).	% No       CIRCLED NUMBER FORTY
unicode_numerical_value(0x534C, 40.0, 40).	% Lo       CJK UNIFIED IDEOGRAPH-534C
unicode_numerical_value(0x10113, 40.0, 40).	% No       AEGEAN NUMBER FORTY
unicode_numerical_value(0x10E6C, 40.0, 40).	% No       RUMI NUMBER FORTY
unicode_numerical_value(0x1105E, 40.0, 40).	% No       BRAHMI NUMBER FORTY
unicode_numerical_value(0x1D36C, 40.0, 40).	% No       COUNTING ROD TENS DIGIT FOUR
unicode_numerical_value(0x2098C, 40.0, 40).	% Lo       CJK UNIFIED IDEOGRAPH-2098C
unicode_numerical_value(0x2099C, 40.0, 40).	% Lo       CJK UNIFIED IDEOGRAPH-2099C

% Total code points: 10

% ================================================

unicode_numerical_value(0x32B6, 41.0, 41).	% No       CIRCLED NUMBER FORTY ONE

% Total code points: 1

% ================================================

unicode_numerical_value(0x32B7, 42.0, 42).	% No       CIRCLED NUMBER FORTY TWO

% Total code points: 1

% ================================================

unicode_numerical_value(0x32B8, 43.0, 43).	% No       CIRCLED NUMBER FORTY THREE

% Total code points: 1

% ================================================

unicode_numerical_value(0x32B9, 44.0, 44).	% No       CIRCLED NUMBER FORTY FOUR

% Total code points: 1

% ================================================

unicode_numerical_value(0x32BA, 45.0, 45).	% No       CIRCLED NUMBER FORTY FIVE

% Total code points: 1

% ================================================

unicode_numerical_value(0x32BB, 46.0, 46).	% No       CIRCLED NUMBER FORTY SIX

% Total code points: 1

% ================================================

unicode_numerical_value(0x32BC, 47.0, 47).	% No       CIRCLED NUMBER FORTY SEVEN

% Total code points: 1

% ================================================

unicode_numerical_value(0x32BD, 48.0, 48).	% No       CIRCLED NUMBER FORTY EIGHT

% Total code points: 1

% ================================================

unicode_numerical_value(0x32BE, 49.0, 49).	% No       CIRCLED NUMBER FORTY NINE

% Total code points: 1

% ================================================

unicode_numerical_value(0x1376, 50.0, 50).	% No       ETHIOPIC NUMBER FIFTY
unicode_numerical_value(0x216C, 50.0, 50).	% Nl       ROMAN NUMERAL FIFTY
unicode_numerical_value(0x217C, 50.0, 50).	% Nl       SMALL ROMAN NUMERAL FIFTY
unicode_numerical_value(0x2186, 50.0, 50).	% Nl       ROMAN NUMERAL FIFTY EARLY FORM
unicode_numerical_value(0x324C, 50.0, 50).	% No       CIRCLED NUMBER FIFTY ON BLACK SQUARE
unicode_numerical_value(0x32BF, 50.0, 50).	% No       CIRCLED NUMBER FIFTY
unicode_numerical_value(0x10114, 50.0, 50).	% No       AEGEAN NUMBER FIFTY
unicode_numerical_value(0x10144, 50.0, 50).	% Nl       GREEK ACROPHONIC ATTIC FIFTY
unicode_numerical_value(0x1014A, 50.0, 50).	% Nl       GREEK ACROPHONIC ATTIC FIFTY TALENTS
unicode_numerical_value(0x10151, 50.0, 50).	% Nl       GREEK ACROPHONIC ATTIC FIFTY STATERS
%10166..10169  ; 50.0, 50).	% Nl   [4] GREEK ACROPHONIC TROEZENIAN FIFTY..GREEK ACROPHONIC THESPIAN FIFTY
unicode_numerical_value(0x10166, 50.0, 50).	% Nl       GREEK ACROPHONIC TROEZENIAN FIFTY
unicode_numerical_value(0x10167, 50.0, 50).	% Nl       GREEK ACROPHONIC TROEZENIAN FIFTY ALTERNATE FORM
unicode_numerical_value(0x10168, 50.0, 50).	% Nl       GREEK ACROPHONIC HERMIONIAN FIFTY
unicode_numerical_value(0x10169, 50.0, 50).	% Nl       GREEK ACROPHONIC THESPIAN FIFTY
unicode_numerical_value(0x10174, 50.0, 50).	% Nl       GREEK ACROPHONIC STRATIAN FIFTY MNAS
unicode_numerical_value(0x10323, 50.0, 50).	% No       OLD ITALIC NUMERAL FIFTY
unicode_numerical_value(0x10A7E, 50.0, 50).	% No       OLD SOUTH ARABIAN NUMBER FIFTY
unicode_numerical_value(0x10E6D, 50.0, 50).	% No       RUMI NUMBER FIFTY
unicode_numerical_value(0x1105F, 50.0, 50).	% No       BRAHMI NUMBER FIFTY
unicode_numerical_value(0x1D36D, 50.0, 50).	% No       COUNTING ROD TENS DIGIT FIVE

% Total code points: 20

% ================================================

unicode_numerical_value(0x1377, 60.0, 60).	% No       ETHIOPIC NUMBER SIXTY
unicode_numerical_value(0x324D, 60.0, 60).	% No       CIRCLED NUMBER SIXTY ON BLACK SQUARE
unicode_numerical_value(0x10115, 60.0, 60).	% No       AEGEAN NUMBER SIXTY
unicode_numerical_value(0x10E6E, 60.0, 60).	% No       RUMI NUMBER SIXTY
unicode_numerical_value(0x11060, 60.0, 60).	% No       BRAHMI NUMBER SIXTY
unicode_numerical_value(0x1D36E, 60.0, 60).	% No       COUNTING ROD TENS DIGIT SIX

% Total code points: 6

% ================================================

unicode_numerical_value(0x1378, 70.0, 70).	% No       ETHIOPIC NUMBER SEVENTY
unicode_numerical_value(0x324E, 70.0, 70).	% No       CIRCLED NUMBER SEVENTY ON BLACK SQUARE
unicode_numerical_value(0x10116, 70.0, 70).	% No       AEGEAN NUMBER SEVENTY
unicode_numerical_value(0x10E6F, 70.0, 70).	% No       RUMI NUMBER SEVENTY
unicode_numerical_value(0x11061, 70.0, 70).	% No       BRAHMI NUMBER SEVENTY
unicode_numerical_value(0x1D36F, 70.0, 70).	% No       COUNTING ROD TENS DIGIT SEVEN

% Total code points: 6

% ================================================

unicode_numerical_value(0x1379, 80.0, 80).	% No       ETHIOPIC NUMBER EIGHTY
unicode_numerical_value(0x324F, 80.0, 80).	% No       CIRCLED NUMBER EIGHTY ON BLACK SQUARE
unicode_numerical_value(0x10117, 80.0, 80).	% No       AEGEAN NUMBER EIGHTY
unicode_numerical_value(0x10E70, 80.0, 80).	% No       RUMI NUMBER EIGHTY
unicode_numerical_value(0x11062, 80.0, 80).	% No       BRAHMI NUMBER EIGHTY
unicode_numerical_value(0x1D370, 80.0, 80).	% No       COUNTING ROD TENS DIGIT EIGHT

% Total code points: 6

% ================================================

unicode_numerical_value(0x137A, 90.0, 90).	% No       ETHIOPIC NUMBER NINETY
unicode_numerical_value(0x10118, 90.0, 90).	% No       AEGEAN NUMBER NINETY
unicode_numerical_value(0x10341, 90.0, 90).	% Nl       GOTHIC LETTER NINETY
unicode_numerical_value(0x10E71, 90.0, 90).	% No       RUMI NUMBER NINETY
unicode_numerical_value(0x11063, 90.0, 90).	% No       BRAHMI NUMBER NINETY
unicode_numerical_value(0x1D371, 90.0, 90).	% No       COUNTING ROD TENS DIGIT NINE

% Total code points: 6

% ================================================

unicode_numerical_value(0x0BF1, 100.0, 100).	% No       TAMIL NUMBER ONE HUNDRED
unicode_numerical_value(0x0D71, 100.0, 100).	% No       MALAYALAM NUMBER ONE HUNDRED
unicode_numerical_value(0x137B, 100.0, 100).	% No       ETHIOPIC NUMBER HUNDRED
unicode_numerical_value(0x216D, 100.0, 100).	% Nl       ROMAN NUMERAL ONE HUNDRED
unicode_numerical_value(0x217D, 100.0, 100).	% Nl       SMALL ROMAN NUMERAL ONE HUNDRED
unicode_numerical_value(0x4F70, 100.0, 100).	% Lo       CJK UNIFIED IDEOGRAPH-4F70
unicode_numerical_value(0x767E, 100.0, 100).	% Lo       CJK UNIFIED IDEOGRAPH-767E
unicode_numerical_value(0x964C, 100.0, 100).	% Lo       CJK UNIFIED IDEOGRAPH-964C
unicode_numerical_value(0x10119, 100.0, 100).	% No       AEGEAN NUMBER ONE HUNDRED
unicode_numerical_value(0x1014B, 100.0, 100).	% Nl       GREEK ACROPHONIC ATTIC ONE HUNDRED TALENTS
unicode_numerical_value(0x10152, 100.0, 100).	% Nl       GREEK ACROPHONIC ATTIC ONE HUNDRED STATERS
unicode_numerical_value(0x1016A, 100.0, 100).	% Nl       GREEK ACROPHONIC THESPIAN ONE HUNDRED
unicode_numerical_value(0x103D5, 100.0, 100).	% Nl       OLD PERSIAN NUMBER HUNDRED
unicode_numerical_value(0x1085D, 100.0, 100).	% No       IMPERIAL ARAMAIC NUMBER ONE HUNDRED
unicode_numerical_value(0x10919, 100.0, 100).	% No       PHOENICIAN NUMBER ONE HUNDRED
unicode_numerical_value(0x10A46, 100.0, 100).	% No       KHAROSHTHI NUMBER ONE HUNDRED
unicode_numerical_value(0x10B5E, 100.0, 100).	% No       INSCRIPTIONAL PARTHIAN NUMBER ONE HUNDRED
unicode_numerical_value(0x10B7E, 100.0, 100).	% No       INSCRIPTIONAL PAHLAVI NUMBER ONE HUNDRED
unicode_numerical_value(0x10E72, 100.0, 100).	% No       RUMI NUMBER ONE HUNDRED
unicode_numerical_value(0x11064, 100.0, 100).	% No       BRAHMI NUMBER ONE HUNDRED

% Total code points: 20

% ================================================

unicode_numerical_value(0x1011A, 200.0, 200).	% No       AEGEAN NUMBER TWO HUNDRED
unicode_numerical_value(0x10E73, 200.0, 200).	% No       RUMI NUMBER TWO HUNDRED

% Total code points: 2

% ================================================

unicode_numerical_value(0x1011B, 300.0, 300).	% No       AEGEAN NUMBER THREE HUNDRED
unicode_numerical_value(0x1016B, 300.0, 300).	% Nl       GREEK ACROPHONIC THESPIAN THREE HUNDRED
unicode_numerical_value(0x10E74, 300.0, 300).	% No       RUMI NUMBER THREE HUNDRED

% Total code points: 3

% ================================================

unicode_numerical_value(0x1011C, 400.0, 400).	% No       AEGEAN NUMBER FOUR HUNDRED
unicode_numerical_value(0x10E75, 400.0, 400).	% No       RUMI NUMBER FOUR HUNDRED

% Total code points: 2

% ================================================

unicode_numerical_value(0x216E, 500.0, 500).	% Nl       ROMAN NUMERAL FIVE HUNDRED
unicode_numerical_value(0x217E, 500.0, 500).	% Nl       SMALL ROMAN NUMERAL FIVE HUNDRED
unicode_numerical_value(0x1011D, 500.0, 500).	% No       AEGEAN NUMBER FIVE HUNDRED
unicode_numerical_value(0x10145, 500.0, 500).	% Nl       GREEK ACROPHONIC ATTIC FIVE HUNDRED
unicode_numerical_value(0x1014C, 500.0, 500).	% Nl       GREEK ACROPHONIC ATTIC FIVE HUNDRED TALENTS
unicode_numerical_value(0x10153, 500.0, 500).	% Nl       GREEK ACROPHONIC ATTIC FIVE HUNDRED STATERS
%1016C..10170  ; 500.0, 500).	% Nl   [5] GREEK ACROPHONIC EPIDAUREAN FIVE HUNDRED..GREEK ACROPHONIC NAXIAN FIVE HUNDRED
unicode_numerical_value(0x1016C, 500.0, 500).	% Nl       GREEK ACROPHONIC EPIDAUREAN FIVE HUNDRED
unicode_numerical_value(0x1016D, 500.0, 500).	% Nl       GREEK ACROPHONIC TROEZENIAN FIVE HUNDRED
unicode_numerical_value(0x1016E, 500.0, 500).	% Nl       GREEK ACROPHONIC THESPIAN FIVE HUNDRED
unicode_numerical_value(0x1016F, 500.0, 500).	% Nl       GREEK ACROPHONIC CARYSTIAN FIVE HUNDRED
unicode_numerical_value(0x10170, 500.0, 500).	% Nl       GREEK ACROPHONIC NAXIAN FIVE HUNDRED
unicode_numerical_value(0x10E76, 500.0, 500).	% No       RUMI NUMBER FIVE HUNDRED

% Total code points: 12

% ================================================

unicode_numerical_value(0x1011E, 600.0, 600).	% No       AEGEAN NUMBER SIX HUNDRED
unicode_numerical_value(0x10E77, 600.0, 600).	% No       RUMI NUMBER SIX HUNDRED

% Total code points: 2

% ================================================

unicode_numerical_value(0x1011F, 700.0, 700).	% No       AEGEAN NUMBER SEVEN HUNDRED
unicode_numerical_value(0x10E78, 700.0, 700).	% No       RUMI NUMBER SEVEN HUNDRED

% Total code points: 2

% ================================================

unicode_numerical_value(0x10120, 800.0, 800).	% No       AEGEAN NUMBER EIGHT HUNDRED
unicode_numerical_value(0x10E79, 800.0, 800).	% No       RUMI NUMBER EIGHT HUNDRED

% Total code points: 2

% ================================================

unicode_numerical_value(0x10121, 900.0, 900).	% No       AEGEAN NUMBER NINE HUNDRED
unicode_numerical_value(0x1034A, 900.0, 900).	% Nl       GOTHIC LETTER NINE HUNDRED
unicode_numerical_value(0x10E7A, 900.0, 900).	% No       RUMI NUMBER NINE HUNDRED

% Total code points: 3

% ================================================

unicode_numerical_value(0x0BF2, 1000.0, 1000).	% No       TAMIL NUMBER ONE THOUSAND
unicode_numerical_value(0x0D72, 1000.0, 1000).	% No       MALAYALAM NUMBER ONE THOUSAND
unicode_numerical_value(0x216F, 1000.0, 1000).	% Nl       ROMAN NUMERAL ONE THOUSAND
%217F..2180    ; 1000.0, 1000).	% Nl   [2] SMALL ROMAN NUMERAL ONE THOUSAND..ROMAN NUMERAL ONE THOUSAND C D
unicode_numerical_value(0x217F, 1000.0, 1000).	% Nl       SMALL ROMAN NUMERAL ONE THOUSAND
unicode_numerical_value(0x2180, 1000.0, 1000).	% Nl       ROMAN NUMERAL ONE THOUSAND C D
unicode_numerical_value(0x4EDF, 1000.0, 1000).	% Lo       CJK UNIFIED IDEOGRAPH-4EDF
unicode_numerical_value(0x5343, 1000.0, 1000).	% Lo       CJK UNIFIED IDEOGRAPH-5343
unicode_numerical_value(0x9621, 1000.0, 1000).	% Lo       CJK UNIFIED IDEOGRAPH-9621
unicode_numerical_value(0x10122, 1000.0, 1000).	% No       AEGEAN NUMBER ONE THOUSAND
unicode_numerical_value(0x1014D, 1000.0, 1000).	% Nl       GREEK ACROPHONIC ATTIC ONE THOUSAND TALENTS
unicode_numerical_value(0x10154, 1000.0, 1000).	% Nl       GREEK ACROPHONIC ATTIC ONE THOUSAND STATERS
unicode_numerical_value(0x10171, 1000.0, 1000).	% Nl       GREEK ACROPHONIC THESPIAN ONE THOUSAND
unicode_numerical_value(0x1085E, 1000.0, 1000).	% No       IMPERIAL ARAMAIC NUMBER ONE THOUSAND
unicode_numerical_value(0x10A47, 1000.0, 1000).	% No       KHAROSHTHI NUMBER ONE THOUSAND
unicode_numerical_value(0x10B5F, 1000.0, 1000).	% No       INSCRIPTIONAL PARTHIAN NUMBER ONE THOUSAND
unicode_numerical_value(0x10B7F, 1000.0, 1000).	% No       INSCRIPTIONAL PAHLAVI NUMBER ONE THOUSAND
unicode_numerical_value(0x11065, 1000.0, 1000).	% No       BRAHMI NUMBER ONE THOUSAND

% Total code points: 17

% ================================================

unicode_numerical_value(0x10123, 2000.0, 2000).	% No       AEGEAN NUMBER TWO THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x10124, 3000.0, 3000).	% No       AEGEAN NUMBER THREE THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x10125, 4000.0, 4000).	% No       AEGEAN NUMBER FOUR THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x2181, 5000.0, 5000).	% Nl       ROMAN NUMERAL FIVE THOUSAND
unicode_numerical_value(0x10126, 5000.0, 5000).	% No       AEGEAN NUMBER FIVE THOUSAND
unicode_numerical_value(0x10146, 5000.0, 5000).	% Nl       GREEK ACROPHONIC ATTIC FIVE THOUSAND
unicode_numerical_value(0x1014E, 5000.0, 5000).	% Nl       GREEK ACROPHONIC ATTIC FIVE THOUSAND TALENTS
unicode_numerical_value(0x10172, 5000.0, 5000).	% Nl       GREEK ACROPHONIC THESPIAN FIVE THOUSAND

% Total code points: 5

% ================================================

unicode_numerical_value(0x10127, 6000.0, 6000).	% No       AEGEAN NUMBER SIX THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x10128, 7000.0, 7000).	% No       AEGEAN NUMBER SEVEN THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x10129, 8000.0, 8000).	% No       AEGEAN NUMBER EIGHT THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x1012A, 9000.0, 9000).	% No       AEGEAN NUMBER NINE THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x137C, 10000.0, 10000).	% No       ETHIOPIC NUMBER TEN THOUSAND
unicode_numerical_value(0x2182, 10000.0, 10000).	% Nl       ROMAN NUMERAL TEN THOUSAND
unicode_numerical_value(0x4E07, 10000.0, 10000).	% Lo       CJK UNIFIED IDEOGRAPH-4E07
unicode_numerical_value(0x842C, 10000.0, 10000).	% Lo       CJK UNIFIED IDEOGRAPH-842C
unicode_numerical_value(0x1012B, 10000.0, 10000).	% No       AEGEAN NUMBER TEN THOUSAND
unicode_numerical_value(0x10155, 10000.0, 10000).	% Nl       GREEK ACROPHONIC ATTIC TEN THOUSAND STATERS
unicode_numerical_value(0x1085F, 10000.0, 10000).	% No       IMPERIAL ARAMAIC NUMBER TEN THOUSAND

% Total code points: 7

% ================================================

unicode_numerical_value(0x1012C, 20000.0, 20000).	% No       AEGEAN NUMBER TWENTY THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x1012D, 30000.0, 30000).	% No       AEGEAN NUMBER THIRTY THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x1012E, 40000.0, 40000).	% No       AEGEAN NUMBER FORTY THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x2187, 50000.0, 50000).	% Nl       ROMAN NUMERAL FIFTY THOUSAND
unicode_numerical_value(0x1012F, 50000.0, 50000).	% No       AEGEAN NUMBER FIFTY THOUSAND
unicode_numerical_value(0x10147, 50000.0, 50000).	% Nl       GREEK ACROPHONIC ATTIC FIFTY THOUSAND
unicode_numerical_value(0x10156, 50000.0, 50000).	% Nl       GREEK ACROPHONIC ATTIC FIFTY THOUSAND STATERS

% Total code points: 4

% ================================================

unicode_numerical_value(0x10130, 60000.0, 60000).	% No       AEGEAN NUMBER SIXTY THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x10131, 70000.0, 70000).	% No       AEGEAN NUMBER SEVENTY THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x10132, 80000.0, 80000).	% No       AEGEAN NUMBER EIGHTY THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x10133, 90000.0, 90000).	% No       AEGEAN NUMBER NINETY THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x2188, 100000.0, 100000).	% Nl       ROMAN NUMERAL ONE HUNDRED THOUSAND

% Total code points: 1

% ================================================

unicode_numerical_value(0x4EBF, 100000000.0, 100000000).	% Lo       CJK UNIFIED IDEOGRAPH-4EBF
unicode_numerical_value(0x5104, 100000000.0, 100000000).	% Lo       CJK UNIFIED IDEOGRAPH-5104

% Total code points: 2

% ================================================

unicode_numerical_value(0x5146, 1000000000000.0, 1000000000000).	% Lo       CJK UNIFIED IDEOGRAPH-5146

% Total code points: 1

% EOF
