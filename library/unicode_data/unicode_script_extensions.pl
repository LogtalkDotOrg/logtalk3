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
# ScriptExtensions-6.2.0.txt
# Date: 2012-08-13, 20:52:17 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2012 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
# The Script_Extensions property indicates which characters are commonly used
# with more than one script, but with a limited number of scripts.
# For each code point, there is one or more property values.  Each such value is a Script property value.
# For more information, see:
#   UAX #24: http://www.unicode.org/reports/tr24/ and
#   UAX #44: http://www.unicode.org/reports/tr44/
#
#  All code points not explicitly listed for Script_Extensions
#  have as their value the corresponding Script property value
#
# @missing: 0000..10FFFF; <script>
*/

% ================================================

% Property:	Script_Extensions

% ================================================

unicode_script_extension(CodePoint, Extension) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_script_extension(CodePointStart, CodePointEnd, Extension),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_script_extension(CodePoint, _, Extension) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_script_extension(CodePointStart, CodePointEnd, Extension),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	;	% missing code point; see original comment above
		unicode_script_extension(CodePoint, Script),
		Extension = [Script]
	).

% Script_Extensions=Arab Syrc

unicode_script_extension(0x064B, 0x0655, ['Arab', 'Syrc']).	% Mn  [11] ARABIC FATHATAN..ARABIC HAMZA BELOW
unicode_script_extension(0x0670, 0x0670, ['Arab', 'Syrc']).	% Mn       ARABIC LETTER SUPERSCRIPT ALEF

% Total code points: 12

% ================================================

% Script_Extensions=Arab Thaa

unicode_script_extension(0x0660, 0x0669, ['Arab', 'Thaa']).	% Nd  [10] ARABIC-INDIC DIGIT ZERO..ARABIC-INDIC DIGIT NINE
unicode_script_extension(0xFDF2, 0xFDF2, ['Arab', 'Thaa']).	% Lo       ARABIC LIGATURE ALLAH ISOLATED FORM
unicode_script_extension(0xFDFD, 0xFDFD, ['Arab', 'Thaa']).	% So       ARABIC LIGATURE BISMILLAH AR-RAHMAN AR-RAHEEM

% Total code points: 12

% ================================================

% Script_Extensions=Armn Geor

unicode_script_extension(0x0589, 0x0589, ['Armn', 'Geor']).	% Po       ARMENIAN FULL STOP

% Total code points: 1

% ================================================

% Script_Extensions=Bopo Hani

unicode_script_extension(0x302A, 0x302D, ['Bopo', 'Hani']).	% Mn   [4] IDEOGRAPHIC LEVEL TONE MARK..IDEOGRAPHIC ENTERING TONE MARK

% Total code points: 4

% ================================================

% Script_Extensions=Cprt Linb

unicode_script_extension(0x10100, 0x10102, ['Cprt', 'Linb']).	% Po   [3] AEGEAN WORD SEPARATOR LINE..AEGEAN CHECK MARK
unicode_script_extension(0x10107, 0x10133, ['Cprt', 'Linb']).	% No  [45] AEGEAN NUMBER ONE..AEGEAN NUMBER NINETY THOUSAND
unicode_script_extension(0x10137, 0x1013F, ['Cprt', 'Linb']).	% So   [9] AEGEAN WEIGHT BASE UNIT..AEGEAN MEASURE THIRD SUBUNIT

% Total code points: 57

% ================================================

% Script_Extensions=Hira Kana

unicode_script_extension(0x3031, 0x3035, ['Hira', 'Kana']).	% Lm   [5] VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF
unicode_script_extension(0x3099, 0x309A, ['Hira', 'Kana']).	% Mn   [2] COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK..COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_script_extension(0x309B, 0x309C, ['Hira', 'Kana']).	% Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_script_extension(0x30A0, 0x30A0, ['Hira', 'Kana']).	% Pd       KATAKANA-HIRAGANA DOUBLE HYPHEN
unicode_script_extension(0x30FC, 0x30FC, ['Hira', 'Kana']).	% Lm       KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_script_extension(0xFF70, 0xFF70, ['Hira', 'Kana']).	% Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_script_extension(0xFF9E, 0xFF9F, ['Hira', 'Kana']).	% Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK

% Total code points: 14

% ================================================

% Script_Extensions=Mong Phag

unicode_script_extension(0x1802, 0x1803, ['Mong', 'Phag']).	% Po   [2] MONGOLIAN COMMA..MONGOLIAN FULL STOP
unicode_script_extension(0x1805, 0x1805, ['Mong', 'Phag']).	% Po       MONGOLIAN FOUR DOTS

% Total code points: 3

% ================================================

% Script_Extensions=Arab Mand Syrc

unicode_script_extension(0x0640, 0x0640, ['Arab', 'Mand', 'Syrc']).	% Lm       ARABIC TATWEEL

% Total code points: 1

% ================================================

% Script_Extensions=Arab Syrc Thaa

unicode_script_extension(0x060C, 0x060C, ['Arab', 'Syrc', 'Thaa']).	% Po       ARABIC COMMA
unicode_script_extension(0x061B, 0x061B, ['Arab', 'Syrc', 'Thaa']).	% Po       ARABIC SEMICOLON
unicode_script_extension(0x061F, 0x061F, ['Arab', 'Syrc', 'Thaa']).	% Po       ARABIC QUESTION MARK

% Total code points: 3

% ================================================

% Script_Extensions=Hani Hira Kana

unicode_script_extension(0x3006, 0x3006, ['Hani', 'Hira', 'Kana']).	% Lo       IDEOGRAPHIC CLOSING MARK
unicode_script_extension(0x303C, 0x303C, ['Hani', 'Hira', 'Kana']).	% Lo       MASU MARK
unicode_script_extension(0x303D, 0x303D, ['Hani', 'Hira', 'Kana']).	% Po       PART ALTERNATION MARK
unicode_script_extension(0x3190, 0x3191, ['Hani', 'Hira', 'Kana']).	% So   [2] IDEOGRAPHIC ANNOTATION LINKING MARK..IDEOGRAPHIC ANNOTATION REVERSE MARK
unicode_script_extension(0x3192, 0x3195, ['Hani', 'Hira', 'Kana']).	% No   [4] IDEOGRAPHIC ANNOTATION ONE MARK..IDEOGRAPHIC ANNOTATION FOUR MARK
unicode_script_extension(0x3196, 0x319F, ['Hani', 'Hira', 'Kana']).	% So  [10] IDEOGRAPHIC ANNOTATION TOP MARK..IDEOGRAPHIC ANNOTATION MAN MARK

% Total code points: 19

% ================================================

% Script_Extensions=Beng Deva Guru Orya Takr

unicode_script_extension(0x0964, 0x0965, ['Beng', 'Deva', 'Guru', 'Orya', 'Takr']).	% Po   [2] DEVANAGARI DANDA..DEVANAGARI DOUBLE DANDA

% Total code points: 2

% ================================================

% Script_Extensions=Buhd Hano Tagb Tglg

unicode_script_extension(0x1735, 0x1736, ['Buhd', 'Hano', 'Tagb', 'Tglg']).	% Po   [2] PHILIPPINE SINGLE PUNCTUATION..PHILIPPINE DOUBLE PUNCTUATION

% Total code points: 2

% ================================================

% Script_Extensions=Bopo Hang Hani Hira Kana

unicode_script_extension(0x3003, 0x3003, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% Po       DITTO MARK
unicode_script_extension(0x3013, 0x3013, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% So       GETA MARK
unicode_script_extension(0x301C, 0x301C, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% Pd       WAVE DASH
unicode_script_extension(0x301D, 0x301D, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% Ps       REVERSED DOUBLE PRIME QUOTATION MARK
unicode_script_extension(0x301E, 0x301F, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% Pe   [2] DOUBLE PRIME QUOTATION MARK..LOW DOUBLE PRIME QUOTATION MARK
unicode_script_extension(0x3030, 0x3030, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% Pd       WAVY DASH
unicode_script_extension(0x3037, 0x3037, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% So       IDEOGRAPHIC TELEGRAPH LINE FEED SEPARATOR SYMBOL
unicode_script_extension(0x303E, 0x303F, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% So   [2] IDEOGRAPHIC VARIATION INDICATOR..IDEOGRAPHIC HALF FILL SPACE
unicode_script_extension(0x31C0, 0x31E3, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% So  [36] CJK STROKE T..CJK STROKE Q
unicode_script_extension(0x3220, 0x3229, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% No  [10] PARENTHESIZED IDEOGRAPH ONE..PARENTHESIZED IDEOGRAPH TEN
unicode_script_extension(0x322A, 0x3243, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% So  [26] PARENTHESIZED IDEOGRAPH MOON..PARENTHESIZED IDEOGRAPH REACH
unicode_script_extension(0x3280, 0x3289, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% No  [10] CIRCLED IDEOGRAPH ONE..CIRCLED IDEOGRAPH TEN
unicode_script_extension(0x328A, 0x32B0, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% So  [39] CIRCLED IDEOGRAPH MOON..CIRCLED IDEOGRAPH NIGHT
unicode_script_extension(0x32C0, 0x32CB, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% So  [12] IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY..IDEOGRAPHIC TELEGRAPH SYMBOL FOR DECEMBER
unicode_script_extension(0x3358, 0x3370, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% So  [25] IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ZERO..IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-FOUR
unicode_script_extension(0x337B, 0x337F, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% So   [5] SQUARE ERA NAME HEISEI..SQUARE CORPORATION
unicode_script_extension(0x33E0, 0x33FE, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% So  [31] IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY ONE..IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTY-ONE
unicode_script_extension(0xFE45, 0xFE46, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana']).	% Po   [2] SESAME DOT..WHITE SESAME DOT

% Total code points: 206

% ================================================

% Script_Extensions=Deva Gujr Guru Kthi Takr

unicode_script_extension(0xA830, 0xA835, ['Deva', 'Gujr', 'Guru', 'Kthi', 'Takr']).	% No   [6] NORTH INDIC FRACTION ONE QUARTER..NORTH INDIC FRACTION THREE SIXTEENTHS
unicode_script_extension(0xA836, 0xA837, ['Deva', 'Gujr', 'Guru', 'Kthi', 'Takr']).	% So   [2] NORTH INDIC QUARTER MARK..NORTH INDIC PLACEHOLDER MARK
unicode_script_extension(0xA838, 0xA838, ['Deva', 'Gujr', 'Guru', 'Kthi', 'Takr']).	% Sc       NORTH INDIC RUPEE MARK
unicode_script_extension(0xA839, 0xA839, ['Deva', 'Gujr', 'Guru', 'Kthi', 'Takr']).	% So       NORTH INDIC QUANTITY MARK

% Total code points: 10

% ================================================

% Script_Extensions=Bopo Hang Hani Hira Kana Yiii

unicode_script_extension(0x3001, 0x3002, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Po   [2] IDEOGRAPHIC COMMA..IDEOGRAPHIC FULL STOP
unicode_script_extension(0x3008, 0x3008, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Ps       LEFT ANGLE BRACKET
unicode_script_extension(0x3009, 0x3009, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Pe       RIGHT ANGLE BRACKET
unicode_script_extension(0x300A, 0x300A, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Ps       LEFT DOUBLE ANGLE BRACKET
unicode_script_extension(0x300B, 0x300B, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Pe       RIGHT DOUBLE ANGLE BRACKET
unicode_script_extension(0x300C, 0x300C, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Ps       LEFT CORNER BRACKET
unicode_script_extension(0x300D, 0x300D, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Pe       RIGHT CORNER BRACKET
unicode_script_extension(0x300E, 0x300E, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Ps       LEFT WHITE CORNER BRACKET
unicode_script_extension(0x300F, 0x300F, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Pe       RIGHT WHITE CORNER BRACKET
unicode_script_extension(0x3010, 0x3010, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Ps       LEFT BLACK LENTICULAR BRACKET
unicode_script_extension(0x3011, 0x3011, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Pe       RIGHT BLACK LENTICULAR BRACKET
unicode_script_extension(0x3014, 0x3014, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Ps       LEFT TORTOISE SHELL BRACKET
unicode_script_extension(0x3015, 0x3015, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Pe       RIGHT TORTOISE SHELL BRACKET
unicode_script_extension(0x3016, 0x3016, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Ps       LEFT WHITE LENTICULAR BRACKET
unicode_script_extension(0x3017, 0x3017, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Pe       RIGHT WHITE LENTICULAR BRACKET
unicode_script_extension(0x3018, 0x3018, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Ps       LEFT WHITE TORTOISE SHELL BRACKET
unicode_script_extension(0x3019, 0x3019, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Pe       RIGHT WHITE TORTOISE SHELL BRACKET
unicode_script_extension(0x301A, 0x301A, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Ps       LEFT WHITE SQUARE BRACKET
unicode_script_extension(0x301B, 0x301B, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Pe       RIGHT WHITE SQUARE BRACKET
unicode_script_extension(0x30FB, 0x30FB, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Po       KATAKANA MIDDLE DOT
unicode_script_extension(0xFF61, 0xFF61, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Po       HALFWIDTH IDEOGRAPHIC FULL STOP
unicode_script_extension(0xFF62, 0xFF62, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Ps       HALFWIDTH LEFT CORNER BRACKET
unicode_script_extension(0xFF63, 0xFF63, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Pe       HALFWIDTH RIGHT CORNER BRACKET
unicode_script_extension(0xFF64, 0xFF65, ['Bopo', 'Hang', 'Hani', 'Hira', 'Kana', 'Yiii']).	% Po   [2] HALFWIDTH IDEOGRAPHIC COMMA..HALFWIDTH KATAKANA MIDDLE DOT

% Total code points: 26

% EOF
