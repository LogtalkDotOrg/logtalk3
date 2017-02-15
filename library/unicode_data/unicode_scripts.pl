%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: June 12, 2013
%
%  Original Unicode file header comments follow

/*
http://www.unicode.org/Public/UNIDATA/Scripts.txt

# Scripts-6.2.0.txt
# Date: 2012-06-04, 17:21:29 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2012 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

# ================================================

# Property:	Script

#  All code points not explicitly listed for Script
#  have the value Unknown (Zzzz).

# @missing: 0000..10FFFF; Unknown

% ================================================
*/

unicode_script_category(CodePoint, Script, Category) :-
	(	var(CodePoint) ->
		% generate CodePoint-script pairs
		unicode_script(CodePointStart, CodePointEnd, Script, Category, _, _),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_script(CodePoint, _, CodePointScript, CodePointCategory, _, _) ->
		Script = CodePointScript,
		Category = CodePointCategory
	;	% if the script name is known, go straight to it
		nonvar(Script) ->
		unicode_script(CodePointStart, CodePointEnd, Script, CodePointCategory, _, _),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Category = CodePointCategory
	;	% if the category name is known, go straight to it
		nonvar(Category) ->
		unicode_script(CodePointStart, CodePointEnd, CodePointScript, Category, _, _),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Script = CodePointScript
	;	% look for a code point range that includes the given code point
		unicode_script(CodePointStart, CodePointEnd, CodePointScript, CodePointCategory, _, _),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Script = CodePointScript,
		Category = CodePointCategory
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Script = 'Zzzz',
		Category = ''
	).

unicode_script(0x0000, 0x001F, 'Common', 'Cc', 32, '<control-0000>..<control-001F>').
unicode_script(0x0020, 0x0020, 'Common', 'Zs', 1, 'SPACE').
unicode_script(0x0021, 0x0023, 'Common', 'Po', 3, 'EXCLAMATION MARK..NUMBER SIGN').
unicode_script(0x0024, 0x0024, 'Common', 'Sc', 1, 'DOLLAR SIGN').
unicode_script(0x0025, 0x0027, 'Common', 'Po', 3, 'PERCENT SIGN..APOSTROPHE').
unicode_script(0x0028, 0x0028, 'Common', 'Ps', 1, 'LEFT PARENTHESIS').
unicode_script(0x0029, 0x0029, 'Common', 'Pe', 1, 'RIGHT PARENTHESIS').
unicode_script(0x002A, 0x002A, 'Common', 'Po', 1, 'ASTERISK').
unicode_script(0x002B, 0x002B, 'Common', 'Sm', 1, 'PLUS SIGN').
unicode_script(0x002C, 0x002C, 'Common', 'Po', 1, 'COMMA').
unicode_script(0x002D, 0x002D, 'Common', 'Pd', 1, 'HYPHEN-MINUS').
unicode_script(0x002E, 0x002F, 'Common', 'Po', 2, 'FULL STOP..SOLIDUS').
unicode_script(0x0030, 0x0039, 'Common', 'Nd', 10, 'DIGIT ZERO..DIGIT NINE').
unicode_script(0x003A, 0x003B, 'Common', 'Po', 2, 'COLON..SEMICOLON').
unicode_script(0x003C, 0x003E, 'Common', 'Sm', 3, 'LESS-THAN SIGN..GREATER-THAN SIGN').
unicode_script(0x003F, 0x0040, 'Common', 'Po', 2, 'QUESTION MARK..COMMERCIAL AT').
unicode_script(0x005B, 0x005B, 'Common', 'Ps', 1, 'LEFT SQUARE BRACKET').
unicode_script(0x005C, 0x005C, 'Common', 'Po', 1, 'REVERSE SOLIDUS').
unicode_script(0x005D, 0x005D, 'Common', 'Pe', 1, 'RIGHT SQUARE BRACKET').
unicode_script(0x005E, 0x005E, 'Common', 'Sk', 1, 'CIRCUMFLEX ACCENT').
unicode_script(0x005F, 0x005F, 'Common', 'Pc', 1, 'LOW LINE').
unicode_script(0x0060, 0x0060, 'Common', 'Sk', 1, 'GRAVE ACCENT').
unicode_script(0x007B, 0x007B, 'Common', 'Ps', 1, 'LEFT CURLY BRACKET').
unicode_script(0x007C, 0x007C, 'Common', 'Sm', 1, 'VERTICAL LINE').
unicode_script(0x007D, 0x007D, 'Common', 'Pe', 1, 'RIGHT CURLY BRACKET').
unicode_script(0x007E, 0x007E, 'Common', 'Sm', 1, 'TILDE').
unicode_script(0x007F, 0x009F, 'Common', 'Cc', 33, '<control-007F>..<control-009F>').
unicode_script(0x00A0, 0x00A0, 'Common', 'Zs', 1, 'NO-BREAK SPACE').
unicode_script(0x00A1, 0x00A1, 'Common', 'Po', 1, 'INVERTED EXCLAMATION MARK').
unicode_script(0x00A2, 0x00A5, 'Common', 'Sc', 4, 'CENT SIGN..YEN SIGN').
unicode_script(0x00A6, 0x00A6, 'Common', 'So', 1, 'BROKEN BAR').
unicode_script(0x00A7, 0x00A7, 'Common', 'Po', 1, 'SECTION SIGN').
unicode_script(0x00A8, 0x00A8, 'Common', 'Sk', 1, 'DIAERESIS').
unicode_script(0x00A9, 0x00A9, 'Common', 'So', 1, 'COPYRIGHT SIGN').
unicode_script(0x00AB, 0x00AB, 'Common', 'Pi', 1, 'LEFT-POINTING DOUBLE ANGLE QUOTATION MARK').
unicode_script(0x00AC, 0x00AC, 'Common', 'Sm', 1, 'NOT SIGN').
unicode_script(0x00AD, 0x00AD, 'Common', 'Cf', 1, 'SOFT HYPHEN').
unicode_script(0x00AE, 0x00AE, 'Common', 'So', 1, 'REGISTERED SIGN').
unicode_script(0x00AF, 0x00AF, 'Common', 'Sk', 1, 'MACRON').
unicode_script(0x00B0, 0x00B0, 'Common', 'So', 1, 'DEGREE SIGN').
unicode_script(0x00B1, 0x00B1, 'Common', 'Sm', 1, 'PLUS-MINUS SIGN').
unicode_script(0x00B2, 0x00B3, 'Common', 'No', 2, 'SUPERSCRIPT TWO..SUPERSCRIPT THREE').
unicode_script(0x00B4, 0x00B4, 'Common', 'Sk', 1, 'ACUTE ACCENT').
unicode_script(0x00B5, 0x00B5, 'Common', 'L&', 1, 'MICRO SIGN').
unicode_script(0x00B6, 0x00B7, 'Common', 'Po', 2, 'PILCROW SIGN..MIDDLE DOT').
unicode_script(0x00B8, 0x00B8, 'Common', 'Sk', 1, 'CEDILLA').
unicode_script(0x00B9, 0x00B9, 'Common', 'No', 1, 'SUPERSCRIPT ONE').
unicode_script(0x00BB, 0x00BB, 'Common', 'Pf', 1, 'RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK').
unicode_script(0x00BC, 0x00BE, 'Common', 'No', 3, 'VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS').
unicode_script(0x00BF, 0x00BF, 'Common', 'Po', 1, 'INVERTED QUESTION MARK').
unicode_script(0x00D7, 0x00D7, 'Common', 'Sm', 1, 'MULTIPLICATION SIGN').
unicode_script(0x00F7, 0x00F7, 'Common', 'Sm', 1, 'DIVISION SIGN').
unicode_script(0x02B9, 0x02C1, 'Common', 'Lm', 9, 'MODIFIER LETTER PRIME..MODIFIER LETTER REVERSED GLOTTAL STOP').
unicode_script(0x02C2, 0x02C5, 'Common', 'Sk', 4, 'MODIFIER LETTER LEFT ARROWHEAD..MODIFIER LETTER DOWN ARROWHEAD').
unicode_script(0x02C6, 0x02D1, 'Common', 'Lm', 12, 'MODIFIER LETTER CIRCUMFLEX ACCENT..MODIFIER LETTER HALF TRIANGULAR COLON').
unicode_script(0x02D2, 0x02DF, 'Common', 'Sk', 14, 'MODIFIER LETTER CENTRED RIGHT HALF RING..MODIFIER LETTER CROSS ACCENT').
unicode_script(0x02E5, 0x02E9, 'Common', 'Sk', 5, 'MODIFIER LETTER EXTRA-HIGH TONE BAR..MODIFIER LETTER EXTRA-LOW TONE BAR').
unicode_script(0x02EC, 0x02EC, 'Common', 'Lm', 1, 'MODIFIER LETTER VOICING').
unicode_script(0x02ED, 0x02ED, 'Common', 'Sk', 1, 'MODIFIER LETTER UNASPIRATED').
unicode_script(0x02EE, 0x02EE, 'Common', 'Lm', 1, 'MODIFIER LETTER DOUBLE APOSTROPHE').
unicode_script(0x02EF, 0x02FF, 'Common', 'Sk', 17, 'MODIFIER LETTER LOW DOWN ARROWHEAD..MODIFIER LETTER LOW LEFT ARROW').
unicode_script(0x0374, 0x0374, 'Common', 'Lm', 1, 'GREEK NUMERAL SIGN').
unicode_script(0x037E, 0x037E, 'Common', 'Po', 1, 'GREEK QUESTION MARK').
unicode_script(0x0385, 0x0385, 'Common', 'Sk', 1, 'GREEK DIALYTIKA TONOS').
unicode_script(0x0387, 0x0387, 'Common', 'Po', 1, 'GREEK ANO TELEIA').
unicode_script(0x0589, 0x0589, 'Common', 'Po', 1, 'ARMENIAN FULL STOP').
unicode_script(0x060C, 0x060C, 'Common', 'Po', 1, 'ARABIC COMMA').
unicode_script(0x061B, 0x061B, 'Common', 'Po', 1, 'ARABIC SEMICOLON').
unicode_script(0x061F, 0x061F, 'Common', 'Po', 1, 'ARABIC QUESTION MARK').
unicode_script(0x0640, 0x0640, 'Common', 'Lm', 1, 'ARABIC TATWEEL').
unicode_script(0x0660, 0x0669, 'Common', 'Nd', 10, 'ARABIC-INDIC DIGIT ZERO..ARABIC-INDIC DIGIT NINE').
unicode_script(0x06DD, 0x06DD, 'Common', 'Cf', 1, 'ARABIC END OF AYAH').
unicode_script(0x0964, 0x0965, 'Common', 'Po', 2, 'DEVANAGARI DANDA..DEVANAGARI DOUBLE DANDA').
unicode_script(0x0E3F, 0x0E3F, 'Common', 'Sc', 1, 'THAI CURRENCY SYMBOL BAHT').
unicode_script(0x0FD5, 0x0FD8, 'Common', 'So', 4, 'RIGHT-FACING SVASTI SIGN..LEFT-FACING SVASTI SIGN WITH DOTS').
unicode_script(0x10FB, 0x10FB, 'Common', 'Po', 1, 'GEORGIAN PARAGRAPH SEPARATOR').
unicode_script(0x16EB, 0x16ED, 'Common', 'Po', 3, 'RUNIC SINGLE PUNCTUATION..RUNIC CROSS PUNCTUATION').
unicode_script(0x1735, 0x1736, 'Common', 'Po', 2, 'PHILIPPINE SINGLE PUNCTUATION..PHILIPPINE DOUBLE PUNCTUATION').
unicode_script(0x1802, 0x1803, 'Common', 'Po', 2, 'MONGOLIAN COMMA..MONGOLIAN FULL STOP').
unicode_script(0x1805, 0x1805, 'Common', 'Po', 1, 'MONGOLIAN FOUR DOTS').
unicode_script(0x1CD3, 0x1CD3, 'Common', 'Po', 1, 'VEDIC SIGN NIHSHVASA').
unicode_script(0x1CE1, 0x1CE1, 'Common', 'Mc', 1, 'VEDIC TONE ATHARVAVEDIC INDEPENDENT SVARITA').
unicode_script(0x1CE9, 0x1CEC, 'Common', 'Lo', 4, 'VEDIC SIGN ANUSVARA ANTARGOMUKHA..VEDIC SIGN ANUSVARA VAMAGOMUKHA WITH TAIL').
unicode_script(0x1CEE, 0x1CF1, 'Common', 'Lo', 4, 'VEDIC SIGN HEXIFORM LONG ANUSVARA..VEDIC SIGN ANUSVARA UBHAYATO MUKHA').
unicode_script(0x1CF2, 0x1CF3, 'Common', 'Mc', 2, 'VEDIC SIGN ARDHAVISARGA..VEDIC SIGN ROTATED ARDHAVISARGA').
unicode_script(0x1CF5, 0x1CF6, 'Common', 'Lo', 2, 'VEDIC SIGN JIHVAMULIYA..VEDIC SIGN UPADHMANIYA').
unicode_script(0x2000, 0x200A, 'Common', 'Zs', 11, 'EN QUAD..HAIR SPACE').
unicode_script(0x200B, 0x200B, 'Common', 'Cf', 1, 'ZERO WIDTH SPACE').
unicode_script(0x200E, 0x200F, 'Common', 'Cf', 2, 'LEFT-TO-RIGHT MARK..RIGHT-TO-LEFT MARK').
unicode_script(0x2010, 0x2015, 'Common', 'Pd', 6, 'HYPHEN..HORIZONTAL BAR').
unicode_script(0x2016, 0x2017, 'Common', 'Po', 2, 'DOUBLE VERTICAL LINE..DOUBLE LOW LINE').
unicode_script(0x2018, 0x2018, 'Common', 'Pi', 1, 'LEFT SINGLE QUOTATION MARK').
unicode_script(0x2019, 0x2019, 'Common', 'Pf', 1, 'RIGHT SINGLE QUOTATION MARK').
unicode_script(0x201A, 0x201A, 'Common', 'Ps', 1, 'SINGLE LOW-9 QUOTATION MARK').
unicode_script(0x201B, 0x201C, 'Common', 'Pi', 2, 'SINGLE HIGH-REVERSED-9 QUOTATION MARK..LEFT DOUBLE QUOTATION MARK').
unicode_script(0x201D, 0x201D, 'Common', 'Pf', 1, 'RIGHT DOUBLE QUOTATION MARK').
unicode_script(0x201E, 0x201E, 'Common', 'Ps', 1, 'DOUBLE LOW-9 QUOTATION MARK').
unicode_script(0x201F, 0x201F, 'Common', 'Pi', 1, 'DOUBLE HIGH-REVERSED-9 QUOTATION MARK').
unicode_script(0x2020, 0x2027, 'Common', 'Po', 8, 'DAGGER..HYPHENATION POINT').
unicode_script(0x2028, 0x2028, 'Common', 'Zl', 1, 'LINE SEPARATOR').
unicode_script(0x2029, 0x2029, 'Common', 'Zp', 1, 'PARAGRAPH SEPARATOR').
unicode_script(0x202A, 0x202E, 'Common', 'Cf', 5, 'LEFT-TO-RIGHT EMBEDDING..RIGHT-TO-LEFT OVERRIDE').
unicode_script(0x202F, 0x202F, 'Common', 'Zs', 1, 'NARROW NO-BREAK SPACE').
unicode_script(0x2030, 0x2038, 'Common', 'Po', 9, 'PER MILLE SIGN..CARET').
unicode_script(0x2039, 0x2039, 'Common', 'Pi', 1, 'SINGLE LEFT-POINTING ANGLE QUOTATION MARK').
unicode_script(0x203A, 0x203A, 'Common', 'Pf', 1, 'SINGLE RIGHT-POINTING ANGLE QUOTATION MARK').
unicode_script(0x203B, 0x203E, 'Common', 'Po', 4, 'REFERENCE MARK..OVERLINE').
unicode_script(0x203F, 0x2040, 'Common', 'Pc', 2, 'UNDERTIE..CHARACTER TIE').
unicode_script(0x2041, 0x2043, 'Common', 'Po', 3, 'CARET INSERTION POINT..HYPHEN BULLET').
unicode_script(0x2044, 0x2044, 'Common', 'Sm', 1, 'FRACTION SLASH').
unicode_script(0x2045, 0x2045, 'Common', 'Ps', 1, 'LEFT SQUARE BRACKET WITH QUILL').
unicode_script(0x2046, 0x2046, 'Common', 'Pe', 1, 'RIGHT SQUARE BRACKET WITH QUILL').
unicode_script(0x2047, 0x2051, 'Common', 'Po', 11, 'DOUBLE QUESTION MARK..TWO ASTERISKS ALIGNED VERTICALLY').
unicode_script(0x2052, 0x2052, 'Common', 'Sm', 1, 'COMMERCIAL MINUS SIGN').
unicode_script(0x2053, 0x2053, 'Common', 'Po', 1, 'SWUNG DASH').
unicode_script(0x2054, 0x2054, 'Common', 'Pc', 1, 'INVERTED UNDERTIE').
unicode_script(0x2055, 0x205E, 'Common', 'Po', 10, 'FLOWER PUNCTUATION MARK..VERTICAL FOUR DOTS').
unicode_script(0x205F, 0x205F, 'Common', 'Zs', 1, 'MEDIUM MATHEMATICAL SPACE').
unicode_script(0x2060, 0x2064, 'Common', 'Cf', 5, 'WORD JOINER..INVISIBLE PLUS').
unicode_script(0x206A, 0x206F, 'Common', 'Cf', 6, 'INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES').
unicode_script(0x2070, 0x2070, 'Common', 'No', 1, 'SUPERSCRIPT ZERO').
unicode_script(0x2074, 0x2079, 'Common', 'No', 6, 'SUPERSCRIPT FOUR..SUPERSCRIPT NINE').
unicode_script(0x207A, 0x207C, 'Common', 'Sm', 3, 'SUPERSCRIPT PLUS SIGN..SUPERSCRIPT EQUALS SIGN').
unicode_script(0x207D, 0x207D, 'Common', 'Ps', 1, 'SUPERSCRIPT LEFT PARENTHESIS').
unicode_script(0x207E, 0x207E, 'Common', 'Pe', 1, 'SUPERSCRIPT RIGHT PARENTHESIS').
unicode_script(0x2080, 0x2089, 'Common', 'No', 10, 'SUBSCRIPT ZERO..SUBSCRIPT NINE').
unicode_script(0x208A, 0x208C, 'Common', 'Sm', 3, 'SUBSCRIPT PLUS SIGN..SUBSCRIPT EQUALS SIGN').
unicode_script(0x208D, 0x208D, 'Common', 'Ps', 1, 'SUBSCRIPT LEFT PARENTHESIS').
unicode_script(0x208E, 0x208E, 'Common', 'Pe', 1, 'SUBSCRIPT RIGHT PARENTHESIS').
unicode_script(0x20A0, 0x20BA, 'Common', 'Sc', 27, 'EURO-CURRENCY SIGN..TURKISH LIRA SIGN').
unicode_script(0x2100, 0x2101, 'Common', 'So', 2, 'ACCOUNT OF..ADDRESSED TO THE SUBJECT').
unicode_script(0x2102, 0x2102, 'Common', 'L&', 1, 'DOUBLE-STRUCK CAPITAL C').
unicode_script(0x2103, 0x2106, 'Common', 'So', 4, 'DEGREE CELSIUS..CADA UNA').
unicode_script(0x2107, 0x2107, 'Common', 'L&', 1, 'EULER CONSTANT').
unicode_script(0x2108, 0x2109, 'Common', 'So', 2, 'SCRUPLE..DEGREE FAHRENHEIT').
unicode_script(0x210A, 0x2113, 'Common', 'L&', 10, 'SCRIPT SMALL G..SCRIPT SMALL L').
unicode_script(0x2114, 0x2114, 'Common', 'So', 1, 'L B BAR SYMBOL').
unicode_script(0x2115, 0x2115, 'Common', 'L&', 1, 'DOUBLE-STRUCK CAPITAL N').
unicode_script(0x2116, 0x2117, 'Common', 'So', 2, 'NUMERO SIGN..SOUND RECORDING COPYRIGHT').
unicode_script(0x2118, 0x2118, 'Common', 'Sm', 1, 'SCRIPT CAPITAL P').
unicode_script(0x2119, 0x211D, 'Common', 'L&', 5, 'DOUBLE-STRUCK CAPITAL P..DOUBLE-STRUCK CAPITAL R').
unicode_script(0x211E, 0x2123, 'Common', 'So', 6, 'PRESCRIPTION TAKE..VERSICLE').
unicode_script(0x2124, 0x2124, 'Common', 'L&', 1, 'DOUBLE-STRUCK CAPITAL Z').
unicode_script(0x2125, 0x2125, 'Common', 'So', 1, 'OUNCE SIGN').
unicode_script(0x2127, 0x2127, 'Common', 'So', 1, 'INVERTED OHM SIGN').
unicode_script(0x2128, 0x2128, 'Common', 'L&', 1, 'BLACK-LETTER CAPITAL Z').
unicode_script(0x2129, 0x2129, 'Common', 'So', 1, 'TURNED GREEK SMALL LETTER IOTA').
unicode_script(0x212C, 0x212D, 'Common', 'L&', 2, 'SCRIPT CAPITAL B..BLACK-LETTER CAPITAL C').
unicode_script(0x212E, 0x212E, 'Common', 'So', 1, 'ESTIMATED SYMBOL').
unicode_script(0x212F, 0x2131, 'Common', 'L&', 3, 'SCRIPT SMALL E..SCRIPT CAPITAL F').
unicode_script(0x2133, 0x2134, 'Common', 'L&', 2, 'SCRIPT CAPITAL M..SCRIPT SMALL O').
unicode_script(0x2135, 0x2138, 'Common', 'Lo', 4, 'ALEF SYMBOL..DALET SYMBOL').
unicode_script(0x2139, 0x2139, 'Common', 'L&', 1, 'INFORMATION SOURCE').
unicode_script(0x213A, 0x213B, 'Common', 'So', 2, 'ROTATED CAPITAL Q..FACSIMILE SIGN').
unicode_script(0x213C, 0x213F, 'Common', 'L&', 4, 'DOUBLE-STRUCK SMALL PI..DOUBLE-STRUCK CAPITAL PI').
unicode_script(0x2140, 0x2144, 'Common', 'Sm', 5, 'DOUBLE-STRUCK N-ARY SUMMATION..TURNED SANS-SERIF CAPITAL Y').
unicode_script(0x2145, 0x2149, 'Common', 'L&', 5, 'DOUBLE-STRUCK ITALIC CAPITAL D..DOUBLE-STRUCK ITALIC SMALL J').
unicode_script(0x214A, 0x214A, 'Common', 'So', 1, 'PROPERTY LINE').
unicode_script(0x214B, 0x214B, 'Common', 'Sm', 1, 'TURNED AMPERSAND').
unicode_script(0x214C, 0x214D, 'Common', 'So', 2, 'PER SIGN..AKTIESELSKAB').
unicode_script(0x214F, 0x214F, 'Common', 'So', 1, 'SYMBOL FOR SAMARITAN SOURCE').
unicode_script(0x2150, 0x215F, 'Common', 'No', 16, 'VULGAR FRACTION ONE SEVENTH..FRACTION NUMERATOR ONE').
unicode_script(0x2189, 0x2189, 'Common', 'No', 1, 'VULGAR FRACTION ZERO THIRDS').
unicode_script(0x2190, 0x2194, 'Common', 'Sm', 5, 'LEFTWARDS ARROW..LEFT RIGHT ARROW').
unicode_script(0x2195, 0x2199, 'Common', 'So', 5, 'UP DOWN ARROW..SOUTH WEST ARROW').
unicode_script(0x219A, 0x219B, 'Common', 'Sm', 2, 'LEFTWARDS ARROW WITH STROKE..RIGHTWARDS ARROW WITH STROKE').
unicode_script(0x219C, 0x219F, 'Common', 'So', 4, 'LEFTWARDS WAVE ARROW..UPWARDS TWO HEADED ARROW').
unicode_script(0x21A0, 0x21A0, 'Common', 'Sm', 1, 'RIGHTWARDS TWO HEADED ARROW').
unicode_script(0x21A1, 0x21A2, 'Common', 'So', 2, 'DOWNWARDS TWO HEADED ARROW..LEFTWARDS ARROW WITH TAIL').
unicode_script(0x21A3, 0x21A3, 'Common', 'Sm', 1, 'RIGHTWARDS ARROW WITH TAIL').
unicode_script(0x21A4, 0x21A5, 'Common', 'So', 2, 'LEFTWARDS ARROW FROM BAR..UPWARDS ARROW FROM BAR').
unicode_script(0x21A6, 0x21A6, 'Common', 'Sm', 1, 'RIGHTWARDS ARROW FROM BAR').
unicode_script(0x21A7, 0x21AD, 'Common', 'So', 7, 'DOWNWARDS ARROW FROM BAR..LEFT RIGHT WAVE ARROW').
unicode_script(0x21AE, 0x21AE, 'Common', 'Sm', 1, 'LEFT RIGHT ARROW WITH STROKE').
unicode_script(0x21AF, 0x21CD, 'Common', 'So', 31, 'DOWNWARDS ZIGZAG ARROW..LEFTWARDS DOUBLE ARROW WITH STROKE').
unicode_script(0x21CE, 0x21CF, 'Common', 'Sm', 2, 'LEFT RIGHT DOUBLE ARROW WITH STROKE..RIGHTWARDS DOUBLE ARROW WITH STROKE').
unicode_script(0x21D0, 0x21D1, 'Common', 'So', 2, 'LEFTWARDS DOUBLE ARROW..UPWARDS DOUBLE ARROW').
unicode_script(0x21D2, 0x21D2, 'Common', 'Sm', 1, 'RIGHTWARDS DOUBLE ARROW').
unicode_script(0x21D3, 0x21D3, 'Common', 'So', 1, 'DOWNWARDS DOUBLE ARROW').
unicode_script(0x21D4, 0x21D4, 'Common', 'Sm', 1, 'LEFT RIGHT DOUBLE ARROW').
unicode_script(0x21D5, 0x21F3, 'Common', 'So', 31, 'UP DOWN DOUBLE ARROW..UP DOWN WHITE ARROW').
unicode_script(0x21F4, 0x22FF, 'Common', 'Sm', 268, 'RIGHT ARROW WITH SMALL CIRCLE..Z NOTATION BAG MEMBERSHIP').
unicode_script(0x2300, 0x2307, 'Common', 'So', 8, 'DIAMETER SIGN..WAVY LINE').
unicode_script(0x2308, 0x230B, 'Common', 'Sm', 4, 'LEFT CEILING..RIGHT FLOOR').
unicode_script(0x230C, 0x231F, 'Common', 'So', 20, 'BOTTOM RIGHT CROP..BOTTOM RIGHT CORNER').
unicode_script(0x2320, 0x2321, 'Common', 'Sm', 2, 'TOP HALF INTEGRAL..BOTTOM HALF INTEGRAL').
unicode_script(0x2322, 0x2328, 'Common', 'So', 7, 'FROWN..KEYBOARD').
unicode_script(0x2329, 0x2329, 'Common', 'Ps', 1, 'LEFT-POINTING ANGLE BRACKET').
unicode_script(0x232A, 0x232A, 'Common', 'Pe', 1, 'RIGHT-POINTING ANGLE BRACKET').
unicode_script(0x232B, 0x237B, 'Common', 'So', 81, 'ERASE TO THE LEFT..NOT CHECK MARK').
unicode_script(0x237C, 0x237C, 'Common', 'Sm', 1, 'RIGHT ANGLE WITH DOWNWARDS ZIGZAG ARROW').
unicode_script(0x237D, 0x239A, 'Common', 'So', 30, 'SHOULDERED OPEN BOX..CLEAR SCREEN SYMBOL').
unicode_script(0x239B, 0x23B3, 'Common', 'Sm', 25, 'LEFT PARENTHESIS UPPER HOOK..SUMMATION BOTTOM').
unicode_script(0x23B4, 0x23DB, 'Common', 'So', 40, 'TOP SQUARE BRACKET..FUSE').
unicode_script(0x23DC, 0x23E1, 'Common', 'Sm', 6, 'TOP PARENTHESIS..BOTTOM TORTOISE SHELL BRACKET').
unicode_script(0x23E2, 0x23F3, 'Common', 'So', 18, 'WHITE TRAPEZIUM..HOURGLASS WITH FLOWING SAND').
unicode_script(0x2400, 0x2426, 'Common', 'So', 39, 'SYMBOL FOR NULL..SYMBOL FOR SUBSTITUTE FORM TWO').
unicode_script(0x2440, 0x244A, 'Common', 'So', 11, 'OCR HOOK..OCR DOUBLE BACKSLASH').
unicode_script(0x2460, 0x249B, 'Common', 'No', 60, 'CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP').
unicode_script(0x249C, 0x24E9, 'Common', 'So', 78, 'PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z').
unicode_script(0x24EA, 0x24FF, 'Common', 'No', 22, 'CIRCLED DIGIT ZERO..NEGATIVE CIRCLED DIGIT ZERO').
unicode_script(0x2500, 0x25B6, 'Common', 'So', 183, 'BOX DRAWINGS LIGHT HORIZONTAL..BLACK RIGHT-POINTING TRIANGLE').
unicode_script(0x25B7, 0x25B7, 'Common', 'Sm', 1, 'WHITE RIGHT-POINTING TRIANGLE').
unicode_script(0x25B8, 0x25C0, 'Common', 'So', 9, 'BLACK RIGHT-POINTING SMALL TRIANGLE..BLACK LEFT-POINTING TRIANGLE').
unicode_script(0x25C1, 0x25C1, 'Common', 'Sm', 1, 'WHITE LEFT-POINTING TRIANGLE').
unicode_script(0x25C2, 0x25F7, 'Common', 'So', 54, 'BLACK LEFT-POINTING SMALL TRIANGLE..WHITE CIRCLE WITH UPPER RIGHT QUADRANT').
unicode_script(0x25F8, 0x25FF, 'Common', 'Sm', 8, 'UPPER LEFT TRIANGLE..LOWER RIGHT TRIANGLE').
unicode_script(0x2600, 0x266E, 'Common', 'So', 111, 'BLACK SUN WITH RAYS..MUSIC NATURAL SIGN').
unicode_script(0x266F, 0x266F, 'Common', 'Sm', 1, 'MUSIC SHARP SIGN').
unicode_script(0x2670, 0x26FF, 'Common', 'So', 144, 'WEST SYRIAC CROSS..WHITE FLAG WITH HORIZONTAL MIDDLE BLACK STRIPE').
unicode_script(0x2701, 0x2767, 'Common', 'So', 103, 'UPPER BLADE SCISSORS..ROTATED FLORAL HEART BULLET').
unicode_script(0x2768, 0x2768, 'Common', 'Ps', 1, 'MEDIUM LEFT PARENTHESIS ORNAMENT').
unicode_script(0x2769, 0x2769, 'Common', 'Pe', 1, 'MEDIUM RIGHT PARENTHESIS ORNAMENT').
unicode_script(0x276A, 0x276A, 'Common', 'Ps', 1, 'MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT').
unicode_script(0x276B, 0x276B, 'Common', 'Pe', 1, 'MEDIUM FLATTENED RIGHT PARENTHESIS ORNAMENT').
unicode_script(0x276C, 0x276C, 'Common', 'Ps', 1, 'MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT').
unicode_script(0x276D, 0x276D, 'Common', 'Pe', 1, 'MEDIUM RIGHT-POINTING ANGLE BRACKET ORNAMENT').
unicode_script(0x276E, 0x276E, 'Common', 'Ps', 1, 'HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT').
unicode_script(0x276F, 0x276F, 'Common', 'Pe', 1, 'HEAVY RIGHT-POINTING ANGLE QUOTATION MARK ORNAMENT').
unicode_script(0x2770, 0x2770, 'Common', 'Ps', 1, 'HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT').
unicode_script(0x2771, 0x2771, 'Common', 'Pe', 1, 'HEAVY RIGHT-POINTING ANGLE BRACKET ORNAMENT').
unicode_script(0x2772, 0x2772, 'Common', 'Ps', 1, 'LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT').
unicode_script(0x2773, 0x2773, 'Common', 'Pe', 1, 'LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT').
unicode_script(0x2774, 0x2774, 'Common', 'Ps', 1, 'MEDIUM LEFT CURLY BRACKET ORNAMENT').
unicode_script(0x2775, 0x2775, 'Common', 'Pe', 1, 'MEDIUM RIGHT CURLY BRACKET ORNAMENT').
unicode_script(0x2776, 0x2793, 'Common', 'No', 30, 'DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN').
unicode_script(0x2794, 0x27BF, 'Common', 'So', 44, 'HEAVY WIDE-HEADED RIGHTWARDS ARROW..DOUBLE CURLY LOOP').
unicode_script(0x27C0, 0x27C4, 'Common', 'Sm', 5, 'THREE DIMENSIONAL ANGLE..OPEN SUPERSET').
unicode_script(0x27C5, 0x27C5, 'Common', 'Ps', 1, 'LEFT S-SHAPED BAG DELIMITER').
unicode_script(0x27C6, 0x27C6, 'Common', 'Pe', 1, 'RIGHT S-SHAPED BAG DELIMITER').
unicode_script(0x27C7, 0x27E5, 'Common', 'Sm', 31, 'OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK').
unicode_script(0x27E6, 0x27E6, 'Common', 'Ps', 1, 'MATHEMATICAL LEFT WHITE SQUARE BRACKET').
unicode_script(0x27E7, 0x27E7, 'Common', 'Pe', 1, 'MATHEMATICAL RIGHT WHITE SQUARE BRACKET').
unicode_script(0x27E8, 0x27E8, 'Common', 'Ps', 1, 'MATHEMATICAL LEFT ANGLE BRACKET').
unicode_script(0x27E9, 0x27E9, 'Common', 'Pe', 1, 'MATHEMATICAL RIGHT ANGLE BRACKET').
unicode_script(0x27EA, 0x27EA, 'Common', 'Ps', 1, 'MATHEMATICAL LEFT DOUBLE ANGLE BRACKET').
unicode_script(0x27EB, 0x27EB, 'Common', 'Pe', 1, 'MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET').
unicode_script(0x27EC, 0x27EC, 'Common', 'Ps', 1, 'MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET').
unicode_script(0x27ED, 0x27ED, 'Common', 'Pe', 1, 'MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET').
unicode_script(0x27EE, 0x27EE, 'Common', 'Ps', 1, 'MATHEMATICAL LEFT FLATTENED PARENTHESIS').
unicode_script(0x27EF, 0x27EF, 'Common', 'Pe', 1, 'MATHEMATICAL RIGHT FLATTENED PARENTHESIS').
unicode_script(0x27F0, 0x27FF, 'Common', 'Sm', 16, 'UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW').
unicode_script(0x2900, 0x2982, 'Common', 'Sm', 131, 'RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE..Z NOTATION TYPE COLON').
unicode_script(0x2983, 0x2983, 'Common', 'Ps', 1, 'LEFT WHITE CURLY BRACKET').
unicode_script(0x2984, 0x2984, 'Common', 'Pe', 1, 'RIGHT WHITE CURLY BRACKET').
unicode_script(0x2985, 0x2985, 'Common', 'Ps', 1, 'LEFT WHITE PARENTHESIS').
unicode_script(0x2986, 0x2986, 'Common', 'Pe', 1, 'RIGHT WHITE PARENTHESIS').
unicode_script(0x2987, 0x2987, 'Common', 'Ps', 1, 'Z NOTATION LEFT IMAGE BRACKET').
unicode_script(0x2988, 0x2988, 'Common', 'Pe', 1, 'Z NOTATION RIGHT IMAGE BRACKET').
unicode_script(0x2989, 0x2989, 'Common', 'Ps', 1, 'Z NOTATION LEFT BINDING BRACKET').
unicode_script(0x298A, 0x298A, 'Common', 'Pe', 1, 'Z NOTATION RIGHT BINDING BRACKET').
unicode_script(0x298B, 0x298B, 'Common', 'Ps', 1, 'LEFT SQUARE BRACKET WITH UNDERBAR').
unicode_script(0x298C, 0x298C, 'Common', 'Pe', 1, 'RIGHT SQUARE BRACKET WITH UNDERBAR').
unicode_script(0x298D, 0x298D, 'Common', 'Ps', 1, 'LEFT SQUARE BRACKET WITH TICK IN TOP CORNER').
unicode_script(0x298E, 0x298E, 'Common', 'Pe', 1, 'RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER').
unicode_script(0x298F, 0x298F, 'Common', 'Ps', 1, 'LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER').
unicode_script(0x2990, 0x2990, 'Common', 'Pe', 1, 'RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER').
unicode_script(0x2991, 0x2991, 'Common', 'Ps', 1, 'LEFT ANGLE BRACKET WITH DOT').
unicode_script(0x2992, 0x2992, 'Common', 'Pe', 1, 'RIGHT ANGLE BRACKET WITH DOT').
unicode_script(0x2993, 0x2993, 'Common', 'Ps', 1, 'LEFT ARC LESS-THAN BRACKET').
unicode_script(0x2994, 0x2994, 'Common', 'Pe', 1, 'RIGHT ARC GREATER-THAN BRACKET').
unicode_script(0x2995, 0x2995, 'Common', 'Ps', 1, 'DOUBLE LEFT ARC GREATER-THAN BRACKET').
unicode_script(0x2996, 0x2996, 'Common', 'Pe', 1, 'DOUBLE RIGHT ARC LESS-THAN BRACKET').
unicode_script(0x2997, 0x2997, 'Common', 'Ps', 1, 'LEFT BLACK TORTOISE SHELL BRACKET').
unicode_script(0x2998, 0x2998, 'Common', 'Pe', 1, 'RIGHT BLACK TORTOISE SHELL BRACKET').
unicode_script(0x2999, 0x29D7, 'Common', 'Sm', 63, 'DOTTED FENCE..BLACK HOURGLASS').
unicode_script(0x29D8, 0x29D8, 'Common', 'Ps', 1, 'LEFT WIGGLY FENCE').
unicode_script(0x29D9, 0x29D9, 'Common', 'Pe', 1, 'RIGHT WIGGLY FENCE').
unicode_script(0x29DA, 0x29DA, 'Common', 'Ps', 1, 'LEFT DOUBLE WIGGLY FENCE').
unicode_script(0x29DB, 0x29DB, 'Common', 'Pe', 1, 'RIGHT DOUBLE WIGGLY FENCE').
unicode_script(0x29DC, 0x29FB, 'Common', 'Sm', 32, 'INCOMPLETE INFINITY..TRIPLE PLUS').
unicode_script(0x29FC, 0x29FC, 'Common', 'Ps', 1, 'LEFT-POINTING CURVED ANGLE BRACKET').
unicode_script(0x29FD, 0x29FD, 'Common', 'Pe', 1, 'RIGHT-POINTING CURVED ANGLE BRACKET').
unicode_script(0x29FE, 0x2AFF, 'Common', 'Sm', 258, 'TINY..N-ARY WHITE VERTICAL BAR').
unicode_script(0x2B00, 0x2B2F, 'Common', 'So', 48, 'NORTH EAST WHITE ARROW..WHITE VERTICAL ELLIPSE').
unicode_script(0x2B30, 0x2B44, 'Common', 'Sm', 21, 'LEFT ARROW WITH SMALL CIRCLE..RIGHTWARDS ARROW THROUGH SUPERSET').
unicode_script(0x2B45, 0x2B46, 'Common', 'So', 2, 'LEFTWARDS QUADRUPLE ARROW..RIGHTWARDS QUADRUPLE ARROW').
unicode_script(0x2B47, 0x2B4C, 'Common', 'Sm', 6, 'REVERSE TILDE OPERATOR ABOVE RIGHTWARDS ARROW..RIGHTWARDS ARROW ABOVE REVERSE TILDE OPERATOR').
unicode_script(0x2B50, 0x2B59, 'Common', 'So', 10, 'WHITE MEDIUM STAR..HEAVY CIRCLED SALTIRE').
unicode_script(0x2E00, 0x2E01, 'Common', 'Po', 2, 'RIGHT ANGLE SUBSTITUTION MARKER..RIGHT ANGLE DOTTED SUBSTITUTION MARKER').
unicode_script(0x2E02, 0x2E02, 'Common', 'Pi', 1, 'LEFT SUBSTITUTION BRACKET').
unicode_script(0x2E03, 0x2E03, 'Common', 'Pf', 1, 'RIGHT SUBSTITUTION BRACKET').
unicode_script(0x2E04, 0x2E04, 'Common', 'Pi', 1, 'LEFT DOTTED SUBSTITUTION BRACKET').
unicode_script(0x2E05, 0x2E05, 'Common', 'Pf', 1, 'RIGHT DOTTED SUBSTITUTION BRACKET').
unicode_script(0x2E06, 0x2E08, 'Common', 'Po', 3, 'RAISED INTERPOLATION MARKER..DOTTED TRANSPOSITION MARKER').
unicode_script(0x2E09, 0x2E09, 'Common', 'Pi', 1, 'LEFT TRANSPOSITION BRACKET').
unicode_script(0x2E0A, 0x2E0A, 'Common', 'Pf', 1, 'RIGHT TRANSPOSITION BRACKET').
unicode_script(0x2E0B, 0x2E0B, 'Common', 'Po', 1, 'RAISED SQUARE').
unicode_script(0x2E0C, 0x2E0C, 'Common', 'Pi', 1, 'LEFT RAISED OMISSION BRACKET').
unicode_script(0x2E0D, 0x2E0D, 'Common', 'Pf', 1, 'RIGHT RAISED OMISSION BRACKET').
unicode_script(0x2E0E, 0x2E16, 'Common', 'Po', 9, 'EDITORIAL CORONIS..DOTTED RIGHT-POINTING ANGLE').
unicode_script(0x2E17, 0x2E17, 'Common', 'Pd', 1, 'DOUBLE OBLIQUE HYPHEN').
unicode_script(0x2E18, 0x2E19, 'Common', 'Po', 2, 'INVERTED INTERROBANG..PALM BRANCH').
unicode_script(0x2E1A, 0x2E1A, 'Common', 'Pd', 1, 'HYPHEN WITH DIAERESIS').
unicode_script(0x2E1B, 0x2E1B, 'Common', 'Po', 1, 'TILDE WITH RING ABOVE').
unicode_script(0x2E1C, 0x2E1C, 'Common', 'Pi', 1, 'LEFT LOW PARAPHRASE BRACKET').
unicode_script(0x2E1D, 0x2E1D, 'Common', 'Pf', 1, 'RIGHT LOW PARAPHRASE BRACKET').
unicode_script(0x2E1E, 0x2E1F, 'Common', 'Po', 2, 'TILDE WITH DOT ABOVE..TILDE WITH DOT BELOW').
unicode_script(0x2E20, 0x2E20, 'Common', 'Pi', 1, 'LEFT VERTICAL BAR WITH QUILL').
unicode_script(0x2E21, 0x2E21, 'Common', 'Pf', 1, 'RIGHT VERTICAL BAR WITH QUILL').
unicode_script(0x2E22, 0x2E22, 'Common', 'Ps', 1, 'TOP LEFT HALF BRACKET').
unicode_script(0x2E23, 0x2E23, 'Common', 'Pe', 1, 'TOP RIGHT HALF BRACKET').
unicode_script(0x2E24, 0x2E24, 'Common', 'Ps', 1, 'BOTTOM LEFT HALF BRACKET').
unicode_script(0x2E25, 0x2E25, 'Common', 'Pe', 1, 'BOTTOM RIGHT HALF BRACKET').
unicode_script(0x2E26, 0x2E26, 'Common', 'Ps', 1, 'LEFT SIDEWAYS U BRACKET').
unicode_script(0x2E27, 0x2E27, 'Common', 'Pe', 1, 'RIGHT SIDEWAYS U BRACKET').
unicode_script(0x2E28, 0x2E28, 'Common', 'Ps', 1, 'LEFT DOUBLE PARENTHESIS').
unicode_script(0x2E29, 0x2E29, 'Common', 'Pe', 1, 'RIGHT DOUBLE PARENTHESIS').
unicode_script(0x2E2A, 0x2E2E, 'Common', 'Po', 5, 'TWO DOTS OVER ONE DOT PUNCTUATION..REVERSED QUESTION MARK').
unicode_script(0x2E2F, 0x2E2F, 'Common', 'Lm', 1, 'VERTICAL TILDE').
unicode_script(0x2E30, 0x2E39, 'Common', 'Po', 10, 'RING POINT..TOP HALF SECTION SIGN').
unicode_script(0x2E3A, 0x2E3B, 'Common', 'Pd', 2, 'TWO-EM DASH..THREE-EM DASH').
unicode_script(0x2FF0, 0x2FFB, 'Common', 'So', 12, 'IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT..IDEOGRAPHIC DESCRIPTION CHARACTER OVERLAID').
unicode_script(0x3000, 0x3000, 'Common', 'Zs', 1, 'IDEOGRAPHIC SPACE').
unicode_script(0x3001, 0x3003, 'Common', 'Po', 3, 'IDEOGRAPHIC COMMA..DITTO MARK').
unicode_script(0x3004, 0x3004, 'Common', 'So', 1, 'JAPANESE INDUSTRIAL STANDARD SYMBOL').
unicode_script(0x3006, 0x3006, 'Common', 'Lo', 1, ' IDEOGRAPHIC CLOSING MARK').
unicode_script(0x3008, 0x3008, 'Common', 'Ps', 1, 'LEFT ANGLE BRACKET').
unicode_script(0x3009, 0x3009, 'Common', 'Pe', 1, 'RIGHT ANGLE BRACKET').
unicode_script(0x300A, 0x300A, 'Common', 'Ps', 1, 'LEFT DOUBLE ANGLE BRACKET').
unicode_script(0x300B, 0x300B, 'Common', 'Pe', 1, 'RIGHT DOUBLE ANGLE BRACKET').
unicode_script(0x300C, 0x300C, 'Common', 'Ps', 1, 'LEFT CORNER BRACKET').
unicode_script(0x300D, 0x300D, 'Common', 'Pe', 1, 'RIGHT CORNER BRACKET').
unicode_script(0x300E, 0x300E, 'Common', 'Ps', 1, 'LEFT WHITE CORNER BRACKET').
unicode_script(0x300F, 0x300F, 'Common', 'Pe', 1, 'RIGHT WHITE CORNER BRACKET').
unicode_script(0x3010, 0x3010, 'Common', 'Ps', 1, 'LEFT BLACK LENTICULAR BRACKET').
unicode_script(0x3011, 0x3011, 'Common', 'Pe', 1, 'RIGHT BLACK LENTICULAR BRACKET').
unicode_script(0x3012, 0x3013, 'Common', 'So', 2, 'POSTAL MARK..GETA MARK').
unicode_script(0x3014, 0x3014, 'Common', 'Ps', 1, 'LEFT TORTOISE SHELL BRACKET').
unicode_script(0x3015, 0x3015, 'Common', 'Pe', 1, 'RIGHT TORTOISE SHELL BRACKET').
unicode_script(0x3016, 0x3016, 'Common', 'Ps', 1, 'LEFT WHITE LENTICULAR BRACKET').
unicode_script(0x3017, 0x3017, 'Common', 'Pe', 1, 'RIGHT WHITE LENTICULAR BRACKET').
unicode_script(0x3018, 0x3018, 'Common', 'Ps', 1, 'LEFT WHITE TORTOISE SHELL BRACKET').
unicode_script(0x3019, 0x3019, 'Common', 'Pe', 1, 'RIGHT WHITE TORTOISE SHELL BRACKET').
unicode_script(0x301A, 0x301A, 'Common', 'Ps', 1, 'LEFT WHITE SQUARE BRACKET').
unicode_script(0x301B, 0x301B, 'Common', 'Pe', 1, 'RIGHT WHITE SQUARE BRACKET').
unicode_script(0x301C, 0x301C, 'Common', 'Pd', 1, 'WAVE DASH').
unicode_script(0x301D, 0x301D, 'Common', 'Ps', 1, 'REVERSED DOUBLE PRIME QUOTATION MARK').
unicode_script(0x301E, 0x301F, 'Common', 'Pe', 2, 'DOUBLE PRIME QUOTATION MARK..LOW DOUBLE PRIME QUOTATION MARK').
unicode_script(0x3020, 0x3020, 'Common', 'So', 1, 'POSTAL MARK FACE').
unicode_script(0x3030, 0x3030, 'Common', 'Pd', 1, 'WAVY DASH').
unicode_script(0x3031, 0x3035, 'Common', 'Lm', 5, 'VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF').
unicode_script(0x3036, 0x3037, 'Common', 'So', 2, 'CIRCLED POSTAL MARK..IDEOGRAPHIC TELEGRAPH LINE FEED SEPARATOR SYMBOL').
unicode_script(0x303C, 0x303C, 'Common', 'Lo', 1, ' MASU MARK').
unicode_script(0x303D, 0x303D, 'Common', 'Po', 1, 'PART ALTERNATION MARK').
unicode_script(0x303E, 0x303F, 'Common', 'So', 2, 'IDEOGRAPHIC VARIATION INDICATOR..IDEOGRAPHIC HALF FILL SPACE').
unicode_script(0x309B, 0x309C, 'Common', 'Sk', 2, 'KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK').
unicode_script(0x30A0, 0x30A0, 'Common', 'Pd', 1, 'KATAKANA-HIRAGANA DOUBLE HYPHEN').
unicode_script(0x30FB, 0x30FB, 'Common', 'Po', 1, 'KATAKANA MIDDLE DOT').
unicode_script(0x30FC, 0x30FC, 'Common', 'Lm', 1, 'KATAKANA-HIRAGANA PROLONGED SOUND MARK').
unicode_script(0x3190, 0x3191, 'Common', 'So', 2, 'IDEOGRAPHIC ANNOTATION LINKING MARK..IDEOGRAPHIC ANNOTATION REVERSE MARK').
unicode_script(0x3192, 0x3195, 'Common', 'No', 4, 'IDEOGRAPHIC ANNOTATION ONE MARK..IDEOGRAPHIC ANNOTATION FOUR MARK').
unicode_script(0x3196, 0x319F, 'Common', 'So', 10, 'IDEOGRAPHIC ANNOTATION TOP MARK..IDEOGRAPHIC ANNOTATION MAN MARK').
unicode_script(0x31C0, 0x31E3, 'Common', 'So', 36, 'CJK STROKE T..CJK STROKE Q').
unicode_script(0x3220, 0x3229, 'Common', 'No', 10, 'PARENTHESIZED IDEOGRAPH ONE..PARENTHESIZED IDEOGRAPH TEN').
unicode_script(0x322A, 0x3247, 'Common', 'So', 30, 'PARENTHESIZED IDEOGRAPH MOON..CIRCLED IDEOGRAPH KOTO').
unicode_script(0x3248, 0x324F, 'Common', 'No', 8, 'CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE').
unicode_script(0x3250, 0x3250, 'Common', 'So', 1, 'PARTNERSHIP SIGN').
unicode_script(0x3251, 0x325F, 'Common', 'No', 15, 'CIRCLED NUMBER TWENTY ONE..CIRCLED NUMBER THIRTY FIVE').
unicode_script(0x327F, 0x327F, 'Common', 'So', 1, 'KOREAN STANDARD SYMBOL').
unicode_script(0x3280, 0x3289, 'Common', 'No', 10, 'CIRCLED IDEOGRAPH ONE..CIRCLED IDEOGRAPH TEN').
unicode_script(0x328A, 0x32B0, 'Common', 'So', 39, 'CIRCLED IDEOGRAPH MOON..CIRCLED IDEOGRAPH NIGHT').
unicode_script(0x32B1, 0x32BF, 'Common', 'No', 15, 'CIRCLED NUMBER THIRTY SIX..CIRCLED NUMBER FIFTY').
unicode_script(0x32C0, 0x32CF, 'Common', 'So', 16, 'IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY..LIMITED LIABILITY SIGN').
unicode_script(0x3358, 0x33FF, 'Common', 'So', 168, 'IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ZERO..SQUARE GAL').
unicode_script(0x4DC0, 0x4DFF, 'Common', 'So', 64, 'HEXAGRAM FOR THE CREATIVE HEAVEN..HEXAGRAM FOR BEFORE COMPLETION').
unicode_script(0xA700, 0xA716, 'Common', 'Sk', 23, 'MODIFIER LETTER CHINESE TONE YIN PING..MODIFIER LETTER EXTRA-LOW LEFT-STEM TONE BAR').
unicode_script(0xA717, 0xA71F, 'Common', 'Lm', 9, 'MODIFIER LETTER DOT VERTICAL BAR..MODIFIER LETTER LOW INVERTED EXCLAMATION MARK').
unicode_script(0xA720, 0xA721, 'Common', 'Sk', 2, 'MODIFIER LETTER STRESS AND HIGH TONE..MODIFIER LETTER STRESS AND LOW TONE').
unicode_script(0xA788, 0xA788, 'Common', 'Lm', 1, 'MODIFIER LETTER LOW CIRCUMFLEX ACCENT').
unicode_script(0xA789, 0xA78A, 'Common', 'Sk', 2, 'MODIFIER LETTER COLON..MODIFIER LETTER SHORT EQUALS SIGN').
unicode_script(0xA830, 0xA835, 'Common', 'No', 6, 'NORTH INDIC FRACTION ONE QUARTER..NORTH INDIC FRACTION THREE SIXTEENTHS').
unicode_script(0xA836, 0xA837, 'Common', 'So', 2, 'NORTH INDIC QUARTER MARK..NORTH INDIC PLACEHOLDER MARK').
unicode_script(0xA838, 0xA838, 'Common', 'Sc', 1, 'NORTH INDIC RUPEE MARK').
unicode_script(0xA839, 0xA839, 'Common', 'So', 1, 'NORTH INDIC QUANTITY MARK').
unicode_script(0xFD3E, 0xFD3E, 'Common', 'Ps', 1, 'ORNATE LEFT PARENTHESIS').
unicode_script(0xFD3F, 0xFD3F, 'Common', 'Pe', 1, 'ORNATE RIGHT PARENTHESIS').
unicode_script(0xFDFD, 0xFDFD, 'Common', 'So', 1, 'ARABIC LIGATURE BISMILLAH AR-RAHMAN AR-RAHEEM').
unicode_script(0xFE10, 0xFE16, 'Common', 'Po', 7, 'PRESENTATION FORM FOR VERTICAL COMMA..PRESENTATION FORM FOR VERTICAL QUESTION MARK').
unicode_script(0xFE17, 0xFE17, 'Common', 'Ps', 1, 'PRESENTATION FORM FOR VERTICAL LEFT WHITE LENTICULAR BRACKET').
unicode_script(0xFE18, 0xFE18, 'Common', 'Pe', 1, 'PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET').
unicode_script(0xFE19, 0xFE19, 'Common', 'Po', 1, 'PRESENTATION FORM FOR VERTICAL HORIZONTAL ELLIPSIS').
unicode_script(0xFE30, 0xFE30, 'Common', 'Po', 1, 'PRESENTATION FORM FOR VERTICAL TWO DOT LEADER').
unicode_script(0xFE31, 0xFE32, 'Common', 'Pd', 2, 'PRESENTATION FORM FOR VERTICAL EM DASH..PRESENTATION FORM FOR VERTICAL EN DASH').
unicode_script(0xFE33, 0xFE34, 'Common', 'Pc', 2, 'PRESENTATION FORM FOR VERTICAL LOW LINE..PRESENTATION FORM FOR VERTICAL WAVY LOW LINE').
unicode_script(0xFE35, 0xFE35, 'Common', 'Ps', 1, 'PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS').
unicode_script(0xFE36, 0xFE36, 'Common', 'Pe', 1, 'PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS').
unicode_script(0xFE37, 0xFE37, 'Common', 'Ps', 1, 'PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET').
unicode_script(0xFE38, 0xFE38, 'Common', 'Pe', 1, 'PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET').
unicode_script(0xFE39, 0xFE39, 'Common', 'Ps', 1, 'PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET').
unicode_script(0xFE3A, 0xFE3A, 'Common', 'Pe', 1, 'PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET').
unicode_script(0xFE3B, 0xFE3B, 'Common', 'Ps', 1, 'PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET').
unicode_script(0xFE3C, 0xFE3C, 'Common', 'Pe', 1, 'PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET').
unicode_script(0xFE3D, 0xFE3D, 'Common', 'Ps', 1, 'PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET').
unicode_script(0xFE3E, 0xFE3E, 'Common', 'Pe', 1, 'PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET').
unicode_script(0xFE3F, 0xFE3F, 'Common', 'Ps', 1, 'PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET').
unicode_script(0xFE40, 0xFE40, 'Common', 'Pe', 1, 'PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET').
unicode_script(0xFE41, 0xFE41, 'Common', 'Ps', 1, 'PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET').
unicode_script(0xFE42, 0xFE42, 'Common', 'Pe', 1, 'PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET').
unicode_script(0xFE43, 0xFE43, 'Common', 'Ps', 1, 'PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET').
unicode_script(0xFE44, 0xFE44, 'Common', 'Pe', 1, 'PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET').
unicode_script(0xFE45, 0xFE46, 'Common', 'Po', 2, 'SESAME DOT..WHITE SESAME DOT').
unicode_script(0xFE47, 0xFE47, 'Common', 'Ps', 1, 'PRESENTATION FORM FOR VERTICAL LEFT SQUARE BRACKET').
unicode_script(0xFE48, 0xFE48, 'Common', 'Pe', 1, 'PRESENTATION FORM FOR VERTICAL RIGHT SQUARE BRACKET').
unicode_script(0xFE49, 0xFE4C, 'Common', 'Po', 4, 'DASHED OVERLINE..DOUBLE WAVY OVERLINE').
unicode_script(0xFE4D, 0xFE4F, 'Common', 'Pc', 3, 'DASHED LOW LINE..WAVY LOW LINE').
unicode_script(0xFE50, 0xFE52, 'Common', 'Po', 3, 'SMALL COMMA..SMALL FULL STOP').
unicode_script(0xFE54, 0xFE57, 'Common', 'Po', 4, 'SMALL SEMICOLON..SMALL EXCLAMATION MARK').
unicode_script(0xFE58, 0xFE58, 'Common', 'Pd', 1, 'SMALL EM DASH').
unicode_script(0xFE59, 0xFE59, 'Common', 'Ps', 1, 'SMALL LEFT PARENTHESIS').
unicode_script(0xFE5A, 0xFE5A, 'Common', 'Pe', 1, 'SMALL RIGHT PARENTHESIS').
unicode_script(0xFE5B, 0xFE5B, 'Common', 'Ps', 1, 'SMALL LEFT CURLY BRACKET').
unicode_script(0xFE5C, 0xFE5C, 'Common', 'Pe', 1, 'SMALL RIGHT CURLY BRACKET').
unicode_script(0xFE5D, 0xFE5D, 'Common', 'Ps', 1, 'SMALL LEFT TORTOISE SHELL BRACKET').
unicode_script(0xFE5E, 0xFE5E, 'Common', 'Pe', 1, 'SMALL RIGHT TORTOISE SHELL BRACKET').
unicode_script(0xFE5F, 0xFE61, 'Common', 'Po', 3, 'SMALL NUMBER SIGN..SMALL ASTERISK').
unicode_script(0xFE62, 0xFE62, 'Common', 'Sm', 1, 'SMALL PLUS SIGN').
unicode_script(0xFE63, 0xFE63, 'Common', 'Pd', 1, 'SMALL HYPHEN-MINUS').
unicode_script(0xFE64, 0xFE66, 'Common', 'Sm', 3, 'SMALL LESS-THAN SIGN..SMALL EQUALS SIGN').
unicode_script(0xFE68, 0xFE68, 'Common', 'Po', 1, 'SMALL REVERSE SOLIDUS').
unicode_script(0xFE69, 0xFE69, 'Common', 'Sc', 1, 'SMALL DOLLAR SIGN').
unicode_script(0xFE6A, 0xFE6B, 'Common', 'Po', 2, 'SMALL PERCENT SIGN..SMALL COMMERCIAL AT').
unicode_script(0xFEFF, 0xFEFF, 'Common', 'Cf', 1, 'ZERO WIDTH NO-BREAK SPACE').
unicode_script(0xFF01, 0xFF03, 'Common', 'Po', 3, 'FULLWIDTH EXCLAMATION MARK..FULLWIDTH NUMBER SIGN').
unicode_script(0xFF04, 0xFF04, 'Common', 'Sc', 1, 'FULLWIDTH DOLLAR SIGN').
unicode_script(0xFF05, 0xFF07, 'Common', 'Po', 3, 'FULLWIDTH PERCENT SIGN..FULLWIDTH APOSTROPHE').
unicode_script(0xFF08, 0xFF08, 'Common', 'Ps', 1, 'FULLWIDTH LEFT PARENTHESIS').
unicode_script(0xFF09, 0xFF09, 'Common', 'Pe', 1, 'FULLWIDTH RIGHT PARENTHESIS').
unicode_script(0xFF0A, 0xFF0A, 'Common', 'Po', 1, 'FULLWIDTH ASTERISK').
unicode_script(0xFF0B, 0xFF0B, 'Common', 'Sm', 1, 'FULLWIDTH PLUS SIGN').
unicode_script(0xFF0C, 0xFF0C, 'Common', 'Po', 1, 'FULLWIDTH COMMA').
unicode_script(0xFF0D, 0xFF0D, 'Common', 'Pd', 1, 'FULLWIDTH HYPHEN-MINUS').
unicode_script(0xFF0E, 0xFF0F, 'Common', 'Po', 2, 'FULLWIDTH FULL STOP..FULLWIDTH SOLIDUS').
unicode_script(0xFF10, 0xFF19, 'Common', 'Nd', 10, 'FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE').
unicode_script(0xFF1A, 0xFF1B, 'Common', 'Po', 2, 'FULLWIDTH COLON..FULLWIDTH SEMICOLON').
unicode_script(0xFF1C, 0xFF1E, 'Common', 'Sm', 3, 'FULLWIDTH LESS-THAN SIGN..FULLWIDTH GREATER-THAN SIGN').
unicode_script(0xFF1F, 0xFF20, 'Common', 'Po', 2, 'FULLWIDTH QUESTION MARK..FULLWIDTH COMMERCIAL AT').
unicode_script(0xFF3B, 0xFF3B, 'Common', 'Ps', 1, 'FULLWIDTH LEFT SQUARE BRACKET').
unicode_script(0xFF3C, 0xFF3C, 'Common', 'Po', 1, 'FULLWIDTH REVERSE SOLIDUS').
unicode_script(0xFF3D, 0xFF3D, 'Common', 'Pe', 1, 'FULLWIDTH RIGHT SQUARE BRACKET').
unicode_script(0xFF3E, 0xFF3E, 'Common', 'Sk', 1, 'FULLWIDTH CIRCUMFLEX ACCENT').
unicode_script(0xFF3F, 0xFF3F, 'Common', 'Pc', 1, 'FULLWIDTH LOW LINE').
unicode_script(0xFF40, 0xFF40, 'Common', 'Sk', 1, 'FULLWIDTH GRAVE ACCENT').
unicode_script(0xFF5B, 0xFF5B, 'Common', 'Ps', 1, 'FULLWIDTH LEFT CURLY BRACKET').
unicode_script(0xFF5C, 0xFF5C, 'Common', 'Sm', 1, 'FULLWIDTH VERTICAL LINE').
unicode_script(0xFF5D, 0xFF5D, 'Common', 'Pe', 1, 'FULLWIDTH RIGHT CURLY BRACKET').
unicode_script(0xFF5E, 0xFF5E, 'Common', 'Sm', 1, 'FULLWIDTH TILDE').
unicode_script(0xFF5F, 0xFF5F, 'Common', 'Ps', 1, 'FULLWIDTH LEFT WHITE PARENTHESIS').
unicode_script(0xFF60, 0xFF60, 'Common', 'Pe', 1, 'FULLWIDTH RIGHT WHITE PARENTHESIS').
unicode_script(0xFF61, 0xFF61, 'Common', 'Po', 1, 'HALFWIDTH IDEOGRAPHIC FULL STOP').
unicode_script(0xFF62, 0xFF62, 'Common', 'Ps', 1, 'HALFWIDTH LEFT CORNER BRACKET').
unicode_script(0xFF63, 0xFF63, 'Common', 'Pe', 1, 'HALFWIDTH RIGHT CORNER BRACKET').
unicode_script(0xFF64, 0xFF65, 'Common', 'Po', 2, 'HALFWIDTH IDEOGRAPHIC COMMA..HALFWIDTH KATAKANA MIDDLE DOT').
unicode_script(0xFF70, 0xFF70, 'Common', 'Lm', 1, 'HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK').
unicode_script(0xFF9E, 0xFF9F, 'Common', 'Lm', 2, 'HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK').
unicode_script(0xFFE0, 0xFFE1, 'Common', 'Sc', 2, 'FULLWIDTH CENT SIGN..FULLWIDTH POUND SIGN').
unicode_script(0xFFE2, 0xFFE2, 'Common', 'Sm', 1, 'FULLWIDTH NOT SIGN').
unicode_script(0xFFE3, 0xFFE3, 'Common', 'Sk', 1, 'FULLWIDTH MACRON').
unicode_script(0xFFE4, 0xFFE4, 'Common', 'So', 1, 'FULLWIDTH BROKEN BAR').
unicode_script(0xFFE5, 0xFFE6, 'Common', 'Sc', 2, 'FULLWIDTH YEN SIGN..FULLWIDTH WON SIGN').
unicode_script(0xFFE8, 0xFFE8, 'Common', 'So', 1, 'HALFWIDTH FORMS LIGHT VERTICAL').
unicode_script(0xFFE9, 0xFFEC, 'Common', 'Sm', 4, 'HALFWIDTH LEFTWARDS ARROW..HALFWIDTH DOWNWARDS ARROW').
unicode_script(0xFFED, 0xFFEE, 'Common', 'So', 2, 'HALFWIDTH BLACK SQUARE..HALFWIDTH WHITE CIRCLE').
unicode_script(0xFFF9, 0xFFFB, 'Common', 'Cf', 3, 'INTERLINEAR ANNOTATION ANCHOR..INTERLINEAR ANNOTATION TERMINATOR').
unicode_script(0xFFFC, 0xFFFD, 'Common', 'So', 2, 'OBJECT REPLACEMENT CHARACTER..REPLACEMENT CHARACTER').
unicode_script(0x10100, 0x10102, 'Common', 'Po', 3, 'AEGEAN WORD SEPARATOR LINE..AEGEAN CHECK MARK').
unicode_script(0x10107, 0x10133, 'Common', 'No', 45, 'AEGEAN NUMBER ONE..AEGEAN NUMBER NINETY THOUSAND').
unicode_script(0x10137, 0x1013F, 'Common', 'So', 9, 'AEGEAN WEIGHT BASE UNIT..AEGEAN MEASURE THIRD SUBUNIT').
unicode_script(0x10190, 0x1019B, 'Common', 'So', 12, 'ROMAN SEXTANS SIGN..ROMAN CENTURIAL SIGN').
unicode_script(0x101D0, 0x101FC, 'Common', 'So', 45, 'PHAISTOS DISC SIGN PEDESTRIAN..PHAISTOS DISC SIGN WAVY BAND').
unicode_script(0x1D000, 0x1D0F5, 'Common', 'So', 246, 'BYZANTINE MUSICAL SYMBOL PSILI..BYZANTINE MUSICAL SYMBOL GORGON NEO KATO').
unicode_script(0x1D100, 0x1D126, 'Common', 'So', 39, 'MUSICAL SYMBOL SINGLE BARLINE..MUSICAL SYMBOL DRUM CLEF-2').
unicode_script(0x1D129, 0x1D164, 'Common', 'So', 60, 'MUSICAL SYMBOL MULTIPLE MEASURE REST..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE').
unicode_script(0x1D165, 0x1D166, 'Common', 'Mc', 2, 'MUSICAL SYMBOL COMBINING STEM..MUSICAL SYMBOL COMBINING SPRECHGESANG STEM').
unicode_script(0x1D16A, 0x1D16C, 'Common', 'So', 3, 'MUSICAL SYMBOL FINGERED TREMOLO-1..MUSICAL SYMBOL FINGERED TREMOLO-3').
unicode_script(0x1D16D, 0x1D172, 'Common', 'Mc', 6, 'MUSICAL SYMBOL COMBINING AUGMENTATION DOT..MUSICAL SYMBOL COMBINING FLAG-5').
unicode_script(0x1D173, 0x1D17A, 'Common', 'Cf', 8, 'MUSICAL SYMBOL BEGIN BEAM..MUSICAL SYMBOL END PHRASE').
unicode_script(0x1D183, 0x1D184, 'Common', 'So', 2, 'MUSICAL SYMBOL ARPEGGIATO UP..MUSICAL SYMBOL ARPEGGIATO DOWN').
unicode_script(0x1D18C, 0x1D1A9, 'Common', 'So', 30, 'MUSICAL SYMBOL RINFORZANDO..MUSICAL SYMBOL DEGREE SLASH').
unicode_script(0x1D1AE, 0x1D1DD, 'Common', 'So', 48, 'MUSICAL SYMBOL PEDAL MARK..MUSICAL SYMBOL PES SUBPUNCTIS').
unicode_script(0x1D300, 0x1D356, 'Common', 'So', 87, 'MONOGRAM FOR EARTH..TETRAGRAM FOR FOSTERING').
unicode_script(0x1D360, 0x1D371, 'Common', 'No', 18, 'COUNTING ROD UNIT DIGIT ONE..COUNTING ROD TENS DIGIT NINE').
unicode_script(0x1D400, 0x1D454, 'Common', 'L&', 85, 'MATHEMATICAL BOLD CAPITAL A..MATHEMATICAL ITALIC SMALL G').
unicode_script(0x1D456, 0x1D49C, 'Common', 'L&', 71, 'MATHEMATICAL ITALIC SMALL I..MATHEMATICAL SCRIPT CAPITAL A').
unicode_script(0x1D49E, 0x1D49F, 'Common', 'L&', 2, 'MATHEMATICAL SCRIPT CAPITAL C..MATHEMATICAL SCRIPT CAPITAL D').
unicode_script(0x1D4A2, 0x1D4A2, 'Common', 'L&', 1, 'MATHEMATICAL SCRIPT CAPITAL G').
unicode_script(0x1D4A5, 0x1D4A6, 'Common', 'L&', 2, 'MATHEMATICAL SCRIPT CAPITAL J..MATHEMATICAL SCRIPT CAPITAL K').
unicode_script(0x1D4A9, 0x1D4AC, 'Common', 'L&', 4, 'MATHEMATICAL SCRIPT CAPITAL N..MATHEMATICAL SCRIPT CAPITAL Q').
unicode_script(0x1D4AE, 0x1D4B9, 'Common', 'L&', 12, 'MATHEMATICAL SCRIPT CAPITAL S..MATHEMATICAL SCRIPT SMALL D').
unicode_script(0x1D4BB, 0x1D4BB, 'Common', 'L&', 1, 'MATHEMATICAL SCRIPT SMALL F').
unicode_script(0x1D4BD, 0x1D4C3, 'Common', 'L&', 7, 'MATHEMATICAL SCRIPT SMALL H..MATHEMATICAL SCRIPT SMALL N').
unicode_script(0x1D4C5, 0x1D505, 'Common', 'L&', 65, 'MATHEMATICAL SCRIPT SMALL P..MATHEMATICAL FRAKTUR CAPITAL B').
unicode_script(0x1D507, 0x1D50A, 'Common', 'L&', 4, 'MATHEMATICAL FRAKTUR CAPITAL D..MATHEMATICAL FRAKTUR CAPITAL G').
unicode_script(0x1D50D, 0x1D514, 'Common', 'L&', 8, 'MATHEMATICAL FRAKTUR CAPITAL J..MATHEMATICAL FRAKTUR CAPITAL Q').
unicode_script(0x1D516, 0x1D51C, 'Common', 'L&', 7, 'MATHEMATICAL FRAKTUR CAPITAL S..MATHEMATICAL FRAKTUR CAPITAL Y').
unicode_script(0x1D51E, 0x1D539, 'Common', 'L&', 28, 'MATHEMATICAL FRAKTUR SMALL A..MATHEMATICAL DOUBLE-STRUCK CAPITAL B').
unicode_script(0x1D53B, 0x1D53E, 'Common', 'L&', 4, 'MATHEMATICAL DOUBLE-STRUCK CAPITAL D..MATHEMATICAL DOUBLE-STRUCK CAPITAL G').
unicode_script(0x1D540, 0x1D544, 'Common', 'L&', 5, 'MATHEMATICAL DOUBLE-STRUCK CAPITAL I..MATHEMATICAL DOUBLE-STRUCK CAPITAL M').
unicode_script(0x1D546, 0x1D546, 'Common', 'L&', 1, 'MATHEMATICAL DOUBLE-STRUCK CAPITAL O').
unicode_script(0x1D54A, 0x1D550, 'Common', 'L&', 7, 'MATHEMATICAL DOUBLE-STRUCK CAPITAL S..MATHEMATICAL DOUBLE-STRUCK CAPITAL Y').
unicode_script(0x1D552, 0x1D6A5, 'Common', 'L&', 340, 'MATHEMATICAL DOUBLE-STRUCK SMALL A..MATHEMATICAL ITALIC SMALL DOTLESS J').
unicode_script(0x1D6A8, 0x1D6C0, 'Common', 'L&', 25, 'MATHEMATICAL BOLD CAPITAL ALPHA..MATHEMATICAL BOLD CAPITAL OMEGA').
unicode_script(0x1D6C1, 0x1D6C1, 'Common', 'Sm', 1, 'MATHEMATICAL BOLD NABLA').
unicode_script(0x1D6C2, 0x1D6DA, 'Common', 'L&', 25, 'MATHEMATICAL BOLD SMALL ALPHA..MATHEMATICAL BOLD SMALL OMEGA').
unicode_script(0x1D6DB, 0x1D6DB, 'Common', 'Sm', 1, 'MATHEMATICAL BOLD PARTIAL DIFFERENTIAL').
unicode_script(0x1D6DC, 0x1D6FA, 'Common', 'L&', 31, 'MATHEMATICAL BOLD EPSILON SYMBOL..MATHEMATICAL ITALIC CAPITAL OMEGA').
unicode_script(0x1D6FB, 0x1D6FB, 'Common', 'Sm', 1, 'MATHEMATICAL ITALIC NABLA').
unicode_script(0x1D6FC, 0x1D714, 'Common', 'L&', 25, 'MATHEMATICAL ITALIC SMALL ALPHA..MATHEMATICAL ITALIC SMALL OMEGA').
unicode_script(0x1D715, 0x1D715, 'Common', 'Sm', 1, 'MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL').
unicode_script(0x1D716, 0x1D734, 'Common', 'L&', 31, 'MATHEMATICAL ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD ITALIC CAPITAL OMEGA').
unicode_script(0x1D735, 0x1D735, 'Common', 'Sm', 1, 'MATHEMATICAL BOLD ITALIC NABLA').
unicode_script(0x1D736, 0x1D74E, 'Common', 'L&', 25, 'MATHEMATICAL BOLD ITALIC SMALL ALPHA..MATHEMATICAL BOLD ITALIC SMALL OMEGA').
unicode_script(0x1D74F, 0x1D74F, 'Common', 'Sm', 1, 'MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL').
unicode_script(0x1D750, 0x1D76E, 'Common', 'L&', 31, 'MATHEMATICAL BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA').
unicode_script(0x1D76F, 0x1D76F, 'Common', 'Sm', 1, 'MATHEMATICAL SANS-SERIF BOLD NABLA').
unicode_script(0x1D770, 0x1D788, 'Common', 'L&', 25, 'MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA').
unicode_script(0x1D789, 0x1D789, 'Common', 'Sm', 1, 'MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL').
unicode_script(0x1D78A, 0x1D7A8, 'Common', 'L&', 31, 'MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA').
unicode_script(0x1D7A9, 0x1D7A9, 'Common', 'Sm', 1, 'MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA').
unicode_script(0x1D7AA, 0x1D7C2, 'Common', 'L&', 25, 'MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA').
unicode_script(0x1D7C3, 0x1D7C3, 'Common', 'Sm', 1, 'MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL').
unicode_script(0x1D7C4, 0x1D7CB, 'Common', 'L&', 8, 'MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD SMALL DIGAMMA').
unicode_script(0x1D7CE, 0x1D7FF, 'Common', 'Nd', 50, 'MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE').
unicode_script(0x1F000, 0x1F02B, 'Common', 'So', 44, 'MAHJONG TILE EAST WIND..MAHJONG TILE BACK').
unicode_script(0x1F030, 0x1F093, 'Common', 'So', 100, 'DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06').
unicode_script(0x1F0A0, 0x1F0AE, 'Common', 'So', 15, 'PLAYING CARD BACK..PLAYING CARD KING OF SPADES').
unicode_script(0x1F0B1, 0x1F0BE, 'Common', 'So', 14, 'PLAYING CARD ACE OF HEARTS..PLAYING CARD KING OF HEARTS').
unicode_script(0x1F0C1, 0x1F0CF, 'Common', 'So', 15, 'PLAYING CARD ACE OF DIAMONDS..PLAYING CARD BLACK JOKER').
unicode_script(0x1F0D1, 0x1F0DF, 'Common', 'So', 15, 'PLAYING CARD ACE OF CLUBS..PLAYING CARD WHITE JOKER').
unicode_script(0x1F100, 0x1F10A, 'Common', 'No', 11, 'DIGIT ZERO FULL STOP..DIGIT NINE COMMA').
unicode_script(0x1F110, 0x1F12E, 'Common', 'So', 31, 'PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED WZ').
unicode_script(0x1F130, 0x1F16B, 'Common', 'So', 60, 'SQUARED LATIN CAPITAL LETTER A..RAISED MD SIGN').
unicode_script(0x1F170, 0x1F19A, 'Common', 'So', 43, 'NEGATIVE SQUARED LATIN CAPITAL LETTER A..SQUARED VS').
unicode_script(0x1F1E6, 0x1F1FF, 'Common', 'So', 26, 'REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z').
unicode_script(0x1F201, 0x1F202, 'Common', 'So', 2, 'SQUARED KATAKANA KOKO..SQUARED KATAKANA SA').
unicode_script(0x1F210, 0x1F23A, 'Common', 'So', 43, 'SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-55B6').
unicode_script(0x1F240, 0x1F248, 'Common', 'So', 9, 'TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557').
unicode_script(0x1F250, 0x1F251, 'Common', 'So', 2, 'CIRCLED IDEOGRAPH ADVANTAGE..CIRCLED IDEOGRAPH ACCEPT').
unicode_script(0x1F300, 0x1F320, 'Common', 'So', 33, 'CYCLONE..SHOOTING STAR').
unicode_script(0x1F330, 0x1F335, 'Common', 'So', 6, 'CHESTNUT..CACTUS').
unicode_script(0x1F337, 0x1F37C, 'Common', 'So', 70, 'TULIP..BABY BOTTLE').
unicode_script(0x1F380, 0x1F393, 'Common', 'So', 20, 'RIBBON..GRADUATION CAP').
unicode_script(0x1F3A0, 0x1F3C4, 'Common', 'So', 37, 'CAROUSEL HORSE..SURFER').
unicode_script(0x1F3C6, 0x1F3CA, 'Common', 'So', 5, 'TROPHY..SWIMMER').
unicode_script(0x1F3E0, 0x1F3F0, 'Common', 'So', 17, 'HOUSE BUILDING..EUROPEAN CASTLE').
unicode_script(0x1F400, 0x1F43E, 'Common', 'So', 63, 'RAT..PAW PRINTS').
unicode_script(0x1F440, 0x1F440, 'Common', 'So', 1, 'EYES').
unicode_script(0x1F442, 0x1F4F7, 'Common', 'So', 182, 'EAR..CAMERA').
unicode_script(0x1F4F9, 0x1F4FC, 'Common', 'So', 4, 'VIDEO CAMERA..VIDEOCASSETTE').
unicode_script(0x1F500, 0x1F53D, 'Common', 'So', 62, 'TWISTED RIGHTWARDS ARROWS..DOWN-POINTING SMALL RED TRIANGLE').
unicode_script(0x1F540, 0x1F543, 'Common', 'So', 4, 'CIRCLED CROSS POMMEE..NOTCHED LEFT SEMICIRCLE WITH THREE DOTS').
unicode_script(0x1F550, 0x1F567, 'Common', 'So', 24, 'CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY').
unicode_script(0x1F5FB, 0x1F640, 'Common', 'So', 70, 'MOUNT FUJI..WEARY CAT FACE').
unicode_script(0x1F645, 0x1F64F, 'Common', 'So', 11, 'FACE WITH NO GOOD GESTURE..PERSON WITH FOLDED HANDS').
unicode_script(0x1F680, 0x1F6C5, 'Common', 'So', 70, 'ROCKET..LEFT LUGGAGE').
unicode_script(0x1F700, 0x1F773, 'Common', 'So', 116, 'ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE').
unicode_script(0xE0001, 0xE0001, 'Common', 'Cf', 1, 'LANGUAGE TAG').
unicode_script(0xE0020, 0xE007F, 'Common', 'Cf', 96, 'TAG SPACE..CANCEL TAG').

% Total code points: 6412

% ================================================

unicode_script(0x0041, 0x005A, 'Latin', 'L&', 26, 'LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z').
unicode_script(0x0061, 0x007A, 'Latin', 'L&', 26, 'LATIN SMALL LETTER A..LATIN SMALL LETTER Z').
unicode_script(0x00AA, 0x00AA, 'Latin', 'Lo', 1, ' FEMININE ORDINAL INDICATOR').
unicode_script(0x00BA, 0x00BA, 'Latin', 'Lo', 1, ' MASCULINE ORDINAL INDICATOR').
unicode_script(0x00C0, 0x00D6, 'Latin', 'L&', 23, 'LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER O WITH DIAERESIS').
unicode_script(0x00D8, 0x00F6, 'Latin', 'L&', 31, 'LATIN CAPITAL LETTER O WITH STROKE..LATIN SMALL LETTER O WITH DIAERESIS').
unicode_script(0x00F8, 0x01BA, 'Latin', 'L&', 195, 'LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER EZH WITH TAIL').
unicode_script(0x01BB, 0x01BB, 'Latin', 'Lo', 1, ' LATIN LETTER TWO WITH STROKE').
unicode_script(0x01BC, 0x01BF, 'Latin', 'L&', 4, 'LATIN CAPITAL LETTER TONE FIVE..LATIN LETTER WYNN').
unicode_script(0x01C0, 0x01C3, 'Latin', 'Lo', 4, 'LATIN LETTER DENTAL CLICK..LATIN LETTER RETROFLEX CLICK').
unicode_script(0x01C4, 0x0293, 'Latin', 'L&', 208, 'LATIN CAPITAL LETTER DZ WITH CARON..LATIN SMALL LETTER EZH WITH CURL').
unicode_script(0x0294, 0x0294, 'Latin', 'Lo', 1, ' LATIN LETTER GLOTTAL STOP').
unicode_script(0x0295, 0x02AF, 'Latin', 'L&', 27, 'LATIN LETTER PHARYNGEAL VOICED FRICATIVE..LATIN SMALL LETTER TURNED H WITH FISHHOOK AND TAIL').
unicode_script(0x02B0, 0x02B8, 'Latin', 'Lm', 9, 'MODIFIER LETTER SMALL H..MODIFIER LETTER SMALL Y').
unicode_script(0x02E0, 0x02E4, 'Latin', 'Lm', 5, 'MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP').
unicode_script(0x1D00, 0x1D25, 'Latin', 'L&', 38, 'LATIN LETTER SMALL CAPITAL A..LATIN LETTER AIN').
unicode_script(0x1D2C, 0x1D5C, 'Latin', 'Lm', 49, 'MODIFIER LETTER CAPITAL A..MODIFIER LETTER SMALL AIN').
unicode_script(0x1D62, 0x1D65, 'Latin', 'Lm', 4, 'LATIN SUBSCRIPT SMALL LETTER I..LATIN SUBSCRIPT SMALL LETTER V').
unicode_script(0x1D6B, 0x1D77, 'Latin', 'L&', 13, 'LATIN SMALL LETTER UE..LATIN SMALL LETTER TURNED G').
unicode_script(0x1D79, 0x1D9A, 'Latin', 'L&', 34, 'LATIN SMALL LETTER INSULAR G..LATIN SMALL LETTER EZH WITH RETROFLEX HOOK').
unicode_script(0x1D9B, 0x1DBE, 'Latin', 'Lm', 36, 'MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL EZH').
unicode_script(0x1E00, 0x1EFF, 'Latin', 'L&', 256, 'LATIN CAPITAL LETTER A WITH RING BELOW..LATIN SMALL LETTER Y WITH LOOP').
unicode_script(0x2071, 0x2071, 'Latin', 'Lm', 1, 'SUPERSCRIPT LATIN SMALL LETTER I').
unicode_script(0x207F, 0x207F, 'Latin', 'Lm', 1, 'SUPERSCRIPT LATIN SMALL LETTER N').
unicode_script(0x2090, 0x209C, 'Latin', 'Lm', 13, 'LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T').
unicode_script(0x212A, 0x212B, 'Latin', 'L&', 2, 'KELVIN SIGN..ANGSTROM SIGN').
unicode_script(0x2132, 0x2132, 'Latin', 'L&', 1, 'TURNED CAPITAL F').
unicode_script(0x214E, 0x214E, 'Latin', 'L&', 1, 'TURNED SMALL F').
unicode_script(0x2160, 0x2182, 'Latin', 'Nl', 35, 'ROMAN NUMERAL ONE..ROMAN NUMERAL TEN THOUSAND').
unicode_script(0x2183, 0x2184, 'Latin', 'L&', 2, 'ROMAN NUMERAL REVERSED ONE HUNDRED..LATIN SMALL LETTER REVERSED C').
unicode_script(0x2185, 0x2188, 'Latin', 'Nl', 4, 'ROMAN NUMERAL SIX LATE FORM..ROMAN NUMERAL ONE HUNDRED THOUSAND').
unicode_script(0x2C60, 0x2C7B, 'Latin', 'L&', 28, 'LATIN CAPITAL LETTER L WITH DOUBLE BAR..LATIN LETTER SMALL CAPITAL TURNED E').
unicode_script(0x2C7C, 0x2C7D, 'Latin', 'Lm', 2, 'LATIN SUBSCRIPT SMALL LETTER J..MODIFIER LETTER CAPITAL V').
unicode_script(0x2C7E, 0x2C7F, 'Latin', 'L&', 2, 'LATIN CAPITAL LETTER S WITH SWASH TAIL..LATIN CAPITAL LETTER Z WITH SWASH TAIL').
unicode_script(0xA722, 0xA76F, 'Latin', 'L&', 78, 'LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF..LATIN SMALL LETTER CON').
unicode_script(0xA770, 0xA770, 'Latin', 'Lm', 1, 'MODIFIER LETTER US').
unicode_script(0xA771, 0xA787, 'Latin', 'L&', 23, 'LATIN SMALL LETTER DUM..LATIN SMALL LETTER INSULAR T').
unicode_script(0xA78B, 0xA78E, 'Latin', 'L&', 4, 'LATIN CAPITAL LETTER SALTILLO..LATIN SMALL LETTER L WITH RETROFLEX HOOK AND BELT').
unicode_script(0xA790, 0xA793, 'Latin', 'L&', 4, 'LATIN CAPITAL LETTER N WITH DESCENDER..LATIN SMALL LETTER C WITH BAR').
unicode_script(0xA7A0, 0xA7AA, 'Latin', 'L&', 11, 'LATIN CAPITAL LETTER G WITH OBLIQUE STROKE..LATIN CAPITAL LETTER H WITH HOOK').
unicode_script(0xA7F8, 0xA7F9, 'Latin', 'Lm', 2, 'MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE').
unicode_script(0xA7FA, 0xA7FA, 'Latin', 'L&', 1, 'LATIN LETTER SMALL CAPITAL TURNED M').
unicode_script(0xA7FB, 0xA7FF, 'Latin', 'Lo', 5, 'LATIN EPIGRAPHIC LETTER REVERSED F..LATIN EPIGRAPHIC LETTER ARCHAIC M').
unicode_script(0xFB00, 0xFB06, 'Latin', 'L&', 7, 'LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST').
unicode_script(0xFF21, 0xFF3A, 'Latin', 'L&', 26, 'FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z').
unicode_script(0xFF41, 0xFF5A, 'Latin', 'L&', 26, 'FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z').

% Total code points: 1272

% ================================================

unicode_script(0x0370, 0x0373, 'Greek', 'L&', 4, 'GREEK CAPITAL LETTER HETA..GREEK SMALL LETTER ARCHAIC SAMPI').
unicode_script(0x0375, 0x0375, 'Greek', 'Sk', 1, 'GREEK LOWER NUMERAL SIGN').
unicode_script(0x0376, 0x0377, 'Greek', 'L&', 2, 'GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA..GREEK SMALL LETTER PAMPHYLIAN DIGAMMA').
unicode_script(0x037A, 0x037A, 'Greek', 'Lm', 1, 'GREEK YPOGEGRAMMENI').
unicode_script(0x037B, 0x037D, 'Greek', 'L&', 3, 'GREEK SMALL REVERSED LUNATE SIGMA SYMBOL..GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL').
unicode_script(0x0384, 0x0384, 'Greek', 'Sk', 1, 'GREEK TONOS').
unicode_script(0x0386, 0x0386, 'Greek', 'L&', 1, 'GREEK CAPITAL LETTER ALPHA WITH TONOS').
unicode_script(0x0388, 0x038A, 'Greek', 'L&', 3, 'GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS').
unicode_script(0x038C, 0x038C, 'Greek', 'L&', 1, 'GREEK CAPITAL LETTER OMICRON WITH TONOS').
unicode_script(0x038E, 0x03A1, 'Greek', 'L&', 20, 'GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK CAPITAL LETTER RHO').
unicode_script(0x03A3, 0x03E1, 'Greek', 'L&', 63, 'GREEK CAPITAL LETTER SIGMA..GREEK SMALL LETTER SAMPI').
unicode_script(0x03F0, 0x03F5, 'Greek', 'L&', 6, 'GREEK KAPPA SYMBOL..GREEK LUNATE EPSILON SYMBOL').
unicode_script(0x03F6, 0x03F6, 'Greek', 'Sm', 1, 'GREEK REVERSED LUNATE EPSILON SYMBOL').
unicode_script(0x03F7, 0x03FF, 'Greek', 'L&', 9, 'GREEK CAPITAL LETTER SHO..GREEK CAPITAL REVERSED DOTTED LUNATE SIGMA SYMBOL').
unicode_script(0x1D26, 0x1D2A, 'Greek', 'L&', 5, 'GREEK LETTER SMALL CAPITAL GAMMA..GREEK LETTER SMALL CAPITAL PSI').
unicode_script(0x1D5D, 0x1D61, 'Greek', 'Lm', 5, 'MODIFIER LETTER SMALL BETA..MODIFIER LETTER SMALL CHI').
unicode_script(0x1D66, 0x1D6A, 'Greek', 'Lm', 5, 'GREEK SUBSCRIPT SMALL LETTER BETA..GREEK SUBSCRIPT SMALL LETTER CHI').
unicode_script(0x1DBF, 0x1DBF, 'Greek', 'Lm', 1, 'MODIFIER LETTER SMALL THETA').
unicode_script(0x1F00, 0x1F15, 'Greek', 'L&', 22, 'GREEK SMALL LETTER ALPHA WITH PSILI..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA').
unicode_script(0x1F18, 0x1F1D, 'Greek', 'L&', 6, 'GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA').
unicode_script(0x1F20, 0x1F45, 'Greek', 'L&', 38, 'GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA').
unicode_script(0x1F48, 0x1F4D, 'Greek', 'L&', 6, 'GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA').
unicode_script(0x1F50, 0x1F57, 'Greek', 'L&', 8, 'GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI').
unicode_script(0x1F59, 0x1F59, 'Greek', 'L&', 1, 'GREEK CAPITAL LETTER UPSILON WITH DASIA').
unicode_script(0x1F5B, 0x1F5B, 'Greek', 'L&', 1, 'GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA').
unicode_script(0x1F5D, 0x1F5D, 'Greek', 'L&', 1, 'GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA').
unicode_script(0x1F5F, 0x1F7D, 'Greek', 'L&', 31, 'GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI..GREEK SMALL LETTER OMEGA WITH OXIA').
unicode_script(0x1F80, 0x1FB4, 'Greek', 'L&', 53, 'GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI').
unicode_script(0x1FB6, 0x1FBC, 'Greek', 'L&', 7, 'GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI').
unicode_script(0x1FBD, 0x1FBD, 'Greek', 'Sk', 1, 'GREEK KORONIS').
unicode_script(0x1FBE, 0x1FBE, 'Greek', 'L&', 1, 'GREEK PROSGEGRAMMENI').
unicode_script(0x1FBF, 0x1FC1, 'Greek', 'Sk', 3, 'GREEK PSILI..GREEK DIALYTIKA AND PERISPOMENI').
unicode_script(0x1FC2, 0x1FC4, 'Greek', 'L&', 3, 'GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI').
unicode_script(0x1FC6, 0x1FCC, 'Greek', 'L&', 7, 'GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI').
unicode_script(0x1FCD, 0x1FCF, 'Greek', 'Sk', 3, 'GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI').
unicode_script(0x1FD0, 0x1FD3, 'Greek', 'L&', 4, 'GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA').
unicode_script(0x1FD6, 0x1FDB, 'Greek', 'L&', 6, 'GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK CAPITAL LETTER IOTA WITH OXIA').
unicode_script(0x1FDD, 0x1FDF, 'Greek', 'Sk', 3, 'GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI').
unicode_script(0x1FE0, 0x1FEC, 'Greek', 'L&', 13, 'GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA').
unicode_script(0x1FED, 0x1FEF, 'Greek', 'Sk', 3, 'GREEK DIALYTIKA AND VARIA..GREEK VARIA').
unicode_script(0x1FF2, 0x1FF4, 'Greek', 'L&', 3, 'GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI').
unicode_script(0x1FF6, 0x1FFC, 'Greek', 'L&', 7, 'GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI').
unicode_script(0x1FFD, 0x1FFE, 'Greek', 'Sk', 2, 'GREEK OXIA..GREEK DASIA').
unicode_script(0x2126, 0x2126, 'Greek', 'L&', 1, 'OHM SIGN').
unicode_script(0x10140, 0x10174, 'Greek', 'Nl', 53, 'GREEK ACROPHONIC ATTIC ONE QUARTER..GREEK ACROPHONIC STRATIAN FIFTY MNAS').
unicode_script(0x10175, 0x10178, 'Greek', 'No', 4, 'GREEK ONE HALF SIGN..GREEK THREE QUARTERS SIGN').
unicode_script(0x10179, 0x10189, 'Greek', 'So', 17, 'GREEK YEAR SIGN..GREEK TRYBLION BASE SIGN').
unicode_script(0x1018A, 0x1018A, 'Greek', 'No', 1, 'GREEK ZERO SIGN').
unicode_script(0x1D200, 0x1D241, 'Greek', 'So', 66, 'GREEK VOCAL NOTATION SYMBOL-1..GREEK INSTRUMENTAL NOTATION SYMBOL-54').
unicode_script(0x1D242, 0x1D244, 'Greek', 'Mn', 3, 'COMBINING GREEK MUSICAL TRISEME..COMBINING GREEK MUSICAL PENTASEME').
unicode_script(0x1D245, 0x1D245, 'Greek', 'So', 1, 'GREEK MUSICAL LEIMMA').

% Total code points: 511

% ================================================

unicode_script(0x0400, 0x0481, 'Cyrillic', 'L&', 130, 'CYRILLIC CAPITAL LETTER IE WITH GRAVE..CYRILLIC SMALL LETTER KOPPA').
unicode_script(0x0482, 0x0482, 'Cyrillic', 'So', 1, 'CYRILLIC THOUSANDS SIGN').
unicode_script(0x0483, 0x0484, 'Cyrillic', 'Mn', 2, 'COMBINING CYRILLIC TITLO..COMBINING CYRILLIC PALATALIZATION').
unicode_script(0x0487, 0x0487, 'Cyrillic', 'Mn', 1, 'COMBINING CYRILLIC POKRYTIE').
unicode_script(0x0488, 0x0489, 'Cyrillic', 'Me', 2, 'COMBINING CYRILLIC HUNDRED THOUSANDS SIGN..COMBINING CYRILLIC MILLIONS SIGN').
unicode_script(0x048A, 0x0527, 'Cyrillic', 'L&', 158, 'CYRILLIC CAPITAL LETTER SHORT I WITH TAIL..CYRILLIC SMALL LETTER SHHA WITH DESCENDER').
unicode_script(0x1D2B, 0x1D2B, 'Cyrillic', 'L&', 1, 'CYRILLIC LETTER SMALL CAPITAL EL').
unicode_script(0x1D78, 0x1D78, 'Cyrillic', 'Lm', 1, 'MODIFIER LETTER CYRILLIC EN').
unicode_script(0x2DE0, 0x2DFF, 'Cyrillic', 'Mn', 32, 'COMBINING CYRILLIC LETTER BE..COMBINING CYRILLIC LETTER IOTIFIED BIG YUS').
unicode_script(0xA640, 0xA66D, 'Cyrillic', 'L&', 46, 'CYRILLIC CAPITAL LETTER ZEMLYA..CYRILLIC SMALL LETTER DOUBLE MONOCULAR O').
unicode_script(0xA66E, 0xA66E, 'Cyrillic', 'Lo', 1, ' CYRILLIC LETTER MULTIOCULAR O').
unicode_script(0xA66F, 0xA66F, 'Cyrillic', 'Mn', 1, 'COMBINING CYRILLIC VZMET').
unicode_script(0xA670, 0xA672, 'Cyrillic', 'Me', 3, 'COMBINING CYRILLIC TEN MILLIONS SIGN..COMBINING CYRILLIC THOUSAND MILLIONS SIGN').
unicode_script(0xA673, 0xA673, 'Cyrillic', 'Po', 1, 'SLAVONIC ASTERISK').
unicode_script(0xA674, 0xA67D, 'Cyrillic', 'Mn', 10, 'COMBINING CYRILLIC LETTER UKRAINIAN IE..COMBINING CYRILLIC PAYEROK').
unicode_script(0xA67E, 0xA67E, 'Cyrillic', 'Po', 1, 'CYRILLIC KAVYKA').
unicode_script(0xA67F, 0xA67F, 'Cyrillic', 'Lm', 1, 'CYRILLIC PAYEROK').
unicode_script(0xA680, 0xA697, 'Cyrillic', 'L&', 24, 'CYRILLIC CAPITAL LETTER DWE..CYRILLIC SMALL LETTER SHWE').
unicode_script(0xA69F, 0xA69F, 'Cyrillic', 'Mn', 1, 'COMBINING CYRILLIC LETTER IOTIFIED E').

% Total code points: 417

% ================================================

unicode_script(0x0531, 0x0556, 'Armenian', 'L&', 38, 'ARMENIAN CAPITAL LETTER AYB..ARMENIAN CAPITAL LETTER FEH').
unicode_script(0x0559, 0x0559, 'Armenian', 'Lm', 1, 'ARMENIAN MODIFIER LETTER LEFT HALF RING').
unicode_script(0x055A, 0x055F, 'Armenian', 'Po', 6, 'ARMENIAN APOSTROPHE..ARMENIAN ABBREVIATION MARK').
unicode_script(0x0561, 0x0587, 'Armenian', 'L&', 39, 'ARMENIAN SMALL LETTER AYB..ARMENIAN SMALL LIGATURE ECH YIWN').
unicode_script(0x058A, 0x058A, 'Armenian', 'Pd', 1, 'ARMENIAN HYPHEN').
unicode_script(0x058F, 0x058F, 'Armenian', 'Sc', 1, 'ARMENIAN DRAM SIGN').
unicode_script(0xFB13, 0xFB17, 'Armenian', 'L&', 5, 'ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH').

% Total code points: 91

% ================================================

unicode_script(0x0591, 0x05BD, 'Hebrew', 'Mn', 45, 'HEBREW ACCENT ETNAHTA..HEBREW POINT METEG').
unicode_script(0x05BE, 0x05BE, 'Hebrew', 'Pd', 1, 'HEBREW PUNCTUATION MAQAF').
unicode_script(0x05BF, 0x05BF, 'Hebrew', 'Mn', 1, 'HEBREW POINT RAFE').
unicode_script(0x05C0, 0x05C0, 'Hebrew', 'Po', 1, 'HEBREW PUNCTUATION PASEQ').
unicode_script(0x05C1, 0x05C2, 'Hebrew', 'Mn', 2, 'HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT').
unicode_script(0x05C3, 0x05C3, 'Hebrew', 'Po', 1, 'HEBREW PUNCTUATION SOF PASUQ').
unicode_script(0x05C4, 0x05C5, 'Hebrew', 'Mn', 2, 'HEBREW MARK UPPER DOT..HEBREW MARK LOWER DOT').
unicode_script(0x05C6, 0x05C6, 'Hebrew', 'Po', 1, 'HEBREW PUNCTUATION NUN HAFUKHA').
unicode_script(0x05C7, 0x05C7, 'Hebrew', 'Mn', 1, 'HEBREW POINT QAMATS QATAN').
unicode_script(0x05D0, 0x05EA, 'Hebrew', 'Lo', 27, 'HEBREW LETTER ALEF..HEBREW LETTER TAV').
unicode_script(0x05F0, 0x05F2, 'Hebrew', 'Lo', 3, 'HEBREW LIGATURE YIDDISH DOUBLE VAV..HEBREW LIGATURE YIDDISH DOUBLE YOD').
unicode_script(0x05F3, 0x05F4, 'Hebrew', 'Po', 2, 'HEBREW PUNCTUATION GERESH..HEBREW PUNCTUATION GERSHAYIM').
unicode_script(0xFB1D, 0xFB1D, 'Hebrew', 'Lo', 1, ' HEBREW LETTER YOD WITH HIRIQ').
unicode_script(0xFB1E, 0xFB1E, 'Hebrew', 'Mn', 1, 'HEBREW POINT JUDEO-SPANISH VARIKA').
unicode_script(0xFB1F, 0xFB28, 'Hebrew', 'Lo', 10, 'HEBREW LIGATURE YIDDISH YOD YOD PATAH..HEBREW LETTER WIDE TAV').
unicode_script(0xFB29, 0xFB29, 'Hebrew', 'Sm', 1, 'HEBREW LETTER ALTERNATIVE PLUS SIGN').
unicode_script(0xFB2A, 0xFB36, 'Hebrew', 'Lo', 13, 'HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH').
unicode_script(0xFB38, 0xFB3C, 'Hebrew', 'Lo', 5, 'HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH').
unicode_script(0xFB3E, 0xFB3E, 'Hebrew', 'Lo', 1, ' HEBREW LETTER MEM WITH DAGESH').
unicode_script(0xFB40, 0xFB41, 'Hebrew', 'Lo', 2, 'HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH').
unicode_script(0xFB43, 0xFB44, 'Hebrew', 'Lo', 2, 'HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH').
unicode_script(0xFB46, 0xFB4F, 'Hebrew', 'Lo', 10, 'HEBREW LETTER TSADI WITH DAGESH..HEBREW LIGATURE ALEF LAMED').

% Total code points: 133

% ================================================

unicode_script(0x0600, 0x0604, 'Arabic', 'Cf', 5, 'ARABIC NUMBER SIGN..ARABIC SIGN SAMVAT').
unicode_script(0x0606, 0x0608, 'Arabic', 'Sm', 3, 'ARABIC-INDIC CUBE ROOT..ARABIC RAY').
unicode_script(0x0609, 0x060A, 'Arabic', 'Po', 2, 'ARABIC-INDIC PER MILLE SIGN..ARABIC-INDIC PER TEN THOUSAND SIGN').
unicode_script(0x060B, 0x060B, 'Arabic', 'Sc', 1, 'AFGHANI SIGN').
unicode_script(0x060D, 0x060D, 'Arabic', 'Po', 1, 'ARABIC DATE SEPARATOR').
unicode_script(0x060E, 0x060F, 'Arabic', 'So', 2, 'ARABIC POETIC VERSE SIGN..ARABIC SIGN MISRA').
unicode_script(0x0610, 0x061A, 'Arabic', 'Mn', 11, 'ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM..ARABIC SMALL KASRA').
unicode_script(0x061E, 0x061E, 'Arabic', 'Po', 1, 'ARABIC TRIPLE DOT PUNCTUATION MARK').
unicode_script(0x0620, 0x063F, 'Arabic', 'Lo', 32, 'ARABIC LETTER KASHMIRI YEH..ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE').
unicode_script(0x0641, 0x064A, 'Arabic', 'Lo', 10, 'ARABIC LETTER FEH..ARABIC LETTER YEH').
unicode_script(0x0656, 0x065E, 'Arabic', 'Mn', 9, 'ARABIC SUBSCRIPT ALEF..ARABIC FATHA WITH TWO DOTS').
unicode_script(0x066A, 0x066D, 'Arabic', 'Po', 4, 'ARABIC PERCENT SIGN..ARABIC FIVE POINTED STAR').
unicode_script(0x066E, 0x066F, 'Arabic', 'Lo', 2, 'ARABIC LETTER DOTLESS BEH..ARABIC LETTER DOTLESS QAF').
unicode_script(0x0671, 0x06D3, 'Arabic', 'Lo', 99, 'ARABIC LETTER ALEF WASLA..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE').
unicode_script(0x06D4, 0x06D4, 'Arabic', 'Po', 1, 'ARABIC FULL STOP').
unicode_script(0x06D5, 0x06D5, 'Arabic', 'Lo', 1, ' ARABIC LETTER AE').
unicode_script(0x06D6, 0x06DC, 'Arabic', 'Mn', 7, 'ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN').
unicode_script(0x06DE, 0x06DE, 'Arabic', 'So', 1, 'ARABIC START OF RUB EL HIZB').
unicode_script(0x06DF, 0x06E4, 'Arabic', 'Mn', 6, 'ARABIC SMALL HIGH ROUNDED ZERO..ARABIC SMALL HIGH MADDA').
unicode_script(0x06E5, 0x06E6, 'Arabic', 'Lm', 2, 'ARABIC SMALL WAW..ARABIC SMALL YEH').
unicode_script(0x06E7, 0x06E8, 'Arabic', 'Mn', 2, 'ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON').
unicode_script(0x06E9, 0x06E9, 'Arabic', 'So', 1, 'ARABIC PLACE OF SAJDAH').
unicode_script(0x06EA, 0x06ED, 'Arabic', 'Mn', 4, 'ARABIC EMPTY CENTRE LOW STOP..ARABIC SMALL LOW MEEM').
unicode_script(0x06EE, 0x06EF, 'Arabic', 'Lo', 2, 'ARABIC LETTER DAL WITH INVERTED V..ARABIC LETTER REH WITH INVERTED V').
unicode_script(0x06F0, 0x06F9, 'Arabic', 'Nd', 10, 'EXTENDED ARABIC-INDIC DIGIT ZERO..EXTENDED ARABIC-INDIC DIGIT NINE').
unicode_script(0x06FA, 0x06FC, 'Arabic', 'Lo', 3, 'ARABIC LETTER SHEEN WITH DOT BELOW..ARABIC LETTER GHAIN WITH DOT BELOW').
unicode_script(0x06FD, 0x06FE, 'Arabic', 'So', 2, 'ARABIC SIGN SINDHI AMPERSAND..ARABIC SIGN SINDHI POSTPOSITION MEN').
unicode_script(0x06FF, 0x06FF, 'Arabic', 'Lo', 1, ' ARABIC LETTER HEH WITH INVERTED V').
unicode_script(0x0750, 0x077F, 'Arabic', 'Lo', 48, 'ARABIC LETTER BEH WITH THREE DOTS HORIZONTALLY BELOW..ARABIC LETTER KAF WITH TWO DOTS ABOVE').
unicode_script(0x08A0, 0x08A0, 'Arabic', 'Lo', 1, ' ARABIC LETTER BEH WITH SMALL V BELOW').
unicode_script(0x08A2, 0x08AC, 'Arabic', 'Lo', 11, 'ARABIC LETTER JEEM WITH TWO DOTS ABOVE..ARABIC LETTER ROHINGYA YEH').
unicode_script(0x08E4, 0x08FE, 'Arabic', 'Mn', 27, 'ARABIC CURLY FATHA..ARABIC DAMMA WITH DOT').
unicode_script(0xFB50, 0xFBB1, 'Arabic', 'Lo', 98, 'ARABIC LETTER ALEF WASLA ISOLATED FORM..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM').
unicode_script(0xFBB2, 0xFBC1, 'Arabic', 'Sk', 16, 'ARABIC SYMBOL DOT ABOVE..ARABIC SYMBOL SMALL TAH BELOW').
unicode_script(0xFBD3, 0xFD3D, 'Arabic', 'Lo', 363, 'ARABIC LETTER NG ISOLATED FORM..ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM').
unicode_script(0xFD50, 0xFD8F, 'Arabic', 'Lo', 64, 'ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM').
unicode_script(0xFD92, 0xFDC7, 'Arabic', 'Lo', 54, 'ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM').
unicode_script(0xFDF0, 0xFDFB, 'Arabic', 'Lo', 12, 'ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU').
unicode_script(0xFDFC, 0xFDFC, 'Arabic', 'Sc', 1, 'RIAL SIGN').
unicode_script(0xFE70, 0xFE74, 'Arabic', 'Lo', 5, 'ARABIC FATHATAN ISOLATED FORM..ARABIC KASRATAN ISOLATED FORM').
unicode_script(0xFE76, 0xFEFC, 'Arabic', 'Lo', 135, 'ARABIC FATHA ISOLATED FORM..ARABIC LIGATURE LAM WITH ALEF FINAL FORM').
unicode_script(0x10E60, 0x10E7E, 'Arabic', 'No', 31, 'RUMI DIGIT ONE..RUMI FRACTION TWO THIRDS').
unicode_script(0x1EE00, 0x1EE03, 'Arabic', 'Lo', 4, 'ARABIC MATHEMATICAL ALEF..ARABIC MATHEMATICAL DAL').
unicode_script(0x1EE05, 0x1EE1F, 'Arabic', 'Lo', 27, 'ARABIC MATHEMATICAL WAW..ARABIC MATHEMATICAL DOTLESS QAF').
unicode_script(0x1EE21, 0x1EE22, 'Arabic', 'Lo', 2, 'ARABIC MATHEMATICAL INITIAL BEH..ARABIC MATHEMATICAL INITIAL JEEM').
unicode_script(0x1EE24, 0x1EE24, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL INITIAL HEH').
unicode_script(0x1EE27, 0x1EE27, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL INITIAL HAH').
unicode_script(0x1EE29, 0x1EE32, 'Arabic', 'Lo', 10, 'ARABIC MATHEMATICAL INITIAL YEH..ARABIC MATHEMATICAL INITIAL QAF').
unicode_script(0x1EE34, 0x1EE37, 'Arabic', 'Lo', 4, 'ARABIC MATHEMATICAL INITIAL SHEEN..ARABIC MATHEMATICAL INITIAL KHAH').
unicode_script(0x1EE39, 0x1EE39, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL INITIAL DAD').
unicode_script(0x1EE3B, 0x1EE3B, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL INITIAL GHAIN').
unicode_script(0x1EE42, 0x1EE42, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL TAILED JEEM').
unicode_script(0x1EE47, 0x1EE47, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL TAILED HAH').
unicode_script(0x1EE49, 0x1EE49, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL TAILED YEH').
unicode_script(0x1EE4B, 0x1EE4B, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL TAILED LAM').
unicode_script(0x1EE4D, 0x1EE4F, 'Arabic', 'Lo', 3, 'ARABIC MATHEMATICAL TAILED NOON..ARABIC MATHEMATICAL TAILED AIN').
unicode_script(0x1EE51, 0x1EE52, 'Arabic', 'Lo', 2, 'ARABIC MATHEMATICAL TAILED SAD..ARABIC MATHEMATICAL TAILED QAF').
unicode_script(0x1EE54, 0x1EE54, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL TAILED SHEEN').
unicode_script(0x1EE57, 0x1EE57, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL TAILED KHAH').
unicode_script(0x1EE59, 0x1EE59, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL TAILED DAD').
unicode_script(0x1EE5B, 0x1EE5B, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL TAILED GHAIN').
unicode_script(0x1EE5D, 0x1EE5D, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL TAILED DOTLESS NOON').
unicode_script(0x1EE5F, 0x1EE5F, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL TAILED DOTLESS QAF').
unicode_script(0x1EE61, 0x1EE62, 'Arabic', 'Lo', 2, 'ARABIC MATHEMATICAL STRETCHED BEH..ARABIC MATHEMATICAL STRETCHED JEEM').
unicode_script(0x1EE64, 0x1EE64, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL STRETCHED HEH').
unicode_script(0x1EE67, 0x1EE6A, 'Arabic', 'Lo', 4, 'ARABIC MATHEMATICAL STRETCHED HAH..ARABIC MATHEMATICAL STRETCHED KAF').
unicode_script(0x1EE6C, 0x1EE72, 'Arabic', 'Lo', 7, 'ARABIC MATHEMATICAL STRETCHED MEEM..ARABIC MATHEMATICAL STRETCHED QAF').
unicode_script(0x1EE74, 0x1EE77, 'Arabic', 'Lo', 4, 'ARABIC MATHEMATICAL STRETCHED SHEEN..ARABIC MATHEMATICAL STRETCHED KHAH').
unicode_script(0x1EE79, 0x1EE7C, 'Arabic', 'Lo', 4, 'ARABIC MATHEMATICAL STRETCHED DAD..ARABIC MATHEMATICAL STRETCHED DOTLESS BEH').
unicode_script(0x1EE7E, 0x1EE7E, 'Arabic', 'Lo', 1, ' ARABIC MATHEMATICAL STRETCHED DOTLESS FEH').
unicode_script(0x1EE80, 0x1EE89, 'Arabic', 'Lo', 10, 'ARABIC MATHEMATICAL LOOPED ALEF..ARABIC MATHEMATICAL LOOPED YEH').
unicode_script(0x1EE8B, 0x1EE9B, 'Arabic', 'Lo', 17, 'ARABIC MATHEMATICAL LOOPED LAM..ARABIC MATHEMATICAL LOOPED GHAIN').
unicode_script(0x1EEA1, 0x1EEA3, 'Arabic', 'Lo', 3, 'ARABIC MATHEMATICAL DOUBLE-STRUCK BEH..ARABIC MATHEMATICAL DOUBLE-STRUCK DAL').
unicode_script(0x1EEA5, 0x1EEA9, 'Arabic', 'Lo', 5, 'ARABIC MATHEMATICAL DOUBLE-STRUCK WAW..ARABIC MATHEMATICAL DOUBLE-STRUCK YEH').
unicode_script(0x1EEAB, 0x1EEBB, 'Arabic', 'Lo', 17, 'ARABIC MATHEMATICAL DOUBLE-STRUCK LAM..ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN').
unicode_script(0x1EEF0, 0x1EEF1, 'Arabic', 'Sm', 2, 'ARABIC MATHEMATICAL OPERATOR MEEM WITH HAH WITH TATWEEL..ARABIC MATHEMATICAL OPERATOR HAH WITH DAL').

% Total code points: 1234

% ================================================

unicode_script(0x0700, 0x070D, 'Syriac', 'Po', 14, 'SYRIAC END OF PARAGRAPH..SYRIAC HARKLEAN ASTERISCUS').
unicode_script(0x070F, 0x070F, 'Syriac', 'Cf', 1, 'SYRIAC ABBREVIATION MARK').
unicode_script(0x0710, 0x0710, 'Syriac', 'Lo', 1, ' SYRIAC LETTER ALAPH').
unicode_script(0x0711, 0x0711, 'Syriac', 'Mn', 1, 'SYRIAC LETTER SUPERSCRIPT ALAPH').
unicode_script(0x0712, 0x072F, 'Syriac', 'Lo', 30, 'SYRIAC LETTER BETH..SYRIAC LETTER PERSIAN DHALATH').
unicode_script(0x0730, 0x074A, 'Syriac', 'Mn', 27, 'SYRIAC PTHAHA ABOVE..SYRIAC BARREKH').
unicode_script(0x074D, 0x074F, 'Syriac', 'Lo', 3, 'SYRIAC LETTER SOGDIAN ZHAIN..SYRIAC LETTER SOGDIAN FE').

% Total code points: 77

% ================================================

unicode_script(0x0780, 0x07A5, 'Thaana', 'Lo', 38, 'THAANA LETTER HAA..THAANA LETTER WAAVU').
unicode_script(0x07A6, 0x07B0, 'Thaana', 'Mn', 11, 'THAANA ABAFILI..THAANA SUKUN').
unicode_script(0x07B1, 0x07B1, 'Thaana', 'Lo', 1, ' THAANA LETTER NAA').

% Total code points: 50

% ================================================

unicode_script(0x0900, 0x0902, 'Devanagari', 'Mn', 3, 'DEVANAGARI SIGN INVERTED CANDRABINDU..DEVANAGARI SIGN ANUSVARA').
unicode_script(0x0903, 0x0903, 'Devanagari', 'Mc', 1, 'DEVANAGARI SIGN VISARGA').
unicode_script(0x0904, 0x0939, 'Devanagari', 'Lo', 54, 'DEVANAGARI LETTER SHORT A..DEVANAGARI LETTER HA').
unicode_script(0x093A, 0x093A, 'Devanagari', 'Mn', 1, 'DEVANAGARI VOWEL SIGN OE').
unicode_script(0x093B, 0x093B, 'Devanagari', 'Mc', 1, 'DEVANAGARI VOWEL SIGN OOE').
unicode_script(0x093C, 0x093C, 'Devanagari', 'Mn', 1, 'DEVANAGARI SIGN NUKTA').
unicode_script(0x093D, 0x093D, 'Devanagari', 'Lo', 1, ' DEVANAGARI SIGN AVAGRAHA').
unicode_script(0x093E, 0x0940, 'Devanagari', 'Mc', 3, 'DEVANAGARI VOWEL SIGN AA..DEVANAGARI VOWEL SIGN II').
unicode_script(0x0941, 0x0948, 'Devanagari', 'Mn', 8, 'DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI').
unicode_script(0x0949, 0x094C, 'Devanagari', 'Mc', 4, 'DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU').
unicode_script(0x094D, 0x094D, 'Devanagari', 'Mn', 1, 'DEVANAGARI SIGN VIRAMA').
unicode_script(0x094E, 0x094F, 'Devanagari', 'Mc', 2, 'DEVANAGARI VOWEL SIGN PRISHTHAMATRA E..DEVANAGARI VOWEL SIGN AW').
unicode_script(0x0950, 0x0950, 'Devanagari', 'Lo', 1, ' DEVANAGARI OM').
unicode_script(0x0953, 0x0957, 'Devanagari', 'Mn', 5, 'DEVANAGARI GRAVE ACCENT..DEVANAGARI VOWEL SIGN UUE').
unicode_script(0x0958, 0x0961, 'Devanagari', 'Lo', 10, 'DEVANAGARI LETTER QA..DEVANAGARI LETTER VOCALIC LL').
unicode_script(0x0962, 0x0963, 'Devanagari', 'Mn', 2, 'DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL').
unicode_script(0x0966, 0x096F, 'Devanagari', 'Nd', 10, 'DEVANAGARI DIGIT ZERO..DEVANAGARI DIGIT NINE').
unicode_script(0x0970, 0x0970, 'Devanagari', 'Po', 1, 'DEVANAGARI ABBREVIATION SIGN').
unicode_script(0x0971, 0x0971, 'Devanagari', 'Lm', 1, 'DEVANAGARI SIGN HIGH SPACING DOT').
unicode_script(0x0972, 0x0977, 'Devanagari', 'Lo', 6, 'DEVANAGARI LETTER CANDRA A..DEVANAGARI LETTER UUE').
unicode_script(0x0979, 0x097F, 'Devanagari', 'Lo', 7, 'DEVANAGARI LETTER ZHA..DEVANAGARI LETTER BBA').
unicode_script(0xA8E0, 0xA8F1, 'Devanagari', 'Mn', 18, 'COMBINING DEVANAGARI DIGIT ZERO..COMBINING DEVANAGARI SIGN AVAGRAHA').
unicode_script(0xA8F2, 0xA8F7, 'Devanagari', 'Lo', 6, 'DEVANAGARI SIGN SPACING CANDRABINDU..DEVANAGARI SIGN CANDRABINDU AVAGRAHA').
unicode_script(0xA8F8, 0xA8FA, 'Devanagari', 'Po', 3, 'DEVANAGARI SIGN PUSHPIKA..DEVANAGARI CARET').
unicode_script(0xA8FB, 0xA8FB, 'Devanagari', 'Lo', 1, ' DEVANAGARI HEADSTROKE').

% Total code points: 151

% ================================================

unicode_script(0x0981, 0x0981, 'Bengali', 'Mn', 1, 'BENGALI SIGN CANDRABINDU').
unicode_script(0x0982, 0x0983, 'Bengali', 'Mc', 2, 'BENGALI SIGN ANUSVARA..BENGALI SIGN VISARGA').
unicode_script(0x0985, 0x098C, 'Bengali', 'Lo', 8, 'BENGALI LETTER A..BENGALI LETTER VOCALIC L').
unicode_script(0x098F, 0x0990, 'Bengali', 'Lo', 2, 'BENGALI LETTER E..BENGALI LETTER AI').
unicode_script(0x0993, 0x09A8, 'Bengali', 'Lo', 22, 'BENGALI LETTER O..BENGALI LETTER NA').
unicode_script(0x09AA, 0x09B0, 'Bengali', 'Lo', 7, 'BENGALI LETTER PA..BENGALI LETTER RA').
unicode_script(0x09B2, 0x09B2, 'Bengali', 'Lo', 1, ' BENGALI LETTER LA').
unicode_script(0x09B6, 0x09B9, 'Bengali', 'Lo', 4, 'BENGALI LETTER SHA..BENGALI LETTER HA').
unicode_script(0x09BC, 0x09BC, 'Bengali', 'Mn', 1, 'BENGALI SIGN NUKTA').
unicode_script(0x09BD, 0x09BD, 'Bengali', 'Lo', 1, ' BENGALI SIGN AVAGRAHA').
unicode_script(0x09BE, 0x09C0, 'Bengali', 'Mc', 3, 'BENGALI VOWEL SIGN AA..BENGALI VOWEL SIGN II').
unicode_script(0x09C1, 0x09C4, 'Bengali', 'Mn', 4, 'BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR').
unicode_script(0x09C7, 0x09C8, 'Bengali', 'Mc', 2, 'BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI').
unicode_script(0x09CB, 0x09CC, 'Bengali', 'Mc', 2, 'BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU').
unicode_script(0x09CD, 0x09CD, 'Bengali', 'Mn', 1, 'BENGALI SIGN VIRAMA').
unicode_script(0x09CE, 0x09CE, 'Bengali', 'Lo', 1, ' BENGALI LETTER KHANDA TA').
unicode_script(0x09D7, 0x09D7, 'Bengali', 'Mc', 1, 'BENGALI AU LENGTH MARK').
unicode_script(0x09DC, 0x09DD, 'Bengali', 'Lo', 2, 'BENGALI LETTER RRA..BENGALI LETTER RHA').
unicode_script(0x09DF, 0x09E1, 'Bengali', 'Lo', 3, 'BENGALI LETTER YYA..BENGALI LETTER VOCALIC LL').
unicode_script(0x09E2, 0x09E3, 'Bengali', 'Mn', 2, 'BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL').
unicode_script(0x09E6, 0x09EF, 'Bengali', 'Nd', 10, 'BENGALI DIGIT ZERO..BENGALI DIGIT NINE').
unicode_script(0x09F0, 0x09F1, 'Bengali', 'Lo', 2, 'BENGALI LETTER RA WITH MIDDLE DIAGONAL..BENGALI LETTER RA WITH LOWER DIAGONAL').
unicode_script(0x09F2, 0x09F3, 'Bengali', 'Sc', 2, 'BENGALI RUPEE MARK..BENGALI RUPEE SIGN').
unicode_script(0x09F4, 0x09F9, 'Bengali', 'No', 6, 'BENGALI CURRENCY NUMERATOR ONE..BENGALI CURRENCY DENOMINATOR SIXTEEN').
unicode_script(0x09FA, 0x09FA, 'Bengali', 'So', 1, 'BENGALI ISSHAR').
unicode_script(0x09FB, 0x09FB, 'Bengali', 'Sc', 1, 'BENGALI GANDA MARK').

% Total code points: 92

% ================================================

unicode_script(0x0A01, 0x0A02, 'Gurmukhi', 'Mn', 2, 'GURMUKHI SIGN ADAK BINDI..GURMUKHI SIGN BINDI').
unicode_script(0x0A03, 0x0A03, 'Gurmukhi', 'Mc', 1, 'GURMUKHI SIGN VISARGA').
unicode_script(0x0A05, 0x0A0A, 'Gurmukhi', 'Lo', 6, 'GURMUKHI LETTER A..GURMUKHI LETTER UU').
unicode_script(0x0A0F, 0x0A10, 'Gurmukhi', 'Lo', 2, 'GURMUKHI LETTER EE..GURMUKHI LETTER AI').
unicode_script(0x0A13, 0x0A28, 'Gurmukhi', 'Lo', 22, 'GURMUKHI LETTER OO..GURMUKHI LETTER NA').
unicode_script(0x0A2A, 0x0A30, 'Gurmukhi', 'Lo', 7, 'GURMUKHI LETTER PA..GURMUKHI LETTER RA').
unicode_script(0x0A32, 0x0A33, 'Gurmukhi', 'Lo', 2, 'GURMUKHI LETTER LA..GURMUKHI LETTER LLA').
unicode_script(0x0A35, 0x0A36, 'Gurmukhi', 'Lo', 2, 'GURMUKHI LETTER VA..GURMUKHI LETTER SHA').
unicode_script(0x0A38, 0x0A39, 'Gurmukhi', 'Lo', 2, 'GURMUKHI LETTER SA..GURMUKHI LETTER HA').
unicode_script(0x0A3C, 0x0A3C, 'Gurmukhi', 'Mn', 1, 'GURMUKHI SIGN NUKTA').
unicode_script(0x0A3E, 0x0A40, 'Gurmukhi', 'Mc', 3, 'GURMUKHI VOWEL SIGN AA..GURMUKHI VOWEL SIGN II').
unicode_script(0x0A41, 0x0A42, 'Gurmukhi', 'Mn', 2, 'GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU').
unicode_script(0x0A47, 0x0A48, 'Gurmukhi', 'Mn', 2, 'GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI').
unicode_script(0x0A4B, 0x0A4D, 'Gurmukhi', 'Mn', 3, 'GURMUKHI VOWEL SIGN OO..GURMUKHI SIGN VIRAMA').
unicode_script(0x0A51, 0x0A51, 'Gurmukhi', 'Mn', 1, 'GURMUKHI SIGN UDAAT').
unicode_script(0x0A59, 0x0A5C, 'Gurmukhi', 'Lo', 4, 'GURMUKHI LETTER KHHA..GURMUKHI LETTER RRA').
unicode_script(0x0A5E, 0x0A5E, 'Gurmukhi', 'Lo', 1, ' GURMUKHI LETTER FA').
unicode_script(0x0A66, 0x0A6F, 'Gurmukhi', 'Nd', 10, 'GURMUKHI DIGIT ZERO..GURMUKHI DIGIT NINE').
unicode_script(0x0A70, 0x0A71, 'Gurmukhi', 'Mn', 2, 'GURMUKHI TIPPI..GURMUKHI ADDAK').
unicode_script(0x0A72, 0x0A74, 'Gurmukhi', 'Lo', 3, 'GURMUKHI IRI..GURMUKHI EK ONKAR').
unicode_script(0x0A75, 0x0A75, 'Gurmukhi', 'Mn', 1, 'GURMUKHI SIGN YAKASH').

% Total code points: 79

% ================================================

unicode_script(0x0A81, 0x0A82, 'Gujarati', 'Mn', 2, 'GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA').
unicode_script(0x0A83, 0x0A83, 'Gujarati', 'Mc', 1, 'GUJARATI SIGN VISARGA').
unicode_script(0x0A85, 0x0A8D, 'Gujarati', 'Lo', 9, 'GUJARATI LETTER A..GUJARATI VOWEL CANDRA E').
unicode_script(0x0A8F, 0x0A91, 'Gujarati', 'Lo', 3, 'GUJARATI LETTER E..GUJARATI VOWEL CANDRA O').
unicode_script(0x0A93, 0x0AA8, 'Gujarati', 'Lo', 22, 'GUJARATI LETTER O..GUJARATI LETTER NA').
unicode_script(0x0AAA, 0x0AB0, 'Gujarati', 'Lo', 7, 'GUJARATI LETTER PA..GUJARATI LETTER RA').
unicode_script(0x0AB2, 0x0AB3, 'Gujarati', 'Lo', 2, 'GUJARATI LETTER LA..GUJARATI LETTER LLA').
unicode_script(0x0AB5, 0x0AB9, 'Gujarati', 'Lo', 5, 'GUJARATI LETTER VA..GUJARATI LETTER HA').
unicode_script(0x0ABC, 0x0ABC, 'Gujarati', 'Mn', 1, 'GUJARATI SIGN NUKTA').
unicode_script(0x0ABD, 0x0ABD, 'Gujarati', 'Lo', 1, ' GUJARATI SIGN AVAGRAHA').
unicode_script(0x0ABE, 0x0AC0, 'Gujarati', 'Mc', 3, 'GUJARATI VOWEL SIGN AA..GUJARATI VOWEL SIGN II').
unicode_script(0x0AC1, 0x0AC5, 'Gujarati', 'Mn', 5, 'GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E').
unicode_script(0x0AC7, 0x0AC8, 'Gujarati', 'Mn', 2, 'GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI').
unicode_script(0x0AC9, 0x0AC9, 'Gujarati', 'Mc', 1, 'GUJARATI VOWEL SIGN CANDRA O').
unicode_script(0x0ACB, 0x0ACC, 'Gujarati', 'Mc', 2, 'GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU').
unicode_script(0x0ACD, 0x0ACD, 'Gujarati', 'Mn', 1, 'GUJARATI SIGN VIRAMA').
unicode_script(0x0AD0, 0x0AD0, 'Gujarati', 'Lo', 1, ' GUJARATI OM').
unicode_script(0x0AE0, 0x0AE1, 'Gujarati', 'Lo', 2, 'GUJARATI LETTER VOCALIC RR..GUJARATI LETTER VOCALIC LL').
unicode_script(0x0AE2, 0x0AE3, 'Gujarati', 'Mn', 2, 'GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL').
unicode_script(0x0AE6, 0x0AEF, 'Gujarati', 'Nd', 10, 'GUJARATI DIGIT ZERO..GUJARATI DIGIT NINE').
unicode_script(0x0AF0, 0x0AF0, 'Gujarati', 'Po', 1, 'GUJARATI ABBREVIATION SIGN').
unicode_script(0x0AF1, 0x0AF1, 'Gujarati', 'Sc', 1, 'GUJARATI RUPEE SIGN').

% Total code points: 84

% ================================================

unicode_script(0x0B01, 0x0B01, 'Oriya', 'Mn', 1, 'ORIYA SIGN CANDRABINDU').
unicode_script(0x0B02, 0x0B03, 'Oriya', 'Mc', 2, 'ORIYA SIGN ANUSVARA..ORIYA SIGN VISARGA').
unicode_script(0x0B05, 0x0B0C, 'Oriya', 'Lo', 8, 'ORIYA LETTER A..ORIYA LETTER VOCALIC L').
unicode_script(0x0B0F, 0x0B10, 'Oriya', 'Lo', 2, 'ORIYA LETTER E..ORIYA LETTER AI').
unicode_script(0x0B13, 0x0B28, 'Oriya', 'Lo', 22, 'ORIYA LETTER O..ORIYA LETTER NA').
unicode_script(0x0B2A, 0x0B30, 'Oriya', 'Lo', 7, 'ORIYA LETTER PA..ORIYA LETTER RA').
unicode_script(0x0B32, 0x0B33, 'Oriya', 'Lo', 2, 'ORIYA LETTER LA..ORIYA LETTER LLA').
unicode_script(0x0B35, 0x0B39, 'Oriya', 'Lo', 5, 'ORIYA LETTER VA..ORIYA LETTER HA').
unicode_script(0x0B3C, 0x0B3C, 'Oriya', 'Mn', 1, 'ORIYA SIGN NUKTA').
unicode_script(0x0B3D, 0x0B3D, 'Oriya', 'Lo', 1, ' ORIYA SIGN AVAGRAHA').
unicode_script(0x0B3E, 0x0B3E, 'Oriya', 'Mc', 1, 'ORIYA VOWEL SIGN AA').
unicode_script(0x0B3F, 0x0B3F, 'Oriya', 'Mn', 1, 'ORIYA VOWEL SIGN I').
unicode_script(0x0B40, 0x0B40, 'Oriya', 'Mc', 1, 'ORIYA VOWEL SIGN II').
unicode_script(0x0B41, 0x0B44, 'Oriya', 'Mn', 4, 'ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR').
unicode_script(0x0B47, 0x0B48, 'Oriya', 'Mc', 2, 'ORIYA VOWEL SIGN E..ORIYA VOWEL SIGN AI').
unicode_script(0x0B4B, 0x0B4C, 'Oriya', 'Mc', 2, 'ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU').
unicode_script(0x0B4D, 0x0B4D, 'Oriya', 'Mn', 1, 'ORIYA SIGN VIRAMA').
unicode_script(0x0B56, 0x0B56, 'Oriya', 'Mn', 1, 'ORIYA AI LENGTH MARK').
unicode_script(0x0B57, 0x0B57, 'Oriya', 'Mc', 1, 'ORIYA AU LENGTH MARK').
unicode_script(0x0B5C, 0x0B5D, 'Oriya', 'Lo', 2, 'ORIYA LETTER RRA..ORIYA LETTER RHA').
unicode_script(0x0B5F, 0x0B61, 'Oriya', 'Lo', 3, 'ORIYA LETTER YYA..ORIYA LETTER VOCALIC LL').
unicode_script(0x0B62, 0x0B63, 'Oriya', 'Mn', 2, 'ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL').
unicode_script(0x0B66, 0x0B6F, 'Oriya', 'Nd', 10, 'ORIYA DIGIT ZERO..ORIYA DIGIT NINE').
unicode_script(0x0B70, 0x0B70, 'Oriya', 'So', 1, 'ORIYA ISSHAR').
unicode_script(0x0B71, 0x0B71, 'Oriya', 'Lo', 1, ' ORIYA LETTER WA').
unicode_script(0x0B72, 0x0B77, 'Oriya', 'No', 6, 'ORIYA FRACTION ONE QUARTER..ORIYA FRACTION THREE SIXTEENTHS').

% Total code points: 90

% ================================================

unicode_script(0x0B82, 0x0B82, 'Tamil', 'Mn', 1, 'TAMIL SIGN ANUSVARA').
unicode_script(0x0B83, 0x0B83, 'Tamil', 'Lo', 1, ' TAMIL SIGN VISARGA').
unicode_script(0x0B85, 0x0B8A, 'Tamil', 'Lo', 6, 'TAMIL LETTER A..TAMIL LETTER UU').
unicode_script(0x0B8E, 0x0B90, 'Tamil', 'Lo', 3, 'TAMIL LETTER E..TAMIL LETTER AI').
unicode_script(0x0B92, 0x0B95, 'Tamil', 'Lo', 4, 'TAMIL LETTER O..TAMIL LETTER KA').
unicode_script(0x0B99, 0x0B9A, 'Tamil', 'Lo', 2, 'TAMIL LETTER NGA..TAMIL LETTER CA').
unicode_script(0x0B9C, 0x0B9C, 'Tamil', 'Lo', 1, ' TAMIL LETTER JA').
unicode_script(0x0B9E, 0x0B9F, 'Tamil', 'Lo', 2, 'TAMIL LETTER NYA..TAMIL LETTER TTA').
unicode_script(0x0BA3, 0x0BA4, 'Tamil', 'Lo', 2, 'TAMIL LETTER NNA..TAMIL LETTER TA').
unicode_script(0x0BA8, 0x0BAA, 'Tamil', 'Lo', 3, 'TAMIL LETTER NA..TAMIL LETTER PA').
unicode_script(0x0BAE, 0x0BB9, 'Tamil', 'Lo', 12, 'TAMIL LETTER MA..TAMIL LETTER HA').
unicode_script(0x0BBE, 0x0BBF, 'Tamil', 'Mc', 2, 'TAMIL VOWEL SIGN AA..TAMIL VOWEL SIGN I').
unicode_script(0x0BC0, 0x0BC0, 'Tamil', 'Mn', 1, 'TAMIL VOWEL SIGN II').
unicode_script(0x0BC1, 0x0BC2, 'Tamil', 'Mc', 2, 'TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU').
unicode_script(0x0BC6, 0x0BC8, 'Tamil', 'Mc', 3, 'TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI').
unicode_script(0x0BCA, 0x0BCC, 'Tamil', 'Mc', 3, 'TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU').
unicode_script(0x0BCD, 0x0BCD, 'Tamil', 'Mn', 1, 'TAMIL SIGN VIRAMA').
unicode_script(0x0BD0, 0x0BD0, 'Tamil', 'Lo', 1, ' TAMIL OM').
unicode_script(0x0BD7, 0x0BD7, 'Tamil', 'Mc', 1, 'TAMIL AU LENGTH MARK').
unicode_script(0x0BE6, 0x0BEF, 'Tamil', 'Nd', 10, 'TAMIL DIGIT ZERO..TAMIL DIGIT NINE').
unicode_script(0x0BF0, 0x0BF2, 'Tamil', 'No', 3, 'TAMIL NUMBER TEN..TAMIL NUMBER ONE THOUSAND').
unicode_script(0x0BF3, 0x0BF8, 'Tamil', 'So', 6, 'TAMIL DAY SIGN..TAMIL AS ABOVE SIGN').
unicode_script(0x0BF9, 0x0BF9, 'Tamil', 'Sc', 1, 'TAMIL RUPEE SIGN').
unicode_script(0x0BFA, 0x0BFA, 'Tamil', 'So', 1, 'TAMIL NUMBER SIGN').

% Total code points: 72

% ================================================

unicode_script(0x0C01, 0x0C03, 'Telugu', 'Mc', 3, 'TELUGU SIGN CANDRABINDU..TELUGU SIGN VISARGA').
unicode_script(0x0C05, 0x0C0C, 'Telugu', 'Lo', 8, 'TELUGU LETTER A..TELUGU LETTER VOCALIC L').
unicode_script(0x0C0E, 0x0C10, 'Telugu', 'Lo', 3, 'TELUGU LETTER E..TELUGU LETTER AI').
unicode_script(0x0C12, 0x0C28, 'Telugu', 'Lo', 23, 'TELUGU LETTER O..TELUGU LETTER NA').
unicode_script(0x0C2A, 0x0C33, 'Telugu', 'Lo', 10, 'TELUGU LETTER PA..TELUGU LETTER LLA').
unicode_script(0x0C35, 0x0C39, 'Telugu', 'Lo', 5, 'TELUGU LETTER VA..TELUGU LETTER HA').
unicode_script(0x0C3D, 0x0C3D, 'Telugu', 'Lo', 1, ' TELUGU SIGN AVAGRAHA').
unicode_script(0x0C3E, 0x0C40, 'Telugu', 'Mn', 3, 'TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II').
unicode_script(0x0C41, 0x0C44, 'Telugu', 'Mc', 4, 'TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR').
unicode_script(0x0C46, 0x0C48, 'Telugu', 'Mn', 3, 'TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI').
unicode_script(0x0C4A, 0x0C4D, 'Telugu', 'Mn', 4, 'TELUGU VOWEL SIGN O..TELUGU SIGN VIRAMA').
unicode_script(0x0C55, 0x0C56, 'Telugu', 'Mn', 2, 'TELUGU LENGTH MARK..TELUGU AI LENGTH MARK').
unicode_script(0x0C58, 0x0C59, 'Telugu', 'Lo', 2, 'TELUGU LETTER TSA..TELUGU LETTER DZA').
unicode_script(0x0C60, 0x0C61, 'Telugu', 'Lo', 2, 'TELUGU LETTER VOCALIC RR..TELUGU LETTER VOCALIC LL').
unicode_script(0x0C62, 0x0C63, 'Telugu', 'Mn', 2, 'TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL').
unicode_script(0x0C66, 0x0C6F, 'Telugu', 'Nd', 10, 'TELUGU DIGIT ZERO..TELUGU DIGIT NINE').
unicode_script(0x0C78, 0x0C7E, 'Telugu', 'No', 7, 'TELUGU FRACTION DIGIT ZERO FOR ODD POWERS OF FOUR..TELUGU FRACTION DIGIT THREE FOR EVEN POWERS OF FOUR').
unicode_script(0x0C7F, 0x0C7F, 'Telugu', 'So', 1, 'TELUGU SIGN TUUMU').

% Total code points: 93

% ================================================

unicode_script(0x0C82, 0x0C83, 'Kannada', 'Mc', 2, 'KANNADA SIGN ANUSVARA..KANNADA SIGN VISARGA').
unicode_script(0x0C85, 0x0C8C, 'Kannada', 'Lo', 8, 'KANNADA LETTER A..KANNADA LETTER VOCALIC L').
unicode_script(0x0C8E, 0x0C90, 'Kannada', 'Lo', 3, 'KANNADA LETTER E..KANNADA LETTER AI').
unicode_script(0x0C92, 0x0CA8, 'Kannada', 'Lo', 23, 'KANNADA LETTER O..KANNADA LETTER NA').
unicode_script(0x0CAA, 0x0CB3, 'Kannada', 'Lo', 10, 'KANNADA LETTER PA..KANNADA LETTER LLA').
unicode_script(0x0CB5, 0x0CB9, 'Kannada', 'Lo', 5, 'KANNADA LETTER VA..KANNADA LETTER HA').
unicode_script(0x0CBC, 0x0CBC, 'Kannada', 'Mn', 1, 'KANNADA SIGN NUKTA').
unicode_script(0x0CBD, 0x0CBD, 'Kannada', 'Lo', 1, ' KANNADA SIGN AVAGRAHA').
unicode_script(0x0CBE, 0x0CBE, 'Kannada', 'Mc', 1, 'KANNADA VOWEL SIGN AA').
unicode_script(0x0CBF, 0x0CBF, 'Kannada', 'Mn', 1, 'KANNADA VOWEL SIGN I').
unicode_script(0x0CC0, 0x0CC4, 'Kannada', 'Mc', 5, 'KANNADA VOWEL SIGN II..KANNADA VOWEL SIGN VOCALIC RR').
unicode_script(0x0CC6, 0x0CC6, 'Kannada', 'Mn', 1, 'KANNADA VOWEL SIGN E').
unicode_script(0x0CC7, 0x0CC8, 'Kannada', 'Mc', 2, 'KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI').
unicode_script(0x0CCA, 0x0CCB, 'Kannada', 'Mc', 2, 'KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO').
unicode_script(0x0CCC, 0x0CCD, 'Kannada', 'Mn', 2, 'KANNADA VOWEL SIGN AU..KANNADA SIGN VIRAMA').
unicode_script(0x0CD5, 0x0CD6, 'Kannada', 'Mc', 2, 'KANNADA LENGTH MARK..KANNADA AI LENGTH MARK').
unicode_script(0x0CDE, 0x0CDE, 'Kannada', 'Lo', 1, ' KANNADA LETTER FA').
unicode_script(0x0CE0, 0x0CE1, 'Kannada', 'Lo', 2, 'KANNADA LETTER VOCALIC RR..KANNADA LETTER VOCALIC LL').
unicode_script(0x0CE2, 0x0CE3, 'Kannada', 'Mn', 2, 'KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL').
unicode_script(0x0CE6, 0x0CEF, 'Kannada', 'Nd', 10, 'KANNADA DIGIT ZERO..KANNADA DIGIT NINE').
unicode_script(0x0CF1, 0x0CF2, 'Kannada', 'Lo', 2, 'KANNADA SIGN JIHVAMULIYA..KANNADA SIGN UPADHMANIYA').

% Total code points: 86

% ================================================

unicode_script(0x0D02, 0x0D03, 'Malayalam', 'Mc', 2, 'MALAYALAM SIGN ANUSVARA..MALAYALAM SIGN VISARGA').
unicode_script(0x0D05, 0x0D0C, 'Malayalam', 'Lo', 8, 'MALAYALAM LETTER A..MALAYALAM LETTER VOCALIC L').
unicode_script(0x0D0E, 0x0D10, 'Malayalam', 'Lo', 3, 'MALAYALAM LETTER E..MALAYALAM LETTER AI').
unicode_script(0x0D12, 0x0D3A, 'Malayalam', 'Lo', 41, 'MALAYALAM LETTER O..MALAYALAM LETTER TTTA').
unicode_script(0x0D3D, 0x0D3D, 'Malayalam', 'Lo', 1, ' MALAYALAM SIGN AVAGRAHA').
unicode_script(0x0D3E, 0x0D40, 'Malayalam', 'Mc', 3, 'MALAYALAM VOWEL SIGN AA..MALAYALAM VOWEL SIGN II').
unicode_script(0x0D41, 0x0D44, 'Malayalam', 'Mn', 4, 'MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC RR').
unicode_script(0x0D46, 0x0D48, 'Malayalam', 'Mc', 3, 'MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI').
unicode_script(0x0D4A, 0x0D4C, 'Malayalam', 'Mc', 3, 'MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU').
unicode_script(0x0D4D, 0x0D4D, 'Malayalam', 'Mn', 1, 'MALAYALAM SIGN VIRAMA').
unicode_script(0x0D4E, 0x0D4E, 'Malayalam', 'Lo', 1, ' MALAYALAM LETTER DOT REPH').
unicode_script(0x0D57, 0x0D57, 'Malayalam', 'Mc', 1, 'MALAYALAM AU LENGTH MARK').
unicode_script(0x0D60, 0x0D61, 'Malayalam', 'Lo', 2, 'MALAYALAM LETTER VOCALIC RR..MALAYALAM LETTER VOCALIC LL').
unicode_script(0x0D62, 0x0D63, 'Malayalam', 'Mn', 2, 'MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL').
unicode_script(0x0D66, 0x0D6F, 'Malayalam', 'Nd', 10, 'MALAYALAM DIGIT ZERO..MALAYALAM DIGIT NINE').
unicode_script(0x0D70, 0x0D75, 'Malayalam', 'No', 6, 'MALAYALAM NUMBER TEN..MALAYALAM FRACTION THREE QUARTERS').
unicode_script(0x0D79, 0x0D79, 'Malayalam', 'So', 1, 'MALAYALAM DATE MARK').
unicode_script(0x0D7A, 0x0D7F, 'Malayalam', 'Lo', 6, 'MALAYALAM LETTER CHILLU NN..MALAYALAM LETTER CHILLU K').

% Total code points: 98

% ================================================

unicode_script(0x0D82, 0x0D83, 'Sinhala', 'Mc', 2, 'SINHALA SIGN ANUSVARAYA..SINHALA SIGN VISARGAYA').
unicode_script(0x0D85, 0x0D96, 'Sinhala', 'Lo', 18, 'SINHALA LETTER AYANNA..SINHALA LETTER AUYANNA').
unicode_script(0x0D9A, 0x0DB1, 'Sinhala', 'Lo', 24, 'SINHALA LETTER ALPAPRAANA KAYANNA..SINHALA LETTER DANTAJA NAYANNA').
unicode_script(0x0DB3, 0x0DBB, 'Sinhala', 'Lo', 9, 'SINHALA LETTER SANYAKA DAYANNA..SINHALA LETTER RAYANNA').
unicode_script(0x0DBD, 0x0DBD, 'Sinhala', 'Lo', 1, ' SINHALA LETTER DANTAJA LAYANNA').
unicode_script(0x0DC0, 0x0DC6, 'Sinhala', 'Lo', 7, 'SINHALA LETTER VAYANNA..SINHALA LETTER FAYANNA').
unicode_script(0x0DCA, 0x0DCA, 'Sinhala', 'Mn', 1, 'SINHALA SIGN AL-LAKUNA').
unicode_script(0x0DCF, 0x0DD1, 'Sinhala', 'Mc', 3, 'SINHALA VOWEL SIGN AELA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA').
unicode_script(0x0DD2, 0x0DD4, 'Sinhala', 'Mn', 3, 'SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA').
unicode_script(0x0DD6, 0x0DD6, 'Sinhala', 'Mn', 1, 'SINHALA VOWEL SIGN DIGA PAA-PILLA').
unicode_script(0x0DD8, 0x0DDF, 'Sinhala', 'Mc', 8, 'SINHALA VOWEL SIGN GAETTA-PILLA..SINHALA VOWEL SIGN GAYANUKITTA').
unicode_script(0x0DF2, 0x0DF3, 'Sinhala', 'Mc', 2, 'SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA').
unicode_script(0x0DF4, 0x0DF4, 'Sinhala', 'Po', 1, 'SINHALA PUNCTUATION KUNDDALIYA').

% Total code points: 80

% ================================================

unicode_script(0x0E01, 0x0E30, 'Thai', 'Lo', 48, 'THAI CHARACTER KO KAI..THAI CHARACTER SARA A').
unicode_script(0x0E31, 0x0E31, 'Thai', 'Mn', 1, 'THAI CHARACTER MAI HAN-AKAT').
unicode_script(0x0E32, 0x0E33, 'Thai', 'Lo', 2, 'THAI CHARACTER SARA AA..THAI CHARACTER SARA AM').
unicode_script(0x0E34, 0x0E3A, 'Thai', 'Mn', 7, 'THAI CHARACTER SARA I..THAI CHARACTER PHINTHU').
unicode_script(0x0E40, 0x0E45, 'Thai', 'Lo', 6, 'THAI CHARACTER SARA E..THAI CHARACTER LAKKHANGYAO').
unicode_script(0x0E46, 0x0E46, 'Thai', 'Lm', 1, 'THAI CHARACTER MAIYAMOK').
unicode_script(0x0E47, 0x0E4E, 'Thai', 'Mn', 8, 'THAI CHARACTER MAITAIKHU..THAI CHARACTER YAMAKKAN').
unicode_script(0x0E4F, 0x0E4F, 'Thai', 'Po', 1, 'THAI CHARACTER FONGMAN').
unicode_script(0x0E50, 0x0E59, 'Thai', 'Nd', 10, 'THAI DIGIT ZERO..THAI DIGIT NINE').
unicode_script(0x0E5A, 0x0E5B, 'Thai', 'Po', 2, 'THAI CHARACTER ANGKHANKHU..THAI CHARACTER KHOMUT').

% Total code points: 86

% ================================================

unicode_script(0x0E81, 0x0E82, 'Lao', 'Lo', 2, 'LAO LETTER KO..LAO LETTER KHO SUNG').
unicode_script(0x0E84, 0x0E84, 'Lao', 'Lo', 1, ' LAO LETTER KHO TAM').
unicode_script(0x0E87, 0x0E88, 'Lao', 'Lo', 2, 'LAO LETTER NGO..LAO LETTER CO').
unicode_script(0x0E8A, 0x0E8A, 'Lao', 'Lo', 1, ' LAO LETTER SO TAM').
unicode_script(0x0E8D, 0x0E8D, 'Lao', 'Lo', 1, ' LAO LETTER NYO').
unicode_script(0x0E94, 0x0E97, 'Lao', 'Lo', 4, 'LAO LETTER DO..LAO LETTER THO TAM').
unicode_script(0x0E99, 0x0E9F, 'Lao', 'Lo', 7, 'LAO LETTER NO..LAO LETTER FO SUNG').
unicode_script(0x0EA1, 0x0EA3, 'Lao', 'Lo', 3, 'LAO LETTER MO..LAO LETTER LO LING').
unicode_script(0x0EA5, 0x0EA5, 'Lao', 'Lo', 1, ' LAO LETTER LO LOOT').
unicode_script(0x0EA7, 0x0EA7, 'Lao', 'Lo', 1, ' LAO LETTER WO').
unicode_script(0x0EAA, 0x0EAB, 'Lao', 'Lo', 2, 'LAO LETTER SO SUNG..LAO LETTER HO SUNG').
unicode_script(0x0EAD, 0x0EB0, 'Lao', 'Lo', 4, 'LAO LETTER O..LAO VOWEL SIGN A').
unicode_script(0x0EB1, 0x0EB1, 'Lao', 'Mn', 1, 'LAO VOWEL SIGN MAI KAN').
unicode_script(0x0EB2, 0x0EB3, 'Lao', 'Lo', 2, 'LAO VOWEL SIGN AA..LAO VOWEL SIGN AM').
unicode_script(0x0EB4, 0x0EB9, 'Lao', 'Mn', 6, 'LAO VOWEL SIGN I..LAO VOWEL SIGN UU').
unicode_script(0x0EBB, 0x0EBC, 'Lao', 'Mn', 2, 'LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO').
unicode_script(0x0EBD, 0x0EBD, 'Lao', 'Lo', 1, ' LAO SEMIVOWEL SIGN NYO').
unicode_script(0x0EC0, 0x0EC4, 'Lao', 'Lo', 5, 'LAO VOWEL SIGN E..LAO VOWEL SIGN AI').
unicode_script(0x0EC6, 0x0EC6, 'Lao', 'Lm', 1, 'LAO KO LA').
unicode_script(0x0EC8, 0x0ECD, 'Lao', 'Mn', 6, 'LAO TONE MAI EK..LAO NIGGAHITA').
unicode_script(0x0ED0, 0x0ED9, 'Lao', 'Nd', 10, 'LAO DIGIT ZERO..LAO DIGIT NINE').
unicode_script(0x0EDC, 0x0EDF, 'Lao', 'Lo', 4, 'LAO HO NO..LAO LETTER KHMU NYO').

% Total code points: 67

% ================================================

unicode_script(0x0F00, 0x0F00, 'Tibetan', 'Lo', 1, ' TIBETAN SYLLABLE OM').
unicode_script(0x0F01, 0x0F03, 'Tibetan', 'So', 3, 'TIBETAN MARK GTER YIG MGO TRUNCATED A..TIBETAN MARK GTER YIG MGO -UM GTER TSHEG MA').
unicode_script(0x0F04, 0x0F12, 'Tibetan', 'Po', 15, 'TIBETAN MARK INITIAL YIG MGO MDUN MA..TIBETAN MARK RGYA GRAM SHAD').
unicode_script(0x0F13, 0x0F13, 'Tibetan', 'So', 1, 'TIBETAN MARK CARET -DZUD RTAGS ME LONG CAN').
unicode_script(0x0F14, 0x0F14, 'Tibetan', 'Po', 1, 'TIBETAN MARK GTER TSHEG').
unicode_script(0x0F15, 0x0F17, 'Tibetan', 'So', 3, 'TIBETAN LOGOTYPE SIGN CHAD RTAGS..TIBETAN ASTROLOGICAL SIGN SGRA GCAN -CHAR RTAGS').
unicode_script(0x0F18, 0x0F19, 'Tibetan', 'Mn', 2, 'TIBETAN ASTROLOGICAL SIGN -KHYUD PA..TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS').
unicode_script(0x0F1A, 0x0F1F, 'Tibetan', 'So', 6, 'TIBETAN SIGN RDEL DKAR GCIG..TIBETAN SIGN RDEL DKAR RDEL NAG').
unicode_script(0x0F20, 0x0F29, 'Tibetan', 'Nd', 10, 'TIBETAN DIGIT ZERO..TIBETAN DIGIT NINE').
unicode_script(0x0F2A, 0x0F33, 'Tibetan', 'No', 10, 'TIBETAN DIGIT HALF ONE..TIBETAN DIGIT HALF ZERO').
unicode_script(0x0F34, 0x0F34, 'Tibetan', 'So', 1, 'TIBETAN MARK BSDUS RTAGS').
unicode_script(0x0F35, 0x0F35, 'Tibetan', 'Mn', 1, 'TIBETAN MARK NGAS BZUNG NYI ZLA').
unicode_script(0x0F36, 0x0F36, 'Tibetan', 'So', 1, 'TIBETAN MARK CARET -DZUD RTAGS BZHI MIG CAN').
unicode_script(0x0F37, 0x0F37, 'Tibetan', 'Mn', 1, 'TIBETAN MARK NGAS BZUNG SGOR RTAGS').
unicode_script(0x0F38, 0x0F38, 'Tibetan', 'So', 1, 'TIBETAN MARK CHE MGO').
unicode_script(0x0F39, 0x0F39, 'Tibetan', 'Mn', 1, 'TIBETAN MARK TSA -PHRU').
unicode_script(0x0F3A, 0x0F3A, 'Tibetan', 'Ps', 1, 'TIBETAN MARK GUG RTAGS GYON').
unicode_script(0x0F3B, 0x0F3B, 'Tibetan', 'Pe', 1, 'TIBETAN MARK GUG RTAGS GYAS').
unicode_script(0x0F3C, 0x0F3C, 'Tibetan', 'Ps', 1, 'TIBETAN MARK ANG KHANG GYON').
unicode_script(0x0F3D, 0x0F3D, 'Tibetan', 'Pe', 1, 'TIBETAN MARK ANG KHANG GYAS').
unicode_script(0x0F3E, 0x0F3F, 'Tibetan', 'Mc', 2, 'TIBETAN SIGN YAR TSHES..TIBETAN SIGN MAR TSHES').
unicode_script(0x0F40, 0x0F47, 'Tibetan', 'Lo', 8, 'TIBETAN LETTER KA..TIBETAN LETTER JA').
unicode_script(0x0F49, 0x0F6C, 'Tibetan', 'Lo', 36, 'TIBETAN LETTER NYA..TIBETAN LETTER RRA').
unicode_script(0x0F71, 0x0F7E, 'Tibetan', 'Mn', 14, 'TIBETAN VOWEL SIGN AA..TIBETAN SIGN RJES SU NGA RO').
unicode_script(0x0F7F, 0x0F7F, 'Tibetan', 'Mc', 1, 'TIBETAN SIGN RNAM BCAD').
unicode_script(0x0F80, 0x0F84, 'Tibetan', 'Mn', 5, 'TIBETAN VOWEL SIGN REVERSED I..TIBETAN MARK HALANTA').
unicode_script(0x0F85, 0x0F85, 'Tibetan', 'Po', 1, 'TIBETAN MARK PALUTA').
unicode_script(0x0F86, 0x0F87, 'Tibetan', 'Mn', 2, 'TIBETAN SIGN LCI RTAGS..TIBETAN SIGN YANG RTAGS').
unicode_script(0x0F88, 0x0F8C, 'Tibetan', 'Lo', 5, 'TIBETAN SIGN LCE TSA CAN..TIBETAN SIGN INVERTED MCHU CAN').
unicode_script(0x0F8D, 0x0F97, 'Tibetan', 'Mn', 11, 'TIBETAN SUBJOINED SIGN LCE TSA CAN..TIBETAN SUBJOINED LETTER JA').
unicode_script(0x0F99, 0x0FBC, 'Tibetan', 'Mn', 36, 'TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA').
unicode_script(0x0FBE, 0x0FC5, 'Tibetan', 'So', 8, 'TIBETAN KU RU KHA..TIBETAN SYMBOL RDO RJE').
unicode_script(0x0FC6, 0x0FC6, 'Tibetan', 'Mn', 1, 'TIBETAN SYMBOL PADMA GDAN').
unicode_script(0x0FC7, 0x0FCC, 'Tibetan', 'So', 6, 'TIBETAN SYMBOL RDO RJE RGYA GRAM..TIBETAN SYMBOL NOR BU BZHI -KHYIL').
unicode_script(0x0FCE, 0x0FCF, 'Tibetan', 'So', 2, 'TIBETAN SIGN RDEL NAG RDEL DKAR..TIBETAN SIGN RDEL NAG GSUM').
unicode_script(0x0FD0, 0x0FD4, 'Tibetan', 'Po', 5, 'TIBETAN MARK BSKA- SHOG GI MGO RGYAN..TIBETAN MARK CLOSING BRDA RNYING YIG MGO SGAB MA').
unicode_script(0x0FD9, 0x0FDA, 'Tibetan', 'Po', 2, 'TIBETAN MARK LEADING MCHAN RTAGS..TIBETAN MARK TRAILING MCHAN RTAGS').

% Total code points: 207

% ================================================

unicode_script(0x1000, 0x102A, 'Myanmar', 'Lo', 43, 'MYANMAR LETTER KA..MYANMAR LETTER AU').
unicode_script(0x102B, 0x102C, 'Myanmar', 'Mc', 2, 'MYANMAR VOWEL SIGN TALL AA..MYANMAR VOWEL SIGN AA').
unicode_script(0x102D, 0x1030, 'Myanmar', 'Mn', 4, 'MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU').
unicode_script(0x1031, 0x1031, 'Myanmar', 'Mc', 1, 'MYANMAR VOWEL SIGN E').
unicode_script(0x1032, 0x1037, 'Myanmar', 'Mn', 6, 'MYANMAR VOWEL SIGN AI..MYANMAR SIGN DOT BELOW').
unicode_script(0x1038, 0x1038, 'Myanmar', 'Mc', 1, 'MYANMAR SIGN VISARGA').
unicode_script(0x1039, 0x103A, 'Myanmar', 'Mn', 2, 'MYANMAR SIGN VIRAMA..MYANMAR SIGN ASAT').
unicode_script(0x103B, 0x103C, 'Myanmar', 'Mc', 2, 'MYANMAR CONSONANT SIGN MEDIAL YA..MYANMAR CONSONANT SIGN MEDIAL RA').
unicode_script(0x103D, 0x103E, 'Myanmar', 'Mn', 2, 'MYANMAR CONSONANT SIGN MEDIAL WA..MYANMAR CONSONANT SIGN MEDIAL HA').
unicode_script(0x103F, 0x103F, 'Myanmar', 'Lo', 1, ' MYANMAR LETTER GREAT SA').
unicode_script(0x1040, 0x1049, 'Myanmar', 'Nd', 10, 'MYANMAR DIGIT ZERO..MYANMAR DIGIT NINE').
unicode_script(0x104A, 0x104F, 'Myanmar', 'Po', 6, 'MYANMAR SIGN LITTLE SECTION..MYANMAR SYMBOL GENITIVE').
unicode_script(0x1050, 0x1055, 'Myanmar', 'Lo', 6, 'MYANMAR LETTER SHA..MYANMAR LETTER VOCALIC LL').
unicode_script(0x1056, 0x1057, 'Myanmar', 'Mc', 2, 'MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR').
unicode_script(0x1058, 0x1059, 'Myanmar', 'Mn', 2, 'MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL').
unicode_script(0x105A, 0x105D, 'Myanmar', 'Lo', 4, 'MYANMAR LETTER MON NGA..MYANMAR LETTER MON BBE').
unicode_script(0x105E, 0x1060, 'Myanmar', 'Mn', 3, 'MYANMAR CONSONANT SIGN MON MEDIAL NA..MYANMAR CONSONANT SIGN MON MEDIAL LA').
unicode_script(0x1061, 0x1061, 'Myanmar', 'Lo', 1, ' MYANMAR LETTER SGAW KAREN SHA').
unicode_script(0x1062, 0x1064, 'Myanmar', 'Mc', 3, 'MYANMAR VOWEL SIGN SGAW KAREN EU..MYANMAR TONE MARK SGAW KAREN KE PHO').
unicode_script(0x1065, 0x1066, 'Myanmar', 'Lo', 2, 'MYANMAR LETTER WESTERN PWO KAREN THA..MYANMAR LETTER WESTERN PWO KAREN PWA').
unicode_script(0x1067, 0x106D, 'Myanmar', 'Mc', 7, 'MYANMAR VOWEL SIGN WESTERN PWO KAREN EU..MYANMAR SIGN WESTERN PWO KAREN TONE-5').
unicode_script(0x106E, 0x1070, 'Myanmar', 'Lo', 3, 'MYANMAR LETTER EASTERN PWO KAREN NNA..MYANMAR LETTER EASTERN PWO KAREN GHWA').
unicode_script(0x1071, 0x1074, 'Myanmar', 'Mn', 4, 'MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE').
unicode_script(0x1075, 0x1081, 'Myanmar', 'Lo', 13, 'MYANMAR LETTER SHAN KA..MYANMAR LETTER SHAN HA').
unicode_script(0x1082, 0x1082, 'Myanmar', 'Mn', 1, 'MYANMAR CONSONANT SIGN SHAN MEDIAL WA').
unicode_script(0x1083, 0x1084, 'Myanmar', 'Mc', 2, 'MYANMAR VOWEL SIGN SHAN AA..MYANMAR VOWEL SIGN SHAN E').
unicode_script(0x1085, 0x1086, 'Myanmar', 'Mn', 2, 'MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y').
unicode_script(0x1087, 0x108C, 'Myanmar', 'Mc', 6, 'MYANMAR SIGN SHAN TONE-2..MYANMAR SIGN SHAN COUNCIL TONE-3').
unicode_script(0x108D, 0x108D, 'Myanmar', 'Mn', 1, 'MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE').
unicode_script(0x108E, 0x108E, 'Myanmar', 'Lo', 1, ' MYANMAR LETTER RUMAI PALAUNG FA').
unicode_script(0x108F, 0x108F, 'Myanmar', 'Mc', 1, 'MYANMAR SIGN RUMAI PALAUNG TONE-5').
unicode_script(0x1090, 0x1099, 'Myanmar', 'Nd', 10, 'MYANMAR SHAN DIGIT ZERO..MYANMAR SHAN DIGIT NINE').
unicode_script(0x109A, 0x109C, 'Myanmar', 'Mc', 3, 'MYANMAR SIGN KHAMTI TONE-1..MYANMAR VOWEL SIGN AITON A').
unicode_script(0x109D, 0x109D, 'Myanmar', 'Mn', 1, 'MYANMAR VOWEL SIGN AITON AI').
unicode_script(0x109E, 0x109F, 'Myanmar', 'So', 2, 'MYANMAR SYMBOL SHAN ONE..MYANMAR SYMBOL SHAN EXCLAMATION').
unicode_script(0xAA60, 0xAA6F, 'Myanmar', 'Lo', 16, 'MYANMAR LETTER KHAMTI GA..MYANMAR LETTER KHAMTI FA').
unicode_script(0xAA70, 0xAA70, 'Myanmar', 'Lm', 1, 'MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION').
unicode_script(0xAA71, 0xAA76, 'Myanmar', 'Lo', 6, 'MYANMAR LETTER KHAMTI XA..MYANMAR LOGOGRAM KHAMTI HM').
unicode_script(0xAA77, 0xAA79, 'Myanmar', 'So', 3, 'MYANMAR SYMBOL AITON EXCLAMATION..MYANMAR SYMBOL AITON TWO').
unicode_script(0xAA7A, 0xAA7A, 'Myanmar', 'Lo', 1, ' MYANMAR LETTER AITON RA').
unicode_script(0xAA7B, 0xAA7B, 'Myanmar', 'Mc', 1, 'MYANMAR SIGN PAO KAREN TONE').

% Total code points: 188

% ================================================

unicode_script(0x10A0, 0x10C5, 'Georgian', 'L&', 38, 'GEORGIAN CAPITAL LETTER AN..GEORGIAN CAPITAL LETTER HOE').
unicode_script(0x10C7, 0x10C7, 'Georgian', 'L&', 1, 'GEORGIAN CAPITAL LETTER YN').
unicode_script(0x10CD, 0x10CD, 'Georgian', 'L&', 1, 'GEORGIAN CAPITAL LETTER AEN').
unicode_script(0x10D0, 0x10FA, 'Georgian', 'Lo', 43, 'GEORGIAN LETTER AN..GEORGIAN LETTER AIN').
unicode_script(0x10FC, 0x10FC, 'Georgian', 'Lm', 1, 'MODIFIER LETTER GEORGIAN NAR').
unicode_script(0x10FD, 0x10FF, 'Georgian', 'Lo', 3, 'GEORGIAN LETTER AEN..GEORGIAN LETTER LABIAL SIGN').
unicode_script(0x2D00, 0x2D25, 'Georgian', 'L&', 38, 'GEORGIAN SMALL LETTER AN..GEORGIAN SMALL LETTER HOE').
unicode_script(0x2D27, 0x2D27, 'Georgian', 'L&', 1, 'GEORGIAN SMALL LETTER YN').
unicode_script(0x2D2D, 0x2D2D, 'Georgian', 'L&', 1, 'GEORGIAN SMALL LETTER AEN').

% Total code points: 127

% ================================================

unicode_script(0x1100, 0x11FF, 'Hangul', 'Lo', 256, 'HANGUL CHOSEONG KIYEOK..HANGUL JONGSEONG SSANGNIEUN').
unicode_script(0x302E, 0x302F, 'Hangul', 'Mc', 2, 'HANGUL SINGLE DOT TONE MARK..HANGUL DOUBLE DOT TONE MARK').
unicode_script(0x3131, 0x318E, 'Hangul', 'Lo', 94, 'HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE').
unicode_script(0x3200, 0x321E, 'Hangul', 'So', 31, 'PARENTHESIZED HANGUL KIYEOK..PARENTHESIZED KOREAN CHARACTER O HU').
unicode_script(0x3260, 0x327E, 'Hangul', 'So', 31, 'CIRCLED HANGUL KIYEOK..CIRCLED HANGUL IEUNG U').
unicode_script(0xA960, 0xA97C, 'Hangul', 'Lo', 29, 'HANGUL CHOSEONG TIKEUT-MIEUM..HANGUL CHOSEONG SSANGYEORINHIEUH').
unicode_script(0xAC00, 0xD7A3, 'Hangul', 'Lo', 11172, 'HANGUL SYLLABLE GA..HANGUL SYLLABLE HIH').
unicode_script(0xD7B0, 0xD7C6, 'Hangul', 'Lo', 23, 'HANGUL JUNGSEONG O-YEO..HANGUL JUNGSEONG ARAEA-E').
unicode_script(0xD7CB, 0xD7FB, 'Hangul', 'Lo', 49, 'HANGUL JONGSEONG NIEUN-RIEUL..HANGUL JONGSEONG PHIEUPH-THIEUTH').
unicode_script(0xFFA0, 0xFFBE, 'Hangul', 'Lo', 31, 'HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH').
unicode_script(0xFFC2, 0xFFC7, 'Hangul', 'Lo', 6, 'HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E').
unicode_script(0xFFCA, 0xFFCF, 'Hangul', 'Lo', 6, 'HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE').
unicode_script(0xFFD2, 0xFFD7, 'Hangul', 'Lo', 6, 'HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU').
unicode_script(0xFFDA, 0xFFDC, 'Hangul', 'Lo', 3, 'HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I').

% Total code points: 11739

% ================================================

unicode_script(0x1200, 0x1248, 'Ethiopic', 'Lo', 73, 'ETHIOPIC SYLLABLE HA..ETHIOPIC SYLLABLE QWA').
unicode_script(0x124A, 0x124D, 'Ethiopic', 'Lo', 4, 'ETHIOPIC SYLLABLE QWI..ETHIOPIC SYLLABLE QWE').
unicode_script(0x1250, 0x1256, 'Ethiopic', 'Lo', 7, 'ETHIOPIC SYLLABLE QHA..ETHIOPIC SYLLABLE QHO').
unicode_script(0x1258, 0x1258, 'Ethiopic', 'Lo', 1, ' ETHIOPIC SYLLABLE QHWA').
unicode_script(0x125A, 0x125D, 'Ethiopic', 'Lo', 4, 'ETHIOPIC SYLLABLE QHWI..ETHIOPIC SYLLABLE QHWE').
unicode_script(0x1260, 0x1288, 'Ethiopic', 'Lo', 41, 'ETHIOPIC SYLLABLE BA..ETHIOPIC SYLLABLE XWA').
unicode_script(0x128A, 0x128D, 'Ethiopic', 'Lo', 4, 'ETHIOPIC SYLLABLE XWI..ETHIOPIC SYLLABLE XWE').
unicode_script(0x1290, 0x12B0, 'Ethiopic', 'Lo', 33, 'ETHIOPIC SYLLABLE NA..ETHIOPIC SYLLABLE KWA').
unicode_script(0x12B2, 0x12B5, 'Ethiopic', 'Lo', 4, 'ETHIOPIC SYLLABLE KWI..ETHIOPIC SYLLABLE KWE').
unicode_script(0x12B8, 0x12BE, 'Ethiopic', 'Lo', 7, 'ETHIOPIC SYLLABLE KXA..ETHIOPIC SYLLABLE KXO').
unicode_script(0x12C0, 0x12C0, 'Ethiopic', 'Lo', 1, ' ETHIOPIC SYLLABLE KXWA').
unicode_script(0x12C2, 0x12C5, 'Ethiopic', 'Lo', 4, 'ETHIOPIC SYLLABLE KXWI..ETHIOPIC SYLLABLE KXWE').
unicode_script(0x12C8, 0x12D6, 'Ethiopic', 'Lo', 15, 'ETHIOPIC SYLLABLE WA..ETHIOPIC SYLLABLE PHARYNGEAL O').
unicode_script(0x12D8, 0x1310, 'Ethiopic', 'Lo', 57, 'ETHIOPIC SYLLABLE ZA..ETHIOPIC SYLLABLE GWA').
unicode_script(0x1312, 0x1315, 'Ethiopic', 'Lo', 4, 'ETHIOPIC SYLLABLE GWI..ETHIOPIC SYLLABLE GWE').
unicode_script(0x1318, 0x135A, 'Ethiopic', 'Lo', 67, 'ETHIOPIC SYLLABLE GGA..ETHIOPIC SYLLABLE FYA').
unicode_script(0x135D, 0x135F, 'Ethiopic', 'Mn', 3, 'ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK..ETHIOPIC COMBINING GEMINATION MARK').
unicode_script(0x1360, 0x1368, 'Ethiopic', 'Po', 9, 'ETHIOPIC SECTION MARK..ETHIOPIC PARAGRAPH SEPARATOR').
unicode_script(0x1369, 0x137C, 'Ethiopic', 'No', 20, 'ETHIOPIC DIGIT ONE..ETHIOPIC NUMBER TEN THOUSAND').
unicode_script(0x1380, 0x138F, 'Ethiopic', 'Lo', 16, 'ETHIOPIC SYLLABLE SEBATBEIT MWA..ETHIOPIC SYLLABLE PWE').
unicode_script(0x1390, 0x1399, 'Ethiopic', 'So', 10, 'ETHIOPIC TONAL MARK YIZET..ETHIOPIC TONAL MARK KURT').
unicode_script(0x2D80, 0x2D96, 'Ethiopic', 'Lo', 23, 'ETHIOPIC SYLLABLE LOA..ETHIOPIC SYLLABLE GGWE').
unicode_script(0x2DA0, 0x2DA6, 'Ethiopic', 'Lo', 7, 'ETHIOPIC SYLLABLE SSA..ETHIOPIC SYLLABLE SSO').
unicode_script(0x2DA8, 0x2DAE, 'Ethiopic', 'Lo', 7, 'ETHIOPIC SYLLABLE CCA..ETHIOPIC SYLLABLE CCO').
unicode_script(0x2DB0, 0x2DB6, 'Ethiopic', 'Lo', 7, 'ETHIOPIC SYLLABLE ZZA..ETHIOPIC SYLLABLE ZZO').
unicode_script(0x2DB8, 0x2DBE, 'Ethiopic', 'Lo', 7, 'ETHIOPIC SYLLABLE CCHA..ETHIOPIC SYLLABLE CCHO').
unicode_script(0x2DC0, 0x2DC6, 'Ethiopic', 'Lo', 7, 'ETHIOPIC SYLLABLE QYA..ETHIOPIC SYLLABLE QYO').
unicode_script(0x2DC8, 0x2DCE, 'Ethiopic', 'Lo', 7, 'ETHIOPIC SYLLABLE KYA..ETHIOPIC SYLLABLE KYO').
unicode_script(0x2DD0, 0x2DD6, 'Ethiopic', 'Lo', 7, 'ETHIOPIC SYLLABLE XYA..ETHIOPIC SYLLABLE XYO').
unicode_script(0x2DD8, 0x2DDE, 'Ethiopic', 'Lo', 7, 'ETHIOPIC SYLLABLE GYA..ETHIOPIC SYLLABLE GYO').
unicode_script(0xAB01, 0xAB06, 'Ethiopic', 'Lo', 6, 'ETHIOPIC SYLLABLE TTHU..ETHIOPIC SYLLABLE TTHO').
unicode_script(0xAB09, 0xAB0E, 'Ethiopic', 'Lo', 6, 'ETHIOPIC SYLLABLE DDHU..ETHIOPIC SYLLABLE DDHO').
unicode_script(0xAB11, 0xAB16, 'Ethiopic', 'Lo', 6, 'ETHIOPIC SYLLABLE DZU..ETHIOPIC SYLLABLE DZO').
unicode_script(0xAB20, 0xAB26, 'Ethiopic', 'Lo', 7, 'ETHIOPIC SYLLABLE CCHHA..ETHIOPIC SYLLABLE CCHHO').
unicode_script(0xAB28, 0xAB2E, 'Ethiopic', 'Lo', 7, 'ETHIOPIC SYLLABLE BBA..ETHIOPIC SYLLABLE BBO').

% Total code points: 495

% ================================================

unicode_script(0x13A0, 0x13F4, 'Cherokee', 'Lo', 85, 'CHEROKEE LETTER A..CHEROKEE LETTER YV').

% Total code points: 85

% ================================================

unicode_script(0x1400, 0x1400, 'Canadian_Aboriginal', 'Pd', 1, 'CANADIAN SYLLABICS HYPHEN').
unicode_script(0x1401, 0x166C, 'Canadian_Aboriginal', 'Lo', 620, 'CANADIAN SYLLABICS E..CANADIAN SYLLABICS CARRIER TTSA').
unicode_script(0x166D, 0x166E, 'Canadian_Aboriginal', 'Po', 2, 'CANADIAN SYLLABICS CHI SIGN..CANADIAN SYLLABICS FULL STOP').
unicode_script(0x166F, 0x167F, 'Canadian_Aboriginal', 'Lo', 17, 'CANADIAN SYLLABICS QAI..CANADIAN SYLLABICS BLACKFOOT W').
unicode_script(0x18B0, 0x18F5, 'Canadian_Aboriginal', 'Lo', 70, 'CANADIAN SYLLABICS OY..CANADIAN SYLLABICS CARRIER DENTAL S').

% Total code points: 710

% ================================================

unicode_script(0x1680, 0x1680, 'Ogham', 'Zs', 1, 'OGHAM SPACE MARK').
unicode_script(0x1681, 0x169A, 'Ogham', 'Lo', 26, 'OGHAM LETTER BEITH..OGHAM LETTER PEITH').
unicode_script(0x169B, 0x169B, 'Ogham', 'Ps', 1, 'OGHAM FEATHER MARK').
unicode_script(0x169C, 0x169C, 'Ogham', 'Pe', 1, 'OGHAM REVERSED FEATHER MARK').

% Total code points: 29

% ================================================

unicode_script(0x16A0, 0x16EA, 'Runic', 'Lo', 75, 'RUNIC LETTER FEHU FEOH FE F..RUNIC LETTER X').
unicode_script(0x16EE, 0x16F0, 'Runic', 'Nl', 3, 'RUNIC ARLAUG SYMBOL..RUNIC BELGTHOR SYMBOL').

% Total code points: 78

% ================================================

unicode_script(0x1780, 0x17B3, 'Khmer', 'Lo', 52, 'KHMER LETTER KA..KHMER INDEPENDENT VOWEL QAU').
unicode_script(0x17B4, 0x17B5, 'Khmer', 'Mn', 2, 'KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA').
unicode_script(0x17B6, 0x17B6, 'Khmer', 'Mc', 1, 'KHMER VOWEL SIGN AA').
unicode_script(0x17B7, 0x17BD, 'Khmer', 'Mn', 7, 'KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA').
unicode_script(0x17BE, 0x17C5, 'Khmer', 'Mc', 8, 'KHMER VOWEL SIGN OE..KHMER VOWEL SIGN AU').
unicode_script(0x17C6, 0x17C6, 'Khmer', 'Mn', 1, 'KHMER SIGN NIKAHIT').
unicode_script(0x17C7, 0x17C8, 'Khmer', 'Mc', 2, 'KHMER SIGN REAHMUK..KHMER SIGN YUUKALEAPINTU').
unicode_script(0x17C9, 0x17D3, 'Khmer', 'Mn', 11, 'KHMER SIGN MUUSIKATOAN..KHMER SIGN BATHAMASAT').
unicode_script(0x17D4, 0x17D6, 'Khmer', 'Po', 3, 'KHMER SIGN KHAN..KHMER SIGN CAMNUC PII KUUH').
unicode_script(0x17D7, 0x17D7, 'Khmer', 'Lm', 1, 'KHMER SIGN LEK TOO').
unicode_script(0x17D8, 0x17DA, 'Khmer', 'Po', 3, 'KHMER SIGN BEYYAL..KHMER SIGN KOOMUUT').
unicode_script(0x17DB, 0x17DB, 'Khmer', 'Sc', 1, 'KHMER CURRENCY SYMBOL RIEL').
unicode_script(0x17DC, 0x17DC, 'Khmer', 'Lo', 1, ' KHMER SIGN AVAKRAHASANYA').
unicode_script(0x17DD, 0x17DD, 'Khmer', 'Mn', 1, 'KHMER SIGN ATTHACAN').
unicode_script(0x17E0, 0x17E9, 'Khmer', 'Nd', 10, 'KHMER DIGIT ZERO..KHMER DIGIT NINE').
unicode_script(0x17F0, 0x17F9, 'Khmer', 'No', 10, 'KHMER SYMBOL LEK ATTAK SON..KHMER SYMBOL LEK ATTAK PRAM-BUON').
unicode_script(0x19E0, 0x19FF, 'Khmer', 'So', 32, 'KHMER SYMBOL PATHAMASAT..KHMER SYMBOL DAP-PRAM ROC').

% Total code points: 146

% ================================================

unicode_script(0x1800, 0x1801, 'Mongolian', 'Po', 2, 'MONGOLIAN BIRGA..MONGOLIAN ELLIPSIS').
unicode_script(0x1804, 0x1804, 'Mongolian', 'Po', 1, 'MONGOLIAN COLON').
unicode_script(0x1806, 0x1806, 'Mongolian', 'Pd', 1, 'MONGOLIAN TODO SOFT HYPHEN').
unicode_script(0x1807, 0x180A, 'Mongolian', 'Po', 4, 'MONGOLIAN SIBE SYLLABLE BOUNDARY MARKER..MONGOLIAN NIRUGU').
unicode_script(0x180B, 0x180D, 'Mongolian', 'Mn', 3, 'MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE').
unicode_script(0x180E, 0x180E, 'Mongolian', 'Zs', 1, 'MONGOLIAN VOWEL SEPARATOR').
unicode_script(0x1810, 0x1819, 'Mongolian', 'Nd', 10, 'MONGOLIAN DIGIT ZERO..MONGOLIAN DIGIT NINE').
unicode_script(0x1820, 0x1842, 'Mongolian', 'Lo', 35, 'MONGOLIAN LETTER A..MONGOLIAN LETTER CHI').
unicode_script(0x1843, 0x1843, 'Mongolian', 'Lm', 1, 'MONGOLIAN LETTER TODO LONG VOWEL SIGN').
unicode_script(0x1844, 0x1877, 'Mongolian', 'Lo', 52, 'MONGOLIAN LETTER TODO E..MONGOLIAN LETTER MANCHU ZHA').
unicode_script(0x1880, 0x18A8, 'Mongolian', 'Lo', 41, 'MONGOLIAN LETTER ALI GALI ANUSVARA ONE..MONGOLIAN LETTER MANCHU ALI GALI BHA').
unicode_script(0x18A9, 0x18A9, 'Mongolian', 'Mn', 1, 'MONGOLIAN LETTER ALI GALI DAGALGA').
unicode_script(0x18AA, 0x18AA, 'Mongolian', 'Lo', 1, ' MONGOLIAN LETTER MANCHU ALI GALI LHA').

% Total code points: 153

% ================================================

unicode_script(0x3041, 0x3096, 'Hiragana', 'Lo', 86, 'HIRAGANA LETTER SMALL A..HIRAGANA LETTER SMALL KE').
unicode_script(0x309D, 0x309E, 'Hiragana', 'Lm', 2, 'HIRAGANA ITERATION MARK..HIRAGANA VOICED ITERATION MARK').
unicode_script(0x309F, 0x309F, 'Hiragana', 'Lo', 1, ' HIRAGANA DIGRAPH YORI').
unicode_script(0x1B001, 0x1B001, 'Hiragana', 'Lo', 1, ' HIRAGANA LETTER ARCHAIC YE').
unicode_script(0x1F200, 0x1F200, 'Hiragana', 'So', 1, 'SQUARE HIRAGANA HOKA').

% Total code points: 91

% ================================================

unicode_script(0x30A1, 0x30FA, 'Katakana', 'Lo', 90, 'KATAKANA LETTER SMALL A..KATAKANA LETTER VO').
unicode_script(0x30FD, 0x30FE, 'Katakana', 'Lm', 2, 'KATAKANA ITERATION MARK..KATAKANA VOICED ITERATION MARK').
unicode_script(0x30FF, 0x30FF, 'Katakana', 'Lo', 1, ' KATAKANA DIGRAPH KOTO').
unicode_script(0x31F0, 0x31FF, 'Katakana', 'Lo', 16, 'KATAKANA LETTER SMALL KU..KATAKANA LETTER SMALL RO').
unicode_script(0x32D0, 0x32FE, 'Katakana', 'So', 47, 'CIRCLED KATAKANA A..CIRCLED KATAKANA WO').
unicode_script(0x3300, 0x3357, 'Katakana', 'So', 88, 'SQUARE APAATO..SQUARE WATTO').
unicode_script(0xFF66, 0xFF6F, 'Katakana', 'Lo', 10, 'HALFWIDTH KATAKANA LETTER WO..HALFWIDTH KATAKANA LETTER SMALL TU').
unicode_script(0xFF71, 0xFF9D, 'Katakana', 'Lo', 45, 'HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N').
unicode_script(0x1B000, 0x1B000, 'Katakana', 'Lo', 1, ' KATAKANA LETTER ARCHAIC E').

% Total code points: 300

% ================================================

unicode_script(0x02EA, 0x02EB, 'Bopomofo', 'Sk', 2, 'MODIFIER LETTER YIN DEPARTING TONE MARK..MODIFIER LETTER YANG DEPARTING TONE MARK').
unicode_script(0x3105, 0x312D, 'Bopomofo', 'Lo', 41, 'BOPOMOFO LETTER B..BOPOMOFO LETTER IH').
unicode_script(0x31A0, 0x31BA, 'Bopomofo', 'Lo', 27, 'BOPOMOFO LETTER BU..BOPOMOFO LETTER ZY').

% Total code points: 70

% ================================================

unicode_script(0x2E80, 0x2E99, 'Han', 'So', 26, 'CJK RADICAL REPEAT..CJK RADICAL RAP').
unicode_script(0x2E9B, 0x2EF3, 'Han', 'So', 89, 'CJK RADICAL CHOKE..CJK RADICAL C-SIMPLIFIED TURTLE').
unicode_script(0x2F00, 0x2FD5, 'Han', 'So', 214, 'KANGXI RADICAL ONE..KANGXI RADICAL FLUTE').
unicode_script(0x3005, 0x3005, 'Han', 'Lm', 1, 'IDEOGRAPHIC ITERATION MARK').
unicode_script(0x3007, 0x3007, 'Han', 'Nl', 1, 'IDEOGRAPHIC NUMBER ZERO').
unicode_script(0x3021, 0x3029, 'Han', 'Nl', 9, 'HANGZHOU NUMERAL ONE..HANGZHOU NUMERAL NINE').
unicode_script(0x3038, 0x303A, 'Han', 'Nl', 3, 'HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY').
unicode_script(0x303B, 0x303B, 'Han', 'Lm', 1, 'VERTICAL IDEOGRAPHIC ITERATION MARK').
unicode_script(0x3400, 0x4DB5, 'Han', 'Lo', 6582, 'CJK UNIFIED IDEOGRAPH-3400..CJK UNIFIED IDEOGRAPH-4DB5').
unicode_script(0x4E00, 0x9FCC, 'Han', 'Lo', 20941, 'CJK UNIFIED IDEOGRAPH-4E00..CJK UNIFIED IDEOGRAPH-9FCC').
unicode_script(0xF900, 0xFA6D, 'Han', 'Lo', 366, 'CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA6D').
unicode_script(0xFA70, 0xFAD9, 'Han', 'Lo', 106, 'CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9').
unicode_script(0x20000, 0x2A6D6, 'Han', 'Lo', 42711, 'CJK UNIFIED IDEOGRAPH-20000..CJK UNIFIED IDEOGRAPH-2A6D6').
unicode_script(0x2A700, 0x2B734, 'Han', 'Lo', 4149, 'CJK UNIFIED IDEOGRAPH-2A700..CJK UNIFIED IDEOGRAPH-2B734').
unicode_script(0x2B740, 0x2B81D, 'Han', 'Lo', 222, 'CJK UNIFIED IDEOGRAPH-2B740..CJK UNIFIED IDEOGRAPH-2B81D').
unicode_script(0x2F800, 0x2FA1D, 'Han', 'Lo', 542, 'CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D').

% Total code points: 75963

% ================================================

unicode_script(0xA000, 0xA014, 'Yi', 'Lo', 21, 'YI SYLLABLE IT..YI SYLLABLE E').
unicode_script(0xA015, 0xA015, 'Yi', 'Lm', 1, 'YI SYLLABLE WU').
unicode_script(0xA016, 0xA48C, 'Yi', 'Lo', 1143, 'YI SYLLABLE BIT..YI SYLLABLE YYR').
unicode_script(0xA490, 0xA4C6, 'Yi', 'So', 55, 'YI RADICAL QOT..YI RADICAL KE').

% Total code points: 1220

% ================================================

unicode_script(0x10300, 0x1031E, 'Old_Italic', 'Lo', 31, 'OLD ITALIC LETTER A..OLD ITALIC LETTER UU').
unicode_script(0x10320, 0x10323, 'Old_Italic', 'No', 4, 'OLD ITALIC NUMERAL ONE..OLD ITALIC NUMERAL FIFTY').

% Total code points: 35

% ================================================

unicode_script(0x10330, 0x10340, 'Gothic', 'Lo', 17, 'GOTHIC LETTER AHSA..GOTHIC LETTER PAIRTHRA').
unicode_script(0x10341, 0x10341, 'Gothic', 'Nl', 1, 'GOTHIC LETTER NINETY').
unicode_script(0x10342, 0x10349, 'Gothic', 'Lo', 8, 'GOTHIC LETTER RAIDA..GOTHIC LETTER OTHAL').
unicode_script(0x1034A, 0x1034A, 'Gothic', 'Nl', 1, 'GOTHIC LETTER NINE HUNDRED').

% Total code points: 27

% ================================================

unicode_script(0x10400, 0x1044F, 'Deseret', 'L&', 80, 'DESERET CAPITAL LETTER LONG I..DESERET SMALL LETTER EW').

% Total code points: 80

% ================================================

unicode_script(0x0300, 0x036F, 'Inherited', 'Mn', 112, 'COMBINING GRAVE ACCENT..COMBINING LATIN SMALL LETTER X').
unicode_script(0x0485, 0x0486, 'Inherited', 'Mn', 2, 'COMBINING CYRILLIC DASIA PNEUMATA..COMBINING CYRILLIC PSILI PNEUMATA').
unicode_script(0x064B, 0x0655, 'Inherited', 'Mn', 11, 'ARABIC FATHATAN..ARABIC HAMZA BELOW').
unicode_script(0x065F, 0x065F, 'Inherited', 'Mn', 1, 'ARABIC WAVY HAMZA BELOW').
unicode_script(0x0670, 0x0670, 'Inherited', 'Mn', 1, 'ARABIC LETTER SUPERSCRIPT ALEF').
unicode_script(0x0951, 0x0952, 'Inherited', 'Mn', 2, 'DEVANAGARI STRESS SIGN UDATTA..DEVANAGARI STRESS SIGN ANUDATTA').
unicode_script(0x1CD0, 0x1CD2, 'Inherited', 'Mn', 3, 'VEDIC TONE KARSHANA..VEDIC TONE PRENKHA').
unicode_script(0x1CD4, 0x1CE0, 'Inherited', 'Mn', 13, 'VEDIC SIGN YAJURVEDIC MIDLINE SVARITA..VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA').
unicode_script(0x1CE2, 0x1CE8, 'Inherited', 'Mn', 7, 'VEDIC SIGN VISARGA SVARITA..VEDIC SIGN VISARGA ANUDATTA WITH TAIL').
unicode_script(0x1CED, 0x1CED, 'Inherited', 'Mn', 1, 'VEDIC SIGN TIRYAK').
unicode_script(0x1CF4, 0x1CF4, 'Inherited', 'Mn', 1, 'VEDIC TONE CANDRA ABOVE').
unicode_script(0x1DC0, 0x1DE6, 'Inherited', 'Mn', 39, 'COMBINING DOTTED GRAVE ACCENT..COMBINING LATIN SMALL LETTER Z').
unicode_script(0x1DFC, 0x1DFF, 'Inherited', 'Mn', 4, 'COMBINING DOUBLE INVERTED BREVE BELOW..COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW').
unicode_script(0x200C, 0x200D, 'Inherited', 'Cf', 2, 'ZERO WIDTH NON-JOINER..ZERO WIDTH JOINER').
unicode_script(0x20D0, 0x20DC, 'Inherited', 'Mn', 13, 'COMBINING LEFT HARPOON ABOVE..COMBINING FOUR DOTS ABOVE').
unicode_script(0x20DD, 0x20E0, 'Inherited', 'Me', 4, 'COMBINING ENCLOSING CIRCLE..COMBINING ENCLOSING CIRCLE BACKSLASH').
unicode_script(0x20E1, 0x20E1, 'Inherited', 'Mn', 1, 'COMBINING LEFT RIGHT ARROW ABOVE').
unicode_script(0x20E2, 0x20E4, 'Inherited', 'Me', 3, 'COMBINING ENCLOSING SCREEN..COMBINING ENCLOSING UPWARD POINTING TRIANGLE').
unicode_script(0x20E5, 0x20F0, 'Inherited', 'Mn', 12, 'COMBINING REVERSE SOLIDUS OVERLAY..COMBINING ASTERISK ABOVE').
unicode_script(0x302A, 0x302D, 'Inherited', 'Mn', 4, 'IDEOGRAPHIC LEVEL TONE MARK..IDEOGRAPHIC ENTERING TONE MARK').
unicode_script(0x3099, 0x309A, 'Inherited', 'Mn', 2, 'COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK..COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK').
unicode_script(0xFE00, 0xFE0F, 'Inherited', 'Mn', 16, 'VARIATION SELECTOR-1..VARIATION SELECTOR-16').
unicode_script(0xFE20, 0xFE26, 'Inherited', 'Mn', 7, 'COMBINING LIGATURE LEFT HALF..COMBINING CONJOINING MACRON').
unicode_script(0x101FD, 0x101FD, 'Inherited', 'Mn', 1, 'PHAISTOS DISC SIGN COMBINING OBLIQUE STROKE').
unicode_script(0x1D167, 0x1D169, 'Inherited', 'Mn', 3, 'MUSICAL SYMBOL COMBINING TREMOLO-1..MUSICAL SYMBOL COMBINING TREMOLO-3').
unicode_script(0x1D17B, 0x1D182, 'Inherited', 'Mn', 8, 'MUSICAL SYMBOL COMBINING ACCENT..MUSICAL SYMBOL COMBINING LOURE').
unicode_script(0x1D185, 0x1D18B, 'Inherited', 'Mn', 7, 'MUSICAL SYMBOL COMBINING DOIT..MUSICAL SYMBOL COMBINING TRIPLE TONGUE').
unicode_script(0x1D1AA, 0x1D1AD, 'Inherited', 'Mn', 4, 'MUSICAL SYMBOL COMBINING DOWN BOW..MUSICAL SYMBOL COMBINING SNAP PIZZICATO').
unicode_script(0xE0100, 0xE01EF, 'Inherited', 'Mn', 240, 'VARIATION SELECTOR-17..VARIATION SELECTOR-256').

% Total code points: 524

% ================================================

unicode_script(0x1700, 0x170C, 'Tagalog', 'Lo', 13, 'TAGALOG LETTER A..TAGALOG LETTER YA').
unicode_script(0x170E, 0x1711, 'Tagalog', 'Lo', 4, 'TAGALOG LETTER LA..TAGALOG LETTER HA').
unicode_script(0x1712, 0x1714, 'Tagalog', 'Mn', 3, 'TAGALOG VOWEL SIGN I..TAGALOG SIGN VIRAMA').

% Total code points: 20

% ================================================

unicode_script(0x1720, 0x1731, 'Hanunoo', 'Lo', 18, 'HANUNOO LETTER A..HANUNOO LETTER HA').
unicode_script(0x1732, 0x1734, 'Hanunoo', 'Mn', 3, 'HANUNOO VOWEL SIGN I..HANUNOO SIGN PAMUDPOD').

% Total code points: 21

% ================================================

unicode_script(0x1740, 0x1751, 'Buhid', 'Lo', 18, 'BUHID LETTER A..BUHID LETTER HA').
unicode_script(0x1752, 0x1753, 'Buhid', 'Mn', 2, 'BUHID VOWEL SIGN I..BUHID VOWEL SIGN U').

% Total code points: 20

% ================================================

unicode_script(0x1760, 0x176C, 'Tagbanwa', 'Lo', 13, 'TAGBANWA LETTER A..TAGBANWA LETTER YA').
unicode_script(0x176E, 0x1770, 'Tagbanwa', 'Lo', 3, 'TAGBANWA LETTER LA..TAGBANWA LETTER SA').
unicode_script(0x1772, 0x1773, 'Tagbanwa', 'Mn', 2, 'TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U').

% Total code points: 18

% ================================================

unicode_script(0x1900, 0x191C, 'Limbu', 'Lo', 29, 'LIMBU VOWEL-CARRIER LETTER..LIMBU LETTER HA').
unicode_script(0x1920, 0x1922, 'Limbu', 'Mn', 3, 'LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN U').
unicode_script(0x1923, 0x1926, 'Limbu', 'Mc', 4, 'LIMBU VOWEL SIGN EE..LIMBU VOWEL SIGN AU').
unicode_script(0x1927, 0x1928, 'Limbu', 'Mn', 2, 'LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O').
unicode_script(0x1929, 0x192B, 'Limbu', 'Mc', 3, 'LIMBU SUBJOINED LETTER YA..LIMBU SUBJOINED LETTER WA').
unicode_script(0x1930, 0x1931, 'Limbu', 'Mc', 2, 'LIMBU SMALL LETTER KA..LIMBU SMALL LETTER NGA').
unicode_script(0x1932, 0x1932, 'Limbu', 'Mn', 1, 'LIMBU SMALL LETTER ANUSVARA').
unicode_script(0x1933, 0x1938, 'Limbu', 'Mc', 6, 'LIMBU SMALL LETTER TA..LIMBU SMALL LETTER LA').
unicode_script(0x1939, 0x193B, 'Limbu', 'Mn', 3, 'LIMBU SIGN MUKPHRENG..LIMBU SIGN SA-I').
unicode_script(0x1940, 0x1940, 'Limbu', 'So', 1, 'LIMBU SIGN LOO').
unicode_script(0x1944, 0x1945, 'Limbu', 'Po', 2, 'LIMBU EXCLAMATION MARK..LIMBU QUESTION MARK').
unicode_script(0x1946, 0x194F, 'Limbu', 'Nd', 10, 'LIMBU DIGIT ZERO..LIMBU DIGIT NINE').

% Total code points: 66

% ================================================

unicode_script(0x1950, 0x196D, 'Tai_Le', 'Lo', 30, 'TAI LE LETTER KA..TAI LE LETTER AI').
unicode_script(0x1970, 0x1974, 'Tai_Le', 'Lo', 5, 'TAI LE LETTER TONE-2..TAI LE LETTER TONE-6').

% Total code points: 35

% ================================================

unicode_script(0x10000, 0x1000B, 'Linear_B', 'Lo', 12, 'LINEAR B SYLLABLE B008 A..LINEAR B SYLLABLE B046 JE').
unicode_script(0x1000D, 0x10026, 'Linear_B', 'Lo', 26, 'LINEAR B SYLLABLE B036 JO..LINEAR B SYLLABLE B032 QO').
unicode_script(0x10028, 0x1003A, 'Linear_B', 'Lo', 19, 'LINEAR B SYLLABLE B060 RA..LINEAR B SYLLABLE B042 WO').
unicode_script(0x1003C, 0x1003D, 'Linear_B', 'Lo', 2, 'LINEAR B SYLLABLE B017 ZA..LINEAR B SYLLABLE B074 ZE').
unicode_script(0x1003F, 0x1004D, 'Linear_B', 'Lo', 15, 'LINEAR B SYLLABLE B020 ZO..LINEAR B SYLLABLE B091 TWO').
unicode_script(0x10050, 0x1005D, 'Linear_B', 'Lo', 14, 'LINEAR B SYMBOL B018..LINEAR B SYMBOL B089').
unicode_script(0x10080, 0x100FA, 'Linear_B', 'Lo', 123, 'LINEAR B IDEOGRAM B100 MAN..LINEAR B IDEOGRAM VESSEL B305').

% Total code points: 211

% ================================================

unicode_script(0x10380, 0x1039D, 'Ugaritic', 'Lo', 30, 'UGARITIC LETTER ALPA..UGARITIC LETTER SSU').
unicode_script(0x1039F, 0x1039F, 'Ugaritic', 'Po', 1, 'UGARITIC WORD DIVIDER').

% Total code points: 31

% ================================================

unicode_script(0x10450, 0x1047F, 'Shavian', 'Lo', 48, 'SHAVIAN LETTER PEEP..SHAVIAN LETTER YEW').

% Total code points: 48

% ================================================

unicode_script(0x10480, 0x1049D, 'Osmanya', 'Lo', 30, 'OSMANYA LETTER ALEF..OSMANYA LETTER OO').
unicode_script(0x104A0, 0x104A9, 'Osmanya', 'Nd', 10, 'OSMANYA DIGIT ZERO..OSMANYA DIGIT NINE').

% Total code points: 40

% ================================================

unicode_script(0x10800, 0x10805, 'Cypriot', 'Lo', 6, 'CYPRIOT SYLLABLE A..CYPRIOT SYLLABLE JA').
unicode_script(0x10808, 0x10808, 'Cypriot', 'Lo', 1, ' CYPRIOT SYLLABLE JO').
unicode_script(0x1080A, 0x10835, 'Cypriot', 'Lo', 44, 'CYPRIOT SYLLABLE KA..CYPRIOT SYLLABLE WO').
unicode_script(0x10837, 0x10838, 'Cypriot', 'Lo', 2, 'CYPRIOT SYLLABLE XA..CYPRIOT SYLLABLE XE').
unicode_script(0x1083C, 0x1083C, 'Cypriot', 'Lo', 1, ' CYPRIOT SYLLABLE ZA').
unicode_script(0x1083F, 0x1083F, 'Cypriot', 'Lo', 1, ' CYPRIOT SYLLABLE ZO').

% Total code points: 55

% ================================================

unicode_script(0x2800, 0x28FF, 'Braille', 'So', 256, 'BRAILLE PATTERN BLANK..BRAILLE PATTERN DOTS-12345678').

% Total code points: 256

% ================================================

unicode_script(0x1A00, 0x1A16, 'Buginese', 'Lo', 23, 'BUGINESE LETTER KA..BUGINESE LETTER HA').
unicode_script(0x1A17, 0x1A18, 'Buginese', 'Mn', 2, 'BUGINESE VOWEL SIGN I..BUGINESE VOWEL SIGN U').
unicode_script(0x1A19, 0x1A1B, 'Buginese', 'Mc', 3, 'BUGINESE VOWEL SIGN E..BUGINESE VOWEL SIGN AE').
unicode_script(0x1A1E, 0x1A1F, 'Buginese', 'Po', 2, 'BUGINESE PALLAWA..BUGINESE END OF SECTION').

% Total code points: 30

% ================================================

unicode_script(0x03E2, 0x03EF, 'Coptic', 'L&', 14, 'COPTIC CAPITAL LETTER SHEI..COPTIC SMALL LETTER DEI').
unicode_script(0x2C80, 0x2CE4, 'Coptic', 'L&', 101, 'COPTIC CAPITAL LETTER ALFA..COPTIC SYMBOL KAI').
unicode_script(0x2CE5, 0x2CEA, 'Coptic', 'So', 6, 'COPTIC SYMBOL MI RO..COPTIC SYMBOL SHIMA SIMA').
unicode_script(0x2CEB, 0x2CEE, 'Coptic', 'L&', 4, 'COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI..COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA').
unicode_script(0x2CEF, 0x2CF1, 'Coptic', 'Mn', 3, 'COPTIC COMBINING NI ABOVE..COPTIC COMBINING SPIRITUS LENIS').
unicode_script(0x2CF2, 0x2CF3, 'Coptic', 'L&', 2, 'COPTIC CAPITAL LETTER BOHAIRIC KHEI..COPTIC SMALL LETTER BOHAIRIC KHEI').
unicode_script(0x2CF9, 0x2CFC, 'Coptic', 'Po', 4, 'COPTIC OLD NUBIAN FULL STOP..COPTIC OLD NUBIAN VERSE DIVIDER').
unicode_script(0x2CFD, 0x2CFD, 'Coptic', 'No', 1, 'COPTIC FRACTION ONE HALF').
unicode_script(0x2CFE, 0x2CFF, 'Coptic', 'Po', 2, 'COPTIC FULL STOP..COPTIC MORPHOLOGICAL DIVIDER').

% Total code points: 137

% ================================================

unicode_script(0x1980, 0x19AB, 'New_Tai_Lue', 'Lo', 44, 'NEW TAI LUE LETTER HIGH QA..NEW TAI LUE LETTER LOW SUA').
unicode_script(0x19B0, 0x19C0, 'New_Tai_Lue', 'Mc', 17, 'NEW TAI LUE VOWEL SIGN VOWEL SHORTENER..NEW TAI LUE VOWEL SIGN IY').
unicode_script(0x19C1, 0x19C7, 'New_Tai_Lue', 'Lo', 7, 'NEW TAI LUE LETTER FINAL V..NEW TAI LUE LETTER FINAL B').
unicode_script(0x19C8, 0x19C9, 'New_Tai_Lue', 'Mc', 2, 'NEW TAI LUE TONE MARK-1..NEW TAI LUE TONE MARK-2').
unicode_script(0x19D0, 0x19D9, 'New_Tai_Lue', 'Nd', 10, 'NEW TAI LUE DIGIT ZERO..NEW TAI LUE DIGIT NINE').
unicode_script(0x19DA, 0x19DA, 'New_Tai_Lue', 'No', 1, 'NEW TAI LUE THAM DIGIT ONE').
unicode_script(0x19DE, 0x19DF, 'New_Tai_Lue', 'So', 2, 'NEW TAI LUE SIGN LAE..NEW TAI LUE SIGN LAEV').

% Total code points: 83

% ================================================

unicode_script(0x2C00, 0x2C2E, 'Glagolitic', 'L&', 47, 'GLAGOLITIC CAPITAL LETTER AZU..GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE').
unicode_script(0x2C30, 0x2C5E, 'Glagolitic', 'L&', 47, 'GLAGOLITIC SMALL LETTER AZU..GLAGOLITIC SMALL LETTER LATINATE MYSLITE').

% Total code points: 94

% ================================================

unicode_script(0x2D30, 0x2D67, 'Tifinagh', 'Lo', 56, 'TIFINAGH LETTER YA..TIFINAGH LETTER YO').
unicode_script(0x2D6F, 0x2D6F, 'Tifinagh', 'Lm', 1, 'TIFINAGH MODIFIER LETTER LABIALIZATION MARK').
unicode_script(0x2D70, 0x2D70, 'Tifinagh', 'Po', 1, 'TIFINAGH SEPARATOR MARK').
unicode_script(0x2D7F, 0x2D7F, 'Tifinagh', 'Mn', 1, 'TIFINAGH CONSONANT JOINER').

% Total code points: 59

% ================================================

unicode_script(0xA800, 0xA801, 'Syloti_Nagri', 'Lo', 2, 'SYLOTI NAGRI LETTER A..SYLOTI NAGRI LETTER I').
unicode_script(0xA802, 0xA802, 'Syloti_Nagri', 'Mn', 1, 'SYLOTI NAGRI SIGN DVISVARA').
unicode_script(0xA803, 0xA805, 'Syloti_Nagri', 'Lo', 3, 'SYLOTI NAGRI LETTER U..SYLOTI NAGRI LETTER O').
unicode_script(0xA806, 0xA806, 'Syloti_Nagri', 'Mn', 1, 'SYLOTI NAGRI SIGN HASANTA').
unicode_script(0xA807, 0xA80A, 'Syloti_Nagri', 'Lo', 4, 'SYLOTI NAGRI LETTER KO..SYLOTI NAGRI LETTER GHO').
unicode_script(0xA80B, 0xA80B, 'Syloti_Nagri', 'Mn', 1, 'SYLOTI NAGRI SIGN ANUSVARA').
unicode_script(0xA80C, 0xA822, 'Syloti_Nagri', 'Lo', 23, 'SYLOTI NAGRI LETTER CO..SYLOTI NAGRI LETTER HO').
unicode_script(0xA823, 0xA824, 'Syloti_Nagri', 'Mc', 2, 'SYLOTI NAGRI VOWEL SIGN A..SYLOTI NAGRI VOWEL SIGN I').
unicode_script(0xA825, 0xA826, 'Syloti_Nagri', 'Mn', 2, 'SYLOTI NAGRI VOWEL SIGN U..SYLOTI NAGRI VOWEL SIGN E').
unicode_script(0xA827, 0xA827, 'Syloti_Nagri', 'Mc', 1, 'SYLOTI NAGRI VOWEL SIGN OO').
unicode_script(0xA828, 0xA82B, 'Syloti_Nagri', 'So', 4, 'SYLOTI NAGRI POETRY MARK-1..SYLOTI NAGRI POETRY MARK-4').

% Total code points: 44

% ================================================

unicode_script(0x103A0, 0x103C3, 'Old_Persian', 'Lo', 36, 'OLD PERSIAN SIGN A..OLD PERSIAN SIGN HA').
unicode_script(0x103C8, 0x103CF, 'Old_Persian', 'Lo', 8, 'OLD PERSIAN SIGN AURAMAZDAA..OLD PERSIAN SIGN BUUMISH').
unicode_script(0x103D0, 0x103D0, 'Old_Persian', 'Po', 1, 'OLD PERSIAN WORD DIVIDER').
unicode_script(0x103D1, 0x103D5, 'Old_Persian', 'Nl', 5, 'OLD PERSIAN NUMBER ONE..OLD PERSIAN NUMBER HUNDRED').

% Total code points: 50

% ================================================

unicode_script(0x10A00, 0x10A00, 'Kharoshthi', 'Lo', 1, ' KHAROSHTHI LETTER A').
unicode_script(0x10A01, 0x10A03, 'Kharoshthi', 'Mn', 3, 'KHAROSHTHI VOWEL SIGN I..KHAROSHTHI VOWEL SIGN VOCALIC R').
unicode_script(0x10A05, 0x10A06, 'Kharoshthi', 'Mn', 2, 'KHAROSHTHI VOWEL SIGN E..KHAROSHTHI VOWEL SIGN O').
unicode_script(0x10A0C, 0x10A0F, 'Kharoshthi', 'Mn', 4, 'KHAROSHTHI VOWEL LENGTH MARK..KHAROSHTHI SIGN VISARGA').
unicode_script(0x10A10, 0x10A13, 'Kharoshthi', 'Lo', 4, 'KHAROSHTHI LETTER KA..KHAROSHTHI LETTER GHA').
unicode_script(0x10A15, 0x10A17, 'Kharoshthi', 'Lo', 3, 'KHAROSHTHI LETTER CA..KHAROSHTHI LETTER JA').
unicode_script(0x10A19, 0x10A33, 'Kharoshthi', 'Lo', 27, 'KHAROSHTHI LETTER NYA..KHAROSHTHI LETTER TTTHA').
unicode_script(0x10A38, 0x10A3A, 'Kharoshthi', 'Mn', 3, 'KHAROSHTHI SIGN BAR ABOVE..KHAROSHTHI SIGN DOT BELOW').
unicode_script(0x10A3F, 0x10A3F, 'Kharoshthi', 'Mn', 1, 'KHAROSHTHI VIRAMA').
unicode_script(0x10A40, 0x10A47, 'Kharoshthi', 'No', 8, 'KHAROSHTHI DIGIT ONE..KHAROSHTHI NUMBER ONE THOUSAND').
unicode_script(0x10A50, 0x10A58, 'Kharoshthi', 'Po', 9, 'KHAROSHTHI PUNCTUATION DOT..KHAROSHTHI PUNCTUATION LINES').

% Total code points: 65

% ================================================

unicode_script(0x1B00, 0x1B03, 'Balinese', 'Mn', 4, 'BALINESE SIGN ULU RICEM..BALINESE SIGN SURANG').
unicode_script(0x1B04, 0x1B04, 'Balinese', 'Mc', 1, 'BALINESE SIGN BISAH').
unicode_script(0x1B05, 0x1B33, 'Balinese', 'Lo', 47, 'BALINESE LETTER AKARA..BALINESE LETTER HA').
unicode_script(0x1B34, 0x1B34, 'Balinese', 'Mn', 1, 'BALINESE SIGN REREKAN').
unicode_script(0x1B35, 0x1B35, 'Balinese', 'Mc', 1, 'BALINESE VOWEL SIGN TEDUNG').
unicode_script(0x1B36, 0x1B3A, 'Balinese', 'Mn', 5, 'BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN RA REPA').
unicode_script(0x1B3B, 0x1B3B, 'Balinese', 'Mc', 1, 'BALINESE VOWEL SIGN RA REPA TEDUNG').
unicode_script(0x1B3C, 0x1B3C, 'Balinese', 'Mn', 1, 'BALINESE VOWEL SIGN LA LENGA').
unicode_script(0x1B3D, 0x1B41, 'Balinese', 'Mc', 5, 'BALINESE VOWEL SIGN LA LENGA TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG').
unicode_script(0x1B42, 0x1B42, 'Balinese', 'Mn', 1, 'BALINESE VOWEL SIGN PEPET').
unicode_script(0x1B43, 0x1B44, 'Balinese', 'Mc', 2, 'BALINESE VOWEL SIGN PEPET TEDUNG..BALINESE ADEG ADEG').
unicode_script(0x1B45, 0x1B4B, 'Balinese', 'Lo', 7, 'BALINESE LETTER KAF SASAK..BALINESE LETTER ASYURA SASAK').
unicode_script(0x1B50, 0x1B59, 'Balinese', 'Nd', 10, 'BALINESE DIGIT ZERO..BALINESE DIGIT NINE').
unicode_script(0x1B5A, 0x1B60, 'Balinese', 'Po', 7, 'BALINESE PANTI..BALINESE PAMENENG').
unicode_script(0x1B61, 0x1B6A, 'Balinese', 'So', 10, 'BALINESE MUSICAL SYMBOL DONG..BALINESE MUSICAL SYMBOL DANG GEDE').
unicode_script(0x1B6B, 0x1B73, 'Balinese', 'Mn', 9, 'BALINESE MUSICAL SYMBOL COMBINING TEGEH..BALINESE MUSICAL SYMBOL COMBINING GONG').
unicode_script(0x1B74, 0x1B7C, 'Balinese', 'So', 9, 'BALINESE MUSICAL SYMBOL RIGHT-HAND OPEN DUG..BALINESE MUSICAL SYMBOL LEFT-HAND OPEN PING').

% Total code points: 121

% ================================================

unicode_script(0x12000, 0x1236E, 'Cuneiform', 'Lo', 879, 'CUNEIFORM SIGN A..CUNEIFORM SIGN ZUM').
unicode_script(0x12400, 0x12462, 'Cuneiform', 'Nl', 99, 'CUNEIFORM NUMERIC SIGN TWO ASH..CUNEIFORM NUMERIC SIGN OLD ASSYRIAN ONE QUARTER').
unicode_script(0x12470, 0x12473, 'Cuneiform', 'Po', 4, 'CUNEIFORM PUNCTUATION SIGN OLD ASSYRIAN WORD DIVIDER..CUNEIFORM PUNCTUATION SIGN DIAGONAL TRICOLON').

% Total code points: 982

% ================================================

unicode_script(0x10900, 0x10915, 'Phoenician', 'Lo', 22, 'PHOENICIAN LETTER ALF..PHOENICIAN LETTER TAU').
unicode_script(0x10916, 0x1091B, 'Phoenician', 'No', 6, 'PHOENICIAN NUMBER ONE..PHOENICIAN NUMBER THREE').
unicode_script(0x1091F, 0x1091F, 'Phoenician', 'Po', 1, 'PHOENICIAN WORD SEPARATOR').

% Total code points: 29

% ================================================

unicode_script(0xA840, 0xA873, 'Phags_Pa', 'Lo', 52, 'PHAGS-PA LETTER KA..PHAGS-PA LETTER CANDRABINDU').
unicode_script(0xA874, 0xA877, 'Phags_Pa', 'Po', 4, 'PHAGS-PA SINGLE HEAD MARK..PHAGS-PA MARK DOUBLE SHAD').

% Total code points: 56

% ================================================

unicode_script(0x07C0, 0x07C9, 'Nko', 'Nd', 10, 'NKO DIGIT ZERO..NKO DIGIT NINE').
unicode_script(0x07CA, 0x07EA, 'Nko', 'Lo', 33, 'NKO LETTER A..NKO LETTER JONA RA').
unicode_script(0x07EB, 0x07F3, 'Nko', 'Mn', 9, 'NKO COMBINING SHORT HIGH TONE..NKO COMBINING DOUBLE DOT ABOVE').
unicode_script(0x07F4, 0x07F5, 'Nko', 'Lm', 2, 'NKO HIGH TONE APOSTROPHE..NKO LOW TONE APOSTROPHE').
unicode_script(0x07F6, 0x07F6, 'Nko', 'So', 1, 'NKO SYMBOL OO DENNEN').
unicode_script(0x07F7, 0x07F9, 'Nko', 'Po', 3, 'NKO SYMBOL GBAKURUNEN..NKO EXCLAMATION MARK').
unicode_script(0x07FA, 0x07FA, 'Nko', 'Lm', 1, 'NKO LAJANYALAN').

% Total code points: 59

% ================================================

unicode_script(0x1B80, 0x1B81, 'Sundanese', 'Mn', 2, 'SUNDANESE SIGN PANYECEK..SUNDANESE SIGN PANGLAYAR').
unicode_script(0x1B82, 0x1B82, 'Sundanese', 'Mc', 1, 'SUNDANESE SIGN PANGWISAD').
unicode_script(0x1B83, 0x1BA0, 'Sundanese', 'Lo', 30, 'SUNDANESE LETTER A..SUNDANESE LETTER HA').
unicode_script(0x1BA1, 0x1BA1, 'Sundanese', 'Mc', 1, 'SUNDANESE CONSONANT SIGN PAMINGKAL').
unicode_script(0x1BA2, 0x1BA5, 'Sundanese', 'Mn', 4, 'SUNDANESE CONSONANT SIGN PANYAKRA..SUNDANESE VOWEL SIGN PANYUKU').
unicode_script(0x1BA6, 0x1BA7, 'Sundanese', 'Mc', 2, 'SUNDANESE VOWEL SIGN PANAELAENG..SUNDANESE VOWEL SIGN PANOLONG').
unicode_script(0x1BA8, 0x1BA9, 'Sundanese', 'Mn', 2, 'SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG').
unicode_script(0x1BAA, 0x1BAA, 'Sundanese', 'Mc', 1, 'SUNDANESE SIGN PAMAAEH').
unicode_script(0x1BAB, 0x1BAB, 'Sundanese', 'Mn', 1, 'SUNDANESE SIGN VIRAMA').
unicode_script(0x1BAC, 0x1BAD, 'Sundanese', 'Mc', 2, 'SUNDANESE CONSONANT SIGN PASANGAN MA..SUNDANESE CONSONANT SIGN PASANGAN WA').
unicode_script(0x1BAE, 0x1BAF, 'Sundanese', 'Lo', 2, 'SUNDANESE LETTER KHA..SUNDANESE LETTER SYA').
unicode_script(0x1BB0, 0x1BB9, 'Sundanese', 'Nd', 10, 'SUNDANESE DIGIT ZERO..SUNDANESE DIGIT NINE').
unicode_script(0x1BBA, 0x1BBF, 'Sundanese', 'Lo', 6, 'SUNDANESE AVAGRAHA..SUNDANESE LETTER FINAL M').
unicode_script(0x1CC0, 0x1CC7, 'Sundanese', 'Po', 8, 'SUNDANESE PUNCTUATION BINDU SURYA..SUNDANESE PUNCTUATION BINDU BA SATANGA').

% Total code points: 72

% ================================================

unicode_script(0x1C00, 0x1C23, 'Lepcha', 'Lo', 36, 'LEPCHA LETTER KA..LEPCHA LETTER A').
unicode_script(0x1C24, 0x1C2B, 'Lepcha', 'Mc', 8, 'LEPCHA SUBJOINED LETTER YA..LEPCHA VOWEL SIGN UU').
unicode_script(0x1C2C, 0x1C33, 'Lepcha', 'Mn', 8, 'LEPCHA VOWEL SIGN E..LEPCHA CONSONANT SIGN T').
unicode_script(0x1C34, 0x1C35, 'Lepcha', 'Mc', 2, 'LEPCHA CONSONANT SIGN NYIN-DO..LEPCHA CONSONANT SIGN KANG').
unicode_script(0x1C36, 0x1C37, 'Lepcha', 'Mn', 2, 'LEPCHA SIGN RAN..LEPCHA SIGN NUKTA').
unicode_script(0x1C3B, 0x1C3F, 'Lepcha', 'Po', 5, 'LEPCHA PUNCTUATION TA-ROL..LEPCHA PUNCTUATION TSHOOK').
unicode_script(0x1C40, 0x1C49, 'Lepcha', 'Nd', 10, 'LEPCHA DIGIT ZERO..LEPCHA DIGIT NINE').
unicode_script(0x1C4D, 0x1C4F, 'Lepcha', 'Lo', 3, 'LEPCHA LETTER TTA..LEPCHA LETTER DDA').

% Total code points: 74

% ================================================

unicode_script(0x1C50, 0x1C59, 'Ol_Chiki', 'Nd', 10, 'OL CHIKI DIGIT ZERO..OL CHIKI DIGIT NINE').
unicode_script(0x1C5A, 0x1C77, 'Ol_Chiki', 'Lo', 30, 'OL CHIKI LETTER LA..OL CHIKI LETTER OH').
unicode_script(0x1C78, 0x1C7D, 'Ol_Chiki', 'Lm', 6, 'OL CHIKI MU TTUDDAG..OL CHIKI AHAD').
unicode_script(0x1C7E, 0x1C7F, 'Ol_Chiki', 'Po', 2, 'OL CHIKI PUNCTUATION MUCAAD..OL CHIKI PUNCTUATION DOUBLE MUCAAD').

% Total code points: 48

% ================================================

unicode_script(0xA500, 0xA60B, 'Vai', 'Lo', 268, 'VAI SYLLABLE EE..VAI SYLLABLE NG').
unicode_script(0xA60C, 0xA60C, 'Vai', 'Lm', 1, 'VAI SYLLABLE LENGTHENER').
unicode_script(0xA60D, 0xA60F, 'Vai', 'Po', 3, 'VAI COMMA..VAI QUESTION MARK').
unicode_script(0xA610, 0xA61F, 'Vai', 'Lo', 16, 'VAI SYLLABLE NDOLE FA..VAI SYMBOL JONG').
unicode_script(0xA620, 0xA629, 'Vai', 'Nd', 10, 'VAI DIGIT ZERO..VAI DIGIT NINE').
unicode_script(0xA62A, 0xA62B, 'Vai', 'Lo', 2, 'VAI SYLLABLE NDOLE MA..VAI SYLLABLE NDOLE DO').

% Total code points: 300

% ================================================

unicode_script(0xA880, 0xA881, 'Saurashtra', 'Mc', 2, 'SAURASHTRA SIGN ANUSVARA..SAURASHTRA SIGN VISARGA').
unicode_script(0xA882, 0xA8B3, 'Saurashtra', 'Lo', 50, 'SAURASHTRA LETTER A..SAURASHTRA LETTER LLA').
unicode_script(0xA8B4, 0xA8C3, 'Saurashtra', 'Mc', 16, 'SAURASHTRA CONSONANT SIGN HAARU..SAURASHTRA VOWEL SIGN AU').
unicode_script(0xA8C4, 0xA8C4, 'Saurashtra', 'Mn', 1, 'SAURASHTRA SIGN VIRAMA').
unicode_script(0xA8CE, 0xA8CF, 'Saurashtra', 'Po', 2, 'SAURASHTRA DANDA..SAURASHTRA DOUBLE DANDA').
unicode_script(0xA8D0, 0xA8D9, 'Saurashtra', 'Nd', 10, 'SAURASHTRA DIGIT ZERO..SAURASHTRA DIGIT NINE').

% Total code points: 81

% ================================================

unicode_script(0xA900, 0xA909, 'Kayah_Li', 'Nd', 10, 'KAYAH LI DIGIT ZERO..KAYAH LI DIGIT NINE').
unicode_script(0xA90A, 0xA925, 'Kayah_Li', 'Lo', 28, 'KAYAH LI LETTER KA..KAYAH LI LETTER OO').
unicode_script(0xA926, 0xA92D, 'Kayah_Li', 'Mn', 8, 'KAYAH LI VOWEL UE..KAYAH LI TONE CALYA PLOPHU').
unicode_script(0xA92E, 0xA92F, 'Kayah_Li', 'Po', 2, 'KAYAH LI SIGN CWI..KAYAH LI SIGN SHYA').

% Total code points: 48

% ================================================

unicode_script(0xA930, 0xA946, 'Rejang', 'Lo', 23, 'REJANG LETTER KA..REJANG LETTER A').
unicode_script(0xA947, 0xA951, 'Rejang', 'Mn', 11, 'REJANG VOWEL SIGN I..REJANG CONSONANT SIGN R').
unicode_script(0xA952, 0xA953, 'Rejang', 'Mc', 2, 'REJANG CONSONANT SIGN H..REJANG VIRAMA').
unicode_script(0xA95F, 0xA95F, 'Rejang', 'Po', 1, 'REJANG SECTION MARK').

% Total code points: 37

% ================================================

unicode_script(0x10280, 0x1029C, 'Lycian', 'Lo', 29, 'LYCIAN LETTER A..LYCIAN LETTER X').

% Total code points: 29

% ================================================

unicode_script(0x102A0, 0x102D0, 'Carian', 'Lo', 49, 'CARIAN LETTER A..CARIAN LETTER UUU3').

% Total code points: 49

% ================================================

unicode_script(0x10920, 0x10939, 'Lydian', 'Lo', 26, 'LYDIAN LETTER A..LYDIAN LETTER C').
unicode_script(0x1093F, 0x1093F, 'Lydian', 'Po', 1, 'LYDIAN TRIANGULAR MARK').

% Total code points: 27

% ================================================

unicode_script(0xAA00, 0xAA28, 'Cham', 'Lo', 41, 'CHAM LETTER A..CHAM LETTER HA').
unicode_script(0xAA29, 0xAA2E, 'Cham', 'Mn', 6, 'CHAM VOWEL SIGN AA..CHAM VOWEL SIGN OE').
unicode_script(0xAA2F, 0xAA30, 'Cham', 'Mc', 2, 'CHAM VOWEL SIGN O..CHAM VOWEL SIGN AI').
unicode_script(0xAA31, 0xAA32, 'Cham', 'Mn', 2, 'CHAM VOWEL SIGN AU..CHAM VOWEL SIGN UE').
unicode_script(0xAA33, 0xAA34, 'Cham', 'Mc', 2, 'CHAM CONSONANT SIGN YA..CHAM CONSONANT SIGN RA').
unicode_script(0xAA35, 0xAA36, 'Cham', 'Mn', 2, 'CHAM CONSONANT SIGN LA..CHAM CONSONANT SIGN WA').
unicode_script(0xAA40, 0xAA42, 'Cham', 'Lo', 3, 'CHAM LETTER FINAL K..CHAM LETTER FINAL NG').
unicode_script(0xAA43, 0xAA43, 'Cham', 'Mn', 1, 'CHAM CONSONANT SIGN FINAL NG').
unicode_script(0xAA44, 0xAA4B, 'Cham', 'Lo', 8, 'CHAM LETTER FINAL CH..CHAM LETTER FINAL SS').
unicode_script(0xAA4C, 0xAA4C, 'Cham', 'Mn', 1, 'CHAM CONSONANT SIGN FINAL M').
unicode_script(0xAA4D, 0xAA4D, 'Cham', 'Mc', 1, 'CHAM CONSONANT SIGN FINAL H').
unicode_script(0xAA50, 0xAA59, 'Cham', 'Nd', 10, 'CHAM DIGIT ZERO..CHAM DIGIT NINE').
unicode_script(0xAA5C, 0xAA5F, 'Cham', 'Po', 4, 'CHAM PUNCTUATION SPIRAL..CHAM PUNCTUATION TRIPLE DANDA').

% Total code points: 83

% ================================================

unicode_script(0x1A20, 0x1A54, 'Tai_Tham', 'Lo', 53, 'TAI THAM LETTER HIGH KA..TAI THAM LETTER GREAT SA').
unicode_script(0x1A55, 0x1A55, 'Tai_Tham', 'Mc', 1, 'TAI THAM CONSONANT SIGN MEDIAL RA').
unicode_script(0x1A56, 0x1A56, 'Tai_Tham', 'Mn', 1, 'TAI THAM CONSONANT SIGN MEDIAL LA').
unicode_script(0x1A57, 0x1A57, 'Tai_Tham', 'Mc', 1, 'TAI THAM CONSONANT SIGN LA TANG LAI').
unicode_script(0x1A58, 0x1A5E, 'Tai_Tham', 'Mn', 7, 'TAI THAM SIGN MAI KANG LAI..TAI THAM CONSONANT SIGN SA').
unicode_script(0x1A60, 0x1A60, 'Tai_Tham', 'Mn', 1, 'TAI THAM SIGN SAKOT').
unicode_script(0x1A61, 0x1A61, 'Tai_Tham', 'Mc', 1, 'TAI THAM VOWEL SIGN A').
unicode_script(0x1A62, 0x1A62, 'Tai_Tham', 'Mn', 1, 'TAI THAM VOWEL SIGN MAI SAT').
unicode_script(0x1A63, 0x1A64, 'Tai_Tham', 'Mc', 2, 'TAI THAM VOWEL SIGN AA..TAI THAM VOWEL SIGN TALL AA').
unicode_script(0x1A65, 0x1A6C, 'Tai_Tham', 'Mn', 8, 'TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN OA BELOW').
unicode_script(0x1A6D, 0x1A72, 'Tai_Tham', 'Mc', 6, 'TAI THAM VOWEL SIGN OY..TAI THAM VOWEL SIGN THAM AI').
unicode_script(0x1A73, 0x1A7C, 'Tai_Tham', 'Mn', 10, 'TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN KHUEN-LUE KARAN').
unicode_script(0x1A7F, 0x1A7F, 'Tai_Tham', 'Mn', 1, 'TAI THAM COMBINING CRYPTOGRAMMIC DOT').
unicode_script(0x1A80, 0x1A89, 'Tai_Tham', 'Nd', 10, 'TAI THAM HORA DIGIT ZERO..TAI THAM HORA DIGIT NINE').
unicode_script(0x1A90, 0x1A99, 'Tai_Tham', 'Nd', 10, 'TAI THAM THAM DIGIT ZERO..TAI THAM THAM DIGIT NINE').
unicode_script(0x1AA0, 0x1AA6, 'Tai_Tham', 'Po', 7, 'TAI THAM SIGN WIANG..TAI THAM SIGN REVERSED ROTATED RANA').
unicode_script(0x1AA7, 0x1AA7, 'Tai_Tham', 'Lm', 1, 'TAI THAM SIGN MAI YAMOK').
unicode_script(0x1AA8, 0x1AAD, 'Tai_Tham', 'Po', 6, 'TAI THAM SIGN KAAN..TAI THAM SIGN CAANG').

% Total code points: 127

% ================================================

unicode_script(0xAA80, 0xAAAF, 'Tai_Viet', 'Lo', 48, 'TAI VIET LETTER LOW KO..TAI VIET LETTER HIGH O').
unicode_script(0xAAB0, 0xAAB0, 'Tai_Viet', 'Mn', 1, 'TAI VIET MAI KANG').
unicode_script(0xAAB1, 0xAAB1, 'Tai_Viet', 'Lo', 1, ' TAI VIET VOWEL AA').
unicode_script(0xAAB2, 0xAAB4, 'Tai_Viet', 'Mn', 3, 'TAI VIET VOWEL I..TAI VIET VOWEL U').
unicode_script(0xAAB5, 0xAAB6, 'Tai_Viet', 'Lo', 2, 'TAI VIET VOWEL E..TAI VIET VOWEL O').
unicode_script(0xAAB7, 0xAAB8, 'Tai_Viet', 'Mn', 2, 'TAI VIET MAI KHIT..TAI VIET VOWEL IA').
unicode_script(0xAAB9, 0xAABD, 'Tai_Viet', 'Lo', 5, 'TAI VIET VOWEL UEA..TAI VIET VOWEL AN').
unicode_script(0xAABE, 0xAABF, 'Tai_Viet', 'Mn', 2, 'TAI VIET VOWEL AM..TAI VIET TONE MAI EK').
unicode_script(0xAAC0, 0xAAC0, 'Tai_Viet', 'Lo', 1, ' TAI VIET TONE MAI NUENG').
unicode_script(0xAAC1, 0xAAC1, 'Tai_Viet', 'Mn', 1, 'TAI VIET TONE MAI THO').
unicode_script(0xAAC2, 0xAAC2, 'Tai_Viet', 'Lo', 1, ' TAI VIET TONE MAI SONG').
unicode_script(0xAADB, 0xAADC, 'Tai_Viet', 'Lo', 2, 'TAI VIET SYMBOL KON..TAI VIET SYMBOL NUENG').
unicode_script(0xAADD, 0xAADD, 'Tai_Viet', 'Lm', 1, 'TAI VIET SYMBOL SAM').
unicode_script(0xAADE, 0xAADF, 'Tai_Viet', 'Po', 2, 'TAI VIET SYMBOL HO HOI..TAI VIET SYMBOL KOI KOI').

% Total code points: 72

% ================================================

unicode_script(0x10B00, 0x10B35, 'Avestan', 'Lo', 54, 'AVESTAN LETTER A..AVESTAN LETTER HE').
unicode_script(0x10B39, 0x10B3F, 'Avestan', 'Po', 7, 'AVESTAN ABBREVIATION MARK..LARGE ONE RING OVER TWO RINGS PUNCTUATION').

% Total code points: 61

% ================================================

unicode_script(0x13000, 0x1342E, 'Egyptian_Hieroglyphs', 'Lo', 1071, 'EGYPTIAN HIEROGLYPH A001..EGYPTIAN HIEROGLYPH AA032').

% Total code points: 1071

% ================================================

unicode_script(0x0800, 0x0815, 'Samaritan', 'Lo', 22, 'SAMARITAN LETTER ALAF..SAMARITAN LETTER TAAF').
unicode_script(0x0816, 0x0819, 'Samaritan', 'Mn', 4, 'SAMARITAN MARK IN..SAMARITAN MARK DAGESH').
unicode_script(0x081A, 0x081A, 'Samaritan', 'Lm', 1, 'SAMARITAN MODIFIER LETTER EPENTHETIC YUT').
unicode_script(0x081B, 0x0823, 'Samaritan', 'Mn', 9, 'SAMARITAN MARK EPENTHETIC YUT..SAMARITAN VOWEL SIGN A').
unicode_script(0x0824, 0x0824, 'Samaritan', 'Lm', 1, 'SAMARITAN MODIFIER LETTER SHORT A').
unicode_script(0x0825, 0x0827, 'Samaritan', 'Mn', 3, 'SAMARITAN VOWEL SIGN SHORT A..SAMARITAN VOWEL SIGN U').
unicode_script(0x0828, 0x0828, 'Samaritan', 'Lm', 1, 'SAMARITAN MODIFIER LETTER I').
unicode_script(0x0829, 0x082D, 'Samaritan', 'Mn', 5, 'SAMARITAN VOWEL SIGN LONG I..SAMARITAN MARK NEQUDAA').
unicode_script(0x0830, 0x083E, 'Samaritan', 'Po', 15, 'SAMARITAN PUNCTUATION NEQUDAA..SAMARITAN PUNCTUATION ANNAAU').

% Total code points: 61

% ================================================

unicode_script(0xA4D0, 0xA4F7, 'Lisu', 'Lo', 40, 'LISU LETTER BA..LISU LETTER OE').
unicode_script(0xA4F8, 0xA4FD, 'Lisu', 'Lm', 6, 'LISU LETTER TONE MYA TI..LISU LETTER TONE MYA JEU').
unicode_script(0xA4FE, 0xA4FF, 'Lisu', 'Po', 2, 'LISU PUNCTUATION COMMA..LISU PUNCTUATION FULL STOP').

% Total code points: 48

% ================================================

unicode_script(0xA6A0, 0xA6E5, 'Bamum', 'Lo', 70, 'BAMUM LETTER A..BAMUM LETTER KI').
unicode_script(0xA6E6, 0xA6EF, 'Bamum', 'Nl', 10, 'BAMUM LETTER MO..BAMUM LETTER KOGHOM').
unicode_script(0xA6F0, 0xA6F1, 'Bamum', 'Mn', 2, 'BAMUM COMBINING MARK KOQNDON..BAMUM COMBINING MARK TUKWENTIS').
unicode_script(0xA6F2, 0xA6F7, 'Bamum', 'Po', 6, 'BAMUM NJAEMLI..BAMUM QUESTION MARK').
unicode_script(0x16800, 0x16A38, 'Bamum', 'Lo', 569, 'BAMUM LETTER PHASE-A NGKUE MFON..BAMUM LETTER PHASE-F VUEQ').

% Total code points: 657

% ================================================

unicode_script(0xA980, 0xA982, 'Javanese', 'Mn', 3, 'JAVANESE SIGN PANYANGGA..JAVANESE SIGN LAYAR').
unicode_script(0xA983, 0xA983, 'Javanese', 'Mc', 1, 'JAVANESE SIGN WIGNYAN').
unicode_script(0xA984, 0xA9B2, 'Javanese', 'Lo', 47, 'JAVANESE LETTER A..JAVANESE LETTER HA').
unicode_script(0xA9B3, 0xA9B3, 'Javanese', 'Mn', 1, 'JAVANESE SIGN CECAK TELU').
unicode_script(0xA9B4, 0xA9B5, 'Javanese', 'Mc', 2, 'JAVANESE VOWEL SIGN TARUNG..JAVANESE VOWEL SIGN TOLONG').
unicode_script(0xA9B6, 0xA9B9, 'Javanese', 'Mn', 4, 'JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN SUKU MENDUT').
unicode_script(0xA9BA, 0xA9BB, 'Javanese', 'Mc', 2, 'JAVANESE VOWEL SIGN TALING..JAVANESE VOWEL SIGN DIRGA MURE').
unicode_script(0xA9BC, 0xA9BC, 'Javanese', 'Mn', 1, 'JAVANESE VOWEL SIGN PEPET').
unicode_script(0xA9BD, 0xA9C0, 'Javanese', 'Mc', 4, 'JAVANESE CONSONANT SIGN KERET..JAVANESE PANGKON').
unicode_script(0xA9C1, 0xA9CD, 'Javanese', 'Po', 13, 'JAVANESE LEFT RERENGGAN..JAVANESE TURNED PADA PISELEH').
unicode_script(0xA9CF, 0xA9CF, 'Javanese', 'Lm', 1, 'JAVANESE PANGRANGKEP').
unicode_script(0xA9D0, 0xA9D9, 'Javanese', 'Nd', 10, 'JAVANESE DIGIT ZERO..JAVANESE DIGIT NINE').
unicode_script(0xA9DE, 0xA9DF, 'Javanese', 'Po', 2, 'JAVANESE PADA TIRTA TUMETES..JAVANESE PADA ISEN-ISEN').

% Total code points: 91

% ================================================

unicode_script(0xAAE0, 0xAAEA, 'Meetei_Mayek', 'Lo', 11, 'MEETEI MAYEK LETTER E..MEETEI MAYEK LETTER SSA').
unicode_script(0xAAEB, 0xAAEB, 'Meetei_Mayek', 'Mc', 1, 'MEETEI MAYEK VOWEL SIGN II').
unicode_script(0xAAEC, 0xAAED, 'Meetei_Mayek', 'Mn', 2, 'MEETEI MAYEK VOWEL SIGN UU..MEETEI MAYEK VOWEL SIGN AAI').
unicode_script(0xAAEE, 0xAAEF, 'Meetei_Mayek', 'Mc', 2, 'MEETEI MAYEK VOWEL SIGN AU..MEETEI MAYEK VOWEL SIGN AAU').
unicode_script(0xAAF0, 0xAAF1, 'Meetei_Mayek', 'Po', 2, 'MEETEI MAYEK CHEIKHAN..MEETEI MAYEK AHANG KHUDAM').
unicode_script(0xAAF2, 0xAAF2, 'Meetei_Mayek', 'Lo', 1, ' MEETEI MAYEK ANJI').
unicode_script(0xAAF3, 0xAAF4, 'Meetei_Mayek', 'Lm', 2, 'MEETEI MAYEK SYLLABLE REPETITION MARK..MEETEI MAYEK WORD REPETITION MARK').
unicode_script(0xAAF5, 0xAAF5, 'Meetei_Mayek', 'Mc', 1, 'MEETEI MAYEK VOWEL SIGN VISARGA').
unicode_script(0xAAF6, 0xAAF6, 'Meetei_Mayek', 'Mn', 1, 'MEETEI MAYEK VIRAMA').
unicode_script(0xABC0, 0xABE2, 'Meetei_Mayek', 'Lo', 35, 'MEETEI MAYEK LETTER KOK..MEETEI MAYEK LETTER I LONSUM').
unicode_script(0xABE3, 0xABE4, 'Meetei_Mayek', 'Mc', 2, 'MEETEI MAYEK VOWEL SIGN ONAP..MEETEI MAYEK VOWEL SIGN INAP').
unicode_script(0xABE5, 0xABE5, 'Meetei_Mayek', 'Mn', 1, 'MEETEI MAYEK VOWEL SIGN ANAP').
unicode_script(0xABE6, 0xABE7, 'Meetei_Mayek', 'Mc', 2, 'MEETEI MAYEK VOWEL SIGN YENAP..MEETEI MAYEK VOWEL SIGN SOUNAP').
unicode_script(0xABE8, 0xABE8, 'Meetei_Mayek', 'Mn', 1, 'MEETEI MAYEK VOWEL SIGN UNAP').
unicode_script(0xABE9, 0xABEA, 'Meetei_Mayek', 'Mc', 2, 'MEETEI MAYEK VOWEL SIGN CHEINAP..MEETEI MAYEK VOWEL SIGN NUNG').
unicode_script(0xABEB, 0xABEB, 'Meetei_Mayek', 'Po', 1, 'MEETEI MAYEK CHEIKHEI').
unicode_script(0xABEC, 0xABEC, 'Meetei_Mayek', 'Mc', 1, 'MEETEI MAYEK LUM IYEK').
unicode_script(0xABED, 0xABED, 'Meetei_Mayek', 'Mn', 1, 'MEETEI MAYEK APUN IYEK').
unicode_script(0xABF0, 0xABF9, 'Meetei_Mayek', 'Nd', 10, 'MEETEI MAYEK DIGIT ZERO..MEETEI MAYEK DIGIT NINE').

% Total code points: 79

% ================================================

unicode_script(0x10840, 0x10855, 'Imperial_Aramaic', 'Lo', 22, 'IMPERIAL ARAMAIC LETTER ALEPH..IMPERIAL ARAMAIC LETTER TAW').
unicode_script(0x10857, 0x10857, 'Imperial_Aramaic', 'Po', 1, 'IMPERIAL ARAMAIC SECTION SIGN').
unicode_script(0x10858, 0x1085F, 'Imperial_Aramaic', 'No', 8, 'IMPERIAL ARAMAIC NUMBER ONE..IMPERIAL ARAMAIC NUMBER TEN THOUSAND').

% Total code points: 31

% ================================================

unicode_script(0x10A60, 0x10A7C, 'Old_South_Arabian', 'Lo', 29, 'OLD SOUTH ARABIAN LETTER HE..OLD SOUTH ARABIAN LETTER THETH').
unicode_script(0x10A7D, 0x10A7E, 'Old_South_Arabian', 'No', 2, 'OLD SOUTH ARABIAN NUMBER ONE..OLD SOUTH ARABIAN NUMBER FIFTY').
unicode_script(0x10A7F, 0x10A7F, 'Old_South_Arabian', 'Po', 1, 'OLD SOUTH ARABIAN NUMERIC INDICATOR').

% Total code points: 32

% ================================================

unicode_script(0x10B40, 0x10B55, 'Inscriptional_Parthian', 'Lo', 22, 'INSCRIPTIONAL PARTHIAN LETTER ALEPH..INSCRIPTIONAL PARTHIAN LETTER TAW').
unicode_script(0x10B58, 0x10B5F, 'Inscriptional_Parthian', 'No', 8, 'INSCRIPTIONAL PARTHIAN NUMBER ONE..INSCRIPTIONAL PARTHIAN NUMBER ONE THOUSAND').

% Total code points: 30

% ================================================

unicode_script(0x10B60, 0x10B72, 'Inscriptional_Pahlavi', 'Lo', 19, 'INSCRIPTIONAL PAHLAVI LETTER ALEPH..INSCRIPTIONAL PAHLAVI LETTER TAW').
unicode_script(0x10B78, 0x10B7F, 'Inscriptional_Pahlavi', 'No', 8, 'INSCRIPTIONAL PAHLAVI NUMBER ONE..INSCRIPTIONAL PAHLAVI NUMBER ONE THOUSAND').

% Total code points: 27

% ================================================

unicode_script(0x10C00, 0x10C48, 'Old_Turkic', 'Lo', 73, 'OLD TURKIC LETTER ORKHON A..OLD TURKIC LETTER ORKHON BASH').

% Total code points: 73

% ================================================

unicode_script(0x11080, 0x11081, 'Kaithi', 'Mn', 2, 'KAITHI SIGN CANDRABINDU..KAITHI SIGN ANUSVARA').
unicode_script(0x11082, 0x11082, 'Kaithi', 'Mc', 1, 'KAITHI SIGN VISARGA').
unicode_script(0x11083, 0x110AF, 'Kaithi', 'Lo', 45, 'KAITHI LETTER A..KAITHI LETTER HA').
unicode_script(0x110B0, 0x110B2, 'Kaithi', 'Mc', 3, 'KAITHI VOWEL SIGN AA..KAITHI VOWEL SIGN II').
unicode_script(0x110B3, 0x110B6, 'Kaithi', 'Mn', 4, 'KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN AI').
unicode_script(0x110B7, 0x110B8, 'Kaithi', 'Mc', 2, 'KAITHI VOWEL SIGN O..KAITHI VOWEL SIGN AU').
unicode_script(0x110B9, 0x110BA, 'Kaithi', 'Mn', 2, 'KAITHI SIGN VIRAMA..KAITHI SIGN NUKTA').
unicode_script(0x110BB, 0x110BC, 'Kaithi', 'Po', 2, 'KAITHI ABBREVIATION SIGN..KAITHI ENUMERATION SIGN').
unicode_script(0x110BD, 0x110BD, 'Kaithi', 'Cf', 1, 'KAITHI NUMBER SIGN').
unicode_script(0x110BE, 0x110C1, 'Kaithi', 'Po', 4, 'KAITHI SECTION MARK..KAITHI DOUBLE DANDA').

% Total code points: 66

% ================================================

unicode_script(0x1BC0, 0x1BE5, 'Batak', 'Lo', 38, 'BATAK LETTER A..BATAK LETTER U').
unicode_script(0x1BE6, 0x1BE6, 'Batak', 'Mn', 1, 'BATAK SIGN TOMPI').
unicode_script(0x1BE7, 0x1BE7, 'Batak', 'Mc', 1, 'BATAK VOWEL SIGN E').
unicode_script(0x1BE8, 0x1BE9, 'Batak', 'Mn', 2, 'BATAK VOWEL SIGN PAKPAK E..BATAK VOWEL SIGN EE').
unicode_script(0x1BEA, 0x1BEC, 'Batak', 'Mc', 3, 'BATAK VOWEL SIGN I..BATAK VOWEL SIGN O').
unicode_script(0x1BED, 0x1BED, 'Batak', 'Mn', 1, 'BATAK VOWEL SIGN KARO O').
unicode_script(0x1BEE, 0x1BEE, 'Batak', 'Mc', 1, 'BATAK VOWEL SIGN U').
unicode_script(0x1BEF, 0x1BF1, 'Batak', 'Mn', 3, 'BATAK VOWEL SIGN U FOR SIMALUNGUN SA..BATAK CONSONANT SIGN H').
unicode_script(0x1BF2, 0x1BF3, 'Batak', 'Mc', 2, 'BATAK PANGOLAT..BATAK PANONGONAN').
unicode_script(0x1BFC, 0x1BFF, 'Batak', 'Po', 4, 'BATAK SYMBOL BINDU NA METEK..BATAK SYMBOL BINDU PANGOLAT').

% Total code points: 56

% ================================================

unicode_script(0x11000, 0x11000, 'Brahmi', 'Mc', 1, 'BRAHMI SIGN CANDRABINDU').
unicode_script(0x11001, 0x11001, 'Brahmi', 'Mn', 1, 'BRAHMI SIGN ANUSVARA').
unicode_script(0x11002, 0x11002, 'Brahmi', 'Mc', 1, 'BRAHMI SIGN VISARGA').
unicode_script(0x11003, 0x11037, 'Brahmi', 'Lo', 53, 'BRAHMI SIGN JIHVAMULIYA..BRAHMI LETTER OLD TAMIL NNNA').
unicode_script(0x11038, 0x11046, 'Brahmi', 'Mn', 15, 'BRAHMI VOWEL SIGN AA..BRAHMI VIRAMA').
unicode_script(0x11047, 0x1104D, 'Brahmi', 'Po', 7, 'BRAHMI DANDA..BRAHMI PUNCTUATION LOTUS').
unicode_script(0x11052, 0x11065, 'Brahmi', 'No', 20, 'BRAHMI NUMBER ONE..BRAHMI NUMBER ONE THOUSAND').
unicode_script(0x11066, 0x1106F, 'Brahmi', 'Nd', 10, 'BRAHMI DIGIT ZERO..BRAHMI DIGIT NINE').

% Total code points: 108

% ================================================

unicode_script(0x0840, 0x0858, 'Mandaic', 'Lo', 25, 'MANDAIC LETTER HALQA..MANDAIC LETTER AIN').
unicode_script(0x0859, 0x085B, 'Mandaic', 'Mn', 3, 'MANDAIC AFFRICATION MARK..MANDAIC GEMINATION MARK').
unicode_script(0x085E, 0x085E, 'Mandaic', 'Po', 1, 'MANDAIC PUNCTUATION').

% Total code points: 29

% ================================================

unicode_script(0x11100, 0x11102, 'Chakma', 'Mn', 3, 'CHAKMA SIGN CANDRABINDU..CHAKMA SIGN VISARGA').
unicode_script(0x11103, 0x11126, 'Chakma', 'Lo', 36, 'CHAKMA LETTER AA..CHAKMA LETTER HAA').
unicode_script(0x11127, 0x1112B, 'Chakma', 'Mn', 5, 'CHAKMA VOWEL SIGN A..CHAKMA VOWEL SIGN UU').
unicode_script(0x1112C, 0x1112C, 'Chakma', 'Mc', 1, 'CHAKMA VOWEL SIGN E').
unicode_script(0x1112D, 0x11134, 'Chakma', 'Mn', 8, 'CHAKMA VOWEL SIGN AI..CHAKMA MAAYYAA').
unicode_script(0x11136, 0x1113F, 'Chakma', 'Nd', 10, 'CHAKMA DIGIT ZERO..CHAKMA DIGIT NINE').
unicode_script(0x11140, 0x11143, 'Chakma', 'Po', 4, 'CHAKMA SECTION MARK..CHAKMA QUESTION MARK').

% Total code points: 67

% ================================================

unicode_script(0x109A0, 0x109B7, 'Meroitic_Cursive', 'Lo', 24, 'MEROITIC CURSIVE LETTER A..MEROITIC CURSIVE LETTER DA').
unicode_script(0x109BE, 0x109BF, 'Meroitic_Cursive', 'Lo', 2, 'MEROITIC CURSIVE LOGOGRAM RMT..MEROITIC CURSIVE LOGOGRAM IMN').

% Total code points: 26

% ================================================

unicode_script(0x10980, 0x1099F, 'Meroitic_Hieroglyphs', 'Lo', 32, 'MEROITIC HIEROGLYPHIC LETTER A..MEROITIC HIEROGLYPHIC SYMBOL VIDJ-2').

% Total code points: 32

% ================================================

unicode_script(0x16F00, 0x16F44, 'Miao', 'Lo', 69, 'MIAO LETTER PA..MIAO LETTER HHA').
unicode_script(0x16F50, 0x16F50, 'Miao', 'Lo', 1, ' MIAO LETTER NASALIZATION').
unicode_script(0x16F51, 0x16F7E, 'Miao', 'Mc', 46, 'MIAO SIGN ASPIRATION..MIAO VOWEL SIGN NG').
unicode_script(0x16F8F, 0x16F92, 'Miao', 'Mn', 4, 'MIAO TONE RIGHT..MIAO TONE BELOW').
unicode_script(0x16F93, 0x16F9F, 'Miao', 'Lm', 13, 'MIAO LETTER TONE-2..MIAO LETTER REFORMED TONE-8').

% Total code points: 133

% ================================================

unicode_script(0x11180, 0x11181, 'Sharada', 'Mn', 2, 'SHARADA SIGN CANDRABINDU..SHARADA SIGN ANUSVARA').
unicode_script(0x11182, 0x11182, 'Sharada', 'Mc', 1, 'SHARADA SIGN VISARGA').
unicode_script(0x11183, 0x111B2, 'Sharada', 'Lo', 48, 'SHARADA LETTER A..SHARADA LETTER HA').
unicode_script(0x111B3, 0x111B5, 'Sharada', 'Mc', 3, 'SHARADA VOWEL SIGN AA..SHARADA VOWEL SIGN II').
unicode_script(0x111B6, 0x111BE, 'Sharada', 'Mn', 9, 'SHARADA VOWEL SIGN U..SHARADA VOWEL SIGN O').
unicode_script(0x111BF, 0x111C0, 'Sharada', 'Mc', 2, 'SHARADA VOWEL SIGN AU..SHARADA SIGN VIRAMA').
unicode_script(0x111C1, 0x111C4, 'Sharada', 'Lo', 4, 'SHARADA SIGN AVAGRAHA..SHARADA OM').
unicode_script(0x111C5, 0x111C8, 'Sharada', 'Po', 4, 'SHARADA DANDA..SHARADA SEPARATOR').
unicode_script(0x111D0, 0x111D9, 'Sharada', 'Nd', 10, 'SHARADA DIGIT ZERO..SHARADA DIGIT NINE').

% Total code points: 83

% ================================================

unicode_script(0x110D0, 0x110E8, 'Sora_Sompeng', 'Lo', 25, 'SORA SOMPENG LETTER SAH..SORA SOMPENG LETTER MAE').
unicode_script(0x110F0, 0x110F9, 'Sora_Sompeng', 'Nd', 10, 'SORA SOMPENG DIGIT ZERO..SORA SOMPENG DIGIT NINE').

% Total code points: 35

% ================================================

unicode_script(0x11680, 0x116AA, 'Takri', 'Lo', 43, 'TAKRI LETTER A..TAKRI LETTER RRA').
unicode_script(0x116AB, 0x116AB, 'Takri', 'Mn', 1, 'TAKRI SIGN ANUSVARA').
unicode_script(0x116AC, 0x116AC, 'Takri', 'Mc', 1, 'TAKRI SIGN VISARGA').
unicode_script(0x116AD, 0x116AD, 'Takri', 'Mn', 1, 'TAKRI VOWEL SIGN AA').
unicode_script(0x116AE, 0x116AF, 'Takri', 'Mc', 2, 'TAKRI VOWEL SIGN I..TAKRI VOWEL SIGN II').
unicode_script(0x116B0, 0x116B5, 'Takri', 'Mn', 6, 'TAKRI VOWEL SIGN U..TAKRI VOWEL SIGN AU').
unicode_script(0x116B6, 0x116B6, 'Takri', 'Mc', 1, 'TAKRI SIGN VIRAMA').
unicode_script(0x116B7, 0x116B7, 'Takri', 'Mn', 1, 'TAKRI SIGN NUKTA').
unicode_script(0x116C0, 0x116C9, 'Takri', 'Nd', 10, 'TAKRI DIGIT ZERO..TAKRI DIGIT NINE').

% Total code points: 66

% EOF
