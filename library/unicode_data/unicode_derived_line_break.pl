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
# DerivedLineBreak-6.2.0.txt
# Date: 2012-08-13, 19:20:17 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2012 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

# ================================================

# Property:	Line_Break

#  All code points not explicitly listed for Line_Break
#  have the value Unknown (XX).

# @missing: 0000..10FFFF, 'Unknown
*/

% ================================================

unicode_line_break(CodePoint, Break) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_line_break(CodePointStart, CodePointEnd, Break),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_line_break(CodePoint, _, CodePointBreak) ->
		Break = CodePointBreak
	;	% look for a code point range that includes the given code point
		unicode_line_break(CodePointStart, CodePointEnd, CodePointBreak),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Break = CodePointBreak
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Break = 'XX'
	).

% Line_Break=Unknown

unicode_line_break(0xE000,   0xF8FF,   'XX'). % Co [6400] <private-use-E000>..<private-use-F8FF>
unicode_line_break(0xF0000,  0xFFFFD,  'XX'). % Co [65534] <private-use-F0000>..<private-use-FFFFD>
unicode_line_break(0x100000, 0x10FFFD, 'XX'). % Co [65534] <private-use-100000>..<private-use-10FFFD>

% The above property value applies to 780870 code points not listed here.
% Total code points: 918338

% ================================================

% Line_Break=Open_Punctuation

unicode_line_break(0x0028,  0x0028,  'OP'). % Ps       LEFT PARENTHESIS
unicode_line_break(0x005B,  0x005B,  'OP'). % Ps       LEFT SQUARE BRACKET
unicode_line_break(0x007B,  0x007B,  'OP'). % Ps       LEFT CURLY BRACKET
unicode_line_break(0x00A1,  0x00A1,  'OP'). % Po       INVERTED EXCLAMATION MARK
unicode_line_break(0x00BF,  0x00BF,  'OP'). % Po       INVERTED QUESTION MARK
unicode_line_break(0x0F3A,  0x0F3A,  'OP'). % Ps       TIBETAN MARK GUG RTAGS GYON
unicode_line_break(0x0F3C,  0x0F3C,  'OP'). % Ps       TIBETAN MARK ANG KHANG GYON
unicode_line_break(0x169B,  0x169B,  'OP'). % Ps       OGHAM FEATHER MARK
unicode_line_break(0x201A,  0x201A,  'OP'). % Ps       SINGLE LOW-9 QUOTATION MARK
unicode_line_break(0x201E,  0x201E,  'OP'). % Ps       DOUBLE LOW-9 QUOTATION MARK
unicode_line_break(0x2045,  0x2045,  'OP'). % Ps       LEFT SQUARE BRACKET WITH QUILL
unicode_line_break(0x207D,  0x207D,  'OP'). % Ps       SUPERSCRIPT LEFT PARENTHESIS
unicode_line_break(0x208D,  0x208D,  'OP'). % Ps       SUBSCRIPT LEFT PARENTHESIS
unicode_line_break(0x2329,  0x2329,  'OP'). % Ps       LEFT-POINTING ANGLE BRACKET
unicode_line_break(0x2768,  0x2768,  'OP'). % Ps       MEDIUM LEFT PARENTHESIS ORNAMENT
unicode_line_break(0x276A,  0x276A,  'OP'). % Ps       MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT
unicode_line_break(0x276C,  0x276C,  'OP'). % Ps       MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT
unicode_line_break(0x276E,  0x276E,  'OP'). % Ps       HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT
unicode_line_break(0x2770,  0x2770,  'OP'). % Ps       HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT
unicode_line_break(0x2772,  0x2772,  'OP'). % Ps       LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT
unicode_line_break(0x2774,  0x2774,  'OP'). % Ps       MEDIUM LEFT CURLY BRACKET ORNAMENT
unicode_line_break(0x27C5,  0x27C5,  'OP'). % Ps       LEFT S-SHAPED BAG DELIMITER
unicode_line_break(0x27E6,  0x27E6,  'OP'). % Ps       MATHEMATICAL LEFT WHITE SQUARE BRACKET
unicode_line_break(0x27E8,  0x27E8,  'OP'). % Ps       MATHEMATICAL LEFT ANGLE BRACKET
unicode_line_break(0x27EA,  0x27EA,  'OP'). % Ps       MATHEMATICAL LEFT DOUBLE ANGLE BRACKET
unicode_line_break(0x27EC,  0x27EC,  'OP'). % Ps       MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET
unicode_line_break(0x27EE,  0x27EE,  'OP'). % Ps       MATHEMATICAL LEFT FLATTENED PARENTHESIS
unicode_line_break(0x2983,  0x2983,  'OP'). % Ps       LEFT WHITE CURLY BRACKET
unicode_line_break(0x2985,  0x2985,  'OP'). % Ps       LEFT WHITE PARENTHESIS
unicode_line_break(0x2987,  0x2987,  'OP'). % Ps       Z NOTATION LEFT IMAGE BRACKET
unicode_line_break(0x2989,  0x2989,  'OP'). % Ps       Z NOTATION LEFT BINDING BRACKET
unicode_line_break(0x298B,  0x298B,  'OP'). % Ps       LEFT SQUARE BRACKET WITH UNDERBAR
unicode_line_break(0x298D,  0x298D,  'OP'). % Ps       LEFT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_line_break(0x298F,  0x298F,  'OP'). % Ps       LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_line_break(0x2991,  0x2991,  'OP'). % Ps       LEFT ANGLE BRACKET WITH DOT
unicode_line_break(0x2993,  0x2993,  'OP'). % Ps       LEFT ARC LESS-THAN BRACKET
unicode_line_break(0x2995,  0x2995,  'OP'). % Ps       DOUBLE LEFT ARC GREATER-THAN BRACKET
unicode_line_break(0x2997,  0x2997,  'OP'). % Ps       LEFT BLACK TORTOISE SHELL BRACKET
unicode_line_break(0x29D8,  0x29D8,  'OP'). % Ps       LEFT WIGGLY FENCE
unicode_line_break(0x29DA,  0x29DA,  'OP'). % Ps       LEFT DOUBLE WIGGLY FENCE
unicode_line_break(0x29FC,  0x29FC,  'OP'). % Ps       LEFT-POINTING CURVED ANGLE BRACKET
unicode_line_break(0x2E18,  0x2E18,  'OP'). % Po       INVERTED INTERROBANG
unicode_line_break(0x2E22,  0x2E22,  'OP'). % Ps       TOP LEFT HALF BRACKET
unicode_line_break(0x2E24,  0x2E24,  'OP'). % Ps       BOTTOM LEFT HALF BRACKET
unicode_line_break(0x2E26,  0x2E26,  'OP'). % Ps       LEFT SIDEWAYS U BRACKET
unicode_line_break(0x2E28,  0x2E28,  'OP'). % Ps       LEFT DOUBLE PARENTHESIS
unicode_line_break(0x3008,  0x3008,  'OP'). % Ps       LEFT ANGLE BRACKET
unicode_line_break(0x300A,  0x300A,  'OP'). % Ps       LEFT DOUBLE ANGLE BRACKET
unicode_line_break(0x300C,  0x300C,  'OP'). % Ps       LEFT CORNER BRACKET
unicode_line_break(0x300E,  0x300E,  'OP'). % Ps       LEFT WHITE CORNER BRACKET
unicode_line_break(0x3010,  0x3010,  'OP'). % Ps       LEFT BLACK LENTICULAR BRACKET
unicode_line_break(0x3014,  0x3014,  'OP'). % Ps       LEFT TORTOISE SHELL BRACKET
unicode_line_break(0x3016,  0x3016,  'OP'). % Ps       LEFT WHITE LENTICULAR BRACKET
unicode_line_break(0x3018,  0x3018,  'OP'). % Ps       LEFT WHITE TORTOISE SHELL BRACKET
unicode_line_break(0x301A,  0x301A,  'OP'). % Ps       LEFT WHITE SQUARE BRACKET
unicode_line_break(0x301D,  0x301D,  'OP'). % Ps       REVERSED DOUBLE PRIME QUOTATION MARK
unicode_line_break(0xFD3E,  0xFD3E,  'OP'). % Ps       ORNATE LEFT PARENTHESIS
unicode_line_break(0xFE17,  0xFE17,  'OP'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE LENTICULAR BRACKET
unicode_line_break(0xFE35,  0xFE35,  'OP'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
unicode_line_break(0xFE37,  0xFE37,  'OP'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
unicode_line_break(0xFE39,  0xFE39,  'OP'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
unicode_line_break(0xFE3B,  0xFE3B,  'OP'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
unicode_line_break(0xFE3D,  0xFE3D,  'OP'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
unicode_line_break(0xFE3F,  0xFE3F,  'OP'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
unicode_line_break(0xFE41,  0xFE41,  'OP'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
unicode_line_break(0xFE43,  0xFE43,  'OP'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
unicode_line_break(0xFE47,  0xFE47,  'OP'). % Ps       PRESENTATION FORM FOR VERTICAL LEFT SQUARE BRACKET
unicode_line_break(0xFE59,  0xFE59,  'OP'). % Ps       SMALL LEFT PARENTHESIS
unicode_line_break(0xFE5B,  0xFE5B,  'OP'). % Ps       SMALL LEFT CURLY BRACKET
unicode_line_break(0xFE5D,  0xFE5D,  'OP'). % Ps       SMALL LEFT TORTOISE SHELL BRACKET
unicode_line_break(0xFF08,  0xFF08,  'OP'). % Ps       FULLWIDTH LEFT PARENTHESIS
unicode_line_break(0xFF3B,  0xFF3B,  'OP'). % Ps       FULLWIDTH LEFT SQUARE BRACKET
unicode_line_break(0xFF5B,  0xFF5B,  'OP'). % Ps       FULLWIDTH LEFT CURLY BRACKET
unicode_line_break(0xFF5F,  0xFF5F,  'OP'). % Ps       FULLWIDTH LEFT WHITE PARENTHESIS
unicode_line_break(0xFF62,  0xFF62,  'OP'). % Ps       HALFWIDTH LEFT CORNER BRACKET
unicode_line_break(0x13258, 0x1325A, 'OP'). % Lo   [3] EGYPTIAN HIEROGLYPH O006A..EGYPTIAN HIEROGLYPH O006C
unicode_line_break(0x13286, 0x13286, 'OP'). % Lo       EGYPTIAN HIEROGLYPH O036A
unicode_line_break(0x13288, 0x13288, 'OP'). % Lo       EGYPTIAN HIEROGLYPH O036C
unicode_line_break(0x13379, 0x13379, 'OP'). % Lo       EGYPTIAN HIEROGLYPH V011A

% Total code points: 81

% ================================================

% Line_Break=Close_Punctuation

unicode_line_break(0x007D, 0x007D, 'CL'). % Pe       RIGHT CURLY BRACKET
unicode_line_break(0x0F3B, 0x0F3B, 'CL'). % Pe       TIBETAN MARK GUG RTAGS GYAS
unicode_line_break(0x0F3D, 0x0F3D, 'CL'). % Pe       TIBETAN MARK ANG KHANG GYAS
unicode_line_break(0x169C, 0x169C, 'CL'). % Pe       OGHAM REVERSED FEATHER MARK
unicode_line_break(0x2046, 0x2046, 'CL'). % Pe       RIGHT SQUARE BRACKET WITH QUILL
unicode_line_break(0x207E, 0x207E, 'CL'). % Pe       SUPERSCRIPT RIGHT PARENTHESIS
unicode_line_break(0x208E, 0x208E, 'CL'). % Pe       SUBSCRIPT RIGHT PARENTHESIS
unicode_line_break(0x232A, 0x232A, 'CL'). % Pe       RIGHT-POINTING ANGLE BRACKET
unicode_line_break(0x2769, 0x2769, 'CL'). % Pe       MEDIUM RIGHT PARENTHESIS ORNAMENT
unicode_line_break(0x276B, 0x276B, 'CL'). % Pe       MEDIUM FLATTENED RIGHT PARENTHESIS ORNAMENT
unicode_line_break(0x276D, 0x276D, 'CL'). % Pe       MEDIUM RIGHT-POINTING ANGLE BRACKET ORNAMENT
unicode_line_break(0x276F, 0x276F, 'CL'). % Pe       HEAVY RIGHT-POINTING ANGLE QUOTATION MARK ORNAMENT
unicode_line_break(0x2771, 0x2771, 'CL'). % Pe       HEAVY RIGHT-POINTING ANGLE BRACKET ORNAMENT
unicode_line_break(0x2773, 0x2773, 'CL'). % Pe       LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT
unicode_line_break(0x2775, 0x2775, 'CL'). % Pe       MEDIUM RIGHT CURLY BRACKET ORNAMENT
unicode_line_break(0x27C6, 0x27C6, 'CL'). % Pe       RIGHT S-SHAPED BAG DELIMITER
unicode_line_break(0x27E7, 0x27E7, 'CL'). % Pe       MATHEMATICAL RIGHT WHITE SQUARE BRACKET
unicode_line_break(0x27E9, 0x27E9, 'CL'). % Pe       MATHEMATICAL RIGHT ANGLE BRACKET
unicode_line_break(0x27EB, 0x27EB, 'CL'). % Pe       MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET
unicode_line_break(0x27ED, 0x27ED, 'CL'). % Pe       MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET
unicode_line_break(0x27EF, 0x27EF, 'CL'). % Pe       MATHEMATICAL RIGHT FLATTENED PARENTHESIS
unicode_line_break(0x2984, 0x2984, 'CL'). % Pe       RIGHT WHITE CURLY BRACKET
unicode_line_break(0x2986, 0x2986, 'CL'). % Pe       RIGHT WHITE PARENTHESIS
unicode_line_break(0x2988, 0x2988, 'CL'). % Pe       Z NOTATION RIGHT IMAGE BRACKET
unicode_line_break(0x298A, 0x298A, 'CL'). % Pe       Z NOTATION RIGHT BINDING BRACKET
unicode_line_break(0x298C, 0x298C, 'CL'). % Pe       RIGHT SQUARE BRACKET WITH UNDERBAR
unicode_line_break(0x298E, 0x298E, 'CL'). % Pe       RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_line_break(0x2990, 0x2990, 'CL'). % Pe       RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_line_break(0x2992, 0x2992, 'CL'). % Pe       RIGHT ANGLE BRACKET WITH DOT
unicode_line_break(0x2994, 0x2994, 'CL'). % Pe       RIGHT ARC GREATER-THAN BRACKET
unicode_line_break(0x2996, 0x2996, 'CL'). % Pe       DOUBLE RIGHT ARC LESS-THAN BRACKET
unicode_line_break(0x2998, 0x2998, 'CL'). % Pe       RIGHT BLACK TORTOISE SHELL BRACKET
unicode_line_break(0x29D9, 0x29D9, 'CL'). % Pe       RIGHT WIGGLY FENCE
unicode_line_break(0x29DB, 0x29DB, 'CL'). % Pe       RIGHT DOUBLE WIGGLY FENCE
unicode_line_break(0x29FD, 0x29FD, 'CL'). % Pe       RIGHT-POINTING CURVED ANGLE BRACKET
unicode_line_break(0x2E23, 0x2E23, 'CL'). % Pe       TOP RIGHT HALF BRACKET
unicode_line_break(0x2E25, 0x2E25, 'CL'). % Pe       BOTTOM RIGHT HALF BRACKET
unicode_line_break(0x2E27, 0x2E27, 'CL'). % Pe       RIGHT SIDEWAYS U BRACKET
unicode_line_break(0x2E29, 0x2E29, 'CL'). % Pe       RIGHT DOUBLE PARENTHESIS
unicode_line_break(0x3001, 0x3002, 'CL'). % Po   [2] IDEOGRAPHIC COMMA..IDEOGRAPHIC FULL STOP
unicode_line_break(0x3009, 0x3009, 'CL'). % Pe       RIGHT ANGLE BRACKET
unicode_line_break(0x300B, 0x300B, 'CL'). % Pe       RIGHT DOUBLE ANGLE BRACKET
unicode_line_break(0x300D, 0x300D, 'CL'). % Pe       RIGHT CORNER BRACKET
unicode_line_break(0x300F, 0x300F, 'CL'). % Pe       RIGHT WHITE CORNER BRACKET
unicode_line_break(0x3011, 0x3011, 'CL'). % Pe       RIGHT BLACK LENTICULAR BRACKET
unicode_line_break(0x3015, 0x3015, 'CL'). % Pe       RIGHT TORTOISE SHELL BRACKET
unicode_line_break(0x3017, 0x3017, 'CL'). % Pe       RIGHT WHITE LENTICULAR BRACKET
unicode_line_break(0x3019, 0x3019, 'CL'). % Pe       RIGHT WHITE TORTOISE SHELL BRACKET
unicode_line_break(0x301B, 0x301B, 'CL'). % Pe       RIGHT WHITE SQUARE BRACKET
unicode_line_break(0x301E, 0x301F, 'CL'). % Pe   [2] DOUBLE PRIME QUOTATION MARK..LOW DOUBLE PRIME QUOTATION MARK
unicode_line_break(0xFD3F, 0xFD3F, 'CL'). % Pe       ORNATE RIGHT PARENTHESIS
unicode_line_break(0xFE11, 0xFE12, 'CL'). % Po   [2] PRESENTATION FORM FOR VERTICAL IDEOGRAPHIC COMMA..PRESENTATION FORM FOR VERTICAL IDEOGRAPHIC FULL STOP
unicode_line_break(0xFE18, 0xFE18, 'CL'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET
unicode_line_break(0xFE36, 0xFE36, 'CL'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
unicode_line_break(0xFE38, 0xFE38, 'CL'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
unicode_line_break(0xFE3A, 0xFE3A, 'CL'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
unicode_line_break(0xFE3C, 0xFE3C, 'CL'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
unicode_line_break(0xFE3E, 0xFE3E, 'CL'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
unicode_line_break(0xFE40, 0xFE40, 'CL'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
unicode_line_break(0xFE42, 0xFE42, 'CL'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
unicode_line_break(0xFE44, 0xFE44, 'CL'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
unicode_line_break(0xFE48, 0xFE48, 'CL'). % Pe       PRESENTATION FORM FOR VERTICAL RIGHT SQUARE BRACKET
unicode_line_break(0xFE50, 0xFE50, 'CL'). % Po       SMALL COMMA
unicode_line_break(0xFE52, 0xFE52, 'CL'). % Po       SMALL FULL STOP
unicode_line_break(0xFE5A, 0xFE5A, 'CL'). % Pe       SMALL RIGHT PARENTHESIS
unicode_line_break(0xFE5C, 0xFE5C, 'CL'). % Pe       SMALL RIGHT CURLY BRACKET
unicode_line_break(0xFE5E, 0xFE5E, 'CL'). % Pe       SMALL RIGHT TORTOISE SHELL BRACKET
unicode_line_break(0xFF09, 0xFF09, 'CL'). % Pe       FULLWIDTH RIGHT PARENTHESIS
unicode_line_break(0xFF0C, 0xFF0C, 'CL'). % Po       FULLWIDTH COMMA
unicode_line_break(0xFF0E, 0xFF0E, 'CL'). % Po       FULLWIDTH FULL STOP
unicode_line_break(0xFF3D, 0xFF3D, 'CL'). % Pe       FULLWIDTH RIGHT SQUARE BRACKET
unicode_line_break(0xFF5D, 0xFF5D, 'CL'). % Pe       FULLWIDTH RIGHT CURLY BRACKET
unicode_line_break(0xFF60, 0xFF60, 'CL'). % Pe       FULLWIDTH RIGHT WHITE PARENTHESIS
unicode_line_break(0xFF61, 0xFF61, 'CL'). % Po       HALFWIDTH IDEOGRAPHIC FULL STOP
unicode_line_break(0xFF63, 0xFF63, 'CL'). % Pe       HALFWIDTH RIGHT CORNER BRACKET
unicode_line_break(0xFF64, 0xFF64, 'CL'). % Po       HALFWIDTH IDEOGRAPHIC COMMA
unicode_line_break(0x1325B, 0x1325D, 'CL'). % Lo   [3] EGYPTIAN HIEROGLYPH O006D..EGYPTIAN HIEROGLYPH O006F
unicode_line_break(0x13282, 0x13282, 'CL'). % Lo       EGYPTIAN HIEROGLYPH O033A
unicode_line_break(0x13287, 0x13287, 'CL'). % Lo       EGYPTIAN HIEROGLYPH O036B
unicode_line_break(0x13289, 0x13289, 'CL'). % Lo       EGYPTIAN HIEROGLYPH O036D
unicode_line_break(0x1337A, 0x1337B, 'CL'). % Lo   [2] EGYPTIAN HIEROGLYPH V011B..EGYPTIAN HIEROGLYPH V011C

% Total code points: 87

% ================================================

% Line_Break=Quotation

unicode_line_break(0x0022, 0x0022, 'QU'). % Po       QUOTATION MARK
unicode_line_break(0x0027, 0x0027, 'QU'). % Po       APOSTROPHE
unicode_line_break(0x00AB, 0x00AB, 'QU'). % Pi       LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_line_break(0x00BB, 0x00BB, 'QU'). % Pf       RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_line_break(0x2018, 0x2018, 'QU'). % Pi       LEFT SINGLE QUOTATION MARK
unicode_line_break(0x2019, 0x2019, 'QU'). % Pf       RIGHT SINGLE QUOTATION MARK
unicode_line_break(0x201B, 0x201C, 'QU'). % Pi   [2] SINGLE HIGH-REVERSED-9 QUOTATION MARK..LEFT DOUBLE QUOTATION MARK
unicode_line_break(0x201D, 0x201D, 'QU'). % Pf       RIGHT DOUBLE QUOTATION MARK
unicode_line_break(0x201F, 0x201F, 'QU'). % Pi       DOUBLE HIGH-REVERSED-9 QUOTATION MARK
unicode_line_break(0x2039, 0x2039, 'QU'). % Pi       SINGLE LEFT-POINTING ANGLE QUOTATION MARK
unicode_line_break(0x203A, 0x203A, 'QU'). % Pf       SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
unicode_line_break(0x275B, 0x275E, 'QU'). % So   [4] HEAVY SINGLE TURNED COMMA QUOTATION MARK ORNAMENT..HEAVY DOUBLE COMMA QUOTATION MARK ORNAMENT
unicode_line_break(0x2E00, 0x2E01, 'QU'). % Po   [2] RIGHT ANGLE SUBSTITUTION MARKER..RIGHT ANGLE DOTTED SUBSTITUTION MARKER
unicode_line_break(0x2E02, 0x2E02, 'QU'). % Pi       LEFT SUBSTITUTION BRACKET
unicode_line_break(0x2E03, 0x2E03, 'QU'). % Pf       RIGHT SUBSTITUTION BRACKET
unicode_line_break(0x2E04, 0x2E04, 'QU'). % Pi       LEFT DOTTED SUBSTITUTION BRACKET
unicode_line_break(0x2E05, 0x2E05, 'QU'). % Pf       RIGHT DOTTED SUBSTITUTION BRACKET
unicode_line_break(0x2E06, 0x2E08, 'QU'). % Po   [3] RAISED INTERPOLATION MARKER..DOTTED TRANSPOSITION MARKER
unicode_line_break(0x2E09, 0x2E09, 'QU'). % Pi       LEFT TRANSPOSITION BRACKET
unicode_line_break(0x2E0A, 0x2E0A, 'QU'). % Pf       RIGHT TRANSPOSITION BRACKET
unicode_line_break(0x2E0B, 0x2E0B, 'QU'). % Po       RAISED SQUARE
unicode_line_break(0x2E0C, 0x2E0C, 'QU'). % Pi       LEFT RAISED OMISSION BRACKET
unicode_line_break(0x2E0D, 0x2E0D, 'QU'). % Pf       RIGHT RAISED OMISSION BRACKET
unicode_line_break(0x2E1C, 0x2E1C, 'QU'). % Pi       LEFT LOW PARAPHRASE BRACKET
unicode_line_break(0x2E1D, 0x2E1D, 'QU'). % Pf       RIGHT LOW PARAPHRASE BRACKET
unicode_line_break(0x2E20, 0x2E20, 'QU'). % Pi       LEFT VERTICAL BAR WITH QUILL
unicode_line_break(0x2E21, 0x2E21, 'QU'). % Pf       RIGHT VERTICAL BAR WITH QUILL

% Total code points: 34

% ================================================

% Line_Break=Glue

unicode_line_break(0x00A0, 0x00A0, 'GL'). % Zs       NO-BREAK SPACE
unicode_line_break(0x034F, 0x034F, 'GL'). % Mn       COMBINING GRAPHEME JOINER
unicode_line_break(0x035C, 0x0362, 'GL'). % Mn   [7] COMBINING DOUBLE BREVE BELOW..COMBINING DOUBLE RIGHTWARDS ARROW BELOW
unicode_line_break(0x0F08, 0x0F08, 'GL'). % Po       TIBETAN MARK SBRUL SHAD
unicode_line_break(0x0F0C, 0x0F0C, 'GL'). % Po       TIBETAN MARK DELIMITER TSHEG BSTAR
unicode_line_break(0x0F12, 0x0F12, 'GL'). % Po       TIBETAN MARK RGYA GRAM SHAD
unicode_line_break(0x0FD9, 0x0FDA, 'GL'). % Po   [2] TIBETAN MARK LEADING MCHAN RTAGS..TIBETAN MARK TRAILING MCHAN RTAGS
unicode_line_break(0x180E, 0x180E, 'GL'). % Zs       MONGOLIAN VOWEL SEPARATOR
unicode_line_break(0x2007, 0x2007, 'GL'). % Zs       FIGURE SPACE
unicode_line_break(0x2011, 0x2011, 'GL'). % Pd       NON-BREAKING HYPHEN
unicode_line_break(0x202F, 0x202F, 'GL'). % Zs       NARROW NO-BREAK SPACE

% Total code points: 18

% ================================================

% Line_Break=Nonstarter

unicode_line_break(0x17D6, 0x17D6, 'NS'). % Po       KHMER SIGN CAMNUC PII KUUH
unicode_line_break(0x203C, 0x203D, 'NS'). % Po   [2] DOUBLE EXCLAMATION MARK..INTERROBANG
unicode_line_break(0x2047, 0x2049, 'NS'). % Po   [3] DOUBLE QUESTION MARK..EXCLAMATION QUESTION MARK
unicode_line_break(0x3005, 0x3005, 'NS'). % Lm       IDEOGRAPHIC ITERATION MARK
unicode_line_break(0x301C, 0x301C, 'NS'). % Pd       WAVE DASH
unicode_line_break(0x303B, 0x303B, 'NS'). % Lm       VERTICAL IDEOGRAPHIC ITERATION MARK
unicode_line_break(0x303C, 0x303C, 'NS'). % Lo       MASU MARK
unicode_line_break(0x309B, 0x309C, 'NS'). % Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_line_break(0x309D, 0x309E, 'NS'). % Lm   [2] HIRAGANA ITERATION MARK..HIRAGANA VOICED ITERATION MARK
unicode_line_break(0x30A0, 0x30A0, 'NS'). % Pd       KATAKANA-HIRAGANA DOUBLE HYPHEN
unicode_line_break(0x30FB, 0x30FB, 'NS'). % Po       KATAKANA MIDDLE DOT
unicode_line_break(0x30FD, 0x30FE, 'NS'). % Lm   [2] KATAKANA ITERATION MARK..KATAKANA VOICED ITERATION MARK
unicode_line_break(0xA015, 0xA015, 'NS'). % Lm       YI SYLLABLE WU
unicode_line_break(0xFE54, 0xFE55, 'NS'). % Po   [2] SMALL SEMICOLON..SMALL COLON
unicode_line_break(0xFF1A, 0xFF1B, 'NS'). % Po   [2] FULLWIDTH COLON..FULLWIDTH SEMICOLON
unicode_line_break(0xFF65, 0xFF65, 'NS'). % Po       HALFWIDTH KATAKANA MIDDLE DOT
unicode_line_break(0xFF9E, 0xFF9F, 'NS'). % Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK

% Total code points: 26

% ================================================

% Line_Break=Exclamation

unicode_line_break(0x0021, 0x0021, 'EX'). % Po       EXCLAMATION MARK
unicode_line_break(0x003F, 0x003F, 'EX'). % Po       QUESTION MARK
unicode_line_break(0x05C6, 0x05C6, 'EX'). % Po       HEBREW PUNCTUATION NUN HAFUKHA
unicode_line_break(0x061B, 0x061B, 'EX'). % Po       ARABIC SEMICOLON
unicode_line_break(0x061E, 0x061F, 'EX'). % Po   [2] ARABIC TRIPLE DOT PUNCTUATION MARK..ARABIC QUESTION MARK
unicode_line_break(0x06D4, 0x06D4, 'EX'). % Po       ARABIC FULL STOP
unicode_line_break(0x07F9, 0x07F9, 'EX'). % Po       NKO EXCLAMATION MARK
unicode_line_break(0x0F0D, 0x0F11, 'EX'). % Po   [5] TIBETAN MARK SHAD..TIBETAN MARK RIN CHEN SPUNGS SHAD
unicode_line_break(0x0F14, 0x0F14, 'EX'). % Po       TIBETAN MARK GTER TSHEG
unicode_line_break(0x1802, 0x1803, 'EX'). % Po   [2] MONGOLIAN COMMA..MONGOLIAN FULL STOP
unicode_line_break(0x1808, 0x1809, 'EX'). % Po   [2] MONGOLIAN MANCHU COMMA..MONGOLIAN MANCHU FULL STOP
unicode_line_break(0x1944, 0x1945, 'EX'). % Po   [2] LIMBU EXCLAMATION MARK..LIMBU QUESTION MARK
unicode_line_break(0x2762, 0x2763, 'EX'). % So   [2] HEAVY EXCLAMATION MARK ORNAMENT..HEAVY HEART EXCLAMATION MARK ORNAMENT
unicode_line_break(0x2CF9, 0x2CF9, 'EX'). % Po       COPTIC OLD NUBIAN FULL STOP
unicode_line_break(0x2CFE, 0x2CFE, 'EX'). % Po       COPTIC FULL STOP
unicode_line_break(0x2E2E, 0x2E2E, 'EX'). % Po       REVERSED QUESTION MARK
unicode_line_break(0xA60E, 0xA60E, 'EX'). % Po       VAI FULL STOP
unicode_line_break(0xA876, 0xA877, 'EX'). % Po   [2] PHAGS-PA MARK SHAD..PHAGS-PA MARK DOUBLE SHAD
unicode_line_break(0xFE15, 0xFE16, 'EX'). % Po   [2] PRESENTATION FORM FOR VERTICAL EXCLAMATION MARK..PRESENTATION FORM FOR VERTICAL QUESTION MARK
unicode_line_break(0xFE56, 0xFE57, 'EX'). % Po   [2] SMALL QUESTION MARK..SMALL EXCLAMATION MARK
unicode_line_break(0xFF01, 0xFF01, 'EX'). % Po       FULLWIDTH EXCLAMATION MARK
unicode_line_break(0xFF1F, 0xFF1F, 'EX'). % Po       FULLWIDTH QUESTION MARK

% Total code points: 34

% ================================================

% Line_Break=Break_Symbols

unicode_line_break(0x002F, 0x002F, 'SY'). % Po       SOLIDUS

% Total code points: 1

% ================================================

% Line_Break=Infix_Numeric

unicode_line_break(0x002C, 0x002C, 'IS'). % Po       COMMA
unicode_line_break(0x002E, 0x002E, 'IS'). % Po       FULL STOP
unicode_line_break(0x003A, 0x003B, 'IS'). % Po   [2] COLON..SEMICOLON
unicode_line_break(0x037E, 0x037E, 'IS'). % Po       GREEK QUESTION MARK
unicode_line_break(0x0589, 0x0589, 'IS'). % Po       ARMENIAN FULL STOP
unicode_line_break(0x060C, 0x060D, 'IS'). % Po   [2] ARABIC COMMA..ARABIC DATE SEPARATOR
unicode_line_break(0x07F8, 0x07F8, 'IS'). % Po       NKO COMMA
unicode_line_break(0x2044, 0x2044, 'IS'). % Sm       FRACTION SLASH
unicode_line_break(0xFE10, 0xFE10, 'IS'). % Po       PRESENTATION FORM FOR VERTICAL COMMA
unicode_line_break(0xFE13, 0xFE14, 'IS'). % Po   [2] PRESENTATION FORM FOR VERTICAL COLON..PRESENTATION FORM FOR VERTICAL SEMICOLON

% Total code points: 13

% ================================================

% Line_Break=Prefix_Numeric

unicode_line_break(0x0024, 0x0024, 'PR'). % Sc       DOLLAR SIGN
unicode_line_break(0x002B, 0x002B, 'PR'). % Sm       PLUS SIGN
unicode_line_break(0x005C, 0x005C, 'PR'). % Po       REVERSE SOLIDUS
unicode_line_break(0x00A3, 0x00A5, 'PR'). % Sc   [3] POUND SIGN..YEN SIGN
unicode_line_break(0x00B1, 0x00B1, 'PR'). % Sm       PLUS-MINUS SIGN
unicode_line_break(0x058F, 0x058F, 'PR'). % Sc       ARMENIAN DRAM SIGN
unicode_line_break(0x09FB, 0x09FB, 'PR'). % Sc       BENGALI GANDA MARK
unicode_line_break(0x0AF1, 0x0AF1, 'PR'). % Sc       GUJARATI RUPEE SIGN
unicode_line_break(0x0BF9, 0x0BF9, 'PR'). % Sc       TAMIL RUPEE SIGN
unicode_line_break(0x0E3F, 0x0E3F, 'PR'). % Sc       THAI CURRENCY SYMBOL BAHT
unicode_line_break(0x17DB, 0x17DB, 'PR'). % Sc       KHMER CURRENCY SYMBOL RIEL
unicode_line_break(0x20A0, 0x20A6, 'PR'). % Sc   [7] EURO-CURRENCY SIGN..NAIRA SIGN
unicode_line_break(0x20A8, 0x20B5, 'PR'). % Sc  [14] RUPEE SIGN..CEDI SIGN
unicode_line_break(0x20B7, 0x20BA, 'PR'). % Sc   [4] SPESMILO SIGN..TURKISH LIRA SIGN
unicode_line_break(0x2116, 0x2116, 'PR'). % So       NUMERO SIGN
unicode_line_break(0x2212, 0x2213, 'PR'). % Sm   [2] MINUS SIGN..MINUS-OR-PLUS SIGN
unicode_line_break(0xFE69, 0xFE69, 'PR'). % Sc       SMALL DOLLAR SIGN
unicode_line_break(0xFF04, 0xFF04, 'PR'). % Sc       FULLWIDTH DOLLAR SIGN
unicode_line_break(0xFFE1, 0xFFE1, 'PR'). % Sc       FULLWIDTH POUND SIGN
unicode_line_break(0xFFE5, 0xFFE6, 'PR'). % Sc   [2] FULLWIDTH YEN SIGN..FULLWIDTH WON SIGN

% Total code points: 45

% ================================================

% Line_Break=Postfix_Numeric

unicode_line_break(0x0025, 0x0025, 'PO'). % Po       PERCENT SIGN
unicode_line_break(0x00A2, 0x00A2, 'PO'). % Sc       CENT SIGN
unicode_line_break(0x00B0, 0x00B0, 'PO'). % So       DEGREE SIGN
unicode_line_break(0x0609, 0x060A, 'PO'). % Po   [2] ARABIC-INDIC PER MILLE SIGN..ARABIC-INDIC PER TEN THOUSAND SIGN
unicode_line_break(0x060B, 0x060B, 'PO'). % Sc       AFGHANI SIGN
unicode_line_break(0x066A, 0x066A, 'PO'). % Po       ARABIC PERCENT SIGN
unicode_line_break(0x09F2, 0x09F3, 'PO'). % Sc   [2] BENGALI RUPEE MARK..BENGALI RUPEE SIGN
unicode_line_break(0x09F9, 0x09F9, 'PO'). % No       BENGALI CURRENCY DENOMINATOR SIXTEEN
unicode_line_break(0x0D79, 0x0D79, 'PO'). % So       MALAYALAM DATE MARK
unicode_line_break(0x2030, 0x2037, 'PO'). % Po   [8] PER MILLE SIGN..REVERSED TRIPLE PRIME
unicode_line_break(0x20A7, 0x20A7, 'PO'). % Sc       PESETA SIGN
unicode_line_break(0x20B6, 0x20B6, 'PO'). % Sc       LIVRE TOURNOIS SIGN
unicode_line_break(0x2103, 0x2103, 'PO'). % So       DEGREE CELSIUS
unicode_line_break(0x2109, 0x2109, 'PO'). % So       DEGREE FAHRENHEIT
unicode_line_break(0xA838, 0xA838, 'PO'). % Sc       NORTH INDIC RUPEE MARK
unicode_line_break(0xFDFC, 0xFDFC, 'PO'). % Sc       RIAL SIGN
unicode_line_break(0xFE6A, 0xFE6A, 'PO'). % Po       SMALL PERCENT SIGN
unicode_line_break(0xFF05, 0xFF05, 'PO'). % Po       FULLWIDTH PERCENT SIGN
unicode_line_break(0xFFE0, 0xFFE0, 'PO'). % Sc       FULLWIDTH CENT SIGN

% Total code points: 28

% ================================================

% Line_Break=Numeric

unicode_line_break(0x0030, 0x0039, 'NU'). % Nd  [10] DIGIT ZERO..DIGIT NINE
unicode_line_break(0x0660, 0x0669, 'NU'). % Nd  [10] ARABIC-INDIC DIGIT ZERO..ARABIC-INDIC DIGIT NINE
unicode_line_break(0x066B, 0x066C, 'NU'). % Po   [2] ARABIC DECIMAL SEPARATOR..ARABIC THOUSANDS SEPARATOR
unicode_line_break(0x06F0, 0x06F9, 'NU'). % Nd  [10] EXTENDED ARABIC-INDIC DIGIT ZERO..EXTENDED ARABIC-INDIC DIGIT NINE
unicode_line_break(0x07C0, 0x07C9, 'NU'). % Nd  [10] NKO DIGIT ZERO..NKO DIGIT NINE
unicode_line_break(0x0966, 0x096F, 'NU'). % Nd  [10] DEVANAGARI DIGIT ZERO..DEVANAGARI DIGIT NINE
unicode_line_break(0x09E6, 0x09EF, 'NU'). % Nd  [10] BENGALI DIGIT ZERO..BENGALI DIGIT NINE
unicode_line_break(0x0A66, 0x0A6F, 'NU'). % Nd  [10] GURMUKHI DIGIT ZERO..GURMUKHI DIGIT NINE
unicode_line_break(0x0AE6, 0x0AEF, 'NU'). % Nd  [10] GUJARATI DIGIT ZERO..GUJARATI DIGIT NINE
unicode_line_break(0x0B66, 0x0B6F, 'NU'). % Nd  [10] ORIYA DIGIT ZERO..ORIYA DIGIT NINE
unicode_line_break(0x0BE6, 0x0BEF, 'NU'). % Nd  [10] TAMIL DIGIT ZERO..TAMIL DIGIT NINE
unicode_line_break(0x0C66, 0x0C6F, 'NU'). % Nd  [10] TELUGU DIGIT ZERO..TELUGU DIGIT NINE
unicode_line_break(0x0CE6, 0x0CEF, 'NU'). % Nd  [10] KANNADA DIGIT ZERO..KANNADA DIGIT NINE
unicode_line_break(0x0D66, 0x0D6F, 'NU'). % Nd  [10] MALAYALAM DIGIT ZERO..MALAYALAM DIGIT NINE
unicode_line_break(0x0E50, 0x0E59, 'NU'). % Nd  [10] THAI DIGIT ZERO..THAI DIGIT NINE
unicode_line_break(0x0ED0, 0x0ED9, 'NU'). % Nd  [10] LAO DIGIT ZERO..LAO DIGIT NINE
unicode_line_break(0x0F20, 0x0F29, 'NU'). % Nd  [10] TIBETAN DIGIT ZERO..TIBETAN DIGIT NINE
unicode_line_break(0x1040, 0x1049, 'NU'). % Nd  [10] MYANMAR DIGIT ZERO..MYANMAR DIGIT NINE
unicode_line_break(0x1090, 0x1099, 'NU'). % Nd  [10] MYANMAR SHAN DIGIT ZERO..MYANMAR SHAN DIGIT NINE
unicode_line_break(0x17E0, 0x17E9, 'NU'). % Nd  [10] KHMER DIGIT ZERO..KHMER DIGIT NINE
unicode_line_break(0x1810, 0x1819, 'NU'). % Nd  [10] MONGOLIAN DIGIT ZERO..MONGOLIAN DIGIT NINE
unicode_line_break(0x1946, 0x194F, 'NU'). % Nd  [10] LIMBU DIGIT ZERO..LIMBU DIGIT NINE
unicode_line_break(0x19D0, 0x19D9, 'NU'). % Nd  [10] NEW TAI LUE DIGIT ZERO..NEW TAI LUE DIGIT NINE
unicode_line_break(0x1A80, 0x1A89, 'NU'). % Nd  [10] TAI THAM HORA DIGIT ZERO..TAI THAM HORA DIGIT NINE
unicode_line_break(0x1A90, 0x1A99, 'NU'). % Nd  [10] TAI THAM THAM DIGIT ZERO..TAI THAM THAM DIGIT NINE
unicode_line_break(0x1B50, 0x1B59, 'NU'). % Nd  [10] BALINESE DIGIT ZERO..BALINESE DIGIT NINE
unicode_line_break(0x1BB0, 0x1BB9, 'NU'). % Nd  [10] SUNDANESE DIGIT ZERO..SUNDANESE DIGIT NINE
unicode_line_break(0x1C40, 0x1C49, 'NU'). % Nd  [10] LEPCHA DIGIT ZERO..LEPCHA DIGIT NINE
unicode_line_break(0x1C50, 0x1C59, 'NU'). % Nd  [10] OL CHIKI DIGIT ZERO..OL CHIKI DIGIT NINE
unicode_line_break(0xA620, 0xA629, 'NU'). % Nd  [10] VAI DIGIT ZERO..VAI DIGIT NINE
unicode_line_break(0xA8D0, 0xA8D9, 'NU'). % Nd  [10] SAURASHTRA DIGIT ZERO..SAURASHTRA DIGIT NINE
unicode_line_break(0xA900, 0xA909, 'NU'). % Nd  [10] KAYAH LI DIGIT ZERO..KAYAH LI DIGIT NINE
unicode_line_break(0xA9D0, 0xA9D9, 'NU'). % Nd  [10] JAVANESE DIGIT ZERO..JAVANESE DIGIT NINE
unicode_line_break(0xAA50, 0xAA59, 'NU'). % Nd  [10] CHAM DIGIT ZERO..CHAM DIGIT NINE
unicode_line_break(0xABF0, 0xABF9, 'NU'). % Nd  [10] MEETEI MAYEK DIGIT ZERO..MEETEI MAYEK DIGIT NINE
unicode_line_break(0x104A0, 0x104A9, 'NU'). % Nd  [10] OSMANYA DIGIT ZERO..OSMANYA DIGIT NINE
unicode_line_break(0x11066, 0x1106F, 'NU'). % Nd  [10] BRAHMI DIGIT ZERO..BRAHMI DIGIT NINE
unicode_line_break(0x110F0, 0x110F9, 'NU'). % Nd  [10] SORA SOMPENG DIGIT ZERO..SORA SOMPENG DIGIT NINE
unicode_line_break(0x11136, 0x1113F, 'NU'). % Nd  [10] CHAKMA DIGIT ZERO..CHAKMA DIGIT NINE
unicode_line_break(0x111D0, 0x111D9, 'NU'). % Nd  [10] SHARADA DIGIT ZERO..SHARADA DIGIT NINE
unicode_line_break(0x116C0, 0x116C9, 'NU'). % Nd  [10] TAKRI DIGIT ZERO..TAKRI DIGIT NINE
unicode_line_break(0x1D7CE, 0x1D7FF, 'NU'). % Nd  [50] MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE

% Total code points: 452

% ================================================

% Line_Break=Alphabetic

unicode_line_break(0x0023, 0x0023, 'AL'). % Po       NUMBER SIGN
unicode_line_break(0x0026, 0x0026, 'AL'). % Po       AMPERSAND
unicode_line_break(0x002A, 0x002A, 'AL'). % Po       ASTERISK
unicode_line_break(0x003C, 0x003E, 'AL'). % Sm   [3] LESS-THAN SIGN..GREATER-THAN SIGN
unicode_line_break(0x0040, 0x0040, 'AL'). % Po       COMMERCIAL AT
unicode_line_break(0x0041, 0x005A, 'AL'). % L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
unicode_line_break(0x005E, 0x005E, 'AL'). % Sk       CIRCUMFLEX ACCENT
unicode_line_break(0x005F, 0x005F, 'AL'). % Pc       LOW LINE
unicode_line_break(0x0060, 0x0060, 'AL'). % Sk       GRAVE ACCENT
unicode_line_break(0x0061, 0x007A, 'AL'). % L&  [26] LATIN SMALL LETTER A..LATIN SMALL LETTER Z
unicode_line_break(0x007E, 0x007E, 'AL'). % Sm       TILDE
unicode_line_break(0x00A6, 0x00A6, 'AL'). % So       BROKEN BAR
unicode_line_break(0x00A9, 0x00A9, 'AL'). % So       COPYRIGHT SIGN
unicode_line_break(0x00AC, 0x00AC, 'AL'). % Sm       NOT SIGN
unicode_line_break(0x00AE, 0x00AE, 'AL'). % So       REGISTERED SIGN
unicode_line_break(0x00AF, 0x00AF, 'AL'). % Sk       MACRON
unicode_line_break(0x00B5, 0x00B5, 'AL'). % L&       MICRO SIGN
unicode_line_break(0x00C0, 0x00D6, 'AL'). % L&  [23] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_line_break(0x00D8, 0x00F6, 'AL'). % L&  [31] LATIN CAPITAL LETTER O WITH STROKE..LATIN SMALL LETTER O WITH DIAERESIS
unicode_line_break(0x00F8, 0x01BA, 'AL'). % L& [195] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER EZH WITH TAIL
unicode_line_break(0x01BB, 0x01BB, 'AL'). % Lo       LATIN LETTER TWO WITH STROKE
unicode_line_break(0x01BC, 0x01BF, 'AL'). % L&   [4] LATIN CAPITAL LETTER TONE FIVE..LATIN LETTER WYNN
unicode_line_break(0x01C0, 0x01C3, 'AL'). % Lo   [4] LATIN LETTER DENTAL CLICK..LATIN LETTER RETROFLEX CLICK
unicode_line_break(0x01C4, 0x0293, 'AL'). % L& [208] LATIN CAPITAL LETTER DZ WITH CARON..LATIN SMALL LETTER EZH WITH CURL
unicode_line_break(0x0294, 0x0294, 'AL'). % Lo       LATIN LETTER GLOTTAL STOP
unicode_line_break(0x0295, 0x02AF, 'AL'). % L&  [27] LATIN LETTER PHARYNGEAL VOICED FRICATIVE..LATIN SMALL LETTER TURNED H WITH FISHHOOK AND TAIL
unicode_line_break(0x02B0, 0x02C1, 'AL'). % Lm  [18] MODIFIER LETTER SMALL H..MODIFIER LETTER REVERSED GLOTTAL STOP
unicode_line_break(0x02C2, 0x02C5, 'AL'). % Sk   [4] MODIFIER LETTER LEFT ARROWHEAD..MODIFIER LETTER DOWN ARROWHEAD
unicode_line_break(0x02C6, 0x02C6, 'AL'). % Lm       MODIFIER LETTER CIRCUMFLEX ACCENT
unicode_line_break(0x02CE, 0x02CF, 'AL'). % Lm   [2] MODIFIER LETTER LOW GRAVE ACCENT..MODIFIER LETTER LOW ACUTE ACCENT
unicode_line_break(0x02D1, 0x02D1, 'AL'). % Lm       MODIFIER LETTER HALF TRIANGULAR COLON
unicode_line_break(0x02D2, 0x02D7, 'AL'). % Sk   [6] MODIFIER LETTER CENTRED RIGHT HALF RING..MODIFIER LETTER MINUS SIGN
unicode_line_break(0x02DC, 0x02DC, 'AL'). % Sk       SMALL TILDE
unicode_line_break(0x02DE, 0x02DE, 'AL'). % Sk       MODIFIER LETTER RHOTIC HOOK
unicode_line_break(0x02E0, 0x02E4, 'AL'). % Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_line_break(0x02E5, 0x02EB, 'AL'). % Sk   [7] MODIFIER LETTER EXTRA-HIGH TONE BAR..MODIFIER LETTER YANG DEPARTING TONE MARK
unicode_line_break(0x02EC, 0x02EC, 'AL'). % Lm       MODIFIER LETTER VOICING
unicode_line_break(0x02ED, 0x02ED, 'AL'). % Sk       MODIFIER LETTER UNASPIRATED
unicode_line_break(0x02EE, 0x02EE, 'AL'). % Lm       MODIFIER LETTER DOUBLE APOSTROPHE
unicode_line_break(0x02EF, 0x02FF, 'AL'). % Sk  [17] MODIFIER LETTER LOW DOWN ARROWHEAD..MODIFIER LETTER LOW LEFT ARROW
unicode_line_break(0x0370, 0x0373, 'AL'). % L&   [4] GREEK CAPITAL LETTER HETA..GREEK SMALL LETTER ARCHAIC SAMPI
unicode_line_break(0x0374, 0x0374, 'AL'). % Lm       GREEK NUMERAL SIGN
unicode_line_break(0x0375, 0x0375, 'AL'). % Sk       GREEK LOWER NUMERAL SIGN
unicode_line_break(0x0376, 0x0377, 'AL'). % L&   [2] GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA..GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
unicode_line_break(0x037A, 0x037A, 'AL'). % Lm       GREEK YPOGEGRAMMENI
unicode_line_break(0x037B, 0x037D, 'AL'). % L&   [3] GREEK SMALL REVERSED LUNATE SIGMA SYMBOL..GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
unicode_line_break(0x0384, 0x0385, 'AL'). % Sk   [2] GREEK TONOS..GREEK DIALYTIKA TONOS
unicode_line_break(0x0386, 0x0386, 'AL'). % L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_line_break(0x0387, 0x0387, 'AL'). % Po       GREEK ANO TELEIA
unicode_line_break(0x0388, 0x038A, 'AL'). % L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_line_break(0x038C, 0x038C, 'AL'). % L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_line_break(0x038E, 0x03A1, 'AL'). % L&  [20] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK CAPITAL LETTER RHO
unicode_line_break(0x03A3, 0x03F5, 'AL'). % L&  [83] GREEK CAPITAL LETTER SIGMA..GREEK LUNATE EPSILON SYMBOL
unicode_line_break(0x03F6, 0x03F6, 'AL'). % Sm       GREEK REVERSED LUNATE EPSILON SYMBOL
unicode_line_break(0x03F7, 0x0481, 'AL'). % L& [139] GREEK CAPITAL LETTER SHO..CYRILLIC SMALL LETTER KOPPA
unicode_line_break(0x0482, 0x0482, 'AL'). % So       CYRILLIC THOUSANDS SIGN
unicode_line_break(0x048A, 0x0527, 'AL'). % L& [158] CYRILLIC CAPITAL LETTER SHORT I WITH TAIL..CYRILLIC SMALL LETTER SHHA WITH DESCENDER
unicode_line_break(0x0531, 0x0556, 'AL'). % L&  [38] ARMENIAN CAPITAL LETTER AYB..ARMENIAN CAPITAL LETTER FEH
unicode_line_break(0x0559, 0x0559, 'AL'). % Lm       ARMENIAN MODIFIER LETTER LEFT HALF RING
unicode_line_break(0x055A, 0x055F, 'AL'). % Po   [6] ARMENIAN APOSTROPHE..ARMENIAN ABBREVIATION MARK
unicode_line_break(0x0561, 0x0587, 'AL'). % L&  [39] ARMENIAN SMALL LETTER AYB..ARMENIAN SMALL LIGATURE ECH YIWN
unicode_line_break(0x05C0, 0x05C0, 'AL'). % Po       HEBREW PUNCTUATION PASEQ
unicode_line_break(0x05C3, 0x05C3, 'AL'). % Po       HEBREW PUNCTUATION SOF PASUQ
unicode_line_break(0x05F3, 0x05F4, 'AL'). % Po   [2] HEBREW PUNCTUATION GERESH..HEBREW PUNCTUATION GERSHAYIM
unicode_line_break(0x0600, 0x0604, 'AL'). % Cf   [5] ARABIC NUMBER SIGN..ARABIC SIGN SAMVAT
unicode_line_break(0x0606, 0x0608, 'AL'). % Sm   [3] ARABIC-INDIC CUBE ROOT..ARABIC RAY
unicode_line_break(0x060E, 0x060F, 'AL'). % So   [2] ARABIC POETIC VERSE SIGN..ARABIC SIGN MISRA
unicode_line_break(0x0620, 0x063F, 'AL'). % Lo  [32] ARABIC LETTER KASHMIRI YEH..ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE
unicode_line_break(0x0640, 0x0640, 'AL'). % Lm       ARABIC TATWEEL
unicode_line_break(0x0641, 0x064A, 'AL'). % Lo  [10] ARABIC LETTER FEH..ARABIC LETTER YEH
unicode_line_break(0x066D, 0x066D, 'AL'). % Po       ARABIC FIVE POINTED STAR
unicode_line_break(0x066E, 0x066F, 'AL'). % Lo   [2] ARABIC LETTER DOTLESS BEH..ARABIC LETTER DOTLESS QAF
unicode_line_break(0x0671, 0x06D3, 'AL'). % Lo  [99] ARABIC LETTER ALEF WASLA..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
unicode_line_break(0x06D5, 0x06D5, 'AL'). % Lo       ARABIC LETTER AE
unicode_line_break(0x06DD, 0x06DD, 'AL'). % Cf       ARABIC END OF AYAH
unicode_line_break(0x06DE, 0x06DE, 'AL'). % So       ARABIC START OF RUB EL HIZB
unicode_line_break(0x06E5, 0x06E6, 'AL'). % Lm   [2] ARABIC SMALL WAW..ARABIC SMALL YEH
unicode_line_break(0x06E9, 0x06E9, 'AL'). % So       ARABIC PLACE OF SAJDAH
unicode_line_break(0x06EE, 0x06EF, 'AL'). % Lo   [2] ARABIC LETTER DAL WITH INVERTED V..ARABIC LETTER REH WITH INVERTED V
unicode_line_break(0x06FA, 0x06FC, 'AL'). % Lo   [3] ARABIC LETTER SHEEN WITH DOT BELOW..ARABIC LETTER GHAIN WITH DOT BELOW
unicode_line_break(0x06FD, 0x06FE, 'AL'). % So   [2] ARABIC SIGN SINDHI AMPERSAND..ARABIC SIGN SINDHI POSTPOSITION MEN
unicode_line_break(0x06FF, 0x06FF, 'AL'). % Lo       ARABIC LETTER HEH WITH INVERTED V
unicode_line_break(0x0700, 0x070D, 'AL'). % Po  [14] SYRIAC END OF PARAGRAPH..SYRIAC HARKLEAN ASTERISCUS
unicode_line_break(0x070F, 0x070F, 'AL'). % Cf       SYRIAC ABBREVIATION MARK
unicode_line_break(0x0710, 0x0710, 'AL'). % Lo       SYRIAC LETTER ALAPH
unicode_line_break(0x0712, 0x072F, 'AL'). % Lo  [30] SYRIAC LETTER BETH..SYRIAC LETTER PERSIAN DHALATH
unicode_line_break(0x074D, 0x07A5, 'AL'). % Lo  [89] SYRIAC LETTER SOGDIAN ZHAIN..THAANA LETTER WAAVU
unicode_line_break(0x07B1, 0x07B1, 'AL'). % Lo       THAANA LETTER NAA
unicode_line_break(0x07CA, 0x07EA, 'AL'). % Lo  [33] NKO LETTER A..NKO LETTER JONA RA
unicode_line_break(0x07F4, 0x07F5, 'AL'). % Lm   [2] NKO HIGH TONE APOSTROPHE..NKO LOW TONE APOSTROPHE
unicode_line_break(0x07F6, 0x07F6, 'AL'). % So       NKO SYMBOL OO DENNEN
unicode_line_break(0x07F7, 0x07F7, 'AL'). % Po       NKO SYMBOL GBAKURUNEN
unicode_line_break(0x07FA, 0x07FA, 'AL'). % Lm       NKO LAJANYALAN
unicode_line_break(0x0800, 0x0815, 'AL'). % Lo  [22] SAMARITAN LETTER ALAF..SAMARITAN LETTER TAAF
unicode_line_break(0x081A, 0x081A, 'AL'). % Lm       SAMARITAN MODIFIER LETTER EPENTHETIC YUT
unicode_line_break(0x0824, 0x0824, 'AL'). % Lm       SAMARITAN MODIFIER LETTER SHORT A
unicode_line_break(0x0828, 0x0828, 'AL'). % Lm       SAMARITAN MODIFIER LETTER I
unicode_line_break(0x0830, 0x083E, 'AL'). % Po  [15] SAMARITAN PUNCTUATION NEQUDAA..SAMARITAN PUNCTUATION ANNAAU
unicode_line_break(0x0840, 0x0858, 'AL'). % Lo  [25] MANDAIC LETTER HALQA..MANDAIC LETTER AIN
unicode_line_break(0x085E, 0x085E, 'AL'). % Po       MANDAIC PUNCTUATION
unicode_line_break(0x08A0, 0x08A0, 'AL'). % Lo       ARABIC LETTER BEH WITH SMALL V BELOW
unicode_line_break(0x08A2, 0x08AC, 'AL'). % Lo  [11] ARABIC LETTER JEEM WITH TWO DOTS ABOVE..ARABIC LETTER ROHINGYA YEH
unicode_line_break(0x0904, 0x0939, 'AL'). % Lo  [54] DEVANAGARI LETTER SHORT A..DEVANAGARI LETTER HA
unicode_line_break(0x093D, 0x093D, 'AL'). % Lo       DEVANAGARI SIGN AVAGRAHA
unicode_line_break(0x0950, 0x0950, 'AL'). % Lo       DEVANAGARI OM
unicode_line_break(0x0958, 0x0961, 'AL'). % Lo  [10] DEVANAGARI LETTER QA..DEVANAGARI LETTER VOCALIC LL
unicode_line_break(0x0970, 0x0970, 'AL'). % Po       DEVANAGARI ABBREVIATION SIGN
unicode_line_break(0x0971, 0x0971, 'AL'). % Lm       DEVANAGARI SIGN HIGH SPACING DOT
unicode_line_break(0x0972, 0x0977, 'AL'). % Lo   [6] DEVANAGARI LETTER CANDRA A..DEVANAGARI LETTER UUE
unicode_line_break(0x0979, 0x097F, 'AL'). % Lo   [7] DEVANAGARI LETTER ZHA..DEVANAGARI LETTER BBA
unicode_line_break(0x0985, 0x098C, 'AL'). % Lo   [8] BENGALI LETTER A..BENGALI LETTER VOCALIC L
unicode_line_break(0x098F, 0x0990, 'AL'). % Lo   [2] BENGALI LETTER E..BENGALI LETTER AI
unicode_line_break(0x0993, 0x09A8, 'AL'). % Lo  [22] BENGALI LETTER O..BENGALI LETTER NA
unicode_line_break(0x09AA, 0x09B0, 'AL'). % Lo   [7] BENGALI LETTER PA..BENGALI LETTER RA
unicode_line_break(0x09B2, 0x09B2, 'AL'). % Lo       BENGALI LETTER LA
unicode_line_break(0x09B6, 0x09B9, 'AL'). % Lo   [4] BENGALI LETTER SHA..BENGALI LETTER HA
unicode_line_break(0x09BD, 0x09BD, 'AL'). % Lo       BENGALI SIGN AVAGRAHA
unicode_line_break(0x09CE, 0x09CE, 'AL'). % Lo       BENGALI LETTER KHANDA TA
unicode_line_break(0x09DC, 0x09DD, 'AL'). % Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_line_break(0x09DF, 0x09E1, 'AL'). % Lo   [3] BENGALI LETTER YYA..BENGALI LETTER VOCALIC LL
unicode_line_break(0x09F0, 0x09F1, 'AL'). % Lo   [2] BENGALI LETTER RA WITH MIDDLE DIAGONAL..BENGALI LETTER RA WITH LOWER DIAGONAL
unicode_line_break(0x09F4, 0x09F8, 'AL'). % No   [5] BENGALI CURRENCY NUMERATOR ONE..BENGALI CURRENCY NUMERATOR ONE LESS THAN THE DENOMINATOR
unicode_line_break(0x09FA, 0x09FA, 'AL'). % So       BENGALI ISSHAR
unicode_line_break(0x0A05, 0x0A0A, 'AL'). % Lo   [6] GURMUKHI LETTER A..GURMUKHI LETTER UU
unicode_line_break(0x0A0F, 0x0A10, 'AL'). % Lo   [2] GURMUKHI LETTER EE..GURMUKHI LETTER AI
unicode_line_break(0x0A13, 0x0A28, 'AL'). % Lo  [22] GURMUKHI LETTER OO..GURMUKHI LETTER NA
unicode_line_break(0x0A2A, 0x0A30, 'AL'). % Lo   [7] GURMUKHI LETTER PA..GURMUKHI LETTER RA
unicode_line_break(0x0A32, 0x0A33, 'AL'). % Lo   [2] GURMUKHI LETTER LA..GURMUKHI LETTER LLA
unicode_line_break(0x0A35, 0x0A36, 'AL'). % Lo   [2] GURMUKHI LETTER VA..GURMUKHI LETTER SHA
unicode_line_break(0x0A38, 0x0A39, 'AL'). % Lo   [2] GURMUKHI LETTER SA..GURMUKHI LETTER HA
unicode_line_break(0x0A59, 0x0A5C, 'AL'). % Lo   [4] GURMUKHI LETTER KHHA..GURMUKHI LETTER RRA
unicode_line_break(0x0A5E, 0x0A5E, 'AL'). % Lo       GURMUKHI LETTER FA
unicode_line_break(0x0A72, 0x0A74, 'AL'). % Lo   [3] GURMUKHI IRI..GURMUKHI EK ONKAR
unicode_line_break(0x0A85, 0x0A8D, 'AL'). % Lo   [9] GUJARATI LETTER A..GUJARATI VOWEL CANDRA E
unicode_line_break(0x0A8F, 0x0A91, 'AL'). % Lo   [3] GUJARATI LETTER E..GUJARATI VOWEL CANDRA O
unicode_line_break(0x0A93, 0x0AA8, 'AL'). % Lo  [22] GUJARATI LETTER O..GUJARATI LETTER NA
unicode_line_break(0x0AAA, 0x0AB0, 'AL'). % Lo   [7] GUJARATI LETTER PA..GUJARATI LETTER RA
unicode_line_break(0x0AB2, 0x0AB3, 'AL'). % Lo   [2] GUJARATI LETTER LA..GUJARATI LETTER LLA
unicode_line_break(0x0AB5, 0x0AB9, 'AL'). % Lo   [5] GUJARATI LETTER VA..GUJARATI LETTER HA
unicode_line_break(0x0ABD, 0x0ABD, 'AL'). % Lo       GUJARATI SIGN AVAGRAHA
unicode_line_break(0x0AD0, 0x0AD0, 'AL'). % Lo       GUJARATI OM
unicode_line_break(0x0AE0, 0x0AE1, 'AL'). % Lo   [2] GUJARATI LETTER VOCALIC RR..GUJARATI LETTER VOCALIC LL
unicode_line_break(0x0AF0, 0x0AF0, 'AL'). % Po       GUJARATI ABBREVIATION SIGN
unicode_line_break(0x0B05, 0x0B0C, 'AL'). % Lo   [8] ORIYA LETTER A..ORIYA LETTER VOCALIC L
unicode_line_break(0x0B0F, 0x0B10, 'AL'). % Lo   [2] ORIYA LETTER E..ORIYA LETTER AI
unicode_line_break(0x0B13, 0x0B28, 'AL'). % Lo  [22] ORIYA LETTER O..ORIYA LETTER NA
unicode_line_break(0x0B2A, 0x0B30, 'AL'). % Lo   [7] ORIYA LETTER PA..ORIYA LETTER RA
unicode_line_break(0x0B32, 0x0B33, 'AL'). % Lo   [2] ORIYA LETTER LA..ORIYA LETTER LLA
unicode_line_break(0x0B35, 0x0B39, 'AL'). % Lo   [5] ORIYA LETTER VA..ORIYA LETTER HA
unicode_line_break(0x0B3D, 0x0B3D, 'AL'). % Lo       ORIYA SIGN AVAGRAHA
unicode_line_break(0x0B5C, 0x0B5D, 'AL'). % Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_line_break(0x0B5F, 0x0B61, 'AL'). % Lo   [3] ORIYA LETTER YYA..ORIYA LETTER VOCALIC LL
unicode_line_break(0x0B70, 0x0B70, 'AL'). % So       ORIYA ISSHAR
unicode_line_break(0x0B71, 0x0B71, 'AL'). % Lo       ORIYA LETTER WA
unicode_line_break(0x0B72, 0x0B77, 'AL'). % No   [6] ORIYA FRACTION ONE QUARTER..ORIYA FRACTION THREE SIXTEENTHS
unicode_line_break(0x0B83, 0x0B83, 'AL'). % Lo       TAMIL SIGN VISARGA
unicode_line_break(0x0B85, 0x0B8A, 'AL'). % Lo   [6] TAMIL LETTER A..TAMIL LETTER UU
unicode_line_break(0x0B8E, 0x0B90, 'AL'). % Lo   [3] TAMIL LETTER E..TAMIL LETTER AI
unicode_line_break(0x0B92, 0x0B95, 'AL'). % Lo   [4] TAMIL LETTER O..TAMIL LETTER KA
unicode_line_break(0x0B99, 0x0B9A, 'AL'). % Lo   [2] TAMIL LETTER NGA..TAMIL LETTER CA
unicode_line_break(0x0B9C, 0x0B9C, 'AL'). % Lo       TAMIL LETTER JA
unicode_line_break(0x0B9E, 0x0B9F, 'AL'). % Lo   [2] TAMIL LETTER NYA..TAMIL LETTER TTA
unicode_line_break(0x0BA3, 0x0BA4, 'AL'). % Lo   [2] TAMIL LETTER NNA..TAMIL LETTER TA
unicode_line_break(0x0BA8, 0x0BAA, 'AL'). % Lo   [3] TAMIL LETTER NA..TAMIL LETTER PA
unicode_line_break(0x0BAE, 0x0BB9, 'AL'). % Lo  [12] TAMIL LETTER MA..TAMIL LETTER HA
unicode_line_break(0x0BD0, 0x0BD0, 'AL'). % Lo       TAMIL OM
unicode_line_break(0x0BF0, 0x0BF2, 'AL'). % No   [3] TAMIL NUMBER TEN..TAMIL NUMBER ONE THOUSAND
unicode_line_break(0x0BF3, 0x0BF8, 'AL'). % So   [6] TAMIL DAY SIGN..TAMIL AS ABOVE SIGN
unicode_line_break(0x0BFA, 0x0BFA, 'AL'). % So       TAMIL NUMBER SIGN
unicode_line_break(0x0C05, 0x0C0C, 'AL'). % Lo   [8] TELUGU LETTER A..TELUGU LETTER VOCALIC L
unicode_line_break(0x0C0E, 0x0C10, 'AL'). % Lo   [3] TELUGU LETTER E..TELUGU LETTER AI
unicode_line_break(0x0C12, 0x0C28, 'AL'). % Lo  [23] TELUGU LETTER O..TELUGU LETTER NA
unicode_line_break(0x0C2A, 0x0C33, 'AL'). % Lo  [10] TELUGU LETTER PA..TELUGU LETTER LLA
unicode_line_break(0x0C35, 0x0C39, 'AL'). % Lo   [5] TELUGU LETTER VA..TELUGU LETTER HA
unicode_line_break(0x0C3D, 0x0C3D, 'AL'). % Lo       TELUGU SIGN AVAGRAHA
unicode_line_break(0x0C58, 0x0C59, 'AL'). % Lo   [2] TELUGU LETTER TSA..TELUGU LETTER DZA
unicode_line_break(0x0C60, 0x0C61, 'AL'). % Lo   [2] TELUGU LETTER VOCALIC RR..TELUGU LETTER VOCALIC LL
unicode_line_break(0x0C78, 0x0C7E, 'AL'). % No   [7] TELUGU FRACTION DIGIT ZERO FOR ODD POWERS OF FOUR..TELUGU FRACTION DIGIT THREE FOR EVEN POWERS OF FOUR
unicode_line_break(0x0C7F, 0x0C7F, 'AL'). % So       TELUGU SIGN TUUMU
unicode_line_break(0x0C85, 0x0C8C, 'AL'). % Lo   [8] KANNADA LETTER A..KANNADA LETTER VOCALIC L
unicode_line_break(0x0C8E, 0x0C90, 'AL'). % Lo   [3] KANNADA LETTER E..KANNADA LETTER AI
unicode_line_break(0x0C92, 0x0CA8, 'AL'). % Lo  [23] KANNADA LETTER O..KANNADA LETTER NA
unicode_line_break(0x0CAA, 0x0CB3, 'AL'). % Lo  [10] KANNADA LETTER PA..KANNADA LETTER LLA
unicode_line_break(0x0CB5, 0x0CB9, 'AL'). % Lo   [5] KANNADA LETTER VA..KANNADA LETTER HA
unicode_line_break(0x0CBD, 0x0CBD, 'AL'). % Lo       KANNADA SIGN AVAGRAHA
unicode_line_break(0x0CDE, 0x0CDE, 'AL'). % Lo       KANNADA LETTER FA
unicode_line_break(0x0CE0, 0x0CE1, 'AL'). % Lo   [2] KANNADA LETTER VOCALIC RR..KANNADA LETTER VOCALIC LL
unicode_line_break(0x0CF1, 0x0CF2, 'AL'). % Lo   [2] KANNADA SIGN JIHVAMULIYA..KANNADA SIGN UPADHMANIYA
unicode_line_break(0x0D05, 0x0D0C, 'AL'). % Lo   [8] MALAYALAM LETTER A..MALAYALAM LETTER VOCALIC L
unicode_line_break(0x0D0E, 0x0D10, 'AL'). % Lo   [3] MALAYALAM LETTER E..MALAYALAM LETTER AI
unicode_line_break(0x0D12, 0x0D3A, 'AL'). % Lo  [41] MALAYALAM LETTER O..MALAYALAM LETTER TTTA
unicode_line_break(0x0D3D, 0x0D3D, 'AL'). % Lo       MALAYALAM SIGN AVAGRAHA
unicode_line_break(0x0D4E, 0x0D4E, 'AL'). % Lo       MALAYALAM LETTER DOT REPH
unicode_line_break(0x0D60, 0x0D61, 'AL'). % Lo   [2] MALAYALAM LETTER VOCALIC RR..MALAYALAM LETTER VOCALIC LL
unicode_line_break(0x0D70, 0x0D75, 'AL'). % No   [6] MALAYALAM NUMBER TEN..MALAYALAM FRACTION THREE QUARTERS
unicode_line_break(0x0D7A, 0x0D7F, 'AL'). % Lo   [6] MALAYALAM LETTER CHILLU NN..MALAYALAM LETTER CHILLU K
unicode_line_break(0x0D85, 0x0D96, 'AL'). % Lo  [18] SINHALA LETTER AYANNA..SINHALA LETTER AUYANNA
unicode_line_break(0x0D9A, 0x0DB1, 'AL'). % Lo  [24] SINHALA LETTER ALPAPRAANA KAYANNA..SINHALA LETTER DANTAJA NAYANNA
unicode_line_break(0x0DB3, 0x0DBB, 'AL'). % Lo   [9] SINHALA LETTER SANYAKA DAYANNA..SINHALA LETTER RAYANNA
unicode_line_break(0x0DBD, 0x0DBD, 'AL'). % Lo       SINHALA LETTER DANTAJA LAYANNA
unicode_line_break(0x0DC0, 0x0DC6, 'AL'). % Lo   [7] SINHALA LETTER VAYANNA..SINHALA LETTER FAYANNA
unicode_line_break(0x0DF4, 0x0DF4, 'AL'). % Po       SINHALA PUNCTUATION KUNDDALIYA
unicode_line_break(0x0E4F, 0x0E4F, 'AL'). % Po       THAI CHARACTER FONGMAN
unicode_line_break(0x0F00, 0x0F00, 'AL'). % Lo       TIBETAN SYLLABLE OM
unicode_line_break(0x0F05, 0x0F05, 'AL'). % Po       TIBETAN MARK CLOSING YIG MGO SGAB MA
unicode_line_break(0x0F13, 0x0F13, 'AL'). % So       TIBETAN MARK CARET -DZUD RTAGS ME LONG CAN
unicode_line_break(0x0F15, 0x0F17, 'AL'). % So   [3] TIBETAN LOGOTYPE SIGN CHAD RTAGS..TIBETAN ASTROLOGICAL SIGN SGRA GCAN -CHAR RTAGS
unicode_line_break(0x0F1A, 0x0F1F, 'AL'). % So   [6] TIBETAN SIGN RDEL DKAR GCIG..TIBETAN SIGN RDEL DKAR RDEL NAG
unicode_line_break(0x0F2A, 0x0F33, 'AL'). % No  [10] TIBETAN DIGIT HALF ONE..TIBETAN DIGIT HALF ZERO
unicode_line_break(0x0F36, 0x0F36, 'AL'). % So       TIBETAN MARK CARET -DZUD RTAGS BZHI MIG CAN
unicode_line_break(0x0F38, 0x0F38, 'AL'). % So       TIBETAN MARK CHE MGO
unicode_line_break(0x0F40, 0x0F47, 'AL'). % Lo   [8] TIBETAN LETTER KA..TIBETAN LETTER JA
unicode_line_break(0x0F49, 0x0F6C, 'AL'). % Lo  [36] TIBETAN LETTER NYA..TIBETAN LETTER RRA
unicode_line_break(0x0F88, 0x0F8C, 'AL'). % Lo   [5] TIBETAN SIGN LCE TSA CAN..TIBETAN SIGN INVERTED MCHU CAN
unicode_line_break(0x0FC0, 0x0FC5, 'AL'). % So   [6] TIBETAN CANTILLATION SIGN HEAVY BEAT..TIBETAN SYMBOL RDO RJE
unicode_line_break(0x0FC7, 0x0FCC, 'AL'). % So   [6] TIBETAN SYMBOL RDO RJE RGYA GRAM..TIBETAN SYMBOL NOR BU BZHI -KHYIL
unicode_line_break(0x0FCE, 0x0FCF, 'AL'). % So   [2] TIBETAN SIGN RDEL NAG RDEL DKAR..TIBETAN SIGN RDEL NAG GSUM
unicode_line_break(0x0FD4, 0x0FD4, 'AL'). % Po       TIBETAN MARK CLOSING BRDA RNYING YIG MGO SGAB MA
unicode_line_break(0x0FD5, 0x0FD8, 'AL'). % So   [4] RIGHT-FACING SVASTI SIGN..LEFT-FACING SVASTI SIGN WITH DOTS
unicode_line_break(0x104C, 0x104F, 'AL'). % Po   [4] MYANMAR SYMBOL LOCATIVE..MYANMAR SYMBOL GENITIVE
unicode_line_break(0x10A0, 0x10C5, 'AL'). % L&  [38] GEORGIAN CAPITAL LETTER AN..GEORGIAN CAPITAL LETTER HOE
unicode_line_break(0x10C7, 0x10C7, 'AL'). % L&       GEORGIAN CAPITAL LETTER YN
unicode_line_break(0x10CD, 0x10CD, 'AL'). % L&       GEORGIAN CAPITAL LETTER AEN
unicode_line_break(0x10D0, 0x10FA, 'AL'). % Lo  [43] GEORGIAN LETTER AN..GEORGIAN LETTER AIN
unicode_line_break(0x10FB, 0x10FB, 'AL'). % Po       GEORGIAN PARAGRAPH SEPARATOR
unicode_line_break(0x10FC, 0x10FC, 'AL'). % Lm       MODIFIER LETTER GEORGIAN NAR
unicode_line_break(0x10FD, 0x10FF, 'AL'). % Lo   [3] GEORGIAN LETTER AEN..GEORGIAN LETTER LABIAL SIGN
unicode_line_break(0x1200, 0x1248, 'AL'). % Lo  [73] ETHIOPIC SYLLABLE HA..ETHIOPIC SYLLABLE QWA
unicode_line_break(0x124A, 0x124D, 'AL'). % Lo   [4] ETHIOPIC SYLLABLE QWI..ETHIOPIC SYLLABLE QWE
unicode_line_break(0x1250, 0x1256, 'AL'). % Lo   [7] ETHIOPIC SYLLABLE QHA..ETHIOPIC SYLLABLE QHO
unicode_line_break(0x1258, 0x1258, 'AL'). % Lo       ETHIOPIC SYLLABLE QHWA
unicode_line_break(0x125A, 0x125D, 'AL'). % Lo   [4] ETHIOPIC SYLLABLE QHWI..ETHIOPIC SYLLABLE QHWE
unicode_line_break(0x1260, 0x1288, 'AL'). % Lo  [41] ETHIOPIC SYLLABLE BA..ETHIOPIC SYLLABLE XWA
unicode_line_break(0x128A, 0x128D, 'AL'). % Lo   [4] ETHIOPIC SYLLABLE XWI..ETHIOPIC SYLLABLE XWE
unicode_line_break(0x1290, 0x12B0, 'AL'). % Lo  [33] ETHIOPIC SYLLABLE NA..ETHIOPIC SYLLABLE KWA
unicode_line_break(0x12B2, 0x12B5, 'AL'). % Lo   [4] ETHIOPIC SYLLABLE KWI..ETHIOPIC SYLLABLE KWE
unicode_line_break(0x12B8, 0x12BE, 'AL'). % Lo   [7] ETHIOPIC SYLLABLE KXA..ETHIOPIC SYLLABLE KXO
unicode_line_break(0x12C0, 0x12C0, 'AL'). % Lo       ETHIOPIC SYLLABLE KXWA
unicode_line_break(0x12C2, 0x12C5, 'AL'). % Lo   [4] ETHIOPIC SYLLABLE KXWI..ETHIOPIC SYLLABLE KXWE
unicode_line_break(0x12C8, 0x12D6, 'AL'). % Lo  [15] ETHIOPIC SYLLABLE WA..ETHIOPIC SYLLABLE PHARYNGEAL O
unicode_line_break(0x12D8, 0x1310, 'AL'). % Lo  [57] ETHIOPIC SYLLABLE ZA..ETHIOPIC SYLLABLE GWA
unicode_line_break(0x1312, 0x1315, 'AL'). % Lo   [4] ETHIOPIC SYLLABLE GWI..ETHIOPIC SYLLABLE GWE
unicode_line_break(0x1318, 0x135A, 'AL'). % Lo  [67] ETHIOPIC SYLLABLE GGA..ETHIOPIC SYLLABLE FYA
unicode_line_break(0x1360, 0x1360, 'AL'). % Po       ETHIOPIC SECTION MARK
unicode_line_break(0x1362, 0x1368, 'AL'). % Po   [7] ETHIOPIC FULL STOP..ETHIOPIC PARAGRAPH SEPARATOR
unicode_line_break(0x1369, 0x137C, 'AL'). % No  [20] ETHIOPIC DIGIT ONE..ETHIOPIC NUMBER TEN THOUSAND
unicode_line_break(0x1380, 0x138F, 'AL'). % Lo  [16] ETHIOPIC SYLLABLE SEBATBEIT MWA..ETHIOPIC SYLLABLE PWE
unicode_line_break(0x1390, 0x1399, 'AL'). % So  [10] ETHIOPIC TONAL MARK YIZET..ETHIOPIC TONAL MARK KURT
unicode_line_break(0x13A0, 0x13F4, 'AL'). % Lo  [85] CHEROKEE LETTER A..CHEROKEE LETTER YV
unicode_line_break(0x1401, 0x166C, 'AL'). % Lo [620] CANADIAN SYLLABICS E..CANADIAN SYLLABICS CARRIER TTSA
unicode_line_break(0x166D, 0x166E, 'AL'). % Po   [2] CANADIAN SYLLABICS CHI SIGN..CANADIAN SYLLABICS FULL STOP
unicode_line_break(0x166F, 0x167F, 'AL'). % Lo  [17] CANADIAN SYLLABICS QAI..CANADIAN SYLLABICS BLACKFOOT W
unicode_line_break(0x1681, 0x169A, 'AL'). % Lo  [26] OGHAM LETTER BEITH..OGHAM LETTER PEITH
unicode_line_break(0x16A0, 0x16EA, 'AL'). % Lo  [75] RUNIC LETTER FEHU FEOH FE F..RUNIC LETTER X
unicode_line_break(0x16EE, 0x16F0, 'AL'). % Nl   [3] RUNIC ARLAUG SYMBOL..RUNIC BELGTHOR SYMBOL
unicode_line_break(0x1700, 0x170C, 'AL'). % Lo  [13] TAGALOG LETTER A..TAGALOG LETTER YA
unicode_line_break(0x170E, 0x1711, 'AL'). % Lo   [4] TAGALOG LETTER LA..TAGALOG LETTER HA
unicode_line_break(0x1720, 0x1731, 'AL'). % Lo  [18] HANUNOO LETTER A..HANUNOO LETTER HA
unicode_line_break(0x1740, 0x1751, 'AL'). % Lo  [18] BUHID LETTER A..BUHID LETTER HA
unicode_line_break(0x1760, 0x176C, 'AL'). % Lo  [13] TAGBANWA LETTER A..TAGBANWA LETTER YA
unicode_line_break(0x176E, 0x1770, 'AL'). % Lo   [3] TAGBANWA LETTER LA..TAGBANWA LETTER SA
unicode_line_break(0x17D9, 0x17D9, 'AL'). % Po       KHMER SIGN PHNAEK MUAN
unicode_line_break(0x17F0, 0x17F9, 'AL'). % No  [10] KHMER SYMBOL LEK ATTAK SON..KHMER SYMBOL LEK ATTAK PRAM-BUON
unicode_line_break(0x1800, 0x1801, 'AL'). % Po   [2] MONGOLIAN BIRGA..MONGOLIAN ELLIPSIS
unicode_line_break(0x1807, 0x1807, 'AL'). % Po       MONGOLIAN SIBE SYLLABLE BOUNDARY MARKER
unicode_line_break(0x180A, 0x180A, 'AL'). % Po       MONGOLIAN NIRUGU
unicode_line_break(0x1820, 0x1842, 'AL'). % Lo  [35] MONGOLIAN LETTER A..MONGOLIAN LETTER CHI
unicode_line_break(0x1843, 0x1843, 'AL'). % Lm       MONGOLIAN LETTER TODO LONG VOWEL SIGN
unicode_line_break(0x1844, 0x1877, 'AL'). % Lo  [52] MONGOLIAN LETTER TODO E..MONGOLIAN LETTER MANCHU ZHA
unicode_line_break(0x1880, 0x18A8, 'AL'). % Lo  [41] MONGOLIAN LETTER ALI GALI ANUSVARA ONE..MONGOLIAN LETTER MANCHU ALI GALI BHA
unicode_line_break(0x18AA, 0x18AA, 'AL'). % Lo       MONGOLIAN LETTER MANCHU ALI GALI LHA
unicode_line_break(0x18B0, 0x18F5, 'AL'). % Lo  [70] CANADIAN SYLLABICS OY..CANADIAN SYLLABICS CARRIER DENTAL S
unicode_line_break(0x1900, 0x191C, 'AL'). % Lo  [29] LIMBU VOWEL-CARRIER LETTER..LIMBU LETTER HA
unicode_line_break(0x1940, 0x1940, 'AL'). % So       LIMBU SIGN LOO
unicode_line_break(0x19E0, 0x19FF, 'AL'). % So  [32] KHMER SYMBOL PATHAMASAT..KHMER SYMBOL DAP-PRAM ROC
unicode_line_break(0x1A00, 0x1A16, 'AL'). % Lo  [23] BUGINESE LETTER KA..BUGINESE LETTER HA
unicode_line_break(0x1A1E, 0x1A1F, 'AL'). % Po   [2] BUGINESE PALLAWA..BUGINESE END OF SECTION
unicode_line_break(0x1B05, 0x1B33, 'AL'). % Lo  [47] BALINESE LETTER AKARA..BALINESE LETTER HA
unicode_line_break(0x1B45, 0x1B4B, 'AL'). % Lo   [7] BALINESE LETTER KAF SASAK..BALINESE LETTER ASYURA SASAK
unicode_line_break(0x1B5C, 0x1B5C, 'AL'). % Po       BALINESE WINDU
unicode_line_break(0x1B61, 0x1B6A, 'AL'). % So  [10] BALINESE MUSICAL SYMBOL DONG..BALINESE MUSICAL SYMBOL DANG GEDE
unicode_line_break(0x1B74, 0x1B7C, 'AL'). % So   [9] BALINESE MUSICAL SYMBOL RIGHT-HAND OPEN DUG..BALINESE MUSICAL SYMBOL LEFT-HAND OPEN PING
unicode_line_break(0x1B83, 0x1BA0, 'AL'). % Lo  [30] SUNDANESE LETTER A..SUNDANESE LETTER HA
unicode_line_break(0x1BAE, 0x1BAF, 'AL'). % Lo   [2] SUNDANESE LETTER KHA..SUNDANESE LETTER SYA
unicode_line_break(0x1BBA, 0x1BE5, 'AL'). % Lo  [44] SUNDANESE AVAGRAHA..BATAK LETTER U
unicode_line_break(0x1BFC, 0x1BFF, 'AL'). % Po   [4] BATAK SYMBOL BINDU NA METEK..BATAK SYMBOL BINDU PANGOLAT
unicode_line_break(0x1C00, 0x1C23, 'AL'). % Lo  [36] LEPCHA LETTER KA..LEPCHA LETTER A
unicode_line_break(0x1C4D, 0x1C4F, 'AL'). % Lo   [3] LEPCHA LETTER TTA..LEPCHA LETTER DDA
unicode_line_break(0x1C5A, 0x1C77, 'AL'). % Lo  [30] OL CHIKI LETTER LA..OL CHIKI LETTER OH
unicode_line_break(0x1C78, 0x1C7D, 'AL'). % Lm   [6] OL CHIKI MU TTUDDAG..OL CHIKI AHAD
unicode_line_break(0x1CC0, 0x1CC7, 'AL'). % Po   [8] SUNDANESE PUNCTUATION BINDU SURYA..SUNDANESE PUNCTUATION BINDU BA SATANGA
unicode_line_break(0x1CD3, 0x1CD3, 'AL'). % Po       VEDIC SIGN NIHSHVASA
unicode_line_break(0x1CE9, 0x1CEC, 'AL'). % Lo   [4] VEDIC SIGN ANUSVARA ANTARGOMUKHA..VEDIC SIGN ANUSVARA VAMAGOMUKHA WITH TAIL
unicode_line_break(0x1CEE, 0x1CF1, 'AL'). % Lo   [4] VEDIC SIGN HEXIFORM LONG ANUSVARA..VEDIC SIGN ANUSVARA UBHAYATO MUKHA
unicode_line_break(0x1CF5, 0x1CF6, 'AL'). % Lo   [2] VEDIC SIGN JIHVAMULIYA..VEDIC SIGN UPADHMANIYA
unicode_line_break(0x1D00, 0x1D2B, 'AL'). % L&  [44] LATIN LETTER SMALL CAPITAL A..CYRILLIC LETTER SMALL CAPITAL EL
unicode_line_break(0x1D2C, 0x1D6A, 'AL'). % Lm  [63] MODIFIER LETTER CAPITAL A..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_line_break(0x1D6B, 0x1D77, 'AL'). % L&  [13] LATIN SMALL LETTER UE..LATIN SMALL LETTER TURNED G
unicode_line_break(0x1D78, 0x1D78, 'AL'). % Lm       MODIFIER LETTER CYRILLIC EN
unicode_line_break(0x1D79, 0x1D9A, 'AL'). % L&  [34] LATIN SMALL LETTER INSULAR G..LATIN SMALL LETTER EZH WITH RETROFLEX HOOK
unicode_line_break(0x1D9B, 0x1DBF, 'AL'). % Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_line_break(0x1E00, 0x1F15, 'AL'). % L& [278] LATIN CAPITAL LETTER A WITH RING BELOW..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_line_break(0x1F18, 0x1F1D, 'AL'). % L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_line_break(0x1F20, 0x1F45, 'AL'). % L&  [38] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_line_break(0x1F48, 0x1F4D, 'AL'). % L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_line_break(0x1F50, 0x1F57, 'AL'). % L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_line_break(0x1F59, 0x1F59, 'AL'). % L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_line_break(0x1F5B, 0x1F5B, 'AL'). % L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_line_break(0x1F5D, 0x1F5D, 'AL'). % L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_line_break(0x1F5F, 0x1F7D, 'AL'). % L&  [31] GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_line_break(0x1F80, 0x1FB4, 'AL'). % L&  [53] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_line_break(0x1FB6, 0x1FBC, 'AL'). % L&   [7] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_line_break(0x1FBD, 0x1FBD, 'AL'). % Sk       GREEK KORONIS
unicode_line_break(0x1FBE, 0x1FBE, 'AL'). % L&       GREEK PROSGEGRAMMENI
unicode_line_break(0x1FBF, 0x1FC1, 'AL'). % Sk   [3] GREEK PSILI..GREEK DIALYTIKA AND PERISPOMENI
unicode_line_break(0x1FC2, 0x1FC4, 'AL'). % L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_line_break(0x1FC6, 0x1FCC, 'AL'). % L&   [7] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_line_break(0x1FCD, 0x1FCF, 'AL'). % Sk   [3] GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI
unicode_line_break(0x1FD0, 0x1FD3, 'AL'). % L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_line_break(0x1FD6, 0x1FDB, 'AL'). % L&   [6] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_line_break(0x1FDD, 0x1FDF, 'AL'). % Sk   [3] GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI
unicode_line_break(0x1FE0, 0x1FEC, 'AL'). % L&  [13] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_line_break(0x1FED, 0x1FEF, 'AL'). % Sk   [3] GREEK DIALYTIKA AND VARIA..GREEK VARIA
unicode_line_break(0x1FF2, 0x1FF4, 'AL'). % L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_line_break(0x1FF6, 0x1FFC, 'AL'). % L&   [7] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_line_break(0x1FFE, 0x1FFE, 'AL'). % Sk       GREEK DASIA
unicode_line_break(0x2017, 0x2017, 'AL'). % Po       DOUBLE LOW LINE
unicode_line_break(0x2022, 0x2023, 'AL'). % Po   [2] BULLET..TRIANGULAR BULLET
unicode_line_break(0x2038, 0x2038, 'AL'). % Po       CARET
unicode_line_break(0x203E, 0x203E, 'AL'). % Po       OVERLINE
unicode_line_break(0x203F, 0x2040, 'AL'). % Pc   [2] UNDERTIE..CHARACTER TIE
unicode_line_break(0x2041, 0x2043, 'AL'). % Po   [3] CARET INSERTION POINT..HYPHEN BULLET
unicode_line_break(0x204A, 0x2051, 'AL'). % Po   [8] TIRONIAN SIGN ET..TWO ASTERISKS ALIGNED VERTICALLY
unicode_line_break(0x2052, 0x2052, 'AL'). % Sm       COMMERCIAL MINUS SIGN
unicode_line_break(0x2053, 0x2053, 'AL'). % Po       SWUNG DASH
unicode_line_break(0x2054, 0x2054, 'AL'). % Pc       INVERTED UNDERTIE
unicode_line_break(0x2055, 0x2055, 'AL'). % Po       FLOWER PUNCTUATION MARK
unicode_line_break(0x2057, 0x2057, 'AL'). % Po       QUADRUPLE PRIME
unicode_line_break(0x205C, 0x205C, 'AL'). % Po       DOTTED CROSS
unicode_line_break(0x2061, 0x2064, 'AL'). % Cf   [4] FUNCTION APPLICATION..INVISIBLE PLUS
unicode_line_break(0x2070, 0x2070, 'AL'). % No       SUPERSCRIPT ZERO
unicode_line_break(0x2071, 0x2071, 'AL'). % Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_line_break(0x2075, 0x2079, 'AL'). % No   [5] SUPERSCRIPT FIVE..SUPERSCRIPT NINE
unicode_line_break(0x207A, 0x207C, 'AL'). % Sm   [3] SUPERSCRIPT PLUS SIGN..SUPERSCRIPT EQUALS SIGN
unicode_line_break(0x2080, 0x2080, 'AL'). % No       SUBSCRIPT ZERO
unicode_line_break(0x2085, 0x2089, 'AL'). % No   [5] SUBSCRIPT FIVE..SUBSCRIPT NINE
unicode_line_break(0x208A, 0x208C, 'AL'). % Sm   [3] SUBSCRIPT PLUS SIGN..SUBSCRIPT EQUALS SIGN
unicode_line_break(0x2090, 0x209C, 'AL'). % Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_line_break(0x2100, 0x2101, 'AL'). % So   [2] ACCOUNT OF..ADDRESSED TO THE SUBJECT
unicode_line_break(0x2102, 0x2102, 'AL'). % L&       DOUBLE-STRUCK CAPITAL C
unicode_line_break(0x2104, 0x2104, 'AL'). % So       CENTRE LINE SYMBOL
unicode_line_break(0x2106, 0x2106, 'AL'). % So       CADA UNA
unicode_line_break(0x2107, 0x2107, 'AL'). % L&       EULER CONSTANT
unicode_line_break(0x2108, 0x2108, 'AL'). % So       SCRUPLE
unicode_line_break(0x210A, 0x2112, 'AL'). % L&   [9] SCRIPT SMALL G..SCRIPT CAPITAL L
unicode_line_break(0x2114, 0x2114, 'AL'). % So       L B BAR SYMBOL
unicode_line_break(0x2115, 0x2115, 'AL'). % L&       DOUBLE-STRUCK CAPITAL N
unicode_line_break(0x2117, 0x2117, 'AL'). % So       SOUND RECORDING COPYRIGHT
unicode_line_break(0x2118, 0x2118, 'AL'). % Sm       SCRIPT CAPITAL P
unicode_line_break(0x2119, 0x211D, 'AL'). % L&   [5] DOUBLE-STRUCK CAPITAL P..DOUBLE-STRUCK CAPITAL R
unicode_line_break(0x211E, 0x2120, 'AL'). % So   [3] PRESCRIPTION TAKE..SERVICE MARK
unicode_line_break(0x2123, 0x2123, 'AL'). % So       VERSICLE
unicode_line_break(0x2124, 0x2124, 'AL'). % L&       DOUBLE-STRUCK CAPITAL Z
unicode_line_break(0x2125, 0x2125, 'AL'). % So       OUNCE SIGN
unicode_line_break(0x2126, 0x2126, 'AL'). % L&       OHM SIGN
unicode_line_break(0x2127, 0x2127, 'AL'). % So       INVERTED OHM SIGN
unicode_line_break(0x2128, 0x2128, 'AL'). % L&       BLACK-LETTER CAPITAL Z
unicode_line_break(0x2129, 0x2129, 'AL'). % So       TURNED GREEK SMALL LETTER IOTA
unicode_line_break(0x212A, 0x212A, 'AL'). % L&       KELVIN SIGN
unicode_line_break(0x212C, 0x212D, 'AL'). % L&   [2] SCRIPT CAPITAL B..BLACK-LETTER CAPITAL C
unicode_line_break(0x212E, 0x212E, 'AL'). % So       ESTIMATED SYMBOL
unicode_line_break(0x212F, 0x2134, 'AL'). % L&   [6] SCRIPT SMALL E..SCRIPT SMALL O
unicode_line_break(0x2135, 0x2138, 'AL'). % Lo   [4] ALEF SYMBOL..DALET SYMBOL
unicode_line_break(0x2139, 0x2139, 'AL'). % L&       INFORMATION SOURCE
unicode_line_break(0x213A, 0x213B, 'AL'). % So   [2] ROTATED CAPITAL Q..FACSIMILE SIGN
unicode_line_break(0x213C, 0x213F, 'AL'). % L&   [4] DOUBLE-STRUCK SMALL PI..DOUBLE-STRUCK CAPITAL PI
unicode_line_break(0x2140, 0x2144, 'AL'). % Sm   [5] DOUBLE-STRUCK N-ARY SUMMATION..TURNED SANS-SERIF CAPITAL Y
unicode_line_break(0x2145, 0x2149, 'AL'). % L&   [5] DOUBLE-STRUCK ITALIC CAPITAL D..DOUBLE-STRUCK ITALIC SMALL J
unicode_line_break(0x214A, 0x214A, 'AL'). % So       PROPERTY LINE
unicode_line_break(0x214B, 0x214B, 'AL'). % Sm       TURNED AMPERSAND
unicode_line_break(0x214C, 0x214D, 'AL'). % So   [2] PER SIGN..AKTIESELSKAB
unicode_line_break(0x214E, 0x214E, 'AL'). % L&       TURNED SMALL F
unicode_line_break(0x214F, 0x214F, 'AL'). % So       SYMBOL FOR SAMARITAN SOURCE
unicode_line_break(0x2150, 0x2153, 'AL'). % No   [4] VULGAR FRACTION ONE SEVENTH..VULGAR FRACTION ONE THIRD
unicode_line_break(0x2156, 0x215A, 'AL'). % No   [5] VULGAR FRACTION TWO FIFTHS..VULGAR FRACTION FIVE SIXTHS
unicode_line_break(0x215C, 0x215D, 'AL'). % No   [2] VULGAR FRACTION THREE EIGHTHS..VULGAR FRACTION FIVE EIGHTHS
unicode_line_break(0x215F, 0x215F, 'AL'). % No       FRACTION NUMERATOR ONE
unicode_line_break(0x216C, 0x216F, 'AL'). % Nl   [4] ROMAN NUMERAL FIFTY..ROMAN NUMERAL ONE THOUSAND
unicode_line_break(0x217A, 0x2182, 'AL'). % Nl   [9] SMALL ROMAN NUMERAL ELEVEN..ROMAN NUMERAL TEN THOUSAND
unicode_line_break(0x2183, 0x2184, 'AL'). % L&   [2] ROMAN NUMERAL REVERSED ONE HUNDRED..LATIN SMALL LETTER REVERSED C
unicode_line_break(0x2185, 0x2188, 'AL'). % Nl   [4] ROMAN NUMERAL SIX LATE FORM..ROMAN NUMERAL ONE HUNDRED THOUSAND
unicode_line_break(0x219A, 0x219B, 'AL'). % Sm   [2] LEFTWARDS ARROW WITH STROKE..RIGHTWARDS ARROW WITH STROKE
unicode_line_break(0x219C, 0x219F, 'AL'). % So   [4] LEFTWARDS WAVE ARROW..UPWARDS TWO HEADED ARROW
unicode_line_break(0x21A0, 0x21A0, 'AL'). % Sm       RIGHTWARDS TWO HEADED ARROW
unicode_line_break(0x21A1, 0x21A2, 'AL'). % So   [2] DOWNWARDS TWO HEADED ARROW..LEFTWARDS ARROW WITH TAIL
unicode_line_break(0x21A3, 0x21A3, 'AL'). % Sm       RIGHTWARDS ARROW WITH TAIL
unicode_line_break(0x21A4, 0x21A5, 'AL'). % So   [2] LEFTWARDS ARROW FROM BAR..UPWARDS ARROW FROM BAR
unicode_line_break(0x21A6, 0x21A6, 'AL'). % Sm       RIGHTWARDS ARROW FROM BAR
unicode_line_break(0x21A7, 0x21AD, 'AL'). % So   [7] DOWNWARDS ARROW FROM BAR..LEFT RIGHT WAVE ARROW
unicode_line_break(0x21AE, 0x21AE, 'AL'). % Sm       LEFT RIGHT ARROW WITH STROKE
unicode_line_break(0x21AF, 0x21CD, 'AL'). % So  [31] DOWNWARDS ZIGZAG ARROW..LEFTWARDS DOUBLE ARROW WITH STROKE
unicode_line_break(0x21CE, 0x21CF, 'AL'). % Sm   [2] LEFT RIGHT DOUBLE ARROW WITH STROKE..RIGHTWARDS DOUBLE ARROW WITH STROKE
unicode_line_break(0x21D0, 0x21D1, 'AL'). % So   [2] LEFTWARDS DOUBLE ARROW..UPWARDS DOUBLE ARROW
unicode_line_break(0x21D3, 0x21D3, 'AL'). % So       DOWNWARDS DOUBLE ARROW
unicode_line_break(0x21D5, 0x21F3, 'AL'). % So  [31] UP DOWN DOUBLE ARROW..UP DOWN WHITE ARROW
unicode_line_break(0x21F4, 0x21FF, 'AL'). % Sm  [12] RIGHT ARROW WITH SMALL CIRCLE..LEFT RIGHT OPEN-HEADED ARROW
unicode_line_break(0x2201, 0x2201, 'AL'). % Sm       COMPLEMENT
unicode_line_break(0x2204, 0x2206, 'AL'). % Sm   [3] THERE DOES NOT EXIST..INCREMENT
unicode_line_break(0x2209, 0x220A, 'AL'). % Sm   [2] NOT AN ELEMENT OF..SMALL ELEMENT OF
unicode_line_break(0x220C, 0x220E, 'AL'). % Sm   [3] DOES NOT CONTAIN AS MEMBER..END OF PROOF
unicode_line_break(0x2210, 0x2210, 'AL'). % Sm       N-ARY COPRODUCT
unicode_line_break(0x2214, 0x2214, 'AL'). % Sm       DOT PLUS
unicode_line_break(0x2216, 0x2219, 'AL'). % Sm   [4] SET MINUS..BULLET OPERATOR
unicode_line_break(0x221B, 0x221C, 'AL'). % Sm   [2] CUBE ROOT..FOURTH ROOT
unicode_line_break(0x2221, 0x2222, 'AL'). % Sm   [2] MEASURED ANGLE..SPHERICAL ANGLE
unicode_line_break(0x2224, 0x2224, 'AL'). % Sm       DOES NOT DIVIDE
unicode_line_break(0x2226, 0x2226, 'AL'). % Sm       NOT PARALLEL TO
unicode_line_break(0x222D, 0x222D, 'AL'). % Sm       TRIPLE INTEGRAL
unicode_line_break(0x222F, 0x2233, 'AL'). % Sm   [5] SURFACE INTEGRAL..ANTICLOCKWISE CONTOUR INTEGRAL
unicode_line_break(0x2238, 0x223B, 'AL'). % Sm   [4] DOT MINUS..HOMOTHETIC
unicode_line_break(0x223E, 0x2247, 'AL'). % Sm  [10] INVERTED LAZY S..NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
unicode_line_break(0x2249, 0x224B, 'AL'). % Sm   [3] NOT ALMOST EQUAL TO..TRIPLE TILDE
unicode_line_break(0x224D, 0x2251, 'AL'). % Sm   [5] EQUIVALENT TO..GEOMETRICALLY EQUAL TO
unicode_line_break(0x2253, 0x225F, 'AL'). % Sm  [13] IMAGE OF OR APPROXIMATELY EQUAL TO..QUESTIONED EQUAL TO
unicode_line_break(0x2262, 0x2263, 'AL'). % Sm   [2] NOT IDENTICAL TO..STRICTLY EQUIVALENT TO
unicode_line_break(0x2268, 0x2269, 'AL'). % Sm   [2] LESS-THAN BUT NOT EQUAL TO..GREATER-THAN BUT NOT EQUAL TO
unicode_line_break(0x226C, 0x226D, 'AL'). % Sm   [2] BETWEEN..NOT EQUIVALENT TO
unicode_line_break(0x2270, 0x2281, 'AL'). % Sm  [18] NEITHER LESS-THAN NOR EQUAL TO..DOES NOT SUCCEED
unicode_line_break(0x2284, 0x2285, 'AL'). % Sm   [2] NOT A SUBSET OF..NOT A SUPERSET OF
unicode_line_break(0x2288, 0x2294, 'AL'). % Sm  [13] NEITHER A SUBSET OF NOR EQUAL TO..SQUARE CUP
unicode_line_break(0x2296, 0x2298, 'AL'). % Sm   [3] CIRCLED MINUS..CIRCLED DIVISION SLASH
unicode_line_break(0x229A, 0x22A4, 'AL'). % Sm  [11] CIRCLED RING OPERATOR..DOWN TACK
unicode_line_break(0x22A6, 0x22BE, 'AL'). % Sm  [25] ASSERTION..RIGHT ANGLE WITH ARC
unicode_line_break(0x22C0, 0x22FF, 'AL'). % Sm  [64] N-ARY LOGICAL AND..Z NOTATION BAG MEMBERSHIP
unicode_line_break(0x2300, 0x2307, 'AL'). % So   [8] DIAMETER SIGN..WAVY LINE
unicode_line_break(0x2308, 0x230B, 'AL'). % Sm   [4] LEFT CEILING..RIGHT FLOOR
unicode_line_break(0x230C, 0x2311, 'AL'). % So   [6] BOTTOM RIGHT CROP..SQUARE LOZENGE
unicode_line_break(0x2313, 0x231F, 'AL'). % So  [13] SEGMENT..BOTTOM RIGHT CORNER
unicode_line_break(0x2320, 0x2321, 'AL'). % Sm   [2] TOP HALF INTEGRAL..BOTTOM HALF INTEGRAL
unicode_line_break(0x2322, 0x2328, 'AL'). % So   [7] FROWN..KEYBOARD
unicode_line_break(0x232B, 0x237B, 'AL'). % So  [81] ERASE TO THE LEFT..NOT CHECK MARK
unicode_line_break(0x237C, 0x237C, 'AL'). % Sm       RIGHT ANGLE WITH DOWNWARDS ZIGZAG ARROW
unicode_line_break(0x237D, 0x239A, 'AL'). % So  [30] SHOULDERED OPEN BOX..CLEAR SCREEN SYMBOL
unicode_line_break(0x239B, 0x23B3, 'AL'). % Sm  [25] LEFT PARENTHESIS UPPER HOOK..SUMMATION BOTTOM
unicode_line_break(0x23B4, 0x23DB, 'AL'). % So  [40] TOP SQUARE BRACKET..FUSE
unicode_line_break(0x23DC, 0x23E1, 'AL'). % Sm   [6] TOP PARENTHESIS..BOTTOM TORTOISE SHELL BRACKET
unicode_line_break(0x23E2, 0x23F3, 'AL'). % So  [18] WHITE TRAPEZIUM..HOURGLASS WITH FLOWING SAND
unicode_line_break(0x2400, 0x2426, 'AL'). % So  [39] SYMBOL FOR NULL..SYMBOL FOR SUBSTITUTE FORM TWO
unicode_line_break(0x2440, 0x244A, 'AL'). % So  [11] OCR HOOK..OCR DOUBLE BACKSLASH
unicode_line_break(0x24FF, 0x24FF, 'AL'). % No       NEGATIVE CIRCLED DIGIT ZERO
unicode_line_break(0x254C, 0x254F, 'AL'). % So   [4] BOX DRAWINGS LIGHT DOUBLE DASH HORIZONTAL..BOX DRAWINGS HEAVY DOUBLE DASH VERTICAL
unicode_line_break(0x2575, 0x257F, 'AL'). % So  [11] BOX DRAWINGS LIGHT UP..BOX DRAWINGS HEAVY UP AND LIGHT DOWN
unicode_line_break(0x2590, 0x2591, 'AL'). % So   [2] RIGHT HALF BLOCK..LIGHT SHADE
unicode_line_break(0x2596, 0x259F, 'AL'). % So  [10] QUADRANT LOWER LEFT..QUADRANT UPPER RIGHT AND LOWER LEFT AND LOWER RIGHT
unicode_line_break(0x25A2, 0x25A2, 'AL'). % So       WHITE SQUARE WITH ROUNDED CORNERS
unicode_line_break(0x25AA, 0x25B1, 'AL'). % So   [8] BLACK SMALL SQUARE..WHITE PARALLELOGRAM
unicode_line_break(0x25B4, 0x25B5, 'AL'). % So   [2] BLACK UP-POINTING SMALL TRIANGLE..WHITE UP-POINTING SMALL TRIANGLE
unicode_line_break(0x25B8, 0x25BB, 'AL'). % So   [4] BLACK RIGHT-POINTING SMALL TRIANGLE..WHITE RIGHT-POINTING POINTER
unicode_line_break(0x25BE, 0x25BF, 'AL'). % So   [2] BLACK DOWN-POINTING SMALL TRIANGLE..WHITE DOWN-POINTING SMALL TRIANGLE
unicode_line_break(0x25C2, 0x25C5, 'AL'). % So   [4] BLACK LEFT-POINTING SMALL TRIANGLE..WHITE LEFT-POINTING POINTER
unicode_line_break(0x25C9, 0x25CA, 'AL'). % So   [2] FISHEYE..LOZENGE
unicode_line_break(0x25CC, 0x25CD, 'AL'). % So   [2] DOTTED CIRCLE..CIRCLE WITH VERTICAL FILL
unicode_line_break(0x25D2, 0x25E1, 'AL'). % So  [16] CIRCLE WITH LOWER HALF BLACK..LOWER HALF CIRCLE
unicode_line_break(0x25E6, 0x25EE, 'AL'). % So   [9] WHITE BULLET..UP-POINTING TRIANGLE WITH RIGHT HALF BLACK
unicode_line_break(0x25F0, 0x25F7, 'AL'). % So   [8] WHITE SQUARE WITH UPPER LEFT QUADRANT..WHITE CIRCLE WITH UPPER RIGHT QUADRANT
unicode_line_break(0x25F8, 0x25FF, 'AL'). % Sm   [8] UPPER LEFT TRIANGLE..LOWER RIGHT TRIANGLE
unicode_line_break(0x2600, 0x2604, 'AL'). % So   [5] BLACK SUN WITH RAYS..COMET
unicode_line_break(0x2607, 0x2608, 'AL'). % So   [2] LIGHTNING..THUNDERSTORM
unicode_line_break(0x260A, 0x260D, 'AL'). % So   [4] ASCENDING NODE..OPPOSITION
unicode_line_break(0x2610, 0x2613, 'AL'). % So   [4] BALLOT BOX..SALTIRE
unicode_line_break(0x2618, 0x261B, 'AL'). % So   [4] SHAMROCK..BLACK RIGHT POINTING INDEX
unicode_line_break(0x261D, 0x261D, 'AL'). % So       WHITE UP POINTING INDEX
unicode_line_break(0x261F, 0x263F, 'AL'). % So  [33] WHITE DOWN POINTING INDEX..MERCURY
unicode_line_break(0x2641, 0x2641, 'AL'). % So       EARTH
unicode_line_break(0x2643, 0x265F, 'AL'). % So  [29] JUPITER..BLACK CHESS PAWN
unicode_line_break(0x2662, 0x2662, 'AL'). % So       WHITE DIAMOND SUIT
unicode_line_break(0x2666, 0x2666, 'AL'). % So       BLACK DIAMOND SUIT
unicode_line_break(0x266B, 0x266B, 'AL'). % So       BEAMED EIGHTH NOTES
unicode_line_break(0x266E, 0x266E, 'AL'). % So       MUSIC NATURAL SIGN
unicode_line_break(0x2670, 0x269D, 'AL'). % So  [46] WEST SYRIAC CROSS..OUTLINED WHITE STAR
unicode_line_break(0x26A0, 0x26BD, 'AL'). % So  [30] WARNING SIGN..SOCCER BALL
unicode_line_break(0x26C0, 0x26C3, 'AL'). % So   [4] WHITE DRAUGHTS MAN..BLACK DRAUGHTS KING
unicode_line_break(0x26CE, 0x26CE, 'AL'). % So       OPHIUCHUS
unicode_line_break(0x26E2, 0x26E2, 'AL'). % So       ASTRONOMICAL SYMBOL FOR URANUS
unicode_line_break(0x26E4, 0x26E7, 'AL'). % So   [4] PENTAGRAM..INVERTED PENTAGRAM
unicode_line_break(0x2701, 0x2756, 'AL'). % So  [86] UPPER BLADE SCISSORS..BLACK DIAMOND MINUS WHITE X
unicode_line_break(0x2758, 0x275A, 'AL'). % So   [3] LIGHT VERTICAL BAR..HEAVY VERTICAL BAR
unicode_line_break(0x275F, 0x2761, 'AL'). % So   [3] HEAVY LOW SINGLE COMMA QUOTATION MARK ORNAMENT..CURVED STEM PARAGRAPH SIGN ORNAMENT
unicode_line_break(0x2764, 0x2767, 'AL'). % So   [4] HEAVY BLACK HEART..ROTATED FLORAL HEART BULLET
unicode_line_break(0x2794, 0x27BF, 'AL'). % So  [44] HEAVY WIDE-HEADED RIGHTWARDS ARROW..DOUBLE CURLY LOOP
unicode_line_break(0x27C0, 0x27C4, 'AL'). % Sm   [5] THREE DIMENSIONAL ANGLE..OPEN SUPERSET
unicode_line_break(0x27C7, 0x27E5, 'AL'). % Sm  [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
unicode_line_break(0x27F0, 0x27FF, 'AL'). % Sm  [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
unicode_line_break(0x2800, 0x28FF, 'AL'). % So [256] BRAILLE PATTERN BLANK..BRAILLE PATTERN DOTS-12345678
unicode_line_break(0x2900, 0x2982, 'AL'). % Sm [131] RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE..Z NOTATION TYPE COLON
unicode_line_break(0x2999, 0x29D7, 'AL'). % Sm  [63] DOTTED FENCE..BLACK HOURGLASS
unicode_line_break(0x29DC, 0x29FB, 'AL'). % Sm  [32] INCOMPLETE INFINITY..TRIPLE PLUS
unicode_line_break(0x29FE, 0x2AFF, 'AL'). % Sm [258] TINY..N-ARY WHITE VERTICAL BAR
unicode_line_break(0x2B00, 0x2B2F, 'AL'). % So  [48] NORTH EAST WHITE ARROW..WHITE VERTICAL ELLIPSE
unicode_line_break(0x2B30, 0x2B44, 'AL'). % Sm  [21] LEFT ARROW WITH SMALL CIRCLE..RIGHTWARDS ARROW THROUGH SUPERSET
unicode_line_break(0x2B45, 0x2B46, 'AL'). % So   [2] LEFTWARDS QUADRUPLE ARROW..RIGHTWARDS QUADRUPLE ARROW
unicode_line_break(0x2B47, 0x2B4C, 'AL'). % Sm   [6] REVERSE TILDE OPERATOR ABOVE RIGHTWARDS ARROW..RIGHTWARDS ARROW ABOVE REVERSE TILDE OPERATOR
unicode_line_break(0x2B50, 0x2B54, 'AL'). % So   [5] WHITE MEDIUM STAR..WHITE RIGHT-POINTING PENTAGON
unicode_line_break(0x2C00, 0x2C2E, 'AL'). % L&  [47] GLAGOLITIC CAPITAL LETTER AZU..GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_line_break(0x2C30, 0x2C5E, 'AL'). % L&  [47] GLAGOLITIC SMALL LETTER AZU..GLAGOLITIC SMALL LETTER LATINATE MYSLITE
unicode_line_break(0x2C60, 0x2C7B, 'AL'). % L&  [28] LATIN CAPITAL LETTER L WITH DOUBLE BAR..LATIN LETTER SMALL CAPITAL TURNED E
unicode_line_break(0x2C7C, 0x2C7D, 'AL'). % Lm   [2] LATIN SUBSCRIPT SMALL LETTER J..MODIFIER LETTER CAPITAL V
unicode_line_break(0x2C7E, 0x2CE4, 'AL'). % L& [103] LATIN CAPITAL LETTER S WITH SWASH TAIL..COPTIC SYMBOL KAI
unicode_line_break(0x2CE5, 0x2CEA, 'AL'). % So   [6] COPTIC SYMBOL MI RO..COPTIC SYMBOL SHIMA SIMA
unicode_line_break(0x2CEB, 0x2CEE, 'AL'). % L&   [4] COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI..COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
unicode_line_break(0x2CF2, 0x2CF3, 'AL'). % L&   [2] COPTIC CAPITAL LETTER BOHAIRIC KHEI..COPTIC SMALL LETTER BOHAIRIC KHEI
unicode_line_break(0x2CFD, 0x2CFD, 'AL'). % No       COPTIC FRACTION ONE HALF
unicode_line_break(0x2D00, 0x2D25, 'AL'). % L&  [38] GEORGIAN SMALL LETTER AN..GEORGIAN SMALL LETTER HOE
unicode_line_break(0x2D27, 0x2D27, 'AL'). % L&       GEORGIAN SMALL LETTER YN
unicode_line_break(0x2D2D, 0x2D2D, 'AL'). % L&       GEORGIAN SMALL LETTER AEN
unicode_line_break(0x2D30, 0x2D67, 'AL'). % Lo  [56] TIFINAGH LETTER YA..TIFINAGH LETTER YO
unicode_line_break(0x2D6F, 0x2D6F, 'AL'). % Lm       TIFINAGH MODIFIER LETTER LABIALIZATION MARK
unicode_line_break(0x2D80, 0x2D96, 'AL'). % Lo  [23] ETHIOPIC SYLLABLE LOA..ETHIOPIC SYLLABLE GGWE
unicode_line_break(0x2DA0, 0x2DA6, 'AL'). % Lo   [7] ETHIOPIC SYLLABLE SSA..ETHIOPIC SYLLABLE SSO
unicode_line_break(0x2DA8, 0x2DAE, 'AL'). % Lo   [7] ETHIOPIC SYLLABLE CCA..ETHIOPIC SYLLABLE CCO
unicode_line_break(0x2DB0, 0x2DB6, 'AL'). % Lo   [7] ETHIOPIC SYLLABLE ZZA..ETHIOPIC SYLLABLE ZZO
unicode_line_break(0x2DB8, 0x2DBE, 'AL'). % Lo   [7] ETHIOPIC SYLLABLE CCHA..ETHIOPIC SYLLABLE CCHO
unicode_line_break(0x2DC0, 0x2DC6, 'AL'). % Lo   [7] ETHIOPIC SYLLABLE QYA..ETHIOPIC SYLLABLE QYO
unicode_line_break(0x2DC8, 0x2DCE, 'AL'). % Lo   [7] ETHIOPIC SYLLABLE KYA..ETHIOPIC SYLLABLE KYO
unicode_line_break(0x2DD0, 0x2DD6, 'AL'). % Lo   [7] ETHIOPIC SYLLABLE XYA..ETHIOPIC SYLLABLE XYO
unicode_line_break(0x2DD8, 0x2DDE, 'AL'). % Lo   [7] ETHIOPIC SYLLABLE GYA..ETHIOPIC SYLLABLE GYO
unicode_line_break(0x2E16, 0x2E16, 'AL'). % Po       DOTTED RIGHT-POINTING ANGLE
unicode_line_break(0x2E1A, 0x2E1A, 'AL'). % Pd       HYPHEN WITH DIAERESIS
unicode_line_break(0x2E1B, 0x2E1B, 'AL'). % Po       TILDE WITH RING ABOVE
unicode_line_break(0x2E1E, 0x2E1F, 'AL'). % Po   [2] TILDE WITH DOT ABOVE..TILDE WITH DOT BELOW
unicode_line_break(0x2E2F, 0x2E2F, 'AL'). % Lm       VERTICAL TILDE
unicode_line_break(0x2E32, 0x2E32, 'AL'). % Po       TURNED COMMA
unicode_line_break(0x2E35, 0x2E39, 'AL'). % Po   [5] TURNED SEMICOLON..TOP HALF SECTION SIGN
unicode_line_break(0x4DC0, 0x4DFF, 'AL'). % So  [64] HEXAGRAM FOR THE CREATIVE HEAVEN..HEXAGRAM FOR BEFORE COMPLETION
unicode_line_break(0xA4D0, 0xA4F7, 'AL'). % Lo  [40] LISU LETTER BA..LISU LETTER OE
unicode_line_break(0xA4F8, 0xA4FD, 'AL'). % Lm   [6] LISU LETTER TONE MYA TI..LISU LETTER TONE MYA JEU
unicode_line_break(0xA500, 0xA60B, 'AL'). % Lo [268] VAI SYLLABLE EE..VAI SYLLABLE NG
unicode_line_break(0xA60C, 0xA60C, 'AL'). % Lm       VAI SYLLABLE LENGTHENER
unicode_line_break(0xA610, 0xA61F, 'AL'). % Lo  [16] VAI SYLLABLE NDOLE FA..VAI SYMBOL JONG
unicode_line_break(0xA62A, 0xA62B, 'AL'). % Lo   [2] VAI SYLLABLE NDOLE MA..VAI SYLLABLE NDOLE DO
unicode_line_break(0xA640, 0xA66D, 'AL'). % L&  [46] CYRILLIC CAPITAL LETTER ZEMLYA..CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
unicode_line_break(0xA66E, 0xA66E, 'AL'). % Lo       CYRILLIC LETTER MULTIOCULAR O
unicode_line_break(0xA673, 0xA673, 'AL'). % Po       SLAVONIC ASTERISK
unicode_line_break(0xA67E, 0xA67E, 'AL'). % Po       CYRILLIC KAVYKA
unicode_line_break(0xA67F, 0xA67F, 'AL'). % Lm       CYRILLIC PAYEROK
unicode_line_break(0xA680, 0xA697, 'AL'). % L&  [24] CYRILLIC CAPITAL LETTER DWE..CYRILLIC SMALL LETTER SHWE
unicode_line_break(0xA6A0, 0xA6E5, 'AL'). % Lo  [70] BAMUM LETTER A..BAMUM LETTER KI
unicode_line_break(0xA6E6, 0xA6EF, 'AL'). % Nl  [10] BAMUM LETTER MO..BAMUM LETTER KOGHOM
unicode_line_break(0xA6F2, 0xA6F2, 'AL'). % Po       BAMUM NJAEMLI
unicode_line_break(0xA700, 0xA716, 'AL'). % Sk  [23] MODIFIER LETTER CHINESE TONE YIN PING..MODIFIER LETTER EXTRA-LOW LEFT-STEM TONE BAR
unicode_line_break(0xA717, 0xA71F, 'AL'). % Lm   [9] MODIFIER LETTER DOT VERTICAL BAR..MODIFIER LETTER LOW INVERTED EXCLAMATION MARK
unicode_line_break(0xA720, 0xA721, 'AL'). % Sk   [2] MODIFIER LETTER STRESS AND HIGH TONE..MODIFIER LETTER STRESS AND LOW TONE
unicode_line_break(0xA722, 0xA76F, 'AL'). % L&  [78] LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF..LATIN SMALL LETTER CON
unicode_line_break(0xA770, 0xA770, 'AL'). % Lm       MODIFIER LETTER US
unicode_line_break(0xA771, 0xA787, 'AL'). % L&  [23] LATIN SMALL LETTER DUM..LATIN SMALL LETTER INSULAR T
unicode_line_break(0xA788, 0xA788, 'AL'). % Lm       MODIFIER LETTER LOW CIRCUMFLEX ACCENT
unicode_line_break(0xA789, 0xA78A, 'AL'). % Sk   [2] MODIFIER LETTER COLON..MODIFIER LETTER SHORT EQUALS SIGN
unicode_line_break(0xA78B, 0xA78E, 'AL'). % L&   [4] LATIN CAPITAL LETTER SALTILLO..LATIN SMALL LETTER L WITH RETROFLEX HOOK AND BELT
unicode_line_break(0xA790, 0xA793, 'AL'). % L&   [4] LATIN CAPITAL LETTER N WITH DESCENDER..LATIN SMALL LETTER C WITH BAR
unicode_line_break(0xA7A0, 0xA7AA, 'AL'). % L&  [11] LATIN CAPITAL LETTER G WITH OBLIQUE STROKE..LATIN CAPITAL LETTER H WITH HOOK
unicode_line_break(0xA7F8, 0xA7F9, 'AL'). % Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_line_break(0xA7FA, 0xA7FA, 'AL'). % L&       LATIN LETTER SMALL CAPITAL TURNED M
unicode_line_break(0xA7FB, 0xA801, 'AL'). % Lo   [7] LATIN EPIGRAPHIC LETTER REVERSED F..SYLOTI NAGRI LETTER I
unicode_line_break(0xA803, 0xA805, 'AL'). % Lo   [3] SYLOTI NAGRI LETTER U..SYLOTI NAGRI LETTER O
unicode_line_break(0xA807, 0xA80A, 'AL'). % Lo   [4] SYLOTI NAGRI LETTER KO..SYLOTI NAGRI LETTER GHO
unicode_line_break(0xA80C, 0xA822, 'AL'). % Lo  [23] SYLOTI NAGRI LETTER CO..SYLOTI NAGRI LETTER HO
unicode_line_break(0xA828, 0xA82B, 'AL'). % So   [4] SYLOTI NAGRI POETRY MARK-1..SYLOTI NAGRI POETRY MARK-4
unicode_line_break(0xA830, 0xA835, 'AL'). % No   [6] NORTH INDIC FRACTION ONE QUARTER..NORTH INDIC FRACTION THREE SIXTEENTHS
unicode_line_break(0xA836, 0xA837, 'AL'). % So   [2] NORTH INDIC QUARTER MARK..NORTH INDIC PLACEHOLDER MARK
unicode_line_break(0xA839, 0xA839, 'AL'). % So       NORTH INDIC QUANTITY MARK
unicode_line_break(0xA840, 0xA873, 'AL'). % Lo  [52] PHAGS-PA LETTER KA..PHAGS-PA LETTER CANDRABINDU
unicode_line_break(0xA882, 0xA8B3, 'AL'). % Lo  [50] SAURASHTRA LETTER A..SAURASHTRA LETTER LLA
unicode_line_break(0xA8F2, 0xA8F7, 'AL'). % Lo   [6] DEVANAGARI SIGN SPACING CANDRABINDU..DEVANAGARI SIGN CANDRABINDU AVAGRAHA
unicode_line_break(0xA8F8, 0xA8FA, 'AL'). % Po   [3] DEVANAGARI SIGN PUSHPIKA..DEVANAGARI CARET
unicode_line_break(0xA8FB, 0xA8FB, 'AL'). % Lo       DEVANAGARI HEADSTROKE
unicode_line_break(0xA90A, 0xA925, 'AL'). % Lo  [28] KAYAH LI LETTER KA..KAYAH LI LETTER OO
unicode_line_break(0xA930, 0xA946, 'AL'). % Lo  [23] REJANG LETTER KA..REJANG LETTER A
unicode_line_break(0xA95F, 0xA95F, 'AL'). % Po       REJANG SECTION MARK
unicode_line_break(0xA984, 0xA9B2, 'AL'). % Lo  [47] JAVANESE LETTER A..JAVANESE LETTER HA
unicode_line_break(0xA9C1, 0xA9C6, 'AL'). % Po   [6] JAVANESE LEFT RERENGGAN..JAVANESE PADA WINDU
unicode_line_break(0xA9CA, 0xA9CD, 'AL'). % Po   [4] JAVANESE PADA ADEG..JAVANESE TURNED PADA PISELEH
unicode_line_break(0xA9CF, 0xA9CF, 'AL'). % Lm       JAVANESE PANGRANGKEP
unicode_line_break(0xA9DE, 0xA9DF, 'AL'). % Po   [2] JAVANESE PADA TIRTA TUMETES..JAVANESE PADA ISEN-ISEN
unicode_line_break(0xAA00, 0xAA28, 'AL'). % Lo  [41] CHAM LETTER A..CHAM LETTER HA
unicode_line_break(0xAA40, 0xAA42, 'AL'). % Lo   [3] CHAM LETTER FINAL K..CHAM LETTER FINAL NG
unicode_line_break(0xAA44, 0xAA4B, 'AL'). % Lo   [8] CHAM LETTER FINAL CH..CHAM LETTER FINAL SS
unicode_line_break(0xAA5C, 0xAA5C, 'AL'). % Po       CHAM PUNCTUATION SPIRAL
unicode_line_break(0xAAE0, 0xAAEA, 'AL'). % Lo  [11] MEETEI MAYEK LETTER E..MEETEI MAYEK LETTER SSA
unicode_line_break(0xAAF2, 0xAAF2, 'AL'). % Lo       MEETEI MAYEK ANJI
unicode_line_break(0xAAF3, 0xAAF4, 'AL'). % Lm   [2] MEETEI MAYEK SYLLABLE REPETITION MARK..MEETEI MAYEK WORD REPETITION MARK
unicode_line_break(0xAB01, 0xAB06, 'AL'). % Lo   [6] ETHIOPIC SYLLABLE TTHU..ETHIOPIC SYLLABLE TTHO
unicode_line_break(0xAB09, 0xAB0E, 'AL'). % Lo   [6] ETHIOPIC SYLLABLE DDHU..ETHIOPIC SYLLABLE DDHO
unicode_line_break(0xAB11, 0xAB16, 'AL'). % Lo   [6] ETHIOPIC SYLLABLE DZU..ETHIOPIC SYLLABLE DZO
unicode_line_break(0xAB20, 0xAB26, 'AL'). % Lo   [7] ETHIOPIC SYLLABLE CCHHA..ETHIOPIC SYLLABLE CCHHO
unicode_line_break(0xAB28, 0xAB2E, 'AL'). % Lo   [7] ETHIOPIC SYLLABLE BBA..ETHIOPIC SYLLABLE BBO
unicode_line_break(0xABC0, 0xABE2, 'AL'). % Lo  [35] MEETEI MAYEK LETTER KOK..MEETEI MAYEK LETTER I LONSUM
unicode_line_break(0xFB00, 0xFB06, 'AL'). % L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_line_break(0xFB13, 0xFB17, 'AL'). % L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_line_break(0xFB29, 0xFB29, 'AL'). % Sm       HEBREW LETTER ALTERNATIVE PLUS SIGN
unicode_line_break(0xFB50, 0xFBB1, 'AL'). % Lo  [98] ARABIC LETTER ALEF WASLA ISOLATED FORM..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
unicode_line_break(0xFBB2, 0xFBC1, 'AL'). % Sk  [16] ARABIC SYMBOL DOT ABOVE..ARABIC SYMBOL SMALL TAH BELOW
unicode_line_break(0xFBD3, 0xFD3D, 'AL'). % Lo [363] ARABIC LETTER NG ISOLATED FORM..ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
unicode_line_break(0xFD50, 0xFD8F, 'AL'). % Lo  [64] ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
unicode_line_break(0xFD92, 0xFDC7, 'AL'). % Lo  [54] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
unicode_line_break(0xFDF0, 0xFDFB, 'AL'). % Lo  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
unicode_line_break(0xFDFD, 0xFDFD, 'AL'). % So       ARABIC LIGATURE BISMILLAH AR-RAHMAN AR-RAHEEM
unicode_line_break(0xFE70, 0xFE74, 'AL'). % Lo   [5] ARABIC FATHATAN ISOLATED FORM..ARABIC KASRATAN ISOLATED FORM
unicode_line_break(0xFE76, 0xFEFC, 'AL'). % Lo [135] ARABIC FATHA ISOLATED FORM..ARABIC LIGATURE LAM WITH ALEF FINAL FORM
unicode_line_break(0xFF66, 0xFF66, 'AL'). % Lo       HALFWIDTH KATAKANA LETTER WO
unicode_line_break(0xFF71, 0xFF9D, 'AL'). % Lo  [45] HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N
unicode_line_break(0xFFA0, 0xFFBE, 'AL'). % Lo  [31] HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH
unicode_line_break(0xFFC2, 0xFFC7, 'AL'). % Lo   [6] HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E
unicode_line_break(0xFFCA, 0xFFCF, 'AL'). % Lo   [6] HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE
unicode_line_break(0xFFD2, 0xFFD7, 'AL'). % Lo   [6] HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU
unicode_line_break(0xFFDA, 0xFFDC, 'AL'). % Lo   [3] HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I
unicode_line_break(0xFFE8, 0xFFE8, 'AL'). % So       HALFWIDTH FORMS LIGHT VERTICAL
unicode_line_break(0xFFE9, 0xFFEC, 'AL'). % Sm   [4] HALFWIDTH LEFTWARDS ARROW..HALFWIDTH DOWNWARDS ARROW
unicode_line_break(0xFFED, 0xFFEE, 'AL'). % So   [2] HALFWIDTH BLACK SQUARE..HALFWIDTH WHITE CIRCLE
unicode_line_break(0x10000, 0x1000B, 'AL'). % Lo  [12] LINEAR B SYLLABLE B008 A..LINEAR B SYLLABLE B046 JE
unicode_line_break(0x1000D, 0x10026, 'AL'). % Lo  [26] LINEAR B SYLLABLE B036 JO..LINEAR B SYLLABLE B032 QO
unicode_line_break(0x10028, 0x1003A, 'AL'). % Lo  [19] LINEAR B SYLLABLE B060 RA..LINEAR B SYLLABLE B042 WO
unicode_line_break(0x1003C, 0x1003D, 'AL'). % Lo   [2] LINEAR B SYLLABLE B017 ZA..LINEAR B SYLLABLE B074 ZE
unicode_line_break(0x1003F, 0x1004D, 'AL'). % Lo  [15] LINEAR B SYLLABLE B020 ZO..LINEAR B SYLLABLE B091 TWO
unicode_line_break(0x10050, 0x1005D, 'AL'). % Lo  [14] LINEAR B SYMBOL B018..LINEAR B SYMBOL B089
unicode_line_break(0x10080, 0x100FA, 'AL'). % Lo [123] LINEAR B IDEOGRAM B100 MAN..LINEAR B IDEOGRAM VESSEL B305
unicode_line_break(0x10107, 0x10133, 'AL'). % No  [45] AEGEAN NUMBER ONE..AEGEAN NUMBER NINETY THOUSAND
unicode_line_break(0x10137, 0x1013F, 'AL'). % So   [9] AEGEAN WEIGHT BASE UNIT..AEGEAN MEASURE THIRD SUBUNIT
unicode_line_break(0x10140, 0x10174, 'AL'). % Nl  [53] GREEK ACROPHONIC ATTIC ONE QUARTER..GREEK ACROPHONIC STRATIAN FIFTY MNAS
unicode_line_break(0x10175, 0x10178, 'AL'). % No   [4] GREEK ONE HALF SIGN..GREEK THREE QUARTERS SIGN
unicode_line_break(0x10179, 0x10189, 'AL'). % So  [17] GREEK YEAR SIGN..GREEK TRYBLION BASE SIGN
unicode_line_break(0x1018A, 0x1018A, 'AL'). % No       GREEK ZERO SIGN
unicode_line_break(0x10190, 0x1019B, 'AL'). % So  [12] ROMAN SEXTANS SIGN..ROMAN CENTURIAL SIGN
unicode_line_break(0x101D0, 0x101FC, 'AL'). % So  [45] PHAISTOS DISC SIGN PEDESTRIAN..PHAISTOS DISC SIGN WAVY BAND
unicode_line_break(0x10280, 0x1029C, 'AL'). % Lo  [29] LYCIAN LETTER A..LYCIAN LETTER X
unicode_line_break(0x102A0, 0x102D0, 'AL'). % Lo  [49] CARIAN LETTER A..CARIAN LETTER UUU3
unicode_line_break(0x10300, 0x1031E, 'AL'). % Lo  [31] OLD ITALIC LETTER A..OLD ITALIC LETTER UU
unicode_line_break(0x10320, 0x10323, 'AL'). % No   [4] OLD ITALIC NUMERAL ONE..OLD ITALIC NUMERAL FIFTY
unicode_line_break(0x10330, 0x10340, 'AL'). % Lo  [17] GOTHIC LETTER AHSA..GOTHIC LETTER PAIRTHRA
unicode_line_break(0x10341, 0x10341, 'AL'). % Nl       GOTHIC LETTER NINETY
unicode_line_break(0x10342, 0x10349, 'AL'). % Lo   [8] GOTHIC LETTER RAIDA..GOTHIC LETTER OTHAL
unicode_line_break(0x1034A, 0x1034A, 'AL'). % Nl       GOTHIC LETTER NINE HUNDRED
unicode_line_break(0x10380, 0x1039D, 'AL'). % Lo  [30] UGARITIC LETTER ALPA..UGARITIC LETTER SSU
unicode_line_break(0x103A0, 0x103C3, 'AL'). % Lo  [36] OLD PERSIAN SIGN A..OLD PERSIAN SIGN HA
unicode_line_break(0x103C8, 0x103CF, 'AL'). % Lo   [8] OLD PERSIAN SIGN AURAMAZDAA..OLD PERSIAN SIGN BUUMISH
unicode_line_break(0x103D1, 0x103D5, 'AL'). % Nl   [5] OLD PERSIAN NUMBER ONE..OLD PERSIAN NUMBER HUNDRED
unicode_line_break(0x10400, 0x1044F, 'AL'). % L&  [80] DESERET CAPITAL LETTER LONG I..DESERET SMALL LETTER EW
unicode_line_break(0x10450, 0x1049D, 'AL'). % Lo  [78] SHAVIAN LETTER PEEP..OSMANYA LETTER OO
unicode_line_break(0x10800, 0x10805, 'AL'). % Lo   [6] CYPRIOT SYLLABLE A..CYPRIOT SYLLABLE JA
unicode_line_break(0x10808, 0x10808, 'AL'). % Lo       CYPRIOT SYLLABLE JO
unicode_line_break(0x1080A, 0x10835, 'AL'). % Lo  [44] CYPRIOT SYLLABLE KA..CYPRIOT SYLLABLE WO
unicode_line_break(0x10837, 0x10838, 'AL'). % Lo   [2] CYPRIOT SYLLABLE XA..CYPRIOT SYLLABLE XE
unicode_line_break(0x1083C, 0x1083C, 'AL'). % Lo       CYPRIOT SYLLABLE ZA
unicode_line_break(0x1083F, 0x10855, 'AL'). % Lo  [23] CYPRIOT SYLLABLE ZO..IMPERIAL ARAMAIC LETTER TAW
unicode_line_break(0x10858, 0x1085F, 'AL'). % No   [8] IMPERIAL ARAMAIC NUMBER ONE..IMPERIAL ARAMAIC NUMBER TEN THOUSAND
unicode_line_break(0x10900, 0x10915, 'AL'). % Lo  [22] PHOENICIAN LETTER ALF..PHOENICIAN LETTER TAU
unicode_line_break(0x10916, 0x1091B, 'AL'). % No   [6] PHOENICIAN NUMBER ONE..PHOENICIAN NUMBER THREE
unicode_line_break(0x10920, 0x10939, 'AL'). % Lo  [26] LYDIAN LETTER A..LYDIAN LETTER C
unicode_line_break(0x1093F, 0x1093F, 'AL'). % Po       LYDIAN TRIANGULAR MARK
unicode_line_break(0x10980, 0x109B7, 'AL'). % Lo  [56] MEROITIC HIEROGLYPHIC LETTER A..MEROITIC CURSIVE LETTER DA
unicode_line_break(0x109BE, 0x109BF, 'AL'). % Lo   [2] MEROITIC CURSIVE LOGOGRAM RMT..MEROITIC CURSIVE LOGOGRAM IMN
unicode_line_break(0x10A00, 0x10A00, 'AL'). % Lo       KHAROSHTHI LETTER A
unicode_line_break(0x10A10, 0x10A13, 'AL'). % Lo   [4] KHAROSHTHI LETTER KA..KHAROSHTHI LETTER GHA
unicode_line_break(0x10A15, 0x10A17, 'AL'). % Lo   [3] KHAROSHTHI LETTER CA..KHAROSHTHI LETTER JA
unicode_line_break(0x10A19, 0x10A33, 'AL'). % Lo  [27] KHAROSHTHI LETTER NYA..KHAROSHTHI LETTER TTTHA
unicode_line_break(0x10A40, 0x10A47, 'AL'). % No   [8] KHAROSHTHI DIGIT ONE..KHAROSHTHI NUMBER ONE THOUSAND
unicode_line_break(0x10A58, 0x10A58, 'AL'). % Po       KHAROSHTHI PUNCTUATION LINES
unicode_line_break(0x10A60, 0x10A7C, 'AL'). % Lo  [29] OLD SOUTH ARABIAN LETTER HE..OLD SOUTH ARABIAN LETTER THETH
unicode_line_break(0x10A7D, 0x10A7E, 'AL'). % No   [2] OLD SOUTH ARABIAN NUMBER ONE..OLD SOUTH ARABIAN NUMBER FIFTY
unicode_line_break(0x10A7F, 0x10A7F, 'AL'). % Po       OLD SOUTH ARABIAN NUMERIC INDICATOR
unicode_line_break(0x10B00, 0x10B35, 'AL'). % Lo  [54] AVESTAN LETTER A..AVESTAN LETTER HE
unicode_line_break(0x10B40, 0x10B55, 'AL'). % Lo  [22] INSCRIPTIONAL PARTHIAN LETTER ALEPH..INSCRIPTIONAL PARTHIAN LETTER TAW
unicode_line_break(0x10B58, 0x10B5F, 'AL'). % No   [8] INSCRIPTIONAL PARTHIAN NUMBER ONE..INSCRIPTIONAL PARTHIAN NUMBER ONE THOUSAND
unicode_line_break(0x10B60, 0x10B72, 'AL'). % Lo  [19] INSCRIPTIONAL PAHLAVI LETTER ALEPH..INSCRIPTIONAL PAHLAVI LETTER TAW
unicode_line_break(0x10B78, 0x10B7F, 'AL'). % No   [8] INSCRIPTIONAL PAHLAVI NUMBER ONE..INSCRIPTIONAL PAHLAVI NUMBER ONE THOUSAND
unicode_line_break(0x10C00, 0x10C48, 'AL'). % Lo  [73] OLD TURKIC LETTER ORKHON A..OLD TURKIC LETTER ORKHON BASH
unicode_line_break(0x10E60, 0x10E7E, 'AL'). % No  [31] RUMI DIGIT ONE..RUMI FRACTION TWO THIRDS
unicode_line_break(0x11003, 0x11037, 'AL'). % Lo  [53] BRAHMI SIGN JIHVAMULIYA..BRAHMI LETTER OLD TAMIL NNNA
unicode_line_break(0x11049, 0x1104D, 'AL'). % Po   [5] BRAHMI PUNCTUATION DOT..BRAHMI PUNCTUATION LOTUS
unicode_line_break(0x11052, 0x11065, 'AL'). % No  [20] BRAHMI NUMBER ONE..BRAHMI NUMBER ONE THOUSAND
unicode_line_break(0x11083, 0x110AF, 'AL'). % Lo  [45] KAITHI LETTER A..KAITHI LETTER HA
unicode_line_break(0x110BB, 0x110BC, 'AL'). % Po   [2] KAITHI ABBREVIATION SIGN..KAITHI ENUMERATION SIGN
unicode_line_break(0x110BD, 0x110BD, 'AL'). % Cf       KAITHI NUMBER SIGN
unicode_line_break(0x110D0, 0x110E8, 'AL'). % Lo  [25] SORA SOMPENG LETTER SAH..SORA SOMPENG LETTER MAE
unicode_line_break(0x11103, 0x11126, 'AL'). % Lo  [36] CHAKMA LETTER AA..CHAKMA LETTER HAA
unicode_line_break(0x11183, 0x111B2, 'AL'). % Lo  [48] SHARADA LETTER A..SHARADA LETTER HA
unicode_line_break(0x111C1, 0x111C4, 'AL'). % Lo   [4] SHARADA SIGN AVAGRAHA..SHARADA OM
unicode_line_break(0x111C7, 0x111C7, 'AL'). % Po       SHARADA ABBREVIATION SIGN
unicode_line_break(0x11680, 0x116AA, 'AL'). % Lo  [43] TAKRI LETTER A..TAKRI LETTER RRA
unicode_line_break(0x12000, 0x1236E, 'AL'). % Lo [879] CUNEIFORM SIGN A..CUNEIFORM SIGN ZUM
unicode_line_break(0x12400, 0x12462, 'AL'). % Nl  [99] CUNEIFORM NUMERIC SIGN TWO ASH..CUNEIFORM NUMERIC SIGN OLD ASSYRIAN ONE QUARTER
unicode_line_break(0x13000, 0x13257, 'AL'). % Lo [600] EGYPTIAN HIEROGLYPH A001..EGYPTIAN HIEROGLYPH O006
unicode_line_break(0x1325E, 0x13281, 'AL'). % Lo  [36] EGYPTIAN HIEROGLYPH O007..EGYPTIAN HIEROGLYPH O033
unicode_line_break(0x13283, 0x13285, 'AL'). % Lo   [3] EGYPTIAN HIEROGLYPH O034..EGYPTIAN HIEROGLYPH O036
unicode_line_break(0x1328A, 0x13378, 'AL'). % Lo [239] EGYPTIAN HIEROGLYPH O037..EGYPTIAN HIEROGLYPH V011
unicode_line_break(0x1337C, 0x1342E, 'AL'). % Lo [179] EGYPTIAN HIEROGLYPH V012..EGYPTIAN HIEROGLYPH AA032
unicode_line_break(0x16800, 0x16A38, 'AL'). % Lo [569] BAMUM LETTER PHASE-A NGKUE MFON..BAMUM LETTER PHASE-F VUEQ
unicode_line_break(0x16F00, 0x16F44, 'AL'). % Lo  [69] MIAO LETTER PA..MIAO LETTER HHA
unicode_line_break(0x16F50, 0x16F50, 'AL'). % Lo       MIAO LETTER NASALIZATION
unicode_line_break(0x16F93, 0x16F9F, 'AL'). % Lm  [13] MIAO LETTER TONE-2..MIAO LETTER REFORMED TONE-8
unicode_line_break(0x1D000, 0x1D0F5, 'AL'). % So [246] BYZANTINE MUSICAL SYMBOL PSILI..BYZANTINE MUSICAL SYMBOL GORGON NEO KATO
unicode_line_break(0x1D100, 0x1D126, 'AL'). % So  [39] MUSICAL SYMBOL SINGLE BARLINE..MUSICAL SYMBOL DRUM CLEF-2
unicode_line_break(0x1D129, 0x1D164, 'AL'). % So  [60] MUSICAL SYMBOL MULTIPLE MEASURE REST..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
unicode_line_break(0x1D16A, 0x1D16C, 'AL'). % So   [3] MUSICAL SYMBOL FINGERED TREMOLO-1..MUSICAL SYMBOL FINGERED TREMOLO-3
unicode_line_break(0x1D183, 0x1D184, 'AL'). % So   [2] MUSICAL SYMBOL ARPEGGIATO UP..MUSICAL SYMBOL ARPEGGIATO DOWN
unicode_line_break(0x1D18C, 0x1D1A9, 'AL'). % So  [30] MUSICAL SYMBOL RINFORZANDO..MUSICAL SYMBOL DEGREE SLASH
unicode_line_break(0x1D1AE, 0x1D1DD, 'AL'). % So  [48] MUSICAL SYMBOL PEDAL MARK..MUSICAL SYMBOL PES SUBPUNCTIS
unicode_line_break(0x1D200, 0x1D241, 'AL'). % So  [66] GREEK VOCAL NOTATION SYMBOL-1..GREEK INSTRUMENTAL NOTATION SYMBOL-54
unicode_line_break(0x1D245, 0x1D245, 'AL'). % So       GREEK MUSICAL LEIMMA
unicode_line_break(0x1D300, 0x1D356, 'AL'). % So  [87] MONOGRAM FOR EARTH..TETRAGRAM FOR FOSTERING
unicode_line_break(0x1D360, 0x1D371, 'AL'). % No  [18] COUNTING ROD UNIT DIGIT ONE..COUNTING ROD TENS DIGIT NINE
unicode_line_break(0x1D400, 0x1D454, 'AL'). % L&  [85] MATHEMATICAL BOLD CAPITAL A..MATHEMATICAL ITALIC SMALL G
unicode_line_break(0x1D456, 0x1D49C, 'AL'). % L&  [71] MATHEMATICAL ITALIC SMALL I..MATHEMATICAL SCRIPT CAPITAL A
unicode_line_break(0x1D49E, 0x1D49F, 'AL'). % L&   [2] MATHEMATICAL SCRIPT CAPITAL C..MATHEMATICAL SCRIPT CAPITAL D
unicode_line_break(0x1D4A2, 0x1D4A2, 'AL'). % L&       MATHEMATICAL SCRIPT CAPITAL G
unicode_line_break(0x1D4A5, 0x1D4A6, 'AL'). % L&   [2] MATHEMATICAL SCRIPT CAPITAL J..MATHEMATICAL SCRIPT CAPITAL K
unicode_line_break(0x1D4A9, 0x1D4AC, 'AL'). % L&   [4] MATHEMATICAL SCRIPT CAPITAL N..MATHEMATICAL SCRIPT CAPITAL Q
unicode_line_break(0x1D4AE, 0x1D4B9, 'AL'). % L&  [12] MATHEMATICAL SCRIPT CAPITAL S..MATHEMATICAL SCRIPT SMALL D
unicode_line_break(0x1D4BB, 0x1D4BB, 'AL'). % L&       MATHEMATICAL SCRIPT SMALL F
unicode_line_break(0x1D4BD, 0x1D4C3, 'AL'). % L&   [7] MATHEMATICAL SCRIPT SMALL H..MATHEMATICAL SCRIPT SMALL N
unicode_line_break(0x1D4C5, 0x1D505, 'AL'). % L&  [65] MATHEMATICAL SCRIPT SMALL P..MATHEMATICAL FRAKTUR CAPITAL B
unicode_line_break(0x1D507, 0x1D50A, 'AL'). % L&   [4] MATHEMATICAL FRAKTUR CAPITAL D..MATHEMATICAL FRAKTUR CAPITAL G
unicode_line_break(0x1D50D, 0x1D514, 'AL'). % L&   [8] MATHEMATICAL FRAKTUR CAPITAL J..MATHEMATICAL FRAKTUR CAPITAL Q
unicode_line_break(0x1D516, 0x1D51C, 'AL'). % L&   [7] MATHEMATICAL FRAKTUR CAPITAL S..MATHEMATICAL FRAKTUR CAPITAL Y
unicode_line_break(0x1D51E, 0x1D539, 'AL'). % L&  [28] MATHEMATICAL FRAKTUR SMALL A..MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_line_break(0x1D53B, 0x1D53E, 'AL'). % L&   [4] MATHEMATICAL DOUBLE-STRUCK CAPITAL D..MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_line_break(0x1D540, 0x1D544, 'AL'). % L&   [5] MATHEMATICAL DOUBLE-STRUCK CAPITAL I..MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_line_break(0x1D546, 0x1D546, 'AL'). % L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_line_break(0x1D54A, 0x1D550, 'AL'). % L&   [7] MATHEMATICAL DOUBLE-STRUCK CAPITAL S..MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_line_break(0x1D552, 0x1D6A5, 'AL'). % L& [340] MATHEMATICAL DOUBLE-STRUCK SMALL A..MATHEMATICAL ITALIC SMALL DOTLESS J
unicode_line_break(0x1D6A8, 0x1D6C0, 'AL'). % L&  [25] MATHEMATICAL BOLD CAPITAL ALPHA..MATHEMATICAL BOLD CAPITAL OMEGA
unicode_line_break(0x1D6C1, 0x1D6C1, 'AL'). % Sm       MATHEMATICAL BOLD NABLA
unicode_line_break(0x1D6C2, 0x1D6DA, 'AL'). % L&  [25] MATHEMATICAL BOLD SMALL ALPHA..MATHEMATICAL BOLD SMALL OMEGA
unicode_line_break(0x1D6DB, 0x1D6DB, 'AL'). % Sm       MATHEMATICAL BOLD PARTIAL DIFFERENTIAL
unicode_line_break(0x1D6DC, 0x1D6FA, 'AL'). % L&  [31] MATHEMATICAL BOLD EPSILON SYMBOL..MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_line_break(0x1D6FB, 0x1D6FB, 'AL'). % Sm       MATHEMATICAL ITALIC NABLA
unicode_line_break(0x1D6FC, 0x1D714, 'AL'). % L&  [25] MATHEMATICAL ITALIC SMALL ALPHA..MATHEMATICAL ITALIC SMALL OMEGA
unicode_line_break(0x1D715, 0x1D715, 'AL'). % Sm       MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL
unicode_line_break(0x1D716, 0x1D734, 'AL'). % L&  [31] MATHEMATICAL ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_line_break(0x1D735, 0x1D735, 'AL'). % Sm       MATHEMATICAL BOLD ITALIC NABLA
unicode_line_break(0x1D736, 0x1D74E, 'AL'). % L&  [25] MATHEMATICAL BOLD ITALIC SMALL ALPHA..MATHEMATICAL BOLD ITALIC SMALL OMEGA
unicode_line_break(0x1D74F, 0x1D74F, 'AL'). % Sm       MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_line_break(0x1D750, 0x1D76E, 'AL'). % L&  [31] MATHEMATICAL BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_line_break(0x1D76F, 0x1D76F, 'AL'). % Sm       MATHEMATICAL SANS-SERIF BOLD NABLA
unicode_line_break(0x1D770, 0x1D788, 'AL'). % L&  [25] MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
unicode_line_break(0x1D789, 0x1D789, 'AL'). % Sm       MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL
unicode_line_break(0x1D78A, 0x1D7A8, 'AL'). % L&  [31] MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_line_break(0x1D7A9, 0x1D7A9, 'AL'). % Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA
unicode_line_break(0x1D7AA, 0x1D7C2, 'AL'). % L&  [25] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
unicode_line_break(0x1D7C3, 0x1D7C3, 'AL'). % Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_line_break(0x1D7C4, 0x1D7CB, 'AL'). % L&   [8] MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD SMALL DIGAMMA
unicode_line_break(0x1EE00, 0x1EE03, 'AL'). % Lo   [4] ARABIC MATHEMATICAL ALEF..ARABIC MATHEMATICAL DAL
unicode_line_break(0x1EE05, 0x1EE1F, 'AL'). % Lo  [27] ARABIC MATHEMATICAL WAW..ARABIC MATHEMATICAL DOTLESS QAF
unicode_line_break(0x1EE21, 0x1EE22, 'AL'). % Lo   [2] ARABIC MATHEMATICAL INITIAL BEH..ARABIC MATHEMATICAL INITIAL JEEM
unicode_line_break(0x1EE24, 0x1EE24, 'AL'). % Lo       ARABIC MATHEMATICAL INITIAL HEH
unicode_line_break(0x1EE27, 0x1EE27, 'AL'). % Lo       ARABIC MATHEMATICAL INITIAL HAH
unicode_line_break(0x1EE29, 0x1EE32, 'AL'). % Lo  [10] ARABIC MATHEMATICAL INITIAL YEH..ARABIC MATHEMATICAL INITIAL QAF
unicode_line_break(0x1EE34, 0x1EE37, 'AL'). % Lo   [4] ARABIC MATHEMATICAL INITIAL SHEEN..ARABIC MATHEMATICAL INITIAL KHAH
unicode_line_break(0x1EE39, 0x1EE39, 'AL'). % Lo       ARABIC MATHEMATICAL INITIAL DAD
unicode_line_break(0x1EE3B, 0x1EE3B, 'AL'). % Lo       ARABIC MATHEMATICAL INITIAL GHAIN
unicode_line_break(0x1EE42, 0x1EE42, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED JEEM
unicode_line_break(0x1EE47, 0x1EE47, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED HAH
unicode_line_break(0x1EE49, 0x1EE49, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED YEH
unicode_line_break(0x1EE4B, 0x1EE4B, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED LAM
unicode_line_break(0x1EE4D, 0x1EE4F, 'AL'). % Lo   [3] ARABIC MATHEMATICAL TAILED NOON..ARABIC MATHEMATICAL TAILED AIN
unicode_line_break(0x1EE51, 0x1EE52, 'AL'). % Lo   [2] ARABIC MATHEMATICAL TAILED SAD..ARABIC MATHEMATICAL TAILED QAF
unicode_line_break(0x1EE54, 0x1EE54, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED SHEEN
unicode_line_break(0x1EE57, 0x1EE57, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED KHAH
unicode_line_break(0x1EE59, 0x1EE59, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED DAD
unicode_line_break(0x1EE5B, 0x1EE5B, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED GHAIN
unicode_line_break(0x1EE5D, 0x1EE5D, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED DOTLESS NOON
unicode_line_break(0x1EE5F, 0x1EE5F, 'AL'). % Lo       ARABIC MATHEMATICAL TAILED DOTLESS QAF
unicode_line_break(0x1EE61, 0x1EE62, 'AL'). % Lo   [2] ARABIC MATHEMATICAL STRETCHED BEH..ARABIC MATHEMATICAL STRETCHED JEEM
unicode_line_break(0x1EE64, 0x1EE64, 'AL'). % Lo       ARABIC MATHEMATICAL STRETCHED HEH
unicode_line_break(0x1EE67, 0x1EE6A, 'AL'). % Lo   [4] ARABIC MATHEMATICAL STRETCHED HAH..ARABIC MATHEMATICAL STRETCHED KAF
unicode_line_break(0x1EE6C, 0x1EE72, 'AL'). % Lo   [7] ARABIC MATHEMATICAL STRETCHED MEEM..ARABIC MATHEMATICAL STRETCHED QAF
unicode_line_break(0x1EE74, 0x1EE77, 'AL'). % Lo   [4] ARABIC MATHEMATICAL STRETCHED SHEEN..ARABIC MATHEMATICAL STRETCHED KHAH
unicode_line_break(0x1EE79, 0x1EE7C, 'AL'). % Lo   [4] ARABIC MATHEMATICAL STRETCHED DAD..ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
unicode_line_break(0x1EE7E, 0x1EE7E, 'AL'). % Lo       ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
unicode_line_break(0x1EE80, 0x1EE89, 'AL'). % Lo  [10] ARABIC MATHEMATICAL LOOPED ALEF..ARABIC MATHEMATICAL LOOPED YEH
unicode_line_break(0x1EE8B, 0x1EE9B, 'AL'). % Lo  [17] ARABIC MATHEMATICAL LOOPED LAM..ARABIC MATHEMATICAL LOOPED GHAIN
unicode_line_break(0x1EEA1, 0x1EEA3, 'AL'). % Lo   [3] ARABIC MATHEMATICAL DOUBLE-STRUCK BEH..ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
unicode_line_break(0x1EEA5, 0x1EEA9, 'AL'). % Lo   [5] ARABIC MATHEMATICAL DOUBLE-STRUCK WAW..ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
unicode_line_break(0x1EEAB, 0x1EEBB, 'AL'). % Lo  [17] ARABIC MATHEMATICAL DOUBLE-STRUCK LAM..ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN
unicode_line_break(0x1EEF0, 0x1EEF1, 'AL'). % Sm   [2] ARABIC MATHEMATICAL OPERATOR MEEM WITH HAH WITH TATWEEL..ARABIC MATHEMATICAL OPERATOR HAH WITH DAL
unicode_line_break(0x1F000, 0x1F02B, 'AL'). % So  [44] MAHJONG TILE EAST WIND..MAHJONG TILE BACK
unicode_line_break(0x1F030, 0x1F093, 'AL'). % So [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
unicode_line_break(0x1F0A0, 0x1F0AE, 'AL'). % So  [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
unicode_line_break(0x1F0B1, 0x1F0BE, 'AL'). % So  [14] PLAYING CARD ACE OF HEARTS..PLAYING CARD KING OF HEARTS
unicode_line_break(0x1F0C1, 0x1F0CF, 'AL'). % So  [15] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD BLACK JOKER
unicode_line_break(0x1F0D1, 0x1F0DF, 'AL'). % So  [15] PLAYING CARD ACE OF CLUBS..PLAYING CARD WHITE JOKER
unicode_line_break(0x1F12E, 0x1F12E, 'AL'). % So       CIRCLED WZ
unicode_line_break(0x1F16A, 0x1F16B, 'AL'). % So   [2] RAISED MC SIGN..RAISED MD SIGN
unicode_line_break(0x1F1E6, 0x1F1FF, 'AL'). % So  [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
unicode_line_break(0x1F300, 0x1F320, 'AL'). % So  [33] CYCLONE..SHOOTING STAR
unicode_line_break(0x1F330, 0x1F335, 'AL'). % So   [6] CHESTNUT..CACTUS
unicode_line_break(0x1F337, 0x1F37C, 'AL'). % So  [70] TULIP..BABY BOTTLE
unicode_line_break(0x1F380, 0x1F393, 'AL'). % So  [20] RIBBON..GRADUATION CAP
unicode_line_break(0x1F3A0, 0x1F3C4, 'AL'). % So  [37] CAROUSEL HORSE..SURFER
unicode_line_break(0x1F3C6, 0x1F3CA, 'AL'). % So   [5] TROPHY..SWIMMER
unicode_line_break(0x1F3E0, 0x1F3F0, 'AL'). % So  [17] HOUSE BUILDING..EUROPEAN CASTLE
unicode_line_break(0x1F400, 0x1F43E, 'AL'). % So  [63] RAT..PAW PRINTS
unicode_line_break(0x1F440, 0x1F440, 'AL'). % So       EYES
unicode_line_break(0x1F442, 0x1F4F7, 'AL'). % So [182] EAR..CAMERA
unicode_line_break(0x1F4F9, 0x1F4FC, 'AL'). % So   [4] VIDEO CAMERA..VIDEOCASSETTE
unicode_line_break(0x1F500, 0x1F53D, 'AL'). % So  [62] TWISTED RIGHTWARDS ARROWS..DOWN-POINTING SMALL RED TRIANGLE
unicode_line_break(0x1F540, 0x1F543, 'AL'). % So   [4] CIRCLED CROSS POMMEE..NOTCHED LEFT SEMICIRCLE WITH THREE DOTS
unicode_line_break(0x1F550, 0x1F567, 'AL'). % So  [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
unicode_line_break(0x1F5FB, 0x1F640, 'AL'). % So  [70] MOUNT FUJI..WEARY CAT FACE
unicode_line_break(0x1F645, 0x1F64F, 'AL'). % So  [11] FACE WITH NO GOOD GESTURE..PERSON WITH FOLDED HANDS
unicode_line_break(0x1F680, 0x1F6C5, 'AL'). % So  [70] ROCKET..LEFT LUGGAGE
unicode_line_break(0x1F700, 0x1F773, 'AL'). % So [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE

% Total code points: 16251

% ================================================

% Line_Break=Ideographic

unicode_line_break(0x2E80, 0x2E99, 'ID'). % So  [26] CJK RADICAL REPEAT..CJK RADICAL RAP
unicode_line_break(0x2E9B, 0x2EF3, 'ID'). % So  [89] CJK RADICAL CHOKE..CJK RADICAL C-SIMPLIFIED TURTLE
unicode_line_break(0x2F00, 0x2FD5, 'ID'). % So [214] KANGXI RADICAL ONE..KANGXI RADICAL FLUTE
unicode_line_break(0x2FF0, 0x2FFB, 'ID'). % So  [12] IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT..IDEOGRAPHIC DESCRIPTION CHARACTER OVERLAID
unicode_line_break(0x3000, 0x3000, 'ID'). % Zs       IDEOGRAPHIC SPACE
unicode_line_break(0x3003, 0x3003, 'ID'). % Po       DITTO MARK
unicode_line_break(0x3004, 0x3004, 'ID'). % So       JAPANESE INDUSTRIAL STANDARD SYMBOL
unicode_line_break(0x3006, 0x3006, 'ID'). % Lo       IDEOGRAPHIC CLOSING MARK
unicode_line_break(0x3007, 0x3007, 'ID'). % Nl       IDEOGRAPHIC NUMBER ZERO
unicode_line_break(0x3012, 0x3013, 'ID'). % So   [2] POSTAL MARK..GETA MARK
unicode_line_break(0x3020, 0x3020, 'ID'). % So       POSTAL MARK FACE
unicode_line_break(0x3021, 0x3029, 'ID'). % Nl   [9] HANGZHOU NUMERAL ONE..HANGZHOU NUMERAL NINE
unicode_line_break(0x3030, 0x3030, 'ID'). % Pd       WAVY DASH
unicode_line_break(0x3031, 0x3035, 'ID'). % Lm   [5] VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF
unicode_line_break(0x3036, 0x3037, 'ID'). % So   [2] CIRCLED POSTAL MARK..IDEOGRAPHIC TELEGRAPH LINE FEED SEPARATOR SYMBOL
unicode_line_break(0x3038, 0x303A, 'ID'). % Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
unicode_line_break(0x303D, 0x303D, 'ID'). % Po       PART ALTERNATION MARK
unicode_line_break(0x303E, 0x303F, 'ID'). % So   [2] IDEOGRAPHIC VARIATION INDICATOR..IDEOGRAPHIC HALF FILL SPACE
unicode_line_break(0x3042, 0x3042, 'ID'). % Lo       HIRAGANA LETTER A
unicode_line_break(0x3044, 0x3044, 'ID'). % Lo       HIRAGANA LETTER I
unicode_line_break(0x3046, 0x3046, 'ID'). % Lo       HIRAGANA LETTER U
unicode_line_break(0x3048, 0x3048, 'ID'). % Lo       HIRAGANA LETTER E
unicode_line_break(0x304A, 0x3062, 'ID'). % Lo  [25] HIRAGANA LETTER O..HIRAGANA LETTER DI
unicode_line_break(0x3064, 0x3082, 'ID'). % Lo  [31] HIRAGANA LETTER TU..HIRAGANA LETTER MO
unicode_line_break(0x3084, 0x3084, 'ID'). % Lo       HIRAGANA LETTER YA
unicode_line_break(0x3086, 0x3086, 'ID'). % Lo       HIRAGANA LETTER YU
unicode_line_break(0x3088, 0x308D, 'ID'). % Lo   [6] HIRAGANA LETTER YO..HIRAGANA LETTER RO
unicode_line_break(0x308F, 0x3094, 'ID'). % Lo   [6] HIRAGANA LETTER WA..HIRAGANA LETTER VU
unicode_line_break(0x309F, 0x309F, 'ID'). % Lo       HIRAGANA DIGRAPH YORI
unicode_line_break(0x30A2, 0x30A2, 'ID'). % Lo       KATAKANA LETTER A
unicode_line_break(0x30A4, 0x30A4, 'ID'). % Lo       KATAKANA LETTER I
unicode_line_break(0x30A6, 0x30A6, 'ID'). % Lo       KATAKANA LETTER U
unicode_line_break(0x30A8, 0x30A8, 'ID'). % Lo       KATAKANA LETTER E
unicode_line_break(0x30AA, 0x30C2, 'ID'). % Lo  [25] KATAKANA LETTER O..KATAKANA LETTER DI
unicode_line_break(0x30C4, 0x30E2, 'ID'). % Lo  [31] KATAKANA LETTER TU..KATAKANA LETTER MO
unicode_line_break(0x30E4, 0x30E4, 'ID'). % Lo       KATAKANA LETTER YA
unicode_line_break(0x30E6, 0x30E6, 'ID'). % Lo       KATAKANA LETTER YU
unicode_line_break(0x30E8, 0x30ED, 'ID'). % Lo   [6] KATAKANA LETTER YO..KATAKANA LETTER RO
unicode_line_break(0x30EF, 0x30F4, 'ID'). % Lo   [6] KATAKANA LETTER WA..KATAKANA LETTER VU
unicode_line_break(0x30F7, 0x30FA, 'ID'). % Lo   [4] KATAKANA LETTER VA..KATAKANA LETTER VO
unicode_line_break(0x30FF, 0x30FF, 'ID'). % Lo       KATAKANA DIGRAPH KOTO
unicode_line_break(0x3105, 0x312D, 'ID'). % Lo  [41] BOPOMOFO LETTER B..BOPOMOFO LETTER IH
unicode_line_break(0x3131, 0x318E, 'ID'). % Lo  [94] HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE
unicode_line_break(0x3190, 0x3191, 'ID'). % So   [2] IDEOGRAPHIC ANNOTATION LINKING MARK..IDEOGRAPHIC ANNOTATION REVERSE MARK
unicode_line_break(0x3192, 0x3195, 'ID'). % No   [4] IDEOGRAPHIC ANNOTATION ONE MARK..IDEOGRAPHIC ANNOTATION FOUR MARK
unicode_line_break(0x3196, 0x319F, 'ID'). % So  [10] IDEOGRAPHIC ANNOTATION TOP MARK..IDEOGRAPHIC ANNOTATION MAN MARK
unicode_line_break(0x31A0, 0x31BA, 'ID'). % Lo  [27] BOPOMOFO LETTER BU..BOPOMOFO LETTER ZY
unicode_line_break(0x31C0, 0x31E3, 'ID'). % So  [36] CJK STROKE T..CJK STROKE Q
unicode_line_break(0x3200, 0x321E, 'ID'). % So  [31] PARENTHESIZED HANGUL KIYEOK..PARENTHESIZED KOREAN CHARACTER O HU
unicode_line_break(0x3220, 0x3229, 'ID'). % No  [10] PARENTHESIZED IDEOGRAPH ONE..PARENTHESIZED IDEOGRAPH TEN
unicode_line_break(0x322A, 0x3247, 'ID'). % So  [30] PARENTHESIZED IDEOGRAPH MOON..CIRCLED IDEOGRAPH KOTO
unicode_line_break(0x3250, 0x3250, 'ID'). % So       PARTNERSHIP SIGN
unicode_line_break(0x3251, 0x325F, 'ID'). % No  [15] CIRCLED NUMBER TWENTY ONE..CIRCLED NUMBER THIRTY FIVE
unicode_line_break(0x3260, 0x327F, 'ID'). % So  [32] CIRCLED HANGUL KIYEOK..KOREAN STANDARD SYMBOL
unicode_line_break(0x3280, 0x3289, 'ID'). % No  [10] CIRCLED IDEOGRAPH ONE..CIRCLED IDEOGRAPH TEN
unicode_line_break(0x328A, 0x32B0, 'ID'). % So  [39] CIRCLED IDEOGRAPH MOON..CIRCLED IDEOGRAPH NIGHT
unicode_line_break(0x32B1, 0x32BF, 'ID'). % No  [15] CIRCLED NUMBER THIRTY SIX..CIRCLED NUMBER FIFTY
unicode_line_break(0x32C0, 0x32FE, 'ID'). % So  [63] IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY..CIRCLED KATAKANA WO
unicode_line_break(0x3300, 0x33FF, 'ID'). % So [256] SQUARE APAATO..SQUARE GAL
unicode_line_break(0x3400, 0x4DB5, 'ID'). % Lo [6582] CJK UNIFIED IDEOGRAPH-3400..CJK UNIFIED IDEOGRAPH-4DB5
unicode_line_break(0x4DB6, 0x4DBF, 'ID'). % Cn  [10] <reserved-4DB6>..<reserved-4DBF>
unicode_line_break(0x4E00, 0x9FCC, 'ID'). % Lo [20941] CJK UNIFIED IDEOGRAPH-4E00..CJK UNIFIED IDEOGRAPH-9FCC
unicode_line_break(0x9FCD, 0x9FFF, 'ID'). % Cn  [51] <reserved-9FCD>..<reserved-9FFF>
unicode_line_break(0xA000, 0xA014, 'ID'). % Lo  [21] YI SYLLABLE IT..YI SYLLABLE E
unicode_line_break(0xA016, 0xA48C, 'ID'). % Lo [1143] YI SYLLABLE BIT..YI SYLLABLE YYR
unicode_line_break(0xA490, 0xA4C6, 'ID'). % So  [55] YI RADICAL QOT..YI RADICAL KE
unicode_line_break(0xF900, 0xFA6D, 'ID'). % Lo [366] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA6D
unicode_line_break(0xFA6E, 0xFA6F, 'ID'). % Cn   [2] <reserved-FA6E>..<reserved-FA6F>
unicode_line_break(0xFA70, 0xFAD9, 'ID'). % Lo [106] CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9
unicode_line_break(0xFADA, 0xFAFF, 'ID'). % Cn  [38] <reserved-FADA>..<reserved-FAFF>
unicode_line_break(0xFE30, 0xFE30, 'ID'). % Po       PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
unicode_line_break(0xFE31, 0xFE32, 'ID'). % Pd   [2] PRESENTATION FORM FOR VERTICAL EM DASH..PRESENTATION FORM FOR VERTICAL EN DASH
unicode_line_break(0xFE33, 0xFE34, 'ID'). % Pc   [2] PRESENTATION FORM FOR VERTICAL LOW LINE..PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
unicode_line_break(0xFE45, 0xFE46, 'ID'). % Po   [2] SESAME DOT..WHITE SESAME DOT
unicode_line_break(0xFE49, 0xFE4C, 'ID'). % Po   [4] DASHED OVERLINE..DOUBLE WAVY OVERLINE
unicode_line_break(0xFE4D, 0xFE4F, 'ID'). % Pc   [3] DASHED LOW LINE..WAVY LOW LINE
unicode_line_break(0xFE51, 0xFE51, 'ID'). % Po       SMALL IDEOGRAPHIC COMMA
unicode_line_break(0xFE58, 0xFE58, 'ID'). % Pd       SMALL EM DASH
unicode_line_break(0xFE5F, 0xFE61, 'ID'). % Po   [3] SMALL NUMBER SIGN..SMALL ASTERISK
unicode_line_break(0xFE62, 0xFE62, 'ID'). % Sm       SMALL PLUS SIGN
unicode_line_break(0xFE63, 0xFE63, 'ID'). % Pd       SMALL HYPHEN-MINUS
unicode_line_break(0xFE64, 0xFE66, 'ID'). % Sm   [3] SMALL LESS-THAN SIGN..SMALL EQUALS SIGN
unicode_line_break(0xFE68, 0xFE68, 'ID'). % Po       SMALL REVERSE SOLIDUS
unicode_line_break(0xFE6B, 0xFE6B, 'ID'). % Po       SMALL COMMERCIAL AT
unicode_line_break(0xFF02, 0xFF03, 'ID'). % Po   [2] FULLWIDTH QUOTATION MARK..FULLWIDTH NUMBER SIGN
unicode_line_break(0xFF06, 0xFF07, 'ID'). % Po   [2] FULLWIDTH AMPERSAND..FULLWIDTH APOSTROPHE
unicode_line_break(0xFF0A, 0xFF0A, 'ID'). % Po       FULLWIDTH ASTERISK
unicode_line_break(0xFF0B, 0xFF0B, 'ID'). % Sm       FULLWIDTH PLUS SIGN
unicode_line_break(0xFF0D, 0xFF0D, 'ID'). % Pd       FULLWIDTH HYPHEN-MINUS
unicode_line_break(0xFF0F, 0xFF0F, 'ID'). % Po       FULLWIDTH SOLIDUS
unicode_line_break(0xFF10, 0xFF19, 'ID'). % Nd  [10] FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
unicode_line_break(0xFF1C, 0xFF1E, 'ID'). % Sm   [3] FULLWIDTH LESS-THAN SIGN..FULLWIDTH GREATER-THAN SIGN
unicode_line_break(0xFF20, 0xFF20, 'ID'). % Po       FULLWIDTH COMMERCIAL AT
unicode_line_break(0xFF21, 0xFF3A, 'ID'). % L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_line_break(0xFF3C, 0xFF3C, 'ID'). % Po       FULLWIDTH REVERSE SOLIDUS
unicode_line_break(0xFF3E, 0xFF3E, 'ID'). % Sk       FULLWIDTH CIRCUMFLEX ACCENT
unicode_line_break(0xFF3F, 0xFF3F, 'ID'). % Pc       FULLWIDTH LOW LINE
unicode_line_break(0xFF40, 0xFF40, 'ID'). % Sk       FULLWIDTH GRAVE ACCENT
unicode_line_break(0xFF41, 0xFF5A, 'ID'). % L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_line_break(0xFF5C, 0xFF5C, 'ID'). % Sm       FULLWIDTH VERTICAL LINE
unicode_line_break(0xFF5E, 0xFF5E, 'ID'). % Sm       FULLWIDTH TILDE
unicode_line_break(0xFFE2, 0xFFE2, 'ID'). % Sm       FULLWIDTH NOT SIGN
unicode_line_break(0xFFE3, 0xFFE3, 'ID'). % Sk       FULLWIDTH MACRON
unicode_line_break(0xFFE4, 0xFFE4, 'ID'). % So       FULLWIDTH BROKEN BAR
unicode_line_break(0x1B000, 0x1B001, 'ID'). % Lo   [2] KATAKANA LETTER ARCHAIC E..HIRAGANA LETTER ARCHAIC YE
unicode_line_break(0x1F200, 0x1F202, 'ID'). % So   [3] SQUARE HIRAGANA HOKA..SQUARED KATAKANA SA
unicode_line_break(0x1F210, 0x1F23A, 'ID'). % So  [43] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-55B6
unicode_line_break(0x1F240, 0x1F248, 'ID'). % So   [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
unicode_line_break(0x1F250, 0x1F251, 'ID'). % So   [2] CIRCLED IDEOGRAPH ADVANTAGE..CIRCLED IDEOGRAPH ACCEPT
unicode_line_break(0x20000, 0x2A6D6, 'ID'). % Lo [42711] CJK UNIFIED IDEOGRAPH-20000..CJK UNIFIED IDEOGRAPH-2A6D6
unicode_line_break(0x2A6D7, 0x2A6FF, 'ID'). % Cn  [41] <reserved-2A6D7>..<reserved-2A6FF>
unicode_line_break(0x2A700, 0x2B734, 'ID'). % Lo [4149] CJK UNIFIED IDEOGRAPH-2A700..CJK UNIFIED IDEOGRAPH-2B734
unicode_line_break(0x2B735, 0x2B73F, 'ID'). % Cn  [11] <reserved-2B735>..<reserved-2B73F>
unicode_line_break(0x2B740, 0x2B81D, 'ID'). % Lo [222] CJK UNIFIED IDEOGRAPH-2B740..CJK UNIFIED IDEOGRAPH-2B81D
unicode_line_break(0x2B81E, 0x2F7FF, 'ID'). % Cn [16354] <reserved-2B81E>..<reserved-2F7FF>
unicode_line_break(0x2F800, 0x2FA1D, 'ID'). % Lo [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D
unicode_line_break(0x2FA1E, 0x2FFFD, 'ID'). % Cn [1504] <reserved-2FA1E>..<reserved-2FFFD>
unicode_line_break(0x30000, 0x3FFFD, 'ID'). % Cn [65534] <reserved-30000>..<reserved-3FFFD>

% Total code points: 161793

% ================================================

% Line_Break=Inseparable

unicode_line_break(0x2024, 0x2026, 'IN'). % Po   [3] ONE DOT LEADER..HORIZONTAL ELLIPSIS
unicode_line_break(0xFE19, 0xFE19, 'IN'). % Po       PRESENTATION FORM FOR VERTICAL HORIZONTAL ELLIPSIS

% Total code points: 4

% ================================================

% Line_Break=Hyphen

unicode_line_break(0x002D, 0x002D, 'HY'). % Pd       HYPHEN-MINUS

% Total code points: 1

% ================================================

% Line_Break=Combining_Mark

unicode_line_break(0x0000, 0x0008, 'CM'). % Cc   [9] <control-0000>..<control-0008>
unicode_line_break(0x000E, 0x001F, 'CM'). % Cc  [18] <control-000E>..<control-001F>
unicode_line_break(0x007F, 0x0084, 'CM'). % Cc   [6] <control-007F>..<control-0084>
unicode_line_break(0x0086, 0x009F, 'CM'). % Cc  [26] <control-0086>..<control-009F>
unicode_line_break(0x0300, 0x034E, 'CM'). % Mn  [79] COMBINING GRAVE ACCENT..COMBINING UPWARDS ARROW BELOW
unicode_line_break(0x0350, 0x035B, 'CM'). % Mn  [12] COMBINING RIGHT ARROWHEAD ABOVE..COMBINING ZIGZAG ABOVE
unicode_line_break(0x0363, 0x036F, 'CM'). % Mn  [13] COMBINING LATIN SMALL LETTER A..COMBINING LATIN SMALL LETTER X
unicode_line_break(0x0483, 0x0487, 'CM'). % Mn   [5] COMBINING CYRILLIC TITLO..COMBINING CYRILLIC POKRYTIE
unicode_line_break(0x0488, 0x0489, 'CM'). % Me   [2] COMBINING CYRILLIC HUNDRED THOUSANDS SIGN..COMBINING CYRILLIC MILLIONS SIGN
unicode_line_break(0x0591, 0x05BD, 'CM'). % Mn  [45] HEBREW ACCENT ETNAHTA..HEBREW POINT METEG
unicode_line_break(0x05BF, 0x05BF, 'CM'). % Mn       HEBREW POINT RAFE
unicode_line_break(0x05C1, 0x05C2, 'CM'). % Mn   [2] HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT
unicode_line_break(0x05C4, 0x05C5, 'CM'). % Mn   [2] HEBREW MARK UPPER DOT..HEBREW MARK LOWER DOT
unicode_line_break(0x05C7, 0x05C7, 'CM'). % Mn       HEBREW POINT QAMATS QATAN
unicode_line_break(0x0610, 0x061A, 'CM'). % Mn  [11] ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM..ARABIC SMALL KASRA
unicode_line_break(0x064B, 0x065F, 'CM'). % Mn  [21] ARABIC FATHATAN..ARABIC WAVY HAMZA BELOW
unicode_line_break(0x0670, 0x0670, 'CM'). % Mn       ARABIC LETTER SUPERSCRIPT ALEF
unicode_line_break(0x06D6, 0x06DC, 'CM'). % Mn   [7] ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN
unicode_line_break(0x06DF, 0x06E4, 'CM'). % Mn   [6] ARABIC SMALL HIGH ROUNDED ZERO..ARABIC SMALL HIGH MADDA
unicode_line_break(0x06E7, 0x06E8, 'CM'). % Mn   [2] ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON
unicode_line_break(0x06EA, 0x06ED, 'CM'). % Mn   [4] ARABIC EMPTY CENTRE LOW STOP..ARABIC SMALL LOW MEEM
unicode_line_break(0x0711, 0x0711, 'CM'). % Mn       SYRIAC LETTER SUPERSCRIPT ALAPH
unicode_line_break(0x0730, 0x074A, 'CM'). % Mn  [27] SYRIAC PTHAHA ABOVE..SYRIAC BARREKH
unicode_line_break(0x07A6, 0x07B0, 'CM'). % Mn  [11] THAANA ABAFILI..THAANA SUKUN
unicode_line_break(0x07EB, 0x07F3, 'CM'). % Mn   [9] NKO COMBINING SHORT HIGH TONE..NKO COMBINING DOUBLE DOT ABOVE
unicode_line_break(0x0816, 0x0819, 'CM'). % Mn   [4] SAMARITAN MARK IN..SAMARITAN MARK DAGESH
unicode_line_break(0x081B, 0x0823, 'CM'). % Mn   [9] SAMARITAN MARK EPENTHETIC YUT..SAMARITAN VOWEL SIGN A
unicode_line_break(0x0825, 0x0827, 'CM'). % Mn   [3] SAMARITAN VOWEL SIGN SHORT A..SAMARITAN VOWEL SIGN U
unicode_line_break(0x0829, 0x082D, 'CM'). % Mn   [5] SAMARITAN VOWEL SIGN LONG I..SAMARITAN MARK NEQUDAA
unicode_line_break(0x0859, 0x085B, 'CM'). % Mn   [3] MANDAIC AFFRICATION MARK..MANDAIC GEMINATION MARK
unicode_line_break(0x08E4, 0x08FE, 'CM'). % Mn  [27] ARABIC CURLY FATHA..ARABIC DAMMA WITH DOT
unicode_line_break(0x0900, 0x0902, 'CM'). % Mn   [3] DEVANAGARI SIGN INVERTED CANDRABINDU..DEVANAGARI SIGN ANUSVARA
unicode_line_break(0x0903, 0x0903, 'CM'). % Mc       DEVANAGARI SIGN VISARGA
unicode_line_break(0x093A, 0x093A, 'CM'). % Mn       DEVANAGARI VOWEL SIGN OE
unicode_line_break(0x093B, 0x093B, 'CM'). % Mc       DEVANAGARI VOWEL SIGN OOE
unicode_line_break(0x093C, 0x093C, 'CM'). % Mn       DEVANAGARI SIGN NUKTA
unicode_line_break(0x093E, 0x0940, 'CM'). % Mc   [3] DEVANAGARI VOWEL SIGN AA..DEVANAGARI VOWEL SIGN II
unicode_line_break(0x0941, 0x0948, 'CM'). % Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
unicode_line_break(0x0949, 0x094C, 'CM'). % Mc   [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU
unicode_line_break(0x094D, 0x094D, 'CM'). % Mn       DEVANAGARI SIGN VIRAMA
unicode_line_break(0x094E, 0x094F, 'CM'). % Mc   [2] DEVANAGARI VOWEL SIGN PRISHTHAMATRA E..DEVANAGARI VOWEL SIGN AW
unicode_line_break(0x0951, 0x0957, 'CM'). % Mn   [7] DEVANAGARI STRESS SIGN UDATTA..DEVANAGARI VOWEL SIGN UUE
unicode_line_break(0x0962, 0x0963, 'CM'). % Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
unicode_line_break(0x0981, 0x0981, 'CM'). % Mn       BENGALI SIGN CANDRABINDU
unicode_line_break(0x0982, 0x0983, 'CM'). % Mc   [2] BENGALI SIGN ANUSVARA..BENGALI SIGN VISARGA
unicode_line_break(0x09BC, 0x09BC, 'CM'). % Mn       BENGALI SIGN NUKTA
unicode_line_break(0x09BE, 0x09C0, 'CM'). % Mc   [3] BENGALI VOWEL SIGN AA..BENGALI VOWEL SIGN II
unicode_line_break(0x09C1, 0x09C4, 'CM'). % Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
unicode_line_break(0x09C7, 0x09C8, 'CM'). % Mc   [2] BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI
unicode_line_break(0x09CB, 0x09CC, 'CM'). % Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_line_break(0x09CD, 0x09CD, 'CM'). % Mn       BENGALI SIGN VIRAMA
unicode_line_break(0x09D7, 0x09D7, 'CM'). % Mc       BENGALI AU LENGTH MARK
unicode_line_break(0x09E2, 0x09E3, 'CM'). % Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
unicode_line_break(0x0A01, 0x0A02, 'CM'). % Mn   [2] GURMUKHI SIGN ADAK BINDI..GURMUKHI SIGN BINDI
unicode_line_break(0x0A03, 0x0A03, 'CM'). % Mc       GURMUKHI SIGN VISARGA
unicode_line_break(0x0A3C, 0x0A3C, 'CM'). % Mn       GURMUKHI SIGN NUKTA
unicode_line_break(0x0A3E, 0x0A40, 'CM'). % Mc   [3] GURMUKHI VOWEL SIGN AA..GURMUKHI VOWEL SIGN II
unicode_line_break(0x0A41, 0x0A42, 'CM'). % Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
unicode_line_break(0x0A47, 0x0A48, 'CM'). % Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
unicode_line_break(0x0A4B, 0x0A4D, 'CM'). % Mn   [3] GURMUKHI VOWEL SIGN OO..GURMUKHI SIGN VIRAMA
unicode_line_break(0x0A51, 0x0A51, 'CM'). % Mn       GURMUKHI SIGN UDAAT
unicode_line_break(0x0A70, 0x0A71, 'CM'). % Mn   [2] GURMUKHI TIPPI..GURMUKHI ADDAK
unicode_line_break(0x0A75, 0x0A75, 'CM'). % Mn       GURMUKHI SIGN YAKASH
unicode_line_break(0x0A81, 0x0A82, 'CM'). % Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
unicode_line_break(0x0A83, 0x0A83, 'CM'). % Mc       GUJARATI SIGN VISARGA
unicode_line_break(0x0ABC, 0x0ABC, 'CM'). % Mn       GUJARATI SIGN NUKTA
unicode_line_break(0x0ABE, 0x0AC0, 'CM'). % Mc   [3] GUJARATI VOWEL SIGN AA..GUJARATI VOWEL SIGN II
unicode_line_break(0x0AC1, 0x0AC5, 'CM'). % Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
unicode_line_break(0x0AC7, 0x0AC8, 'CM'). % Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
unicode_line_break(0x0AC9, 0x0AC9, 'CM'). % Mc       GUJARATI VOWEL SIGN CANDRA O
unicode_line_break(0x0ACB, 0x0ACC, 'CM'). % Mc   [2] GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU
unicode_line_break(0x0ACD, 0x0ACD, 'CM'). % Mn       GUJARATI SIGN VIRAMA
unicode_line_break(0x0AE2, 0x0AE3, 'CM'). % Mn   [2] GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL
unicode_line_break(0x0B01, 0x0B01, 'CM'). % Mn       ORIYA SIGN CANDRABINDU
unicode_line_break(0x0B02, 0x0B03, 'CM'). % Mc   [2] ORIYA SIGN ANUSVARA..ORIYA SIGN VISARGA
unicode_line_break(0x0B3C, 0x0B3C, 'CM'). % Mn       ORIYA SIGN NUKTA
unicode_line_break(0x0B3E, 0x0B3E, 'CM'). % Mc       ORIYA VOWEL SIGN AA
unicode_line_break(0x0B3F, 0x0B3F, 'CM'). % Mn       ORIYA VOWEL SIGN I
unicode_line_break(0x0B40, 0x0B40, 'CM'). % Mc       ORIYA VOWEL SIGN II
unicode_line_break(0x0B41, 0x0B44, 'CM'). % Mn   [4] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR
unicode_line_break(0x0B47, 0x0B48, 'CM'). % Mc   [2] ORIYA VOWEL SIGN E..ORIYA VOWEL SIGN AI
unicode_line_break(0x0B4B, 0x0B4C, 'CM'). % Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
unicode_line_break(0x0B4D, 0x0B4D, 'CM'). % Mn       ORIYA SIGN VIRAMA
unicode_line_break(0x0B56, 0x0B56, 'CM'). % Mn       ORIYA AI LENGTH MARK
unicode_line_break(0x0B57, 0x0B57, 'CM'). % Mc       ORIYA AU LENGTH MARK
unicode_line_break(0x0B62, 0x0B63, 'CM'). % Mn   [2] ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL
unicode_line_break(0x0B82, 0x0B82, 'CM'). % Mn       TAMIL SIGN ANUSVARA
unicode_line_break(0x0BBE, 0x0BBF, 'CM'). % Mc   [2] TAMIL VOWEL SIGN AA..TAMIL VOWEL SIGN I
unicode_line_break(0x0BC0, 0x0BC0, 'CM'). % Mn       TAMIL VOWEL SIGN II
unicode_line_break(0x0BC1, 0x0BC2, 'CM'). % Mc   [2] TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU
unicode_line_break(0x0BC6, 0x0BC8, 'CM'). % Mc   [3] TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI
unicode_line_break(0x0BCA, 0x0BCC, 'CM'). % Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_line_break(0x0BCD, 0x0BCD, 'CM'). % Mn       TAMIL SIGN VIRAMA
unicode_line_break(0x0BD7, 0x0BD7, 'CM'). % Mc       TAMIL AU LENGTH MARK
unicode_line_break(0x0C01, 0x0C03, 'CM'). % Mc   [3] TELUGU SIGN CANDRABINDU..TELUGU SIGN VISARGA
unicode_line_break(0x0C3E, 0x0C40, 'CM'). % Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
unicode_line_break(0x0C41, 0x0C44, 'CM'). % Mc   [4] TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR
unicode_line_break(0x0C46, 0x0C48, 'CM'). % Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
unicode_line_break(0x0C4A, 0x0C4D, 'CM'). % Mn   [4] TELUGU VOWEL SIGN O..TELUGU SIGN VIRAMA
unicode_line_break(0x0C55, 0x0C56, 'CM'). % Mn   [2] TELUGU LENGTH MARK..TELUGU AI LENGTH MARK
unicode_line_break(0x0C62, 0x0C63, 'CM'). % Mn   [2] TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL
unicode_line_break(0x0C82, 0x0C83, 'CM'). % Mc   [2] KANNADA SIGN ANUSVARA..KANNADA SIGN VISARGA
unicode_line_break(0x0CBC, 0x0CBC, 'CM'). % Mn       KANNADA SIGN NUKTA
unicode_line_break(0x0CBE, 0x0CBE, 'CM'). % Mc       KANNADA VOWEL SIGN AA
unicode_line_break(0x0CBF, 0x0CBF, 'CM'). % Mn       KANNADA VOWEL SIGN I
unicode_line_break(0x0CC0, 0x0CC4, 'CM'). % Mc   [5] KANNADA VOWEL SIGN II..KANNADA VOWEL SIGN VOCALIC RR
unicode_line_break(0x0CC6, 0x0CC6, 'CM'). % Mn       KANNADA VOWEL SIGN E
unicode_line_break(0x0CC7, 0x0CC8, 'CM'). % Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_line_break(0x0CCA, 0x0CCB, 'CM'). % Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_line_break(0x0CCC, 0x0CCD, 'CM'). % Mn   [2] KANNADA VOWEL SIGN AU..KANNADA SIGN VIRAMA
unicode_line_break(0x0CD5, 0x0CD6, 'CM'). % Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
unicode_line_break(0x0CE2, 0x0CE3, 'CM'). % Mn   [2] KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL
unicode_line_break(0x0D02, 0x0D03, 'CM'). % Mc   [2] MALAYALAM SIGN ANUSVARA..MALAYALAM SIGN VISARGA
unicode_line_break(0x0D3E, 0x0D40, 'CM'). % Mc   [3] MALAYALAM VOWEL SIGN AA..MALAYALAM VOWEL SIGN II
unicode_line_break(0x0D41, 0x0D44, 'CM'). % Mn   [4] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC RR
unicode_line_break(0x0D46, 0x0D48, 'CM'). % Mc   [3] MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI
unicode_line_break(0x0D4A, 0x0D4C, 'CM'). % Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_line_break(0x0D4D, 0x0D4D, 'CM'). % Mn       MALAYALAM SIGN VIRAMA
unicode_line_break(0x0D57, 0x0D57, 'CM'). % Mc       MALAYALAM AU LENGTH MARK
unicode_line_break(0x0D62, 0x0D63, 'CM'). % Mn   [2] MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL
unicode_line_break(0x0D82, 0x0D83, 'CM'). % Mc   [2] SINHALA SIGN ANUSVARAYA..SINHALA SIGN VISARGAYA
unicode_line_break(0x0DCA, 0x0DCA, 'CM'). % Mn       SINHALA SIGN AL-LAKUNA
unicode_line_break(0x0DCF, 0x0DD1, 'CM'). % Mc   [3] SINHALA VOWEL SIGN AELA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA
unicode_line_break(0x0DD2, 0x0DD4, 'CM'). % Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
unicode_line_break(0x0DD6, 0x0DD6, 'CM'). % Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
unicode_line_break(0x0DD8, 0x0DDF, 'CM'). % Mc   [8] SINHALA VOWEL SIGN GAETTA-PILLA..SINHALA VOWEL SIGN GAYANUKITTA
unicode_line_break(0x0DF2, 0x0DF3, 'CM'). % Mc   [2] SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA
unicode_line_break(0x0F18, 0x0F19, 'CM'). % Mn   [2] TIBETAN ASTROLOGICAL SIGN -KHYUD PA..TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS
unicode_line_break(0x0F35, 0x0F35, 'CM'). % Mn       TIBETAN MARK NGAS BZUNG NYI ZLA
unicode_line_break(0x0F37, 0x0F37, 'CM'). % Mn       TIBETAN MARK NGAS BZUNG SGOR RTAGS
unicode_line_break(0x0F39, 0x0F39, 'CM'). % Mn       TIBETAN MARK TSA -PHRU
unicode_line_break(0x0F3E, 0x0F3F, 'CM'). % Mc   [2] TIBETAN SIGN YAR TSHES..TIBETAN SIGN MAR TSHES
unicode_line_break(0x0F71, 0x0F7E, 'CM'). % Mn  [14] TIBETAN VOWEL SIGN AA..TIBETAN SIGN RJES SU NGA RO
unicode_line_break(0x0F80, 0x0F84, 'CM'). % Mn   [5] TIBETAN VOWEL SIGN REVERSED I..TIBETAN MARK HALANTA
unicode_line_break(0x0F86, 0x0F87, 'CM'). % Mn   [2] TIBETAN SIGN LCI RTAGS..TIBETAN SIGN YANG RTAGS
unicode_line_break(0x0F8D, 0x0F97, 'CM'). % Mn  [11] TIBETAN SUBJOINED SIGN LCE TSA CAN..TIBETAN SUBJOINED LETTER JA
unicode_line_break(0x0F99, 0x0FBC, 'CM'). % Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
unicode_line_break(0x0FC6, 0x0FC6, 'CM'). % Mn       TIBETAN SYMBOL PADMA GDAN
unicode_line_break(0x135D, 0x135F, 'CM'). % Mn   [3] ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK..ETHIOPIC COMBINING GEMINATION MARK
unicode_line_break(0x1712, 0x1714, 'CM'). % Mn   [3] TAGALOG VOWEL SIGN I..TAGALOG SIGN VIRAMA
unicode_line_break(0x1732, 0x1734, 'CM'). % Mn   [3] HANUNOO VOWEL SIGN I..HANUNOO SIGN PAMUDPOD
unicode_line_break(0x1752, 0x1753, 'CM'). % Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
unicode_line_break(0x1772, 0x1773, 'CM'). % Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
unicode_line_break(0x180B, 0x180D, 'CM'). % Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
unicode_line_break(0x18A9, 0x18A9, 'CM'). % Mn       MONGOLIAN LETTER ALI GALI DAGALGA
unicode_line_break(0x1920, 0x1922, 'CM'). % Mn   [3] LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN U
unicode_line_break(0x1923, 0x1926, 'CM'). % Mc   [4] LIMBU VOWEL SIGN EE..LIMBU VOWEL SIGN AU
unicode_line_break(0x1927, 0x1928, 'CM'). % Mn   [2] LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O
unicode_line_break(0x1929, 0x192B, 'CM'). % Mc   [3] LIMBU SUBJOINED LETTER YA..LIMBU SUBJOINED LETTER WA
unicode_line_break(0x1930, 0x1931, 'CM'). % Mc   [2] LIMBU SMALL LETTER KA..LIMBU SMALL LETTER NGA
unicode_line_break(0x1932, 0x1932, 'CM'). % Mn       LIMBU SMALL LETTER ANUSVARA
unicode_line_break(0x1933, 0x1938, 'CM'). % Mc   [6] LIMBU SMALL LETTER TA..LIMBU SMALL LETTER LA
unicode_line_break(0x1939, 0x193B, 'CM'). % Mn   [3] LIMBU SIGN MUKPHRENG..LIMBU SIGN SA-I
unicode_line_break(0x1A17, 0x1A18, 'CM'). % Mn   [2] BUGINESE VOWEL SIGN I..BUGINESE VOWEL SIGN U
unicode_line_break(0x1A19, 0x1A1B, 'CM'). % Mc   [3] BUGINESE VOWEL SIGN E..BUGINESE VOWEL SIGN AE
unicode_line_break(0x1A7F, 0x1A7F, 'CM'). % Mn       TAI THAM COMBINING CRYPTOGRAMMIC DOT
unicode_line_break(0x1B00, 0x1B03, 'CM'). % Mn   [4] BALINESE SIGN ULU RICEM..BALINESE SIGN SURANG
unicode_line_break(0x1B04, 0x1B04, 'CM'). % Mc       BALINESE SIGN BISAH
unicode_line_break(0x1B34, 0x1B34, 'CM'). % Mn       BALINESE SIGN REREKAN
unicode_line_break(0x1B35, 0x1B35, 'CM'). % Mc       BALINESE VOWEL SIGN TEDUNG
unicode_line_break(0x1B36, 0x1B3A, 'CM'). % Mn   [5] BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN RA REPA
unicode_line_break(0x1B3B, 0x1B3B, 'CM'). % Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_line_break(0x1B3C, 0x1B3C, 'CM'). % Mn       BALINESE VOWEL SIGN LA LENGA
unicode_line_break(0x1B3D, 0x1B41, 'CM'). % Mc   [5] BALINESE VOWEL SIGN LA LENGA TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG
unicode_line_break(0x1B42, 0x1B42, 'CM'). % Mn       BALINESE VOWEL SIGN PEPET
unicode_line_break(0x1B43, 0x1B44, 'CM'). % Mc   [2] BALINESE VOWEL SIGN PEPET TEDUNG..BALINESE ADEG ADEG
unicode_line_break(0x1B6B, 0x1B73, 'CM'). % Mn   [9] BALINESE MUSICAL SYMBOL COMBINING TEGEH..BALINESE MUSICAL SYMBOL COMBINING GONG
unicode_line_break(0x1B80, 0x1B81, 'CM'). % Mn   [2] SUNDANESE SIGN PANYECEK..SUNDANESE SIGN PANGLAYAR
unicode_line_break(0x1B82, 0x1B82, 'CM'). % Mc       SUNDANESE SIGN PANGWISAD
unicode_line_break(0x1BA1, 0x1BA1, 'CM'). % Mc       SUNDANESE CONSONANT SIGN PAMINGKAL
unicode_line_break(0x1BA2, 0x1BA5, 'CM'). % Mn   [4] SUNDANESE CONSONANT SIGN PANYAKRA..SUNDANESE VOWEL SIGN PANYUKU
unicode_line_break(0x1BA6, 0x1BA7, 'CM'). % Mc   [2] SUNDANESE VOWEL SIGN PANAELAENG..SUNDANESE VOWEL SIGN PANOLONG
unicode_line_break(0x1BA8, 0x1BA9, 'CM'). % Mn   [2] SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG
unicode_line_break(0x1BAA, 0x1BAA, 'CM'). % Mc       SUNDANESE SIGN PAMAAEH
unicode_line_break(0x1BAB, 0x1BAB, 'CM'). % Mn       SUNDANESE SIGN VIRAMA
unicode_line_break(0x1BAC, 0x1BAD, 'CM'). % Mc   [2] SUNDANESE CONSONANT SIGN PASANGAN MA..SUNDANESE CONSONANT SIGN PASANGAN WA
unicode_line_break(0x1BE6, 0x1BE6, 'CM'). % Mn       BATAK SIGN TOMPI
unicode_line_break(0x1BE7, 0x1BE7, 'CM'). % Mc       BATAK VOWEL SIGN E
unicode_line_break(0x1BE8, 0x1BE9, 'CM'). % Mn   [2] BATAK VOWEL SIGN PAKPAK E..BATAK VOWEL SIGN EE
unicode_line_break(0x1BEA, 0x1BEC, 'CM'). % Mc   [3] BATAK VOWEL SIGN I..BATAK VOWEL SIGN O
unicode_line_break(0x1BED, 0x1BED, 'CM'). % Mn       BATAK VOWEL SIGN KARO O
unicode_line_break(0x1BEE, 0x1BEE, 'CM'). % Mc       BATAK VOWEL SIGN U
unicode_line_break(0x1BEF, 0x1BF1, 'CM'). % Mn   [3] BATAK VOWEL SIGN U FOR SIMALUNGUN SA..BATAK CONSONANT SIGN H
unicode_line_break(0x1BF2, 0x1BF3, 'CM'). % Mc   [2] BATAK PANGOLAT..BATAK PANONGONAN
unicode_line_break(0x1C24, 0x1C2B, 'CM'). % Mc   [8] LEPCHA SUBJOINED LETTER YA..LEPCHA VOWEL SIGN UU
unicode_line_break(0x1C2C, 0x1C33, 'CM'). % Mn   [8] LEPCHA VOWEL SIGN E..LEPCHA CONSONANT SIGN T
unicode_line_break(0x1C34, 0x1C35, 'CM'). % Mc   [2] LEPCHA CONSONANT SIGN NYIN-DO..LEPCHA CONSONANT SIGN KANG
unicode_line_break(0x1C36, 0x1C37, 'CM'). % Mn   [2] LEPCHA SIGN RAN..LEPCHA SIGN NUKTA
unicode_line_break(0x1CD0, 0x1CD2, 'CM'). % Mn   [3] VEDIC TONE KARSHANA..VEDIC TONE PRENKHA
unicode_line_break(0x1CD4, 0x1CE0, 'CM'). % Mn  [13] VEDIC SIGN YAJURVEDIC MIDLINE SVARITA..VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA
unicode_line_break(0x1CE1, 0x1CE1, 'CM'). % Mc       VEDIC TONE ATHARVAVEDIC INDEPENDENT SVARITA
unicode_line_break(0x1CE2, 0x1CE8, 'CM'). % Mn   [7] VEDIC SIGN VISARGA SVARITA..VEDIC SIGN VISARGA ANUDATTA WITH TAIL
unicode_line_break(0x1CED, 0x1CED, 'CM'). % Mn       VEDIC SIGN TIRYAK
unicode_line_break(0x1CF2, 0x1CF3, 'CM'). % Mc   [2] VEDIC SIGN ARDHAVISARGA..VEDIC SIGN ROTATED ARDHAVISARGA
unicode_line_break(0x1CF4, 0x1CF4, 'CM'). % Mn       VEDIC TONE CANDRA ABOVE
unicode_line_break(0x1DC0, 0x1DE6, 'CM'). % Mn  [39] COMBINING DOTTED GRAVE ACCENT..COMBINING LATIN SMALL LETTER Z
unicode_line_break(0x1DFC, 0x1DFF, 'CM'). % Mn   [4] COMBINING DOUBLE INVERTED BREVE BELOW..COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW
unicode_line_break(0x200C, 0x200F, 'CM'). % Cf   [4] ZERO WIDTH NON-JOINER..RIGHT-TO-LEFT MARK
unicode_line_break(0x202A, 0x202E, 'CM'). % Cf   [5] LEFT-TO-RIGHT EMBEDDING..RIGHT-TO-LEFT OVERRIDE
unicode_line_break(0x206A, 0x206F, 'CM'). % Cf   [6] INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES
unicode_line_break(0x20D0, 0x20DC, 'CM'). % Mn  [13] COMBINING LEFT HARPOON ABOVE..COMBINING FOUR DOTS ABOVE
unicode_line_break(0x20DD, 0x20E0, 'CM'). % Me   [4] COMBINING ENCLOSING CIRCLE..COMBINING ENCLOSING CIRCLE BACKSLASH
unicode_line_break(0x20E1, 0x20E1, 'CM'). % Mn       COMBINING LEFT RIGHT ARROW ABOVE
unicode_line_break(0x20E2, 0x20E4, 'CM'). % Me   [3] COMBINING ENCLOSING SCREEN..COMBINING ENCLOSING UPWARD POINTING TRIANGLE
unicode_line_break(0x20E5, 0x20F0, 'CM'). % Mn  [12] COMBINING REVERSE SOLIDUS OVERLAY..COMBINING ASTERISK ABOVE
unicode_line_break(0x2CEF, 0x2CF1, 'CM'). % Mn   [3] COPTIC COMBINING NI ABOVE..COPTIC COMBINING SPIRITUS LENIS
unicode_line_break(0x2D7F, 0x2D7F, 'CM'). % Mn       TIFINAGH CONSONANT JOINER
unicode_line_break(0x2DE0, 0x2DFF, 'CM'). % Mn  [32] COMBINING CYRILLIC LETTER BE..COMBINING CYRILLIC LETTER IOTIFIED BIG YUS
unicode_line_break(0x302A, 0x302D, 'CM'). % Mn   [4] IDEOGRAPHIC LEVEL TONE MARK..IDEOGRAPHIC ENTERING TONE MARK
unicode_line_break(0x302E, 0x302F, 'CM'). % Mc   [2] HANGUL SINGLE DOT TONE MARK..HANGUL DOUBLE DOT TONE MARK
unicode_line_break(0x3099, 0x309A, 'CM'). % Mn   [2] COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK..COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_line_break(0xA66F, 0xA66F, 'CM'). % Mn       COMBINING CYRILLIC VZMET
unicode_line_break(0xA670, 0xA672, 'CM'). % Me   [3] COMBINING CYRILLIC TEN MILLIONS SIGN..COMBINING CYRILLIC THOUSAND MILLIONS SIGN
unicode_line_break(0xA674, 0xA67D, 'CM'). % Mn  [10] COMBINING CYRILLIC LETTER UKRAINIAN IE..COMBINING CYRILLIC PAYEROK
unicode_line_break(0xA69F, 0xA69F, 'CM'). % Mn       COMBINING CYRILLIC LETTER IOTIFIED E
unicode_line_break(0xA6F0, 0xA6F1, 'CM'). % Mn   [2] BAMUM COMBINING MARK KOQNDON..BAMUM COMBINING MARK TUKWENTIS
unicode_line_break(0xA802, 0xA802, 'CM'). % Mn       SYLOTI NAGRI SIGN DVISVARA
unicode_line_break(0xA806, 0xA806, 'CM'). % Mn       SYLOTI NAGRI SIGN HASANTA
unicode_line_break(0xA80B, 0xA80B, 'CM'). % Mn       SYLOTI NAGRI SIGN ANUSVARA
unicode_line_break(0xA823, 0xA824, 'CM'). % Mc   [2] SYLOTI NAGRI VOWEL SIGN A..SYLOTI NAGRI VOWEL SIGN I
unicode_line_break(0xA825, 0xA826, 'CM'). % Mn   [2] SYLOTI NAGRI VOWEL SIGN U..SYLOTI NAGRI VOWEL SIGN E
unicode_line_break(0xA827, 0xA827, 'CM'). % Mc       SYLOTI NAGRI VOWEL SIGN OO
unicode_line_break(0xA880, 0xA881, 'CM'). % Mc   [2] SAURASHTRA SIGN ANUSVARA..SAURASHTRA SIGN VISARGA
unicode_line_break(0xA8B4, 0xA8C3, 'CM'). % Mc  [16] SAURASHTRA CONSONANT SIGN HAARU..SAURASHTRA VOWEL SIGN AU
unicode_line_break(0xA8C4, 0xA8C4, 'CM'). % Mn       SAURASHTRA SIGN VIRAMA
unicode_line_break(0xA8E0, 0xA8F1, 'CM'). % Mn  [18] COMBINING DEVANAGARI DIGIT ZERO..COMBINING DEVANAGARI SIGN AVAGRAHA
unicode_line_break(0xA926, 0xA92D, 'CM'). % Mn   [8] KAYAH LI VOWEL UE..KAYAH LI TONE CALYA PLOPHU
unicode_line_break(0xA947, 0xA951, 'CM'). % Mn  [11] REJANG VOWEL SIGN I..REJANG CONSONANT SIGN R
unicode_line_break(0xA952, 0xA953, 'CM'). % Mc   [2] REJANG CONSONANT SIGN H..REJANG VIRAMA
unicode_line_break(0xA980, 0xA982, 'CM'). % Mn   [3] JAVANESE SIGN PANYANGGA..JAVANESE SIGN LAYAR
unicode_line_break(0xA983, 0xA983, 'CM'). % Mc       JAVANESE SIGN WIGNYAN
unicode_line_break(0xA9B3, 0xA9B3, 'CM'). % Mn       JAVANESE SIGN CECAK TELU
unicode_line_break(0xA9B4, 0xA9B5, 'CM'). % Mc   [2] JAVANESE VOWEL SIGN TARUNG..JAVANESE VOWEL SIGN TOLONG
unicode_line_break(0xA9B6, 0xA9B9, 'CM'). % Mn   [4] JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN SUKU MENDUT
unicode_line_break(0xA9BA, 0xA9BB, 'CM'). % Mc   [2] JAVANESE VOWEL SIGN TALING..JAVANESE VOWEL SIGN DIRGA MURE
unicode_line_break(0xA9BC, 0xA9BC, 'CM'). % Mn       JAVANESE VOWEL SIGN PEPET
unicode_line_break(0xA9BD, 0xA9C0, 'CM'). % Mc   [4] JAVANESE CONSONANT SIGN KERET..JAVANESE PANGKON
unicode_line_break(0xAA29, 0xAA2E, 'CM'). % Mn   [6] CHAM VOWEL SIGN AA..CHAM VOWEL SIGN OE
unicode_line_break(0xAA2F, 0xAA30, 'CM'). % Mc   [2] CHAM VOWEL SIGN O..CHAM VOWEL SIGN AI
unicode_line_break(0xAA31, 0xAA32, 'CM'). % Mn   [2] CHAM VOWEL SIGN AU..CHAM VOWEL SIGN UE
unicode_line_break(0xAA33, 0xAA34, 'CM'). % Mc   [2] CHAM CONSONANT SIGN YA..CHAM CONSONANT SIGN RA
unicode_line_break(0xAA35, 0xAA36, 'CM'). % Mn   [2] CHAM CONSONANT SIGN LA..CHAM CONSONANT SIGN WA
unicode_line_break(0xAA43, 0xAA43, 'CM'). % Mn       CHAM CONSONANT SIGN FINAL NG
unicode_line_break(0xAA4C, 0xAA4C, 'CM'). % Mn       CHAM CONSONANT SIGN FINAL M
unicode_line_break(0xAA4D, 0xAA4D, 'CM'). % Mc       CHAM CONSONANT SIGN FINAL H
unicode_line_break(0xAAEB, 0xAAEB, 'CM'). % Mc       MEETEI MAYEK VOWEL SIGN II
unicode_line_break(0xAAEC, 0xAAED, 'CM'). % Mn   [2] MEETEI MAYEK VOWEL SIGN UU..MEETEI MAYEK VOWEL SIGN AAI
unicode_line_break(0xAAEE, 0xAAEF, 'CM'). % Mc   [2] MEETEI MAYEK VOWEL SIGN AU..MEETEI MAYEK VOWEL SIGN AAU
unicode_line_break(0xAAF5, 0xAAF5, 'CM'). % Mc       MEETEI MAYEK VOWEL SIGN VISARGA
unicode_line_break(0xAAF6, 0xAAF6, 'CM'). % Mn       MEETEI MAYEK VIRAMA
unicode_line_break(0xABE3, 0xABE4, 'CM'). % Mc   [2] MEETEI MAYEK VOWEL SIGN ONAP..MEETEI MAYEK VOWEL SIGN INAP
unicode_line_break(0xABE5, 0xABE5, 'CM'). % Mn       MEETEI MAYEK VOWEL SIGN ANAP
unicode_line_break(0xABE6, 0xABE7, 'CM'). % Mc   [2] MEETEI MAYEK VOWEL SIGN YENAP..MEETEI MAYEK VOWEL SIGN SOUNAP
unicode_line_break(0xABE8, 0xABE8, 'CM'). % Mn       MEETEI MAYEK VOWEL SIGN UNAP
unicode_line_break(0xABE9, 0xABEA, 'CM'). % Mc   [2] MEETEI MAYEK VOWEL SIGN CHEINAP..MEETEI MAYEK VOWEL SIGN NUNG
unicode_line_break(0xABEC, 0xABEC, 'CM'). % Mc       MEETEI MAYEK LUM IYEK
unicode_line_break(0xABED, 0xABED, 'CM'). % Mn       MEETEI MAYEK APUN IYEK
unicode_line_break(0xFB1E, 0xFB1E, 'CM'). % Mn       HEBREW POINT JUDEO-SPANISH VARIKA
unicode_line_break(0xFE00, 0xFE0F, 'CM'). % Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
unicode_line_break(0xFE20, 0xFE26, 'CM'). % Mn   [7] COMBINING LIGATURE LEFT HALF..COMBINING CONJOINING MACRON
unicode_line_break(0xFFF9, 0xFFFB, 'CM'). % Cf   [3] INTERLINEAR ANNOTATION ANCHOR..INTERLINEAR ANNOTATION TERMINATOR
unicode_line_break(0x101FD, 0x101FD, 'CM'). % Mn       PHAISTOS DISC SIGN COMBINING OBLIQUE STROKE
unicode_line_break(0x10A01, 0x10A03, 'CM'). % Mn   [3] KHAROSHTHI VOWEL SIGN I..KHAROSHTHI VOWEL SIGN VOCALIC R
unicode_line_break(0x10A05, 0x10A06, 'CM'). % Mn   [2] KHAROSHTHI VOWEL SIGN E..KHAROSHTHI VOWEL SIGN O
unicode_line_break(0x10A0C, 0x10A0F, 'CM'). % Mn   [4] KHAROSHTHI VOWEL LENGTH MARK..KHAROSHTHI SIGN VISARGA
unicode_line_break(0x10A38, 0x10A3A, 'CM'). % Mn   [3] KHAROSHTHI SIGN BAR ABOVE..KHAROSHTHI SIGN DOT BELOW
unicode_line_break(0x10A3F, 0x10A3F, 'CM'). % Mn       KHAROSHTHI VIRAMA
unicode_line_break(0x11000, 0x11000, 'CM'). % Mc       BRAHMI SIGN CANDRABINDU
unicode_line_break(0x11001, 0x11001, 'CM'). % Mn       BRAHMI SIGN ANUSVARA
unicode_line_break(0x11002, 0x11002, 'CM'). % Mc       BRAHMI SIGN VISARGA
unicode_line_break(0x11038, 0x11046, 'CM'). % Mn  [15] BRAHMI VOWEL SIGN AA..BRAHMI VIRAMA
unicode_line_break(0x11080, 0x11081, 'CM'). % Mn   [2] KAITHI SIGN CANDRABINDU..KAITHI SIGN ANUSVARA
unicode_line_break(0x11082, 0x11082, 'CM'). % Mc       KAITHI SIGN VISARGA
unicode_line_break(0x110B0, 0x110B2, 'CM'). % Mc   [3] KAITHI VOWEL SIGN AA..KAITHI VOWEL SIGN II
unicode_line_break(0x110B3, 0x110B6, 'CM'). % Mn   [4] KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN AI
unicode_line_break(0x110B7, 0x110B8, 'CM'). % Mc   [2] KAITHI VOWEL SIGN O..KAITHI VOWEL SIGN AU
unicode_line_break(0x110B9, 0x110BA, 'CM'). % Mn   [2] KAITHI SIGN VIRAMA..KAITHI SIGN NUKTA
unicode_line_break(0x11100, 0x11102, 'CM'). % Mn   [3] CHAKMA SIGN CANDRABINDU..CHAKMA SIGN VISARGA
unicode_line_break(0x11127, 0x1112B, 'CM'). % Mn   [5] CHAKMA VOWEL SIGN A..CHAKMA VOWEL SIGN UU
unicode_line_break(0x1112C, 0x1112C, 'CM'). % Mc       CHAKMA VOWEL SIGN E
unicode_line_break(0x1112D, 0x11134, 'CM'). % Mn   [8] CHAKMA VOWEL SIGN AI..CHAKMA MAAYYAA
unicode_line_break(0x11180, 0x11181, 'CM'). % Mn   [2] SHARADA SIGN CANDRABINDU..SHARADA SIGN ANUSVARA
unicode_line_break(0x11182, 0x11182, 'CM'). % Mc       SHARADA SIGN VISARGA
unicode_line_break(0x111B3, 0x111B5, 'CM'). % Mc   [3] SHARADA VOWEL SIGN AA..SHARADA VOWEL SIGN II
unicode_line_break(0x111B6, 0x111BE, 'CM'). % Mn   [9] SHARADA VOWEL SIGN U..SHARADA VOWEL SIGN O
unicode_line_break(0x111BF, 0x111C0, 'CM'). % Mc   [2] SHARADA VOWEL SIGN AU..SHARADA SIGN VIRAMA
unicode_line_break(0x116AB, 0x116AB, 'CM'). % Mn       TAKRI SIGN ANUSVARA
unicode_line_break(0x116AC, 0x116AC, 'CM'). % Mc       TAKRI SIGN VISARGA
unicode_line_break(0x116AD, 0x116AD, 'CM'). % Mn       TAKRI VOWEL SIGN AA
unicode_line_break(0x116AE, 0x116AF, 'CM'). % Mc   [2] TAKRI VOWEL SIGN I..TAKRI VOWEL SIGN II
unicode_line_break(0x116B0, 0x116B5, 'CM'). % Mn   [6] TAKRI VOWEL SIGN U..TAKRI VOWEL SIGN AU
unicode_line_break(0x116B6, 0x116B6, 'CM'). % Mc       TAKRI SIGN VIRAMA
unicode_line_break(0x116B7, 0x116B7, 'CM'). % Mn       TAKRI SIGN NUKTA
unicode_line_break(0x16F51, 0x16F7E, 'CM'). % Mc  [46] MIAO SIGN ASPIRATION..MIAO VOWEL SIGN NG
unicode_line_break(0x16F8F, 0x16F92, 'CM'). % Mn   [4] MIAO TONE RIGHT..MIAO TONE BELOW
unicode_line_break(0x1D165, 0x1D166, 'CM'). % Mc   [2] MUSICAL SYMBOL COMBINING STEM..MUSICAL SYMBOL COMBINING SPRECHGESANG STEM
unicode_line_break(0x1D167, 0x1D169, 'CM'). % Mn   [3] MUSICAL SYMBOL COMBINING TREMOLO-1..MUSICAL SYMBOL COMBINING TREMOLO-3
unicode_line_break(0x1D16D, 0x1D172, 'CM'). % Mc   [6] MUSICAL SYMBOL COMBINING AUGMENTATION DOT..MUSICAL SYMBOL COMBINING FLAG-5
unicode_line_break(0x1D173, 0x1D17A, 'CM'). % Cf   [8] MUSICAL SYMBOL BEGIN BEAM..MUSICAL SYMBOL END PHRASE
unicode_line_break(0x1D17B, 0x1D182, 'CM'). % Mn   [8] MUSICAL SYMBOL COMBINING ACCENT..MUSICAL SYMBOL COMBINING LOURE
unicode_line_break(0x1D185, 0x1D18B, 'CM'). % Mn   [7] MUSICAL SYMBOL COMBINING DOIT..MUSICAL SYMBOL COMBINING TRIPLE TONGUE
unicode_line_break(0x1D1AA, 0x1D1AD, 'CM'). % Mn   [4] MUSICAL SYMBOL COMBINING DOWN BOW..MUSICAL SYMBOL COMBINING SNAP PIZZICATO
unicode_line_break(0x1D242, 0x1D244, 'CM'). % Mn   [3] COMBINING GREEK MUSICAL TRISEME..COMBINING GREEK MUSICAL PENTASEME
unicode_line_break(0xE0001, 0xE0001, 'CM'). % Cf       LANGUAGE TAG
unicode_line_break(0xE0020, 0xE007F, 'CM'). % Cf  [96] TAG SPACE..CANCEL TAG
unicode_line_break(0xE0100, 0xE01EF, 'CM'). % Mn [240] VARIATION SELECTOR-17..VARIATION SELECTOR-256

% Total code points: 1628

% ================================================

% Line_Break=Break_Before

unicode_line_break(0x00B4, 0x00B4, 'BB'). % Sk       ACUTE ACCENT
unicode_line_break(0x02C8, 0x02C8, 'BB'). % Lm       MODIFIER LETTER VERTICAL LINE
unicode_line_break(0x02CC, 0x02CC, 'BB'). % Lm       MODIFIER LETTER LOW VERTICAL LINE
unicode_line_break(0x02DF, 0x02DF, 'BB'). % Sk       MODIFIER LETTER CROSS ACCENT
unicode_line_break(0x0F01, 0x0F03, 'BB'). % So   [3] TIBETAN MARK GTER YIG MGO TRUNCATED A..TIBETAN MARK GTER YIG MGO -UM GTER TSHEG MA
unicode_line_break(0x0F04, 0x0F04, 'BB'). % Po       TIBETAN MARK INITIAL YIG MGO MDUN MA
unicode_line_break(0x0F06, 0x0F07, 'BB'). % Po   [2] TIBETAN MARK CARET YIG MGO PHUR SHAD MA..TIBETAN MARK YIG MGO TSHEG SHAD MA
unicode_line_break(0x0F09, 0x0F0A, 'BB'). % Po   [2] TIBETAN MARK BSKUR YIG MGO..TIBETAN MARK BKA- SHOG YIG MGO
unicode_line_break(0x0FD0, 0x0FD1, 'BB'). % Po   [2] TIBETAN MARK BSKA- SHOG GI MGO RGYAN..TIBETAN MARK MNYAM YIG GI MGO RGYAN
unicode_line_break(0x0FD3, 0x0FD3, 'BB'). % Po       TIBETAN MARK INITIAL BRDA RNYING YIG MGO MDUN MA
unicode_line_break(0x1806, 0x1806, 'BB'). % Pd       MONGOLIAN TODO SOFT HYPHEN
unicode_line_break(0x1FFD, 0x1FFD, 'BB'). % Sk       GREEK OXIA
unicode_line_break(0xA874, 0xA875, 'BB'). % Po   [2] PHAGS-PA SINGLE HEAD MARK..PHAGS-PA DOUBLE HEAD MARK

% Total code points: 19

% ================================================

% Line_Break=Break_After

unicode_line_break(0x0009, 0x0009, 'BA'). % Cc       <control-0009>
unicode_line_break(0x007C, 0x007C, 'BA'). % Sm       VERTICAL LINE
unicode_line_break(0x00AD, 0x00AD, 'BA'). % Cf       SOFT HYPHEN
unicode_line_break(0x058A, 0x058A, 'BA'). % Pd       ARMENIAN HYPHEN
unicode_line_break(0x05BE, 0x05BE, 'BA'). % Pd       HEBREW PUNCTUATION MAQAF
unicode_line_break(0x0964, 0x0965, 'BA'). % Po   [2] DEVANAGARI DANDA..DEVANAGARI DOUBLE DANDA
unicode_line_break(0x0E5A, 0x0E5B, 'BA'). % Po   [2] THAI CHARACTER ANGKHANKHU..THAI CHARACTER KHOMUT
unicode_line_break(0x0F0B, 0x0F0B, 'BA'). % Po       TIBETAN MARK INTERSYLLABIC TSHEG
unicode_line_break(0x0F34, 0x0F34, 'BA'). % So       TIBETAN MARK BSDUS RTAGS
unicode_line_break(0x0F7F, 0x0F7F, 'BA'). % Mc       TIBETAN SIGN RNAM BCAD
unicode_line_break(0x0F85, 0x0F85, 'BA'). % Po       TIBETAN MARK PALUTA
unicode_line_break(0x0FBE, 0x0FBF, 'BA'). % So   [2] TIBETAN KU RU KHA..TIBETAN KU RU KHA BZHI MIG CAN
unicode_line_break(0x0FD2, 0x0FD2, 'BA'). % Po       TIBETAN MARK NYIS TSHEG
unicode_line_break(0x104A, 0x104B, 'BA'). % Po   [2] MYANMAR SIGN LITTLE SECTION..MYANMAR SIGN SECTION
unicode_line_break(0x1361, 0x1361, 'BA'). % Po       ETHIOPIC WORDSPACE
unicode_line_break(0x1400, 0x1400, 'BA'). % Pd       CANADIAN SYLLABICS HYPHEN
unicode_line_break(0x1680, 0x1680, 'BA'). % Zs       OGHAM SPACE MARK
unicode_line_break(0x16EB, 0x16ED, 'BA'). % Po   [3] RUNIC SINGLE PUNCTUATION..RUNIC CROSS PUNCTUATION
unicode_line_break(0x1735, 0x1736, 'BA'). % Po   [2] PHILIPPINE SINGLE PUNCTUATION..PHILIPPINE DOUBLE PUNCTUATION
unicode_line_break(0x17D4, 0x17D5, 'BA'). % Po   [2] KHMER SIGN KHAN..KHMER SIGN BARIYOOSAN
unicode_line_break(0x17D8, 0x17D8, 'BA'). % Po       KHMER SIGN BEYYAL
unicode_line_break(0x17DA, 0x17DA, 'BA'). % Po       KHMER SIGN KOOMUUT
unicode_line_break(0x1804, 0x1805, 'BA'). % Po   [2] MONGOLIAN COLON..MONGOLIAN FOUR DOTS
unicode_line_break(0x1B5A, 0x1B5B, 'BA'). % Po   [2] BALINESE PANTI..BALINESE PAMADA
unicode_line_break(0x1B5D, 0x1B60, 'BA'). % Po   [4] BALINESE CARIK PAMUNGKAH..BALINESE PAMENENG
unicode_line_break(0x1C3B, 0x1C3F, 'BA'). % Po   [5] LEPCHA PUNCTUATION TA-ROL..LEPCHA PUNCTUATION TSHOOK
unicode_line_break(0x1C7E, 0x1C7F, 'BA'). % Po   [2] OL CHIKI PUNCTUATION MUCAAD..OL CHIKI PUNCTUATION DOUBLE MUCAAD
unicode_line_break(0x2000, 0x2006, 'BA'). % Zs   [7] EN QUAD..SIX-PER-EM SPACE
unicode_line_break(0x2008, 0x200A, 'BA'). % Zs   [3] PUNCTUATION SPACE..HAIR SPACE
unicode_line_break(0x2010, 0x2010, 'BA'). % Pd       HYPHEN
unicode_line_break(0x2012, 0x2013, 'BA'). % Pd   [2] FIGURE DASH..EN DASH
unicode_line_break(0x2027, 0x2027, 'BA'). % Po       HYPHENATION POINT
unicode_line_break(0x2056, 0x2056, 'BA'). % Po       THREE DOT PUNCTUATION
unicode_line_break(0x2058, 0x205B, 'BA'). % Po   [4] FOUR DOT PUNCTUATION..FOUR DOT MARK
unicode_line_break(0x205D, 0x205E, 'BA'). % Po   [2] TRICOLON..VERTICAL FOUR DOTS
unicode_line_break(0x205F, 0x205F, 'BA'). % Zs       MEDIUM MATHEMATICAL SPACE
unicode_line_break(0x2CFA, 0x2CFC, 'BA'). % Po   [3] COPTIC OLD NUBIAN DIRECT QUESTION MARK..COPTIC OLD NUBIAN VERSE DIVIDER
unicode_line_break(0x2CFF, 0x2CFF, 'BA'). % Po       COPTIC MORPHOLOGICAL DIVIDER
unicode_line_break(0x2D70, 0x2D70, 'BA'). % Po       TIFINAGH SEPARATOR MARK
unicode_line_break(0x2E0E, 0x2E15, 'BA'). % Po   [8] EDITORIAL CORONIS..UPWARDS ANCORA
unicode_line_break(0x2E17, 0x2E17, 'BA'). % Pd       DOUBLE OBLIQUE HYPHEN
unicode_line_break(0x2E19, 0x2E19, 'BA'). % Po       PALM BRANCH
unicode_line_break(0x2E2A, 0x2E2D, 'BA'). % Po   [4] TWO DOTS OVER ONE DOT PUNCTUATION..FIVE DOT MARK
unicode_line_break(0x2E30, 0x2E31, 'BA'). % Po   [2] RING POINT..WORD SEPARATOR MIDDLE DOT
unicode_line_break(0x2E33, 0x2E34, 'BA'). % Po   [2] RAISED DOT..RAISED COMMA
unicode_line_break(0xA4FE, 0xA4FF, 'BA'). % Po   [2] LISU PUNCTUATION COMMA..LISU PUNCTUATION FULL STOP
unicode_line_break(0xA60D, 0xA60D, 'BA'). % Po       VAI COMMA
unicode_line_break(0xA60F, 0xA60F, 'BA'). % Po       VAI QUESTION MARK
unicode_line_break(0xA6F3, 0xA6F7, 'BA'). % Po   [5] BAMUM FULL STOP..BAMUM QUESTION MARK
unicode_line_break(0xA8CE, 0xA8CF, 'BA'). % Po   [2] SAURASHTRA DANDA..SAURASHTRA DOUBLE DANDA
unicode_line_break(0xA92E, 0xA92F, 'BA'). % Po   [2] KAYAH LI SIGN CWI..KAYAH LI SIGN SHYA
unicode_line_break(0xA9C7, 0xA9C9, 'BA'). % Po   [3] JAVANESE PADA PANGKAT..JAVANESE PADA LUNGSI
unicode_line_break(0xAA5D, 0xAA5F, 'BA'). % Po   [3] CHAM PUNCTUATION DANDA..CHAM PUNCTUATION TRIPLE DANDA
unicode_line_break(0xAAF0, 0xAAF1, 'BA'). % Po   [2] MEETEI MAYEK CHEIKHAN..MEETEI MAYEK AHANG KHUDAM
unicode_line_break(0xABEB, 0xABEB, 'BA'). % Po       MEETEI MAYEK CHEIKHEI
unicode_line_break(0x10100, 0x10102, 'BA'). % Po   [3] AEGEAN WORD SEPARATOR LINE..AEGEAN CHECK MARK
unicode_line_break(0x1039F, 0x1039F, 'BA'). % Po       UGARITIC WORD DIVIDER
unicode_line_break(0x103D0, 0x103D0, 'BA'). % Po       OLD PERSIAN WORD DIVIDER
unicode_line_break(0x10857, 0x10857, 'BA'). % Po       IMPERIAL ARAMAIC SECTION SIGN
unicode_line_break(0x1091F, 0x1091F, 'BA'). % Po       PHOENICIAN WORD SEPARATOR
unicode_line_break(0x10A50, 0x10A57, 'BA'). % Po   [8] KHAROSHTHI PUNCTUATION DOT..KHAROSHTHI PUNCTUATION DOUBLE DANDA
unicode_line_break(0x10B39, 0x10B3F, 'BA'). % Po   [7] AVESTAN ABBREVIATION MARK..LARGE ONE RING OVER TWO RINGS PUNCTUATION
unicode_line_break(0x11047, 0x11048, 'BA'). % Po   [2] BRAHMI DANDA..BRAHMI DOUBLE DANDA
unicode_line_break(0x110BE, 0x110C1, 'BA'). % Po   [4] KAITHI SECTION MARK..KAITHI DOUBLE DANDA
unicode_line_break(0x11140, 0x11143, 'BA'). % Po   [4] CHAKMA SECTION MARK..CHAKMA QUESTION MARK
unicode_line_break(0x111C5, 0x111C6, 'BA'). % Po   [2] SHARADA DANDA..SHARADA DOUBLE DANDA
unicode_line_break(0x111C8, 0x111C8, 'BA'). % Po       SHARADA SEPARATOR
unicode_line_break(0x12470, 0x12473, 'BA'). % Po   [4] CUNEIFORM PUNCTUATION SIGN OLD ASSYRIAN WORD DIVIDER..CUNEIFORM PUNCTUATION SIGN DIAGONAL TRICOLON

% Total code points: 151

% ================================================

% Line_Break=Space

unicode_line_break(0x0020, 0x0020, 'SP'). % Zs       SPACE

% Total code points: 1

% ================================================

% Line_Break=Mandatory_Break

unicode_line_break(0x000B, 0x000C, 'BK'). % Cc   [2] <control-000B>..<control-000C>
unicode_line_break(0x2028, 0x2028, 'BK'). % Zl       LINE SEPARATOR
unicode_line_break(0x2029, 0x2029, 'BK'). % Zp       PARAGRAPH SEPARATOR

% Total code points: 4

% ================================================

% Line_Break=Carriage_Return

unicode_line_break(0x000D, 0x000D, 'CR'). % Cc       <control-000D>

% Total code points: 1

% ================================================

% Line_Break=Line_Feed

unicode_line_break(0x000A, 0x000A, 'LF'). % Cc       <control-000A>

% Total code points: 1

% ================================================

% Line_Break=Contingent_Break

unicode_line_break(0xFFFC, 0xFFFC, 'CB'). % So       OBJECT REPLACEMENT CHARACTER

% Total code points: 1

% ================================================

% Line_Break=Complex_Context

unicode_line_break(0x0E01, 0x0E30, 'SA'). % Lo  [48] THAI CHARACTER KO KAI..THAI CHARACTER SARA A
unicode_line_break(0x0E31, 0x0E31, 'SA'). % Mn       THAI CHARACTER MAI HAN-AKAT
unicode_line_break(0x0E32, 0x0E33, 'SA'). % Lo   [2] THAI CHARACTER SARA AA..THAI CHARACTER SARA AM
unicode_line_break(0x0E34, 0x0E3A, 'SA'). % Mn   [7] THAI CHARACTER SARA I..THAI CHARACTER PHINTHU
unicode_line_break(0x0E40, 0x0E45, 'SA'). % Lo   [6] THAI CHARACTER SARA E..THAI CHARACTER LAKKHANGYAO
unicode_line_break(0x0E46, 0x0E46, 'SA'). % Lm       THAI CHARACTER MAIYAMOK
unicode_line_break(0x0E47, 0x0E4E, 'SA'). % Mn   [8] THAI CHARACTER MAITAIKHU..THAI CHARACTER YAMAKKAN
unicode_line_break(0x0E81, 0x0E82, 'SA'). % Lo   [2] LAO LETTER KO..LAO LETTER KHO SUNG
unicode_line_break(0x0E84, 0x0E84, 'SA'). % Lo       LAO LETTER KHO TAM
unicode_line_break(0x0E87, 0x0E88, 'SA'). % Lo   [2] LAO LETTER NGO..LAO LETTER CO
unicode_line_break(0x0E8A, 0x0E8A, 'SA'). % Lo       LAO LETTER SO TAM
unicode_line_break(0x0E8D, 0x0E8D, 'SA'). % Lo       LAO LETTER NYO
unicode_line_break(0x0E94, 0x0E97, 'SA'). % Lo   [4] LAO LETTER DO..LAO LETTER THO TAM
unicode_line_break(0x0E99, 0x0E9F, 'SA'). % Lo   [7] LAO LETTER NO..LAO LETTER FO SUNG
unicode_line_break(0x0EA1, 0x0EA3, 'SA'). % Lo   [3] LAO LETTER MO..LAO LETTER LO LING
unicode_line_break(0x0EA5, 0x0EA5, 'SA'). % Lo       LAO LETTER LO LOOT
unicode_line_break(0x0EA7, 0x0EA7, 'SA'). % Lo       LAO LETTER WO
unicode_line_break(0x0EAA, 0x0EAB, 'SA'). % Lo   [2] LAO LETTER SO SUNG..LAO LETTER HO SUNG
unicode_line_break(0x0EAD, 0x0EB0, 'SA'). % Lo   [4] LAO LETTER O..LAO VOWEL SIGN A
unicode_line_break(0x0EB1, 0x0EB1, 'SA'). % Mn       LAO VOWEL SIGN MAI KAN
unicode_line_break(0x0EB2, 0x0EB3, 'SA'). % Lo   [2] LAO VOWEL SIGN AA..LAO VOWEL SIGN AM
unicode_line_break(0x0EB4, 0x0EB9, 'SA'). % Mn   [6] LAO VOWEL SIGN I..LAO VOWEL SIGN UU
unicode_line_break(0x0EBB, 0x0EBC, 'SA'). % Mn   [2] LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO
unicode_line_break(0x0EBD, 0x0EBD, 'SA'). % Lo       LAO SEMIVOWEL SIGN NYO
unicode_line_break(0x0EC0, 0x0EC4, 'SA'). % Lo   [5] LAO VOWEL SIGN E..LAO VOWEL SIGN AI
unicode_line_break(0x0EC6, 0x0EC6, 'SA'). % Lm       LAO KO LA
unicode_line_break(0x0EC8, 0x0ECD, 'SA'). % Mn   [6] LAO TONE MAI EK..LAO NIGGAHITA
unicode_line_break(0x0EDC, 0x0EDF, 'SA'). % Lo   [4] LAO HO NO..LAO LETTER KHMU NYO
unicode_line_break(0x1000, 0x102A, 'SA'). % Lo  [43] MYANMAR LETTER KA..MYANMAR LETTER AU
unicode_line_break(0x102B, 0x102C, 'SA'). % Mc   [2] MYANMAR VOWEL SIGN TALL AA..MYANMAR VOWEL SIGN AA
unicode_line_break(0x102D, 0x1030, 'SA'). % Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
unicode_line_break(0x1031, 0x1031, 'SA'). % Mc       MYANMAR VOWEL SIGN E
unicode_line_break(0x1032, 0x1037, 'SA'). % Mn   [6] MYANMAR VOWEL SIGN AI..MYANMAR SIGN DOT BELOW
unicode_line_break(0x1038, 0x1038, 'SA'). % Mc       MYANMAR SIGN VISARGA
unicode_line_break(0x1039, 0x103A, 'SA'). % Mn   [2] MYANMAR SIGN VIRAMA..MYANMAR SIGN ASAT
unicode_line_break(0x103B, 0x103C, 'SA'). % Mc   [2] MYANMAR CONSONANT SIGN MEDIAL YA..MYANMAR CONSONANT SIGN MEDIAL RA
unicode_line_break(0x103D, 0x103E, 'SA'). % Mn   [2] MYANMAR CONSONANT SIGN MEDIAL WA..MYANMAR CONSONANT SIGN MEDIAL HA
unicode_line_break(0x103F, 0x103F, 'SA'). % Lo       MYANMAR LETTER GREAT SA
unicode_line_break(0x1050, 0x1055, 'SA'). % Lo   [6] MYANMAR LETTER SHA..MYANMAR LETTER VOCALIC LL
unicode_line_break(0x1056, 0x1057, 'SA'). % Mc   [2] MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR
unicode_line_break(0x1058, 0x1059, 'SA'). % Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
unicode_line_break(0x105A, 0x105D, 'SA'). % Lo   [4] MYANMAR LETTER MON NGA..MYANMAR LETTER MON BBE
unicode_line_break(0x105E, 0x1060, 'SA'). % Mn   [3] MYANMAR CONSONANT SIGN MON MEDIAL NA..MYANMAR CONSONANT SIGN MON MEDIAL LA
unicode_line_break(0x1061, 0x1061, 'SA'). % Lo       MYANMAR LETTER SGAW KAREN SHA
unicode_line_break(0x1062, 0x1064, 'SA'). % Mc   [3] MYANMAR VOWEL SIGN SGAW KAREN EU..MYANMAR TONE MARK SGAW KAREN KE PHO
unicode_line_break(0x1065, 0x1066, 'SA'). % Lo   [2] MYANMAR LETTER WESTERN PWO KAREN THA..MYANMAR LETTER WESTERN PWO KAREN PWA
unicode_line_break(0x1067, 0x106D, 'SA'). % Mc   [7] MYANMAR VOWEL SIGN WESTERN PWO KAREN EU..MYANMAR SIGN WESTERN PWO KAREN TONE-5
unicode_line_break(0x106E, 0x1070, 'SA'). % Lo   [3] MYANMAR LETTER EASTERN PWO KAREN NNA..MYANMAR LETTER EASTERN PWO KAREN GHWA
unicode_line_break(0x1071, 0x1074, 'SA'). % Mn   [4] MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE
unicode_line_break(0x1075, 0x1081, 'SA'). % Lo  [13] MYANMAR LETTER SHAN KA..MYANMAR LETTER SHAN HA
unicode_line_break(0x1082, 0x1082, 'SA'). % Mn       MYANMAR CONSONANT SIGN SHAN MEDIAL WA
unicode_line_break(0x1083, 0x1084, 'SA'). % Mc   [2] MYANMAR VOWEL SIGN SHAN AA..MYANMAR VOWEL SIGN SHAN E
unicode_line_break(0x1085, 0x1086, 'SA'). % Mn   [2] MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y
unicode_line_break(0x1087, 0x108C, 'SA'). % Mc   [6] MYANMAR SIGN SHAN TONE-2..MYANMAR SIGN SHAN COUNCIL TONE-3
unicode_line_break(0x108D, 0x108D, 'SA'). % Mn       MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE
unicode_line_break(0x108E, 0x108E, 'SA'). % Lo       MYANMAR LETTER RUMAI PALAUNG FA
unicode_line_break(0x108F, 0x108F, 'SA'). % Mc       MYANMAR SIGN RUMAI PALAUNG TONE-5
unicode_line_break(0x109A, 0x109C, 'SA'). % Mc   [3] MYANMAR SIGN KHAMTI TONE-1..MYANMAR VOWEL SIGN AITON A
unicode_line_break(0x109D, 0x109D, 'SA'). % Mn       MYANMAR VOWEL SIGN AITON AI
unicode_line_break(0x109E, 0x109F, 'SA'). % So   [2] MYANMAR SYMBOL SHAN ONE..MYANMAR SYMBOL SHAN EXCLAMATION
unicode_line_break(0x1780, 0x17B3, 'SA'). % Lo  [52] KHMER LETTER KA..KHMER INDEPENDENT VOWEL QAU
unicode_line_break(0x17B4, 0x17B5, 'SA'). % Mn   [2] KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA
unicode_line_break(0x17B6, 0x17B6, 'SA'). % Mc       KHMER VOWEL SIGN AA
unicode_line_break(0x17B7, 0x17BD, 'SA'). % Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
unicode_line_break(0x17BE, 0x17C5, 'SA'). % Mc   [8] KHMER VOWEL SIGN OE..KHMER VOWEL SIGN AU
unicode_line_break(0x17C6, 0x17C6, 'SA'). % Mn       KHMER SIGN NIKAHIT
unicode_line_break(0x17C7, 0x17C8, 'SA'). % Mc   [2] KHMER SIGN REAHMUK..KHMER SIGN YUUKALEAPINTU
unicode_line_break(0x17C9, 0x17D3, 'SA'). % Mn  [11] KHMER SIGN MUUSIKATOAN..KHMER SIGN BATHAMASAT
unicode_line_break(0x17D7, 0x17D7, 'SA'). % Lm       KHMER SIGN LEK TOO
unicode_line_break(0x17DC, 0x17DC, 'SA'). % Lo       KHMER SIGN AVAKRAHASANYA
unicode_line_break(0x17DD, 0x17DD, 'SA'). % Mn       KHMER SIGN ATTHACAN
unicode_line_break(0x1950, 0x196D, 'SA'). % Lo  [30] TAI LE LETTER KA..TAI LE LETTER AI
unicode_line_break(0x1970, 0x1974, 'SA'). % Lo   [5] TAI LE LETTER TONE-2..TAI LE LETTER TONE-6
unicode_line_break(0x1980, 0x19AB, 'SA'). % Lo  [44] NEW TAI LUE LETTER HIGH QA..NEW TAI LUE LETTER LOW SUA
unicode_line_break(0x19B0, 0x19C0, 'SA'). % Mc  [17] NEW TAI LUE VOWEL SIGN VOWEL SHORTENER..NEW TAI LUE VOWEL SIGN IY
unicode_line_break(0x19C1, 0x19C7, 'SA'). % Lo   [7] NEW TAI LUE LETTER FINAL V..NEW TAI LUE LETTER FINAL B
unicode_line_break(0x19C8, 0x19C9, 'SA'). % Mc   [2] NEW TAI LUE TONE MARK-1..NEW TAI LUE TONE MARK-2
unicode_line_break(0x19DA, 0x19DA, 'SA'). % No       NEW TAI LUE THAM DIGIT ONE
unicode_line_break(0x19DE, 0x19DF, 'SA'). % So   [2] NEW TAI LUE SIGN LAE..NEW TAI LUE SIGN LAEV
unicode_line_break(0x1A20, 0x1A54, 'SA'). % Lo  [53] TAI THAM LETTER HIGH KA..TAI THAM LETTER GREAT SA
unicode_line_break(0x1A55, 0x1A55, 'SA'). % Mc       TAI THAM CONSONANT SIGN MEDIAL RA
unicode_line_break(0x1A56, 0x1A56, 'SA'). % Mn       TAI THAM CONSONANT SIGN MEDIAL LA
unicode_line_break(0x1A57, 0x1A57, 'SA'). % Mc       TAI THAM CONSONANT SIGN LA TANG LAI
unicode_line_break(0x1A58, 0x1A5E, 'SA'). % Mn   [7] TAI THAM SIGN MAI KANG LAI..TAI THAM CONSONANT SIGN SA
unicode_line_break(0x1A60, 0x1A60, 'SA'). % Mn       TAI THAM SIGN SAKOT
unicode_line_break(0x1A61, 0x1A61, 'SA'). % Mc       TAI THAM VOWEL SIGN A
unicode_line_break(0x1A62, 0x1A62, 'SA'). % Mn       TAI THAM VOWEL SIGN MAI SAT
unicode_line_break(0x1A63, 0x1A64, 'SA'). % Mc   [2] TAI THAM VOWEL SIGN AA..TAI THAM VOWEL SIGN TALL AA
unicode_line_break(0x1A65, 0x1A6C, 'SA'). % Mn   [8] TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN OA BELOW
unicode_line_break(0x1A6D, 0x1A72, 'SA'). % Mc   [6] TAI THAM VOWEL SIGN OY..TAI THAM VOWEL SIGN THAM AI
unicode_line_break(0x1A73, 0x1A7C, 'SA'). % Mn  [10] TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN KHUEN-LUE KARAN
unicode_line_break(0x1AA0, 0x1AA6, 'SA'). % Po   [7] TAI THAM SIGN WIANG..TAI THAM SIGN REVERSED ROTATED RANA
unicode_line_break(0x1AA7, 0x1AA7, 'SA'). % Lm       TAI THAM SIGN MAI YAMOK
unicode_line_break(0x1AA8, 0x1AAD, 'SA'). % Po   [6] TAI THAM SIGN KAAN..TAI THAM SIGN CAANG
unicode_line_break(0xAA60, 0xAA6F, 'SA'). % Lo  [16] MYANMAR LETTER KHAMTI GA..MYANMAR LETTER KHAMTI FA
unicode_line_break(0xAA70, 0xAA70, 'SA'). % Lm       MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION
unicode_line_break(0xAA71, 0xAA76, 'SA'). % Lo   [6] MYANMAR LETTER KHAMTI XA..MYANMAR LOGOGRAM KHAMTI HM
unicode_line_break(0xAA77, 0xAA79, 'SA'). % So   [3] MYANMAR SYMBOL AITON EXCLAMATION..MYANMAR SYMBOL AITON TWO
unicode_line_break(0xAA7A, 0xAA7A, 'SA'). % Lo       MYANMAR LETTER AITON RA
unicode_line_break(0xAA7B, 0xAA7B, 'SA'). % Mc       MYANMAR SIGN PAO KAREN TONE
unicode_line_break(0xAA80, 0xAAAF, 'SA'). % Lo  [48] TAI VIET LETTER LOW KO..TAI VIET LETTER HIGH O
unicode_line_break(0xAAB0, 0xAAB0, 'SA'). % Mn       TAI VIET MAI KANG
unicode_line_break(0xAAB1, 0xAAB1, 'SA'). % Lo       TAI VIET VOWEL AA
unicode_line_break(0xAAB2, 0xAAB4, 'SA'). % Mn   [3] TAI VIET VOWEL I..TAI VIET VOWEL U
unicode_line_break(0xAAB5, 0xAAB6, 'SA'). % Lo   [2] TAI VIET VOWEL E..TAI VIET VOWEL O
unicode_line_break(0xAAB7, 0xAAB8, 'SA'). % Mn   [2] TAI VIET MAI KHIT..TAI VIET VOWEL IA
unicode_line_break(0xAAB9, 0xAABD, 'SA'). % Lo   [5] TAI VIET VOWEL UEA..TAI VIET VOWEL AN
unicode_line_break(0xAABE, 0xAABF, 'SA'). % Mn   [2] TAI VIET VOWEL AM..TAI VIET TONE MAI EK
unicode_line_break(0xAAC0, 0xAAC0, 'SA'). % Lo       TAI VIET TONE MAI NUENG
unicode_line_break(0xAAC1, 0xAAC1, 'SA'). % Mn       TAI VIET TONE MAI THO
unicode_line_break(0xAAC2, 0xAAC2, 'SA'). % Lo       TAI VIET TONE MAI SONG
unicode_line_break(0xAADB, 0xAADC, 'SA'). % Lo   [2] TAI VIET SYMBOL KON..TAI VIET SYMBOL NUENG
unicode_line_break(0xAADD, 0xAADD, 'SA'). % Lm       TAI VIET SYMBOL SAM
unicode_line_break(0xAADE, 0xAADF, 'SA'). % Po   [2] TAI VIET SYMBOL HO HOI..TAI VIET SYMBOL KOI KOI

% Total code points: 665

% ================================================

% Line_Break=Ambiguous

unicode_line_break(0x00A7, 0x00A7, 'AI'). % Po       SECTION SIGN
unicode_line_break(0x00A8, 0x00A8, 'AI'). % Sk       DIAERESIS
unicode_line_break(0x00AA, 0x00AA, 'AI'). % Lo       FEMININE ORDINAL INDICATOR
unicode_line_break(0x00B2, 0x00B3, 'AI'). % No   [2] SUPERSCRIPT TWO..SUPERSCRIPT THREE
unicode_line_break(0x00B6, 0x00B7, 'AI'). % Po   [2] PILCROW SIGN..MIDDLE DOT
unicode_line_break(0x00B8, 0x00B8, 'AI'). % Sk       CEDILLA
unicode_line_break(0x00B9, 0x00B9, 'AI'). % No       SUPERSCRIPT ONE
unicode_line_break(0x00BA, 0x00BA, 'AI'). % Lo       MASCULINE ORDINAL INDICATOR
unicode_line_break(0x00BC, 0x00BE, 'AI'). % No   [3] VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS
unicode_line_break(0x00D7, 0x00D7, 'AI'). % Sm       MULTIPLICATION SIGN
unicode_line_break(0x00F7, 0x00F7, 'AI'). % Sm       DIVISION SIGN
unicode_line_break(0x02C7, 0x02C7, 'AI'). % Lm       CARON
unicode_line_break(0x02C9, 0x02CB, 'AI'). % Lm   [3] MODIFIER LETTER MACRON..MODIFIER LETTER GRAVE ACCENT
unicode_line_break(0x02CD, 0x02CD, 'AI'). % Lm       MODIFIER LETTER LOW MACRON
unicode_line_break(0x02D0, 0x02D0, 'AI'). % Lm       MODIFIER LETTER TRIANGULAR COLON
unicode_line_break(0x02D8, 0x02DB, 'AI'). % Sk   [4] BREVE..OGONEK
unicode_line_break(0x02DD, 0x02DD, 'AI'). % Sk       DOUBLE ACUTE ACCENT
unicode_line_break(0x2015, 0x2015, 'AI'). % Pd       HORIZONTAL BAR
unicode_line_break(0x2016, 0x2016, 'AI'). % Po       DOUBLE VERTICAL LINE
unicode_line_break(0x2020, 0x2021, 'AI'). % Po   [2] DAGGER..DOUBLE DAGGER
unicode_line_break(0x203B, 0x203B, 'AI'). % Po       REFERENCE MARK
unicode_line_break(0x2074, 0x2074, 'AI'). % No       SUPERSCRIPT FOUR
unicode_line_break(0x207F, 0x207F, 'AI'). % Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_line_break(0x2081, 0x2084, 'AI'). % No   [4] SUBSCRIPT ONE..SUBSCRIPT FOUR
unicode_line_break(0x2105, 0x2105, 'AI'). % So       CARE OF
unicode_line_break(0x2113, 0x2113, 'AI'). % L&       SCRIPT SMALL L
unicode_line_break(0x2121, 0x2122, 'AI'). % So   [2] TELEPHONE SIGN..TRADE MARK SIGN
unicode_line_break(0x212B, 0x212B, 'AI'). % L&       ANGSTROM SIGN
unicode_line_break(0x2154, 0x2155, 'AI'). % No   [2] VULGAR FRACTION TWO THIRDS..VULGAR FRACTION ONE FIFTH
unicode_line_break(0x215B, 0x215B, 'AI'). % No       VULGAR FRACTION ONE EIGHTH
unicode_line_break(0x215E, 0x215E, 'AI'). % No       VULGAR FRACTION SEVEN EIGHTHS
unicode_line_break(0x2160, 0x216B, 'AI'). % Nl  [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
unicode_line_break(0x2170, 0x2179, 'AI'). % Nl  [10] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL TEN
unicode_line_break(0x2189, 0x2189, 'AI'). % No       VULGAR FRACTION ZERO THIRDS
unicode_line_break(0x2190, 0x2194, 'AI'). % Sm   [5] LEFTWARDS ARROW..LEFT RIGHT ARROW
unicode_line_break(0x2195, 0x2199, 'AI'). % So   [5] UP DOWN ARROW..SOUTH WEST ARROW
unicode_line_break(0x21D2, 0x21D2, 'AI'). % Sm       RIGHTWARDS DOUBLE ARROW
unicode_line_break(0x21D4, 0x21D4, 'AI'). % Sm       LEFT RIGHT DOUBLE ARROW
unicode_line_break(0x2200, 0x2200, 'AI'). % Sm       FOR ALL
unicode_line_break(0x2202, 0x2203, 'AI'). % Sm   [2] PARTIAL DIFFERENTIAL..THERE EXISTS
unicode_line_break(0x2207, 0x2208, 'AI'). % Sm   [2] NABLA..ELEMENT OF
unicode_line_break(0x220B, 0x220B, 'AI'). % Sm       CONTAINS AS MEMBER
unicode_line_break(0x220F, 0x220F, 'AI'). % Sm       N-ARY PRODUCT
unicode_line_break(0x2211, 0x2211, 'AI'). % Sm       N-ARY SUMMATION
unicode_line_break(0x2215, 0x2215, 'AI'). % Sm       DIVISION SLASH
unicode_line_break(0x221A, 0x221A, 'AI'). % Sm       SQUARE ROOT
unicode_line_break(0x221D, 0x2220, 'AI'). % Sm   [4] PROPORTIONAL TO..ANGLE
unicode_line_break(0x2223, 0x2223, 'AI'). % Sm       DIVIDES
unicode_line_break(0x2225, 0x2225, 'AI'). % Sm       PARALLEL TO
unicode_line_break(0x2227, 0x222C, 'AI'). % Sm   [6] LOGICAL AND..DOUBLE INTEGRAL
unicode_line_break(0x222E, 0x222E, 'AI'). % Sm       CONTOUR INTEGRAL
unicode_line_break(0x2234, 0x2237, 'AI'). % Sm   [4] THEREFORE..PROPORTION
unicode_line_break(0x223C, 0x223D, 'AI'). % Sm   [2] TILDE OPERATOR..REVERSED TILDE
unicode_line_break(0x2248, 0x2248, 'AI'). % Sm       ALMOST EQUAL TO
unicode_line_break(0x224C, 0x224C, 'AI'). % Sm       ALL EQUAL TO
unicode_line_break(0x2252, 0x2252, 'AI'). % Sm       APPROXIMATELY EQUAL TO OR THE IMAGE OF
unicode_line_break(0x2260, 0x2261, 'AI'). % Sm   [2] NOT EQUAL TO..IDENTICAL TO
unicode_line_break(0x2264, 0x2267, 'AI'). % Sm   [4] LESS-THAN OR EQUAL TO..GREATER-THAN OVER EQUAL TO
unicode_line_break(0x226A, 0x226B, 'AI'). % Sm   [2] MUCH LESS-THAN..MUCH GREATER-THAN
unicode_line_break(0x226E, 0x226F, 'AI'). % Sm   [2] NOT LESS-THAN..NOT GREATER-THAN
unicode_line_break(0x2282, 0x2283, 'AI'). % Sm   [2] SUBSET OF..SUPERSET OF
unicode_line_break(0x2286, 0x2287, 'AI'). % Sm   [2] SUBSET OF OR EQUAL TO..SUPERSET OF OR EQUAL TO
unicode_line_break(0x2295, 0x2295, 'AI'). % Sm       CIRCLED PLUS
unicode_line_break(0x2299, 0x2299, 'AI'). % Sm       CIRCLED DOT OPERATOR
unicode_line_break(0x22A5, 0x22A5, 'AI'). % Sm       UP TACK
unicode_line_break(0x22BF, 0x22BF, 'AI'). % Sm       RIGHT TRIANGLE
unicode_line_break(0x2312, 0x2312, 'AI'). % So       ARC
unicode_line_break(0x2460, 0x249B, 'AI'). % No  [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
unicode_line_break(0x249C, 0x24E9, 'AI'). % So  [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_line_break(0x24EA, 0x24FE, 'AI'). % No  [21] CIRCLED DIGIT ZERO..DOUBLE CIRCLED NUMBER TEN
unicode_line_break(0x2500, 0x254B, 'AI'). % So  [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
unicode_line_break(0x2550, 0x2574, 'AI'). % So  [37] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT LEFT
unicode_line_break(0x2580, 0x258F, 'AI'). % So  [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
unicode_line_break(0x2592, 0x2595, 'AI'). % So   [4] MEDIUM SHADE..RIGHT ONE EIGHTH BLOCK
unicode_line_break(0x25A0, 0x25A1, 'AI'). % So   [2] BLACK SQUARE..WHITE SQUARE
unicode_line_break(0x25A3, 0x25A9, 'AI'). % So   [7] WHITE SQUARE CONTAINING BLACK SMALL SQUARE..SQUARE WITH DIAGONAL CROSSHATCH FILL
unicode_line_break(0x25B2, 0x25B3, 'AI'). % So   [2] BLACK UP-POINTING TRIANGLE..WHITE UP-POINTING TRIANGLE
unicode_line_break(0x25B6, 0x25B6, 'AI'). % So       BLACK RIGHT-POINTING TRIANGLE
unicode_line_break(0x25B7, 0x25B7, 'AI'). % Sm       WHITE RIGHT-POINTING TRIANGLE
unicode_line_break(0x25BC, 0x25BD, 'AI'). % So   [2] BLACK DOWN-POINTING TRIANGLE..WHITE DOWN-POINTING TRIANGLE
unicode_line_break(0x25C0, 0x25C0, 'AI'). % So       BLACK LEFT-POINTING TRIANGLE
unicode_line_break(0x25C1, 0x25C1, 'AI'). % Sm       WHITE LEFT-POINTING TRIANGLE
unicode_line_break(0x25C6, 0x25C8, 'AI'). % So   [3] BLACK DIAMOND..WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND
unicode_line_break(0x25CB, 0x25CB, 'AI'). % So       WHITE CIRCLE
unicode_line_break(0x25CE, 0x25D1, 'AI'). % So   [4] BULLSEYE..CIRCLE WITH RIGHT HALF BLACK
unicode_line_break(0x25E2, 0x25E5, 'AI'). % So   [4] BLACK LOWER RIGHT TRIANGLE..BLACK UPPER RIGHT TRIANGLE
unicode_line_break(0x25EF, 0x25EF, 'AI'). % So       LARGE CIRCLE
unicode_line_break(0x2605, 0x2606, 'AI'). % So   [2] BLACK STAR..WHITE STAR
unicode_line_break(0x2609, 0x2609, 'AI'). % So       SUN
unicode_line_break(0x260E, 0x260F, 'AI'). % So   [2] BLACK TELEPHONE..WHITE TELEPHONE
unicode_line_break(0x2614, 0x2617, 'AI'). % So   [4] UMBRELLA WITH RAIN DROPS..BLACK SHOGI PIECE
unicode_line_break(0x261C, 0x261C, 'AI'). % So       WHITE LEFT POINTING INDEX
unicode_line_break(0x261E, 0x261E, 'AI'). % So       WHITE RIGHT POINTING INDEX
unicode_line_break(0x2640, 0x2640, 'AI'). % So       FEMALE SIGN
unicode_line_break(0x2642, 0x2642, 'AI'). % So       MALE SIGN
unicode_line_break(0x2660, 0x2661, 'AI'). % So   [2] BLACK SPADE SUIT..WHITE HEART SUIT
unicode_line_break(0x2663, 0x2665, 'AI'). % So   [3] BLACK CLUB SUIT..BLACK HEART SUIT
unicode_line_break(0x2667, 0x266A, 'AI'). % So   [4] WHITE CLUB SUIT..EIGHTH NOTE
unicode_line_break(0x266C, 0x266D, 'AI'). % So   [2] BEAMED SIXTEENTH NOTES..MUSIC FLAT SIGN
unicode_line_break(0x266F, 0x266F, 'AI'). % Sm       MUSIC SHARP SIGN
unicode_line_break(0x269E, 0x269F, 'AI'). % So   [2] THREE LINES CONVERGING RIGHT..THREE LINES CONVERGING LEFT
unicode_line_break(0x26BE, 0x26BF, 'AI'). % So   [2] BASEBALL..SQUARED KEY
unicode_line_break(0x26C4, 0x26CD, 'AI'). % So  [10] SNOWMAN WITHOUT SNOW..DISABLED CAR
unicode_line_break(0x26CF, 0x26E1, 'AI'). % So  [19] PICK..RESTRICTED LEFT ENTRY-2
unicode_line_break(0x26E3, 0x26E3, 'AI'). % So       HEAVY CIRCLE WITH STROKE AND TWO DOTS ABOVE
unicode_line_break(0x26E8, 0x26FF, 'AI'). % So  [24] BLACK CROSS ON SHIELD..WHITE FLAG WITH HORIZONTAL MIDDLE BLACK STRIPE
unicode_line_break(0x2757, 0x2757, 'AI'). % So       HEAVY EXCLAMATION MARK SYMBOL
unicode_line_break(0x2776, 0x2793, 'AI'). % No  [30] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
unicode_line_break(0x2B55, 0x2B59, 'AI'). % So   [5] HEAVY LARGE CIRCLE..HEAVY CIRCLED SALTIRE
unicode_line_break(0x3248, 0x324F, 'AI'). % No   [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
unicode_line_break(0xFFFD, 0xFFFD, 'AI'). % So       REPLACEMENT CHARACTER
unicode_line_break(0x1F100, 0x1F10A, 'AI'). % No  [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
unicode_line_break(0x1F110, 0x1F12D, 'AI'). % So  [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
unicode_line_break(0x1F130, 0x1F169, 'AI'). % So  [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
unicode_line_break(0x1F170, 0x1F19A, 'AI'). % So  [43] NEGATIVE SQUARED LATIN CAPITAL LETTER A..SQUARED VS

% Total code points: 724

% ================================================

% Line_Break=Break_Both

unicode_line_break(0x2014, 0x2014, 'B2'). % Pd       EM DASH
unicode_line_break(0x2E3A, 0x2E3B, 'B2'). % Pd   [2] TWO-EM DASH..THREE-EM DASH

% Total code points: 3

% ================================================

% Line_Break=Surrogate

unicode_line_break(0xD800, 0xDFFF, 'SG'). % Cs [2048] <surrogate-D800>..<surrogate-DFFF>

% Total code points: 2048

% ================================================

% Line_Break=ZWSpace

unicode_line_break(0x200B, 0x200B, 'ZW'). % Cf       ZERO WIDTH SPACE

% Total code points: 1

% ================================================

% Line_Break=Next_Line

unicode_line_break(0x0085, 0x0085, 'NL'). % Cc       <control-0085>

% Total code points: 1

% ================================================

% Line_Break=Word_Joiner

unicode_line_break(0x2060, 0x2060, 'WJ'). % Cf       WORD JOINER
unicode_line_break(0xFEFF, 0xFEFF, 'WJ'). % Cf       ZERO WIDTH NO-BREAK SPACE

% Total code points: 2

% ================================================

% Line_Break=JL

unicode_line_break(0x1100, 0x115F, 'JL'). % Lo  [96] HANGUL CHOSEONG KIYEOK..HANGUL CHOSEONG FILLER
unicode_line_break(0xA960, 0xA97C, 'JL'). % Lo  [29] HANGUL CHOSEONG TIKEUT-MIEUM..HANGUL CHOSEONG SSANGYEORINHIEUH

% Total code points: 125

% ================================================

% Line_Break=JV

unicode_line_break(0x1160, 0x11A7, 'JV'). % Lo  [72] HANGUL JUNGSEONG FILLER..HANGUL JUNGSEONG O-YAE
unicode_line_break(0xD7B0, 0xD7C6, 'JV'). % Lo  [23] HANGUL JUNGSEONG O-YEO..HANGUL JUNGSEONG ARAEA-E

% Total code points: 95

% ================================================

% Line_Break=JT

unicode_line_break(0x11A8, 0x11FF, 'JT'). % Lo  [88] HANGUL JONGSEONG KIYEOK..HANGUL JONGSEONG SSANGNIEUN
unicode_line_break(0xD7CB, 0xD7FB, 'JT'). % Lo  [49] HANGUL JONGSEONG NIEUN-RIEUL..HANGUL JONGSEONG PHIEUPH-THIEUTH

% Total code points: 137

% ================================================

% Line_Break=H2

unicode_line_break(0xAC00, 0xAC00 , 'H2'). % Lo       HANGUL SYLLABLE GA
unicode_line_break(0xAC1C, 0xAC1C , 'H2'). % Lo       HANGUL SYLLABLE GAE
unicode_line_break(0xAC38, 0xAC38 , 'H2'). % Lo       HANGUL SYLLABLE GYA
unicode_line_break(0xAC54, 0xAC54 , 'H2'). % Lo       HANGUL SYLLABLE GYAE
unicode_line_break(0xAC70, 0xAC70 , 'H2'). % Lo       HANGUL SYLLABLE GEO
unicode_line_break(0xAC8C, 0xAC8C , 'H2'). % Lo       HANGUL SYLLABLE GE
unicode_line_break(0xACA8, 0xACA8 , 'H2'). % Lo       HANGUL SYLLABLE GYEO
unicode_line_break(0xACC4, 0xACC4 , 'H2'). % Lo       HANGUL SYLLABLE GYE
unicode_line_break(0xACE0, 0xACE0 , 'H2'). % Lo       HANGUL SYLLABLE GO
unicode_line_break(0xACFC, 0xACFC , 'H2'). % Lo       HANGUL SYLLABLE GWA
unicode_line_break(0xAD18, 0xAD18 , 'H2'). % Lo       HANGUL SYLLABLE GWAE
unicode_line_break(0xAD34, 0xAD34 , 'H2'). % Lo       HANGUL SYLLABLE GOE
unicode_line_break(0xAD50, 0xAD50 , 'H2'). % Lo       HANGUL SYLLABLE GYO
unicode_line_break(0xAD6C, 0xAD6C , 'H2'). % Lo       HANGUL SYLLABLE GU
unicode_line_break(0xAD88, 0xAD88 , 'H2'). % Lo       HANGUL SYLLABLE GWEO
unicode_line_break(0xADA4, 0xADA4 , 'H2'). % Lo       HANGUL SYLLABLE GWE
unicode_line_break(0xADC0, 0xADC0 , 'H2'). % Lo       HANGUL SYLLABLE GWI
unicode_line_break(0xADDC, 0xADDC , 'H2'). % Lo       HANGUL SYLLABLE GYU
unicode_line_break(0xADF8, 0xADF8 , 'H2'). % Lo       HANGUL SYLLABLE GEU
unicode_line_break(0xAE14, 0xAE14 , 'H2'). % Lo       HANGUL SYLLABLE GYI
unicode_line_break(0xAE30, 0xAE30 , 'H2'). % Lo       HANGUL SYLLABLE GI
unicode_line_break(0xAE4C, 0xAE4C , 'H2'). % Lo       HANGUL SYLLABLE GGA
unicode_line_break(0xAE68, 0xAE68 , 'H2'). % Lo       HANGUL SYLLABLE GGAE
unicode_line_break(0xAE84, 0xAE84 , 'H2'). % Lo       HANGUL SYLLABLE GGYA
unicode_line_break(0xAEA0, 0xAEA0 , 'H2'). % Lo       HANGUL SYLLABLE GGYAE
unicode_line_break(0xAEBC, 0xAEBC , 'H2'). % Lo       HANGUL SYLLABLE GGEO
unicode_line_break(0xAED8, 0xAED8 , 'H2'). % Lo       HANGUL SYLLABLE GGE
unicode_line_break(0xAEF4, 0xAEF4 , 'H2'). % Lo       HANGUL SYLLABLE GGYEO
unicode_line_break(0xAF10, 0xAF10 , 'H2'). % Lo       HANGUL SYLLABLE GGYE
unicode_line_break(0xAF2C, 0xAF2C , 'H2'). % Lo       HANGUL SYLLABLE GGO
unicode_line_break(0xAF48, 0xAF48 , 'H2'). % Lo       HANGUL SYLLABLE GGWA
unicode_line_break(0xAF64, 0xAF64 , 'H2'). % Lo       HANGUL SYLLABLE GGWAE
unicode_line_break(0xAF80, 0xAF80 , 'H2'). % Lo       HANGUL SYLLABLE GGOE
unicode_line_break(0xAF9C, 0xAF9C , 'H2'). % Lo       HANGUL SYLLABLE GGYO
unicode_line_break(0xAFB8, 0xAFB8 , 'H2'). % Lo       HANGUL SYLLABLE GGU
unicode_line_break(0xAFD4, 0xAFD4 , 'H2'). % Lo       HANGUL SYLLABLE GGWEO
unicode_line_break(0xAFF0, 0xAFF0 , 'H2'). % Lo       HANGUL SYLLABLE GGWE
unicode_line_break(0xB00C, 0xB00C , 'H2'). % Lo       HANGUL SYLLABLE GGWI
unicode_line_break(0xB028, 0xB028 , 'H2'). % Lo       HANGUL SYLLABLE GGYU
unicode_line_break(0xB044, 0xB044 , 'H2'). % Lo       HANGUL SYLLABLE GGEU
unicode_line_break(0xB060, 0xB060 , 'H2'). % Lo       HANGUL SYLLABLE GGYI
unicode_line_break(0xB07C, 0xB07C , 'H2'). % Lo       HANGUL SYLLABLE GGI
unicode_line_break(0xB098, 0xB098 , 'H2'). % Lo       HANGUL SYLLABLE NA
unicode_line_break(0xB0B4, 0xB0B4 , 'H2'). % Lo       HANGUL SYLLABLE NAE
unicode_line_break(0xB0D0, 0xB0D0 , 'H2'). % Lo       HANGUL SYLLABLE NYA
unicode_line_break(0xB0EC, 0xB0EC , 'H2'). % Lo       HANGUL SYLLABLE NYAE
unicode_line_break(0xB108, 0xB108 , 'H2'). % Lo       HANGUL SYLLABLE NEO
unicode_line_break(0xB124, 0xB124 , 'H2'). % Lo       HANGUL SYLLABLE NE
unicode_line_break(0xB140, 0xB140 , 'H2'). % Lo       HANGUL SYLLABLE NYEO
unicode_line_break(0xB15C, 0xB15C , 'H2'). % Lo       HANGUL SYLLABLE NYE
unicode_line_break(0xB178, 0xB178 , 'H2'). % Lo       HANGUL SYLLABLE NO
unicode_line_break(0xB194, 0xB194 , 'H2'). % Lo       HANGUL SYLLABLE NWA
unicode_line_break(0xB1B0, 0xB1B0 , 'H2'). % Lo       HANGUL SYLLABLE NWAE
unicode_line_break(0xB1CC, 0xB1CC , 'H2'). % Lo       HANGUL SYLLABLE NOE
unicode_line_break(0xB1E8, 0xB1E8 , 'H2'). % Lo       HANGUL SYLLABLE NYO
unicode_line_break(0xB204, 0xB204 , 'H2'). % Lo       HANGUL SYLLABLE NU
unicode_line_break(0xB220, 0xB220 , 'H2'). % Lo       HANGUL SYLLABLE NWEO
unicode_line_break(0xB23C, 0xB23C , 'H2'). % Lo       HANGUL SYLLABLE NWE
unicode_line_break(0xB258, 0xB258 , 'H2'). % Lo       HANGUL SYLLABLE NWI
unicode_line_break(0xB274, 0xB274 , 'H2'). % Lo       HANGUL SYLLABLE NYU
unicode_line_break(0xB290, 0xB290 , 'H2'). % Lo       HANGUL SYLLABLE NEU
unicode_line_break(0xB2AC, 0xB2AC , 'H2'). % Lo       HANGUL SYLLABLE NYI
unicode_line_break(0xB2C8, 0xB2C8 , 'H2'). % Lo       HANGUL SYLLABLE NI
unicode_line_break(0xB2E4, 0xB2E4 , 'H2'). % Lo       HANGUL SYLLABLE DA
unicode_line_break(0xB300, 0xB300 , 'H2'). % Lo       HANGUL SYLLABLE DAE
unicode_line_break(0xB31C, 0xB31C , 'H2'). % Lo       HANGUL SYLLABLE DYA
unicode_line_break(0xB338, 0xB338 , 'H2'). % Lo       HANGUL SYLLABLE DYAE
unicode_line_break(0xB354, 0xB354 , 'H2'). % Lo       HANGUL SYLLABLE DEO
unicode_line_break(0xB370, 0xB370 , 'H2'). % Lo       HANGUL SYLLABLE DE
unicode_line_break(0xB38C, 0xB38C , 'H2'). % Lo       HANGUL SYLLABLE DYEO
unicode_line_break(0xB3A8, 0xB3A8 , 'H2'). % Lo       HANGUL SYLLABLE DYE
unicode_line_break(0xB3C4, 0xB3C4 , 'H2'). % Lo       HANGUL SYLLABLE DO
unicode_line_break(0xB3E0, 0xB3E0 , 'H2'). % Lo       HANGUL SYLLABLE DWA
unicode_line_break(0xB3FC, 0xB3FC , 'H2'). % Lo       HANGUL SYLLABLE DWAE
unicode_line_break(0xB418, 0xB418 , 'H2'). % Lo       HANGUL SYLLABLE DOE
unicode_line_break(0xB434, 0xB434 , 'H2'). % Lo       HANGUL SYLLABLE DYO
unicode_line_break(0xB450, 0xB450 , 'H2'). % Lo       HANGUL SYLLABLE DU
unicode_line_break(0xB46C, 0xB46C , 'H2'). % Lo       HANGUL SYLLABLE DWEO
unicode_line_break(0xB488, 0xB488 , 'H2'). % Lo       HANGUL SYLLABLE DWE
unicode_line_break(0xB4A4, 0xB4A4 , 'H2'). % Lo       HANGUL SYLLABLE DWI
unicode_line_break(0xB4C0, 0xB4C0 , 'H2'). % Lo       HANGUL SYLLABLE DYU
unicode_line_break(0xB4DC, 0xB4DC , 'H2'). % Lo       HANGUL SYLLABLE DEU
unicode_line_break(0xB4F8, 0xB4F8 , 'H2'). % Lo       HANGUL SYLLABLE DYI
unicode_line_break(0xB514, 0xB514 , 'H2'). % Lo       HANGUL SYLLABLE DI
unicode_line_break(0xB530, 0xB530 , 'H2'). % Lo       HANGUL SYLLABLE DDA
unicode_line_break(0xB54C, 0xB54C , 'H2'). % Lo       HANGUL SYLLABLE DDAE
unicode_line_break(0xB568, 0xB568 , 'H2'). % Lo       HANGUL SYLLABLE DDYA
unicode_line_break(0xB584, 0xB584 , 'H2'). % Lo       HANGUL SYLLABLE DDYAE
unicode_line_break(0xB5A0, 0xB5A0 , 'H2'). % Lo       HANGUL SYLLABLE DDEO
unicode_line_break(0xB5BC, 0xB5BC , 'H2'). % Lo       HANGUL SYLLABLE DDE
unicode_line_break(0xB5D8, 0xB5D8 , 'H2'). % Lo       HANGUL SYLLABLE DDYEO
unicode_line_break(0xB5F4, 0xB5F4 , 'H2'). % Lo       HANGUL SYLLABLE DDYE
unicode_line_break(0xB610, 0xB610 , 'H2'). % Lo       HANGUL SYLLABLE DDO
unicode_line_break(0xB62C, 0xB62C , 'H2'). % Lo       HANGUL SYLLABLE DDWA
unicode_line_break(0xB648, 0xB648 , 'H2'). % Lo       HANGUL SYLLABLE DDWAE
unicode_line_break(0xB664, 0xB664 , 'H2'). % Lo       HANGUL SYLLABLE DDOE
unicode_line_break(0xB680, 0xB680 , 'H2'). % Lo       HANGUL SYLLABLE DDYO
unicode_line_break(0xB69C, 0xB69C , 'H2'). % Lo       HANGUL SYLLABLE DDU
unicode_line_break(0xB6B8, 0xB6B8 , 'H2'). % Lo       HANGUL SYLLABLE DDWEO
unicode_line_break(0xB6D4, 0xB6D4 , 'H2'). % Lo       HANGUL SYLLABLE DDWE
unicode_line_break(0xB6F0, 0xB6F0 , 'H2'). % Lo       HANGUL SYLLABLE DDWI
unicode_line_break(0xB70C, 0xB70C , 'H2'). % Lo       HANGUL SYLLABLE DDYU
unicode_line_break(0xB728, 0xB728 , 'H2'). % Lo       HANGUL SYLLABLE DDEU
unicode_line_break(0xB744, 0xB744 , 'H2'). % Lo       HANGUL SYLLABLE DDYI
unicode_line_break(0xB760, 0xB760 , 'H2'). % Lo       HANGUL SYLLABLE DDI
unicode_line_break(0xB77C, 0xB77C , 'H2'). % Lo       HANGUL SYLLABLE RA
unicode_line_break(0xB798, 0xB798 , 'H2'). % Lo       HANGUL SYLLABLE RAE
unicode_line_break(0xB7B4, 0xB7B4 , 'H2'). % Lo       HANGUL SYLLABLE RYA
unicode_line_break(0xB7D0, 0xB7D0 , 'H2'). % Lo       HANGUL SYLLABLE RYAE
unicode_line_break(0xB7EC, 0xB7EC , 'H2'). % Lo       HANGUL SYLLABLE REO
unicode_line_break(0xB808, 0xB808 , 'H2'). % Lo       HANGUL SYLLABLE RE
unicode_line_break(0xB824, 0xB824 , 'H2'). % Lo       HANGUL SYLLABLE RYEO
unicode_line_break(0xB840, 0xB840 , 'H2'). % Lo       HANGUL SYLLABLE RYE
unicode_line_break(0xB85C, 0xB85C , 'H2'). % Lo       HANGUL SYLLABLE RO
unicode_line_break(0xB878, 0xB878 , 'H2'). % Lo       HANGUL SYLLABLE RWA
unicode_line_break(0xB894, 0xB894 , 'H2'). % Lo       HANGUL SYLLABLE RWAE
unicode_line_break(0xB8B0, 0xB8B0 , 'H2'). % Lo       HANGUL SYLLABLE ROE
unicode_line_break(0xB8CC, 0xB8CC , 'H2'). % Lo       HANGUL SYLLABLE RYO
unicode_line_break(0xB8E8, 0xB8E8 , 'H2'). % Lo       HANGUL SYLLABLE RU
unicode_line_break(0xB904, 0xB904 , 'H2'). % Lo       HANGUL SYLLABLE RWEO
unicode_line_break(0xB920, 0xB920 , 'H2'). % Lo       HANGUL SYLLABLE RWE
unicode_line_break(0xB93C, 0xB93C , 'H2'). % Lo       HANGUL SYLLABLE RWI
unicode_line_break(0xB958, 0xB958 , 'H2'). % Lo       HANGUL SYLLABLE RYU
unicode_line_break(0xB974, 0xB974 , 'H2'). % Lo       HANGUL SYLLABLE REU
unicode_line_break(0xB990, 0xB990 , 'H2'). % Lo       HANGUL SYLLABLE RYI
unicode_line_break(0xB9AC, 0xB9AC , 'H2'). % Lo       HANGUL SYLLABLE RI
unicode_line_break(0xB9C8, 0xB9C8 , 'H2'). % Lo       HANGUL SYLLABLE MA
unicode_line_break(0xB9E4, 0xB9E4 , 'H2'). % Lo       HANGUL SYLLABLE MAE
unicode_line_break(0xBA00, 0xBA00 , 'H2'). % Lo       HANGUL SYLLABLE MYA
unicode_line_break(0xBA1C, 0xBA1C , 'H2'). % Lo       HANGUL SYLLABLE MYAE
unicode_line_break(0xBA38, 0xBA38 , 'H2'). % Lo       HANGUL SYLLABLE MEO
unicode_line_break(0xBA54, 0xBA54 , 'H2'). % Lo       HANGUL SYLLABLE ME
unicode_line_break(0xBA70, 0xBA70 , 'H2'). % Lo       HANGUL SYLLABLE MYEO
unicode_line_break(0xBA8C, 0xBA8C , 'H2'). % Lo       HANGUL SYLLABLE MYE
unicode_line_break(0xBAA8, 0xBAA8 , 'H2'). % Lo       HANGUL SYLLABLE MO
unicode_line_break(0xBAC4, 0xBAC4 , 'H2'). % Lo       HANGUL SYLLABLE MWA
unicode_line_break(0xBAE0, 0xBAE0 , 'H2'). % Lo       HANGUL SYLLABLE MWAE
unicode_line_break(0xBAFC, 0xBAFC , 'H2'). % Lo       HANGUL SYLLABLE MOE
unicode_line_break(0xBB18, 0xBB18 , 'H2'). % Lo       HANGUL SYLLABLE MYO
unicode_line_break(0xBB34, 0xBB34 , 'H2'). % Lo       HANGUL SYLLABLE MU
unicode_line_break(0xBB50, 0xBB50 , 'H2'). % Lo       HANGUL SYLLABLE MWEO
unicode_line_break(0xBB6C, 0xBB6C , 'H2'). % Lo       HANGUL SYLLABLE MWE
unicode_line_break(0xBB88, 0xBB88 , 'H2'). % Lo       HANGUL SYLLABLE MWI
unicode_line_break(0xBBA4, 0xBBA4 , 'H2'). % Lo       HANGUL SYLLABLE MYU
unicode_line_break(0xBBC0, 0xBBC0 , 'H2'). % Lo       HANGUL SYLLABLE MEU
unicode_line_break(0xBBDC, 0xBBDC , 'H2'). % Lo       HANGUL SYLLABLE MYI
unicode_line_break(0xBBF8, 0xBBF8 , 'H2'). % Lo       HANGUL SYLLABLE MI
unicode_line_break(0xBC14, 0xBC14 , 'H2'). % Lo       HANGUL SYLLABLE BA
unicode_line_break(0xBC30, 0xBC30 , 'H2'). % Lo       HANGUL SYLLABLE BAE
unicode_line_break(0xBC4C, 0xBC4C , 'H2'). % Lo       HANGUL SYLLABLE BYA
unicode_line_break(0xBC68, 0xBC68 , 'H2'). % Lo       HANGUL SYLLABLE BYAE
unicode_line_break(0xBC84, 0xBC84 , 'H2'). % Lo       HANGUL SYLLABLE BEO
unicode_line_break(0xBCA0, 0xBCA0 , 'H2'). % Lo       HANGUL SYLLABLE BE
unicode_line_break(0xBCBC, 0xBCBC , 'H2'). % Lo       HANGUL SYLLABLE BYEO
unicode_line_break(0xBCD8, 0xBCD8 , 'H2'). % Lo       HANGUL SYLLABLE BYE
unicode_line_break(0xBCF4, 0xBCF4 , 'H2'). % Lo       HANGUL SYLLABLE BO
unicode_line_break(0xBD10, 0xBD10 , 'H2'). % Lo       HANGUL SYLLABLE BWA
unicode_line_break(0xBD2C, 0xBD2C , 'H2'). % Lo       HANGUL SYLLABLE BWAE
unicode_line_break(0xBD48, 0xBD48 , 'H2'). % Lo       HANGUL SYLLABLE BOE
unicode_line_break(0xBD64, 0xBD64 , 'H2'). % Lo       HANGUL SYLLABLE BYO
unicode_line_break(0xBD80, 0xBD80 , 'H2'). % Lo       HANGUL SYLLABLE BU
unicode_line_break(0xBD9C, 0xBD9C , 'H2'). % Lo       HANGUL SYLLABLE BWEO
unicode_line_break(0xBDB8, 0xBDB8 , 'H2'). % Lo       HANGUL SYLLABLE BWE
unicode_line_break(0xBDD4, 0xBDD4 , 'H2'). % Lo       HANGUL SYLLABLE BWI
unicode_line_break(0xBDF0, 0xBDF0 , 'H2'). % Lo       HANGUL SYLLABLE BYU
unicode_line_break(0xBE0C, 0xBE0C , 'H2'). % Lo       HANGUL SYLLABLE BEU
unicode_line_break(0xBE28, 0xBE28 , 'H2'). % Lo       HANGUL SYLLABLE BYI
unicode_line_break(0xBE44, 0xBE44 , 'H2'). % Lo       HANGUL SYLLABLE BI
unicode_line_break(0xBE60, 0xBE60 , 'H2'). % Lo       HANGUL SYLLABLE BBA
unicode_line_break(0xBE7C, 0xBE7C , 'H2'). % Lo       HANGUL SYLLABLE BBAE
unicode_line_break(0xBE98, 0xBE98 , 'H2'). % Lo       HANGUL SYLLABLE BBYA
unicode_line_break(0xBEB4, 0xBEB4 , 'H2'). % Lo       HANGUL SYLLABLE BBYAE
unicode_line_break(0xBED0, 0xBED0 , 'H2'). % Lo       HANGUL SYLLABLE BBEO
unicode_line_break(0xBEEC, 0xBEEC , 'H2'). % Lo       HANGUL SYLLABLE BBE
unicode_line_break(0xBF08, 0xBF08 , 'H2'). % Lo       HANGUL SYLLABLE BBYEO
unicode_line_break(0xBF24, 0xBF24 , 'H2'). % Lo       HANGUL SYLLABLE BBYE
unicode_line_break(0xBF40, 0xBF40 , 'H2'). % Lo       HANGUL SYLLABLE BBO
unicode_line_break(0xBF5C, 0xBF5C , 'H2'). % Lo       HANGUL SYLLABLE BBWA
unicode_line_break(0xBF78, 0xBF78 , 'H2'). % Lo       HANGUL SYLLABLE BBWAE
unicode_line_break(0xBF94, 0xBF94 , 'H2'). % Lo       HANGUL SYLLABLE BBOE
unicode_line_break(0xBFB0, 0xBFB0 , 'H2'). % Lo       HANGUL SYLLABLE BBYO
unicode_line_break(0xBFCC, 0xBFCC , 'H2'). % Lo       HANGUL SYLLABLE BBU
unicode_line_break(0xBFE8, 0xBFE8 , 'H2'). % Lo       HANGUL SYLLABLE BBWEO
unicode_line_break(0xC004, 0xC004 , 'H2'). % Lo       HANGUL SYLLABLE BBWE
unicode_line_break(0xC020, 0xC020 , 'H2'). % Lo       HANGUL SYLLABLE BBWI
unicode_line_break(0xC03C, 0xC03C , 'H2'). % Lo       HANGUL SYLLABLE BBYU
unicode_line_break(0xC058, 0xC058 , 'H2'). % Lo       HANGUL SYLLABLE BBEU
unicode_line_break(0xC074, 0xC074 , 'H2'). % Lo       HANGUL SYLLABLE BBYI
unicode_line_break(0xC090, 0xC090 , 'H2'). % Lo       HANGUL SYLLABLE BBI
unicode_line_break(0xC0AC, 0xC0AC , 'H2'). % Lo       HANGUL SYLLABLE SA
unicode_line_break(0xC0C8, 0xC0C8 , 'H2'). % Lo       HANGUL SYLLABLE SAE
unicode_line_break(0xC0E4, 0xC0E4 , 'H2'). % Lo       HANGUL SYLLABLE SYA
unicode_line_break(0xC100, 0xC100 , 'H2'). % Lo       HANGUL SYLLABLE SYAE
unicode_line_break(0xC11C, 0xC11C , 'H2'). % Lo       HANGUL SYLLABLE SEO
unicode_line_break(0xC138, 0xC138 , 'H2'). % Lo       HANGUL SYLLABLE SE
unicode_line_break(0xC154, 0xC154 , 'H2'). % Lo       HANGUL SYLLABLE SYEO
unicode_line_break(0xC170, 0xC170 , 'H2'). % Lo       HANGUL SYLLABLE SYE
unicode_line_break(0xC18C, 0xC18C , 'H2'). % Lo       HANGUL SYLLABLE SO
unicode_line_break(0xC1A8, 0xC1A8 , 'H2'). % Lo       HANGUL SYLLABLE SWA
unicode_line_break(0xC1C4, 0xC1C4 , 'H2'). % Lo       HANGUL SYLLABLE SWAE
unicode_line_break(0xC1E0, 0xC1E0 , 'H2'). % Lo       HANGUL SYLLABLE SOE
unicode_line_break(0xC1FC, 0xC1FC , 'H2'). % Lo       HANGUL SYLLABLE SYO
unicode_line_break(0xC218, 0xC218 , 'H2'). % Lo       HANGUL SYLLABLE SU
unicode_line_break(0xC234, 0xC234 , 'H2'). % Lo       HANGUL SYLLABLE SWEO
unicode_line_break(0xC250, 0xC250 , 'H2'). % Lo       HANGUL SYLLABLE SWE
unicode_line_break(0xC26C, 0xC26C , 'H2'). % Lo       HANGUL SYLLABLE SWI
unicode_line_break(0xC288, 0xC288 , 'H2'). % Lo       HANGUL SYLLABLE SYU
unicode_line_break(0xC2A4, 0xC2A4 , 'H2'). % Lo       HANGUL SYLLABLE SEU
unicode_line_break(0xC2C0, 0xC2C0 , 'H2'). % Lo       HANGUL SYLLABLE SYI
unicode_line_break(0xC2DC, 0xC2DC , 'H2'). % Lo       HANGUL SYLLABLE SI
unicode_line_break(0xC2F8, 0xC2F8 , 'H2'). % Lo       HANGUL SYLLABLE SSA
unicode_line_break(0xC314, 0xC314 , 'H2'). % Lo       HANGUL SYLLABLE SSAE
unicode_line_break(0xC330, 0xC330 , 'H2'). % Lo       HANGUL SYLLABLE SSYA
unicode_line_break(0xC34C, 0xC34C , 'H2'). % Lo       HANGUL SYLLABLE SSYAE
unicode_line_break(0xC368, 0xC368 , 'H2'). % Lo       HANGUL SYLLABLE SSEO
unicode_line_break(0xC384, 0xC384 , 'H2'). % Lo       HANGUL SYLLABLE SSE
unicode_line_break(0xC3A0, 0xC3A0 , 'H2'). % Lo       HANGUL SYLLABLE SSYEO
unicode_line_break(0xC3BC, 0xC3BC , 'H2'). % Lo       HANGUL SYLLABLE SSYE
unicode_line_break(0xC3D8, 0xC3D8 , 'H2'). % Lo       HANGUL SYLLABLE SSO
unicode_line_break(0xC3F4, 0xC3F4 , 'H2'). % Lo       HANGUL SYLLABLE SSWA
unicode_line_break(0xC410, 0xC410 , 'H2'). % Lo       HANGUL SYLLABLE SSWAE
unicode_line_break(0xC42C, 0xC42C , 'H2'). % Lo       HANGUL SYLLABLE SSOE
unicode_line_break(0xC448, 0xC448 , 'H2'). % Lo       HANGUL SYLLABLE SSYO
unicode_line_break(0xC464, 0xC464 , 'H2'). % Lo       HANGUL SYLLABLE SSU
unicode_line_break(0xC480, 0xC480 , 'H2'). % Lo       HANGUL SYLLABLE SSWEO
unicode_line_break(0xC49C, 0xC49C , 'H2'). % Lo       HANGUL SYLLABLE SSWE
unicode_line_break(0xC4B8, 0xC4B8 , 'H2'). % Lo       HANGUL SYLLABLE SSWI
unicode_line_break(0xC4D4, 0xC4D4 , 'H2'). % Lo       HANGUL SYLLABLE SSYU
unicode_line_break(0xC4F0, 0xC4F0 , 'H2'). % Lo       HANGUL SYLLABLE SSEU
unicode_line_break(0xC50C, 0xC50C , 'H2'). % Lo       HANGUL SYLLABLE SSYI
unicode_line_break(0xC528, 0xC528 , 'H2'). % Lo       HANGUL SYLLABLE SSI
unicode_line_break(0xC544, 0xC544 , 'H2'). % Lo       HANGUL SYLLABLE A
unicode_line_break(0xC560, 0xC560 , 'H2'). % Lo       HANGUL SYLLABLE AE
unicode_line_break(0xC57C, 0xC57C , 'H2'). % Lo       HANGUL SYLLABLE YA
unicode_line_break(0xC598, 0xC598 , 'H2'). % Lo       HANGUL SYLLABLE YAE
unicode_line_break(0xC5B4, 0xC5B4 , 'H2'). % Lo       HANGUL SYLLABLE EO
unicode_line_break(0xC5D0, 0xC5D0 , 'H2'). % Lo       HANGUL SYLLABLE E
unicode_line_break(0xC5EC, 0xC5EC , 'H2'). % Lo       HANGUL SYLLABLE YEO
unicode_line_break(0xC608, 0xC608 , 'H2'). % Lo       HANGUL SYLLABLE YE
unicode_line_break(0xC624, 0xC624 , 'H2'). % Lo       HANGUL SYLLABLE O
unicode_line_break(0xC640, 0xC640 , 'H2'). % Lo       HANGUL SYLLABLE WA
unicode_line_break(0xC65C, 0xC65C , 'H2'). % Lo       HANGUL SYLLABLE WAE
unicode_line_break(0xC678, 0xC678 , 'H2'). % Lo       HANGUL SYLLABLE OE
unicode_line_break(0xC694, 0xC694 , 'H2'). % Lo       HANGUL SYLLABLE YO
unicode_line_break(0xC6B0, 0xC6B0 , 'H2'). % Lo       HANGUL SYLLABLE U
unicode_line_break(0xC6CC, 0xC6CC , 'H2'). % Lo       HANGUL SYLLABLE WEO
unicode_line_break(0xC6E8, 0xC6E8 , 'H2'). % Lo       HANGUL SYLLABLE WE
unicode_line_break(0xC704, 0xC704 , 'H2'). % Lo       HANGUL SYLLABLE WI
unicode_line_break(0xC720, 0xC720 , 'H2'). % Lo       HANGUL SYLLABLE YU
unicode_line_break(0xC73C, 0xC73C , 'H2'). % Lo       HANGUL SYLLABLE EU
unicode_line_break(0xC758, 0xC758 , 'H2'). % Lo       HANGUL SYLLABLE YI
unicode_line_break(0xC774, 0xC774 , 'H2'). % Lo       HANGUL SYLLABLE I
unicode_line_break(0xC790, 0xC790 , 'H2'). % Lo       HANGUL SYLLABLE JA
unicode_line_break(0xC7AC, 0xC7AC , 'H2'). % Lo       HANGUL SYLLABLE JAE
unicode_line_break(0xC7C8, 0xC7C8 , 'H2'). % Lo       HANGUL SYLLABLE JYA
unicode_line_break(0xC7E4, 0xC7E4 , 'H2'). % Lo       HANGUL SYLLABLE JYAE
unicode_line_break(0xC800, 0xC800 , 'H2'). % Lo       HANGUL SYLLABLE JEO
unicode_line_break(0xC81C, 0xC81C , 'H2'). % Lo       HANGUL SYLLABLE JE
unicode_line_break(0xC838, 0xC838 , 'H2'). % Lo       HANGUL SYLLABLE JYEO
unicode_line_break(0xC854, 0xC854 , 'H2'). % Lo       HANGUL SYLLABLE JYE
unicode_line_break(0xC870, 0xC870 , 'H2'). % Lo       HANGUL SYLLABLE JO
unicode_line_break(0xC88C, 0xC88C , 'H2'). % Lo       HANGUL SYLLABLE JWA
unicode_line_break(0xC8A8, 0xC8A8 , 'H2'). % Lo       HANGUL SYLLABLE JWAE
unicode_line_break(0xC8C4, 0xC8C4 , 'H2'). % Lo       HANGUL SYLLABLE JOE
unicode_line_break(0xC8E0, 0xC8E0 , 'H2'). % Lo       HANGUL SYLLABLE JYO
unicode_line_break(0xC8FC, 0xC8FC , 'H2'). % Lo       HANGUL SYLLABLE JU
unicode_line_break(0xC918, 0xC918 , 'H2'). % Lo       HANGUL SYLLABLE JWEO
unicode_line_break(0xC934, 0xC934 , 'H2'). % Lo       HANGUL SYLLABLE JWE
unicode_line_break(0xC950, 0xC950 , 'H2'). % Lo       HANGUL SYLLABLE JWI
unicode_line_break(0xC96C, 0xC96C , 'H2'). % Lo       HANGUL SYLLABLE JYU
unicode_line_break(0xC988, 0xC988 , 'H2'). % Lo       HANGUL SYLLABLE JEU
unicode_line_break(0xC9A4, 0xC9A4 , 'H2'). % Lo       HANGUL SYLLABLE JYI
unicode_line_break(0xC9C0, 0xC9C0 , 'H2'). % Lo       HANGUL SYLLABLE JI
unicode_line_break(0xC9DC, 0xC9DC , 'H2'). % Lo       HANGUL SYLLABLE JJA
unicode_line_break(0xC9F8, 0xC9F8 , 'H2'). % Lo       HANGUL SYLLABLE JJAE
unicode_line_break(0xCA14, 0xCA14 , 'H2'). % Lo       HANGUL SYLLABLE JJYA
unicode_line_break(0xCA30, 0xCA30 , 'H2'). % Lo       HANGUL SYLLABLE JJYAE
unicode_line_break(0xCA4C, 0xCA4C , 'H2'). % Lo       HANGUL SYLLABLE JJEO
unicode_line_break(0xCA68, 0xCA68 , 'H2'). % Lo       HANGUL SYLLABLE JJE
unicode_line_break(0xCA84, 0xCA84 , 'H2'). % Lo       HANGUL SYLLABLE JJYEO
unicode_line_break(0xCAA0, 0xCAA0 , 'H2'). % Lo       HANGUL SYLLABLE JJYE
unicode_line_break(0xCABC, 0xCABC , 'H2'). % Lo       HANGUL SYLLABLE JJO
unicode_line_break(0xCAD8, 0xCAD8 , 'H2'). % Lo       HANGUL SYLLABLE JJWA
unicode_line_break(0xCAF4, 0xCAF4 , 'H2'). % Lo       HANGUL SYLLABLE JJWAE
unicode_line_break(0xCB10, 0xCB10 , 'H2'). % Lo       HANGUL SYLLABLE JJOE
unicode_line_break(0xCB2C, 0xCB2C , 'H2'). % Lo       HANGUL SYLLABLE JJYO
unicode_line_break(0xCB48, 0xCB48 , 'H2'). % Lo       HANGUL SYLLABLE JJU
unicode_line_break(0xCB64, 0xCB64 , 'H2'). % Lo       HANGUL SYLLABLE JJWEO
unicode_line_break(0xCB80, 0xCB80 , 'H2'). % Lo       HANGUL SYLLABLE JJWE
unicode_line_break(0xCB9C, 0xCB9C , 'H2'). % Lo       HANGUL SYLLABLE JJWI
unicode_line_break(0xCBB8, 0xCBB8 , 'H2'). % Lo       HANGUL SYLLABLE JJYU
unicode_line_break(0xCBD4, 0xCBD4 , 'H2'). % Lo       HANGUL SYLLABLE JJEU
unicode_line_break(0xCBF0, 0xCBF0 , 'H2'). % Lo       HANGUL SYLLABLE JJYI
unicode_line_break(0xCC0C, 0xCC0C , 'H2'). % Lo       HANGUL SYLLABLE JJI
unicode_line_break(0xCC28, 0xCC28 , 'H2'). % Lo       HANGUL SYLLABLE CA
unicode_line_break(0xCC44, 0xCC44 , 'H2'). % Lo       HANGUL SYLLABLE CAE
unicode_line_break(0xCC60, 0xCC60 , 'H2'). % Lo       HANGUL SYLLABLE CYA
unicode_line_break(0xCC7C, 0xCC7C , 'H2'). % Lo       HANGUL SYLLABLE CYAE
unicode_line_break(0xCC98, 0xCC98 , 'H2'). % Lo       HANGUL SYLLABLE CEO
unicode_line_break(0xCCB4, 0xCCB4 , 'H2'). % Lo       HANGUL SYLLABLE CE
unicode_line_break(0xCCD0, 0xCCD0 , 'H2'). % Lo       HANGUL SYLLABLE CYEO
unicode_line_break(0xCCEC, 0xCCEC , 'H2'). % Lo       HANGUL SYLLABLE CYE
unicode_line_break(0xCD08, 0xCD08 , 'H2'). % Lo       HANGUL SYLLABLE CO
unicode_line_break(0xCD24, 0xCD24 , 'H2'). % Lo       HANGUL SYLLABLE CWA
unicode_line_break(0xCD40, 0xCD40 , 'H2'). % Lo       HANGUL SYLLABLE CWAE
unicode_line_break(0xCD5C, 0xCD5C , 'H2'). % Lo       HANGUL SYLLABLE COE
unicode_line_break(0xCD78, 0xCD78 , 'H2'). % Lo       HANGUL SYLLABLE CYO
unicode_line_break(0xCD94, 0xCD94 , 'H2'). % Lo       HANGUL SYLLABLE CU
unicode_line_break(0xCDB0, 0xCDB0 , 'H2'). % Lo       HANGUL SYLLABLE CWEO
unicode_line_break(0xCDCC, 0xCDCC , 'H2'). % Lo       HANGUL SYLLABLE CWE
unicode_line_break(0xCDE8, 0xCDE8 , 'H2'). % Lo       HANGUL SYLLABLE CWI
unicode_line_break(0xCE04, 0xCE04 , 'H2'). % Lo       HANGUL SYLLABLE CYU
unicode_line_break(0xCE20, 0xCE20 , 'H2'). % Lo       HANGUL SYLLABLE CEU
unicode_line_break(0xCE3C, 0xCE3C , 'H2'). % Lo       HANGUL SYLLABLE CYI
unicode_line_break(0xCE58, 0xCE58 , 'H2'). % Lo       HANGUL SYLLABLE CI
unicode_line_break(0xCE74, 0xCE74 , 'H2'). % Lo       HANGUL SYLLABLE KA
unicode_line_break(0xCE90, 0xCE90 , 'H2'). % Lo       HANGUL SYLLABLE KAE
unicode_line_break(0xCEAC, 0xCEAC , 'H2'). % Lo       HANGUL SYLLABLE KYA
unicode_line_break(0xCEC8, 0xCEC8 , 'H2'). % Lo       HANGUL SYLLABLE KYAE
unicode_line_break(0xCEE4, 0xCEE4 , 'H2'). % Lo       HANGUL SYLLABLE KEO
unicode_line_break(0xCF00, 0xCF00 , 'H2'). % Lo       HANGUL SYLLABLE KE
unicode_line_break(0xCF1C, 0xCF1C , 'H2'). % Lo       HANGUL SYLLABLE KYEO
unicode_line_break(0xCF38, 0xCF38 , 'H2'). % Lo       HANGUL SYLLABLE KYE
unicode_line_break(0xCF54, 0xCF54 , 'H2'). % Lo       HANGUL SYLLABLE KO
unicode_line_break(0xCF70, 0xCF70 , 'H2'). % Lo       HANGUL SYLLABLE KWA
unicode_line_break(0xCF8C, 0xCF8C , 'H2'). % Lo       HANGUL SYLLABLE KWAE
unicode_line_break(0xCFA8, 0xCFA8 , 'H2'). % Lo       HANGUL SYLLABLE KOE
unicode_line_break(0xCFC4, 0xCFC4 , 'H2'). % Lo       HANGUL SYLLABLE KYO
unicode_line_break(0xCFE0, 0xCFE0 , 'H2'). % Lo       HANGUL SYLLABLE KU
unicode_line_break(0xCFFC, 0xCFFC , 'H2'). % Lo       HANGUL SYLLABLE KWEO
unicode_line_break(0xD018, 0xD018 , 'H2'). % Lo       HANGUL SYLLABLE KWE
unicode_line_break(0xD034, 0xD034 , 'H2'). % Lo       HANGUL SYLLABLE KWI
unicode_line_break(0xD050, 0xD050 , 'H2'). % Lo       HANGUL SYLLABLE KYU
unicode_line_break(0xD06C, 0xD06C , 'H2'). % Lo       HANGUL SYLLABLE KEU
unicode_line_break(0xD088, 0xD088 , 'H2'). % Lo       HANGUL SYLLABLE KYI
unicode_line_break(0xD0A4, 0xD0A4 , 'H2'). % Lo       HANGUL SYLLABLE KI
unicode_line_break(0xD0C0, 0xD0C0 , 'H2'). % Lo       HANGUL SYLLABLE TA
unicode_line_break(0xD0DC, 0xD0DC , 'H2'). % Lo       HANGUL SYLLABLE TAE
unicode_line_break(0xD0F8, 0xD0F8 , 'H2'). % Lo       HANGUL SYLLABLE TYA
unicode_line_break(0xD114, 0xD114 , 'H2'). % Lo       HANGUL SYLLABLE TYAE
unicode_line_break(0xD130, 0xD130 , 'H2'). % Lo       HANGUL SYLLABLE TEO
unicode_line_break(0xD14C, 0xD14C , 'H2'). % Lo       HANGUL SYLLABLE TE
unicode_line_break(0xD168, 0xD168 , 'H2'). % Lo       HANGUL SYLLABLE TYEO
unicode_line_break(0xD184, 0xD184 , 'H2'). % Lo       HANGUL SYLLABLE TYE
unicode_line_break(0xD1A0, 0xD1A0 , 'H2'). % Lo       HANGUL SYLLABLE TO
unicode_line_break(0xD1BC, 0xD1BC , 'H2'). % Lo       HANGUL SYLLABLE TWA
unicode_line_break(0xD1D8, 0xD1D8 , 'H2'). % Lo       HANGUL SYLLABLE TWAE
unicode_line_break(0xD1F4, 0xD1F4 , 'H2'). % Lo       HANGUL SYLLABLE TOE
unicode_line_break(0xD210, 0xD210 , 'H2'). % Lo       HANGUL SYLLABLE TYO
unicode_line_break(0xD22C, 0xD22C , 'H2'). % Lo       HANGUL SYLLABLE TU
unicode_line_break(0xD248, 0xD248 , 'H2'). % Lo       HANGUL SYLLABLE TWEO
unicode_line_break(0xD264, 0xD264 , 'H2'). % Lo       HANGUL SYLLABLE TWE
unicode_line_break(0xD280, 0xD280 , 'H2'). % Lo       HANGUL SYLLABLE TWI
unicode_line_break(0xD29C, 0xD29C , 'H2'). % Lo       HANGUL SYLLABLE TYU
unicode_line_break(0xD2B8, 0xD2B8 , 'H2'). % Lo       HANGUL SYLLABLE TEU
unicode_line_break(0xD2D4, 0xD2D4 , 'H2'). % Lo       HANGUL SYLLABLE TYI
unicode_line_break(0xD2F0, 0xD2F0 , 'H2'). % Lo       HANGUL SYLLABLE TI
unicode_line_break(0xD30C, 0xD30C , 'H2'). % Lo       HANGUL SYLLABLE PA
unicode_line_break(0xD328, 0xD328 , 'H2'). % Lo       HANGUL SYLLABLE PAE
unicode_line_break(0xD344, 0xD344 , 'H2'). % Lo       HANGUL SYLLABLE PYA
unicode_line_break(0xD360, 0xD360 , 'H2'). % Lo       HANGUL SYLLABLE PYAE
unicode_line_break(0xD37C, 0xD37C , 'H2'). % Lo       HANGUL SYLLABLE PEO
unicode_line_break(0xD398, 0xD398 , 'H2'). % Lo       HANGUL SYLLABLE PE
unicode_line_break(0xD3B4, 0xD3B4 , 'H2'). % Lo       HANGUL SYLLABLE PYEO
unicode_line_break(0xD3D0, 0xD3D0 , 'H2'). % Lo       HANGUL SYLLABLE PYE
unicode_line_break(0xD3EC, 0xD3EC , 'H2'). % Lo       HANGUL SYLLABLE PO
unicode_line_break(0xD408, 0xD408 , 'H2'). % Lo       HANGUL SYLLABLE PWA
unicode_line_break(0xD424, 0xD424 , 'H2'). % Lo       HANGUL SYLLABLE PWAE
unicode_line_break(0xD440, 0xD440 , 'H2'). % Lo       HANGUL SYLLABLE POE
unicode_line_break(0xD45C, 0xD45C , 'H2'). % Lo       HANGUL SYLLABLE PYO
unicode_line_break(0xD478, 0xD478 , 'H2'). % Lo       HANGUL SYLLABLE PU
unicode_line_break(0xD494, 0xD494 , 'H2'). % Lo       HANGUL SYLLABLE PWEO
unicode_line_break(0xD4B0, 0xD4B0 , 'H2'). % Lo       HANGUL SYLLABLE PWE
unicode_line_break(0xD4CC, 0xD4CC , 'H2'). % Lo       HANGUL SYLLABLE PWI
unicode_line_break(0xD4E8, 0xD4E8 , 'H2'). % Lo       HANGUL SYLLABLE PYU
unicode_line_break(0xD504, 0xD504 , 'H2'). % Lo       HANGUL SYLLABLE PEU
unicode_line_break(0xD520, 0xD520 , 'H2'). % Lo       HANGUL SYLLABLE PYI
unicode_line_break(0xD53C, 0xD53C , 'H2'). % Lo       HANGUL SYLLABLE PI
unicode_line_break(0xD558, 0xD558 , 'H2'). % Lo       HANGUL SYLLABLE HA
unicode_line_break(0xD574, 0xD574 , 'H2'). % Lo       HANGUL SYLLABLE HAE
unicode_line_break(0xD590, 0xD590 , 'H2'). % Lo       HANGUL SYLLABLE HYA
unicode_line_break(0xD5AC, 0xD5AC , 'H2'). % Lo       HANGUL SYLLABLE HYAE
unicode_line_break(0xD5C8, 0xD5C8 , 'H2'). % Lo       HANGUL SYLLABLE HEO
unicode_line_break(0xD5E4, 0xD5E4 , 'H2'). % Lo       HANGUL SYLLABLE HE
unicode_line_break(0xD600, 0xD600 , 'H2'). % Lo       HANGUL SYLLABLE HYEO
unicode_line_break(0xD61C, 0xD61C , 'H2'). % Lo       HANGUL SYLLABLE HYE
unicode_line_break(0xD638, 0xD638 , 'H2'). % Lo       HANGUL SYLLABLE HO
unicode_line_break(0xD654, 0xD654 , 'H2'). % Lo       HANGUL SYLLABLE HWA
unicode_line_break(0xD670, 0xD670 , 'H2'). % Lo       HANGUL SYLLABLE HWAE
unicode_line_break(0xD68C, 0xD68C , 'H2'). % Lo       HANGUL SYLLABLE HOE
unicode_line_break(0xD6A8, 0xD6A8 , 'H2'). % Lo       HANGUL SYLLABLE HYO
unicode_line_break(0xD6C4, 0xD6C4 , 'H2'). % Lo       HANGUL SYLLABLE HU
unicode_line_break(0xD6E0, 0xD6E0 , 'H2'). % Lo       HANGUL SYLLABLE HWEO
unicode_line_break(0xD6FC, 0xD6FC , 'H2'). % Lo       HANGUL SYLLABLE HWE
unicode_line_break(0xD718, 0xD718 , 'H2'). % Lo       HANGUL SYLLABLE HWI
unicode_line_break(0xD734, 0xD734 , 'H2'). % Lo       HANGUL SYLLABLE HYU
unicode_line_break(0xD750, 0xD750 , 'H2'). % Lo       HANGUL SYLLABLE HEU
unicode_line_break(0xD76C, 0xD76C , 'H2'). % Lo       HANGUL SYLLABLE HYI
unicode_line_break(0xD788, 0xD788 , 'H2'). % Lo       HANGUL SYLLABLE HI

% Total code points: 399

% ================================================

% Line_Break=H3

unicode_line_break(0xAC01, 0xAC1B, 'H3'). % Lo  [27] HANGUL SYLLABLE GAG..HANGUL SYLLABLE GAH
unicode_line_break(0xAC1D, 0xAC37, 'H3'). % Lo  [27] HANGUL SYLLABLE GAEG..HANGUL SYLLABLE GAEH
unicode_line_break(0xAC39, 0xAC53, 'H3'). % Lo  [27] HANGUL SYLLABLE GYAG..HANGUL SYLLABLE GYAH
unicode_line_break(0xAC55, 0xAC6F, 'H3'). % Lo  [27] HANGUL SYLLABLE GYAEG..HANGUL SYLLABLE GYAEH
unicode_line_break(0xAC71, 0xAC8B, 'H3'). % Lo  [27] HANGUL SYLLABLE GEOG..HANGUL SYLLABLE GEOH
unicode_line_break(0xAC8D, 0xACA7, 'H3'). % Lo  [27] HANGUL SYLLABLE GEG..HANGUL SYLLABLE GEH
unicode_line_break(0xACA9, 0xACC3, 'H3'). % Lo  [27] HANGUL SYLLABLE GYEOG..HANGUL SYLLABLE GYEOH
unicode_line_break(0xACC5, 0xACDF, 'H3'). % Lo  [27] HANGUL SYLLABLE GYEG..HANGUL SYLLABLE GYEH
unicode_line_break(0xACE1, 0xACFB, 'H3'). % Lo  [27] HANGUL SYLLABLE GOG..HANGUL SYLLABLE GOH
unicode_line_break(0xACFD, 0xAD17, 'H3'). % Lo  [27] HANGUL SYLLABLE GWAG..HANGUL SYLLABLE GWAH
unicode_line_break(0xAD19, 0xAD33, 'H3'). % Lo  [27] HANGUL SYLLABLE GWAEG..HANGUL SYLLABLE GWAEH
unicode_line_break(0xAD35, 0xAD4F, 'H3'). % Lo  [27] HANGUL SYLLABLE GOEG..HANGUL SYLLABLE GOEH
unicode_line_break(0xAD51, 0xAD6B, 'H3'). % Lo  [27] HANGUL SYLLABLE GYOG..HANGUL SYLLABLE GYOH
unicode_line_break(0xAD6D, 0xAD87, 'H3'). % Lo  [27] HANGUL SYLLABLE GUG..HANGUL SYLLABLE GUH
unicode_line_break(0xAD89, 0xADA3, 'H3'). % Lo  [27] HANGUL SYLLABLE GWEOG..HANGUL SYLLABLE GWEOH
unicode_line_break(0xADA5, 0xADBF, 'H3'). % Lo  [27] HANGUL SYLLABLE GWEG..HANGUL SYLLABLE GWEH
unicode_line_break(0xADC1, 0xADDB, 'H3'). % Lo  [27] HANGUL SYLLABLE GWIG..HANGUL SYLLABLE GWIH
unicode_line_break(0xADDD, 0xADF7, 'H3'). % Lo  [27] HANGUL SYLLABLE GYUG..HANGUL SYLLABLE GYUH
unicode_line_break(0xADF9, 0xAE13, 'H3'). % Lo  [27] HANGUL SYLLABLE GEUG..HANGUL SYLLABLE GEUH
unicode_line_break(0xAE15, 0xAE2F, 'H3'). % Lo  [27] HANGUL SYLLABLE GYIG..HANGUL SYLLABLE GYIH
unicode_line_break(0xAE31, 0xAE4B, 'H3'). % Lo  [27] HANGUL SYLLABLE GIG..HANGUL SYLLABLE GIH
unicode_line_break(0xAE4D, 0xAE67, 'H3'). % Lo  [27] HANGUL SYLLABLE GGAG..HANGUL SYLLABLE GGAH
unicode_line_break(0xAE69, 0xAE83, 'H3'). % Lo  [27] HANGUL SYLLABLE GGAEG..HANGUL SYLLABLE GGAEH
unicode_line_break(0xAE85, 0xAE9F, 'H3'). % Lo  [27] HANGUL SYLLABLE GGYAG..HANGUL SYLLABLE GGYAH
unicode_line_break(0xAEA1, 0xAEBB, 'H3'). % Lo  [27] HANGUL SYLLABLE GGYAEG..HANGUL SYLLABLE GGYAEH
unicode_line_break(0xAEBD, 0xAED7, 'H3'). % Lo  [27] HANGUL SYLLABLE GGEOG..HANGUL SYLLABLE GGEOH
unicode_line_break(0xAED9, 0xAEF3, 'H3'). % Lo  [27] HANGUL SYLLABLE GGEG..HANGUL SYLLABLE GGEH
unicode_line_break(0xAEF5, 0xAF0F, 'H3'). % Lo  [27] HANGUL SYLLABLE GGYEOG..HANGUL SYLLABLE GGYEOH
unicode_line_break(0xAF11, 0xAF2B, 'H3'). % Lo  [27] HANGUL SYLLABLE GGYEG..HANGUL SYLLABLE GGYEH
unicode_line_break(0xAF2D, 0xAF47, 'H3'). % Lo  [27] HANGUL SYLLABLE GGOG..HANGUL SYLLABLE GGOH
unicode_line_break(0xAF49, 0xAF63, 'H3'). % Lo  [27] HANGUL SYLLABLE GGWAG..HANGUL SYLLABLE GGWAH
unicode_line_break(0xAF65, 0xAF7F, 'H3'). % Lo  [27] HANGUL SYLLABLE GGWAEG..HANGUL SYLLABLE GGWAEH
unicode_line_break(0xAF81, 0xAF9B, 'H3'). % Lo  [27] HANGUL SYLLABLE GGOEG..HANGUL SYLLABLE GGOEH
unicode_line_break(0xAF9D, 0xAFB7, 'H3'). % Lo  [27] HANGUL SYLLABLE GGYOG..HANGUL SYLLABLE GGYOH
unicode_line_break(0xAFB9, 0xAFD3, 'H3'). % Lo  [27] HANGUL SYLLABLE GGUG..HANGUL SYLLABLE GGUH
unicode_line_break(0xAFD5, 0xAFEF, 'H3'). % Lo  [27] HANGUL SYLLABLE GGWEOG..HANGUL SYLLABLE GGWEOH
unicode_line_break(0xAFF1, 0xB00B, 'H3'). % Lo  [27] HANGUL SYLLABLE GGWEG..HANGUL SYLLABLE GGWEH
unicode_line_break(0xB00D, 0xB027, 'H3'). % Lo  [27] HANGUL SYLLABLE GGWIG..HANGUL SYLLABLE GGWIH
unicode_line_break(0xB029, 0xB043, 'H3'). % Lo  [27] HANGUL SYLLABLE GGYUG..HANGUL SYLLABLE GGYUH
unicode_line_break(0xB045, 0xB05F, 'H3'). % Lo  [27] HANGUL SYLLABLE GGEUG..HANGUL SYLLABLE GGEUH
unicode_line_break(0xB061, 0xB07B, 'H3'). % Lo  [27] HANGUL SYLLABLE GGYIG..HANGUL SYLLABLE GGYIH
unicode_line_break(0xB07D, 0xB097, 'H3'). % Lo  [27] HANGUL SYLLABLE GGIG..HANGUL SYLLABLE GGIH
unicode_line_break(0xB099, 0xB0B3, 'H3'). % Lo  [27] HANGUL SYLLABLE NAG..HANGUL SYLLABLE NAH
unicode_line_break(0xB0B5, 0xB0CF, 'H3'). % Lo  [27] HANGUL SYLLABLE NAEG..HANGUL SYLLABLE NAEH
unicode_line_break(0xB0D1, 0xB0EB, 'H3'). % Lo  [27] HANGUL SYLLABLE NYAG..HANGUL SYLLABLE NYAH
unicode_line_break(0xB0ED, 0xB107, 'H3'). % Lo  [27] HANGUL SYLLABLE NYAEG..HANGUL SYLLABLE NYAEH
unicode_line_break(0xB109, 0xB123, 'H3'). % Lo  [27] HANGUL SYLLABLE NEOG..HANGUL SYLLABLE NEOH
unicode_line_break(0xB125, 0xB13F, 'H3'). % Lo  [27] HANGUL SYLLABLE NEG..HANGUL SYLLABLE NEH
unicode_line_break(0xB141, 0xB15B, 'H3'). % Lo  [27] HANGUL SYLLABLE NYEOG..HANGUL SYLLABLE NYEOH
unicode_line_break(0xB15D, 0xB177, 'H3'). % Lo  [27] HANGUL SYLLABLE NYEG..HANGUL SYLLABLE NYEH
unicode_line_break(0xB179, 0xB193, 'H3'). % Lo  [27] HANGUL SYLLABLE NOG..HANGUL SYLLABLE NOH
unicode_line_break(0xB195, 0xB1AF, 'H3'). % Lo  [27] HANGUL SYLLABLE NWAG..HANGUL SYLLABLE NWAH
unicode_line_break(0xB1B1, 0xB1CB, 'H3'). % Lo  [27] HANGUL SYLLABLE NWAEG..HANGUL SYLLABLE NWAEH
unicode_line_break(0xB1CD, 0xB1E7, 'H3'). % Lo  [27] HANGUL SYLLABLE NOEG..HANGUL SYLLABLE NOEH
unicode_line_break(0xB1E9, 0xB203, 'H3'). % Lo  [27] HANGUL SYLLABLE NYOG..HANGUL SYLLABLE NYOH
unicode_line_break(0xB205, 0xB21F, 'H3'). % Lo  [27] HANGUL SYLLABLE NUG..HANGUL SYLLABLE NUH
unicode_line_break(0xB221, 0xB23B, 'H3'). % Lo  [27] HANGUL SYLLABLE NWEOG..HANGUL SYLLABLE NWEOH
unicode_line_break(0xB23D, 0xB257, 'H3'). % Lo  [27] HANGUL SYLLABLE NWEG..HANGUL SYLLABLE NWEH
unicode_line_break(0xB259, 0xB273, 'H3'). % Lo  [27] HANGUL SYLLABLE NWIG..HANGUL SYLLABLE NWIH
unicode_line_break(0xB275, 0xB28F, 'H3'). % Lo  [27] HANGUL SYLLABLE NYUG..HANGUL SYLLABLE NYUH
unicode_line_break(0xB291, 0xB2AB, 'H3'). % Lo  [27] HANGUL SYLLABLE NEUG..HANGUL SYLLABLE NEUH
unicode_line_break(0xB2AD, 0xB2C7, 'H3'). % Lo  [27] HANGUL SYLLABLE NYIG..HANGUL SYLLABLE NYIH
unicode_line_break(0xB2C9, 0xB2E3, 'H3'). % Lo  [27] HANGUL SYLLABLE NIG..HANGUL SYLLABLE NIH
unicode_line_break(0xB2E5, 0xB2FF, 'H3'). % Lo  [27] HANGUL SYLLABLE DAG..HANGUL SYLLABLE DAH
unicode_line_break(0xB301, 0xB31B, 'H3'). % Lo  [27] HANGUL SYLLABLE DAEG..HANGUL SYLLABLE DAEH
unicode_line_break(0xB31D, 0xB337, 'H3'). % Lo  [27] HANGUL SYLLABLE DYAG..HANGUL SYLLABLE DYAH
unicode_line_break(0xB339, 0xB353, 'H3'). % Lo  [27] HANGUL SYLLABLE DYAEG..HANGUL SYLLABLE DYAEH
unicode_line_break(0xB355, 0xB36F, 'H3'). % Lo  [27] HANGUL SYLLABLE DEOG..HANGUL SYLLABLE DEOH
unicode_line_break(0xB371, 0xB38B, 'H3'). % Lo  [27] HANGUL SYLLABLE DEG..HANGUL SYLLABLE DEH
unicode_line_break(0xB38D, 0xB3A7, 'H3'). % Lo  [27] HANGUL SYLLABLE DYEOG..HANGUL SYLLABLE DYEOH
unicode_line_break(0xB3A9, 0xB3C3, 'H3'). % Lo  [27] HANGUL SYLLABLE DYEG..HANGUL SYLLABLE DYEH
unicode_line_break(0xB3C5, 0xB3DF, 'H3'). % Lo  [27] HANGUL SYLLABLE DOG..HANGUL SYLLABLE DOH
unicode_line_break(0xB3E1, 0xB3FB, 'H3'). % Lo  [27] HANGUL SYLLABLE DWAG..HANGUL SYLLABLE DWAH
unicode_line_break(0xB3FD, 0xB417, 'H3'). % Lo  [27] HANGUL SYLLABLE DWAEG..HANGUL SYLLABLE DWAEH
unicode_line_break(0xB419, 0xB433, 'H3'). % Lo  [27] HANGUL SYLLABLE DOEG..HANGUL SYLLABLE DOEH
unicode_line_break(0xB435, 0xB44F, 'H3'). % Lo  [27] HANGUL SYLLABLE DYOG..HANGUL SYLLABLE DYOH
unicode_line_break(0xB451, 0xB46B, 'H3'). % Lo  [27] HANGUL SYLLABLE DUG..HANGUL SYLLABLE DUH
unicode_line_break(0xB46D, 0xB487, 'H3'). % Lo  [27] HANGUL SYLLABLE DWEOG..HANGUL SYLLABLE DWEOH
unicode_line_break(0xB489, 0xB4A3, 'H3'). % Lo  [27] HANGUL SYLLABLE DWEG..HANGUL SYLLABLE DWEH
unicode_line_break(0xB4A5, 0xB4BF, 'H3'). % Lo  [27] HANGUL SYLLABLE DWIG..HANGUL SYLLABLE DWIH
unicode_line_break(0xB4C1, 0xB4DB, 'H3'). % Lo  [27] HANGUL SYLLABLE DYUG..HANGUL SYLLABLE DYUH
unicode_line_break(0xB4DD, 0xB4F7, 'H3'). % Lo  [27] HANGUL SYLLABLE DEUG..HANGUL SYLLABLE DEUH
unicode_line_break(0xB4F9, 0xB513, 'H3'). % Lo  [27] HANGUL SYLLABLE DYIG..HANGUL SYLLABLE DYIH
unicode_line_break(0xB515, 0xB52F, 'H3'). % Lo  [27] HANGUL SYLLABLE DIG..HANGUL SYLLABLE DIH
unicode_line_break(0xB531, 0xB54B, 'H3'). % Lo  [27] HANGUL SYLLABLE DDAG..HANGUL SYLLABLE DDAH
unicode_line_break(0xB54D, 0xB567, 'H3'). % Lo  [27] HANGUL SYLLABLE DDAEG..HANGUL SYLLABLE DDAEH
unicode_line_break(0xB569, 0xB583, 'H3'). % Lo  [27] HANGUL SYLLABLE DDYAG..HANGUL SYLLABLE DDYAH
unicode_line_break(0xB585, 0xB59F, 'H3'). % Lo  [27] HANGUL SYLLABLE DDYAEG..HANGUL SYLLABLE DDYAEH
unicode_line_break(0xB5A1, 0xB5BB, 'H3'). % Lo  [27] HANGUL SYLLABLE DDEOG..HANGUL SYLLABLE DDEOH
unicode_line_break(0xB5BD, 0xB5D7, 'H3'). % Lo  [27] HANGUL SYLLABLE DDEG..HANGUL SYLLABLE DDEH
unicode_line_break(0xB5D9, 0xB5F3, 'H3'). % Lo  [27] HANGUL SYLLABLE DDYEOG..HANGUL SYLLABLE DDYEOH
unicode_line_break(0xB5F5, 0xB60F, 'H3'). % Lo  [27] HANGUL SYLLABLE DDYEG..HANGUL SYLLABLE DDYEH
unicode_line_break(0xB611, 0xB62B, 'H3'). % Lo  [27] HANGUL SYLLABLE DDOG..HANGUL SYLLABLE DDOH
unicode_line_break(0xB62D, 0xB647, 'H3'). % Lo  [27] HANGUL SYLLABLE DDWAG..HANGUL SYLLABLE DDWAH
unicode_line_break(0xB649, 0xB663, 'H3'). % Lo  [27] HANGUL SYLLABLE DDWAEG..HANGUL SYLLABLE DDWAEH
unicode_line_break(0xB665, 0xB67F, 'H3'). % Lo  [27] HANGUL SYLLABLE DDOEG..HANGUL SYLLABLE DDOEH
unicode_line_break(0xB681, 0xB69B, 'H3'). % Lo  [27] HANGUL SYLLABLE DDYOG..HANGUL SYLLABLE DDYOH
unicode_line_break(0xB69D, 0xB6B7, 'H3'). % Lo  [27] HANGUL SYLLABLE DDUG..HANGUL SYLLABLE DDUH
unicode_line_break(0xB6B9, 0xB6D3, 'H3'). % Lo  [27] HANGUL SYLLABLE DDWEOG..HANGUL SYLLABLE DDWEOH
unicode_line_break(0xB6D5, 0xB6EF, 'H3'). % Lo  [27] HANGUL SYLLABLE DDWEG..HANGUL SYLLABLE DDWEH
unicode_line_break(0xB6F1, 0xB70B, 'H3'). % Lo  [27] HANGUL SYLLABLE DDWIG..HANGUL SYLLABLE DDWIH
unicode_line_break(0xB70D, 0xB727, 'H3'). % Lo  [27] HANGUL SYLLABLE DDYUG..HANGUL SYLLABLE DDYUH
unicode_line_break(0xB729, 0xB743, 'H3'). % Lo  [27] HANGUL SYLLABLE DDEUG..HANGUL SYLLABLE DDEUH
unicode_line_break(0xB745, 0xB75F, 'H3'). % Lo  [27] HANGUL SYLLABLE DDYIG..HANGUL SYLLABLE DDYIH
unicode_line_break(0xB761, 0xB77B, 'H3'). % Lo  [27] HANGUL SYLLABLE DDIG..HANGUL SYLLABLE DDIH
unicode_line_break(0xB77D, 0xB797, 'H3'). % Lo  [27] HANGUL SYLLABLE RAG..HANGUL SYLLABLE RAH
unicode_line_break(0xB799, 0xB7B3, 'H3'). % Lo  [27] HANGUL SYLLABLE RAEG..HANGUL SYLLABLE RAEH
unicode_line_break(0xB7B5, 0xB7CF, 'H3'). % Lo  [27] HANGUL SYLLABLE RYAG..HANGUL SYLLABLE RYAH
unicode_line_break(0xB7D1, 0xB7EB, 'H3'). % Lo  [27] HANGUL SYLLABLE RYAEG..HANGUL SYLLABLE RYAEH
unicode_line_break(0xB7ED, 0xB807, 'H3'). % Lo  [27] HANGUL SYLLABLE REOG..HANGUL SYLLABLE REOH
unicode_line_break(0xB809, 0xB823, 'H3'). % Lo  [27] HANGUL SYLLABLE REG..HANGUL SYLLABLE REH
unicode_line_break(0xB825, 0xB83F, 'H3'). % Lo  [27] HANGUL SYLLABLE RYEOG..HANGUL SYLLABLE RYEOH
unicode_line_break(0xB841, 0xB85B, 'H3'). % Lo  [27] HANGUL SYLLABLE RYEG..HANGUL SYLLABLE RYEH
unicode_line_break(0xB85D, 0xB877, 'H3'). % Lo  [27] HANGUL SYLLABLE ROG..HANGUL SYLLABLE ROH
unicode_line_break(0xB879, 0xB893, 'H3'). % Lo  [27] HANGUL SYLLABLE RWAG..HANGUL SYLLABLE RWAH
unicode_line_break(0xB895, 0xB8AF, 'H3'). % Lo  [27] HANGUL SYLLABLE RWAEG..HANGUL SYLLABLE RWAEH
unicode_line_break(0xB8B1, 0xB8CB, 'H3'). % Lo  [27] HANGUL SYLLABLE ROEG..HANGUL SYLLABLE ROEH
unicode_line_break(0xB8CD, 0xB8E7, 'H3'). % Lo  [27] HANGUL SYLLABLE RYOG..HANGUL SYLLABLE RYOH
unicode_line_break(0xB8E9, 0xB903, 'H3'). % Lo  [27] HANGUL SYLLABLE RUG..HANGUL SYLLABLE RUH
unicode_line_break(0xB905, 0xB91F, 'H3'). % Lo  [27] HANGUL SYLLABLE RWEOG..HANGUL SYLLABLE RWEOH
unicode_line_break(0xB921, 0xB93B, 'H3'). % Lo  [27] HANGUL SYLLABLE RWEG..HANGUL SYLLABLE RWEH
unicode_line_break(0xB93D, 0xB957, 'H3'). % Lo  [27] HANGUL SYLLABLE RWIG..HANGUL SYLLABLE RWIH
unicode_line_break(0xB959, 0xB973, 'H3'). % Lo  [27] HANGUL SYLLABLE RYUG..HANGUL SYLLABLE RYUH
unicode_line_break(0xB975, 0xB98F, 'H3'). % Lo  [27] HANGUL SYLLABLE REUG..HANGUL SYLLABLE REUH
unicode_line_break(0xB991, 0xB9AB, 'H3'). % Lo  [27] HANGUL SYLLABLE RYIG..HANGUL SYLLABLE RYIH
unicode_line_break(0xB9AD, 0xB9C7, 'H3'). % Lo  [27] HANGUL SYLLABLE RIG..HANGUL SYLLABLE RIH
unicode_line_break(0xB9C9, 0xB9E3, 'H3'). % Lo  [27] HANGUL SYLLABLE MAG..HANGUL SYLLABLE MAH
unicode_line_break(0xB9E5, 0xB9FF, 'H3'). % Lo  [27] HANGUL SYLLABLE MAEG..HANGUL SYLLABLE MAEH
unicode_line_break(0xBA01, 0xBA1B, 'H3'). % Lo  [27] HANGUL SYLLABLE MYAG..HANGUL SYLLABLE MYAH
unicode_line_break(0xBA1D, 0xBA37, 'H3'). % Lo  [27] HANGUL SYLLABLE MYAEG..HANGUL SYLLABLE MYAEH
unicode_line_break(0xBA39, 0xBA53, 'H3'). % Lo  [27] HANGUL SYLLABLE MEOG..HANGUL SYLLABLE MEOH
unicode_line_break(0xBA55, 0xBA6F, 'H3'). % Lo  [27] HANGUL SYLLABLE MEG..HANGUL SYLLABLE MEH
unicode_line_break(0xBA71, 0xBA8B, 'H3'). % Lo  [27] HANGUL SYLLABLE MYEOG..HANGUL SYLLABLE MYEOH
unicode_line_break(0xBA8D, 0xBAA7, 'H3'). % Lo  [27] HANGUL SYLLABLE MYEG..HANGUL SYLLABLE MYEH
unicode_line_break(0xBAA9, 0xBAC3, 'H3'). % Lo  [27] HANGUL SYLLABLE MOG..HANGUL SYLLABLE MOH
unicode_line_break(0xBAC5, 0xBADF, 'H3'). % Lo  [27] HANGUL SYLLABLE MWAG..HANGUL SYLLABLE MWAH
unicode_line_break(0xBAE1, 0xBAFB, 'H3'). % Lo  [27] HANGUL SYLLABLE MWAEG..HANGUL SYLLABLE MWAEH
unicode_line_break(0xBAFD, 0xBB17, 'H3'). % Lo  [27] HANGUL SYLLABLE MOEG..HANGUL SYLLABLE MOEH
unicode_line_break(0xBB19, 0xBB33, 'H3'). % Lo  [27] HANGUL SYLLABLE MYOG..HANGUL SYLLABLE MYOH
unicode_line_break(0xBB35, 0xBB4F, 'H3'). % Lo  [27] HANGUL SYLLABLE MUG..HANGUL SYLLABLE MUH
unicode_line_break(0xBB51, 0xBB6B, 'H3'). % Lo  [27] HANGUL SYLLABLE MWEOG..HANGUL SYLLABLE MWEOH
unicode_line_break(0xBB6D, 0xBB87, 'H3'). % Lo  [27] HANGUL SYLLABLE MWEG..HANGUL SYLLABLE MWEH
unicode_line_break(0xBB89, 0xBBA3, 'H3'). % Lo  [27] HANGUL SYLLABLE MWIG..HANGUL SYLLABLE MWIH
unicode_line_break(0xBBA5, 0xBBBF, 'H3'). % Lo  [27] HANGUL SYLLABLE MYUG..HANGUL SYLLABLE MYUH
unicode_line_break(0xBBC1, 0xBBDB, 'H3'). % Lo  [27] HANGUL SYLLABLE MEUG..HANGUL SYLLABLE MEUH
unicode_line_break(0xBBDD, 0xBBF7, 'H3'). % Lo  [27] HANGUL SYLLABLE MYIG..HANGUL SYLLABLE MYIH
unicode_line_break(0xBBF9, 0xBC13, 'H3'). % Lo  [27] HANGUL SYLLABLE MIG..HANGUL SYLLABLE MIH
unicode_line_break(0xBC15, 0xBC2F, 'H3'). % Lo  [27] HANGUL SYLLABLE BAG..HANGUL SYLLABLE BAH
unicode_line_break(0xBC31, 0xBC4B, 'H3'). % Lo  [27] HANGUL SYLLABLE BAEG..HANGUL SYLLABLE BAEH
unicode_line_break(0xBC4D, 0xBC67, 'H3'). % Lo  [27] HANGUL SYLLABLE BYAG..HANGUL SYLLABLE BYAH
unicode_line_break(0xBC69, 0xBC83, 'H3'). % Lo  [27] HANGUL SYLLABLE BYAEG..HANGUL SYLLABLE BYAEH
unicode_line_break(0xBC85, 0xBC9F, 'H3'). % Lo  [27] HANGUL SYLLABLE BEOG..HANGUL SYLLABLE BEOH
unicode_line_break(0xBCA1, 0xBCBB, 'H3'). % Lo  [27] HANGUL SYLLABLE BEG..HANGUL SYLLABLE BEH
unicode_line_break(0xBCBD, 0xBCD7, 'H3'). % Lo  [27] HANGUL SYLLABLE BYEOG..HANGUL SYLLABLE BYEOH
unicode_line_break(0xBCD9, 0xBCF3, 'H3'). % Lo  [27] HANGUL SYLLABLE BYEG..HANGUL SYLLABLE BYEH
unicode_line_break(0xBCF5, 0xBD0F, 'H3'). % Lo  [27] HANGUL SYLLABLE BOG..HANGUL SYLLABLE BOH
unicode_line_break(0xBD11, 0xBD2B, 'H3'). % Lo  [27] HANGUL SYLLABLE BWAG..HANGUL SYLLABLE BWAH
unicode_line_break(0xBD2D, 0xBD47, 'H3'). % Lo  [27] HANGUL SYLLABLE BWAEG..HANGUL SYLLABLE BWAEH
unicode_line_break(0xBD49, 0xBD63, 'H3'). % Lo  [27] HANGUL SYLLABLE BOEG..HANGUL SYLLABLE BOEH
unicode_line_break(0xBD65, 0xBD7F, 'H3'). % Lo  [27] HANGUL SYLLABLE BYOG..HANGUL SYLLABLE BYOH
unicode_line_break(0xBD81, 0xBD9B, 'H3'). % Lo  [27] HANGUL SYLLABLE BUG..HANGUL SYLLABLE BUH
unicode_line_break(0xBD9D, 0xBDB7, 'H3'). % Lo  [27] HANGUL SYLLABLE BWEOG..HANGUL SYLLABLE BWEOH
unicode_line_break(0xBDB9, 0xBDD3, 'H3'). % Lo  [27] HANGUL SYLLABLE BWEG..HANGUL SYLLABLE BWEH
unicode_line_break(0xBDD5, 0xBDEF, 'H3'). % Lo  [27] HANGUL SYLLABLE BWIG..HANGUL SYLLABLE BWIH
unicode_line_break(0xBDF1, 0xBE0B, 'H3'). % Lo  [27] HANGUL SYLLABLE BYUG..HANGUL SYLLABLE BYUH
unicode_line_break(0xBE0D, 0xBE27, 'H3'). % Lo  [27] HANGUL SYLLABLE BEUG..HANGUL SYLLABLE BEUH
unicode_line_break(0xBE29, 0xBE43, 'H3'). % Lo  [27] HANGUL SYLLABLE BYIG..HANGUL SYLLABLE BYIH
unicode_line_break(0xBE45, 0xBE5F, 'H3'). % Lo  [27] HANGUL SYLLABLE BIG..HANGUL SYLLABLE BIH
unicode_line_break(0xBE61, 0xBE7B, 'H3'). % Lo  [27] HANGUL SYLLABLE BBAG..HANGUL SYLLABLE BBAH
unicode_line_break(0xBE7D, 0xBE97, 'H3'). % Lo  [27] HANGUL SYLLABLE BBAEG..HANGUL SYLLABLE BBAEH
unicode_line_break(0xBE99, 0xBEB3, 'H3'). % Lo  [27] HANGUL SYLLABLE BBYAG..HANGUL SYLLABLE BBYAH
unicode_line_break(0xBEB5, 0xBECF, 'H3'). % Lo  [27] HANGUL SYLLABLE BBYAEG..HANGUL SYLLABLE BBYAEH
unicode_line_break(0xBED1, 0xBEEB, 'H3'). % Lo  [27] HANGUL SYLLABLE BBEOG..HANGUL SYLLABLE BBEOH
unicode_line_break(0xBEED, 0xBF07, 'H3'). % Lo  [27] HANGUL SYLLABLE BBEG..HANGUL SYLLABLE BBEH
unicode_line_break(0xBF09, 0xBF23, 'H3'). % Lo  [27] HANGUL SYLLABLE BBYEOG..HANGUL SYLLABLE BBYEOH
unicode_line_break(0xBF25, 0xBF3F, 'H3'). % Lo  [27] HANGUL SYLLABLE BBYEG..HANGUL SYLLABLE BBYEH
unicode_line_break(0xBF41, 0xBF5B, 'H3'). % Lo  [27] HANGUL SYLLABLE BBOG..HANGUL SYLLABLE BBOH
unicode_line_break(0xBF5D, 0xBF77, 'H3'). % Lo  [27] HANGUL SYLLABLE BBWAG..HANGUL SYLLABLE BBWAH
unicode_line_break(0xBF79, 0xBF93, 'H3'). % Lo  [27] HANGUL SYLLABLE BBWAEG..HANGUL SYLLABLE BBWAEH
unicode_line_break(0xBF95, 0xBFAF, 'H3'). % Lo  [27] HANGUL SYLLABLE BBOEG..HANGUL SYLLABLE BBOEH
unicode_line_break(0xBFB1, 0xBFCB, 'H3'). % Lo  [27] HANGUL SYLLABLE BBYOG..HANGUL SYLLABLE BBYOH
unicode_line_break(0xBFCD, 0xBFE7, 'H3'). % Lo  [27] HANGUL SYLLABLE BBUG..HANGUL SYLLABLE BBUH
unicode_line_break(0xBFE9, 0xC003, 'H3'). % Lo  [27] HANGUL SYLLABLE BBWEOG..HANGUL SYLLABLE BBWEOH
unicode_line_break(0xC005, 0xC01F, 'H3'). % Lo  [27] HANGUL SYLLABLE BBWEG..HANGUL SYLLABLE BBWEH
unicode_line_break(0xC021, 0xC03B, 'H3'). % Lo  [27] HANGUL SYLLABLE BBWIG..HANGUL SYLLABLE BBWIH
unicode_line_break(0xC03D, 0xC057, 'H3'). % Lo  [27] HANGUL SYLLABLE BBYUG..HANGUL SYLLABLE BBYUH
unicode_line_break(0xC059, 0xC073, 'H3'). % Lo  [27] HANGUL SYLLABLE BBEUG..HANGUL SYLLABLE BBEUH
unicode_line_break(0xC075, 0xC08F, 'H3'). % Lo  [27] HANGUL SYLLABLE BBYIG..HANGUL SYLLABLE BBYIH
unicode_line_break(0xC091, 0xC0AB, 'H3'). % Lo  [27] HANGUL SYLLABLE BBIG..HANGUL SYLLABLE BBIH
unicode_line_break(0xC0AD, 0xC0C7, 'H3'). % Lo  [27] HANGUL SYLLABLE SAG..HANGUL SYLLABLE SAH
unicode_line_break(0xC0C9, 0xC0E3, 'H3'). % Lo  [27] HANGUL SYLLABLE SAEG..HANGUL SYLLABLE SAEH
unicode_line_break(0xC0E5, 0xC0FF, 'H3'). % Lo  [27] HANGUL SYLLABLE SYAG..HANGUL SYLLABLE SYAH
unicode_line_break(0xC101, 0xC11B, 'H3'). % Lo  [27] HANGUL SYLLABLE SYAEG..HANGUL SYLLABLE SYAEH
unicode_line_break(0xC11D, 0xC137, 'H3'). % Lo  [27] HANGUL SYLLABLE SEOG..HANGUL SYLLABLE SEOH
unicode_line_break(0xC139, 0xC153, 'H3'). % Lo  [27] HANGUL SYLLABLE SEG..HANGUL SYLLABLE SEH
unicode_line_break(0xC155, 0xC16F, 'H3'). % Lo  [27] HANGUL SYLLABLE SYEOG..HANGUL SYLLABLE SYEOH
unicode_line_break(0xC171, 0xC18B, 'H3'). % Lo  [27] HANGUL SYLLABLE SYEG..HANGUL SYLLABLE SYEH
unicode_line_break(0xC18D, 0xC1A7, 'H3'). % Lo  [27] HANGUL SYLLABLE SOG..HANGUL SYLLABLE SOH
unicode_line_break(0xC1A9, 0xC1C3, 'H3'). % Lo  [27] HANGUL SYLLABLE SWAG..HANGUL SYLLABLE SWAH
unicode_line_break(0xC1C5, 0xC1DF, 'H3'). % Lo  [27] HANGUL SYLLABLE SWAEG..HANGUL SYLLABLE SWAEH
unicode_line_break(0xC1E1, 0xC1FB, 'H3'). % Lo  [27] HANGUL SYLLABLE SOEG..HANGUL SYLLABLE SOEH
unicode_line_break(0xC1FD, 0xC217, 'H3'). % Lo  [27] HANGUL SYLLABLE SYOG..HANGUL SYLLABLE SYOH
unicode_line_break(0xC219, 0xC233, 'H3'). % Lo  [27] HANGUL SYLLABLE SUG..HANGUL SYLLABLE SUH
unicode_line_break(0xC235, 0xC24F, 'H3'). % Lo  [27] HANGUL SYLLABLE SWEOG..HANGUL SYLLABLE SWEOH
unicode_line_break(0xC251, 0xC26B, 'H3'). % Lo  [27] HANGUL SYLLABLE SWEG..HANGUL SYLLABLE SWEH
unicode_line_break(0xC26D, 0xC287, 'H3'). % Lo  [27] HANGUL SYLLABLE SWIG..HANGUL SYLLABLE SWIH
unicode_line_break(0xC289, 0xC2A3, 'H3'). % Lo  [27] HANGUL SYLLABLE SYUG..HANGUL SYLLABLE SYUH
unicode_line_break(0xC2A5, 0xC2BF, 'H3'). % Lo  [27] HANGUL SYLLABLE SEUG..HANGUL SYLLABLE SEUH
unicode_line_break(0xC2C1, 0xC2DB, 'H3'). % Lo  [27] HANGUL SYLLABLE SYIG..HANGUL SYLLABLE SYIH
unicode_line_break(0xC2DD, 0xC2F7, 'H3'). % Lo  [27] HANGUL SYLLABLE SIG..HANGUL SYLLABLE SIH
unicode_line_break(0xC2F9, 0xC313, 'H3'). % Lo  [27] HANGUL SYLLABLE SSAG..HANGUL SYLLABLE SSAH
unicode_line_break(0xC315, 0xC32F, 'H3'). % Lo  [27] HANGUL SYLLABLE SSAEG..HANGUL SYLLABLE SSAEH
unicode_line_break(0xC331, 0xC34B, 'H3'). % Lo  [27] HANGUL SYLLABLE SSYAG..HANGUL SYLLABLE SSYAH
unicode_line_break(0xC34D, 0xC367, 'H3'). % Lo  [27] HANGUL SYLLABLE SSYAEG..HANGUL SYLLABLE SSYAEH
unicode_line_break(0xC369, 0xC383, 'H3'). % Lo  [27] HANGUL SYLLABLE SSEOG..HANGUL SYLLABLE SSEOH
unicode_line_break(0xC385, 0xC39F, 'H3'). % Lo  [27] HANGUL SYLLABLE SSEG..HANGUL SYLLABLE SSEH
unicode_line_break(0xC3A1, 0xC3BB, 'H3'). % Lo  [27] HANGUL SYLLABLE SSYEOG..HANGUL SYLLABLE SSYEOH
unicode_line_break(0xC3BD, 0xC3D7, 'H3'). % Lo  [27] HANGUL SYLLABLE SSYEG..HANGUL SYLLABLE SSYEH
unicode_line_break(0xC3D9, 0xC3F3, 'H3'). % Lo  [27] HANGUL SYLLABLE SSOG..HANGUL SYLLABLE SSOH
unicode_line_break(0xC3F5, 0xC40F, 'H3'). % Lo  [27] HANGUL SYLLABLE SSWAG..HANGUL SYLLABLE SSWAH
unicode_line_break(0xC411, 0xC42B, 'H3'). % Lo  [27] HANGUL SYLLABLE SSWAEG..HANGUL SYLLABLE SSWAEH
unicode_line_break(0xC42D, 0xC447, 'H3'). % Lo  [27] HANGUL SYLLABLE SSOEG..HANGUL SYLLABLE SSOEH
unicode_line_break(0xC449, 0xC463, 'H3'). % Lo  [27] HANGUL SYLLABLE SSYOG..HANGUL SYLLABLE SSYOH
unicode_line_break(0xC465, 0xC47F, 'H3'). % Lo  [27] HANGUL SYLLABLE SSUG..HANGUL SYLLABLE SSUH
unicode_line_break(0xC481, 0xC49B, 'H3'). % Lo  [27] HANGUL SYLLABLE SSWEOG..HANGUL SYLLABLE SSWEOH
unicode_line_break(0xC49D, 0xC4B7, 'H3'). % Lo  [27] HANGUL SYLLABLE SSWEG..HANGUL SYLLABLE SSWEH
unicode_line_break(0xC4B9, 0xC4D3, 'H3'). % Lo  [27] HANGUL SYLLABLE SSWIG..HANGUL SYLLABLE SSWIH
unicode_line_break(0xC4D5, 0xC4EF, 'H3'). % Lo  [27] HANGUL SYLLABLE SSYUG..HANGUL SYLLABLE SSYUH
unicode_line_break(0xC4F1, 0xC50B, 'H3'). % Lo  [27] HANGUL SYLLABLE SSEUG..HANGUL SYLLABLE SSEUH
unicode_line_break(0xC50D, 0xC527, 'H3'). % Lo  [27] HANGUL SYLLABLE SSYIG..HANGUL SYLLABLE SSYIH
unicode_line_break(0xC529, 0xC543, 'H3'). % Lo  [27] HANGUL SYLLABLE SSIG..HANGUL SYLLABLE SSIH
unicode_line_break(0xC545, 0xC55F, 'H3'). % Lo  [27] HANGUL SYLLABLE AG..HANGUL SYLLABLE AH
unicode_line_break(0xC561, 0xC57B, 'H3'). % Lo  [27] HANGUL SYLLABLE AEG..HANGUL SYLLABLE AEH
unicode_line_break(0xC57D, 0xC597, 'H3'). % Lo  [27] HANGUL SYLLABLE YAG..HANGUL SYLLABLE YAH
unicode_line_break(0xC599, 0xC5B3, 'H3'). % Lo  [27] HANGUL SYLLABLE YAEG..HANGUL SYLLABLE YAEH
unicode_line_break(0xC5B5, 0xC5CF, 'H3'). % Lo  [27] HANGUL SYLLABLE EOG..HANGUL SYLLABLE EOH
unicode_line_break(0xC5D1, 0xC5EB, 'H3'). % Lo  [27] HANGUL SYLLABLE EG..HANGUL SYLLABLE EH
unicode_line_break(0xC5ED, 0xC607, 'H3'). % Lo  [27] HANGUL SYLLABLE YEOG..HANGUL SYLLABLE YEOH
unicode_line_break(0xC609, 0xC623, 'H3'). % Lo  [27] HANGUL SYLLABLE YEG..HANGUL SYLLABLE YEH
unicode_line_break(0xC625, 0xC63F, 'H3'). % Lo  [27] HANGUL SYLLABLE OG..HANGUL SYLLABLE OH
unicode_line_break(0xC641, 0xC65B, 'H3'). % Lo  [27] HANGUL SYLLABLE WAG..HANGUL SYLLABLE WAH
unicode_line_break(0xC65D, 0xC677, 'H3'). % Lo  [27] HANGUL SYLLABLE WAEG..HANGUL SYLLABLE WAEH
unicode_line_break(0xC679, 0xC693, 'H3'). % Lo  [27] HANGUL SYLLABLE OEG..HANGUL SYLLABLE OEH
unicode_line_break(0xC695, 0xC6AF, 'H3'). % Lo  [27] HANGUL SYLLABLE YOG..HANGUL SYLLABLE YOH
unicode_line_break(0xC6B1, 0xC6CB, 'H3'). % Lo  [27] HANGUL SYLLABLE UG..HANGUL SYLLABLE UH
unicode_line_break(0xC6CD, 0xC6E7, 'H3'). % Lo  [27] HANGUL SYLLABLE WEOG..HANGUL SYLLABLE WEOH
unicode_line_break(0xC6E9, 0xC703, 'H3'). % Lo  [27] HANGUL SYLLABLE WEG..HANGUL SYLLABLE WEH
unicode_line_break(0xC705, 0xC71F, 'H3'). % Lo  [27] HANGUL SYLLABLE WIG..HANGUL SYLLABLE WIH
unicode_line_break(0xC721, 0xC73B, 'H3'). % Lo  [27] HANGUL SYLLABLE YUG..HANGUL SYLLABLE YUH
unicode_line_break(0xC73D, 0xC757, 'H3'). % Lo  [27] HANGUL SYLLABLE EUG..HANGUL SYLLABLE EUH
unicode_line_break(0xC759, 0xC773, 'H3'). % Lo  [27] HANGUL SYLLABLE YIG..HANGUL SYLLABLE YIH
unicode_line_break(0xC775, 0xC78F, 'H3'). % Lo  [27] HANGUL SYLLABLE IG..HANGUL SYLLABLE IH
unicode_line_break(0xC791, 0xC7AB, 'H3'). % Lo  [27] HANGUL SYLLABLE JAG..HANGUL SYLLABLE JAH
unicode_line_break(0xC7AD, 0xC7C7, 'H3'). % Lo  [27] HANGUL SYLLABLE JAEG..HANGUL SYLLABLE JAEH
unicode_line_break(0xC7C9, 0xC7E3, 'H3'). % Lo  [27] HANGUL SYLLABLE JYAG..HANGUL SYLLABLE JYAH
unicode_line_break(0xC7E5, 0xC7FF, 'H3'). % Lo  [27] HANGUL SYLLABLE JYAEG..HANGUL SYLLABLE JYAEH
unicode_line_break(0xC801, 0xC81B, 'H3'). % Lo  [27] HANGUL SYLLABLE JEOG..HANGUL SYLLABLE JEOH
unicode_line_break(0xC81D, 0xC837, 'H3'). % Lo  [27] HANGUL SYLLABLE JEG..HANGUL SYLLABLE JEH
unicode_line_break(0xC839, 0xC853, 'H3'). % Lo  [27] HANGUL SYLLABLE JYEOG..HANGUL SYLLABLE JYEOH
unicode_line_break(0xC855, 0xC86F, 'H3'). % Lo  [27] HANGUL SYLLABLE JYEG..HANGUL SYLLABLE JYEH
unicode_line_break(0xC871, 0xC88B, 'H3'). % Lo  [27] HANGUL SYLLABLE JOG..HANGUL SYLLABLE JOH
unicode_line_break(0xC88D, 0xC8A7, 'H3'). % Lo  [27] HANGUL SYLLABLE JWAG..HANGUL SYLLABLE JWAH
unicode_line_break(0xC8A9, 0xC8C3, 'H3'). % Lo  [27] HANGUL SYLLABLE JWAEG..HANGUL SYLLABLE JWAEH
unicode_line_break(0xC8C5, 0xC8DF, 'H3'). % Lo  [27] HANGUL SYLLABLE JOEG..HANGUL SYLLABLE JOEH
unicode_line_break(0xC8E1, 0xC8FB, 'H3'). % Lo  [27] HANGUL SYLLABLE JYOG..HANGUL SYLLABLE JYOH
unicode_line_break(0xC8FD, 0xC917, 'H3'). % Lo  [27] HANGUL SYLLABLE JUG..HANGUL SYLLABLE JUH
unicode_line_break(0xC919, 0xC933, 'H3'). % Lo  [27] HANGUL SYLLABLE JWEOG..HANGUL SYLLABLE JWEOH
unicode_line_break(0xC935, 0xC94F, 'H3'). % Lo  [27] HANGUL SYLLABLE JWEG..HANGUL SYLLABLE JWEH
unicode_line_break(0xC951, 0xC96B, 'H3'). % Lo  [27] HANGUL SYLLABLE JWIG..HANGUL SYLLABLE JWIH
unicode_line_break(0xC96D, 0xC987, 'H3'). % Lo  [27] HANGUL SYLLABLE JYUG..HANGUL SYLLABLE JYUH
unicode_line_break(0xC989, 0xC9A3, 'H3'). % Lo  [27] HANGUL SYLLABLE JEUG..HANGUL SYLLABLE JEUH
unicode_line_break(0xC9A5, 0xC9BF, 'H3'). % Lo  [27] HANGUL SYLLABLE JYIG..HANGUL SYLLABLE JYIH
unicode_line_break(0xC9C1, 0xC9DB, 'H3'). % Lo  [27] HANGUL SYLLABLE JIG..HANGUL SYLLABLE JIH
unicode_line_break(0xC9DD, 0xC9F7, 'H3'). % Lo  [27] HANGUL SYLLABLE JJAG..HANGUL SYLLABLE JJAH
unicode_line_break(0xC9F9, 0xCA13, 'H3'). % Lo  [27] HANGUL SYLLABLE JJAEG..HANGUL SYLLABLE JJAEH
unicode_line_break(0xCA15, 0xCA2F, 'H3'). % Lo  [27] HANGUL SYLLABLE JJYAG..HANGUL SYLLABLE JJYAH
unicode_line_break(0xCA31, 0xCA4B, 'H3'). % Lo  [27] HANGUL SYLLABLE JJYAEG..HANGUL SYLLABLE JJYAEH
unicode_line_break(0xCA4D, 0xCA67, 'H3'). % Lo  [27] HANGUL SYLLABLE JJEOG..HANGUL SYLLABLE JJEOH
unicode_line_break(0xCA69, 0xCA83, 'H3'). % Lo  [27] HANGUL SYLLABLE JJEG..HANGUL SYLLABLE JJEH
unicode_line_break(0xCA85, 0xCA9F, 'H3'). % Lo  [27] HANGUL SYLLABLE JJYEOG..HANGUL SYLLABLE JJYEOH
unicode_line_break(0xCAA1, 0xCABB, 'H3'). % Lo  [27] HANGUL SYLLABLE JJYEG..HANGUL SYLLABLE JJYEH
unicode_line_break(0xCABD, 0xCAD7, 'H3'). % Lo  [27] HANGUL SYLLABLE JJOG..HANGUL SYLLABLE JJOH
unicode_line_break(0xCAD9, 0xCAF3, 'H3'). % Lo  [27] HANGUL SYLLABLE JJWAG..HANGUL SYLLABLE JJWAH
unicode_line_break(0xCAF5, 0xCB0F, 'H3'). % Lo  [27] HANGUL SYLLABLE JJWAEG..HANGUL SYLLABLE JJWAEH
unicode_line_break(0xCB11, 0xCB2B, 'H3'). % Lo  [27] HANGUL SYLLABLE JJOEG..HANGUL SYLLABLE JJOEH
unicode_line_break(0xCB2D, 0xCB47, 'H3'). % Lo  [27] HANGUL SYLLABLE JJYOG..HANGUL SYLLABLE JJYOH
unicode_line_break(0xCB49, 0xCB63, 'H3'). % Lo  [27] HANGUL SYLLABLE JJUG..HANGUL SYLLABLE JJUH
unicode_line_break(0xCB65, 0xCB7F, 'H3'). % Lo  [27] HANGUL SYLLABLE JJWEOG..HANGUL SYLLABLE JJWEOH
unicode_line_break(0xCB81, 0xCB9B, 'H3'). % Lo  [27] HANGUL SYLLABLE JJWEG..HANGUL SYLLABLE JJWEH
unicode_line_break(0xCB9D, 0xCBB7, 'H3'). % Lo  [27] HANGUL SYLLABLE JJWIG..HANGUL SYLLABLE JJWIH
unicode_line_break(0xCBB9, 0xCBD3, 'H3'). % Lo  [27] HANGUL SYLLABLE JJYUG..HANGUL SYLLABLE JJYUH
unicode_line_break(0xCBD5, 0xCBEF, 'H3'). % Lo  [27] HANGUL SYLLABLE JJEUG..HANGUL SYLLABLE JJEUH
unicode_line_break(0xCBF1, 0xCC0B, 'H3'). % Lo  [27] HANGUL SYLLABLE JJYIG..HANGUL SYLLABLE JJYIH
unicode_line_break(0xCC0D, 0xCC27, 'H3'). % Lo  [27] HANGUL SYLLABLE JJIG..HANGUL SYLLABLE JJIH
unicode_line_break(0xCC29, 0xCC43, 'H3'). % Lo  [27] HANGUL SYLLABLE CAG..HANGUL SYLLABLE CAH
unicode_line_break(0xCC45, 0xCC5F, 'H3'). % Lo  [27] HANGUL SYLLABLE CAEG..HANGUL SYLLABLE CAEH
unicode_line_break(0xCC61, 0xCC7B, 'H3'). % Lo  [27] HANGUL SYLLABLE CYAG..HANGUL SYLLABLE CYAH
unicode_line_break(0xCC7D, 0xCC97, 'H3'). % Lo  [27] HANGUL SYLLABLE CYAEG..HANGUL SYLLABLE CYAEH
unicode_line_break(0xCC99, 0xCCB3, 'H3'). % Lo  [27] HANGUL SYLLABLE CEOG..HANGUL SYLLABLE CEOH
unicode_line_break(0xCCB5, 0xCCCF, 'H3'). % Lo  [27] HANGUL SYLLABLE CEG..HANGUL SYLLABLE CEH
unicode_line_break(0xCCD1, 0xCCEB, 'H3'). % Lo  [27] HANGUL SYLLABLE CYEOG..HANGUL SYLLABLE CYEOH
unicode_line_break(0xCCED, 0xCD07, 'H3'). % Lo  [27] HANGUL SYLLABLE CYEG..HANGUL SYLLABLE CYEH
unicode_line_break(0xCD09, 0xCD23, 'H3'). % Lo  [27] HANGUL SYLLABLE COG..HANGUL SYLLABLE COH
unicode_line_break(0xCD25, 0xCD3F, 'H3'). % Lo  [27] HANGUL SYLLABLE CWAG..HANGUL SYLLABLE CWAH
unicode_line_break(0xCD41, 0xCD5B, 'H3'). % Lo  [27] HANGUL SYLLABLE CWAEG..HANGUL SYLLABLE CWAEH
unicode_line_break(0xCD5D, 0xCD77, 'H3'). % Lo  [27] HANGUL SYLLABLE COEG..HANGUL SYLLABLE COEH
unicode_line_break(0xCD79, 0xCD93, 'H3'). % Lo  [27] HANGUL SYLLABLE CYOG..HANGUL SYLLABLE CYOH
unicode_line_break(0xCD95, 0xCDAF, 'H3'). % Lo  [27] HANGUL SYLLABLE CUG..HANGUL SYLLABLE CUH
unicode_line_break(0xCDB1, 0xCDCB, 'H3'). % Lo  [27] HANGUL SYLLABLE CWEOG..HANGUL SYLLABLE CWEOH
unicode_line_break(0xCDCD, 0xCDE7, 'H3'). % Lo  [27] HANGUL SYLLABLE CWEG..HANGUL SYLLABLE CWEH
unicode_line_break(0xCDE9, 0xCE03, 'H3'). % Lo  [27] HANGUL SYLLABLE CWIG..HANGUL SYLLABLE CWIH
unicode_line_break(0xCE05, 0xCE1F, 'H3'). % Lo  [27] HANGUL SYLLABLE CYUG..HANGUL SYLLABLE CYUH
unicode_line_break(0xCE21, 0xCE3B, 'H3'). % Lo  [27] HANGUL SYLLABLE CEUG..HANGUL SYLLABLE CEUH
unicode_line_break(0xCE3D, 0xCE57, 'H3'). % Lo  [27] HANGUL SYLLABLE CYIG..HANGUL SYLLABLE CYIH
unicode_line_break(0xCE59, 0xCE73, 'H3'). % Lo  [27] HANGUL SYLLABLE CIG..HANGUL SYLLABLE CIH
unicode_line_break(0xCE75, 0xCE8F, 'H3'). % Lo  [27] HANGUL SYLLABLE KAG..HANGUL SYLLABLE KAH
unicode_line_break(0xCE91, 0xCEAB, 'H3'). % Lo  [27] HANGUL SYLLABLE KAEG..HANGUL SYLLABLE KAEH
unicode_line_break(0xCEAD, 0xCEC7, 'H3'). % Lo  [27] HANGUL SYLLABLE KYAG..HANGUL SYLLABLE KYAH
unicode_line_break(0xCEC9, 0xCEE3, 'H3'). % Lo  [27] HANGUL SYLLABLE KYAEG..HANGUL SYLLABLE KYAEH
unicode_line_break(0xCEE5, 0xCEFF, 'H3'). % Lo  [27] HANGUL SYLLABLE KEOG..HANGUL SYLLABLE KEOH
unicode_line_break(0xCF01, 0xCF1B, 'H3'). % Lo  [27] HANGUL SYLLABLE KEG..HANGUL SYLLABLE KEH
unicode_line_break(0xCF1D, 0xCF37, 'H3'). % Lo  [27] HANGUL SYLLABLE KYEOG..HANGUL SYLLABLE KYEOH
unicode_line_break(0xCF39, 0xCF53, 'H3'). % Lo  [27] HANGUL SYLLABLE KYEG..HANGUL SYLLABLE KYEH
unicode_line_break(0xCF55, 0xCF6F, 'H3'). % Lo  [27] HANGUL SYLLABLE KOG..HANGUL SYLLABLE KOH
unicode_line_break(0xCF71, 0xCF8B, 'H3'). % Lo  [27] HANGUL SYLLABLE KWAG..HANGUL SYLLABLE KWAH
unicode_line_break(0xCF8D, 0xCFA7, 'H3'). % Lo  [27] HANGUL SYLLABLE KWAEG..HANGUL SYLLABLE KWAEH
unicode_line_break(0xCFA9, 0xCFC3, 'H3'). % Lo  [27] HANGUL SYLLABLE KOEG..HANGUL SYLLABLE KOEH
unicode_line_break(0xCFC5, 0xCFDF, 'H3'). % Lo  [27] HANGUL SYLLABLE KYOG..HANGUL SYLLABLE KYOH
unicode_line_break(0xCFE1, 0xCFFB, 'H3'). % Lo  [27] HANGUL SYLLABLE KUG..HANGUL SYLLABLE KUH
unicode_line_break(0xCFFD, 0xD017, 'H3'). % Lo  [27] HANGUL SYLLABLE KWEOG..HANGUL SYLLABLE KWEOH
unicode_line_break(0xD019, 0xD033, 'H3'). % Lo  [27] HANGUL SYLLABLE KWEG..HANGUL SYLLABLE KWEH
unicode_line_break(0xD035, 0xD04F, 'H3'). % Lo  [27] HANGUL SYLLABLE KWIG..HANGUL SYLLABLE KWIH
unicode_line_break(0xD051, 0xD06B, 'H3'). % Lo  [27] HANGUL SYLLABLE KYUG..HANGUL SYLLABLE KYUH
unicode_line_break(0xD06D, 0xD087, 'H3'). % Lo  [27] HANGUL SYLLABLE KEUG..HANGUL SYLLABLE KEUH
unicode_line_break(0xD089, 0xD0A3, 'H3'). % Lo  [27] HANGUL SYLLABLE KYIG..HANGUL SYLLABLE KYIH
unicode_line_break(0xD0A5, 0xD0BF, 'H3'). % Lo  [27] HANGUL SYLLABLE KIG..HANGUL SYLLABLE KIH
unicode_line_break(0xD0C1, 0xD0DB, 'H3'). % Lo  [27] HANGUL SYLLABLE TAG..HANGUL SYLLABLE TAH
unicode_line_break(0xD0DD, 0xD0F7, 'H3'). % Lo  [27] HANGUL SYLLABLE TAEG..HANGUL SYLLABLE TAEH
unicode_line_break(0xD0F9, 0xD113, 'H3'). % Lo  [27] HANGUL SYLLABLE TYAG..HANGUL SYLLABLE TYAH
unicode_line_break(0xD115, 0xD12F, 'H3'). % Lo  [27] HANGUL SYLLABLE TYAEG..HANGUL SYLLABLE TYAEH
unicode_line_break(0xD131, 0xD14B, 'H3'). % Lo  [27] HANGUL SYLLABLE TEOG..HANGUL SYLLABLE TEOH
unicode_line_break(0xD14D, 0xD167, 'H3'). % Lo  [27] HANGUL SYLLABLE TEG..HANGUL SYLLABLE TEH
unicode_line_break(0xD169, 0xD183, 'H3'). % Lo  [27] HANGUL SYLLABLE TYEOG..HANGUL SYLLABLE TYEOH
unicode_line_break(0xD185, 0xD19F, 'H3'). % Lo  [27] HANGUL SYLLABLE TYEG..HANGUL SYLLABLE TYEH
unicode_line_break(0xD1A1, 0xD1BB, 'H3'). % Lo  [27] HANGUL SYLLABLE TOG..HANGUL SYLLABLE TOH
unicode_line_break(0xD1BD, 0xD1D7, 'H3'). % Lo  [27] HANGUL SYLLABLE TWAG..HANGUL SYLLABLE TWAH
unicode_line_break(0xD1D9, 0xD1F3, 'H3'). % Lo  [27] HANGUL SYLLABLE TWAEG..HANGUL SYLLABLE TWAEH
unicode_line_break(0xD1F5, 0xD20F, 'H3'). % Lo  [27] HANGUL SYLLABLE TOEG..HANGUL SYLLABLE TOEH
unicode_line_break(0xD211, 0xD22B, 'H3'). % Lo  [27] HANGUL SYLLABLE TYOG..HANGUL SYLLABLE TYOH
unicode_line_break(0xD22D, 0xD247, 'H3'). % Lo  [27] HANGUL SYLLABLE TUG..HANGUL SYLLABLE TUH
unicode_line_break(0xD249, 0xD263, 'H3'). % Lo  [27] HANGUL SYLLABLE TWEOG..HANGUL SYLLABLE TWEOH
unicode_line_break(0xD265, 0xD27F, 'H3'). % Lo  [27] HANGUL SYLLABLE TWEG..HANGUL SYLLABLE TWEH
unicode_line_break(0xD281, 0xD29B, 'H3'). % Lo  [27] HANGUL SYLLABLE TWIG..HANGUL SYLLABLE TWIH
unicode_line_break(0xD29D, 0xD2B7, 'H3'). % Lo  [27] HANGUL SYLLABLE TYUG..HANGUL SYLLABLE TYUH
unicode_line_break(0xD2B9, 0xD2D3, 'H3'). % Lo  [27] HANGUL SYLLABLE TEUG..HANGUL SYLLABLE TEUH
unicode_line_break(0xD2D5, 0xD2EF, 'H3'). % Lo  [27] HANGUL SYLLABLE TYIG..HANGUL SYLLABLE TYIH
unicode_line_break(0xD2F1, 0xD30B, 'H3'). % Lo  [27] HANGUL SYLLABLE TIG..HANGUL SYLLABLE TIH
unicode_line_break(0xD30D, 0xD327, 'H3'). % Lo  [27] HANGUL SYLLABLE PAG..HANGUL SYLLABLE PAH
unicode_line_break(0xD329, 0xD343, 'H3'). % Lo  [27] HANGUL SYLLABLE PAEG..HANGUL SYLLABLE PAEH
unicode_line_break(0xD345, 0xD35F, 'H3'). % Lo  [27] HANGUL SYLLABLE PYAG..HANGUL SYLLABLE PYAH
unicode_line_break(0xD361, 0xD37B, 'H3'). % Lo  [27] HANGUL SYLLABLE PYAEG..HANGUL SYLLABLE PYAEH
unicode_line_break(0xD37D, 0xD397, 'H3'). % Lo  [27] HANGUL SYLLABLE PEOG..HANGUL SYLLABLE PEOH
unicode_line_break(0xD399, 0xD3B3, 'H3'). % Lo  [27] HANGUL SYLLABLE PEG..HANGUL SYLLABLE PEH
unicode_line_break(0xD3B5, 0xD3CF, 'H3'). % Lo  [27] HANGUL SYLLABLE PYEOG..HANGUL SYLLABLE PYEOH
unicode_line_break(0xD3D1, 0xD3EB, 'H3'). % Lo  [27] HANGUL SYLLABLE PYEG..HANGUL SYLLABLE PYEH
unicode_line_break(0xD3ED, 0xD407, 'H3'). % Lo  [27] HANGUL SYLLABLE POG..HANGUL SYLLABLE POH
unicode_line_break(0xD409, 0xD423, 'H3'). % Lo  [27] HANGUL SYLLABLE PWAG..HANGUL SYLLABLE PWAH
unicode_line_break(0xD425, 0xD43F, 'H3'). % Lo  [27] HANGUL SYLLABLE PWAEG..HANGUL SYLLABLE PWAEH
unicode_line_break(0xD441, 0xD45B, 'H3'). % Lo  [27] HANGUL SYLLABLE POEG..HANGUL SYLLABLE POEH
unicode_line_break(0xD45D, 0xD477, 'H3'). % Lo  [27] HANGUL SYLLABLE PYOG..HANGUL SYLLABLE PYOH
unicode_line_break(0xD479, 0xD493, 'H3'). % Lo  [27] HANGUL SYLLABLE PUG..HANGUL SYLLABLE PUH
unicode_line_break(0xD495, 0xD4AF, 'H3'). % Lo  [27] HANGUL SYLLABLE PWEOG..HANGUL SYLLABLE PWEOH
unicode_line_break(0xD4B1, 0xD4CB, 'H3'). % Lo  [27] HANGUL SYLLABLE PWEG..HANGUL SYLLABLE PWEH
unicode_line_break(0xD4CD, 0xD4E7, 'H3'). % Lo  [27] HANGUL SYLLABLE PWIG..HANGUL SYLLABLE PWIH
unicode_line_break(0xD4E9, 0xD503, 'H3'). % Lo  [27] HANGUL SYLLABLE PYUG..HANGUL SYLLABLE PYUH
unicode_line_break(0xD505, 0xD51F, 'H3'). % Lo  [27] HANGUL SYLLABLE PEUG..HANGUL SYLLABLE PEUH
unicode_line_break(0xD521, 0xD53B, 'H3'). % Lo  [27] HANGUL SYLLABLE PYIG..HANGUL SYLLABLE PYIH
unicode_line_break(0xD53D, 0xD557, 'H3'). % Lo  [27] HANGUL SYLLABLE PIG..HANGUL SYLLABLE PIH
unicode_line_break(0xD559, 0xD573, 'H3'). % Lo  [27] HANGUL SYLLABLE HAG..HANGUL SYLLABLE HAH
unicode_line_break(0xD575, 0xD58F, 'H3'). % Lo  [27] HANGUL SYLLABLE HAEG..HANGUL SYLLABLE HAEH
unicode_line_break(0xD591, 0xD5AB, 'H3'). % Lo  [27] HANGUL SYLLABLE HYAG..HANGUL SYLLABLE HYAH
unicode_line_break(0xD5AD, 0xD5C7, 'H3'). % Lo  [27] HANGUL SYLLABLE HYAEG..HANGUL SYLLABLE HYAEH
unicode_line_break(0xD5C9, 0xD5E3, 'H3'). % Lo  [27] HANGUL SYLLABLE HEOG..HANGUL SYLLABLE HEOH
unicode_line_break(0xD5E5, 0xD5FF, 'H3'). % Lo  [27] HANGUL SYLLABLE HEG..HANGUL SYLLABLE HEH
unicode_line_break(0xD601, 0xD61B, 'H3'). % Lo  [27] HANGUL SYLLABLE HYEOG..HANGUL SYLLABLE HYEOH
unicode_line_break(0xD61D, 0xD637, 'H3'). % Lo  [27] HANGUL SYLLABLE HYEG..HANGUL SYLLABLE HYEH
unicode_line_break(0xD639, 0xD653, 'H3'). % Lo  [27] HANGUL SYLLABLE HOG..HANGUL SYLLABLE HOH
unicode_line_break(0xD655, 0xD66F, 'H3'). % Lo  [27] HANGUL SYLLABLE HWAG..HANGUL SYLLABLE HWAH
unicode_line_break(0xD671, 0xD68B, 'H3'). % Lo  [27] HANGUL SYLLABLE HWAEG..HANGUL SYLLABLE HWAEH
unicode_line_break(0xD68D, 0xD6A7, 'H3'). % Lo  [27] HANGUL SYLLABLE HOEG..HANGUL SYLLABLE HOEH
unicode_line_break(0xD6A9, 0xD6C3, 'H3'). % Lo  [27] HANGUL SYLLABLE HYOG..HANGUL SYLLABLE HYOH
unicode_line_break(0xD6C5, 0xD6DF, 'H3'). % Lo  [27] HANGUL SYLLABLE HUG..HANGUL SYLLABLE HUH
unicode_line_break(0xD6E1, 0xD6FB, 'H3'). % Lo  [27] HANGUL SYLLABLE HWEOG..HANGUL SYLLABLE HWEOH
unicode_line_break(0xD6FD, 0xD717, 'H3'). % Lo  [27] HANGUL SYLLABLE HWEG..HANGUL SYLLABLE HWEH
unicode_line_break(0xD719, 0xD733, 'H3'). % Lo  [27] HANGUL SYLLABLE HWIG..HANGUL SYLLABLE HWIH
unicode_line_break(0xD735, 0xD74F, 'H3'). % Lo  [27] HANGUL SYLLABLE HYUG..HANGUL SYLLABLE HYUH
unicode_line_break(0xD751, 0xD76B, 'H3'). % Lo  [27] HANGUL SYLLABLE HEUG..HANGUL SYLLABLE HEUH
unicode_line_break(0xD76D, 0xD787, 'H3'). % Lo  [27] HANGUL SYLLABLE HYIG..HANGUL SYLLABLE HYIH
unicode_line_break(0xD789, 0xD7A3, 'H3'). % Lo  [27] HANGUL SYLLABLE HIG..HANGUL SYLLABLE HIH

% Total code points: 10773

% ================================================

% Line_Break=Close_Parenthesis

unicode_line_break(0x0029, 0x0029, 'CP'). % Pe       RIGHT PARENTHESIS
unicode_line_break(0x005D, 0x005D, 'CP'). % Pe       RIGHT SQUARE BRACKET

% Total code points: 2

% ================================================

% Line_Break=Hebrew_Letter

unicode_line_break(0x05D0, 0x05EA, 'HL'). % Lo  [27] HEBREW LETTER ALEF..HEBREW LETTER TAV
unicode_line_break(0x05F0, 0x05F2, 'HL'). % Lo   [3] HEBREW LIGATURE YIDDISH DOUBLE VAV..HEBREW LIGATURE YIDDISH DOUBLE YOD
unicode_line_break(0xFB1D, 0xFB1D, 'HL'). % Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_line_break(0xFB1F, 0xFB28, 'HL'). % Lo  [10] HEBREW LIGATURE YIDDISH YOD YOD PATAH..HEBREW LETTER WIDE TAV
unicode_line_break(0xFB2A, 0xFB36, 'HL'). % Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_line_break(0xFB38, 0xFB3C, 'HL'). % Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_line_break(0xFB3E, 0xFB3E, 'HL'). % Lo       HEBREW LETTER MEM WITH DAGESH
unicode_line_break(0xFB40, 0xFB41, 'HL'). % Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_line_break(0xFB43, 0xFB44, 'HL'). % Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_line_break(0xFB46, 0xFB4F, 'HL'). % Lo  [10] HEBREW LETTER TSADI WITH DAGESH..HEBREW LIGATURE ALEF LAMED

% Total code points: 74

% ================================================

% Line_Break=Conditional_Japanese_Starter

unicode_line_break(0x3041, 0x3041, 'CJ'). % Lo       HIRAGANA LETTER SMALL A
unicode_line_break(0x3043, 0x3043, 'CJ'). % Lo       HIRAGANA LETTER SMALL I
unicode_line_break(0x3045, 0x3045, 'CJ'). % Lo       HIRAGANA LETTER SMALL U
unicode_line_break(0x3047, 0x3047, 'CJ'). % Lo       HIRAGANA LETTER SMALL E
unicode_line_break(0x3049, 0x3049, 'CJ'). % Lo       HIRAGANA LETTER SMALL O
unicode_line_break(0x3063, 0x3063, 'CJ'). % Lo       HIRAGANA LETTER SMALL TU
unicode_line_break(0x3083, 0x3083, 'CJ'). % Lo       HIRAGANA LETTER SMALL YA
unicode_line_break(0x3085, 0x3085, 'CJ'). % Lo       HIRAGANA LETTER SMALL YU
unicode_line_break(0x3087, 0x3087, 'CJ'). % Lo       HIRAGANA LETTER SMALL YO
unicode_line_break(0x308E, 0x308E, 'CJ'). % Lo       HIRAGANA LETTER SMALL WA
unicode_line_break(0x3095, 0x3096, 'CJ'). % Lo   [2] HIRAGANA LETTER SMALL KA..HIRAGANA LETTER SMALL KE
unicode_line_break(0x30A1, 0x30A1, 'CJ'). % Lo       KATAKANA LETTER SMALL A
unicode_line_break(0x30A3, 0x30A3, 'CJ'). % Lo       KATAKANA LETTER SMALL I
unicode_line_break(0x30A5, 0x30A5, 'CJ'). % Lo       KATAKANA LETTER SMALL U
unicode_line_break(0x30A7, 0x30A7, 'CJ'). % Lo       KATAKANA LETTER SMALL E
unicode_line_break(0x30A9, 0x30A9, 'CJ'). % Lo       KATAKANA LETTER SMALL O
unicode_line_break(0x30C3, 0x30C3, 'CJ'). % Lo       KATAKANA LETTER SMALL TU
unicode_line_break(0x30E3, 0x30E3, 'CJ'). % Lo       KATAKANA LETTER SMALL YA
unicode_line_break(0x30E5, 0x30E5, 'CJ'). % Lo       KATAKANA LETTER SMALL YU
unicode_line_break(0x30E7, 0x30E7, 'CJ'). % Lo       KATAKANA LETTER SMALL YO
unicode_line_break(0x30EE, 0x30EE, 'CJ'). % Lo       KATAKANA LETTER SMALL WA
unicode_line_break(0x30F5, 0x30F6, 'CJ'). % Lo   [2] KATAKANA LETTER SMALL KA..KATAKANA LETTER SMALL KE
unicode_line_break(0x30FC, 0x30FC, 'CJ'). % Lm       KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_line_break(0x31F0, 0x31FF, 'CJ'). % Lo  [16] KATAKANA LETTER SMALL KU..KATAKANA LETTER SMALL RO
unicode_line_break(0xFF67, 0xFF6F, 'CJ'). % Lo   [9] HALFWIDTH KATAKANA LETTER SMALL A..HALFWIDTH KATAKANA LETTER SMALL TU
unicode_line_break(0xFF70, 0xFF70, 'CJ'). % Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK

% Total code points: 51

% EOF
