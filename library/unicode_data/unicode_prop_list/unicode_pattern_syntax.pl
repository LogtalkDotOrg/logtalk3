%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 15, 2012
%
%  Original Unicode file header comments follow

/*
# PropList-6.1.0.txt
# Date: 2011-11-30, 01:49:54 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
*/

unicode_pattern_syntax(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_pattern_syntax(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_pattern_syntax(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_pattern_syntax(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_pattern_syntax(0x0021, 0x0023).	% Pattern_Syntax # Po   [3] EXCLAMATION MARK..NUMBER SIGN
unicode_pattern_syntax(0x0024, 0x0024).	% Pattern_Syntax # Sc       DOLLAR SIGN
unicode_pattern_syntax(0x0025, 0x0027).	% Pattern_Syntax # Po   [3] PERCENT SIGN..APOSTROPHE
unicode_pattern_syntax(0x0028, 0x0028).	% Pattern_Syntax # Ps       LEFT PARENTHESIS
unicode_pattern_syntax(0x0029, 0x0029).	% Pattern_Syntax # Pe       RIGHT PARENTHESIS
unicode_pattern_syntax(0x002A, 0x002A).	% Pattern_Syntax # Po       ASTERISK
unicode_pattern_syntax(0x002B, 0x002B).	% Pattern_Syntax # Sm       PLUS SIGN
unicode_pattern_syntax(0x002C, 0x002C).	% Pattern_Syntax # Po       COMMA
unicode_pattern_syntax(0x002D, 0x002D).	% Pattern_Syntax # Pd       HYPHEN-MINUS
unicode_pattern_syntax(0x002E, 0x002F).	% Pattern_Syntax # Po   [2] FULL STOP..SOLIDUS
unicode_pattern_syntax(0x003A, 0x003B).	% Pattern_Syntax # Po   [2] COLON..SEMICOLON
unicode_pattern_syntax(0x003C, 0x003E).	% Pattern_Syntax # Sm   [3] LESS-THAN SIGN..GREATER-THAN SIGN
unicode_pattern_syntax(0x003F, 0x0040).	% Pattern_Syntax # Po   [2] QUESTION MARK..COMMERCIAL AT
unicode_pattern_syntax(0x005B, 0x005B).	% Pattern_Syntax # Ps       LEFT SQUARE BRACKET
unicode_pattern_syntax(0x005C, 0x005C).	% Pattern_Syntax # Po       REVERSE SOLIDUS
unicode_pattern_syntax(0x005D, 0x005D).	% Pattern_Syntax # Pe       RIGHT SQUARE BRACKET
unicode_pattern_syntax(0x005E, 0x005E).	% Pattern_Syntax # Sk       CIRCUMFLEX ACCENT
unicode_pattern_syntax(0x0060, 0x0060).	% Pattern_Syntax # Sk       GRAVE ACCENT
unicode_pattern_syntax(0x007B, 0x007B).	% Pattern_Syntax # Ps       LEFT CURLY BRACKET
unicode_pattern_syntax(0x007C, 0x007C).	% Pattern_Syntax # Sm       VERTICAL LINE
unicode_pattern_syntax(0x007D, 0x007D).	% Pattern_Syntax # Pe       RIGHT CURLY BRACKET
unicode_pattern_syntax(0x007E, 0x007E).	% Pattern_Syntax # Sm       TILDE
unicode_pattern_syntax(0x00A1, 0x00A1).	% Pattern_Syntax # Po       INVERTED EXCLAMATION MARK
unicode_pattern_syntax(0x00A2, 0x00A5).	% Pattern_Syntax # Sc   [4] CENT SIGN..YEN SIGN
unicode_pattern_syntax(0x00A6, 0x00A6).	% Pattern_Syntax # So       BROKEN BAR
unicode_pattern_syntax(0x00A7, 0x00A7).	% Pattern_Syntax # Po       SECTION SIGN
unicode_pattern_syntax(0x00A9, 0x00A9).	% Pattern_Syntax # So       COPYRIGHT SIGN
unicode_pattern_syntax(0x00AB, 0x00AB).	% Pattern_Syntax # Pi       LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_pattern_syntax(0x00AC, 0x00AC).	% Pattern_Syntax # Sm       NOT SIGN
unicode_pattern_syntax(0x00AE, 0x00AE).	% Pattern_Syntax # So       REGISTERED SIGN
unicode_pattern_syntax(0x00B0, 0x00B0).	% Pattern_Syntax # So       DEGREE SIGN
unicode_pattern_syntax(0x00B1, 0x00B1).	% Pattern_Syntax # Sm       PLUS-MINUS SIGN
unicode_pattern_syntax(0x00B6, 0x00B6).	% Pattern_Syntax # Po       PILCROW SIGN
unicode_pattern_syntax(0x00BB, 0x00BB).	% Pattern_Syntax # Pf       RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_pattern_syntax(0x00BF, 0x00BF).	% Pattern_Syntax # Po       INVERTED QUESTION MARK
unicode_pattern_syntax(0x00D7, 0x00D7).	% Pattern_Syntax # Sm       MULTIPLICATION SIGN
unicode_pattern_syntax(0x00F7, 0x00F7).	% Pattern_Syntax # Sm       DIVISION SIGN
unicode_pattern_syntax(0x2010, 0x2015).	% Pattern_Syntax # Pd   [6] HYPHEN..HORIZONTAL BAR
unicode_pattern_syntax(0x2016, 0x2017).	% Pattern_Syntax # Po   [2] DOUBLE VERTICAL LINE..DOUBLE LOW LINE
unicode_pattern_syntax(0x2018, 0x2018).	% Pattern_Syntax # Pi       LEFT SINGLE QUOTATION MARK
unicode_pattern_syntax(0x2019, 0x2019).	% Pattern_Syntax # Pf       RIGHT SINGLE QUOTATION MARK
unicode_pattern_syntax(0x201A, 0x201A).	% Pattern_Syntax # Ps       SINGLE LOW-9 QUOTATION MARK
unicode_pattern_syntax(0x201B, 0x201C).	% Pattern_Syntax # Pi   [2] SINGLE HIGH-REVERSED-9 QUOTATION MARK..LEFT DOUBLE QUOTATION MARK
unicode_pattern_syntax(0x201D, 0x201D).	% Pattern_Syntax # Pf       RIGHT DOUBLE QUOTATION MARK
unicode_pattern_syntax(0x201E, 0x201E).	% Pattern_Syntax # Ps       DOUBLE LOW-9 QUOTATION MARK
unicode_pattern_syntax(0x201F, 0x201F).	% Pattern_Syntax # Pi       DOUBLE HIGH-REVERSED-9 QUOTATION MARK
unicode_pattern_syntax(0x2020, 0x2027).	% Pattern_Syntax # Po   [8] DAGGER..HYPHENATION POINT
unicode_pattern_syntax(0x2030, 0x2038).	% Pattern_Syntax # Po   [9] PER MILLE SIGN..CARET
unicode_pattern_syntax(0x2039, 0x2039).	% Pattern_Syntax # Pi       SINGLE LEFT-POINTING ANGLE QUOTATION MARK
unicode_pattern_syntax(0x203A, 0x203A).	% Pattern_Syntax # Pf       SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
unicode_pattern_syntax(0x203B, 0x203E).	% Pattern_Syntax # Po   [4] REFERENCE MARK..OVERLINE
unicode_pattern_syntax(0x2041, 0x2043).	% Pattern_Syntax # Po   [3] CARET INSERTION POINT..HYPHEN BULLET
unicode_pattern_syntax(0x2044, 0x2044).	% Pattern_Syntax # Sm       FRACTION SLASH
unicode_pattern_syntax(0x2045, 0x2045).	% Pattern_Syntax # Ps       LEFT SQUARE BRACKET WITH QUILL
unicode_pattern_syntax(0x2046, 0x2046).	% Pattern_Syntax # Pe       RIGHT SQUARE BRACKET WITH QUILL
unicode_pattern_syntax(0x2047, 0x2051).	% Pattern_Syntax # Po  [11] DOUBLE QUESTION MARK..TWO ASTERISKS ALIGNED VERTICALLY
unicode_pattern_syntax(0x2052, 0x2052).	% Pattern_Syntax # Sm       COMMERCIAL MINUS SIGN
unicode_pattern_syntax(0x2053, 0x2053).	% Pattern_Syntax # Po       SWUNG DASH
unicode_pattern_syntax(0x2055, 0x205E).	% Pattern_Syntax # Po  [10] FLOWER PUNCTUATION MARK..VERTICAL FOUR DOTS
unicode_pattern_syntax(0x2190, 0x2194).	% Pattern_Syntax # Sm   [5] LEFTWARDS ARROW..LEFT RIGHT ARROW
unicode_pattern_syntax(0x2195, 0x2199).	% Pattern_Syntax # So   [5] UP DOWN ARROW..SOUTH WEST ARROW
unicode_pattern_syntax(0x219A, 0x219B).	% Pattern_Syntax # Sm   [2] LEFTWARDS ARROW WITH STROKE..RIGHTWARDS ARROW WITH STROKE
unicode_pattern_syntax(0x219C, 0x219F).	% Pattern_Syntax # So   [4] LEFTWARDS WAVE ARROW..UPWARDS TWO HEADED ARROW
unicode_pattern_syntax(0x21A0, 0x21A0).	% Pattern_Syntax # Sm       RIGHTWARDS TWO HEADED ARROW
unicode_pattern_syntax(0x21A1, 0x21A2).	% Pattern_Syntax # So   [2] DOWNWARDS TWO HEADED ARROW..LEFTWARDS ARROW WITH TAIL
unicode_pattern_syntax(0x21A3, 0x21A3).	% Pattern_Syntax # Sm       RIGHTWARDS ARROW WITH TAIL
unicode_pattern_syntax(0x21A4, 0x21A5).	% Pattern_Syntax # So   [2] LEFTWARDS ARROW FROM BAR..UPWARDS ARROW FROM BAR
unicode_pattern_syntax(0x21A6, 0x21A6).	% Pattern_Syntax # Sm       RIGHTWARDS ARROW FROM BAR
unicode_pattern_syntax(0x21A7, 0x21AD).	% Pattern_Syntax # So   [7] DOWNWARDS ARROW FROM BAR..LEFT RIGHT WAVE ARROW
unicode_pattern_syntax(0x21AE, 0x21AE).	% Pattern_Syntax # Sm       LEFT RIGHT ARROW WITH STROKE
unicode_pattern_syntax(0x21AF, 0x21CD).	% Pattern_Syntax # So  [31] DOWNWARDS ZIGZAG ARROW..LEFTWARDS DOUBLE ARROW WITH STROKE
unicode_pattern_syntax(0x21CE, 0x21CF).	% Pattern_Syntax # Sm   [2] LEFT RIGHT DOUBLE ARROW WITH STROKE..RIGHTWARDS DOUBLE ARROW WITH STROKE
unicode_pattern_syntax(0x21D0, 0x21D1).	% Pattern_Syntax # So   [2] LEFTWARDS DOUBLE ARROW..UPWARDS DOUBLE ARROW
unicode_pattern_syntax(0x21D2, 0x21D2).	% Pattern_Syntax # Sm       RIGHTWARDS DOUBLE ARROW
unicode_pattern_syntax(0x21D3, 0x21D3).	% Pattern_Syntax # So       DOWNWARDS DOUBLE ARROW
unicode_pattern_syntax(0x21D4, 0x21D4).	% Pattern_Syntax # Sm       LEFT RIGHT DOUBLE ARROW
unicode_pattern_syntax(0x21D5, 0x21F3).	% Pattern_Syntax # So  [31] UP DOWN DOUBLE ARROW..UP DOWN WHITE ARROW
unicode_pattern_syntax(0x21F4, 0x22FF).	% Pattern_Syntax # Sm [268] RIGHT ARROW WITH SMALL CIRCLE..Z NOTATION BAG MEMBERSHIP
unicode_pattern_syntax(0x2300, 0x2307).	% Pattern_Syntax # So   [8] DIAMETER SIGN..WAVY LINE
unicode_pattern_syntax(0x2308, 0x230B).	% Pattern_Syntax # Sm   [4] LEFT CEILING..RIGHT FLOOR
unicode_pattern_syntax(0x230C, 0x231F).	% Pattern_Syntax # So  [20] BOTTOM RIGHT CROP..BOTTOM RIGHT CORNER
unicode_pattern_syntax(0x2320, 0x2321).	% Pattern_Syntax # Sm   [2] TOP HALF INTEGRAL..BOTTOM HALF INTEGRAL
unicode_pattern_syntax(0x2322, 0x2328).	% Pattern_Syntax # So   [7] FROWN..KEYBOARD
unicode_pattern_syntax(0x2329, 0x2329).	% Pattern_Syntax # Ps       LEFT-POINTING ANGLE BRACKET
unicode_pattern_syntax(0x232A, 0x232A).	% Pattern_Syntax # Pe       RIGHT-POINTING ANGLE BRACKET
unicode_pattern_syntax(0x232B, 0x237B).	% Pattern_Syntax # So  [81] ERASE TO THE LEFT..NOT CHECK MARK
unicode_pattern_syntax(0x237C, 0x237C).	% Pattern_Syntax # Sm       RIGHT ANGLE WITH DOWNWARDS ZIGZAG ARROW
unicode_pattern_syntax(0x237D, 0x239A).	% Pattern_Syntax # So  [30] SHOULDERED OPEN BOX..CLEAR SCREEN SYMBOL
unicode_pattern_syntax(0x239B, 0x23B3).	% Pattern_Syntax # Sm  [25] LEFT PARENTHESIS UPPER HOOK..SUMMATION BOTTOM
unicode_pattern_syntax(0x23B4, 0x23DB).	% Pattern_Syntax # So  [40] TOP SQUARE BRACKET..FUSE
unicode_pattern_syntax(0x23DC, 0x23E1).	% Pattern_Syntax # Sm   [6] TOP PARENTHESIS..BOTTOM TORTOISE SHELL BRACKET
unicode_pattern_syntax(0x23E2, 0x23F3).	% Pattern_Syntax # So  [18] WHITE TRAPEZIUM..HOURGLASS WITH FLOWING SAND
unicode_pattern_syntax(0x23F4, 0x23FF).	% Pattern_Syntax # Cn  [12] <reserved-23F4>..<reserved-23FF>
unicode_pattern_syntax(0x2400, 0x2426).	% Pattern_Syntax # So  [39] SYMBOL FOR NULL..SYMBOL FOR SUBSTITUTE FORM TWO
unicode_pattern_syntax(0x2427, 0x243F).	% Pattern_Syntax # Cn  [25] <reserved-2427>..<reserved-243F>
unicode_pattern_syntax(0x2440, 0x244A).	% Pattern_Syntax # So  [11] OCR HOOK..OCR DOUBLE BACKSLASH
unicode_pattern_syntax(0x244B, 0x245F).	% Pattern_Syntax # Cn  [21] <reserved-244B>..<reserved-245F>
unicode_pattern_syntax(0x2500, 0x25B6).	% Pattern_Syntax # So [183] BOX DRAWINGS LIGHT HORIZONTAL..BLACK RIGHT-POINTING TRIANGLE
unicode_pattern_syntax(0x25B7, 0x25B7).	% Pattern_Syntax # Sm       WHITE RIGHT-POINTING TRIANGLE
unicode_pattern_syntax(0x25B8, 0x25C0).	% Pattern_Syntax # So   [9] BLACK RIGHT-POINTING SMALL TRIANGLE..BLACK LEFT-POINTING TRIANGLE
unicode_pattern_syntax(0x25C1, 0x25C1).	% Pattern_Syntax # Sm       WHITE LEFT-POINTING TRIANGLE
unicode_pattern_syntax(0x25C2, 0x25F7).	% Pattern_Syntax # So  [54] BLACK LEFT-POINTING SMALL TRIANGLE..WHITE CIRCLE WITH UPPER RIGHT QUADRANT
unicode_pattern_syntax(0x25F8, 0x25FF).	% Pattern_Syntax # Sm   [8] UPPER LEFT TRIANGLE..LOWER RIGHT TRIANGLE
unicode_pattern_syntax(0x2600, 0x266E).	% Pattern_Syntax # So [111] BLACK SUN WITH RAYS..MUSIC NATURAL SIGN
unicode_pattern_syntax(0x266F, 0x266F).	% Pattern_Syntax # Sm       MUSIC SHARP SIGN
unicode_pattern_syntax(0x2670, 0x26FF).	% Pattern_Syntax # So [144] WEST SYRIAC CROSS..WHITE FLAG WITH HORIZONTAL MIDDLE BLACK STRIPE
unicode_pattern_syntax(0x2700, 0x2700).	% Pattern_Syntax # Cn       <reserved-2700>
unicode_pattern_syntax(0x2701, 0x2767).	% Pattern_Syntax # So [103] UPPER BLADE SCISSORS..ROTATED FLORAL HEART BULLET
unicode_pattern_syntax(0x2768, 0x2768).	% Pattern_Syntax # Ps       MEDIUM LEFT PARENTHESIS ORNAMENT
unicode_pattern_syntax(0x2769, 0x2769).	% Pattern_Syntax # Pe       MEDIUM RIGHT PARENTHESIS ORNAMENT
unicode_pattern_syntax(0x276A, 0x276A).	% Pattern_Syntax # Ps       MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT
unicode_pattern_syntax(0x276B, 0x276B).	% Pattern_Syntax # Pe       MEDIUM FLATTENED RIGHT PARENTHESIS ORNAMENT
unicode_pattern_syntax(0x276C, 0x276C).	% Pattern_Syntax # Ps       MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT
unicode_pattern_syntax(0x276D, 0x276D).	% Pattern_Syntax # Pe       MEDIUM RIGHT-POINTING ANGLE BRACKET ORNAMENT
unicode_pattern_syntax(0x276E, 0x276E).	% Pattern_Syntax # Ps       HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT
unicode_pattern_syntax(0x276F, 0x276F).	% Pattern_Syntax # Pe       HEAVY RIGHT-POINTING ANGLE QUOTATION MARK ORNAMENT
unicode_pattern_syntax(0x2770, 0x2770).	% Pattern_Syntax # Ps       HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT
unicode_pattern_syntax(0x2771, 0x2771).	% Pattern_Syntax # Pe       HEAVY RIGHT-POINTING ANGLE BRACKET ORNAMENT
unicode_pattern_syntax(0x2772, 0x2772).	% Pattern_Syntax # Ps       LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT
unicode_pattern_syntax(0x2773, 0x2773).	% Pattern_Syntax # Pe       LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT
unicode_pattern_syntax(0x2774, 0x2774).	% Pattern_Syntax # Ps       MEDIUM LEFT CURLY BRACKET ORNAMENT
unicode_pattern_syntax(0x2775, 0x2775).	% Pattern_Syntax # Pe       MEDIUM RIGHT CURLY BRACKET ORNAMENT
unicode_pattern_syntax(0x2794, 0x27BF).	% Pattern_Syntax # So  [44] HEAVY WIDE-HEADED RIGHTWARDS ARROW..DOUBLE CURLY LOOP
unicode_pattern_syntax(0x27C0, 0x27C4).	% Pattern_Syntax # Sm   [5] THREE DIMENSIONAL ANGLE..OPEN SUPERSET
unicode_pattern_syntax(0x27C5, 0x27C5).	% Pattern_Syntax # Ps       LEFT S-SHAPED BAG DELIMITER
unicode_pattern_syntax(0x27C6, 0x27C6).	% Pattern_Syntax # Pe       RIGHT S-SHAPED BAG DELIMITER
unicode_pattern_syntax(0x27C7, 0x27E5).	% Pattern_Syntax # Sm  [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
unicode_pattern_syntax(0x27E6, 0x27E6).	% Pattern_Syntax # Ps       MATHEMATICAL LEFT WHITE SQUARE BRACKET
unicode_pattern_syntax(0x27E7, 0x27E7).	% Pattern_Syntax # Pe       MATHEMATICAL RIGHT WHITE SQUARE BRACKET
unicode_pattern_syntax(0x27E8, 0x27E8).	% Pattern_Syntax # Ps       MATHEMATICAL LEFT ANGLE BRACKET
unicode_pattern_syntax(0x27E9, 0x27E9).	% Pattern_Syntax # Pe       MATHEMATICAL RIGHT ANGLE BRACKET
unicode_pattern_syntax(0x27EA, 0x27EA).	% Pattern_Syntax # Ps       MATHEMATICAL LEFT DOUBLE ANGLE BRACKET
unicode_pattern_syntax(0x27EB, 0x27EB).	% Pattern_Syntax # Pe       MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET
unicode_pattern_syntax(0x27EC, 0x27EC).	% Pattern_Syntax # Ps       MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET
unicode_pattern_syntax(0x27ED, 0x27ED).	% Pattern_Syntax # Pe       MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET
unicode_pattern_syntax(0x27EE, 0x27EE).	% Pattern_Syntax # Ps       MATHEMATICAL LEFT FLATTENED PARENTHESIS
unicode_pattern_syntax(0x27EF, 0x27EF).	% Pattern_Syntax # Pe       MATHEMATICAL RIGHT FLATTENED PARENTHESIS
unicode_pattern_syntax(0x27F0, 0x27FF).	% Pattern_Syntax # Sm  [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
unicode_pattern_syntax(0x2800, 0x28FF).	% Pattern_Syntax # So [256] BRAILLE PATTERN BLANK..BRAILLE PATTERN DOTS-12345678
unicode_pattern_syntax(0x2900, 0x2982).	% Pattern_Syntax # Sm [131] RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE..Z NOTATION TYPE COLON
unicode_pattern_syntax(0x2983, 0x2983).	% Pattern_Syntax # Ps       LEFT WHITE CURLY BRACKET
unicode_pattern_syntax(0x2984, 0x2984).	% Pattern_Syntax # Pe       RIGHT WHITE CURLY BRACKET
unicode_pattern_syntax(0x2985, 0x2985).	% Pattern_Syntax # Ps       LEFT WHITE PARENTHESIS
unicode_pattern_syntax(0x2986, 0x2986).	% Pattern_Syntax # Pe       RIGHT WHITE PARENTHESIS
unicode_pattern_syntax(0x2987, 0x2987).	% Pattern_Syntax # Ps       Z NOTATION LEFT IMAGE BRACKET
unicode_pattern_syntax(0x2988, 0x2988).	% Pattern_Syntax # Pe       Z NOTATION RIGHT IMAGE BRACKET
unicode_pattern_syntax(0x2989, 0x2989).	% Pattern_Syntax # Ps       Z NOTATION LEFT BINDING BRACKET
unicode_pattern_syntax(0x298A, 0x298A).	% Pattern_Syntax # Pe       Z NOTATION RIGHT BINDING BRACKET
unicode_pattern_syntax(0x298B, 0x298B).	% Pattern_Syntax # Ps       LEFT SQUARE BRACKET WITH UNDERBAR
unicode_pattern_syntax(0x298C, 0x298C).	% Pattern_Syntax # Pe       RIGHT SQUARE BRACKET WITH UNDERBAR
unicode_pattern_syntax(0x298D, 0x298D).	% Pattern_Syntax # Ps       LEFT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_pattern_syntax(0x298E, 0x298E).	% Pattern_Syntax # Pe       RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_pattern_syntax(0x298F, 0x298F).	% Pattern_Syntax # Ps       LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_pattern_syntax(0x2990, 0x2990).	% Pattern_Syntax # Pe       RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_pattern_syntax(0x2991, 0x2991).	% Pattern_Syntax # Ps       LEFT ANGLE BRACKET WITH DOT
unicode_pattern_syntax(0x2992, 0x2992).	% Pattern_Syntax # Pe       RIGHT ANGLE BRACKET WITH DOT
unicode_pattern_syntax(0x2993, 0x2993).	% Pattern_Syntax # Ps       LEFT ARC LESS-THAN BRACKET
unicode_pattern_syntax(0x2994, 0x2994).	% Pattern_Syntax # Pe       RIGHT ARC GREATER-THAN BRACKET
unicode_pattern_syntax(0x2995, 0x2995).	% Pattern_Syntax # Ps       DOUBLE LEFT ARC GREATER-THAN BRACKET
unicode_pattern_syntax(0x2996, 0x2996).	% Pattern_Syntax # Pe       DOUBLE RIGHT ARC LESS-THAN BRACKET
unicode_pattern_syntax(0x2997, 0x2997).	% Pattern_Syntax # Ps       LEFT BLACK TORTOISE SHELL BRACKET
unicode_pattern_syntax(0x2998, 0x2998).	% Pattern_Syntax # Pe       RIGHT BLACK TORTOISE SHELL BRACKET
unicode_pattern_syntax(0x2999, 0x29D7).	% Pattern_Syntax # Sm  [63] DOTTED FENCE..BLACK HOURGLASS
unicode_pattern_syntax(0x29D8, 0x29D8).	% Pattern_Syntax # Ps       LEFT WIGGLY FENCE
unicode_pattern_syntax(0x29D9, 0x29D9).	% Pattern_Syntax # Pe       RIGHT WIGGLY FENCE
unicode_pattern_syntax(0x29DA, 0x29DA).	% Pattern_Syntax # Ps       LEFT DOUBLE WIGGLY FENCE
unicode_pattern_syntax(0x29DB, 0x29DB).	% Pattern_Syntax # Pe       RIGHT DOUBLE WIGGLY FENCE
unicode_pattern_syntax(0x29DC, 0x29FB).	% Pattern_Syntax # Sm  [32] INCOMPLETE INFINITY..TRIPLE PLUS
unicode_pattern_syntax(0x29FC, 0x29FC).	% Pattern_Syntax # Ps       LEFT-POINTING CURVED ANGLE BRACKET
unicode_pattern_syntax(0x29FD, 0x29FD).	% Pattern_Syntax # Pe       RIGHT-POINTING CURVED ANGLE BRACKET
unicode_pattern_syntax(0x29FE, 0x2AFF).	% Pattern_Syntax # Sm [258] TINY..N-ARY WHITE VERTICAL BAR
unicode_pattern_syntax(0x2B00, 0x2B2F).	% Pattern_Syntax # So  [48] NORTH EAST WHITE ARROW..WHITE VERTICAL ELLIPSE
unicode_pattern_syntax(0x2B30, 0x2B44).	% Pattern_Syntax # Sm  [21] LEFT ARROW WITH SMALL CIRCLE..RIGHTWARDS ARROW THROUGH SUPERSET
unicode_pattern_syntax(0x2B45, 0x2B46).	% Pattern_Syntax # So   [2] LEFTWARDS QUADRUPLE ARROW..RIGHTWARDS QUADRUPLE ARROW
unicode_pattern_syntax(0x2B47, 0x2B4C).	% Pattern_Syntax # Sm   [6] REVERSE TILDE OPERATOR ABOVE RIGHTWARDS ARROW..RIGHTWARDS ARROW ABOVE REVERSE TILDE OPERATOR
unicode_pattern_syntax(0x2B4D, 0x2B4F).	% Pattern_Syntax # Cn   [3] <reserved-2B4D>..<reserved-2B4F>
unicode_pattern_syntax(0x2B50, 0x2B59).	% Pattern_Syntax # So  [10] WHITE MEDIUM STAR..HEAVY CIRCLED SALTIRE
unicode_pattern_syntax(0x2B5A, 0x2BFF).	% Pattern_Syntax # Cn [166] <reserved-2B5A>..<reserved-2BFF>
unicode_pattern_syntax(0x2E00, 0x2E01).	% Pattern_Syntax # Po   [2] RIGHT ANGLE SUBSTITUTION MARKER..RIGHT ANGLE DOTTED SUBSTITUTION MARKER
unicode_pattern_syntax(0x2E02, 0x2E02).	% Pattern_Syntax # Pi       LEFT SUBSTITUTION BRACKET
unicode_pattern_syntax(0x2E03, 0x2E03).	% Pattern_Syntax # Pf       RIGHT SUBSTITUTION BRACKET
unicode_pattern_syntax(0x2E04, 0x2E04).	% Pattern_Syntax # Pi       LEFT DOTTED SUBSTITUTION BRACKET
unicode_pattern_syntax(0x2E05, 0x2E05).	% Pattern_Syntax # Pf       RIGHT DOTTED SUBSTITUTION BRACKET
unicode_pattern_syntax(0x2E06, 0x2E08).	% Pattern_Syntax # Po   [3] RAISED INTERPOLATION MARKER..DOTTED TRANSPOSITION MARKER
unicode_pattern_syntax(0x2E09, 0x2E09).	% Pattern_Syntax # Pi       LEFT TRANSPOSITION BRACKET
unicode_pattern_syntax(0x2E0A, 0x2E0A).	% Pattern_Syntax # Pf       RIGHT TRANSPOSITION BRACKET
unicode_pattern_syntax(0x2E0B, 0x2E0B).	% Pattern_Syntax # Po       RAISED SQUARE
unicode_pattern_syntax(0x2E0C, 0x2E0C).	% Pattern_Syntax # Pi       LEFT RAISED OMISSION BRACKET
unicode_pattern_syntax(0x2E0D, 0x2E0D).	% Pattern_Syntax # Pf       RIGHT RAISED OMISSION BRACKET
unicode_pattern_syntax(0x2E0E, 0x2E16).	% Pattern_Syntax # Po   [9] EDITORIAL CORONIS..DOTTED RIGHT-POINTING ANGLE
unicode_pattern_syntax(0x2E17, 0x2E17).	% Pattern_Syntax # Pd       DOUBLE OBLIQUE HYPHEN
unicode_pattern_syntax(0x2E18, 0x2E19).	% Pattern_Syntax # Po   [2] INVERTED INTERROBANG..PALM BRANCH
unicode_pattern_syntax(0x2E1A, 0x2E1A).	% Pattern_Syntax # Pd       HYPHEN WITH DIAERESIS
unicode_pattern_syntax(0x2E1B, 0x2E1B).	% Pattern_Syntax # Po       TILDE WITH RING ABOVE
unicode_pattern_syntax(0x2E1C, 0x2E1C).	% Pattern_Syntax # Pi       LEFT LOW PARAPHRASE BRACKET
unicode_pattern_syntax(0x2E1D, 0x2E1D).	% Pattern_Syntax # Pf       RIGHT LOW PARAPHRASE BRACKET
unicode_pattern_syntax(0x2E1E, 0x2E1F).	% Pattern_Syntax # Po   [2] TILDE WITH DOT ABOVE..TILDE WITH DOT BELOW
unicode_pattern_syntax(0x2E20, 0x2E20).	% Pattern_Syntax # Pi       LEFT VERTICAL BAR WITH QUILL
unicode_pattern_syntax(0x2E21, 0x2E21).	% Pattern_Syntax # Pf       RIGHT VERTICAL BAR WITH QUILL
unicode_pattern_syntax(0x2E22, 0x2E22).	% Pattern_Syntax # Ps       TOP LEFT HALF BRACKET
unicode_pattern_syntax(0x2E23, 0x2E23).	% Pattern_Syntax # Pe       TOP RIGHT HALF BRACKET
unicode_pattern_syntax(0x2E24, 0x2E24).	% Pattern_Syntax # Ps       BOTTOM LEFT HALF BRACKET
unicode_pattern_syntax(0x2E25, 0x2E25).	% Pattern_Syntax # Pe       BOTTOM RIGHT HALF BRACKET
unicode_pattern_syntax(0x2E26, 0x2E26).	% Pattern_Syntax # Ps       LEFT SIDEWAYS U BRACKET
unicode_pattern_syntax(0x2E27, 0x2E27).	% Pattern_Syntax # Pe       RIGHT SIDEWAYS U BRACKET
unicode_pattern_syntax(0x2E28, 0x2E28).	% Pattern_Syntax # Ps       LEFT DOUBLE PARENTHESIS
unicode_pattern_syntax(0x2E29, 0x2E29).	% Pattern_Syntax # Pe       RIGHT DOUBLE PARENTHESIS
unicode_pattern_syntax(0x2E2A, 0x2E2E).	% Pattern_Syntax # Po   [5] TWO DOTS OVER ONE DOT PUNCTUATION..REVERSED QUESTION MARK
unicode_pattern_syntax(0x2E2F, 0x2E2F).	% Pattern_Syntax # Lm       VERTICAL TILDE
unicode_pattern_syntax(0x2E30, 0x2E39).	% Pattern_Syntax # Po  [10] RING POINT..TOP HALF SECTION SIGN
unicode_pattern_syntax(0x2E3A, 0x2E3B).	% Pattern_Syntax # Pd   [2] TWO-EM DASH..THREE-EM DASH
unicode_pattern_syntax(0x2E3C, 0x2E7F).	% Pattern_Syntax # Cn  [68] <reserved-2E3C>..<reserved-2E7F>
unicode_pattern_syntax(0x3001, 0x3003).	% Pattern_Syntax # Po   [3] IDEOGRAPHIC COMMA..DITTO MARK
unicode_pattern_syntax(0x3008, 0x3008).	% Pattern_Syntax # Ps       LEFT ANGLE BRACKET
unicode_pattern_syntax(0x3009, 0x3009).	% Pattern_Syntax # Pe       RIGHT ANGLE BRACKET
unicode_pattern_syntax(0x300A, 0x300A).	% Pattern_Syntax # Ps       LEFT DOUBLE ANGLE BRACKET
unicode_pattern_syntax(0x300B, 0x300B).	% Pattern_Syntax # Pe       RIGHT DOUBLE ANGLE BRACKET
unicode_pattern_syntax(0x300C, 0x300C).	% Pattern_Syntax # Ps       LEFT CORNER BRACKET
unicode_pattern_syntax(0x300D, 0x300D).	% Pattern_Syntax # Pe       RIGHT CORNER BRACKET
unicode_pattern_syntax(0x300E, 0x300E).	% Pattern_Syntax # Ps       LEFT WHITE CORNER BRACKET
unicode_pattern_syntax(0x300F, 0x300F).	% Pattern_Syntax # Pe       RIGHT WHITE CORNER BRACKET
unicode_pattern_syntax(0x3010, 0x3010).	% Pattern_Syntax # Ps       LEFT BLACK LENTICULAR BRACKET
unicode_pattern_syntax(0x3011, 0x3011).	% Pattern_Syntax # Pe       RIGHT BLACK LENTICULAR BRACKET
unicode_pattern_syntax(0x3012, 0x3013).	% Pattern_Syntax # So   [2] POSTAL MARK..GETA MARK
unicode_pattern_syntax(0x3014, 0x3014).	% Pattern_Syntax # Ps       LEFT TORTOISE SHELL BRACKET
unicode_pattern_syntax(0x3015, 0x3015).	% Pattern_Syntax # Pe       RIGHT TORTOISE SHELL BRACKET
unicode_pattern_syntax(0x3016, 0x3016).	% Pattern_Syntax # Ps       LEFT WHITE LENTICULAR BRACKET
unicode_pattern_syntax(0x3017, 0x3017).	% Pattern_Syntax # Pe       RIGHT WHITE LENTICULAR BRACKET
unicode_pattern_syntax(0x3018, 0x3018).	% Pattern_Syntax # Ps       LEFT WHITE TORTOISE SHELL BRACKET
unicode_pattern_syntax(0x3019, 0x3019).	% Pattern_Syntax # Pe       RIGHT WHITE TORTOISE SHELL BRACKET
unicode_pattern_syntax(0x301A, 0x301A).	% Pattern_Syntax # Ps       LEFT WHITE SQUARE BRACKET
unicode_pattern_syntax(0x301B, 0x301B).	% Pattern_Syntax # Pe       RIGHT WHITE SQUARE BRACKET
unicode_pattern_syntax(0x301C, 0x301C).	% Pattern_Syntax # Pd       WAVE DASH
unicode_pattern_syntax(0x301D, 0x301D).	% Pattern_Syntax # Ps       REVERSED DOUBLE PRIME QUOTATION MARK
unicode_pattern_syntax(0x301E, 0x301F).	% Pattern_Syntax # Pe   [2] DOUBLE PRIME QUOTATION MARK..LOW DOUBLE PRIME QUOTATION MARK
unicode_pattern_syntax(0x3020, 0x3020).	% Pattern_Syntax # So       POSTAL MARK FACE
unicode_pattern_syntax(0x3030, 0x3030).	% Pattern_Syntax # Pd       WAVY DASH
unicode_pattern_syntax(0xFD3E, 0xFD3E).	% Pattern_Syntax # Ps       ORNATE LEFT PARENTHESIS
unicode_pattern_syntax(0xFD3F, 0xFD3F).	% Pattern_Syntax # Pe       ORNATE RIGHT PARENTHESIS
unicode_pattern_syntax(0xFE45, 0xFE46).	% Pattern_Syntax # Po   [2] SESAME DOT..WHITE SESAME DOT

% Total code points: 2760
