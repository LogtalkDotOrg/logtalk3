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

unicode_other_math(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_other_math(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_other_math(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_other_math(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_other_math(0x005E, 0x005E).	% Other_Math # Sk       CIRCUMFLEX ACCENT
unicode_other_math(0x03D0, 0x03D2).	% Other_Math # L&   [3] GREEK BETA SYMBOL..GREEK UPSILON WITH HOOK SYMBOL
unicode_other_math(0x03D5, 0x03D5).	% Other_Math # L&       GREEK PHI SYMBOL
unicode_other_math(0x03F0, 0x03F1).	% Other_Math # L&   [2] GREEK KAPPA SYMBOL..GREEK RHO SYMBOL
unicode_other_math(0x03F4, 0x03F5).	% Other_Math # L&   [2] GREEK CAPITAL THETA SYMBOL..GREEK LUNATE EPSILON SYMBOL
unicode_other_math(0x2016, 0x2016).	% Other_Math # Po       DOUBLE VERTICAL LINE
unicode_other_math(0x2032, 0x2034).	% Other_Math # Po   [3] PRIME..TRIPLE PRIME
unicode_other_math(0x2040, 0x2040).	% Other_Math # Pc       CHARACTER TIE
unicode_other_math(0x2061, 0x2064).	% Other_Math # Cf   [4] FUNCTION APPLICATION..INVISIBLE PLUS
unicode_other_math(0x207D, 0x207D).	% Other_Math # Ps       SUPERSCRIPT LEFT PARENTHESIS
unicode_other_math(0x207E, 0x207E).	% Other_Math # Pe       SUPERSCRIPT RIGHT PARENTHESIS
unicode_other_math(0x208D, 0x208D).	% Other_Math # Ps       SUBSCRIPT LEFT PARENTHESIS
unicode_other_math(0x208E, 0x208E).	% Other_Math # Pe       SUBSCRIPT RIGHT PARENTHESIS
unicode_other_math(0x20D0, 0x20DC).	% Other_Math # Mn  [13] COMBINING LEFT HARPOON ABOVE..COMBINING FOUR DOTS ABOVE
unicode_other_math(0x20E1, 0x20E1).	% Other_Math # Mn       COMBINING LEFT RIGHT ARROW ABOVE
unicode_other_math(0x20E5, 0x20E6).	% Other_Math # Mn   [2] COMBINING REVERSE SOLIDUS OVERLAY..COMBINING DOUBLE VERTICAL STROKE OVERLAY
unicode_other_math(0x20EB, 0x20EF).	% Other_Math # Mn   [5] COMBINING LONG DOUBLE SOLIDUS OVERLAY..COMBINING RIGHT ARROW BELOW
unicode_other_math(0x2102, 0x2102).	% Other_Math # L&       DOUBLE-STRUCK CAPITAL C
unicode_other_math(0x2107, 0x2107).	% Other_Math # L&       EULER CONSTANT
unicode_other_math(0x210A, 0x2113).	% Other_Math # L&  [10] SCRIPT SMALL G..SCRIPT SMALL L
unicode_other_math(0x2115, 0x2115).	% Other_Math # L&       DOUBLE-STRUCK CAPITAL N
unicode_other_math(0x2119, 0x211D).	% Other_Math # L&   [5] DOUBLE-STRUCK CAPITAL P..DOUBLE-STRUCK CAPITAL R
unicode_other_math(0x2124, 0x2124).	% Other_Math # L&       DOUBLE-STRUCK CAPITAL Z
unicode_other_math(0x2128, 0x2128).	% Other_Math # L&       BLACK-LETTER CAPITAL Z
unicode_other_math(0x2129, 0x2129).	% Other_Math # So       TURNED GREEK SMALL LETTER IOTA
unicode_other_math(0x212C, 0x212D).	% Other_Math # L&   [2] SCRIPT CAPITAL B..BLACK-LETTER CAPITAL C
unicode_other_math(0x212F, 0x2131).	% Other_Math # L&   [3] SCRIPT SMALL E..SCRIPT CAPITAL F
unicode_other_math(0x2133, 0x2134).	% Other_Math # L&   [2] SCRIPT CAPITAL M..SCRIPT SMALL O
unicode_other_math(0x2135, 0x2138).	% Other_Math # Lo   [4] ALEF SYMBOL..DALET SYMBOL
unicode_other_math(0x213C, 0x213F).	% Other_Math # L&   [4] DOUBLE-STRUCK SMALL PI..DOUBLE-STRUCK CAPITAL PI
unicode_other_math(0x2145, 0x2149).	% Other_Math # L&   [5] DOUBLE-STRUCK ITALIC CAPITAL D..DOUBLE-STRUCK ITALIC SMALL J
unicode_other_math(0x2195, 0x2199).	% Other_Math # So   [5] UP DOWN ARROW..SOUTH WEST ARROW
unicode_other_math(0x219C, 0x219F).	% Other_Math # So   [4] LEFTWARDS WAVE ARROW..UPWARDS TWO HEADED ARROW
unicode_other_math(0x21A1, 0x21A2).	% Other_Math # So   [2] DOWNWARDS TWO HEADED ARROW..LEFTWARDS ARROW WITH TAIL
unicode_other_math(0x21A4, 0x21A5).	% Other_Math # So   [2] LEFTWARDS ARROW FROM BAR..UPWARDS ARROW FROM BAR
unicode_other_math(0x21A7, 0x21A7).	% Other_Math # So       DOWNWARDS ARROW FROM BAR
unicode_other_math(0x21A9, 0x21AD).	% Other_Math # So   [5] LEFTWARDS ARROW WITH HOOK..LEFT RIGHT WAVE ARROW
unicode_other_math(0x21B0, 0x21B1).	% Other_Math # So   [2] UPWARDS ARROW WITH TIP LEFTWARDS..UPWARDS ARROW WITH TIP RIGHTWARDS
unicode_other_math(0x21B6, 0x21B7).	% Other_Math # So   [2] ANTICLOCKWISE TOP SEMICIRCLE ARROW..CLOCKWISE TOP SEMICIRCLE ARROW
unicode_other_math(0x21BC, 0x21CD).	% Other_Math # So  [18] LEFTWARDS HARPOON WITH BARB UPWARDS..LEFTWARDS DOUBLE ARROW WITH STROKE
unicode_other_math(0x21D0, 0x21D1).	% Other_Math # So   [2] LEFTWARDS DOUBLE ARROW..UPWARDS DOUBLE ARROW
unicode_other_math(0x21D3, 0x21D3).	% Other_Math # So       DOWNWARDS DOUBLE ARROW
unicode_other_math(0x21D5, 0x21DB).	% Other_Math # So   [7] UP DOWN DOUBLE ARROW..RIGHTWARDS TRIPLE ARROW
unicode_other_math(0x21DD, 0x21DD).	% Other_Math # So       RIGHTWARDS SQUIGGLE ARROW
unicode_other_math(0x21E4, 0x21E5).	% Other_Math # So   [2] LEFTWARDS ARROW TO BAR..RIGHTWARDS ARROW TO BAR
unicode_other_math(0x23B4, 0x23B5).	% Other_Math # So   [2] TOP SQUARE BRACKET..BOTTOM SQUARE BRACKET
unicode_other_math(0x23B7, 0x23B7).	% Other_Math # So       RADICAL SYMBOL BOTTOM
unicode_other_math(0x23D0, 0x23D0).	% Other_Math # So       VERTICAL LINE EXTENSION
unicode_other_math(0x23E2, 0x23E2).	% Other_Math # So       WHITE TRAPEZIUM
unicode_other_math(0x25A0, 0x25A1).	% Other_Math # So   [2] BLACK SQUARE..WHITE SQUARE
unicode_other_math(0x25AE, 0x25B6).	% Other_Math # So   [9] BLACK VERTICAL RECTANGLE..BLACK RIGHT-POINTING TRIANGLE
unicode_other_math(0x25BC, 0x25C0).	% Other_Math # So   [5] BLACK DOWN-POINTING TRIANGLE..BLACK LEFT-POINTING TRIANGLE
unicode_other_math(0x25C6, 0x25C7).	% Other_Math # So   [2] BLACK DIAMOND..WHITE DIAMOND
unicode_other_math(0x25CA, 0x25CB).	% Other_Math # So   [2] LOZENGE..WHITE CIRCLE
unicode_other_math(0x25CF, 0x25D3).	% Other_Math # So   [5] BLACK CIRCLE..CIRCLE WITH UPPER HALF BLACK
unicode_other_math(0x25E2, 0x25E2).	% Other_Math # So       BLACK LOWER RIGHT TRIANGLE
unicode_other_math(0x25E4, 0x25E4).	% Other_Math # So       BLACK UPPER LEFT TRIANGLE
unicode_other_math(0x25E7, 0x25EC).	% Other_Math # So   [6] SQUARE WITH LEFT HALF BLACK..WHITE UP-POINTING TRIANGLE WITH DOT
unicode_other_math(0x2605, 0x2606).	% Other_Math # So   [2] BLACK STAR..WHITE STAR
unicode_other_math(0x2640, 0x2640).	% Other_Math # So       FEMALE SIGN
unicode_other_math(0x2642, 0x2642).	% Other_Math # So       MALE SIGN
unicode_other_math(0x2660, 0x2663).	% Other_Math # So   [4] BLACK SPADE SUIT..BLACK CLUB SUIT
unicode_other_math(0x266D, 0x266E).	% Other_Math # So   [2] MUSIC FLAT SIGN..MUSIC NATURAL SIGN
unicode_other_math(0x27C5, 0x27C5).	% Other_Math # Ps       LEFT S-SHAPED BAG DELIMITER
unicode_other_math(0x27C6, 0x27C6).	% Other_Math # Pe       RIGHT S-SHAPED BAG DELIMITER
unicode_other_math(0x27E6, 0x27E6).	% Other_Math # Ps       MATHEMATICAL LEFT WHITE SQUARE BRACKET
unicode_other_math(0x27E7, 0x27E7).	% Other_Math # Pe       MATHEMATICAL RIGHT WHITE SQUARE BRACKET
unicode_other_math(0x27E8, 0x27E8).	% Other_Math # Ps       MATHEMATICAL LEFT ANGLE BRACKET
unicode_other_math(0x27E9, 0x27E9).	% Other_Math # Pe       MATHEMATICAL RIGHT ANGLE BRACKET
unicode_other_math(0x27EA, 0x27EA).	% Other_Math # Ps       MATHEMATICAL LEFT DOUBLE ANGLE BRACKET
unicode_other_math(0x27EB, 0x27EB).	% Other_Math # Pe       MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET
unicode_other_math(0x27EC, 0x27EC).	% Other_Math # Ps       MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET
unicode_other_math(0x27ED, 0x27ED).	% Other_Math # Pe       MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET
unicode_other_math(0x27EE, 0x27EE).	% Other_Math # Ps       MATHEMATICAL LEFT FLATTENED PARENTHESIS
unicode_other_math(0x27EF, 0x27EF).	% Other_Math # Pe       MATHEMATICAL RIGHT FLATTENED PARENTHESIS
unicode_other_math(0x2983, 0x2983).	% Other_Math # Ps       LEFT WHITE CURLY BRACKET
unicode_other_math(0x2984, 0x2984).	% Other_Math # Pe       RIGHT WHITE CURLY BRACKET
unicode_other_math(0x2985, 0x2985).	% Other_Math # Ps       LEFT WHITE PARENTHESIS
unicode_other_math(0x2986, 0x2986).	% Other_Math # Pe       RIGHT WHITE PARENTHESIS
unicode_other_math(0x2987, 0x2987).	% Other_Math # Ps       Z NOTATION LEFT IMAGE BRACKET
unicode_other_math(0x2988, 0x2988).	% Other_Math # Pe       Z NOTATION RIGHT IMAGE BRACKET
unicode_other_math(0x2989, 0x2989).	% Other_Math # Ps       Z NOTATION LEFT BINDING BRACKET
unicode_other_math(0x298A, 0x298A).	% Other_Math # Pe       Z NOTATION RIGHT BINDING BRACKET
unicode_other_math(0x298B, 0x298B).	% Other_Math # Ps       LEFT SQUARE BRACKET WITH UNDERBAR
unicode_other_math(0x298C, 0x298C).	% Other_Math # Pe       RIGHT SQUARE BRACKET WITH UNDERBAR
unicode_other_math(0x298D, 0x298D).	% Other_Math # Ps       LEFT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_other_math(0x298E, 0x298E).	% Other_Math # Pe       RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_other_math(0x298F, 0x298F).	% Other_Math # Ps       LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_other_math(0x2990, 0x2990).	% Other_Math # Pe       RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_other_math(0x2991, 0x2991).	% Other_Math # Ps       LEFT ANGLE BRACKET WITH DOT
unicode_other_math(0x2992, 0x2992).	% Other_Math # Pe       RIGHT ANGLE BRACKET WITH DOT
unicode_other_math(0x2993, 0x2993).	% Other_Math # Ps       LEFT ARC LESS-THAN BRACKET
unicode_other_math(0x2994, 0x2994).	% Other_Math # Pe       RIGHT ARC GREATER-THAN BRACKET
unicode_other_math(0x2995, 0x2995).	% Other_Math # Ps       DOUBLE LEFT ARC GREATER-THAN BRACKET
unicode_other_math(0x2996, 0x2996).	% Other_Math # Pe       DOUBLE RIGHT ARC LESS-THAN BRACKET
unicode_other_math(0x2997, 0x2997).	% Other_Math # Ps       LEFT BLACK TORTOISE SHELL BRACKET
unicode_other_math(0x2998, 0x2998).	% Other_Math # Pe       RIGHT BLACK TORTOISE SHELL BRACKET
unicode_other_math(0x29D8, 0x29D8).	% Other_Math # Ps       LEFT WIGGLY FENCE
unicode_other_math(0x29D9, 0x29D9).	% Other_Math # Pe       RIGHT WIGGLY FENCE
unicode_other_math(0x29DA, 0x29DA).	% Other_Math # Ps       LEFT DOUBLE WIGGLY FENCE
unicode_other_math(0x29DB, 0x29DB).	% Other_Math # Pe       RIGHT DOUBLE WIGGLY FENCE
unicode_other_math(0x29FC, 0x29FC).	% Other_Math # Ps       LEFT-POINTING CURVED ANGLE BRACKET
unicode_other_math(0x29FD, 0x29FD).	% Other_Math # Pe       RIGHT-POINTING CURVED ANGLE BRACKET
unicode_other_math(0xFE61, 0xFE61).	% Other_Math # Po       SMALL ASTERISK
unicode_other_math(0xFE63, 0xFE63).	% Other_Math # Pd       SMALL HYPHEN-MINUS
unicode_other_math(0xFE68, 0xFE68).	% Other_Math # Po       SMALL REVERSE SOLIDUS
unicode_other_math(0xFF3C, 0xFF3C).	% Other_Math # Po       FULLWIDTH REVERSE SOLIDUS
unicode_other_math(0xFF3E, 0xFF3E).	% Other_Math # Sk       FULLWIDTH CIRCUMFLEX ACCENT
unicode_other_math(0x1D400, 0x1D454).	% Other_Math # L&  [85] MATHEMATICAL BOLD CAPITAL A..MATHEMATICAL ITALIC SMALL G
unicode_other_math(0x1D456, 0x1D49C).	% Other_Math # L&  [71] MATHEMATICAL ITALIC SMALL I..MATHEMATICAL SCRIPT CAPITAL A
unicode_other_math(0x1D49E, 0x1D49F).	% Other_Math # L&   [2] MATHEMATICAL SCRIPT CAPITAL C..MATHEMATICAL SCRIPT CAPITAL D
unicode_other_math(0x1D4A2, 0x1D4A2).	% Other_Math # L&       MATHEMATICAL SCRIPT CAPITAL G
unicode_other_math(0x1D4A5, 0x1D4A6).	% Other_Math # L&   [2] MATHEMATICAL SCRIPT CAPITAL J..MATHEMATICAL SCRIPT CAPITAL K
unicode_other_math(0x1D4A9, 0x1D4AC).	% Other_Math # L&   [4] MATHEMATICAL SCRIPT CAPITAL N..MATHEMATICAL SCRIPT CAPITAL Q
unicode_other_math(0x1D4AE, 0x1D4B9).	% Other_Math # L&  [12] MATHEMATICAL SCRIPT CAPITAL S..MATHEMATICAL SCRIPT SMALL D
unicode_other_math(0x1D4BB, 0x1D4BB).	% Other_Math # L&       MATHEMATICAL SCRIPT SMALL F
unicode_other_math(0x1D4BD, 0x1D4C3).	% Other_Math # L&   [7] MATHEMATICAL SCRIPT SMALL H..MATHEMATICAL SCRIPT SMALL N
unicode_other_math(0x1D4C5, 0x1D505).	% Other_Math # L&  [65] MATHEMATICAL SCRIPT SMALL P..MATHEMATICAL FRAKTUR CAPITAL B
unicode_other_math(0x1D507, 0x1D50A).	% Other_Math # L&   [4] MATHEMATICAL FRAKTUR CAPITAL D..MATHEMATICAL FRAKTUR CAPITAL G
unicode_other_math(0x1D50D, 0x1D514).	% Other_Math # L&   [8] MATHEMATICAL FRAKTUR CAPITAL J..MATHEMATICAL FRAKTUR CAPITAL Q
unicode_other_math(0x1D516, 0x1D51C).	% Other_Math # L&   [7] MATHEMATICAL FRAKTUR CAPITAL S..MATHEMATICAL FRAKTUR CAPITAL Y
unicode_other_math(0x1D51E, 0x1D539).	% Other_Math # L&  [28] MATHEMATICAL FRAKTUR SMALL A..MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_other_math(0x1D53B, 0x1D53E).	% Other_Math # L&   [4] MATHEMATICAL DOUBLE-STRUCK CAPITAL D..MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_other_math(0x1D540, 0x1D544).	% Other_Math # L&   [5] MATHEMATICAL DOUBLE-STRUCK CAPITAL I..MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_other_math(0x1D546, 0x1D546).	% Other_Math # L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_other_math(0x1D54A, 0x1D550).	% Other_Math # L&   [7] MATHEMATICAL DOUBLE-STRUCK CAPITAL S..MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_other_math(0x1D552, 0x1D6A5).	% Other_Math # L& [340] MATHEMATICAL DOUBLE-STRUCK SMALL A..MATHEMATICAL ITALIC SMALL DOTLESS J
unicode_other_math(0x1D6A8, 0x1D6C0).	% Other_Math # L&  [25] MATHEMATICAL BOLD CAPITAL ALPHA..MATHEMATICAL BOLD CAPITAL OMEGA
unicode_other_math(0x1D6C2, 0x1D6DA).	% Other_Math # L&  [25] MATHEMATICAL BOLD SMALL ALPHA..MATHEMATICAL BOLD SMALL OMEGA
unicode_other_math(0x1D6DC, 0x1D6FA).	% Other_Math # L&  [31] MATHEMATICAL BOLD EPSILON SYMBOL..MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_other_math(0x1D6FC, 0x1D714).	% Other_Math # L&  [25] MATHEMATICAL ITALIC SMALL ALPHA..MATHEMATICAL ITALIC SMALL OMEGA
unicode_other_math(0x1D716, 0x1D734).	% Other_Math # L&  [31] MATHEMATICAL ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_other_math(0x1D736, 0x1D74E).	% Other_Math # L&  [25] MATHEMATICAL BOLD ITALIC SMALL ALPHA..MATHEMATICAL BOLD ITALIC SMALL OMEGA
unicode_other_math(0x1D750, 0x1D76E).	% Other_Math # L&  [31] MATHEMATICAL BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_other_math(0x1D770, 0x1D788).	% Other_Math # L&  [25] MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
unicode_other_math(0x1D78A, 0x1D7A8).	% Other_Math # L&  [31] MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_other_math(0x1D7AA, 0x1D7C2).	% Other_Math # L&  [25] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
unicode_other_math(0x1D7C4, 0x1D7CB).	% Other_Math # L&   [8] MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD SMALL DIGAMMA
unicode_other_math(0x1D7CE, 0x1D7FF).	% Other_Math # Nd  [50] MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
unicode_other_math(0x1EE00, 0x1EE03).	% Other_Math # Lo   [4] ARABIC MATHEMATICAL ALEF..ARABIC MATHEMATICAL DAL
unicode_other_math(0x1EE05, 0x1EE1F).	% Other_Math # Lo  [27] ARABIC MATHEMATICAL WAW..ARABIC MATHEMATICAL DOTLESS QAF
unicode_other_math(0x1EE21, 0x1EE22).	% Other_Math # Lo   [2] ARABIC MATHEMATICAL INITIAL BEH..ARABIC MATHEMATICAL INITIAL JEEM
unicode_other_math(0x1EE24, 0x1EE24).	% Other_Math # Lo       ARABIC MATHEMATICAL INITIAL HEH
unicode_other_math(0x1EE27, 0x1EE27).	% Other_Math # Lo       ARABIC MATHEMATICAL INITIAL HAH
unicode_other_math(0x1EE29, 0x1EE32).	% Other_Math # Lo  [10] ARABIC MATHEMATICAL INITIAL YEH..ARABIC MATHEMATICAL INITIAL QAF
unicode_other_math(0x1EE34, 0x1EE37).	% Other_Math # Lo   [4] ARABIC MATHEMATICAL INITIAL SHEEN..ARABIC MATHEMATICAL INITIAL KHAH
unicode_other_math(0x1EE39, 0x1EE39).	% Other_Math # Lo       ARABIC MATHEMATICAL INITIAL DAD
unicode_other_math(0x1EE3B, 0x1EE3B).	% Other_Math # Lo       ARABIC MATHEMATICAL INITIAL GHAIN
unicode_other_math(0x1EE42, 0x1EE42).	% Other_Math # Lo       ARABIC MATHEMATICAL TAILED JEEM
unicode_other_math(0x1EE47, 0x1EE47).	% Other_Math # Lo       ARABIC MATHEMATICAL TAILED HAH
unicode_other_math(0x1EE49, 0x1EE49).	% Other_Math # Lo       ARABIC MATHEMATICAL TAILED YEH
unicode_other_math(0x1EE4B, 0x1EE4B).	% Other_Math # Lo       ARABIC MATHEMATICAL TAILED LAM
unicode_other_math(0x1EE4D, 0x1EE4F).	% Other_Math # Lo   [3] ARABIC MATHEMATICAL TAILED NOON..ARABIC MATHEMATICAL TAILED AIN
unicode_other_math(0x1EE51, 0x1EE52).	% Other_Math # Lo   [2] ARABIC MATHEMATICAL TAILED SAD..ARABIC MATHEMATICAL TAILED QAF
unicode_other_math(0x1EE54, 0x1EE54).	% Other_Math # Lo       ARABIC MATHEMATICAL TAILED SHEEN
unicode_other_math(0x1EE57, 0x1EE57).	% Other_Math # Lo       ARABIC MATHEMATICAL TAILED KHAH
unicode_other_math(0x1EE59, 0x1EE59).	% Other_Math # Lo       ARABIC MATHEMATICAL TAILED DAD
unicode_other_math(0x1EE5B, 0x1EE5B).	% Other_Math # Lo       ARABIC MATHEMATICAL TAILED GHAIN
unicode_other_math(0x1EE5D, 0x1EE5D).	% Other_Math # Lo       ARABIC MATHEMATICAL TAILED DOTLESS NOON
unicode_other_math(0x1EE5F, 0x1EE5F).	% Other_Math # Lo       ARABIC MATHEMATICAL TAILED DOTLESS QAF
unicode_other_math(0x1EE61, 0x1EE62).	% Other_Math # Lo   [2] ARABIC MATHEMATICAL STRETCHED BEH..ARABIC MATHEMATICAL STRETCHED JEEM
unicode_other_math(0x1EE64, 0x1EE64).	% Other_Math # Lo       ARABIC MATHEMATICAL STRETCHED HEH
unicode_other_math(0x1EE67, 0x1EE6A).	% Other_Math # Lo   [4] ARABIC MATHEMATICAL STRETCHED HAH..ARABIC MATHEMATICAL STRETCHED KAF
unicode_other_math(0x1EE6C, 0x1EE72).	% Other_Math # Lo   [7] ARABIC MATHEMATICAL STRETCHED MEEM..ARABIC MATHEMATICAL STRETCHED QAF
unicode_other_math(0x1EE74, 0x1EE77).	% Other_Math # Lo   [4] ARABIC MATHEMATICAL STRETCHED SHEEN..ARABIC MATHEMATICAL STRETCHED KHAH
unicode_other_math(0x1EE79, 0x1EE7C).	% Other_Math # Lo   [4] ARABIC MATHEMATICAL STRETCHED DAD..ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
unicode_other_math(0x1EE7E, 0x1EE7E).	% Other_Math # Lo       ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
unicode_other_math(0x1EE80, 0x1EE89).	% Other_Math # Lo  [10] ARABIC MATHEMATICAL LOOPED ALEF..ARABIC MATHEMATICAL LOOPED YEH
unicode_other_math(0x1EE8B, 0x1EE9B).	% Other_Math # Lo  [17] ARABIC MATHEMATICAL LOOPED LAM..ARABIC MATHEMATICAL LOOPED GHAIN
unicode_other_math(0x1EEA1, 0x1EEA3).	% Other_Math # Lo   [3] ARABIC MATHEMATICAL DOUBLE-STRUCK BEH..ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
unicode_other_math(0x1EEA5, 0x1EEA9).	% Other_Math # Lo   [5] ARABIC MATHEMATICAL DOUBLE-STRUCK WAW..ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
unicode_other_math(0x1EEAB, 0x1EEBB).	% Other_Math # Lo  [17] ARABIC MATHEMATICAL DOUBLE-STRUCK LAM..ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN

% Total code points: 1358
