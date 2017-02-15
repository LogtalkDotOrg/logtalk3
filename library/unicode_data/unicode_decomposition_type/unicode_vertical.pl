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
# DerivedDecompositionType-6.1.0.txt
# Date: 2011-07-25, 00:54:13 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

# ================================================

# Decomposition_Type (from UnicodeData.txt, field 5: see UAX #44: http://www.unicode.org/reports/tr44/)

#  All code points not explicitly listed for Decomposition_Type
#  have the value None.

# @missing: 0000..10FFFF; None
*/

unicode_vertical(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_vertical(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_vertical(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_vertical(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_vertical(0x309F, 0x309F).	% Vertical Lo       HIRAGANA DIGRAPH YORI
unicode_vertical(0x30FF, 0x30FF).	% Vertical Lo       KATAKANA DIGRAPH KOTO
unicode_vertical(0xFE10, 0xFE16).	% Vertical Po   [7] PRESENTATION FORM FOR VERTICAL COMMA..PRESENTATION FORM FOR VERTICAL QUESTION MARK
unicode_vertical(0xFE17, 0xFE17).	% Vertical Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE LENTICULAR BRACKET
unicode_vertical(0xFE18, 0xFE18).	% Vertical Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET
unicode_vertical(0xFE19, 0xFE19).	% Vertical Po       PRESENTATION FORM FOR VERTICAL HORIZONTAL ELLIPSIS
unicode_vertical(0xFE30, 0xFE30).	% Vertical Po       PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
unicode_vertical(0xFE31, 0xFE32).	% Vertical Pd   [2] PRESENTATION FORM FOR VERTICAL EM DASH..PRESENTATION FORM FOR VERTICAL EN DASH
unicode_vertical(0xFE33, 0xFE34).	% Vertical Pc   [2] PRESENTATION FORM FOR VERTICAL LOW LINE..PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
unicode_vertical(0xFE35, 0xFE35).	% Vertical Ps       PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
unicode_vertical(0xFE36, 0xFE36).	% Vertical Pe       PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
unicode_vertical(0xFE37, 0xFE37).	% Vertical Ps       PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
unicode_vertical(0xFE38, 0xFE38).	% Vertical Pe       PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
unicode_vertical(0xFE39, 0xFE39).	% Vertical Ps       PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
unicode_vertical(0xFE3A, 0xFE3A).	% Vertical Pe       PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
unicode_vertical(0xFE3B, 0xFE3B).	% Vertical Ps       PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
unicode_vertical(0xFE3C, 0xFE3C).	% Vertical Pe       PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
unicode_vertical(0xFE3D, 0xFE3D).	% Vertical Ps       PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
unicode_vertical(0xFE3E, 0xFE3E).	% Vertical Pe       PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
unicode_vertical(0xFE3F, 0xFE3F).	% Vertical Ps       PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
unicode_vertical(0xFE40, 0xFE40).	% Vertical Pe       PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
unicode_vertical(0xFE41, 0xFE41).	% Vertical Ps       PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
unicode_vertical(0xFE42, 0xFE42).	% Vertical Pe       PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
unicode_vertical(0xFE43, 0xFE43).	% Vertical Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
unicode_vertical(0xFE44, 0xFE44).	% Vertical Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
unicode_vertical(0xFE47, 0xFE47).	% Vertical Ps       PRESENTATION FORM FOR VERTICAL LEFT SQUARE BRACKET
unicode_vertical(0xFE48, 0xFE48).	% Vertical Pe       PRESENTATION FORM FOR VERTICAL RIGHT SQUARE BRACKET

% Total code points: 35
