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

unicode_small(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_small(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_small(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_small(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_small(0xFE50, 0xFE52).	% Small Po   [3] SMALL COMMA..SMALL FULL STOP
unicode_small(0xFE54, 0xFE57).	% Small Po   [4] SMALL SEMICOLON..SMALL EXCLAMATION MARK
unicode_small(0xFE58, 0xFE58).	% Small Pd       SMALL EM DASH
unicode_small(0xFE59, 0xFE59).	% Small Ps       SMALL LEFT PARENTHESIS
unicode_small(0xFE5A, 0xFE5A).	% Small Pe       SMALL RIGHT PARENTHESIS
unicode_small(0xFE5B, 0xFE5B).	% Small Ps       SMALL LEFT CURLY BRACKET
unicode_small(0xFE5C, 0xFE5C).	% Small Pe       SMALL RIGHT CURLY BRACKET
unicode_small(0xFE5D, 0xFE5D).	% Small Ps       SMALL LEFT TORTOISE SHELL BRACKET
unicode_small(0xFE5E, 0xFE5E).	% Small Pe       SMALL RIGHT TORTOISE SHELL BRACKET
unicode_small(0xFE5F, 0xFE61).	% Small Po   [3] SMALL NUMBER SIGN..SMALL ASTERISK
unicode_small(0xFE62, 0xFE62).	% Small Sm       SMALL PLUS SIGN
unicode_small(0xFE63, 0xFE63).	% Small Pd       SMALL HYPHEN-MINUS
unicode_small(0xFE64, 0xFE66).	% Small Sm   [3] SMALL LESS-THAN SIGN..SMALL EQUALS SIGN
unicode_small(0xFE68, 0xFE68).	% Small Po       SMALL REVERSE SOLIDUS
unicode_small(0xFE69, 0xFE69).	% Small Sc       SMALL DOLLAR SIGN
unicode_small(0xFE6A, 0xFE6B).	% Small Po   [2] SMALL PERCENT SIGN..SMALL COMMERCIAL AT

% Total code points: 26
