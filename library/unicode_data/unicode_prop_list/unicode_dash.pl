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

unicode_dash(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_dash(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_dash(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_dash(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_dash(0x002D, 0x002D).	% Dash # Pd       HYPHEN-MINUS
unicode_dash(0x058A, 0x058A).	% Dash # Pd       ARMENIAN HYPHEN
unicode_dash(0x05BE, 0x05BE).	% Dash # Pd       HEBREW PUNCTUATION MAQAF
unicode_dash(0x1400, 0x1400).	% Dash # Pd       CANADIAN SYLLABICS HYPHEN
unicode_dash(0x1806, 0x1806).	% Dash # Pd       MONGOLIAN TODO SOFT HYPHEN
unicode_dash(0x2010, 0x2015).	% Dash # Pd   [6] HYPHEN..HORIZONTAL BAR
unicode_dash(0x2053, 0x2053).	% Dash # Po       SWUNG DASH
unicode_dash(0x207B, 0x207B).	% Dash # Sm       SUPERSCRIPT MINUS
unicode_dash(0x208B, 0x208B).	% Dash # Sm       SUBSCRIPT MINUS
unicode_dash(0x2212, 0x2212).	% Dash # Sm       MINUS SIGN
unicode_dash(0x2E17, 0x2E17).	% Dash # Pd       DOUBLE OBLIQUE HYPHEN
unicode_dash(0x2E1A, 0x2E1A).	% Dash # Pd       HYPHEN WITH DIAERESIS
unicode_dash(0x2E3A, 0x2E3B).	% Dash # Pd   [2] TWO-EM DASH..THREE-EM DASH
unicode_dash(0x301C, 0x301C).	% Dash # Pd       WAVE DASH
unicode_dash(0x3030, 0x3030).	% Dash # Pd       WAVY DASH
unicode_dash(0x30A0, 0x30A0).	% Dash # Pd       KATAKANA-HIRAGANA DOUBLE HYPHEN
unicode_dash(0xFE31, 0xFE32).	% Dash # Pd   [2] PRESENTATION FORM FOR VERTICAL EM DASH..PRESENTATION FORM FOR VERTICAL EN DASH
unicode_dash(0xFE58, 0xFE58).	% Dash # Pd       SMALL EM DASH
unicode_dash(0xFE63, 0xFE63).	% Dash # Pd       SMALL HYPHEN-MINUS
unicode_dash(0xFF0D, 0xFF0D).	% Dash # Pd       FULLWIDTH HYPHEN-MINUS

% Total code points: 27
