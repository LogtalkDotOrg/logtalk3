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

unicode_hyphen(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_hyphen(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_hyphen(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_hyphen(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_hyphen(0x002D, 0x002D).	% Hyphen # Pd       HYPHEN-MINUS
unicode_hyphen(0x00AD, 0x00AD).	% Hyphen # Cf       SOFT HYPHEN
unicode_hyphen(0x058A, 0x058A).	% Hyphen # Pd       ARMENIAN HYPHEN
unicode_hyphen(0x1806, 0x1806).	% Hyphen # Pd       MONGOLIAN TODO SOFT HYPHEN
unicode_hyphen(0x2010, 0x2011).	% Hyphen # Pd   [2] HYPHEN..NON-BREAKING HYPHEN
unicode_hyphen(0x2E17, 0x2E17).	% Hyphen # Pd       DOUBLE OBLIQUE HYPHEN
unicode_hyphen(0x30FB, 0x30FB).	% Hyphen # Po       KATAKANA MIDDLE DOT
unicode_hyphen(0xFE63, 0xFE63).	% Hyphen # Pd       SMALL HYPHEN-MINUS
unicode_hyphen(0xFF0D, 0xFF0D).	% Hyphen # Pd       FULLWIDTH HYPHEN-MINUS
unicode_hyphen(0xFF65, 0xFF65).	% Hyphen # Po       HALFWIDTH KATAKANA MIDDLE DOT

% Total code points: 11
