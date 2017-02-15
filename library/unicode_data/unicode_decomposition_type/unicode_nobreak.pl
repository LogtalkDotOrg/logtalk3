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

unicode_nobreak(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_nobreak(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_nobreak(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_nobreak(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_nobreak(0x00A0, 0x00A0).	% Nobreak Zs       NO-BREAK SPACE
unicode_nobreak(0x0F0C, 0x0F0C).	% Nobreak Po       TIBETAN MARK DELIMITER TSHEG BSTAR
unicode_nobreak(0x2007, 0x2007).	% Nobreak Zs       FIGURE SPACE
unicode_nobreak(0x2011, 0x2011).	% Nobreak Pd       NON-BREAKING HYPHEN
unicode_nobreak(0x202F, 0x202F).	% Nobreak Zs       NARROW NO-BREAK SPACE

% Total code points: 5
