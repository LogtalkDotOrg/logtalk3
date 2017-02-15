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

unicode_white_space(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_white_space(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_white_space(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_white_space(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_white_space(0x0009, 0x000D).	% White_Space # Cc   [5] <control-0009>..<control-000D>
unicode_white_space(0x0020, 0x0020).	% White_Space # Zs       SPACE
unicode_white_space(0x0085, 0x0085).	% White_Space # Cc       <control-0085>
unicode_white_space(0x00A0, 0x00A0).	% White_Space # Zs       NO-BREAK SPACE
unicode_white_space(0x1680, 0x1680).	% White_Space # Zs       OGHAM SPACE MARK
unicode_white_space(0x180E, 0x180E).	% White_Space # Zs       MONGOLIAN VOWEL SEPARATOR
unicode_white_space(0x2000, 0x200A).	% White_Space # Zs  [11] EN QUAD..HAIR SPACE
unicode_white_space(0x2028, 0x2028).	% White_Space # Zl       LINE SEPARATOR
unicode_white_space(0x2029, 0x2029).	% White_Space # Zp       PARAGRAPH SEPARATOR
unicode_white_space(0x202F, 0x202F).	% White_Space # Zs       NARROW NO-BREAK SPACE
unicode_white_space(0x205F, 0x205F).	% White_Space # Zs       MEDIUM MATHEMATICAL SPACE
unicode_white_space(0x3000, 0x3000).	% White_Space # Zs       IDEOGRAPHIC SPACE

% Total code points: 26
