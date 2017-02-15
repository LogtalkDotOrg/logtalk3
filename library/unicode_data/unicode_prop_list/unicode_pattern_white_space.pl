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

unicode_pattern_white_space(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_pattern_white_space(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_pattern_white_space(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_pattern_white_space(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_pattern_white_space(0x0009, 0x000D).	% Pattern_White_Space # Cc   [5] <control-0009>..<control-000D>
unicode_pattern_white_space(0x0020, 0x0020).	% Pattern_White_Space # Zs       SPACE
unicode_pattern_white_space(0x0085, 0x0085).	% Pattern_White_Space # Cc       <control-0085>
unicode_pattern_white_space(0x200E, 0x200F).	% Pattern_White_Space # Cf   [2] LEFT-TO-RIGHT MARK..RIGHT-TO-LEFT MARK
unicode_pattern_white_space(0x2028, 0x2028).	% Pattern_White_Space # Zl       LINE SEPARATOR
unicode_pattern_white_space(0x2029, 0x2029).	% Pattern_White_Space # Zp       PARAGRAPH SEPARATOR

% Total code points: 11
