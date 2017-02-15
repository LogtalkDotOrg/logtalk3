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

unicode_ids_binary_operator(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_ids_binary_operator(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_ids_binary_operator(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_ids_binary_operator(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_ids_binary_operator(0x2FF0, 0x2FF1).	% IDS_Binary_Operator # So   [2] IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT..IDEOGRAPHIC DESCRIPTION CHARACTER ABOVE TO BELOW
unicode_ids_binary_operator(0x2FF4, 0x2FFB).	% IDS_Binary_Operator # So   [8] IDEOGRAPHIC DESCRIPTION CHARACTER FULL SURROUND..IDEOGRAPHIC DESCRIPTION CHARACTER OVERLAID

% Total code points: 10
