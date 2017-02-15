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

unicode_other_id_continue(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_other_id_continue(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_other_id_continue(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_other_id_continue(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_other_id_continue(0x00B7, 0x00B7).	% Other_ID_Continue # Po       MIDDLE DOT
unicode_other_id_continue(0x0387, 0x0387).	% Other_ID_Continue # Po       GREEK ANO TELEIA
unicode_other_id_continue(0x1369, 0x1371).	% Other_ID_Continue # No   [9] ETHIOPIC DIGIT ONE..ETHIOPIC DIGIT NINE
unicode_other_id_continue(0x19DA, 0x19DA).	% Other_ID_Continue # No       NEW TAI LUE THAM DIGIT ONE

% Total code points: 12
