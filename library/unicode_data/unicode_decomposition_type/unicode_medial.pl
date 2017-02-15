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

unicode_medial(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_medial(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_medial(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_medial(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_medial(0xFB55, 0xFB55).	% Medial Lo       ARABIC LETTER BEEH MEDIAL FORM
unicode_medial(0xFB59, 0xFB59).	% Medial Lo       ARABIC LETTER PEH MEDIAL FORM
unicode_medial(0xFB5D, 0xFB5D).	% Medial Lo       ARABIC LETTER BEHEH MEDIAL FORM
unicode_medial(0xFB61, 0xFB61).	% Medial Lo       ARABIC LETTER TTEHEH MEDIAL FORM
unicode_medial(0xFB65, 0xFB65).	% Medial Lo       ARABIC LETTER TEHEH MEDIAL FORM
unicode_medial(0xFB69, 0xFB69).	% Medial Lo       ARABIC LETTER TTEH MEDIAL FORM
unicode_medial(0xFB6D, 0xFB6D).	% Medial Lo       ARABIC LETTER VEH MEDIAL FORM
unicode_medial(0xFB71, 0xFB71).	% Medial Lo       ARABIC LETTER PEHEH MEDIAL FORM
unicode_medial(0xFB75, 0xFB75).	% Medial Lo       ARABIC LETTER DYEH MEDIAL FORM
unicode_medial(0xFB79, 0xFB79).	% Medial Lo       ARABIC LETTER NYEH MEDIAL FORM
unicode_medial(0xFB7D, 0xFB7D).	% Medial Lo       ARABIC LETTER TCHEH MEDIAL FORM
unicode_medial(0xFB81, 0xFB81).	% Medial Lo       ARABIC LETTER TCHEHEH MEDIAL FORM
unicode_medial(0xFB91, 0xFB91).	% Medial Lo       ARABIC LETTER KEHEH MEDIAL FORM
unicode_medial(0xFB95, 0xFB95).	% Medial Lo       ARABIC LETTER GAF MEDIAL FORM
unicode_medial(0xFB99, 0xFB99).	% Medial Lo       ARABIC LETTER GUEH MEDIAL FORM
unicode_medial(0xFB9D, 0xFB9D).	% Medial Lo       ARABIC LETTER NGOEH MEDIAL FORM
unicode_medial(0xFBA3, 0xFBA3).	% Medial Lo       ARABIC LETTER RNOON MEDIAL FORM
unicode_medial(0xFBA9, 0xFBA9).	% Medial Lo       ARABIC LETTER HEH GOAL MEDIAL FORM
unicode_medial(0xFBAD, 0xFBAD).	% Medial Lo       ARABIC LETTER HEH DOACHASHMEE MEDIAL FORM
unicode_medial(0xFBD6, 0xFBD6).	% Medial Lo       ARABIC LETTER NG MEDIAL FORM
unicode_medial(0xFBE7, 0xFBE7).	% Medial Lo       ARABIC LETTER E MEDIAL FORM
unicode_medial(0xFBE9, 0xFBE9).	% Medial Lo       ARABIC LETTER UIGHUR KAZAKH KIRGHIZ ALEF MAKSURA MEDIAL FORM
unicode_medial(0xFBFF, 0xFBFF).	% Medial Lo       ARABIC LETTER FARSI YEH MEDIAL FORM
unicode_medial(0xFCDF, 0xFCF4).	% Medial Lo  [22] ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM MEDIAL FORM..ARABIC LIGATURE SHADDA WITH KASRA MEDIAL FORM
unicode_medial(0xFD34, 0xFD3B).	% Medial Lo   [8] ARABIC LIGATURE SEEN WITH JEEM MEDIAL FORM..ARABIC LIGATURE ZAH WITH MEEM MEDIAL FORM
unicode_medial(0xFE71, 0xFE71).	% Medial Lo       ARABIC TATWEEL WITH FATHATAN ABOVE
unicode_medial(0xFE77, 0xFE77).	% Medial Lo       ARABIC FATHA MEDIAL FORM
unicode_medial(0xFE79, 0xFE79).	% Medial Lo       ARABIC DAMMA MEDIAL FORM
unicode_medial(0xFE7B, 0xFE7B).	% Medial Lo       ARABIC KASRA MEDIAL FORM
unicode_medial(0xFE7D, 0xFE7D).	% Medial Lo       ARABIC SHADDA MEDIAL FORM
unicode_medial(0xFE7F, 0xFE7F).	% Medial Lo       ARABIC SUKUN MEDIAL FORM
unicode_medial(0xFE8C, 0xFE8C).	% Medial Lo       ARABIC LETTER YEH WITH HAMZA ABOVE MEDIAL FORM
unicode_medial(0xFE92, 0xFE92).	% Medial Lo       ARABIC LETTER BEH MEDIAL FORM
unicode_medial(0xFE98, 0xFE98).	% Medial Lo       ARABIC LETTER TEH MEDIAL FORM
unicode_medial(0xFE9C, 0xFE9C).	% Medial Lo       ARABIC LETTER THEH MEDIAL FORM
unicode_medial(0xFEA0, 0xFEA0).	% Medial Lo       ARABIC LETTER JEEM MEDIAL FORM
unicode_medial(0xFEA4, 0xFEA4).	% Medial Lo       ARABIC LETTER HAH MEDIAL FORM
unicode_medial(0xFEA8, 0xFEA8).	% Medial Lo       ARABIC LETTER KHAH MEDIAL FORM
unicode_medial(0xFEB4, 0xFEB4).	% Medial Lo       ARABIC LETTER SEEN MEDIAL FORM
unicode_medial(0xFEB8, 0xFEB8).	% Medial Lo       ARABIC LETTER SHEEN MEDIAL FORM
unicode_medial(0xFEBC, 0xFEBC).	% Medial Lo       ARABIC LETTER SAD MEDIAL FORM
unicode_medial(0xFEC0, 0xFEC0).	% Medial Lo       ARABIC LETTER DAD MEDIAL FORM
unicode_medial(0xFEC4, 0xFEC4).	% Medial Lo       ARABIC LETTER TAH MEDIAL FORM
unicode_medial(0xFEC8, 0xFEC8).	% Medial Lo       ARABIC LETTER ZAH MEDIAL FORM
unicode_medial(0xFECC, 0xFECC).	% Medial Lo       ARABIC LETTER AIN MEDIAL FORM
unicode_medial(0xFED0, 0xFED0).	% Medial Lo       ARABIC LETTER GHAIN MEDIAL FORM
unicode_medial(0xFED4, 0xFED4).	% Medial Lo       ARABIC LETTER FEH MEDIAL FORM
unicode_medial(0xFED8, 0xFED8).	% Medial Lo       ARABIC LETTER QAF MEDIAL FORM
unicode_medial(0xFEDC, 0xFEDC).	% Medial Lo       ARABIC LETTER KAF MEDIAL FORM
unicode_medial(0xFEE0, 0xFEE0).	% Medial Lo       ARABIC LETTER LAM MEDIAL FORM
unicode_medial(0xFEE4, 0xFEE4).	% Medial Lo       ARABIC LETTER MEEM MEDIAL FORM
unicode_medial(0xFEE8, 0xFEE8).	% Medial Lo       ARABIC LETTER NOON MEDIAL FORM
unicode_medial(0xFEEC, 0xFEEC).	% Medial Lo       ARABIC LETTER HEH MEDIAL FORM
unicode_medial(0xFEF4, 0xFEF4).	% Medial Lo       ARABIC LETTER YEH MEDIAL FORM

% Total code points: 82
