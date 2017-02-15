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

unicode_initial(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_initial(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_initial(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_initial(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_initial(0xFB54, 0xFB54).	% Initial Lo       ARABIC LETTER BEEH INITIAL FORM
unicode_initial(0xFB58, 0xFB58).	% Initial Lo       ARABIC LETTER PEH INITIAL FORM
unicode_initial(0xFB5C, 0xFB5C).	% Initial Lo       ARABIC LETTER BEHEH INITIAL FORM
unicode_initial(0xFB60, 0xFB60).	% Initial Lo       ARABIC LETTER TTEHEH INITIAL FORM
unicode_initial(0xFB64, 0xFB64).	% Initial Lo       ARABIC LETTER TEHEH INITIAL FORM
unicode_initial(0xFB68, 0xFB68).	% Initial Lo       ARABIC LETTER TTEH INITIAL FORM
unicode_initial(0xFB6C, 0xFB6C).	% Initial Lo       ARABIC LETTER VEH INITIAL FORM
unicode_initial(0xFB70, 0xFB70).	% Initial Lo       ARABIC LETTER PEHEH INITIAL FORM
unicode_initial(0xFB74, 0xFB74).	% Initial Lo       ARABIC LETTER DYEH INITIAL FORM
unicode_initial(0xFB78, 0xFB78).	% Initial Lo       ARABIC LETTER NYEH INITIAL FORM
unicode_initial(0xFB7C, 0xFB7C).	% Initial Lo       ARABIC LETTER TCHEH INITIAL FORM
unicode_initial(0xFB80, 0xFB80).	% Initial Lo       ARABIC LETTER TCHEHEH INITIAL FORM
unicode_initial(0xFB90, 0xFB90).	% Initial Lo       ARABIC LETTER KEHEH INITIAL FORM
unicode_initial(0xFB94, 0xFB94).	% Initial Lo       ARABIC LETTER GAF INITIAL FORM
unicode_initial(0xFB98, 0xFB98).	% Initial Lo       ARABIC LETTER GUEH INITIAL FORM
unicode_initial(0xFB9C, 0xFB9C).	% Initial Lo       ARABIC LETTER NGOEH INITIAL FORM
unicode_initial(0xFBA2, 0xFBA2).	% Initial Lo       ARABIC LETTER RNOON INITIAL FORM
unicode_initial(0xFBA8, 0xFBA8).	% Initial Lo       ARABIC LETTER HEH GOAL INITIAL FORM
unicode_initial(0xFBAC, 0xFBAC).	% Initial Lo       ARABIC LETTER HEH DOACHASHMEE INITIAL FORM
unicode_initial(0xFBD5, 0xFBD5).	% Initial Lo       ARABIC LETTER NG INITIAL FORM
unicode_initial(0xFBE6, 0xFBE6).	% Initial Lo       ARABIC LETTER E INITIAL FORM
unicode_initial(0xFBE8, 0xFBE8).	% Initial Lo       ARABIC LETTER UIGHUR KAZAKH KIRGHIZ ALEF MAKSURA INITIAL FORM
unicode_initial(0xFBF8, 0xFBF8).	% Initial Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E INITIAL FORM
unicode_initial(0xFBFB, 0xFBFB).	% Initial Lo       ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA INITIAL FORM
unicode_initial(0xFBFE, 0xFBFE).	% Initial Lo       ARABIC LETTER FARSI YEH INITIAL FORM
unicode_initial(0xFC97, 0xFCDE).	% Initial Lo  [72] ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH JEEM INITIAL FORM..ARABIC LIGATURE YEH WITH HEH INITIAL FORM
unicode_initial(0xFD2D, 0xFD33).	% Initial Lo   [7] ARABIC LIGATURE SHEEN WITH JEEM INITIAL FORM..ARABIC LIGATURE TAH WITH MEEM INITIAL FORM
unicode_initial(0xFD50, 0xFD50).	% Initial Lo       ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM
unicode_initial(0xFD52, 0xFD57).	% Initial Lo   [6] ARABIC LIGATURE TEH WITH HAH WITH JEEM INITIAL FORM..ARABIC LIGATURE TEH WITH MEEM WITH KHAH INITIAL FORM
unicode_initial(0xFD59, 0xFD59 ).	% Initial Lo       ARABIC LIGATURE JEEM WITH MEEM WITH HAH INITIAL FORM
unicode_initial(0xFD5C, 0xFD5D).	% Initial Lo   [2] ARABIC LIGATURE SEEN WITH HAH WITH JEEM INITIAL FORM..ARABIC LIGATURE SEEN WITH JEEM WITH HAH INITIAL FORM
unicode_initial(0xFD60, 0xFD61).	% Initial Lo   [2] ARABIC LIGATURE SEEN WITH MEEM WITH HAH INITIAL FORM..ARABIC LIGATURE SEEN WITH MEEM WITH JEEM INITIAL FORM
unicode_initial(0xFD63, 0xFD63).	% Initial Lo       ARABIC LIGATURE SEEN WITH MEEM WITH MEEM INITIAL FORM
unicode_initial(0xFD65, 0xFD65).	% Initial Lo       ARABIC LIGATURE SAD WITH HAH WITH HAH INITIAL FORM
unicode_initial(0xFD68, 0xFD68).	% Initial Lo       ARABIC LIGATURE SHEEN WITH HAH WITH MEEM INITIAL FORM
unicode_initial(0xFD6B, 0xFD6B).	% Initial Lo       ARABIC LIGATURE SHEEN WITH MEEM WITH KHAH INITIAL FORM
unicode_initial(0xFD6D, 0xFD6D).	% Initial Lo       ARABIC LIGATURE SHEEN WITH MEEM WITH MEEM INITIAL FORM
unicode_initial(0xFD70, 0xFD70).	% Initial Lo       ARABIC LIGATURE DAD WITH KHAH WITH MEEM INITIAL FORM
unicode_initial(0xFD72, 0xFD73).	% Initial Lo   [2] ARABIC LIGATURE TAH WITH MEEM WITH HAH INITIAL FORM..ARABIC LIGATURE TAH WITH MEEM WITH MEEM INITIAL FORM
unicode_initial(0xFD77, 0xFD77).	% Initial Lo       ARABIC LIGATURE AIN WITH MEEM WITH MEEM INITIAL FORM
unicode_initial(0xFD7D, 0xFD7D).	% Initial Lo       ARABIC LIGATURE FEH WITH KHAH WITH MEEM INITIAL FORM
unicode_initial(0xFD83, 0xFD83).	% Initial Lo       ARABIC LIGATURE LAM WITH JEEM WITH JEEM INITIAL FORM
unicode_initial(0xFD86, 0xFD86).	% Initial Lo       ARABIC LIGATURE LAM WITH KHAH WITH MEEM INITIAL FORM
unicode_initial(0xFD88, 0xFD8A).	% Initial Lo   [3] ARABIC LIGATURE LAM WITH MEEM WITH HAH INITIAL FORM..ARABIC LIGATURE MEEM WITH HAH WITH MEEM INITIAL FORM
unicode_initial(0xFD8C, 0xFD8F).	% Initial Lo   [4] ARABIC LIGATURE MEEM WITH JEEM WITH HAH INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
unicode_initial(0xFD92, 0xFD95).	% Initial Lo   [4] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH HAH WITH MEEM INITIAL FORM
unicode_initial(0xFD98, 0xFD98).	% Initial Lo       ARABIC LIGATURE NOON WITH JEEM WITH MEEM INITIAL FORM
unicode_initial(0xFD9D, 0xFD9D).	% Initial Lo       ARABIC LIGATURE YEH WITH MEEM WITH MEEM INITIAL FORM
unicode_initial(0xFDB4, 0xFDB5).	% Initial Lo   [2] ARABIC LIGATURE QAF WITH MEEM WITH HAH INITIAL FORM..ARABIC LIGATURE LAM WITH HAH WITH MEEM INITIAL FORM
unicode_initial(0xFDB8, 0xFDB8).	% Initial Lo       ARABIC LIGATURE NOON WITH JEEM WITH HAH INITIAL FORM
unicode_initial(0xFDBA, 0xFDBA).	% Initial Lo       ARABIC LIGATURE LAM WITH JEEM WITH MEEM INITIAL FORM
unicode_initial(0xFDC3, 0xFDC5).	% Initial Lo   [3] ARABIC LIGATURE KAF WITH MEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE SAD WITH MEEM WITH MEEM INITIAL FORM
unicode_initial(0xFE8B, 0xFE8B).	% Initial Lo       ARABIC LETTER YEH WITH HAMZA ABOVE INITIAL FORM
unicode_initial(0xFE91, 0xFE91).	% Initial Lo       ARABIC LETTER BEH INITIAL FORM
unicode_initial(0xFE97, 0xFE97).	% Initial Lo       ARABIC LETTER TEH INITIAL FORM
unicode_initial(0xFE9B, 0xFE9B).	% Initial Lo       ARABIC LETTER THEH INITIAL FORM
unicode_initial(0xFE9F, 0xFE9F).	% Initial Lo       ARABIC LETTER JEEM INITIAL FORM
unicode_initial(0xFEA3, 0xFEA3).	% Initial Lo       ARABIC LETTER HAH INITIAL FORM
unicode_initial(0xFEA7, 0xFEA7).	% Initial Lo       ARABIC LETTER KHAH INITIAL FORM
unicode_initial(0xFEB3, 0xFEB3).	% Initial Lo       ARABIC LETTER SEEN INITIAL FORM
unicode_initial(0xFEB7, 0xFEB7).	% Initial Lo       ARABIC LETTER SHEEN INITIAL FORM
unicode_initial(0xFEBB, 0xFEBB).	% Initial Lo       ARABIC LETTER SAD INITIAL FORM
unicode_initial(0xFEBF, 0xFEBF).	% Initial Lo       ARABIC LETTER DAD INITIAL FORM
unicode_initial(0xFEC3, 0xFEC3).	% Initial Lo       ARABIC LETTER TAH INITIAL FORM
unicode_initial(0xFEC7, 0xFEC7).	% Initial Lo       ARABIC LETTER ZAH INITIAL FORM
unicode_initial(0xFECB, 0xFECB).	% Initial Lo       ARABIC LETTER AIN INITIAL FORM
unicode_initial(0xFECF, 0xFECF).	% Initial Lo       ARABIC LETTER GHAIN INITIAL FORM
unicode_initial(0xFED3, 0xFED3).	% Initial Lo       ARABIC LETTER FEH INITIAL FORM
unicode_initial(0xFED7, 0xFED7).	% Initial Lo       ARABIC LETTER QAF INITIAL FORM
unicode_initial(0xFEDB, 0xFEDB).	% Initial Lo       ARABIC LETTER KAF INITIAL FORM
unicode_initial(0xFEDF, 0xFEDF).	% Initial Lo       ARABIC LETTER LAM INITIAL FORM
unicode_initial(0xFEE3, 0xFEE3).	% Initial Lo       ARABIC LETTER MEEM INITIAL FORM
unicode_initial(0xFEE7, 0xFEE7).	% Initial Lo       ARABIC LETTER NOON INITIAL FORM
unicode_initial(0xFEEB, 0xFEEB).	% Initial Lo       ARABIC LETTER HEH INITIAL FORM
unicode_initial(0xFEF3, 0xFEF3).	% Initial Lo       ARABIC LETTER YEH INITIAL FORM

% Total code points: 171
