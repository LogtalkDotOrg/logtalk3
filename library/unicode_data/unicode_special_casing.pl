%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: September 30, 2012
%
%  Original Unicode file header comments follow

/*
# SpecialCasing-6.1.0.txt
# Date: 2011-11-27, 05:10:51 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
#
# Special Casing Properties
#
# This file is a supplement to the UnicodeData file.
# It contains additional information about the casing of Unicode characters.
# (For compatibility, the UnicodeData.txt file only contains case mappings for
# characters where they are 1-1, and independent of context and language.
# For more information, see the discussion of Case Mappings in the Unicode Standard.
#
# All code points not listed in this file that do not have a simple case mappings
# in UnicodeData.txt map to themselves.
# ================================================================================
# Format
# ================================================================================
# The entries in this file are in the following machine-readable format:
#
# <code>; <lower> ; <title> ; <upper> ; (<condition_list> ;)? # <comment>
#
# <code>, <lower>, <title>, and <upper> provide character values in hex. If there is more
# than one character, they are separated by spaces. Other than as used to separate 
# elements, spaces are to be ignored.
#
# The <condition_list> is optional. Where present, it consists of one or more language IDs
# or contexts, separated by spaces. In these conditions:
# - A condition list overrides the normal behavior if all of the listed conditions are true.
# - The context is always the context of the characters in the original string,
#   NOT in the resulting string.
# - Case distinctions in the condition list are not significant.
# - Conditions preceded by "Not_" represent the negation of the condition.
# The condition list is not represented in the UCD as a formal property.
#
# A language ID is defined by BCP 47, with '-' and '_' treated equivalently.
#
# A context for a character C is defined by Section 3.13 Default Case 
# Operations, of The Unicode Standard, Version 5.0.
# (This is identical to the context defined by Unicode 4.1.0,
#  as specified in http://www.unicode.org/versions/Unicode4.1.0/)
#
# Parsers of this file must be prepared to deal with future additions to this format:
#  * Additional contexts
#  * Additional fields
# ================================================================================

# @missing: 0000..10FFFF; <slc>; <stc>; <suc>;
*/

% ================================================================================
% Unconditional mappings
% ================================================================================

% The German es-zed is special--the normal mapping is to SS.
% Note: the titlecase should never occur in practice. It is equal to titlecase(uppercase(<es-zed>))

unicode_special_casing(0x00DF, [0x00DF], [0x0053, 0x0073], [0x0053, 0x0053], []). % LATIN SMALL LETTER SHARP S

% Preserve canonical equivalence for I with dot. Turkic is handled below.

unicode_special_casing(0x0130, [0x0069, 0x0307], [0x0130], [0x0130], []). % LATIN CAPITAL LETTER I WITH DOT ABOVE

% Ligatures

unicode_special_casing(0xFB00, [0xFB00], [0x0046, 0x0066], [0x0046, 0x0046], []). % LATIN SMALL LIGATURE FF
unicode_special_casing(0xFB01, [0xFB01], [0x0046, 0x0069], [0x0046, 0x0049], []). % LATIN SMALL LIGATURE FI
unicode_special_casing(0xFB02, [0xFB02], [0x0046, 0x006C], [0x0046, 0x004C], []). % LATIN SMALL LIGATURE FL
unicode_special_casing(0xFB03, [0xFB03], [0x0046, 0x0066, 0x0069], [0x0046, 0x0046, 0x0049], []). % LATIN SMALL LIGATURE FFI
unicode_special_casing(0xFB04, [0xFB04], [0x0046, 0x0066, 0x006C], [0x0046, 0x0046, 0x004C], []). % LATIN SMALL LIGATURE FFL
unicode_special_casing(0xFB05, [0xFB05], [0x0053, 0x0074], [0x0053, 0x0054], []). % LATIN SMALL LIGATURE LONG S T
unicode_special_casing(0xFB06, [0xFB06], [0x0053, 0x0074], [0x0053, 0x0054], []). % LATIN SMALL LIGATURE ST

unicode_special_casing(0x0587, [0x0587], [0x0535, 0x0582], [0x0535, 0x0552], []). % ARMENIAN SMALL LIGATURE ECH YIWN
unicode_special_casing(0xFB13, [0xFB13], [0x0544, 0x0576], [0x0544, 0x0546], []). % ARMENIAN SMALL LIGATURE MEN NOW
unicode_special_casing(0xFB14, [0xFB14], [0x0544, 0x0565], [0x0544, 0x0535], []). % ARMENIAN SMALL LIGATURE MEN ECH
unicode_special_casing(0xFB15, [0xFB15], [0x0544, 0x056B], [0x0544, 0x053B], []). % ARMENIAN SMALL LIGATURE MEN INI
unicode_special_casing(0xFB16, [0xFB16], [0x054E, 0x0576], [0x054E, 0x0546], []). % ARMENIAN SMALL LIGATURE VEW NOW
unicode_special_casing(0xFB17, [0xFB17], [0x0544, 0x056D], [0x0544, 0x053D], []). % ARMENIAN SMALL LIGATURE MEN XEH

% No corresponding uppercase precomposed character

unicode_special_casing(0x0149, [0x0149], [0x02BC, 0x004E], [0x02BC, 0x004E], []). % LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
unicode_special_casing(0x0390, [0x0390], [0x0399, 0x0308, 0x0301], [0x0399, 0x0308, 0x0301], []). % GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
unicode_special_casing(0x03B0, [0x03B0], [0x03A5, 0x0308, 0x0301], [0x03A5, 0x0308, 0x0301], []). % GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
unicode_special_casing(0x01F0, [0x01F0], [0x004A, 0x030C], [0x004A, 0x030C], []). % LATIN SMALL LETTER J WITH CARON
unicode_special_casing(0x1E96, [0x1E96], [0x0048, 0x0331], [0x0048, 0x0331], []). % LATIN SMALL LETTER H WITH LINE BELOW
unicode_special_casing(0x1E97, [0x1E97], [0x0054, 0x0308], [0x0054, 0x0308], []). % LATIN SMALL LETTER T WITH DIAERESIS
unicode_special_casing(0x1E98, [0x1E98], [0x0057, 0x030A], [0x0057, 0x030A], []). % LATIN SMALL LETTER W WITH RING ABOVE
unicode_special_casing(0x1E99, [0x1E99], [0x0059, 0x030A], [0x0059, 0x030A], []). % LATIN SMALL LETTER Y WITH RING ABOVE
unicode_special_casing(0x1E9A, [0x1E9A], [0x0041, 0x02BE], [0x0041, 0x02BE], []). % LATIN SMALL LETTER A WITH RIGHT HALF RING
unicode_special_casing(0x1F50, [0x1F50], [0x03A5, 0x0313], [0x03A5, 0x0313], []). % GREEK SMALL LETTER UPSILON WITH PSILI
unicode_special_casing(0x1F52, [0x1F52], [0x03A5, 0x0313, 0x0300], [0x03A5, 0x0313, 0x0300], []). % GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
unicode_special_casing(0x1F54, [0x1F54], [0x03A5, 0x0313, 0x0301], [0x03A5, 0x0313, 0x0301], []). % GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
unicode_special_casing(0x1F56, [0x1F56], [0x03A5, 0x0313, 0x0342], [0x03A5, 0x0313, 0x0342], []). % GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
unicode_special_casing(0x1FB6, [0x1FB6], [0x0391, 0x0342], [0x0391, 0x0342], []). % GREEK SMALL LETTER ALPHA WITH PERISPOMENI
unicode_special_casing(0x1FC6, [0x1FC6], [0x0397, 0x0342], [0x0397, 0x0342], []). % GREEK SMALL LETTER ETA WITH PERISPOMENI
unicode_special_casing(0x1FD2, [0x1FD2], [0x0399, 0x0308, 0x0300], [0x0399, 0x0308, 0x0300], []). % GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
unicode_special_casing(0x1FD3, [0x1FD3], [0x0399, 0x0308, 0x0301], [0x0399, 0x0308, 0x0301], []). % GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_special_casing(0x1FD6, [0x1FD6], [0x0399, 0x0342], [0399, 0x0342], []). % GREEK SMALL LETTER IOTA WITH PERISPOMENI
unicode_special_casing(0x1FD7, [0x1FD7], [0x0399, 0x0308, 0x0342], [0x0399, 0x0308, 0x0342], []). % GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
unicode_special_casing(0x1FE2, [0x1FE2], [0x03A5, 0x0308, 0x0300], [0x03A5, 0x0308, 0x0300], []). % GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
unicode_special_casing(0x1FE3, [0x1FE3], [0x03A5, 0x0308, 0x0301], [0x03A5, 0x0308, 0x0301], []). % GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
unicode_special_casing(0x1FE4, [0x1FE4], [0x03A1, 0x0313], [0x03A1, 0x0313], []). % GREEK SMALL LETTER RHO WITH PSILI
unicode_special_casing(0x1FE6, [0x1FE6], [0x03A5, 0x0342], [0x03A5, 0x0342], []). % GREEK SMALL LETTER UPSILON WITH PERISPOMENI
unicode_special_casing(0x1FE7, [0x1FE7], [0x03A5, 0x0308, 0x0342], [0x03A5, 0x0308, 0x0342], []). % GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
unicode_special_casing(0x1FF6, [0x1FF6], [0x03A9, 0x0342], [0x03A9, 0x0342], []). % GREEK SMALL LETTER OMEGA WITH PERISPOMENI

% IMPORTANT-when iota-subscript (0345) is uppercased or titlecased,
%  the result will be incorrect unless the iota-subscript is moved to the end
%  of any sequence of combining marks. Otherwise, the accents will go on the capital iota.
%  This process can be achieved by first transforming the text to NFC before casing.
%  E.g. <alpha><iota_subscript><acute> is uppercased to <ALPHA><acute><IOTA>

% The following cases are already in the UnicodeData file, so are only commented here.

% 0345; 0345; 0345; 0399; # COMBINING GREEK YPOGEGRAMMENI

% All letters with YPOGEGRAMMENI (iota-subscript) or PROSGEGRAMMENI (iota adscript)
% have special uppercases.
% Note: characters with PROSGEGRAMMENI are actually titlecase, not uppercase!

unicode_special_casing(0x1F80, [0x1F80], [0x1F88], [0x1F08, 0x0399], []). % GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
unicode_special_casing(0x1F81, [0x1F81], [0x1F89], [0x1F09, 0x0399], []). % GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
unicode_special_casing(0x1F82, [0x1F82], [0x1F8A], [0x1F0A, 0x0399], []). % GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
unicode_special_casing(0x1F83, [0x1F83], [0x1F8B], [0x1F0B, 0x0399], []). % GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
unicode_special_casing(0x1F84, [0x1F84], [0x1F8C], [0x1F0C, 0x0399], []). % GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
unicode_special_casing(0x1F85, [0x1F85], [0x1F8D], [0x1F0D, 0x0399], []). % GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
unicode_special_casing(0x1F86, [0x1F86], [0x1F8E], [0x1F0E, 0x0399], []). % GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
unicode_special_casing(0x1F87, [0x1F87], [0x1F8F], [0x1F0F, 0x0399], []). % GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_special_casing(0x1F88, [0x1F80], [0x1F88], [0x1F08, 0x0399], []). % GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
unicode_special_casing(0x1F89, [0x1F81], [0x1F89], [0x1F09, 0x0399], []). % GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
unicode_special_casing(0x1F8A, [0x1F82], [0x1F8A], [0x1F0A, 0x0399], []). % GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
unicode_special_casing(0x1F8B, [0x1F83], [0x1F8B], [0x1F0B, 0x0399], []). % GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
unicode_special_casing(0x1F8C, [0x1F84], [0x1F8C], [0x1F0C, 0x0399], []). % GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
unicode_special_casing(0x1F8D, [0x1F85], [0x1F8D], [0x1F0D, 0x0399], []). % GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
unicode_special_casing(0x1F8E, [0x1F86], [0x1F8E], [0x1F0E, 0x0399], []). % GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
unicode_special_casing(0x1F8F, [0x1F87], [0x1F8F], [0x1F0F, 0x0399], []). % GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_special_casing(0x1F90, [0x1F90], [0x1F98], [0x1F28, 0x0399], []). % GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
unicode_special_casing(0x1F91, [0x1F91], [0x1F99], [0x1F29, 0x0399], []). % GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
unicode_special_casing(0x1F92, [0x1F92], [0x1F9A], [0x1F2A, 0x0399], []). % GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
unicode_special_casing(0x1F93, [0x1F93], [0x1F9B], [0x1F2B, 0x0399], []). % GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
unicode_special_casing(0x1F94, [0x1F94], [0x1F9C], [0x1F2C, 0x0399], []). % GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
unicode_special_casing(0x1F95, [0x1F95], [0x1F9D], [0x1F2D, 0x0399], []). % GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
unicode_special_casing(0x1F96, [0x1F96], [0x1F9E], [0x1F2E, 0x0399], []). % GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
unicode_special_casing(0x1F97, [0x1F97], [0x1F9F], [0x1F2F, 0x0399], []). % GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_special_casing(0x1F98, [0x1F90], [0x1F98], [0x1F28, 0x0399], []). % GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
unicode_special_casing(0x1F99, [0x1F91], [0x1F99], [0x1F29, 0x0399], []). % GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
unicode_special_casing(0x1F9A, [0x1F92], [0x1F9A], [0x1F2A, 0x0399], []). % GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
unicode_special_casing(0x1F9B, [0x1F93], [0x1F9B], [0x1F2B, 0x0399], []). % GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
unicode_special_casing(0x1F9C, [0x1F94], [0x1F9C], [0x1F2C, 0x0399], []). % GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
unicode_special_casing(0x1F9D, [0x1F95], [0x1F9D], [0x1F2D, 0x0399], []). % GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
unicode_special_casing(0x1F9E, [0x1F96], [0x1F9E], [0x1F2E, 0x0399], []). % GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
unicode_special_casing(0x1F9F, [0x1F97], [0x1F9F], [0x1F2F, 0x0399], []). % GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_special_casing(0x1FA0, [0x1FA0], [0x1FA8], [0x1F68, 0x0399], []). % GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
unicode_special_casing(0x1FA1, [0x1FA1], [0x1FA9], [0x1F69, 0x0399], []). % GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
unicode_special_casing(0x1FA2, [0x1FA2], [0x1FAA], [0x1F6A, 0x0399], []). % GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
unicode_special_casing(0x1FA3, [0x1FA3], [0x1FAB], [0x1F6B, 0x0399], []). % GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
unicode_special_casing(0x1FA4, [0x1FA4], [0x1FAC], [0x1F6C, 0x0399], []). % GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
unicode_special_casing(0x1FA5, [0x1FA5], [0x1FAD], [0x1F6D, 0x0399], []). % GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
unicode_special_casing(0x1FA6, [0x1FA6], [0x1FAE], [0x1F6E, 0x0399], []). % GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
unicode_special_casing(0x1FA7, [0x1FA7], [0x1FAF], [0x1F6F, 0x0399], []). % GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
unicode_special_casing(0x1FA8, [0x1FA0], [0x1FA8], [0x1F68, 0x0399], []). % GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
unicode_special_casing(0x1FA9, [0x1FA1], [0x1FA9], [0x1F69, 0x0399], []). % GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
unicode_special_casing(0x1FAA, [0x1FA2], [0x1FAA], [0x1F6A, 0x0399], []). % GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
unicode_special_casing(0x1FAB, [0x1FA3], [0x1FAB], [0x1F6B, 0x0399], []). % GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
unicode_special_casing(0x1FAC, [0x1FA4], [0x1FAC], [0x1F6C, 0x0399], []). % GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
unicode_special_casing(0x1FAD, [0x1FA5], [0x1FAD], [0x1F6D, 0x0399], []). % GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
unicode_special_casing(0x1FAE, [0x1FA6], [0x1FAE], [0x1F6E, 0x0399], []). % GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
unicode_special_casing(0x1FAF, [0x1FA7], [0x1FAF], [0x1F6F, 0x0399], []). % GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
unicode_special_casing(0x1FB3, [0x1FB3], [0x1FBC], [0x0391, 0x0399], []). % GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
unicode_special_casing(0x1FBC, [0x1FB3], [0x1FBC], [0x0391, 0x0399], []). % GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_special_casing(0x1FC3, [0x1FC3], [0x1FCC], [0x0397, 0x0399], []). % GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
unicode_special_casing(0x1FCC, [0x1FC3], [0x1FCC], [0x0397, 0x0399], []). % GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_special_casing(0x1FF3, [0x1FF3], [0x1FFC], [0x03A9, 0x0399], []). % GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
unicode_special_casing(0x1FFC, [0x1FF3], [0x1FFC], [0x03A9, 0x0399], []). % GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI

% Some characters with YPOGEGRAMMENI also have no corresponding titlecases

unicode_special_casing(0x1FB2, [0x1FB2], [0x1FBA, 0x0345], [0x1FBA, 0x0399], []). % GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
unicode_special_casing(0x1FB4, [0x1FB4], [0x0386, 0x0345], [0x0386, 0x0399], []). % GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_special_casing(0x1FC2, [0x1FC2], [0x1FCA, 0x0345], [0x1FCA, 0x0399], []). % GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
unicode_special_casing(0x1FC4, [0x1FC4], [0x0389, 0x0345], [0x0389, 0x0399], []). % GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_special_casing(0x1FF2, [0x1FF2], [0x1FFA, 0x0345], [0x1FFA, 0x0399], []). % GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
unicode_special_casing(0x1FF4, [0x1FF4], [0x038F, 0x0345], [0x038F, 0x0399], []). % GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI

unicode_special_casing(0x1FB7, [0x1FB7], [0x0391, 0x0342, 0x0345], [0x0391, 0x0342, 0x0399], []). % GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_special_casing(0x1FC7, [0x1FC7], [0x0397, 0x0342, 0x0345], [0x0397, 0x0342, 0x0399], []). % GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
unicode_special_casing(0x1FF7, [0x1FF7], [0x03A9, 0x0342, 0x0345], [0x03A9, 0x0342, 0x0399], []). % GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI

% ================================================================================
% Conditional Mappings
% The remainder of this file provides conditional casing data used to produce 
% full case mappings.
% ================================================================================
% Language-Insensitive Mappings
% These are characters whose full case mappings do not depend on language, but do
% depend on context (which characters come before or after). For more information
% see the header of this file and the Unicode Standard.
% ================================================================================

% Special case for final form of sigma

unicode_special_casing(0x03A3, [0x03C2], [0x03A3], [0x03A3], ['Final_Sigma']). % GREEK CAPITAL LETTER SIGMA

% Note: the following cases for non-final are already in the UnicodeData file.

% 03A3; 03C3; 03A3; 03A3; # GREEK CAPITAL LETTER SIGMA
% 03C3; 03C3; 03A3; 03A3; # GREEK SMALL LETTER SIGMA
% 03C2; 03C2; 03A3; 03A3; # GREEK SMALL LETTER FINAL SIGMA

% Note: the following cases are not included, since they would case-fold in lowercasing

% 03C3; 03C2; 03A3; 03A3; Final_Sigma; # GREEK SMALL LETTER SIGMA
% 03C2; 03C3; 03A3; 03A3; Not_Final_Sigma; # GREEK SMALL LETTER FINAL SIGMA

% ================================================================================
% Language-Sensitive Mappings
% These are characters whose full case mappings depend on language and perhaps also
% context (which characters come before or after). For more information
% see the header of this file and the Unicode Standard.
% ================================================================================

% Lithuanian

% Lithuanian retains the dot in a lowercase i when followed by accents.

% Remove DOT ABOVE after "i" with upper or titlecase

unicode_special_casing(0x0307, [0x0307], [], [], [lt, 'After_Soft_Dotted']). % COMBINING DOT ABOVE

% Introduce an explicit dot above when lowercasing capital I's and J's
% whenever there are more accents above.
% (of the accents used in Lithuanian: grave, acute, tilde above, and ogonek)

unicode_special_casing(0x0049, [0x0069, 0x0307], [0x0049], [0x0049], [lt, 'More_Above']). % LATIN CAPITAL LETTER I
unicode_special_casing(0x004A, [0x006A, 0x0307], [0x004A], [0x004A], [lt, 'More_Above']). % LATIN CAPITAL LETTER J
unicode_special_casing(0x012E, [0x012F, 0x0307], [0x012E], [0x012E], [lt, 'More_Above']). % LATIN CAPITAL LETTER I WITH OGONEK
unicode_special_casing(0x00CC, [0x0069, 0x0307, 0x0300], [0x00CC], [0x00CC], [lt]). % LATIN CAPITAL LETTER I WITH GRAVE
unicode_special_casing(0x00CD, [0x0069, 0x0307, 0x0301], [0x00CD], [0x00CD], [lt]). % LATIN CAPITAL LETTER I WITH ACUTE
unicode_special_casing(0x0128, [0x0069, 0x0307, 0x0303], [0x0128], [0x0128], [lt]). % LATIN CAPITAL LETTER I WITH TILDE

% ================================================================================

% Turkish and Azeri

% I and i-dotless; I-dot and i are case pairs in Turkish and Azeri
% The following rules handle those cases.

unicode_special_casing(0x0130, [0x0069], [0x0130], [0x0130], [tr]). % LATIN CAPITAL LETTER I WITH DOT ABOVE
unicode_special_casing(0x0130, [0x0069], [0x0130], [0x0130], [az]). % LATIN CAPITAL LETTER I WITH DOT ABOVE

% When lowercasing, remove dot_above in the sequence I + dot_above, which will turn into i.
% This matches the behavior of the canonically equivalent I-dot_above

unicode_special_casing(0x0307, [], [0x0307], [0x0307], [tr, 'After_I']). % COMBINING DOT ABOVE
unicode_special_casing(0x0307, [], [0x0307], [0x0307], [az, 'After_I']). % COMBINING DOT ABOVE

% When lowercasing, unless an I is before a dot_above, it turns into a dotless i.

unicode_special_casing(0x0049, [0x0131], [0x0049], [0x0049], [tr, 'Not_Before_Dot']). % LATIN CAPITAL LETTER I
unicode_special_casing(0x0049, [0x0131], [0x0049], [0x0049], [az, 'Not_Before_Dot']). % LATIN CAPITAL LETTER I

% When uppercasing, i turns into a dotted capital I

unicode_special_casing(0x0069, [0x0069], [0x0130], [0x0130], [tr]). % LATIN SMALL LETTER I
unicode_special_casing(0x0069, [0x0069], [0x0130], [0x0130], [az]). % LATIN SMALL LETTER I

% Note: the following case is already in the UnicodeData file.

% 0131; 0131; 0049; 0049; tr; # LATIN SMALL LETTER DOTLESS I

% EOF
