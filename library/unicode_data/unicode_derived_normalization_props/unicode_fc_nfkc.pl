%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 27, 2012
%
%  Original Unicode file header comments follow

/*
# DerivedNormalizationProps-6.1.0.txt
# Date: 2011-07-26, 04:18:07 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
*/

% ================================================

% Derived Property: FC_NFKC_Closure  (DEPRECATED as of Unicode 6.0.0)
%  Generated from computing: b = NFKC(Fold(a)); c = NFKC(Fold(b));
%  Then if (c != b) add the mapping from a to c to the set of
%  mappings that constitute the FC_NFKC_Closure list
%  Uses the full case folding from CaseFolding.txt, without the T option.

unicode_fc_nfkc(0x037A, [0x0020, 0x03B9]).					% Lm  GREEK YPOGEGRAMMENI
unicode_fc_nfkc(0x03D2, [0x03C5]).							% L&  GREEK UPSILON WITH HOOK SYMBOL
unicode_fc_nfkc(0x03D3, [0x03CD]).							% L&  GREEK UPSILON WITH ACUTE AND HOOK SYMBOL
unicode_fc_nfkc(0x03D4, [0x03CB]).							% L&  GREEK UPSILON WITH DIAERESIS AND HOOK SYMBOL
unicode_fc_nfkc(0x03F2, [0x03C3]).							% L&  GREEK LUNATE SIGMA SYMBOL
unicode_fc_nfkc(0x03F9, [0x03C3]).							% L&  GREEK CAPITAL LUNATE SIGMA SYMBOL
unicode_fc_nfkc(0x1D2C, [0x0061]).							% Lm  MODIFIER LETTER CAPITAL A
unicode_fc_nfkc(0x1D2D, [0x00E6]).							% Lm  MODIFIER LETTER CAPITAL AE
unicode_fc_nfkc(0x1D2E, [0x0062]).							% Lm  MODIFIER LETTER CAPITAL B
unicode_fc_nfkc(0x1D30, [0x0064]).							% Lm  MODIFIER LETTER CAPITAL D
unicode_fc_nfkc(0x1D31, [0x0065]).							% Lm  MODIFIER LETTER CAPITAL E
unicode_fc_nfkc(0x1D32, [0x01DD]).							% Lm  MODIFIER LETTER CAPITAL REVERSED E
unicode_fc_nfkc(0x1D33, [0x0067]).							% Lm  MODIFIER LETTER CAPITAL G
unicode_fc_nfkc(0x1D34, [0x0068]).							% Lm  MODIFIER LETTER CAPITAL H
unicode_fc_nfkc(0x1D35, [0x0069]).							% Lm  MODIFIER LETTER CAPITAL I
unicode_fc_nfkc(0x1D36, [0x006A]).							% Lm  MODIFIER LETTER CAPITAL J
unicode_fc_nfkc(0x1D37, [0x006B]).							% Lm  MODIFIER LETTER CAPITAL K
unicode_fc_nfkc(0x1D38, [0x006C]).							% Lm  MODIFIER LETTER CAPITAL L
unicode_fc_nfkc(0x1D39, [0x006D]).							% Lm  MODIFIER LETTER CAPITAL M
unicode_fc_nfkc(0x1D3A, [0x006E]).							% Lm  MODIFIER LETTER CAPITAL N
unicode_fc_nfkc(0x1D3C, [0x006F]).							% Lm  MODIFIER LETTER CAPITAL O
unicode_fc_nfkc(0x1D3D, [0x0223]).							% Lm  MODIFIER LETTER CAPITAL OU
unicode_fc_nfkc(0x1D3E, [0x0070]).							% Lm  MODIFIER LETTER CAPITAL P
unicode_fc_nfkc(0x1D3F, [0x0072]).							% Lm  MODIFIER LETTER CAPITAL R
unicode_fc_nfkc(0x1D40, [0x0074]).							% Lm  MODIFIER LETTER CAPITAL T
unicode_fc_nfkc(0x1D41, [0x0075]).							% Lm  MODIFIER LETTER CAPITAL U
unicode_fc_nfkc(0x1D42, [0x0077]).							% Lm  MODIFIER LETTER CAPITAL W
unicode_fc_nfkc(0x20A8, [0x0072, 0x0073]).					% Sc  RUPEE SIGN
unicode_fc_nfkc(0x2102, [0x0063]).							% L&  DOUBLE-STRUCK CAPITAL C
unicode_fc_nfkc(0x2103, [0x00B0, 0x0063]).					% So  DEGREE CELSIUS
unicode_fc_nfkc(0x2107, [0x025B]).							% L&  EULER CONSTANT
unicode_fc_nfkc(0x2109, [0x00B0, 0x0066]).					% So  DEGREE FAHRENHEIT
unicode_fc_nfkc(0x210B, [0x0068]).							% L&  SCRIPT CAPITAL H
unicode_fc_nfkc(0x210C, [0x0068]).							% L&  BLACK-LETTER CAPITAL H
unicode_fc_nfkc(0x210D, [0x0068]).							% L&  DOUBLE-STRUCK CAPITAL H
unicode_fc_nfkc(0x2110, [0x0069]).							% L&  SCRIPT CAPITAL I
unicode_fc_nfkc(0x2111, [0x0069]).							% L&  BLACK-LETTER CAPITAL I
unicode_fc_nfkc(0x2112, [0x006C]).							% L&  SCRIPT CAPITAL L
unicode_fc_nfkc(0x2115, [0x006E]).							% L&  DOUBLE-STRUCK CAPITAL N
unicode_fc_nfkc(0x2116, [0x006E, 0x006F]).					% So  NUMERO SIGN
unicode_fc_nfkc(0x2119, [0x0070]).							% L&  DOUBLE-STRUCK CAPITAL P
unicode_fc_nfkc(0x211A, [0x0071]).							% L&  DOUBLE-STRUCK CAPITAL Q
unicode_fc_nfkc(0x211B, [0x0072]).							% L&  SCRIPT CAPITAL R
unicode_fc_nfkc(0x211C, [0x0072]).							% L&  BLACK-LETTER CAPITAL R
unicode_fc_nfkc(0x211D, [0x0072]).							% L&  DOUBLE-STRUCK CAPITAL R
unicode_fc_nfkc(0x2120, [0x0073, 0x006D]).					% So  SERVICE MARK
unicode_fc_nfkc(0x2121, [0x0074, 0x0065, 0x006C]).			% So  TELEPHONE SIGN
unicode_fc_nfkc(0x2122, [0x0074, 0x006D]).					% So  TRADE MARK SIGN
unicode_fc_nfkc(0x2124, [0x007A]).							% L&  DOUBLE-STRUCK CAPITAL Z
unicode_fc_nfkc(0x2128, [0x007A]).							% L&  BLACK-LETTER CAPITAL Z
unicode_fc_nfkc(0x212C, [0x0062]).							% L&  SCRIPT CAPITAL B
unicode_fc_nfkc(0x212D, [0x0063]).							% L&  BLACK-LETTER CAPITAL C
unicode_fc_nfkc(0x2130, [0x0065]).							% L&  SCRIPT CAPITAL E
unicode_fc_nfkc(0x2131, [0x0066]).							% L&  SCRIPT CAPITAL F
unicode_fc_nfkc(0x2133, [0x006D]).							% L&  SCRIPT CAPITAL M
unicode_fc_nfkc(0x213B, [0x0066, 0x0061, 0x0078]).			% So  FACSIMILE SIGN
unicode_fc_nfkc(0x213E, [0x03B3]).							% L&  DOUBLE-STRUCK CAPITAL GAMMA
unicode_fc_nfkc(0x213F, [0x03C0]).							% L&  DOUBLE-STRUCK CAPITAL PI
unicode_fc_nfkc(0x2145, [0x0064]).							% L&  DOUBLE-STRUCK ITALIC CAPITAL D
unicode_fc_nfkc(0x2C7D, [0x0076]).							% Lm  MODIFIER LETTER CAPITAL V
unicode_fc_nfkc(0x3250, [0x0070, 0x0074, 0x0065]).			% So  PARTNERSHIP SIGN
unicode_fc_nfkc(0x32CC, [0x0068, 0x0067]).					% So  SQUARE HG
unicode_fc_nfkc(0x32CE, [0x0065, 0x0076]).					% So  SQUARE EV
unicode_fc_nfkc(0x32CF, [0x006C, 0x0074, 0x0064]).			% So  LIMITED LIABILITY SIGN
unicode_fc_nfkc(0x3371, [0x0068, 0x0070, 0x0061]).			% So  SQUARE HPA
unicode_fc_nfkc(0x3373, [0x0061, 0x0075]).					% So  SQUARE AU
unicode_fc_nfkc(0x3375, [0x006F, 0x0076]).					% So  SQUARE OV
unicode_fc_nfkc(0x337A, [0x0069, 0x0075]).					% So  SQUARE IU
unicode_fc_nfkc(0x3380, [0x0070, 0x0061]).					% So  SQUARE PA AMPS
unicode_fc_nfkc(0x3381, [0x006E, 0x0061]).					% So  SQUARE NA
unicode_fc_nfkc(0x3382, [0x03BC, 0x0061]).					% So  SQUARE MU A
unicode_fc_nfkc(0x3383, [0x006D, 0x0061]).					% So  SQUARE MA
unicode_fc_nfkc(0x3384, [0x006B, 0x0061]).					% So  SQUARE KA
unicode_fc_nfkc(0x3385, [0x006B, 0x0062]).					% So  SQUARE KB
unicode_fc_nfkc(0x3386, [0x006D, 0x0062]).					% So  SQUARE MB
unicode_fc_nfkc(0x3387, [0x0067, 0x0062]).					% So  SQUARE GB
unicode_fc_nfkc(0x338A, [0x0070, 0x0066]).					% So  SQUARE PF
unicode_fc_nfkc(0x338B, [0x006E, 0x0066]).					% So  SQUARE NF
unicode_fc_nfkc(0x338C, [0x03BC, 0x0066]).					% So  SQUARE MU F
unicode_fc_nfkc(0x3390, [0x0068, 0x007A]).					% So  SQUARE HZ
unicode_fc_nfkc(0x3391, [0x006B, 0x0068, 0x007A]).			% So  SQUARE KHZ
unicode_fc_nfkc(0x3392, [0x006D, 0x0068, 0x007A]).			% So  SQUARE MHZ
unicode_fc_nfkc(0x3393, [0x0067, 0x0068, 0x007A]).			% So  SQUARE GHZ
unicode_fc_nfkc(0x3394, [0x0074, 0x0068, 0x007A]).			% So  SQUARE THZ
unicode_fc_nfkc(0x33A9, [0x0070, 0x0061]).					% So  SQUARE PA
unicode_fc_nfkc(0x33AA, [0x006B, 0x0070, 0x0061]).			% So  SQUARE KPA
unicode_fc_nfkc(0x33AB, [0x006D, 0x0070, 0x0061]).			% So  SQUARE MPA
unicode_fc_nfkc(0x33AC, [0x0067, 0x0070, 0x0061]).			% So  SQUARE GPA
unicode_fc_nfkc(0x33B4, [0x0070, 0x0076]).					% So  SQUARE PV
unicode_fc_nfkc(0x33B5, [0x006E, 0x0076]).					% So  SQUARE NV
unicode_fc_nfkc(0x33B6, [0x03BC, 0x0076]).					% So  SQUARE MU V
unicode_fc_nfkc(0x33B7, [0x006D, 0x0076]).					% So  SQUARE MV
unicode_fc_nfkc(0x33B8, [0x006B, 0x0076]).					% So  SQUARE KV
unicode_fc_nfkc(0x33B9, [0x006D, 0x0076]).					% So  SQUARE MV MEGA
unicode_fc_nfkc(0x33BA, [0x0070, 0x0077]).					% So  SQUARE PW
unicode_fc_nfkc(0x33BB, [0x006E, 0x0077]).					% So  SQUARE NW
unicode_fc_nfkc(0x33BC, [0x03BC, 0x0077]).					% So  SQUARE MU W
unicode_fc_nfkc(0x33BD, [0x006D, 0x0077]).					% So  SQUARE MW
unicode_fc_nfkc(0x33BE, [0x006B, 0x0077]).					% So  SQUARE KW
unicode_fc_nfkc(0x33BF, [0x006D, 0x0077]).					% So  SQUARE MW MEGA
unicode_fc_nfkc(0x33C0, [0x006B, 0x03C9]).					% So  SQUARE K OHM
unicode_fc_nfkc(0x33C1, [0x006D, 0x03C9]).					% So  SQUARE M OHM
unicode_fc_nfkc(0x33C3, [0x0062, 0x0071]).					% So  SQUARE BQ
unicode_fc_nfkc(0x33C6, [0x0063, 0x2215, 0x006B, 0x0067]).	% So SQUARE C OVER KG
unicode_fc_nfkc(0x33C7, [0x0063, 0x006F, 0x002E]).			% So  SQUARE CO
unicode_fc_nfkc(0x33C8, [0x0064, 0x0062]).					% So  SQUARE DB
unicode_fc_nfkc(0x33C9, [0x0067, 0x0079]).					% So  SQUARE GY
unicode_fc_nfkc(0x33CB, [0x0068, 0x0070]).					% So  SQUARE HP
unicode_fc_nfkc(0x33CD, [0x006B, 0x006B]).					% So  SQUARE KK
unicode_fc_nfkc(0x33CE, [0x006B, 0x006D]).					% So  SQUARE KM CAPITAL
unicode_fc_nfkc(0x33D7, [0x0070, 0x0068]).					% So  SQUARE PH
unicode_fc_nfkc(0x33D9, [0x0070, 0x0070, 0x006D]).			% So  SQUARE PPM
unicode_fc_nfkc(0x33DA, [0x0070, 0x0072]).					% So  SQUARE PR
unicode_fc_nfkc(0x33DC, [0x0073, 0x0076]).					% So  SQUARE SV
unicode_fc_nfkc(0x33DD, [0x0077, 0x0062]).					% So  SQUARE WB
unicode_fc_nfkc(0x33DE, [0x0076, 0x2215, 0x006D]).			% So  SQUARE V OVER M
unicode_fc_nfkc(0x33DF, [0x0061, 0x2215, 0x006D]).			% So  SQUARE A OVER M
unicode_fc_nfkc(0xA7F8, [0x0127]).							% Lm  MODIFIER LETTER CAPITAL H WITH STROKE
unicode_fc_nfkc(0x1D400, [0x0061]).							% L&  MATHEMATICAL BOLD CAPITAL A
unicode_fc_nfkc(0x1D401, [0x0062]).							% L&  MATHEMATICAL BOLD CAPITAL B
unicode_fc_nfkc(0x1D402, [0x0063]).							% L&  MATHEMATICAL BOLD CAPITAL C
unicode_fc_nfkc(0x1D403, [0x0064]).							% L&  MATHEMATICAL BOLD CAPITAL D
unicode_fc_nfkc(0x1D404, [0x0065]).							% L&  MATHEMATICAL BOLD CAPITAL E
unicode_fc_nfkc(0x1D405, [0x0066]).							% L&  MATHEMATICAL BOLD CAPITAL F
unicode_fc_nfkc(0x1D406, [0x0067]).							% L&  MATHEMATICAL BOLD CAPITAL G
unicode_fc_nfkc(0x1D407, [0x0068]).							% L&  MATHEMATICAL BOLD CAPITAL H
unicode_fc_nfkc(0x1D408, [0x0069]).							% L&  MATHEMATICAL BOLD CAPITAL I
unicode_fc_nfkc(0x1D409, [0x006A]).							% L&  MATHEMATICAL BOLD CAPITAL J
unicode_fc_nfkc(0x1D40A, [0x006B]).							% L&  MATHEMATICAL BOLD CAPITAL K
unicode_fc_nfkc(0x1D40B, [0x006C]).							% L&  MATHEMATICAL BOLD CAPITAL L
unicode_fc_nfkc(0x1D40C, [0x006D]).							% L&  MATHEMATICAL BOLD CAPITAL M
unicode_fc_nfkc(0x1D40D, [0x006E]).							% L&  MATHEMATICAL BOLD CAPITAL N
unicode_fc_nfkc(0x1D40E, [0x006F]).							% L&  MATHEMATICAL BOLD CAPITAL O
unicode_fc_nfkc(0x1D40F, [0x0070]).							% L&  MATHEMATICAL BOLD CAPITAL P
unicode_fc_nfkc(0x1D410, [0x0071]).							% L&  MATHEMATICAL BOLD CAPITAL Q
unicode_fc_nfkc(0x1D411, [0x0072]).							% L&  MATHEMATICAL BOLD CAPITAL R
unicode_fc_nfkc(0x1D412, [0x0073]).							% L&  MATHEMATICAL BOLD CAPITAL S
unicode_fc_nfkc(0x1D413, [0x0074]).							% L&  MATHEMATICAL BOLD CAPITAL T
unicode_fc_nfkc(0x1D414, [0x0075]).							% L&  MATHEMATICAL BOLD CAPITAL U
unicode_fc_nfkc(0x1D415, [0x0076]).							% L&  MATHEMATICAL BOLD CAPITAL V
unicode_fc_nfkc(0x1D416, [0x0077]).							% L&  MATHEMATICAL BOLD CAPITAL W
unicode_fc_nfkc(0x1D417, [0x0078]).							% L&  MATHEMATICAL BOLD CAPITAL X
unicode_fc_nfkc(0x1D418, [0x0079]).							% L&  MATHEMATICAL BOLD CAPITAL Y
unicode_fc_nfkc(0x1D419, [0x007A]).							% L&  MATHEMATICAL BOLD CAPITAL Z
unicode_fc_nfkc(0x1D434, [0x0061]).							% L&  MATHEMATICAL ITALIC CAPITAL A
unicode_fc_nfkc(0x1D435, [0x0062]).							% L&  MATHEMATICAL ITALIC CAPITAL B
unicode_fc_nfkc(0x1D436, [0x0063]).							% L&  MATHEMATICAL ITALIC CAPITAL C
unicode_fc_nfkc(0x1D437, [0x0064]).							% L&  MATHEMATICAL ITALIC CAPITAL D
unicode_fc_nfkc(0x1D438, [0x0065]).							% L&  MATHEMATICAL ITALIC CAPITAL E
unicode_fc_nfkc(0x1D439, [0x0066]).							% L&  MATHEMATICAL ITALIC CAPITAL F
unicode_fc_nfkc(0x1D43A, [0x0067]).							% L&  MATHEMATICAL ITALIC CAPITAL G
unicode_fc_nfkc(0x1D43B, [0x0068]).							% L&  MATHEMATICAL ITALIC CAPITAL H
unicode_fc_nfkc(0x1D43C, [0x0069]).							% L&  MATHEMATICAL ITALIC CAPITAL I
unicode_fc_nfkc(0x1D43D, [0x006A]).							% L&  MATHEMATICAL ITALIC CAPITAL J
unicode_fc_nfkc(0x1D43E, [0x006B]).							% L&  MATHEMATICAL ITALIC CAPITAL K
unicode_fc_nfkc(0x1D43F, [0x006C]).							% L&  MATHEMATICAL ITALIC CAPITAL L
unicode_fc_nfkc(0x1D440, [0x006D]).							% L&  MATHEMATICAL ITALIC CAPITAL M
unicode_fc_nfkc(0x1D441, [0x006E]).							% L&  MATHEMATICAL ITALIC CAPITAL N
unicode_fc_nfkc(0x1D442, [0x006F]).							% L&  MATHEMATICAL ITALIC CAPITAL O
unicode_fc_nfkc(0x1D443, [0x0070]).							% L&  MATHEMATICAL ITALIC CAPITAL P
unicode_fc_nfkc(0x1D444, [0x0071]).							% L&  MATHEMATICAL ITALIC CAPITAL Q
unicode_fc_nfkc(0x1D445, [0x0072]).							% L&  MATHEMATICAL ITALIC CAPITAL R
unicode_fc_nfkc(0x1D446, [0x0073]).							% L&  MATHEMATICAL ITALIC CAPITAL S
unicode_fc_nfkc(0x1D447, [0x0074]).							% L&  MATHEMATICAL ITALIC CAPITAL T
unicode_fc_nfkc(0x1D448, [0x0075]).							% L&  MATHEMATICAL ITALIC CAPITAL U
unicode_fc_nfkc(0x1D449, [0x0076]).							% L&  MATHEMATICAL ITALIC CAPITAL V
unicode_fc_nfkc(0x1D44A, [0x0077]).							% L&  MATHEMATICAL ITALIC CAPITAL W
unicode_fc_nfkc(0x1D44B, [0x0078]).							% L&  MATHEMATICAL ITALIC CAPITAL X
unicode_fc_nfkc(0x1D44C, [0x0079]).							% L&  MATHEMATICAL ITALIC CAPITAL Y
unicode_fc_nfkc(0x1D44D, [0x007A]).							% L&  MATHEMATICAL ITALIC CAPITAL Z
unicode_fc_nfkc(0x1D468, [0x0061]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL A
unicode_fc_nfkc(0x1D469, [0x0062]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL B
unicode_fc_nfkc(0x1D46A, [0x0063]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL C
unicode_fc_nfkc(0x1D46B, [0x0064]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL D
unicode_fc_nfkc(0x1D46C, [0x0065]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL E
unicode_fc_nfkc(0x1D46D, [0x0066]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL F
unicode_fc_nfkc(0x1D46E, [0x0067]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL G
unicode_fc_nfkc(0x1D46F, [0x0068]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL H
unicode_fc_nfkc(0x1D470, [0x0069]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL I
unicode_fc_nfkc(0x1D471, [0x006A]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL J
unicode_fc_nfkc(0x1D472, [0x006B]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL K
unicode_fc_nfkc(0x1D473, [0x006C]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL L
unicode_fc_nfkc(0x1D474, [0x006D]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL M
unicode_fc_nfkc(0x1D475, [0x006E]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL N
unicode_fc_nfkc(0x1D476, [0x006F]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL O
unicode_fc_nfkc(0x1D477, [0x0070]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL P
unicode_fc_nfkc(0x1D478, [0x0071]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL Q
unicode_fc_nfkc(0x1D479, [0x0072]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL R
unicode_fc_nfkc(0x1D47A, [0x0073]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL S
unicode_fc_nfkc(0x1D47B, [0x0074]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL T
unicode_fc_nfkc(0x1D47C, [0x0075]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL U
unicode_fc_nfkc(0x1D47D, [0x0076]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL V
unicode_fc_nfkc(0x1D47E, [0x0077]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL W
unicode_fc_nfkc(0x1D47F, [0x0078]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL X
unicode_fc_nfkc(0x1D480, [0x0079]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL Y
unicode_fc_nfkc(0x1D481, [0x007A]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL Z
unicode_fc_nfkc(0x1D49C, [0x0061]).							% L&  MATHEMATICAL SCRIPT CAPITAL A
unicode_fc_nfkc(0x1D49E, [0x0063]).							% L&  MATHEMATICAL SCRIPT CAPITAL C
unicode_fc_nfkc(0x1D49F, [0x0064]).							% L&  MATHEMATICAL SCRIPT CAPITAL D
unicode_fc_nfkc(0x1D4A2, [0x0067]).							% L&  MATHEMATICAL SCRIPT CAPITAL G
unicode_fc_nfkc(0x1D4A5, [0x006A]).							% L&  MATHEMATICAL SCRIPT CAPITAL J
unicode_fc_nfkc(0x1D4A6, [0x006B]).							% L&  MATHEMATICAL SCRIPT CAPITAL K
unicode_fc_nfkc(0x1D4A9, [0x006E]).							% L&  MATHEMATICAL SCRIPT CAPITAL N
unicode_fc_nfkc(0x1D4AA, [0x006F]).							% L&  MATHEMATICAL SCRIPT CAPITAL O
unicode_fc_nfkc(0x1D4AB, [0x0070]).							% L&  MATHEMATICAL SCRIPT CAPITAL P
unicode_fc_nfkc(0x1D4AC, [0x0071]).							% L&  MATHEMATICAL SCRIPT CAPITAL Q
unicode_fc_nfkc(0x1D4AE, [0x0073]).							% L&  MATHEMATICAL SCRIPT CAPITAL S
unicode_fc_nfkc(0x1D4AF, [0x0074]).							% L&  MATHEMATICAL SCRIPT CAPITAL T
unicode_fc_nfkc(0x1D4B0, [0x0075]).							% L&  MATHEMATICAL SCRIPT CAPITAL U
unicode_fc_nfkc(0x1D4B1, [0x0076]).							% L&  MATHEMATICAL SCRIPT CAPITAL V
unicode_fc_nfkc(0x1D4B2, [0x0077]).							% L&  MATHEMATICAL SCRIPT CAPITAL W
unicode_fc_nfkc(0x1D4B3, [0x0078]).							% L&  MATHEMATICAL SCRIPT CAPITAL X
unicode_fc_nfkc(0x1D4B4, [0x0079]).							% L&  MATHEMATICAL SCRIPT CAPITAL Y
unicode_fc_nfkc(0x1D4B5, [0x007A]).							% L&  MATHEMATICAL SCRIPT CAPITAL Z
unicode_fc_nfkc(0x1D4D0, [0x0061]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL A
unicode_fc_nfkc(0x1D4D1, [0x0062]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL B
unicode_fc_nfkc(0x1D4D2, [0x0063]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL C
unicode_fc_nfkc(0x1D4D3, [0x0064]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL D
unicode_fc_nfkc(0x1D4D4, [0x0065]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL E
unicode_fc_nfkc(0x1D4D5, [0x0066]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL F
unicode_fc_nfkc(0x1D4D6, [0x0067]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL G
unicode_fc_nfkc(0x1D4D7, [0x0068]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL H
unicode_fc_nfkc(0x1D4D8, [0x0069]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL I
unicode_fc_nfkc(0x1D4D9, [0x006A]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL J
unicode_fc_nfkc(0x1D4DA, [0x006B]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL K
unicode_fc_nfkc(0x1D4DB, [0x006C]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL L
unicode_fc_nfkc(0x1D4DC, [0x006D]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL M
unicode_fc_nfkc(0x1D4DD, [0x006E]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL N
unicode_fc_nfkc(0x1D4DE, [0x006F]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL O
unicode_fc_nfkc(0x1D4DF, [0x0070]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL P
unicode_fc_nfkc(0x1D4E0, [0x0071]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL Q
unicode_fc_nfkc(0x1D4E1, [0x0072]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL R
unicode_fc_nfkc(0x1D4E2, [0x0073]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL S
unicode_fc_nfkc(0x1D4E3, [0x0074]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL T
unicode_fc_nfkc(0x1D4E4, [0x0075]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL U
unicode_fc_nfkc(0x1D4E5, [0x0076]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL V
unicode_fc_nfkc(0x1D4E6, [0x0077]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL W
unicode_fc_nfkc(0x1D4E7, [0x0078]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL X
unicode_fc_nfkc(0x1D4E8, [0x0079]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL Y
unicode_fc_nfkc(0x1D4E9, [0x007A]).							% L&  MATHEMATICAL BOLD SCRIPT CAPITAL Z
unicode_fc_nfkc(0x1D504, [0x0061]).							% L&  MATHEMATICAL FRAKTUR CAPITAL A
unicode_fc_nfkc(0x1D505, [0x0062]).							% L&  MATHEMATICAL FRAKTUR CAPITAL B
unicode_fc_nfkc(0x1D507, [0x0064]).							% L&  MATHEMATICAL FRAKTUR CAPITAL D
unicode_fc_nfkc(0x1D508, [0x0065]).							% L&  MATHEMATICAL FRAKTUR CAPITAL E
unicode_fc_nfkc(0x1D509, [0x0066]).							% L&  MATHEMATICAL FRAKTUR CAPITAL F
unicode_fc_nfkc(0x1D50A, [0x0067]).							% L&  MATHEMATICAL FRAKTUR CAPITAL G
unicode_fc_nfkc(0x1D50D, [0x006A]).							% L&  MATHEMATICAL FRAKTUR CAPITAL J
unicode_fc_nfkc(0x1D50E, [0x006B]).							% L&  MATHEMATICAL FRAKTUR CAPITAL K
unicode_fc_nfkc(0x1D50F, [0x006C]).							% L&  MATHEMATICAL FRAKTUR CAPITAL L
unicode_fc_nfkc(0x1D510, [0x006D]).							% L&  MATHEMATICAL FRAKTUR CAPITAL M
unicode_fc_nfkc(0x1D511, [0x006E]).							% L&  MATHEMATICAL FRAKTUR CAPITAL N
unicode_fc_nfkc(0x1D512, [0x006F]).							% L&  MATHEMATICAL FRAKTUR CAPITAL O
unicode_fc_nfkc(0x1D513, [0x0070]).							% L&  MATHEMATICAL FRAKTUR CAPITAL P
unicode_fc_nfkc(0x1D514, [0x0071]).							% L&  MATHEMATICAL FRAKTUR CAPITAL Q
unicode_fc_nfkc(0x1D516, [0x0073]).							% L&  MATHEMATICAL FRAKTUR CAPITAL S
unicode_fc_nfkc(0x1D517, [0x0074]).							% L&  MATHEMATICAL FRAKTUR CAPITAL T
unicode_fc_nfkc(0x1D518, [0x0075]).							% L&  MATHEMATICAL FRAKTUR CAPITAL U
unicode_fc_nfkc(0x1D519, [0x0076]).							% L&  MATHEMATICAL FRAKTUR CAPITAL V
unicode_fc_nfkc(0x1D51A, [0x0077]).							% L&  MATHEMATICAL FRAKTUR CAPITAL W
unicode_fc_nfkc(0x1D51B, [0x0078]).							% L&  MATHEMATICAL FRAKTUR CAPITAL X
unicode_fc_nfkc(0x1D51C, [0x0079]).							% L&  MATHEMATICAL FRAKTUR CAPITAL Y
unicode_fc_nfkc(0x1D538, [0x0061]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL A
unicode_fc_nfkc(0x1D539, [0x0062]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_fc_nfkc(0x1D53B, [0x0064]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL D
unicode_fc_nfkc(0x1D53C, [0x0065]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL E
unicode_fc_nfkc(0x1D53D, [0x0066]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL F
unicode_fc_nfkc(0x1D53E, [0x0067]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_fc_nfkc(0x1D540, [0x0069]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL I
unicode_fc_nfkc(0x1D541, [0x006A]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL J
unicode_fc_nfkc(0x1D542, [0x006B]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL K
unicode_fc_nfkc(0x1D543, [0x006C]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL L
unicode_fc_nfkc(0x1D544, [0x006D]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_fc_nfkc(0x1D546, [0x006F]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_fc_nfkc(0x1D54A, [0x0073]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL S
unicode_fc_nfkc(0x1D54B, [0x0074]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL T
unicode_fc_nfkc(0x1D54C, [0x0075]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL U
unicode_fc_nfkc(0x1D54D, [0x0076]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL V
unicode_fc_nfkc(0x1D54E, [0x0077]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL W
unicode_fc_nfkc(0x1D54F, [0x0078]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL X
unicode_fc_nfkc(0x1D550, [0x0079]).							% L&  MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_fc_nfkc(0x1D56C, [0x0061]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL A
unicode_fc_nfkc(0x1D56D, [0x0062]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL B
unicode_fc_nfkc(0x1D56E, [0x0063]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL C
unicode_fc_nfkc(0x1D56F, [0x0064]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL D
unicode_fc_nfkc(0x1D570, [0x0065]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL E
unicode_fc_nfkc(0x1D571, [0x0066]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL F
unicode_fc_nfkc(0x1D572, [0x0067]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL G
unicode_fc_nfkc(0x1D573, [0x0068]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL H
unicode_fc_nfkc(0x1D574, [0x0069]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL I
unicode_fc_nfkc(0x1D575, [0x006A]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL J
unicode_fc_nfkc(0x1D576, [0x006B]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL K
unicode_fc_nfkc(0x1D577, [0x006C]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL L
unicode_fc_nfkc(0x1D578, [0x006D]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL M
unicode_fc_nfkc(0x1D579, [0x006E]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL N
unicode_fc_nfkc(0x1D57A, [0x006F]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL O
unicode_fc_nfkc(0x1D57B, [0x0070]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL P
unicode_fc_nfkc(0x1D57C, [0x0071]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL Q
unicode_fc_nfkc(0x1D57D, [0x0072]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL R
unicode_fc_nfkc(0x1D57E, [0x0073]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL S
unicode_fc_nfkc(0x1D57F, [0x0074]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL T
unicode_fc_nfkc(0x1D580, [0x0075]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL U
unicode_fc_nfkc(0x1D581, [0x0076]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL V
unicode_fc_nfkc(0x1D582, [0x0077]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL W
unicode_fc_nfkc(0x1D583, [0x0078]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL X
unicode_fc_nfkc(0x1D584, [0x0079]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL Y
unicode_fc_nfkc(0x1D585, [0x007A]).							% L&  MATHEMATICAL BOLD FRAKTUR CAPITAL Z
unicode_fc_nfkc(0x1D5A0, [0x0061]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL A
unicode_fc_nfkc(0x1D5A1, [0x0062]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL B
unicode_fc_nfkc(0x1D5A2, [0x0063]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL C
unicode_fc_nfkc(0x1D5A3, [0x0064]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL D
unicode_fc_nfkc(0x1D5A4, [0x0065]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL E
unicode_fc_nfkc(0x1D5A5, [0x0066]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL F
unicode_fc_nfkc(0x1D5A6, [0x0067]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL G
unicode_fc_nfkc(0x1D5A7, [0x0068]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL H
unicode_fc_nfkc(0x1D5A8, [0x0069]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL I
unicode_fc_nfkc(0x1D5A9, [0x006A]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL J
unicode_fc_nfkc(0x1D5AA, [0x006B]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL K
unicode_fc_nfkc(0x1D5AB, [0x006C]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL L
unicode_fc_nfkc(0x1D5AC, [0x006D]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL M
unicode_fc_nfkc(0x1D5AD, [0x006E]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL N
unicode_fc_nfkc(0x1D5AE, [0x006F]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL O
unicode_fc_nfkc(0x1D5AF, [0x0070]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL P
unicode_fc_nfkc(0x1D5B0, [0x0071]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL Q
unicode_fc_nfkc(0x1D5B1, [0x0072]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL R
unicode_fc_nfkc(0x1D5B2, [0x0073]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL S
unicode_fc_nfkc(0x1D5B3, [0x0074]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL T
unicode_fc_nfkc(0x1D5B4, [0x0075]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL U
unicode_fc_nfkc(0x1D5B5, [0x0076]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL V
unicode_fc_nfkc(0x1D5B6, [0x0077]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL W
unicode_fc_nfkc(0x1D5B7, [0x0078]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL X
unicode_fc_nfkc(0x1D5B8, [0x0079]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL Y
unicode_fc_nfkc(0x1D5B9, [0x007A]).							% L&  MATHEMATICAL SANS-SERIF CAPITAL Z
unicode_fc_nfkc(0x1D5D4, [0x0061]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL A
unicode_fc_nfkc(0x1D5D5, [0x0062]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL B
unicode_fc_nfkc(0x1D5D6, [0x0063]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL C
unicode_fc_nfkc(0x1D5D7, [0x0064]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL D
unicode_fc_nfkc(0x1D5D8, [0x0065]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL E
unicode_fc_nfkc(0x1D5D9, [0x0066]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL F
unicode_fc_nfkc(0x1D5DA, [0x0067]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL G
unicode_fc_nfkc(0x1D5DB, [0x0068]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL H
unicode_fc_nfkc(0x1D5DC, [0x0069]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL I
unicode_fc_nfkc(0x1D5DD, [0x006A]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL J
unicode_fc_nfkc(0x1D5DE, [0x006B]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL K
unicode_fc_nfkc(0x1D5DF, [0x006C]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL L
unicode_fc_nfkc(0x1D5E0, [0x006D]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL M
unicode_fc_nfkc(0x1D5E1, [0x006E]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL N
unicode_fc_nfkc(0x1D5E2, [0x006F]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL O
unicode_fc_nfkc(0x1D5E3, [0x0070]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL P
unicode_fc_nfkc(0x1D5E4, [0x0071]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL Q
unicode_fc_nfkc(0x1D5E5, [0x0072]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL R
unicode_fc_nfkc(0x1D5E6, [0x0073]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL S
unicode_fc_nfkc(0x1D5E7, [0x0074]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL T
unicode_fc_nfkc(0x1D5E8, [0x0075]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL U
unicode_fc_nfkc(0x1D5E9, [0x0076]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL V
unicode_fc_nfkc(0x1D5EA, [0x0077]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL W
unicode_fc_nfkc(0x1D5EB, [0x0078]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL X
unicode_fc_nfkc(0x1D5EC, [0x0079]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL Y
unicode_fc_nfkc(0x1D5ED, [0x007A]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL Z
unicode_fc_nfkc(0x1D608, [0x0061]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL A
unicode_fc_nfkc(0x1D609, [0x0062]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL B
unicode_fc_nfkc(0x1D60A, [0x0063]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL C
unicode_fc_nfkc(0x1D60B, [0x0064]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL D
unicode_fc_nfkc(0x1D60C, [0x0065]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL E
unicode_fc_nfkc(0x1D60D, [0x0066]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL F
unicode_fc_nfkc(0x1D60E, [0x0067]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL G
unicode_fc_nfkc(0x1D60F, [0x0068]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL H
unicode_fc_nfkc(0x1D610, [0x0069]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL I
unicode_fc_nfkc(0x1D611, [0x006A]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL J
unicode_fc_nfkc(0x1D612, [0x006B]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL K
unicode_fc_nfkc(0x1D613, [0x006C]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL L
unicode_fc_nfkc(0x1D614, [0x006D]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL M
unicode_fc_nfkc(0x1D615, [0x006E]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL N
unicode_fc_nfkc(0x1D616, [0x006F]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL O
unicode_fc_nfkc(0x1D617, [0x0070]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL P
unicode_fc_nfkc(0x1D618, [0x0071]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL Q
unicode_fc_nfkc(0x1D619, [0x0072]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL R
unicode_fc_nfkc(0x1D61A, [0x0073]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL S
unicode_fc_nfkc(0x1D61B, [0x0074]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL T
unicode_fc_nfkc(0x1D61C, [0x0075]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL U
unicode_fc_nfkc(0x1D61D, [0x0076]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL V
unicode_fc_nfkc(0x1D61E, [0x0077]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL W
unicode_fc_nfkc(0x1D61F, [0x0078]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL X
unicode_fc_nfkc(0x1D620, [0x0079]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL Y
unicode_fc_nfkc(0x1D621, [0x007A]).							% L&  MATHEMATICAL SANS-SERIF ITALIC CAPITAL Z
unicode_fc_nfkc(0x1D63C, [0x0061]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL A
unicode_fc_nfkc(0x1D63D, [0x0062]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL B
unicode_fc_nfkc(0x1D63E, [0x0063]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL C
unicode_fc_nfkc(0x1D63F, [0x0064]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL D
unicode_fc_nfkc(0x1D640, [0x0065]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL E
unicode_fc_nfkc(0x1D641, [0x0066]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL F
unicode_fc_nfkc(0x1D642, [0x0067]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL G
unicode_fc_nfkc(0x1D643, [0x0068]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL H
unicode_fc_nfkc(0x1D644, [0x0069]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL I
unicode_fc_nfkc(0x1D645, [0x006A]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL J
unicode_fc_nfkc(0x1D646, [0x006B]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL K
unicode_fc_nfkc(0x1D647, [0x006C]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL L
unicode_fc_nfkc(0x1D648, [0x006D]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL M
unicode_fc_nfkc(0x1D649, [0x006E]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL N
unicode_fc_nfkc(0x1D64A, [0x006F]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL O
unicode_fc_nfkc(0x1D64B, [0x0070]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL P
unicode_fc_nfkc(0x1D64C, [0x0071]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Q
unicode_fc_nfkc(0x1D64D, [0x0072]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL R
unicode_fc_nfkc(0x1D64E, [0x0073]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL S
unicode_fc_nfkc(0x1D64F, [0x0074]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL T
unicode_fc_nfkc(0x1D650, [0x0075]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL U
unicode_fc_nfkc(0x1D651, [0x0076]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL V
unicode_fc_nfkc(0x1D652, [0x0077]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL W
unicode_fc_nfkc(0x1D653, [0x0078]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL X
unicode_fc_nfkc(0x1D654, [0x0079]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Y
unicode_fc_nfkc(0x1D655, [0x007A]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Z
unicode_fc_nfkc(0x1D670, [0x0061]).							% L&  MATHEMATICAL MONOSPACE CAPITAL A
unicode_fc_nfkc(0x1D671, [0x0062]).							% L&  MATHEMATICAL MONOSPACE CAPITAL B
unicode_fc_nfkc(0x1D672, [0x0063]).							% L&  MATHEMATICAL MONOSPACE CAPITAL C
unicode_fc_nfkc(0x1D673, [0x0064]).							% L&  MATHEMATICAL MONOSPACE CAPITAL D
unicode_fc_nfkc(0x1D674, [0x0065]).							% L&  MATHEMATICAL MONOSPACE CAPITAL E
unicode_fc_nfkc(0x1D675, [0x0066]).							% L&  MATHEMATICAL MONOSPACE CAPITAL F
unicode_fc_nfkc(0x1D676, [0x0067]).							% L&  MATHEMATICAL MONOSPACE CAPITAL G
unicode_fc_nfkc(0x1D677, [0x0068]).							% L&  MATHEMATICAL MONOSPACE CAPITAL H
unicode_fc_nfkc(0x1D678, [0x0069]).							% L&  MATHEMATICAL MONOSPACE CAPITAL I
unicode_fc_nfkc(0x1D679, [0x006A]).							% L&  MATHEMATICAL MONOSPACE CAPITAL J
unicode_fc_nfkc(0x1D67A, [0x006B]).							% L&  MATHEMATICAL MONOSPACE CAPITAL K
unicode_fc_nfkc(0x1D67B, [0x006C]).							% L&  MATHEMATICAL MONOSPACE CAPITAL L
unicode_fc_nfkc(0x1D67C, [0x006D]).							% L&  MATHEMATICAL MONOSPACE CAPITAL M
unicode_fc_nfkc(0x1D67D, [0x006E]).							% L&  MATHEMATICAL MONOSPACE CAPITAL N
unicode_fc_nfkc(0x1D67E, [0x006F]).							% L&  MATHEMATICAL MONOSPACE CAPITAL O
unicode_fc_nfkc(0x1D67F, [0x0070]).							% L&  MATHEMATICAL MONOSPACE CAPITAL P
unicode_fc_nfkc(0x1D680, [0x0071]).							% L&  MATHEMATICAL MONOSPACE CAPITAL Q
unicode_fc_nfkc(0x1D681, [0x0072]).							% L&  MATHEMATICAL MONOSPACE CAPITAL R
unicode_fc_nfkc(0x1D682, [0x0073]).							% L&  MATHEMATICAL MONOSPACE CAPITAL S
unicode_fc_nfkc(0x1D683, [0x0074]).							% L&  MATHEMATICAL MONOSPACE CAPITAL T
unicode_fc_nfkc(0x1D684, [0x0075]).							% L&  MATHEMATICAL MONOSPACE CAPITAL U
unicode_fc_nfkc(0x1D685, [0x0076]).							% L&  MATHEMATICAL MONOSPACE CAPITAL V
unicode_fc_nfkc(0x1D686, [0x0077]).							% L&  MATHEMATICAL MONOSPACE CAPITAL W
unicode_fc_nfkc(0x1D687, [0x0078]).							% L&  MATHEMATICAL MONOSPACE CAPITAL X
unicode_fc_nfkc(0x1D688, [0x0079]).							% L&  MATHEMATICAL MONOSPACE CAPITAL Y
unicode_fc_nfkc(0x1D689, [0x007A]).							% L&  MATHEMATICAL MONOSPACE CAPITAL Z
unicode_fc_nfkc(0x1D6A8, [0x03B1]).							% L&  MATHEMATICAL BOLD CAPITAL ALPHA
unicode_fc_nfkc(0x1D6A9, [0x03B2]).							% L&  MATHEMATICAL BOLD CAPITAL BETA
unicode_fc_nfkc(0x1D6AA, [0x03B3]).							% L&  MATHEMATICAL BOLD CAPITAL GAMMA
unicode_fc_nfkc(0x1D6AB, [0x03B4]).							% L&  MATHEMATICAL BOLD CAPITAL DELTA
unicode_fc_nfkc(0x1D6AC, [0x03B5]).							% L&  MATHEMATICAL BOLD CAPITAL EPSILON
unicode_fc_nfkc(0x1D6AD, [0x03B6]).							% L&  MATHEMATICAL BOLD CAPITAL ZETA
unicode_fc_nfkc(0x1D6AE, [0x03B7]).							% L&  MATHEMATICAL BOLD CAPITAL ETA
unicode_fc_nfkc(0x1D6AF, [0x03B8]).							% L&  MATHEMATICAL BOLD CAPITAL THETA
unicode_fc_nfkc(0x1D6B0, [0x03B9]).							% L&  MATHEMATICAL BOLD CAPITAL IOTA
unicode_fc_nfkc(0x1D6B1, [0x03BA]).							% L&  MATHEMATICAL BOLD CAPITAL KAPPA
unicode_fc_nfkc(0x1D6B2, [0x03BB]).							% L&  MATHEMATICAL BOLD CAPITAL LAMDA
unicode_fc_nfkc(0x1D6B3, [0x03BC]).							% L&  MATHEMATICAL BOLD CAPITAL MU
unicode_fc_nfkc(0x1D6B4, [0x03BD]).							% L&  MATHEMATICAL BOLD CAPITAL NU
unicode_fc_nfkc(0x1D6B5, [0x03BE]).							% L&  MATHEMATICAL BOLD CAPITAL XI
unicode_fc_nfkc(0x1D6B6, [0x03BF]).							% L&  MATHEMATICAL BOLD CAPITAL OMICRON
unicode_fc_nfkc(0x1D6B7, [0x03C0]).							% L&  MATHEMATICAL BOLD CAPITAL PI
unicode_fc_nfkc(0x1D6B8, [0x03C1]).							% L&  MATHEMATICAL BOLD CAPITAL RHO
unicode_fc_nfkc(0x1D6B9, [0x03B8]).							% L&  MATHEMATICAL BOLD CAPITAL THETA SYMBOL
unicode_fc_nfkc(0x1D6BA, [0x03C3]).							% L&  MATHEMATICAL BOLD CAPITAL SIGMA
unicode_fc_nfkc(0x1D6BB, [0x03C4]).							% L&  MATHEMATICAL BOLD CAPITAL TAU
unicode_fc_nfkc(0x1D6BC, [0x03C5]).							% L&  MATHEMATICAL BOLD CAPITAL UPSILON
unicode_fc_nfkc(0x1D6BD, [0x03C6]).							% L&  MATHEMATICAL BOLD CAPITAL PHI
unicode_fc_nfkc(0x1D6BE, [0x03C7]).							% L&  MATHEMATICAL BOLD CAPITAL CHI
unicode_fc_nfkc(0x1D6BF, [0x03C8]).							% L&  MATHEMATICAL BOLD CAPITAL PSI
unicode_fc_nfkc(0x1D6C0, [0x03C9]).							% L&  MATHEMATICAL BOLD CAPITAL OMEGA
unicode_fc_nfkc(0x1D6D3, [0x03C3]).							% L&  MATHEMATICAL BOLD SMALL FINAL SIGMA
unicode_fc_nfkc(0x1D6E2, [0x03B1]).							% L&  MATHEMATICAL ITALIC CAPITAL ALPHA
unicode_fc_nfkc(0x1D6E3, [0x03B2]).							% L&  MATHEMATICAL ITALIC CAPITAL BETA
unicode_fc_nfkc(0x1D6E4, [0x03B3]).							% L&  MATHEMATICAL ITALIC CAPITAL GAMMA
unicode_fc_nfkc(0x1D6E5, [0x03B4]).							% L&  MATHEMATICAL ITALIC CAPITAL DELTA
unicode_fc_nfkc(0x1D6E6, [0x03B5]).							% L&  MATHEMATICAL ITALIC CAPITAL EPSILON
unicode_fc_nfkc(0x1D6E7, [0x03B6]).							% L&  MATHEMATICAL ITALIC CAPITAL ZETA
unicode_fc_nfkc(0x1D6E8, [0x03B7]).							% L&  MATHEMATICAL ITALIC CAPITAL ETA
unicode_fc_nfkc(0x1D6E9, [0x03B8]).							% L&  MATHEMATICAL ITALIC CAPITAL THETA
unicode_fc_nfkc(0x1D6EA, [0x03B9]).							% L&  MATHEMATICAL ITALIC CAPITAL IOTA
unicode_fc_nfkc(0x1D6EB, [0x03BA]).							% L&  MATHEMATICAL ITALIC CAPITAL KAPPA
unicode_fc_nfkc(0x1D6EC, [0x03BB]).							% L&  MATHEMATICAL ITALIC CAPITAL LAMDA
unicode_fc_nfkc(0x1D6ED, [0x03BC]).							% L&  MATHEMATICAL ITALIC CAPITAL MU
unicode_fc_nfkc(0x1D6EE, [0x03BD]).							% L&  MATHEMATICAL ITALIC CAPITAL NU
unicode_fc_nfkc(0x1D6EF, [0x03BE]).							% L&  MATHEMATICAL ITALIC CAPITAL XI
unicode_fc_nfkc(0x1D6F0, [0x03BF]).							% L&  MATHEMATICAL ITALIC CAPITAL OMICRON
unicode_fc_nfkc(0x1D6F1, [0x03C0]).							% L&  MATHEMATICAL ITALIC CAPITAL PI
unicode_fc_nfkc(0x1D6F2, [0x03C1]).							% L&  MATHEMATICAL ITALIC CAPITAL RHO
unicode_fc_nfkc(0x1D6F3, [0x03B8]).							% L&  MATHEMATICAL ITALIC CAPITAL THETA SYMBOL
unicode_fc_nfkc(0x1D6F4, [0x03C3]).							% L&  MATHEMATICAL ITALIC CAPITAL SIGMA
unicode_fc_nfkc(0x1D6F5, [0x03C4]).							% L&  MATHEMATICAL ITALIC CAPITAL TAU
unicode_fc_nfkc(0x1D6F6, [0x03C5]).							% L&  MATHEMATICAL ITALIC CAPITAL UPSILON
unicode_fc_nfkc(0x1D6F7, [0x03C6]).							% L&  MATHEMATICAL ITALIC CAPITAL PHI
unicode_fc_nfkc(0x1D6F8, [0x03C7]).							% L&  MATHEMATICAL ITALIC CAPITAL CHI
unicode_fc_nfkc(0x1D6F9, [0x03C8]).							% L&  MATHEMATICAL ITALIC CAPITAL PSI
unicode_fc_nfkc(0x1D6FA, [0x03C9]).							% L&  MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_fc_nfkc(0x1D70D, [0x03C3]).							% L&  MATHEMATICAL ITALIC SMALL FINAL SIGMA
unicode_fc_nfkc(0x1D71C, [0x03B1]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL ALPHA
unicode_fc_nfkc(0x1D71D, [0x03B2]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL BETA
unicode_fc_nfkc(0x1D71E, [0x03B3]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL GAMMA
unicode_fc_nfkc(0x1D71F, [0x03B4]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL DELTA
unicode_fc_nfkc(0x1D720, [0x03B5]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL EPSILON
unicode_fc_nfkc(0x1D721, [0x03B6]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL ZETA
unicode_fc_nfkc(0x1D722, [0x03B7]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL ETA
unicode_fc_nfkc(0x1D723, [0x03B8]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL THETA
unicode_fc_nfkc(0x1D724, [0x03B9]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL IOTA
unicode_fc_nfkc(0x1D725, [0x03BA]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL KAPPA
unicode_fc_nfkc(0x1D726, [0x03BB]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL LAMDA
unicode_fc_nfkc(0x1D727, [0x03BC]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL MU
unicode_fc_nfkc(0x1D728, [0x03BD]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL NU
unicode_fc_nfkc(0x1D729, [0x03BE]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL XI
unicode_fc_nfkc(0x1D72A, [0x03BF]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL OMICRON
unicode_fc_nfkc(0x1D72B, [0x03C0]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL PI
unicode_fc_nfkc(0x1D72C, [0x03C1]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL RHO
unicode_fc_nfkc(0x1D72D, [0x03B8]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL THETA SYMBOL
unicode_fc_nfkc(0x1D72E, [0x03C3]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL SIGMA
unicode_fc_nfkc(0x1D72F, [0x03C4]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL TAU
unicode_fc_nfkc(0x1D730, [0x03C5]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL UPSILON
unicode_fc_nfkc(0x1D731, [0x03C6]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL PHI
unicode_fc_nfkc(0x1D732, [0x03C7]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL CHI
unicode_fc_nfkc(0x1D733, [0x03C8]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL PSI
unicode_fc_nfkc(0x1D734, [0x03C9]).							% L&  MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_fc_nfkc(0x1D747, [0x03C3]).							% L&  MATHEMATICAL BOLD ITALIC SMALL FINAL SIGMA
unicode_fc_nfkc(0x1D756, [0x03B1]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL ALPHA
unicode_fc_nfkc(0x1D757, [0x03B2]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL BETA
unicode_fc_nfkc(0x1D758, [0x03B3]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL GAMMA
unicode_fc_nfkc(0x1D759, [0x03B4]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL DELTA
unicode_fc_nfkc(0x1D75A, [0x03B5]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL EPSILON
unicode_fc_nfkc(0x1D75B, [0x03B6]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL ZETA
unicode_fc_nfkc(0x1D75C, [0x03B7]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL ETA
unicode_fc_nfkc(0x1D75D, [0x03B8]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA
unicode_fc_nfkc(0x1D75E, [0x03B9]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL IOTA
unicode_fc_nfkc(0x1D75F, [0x03BA]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL KAPPA
unicode_fc_nfkc(0x1D760, [0x03BB]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL LAMDA
unicode_fc_nfkc(0x1D761, [0x03BC]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL MU
unicode_fc_nfkc(0x1D762, [0x03BD]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL NU
unicode_fc_nfkc(0x1D763, [0x03BE]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL XI
unicode_fc_nfkc(0x1D764, [0x03BF]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL OMICRON
unicode_fc_nfkc(0x1D765, [0x03C0]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL PI
unicode_fc_nfkc(0x1D766, [0x03C1]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL RHO
unicode_fc_nfkc(0x1D767, [0x03B8]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA SYMBOL
unicode_fc_nfkc(0x1D768, [0x03C3]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL SIGMA
unicode_fc_nfkc(0x1D769, [0x03C4]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL TAU
unicode_fc_nfkc(0x1D76A, [0x03C5]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL UPSILON
unicode_fc_nfkc(0x1D76B, [0x03C6]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL PHI
unicode_fc_nfkc(0x1D76C, [0x03C7]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL CHI
unicode_fc_nfkc(0x1D76D, [0x03C8]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL PSI
unicode_fc_nfkc(0x1D76E, [0x03C9]).							% L&  MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_fc_nfkc(0x1D781, [0x03C3]).							% L&  MATHEMATICAL SANS-SERIF BOLD SMALL FINAL SIGMA
unicode_fc_nfkc(0x1D790, [0x03B1]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ALPHA
unicode_fc_nfkc(0x1D791, [0x03B2]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL BETA
unicode_fc_nfkc(0x1D792, [0x03B3]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL GAMMA
unicode_fc_nfkc(0x1D793, [0x03B4]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL DELTA
unicode_fc_nfkc(0x1D794, [0x03B5]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL EPSILON
unicode_fc_nfkc(0x1D795, [0x03B6]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ZETA
unicode_fc_nfkc(0x1D796, [0x03B7]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ETA
unicode_fc_nfkc(0x1D797, [0x03B8]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA
unicode_fc_nfkc(0x1D798, [0x03B9]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL IOTA
unicode_fc_nfkc(0x1D799, [0x03BA]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL KAPPA
unicode_fc_nfkc(0x1D79A, [0x03BB]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL LAMDA
unicode_fc_nfkc(0x1D79B, [0x03BC]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL MU
unicode_fc_nfkc(0x1D79C, [0x03BD]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL NU
unicode_fc_nfkc(0x1D79D, [0x03BE]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL XI
unicode_fc_nfkc(0x1D79E, [0x03BF]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMICRON
unicode_fc_nfkc(0x1D79F, [0x03C0]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PI
unicode_fc_nfkc(0x1D7A0, [0x03C1]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL RHO
unicode_fc_nfkc(0x1D7A1, [0x03B8]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA SYMBOL
unicode_fc_nfkc(0x1D7A2, [0x03C3]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL SIGMA
unicode_fc_nfkc(0x1D7A3, [0x03C4]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL TAU
unicode_fc_nfkc(0x1D7A4, [0x03C5]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL UPSILON
unicode_fc_nfkc(0x1D7A5, [0x03C6]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PHI
unicode_fc_nfkc(0x1D7A6, [0x03C7]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL CHI
unicode_fc_nfkc(0x1D7A7, [0x03C8]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PSI
unicode_fc_nfkc(0x1D7A8, [0x03C9]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_fc_nfkc(0x1D7BB, [0x03C3]).							% L&  MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL FINAL SIGMA
unicode_fc_nfkc(0x1D7CA, [0x03DD]).							% L&  MATHEMATICAL BOLD CAPITAL DIGAMMA
unicode_fc_nfkc(0x1F110, [0x0028, 0x0061, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER A
unicode_fc_nfkc(0x1F111, [0x0028, 0x0062, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER B
unicode_fc_nfkc(0x1F112, [0x0028, 0x0063, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER C
unicode_fc_nfkc(0x1F113, [0x0028, 0x0064, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER D
unicode_fc_nfkc(0x1F114, [0x0028, 0x0065, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER E
unicode_fc_nfkc(0x1F115, [0x0028, 0x0066, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER F
unicode_fc_nfkc(0x1F116, [0x0028, 0x0067, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER G
unicode_fc_nfkc(0x1F117, [0x0028, 0x0068, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER H
unicode_fc_nfkc(0x1F118, [0x0028, 0x0069, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER I
unicode_fc_nfkc(0x1F119, [0x0028, 0x006A, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER J
unicode_fc_nfkc(0x1F11A, [0x0028, 0x006B, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER K
unicode_fc_nfkc(0x1F11B, [0x0028, 0x006C, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER L
unicode_fc_nfkc(0x1F11C, [0x0028, 0x006D, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER M
unicode_fc_nfkc(0x1F11D, [0x0028, 0x006E, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER N
unicode_fc_nfkc(0x1F11E, [0x0028, 0x006F, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER O
unicode_fc_nfkc(0x1F11F, [0x0028, 0x0070, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER P
unicode_fc_nfkc(0x1F120, [0x0028, 0x0071, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER Q
unicode_fc_nfkc(0x1F121, [0x0028, 0x0072, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER R
unicode_fc_nfkc(0x1F122, [0x0028, 0x0073, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER S
unicode_fc_nfkc(0x1F123, [0x0028, 0x0074, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER T
unicode_fc_nfkc(0x1F124, [0x0028, 0x0075, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER U
unicode_fc_nfkc(0x1F125, [0x0028, 0x0076, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER V
unicode_fc_nfkc(0x1F126, [0x0028, 0x0077, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER W
unicode_fc_nfkc(0x1F127, [0x0028, 0x0078, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER X
unicode_fc_nfkc(0x1F128, [0x0028, 0x0079, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER Y
unicode_fc_nfkc(0x1F129, [0x0028, 0x007A, 0x0029]).			% So  PARENTHESIZED LATIN CAPITAL LETTER Z
unicode_fc_nfkc(0x1F12A, [0x3014, 0x0073, 0x3015]).			% So  TORTOISE SHELL BRACKETED LATIN CAPITAL LETTER S
unicode_fc_nfkc(0x1F12B, [0x0063]).							% So  CIRCLED ITALIC LATIN CAPITAL LETTER C
unicode_fc_nfkc(0x1F12C, [0x0072]).							% So  CIRCLED ITALIC LATIN CAPITAL LETTER R
unicode_fc_nfkc(0x1F12D, [0x0063, 0x0064]).					% So  CIRCLED CD
unicode_fc_nfkc(0x1F12E, [0x0077, 0x007A]).					% So  CIRCLED WZ
unicode_fc_nfkc(0x1F130, [0x0061]).							% So  SQUARED LATIN CAPITAL LETTER A
unicode_fc_nfkc(0x1F131, [0x0062]).							% So  SQUARED LATIN CAPITAL LETTER B
unicode_fc_nfkc(0x1F132, [0x0063]).							% So  SQUARED LATIN CAPITAL LETTER C
unicode_fc_nfkc(0x1F133, [0x0064]).							% So  SQUARED LATIN CAPITAL LETTER D
unicode_fc_nfkc(0x1F134, [0x0065]).							% So  SQUARED LATIN CAPITAL LETTER E
unicode_fc_nfkc(0x1F135, [0x0066]).							% So  SQUARED LATIN CAPITAL LETTER F
unicode_fc_nfkc(0x1F136, [0x0067]).							% So  SQUARED LATIN CAPITAL LETTER G
unicode_fc_nfkc(0x1F137, [0x0068]).							% So  SQUARED LATIN CAPITAL LETTER H
unicode_fc_nfkc(0x1F138, [0x0069]).							% So  SQUARED LATIN CAPITAL LETTER I
unicode_fc_nfkc(0x1F139, [0x006A]).							% So  SQUARED LATIN CAPITAL LETTER J
unicode_fc_nfkc(0x1F13A, [0x006B]).							% So  SQUARED LATIN CAPITAL LETTER K
unicode_fc_nfkc(0x1F13B, [0x006C]).							% So  SQUARED LATIN CAPITAL LETTER L
unicode_fc_nfkc(0x1F13C, [0x006D]).							% So  SQUARED LATIN CAPITAL LETTER M
unicode_fc_nfkc(0x1F13D, [0x006E]).							% So  SQUARED LATIN CAPITAL LETTER N
unicode_fc_nfkc(0x1F13E, [0x006F]).							% So  SQUARED LATIN CAPITAL LETTER O
unicode_fc_nfkc(0x1F13F, [0x0070]).							% So  SQUARED LATIN CAPITAL LETTER P
unicode_fc_nfkc(0x1F140, [0x0071]).							% So  SQUARED LATIN CAPITAL LETTER Q
unicode_fc_nfkc(0x1F141, [0x0072]).							% So  SQUARED LATIN CAPITAL LETTER R
unicode_fc_nfkc(0x1F142, [0x0073]).							% So  SQUARED LATIN CAPITAL LETTER S
unicode_fc_nfkc(0x1F143, [0x0074]).							% So  SQUARED LATIN CAPITAL LETTER T
unicode_fc_nfkc(0x1F144, [0x0075]).							% So  SQUARED LATIN CAPITAL LETTER U
unicode_fc_nfkc(0x1F145, [0x0076]).							% So  SQUARED LATIN CAPITAL LETTER V
unicode_fc_nfkc(0x1F146, [0x0077]).							% So  SQUARED LATIN CAPITAL LETTER W
unicode_fc_nfkc(0x1F147, [0x0078]).							% So  SQUARED LATIN CAPITAL LETTER X
unicode_fc_nfkc(0x1F148, [0x0079]).							% So  SQUARED LATIN CAPITAL LETTER Y
unicode_fc_nfkc(0x1F149, [0x007A]).							% So  SQUARED LATIN CAPITAL LETTER Z
unicode_fc_nfkc(0x1F14A, [0x0068, 0x0076]).					% So  SQUARED HV
unicode_fc_nfkc(0x1F14B, [0x006D, 0x0076]).					% So  SQUARED MV
unicode_fc_nfkc(0x1F14C, [0x0073, 0x0064]).					% So  SQUARED SD
unicode_fc_nfkc(0x1F14D, [0x0073, 0x0073]).					% So  SQUARED SS
unicode_fc_nfkc(0x1F14E, [0x0070, 0x0070, 0x0076]).			% So  SQUARED PPV
unicode_fc_nfkc(0x1F14F, [0x0077, 0x0063]).					% So  SQUARED WC
unicode_fc_nfkc(0x1F16A, [0x006D, 0x0063]).					% So  RAISED MC SIGN
unicode_fc_nfkc(0x1F16B, [0x006D, 0x0064]).					% So  RAISED MD SIGN
unicode_fc_nfkc(0x1F190, [0x0064, 0x006A]).					% So  SQUARE DJ

% Total code points: 633
