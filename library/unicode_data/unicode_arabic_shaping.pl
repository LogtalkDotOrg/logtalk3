%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 20, 2012
%
%  Original Unicode file header comments follow

/*
# ArabicShaping-6.1.0.txt
# Date: 2011-04-15, 23:16:00 GMT [KW]
#
# This file is a normative contributory data file in the
# Unicode Character Database.
#
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
#
# This file defines the Joining_Type and Joining_Group
# property values for Arabic, Syriac, N'Ko, and Mandaic
# positional shaping, repeating in machine readable form the
# information exemplified in Tables 8-3, 8-8, 8-9, 8-10, 8-13, 8-14,
# 8-15, 13-5, 14-5, and 14-6 of The Unicode Standard, Version 6.1.
#
# See sections 8.2, 8.3, 13.5, and 14.12 of The Unicode Standard, 
# Version 6.1 for more information.
#
# Each line contains four fields, separated by a semicolon.
#
# Field 0: the code point, in 4-digit hexadecimal
#   form, of an Arabic, Syriac, N'Ko, or Mandaic character.
#
# Field 1: gives a short schematic name for that character.
#   The schematic name is descriptive of the shape, based as
#   consistently as possible on a name for the skeleton and
#   then the diacritic marks applied to the skeleton, if any.
#   Note that this schematic name is considered a comment,
#   and does not constitute a formal property value.
#
# Field 2: defines the joining type (property name: Joining_Type)
#   R Right_Joining
#   L Left_Joining
#   D Dual_Joining
#   C Join_Causing
#   U Non_Joining
#   T Transparent
#     See Section 8.2, Arabic for more information on these types.
#
# Field 3: defines the joining group (property name: Joining_Group)
#
# The values of the joining group are based schematically on character
# names. Where a schematic character name consists of two or more parts separated
# by spaces, the formal Joining_Group property value, as specified in
# PropertyValueAliases.txt, consists of the same name parts joined by
# underscores. Hence, the entry:
#
#   0629, 'TEH MARBUTA', 'R', 'TEH MARBUTA
#
# corresponds to [Joining_Group = Teh_Marbuta].
#
# Note: The property value now designated [Joining_Group = Teh_Marbuta_Goal] 
#   used to apply to both of the following characters
#   in earlier versions of the standard:
#
#   U+06C2 ARABIC LETTER HEH GOAL WITH HAMZA ABOVE
#   U+06C3 ARABIC LETTER TEH MARBUTA GOAL
#
#   However, it currently applies only to U+06C3, and *not* to U+06C2.
#   To avoid destabilizing existing Joining_Group property aliases, the
#   prior Joining_Group value for U+06C3 (Hamza_On_Heh_Goal) has been
#   retained as a property value alias, despite the fact that it
#   no longer applies to its namesake character, U+06C2.
#   See PropertyValueAliases.txt.
#
# When other cursive scripts are added to the Unicode Standard in
# the future, the joining group value of all its letters will default
# to jg=No_Joining_Group in this data file. Other, more specific
# joining group values will be defined only if an explicit proposal
# to define those values exactly has been approved by the UTC. This
# is the convention exemplified by the N'Ko and Mandaic scripts. Only the Arabic
# and Syriac scripts currently have explicit joining group values defined.
#
# Note: Code points that are not explicitly listed in this file are
# either of joining type T or U:
#
# - Those that not explicitly listed that are of General Category Mn, Me, or Cf
#   have joining type T.
# - All others not explicitly listed have joining type U.
#
# For an explicit listing of characters of joining type T, see
# the derived property file DerivedJoiningType.txt.
#
# There are currently no characters of joining type L defined in Unicode.
#
# #############################################################
*/

% Unicode, 'Schematic Name, 'Joining Type, 'Joining Group

% Arabic Characters

unicode_arabic_shaping(0x0600, 'ARABIC NUMBER SIGN', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x0601, 'ARABIC SIGN SANAH', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x0602, 'ARABIC FOOTNOTE MARKER', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x0603, 'ARABIC SIGN SAFHA', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x0604, 'ARABIC SIGN SAMVAT', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x0608, 'ARABIC RAY', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x060B, 'AFGHANI SIGN', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x0620, 'DOTLESS YEH WITH SEPARATE RING BELOW', 'D', 'YEH').
unicode_arabic_shaping(0x0621, 'HAMZA', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x0622, 'ALEF WITH MADDA ABOVE', 'R', 'ALEF').
unicode_arabic_shaping(0x0623, 'ALEF WITH HAMZA ABOVE', 'R', 'ALEF').
unicode_arabic_shaping(0x0624, 'WAW WITH HAMZA ABOVE', 'R', 'WAW').
unicode_arabic_shaping(0x0625, 'ALEF WITH HAMZA BELOW', 'R', 'ALEF').
unicode_arabic_shaping(0x0626, 'DOTLESS YEH WITH HAMZA ABOVE', 'D', 'YEH').
unicode_arabic_shaping(0x0627, 'ALEF', 'R', 'ALEF').
unicode_arabic_shaping(0x0628, 'BEH', 'D', 'BEH').
unicode_arabic_shaping(0x0629, 'TEH MARBUTA', 'R', 'TEH MARBUTA').
unicode_arabic_shaping(0x062A, 'DOTLESS BEH WITH 2 DOTS ABOVE', 'D', 'BEH').
unicode_arabic_shaping(0x062B, 'DOTLESS BEH WITH 3 DOTS ABOVE', 'D', 'BEH').
unicode_arabic_shaping(0x062C, 'HAH WITH DOT BELOW', 'D', 'HAH').
unicode_arabic_shaping(0x062D, 'HAH', 'D', 'HAH').
unicode_arabic_shaping(0x062E, 'HAH WITH DOT ABOVE', 'D', 'HAH').
unicode_arabic_shaping(0x062F, 'DAL', 'R', 'DAL').
unicode_arabic_shaping(0x0630, 'DAL WITH DOT ABOVE', 'R', 'DAL').
unicode_arabic_shaping(0x0631, 'REH', 'R', 'REH').
unicode_arabic_shaping(0x0632, 'REH WITH DOT ABOVE', 'R', 'REH').
unicode_arabic_shaping(0x0633, 'SEEN', 'D', 'SEEN').
unicode_arabic_shaping(0x0634, 'SEEN WITH 3 DOTS ABOVE', 'D', 'SEEN').
unicode_arabic_shaping(0x0635, 'SAD', 'D', 'SAD').
unicode_arabic_shaping(0x0636, 'SAD WITH DOT ABOVE', 'D', 'SAD').
unicode_arabic_shaping(0x0637, 'TAH', 'D', 'TAH').
unicode_arabic_shaping(0x0638, 'TAH WITH DOT ABOVE', 'D', 'TAH').
unicode_arabic_shaping(0x0639, 'AIN', 'D', 'AIN').
unicode_arabic_shaping(0x063A, 'AIN WITH DOT ABOVE', 'D', 'AIN').
unicode_arabic_shaping(0x063B, 'KEHEH WITH 2 DOTS ABOVE', 'D', 'GAF').
unicode_arabic_shaping(0x063C, 'KEHEH WITH 3 DOTS BELOW', 'D', 'GAF').
unicode_arabic_shaping(0x063D, 'FARSI YEH WITH INVERTED V ABOVE', 'D', 'FARSI YEH').
unicode_arabic_shaping(0x063E, 'FARSI YEH WITH 2 DOTS ABOVE', 'D', 'FARSI YEH').
unicode_arabic_shaping(0x063F, 'FARSI YEH WITH 3 DOTS ABOVE', 'D', 'FARSI YEH').
unicode_arabic_shaping(0x0640, 'TATWEEL', 'C', 'No_Joining_Group').
unicode_arabic_shaping(0x0641, 'FEH', 'D', 'FEH').
unicode_arabic_shaping(0x0642, 'QAF', 'D', 'QAF').
unicode_arabic_shaping(0x0643, 'KAF', 'D', 'KAF').
unicode_arabic_shaping(0x0644, 'LAM', 'D', 'LAM').
unicode_arabic_shaping(0x0645, 'MEEM', 'D', 'MEEM').
unicode_arabic_shaping(0x0646, 'NOON', 'D', 'NOON').
unicode_arabic_shaping(0x0647, 'HEH', 'D', 'HEH').
unicode_arabic_shaping(0x0648, 'WAW', 'R', 'WAW').
unicode_arabic_shaping(0x0649, 'DOTLESS YEH', 'D', 'YEH').
unicode_arabic_shaping(0x064A, 'YEH', 'D', 'YEH').
unicode_arabic_shaping(0x066E, 'DOTLESS BEH', 'D', 'BEH').
unicode_arabic_shaping(0x066F, 'DOTLESS QAF', 'D', 'QAF').
unicode_arabic_shaping(0x0671, 'ALEF WITH WASLA ABOVE', 'R', 'ALEF').
unicode_arabic_shaping(0x0672, 'ALEF WITH WAVY HAMZA ABOVE', 'R', 'ALEF').
unicode_arabic_shaping(0x0673, 'ALEF WITH WAVY HAMZA BELOW', 'R', 'ALEF').
unicode_arabic_shaping(0x0674, 'HIGH HAMZA', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x0675, 'HIGH HAMZA ALEF', 'R', 'ALEF').
unicode_arabic_shaping(0x0676, 'HIGH HAMZA WAW', 'R', 'WAW').
unicode_arabic_shaping(0x0677, 'HIGH HAMZA WAW WITH DAMMA ABOVE', 'R', 'WAW').
unicode_arabic_shaping(0x0678, 'HIGH HAMZA DOTLESS YEH', 'D', 'YEH').
unicode_arabic_shaping(0x0679, 'DOTLESS BEH WITH TAH ABOVE', 'D', 'BEH').
unicode_arabic_shaping(0x067A, 'DOTLESS BEH WITH VERTICAL 2 DOTS ABOVE', 'D', 'BEH').
unicode_arabic_shaping(0x067B, 'DOTLESS BEH WITH VERTICAL 2 DOTS BELOW', 'D', 'BEH').
unicode_arabic_shaping(0x067C, 'DOTLESS BEH WITH ATTACHED RING BELOW AND 2 DOTS ABOVE', 'D', 'BEH').
unicode_arabic_shaping(0x067D, 'DOTLESS BEH WITH INVERTED 3 DOTS ABOVE', 'D', 'BEH').
unicode_arabic_shaping(0x067E, 'DOTLESS BEH WITH 3 DOTS BELOW', 'D', 'BEH').
unicode_arabic_shaping(0x067F, 'DOTLESS BEH WITH 4 DOTS ABOVE', 'D', 'BEH').
unicode_arabic_shaping(0x0680, 'DOTLESS BEH WITH 4 DOTS BELOW', 'D', 'BEH').
unicode_arabic_shaping(0x0681, 'HAH WITH HAMZA ABOVE', 'D', 'HAH').
unicode_arabic_shaping(0x0682, 'HAH WITH VERTICAL 2 DOTS ABOVE', 'D', 'HAH').
unicode_arabic_shaping(0x0683, 'HAH WITH 2 DOTS BELOW', 'D', 'HAH').
unicode_arabic_shaping(0x0684, 'HAH WITH VERTICAL 2 DOTS BELOW', 'D', 'HAH').
unicode_arabic_shaping(0x0685, 'HAH WITH 3 DOTS ABOVE', 'D', 'HAH').
unicode_arabic_shaping(0x0686, 'HAH WITH 3 DOTS BELOW', 'D', 'HAH').
unicode_arabic_shaping(0x0687, 'HAH WITH 4 DOTS BELOW', 'D', 'HAH').
unicode_arabic_shaping(0x0688, 'DAL WITH TAH ABOVE', 'R', 'DAL').
unicode_arabic_shaping(0x0689, 'DAL WITH ATTACHED RING BELOW', 'R', 'DAL').
unicode_arabic_shaping(0x068A, 'DAL WITH DOT BELOW', 'R', 'DAL').
unicode_arabic_shaping(0x068B, 'DAL WITH DOT BELOW AND TAH ABOVE', 'R', 'DAL').
unicode_arabic_shaping(0x068C, 'DAL WITH 2 DOTS ABOVE', 'R', 'DAL').
unicode_arabic_shaping(0x068D, 'DAL WITH 2 DOTS BELOW', 'R', 'DAL').
unicode_arabic_shaping(0x068E, 'DAL WITH 3 DOTS ABOVE', 'R', 'DAL').
unicode_arabic_shaping(0x068F, 'DAL WITH INVERTED 3 DOTS ABOVE', 'R', 'DAL').
unicode_arabic_shaping(0x0690, 'DAL WITH 4 DOTS ABOVE', 'R', 'DAL').
unicode_arabic_shaping(0x0691, 'REH WITH TAH ABOVE', 'R', 'REH').
unicode_arabic_shaping(0x0692, 'REH WITH V ABOVE', 'R', 'REH').
unicode_arabic_shaping(0x0693, 'REH WITH ATTACHED RING BELOW', 'R', 'REH').
unicode_arabic_shaping(0x0694, 'REH WITH DOT BELOW', 'R', 'REH').
unicode_arabic_shaping(0x0695, 'REH WITH V BELOW', 'R', 'REH').
unicode_arabic_shaping(0x0696, 'REH WITH DOT BELOW AND DOT WITHIN', 'R', 'REH').
unicode_arabic_shaping(0x0697, 'REH WITH 2 DOTS ABOVE', 'R', 'REH').
unicode_arabic_shaping(0x0698, 'REH WITH 3 DOTS ABOVE', 'R', 'REH').
unicode_arabic_shaping(0x0699, 'REH WITH 4 DOTS ABOVE', 'R', 'REH').
unicode_arabic_shaping(0x069A, 'SEEN WITH DOT BELOW AND DOT ABOVE', 'D', 'SEEN').
unicode_arabic_shaping(0x069B, 'SEEN WITH 3 DOTS BELOW', 'D', 'SEEN').
unicode_arabic_shaping(0x069C, 'SEEN WITH 3 DOTS BELOW AND 3 DOTS ABOVE', 'D', 'SEEN').
unicode_arabic_shaping(0x069D, 'SAD WITH 2 DOTS BELOW', 'D', 'SAD').
unicode_arabic_shaping(0x069E, 'SAD WITH 3 DOTS ABOVE', 'D', 'SAD').
unicode_arabic_shaping(0x069F, 'TAH WITH 3 DOTS ABOVE', 'D', 'TAH').
unicode_arabic_shaping(0x06A0, 'AIN WITH 3 DOTS ABOVE', 'D', 'AIN').
unicode_arabic_shaping(0x06A1, 'DOTLESS FEH', 'D', 'FEH').
unicode_arabic_shaping(0x06A2, 'DOTLESS FEH WITH DOT BELOW', 'D', 'FEH').
unicode_arabic_shaping(0x06A3, 'FEH WITH DOT BELOW', 'D', 'FEH').
unicode_arabic_shaping(0x06A4, 'DOTLESS FEH WITH 3 DOTS ABOVE', 'D', 'FEH').
unicode_arabic_shaping(0x06A5, 'DOTLESS FEH WITH 3 DOTS BELOW', 'D', 'FEH').
unicode_arabic_shaping(0x06A6, 'DOTLESS FEH WITH 4 DOTS ABOVE', 'D', 'FEH').
unicode_arabic_shaping(0x06A7, 'DOTLESS QAF WITH DOT ABOVE', 'D', 'QAF').
unicode_arabic_shaping(0x06A8, 'DOTLESS QAF WITH 3 DOTS ABOVE', 'D', 'QAF').
unicode_arabic_shaping(0x06A9, 'KEHEH', 'D', 'GAF').
unicode_arabic_shaping(0x06AA, 'SWASH KAF', 'D', 'SWASH KAF').
unicode_arabic_shaping(0x06AB, 'KEHEH WITH ATTACHED RING BELOW', 'D', 'GAF').
unicode_arabic_shaping(0x06AC, 'KAF WITH DOT ABOVE', 'D', 'KAF').
unicode_arabic_shaping(0x06AD, 'KAF WITH 3 DOTS ABOVE', 'D', 'KAF').
unicode_arabic_shaping(0x06AE, 'KAF WITH 3 DOTS BELOW', 'D', 'KAF').
unicode_arabic_shaping(0x06AF, 'GAF', 'D', 'GAF').
unicode_arabic_shaping(0x06B0, 'GAF WITH ATTACHED RING BELOW', 'D', 'GAF').
unicode_arabic_shaping(0x06B1, 'GAF WITH 2 DOTS ABOVE', 'D', 'GAF').
unicode_arabic_shaping(0x06B2, 'GAF WITH 2 DOTS BELOW', 'D', 'GAF').
unicode_arabic_shaping(0x06B3, 'GAF WITH VERTICAL 2 DOTS BELOW', 'D', 'GAF').
unicode_arabic_shaping(0x06B4, 'GAF WITH 3 DOTS ABOVE', 'D', 'GAF').
unicode_arabic_shaping(0x06B5, 'LAM WITH V ABOVE', 'D', 'LAM').
unicode_arabic_shaping(0x06B6, 'LAM WITH DOT ABOVE', 'D', 'LAM').
unicode_arabic_shaping(0x06B7, 'LAM WITH 3 DOTS ABOVE', 'D', 'LAM').
unicode_arabic_shaping(0x06B8, 'LAM WITH 3 DOTS BELOW', 'D', 'LAM').
unicode_arabic_shaping(0x06B9, 'NOON WITH DOT BELOW', 'D', 'NOON').
unicode_arabic_shaping(0x06BA, 'DOTLESS NOON', 'D', 'NOON').
unicode_arabic_shaping(0x06BB, 'DOTLESS NOON WITH TAH ABOVE', 'D', 'NOON').
unicode_arabic_shaping(0x06BC, 'NOON WITH ATTACHED RING BELOW', 'D', 'NOON').
unicode_arabic_shaping(0x06BD, 'NYA', 'D', 'NYA').
unicode_arabic_shaping(0x06BE, 'KNOTTED HEH', 'D', 'KNOTTED HEH').
unicode_arabic_shaping(0x06BF, 'HAH WITH 3 DOTS BELOW AND DOT ABOVE', 'D', 'HAH').
unicode_arabic_shaping(0x06C0, 'DOTLESS TEH MARBUTA WITH HAMZA ABOVE', 'R', 'TEH MARBUTA').
unicode_arabic_shaping(0x06C1, 'HEH GOAL', 'D', 'HEH GOAL').
unicode_arabic_shaping(0x06C2, 'HEH GOAL WITH HAMZA ABOVE', 'D', 'HEH GOAL').
unicode_arabic_shaping(0x06C3, 'TEH MARBUTA GOAL', 'R', 'TEH MARBUTA GOAL').
unicode_arabic_shaping(0x06C4, 'WAW WITH ATTACHED RING WITHIN', 'R', 'WAW').
unicode_arabic_shaping(0x06C5, 'WAW WITH BAR', 'R', 'WAW').
unicode_arabic_shaping(0x06C6, 'WAW WITH V ABOVE', 'R', 'WAW').
unicode_arabic_shaping(0x06C7, 'WAW WITH DAMMA ABOVE', 'R', 'WAW').
unicode_arabic_shaping(0x06C8, 'WAW WITH ALEF ABOVE', 'R', 'WAW').
unicode_arabic_shaping(0x06C9, 'WAW WITH INVERTED V ABOVE', 'R', 'WAW').
unicode_arabic_shaping(0x06CA, 'WAW WITH 2 DOTS ABOVE', 'R', 'WAW').
unicode_arabic_shaping(0x06CB, 'WAW WITH 3 DOTS ABOVE', 'R', 'WAW').
unicode_arabic_shaping(0x06CC, 'FARSI YEH', 'D', 'FARSI YEH').
unicode_arabic_shaping(0x06CD, 'YEH WITH TAIL', 'R', 'YEH WITH TAIL').
unicode_arabic_shaping(0x06CE, 'FARSI YEH WITH V ABOVE', 'D', 'FARSI YEH').
unicode_arabic_shaping(0x06CF, 'WAW WITH DOT ABOVE', 'R', 'WAW').
unicode_arabic_shaping(0x06D0, 'DOTLESS YEH WITH VERTICAL 2 DOTS BELOW', 'D', 'YEH').
unicode_arabic_shaping(0x06D1, 'DOTLESS YEH WITH 3 DOTS BELOW', 'D', 'YEH').
unicode_arabic_shaping(0x06D2, 'YEH BARREE', 'R', 'YEH BARREE').
unicode_arabic_shaping(0x06D3, 'YEH BARREE WITH HAMZA ABOVE', 'R', 'YEH BARREE').
unicode_arabic_shaping(0x06D5, 'DOTLESS TEH MARBUTA', 'R', 'TEH MARBUTA').
unicode_arabic_shaping(0x06DD, 'ARABIC END OF AYAH', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x06EE, 'DAL WITH INVERTED V ABOVE', 'R', 'DAL').
unicode_arabic_shaping(0x06EF, 'REH WITH INVERTED V ABOVE', 'R', 'REH').
unicode_arabic_shaping(0x06FA, 'SEEN WITH DOT BELOW AND 3 DOTS ABOVE', 'D', 'SEEN').
unicode_arabic_shaping(0x06FB, 'SAD WITH DOT BELOW AND DOT ABOVE', 'D', 'SAD').
unicode_arabic_shaping(0x06FC, 'AIN WITH DOT BELOW AND DOT ABOVE', 'D', 'AIN').
unicode_arabic_shaping(0x06FF, 'KNOTTED HEH WITH INVERTED V ABOVE', 'D', 'KNOTTED HEH').

% Syriac Characters

unicode_arabic_shaping(0x0710, 'ALAPH', 'R', 'ALAPH').
unicode_arabic_shaping(0x0712, 'BETH', 'D', 'BETH').
unicode_arabic_shaping(0x0713, 'GAMAL', 'D', 'GAMAL').
unicode_arabic_shaping(0x0714, 'GAMAL GARSHUNI', 'D', 'GAMAL').
unicode_arabic_shaping(0x0715, 'DALATH', 'R', 'DALATH RISH').
unicode_arabic_shaping(0x0716, 'DOTLESS DALATH RISH', 'R', 'DALATH RISH').
unicode_arabic_shaping(0x0717, 'HE', 'R', 'HE').
unicode_arabic_shaping(0x0718, 'WAW', 'R', 'SYRIAC WAW').
unicode_arabic_shaping(0x0719, 'ZAIN', 'R', 'ZAIN').
unicode_arabic_shaping(0x071A, 'HETH', 'D', 'HETH').
unicode_arabic_shaping(0x071B, 'TETH', 'D', 'TETH').
unicode_arabic_shaping(0x071C, 'TETH GARSHUNI', 'D', 'TETH').
unicode_arabic_shaping(0x071D, 'YUDH', 'D', 'YUDH').
unicode_arabic_shaping(0x071E, 'YUDH HE', 'R', 'YUDH HE').
unicode_arabic_shaping(0x071F, 'KAPH', 'D', 'KAPH').
unicode_arabic_shaping(0x0720, 'LAMADH', 'D', 'LAMADH').
unicode_arabic_shaping(0x0721, 'MIM', 'D', 'MIM').
unicode_arabic_shaping(0x0722, 'NUN', 'D', 'NUN').
unicode_arabic_shaping(0x0723, 'SEMKATH', 'D', 'SEMKATH').
unicode_arabic_shaping(0x0724, 'FINAL SEMKATH', 'D', 'FINAL SEMKATH').
unicode_arabic_shaping(0x0725, 'E', 'D', 'E').
unicode_arabic_shaping(0x0726, 'PE', 'D', 'PE').
unicode_arabic_shaping(0x0727, 'REVERSED PE', 'D', 'REVERSED PE').
unicode_arabic_shaping(0x0728, 'SADHE', 'R', 'SADHE').
unicode_arabic_shaping(0x0729, 'QAPH', 'D', 'QAPH').
unicode_arabic_shaping(0x072A, 'RISH', 'R', 'DALATH RISH').
unicode_arabic_shaping(0x072B, 'SHIN', 'D', 'SHIN').
unicode_arabic_shaping(0x072C, 'TAW', 'R', 'TAW').
unicode_arabic_shaping(0x072D, 'PERSIAN BHETH', 'D', 'BETH').
unicode_arabic_shaping(0x072E, 'PERSIAN GHAMAL', 'D', 'GAMAL').
unicode_arabic_shaping(0x072F, 'PERSIAN DHALATH', 'R', 'DALATH RISH').
unicode_arabic_shaping(0x074D, 'SOGDIAN ZHAIN', 'R', 'ZHAIN').
unicode_arabic_shaping(0x074E, 'SOGDIAN KHAPH', 'D', 'KHAPH').
unicode_arabic_shaping(0x074F, 'SOGDIAN FE', 'D', 'FE').

% Arabic Supplement Characters

unicode_arabic_shaping(0x0750, 'DOTLESS BEH WITH HORIZONTAL 3 DOTS BELOW', 'D', 'BEH').
unicode_arabic_shaping(0x0751, 'BEH WITH 3 DOTS ABOVE', 'D', 'BEH').
unicode_arabic_shaping(0x0752, 'DOTLESS BEH WITH INVERTED 3 DOTS BELOW', 'D', 'BEH').
unicode_arabic_shaping(0x0753, 'DOTLESS BEH WITH INVERTED 3 DOTS BELOW AND 2 DOTS ABOVE', 'D', 'BEH').
unicode_arabic_shaping(0x0754, 'DOTLESS BEH WITH 2 DOTS BELOW AND DOT ABOVE', 'D', 'BEH').
unicode_arabic_shaping(0x0755, 'DOTLESS BEH WITH INVERTED V BELOW', 'D', 'BEH').
unicode_arabic_shaping(0x0756, 'DOTLESS BEH WITH V ABOVE', 'D', 'BEH').
unicode_arabic_shaping(0x0757, 'HAH WITH 2 DOTS ABOVE', 'D', 'HAH').
unicode_arabic_shaping(0x0758, 'HAH WITH INVERTED 3 DOTS BELOW', 'D', 'HAH').
unicode_arabic_shaping(0x0759, 'DAL WITH VERTICAL 2 DOTS BELOW AND TAH ABOVE', 'R', 'DAL').
unicode_arabic_shaping(0x075A, 'DAL WITH INVERTED V BELOW', 'R', 'DAL').
unicode_arabic_shaping(0x075B, 'REH WITH BAR', 'R', 'REH').
unicode_arabic_shaping(0x075C, 'SEEN WITH 4 DOTS ABOVE', 'D', 'SEEN').
unicode_arabic_shaping(0x075D, 'AIN WITH 2 DOTS ABOVE', 'D', 'AIN').
unicode_arabic_shaping(0x075E, 'AIN WITH INVERTED 3 DOTS ABOVE', 'D', 'AIN').
unicode_arabic_shaping(0x075F, 'AIN WITH VERTICAL 2 DOTS ABOVE', 'D', 'AIN').
unicode_arabic_shaping(0x0760, 'DOTLESS FEH WITH 2 DOTS BELOW', 'D', 'FEH').
unicode_arabic_shaping(0x0761, 'DOTLESS FEH WITH INVERTED 3 DOTS BELOW', 'D', 'FEH').
unicode_arabic_shaping(0x0762, 'KEHEH WITH DOT ABOVE', 'D', 'GAF').
unicode_arabic_shaping(0x0763, 'KEHEH WITH 3 DOTS ABOVE', 'D', 'GAF').
unicode_arabic_shaping(0x0764, 'KEHEH WITH INVERTED 3 DOTS BELOW', 'D', 'GAF').
unicode_arabic_shaping(0x0765, 'MEEM WITH DOT ABOVE', 'D', 'MEEM').
unicode_arabic_shaping(0x0766, 'MEEM WITH DOT BELOW', 'D', 'MEEM').
unicode_arabic_shaping(0x0767, 'NOON WITH 2 DOTS BELOW', 'D', 'NOON').
unicode_arabic_shaping(0x0768, 'NOON WITH TAH ABOVE', 'D', 'NOON').
unicode_arabic_shaping(0x0769, 'NOON WITH V ABOVE', 'D', 'NOON').
unicode_arabic_shaping(0x076A, 'LAM WITH BAR', 'D', 'LAM').
unicode_arabic_shaping(0x076B, 'REH WITH VERTICAL 2 DOTS ABOVE', 'R', 'REH').
unicode_arabic_shaping(0x076C, 'REH WITH HAMZA ABOVE', 'R', 'REH').
unicode_arabic_shaping(0x076D, 'SEEN WITH VERTICAL 2 DOTS ABOVE', 'D', 'SEEN').
unicode_arabic_shaping(0x076E, 'HAH WITH TAH BELOW', 'D', 'HAH').
unicode_arabic_shaping(0x076F, 'HAH WITH TAH AND 2 DOTS BELOW', 'D', 'HAH').
unicode_arabic_shaping(0x0770, 'SEEN WITH 2 DOTS AND TAH ABOVE', 'D', 'SEEN').
unicode_arabic_shaping(0x0771, 'REH WITH 2 DOTS AND TAH ABOVE', 'R', 'REH').
unicode_arabic_shaping(0x0772, 'HAH WITH TAH ABOVE', 'D', 'HAH').
unicode_arabic_shaping(0x0773, 'ALEF WITH DIGIT TWO ABOVE', 'R', 'ALEF').
unicode_arabic_shaping(0x0774, 'ALEF WITH DIGIT THREE ABOVE', 'R', 'ALEF').
unicode_arabic_shaping(0x0775, 'FARSI YEH WITH DIGIT TWO ABOVE', 'D', 'FARSI YEH').
unicode_arabic_shaping(0x0776, 'FARSI YEH WITH DIGIT THREE ABOVE', 'D', 'FARSI YEH').
unicode_arabic_shaping(0x0777, 'DOTLESS YEH WITH DIGIT FOUR BELOW', 'D', 'YEH').
unicode_arabic_shaping(0x0778, 'WAW WITH DIGIT TWO ABOVE', 'R', 'WAW').
unicode_arabic_shaping(0x0779, 'WAW WITH DIGIT THREE ABOVE', 'R', 'WAW').
unicode_arabic_shaping(0x077A, 'BURUSHASKI YEH BARREE WITH DIGIT TWO ABOVE', 'D', 'BURUSHASKI YEH BARREE').
unicode_arabic_shaping(0x077B, 'BURUSHASKI YEH BARREE WITH DIGIT THREE ABOVE', 'D', 'BURUSHASKI YEH BARREE').
unicode_arabic_shaping(0x077C, 'HAH WITH DIGIT FOUR BELOW', 'D', 'HAH').
unicode_arabic_shaping(0x077D, 'SEEN WITH DIGIT FOUR ABOVE', 'D', 'SEEN').
unicode_arabic_shaping(0x077E, 'SEEN WITH INVERTED V ABOVE', 'D', 'SEEN').
unicode_arabic_shaping(0x077F, 'KAF WITH 2 DOTS ABOVE', 'D', 'KAF').

% N'Ko Characters

unicode_arabic_shaping(0x07CA, 'NKO A', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07CB, 'NKO EE', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07CC, 'NKO I', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07CD, 'NKO E', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07CE, 'NKO U', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07CF, 'NKO OO', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07D0, 'NKO O', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07D1, 'NKO DAGBASINNA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07D2, 'NKO N', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07D3, 'NKO BA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07D4, 'NKO PA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07D5, 'NKO TA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07D6, 'NKO JA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07D7, 'NKO CHA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07D8, 'NKO DA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07D9, 'NKO RA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07DA, 'NKO RRA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07DB, 'NKO SA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07DC, 'NKO GBA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07DD, 'NKO FA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07DE, 'NKO KA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07DF, 'NKO LA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07E0, 'NKO NA WOLOSO', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07E1, 'NKO MA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07E2, 'NKO NYA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07E3, 'NKO NA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07E4, 'NKO HA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07E5, 'NKO WA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07E6, 'NKO YA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07E7, 'NKO NYA WOLOSO', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07E8, 'NKO JONA JA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07E9, 'NKO JONA CHA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07EA, 'NKO JONA RA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x07FA, 'NKO LAJANYALAN', 'C', 'No_Joining_Group').

% Mandaic Characters

unicode_arabic_shaping(0x0840, 'MANDAIC HALQA', 'R', 'No_Joining_Group').
unicode_arabic_shaping(0x0841, 'MANDAIC AB', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x0842, 'MANDAIC AG', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x0843, 'MANDAIC AD', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x0844, 'MANDAIC AH', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x0845, 'MANDAIC USHENNA', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x0846, 'MANDAIC AZ', 'R', 'No_Joining_Group').
unicode_arabic_shaping(0x0847, 'MANDAIC IT', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x0848, 'MANDAIC ATT', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x0849, 'MANDAIC AKSA', 'R', 'No_Joining_Group').
unicode_arabic_shaping(0x084A, 'MANDAIC AK', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x084B, 'MANDAIC AL', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x084C, 'MANDAIC AM', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x084D, 'MANDAIC AN', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x084E, 'MANDAIC AS', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x084F, 'MANDAIC IN', 'R', 'No_Joining_Group').
unicode_arabic_shaping(0x0850, 'MANDAIC AP', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x0851, 'MANDAIC ASZ', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x0852, 'MANDAIC AQ', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x0853, 'MANDAIC AR', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x0854, 'MANDAIC ASH', 'R', 'No_Joining_Group').
unicode_arabic_shaping(0x0855, 'MANDAIC AT', 'D', 'No_Joining_Group').
unicode_arabic_shaping(0x0856, 'MANDAIC DUSHENNA', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x0857, 'MANDAIC KAD', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x0858, 'MANDAIC AIN', 'U', 'No_Joining_Group').

% Arabic Extended-A Characters

unicode_arabic_shaping(0x08A0, 'DOTLESS BEH WITH V BELOW', 'D', 'BEH').
unicode_arabic_shaping(0x08A2, 'HAH WITH DOT BELOW AND 2 DOTS ABOVE', 'D', 'HAH').
unicode_arabic_shaping(0x08A3, 'TAH WITH 2 DOTS ABOVE', 'D', 'TAH').
unicode_arabic_shaping(0x08A4, 'DOTLESS FEH WITH DOT BELOW AND 3 DOTS ABOVE', 'D', 'FEH').
unicode_arabic_shaping(0x08A5, 'QAF WITH DOT BELOW', 'D', 'QAF').
unicode_arabic_shaping(0x08A6, 'LAM WITH DOUBLE BAR', 'D', 'LAM').
unicode_arabic_shaping(0x08A7, 'MEEM WITH 3 DOTS ABOVE', 'D', 'MEEM').
unicode_arabic_shaping(0x08A8, 'YEH WITH HAMZA ABOVE', 'D', 'YEH').
unicode_arabic_shaping(0x08A9, 'YEH WITH DOT ABOVE', 'D', 'YEH').
unicode_arabic_shaping(0x08AA, 'REH WITH LOOP', 'R', 'REH').
unicode_arabic_shaping(0x08AB, 'WAW WITH DOT WITHIN', 'R', 'WAW').
unicode_arabic_shaping(0x08AC, 'ROHINGYA YEH', 'R', 'ROHINGYA YEH').

% Other

unicode_arabic_shaping(0x200C, 'ZERO WIDTH NON-JOINER', 'U', 'No_Joining_Group').
unicode_arabic_shaping(0x200D, 'ZERO WIDTH JOINER', 'C', 'No_Joining_Group').

% EOF
