%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 28, 2012
%
%  Original Unicode file header comments follow

/*
# DerivedJoiningGroup-6.1.0.txt
# Date: 2011-07-25, 00:54:14 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

# ================================================

# Joining Group (listing ArabicShaping.txt, field 3)

#  All code points not explicitly listed for Joining_Group
#  have the value No_Joining_Group.

# @missing: 0000..10FFFF; No_Joining_Group
*/

unicode_joining_group(CodePoint, Group) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_joining_group(CodePointStart, CodePointEnd, Group),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_joining_group(CodePoint, _, CodePointGroup) ->
		Group = CodePointGroup
	;	% look for a code point range that includes the given code point
		unicode_joining_group(CodePointStart, CodePointEnd, CodePointGroup),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Group = CodePointGroup
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Group = 'No_Joining_Group'
	).

% ================================================

unicode_joining_group(0x0639, 0x063A, 'Ain'). % Lo   [2] ARABIC LETTER AIN..ARABIC LETTER GHAIN
unicode_joining_group(0x06A0, 0x06A0, 'Ain'). % Lo       ARABIC LETTER AIN WITH THREE DOTS ABOVE
unicode_joining_group(0x06FC, 0x06FC, 'Ain'). % Lo       ARABIC LETTER GHAIN WITH DOT BELOW
unicode_joining_group(0x075D, 0x075F, 'Ain'). % Lo   [3] ARABIC LETTER AIN WITH TWO DOTS ABOVE..ARABIC LETTER AIN WITH TWO DOTS VERTICALLY ABOVE

% Total code points: 7

% ================================================

unicode_joining_group(0x0710, 0x0710, 'Alaph'). % Lo       SYRIAC LETTER ALAPH

% Total code points: 1

% ================================================

unicode_joining_group(0x0622, 0x0623, 'Alef'). % Lo   [2] ARABIC LETTER ALEF WITH MADDA ABOVE..ARABIC LETTER ALEF WITH HAMZA ABOVE
unicode_joining_group(0x0625, 0x0625, 'Alef'). % Lo       ARABIC LETTER ALEF WITH HAMZA BELOW
unicode_joining_group(0x0627, 0x0627, 'Alef'). % Lo       ARABIC LETTER ALEF
unicode_joining_group(0x0671, 0x0673, 'Alef'). % Lo   [3] ARABIC LETTER ALEF WASLA..ARABIC LETTER ALEF WITH WAVY HAMZA BELOW
unicode_joining_group(0x0675, 0x0675, 'Alef'). % Lo       ARABIC LETTER HIGH HAMZA ALEF
unicode_joining_group(0x0773, 0x0774, 'Alef'). % Lo   [2] ARABIC LETTER ALEF WITH EXTENDED ARABIC-INDIC DIGIT TWO ABOVE..ARABIC LETTER ALEF WITH EXTENDED ARABIC-INDIC DIGIT THREE ABOVE

% Total code points: 10

% ================================================

unicode_joining_group(0x0628, 0x0628, 'Beh'). % Lo       ARABIC LETTER BEH
unicode_joining_group(0x062A, 0x062B, 'Beh'). % Lo   [2] ARABIC LETTER TEH..ARABIC LETTER THEH
unicode_joining_group(0x066E, 0x066E, 'Beh'). % Lo       ARABIC LETTER DOTLESS BEH
unicode_joining_group(0x0679, 0x0680, 'Beh'). % Lo   [8] ARABIC LETTER TTEH..ARABIC LETTER BEHEH
unicode_joining_group(0x0750, 0x0756, 'Beh'). % Lo   [7] ARABIC LETTER BEH WITH THREE DOTS HORIZONTALLY BELOW..ARABIC LETTER BEH WITH SMALL V
unicode_joining_group(0x08A0, 0x08A0, 'Beh'). % Lo       ARABIC LETTER BEH WITH SMALL V BELOW

% Total code points: 20

% ================================================

unicode_joining_group(0x0712, 0x0712, 'Beth'). % Lo       SYRIAC LETTER BETH
unicode_joining_group(0x072D, 0x072D, 'Beth'). % Lo       SYRIAC LETTER PERSIAN BHETH

% Total code points: 2

% ================================================

unicode_joining_group(0x062F, 0x0630, 'Dal'). % Lo   [2] ARABIC LETTER DAL..ARABIC LETTER THAL
unicode_joining_group(0x0688, 0x0690, 'Dal'). % Lo   [9] ARABIC LETTER DDAL..ARABIC LETTER DAL WITH FOUR DOTS ABOVE
unicode_joining_group(0x06EE, 0x06EE, 'Dal'). % Lo       ARABIC LETTER DAL WITH INVERTED V
unicode_joining_group(0x0759, 0x075A, 'Dal'). % Lo   [2] ARABIC LETTER DAL WITH TWO DOTS VERTICALLY BELOW AND SMALL TAH..ARABIC LETTER DAL WITH INVERTED SMALL V BELOW

% Total code points: 14

% ================================================

unicode_joining_group(0x0715, 0x0716, 'Dalath_Rish'). % Lo   [2] SYRIAC LETTER DALATH..SYRIAC LETTER DOTLESS DALATH RISH
unicode_joining_group(0x072A, 0x072A, 'Dalath_Rish'). % Lo       SYRIAC LETTER RISH
unicode_joining_group(0x072F, 0x072F, 'Dalath_Rish'). % Lo       SYRIAC LETTER PERSIAN DHALATH

% Total code points: 4

% ================================================

unicode_joining_group(0x0725, 0x0725, 'E'). % Lo       SYRIAC LETTER E

% Total code points: 1

% ================================================

unicode_joining_group(0x0641, 0x0641, 'Feh'). % Lo       ARABIC LETTER FEH
unicode_joining_group(0x06A1, 0x06A6, 'Feh'). % Lo   [6] ARABIC LETTER DOTLESS FEH..ARABIC LETTER PEHEH
unicode_joining_group(0x0760, 0x0761, 'Feh'). % Lo   [2] ARABIC LETTER FEH WITH TWO DOTS BELOW..ARABIC LETTER FEH WITH THREE DOTS POINTING UPWARDS BELOW
unicode_joining_group(0x08A4, 0x08A4, 'Feh'). % Lo       ARABIC LETTER FEH WITH DOT BELOW AND THREE DOTS ABOVE

% Total code points: 10

% ================================================

unicode_joining_group(0x0724, 0x0724, 'Final_Semkath'). % Lo       SYRIAC LETTER FINAL SEMKATH

% Total code points: 1

% ================================================

unicode_joining_group(0x063B, 0x063C, 'Gaf'). % Lo   [2] ARABIC LETTER KEHEH WITH TWO DOTS ABOVE..ARABIC LETTER KEHEH WITH THREE DOTS BELOW
unicode_joining_group(0x06A9, 0x06A9, 'Gaf'). % Lo       ARABIC LETTER KEHEH
unicode_joining_group(0x06AB, 0x06AB, 'Gaf'). % Lo       ARABIC LETTER KAF WITH RING
unicode_joining_group(0x06AF, 0x06B4, 'Gaf'). % Lo   [6] ARABIC LETTER GAF..ARABIC LETTER GAF WITH THREE DOTS ABOVE
unicode_joining_group(0x0762, 0x0764, 'Gaf'). % Lo   [3] ARABIC LETTER KEHEH WITH DOT ABOVE..ARABIC LETTER KEHEH WITH THREE DOTS POINTING UPWARDS BELOW

% Total code points: 13

% ================================================

unicode_joining_group(0x0713, 0x0714, 'Gamal'). % Lo   [2] SYRIAC LETTER GAMAL..SYRIAC LETTER GAMAL GARSHUNI
unicode_joining_group(0x072E, 0x072E, 'Gamal'). % Lo       SYRIAC LETTER PERSIAN GHAMAL

% Total code points: 3

% ================================================

unicode_joining_group(0x062C, 0x062E, 'Hah'). % Lo   [3] ARABIC LETTER JEEM..ARABIC LETTER KHAH
unicode_joining_group(0x0681, 0x0687, 'Hah'). % Lo   [7] ARABIC LETTER HAH WITH HAMZA ABOVE..ARABIC LETTER TCHEHEH
unicode_joining_group(0x06BF, 0x06BF, 'Hah'). % Lo       ARABIC LETTER TCHEH WITH DOT ABOVE
unicode_joining_group(0x0757, 0x0758, 'Hah'). % Lo   [2] ARABIC LETTER HAH WITH TWO DOTS ABOVE..ARABIC LETTER HAH WITH THREE DOTS POINTING UPWARDS BELOW
unicode_joining_group(0x076E, 0x076F, 'Hah'). % Lo   [2] ARABIC LETTER HAH WITH SMALL ARABIC LETTER TAH BELOW..ARABIC LETTER HAH WITH SMALL ARABIC LETTER TAH AND TWO DOTS
unicode_joining_group(0x0772, 0x0772, 'Hah'). % Lo       ARABIC LETTER HAH WITH SMALL ARABIC LETTER TAH ABOVE
unicode_joining_group(0x077C, 0x077C, 'Hah'). % Lo       ARABIC LETTER HAH WITH EXTENDED ARABIC-INDIC DIGIT FOUR BELOW
unicode_joining_group(0x08A2, 0x08A2, 'Hah'). % Lo       ARABIC LETTER JEEM WITH TWO DOTS ABOVE

% Total code points: 18

% ================================================

unicode_joining_group(0x06C3, 0x06C3, 'Teh_Marbuta_Goal'). % Lo       ARABIC LETTER TEH MARBUTA GOAL

% Total code points: 1

% ================================================

unicode_joining_group(0x0717, 0x0717, 'He'). % Lo       SYRIAC LETTER HE

% Total code points: 1

% ================================================

unicode_joining_group(0x0647, 0x0647, 'Heh'). % Lo       ARABIC LETTER HEH

% Total code points: 1

% ================================================

unicode_joining_group(0x06C1, 0x06C2, 'Heh_Goal'). % Lo   [2] ARABIC LETTER HEH GOAL..ARABIC LETTER HEH GOAL WITH HAMZA ABOVE

% Total code points: 2

% ================================================

unicode_joining_group(0x071A, 0x071A, 'Heth'). % Lo       SYRIAC LETTER HETH

% Total code points: 1

% ================================================

unicode_joining_group(0x0643, 0x0643, 'Kaf'). % Lo       ARABIC LETTER KAF
unicode_joining_group(0x06AC, 0x06AE, 'Kaf'). % Lo   [3] ARABIC LETTER KAF WITH DOT ABOVE..ARABIC LETTER KAF WITH THREE DOTS BELOW
unicode_joining_group(0x077F, 0x077F, 'Kaf'). % Lo       ARABIC LETTER KAF WITH TWO DOTS ABOVE

% Total code points: 5

% ================================================

unicode_joining_group(0x071F, 0x071F, 'Kaph'). % Lo       SYRIAC LETTER KAPH

% Total code points: 1

% ================================================

unicode_joining_group(0x06BE, 0x06BE, 'Knotted_Heh'). % Lo       ARABIC LETTER HEH DOACHASHMEE
unicode_joining_group(0x06FF, 0x06FF, 'Knotted_Heh'). % Lo       ARABIC LETTER HEH WITH INVERTED V

% Total code points: 2

% ================================================

unicode_joining_group(0x0644, 0x0644, 'Lam'). % Lo       ARABIC LETTER LAM
unicode_joining_group(0x06B5, 0x06B8, 'Lam'). % Lo   [4] ARABIC LETTER LAM WITH SMALL V..ARABIC LETTER LAM WITH THREE DOTS BELOW
unicode_joining_group(0x076A, 0x076A, 'Lam'). % Lo       ARABIC LETTER LAM WITH BAR
unicode_joining_group(0x08A6, 0x08A6, 'Lam'). % Lo       ARABIC LETTER LAM WITH DOUBLE BAR

% Total code points: 7

% ================================================

unicode_joining_group(0x0720, 0x0720, 'Lamadh'). % Lo       SYRIAC LETTER LAMADH

% Total code points: 1

% ================================================

unicode_joining_group(0x0645, 0x0645, 'Meem'). % Lo       ARABIC LETTER MEEM
unicode_joining_group(0x0765, 0x0766, 'Meem'). % Lo   [2] ARABIC LETTER MEEM WITH DOT ABOVE..ARABIC LETTER MEEM WITH DOT BELOW
unicode_joining_group(0x08A7, 0x08A7, 'Meem'). % Lo       ARABIC LETTER MEEM WITH THREE DOTS ABOVE

% Total code points: 4

% ================================================

unicode_joining_group(0x0721, 0x0721, 'Mim'). % Lo       SYRIAC LETTER MIM

% Total code points: 1

% ================================================

unicode_joining_group(0x0646, 0x0646, 'Noon'). % Lo       ARABIC LETTER NOON
unicode_joining_group(0x06B9, 0x06BC, 'Noon'). % Lo   [4] ARABIC LETTER NOON WITH DOT BELOW..ARABIC LETTER NOON WITH RING
unicode_joining_group(0x0767, 0x0769, 'Noon'). % Lo   [3] ARABIC LETTER NOON WITH TWO DOTS BELOW..ARABIC LETTER NOON WITH SMALL V

% Total code points: 8

% ================================================

unicode_joining_group(0x0722, 0x0722, 'Nun'). % Lo       SYRIAC LETTER NUN

% Total code points: 1

% ================================================

unicode_joining_group(0x0726, 0x0726, 'Pe'). % Lo       SYRIAC LETTER PE

% Total code points: 1

% ================================================

unicode_joining_group(0x0642, 0x0642, 'Qaf'). % Lo       ARABIC LETTER QAF
unicode_joining_group(0x066F, 0x066F, 'Qaf'). % Lo       ARABIC LETTER DOTLESS QAF
unicode_joining_group(0x06A7, 0x06A8, 'Qaf'). % Lo   [2] ARABIC LETTER QAF WITH DOT ABOVE..ARABIC LETTER QAF WITH THREE DOTS ABOVE
unicode_joining_group(0x08A5, 0x08A5, 'Qaf'). % Lo       ARABIC LETTER QAF WITH DOT BELOW

% Total code points: 5

% ================================================

unicode_joining_group(0x0729, 0x0729, 'Qaph'). % Lo       SYRIAC LETTER QAPH

% Total code points: 1

% ================================================

unicode_joining_group(0x0631, 0x0632, 'Reh'). % Lo   [2] ARABIC LETTER REH..ARABIC LETTER ZAIN
unicode_joining_group(0x0691, 0x0699, 'Reh'). % Lo   [9] ARABIC LETTER RREH..ARABIC LETTER REH WITH FOUR DOTS ABOVE
unicode_joining_group(0x06EF, 0x06EF, 'Reh'). % Lo       ARABIC LETTER REH WITH INVERTED V
unicode_joining_group(0x075B, 0x075B, 'Reh'). % Lo       ARABIC LETTER REH WITH STROKE
unicode_joining_group(0x076B, 0x076C, 'Reh'). % Lo   [2] ARABIC LETTER REH WITH TWO DOTS VERTICALLY ABOVE..ARABIC LETTER REH WITH HAMZA ABOVE
unicode_joining_group(0x0771, 0x0771, 'Reh'). % Lo       ARABIC LETTER REH WITH SMALL ARABIC LETTER TAH AND TWO DOTS
unicode_joining_group(0x08AA, 0x08AA, 'Reh'). % Lo       ARABIC LETTER REH WITH LOOP

% Total code points: 17

% ================================================

unicode_joining_group(0x0727, 0x0727, 'Reversed_Pe'). % Lo       SYRIAC LETTER REVERSED PE

% Total code points: 1

% ================================================

unicode_joining_group(0x0635, 0x0636, 'Sad'). % Lo   [2] ARABIC LETTER SAD..ARABIC LETTER DAD
unicode_joining_group(0x069D, 0x069E, 'Sad'). % Lo   [2] ARABIC LETTER SAD WITH TWO DOTS BELOW..ARABIC LETTER SAD WITH THREE DOTS ABOVE
unicode_joining_group(0x06FB, 0x06FB, 'Sad'). % Lo       ARABIC LETTER DAD WITH DOT BELOW

% Total code points: 5

% ================================================

unicode_joining_group(0x0728, 0x0728, 'Sadhe'). % Lo       SYRIAC LETTER SADHE

% Total code points: 1

% ================================================

unicode_joining_group(0x0633, 0x0634, 'Seen'). % Lo   [2] ARABIC LETTER SEEN..ARABIC LETTER SHEEN
unicode_joining_group(0x069A, 0x069C, 'Seen'). % Lo   [3] ARABIC LETTER SEEN WITH DOT BELOW AND DOT ABOVE..ARABIC LETTER SEEN WITH THREE DOTS BELOW AND THREE DOTS ABOVE
unicode_joining_group(0x06FA, 0x06FA, 'Seen'). % Lo       ARABIC LETTER SHEEN WITH DOT BELOW
unicode_joining_group(0x075C, 0x075C, 'Seen'). % Lo       ARABIC LETTER SEEN WITH FOUR DOTS ABOVE
unicode_joining_group(0x076D, 0x076D, 'Seen'). % Lo       ARABIC LETTER SEEN WITH TWO DOTS VERTICALLY ABOVE
unicode_joining_group(0x0770, 0x0770, 'Seen'). % Lo       ARABIC LETTER SEEN WITH SMALL ARABIC LETTER TAH AND TWO DOTS
unicode_joining_group(0x077D, 0x077E, 'Seen'). % Lo   [2] ARABIC LETTER SEEN WITH EXTENDED ARABIC-INDIC DIGIT FOUR ABOVE..ARABIC LETTER SEEN WITH INVERTED V

% Total code points: 11

% ================================================

unicode_joining_group(0x0723, 0x0723, 'Semkath'). % Lo       SYRIAC LETTER SEMKATH

% Total code points: 1

% ================================================

unicode_joining_group(0x072B, 0x072B, 'Shin'). % Lo       SYRIAC LETTER SHIN

% Total code points: 1

% ================================================

unicode_joining_group(0x06AA, 0x06AA, 'Swash_Kaf'). % Lo       ARABIC LETTER SWASH KAF

% Total code points: 1

% ================================================

unicode_joining_group(0x0637, 0x0638, 'Tah'). % Lo   [2] ARABIC LETTER TAH..ARABIC LETTER ZAH
unicode_joining_group(0x069F, 0x069F, 'Tah'). % Lo       ARABIC LETTER TAH WITH THREE DOTS ABOVE
unicode_joining_group(0x08A3, 0x08A3, 'Tah'). % Lo       ARABIC LETTER TAH WITH TWO DOTS ABOVE

% Total code points: 4

% ================================================

unicode_joining_group(0x072C, 0x072C, 'Taw'). % Lo       SYRIAC LETTER TAW

% Total code points: 1

% ================================================

unicode_joining_group(0x0629, 0x0629, 'Teh_Marbuta'). % Lo       ARABIC LETTER TEH MARBUTA
unicode_joining_group(0x06C0, 0x06C0, 'Teh_Marbuta'). % Lo       ARABIC LETTER HEH WITH YEH ABOVE
unicode_joining_group(0x06D5, 0x06D5, 'Teh_Marbuta'). % Lo       ARABIC LETTER AE

% Total code points: 3

% ================================================

unicode_joining_group(0x071B, 0x071C, 'Teth'). % Lo   [2] SYRIAC LETTER TETH..SYRIAC LETTER TETH GARSHUNI

% Total code points: 2

% ================================================

unicode_joining_group(0x0624, 0x0624, 'Waw'). % Lo       ARABIC LETTER WAW WITH HAMZA ABOVE
unicode_joining_group(0x0648, 0x0648, 'Waw'). % Lo       ARABIC LETTER WAW
unicode_joining_group(0x0676, 0x0677, 'Waw'). % Lo   [2] ARABIC LETTER HIGH HAMZA WAW..ARABIC LETTER U WITH HAMZA ABOVE
unicode_joining_group(0x06C4, 0x06CB, 'Waw'). % Lo   [8] ARABIC LETTER WAW WITH RING..ARABIC LETTER VE
unicode_joining_group(0x06CF, 0x06CF, 'Waw'). % Lo       ARABIC LETTER WAW WITH DOT ABOVE
unicode_joining_group(0x0778, 0x0779, 'Waw'). % Lo   [2] ARABIC LETTER WAW WITH EXTENDED ARABIC-INDIC DIGIT TWO ABOVE..ARABIC LETTER WAW WITH EXTENDED ARABIC-INDIC DIGIT THREE ABOVE
unicode_joining_group(0x08AB, 0x08AB, 'Waw'). % Lo       ARABIC LETTER WAW WITH DOT WITHIN

% Total code points: 16

% ================================================

unicode_joining_group(0x0718, 0x0718, 'Syriac_Waw'). % Lo       SYRIAC LETTER WAW

% Total code points: 1

% ================================================

unicode_joining_group(0x0620, 0x0620, 'Yeh'). % Lo       ARABIC LETTER KASHMIRI YEH
unicode_joining_group(0x0626, 0x0626, 'Yeh'). % Lo       ARABIC LETTER YEH WITH HAMZA ABOVE
unicode_joining_group(0x0649, 0x064A, 'Yeh'). % Lo   [2] ARABIC LETTER ALEF MAKSURA..ARABIC LETTER YEH
unicode_joining_group(0x0678, 0x0678, 'Yeh'). % Lo       ARABIC LETTER HIGH HAMZA YEH
unicode_joining_group(0x06D0, 0x06D1, 'Yeh'). % Lo   [2] ARABIC LETTER E..ARABIC LETTER YEH WITH THREE DOTS BELOW
unicode_joining_group(0x0777, 0x0777, 'Yeh'). % Lo       ARABIC LETTER FARSI YEH WITH EXTENDED ARABIC-INDIC DIGIT FOUR BELOW
unicode_joining_group(0x08A8, 0x08A9, 'Yeh'). % Lo   [2] ARABIC LETTER YEH WITH TWO DOTS BELOW AND HAMZA ABOVE..ARABIC LETTER YEH WITH TWO DOTS BELOW AND DOT ABOVE

% Total code points: 10

% ================================================

unicode_joining_group(0x06D2, 0x06D3, 'Yeh_Barree'). % Lo   [2] ARABIC LETTER YEH BARREE..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE

% Total code points: 2

% ================================================

unicode_joining_group(0x06CD, 0x06CD, 'Yeh_With_Tail'). % Lo       ARABIC LETTER YEH WITH TAIL

% Total code points: 1

% ================================================

unicode_joining_group(0x071D, 0x071D, 'Yudh'). % Lo       SYRIAC LETTER YUDH

% Total code points: 1

% ================================================

unicode_joining_group(0x071E, 0x071E, 'Yudh_He'). % Lo       SYRIAC LETTER YUDH HE

% Total code points: 1

% ================================================

unicode_joining_group(0x0719, 0x0719, 'Zain'). % Lo       SYRIAC LETTER ZAIN

% Total code points: 1

% ================================================

unicode_joining_group(0x074D, 0x074D, 'Zhain'). % Lo       SYRIAC LETTER SOGDIAN ZHAIN

% Total code points: 1

% ================================================

unicode_joining_group(0x074E, 0x074E, 'Khaph'). % Lo       SYRIAC LETTER SOGDIAN KHAPH

% Total code points: 1

% ================================================

unicode_joining_group(0x074F, 0x074F, 'Fe'). % Lo       SYRIAC LETTER SOGDIAN FE

% Total code points: 1

% ================================================

unicode_joining_group(0x077A, 0x077B, 'Burushaski_Yeh_Barree'). % Lo   [2] ARABIC LETTER YEH BARREE WITH EXTENDED ARABIC-INDIC DIGIT TWO ABOVE..ARABIC LETTER YEH BARREE WITH EXTENDED ARABIC-INDIC DIGIT THREE ABOVE

% Total code points: 2

% ================================================

unicode_joining_group(0x063D, 0x063F, 'Farsi_Yeh'). % Lo   [3] ARABIC LETTER FARSI YEH WITH INVERTED V..ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE
unicode_joining_group(0x06CC, 0x06CC, 'Farsi_Yeh'). % Lo       ARABIC LETTER FARSI YEH
unicode_joining_group(0x06CE, 0x06CE, 'Farsi_Yeh'). % Lo       ARABIC LETTER YEH WITH SMALL V
unicode_joining_group(0x0775, 0x0776, 'Farsi_Yeh'). % Lo   [2] ARABIC LETTER FARSI YEH WITH EXTENDED ARABIC-INDIC DIGIT TWO ABOVE..ARABIC LETTER FARSI YEH WITH EXTENDED ARABIC-INDIC DIGIT THREE ABOVE

% Total code points: 7

% ================================================

unicode_joining_group(0x06BD, 0x06BD, 'Nya'). % Lo       ARABIC LETTER NOON WITH THREE DOTS ABOVE

% Total code points: 1

% ================================================

unicode_joining_group(0x08AC, 0x08AC, 'Rohingya_Yeh'). % Lo       ARABIC LETTER ROHINGYA YEH

% Total code points: 1

% EOF
