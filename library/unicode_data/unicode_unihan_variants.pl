%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: April 23, 2013
%
%  Original Unicode file header comments follow
%
%  In those cases where there multiple choices, the first one is taken

/*
#
# Unihan_Variants.txt
# Date: 2012-08-17 17:13:30 GMT [JHJ]
# Unicode version: 6.2.0
#
# Unicode Character Database
# Copyright (c) 1991-2012 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
#
# This file contains data on the following fields from the Unihan database:
#	kCompatibilityVariant
#	kSemanticVariant
#	kSimplifiedVariant
#	kSpecializedSemanticVariant
#	kTraditionalVariant
#	kZVariant
#
# For details on the file format, see http://www.unicode.org/reports/tr38/
#
*/

unicode_unihan_variant(CodePoint, CodePointVariant) :-
	unicode_unihan_variant(CodePoint, _, CodePointVariant).

unicode_unihan_variant(0x3400, kSemanticVariant, 0x4E18).
unicode_unihan_variant(0x3405, kSemanticVariant, 0x4E94). %<kMatthews
unicode_unihan_variant(0x342B, kSemanticVariant, 0x51F6).
unicode_unihan_variant(0x342E, kSemanticVariant, 0x8944).
unicode_unihan_variant(0x342F, kSemanticVariant, 0x5EB8).
unicode_unihan_variant(0x3431, kSemanticVariant, 0x9B12). %<kMatthews
unicode_unihan_variant(0x3434, kSemanticVariant, 0x6500).
unicode_unihan_variant(0x343D, kTraditionalVariant, 0x5051).
unicode_unihan_variant(0x3441, kSemanticVariant, 0x20B74).
unicode_unihan_variant(0x3444, kSemanticVariant, 0x4FAE). %<kMatthews
unicode_unihan_variant(0x3447, kTraditionalVariant, 0x3473).
unicode_unihan_variant(0x3448, kTraditionalVariant, 0x5032).
unicode_unihan_variant(0x344C, kSemanticVariant, 0x5C29). %<kMatthews
unicode_unihan_variant(0x3454, kTraditionalVariant, 0x346F).
unicode_unihan_variant(0x345D, kSemanticVariant, 0x22671). %<kMeyerWempe
unicode_unihan_variant(0x3469, kTraditionalVariant, 0x5138).
unicode_unihan_variant(0x346F, kSimplifiedVariant, 0x3454).
unicode_unihan_variant(0x3473, kSimplifiedVariant, 0x3447).
unicode_unihan_variant(0x3487, kSemanticVariant, 0x511B). %<kMatthews
unicode_unihan_variant(0x348B, kSemanticVariant, 0x5EDD). %<kMatthews 0x53AE<kMatthews
unicode_unihan_variant(0x3493, kSimplifiedVariant, 0x20242).
unicode_unihan_variant(0x349A, kSemanticVariant, 0x7A69). %<kFenn,kMatthews
unicode_unihan_variant(0x349A, kSpecializedSemanticVariant, 0x6587). %<kFenn
unicode_unihan_variant(0x349E, kSemanticVariant, 0x5136). %<kMatthews
unicode_unihan_variant(0x34B0, kSemanticVariant, 0x5168). %<kMatthews
unicode_unihan_variant(0x34B2, kSemanticVariant, 0x8CA1). %<kMatthews
unicode_unihan_variant(0x34B7, kSemanticVariant, 0x8208). %<kLau,kMatthews
unicode_unihan_variant(0x34B7, kSpecializedSemanticVariant, 0x8208). %<kFenn
unicode_unihan_variant(0x34C1, kSemanticVariant, 0x7F51). %<kMatthews
unicode_unihan_variant(0x34DF, kSpecializedSemanticVariant, 0x62AB). %<kMeyerWempe
unicode_unihan_variant(0x34E5, kTraditionalVariant, 0x528F).
unicode_unihan_variant(0x34E8, kSimplifiedVariant, 0x523E).
unicode_unihan_variant(0x34EE, kSemanticVariant, 0x5F6B). %<kLau,kMatthews 0x96D5<kLau,kMatthews
unicode_unihan_variant(0x34F7, kSemanticVariant, 0x5293). %<kMatthews
unicode_unihan_variant(0x34F8, kSemanticVariant, 0x65B2). %<kMatthews
unicode_unihan_variant(0x3509, kTraditionalVariant, 0x529A).
unicode_unihan_variant(0x350D, kSemanticVariant, 0x4977). %<kMatthews
unicode_unihan_variant(0x3531, kSemanticVariant, 0x659E). %<kMatthews
unicode_unihan_variant(0x353E, kSemanticVariant, 0x5369). %<kMatthews
unicode_unihan_variant(0x353E, kSpecializedSemanticVariant, 0x5369). %<kFenn
unicode_unihan_variant(0x3551, kSemanticVariant, 0x2228D). %<kLau 0x53A8<kLau
unicode_unihan_variant(0x355A, kSemanticVariant, 0x722A). %<kMatthews 0x722B<kMatthews
unicode_unihan_variant(0x355E, kSemanticVariant, 0x5237). %<kMatthews
unicode_unihan_variant(0x3588, kZVariant, 0x439B).
unicode_unihan_variant(0x358A, kTraditionalVariant, 0x565A).
unicode_unihan_variant(0x359E, kTraditionalVariant, 0x558E).
unicode_unihan_variant(0x35D6, kSemanticVariant, 0x5556). %<kMatthews 0x5649<kMatthews 0x5557<kMatthews
unicode_unihan_variant(0x35F2, kSimplifiedVariant, 0x20D7E).
unicode_unihan_variant(0x35F3, kSemanticVariant, 0x55D2). %<kLau
unicode_unihan_variant(0x3605, kSemanticVariant, 0x929C). %<kMeyerWempe 0x5563<kFenn
unicode_unihan_variant(0x360E, kTraditionalVariant, 0x361A).
unicode_unihan_variant(0x361A, kSimplifiedVariant, 0x360E).
unicode_unihan_variant(0x363D, kZVariant, 0x39B3).
unicode_unihan_variant(0x366E, kSemanticVariant, 0x5854). %<kMeyerWempe
unicode_unihan_variant(0x3673, kSemanticVariant, 0x8F57). %<kMeyerWempe
unicode_unihan_variant(0x369D, kSemanticVariant, 0x595E). %<kMatthews 0x594E<kMatthews
unicode_unihan_variant(0x36A3, kSemanticVariant, 0x59E3). %<kMatthews
unicode_unihan_variant(0x36AF, kTraditionalVariant, 0x3704).
unicode_unihan_variant(0x36C0, kTraditionalVariant, 0x5AB0).
unicode_unihan_variant(0x36DB, kSemanticVariant, 0x5A20). %<kMatthews
unicode_unihan_variant(0x36DB, kSpecializedSemanticVariant, 0x5A20). %<kFenn
unicode_unihan_variant(0x36DF, kTraditionalVariant, 0x217B5).
unicode_unihan_variant(0x36E0, kTraditionalVariant, 0x21883).
unicode_unihan_variant(0x36E3, kTraditionalVariant, 0x370F).
unicode_unihan_variant(0x36E4, kTraditionalVariant, 0x5B4B).
unicode_unihan_variant(0x36FF, kTraditionalVariant, 0x21839).
unicode_unihan_variant(0x3704, kSimplifiedVariant, 0x36AF).
unicode_unihan_variant(0x370F, kSimplifiedVariant, 0x36E3).
unicode_unihan_variant(0x3722, kSimplifiedVariant, 0x217B1).
unicode_unihan_variant(0x3737, kSimplifiedVariant, 0x21760).
unicode_unihan_variant(0x3743, kSemanticVariant, 0x5A29). %<kMeyerWempe
unicode_unihan_variant(0x375B, kSemanticVariant, 0x5BBF). %<kMatthews
unicode_unihan_variant(0x3775, kSemanticVariant, 0x7919). %<kMatthews 0x788D<kMatthews
unicode_unihan_variant(0x3790, kSemanticVariant, 0x5C45). %<kMatthews
unicode_unihan_variant(0x379E, kSimplifiedVariant, 0x2AA0A).
unicode_unihan_variant(0x37C6, kTraditionalVariant, 0x380F).
unicode_unihan_variant(0x37D7, kSemanticVariant, 0x90A0). %<kMatthews 0x8C73<kMatthews
unicode_unihan_variant(0x37DC, kTraditionalVariant, 0x21FB1).
unicode_unihan_variant(0x380A, kSemanticVariant, 0x5D87). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x380F, kSimplifiedVariant, 0x37C6).
unicode_unihan_variant(0x382F, kSemanticVariant, 0x4EE5). %<kFenn
unicode_unihan_variant(0x3836, kSemanticVariant, 0x5E06). %<kMatthews
unicode_unihan_variant(0x3858, kSemanticVariant, 0x2214F).
unicode_unihan_variant(0x387F, kSemanticVariant, 0x65A5). %<kMatthews
unicode_unihan_variant(0x389D, kSimplifiedVariant, 0x222C8).
unicode_unihan_variant(0x38F6, kSemanticVariant, 0x9085). %<kMeyerWempe
unicode_unihan_variant(0x38FA, kSemanticVariant, 0x5FC3). %<kMatthews
unicode_unihan_variant(0x38FC, kSemanticVariant, 0x5FCD). %<kMatthews
unicode_unihan_variant(0x3912, kSemanticVariant, 0x7B28). %<kFenn
unicode_unihan_variant(0x3918, kTraditionalVariant, 0x396E).
unicode_unihan_variant(0x3935, kSemanticVariant, 0x6148). %<kFenn
unicode_unihan_variant(0x3943, kSemanticVariant, 0x60B6). %<kMatthews,kMeyerWempe 0x61E3<kMatthews
unicode_unihan_variant(0x396E, kSimplifiedVariant, 0x3918).
unicode_unihan_variant(0x3975, kSemanticVariant, 0x6141). %<kMatthews
unicode_unihan_variant(0x398E, kSimplifiedVariant, 0x226EF).
unicode_unihan_variant(0x39A7, kSemanticVariant, 0x61AF). %<kMeyerWempe
unicode_unihan_variant(0x39B3, kZVariant, 0x363D).
unicode_unihan_variant(0x39C3, kSemanticVariant, 0x6536). %<kMatthews
unicode_unihan_variant(0x39CF, kTraditionalVariant, 0x6386).
unicode_unihan_variant(0x39D0, kTraditionalVariant, 0x3A73).
unicode_unihan_variant(0x39D1, kTraditionalVariant, 0x649D).
unicode_unihan_variant(0x39D6, kSemanticVariant, 0x627C). %<kMatthews 0x6424<kMatthews
unicode_unihan_variant(0x39DC, kSemanticVariant, 0x64F8). %<kMatthews
unicode_unihan_variant(0x39DF, kTraditionalVariant, 0x64D3).
unicode_unihan_variant(0x39F0, kTraditionalVariant, 0x64FD).
unicode_unihan_variant(0x3A09, kSemanticVariant, 0x62BF). %<kMatthews
unicode_unihan_variant(0x3A28, kSemanticVariant, 0x3A45). %<kMeyerWempe
unicode_unihan_variant(0x3A2B, kTraditionalVariant, 0x3A5C).
unicode_unihan_variant(0x3A3C, kSemanticVariant, 0x6482). %<kFenn
unicode_unihan_variant(0x3A41, kSemanticVariant, 0x6409). %<kMatthews
unicode_unihan_variant(0x3A45, kSemanticVariant, 0x3A28). %<kMeyerWempe
unicode_unihan_variant(0x3A5C, kSemanticVariant, 0x652C). %<kMeyerWempe 0x64E5<kMeyerWempe
unicode_unihan_variant(0x3A5C, kSimplifiedVariant, 0x3A2B).
unicode_unihan_variant(0x3A73, kSimplifiedVariant, 0x39D0).
unicode_unihan_variant(0x3A79, kSemanticVariant, 0x22DA3). %<kMatthews
unicode_unihan_variant(0x3A85, kSemanticVariant, 0x66F4). %<kMatthews
unicode_unihan_variant(0x3AAF, kSemanticVariant, 0x8209).
unicode_unihan_variant(0x3AC3, kSemanticVariant, 0x5043). %<kMatthews
unicode_unihan_variant(0x3ADA, kSemanticVariant, 0x6612). %<kHanYu
unicode_unihan_variant(0x3ADA, kZVariant, 0x66F6).
unicode_unihan_variant(0x3B05, kSemanticVariant, 0x66FC). %<kMatthews
unicode_unihan_variant(0x3B31, kSemanticVariant, 0x6701). %<kMatthews
unicode_unihan_variant(0x3B39, kSemanticVariant, 0x8E2D). %<kMeyerWempe
unicode_unihan_variant(0x3B4E, kTraditionalVariant, 0x68E1).
unicode_unihan_variant(0x3B4F, kTraditionalVariant, 0x6932).
unicode_unihan_variant(0x3B63, kTraditionalVariant, 0x2364E).
unicode_unihan_variant(0x3B64, kTraditionalVariant, 0x6A22).
unicode_unihan_variant(0x3B68, kSemanticVariant, 0x6930). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x3B74, kTraditionalVariant, 0x6A2B).
unicode_unihan_variant(0x3BA3, kSemanticVariant, 0x69E9). %<kMatthews 0x69EA<kMatthews
unicode_unihan_variant(0x3BC3, kSemanticVariant, 0x6F06). %<kMatthews 0x687C<kMatthews
unicode_unihan_variant(0x3BED, kSemanticVariant, 0x6A10). %<kMatthews 0x6AD3<kMatthews
unicode_unihan_variant(0x3BF6, kSemanticVariant, 0x68D5). %<kFenn
unicode_unihan_variant(0x3BFD, kSemanticVariant, 0x6AB3). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x3C0D, kSemanticVariant, 0x6AF3). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x3C69, kTraditionalVariant, 0x6BB0).
unicode_unihan_variant(0x3C6E, kTraditionalVariant, 0x6BA8).
unicode_unihan_variant(0x3C7F, kSemanticVariant, 0x6BBC). %<kMatthews 0x58F3<kMatthews
unicode_unihan_variant(0x3C92, kSemanticVariant, 0x52FD). %<kMathews,kMeyerWempe
unicode_unihan_variant(0x3C93, kSemanticVariant, 0x6BE7). %<kMatthews
unicode_unihan_variant(0x3CBF, kTraditionalVariant, 0x7007).
unicode_unihan_variant(0x3CC4, kSemanticVariant, 0x6D8E). %<kMatthews
unicode_unihan_variant(0x3CD4, kTraditionalVariant, 0x6FE7).
unicode_unihan_variant(0x3CD5, kTraditionalVariant, 0x7061).
unicode_unihan_variant(0x3CE0, kTraditionalVariant, 0x6FBE).
unicode_unihan_variant(0x3CE1, kTraditionalVariant, 0x6FC4).
unicode_unihan_variant(0x3CE2, kTraditionalVariant, 0x23FB7).
unicode_unihan_variant(0x3CFD, kTraditionalVariant, 0x7030).
unicode_unihan_variant(0x3D11, kSemanticVariant, 0x6EAF). %<kMatthews
unicode_unihan_variant(0x3D11, kSpecializedSemanticVariant, 0x6EAF). %<kFenn
unicode_unihan_variant(0x3D14, kSemanticVariant, 0x6F5D). %<kMatthews
unicode_unihan_variant(0x3D52, kSemanticVariant, 0x6CB8). %<kFenn
unicode_unihan_variant(0x3D89, kTraditionalVariant, 0x9E02).
unicode_unihan_variant(0x3DB6, kTraditionalVariant, 0x71F6).
unicode_unihan_variant(0x3DBD, kTraditionalVariant, 0x7171).
unicode_unihan_variant(0x3DC9, kSemanticVariant, 0x71A8). %<kMatthews
unicode_unihan_variant(0x3DC9, kSpecializedSemanticVariant, 0x71A8). %<kFenn
unicode_unihan_variant(0x3DE0, kSemanticVariant, 0x71D0). %<kMatthews 0x7CA6<kMatthews
unicode_unihan_variant(0x3DFB, kSemanticVariant, 0x7173). %<kMatthews
unicode_unihan_variant(0x3DFF, kSimplifiedVariant, 0x24237).
unicode_unihan_variant(0x3E8D, kTraditionalVariant, 0x7371).
unicode_unihan_variant(0x3E8F, kSimplifiedVariant, 0x2480B).
unicode_unihan_variant(0x3EC5, kTraditionalVariant, 0x74AF).
unicode_unihan_variant(0x3ECF, kTraditionalVariant, 0x24AE9).
unicode_unihan_variant(0x3ED8, kTraditionalVariant, 0x24ABA).
unicode_unihan_variant(0x3F63, kSemanticVariant, 0x57DF). %<kMatthews
unicode_unihan_variant(0x3F7D, kSemanticVariant, 0x75FC). %<kMeyerWempe
unicode_unihan_variant(0x3F99, kSemanticVariant, 0x812A). %<kMatthews
unicode_unihan_variant(0x3FDF, kSemanticVariant, 0x76A6). %<kFenn 0x768E<kMatthews
unicode_unihan_variant(0x3FE7, kSimplifiedVariant, 0x24F6F).
unicode_unihan_variant(0x3FF7, kSemanticVariant, 0x7CD9). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x400B, kSemanticVariant, 0x9E7D). %<kLau
unicode_unihan_variant(0x4020, kSemanticVariant, 0x77BF). %<kMatthews
unicode_unihan_variant(0x4022, kSemanticVariant, 0x77AC). %<kMatthews
unicode_unihan_variant(0x4025, kTraditionalVariant, 0x407B).
unicode_unihan_variant(0x4039, kSemanticVariant, 0x776B). %<kMatthews
unicode_unihan_variant(0x4039, kSimplifiedVariant, 0x25174).
unicode_unihan_variant(0x4056, kTraditionalVariant, 0x779C).
unicode_unihan_variant(0x406A, kSimplifiedVariant, 0x251E2).
unicode_unihan_variant(0x407B, kSimplifiedVariant, 0x4025).
unicode_unihan_variant(0x4080, kSemanticVariant, 0x252DF).
unicode_unihan_variant(0x40AB, kSemanticVariant, 0x592F). %<kFenn
unicode_unihan_variant(0x40B5, kTraditionalVariant, 0x78BD).
unicode_unihan_variant(0x40C9, kSemanticVariant, 0x73C9). %<kMatthews
unicode_unihan_variant(0x40D8, kSemanticVariant, 0x785C). %<kMatthews
unicode_unihan_variant(0x4103, kSemanticVariant, 0x6B83). %<kMatthews
unicode_unihan_variant(0x410D, kSemanticVariant, 0x8721). %<kMeyerWempe
unicode_unihan_variant(0x410D, kSpecializedSemanticVariant, 0x8721).
unicode_unihan_variant(0x412F, kSemanticVariant, 0x2219E). %<kFenn
unicode_unihan_variant(0x413A, kSemanticVariant, 0x7A0A). %<kMeyerWempe
unicode_unihan_variant(0x4149, kTraditionalVariant, 0x7A0F).
unicode_unihan_variant(0x416A, kTraditionalVariant, 0x258A2).
unicode_unihan_variant(0x418B, kSemanticVariant, 0x79CB). %<kMatthews
unicode_unihan_variant(0x418B, kZVariant, 0x9F9D).
unicode_unihan_variant(0x4194, kSemanticVariant, 0x7AC9). %<kMeyerWempe
unicode_unihan_variant(0x41AB, kSemanticVariant, 0x7A93). %<kMeyerWempe 0x7A97<kMeyerWempe
unicode_unihan_variant(0x41F2, kTraditionalVariant, 0x7B74).
unicode_unihan_variant(0x4207, kSemanticVariant, 0x7F69). %<kLau,kMatthews
unicode_unihan_variant(0x4259, kSimplifiedVariant, 0x25B00).
unicode_unihan_variant(0x4264, kTraditionalVariant, 0x7C54).
unicode_unihan_variant(0x426C, kSimplifiedVariant, 0x2B088).
unicode_unihan_variant(0x4272, kSimplifiedVariant, 0x25B9C).
unicode_unihan_variant(0x4275, kSemanticVariant, 0x994C). %<kMatthews 0x7C51<kMatthews
unicode_unihan_variant(0x429C, kSemanticVariant, 0x7CF0). %<kFenn
unicode_unihan_variant(0x42AD, kSimplifiedVariant, 0x25E85).
unicode_unihan_variant(0x42B7, kSimplifiedVariant, 0x4336).
unicode_unihan_variant(0x42D9, kSimplifiedVariant, 0x433A).
unicode_unihan_variant(0x42DA, kSimplifiedVariant, 0x433B).
unicode_unihan_variant(0x42F2, kSemanticVariant, 0x7E69). %<kMeyerWempe
unicode_unihan_variant(0x42FB, kSimplifiedVariant, 0x433E).
unicode_unihan_variant(0x42FF, kSimplifiedVariant, 0x26213).
unicode_unihan_variant(0x4308, kSimplifiedVariant, 0x26216).
unicode_unihan_variant(0x430B, kSimplifiedVariant, 0x26218).
unicode_unihan_variant(0x4316, kSimplifiedVariant, 0x2621C).
unicode_unihan_variant(0x431D, kSimplifiedVariant, 0x2621F).
unicode_unihan_variant(0x431F, kSimplifiedVariant, 0x2621E).
unicode_unihan_variant(0x4325, kSemanticVariant, 0x7E2F). %<kFenn
unicode_unihan_variant(0x4325, kSimplifiedVariant, 0x26220).
unicode_unihan_variant(0x432B, kSemanticVariant, 0x7E9C). %<kMeyerWempe
unicode_unihan_variant(0x4330, kSimplifiedVariant, 0x26219).
unicode_unihan_variant(0x4336, kTraditionalVariant, 0x42B7).
unicode_unihan_variant(0x4337, kTraditionalVariant, 0x7D2C).
unicode_unihan_variant(0x4338, kTraditionalVariant, 0x7E33).
unicode_unihan_variant(0x4339, kTraditionalVariant, 0x7D45).
unicode_unihan_variant(0x433A, kTraditionalVariant, 0x42D9).
unicode_unihan_variant(0x433B, kTraditionalVariant, 0x42DA).
unicode_unihan_variant(0x433C, kTraditionalVariant, 0x7D90).
unicode_unihan_variant(0x433D, kTraditionalVariant, 0x7DB5).
unicode_unihan_variant(0x433E, kTraditionalVariant, 0x42FB).
unicode_unihan_variant(0x4340, kTraditionalVariant, 0x7E7F).
unicode_unihan_variant(0x4341, kTraditionalVariant, 0x7E78).
unicode_unihan_variant(0x439B, kZVariant, 0x3588).
unicode_unihan_variant(0x43D5, kSemanticVariant, 0x98EA). %<kMatthews
unicode_unihan_variant(0x4430, kSemanticVariant, 0x9948). %<kMatthews
unicode_unihan_variant(0x4492, kSemanticVariant, 0x82D5). %<kMatthews
unicode_unihan_variant(0x44D5, kTraditionalVariant, 0x85B3).
unicode_unihan_variant(0x451B, kSemanticVariant, 0x6ABE). %<kFenn
unicode_unihan_variant(0x454C, kSemanticVariant, 0x7A1A). %<kMatthews
unicode_unihan_variant(0x4573, kSimplifiedVariant, 0x26C34).
unicode_unihan_variant(0x4588, kSemanticVariant, 0x8650). %<kHanYu
unicode_unihan_variant(0x458D, kSemanticVariant, 0x8654). %<kMatthews
unicode_unihan_variant(0x458F, kSemanticVariant, 0x51E6). %<kFenn 0x8655<kMatthews
unicode_unihan_variant(0x458F, kSpecializedSemanticVariant, 0x8655). %<kFenn
unicode_unihan_variant(0x459F, kSemanticVariant, 0x8771). %<kMatthews
unicode_unihan_variant(0x45A3, kSemanticVariant, 0x86A4). %<kLau,kMatthews
unicode_unihan_variant(0x45B5, kSemanticVariant, 0x872B). %<kMatthews
unicode_unihan_variant(0x45D6, kTraditionalVariant, 0x87AE).
unicode_unihan_variant(0x45FF, kSimplifiedVariant, 0x2725E).
unicode_unihan_variant(0x4611, kSemanticVariant, 0x8109). %<kMeyerWempe
unicode_unihan_variant(0x461A, kSemanticVariant, 0x5352). %<kLau,kMatthews
unicode_unihan_variant(0x461B, kTraditionalVariant, 0x2775E).
unicode_unihan_variant(0x461E, kTraditionalVariant, 0x27717).
unicode_unihan_variant(0x463A, kSemanticVariant, 0x7DBB). %<kFenn
unicode_unihan_variant(0x464A, kTraditionalVariant, 0x27735).
unicode_unihan_variant(0x464C, kTraditionalVariant, 0x4661).
unicode_unihan_variant(0x4653, kTraditionalVariant, 0x896C).
unicode_unihan_variant(0x465D, kSemanticVariant, 0x893B). %<kLau,kMatthews 0x893A<kMatthews
unicode_unihan_variant(0x4661, kSimplifiedVariant, 0x464C).
unicode_unihan_variant(0x468E, kSemanticVariant, 0x2517E). %<kMeyerWempe
unicode_unihan_variant(0x46E1, kSemanticVariant, 0x8A71). %<kMatthews
unicode_unihan_variant(0x46FB, kSemanticVariant, 0x8A98). %<kMatthews
unicode_unihan_variant(0x46FC, kSemanticVariant, 0x8B6D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4700, kSimplifiedVariant, 0x4727).
unicode_unihan_variant(0x470A, kSemanticVariant, 0x5608). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4723, kTraditionalVariant, 0x8A22).
unicode_unihan_variant(0x4725, kTraditionalVariant, 0x27A59).
unicode_unihan_variant(0x4727, kTraditionalVariant, 0x4700).
unicode_unihan_variant(0x4729, kTraditionalVariant, 0x8B8C).
unicode_unihan_variant(0x4736, kSemanticVariant, 0x8C47). %<kMatthews
unicode_unihan_variant(0x474B, kSemanticVariant, 0x8C75). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4759, kTraditionalVariant, 0x8C99).
unicode_unihan_variant(0x475C, kSemanticVariant, 0x72FB). %<kMeyerWempe
unicode_unihan_variant(0x4764, kSemanticVariant, 0x7360). %<kMeyerWempe
unicode_unihan_variant(0x477B, kSimplifiedVariant, 0x27E55).
unicode_unihan_variant(0x477C, kSimplifiedVariant, 0x478D).
unicode_unihan_variant(0x4788, kSimplifiedVariant, 0x27E51).
unicode_unihan_variant(0x478C, kTraditionalVariant, 0x27D73).
unicode_unihan_variant(0x478D, kTraditionalVariant, 0x477C).
unicode_unihan_variant(0x478E, kTraditionalVariant, 0x27DA7).
unicode_unihan_variant(0x4790, kTraditionalVariant, 0x8CF0).
unicode_unihan_variant(0x4793, kSemanticVariant, 0x8D6C). %<kMeyerWempe
unicode_unihan_variant(0x47E2, kTraditionalVariant, 0x8E8E).
unicode_unihan_variant(0x4831, kSemanticVariant, 0x8E85). %<kMatthews
unicode_unihan_variant(0x4880, kTraditionalVariant, 0x282B0).
unicode_unihan_variant(0x4881, kTraditionalVariant, 0x282B8).
unicode_unihan_variant(0x4882, kTraditionalVariant, 0x282E2).
unicode_unihan_variant(0x4899, kSemanticVariant, 0x6557). %<kMatthews
unicode_unihan_variant(0x48A8, kSimplifiedVariant, 0x28479).
unicode_unihan_variant(0x48E9, kSemanticVariant, 0x9187). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x490D, kSemanticVariant, 0x288A5).
unicode_unihan_variant(0x4940, kSemanticVariant, 0x6375). %<kFenn
unicode_unihan_variant(0x4947, kSimplifiedVariant, 0x4982).
unicode_unihan_variant(0x4968, kSemanticVariant, 0x9462). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4969, kSimplifiedVariant, 0x28C56).
unicode_unihan_variant(0x4971, kSimplifiedVariant, 0x497E).
unicode_unihan_variant(0x4977, kSemanticVariant, 0x350D). %<kMatthews
unicode_unihan_variant(0x497A, kTraditionalVariant, 0x91FE).
unicode_unihan_variant(0x497D, kTraditionalVariant, 0x93FA).
unicode_unihan_variant(0x497E, kTraditionalVariant, 0x4971).
unicode_unihan_variant(0x497F, kTraditionalVariant, 0x28BC5).
unicode_unihan_variant(0x4980, kTraditionalVariant, 0x289AB).
unicode_unihan_variant(0x4981, kTraditionalVariant, 0x289DC).
unicode_unihan_variant(0x4982, kTraditionalVariant, 0x4947).
unicode_unihan_variant(0x4983, kTraditionalVariant, 0x942F).
unicode_unihan_variant(0x4985, kTraditionalVariant, 0x9425).
unicode_unihan_variant(0x4993, kSemanticVariant, 0x8998). %<kMeyerWempe 0x26552<kMeyerWempe
unicode_unihan_variant(0x4998, kSimplifiedVariant, 0x28E04).
unicode_unihan_variant(0x499B, kSimplifiedVariant, 0x49B6).
unicode_unihan_variant(0x499F, kSimplifiedVariant, 0x49B7).
unicode_unihan_variant(0x49B3, kSimplifiedVariant, 0x28DFF).
unicode_unihan_variant(0x49B6, kTraditionalVariant, 0x499B).
unicode_unihan_variant(0x49B7, kTraditionalVariant, 0x499F).
unicode_unihan_variant(0x49E2, kSimplifiedVariant, 0x28E1F).
unicode_unihan_variant(0x4A18, kSemanticVariant, 0x9730). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4A5E, kSemanticVariant, 0x97C2). %<kMatthews
unicode_unihan_variant(0x4A8A, kSemanticVariant, 0x9F93). %<kMatthews
unicode_unihan_variant(0x4A8F, kSimplifiedVariant, 0x293FC).
unicode_unihan_variant(0x4A97, kSimplifiedVariant, 0x29400).
unicode_unihan_variant(0x4A98, kSimplifiedVariant, 0x293FF).
unicode_unihan_variant(0x4ABF, kSemanticVariant, 0x56DF). %<kMatthews
unicode_unihan_variant(0x4AF4, kSimplifiedVariant, 0x29597).
unicode_unihan_variant(0x4B12, kSemanticVariant, 0x98BC). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4B18, kSimplifiedVariant, 0x2966E).
unicode_unihan_variant(0x4B1D, kSimplifiedVariant, 0x2966F).
unicode_unihan_variant(0x4B1E, kSimplifiedVariant, 0x29667).
unicode_unihan_variant(0x4B21, kSemanticVariant, 0x7FE5). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4B40, kSimplifiedVariant, 0x29807).
unicode_unihan_variant(0x4B43, kSimplifiedVariant, 0x29808).
unicode_unihan_variant(0x4B6A, kTraditionalVariant, 0x297AF).
unicode_unihan_variant(0x4B7E, kSemanticVariant, 0x99B1). %<kFenn
unicode_unihan_variant(0x4B7F, kSimplifiedVariant, 0x299ED).
unicode_unihan_variant(0x4B9D, kSimplifiedVariant, 0x299F0).
unicode_unihan_variant(0x4B9E, kSimplifiedVariant, 0x29A01).
unicode_unihan_variant(0x4BA0, kSimplifiedVariant, 0x299FF).
unicode_unihan_variant(0x4BAB, kSimplifiedVariant, 0x29A07).
unicode_unihan_variant(0x4BB3, kSimplifiedVariant, 0x29A0F).
unicode_unihan_variant(0x4BBE, kSimplifiedVariant, 0x299EA).
unicode_unihan_variant(0x4BC0, kSimplifiedVariant, 0x4BC5).
unicode_unihan_variant(0x4BC3, kTraditionalVariant, 0x298D1).
unicode_unihan_variant(0x4BC4, kTraditionalVariant, 0x9A27).
unicode_unihan_variant(0x4BC5, kTraditionalVariant, 0x4BC0).
unicode_unihan_variant(0x4BCC, kSemanticVariant, 0x5C3B). %<kMatthews
unicode_unihan_variant(0x4BFB, kSemanticVariant, 0x9AFB). %<kLau
unicode_unihan_variant(0x4C17, kSemanticVariant, 0x9B2E). %<kFenn
unicode_unihan_variant(0x4C3E, kSimplifiedVariant, 0x9C83).
unicode_unihan_variant(0x4C47, kSemanticVariant, 0x9C53). %<kMatthews
unicode_unihan_variant(0x4C59, kSimplifiedVariant, 0x29F88).
unicode_unihan_variant(0x4C6C, kSimplifiedVariant, 0x29F8A).
unicode_unihan_variant(0x4C70, kSimplifiedVariant, 0x29F8B).
unicode_unihan_variant(0x4C77, kSemanticVariant, 0x6F01). %<kMatthews
unicode_unihan_variant(0x4C77, kSimplifiedVariant, 0x4CA3).
unicode_unihan_variant(0x4C7D, kSemanticVariant, 0x9BE7). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x4C7D, kSimplifiedVariant, 0x4C9D).
unicode_unihan_variant(0x4C81, kSimplifiedVariant, 0x9CDA).
unicode_unihan_variant(0x4C96, kSimplifiedVariant, 0x29F82).
unicode_unihan_variant(0x4C9D, kTraditionalVariant, 0x4C7D).
unicode_unihan_variant(0x4C9E, kTraditionalVariant, 0x29D98).
unicode_unihan_variant(0x4C9F, kTraditionalVariant, 0x9BA3).
unicode_unihan_variant(0x4CA0, kTraditionalVariant, 0x9C06).
unicode_unihan_variant(0x4CA1, kTraditionalVariant, 0x9C0C).
unicode_unihan_variant(0x4CA2, kTraditionalVariant, 0x9C27).
unicode_unihan_variant(0x4CA3, kTraditionalVariant, 0x4C77).
unicode_unihan_variant(0x4CB0, kSimplifiedVariant, 0x2A242).
unicode_unihan_variant(0x4D09, kSimplifiedVariant, 0x9E6E).
unicode_unihan_variant(0x4D13, kTraditionalVariant, 0x9CFE).
unicode_unihan_variant(0x4D14, kTraditionalVariant, 0x9D41).
unicode_unihan_variant(0x4D15, kTraditionalVariant, 0x9D37).
unicode_unihan_variant(0x4D16, kTraditionalVariant, 0x9D84).
unicode_unihan_variant(0x4D17, kTraditionalVariant, 0x9DAA).
unicode_unihan_variant(0x4D18, kTraditionalVariant, 0x9DC8).
unicode_unihan_variant(0x4D19, kTraditionalVariant, 0x9DFF).
unicode_unihan_variant(0x4D2C, kSimplifiedVariant, 0x2A388).
unicode_unihan_variant(0x4D34, kSimplifiedVariant, 0x2A38B).
unicode_unihan_variant(0x4D39, kSemanticVariant, 0x991C). %<kMatthews
unicode_unihan_variant(0x4D51, kSemanticVariant, 0x4D52). %<kMatthews
unicode_unihan_variant(0x4D52, kSemanticVariant, 0x4D51). %<kMatthews
unicode_unihan_variant(0x4D8F, kSemanticVariant, 0x6D95). %<kMatthews
unicode_unihan_variant(0x4DAE, kTraditionalVariant, 0x9F91).
unicode_unihan_variant(0x4E00, kSemanticVariant, 0x5F0C). %<kLau,kMatthews,kMeyerWempe 0x58F9<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x4E00, kSpecializedSemanticVariant, 0x58F9).
unicode_unihan_variant(0x4E03, kSemanticVariant, 0x67D2). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x4E03, kSpecializedSemanticVariant, 0x67D2). %<kFenn
unicode_unihan_variant(0x4E04, kZVariant, 0x4E0A).
unicode_unihan_variant(0x4E05, kZVariant, 0x4E0B).
unicode_unihan_variant(0x4E07, kSemanticVariant, 0x534D). %<kFenn 0x842C<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x4E07, kTraditionalVariant, 0x842C).
unicode_unihan_variant(0x4E09, kSemanticVariant, 0x53C1). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x4E09, kSpecializedSemanticVariant, 0x53C1). %<kFenn
unicode_unihan_variant(0x4E0A, kZVariant, 0x4E04).
unicode_unihan_variant(0x4E0B, kZVariant, 0x4E05).
unicode_unihan_variant(0x4E0C, kZVariant, 0x5176).
unicode_unihan_variant(0x4E0D, kZVariant, 0xF967).
unicode_unihan_variant(0x4E0E, kSemanticVariant, 0x8207). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4E0E, kTraditionalVariant, 0x8207).
unicode_unihan_variant(0x4E10, kSemanticVariant, 0x5303). %<kMatthews
unicode_unihan_variant(0x4E11, kTraditionalVariant, 0x919C).
unicode_unihan_variant(0x4E13, kTraditionalVariant, 0x5C08).
unicode_unihan_variant(0x4E15, kSpecializedSemanticVariant, 0x4EF3). %<kMeyerWempe
unicode_unihan_variant(0x4E16, kZVariant, 0x4E17).
unicode_unihan_variant(0x4E17, kZVariant, 0x4E16).
unicode_unihan_variant(0x4E18, kSemanticVariant, 0x3400). %0x5775<kMatthews
unicode_unihan_variant(0x4E18, kZVariant, 0x4E20).
unicode_unihan_variant(0x4E1A, kTraditionalVariant, 0x696D).
unicode_unihan_variant(0x4E1B, kTraditionalVariant, 0x53E2).
unicode_unihan_variant(0x4E1C, kSemanticVariant, 0x6771). %<kFenn
unicode_unihan_variant(0x4E1C, kTraditionalVariant, 0x6771).
unicode_unihan_variant(0x4E1D, kTraditionalVariant, 0x7D72).
unicode_unihan_variant(0x4E1F, kSemanticVariant, 0x4E22). %<kHKGlyph,kMatthews).
unicode_unihan_variant(0x4E1F, kSimplifiedVariant, 0x4E22).
unicode_unihan_variant(0x4E21, kSemanticVariant, 0x5169). %<kMatthews 0x4E24<kFenn
unicode_unihan_variant(0x4E21, kZVariant, 0x5169).
unicode_unihan_variant(0x4E22, kSemanticVariant, 0x4E1F). %<kHKGlyph,kMatthews).
unicode_unihan_variant(0x4E22, kTraditionalVariant, 0x4E1F).
unicode_unihan_variant(0x4E23, kZVariant, 0x9149).
unicode_unihan_variant(0x4E24, kSemanticVariant, 0x4E21). %<kFenn 0x5169<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x4E24, kTraditionalVariant, 0x5169).
unicode_unihan_variant(0x4E25, kTraditionalVariant, 0x56B4).
unicode_unihan_variant(0x4E26, kSemanticVariant, 0x5E77). %<kMatthews,kMeyerWempe 0x5E76<kMatthews,kMeyerWempe 0x7ADD<kMatthews
unicode_unihan_variant(0x4E26, kSimplifiedVariant, 0x5E76).
unicode_unihan_variant(0x4E26, kZVariant, 0x5E77).
unicode_unihan_variant(0x4E27, kTraditionalVariant, 0x55AA).
unicode_unihan_variant(0x4E2A, kSemanticVariant, 0x500B). %<kLau,kMatthews 0x7B87<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x4E2A, kSpecializedSemanticVariant, 0x500B). %<kMeyerWempe
unicode_unihan_variant(0x4E2A, kTraditionalVariant, 0x500B).
unicode_unihan_variant(0x4E2C, kZVariant, 0x723F).
unicode_unihan_variant(0x4E2D, kSpecializedSemanticVariant, 0x585A). %<kFenn
unicode_unihan_variant(0x4E30, kTraditionalVariant, 0x8C50).
unicode_unihan_variant(0x4E31, kZVariant, 0x535D).
unicode_unihan_variant(0x4E32, kZVariant, 0xF905).
unicode_unihan_variant(0x4E34, kTraditionalVariant, 0x81E8).
unicode_unihan_variant(0x4E39, kZVariant, 0xF95E).
unicode_unihan_variant(0x4E3A, kTraditionalVariant, 0x70BA).
unicode_unihan_variant(0x4E3A, kZVariant, 0x70BA).
unicode_unihan_variant(0x4E3C, kSpecializedSemanticVariant, 0x4E95).
unicode_unihan_variant(0x4E3D, kTraditionalVariant, 0x9E97).
unicode_unihan_variant(0x4E3E, kTraditionalVariant, 0x8209).
unicode_unihan_variant(0x4E43, kSemanticVariant, 0x8FFA). %<kMatthews 0x5EFC<kMatthews
unicode_unihan_variant(0x4E43, kZVariant, 0x5EFC).
unicode_unihan_variant(0x4E45, kZVariant, 0x4E46).
unicode_unihan_variant(0x4E46, kZVariant, 0x4E45).
unicode_unihan_variant(0x4E47, kZVariant, 0x8650).
unicode_unihan_variant(0x4E48, kSemanticVariant, 0x5E7A). %<kMatthews
unicode_unihan_variant(0x4E48, kTraditionalVariant, 0x5E7A). %0x9EBC 0x9EBD
unicode_unihan_variant(0x4E49, kSemanticVariant, 0x7FA9). %<kFenn
unicode_unihan_variant(0x4E49, kTraditionalVariant, 0x7FA9).
unicode_unihan_variant(0x4E4C, kTraditionalVariant, 0x70CF).
unicode_unihan_variant(0x4E50, kTraditionalVariant, 0x6A02).
unicode_unihan_variant(0x4E54, kSemanticVariant, 0x55AC). %<kFenn
unicode_unihan_variant(0x4E54, kTraditionalVariant, 0x55AC).
unicode_unihan_variant(0x4E55, kZVariant, 0x864E).
unicode_unihan_variant(0x4E57, kZVariant, 0x4E58).
unicode_unihan_variant(0x4E58, kZVariant, 0x4E57).
unicode_unihan_variant(0x4E5A, kSemanticVariant, 0x96B1). %<kMatthews
unicode_unihan_variant(0x4E5D, kSemanticVariant, 0x7396). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x4E60, kTraditionalVariant, 0x7FD2).
unicode_unihan_variant(0x4E61, kTraditionalVariant, 0x9109).
unicode_unihan_variant(0x4E66, kTraditionalVariant, 0x66F8).
unicode_unihan_variant(0x4E69, kZVariant, 0x7A3D).
unicode_unihan_variant(0x4E70, kTraditionalVariant, 0x8CB7).
unicode_unihan_variant(0x4E71, kSemanticVariant, 0x4E82). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4E71, kTraditionalVariant, 0x4E82).
unicode_unihan_variant(0x4E7E, kSimplifiedVariant, 0x5E72).
unicode_unihan_variant(0x4E7E, kSpecializedSemanticVariant, 0x4E81). %<kFenn
unicode_unihan_variant(0x4E80, kZVariant, 0x9F9C).
unicode_unihan_variant(0x4E81, kSpecializedSemanticVariant, 0x4E7E). %<kFenn
unicode_unihan_variant(0x4E81, kZVariant, 0x4E7E).
unicode_unihan_variant(0x4E82, kSemanticVariant, 0x4E71). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4E82, kSimplifiedVariant, 0x4E71).
unicode_unihan_variant(0x4E86, kZVariant, 0xF9BA).
unicode_unihan_variant(0x4E88, kSemanticVariant, 0x4F59). %<kFenn
unicode_unihan_variant(0x4E88, kZVariant, 0x8C6B).
unicode_unihan_variant(0x4E89, kSemanticVariant, 0x722D). %<kMatthews
unicode_unihan_variant(0x4E89, kTraditionalVariant, 0x722D).
unicode_unihan_variant(0x4E8A, kSemanticVariant, 0x4E8B). %<kFenn
unicode_unihan_variant(0x4E8A, kZVariant, 0x4E8B).
unicode_unihan_variant(0x4E8B, kSemanticVariant, 0x4E8A). %<kFenn
unicode_unihan_variant(0x4E8B, kZVariant, 0x4E8A).
unicode_unihan_variant(0x4E8C, kSemanticVariant, 0x5F0D). %<kMatthews,kMeyerWempe 0x8CB3<kMeyerWempe
unicode_unihan_variant(0x4E8C, kSpecializedSemanticVariant, 0x8CB3).
unicode_unihan_variant(0x4E8C, kZVariant, 0x8D30).
unicode_unihan_variant(0x4E8E, kSemanticVariant, 0x6275). %<kLau 0x65BC<kLau
unicode_unihan_variant(0x4E8E, kTraditionalVariant, 0x65BC).
unicode_unihan_variant(0x4E8E, kZVariant, 0x6275).
unicode_unihan_variant(0x4E8F, kSemanticVariant, 0x65BC). %<kMatthews 0x6275<kMatthews
unicode_unihan_variant(0x4E8F, kTraditionalVariant, 0x8667).
unicode_unihan_variant(0x4E91, kTraditionalVariant, 0x96F2).
unicode_unihan_variant(0x4E93, kZVariant, 0x5176).
unicode_unihan_variant(0x4E94, kSemanticVariant, 0x3405). %<kMatthews
unicode_unihan_variant(0x4E94, kSpecializedSemanticVariant, 0x4F0D). %<kMeyerWempe
unicode_unihan_variant(0x4E94, kZVariant, 0x4F0D).
unicode_unihan_variant(0x4E95, kSpecializedSemanticVariant, 0x4E3C).
unicode_unihan_variant(0x4E97, kSemanticVariant, 0x5C81). %<kFenn 0x6B72<kMatthews 0x21ED5<kFenn
unicode_unihan_variant(0x4E97, kZVariant, 0x6B72).
unicode_unihan_variant(0x4E98, kZVariant, 0x4E99).
unicode_unihan_variant(0x4E9A, kTraditionalVariant, 0x4E9E).
unicode_unihan_variant(0x4E9C, kZVariant, 0x4E9E).
unicode_unihan_variant(0x4E9D, kSemanticVariant, 0x9F4A). %<kMatthews
unicode_unihan_variant(0x4E9D, kZVariant, 0x9F4A).
unicode_unihan_variant(0x4E9E, kSimplifiedVariant, 0x4E9A).
unicode_unihan_variant(0x4E9E, kZVariant, 0x4E9C).
unicode_unihan_variant(0x4EA1, kSemanticVariant, 0x5166). %<kLau,kMatthews 0x4EBE<kFenn
unicode_unihan_variant(0x4EA1, kZVariant, 0x4EBE).
unicode_unihan_variant(0x4EA7, kTraditionalVariant, 0x7522).
unicode_unihan_variant(0x4EA7, kZVariant, 0x7522).
unicode_unihan_variant(0x4EA9, kTraditionalVariant, 0x755D).
unicode_unihan_variant(0x4EAC, kZVariant, 0x4EB0).
unicode_unihan_variant(0x4EAE, kZVariant, 0xF977).
unicode_unihan_variant(0x4EAF, kZVariant, 0x4EAB).
unicode_unihan_variant(0x4EB0, kZVariant, 0x4EAC).
unicode_unihan_variant(0x4EB1, kZVariant, 0x591C).
unicode_unihan_variant(0x4EB2, kSemanticVariant, 0x699B). %<kMatthews
unicode_unihan_variant(0x4EB2, kTraditionalVariant, 0x89AA).
unicode_unihan_variant(0x4EB5, kTraditionalVariant, 0x893B).
unicode_unihan_variant(0x4EB7, kZVariant, 0x5EC9).
unicode_unihan_variant(0x4EB8, kTraditionalVariant, 0x56B2).
unicode_unihan_variant(0x4EBA, kSemanticVariant, 0x4EBB). %<kMatthews
unicode_unihan_variant(0x4EBB, kSemanticVariant, 0x4EBA). %<kMatthews
unicode_unihan_variant(0x4EBC, kSemanticVariant, 0x96C6). %<kLau,kMatthews
unicode_unihan_variant(0x4EBE, kSemanticVariant, 0x4EA1). %<kFenn
unicode_unihan_variant(0x4EBF, kTraditionalVariant, 0x5104).
unicode_unihan_variant(0x4EC0, kZVariant, 0xF9FD).
unicode_unihan_variant(0x4EC1, kZVariant, 0x5FC8).
unicode_unihan_variant(0x4EC2, kZVariant, 0x50CD).
unicode_unihan_variant(0x4EC3, kZVariant, 0x505C).
unicode_unihan_variant(0x4EC5, kTraditionalVariant, 0x50C5).
unicode_unihan_variant(0x4EC6, kTraditionalVariant, 0x50D5).
unicode_unihan_variant(0x4EC7, kSemanticVariant, 0x8B8E). %<kMeyerWempe 0x8B90<kLau,kMeyerWempe
unicode_unihan_variant(0x4EC7, kSpecializedSemanticVariant, 0x8B8E). %<kFenn 0x8B90<kFenn
unicode_unihan_variant(0x4EC7, kZVariant, 0x8B90).
unicode_unihan_variant(0x4ECE, kSemanticVariant, 0x5F9E). %<kMatthews
unicode_unihan_variant(0x4ECE, kSpecializedSemanticVariant, 0x5F9E). %<kFenn
unicode_unihan_variant(0x4ECE, kTraditionalVariant, 0x5F9E).
unicode_unihan_variant(0x4ECF, kZVariant, 0x4F5B).
unicode_unihan_variant(0x4ED1, kTraditionalVariant, 0x4F96).
unicode_unihan_variant(0x4ED3, kTraditionalVariant, 0x5009).
unicode_unihan_variant(0x4ED6, kSemanticVariant, 0x5979). %<kMatthews 0x7260<kMatthews
unicode_unihan_variant(0x4ED9, kSemanticVariant, 0x50CA). %<kLau,kMatthews
unicode_unihan_variant(0x4ED9, kSpecializedSemanticVariant, 0x50CA).
unicode_unihan_variant(0x4ED9, kZVariant, 0x50CA).
unicode_unihan_variant(0x4EDD, kSemanticVariant, 0x540C). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x4EDD, kZVariant, 0x540C).
unicode_unihan_variant(0x4EDE, kZVariant, 0x4EED).
unicode_unihan_variant(0x4EDF, kSemanticVariant, 0x5343). %<kMeyerWempe 0x9621<kFenn
unicode_unihan_variant(0x4EDF, kSpecializedSemanticVariant, 0x5343).
unicode_unihan_variant(0x4EDF, kZVariant, 0x5343).
unicode_unihan_variant(0x4EE4, kZVariant, 0xF9A8).
unicode_unihan_variant(0x4EE5, kSemanticVariant, 0x382F). %<kFenn
unicode_unihan_variant(0x4EEA, kTraditionalVariant, 0x5100).
unicode_unihan_variant(0x4EEC, kTraditionalVariant, 0x5011).
unicode_unihan_variant(0x4EED, kZVariant, 0x4EDE).
unicode_unihan_variant(0x4EEE, kZVariant, 0x5047).
unicode_unihan_variant(0x4EF3, kSpecializedSemanticVariant, 0x4E15). %<kMeyerWempe
unicode_unihan_variant(0x4EF7, kTraditionalVariant, 0x50F9).
unicode_unihan_variant(0x4EFB, kSemanticVariant, 0x4EFC). %<kHKGlyph
unicode_unihan_variant(0x4EFB, kZVariant, 0x4EFC). %<kHKGlyph
unicode_unihan_variant(0x4EFC, kSemanticVariant, 0x4EFB). %<kHKGlyph
unicode_unihan_variant(0x4EFC, kZVariant, 0x4EFB). %<kHKGlyph
unicode_unihan_variant(0x4EFD, kSpecializedSemanticVariant, 0x5206). %<kFenn
unicode_unihan_variant(0x4EFD, kZVariant, 0x5F6C).
unicode_unihan_variant(0x4EFE, kSemanticVariant, 0x4F4E). %<kMatthews
unicode_unihan_variant(0x4EFF, kSemanticVariant, 0x5023). %<kMatthews
unicode_unihan_variant(0x4EFF, kZVariant, 0x5023).
unicode_unihan_variant(0x4F00, kSemanticVariant, 0x5F78). %<kMatthews
unicode_unihan_variant(0x4F03, kSemanticVariant, 0x59A4). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4F0D, kSpecializedSemanticVariant, 0x4E94). %<kMeyerWempe
unicode_unihan_variant(0x4F0D, kZVariant, 0x4E94).
unicode_unihan_variant(0x4F17, kTraditionalVariant, 0x773E).
unicode_unihan_variant(0x4F18, kTraditionalVariant, 0x512A).
unicode_unihan_variant(0x4F19, kZVariant, 0x5925).
unicode_unihan_variant(0x4F1A, kSemanticVariant, 0x6703). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4F1A, kTraditionalVariant, 0x6703).
unicode_unihan_variant(0x4F1B, kTraditionalVariant, 0x50B4).
unicode_unihan_variant(0x4F1C, kZVariant, 0x5005).
unicode_unihan_variant(0x4F1D, kZVariant, 0x50B3).
unicode_unihan_variant(0x4F1E, kTraditionalVariant, 0x5098).
unicode_unihan_variant(0x4F1F, kTraditionalVariant, 0x5049).
unicode_unihan_variant(0x4F20, kTraditionalVariant, 0x50B3).
unicode_unihan_variant(0x4F21, kTraditionalVariant, 0x4FE5).
unicode_unihan_variant(0x4F23, kTraditionalVariant, 0x4FD4).
unicode_unihan_variant(0x4F24, kTraditionalVariant, 0x50B7).
unicode_unihan_variant(0x4F25, kTraditionalVariant, 0x5000).
unicode_unihan_variant(0x4F26, kTraditionalVariant, 0x502B).
unicode_unihan_variant(0x4F27, kTraditionalVariant, 0x5096).
unicode_unihan_variant(0x4F29, kZVariant, 0x4FE1).
unicode_unihan_variant(0x4F2A, kTraditionalVariant, 0x507D). %0x50DE
unicode_unihan_variant(0x4F2B, kTraditionalVariant, 0x4F47).
unicode_unihan_variant(0x4F2D, kSemanticVariant, 0x7384). %<kMatthews
unicode_unihan_variant(0x4F31, kSemanticVariant, 0x4F60). %<kMatthews
unicode_unihan_variant(0x4F32, kZVariant, 0x4F60).
unicode_unihan_variant(0x4F37, kSemanticVariant, 0x5191). %<kFenn 0x80C4<kMatthews
unicode_unihan_variant(0x4F37, kSpecializedSemanticVariant, 0x80C4). %<kMeyerWempe
unicode_unihan_variant(0x4F47, kSemanticVariant, 0x7ADA). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4F47, kSimplifiedVariant, 0x4F2B).
unicode_unihan_variant(0x4F48, kZVariant, 0x5E03).
unicode_unihan_variant(0x4F4B, kSemanticVariant, 0x7D39). %<kMatthews
unicode_unihan_variant(0x4F4E, kSemanticVariant, 0x4EFE). %<kMatthews
unicode_unihan_variant(0x4F51, kSemanticVariant, 0x7950). %<kFenn
unicode_unihan_variant(0x4F53, kSemanticVariant, 0x9AD4). %<kMatthews 0x8EC6<kFenn
unicode_unihan_variant(0x4F53, kTraditionalVariant, 0x9AD4).
unicode_unihan_variant(0x4F54, kSpecializedSemanticVariant, 0x5360). %<kFenn
unicode_unihan_variant(0x4F54, kZVariant, 0x5360).
unicode_unihan_variant(0x4F57, kSemanticVariant, 0x5B83). %<kMatthews
unicode_unihan_variant(0x4F59, kSemanticVariant, 0x4E88). %<kFenn
unicode_unihan_variant(0x4F59, kSimplifiedVariant, 0x4F59).
unicode_unihan_variant(0x4F59, kTraditionalVariant, 0x4F59). %0x9918
unicode_unihan_variant(0x4F5B, kZVariant, 0x4ECF).
unicode_unihan_variant(0x4F5E, kZVariant, 0x4FAB).
unicode_unihan_variant(0x4F60, kSemanticVariant, 0x4F31). %<kMatthews
unicode_unihan_variant(0x4F60, kZVariant, 0x59B3).
unicode_unihan_variant(0x4F63, kTraditionalVariant, 0x50AD).
unicode_unihan_variant(0x4F65, kTraditionalVariant, 0x50C9).
unicode_unihan_variant(0x4F6C, kSpecializedSemanticVariant, 0x59C6). %<kFenn
unicode_unihan_variant(0x4F70, kSemanticVariant, 0x767E). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4F70, kZVariant, 0x767E).
unicode_unihan_variant(0x4F75, kSemanticVariant, 0x202A7). %<kFenn
unicode_unihan_variant(0x4F75, kSimplifiedVariant, 0x5E76).
unicode_unihan_variant(0x4F75, kZVariant, 0x5002).
unicode_unihan_variant(0x4F81, kSemanticVariant, 0x99EA). %<kMatthews,kMeyerWempe 0x8A75<kMeyerWempe
unicode_unihan_variant(0x4F82, kSemanticVariant, 0x8A17). %<kMatthews
unicode_unihan_variant(0x4F84, kZVariant, 0x59EA).
unicode_unihan_variant(0x4F85, kSemanticVariant, 0x8CC5). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4F86, kSemanticVariant, 0x6765). %<kMeyerWempe
unicode_unihan_variant(0x4F86, kSimplifiedVariant, 0x6765).
unicode_unihan_variant(0x4F86, kSpecializedSemanticVariant, 0x8012). %<kMeyerWempe
unicode_unihan_variant(0x4F86, kZVariant, 0xF92D).
unicode_unihan_variant(0x4F89, kZVariant, 0x8A87).
unicode_unihan_variant(0x4F8E, kSemanticVariant, 0x6549). %<kMatthews
unicode_unihan_variant(0x4F96, kSemanticVariant, 0x5D19). %<kFenn
unicode_unihan_variant(0x4F96, kSimplifiedVariant, 0x4ED1).
unicode_unihan_variant(0x4F9A, kSemanticVariant, 0x6B89). %<kMatthews 0x72E5<kMatthews
unicode_unihan_variant(0x4FA0, kTraditionalVariant, 0x4FE0).
unicode_unihan_variant(0x4FA1, kZVariant, 0x50F9).
unicode_unihan_variant(0x4FA3, kSemanticVariant, 0x4FB6). %<kMatthews
unicode_unihan_variant(0x4FA3, kTraditionalVariant, 0x4FB6).
unicode_unihan_variant(0x4FA5, kTraditionalVariant, 0x50E5).
unicode_unihan_variant(0x4FA6, kTraditionalVariant, 0x5075).
unicode_unihan_variant(0x4FA7, kTraditionalVariant, 0x5074).
unicode_unihan_variant(0x4FA8, kTraditionalVariant, 0x50D1).
unicode_unihan_variant(0x4FA9, kTraditionalVariant, 0x5108).
unicode_unihan_variant(0x4FAA, kTraditionalVariant, 0x5115).
unicode_unihan_variant(0x4FAB, kZVariant, 0x4F5E).
unicode_unihan_variant(0x4FAC, kTraditionalVariant, 0x5102).
unicode_unihan_variant(0x4FAD, kZVariant, 0x5118).
unicode_unihan_variant(0x4FAE, kSemanticVariant, 0x3444). %<kMatthews
unicode_unihan_variant(0x4FB6, kSemanticVariant, 0x4FA3). %<kMatthews
unicode_unihan_variant(0x4FB6, kSimplifiedVariant, 0x4FA3).
unicode_unihan_variant(0x4FBF, kZVariant, 0xF965).
unicode_unihan_variant(0x4FC1, kSimplifiedVariant, 0x4FE3).
unicode_unihan_variant(0x4FC2, kSimplifiedVariant, 0x7CFB).
unicode_unihan_variant(0x4FC8, kSemanticVariant, 0x56B3). %<kMatthews
unicode_unihan_variant(0x4FCA, kZVariant, 0x5101).
unicode_unihan_variant(0x4FCE, kZVariant, 0x723C).
unicode_unihan_variant(0x4FD3, kSemanticVariant, 0x5F91). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4FD3, kSpecializedSemanticVariant, 0x52C1). %<kMeyerWempe
unicode_unihan_variant(0x4FD4, kSimplifiedVariant, 0x4F23).
unicode_unihan_variant(0x4FDE, kSemanticVariant, 0x516A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4FDE, kZVariant, 0x516A).
unicode_unihan_variant(0x4FDF, kSemanticVariant, 0x7AE2). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4FDF, kZVariant, 0x7AE2).
unicode_unihan_variant(0x4FE0, kSimplifiedVariant, 0x4FA0).
unicode_unihan_variant(0x4FE1, kZVariant, 0x4F29).
unicode_unihan_variant(0x4FE3, kTraditionalVariant, 0x4FC1).
unicode_unihan_variant(0x4FE5, kSimplifiedVariant, 0x4F21).
unicode_unihan_variant(0x4FE5, kZVariant, 0x4F21).
unicode_unihan_variant(0x4FE6, kTraditionalVariant, 0x5114).
unicode_unihan_variant(0x4FE8, kTraditionalVariant, 0x513C).
unicode_unihan_variant(0x4FE9, kTraditionalVariant, 0x5006).
unicode_unihan_variant(0x4FEA, kTraditionalVariant, 0x5137).
unicode_unihan_variant(0x4FEB, kTraditionalVariant, 0x5008).
unicode_unihan_variant(0x4FED, kTraditionalVariant, 0x5109).
unicode_unihan_variant(0x4FEF, kSemanticVariant, 0x982B). %<kMatthews
unicode_unihan_variant(0x4FF1, kZVariant, 0x5036).
unicode_unihan_variant(0x4FF2, kZVariant, 0x6548).
unicode_unihan_variant(0x4FFB, kSemanticVariant, 0x5099). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x4FFB, kZVariant, 0x5099).
unicode_unihan_variant(0x5000, kSimplifiedVariant, 0x4F25).
unicode_unihan_variant(0x5002, kZVariant, 0x4F75).
unicode_unihan_variant(0x5005, kZVariant, 0x4F1C).
unicode_unihan_variant(0x5006, kSimplifiedVariant, 0x4FE9).
unicode_unihan_variant(0x5008, kSemanticVariant, 0x5FA0). %<kLau,kMatthews 0x52D1<kLau
unicode_unihan_variant(0x5008, kSimplifiedVariant, 0x4FEB).
unicode_unihan_variant(0x5009, kSimplifiedVariant, 0x4ED3).
unicode_unihan_variant(0x500B, kSemanticVariant, 0x4E2A). %<kLau,kMatthews 0x7B87<kLau,kMatthews
unicode_unihan_variant(0x500B, kSimplifiedVariant, 0x4E2A).
unicode_unihan_variant(0x500B, kSpecializedSemanticVariant, 0x4E2A). %<kMeyerWempe
unicode_unihan_variant(0x500F, kSemanticVariant, 0x5010). %<kFenn
unicode_unihan_variant(0x5010, kSemanticVariant, 0x500F). %<kFenn
unicode_unihan_variant(0x5011, kSimplifiedVariant, 0x4EEC).
unicode_unihan_variant(0x5016, kSemanticVariant, 0x5E78). %<kFenn
unicode_unihan_variant(0x5016, kSpecializedSemanticVariant, 0x56DF). %<kFenn
unicode_unihan_variant(0x5016, kZVariant, 0x5E78).
unicode_unihan_variant(0x5018, kSemanticVariant, 0x513B). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x501F, kZVariant, 0x85C9).
unicode_unihan_variant(0x5023, kSemanticVariant, 0x4EFF). %<kMatthews
unicode_unihan_variant(0x5023, kZVariant, 0x4EFF).
unicode_unihan_variant(0x5024, kSemanticVariant, 0x503C). %<kLau,kMatthews
unicode_unihan_variant(0x5024, kZVariant, 0x503C).
unicode_unihan_variant(0x502B, kSimplifiedVariant, 0x4F26).
unicode_unihan_variant(0x502B, kZVariant, 0xF9D4).
unicode_unihan_variant(0x5032, kSimplifiedVariant, 0x3448).
unicode_unihan_variant(0x5036, kZVariant, 0x4FF1).
unicode_unihan_variant(0x5039, kZVariant, 0x5109).
unicode_unihan_variant(0x503A, kTraditionalVariant, 0x50B5).
unicode_unihan_variant(0x503C, kSemanticVariant, 0x5024). %<kLau,kMatthews
unicode_unihan_variant(0x503C, kZVariant, 0x5024).
unicode_unihan_variant(0x503E, kTraditionalVariant, 0x50BE).
unicode_unihan_variant(0x5041, kSemanticVariant, 0x7A6A). %<kMatthews 0x7A31<kMatthews
unicode_unihan_variant(0x5043, kSemanticVariant, 0x3AC3). %<kMatthews
unicode_unihan_variant(0x5047, kZVariant, 0x4EEE).
unicode_unihan_variant(0x5049, kSimplifiedVariant, 0x4F1F).
unicode_unihan_variant(0x504A, kSemanticVariant, 0x8E3D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x504E, kSpecializedSemanticVariant, 0x6E28). %<kMeyerWempe
unicode_unihan_variant(0x5050, kSemanticVariant, 0x8D0B). %<kMatthews 0x8D17<kMatthews
unicode_unihan_variant(0x5051, kSimplifiedVariant, 0x343D).
unicode_unihan_variant(0x5052, kSemanticVariant, 0x8569).
unicode_unihan_variant(0x505C, kZVariant, 0x4EC3).
unicode_unihan_variant(0x5065, kSemanticVariant, 0x5FA4). %<kFenn
unicode_unihan_variant(0x506A, kSemanticVariant, 0x903C). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x506C, kTraditionalVariant, 0x50AF).
unicode_unihan_variant(0x5074, kSimplifiedVariant, 0x4FA7).
unicode_unihan_variant(0x5075, kSimplifiedVariant, 0x4FA6).
unicode_unihan_variant(0x5077, kSemanticVariant, 0x5078). %<kLau
unicode_unihan_variant(0x5077, kZVariant, 0x5078).
unicode_unihan_variant(0x5078, kSemanticVariant, 0x5077). %<kLau
unicode_unihan_variant(0x5078, kZVariant, 0x5077).
unicode_unihan_variant(0x5079, kZVariant, 0x5099).
unicode_unihan_variant(0x507A, kSemanticVariant, 0x661D). %<kMatthews 0x54B1<kFenn
unicode_unihan_variant(0x507A, kSpecializedSemanticVariant, 0x5592). %<kFenn
unicode_unihan_variant(0x507B, kTraditionalVariant, 0x50C2).
unicode_unihan_variant(0x507D, kSimplifiedVariant, 0x4F2A).
unicode_unihan_variant(0x507D, kZVariant, 0x50DE).
unicode_unihan_variant(0x507E, kTraditionalVariant, 0x50E8).
unicode_unihan_variant(0x507F, kTraditionalVariant, 0x511F).
unicode_unihan_variant(0x5081, kSemanticVariant, 0x53DF). %<kMatthews,kMeyerWempe 0x53DC<kMatthews
unicode_unihan_variant(0x5081, kSpecializedSemanticVariant, 0x58B0). %<kMeyerWempe
unicode_unihan_variant(0x508C, kSemanticVariant, 0x7F75). %<kMatthews
unicode_unihan_variant(0x508D, kSpecializedSemanticVariant, 0x65C1). %<kFenn
unicode_unihan_variant(0x508F, kSemanticVariant, 0x642A). %<kMatthews
unicode_unihan_variant(0x5091, kSemanticVariant, 0x6770). %<kMatthews
unicode_unihan_variant(0x5091, kSimplifiedVariant, 0x6770).
unicode_unihan_variant(0x5096, kSimplifiedVariant, 0x4F27).
unicode_unihan_variant(0x5098, kSemanticVariant, 0x7E56). %<kMatthews
unicode_unihan_variant(0x5098, kSimplifiedVariant, 0x4F1E).
unicode_unihan_variant(0x5099, kSemanticVariant, 0x4FFB). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5099, kSimplifiedVariant, 0x5907).
unicode_unihan_variant(0x509C, kSemanticVariant, 0x5FAD). %<kMatthews
unicode_unihan_variant(0x50A2, kZVariant, 0x5BB6).
unicode_unihan_variant(0x50A4, kZVariant, 0x510E).
unicode_unihan_variant(0x50A5, kTraditionalVariant, 0x513B).
unicode_unihan_variant(0x50A7, kTraditionalVariant, 0x5110).
unicode_unihan_variant(0x50A8, kTraditionalVariant, 0x5132).
unicode_unihan_variant(0x50A9, kTraditionalVariant, 0x513A).
unicode_unihan_variant(0x50AD, kSimplifiedVariant, 0x4F63).
unicode_unihan_variant(0x50AF, kSimplifiedVariant, 0x506C).
unicode_unihan_variant(0x50B3, kSimplifiedVariant, 0x4F20).
unicode_unihan_variant(0x50B4, kSimplifiedVariant, 0x4F1B).
unicode_unihan_variant(0x50B5, kSimplifiedVariant, 0x503A).
unicode_unihan_variant(0x50B7, kSimplifiedVariant, 0x4F24).
unicode_unihan_variant(0x50BB, kSemanticVariant, 0x510D). %<kFenn
unicode_unihan_variant(0x50BD, kSemanticVariant, 0x615E). %<kMeyerWempe
unicode_unihan_variant(0x50BE, kSimplifiedVariant, 0x503E).
unicode_unihan_variant(0x50C2, kSimplifiedVariant, 0x507B).
unicode_unihan_variant(0x50C5, kSimplifiedVariant, 0x4EC5).
unicode_unihan_variant(0x50C9, kSimplifiedVariant, 0x4F65).
unicode_unihan_variant(0x50CA, kSemanticVariant, 0x4ED9). %<kLau,kMatthews
unicode_unihan_variant(0x50CA, kSpecializedSemanticVariant, 0x4ED9).
unicode_unihan_variant(0x50CA, kZVariant, 0x4ED9).
unicode_unihan_variant(0x50CD, kZVariant, 0x4EC2).
unicode_unihan_variant(0x50CF, kZVariant, 0x8C61).
unicode_unihan_variant(0x50D1, kSimplifiedVariant, 0x4FA8).
unicode_unihan_variant(0x50D5, kSimplifiedVariant, 0x4EC6).
unicode_unihan_variant(0x50DA, kZVariant, 0xF9BB).
unicode_unihan_variant(0x50DE, kSimplifiedVariant, 0x4F2A).
unicode_unihan_variant(0x50DE, kZVariant, 0x507D).
unicode_unihan_variant(0x50E3, kZVariant, 0x50ED).
unicode_unihan_variant(0x50E5, kSimplifiedVariant, 0x4FA5).
unicode_unihan_variant(0x50E5, kZVariant, 0x5FBA).
unicode_unihan_variant(0x50E8, kSimplifiedVariant, 0x507E).
unicode_unihan_variant(0x50ED, kZVariant, 0x50E3).
unicode_unihan_variant(0x50F1, kSemanticVariant, 0x96C7). %<kLau,kMeyerWempe
unicode_unihan_variant(0x50F9, kSimplifiedVariant, 0x4EF7).
unicode_unihan_variant(0x5100, kSimplifiedVariant, 0x4EEA).
unicode_unihan_variant(0x5101, kZVariant, 0x4FCA).
unicode_unihan_variant(0x5102, kSimplifiedVariant, 0x4FAC).
unicode_unihan_variant(0x5104, kSimplifiedVariant, 0x4EBF).
unicode_unihan_variant(0x5106, kSemanticVariant, 0x8B66). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5108, kSimplifiedVariant, 0x4FA9).
unicode_unihan_variant(0x5109, kSimplifiedVariant, 0x4FED).
unicode_unihan_variant(0x510C, kZVariant, 0x50E5).
unicode_unihan_variant(0x510D, kSemanticVariant, 0x50BB).
unicode_unihan_variant(0x510E, kSpecializedSemanticVariant, 0x8F09). %<kFenn
unicode_unihan_variant(0x510E, kZVariant, 0x50A4).
unicode_unihan_variant(0x5110, kSimplifiedVariant, 0x50A7).
unicode_unihan_variant(0x5113, kSemanticVariant, 0x6AAF). %<kLau 0x67B1<kLau
unicode_unihan_variant(0x5114, kSimplifiedVariant, 0x4FE6).
unicode_unihan_variant(0x5115, kSimplifiedVariant, 0x4FAA).
unicode_unihan_variant(0x5118, kSimplifiedVariant, 0x5C3D).
unicode_unihan_variant(0x511B, kSemanticVariant, 0x3487). %<kMatthews
unicode_unihan_variant(0x511F, kSimplifiedVariant, 0x507F).
unicode_unihan_variant(0x5121, kSpecializedSemanticVariant, 0x9179). %<kMeyerWempe
unicode_unihan_variant(0x5123, kSimplifiedVariant, 0x201B2).
unicode_unihan_variant(0x512A, kSimplifiedVariant, 0x4F18).
unicode_unihan_variant(0x512D, kSemanticVariant, 0x896F). %<kMatthews
unicode_unihan_variant(0x5132, kSimplifiedVariant, 0x50A8).
unicode_unihan_variant(0x5136, kSemanticVariant, 0x349E). %<kMatthews
unicode_unihan_variant(0x5137, kSimplifiedVariant, 0x4FEA).
unicode_unihan_variant(0x5138, kSimplifiedVariant, 0x3469).
unicode_unihan_variant(0x513A, kSimplifiedVariant, 0x50A9).
unicode_unihan_variant(0x513B, kSemanticVariant, 0x5018). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x513B, kSimplifiedVariant, 0x50A5).
unicode_unihan_variant(0x513C, kSimplifiedVariant, 0x4FE8).
unicode_unihan_variant(0x513F, kTraditionalVariant, 0x5152).
unicode_unihan_variant(0x5140, kZVariant, 0xFA0C).
unicode_unihan_variant(0x5147, kZVariant, 0x51F6).
unicode_unihan_variant(0x5149, kZVariant, 0x706E).
unicode_unihan_variant(0x514B, kTraditionalVariant, 0x524B).
unicode_unihan_variant(0x514C, kSemanticVariant, 0x5151). %<kMatthews
unicode_unihan_variant(0x514C, kSimplifiedVariant, 0x5151).
unicode_unihan_variant(0x514E, kSemanticVariant, 0x5154). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x514E, kZVariant, 0x5154).
unicode_unihan_variant(0x5150, kZVariant, 0x5152).
unicode_unihan_variant(0x5151, kSemanticVariant, 0x514C). %<kMatthews
unicode_unihan_variant(0x5151, kTraditionalVariant, 0x514C).
unicode_unihan_variant(0x5152, kSimplifiedVariant, 0x513F).
unicode_unihan_variant(0x5154, kSemanticVariant, 0x514E). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5154, kZVariant, 0x514E).
unicode_unihan_variant(0x5156, kTraditionalVariant, 0x5157).
unicode_unihan_variant(0x5157, kSimplifiedVariant, 0x5156).
unicode_unihan_variant(0x515A, kTraditionalVariant, 0x9EE8).
unicode_unihan_variant(0x5166, kSemanticVariant, 0x4EA1). %<kLau,kMatthews
unicode_unihan_variant(0x5166, kZVariant, 0x4EA1).
unicode_unihan_variant(0x5167, kSimplifiedVariant, 0x5185).
unicode_unihan_variant(0x5168, kSemanticVariant, 0x34B0). %<kMatthews
unicode_unihan_variant(0x5169, kSemanticVariant, 0x4E21). %<kMatthews 0x4E24<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5169, kSimplifiedVariant, 0x4E24).
unicode_unihan_variant(0x5169, kZVariant, 0x4E21).
unicode_unihan_variant(0x516A, kSemanticVariant, 0x4FDE). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x516B, kSemanticVariant, 0x634C). %<kLau,kMeyerWempe
unicode_unihan_variant(0x516B, kSpecializedSemanticVariant, 0x634C).
unicode_unihan_variant(0x516D, kSpecializedSemanticVariant, 0x9678).
unicode_unihan_variant(0x516D, kZVariant, 0xF9D1).
unicode_unihan_variant(0x516E, kSemanticVariant, 0x20503). %<kFenn
unicode_unihan_variant(0x5170, kTraditionalVariant, 0x862D).
unicode_unihan_variant(0x5173, kTraditionalVariant, 0x95DC).
unicode_unihan_variant(0x5174, kTraditionalVariant, 0x8208).
unicode_unihan_variant(0x5176, kZVariant, 0x4E0C).
unicode_unihan_variant(0x5179, kSemanticVariant, 0x7386). %<kMatthews 0x8332<kMatthews
unicode_unihan_variant(0x5179, kTraditionalVariant, 0x8332).
unicode_unihan_variant(0x517B, kTraditionalVariant, 0x990A).
unicode_unihan_variant(0x517D, kTraditionalVariant, 0x7378).
unicode_unihan_variant(0x517F, kZVariant, 0x85DD).
unicode_unihan_variant(0x5181, kTraditionalVariant, 0x56C5).
unicode_unihan_variant(0x5182, kSemanticVariant, 0x5770). %<kMatthews 0x518B<kMatthews
unicode_unihan_variant(0x5184, kSemanticVariant, 0x5189). %<kMatthews
unicode_unihan_variant(0x5185, kTraditionalVariant, 0x5167).
unicode_unihan_variant(0x5186, kZVariant, 0x5713).
unicode_unihan_variant(0x5188, kTraditionalVariant, 0x5CA1).
unicode_unihan_variant(0x5189, kSemanticVariant, 0x5184). %<kMatthews
unicode_unihan_variant(0x518A, kSimplifiedVariant, 0x518C).
unicode_unihan_variant(0x518B, kSemanticVariant, 0x5182). %<kMatthews 0x5770<kMatthews,kMeyerWempe
unicode_unihan_variant(0x518C, kTraditionalVariant, 0x518A).
unicode_unihan_variant(0x5190, kZVariant, 0x5192).
unicode_unihan_variant(0x5191, kSemanticVariant, 0x4F37). %<kFenn 0x80C4<kFenn
unicode_unihan_variant(0x5191, kZVariant, 0x80C4).
unicode_unihan_variant(0x5192, kSemanticVariant, 0x2F8D2). %<kHKGlyph
unicode_unihan_variant(0x5197, kSemanticVariant, 0x5B82). %<kMatthews
unicode_unihan_variant(0x5197, kZVariant, 0x5B82).
unicode_unihan_variant(0x5199, kTraditionalVariant, 0x5BEB).
unicode_unihan_variant(0x519B, kTraditionalVariant, 0x8ECD).
unicode_unihan_variant(0x519C, kTraditionalVariant, 0x8FB2).
unicode_unihan_variant(0x519D, kZVariant, 0x5B9C).
unicode_unihan_variant(0x51A2, kZVariant, 0x585A).
unicode_unihan_variant(0x51A4, kSemanticVariant, 0x5BC3). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x51A4, kZVariant, 0x5BC3).
unicode_unihan_variant(0x51A6, kZVariant, 0x5BC7).
unicode_unihan_variant(0x51A8, kZVariant, 0x5BCC).
unicode_unihan_variant(0x51A9, kSemanticVariant, 0x5BEB). %<kLau,kMatthews
unicode_unihan_variant(0x51AA, kSemanticVariant, 0x7F83). %<kMatthews 0x9F0F<kMeyerWempe
unicode_unihan_variant(0x51AA, kSimplifiedVariant, 0x5E42).
unicode_unihan_variant(0x51AA, kZVariant, 0x7F83).
unicode_unihan_variant(0x51AB, kSemanticVariant, 0x6C37). %<kMatthews 0x51B0<kMatthews
unicode_unihan_variant(0x51AC, kZVariant, 0x9F15).
unicode_unihan_variant(0x51AF, kTraditionalVariant, 0x99AE).
unicode_unihan_variant(0x51B0, kSemanticVariant, 0x51AB). %<kMatthews 0x6C37<kLau,kMatthews
unicode_unihan_variant(0x51B0, kZVariant, 0x6C37).
unicode_unihan_variant(0x51B1, kSemanticVariant, 0x6C8D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x51B1, kZVariant, 0x6C8D).
unicode_unihan_variant(0x51B2, kSemanticVariant, 0x6C96). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x51B2, kTraditionalVariant, 0x6C96). %0x885D
unicode_unihan_variant(0x51B2, kZVariant, 0x6C96).
unicode_unihan_variant(0x51B3, kSemanticVariant, 0x6C7A). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x51B3, kTraditionalVariant, 0x6C7A).
unicode_unihan_variant(0x51B5, kSemanticVariant, 0x6CC1). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x51B5, kTraditionalVariant, 0x6CC1).
unicode_unihan_variant(0x51B7, kZVariant, 0xF92E).
unicode_unihan_variant(0x51BB, kTraditionalVariant, 0x51CD).
unicode_unihan_variant(0x51BD, kZVariant, 0x6D0C).
unicode_unihan_variant(0x51C0, kTraditionalVariant, 0x51C8).
unicode_unihan_variant(0x51C0, kZVariant, 0x6DE8).
unicode_unihan_variant(0x51C2, kSemanticVariant, 0x6D7C). %<kMatthews
unicode_unihan_variant(0x51C3, kZVariant, 0x5857).
unicode_unihan_variant(0x51C4, kSemanticVariant, 0x6DD2). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x51C4, kZVariant, 0x6DD2).
unicode_unihan_variant(0x51C5, kZVariant, 0x6DB8).
unicode_unihan_variant(0x51C6, kSemanticVariant, 0x6E96). %<kMeyerWempe
unicode_unihan_variant(0x51C6, kTraditionalVariant, 0x6E96).
unicode_unihan_variant(0x51C8, kSemanticVariant, 0x6DE8). %<kLau,kMatthews
unicode_unihan_variant(0x51C8, kSimplifiedVariant, 0x51C0).
unicode_unihan_variant(0x51C8, kSpecializedSemanticVariant, 0x6DE8). %<kMeyerWempe
unicode_unihan_variant(0x51C9, kSemanticVariant, 0x6DBC). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x51C9, kTraditionalVariant, 0x6DBC).
unicode_unihan_variant(0x51CB, kZVariant, 0x96D5).
unicode_unihan_variant(0x51CC, kZVariant, 0xF955).
unicode_unihan_variant(0x51CD, kSimplifiedVariant, 0x51BB).
unicode_unihan_variant(0x51CF, kSemanticVariant, 0x6E1B). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x51CF, kTraditionalVariant, 0x6E1B).
unicode_unihan_variant(0x51D1, kSemanticVariant, 0x6E4A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x51D1, kTraditionalVariant, 0x6E4A).
unicode_unihan_variant(0x51D6, kZVariant, 0x6E96).
unicode_unihan_variant(0x51D9, kSimplifiedVariant, 0x2A79D).
unicode_unihan_variant(0x51DB, kSemanticVariant, 0x61D4). %<kFenn
unicode_unihan_variant(0x51DB, kTraditionalVariant, 0x51DC).
unicode_unihan_variant(0x51DC, kSimplifiedVariant, 0x51DB).
unicode_unihan_variant(0x51E0, kSpecializedSemanticVariant, 0x5E7E). %<kFenn
unicode_unihan_variant(0x51E0, kTraditionalVariant, 0x5E7E).
unicode_unihan_variant(0x51E1, kSemanticVariant, 0x51E2). %<kMatthews,kMeyerWempe 0x51E3<kMatthews
unicode_unihan_variant(0x51E1, kZVariant, 0x51E2).
unicode_unihan_variant(0x51E2, kSemanticVariant, 0x51E1). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x51E2, kZVariant, 0x51E1).
unicode_unihan_variant(0x51E3, kSemanticVariant, 0x51E1). %<kMatthews
unicode_unihan_variant(0x51E3, kZVariant, 0x51E1).
unicode_unihan_variant(0x51E4, kTraditionalVariant, 0x9CF3).
unicode_unihan_variant(0x51E6, kSemanticVariant, 0x458F). %<kFenn 0x8655<kMatthews
unicode_unihan_variant(0x51E6, kSpecializedSemanticVariant, 0x8655). %<kFenn
unicode_unihan_variant(0x51E6, kZVariant, 0x8655).
unicode_unihan_variant(0x51E8, kZVariant, 0x98A8).
unicode_unihan_variant(0x51EB, kTraditionalVariant, 0x9CE7).
unicode_unihan_variant(0x51ED, kTraditionalVariant, 0x6191).
unicode_unihan_variant(0x51EF, kTraditionalVariant, 0x51F1).
unicode_unihan_variant(0x51F1, kSimplifiedVariant, 0x51EF).
unicode_unihan_variant(0x51F3, kSemanticVariant, 0x6AC8). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x51F4, kSemanticVariant, 0x6191). %<kLau,kMatthews
unicode_unihan_variant(0x51F4, kZVariant, 0x6191).
unicode_unihan_variant(0x51F6, kSemanticVariant, 0x342B).
unicode_unihan_variant(0x51F6, kZVariant, 0x5147).
unicode_unihan_variant(0x51F7, kZVariant, 0x584A).
unicode_unihan_variant(0x51F9, kSemanticVariant, 0x20544). %<kLau
unicode_unihan_variant(0x51FA, kZVariant, 0x5C80).
unicode_unihan_variant(0x51FB, kTraditionalVariant, 0x64CA).
unicode_unihan_variant(0x51FC, kZVariant, 0x5E7D).
unicode_unihan_variant(0x51FD, kSemanticVariant, 0x51FE). %<kMatthews 0x5705<kMatthews
unicode_unihan_variant(0x51FD, kZVariant, 0x51FE).
unicode_unihan_variant(0x51FE, kSemanticVariant, 0x51FD). %<kMatthews 0x5705<kMatthews
unicode_unihan_variant(0x51FE, kZVariant, 0x51FD).
unicode_unihan_variant(0x51FF, kTraditionalVariant, 0x947F).
unicode_unihan_variant(0x5200, kSemanticVariant, 0x5202). %<kMatthews
unicode_unihan_variant(0x5200, kZVariant, 0x5202).
unicode_unihan_variant(0x5202, kSemanticVariant, 0x5200). %<kMatthews
unicode_unihan_variant(0x5202, kZVariant, 0x5200).
unicode_unihan_variant(0x5203, kSemanticVariant, 0x5204). %<kFenn
unicode_unihan_variant(0x5203, kZVariant, 0x5204).
unicode_unihan_variant(0x5204, kSemanticVariant, 0x5203). %<kFenn
unicode_unihan_variant(0x5204, kZVariant, 0x5203).
unicode_unihan_variant(0x5205, kSemanticVariant, 0x5275). %<kMatthews
unicode_unihan_variant(0x5206, kSpecializedSemanticVariant, 0x4EFD). %<kFenn
unicode_unihan_variant(0x5207, kZVariant, 0xFA00).
unicode_unihan_variant(0x5208, kZVariant, 0x82C5).
unicode_unihan_variant(0x520A, kSemanticVariant, 0x520B). %<kMatthews
unicode_unihan_variant(0x520A, kZVariant, 0x520B).
unicode_unihan_variant(0x520B, kSemanticVariant, 0x520A). %<kMatthews
unicode_unihan_variant(0x520B, kZVariant, 0x520A).
unicode_unihan_variant(0x520D, kTraditionalVariant, 0x82BB).
unicode_unihan_variant(0x5212, kSpecializedSemanticVariant, 0x5283). %<kFenn
unicode_unihan_variant(0x5212, kTraditionalVariant, 0x5283).
unicode_unihan_variant(0x5213, kSemanticVariant, 0x56ED). %<kMatthews
unicode_unihan_variant(0x5214, kZVariant, 0x6289).
unicode_unihan_variant(0x5217, kZVariant, 0xF99C).
unicode_unihan_variant(0x5218, kSemanticVariant, 0x5289). %<kLau
unicode_unihan_variant(0x5218, kTraditionalVariant, 0x5289).
unicode_unihan_variant(0x5219, kTraditionalVariant, 0x5247).
unicode_unihan_variant(0x521A, kTraditionalVariant, 0x525B).
unicode_unihan_variant(0x521B, kTraditionalVariant, 0x5275).
unicode_unihan_variant(0x5220, kSemanticVariant, 0x522A). %<kMatthews
unicode_unihan_variant(0x5220, kTraditionalVariant, 0x522A).
unicode_unihan_variant(0x5225, kSimplifiedVariant, 0x522B).
unicode_unihan_variant(0x5226, kSemanticVariant, 0x5227). %<kFenn 0x52AB<kLau,kMeyerWempe
unicode_unihan_variant(0x5226, kZVariant, 0x52AB).
unicode_unihan_variant(0x5227, kSemanticVariant, 0x5226). %<kFenn 0x52AB<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5228, kSemanticVariant, 0x924B). %<kMatthews
unicode_unihan_variant(0x5229, kSemanticVariant, 0x25762). %<kLau
unicode_unihan_variant(0x5229, kZVariant, 0xF9DD).
unicode_unihan_variant(0x522A, kSemanticVariant, 0x5220). %<kMatthews
unicode_unihan_variant(0x522A, kSimplifiedVariant, 0x5220).
unicode_unihan_variant(0x522B, kTraditionalVariant, 0x5225).
unicode_unihan_variant(0x522C, kTraditionalVariant, 0x5257).
unicode_unihan_variant(0x522D, kTraditionalVariant, 0x5244).
unicode_unihan_variant(0x522E, kTraditionalVariant, 0x98B3).
unicode_unihan_variant(0x5231, kSemanticVariant, 0x5275). %<kHanYu:TZ,kMeyerWempe
unicode_unihan_variant(0x5231, kZVariant, 0x524F).
unicode_unihan_variant(0x5234, kSemanticVariant, 0x5241). %<kMeyerWempe
unicode_unihan_variant(0x5234, kZVariant, 0x5241).
unicode_unihan_variant(0x5236, kTraditionalVariant, 0x88FD).
unicode_unihan_variant(0x5237, kSemanticVariant, 0x355E). %<kMatthews
unicode_unihan_variant(0x5239, kTraditionalVariant, 0x524E).
unicode_unihan_variant(0x523A, kSpecializedSemanticVariant, 0x83BF). %<kMeyerWempe
unicode_unihan_variant(0x523A, kZVariant, 0x523E).
unicode_unihan_variant(0x523C, kSemanticVariant, 0x52AB). %<kMatthews
unicode_unihan_variant(0x523C, kZVariant, 0x52AB).
unicode_unihan_variant(0x523D, kTraditionalVariant, 0x528A).
unicode_unihan_variant(0x523E, kTraditionalVariant, 0x34E8).
unicode_unihan_variant(0x523E, kZVariant, 0x523A).
unicode_unihan_variant(0x523F, kTraditionalVariant, 0x528C).
unicode_unihan_variant(0x5240, kTraditionalVariant, 0x5274).
unicode_unihan_variant(0x5241, kSemanticVariant, 0x5234). %<kMeyerWempe
unicode_unihan_variant(0x5241, kZVariant, 0x5234).
unicode_unihan_variant(0x5242, kTraditionalVariant, 0x5291).
unicode_unihan_variant(0x5243, kSemanticVariant, 0x9B00). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5244, kSimplifiedVariant, 0x522D).
unicode_unihan_variant(0x5247, kSimplifiedVariant, 0x5219).
unicode_unihan_variant(0x5249, kSpecializedSemanticVariant, 0x92BC).
unicode_unihan_variant(0x524B, kSemanticVariant, 0x5C05). %<kHKGlyph,kMatthews).
unicode_unihan_variant(0x524B, kSimplifiedVariant, 0x514B).
unicode_unihan_variant(0x524C, kSpecializedSemanticVariant, 0x5DE4). %<kFenn
unicode_unihan_variant(0x524E, kSimplifiedVariant, 0x5239).
unicode_unihan_variant(0x524F, kZVariant, 0x5231).
unicode_unihan_variant(0x5250, kTraditionalVariant, 0x526E).
unicode_unihan_variant(0x5251, kTraditionalVariant, 0x528D).
unicode_unihan_variant(0x5257, kSemanticVariant, 0x5277). %<kLau,kMatthews
unicode_unihan_variant(0x5257, kSimplifiedVariant, 0x522C).
unicode_unihan_variant(0x5259, kSpecializedSemanticVariant, 0x5275). %<kFenn
unicode_unihan_variant(0x5259, kZVariant, 0x5275).
unicode_unihan_variant(0x525B, kSimplifiedVariant, 0x521A).
unicode_unihan_variant(0x525D, kSimplifiedVariant, 0x5265).
unicode_unihan_variant(0x5263, kZVariant, 0x528D).
unicode_unihan_variant(0x5264, kZVariant, 0x5291).
unicode_unihan_variant(0x5265, kTraditionalVariant, 0x525D).
unicode_unihan_variant(0x5266, kSemanticVariant, 0x95B9). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5267, kTraditionalVariant, 0x5287).
unicode_unihan_variant(0x5269, kSemanticVariant, 0x8CF8). %<kLau,kMatthews
unicode_unihan_variant(0x5269, kZVariant, 0x5270).
unicode_unihan_variant(0x526A, kSemanticVariant, 0x7FE6). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x526E, kSimplifiedVariant, 0x5250).
unicode_unihan_variant(0x5270, kZVariant, 0x5269).
unicode_unihan_variant(0x5271, kZVariant, 0x528D).
unicode_unihan_variant(0x5273, kSemanticVariant, 0x5284). %<kMeyerWempe
unicode_unihan_variant(0x5273, kSpecializedSemanticVariant, 0x5284).
unicode_unihan_variant(0x5273, kZVariant, 0x7B9A).
unicode_unihan_variant(0x5274, kSimplifiedVariant, 0x5240).
unicode_unihan_variant(0x5275, kSemanticVariant, 0x5205). %<kMatthews 0x5231<kMeyerWempe,kHanYu 0x6227<kMatthews
unicode_unihan_variant(0x5275, kSimplifiedVariant, 0x521B).
unicode_unihan_variant(0x5275, kSpecializedSemanticVariant, 0x5259). %<kFenn
unicode_unihan_variant(0x5275, kZVariant, 0x5259).
unicode_unihan_variant(0x5277, kSemanticVariant, 0x5257). %<kLau,kMatthews
unicode_unihan_variant(0x5277, kZVariant, 0x93DF).
unicode_unihan_variant(0x527E, kSimplifiedVariant, 0x206C5).
unicode_unihan_variant(0x527F, kSemanticVariant, 0x207B0). %<kFenn
unicode_unihan_variant(0x527F, kZVariant, 0x52E6).
unicode_unihan_variant(0x5283, kSimplifiedVariant, 0x5212).
unicode_unihan_variant(0x5283, kSpecializedSemanticVariant, 0x5212). %<kFenn
unicode_unihan_variant(0x5284, kSemanticVariant, 0x5273). %<kMeyerWempe
unicode_unihan_variant(0x5284, kSpecializedSemanticVariant, 0x5273).
unicode_unihan_variant(0x5286, kZVariant, 0x942E).
unicode_unihan_variant(0x5287, kSimplifiedVariant, 0x5267).
unicode_unihan_variant(0x5288, kSemanticVariant, 0x64D7). %<kMeyerWempe
unicode_unihan_variant(0x5289, kSemanticVariant, 0x5218). %<kLau
unicode_unihan_variant(0x5289, kSimplifiedVariant, 0x5218).
unicode_unihan_variant(0x528A, kSimplifiedVariant, 0x523D).
unicode_unihan_variant(0x528C, kSimplifiedVariant, 0x523F).
unicode_unihan_variant(0x528D, kSemanticVariant, 0x5292). %<kMatthews
unicode_unihan_variant(0x528D, kSimplifiedVariant, 0x5251).
unicode_unihan_variant(0x528E, kSemanticVariant, 0x5294). %<kFenn
unicode_unihan_variant(0x528F, kSimplifiedVariant, 0x34E5).
unicode_unihan_variant(0x5291, kSimplifiedVariant, 0x5242).
unicode_unihan_variant(0x5291, kZVariant, 0x5264).
unicode_unihan_variant(0x5292, kSemanticVariant, 0x528D). %<kMatthews
unicode_unihan_variant(0x5293, kSemanticVariant, 0x34F7). %<kMatthews
unicode_unihan_variant(0x5294, kSemanticVariant, 0x528E). %<kFenn
unicode_unihan_variant(0x5294, kZVariant, 0x528D).
unicode_unihan_variant(0x529A, kSimplifiedVariant, 0x3509).
unicode_unihan_variant(0x529B, kZVariant, 0xF98A).
unicode_unihan_variant(0x529D, kSemanticVariant, 0x52F8). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x529D, kTraditionalVariant, 0x52F8).
unicode_unihan_variant(0x529E, kSemanticVariant, 0x8FA6). %<kFenn
unicode_unihan_variant(0x529E, kTraditionalVariant, 0x8FA6).
unicode_unihan_variant(0x52A1, kTraditionalVariant, 0x52D9).
unicode_unihan_variant(0x52A2, kTraditionalVariant, 0x52F1).
unicode_unihan_variant(0x52A2, kZVariant, 0x52F5).
unicode_unihan_variant(0x52A3, kZVariant, 0xF99D).
unicode_unihan_variant(0x52A8, kTraditionalVariant, 0x52D5).
unicode_unihan_variant(0x52AB, kSemanticVariant, 0x5226). %<kLau,kMeyerWempe 0x5227<kLau,kMatthews,kMeyerWempe 0x523C<kMatthews
unicode_unihan_variant(0x52AB, kZVariant, 0x5226).
unicode_unihan_variant(0x52B1, kTraditionalVariant, 0x52F5).
unicode_unihan_variant(0x52B2, kTraditionalVariant, 0x52C1).
unicode_unihan_variant(0x52B3, kSpecializedSemanticVariant, 0x52DE). %<kFenn
unicode_unihan_variant(0x52B3, kTraditionalVariant, 0x52DE).
unicode_unihan_variant(0x52B4, kZVariant, 0x52DE).
unicode_unihan_variant(0x52B5, kZVariant, 0x5238).
unicode_unihan_variant(0x52B9, kSemanticVariant, 0x6548). %<kMeyerWempe
unicode_unihan_variant(0x52B9, kZVariant, 0x6548).
unicode_unihan_variant(0x52BF, kTraditionalVariant, 0x52E2).
unicode_unihan_variant(0x52C1, kSimplifiedVariant, 0x52B2).
unicode_unihan_variant(0x52C1, kSpecializedSemanticVariant, 0x4FD3). %<kMeyerWempe
unicode_unihan_variant(0x52C3, kZVariant, 0x8274).
unicode_unihan_variant(0x52C4, kSemanticVariant, 0x654F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x52C5, kSemanticVariant, 0x52D1). %<kMatthews 0x6555<kMatthews
unicode_unihan_variant(0x52C5, kSpecializedSemanticVariant, 0x52D1). %<kMeyerWempe
unicode_unihan_variant(0x52CB, kTraditionalVariant, 0x52DB).
unicode_unihan_variant(0x52D1, kSemanticVariant, 0x5008). %<kLau 0x52C5<kMatthews 0x6555<kMatthews 0x5FA0<kLau
unicode_unihan_variant(0x52D1, kSpecializedSemanticVariant, 0x52C5). %<kMeyerWempe
unicode_unihan_variant(0x52D2, kZVariant, 0xF952).
unicode_unihan_variant(0x52D5, kSimplifiedVariant, 0x52A8).
unicode_unihan_variant(0x52D6, kSemanticVariant, 0x52D7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x52D7, kSemanticVariant, 0x52D6). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x52D9, kSimplifiedVariant, 0x52A1).
unicode_unihan_variant(0x52DA, kTraditionalVariant, 0x52E9).
unicode_unihan_variant(0x52DB, kSemanticVariant, 0x52F3). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x52DB, kSimplifiedVariant, 0x52CB).
unicode_unihan_variant(0x52DB, kZVariant, 0x52F2).
unicode_unihan_variant(0x52DD, kSimplifiedVariant, 0x80DC).
unicode_unihan_variant(0x52DE, kSimplifiedVariant, 0x52B3).
unicode_unihan_variant(0x52DE, kSpecializedSemanticVariant, 0x52B3). %<kFenn
unicode_unihan_variant(0x52DE, kZVariant, 0xF92F).
unicode_unihan_variant(0x52E2, kSimplifiedVariant, 0x52BF).
unicode_unihan_variant(0x52E4, kZVariant, 0x61C3).
unicode_unihan_variant(0x52E6, kZVariant, 0x527F).
unicode_unihan_variant(0x52E7, kZVariant, 0x52F8).
unicode_unihan_variant(0x52E9, kSimplifiedVariant, 0x52DA).
unicode_unihan_variant(0x52F0, kSemanticVariant, 0x5354). %<kFenn 0x604A<kMatthews
unicode_unihan_variant(0x52F0, kSpecializedSemanticVariant, 0x8105). %<kMeyerWempe
unicode_unihan_variant(0x52F1, kSimplifiedVariant, 0x52A2).
unicode_unihan_variant(0x52F2, kZVariant, 0x52DB).
unicode_unihan_variant(0x52F3, kSemanticVariant, 0x52DB). %<kHKGlyph,kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x52F3, kZVariant, 0x52DB).
unicode_unihan_variant(0x52F5, kSimplifiedVariant, 0x52B1).
unicode_unihan_variant(0x52F5, kZVariant, 0x52A2).
unicode_unihan_variant(0x52F8, kSemanticVariant, 0x529D). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x52F8, kSimplifiedVariant, 0x529D).
unicode_unihan_variant(0x52F8, kZVariant, 0x52E7).
unicode_unihan_variant(0x52F9, kSemanticVariant, 0x5305). %<kMatthews
unicode_unihan_variant(0x52FB, kSemanticVariant, 0x5300). %<kHKGlyph,kMatthews
unicode_unihan_variant(0x52FB, kSimplifiedVariant, 0x5300).
unicode_unihan_variant(0x52FD, kSemanticVariant, 0x3C92). %<kMathews,kMeyerWempe 0x83E2<kMatthews
unicode_unihan_variant(0x52FE, kZVariant, 0x53E5).
unicode_unihan_variant(0x5300, kSemanticVariant, 0x52FB). %<kHKGlyph,kMatthews
unicode_unihan_variant(0x5300, kTraditionalVariant, 0x52FB).
unicode_unihan_variant(0x5303, kSemanticVariant, 0x4E10). %<kMatthews 0x5304<kMatthews
unicode_unihan_variant(0x5304, kSemanticVariant, 0x5303). %<kMatthews
unicode_unihan_variant(0x5305, kSemanticVariant, 0x52F9). %<kMatthews
unicode_unihan_variant(0x5306, kSemanticVariant, 0x60A4). %<kFenn 0x6031<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x5308, kSemanticVariant, 0x80F8). %<kLau,kMatthews,kMeyerWempe 0x80F7<kLau,kMatthews
unicode_unihan_variant(0x530A, kSemanticVariant, 0x63AC). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x530B, kSemanticVariant, 0x9676). %<kLau,kMatthews
unicode_unihan_variant(0x530F, kSemanticVariant, 0x74DF). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5317, kZVariant, 0xF963).
unicode_unihan_variant(0x5318, kSemanticVariant, 0x8166). %<kMatthews
unicode_unihan_variant(0x5319, kSemanticVariant, 0x9349). %<kMatthews
unicode_unihan_variant(0x531D, kSemanticVariant, 0x5E00). %<kMatthews 0x8FCA<kMatthews
unicode_unihan_variant(0x5326, kTraditionalVariant, 0x532D).
unicode_unihan_variant(0x5327, kSemanticVariant, 0x7BCB). %<kMatthews
unicode_unihan_variant(0x532D, kSimplifiedVariant, 0x5326).
unicode_unihan_variant(0x532E, kTraditionalVariant, 0x5331).
unicode_unihan_variant(0x532F, kSemanticVariant, 0x6ED9). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x532F, kSimplifiedVariant, 0x6C47).
unicode_unihan_variant(0x5331, kSemanticVariant, 0x6A3B). %<kMeyerWempe 0x9400<kMatthews 0x6AC3<kLau,kMatthews,kMeyerWempe 0x994B<kLau
unicode_unihan_variant(0x5331, kSimplifiedVariant, 0x532E).
unicode_unihan_variant(0x5333, kSemanticVariant, 0x5969). %<kFenn
unicode_unihan_variant(0x5333, kZVariant, 0x5969).
unicode_unihan_variant(0x5335, kSemanticVariant, 0x6ADD). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5339, kSemanticVariant, 0x758B). %<kMeyerWempe
unicode_unihan_variant(0x5339, kZVariant, 0x758B).
unicode_unihan_variant(0x533A, kTraditionalVariant, 0x5340).
unicode_unihan_variant(0x533B, kSemanticVariant, 0x91AB). %<kMatthews
unicode_unihan_variant(0x533B, kTraditionalVariant, 0x91AB).
unicode_unihan_variant(0x533F, kZVariant, 0xF9EB).
unicode_unihan_variant(0x5340, kSimplifiedVariant, 0x533A).
unicode_unihan_variant(0x5341, kSpecializedSemanticVariant, 0x62FE).
unicode_unihan_variant(0x5341, kZVariant, 0xF973).
unicode_unihan_variant(0x5343, kSemanticVariant, 0x4EDF). %<kMeyerWempe
unicode_unihan_variant(0x5343, kSpecializedSemanticVariant, 0x4EDF).
unicode_unihan_variant(0x5343, kZVariant, 0x4EDF).
unicode_unihan_variant(0x5344, kSemanticVariant, 0x5EFF). %<kMatthews
unicode_unihan_variant(0x5344, kZVariant, 0x5EFF).
unicode_unihan_variant(0x5346, kZVariant, 0x5352).
unicode_unihan_variant(0x5347, kZVariant, 0x6607).
unicode_unihan_variant(0x5349, kSemanticVariant, 0x8294). %<kMatthews
unicode_unihan_variant(0x534B, kZVariant, 0x4E16).
unicode_unihan_variant(0x534D, kSemanticVariant, 0x4E07). %<kFenn 0x5350 0x842C<kFenn
unicode_unihan_variant(0x534D, kZVariant, 0x5350).
unicode_unihan_variant(0x534E, kTraditionalVariant, 0x83EF).
unicode_unihan_variant(0x534F, kTraditionalVariant, 0x5354).
unicode_unihan_variant(0x5350, kSemanticVariant, 0x534D).
unicode_unihan_variant(0x5350, kZVariant, 0x534D).
unicode_unihan_variant(0x5352, kSemanticVariant, 0x461A). %<kLau,kMatthews
unicode_unihan_variant(0x5352, kZVariant, 0x5346).
unicode_unihan_variant(0x5353, kSpecializedSemanticVariant, 0x68F9). %<kMeyerWempe
unicode_unihan_variant(0x5354, kSemanticVariant, 0x52F0). %<kFenn 0x65EA<kMatthews
unicode_unihan_variant(0x5354, kSimplifiedVariant, 0x534F).
unicode_unihan_variant(0x5354, kZVariant, 0x604A).
unicode_unihan_variant(0x5355, kSemanticVariant, 0x55AE). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5355, kTraditionalVariant, 0x55AE).
unicode_unihan_variant(0x5356, kTraditionalVariant, 0x8CE3).
unicode_unihan_variant(0x5358, kZVariant, 0x55AE).
unicode_unihan_variant(0x535A, kSemanticVariant, 0x613D). %<kMatthews
unicode_unihan_variant(0x535A, kZVariant, 0x613D).
unicode_unihan_variant(0x5360, kSpecializedSemanticVariant, 0x4F54). %<kFenn
unicode_unihan_variant(0x5360, kZVariant, 0x4F54).
unicode_unihan_variant(0x5362, kTraditionalVariant, 0x76E7).
unicode_unihan_variant(0x5364, kTraditionalVariant, 0x9E75).
unicode_unihan_variant(0x5367, kSemanticVariant, 0x81E5). %<kHKGlyph
unicode_unihan_variant(0x5369, kSemanticVariant, 0x353E). %<kMatthews
unicode_unihan_variant(0x5369, kSpecializedSemanticVariant, 0x353E). %<kFenn 0x7BC0<kFenn
unicode_unihan_variant(0x5369, kZVariant, 0x90E8).
unicode_unihan_variant(0x536B, kTraditionalVariant, 0x885B).
unicode_unihan_variant(0x536C, kSemanticVariant, 0x536D). %<kMatthews
unicode_unihan_variant(0x536D, kSemanticVariant, 0x536C). %<kMatthews
unicode_unihan_variant(0x536E, kZVariant, 0x5DF5).
unicode_unihan_variant(0x536F, kZVariant, 0x623C).
unicode_unihan_variant(0x5373, kSemanticVariant, 0x537D). %<kLau,kMatthews
unicode_unihan_variant(0x5373, kZVariant, 0x537D).
unicode_unihan_variant(0x5374, kSemanticVariant, 0x537B). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5374, kTraditionalVariant, 0x537B).
unicode_unihan_variant(0x5375, kZVariant, 0xF91C).
unicode_unihan_variant(0x5377, kZVariant, 0x6372).
unicode_unihan_variant(0x5378, kZVariant, 0x7F37).
unicode_unihan_variant(0x5379, kSemanticVariant, 0x6064). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5379, kZVariant, 0x6064).
unicode_unihan_variant(0x537B, kSemanticVariant, 0x5374). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x537B, kSimplifiedVariant, 0x5374).
unicode_unihan_variant(0x537D, kSemanticVariant, 0x5373). %<kLau,kMatthews
unicode_unihan_variant(0x537D, kZVariant, 0x5373).
unicode_unihan_variant(0x5382, kTraditionalVariant, 0x5EE0).
unicode_unihan_variant(0x5384, kSemanticVariant, 0x6239). %<kMatthews
unicode_unihan_variant(0x5384, kZVariant, 0x9628).
unicode_unihan_variant(0x5385, kTraditionalVariant, 0x5EF3).
unicode_unihan_variant(0x5386, kSemanticVariant, 0x53B2). %<kFenn
unicode_unihan_variant(0x5386, kTraditionalVariant, 0x66C6). %0x6B77
unicode_unihan_variant(0x5386, kZVariant, 0x6B77).
unicode_unihan_variant(0x5389, kTraditionalVariant, 0x53B2).
unicode_unihan_variant(0x538B, kTraditionalVariant, 0x58D3).
unicode_unihan_variant(0x538C, kTraditionalVariant, 0x53AD).
unicode_unihan_variant(0x538D, kTraditionalVariant, 0x5399).
unicode_unihan_variant(0x538E, kSemanticVariant, 0x7825). %<kMatthews
unicode_unihan_variant(0x5390, kTraditionalVariant, 0x9F8E).
unicode_unihan_variant(0x5390, kZVariant, 0x9F90).
unicode_unihan_variant(0x5393, kSemanticVariant, 0x5D16). %<kMatthews
unicode_unihan_variant(0x5395, kTraditionalVariant, 0x5EC1).
unicode_unihan_variant(0x5395, kZVariant, 0x5EC1).
unicode_unihan_variant(0x5396, kSemanticVariant, 0x5EAC). %<kMeyerWempe
unicode_unihan_variant(0x5396, kZVariant, 0x9F90).
unicode_unihan_variant(0x5398, kSemanticVariant, 0x91D0). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5398, kTraditionalVariant, 0x91D0).
unicode_unihan_variant(0x5399, kSimplifiedVariant, 0x538D).
unicode_unihan_variant(0x539B, kZVariant, 0x5EF3).
unicode_unihan_variant(0x53A0, kSemanticVariant, 0x5EC1). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x53A0, kZVariant, 0x5EC1).
unicode_unihan_variant(0x53A2, kTraditionalVariant, 0x5EC2).
unicode_unihan_variant(0x53A3, kTraditionalVariant, 0x53B4).
unicode_unihan_variant(0x53A4, kSemanticVariant, 0x66C6). %<kLau,kMatthews
unicode_unihan_variant(0x53A6, kSemanticVariant, 0x5EC8). %<kLau,kMatthews
unicode_unihan_variant(0x53A6, kTraditionalVariant, 0x5EC8).
unicode_unihan_variant(0x53A8, kSemanticVariant, 0x5EDA). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x53A8, kSpecializedSemanticVariant, 0x2228D).
unicode_unihan_variant(0x53A8, kTraditionalVariant, 0x5EDA).
unicode_unihan_variant(0x53A9, kSemanticVariant, 0x5ED0). %<kMatthews
unicode_unihan_variant(0x53A9, kTraditionalVariant, 0x5EC4).
unicode_unihan_variant(0x53AA, kSemanticVariant, 0x5ED1). %<kMatthews
unicode_unihan_variant(0x53AD, kSimplifiedVariant, 0x538C).
unicode_unihan_variant(0x53AE, kSemanticVariant, 0x348B). %<kMatthews 0x5EDD<kMatthews
unicode_unihan_variant(0x53AE, kTraditionalVariant, 0x5EDD).
unicode_unihan_variant(0x53B0, kSemanticVariant, 0x5EE0). %<kLau,kMatthews
unicode_unihan_variant(0x53B0, kZVariant, 0x5EE0).
unicode_unihan_variant(0x53B2, kSemanticVariant, 0x5386). %<kFenn
unicode_unihan_variant(0x53B2, kSimplifiedVariant, 0x5389).
unicode_unihan_variant(0x53B3, kZVariant, 0x56B4).
unicode_unihan_variant(0x53B4, kSimplifiedVariant, 0x53A3).
unicode_unihan_variant(0x53B5, kZVariant, 0x6E90).
unicode_unihan_variant(0x53B6, kSemanticVariant, 0x67D0). %<kLau
unicode_unihan_variant(0x53B6, kSpecializedSemanticVariant, 0x67D0). %<kFenn
unicode_unihan_variant(0x53B6, kZVariant, 0x79C1).
unicode_unihan_variant(0x53B7, kSemanticVariant, 0x80B1). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x53B9, kSemanticVariant, 0x53F4). %<kMeyerWempe
unicode_unihan_variant(0x53BF, kTraditionalVariant, 0x7E23).
unicode_unihan_variant(0x53C1, kSemanticVariant, 0x4E09). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x53C1, kSpecializedSemanticVariant, 0x4E09). %<kFenn
unicode_unihan_variant(0x53C1, kTraditionalVariant, 0x53C4).
unicode_unihan_variant(0x53C1, kZVariant, 0x53C3).
unicode_unihan_variant(0x53C2, kTraditionalVariant, 0x53C3).
unicode_unihan_variant(0x53C3, kSemanticVariant, 0x53C5). %<kMatthews
unicode_unihan_variant(0x53C3, kSimplifiedVariant, 0x53C2).
unicode_unihan_variant(0x53C3, kSpecializedSemanticVariant, 0x53C5). %<kFenn 0x8593<kFenn
unicode_unihan_variant(0x53C3, kZVariant, 0xF96B).
unicode_unihan_variant(0x53C4, kSimplifiedVariant, 0x53C1).
unicode_unihan_variant(0x53C4, kZVariant, 0x53C3).
unicode_unihan_variant(0x53C5, kSemanticVariant, 0x53C3). %<kMatthews
unicode_unihan_variant(0x53C5, kSpecializedSemanticVariant, 0x53C3). %<kFenn
unicode_unihan_variant(0x53C6, kZVariant, 0x9749).
unicode_unihan_variant(0x53C7, kZVariant, 0x9746).
unicode_unihan_variant(0x53C9, kSpecializedSemanticVariant, 0x91F5). %<kMeyerWempe
unicode_unihan_variant(0x53CC, kSemanticVariant, 0x96D9). %<kMatthews
unicode_unihan_variant(0x53CC, kSpecializedSemanticVariant, 0x96D9). %<kFenn
unicode_unihan_variant(0x53CC, kTraditionalVariant, 0x96D9).
unicode_unihan_variant(0x53CE, kSemanticVariant, 0x6536). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x53CE, kZVariant, 0x6536).
unicode_unihan_variant(0x53D1, kTraditionalVariant, 0x767C). %0x9AEE
unicode_unihan_variant(0x53D1, kZVariant, 0x767C).
unicode_unihan_variant(0x53D4, kSemanticVariant, 0x5C17). %<kMatthews
unicode_unihan_variant(0x53D8, kSemanticVariant, 0x8B8A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x53D8, kTraditionalVariant, 0x8B8A).
unicode_unihan_variant(0x53D9, kSemanticVariant, 0x654D). %<kFenn 0x6558<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x53D9, kTraditionalVariant, 0x6558).
unicode_unihan_variant(0x53DC, kSemanticVariant, 0x5081). %<kMatthews 0x53DF<kMatthews
unicode_unihan_variant(0x53DC, kZVariant, 0x53DF).
unicode_unihan_variant(0x53DD, kZVariant, 0x544A).
unicode_unihan_variant(0x53DF, kSemanticVariant, 0x5081). %<kMatthews,kMeyerWempe 0x53DC<kMatthews
unicode_unihan_variant(0x53DF, kSpecializedSemanticVariant, 0x58B0). %<kMeyerWempe
unicode_unihan_variant(0x53DF, kZVariant, 0x53DC).
unicode_unihan_variant(0x53E0, kSemanticVariant, 0x66E1). %<kMatthews 0x758A<kMatthews,kMeyerWempe
unicode_unihan_variant(0x53E0, kTraditionalVariant, 0x758A).
unicode_unihan_variant(0x53E1, kSemanticVariant, 0x777F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x53E1, kZVariant, 0x777F).
unicode_unihan_variant(0x53E2, kSemanticVariant, 0x6A37). %<kFenn
unicode_unihan_variant(0x53E2, kSimplifiedVariant, 0x4E1B).
unicode_unihan_variant(0x53E5, kZVariant, 0xF906).
unicode_unihan_variant(0x53E8, kSemanticVariant, 0x9955). %<kMeyerWempe
unicode_unihan_variant(0x53EA, kSemanticVariant, 0x5B50). %<kLau
unicode_unihan_variant(0x53EA, kTraditionalVariant, 0x96BB).
unicode_unihan_variant(0x53EA, kZVariant, 0x6B62).
unicode_unihan_variant(0x53EB, kSemanticVariant, 0x544C). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x53F0, kSemanticVariant, 0x81FA). %<kHKGlyph,kLau
unicode_unihan_variant(0x53F0, kSimplifiedVariant, 0x53F0).
unicode_unihan_variant(0x53F0, kTraditionalVariant, 0x53F0). %0x6AAF 0x81FA 0x98B1
unicode_unihan_variant(0x53F4, kSemanticVariant, 0x53B9). %<kMeyerWempe
unicode_unihan_variant(0x53F5, kSemanticVariant, 0x5C00). %<kLau,kMatthews
unicode_unihan_variant(0x53F6, kTraditionalVariant, 0x8449).
unicode_unihan_variant(0x53F7, kSemanticVariant, 0x865F). %<kLau,kMatthews
unicode_unihan_variant(0x53F7, kSpecializedSemanticVariant, 0x865F). %<kFenn
unicode_unihan_variant(0x53F7, kTraditionalVariant, 0x865F).
unicode_unihan_variant(0x53F9, kTraditionalVariant, 0x5606).
unicode_unihan_variant(0x53FD, kTraditionalVariant, 0x5630).
unicode_unihan_variant(0x5401, kZVariant, 0x7C72).
unicode_unihan_variant(0x5403, kSemanticVariant, 0x55AB). %<kLau,kMatthews
unicode_unihan_variant(0x5403, kZVariant, 0x55AB).
unicode_unihan_variant(0x5408, kZVariant, 0x95A4).
unicode_unihan_variant(0x540A, kSemanticVariant, 0x5F14). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x540B, kSpecializedSemanticVariant, 0x5BF8).
unicode_unihan_variant(0x540C, kSemanticVariant, 0x4EDD). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x540C, kTraditionalVariant, 0x8855).
unicode_unihan_variant(0x540C, kZVariant, 0x4EDD).
unicode_unihan_variant(0x540E, kSimplifiedVariant, 0x540E).
unicode_unihan_variant(0x540E, kTraditionalVariant, 0x540E). %0x5F8C
unicode_unihan_variant(0x540F, kZVariant, 0xF9DE).
unicode_unihan_variant(0x5411, kTraditionalVariant, 0x66CF).
unicode_unihan_variant(0x5411, kZVariant, 0x56AE).
unicode_unihan_variant(0x5412, kSemanticVariant, 0x54A4). %<kMatthews
unicode_unihan_variant(0x5412, kSimplifiedVariant, 0x54A4).
unicode_unihan_variant(0x5413, kTraditionalVariant, 0x5687).
unicode_unihan_variant(0x5415, kTraditionalVariant, 0x5442).
unicode_unihan_variant(0x5417, kTraditionalVariant, 0x55CE).
unicode_unihan_variant(0x541A, kSemanticVariant, 0x54BF). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x541D, kSemanticVariant, 0x6061). %<kMatthews 0x608B<kMatthews
unicode_unihan_variant(0x541D, kZVariant, 0xF9ED).
unicode_unihan_variant(0x541E, kSemanticVariant, 0x5451). %<kMatthews
unicode_unihan_variant(0x541E, kZVariant, 0x5451).
unicode_unihan_variant(0x541F, kSemanticVariant, 0x552B). %<kFenn
unicode_unihan_variant(0x5423, kSemanticVariant, 0x551A). %<kMatthews
unicode_unihan_variant(0x5423, kTraditionalVariant, 0x551A).
unicode_unihan_variant(0x5428, kSemanticVariant, 0x554D). %<kMeyerWempe
unicode_unihan_variant(0x5428, kTraditionalVariant, 0x5678).
unicode_unihan_variant(0x542B, kSpecializedSemanticVariant, 0x5505). %<kMeyerWempe
unicode_unihan_variant(0x542C, kSemanticVariant, 0x807D). %<kLau,kMatthews
unicode_unihan_variant(0x542C, kSpecializedSemanticVariant, 0x807D). %<kFenn
unicode_unihan_variant(0x542C, kTraditionalVariant, 0x807D).
unicode_unihan_variant(0x542F, kTraditionalVariant, 0x555F).
unicode_unihan_variant(0x542F, kZVariant, 0x5553).
unicode_unihan_variant(0x5432, kSemanticVariant, 0x54C2). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5433, kSemanticVariant, 0x5449). %<kMatthews
unicode_unihan_variant(0x5433, kSimplifiedVariant, 0x5434).
unicode_unihan_variant(0x5433, kZVariant, 0x5449).
unicode_unihan_variant(0x5434, kTraditionalVariant, 0x5433).
unicode_unihan_variant(0x5436, kSemanticVariant, 0x8A25). %<kLau,kMatthews
unicode_unihan_variant(0x5436, kSimplifiedVariant, 0x5450).
unicode_unihan_variant(0x5438, kSemanticVariant, 0x564F). %<kMatthews
unicode_unihan_variant(0x5439, kSemanticVariant, 0x9FA1). %<kMatthews
unicode_unihan_variant(0x543B, kSemanticVariant, 0x5461). %<kMatthews 0x80B3<kMatthews,kMeyerWempe
unicode_unihan_variant(0x543F, kSemanticVariant, 0x544A). %<kMatthews
unicode_unihan_variant(0x543F, kZVariant, 0x544A).
unicode_unihan_variant(0x5442, kSimplifiedVariant, 0x5415).
unicode_unihan_variant(0x5442, kZVariant, 0xF980).
unicode_unihan_variant(0x5443, kSpecializedSemanticVariant, 0x9628). %<kMeyerWempe
unicode_unihan_variant(0x5446, kSemanticVariant, 0x7343). %<kMatthews
unicode_unihan_variant(0x5446, kSpecializedSemanticVariant, 0x7343). %<kMeyerWempe
unicode_unihan_variant(0x5446, kZVariant, 0x9A03).
unicode_unihan_variant(0x5449, kSemanticVariant, 0x5433). %<kMatthews
unicode_unihan_variant(0x5449, kZVariant, 0x5433).
unicode_unihan_variant(0x544A, kSemanticVariant, 0x543F). %<kMatthews
unicode_unihan_variant(0x544A, kZVariant, 0x543F).
unicode_unihan_variant(0x544C, kSemanticVariant, 0x53EB). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5450, kTraditionalVariant, 0x5436).
unicode_unihan_variant(0x5451, kSemanticVariant, 0x541E). %<kMatthews
unicode_unihan_variant(0x5451, kZVariant, 0x541E).
unicode_unihan_variant(0x5452, kTraditionalVariant, 0x5638).
unicode_unihan_variant(0x5453, kTraditionalVariant, 0x56C8).
unicode_unihan_variant(0x5455, kTraditionalVariant, 0x5614).
unicode_unihan_variant(0x5456, kTraditionalVariant, 0x56A6).
unicode_unihan_variant(0x5457, kTraditionalVariant, 0x5504).
unicode_unihan_variant(0x5458, kTraditionalVariant, 0x54E1).
unicode_unihan_variant(0x5459, kTraditionalVariant, 0x54BC).
unicode_unihan_variant(0x545B, kTraditionalVariant, 0x55C6).
unicode_unihan_variant(0x545C, kTraditionalVariant, 0x55DA).
unicode_unihan_variant(0x5460, kSemanticVariant, 0x6B55). %<kMatthews 0x5674<kMatthews
unicode_unihan_variant(0x5461, kSemanticVariant, 0x543B). %<kMatthews
unicode_unihan_variant(0x5468, kSemanticVariant, 0x9031). %<kLau
unicode_unihan_variant(0x546A, kSemanticVariant, 0x8A4B). %<kFenn 0x5492<kLau
unicode_unihan_variant(0x546A, kZVariant, 0x5492).
unicode_unihan_variant(0x5471, kZVariant, 0x54CC).
unicode_unihan_variant(0x5474, kSpecializedSemanticVariant, 0x8A6C). %<kMeyerWempe
unicode_unihan_variant(0x5475, kZVariant, 0x8A36).
unicode_unihan_variant(0x547C, kZVariant, 0x8B3C).
unicode_unihan_variant(0x5480, kSemanticVariant, 0x89DC). %<kLau 0x5634<kLau
unicode_unihan_variant(0x5480, kZVariant, 0x5634).
unicode_unihan_variant(0x5482, kSemanticVariant, 0x20BD7). %<kFenn
unicode_unihan_variant(0x548A, kSemanticVariant, 0x548C). %<kLau,kMatthews 0x9FA2<kLau,kMatthews
unicode_unihan_variant(0x548C, kSemanticVariant, 0x548A). %<kLau,kMatthews 0x9FA2<kLau,kMatthews
unicode_unihan_variant(0x548C, kSpecializedSemanticVariant, 0x9FA2). %<kFenn
unicode_unihan_variant(0x548C, kZVariant, 0x9FA2).
unicode_unihan_variant(0x548F, kSemanticVariant, 0x8A60). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x548F, kTraditionalVariant, 0x8A60).
unicode_unihan_variant(0x5492, kSemanticVariant, 0x546A). %<kLau
unicode_unihan_variant(0x5492, kZVariant, 0x546A).
unicode_unihan_variant(0x5499, kTraditionalVariant, 0x56A8).
unicode_unihan_variant(0x549B, kTraditionalVariant, 0x5680).
unicode_unihan_variant(0x549D, kTraditionalVariant, 0x565D).
unicode_unihan_variant(0x54A4, kSemanticVariant, 0x5412). %<kMatthews
unicode_unihan_variant(0x54A4, kTraditionalVariant, 0x5412).
unicode_unihan_variant(0x54A5, kSpecializedSemanticVariant, 0x563B). %<kMeyerWempe
unicode_unihan_variant(0x54A8, kSemanticVariant, 0x55DE). %<kMatthews,kMeyerWempe 0x8AEE<kMatthews,kMeyerWempe
unicode_unihan_variant(0x54A8, kZVariant, 0x8C18).
unicode_unihan_variant(0x54A9, kSemanticVariant, 0x54F6). %<kFenn
unicode_unihan_variant(0x54AC, kSemanticVariant, 0x9F69). %<kLau,kMatthews
unicode_unihan_variant(0x54AC, kZVariant, 0x9F69).
unicode_unihan_variant(0x54AE, kSpecializedSemanticVariant, 0x80C4). %<kMeyerWempe
unicode_unihan_variant(0x54AF, kZVariant, 0x8A7B).
unicode_unihan_variant(0x54B1, kSemanticVariant, 0x507A). %<kFenn
unicode_unihan_variant(0x54B1, kSpecializedSemanticVariant, 0x5592). %<kFenn
unicode_unihan_variant(0x54B2, kSemanticVariant, 0x7B11). %<kFenn
unicode_unihan_variant(0x54B2, kZVariant, 0x7B11).
unicode_unihan_variant(0x54B3, kSemanticVariant, 0x6B2C). %<kMatthews
unicode_unihan_variant(0x54B3, kSpecializedSemanticVariant, 0x6B2C). %<kFenn
unicode_unihan_variant(0x54B7, kSemanticVariant, 0x5555). %<kMatthews
unicode_unihan_variant(0x54B8, kTraditionalVariant, 0x9E79).
unicode_unihan_variant(0x54BC, kSimplifiedVariant, 0x5459).
unicode_unihan_variant(0x54BD, kSemanticVariant, 0x56A5). %<kMatthews
unicode_unihan_variant(0x54BD, kSpecializedSemanticVariant, 0x56A5). %<kFenn
unicode_unihan_variant(0x54BD, kZVariant, 0xF99E).
unicode_unihan_variant(0x54BF, kSemanticVariant, 0x541A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x54C2, kSemanticVariant, 0x5432). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x54C4, kZVariant, 0x95A7).
unicode_unihan_variant(0x54CC, kZVariant, 0x5471).
unicode_unihan_variant(0x54CD, kSemanticVariant, 0x97FF). %<kMatthews
unicode_unihan_variant(0x54CD, kTraditionalVariant, 0x97FF).
unicode_unihan_variant(0x54D1, kTraditionalVariant, 0x555E).
unicode_unihan_variant(0x54D2, kTraditionalVariant, 0x5660).
unicode_unihan_variant(0x54D3, kTraditionalVariant, 0x5635).
unicode_unihan_variant(0x54D4, kTraditionalVariant, 0x55F6).
unicode_unihan_variant(0x54D5, kTraditionalVariant, 0x5666).
unicode_unihan_variant(0x54D7, kTraditionalVariant, 0x5629).
unicode_unihan_variant(0x54D9, kTraditionalVariant, 0x5672).
unicode_unihan_variant(0x54DC, kTraditionalVariant, 0x568C).
unicode_unihan_variant(0x54DD, kTraditionalVariant, 0x5665).
unicode_unihan_variant(0x54DF, kTraditionalVariant, 0x55B2).
unicode_unihan_variant(0x54E1, kSimplifiedVariant, 0x5458).
unicode_unihan_variant(0x54E6, kSemanticVariant, 0x8A90). %<kMeyerWempe
unicode_unihan_variant(0x54EF, kSimplifiedVariant, 0x20BDF).
unicode_unihan_variant(0x54F2, kSemanticVariant, 0x5586). %<kLau 0x608A<kMatthews
unicode_unihan_variant(0x54F4, kSemanticVariant, 0x55A8). %<kMatthews
unicode_unihan_variant(0x54F6, kSemanticVariant, 0x54A9). %<kFenn
unicode_unihan_variant(0x54FC, kSpecializedSemanticVariant, 0x82DB). %<kFenn
unicode_unihan_variant(0x5501, kSemanticVariant, 0x55AD). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5504, kSimplifiedVariant, 0x5457).
unicode_unihan_variant(0x5505, kSpecializedSemanticVariant, 0x542B). %<kMeyerWempe
unicode_unihan_variant(0x5507, kSemanticVariant, 0x8123). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x5507, kZVariant, 0x8123).
unicode_unihan_variant(0x5509, kSemanticVariant, 0x6B38). %<kFenn
unicode_unihan_variant(0x5509, kSpecializedSemanticVariant, 0x6B38). %<kMeyerWempe
unicode_unihan_variant(0x5509, kZVariant, 0x8A92).
unicode_unihan_variant(0x550A, kSemanticVariant, 0x7864). %<kMeyerWempe
unicode_unihan_variant(0x5516, kZVariant, 0x555E).
unicode_unihan_variant(0x551A, kSemanticVariant, 0x5423). %<kMatthews
unicode_unihan_variant(0x551A, kSimplifiedVariant, 0x5423).
unicode_unihan_variant(0x551B, kTraditionalVariant, 0x561C).
unicode_unihan_variant(0x551D, kTraditionalVariant, 0x55CA).
unicode_unihan_variant(0x5520, kTraditionalVariant, 0x562E).
unicode_unihan_variant(0x5521, kTraditionalVariant, 0x5562).
unicode_unihan_variant(0x5522, kTraditionalVariant, 0x55E9).
unicode_unihan_variant(0x5524, kTraditionalVariant, 0x559A).
unicode_unihan_variant(0x5527, kZVariant, 0x559E).
unicode_unihan_variant(0x552B, kSemanticVariant, 0x541F). %<kFenn
unicode_unihan_variant(0x552C, kSemanticVariant, 0x8AD5). %<kMatthews 0x7307<kMeyerWempe 0x8653<kMeyerWempe
unicode_unihan_variant(0x5537, kZVariant, 0x5539).
unicode_unihan_variant(0x553E, kSemanticVariant, 0x6DB6). %<kMatthews
unicode_unihan_variant(0x553E, kSpecializedSemanticVariant, 0x6DB6). %<kFenn
unicode_unihan_variant(0x5543, kSemanticVariant, 0x9F66). %<kFenn
unicode_unihan_variant(0x554A, kSpecializedSemanticVariant, 0x963F). %<kFenn
unicode_unihan_variant(0x554D, kSemanticVariant, 0x5428). %<kMeyerWempe
unicode_unihan_variant(0x554E, kSemanticVariant, 0x5FE4). %<kMatthews
unicode_unihan_variant(0x554F, kSimplifiedVariant, 0x95EE).
unicode_unihan_variant(0x5553, kSemanticVariant, 0x555F). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5553, kZVariant, 0x555F).
unicode_unihan_variant(0x5554, kZVariant, 0x5553).
unicode_unihan_variant(0x5555, kSemanticVariant, 0x54B7). %<kMatthews
unicode_unihan_variant(0x5556, kSemanticVariant, 0x35D6). %<kMatthews 0x5649<kMatthews 0x5557<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5556, kZVariant, 0x5557).
unicode_unihan_variant(0x5557, kSemanticVariant, 0x35D6). %<kMatthews 0x5556<kMatthews,kMeyerWempe 0x5649<kMatthews
unicode_unihan_variant(0x5557, kZVariant, 0x5556).
unicode_unihan_variant(0x555C, kSemanticVariant, 0x6B3C). %<kMatthews 0x8AC1<kMeyerWempe 0x6B60<kMatthews
unicode_unihan_variant(0x555E, kSemanticVariant, 0x7602). %<kFenn
unicode_unihan_variant(0x555E, kSimplifiedVariant, 0x54D1).
unicode_unihan_variant(0x555E, kZVariant, 0x5516).
unicode_unihan_variant(0x555F, kSemanticVariant, 0x5553). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x555F, kSimplifiedVariant, 0x542F).
unicode_unihan_variant(0x5562, kSimplifiedVariant, 0x5521).
unicode_unihan_variant(0x5563, kSemanticVariant, 0x3605). %<kFenn
unicode_unihan_variant(0x5567, kTraditionalVariant, 0x5616).
unicode_unihan_variant(0x556C, kTraditionalVariant, 0x55C7).
unicode_unihan_variant(0x556D, kTraditionalVariant, 0x56C0).
unicode_unihan_variant(0x556E, kTraditionalVariant, 0x5699).
unicode_unihan_variant(0x556F, kTraditionalVariant, 0x5613).
unicode_unihan_variant(0x556F, kZVariant, 0x5613).
unicode_unihan_variant(0x5570, kTraditionalVariant, 0x56C9).
unicode_unihan_variant(0x5570, kZVariant, 0x56C9).
unicode_unihan_variant(0x5574, kTraditionalVariant, 0x563D).
unicode_unihan_variant(0x5578, kTraditionalVariant, 0x562F).
unicode_unihan_variant(0x557C, kSemanticVariant, 0x55C1). %<kLau,kMatthews
unicode_unihan_variant(0x5582, kSemanticVariant, 0x9927). %<kFenn
unicode_unihan_variant(0x5582, kSpecializedSemanticVariant, 0x9935). %<kMeyerWempe
unicode_unihan_variant(0x5583, kSemanticVariant, 0x8AF5). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5583, kZVariant, 0x5AD0).
unicode_unihan_variant(0x5584, kZVariant, 0x8B71).
unicode_unihan_variant(0x5586, kSemanticVariant, 0x54F2). %<kLau
unicode_unihan_variant(0x5587, kZVariant, 0xF90B).
unicode_unihan_variant(0x558E, kSimplifiedVariant, 0x359E).
unicode_unihan_variant(0x5590, kSemanticVariant, 0x90C1). %<kMeyerWempe
unicode_unihan_variant(0x5592, kSemanticVariant, 0x661D). %<kMatthews
unicode_unihan_variant(0x5592, kSpecializedSemanticVariant, 0x507A). %<kFenn 0x54B1<kFenn
unicode_unihan_variant(0x559A, kSimplifiedVariant, 0x5524).
unicode_unihan_variant(0x559C, kSemanticVariant, 0x6199).
unicode_unihan_variant(0x559E, kZVariant, 0x5527).
unicode_unihan_variant(0x55A7, kSemanticVariant, 0x8AE0). %<kMeyerWempe
unicode_unihan_variant(0x55A7, kZVariant, 0x5405).
unicode_unihan_variant(0x55A8, kSemanticVariant, 0x54F4). %<kMatthews
unicode_unihan_variant(0x55A9, kZVariant, 0x55BB).
unicode_unihan_variant(0x55AA, kSimplifiedVariant, 0x4E27).
unicode_unihan_variant(0x55AB, kSemanticVariant, 0x5403). %<kLau,kMatthews
unicode_unihan_variant(0x55AB, kZVariant, 0x5403).
unicode_unihan_variant(0x55AC, kSemanticVariant, 0x4E54). %<kFenn
unicode_unihan_variant(0x55AC, kSimplifiedVariant, 0x4E54).
unicode_unihan_variant(0x55AD, kSemanticVariant, 0x5501). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x55AE, kSemanticVariant, 0x5355). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x55AE, kSimplifiedVariant, 0x5355).
unicode_unihan_variant(0x55AE, kZVariant, 0x5358).
unicode_unihan_variant(0x55B0, kZVariant, 0x9910).
unicode_unihan_variant(0x55B2, kSimplifiedVariant, 0x54DF).
unicode_unihan_variant(0x55B6, kZVariant, 0x71DF).
unicode_unihan_variant(0x55B7, kTraditionalVariant, 0x5674).
unicode_unihan_variant(0x55BB, kZVariant, 0x55A9).
unicode_unihan_variant(0x55BD, kTraditionalVariant, 0x560D).
unicode_unihan_variant(0x55BE, kTraditionalVariant, 0x56B3).
unicode_unihan_variant(0x55BF, kSemanticVariant, 0x566A). %<kMatthews
unicode_unihan_variant(0x55C0, kZVariant, 0xFA0D).
unicode_unihan_variant(0x55C1, kSemanticVariant, 0x557C). %<kLau,kMatthews
unicode_unihan_variant(0x55C5, kSemanticVariant, 0x9F45). %<kMeyerWempe
unicode_unihan_variant(0x55C6, kSimplifiedVariant, 0x545B).
unicode_unihan_variant(0x55C7, kSimplifiedVariant, 0x556C).
unicode_unihan_variant(0x55C9, kSemanticVariant, 0x8186). %<kMatthews
unicode_unihan_variant(0x55CA, kSimplifiedVariant, 0x551D).
unicode_unihan_variant(0x55CE, kSimplifiedVariant, 0x5417).
unicode_unihan_variant(0x55D2, kSemanticVariant, 0x35F3). %<kLau
unicode_unihan_variant(0x55DA, kSimplifiedVariant, 0x545C).
unicode_unihan_variant(0x55DE, kSemanticVariant, 0x54A8). %<kMatthews,kMeyerWempe 0x8AEE<kMatthews,kMeyerWempe
unicode_unihan_variant(0x55E5, kSemanticVariant, 0x568E). %<kMatthews 0x5637<kMatthews
unicode_unihan_variant(0x55E9, kSimplifiedVariant, 0x5522).
unicode_unihan_variant(0x55EB, kTraditionalVariant, 0x56C1).
unicode_unihan_variant(0x55EC, kZVariant, 0x5475).
unicode_unihan_variant(0x55F3, kTraditionalVariant, 0x566F).
unicode_unihan_variant(0x55F6, kSimplifiedVariant, 0x54D4).
unicode_unihan_variant(0x55F9, kSimplifiedVariant, 0x2A84F).
unicode_unihan_variant(0x55FB, kSpecializedSemanticVariant, 0x8655). %<kMeyerWempe
unicode_unihan_variant(0x55FD, kSemanticVariant, 0x7636). %<kMatthews
unicode_unihan_variant(0x5606, kSemanticVariant, 0x6B4E). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x5606, kSimplifiedVariant, 0x53F9).
unicode_unihan_variant(0x5608, kSemanticVariant, 0x470A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x560D, kSemanticVariant, 0x8B31). %<kMeyerWempe
unicode_unihan_variant(0x560D, kSimplifiedVariant, 0x55BD).
unicode_unihan_variant(0x560E, kZVariant, 0x5C1C).
unicode_unihan_variant(0x5611, kZVariant, 0x547C).
unicode_unihan_variant(0x5613, kSimplifiedVariant, 0x556F).
unicode_unihan_variant(0x5613, kZVariant, 0x556F).
unicode_unihan_variant(0x5614, kSimplifiedVariant, 0x5455).
unicode_unihan_variant(0x5614, kSpecializedSemanticVariant, 0x616A). %<kFenn
unicode_unihan_variant(0x5616, kSimplifiedVariant, 0x5567).
unicode_unihan_variant(0x5617, kSemanticVariant, 0x751E). %<kMeyerWempe
unicode_unihan_variant(0x5617, kSimplifiedVariant, 0x5C1D).
unicode_unihan_variant(0x5618, kTraditionalVariant, 0x5653).
unicode_unihan_variant(0x561C, kSimplifiedVariant, 0x551B).
unicode_unihan_variant(0x5624, kTraditionalVariant, 0x56B6).
unicode_unihan_variant(0x5628, kZVariant, 0x562F).
unicode_unihan_variant(0x5629, kSemanticVariant, 0x8B41). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5629, kSimplifiedVariant, 0x54D7).
unicode_unihan_variant(0x5629, kZVariant, 0x8B41).
unicode_unihan_variant(0x562E, kSimplifiedVariant, 0x5520).
unicode_unihan_variant(0x562F, kSemanticVariant, 0x6B57). %<kMatthews
unicode_unihan_variant(0x562F, kSimplifiedVariant, 0x5578).
unicode_unihan_variant(0x562F, kZVariant, 0x5628).
unicode_unihan_variant(0x5630, kSimplifiedVariant, 0x53FD).
unicode_unihan_variant(0x5631, kSemanticVariant, 0x56D1). %<kMatthews
unicode_unihan_variant(0x5631, kTraditionalVariant, 0x56D1).
unicode_unihan_variant(0x5634, kSemanticVariant, 0x5480). %<kLau 0x89DC<kLau
unicode_unihan_variant(0x5634, kZVariant, 0x5480).
unicode_unihan_variant(0x5635, kSimplifiedVariant, 0x54D3).
unicode_unihan_variant(0x5637, kSemanticVariant, 0x55E5). %<kMatthews
unicode_unihan_variant(0x5638, kSimplifiedVariant, 0x5452).
unicode_unihan_variant(0x563B, kSpecializedSemanticVariant, 0x54A5). %<kMeyerWempe
unicode_unihan_variant(0x563D, kSimplifiedVariant, 0x5574).
unicode_unihan_variant(0x563F, kSemanticVariant, 0x9ED8). %<kFenn
unicode_unihan_variant(0x5641, kZVariant, 0x6076).
unicode_unihan_variant(0x5642, kSemanticVariant, 0x8B50). %<kMeyerWempe
unicode_unihan_variant(0x5645, kSimplifiedVariant, 0x20BE0).
unicode_unihan_variant(0x5645, kZVariant, 0x5655).
unicode_unihan_variant(0x5649, kSemanticVariant, 0x35D6). %<kMatthews 0x5556<kMatthews 0x5557<kMatthews
unicode_unihan_variant(0x564F, kSemanticVariant, 0x5438). %<kMatthews
unicode_unihan_variant(0x5650, kSemanticVariant, 0x5668). %<kMatthews
unicode_unihan_variant(0x5650, kZVariant, 0x5668).
unicode_unihan_variant(0x5651, kSemanticVariant, 0x568E). %<kFenn
unicode_unihan_variant(0x5653, kSimplifiedVariant, 0x5618).
unicode_unihan_variant(0x5655, kZVariant, 0x5645).
unicode_unihan_variant(0x565A, kSimplifiedVariant, 0x358A).
unicode_unihan_variant(0x565C, kTraditionalVariant, 0x5695).
unicode_unihan_variant(0x565D, kSimplifiedVariant, 0x549D).
unicode_unihan_variant(0x5660, kSimplifiedVariant, 0x54D2).
unicode_unihan_variant(0x5665, kSimplifiedVariant, 0x54DD).
unicode_unihan_variant(0x5666, kSemanticVariant, 0x27B28). %<kMeyerWempe
unicode_unihan_variant(0x5666, kSimplifiedVariant, 0x54D5).
unicode_unihan_variant(0x5668, kSemanticVariant, 0x5650). %<kMatthews
unicode_unihan_variant(0x5668, kZVariant, 0x5650).
unicode_unihan_variant(0x566A, kSemanticVariant, 0x55BF). %<kMatthews 0x8B5F<kLau
unicode_unihan_variant(0x566A, kZVariant, 0x8B5F).
unicode_unihan_variant(0x566F, kSimplifiedVariant, 0x55F3).
unicode_unihan_variant(0x5672, kSimplifiedVariant, 0x54D9).
unicode_unihan_variant(0x5674, kSemanticVariant, 0x5460). %<kMatthews 0x6B55<kMatthews
unicode_unihan_variant(0x5674, kSimplifiedVariant, 0x55B7).
unicode_unihan_variant(0x5678, kSimplifiedVariant, 0x5428).
unicode_unihan_variant(0x5679, kSimplifiedVariant, 0x5F53).
unicode_unihan_variant(0x5680, kSimplifiedVariant, 0x549B).
unicode_unihan_variant(0x5687, kSimplifiedVariant, 0x5413).
unicode_unihan_variant(0x568C, kSimplifiedVariant, 0x54DC).
unicode_unihan_variant(0x568E, kSemanticVariant, 0x55E5). %<kMatthews 0x5651<kFenn
unicode_unihan_variant(0x568F, kZVariant, 0x5694).
unicode_unihan_variant(0x5694, kZVariant, 0x568F).
unicode_unihan_variant(0x5695, kSimplifiedVariant, 0x565C).
unicode_unihan_variant(0x5699, kSimplifiedVariant, 0x556E).
unicode_unihan_variant(0x5699, kZVariant, 0x9F67).
unicode_unihan_variant(0x56A0, kZVariant, 0x700F).
unicode_unihan_variant(0x56A2, kZVariant, 0x56CA).
unicode_unihan_variant(0x56A3, kTraditionalVariant, 0x56C2).
unicode_unihan_variant(0x56A5, kSemanticVariant, 0x54BD). %<kMatthews
unicode_unihan_variant(0x56A5, kSpecializedSemanticVariant, 0x54BD). %<kFenn
unicode_unihan_variant(0x56A5, kZVariant, 0xF99E).
unicode_unihan_variant(0x56A6, kSimplifiedVariant, 0x5456).
unicode_unihan_variant(0x56A8, kSimplifiedVariant, 0x5499).
unicode_unihan_variant(0x56AC, kSemanticVariant, 0x9870). %<kMatthews
unicode_unihan_variant(0x56B2, kSimplifiedVariant, 0x4EB8).
unicode_unihan_variant(0x56B3, kSemanticVariant, 0x4FC8). %<kMatthews
unicode_unihan_variant(0x56B3, kSimplifiedVariant, 0x55BE).
unicode_unihan_variant(0x56B4, kSimplifiedVariant, 0x4E25).
unicode_unihan_variant(0x56B4, kZVariant, 0x53B3).
unicode_unihan_variant(0x56B5, kSemanticVariant, 0x995E). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x56B6, kSimplifiedVariant, 0x5624).
unicode_unihan_variant(0x56B9, kSemanticVariant, 0x21148). %<kMeyerWempe
unicode_unihan_variant(0x56BB, kZVariant, 0x56C2).
unicode_unihan_variant(0x56C0, kSimplifiedVariant, 0x556D).
unicode_unihan_variant(0x56C1, kSemanticVariant, 0x8B98). %<kMeyerWempe
unicode_unihan_variant(0x56C1, kSimplifiedVariant, 0x55EB).
unicode_unihan_variant(0x56C2, kSimplifiedVariant, 0x56A3).
unicode_unihan_variant(0x56C5, kSimplifiedVariant, 0x5181).
unicode_unihan_variant(0x56C8, kSimplifiedVariant, 0x5453).
unicode_unihan_variant(0x56C9, kSimplifiedVariant, 0x5570).
unicode_unihan_variant(0x56C9, kZVariant, 0x5570).
unicode_unihan_variant(0x56CA, kZVariant, 0x56A2).
unicode_unihan_variant(0x56CC, kZVariant, 0x8607).
unicode_unihan_variant(0x56D1, kSemanticVariant, 0x5631). %<kMatthews
unicode_unihan_variant(0x56D1, kSimplifiedVariant, 0x5631).
unicode_unihan_variant(0x56D3, kSemanticVariant, 0x9F67). %<kLau,kMatthews
unicode_unihan_variant(0x56D3, kSpecializedSemanticVariant, 0x9F67). %<kMeyerWempe
unicode_unihan_variant(0x56D3, kZVariant, 0x5699).
unicode_unihan_variant(0x56D8, kSemanticVariant, 0x56EC). %<kFenn 0x56DE<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x56D8, kZVariant, 0x56DE).
unicode_unihan_variant(0x56DB, kSemanticVariant, 0x8086). %<kLau
unicode_unihan_variant(0x56DB, kSpecializedSemanticVariant, 0x8086). %<kFenn
unicode_unihan_variant(0x56DE, kSemanticVariant, 0x56D8). %<kLau,kMatthews,kMeyerWempe 0x5EFB<kMeyerWempe 0x56EC<kLau,kMatthews
unicode_unihan_variant(0x56DE, kZVariant, 0x56EC).
unicode_unihan_variant(0x56DF, kSemanticVariant, 0x4ABF). %<kMatthews 0x9856<kMatthews
unicode_unihan_variant(0x56DF, kSpecializedSemanticVariant, 0x5016). %<kFenn 0x5E78<kFenn
unicode_unihan_variant(0x56E2, kTraditionalVariant, 0x5718).
unicode_unihan_variant(0x56E3, kZVariant, 0x5718).
unicode_unihan_variant(0x56E4, kSemanticVariant, 0x25AF1). %<kLau
unicode_unihan_variant(0x56EA, kSemanticVariant, 0x56F1). %<kHKGlyph
unicode_unihan_variant(0x56EA, kSimplifiedVariant, 0x56F1).
unicode_unihan_variant(0x56EC, kSemanticVariant, 0x56D8). %<kFenn 0x56DE<kLau,kMatthews
unicode_unihan_variant(0x56ED, kSemanticVariant, 0x5213). %<kMatthews
unicode_unihan_variant(0x56ED, kTraditionalVariant, 0x5712).
unicode_unihan_variant(0x56EE, kSemanticVariant, 0x21219). %<kFenn
unicode_unihan_variant(0x56EE, kSpecializedSemanticVariant, 0x8AE4). %<kMeyerWempe
unicode_unihan_variant(0x56EF, kSemanticVariant, 0x570B). %<kMeyerWempe
unicode_unihan_variant(0x56EF, kZVariant, 0x570B).
unicode_unihan_variant(0x56F0, kSemanticVariant, 0x774F). %<kFenn
unicode_unihan_variant(0x56F0, kTraditionalVariant, 0x774F).
unicode_unihan_variant(0x56F1, kSemanticVariant, 0x56EA). %<kHKGlyph
unicode_unihan_variant(0x56F1, kTraditionalVariant, 0x56EA).
unicode_unihan_variant(0x56F2, kZVariant, 0x570D).
unicode_unihan_variant(0x56F3, kZVariant, 0x5716).
unicode_unihan_variant(0x56F4, kTraditionalVariant, 0x570D).
unicode_unihan_variant(0x56F5, kTraditionalVariant, 0x5707).
unicode_unihan_variant(0x56F9, kZVariant, 0xF9A9).
unicode_unihan_variant(0x56FD, kTraditionalVariant, 0x570B).
unicode_unihan_variant(0x56FE, kTraditionalVariant, 0x5716).
unicode_unihan_variant(0x5700, kZVariant, 0x570B).
unicode_unihan_variant(0x5705, kSemanticVariant, 0x51FD). %<kMatthews 0x51FE<kMatthews
unicode_unihan_variant(0x5706, kTraditionalVariant, 0x5713).
unicode_unihan_variant(0x5707, kSimplifiedVariant, 0x56F5).
unicode_unihan_variant(0x5708, kZVariant, 0x570F).
unicode_unihan_variant(0x570B, kSemanticVariant, 0x56EF). %<kMeyerWempe
unicode_unihan_variant(0x570B, kSimplifiedVariant, 0x56FD).
unicode_unihan_variant(0x570B, kZVariant, 0x5700).
unicode_unihan_variant(0x570D, kSimplifiedVariant, 0x56F4).
unicode_unihan_variant(0x570D, kZVariant, 0x56F2).
unicode_unihan_variant(0x570E, kZVariant, 0x5713).
unicode_unihan_variant(0x570F, kZVariant, 0x5708).
unicode_unihan_variant(0x5712, kSimplifiedVariant, 0x56ED).
unicode_unihan_variant(0x5713, kSimplifiedVariant, 0x5706).
unicode_unihan_variant(0x5713, kZVariant, 0x570E).
unicode_unihan_variant(0x5715, kSemanticVariant, 0x5716). %<kMatthews
unicode_unihan_variant(0x5716, kSemanticVariant, 0x5715). %<kMatthews
unicode_unihan_variant(0x5716, kSimplifiedVariant, 0x56FE).
unicode_unihan_variant(0x5716, kZVariant, 0x56F3).
unicode_unihan_variant(0x5718, kSimplifiedVariant, 0x56E2).
unicode_unihan_variant(0x5718, kZVariant, 0x56E3).
unicode_unihan_variant(0x571D, kSemanticVariant, 0x571E). %<kXHC1983
unicode_unihan_variant(0x571E, kSemanticVariant, 0x571D). %<kXHC1983
unicode_unihan_variant(0x571E, kSimplifiedVariant, 0x2A8AE).
unicode_unihan_variant(0x5722, kSemanticVariant, 0x753A). %<kMeyerWempe
unicode_unihan_variant(0x5723, kSemanticVariant, 0x8056). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5723, kTraditionalVariant, 0x8056).
unicode_unihan_variant(0x5727, kZVariant, 0x58D3).
unicode_unihan_variant(0x572C, kSemanticVariant, 0x6747). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x572D, kSemanticVariant, 0x73EA). %<kMatthews
unicode_unihan_variant(0x5739, kTraditionalVariant, 0x58D9).
unicode_unihan_variant(0x573A, kTraditionalVariant, 0x5834).
unicode_unihan_variant(0x573B, kZVariant, 0x57A0).
unicode_unihan_variant(0x5740, kZVariant, 0x962F).
unicode_unihan_variant(0x5742, kTraditionalVariant, 0x962A).
unicode_unihan_variant(0x5746, kSemanticVariant, 0x58B3). %<kLau
unicode_unihan_variant(0x5746, kSpecializedSemanticVariant, 0x58B3).
unicode_unihan_variant(0x574B, kSemanticVariant, 0x574C). %<kMatthews
unicode_unihan_variant(0x574C, kSemanticVariant, 0x574B). %<kMatthews
unicode_unihan_variant(0x574E, kSemanticVariant, 0x57F3). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x574F, kSemanticVariant, 0x576F). %<kMatthews
unicode_unihan_variant(0x574F, kSpecializedSemanticVariant, 0x576F). %<kMeyerWempe
unicode_unihan_variant(0x574F, kTraditionalVariant, 0x58DE).
unicode_unihan_variant(0x5750, kSpecializedSemanticVariant, 0x5EA7). %<kMeyerWempe
unicode_unihan_variant(0x5750, kZVariant, 0x5EA7).
unicode_unihan_variant(0x5751, kSemanticVariant, 0x962C). %<kMatthews
unicode_unihan_variant(0x5751, kZVariant, 0x962C).
unicode_unihan_variant(0x5757, kTraditionalVariant, 0x584A).
unicode_unihan_variant(0x575A, kTraditionalVariant, 0x5805).
unicode_unihan_variant(0x575B, kSemanticVariant, 0x58C7). %<kMatthews
unicode_unihan_variant(0x575B, kTraditionalVariant, 0x58C7). %0x7F48
unicode_unihan_variant(0x575B, kZVariant, 0x58C7).
unicode_unihan_variant(0x575C, kTraditionalVariant, 0x58E2).
unicode_unihan_variant(0x575D, kTraditionalVariant, 0x58E9).
unicode_unihan_variant(0x575E, kTraditionalVariant, 0x5862).
unicode_unihan_variant(0x575F, kSemanticVariant, 0x58B3). %<kMatthews
unicode_unihan_variant(0x575F, kTraditionalVariant, 0x58B3).
unicode_unihan_variant(0x5760, kTraditionalVariant, 0x589C).
unicode_unihan_variant(0x5764, kSemanticVariant, 0x5803). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x576B, kZVariant, 0x5E97).
unicode_unihan_variant(0x576D, kSemanticVariant, 0x6CE5). %<kLau,kMatthews
unicode_unihan_variant(0x576D, kSpecializedSemanticVariant, 0x6CE5). %<kFenn
unicode_unihan_variant(0x576F, kSemanticVariant, 0x574F). %<kMatthews
unicode_unihan_variant(0x576F, kSpecializedSemanticVariant, 0x574F). %<kMeyerWempe
unicode_unihan_variant(0x576F, kZVariant, 0x58DE).
unicode_unihan_variant(0x5770, kSemanticVariant, 0x5182). %<kMatthews 0x518B<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5775, kSemanticVariant, 0x4E18). %<kMatthews
unicode_unihan_variant(0x5782, kZVariant, 0x57C0).
unicode_unihan_variant(0x5784, kTraditionalVariant, 0x58DF).
unicode_unihan_variant(0x5785, kTraditionalVariant, 0x58E0).
unicode_unihan_variant(0x5786, kTraditionalVariant, 0x58DA).
unicode_unihan_variant(0x5792, kTraditionalVariant, 0x58D8).
unicode_unihan_variant(0x5793, kZVariant, 0x9654).
unicode_unihan_variant(0x5794, kSemanticVariant, 0x967B). %<kMatthews,kMeyerWempe 0x5819<kMatthews,kMeyerWempe
unicode_unihan_variant(0x579B, kSemanticVariant, 0x579C). %<kFenn
unicode_unihan_variant(0x579B, kZVariant, 0x579C).
unicode_unihan_variant(0x579C, kSemanticVariant, 0x579B). %<kFenn
unicode_unihan_variant(0x57A0, kZVariant, 0x573B).
unicode_unihan_variant(0x57A6, kTraditionalVariant, 0x58BE).
unicode_unihan_variant(0x57A9, kTraditionalVariant, 0x580A).
unicode_unihan_variant(0x57A9, kZVariant, 0x8056).
unicode_unihan_variant(0x57AB, kTraditionalVariant, 0x588A).
unicode_unihan_variant(0x57AD, kTraditionalVariant, 0x57E1).
unicode_unihan_variant(0x57B1, kTraditionalVariant, 0x58CB).
unicode_unihan_variant(0x57B2, kTraditionalVariant, 0x584F).
unicode_unihan_variant(0x57B4, kTraditionalVariant, 0x5816).
unicode_unihan_variant(0x57B5, kSimplifiedVariant, 0x57EF).
unicode_unihan_variant(0x57BB, kSemanticVariant, 0x58E9). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x57BB, kZVariant, 0x58E9).
unicode_unihan_variant(0x57C0, kZVariant, 0x5782).
unicode_unihan_variant(0x57C7, kSemanticVariant, 0x752C). %<kFenn
unicode_unihan_variant(0x57D2, kZVariant, 0x57D3).
unicode_unihan_variant(0x57D8, kTraditionalVariant, 0x5852).
unicode_unihan_variant(0x57D9, kTraditionalVariant, 0x5864).
unicode_unihan_variant(0x57DA, kTraditionalVariant, 0x581D).
unicode_unihan_variant(0x57DC, kSemanticVariant, 0x91CE). %<kLau,kMatthews
unicode_unihan_variant(0x57DC, kZVariant, 0x91CE).
unicode_unihan_variant(0x57DF, kSemanticVariant, 0x3F63). %<kMatthews
unicode_unihan_variant(0x57DF, kSpecializedSemanticVariant, 0x8B7D). %<kFenn
unicode_unihan_variant(0x57E1, kSimplifiedVariant, 0x57AD).
unicode_unihan_variant(0x57EF, kTraditionalVariant, 0x57B5).
unicode_unihan_variant(0x57F0, kSimplifiedVariant, 0x91C7).
unicode_unihan_variant(0x57F3, kSemanticVariant, 0x574E). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x57F6, kSemanticVariant, 0x84FA). %<kMatthews 0x79C7<kMatthews 0x85DD<kMatthews
unicode_unihan_variant(0x57F7, kSemanticVariant, 0x6267). %<kFenn
unicode_unihan_variant(0x57F7, kSimplifiedVariant, 0x6267).
unicode_unihan_variant(0x57FA, kSpecializedSemanticVariant, 0x628D). %<kFenn 0x62EF<kFenn
unicode_unihan_variant(0x57FC, kZVariant, 0x5D0E).
unicode_unihan_variant(0x57FD, kSemanticVariant, 0x6383). %<kMeyerWempe
unicode_unihan_variant(0x5803, kSemanticVariant, 0x5764). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5805, kSimplifiedVariant, 0x575A).
unicode_unihan_variant(0x5806, kSpecializedSemanticVariant, 0x200A4). %<kFenn
unicode_unihan_variant(0x5807, kSemanticVariant, 0x83EB). %<kMatthews
unicode_unihan_variant(0x5807, kZVariant, 0x83EB).
unicode_unihan_variant(0x5808, kSemanticVariant, 0x7F38). %<kLau,kMatthews 0x7F41<kLau,kMatthews
unicode_unihan_variant(0x580A, kSimplifiedVariant, 0x57A9).
unicode_unihan_variant(0x580A, kZVariant, 0x8056).
unicode_unihan_variant(0x5811, kTraditionalVariant, 0x5879).
unicode_unihan_variant(0x5815, kTraditionalVariant, 0x58AE).
unicode_unihan_variant(0x5816, kSimplifiedVariant, 0x57B4).
unicode_unihan_variant(0x5818, kSemanticVariant, 0x584D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5819, kSemanticVariant, 0x5794). %<kMatthews,kMeyerWempe 0x967B<kMatthews,kMeyerWempe
unicode_unihan_variant(0x581D, kSimplifiedVariant, 0x57DA).
unicode_unihan_variant(0x5824, kSemanticVariant, 0x9684). %<kMeyerWempe
unicode_unihan_variant(0x5826, kSemanticVariant, 0x968E). %<kMatthews
unicode_unihan_variant(0x5826, kZVariant, 0x968E).
unicode_unihan_variant(0x582D, kSemanticVariant, 0x968D). %<kMatthews
unicode_unihan_variant(0x582F, kSimplifiedVariant, 0x5C27).
unicode_unihan_variant(0x5831, kSemanticVariant, 0x62A5). %<kMeyerWempe
unicode_unihan_variant(0x5831, kSimplifiedVariant, 0x62A5).
unicode_unihan_variant(0x5834, kSemanticVariant, 0x5872). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5834, kSimplifiedVariant, 0x573A).
unicode_unihan_variant(0x5834, kSpecializedSemanticVariant, 0x5872).
unicode_unihan_variant(0x5834, kZVariant, 0x5872).
unicode_unihan_variant(0x583A, kZVariant, 0x754C).
unicode_unihan_variant(0x583F, kSemanticVariant, 0x9E7C). %<kFenn 0x9E7B<kFenn
unicode_unihan_variant(0x5846, kZVariant, 0x58EA).
unicode_unihan_variant(0x5848, kZVariant, 0x588D).
unicode_unihan_variant(0x584A, kSimplifiedVariant, 0x5757).
unicode_unihan_variant(0x584A, kZVariant, 0x51F7).
unicode_unihan_variant(0x584B, kSimplifiedVariant, 0x8314).
unicode_unihan_variant(0x584D, kSemanticVariant, 0x5818). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x584F, kSimplifiedVariant, 0x57B2).
unicode_unihan_variant(0x5850, kSemanticVariant, 0x5851). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5851, kSemanticVariant, 0x5850). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5852, kSimplifiedVariant, 0x57D8).
unicode_unihan_variant(0x5854, kSemanticVariant, 0x366E). %<kMeyerWempe 0x5896<kFenn
unicode_unihan_variant(0x5854, kZVariant, 0x5896).
unicode_unihan_variant(0x5857, kSimplifiedVariant, 0x6D82).
unicode_unihan_variant(0x5857, kZVariant, 0x51C3).
unicode_unihan_variant(0x585A, kSpecializedSemanticVariant, 0x4E2D). %<kFenn
unicode_unihan_variant(0x585A, kZVariant, 0xFA10).
unicode_unihan_variant(0x585C, kZVariant, 0x5873).
unicode_unihan_variant(0x585E, kZVariant, 0xF96C).
unicode_unihan_variant(0x585F, kSemanticVariant, 0x846C). %<kMatthews
unicode_unihan_variant(0x585F, kZVariant, 0x846C).
unicode_unihan_variant(0x5861, kSemanticVariant, 0x7AB4). %<kMatthews
unicode_unihan_variant(0x5861, kZVariant, 0x586B).
unicode_unihan_variant(0x5862, kSemanticVariant, 0x969D). %<kLau 0x9696<kMatthews 0x9114<kMeyerWempe
unicode_unihan_variant(0x5862, kSimplifiedVariant, 0x575E).
unicode_unihan_variant(0x5864, kSemanticVariant, 0x58CE). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5864, kSimplifiedVariant, 0x57D9).
unicode_unihan_variant(0x5869, kSemanticVariant, 0x9E7D). %<kMatthews
unicode_unihan_variant(0x5869, kZVariant, 0x9E7D).
unicode_unihan_variant(0x586B, kZVariant, 0x5861).
unicode_unihan_variant(0x5872, kSemanticVariant, 0x5834). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5872, kSpecializedSemanticVariant, 0x5834).
unicode_unihan_variant(0x5873, kZVariant, 0x585C).
unicode_unihan_variant(0x5875, kSimplifiedVariant, 0x5C18).
unicode_unihan_variant(0x5879, kSemanticVariant, 0x58CD). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5879, kSimplifiedVariant, 0x5811).
unicode_unihan_variant(0x587E, kSpecializedSemanticVariant, 0x5B70). %<kMeyerWempe
unicode_unihan_variant(0x5888, kSemanticVariant, 0x78E1). %<kLau,kMatthews
unicode_unihan_variant(0x588A, kSimplifiedVariant, 0x57AB).
unicode_unihan_variant(0x588D, kZVariant, 0x5848).
unicode_unihan_variant(0x5896, kSemanticVariant, 0x5854). %<kFenn
unicode_unihan_variant(0x5896, kZVariant, 0x5854).
unicode_unihan_variant(0x5897, kZVariant, 0x589E).
unicode_unihan_variant(0x5899, kTraditionalVariant, 0x7246).
unicode_unihan_variant(0x5899, kZVariant, 0x7246).
unicode_unihan_variant(0x589C, kSimplifiedVariant, 0x5760).
unicode_unihan_variant(0x589D, kSemanticVariant, 0x78FD). %<kMatthews
unicode_unihan_variant(0x58A4, kSemanticVariant, 0x96A4). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x58AB, kSemanticVariant, 0x6A3D). %<kMatthews 0x7F47<kMatthews
unicode_unihan_variant(0x58AB, kZVariant, 0x6A3D).
unicode_unihan_variant(0x58AE, kSimplifiedVariant, 0x5815).
unicode_unihan_variant(0x58B0, kSemanticVariant, 0x7F48). %<kFenn 0x58DC<kMatthews
unicode_unihan_variant(0x58B0, kSpecializedSemanticVariant, 0x5081). %<kMeyerWempe
unicode_unihan_variant(0x58B3, kSemanticVariant, 0x5746). %<kLau 0x575F<kMatthews
unicode_unihan_variant(0x58B3, kSimplifiedVariant, 0x575F).
unicode_unihan_variant(0x58B3, kSpecializedSemanticVariant, 0x5746).
unicode_unihan_variant(0x58BB, kSemanticVariant, 0x5EE7). %<kLau,kMatthews 0x7246<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x58BB, kZVariant, 0x7246).
unicode_unihan_variant(0x58BE, kSimplifiedVariant, 0x57A6).
unicode_unihan_variant(0x58C7, kSemanticVariant, 0x575B). %<kMatthews
unicode_unihan_variant(0x58C7, kSimplifiedVariant, 0x575B).
unicode_unihan_variant(0x58C8, kSimplifiedVariant, 0x21484).
unicode_unihan_variant(0x58CA, kZVariant, 0x58DE).
unicode_unihan_variant(0x58CB, kSimplifiedVariant, 0x57B1).
unicode_unihan_variant(0x58CC, kZVariant, 0x58E4).
unicode_unihan_variant(0x58CD, kSemanticVariant, 0x5879). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x58CE, kSemanticVariant, 0x5864). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x58D0, kSemanticVariant, 0x74BD). %<kMatthews
unicode_unihan_variant(0x58D3, kSimplifiedVariant, 0x538B).
unicode_unihan_variant(0x58D3, kZVariant, 0x5727).
unicode_unihan_variant(0x58D8, kSimplifiedVariant, 0x5792).
unicode_unihan_variant(0x58D8, kZVariant, 0x5841).
unicode_unihan_variant(0x58D9, kSimplifiedVariant, 0x5739).
unicode_unihan_variant(0x58DA, kSimplifiedVariant, 0x5786).
unicode_unihan_variant(0x58DC, kSemanticVariant, 0x58B0). %<kMatthews 0x7F48<kMatthews
unicode_unihan_variant(0x58DE, kSimplifiedVariant, 0x574F).
unicode_unihan_variant(0x58DE, kZVariant, 0x58CA).
unicode_unihan_variant(0x58DF, kSimplifiedVariant, 0x5784).
unicode_unihan_variant(0x58DF, kZVariant, 0xF942).
unicode_unihan_variant(0x58E0, kSimplifiedVariant, 0x5785).
unicode_unihan_variant(0x58E2, kSimplifiedVariant, 0x575C).
unicode_unihan_variant(0x58E4, kZVariant, 0x58CC).
unicode_unihan_variant(0x58E9, kSemanticVariant, 0x57BB). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x58E9, kSimplifiedVariant, 0x575D).
unicode_unihan_variant(0x58EA, kZVariant, 0x5846).
unicode_unihan_variant(0x58EE, kTraditionalVariant, 0x58EF).
unicode_unihan_variant(0x58EF, kSimplifiedVariant, 0x58EE).
unicode_unihan_variant(0x58F0, kSemanticVariant, 0x8072). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x58F0, kTraditionalVariant, 0x8072).
unicode_unihan_variant(0x58F2, kZVariant, 0x8CE3).
unicode_unihan_variant(0x58F3, kSemanticVariant, 0x3C7F). %<kMatthews 0x6BBB<kFenn
unicode_unihan_variant(0x58F3, kSpecializedSemanticVariant, 0x6BBC). %<kFenn
unicode_unihan_variant(0x58F3, kTraditionalVariant, 0x6BBC).
unicode_unihan_variant(0x58F3, kZVariant, 0x6BBC).
unicode_unihan_variant(0x58F6, kTraditionalVariant, 0x58FA).
unicode_unihan_variant(0x58F7, kZVariant, 0x58FA).
unicode_unihan_variant(0x58F8, kTraditionalVariant, 0x58FC).
unicode_unihan_variant(0x58F9, kSemanticVariant, 0x4E00). %<kLau,kMatthews,kMeyerWempe 0x5F0C<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x58F9, kSpecializedSemanticVariant, 0x4E00).
unicode_unihan_variant(0x58F9, kZVariant, 0x640B).
unicode_unihan_variant(0x58FA, kSimplifiedVariant, 0x58F6).
unicode_unihan_variant(0x58FA, kZVariant, 0x58F7).
unicode_unihan_variant(0x58FB, kSemanticVariant, 0x5A7F). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x58FC, kSimplifiedVariant, 0x58F8).
unicode_unihan_variant(0x58FD, kSemanticVariant, 0x5BFF). %<kFenn 0x5900<kLau
unicode_unihan_variant(0x58FD, kSimplifiedVariant, 0x5BFF).
unicode_unihan_variant(0x5900, kSemanticVariant, 0x58FD). %<kLau 0x5BFF<kFenn
unicode_unihan_variant(0x5900, kZVariant, 0x58FD).
unicode_unihan_variant(0x5904, kTraditionalVariant, 0x8655).
unicode_unihan_variant(0x5905, kSemanticVariant, 0x964D). %<kMatthews
unicode_unihan_variant(0x5905, kSpecializedSemanticVariant, 0x964D). %<kFenn
unicode_unihan_variant(0x5907, kTraditionalVariant, 0x5099).
unicode_unihan_variant(0x5909, kSemanticVariant, 0x8B8A). %<kHanyu:T
unicode_unihan_variant(0x590A, kZVariant, 0x6535).
unicode_unihan_variant(0x590D, kTraditionalVariant, 0x5FA9). %0x8907 0x8986
unicode_unihan_variant(0x590D, kZVariant, 0x5FA9).
unicode_unihan_variant(0x5910, kZVariant, 0x657B).
unicode_unihan_variant(0x5916, kSpecializedSemanticVariant, 0x8200). %<kFenn
unicode_unihan_variant(0x5918, kZVariant, 0x536F).
unicode_unihan_variant(0x591A, kZVariant, 0x591B).
unicode_unihan_variant(0x591B, kZVariant, 0x591A).
unicode_unihan_variant(0x591C, kZVariant, 0x4EB1).
unicode_unihan_variant(0x591F, kSemanticVariant, 0x5F40). %<kFenn 0x5920<kLau,kMatthews
unicode_unihan_variant(0x591F, kTraditionalVariant, 0x5920).
unicode_unihan_variant(0x5920, kSemanticVariant, 0x591F). %<kLau,kMatthews 0x5F40<kFenn
unicode_unihan_variant(0x5920, kSimplifiedVariant, 0x591F).
unicode_unihan_variant(0x5922, kSemanticVariant, 0x68A6). %<kLau,kMeyerWempe 0x5923<kMatthews
unicode_unihan_variant(0x5922, kSimplifiedVariant, 0x68A6).
unicode_unihan_variant(0x5923, kSemanticVariant, 0x5922). %<kMatthews
unicode_unihan_variant(0x5925, kZVariant, 0x4F19).
unicode_unihan_variant(0x5929, kSemanticVariant, 0x975D). %<kMatthews
unicode_unihan_variant(0x592D, kZVariant, 0x6B80).
unicode_unihan_variant(0x592F, kSemanticVariant, 0x40AB). %<kFenn
unicode_unihan_variant(0x5934, kTraditionalVariant, 0x982D).
unicode_unihan_variant(0x5938, kSemanticVariant, 0x8A87). %<kMeyerWempe
unicode_unihan_variant(0x5938, kTraditionalVariant, 0x8A87).
unicode_unihan_variant(0x5939, kTraditionalVariant, 0x593E).
unicode_unihan_variant(0x593A, kTraditionalVariant, 0x596A).
unicode_unihan_variant(0x593E, kSimplifiedVariant, 0x5939).
unicode_unihan_variant(0x5941, kTraditionalVariant, 0x5969).
unicode_unihan_variant(0x5942, kTraditionalVariant, 0x5950).
unicode_unihan_variant(0x5947, kSemanticVariant, 0x7AD2). %<kLau,kMatthews
unicode_unihan_variant(0x5947, kZVariant, 0x7AD2).
unicode_unihan_variant(0x5948, kSemanticVariant, 0x67F0). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5948, kZVariant, 0x67F0).
unicode_unihan_variant(0x594B, kTraditionalVariant, 0x596E).
unicode_unihan_variant(0x594C, kZVariant, 0x9EDE).
unicode_unihan_variant(0x594E, kSemanticVariant, 0x369D). %<kMatthews 0x595E<kMatthews
unicode_unihan_variant(0x5950, kSimplifiedVariant, 0x5942).
unicode_unihan_variant(0x5951, kZVariant, 0xF909).
unicode_unihan_variant(0x5954, kSemanticVariant, 0x7287). %<kMatthews
unicode_unihan_variant(0x5954, kSpecializedSemanticVariant, 0x7287). %<kFenn
unicode_unihan_variant(0x5954, kZVariant, 0x7287).
unicode_unihan_variant(0x5955, kSemanticVariant, 0x5F08). %<kFenn
unicode_unihan_variant(0x5956, kTraditionalVariant, 0x734E).
unicode_unihan_variant(0x5956, kZVariant, 0x596C).
unicode_unihan_variant(0x5958, kZVariant, 0x5F09).
unicode_unihan_variant(0x595E, kSemanticVariant, 0x369D). %<kMatthews 0x594E<kMatthews
unicode_unihan_variant(0x5965, kTraditionalVariant, 0x5967).
unicode_unihan_variant(0x5967, kSimplifiedVariant, 0x5965).
unicode_unihan_variant(0x5968, kZVariant, 0x596C).
unicode_unihan_variant(0x5969, kSemanticVariant, 0x5333). %<kFenn
unicode_unihan_variant(0x5969, kSimplifiedVariant, 0x5941).
unicode_unihan_variant(0x5969, kZVariant, 0x5333).
unicode_unihan_variant(0x596A, kSimplifiedVariant, 0x593A).
unicode_unihan_variant(0x596E, kSimplifiedVariant, 0x594B).
unicode_unihan_variant(0x5973, kZVariant, 0xF981).
unicode_unihan_variant(0x5976, kSemanticVariant, 0x59B3). %<kFenn 0x5B2D<kMatthews
unicode_unihan_variant(0x5976, kSpecializedSemanticVariant, 0x5B2D). %<kMeyerWempe
unicode_unihan_variant(0x5976, kZVariant, 0x5B2D).
unicode_unihan_variant(0x5978, kTraditionalVariant, 0x59E6).
unicode_unihan_variant(0x5979, kSemanticVariant, 0x4ED6). %<kMatthews
unicode_unihan_variant(0x597C, kSimplifiedVariant, 0x59F9).
unicode_unihan_variant(0x5986, kTraditionalVariant, 0x599D).
unicode_unihan_variant(0x5987, kTraditionalVariant, 0x5A66).
unicode_unihan_variant(0x5988, kTraditionalVariant, 0x5ABD).
unicode_unihan_variant(0x598A, kSemanticVariant, 0x59D9). %<kMatthews
unicode_unihan_variant(0x598A, kZVariant, 0x59D9).
unicode_unihan_variant(0x598D, kZVariant, 0x59F8).
unicode_unihan_variant(0x5992, kSemanticVariant, 0x59AC). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5992, kZVariant, 0x59AC).
unicode_unihan_variant(0x5999, kSemanticVariant, 0x7385). %<kLau,kMatthews
unicode_unihan_variant(0x5999, kZVariant, 0x7385).
unicode_unihan_variant(0x599D, kSemanticVariant, 0x7CA7). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x599D, kSimplifiedVariant, 0x5986).
unicode_unihan_variant(0x59A4, kSemanticVariant, 0x4F03). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x59A9, kTraditionalVariant, 0x5AF5).
unicode_unihan_variant(0x59AA, kTraditionalVariant, 0x5AD7).
unicode_unihan_variant(0x59AB, kTraditionalVariant, 0x5AAF).
unicode_unihan_variant(0x59AB, kZVariant, 0x5AAF).
unicode_unihan_variant(0x59AC, kSemanticVariant, 0x5992). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x59AC, kZVariant, 0x5992).
unicode_unihan_variant(0x59B3, kSemanticVariant, 0x5976). %<kFenn 0x5B2D<kMatthews
unicode_unihan_variant(0x59B3, kZVariant, 0x4F60).
unicode_unihan_variant(0x59C6, kSemanticVariant, 0x59E5). %<kMatthews
unicode_unihan_variant(0x59C6, kSpecializedSemanticVariant, 0x4F6C). %<kFenn 0x59E5<kFenn
unicode_unihan_variant(0x59C9, kSemanticVariant, 0x59CA). %<kFenn
unicode_unihan_variant(0x59C9, kZVariant, 0x59CA).
unicode_unihan_variant(0x59CA, kSemanticVariant, 0x59C9). %<kFenn
unicode_unihan_variant(0x59CA, kZVariant, 0x59C9).
unicode_unihan_variant(0x59CD, kSemanticVariant, 0x59D7). %<kMatthews
unicode_unihan_variant(0x59CD, kSimplifiedVariant, 0x59D7).
unicode_unihan_variant(0x59D7, kSemanticVariant, 0x59CD). %<kMatthews
unicode_unihan_variant(0x59D7, kTraditionalVariant, 0x59CD).
unicode_unihan_variant(0x59D9, kSemanticVariant, 0x598A). %<kMatthews
unicode_unihan_variant(0x59DC, kZVariant, 0x8591).
unicode_unihan_variant(0x59E2, kZVariant, 0x5A1F).
unicode_unihan_variant(0x59E3, kSemanticVariant, 0x36A3). %<kMatthews
unicode_unihan_variant(0x59E5, kSemanticVariant, 0x59C6). %<kMatthews
unicode_unihan_variant(0x59E5, kSpecializedSemanticVariant, 0x59C6). %<kFenn
unicode_unihan_variant(0x59E6, kSimplifiedVariant, 0x5978).
unicode_unihan_variant(0x59EA, kZVariant, 0x4F84).
unicode_unihan_variant(0x59EB, kZVariant, 0x59EC).
unicode_unihan_variant(0x59EC, kZVariant, 0x59EB).
unicode_unihan_variant(0x59F8, kZVariant, 0x598D).
unicode_unihan_variant(0x59F9, kTraditionalVariant, 0x597C).
unicode_unihan_variant(0x59FB, kSemanticVariant, 0x5A63). %<kMatthews
unicode_unihan_variant(0x59FB, kZVariant, 0x5A63).
unicode_unihan_variant(0x5A04, kTraditionalVariant, 0x5A41).
unicode_unihan_variant(0x5A05, kTraditionalVariant, 0x5A6D).
unicode_unihan_variant(0x5A06, kTraditionalVariant, 0x5B08).
unicode_unihan_variant(0x5A07, kTraditionalVariant, 0x5B0C).
unicode_unihan_variant(0x5A08, kTraditionalVariant, 0x5B4C).
unicode_unihan_variant(0x5A18, kSemanticVariant, 0x5B43). %<kLau
unicode_unihan_variant(0x5A18, kZVariant, 0x5B43).
unicode_unihan_variant(0x5A1A, kZVariant, 0x5583).
unicode_unihan_variant(0x5A1B, kSimplifiedVariant, 0x5A31).
unicode_unihan_variant(0x5A1F, kZVariant, 0x59E2).
unicode_unihan_variant(0x5A20, kSemanticVariant, 0x36DB). %<kMatthews
unicode_unihan_variant(0x5A20, kSpecializedSemanticVariant, 0x36DB). %<kFenn 0x7156<kFenn
unicode_unihan_variant(0x5A29, kSemanticVariant, 0x3743). %<kMeyerWempe
unicode_unihan_variant(0x5A2C, kSemanticVariant, 0x5AF5). %<kMatthews
unicode_unihan_variant(0x5A2F, kZVariant, 0x5A1B).
unicode_unihan_variant(0x5A31, kTraditionalVariant, 0x5A1B).
unicode_unihan_variant(0x5A32, kTraditionalVariant, 0x5AA7).
unicode_unihan_variant(0x5A34, kTraditionalVariant, 0x5AFB).
unicode_unihan_variant(0x5A3F, kZVariant, 0x5A40).
unicode_unihan_variant(0x5A40, kZVariant, 0x5A3F).
unicode_unihan_variant(0x5A41, kSimplifiedVariant, 0x5A04).
unicode_unihan_variant(0x5A63, kSemanticVariant, 0x59FB). %<kMatthews
unicode_unihan_variant(0x5A63, kZVariant, 0x59FB).
unicode_unihan_variant(0x5A66, kSemanticVariant, 0x5A8D). %<kFenn
unicode_unihan_variant(0x5A66, kSimplifiedVariant, 0x5987).
unicode_unihan_variant(0x5A6C, kSemanticVariant, 0x6DEB). %<kMeyerWempe
unicode_unihan_variant(0x5A6C, kZVariant, 0x6DEB).
unicode_unihan_variant(0x5A6D, kSimplifiedVariant, 0x5A05).
unicode_unihan_variant(0x5A73, kTraditionalVariant, 0x5AFF).
unicode_unihan_variant(0x5A74, kTraditionalVariant, 0x5B30).
unicode_unihan_variant(0x5A75, kTraditionalVariant, 0x5B0B).
unicode_unihan_variant(0x5A76, kTraditionalVariant, 0x5B38).
unicode_unihan_variant(0x5A7E, kZVariant, 0x5AAE).
unicode_unihan_variant(0x5A7F, kSemanticVariant, 0x58FB). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5A7F, kZVariant, 0x805F).
unicode_unihan_variant(0x5A86, kSemanticVariant, 0x5AE9). %<kCowles
unicode_unihan_variant(0x5A86, kSpecializedSemanticVariant, 0x5AF0). %<kFenn
unicode_unihan_variant(0x5A8D, kSemanticVariant, 0x5A66). %<kFenn
unicode_unihan_variant(0x5AA7, kSimplifiedVariant, 0x5A32).
unicode_unihan_variant(0x5AAA, kTraditionalVariant, 0x5ABC).
unicode_unihan_variant(0x5AAD, kTraditionalVariant, 0x5B03).
unicode_unihan_variant(0x5AAF, kSimplifiedVariant, 0x59AB).
unicode_unihan_variant(0x5AAF, kZVariant, 0x5B00).
unicode_unihan_variant(0x5AB0, kSimplifiedVariant, 0x36C0).
unicode_unihan_variant(0x5ABC, kSimplifiedVariant, 0x5AAA).
unicode_unihan_variant(0x5ABD, kSimplifiedVariant, 0x5988).
unicode_unihan_variant(0x5ABF, kSemanticVariant, 0x6127). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5ABF, kZVariant, 0x6127).
unicode_unihan_variant(0x5AC9, kSemanticVariant, 0x21731). %<kFenn
unicode_unihan_variant(0x5ACB, kZVariant, 0x5B1D).
unicode_unihan_variant(0x5AD0, kZVariant, 0x5583).
unicode_unihan_variant(0x5AD2, kTraditionalVariant, 0x5B21).
unicode_unihan_variant(0x5AD4, kTraditionalVariant, 0x5B2A).
unicode_unihan_variant(0x5AD5, kSemanticVariant, 0x5ADB). %<kMatthews
unicode_unihan_variant(0x5AD7, kSimplifiedVariant, 0x59AA).
unicode_unihan_variant(0x5ADB, kSemanticVariant, 0x5AD5). %<kMatthews
unicode_unihan_variant(0x5AE9, kSemanticVariant, 0x5A86). %<kCowles 0x217BE<kMeyerWempe
unicode_unihan_variant(0x5AE9, kSpecializedSemanticVariant, 0x5AF0). %<kFenn
unicode_unihan_variant(0x5AEB, kSemanticVariant, 0x5B37). %<kMeyerWempe
unicode_unihan_variant(0x5AF0, kSpecializedSemanticVariant, 0x5A86). %<kFenn 0x5AE9<kFenn
unicode_unihan_variant(0x5AF1, kTraditionalVariant, 0x5B19).
unicode_unihan_variant(0x5AF5, kSemanticVariant, 0x5A2C). %<kMatthews
unicode_unihan_variant(0x5AF5, kSimplifiedVariant, 0x59A9).
unicode_unihan_variant(0x5AFA, kSemanticVariant, 0x5AFB). %<kHKGlyph,kMatthews
unicode_unihan_variant(0x5AFA, kZVariant, 0x5AFB).
unicode_unihan_variant(0x5AFB, kSemanticVariant, 0x5AFA). %<kHKGlyph,kMatthews
unicode_unihan_variant(0x5AFB, kSimplifiedVariant, 0x5A34).
unicode_unihan_variant(0x5AFF, kSimplifiedVariant, 0x5A73).
unicode_unihan_variant(0x5B00, kZVariant, 0x5AAF).
unicode_unihan_variant(0x5B03, kSimplifiedVariant, 0x5AAD).
unicode_unihan_variant(0x5B08, kSimplifiedVariant, 0x5A06).
unicode_unihan_variant(0x5B0B, kSimplifiedVariant, 0x5A75).
unicode_unihan_variant(0x5B0C, kSemanticVariant, 0x649F). %<kMatthews
unicode_unihan_variant(0x5B0C, kSimplifiedVariant, 0x5A07).
unicode_unihan_variant(0x5B19, kSimplifiedVariant, 0x5AF1).
unicode_unihan_variant(0x5B1D, kZVariant, 0x88CA).
unicode_unihan_variant(0x5B21, kSimplifiedVariant, 0x5AD2).
unicode_unihan_variant(0x5B22, kZVariant, 0x5A18).
unicode_unihan_variant(0x5B24, kSimplifiedVariant, 0x5B37).
unicode_unihan_variant(0x5B2A, kSimplifiedVariant, 0x5AD4).
unicode_unihan_variant(0x5B2D, kSemanticVariant, 0x5976). %<kMatthews 0x59B3<kMatthews
unicode_unihan_variant(0x5B2D, kSpecializedSemanticVariant, 0x5976). %<kMeyerWempe
unicode_unihan_variant(0x5B30, kSemanticVariant, 0x5B7E). %<kMatthews
unicode_unihan_variant(0x5B30, kSimplifiedVariant, 0x5A74).
unicode_unihan_variant(0x5B32, kSpecializedSemanticVariant, 0x60F1). %<kMeyerWempe
unicode_unihan_variant(0x5B37, kSemanticVariant, 0x5AEB). %<kMeyerWempe
unicode_unihan_variant(0x5B37, kTraditionalVariant, 0x5B24).
unicode_unihan_variant(0x5B38, kSimplifiedVariant, 0x5A76).
unicode_unihan_variant(0x5B3E, kZVariant, 0x61F6).
unicode_unihan_variant(0x5B40, kSemanticVariant, 0x971C). %<kLau
unicode_unihan_variant(0x5B43, kSemanticVariant, 0x5A18). %<kLau
unicode_unihan_variant(0x5B43, kZVariant, 0x5A18).
unicode_unihan_variant(0x5B4B, kSimplifiedVariant, 0x36E4).
unicode_unihan_variant(0x5B4C, kSimplifiedVariant, 0x5A08).
unicode_unihan_variant(0x5B50, kSemanticVariant, 0x53EA). %<kLau
unicode_unihan_variant(0x5B59, kTraditionalVariant, 0x5B6B).
unicode_unihan_variant(0x5B5A, kZVariant, 0x5B75).
unicode_unihan_variant(0x5B5B, kSpecializedSemanticVariant, 0x67CF). %<kFenn 0x6822<kFenn
unicode_unihan_variant(0x5B66, kTraditionalVariant, 0x5B78).
unicode_unihan_variant(0x5B6A, kTraditionalVariant, 0x5B7F).
unicode_unihan_variant(0x5B6B, kSimplifiedVariant, 0x5B59).
unicode_unihan_variant(0x5B6D, kSemanticVariant, 0x27D2F). %<kMeyerWempe
unicode_unihan_variant(0x5B70, kSpecializedSemanticVariant, 0x587E). %<kMeyerWempe
unicode_unihan_variant(0x5B73, kZVariant, 0x5B76).
unicode_unihan_variant(0x5B75, kZVariant, 0x5B5A).
unicode_unihan_variant(0x5B76, kZVariant, 0x5B73).
unicode_unihan_variant(0x5B78, kSemanticVariant, 0x6588). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5B78, kSimplifiedVariant, 0x5B66).
unicode_unihan_variant(0x5B7C, kSemanticVariant, 0x5B7D). %<kMatthews
unicode_unihan_variant(0x5B7C, kZVariant, 0x5B7D).
unicode_unihan_variant(0x5B7D, kSemanticVariant, 0x5B7C). %<kMatthews
unicode_unihan_variant(0x5B7D, kZVariant, 0x5B7C).
unicode_unihan_variant(0x5B7E, kSemanticVariant, 0x5B30). %<kMatthews
unicode_unihan_variant(0x5B7F, kSimplifiedVariant, 0x5B6A).
unicode_unihan_variant(0x5B81, kTraditionalVariant, 0x5BE7).
unicode_unihan_variant(0x5B82, kSemanticVariant, 0x5197). %<kMatthews
unicode_unihan_variant(0x5B82, kZVariant, 0x5197).
unicode_unihan_variant(0x5B83, kSemanticVariant, 0x4F57). %<kMatthews 0x7260<kLau
unicode_unihan_variant(0x5B83, kSpecializedSemanticVariant, 0x7260).
unicode_unihan_variant(0x5B83, kZVariant, 0x7260).
unicode_unihan_variant(0x5B85, kZVariant, 0xFA04).
unicode_unihan_variant(0x5B8C, kSemanticVariant, 0x70DF). %<kLau 0x7159<kLau
unicode_unihan_variant(0x5B8D, kZVariant, 0x8089).
unicode_unihan_variant(0x5B90, kSemanticVariant, 0x5B9C). %<kMatthews
unicode_unihan_variant(0x5B9C, kSemanticVariant, 0x5B90). %<kMatthews
unicode_unihan_variant(0x5B9C, kZVariant, 0x519D).
unicode_unihan_variant(0x5B9D, kSemanticVariant, 0x5BF3). %<kFenn 0x5BF6<kMeyerWempe
unicode_unihan_variant(0x5B9D, kTraditionalVariant, 0x5BF6).
unicode_unihan_variant(0x5B9E, kTraditionalVariant, 0x5BE6).
unicode_unihan_variant(0x5B9F, kZVariant, 0x5BE6).
unicode_unihan_variant(0x5BA0, kTraditionalVariant, 0x5BF5).
unicode_unihan_variant(0x5BA1, kTraditionalVariant, 0x5BE9).
unicode_unihan_variant(0x5BAA, kTraditionalVariant, 0x61B2).
unicode_unihan_variant(0x5BAB, kTraditionalVariant, 0x5BAE).
unicode_unihan_variant(0x5BAE, kSimplifiedVariant, 0x5BAB).
unicode_unihan_variant(0x5BB4, kSemanticVariant, 0x8B8C). %<kLau,kMatthews
unicode_unihan_variant(0x5BB6, kZVariant, 0x50A2).
unicode_unihan_variant(0x5BBB, kSemanticVariant, 0x5BC6). %<kMatthews
unicode_unihan_variant(0x5BBB, kZVariant, 0x5BC6).
unicode_unihan_variant(0x5BBC, kSemanticVariant, 0x5BC7). %<kMatthews
unicode_unihan_variant(0x5BBD, kTraditionalVariant, 0x5BEC).
unicode_unihan_variant(0x5BBE, kTraditionalVariant, 0x8CD3).
unicode_unihan_variant(0x5BBF, kSemanticVariant, 0x375B). %<kMatthews
unicode_unihan_variant(0x5BC3, kSemanticVariant, 0x51A4). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5BC3, kZVariant, 0x51A4).
unicode_unihan_variant(0x5BC6, kSemanticVariant, 0x5BBB). %<kMatthews
unicode_unihan_variant(0x5BC6, kZVariant, 0x5BBB).
unicode_unihan_variant(0x5BC7, kSemanticVariant, 0x5BBC). %<kMatthews
unicode_unihan_variant(0x5BCC, kZVariant, 0x51A8).
unicode_unihan_variant(0x5BCD, kSemanticVariant, 0x5BE7). %<kMatthews
unicode_unihan_variant(0x5BD3, kZVariant, 0x5EBD).
unicode_unihan_variant(0x5BD4, kSemanticVariant, 0x5BE6). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5BD5, kSemanticVariant, 0x5BDC). %<kFenn
unicode_unihan_variant(0x5BD5, kZVariant, 0x5BE7).
unicode_unihan_variant(0x5BD7, kSemanticVariant, 0x752F). %<kMatthews
unicode_unihan_variant(0x5BD8, kSemanticVariant, 0x7F6E). %<kMatthews
unicode_unihan_variant(0x5BD8, kZVariant, 0x7F6E).
unicode_unihan_variant(0x5BDA, kSemanticVariant, 0x5B9D). %<kMatthews 0x5BF3<kMatthews
unicode_unihan_variant(0x5BDB, kZVariant, 0x5BEC).
unicode_unihan_variant(0x5BDC, kSemanticVariant, 0x5BD5). %<kFenn
unicode_unihan_variant(0x5BDC, kZVariant, 0x5BE7).
unicode_unihan_variant(0x5BDD, kTraditionalVariant, 0x5BE2).
unicode_unihan_variant(0x5BDF, kSemanticVariant, 0x8A67). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5BDF, kZVariant, 0x8A67).
unicode_unihan_variant(0x5BE0, kSemanticVariant, 0x7AB6). %<kMatthews,kMeyerWempe,kPhonetic
unicode_unihan_variant(0x5BE0, kSpecializedSemanticVariant, 0x7AB6). %<kMeyerWempe
unicode_unihan_variant(0x5BE1, kSemanticVariant, 0x95DC). %<kLau
unicode_unihan_variant(0x5BE2, kSimplifiedVariant, 0x5BDD).
unicode_unihan_variant(0x5BE6, kSemanticVariant, 0x5BD4). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5BE6, kSimplifiedVariant, 0x5B9E).
unicode_unihan_variant(0x5BE6, kZVariant, 0x5B9F).
unicode_unihan_variant(0x5BE7, kSemanticVariant, 0x5BCD). %<kMatthews 0x21A34<kMeyerWempe
unicode_unihan_variant(0x5BE7, kSimplifiedVariant, 0x5B81).
unicode_unihan_variant(0x5BE8, kSemanticVariant, 0x7826). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5BE9, kSimplifiedVariant, 0x5BA1).
unicode_unihan_variant(0x5BEB, kSemanticVariant, 0x51A9). %<kLau,kMatthews
unicode_unihan_variant(0x5BEB, kSimplifiedVariant, 0x5199).
unicode_unihan_variant(0x5BEC, kSimplifiedVariant, 0x5BBD).
unicode_unihan_variant(0x5BEC, kZVariant, 0x5BDB).
unicode_unihan_variant(0x5BEE, kZVariant, 0xF9BC).
unicode_unihan_variant(0x5BF3, kSemanticVariant, 0x5B9D). %<kFenn 0x5BF6<kFenn
unicode_unihan_variant(0x5BF3, kZVariant, 0x5BF6).
unicode_unihan_variant(0x5BF5, kSemanticVariant, 0x205A5). %<kFenn
unicode_unihan_variant(0x5BF5, kSimplifiedVariant, 0x5BA0).
unicode_unihan_variant(0x5BF6, kSemanticVariant, 0x5B9D). %<kMeyerWempe 0x5BF3<kFenn
unicode_unihan_variant(0x5BF6, kSimplifiedVariant, 0x5B9D).
unicode_unihan_variant(0x5BF8, kSpecializedSemanticVariant, 0x540B).
unicode_unihan_variant(0x5BF9, kSemanticVariant, 0x5C0D). %<kMatthews
unicode_unihan_variant(0x5BF9, kSpecializedSemanticVariant, 0x5C0D). %<kMeyerWempe
unicode_unihan_variant(0x5BF9, kTraditionalVariant, 0x5C0D).
unicode_unihan_variant(0x5BFB, kTraditionalVariant, 0x5C0B).
unicode_unihan_variant(0x5BFC, kTraditionalVariant, 0x5C0E).
unicode_unihan_variant(0x5BFF, kSemanticVariant, 0x58FD). %<kFenn 0x5900<kFenn
unicode_unihan_variant(0x5BFF, kTraditionalVariant, 0x58FD).
unicode_unihan_variant(0x5C00, kSemanticVariant, 0x53F5). %<kLau,kMatthews
unicode_unihan_variant(0x5C02, kZVariant, 0x5C08).
unicode_unihan_variant(0x5C03, kSemanticVariant, 0x65C9). %<kMatthews 0x6577<kMatthews
unicode_unihan_variant(0x5C05, kSemanticVariant, 0x524B). %<kHKGlyph,kMatthews
unicode_unihan_variant(0x5C05, kZVariant, 0x524B).
unicode_unihan_variant(0x5C06, kSemanticVariant, 0x2456D). %<kFenn
unicode_unihan_variant(0x5C06, kSpecializedSemanticVariant, 0x5C07). %<kFenn
unicode_unihan_variant(0x5C06, kTraditionalVariant, 0x5C07).
unicode_unihan_variant(0x5C07, kSemanticVariant, 0x2456D).
unicode_unihan_variant(0x5C07, kSimplifiedVariant, 0x5C06).
unicode_unihan_variant(0x5C07, kSpecializedSemanticVariant, 0x5C06). %<kFenn 0x757A<kFenn
unicode_unihan_variant(0x5C08, kSemanticVariant, 0x8011). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5C08, kSimplifiedVariant, 0x4E13).
unicode_unihan_variant(0x5C09, kSpecializedSemanticVariant, 0x71A8). %<kFenn
unicode_unihan_variant(0x5C0B, kSimplifiedVariant, 0x5BFB).
unicode_unihan_variant(0x5C0D, kSemanticVariant, 0x5BF9). %<kMatthews
unicode_unihan_variant(0x5C0D, kSimplifiedVariant, 0x5BF9).
unicode_unihan_variant(0x5C0D, kSpecializedSemanticVariant, 0x5BF9). %<kMeyerWempe
unicode_unihan_variant(0x5C0E, kSimplifiedVariant, 0x5BFC).
unicode_unihan_variant(0x5C12, kSemanticVariant, 0x5C14). %<kMatthews 0x723E<kMatthews
unicode_unihan_variant(0x5C12, kZVariant, 0x723E).
unicode_unihan_variant(0x5C13, kZVariant, 0x723E).
unicode_unihan_variant(0x5C14, kSemanticVariant, 0x5C12). %<kMatthews 0x723E<kMatthews
unicode_unihan_variant(0x5C14, kTraditionalVariant, 0x723E).
unicode_unihan_variant(0x5C17, kSemanticVariant, 0x53D4). %<kMatthews
unicode_unihan_variant(0x5C18, kTraditionalVariant, 0x5875).
unicode_unihan_variant(0x5C19, kZVariant, 0x5C1A).
unicode_unihan_variant(0x5C1A, kZVariant, 0x5C19).
unicode_unihan_variant(0x5C1C, kZVariant, 0x560E).
unicode_unihan_variant(0x5C1D, kTraditionalVariant, 0x5617).
unicode_unihan_variant(0x5C20, kZVariant, 0x9BAE).
unicode_unihan_variant(0x5C22, kSemanticVariant, 0x5C23). %<kMatthews
unicode_unihan_variant(0x5C23, kSemanticVariant, 0x5C22). %<kMatthews
unicode_unihan_variant(0x5C25, kZVariant, 0x5C26).
unicode_unihan_variant(0x5C26, kZVariant, 0x5C25).
unicode_unihan_variant(0x5C27, kTraditionalVariant, 0x582F).
unicode_unihan_variant(0x5C28, kSemanticVariant, 0x72F5). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5C28, kZVariant, 0x9F8D).
unicode_unihan_variant(0x5C29, kSemanticVariant, 0x344C). %<kMatthews
unicode_unihan_variant(0x5C2A, kZVariant, 0x5C2B).
unicode_unihan_variant(0x5C2B, kZVariant, 0x5C2A).
unicode_unihan_variant(0x5C2D, kZVariant, 0x582F).
unicode_unihan_variant(0x5C30, kSemanticVariant, 0x816B). %<kMatthews
unicode_unihan_variant(0x5C32, kSemanticVariant, 0x5C37). %<kLau,kMatthews
unicode_unihan_variant(0x5C34, kTraditionalVariant, 0x5C37).
unicode_unihan_variant(0x5C37, kSemanticVariant, 0x5C32). %<kLau,kMatthews
unicode_unihan_variant(0x5C37, kSimplifiedVariant, 0x5C34).
unicode_unihan_variant(0x5C38, kSemanticVariant, 0x5C4D). %<kMeyerWempe
unicode_unihan_variant(0x5C38, kTraditionalVariant, 0x5C4D).
unicode_unihan_variant(0x5C3B, kSemanticVariant, 0x4BCC). %<kMatthews
unicode_unihan_variant(0x5C3D, kSemanticVariant, 0x76E1). %<kLau,kMatthews
unicode_unihan_variant(0x5C3D, kTraditionalVariant, 0x5118). %0x76E1
unicode_unihan_variant(0x5C3D, kZVariant, 0x76E1).
unicode_unihan_variant(0x5C3F, kSpecializedSemanticVariant, 0x6EBA). %<kFenn
unicode_unihan_variant(0x5C3F, kZVariant, 0xF9BD).
unicode_unihan_variant(0x5C42, kTraditionalVariant, 0x5C64).
unicode_unihan_variant(0x5C43, kTraditionalVariant, 0x5C53).
unicode_unihan_variant(0x5C45, kSemanticVariant, 0x3790). %<kMatthews
unicode_unihan_variant(0x5C46, kSemanticVariant, 0x5C4A). %<kMatthews
unicode_unihan_variant(0x5C46, kSimplifiedVariant, 0x5C4A).
unicode_unihan_variant(0x5C49, kSemanticVariant, 0x5C5C). %<kLau,kMatthews
unicode_unihan_variant(0x5C49, kTraditionalVariant, 0x5C5C).
unicode_unihan_variant(0x5C4A, kSemanticVariant, 0x5C46). %<kMatthews
unicode_unihan_variant(0x5C4A, kTraditionalVariant, 0x5C46).
unicode_unihan_variant(0x5C4D, kSemanticVariant, 0x5C38). %<kMeyerWempe
unicode_unihan_variant(0x5C4D, kSimplifiedVariant, 0x5C38).
unicode_unihan_variant(0x5C4F, kZVariant, 0x6452).
unicode_unihan_variant(0x5C53, kSimplifiedVariant, 0x5C43).
unicode_unihan_variant(0x5C5B, kZVariant, 0x6452).
unicode_unihan_variant(0x5C5C, kSemanticVariant, 0x21C95). %<kFenn
unicode_unihan_variant(0x5C5C, kSimplifiedVariant, 0x5C49).
unicode_unihan_variant(0x5C5E, kSemanticVariant, 0x21C46). %<kFenn 0x5C6C<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5C5E, kTraditionalVariant, 0x5C6C).
unicode_unihan_variant(0x5C61, kTraditionalVariant, 0x5C62).
unicode_unihan_variant(0x5C62, kSimplifiedVariant, 0x5C61).
unicode_unihan_variant(0x5C62, kZVariant, 0xF94B).
unicode_unihan_variant(0x5C63, kSemanticVariant, 0x8E5D). %<kMatthews 0x8EA7<kMatthews
unicode_unihan_variant(0x5C64, kSimplifiedVariant, 0x5C42).
unicode_unihan_variant(0x5C65, kZVariant, 0xF9DF).
unicode_unihan_variant(0x5C66, kTraditionalVariant, 0x5C68).
unicode_unihan_variant(0x5C68, kSimplifiedVariant, 0x5C66).
unicode_unihan_variant(0x5C69, kSimplifiedVariant, 0x2AA17).
unicode_unihan_variant(0x5C69, kZVariant, 0x8E7A).
unicode_unihan_variant(0x5C6C, kSemanticVariant, 0x5C5E). %<kLau,kMatthews,kMeyerWempe 0x21C46<kFenn
unicode_unihan_variant(0x5C6C, kSimplifiedVariant, 0x5C5E).
unicode_unihan_variant(0x5C6D, kZVariant, 0x5C53).
unicode_unihan_variant(0x5C7F, kTraditionalVariant, 0x5DBC).
unicode_unihan_variant(0x5C81, kSemanticVariant, 0x4E97). %<kFenn 0x6B72<kMatthews 0x21ED5<kFenn
unicode_unihan_variant(0x5C81, kTraditionalVariant, 0x6B72).
unicode_unihan_variant(0x5C82, kTraditionalVariant, 0x8C48).
unicode_unihan_variant(0x5C90, kZVariant, 0x6B67).
unicode_unihan_variant(0x5C96, kTraditionalVariant, 0x5D87).
unicode_unihan_variant(0x5C97, kTraditionalVariant, 0x5D17).
unicode_unihan_variant(0x5C97, kZVariant, 0x5CA1).
unicode_unihan_variant(0x5C98, kTraditionalVariant, 0x5CF4).
unicode_unihan_variant(0x5C99, kTraditionalVariant, 0x5DB4).
unicode_unihan_variant(0x5C9A, kTraditionalVariant, 0x5D50).
unicode_unihan_variant(0x5C9B, kTraditionalVariant, 0x5CF6).
unicode_unihan_variant(0x5CA1, kSemanticVariant, 0x5D17). %<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x5CA1, kSimplifiedVariant, 0x5188).
unicode_unihan_variant(0x5CA8, kSemanticVariant, 0x7820). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5CA9, kSemanticVariant, 0x5DD6). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5CA9, kZVariant, 0x5DD6).
unicode_unihan_variant(0x5CAB, kZVariant, 0x5CC0).
unicode_unihan_variant(0x5CAD, kTraditionalVariant, 0x5DBA).
unicode_unihan_variant(0x5CAD, kZVariant, 0x5CBA).
unicode_unihan_variant(0x5CB3, kZVariant, 0x5DBD).
unicode_unihan_variant(0x5CBA, kZVariant, 0x5CAD).
unicode_unihan_variant(0x5CBD, kTraditionalVariant, 0x5D2C).
unicode_unihan_variant(0x5CBD, kZVariant, 0x5D20).
unicode_unihan_variant(0x5CBF, kTraditionalVariant, 0x5DCB).
unicode_unihan_variant(0x5CC0, kZVariant, 0x5CAB).
unicode_unihan_variant(0x5CC4, kTraditionalVariant, 0x5DA7).
unicode_unihan_variant(0x5CD2, kSemanticVariant, 0x6D1E). %<kMeyerWempe
unicode_unihan_variant(0x5CE1, kTraditionalVariant, 0x5CFD).
unicode_unihan_variant(0x5CE3, kTraditionalVariant, 0x5DA2).
unicode_unihan_variant(0x5CE4, kTraditionalVariant, 0x5DA0).
unicode_unihan_variant(0x5CE5, kTraditionalVariant, 0x5D22).
unicode_unihan_variant(0x5CE6, kTraditionalVariant, 0x5DD2).
unicode_unihan_variant(0x5CE8, kSemanticVariant, 0x5CE9). %<kHKGlyph,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5CE8, kZVariant, 0x5CE9).
unicode_unihan_variant(0x5CE9, kSemanticVariant, 0x5CE8). %<kHKGlyph,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5CE9, kZVariant, 0x5CE8).
unicode_unihan_variant(0x5CED, kSemanticVariant, 0x9657). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5CEF, kSemanticVariant, 0x5CF0). %<kHKGlyph
unicode_unihan_variant(0x5CEF, kZVariant, 0x5CF0).
unicode_unihan_variant(0x5CF0, kSemanticVariant, 0x5CEF). %<kHKGlyph
unicode_unihan_variant(0x5CF4, kSimplifiedVariant, 0x5C98).
unicode_unihan_variant(0x5CF6, kSimplifiedVariant, 0x5C9B).
unicode_unihan_variant(0x5CF6, kZVariant, 0x5D8B).
unicode_unihan_variant(0x5CFA, kZVariant, 0x786C).
unicode_unihan_variant(0x5CFD, kSimplifiedVariant, 0x5CE1).
unicode_unihan_variant(0x5D02, kTraditionalVariant, 0x5D97).
unicode_unihan_variant(0x5D03, kTraditionalVariant, 0x5D0D).
unicode_unihan_variant(0x5D04, kTraditionalVariant, 0x5DAE).
unicode_unihan_variant(0x5D08, kZVariant, 0x5D07).
unicode_unihan_variant(0x5D0B, kZVariant, 0x83EF).
unicode_unihan_variant(0x5D0D, kSimplifiedVariant, 0x5D03).
unicode_unihan_variant(0x5D0E, kZVariant, 0x57FC).
unicode_unihan_variant(0x5D10, kZVariant, 0x5D11).
unicode_unihan_variant(0x5D11, kZVariant, 0x5D10).
unicode_unihan_variant(0x5D12, kSemanticVariant, 0x5D2A). %<kMatthews
unicode_unihan_variant(0x5D14, kSemanticVariant, 0x78EA). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5D15, kZVariant, 0x5D16).
unicode_unihan_variant(0x5D16, kSemanticVariant, 0x5393). %<kMatthews
unicode_unihan_variant(0x5D17, kSemanticVariant, 0x5CA1). %<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x5D17, kSimplifiedVariant, 0x5C97).
unicode_unihan_variant(0x5D17, kZVariant, 0x5CA1).
unicode_unihan_variant(0x5D18, kSemanticVariant, 0x5D19). %<kMeyerWempe
unicode_unihan_variant(0x5D18, kZVariant, 0x5D19).
unicode_unihan_variant(0x5D19, kSemanticVariant, 0x4F96). %<kFenn 0x5D18<kMeyerWempe
unicode_unihan_variant(0x5D19, kZVariant, 0xF9D5).
unicode_unihan_variant(0x5D1F, kZVariant, 0x5D94).
unicode_unihan_variant(0x5D20, kZVariant, 0x5CBD).
unicode_unihan_variant(0x5D22, kSimplifiedVariant, 0x5CE5).
unicode_unihan_variant(0x5D27, kSemanticVariant, 0x5D69). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5D2A, kSemanticVariant, 0x5D12). %<kMatthews
unicode_unihan_variant(0x5D2C, kSimplifiedVariant, 0x5CBD).
unicode_unihan_variant(0x5D2D, kTraditionalVariant, 0x5D84).
unicode_unihan_variant(0x5D46, kZVariant, 0x5D47).
unicode_unihan_variant(0x5D50, kSimplifiedVariant, 0x5C9A).
unicode_unihan_variant(0x5D50, kZVariant, 0xF921).
unicode_unihan_variant(0x5D57, kSemanticVariant, 0x6B72).
unicode_unihan_variant(0x5D58, kTraditionalVariant, 0x5DB8).
unicode_unihan_variant(0x5D5A, kTraditionalVariant, 0x5D94).
unicode_unihan_variant(0x5D5C, kZVariant, 0x5D0E).
unicode_unihan_variant(0x5D5D, kTraditionalVariant, 0x5D81).
unicode_unihan_variant(0x5D60, kSemanticVariant, 0x8C3F). %<kMeyerWempe
unicode_unihan_variant(0x5D69, kSemanticVariant, 0x5D27). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5D6F, kZVariant, 0x5D73).
unicode_unihan_variant(0x5D73, kZVariant, 0x5D6F).
unicode_unihan_variant(0x5D7C, kSimplifiedVariant, 0x21DB4).
unicode_unihan_variant(0x5D81, kSimplifiedVariant, 0x5D5D).
unicode_unihan_variant(0x5D83, kSemanticVariant, 0x78DB). %<kMeyerWempe
unicode_unihan_variant(0x5D84, kSemanticVariant, 0x5DC9). %<kMatthews
unicode_unihan_variant(0x5D84, kSimplifiedVariant, 0x5D2D).
unicode_unihan_variant(0x5D87, kSemanticVariant, 0x380A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5D87, kSimplifiedVariant, 0x5C96).
unicode_unihan_variant(0x5D8B, kZVariant, 0x5CF6).
unicode_unihan_variant(0x5D8C, kZVariant, 0x5CF6).
unicode_unihan_variant(0x5D8E, kSemanticVariant, 0x851A). %<kMatthews
unicode_unihan_variant(0x5D94, kSimplifiedVariant, 0x5D5A).
unicode_unihan_variant(0x5D94, kZVariant, 0x5D1F).
unicode_unihan_variant(0x5D97, kSimplifiedVariant, 0x5D02).
unicode_unihan_variant(0x5D9D, kSemanticVariant, 0x78F4). %<kMatthews
unicode_unihan_variant(0x5DA0, kSimplifiedVariant, 0x5CE4).
unicode_unihan_variant(0x5DA2, kSimplifiedVariant, 0x5CE3).
unicode_unihan_variant(0x5DA7, kSimplifiedVariant, 0x5CC4).
unicode_unihan_variant(0x5DAE, kSimplifiedVariant, 0x5D04).
unicode_unihan_variant(0x5DB4, kSimplifiedVariant, 0x5C99).
unicode_unihan_variant(0x5DB8, kSimplifiedVariant, 0x5D58).
unicode_unihan_variant(0x5DBA, kSimplifiedVariant, 0x5CAD).
unicode_unihan_variant(0x5DBA, kZVariant, 0xF9AB).
unicode_unihan_variant(0x5DBC, kSimplifiedVariant, 0x5C7F).
unicode_unihan_variant(0x5DBD, kZVariant, 0x5CB3).
unicode_unihan_variant(0x5DC5, kTraditionalVariant, 0x5DD4).
unicode_unihan_variant(0x5DC9, kSemanticVariant, 0x5D84). %<kMatthews
unicode_unihan_variant(0x5DCB, kSimplifiedVariant, 0x5CBF).
unicode_unihan_variant(0x5DCC, kZVariant, 0x5DD6).
unicode_unihan_variant(0x5DCD, kSemanticVariant, 0x9B4F). %<kMeyerWempe
unicode_unihan_variant(0x5DD2, kSimplifiedVariant, 0x5CE6).
unicode_unihan_variant(0x5DD4, kSimplifiedVariant, 0x5DC5).
unicode_unihan_variant(0x5DD4, kZVariant, 0x5DD3).
unicode_unihan_variant(0x5DD6, kSemanticVariant, 0x5CA9). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5DD6, kZVariant, 0x5DCC).
unicode_unihan_variant(0x5DDB, kSemanticVariant, 0x5DDD). %<kMatthews
unicode_unihan_variant(0x5DDD, kSemanticVariant, 0x5DDB). %<kMatthews
unicode_unihan_variant(0x5DE1, kSemanticVariant, 0x5EF5). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5DE1, kZVariant, 0x5EF5).
unicode_unihan_variant(0x5DE2, kZVariant, 0x5DE3).
unicode_unihan_variant(0x5DE3, kZVariant, 0x5DE2).
unicode_unihan_variant(0x5DE4, kSemanticVariant, 0x9B23). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5DE4, kSpecializedSemanticVariant, 0x524C). %<kFenn 0x9B23<kFenn
unicode_unihan_variant(0x5DE8, kZVariant, 0x9245).
unicode_unihan_variant(0x5DE9, kTraditionalVariant, 0x978F).
unicode_unihan_variant(0x5DEF, kTraditionalVariant, 0x5DF0).
unicode_unihan_variant(0x5DF0, kSimplifiedVariant, 0x5DEF).
unicode_unihan_variant(0x5DF4, kZVariant, 0x7B06).
unicode_unihan_variant(0x5DF5, kZVariant, 0x536E).
unicode_unihan_variant(0x5DFA, kZVariant, 0x5DFD).
unicode_unihan_variant(0x5DFB, kZVariant, 0x6372).
unicode_unihan_variant(0x5DFD, kZVariant, 0x5DFA).
unicode_unihan_variant(0x5E00, kSemanticVariant, 0x531D). %<kMatthews 0x8FCA<kMatthews
unicode_unihan_variant(0x5E01, kTraditionalVariant, 0x5E63).
unicode_unihan_variant(0x5E03, kZVariant, 0x4F48).
unicode_unihan_variant(0x5E05, kTraditionalVariant, 0x5E25).
unicode_unihan_variant(0x5E06, kSemanticVariant, 0x3836). %<kMatthews
unicode_unihan_variant(0x5E06, kSpecializedSemanticVariant, 0x62DA). %<kFenn
unicode_unihan_variant(0x5E08, kTraditionalVariant, 0x5E2B).
unicode_unihan_variant(0x5E0B, kSemanticVariant, 0x7D19). %<kMatthews
unicode_unihan_variant(0x5E0B, kZVariant, 0x7D19).
unicode_unihan_variant(0x5E0C, kZVariant, 0x7A00).
unicode_unihan_variant(0x5E0F, kTraditionalVariant, 0x5E43).
unicode_unihan_variant(0x5E10, kTraditionalVariant, 0x5E33).
unicode_unihan_variant(0x5E12, kSemanticVariant, 0x888B). %<kLau,kMatthews
unicode_unihan_variant(0x5E18, kTraditionalVariant, 0x7C3E).
unicode_unihan_variant(0x5E19, kSemanticVariant, 0x88A0). %<kMatthews
unicode_unihan_variant(0x5E1A, kSemanticVariant, 0x7B92). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5E1A, kSpecializedSemanticVariant, 0x7B92). %<kFenn
unicode_unihan_variant(0x5E1A, kZVariant, 0x83F7).
unicode_unihan_variant(0x5E1C, kTraditionalVariant, 0x5E5F).
unicode_unihan_variant(0x5E21, kZVariant, 0x5E32).
unicode_unihan_variant(0x5E25, kSimplifiedVariant, 0x5E05).
unicode_unihan_variant(0x5E26, kTraditionalVariant, 0x5E36).
unicode_unihan_variant(0x5E27, kTraditionalVariant, 0x5E40).
unicode_unihan_variant(0x5E2B, kSimplifiedVariant, 0x5E08).
unicode_unihan_variant(0x5E2C, kSemanticVariant, 0x88D9). %<kMatthews
unicode_unihan_variant(0x5E2C, kZVariant, 0x88D9).
unicode_unihan_variant(0x5E2D, kZVariant, 0x84C6).
unicode_unihan_variant(0x5E2E, kSemanticVariant, 0x5E6B). %<kMatthews,kMeyerWempe 0x5E47<kFenn
unicode_unihan_variant(0x5E2E, kTraditionalVariant, 0x5E6B).
unicode_unihan_variant(0x5E2F, kZVariant, 0x5E36).
unicode_unihan_variant(0x5E30, kZVariant, 0x6B78).
unicode_unihan_variant(0x5E31, kTraditionalVariant, 0x5E6C).
unicode_unihan_variant(0x5E32, kZVariant, 0x5E21).
unicode_unihan_variant(0x5E33, kSemanticVariant, 0x8CEC). %<kLau
unicode_unihan_variant(0x5E33, kSimplifiedVariant, 0x5E10).
unicode_unihan_variant(0x5E33, kZVariant, 0x8CEC).
unicode_unihan_variant(0x5E36, kSimplifiedVariant, 0x5E26).
unicode_unihan_variant(0x5E36, kZVariant, 0x5E2F).
unicode_unihan_variant(0x5E3B, kTraditionalVariant, 0x5E58).
unicode_unihan_variant(0x5E3C, kTraditionalVariant, 0x5E57).
unicode_unihan_variant(0x5E40, kSimplifiedVariant, 0x5E27).
unicode_unihan_variant(0x5E42, kSemanticVariant, 0x9F0F). %<kMeyerWempe
unicode_unihan_variant(0x5E42, kTraditionalVariant, 0x51AA).
unicode_unihan_variant(0x5E43, kSimplifiedVariant, 0x5E0F).
unicode_unihan_variant(0x5E47, kSemanticVariant, 0x5E2E). %<kFenn 0x5E6B<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5E47, kZVariant, 0x5E6B).
unicode_unihan_variant(0x5E55, kSemanticVariant, 0x5E59). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5E57, kSimplifiedVariant, 0x5E3C).
unicode_unihan_variant(0x5E58, kSimplifiedVariant, 0x5E3B).
unicode_unihan_variant(0x5E59, kSemanticVariant, 0x5E55). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5E5A, kZVariant, 0x5E6B).
unicode_unihan_variant(0x5E5E, kSemanticVariant, 0x896E). %<kMeyerWempe
unicode_unihan_variant(0x5E5F, kSemanticVariant, 0x65D8). %<kMatthews
unicode_unihan_variant(0x5E5F, kSimplifiedVariant, 0x5E1C).
unicode_unihan_variant(0x5E61, kSemanticVariant, 0x65DB). %<kFenn
unicode_unihan_variant(0x5E62, kZVariant, 0x6A66).
unicode_unihan_variant(0x5E63, kSimplifiedVariant, 0x5E01).
unicode_unihan_variant(0x5E64, kZVariant, 0x5E63).
unicode_unihan_variant(0x5E6B, kSemanticVariant, 0x5E2E). %<kMatthews,kMeyerWempe 0x5E47<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5E6B, kSimplifiedVariant, 0x5E2E).
unicode_unihan_variant(0x5E6C, kSimplifiedVariant, 0x5E31).
unicode_unihan_variant(0x5E71, kZVariant, 0x8974).
unicode_unihan_variant(0x5E72, kTraditionalVariant, 0x4E7E). %0x5E79
unicode_unihan_variant(0x5E72, kZVariant, 0x4E7E).
unicode_unihan_variant(0x5E74, kZVariant, 0xF995).
unicode_unihan_variant(0x5E75, kSemanticVariant, 0x5F00). %<kFenn
unicode_unihan_variant(0x5E76, kSemanticVariant, 0x4E26). %<kMatthews,kMeyerWempe 0x5E77<kMatthews,kMeyerWempe 0x7ADD<kMatthews
unicode_unihan_variant(0x5E76, kTraditionalVariant, 0x4E26). %0x4F75
unicode_unihan_variant(0x5E76, kZVariant, 0x4E26).
unicode_unihan_variant(0x5E77, kSemanticVariant, 0x4E26). %<kMatthews,kMeyerWempe 0x5E76<kMatthews,kMeyerWempe 0x7ADD<kMatthews
unicode_unihan_variant(0x5E77, kZVariant, 0x4E26).
unicode_unihan_variant(0x5E78, kSemanticVariant, 0x5016). %<kFenn
unicode_unihan_variant(0x5E78, kSpecializedSemanticVariant, 0x56DF). %<kFenn
unicode_unihan_variant(0x5E78, kZVariant, 0x5016).
unicode_unihan_variant(0x5E79, kSimplifiedVariant, 0x5E72).
unicode_unihan_variant(0x5E7A, kSemanticVariant, 0x4E48). %<kMatthews
unicode_unihan_variant(0x5E7A, kSimplifiedVariant, 0x4E48).
unicode_unihan_variant(0x5E7D, kZVariant, 0x51FC).
unicode_unihan_variant(0x5E7E, kSimplifiedVariant, 0x51E0).
unicode_unihan_variant(0x5E7E, kSpecializedSemanticVariant, 0x51E0). %<kFenn
unicode_unihan_variant(0x5E7F, kTraditionalVariant, 0x5EE3).
unicode_unihan_variant(0x5E81, kZVariant, 0x5EF3).
unicode_unihan_variant(0x5E83, kZVariant, 0x5EE3).
unicode_unihan_variant(0x5E84, kSemanticVariant, 0x838A). %<kLau
unicode_unihan_variant(0x5E84, kTraditionalVariant, 0x838A).
unicode_unihan_variant(0x5E85, kSemanticVariant, 0x9EBD). %<kFenn 0x9EBC<kMatthews
unicode_unihan_variant(0x5E85, kZVariant, 0x9EBC).
unicode_unihan_variant(0x5E86, kTraditionalVariant, 0x6176).
unicode_unihan_variant(0x5E8A, kSemanticVariant, 0x7240). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5E8A, kZVariant, 0x7240).
unicode_unihan_variant(0x5E90, kTraditionalVariant, 0x5EEC).
unicode_unihan_variant(0x5E91, kTraditionalVariant, 0x5EE1).
unicode_unihan_variant(0x5E93, kTraditionalVariant, 0x5EAB).
unicode_unihan_variant(0x5E94, kTraditionalVariant, 0x61C9).
unicode_unihan_variant(0x5E97, kZVariant, 0x576B).
unicode_unihan_variant(0x5E99, kSemanticVariant, 0x5EDF). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5E99, kTraditionalVariant, 0x5EDF).
unicode_unihan_variant(0x5E9E, kTraditionalVariant, 0x9F90).
unicode_unihan_variant(0x5E9F, kTraditionalVariant, 0x5EE2).
unicode_unihan_variant(0x5EA6, kZVariant, 0xFA0B).
unicode_unihan_variant(0x5EA7, kSpecializedSemanticVariant, 0x5750). %<kMeyerWempe
unicode_unihan_variant(0x5EA7, kZVariant, 0x5750).
unicode_unihan_variant(0x5EAB, kSimplifiedVariant, 0x5E93).
unicode_unihan_variant(0x5EAC, kSemanticVariant, 0x5396). %<kMeyerWempe
unicode_unihan_variant(0x5EB5, kSemanticVariant, 0x83F4). %<kHKGlyph,kMatthews).
unicode_unihan_variant(0x5EB5, kZVariant, 0x83F4).
unicode_unihan_variant(0x5EB6, kZVariant, 0x5EBB).
unicode_unihan_variant(0x5EB8, kSemanticVariant, 0x342F). %0x2018C
unicode_unihan_variant(0x5EBB, kZVariant, 0x5EB6).
unicode_unihan_variant(0x5EBC, kZVariant, 0x5ECE).
unicode_unihan_variant(0x5EBD, kZVariant, 0x5BD3).
unicode_unihan_variant(0x5EBE, kSemanticVariant, 0x659E). %<kMatthews
unicode_unihan_variant(0x5EBF, kZVariant, 0x5EDF).
unicode_unihan_variant(0x5EC0, kSemanticVariant, 0x5ECB). %<kMatthews
unicode_unihan_variant(0x5EC1, kSemanticVariant, 0x53A0). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5EC1, kSimplifiedVariant, 0x5395).
unicode_unihan_variant(0x5EC2, kSimplifiedVariant, 0x53A2).
unicode_unihan_variant(0x5EC3, kZVariant, 0x5EE2).
unicode_unihan_variant(0x5EC4, kSimplifiedVariant, 0x53A9).
unicode_unihan_variant(0x5EC4, kZVariant, 0x5ED0).
unicode_unihan_variant(0x5EC8, kSemanticVariant, 0x53A6). %<kLau,kMatthews
unicode_unihan_variant(0x5EC8, kSimplifiedVariant, 0x53A6).
unicode_unihan_variant(0x5EC9, kZVariant, 0xF9A2).
unicode_unihan_variant(0x5ECA, kZVariant, 0xF928).
unicode_unihan_variant(0x5ECB, kSemanticVariant, 0x5EC0). %<kMatthews
unicode_unihan_variant(0x5ECC, kSemanticVariant, 0x8C78). %<kMatthews
unicode_unihan_variant(0x5ECF, kZVariant, 0x5EC4).
unicode_unihan_variant(0x5ED0, kSemanticVariant, 0x53A9). %<kMatthews
unicode_unihan_variant(0x5ED0, kZVariant, 0x5EC4).
unicode_unihan_variant(0x5ED1, kSemanticVariant, 0x53AA). %<kMatthews
unicode_unihan_variant(0x5ED3, kZVariant, 0xFA0B).
unicode_unihan_variant(0x5ED5, kSemanticVariant, 0x852D). %<kMatthews
unicode_unihan_variant(0x5EDA, kSemanticVariant, 0x53A8). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5EDA, kSimplifiedVariant, 0x53A8).
unicode_unihan_variant(0x5EDD, kSemanticVariant, 0x348B). %<kMatthews 0x53AE<kMatthews
unicode_unihan_variant(0x5EDD, kSimplifiedVariant, 0x53AE).
unicode_unihan_variant(0x5EDF, kSemanticVariant, 0x5E99). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5EDF, kSimplifiedVariant, 0x5E99).
unicode_unihan_variant(0x5EE0, kSemanticVariant, 0x53B0). %<kLau,kMatthews
unicode_unihan_variant(0x5EE0, kSimplifiedVariant, 0x5382).
unicode_unihan_variant(0x5EE0, kZVariant, 0x53B0).
unicode_unihan_variant(0x5EE1, kSimplifiedVariant, 0x5E91).
unicode_unihan_variant(0x5EE2, kSimplifiedVariant, 0x5E9F).
unicode_unihan_variant(0x5EE2, kZVariant, 0x5EC3).
unicode_unihan_variant(0x5EE3, kSimplifiedVariant, 0x5E7F).
unicode_unihan_variant(0x5EE7, kSemanticVariant, 0x58BB). %<kLau,kMatthews 0x7246<kLau,kMatthews
unicode_unihan_variant(0x5EE9, kSimplifiedVariant, 0x5EEA).
unicode_unihan_variant(0x5EE9, kZVariant, 0x7A1F).
unicode_unihan_variant(0x5EEA, kTraditionalVariant, 0x5EE9).
unicode_unihan_variant(0x5EEA, kZVariant, 0x7A1F).
unicode_unihan_variant(0x5EEC, kSimplifiedVariant, 0x5E90).
unicode_unihan_variant(0x5EEC, kZVariant, 0xF982).
unicode_unihan_variant(0x5EF0, kZVariant, 0x5EF3).
unicode_unihan_variant(0x5EF1, kSemanticVariant, 0x96DD). %<kMeyerWempe
unicode_unihan_variant(0x5EF3, kSimplifiedVariant, 0x5385).
unicode_unihan_variant(0x5EF3, kZVariant, 0x5EF0).
unicode_unihan_variant(0x5EF5, kSemanticVariant, 0x5DE1). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5EF5, kZVariant, 0x5DE1).
unicode_unihan_variant(0x5EF8, kSemanticVariant, 0x8FEA). %<kMatthews
unicode_unihan_variant(0x5EF8, kZVariant, 0x8FEA).
unicode_unihan_variant(0x5EF9, kSemanticVariant, 0x8FEB). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5EFB, kSemanticVariant, 0x56DE). %<kMeyerWempe 0x8FF4<kLau,kMatthews
unicode_unihan_variant(0x5EFB, kZVariant, 0x56DE).
unicode_unihan_variant(0x5EFC, kSemanticVariant, 0x4E43). %<kMatthews 0x8FFA<kMatthews
unicode_unihan_variant(0x5EFC, kZVariant, 0x4E43).
unicode_unihan_variant(0x5EFD, kZVariant, 0x56DE).
unicode_unihan_variant(0x5EFE, kZVariant, 0x5EFF).
unicode_unihan_variant(0x5EFF, kSemanticVariant, 0x5344). %<kMatthews
unicode_unihan_variant(0x5EFF, kZVariant, 0x5EFE).
unicode_unihan_variant(0x5F00, kSemanticVariant, 0x5E75). %<kFenn
unicode_unihan_variant(0x5F00, kTraditionalVariant, 0x958B).
unicode_unihan_variant(0x5F01, kZVariant, 0x8FA8).
unicode_unihan_variant(0x5F02, kTraditionalVariant, 0x7570).
unicode_unihan_variant(0x5F03, kTraditionalVariant, 0x68C4).
unicode_unihan_variant(0x5F04, kSpecializedSemanticVariant, 0x7627). %<kFenn
unicode_unihan_variant(0x5F04, kZVariant, 0xF943).
unicode_unihan_variant(0x5F08, kSemanticVariant, 0x5955). %<kFenn
unicode_unihan_variant(0x5F09, kZVariant, 0x5958).
unicode_unihan_variant(0x5F0A, kSemanticVariant, 0x21681). %<kFenn
unicode_unihan_variant(0x5F0C, kSemanticVariant, 0x4E00). %<kLau,kMatthews,kMeyerWempe 0x58F9<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5F0C, kZVariant, 0x640B).
unicode_unihan_variant(0x5F0D, kSemanticVariant, 0x4E8C). %<kMatthews,kMeyerWempe 0x8CB3<kMeyerWempe
unicode_unihan_variant(0x5F0D, kSpecializedSemanticVariant, 0x8CB3).
unicode_unihan_variant(0x5F0D, kZVariant, 0x4E8C).
unicode_unihan_variant(0x5F11, kTraditionalVariant, 0x5F12).
unicode_unihan_variant(0x5F12, kSimplifiedVariant, 0x5F11).
unicode_unihan_variant(0x5F14, kSemanticVariant, 0x540A). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5F20, kTraditionalVariant, 0x5F35).
unicode_unihan_variant(0x5F22, kSemanticVariant, 0x97DC). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5F25, kTraditionalVariant, 0x5F4C).
unicode_unihan_variant(0x5F26, kSemanticVariant, 0x7D43). %<kFenn
unicode_unihan_variant(0x5F26, kZVariant, 0x7D43).
unicode_unihan_variant(0x5F2A, kTraditionalVariant, 0x5F33).
unicode_unihan_variant(0x5F2F, kTraditionalVariant, 0x5F4E).
unicode_unihan_variant(0x5F33, kSimplifiedVariant, 0x5F2A).
unicode_unihan_variant(0x5F35, kSimplifiedVariant, 0x5F20).
unicode_unihan_variant(0x5F37, kSemanticVariant, 0x5F3A). %<kLau,kMatthews 0x5F4A<kLau,kMatthews
unicode_unihan_variant(0x5F37, kSimplifiedVariant, 0x5F3A).
unicode_unihan_variant(0x5F37, kSpecializedSemanticVariant, 0x5F4A). %<kFenn
unicode_unihan_variant(0x5F39, kTraditionalVariant, 0x5F48).
unicode_unihan_variant(0x5F3A, kSemanticVariant, 0x5F37). %<kLau,kMatthews 0x5F4A<kLau,kMatthews
unicode_unihan_variant(0x5F3A, kSpecializedSemanticVariant, 0x5F4A). %<kFenn
unicode_unihan_variant(0x5F3A, kTraditionalVariant, 0x5F37).
unicode_unihan_variant(0x5F3E, kZVariant, 0x5F48).
unicode_unihan_variant(0x5F40, kSemanticVariant, 0x591F). %<kFenn 0x5920<kFenn
unicode_unihan_variant(0x5F42, kZVariant, 0x767C).
unicode_unihan_variant(0x5F48, kSimplifiedVariant, 0x5F39).
unicode_unihan_variant(0x5F4A, kSemanticVariant, 0x5F37). %<kLau,kMatthews 0x5F3A<kLau,kMatthews
unicode_unihan_variant(0x5F4A, kSpecializedSemanticVariant, 0x5F37). %<kFenn 0x5F3A<kFenn
unicode_unihan_variant(0x5F4A, kZVariant, 0x5F3A).
unicode_unihan_variant(0x5F4C, kSimplifiedVariant, 0x5F25).
unicode_unihan_variant(0x5F4E, kSimplifiedVariant, 0x5F2F).
unicode_unihan_variant(0x5F50, kSemanticVariant, 0x5F51). %<kFenn
unicode_unihan_variant(0x5F50, kZVariant, 0x5F51).
unicode_unihan_variant(0x5F51, kSemanticVariant, 0x5F50). %<kFenn
unicode_unihan_variant(0x5F51, kZVariant, 0x5F50).
unicode_unihan_variant(0x5F52, kSemanticVariant, 0x6B78). %<kMatthews
unicode_unihan_variant(0x5F52, kTraditionalVariant, 0x6B78).
unicode_unihan_variant(0x5F53, kSpecializedSemanticVariant, 0x7576). %<kFenn
unicode_unihan_variant(0x5F53, kTraditionalVariant, 0x5679). %0x7576
unicode_unihan_variant(0x5F53, kZVariant, 0x7576).
unicode_unihan_variant(0x5F55, kTraditionalVariant, 0x9304).
unicode_unihan_variant(0x5F55, kZVariant, 0x9304).
unicode_unihan_variant(0x5F57, kZVariant, 0x7BF2).
unicode_unihan_variant(0x5F59, kSimplifiedVariant, 0x6C47).
unicode_unihan_variant(0x5F59, kZVariant, 0x5F5A).
unicode_unihan_variant(0x5F5A, kZVariant, 0x5F59).
unicode_unihan_variant(0x5F5B, kZVariant, 0x5F5D).
unicode_unihan_variant(0x5F5C, kZVariant, 0x5F5D).
unicode_unihan_variant(0x5F5D, kTraditionalVariant, 0x5F5E).
unicode_unihan_variant(0x5F5D, kZVariant, 0x5F5B).
unicode_unihan_variant(0x5F5E, kSimplifiedVariant, 0x5F5D).
unicode_unihan_variant(0x5F5F, kTraditionalVariant, 0x5F60).
unicode_unihan_variant(0x5F60, kSimplifiedVariant, 0x5F5F).
unicode_unihan_variant(0x5F65, kSimplifiedVariant, 0x5F66).
unicode_unihan_variant(0x5F66, kTraditionalVariant, 0x5F65).
unicode_unihan_variant(0x5F68, kTraditionalVariant, 0x5F72).
unicode_unihan_variant(0x5F68, kZVariant, 0x5F72).
unicode_unihan_variant(0x5F6B, kSemanticVariant, 0x34EE). %<kLau,kMatthews 0x96D5<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5F6B, kSpecializedSemanticVariant, 0x9D70).
unicode_unihan_variant(0x5F6C, kSemanticVariant, 0x658C). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5F6C, kZVariant, 0x4EFD).
unicode_unihan_variant(0x5F72, kSimplifiedVariant, 0x5F68).
unicode_unihan_variant(0x5F72, kZVariant, 0x5F68).
unicode_unihan_variant(0x5F77, kSemanticVariant, 0x9AE3). %<kMatthews,kMeyerWempe 0x5FAC<kLau
unicode_unihan_variant(0x5F77, kZVariant, 0x4EFF).
unicode_unihan_variant(0x5F78, kSemanticVariant, 0x4F00). %<kMatthews
unicode_unihan_variant(0x5F7B, kTraditionalVariant, 0x5FB9).
unicode_unihan_variant(0x5F7F, kZVariant, 0x9AF4).
unicode_unihan_variant(0x5F80, kSemanticVariant, 0x5F83). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5F80, kSpecializedSemanticVariant, 0x5F83). %<kFenn
unicode_unihan_variant(0x5F80, kZVariant, 0x5F83).
unicode_unihan_variant(0x5F81, kSimplifiedVariant, 0x5F81).
unicode_unihan_variant(0x5F81, kTraditionalVariant, 0x5F81). %0x5FB5
unicode_unihan_variant(0x5F83, kSemanticVariant, 0x5F80). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5F83, kSpecializedSemanticVariant, 0x5F80). %<kFenn
unicode_unihan_variant(0x5F83, kZVariant, 0x5F80).
unicode_unihan_variant(0x5F84, kTraditionalVariant, 0x5F91).
unicode_unihan_variant(0x5F8B, kZVariant, 0xF9D8).
unicode_unihan_variant(0x5F8C, kSimplifiedVariant, 0x540E).
unicode_unihan_variant(0x5F91, kSemanticVariant, 0x4FD3). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5F91, kSimplifiedVariant, 0x5F84).
unicode_unihan_variant(0x5F91, kSpecializedSemanticVariant, 0x52C1). %<kMeyerWempe
unicode_unihan_variant(0x5F92, kSemanticVariant, 0x28452). %<kFenn
unicode_unihan_variant(0x5F93, kZVariant, 0x5F9E).
unicode_unihan_variant(0x5F95, kTraditionalVariant, 0x5FA0).
unicode_unihan_variant(0x5F9E, kSemanticVariant, 0x4ECE). %<kMatthews
unicode_unihan_variant(0x5F9E, kSimplifiedVariant, 0x4ECE).
unicode_unihan_variant(0x5F9E, kSpecializedSemanticVariant, 0x4ECE). %<kFenn
unicode_unihan_variant(0x5F9E, kZVariant, 0x5F93).
unicode_unihan_variant(0x5FA0, kSemanticVariant, 0x5008). %<kLau,kMatthews 0x52D1<kLau
unicode_unihan_variant(0x5FA0, kSimplifiedVariant, 0x5F95).
unicode_unihan_variant(0x5FA0, kZVariant, 0x4F86).
unicode_unihan_variant(0x5FA1, kSemanticVariant, 0x99AD). %<kMatthews
unicode_unihan_variant(0x5FA1, kSpecializedSemanticVariant, 0x99AD). %<kMeyerWempe
unicode_unihan_variant(0x5FA1, kTraditionalVariant, 0x79A6).
unicode_unihan_variant(0x5FA4, kSemanticVariant, 0x5065). %<kFenn
unicode_unihan_variant(0x5FA7, kSemanticVariant, 0x904D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5FA7, kZVariant, 0x904D).
unicode_unihan_variant(0x5FA8, kZVariant, 0x9051).
unicode_unihan_variant(0x5FA9, kSimplifiedVariant, 0x590D).
unicode_unihan_variant(0x5FA9, kZVariant, 0x8986).
unicode_unihan_variant(0x5FAC, kSemanticVariant, 0x5F77). %<kLau
unicode_unihan_variant(0x5FAD, kSemanticVariant, 0x509C). %<kMatthews
unicode_unihan_variant(0x5FB3, kZVariant, 0x5FB7).
unicode_unihan_variant(0x5FB4, kZVariant, 0x5FB5).
unicode_unihan_variant(0x5FB5, kSimplifiedVariant, 0x5F81). %0x5FB5
unicode_unihan_variant(0x5FB5, kTraditionalVariant, 0x5FB5).
unicode_unihan_variant(0x5FB7, kSemanticVariant, 0x60B3). %<kLau,kMatthews 0x60EA<kFenn
unicode_unihan_variant(0x5FB7, kZVariant, 0x60EA).
unicode_unihan_variant(0x5FB9, kSemanticVariant, 0x6F88). %<kLau
unicode_unihan_variant(0x5FB9, kSimplifiedVariant, 0x5F7B).
unicode_unihan_variant(0x5FBA, kZVariant, 0x50E5).
unicode_unihan_variant(0x5FC3, kSemanticVariant, 0x38FA). %<kMatthews 0x5FC4<kMatthews
unicode_unihan_variant(0x5FC4, kSemanticVariant, 0x5FC3). %<kMatthews
unicode_unihan_variant(0x5FC6, kTraditionalVariant, 0x61B6).
unicode_unihan_variant(0x5FC8, kZVariant, 0x4EC1).
unicode_unihan_variant(0x5FCD, kSemanticVariant, 0x38FC). %<kMatthews
unicode_unihan_variant(0x5FCF, kTraditionalVariant, 0x61FA).
unicode_unihan_variant(0x5FD7, kSimplifiedVariant, 0x5FD7).
unicode_unihan_variant(0x5FD7, kTraditionalVariant, 0x5FD7). %0x8A8C
unicode_unihan_variant(0x5FD7, kZVariant, 0x8A8C).
unicode_unihan_variant(0x5FDC, kZVariant, 0x61C9).
unicode_unihan_variant(0x5FDE, kZVariant, 0x668B).
unicode_unihan_variant(0x5FE4, kSemanticVariant, 0x554E). %<kMatthews 0x8FD5<kMeyerWempe
unicode_unihan_variant(0x5FE7, kTraditionalVariant, 0x6182).
unicode_unihan_variant(0x5FED, kSemanticVariant, 0x662A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x5FF0, kZVariant, 0x60B4).
unicode_unihan_variant(0x5FF5, kZVariant, 0xF9A3).
unicode_unihan_variant(0x5FFB, kSemanticVariant, 0x6B23). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x5FFB, kZVariant, 0x6B23).
unicode_unihan_variant(0x5FFC, kSemanticVariant, 0x6177). %<kHKGlyph
unicode_unihan_variant(0x5FFE, kTraditionalVariant, 0x613E).
unicode_unihan_variant(0x6000, kSemanticVariant, 0x61F7). %<kLau,kMatthews
unicode_unihan_variant(0x6000, kTraditionalVariant, 0x61F7).
unicode_unihan_variant(0x6001, kTraditionalVariant, 0x614B).
unicode_unihan_variant(0x6002, kTraditionalVariant, 0x616B).
unicode_unihan_variant(0x6003, kTraditionalVariant, 0x61AE).
unicode_unihan_variant(0x6004, kTraditionalVariant, 0x616A).
unicode_unihan_variant(0x6005, kTraditionalVariant, 0x60B5).
unicode_unihan_variant(0x6006, kTraditionalVariant, 0x6134).
unicode_unihan_variant(0x600C, kSemanticVariant, 0x61F7). %<kFenn
unicode_unihan_variant(0x6012, kZVariant, 0xF960).
unicode_unihan_variant(0x601C, kTraditionalVariant, 0x6190).
unicode_unihan_variant(0x602A, kSemanticVariant, 0x6060). %<kLau,kMatthews
unicode_unihan_variant(0x6031, kSemanticVariant, 0x5306). %<kHKGlyph,kLau,kMatthews 0x60A4<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6033, kSemanticVariant, 0x604D). %<kMatthews
unicode_unihan_variant(0x603B, kTraditionalVariant, 0x7E3D).
unicode_unihan_variant(0x603C, kTraditionalVariant, 0x61DF).
unicode_unihan_variant(0x603F, kTraditionalVariant, 0x61CC).
unicode_unihan_variant(0x6042, kZVariant, 0x609B).
unicode_unihan_variant(0x6046, kSemanticVariant, 0x6052). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6046, kSimplifiedVariant, 0x6052).
unicode_unihan_variant(0x604A, kSemanticVariant, 0x52F0). %<kMatthews
unicode_unihan_variant(0x604A, kZVariant, 0x5354).
unicode_unihan_variant(0x604B, kSemanticVariant, 0x6200). %<kMatthews
unicode_unihan_variant(0x604B, kSpecializedSemanticVariant, 0x6200). %<kFenn
unicode_unihan_variant(0x604B, kTraditionalVariant, 0x6200).
unicode_unihan_variant(0x604D, kSemanticVariant, 0x6033). %<kMatthews
unicode_unihan_variant(0x6050, kSemanticVariant, 0x22662). %<kLau
unicode_unihan_variant(0x6052, kSemanticVariant, 0x6046). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6052, kTraditionalVariant, 0x6046).
unicode_unihan_variant(0x6060, kSemanticVariant, 0x602A). %<kLau,kMatthews
unicode_unihan_variant(0x6061, kSemanticVariant, 0x541D). %<kMatthews 0x608B<kMatthews
unicode_unihan_variant(0x6064, kSemanticVariant, 0x5379). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6064, kZVariant, 0x8CC9).
unicode_unihan_variant(0x6065, kSemanticVariant, 0x803B). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6065, kSimplifiedVariant, 0x803B).
unicode_unihan_variant(0x6069, kSemanticVariant, 0x6441). %<kMatthews
unicode_unihan_variant(0x6073, kTraditionalVariant, 0x61C7).
unicode_unihan_variant(0x6075, kZVariant, 0x60E0).
unicode_unihan_variant(0x6076, kTraditionalVariant, 0x60E1).
unicode_unihan_variant(0x6078, kTraditionalVariant, 0x615F).
unicode_unihan_variant(0x6079, kTraditionalVariant, 0x61E8).
unicode_unihan_variant(0x607A, kTraditionalVariant, 0x6137).
unicode_unihan_variant(0x607B, kTraditionalVariant, 0x60FB).
unicode_unihan_variant(0x607C, kTraditionalVariant, 0x60F1).
unicode_unihan_variant(0x607D, kTraditionalVariant, 0x60F2).
unicode_unihan_variant(0x607F, kSemanticVariant, 0x6142). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x607F, kZVariant, 0x6142).
unicode_unihan_variant(0x6081, kSemanticVariant, 0x61C1). %<kMeyerWempe
unicode_unihan_variant(0x6085, kSemanticVariant, 0x60A6). %<kMatthews
unicode_unihan_variant(0x6085, kSimplifiedVariant, 0x60A6).
unicode_unihan_variant(0x608A, kSemanticVariant, 0x54F2). %<kMatthews
unicode_unihan_variant(0x608B, kSemanticVariant, 0x541D). %<kMatthews 0x6061<kMatthews
unicode_unihan_variant(0x608B, kZVariant, 0x541D).
unicode_unihan_variant(0x6090, kSemanticVariant, 0x60D5). %<kMatthews
unicode_unihan_variant(0x609A, kSemanticVariant, 0x22960). %<kLau,kMeyerWempe
unicode_unihan_variant(0x609B, kZVariant, 0x6042).
unicode_unihan_variant(0x609E, kSemanticVariant, 0x8AA4). %<kLau
unicode_unihan_variant(0x609E, kSimplifiedVariant, 0x60AE).
unicode_unihan_variant(0x60A0, kSemanticVariant, 0x6EFA). %<kFenn
unicode_unihan_variant(0x60A4, kSemanticVariant, 0x5306). %<kFenn 0x6031<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x60A4, kZVariant, 0x5306).
unicode_unihan_variant(0x60A6, kSemanticVariant, 0x6085). %<kMatthews
unicode_unihan_variant(0x60A6, kTraditionalVariant, 0x6085).
unicode_unihan_variant(0x60AA, kZVariant, 0x60E1).
unicode_unihan_variant(0x60AB, kTraditionalVariant, 0x6128).
unicode_unihan_variant(0x60AB, kZVariant, 0x6164).
unicode_unihan_variant(0x60AC, kTraditionalVariant, 0x61F8).
unicode_unihan_variant(0x60AD, kTraditionalVariant, 0x6173).
unicode_unihan_variant(0x60AE, kTraditionalVariant, 0x609E).
unicode_unihan_variant(0x60AF, kTraditionalVariant, 0x61AB).
unicode_unihan_variant(0x60B3, kSemanticVariant, 0x5FB7). %<kLau,kMatthews
unicode_unihan_variant(0x60B3, kZVariant, 0x5FB7).
unicode_unihan_variant(0x60B4, kZVariant, 0x5FF0).
unicode_unihan_variant(0x60B5, kSimplifiedVariant, 0x6005).
unicode_unihan_variant(0x60B6, kSemanticVariant, 0x3943). %<kMatthews,kMeyerWempe 0x61E3<kLau,kMatthews
unicode_unihan_variant(0x60B6, kSimplifiedVariant, 0x95F7).
unicode_unihan_variant(0x60BD, kZVariant, 0x6DD2).
unicode_unihan_variant(0x60C7, kSemanticVariant, 0x6566). %<kLau,kMatthews
unicode_unihan_variant(0x60CA, kTraditionalVariant, 0x9A5A).
unicode_unihan_variant(0x60D5, kSemanticVariant, 0x6090). %<kMatthews
unicode_unihan_variant(0x60DB, kSemanticVariant, 0x60FD). %<kMatthews
unicode_unihan_variant(0x60DD, kSemanticVariant, 0x22835). %<kMeyerWempe
unicode_unihan_variant(0x60E0, kZVariant, 0x6075).
unicode_unihan_variant(0x60E1, kSimplifiedVariant, 0x6076).
unicode_unihan_variant(0x60E1, kZVariant, 0xF9B9).
unicode_unihan_variant(0x60E7, kSemanticVariant, 0x61FC). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x60E7, kTraditionalVariant, 0x61FC).
unicode_unihan_variant(0x60E8, kTraditionalVariant, 0x6158).
unicode_unihan_variant(0x60E9, kSemanticVariant, 0x61F2). %<kMatthews
unicode_unihan_variant(0x60E9, kTraditionalVariant, 0x61F2).
unicode_unihan_variant(0x60EA, kSemanticVariant, 0x5FB7). %<kFenn
unicode_unihan_variant(0x60EA, kZVariant, 0x5FB7).
unicode_unihan_variant(0x60EB, kTraditionalVariant, 0x618A).
unicode_unihan_variant(0x60EC, kTraditionalVariant, 0x611C).
unicode_unihan_variant(0x60ED, kTraditionalVariant, 0x615A).
unicode_unihan_variant(0x60EE, kTraditionalVariant, 0x619A).
unicode_unihan_variant(0x60EF, kTraditionalVariant, 0x6163).
unicode_unihan_variant(0x60F1, kSimplifiedVariant, 0x607C).
unicode_unihan_variant(0x60F1, kSpecializedSemanticVariant, 0x5B32). %<kMeyerWempe
unicode_unihan_variant(0x60F2, kSimplifiedVariant, 0x607D).
unicode_unihan_variant(0x60F7, kZVariant, 0x8822).
unicode_unihan_variant(0x60FA, kSemanticVariant, 0x2272B). %<kFenn
unicode_unihan_variant(0x60FA, kSpecializedSemanticVariant, 0x9192). %<kMeyerWempe
unicode_unihan_variant(0x60FB, kSimplifiedVariant, 0x607B).
unicode_unihan_variant(0x60FD, kSemanticVariant, 0x60DB). %<kMatthews
unicode_unihan_variant(0x6108, kSemanticVariant, 0x6109). %<kMeyerWempe
unicode_unihan_variant(0x6108, kSpecializedSemanticVariant, 0x7609). %<kFenn
unicode_unihan_variant(0x6108, kZVariant, 0x7652).
unicode_unihan_variant(0x6109, kSemanticVariant, 0x6108). %<kMeyerWempe
unicode_unihan_variant(0x610D, kSemanticVariant, 0x61AB). %<kMatthews
unicode_unihan_variant(0x611B, kSimplifiedVariant, 0x7231).
unicode_unihan_variant(0x611C, kSimplifiedVariant, 0x60EC).
unicode_unihan_variant(0x6120, kSemanticVariant, 0x614D). %<kHKGlyph,kMeyerWempe
unicode_unihan_variant(0x6120, kTraditionalVariant, 0x614D).
unicode_unihan_variant(0x6124, kTraditionalVariant, 0x61A4).
unicode_unihan_variant(0x6126, kTraditionalVariant, 0x6192).
unicode_unihan_variant(0x6127, kSemanticVariant, 0x5ABF). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6127, kZVariant, 0x5ABF).
unicode_unihan_variant(0x6128, kSimplifiedVariant, 0x60AB).
unicode_unihan_variant(0x6128, kZVariant, 0x6164).
unicode_unihan_variant(0x612C, kSemanticVariant, 0x8A34). %<kMatthews
unicode_unihan_variant(0x612C, kSpecializedSemanticVariant, 0x8A34). %<kFenn
unicode_unihan_variant(0x612C, kZVariant, 0x8A34).
unicode_unihan_variant(0x6134, kSimplifiedVariant, 0x6006).
unicode_unihan_variant(0x6137, kSimplifiedVariant, 0x607A).
unicode_unihan_variant(0x613B, kSemanticVariant, 0x905C). %<kFenn
unicode_unihan_variant(0x613C, kZVariant, 0x7718).
unicode_unihan_variant(0x613D, kSemanticVariant, 0x535A). %<kMatthews
unicode_unihan_variant(0x613D, kZVariant, 0x535A).
unicode_unihan_variant(0x613E, kSimplifiedVariant, 0x5FFE).
unicode_unihan_variant(0x613F, kSemanticVariant, 0x9858). %<kFenn
unicode_unihan_variant(0x613F, kSimplifiedVariant, 0x613F).
unicode_unihan_variant(0x613F, kSpecializedSemanticVariant, 0x9858). %<kHanYu:T
unicode_unihan_variant(0x613F, kTraditionalVariant, 0x613F). %0x9858
unicode_unihan_variant(0x6141, kSemanticVariant, 0x3975). %<kMatthews
unicode_unihan_variant(0x6142, kSemanticVariant, 0x607F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6142, kZVariant, 0x607F).
unicode_unihan_variant(0x6144, kSimplifiedVariant, 0x6817).
unicode_unihan_variant(0x6144, kZVariant, 0xF9D9).
unicode_unihan_variant(0x6148, kSemanticVariant, 0x3935). %<kFenn
unicode_unihan_variant(0x614B, kSimplifiedVariant, 0x6001).
unicode_unihan_variant(0x614D, kSemanticVariant, 0x6120). %<kHKGlyph,kMeyerWempe
unicode_unihan_variant(0x614D, kSimplifiedVariant, 0x6120).
unicode_unihan_variant(0x614E, kZVariant, 0x613C).
unicode_unihan_variant(0x6151, kTraditionalVariant, 0x61FE).
unicode_unihan_variant(0x6158, kSimplifiedVariant, 0x60E8).
unicode_unihan_variant(0x6159, kSemanticVariant, 0x615A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6159, kZVariant, 0x615A).
unicode_unihan_variant(0x615A, kSemanticVariant, 0x6159). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x615A, kSimplifiedVariant, 0x60ED).
unicode_unihan_variant(0x615E, kSemanticVariant, 0x50BD). %<kMeyerWempe
unicode_unihan_variant(0x615F, kSimplifiedVariant, 0x6078).
unicode_unihan_variant(0x6163, kSimplifiedVariant, 0x60EF).
unicode_unihan_variant(0x6164, kZVariant, 0x6128).
unicode_unihan_variant(0x616A, kSimplifiedVariant, 0x6004).
unicode_unihan_variant(0x616A, kSpecializedSemanticVariant, 0x5614). %<kFenn
unicode_unihan_variant(0x616B, kSimplifiedVariant, 0x6002).
unicode_unihan_variant(0x616D, kZVariant, 0x6196).
unicode_unihan_variant(0x616E, kSimplifiedVariant, 0x8651).
unicode_unihan_variant(0x6173, kSimplifiedVariant, 0x60AD).
unicode_unihan_variant(0x6176, kSimplifiedVariant, 0x5E86).
unicode_unihan_variant(0x6177, kSemanticVariant, 0x5FFC). %<kHKGlyph
unicode_unihan_variant(0x617C, kSemanticVariant, 0x617D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x617D, kSemanticVariant, 0x617C). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x617D, kZVariant, 0x617C).
unicode_unihan_variant(0x617E, kSemanticVariant, 0x6B32). %<kMeyerWempe
unicode_unihan_variant(0x617E, kZVariant, 0x6B32).
unicode_unihan_variant(0x6182, kSimplifiedVariant, 0x5FE7).
unicode_unihan_variant(0x6182, kSpecializedSemanticVariant, 0x61EE). %<kHanYu
unicode_unihan_variant(0x6185, kZVariant, 0x615F).
unicode_unihan_variant(0x6187, kSemanticVariant, 0x61A9). %<kMatthews
unicode_unihan_variant(0x6187, kZVariant, 0x61A9).
unicode_unihan_variant(0x618A, kSimplifiedVariant, 0x60EB).
unicode_unihan_variant(0x618B, kSemanticVariant, 0x22833). %<kMeyerWempe
unicode_unihan_variant(0x6190, kSimplifiedVariant, 0x601C).
unicode_unihan_variant(0x6190, kZVariant, 0xF98F).
unicode_unihan_variant(0x6191, kSemanticVariant, 0x51F4). %<kLau,kMatthews 0x205E6<kHanYu,kMeyerWempe
unicode_unihan_variant(0x6191, kSimplifiedVariant, 0x51ED).
unicode_unihan_variant(0x6191, kZVariant, 0x51F4).
unicode_unihan_variant(0x6192, kSimplifiedVariant, 0x6126).
unicode_unihan_variant(0x6194, kSemanticVariant, 0x9866). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6196, kZVariant, 0x616D).
unicode_unihan_variant(0x6199, kSemanticVariant, 0x559C).
unicode_unihan_variant(0x619A, kSimplifiedVariant, 0x60EE).
unicode_unihan_variant(0x619D, kSemanticVariant, 0x8B48). %<kMatthews,kMeyerWempe 0x61DF<kMatthews
unicode_unihan_variant(0x61A4, kSimplifiedVariant, 0x6124).
unicode_unihan_variant(0x61A9, kSemanticVariant, 0x6187). %<kMatthews
unicode_unihan_variant(0x61A9, kZVariant, 0x6187).
unicode_unihan_variant(0x61AB, kSemanticVariant, 0x610D). %<kMatthews
unicode_unihan_variant(0x61AB, kSimplifiedVariant, 0x60AF).
unicode_unihan_variant(0x61AE, kSimplifiedVariant, 0x6003).
unicode_unihan_variant(0x61AF, kSemanticVariant, 0x39A7). %<kMeyerWempe
unicode_unihan_variant(0x61B0, kSemanticVariant, 0x8B4E). %<kMatthews
unicode_unihan_variant(0x61B2, kSimplifiedVariant, 0x5BAA).
unicode_unihan_variant(0x61B6, kSimplifiedVariant, 0x5FC6).
unicode_unihan_variant(0x61C0, kSimplifiedVariant, 0x22653).
unicode_unihan_variant(0x61C1, kSemanticVariant, 0x6081). %<kMeyerWempe
unicode_unihan_variant(0x61C3, kZVariant, 0x52E4).
unicode_unihan_variant(0x61C7, kSimplifiedVariant, 0x6073).
unicode_unihan_variant(0x61C9, kSimplifiedVariant, 0x5E94).
unicode_unihan_variant(0x61C9, kSpecializedSemanticVariant, 0x786C). %<kFenn
unicode_unihan_variant(0x61C9, kZVariant, 0x5FDC).
unicode_unihan_variant(0x61CC, kSimplifiedVariant, 0x603F).
unicode_unihan_variant(0x61CD, kSimplifiedVariant, 0x61D4).
unicode_unihan_variant(0x61D0, kZVariant, 0x61F7).
unicode_unihan_variant(0x61D1, kTraditionalVariant, 0x61E3).
unicode_unihan_variant(0x61D2, kTraditionalVariant, 0x61F6).
unicode_unihan_variant(0x61D4, kSemanticVariant, 0x51DB). %<kFenn
unicode_unihan_variant(0x61D4, kTraditionalVariant, 0x61CD).
unicode_unihan_variant(0x61DC, kSemanticVariant, 0x61F5). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x61DE, kZVariant, 0x8499).
unicode_unihan_variant(0x61DF, kSemanticVariant, 0x619D). %<kMatthews 0x8B48<kMatthews
unicode_unihan_variant(0x61DF, kSimplifiedVariant, 0x603C).
unicode_unihan_variant(0x61E3, kSemanticVariant, 0x3943). %<kMatthews 0x60B6<kLau,kMatthews
unicode_unihan_variant(0x61E3, kSimplifiedVariant, 0x61D1).
unicode_unihan_variant(0x61E5, kSemanticVariant, 0x61EB). %<kMatthews
unicode_unihan_variant(0x61E8, kSimplifiedVariant, 0x6079).
unicode_unihan_variant(0x61EB, kSemanticVariant, 0x61E5). %<kMatthews
unicode_unihan_variant(0x61EE, kSpecializedSemanticVariant, 0x6182). %<kHanYu
unicode_unihan_variant(0x61F2, kSemanticVariant, 0x60E9). %<kMatthews
unicode_unihan_variant(0x61F2, kSimplifiedVariant, 0x60E9).
unicode_unihan_variant(0x61F4, kZVariant, 0x61FA).
unicode_unihan_variant(0x61F5, kSemanticVariant, 0x61DC). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x61F6, kSimplifiedVariant, 0x61D2).
unicode_unihan_variant(0x61F7, kSemanticVariant, 0x6000). %<kLau,kMatthews 0x600C<kFenn
unicode_unihan_variant(0x61F7, kSimplifiedVariant, 0x6000).
unicode_unihan_variant(0x61F8, kSimplifiedVariant, 0x60AC).
unicode_unihan_variant(0x61FA, kSimplifiedVariant, 0x5FCF).
unicode_unihan_variant(0x61FA, kZVariant, 0x61F4).
unicode_unihan_variant(0x61FC, kSemanticVariant, 0x60E7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x61FC, kSimplifiedVariant, 0x60E7).
unicode_unihan_variant(0x61FD, kSemanticVariant, 0x6B61). %<kFenn 0x6B22<kFenn
unicode_unihan_variant(0x61FD, kZVariant, 0x6B61).
unicode_unihan_variant(0x61FE, kSimplifiedVariant, 0x6151).
unicode_unihan_variant(0x6200, kSemanticVariant, 0x604B). %<kMatthews
unicode_unihan_variant(0x6200, kSimplifiedVariant, 0x604B).
unicode_unihan_variant(0x6200, kSpecializedSemanticVariant, 0x604B). %<kFenn
unicode_unihan_variant(0x6200, kZVariant, 0xF990).
unicode_unihan_variant(0x6206, kTraditionalVariant, 0x6207).
unicode_unihan_variant(0x6207, kSimplifiedVariant, 0x6206).
unicode_unihan_variant(0x6209, kSemanticVariant, 0x925E). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x620B, kTraditionalVariant, 0x6214).
unicode_unihan_variant(0x620F, kTraditionalVariant, 0x6232).
unicode_unihan_variant(0x6214, kSimplifiedVariant, 0x620B).
unicode_unihan_variant(0x6217, kTraditionalVariant, 0x6227).
unicode_unihan_variant(0x6218, kTraditionalVariant, 0x6230).
unicode_unihan_variant(0x621B, kZVariant, 0x621E).
unicode_unihan_variant(0x621D, kZVariant, 0x8CCA).
unicode_unihan_variant(0x621E, kZVariant, 0x621B).
unicode_unihan_variant(0x6226, kZVariant, 0x6230).
unicode_unihan_variant(0x6227, kSemanticVariant, 0x5275). %<kMatthews
unicode_unihan_variant(0x6227, kSimplifiedVariant, 0x6217).
unicode_unihan_variant(0x6229, kSimplifiedVariant, 0x622C).
unicode_unihan_variant(0x622C, kTraditionalVariant, 0x6229).
unicode_unihan_variant(0x622E, kZVariant, 0xF9D2).
unicode_unihan_variant(0x622F, kSemanticVariant, 0x6232). %<kMatthews
unicode_unihan_variant(0x622F, kTraditionalVariant, 0x6231).
unicode_unihan_variant(0x622F, kZVariant, 0x6232).
unicode_unihan_variant(0x6230, kSimplifiedVariant, 0x6218).
unicode_unihan_variant(0x6230, kZVariant, 0x6226).
unicode_unihan_variant(0x6231, kSemanticVariant, 0x6232). %<kLau
unicode_unihan_variant(0x6231, kSimplifiedVariant, 0x622F).
unicode_unihan_variant(0x6231, kZVariant, 0x6232).
unicode_unihan_variant(0x6232, kSemanticVariant, 0x622F). %<kMatthews 0x6231<kLau
unicode_unihan_variant(0x6232, kSimplifiedVariant, 0x620F).
unicode_unihan_variant(0x6236, kSimplifiedVariant, 0x6237).
unicode_unihan_variant(0x6236, kZVariant, 0x6238).
unicode_unihan_variant(0x6237, kTraditionalVariant, 0x6236).
unicode_unihan_variant(0x6238, kZVariant, 0x6236).
unicode_unihan_variant(0x6239, kSemanticVariant, 0x5384). %<kMatthews
unicode_unihan_variant(0x623B, kZVariant, 0x623E).
unicode_unihan_variant(0x623C, kZVariant, 0x536F).
unicode_unihan_variant(0x623E, kZVariant, 0x623B).
unicode_unihan_variant(0x6241, kSpecializedSemanticVariant, 0x8251). %<kFenn
unicode_unihan_variant(0x6241, kZVariant, 0x78A5).
unicode_unihan_variant(0x6245, kSemanticVariant, 0x624A). %<kMatthews
unicode_unihan_variant(0x6247, kZVariant, 0x717D).
unicode_unihan_variant(0x624A, kSemanticVariant, 0x6245). %<kMatthews
unicode_unihan_variant(0x624B, kSemanticVariant, 0x624C). %<kMatthews
unicode_unihan_variant(0x624C, kSemanticVariant, 0x624B). %<kMatthews
unicode_unihan_variant(0x624D, kSpecializedSemanticVariant, 0x7E94). %<kFenn 0x8CA1<kFenn
unicode_unihan_variant(0x624E, kSemanticVariant, 0x7D2E). %<kMeyerWempe
unicode_unihan_variant(0x6250, kZVariant, 0x6738).
unicode_unihan_variant(0x6251, kSemanticVariant, 0x64B2). %<kLau,kMatthews
unicode_unihan_variant(0x6251, kTraditionalVariant, 0x64B2).
unicode_unihan_variant(0x6255, kZVariant, 0x62C2).
unicode_unihan_variant(0x6258, kSemanticVariant, 0x62D3). %<kLau,kMatthews
unicode_unihan_variant(0x6258, kSpecializedSemanticVariant, 0x62D3). %<kMeyerWempe
unicode_unihan_variant(0x625B, kZVariant, 0x6443).
unicode_unihan_variant(0x625E, kSemanticVariant, 0x634D). %<kMeyerWempe
unicode_unihan_variant(0x625E, kZVariant, 0x64C0).
unicode_unihan_variant(0x6260, kZVariant, 0x640B).
unicode_unihan_variant(0x6267, kSemanticVariant, 0x57F7). %<kFenn
unicode_unihan_variant(0x6267, kTraditionalVariant, 0x57F7).
unicode_unihan_variant(0x6269, kTraditionalVariant, 0x64F4).
unicode_unihan_variant(0x626A, kTraditionalVariant, 0x636B).
unicode_unihan_variant(0x626B, kTraditionalVariant, 0x6383).
unicode_unihan_variant(0x626C, kTraditionalVariant, 0x63DA).
unicode_unihan_variant(0x626F, kSemanticVariant, 0x64A6). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x626F, kZVariant, 0x64A6).
unicode_unihan_variant(0x6270, kTraditionalVariant, 0x64FE).
unicode_unihan_variant(0x6271, kZVariant, 0x63D2).
unicode_unihan_variant(0x6273, kSemanticVariant, 0x6500). %<kLau,kMatthews
unicode_unihan_variant(0x6275, kSemanticVariant, 0x4E8E). %<kLau 0x4E8F<kMatthews 0x65BC<kLau,kMatthews
unicode_unihan_variant(0x6275, kZVariant, 0x4E8E).
unicode_unihan_variant(0x627A, kZVariant, 0x62B5).
unicode_unihan_variant(0x627C, kSemanticVariant, 0x39D6). %<kMatthews 0x6424<kMatthews
unicode_unihan_variant(0x6283, kZVariant, 0x62DA).
unicode_unihan_variant(0x6289, kZVariant, 0x5214).
unicode_unihan_variant(0x628C, kSemanticVariant, 0x8200). %<kMatthews
unicode_unihan_variant(0x628D, kSemanticVariant, 0x62EF). %<kFenn
unicode_unihan_variant(0x628D, kSpecializedSemanticVariant, 0x57FA). %<kFenn
unicode_unihan_variant(0x6298, kSpecializedSemanticVariant, 0x7FFC). %<kFenn 0x62C3<kFenn
unicode_unihan_variant(0x629A, kTraditionalVariant, 0x64AB).
unicode_unihan_variant(0x629B, kTraditionalVariant, 0x62CB).
unicode_unihan_variant(0x629C, kZVariant, 0x62D4).
unicode_unihan_variant(0x629D, kSemanticVariant, 0x62D7). %<kLau
unicode_unihan_variant(0x629E, kZVariant, 0x64C7).
unicode_unihan_variant(0x629F, kTraditionalVariant, 0x6476).
unicode_unihan_variant(0x62A0, kTraditionalVariant, 0x6473).
unicode_unihan_variant(0x62A1, kTraditionalVariant, 0x6384).
unicode_unihan_variant(0x62A2, kTraditionalVariant, 0x6436).
unicode_unihan_variant(0x62A4, kTraditionalVariant, 0x8B77).
unicode_unihan_variant(0x62A5, kSemanticVariant, 0x5831). %<kMeyerWempe
unicode_unihan_variant(0x62A5, kTraditionalVariant, 0x5831).
unicode_unihan_variant(0x62AB, kSpecializedSemanticVariant, 0x34DF). %<kMeyerWempe
unicode_unihan_variant(0x62AC, kSemanticVariant, 0x64E1). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x62AC, kSpecializedSemanticVariant, 0x64E1). %<kHanYu
unicode_unihan_variant(0x62B1, kZVariant, 0x6294).
unicode_unihan_variant(0x62B5, kZVariant, 0x627A).
unicode_unihan_variant(0x62BB, kZVariant, 0x4F38).
unicode_unihan_variant(0x62BD, kSemanticVariant, 0x22B46). %<kFenn
unicode_unihan_variant(0x62BF, kSemanticVariant, 0x3A09). %<kMatthews
unicode_unihan_variant(0x62C2, kZVariant, 0x6255).
unicode_unihan_variant(0x62C3, kSpecializedSemanticVariant, 0x6298). %<kFenn
unicode_unihan_variant(0x62C5, kSemanticVariant, 0x64D4). %<kLau
unicode_unihan_variant(0x62C5, kSpecializedSemanticVariant, 0x64D4). %<kFenn
unicode_unihan_variant(0x62C5, kTraditionalVariant, 0x64D4).
unicode_unihan_variant(0x62C6, kSpecializedSemanticVariant, 0x8CAC). %<kFenn
unicode_unihan_variant(0x62C7, kSemanticVariant, 0x27FF9). %<kMeyerWempe
unicode_unihan_variant(0x62C8, kSpecializedSemanticVariant, 0x6541). %<kFenn
unicode_unihan_variant(0x62C9, kZVariant, 0xF925).
unicode_unihan_variant(0x62CA, kZVariant, 0x64AB).
unicode_unihan_variant(0x62CB, kSimplifiedVariant, 0x629B).
unicode_unihan_variant(0x62CF, kSemanticVariant, 0x62FF). %<kLau,kMatthews,kMeyerWempe 0x6310<kMatthews
unicode_unihan_variant(0x62CF, kZVariant, 0xF95B).
unicode_unihan_variant(0x62D3, kSemanticVariant, 0x6258). %<kLau,kMatthews
unicode_unihan_variant(0x62D3, kSpecializedSemanticVariant, 0x6258). %<kMeyerWempe
unicode_unihan_variant(0x62D3, kZVariant, 0xFA02).
unicode_unihan_variant(0x62D4, kZVariant, 0x629C).
unicode_unihan_variant(0x62D5, kSemanticVariant, 0x62D6). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x62D5, kZVariant, 0x62D6).
unicode_unihan_variant(0x62D6, kSemanticVariant, 0x62D5). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x62D6, kZVariant, 0x62D5).
unicode_unihan_variant(0x62D7, kSemanticVariant, 0x629D). %<kLau
unicode_unihan_variant(0x62DA, kSpecializedSemanticVariant, 0x5E06). %<kFenn
unicode_unihan_variant(0x62DA, kZVariant, 0x6283).
unicode_unihan_variant(0x62DC, kZVariant, 0x62DD).
unicode_unihan_variant(0x62DD, kZVariant, 0x62DC).
unicode_unihan_variant(0x62DF, kTraditionalVariant, 0x64EC).
unicode_unihan_variant(0x62E1, kZVariant, 0x64F4).
unicode_unihan_variant(0x62E2, kTraditionalVariant, 0x650F).
unicode_unihan_variant(0x62E3, kTraditionalVariant, 0x63C0).
unicode_unihan_variant(0x62E5, kTraditionalVariant, 0x64C1).
unicode_unihan_variant(0x62E6, kTraditionalVariant, 0x6514).
unicode_unihan_variant(0x62E7, kTraditionalVariant, 0x64F0).
unicode_unihan_variant(0x62E8, kTraditionalVariant, 0x64A5).
unicode_unihan_variant(0x62E9, kTraditionalVariant, 0x64C7).
unicode_unihan_variant(0x62ED, kSemanticVariant, 0x22091). %<kFenn
unicode_unihan_variant(0x62EF, kSemanticVariant, 0x628D). %<kFenn
unicode_unihan_variant(0x62EF, kSpecializedSemanticVariant, 0x57FA). %<kFenn
unicode_unihan_variant(0x62F4, kZVariant, 0x63CE).
unicode_unihan_variant(0x62FC, kSemanticVariant, 0x6452). %<kMatthews
unicode_unihan_variant(0x62FC, kSpecializedSemanticVariant, 0x79C9). %<kMeyerWempe
unicode_unihan_variant(0x62FD, kSpecializedSemanticVariant, 0x66F3). %<kFenn
unicode_unihan_variant(0x62FE, kSpecializedSemanticVariant, 0x5341).
unicode_unihan_variant(0x62FF, kSemanticVariant, 0x62CF). %<kLau,kMatthews,kMeyerWempe 0x6310<kMatthews
unicode_unihan_variant(0x62FF, kZVariant, 0xF95B).
unicode_unihan_variant(0x6302, kSemanticVariant, 0x639B). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6302, kTraditionalVariant, 0x639B).
unicode_unihan_variant(0x6305, kZVariant, 0x6306).
unicode_unihan_variant(0x6306, kZVariant, 0x6305).
unicode_unihan_variant(0x6309, kSpecializedSemanticVariant, 0x6435). %<kFenn
unicode_unihan_variant(0x6310, kSemanticVariant, 0x62CF). %<kMatthews 0x62FF<kMatthews
unicode_unihan_variant(0x6319, kZVariant, 0x8209).
unicode_unihan_variant(0x631A, kTraditionalVariant, 0x646F).
unicode_unihan_variant(0x631B, kTraditionalVariant, 0x6523).
unicode_unihan_variant(0x631C, kTraditionalVariant, 0x6397).
unicode_unihan_variant(0x631D, kTraditionalVariant, 0x64BE).
unicode_unihan_variant(0x631E, kTraditionalVariant, 0x64BB).
unicode_unihan_variant(0x631F, kTraditionalVariant, 0x633E).
unicode_unihan_variant(0x6320, kTraditionalVariant, 0x6493).
unicode_unihan_variant(0x6321, kTraditionalVariant, 0x64CB).
unicode_unihan_variant(0x6322, kTraditionalVariant, 0x649F).
unicode_unihan_variant(0x6323, kTraditionalVariant, 0x6399).
unicode_unihan_variant(0x6324, kTraditionalVariant, 0x64E0).
unicode_unihan_variant(0x6325, kTraditionalVariant, 0x63EE).
unicode_unihan_variant(0x6326, kTraditionalVariant, 0x648F).
unicode_unihan_variant(0x6329, kSimplifiedVariant, 0x635D).
unicode_unihan_variant(0x632B, kSemanticVariant, 0x7E72). %<kMatthews
unicode_unihan_variant(0x632D, kSemanticVariant, 0x9ABE). %<kMatthews 0x9BC1<kMatthews
unicode_unihan_variant(0x6331, kSemanticVariant, 0x6332). %<kMatthews
unicode_unihan_variant(0x6332, kSemanticVariant, 0x6331). %<kMatthews
unicode_unihan_variant(0x6337, kSemanticVariant, 0x6412). %<kMeyerWempe
unicode_unihan_variant(0x633C, kSemanticVariant, 0x637C). %<kMatthews
unicode_unihan_variant(0x633D, kSpecializedSemanticVariant, 0x6387). %<kMeyerWempe
unicode_unihan_variant(0x633D, kTraditionalVariant, 0x8F13).
unicode_unihan_variant(0x633E, kSimplifiedVariant, 0x631F).
unicode_unihan_variant(0x6346, kSemanticVariant, 0x7D91). %<kLau
unicode_unihan_variant(0x6346, kZVariant, 0x7D91).
unicode_unihan_variant(0x6349, kSpecializedSemanticVariant, 0x6753). %<kMeyerWempe
unicode_unihan_variant(0x634C, kSemanticVariant, 0x516B). %<kLau,kMeyerWempe
unicode_unihan_variant(0x634C, kSpecializedSemanticVariant, 0x516B).
unicode_unihan_variant(0x634D, kSemanticVariant, 0x625E). %<kMeyerWempe
unicode_unihan_variant(0x634D, kZVariant, 0x625E).
unicode_unihan_variant(0x634F, kSemanticVariant, 0x63D1). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6353, kSemanticVariant, 0x63F6). %<kLau,kMatthews
unicode_unihan_variant(0x635C, kZVariant, 0x641C).
unicode_unihan_variant(0x635D, kTraditionalVariant, 0x6329).
unicode_unihan_variant(0x635E, kTraditionalVariant, 0x6488).
unicode_unihan_variant(0x635F, kTraditionalVariant, 0x640D).
unicode_unihan_variant(0x6361, kTraditionalVariant, 0x64BF).
unicode_unihan_variant(0x6362, kTraditionalVariant, 0x63DB).
unicode_unihan_variant(0x6363, kTraditionalVariant, 0x6417).
unicode_unihan_variant(0x6366, kSemanticVariant, 0x64D2). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6368, kSimplifiedVariant, 0x820D).
unicode_unihan_variant(0x6368, kSpecializedSemanticVariant, 0x820D). %<kMeyerWempe
unicode_unihan_variant(0x636B, kSimplifiedVariant, 0x626A).
unicode_unihan_variant(0x636C, kSemanticVariant, 0x64AB). %<kMatthews
unicode_unihan_variant(0x636E, kTraditionalVariant, 0x64DA).
unicode_unihan_variant(0x6372, kZVariant, 0x5377).
unicode_unihan_variant(0x6374, kZVariant, 0x7E3D).
unicode_unihan_variant(0x6375, kSemanticVariant, 0x4940). %<kFenn
unicode_unihan_variant(0x6376, kSemanticVariant, 0x7BA0). %<kMeyerWempe
unicode_unihan_variant(0x6376, kZVariant, 0x6425).
unicode_unihan_variant(0x637B, kZVariant, 0xF9A4).
unicode_unihan_variant(0x637C, kSemanticVariant, 0x633C). %<kMatthews
unicode_unihan_variant(0x6382, kSemanticVariant, 0x6541). %<kMatthews
unicode_unihan_variant(0x6383, kSemanticVariant, 0x57FD). %<kMeyerWempe
unicode_unihan_variant(0x6383, kSimplifiedVariant, 0x626B).
unicode_unihan_variant(0x6384, kSimplifiedVariant, 0x62A1).
unicode_unihan_variant(0x6386, kSimplifiedVariant, 0x39CF).
unicode_unihan_variant(0x6387, kSpecializedSemanticVariant, 0x633D). %<kMeyerWempe
unicode_unihan_variant(0x6389, kSpecializedSemanticVariant, 0x68F9). %<kMeyerWempe
unicode_unihan_variant(0x638F, kSemanticVariant, 0x642F). %<kLau,kMatthews
unicode_unihan_variant(0x638F, kZVariant, 0x642F).
unicode_unihan_variant(0x6390, kSemanticVariant, 0x2073C). %<kFenn
unicode_unihan_variant(0x6397, kSimplifiedVariant, 0x631C).
unicode_unihan_variant(0x6398, kZVariant, 0x6485).
unicode_unihan_variant(0x6399, kSimplifiedVariant, 0x6323).
unicode_unihan_variant(0x639B, kSemanticVariant, 0x6302). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x639B, kSimplifiedVariant, 0x6302).
unicode_unihan_variant(0x63A0, kZVariant, 0xF975).
unicode_unihan_variant(0x63A1, kSimplifiedVariant, 0x91C7).
unicode_unihan_variant(0x63A9, kSemanticVariant, 0x63DC). %<kLau,kMatthews
unicode_unihan_variant(0x63A9, kSpecializedSemanticVariant, 0x63DE). %<kMeyerWempe
unicode_unihan_variant(0x63AC, kSemanticVariant, 0x530A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x63B2, kZVariant, 0x63ED).
unicode_unihan_variant(0x63B3, kTraditionalVariant, 0x64C4).
unicode_unihan_variant(0x63B4, kTraditionalVariant, 0x6451).
unicode_unihan_variant(0x63B7, kTraditionalVariant, 0x64F2).
unicode_unihan_variant(0x63B8, kTraditionalVariant, 0x64A3).
unicode_unihan_variant(0x63BA, kTraditionalVariant, 0x647B).
unicode_unihan_variant(0x63BB, kZVariant, 0x6414).
unicode_unihan_variant(0x63BC, kTraditionalVariant, 0x645C).
unicode_unihan_variant(0x63BD, kSemanticVariant, 0x78B0). %<kFenn
unicode_unihan_variant(0x63C0, kSimplifiedVariant, 0x62E3).
unicode_unihan_variant(0x63C5, kSemanticVariant, 0x7814). %<kMatthews
unicode_unihan_variant(0x63C5, kSpecializedSemanticVariant, 0x7814). %<kMeyerWempe
unicode_unihan_variant(0x63CE, kZVariant, 0x62F4).
unicode_unihan_variant(0x63D1, kSemanticVariant, 0x634F). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x63D2, kZVariant, 0x6271).
unicode_unihan_variant(0x63DA, kSimplifiedVariant, 0x626C).
unicode_unihan_variant(0x63DB, kSimplifiedVariant, 0x6362).
unicode_unihan_variant(0x63DC, kSemanticVariant, 0x63A9). %<kLau,kMatthews
unicode_unihan_variant(0x63DC, kSpecializedSemanticVariant, 0x63A9). %<kMeyerWempe
unicode_unihan_variant(0x63DD, kSemanticVariant, 0x648D). %<kMatthews
unicode_unihan_variant(0x63DD, kSpecializedSemanticVariant, 0x6522). %<kFenn
unicode_unihan_variant(0x63DE, kSpecializedSemanticVariant, 0x63A9). %<kMeyerWempe
unicode_unihan_variant(0x63E6, kSemanticVariant, 0x650B). %<kMatthews
unicode_unihan_variant(0x63EA, kSemanticVariant, 0x63EB). %<kMeyerWempe
unicode_unihan_variant(0x63EB, kSemanticVariant, 0x63EA). %<kMeyerWempe
unicode_unihan_variant(0x63ED, kSpecializedSemanticVariant, 0x7D50). %<kFenn
unicode_unihan_variant(0x63ED, kZVariant, 0x63B2).
unicode_unihan_variant(0x63EE, kSimplifiedVariant, 0x6325).
unicode_unihan_variant(0x63EF, kZVariant, 0x6404).
unicode_unihan_variant(0x63F6, kSemanticVariant, 0x6353). %<kLau,kMatthews
unicode_unihan_variant(0x63F7, kZVariant, 0x63D2).
unicode_unihan_variant(0x63F8, kSemanticVariant, 0x6942). %<kLau 0x6463<kMatthews
unicode_unihan_variant(0x63FA, kZVariant, 0x6416).
unicode_unihan_variant(0x63FD, kTraditionalVariant, 0x652C).
unicode_unihan_variant(0x63FE, kTraditionalVariant, 0x6435).
unicode_unihan_variant(0x63FF, kTraditionalVariant, 0x64B3).
unicode_unihan_variant(0x6400, kTraditionalVariant, 0x6519).
unicode_unihan_variant(0x6401, kTraditionalVariant, 0x64F1).
unicode_unihan_variant(0x6402, kTraditionalVariant, 0x645F).
unicode_unihan_variant(0x6403, kSemanticVariant, 0x7E3D). %<kFenn 0x6460<kFenn
unicode_unihan_variant(0x6403, kZVariant, 0x7E3D).
unicode_unihan_variant(0x6405, kTraditionalVariant, 0x652A).
unicode_unihan_variant(0x6406, kZVariant, 0x69CB).
unicode_unihan_variant(0x6407, kSemanticVariant, 0x64B3). %<kMatthews
unicode_unihan_variant(0x6409, kSemanticVariant, 0x3A41). %<kMatthews
unicode_unihan_variant(0x640D, kSimplifiedVariant, 0x635F).
unicode_unihan_variant(0x6412, kSemanticVariant, 0x6337). %<kMeyerWempe
unicode_unihan_variant(0x6414, kZVariant, 0x63BB).
unicode_unihan_variant(0x6416, kSemanticVariant, 0x6447). %<kMatthews
unicode_unihan_variant(0x6416, kSimplifiedVariant, 0x6447).
unicode_unihan_variant(0x6416, kZVariant, 0x63FA).
unicode_unihan_variant(0x6417, kSemanticVariant, 0x64E3). %<kLau,kMatthews
unicode_unihan_variant(0x6417, kSimplifiedVariant, 0x6363).
unicode_unihan_variant(0x6417, kZVariant, 0x64E3).
unicode_unihan_variant(0x641C, kSemanticVariant, 0x22BF1). %<kHanYu 0x22CB7<kFenn
unicode_unihan_variant(0x641C, kZVariant, 0x635C).
unicode_unihan_variant(0x641E, kSemanticVariant, 0x652A). %<kLau
unicode_unihan_variant(0x6424, kSemanticVariant, 0x39D6). %<kMatthews 0x627C<kMatthews
unicode_unihan_variant(0x6425, kZVariant, 0x6376).
unicode_unihan_variant(0x6427, kZVariant, 0x717D).
unicode_unihan_variant(0x6428, kZVariant, 0x642D).
unicode_unihan_variant(0x642A, kSemanticVariant, 0x508F). %<kMatthews
unicode_unihan_variant(0x642B, kSemanticVariant, 0x642C). %<kMeyerWempe
unicode_unihan_variant(0x642C, kSemanticVariant, 0x642B). %<kMeyerWempe
unicode_unihan_variant(0x642D, kSemanticVariant, 0x6498). %<kMeyerWempe
unicode_unihan_variant(0x642D, kZVariant, 0x6428).
unicode_unihan_variant(0x642F, kSemanticVariant, 0x638F). %<kLau,kMatthews
unicode_unihan_variant(0x642F, kZVariant, 0x638F).
unicode_unihan_variant(0x6435, kSimplifiedVariant, 0x63FE).
unicode_unihan_variant(0x6435, kSpecializedSemanticVariant, 0x6309). %<kFenn
unicode_unihan_variant(0x6436, kSimplifiedVariant, 0x62A2).
unicode_unihan_variant(0x643A, kSemanticVariant, 0x651C). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x643A, kTraditionalVariant, 0x651C).
unicode_unihan_variant(0x6441, kSemanticVariant, 0x6069). %<kMatthews
unicode_unihan_variant(0x6442, kZVariant, 0x651D).
unicode_unihan_variant(0x6443, kZVariant, 0x625B).
unicode_unihan_variant(0x6444, kTraditionalVariant, 0x651D).
unicode_unihan_variant(0x6445, kTraditionalVariant, 0x6504).
unicode_unihan_variant(0x6446, kTraditionalVariant, 0x64FA).
unicode_unihan_variant(0x6447, kSemanticVariant, 0x6416). %<kMatthews
unicode_unihan_variant(0x6447, kTraditionalVariant, 0x6416).
unicode_unihan_variant(0x6448, kTraditionalVariant, 0x64EF).
unicode_unihan_variant(0x644A, kTraditionalVariant, 0x6524).
unicode_unihan_variant(0x644B, kSimplifiedVariant, 0x22AEC).
unicode_unihan_variant(0x6451, kSimplifiedVariant, 0x63B4).
unicode_unihan_variant(0x6452, kSemanticVariant, 0x62FC). %<kMatthews
unicode_unihan_variant(0x6452, kZVariant, 0x5C4F).
unicode_unihan_variant(0x6454, kSpecializedSemanticVariant, 0x7529). %<kFenn
unicode_unihan_variant(0x645C, kSimplifiedVariant, 0x63BC).
unicode_unihan_variant(0x645F, kSimplifiedVariant, 0x6402).
unicode_unihan_variant(0x6460, kSemanticVariant, 0x6403). %<kFenn 0x7E3D<kFenn
unicode_unihan_variant(0x6461, kSemanticVariant, 0x6E89). %<kMatthews
unicode_unihan_variant(0x6463, kSemanticVariant, 0x63F8). %<kMatthews
unicode_unihan_variant(0x646D, kSpecializedSemanticVariant, 0x62D3). %<kMeyerWempe
unicode_unihan_variant(0x646D, kZVariant, 0x62D3).
unicode_unihan_variant(0x646E, kSemanticVariant, 0x22CC6). %<kMeyerWempe
unicode_unihan_variant(0x646F, kSimplifiedVariant, 0x631A).
unicode_unihan_variant(0x6473, kSimplifiedVariant, 0x62A0).
unicode_unihan_variant(0x6474, kSpecializedSemanticVariant, 0x8212).
unicode_unihan_variant(0x6476, kSimplifiedVariant, 0x629F).
unicode_unihan_variant(0x647B, kSimplifiedVariant, 0x63BA).
unicode_unihan_variant(0x6482, kSemanticVariant, 0x3A3C). %<kFenn
unicode_unihan_variant(0x6483, kZVariant, 0x64CA).
unicode_unihan_variant(0x6484, kTraditionalVariant, 0x6516).
unicode_unihan_variant(0x6485, kZVariant, 0x6398).
unicode_unihan_variant(0x6486, kSemanticVariant, 0x6487). %<kFenn,kMeyerWempe
unicode_unihan_variant(0x6487, kSemanticVariant, 0x6486). %<kFenn,kMeyerWempe
unicode_unihan_variant(0x6488, kSimplifiedVariant, 0x635E).
unicode_unihan_variant(0x648D, kSemanticVariant, 0x63DD). %<kMatthews
unicode_unihan_variant(0x648D, kSpecializedSemanticVariant, 0x6522). %<kFenn
unicode_unihan_variant(0x648F, kSimplifiedVariant, 0x6326).
unicode_unihan_variant(0x6490, kSemanticVariant, 0x6491). %<kFenn
unicode_unihan_variant(0x6490, kSimplifiedVariant, 0x6491).
unicode_unihan_variant(0x6491, kSemanticVariant, 0x6490). %<kFenn
unicode_unihan_variant(0x6491, kTraditionalVariant, 0x6490).
unicode_unihan_variant(0x6493, kSimplifiedVariant, 0x6320).
unicode_unihan_variant(0x6498, kSemanticVariant, 0x642D). %<kMeyerWempe
unicode_unihan_variant(0x649A, kZVariant, 0xF991).
unicode_unihan_variant(0x649D, kSimplifiedVariant, 0x39D1).
unicode_unihan_variant(0x649F, kSemanticVariant, 0x5B0C). %<kMatthews
unicode_unihan_variant(0x649F, kSimplifiedVariant, 0x6322).
unicode_unihan_variant(0x64A2, kSpecializedSemanticVariant, 0x64A3).
unicode_unihan_variant(0x64A3, kSimplifiedVariant, 0x63B8).
unicode_unihan_variant(0x64A3, kSpecializedSemanticVariant, 0x64A2).
unicode_unihan_variant(0x64A5, kSimplifiedVariant, 0x62E8).
unicode_unihan_variant(0x64A6, kSemanticVariant, 0x626F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x64A6, kZVariant, 0x626F).
unicode_unihan_variant(0x64AB, kSemanticVariant, 0x636C). %<kMatthews
unicode_unihan_variant(0x64AB, kSimplifiedVariant, 0x629A).
unicode_unihan_variant(0x64AB, kZVariant, 0x62CA).
unicode_unihan_variant(0x64B2, kSemanticVariant, 0x6251). %<kLau,kMatthews
unicode_unihan_variant(0x64B2, kSimplifiedVariant, 0x6251).
unicode_unihan_variant(0x64B2, kZVariant, 0x6534).
unicode_unihan_variant(0x64B3, kSemanticVariant, 0x6407). %<kMatthews
unicode_unihan_variant(0x64B3, kSimplifiedVariant, 0x63FF).
unicode_unihan_variant(0x64B5, kTraditionalVariant, 0x6506).
unicode_unihan_variant(0x64B7, kTraditionalVariant, 0x64F7).
unicode_unihan_variant(0x64B8, kTraditionalVariant, 0x64FC).
unicode_unihan_variant(0x64B9, kZVariant, 0x652A).
unicode_unihan_variant(0x64BA, kTraditionalVariant, 0x651B).
unicode_unihan_variant(0x64BB, kSimplifiedVariant, 0x631E).
unicode_unihan_variant(0x64BE, kSimplifiedVariant, 0x631D).
unicode_unihan_variant(0x64BF, kSimplifiedVariant, 0x6361).
unicode_unihan_variant(0x64C0, kZVariant, 0x625E).
unicode_unihan_variant(0x64C1, kSimplifiedVariant, 0x62E5).
unicode_unihan_variant(0x64C3, kSemanticVariant, 0x652E). %<kMeyerWempe
unicode_unihan_variant(0x64C4, kSemanticVariant, 0x865C). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x64C4, kSimplifiedVariant, 0x63B3).
unicode_unihan_variant(0x64C4, kSpecializedSemanticVariant, 0x865C). %<kFenn
unicode_unihan_variant(0x64C4, kZVariant, 0xF930).
unicode_unihan_variant(0x64C7, kSimplifiedVariant, 0x62E9).
unicode_unihan_variant(0x64C7, kZVariant, 0x629E).
unicode_unihan_variant(0x64CA, kSimplifiedVariant, 0x51FB).
unicode_unihan_variant(0x64CA, kZVariant, 0x6483).
unicode_unihan_variant(0x64CB, kSemanticVariant, 0x6529). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x64CB, kSimplifiedVariant, 0x6321).
unicode_unihan_variant(0x64D2, kSemanticVariant, 0x6366). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x64D3, kSimplifiedVariant, 0x39DF).
unicode_unihan_variant(0x64D4, kSemanticVariant, 0x62C5). %<kLau
unicode_unihan_variant(0x64D4, kSimplifiedVariant, 0x62C5).
unicode_unihan_variant(0x64D4, kSpecializedSemanticVariant, 0x62C5). %<kFenn
unicode_unihan_variant(0x64D7, kSemanticVariant, 0x5288). %<kMeyerWempe
unicode_unihan_variant(0x64DA, kSimplifiedVariant, 0x636E).
unicode_unihan_variant(0x64DA, kZVariant, 0x62E0).
unicode_unihan_variant(0x64DE, kTraditionalVariant, 0x64FB).
unicode_unihan_variant(0x64E0, kSimplifiedVariant, 0x6324).
unicode_unihan_variant(0x64E1, kSemanticVariant, 0x62AC). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x64E1, kSpecializedSemanticVariant, 0x62AC). %<kHanYu
unicode_unihan_variant(0x64E3, kSemanticVariant, 0x6417). %<kLau,kMatthews
unicode_unihan_variant(0x64E3, kSimplifiedVariant, 0x22B4F).
unicode_unihan_variant(0x64E3, kZVariant, 0x6417).
unicode_unihan_variant(0x64E5, kSemanticVariant, 0x3A5C). %<kMeyerWempe 0x652C<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x64E7, kSemanticVariant, 0x8209). %<kHKGlyph
unicode_unihan_variant(0x64E7, kZVariant, 0x8209).
unicode_unihan_variant(0x64EC, kSimplifiedVariant, 0x62DF).
unicode_unihan_variant(0x64EC, kZVariant, 0x5117).
unicode_unihan_variant(0x64EF, kSimplifiedVariant, 0x6448).
unicode_unihan_variant(0x64F0, kSimplifiedVariant, 0x62E7).
unicode_unihan_variant(0x64F1, kSimplifiedVariant, 0x6401).
unicode_unihan_variant(0x64F2, kSemanticVariant, 0x64FF). %<kTang
unicode_unihan_variant(0x64F2, kSimplifiedVariant, 0x63B7).
unicode_unihan_variant(0x64F4, kSimplifiedVariant, 0x6269).
unicode_unihan_variant(0x64F7, kSimplifiedVariant, 0x64B7).
unicode_unihan_variant(0x64F8, kSemanticVariant, 0x39DC). %<kMatthews
unicode_unihan_variant(0x64FA, kSimplifiedVariant, 0x6446).
unicode_unihan_variant(0x64FB, kSimplifiedVariant, 0x64DE).
unicode_unihan_variant(0x64FC, kSimplifiedVariant, 0x64B8).
unicode_unihan_variant(0x64FD, kSimplifiedVariant, 0x39F0).
unicode_unihan_variant(0x64FE, kSimplifiedVariant, 0x6270).
unicode_unihan_variant(0x64FF, kSemanticVariant, 0x64F2). %<kTang
unicode_unihan_variant(0x6500, kSemanticVariant, 0x3434). %0x6273<kLau,kMatthews
unicode_unihan_variant(0x6504, kSimplifiedVariant, 0x6445).
unicode_unihan_variant(0x6505, kSemanticVariant, 0x6522). %<kMeyerWempe
unicode_unihan_variant(0x6505, kZVariant, 0x6522).
unicode_unihan_variant(0x6506, kSimplifiedVariant, 0x64B5).
unicode_unihan_variant(0x650B, kSemanticVariant, 0x63E6). %<kMatthews
unicode_unihan_variant(0x650F, kSimplifiedVariant, 0x62E2).
unicode_unihan_variant(0x6512, kTraditionalVariant, 0x6522).
unicode_unihan_variant(0x6514, kSimplifiedVariant, 0x62E6).
unicode_unihan_variant(0x6516, kSimplifiedVariant, 0x6484).
unicode_unihan_variant(0x6519, kSimplifiedVariant, 0x6400).
unicode_unihan_variant(0x651B, kSimplifiedVariant, 0x64BA).
unicode_unihan_variant(0x651C, kSemanticVariant, 0x643A). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x651C, kSimplifiedVariant, 0x643A).
unicode_unihan_variant(0x651D, kSimplifiedVariant, 0x6444).
unicode_unihan_variant(0x651D, kZVariant, 0x6442).
unicode_unihan_variant(0x6522, kSemanticVariant, 0x6505). %<kMeyerWempe
unicode_unihan_variant(0x6522, kSimplifiedVariant, 0x6512).
unicode_unihan_variant(0x6522, kSpecializedSemanticVariant, 0x63DD). %<kFenn 0x648D<kFenn
unicode_unihan_variant(0x6522, kZVariant, 0x6505).
unicode_unihan_variant(0x6523, kSimplifiedVariant, 0x631B).
unicode_unihan_variant(0x6524, kSimplifiedVariant, 0x644A).
unicode_unihan_variant(0x6529, kSemanticVariant, 0x64CB). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x652A, kSemanticVariant, 0x641E). %<kLau
unicode_unihan_variant(0x652A, kSimplifiedVariant, 0x6405).
unicode_unihan_variant(0x652C, kSemanticVariant, 0x3A5C). %<kMeyerWempe 0x64E5<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x652C, kSimplifiedVariant, 0x63FD).
unicode_unihan_variant(0x652E, kSemanticVariant, 0x64C3). %<kMeyerWempe
unicode_unihan_variant(0x6534, kSemanticVariant, 0x6535). %<kMatthews
unicode_unihan_variant(0x6534, kZVariant, 0x64B2).
unicode_unihan_variant(0x6535, kSemanticVariant, 0x6534). %<kMatthews
unicode_unihan_variant(0x6535, kZVariant, 0x590A).
unicode_unihan_variant(0x6536, kSemanticVariant, 0x39C3). %<kMatthews 0x53CE<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6536, kZVariant, 0x53CE).
unicode_unihan_variant(0x6537, kSemanticVariant, 0x8003). %<kMatthews
unicode_unihan_variant(0x6537, kZVariant, 0x8003).
unicode_unihan_variant(0x6541, kSemanticVariant, 0x6382). %<kMatthews
unicode_unihan_variant(0x6541, kSpecializedSemanticVariant, 0x62C8). %<kFenn
unicode_unihan_variant(0x6543, kSemanticVariant, 0x668B). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6548, kSemanticVariant, 0x52B9). %<kMeyerWempe
unicode_unihan_variant(0x6548, kZVariant, 0x52B9).
unicode_unihan_variant(0x6549, kSemanticVariant, 0x4F8E). %<kMatthews
unicode_unihan_variant(0x654C, kTraditionalVariant, 0x6575).
unicode_unihan_variant(0x654D, kSemanticVariant, 0x53D9). %<kFenn 0x6558<kHKGlyph,kMeyerWempe
unicode_unihan_variant(0x654D, kZVariant, 0x53D9).
unicode_unihan_variant(0x654E, kSemanticVariant, 0x6559). %<kLau,kMatthews
unicode_unihan_variant(0x654E, kZVariant, 0x6559).
unicode_unihan_variant(0x654F, kSemanticVariant, 0x52C4). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6553, kZVariant, 0x655A).
unicode_unihan_variant(0x6555, kSemanticVariant, 0x52C5). %<kMatthews 0x52D1<kMatthews
unicode_unihan_variant(0x6555, kSpecializedSemanticVariant, 0x52C5). %<kMeyerWempe
unicode_unihan_variant(0x6557, kSemanticVariant, 0x4899). %<kMatthews
unicode_unihan_variant(0x6557, kSimplifiedVariant, 0x8D25).
unicode_unihan_variant(0x6558, kSemanticVariant, 0x53D9). %<kHKGlyph,kLau,kMatthews 0x654D<kHKGlyph,kMeyerWempe
unicode_unihan_variant(0x6558, kSimplifiedVariant, 0x53D9).
unicode_unihan_variant(0x6558, kZVariant, 0x654D).
unicode_unihan_variant(0x6559, kSemanticVariant, 0x654E). %<kLau,kMatthews
unicode_unihan_variant(0x6559, kZVariant, 0x654E).
unicode_unihan_variant(0x655A, kZVariant, 0x6553).
unicode_unihan_variant(0x655B, kTraditionalVariant, 0x6582).
unicode_unihan_variant(0x6560, kSemanticVariant, 0x656A). %<kMatthews
unicode_unihan_variant(0x6560, kZVariant, 0x656A).
unicode_unihan_variant(0x6566, kSemanticVariant, 0x60C7). %<kLau,kMatthews
unicode_unihan_variant(0x6569, kTraditionalVariant, 0x6586).
unicode_unihan_variant(0x6569, kZVariant, 0x6585).
unicode_unihan_variant(0x656A, kSemanticVariant, 0x6560). %<kMatthews
unicode_unihan_variant(0x6570, kTraditionalVariant, 0x6578).
unicode_unihan_variant(0x6575, kSimplifiedVariant, 0x654C).
unicode_unihan_variant(0x6577, kSemanticVariant, 0x5C03). %<kMatthews 0x65C9<kMatthews
unicode_unihan_variant(0x6578, kSimplifiedVariant, 0x6570).
unicode_unihan_variant(0x657A, kSemanticVariant, 0x9A45). %<kLau,kMatthews
unicode_unihan_variant(0x657B, kZVariant, 0x5910).
unicode_unihan_variant(0x657D, kZVariant, 0x657F).
unicode_unihan_variant(0x657F, kZVariant, 0x657D).
unicode_unihan_variant(0x6581, kSemanticVariant, 0x24895). %<kMeyerWempe
unicode_unihan_variant(0x6582, kSimplifiedVariant, 0x655B).
unicode_unihan_variant(0x6583, kSimplifiedVariant, 0x6BD9).
unicode_unihan_variant(0x6585, kSemanticVariant, 0x6586). %<kMatthews
unicode_unihan_variant(0x6585, kSimplifiedVariant, 0x22F7E).
unicode_unihan_variant(0x6585, kZVariant, 0x6586).
unicode_unihan_variant(0x6586, kSemanticVariant, 0x6585). %<kMatthews 0x93F0<kMatthews
unicode_unihan_variant(0x6586, kSimplifiedVariant, 0x6569).
unicode_unihan_variant(0x6586, kZVariant, 0x6585).
unicode_unihan_variant(0x6587, kSpecializedSemanticVariant, 0x349A). %<kFenn 0x7A69<kFenn
unicode_unihan_variant(0x6588, kSemanticVariant, 0x5B78). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6588, kZVariant, 0x5B78).
unicode_unihan_variant(0x6589, kSemanticVariant, 0x9F4A). %<kMatthews
unicode_unihan_variant(0x6589, kZVariant, 0x9F4A).
unicode_unihan_variant(0x658B, kSemanticVariant, 0x9F4B). %<kLau,kMatthews
unicode_unihan_variant(0x658B, kSpecializedSemanticVariant, 0x9F4A). %<kFenn
unicode_unihan_variant(0x658B, kTraditionalVariant, 0x9F4B).
unicode_unihan_variant(0x658C, kSemanticVariant, 0x5F6C). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x658D, kSpecializedSemanticVariant, 0x89BA). %<kFenn
unicode_unihan_variant(0x658D, kZVariant, 0x89BA).
unicode_unihan_variant(0x658E, kZVariant, 0x9F4B).
unicode_unihan_variant(0x6593, kTraditionalVariant, 0x6595).
unicode_unihan_variant(0x6595, kSimplifiedVariant, 0x6593).
unicode_unihan_variant(0x6597, kTraditionalVariant, 0x9B25).
unicode_unihan_variant(0x659A, kSemanticVariant, 0x659D). %<kMatthews
unicode_unihan_variant(0x659D, kSemanticVariant, 0x659A). %<kMatthews
unicode_unihan_variant(0x659D, kSpecializedSemanticVariant, 0x6A9F). %<kMeyerWempe
unicode_unihan_variant(0x659E, kSemanticVariant, 0x3531). %<kMatthews 0x5EBE<kMatthews
unicode_unihan_variant(0x65A4, kSemanticVariant, 0x89D4). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x65A4, kZVariant, 0x89D4).
unicode_unihan_variant(0x65A5, kSemanticVariant, 0x387F). %<kMatthews
unicode_unihan_variant(0x65A9, kTraditionalVariant, 0x65AC).
unicode_unihan_variant(0x65AC, kSimplifiedVariant, 0x65A9).
unicode_unihan_variant(0x65AD, kSemanticVariant, 0x65B7). %<kFenn
unicode_unihan_variant(0x65AD, kTraditionalVariant, 0x65B7).
unicode_unihan_variant(0x65B2, kSemanticVariant, 0x34F8). %<kMatthews 0x65B5<kMatthews
unicode_unihan_variant(0x65B5, kSemanticVariant, 0x65B2). %<kMatthews
unicode_unihan_variant(0x65B7, kSemanticVariant, 0x65AD). %<kFenn
unicode_unihan_variant(0x65B7, kSimplifiedVariant, 0x65AD).
unicode_unihan_variant(0x65BC, kSemanticVariant, 0x4E8E). %<kLau 0x4E8F<kMatthews 0x6275<kLau,kMatthews
unicode_unihan_variant(0x65BC, kSimplifiedVariant, 0x4E8E).
unicode_unihan_variant(0x65BE, kSemanticVariant, 0x65C6). %<kMatthews
unicode_unihan_variant(0x65C1, kSpecializedSemanticVariant, 0x508D). %<kFenn
unicode_unihan_variant(0x65C2, kSemanticVariant, 0x65D7). %<kLau,kMatthews
unicode_unihan_variant(0x65C5, kZVariant, 0xF983).
unicode_unihan_variant(0x65C6, kSemanticVariant, 0x65BE). %<kMatthews
unicode_unihan_variant(0x65C8, kSemanticVariant, 0x65D2). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x65C9, kSemanticVariant, 0x5C03). %<kMatthews 0x6577<kMatthews
unicode_unihan_variant(0x65D2, kSemanticVariant, 0x65C8). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x65D7, kSemanticVariant, 0x65C2). %<kLau,kMatthews
unicode_unihan_variant(0x65D8, kSemanticVariant, 0x5E5F). %<kMatthews
unicode_unihan_variant(0x65DB, kSemanticVariant, 0x5E61). %<kFenn
unicode_unihan_variant(0x65E0, kSemanticVariant, 0x7121). %<kFenn
unicode_unihan_variant(0x65E0, kTraditionalVariant, 0x7121).
unicode_unihan_variant(0x65E2, kZVariant, 0x65E3).
unicode_unihan_variant(0x65E6, kZVariant, 0x86CB).
unicode_unihan_variant(0x65E7, kSemanticVariant, 0x820A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x65E7, kTraditionalVariant, 0x820A).
unicode_unihan_variant(0x65EA, kSemanticVariant, 0x5354). %<kMatthews
unicode_unihan_variant(0x65ED, kZVariant, 0x65EF).
unicode_unihan_variant(0x65EE, kZVariant, 0x65ED).
unicode_unihan_variant(0x65EF, kZVariant, 0x65ED).
unicode_unihan_variant(0x65F6, kSemanticVariant, 0x6642). %<kFenn 0x65F9<kFenn
unicode_unihan_variant(0x65F6, kTraditionalVariant, 0x6642).
unicode_unihan_variant(0x65F7, kTraditionalVariant, 0x66E0).
unicode_unihan_variant(0x65F8, kTraditionalVariant, 0x6698).
unicode_unihan_variant(0x65F9, kSemanticVariant, 0x65F6). %<kFenn 0x6642<kLau,kMatthews
unicode_unihan_variant(0x6602, kSemanticVariant, 0x663B). %<kMatthews
unicode_unihan_variant(0x6607, kZVariant, 0x5347).
unicode_unihan_variant(0x660E, kSemanticVariant, 0x6719). %<kMatthews
unicode_unihan_variant(0x660F, kSemanticVariant, 0x662C). %<kMatthews
unicode_unihan_variant(0x6612, kSemanticVariant, 0x3ADA). %<kHanYu
unicode_unihan_variant(0x6613, kZVariant, 0xF9E0).
unicode_unihan_variant(0x6619, kTraditionalVariant, 0x66C7).
unicode_unihan_variant(0x661A, kZVariant, 0x613C).
unicode_unihan_variant(0x661C, kZVariant, 0x967D).
unicode_unihan_variant(0x661D, kSemanticVariant, 0x507A). %<kMatthews 0x5592<kMatthews
unicode_unihan_variant(0x6620, kSemanticVariant, 0x668E). %<kMatthews
unicode_unihan_variant(0x6620, kZVariant, 0x668E).
unicode_unihan_variant(0x6625, kZVariant, 0x8405).
unicode_unihan_variant(0x662A, kSemanticVariant, 0x5FED). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x662C, kSemanticVariant, 0x660F). %<kMatthews
unicode_unihan_variant(0x662F, kZVariant, 0x6630).
unicode_unihan_variant(0x6630, kZVariant, 0x662F).
unicode_unihan_variant(0x6635, kSemanticVariant, 0x66B1). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6636, kZVariant, 0x66A2).
unicode_unihan_variant(0x6637, kSemanticVariant, 0x25055). %<kFenn
unicode_unihan_variant(0x663B, kSemanticVariant, 0x6602). %<kMatthews
unicode_unihan_variant(0x663C, kTraditionalVariant, 0x665D).
unicode_unihan_variant(0x663D, kTraditionalVariant, 0x66E8).
unicode_unihan_variant(0x663E, kTraditionalVariant, 0x986F).
unicode_unihan_variant(0x663F, kZVariant, 0x66E0).
unicode_unihan_variant(0x6641, kSemanticVariant, 0x671D). %<kMatthews
unicode_unihan_variant(0x6641, kSpecializedSemanticVariant, 0x671D). %<kMeyerWempe
unicode_unihan_variant(0x6642, kSemanticVariant, 0x65F6). %<kFenn 0x65F9<kLau,kMatthews
unicode_unihan_variant(0x6642, kSimplifiedVariant, 0x65F6).
unicode_unihan_variant(0x6643, kZVariant, 0x6644).
unicode_unihan_variant(0x6644, kZVariant, 0x6643).
unicode_unihan_variant(0x6645, kSemanticVariant, 0x70DC). %<kMatthews
unicode_unihan_variant(0x6649, kSemanticVariant, 0x664B). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6649, kSimplifiedVariant, 0x664B).
unicode_unihan_variant(0x664B, kSemanticVariant, 0x6649). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x664B, kTraditionalVariant, 0x6649).
unicode_unihan_variant(0x6652, kSemanticVariant, 0x66EC). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6652, kTraditionalVariant, 0x66EC).
unicode_unihan_variant(0x6653, kTraditionalVariant, 0x66C9).
unicode_unihan_variant(0x6654, kTraditionalVariant, 0x66C4).
unicode_unihan_variant(0x6655, kTraditionalVariant, 0x6688).
unicode_unihan_variant(0x6656, kTraditionalVariant, 0x6689).
unicode_unihan_variant(0x665A, kZVariant, 0x6669).
unicode_unihan_variant(0x665D, kSimplifiedVariant, 0x663C).
unicode_unihan_variant(0x6662, kZVariant, 0x6670).
unicode_unihan_variant(0x6669, kZVariant, 0x665A).
unicode_unihan_variant(0x6670, kSemanticVariant, 0x6673). %<kFenn
unicode_unihan_variant(0x6670, kSpecializedSemanticVariant, 0x6673). %<kMeyerWempe
unicode_unihan_variant(0x6670, kZVariant, 0x6662).
unicode_unihan_variant(0x6673, kSemanticVariant, 0x6670). %<kFenn
unicode_unihan_variant(0x6673, kSpecializedSemanticVariant, 0x6670). %<kMeyerWempe
unicode_unihan_variant(0x6674, kZVariant, 0xFA12).
unicode_unihan_variant(0x6681, kZVariant, 0x66C9).
unicode_unihan_variant(0x6682, kTraditionalVariant, 0x66AB).
unicode_unihan_variant(0x6688, kSimplifiedVariant, 0x6655).
unicode_unihan_variant(0x6689, kSimplifiedVariant, 0x6656).
unicode_unihan_variant(0x668B, kSemanticVariant, 0x6543). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x668B, kZVariant, 0x5FDE).
unicode_unihan_variant(0x668E, kSemanticVariant, 0x6620). %<kMatthews
unicode_unihan_variant(0x668E, kZVariant, 0x6620).
unicode_unihan_variant(0x6696, kSemanticVariant, 0x7156). %<kLau,kMatthews
unicode_unihan_variant(0x6696, kSpecializedSemanticVariant, 0x5A20). %<kFenn 0x7156<kFenn
unicode_unihan_variant(0x6696, kZVariant, 0x7156).
unicode_unihan_variant(0x6697, kZVariant, 0x667B).
unicode_unihan_variant(0x6698, kSimplifiedVariant, 0x65F8).
unicode_unihan_variant(0x66A0, kSemanticVariant, 0x769C). %<kMatthews 0x2325E<kMeyerWempe
unicode_unihan_variant(0x66A2, kSimplifiedVariant, 0x7545).
unicode_unihan_variant(0x66A6, kZVariant, 0x66C6).
unicode_unihan_variant(0x66A7, kTraditionalVariant, 0x66D6).
unicode_unihan_variant(0x66A8, kZVariant, 0x66C1).
unicode_unihan_variant(0x66AB, kSimplifiedVariant, 0x6682).
unicode_unihan_variant(0x66B1, kSemanticVariant, 0x6635). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x66B4, kSemanticVariant, 0x8663). %<kMatthews
unicode_unihan_variant(0x66B4, kZVariant, 0xFA06).
unicode_unihan_variant(0x66B8, kZVariant, 0x77AD).
unicode_unihan_variant(0x66C4, kSimplifiedVariant, 0x6654).
unicode_unihan_variant(0x66C4, kZVariant, 0x66C5).
unicode_unihan_variant(0x66C5, kZVariant, 0x66C4).
unicode_unihan_variant(0x66C6, kSemanticVariant, 0x53A4). %<kLau,kMatthews 0x6B74<kMeyerWempe 0x6B77<kMeyerWempe
unicode_unihan_variant(0x66C6, kSimplifiedVariant, 0x5386).
unicode_unihan_variant(0x66C7, kSimplifiedVariant, 0x6619).
unicode_unihan_variant(0x66C9, kSimplifiedVariant, 0x6653).
unicode_unihan_variant(0x66C9, kZVariant, 0x6681).
unicode_unihan_variant(0x66CF, kSimplifiedVariant, 0x5411).
unicode_unihan_variant(0x66D6, kSemanticVariant, 0x77B9). %<kMeyerWempe
unicode_unihan_variant(0x66D6, kSimplifiedVariant, 0x66A7).
unicode_unihan_variant(0x66DC, kSemanticVariant, 0x71FF). %<kCowles 0x8000<kCowles
unicode_unihan_variant(0x66E0, kSimplifiedVariant, 0x65F7).
unicode_unihan_variant(0x66E0, kZVariant, 0x663F).
unicode_unihan_variant(0x66E1, kSemanticVariant, 0x53E0). %<kMatthews 0x758A<kMatthews
unicode_unihan_variant(0x66E1, kZVariant, 0x758A).
unicode_unihan_variant(0x66E5, kSimplifiedVariant, 0x23190).
unicode_unihan_variant(0x66E8, kSimplifiedVariant, 0x663D).
unicode_unihan_variant(0x66EC, kSemanticVariant, 0x6652). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x66EC, kSimplifiedVariant, 0x6652).
unicode_unihan_variant(0x66F3, kSpecializedSemanticVariant, 0x62FD). %<kFenn
unicode_unihan_variant(0x66F3, kZVariant, 0x66F5).
unicode_unihan_variant(0x66F4, kSemanticVariant, 0x3A85). %<kMatthews
unicode_unihan_variant(0x66F4, kZVariant, 0xF901).
unicode_unihan_variant(0x66F5, kZVariant, 0x66F3).
unicode_unihan_variant(0x66F6, kZVariant, 0x3ADA).
unicode_unihan_variant(0x66F8, kSimplifiedVariant, 0x4E66).
unicode_unihan_variant(0x66F9, kZVariant, 0x66FA).
unicode_unihan_variant(0x66FC, kSemanticVariant, 0x3B05). %<kMatthews
unicode_unihan_variant(0x66FD, kZVariant, 0x66FE).
unicode_unihan_variant(0x66FE, kZVariant, 0x66FD).
unicode_unihan_variant(0x6701, kSemanticVariant, 0x3B31). %<kMatthews
unicode_unihan_variant(0x6702, kZVariant, 0x52D7).
unicode_unihan_variant(0x6703, kSemanticVariant, 0x4F1A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6703, kSimplifiedVariant, 0x4F1A).
unicode_unihan_variant(0x670C, kZVariant, 0x9812).
unicode_unihan_variant(0x6710, kSemanticVariant, 0x9E1C). %<kMatthews 0x9D1D<kMatthews
unicode_unihan_variant(0x6710, kZVariant, 0x80CA).
unicode_unihan_variant(0x6716, kZVariant, 0x6717).
unicode_unihan_variant(0x6717, kZVariant, 0xF929).
unicode_unihan_variant(0x6719, kSemanticVariant, 0x660E). %<kMatthews
unicode_unihan_variant(0x671B, kSemanticVariant, 0x6722). %<kMatthews
unicode_unihan_variant(0x671B, kZVariant, 0x6722).
unicode_unihan_variant(0x671D, kSemanticVariant, 0x6641). %<kMatthews
unicode_unihan_variant(0x671D, kSpecializedSemanticVariant, 0x6641). %<kMeyerWempe
unicode_unihan_variant(0x671E, kSemanticVariant, 0x7A18). %<kMatthews
unicode_unihan_variant(0x671E, kZVariant, 0x671F).
unicode_unihan_variant(0x671F, kSpecializedSemanticVariant, 0x901E). %<kFenn
unicode_unihan_variant(0x671F, kZVariant, 0x671E).
unicode_unihan_variant(0x6722, kSemanticVariant, 0x671B). %<kMatthews
unicode_unihan_variant(0x6722, kZVariant, 0x671B).
unicode_unihan_variant(0x6725, kSimplifiedVariant, 0x266E8).
unicode_unihan_variant(0x6726, kSpecializedSemanticVariant, 0x77C7).
unicode_unihan_variant(0x6726, kZVariant, 0x8499).
unicode_unihan_variant(0x6727, kSimplifiedVariant, 0x80E7).
unicode_unihan_variant(0x6727, kSpecializedSemanticVariant, 0x77D3).
unicode_unihan_variant(0x6729, kZVariant, 0x7B49).
unicode_unihan_variant(0x672C, kZVariant, 0x5932).
unicode_unihan_variant(0x672D, kZVariant, 0x7B9A).
unicode_unihan_variant(0x672F, kTraditionalVariant, 0x8853).
unicode_unihan_variant(0x6733, kSemanticVariant, 0x6777). %<kMatthews
unicode_unihan_variant(0x6734, kSemanticVariant, 0x6A38). %<kMatthews
unicode_unihan_variant(0x6734, kTraditionalVariant, 0x6A38).
unicode_unihan_variant(0x6735, kSemanticVariant, 0x6736). %<kLau,kMatthews
unicode_unihan_variant(0x6735, kSpecializedSemanticVariant, 0x6736). %<kMeyerWempe
unicode_unihan_variant(0x6735, kZVariant, 0x6736).
unicode_unihan_variant(0x6736, kSemanticVariant, 0x6735). %<kLau,kMatthews
unicode_unihan_variant(0x6736, kSpecializedSemanticVariant, 0x6735). %<kMeyerWempe
unicode_unihan_variant(0x6736, kZVariant, 0x6735).
unicode_unihan_variant(0x6738, kZVariant, 0x6250).
unicode_unihan_variant(0x673A, kSemanticVariant, 0x6A5F). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x673A, kTraditionalVariant, 0x6A5F).
unicode_unihan_variant(0x673F, kSemanticVariant, 0x83BF). %<kMatthews
unicode_unihan_variant(0x6740, kTraditionalVariant, 0x6BBA).
unicode_unihan_variant(0x6742, kTraditionalVariant, 0x96DC).
unicode_unihan_variant(0x6743, kTraditionalVariant, 0x6B0A).
unicode_unihan_variant(0x6746, kTraditionalVariant, 0x687F).
unicode_unihan_variant(0x6747, kSemanticVariant, 0x572C). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x674E, kZVariant, 0xF9E1).
unicode_unihan_variant(0x6751, kSemanticVariant, 0x90A8). %<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x6753, kSpecializedSemanticVariant, 0x6349). %<kMeyerWempe
unicode_unihan_variant(0x675D, kSemanticVariant, 0x67C2). %<kMeyerWempe
unicode_unihan_variant(0x6760, kSemanticVariant, 0x69D3). %<kFenn
unicode_unihan_variant(0x6760, kZVariant, 0x69D3).
unicode_unihan_variant(0x6761, kSemanticVariant, 0x689D). %<kMeyerWempe
unicode_unihan_variant(0x6761, kTraditionalVariant, 0x689D).
unicode_unihan_variant(0x6765, kSemanticVariant, 0x4F86). %<kMeyerWempe
unicode_unihan_variant(0x6765, kTraditionalVariant, 0x4F86).
unicode_unihan_variant(0x6767, kSemanticVariant, 0x8292). %<kLau
unicode_unihan_variant(0x6768, kTraditionalVariant, 0x694A).
unicode_unihan_variant(0x6769, kTraditionalVariant, 0x69AA).
unicode_unihan_variant(0x676F, kSemanticVariant, 0x76C3). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x676F, kZVariant, 0x76C3).
unicode_unihan_variant(0x6770, kSemanticVariant, 0x5091). %<kMatthews
unicode_unihan_variant(0x6770, kTraditionalVariant, 0x5091).
unicode_unihan_variant(0x6771, kSemanticVariant, 0x4E1C). %<kFenn
unicode_unihan_variant(0x6771, kSimplifiedVariant, 0x4E1C).
unicode_unihan_variant(0x6774, kSimplifiedVariant, 0x9528).
unicode_unihan_variant(0x6777, kSemanticVariant, 0x6733). %<kMatthews
unicode_unihan_variant(0x6778, kSemanticVariant, 0x6BB3). %<kMatthews
unicode_unihan_variant(0x677E, kTraditionalVariant, 0x9B06).
unicode_unihan_variant(0x677E, kZVariant, 0x67A9).
unicode_unihan_variant(0x677F, kSemanticVariant, 0x7248). %<kMeyerWempe
unicode_unihan_variant(0x677F, kTraditionalVariant, 0x95C6).
unicode_unihan_variant(0x6780, kZVariant, 0x677E).
unicode_unihan_variant(0x6781, kSimplifiedVariant, 0x6781).
unicode_unihan_variant(0x6781, kTraditionalVariant, 0x6781). %0x6975
unicode_unihan_variant(0x6781, kZVariant, 0x6975).
unicode_unihan_variant(0x6784, kTraditionalVariant, 0x69CB).
unicode_unihan_variant(0x678F, kSemanticVariant, 0x6960). %<kMeyerWempe
unicode_unihan_variant(0x678F, kZVariant, 0x6960).
unicode_unihan_variant(0x6797, kZVariant, 0xF9F4).
unicode_unihan_variant(0x679C, kSemanticVariant, 0x83D3). %<kFenn
unicode_unihan_variant(0x679E, kTraditionalVariant, 0x6A05).
unicode_unihan_variant(0x67A2, kTraditionalVariant, 0x6A1E).
unicode_unihan_variant(0x67A3, kTraditionalVariant, 0x68D7).
unicode_unihan_variant(0x67A5, kTraditionalVariant, 0x6AEA).
unicode_unihan_variant(0x67A7, kTraditionalVariant, 0x6898).
unicode_unihan_variant(0x67A8, kTraditionalVariant, 0x68D6).
unicode_unihan_variant(0x67A9, kZVariant, 0x677E).
unicode_unihan_variant(0x67AA, kTraditionalVariant, 0x69CD).
unicode_unihan_variant(0x67AB, kTraditionalVariant, 0x6953).
unicode_unihan_variant(0x67AC, kSemanticVariant, 0x6960). %<kMatthews
unicode_unihan_variant(0x67AD, kTraditionalVariant, 0x689F).
unicode_unihan_variant(0x67B1, kSemanticVariant, 0x5113). %<kLau 0x6AAF<kHKGlyph,kLau,kMeyerWempe
unicode_unihan_variant(0x67B4, kZVariant, 0x62D0).
unicode_unihan_variant(0x67B6, kSemanticVariant, 0x69A2). %<kMatthews
unicode_unihan_variant(0x67BE, kSemanticVariant, 0x67F9). %<kMatthews
unicode_unihan_variant(0x67BE, kZVariant, 0x67FF).
unicode_unihan_variant(0x67C1, kSemanticVariant, 0x8235). %<kMeyerWempe
unicode_unihan_variant(0x67C2, kSemanticVariant, 0x675D). %<kMeyerWempe
unicode_unihan_variant(0x67C4, kSemanticVariant, 0x68C5). %<kMatthews
unicode_unihan_variant(0x67C6, kZVariant, 0x62C9).
unicode_unihan_variant(0x67CF, kSemanticVariant, 0x6822). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x67CF, kSpecializedSemanticVariant, 0x5B5B). %<kFenn
unicode_unihan_variant(0x67CF, kZVariant, 0x6822).
unicode_unihan_variant(0x67D0, kSemanticVariant, 0x53B6). %<kLau
unicode_unihan_variant(0x67D0, kSpecializedSemanticVariant, 0x53B6). %<kFenn
unicode_unihan_variant(0x67D2, kSemanticVariant, 0x4E03). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x67D2, kSpecializedSemanticVariant, 0x4E03). %<kFenn
unicode_unihan_variant(0x67D2, kZVariant, 0x6F06).
unicode_unihan_variant(0x67DA, kSemanticVariant, 0x6AFE). %<kMatthews
unicode_unihan_variant(0x67DC, kSemanticVariant, 0x6AC3). %<kHKGlyph
unicode_unihan_variant(0x67DC, kTraditionalVariant, 0x6AC3).
unicode_unihan_variant(0x67E0, kTraditionalVariant, 0x6AB8).
unicode_unihan_variant(0x67E5, kSemanticVariant, 0x67FB). %<kMatthews
unicode_unihan_variant(0x67E5, kZVariant, 0x67FB).
unicode_unihan_variant(0x67E9, kZVariant, 0x67FE).
unicode_unihan_variant(0x67F0, kSemanticVariant, 0x5948). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x67F0, kZVariant, 0x5948).
unicode_unihan_variant(0x67F3, kSemanticVariant, 0x6A89). %<kMatthews
unicode_unihan_variant(0x67F4, kZVariant, 0x8308).
unicode_unihan_variant(0x67F5, kSimplifiedVariant, 0x6805).
unicode_unihan_variant(0x67F9, kSemanticVariant, 0x67BE). %<kMatthews
unicode_unihan_variant(0x67FB, kSemanticVariant, 0x67E5). %<kMatthews
unicode_unihan_variant(0x67FB, kZVariant, 0x67E5).
unicode_unihan_variant(0x67FD, kTraditionalVariant, 0x6A89).
unicode_unihan_variant(0x67FE, kZVariant, 0x67E9).
unicode_unihan_variant(0x67FF, kZVariant, 0x67BE).
unicode_unihan_variant(0x6800, kTraditionalVariant, 0x6894).
unicode_unihan_variant(0x6804, kZVariant, 0x69AE).
unicode_unihan_variant(0x6805, kTraditionalVariant, 0x67F5).
unicode_unihan_variant(0x6807, kSemanticVariant, 0x6A81). %<kMatthews
unicode_unihan_variant(0x6807, kTraditionalVariant, 0x6A19).
unicode_unihan_variant(0x6808, kTraditionalVariant, 0x68E7).
unicode_unihan_variant(0x6809, kTraditionalVariant, 0x6ADB).
unicode_unihan_variant(0x680A, kTraditionalVariant, 0x6AF3).
unicode_unihan_variant(0x680B, kTraditionalVariant, 0x68DF).
unicode_unihan_variant(0x680C, kTraditionalVariant, 0x6AE8).
unicode_unihan_variant(0x680E, kTraditionalVariant, 0x6ADF).
unicode_unihan_variant(0x680F, kTraditionalVariant, 0x6B04).
unicode_unihan_variant(0x6811, kTraditionalVariant, 0x6A39).
unicode_unihan_variant(0x6812, kSemanticVariant, 0x7C28). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6816, kSemanticVariant, 0x68F2). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6816, kTraditionalVariant, 0x68F2).
unicode_unihan_variant(0x6817, kTraditionalVariant, 0x6144).
unicode_unihan_variant(0x6817, kZVariant, 0xF9D9).
unicode_unihan_variant(0x681B, kSemanticVariant, 0x8354). %<kMatthews
unicode_unihan_variant(0x681D, kZVariant, 0x6A9C).
unicode_unihan_variant(0x6822, kSemanticVariant, 0x67CF). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6822, kSpecializedSemanticVariant, 0x5B5B). %<kFenn
unicode_unihan_variant(0x6830, kSemanticVariant, 0x7B4F). %<kMatthews
unicode_unihan_variant(0x6837, kTraditionalVariant, 0x6A23).
unicode_unihan_variant(0x6838, kZVariant, 0x8988).
unicode_unihan_variant(0x683E, kTraditionalVariant, 0x6B12).
unicode_unihan_variant(0x6841, kZVariant, 0x822A).
unicode_unihan_variant(0x684C, kSemanticVariant, 0x68F9). %<kMatthews
unicode_unihan_variant(0x684C, kSpecializedSemanticVariant, 0x68F9). %<kFenn
unicode_unihan_variant(0x684C, kZVariant, 0x68F9).
unicode_unihan_variant(0x6851, kZVariant, 0x6852).
unicode_unihan_variant(0x6852, kZVariant, 0x6851).
unicode_unihan_variant(0x685C, kZVariant, 0x6AFB).
unicode_unihan_variant(0x685F, kZVariant, 0x68E7).
unicode_unihan_variant(0x6860, kTraditionalVariant, 0x690F).
unicode_unihan_variant(0x6861, kTraditionalVariant, 0x6A48).
unicode_unihan_variant(0x6862, kTraditionalVariant, 0x6968).
unicode_unihan_variant(0x6863, kTraditionalVariant, 0x6A94).
unicode_unihan_variant(0x6864, kTraditionalVariant, 0x69BF).
unicode_unihan_variant(0x6865, kTraditionalVariant, 0x6A4B).
unicode_unihan_variant(0x6866, kTraditionalVariant, 0x6A3A).
unicode_unihan_variant(0x6867, kTraditionalVariant, 0x6A9C).
unicode_unihan_variant(0x6868, kTraditionalVariant, 0x69F3).
unicode_unihan_variant(0x6869, kTraditionalVariant, 0x6A01).
unicode_unihan_variant(0x686A, kTraditionalVariant, 0x6A33).
unicode_unihan_variant(0x686E, kZVariant, 0x676F).
unicode_unihan_variant(0x687C, kSemanticVariant, 0x3BC3). %<kMatthews 0x6F06<kMatthews
unicode_unihan_variant(0x687F, kSimplifiedVariant, 0x6746).
unicode_unihan_variant(0x6881, kSemanticVariant, 0x6A11). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6881, kZVariant, 0xF97A).
unicode_unihan_variant(0x6885, kZVariant, 0x6973).
unicode_unihan_variant(0x6894, kSimplifiedVariant, 0x6800).
unicode_unihan_variant(0x6898, kSimplifiedVariant, 0x67A7).
unicode_unihan_variant(0x689D, kSemanticVariant, 0x6761). %<kMeyerWempe 0x7CF6<kLau
unicode_unihan_variant(0x689D, kSimplifiedVariant, 0x6761).
unicode_unihan_variant(0x689F, kSemanticVariant, 0x9D1E). %<kFenn
unicode_unihan_variant(0x689F, kSimplifiedVariant, 0x67AD).
unicode_unihan_variant(0x68A6, kSemanticVariant, 0x5922). %<kLau,kMeyerWempe
unicode_unihan_variant(0x68A6, kTraditionalVariant, 0x5922).
unicode_unihan_variant(0x68A8, kSemanticVariant, 0x68C3). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x68A8, kZVariant, 0xF9E2).
unicode_unihan_variant(0x68B2, kSemanticVariant, 0x68F3). %<kMatthews
unicode_unihan_variant(0x68B2, kSimplifiedVariant, 0x68C1).
unicode_unihan_variant(0x68B9, kSemanticVariant, 0x6AB3). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x68B9, kZVariant, 0x6AB3).
unicode_unihan_variant(0x68BC, kTraditionalVariant, 0x6AAE).
unicode_unihan_variant(0x68BE, kTraditionalVariant, 0x68F6).
unicode_unihan_variant(0x68BF, kTraditionalVariant, 0x69E4).
unicode_unihan_variant(0x68C0, kTraditionalVariant, 0x6AA2).
unicode_unihan_variant(0x68C1, kTraditionalVariant, 0x68B2).
unicode_unihan_variant(0x68C2, kTraditionalVariant, 0x6B1E).
unicode_unihan_variant(0x68C3, kSemanticVariant, 0x68A8). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x68C4, kSimplifiedVariant, 0x5F03).
unicode_unihan_variant(0x68C5, kSemanticVariant, 0x67C4). %<kMatthews
unicode_unihan_variant(0x68CA, kSemanticVariant, 0x7881). %<kLau,kMatthews 0x68CB<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x68CA, kZVariant, 0x68CB).
unicode_unihan_variant(0x68CB, kSemanticVariant, 0x68CA). %<kLau,kMatthews,kMeyerWempe 0x7881<kLau,kMatthews
unicode_unihan_variant(0x68D0, kSemanticVariant, 0x69A7). %<kMatthews
unicode_unihan_variant(0x68D1, kSemanticVariant, 0x7BFA). %<kMeyerWempe
unicode_unihan_variant(0x68D5, kSemanticVariant, 0x3BF6). %<kFenn 0x6936<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x68D6, kSimplifiedVariant, 0x67A8).
unicode_unihan_variant(0x68D7, kSimplifiedVariant, 0x67A3).
unicode_unihan_variant(0x68DF, kSimplifiedVariant, 0x680B).
unicode_unihan_variant(0x68E1, kSimplifiedVariant, 0x3B4E).
unicode_unihan_variant(0x68E5, kSemanticVariant, 0x6A0A). %<kMatthews
unicode_unihan_variant(0x68E7, kSimplifiedVariant, 0x6808).
unicode_unihan_variant(0x68E7, kZVariant, 0x685F).
unicode_unihan_variant(0x68F0, kSemanticVariant, 0x69CC). %<kLau
unicode_unihan_variant(0x68F1, kSemanticVariant, 0x7A1C). %<kMeyerWempe 0x695E<kFenn
unicode_unihan_variant(0x68F2, kSemanticVariant, 0x6816). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x68F2, kSimplifiedVariant, 0x6816).
unicode_unihan_variant(0x68F3, kSemanticVariant, 0x68B2). %<kMatthews
unicode_unihan_variant(0x68F6, kSimplifiedVariant, 0x68BE).
unicode_unihan_variant(0x68F9, kSemanticVariant, 0x684C). %<kMatthews 0x6AC2<kLau
unicode_unihan_variant(0x68F9, kSpecializedSemanticVariant, 0x5353). %<kMeyerWempe
unicode_unihan_variant(0x68F9, kZVariant, 0x684C).
unicode_unihan_variant(0x6900, kSemanticVariant, 0x76CC). %<kMatthews,kMeyerWempe 0x7897<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6901, kSemanticVariant, 0x69E8). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6901, kTraditionalVariant, 0x69E8).
unicode_unihan_variant(0x690E, kSemanticVariant, 0x69CC). %<kMatthews
unicode_unihan_variant(0x690F, kSimplifiedVariant, 0x6860).
unicode_unihan_variant(0x6912, kZVariant, 0x832D).
unicode_unihan_variant(0x6918, kZVariant, 0x695A).
unicode_unihan_variant(0x691C, kZVariant, 0x6AA2).
unicode_unihan_variant(0x691D, kTraditionalVariant, 0x69FC).
unicode_unihan_variant(0x691F, kTraditionalVariant, 0x6ADD).
unicode_unihan_variant(0x6920, kTraditionalVariant, 0x69E7).
unicode_unihan_variant(0x6922, kTraditionalVariant, 0x69F6).
unicode_unihan_variant(0x6924, kTraditionalVariant, 0x6B0F).
unicode_unihan_variant(0x692B, kTraditionalVariant, 0x6A3F).
unicode_unihan_variant(0x692B, kZVariant, 0x6A3F).
unicode_unihan_variant(0x692D, kTraditionalVariant, 0x6A62).
unicode_unihan_variant(0x6930, kSemanticVariant, 0x3B68). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6932, kSimplifiedVariant, 0x3B4F).
unicode_unihan_variant(0x6936, kSemanticVariant, 0x68D5). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6936, kZVariant, 0x68D5).
unicode_unihan_variant(0x6939, kSemanticVariant, 0x845A). %<kFenn
unicode_unihan_variant(0x6942, kSemanticVariant, 0x63F8). %<kLau 0x69CE<kMatthews
unicode_unihan_variant(0x6942, kZVariant, 0x67E5).
unicode_unihan_variant(0x694A, kSimplifiedVariant, 0x6768).
unicode_unihan_variant(0x6953, kSimplifiedVariant, 0x67AB).
unicode_unihan_variant(0x6955, kZVariant, 0x6A62).
unicode_unihan_variant(0x6959, kZVariant, 0x8302).
unicode_unihan_variant(0x695A, kZVariant, 0x6918).
unicode_unihan_variant(0x695C, kZVariant, 0x80E1).
unicode_unihan_variant(0x695E, kSemanticVariant, 0x68F1). %<kFenn 0x7A1C<kFenn
unicode_unihan_variant(0x6960, kSemanticVariant, 0x678F). %<kMeyerWempe 0x67AC<kMatthews
unicode_unihan_variant(0x6960, kZVariant, 0x678F).
unicode_unihan_variant(0x6961, kZVariant, 0x6986).
unicode_unihan_variant(0x6965, kSemanticVariant, 0x6966). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6966, kSemanticVariant, 0x6965). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6968, kSimplifiedVariant, 0x6862).
unicode_unihan_variant(0x696B, kSemanticVariant, 0x8265). %<kMatthews 0x6A9D<kMatthews
unicode_unihan_variant(0x696D, kSimplifiedVariant, 0x4E1A).
unicode_unihan_variant(0x6973, kZVariant, 0x6885).
unicode_unihan_variant(0x6975, kSimplifiedVariant, 0x6781).
unicode_unihan_variant(0x697C, kTraditionalVariant, 0x6A13).
unicode_unihan_variant(0x697D, kZVariant, 0x6A02).
unicode_unihan_variant(0x6982, kSemanticVariant, 0x69E9). %<kFenn
unicode_unihan_variant(0x6982, kZVariant, 0x69EA).
unicode_unihan_variant(0x6984, kTraditionalVariant, 0x6B16).
unicode_unihan_variant(0x6985, kTraditionalVariant, 0x69B2).
unicode_unihan_variant(0x6986, kZVariant, 0x6961).
unicode_unihan_variant(0x6987, kTraditionalVariant, 0x6AEC).
unicode_unihan_variant(0x6988, kTraditionalVariant, 0x6ADA).
unicode_unihan_variant(0x6989, kTraditionalVariant, 0x6AF8).
unicode_unihan_variant(0x698E, kSemanticVariant, 0x6A9F). %<kMatthews
unicode_unihan_variant(0x6998, kZVariant, 0x77E9).
unicode_unihan_variant(0x699B, kSemanticVariant, 0x4EB2). %<kMatthews
unicode_unihan_variant(0x69A2, kSemanticVariant, 0x67B6). %<kMatthews
unicode_unihan_variant(0x69A6, kZVariant, 0x4E7E).
unicode_unihan_variant(0x69A7, kSemanticVariant, 0x68D0). %<kMatthews
unicode_unihan_variant(0x69A8, kSemanticVariant, 0x91A1). %<kMatthews
unicode_unihan_variant(0x69A8, kSpecializedSemanticVariant, 0x9162). %<kMeyerWempe
unicode_unihan_variant(0x69AA, kSimplifiedVariant, 0x6769).
unicode_unihan_variant(0x69AE, kSemanticVariant, 0x8363). %<kMeyerWempe
unicode_unihan_variant(0x69AE, kSimplifiedVariant, 0x8363).
unicode_unihan_variant(0x69B2, kSimplifiedVariant, 0x6985).
unicode_unihan_variant(0x69B4, kSemanticVariant, 0x6A4A). %<kMeyerWempe
unicode_unihan_variant(0x69BF, kSimplifiedVariant, 0x6864).
unicode_unihan_variant(0x69C0, kSemanticVariant, 0x7A3F). %<kLau
unicode_unihan_variant(0x69C5, kZVariant, 0x6838).
unicode_unihan_variant(0x69C7, kZVariant, 0x69D9).
unicode_unihan_variant(0x69CA, kSemanticVariant, 0x9399). %<kMeyerWempe
unicode_unihan_variant(0x69CB, kSimplifiedVariant, 0x6784).
unicode_unihan_variant(0x69CC, kSemanticVariant, 0x68F0). %<kLau 0x690E<kMatthews
unicode_unihan_variant(0x69CD, kSimplifiedVariant, 0x67AA).
unicode_unihan_variant(0x69CE, kSemanticVariant, 0x6942). %<kMatthews
unicode_unihan_variant(0x69D3, kSemanticVariant, 0x6760). %<kFenn 0x7BE2<kMeyerWempe
unicode_unihan_variant(0x69D3, kZVariant, 0x6760).
unicode_unihan_variant(0x69D6, kSemanticVariant, 0x6A50). %<kMeyerWempe
unicode_unihan_variant(0x69D8, kZVariant, 0x6A23).
unicode_unihan_variant(0x69D9, kZVariant, 0x69C7).
unicode_unihan_variant(0x69DA, kTraditionalVariant, 0x6A9F).
unicode_unihan_variant(0x69DB, kTraditionalVariant, 0x6ABB).
unicode_unihan_variant(0x69DC, kZVariant, 0x6A87).
unicode_unihan_variant(0x69DF, kTraditionalVariant, 0x6AB3).
unicode_unihan_variant(0x69E0, kTraditionalVariant, 0x6AE7).
unicode_unihan_variant(0x69E4, kSimplifiedVariant, 0x68BF).
unicode_unihan_variant(0x69E7, kSimplifiedVariant, 0x6920).
unicode_unihan_variant(0x69E8, kSemanticVariant, 0x6901). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x69E8, kSimplifiedVariant, 0x6901).
unicode_unihan_variant(0x69E9, kSemanticVariant, 0x3BA3). %<kMatthews 0x6982<kFenn
unicode_unihan_variant(0x69EA, kSemanticVariant, 0x3BA3). %<kMatthews
unicode_unihan_variant(0x69F3, kSimplifiedVariant, 0x6868).
unicode_unihan_variant(0x69F6, kSimplifiedVariant, 0x6922).
unicode_unihan_variant(0x69F9, kSemanticVariant, 0x234CC). %<kMeyerWempe
unicode_unihan_variant(0x69FB, kZVariant, 0x898F).
unicode_unihan_variant(0x69FC, kSimplifiedVariant, 0x691D).
unicode_unihan_variant(0x6A01, kSimplifiedVariant, 0x6869).
unicode_unihan_variant(0x6A02, kSimplifiedVariant, 0x4E50).
unicode_unihan_variant(0x6A02, kZVariant, 0xF914).
unicode_unihan_variant(0x6A05, kSimplifiedVariant, 0x679E).
unicode_unihan_variant(0x6A0A, kSemanticVariant, 0x68E5). %<kMatthews
unicode_unihan_variant(0x6A10, kSemanticVariant, 0x3BED). %<kMatthews 0x826A<kMatthews 0x6AD3<kMatthews
unicode_unihan_variant(0x6A10, kZVariant, 0x6AD3).
unicode_unihan_variant(0x6A11, kSemanticVariant, 0x6881). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6A13, kSimplifiedVariant, 0x697C).
unicode_unihan_variant(0x6A13, kZVariant, 0xF94C).
unicode_unihan_variant(0x6A19, kSimplifiedVariant, 0x6807).
unicode_unihan_variant(0x6A1A, kSemanticVariant, 0x8F46). %<kMatthews
unicode_unihan_variant(0x6A1E, kSimplifiedVariant, 0x67A2).
unicode_unihan_variant(0x6A22, kSimplifiedVariant, 0x3B64).
unicode_unihan_variant(0x6A23, kSimplifiedVariant, 0x6837).
unicode_unihan_variant(0x6A23, kZVariant, 0x69D8).
unicode_unihan_variant(0x6A29, kZVariant, 0x6B0A).
unicode_unihan_variant(0x6A2A, kTraditionalVariant, 0x6A6B).
unicode_unihan_variant(0x6A2B, kSimplifiedVariant, 0x3B74).
unicode_unihan_variant(0x6A2F, kTraditionalVariant, 0x6AA3).
unicode_unihan_variant(0x6A31, kTraditionalVariant, 0x6AFB).
unicode_unihan_variant(0x6A33, kSimplifiedVariant, 0x686A).
unicode_unihan_variant(0x6A37, kSemanticVariant, 0x53E2). %<kFenn
unicode_unihan_variant(0x6A38, kSemanticVariant, 0x6734). %<kMatthews
unicode_unihan_variant(0x6A38, kSimplifiedVariant, 0x6734).
unicode_unihan_variant(0x6A39, kSimplifiedVariant, 0x6811).
unicode_unihan_variant(0x6A3A, kSimplifiedVariant, 0x6866).
unicode_unihan_variant(0x6A3B, kSemanticVariant, 0x5331). %<kMeyerWempe 0x6AC3<kMeyerWempe
unicode_unihan_variant(0x6A3D, kSemanticVariant, 0x58AB). %<kMatthews 0x7F47<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6A3D, kZVariant, 0x7F47).
unicode_unihan_variant(0x6A3F, kSimplifiedVariant, 0x692B).
unicode_unihan_variant(0x6A3F, kZVariant, 0x692B).
unicode_unihan_variant(0x6A46, kSemanticVariant, 0x7121). %<kMatthews
unicode_unihan_variant(0x6A47, kSemanticVariant, 0x97BD). %<kFenn
unicode_unihan_variant(0x6A47, kZVariant, 0x9792).
unicode_unihan_variant(0x6A48, kSimplifiedVariant, 0x6861).
unicode_unihan_variant(0x6A4A, kSemanticVariant, 0x69B4). %<kMeyerWempe
unicode_unihan_variant(0x6A4B, kSimplifiedVariant, 0x6865).
unicode_unihan_variant(0x6A50, kSemanticVariant, 0x69D6). %<kMeyerWempe
unicode_unihan_variant(0x6A53, kSemanticVariant, 0x8563). %<kMeyerWempe
unicode_unihan_variant(0x6A5B, kSemanticVariant, 0x6A5C). %<kFenn,kMeyerWempe
unicode_unihan_variant(0x6A5C, kSemanticVariant, 0x6A5B). %<kFenn,kMeyerWempe
unicode_unihan_variant(0x6A5F, kSemanticVariant, 0x673A). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6A5F, kSimplifiedVariant, 0x673A).
unicode_unihan_variant(0x6A62, kSimplifiedVariant, 0x692D).
unicode_unihan_variant(0x6A62, kZVariant, 0x6955).
unicode_unihan_variant(0x6A65, kTraditionalVariant, 0x6AEB).
unicode_unihan_variant(0x6A66, kZVariant, 0x5E62).
unicode_unihan_variant(0x6A6B, kSimplifiedVariant, 0x6A2A).
unicode_unihan_variant(0x6A71, kTraditionalVariant, 0x6AE5).
unicode_unihan_variant(0x6A79, kTraditionalVariant, 0x6AD3).
unicode_unihan_variant(0x6A7C, kTraditionalVariant, 0x6ADE).
unicode_unihan_variant(0x6A81, kSemanticVariant, 0x6807). %<kMatthews
unicode_unihan_variant(0x6A81, kSimplifiedVariant, 0x6AA9).
unicode_unihan_variant(0x6A87, kZVariant, 0x69DC).
unicode_unihan_variant(0x6A89, kSemanticVariant, 0x67F3). %<kMatthews
unicode_unihan_variant(0x6A89, kSimplifiedVariant, 0x67FD).
unicode_unihan_variant(0x6A90, kSemanticVariant, 0x7C37). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6A94, kSemanticVariant, 0x6B13). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6A94, kSimplifiedVariant, 0x6863).
unicode_unihan_variant(0x6A98, kZVariant, 0x8617).
unicode_unihan_variant(0x6A9B, kSemanticVariant, 0x7C3B). %<kMeyerWempe
unicode_unihan_variant(0x6A9C, kSimplifiedVariant, 0x6867).
unicode_unihan_variant(0x6A9C, kZVariant, 0x681D).
unicode_unihan_variant(0x6A9D, kSemanticVariant, 0x696B). %<kMatthews 0x8265<kMatthews
unicode_unihan_variant(0x6A9F, kSemanticVariant, 0x698E). %<kMatthews
unicode_unihan_variant(0x6A9F, kSimplifiedVariant, 0x69DA).
unicode_unihan_variant(0x6A9F, kSpecializedSemanticVariant, 0x659D). %<kMeyerWempe
unicode_unihan_variant(0x6AA0, kSemanticVariant, 0x22427). %<kMeyerWempe
unicode_unihan_variant(0x6AA2, kSimplifiedVariant, 0x68C0).
unicode_unihan_variant(0x6AA2, kZVariant, 0x691C).
unicode_unihan_variant(0x6AA3, kSemanticVariant, 0x8262). %<kMatthews
unicode_unihan_variant(0x6AA3, kSimplifiedVariant, 0x6A2F).
unicode_unihan_variant(0x6AA9, kTraditionalVariant, 0x6A81).
unicode_unihan_variant(0x6AAD, kSimplifiedVariant, 0x23634).
unicode_unihan_variant(0x6AAE, kSimplifiedVariant, 0x68BC).
unicode_unihan_variant(0x6AAF, kSemanticVariant, 0x5113). %<kLau 0x67B1<kHKGlyph,kLau,kMeyerWempe
unicode_unihan_variant(0x6AAF, kSimplifiedVariant, 0x53F0).
unicode_unihan_variant(0x6AB3, kSemanticVariant, 0x3BFD). %<kMatthews,kMeyerWempe 0x68B9<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6AB3, kSimplifiedVariant, 0x69DF).
unicode_unihan_variant(0x6AB3, kZVariant, 0x68B9).
unicode_unihan_variant(0x6AB4, kSemanticVariant, 0x7A6B). %<kMeyerWempe
unicode_unihan_variant(0x6AB8, kSimplifiedVariant, 0x67E0).
unicode_unihan_variant(0x6ABB, kSimplifiedVariant, 0x69DB).
unicode_unihan_variant(0x6ABC, kZVariant, 0x6AFD).
unicode_unihan_variant(0x6ABE, kSemanticVariant, 0x451B). %<kFenn
unicode_unihan_variant(0x6AC2, kSemanticVariant, 0x68F9). %<kLau
unicode_unihan_variant(0x6AC2, kSpecializedSemanticVariant, 0x6389). %<kMeyerWempe
unicode_unihan_variant(0x6AC2, kZVariant, 0x68F9).
unicode_unihan_variant(0x6AC3, kSemanticVariant, 0x5331). %<kLau,kMatthews,kMeyerWempe 0x67DC<kHKGlyph 0x6A3B<kMeyerWempe 0x994B<kLau 0x9400<kMatthews
unicode_unihan_variant(0x6AC3, kSimplifiedVariant, 0x67DC).
unicode_unihan_variant(0x6AC8, kSemanticVariant, 0x51F3). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x6AC9, kZVariant, 0x6AE5).
unicode_unihan_variant(0x6ACC, kSemanticVariant, 0x8030). %<kMatthews
unicode_unihan_variant(0x6AD3, kSemanticVariant, 0x3BED). %<kMatthews 0x6A10<kMatthews 0x826A<kLau,kMatthews
unicode_unihan_variant(0x6AD3, kSimplifiedVariant, 0x6A79).
unicode_unihan_variant(0x6AD3, kZVariant, 0xF931).
unicode_unihan_variant(0x6ADA, kSimplifiedVariant, 0x6988).
unicode_unihan_variant(0x6ADB, kSimplifiedVariant, 0x6809).
unicode_unihan_variant(0x6ADD, kSemanticVariant, 0x5335). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6ADD, kSimplifiedVariant, 0x691F).
unicode_unihan_variant(0x6ADE, kSimplifiedVariant, 0x6A7C).
unicode_unihan_variant(0x6ADF, kSimplifiedVariant, 0x680E).
unicode_unihan_variant(0x6AE5, kSimplifiedVariant, 0x6A71).
unicode_unihan_variant(0x6AE5, kZVariant, 0x6AC9).
unicode_unihan_variant(0x6AE7, kSimplifiedVariant, 0x69E0).
unicode_unihan_variant(0x6AE8, kSimplifiedVariant, 0x680C).
unicode_unihan_variant(0x6AEA, kSimplifiedVariant, 0x67A5).
unicode_unihan_variant(0x6AEB, kSimplifiedVariant, 0x6A65).
unicode_unihan_variant(0x6AEC, kSimplifiedVariant, 0x6987).
unicode_unihan_variant(0x6AF1, kSemanticVariant, 0x8616). %<kMatthews
unicode_unihan_variant(0x6AF1, kSimplifiedVariant, 0x8616).
unicode_unihan_variant(0x6AF3, kSemanticVariant, 0x3C0D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6AF3, kSimplifiedVariant, 0x680A).
unicode_unihan_variant(0x6AF8, kSimplifiedVariant, 0x6989).
unicode_unihan_variant(0x6AFA, kSemanticVariant, 0x6B1E). %<kMeyerWempe
unicode_unihan_variant(0x6AFB, kSimplifiedVariant, 0x6A31).
unicode_unihan_variant(0x6AFD, kZVariant, 0x6ABC).
unicode_unihan_variant(0x6AFE, kSemanticVariant, 0x67DA). %<kMatthews
unicode_unihan_variant(0x6B04, kSimplifiedVariant, 0x680F).
unicode_unihan_variant(0x6B04, kZVariant, 0xF91D).
unicode_unihan_variant(0x6B0A, kSimplifiedVariant, 0x6743).
unicode_unihan_variant(0x6B0A, kZVariant, 0x6A29).
unicode_unihan_variant(0x6B0D, kSimplifiedVariant, 0x23424).
unicode_unihan_variant(0x6B0F, kSimplifiedVariant, 0x6924).
unicode_unihan_variant(0x6B12, kSimplifiedVariant, 0x683E).
unicode_unihan_variant(0x6B12, kZVariant, 0x7064).
unicode_unihan_variant(0x6B13, kSemanticVariant, 0x6A94). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6B13, kSimplifiedVariant, 0x235CB).
unicode_unihan_variant(0x6B16, kSimplifiedVariant, 0x6984).
unicode_unihan_variant(0x6B1D, kSemanticVariant, 0x9B31). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6B1D, kZVariant, 0x9B31).
unicode_unihan_variant(0x6B1E, kSemanticVariant, 0x6AFA). %<kMeyerWempe
unicode_unihan_variant(0x6B1E, kSimplifiedVariant, 0x68C2).
unicode_unihan_variant(0x6B20, kZVariant, 0x7F3A).
unicode_unihan_variant(0x6B22, kSemanticVariant, 0x61FD). %<kFenn 0x6B61<kFenn
unicode_unihan_variant(0x6B22, kTraditionalVariant, 0x6B61).
unicode_unihan_variant(0x6B23, kSemanticVariant, 0x5FFB). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6B23, kZVariant, 0x5FFB).
unicode_unihan_variant(0x6B24, kTraditionalVariant, 0x6B5F).
unicode_unihan_variant(0x6B27, kTraditionalVariant, 0x6B50).
unicode_unihan_variant(0x6B2C, kSemanticVariant, 0x54B3). %<kMatthews
unicode_unihan_variant(0x6B2C, kSpecializedSemanticVariant, 0x54B3). %<kFenn
unicode_unihan_variant(0x6B32, kSemanticVariant, 0x617E). %<kMeyerWempe
unicode_unihan_variant(0x6B32, kZVariant, 0x617E).
unicode_unihan_variant(0x6B35, kSemanticVariant, 0x6B3E). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6B36, kSemanticVariant, 0x20CBF). %<kFenn
unicode_unihan_variant(0x6B38, kSemanticVariant, 0x5509). %<kFenn
unicode_unihan_variant(0x6B38, kSpecializedSemanticVariant, 0x5509). %<kMeyerWempe
unicode_unihan_variant(0x6B38, kZVariant, 0x5509).
unicode_unihan_variant(0x6B39, kSemanticVariant, 0x7317). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6B3C, kSemanticVariant, 0x555C). %<kMatthews 0x6B60<kMatthews
unicode_unihan_variant(0x6B3D, kSimplifiedVariant, 0x94A6).
unicode_unihan_variant(0x6B3E, kSemanticVariant, 0x6B35). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6B49, kZVariant, 0x6B20).
unicode_unihan_variant(0x6B4C, kSemanticVariant, 0x8B0C). %<kLau,kMatthews
unicode_unihan_variant(0x6B4E, kSemanticVariant, 0x5606). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x6B50, kSimplifiedVariant, 0x6B27).
unicode_unihan_variant(0x6B53, kZVariant, 0x6B61).
unicode_unihan_variant(0x6B55, kSemanticVariant, 0x5460). %<kMatthews 0x5674<kMatthews
unicode_unihan_variant(0x6B57, kSemanticVariant, 0x562F). %<kMatthews
unicode_unihan_variant(0x6B5B, kZVariant, 0x6582).
unicode_unihan_variant(0x6B5F, kSimplifiedVariant, 0x6B24).
unicode_unihan_variant(0x6B60, kSemanticVariant, 0x555C). %<kMatthews 0x6B3C<kMatthews
unicode_unihan_variant(0x6B61, kSemanticVariant, 0x61FD). %<kFenn 0x6B22<kFenn
unicode_unihan_variant(0x6B61, kSimplifiedVariant, 0x6B22).
unicode_unihan_variant(0x6B61, kZVariant, 0x9A69).
unicode_unihan_variant(0x6B62, kZVariant, 0x53EA).
unicode_unihan_variant(0x6B67, kZVariant, 0x5C90).
unicode_unihan_variant(0x6B69, kZVariant, 0x6B65).
unicode_unihan_variant(0x6B6F, kZVariant, 0x9F52).
unicode_unihan_variant(0x6B71, kSemanticVariant, 0x8E35). %<kFenn
unicode_unihan_variant(0x6B72, kSemanticVariant, 0x4E97). %<kMatthews 0x5C81<kMatthews 0x5D57 0x6B73<kMatthews 0x21ED5<kLau,kMeyerWempe
unicode_unihan_variant(0x6B72, kSimplifiedVariant, 0x5C81).
unicode_unihan_variant(0x6B72, kZVariant, 0x4E97).
unicode_unihan_variant(0x6B73, kSemanticVariant, 0x6B72). %<kMatthews
unicode_unihan_variant(0x6B73, kZVariant, 0x6B72).
unicode_unihan_variant(0x6B74, kSemanticVariant, 0x66C6). %<kMeyerWempe 0x6B77<kMeyerWempe
unicode_unihan_variant(0x6B74, kZVariant, 0x6B77).
unicode_unihan_variant(0x6B77, kSemanticVariant, 0x66C6). %<kMeyerWempe 0x6B74<kMeyerWempe
unicode_unihan_variant(0x6B77, kSimplifiedVariant, 0x5386).
unicode_unihan_variant(0x6B78, kSemanticVariant, 0x5F52). %<kMatthews
unicode_unihan_variant(0x6B78, kSimplifiedVariant, 0x5F52).
unicode_unihan_variant(0x6B78, kZVariant, 0x5E30).
unicode_unihan_variant(0x6B79, kSemanticVariant, 0x6B7A). %<kLau,kMatthews
unicode_unihan_variant(0x6B79, kZVariant, 0x6B7A).
unicode_unihan_variant(0x6B7A, kSemanticVariant, 0x6B79). %<kLau,kMatthews
unicode_unihan_variant(0x6B7A, kZVariant, 0x6B79).
unicode_unihan_variant(0x6B7C, kTraditionalVariant, 0x6BB2).
unicode_unihan_variant(0x6B7F, kSemanticVariant, 0x6B81). %<kMatthews
unicode_unihan_variant(0x6B7F, kSimplifiedVariant, 0x6B81).
unicode_unihan_variant(0x6B80, kZVariant, 0x592D).
unicode_unihan_variant(0x6B81, kSemanticVariant, 0x6B7F). %<kMatthews
unicode_unihan_variant(0x6B81, kTraditionalVariant, 0x6B7F).
unicode_unihan_variant(0x6B83, kSemanticVariant, 0x4103). %<kMatthews
unicode_unihan_variant(0x6B87, kTraditionalVariant, 0x6BA4).
unicode_unihan_variant(0x6B89, kSemanticVariant, 0x4F9A). %<kMatthews 0x72E5<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6B8B, kTraditionalVariant, 0x6B98).
unicode_unihan_variant(0x6B8D, kSpecializedSemanticVariant, 0x83A9). %<kMeyerWempe
unicode_unihan_variant(0x6B92, kTraditionalVariant, 0x6B9E).
unicode_unihan_variant(0x6B92, kZVariant, 0x9695).
unicode_unihan_variant(0x6B93, kTraditionalVariant, 0x6BAE).
unicode_unihan_variant(0x6B98, kSimplifiedVariant, 0x6B8B).
unicode_unihan_variant(0x6B9A, kTraditionalVariant, 0x6BAB).
unicode_unihan_variant(0x6B9E, kSimplifiedVariant, 0x6B92).
unicode_unihan_variant(0x6B9E, kZVariant, 0x9695).
unicode_unihan_variant(0x6BA1, kTraditionalVariant, 0x6BAF).
unicode_unihan_variant(0x6BA4, kSimplifiedVariant, 0x6B87).
unicode_unihan_variant(0x6BA8, kSimplifiedVariant, 0x3C6E).
unicode_unihan_variant(0x6BAB, kSimplifiedVariant, 0x6B9A).
unicode_unihan_variant(0x6BAE, kSimplifiedVariant, 0x6B93).
unicode_unihan_variant(0x6BAF, kSimplifiedVariant, 0x6BA1).
unicode_unihan_variant(0x6BB0, kSemanticVariant, 0x2688C). %<kMeyerWempe
unicode_unihan_variant(0x6BB0, kSimplifiedVariant, 0x3C69).
unicode_unihan_variant(0x6BB1, kZVariant, 0x6BB2).
unicode_unihan_variant(0x6BB2, kSimplifiedVariant, 0x6B7C).
unicode_unihan_variant(0x6BB3, kSemanticVariant, 0x6778). %<kMatthews
unicode_unihan_variant(0x6BB4, kTraditionalVariant, 0x6BC6).
unicode_unihan_variant(0x6BBA, kSimplifiedVariant, 0x6740).
unicode_unihan_variant(0x6BBA, kZVariant, 0xF970).
unicode_unihan_variant(0x6BBB, kSemanticVariant, 0x58F3). %<kFenn
unicode_unihan_variant(0x6BBB, kSpecializedSemanticVariant, 0x6BBC). %<kFenn
unicode_unihan_variant(0x6BBB, kZVariant, 0x6BBC).
unicode_unihan_variant(0x6BBC, kSemanticVariant, 0x3C7F). %<kMatthews
unicode_unihan_variant(0x6BBC, kSimplifiedVariant, 0x58F3).
unicode_unihan_variant(0x6BBC, kSpecializedSemanticVariant, 0x58F3). %<kFenn 0x6BBB<kFenn
unicode_unihan_variant(0x6BBD, kSemanticVariant, 0x6DC6). %<kLau
unicode_unihan_variant(0x6BC0, kSimplifiedVariant, 0x6BC1).
unicode_unihan_variant(0x6BC1, kTraditionalVariant, 0x6BC0).
unicode_unihan_variant(0x6BC2, kTraditionalVariant, 0x8F42).
unicode_unihan_variant(0x6BC6, kSimplifiedVariant, 0x6BB4).
unicode_unihan_variant(0x6BCE, kZVariant, 0x6BCF).
unicode_unihan_variant(0x6BCF, kZVariant, 0x6BCE).
unicode_unihan_variant(0x6BD3, kSemanticVariant, 0x80B2). %<kMatthews
unicode_unihan_variant(0x6BD3, kZVariant, 0x80B2).
unicode_unihan_variant(0x6BD5, kTraditionalVariant, 0x7562).
unicode_unihan_variant(0x6BD7, kSemanticVariant, 0x6BD8). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6BD7, kZVariant, 0x6BD8).
unicode_unihan_variant(0x6BD8, kSemanticVariant, 0x6BD7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6BD9, kTraditionalVariant, 0x6583).
unicode_unihan_variant(0x6BE1, kSemanticVariant, 0x6C08). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6BE1, kTraditionalVariant, 0x6C08).
unicode_unihan_variant(0x6BE7, kSemanticVariant, 0x3C93). %<kMatthews
unicode_unihan_variant(0x6BEC, kZVariant, 0x7403).
unicode_unihan_variant(0x6BF5, kTraditionalVariant, 0x6BFF).
unicode_unihan_variant(0x6BFF, kSimplifiedVariant, 0x6BF5).
unicode_unihan_variant(0x6C02, kSimplifiedVariant, 0x7266).
unicode_unihan_variant(0x6C02, kZVariant, 0x729B).
unicode_unihan_variant(0x6C05, kSemanticVariant, 0x9DE9). %<kFenn
unicode_unihan_variant(0x6C07, kTraditionalVariant, 0x6C0C).
unicode_unihan_variant(0x6C08, kSemanticVariant, 0x6BE1). %<kMatthews,kMeyerWempe 0x6C0A<kMatthews
unicode_unihan_variant(0x6C08, kSimplifiedVariant, 0x6BE1).
unicode_unihan_variant(0x6C0A, kSemanticVariant, 0x6C08). %<kMatthews
unicode_unihan_variant(0x6C0C, kSimplifiedVariant, 0x6C07).
unicode_unihan_variant(0x6C13, kSemanticVariant, 0x753F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6C14, kTraditionalVariant, 0x6C23).
unicode_unihan_variant(0x6C17, kZVariant, 0x6C23).
unicode_unihan_variant(0x6C1C, kSemanticVariant, 0x967D). %<kMatthews 0x9633<kFenn
unicode_unihan_variant(0x6C22, kTraditionalVariant, 0x6C2B).
unicode_unihan_variant(0x6C23, kSemanticVariant, 0x7081). %<kLau,kMatthews
unicode_unihan_variant(0x6C23, kSimplifiedVariant, 0x6C14).
unicode_unihan_variant(0x6C23, kZVariant, 0x6C17).
unicode_unihan_variant(0x6C24, kSemanticVariant, 0x7D6A). %<kMeyerWempe
unicode_unihan_variant(0x6C29, kTraditionalVariant, 0x6C2C).
unicode_unihan_variant(0x6C2B, kSimplifiedVariant, 0x6C22).
unicode_unihan_variant(0x6C2C, kSimplifiedVariant, 0x6C29).
unicode_unihan_variant(0x6C2F, kSemanticVariant, 0x7DA0). %<kMatthews
unicode_unihan_variant(0x6C32, kTraditionalVariant, 0x6C33).
unicode_unihan_variant(0x6C33, kSimplifiedVariant, 0x6C32).
unicode_unihan_variant(0x6C34, kSemanticVariant, 0x6C35). %<kMatthews
unicode_unihan_variant(0x6C35, kSemanticVariant, 0x6C34). %<kMatthews
unicode_unihan_variant(0x6C37, kSemanticVariant, 0x51AB). %<kMatthews 0x51B0<kLau,kMatthews
unicode_unihan_variant(0x6C37, kZVariant, 0x51B0).
unicode_unihan_variant(0x6C3C, kSemanticVariant, 0x6EBA). %<kMatthews
unicode_unihan_variant(0x6C3D, kZVariant, 0x5C3F).
unicode_unihan_variant(0x6C3E, kZVariant, 0x6CDB).
unicode_unihan_variant(0x6C47, kTraditionalVariant, 0x532F). %0x5F59
unicode_unihan_variant(0x6C47, kZVariant, 0x5F59).
unicode_unihan_variant(0x6C49, kTraditionalVariant, 0x6F22).
unicode_unihan_variant(0x6C4E, kSemanticVariant, 0x6CDB). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6C4E, kZVariant, 0x6CDB).
unicode_unihan_variant(0x6C59, kSemanticVariant, 0x6C61). %<kLau 0x6D3F<kMatthews 0x6C5A<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6C59, kZVariant, 0x6C5A).
unicode_unihan_variant(0x6C5A, kSemanticVariant, 0x6C59). %<kMatthews,kMeyerWempe 0x6D3F<kMatthews
unicode_unihan_variant(0x6C5A, kZVariant, 0x6C59).
unicode_unihan_variant(0x6C61, kSemanticVariant, 0x6C59). %<kLau
unicode_unihan_variant(0x6C61, kZVariant, 0x6C59).
unicode_unihan_variant(0x6C64, kTraditionalVariant, 0x6E6F).
unicode_unihan_variant(0x6C68, kSpecializedSemanticVariant, 0x6C69).
unicode_unihan_variant(0x6C69, kSpecializedSemanticVariant, 0x6C68).
unicode_unihan_variant(0x6C6B, kSpecializedSemanticVariant, 0x9631). %<kHanYu
unicode_unihan_variant(0x6C79, kTraditionalVariant, 0x6D36).
unicode_unihan_variant(0x6C7A, kSemanticVariant, 0x51B3). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6C7A, kSimplifiedVariant, 0x51B3).
unicode_unihan_variant(0x6C85, kZVariant, 0x6E90).
unicode_unihan_variant(0x6C88, kSemanticVariant, 0x6C89). %<kMatthews
unicode_unihan_variant(0x6C88, kSpecializedSemanticVariant, 0x6C89). %<kFenn
unicode_unihan_variant(0x6C88, kTraditionalVariant, 0x700B).
unicode_unihan_variant(0x6C89, kSemanticVariant, 0x6C88). %<kMatthews
unicode_unihan_variant(0x6C89, kSpecializedSemanticVariant, 0x6C88). %<kFenn
unicode_unihan_variant(0x6C89, kZVariant, 0x700B).
unicode_unihan_variant(0x6C8D, kSemanticVariant, 0x51B1). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6C8D, kZVariant, 0x51B1).
unicode_unihan_variant(0x6C92, kSimplifiedVariant, 0x6CA1).
unicode_unihan_variant(0x6C92, kSpecializedSemanticVariant, 0x6CA1). %<kFenn
unicode_unihan_variant(0x6C96, kSemanticVariant, 0x51B2). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6C96, kSimplifiedVariant, 0x51B2).
unicode_unihan_variant(0x6C96, kZVariant, 0x76C5).
unicode_unihan_variant(0x6C9F, kTraditionalVariant, 0x6E9D).
unicode_unihan_variant(0x6CA1, kSpecializedSemanticVariant, 0x6C92). %<kFenn
unicode_unihan_variant(0x6CA1, kTraditionalVariant, 0x6C92).
unicode_unihan_variant(0x6CA2, kZVariant, 0x6FA4).
unicode_unihan_variant(0x6CA3, kTraditionalVariant, 0x7043).
unicode_unihan_variant(0x6CA4, kTraditionalVariant, 0x6F1A).
unicode_unihan_variant(0x6CA5, kTraditionalVariant, 0x701D).
unicode_unihan_variant(0x6CA6, kTraditionalVariant, 0x6DEA).
unicode_unihan_variant(0x6CA7, kTraditionalVariant, 0x6EC4).
unicode_unihan_variant(0x6CA8, kTraditionalVariant, 0x6E22).
unicode_unihan_variant(0x6CA8, kZVariant, 0x6E22).
unicode_unihan_variant(0x6CA9, kTraditionalVariant, 0x6E88).
unicode_unihan_variant(0x6CAA, kSemanticVariant, 0x51B1). %<kMeyerWempe
unicode_unihan_variant(0x6CAA, kTraditionalVariant, 0x6EEC).
unicode_unihan_variant(0x6CB1, kSemanticVariant, 0x6CB2). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6CB1, kZVariant, 0x6CB2).
unicode_unihan_variant(0x6CB2, kSemanticVariant, 0x6CB1). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6CB2, kZVariant, 0x6CB1).
unicode_unihan_variant(0x6CB5, kZVariant, 0x6FD4).
unicode_unihan_variant(0x6CB8, kSemanticVariant, 0x3D52). %<kFenn
unicode_unihan_variant(0x6CC1, kSemanticVariant, 0x51B5). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6CC1, kSimplifiedVariant, 0x51B5).
unicode_unihan_variant(0x6CC4, kSemanticVariant, 0x6D29). %<kHKGlyph,kLau,kMeyerWempe 0x6E2B<kFenn
unicode_unihan_variant(0x6CC9, kSemanticVariant, 0x6D24). %<kMatthews
unicode_unihan_variant(0x6CCA, kZVariant, 0x6CFA).
unicode_unihan_variant(0x6CD6, kZVariant, 0x8305).
unicode_unihan_variant(0x6CD9, kZVariant, 0x6D34).
unicode_unihan_variant(0x6CDB, kSemanticVariant, 0x6C4E). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6CDB, kZVariant, 0x6C4E).
unicode_unihan_variant(0x6CDD, kSemanticVariant, 0x9061). %<kMatthews
unicode_unihan_variant(0x6CDD, kZVariant, 0x6EAF).
unicode_unihan_variant(0x6CDE, kTraditionalVariant, 0x6FD8).
unicode_unihan_variant(0x6CE1, kSpecializedSemanticVariant, 0x7B94). %<kMeyerWempe
unicode_unihan_variant(0x6CE5, kSemanticVariant, 0x576D). %<kLau,kMatthews
unicode_unihan_variant(0x6CE5, kSpecializedSemanticVariant, 0x576D). %<kFenn
unicode_unihan_variant(0x6CE5, kZVariant, 0xF9E3).
unicode_unihan_variant(0x6CE8, kTraditionalVariant, 0x8A3B).
unicode_unihan_variant(0x6CEA, kSemanticVariant, 0x6DDA). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6CEA, kTraditionalVariant, 0x6DDA).
unicode_unihan_variant(0x6CF6, kTraditionalVariant, 0x6FA9).
unicode_unihan_variant(0x6CF7, kTraditionalVariant, 0x7027).
unicode_unihan_variant(0x6CF8, kTraditionalVariant, 0x7018).
unicode_unihan_variant(0x6CFA, kTraditionalVariant, 0x6FFC).
unicode_unihan_variant(0x6CFB, kTraditionalVariant, 0x7009).
unicode_unihan_variant(0x6CFC, kTraditionalVariant, 0x6F51).
unicode_unihan_variant(0x6CFD, kTraditionalVariant, 0x6FA4).
unicode_unihan_variant(0x6CFE, kTraditionalVariant, 0x6D87).
unicode_unihan_variant(0x6D01, kTraditionalVariant, 0x6F54).
unicode_unihan_variant(0x6D0C, kZVariant, 0x51BD).
unicode_unihan_variant(0x6D12, kSemanticVariant, 0x7051). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6D12, kTraditionalVariant, 0x7051).
unicode_unihan_variant(0x6D1A, kZVariant, 0x6D2A).
unicode_unihan_variant(0x6D1B, kZVariant, 0xF915).
unicode_unihan_variant(0x6D1E, kSemanticVariant, 0x5CD2). %<kMeyerWempe
unicode_unihan_variant(0x6D1F, kZVariant, 0x6D95).
unicode_unihan_variant(0x6D24, kSemanticVariant, 0x6CC9). %<kMatthews
unicode_unihan_variant(0x6D29, kSemanticVariant, 0x6CC4). %<kHKGlyph,kLau,kMeyerWempe 0x6E2B<kFenn
unicode_unihan_variant(0x6D2A, kZVariant, 0x6D1A).
unicode_unihan_variant(0x6D34, kZVariant, 0x6CD9).
unicode_unihan_variant(0x6D36, kSimplifiedVariant, 0x6C79).
unicode_unihan_variant(0x6D3C, kTraditionalVariant, 0x7AAA).
unicode_unihan_variant(0x6D3F, kSemanticVariant, 0x6C59). %<kMatthews 0x6C5A<kMatthews
unicode_unihan_variant(0x6D41, kZVariant, 0xF9CA).
unicode_unihan_variant(0x6D43, kTraditionalVariant, 0x6D79).
unicode_unihan_variant(0x6D44, kZVariant, 0x6DE8).
unicode_unihan_variant(0x6D45, kTraditionalVariant, 0x6DFA).
unicode_unihan_variant(0x6D46, kTraditionalVariant, 0x6F3F).
unicode_unihan_variant(0x6D47, kTraditionalVariant, 0x6F86).
unicode_unihan_variant(0x6D48, kTraditionalVariant, 0x6E5E).
unicode_unihan_variant(0x6D49, kZVariant, 0x6EAE).
unicode_unihan_variant(0x6D4A, kTraditionalVariant, 0x6FC1).
unicode_unihan_variant(0x6D4B, kTraditionalVariant, 0x6E2C).
unicode_unihan_variant(0x6D4D, kTraditionalVariant, 0x6FAE).
unicode_unihan_variant(0x6D4E, kTraditionalVariant, 0x6FDF).
unicode_unihan_variant(0x6D4F, kTraditionalVariant, 0x700F).
unicode_unihan_variant(0x6D50, kTraditionalVariant, 0x6EFB).
unicode_unihan_variant(0x6D51, kTraditionalVariant, 0x6E3E).
unicode_unihan_variant(0x6D52, kTraditionalVariant, 0x6EF8).
unicode_unihan_variant(0x6D53, kTraditionalVariant, 0x6FC3).
unicode_unihan_variant(0x6D54, kTraditionalVariant, 0x6F6F).
unicode_unihan_variant(0x6D55, kTraditionalVariant, 0x6FDC).
unicode_unihan_variant(0x6D5A, kSemanticVariant, 0x6FEC). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6D5A, kZVariant, 0x6FEC).
unicode_unihan_variant(0x6D5C, kZVariant, 0x6FF1).
unicode_unihan_variant(0x6D63, kSemanticVariant, 0x6FA3). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6D69, kZVariant, 0x6F94).
unicode_unihan_variant(0x6D6A, kZVariant, 0xF92A).
unicode_unihan_variant(0x6D79, kSimplifiedVariant, 0x6D43).
unicode_unihan_variant(0x6D7C, kSemanticVariant, 0x51C2). %<kMatthews 0x6F63<kHanYu
unicode_unihan_variant(0x6D82, kTraditionalVariant, 0x5857).
unicode_unihan_variant(0x6D87, kSimplifiedVariant, 0x6CFE).
unicode_unihan_variant(0x6D89, kZVariant, 0x6E09).
unicode_unihan_variant(0x6D8C, kSemanticVariant, 0x6E67). %<kMatthews
unicode_unihan_variant(0x6D8C, kZVariant, 0x6E67).
unicode_unihan_variant(0x6D8E, kSemanticVariant, 0x3CC4). %<kMatthews
unicode_unihan_variant(0x6D95, kSemanticVariant, 0x4D8F). %<kMatthews
unicode_unihan_variant(0x6D95, kZVariant, 0x6D1F).
unicode_unihan_variant(0x6D96, kSemanticVariant, 0x8385). %<kMatthews 0x849E<kMatthews
unicode_unihan_variant(0x6D99, kZVariant, 0x6DDA).
unicode_unihan_variant(0x6D9A, kZVariant, 0x6D97).
unicode_unihan_variant(0x6D9B, kTraditionalVariant, 0x6FE4).
unicode_unihan_variant(0x6D9C, kZVariant, 0x7006).
unicode_unihan_variant(0x6D9D, kTraditionalVariant, 0x6F87).
unicode_unihan_variant(0x6D9E, kTraditionalVariant, 0x6DF6).
unicode_unihan_variant(0x6D9F, kTraditionalVariant, 0x6F23).
unicode_unihan_variant(0x6DA0, kTraditionalVariant, 0x6F7F).
unicode_unihan_variant(0x6DA1, kTraditionalVariant, 0x6E26).
unicode_unihan_variant(0x6DA2, kTraditionalVariant, 0x6EB3).
unicode_unihan_variant(0x6DA2, kZVariant, 0x6EB3).
unicode_unihan_variant(0x6DA3, kTraditionalVariant, 0x6E19).
unicode_unihan_variant(0x6DA4, kTraditionalVariant, 0x6ECC).
unicode_unihan_variant(0x6DA6, kTraditionalVariant, 0x6F64).
unicode_unihan_variant(0x6DA7, kTraditionalVariant, 0x6F97).
unicode_unihan_variant(0x6DA8, kTraditionalVariant, 0x6F32).
unicode_unihan_variant(0x6DA9, kTraditionalVariant, 0x6F80).
unicode_unihan_variant(0x6DB6, kSemanticVariant, 0x553E). %<kMatthews
unicode_unihan_variant(0x6DB6, kSpecializedSemanticVariant, 0x553E). %<kFenn
unicode_unihan_variant(0x6DB8, kZVariant, 0x51C5).
unicode_unihan_variant(0x6DBC, kSemanticVariant, 0x51C9). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6DBC, kSimplifiedVariant, 0x51C9).
unicode_unihan_variant(0x6DBC, kZVariant, 0xF979).
unicode_unihan_variant(0x6DC0, kTraditionalVariant, 0x6FB1).
unicode_unihan_variant(0x6DC6, kSemanticVariant, 0x6BBD). %<kLau
unicode_unihan_variant(0x6DCB, kZVariant, 0xF9F5).
unicode_unihan_variant(0x6DD2, kSemanticVariant, 0x51C4). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6DD2, kZVariant, 0x51C4).
unicode_unihan_variant(0x6DDA, kSemanticVariant, 0x6CEA). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6DDA, kSimplifiedVariant, 0x6CEA).
unicode_unihan_variant(0x6DDA, kZVariant, 0xF94D).
unicode_unihan_variant(0x6DDC, kSemanticVariant, 0x6F30). %<kMeyerWempe
unicode_unihan_variant(0x6DE1, kSemanticVariant, 0x6FB9). %<kFenn
unicode_unihan_variant(0x6DE1, kSpecializedSemanticVariant, 0x6FB9). %<kMeyerWempe
unicode_unihan_variant(0x6DE1, kZVariant, 0x6FB9).
unicode_unihan_variant(0x6DE5, kSimplifiedVariant, 0x6E0C).
unicode_unihan_variant(0x6DE8, kSemanticVariant, 0x51C8). %<kLau,kMatthews
unicode_unihan_variant(0x6DE8, kSpecializedSemanticVariant, 0x51C8). %<kMeyerWempe
unicode_unihan_variant(0x6DE8, kZVariant, 0x51C8).
unicode_unihan_variant(0x6DEA, kSimplifiedVariant, 0x6CA6).
unicode_unihan_variant(0x6DEB, kSemanticVariant, 0x5A6C). %<kMeyerWempe
unicode_unihan_variant(0x6DEB, kZVariant, 0x5A6C).
unicode_unihan_variant(0x6DF3, kSemanticVariant, 0x6E7B). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6DF5, kSimplifiedVariant, 0x6E0A).
unicode_unihan_variant(0x6DF6, kSimplifiedVariant, 0x6D9E).
unicode_unihan_variant(0x6DF8, kZVariant, 0x6E05).
unicode_unihan_variant(0x6DF9, kSemanticVariant, 0x6E30). %<kMatthews
unicode_unihan_variant(0x6DFA, kSimplifiedVariant, 0x6D45).
unicode_unihan_variant(0x6E05, kZVariant, 0x6DF8).
unicode_unihan_variant(0x6E07, kZVariant, 0x6E34).
unicode_unihan_variant(0x6E08, kZVariant, 0x6FDF).
unicode_unihan_variant(0x6E09, kZVariant, 0x6D89).
unicode_unihan_variant(0x6E0A, kTraditionalVariant, 0x6DF5).
unicode_unihan_variant(0x6E0B, kZVariant, 0x6F80).
unicode_unihan_variant(0x6E0C, kTraditionalVariant, 0x6DE5).
unicode_unihan_variant(0x6E0D, kTraditionalVariant, 0x6F2C).
unicode_unihan_variant(0x6E0E, kTraditionalVariant, 0x7006).
unicode_unihan_variant(0x6E10, kTraditionalVariant, 0x6F38).
unicode_unihan_variant(0x6E11, kTraditionalVariant, 0x6FA0).
unicode_unihan_variant(0x6E13, kZVariant, 0x6EAA).
unicode_unihan_variant(0x6E14, kTraditionalVariant, 0x6F01).
unicode_unihan_variant(0x6E15, kZVariant, 0x6DF5).
unicode_unihan_variant(0x6E17, kTraditionalVariant, 0x6EF2).
unicode_unihan_variant(0x6E19, kSimplifiedVariant, 0x6DA3).
unicode_unihan_variant(0x6E1A, kSemanticVariant, 0x967C). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6E1B, kSemanticVariant, 0x51CF). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6E1B, kSimplifiedVariant, 0x51CF).
unicode_unihan_variant(0x6E22, kSimplifiedVariant, 0x6CA8).
unicode_unihan_variant(0x6E22, kZVariant, 0x6CA8).
unicode_unihan_variant(0x6E26, kSimplifiedVariant, 0x6DA1).
unicode_unihan_variant(0x6E28, kSemanticVariant, 0x9688). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6E28, kSpecializedSemanticVariant, 0x504E). %<kMeyerWempe
unicode_unihan_variant(0x6E29, kSemanticVariant, 0x6EAB). %<kMatthews
unicode_unihan_variant(0x6E29, kTraditionalVariant, 0x6EAB).
unicode_unihan_variant(0x6E2B, kSemanticVariant, 0x6CC4). %0x6D29<kFenn
unicode_unihan_variant(0x6E2C, kSimplifiedVariant, 0x6D4B).
unicode_unihan_variant(0x6E30, kSemanticVariant, 0x6DF9). %<kMatthews
unicode_unihan_variant(0x6E34, kZVariant, 0x6E07).
unicode_unihan_variant(0x6E38, kZVariant, 0x9030).
unicode_unihan_variant(0x6E39, kSemanticVariant, 0x22550). %<kMeyerWempe 0x8A07<kMeyerWempe
unicode_unihan_variant(0x6E3E, kSimplifiedVariant, 0x6D51).
unicode_unihan_variant(0x6E4A, kSemanticVariant, 0x51D1). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6E4A, kSimplifiedVariant, 0x51D1).
unicode_unihan_variant(0x6E4F, kSemanticVariant, 0x9808). %<kMatthews
unicode_unihan_variant(0x6E51, kSpecializedSemanticVariant, 0x9191). %<kMeyerWempe
unicode_unihan_variant(0x6E5E, kSimplifiedVariant, 0x6D48).
unicode_unihan_variant(0x6E5F, kZVariant, 0x6CC1).
unicode_unihan_variant(0x6E67, kSemanticVariant, 0x6D8C). %<kMatthews
unicode_unihan_variant(0x6E67, kZVariant, 0x6D8C).
unicode_unihan_variant(0x6E6F, kSimplifiedVariant, 0x6C64).
unicode_unihan_variant(0x6E7B, kSemanticVariant, 0x6DF3). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6E7E, kTraditionalVariant, 0x7063).
unicode_unihan_variant(0x6E7F, kTraditionalVariant, 0x6FD5).
unicode_unihan_variant(0x6E7F, kZVariant, 0x6EBC).
unicode_unihan_variant(0x6E80, kZVariant, 0x6EFF).
unicode_unihan_variant(0x6E81, kZVariant, 0x6FDA).
unicode_unihan_variant(0x6E83, kTraditionalVariant, 0x6F70).
unicode_unihan_variant(0x6E85, kTraditionalVariant, 0x6FFA).
unicode_unihan_variant(0x6E86, kTraditionalVariant, 0x6F35).
unicode_unihan_variant(0x6E87, kTraditionalVariant, 0x6F0A).
unicode_unihan_variant(0x6E87, kZVariant, 0x6F0A).
unicode_unihan_variant(0x6E88, kSimplifiedVariant, 0x6CA9).
unicode_unihan_variant(0x6E88, kZVariant, 0x6F59).
unicode_unihan_variant(0x6E89, kSemanticVariant, 0x6461). %<kMatthews
unicode_unihan_variant(0x6E89, kZVariant, 0x6F11).
unicode_unihan_variant(0x6E8C, kZVariant, 0x6F51).
unicode_unihan_variant(0x6E90, kZVariant, 0x53B5).
unicode_unihan_variant(0x6E96, kSemanticVariant, 0x51C6). %<kMeyerWempe
unicode_unihan_variant(0x6E96, kSimplifiedVariant, 0x51C6).
unicode_unihan_variant(0x6E96, kZVariant, 0x51D6).
unicode_unihan_variant(0x6E9C, kZVariant, 0xF9CB).
unicode_unihan_variant(0x6E9D, kSimplifiedVariant, 0x6C9F).
unicode_unihan_variant(0x6EAA, kZVariant, 0x6E13).
unicode_unihan_variant(0x6EAB, kSemanticVariant, 0x6E29). %<kMatthews
unicode_unihan_variant(0x6EAB, kSimplifiedVariant, 0x6E29).
unicode_unihan_variant(0x6EAE, kZVariant, 0x6D49).
unicode_unihan_variant(0x6EAF, kSemanticVariant, 0x9061). %<kMeyerWempe
unicode_unihan_variant(0x6EAF, kSpecializedSemanticVariant, 0x3D11). %<kFenn
unicode_unihan_variant(0x6EAF, kZVariant, 0x6CDD).
unicode_unihan_variant(0x6EB3, kSimplifiedVariant, 0x6DA2).
unicode_unihan_variant(0x6EB3, kZVariant, 0x6DA2).
unicode_unihan_variant(0x6EBA, kSemanticVariant, 0x6C3C). %<kMatthews
unicode_unihan_variant(0x6EBA, kSpecializedSemanticVariant, 0x5C3F). %<kFenn
unicode_unihan_variant(0x6EBA, kZVariant, 0x5C3F).
unicode_unihan_variant(0x6EBB, kSemanticVariant, 0x891F). %<kFenn
unicode_unihan_variant(0x6EBC, kSemanticVariant, 0x6FD5). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6EBC, kZVariant, 0x6FD5).
unicode_unihan_variant(0x6EBD, kSemanticVariant, 0x7E1F). %<kLau
unicode_unihan_variant(0x6EC4, kSimplifiedVariant, 0x6CA7).
unicode_unihan_variant(0x6EC5, kSemanticVariant, 0x70D5). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6EC5, kSimplifiedVariant, 0x706D).
unicode_unihan_variant(0x6ECC, kSimplifiedVariant, 0x6DA4).
unicode_unihan_variant(0x6ECE, kSimplifiedVariant, 0x8365).
unicode_unihan_variant(0x6ED1, kZVariant, 0x78C6).
unicode_unihan_variant(0x6ED7, kTraditionalVariant, 0x6F77).
unicode_unihan_variant(0x6ED9, kSemanticVariant, 0x532F). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6EDA, kTraditionalVariant, 0x6EFE).
unicode_unihan_variant(0x6EDE, kTraditionalVariant, 0x6EEF).
unicode_unihan_variant(0x6EDF, kTraditionalVariant, 0x7067).
unicode_unihan_variant(0x6EDF, kZVariant, 0x7054).
unicode_unihan_variant(0x6EE0, kTraditionalVariant, 0x7044).
unicode_unihan_variant(0x6EE1, kTraditionalVariant, 0x6EFF).
unicode_unihan_variant(0x6EE2, kTraditionalVariant, 0x7005).
unicode_unihan_variant(0x6EE4, kTraditionalVariant, 0x6FFE).
unicode_unihan_variant(0x6EE5, kTraditionalVariant, 0x6FEB).
unicode_unihan_variant(0x6EE6, kTraditionalVariant, 0x7064).
unicode_unihan_variant(0x6EE8, kTraditionalVariant, 0x6FF1).
unicode_unihan_variant(0x6EE9, kTraditionalVariant, 0x7058).
unicode_unihan_variant(0x6EEA, kTraditionalVariant, 0x6FA6).
unicode_unihan_variant(0x6EEC, kSimplifiedVariant, 0x6CAA).
unicode_unihan_variant(0x6EEF, kSimplifiedVariant, 0x6EDE).
unicode_unihan_variant(0x6EF2, kSimplifiedVariant, 0x6E17).
unicode_unihan_variant(0x6EF8, kSimplifiedVariant, 0x6D52).
unicode_unihan_variant(0x6EFA, kSemanticVariant, 0x60A0). %<kFenn
unicode_unihan_variant(0x6EFB, kSimplifiedVariant, 0x6D50).
unicode_unihan_variant(0x6EFE, kSimplifiedVariant, 0x6EDA).
unicode_unihan_variant(0x6EFE, kSpecializedSemanticVariant, 0x83CC). %<kMeyerWempe
unicode_unihan_variant(0x6EFF, kSimplifiedVariant, 0x6EE1).
unicode_unihan_variant(0x6F01, kSemanticVariant, 0x4C77). %<kMatthews
unicode_unihan_variant(0x6F01, kSimplifiedVariant, 0x6E14).
unicode_unihan_variant(0x6F06, kSemanticVariant, 0x3BC3). %<kMatthews 0x687C<kMatthews
unicode_unihan_variant(0x6F06, kZVariant, 0x67D2).
unicode_unihan_variant(0x6F07, kSemanticVariant, 0x22CDC). %<kMeyerWempe
unicode_unihan_variant(0x6F0A, kSimplifiedVariant, 0x6E87).
unicode_unihan_variant(0x6F0A, kZVariant, 0x6E87).
unicode_unihan_variant(0x6F0E, kSemanticVariant, 0x6F68). %<kMatthews
unicode_unihan_variant(0x6F0F, kZVariant, 0xF94E).
unicode_unihan_variant(0x6F11, kZVariant, 0x6E89).
unicode_unihan_variant(0x6F13, kTraditionalVariant, 0x7055).
unicode_unihan_variant(0x6F1A, kSimplifiedVariant, 0x6CA4).
unicode_unihan_variant(0x6F22, kSimplifiedVariant, 0x6C49).
unicode_unihan_variant(0x6F23, kSimplifiedVariant, 0x6D9F).
unicode_unihan_variant(0x6F24, kTraditionalVariant, 0x7060).
unicode_unihan_variant(0x6F2B, kSemanticVariant, 0x6FB7). %<kMatthews
unicode_unihan_variant(0x6F2C, kSimplifiedVariant, 0x6E0D).
unicode_unihan_variant(0x6F30, kSemanticVariant, 0x6DDC). %<kMeyerWempe
unicode_unihan_variant(0x6F31, kZVariant, 0x6F44).
unicode_unihan_variant(0x6F32, kSimplifiedVariant, 0x6DA8).
unicode_unihan_variant(0x6F35, kSimplifiedVariant, 0x6E86).
unicode_unihan_variant(0x6F38, kSimplifiedVariant, 0x6E10).
unicode_unihan_variant(0x6F3E, kSemanticVariant, 0x7001). %<kMatthews
unicode_unihan_variant(0x6F3E, kZVariant, 0x7001).
unicode_unihan_variant(0x6F3F, kSemanticVariant, 0x2456F). %<kFenn
unicode_unihan_variant(0x6F3F, kSimplifiedVariant, 0x6D46).
unicode_unihan_variant(0x6F40, kZVariant, 0x6F68).
unicode_unihan_variant(0x6F41, kSimplifiedVariant, 0x988D).
unicode_unihan_variant(0x6F45, kZVariant, 0x704C).
unicode_unihan_variant(0x6F46, kTraditionalVariant, 0x7020).
unicode_unihan_variant(0x6F47, kTraditionalVariant, 0x701F).
unicode_unihan_variant(0x6F4B, kTraditionalVariant, 0x7032).
unicode_unihan_variant(0x6F4D, kTraditionalVariant, 0x6FF0).
unicode_unihan_variant(0x6F51, kSimplifiedVariant, 0x6CFC).
unicode_unihan_variant(0x6F51, kZVariant, 0x6E8C).
unicode_unihan_variant(0x6F54, kSimplifiedVariant, 0x6D01).
unicode_unihan_variant(0x6F59, kZVariant, 0x6E88).
unicode_unihan_variant(0x6F5B, kSemanticVariant, 0x6F5C). %<kMatthews
unicode_unihan_variant(0x6F5B, kSimplifiedVariant, 0x6F5C).
unicode_unihan_variant(0x6F5C, kSemanticVariant, 0x6F5B). %<kMatthews
unicode_unihan_variant(0x6F5C, kTraditionalVariant, 0x6F5B).
unicode_unihan_variant(0x6F5D, kSemanticVariant, 0x3D14). %<kMatthews
unicode_unihan_variant(0x6F63, kSemanticVariant, 0x6D7C). %<kHanYu
unicode_unihan_variant(0x6F64, kSimplifiedVariant, 0x6DA6).
unicode_unihan_variant(0x6F66, kSpecializedSemanticVariant, 0x6F87). %<kFenn
unicode_unihan_variant(0x6F68, kSemanticVariant, 0x6F0E). %<kMatthews
unicode_unihan_variant(0x6F68, kZVariant, 0x6F40).
unicode_unihan_variant(0x6F6C, kSemanticVariant, 0x7058). %<kMatthews
unicode_unihan_variant(0x6F6F, kSimplifiedVariant, 0x6D54).
unicode_unihan_variant(0x6F70, kSimplifiedVariant, 0x6E83).
unicode_unihan_variant(0x6F74, kTraditionalVariant, 0x7026).
unicode_unihan_variant(0x6F77, kSimplifiedVariant, 0x6ED7).
unicode_unihan_variant(0x6F78, kZVariant, 0x23F7D).
unicode_unihan_variant(0x6F7F, kSimplifiedVariant, 0x6DA0).
unicode_unihan_variant(0x6F80, kSemanticVariant, 0x6F81). %<kMeyerWempe 0x6FC7<kMatthews,kMeyerWempe 0x7012<kMatthews
unicode_unihan_variant(0x6F80, kSimplifiedVariant, 0x6DA9).
unicode_unihan_variant(0x6F80, kZVariant, 0x6F81).
unicode_unihan_variant(0x6F81, kSemanticVariant, 0x6F80). %<kMeyerWempe 0x6FC7<kMeyerWempe
unicode_unihan_variant(0x6F81, kZVariant, 0x6F80).
unicode_unihan_variant(0x6F84, kZVariant, 0x6F82).
unicode_unihan_variant(0x6F85, kSimplifiedVariant, 0x23DA9).
unicode_unihan_variant(0x6F86, kSimplifiedVariant, 0x6D47).
unicode_unihan_variant(0x6F87, kSimplifiedVariant, 0x6D9D).
unicode_unihan_variant(0x6F87, kSpecializedSemanticVariant, 0x6F66). %<kFenn
unicode_unihan_variant(0x6F88, kSemanticVariant, 0x5FB9). %<kLau
unicode_unihan_variant(0x6F91, kZVariant, 0x6E9C).
unicode_unihan_variant(0x6F94, kZVariant, 0x6D69).
unicode_unihan_variant(0x6F97, kSemanticVariant, 0x78F5). %<kMatthews
unicode_unihan_variant(0x6F97, kSimplifiedVariant, 0x6DA7).
unicode_unihan_variant(0x6F99, kZVariant, 0x6F5F).
unicode_unihan_variant(0x6F9B, kTraditionalVariant, 0x7002).
unicode_unihan_variant(0x6F9B, kZVariant, 0x7002).
unicode_unihan_variant(0x6F9C, kTraditionalVariant, 0x703E).
unicode_unihan_variant(0x6FA0, kSimplifiedVariant, 0x6E11).
unicode_unihan_variant(0x6FA3, kSemanticVariant, 0x6D63). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6FA4, kSimplifiedVariant, 0x6CFD).
unicode_unihan_variant(0x6FA4, kZVariant, 0x6CA2).
unicode_unihan_variant(0x6FA6, kSimplifiedVariant, 0x6EEA).
unicode_unihan_variant(0x6FA9, kSimplifiedVariant, 0x6CF6).
unicode_unihan_variant(0x6FAE, kSimplifiedVariant, 0x6D4D).
unicode_unihan_variant(0x6FB1, kSimplifiedVariant, 0x6DC0).
unicode_unihan_variant(0x6FB3, kSemanticVariant, 0x8956). %<kMatthews 0x2570C<kMatthews
unicode_unihan_variant(0x6FB7, kSemanticVariant, 0x6F2B). %<kMatthews
unicode_unihan_variant(0x6FB9, kSemanticVariant, 0x6DE1). %<kFenn
unicode_unihan_variant(0x6FB9, kSpecializedSemanticVariant, 0x6DE1). %<kMeyerWempe
unicode_unihan_variant(0x6FB9, kZVariant, 0x6DE1).
unicode_unihan_variant(0x6FBE, kSimplifiedVariant, 0x3CE0).
unicode_unihan_variant(0x6FC1, kSimplifiedVariant, 0x6D4A).
unicode_unihan_variant(0x6FC3, kSimplifiedVariant, 0x6D53).
unicode_unihan_variant(0x6FC4, kSimplifiedVariant, 0x3CE1).
unicode_unihan_variant(0x6FC6, kSimplifiedVariant, 0x23E23).
unicode_unihan_variant(0x6FC7, kSemanticVariant, 0x6F80). %<kMatthews,kMeyerWempe 0x6F81<kMeyerWempe 0x7012<kMatthews
unicode_unihan_variant(0x6FC7, kZVariant, 0x6F80).
unicode_unihan_variant(0x6FD1, kTraditionalVariant, 0x7028).
unicode_unihan_variant(0x6FD2, kTraditionalVariant, 0x7015).
unicode_unihan_variant(0x6FD4, kZVariant, 0x6CB5).
unicode_unihan_variant(0x6FD5, kSemanticVariant, 0x6EBC). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6FD5, kSimplifiedVariant, 0x6E7F).
unicode_unihan_variant(0x6FD8, kSimplifiedVariant, 0x6CDE).
unicode_unihan_variant(0x6FDA, kZVariant, 0x6E81).
unicode_unihan_variant(0x6FDB, kSemanticVariant, 0x9725). %<kMeyerWempe 0x9740<kMatthews
unicode_unihan_variant(0x6FDC, kSimplifiedVariant, 0x6D55).
unicode_unihan_variant(0x6FDC, kZVariant, 0x6D55).
unicode_unihan_variant(0x6FDF, kSimplifiedVariant, 0x6D4E).
unicode_unihan_variant(0x6FE4, kSimplifiedVariant, 0x6D9B).
unicode_unihan_variant(0x6FE7, kSimplifiedVariant, 0x3CD4).
unicode_unihan_variant(0x6FEB, kSimplifiedVariant, 0x6EE5).
unicode_unihan_variant(0x6FEC, kSemanticVariant, 0x6D5A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x6FEC, kZVariant, 0x6D5A).
unicode_unihan_variant(0x6FF0, kSimplifiedVariant, 0x6F4D).
unicode_unihan_variant(0x6FF1, kSemanticVariant, 0x6FF5). %<kMatthews,kMeyerWempe 0x7015<kFenn
unicode_unihan_variant(0x6FF1, kSimplifiedVariant, 0x6EE8).
unicode_unihan_variant(0x6FF3, kZVariant, 0x6F5B).
unicode_unihan_variant(0x6FF5, kSemanticVariant, 0x6FF1). %<kMatthews,kMeyerWempe 0x7015<kMatthews
unicode_unihan_variant(0x6FF6, kSemanticVariant, 0x95CA). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x6FF6, kZVariant, 0x95CA).
unicode_unihan_variant(0x6FFA, kSimplifiedVariant, 0x6E85).
unicode_unihan_variant(0x6FFC, kSimplifiedVariant, 0x6CFA).
unicode_unihan_variant(0x6FFC, kZVariant, 0x6CCA).
unicode_unihan_variant(0x6FFE, kSimplifiedVariant, 0x6EE4).
unicode_unihan_variant(0x6FFE, kZVariant, 0xF984).
unicode_unihan_variant(0x6FFF, kSemanticVariant, 0x7805). %<kMeyerWempe
unicode_unihan_variant(0x7001, kSemanticVariant, 0x6F3E). %<kMatthews
unicode_unihan_variant(0x7001, kZVariant, 0x6F3E).
unicode_unihan_variant(0x7002, kSimplifiedVariant, 0x6F9B).
unicode_unihan_variant(0x7002, kZVariant, 0x6F9B).
unicode_unihan_variant(0x7003, kSimplifiedVariant, 0x23F77).
unicode_unihan_variant(0x7005, kSimplifiedVariant, 0x6EE2).
unicode_unihan_variant(0x7006, kSimplifiedVariant, 0x6E0E).
unicode_unihan_variant(0x7007, kSimplifiedVariant, 0x3CBF).
unicode_unihan_variant(0x7009, kSimplifiedVariant, 0x6CFB).
unicode_unihan_variant(0x700B, kSimplifiedVariant, 0x6C88).
unicode_unihan_variant(0x700F, kSimplifiedVariant, 0x6D4F).
unicode_unihan_variant(0x7012, kSemanticVariant, 0x6F80). %<kMatthews 0x6FC7<kMatthews
unicode_unihan_variant(0x7012, kZVariant, 0x6F80).
unicode_unihan_variant(0x7015, kSemanticVariant, 0x6FF1). %<kFenn 0x6FF5<kMatthews
unicode_unihan_variant(0x7015, kSimplifiedVariant, 0x6FD2).
unicode_unihan_variant(0x7018, kSimplifiedVariant, 0x6CF8).
unicode_unihan_variant(0x701D, kSimplifiedVariant, 0x6CA5).
unicode_unihan_variant(0x701F, kSimplifiedVariant, 0x6F47).
unicode_unihan_variant(0x7020, kSimplifiedVariant, 0x6F46).
unicode_unihan_variant(0x7026, kSimplifiedVariant, 0x6F74).
unicode_unihan_variant(0x7027, kSimplifiedVariant, 0x6CF7).
unicode_unihan_variant(0x7028, kSimplifiedVariant, 0x6FD1).
unicode_unihan_variant(0x7028, kZVariant, 0x702C).
unicode_unihan_variant(0x702C, kZVariant, 0x7028).
unicode_unihan_variant(0x7030, kSimplifiedVariant, 0x3CFD).
unicode_unihan_variant(0x7030, kZVariant, 0x5F4C).
unicode_unihan_variant(0x7032, kSimplifiedVariant, 0x6F4B).
unicode_unihan_variant(0x703E, kSimplifiedVariant, 0x6F9C).
unicode_unihan_variant(0x7043, kSimplifiedVariant, 0x6CA3).
unicode_unihan_variant(0x7044, kSimplifiedVariant, 0x6EE0).
unicode_unihan_variant(0x704C, kZVariant, 0x6F45).
unicode_unihan_variant(0x704E, kZVariant, 0x7054).
unicode_unihan_variant(0x704F, kTraditionalVariant, 0x705D).
unicode_unihan_variant(0x7051, kSemanticVariant, 0x6D12). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7051, kSimplifiedVariant, 0x6D12).
unicode_unihan_variant(0x7054, kZVariant, 0x7069).
unicode_unihan_variant(0x7055, kSimplifiedVariant, 0x6F13).
unicode_unihan_variant(0x7058, kSemanticVariant, 0x6F6C). %<kMatthews
unicode_unihan_variant(0x7058, kSimplifiedVariant, 0x6EE9).
unicode_unihan_variant(0x7059, kSimplifiedVariant, 0x23EBC).
unicode_unihan_variant(0x705D, kSimplifiedVariant, 0x704F).
unicode_unihan_variant(0x7060, kSimplifiedVariant, 0x6F24).
unicode_unihan_variant(0x7061, kSimplifiedVariant, 0x3CD5).
unicode_unihan_variant(0x7063, kSimplifiedVariant, 0x6E7E).
unicode_unihan_variant(0x7064, kSimplifiedVariant, 0x6EE6).
unicode_unihan_variant(0x7067, kSimplifiedVariant, 0x6EDF).
unicode_unihan_variant(0x7067, kZVariant, 0x7054).
unicode_unihan_variant(0x706B, kSemanticVariant, 0x706C). %<kMatthews
unicode_unihan_variant(0x706C, kSemanticVariant, 0x706B). %<kMatthews
unicode_unihan_variant(0x706D, kTraditionalVariant, 0x6EC5).
unicode_unihan_variant(0x706E, kZVariant, 0x5149).
unicode_unihan_variant(0x706F, kSemanticVariant, 0x71C8). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x706F, kTraditionalVariant, 0x71C8).
unicode_unihan_variant(0x7070, kSemanticVariant, 0x2F835). %<kFenn
unicode_unihan_variant(0x7074, kSemanticVariant, 0x70D8). %<kMatthews
unicode_unihan_variant(0x7075, kSemanticVariant, 0x9748). %<kFenn
unicode_unihan_variant(0x7075, kTraditionalVariant, 0x9748).
unicode_unihan_variant(0x7076, kSemanticVariant, 0x7AC8). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x707D, kSemanticVariant, 0x707E). %<kLau,kMatthews,kMeyerWempe 0x70D6<kLau,kMatthews 0x83D1<kFenn
unicode_unihan_variant(0x707D, kSimplifiedVariant, 0x707E).
unicode_unihan_variant(0x707D, kZVariant, 0x70D6).
unicode_unihan_variant(0x707E, kSemanticVariant, 0x707D). %<kLau,kMatthews,kMeyerWempe 0x83D1<kFenn 0x70D6<kLau,kMatthews
unicode_unihan_variant(0x707E, kTraditionalVariant, 0x707D).
unicode_unihan_variant(0x707F, kTraditionalVariant, 0x71E6).
unicode_unihan_variant(0x7080, kTraditionalVariant, 0x716C).
unicode_unihan_variant(0x7081, kSemanticVariant, 0x6C23). %<kLau,kMatthews
unicode_unihan_variant(0x7085, kZVariant, 0x803F).
unicode_unihan_variant(0x7089, kSemanticVariant, 0x7210). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7089, kTraditionalVariant, 0x7210).
unicode_unihan_variant(0x7094, kZVariant, 0x803F).
unicode_unihan_variant(0x7099, kZVariant, 0xF9FB).
unicode_unihan_variant(0x709C, kTraditionalVariant, 0x7152).
unicode_unihan_variant(0x709D, kTraditionalVariant, 0x7197).
unicode_unihan_variant(0x70A4, kSemanticVariant, 0x7167). %<kLau,kMatthews
unicode_unihan_variant(0x70A4, kZVariant, 0x7167).
unicode_unihan_variant(0x70AB, kSpecializedSemanticVariant, 0x8852).
unicode_unihan_variant(0x70AE, kSemanticVariant, 0x7832). %<kMeyerWempe 0x70B0<kMatthews 0x792E<kMeyerWempe
unicode_unihan_variant(0x70AE, kSpecializedSemanticVariant, 0x70B0). %<kFenn
unicode_unihan_variant(0x70AE, kZVariant, 0x7832).
unicode_unihan_variant(0x70AF, kSemanticVariant, 0x70F1). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x70AF, kZVariant, 0x70F1).
unicode_unihan_variant(0x70B0, kSemanticVariant, 0x70AE). %<kMatthews
unicode_unihan_variant(0x70B0, kSpecializedSemanticVariant, 0x70AE). %<kFenn
unicode_unihan_variant(0x70B0, kZVariant, 0x70AE).
unicode_unihan_variant(0x70B8, kSemanticVariant, 0x7160). %<kLau,kMatthews
unicode_unihan_variant(0x70B8, kSpecializedSemanticVariant, 0x7160). %<kFenn
unicode_unihan_variant(0x70B9, kSemanticVariant, 0x9EDE). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x70B9, kTraditionalVariant, 0x9EDE).
unicode_unihan_variant(0x70BA, kSemanticVariant, 0x7232). %<kHKGlyph
unicode_unihan_variant(0x70BA, kSimplifiedVariant, 0x4E3A).
unicode_unihan_variant(0x70BA, kZVariant, 0x7232).
unicode_unihan_variant(0x70BC, kTraditionalVariant, 0x7149).
unicode_unihan_variant(0x70BD, kTraditionalVariant, 0x71BE).
unicode_unihan_variant(0x70C1, kTraditionalVariant, 0x720D).
unicode_unihan_variant(0x70C2, kTraditionalVariant, 0x721B).
unicode_unihan_variant(0x70C3, kTraditionalVariant, 0x70F4).
unicode_unihan_variant(0x70C9, kZVariant, 0x7165).
unicode_unihan_variant(0x70CA, kSemanticVariant, 0x716C). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x70CF, kSimplifiedVariant, 0x4E4C).
unicode_unihan_variant(0x70D5, kSemanticVariant, 0x6EC5). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x70D6, kSemanticVariant, 0x707D). %<kLau,kMatthews 0x707E<kLau,kMatthews 0x83D1<kFenn
unicode_unihan_variant(0x70D6, kZVariant, 0x707D).
unicode_unihan_variant(0x70D8, kSemanticVariant, 0x7074). %<kMatthews
unicode_unihan_variant(0x70D9, kZVariant, 0xF916).
unicode_unihan_variant(0x70DB, kSemanticVariant, 0x71ED). %<kLau,kMatthews
unicode_unihan_variant(0x70DB, kTraditionalVariant, 0x71ED).
unicode_unihan_variant(0x70DC, kSemanticVariant, 0x6645). %<kMatthews
unicode_unihan_variant(0x70DD, kSemanticVariant, 0x84B8). %<kFenn
unicode_unihan_variant(0x70DF, kSemanticVariant, 0x5B8C). %<kLau 0x83F8<kFenn 0x7159<kHKGlyph,kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x70DF, kSpecializedSemanticVariant, 0x83F8).
unicode_unihan_variant(0x70DF, kTraditionalVariant, 0x7159).
unicode_unihan_variant(0x70E6, kTraditionalVariant, 0x7169).
unicode_unihan_variant(0x70E7, kTraditionalVariant, 0x71D2).
unicode_unihan_variant(0x70E8, kTraditionalVariant, 0x71C1).
unicode_unihan_variant(0x70E9, kTraditionalVariant, 0x71F4).
unicode_unihan_variant(0x70EB, kTraditionalVariant, 0x71D9).
unicode_unihan_variant(0x70EC, kTraditionalVariant, 0x71FC).
unicode_unihan_variant(0x70ED, kTraditionalVariant, 0x71B1).
unicode_unihan_variant(0x70F1, kSemanticVariant, 0x70AF). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x70F1, kZVariant, 0x70AF).
unicode_unihan_variant(0x70F4, kSimplifiedVariant, 0x70C3).
unicode_unihan_variant(0x7114, kZVariant, 0x7130).
unicode_unihan_variant(0x7115, kTraditionalVariant, 0x7165).
unicode_unihan_variant(0x7116, kTraditionalVariant, 0x71DC).
unicode_unihan_variant(0x7118, kTraditionalVariant, 0x71FE).
unicode_unihan_variant(0x7121, kSemanticVariant, 0x65E0). %<kFenn 0x6A46<kMatthews
unicode_unihan_variant(0x7121, kSimplifiedVariant, 0x65E0).
unicode_unihan_variant(0x7126, kZVariant, 0x7133).
unicode_unihan_variant(0x712D, kZVariant, 0x7162).
unicode_unihan_variant(0x7130, kSemanticVariant, 0x71C4). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7130, kZVariant, 0x7114).
unicode_unihan_variant(0x713C, kZVariant, 0x71D2).
unicode_unihan_variant(0x7145, kSemanticVariant, 0x935B). %<kLau,kMatthews
unicode_unihan_variant(0x7145, kZVariant, 0x935B).
unicode_unihan_variant(0x7146, kSemanticVariant, 0x935C). %<kMeyerWempe
unicode_unihan_variant(0x7147, kSemanticVariant, 0x8F1D). %<kFenn
unicode_unihan_variant(0x7149, kSimplifiedVariant, 0x70BC).
unicode_unihan_variant(0x7149, kZVariant, 0xF993).
unicode_unihan_variant(0x7151, kSemanticVariant, 0x716E). %<kFenn
unicode_unihan_variant(0x7151, kZVariant, 0x716E).
unicode_unihan_variant(0x7152, kSimplifiedVariant, 0x709C).
unicode_unihan_variant(0x7155, kZVariant, 0x7199).
unicode_unihan_variant(0x7156, kSemanticVariant, 0x6696). %<kLau,kMatthews
unicode_unihan_variant(0x7156, kSpecializedSemanticVariant, 0x5A20). %<kFenn 0x6696<kFenn
unicode_unihan_variant(0x7159, kSemanticVariant, 0x5B8C). %<kLau 0x70DF<kHKGlyph,kLau,kMatthews,kMeyerWempe 0x83F8
unicode_unihan_variant(0x7159, kSimplifiedVariant, 0x70DF).
unicode_unihan_variant(0x7159, kSpecializedSemanticVariant, 0x83F8).
unicode_unihan_variant(0x7160, kSemanticVariant, 0x70B8). %<kLau,kMatthews
unicode_unihan_variant(0x7160, kSpecializedSemanticVariant, 0x70B8). %<kFenn
unicode_unihan_variant(0x7162, kSimplifiedVariant, 0x8315).
unicode_unihan_variant(0x7165, kSimplifiedVariant, 0x7115).
unicode_unihan_variant(0x7165, kZVariant, 0x70C9).
unicode_unihan_variant(0x7167, kSemanticVariant, 0x70A4). %<kLau,kMatthews
unicode_unihan_variant(0x7167, kZVariant, 0x70A4).
unicode_unihan_variant(0x7169, kSimplifiedVariant, 0x70E6).
unicode_unihan_variant(0x716C, kSemanticVariant, 0x70CA). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x716C, kSimplifiedVariant, 0x7080).
unicode_unihan_variant(0x716E, kSemanticVariant, 0x7151). %<kFenn
unicode_unihan_variant(0x716E, kZVariant, 0x7151).
unicode_unihan_variant(0x7171, kSimplifiedVariant, 0x3DBD).
unicode_unihan_variant(0x7173, kSemanticVariant, 0x3DFB). %<kMatthews
unicode_unihan_variant(0x7174, kTraditionalVariant, 0x7185).
unicode_unihan_variant(0x717D, kZVariant, 0x6427).
unicode_unihan_variant(0x7185, kSimplifiedVariant, 0x7174).
unicode_unihan_variant(0x7187, kSemanticVariant, 0x71FA). %<kMatthews
unicode_unihan_variant(0x7188, kSemanticVariant, 0x7199). %<kMatthews
unicode_unihan_variant(0x7188, kZVariant, 0x7199).
unicode_unihan_variant(0x7189, kSimplifiedVariant, 0x24236).
unicode_unihan_variant(0x718C, kSimplifiedVariant, 0x241C4).
unicode_unihan_variant(0x718F, kSemanticVariant, 0x71FB). %<kLau,kMatthews,kMeyerWempe 0x24455
unicode_unihan_variant(0x718F, kZVariant, 0x71FB).
unicode_unihan_variant(0x7192, kSimplifiedVariant, 0x8367).
unicode_unihan_variant(0x7193, kSimplifiedVariant, 0x241A1).
unicode_unihan_variant(0x7194, kSemanticVariant, 0x9394). %<kLau,kMatthews
unicode_unihan_variant(0x7194, kZVariant, 0x9394).
unicode_unihan_variant(0x7197, kSimplifiedVariant, 0x709D).
unicode_unihan_variant(0x7199, kSemanticVariant, 0x7188). %<kMatthews
unicode_unihan_variant(0x7199, kZVariant, 0x7155).
unicode_unihan_variant(0x71A1, kSimplifiedVariant, 0x242CF).
unicode_unihan_variant(0x71A8, kSemanticVariant, 0x3DC9). %<kMatthews
unicode_unihan_variant(0x71A8, kSpecializedSemanticVariant, 0x3DC9). %<kFenn 0x5C09<kFenn
unicode_unihan_variant(0x71B1, kSemanticVariant, 0x24360). %<kLau,kMeyerWempe
unicode_unihan_variant(0x71B1, kSimplifiedVariant, 0x70ED).
unicode_unihan_variant(0x71B2, kSimplifiedVariant, 0x988E).
unicode_unihan_variant(0x71B9, kZVariant, 0x71BA).
unicode_unihan_variant(0x71BA, kZVariant, 0x71B9).
unicode_unihan_variant(0x71BE, kSimplifiedVariant, 0x70BD).
unicode_unihan_variant(0x71C1, kSimplifiedVariant, 0x70E8).
unicode_unihan_variant(0x71C4, kSemanticVariant, 0x7130). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x71C8, kSemanticVariant, 0x706F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x71C8, kSimplifiedVariant, 0x706F).
unicode_unihan_variant(0x71CE, kZVariant, 0xF9C0).
unicode_unihan_variant(0x71D0, kSemanticVariant, 0x3DE0). %<kMatthews 0x7CA6<kMatthews,kMeyerWempe
unicode_unihan_variant(0x71D0, kZVariant, 0xF9EE).
unicode_unihan_variant(0x71D2, kSemanticVariant, 0x7C2B). %<kLau
unicode_unihan_variant(0x71D2, kSimplifiedVariant, 0x70E7).
unicode_unihan_variant(0x71D5, kZVariant, 0x9DF0).
unicode_unihan_variant(0x71D7, kZVariant, 0x721B).
unicode_unihan_variant(0x71D9, kSimplifiedVariant, 0x70EB).
unicode_unihan_variant(0x71DC, kSimplifiedVariant, 0x7116).
unicode_unihan_variant(0x71DF, kSimplifiedVariant, 0x8425).
unicode_unihan_variant(0x71E3, kZVariant, 0x71F7).
unicode_unihan_variant(0x71E6, kSimplifiedVariant, 0x707F).
unicode_unihan_variant(0x71ED, kSemanticVariant, 0x70DB). %<kLau,kMatthews
unicode_unihan_variant(0x71ED, kSimplifiedVariant, 0x70DB).
unicode_unihan_variant(0x71F4, kSimplifiedVariant, 0x70E9).
unicode_unihan_variant(0x71F6, kSimplifiedVariant, 0x3DB6).
unicode_unihan_variant(0x71F7, kZVariant, 0x71E3).
unicode_unihan_variant(0x71FA, kSemanticVariant, 0x7187). %<kMatthews
unicode_unihan_variant(0x71FB, kSemanticVariant, 0x718F). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x71FB, kZVariant, 0x718F).
unicode_unihan_variant(0x71FC, kSimplifiedVariant, 0x70EC).
unicode_unihan_variant(0x71FE, kSimplifiedVariant, 0x7118).
unicode_unihan_variant(0x71FF, kSemanticVariant, 0x66DC). %<kCowles 0x8000<kCowles,kFenn
unicode_unihan_variant(0x7204, kSimplifiedVariant, 0x241C3).
unicode_unihan_variant(0x720B, kZVariant, 0x24455).
unicode_unihan_variant(0x720D, kSimplifiedVariant, 0x70C1).
unicode_unihan_variant(0x7210, kSemanticVariant, 0x7089). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7210, kSimplifiedVariant, 0x7089).
unicode_unihan_variant(0x7210, kZVariant, 0xF932).
unicode_unihan_variant(0x721B, kSimplifiedVariant, 0x70C2).
unicode_unihan_variant(0x722A, kSemanticVariant, 0x355A). %<kMatthews 0x722B<kLau,kMatthews
unicode_unihan_variant(0x722B, kSemanticVariant, 0x355A). %<kMatthews 0x722A<kLau,kMatthews
unicode_unihan_variant(0x722D, kSemanticVariant, 0x4E89). %<kMatthews
unicode_unihan_variant(0x722D, kSimplifiedVariant, 0x4E89).
unicode_unihan_variant(0x7231, kTraditionalVariant, 0x611B).
unicode_unihan_variant(0x7232, kSemanticVariant, 0x70BA). %<kHKGlyph
unicode_unihan_variant(0x7232, kZVariant, 0x70BA).
unicode_unihan_variant(0x7237, kTraditionalVariant, 0x723A).
unicode_unihan_variant(0x723A, kSimplifiedVariant, 0x7237).
unicode_unihan_variant(0x723C, kZVariant, 0x4FCE).
unicode_unihan_variant(0x723E, kSemanticVariant, 0x5C12). %<kMatthews 0x5C14<kMatthews
unicode_unihan_variant(0x723E, kSimplifiedVariant, 0x5C14).
unicode_unihan_variant(0x7240, kSemanticVariant, 0x5E8A). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7240, kZVariant, 0x5E8A).
unicode_unihan_variant(0x7244, kSemanticVariant, 0x8E4C). %<kMatthews
unicode_unihan_variant(0x7246, kSemanticVariant, 0x58BB). %<kLau,kMatthews,kMeyerWempe 0x5EE7<kLau,kMatthews
unicode_unihan_variant(0x7246, kSimplifiedVariant, 0x5899).
unicode_unihan_variant(0x7246, kZVariant, 0x58BB).
unicode_unihan_variant(0x7248, kSemanticVariant, 0x677F). %<kMeyerWempe
unicode_unihan_variant(0x724B, kSemanticVariant, 0x7B8B). %<kLau,kMatthews
unicode_unihan_variant(0x724B, kZVariant, 0x7B8B).
unicode_unihan_variant(0x724D, kTraditionalVariant, 0x7258).
unicode_unihan_variant(0x7255, kSemanticVariant, 0x7ABB). %<kFenn 0x7A97<kFenn 0x7A93<kLau,kMatthews
unicode_unihan_variant(0x7258, kSimplifiedVariant, 0x724D).
unicode_unihan_variant(0x725B, kSemanticVariant, 0x725C). %<kMatthews
unicode_unihan_variant(0x725C, kSemanticVariant, 0x725B). %<kMatthews
unicode_unihan_variant(0x725F, kSemanticVariant, 0x9EB0). %<kMatthews
unicode_unihan_variant(0x7260, kSemanticVariant, 0x4ED6). %<kMatthews 0x5B83<kLau
unicode_unihan_variant(0x7260, kSpecializedSemanticVariant, 0x5B83).
unicode_unihan_variant(0x7260, kZVariant, 0x5B83).
unicode_unihan_variant(0x7262, kZVariant, 0xF946).
unicode_unihan_variant(0x7266, kTraditionalVariant, 0x6C02).
unicode_unihan_variant(0x7266, kZVariant, 0x729B).
unicode_unihan_variant(0x7274, kSemanticVariant, 0x89DD). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7274, kSpecializedSemanticVariant, 0x7274).
unicode_unihan_variant(0x7274, kZVariant, 0x89DD).
unicode_unihan_variant(0x7275, kTraditionalVariant, 0x727D).
unicode_unihan_variant(0x727A, kTraditionalVariant, 0x72A7).
unicode_unihan_variant(0x727D, kSimplifiedVariant, 0x7275).
unicode_unihan_variant(0x7281, kSemanticVariant, 0x7282). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7281, kZVariant, 0x7282).
unicode_unihan_variant(0x7282, kSemanticVariant, 0x7281). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7282, kZVariant, 0x7281).
unicode_unihan_variant(0x7284, kSpecializedSemanticVariant, 0x8E26). %<kFenn
unicode_unihan_variant(0x7287, kSemanticVariant, 0x5954). %<kMatthews
unicode_unihan_variant(0x7287, kSpecializedSemanticVariant, 0x5954). %<kFenn
unicode_unihan_variant(0x7287, kZVariant, 0x5954).
unicode_unihan_variant(0x728A, kTraditionalVariant, 0x72A2).
unicode_unihan_variant(0x7296, kSimplifiedVariant, 0x8366).
unicode_unihan_variant(0x729B, kZVariant, 0x9AE6).
unicode_unihan_variant(0x72A0, kZVariant, 0x72A7).
unicode_unihan_variant(0x72A2, kSimplifiedVariant, 0x728A).
unicode_unihan_variant(0x72A7, kSimplifiedVariant, 0x727A).
unicode_unihan_variant(0x72AC, kSemanticVariant, 0x72AD). %<kLau,kMatthews
unicode_unihan_variant(0x72AD, kSemanticVariant, 0x72AC). %<kLau,kMatthews
unicode_unihan_variant(0x72B2, kSemanticVariant, 0x8C7A). %<kMatthews
unicode_unihan_variant(0x72B2, kZVariant, 0x8C7A).
unicode_unihan_variant(0x72B4, kSemanticVariant, 0x8C7B). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x72B6, kTraditionalVariant, 0x72C0).
unicode_unihan_variant(0x72B7, kTraditionalVariant, 0x7377).
unicode_unihan_variant(0x72B8, kTraditionalVariant, 0x7341).
unicode_unihan_variant(0x72B9, kSemanticVariant, 0x7336). %<kFenn
unicode_unihan_variant(0x72B9, kTraditionalVariant, 0x7336).
unicode_unihan_variant(0x72C0, kSimplifiedVariant, 0x72B6).
unicode_unihan_variant(0x72C8, kTraditionalVariant, 0x72FD).
unicode_unihan_variant(0x72DD, kTraditionalVariant, 0x736E).
unicode_unihan_variant(0x72DE, kTraditionalVariant, 0x7370).
unicode_unihan_variant(0x72E2, kZVariant, 0x8C89).
unicode_unihan_variant(0x72E5, kSemanticVariant, 0x4F9A). %<kMatthews 0x6B89<kMatthews,kMeyerWempe
unicode_unihan_variant(0x72EC, kSemanticVariant, 0x7368). %<kFenn
unicode_unihan_variant(0x72EC, kTraditionalVariant, 0x7368).
unicode_unihan_variant(0x72ED, kTraditionalVariant, 0x72F9).
unicode_unihan_variant(0x72EE, kTraditionalVariant, 0x7345).
unicode_unihan_variant(0x72EF, kTraditionalVariant, 0x736A).
unicode_unihan_variant(0x72F0, kTraditionalVariant, 0x7319).
unicode_unihan_variant(0x72F1, kTraditionalVariant, 0x7344).
unicode_unihan_variant(0x72F2, kTraditionalVariant, 0x733B).
unicode_unihan_variant(0x72F5, kSemanticVariant, 0x5C28). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x72F7, kSemanticVariant, 0x7367). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x72F8, kSemanticVariant, 0x8C8D). %<kHKGlyph,kMatthews,kMeyerWempe
unicode_unihan_variant(0x72F9, kSemanticVariant, 0x965C). %<kFenn
unicode_unihan_variant(0x72F9, kSimplifiedVariant, 0x72ED).
unicode_unihan_variant(0x72FB, kSemanticVariant, 0x475C). %<kMeyerWempe
unicode_unihan_variant(0x72FC, kZVariant, 0xF92B).
unicode_unihan_variant(0x72FD, kSimplifiedVariant, 0x72C8).
unicode_unihan_variant(0x7303, kTraditionalVariant, 0x736B).
unicode_unihan_variant(0x7307, kSemanticVariant, 0x552C). %<kMeyerWempe 0x8653<kMatthews,kMeyerWempe
unicode_unihan_variant(0x730B, kSemanticVariant, 0x98C7). %<kMatthews 0x98AE<kMatthews
unicode_unihan_variant(0x730E, kTraditionalVariant, 0x7375).
unicode_unihan_variant(0x7315, kTraditionalVariant, 0x737C).
unicode_unihan_variant(0x7317, kSemanticVariant, 0x6B39). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7319, kSimplifiedVariant, 0x72F0).
unicode_unihan_variant(0x731F, kZVariant, 0x7375).
unicode_unihan_variant(0x7321, kTraditionalVariant, 0x7380).
unicode_unihan_variant(0x7328, kSemanticVariant, 0x876F). %<kMatthews,kMeyerWempe 0x733F<kMatthews,kMeyerWempe
unicode_unihan_variant(0x732A, kSemanticVariant, 0x8C6C). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x732A, kTraditionalVariant, 0x8C6C).
unicode_unihan_variant(0x732A, kZVariant, 0xFA16).
unicode_unihan_variant(0x732B, kSemanticVariant, 0x8C93). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x732B, kTraditionalVariant, 0x8C93).
unicode_unihan_variant(0x732C, kSemanticVariant, 0x875F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x732C, kTraditionalVariant, 0x875F).
unicode_unihan_variant(0x732E, kSemanticVariant, 0x737B). %<kLau,kMatthews
unicode_unihan_variant(0x732E, kTraditionalVariant, 0x737B).
unicode_unihan_variant(0x7336, kSemanticVariant, 0x72B9). %<kFenn
unicode_unihan_variant(0x7336, kSimplifiedVariant, 0x72B9).
unicode_unihan_variant(0x733B, kSimplifiedVariant, 0x72F2).
unicode_unihan_variant(0x733E, kZVariant, 0x736A).
unicode_unihan_variant(0x733F, kSemanticVariant, 0x7328). %<kMatthews,kMeyerWempe 0x876F<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7341, kSimplifiedVariant, 0x72B8).
unicode_unihan_variant(0x7343, kSemanticVariant, 0x5446). %<kMatthews
unicode_unihan_variant(0x7343, kSpecializedSemanticVariant, 0x5446). %<kMeyerWempe
unicode_unihan_variant(0x7344, kSimplifiedVariant, 0x72F1).
unicode_unihan_variant(0x7345, kSimplifiedVariant, 0x72EE).
unicode_unihan_variant(0x734E, kSimplifiedVariant, 0x5956).
unicode_unihan_variant(0x734E, kZVariant, 0x596C).
unicode_unihan_variant(0x7350, kSemanticVariant, 0x9E9E). %<kMatthews
unicode_unihan_variant(0x7360, kSemanticVariant, 0x4764). %<kMeyerWempe
unicode_unihan_variant(0x7363, kZVariant, 0x7378).
unicode_unihan_variant(0x7367, kSemanticVariant, 0x72F7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7368, kSemanticVariant, 0x72EC). %<kFenn
unicode_unihan_variant(0x7368, kSimplifiedVariant, 0x72EC).
unicode_unihan_variant(0x736A, kSimplifiedVariant, 0x72EF).
unicode_unihan_variant(0x736A, kZVariant, 0x733E).
unicode_unihan_variant(0x736B, kSemanticVariant, 0x7381). %<kMeyerWempe
unicode_unihan_variant(0x736B, kSimplifiedVariant, 0x7303).
unicode_unihan_variant(0x736D, kTraditionalVariant, 0x737A).
unicode_unihan_variant(0x736E, kSimplifiedVariant, 0x72DD).
unicode_unihan_variant(0x7370, kSimplifiedVariant, 0x72DE).
unicode_unihan_variant(0x7371, kSimplifiedVariant, 0x3E8D).
unicode_unihan_variant(0x7372, kSimplifiedVariant, 0x83B7).
unicode_unihan_variant(0x7375, kSemanticVariant, 0x248AA<kTang).
unicode_unihan_variant(0x7375, kSimplifiedVariant, 0x730E).
unicode_unihan_variant(0x7375, kZVariant, 0xF9A7).
unicode_unihan_variant(0x7377, kSimplifiedVariant, 0x72B7).
unicode_unihan_variant(0x7378, kSimplifiedVariant, 0x517D).
unicode_unihan_variant(0x737A, kSimplifiedVariant, 0x736D).
unicode_unihan_variant(0x737B, kSemanticVariant, 0x732E). %<kLau,kMatthews
unicode_unihan_variant(0x737B, kSimplifiedVariant, 0x732E).
unicode_unihan_variant(0x737C, kSimplifiedVariant, 0x7315).
unicode_unihan_variant(0x737E, kSemanticVariant, 0x8C9B). %<kMatthews
unicode_unihan_variant(0x7380, kSimplifiedVariant, 0x7321).
unicode_unihan_variant(0x7381, kSemanticVariant, 0x736B). %<kMeyerWempe
unicode_unihan_variant(0x7381, kSimplifiedVariant, 0x247A4).
unicode_unihan_variant(0x7384, kSemanticVariant, 0x4F2D). %<kMatthews
unicode_unihan_variant(0x7385, kSemanticVariant, 0x5999). %<kLau,kMatthews
unicode_unihan_variant(0x7385, kZVariant, 0x5999).
unicode_unihan_variant(0x7386, kSemanticVariant, 0x5179). %<kMatthews 0x8332<kMatthews
unicode_unihan_variant(0x7386, kZVariant, 0x8332).
unicode_unihan_variant(0x7387, kZVariant, 0xF961).
unicode_unihan_variant(0x7391, kTraditionalVariant, 0x74A3).
unicode_unihan_variant(0x7396, kSemanticVariant, 0x4E5D). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7399, kZVariant, 0x74B5).
unicode_unihan_variant(0x739A, kTraditionalVariant, 0x7452).
unicode_unihan_variant(0x739B, kTraditionalVariant, 0x746A).
unicode_unihan_variant(0x739F, kSemanticVariant, 0x73AB). %<kLau,kMatthews
unicode_unihan_variant(0x739F, kZVariant, 0x73C9).
unicode_unihan_variant(0x73AB, kSemanticVariant, 0x739F). %<kLau,kMatthews
unicode_unihan_variant(0x73AE, kTraditionalVariant, 0x744B).
unicode_unihan_variant(0x73AF, kTraditionalVariant, 0x74B0).
unicode_unihan_variant(0x73B0, kTraditionalVariant, 0x73FE).
unicode_unihan_variant(0x73B1, kTraditionalVariant, 0x7472).
unicode_unihan_variant(0x73B2, kZVariant, 0xF9AD).
unicode_unihan_variant(0x73B3, kSemanticVariant, 0x7447). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x73BA, kTraditionalVariant, 0x74BD).
unicode_unihan_variant(0x73C9, kSemanticVariant, 0x40C9). %<kMatthews 0x7807<kFenn
unicode_unihan_variant(0x73C9, kZVariant, 0x739F).
unicode_unihan_variant(0x73CD, kSemanticVariant, 0x73CE). %<kLau,kMatthews
unicode_unihan_variant(0x73CD, kZVariant, 0x73CE).
unicode_unihan_variant(0x73CE, kSemanticVariant, 0x73CD). %<kLau,kMatthews
unicode_unihan_variant(0x73CE, kZVariant, 0x73CD).
unicode_unihan_variant(0x73D0, kTraditionalVariant, 0x743A).
unicode_unihan_variant(0x73D1, kTraditionalVariant, 0x74CF).
unicode_unihan_variant(0x73DE, kZVariant, 0xF917).
unicode_unihan_variant(0x73EA, kSemanticVariant, 0x572D). %<kMatthews
unicode_unihan_variant(0x73F0, kTraditionalVariant, 0x74AB).
unicode_unihan_variant(0x73F2, kTraditionalVariant, 0x743F).
unicode_unihan_variant(0x73F6, kZVariant, 0x7445).
unicode_unihan_variant(0x73F7, kSemanticVariant, 0x7894). %<kMeyerWempe
unicode_unihan_variant(0x73FE, kSimplifiedVariant, 0x73B0).
unicode_unihan_variant(0x7404, kSemanticVariant, 0x9799). %<kMeyerWempe
unicode_unihan_variant(0x7405, kSemanticVariant, 0x746F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7405, kZVariant, 0x746F).
unicode_unihan_variant(0x7406, kZVariant, 0xF9E4).
unicode_unihan_variant(0x7409, kSemanticVariant, 0x7460). %<kLau,kMatthews
unicode_unihan_variant(0x7409, kZVariant, 0xF9CC).
unicode_unihan_variant(0x740E, kZVariant, 0x74A1).
unicode_unihan_variant(0x740F, kTraditionalVariant, 0x7489).
unicode_unihan_variant(0x7410, kTraditionalVariant, 0x7463).
unicode_unihan_variant(0x7431, kZVariant, 0x96D5).
unicode_unihan_variant(0x743A, kSimplifiedVariant, 0x73D0).
unicode_unihan_variant(0x743C, kTraditionalVariant, 0x74CA).
unicode_unihan_variant(0x743F, kSimplifiedVariant, 0x73F2).
unicode_unihan_variant(0x7445, kZVariant, 0x73F6).
unicode_unihan_variant(0x7447, kSemanticVariant, 0x73B3). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x744B, kSimplifiedVariant, 0x73AE).
unicode_unihan_variant(0x7452, kSimplifiedVariant, 0x739A).
unicode_unihan_variant(0x7459, kSemanticVariant, 0x78AF). %<kMatthews
unicode_unihan_variant(0x7460, kSemanticVariant, 0x7409). %<kLau,kMatthews
unicode_unihan_variant(0x7463, kSimplifiedVariant, 0x7410).
unicode_unihan_variant(0x7464, kSimplifiedVariant, 0x7476).
unicode_unihan_variant(0x7469, kSimplifiedVariant, 0x83B9).
unicode_unihan_variant(0x7469, kZVariant, 0xF9AE).
unicode_unihan_variant(0x746A, kSimplifiedVariant, 0x739B).
unicode_unihan_variant(0x746F, kSemanticVariant, 0x7405). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x746F, kZVariant, 0x7405).
unicode_unihan_variant(0x7472, kSimplifiedVariant, 0x73B1).
unicode_unihan_variant(0x7476, kTraditionalVariant, 0x7464).
unicode_unihan_variant(0x7477, kTraditionalVariant, 0x74A6).
unicode_unihan_variant(0x7478, kZVariant, 0x74B8).
unicode_unihan_variant(0x747D, kSimplifiedVariant, 0x2AED0).
unicode_unihan_variant(0x7481, kSemanticVariant, 0x249DA). %<kMeyerWempe
unicode_unihan_variant(0x7483, kSemanticVariant, 0x74C8). %<kFenn
unicode_unihan_variant(0x7487, kSemanticVariant, 0x74BF). %<kMatthews
unicode_unihan_variant(0x7489, kSimplifiedVariant, 0x740F).
unicode_unihan_variant(0x748E, kTraditionalVariant, 0x74D4).
unicode_unihan_variant(0x7498, kZVariant, 0xF9EF).
unicode_unihan_variant(0x74A1, kZVariant, 0x740E).
unicode_unihan_variant(0x74A2, kSemanticVariant, 0x7409). %<kMeyerWempe
unicode_unihan_variant(0x74A3, kSimplifiedVariant, 0x7391).
unicode_unihan_variant(0x74A6, kSimplifiedVariant, 0x7477).
unicode_unihan_variant(0x74AB, kSimplifiedVariant, 0x73F0).
unicode_unihan_variant(0x74AF, kSimplifiedVariant, 0x3EC5).
unicode_unihan_variant(0x74B0, kSimplifiedVariant, 0x73AF).
unicode_unihan_variant(0x74B5, kZVariant, 0x7399).
unicode_unihan_variant(0x74B8, kZVariant, 0x7478).
unicode_unihan_variant(0x74BD, kSemanticVariant, 0x58D0). %<kMatthews
unicode_unihan_variant(0x74BD, kSimplifiedVariant, 0x73BA).
unicode_unihan_variant(0x74BF, kSemanticVariant, 0x7487). %<kMatthews
unicode_unihan_variant(0x74C8, kSemanticVariant, 0x7483). %<kFenn
unicode_unihan_variant(0x74CA, kSimplifiedVariant, 0x743C).
unicode_unihan_variant(0x74CF, kSimplifiedVariant, 0x73D1).
unicode_unihan_variant(0x74D2, kTraditionalVariant, 0x74DA).
unicode_unihan_variant(0x74D4, kSimplifiedVariant, 0x748E).
unicode_unihan_variant(0x74D5, kSimplifiedVariant, 0x24980).
unicode_unihan_variant(0x74DA, kSimplifiedVariant, 0x74D2).
unicode_unihan_variant(0x74DF, kSemanticVariant, 0x530F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x74EE, kSemanticVariant, 0x7F4B). %<kMatthews 0x7515<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x74EF, kTraditionalVariant, 0x750C).
unicode_unihan_variant(0x74F6, kSemanticVariant, 0x7F3E). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x74F6, kZVariant, 0x7F3E).
unicode_unihan_variant(0x74F7, kSemanticVariant, 0x78C1). %<kMatthews
unicode_unihan_variant(0x74F7, kZVariant, 0x7506).
unicode_unihan_variant(0x7501, kZVariant, 0x74F6).
unicode_unihan_variant(0x7506, kZVariant, 0x74F7).
unicode_unihan_variant(0x750C, kSimplifiedVariant, 0x74EF).
unicode_unihan_variant(0x750E, kSemanticVariant, 0x78DA). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7515, kSemanticVariant, 0x74EE). %<kLau,kMatthews,kMeyerWempe 0x7F4B<kMatthews
unicode_unihan_variant(0x7516, kSemanticVariant, 0x7F4C). %<kMatthews 0x7F43<kMatthews
unicode_unihan_variant(0x751A, kZVariant, 0xF9FD).
unicode_unihan_variant(0x751C, kZVariant, 0x751B).
unicode_unihan_variant(0x751E, kSemanticVariant, 0x5617). %<kMeyerWempe
unicode_unihan_variant(0x751E, kZVariant, 0x5617).
unicode_unihan_variant(0x7522, kSimplifiedVariant, 0x4EA7).
unicode_unihan_variant(0x7523, kZVariant, 0x7522).
unicode_unihan_variant(0x7524, kSemanticVariant, 0x8564). %<kMatthews
unicode_unihan_variant(0x7526, kSemanticVariant, 0x7A4C). %<kLau,kMatthews
unicode_unihan_variant(0x7529, kSpecializedSemanticVariant, 0x6454). %<kFenn
unicode_unihan_variant(0x752A, kZVariant, 0x89D2).
unicode_unihan_variant(0x752C, kSemanticVariant, 0x57C7). %<kFenn
unicode_unihan_variant(0x752F, kSemanticVariant, 0x5BD7). %<kMatthews
unicode_unihan_variant(0x7535, kTraditionalVariant, 0x96FB).
unicode_unihan_variant(0x753A, kSemanticVariant, 0x5722). %<kMeyerWempe
unicode_unihan_variant(0x753B, kSemanticVariant, 0x756B). %<kMatthews
unicode_unihan_variant(0x753B, kSpecializedSemanticVariant, 0x756B). %<kMeyerWempe
unicode_unihan_variant(0x753B, kTraditionalVariant, 0x756B).
unicode_unihan_variant(0x753D, kSemanticVariant, 0x754E). %<kMatthews
unicode_unihan_variant(0x753E, kSemanticVariant, 0x83D1). %<kMatthews
unicode_unihan_variant(0x753F, kSemanticVariant, 0x6C13). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7544, kSemanticVariant, 0x7559). %<kFenn 0x7571<kFenn
unicode_unihan_variant(0x7544, kZVariant, 0x7559).
unicode_unihan_variant(0x7545, kTraditionalVariant, 0x66A2).
unicode_unihan_variant(0x7546, kSemanticVariant, 0x755D). %<kFenn
unicode_unihan_variant(0x7546, kZVariant, 0x755D).
unicode_unihan_variant(0x754A, kSemanticVariant, 0x8015). %<kLau,kMatthews
unicode_unihan_variant(0x754A, kSpecializedSemanticVariant, 0x8015). %<kFenn
unicode_unihan_variant(0x754C, kZVariant, 0x754D).
unicode_unihan_variant(0x754D, kZVariant, 0x754C).
unicode_unihan_variant(0x754E, kSemanticVariant, 0x753D). %<kMatthews
unicode_unihan_variant(0x7552, kZVariant, 0x755D).
unicode_unihan_variant(0x7559, kSemanticVariant, 0x7544). %<kFenn 0x7571<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7559, kZVariant, 0x7544).
unicode_unihan_variant(0x755D, kSemanticVariant, 0x7546). %<kFenn
unicode_unihan_variant(0x755D, kSimplifiedVariant, 0x4EA9).
unicode_unihan_variant(0x755D, kZVariant, 0x756E).
unicode_unihan_variant(0x7562, kSimplifiedVariant, 0x6BD5).
unicode_unihan_variant(0x7565, kSemanticVariant, 0x7567). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x7565, kZVariant, 0x7567).
unicode_unihan_variant(0x7567, kSemanticVariant, 0x7565). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x7567, kZVariant, 0x7565).
unicode_unihan_variant(0x756A, kZVariant, 0x8E6F).
unicode_unihan_variant(0x756B, kSemanticVariant, 0x753B). %<kMatthews 0x7575<kMatthews
unicode_unihan_variant(0x756B, kSimplifiedVariant, 0x753B).
unicode_unihan_variant(0x756B, kSpecializedSemanticVariant, 0x753B). %<kMeyerWempe
unicode_unihan_variant(0x756B, kZVariant, 0x753B).
unicode_unihan_variant(0x756C, kZVariant, 0x756D).
unicode_unihan_variant(0x756D, kZVariant, 0x756C).
unicode_unihan_variant(0x756E, kZVariant, 0x755D).
unicode_unihan_variant(0x7570, kSimplifiedVariant, 0x5F02).
unicode_unihan_variant(0x7570, kZVariant, 0xF962).
unicode_unihan_variant(0x7571, kSemanticVariant, 0x7544). %<kFenn 0x7559<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7572, kZVariant, 0x756C).
unicode_unihan_variant(0x7573, kZVariant, 0x758A).
unicode_unihan_variant(0x7574, kTraditionalVariant, 0x7587).
unicode_unihan_variant(0x7575, kSemanticVariant, 0x756B). %<kMatthews
unicode_unihan_variant(0x7575, kZVariant, 0x756B).
unicode_unihan_variant(0x7576, kSimplifiedVariant, 0x5F53).
unicode_unihan_variant(0x7576, kSpecializedSemanticVariant, 0x5F53). %<kFenn
unicode_unihan_variant(0x757A, kSemanticVariant, 0x7586). %<kMatthews
unicode_unihan_variant(0x757A, kSpecializedSemanticVariant, 0x5C07). %<kFenn
unicode_unihan_variant(0x757D, kSemanticVariant, 0x7583). %<kMatthews
unicode_unihan_variant(0x7583, kSemanticVariant, 0x757D). %<kMatthews
unicode_unihan_variant(0x7586, kSemanticVariant, 0x757A). %<kMatthews
unicode_unihan_variant(0x7586, kSpecializedSemanticVariant, 0x5C07). %<kFenn
unicode_unihan_variant(0x7587, kSimplifiedVariant, 0x7574).
unicode_unihan_variant(0x7589, kZVariant, 0x758A).
unicode_unihan_variant(0x758A, kSemanticVariant, 0x53E0). %<kMatthews,kMeyerWempe 0x66E1<kMatthews
unicode_unihan_variant(0x758A, kSimplifiedVariant, 0x53E0).
unicode_unihan_variant(0x758A, kZVariant, 0x66E1).
unicode_unihan_variant(0x758B, kSemanticVariant, 0x5339). %<kMeyerWempe
unicode_unihan_variant(0x758B, kZVariant, 0x5339).
unicode_unihan_variant(0x758D, kSemanticVariant, 0x86CB). %<kMeyerWempe
unicode_unihan_variant(0x758E, kSemanticVariant, 0x758F). %<kFenn
unicode_unihan_variant(0x758F, kSemanticVariant, 0x758E). %<kFenn
unicode_unihan_variant(0x7596, kTraditionalVariant, 0x7664).
unicode_unihan_variant(0x7597, kTraditionalVariant, 0x7642).
unicode_unihan_variant(0x759F, kTraditionalVariant, 0x7627).
unicode_unihan_variant(0x75A0, kTraditionalVariant, 0x7658).
unicode_unihan_variant(0x75A1, kTraditionalVariant, 0x760D).
unicode_unihan_variant(0x75A3, kSemanticVariant, 0x80AC). %<kMatthews
unicode_unihan_variant(0x75AC, kTraditionalVariant, 0x7667).
unicode_unihan_variant(0x75AD, kTraditionalVariant, 0x7632).
unicode_unihan_variant(0x75AE, kTraditionalVariant, 0x7621).
unicode_unihan_variant(0x75AF, kTraditionalVariant, 0x760B).
unicode_unihan_variant(0x75B1, kSemanticVariant, 0x76B0). %<kMatthews
unicode_unihan_variant(0x75B1, kTraditionalVariant, 0x76B0).
unicode_unihan_variant(0x75B4, kSemanticVariant, 0x75FE). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x75B4, kTraditionalVariant, 0x75FE).
unicode_unihan_variant(0x75B8, kSemanticVariant, 0x7649). %<kLau,kMeyerWempe
unicode_unihan_variant(0x75B9, kSemanticVariant, 0x80D7). %<kMatthews
unicode_unihan_variant(0x75C7, kTraditionalVariant, 0x7665).
unicode_unihan_variant(0x75C8, kTraditionalVariant, 0x7670).
unicode_unihan_variant(0x75C9, kTraditionalVariant, 0x75D9).
unicode_unihan_variant(0x75D0, kSemanticVariant, 0x8698). %<kMeyerWempe
unicode_unihan_variant(0x75D2, kSemanticVariant, 0x7662). %<kMatthews
unicode_unihan_variant(0x75D2, kTraditionalVariant, 0x7662).
unicode_unihan_variant(0x75D6, kTraditionalVariant, 0x7602).
unicode_unihan_variant(0x75D9, kSimplifiedVariant, 0x75C9).
unicode_unihan_variant(0x75E2, kZVariant, 0xF9E5).
unicode_unihan_variant(0x75E8, kTraditionalVariant, 0x7646).
unicode_unihan_variant(0x75E9, kZVariant, 0x7626).
unicode_unihan_variant(0x75EA, kTraditionalVariant, 0x7613).
unicode_unihan_variant(0x75EB, kTraditionalVariant, 0x7647).
unicode_unihan_variant(0x75EE, kSemanticVariant, 0x8139). %<kMatthews
unicode_unihan_variant(0x75F2, kSpecializedSemanticVariant, 0x75F3). %<kMeyerWempe
unicode_unihan_variant(0x75F2, kZVariant, 0x75F3).
unicode_unihan_variant(0x75F3, kSpecializedSemanticVariant, 0x75F2). %<kMeyerWempe
unicode_unihan_variant(0x75F3, kZVariant, 0x75F2).
unicode_unihan_variant(0x75F4, kSemanticVariant, 0x7661). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x75F9, kSemanticVariant, 0x75FA). %<kMatthews
unicode_unihan_variant(0x75FA, kSemanticVariant, 0x75F9). %<kMatthews
unicode_unihan_variant(0x75FC, kSemanticVariant, 0x3F7D). %<kMeyerWempe
unicode_unihan_variant(0x75FE, kSemanticVariant, 0x75B4). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x75FE, kSimplifiedVariant, 0x75B4).
unicode_unihan_variant(0x7602, kSemanticVariant, 0x555E). %<kFenn
unicode_unihan_variant(0x7602, kSimplifiedVariant, 0x75D6).
unicode_unihan_variant(0x7605, kTraditionalVariant, 0x7649).
unicode_unihan_variant(0x7606, kTraditionalVariant, 0x762E).
unicode_unihan_variant(0x7609, kSemanticVariant, 0x7652). %<kHKGlyph
unicode_unihan_variant(0x7609, kSpecializedSemanticVariant, 0x6108). %<kFenn
unicode_unihan_variant(0x760B, kSimplifiedVariant, 0x75AF).
unicode_unihan_variant(0x760D, kSimplifiedVariant, 0x75A1).
unicode_unihan_variant(0x7613, kSimplifiedVariant, 0x75EA).
unicode_unihan_variant(0x7617, kTraditionalVariant, 0x761E).
unicode_unihan_variant(0x7618, kTraditionalVariant, 0x763A).
unicode_unihan_variant(0x7618, kZVariant, 0x763A).
unicode_unihan_variant(0x761E, kSimplifiedVariant, 0x7617).
unicode_unihan_variant(0x7621, kSimplifiedVariant, 0x75AE).
unicode_unihan_variant(0x7626, kZVariant, 0x75E9).
unicode_unihan_variant(0x7627, kSimplifiedVariant, 0x759F).
unicode_unihan_variant(0x7627, kSpecializedSemanticVariant, 0x5F04). %<kFenn
unicode_unihan_variant(0x7628, kSemanticVariant, 0x7672). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x762A, kTraditionalVariant, 0x765F).
unicode_unihan_variant(0x762B, kTraditionalVariant, 0x7671).
unicode_unihan_variant(0x762E, kSimplifiedVariant, 0x7606).
unicode_unihan_variant(0x7632, kSimplifiedVariant, 0x75AD).
unicode_unihan_variant(0x7636, kSemanticVariant, 0x55FD). %<kMatthews
unicode_unihan_variant(0x763A, kSemanticVariant, 0x763B). %<kMatthews
unicode_unihan_variant(0x763A, kSimplifiedVariant, 0x7618).
unicode_unihan_variant(0x763B, kSemanticVariant, 0x763A). %<kMatthews
unicode_unihan_variant(0x763E, kTraditionalVariant, 0x766E).
unicode_unihan_variant(0x763F, kTraditionalVariant, 0x766D).
unicode_unihan_variant(0x7642, kSimplifiedVariant, 0x7597).
unicode_unihan_variant(0x7646, kSimplifiedVariant, 0x75E8).
unicode_unihan_variant(0x7647, kSimplifiedVariant, 0x75EB).
unicode_unihan_variant(0x7649, kSemanticVariant, 0x75B8). %<kLau,kMeyerWempe
unicode_unihan_variant(0x7649, kSimplifiedVariant, 0x7605).
unicode_unihan_variant(0x7652, kSemanticVariant, 0x7609). %<kHKGlyph
unicode_unihan_variant(0x7652, kZVariant, 0x6108).
unicode_unihan_variant(0x7658, kSimplifiedVariant, 0x75A0).
unicode_unihan_variant(0x765B, kZVariant, 0x765D).
unicode_unihan_variant(0x765E, kTraditionalVariant, 0x7669).
unicode_unihan_variant(0x765F, kSimplifiedVariant, 0x762A).
unicode_unihan_variant(0x7661, kSemanticVariant, 0x75F4). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x7662, kSemanticVariant, 0x75D2). %<kFenn,kMatthews
unicode_unihan_variant(0x7662, kSimplifiedVariant, 0x75D2).
unicode_unihan_variant(0x7663, kTraditionalVariant, 0x766C).
unicode_unihan_variant(0x7664, kSimplifiedVariant, 0x7596).
unicode_unihan_variant(0x7665, kSimplifiedVariant, 0x75C7).
unicode_unihan_variant(0x7667, kSimplifiedVariant, 0x75AC).
unicode_unihan_variant(0x7668, kZVariant, 0x970D).
unicode_unihan_variant(0x7669, kSimplifiedVariant, 0x765E).
unicode_unihan_variant(0x7669, kZVariant, 0xF90E).
unicode_unihan_variant(0x766B, kTraditionalVariant, 0x7672).
unicode_unihan_variant(0x766C, kSimplifiedVariant, 0x7663).
unicode_unihan_variant(0x766D, kSimplifiedVariant, 0x763F).
unicode_unihan_variant(0x766E, kSimplifiedVariant, 0x763E).
unicode_unihan_variant(0x766F, kSemanticVariant, 0x81DE). %<kMeyerWempe
unicode_unihan_variant(0x7670, kSimplifiedVariant, 0x75C8).
unicode_unihan_variant(0x7671, kSimplifiedVariant, 0x762B).
unicode_unihan_variant(0x7672, kSemanticVariant, 0x7628). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7672, kSimplifiedVariant, 0x766B).
unicode_unihan_variant(0x767A, kZVariant, 0x767C).
unicode_unihan_variant(0x767B, kSemanticVariant, 0x8C4B). %<kFenn
unicode_unihan_variant(0x767C, kSimplifiedVariant, 0x53D1).
unicode_unihan_variant(0x767C, kZVariant, 0x767A).
unicode_unihan_variant(0x767E, kSemanticVariant, 0x4F70). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x767E, kZVariant, 0x4F70).
unicode_unihan_variant(0x7681, kSemanticVariant, 0x7682). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7681, kZVariant, 0x7682).
unicode_unihan_variant(0x7682, kSemanticVariant, 0x7681). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7682, kZVariant, 0x7681).
unicode_unihan_variant(0x7683, kSemanticVariant, 0x8C8E). %<kMatthews 0x8C8C<kFenn
unicode_unihan_variant(0x7688, kZVariant, 0x6B78).
unicode_unihan_variant(0x768B, kSemanticVariant, 0x81EF). %<kMatthews 0x777E<kMatthews
unicode_unihan_variant(0x768B, kZVariant, 0x7690).
unicode_unihan_variant(0x768E, kSemanticVariant, 0x3FDF). %<kMatthews 0x76A6<kFenn
unicode_unihan_variant(0x7690, kZVariant, 0x768B).
unicode_unihan_variant(0x7691, kTraditionalVariant, 0x769A).
unicode_unihan_variant(0x7693, kSemanticVariant, 0x9865). %<kMatthews 0x769D<kMatthews
unicode_unihan_variant(0x769A, kSimplifiedVariant, 0x7691).
unicode_unihan_variant(0x769C, kSemanticVariant, 0x66A0). %<kMatthews
unicode_unihan_variant(0x769D, kSemanticVariant, 0x7693). %<kMatthews 0x9865<kMatthews
unicode_unihan_variant(0x769E, kSemanticVariant, 0x76A1). %<kMatthews 0x76A5<kMatthews
unicode_unihan_variant(0x769F, kSimplifiedVariant, 0x24F80).
unicode_unihan_variant(0x76A1, kSemanticVariant, 0x769E). %<kMatthews 0x76A5<kMatthews
unicode_unihan_variant(0x76A5, kSemanticVariant, 0x769E). %<kMatthews 0x76A1<kMatthews
unicode_unihan_variant(0x76A6, kSemanticVariant, 0x3FDF). %<kFenn 0x768E). %<kFenn
unicode_unihan_variant(0x76B0, kSemanticVariant, 0x75B1). %<kMatthews
unicode_unihan_variant(0x76B0, kSimplifiedVariant, 0x75B1).
unicode_unihan_variant(0x76B1, kTraditionalVariant, 0x76BA).
unicode_unihan_variant(0x76B2, kTraditionalVariant, 0x76B8).
unicode_unihan_variant(0x76B7, kSemanticVariant, 0x9F14). %<kLau,kMatthews 0x9F13<kLau,kMatthews
unicode_unihan_variant(0x76B7, kZVariant, 0x9F13).
unicode_unihan_variant(0x76B8, kSimplifiedVariant, 0x76B2).
unicode_unihan_variant(0x76BA, kSemanticVariant, 0x7E10). %<kLau
unicode_unihan_variant(0x76BA, kSimplifiedVariant, 0x76B1).
unicode_unihan_variant(0x76BC, kZVariant, 0x9F13).
unicode_unihan_variant(0x76C3, kSemanticVariant, 0x676F). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x76C5, kZVariant, 0x6C96).
unicode_unihan_variant(0x76C7, kSemanticVariant, 0x76CD). %<kMatthews
unicode_unihan_variant(0x76C7, kZVariant, 0x76CD).
unicode_unihan_variant(0x76CA, kSemanticVariant, 0xFA17). %<kMeyerWempe
unicode_unihan_variant(0x76CA, kZVariant, 0xFA17).
unicode_unihan_variant(0x76CC, kSemanticVariant, 0x6900). %<kMatthews,kMeyerWempe 0x7897<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x76CD, kSemanticVariant, 0x76C7). %<kMatthews
unicode_unihan_variant(0x76CD, kZVariant, 0x76C7).
unicode_unihan_variant(0x76CF, kTraditionalVariant, 0x76DE).
unicode_unihan_variant(0x76D0, kTraditionalVariant, 0x9E7D).
unicode_unihan_variant(0x76D1, kTraditionalVariant, 0x76E3).
unicode_unihan_variant(0x76D6, kSemanticVariant, 0x84CB). %<kLau,kMatthews,kMeyerWempe 0x8462<kFenn
unicode_unihan_variant(0x76D6, kTraditionalVariant, 0x84CB).
unicode_unihan_variant(0x76D7, kSemanticVariant, 0x76DC). %<kMatthews
unicode_unihan_variant(0x76D7, kTraditionalVariant, 0x76DC).
unicode_unihan_variant(0x76D8, kTraditionalVariant, 0x76E4).
unicode_unihan_variant(0x76D9, kSemanticVariant, 0x7C20). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x76DC, kSemanticVariant, 0x76D7). %<kMatthews
unicode_unihan_variant(0x76DC, kSimplifiedVariant, 0x76D7).
unicode_unihan_variant(0x76DE, kSimplifiedVariant, 0x76CF).
unicode_unihan_variant(0x76E1, kSemanticVariant, 0x5C3D). %<kLau,kMatthews
unicode_unihan_variant(0x76E1, kSimplifiedVariant, 0x5C3D).
unicode_unihan_variant(0x76E3, kSimplifiedVariant, 0x76D1).
unicode_unihan_variant(0x76E4, kSimplifiedVariant, 0x76D8).
unicode_unihan_variant(0x76E7, kSimplifiedVariant, 0x5362).
unicode_unihan_variant(0x76E9, kSemanticVariant, 0x76ED). %<kMeyerWempe
unicode_unihan_variant(0x76EA, kSemanticVariant, 0x8569). %0x862F
unicode_unihan_variant(0x76EA, kSimplifiedVariant, 0x8361).
unicode_unihan_variant(0x76EA, kSpecializedSemanticVariant, 0x903F). %<kFenn 0x28329<kFenn
unicode_unihan_variant(0x76ED, kSemanticVariant, 0x76E9). %<kMeyerWempe
unicode_unihan_variant(0x7701, kZVariant, 0xF96D).
unicode_unihan_variant(0x7707, kSemanticVariant, 0x7DF2). %<kMeyerWempe
unicode_unihan_variant(0x7708, kSemanticVariant, 0x8EAD). %<kMatthews
unicode_unihan_variant(0x770D, kTraditionalVariant, 0x7798).
unicode_unihan_variant(0x770E, kSemanticVariant, 0x8996). %<kMatthews
unicode_unihan_variant(0x7718, kZVariant, 0x613C).
unicode_unihan_variant(0x7719, kZVariant, 0x77AA).
unicode_unihan_variant(0x771E, kSemanticVariant, 0x771F). %<kLau,kMatthews
unicode_unihan_variant(0x771E, kZVariant, 0x771F).
unicode_unihan_variant(0x771F, kSemanticVariant, 0x771E). %<kLau,kMatthews
unicode_unihan_variant(0x771F, kZVariant, 0x771E).
unicode_unihan_variant(0x7720, kSpecializedSemanticVariant, 0x7DE1). %<kMeyerWempe
unicode_unihan_variant(0x7725, kSemanticVariant, 0x7726). %<kMatthews
unicode_unihan_variant(0x7725, kSimplifiedVariant, 0x7726).
unicode_unihan_variant(0x7726, kSemanticVariant, 0x7725). %<kMatthews
unicode_unihan_variant(0x7726, kTraditionalVariant, 0x7725).
unicode_unihan_variant(0x772C, kTraditionalVariant, 0x77D3).
unicode_unihan_variant(0x773E, kSemanticVariant, 0x8846). %<kLau,kMatthews 0x2009D<kLau
unicode_unihan_variant(0x773E, kSimplifiedVariant, 0x4F17).
unicode_unihan_variant(0x773E, kZVariant, 0x8846).
unicode_unihan_variant(0x7740, kSemanticVariant, 0x8457).
unicode_unihan_variant(0x7740, kSpecializedSemanticVariant, 0x8457). %<kFenn
unicode_unihan_variant(0x7740, kTraditionalVariant, 0x8457).
unicode_unihan_variant(0x7741, kTraditionalVariant, 0x775C).
unicode_unihan_variant(0x774D, kSimplifiedVariant, 0x2AFA2).
unicode_unihan_variant(0x774F, kSemanticVariant, 0x56F0). %<kFenn
unicode_unihan_variant(0x774F, kSimplifiedVariant, 0x56F0).
unicode_unihan_variant(0x7750, kTraditionalVariant, 0x775E).
unicode_unihan_variant(0x7751, kTraditionalVariant, 0x77BC).
unicode_unihan_variant(0x775C, kSimplifiedVariant, 0x7741).
unicode_unihan_variant(0x775E, kSimplifiedVariant, 0x7750).
unicode_unihan_variant(0x776B, kSemanticVariant, 0x4039). %<kMatthews
unicode_unihan_variant(0x7779, kSemanticVariant, 0x89A9). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x777E, kSemanticVariant, 0x768B). %<kMatthews
unicode_unihan_variant(0x777F, kSemanticVariant, 0x53E1). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x777F, kZVariant, 0x53E1).
unicode_unihan_variant(0x7785, kSemanticVariant, 0x77C1). %<kFenn
unicode_unihan_variant(0x7786, kTraditionalVariant, 0x77B6).
unicode_unihan_variant(0x778D, kSemanticVariant, 0x2521F). %<kHanYu,kMeyerWempe
unicode_unihan_variant(0x7792, kTraditionalVariant, 0x779E).
unicode_unihan_variant(0x7798, kSimplifiedVariant, 0x770D).
unicode_unihan_variant(0x779C, kSimplifiedVariant, 0x4056).
unicode_unihan_variant(0x779E, kSimplifiedVariant, 0x7792).
unicode_unihan_variant(0x77A0, kSemanticVariant, 0x77AA). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x77A4, kSimplifiedVariant, 0x251A7).
unicode_unihan_variant(0x77A5, kSemanticVariant, 0x82E4). %<kMatthews
unicode_unihan_variant(0x77A9, kTraditionalVariant, 0x77DA).
unicode_unihan_variant(0x77AA, kSemanticVariant, 0x77A0). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x77AA, kZVariant, 0x7719).
unicode_unihan_variant(0x77AC, kSemanticVariant, 0x4022). %<kMatthews
unicode_unihan_variant(0x77AD, kZVariant, 0x4E86).
unicode_unihan_variant(0x77AF, kSemanticVariant, 0x77B7). %<kMeyerWempe
unicode_unihan_variant(0x77B0, kSemanticVariant, 0x77D9). %<kMatthews
unicode_unihan_variant(0x77B6, kSimplifiedVariant, 0x7786).
unicode_unihan_variant(0x77B7, kSemanticVariant, 0x77AF). %<kMeyerWempe
unicode_unihan_variant(0x77B9, kSemanticVariant, 0x66D6). %<kMeyerWempe
unicode_unihan_variant(0x77BC, kSimplifiedVariant, 0x7751).
unicode_unihan_variant(0x77BF, kSemanticVariant, 0x4020). %<kMatthews
unicode_unihan_variant(0x77C1, kSemanticVariant, 0x7785). %<kFenn
unicode_unihan_variant(0x77C7, kSpecializedSemanticVariant, 0x6726).
unicode_unihan_variant(0x77C7, kZVariant, 0x8499).
unicode_unihan_variant(0x77C8, kZVariant, 0x77CF).
unicode_unihan_variant(0x77CB, kSemanticVariant, 0x77D6). %<kMeyerWempe
unicode_unihan_variant(0x77CF, kZVariant, 0x77C8).
unicode_unihan_variant(0x77D3, kSimplifiedVariant, 0x772C).
unicode_unihan_variant(0x77D3, kSpecializedSemanticVariant, 0x6727).
unicode_unihan_variant(0x77D6, kSemanticVariant, 0x77CB). %<kMeyerWempe
unicode_unihan_variant(0x77D9, kSemanticVariant, 0x77B0). %<kMatthews
unicode_unihan_variant(0x77DA, kSimplifiedVariant, 0x77A9).
unicode_unihan_variant(0x77E2, kZVariant, 0x7B36).
unicode_unihan_variant(0x77E7, kSemanticVariant, 0x8A20). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x77E9, kZVariant, 0x6998).
unicode_unihan_variant(0x77EB, kTraditionalVariant, 0x77EF).
unicode_unihan_variant(0x77EE, kSemanticVariant, 0x8EB7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x77EF, kSimplifiedVariant, 0x77EB).
unicode_unihan_variant(0x77F0, kSpecializedSemanticVariant, 0x8E6D). %<kMeyerWempe
unicode_unihan_variant(0x77F3, kSemanticVariant, 0x25418). %<kHanYu
unicode_unihan_variant(0x77F6, kTraditionalVariant, 0x78EF).
unicode_unihan_variant(0x77FE, kTraditionalVariant, 0x792C).
unicode_unihan_variant(0x77FF, kTraditionalVariant, 0x7926).
unicode_unihan_variant(0x7800, kTraditionalVariant, 0x78AD).
unicode_unihan_variant(0x7801, kTraditionalVariant, 0x78BC).
unicode_unihan_variant(0x7805, kSemanticVariant, 0x6FFF). %<kMeyerWempe
unicode_unihan_variant(0x7807, kSemanticVariant, 0x73C9). %<kFenn
unicode_unihan_variant(0x780C, kSemanticVariant, 0x7E96). %<kLau
unicode_unihan_variant(0x7812, kSemanticVariant, 0x78C7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7814, kSemanticVariant, 0x63C5). %<kMatthews
unicode_unihan_variant(0x7814, kSpecializedSemanticVariant, 0x63C5). %<kMeyerWempe
unicode_unihan_variant(0x7814, kZVariant, 0x784E).
unicode_unihan_variant(0x7815, kZVariant, 0x788E).
unicode_unihan_variant(0x7816, kTraditionalVariant, 0x78DA).
unicode_unihan_variant(0x7817, kTraditionalVariant, 0x7868).
unicode_unihan_variant(0x781A, kTraditionalVariant, 0x786F).
unicode_unihan_variant(0x781C, kTraditionalVariant, 0x78B8).
unicode_unihan_variant(0x7820, kSemanticVariant, 0x5CA8). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7823, kSemanticVariant, 0x254FF). %<kFenn 0x9248<kMatthews
unicode_unihan_variant(0x7825, kSemanticVariant, 0x538E). %<kMatthews
unicode_unihan_variant(0x7826, kSemanticVariant, 0x5BE8). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7827, kSemanticVariant, 0x78AA). %<kLau,kMatthews
unicode_unihan_variant(0x7827, kZVariant, 0x78AA).
unicode_unihan_variant(0x7832, kSemanticVariant, 0x70AE). %<kMeyerWempe 0x792E<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7832, kZVariant, 0x70AE).
unicode_unihan_variant(0x783A, kTraditionalVariant, 0x792A).
unicode_unihan_variant(0x783B, kTraditionalVariant, 0x7931).
unicode_unihan_variant(0x783E, kTraditionalVariant, 0x792B).
unicode_unihan_variant(0x783F, kZVariant, 0x7926).
unicode_unihan_variant(0x7840, kTraditionalVariant, 0x790E).
unicode_unihan_variant(0x7841, kTraditionalVariant, 0x785C).
unicode_unihan_variant(0x784E, kZVariant, 0x7814).
unicode_unihan_variant(0x7855, kTraditionalVariant, 0x78A9).
unicode_unihan_variant(0x7856, kTraditionalVariant, 0x7864).
unicode_unihan_variant(0x7857, kTraditionalVariant, 0x78FD).
unicode_unihan_variant(0x7859, kTraditionalVariant, 0x78D1).
unicode_unihan_variant(0x785A, kTraditionalVariant, 0x7904).
unicode_unihan_variant(0x785A, kZVariant, 0x7904).
unicode_unihan_variant(0x785C, kSemanticVariant, 0x40D8). %<kMatthews 0x8E01<kMeyerWempe
unicode_unihan_variant(0x785C, kSimplifiedVariant, 0x7841).
unicode_unihan_variant(0x7864, kSemanticVariant, 0x550A). %<kMeyerWempe
unicode_unihan_variant(0x7864, kSimplifiedVariant, 0x7856).
unicode_unihan_variant(0x7868, kSimplifiedVariant, 0x7817).
unicode_unihan_variant(0x786B, kZVariant, 0xF9CE).
unicode_unihan_variant(0x786C, kSpecializedSemanticVariant, 0x61C9). %<kFenn
unicode_unihan_variant(0x786C, kZVariant, 0x5CFA).
unicode_unihan_variant(0x786D, kSemanticVariant, 0x2541E). %<kMeyerWempe
unicode_unihan_variant(0x786E, kTraditionalVariant, 0x78BA).
unicode_unihan_variant(0x786F, kSimplifiedVariant, 0x781A).
unicode_unihan_variant(0x7875, kTraditionalVariant, 0x78E0).
unicode_unihan_variant(0x7875, kZVariant, 0x78E0).
unicode_unihan_variant(0x7877, kTraditionalVariant, 0x7906). %0x9E7C
unicode_unihan_variant(0x7877, kZVariant, 0x9E7C).
unicode_unihan_variant(0x7881, kSemanticVariant, 0x68CA). %<kLau,kMatthews 0x68CB<kLau,kMatthews
unicode_unihan_variant(0x788C, kZVariant, 0xF93B).
unicode_unihan_variant(0x788D, kSemanticVariant, 0x3775). %<kMatthews 0x7919<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x788D, kTraditionalVariant, 0x7919).
unicode_unihan_variant(0x788E, kZVariant, 0x7815).
unicode_unihan_variant(0x7894, kSemanticVariant, 0x73F7). %<kMeyerWempe
unicode_unihan_variant(0x7895, kZVariant, 0x5D0E).
unicode_unihan_variant(0x7897, kSemanticVariant, 0x6900). %<kMatthews,kMeyerWempe 0x76CC<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7899, kSimplifiedVariant, 0x2543B).
unicode_unihan_variant(0x789B, kTraditionalVariant, 0x78E7).
unicode_unihan_variant(0x789C, kTraditionalVariant, 0x78E3).
unicode_unihan_variant(0x78A5, kZVariant, 0x6241).
unicode_unihan_variant(0x78A9, kSimplifiedVariant, 0x7855).
unicode_unihan_variant(0x78AA, kSemanticVariant, 0x7827). %<kLau,kMatthews
unicode_unihan_variant(0x78AA, kZVariant, 0x7827).
unicode_unihan_variant(0x78AD, kSimplifiedVariant, 0x7800).
unicode_unihan_variant(0x78AF, kSemanticVariant, 0x7459). %<kMatthews
unicode_unihan_variant(0x78AF, kZVariant, 0x7459).
unicode_unihan_variant(0x78B0, kSemanticVariant, 0x63BD). %<kFenn
unicode_unihan_variant(0x78B1, kSemanticVariant, 0x9E7C). %<kLau 0x9E7B<kLau
unicode_unihan_variant(0x78B8, kSimplifiedVariant, 0x781C).
unicode_unihan_variant(0x78BA, kSimplifiedVariant, 0x786E).
unicode_unihan_variant(0x78BC, kSimplifiedVariant, 0x7801).
unicode_unihan_variant(0x78BD, kSimplifiedVariant, 0x40B5).
unicode_unihan_variant(0x78C1, kSemanticVariant, 0x74F7). %<kMatthews
unicode_unihan_variant(0x78C6, kZVariant, 0x6ED1).
unicode_unihan_variant(0x78C7, kSemanticVariant, 0x7812). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x78CA, kSemanticVariant, 0x78E5). %<kMatthews
unicode_unihan_variant(0x78CA, kZVariant, 0xF947).
unicode_unihan_variant(0x78CC, kSemanticVariant, 0x27C0A). %<kMeyerWempe
unicode_unihan_variant(0x78D1, kSimplifiedVariant, 0x7859).
unicode_unihan_variant(0x78D2, kSemanticVariant, 0x9695). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x78DA, kSemanticVariant, 0x750E). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x78DA, kSimplifiedVariant, 0x7816).
unicode_unihan_variant(0x78DB, kSemanticVariant, 0x5D83). %<kMeyerWempe
unicode_unihan_variant(0x78E0, kSimplifiedVariant, 0x7875).
unicode_unihan_variant(0x78E1, kSemanticVariant, 0x5888). %<kLau,kMatthews
unicode_unihan_variant(0x78E3, kSimplifiedVariant, 0x789C).
unicode_unihan_variant(0x78E5, kSemanticVariant, 0x78CA). %<kMatthews
unicode_unihan_variant(0x78E7, kSimplifiedVariant, 0x789B).
unicode_unihan_variant(0x78EA, kSemanticVariant, 0x5D14). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x78EF, kSimplifiedVariant, 0x77F6).
unicode_unihan_variant(0x78F4, kSemanticVariant, 0x5D9D). %<kMatthews
unicode_unihan_variant(0x78F5, kSemanticVariant, 0x6F97). %<kMatthews
unicode_unihan_variant(0x78FB, kZVariant, 0xF964).
unicode_unihan_variant(0x78FD, kSemanticVariant, 0x589D). %<kMatthews
unicode_unihan_variant(0x78FD, kSimplifiedVariant, 0x7857).
unicode_unihan_variant(0x7904, kSimplifiedVariant, 0x785A).
unicode_unihan_variant(0x7904, kZVariant, 0x785A).
unicode_unihan_variant(0x7906, kSimplifiedVariant, 0x7877).
unicode_unihan_variant(0x7906, kZVariant, 0x96AA).
unicode_unihan_variant(0x790E, kSimplifiedVariant, 0x7840).
unicode_unihan_variant(0x7912, kSimplifiedVariant, 0x2541F).
unicode_unihan_variant(0x7919, kSemanticVariant, 0x3775). %<kMatthews 0x788D<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7919, kSimplifiedVariant, 0x788D).
unicode_unihan_variant(0x791F, kSemanticVariant, 0x792E). %<kLau,kMatthews
unicode_unihan_variant(0x7921, kZVariant, 0x7934).
unicode_unihan_variant(0x7926, kSemanticVariant, 0x945B). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7926, kSimplifiedVariant, 0x77FF).
unicode_unihan_variant(0x792A, kSimplifiedVariant, 0x783A).
unicode_unihan_variant(0x792A, kZVariant, 0xF985).
unicode_unihan_variant(0x792B, kSimplifiedVariant, 0x783E).
unicode_unihan_variant(0x792C, kSimplifiedVariant, 0x77FE).
unicode_unihan_variant(0x792E, kSemanticVariant, 0x70AE). %<kMeyerWempe 0x7832<kLau,kMatthews,kMeyerWempe 0x791F<kLau,kMatthews
unicode_unihan_variant(0x792E, kZVariant, 0x70AE).
unicode_unihan_variant(0x7931, kSemanticVariant, 0x24BA8). %<kMeyerWempe
unicode_unihan_variant(0x7931, kSimplifiedVariant, 0x783B).
unicode_unihan_variant(0x7934, kZVariant, 0x7921).
unicode_unihan_variant(0x7936, kSemanticVariant, 0x7F50). %<kFenn
unicode_unihan_variant(0x793A, kSemanticVariant, 0x793B). %<kMatthews
unicode_unihan_variant(0x793B, kSemanticVariant, 0x793A). %<kMatthews
unicode_unihan_variant(0x793C, kSemanticVariant, 0xFA18). %<kMatthews 0x79AE<kMatthews,kMeyerWempe
unicode_unihan_variant(0x793C, kTraditionalVariant, 0x79AE).
unicode_unihan_variant(0x793C, kZVariant, 0xFA18).
unicode_unihan_variant(0x793F, kSemanticVariant, 0x79B4). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7943, kTraditionalVariant, 0x79A1).
unicode_unihan_variant(0x7947, kSpecializedSemanticVariant, 0x53EA).
unicode_unihan_variant(0x794E, kTraditionalVariant, 0x7995).
unicode_unihan_variant(0x7950, kSemanticVariant, 0x4F51). %<kFenn
unicode_unihan_variant(0x7955, kSemanticVariant, 0x79D8). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7955, kZVariant, 0x79D8).
unicode_unihan_variant(0x7957, kZVariant, 0x53EA).
unicode_unihan_variant(0x7958, kZVariant, 0x7B97).
unicode_unihan_variant(0x795E, kZVariant, 0xFA19).
unicode_unihan_variant(0x7962, kTraditionalVariant, 0x79B0).
unicode_unihan_variant(0x7965, kZVariant, 0xFA1A).
unicode_unihan_variant(0x796F, kTraditionalVariant, 0x798E).
unicode_unihan_variant(0x7977, kTraditionalVariant, 0x79B1).
unicode_unihan_variant(0x7978, kTraditionalVariant, 0x798D).
unicode_unihan_variant(0x797F, kSimplifiedVariant, 0x7984).
unicode_unihan_variant(0x797F, kZVariant, 0xF93C).
unicode_unihan_variant(0x7980, kSemanticVariant, 0x7A1F). %<kMeyerWempe
unicode_unihan_variant(0x7980, kTraditionalVariant, 0x7A1F).
unicode_unihan_variant(0x7982, kSemanticVariant, 0x79B1). %<kMatthews
unicode_unihan_variant(0x7984, kTraditionalVariant, 0x797F).
unicode_unihan_variant(0x7985, kTraditionalVariant, 0x79AA).
unicode_unihan_variant(0x798D, kSimplifiedVariant, 0x7978).
unicode_unihan_variant(0x798E, kSimplifiedVariant, 0x796F).
unicode_unihan_variant(0x798F, kZVariant, 0xFA1B).
unicode_unihan_variant(0x7995, kSimplifiedVariant, 0x794E).
unicode_unihan_variant(0x79A1, kSimplifiedVariant, 0x7943).
unicode_unihan_variant(0x79A5, kZVariant, 0x797A).
unicode_unihan_variant(0x79A6, kSimplifiedVariant, 0x5FA1).
unicode_unihan_variant(0x79AA, kSimplifiedVariant, 0x7985).
unicode_unihan_variant(0x79AE, kSemanticVariant, 0x793C). %<kMatthews,kMeyerWempe 0xFA18<kMatthews
unicode_unihan_variant(0x79AE, kSimplifiedVariant, 0x793C).
unicode_unihan_variant(0x79AE, kZVariant, 0xF9B6).
unicode_unihan_variant(0x79B0, kSimplifiedVariant, 0x7962).
unicode_unihan_variant(0x79B1, kSemanticVariant, 0x7982). %<kMatthews
unicode_unihan_variant(0x79B1, kSimplifiedVariant, 0x7977).
unicode_unihan_variant(0x79B4, kSemanticVariant, 0x793F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x79BB, kSpecializedSemanticVariant, 0x96E2). %<kFenn
unicode_unihan_variant(0x79BB, kTraditionalVariant, 0x96E2).
unicode_unihan_variant(0x79BF, kSemanticVariant, 0x79C3). %<kMatthews
unicode_unihan_variant(0x79BF, kSimplifiedVariant, 0x79C3).
unicode_unihan_variant(0x79C1, kZVariant, 0x53B6).
unicode_unihan_variant(0x79C3, kSemanticVariant, 0x79BF). %<kMatthews
unicode_unihan_variant(0x79C3, kTraditionalVariant, 0x79BF).
unicode_unihan_variant(0x79C4, kSemanticVariant, 0x8014). %<kMeyerWempe
unicode_unihan_variant(0x79C6, kSemanticVariant, 0x7A08). %<kMatthews
unicode_unihan_variant(0x79C6, kTraditionalVariant, 0x7A08).
unicode_unihan_variant(0x79C7, kSemanticVariant, 0x57F6). %<kMatthews 0x84FA<kMatthews 0x85DD<kMatthews
unicode_unihan_variant(0x79C8, kSemanticVariant, 0x7C7C). %<kMatthews
unicode_unihan_variant(0x79C8, kSimplifiedVariant, 0x7C7C).
unicode_unihan_variant(0x79C9, kSpecializedSemanticVariant, 0x62FC). %<kMeyerWempe
unicode_unihan_variant(0x79CA, kZVariant, 0xF995).
unicode_unihan_variant(0x79CB, kSemanticVariant, 0x418B). %<kMatthews 0x79CC<kLau 0x9F9D<kMatthews
unicode_unihan_variant(0x79CC, kSemanticVariant, 0x79CB). %<kLau
unicode_unihan_variant(0x79CD, kTraditionalVariant, 0x7A2E).
unicode_unihan_variant(0x79CF, kSemanticVariant, 0x8017). %<kMatthews
unicode_unihan_variant(0x79CF, kZVariant, 0x8017).
unicode_unihan_variant(0x79D0, kSemanticVariant, 0x8018). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x79D0, kZVariant, 0x8018).
unicode_unihan_variant(0x79D4, kSemanticVariant, 0x7CB3). %<kMatthews,kMeyerWempe 0x7A09<kMatthews,kMeyerWempe
unicode_unihan_variant(0x79D5, kSemanticVariant, 0x7C83). %<kMatthews
unicode_unihan_variant(0x79D8, kSemanticVariant, 0x7955). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x79E4, kSpecializedSemanticVariant, 0x7A31). %<kMeyerWempe
unicode_unihan_variant(0x79EF, kTraditionalVariant, 0x7A4D).
unicode_unihan_variant(0x79F0, kTraditionalVariant, 0x7A31).
unicode_unihan_variant(0x79F8, kSemanticVariant, 0x7A2D). %<kMatthews
unicode_unihan_variant(0x79FB, kSemanticVariant, 0x8FFB). %<kMatthews
unicode_unihan_variant(0x79FD, kTraditionalVariant, 0x7A62).
unicode_unihan_variant(0x79FE, kTraditionalVariant, 0x7A60).
unicode_unihan_variant(0x7A00, kZVariant, 0x5E0C).
unicode_unihan_variant(0x7A05, kSimplifiedVariant, 0x7A0E).
unicode_unihan_variant(0x7A06, kTraditionalVariant, 0x7A6D).
unicode_unihan_variant(0x7A08, kSemanticVariant, 0x79C6). %<kMatthews
unicode_unihan_variant(0x7A08, kSimplifiedVariant, 0x79C6).
unicode_unihan_variant(0x7A09, kSemanticVariant, 0x79D4). %<kMatthews,kMeyerWempe 0x7CB3<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7A0A, kSemanticVariant, 0x413A). %<kMeyerWempe
unicode_unihan_variant(0x7A0E, kTraditionalVariant, 0x7A05).
unicode_unihan_variant(0x7A0F, kSimplifiedVariant, 0x4149).
unicode_unihan_variant(0x7A18, kSemanticVariant, 0x671E). %<kMatthews
unicode_unihan_variant(0x7A1A, kSemanticVariant, 0x454C). %<kMatthews 0x7A49<kMatthews 0x7A3A<kMeyerWempe
unicode_unihan_variant(0x7A1A, kZVariant, 0x7A49).
unicode_unihan_variant(0x7A1C, kSemanticVariant, 0x68F1). %<kMeyerWempe 0x695E<kFenn
unicode_unihan_variant(0x7A1C, kZVariant, 0xF956).
unicode_unihan_variant(0x7A1F, kSemanticVariant, 0x7980). %<kMeyerWempe
unicode_unihan_variant(0x7A1F, kSimplifiedVariant, 0x7980).
unicode_unihan_variant(0x7A1F, kZVariant, 0x5EEA).
unicode_unihan_variant(0x7A23, kTraditionalVariant, 0x7A4C).
unicode_unihan_variant(0x7A2C, kSemanticVariant, 0x7CEF). %<kMatthews
unicode_unihan_variant(0x7A2D, kSemanticVariant, 0x79F8). %<kMatthews
unicode_unihan_variant(0x7A2E, kSimplifiedVariant, 0x79CD).
unicode_unihan_variant(0x7A31, kSemanticVariant, 0x5041). %<kMatthews 0x7A6A<kMatthews
unicode_unihan_variant(0x7A31, kSimplifiedVariant, 0x79F0).
unicode_unihan_variant(0x7A31, kSpecializedSemanticVariant, 0x79E4). %<kMeyerWempe
unicode_unihan_variant(0x7A32, kZVariant, 0x7A3B).
unicode_unihan_variant(0x7A33, kTraditionalVariant, 0x7A69).
unicode_unihan_variant(0x7A3A, kSemanticVariant, 0x7A1A). %<kMeyerWempe
unicode_unihan_variant(0x7A3B, kZVariant, 0x7A32).
unicode_unihan_variant(0x7A3D, kZVariant, 0x4E69).
unicode_unihan_variant(0x7A3E, kSemanticVariant, 0x7A3F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7A3F, kSemanticVariant, 0x69C0). %<kLau 0x7A3E<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7A3F, kZVariant, 0x7A3E).
unicode_unihan_variant(0x7A40, kSemanticVariant, 0x7CD3). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7A40, kSimplifiedVariant, 0x8C37).
unicode_unihan_variant(0x7A42, kZVariant, 0x7A57).
unicode_unihan_variant(0x7A45, kSemanticVariant, 0x7CE0). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7A45, kZVariant, 0x7CE0).
unicode_unihan_variant(0x7A49, kSemanticVariant, 0x7A1A). %<kMatthews
unicode_unihan_variant(0x7A49, kZVariant, 0x7A1A).
unicode_unihan_variant(0x7A4C, kSemanticVariant, 0x7526). %<kLau,kMatthews
unicode_unihan_variant(0x7A4C, kSimplifiedVariant, 0x7A23).
unicode_unihan_variant(0x7A4D, kSimplifiedVariant, 0x79EF).
unicode_unihan_variant(0x7A4E, kSemanticVariant, 0x9834). %<kMatthews
unicode_unihan_variant(0x7A4E, kSimplifiedVariant, 0x9896).
unicode_unihan_variant(0x7A4E, kSpecializedSemanticVariant, 0x7F72). %<kMeyerWempe
unicode_unihan_variant(0x7A50, kZVariant, 0x79CB).
unicode_unihan_variant(0x7A51, kTraditionalVariant, 0x7A61).
unicode_unihan_variant(0x7A57, kZVariant, 0x7A42).
unicode_unihan_variant(0x7A5E, kZVariant, 0x7A6D).
unicode_unihan_variant(0x7A60, kSimplifiedVariant, 0x79FE).
unicode_unihan_variant(0x7A61, kSimplifiedVariant, 0x7A51).
unicode_unihan_variant(0x7A62, kSimplifiedVariant, 0x79FD).
unicode_unihan_variant(0x7A63, kZVariant, 0x7A70).
unicode_unihan_variant(0x7A64, kSemanticVariant, 0x7CEF). %<kFenn
unicode_unihan_variant(0x7A69, kSemanticVariant, 0x349A). %<kMatthews
unicode_unihan_variant(0x7A69, kSimplifiedVariant, 0x7A33).
unicode_unihan_variant(0x7A69, kSpecializedSemanticVariant, 0x6587). %<kFenn
unicode_unihan_variant(0x7A6A, kSemanticVariant, 0x5041). %<kMatthews 0x7A31<kMatthews
unicode_unihan_variant(0x7A6B, kSemanticVariant, 0x6AB4). %<kMeyerWempe
unicode_unihan_variant(0x7A6B, kSimplifiedVariant, 0x83B7).
unicode_unihan_variant(0x7A6D, kSimplifiedVariant, 0x7A06).
unicode_unihan_variant(0x7A6D, kZVariant, 0x7A5E).
unicode_unihan_variant(0x7A70, kZVariant, 0x7A63).
unicode_unihan_variant(0x7A77, kTraditionalVariant, 0x7AAE).
unicode_unihan_variant(0x7A7D, kSemanticVariant, 0x9631). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7A7D, kZVariant, 0x9631).
unicode_unihan_variant(0x7A83, kSemanticVariant, 0x7ACA). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7A83, kTraditionalVariant, 0x7ACA).
unicode_unihan_variant(0x7A8C, kSemanticVariant, 0x7A96). %<kMatthews
unicode_unihan_variant(0x7A8D, kTraditionalVariant, 0x7AC5).
unicode_unihan_variant(0x7A8E, kTraditionalVariant, 0x7AB5).
unicode_unihan_variant(0x7A91, kTraditionalVariant, 0x7AAF).
unicode_unihan_variant(0x7A93, kSemanticVariant, 0x41AB). %<kMeyerWempe 0x7255<kLau,kMatthews 0x7A97<kLau,kMatthews,kMeyerWempe 0x7ABB<kFenn
unicode_unihan_variant(0x7A95, kSpecializedSemanticVariant, 0x963F). %<kMeyerWempe
unicode_unihan_variant(0x7A96, kSemanticVariant, 0x7A8C). %<kMatthews
unicode_unihan_variant(0x7A97, kSemanticVariant, 0x41AB). %<kMeyerWempe 0x7255<kFenn 0x7A93<kLau,kMatthews,kMeyerWempe 0x7ABB<kFenn
unicode_unihan_variant(0x7A97, kZVariant, 0x7A93).
unicode_unihan_variant(0x7A9C, kTraditionalVariant, 0x7AC4).
unicode_unihan_variant(0x7A9D, kTraditionalVariant, 0x7AA9).
unicode_unihan_variant(0x7AA5, kTraditionalVariant, 0x7ABA).
unicode_unihan_variant(0x7AA6, kTraditionalVariant, 0x7AC7).
unicode_unihan_variant(0x7AA9, kSimplifiedVariant, 0x7A9D).
unicode_unihan_variant(0x7AAA, kSimplifiedVariant, 0x6D3C).
unicode_unihan_variant(0x7AAD, kTraditionalVariant, 0x7AB6).
unicode_unihan_variant(0x7AAE, kSemanticVariant, 0x7AC6). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7AAE, kSimplifiedVariant, 0x7A77).
unicode_unihan_variant(0x7AAF, kSemanticVariant, 0x7AB0). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7AAF, kSimplifiedVariant, 0x7A91).
unicode_unihan_variant(0x7AAF, kZVariant, 0x7AB0).
unicode_unihan_variant(0x7AB0, kSemanticVariant, 0x7AAF). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7AB0, kZVariant, 0x7AAF).
unicode_unihan_variant(0x7AB4, kSemanticVariant, 0x5861). %<kMatthews
unicode_unihan_variant(0x7AB5, kSimplifiedVariant, 0x7A8E).
unicode_unihan_variant(0x7AB6, kSemanticVariant, 0x5BE0). %<kMatthews,kMeyerWempe,kPhonetic
unicode_unihan_variant(0x7AB6, kSimplifiedVariant, 0x7AAD).
unicode_unihan_variant(0x7AB6, kSpecializedSemanticVariant, 0x5BE0). %<kMeyerWempe
unicode_unihan_variant(0x7ABA, kSimplifiedVariant, 0x7AA5).
unicode_unihan_variant(0x7ABB, kSemanticVariant, 0x7255). %<kFenn 0x7A93<kFenn 0x7A97<kFenn
unicode_unihan_variant(0x7AC3, kZVariant, 0x7076).
unicode_unihan_variant(0x7AC4, kSimplifiedVariant, 0x7A9C).
unicode_unihan_variant(0x7AC5, kSimplifiedVariant, 0x7A8D).
unicode_unihan_variant(0x7AC6, kSemanticVariant, 0x7AAE). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7AC7, kSimplifiedVariant, 0x7AA6).
unicode_unihan_variant(0x7AC8, kSemanticVariant, 0x7076). %<kHKGlyph,kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7AC9, kSemanticVariant, 0x4194). %<kMeyerWempe 0x25945<kMeyerWempe
unicode_unihan_variant(0x7ACA, kSemanticVariant, 0x7A83). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7ACA, kSimplifiedVariant, 0x7A83).
unicode_unihan_variant(0x7ACB, kZVariant, 0xF9F7).
unicode_unihan_variant(0x7AD2, kSemanticVariant, 0x5947). %<kLau,kMatthews
unicode_unihan_variant(0x7AD2, kZVariant, 0x5947).
unicode_unihan_variant(0x7AD6, kTraditionalVariant, 0x8C4E).
unicode_unihan_variant(0x7AD6, kZVariant, 0x8C4E).
unicode_unihan_variant(0x7ADA, kSemanticVariant, 0x4F47). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7ADC, kSpecializedSemanticVariant, 0x9F8D).
unicode_unihan_variant(0x7ADC, kZVariant, 0x9F8D).
unicode_unihan_variant(0x7ADD, kSemanticVariant, 0x4E26). %<kMatthews 0x5E76<kMatthews 0x5E77<kMatthews
unicode_unihan_variant(0x7ADD, kZVariant, 0x4E26).
unicode_unihan_variant(0x7ADE, kTraditionalVariant, 0x7AF6).
unicode_unihan_variant(0x7AE2, kSemanticVariant, 0x4FDF). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7AE2, kZVariant, 0x4FDF).
unicode_unihan_variant(0x7AEA, kSemanticVariant, 0x8C4E). %<kLau,kMatthews
unicode_unihan_variant(0x7AEA, kZVariant, 0x8C4E).
unicode_unihan_variant(0x7AF6, kSemanticVariant, 0x8AA9). %<kMatthews
unicode_unihan_variant(0x7AF6, kSimplifiedVariant, 0x7ADE).
unicode_unihan_variant(0x7AF6, kZVariant, 0x7AF8).
unicode_unihan_variant(0x7AF8, kZVariant, 0x7AF6).
unicode_unihan_variant(0x7AFB, kSemanticVariant, 0x7C15). %<kMeyerWempe 0x25D5A<kMeyerWempe
unicode_unihan_variant(0x7AFE, kZVariant, 0x7BEA).
unicode_unihan_variant(0x7AFF, kSemanticVariant, 0x2353C). %<kLau
unicode_unihan_variant(0x7B03, kTraditionalVariant, 0x7BE4).
unicode_unihan_variant(0x7B06, kZVariant, 0x5DF4).
unicode_unihan_variant(0x7B0B, kSemanticVariant, 0x7B4D). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7B0B, kTraditionalVariant, 0x7B4D).
unicode_unihan_variant(0x7B11, kSemanticVariant, 0x54B2). %<kFenn
unicode_unihan_variant(0x7B11, kZVariant, 0x54B2).
unicode_unihan_variant(0x7B13, kSemanticVariant, 0x7BE6). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7B14, kSemanticVariant, 0x7B46). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7B14, kTraditionalVariant, 0x7B46).
unicode_unihan_variant(0x7B15, kTraditionalVariant, 0x7B67).
unicode_unihan_variant(0x7B1B, kSemanticVariant, 0x7BF4). %<kFenn
unicode_unihan_variant(0x7B20, kZVariant, 0xF9F8).
unicode_unihan_variant(0x7B28, kSemanticVariant, 0x3912). %<kFenn
unicode_unihan_variant(0x7B3A, kTraditionalVariant, 0x7B8B).
unicode_unihan_variant(0x7B3B, kZVariant, 0x7B47).
unicode_unihan_variant(0x7B3C, kTraditionalVariant, 0x7C60).
unicode_unihan_variant(0x7B3E, kTraditionalVariant, 0x7C69).
unicode_unihan_variant(0x7B46, kSemanticVariant, 0x7B14). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7B46, kSimplifiedVariant, 0x7B14).
unicode_unihan_variant(0x7B47, kZVariant, 0x7B3B).
unicode_unihan_variant(0x7B49, kZVariant, 0x6729).
unicode_unihan_variant(0x7B4B, kZVariant, 0x89D4).
unicode_unihan_variant(0x7B4D, kSemanticVariant, 0x7B0B). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7B4D, kSimplifiedVariant, 0x7B0B).
unicode_unihan_variant(0x7B4F, kSemanticVariant, 0x6830). %<kMatthews
unicode_unihan_variant(0x7B50, kZVariant, 0x7B7A).
unicode_unihan_variant(0x7B51, kTraditionalVariant, 0x7BC9).
unicode_unihan_variant(0x7B54, kSemanticVariant, 0x8345). %<kLau,kMeyerWempe
unicode_unihan_variant(0x7B54, kZVariant, 0x8345).
unicode_unihan_variant(0x7B56, kZVariant, 0x7B5E).
unicode_unihan_variant(0x7B5A, kTraditionalVariant, 0x7BF3).
unicode_unihan_variant(0x7B5B, kTraditionalVariant, 0x7BE9).
unicode_unihan_variant(0x7B5C, kTraditionalVariant, 0x7C39).
unicode_unihan_variant(0x7B5D, kTraditionalVariant, 0x7B8F).
unicode_unihan_variant(0x7B5E, kZVariant, 0x7B56).
unicode_unihan_variant(0x7B66, kSemanticVariant, 0x7BA1). %<kMatthews
unicode_unihan_variant(0x7B67, kSimplifiedVariant, 0x7B15).
unicode_unihan_variant(0x7B6D, kSemanticVariant, 0x7B97). %<kMatthews
unicode_unihan_variant(0x7B6F, kSemanticVariant, 0x7BB8). %<kMatthews
unicode_unihan_variant(0x7B71, kSemanticVariant, 0x7BE0). %<kMatthews
unicode_unihan_variant(0x7B71, kZVariant, 0x7BE0).
unicode_unihan_variant(0x7B74, kSimplifiedVariant, 0x41F2).
unicode_unihan_variant(0x7B79, kTraditionalVariant, 0x7C4C).
unicode_unihan_variant(0x7B7C, kTraditionalVariant, 0x7BD4).
unicode_unihan_variant(0x7B7E, kTraditionalVariant, 0x7C3D).
unicode_unihan_variant(0x7B7E, kZVariant, 0x7C64).
unicode_unihan_variant(0x7B7F, kZVariant, 0x7BE0).
unicode_unihan_variant(0x7B80, kTraditionalVariant, 0x7C21).
unicode_unihan_variant(0x7B87, kSemanticVariant, 0x4E2A). %<kLau,kMatthews,kMeyerWempe 0x500B<kLau,kMatthews
unicode_unihan_variant(0x7B87, kSpecializedSemanticVariant, 0x500B). %<kMeyerWempe
unicode_unihan_variant(0x7B87, kZVariant, 0x500B).
unicode_unihan_variant(0x7B8B, kSemanticVariant, 0x724B). %<kLau,kMatthews
unicode_unihan_variant(0x7B8B, kSimplifiedVariant, 0x7B3A).
unicode_unihan_variant(0x7B8B, kZVariant, 0x724B).
unicode_unihan_variant(0x7B8D, kSemanticVariant, 0x7B9B). %<kFenn
unicode_unihan_variant(0x7B8E, kSemanticVariant, 0x7BEA). %<kMatthews
unicode_unihan_variant(0x7B8F, kSimplifiedVariant, 0x7B5D).
unicode_unihan_variant(0x7B92, kSemanticVariant, 0x5E1A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7B92, kSpecializedSemanticVariant, 0x5E1A). %<kFenn
unicode_unihan_variant(0x7B93, kTraditionalVariant, 0x7C59).
unicode_unihan_variant(0x7B94, kSpecializedSemanticVariant, 0x6CE1). %<kMeyerWempe
unicode_unihan_variant(0x7B96, kSemanticVariant, 0x7C43). %<kMeyerWempe
unicode_unihan_variant(0x7B97, kSemanticVariant, 0x7B6D). %<kMatthews
unicode_unihan_variant(0x7B97, kZVariant, 0x7958).
unicode_unihan_variant(0x7B9A, kZVariant, 0x5273).
unicode_unihan_variant(0x7B9B, kSemanticVariant, 0x7B8D). %<kFenn
unicode_unihan_variant(0x7B9D, kZVariant, 0x94B3).
unicode_unihan_variant(0x7BA0, kSemanticVariant, 0x6376). %<kMeyerWempe
unicode_unihan_variant(0x7BA1, kSemanticVariant, 0x7B66). %<kMatthews
unicode_unihan_variant(0x7BA6, kTraditionalVariant, 0x7C00).
unicode_unihan_variant(0x7BA7, kTraditionalVariant, 0x7BCB).
unicode_unihan_variant(0x7BA8, kTraditionalVariant, 0x7C5C).
unicode_unihan_variant(0x7BA9, kTraditionalVariant, 0x7C6E).
unicode_unihan_variant(0x7BAA, kTraditionalVariant, 0x7C1E).
unicode_unihan_variant(0x7BAB, kTraditionalVariant, 0x7C2B).
unicode_unihan_variant(0x7BAC, kZVariant, 0x7BDB).
unicode_unihan_variant(0x7BB3, kZVariant, 0x7C08).
unicode_unihan_variant(0x7BB4, kSemanticVariant, 0x91DD). %<kMeyerWempe 0x937C<kMeyerWempe
unicode_unihan_variant(0x7BB8, kSemanticVariant, 0x7B6F). %<kMatthews
unicode_unihan_variant(0x7BBE, kSemanticVariant, 0x7C2B). %<kMatthews
unicode_unihan_variant(0x7BC0, kSimplifiedVariant, 0x8282).
unicode_unihan_variant(0x7BC0, kSpecializedSemanticVariant, 0x5369). %<kFenn
unicode_unihan_variant(0x7BC0, kZVariant, 0x7F47).
unicode_unihan_variant(0x7BC4, kSimplifiedVariant, 0x8303).
unicode_unihan_variant(0x7BC9, kSimplifiedVariant, 0x7B51).
unicode_unihan_variant(0x7BCB, kSemanticVariant, 0x5327). %<kMatthews
unicode_unihan_variant(0x7BCB, kSimplifiedVariant, 0x7BA7).
unicode_unihan_variant(0x7BD1, kTraditionalVariant, 0x7C23).
unicode_unihan_variant(0x7BD3, kTraditionalVariant, 0x7C0D).
unicode_unihan_variant(0x7BD4, kSimplifiedVariant, 0x7B7C).
unicode_unihan_variant(0x7BD8, kSimplifiedVariant, 0x25B20).
unicode_unihan_variant(0x7BDB, kZVariant, 0x7BAC).
unicode_unihan_variant(0x7BE0, kSemanticVariant, 0x7B71). %<kMatthews
unicode_unihan_variant(0x7BE0, kZVariant, 0x7B7F).
unicode_unihan_variant(0x7BE1, kSemanticVariant, 0x7C12). %<kMatthews
unicode_unihan_variant(0x7BE1, kZVariant, 0x7C12).
unicode_unihan_variant(0x7BE2, kSemanticVariant, 0x69D3). %<kMeyerWempe
unicode_unihan_variant(0x7BE4, kSimplifiedVariant, 0x7B03).
unicode_unihan_variant(0x7BE6, kSemanticVariant, 0x7B13). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7BE9, kSemanticVariant, 0x7C1B). %<kMatthews 0x7C01<kMatthews
unicode_unihan_variant(0x7BE9, kSimplifiedVariant, 0x7B5B).
unicode_unihan_variant(0x7BEA, kSemanticVariant, 0x7B8E). %<kMatthews
unicode_unihan_variant(0x7BEA, kZVariant, 0x7AFE).
unicode_unihan_variant(0x7BED, kZVariant, 0x7C60).
unicode_unihan_variant(0x7BEE, kTraditionalVariant, 0x7C43).
unicode_unihan_variant(0x7BEF, kTraditionalVariant, 0x7C5B).
unicode_unihan_variant(0x7BEF, kZVariant, 0x7C5B).
unicode_unihan_variant(0x7BF1, kTraditionalVariant, 0x7C6C).
unicode_unihan_variant(0x7BF2, kZVariant, 0x5F57).
unicode_unihan_variant(0x7BF3, kSimplifiedVariant, 0x7B5A).
unicode_unihan_variant(0x7BF4, kSemanticVariant, 0x7B1B). %<kFenn
unicode_unihan_variant(0x7BFA, kSemanticVariant, 0x68D1). %<kMeyerWempe
unicode_unihan_variant(0x7C00, kSimplifiedVariant, 0x7BA6).
unicode_unihan_variant(0x7C01, kSemanticVariant, 0x7BE9). %<kMatthews 0x7C1B<kMatthews
unicode_unihan_variant(0x7C08, kZVariant, 0x7BB3).
unicode_unihan_variant(0x7C0C, kSemanticVariant, 0x96DD). %<kCowles
unicode_unihan_variant(0x7C0D, kSimplifiedVariant, 0x7BD3).
unicode_unihan_variant(0x7C11, kZVariant, 0x84D1).
unicode_unihan_variant(0x7C12, kSemanticVariant, 0x7BE1). %<kMatthews
unicode_unihan_variant(0x7C14, kZVariant, 0x7C11).
unicode_unihan_variant(0x7C15, kSemanticVariant, 0x7AFB). %<kMeyerWempe
unicode_unihan_variant(0x7C16, kTraditionalVariant, 0x7C6A).
unicode_unihan_variant(0x7C18, kZVariant, 0x7C2B).
unicode_unihan_variant(0x7C1B, kSemanticVariant, 0x7BE9). %<kMatthews 0x7C01<kMatthews
unicode_unihan_variant(0x7C1E, kSimplifiedVariant, 0x7BAA).
unicode_unihan_variant(0x7C20, kSemanticVariant, 0x76D9). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7C21, kSemanticVariant, 0x8015). %<kLau
unicode_unihan_variant(0x7C21, kSimplifiedVariant, 0x7B80).
unicode_unihan_variant(0x7C23, kSemanticVariant, 0x8562). %<kMeyerWempe 0x7C44<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7C23, kSimplifiedVariant, 0x7BD1).
unicode_unihan_variant(0x7C28, kSemanticVariant, 0x6812). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7C2B, kSemanticVariant, 0x71D2). %<kLau 0x7BBE<kMatthews
unicode_unihan_variant(0x7C2B, kSimplifiedVariant, 0x7BAB).
unicode_unihan_variant(0x7C37, kSemanticVariant, 0x6A90). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7C39, kSimplifiedVariant, 0x7B5C).
unicode_unihan_variant(0x7C3B, kSemanticVariant, 0x6A9B). %<kMeyerWempe
unicode_unihan_variant(0x7C3D, kSimplifiedVariant, 0x7B7E).
unicode_unihan_variant(0x7C3E, kSemanticVariant, 0x22156). %<kMeyerWempe 0x2214F<kLau
unicode_unihan_variant(0x7C3E, kSimplifiedVariant, 0x5E18).
unicode_unihan_variant(0x7C3E, kZVariant, 0xF9A6).
unicode_unihan_variant(0x7C41, kTraditionalVariant, 0x7C5F).
unicode_unihan_variant(0x7C43, kSemanticVariant, 0x7B96). %<kMeyerWempe
unicode_unihan_variant(0x7C43, kSimplifiedVariant, 0x7BEE).
unicode_unihan_variant(0x7C44, kSemanticVariant, 0x7C23). %<kMatthews,kMeyerWempe 0x8562<kMeyerWempe
unicode_unihan_variant(0x7C4B, kSimplifiedVariant, 0x25B1E).
unicode_unihan_variant(0x7C4C, kSimplifiedVariant, 0x7B79).
unicode_unihan_variant(0x7C50, kSemanticVariant, 0x85E4). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7C50, kZVariant, 0x7C58).
unicode_unihan_variant(0x7C51, kSemanticVariant, 0x4275). %<kMatthews 0x994C<kMatthews
unicode_unihan_variant(0x7C54, kSimplifiedVariant, 0x4264).
unicode_unihan_variant(0x7C54, kZVariant, 0x85EA).
unicode_unihan_variant(0x7C56, kZVariant, 0x7C64).
unicode_unihan_variant(0x7C58, kZVariant, 0x7C50).
unicode_unihan_variant(0x7C59, kSimplifiedVariant, 0x7B93).
unicode_unihan_variant(0x7C5B, kSimplifiedVariant, 0x7BEF).
unicode_unihan_variant(0x7C5C, kSimplifiedVariant, 0x7BA8).
unicode_unihan_variant(0x7C5F, kSimplifiedVariant, 0x7C41).
unicode_unihan_variant(0x7C60, kSimplifiedVariant, 0x7B3C).
unicode_unihan_variant(0x7C60, kZVariant, 0xF944).
unicode_unihan_variant(0x7C64, kZVariant, 0x7C56).
unicode_unihan_variant(0x7C65, kSemanticVariant, 0x9FA0).
unicode_unihan_variant(0x7C69, kSimplifiedVariant, 0x7B3E).
unicode_unihan_variant(0x7C6A, kSimplifiedVariant, 0x7C16).
unicode_unihan_variant(0x7C6C, kSimplifiedVariant, 0x7BF1).
unicode_unihan_variant(0x7C6E, kSimplifiedVariant, 0x7BA9).
unicode_unihan_variant(0x7C72, kSemanticVariant, 0x9FA5). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7C74, kTraditionalVariant, 0x7CF4).
unicode_unihan_variant(0x7C7B, kTraditionalVariant, 0x985E).
unicode_unihan_variant(0x7C7C, kSemanticVariant, 0x79C8). %<kMatthews
unicode_unihan_variant(0x7C7C, kTraditionalVariant, 0x79C8).
unicode_unihan_variant(0x7C83, kSemanticVariant, 0x79D5). %<kMatthews
unicode_unihan_variant(0x7C8B, kZVariant, 0x7CB9).
unicode_unihan_variant(0x7C92, kZVariant, 0xF9F9).
unicode_unihan_variant(0x7C97, kSemanticVariant, 0x9EA4). %<kMatthews 0x9E84<kMatthews 0x89D5<kMatthews
unicode_unihan_variant(0x7C97, kZVariant, 0x9E84).
unicode_unihan_variant(0x7C98, kSemanticVariant, 0x9ECF). %<kLau
unicode_unihan_variant(0x7C98, kSpecializedSemanticVariant, 0x9ECF). %<kMeyerWempe
unicode_unihan_variant(0x7C9B, kZVariant, 0x8085).
unicode_unihan_variant(0x7C9C, kTraditionalVariant, 0x7CF6).
unicode_unihan_variant(0x7C9D, kTraditionalVariant, 0x7CF2).
unicode_unihan_variant(0x7CA4, kSemanticVariant, 0x7CB5). %<kMatthews
unicode_unihan_variant(0x7CA4, kTraditionalVariant, 0x7CB5).
unicode_unihan_variant(0x7CA5, kZVariant, 0x9B3B).
unicode_unihan_variant(0x7CA6, kSemanticVariant, 0x3DE0). %<kMatthews 0x71D0<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7CA7, kSemanticVariant, 0x599D). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7CAA, kTraditionalVariant, 0x7CDE).
unicode_unihan_variant(0x7CAB, kZVariant, 0x7CEF).
unicode_unihan_variant(0x7CAE, kSemanticVariant, 0x7CE7). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7CAE, kTraditionalVariant, 0x7CE7).
unicode_unihan_variant(0x7CB3, kSemanticVariant, 0x79D4). %<kMatthews,kMeyerWempe 0x7A09<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7CB5, kSemanticVariant, 0x7CA4). %<kMatthews
unicode_unihan_variant(0x7CB5, kSimplifiedVariant, 0x7CA4).
unicode_unihan_variant(0x7CB9, kZVariant, 0x7C8B).
unicode_unihan_variant(0x7CBD, kSemanticVariant, 0x7CC9). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x7CBD, kZVariant, 0x7CED).
unicode_unihan_variant(0x7CBE, kZVariant, 0xFA1D).
unicode_unihan_variant(0x7CC1, kTraditionalVariant, 0x7CDD).
unicode_unihan_variant(0x7CC7, kSemanticVariant, 0x9931). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7CC7, kTraditionalVariant, 0x9931).
unicode_unihan_variant(0x7CC9, kSemanticVariant, 0x7CBD). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x7CCA, kZVariant, 0x80E1).
unicode_unihan_variant(0x7CD3, kSemanticVariant, 0x7A40). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7CD5, kSemanticVariant, 0x993B). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7CD6, kSemanticVariant, 0x9939). %<kMatthews
unicode_unihan_variant(0x7CD6, kZVariant, 0xFA03).
unicode_unihan_variant(0x7CD9, kSemanticVariant, 0x3FF7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7CDD, kSimplifiedVariant, 0x7CC1).
unicode_unihan_variant(0x7CDE, kSimplifiedVariant, 0x7CAA).
unicode_unihan_variant(0x7CDF, kSemanticVariant, 0x91A9). %<kMatthews
unicode_unihan_variant(0x7CE0, kSemanticVariant, 0x7A45). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7CE0, kZVariant, 0x7A45).
unicode_unihan_variant(0x7CE1, kSemanticVariant, 0x7CE8). %<kMatthews
unicode_unihan_variant(0x7CE4, kSemanticVariant, 0x994A). %<kMeyerWempe
unicode_unihan_variant(0x7CE4, kZVariant, 0x9993).
unicode_unihan_variant(0x7CE7, kSemanticVariant, 0x7CAE). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7CE7, kSimplifiedVariant, 0x7CAE).
unicode_unihan_variant(0x7CE8, kSemanticVariant, 0x7CE1). %<kMatthews
unicode_unihan_variant(0x7CED, kZVariant, 0x7CC9).
unicode_unihan_variant(0x7CEF, kSemanticVariant, 0x7A2C). %<kMatthews 0x7A64<kFenn
unicode_unihan_variant(0x7CEF, kZVariant, 0x7CAB).
unicode_unihan_variant(0x7CF0, kSemanticVariant, 0x429C). %<kFenn
unicode_unihan_variant(0x7CF2, kSimplifiedVariant, 0x7C9D).
unicode_unihan_variant(0x7CF4, kSimplifiedVariant, 0x7C74).
unicode_unihan_variant(0x7CF6, kSemanticVariant, 0x689D). %<kLau
unicode_unihan_variant(0x7CF6, kSimplifiedVariant, 0x7C9C).
unicode_unihan_variant(0x7CF8, kSemanticVariant, 0x7CF9). %<kMatthews
unicode_unihan_variant(0x7CF8, kZVariant, 0x7D72).
unicode_unihan_variant(0x7CF9, kSemanticVariant, 0x7CF8). %<kMatthews
unicode_unihan_variant(0x7CF9, kSimplifiedVariant, 0x7E9F).
unicode_unihan_variant(0x7CFA, kSemanticVariant, 0x7CFE). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7CFA, kZVariant, 0x7CFE).
unicode_unihan_variant(0x7CFB, kTraditionalVariant, 0x4FC2). %0x7E6B
unicode_unihan_variant(0x7CFB, kZVariant, 0x7E6B).
unicode_unihan_variant(0x7CFE, kSemanticVariant, 0x7CFA). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7CFE, kSimplifiedVariant, 0x7EA0).
unicode_unihan_variant(0x7CFE, kZVariant, 0x7CFA).
unicode_unihan_variant(0x7D00, kSimplifiedVariant, 0x7EAA).
unicode_unihan_variant(0x7D02, kSimplifiedVariant, 0x7EA3).
unicode_unihan_variant(0x7D04, kSimplifiedVariant, 0x7EA6).
unicode_unihan_variant(0x7D05, kSimplifiedVariant, 0x7EA2).
unicode_unihan_variant(0x7D06, kSimplifiedVariant, 0x7EA1).
unicode_unihan_variant(0x7D07, kSimplifiedVariant, 0x7EA5).
unicode_unihan_variant(0x7D08, kSimplifiedVariant, 0x7EA8).
unicode_unihan_variant(0x7D09, kSimplifiedVariant, 0x7EAB).
unicode_unihan_variant(0x7D0B, kSimplifiedVariant, 0x7EB9).
unicode_unihan_variant(0x7D0D, kSimplifiedVariant, 0x7EB3).
unicode_unihan_variant(0x7D10, kSimplifiedVariant, 0x7EBD).
unicode_unihan_variant(0x7D10, kZVariant, 0xF9CF).
unicode_unihan_variant(0x7D13, kSimplifiedVariant, 0x7EBE).
unicode_unihan_variant(0x7D14, kSimplifiedVariant, 0x7EAF).
unicode_unihan_variant(0x7D15, kSimplifiedVariant, 0x7EB0).
unicode_unihan_variant(0x7D16, kSimplifiedVariant, 0x7EBC).
unicode_unihan_variant(0x7D17, kSimplifiedVariant, 0x7EB1).
unicode_unihan_variant(0x7D18, kSimplifiedVariant, 0x7EAE).
unicode_unihan_variant(0x7D19, kSemanticVariant, 0x5E0B). %<kMatthews
unicode_unihan_variant(0x7D19, kSimplifiedVariant, 0x7EB8).
unicode_unihan_variant(0x7D1A, kSemanticVariant, 0x28E1A). %<kFenn
unicode_unihan_variant(0x7D1A, kSimplifiedVariant, 0x7EA7).
unicode_unihan_variant(0x7D1B, kSimplifiedVariant, 0x7EB7).
unicode_unihan_variant(0x7D1C, kSimplifiedVariant, 0x7EAD).
unicode_unihan_variant(0x7D1D, kSimplifiedVariant, 0x7EB4).
unicode_unihan_variant(0x7D1F, kSemanticVariant, 0x887F). %<kFenn
unicode_unihan_variant(0x7D21, kSimplifiedVariant, 0x7EBA).
unicode_unihan_variant(0x7D22, kZVariant, 0xF96A).
unicode_unihan_variant(0x7D25, kSemanticVariant, 0x7D2E).
unicode_unihan_variant(0x7D25, kZVariant, 0x7D2E).
unicode_unihan_variant(0x7D27, kTraditionalVariant, 0x7DCA).
unicode_unihan_variant(0x7D2C, kSemanticVariant, 0x7DA2). %<kFenn
unicode_unihan_variant(0x7D2C, kSimplifiedVariant, 0x4337).
unicode_unihan_variant(0x7D2C, kZVariant, 0x7DA2).
unicode_unihan_variant(0x7D2E, kSemanticVariant, 0x624E). %<kMeyerWempe 0x7D25<kFenn
unicode_unihan_variant(0x7D2E, kZVariant, 0x624E).
unicode_unihan_variant(0x7D2F, kTraditionalVariant, 0x7E8D).
unicode_unihan_variant(0x7D30, kSimplifiedVariant, 0x7EC6).
unicode_unihan_variant(0x7D31, kSimplifiedVariant, 0x7EC2).
unicode_unihan_variant(0x7D32, kSemanticVariant, 0x7DE4). %<kMeyerWempe
unicode_unihan_variant(0x7D32, kSimplifiedVariant, 0x7EC1).
unicode_unihan_variant(0x7D33, kSimplifiedVariant, 0x7EC5).
unicode_unihan_variant(0x7D35, kSimplifiedVariant, 0x7EBB).
unicode_unihan_variant(0x7D39, kSemanticVariant, 0x4F4B). %<kMatthews
unicode_unihan_variant(0x7D39, kSimplifiedVariant, 0x7ECD).
unicode_unihan_variant(0x7D3A, kSimplifiedVariant, 0x7EC0).
unicode_unihan_variant(0x7D3C, kSimplifiedVariant, 0x7ECB).
unicode_unihan_variant(0x7D3F, kSimplifiedVariant, 0x7ED0).
unicode_unihan_variant(0x7D40, kSimplifiedVariant, 0x7ECC).
unicode_unihan_variant(0x7D42, kSimplifiedVariant, 0x7EC8).
unicode_unihan_variant(0x7D43, kSemanticVariant, 0x5F26). %<kFenn
unicode_unihan_variant(0x7D43, kZVariant, 0x5F26).
unicode_unihan_variant(0x7D44, kSimplifiedVariant, 0x7EC4).
unicode_unihan_variant(0x7D45, kSemanticVariant, 0x8927). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7D45, kSimplifiedVariant, 0x4339).
unicode_unihan_variant(0x7D46, kSimplifiedVariant, 0x7ECA).
unicode_unihan_variant(0x7D4B, kZVariant, 0x7E8A).
unicode_unihan_variant(0x7D4C, kZVariant, 0x7D93).
unicode_unihan_variant(0x7D4E, kSimplifiedVariant, 0x7ED7).
unicode_unihan_variant(0x7D4F, kZVariant, 0x7D32).
unicode_unihan_variant(0x7D50, kSimplifiedVariant, 0x7ED3).
unicode_unihan_variant(0x7D50, kSpecializedSemanticVariant, 0x63ED). %<kFenn
unicode_unihan_variant(0x7D55, kSimplifiedVariant, 0x7EDD).
unicode_unihan_variant(0x7D56, kSemanticVariant, 0x7E8A). %<kMatthews
unicode_unihan_variant(0x7D56, kSpecializedSemanticVariant, 0x7E8A). %<kMeyerWempe
unicode_unihan_variant(0x7D56, kZVariant, 0x7E8A).
unicode_unihan_variant(0x7D5A, kSemanticVariant, 0x7DEA). %<kMatthews 0x7E06<kMatthews
unicode_unihan_variant(0x7D5B, kSemanticVariant, 0x7E1A). %<kMatthews
unicode_unihan_variant(0x7D5B, kSimplifiedVariant, 0x7EE6).
unicode_unihan_variant(0x7D5D, kSemanticVariant, 0x88B4). %<kLau,kMatthews 0x8932<kLau,kMatthews
unicode_unihan_variant(0x7D5D, kSimplifiedVariant, 0x7ED4).
unicode_unihan_variant(0x7D5D, kZVariant, 0x8932).
unicode_unihan_variant(0x7D5E, kSimplifiedVariant, 0x7EDE).
unicode_unihan_variant(0x7D61, kSimplifiedVariant, 0x7EDC).
unicode_unihan_variant(0x7D62, kSimplifiedVariant, 0x7EDA).
unicode_unihan_variant(0x7D63, kSpecializedSemanticVariant, 0x80CC). %<kFenn
unicode_unihan_variant(0x7D65, kSemanticVariant, 0x97B4). %<kMatthews
unicode_unihan_variant(0x7D66, kSimplifiedVariant, 0x7ED9).
unicode_unihan_variant(0x7D68, kSimplifiedVariant, 0x7ED2).
unicode_unihan_variant(0x7D6A, kSemanticVariant, 0x6C24). %<kMeyerWempe
unicode_unihan_variant(0x7D70, kSimplifiedVariant, 0x7ED6).
unicode_unihan_variant(0x7D71, kSemanticVariant, 0x7D82). %<kHanYu:T
unicode_unihan_variant(0x7D71, kSimplifiedVariant, 0x7EDF).
unicode_unihan_variant(0x7D72, kSimplifiedVariant, 0x4E1D).
unicode_unihan_variant(0x7D72, kZVariant, 0x7E9F).
unicode_unihan_variant(0x7D73, kSimplifiedVariant, 0x7EDB).
unicode_unihan_variant(0x7D75, kZVariant, 0x7E6A).
unicode_unihan_variant(0x7D76, kZVariant, 0x7D55).
unicode_unihan_variant(0x7D77, kTraditionalVariant, 0x7E36).
unicode_unihan_variant(0x7D79, kSimplifiedVariant, 0x7EE2).
unicode_unihan_variant(0x7D7A, kSimplifiedVariant, 0x2B128).
unicode_unihan_variant(0x7D80, kSimplifiedVariant, 0x2620C).
unicode_unihan_variant(0x7D81, kSimplifiedVariant, 0x7ED1).
unicode_unihan_variant(0x7D82, kSemanticVariant, 0x7D71). %<kHanYu
unicode_unihan_variant(0x7D83, kSimplifiedVariant, 0x7EE1).
unicode_unihan_variant(0x7D86, kSimplifiedVariant, 0x7EE0).
unicode_unihan_variant(0x7D87, kSimplifiedVariant, 0x2620B).
unicode_unihan_variant(0x7D88, kSimplifiedVariant, 0x7EE8).
unicode_unihan_variant(0x7D89, kSemanticVariant, 0x7E61). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x7D89, kZVariant, 0x7E61).
unicode_unihan_variant(0x7D8C, kSimplifiedVariant, 0x7EE4).
unicode_unihan_variant(0x7D8F, kSimplifiedVariant, 0x7EE5).
unicode_unihan_variant(0x7D90, kSimplifiedVariant, 0x433C).
unicode_unihan_variant(0x7D91, kSemanticVariant, 0x6346). %<kLau
unicode_unihan_variant(0x7D91, kZVariant, 0x6346).
unicode_unihan_variant(0x7D93, kSemanticVariant, 0x26007). %<kHanYu
unicode_unihan_variant(0x7D93, kSimplifiedVariant, 0x7ECF).
unicode_unihan_variant(0x7D93, kZVariant, 0x7D4C).
unicode_unihan_variant(0x7D99, kZVariant, 0x7E7C).
unicode_unihan_variant(0x7D9A, kZVariant, 0x7E8C).
unicode_unihan_variant(0x7D9C, kSimplifiedVariant, 0x7EFC).
unicode_unihan_variant(0x7D9E, kSimplifiedVariant, 0x7F0D).
unicode_unihan_variant(0x7DA0, kSemanticVariant, 0x6C2F). %<kMatthews
unicode_unihan_variant(0x7DA0, kSimplifiedVariant, 0x7EFF).
unicode_unihan_variant(0x7DA0, kZVariant, 0xF93D).
unicode_unihan_variant(0x7DA2, kSemanticVariant, 0x7D2C). %<kFenn
unicode_unihan_variant(0x7DA2, kSimplifiedVariant, 0x7EF8).
unicode_unihan_variant(0x7DA3, kSimplifiedVariant, 0x7EFB).
unicode_unihan_variant(0x7DAB, kSemanticVariant, 0x7DDA). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x7DAC, kSimplifiedVariant, 0x7EF6).
unicode_unihan_variant(0x7DAD, kSimplifiedVariant, 0x7EF4).
unicode_unihan_variant(0x7DAF, kSimplifiedVariant, 0x7EF9).
unicode_unihan_variant(0x7DB0, kSimplifiedVariant, 0x7EFE).
unicode_unihan_variant(0x7DB1, kSimplifiedVariant, 0x7EB2).
unicode_unihan_variant(0x7DB2, kSimplifiedVariant, 0x7F51).
unicode_unihan_variant(0x7DB3, kSemanticVariant, 0x7E43). %<kLau,kMatthews
unicode_unihan_variant(0x7DB3, kZVariant, 0x7E43).
unicode_unihan_variant(0x7DB4, kSimplifiedVariant, 0x7F00).
unicode_unihan_variant(0x7DB5, kSimplifiedVariant, 0x433D).
unicode_unihan_variant(0x7DB8, kSimplifiedVariant, 0x7EB6).
unicode_unihan_variant(0x7DB9, kSimplifiedVariant, 0x7EFA).
unicode_unihan_variant(0x7DBA, kSimplifiedVariant, 0x7EEE).
unicode_unihan_variant(0x7DBB, kSemanticVariant, 0x463A). %<kFenn
unicode_unihan_variant(0x7DBB, kSimplifiedVariant, 0x7EFD).
unicode_unihan_variant(0x7DBD, kSimplifiedVariant, 0x7EF0).
unicode_unihan_variant(0x7DBE, kSimplifiedVariant, 0x7EEB).
unicode_unihan_variant(0x7DBF, kSemanticVariant, 0x7DDC). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7DBF, kSimplifiedVariant, 0x7EF5).
unicode_unihan_variant(0x7DC4, kSimplifiedVariant, 0x7EF2).
unicode_unihan_variant(0x7DC7, kSimplifiedVariant, 0x7F01).
unicode_unihan_variant(0x7DCA, kSimplifiedVariant, 0x7D27).
unicode_unihan_variant(0x7DCB, kSimplifiedVariant, 0x7EEF).
unicode_unihan_variant(0x7DCD, kSimplifiedVariant, 0x2620F).
unicode_unihan_variant(0x7DCF, kZVariant, 0x7E3D).
unicode_unihan_variant(0x7DD1, kZVariant, 0x7DA0).
unicode_unihan_variant(0x7DD2, kSimplifiedVariant, 0x7EEA).
unicode_unihan_variant(0x7DD2, kZVariant, 0x7DD6).
unicode_unihan_variant(0x7DD3, kSimplifiedVariant, 0x7EEC).
unicode_unihan_variant(0x7DD6, kZVariant, 0x7DD2).
unicode_unihan_variant(0x7DD7, kSimplifiedVariant, 0x7F03).
unicode_unihan_variant(0x7DD8, kSimplifiedVariant, 0x7F04).
unicode_unihan_variant(0x7DD9, kSimplifiedVariant, 0x7F02).
unicode_unihan_variant(0x7DDA, kSemanticVariant, 0x7DAB). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x7DDA, kSimplifiedVariant, 0x7EBF).
unicode_unihan_variant(0x7DDC, kSemanticVariant, 0x7DBF). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7DDD, kSimplifiedVariant, 0x7F09).
unicode_unihan_variant(0x7DDE, kSimplifiedVariant, 0x7F0E).
unicode_unihan_variant(0x7DE0, kSimplifiedVariant, 0x7F14).
unicode_unihan_variant(0x7DE1, kSimplifiedVariant, 0x7F17).
unicode_unihan_variant(0x7DE1, kSpecializedSemanticVariant, 0x7720). %<kMeyerWempe
unicode_unihan_variant(0x7DE3, kSemanticVariant, 0x7E01). %<kMatthews
unicode_unihan_variant(0x7DE3, kSimplifiedVariant, 0x7F18).
unicode_unihan_variant(0x7DE3, kZVariant, 0x7E01).
unicode_unihan_variant(0x7DE4, kSemanticVariant, 0x7D32). %<kMeyerWempe
unicode_unihan_variant(0x7DE5, kSemanticVariant, 0x8913). %<kMatthews
unicode_unihan_variant(0x7DE6, kSimplifiedVariant, 0x7F0C).
unicode_unihan_variant(0x7DE8, kSimplifiedVariant, 0x7F16).
unicode_unihan_variant(0x7DE9, kSimplifiedVariant, 0x7F13).
unicode_unihan_variant(0x7DEA, kSemanticVariant, 0x7D5A). %<kMatthews 0x7E06<kMatthews
unicode_unihan_variant(0x7DEC, kSimplifiedVariant, 0x7F05).
unicode_unihan_variant(0x7DEF, kSimplifiedVariant, 0x7EAC).
unicode_unihan_variant(0x7DF0, kSimplifiedVariant, 0x26215).
unicode_unihan_variant(0x7DF1, kSimplifiedVariant, 0x7F11).
unicode_unihan_variant(0x7DF2, kSemanticVariant, 0x7707). %<kMeyerWempe
unicode_unihan_variant(0x7DF2, kSimplifiedVariant, 0x7F08).
unicode_unihan_variant(0x7DF4, kSimplifiedVariant, 0x7EC3).
unicode_unihan_variant(0x7DF6, kSimplifiedVariant, 0x7F0F).
unicode_unihan_variant(0x7DF7, kSimplifiedVariant, 0x26209).
unicode_unihan_variant(0x7DF8, kSimplifiedVariant, 0x26211).
unicode_unihan_variant(0x7DF9, kSimplifiedVariant, 0x7F07).
unicode_unihan_variant(0x7DFB, kSimplifiedVariant, 0x81F4).
unicode_unihan_variant(0x7DFC, kZVariant, 0x7E15).
unicode_unihan_variant(0x7E01, kSemanticVariant, 0x7DE3). %<kMatthews
unicode_unihan_variant(0x7E01, kZVariant, 0x7DE3).
unicode_unihan_variant(0x7E02, kZVariant, 0x7E3D).
unicode_unihan_variant(0x7E04, kZVariant, 0x7E69).
unicode_unihan_variant(0x7E06, kSemanticVariant, 0x7D5A). %<kMatthews 0x7DEA<kMatthews
unicode_unihan_variant(0x7E08, kSimplifiedVariant, 0x8426).
unicode_unihan_variant(0x7E09, kSimplifiedVariant, 0x7F19).
unicode_unihan_variant(0x7E0A, kSimplifiedVariant, 0x7F22).
unicode_unihan_variant(0x7E0B, kSimplifiedVariant, 0x7F12).
unicode_unihan_variant(0x7E0E, kSimplifiedVariant, 0x26214).
unicode_unihan_variant(0x7E10, kSemanticVariant, 0x76BA). %<kLau
unicode_unihan_variant(0x7E10, kSimplifiedVariant, 0x7EC9).
unicode_unihan_variant(0x7E11, kSimplifiedVariant, 0x7F23).
unicode_unihan_variant(0x7E15, kSimplifiedVariant, 0x7F0A).
unicode_unihan_variant(0x7E15, kSpecializedSemanticVariant, 0x97DE). %<kMeyerWempe
unicode_unihan_variant(0x7E17, kSimplifiedVariant, 0x7F1E).
unicode_unihan_variant(0x7E1A, kSemanticVariant, 0x7D5B). %<kMatthews
unicode_unihan_variant(0x7E1B, kSimplifiedVariant, 0x7F1A).
unicode_unihan_variant(0x7E1D, kSimplifiedVariant, 0x7F1C).
unicode_unihan_variant(0x7E1E, kSimplifiedVariant, 0x7F1F).
unicode_unihan_variant(0x7E1F, kSemanticVariant, 0x6EBD). %<kLau
unicode_unihan_variant(0x7E1F, kSimplifiedVariant, 0x7F1B).
unicode_unihan_variant(0x7E23, kSimplifiedVariant, 0x53BF).
unicode_unihan_variant(0x7E26, kZVariant, 0x7E31).
unicode_unihan_variant(0x7E2B, kSimplifiedVariant, 0x7F1D).
unicode_unihan_variant(0x7E2C, kSimplifiedVariant, 0x2621A).
unicode_unihan_variant(0x7E2D, kSemanticVariant, 0x8935). %<kMeyerWempe
unicode_unihan_variant(0x7E2D, kSimplifiedVariant, 0x7F21).
unicode_unihan_variant(0x7E2E, kSimplifiedVariant, 0x7F29).
unicode_unihan_variant(0x7E2F, kSemanticVariant, 0x4325). %<kFenn
unicode_unihan_variant(0x7E31, kSimplifiedVariant, 0x7EB5).
unicode_unihan_variant(0x7E32, kSimplifiedVariant, 0x7F27).
unicode_unihan_variant(0x7E33, kSimplifiedVariant, 0x4338).
unicode_unihan_variant(0x7E34, kZVariant, 0x7EA4).
unicode_unihan_variant(0x7E35, kSimplifiedVariant, 0x7F26).
unicode_unihan_variant(0x7E36, kSimplifiedVariant, 0x7D77).
unicode_unihan_variant(0x7E37, kSimplifiedVariant, 0x7F15).
unicode_unihan_variant(0x7E39, kSimplifiedVariant, 0x7F25).
unicode_unihan_variant(0x7E3A, kSimplifiedVariant, 0x26210).
unicode_unihan_variant(0x7E3D, kSemanticVariant, 0x6403). %<kFenn 0x6460<kFenn
unicode_unihan_variant(0x7E3D, kSimplifiedVariant, 0x603B).
unicode_unihan_variant(0x7E3D, kZVariant, 0x7DCF).
unicode_unihan_variant(0x7E3E, kSimplifiedVariant, 0x7EE9).
unicode_unihan_variant(0x7E41, kZVariant, 0x7DD0).
unicode_unihan_variant(0x7E43, kSemanticVariant, 0x7DB3). %<kLau,kMatthews
unicode_unihan_variant(0x7E43, kSimplifiedVariant, 0x7EF7).
unicode_unihan_variant(0x7E43, kZVariant, 0x7DB3).
unicode_unihan_variant(0x7E45, kSemanticVariant, 0x7E70). %<kMatthews
unicode_unihan_variant(0x7E45, kSimplifiedVariant, 0x7F2B).
unicode_unihan_variant(0x7E46, kSimplifiedVariant, 0x7F2A).
unicode_unihan_variant(0x7E48, kSemanticVariant, 0x7E66). %<kMatthews 0x8941<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7E48, kZVariant, 0x7E66).
unicode_unihan_variant(0x7E4A, kZVariant, 0x7E96).
unicode_unihan_variant(0x7E4B, kZVariant, 0x7E6B).
unicode_unihan_variant(0x7E4D, kZVariant, 0x7E61).
unicode_unihan_variant(0x7E4F, kSimplifiedVariant, 0x2621D).
unicode_unihan_variant(0x7E52, kSimplifiedVariant, 0x7F2F).
unicode_unihan_variant(0x7E53, kSimplifiedVariant, 0x2621B).
unicode_unihan_variant(0x7E54, kSimplifiedVariant, 0x7EC7).
unicode_unihan_variant(0x7E55, kSimplifiedVariant, 0x7F2E).
unicode_unihan_variant(0x7E56, kSemanticVariant, 0x5098). %<kMatthews
unicode_unihan_variant(0x7E59, kZVariant, 0x7FFB).
unicode_unihan_variant(0x7E5A, kSimplifiedVariant, 0x7F2D).
unicode_unihan_variant(0x7E5E, kSemanticVariant, 0x9076). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7E5E, kSimplifiedVariant, 0x7ED5).
unicode_unihan_variant(0x7E5F, kSimplifiedVariant, 0x2620E).
unicode_unihan_variant(0x7E61, kSemanticVariant, 0x7D89). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x7E61, kSimplifiedVariant, 0x7EE3).
unicode_unihan_variant(0x7E61, kZVariant, 0x7E4D).
unicode_unihan_variant(0x7E62, kSimplifiedVariant, 0x7F0B).
unicode_unihan_variant(0x7E62, kSpecializedSemanticVariant, 0x7E6A). %<kMeyerWempe
unicode_unihan_variant(0x7E62, kZVariant, 0x7E6A).
unicode_unihan_variant(0x7E66, kSemanticVariant, 0x7E48). %<kMatthews 0x8941<kMatthews
unicode_unihan_variant(0x7E66, kZVariant, 0x7E48).
unicode_unihan_variant(0x7E69, kSemanticVariant, 0x42F2). %<kMeyerWempe
unicode_unihan_variant(0x7E69, kSimplifiedVariant, 0x7EF3).
unicode_unihan_variant(0x7E69, kZVariant, 0x7E04).
unicode_unihan_variant(0x7E6A, kSimplifiedVariant, 0x7ED8).
unicode_unihan_variant(0x7E6A, kSpecializedSemanticVariant, 0x7E62). %<kMeyerWempe
unicode_unihan_variant(0x7E6A, kZVariant, 0x7F0B).
unicode_unihan_variant(0x7E6B, kSimplifiedVariant, 0x7CFB).
unicode_unihan_variant(0x7E6B, kZVariant, 0x7E4B).
unicode_unihan_variant(0x7E6D, kSimplifiedVariant, 0x8327).
unicode_unihan_variant(0x7E6E, kSemanticVariant, 0x97C1). %<kLau,kMatthews
unicode_unihan_variant(0x7E6F, kSimplifiedVariant, 0x7F33).
unicode_unihan_variant(0x7E70, kSemanticVariant, 0x7E45). %<kMatthews
unicode_unihan_variant(0x7E70, kSimplifiedVariant, 0x7F32).
unicode_unihan_variant(0x7E72, kSemanticVariant, 0x632B). %<kMatthews
unicode_unihan_variant(0x7E73, kSimplifiedVariant, 0x7F34).
unicode_unihan_variant(0x7E78, kSimplifiedVariant, 0x4341).
unicode_unihan_variant(0x7E79, kSimplifiedVariant, 0x7ECE).
unicode_unihan_variant(0x7E7B, kSimplifiedVariant, 0x26221).
unicode_unihan_variant(0x7E7C, kSimplifiedVariant, 0x7EE7).
unicode_unihan_variant(0x7E7D, kSimplifiedVariant, 0x7F24).
unicode_unihan_variant(0x7E7E, kSimplifiedVariant, 0x7F31).
unicode_unihan_variant(0x7E7F, kSimplifiedVariant, 0x4340).
unicode_unihan_variant(0x7E7F, kZVariant, 0x8964).
unicode_unihan_variant(0x7E81, kSimplifiedVariant, 0x2B138).
unicode_unihan_variant(0x7E82, kSpecializedSemanticVariant, 0x81C7). %<kFenn 0x243B1<kFenn
unicode_unihan_variant(0x7E87, kSimplifiedVariant, 0x98A3).
unicode_unihan_variant(0x7E87, kZVariant, 0x98A3).
unicode_unihan_variant(0x7E88, kSimplifiedVariant, 0x7F2C).
unicode_unihan_variant(0x7E89, kZVariant, 0x7E98).
unicode_unihan_variant(0x7E8A, kSemanticVariant, 0x7D56). %<kMatthews
unicode_unihan_variant(0x7E8A, kSimplifiedVariant, 0x7EA9).
unicode_unihan_variant(0x7E8A, kSpecializedSemanticVariant, 0x7D56). %<kMeyerWempe
unicode_unihan_variant(0x7E8A, kZVariant, 0x7D4B).
unicode_unihan_variant(0x7E8C, kSimplifiedVariant, 0x7EED).
unicode_unihan_variant(0x7E8D, kSimplifiedVariant, 0x7D2F).
unicode_unihan_variant(0x7E8E, kSemanticVariant, 0x7E96). %<kMeyerWempe
unicode_unihan_variant(0x7E8E, kZVariant, 0x7E96).
unicode_unihan_variant(0x7E8F, kSimplifiedVariant, 0x7F20).
unicode_unihan_variant(0x7E92, kZVariant, 0x7E8F).
unicode_unihan_variant(0x7E93, kSimplifiedVariant, 0x7F28).
unicode_unihan_variant(0x7E94, kSpecializedSemanticVariant, 0x624D). %<kFenn
unicode_unihan_variant(0x7E96, kSemanticVariant, 0x780C). %<kLau 0x7E8E<kMeyerWempe
unicode_unihan_variant(0x7E96, kSimplifiedVariant, 0x7EA4).
unicode_unihan_variant(0x7E98, kSimplifiedVariant, 0x7F35).
unicode_unihan_variant(0x7E98, kZVariant, 0x7E89).
unicode_unihan_variant(0x7E9C, kSemanticVariant, 0x432B). %<kMeyerWempe
unicode_unihan_variant(0x7E9C, kSimplifiedVariant, 0x7F06).
unicode_unihan_variant(0x7E9F, kTraditionalVariant, 0x7CF9).
unicode_unihan_variant(0x7EA0, kTraditionalVariant, 0x7CFE).
unicode_unihan_variant(0x7EA1, kTraditionalVariant, 0x7D06).
unicode_unihan_variant(0x7EA2, kTraditionalVariant, 0x7D05).
unicode_unihan_variant(0x7EA3, kTraditionalVariant, 0x7D02).
unicode_unihan_variant(0x7EA4, kTraditionalVariant, 0x7E96).
unicode_unihan_variant(0x7EA4, kZVariant, 0x7E34).
unicode_unihan_variant(0x7EA5, kTraditionalVariant, 0x7D07).
unicode_unihan_variant(0x7EA6, kTraditionalVariant, 0x7D04).
unicode_unihan_variant(0x7EA7, kTraditionalVariant, 0x7D1A).
unicode_unihan_variant(0x7EA8, kTraditionalVariant, 0x7D08).
unicode_unihan_variant(0x7EA9, kTraditionalVariant, 0x7E8A).
unicode_unihan_variant(0x7EAA, kTraditionalVariant, 0x7D00).
unicode_unihan_variant(0x7EAB, kTraditionalVariant, 0x7D09).
unicode_unihan_variant(0x7EAC, kTraditionalVariant, 0x7DEF).
unicode_unihan_variant(0x7EAD, kTraditionalVariant, 0x7D1C).
unicode_unihan_variant(0x7EAE, kTraditionalVariant, 0x7D18).
unicode_unihan_variant(0x7EAF, kTraditionalVariant, 0x7D14).
unicode_unihan_variant(0x7EB0, kTraditionalVariant, 0x7D15).
unicode_unihan_variant(0x7EB1, kTraditionalVariant, 0x7D17).
unicode_unihan_variant(0x7EB2, kTraditionalVariant, 0x7DB1).
unicode_unihan_variant(0x7EB3, kTraditionalVariant, 0x7D0D).
unicode_unihan_variant(0x7EB4, kTraditionalVariant, 0x7D1D).
unicode_unihan_variant(0x7EB5, kTraditionalVariant, 0x7E31).
unicode_unihan_variant(0x7EB6, kTraditionalVariant, 0x7DB8).
unicode_unihan_variant(0x7EB7, kTraditionalVariant, 0x7D1B).
unicode_unihan_variant(0x7EB8, kTraditionalVariant, 0x7D19).
unicode_unihan_variant(0x7EB9, kTraditionalVariant, 0x7D0B).
unicode_unihan_variant(0x7EBA, kTraditionalVariant, 0x7D21).
unicode_unihan_variant(0x7EBB, kTraditionalVariant, 0x7D35).
unicode_unihan_variant(0x7EBC, kTraditionalVariant, 0x7D16).
unicode_unihan_variant(0x7EBD, kTraditionalVariant, 0x7D10).
unicode_unihan_variant(0x7EBE, kTraditionalVariant, 0x7D13).
unicode_unihan_variant(0x7EBF, kTraditionalVariant, 0x7DDA).
unicode_unihan_variant(0x7EBF, kZVariant, 0x7DDA).
unicode_unihan_variant(0x7EC0, kTraditionalVariant, 0x7D3A).
unicode_unihan_variant(0x7EC1, kTraditionalVariant, 0x7D32).
unicode_unihan_variant(0x7EC2, kTraditionalVariant, 0x7D31).
unicode_unihan_variant(0x7EC3, kTraditionalVariant, 0x7DF4).
unicode_unihan_variant(0x7EC4, kTraditionalVariant, 0x7D44).
unicode_unihan_variant(0x7EC5, kTraditionalVariant, 0x7D33).
unicode_unihan_variant(0x7EC6, kTraditionalVariant, 0x7D30).
unicode_unihan_variant(0x7EC7, kTraditionalVariant, 0x7E54).
unicode_unihan_variant(0x7EC8, kTraditionalVariant, 0x7D42).
unicode_unihan_variant(0x7EC9, kTraditionalVariant, 0x7E10).
unicode_unihan_variant(0x7ECA, kTraditionalVariant, 0x7D46).
unicode_unihan_variant(0x7ECB, kTraditionalVariant, 0x7D3C).
unicode_unihan_variant(0x7ECC, kTraditionalVariant, 0x7D40).
unicode_unihan_variant(0x7ECD, kTraditionalVariant, 0x7D39).
unicode_unihan_variant(0x7ECE, kTraditionalVariant, 0x7E79).
unicode_unihan_variant(0x7ECF, kTraditionalVariant, 0x7D93).
unicode_unihan_variant(0x7ED0, kTraditionalVariant, 0x7D3F).
unicode_unihan_variant(0x7ED1, kTraditionalVariant, 0x7D81).
unicode_unihan_variant(0x7ED2, kTraditionalVariant, 0x7D68).
unicode_unihan_variant(0x7ED3, kTraditionalVariant, 0x7D50).
unicode_unihan_variant(0x7ED4, kTraditionalVariant, 0x7D5D).
unicode_unihan_variant(0x7ED4, kZVariant, 0x8932).
unicode_unihan_variant(0x7ED5, kTraditionalVariant, 0x7E5E).
unicode_unihan_variant(0x7ED6, kTraditionalVariant, 0x7D70).
unicode_unihan_variant(0x7ED7, kTraditionalVariant, 0x7D4E).
unicode_unihan_variant(0x7ED8, kTraditionalVariant, 0x7E6A).
unicode_unihan_variant(0x7ED9, kTraditionalVariant, 0x7D66).
unicode_unihan_variant(0x7EDA, kTraditionalVariant, 0x7D62).
unicode_unihan_variant(0x7EDB, kTraditionalVariant, 0x7D73).
unicode_unihan_variant(0x7EDC, kTraditionalVariant, 0x7D61).
unicode_unihan_variant(0x7EDD, kTraditionalVariant, 0x7D55).
unicode_unihan_variant(0x7EDD, kZVariant, 0x7D55).
unicode_unihan_variant(0x7EDE, kTraditionalVariant, 0x7D5E).
unicode_unihan_variant(0x7EDF, kTraditionalVariant, 0x7D71).
unicode_unihan_variant(0x7EE0, kTraditionalVariant, 0x7D86).
unicode_unihan_variant(0x7EE1, kTraditionalVariant, 0x7D83).
unicode_unihan_variant(0x7EE2, kTraditionalVariant, 0x7D79).
unicode_unihan_variant(0x7EE3, kTraditionalVariant, 0x7E61).
unicode_unihan_variant(0x7EE3, kZVariant, 0x7E61).
unicode_unihan_variant(0x7EE4, kTraditionalVariant, 0x7D8C).
unicode_unihan_variant(0x7EE5, kTraditionalVariant, 0x7D8F).
unicode_unihan_variant(0x7EE6, kTraditionalVariant, 0x7D5B).
unicode_unihan_variant(0x7EE7, kTraditionalVariant, 0x7E7C).
unicode_unihan_variant(0x7EE8, kTraditionalVariant, 0x7D88).
unicode_unihan_variant(0x7EE9, kTraditionalVariant, 0x7E3E).
unicode_unihan_variant(0x7EEA, kTraditionalVariant, 0x7DD2).
unicode_unihan_variant(0x7EEB, kTraditionalVariant, 0x7DBE).
unicode_unihan_variant(0x7EEC, kTraditionalVariant, 0x7DD3).
unicode_unihan_variant(0x7EED, kTraditionalVariant, 0x7E8C).
unicode_unihan_variant(0x7EEE, kTraditionalVariant, 0x7DBA).
unicode_unihan_variant(0x7EEF, kTraditionalVariant, 0x7DCB).
unicode_unihan_variant(0x7EF0, kTraditionalVariant, 0x7DBD).
unicode_unihan_variant(0x7EF1, kTraditionalVariant, 0x979D).
unicode_unihan_variant(0x7EF1, kZVariant, 0x7DD4).
unicode_unihan_variant(0x7EF2, kTraditionalVariant, 0x7DC4).
unicode_unihan_variant(0x7EF3, kTraditionalVariant, 0x7E69).
unicode_unihan_variant(0x7EF4, kTraditionalVariant, 0x7DAD).
unicode_unihan_variant(0x7EF5, kTraditionalVariant, 0x7DBF).
unicode_unihan_variant(0x7EF6, kTraditionalVariant, 0x7DAC).
unicode_unihan_variant(0x7EF7, kTraditionalVariant, 0x7E43).
unicode_unihan_variant(0x7EF7, kZVariant, 0x7E43).
unicode_unihan_variant(0x7EF8, kTraditionalVariant, 0x7DA2).
unicode_unihan_variant(0x7EF9, kTraditionalVariant, 0x7DAF).
unicode_unihan_variant(0x7EFA, kTraditionalVariant, 0x7DB9).
unicode_unihan_variant(0x7EFB, kTraditionalVariant, 0x7DA3).
unicode_unihan_variant(0x7EFC, kTraditionalVariant, 0x7D9C).
unicode_unihan_variant(0x7EFD, kTraditionalVariant, 0x7DBB).
unicode_unihan_variant(0x7EFE, kTraditionalVariant, 0x7DB0).
unicode_unihan_variant(0x7EFF, kTraditionalVariant, 0x7DA0).
unicode_unihan_variant(0x7EFF, kZVariant, 0x7DA0).
unicode_unihan_variant(0x7F00, kTraditionalVariant, 0x7DB4).
unicode_unihan_variant(0x7F01, kTraditionalVariant, 0x7DC7).
unicode_unihan_variant(0x7F02, kTraditionalVariant, 0x7DD9).
unicode_unihan_variant(0x7F03, kTraditionalVariant, 0x7DD7).
unicode_unihan_variant(0x7F04, kTraditionalVariant, 0x7DD8).
unicode_unihan_variant(0x7F05, kTraditionalVariant, 0x7DEC).
unicode_unihan_variant(0x7F06, kTraditionalVariant, 0x7E9C).
unicode_unihan_variant(0x7F07, kTraditionalVariant, 0x7DF9).
unicode_unihan_variant(0x7F08, kTraditionalVariant, 0x7DF2).
unicode_unihan_variant(0x7F09, kTraditionalVariant, 0x7DDD).
unicode_unihan_variant(0x7F0A, kTraditionalVariant, 0x7E15).
unicode_unihan_variant(0x7F0B, kTraditionalVariant, 0x7E62).
unicode_unihan_variant(0x7F0B, kZVariant, 0x7E6A).
unicode_unihan_variant(0x7F0C, kTraditionalVariant, 0x7DE6).
unicode_unihan_variant(0x7F0D, kTraditionalVariant, 0x7D9E).
unicode_unihan_variant(0x7F0E, kTraditionalVariant, 0x7DDE).
unicode_unihan_variant(0x7F0F, kTraditionalVariant, 0x7DF6).
unicode_unihan_variant(0x7F10, kZVariant, 0x7DDA).
unicode_unihan_variant(0x7F11, kTraditionalVariant, 0x7DF1).
unicode_unihan_variant(0x7F12, kTraditionalVariant, 0x7E0B).
unicode_unihan_variant(0x7F13, kTraditionalVariant, 0x7DE9).
unicode_unihan_variant(0x7F14, kTraditionalVariant, 0x7DE0).
unicode_unihan_variant(0x7F15, kTraditionalVariant, 0x7E37).
unicode_unihan_variant(0x7F16, kTraditionalVariant, 0x7DE8).
unicode_unihan_variant(0x7F17, kTraditionalVariant, 0x7DE1).
unicode_unihan_variant(0x7F18, kTraditionalVariant, 0x7DE3).
unicode_unihan_variant(0x7F19, kTraditionalVariant, 0x7E09).
unicode_unihan_variant(0x7F1A, kTraditionalVariant, 0x7E1B).
unicode_unihan_variant(0x7F1B, kTraditionalVariant, 0x7E1F).
unicode_unihan_variant(0x7F1C, kTraditionalVariant, 0x7E1D).
unicode_unihan_variant(0x7F1D, kTraditionalVariant, 0x7E2B).
unicode_unihan_variant(0x7F1E, kTraditionalVariant, 0x7E17).
unicode_unihan_variant(0x7F1F, kTraditionalVariant, 0x7E1E).
unicode_unihan_variant(0x7F20, kTraditionalVariant, 0x7E8F).
unicode_unihan_variant(0x7F21, kTraditionalVariant, 0x7E2D).
unicode_unihan_variant(0x7F22, kTraditionalVariant, 0x7E0A).
unicode_unihan_variant(0x7F23, kTraditionalVariant, 0x7E11).
unicode_unihan_variant(0x7F24, kTraditionalVariant, 0x7E7D).
unicode_unihan_variant(0x7F25, kTraditionalVariant, 0x7E39).
unicode_unihan_variant(0x7F26, kTraditionalVariant, 0x7E35).
unicode_unihan_variant(0x7F27, kTraditionalVariant, 0x7E32).
unicode_unihan_variant(0x7F28, kTraditionalVariant, 0x7E93).
unicode_unihan_variant(0x7F29, kTraditionalVariant, 0x7E2E).
unicode_unihan_variant(0x7F2A, kTraditionalVariant, 0x7E46).
unicode_unihan_variant(0x7F2B, kTraditionalVariant, 0x7E45).
unicode_unihan_variant(0x7F2C, kTraditionalVariant, 0x7E88).
unicode_unihan_variant(0x7F2D, kTraditionalVariant, 0x7E5A).
unicode_unihan_variant(0x7F2E, kTraditionalVariant, 0x7E55).
unicode_unihan_variant(0x7F2F, kTraditionalVariant, 0x7E52).
unicode_unihan_variant(0x7F30, kTraditionalVariant, 0x97C1).
unicode_unihan_variant(0x7F30, kZVariant, 0x7E6E).
unicode_unihan_variant(0x7F31, kTraditionalVariant, 0x7E7E).
unicode_unihan_variant(0x7F32, kTraditionalVariant, 0x7E70).
unicode_unihan_variant(0x7F33, kTraditionalVariant, 0x7E6F).
unicode_unihan_variant(0x7F34, kTraditionalVariant, 0x7E73).
unicode_unihan_variant(0x7F35, kTraditionalVariant, 0x7E98).
unicode_unihan_variant(0x7F36, kSemanticVariant, 0x7F3B). %<kMatthews
unicode_unihan_variant(0x7F36, kZVariant, 0x7F50).
unicode_unihan_variant(0x7F37, kZVariant, 0x5378).
unicode_unihan_variant(0x7F38, kSemanticVariant, 0x5808). %<kLau,kMatthews 0x7F41<kLau,kMatthews
unicode_unihan_variant(0x7F3A, kZVariant, 0x6B20).
unicode_unihan_variant(0x7F3B, kSemanticVariant, 0x7F36). %<kMatthews
unicode_unihan_variant(0x7F3D, kSemanticVariant, 0x9262). %<kHKGlyph
unicode_unihan_variant(0x7F3D, kSimplifiedVariant, 0x94B5).
unicode_unihan_variant(0x7F3E, kSemanticVariant, 0x74F6). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7F3E, kZVariant, 0x74F6).
unicode_unihan_variant(0x7F41, kSemanticVariant, 0x5808). %<kLau,kMatthews 0x7F38<kLau,kMatthews
unicode_unihan_variant(0x7F42, kTraditionalVariant, 0x7F4C).
unicode_unihan_variant(0x7F43, kSemanticVariant, 0x7516). %<kMatthews 0x7F4C<kMatthews
unicode_unihan_variant(0x7F45, kSemanticVariant, 0x28EF2). %<kFenn
unicode_unihan_variant(0x7F47, kSemanticVariant, 0x58AB). %<kMatthews 0x6A3D<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7F47, kZVariant, 0x6A3D).
unicode_unihan_variant(0x7F48, kSemanticVariant, 0x58B0). %<kFenn 0x58DC<kMatthews
unicode_unihan_variant(0x7F48, kSimplifiedVariant, 0x575B).
unicode_unihan_variant(0x7F48, kSpecializedSemanticVariant, 0x58B0). %<kMeyerWempe
unicode_unihan_variant(0x7F4B, kSemanticVariant, 0x74EE). %<kMatthews 0x7515<kMatthews
unicode_unihan_variant(0x7F4C, kSemanticVariant, 0x7516). %<kMatthews 0x7F43<kMatthews
unicode_unihan_variant(0x7F4C, kSimplifiedVariant, 0x7F42).
unicode_unihan_variant(0x7F50, kSemanticVariant, 0x7936). %<kFenn
unicode_unihan_variant(0x7F50, kZVariant, 0x7F36).
unicode_unihan_variant(0x7F51, kSemanticVariant, 0x34C1). %<kMatthews 0x7F54<kFenn
unicode_unihan_variant(0x7F51, kTraditionalVariant, 0x7DB2).
unicode_unihan_variant(0x7F54, kSemanticVariant, 0x7F51). %<kFenn
unicode_unihan_variant(0x7F57, kSemanticVariant, 0x7F85). %<kFenn
unicode_unihan_variant(0x7F57, kTraditionalVariant, 0x7F85).
unicode_unihan_variant(0x7F57, kZVariant, 0x56C9).
unicode_unihan_variant(0x7F58, kZVariant, 0x7F66).
unicode_unihan_variant(0x7F5A, kTraditionalVariant, 0x7F70).
unicode_unihan_variant(0x7F62, kTraditionalVariant, 0x7F77).
unicode_unihan_variant(0x7F65, kZVariant, 0x7F82).
unicode_unihan_variant(0x7F69, kSemanticVariant, 0x4207). %<kLau,kMatthews
unicode_unihan_variant(0x7F6A, kSemanticVariant, 0x8FA0). %<kLau,kMatthews
unicode_unihan_variant(0x7F6A, kZVariant, 0x8FA0).
unicode_unihan_variant(0x7F6E, kSemanticVariant, 0x5BD8). %<kMatthews
unicode_unihan_variant(0x7F6E, kZVariant, 0x5BD8).
unicode_unihan_variant(0x7F70, kSimplifiedVariant, 0x7F5A).
unicode_unihan_variant(0x7F70, kZVariant, 0x7F78).
unicode_unihan_variant(0x7F72, kSpecializedSemanticVariant, 0x7A4E). %<kMeyerWempe
unicode_unihan_variant(0x7F74, kTraditionalVariant, 0x7F86).
unicode_unihan_variant(0x7F75, kSemanticVariant, 0x508C). %<kMatthews
unicode_unihan_variant(0x7F75, kSimplifiedVariant, 0x9A82).
unicode_unihan_variant(0x7F76, kZVariant, 0x7F80).
unicode_unihan_variant(0x7F77, kSimplifiedVariant, 0x7F62).
unicode_unihan_variant(0x7F79, kZVariant, 0xF9E6).
unicode_unihan_variant(0x7F80, kZVariant, 0x7F76).
unicode_unihan_variant(0x7F81, kTraditionalVariant, 0x7F88).
unicode_unihan_variant(0x7F82, kZVariant, 0x7F65).
unicode_unihan_variant(0x7F83, kSemanticVariant, 0x51AA). %<kMatthews
unicode_unihan_variant(0x7F83, kZVariant, 0x51AA).
unicode_unihan_variant(0x7F85, kSemanticVariant, 0x7F57). %<kFenn
unicode_unihan_variant(0x7F85, kSimplifiedVariant, 0x7F57).
unicode_unihan_variant(0x7F85, kZVariant, 0xF90F).
unicode_unihan_variant(0x7F86, kSimplifiedVariant, 0x7F74).
unicode_unihan_variant(0x7F87, kZVariant, 0x7F88).
unicode_unihan_variant(0x7F88, kSemanticVariant, 0x898A). %<kMatthews
unicode_unihan_variant(0x7F88, kSimplifiedVariant, 0x7F81).
unicode_unihan_variant(0x7F88, kZVariant, 0x898A).
unicode_unihan_variant(0x7F8B, kSimplifiedVariant, 0x8288).
unicode_unihan_variant(0x7F8C, kSemanticVariant, 0x7F97). %<kMatthews
unicode_unihan_variant(0x7F8E, kSemanticVariant, 0x7F99). %<kMeyerWempe
unicode_unihan_variant(0x7F93, kSemanticVariant, 0x8C5D). %<kMeyerWempe
unicode_unihan_variant(0x7F96, kSemanticVariant, 0x26369). %<kMeyerWempe
unicode_unihan_variant(0x7F97, kSemanticVariant, 0x7F8C). %<kMatthews
unicode_unihan_variant(0x7F99, kSemanticVariant, 0x7F8E). %<kMeyerWempe
unicode_unihan_variant(0x7F9A, kZVariant, 0xF9AF).
unicode_unihan_variant(0x7F9D, kZVariant, 0x7274).
unicode_unihan_variant(0x7F9F, kTraditionalVariant, 0x7FA5).
unicode_unihan_variant(0x7FA1, kSemanticVariant, 0x7FA8). %<kFenn
unicode_unihan_variant(0x7FA1, kZVariant, 0x7FA8).
unicode_unihan_variant(0x7FA3, kSemanticVariant, 0x7FA4). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x7FA3, kZVariant, 0x7FA4).
unicode_unihan_variant(0x7FA4, kSemanticVariant, 0x7FA3). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x7FA4, kZVariant, 0x7FA3).
unicode_unihan_variant(0x7FA5, kSimplifiedVariant, 0x7F9F).
unicode_unihan_variant(0x7FA8, kSemanticVariant, 0x7FA1). %<kFenn
unicode_unihan_variant(0x7FA9, kSemanticVariant, 0x4E49). %<kFenn
unicode_unihan_variant(0x7FA9, kSimplifiedVariant, 0x4E49).
unicode_unihan_variant(0x7FAE, kSemanticVariant, 0x7FB9). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7FAE, kZVariant, 0x7FB9).
unicode_unihan_variant(0x7FB4, kSemanticVariant, 0x7FB6). %<kMatthews
unicode_unihan_variant(0x7FB6, kSemanticVariant, 0x7FB4). %<kMatthews
unicode_unihan_variant(0x7FB6, kSpecializedSemanticVariant, 0x81BB).
unicode_unihan_variant(0x7FB9, kSemanticVariant, 0x7FAE). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7FB9, kZVariant, 0x7FAE).
unicode_unihan_variant(0x7FBD, kZVariant, 0xFA1E).
unicode_unihan_variant(0x7FC4, kSemanticVariant, 0x7FC5). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7FC4, kZVariant, 0x7FC5).
unicode_unihan_variant(0x7FC5, kSemanticVariant, 0x7FC4). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7FC5, kZVariant, 0x7FC4).
unicode_unihan_variant(0x7FC6, kZVariant, 0x7FE0).
unicode_unihan_variant(0x7FD2, kSimplifiedVariant, 0x4E60).
unicode_unihan_variant(0x7FD8, kTraditionalVariant, 0x7FF9).
unicode_unihan_variant(0x7FD9, kTraditionalVariant, 0x7FFD).
unicode_unihan_variant(0x7FDA, kTraditionalVariant, 0x7FEC).
unicode_unihan_variant(0x7FDA, kZVariant, 0x7FEC).
unicode_unihan_variant(0x7FE0, kZVariant, 0x7FC6).
unicode_unihan_variant(0x7FE5, kSemanticVariant, 0x4B21). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x7FE6, kSemanticVariant, 0x526A). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x7FEC, kSimplifiedVariant, 0x7FDA).
unicode_unihan_variant(0x7FEC, kZVariant, 0x7FDA).
unicode_unihan_variant(0x7FF1, kSemanticVariant, 0x7FFA). %<kHKGlyph
unicode_unihan_variant(0x7FF1, kZVariant, 0x7FFA).
unicode_unihan_variant(0x7FF6, kZVariant, 0x7FF1).
unicode_unihan_variant(0x7FF9, kSimplifiedVariant, 0x7FD8).
unicode_unihan_variant(0x7FFA, kSemanticVariant, 0x7FF1). %<kHKGlyph
unicode_unihan_variant(0x7FFA, kZVariant, 0x7FF1).
unicode_unihan_variant(0x7FFB, kZVariant, 0x7E59).
unicode_unihan_variant(0x7FFC, kSpecializedSemanticVariant, 0x6298). %<kFenn
unicode_unihan_variant(0x7FFD, kSimplifiedVariant, 0x7FD9).
unicode_unihan_variant(0x7FFD, kZVariant, 0x7FD9).
unicode_unihan_variant(0x8000, kSemanticVariant, 0x66DC). %<kCowles 0x71FF<kCowles,kFenn
unicode_unihan_variant(0x8001, kZVariant, 0x8002).
unicode_unihan_variant(0x8002, kZVariant, 0x8001).
unicode_unihan_variant(0x8003, kSemanticVariant, 0x6537). %<kMatthews
unicode_unihan_variant(0x8003, kZVariant, 0x6537).
unicode_unihan_variant(0x8007, kZVariant, 0x8008).
unicode_unihan_variant(0x8008, kZVariant, 0x8007).
unicode_unihan_variant(0x8009, kZVariant, 0x8008).
unicode_unihan_variant(0x800F, kSemanticVariant, 0x8010). %<kMatthews
unicode_unihan_variant(0x8010, kSemanticVariant, 0x800F). %<kMatthews
unicode_unihan_variant(0x8011, kSemanticVariant, 0x5C08). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8012, kSpecializedSemanticVariant, 0x4F86). %<kMeyerWempe
unicode_unihan_variant(0x8014, kSemanticVariant, 0x79C4). %<kMeyerWempe
unicode_unihan_variant(0x8015, kSemanticVariant, 0x754A). %<kLau,kMatthews 0x7C21<kLau
unicode_unihan_variant(0x8015, kSpecializedSemanticVariant, 0x754A). %<kFenn
unicode_unihan_variant(0x8017, kSemanticVariant, 0x79CF). %<kMatthews
unicode_unihan_variant(0x8017, kZVariant, 0x79CF).
unicode_unihan_variant(0x8018, kSemanticVariant, 0x79D0). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8018, kZVariant, 0x79D0).
unicode_unihan_variant(0x8019, kSemanticVariant, 0x9200). %<kMatthews
unicode_unihan_variant(0x8021, kZVariant, 0x92E4).
unicode_unihan_variant(0x8022, kTraditionalVariant, 0x802E).
unicode_unihan_variant(0x8024, kSemanticVariant, 0x85C9). %<kMatthews
unicode_unihan_variant(0x8024, kZVariant, 0x85C9).
unicode_unihan_variant(0x8027, kTraditionalVariant, 0x802C).
unicode_unihan_variant(0x8028, kSemanticVariant, 0x9392). %<kMatthews
unicode_unihan_variant(0x802C, kSimplifiedVariant, 0x8027).
unicode_unihan_variant(0x802E, kSimplifiedVariant, 0x8022).
unicode_unihan_variant(0x8030, kSemanticVariant, 0x6ACC). %<kMatthews
unicode_unihan_variant(0x8038, kTraditionalVariant, 0x8073).
unicode_unihan_variant(0x803B, kSemanticVariant, 0x6065). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x803B, kTraditionalVariant, 0x6065).
unicode_unihan_variant(0x803C, kSemanticVariant, 0x8043). %<kMatthews
unicode_unihan_variant(0x803D, kZVariant, 0x8EAD).
unicode_unihan_variant(0x803F, kZVariant, 0x7085).
unicode_unihan_variant(0x8042, kTraditionalVariant, 0x8076).
unicode_unihan_variant(0x8043, kSemanticVariant, 0x803C). %<kMatthews
unicode_unihan_variant(0x8046, kZVariant, 0xF9B0).
unicode_unihan_variant(0x804B, kTraditionalVariant, 0x807E).
unicode_unihan_variant(0x804C, kTraditionalVariant, 0x8077).
unicode_unihan_variant(0x804D, kTraditionalVariant, 0x8079).
unicode_unihan_variant(0x8054, kTraditionalVariant, 0x806F).
unicode_unihan_variant(0x8056, kSemanticVariant, 0x5723). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8056, kSimplifiedVariant, 0x5723).
unicode_unihan_variant(0x8056, kZVariant, 0x580A).
unicode_unihan_variant(0x805D, kSemanticVariant, 0x9998). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x805E, kSimplifiedVariant, 0x95FB).
unicode_unihan_variant(0x805F, kZVariant, 0x5A7F).
unicode_unihan_variant(0x8061, kZVariant, 0x8070).
unicode_unihan_variant(0x8066, kSemanticVariant, 0x8070). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8068, kZVariant, 0x806F).
unicode_unihan_variant(0x8069, kTraditionalVariant, 0x8075).
unicode_unihan_variant(0x806A, kTraditionalVariant, 0x8070).
unicode_unihan_variant(0x806B, kZVariant, 0x806F).
unicode_unihan_variant(0x806E, kZVariant, 0x806F).
unicode_unihan_variant(0x806F, kSimplifiedVariant, 0x8054).
unicode_unihan_variant(0x806F, kZVariant, 0x8068).
unicode_unihan_variant(0x8070, kSemanticVariant, 0x8066). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8070, kSimplifiedVariant, 0x806A).
unicode_unihan_variant(0x8070, kZVariant, 0x8061).
unicode_unihan_variant(0x8072, kSemanticVariant, 0x58F0). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8072, kSimplifiedVariant, 0x58F0).
unicode_unihan_variant(0x8073, kSimplifiedVariant, 0x8038).
unicode_unihan_variant(0x8074, kZVariant, 0x807D).
unicode_unihan_variant(0x8075, kSimplifiedVariant, 0x8069).
unicode_unihan_variant(0x8076, kSimplifiedVariant, 0x8042).
unicode_unihan_variant(0x8077, kSemanticVariant, 0x8EC4). %<kLau,kMatthews
unicode_unihan_variant(0x8077, kSimplifiedVariant, 0x804C).
unicode_unihan_variant(0x8079, kSimplifiedVariant, 0x804D).
unicode_unihan_variant(0x807B, kSemanticVariant, 0x9B59). %<kMeyerWempe
unicode_unihan_variant(0x807C, kZVariant, 0x807D).
unicode_unihan_variant(0x807D, kSemanticVariant, 0x542C). %<kLau,kMatthews
unicode_unihan_variant(0x807D, kSimplifiedVariant, 0x542C).
unicode_unihan_variant(0x807D, kSpecializedSemanticVariant, 0x542C). %<kFenn
unicode_unihan_variant(0x807E, kSimplifiedVariant, 0x804B).
unicode_unihan_variant(0x807E, kZVariant, 0xF945).
unicode_unihan_variant(0x807F, kZVariant, 0x8080).
unicode_unihan_variant(0x8080, kZVariant, 0x807F).
unicode_unihan_variant(0x8083, kTraditionalVariant, 0x8085).
unicode_unihan_variant(0x8085, kSimplifiedVariant, 0x8083).
unicode_unihan_variant(0x8085, kZVariant, 0x7C9B).
unicode_unihan_variant(0x8086, kSemanticVariant, 0x56DB). %<kLau
unicode_unihan_variant(0x8086, kSpecializedSemanticVariant, 0x56DB). %<kFenn
unicode_unihan_variant(0x8086, kZVariant, 0x56DB).
unicode_unihan_variant(0x8087, kSemanticVariant, 0x8088). %<kLau,kMatthews
unicode_unihan_variant(0x8088, kSemanticVariant, 0x8087). %<kLau,kMatthews
unicode_unihan_variant(0x8089, kZVariant, 0x5B8D).
unicode_unihan_variant(0x808B, kZVariant, 0xF953).
unicode_unihan_variant(0x8090, kSemanticVariant, 0x80F3). %<kMatthews 0x29A18<kFenn
unicode_unihan_variant(0x8090, kZVariant, 0x80F3).
unicode_unihan_variant(0x8095, kSemanticVariant, 0x97CC). %<kLau
unicode_unihan_variant(0x8095, kSpecializedSemanticVariant, 0x97CC). %<kMeyerWempe
unicode_unihan_variant(0x80A0, kTraditionalVariant, 0x8178).
unicode_unihan_variant(0x80A1, kSemanticVariant, 0x26676). %<kLau,kMeyerWempe
unicode_unihan_variant(0x80A1, kZVariant, 0x8135).
unicode_unihan_variant(0x80A2, kSemanticVariant, 0x80D1). %<kMatthews
unicode_unihan_variant(0x80A4, kTraditionalVariant, 0x819A).
unicode_unihan_variant(0x80A7, kSemanticVariant, 0x80DA). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x80A8, kSemanticVariant, 0x80EE). %<kMatthews
unicode_unihan_variant(0x80AC, kSemanticVariant, 0x75A3). %<kMatthews
unicode_unihan_variant(0x80AE, kTraditionalVariant, 0x9AAF).
unicode_unihan_variant(0x80AF, kZVariant, 0x80BB).
unicode_unihan_variant(0x80B1, kSemanticVariant, 0x53B7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x80B2, kSemanticVariant, 0x6BD3). %<kMatthews
unicode_unihan_variant(0x80B2, kZVariant, 0x6BD3).
unicode_unihan_variant(0x80B3, kSemanticVariant, 0x543B). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x80B4, kSemanticVariant, 0x991A). %<kLau
unicode_unihan_variant(0x80B4, kTraditionalVariant, 0x991A).
unicode_unihan_variant(0x80BB, kZVariant, 0x80AF).
unicode_unihan_variant(0x80BE, kTraditionalVariant, 0x814E).
unicode_unihan_variant(0x80BF, kTraditionalVariant, 0x816B).
unicode_unihan_variant(0x80C0, kTraditionalVariant, 0x8139).
unicode_unihan_variant(0x80C1, kTraditionalVariant, 0x8105).
unicode_unihan_variant(0x80C4, kSemanticVariant, 0x4F37). %<kMatthews 0x5191<kFenn
unicode_unihan_variant(0x80C4, kSpecializedSemanticVariant, 0x4F37). %<kMeyerWempe
unicode_unihan_variant(0x80C4, kZVariant, 0x5191).
unicode_unihan_variant(0x80C6, kSemanticVariant, 0x81BD). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x80C6, kTraditionalVariant, 0x81BD).
unicode_unihan_variant(0x80CA, kZVariant, 0x6710).
unicode_unihan_variant(0x80CC, kSpecializedSemanticVariant, 0x7D63). %<kFenn
unicode_unihan_variant(0x80D1, kSemanticVariant, 0x80A2). %<kMatthews
unicode_unihan_variant(0x80D3, kSemanticVariant, 0x26693).
unicode_unihan_variant(0x80D4, kSemanticVariant, 0x9AB4). %<kMatthews
unicode_unihan_variant(0x80D7, kSemanticVariant, 0x75B9). %<kMatthews
unicode_unihan_variant(0x80DA, kSemanticVariant, 0x80A7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x80DC, kTraditionalVariant, 0x52DD).
unicode_unihan_variant(0x80DD, kZVariant, 0x90C5).
unicode_unihan_variant(0x80DE, kZVariant, 0x812C).
unicode_unihan_variant(0x80E1, kTraditionalVariant, 0x885A). %0x9B0D
unicode_unihan_variant(0x80E1, kZVariant, 0x695C).
unicode_unihan_variant(0x80E7, kTraditionalVariant, 0x6727).
unicode_unihan_variant(0x80E8, kTraditionalVariant, 0x8156).
unicode_unihan_variant(0x80EA, kTraditionalVariant, 0x81DA).
unicode_unihan_variant(0x80EB, kTraditionalVariant, 0x811B).
unicode_unihan_variant(0x80ED, kSemanticVariant, 0x81D9). %<kLau,kMatthews
unicode_unihan_variant(0x80ED, kZVariant, 0x81D9).
unicode_unihan_variant(0x80EE, kSemanticVariant, 0x80A8). %<kMatthews
unicode_unihan_variant(0x80EF, kSemanticVariant, 0x9ABB). %<kMeyerWempe
unicode_unihan_variant(0x80F3, kSemanticVariant, 0x8090). %<kMatthews 0x29A18<kFenn
unicode_unihan_variant(0x80F3, kZVariant, 0x8090).
unicode_unihan_variant(0x80F6, kTraditionalVariant, 0x81A0).
unicode_unihan_variant(0x80F7, kSemanticVariant, 0x5308). %<kLau,kMatthews 0x80F8<kLau,kMatthews
unicode_unihan_variant(0x80F7, kZVariant, 0x80F8).
unicode_unihan_variant(0x80F8, kSemanticVariant, 0x5308). %<kLau,kMatthews,kMeyerWempe 0x80F7<kLau,kMatthews
unicode_unihan_variant(0x80F8, kZVariant, 0x80F7).
unicode_unihan_variant(0x80FC, kZVariant, 0x8141).
unicode_unihan_variant(0x8103, kSemanticVariant, 0x8106). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8105, kSemanticVariant, 0x8107). %<kMatthews
unicode_unihan_variant(0x8105, kSimplifiedVariant, 0x80C1).
unicode_unihan_variant(0x8105, kSpecializedSemanticVariant, 0x52F0). %<kMeyerWempe
unicode_unihan_variant(0x8106, kSemanticVariant, 0x8103). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8107, kSemanticVariant, 0x8105). %<kMatthews
unicode_unihan_variant(0x8107, kZVariant, 0x8105).
unicode_unihan_variant(0x8108, kSemanticVariant, 0x8847). %<kLau,kMatthews 0x8109<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8108, kSimplifiedVariant, 0x8109).
unicode_unihan_variant(0x8108, kSpecializedSemanticVariant, 0x8847). %<kFenn
unicode_unihan_variant(0x8109, kSemanticVariant, 0x4611). %<kMeyerWempe 0x8108<kLau,kMatthews,kMeyerWempe 0x8847<kLau,kMatthews
unicode_unihan_variant(0x8109, kSpecializedSemanticVariant, 0x8847). %<kFenn
unicode_unihan_variant(0x8109, kTraditionalVariant, 0x8108).
unicode_unihan_variant(0x810D, kTraditionalVariant, 0x81BE).
unicode_unihan_variant(0x810F, kTraditionalVariant, 0x81DF). %0x9AD2
unicode_unihan_variant(0x810F, kZVariant, 0x9AD2).
unicode_unihan_variant(0x8110, kTraditionalVariant, 0x81CD).
unicode_unihan_variant(0x8111, kTraditionalVariant, 0x8166).
unicode_unihan_variant(0x8113, kTraditionalVariant, 0x81BF).
unicode_unihan_variant(0x8114, kTraditionalVariant, 0x81E0).
unicode_unihan_variant(0x811A, kSemanticVariant, 0x8173). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x811A, kTraditionalVariant, 0x8173).
unicode_unihan_variant(0x811B, kSimplifiedVariant, 0x80EB).
unicode_unihan_variant(0x8123, kSemanticVariant, 0x5507). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x8123, kZVariant, 0x5507).
unicode_unihan_variant(0x8125, kSimplifiedVariant, 0x23370).
unicode_unihan_variant(0x8127, kZVariant, 0x6718).
unicode_unihan_variant(0x812A, kSemanticVariant, 0x3F99). %<kMatthews
unicode_unihan_variant(0x812B, kSimplifiedVariant, 0x8131).
unicode_unihan_variant(0x812C, kZVariant, 0x80DE).
unicode_unihan_variant(0x8131, kTraditionalVariant, 0x812B).
unicode_unihan_variant(0x8132, kZVariant, 0x5C3F).
unicode_unihan_variant(0x8133, kZVariant, 0x8166).
unicode_unihan_variant(0x8135, kZVariant, 0x80A1).
unicode_unihan_variant(0x8136, kTraditionalVariant, 0x8161).
unicode_unihan_variant(0x8138, kTraditionalVariant, 0x81C9).
unicode_unihan_variant(0x8139, kSemanticVariant, 0x75EE). %<kMatthews
unicode_unihan_variant(0x8139, kSimplifiedVariant, 0x80C0).
unicode_unihan_variant(0x8141, kZVariant, 0x80FC).
unicode_unihan_variant(0x814A, kSemanticVariant, 0x81C8). %<kLau,kMatthews 0x81D8<kLau,kMatthews
unicode_unihan_variant(0x814A, kTraditionalVariant, 0x81D8).
unicode_unihan_variant(0x814C, kSpecializedSemanticVariant, 0x9AAF). %<kFenn
unicode_unihan_variant(0x814E, kSimplifiedVariant, 0x80BE).
unicode_unihan_variant(0x8155, kSemanticVariant, 0x26675). %<kFenn
unicode_unihan_variant(0x8156, kSimplifiedVariant, 0x80E8).
unicode_unihan_variant(0x8158, kTraditionalVariant, 0x8195).
unicode_unihan_variant(0x8158, kZVariant, 0x8195).
unicode_unihan_variant(0x815F, kZVariant, 0x81A3).
unicode_unihan_variant(0x8161, kSimplifiedVariant, 0x8136).
unicode_unihan_variant(0x8162, kSemanticVariant, 0x9AC3). %<kMatthews
unicode_unihan_variant(0x8166, kSemanticVariant, 0x5318). %<kMatthews
unicode_unihan_variant(0x8166, kSimplifiedVariant, 0x8111).
unicode_unihan_variant(0x816A, kSimplifiedVariant, 0x2336F).
unicode_unihan_variant(0x816B, kSemanticVariant, 0x5C30). %<kMatthews
unicode_unihan_variant(0x816B, kSimplifiedVariant, 0x80BF).
unicode_unihan_variant(0x816D, kSemanticVariant, 0x9F76). %<kLau,kMatthews
unicode_unihan_variant(0x816D, kTraditionalVariant, 0x9F76).
unicode_unihan_variant(0x816E, kSemanticVariant, 0x984B). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8173, kSemanticVariant, 0x811A). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8173, kSimplifiedVariant, 0x811A).
unicode_unihan_variant(0x8178, kSimplifiedVariant, 0x80A0).
unicode_unihan_variant(0x8178, kZVariant, 0x8193).
unicode_unihan_variant(0x817B, kTraditionalVariant, 0x81A9).
unicode_unihan_variant(0x817C, kTraditionalVariant, 0x9766).
unicode_unihan_variant(0x817D, kTraditionalVariant, 0x8183).
unicode_unihan_variant(0x817E, kTraditionalVariant, 0x9A30).
unicode_unihan_variant(0x817F, kSemanticVariant, 0x9ABD). %<kMatthews
unicode_unihan_variant(0x8183, kSimplifiedVariant, 0x817D).
unicode_unihan_variant(0x8186, kSemanticVariant, 0x55C9). %<kMatthews
unicode_unihan_variant(0x818B, kSemanticVariant, 0x81AB). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8191, kTraditionalVariant, 0x81CF).
unicode_unihan_variant(0x8195, kSimplifiedVariant, 0x8158).
unicode_unihan_variant(0x8195, kZVariant, 0x8158).
unicode_unihan_variant(0x8198, kZVariant, 0x81D5).
unicode_unihan_variant(0x819A, kSimplifiedVariant, 0x80A4).
unicode_unihan_variant(0x81A0, kSimplifiedVariant, 0x80F6).
unicode_unihan_variant(0x81A2, kSimplifiedVariant, 0x2677C).
unicode_unihan_variant(0x81A3, kZVariant, 0x815F).
unicode_unihan_variant(0x81A9, kSimplifiedVariant, 0x817B).
unicode_unihan_variant(0x81AB, kSemanticVariant, 0x818B). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x81B3, kSemanticVariant, 0x994D). %<kLau,kMatthews
unicode_unihan_variant(0x81B8, kZVariant, 0x9AD3).
unicode_unihan_variant(0x81BB, kSpecializedSemanticVariant, 0x7FB6).
unicode_unihan_variant(0x81BD, kSemanticVariant, 0x80C6). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x81BD, kSimplifiedVariant, 0x80C6).
unicode_unihan_variant(0x81BE, kSemanticVariant, 0x9C60). %<kMatthews
unicode_unihan_variant(0x81BE, kSimplifiedVariant, 0x810D).
unicode_unihan_variant(0x81BE, kSpecializedSemanticVariant, 0x9C60). %<kMeyerWempe
unicode_unihan_variant(0x81BF, kSimplifiedVariant, 0x8113).
unicode_unihan_variant(0x81C7, kSemanticVariant, 0x243B1). %<kFenn
unicode_unihan_variant(0x81C7, kSpecializedSemanticVariant, 0x7E82). %<kFenn
unicode_unihan_variant(0x81C8, kSemanticVariant, 0x814A). %<kLau,kMatthews 0x81D8<kLau,kMatthews
unicode_unihan_variant(0x81C8, kZVariant, 0x81D8).
unicode_unihan_variant(0x81C9, kSimplifiedVariant, 0x8138).
unicode_unihan_variant(0x81CA, kSemanticVariant, 0x267A3). %<kHanYu:T
unicode_unihan_variant(0x81CD, kSimplifiedVariant, 0x8110).
unicode_unihan_variant(0x81CF, kSemanticVariant, 0x9AD5). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x81CF, kSimplifiedVariant, 0x8191).
unicode_unihan_variant(0x81CF, kZVariant, 0x9AD5).
unicode_unihan_variant(0x81D3, kZVariant, 0x81DF).
unicode_unihan_variant(0x81D5, kZVariant, 0x8198).
unicode_unihan_variant(0x81D7, kSimplifiedVariant, 0x23391).
unicode_unihan_variant(0x81D8, kSemanticVariant, 0x814A). %<kLau,kMatthews 0x81C8<kLau,kMatthews
unicode_unihan_variant(0x81D8, kSimplifiedVariant, 0x814A).
unicode_unihan_variant(0x81D9, kSemanticVariant, 0x80ED). %<kLau,kMatthews
unicode_unihan_variant(0x81D9, kZVariant, 0x80ED).
unicode_unihan_variant(0x81DA, kSimplifiedVariant, 0x80EA).
unicode_unihan_variant(0x81DC, kTraditionalVariant, 0x81E2).
unicode_unihan_variant(0x81DE, kSemanticVariant, 0x766F). %<kMeyerWempe
unicode_unihan_variant(0x81DF, kSimplifiedVariant, 0x810F).
unicode_unihan_variant(0x81E0, kSimplifiedVariant, 0x8114).
unicode_unihan_variant(0x81E2, kSimplifiedVariant, 0x81DC).
unicode_unihan_variant(0x81E5, kSemanticVariant, 0x5367). %<kHKGlyph
unicode_unihan_variant(0x81E8, kSimplifiedVariant, 0x4E34).
unicode_unihan_variant(0x81E8, kZVariant, 0xF9F6).
unicode_unihan_variant(0x81EF, kSemanticVariant, 0x768B). %<kMatthews
unicode_unihan_variant(0x81F4, kTraditionalVariant, 0x7DFB).
unicode_unihan_variant(0x81FA, kSemanticVariant, 0x53F0). %<kHKGlyph,kLau
unicode_unihan_variant(0x81FA, kSimplifiedVariant, 0x53F0).
unicode_unihan_variant(0x8200, kSemanticVariant, 0x628C). %<kMatthews
unicode_unihan_variant(0x8200, kSpecializedSemanticVariant, 0x5916). %<kFenn
unicode_unihan_variant(0x8203, kZVariant, 0x8204).
unicode_unihan_variant(0x8204, kZVariant, 0x8203).
unicode_unihan_variant(0x8206, kTraditionalVariant, 0x8F3F).
unicode_unihan_variant(0x8207, kSemanticVariant, 0x4E0E). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8207, kSimplifiedVariant, 0x4E0E).
unicode_unihan_variant(0x8208, kSemanticVariant, 0x34B7). %<kLau,kMatthews
unicode_unihan_variant(0x8208, kSimplifiedVariant, 0x5174).
unicode_unihan_variant(0x8208, kSpecializedSemanticVariant, 0x34B7). %<kFenn
unicode_unihan_variant(0x8209, kSemanticVariant, 0x3AAF). %<kFenn 0x64E7<kHKGlyph
unicode_unihan_variant(0x8209, kSimplifiedVariant, 0x4E3E).
unicode_unihan_variant(0x8209, kZVariant, 0x64E7).
unicode_unihan_variant(0x820A, kSemanticVariant, 0x65E7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x820A, kSimplifiedVariant, 0x65E7).
unicode_unihan_variant(0x820D, kSpecializedSemanticVariant, 0x6368). %<kMeyerWempe
unicode_unihan_variant(0x820D, kTraditionalVariant, 0x6368).
unicode_unihan_variant(0x820D, kZVariant, 0x820E).
unicode_unihan_variant(0x820E, kZVariant, 0x820D).
unicode_unihan_variant(0x8210, kSemanticVariant, 0x8213). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8212, kSpecializedSemanticVariant, 0x6474).
unicode_unihan_variant(0x8213, kSemanticVariant, 0x8210). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8214, kSemanticVariant, 0x9902). %<kFenn
unicode_unihan_variant(0x8216, kSemanticVariant, 0x92EA). %<kLau
unicode_unihan_variant(0x8216, kSpecializedSemanticVariant, 0x92EA). %<kMeyerWempe
unicode_unihan_variant(0x8216, kZVariant, 0x92EA).
unicode_unihan_variant(0x8217, kZVariant, 0x92EA).
unicode_unihan_variant(0x8218, kSemanticVariant, 0x9928). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8218, kZVariant, 0x9928).
unicode_unihan_variant(0x8221, kSemanticVariant, 0x8239). %<kMeyerWempe
unicode_unihan_variant(0x8221, kZVariant, 0x8239).
unicode_unihan_variant(0x8223, kTraditionalVariant, 0x8264).
unicode_unihan_variant(0x8226, kSemanticVariant, 0x825C). %<kMeyerWempe
unicode_unihan_variant(0x8229, kZVariant, 0x8239).
unicode_unihan_variant(0x822E, kZVariant, 0x6AD3).
unicode_unihan_variant(0x8230, kTraditionalVariant, 0x8266).
unicode_unihan_variant(0x8231, kTraditionalVariant, 0x8259).
unicode_unihan_variant(0x8235, kSemanticVariant, 0x67C1). %<kMeyerWempe
unicode_unihan_variant(0x8235, kZVariant, 0x67C1).
unicode_unihan_variant(0x8236, kZVariant, 0x824A).
unicode_unihan_variant(0x8239, kSemanticVariant, 0x8221). %<kMeyerWempe
unicode_unihan_variant(0x8239, kZVariant, 0x8229).
unicode_unihan_variant(0x823B, kTraditionalVariant, 0x826B).
unicode_unihan_variant(0x823B, kZVariant, 0x6AD3).
unicode_unihan_variant(0x824A, kZVariant, 0x8236).
unicode_unihan_variant(0x8251, kSpecializedSemanticVariant, 0x6241). %<kFenn
unicode_unihan_variant(0x8259, kSimplifiedVariant, 0x8231).
unicode_unihan_variant(0x825C, kSemanticVariant, 0x8226). %<kMeyerWempe
unicode_unihan_variant(0x8262, kSemanticVariant, 0x6AA3). %<kMatthews
unicode_unihan_variant(0x8263, kZVariant, 0x6AD3).
unicode_unihan_variant(0x8264, kSimplifiedVariant, 0x8223).
unicode_unihan_variant(0x8265, kSemanticVariant, 0x696B). %<kMatthews 0x6A9D<kMatthews
unicode_unihan_variant(0x8266, kSimplifiedVariant, 0x8230).
unicode_unihan_variant(0x826A, kSemanticVariant, 0x6A10). %<kMatthews 0x6AD3<kLau,kMatthews
unicode_unihan_variant(0x826A, kZVariant, 0x6AD3).
unicode_unihan_variant(0x826B, kSimplifiedVariant, 0x823B).
unicode_unihan_variant(0x826B, kZVariant, 0x6AD3).
unicode_unihan_variant(0x826F, kZVariant, 0xF97C).
unicode_unihan_variant(0x8270, kTraditionalVariant, 0x8271).
unicode_unihan_variant(0x8271, kSimplifiedVariant, 0x8270).
unicode_unihan_variant(0x8273, kTraditionalVariant, 0x8277).
unicode_unihan_variant(0x8273, kZVariant, 0x8C54).
unicode_unihan_variant(0x8274, kZVariant, 0x52C3).
unicode_unihan_variant(0x8276, kSemanticVariant, 0x8C53). %<kMatthews 0x8C54<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x8277, kSimplifiedVariant, 0x8273).
unicode_unihan_variant(0x8277, kZVariant, 0x8C54).
unicode_unihan_variant(0x8278, kSemanticVariant, 0x8349). %<kMatthews 0x8279<kMatthews
unicode_unihan_variant(0x8279, kSemanticVariant, 0x8278). %<kMatthews 0x8349<kMatthews
unicode_unihan_variant(0x827A, kTraditionalVariant, 0x85DD).
unicode_unihan_variant(0x827D, kZVariant, 0x97ED).
unicode_unihan_variant(0x8282, kTraditionalVariant, 0x7BC0).
unicode_unihan_variant(0x8288, kTraditionalVariant, 0x7F8B).
unicode_unihan_variant(0x828A, kZVariant, 0x831C).
unicode_unihan_variant(0x8292, kSemanticVariant, 0x6767). %<kLau
unicode_unihan_variant(0x8294, kSemanticVariant, 0x5349). %<kMatthews
unicode_unihan_variant(0x8297, kTraditionalVariant, 0x858C).
unicode_unihan_variant(0x8298, kZVariant, 0x853D).
unicode_unihan_variant(0x829C, kTraditionalVariant, 0x856A).
unicode_unihan_variant(0x829F, kSemanticVariant, 0x206B9). %<kMeyerWempe
unicode_unihan_variant(0x82A6, kSemanticVariant, 0x8606). %<kFenn
unicode_unihan_variant(0x82A6, kTraditionalVariant, 0x8606).
unicode_unihan_variant(0x82AD, kZVariant, 0x5DF4).
unicode_unihan_variant(0x82B1, kSemanticVariant, 0x8624). %<kLau
unicode_unihan_variant(0x82B8, kTraditionalVariant, 0x8553).
unicode_unihan_variant(0x82B8, kZVariant, 0x85DD).
unicode_unihan_variant(0x82BB, kSemanticVariant, 0x84AD). %<kMatthews,kMeyerWempe 0x26DDD
unicode_unihan_variant(0x82BB, kSimplifiedVariant, 0x520D).
unicode_unihan_variant(0x82C1, kTraditionalVariant, 0x84EF).
unicode_unihan_variant(0x82C5, kZVariant, 0x5208).
unicode_unihan_variant(0x82C7, kTraditionalVariant, 0x8466).
unicode_unihan_variant(0x82C8, kTraditionalVariant, 0x85F6).
unicode_unihan_variant(0x82CB, kTraditionalVariant, 0x83A7).
unicode_unihan_variant(0x82CC, kTraditionalVariant, 0x8407).
unicode_unihan_variant(0x82CD, kTraditionalVariant, 0x84BC).
unicode_unihan_variant(0x82CE, kTraditionalVariant, 0x82E7).
unicode_unihan_variant(0x82CE, kZVariant, 0x8499).
unicode_unihan_variant(0x82CF, kTraditionalVariant, 0x8607).
unicode_unihan_variant(0x82D1, kZVariant, 0x83C0).
unicode_unihan_variant(0x82D5, kSemanticVariant, 0x4492). %<kMatthews
unicode_unihan_variant(0x82DB, kSpecializedSemanticVariant, 0x54FC). %<kFenn
unicode_unihan_variant(0x82DF, kSemanticVariant, 0x830D). %<kMatthews
unicode_unihan_variant(0x82E1, kSemanticVariant, 0x82E2). %<kMatthews
unicode_unihan_variant(0x82E2, kSemanticVariant, 0x82E1). %<kMatthews
unicode_unihan_variant(0x82E4, kSemanticVariant, 0x77A5). %<kMatthews
unicode_unihan_variant(0x82E5, kZVariant, 0xF974).
unicode_unihan_variant(0x82E7, kSimplifiedVariant, 0x82CE).
unicode_unihan_variant(0x82E7, kTraditionalVariant, 0x85B4).
unicode_unihan_variant(0x82F9, kTraditionalVariant, 0x860B).
unicode_unihan_variant(0x82FA, kSemanticVariant, 0x8393). %<kMatthews
unicode_unihan_variant(0x82FA, kZVariant, 0x8393).
unicode_unihan_variant(0x82FD, kZVariant, 0x83C7).
unicode_unihan_variant(0x82FF, kZVariant, 0x83CB).
unicode_unihan_variant(0x8303, kTraditionalVariant, 0x7BC4).
unicode_unihan_variant(0x8305, kZVariant, 0x6CD6).
unicode_unihan_variant(0x8306, kZVariant, 0x8305).
unicode_unihan_variant(0x8308, kZVariant, 0x67F4).
unicode_unihan_variant(0x830D, kSemanticVariant, 0x82DF). %<kMatthews
unicode_unihan_variant(0x830E, kTraditionalVariant, 0x8396).
unicode_unihan_variant(0x830F, kTraditionalVariant, 0x8622).
unicode_unihan_variant(0x8311, kTraditionalVariant, 0x8526).
unicode_unihan_variant(0x8314, kTraditionalVariant, 0x584B).
unicode_unihan_variant(0x8315, kTraditionalVariant, 0x7162).
unicode_unihan_variant(0x8318, kZVariant, 0x8354).
unicode_unihan_variant(0x8320, kSemanticVariant, 0x8585). %<kMatthews
unicode_unihan_variant(0x8327, kTraditionalVariant, 0x7E6D).
unicode_unihan_variant(0x832D, kZVariant, 0x6912).
unicode_unihan_variant(0x8332, kSemanticVariant, 0x5179). %<kMatthews 0x7386<kMatthews
unicode_unihan_variant(0x8332, kSimplifiedVariant, 0x5179).
unicode_unihan_variant(0x8332, kZVariant, 0x7386).
unicode_unihan_variant(0x8336, kZVariant, 0xF9FE).
unicode_unihan_variant(0x8345, kSemanticVariant, 0x7B54). %<kLau,kMeyerWempe
unicode_unihan_variant(0x8345, kZVariant, 0x7B54).
unicode_unihan_variant(0x8346, kSemanticVariant, 0x834A). %<kMatthews
unicode_unihan_variant(0x8346, kTraditionalVariant, 0x834A).
unicode_unihan_variant(0x8347, kSemanticVariant, 0x8395). %<kMeyerWempe
unicode_unihan_variant(0x8349, kSemanticVariant, 0x8278). %<kMatthews 0x8279<kMatthews
unicode_unihan_variant(0x834A, kSemanticVariant, 0x8346). %<kMatthews
unicode_unihan_variant(0x834A, kSimplifiedVariant, 0x8346).
unicode_unihan_variant(0x834D, kSemanticVariant, 0x854E). %<kMatthews
unicode_unihan_variant(0x8350, kSemanticVariant, 0x85A6). %<kHKGlyph,kMeyerWempe
unicode_unihan_variant(0x8350, kTraditionalVariant, 0x85A6).
unicode_unihan_variant(0x8354, kSemanticVariant, 0x681B). %<kMatthews
unicode_unihan_variant(0x8358, kZVariant, 0x838A).
unicode_unihan_variant(0x8359, kTraditionalVariant, 0x8598).
unicode_unihan_variant(0x835A, kTraditionalVariant, 0x83A2).
unicode_unihan_variant(0x835B, kTraditionalVariant, 0x8558).
unicode_unihan_variant(0x835C, kTraditionalVariant, 0x84FD).
unicode_unihan_variant(0x835D, kTraditionalVariant, 0x8434).
unicode_unihan_variant(0x835D, kZVariant, 0x8434).
unicode_unihan_variant(0x835E, kTraditionalVariant, 0x854E).
unicode_unihan_variant(0x835F, kTraditionalVariant, 0x8588).
unicode_unihan_variant(0x8360, kTraditionalVariant, 0x85BA).
unicode_unihan_variant(0x8361, kTraditionalVariant, 0x76EA). %0x8569
unicode_unihan_variant(0x8363, kSemanticVariant, 0x69AE). %<kMeyerWempe
unicode_unihan_variant(0x8363, kTraditionalVariant, 0x69AE).
unicode_unihan_variant(0x8364, kTraditionalVariant, 0x8477).
unicode_unihan_variant(0x8365, kTraditionalVariant, 0x6ECE).
unicode_unihan_variant(0x8366, kTraditionalVariant, 0x7296).
unicode_unihan_variant(0x8367, kTraditionalVariant, 0x7192).
unicode_unihan_variant(0x8368, kTraditionalVariant, 0x8541).
unicode_unihan_variant(0x8369, kTraditionalVariant, 0x85CE).
unicode_unihan_variant(0x836A, kTraditionalVariant, 0x84C0).
unicode_unihan_variant(0x836B, kTraditionalVariant, 0x852D).
unicode_unihan_variant(0x836C, kTraditionalVariant, 0x8552).
unicode_unihan_variant(0x836D, kTraditionalVariant, 0x8452).
unicode_unihan_variant(0x836E, kTraditionalVariant, 0x8464).
unicode_unihan_variant(0x836F, kTraditionalVariant, 0x85E5).
unicode_unihan_variant(0x8373, kSemanticVariant, 0x8C46). %<kLau
unicode_unihan_variant(0x837D, kZVariant, 0x840E).
unicode_unihan_variant(0x8380, kSemanticVariant, 0x8FB2). %<kMatthews
unicode_unihan_variant(0x8385, kSemanticVariant, 0x6D96). %<kMatthews 0x849E<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8385, kTraditionalVariant, 0x849E).
unicode_unihan_variant(0x8386, kZVariant, 0x84B2).
unicode_unihan_variant(0x838A, kSemanticVariant, 0x5E84). %<kLau
unicode_unihan_variant(0x838A, kSimplifiedVariant, 0x5E84).
unicode_unihan_variant(0x838A, kZVariant, 0x8358).
unicode_unihan_variant(0x8393, kSemanticVariant, 0x82FA). %<kMatthews
unicode_unihan_variant(0x8393, kZVariant, 0x82FA).
unicode_unihan_variant(0x8395, kSemanticVariant, 0x8347). %<kMeyerWempe
unicode_unihan_variant(0x8396, kSimplifiedVariant, 0x830E).
unicode_unihan_variant(0x839C, kSemanticVariant, 0x84E7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x83A2, kSimplifiedVariant, 0x835A).
unicode_unihan_variant(0x83A7, kSimplifiedVariant, 0x82CB).
unicode_unihan_variant(0x83A9, kSpecializedSemanticVariant, 0x6B8D). %<kMeyerWempe
unicode_unihan_variant(0x83B1, kTraditionalVariant, 0x840A).
unicode_unihan_variant(0x83B2, kTraditionalVariant, 0x84EE).
unicode_unihan_variant(0x83B3, kTraditionalVariant, 0x8494).
unicode_unihan_variant(0x83B4, kTraditionalVariant, 0x8435).
unicode_unihan_variant(0x83B5, kZVariant, 0x83DF).
unicode_unihan_variant(0x83B6, kTraditionalVariant, 0x859F).
unicode_unihan_variant(0x83B7, kTraditionalVariant, 0x7372). %0x7A6B
unicode_unihan_variant(0x83B7, kZVariant, 0x7A6B).
unicode_unihan_variant(0x83B8, kTraditionalVariant, 0x8555).
unicode_unihan_variant(0x83B9, kTraditionalVariant, 0x7469).
unicode_unihan_variant(0x83BA, kTraditionalVariant, 0x9DAF).
unicode_unihan_variant(0x83BC, kTraditionalVariant, 0x84F4).
unicode_unihan_variant(0x83BC, kZVariant, 0x8493).
unicode_unihan_variant(0x83BF, kSemanticVariant, 0x673F). %<kMatthews
unicode_unihan_variant(0x83BF, kSpecializedSemanticVariant, 0x523A). %<kMeyerWempe
unicode_unihan_variant(0x83C0, kZVariant, 0x82D1).
unicode_unihan_variant(0x83C7, kSemanticVariant, 0x83F0). %<kMeyerWempe
unicode_unihan_variant(0x83C7, kZVariant, 0x82FD).
unicode_unihan_variant(0x83CB, kZVariant, 0x82FF).
unicode_unihan_variant(0x83CC, kSemanticVariant, 0x8548). %<kMatthews
unicode_unihan_variant(0x83CC, kSpecializedSemanticVariant, 0x6EFE). %<kMeyerWempe
unicode_unihan_variant(0x83D1, kSemanticVariant, 0x707D). %<kFenn 0x707E<kFenn 0x70D6<kFenn 0x753E<kMatthews
unicode_unihan_variant(0x83D3, kSemanticVariant, 0x679C). %<kFenn
unicode_unihan_variant(0x83D4, kSemanticVariant, 0x8514). %<kMeyerWempe
unicode_unihan_variant(0x83D4, kZVariant, 0x8514).
unicode_unihan_variant(0x83DD, kZVariant, 0x853D).
unicode_unihan_variant(0x83DF, kZVariant, 0x83B5).
unicode_unihan_variant(0x83E2, kSemanticVariant, 0x52FD). %<kMatthews
unicode_unihan_variant(0x83EB, kSemanticVariant, 0x5807). %<kMatthews
unicode_unihan_variant(0x83EB, kZVariant, 0x5807).
unicode_unihan_variant(0x83EF, kSimplifiedVariant, 0x534E).
unicode_unihan_variant(0x83EF, kSpecializedSemanticVariant, 0x8550). %<kFenn
unicode_unihan_variant(0x83EF, kZVariant, 0x5D0B).
unicode_unihan_variant(0x83F0, kSemanticVariant, 0x83C7). %<kMeyerWempe
unicode_unihan_variant(0x83F0, kZVariant, 0x82FD).
unicode_unihan_variant(0x83F1, kZVariant, 0xF958).
unicode_unihan_variant(0x83F4, kSemanticVariant, 0x5EB5). %<kHKGlyph,kMatthews
unicode_unihan_variant(0x83F7, kZVariant, 0x5E1A).
unicode_unihan_variant(0x83F8, kSemanticVariant, 0x70DF). %<kFenn 0x7159
unicode_unihan_variant(0x83F8, kSpecializedSemanticVariant, 0x7159).
unicode_unihan_variant(0x83FB, kZVariant, 0x9EBB).
unicode_unihan_variant(0x8405, kZVariant, 0x6625).
unicode_unihan_variant(0x8406, kSemanticVariant, 0x84D6). %<kMatthews
unicode_unihan_variant(0x8406, kZVariant, 0x853D).
unicode_unihan_variant(0x8407, kSimplifiedVariant, 0x82CC).
unicode_unihan_variant(0x840A, kSimplifiedVariant, 0x83B1).
unicode_unihan_variant(0x840C, kZVariant, 0x8420).
unicode_unihan_variant(0x840E, kZVariant, 0x837D).
unicode_unihan_variant(0x841A, kZVariant, 0x8600).
unicode_unihan_variant(0x841D, kTraditionalVariant, 0x863F).
unicode_unihan_variant(0x8420, kZVariant, 0x840C).
unicode_unihan_variant(0x8424, kTraditionalVariant, 0x87A2).
unicode_unihan_variant(0x8425, kTraditionalVariant, 0x71DF).
unicode_unihan_variant(0x8426, kTraditionalVariant, 0x7E08).
unicode_unihan_variant(0x8427, kTraditionalVariant, 0x856D).
unicode_unihan_variant(0x8428, kTraditionalVariant, 0x85A9).
unicode_unihan_variant(0x842C, kSemanticVariant, 0x4E07). %<kLau,kMatthews,kMeyerWempe 0x534D<kFenn
unicode_unihan_variant(0x842C, kSimplifiedVariant, 0x4E07).
unicode_unihan_variant(0x8431, kSemanticVariant, 0x8432). %<kMatthews
unicode_unihan_variant(0x8432, kSemanticVariant, 0x8431). %<kMatthews
unicode_unihan_variant(0x8434, kSimplifiedVariant, 0x835D).
unicode_unihan_variant(0x8435, kSimplifiedVariant, 0x83B4).
unicode_unihan_variant(0x843C, kZVariant, 0x855A).
unicode_unihan_variant(0x843D, kZVariant, 0xF918).
unicode_unihan_variant(0x8441, kSemanticVariant, 0x8591). %<kMatthews
unicode_unihan_variant(0x8449, kSimplifiedVariant, 0x53F6).
unicode_unihan_variant(0x8449, kZVariant, 0xF96E).
unicode_unihan_variant(0x8452, kSimplifiedVariant, 0x836D).
unicode_unihan_variant(0x8457, kSemanticVariant, 0x7740).
unicode_unihan_variant(0x8457, kSimplifiedVariant, 0x7740).
unicode_unihan_variant(0x8457, kSpecializedSemanticVariant, 0x7740). %<kFenn 0x87AB<kFenn
unicode_unihan_variant(0x845A, kSemanticVariant, 0x6939). %<kFenn
unicode_unihan_variant(0x8462, kSemanticVariant, 0x76D6). %<kFenn 0x84CB<kLau,kMeyerWempe
unicode_unihan_variant(0x8462, kZVariant, 0x84CB).
unicode_unihan_variant(0x8464, kSimplifiedVariant, 0x836E).
unicode_unihan_variant(0x8466, kSimplifiedVariant, 0x82C7).
unicode_unihan_variant(0x846B, kZVariant, 0x80E1).
unicode_unihan_variant(0x846C, kSemanticVariant, 0x585F). %<kMatthews
unicode_unihan_variant(0x846C, kZVariant, 0x585F).
unicode_unihan_variant(0x846F, kSemanticVariant, 0x85E5). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8471, kSemanticVariant, 0x8525). %<kLau
unicode_unihan_variant(0x8471, kTraditionalVariant, 0x8525).
unicode_unihan_variant(0x8477, kSimplifiedVariant, 0x8364).
unicode_unihan_variant(0x8487, kTraditionalVariant, 0x8546).
unicode_unihan_variant(0x8489, kTraditionalVariant, 0x8562).
unicode_unihan_variant(0x848B, kTraditionalVariant, 0x8523).
unicode_unihan_variant(0x848C, kTraditionalVariant, 0x851E).
unicode_unihan_variant(0x848D, kZVariant, 0x853F).
unicode_unihan_variant(0x8493, kSemanticVariant, 0x84F4). %<kMatthews
unicode_unihan_variant(0x8493, kZVariant, 0x84F4).
unicode_unihan_variant(0x8494, kSimplifiedVariant, 0x83B3).
unicode_unihan_variant(0x8499, kZVariant, 0x61DE).
unicode_unihan_variant(0x849E, kSemanticVariant, 0x6D96). %<kMatthews 0x8385<kMatthews,kMeyerWempe
unicode_unihan_variant(0x849E, kSimplifiedVariant, 0x8385).
unicode_unihan_variant(0x84AD, kSemanticVariant, 0x82BB). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x84B2, kZVariant, 0x8386).
unicode_unihan_variant(0x84B8, kSemanticVariant, 0x70DD). %<kFenn
unicode_unihan_variant(0x84BC, kSimplifiedVariant, 0x82CD).
unicode_unihan_variant(0x84C0, kSimplifiedVariant, 0x836A).
unicode_unihan_variant(0x84C6, kZVariant, 0x5E2D).
unicode_unihan_variant(0x84CB, kSemanticVariant, 0x76D6). %<kLau,kMatthews,kMeyerWempe 0x8462<kLau,kMeyerWempe
unicode_unihan_variant(0x84CB, kSimplifiedVariant, 0x76D6).
unicode_unihan_variant(0x84D6, kSemanticVariant, 0x8406). %<kMatthews
unicode_unihan_variant(0x84D6, kZVariant, 0x8298).
unicode_unihan_variant(0x84DD, kTraditionalVariant, 0x85CD).
unicode_unihan_variant(0x84DF, kTraditionalVariant, 0x858A).
unicode_unihan_variant(0x84E0, kTraditionalVariant, 0x863A).
unicode_unihan_variant(0x84E1, kZVariant, 0x53C3).
unicode_unihan_variant(0x84E3, kTraditionalVariant, 0x8577).
unicode_unihan_variant(0x84E5, kTraditionalVariant, 0x93A3).
unicode_unihan_variant(0x84E6, kTraditionalVariant, 0x9A40).
unicode_unihan_variant(0x84E7, kSemanticVariant, 0x839C). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x84EE, kSimplifiedVariant, 0x83B2).
unicode_unihan_variant(0x84EF, kSimplifiedVariant, 0x82C1).
unicode_unihan_variant(0x84F4, kSemanticVariant, 0x8493). %<kMatthews
unicode_unihan_variant(0x84F4, kSimplifiedVariant, 0x83BC).
unicode_unihan_variant(0x84F4, kZVariant, 0x8493).
unicode_unihan_variant(0x84FA, kSemanticVariant, 0x57F6). %<kMatthews 0x79C7<kMatthews 0x85DD<kLau,kMatthews
unicode_unihan_variant(0x84FC, kZVariant, 0xF9C2).
unicode_unihan_variant(0x84FD, kSimplifiedVariant, 0x835C).
unicode_unihan_variant(0x8502, kTraditionalVariant, 0x8646).
unicode_unihan_variant(0x8514, kSemanticVariant, 0x83D4). %<kMeyerWempe
unicode_unihan_variant(0x8514, kZVariant, 0x83D4).
unicode_unihan_variant(0x851A, kSemanticVariant, 0x5D8E). %<kMatthews
unicode_unihan_variant(0x851E, kSimplifiedVariant, 0x848C).
unicode_unihan_variant(0x851E, kSpecializedSemanticVariant, 0x8938). %<kMeyerWempe
unicode_unihan_variant(0x8523, kSimplifiedVariant, 0x848B).
unicode_unihan_variant(0x8525, kSemanticVariant, 0x8471). %<kLau
unicode_unihan_variant(0x8525, kSimplifiedVariant, 0x8471).
unicode_unihan_variant(0x8526, kSimplifiedVariant, 0x8311).
unicode_unihan_variant(0x852D, kSemanticVariant, 0x5ED5). %<kMatthews
unicode_unihan_variant(0x852D, kSimplifiedVariant, 0x836B).
unicode_unihan_variant(0x8534, kSemanticVariant, 0x9EBB).
unicode_unihan_variant(0x8534, kSpecializedSemanticVariant, 0x9EBB). %<kMeyerWempe
unicode_unihan_variant(0x8535, kZVariant, 0x85CF).
unicode_unihan_variant(0x8537, kTraditionalVariant, 0x8594).
unicode_unihan_variant(0x8539, kTraditionalVariant, 0x861E).
unicode_unihan_variant(0x853A, kTraditionalVariant, 0x85FA).
unicode_unihan_variant(0x853C, kTraditionalVariant, 0x85F9).
unicode_unihan_variant(0x853D, kZVariant, 0x8298).
unicode_unihan_variant(0x853F, kZVariant, 0x848D).
unicode_unihan_variant(0x8541, kSimplifiedVariant, 0x8368).
unicode_unihan_variant(0x8546, kSimplifiedVariant, 0x8487).
unicode_unihan_variant(0x8548, kSemanticVariant, 0x83CC). %<kMatthews
unicode_unihan_variant(0x854A, kSemanticVariant, 0x854B). %<kMatthews
unicode_unihan_variant(0x854A, kZVariant, 0x8602).
unicode_unihan_variant(0x854B, kSemanticVariant, 0x854A). %<kMatthews
unicode_unihan_variant(0x854B, kZVariant, 0x854A).
unicode_unihan_variant(0x854E, kSemanticVariant, 0x834D). %<kMatthews
unicode_unihan_variant(0x854E, kSimplifiedVariant, 0x835E).
unicode_unihan_variant(0x8550, kSpecializedSemanticVariant, 0x83EF). %<kFenn
unicode_unihan_variant(0x8552, kSimplifiedVariant, 0x836C).
unicode_unihan_variant(0x8553, kSimplifiedVariant, 0x82B8).
unicode_unihan_variant(0x8555, kSimplifiedVariant, 0x83B8).
unicode_unihan_variant(0x8558, kSimplifiedVariant, 0x835B).
unicode_unihan_variant(0x855A, kZVariant, 0x843C).
unicode_unihan_variant(0x8562, kSemanticVariant, 0x7C23). %<kMeyerWempe 0x7C44<kMeyerWempe
unicode_unihan_variant(0x8562, kSimplifiedVariant, 0x8489).
unicode_unihan_variant(0x8563, kSemanticVariant, 0x6A53). %<kMeyerWempe
unicode_unihan_variant(0x8564, kSemanticVariant, 0x7524). %<kMatthews
unicode_unihan_variant(0x8569, kSemanticVariant, 0x5052). %0x76EA 0x862F
unicode_unihan_variant(0x8569, kSimplifiedVariant, 0x8361).
unicode_unihan_variant(0x856A, kSimplifiedVariant, 0x829C).
unicode_unihan_variant(0x856D, kSimplifiedVariant, 0x8427).
unicode_unihan_variant(0x8570, kTraditionalVariant, 0x8580).
unicode_unihan_variant(0x8572, kTraditionalVariant, 0x8604).
unicode_unihan_variant(0x8574, kTraditionalVariant, 0x860A).
unicode_unihan_variant(0x8574, kZVariant, 0x860A).
unicode_unihan_variant(0x8577, kSimplifiedVariant, 0x84E3).
unicode_unihan_variant(0x8580, kSimplifiedVariant, 0x8570).
unicode_unihan_variant(0x8585, kSemanticVariant, 0x8320). %<kMatthews
unicode_unihan_variant(0x8588, kSimplifiedVariant, 0x835F).
unicode_unihan_variant(0x858A, kSimplifiedVariant, 0x84DF).
unicode_unihan_variant(0x858C, kSimplifiedVariant, 0x8297).
unicode_unihan_variant(0x8591, kSemanticVariant, 0x8441). %<kMatthews
unicode_unihan_variant(0x8591, kZVariant, 0x59DC).
unicode_unihan_variant(0x8593, kSpecializedSemanticVariant, 0x53C3). %<kFenn
unicode_unihan_variant(0x8594, kSimplifiedVariant, 0x8537).
unicode_unihan_variant(0x8598, kSimplifiedVariant, 0x8359).
unicode_unihan_variant(0x859F, kSemanticVariant, 0x861E). %<kMeyerWempe
unicode_unihan_variant(0x859F, kSimplifiedVariant, 0x83B6).
unicode_unihan_variant(0x85A6, kSemanticVariant, 0x8350). %<kHKGlyph,kMeyerWempe
unicode_unihan_variant(0x85A6, kSimplifiedVariant, 0x8350).
unicode_unihan_variant(0x85A9, kSimplifiedVariant, 0x8428).
unicode_unihan_variant(0x85AB, kZVariant, 0x85B0).
unicode_unihan_variant(0x85AC, kZVariant, 0x85E5).
unicode_unihan_variant(0x85AE, kTraditionalVariant, 0x85EA).
unicode_unihan_variant(0x85AF, kSemanticVariant, 0x85F7). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x85B0, kZVariant, 0x85AB).
unicode_unihan_variant(0x85B3, kSimplifiedVariant, 0x44D5).
unicode_unihan_variant(0x85B4, kSimplifiedVariant, 0x82E7).
unicode_unihan_variant(0x85BA, kSimplifiedVariant, 0x8360).
unicode_unihan_variant(0x85BA, kZVariant, 0x8415).
unicode_unihan_variant(0x85C7, kSemanticVariant, 0x288E6). %<kMeyerWempe
unicode_unihan_variant(0x85C9, kSemanticVariant, 0x8024). %<kMatthews
unicode_unihan_variant(0x85C9, kZVariant, 0x501F).
unicode_unihan_variant(0x85CD, kSimplifiedVariant, 0x84DD).
unicode_unihan_variant(0x85CE, kSimplifiedVariant, 0x8369).
unicode_unihan_variant(0x85CF, kZVariant, 0x8535).
unicode_unihan_variant(0x85D3, kTraditionalVariant, 0x861A).
unicode_unihan_variant(0x85DD, kSemanticVariant, 0x57F6). %<kMatthews 0x79C7<kMatthews 0x84FA<kLau,kMatthews
unicode_unihan_variant(0x85DD, kSimplifiedVariant, 0x827A).
unicode_unihan_variant(0x85DD, kZVariant, 0x517F).
unicode_unihan_variant(0x85DF, kSemanticVariant, 0x863D). %<kMeyerWempe
unicode_unihan_variant(0x85E4, kSemanticVariant, 0x7C50). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x85E5, kSemanticVariant, 0x846F). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x85E5, kSimplifiedVariant, 0x836F).
unicode_unihan_variant(0x85E5, kZVariant, 0x85AC).
unicode_unihan_variant(0x85E6, kSemanticVariant, 0x8611). %<kFenn
unicode_unihan_variant(0x85EA, kSimplifiedVariant, 0x85AE).
unicode_unihan_variant(0x85EA, kZVariant, 0x7C54).
unicode_unihan_variant(0x85F4, kZVariant, 0x860A).
unicode_unihan_variant(0x85F6, kSimplifiedVariant, 0x82C8).
unicode_unihan_variant(0x85F7, kSemanticVariant, 0x85AF). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x85F9, kSimplifiedVariant, 0x853C).
unicode_unihan_variant(0x85FA, kSimplifiedVariant, 0x853A).
unicode_unihan_variant(0x8600, kZVariant, 0x841A).
unicode_unihan_variant(0x8602, kZVariant, 0x854A).
unicode_unihan_variant(0x8604, kSimplifiedVariant, 0x8572).
unicode_unihan_variant(0x8606, kSemanticVariant, 0x82A6). %<kFenn
unicode_unihan_variant(0x8606, kSimplifiedVariant, 0x82A6).
unicode_unihan_variant(0x8606, kZVariant, 0xF935).
unicode_unihan_variant(0x8607, kSimplifiedVariant, 0x82CF).
unicode_unihan_variant(0x860A, kSimplifiedVariant, 0x8574).
unicode_unihan_variant(0x860B, kSimplifiedVariant, 0x82F9).
unicode_unihan_variant(0x860F, kZVariant, 0x8614).
unicode_unihan_variant(0x8611, kSemanticVariant, 0x85E6). %<kFenn
unicode_unihan_variant(0x8613, kZVariant, 0x8607).
unicode_unihan_variant(0x8614, kZVariant, 0x860F).
unicode_unihan_variant(0x8616, kSemanticVariant, 0x6AF1). %<kMatthews
unicode_unihan_variant(0x8616, kTraditionalVariant, 0x6AF1).
unicode_unihan_variant(0x8616, kZVariant, 0x8617).
unicode_unihan_variant(0x8617, kZVariant, 0x6A98).
unicode_unihan_variant(0x861A, kSimplifiedVariant, 0x85D3).
unicode_unihan_variant(0x861E, kSemanticVariant, 0x859F). %<kMeyerWempe
unicode_unihan_variant(0x861E, kSimplifiedVariant, 0x8539).
unicode_unihan_variant(0x8622, kSimplifiedVariant, 0x830F).
unicode_unihan_variant(0x8624, kSemanticVariant, 0x82B1). %<kLau
unicode_unihan_variant(0x862D, kSimplifiedVariant, 0x5170).
unicode_unihan_variant(0x862F, kSemanticVariant, 0x76EA). %0x8569
unicode_unihan_variant(0x863A, kSimplifiedVariant, 0x84E0).
unicode_unihan_variant(0x863D, kSemanticVariant, 0x85DF). %<kMeyerWempe
unicode_unihan_variant(0x863F, kSimplifiedVariant, 0x841D).
unicode_unihan_variant(0x863F, kZVariant, 0xF910).
unicode_unihan_variant(0x8646, kSimplifiedVariant, 0x8502).
unicode_unihan_variant(0x864E, kZVariant, 0x4E55).
unicode_unihan_variant(0x864F, kTraditionalVariant, 0x865C).
unicode_unihan_variant(0x8650, kSemanticVariant, 0x4588). %<kHanYu
unicode_unihan_variant(0x8650, kZVariant, 0x4E47).
unicode_unihan_variant(0x8651, kTraditionalVariant, 0x616E).
unicode_unihan_variant(0x8653, kSemanticVariant, 0x552C). %<kMeyerWempe 0x7307<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8654, kSemanticVariant, 0x458D). %<kMatthews
unicode_unihan_variant(0x8655, kSemanticVariant, 0x458F). %<kMatthews 0x51E6<kMatthews
unicode_unihan_variant(0x8655, kSimplifiedVariant, 0x5904).
unicode_unihan_variant(0x8655, kSpecializedSemanticVariant, 0x458F). %<kFenn 0x51E6<kFenn
unicode_unihan_variant(0x8655, kZVariant, 0x51E6).
unicode_unihan_variant(0x8656, kZVariant, 0x547C).
unicode_unihan_variant(0x865A, kTraditionalVariant, 0x865B).
unicode_unihan_variant(0x865B, kSimplifiedVariant, 0x865A).
unicode_unihan_variant(0x865C, kSemanticVariant, 0x64C4). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x865C, kSimplifiedVariant, 0x864F).
unicode_unihan_variant(0x865C, kSpecializedSemanticVariant, 0x64C4). %<kFenn
unicode_unihan_variant(0x865C, kZVariant, 0xF936).
unicode_unihan_variant(0x865E, kSemanticVariant, 0x299A2). %<kMeyerWempe
unicode_unihan_variant(0x865F, kSemanticVariant, 0x53F7). %<kLau,kMatthews
unicode_unihan_variant(0x865F, kSimplifiedVariant, 0x53F7).
unicode_unihan_variant(0x865F, kSpecializedSemanticVariant, 0x53F7). %<kFenn
unicode_unihan_variant(0x8663, kSemanticVariant, 0x66B4). %<kMatthews
unicode_unihan_variant(0x8667, kSimplifiedVariant, 0x4E8F).
unicode_unihan_variant(0x866B, kSemanticVariant, 0x87F2). %<kMeyerWempe
unicode_unihan_variant(0x866B, kTraditionalVariant, 0x87F2).
unicode_unihan_variant(0x866C, kSemanticVariant, 0x86AA). %<kMeyerWempe 0x866F<kMatthews
unicode_unihan_variant(0x866C, kTraditionalVariant, 0x866F).
unicode_unihan_variant(0x866E, kTraditionalVariant, 0x87E3).
unicode_unihan_variant(0x866F, kSemanticVariant, 0x866C). %<kMatthews
unicode_unihan_variant(0x866F, kSimplifiedVariant, 0x866C).
unicode_unihan_variant(0x8671, kSemanticVariant, 0x8768). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x867A, kZVariant, 0x8770).
unicode_unihan_variant(0x867D, kTraditionalVariant, 0x96D6).
unicode_unihan_variant(0x867E, kTraditionalVariant, 0x8766).
unicode_unihan_variant(0x867F, kTraditionalVariant, 0x8806).
unicode_unihan_variant(0x8680, kTraditionalVariant, 0x8755).
unicode_unihan_variant(0x8681, kTraditionalVariant, 0x87FB).
unicode_unihan_variant(0x8682, kTraditionalVariant, 0x879E).
unicode_unihan_variant(0x8683, kTraditionalVariant, 0x8801).
unicode_unihan_variant(0x8683, kZVariant, 0x8801).
unicode_unihan_variant(0x868A, kSemanticVariant, 0x87C1). %<kMatthews
unicode_unihan_variant(0x868A, kZVariant, 0x87A1).
unicode_unihan_variant(0x868B, kSemanticVariant, 0x8739). %<kMatthews
unicode_unihan_variant(0x8693, kSemanticVariant, 0x87BE). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8695, kSemanticVariant, 0x8836). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8695, kTraditionalVariant, 0x8836).
unicode_unihan_variant(0x8698, kSemanticVariant, 0x75D0). %<kMeyerWempe 0x86D4<kMatthews 0x86D5<kMatthews
unicode_unihan_variant(0x869D, kSemanticVariant, 0x8786). %<kMatthews
unicode_unihan_variant(0x86A4, kSemanticVariant, 0x45A3). %<kLau,kMatthews
unicode_unihan_variant(0x86A6, kSemanticVariant, 0x86BA). %<kMatthews
unicode_unihan_variant(0x86AA, kSemanticVariant, 0x866C). %<kMeyerWempe
unicode_unihan_variant(0x86AC, kTraditionalVariant, 0x8706).
unicode_unihan_variant(0x86BA, kSemanticVariant, 0x86A6). %<kMatthews
unicode_unihan_variant(0x86C4, kSemanticVariant, 0x86CC). %<kMatthews
unicode_unihan_variant(0x86C6, kSemanticVariant, 0x27405). %<kMeyerWempe
unicode_unihan_variant(0x86CA, kTraditionalVariant, 0x8831).
unicode_unihan_variant(0x86CB, kSemanticVariant, 0x8711). %<kLau,kMatthews
unicode_unihan_variant(0x86CB, kZVariant, 0x65E6).
unicode_unihan_variant(0x86CC, kSemanticVariant, 0x86C4). %<kMatthews
unicode_unihan_variant(0x86CD, kZVariant, 0x87A2).
unicode_unihan_variant(0x86CE, kTraditionalVariant, 0x8823).
unicode_unihan_variant(0x86CF, kTraditionalVariant, 0x87F6).
unicode_unihan_variant(0x86D3, kSemanticVariant, 0x8786). %<kMeyerWempe
unicode_unihan_variant(0x86D4, kSemanticVariant, 0x8698). %<kMatthews 0x86D5<kMatthews,kMeyerWempe
unicode_unihan_variant(0x86D4, kZVariant, 0x8716).
unicode_unihan_variant(0x86D5, kSemanticVariant, 0x8698). %<kMatthews 0x86D4<kMatthews,kMeyerWempe
unicode_unihan_variant(0x86D7, kSemanticVariant, 0x27313). %<kHanYu
unicode_unihan_variant(0x86D9, kSemanticVariant, 0x9F03). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x86DB, kSemanticVariant, 0x9F04). %<kMatthews
unicode_unihan_variant(0x86DB, kZVariant, 0x9F04).
unicode_unihan_variant(0x86EE, kTraditionalVariant, 0x883B).
unicode_unihan_variant(0x86F0, kTraditionalVariant, 0x87C4).
unicode_unihan_variant(0x86F1, kTraditionalVariant, 0x86FA).
unicode_unihan_variant(0x86F2, kTraditionalVariant, 0x87EF).
unicode_unihan_variant(0x86F3, kTraditionalVariant, 0x8784).
unicode_unihan_variant(0x86F4, kTraditionalVariant, 0x8810).
unicode_unihan_variant(0x86FA, kSimplifiedVariant, 0x86F1).
unicode_unihan_variant(0x86FB, kSimplifiedVariant, 0x8715).
unicode_unihan_variant(0x8702, kSemanticVariant, 0x882D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8706, kSimplifiedVariant, 0x86AC).
unicode_unihan_variant(0x8707, kSemanticVariant, 0x272CD). %<kFenn
unicode_unihan_variant(0x870B, kSemanticVariant, 0x8782). %<kMatthews
unicode_unihan_variant(0x870B, kSpecializedSemanticVariant, 0x8782). %<kMeyerWempe
unicode_unihan_variant(0x870D, kSemanticVariant, 0x8829). %<kMatthews
unicode_unihan_variant(0x8711, kSemanticVariant, 0x86CB). %<kLau,kMatthews
unicode_unihan_variant(0x8715, kTraditionalVariant, 0x86FB).
unicode_unihan_variant(0x8716, kZVariant, 0x86D4).
unicode_unihan_variant(0x8717, kTraditionalVariant, 0x8778).
unicode_unihan_variant(0x8718, kZVariant, 0x9F05).
unicode_unihan_variant(0x871D, kSemanticVariant, 0x871E). %<kMeyerWempe
unicode_unihan_variant(0x871E, kSemanticVariant, 0x871D). %<kMeyerWempe
unicode_unihan_variant(0x8721, kSemanticVariant, 0x410D). %<kMeyerWempe 0x881F<kLau,kMatthews
unicode_unihan_variant(0x8721, kSimplifiedVariant, 0x8721).
unicode_unihan_variant(0x8721, kSpecializedSemanticVariant, 0x410D). %0x881F<kFenn
unicode_unihan_variant(0x8721, kTraditionalVariant, 0x8721). %0x881F
unicode_unihan_variant(0x8728, kSemanticVariant, 0x8776). %<kMatthews
unicode_unihan_variant(0x8728, kSpecializedSemanticVariant, 0x8776). %<kFenn
unicode_unihan_variant(0x872B, kSemanticVariant, 0x45B5). %<kMatthews
unicode_unihan_variant(0x8736, kSemanticVariant, 0x87C0). %<kMeyerWempe
unicode_unihan_variant(0x8739, kSemanticVariant, 0x868B). %<kMatthews
unicode_unihan_variant(0x8739, kZVariant, 0x868B).
unicode_unihan_variant(0x873A, kZVariant, 0x9713).
unicode_unihan_variant(0x8743, kSemanticVariant, 0x87AE). %<kMeyerWempe
unicode_unihan_variant(0x8747, kSemanticVariant, 0x8805). %<kMeyerWempe
unicode_unihan_variant(0x8747, kTraditionalVariant, 0x8805).
unicode_unihan_variant(0x8748, kTraditionalVariant, 0x87C8).
unicode_unihan_variant(0x8749, kTraditionalVariant, 0x87EC).
unicode_unihan_variant(0x874B, kZVariant, 0x881F).
unicode_unihan_variant(0x874E, kSemanticVariant, 0x880D). %<kMeyerWempe
unicode_unihan_variant(0x8755, kSimplifiedVariant, 0x8680).
unicode_unihan_variant(0x875F, kSemanticVariant, 0x732C). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x875F, kSimplifiedVariant, 0x732C).
unicode_unihan_variant(0x8765, kSemanticVariant, 0x87CA). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8766, kSemanticVariant, 0x9C15). %<kMeyerWempe
unicode_unihan_variant(0x8766, kSimplifiedVariant, 0x867E).
unicode_unihan_variant(0x8766, kSpecializedSemanticVariant, 0x9C15). %<kFenn
unicode_unihan_variant(0x8768, kSemanticVariant, 0x8671). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8768, kZVariant, 0x8671).
unicode_unihan_variant(0x876F, kSemanticVariant, 0x7328). %<kMatthews,kMeyerWempe 0x733F<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8771, kSemanticVariant, 0x459F). %<kMatthews
unicode_unihan_variant(0x8774, kZVariant, 0x80E1).
unicode_unihan_variant(0x8776, kSemanticVariant, 0x8728). %<kMatthews
unicode_unihan_variant(0x8776, kSpecializedSemanticVariant, 0x8728). %<kFenn
unicode_unihan_variant(0x8778, kSimplifiedVariant, 0x8717).
unicode_unihan_variant(0x877C, kTraditionalVariant, 0x87BB).
unicode_unihan_variant(0x877E, kTraditionalVariant, 0x8811).
unicode_unihan_variant(0x877F, kZVariant, 0x8805).
unicode_unihan_variant(0x8780, kTraditionalVariant, 0x87BF).
unicode_unihan_variant(0x8782, kSemanticVariant, 0x870B). %<kMatthews
unicode_unihan_variant(0x8782, kSpecializedSemanticVariant, 0x870B). %<kMeyerWempe
unicode_unihan_variant(0x8784, kSimplifiedVariant, 0x86F3).
unicode_unihan_variant(0x8786, kSemanticVariant, 0x869D). %<kMatthews 0x86D3<kMeyerWempe
unicode_unihan_variant(0x878D, kSemanticVariant, 0x878E). %<kMeyerWempe
unicode_unihan_variant(0x878E, kSemanticVariant, 0x878D). %<kMeyerWempe
unicode_unihan_variant(0x8797, kSemanticVariant, 0x87B3). %<kMeyerWempe
unicode_unihan_variant(0x8797, kZVariant, 0x87B3).
unicode_unihan_variant(0x8798, kSemanticVariant, 0x87FB). %<kLau
unicode_unihan_variant(0x8799, kSemanticVariant, 0x8827). %<kMatthews
unicode_unihan_variant(0x879E, kSimplifiedVariant, 0x8682).
unicode_unihan_variant(0x87A2, kSimplifiedVariant, 0x8424).
unicode_unihan_variant(0x87A2, kZVariant, 0x86CD).
unicode_unihan_variant(0x87A8, kTraditionalVariant, 0x87CE).
unicode_unihan_variant(0x87AB, kSpecializedSemanticVariant, 0x8457). %<kFenn
unicode_unihan_variant(0x87AE, kSemanticVariant, 0x8743). %<kMeyerWempe
unicode_unihan_variant(0x87AE, kSimplifiedVariant, 0x45D6).
unicode_unihan_variant(0x87B3, kSemanticVariant, 0x8797). %<kMeyerWempe
unicode_unihan_variant(0x87B3, kZVariant, 0x8797).
unicode_unihan_variant(0x87B5, kZVariant, 0x8731).
unicode_unihan_variant(0x87BA, kSemanticVariant, 0x8803). %<kMeyerWempe
unicode_unihan_variant(0x87BA, kZVariant, 0xF911).
unicode_unihan_variant(0x87BB, kSimplifiedVariant, 0x877C).
unicode_unihan_variant(0x87BE, kSemanticVariant, 0x8693). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x87BF, kSimplifiedVariant, 0x8780).
unicode_unihan_variant(0x87C0, kSemanticVariant, 0x8736). %<kMeyerWempe
unicode_unihan_variant(0x87C1, kSemanticVariant, 0x868A). %<kMatthews
unicode_unihan_variant(0x87C1, kZVariant, 0x868A).
unicode_unihan_variant(0x87C4, kSimplifiedVariant, 0x86F0).
unicode_unihan_variant(0x87C7, kZVariant, 0x87C6).
unicode_unihan_variant(0x87C8, kSimplifiedVariant, 0x8748).
unicode_unihan_variant(0x87CA, kSemanticVariant, 0x8765). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x87CE, kSimplifiedVariant, 0x87A8).
unicode_unihan_variant(0x87CF, kTraditionalVariant, 0x8828).
unicode_unihan_variant(0x87D2, kZVariant, 0x880E).
unicode_unihan_variant(0x87DA, kSemanticVariant, 0x87DB). %<kLau
unicode_unihan_variant(0x87DB, kSemanticVariant, 0x87DA). %<kLau
unicode_unihan_variant(0x87E3, kSimplifiedVariant, 0x866E).
unicode_unihan_variant(0x87EC, kSimplifiedVariant, 0x8749).
unicode_unihan_variant(0x87EE, kSemanticVariant, 0x87FA). %<kMatthews
unicode_unihan_variant(0x87EF, kSimplifiedVariant, 0x86F2).
unicode_unihan_variant(0x87F2, kSemanticVariant, 0x866B). %<kMeyerWempe
unicode_unihan_variant(0x87F2, kSimplifiedVariant, 0x866B).
unicode_unihan_variant(0x87F6, kSimplifiedVariant, 0x86CF).
unicode_unihan_variant(0x87F9, kSemanticVariant, 0x880F). %<kLau
unicode_unihan_variant(0x87FA, kSemanticVariant, 0x87EE). %<kMatthews
unicode_unihan_variant(0x87FB, kSemanticVariant, 0x8798). %<kLau
unicode_unihan_variant(0x87FB, kSimplifiedVariant, 0x8681).
unicode_unihan_variant(0x8801, kSimplifiedVariant, 0x8683).
unicode_unihan_variant(0x8801, kZVariant, 0x8683).
unicode_unihan_variant(0x8803, kSemanticVariant, 0x87BA). %<kMeyerWempe
unicode_unihan_variant(0x8805, kSemanticVariant, 0x8747). %<kMeyerWempe
unicode_unihan_variant(0x8805, kSimplifiedVariant, 0x8747).
unicode_unihan_variant(0x8806, kSimplifiedVariant, 0x867F).
unicode_unihan_variant(0x880D, kSemanticVariant, 0x874E). %<kMeyerWempe
unicode_unihan_variant(0x880E, kZVariant, 0x87D2).
unicode_unihan_variant(0x880F, kSemanticVariant, 0x87F9). %<kLau
unicode_unihan_variant(0x880F, kZVariant, 0x87F9).
unicode_unihan_variant(0x8810, kSimplifiedVariant, 0x86F4).
unicode_unihan_variant(0x8811, kSimplifiedVariant, 0x877E).
unicode_unihan_variant(0x8814, kSemanticVariant, 0x27422). %<kMeyerWempe
unicode_unihan_variant(0x881F, kSemanticVariant, 0x8721). %<kLau,kMatthews
unicode_unihan_variant(0x881F, kSimplifiedVariant, 0x8721).
unicode_unihan_variant(0x881F, kSpecializedSemanticVariant, 0x8721). %<kFenn
unicode_unihan_variant(0x8822, kZVariant, 0x60F7).
unicode_unihan_variant(0x8823, kSimplifiedVariant, 0x86CE).
unicode_unihan_variant(0x8827, kSemanticVariant, 0x8799). %<kMatthews
unicode_unihan_variant(0x8827, kZVariant, 0x8839).
unicode_unihan_variant(0x8828, kSimplifiedVariant, 0x87CF).
unicode_unihan_variant(0x8829, kSemanticVariant, 0x870D). %<kMatthews
unicode_unihan_variant(0x882D, kSemanticVariant, 0x8702). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8831, kSimplifiedVariant, 0x86CA).
unicode_unihan_variant(0x8836, kSemanticVariant, 0x8695). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8836, kSimplifiedVariant, 0x8695).
unicode_unihan_variant(0x8839, kZVariant, 0x8827).
unicode_unihan_variant(0x883B, kSimplifiedVariant, 0x86EE).
unicode_unihan_variant(0x8842, kSemanticVariant, 0x8844). %<kMatthews
unicode_unihan_variant(0x8844, kSemanticVariant, 0x8842). %<kMatthews
unicode_unihan_variant(0x8844, kZVariant, 0x8842).
unicode_unihan_variant(0x8845, kSemanticVariant, 0x91C1). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8845, kTraditionalVariant, 0x91C1).
unicode_unihan_variant(0x8846, kSemanticVariant, 0x773E). %<kLau,kMatthews 0x2009D<kFenn
unicode_unihan_variant(0x8846, kZVariant, 0x773E).
unicode_unihan_variant(0x8847, kSemanticVariant, 0x8108). %<kLau,kMatthews 0x8109<kLau,kMatthews
unicode_unihan_variant(0x8847, kSpecializedSemanticVariant, 0x8108). %<kFenn 0x8109<kFenn
unicode_unihan_variant(0x884C, kZVariant, 0xFA08).
unicode_unihan_variant(0x8852, kSpecializedSemanticVariant, 0x70AB).
unicode_unihan_variant(0x8853, kSimplifiedVariant, 0x672F).
unicode_unihan_variant(0x8854, kTraditionalVariant, 0x929C).
unicode_unihan_variant(0x8855, kSimplifiedVariant, 0x540C).
unicode_unihan_variant(0x8857, kZVariant, 0x4E8D).
unicode_unihan_variant(0x885A, kSimplifiedVariant, 0x80E1).
unicode_unihan_variant(0x885B, kSemanticVariant, 0x885E). %<kLau,kMatthews
unicode_unihan_variant(0x885B, kSimplifiedVariant, 0x536B).
unicode_unihan_variant(0x885D, kSemanticVariant, 0x27602). %<kFenn
unicode_unihan_variant(0x885D, kSimplifiedVariant, 0x51B2).
unicode_unihan_variant(0x885E, kSemanticVariant, 0x885B). %<kLau,kMatthews
unicode_unihan_variant(0x885E, kZVariant, 0x885B).
unicode_unihan_variant(0x8863, kSemanticVariant, 0x8864). %<kMatthews
unicode_unihan_variant(0x8863, kSpecializedSemanticVariant, 0x8864). %<kFenn
unicode_unihan_variant(0x8864, kSemanticVariant, 0x8863). %<kMatthews
unicode_unihan_variant(0x8864, kSpecializedSemanticVariant, 0x8863). %<kFenn
unicode_unihan_variant(0x8865, kTraditionalVariant, 0x88DC).
unicode_unihan_variant(0x8868, kTraditionalVariant, 0x9336).
unicode_unihan_variant(0x886C, kTraditionalVariant, 0x896F).
unicode_unihan_variant(0x886E, kSemanticVariant, 0x889E). %<kMatthews
unicode_unihan_variant(0x886E, kTraditionalVariant, 0x889E).
unicode_unihan_variant(0x887D, kSemanticVariant, 0x88B5). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x887F, kSemanticVariant, 0x7D1F). %<kFenn 0x895F<kMatthews
unicode_unihan_variant(0x8884, kTraditionalVariant, 0x8956).
unicode_unihan_variant(0x8885, kTraditionalVariant, 0x88CA).
unicode_unihan_variant(0x8885, kZVariant, 0x5B1D).
unicode_unihan_variant(0x8886, kTraditionalVariant, 0x8918).
unicode_unihan_variant(0x888B, kSemanticVariant, 0x5E12). %<kLau,kMatthews
unicode_unihan_variant(0x8892, kSemanticVariant, 0x8962). %<kLau,kMatthews
unicode_unihan_variant(0x8896, kZVariant, 0x890E).
unicode_unihan_variant(0x889C, kSemanticVariant, 0x896A). %<kLau
unicode_unihan_variant(0x889C, kTraditionalVariant, 0x896A).
unicode_unihan_variant(0x889E, kSemanticVariant, 0x886E). %<kMatthews
unicode_unihan_variant(0x889E, kSimplifiedVariant, 0x886E).
unicode_unihan_variant(0x88A0, kSemanticVariant, 0x5E19). %<kMatthews
unicode_unihan_variant(0x88AD, kTraditionalVariant, 0x8972).
unicode_unihan_variant(0x88AF, kTraditionalVariant, 0x894F).
unicode_unihan_variant(0x88B4, kSemanticVariant, 0x7D5D). %<kLau,kMatthews 0x8932<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x88B4, kZVariant, 0x8932).
unicode_unihan_variant(0x88B5, kSemanticVariant, 0x887D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x88B7, kSemanticVariant, 0x88CC). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x88C2, kZVariant, 0xF9A0).
unicode_unihan_variant(0x88C5, kTraditionalVariant, 0x88DD).
unicode_unihan_variant(0x88C6, kTraditionalVariant, 0x8960).
unicode_unihan_variant(0x88C8, kTraditionalVariant, 0x890C).
unicode_unihan_variant(0x88C9, kSemanticVariant, 0x8903). %<kFenn
unicode_unihan_variant(0x88CA, kSimplifiedVariant, 0x8885).
unicode_unihan_variant(0x88CA, kZVariant, 0x5B1D).
unicode_unihan_variant(0x88CC, kSemanticVariant, 0x88B7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x88CF, kSemanticVariant, 0x88E1). %<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x88CF, kSpecializedSemanticVariant, 0x88E1). %<kMeyerWempe
unicode_unihan_variant(0x88CF, kZVariant, 0xF9E9).
unicode_unihan_variant(0x88D2, kSpecializedSemanticVariant, 0x9D04). %<kMeyerWempe
unicode_unihan_variant(0x88D9, kSemanticVariant, 0x5E2C). %<kMatthews 0x88E0<kFenn
unicode_unihan_variant(0x88D9, kZVariant, 0x5E2C).
unicode_unihan_variant(0x88DC, kSimplifiedVariant, 0x8865).
unicode_unihan_variant(0x88DD, kSimplifiedVariant, 0x88C5).
unicode_unihan_variant(0x88E0, kSemanticVariant, 0x88D9). %<kFenn
unicode_unihan_variant(0x88E0, kZVariant, 0x88D9).
unicode_unihan_variant(0x88E1, kSemanticVariant, 0x88CF). %<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x88E1, kSimplifiedVariant, 0x91CC).
unicode_unihan_variant(0x88E1, kSpecializedSemanticVariant, 0x88CF). %<kMeyerWempe
unicode_unihan_variant(0x88E2, kTraditionalVariant, 0x8933).
unicode_unihan_variant(0x88E3, kTraditionalVariant, 0x895D).
unicode_unihan_variant(0x88E4, kTraditionalVariant, 0x8932).
unicode_unihan_variant(0x88E5, kTraditionalVariant, 0x8949).
unicode_unihan_variant(0x88F4, kZVariant, 0x88F5).
unicode_unihan_variant(0x88F5, kZVariant, 0x88F4).
unicode_unihan_variant(0x88F8, kSemanticVariant, 0x8EB6). %<kLau,kMatthews
unicode_unihan_variant(0x88F8, kZVariant, 0xF912).
unicode_unihan_variant(0x88FD, kSimplifiedVariant, 0x5236).
unicode_unihan_variant(0x8903, kSemanticVariant, 0x88C9). %<kFenn
unicode_unihan_variant(0x8907, kSimplifiedVariant, 0x590D).
unicode_unihan_variant(0x8907, kZVariant, 0x5FA9).
unicode_unihan_variant(0x890C, kSimplifiedVariant, 0x88C8).
unicode_unihan_variant(0x890E, kSemanticVariant, 0x890F). %<kMatthews
unicode_unihan_variant(0x890F, kSemanticVariant, 0x890E). %<kMatthews
unicode_unihan_variant(0x8912, kSemanticVariant, 0x8943). %<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x8912, kZVariant, 0x8943).
unicode_unihan_variant(0x8913, kSemanticVariant, 0x7DE5). %<kMatthews
unicode_unihan_variant(0x8913, kZVariant, 0x8934).
unicode_unihan_variant(0x8918, kSimplifiedVariant, 0x8886).
unicode_unihan_variant(0x891B, kTraditionalVariant, 0x8938).
unicode_unihan_variant(0x891F, kSemanticVariant, 0x6EBB). %<kFenn
unicode_unihan_variant(0x8922, kSemanticVariant, 0x8931). %<kMatthews
unicode_unihan_variant(0x8927, kSemanticVariant, 0x7D45). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8931, kSemanticVariant, 0x8922). %<kMatthews
unicode_unihan_variant(0x8932, kSemanticVariant, 0x7D5D). %<kLau,kMatthews 0x88B4<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8932, kSimplifiedVariant, 0x88E4).
unicode_unihan_variant(0x8932, kZVariant, 0x7DAF).
unicode_unihan_variant(0x8933, kSimplifiedVariant, 0x88E2).
unicode_unihan_variant(0x8934, kTraditionalVariant, 0x8964).
unicode_unihan_variant(0x8934, kZVariant, 0x8913).
unicode_unihan_variant(0x8935, kSemanticVariant, 0x7E2D). %<kMeyerWempe
unicode_unihan_variant(0x8938, kSimplifiedVariant, 0x891B).
unicode_unihan_variant(0x8938, kSpecializedSemanticVariant, 0x851E). %<kMeyerWempe
unicode_unihan_variant(0x893A, kSemanticVariant, 0x465D). %<kMatthews 0x893B<kMatthews
unicode_unihan_variant(0x893B, kSemanticVariant, 0x465D). %<kLau,kMatthews 0x893A<kMatthews
unicode_unihan_variant(0x893B, kSimplifiedVariant, 0x4EB5).
unicode_unihan_variant(0x8940, kSimplifiedVariant, 0x2B300).
unicode_unihan_variant(0x8941, kSemanticVariant, 0x7E48). %<kMatthews,kMeyerWempe 0x7E66<kMatthews
unicode_unihan_variant(0x8943, kSemanticVariant, 0x8912). %<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x8943, kZVariant, 0x8912).
unicode_unihan_variant(0x8944, kSemanticVariant, 0x342E).
unicode_unihan_variant(0x8949, kSimplifiedVariant, 0x88E5).
unicode_unihan_variant(0x894D, kSemanticVariant, 0x96DC). %<kMatthews
unicode_unihan_variant(0x894F, kSimplifiedVariant, 0x88AF).
unicode_unihan_variant(0x8955, kTraditionalVariant, 0x8974).
unicode_unihan_variant(0x8955, kZVariant, 0x8974).
unicode_unihan_variant(0x8956, kSemanticVariant, 0x6FB3). %<kMatthews 0x2570C<kMatthews
unicode_unihan_variant(0x8956, kSimplifiedVariant, 0x8884).
unicode_unihan_variant(0x895C, kSemanticVariant, 0x895D). %<kMeyerWempe
unicode_unihan_variant(0x895C, kSpecializedSemanticVariant, 0x8F26). %<kMeyerWempe
unicode_unihan_variant(0x895D, kSemanticVariant, 0x895C). %<kMeyerWempe
unicode_unihan_variant(0x895D, kSimplifiedVariant, 0x88E3).
unicode_unihan_variant(0x895F, kSemanticVariant, 0x887F). %<kMatthews
unicode_unihan_variant(0x8960, kSimplifiedVariant, 0x88C6).
unicode_unihan_variant(0x8962, kSemanticVariant, 0x8892). %<kLau,kMatthews
unicode_unihan_variant(0x8964, kSimplifiedVariant, 0x8934).
unicode_unihan_variant(0x8964, kZVariant, 0xF924).
unicode_unihan_variant(0x896A, kSemanticVariant, 0x889C). %<kLau 0x97E4<kMatthews 0x97C8<kMatthews
unicode_unihan_variant(0x896A, kSimplifiedVariant, 0x889C).
unicode_unihan_variant(0x896A, kZVariant, 0x97C8).
unicode_unihan_variant(0x896C, kSimplifiedVariant, 0x4653).
unicode_unihan_variant(0x896E, kSemanticVariant, 0x5E5E). %<kMeyerWempe
unicode_unihan_variant(0x896F, kSemanticVariant, 0x512D). %<kMatthews
unicode_unihan_variant(0x896F, kSimplifiedVariant, 0x886C).
unicode_unihan_variant(0x8972, kSimplifiedVariant, 0x88AD).
unicode_unihan_variant(0x8974, kSimplifiedVariant, 0x8955).
unicode_unihan_variant(0x8974, kZVariant, 0x5E71).
unicode_unihan_variant(0x897E, kZVariant, 0x897F).
unicode_unihan_variant(0x897F, kZVariant, 0x8980).
unicode_unihan_variant(0x8980, kZVariant, 0x897F).
unicode_unihan_variant(0x8986, kSimplifiedVariant, 0x590D).
unicode_unihan_variant(0x8986, kZVariant, 0x5FA9).
unicode_unihan_variant(0x8987, kSemanticVariant, 0x9738). %<kMeyerWempe
unicode_unihan_variant(0x8987, kZVariant, 0x9738).
unicode_unihan_variant(0x8988, kZVariant, 0x6838).
unicode_unihan_variant(0x8989, kZVariant, 0x7F88).
unicode_unihan_variant(0x898A, kSemanticVariant, 0x7F88). %<kMatthews
unicode_unihan_variant(0x898A, kZVariant, 0x7F88).
unicode_unihan_variant(0x898B, kSimplifiedVariant, 0x89C1).
unicode_unihan_variant(0x898C, kSemanticVariant, 0x89C0). %<kMatthews
unicode_unihan_variant(0x898C, kSpecializedSemanticVariant, 0x89C0). %<kFenn
unicode_unihan_variant(0x898E, kSimplifiedVariant, 0x89C3).
unicode_unihan_variant(0x898F, kSimplifiedVariant, 0x89C4).
unicode_unihan_variant(0x898F, kZVariant, 0x69FB).
unicode_unihan_variant(0x8990, kSemanticVariant, 0x89BA). %<kMatthews
unicode_unihan_variant(0x8993, kSemanticVariant, 0x8994). %<kMatthews
unicode_unihan_variant(0x8993, kSimplifiedVariant, 0x89C5).
unicode_unihan_variant(0x8994, kSemanticVariant, 0x8993). %<kMatthews
unicode_unihan_variant(0x8996, kSemanticVariant, 0x770E). %<kMatthews
unicode_unihan_variant(0x8996, kSimplifiedVariant, 0x89C6).
unicode_unihan_variant(0x8998, kSemanticVariant, 0x4993). %<kMeyerWempe 0x26552<kMeyerWempe
unicode_unihan_variant(0x8998, kSimplifiedVariant, 0x89C7).
unicode_unihan_variant(0x8999, kZVariant, 0x89BC).
unicode_unihan_variant(0x899A, kZVariant, 0x89BA).
unicode_unihan_variant(0x89A1, kSimplifiedVariant, 0x89CB).
unicode_unihan_variant(0x89A5, kSemanticVariant, 0x9766). %<kMeyerWempe
unicode_unihan_variant(0x89A5, kSimplifiedVariant, 0x89CD).
unicode_unihan_variant(0x89A6, kSimplifiedVariant, 0x89CE).
unicode_unihan_variant(0x89A9, kSemanticVariant, 0x7779). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x89AA, kSimplifiedVariant, 0x4EB2).
unicode_unihan_variant(0x89AC, kSimplifiedVariant, 0x89CA).
unicode_unihan_variant(0x89AF, kSimplifiedVariant, 0x89CF).
unicode_unihan_variant(0x89B0, kSemanticVariant, 0x89B7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x89B2, kSimplifiedVariant, 0x89D0).
unicode_unihan_variant(0x89B3, kZVariant, 0x89C0).
unicode_unihan_variant(0x89B5, kZVariant, 0x89B8).
unicode_unihan_variant(0x89B7, kSemanticVariant, 0x89B0). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x89B7, kSimplifiedVariant, 0x89D1).
unicode_unihan_variant(0x89B8, kZVariant, 0x89B5).
unicode_unihan_variant(0x89BA, kSemanticVariant, 0x8990). %<kMatthews
unicode_unihan_variant(0x89BA, kSimplifiedVariant, 0x89C9).
unicode_unihan_variant(0x89BA, kSpecializedSemanticVariant, 0x658D). %<kFenn
unicode_unihan_variant(0x89BA, kZVariant, 0x899A).
unicode_unihan_variant(0x89BC, kSimplifiedVariant, 0x2B328).
unicode_unihan_variant(0x89BD, kSimplifiedVariant, 0x89C8).
unicode_unihan_variant(0x89BD, kZVariant, 0x89A7).
unicode_unihan_variant(0x89BF, kSimplifiedVariant, 0x89CC).
unicode_unihan_variant(0x89C0, kSemanticVariant, 0x898C). %<kMatthews
unicode_unihan_variant(0x89C0, kSimplifiedVariant, 0x89C2).
unicode_unihan_variant(0x89C0, kSpecializedSemanticVariant, 0x898C). %<kFenn
unicode_unihan_variant(0x89C0, kZVariant, 0x898C).
unicode_unihan_variant(0x89C1, kTraditionalVariant, 0x898B).
unicode_unihan_variant(0x89C2, kTraditionalVariant, 0x89C0).
unicode_unihan_variant(0x89C3, kTraditionalVariant, 0x898E).
unicode_unihan_variant(0x89C4, kTraditionalVariant, 0x898F).
unicode_unihan_variant(0x89C5, kTraditionalVariant, 0x8993).
unicode_unihan_variant(0x89C6, kTraditionalVariant, 0x8996).
unicode_unihan_variant(0x89C7, kTraditionalVariant, 0x8998).
unicode_unihan_variant(0x89C8, kTraditionalVariant, 0x89BD).
unicode_unihan_variant(0x89C9, kTraditionalVariant, 0x89BA).
unicode_unihan_variant(0x89CA, kTraditionalVariant, 0x89AC).
unicode_unihan_variant(0x89CB, kTraditionalVariant, 0x89A1).
unicode_unihan_variant(0x89CC, kTraditionalVariant, 0x89BF).
unicode_unihan_variant(0x89CD, kTraditionalVariant, 0x89A5).
unicode_unihan_variant(0x89CE, kTraditionalVariant, 0x89A6).
unicode_unihan_variant(0x89CF, kTraditionalVariant, 0x89AF).
unicode_unihan_variant(0x89D0, kTraditionalVariant, 0x89B2).
unicode_unihan_variant(0x89D1, kTraditionalVariant, 0x89B7).
unicode_unihan_variant(0x89D2, kZVariant, 0x752A).
unicode_unihan_variant(0x89D4, kSemanticVariant, 0x65A4). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x89D4, kZVariant, 0x65A4).
unicode_unihan_variant(0x89D5, kSemanticVariant, 0x7C97). %<kMatthews 0x9E84<kMatthews 0x9EA4<kMatthews
unicode_unihan_variant(0x89DC, kSemanticVariant, 0x5480). %<kLau 0x5634<kLau
unicode_unihan_variant(0x89DD, kSemanticVariant, 0x7274). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x89DD, kSpecializedSemanticVariant, 0x7274).
unicode_unihan_variant(0x89DD, kZVariant, 0x7274).
unicode_unihan_variant(0x89DE, kTraditionalVariant, 0x89F4).
unicode_unihan_variant(0x89E3, kSemanticVariant, 0x89E7). %<kLau,kMatthews
unicode_unihan_variant(0x89E3, kSpecializedSemanticVariant, 0x89E7). %<kFenn
unicode_unihan_variant(0x89E3, kZVariant, 0x89E7).
unicode_unihan_variant(0x89E5, kSemanticVariant, 0x89F5). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x89E6, kTraditionalVariant, 0x89F8).
unicode_unihan_variant(0x89E7, kSemanticVariant, 0x89E3). %<kLau,kMatthews
unicode_unihan_variant(0x89E7, kSpecializedSemanticVariant, 0x89E3). %<kFenn
unicode_unihan_variant(0x89E7, kZVariant, 0x89E3).
unicode_unihan_variant(0x89EF, kTraditionalVariant, 0x89F6).
unicode_unihan_variant(0x89F4, kSimplifiedVariant, 0x89DE).
unicode_unihan_variant(0x89F5, kSemanticVariant, 0x89E5). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x89F6, kSimplifiedVariant, 0x89EF).
unicode_unihan_variant(0x89F8, kSimplifiedVariant, 0x89E6).
unicode_unihan_variant(0x8A00, kZVariant, 0x8A01).
unicode_unihan_variant(0x8A01, kSimplifiedVariant, 0x8BA0).
unicode_unihan_variant(0x8A02, kSimplifiedVariant, 0x8BA2).
unicode_unihan_variant(0x8A03, kSimplifiedVariant, 0x8BA3).
unicode_unihan_variant(0x8A07, kSemanticVariant, 0x6E39). %<kMeyerWempe 0x22550<kMeyerWempe
unicode_unihan_variant(0x8A08, kSimplifiedVariant, 0x8BA1).
unicode_unihan_variant(0x8A0A, kSimplifiedVariant, 0x8BAF).
unicode_unihan_variant(0x8A0C, kSimplifiedVariant, 0x8BA7).
unicode_unihan_variant(0x8A0E, kSimplifiedVariant, 0x8BA8).
unicode_unihan_variant(0x8A10, kSimplifiedVariant, 0x8BA6).
unicode_unihan_variant(0x8A11, kSimplifiedVariant, 0x2B359).
unicode_unihan_variant(0x8A12, kSimplifiedVariant, 0x8BB1).
unicode_unihan_variant(0x8A13, kSimplifiedVariant, 0x8BAD).
unicode_unihan_variant(0x8A15, kSimplifiedVariant, 0x8BAA).
unicode_unihan_variant(0x8A16, kSimplifiedVariant, 0x8BAB).
unicode_unihan_variant(0x8A17, kSemanticVariant, 0x4F82). %<kMatthews
unicode_unihan_variant(0x8A17, kSimplifiedVariant, 0x8BAC).
unicode_unihan_variant(0x8A18, kSimplifiedVariant, 0x8BB0).
unicode_unihan_variant(0x8A1A, kTraditionalVariant, 0x8ABE).
unicode_unihan_variant(0x8A1B, kSemanticVariant, 0x8B4C). %<kHKGlyph,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8A1B, kSimplifiedVariant, 0x8BB9).
unicode_unihan_variant(0x8A1B, kZVariant, 0x8B4C).
unicode_unihan_variant(0x8A1D, kSimplifiedVariant, 0x8BB6).
unicode_unihan_variant(0x8A1F, kSimplifiedVariant, 0x8BBC).
unicode_unihan_variant(0x8A20, kSemanticVariant, 0x77E7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8A22, kSimplifiedVariant, 0x4723).
unicode_unihan_variant(0x8A23, kSimplifiedVariant, 0x8BC0).
unicode_unihan_variant(0x8A25, kSemanticVariant, 0x5436). %<kLau,kMatthews
unicode_unihan_variant(0x8A25, kSimplifiedVariant, 0x8BB7).
unicode_unihan_variant(0x8A26, kSemanticVariant, 0x8AF6). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8A29, kSemanticVariant, 0x8A7E). %<kMatthews
unicode_unihan_variant(0x8A29, kSimplifiedVariant, 0x8BBB).
unicode_unihan_variant(0x8A2A, kSimplifiedVariant, 0x8BBF).
unicode_unihan_variant(0x8A2D, kSimplifiedVariant, 0x8BBE).
unicode_unihan_variant(0x8A31, kSimplifiedVariant, 0x8BB8).
unicode_unihan_variant(0x8A33, kZVariant, 0x8B6F).
unicode_unihan_variant(0x8A34, kSemanticVariant, 0x612C). %<kMatthews
unicode_unihan_variant(0x8A34, kSimplifiedVariant, 0x8BC9).
unicode_unihan_variant(0x8A34, kSpecializedSemanticVariant, 0x612C). %<kFenn
unicode_unihan_variant(0x8A36, kSimplifiedVariant, 0x8BC3).
unicode_unihan_variant(0x8A36, kZVariant, 0x5475).
unicode_unihan_variant(0x8A3A, kSimplifiedVariant, 0x8BCA).
unicode_unihan_variant(0x8A3B, kSimplifiedVariant, 0x6CE8).
unicode_unihan_variant(0x8A3C, kSemanticVariant, 0x8B49). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8A3C, kZVariant, 0x8B49).
unicode_unihan_variant(0x8A3D, kSemanticVariant, 0x8A6C). %<kMatthews
unicode_unihan_variant(0x8A40, kSimplifiedVariant, 0x27BAA).
unicode_unihan_variant(0x8A41, kSimplifiedVariant, 0x8BC2).
unicode_unihan_variant(0x8A46, kSimplifiedVariant, 0x8BCB).
unicode_unihan_variant(0x8A4B, kSemanticVariant, 0x546A). %<kFenn
unicode_unihan_variant(0x8A4E, kSimplifiedVariant, 0x8BB5).
unicode_unihan_variant(0x8A50, kSimplifiedVariant, 0x8BC8).
unicode_unihan_variant(0x8A50, kSpecializedSemanticVariant, 0x91A1). %<kFenn
unicode_unihan_variant(0x8A51, kSimplifiedVariant, 0x2B35F).
unicode_unihan_variant(0x8A52, kSimplifiedVariant, 0x8BD2).
unicode_unihan_variant(0x8A54, kSimplifiedVariant, 0x8BCF).
unicode_unihan_variant(0x8A55, kSimplifiedVariant, 0x8BC4).
unicode_unihan_variant(0x8A56, kSimplifiedVariant, 0x8BD0).
unicode_unihan_variant(0x8A57, kSimplifiedVariant, 0x8BC7).
unicode_unihan_variant(0x8A58, kSimplifiedVariant, 0x8BCE).
unicode_unihan_variant(0x8A5B, kSimplifiedVariant, 0x8BC5).
unicode_unihan_variant(0x8A5E, kSimplifiedVariant, 0x8BCD).
unicode_unihan_variant(0x8A5F, kTraditionalVariant, 0x8B8B).
unicode_unihan_variant(0x8A5F, kZVariant, 0x8B8B).
unicode_unihan_variant(0x8A60, kSemanticVariant, 0x548F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8A60, kSimplifiedVariant, 0x548F).
unicode_unihan_variant(0x8A61, kSimplifiedVariant, 0x8BE9).
unicode_unihan_variant(0x8A62, kSimplifiedVariant, 0x8BE2).
unicode_unihan_variant(0x8A63, kSimplifiedVariant, 0x8BE3).
unicode_unihan_variant(0x8A64, kSemanticVariant, 0x8B0A). %<kMatthews
unicode_unihan_variant(0x8A66, kSimplifiedVariant, 0x8BD5).
unicode_unihan_variant(0x8A67, kSemanticVariant, 0x5BDF). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8A67, kZVariant, 0x5BDF).
unicode_unihan_variant(0x8A69, kSimplifiedVariant, 0x8BD7).
unicode_unihan_variant(0x8A6B, kSimplifiedVariant, 0x8BE7).
unicode_unihan_variant(0x8A6C, kSemanticVariant, 0x8A3D). %<kMatthews
unicode_unihan_variant(0x8A6C, kSimplifiedVariant, 0x8BDF).
unicode_unihan_variant(0x8A6C, kSpecializedSemanticVariant, 0x5474). %<kMeyerWempe
unicode_unihan_variant(0x8A6D, kSimplifiedVariant, 0x8BE1).
unicode_unihan_variant(0x8A6E, kSimplifiedVariant, 0x8BE0).
unicode_unihan_variant(0x8A70, kSimplifiedVariant, 0x8BD8).
unicode_unihan_variant(0x8A71, kSemanticVariant, 0x46E1). %<kMatthews
unicode_unihan_variant(0x8A71, kSimplifiedVariant, 0x8BDD).
unicode_unihan_variant(0x8A72, kSimplifiedVariant, 0x8BE5).
unicode_unihan_variant(0x8A73, kSimplifiedVariant, 0x8BE6).
unicode_unihan_variant(0x8A75, kSemanticVariant, 0x4F81). %<kMeyerWempe 0x99EA<kMeyerWempe
unicode_unihan_variant(0x8A75, kSimplifiedVariant, 0x8BDC).
unicode_unihan_variant(0x8A76, kZVariant, 0x916C).
unicode_unihan_variant(0x8A7B, kZVariant, 0x54AF).
unicode_unihan_variant(0x8A7C, kSimplifiedVariant, 0x8BD9).
unicode_unihan_variant(0x8A7E, kSemanticVariant, 0x8A29). %<kMatthews
unicode_unihan_variant(0x8A7F, kSimplifiedVariant, 0x8BD6).
unicode_unihan_variant(0x8A84, kSimplifiedVariant, 0x8BD4).
unicode_unihan_variant(0x8A85, kSimplifiedVariant, 0x8BDB).
unicode_unihan_variant(0x8A86, kSimplifiedVariant, 0x8BD3).
unicode_unihan_variant(0x8A87, kSemanticVariant, 0x5938). %<kMeyerWempe
unicode_unihan_variant(0x8A87, kSimplifiedVariant, 0x5938).
unicode_unihan_variant(0x8A89, kTraditionalVariant, 0x8B7D).
unicode_unihan_variant(0x8A8A, kTraditionalVariant, 0x8B04).
unicode_unihan_variant(0x8A8C, kSimplifiedVariant, 0x5FD7).
unicode_unihan_variant(0x8A8D, kSimplifiedVariant, 0x8BA4).
unicode_unihan_variant(0x8A90, kSemanticVariant, 0x54E6). %<kMeyerWempe
unicode_unihan_variant(0x8A91, kSimplifiedVariant, 0x8BF3).
unicode_unihan_variant(0x8A92, kSimplifiedVariant, 0x8BF6).
unicode_unihan_variant(0x8A92, kZVariant, 0x5509).
unicode_unihan_variant(0x8A95, kSemanticVariant, 0x27A59). %<kLau,kMeyerWempe
unicode_unihan_variant(0x8A95, kSimplifiedVariant, 0x8BDE).
unicode_unihan_variant(0x8A98, kSemanticVariant, 0x46FB). %<kMatthews
unicode_unihan_variant(0x8A98, kSimplifiedVariant, 0x8BF1).
unicode_unihan_variant(0x8A9A, kSemanticVariant, 0x8B59). %<kFenn
unicode_unihan_variant(0x8A9A, kSimplifiedVariant, 0x8BEE).
unicode_unihan_variant(0x8A9E, kSimplifiedVariant, 0x8BED).
unicode_unihan_variant(0x8AA0, kSimplifiedVariant, 0x8BDA).
unicode_unihan_variant(0x8AA1, kSimplifiedVariant, 0x8BEB).
unicode_unihan_variant(0x8AA3, kSimplifiedVariant, 0x8BEC).
unicode_unihan_variant(0x8AA4, kSemanticVariant, 0x609E). %<kLau
unicode_unihan_variant(0x8AA4, kSimplifiedVariant, 0x8BEF).
unicode_unihan_variant(0x8AA5, kSimplifiedVariant, 0x8BF0).
unicode_unihan_variant(0x8AA6, kSimplifiedVariant, 0x8BF5).
unicode_unihan_variant(0x8AA8, kSimplifiedVariant, 0x8BF2).
unicode_unihan_variant(0x8AA9, kSemanticVariant, 0x7AF6). %<kMatthews
unicode_unihan_variant(0x8AAA, kSimplifiedVariant, 0x8BF4).
unicode_unihan_variant(0x8AAA, kZVariant, 0x8AAC).
unicode_unihan_variant(0x8AAC, kZVariant, 0x8AAA).
unicode_unihan_variant(0x8AAD, kZVariant, 0x8B80).
unicode_unihan_variant(0x8AB0, kSimplifiedVariant, 0x8C01).
unicode_unihan_variant(0x8AB2, kSimplifiedVariant, 0x8BFE).
unicode_unihan_variant(0x8AB6, kSimplifiedVariant, 0x8C07).
unicode_unihan_variant(0x8AB9, kSimplifiedVariant, 0x8BFD).
unicode_unihan_variant(0x8ABC, kSimplifiedVariant, 0x8C0A).
unicode_unihan_variant(0x8ABE, kSimplifiedVariant, 0x8A1A).
unicode_unihan_variant(0x8ABF, kSimplifiedVariant, 0x8C03).
unicode_unihan_variant(0x8AC1, kSemanticVariant, 0x555C). %<kMeyerWempe
unicode_unihan_variant(0x8AC2, kSimplifiedVariant, 0x8C04).
unicode_unihan_variant(0x8AC4, kSimplifiedVariant, 0x8C06).
unicode_unihan_variant(0x8AC7, kSemanticVariant, 0x8B5A). %<kLau
unicode_unihan_variant(0x8AC7, kSimplifiedVariant, 0x8C08).
unicode_unihan_variant(0x8AC9, kSemanticVariant, 0x9935). %<kLau
unicode_unihan_variant(0x8AC9, kSimplifiedVariant, 0x8BFF).
unicode_unihan_variant(0x8ACA, kSemanticVariant, 0x97AB). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8ACB, kSimplifiedVariant, 0x8BF7).
unicode_unihan_variant(0x8ACC, kZVariant, 0x8AEB).
unicode_unihan_variant(0x8ACD, kSimplifiedVariant, 0x8BE4).
unicode_unihan_variant(0x8ACF, kSimplifiedVariant, 0x8BF9).
unicode_unihan_variant(0x8AD1, kSimplifiedVariant, 0x8BFC).
unicode_unihan_variant(0x8AD2, kSimplifiedVariant, 0x8C05).
unicode_unihan_variant(0x8AD5, kSemanticVariant, 0x552C). %<kMatthews
unicode_unihan_variant(0x8AD6, kSimplifiedVariant, 0x8BBA).
unicode_unihan_variant(0x8AD6, kZVariant, 0xF941).
unicode_unihan_variant(0x8AD7, kSimplifiedVariant, 0x8C02).
unicode_unihan_variant(0x8ADB, kSimplifiedVariant, 0x8C00).
unicode_unihan_variant(0x8ADC, kSimplifiedVariant, 0x8C0D).
unicode_unihan_variant(0x8ADD, kSimplifiedVariant, 0x8C1E).
unicode_unihan_variant(0x8ADE, kSimplifiedVariant, 0x8C1D).
unicode_unihan_variant(0x8AE0, kSemanticVariant, 0x55A7). %<kMeyerWempe
unicode_unihan_variant(0x8AE0, kZVariant, 0x55A7).
unicode_unihan_variant(0x8AE1, kSemanticVariant, 0x8B1A). %<kMatthews
unicode_unihan_variant(0x8AE2, kSimplifiedVariant, 0x8BE8).
unicode_unihan_variant(0x8AE4, kSemanticVariant, 0x8B8D). %<kMatthews
unicode_unihan_variant(0x8AE4, kSimplifiedVariant, 0x8C14).
unicode_unihan_variant(0x8AE4, kSpecializedSemanticVariant, 0x56EE). %<kMeyerWempe
unicode_unihan_variant(0x8AE6, kSimplifiedVariant, 0x8C1B).
unicode_unihan_variant(0x8AE7, kSemanticVariant, 0x9FA4). %<kMatthews
unicode_unihan_variant(0x8AE7, kSimplifiedVariant, 0x8C10).
unicode_unihan_variant(0x8AE9, kZVariant, 0x8B5C).
unicode_unihan_variant(0x8AEB, kSimplifiedVariant, 0x8C0F).
unicode_unihan_variant(0x8AED, kSimplifiedVariant, 0x8C15).
unicode_unihan_variant(0x8AEE, kSemanticVariant, 0x54A8). %<kMatthews,kMeyerWempe 0x55DE<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8AEE, kSimplifiedVariant, 0x8C18).
unicode_unihan_variant(0x8AEE, kZVariant, 0x54A8).
unicode_unihan_variant(0x8AF0, kSimplifiedVariant, 0x2B370).
unicode_unihan_variant(0x8AF1, kSimplifiedVariant, 0x8BB3).
unicode_unihan_variant(0x8AF3, kSimplifiedVariant, 0x8C19).
unicode_unihan_variant(0x8AF5, kSemanticVariant, 0x5583). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8AF6, kSemanticVariant, 0x8A26). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8AF6, kSimplifiedVariant, 0x8C0C).
unicode_unihan_variant(0x8AF7, kSimplifiedVariant, 0x8BBD).
unicode_unihan_variant(0x8AF8, kSimplifiedVariant, 0x8BF8).
unicode_unihan_variant(0x8AFA, kSimplifiedVariant, 0x8C1A).
unicode_unihan_variant(0x8AFC, kSimplifiedVariant, 0x8C16).
unicode_unihan_variant(0x8AFC, kSpecializedSemanticVariant, 0x7156). %<kMeyerWempe
unicode_unihan_variant(0x8AFE, kSimplifiedVariant, 0x8BFA).
unicode_unihan_variant(0x8B00, kSimplifiedVariant, 0x8C0B).
unicode_unihan_variant(0x8B01, kSimplifiedVariant, 0x8C12).
unicode_unihan_variant(0x8B02, kSimplifiedVariant, 0x8C13).
unicode_unihan_variant(0x8B04, kSimplifiedVariant, 0x8A8A).
unicode_unihan_variant(0x8B05, kSimplifiedVariant, 0x8BCC).
unicode_unihan_variant(0x8B0A, kSemanticVariant, 0x8A64). %<kMatthews
unicode_unihan_variant(0x8B0A, kSimplifiedVariant, 0x8C0E).
unicode_unihan_variant(0x8B0C, kSemanticVariant, 0x6B4C). %<kLau,kMatthews
unicode_unihan_variant(0x8B0E, kSimplifiedVariant, 0x8C1C).
unicode_unihan_variant(0x8B0F, kSimplifiedVariant, 0x2B372).
unicode_unihan_variant(0x8B10, kSimplifiedVariant, 0x8C27).
unicode_unihan_variant(0x8B14, kSimplifiedVariant, 0x8C11).
unicode_unihan_variant(0x8B16, kSimplifiedVariant, 0x8C21).
unicode_unihan_variant(0x8B17, kSimplifiedVariant, 0x8C24).
unicode_unihan_variant(0x8B19, kSimplifiedVariant, 0x8C26).
unicode_unihan_variant(0x8B1A, kSemanticVariant, 0x8AE1). %<kMatthews
unicode_unihan_variant(0x8B1A, kSimplifiedVariant, 0x8C25).
unicode_unihan_variant(0x8B1B, kSimplifiedVariant, 0x8BB2).
unicode_unihan_variant(0x8B1D, kSimplifiedVariant, 0x8C22).
unicode_unihan_variant(0x8B20, kSimplifiedVariant, 0x8C23).
unicode_unihan_variant(0x8B21, kZVariant, 0x8B20).
unicode_unihan_variant(0x8B28, kSimplifiedVariant, 0x8C1F).
unicode_unihan_variant(0x8B2B, kSemanticVariant, 0x8B81). %<kMatthews
unicode_unihan_variant(0x8B2B, kSimplifiedVariant, 0x8C2A).
unicode_unihan_variant(0x8B2C, kSimplifiedVariant, 0x8C2C).
unicode_unihan_variant(0x8B31, kSemanticVariant, 0x560D). %<kMeyerWempe
unicode_unihan_variant(0x8B33, kSimplifiedVariant, 0x8BB4).
unicode_unihan_variant(0x8B39, kSimplifiedVariant, 0x8C28).
unicode_unihan_variant(0x8B3C, kZVariant, 0x547C).
unicode_unihan_variant(0x8B3E, kSimplifiedVariant, 0x8C29).
unicode_unihan_variant(0x8B41, kSemanticVariant, 0x5629). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8B41, kZVariant, 0x54D7).
unicode_unihan_variant(0x8B48, kSemanticVariant, 0x619D). %<kMatthews,kMeyerWempe 0x61DF<kMatthews
unicode_unihan_variant(0x8B49, kSemanticVariant, 0x8A3C). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8B49, kSimplifiedVariant, 0x8BC1).
unicode_unihan_variant(0x8B4A, kSimplifiedVariant, 0x2B362).
unicode_unihan_variant(0x8B4C, kSemanticVariant, 0x8A1B). %<kHKGlyph,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8B4C, kZVariant, 0x8A1B).
unicode_unihan_variant(0x8B4E, kSemanticVariant, 0x61B0). %<kMatthews
unicode_unihan_variant(0x8B4E, kSimplifiedVariant, 0x8C32).
unicode_unihan_variant(0x8B4F, kSimplifiedVariant, 0x8BA5).
unicode_unihan_variant(0x8B50, kSemanticVariant, 0x5642). %<kMeyerWempe
unicode_unihan_variant(0x8B56, kSimplifiedVariant, 0x8C2E).
unicode_unihan_variant(0x8B58, kSimplifiedVariant, 0x8BC6).
unicode_unihan_variant(0x8B59, kSemanticVariant, 0x8A9A). %<kFenn
unicode_unihan_variant(0x8B59, kSimplifiedVariant, 0x8C2F).
unicode_unihan_variant(0x8B5A, kSemanticVariant, 0x8AC7). %<kLau
unicode_unihan_variant(0x8B5A, kSimplifiedVariant, 0x8C2D).
unicode_unihan_variant(0x8B5C, kSimplifiedVariant, 0x8C31).
unicode_unihan_variant(0x8B5F, kSemanticVariant, 0x566A). %<kLau
unicode_unihan_variant(0x8B5F, kZVariant, 0x566A).
unicode_unihan_variant(0x8B66, kSemanticVariant, 0x5106). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8B6B, kSimplifiedVariant, 0x8C35).
unicode_unihan_variant(0x8B6D, kSemanticVariant, 0x46FC). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8B6F, kSimplifiedVariant, 0x8BD1).
unicode_unihan_variant(0x8B6F, kZVariant, 0x8A33).
unicode_unihan_variant(0x8B70, kSimplifiedVariant, 0x8BAE).
unicode_unihan_variant(0x8B71, kZVariant, 0x5584).
unicode_unihan_variant(0x8B72, kZVariant, 0x8B93).
unicode_unihan_variant(0x8B74, kSimplifiedVariant, 0x8C34).
unicode_unihan_variant(0x8B77, kSimplifiedVariant, 0x62A4).
unicode_unihan_variant(0x8B78, kSimplifiedVariant, 0x8BEA).
unicode_unihan_variant(0x8B7D, kSimplifiedVariant, 0x8A89).
unicode_unihan_variant(0x8B7D, kSpecializedSemanticVariant, 0x57DF). %<kFenn
unicode_unihan_variant(0x8B7E, kSimplifiedVariant, 0x8C2B).
unicode_unihan_variant(0x8B7E, kZVariant, 0x8B2D).
unicode_unihan_variant(0x8B80, kSimplifiedVariant, 0x8BFB).
unicode_unihan_variant(0x8B80, kZVariant, 0xF95A).
unicode_unihan_variant(0x8B81, kSemanticVariant, 0x8B2B). %<kMatthews
unicode_unihan_variant(0x8B83, kZVariant, 0x8B9A).
unicode_unihan_variant(0x8B85, kZVariant, 0x8C09).
unicode_unihan_variant(0x8B86, kZVariant, 0x8B8F).
unicode_unihan_variant(0x8B8A, kSemanticVariant, 0x53D8). %<kMatthews,kMeyerWempe 0x5909<kHanyu:T
unicode_unihan_variant(0x8B8A, kSimplifiedVariant, 0x53D8).
unicode_unihan_variant(0x8B8B, kSimplifiedVariant, 0x8A5F).
unicode_unihan_variant(0x8B8B, kZVariant, 0x8A5F).
unicode_unihan_variant(0x8B8C, kSemanticVariant, 0x5BB4). %<kLau,kMatthews
unicode_unihan_variant(0x8B8C, kSimplifiedVariant, 0x4729).
unicode_unihan_variant(0x8B8C, kZVariant, 0x71D5).
unicode_unihan_variant(0x8B8D, kSemanticVariant, 0x8AE4). %<kMatthews
unicode_unihan_variant(0x8B8E, kSemanticVariant, 0x4EC7). %<kMeyerWempe 0x8B90<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8B8E, kSimplifiedVariant, 0x96E0).
unicode_unihan_variant(0x8B8E, kSpecializedSemanticVariant, 0x4EC7). %<kFenn
unicode_unihan_variant(0x8B8E, kZVariant, 0x4EC7).
unicode_unihan_variant(0x8B90, kSemanticVariant, 0x4EC7). %<kLau,kMeyerWempe 0x8B8E<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8B90, kSpecializedSemanticVariant, 0x4EC7). %<kFenn
unicode_unihan_variant(0x8B90, kZVariant, 0x4EC7).
unicode_unihan_variant(0x8B92, kSimplifiedVariant, 0x8C17).
unicode_unihan_variant(0x8B93, kSimplifiedVariant, 0x8BA9).
unicode_unihan_variant(0x8B95, kSimplifiedVariant, 0x8C30).
unicode_unihan_variant(0x8B96, kSimplifiedVariant, 0x8C36).
unicode_unihan_variant(0x8B98, kSemanticVariant, 0x56C1). %<kMeyerWempe
unicode_unihan_variant(0x8B99, kZVariant, 0x6B61).
unicode_unihan_variant(0x8B9A, kZVariant, 0x8B83).
unicode_unihan_variant(0x8B9C, kSimplifiedVariant, 0x8C20).
unicode_unihan_variant(0x8B9E, kSimplifiedVariant, 0x8C33).
unicode_unihan_variant(0x8BA0, kTraditionalVariant, 0x8A01).
unicode_unihan_variant(0x8BA0, kZVariant, 0x8A00).
unicode_unihan_variant(0x8BA1, kTraditionalVariant, 0x8A08).
unicode_unihan_variant(0x8BA2, kTraditionalVariant, 0x8A02).
unicode_unihan_variant(0x8BA3, kTraditionalVariant, 0x8A03).
unicode_unihan_variant(0x8BA4, kTraditionalVariant, 0x8A8D).
unicode_unihan_variant(0x8BA5, kTraditionalVariant, 0x8B4F).
unicode_unihan_variant(0x8BA6, kTraditionalVariant, 0x8A10).
unicode_unihan_variant(0x8BA7, kTraditionalVariant, 0x8A0C).
unicode_unihan_variant(0x8BA8, kTraditionalVariant, 0x8A0E).
unicode_unihan_variant(0x8BA9, kTraditionalVariant, 0x8B93).
unicode_unihan_variant(0x8BAA, kTraditionalVariant, 0x8A15).
unicode_unihan_variant(0x8BAB, kTraditionalVariant, 0x8A16).
unicode_unihan_variant(0x8BAC, kTraditionalVariant, 0x8A17).
unicode_unihan_variant(0x8BAD, kTraditionalVariant, 0x8A13).
unicode_unihan_variant(0x8BAE, kTraditionalVariant, 0x8B70).
unicode_unihan_variant(0x8BAF, kTraditionalVariant, 0x8A0A).
unicode_unihan_variant(0x8BB0, kTraditionalVariant, 0x8A18).
unicode_unihan_variant(0x8BB1, kTraditionalVariant, 0x8A12).
unicode_unihan_variant(0x8BB2, kTraditionalVariant, 0x8B1B).
unicode_unihan_variant(0x8BB3, kTraditionalVariant, 0x8AF1).
unicode_unihan_variant(0x8BB4, kTraditionalVariant, 0x8B33).
unicode_unihan_variant(0x8BB5, kTraditionalVariant, 0x8A4E).
unicode_unihan_variant(0x8BB6, kTraditionalVariant, 0x8A1D).
unicode_unihan_variant(0x8BB7, kTraditionalVariant, 0x8A25).
unicode_unihan_variant(0x8BB8, kTraditionalVariant, 0x8A31).
unicode_unihan_variant(0x8BB9, kTraditionalVariant, 0x8A1B).
unicode_unihan_variant(0x8BBA, kTraditionalVariant, 0x8AD6).
unicode_unihan_variant(0x8BBB, kTraditionalVariant, 0x8A29).
unicode_unihan_variant(0x8BBC, kTraditionalVariant, 0x8A1F).
unicode_unihan_variant(0x8BBD, kTraditionalVariant, 0x8AF7).
unicode_unihan_variant(0x8BBE, kTraditionalVariant, 0x8A2D).
unicode_unihan_variant(0x8BBF, kTraditionalVariant, 0x8A2A).
unicode_unihan_variant(0x8BC0, kTraditionalVariant, 0x8A23).
unicode_unihan_variant(0x8BC1, kTraditionalVariant, 0x8B49).
unicode_unihan_variant(0x8BC2, kTraditionalVariant, 0x8A41).
unicode_unihan_variant(0x8BC3, kTraditionalVariant, 0x8A36).
unicode_unihan_variant(0x8BC3, kZVariant, 0x5475).
unicode_unihan_variant(0x8BC4, kTraditionalVariant, 0x8A55).
unicode_unihan_variant(0x8BC5, kTraditionalVariant, 0x8A5B).
unicode_unihan_variant(0x8BC6, kTraditionalVariant, 0x8B58).
unicode_unihan_variant(0x8BC7, kTraditionalVariant, 0x8A57).
unicode_unihan_variant(0x8BC8, kTraditionalVariant, 0x8A50).
unicode_unihan_variant(0x8BC9, kTraditionalVariant, 0x8A34).
unicode_unihan_variant(0x8BCA, kTraditionalVariant, 0x8A3A).
unicode_unihan_variant(0x8BCB, kTraditionalVariant, 0x8A46).
unicode_unihan_variant(0x8BCC, kTraditionalVariant, 0x8B05).
unicode_unihan_variant(0x8BCD, kTraditionalVariant, 0x8A5E).
unicode_unihan_variant(0x8BCE, kTraditionalVariant, 0x8A58).
unicode_unihan_variant(0x8BCF, kTraditionalVariant, 0x8A54).
unicode_unihan_variant(0x8BD0, kTraditionalVariant, 0x8A56).
unicode_unihan_variant(0x8BD1, kTraditionalVariant, 0x8B6F).
unicode_unihan_variant(0x8BD2, kTraditionalVariant, 0x8A52).
unicode_unihan_variant(0x8BD3, kTraditionalVariant, 0x8A86).
unicode_unihan_variant(0x8BD4, kTraditionalVariant, 0x8A84).
unicode_unihan_variant(0x8BD5, kTraditionalVariant, 0x8A66).
unicode_unihan_variant(0x8BD6, kTraditionalVariant, 0x8A7F).
unicode_unihan_variant(0x8BD7, kTraditionalVariant, 0x8A69).
unicode_unihan_variant(0x8BD8, kTraditionalVariant, 0x8A70).
unicode_unihan_variant(0x8BD9, kTraditionalVariant, 0x8A7C).
unicode_unihan_variant(0x8BDA, kTraditionalVariant, 0x8AA0).
unicode_unihan_variant(0x8BDB, kTraditionalVariant, 0x8A85).
unicode_unihan_variant(0x8BDC, kTraditionalVariant, 0x8A75).
unicode_unihan_variant(0x8BDD, kTraditionalVariant, 0x8A71).
unicode_unihan_variant(0x8BDE, kTraditionalVariant, 0x8A95).
unicode_unihan_variant(0x8BDF, kTraditionalVariant, 0x8A6C).
unicode_unihan_variant(0x8BE0, kTraditionalVariant, 0x8A6E).
unicode_unihan_variant(0x8BE1, kTraditionalVariant, 0x8A6D).
unicode_unihan_variant(0x8BE2, kTraditionalVariant, 0x8A62).
unicode_unihan_variant(0x8BE3, kTraditionalVariant, 0x8A63).
unicode_unihan_variant(0x8BE4, kTraditionalVariant, 0x8ACD).
unicode_unihan_variant(0x8BE5, kTraditionalVariant, 0x8A72).
unicode_unihan_variant(0x8BE6, kTraditionalVariant, 0x8A73).
unicode_unihan_variant(0x8BE7, kTraditionalVariant, 0x8A6B).
unicode_unihan_variant(0x8BE8, kTraditionalVariant, 0x8AE2).
unicode_unihan_variant(0x8BE9, kTraditionalVariant, 0x8A61).
unicode_unihan_variant(0x8BEA, kTraditionalVariant, 0x8B78).
unicode_unihan_variant(0x8BEB, kTraditionalVariant, 0x8AA1).
unicode_unihan_variant(0x8BEC, kTraditionalVariant, 0x8AA3).
unicode_unihan_variant(0x8BED, kTraditionalVariant, 0x8A9E).
unicode_unihan_variant(0x8BEE, kTraditionalVariant, 0x8A9A).
unicode_unihan_variant(0x8BEF, kTraditionalVariant, 0x8AA4).
unicode_unihan_variant(0x8BF0, kTraditionalVariant, 0x8AA5).
unicode_unihan_variant(0x8BF1, kTraditionalVariant, 0x8A98).
unicode_unihan_variant(0x8BF2, kTraditionalVariant, 0x8AA8).
unicode_unihan_variant(0x8BF3, kTraditionalVariant, 0x8A91).
unicode_unihan_variant(0x8BF4, kTraditionalVariant, 0x8AAA).
unicode_unihan_variant(0x8BF5, kTraditionalVariant, 0x8AA6).
unicode_unihan_variant(0x8BF6, kTraditionalVariant, 0x8A92).
unicode_unihan_variant(0x8BF6, kZVariant, 0x5509).
unicode_unihan_variant(0x8BF7, kTraditionalVariant, 0x8ACB).
unicode_unihan_variant(0x8BF8, kTraditionalVariant, 0x8AF8).
unicode_unihan_variant(0x8BF8, kZVariant, 0xFA22).
unicode_unihan_variant(0x8BF9, kTraditionalVariant, 0x8ACF).
unicode_unihan_variant(0x8BFA, kTraditionalVariant, 0x8AFE).
unicode_unihan_variant(0x8BFB, kTraditionalVariant, 0x8B80).
unicode_unihan_variant(0x8BFC, kTraditionalVariant, 0x8AD1).
unicode_unihan_variant(0x8BFD, kTraditionalVariant, 0x8AB9).
unicode_unihan_variant(0x8BFE, kTraditionalVariant, 0x8AB2).
unicode_unihan_variant(0x8BFF, kTraditionalVariant, 0x8AC9).
unicode_unihan_variant(0x8C00, kTraditionalVariant, 0x8ADB).
unicode_unihan_variant(0x8C01, kTraditionalVariant, 0x8AB0).
unicode_unihan_variant(0x8C02, kTraditionalVariant, 0x8AD7).
unicode_unihan_variant(0x8C03, kTraditionalVariant, 0x8ABF).
unicode_unihan_variant(0x8C04, kTraditionalVariant, 0x8AC2).
unicode_unihan_variant(0x8C05, kTraditionalVariant, 0x8AD2).
unicode_unihan_variant(0x8C06, kTraditionalVariant, 0x8AC4).
unicode_unihan_variant(0x8C07, kTraditionalVariant, 0x8AB6).
unicode_unihan_variant(0x8C08, kTraditionalVariant, 0x8AC7).
unicode_unihan_variant(0x8C09, kZVariant, 0x8B85).
unicode_unihan_variant(0x8C0A, kTraditionalVariant, 0x8ABC).
unicode_unihan_variant(0x8C0B, kTraditionalVariant, 0x8B00).
unicode_unihan_variant(0x8C0C, kTraditionalVariant, 0x8AF6).
unicode_unihan_variant(0x8C0D, kTraditionalVariant, 0x8ADC).
unicode_unihan_variant(0x8C0E, kTraditionalVariant, 0x8B0A).
unicode_unihan_variant(0x8C0F, kTraditionalVariant, 0x8AEB).
unicode_unihan_variant(0x8C10, kTraditionalVariant, 0x8AE7).
unicode_unihan_variant(0x8C11, kTraditionalVariant, 0x8B14).
unicode_unihan_variant(0x8C12, kTraditionalVariant, 0x8B01).
unicode_unihan_variant(0x8C13, kTraditionalVariant, 0x8B02).
unicode_unihan_variant(0x8C14, kTraditionalVariant, 0x8AE4).
unicode_unihan_variant(0x8C15, kTraditionalVariant, 0x8AED).
unicode_unihan_variant(0x8C16, kTraditionalVariant, 0x8AFC).
unicode_unihan_variant(0x8C17, kTraditionalVariant, 0x8B92).
unicode_unihan_variant(0x8C18, kTraditionalVariant, 0x8AEE).
unicode_unihan_variant(0x8C18, kZVariant, 0x54A8).
unicode_unihan_variant(0x8C19, kTraditionalVariant, 0x8AF3).
unicode_unihan_variant(0x8C1A, kTraditionalVariant, 0x8AFA).
unicode_unihan_variant(0x8C1B, kTraditionalVariant, 0x8AE6).
unicode_unihan_variant(0x8C1C, kTraditionalVariant, 0x8B0E).
unicode_unihan_variant(0x8C1D, kTraditionalVariant, 0x8ADE).
unicode_unihan_variant(0x8C1E, kTraditionalVariant, 0x8ADD).
unicode_unihan_variant(0x8C1F, kTraditionalVariant, 0x8B28).
unicode_unihan_variant(0x8C20, kTraditionalVariant, 0x8B9C).
unicode_unihan_variant(0x8C21, kTraditionalVariant, 0x8B16).
unicode_unihan_variant(0x8C22, kTraditionalVariant, 0x8B1D).
unicode_unihan_variant(0x8C23, kTraditionalVariant, 0x8B20).
unicode_unihan_variant(0x8C23, kZVariant, 0x8B20).
unicode_unihan_variant(0x8C24, kTraditionalVariant, 0x8B17).
unicode_unihan_variant(0x8C25, kTraditionalVariant, 0x8B1A).
unicode_unihan_variant(0x8C26, kTraditionalVariant, 0x8B19).
unicode_unihan_variant(0x8C27, kTraditionalVariant, 0x8B10).
unicode_unihan_variant(0x8C28, kTraditionalVariant, 0x8B39).
unicode_unihan_variant(0x8C29, kTraditionalVariant, 0x8B3E).
unicode_unihan_variant(0x8C2A, kTraditionalVariant, 0x8B2B).
unicode_unihan_variant(0x8C2B, kTraditionalVariant, 0x8B7E).
unicode_unihan_variant(0x8C2B, kZVariant, 0x8B2D).
unicode_unihan_variant(0x8C2C, kTraditionalVariant, 0x8B2C).
unicode_unihan_variant(0x8C2D, kTraditionalVariant, 0x8B5A).
unicode_unihan_variant(0x8C2E, kTraditionalVariant, 0x8B56).
unicode_unihan_variant(0x8C2F, kTraditionalVariant, 0x8B59).
unicode_unihan_variant(0x8C30, kTraditionalVariant, 0x8B95).
unicode_unihan_variant(0x8C31, kTraditionalVariant, 0x8B5C).
unicode_unihan_variant(0x8C32, kTraditionalVariant, 0x8B4E).
unicode_unihan_variant(0x8C33, kTraditionalVariant, 0x8B9E).
unicode_unihan_variant(0x8C34, kTraditionalVariant, 0x8B74).
unicode_unihan_variant(0x8C35, kTraditionalVariant, 0x8B6B).
unicode_unihan_variant(0x8C36, kTraditionalVariant, 0x8B96).
unicode_unihan_variant(0x8C37, kTraditionalVariant, 0x7A40).
unicode_unihan_variant(0x8C3F, kSemanticVariant, 0x5D60). %<kMeyerWempe
unicode_unihan_variant(0x8C46, kSemanticVariant, 0x8373). %<kLau
unicode_unihan_variant(0x8C47, kSemanticVariant, 0x4736). %<kMatthews
unicode_unihan_variant(0x8C48, kSimplifiedVariant, 0x5C82).
unicode_unihan_variant(0x8C4A, kZVariant, 0x8C50).
unicode_unihan_variant(0x8C4B, kSemanticVariant, 0x767B). %<kFenn
unicode_unihan_variant(0x8C4E, kSemanticVariant, 0x7AEA). %<kLau,kMatthews
unicode_unihan_variant(0x8C4E, kSimplifiedVariant, 0x7AD6).
unicode_unihan_variant(0x8C50, kSimplifiedVariant, 0x4E30).
unicode_unihan_variant(0x8C50, kZVariant, 0x8C4A).
unicode_unihan_variant(0x8C53, kSemanticVariant, 0x8276). %<kMatthews 0x8C54<kMatthews
unicode_unihan_variant(0x8C54, kSemanticVariant, 0x8276). %<kHKGlyph,kLau,kMatthews 0x8C53<kMatthews
unicode_unihan_variant(0x8C54, kZVariant, 0x8276).
unicode_unihan_variant(0x8C58, kSemanticVariant, 0x8C5A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8C5A, kSemanticVariant, 0x8C58). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8C5C, kSemanticVariant, 0x8C63). %<kMatthews
unicode_unihan_variant(0x8C5C, kZVariant, 0x8C63).
unicode_unihan_variant(0x8C5D, kSemanticVariant, 0x7F93). %<kMeyerWempe
unicode_unihan_variant(0x8C63, kSemanticVariant, 0x8C5C). %<kMatthews
unicode_unihan_variant(0x8C63, kZVariant, 0x8C5C).
unicode_unihan_variant(0x8C6B, kSemanticVariant, 0x9810). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8C6B, kZVariant, 0x4E88).
unicode_unihan_variant(0x8C6C, kSemanticVariant, 0x732A). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8C6C, kSimplifiedVariant, 0x732A).
unicode_unihan_variant(0x8C6E, kTraditionalVariant, 0x8C76).
unicode_unihan_variant(0x8C73, kSemanticVariant, 0x37D7). %<kMatthews 0x90A0<kMatthews
unicode_unihan_variant(0x8C74, kSemanticVariant, 0x8E62). %<kMeyerWempe
unicode_unihan_variant(0x8C75, kSemanticVariant, 0x474B). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8C76, kSimplifiedVariant, 0x8C6E).
unicode_unihan_variant(0x8C78, kSemanticVariant, 0x5ECC). %<kMatthews
unicode_unihan_variant(0x8C7A, kSemanticVariant, 0x72B2). %<kMatthews
unicode_unihan_variant(0x8C7A, kZVariant, 0x72B2).
unicode_unihan_variant(0x8C7B, kSemanticVariant, 0x72B4). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8C89, kZVariant, 0x72E2).
unicode_unihan_variant(0x8C8A, kZVariant, 0x8C98).
unicode_unihan_variant(0x8C8C, kSemanticVariant, 0x7683). %<kFenn
unicode_unihan_variant(0x8C8C, kZVariant, 0x7683).
unicode_unihan_variant(0x8C8D, kSemanticVariant, 0x72F8). %<kHKGlyph,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8C8E, kSemanticVariant, 0x7683). %<kMatthews
unicode_unihan_variant(0x8C93, kSemanticVariant, 0x732B). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8C93, kSimplifiedVariant, 0x732B).
unicode_unihan_variant(0x8C98, kZVariant, 0x8C8A).
unicode_unihan_variant(0x8C99, kSimplifiedVariant, 0x4759).
unicode_unihan_variant(0x8C9B, kSemanticVariant, 0x737E). %<kMatthews
unicode_unihan_variant(0x8C9D, kSimplifiedVariant, 0x8D1D).
unicode_unihan_variant(0x8C9E, kSimplifiedVariant, 0x8D1E).
unicode_unihan_variant(0x8C9F, kSimplifiedVariant, 0x8D20).
unicode_unihan_variant(0x8CA0, kSimplifiedVariant, 0x8D1F).
unicode_unihan_variant(0x8CA1, kSemanticVariant, 0x34B2). %<kMatthews
unicode_unihan_variant(0x8CA1, kSimplifiedVariant, 0x8D22).
unicode_unihan_variant(0x8CA1, kSpecializedSemanticVariant, 0x624D). %<kFenn
unicode_unihan_variant(0x8CA2, kSimplifiedVariant, 0x8D21).
unicode_unihan_variant(0x8CA7, kSimplifiedVariant, 0x8D2B).
unicode_unihan_variant(0x8CA8, kSimplifiedVariant, 0x8D27).
unicode_unihan_variant(0x8CA9, kSemanticVariant, 0x27DB6). %<kFenn
unicode_unihan_variant(0x8CA9, kSimplifiedVariant, 0x8D29).
unicode_unihan_variant(0x8CAA, kSimplifiedVariant, 0x8D2A).
unicode_unihan_variant(0x8CAB, kSimplifiedVariant, 0x8D2F).
unicode_unihan_variant(0x8CAC, kSimplifiedVariant, 0x8D23).
unicode_unihan_variant(0x8CAC, kSpecializedSemanticVariant, 0x62C6). %<kFenn
unicode_unihan_variant(0x8CAD, kSemanticVariant, 0x8CEA). %<kMatthews
unicode_unihan_variant(0x8CAD, kZVariant, 0x8CEA).
unicode_unihan_variant(0x8CAE, kZVariant, 0x4E8C).
unicode_unihan_variant(0x8CAF, kSimplifiedVariant, 0x8D2E).
unicode_unihan_variant(0x8CB0, kSimplifiedVariant, 0x8D33).
unicode_unihan_variant(0x8CB2, kSimplifiedVariant, 0x8D40).
unicode_unihan_variant(0x8CB3, kSemanticVariant, 0x4E8C). %<kMeyerWempe 0x5F0D<kMeyerWempe
unicode_unihan_variant(0x8CB3, kSimplifiedVariant, 0x8D30).
unicode_unihan_variant(0x8CB3, kSpecializedSemanticVariant, 0x4E8C). %0x5F0D
unicode_unihan_variant(0x8CB3, kZVariant, 0x4E8C).
unicode_unihan_variant(0x8CB4, kSimplifiedVariant, 0x8D35).
unicode_unihan_variant(0x8CB6, kSimplifiedVariant, 0x8D2C).
unicode_unihan_variant(0x8CB7, kSimplifiedVariant, 0x4E70).
unicode_unihan_variant(0x8CB8, kSimplifiedVariant, 0x8D37).
unicode_unihan_variant(0x8CBA, kSimplifiedVariant, 0x8D36).
unicode_unihan_variant(0x8CBB, kSimplifiedVariant, 0x8D39).
unicode_unihan_variant(0x8CBC, kSimplifiedVariant, 0x8D34).
unicode_unihan_variant(0x8CBD, kSimplifiedVariant, 0x8D3B).
unicode_unihan_variant(0x8CBF, kSimplifiedVariant, 0x8D38).
unicode_unihan_variant(0x8CC0, kSimplifiedVariant, 0x8D3A).
unicode_unihan_variant(0x8CC1, kSimplifiedVariant, 0x8D32).
unicode_unihan_variant(0x8CC2, kSimplifiedVariant, 0x8D42).
unicode_unihan_variant(0x8CC3, kSimplifiedVariant, 0x8D41).
unicode_unihan_variant(0x8CC4, kSimplifiedVariant, 0x8D3F).
unicode_unihan_variant(0x8CC5, kSemanticVariant, 0x4F85). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8CC5, kSimplifiedVariant, 0x8D45).
unicode_unihan_variant(0x8CC7, kSimplifiedVariant, 0x8D44).
unicode_unihan_variant(0x8CC8, kSimplifiedVariant, 0x8D3E).
unicode_unihan_variant(0x8CC9, kZVariant, 0x6064).
unicode_unihan_variant(0x8CCA, kSimplifiedVariant, 0x8D3C).
unicode_unihan_variant(0x8CCA, kZVariant, 0x621D).
unicode_unihan_variant(0x8CCD, kSemanticVariant, 0x8D13). %<kLau,kMatthews,kMeyerWempe 0x8D1C<kFenn
unicode_unihan_variant(0x8CD1, kSimplifiedVariant, 0x8D48).
unicode_unihan_variant(0x8CD2, kSimplifiedVariant, 0x8D4A).
unicode_unihan_variant(0x8CD3, kSemanticVariant, 0x8CD4). %<kMatthews
unicode_unihan_variant(0x8CD3, kSimplifiedVariant, 0x5BBE).
unicode_unihan_variant(0x8CD4, kSemanticVariant, 0x8CD3). %<kMatthews
unicode_unihan_variant(0x8CD4, kZVariant, 0x8CD3).
unicode_unihan_variant(0x8CD5, kSimplifiedVariant, 0x8D47).
unicode_unihan_variant(0x8CD9, kSimplifiedVariant, 0x8D52).
unicode_unihan_variant(0x8CDA, kSimplifiedVariant, 0x8D49).
unicode_unihan_variant(0x8CDB, kSemanticVariant, 0x8D0A). %<kLau,kMatthews
unicode_unihan_variant(0x8CDB, kZVariant, 0x8D0A).
unicode_unihan_variant(0x8CDC, kSimplifiedVariant, 0x8D50).
unicode_unihan_variant(0x8CDE, kSimplifiedVariant, 0x8D4F).
unicode_unihan_variant(0x8CDF, kSimplifiedVariant, 0x27E56).
unicode_unihan_variant(0x8CE0, kSimplifiedVariant, 0x8D54).
unicode_unihan_variant(0x8CE1, kSimplifiedVariant, 0x8D53).
unicode_unihan_variant(0x8CE2, kSemanticVariant, 0x8D12). %<kMatthews
unicode_unihan_variant(0x8CE2, kSimplifiedVariant, 0x8D24).
unicode_unihan_variant(0x8CE3, kSimplifiedVariant, 0x5356).
unicode_unihan_variant(0x8CE3, kZVariant, 0x58F2).
unicode_unihan_variant(0x8CE4, kSimplifiedVariant, 0x8D31).
unicode_unihan_variant(0x8CE6, kSimplifiedVariant, 0x8D4B).
unicode_unihan_variant(0x8CE7, kSimplifiedVariant, 0x8D55).
unicode_unihan_variant(0x8CEA, kSemanticVariant, 0x8CAD). %<kMatthews
unicode_unihan_variant(0x8CEA, kSimplifiedVariant, 0x8D28).
unicode_unihan_variant(0x8CEA, kZVariant, 0x8CAD).
unicode_unihan_variant(0x8CEB, kSemanticVariant, 0x9F4E). %<kMatthews
unicode_unihan_variant(0x8CEC, kSemanticVariant, 0x5E33). %<kLau
unicode_unihan_variant(0x8CEC, kSimplifiedVariant, 0x8D26).
unicode_unihan_variant(0x8CEC, kZVariant, 0x5E33).
unicode_unihan_variant(0x8CED, kSimplifiedVariant, 0x8D4C).
unicode_unihan_variant(0x8CEE, kSemanticVariant, 0x8D10). %<kMatthews
unicode_unihan_variant(0x8CF0, kSimplifiedVariant, 0x4790).
unicode_unihan_variant(0x8CF4, kSemanticVariant, 0x983C). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8CF4, kSimplifiedVariant, 0x8D56).
unicode_unihan_variant(0x8CF5, kSimplifiedVariant, 0x8D57).
unicode_unihan_variant(0x8CF8, kSemanticVariant, 0x5269). %<kLau,kMatthews
unicode_unihan_variant(0x8CF8, kZVariant, 0x5269).
unicode_unihan_variant(0x8CFA, kSemanticVariant, 0x8D03). %<kLau,kMatthews
unicode_unihan_variant(0x8CFA, kSimplifiedVariant, 0x8D5A).
unicode_unihan_variant(0x8CFA, kSpecializedSemanticVariant, 0x8D03). %<kFenn 0x27E16<kFenn
unicode_unihan_variant(0x8CFB, kSimplifiedVariant, 0x8D59).
unicode_unihan_variant(0x8CFC, kSimplifiedVariant, 0x8D2D).
unicode_unihan_variant(0x8CFD, kSimplifiedVariant, 0x8D5B).
unicode_unihan_variant(0x8CFE, kSimplifiedVariant, 0x8D5C).
unicode_unihan_variant(0x8D03, kSemanticVariant, 0x8CFA). %<kLau,kMatthews 0x27E16<kFenn
unicode_unihan_variant(0x8D03, kSimplifiedVariant, 0x27E57).
unicode_unihan_variant(0x8D03, kSpecializedSemanticVariant, 0x8CFA). %<kFenn
unicode_unihan_variant(0x8D04, kSimplifiedVariant, 0x8D3D).
unicode_unihan_variant(0x8D05, kSimplifiedVariant, 0x8D58).
unicode_unihan_variant(0x8D07, kSimplifiedVariant, 0x8D5F).
unicode_unihan_variant(0x8D08, kSimplifiedVariant, 0x8D60).
unicode_unihan_variant(0x8D0A, kSemanticVariant, 0x8CDB). %<kLau,kMatthews
unicode_unihan_variant(0x8D0A, kSimplifiedVariant, 0x8D5E).
unicode_unihan_variant(0x8D0A, kZVariant, 0x8CDB).
unicode_unihan_variant(0x8D0B, kSemanticVariant, 0x5050). %<kMatthews 0x8D17<kMatthews
unicode_unihan_variant(0x8D0D, kSimplifiedVariant, 0x8D61).
unicode_unihan_variant(0x8D0F, kSimplifiedVariant, 0x8D62).
unicode_unihan_variant(0x8D10, kSemanticVariant, 0x8CEE). %<kMatthews
unicode_unihan_variant(0x8D10, kSimplifiedVariant, 0x8D46).
unicode_unihan_variant(0x8D12, kSemanticVariant, 0x8CE2). %<kMatthews
unicode_unihan_variant(0x8D13, kSemanticVariant, 0x8CCD). %<kLau,kMatthews,kMeyerWempe 0x8D1C<kFenn
unicode_unihan_variant(0x8D13, kSimplifiedVariant, 0x8D43).
unicode_unihan_variant(0x8D14, kSimplifiedVariant, 0x8D51).
unicode_unihan_variant(0x8D16, kSimplifiedVariant, 0x8D4E).
unicode_unihan_variant(0x8D17, kSemanticVariant, 0x5050). %<kMatthews 0x8D0B<kMatthews
unicode_unihan_variant(0x8D17, kSimplifiedVariant, 0x8D5D).
unicode_unihan_variant(0x8D17, kZVariant, 0x8D0B).
unicode_unihan_variant(0x8D1B, kSimplifiedVariant, 0x8D63).
unicode_unihan_variant(0x8D1C, kSemanticVariant, 0x8CCD). %<kFenn 0x8D13<kFenn
unicode_unihan_variant(0x8D1C, kZVariant, 0x8D13).
unicode_unihan_variant(0x8D1D, kTraditionalVariant, 0x8C9D).
unicode_unihan_variant(0x8D1E, kTraditionalVariant, 0x8C9E).
unicode_unihan_variant(0x8D1F, kTraditionalVariant, 0x8CA0).
unicode_unihan_variant(0x8D20, kTraditionalVariant, 0x8C9F).
unicode_unihan_variant(0x8D21, kTraditionalVariant, 0x8CA2).
unicode_unihan_variant(0x8D22, kTraditionalVariant, 0x8CA1).
unicode_unihan_variant(0x8D23, kTraditionalVariant, 0x8CAC).
unicode_unihan_variant(0x8D24, kTraditionalVariant, 0x8CE2).
unicode_unihan_variant(0x8D25, kTraditionalVariant, 0x6557).
unicode_unihan_variant(0x8D26, kTraditionalVariant, 0x8CEC).
unicode_unihan_variant(0x8D26, kZVariant, 0x5E33).
unicode_unihan_variant(0x8D27, kTraditionalVariant, 0x8CA8).
unicode_unihan_variant(0x8D28, kTraditionalVariant, 0x8CEA).
unicode_unihan_variant(0x8D29, kTraditionalVariant, 0x8CA9).
unicode_unihan_variant(0x8D2A, kTraditionalVariant, 0x8CAA).
unicode_unihan_variant(0x8D2B, kTraditionalVariant, 0x8CA7).
unicode_unihan_variant(0x8D2C, kTraditionalVariant, 0x8CB6).
unicode_unihan_variant(0x8D2D, kTraditionalVariant, 0x8CFC).
unicode_unihan_variant(0x8D2E, kTraditionalVariant, 0x8CAF).
unicode_unihan_variant(0x8D2F, kTraditionalVariant, 0x8CAB).
unicode_unihan_variant(0x8D30, kTraditionalVariant, 0x8CB3).
unicode_unihan_variant(0x8D30, kZVariant, 0x4E8C).
unicode_unihan_variant(0x8D31, kTraditionalVariant, 0x8CE4).
unicode_unihan_variant(0x8D32, kTraditionalVariant, 0x8CC1).
unicode_unihan_variant(0x8D33, kTraditionalVariant, 0x8CB0).
unicode_unihan_variant(0x8D34, kTraditionalVariant, 0x8CBC).
unicode_unihan_variant(0x8D35, kTraditionalVariant, 0x8CB4).
unicode_unihan_variant(0x8D36, kTraditionalVariant, 0x8CBA).
unicode_unihan_variant(0x8D37, kTraditionalVariant, 0x8CB8).
unicode_unihan_variant(0x8D38, kTraditionalVariant, 0x8CBF).
unicode_unihan_variant(0x8D39, kTraditionalVariant, 0x8CBB).
unicode_unihan_variant(0x8D3A, kTraditionalVariant, 0x8CC0).
unicode_unihan_variant(0x8D3B, kTraditionalVariant, 0x8CBD).
unicode_unihan_variant(0x8D3C, kTraditionalVariant, 0x8CCA).
unicode_unihan_variant(0x8D3D, kTraditionalVariant, 0x8D04).
unicode_unihan_variant(0x8D3E, kTraditionalVariant, 0x8CC8).
unicode_unihan_variant(0x8D3F, kTraditionalVariant, 0x8CC4).
unicode_unihan_variant(0x8D40, kTraditionalVariant, 0x8CB2).
unicode_unihan_variant(0x8D41, kTraditionalVariant, 0x8CC3).
unicode_unihan_variant(0x8D42, kTraditionalVariant, 0x8CC2).
unicode_unihan_variant(0x8D43, kTraditionalVariant, 0x8D13).
unicode_unihan_variant(0x8D44, kTraditionalVariant, 0x8CC7).
unicode_unihan_variant(0x8D45, kTraditionalVariant, 0x8CC5).
unicode_unihan_variant(0x8D46, kTraditionalVariant, 0x8D10).
unicode_unihan_variant(0x8D47, kTraditionalVariant, 0x8CD5).
unicode_unihan_variant(0x8D48, kTraditionalVariant, 0x8CD1).
unicode_unihan_variant(0x8D49, kTraditionalVariant, 0x8CDA).
unicode_unihan_variant(0x8D4A, kTraditionalVariant, 0x8CD2).
unicode_unihan_variant(0x8D4B, kTraditionalVariant, 0x8CE6).
unicode_unihan_variant(0x8D4C, kTraditionalVariant, 0x8CED).
unicode_unihan_variant(0x8D4D, kTraditionalVariant, 0x9F4E).
unicode_unihan_variant(0x8D4E, kTraditionalVariant, 0x8D16).
unicode_unihan_variant(0x8D4F, kTraditionalVariant, 0x8CDE).
unicode_unihan_variant(0x8D50, kTraditionalVariant, 0x8CDC).
unicode_unihan_variant(0x8D51, kTraditionalVariant, 0x8D14).
unicode_unihan_variant(0x8D52, kTraditionalVariant, 0x8CD9).
unicode_unihan_variant(0x8D53, kTraditionalVariant, 0x8CE1).
unicode_unihan_variant(0x8D54, kTraditionalVariant, 0x8CE0).
unicode_unihan_variant(0x8D55, kTraditionalVariant, 0x8CE7).
unicode_unihan_variant(0x8D56, kTraditionalVariant, 0x8CF4).
unicode_unihan_variant(0x8D57, kTraditionalVariant, 0x8CF5).
unicode_unihan_variant(0x8D58, kTraditionalVariant, 0x8D05).
unicode_unihan_variant(0x8D59, kTraditionalVariant, 0x8CFB).
unicode_unihan_variant(0x8D5A, kTraditionalVariant, 0x8CFA).
unicode_unihan_variant(0x8D5B, kTraditionalVariant, 0x8CFD).
unicode_unihan_variant(0x8D5C, kTraditionalVariant, 0x8CFE).
unicode_unihan_variant(0x8D5D, kTraditionalVariant, 0x8D17).
unicode_unihan_variant(0x8D5D, kZVariant, 0x8D0B).
unicode_unihan_variant(0x8D5E, kTraditionalVariant, 0x8D0A).
unicode_unihan_variant(0x8D5F, kTraditionalVariant, 0x8D07).
unicode_unihan_variant(0x8D60, kTraditionalVariant, 0x8D08).
unicode_unihan_variant(0x8D61, kTraditionalVariant, 0x8D0D).
unicode_unihan_variant(0x8D62, kTraditionalVariant, 0x8D0F).
unicode_unihan_variant(0x8D63, kTraditionalVariant, 0x8D1B).
unicode_unihan_variant(0x8D64, kSemanticVariant, 0x2418D). %<kFenn
unicode_unihan_variant(0x8D6A, kTraditionalVariant, 0x8D6C).
unicode_unihan_variant(0x8D6C, kSemanticVariant, 0x4793). %<kMeyerWempe
unicode_unihan_variant(0x8D6C, kSimplifiedVariant, 0x8D6A).
unicode_unihan_variant(0x8D70, kZVariant, 0x8D71).
unicode_unihan_variant(0x8D71, kZVariant, 0x8D70).
unicode_unihan_variant(0x8D75, kTraditionalVariant, 0x8D99).
unicode_unihan_variant(0x8D76, kSemanticVariant, 0x8D95). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8D76, kTraditionalVariant, 0x8D95).
unicode_unihan_variant(0x8D81, kSemanticVariant, 0x8D82). %<kMatthews
unicode_unihan_variant(0x8D81, kZVariant, 0x8D82).
unicode_unihan_variant(0x8D82, kSemanticVariant, 0x8D81). %<kMatthews
unicode_unihan_variant(0x8D82, kZVariant, 0x8D81).
unicode_unihan_variant(0x8D84, kSemanticVariant, 0x8DD9). %<kMatthews
unicode_unihan_variant(0x8D84, kSpecializedSemanticVariant, 0x8DD9). %<kMeyerWempe
unicode_unihan_variant(0x8D8B, kSemanticVariant, 0x8DA8). %<kMatthews
unicode_unihan_variant(0x8D8B, kTraditionalVariant, 0x8DA8).
unicode_unihan_variant(0x8D92, kSemanticVariant, 0x8DF3). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8D95, kSemanticVariant, 0x8D76). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8D95, kSimplifiedVariant, 0x8D76).
unicode_unihan_variant(0x8D99, kSimplifiedVariant, 0x8D75).
unicode_unihan_variant(0x8DA8, kSemanticVariant, 0x8D8B). %<kMatthews
unicode_unihan_variant(0x8DA8, kSimplifiedVariant, 0x8D8B).
unicode_unihan_variant(0x8DAE, kSemanticVariant, 0x8E81). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8DAF, kSemanticVariant, 0x8E8D). %<kMatthews
unicode_unihan_variant(0x8DB1, kTraditionalVariant, 0x8DB2).
unicode_unihan_variant(0x8DB2, kSimplifiedVariant, 0x8DB1).
unicode_unihan_variant(0x8DB8, kTraditionalVariant, 0x8E89).
unicode_unihan_variant(0x8DC3, kTraditionalVariant, 0x8E8D).
unicode_unihan_variant(0x8DC4, kTraditionalVariant, 0x8E4C).
unicode_unihan_variant(0x8DD0, kSpecializedSemanticVariant, 0x8DF4). %<kFenn
unicode_unihan_variant(0x8DD6, kSemanticVariant, 0x8E60). %<kMeyerWempe
unicode_unihan_variant(0x8DD9, kSemanticVariant, 0x8D84). %<kMatthews
unicode_unihan_variant(0x8DD9, kSpecializedSemanticVariant, 0x8D84). %<kMeyerWempe
unicode_unihan_variant(0x8DDE, kTraditionalVariant, 0x8E92).
unicode_unihan_variant(0x8DE1, kSemanticVariant, 0x8E5F). %<kHKGlyph,kLau,kMatthews,kMeyerWempe 0x8FF9<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x8DE1, kSimplifiedVariant, 0x8FF9).
unicode_unihan_variant(0x8DE1, kSpecializedSemanticVariant, 0x901F). %<kMeyerWempe
unicode_unihan_variant(0x8DE5, kSemanticVariant, 0x8DFA). %<kMatthews
unicode_unihan_variant(0x8DE5, kZVariant, 0x8DFA).
unicode_unihan_variant(0x8DEF, kZVariant, 0xF937).
unicode_unihan_variant(0x8DF3, kSemanticVariant, 0x8D92). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8DF4, kSemanticVariant, 0x8E39). %<kMatthews
unicode_unihan_variant(0x8DF4, kSpecializedSemanticVariant, 0x8DD0). %<kFenn
unicode_unihan_variant(0x8DF5, kTraditionalVariant, 0x8E10).
unicode_unihan_variant(0x8DF6, kTraditionalVariant, 0x8E82).
unicode_unihan_variant(0x8DF7, kTraditionalVariant, 0x8E7A).
unicode_unihan_variant(0x8DF8, kTraditionalVariant, 0x8E55).
unicode_unihan_variant(0x8DF9, kTraditionalVariant, 0x8E9A).
unicode_unihan_variant(0x8DFA, kSemanticVariant, 0x8DE5). %<kMatthews
unicode_unihan_variant(0x8DFA, kZVariant, 0x8DE5).
unicode_unihan_variant(0x8DFB, kTraditionalVariant, 0x8E8B).
unicode_unihan_variant(0x8E01, kSemanticVariant, 0x785C). %<kMeyerWempe
unicode_unihan_variant(0x8E0A, kSemanticVariant, 0x8E34). %<kLau,kMatthews
unicode_unihan_variant(0x8E0A, kTraditionalVariant, 0x8E34).
unicode_unihan_variant(0x8E0C, kTraditionalVariant, 0x8E8A).
unicode_unihan_variant(0x8E0F, kSemanticVariant, 0x8E4B). %<kMeyerWempe
unicode_unihan_variant(0x8E10, kSimplifiedVariant, 0x8DF5).
unicode_unihan_variant(0x8E26, kSpecializedSemanticVariant, 0x7284). %<kFenn
unicode_unihan_variant(0x8E28, kZVariant, 0x8E64).
unicode_unihan_variant(0x8E2A, kSemanticVariant, 0x8E64). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x8E2A, kTraditionalVariant, 0x8E64).
unicode_unihan_variant(0x8E2C, kTraditionalVariant, 0x8E93).
unicode_unihan_variant(0x8E2D, kSemanticVariant, 0x3B39). %<kMeyerWempe
unicode_unihan_variant(0x8E2F, kTraditionalVariant, 0x8E91).
unicode_unihan_variant(0x8E30, kSemanticVariant, 0x903E). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8E34, kSemanticVariant, 0x8E0A). %<kLau,kMatthews
unicode_unihan_variant(0x8E34, kSimplifiedVariant, 0x8E0A).
unicode_unihan_variant(0x8E35, kSemanticVariant, 0x6B71). %<kFenn
unicode_unihan_variant(0x8E39, kSemanticVariant, 0x8DF4). %<kMatthews
unicode_unihan_variant(0x8E3D, kSemanticVariant, 0x504A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8E44, kSemanticVariant, 0x8E4F). %<kLau,kMatthews
unicode_unihan_variant(0x8E4B, kSemanticVariant, 0x8E0F). %<kMeyerWempe 0x8E79<kMatthews
unicode_unihan_variant(0x8E4C, kSemanticVariant, 0x7244). %<kMatthews
unicode_unihan_variant(0x8E4C, kSimplifiedVariant, 0x8DC4).
unicode_unihan_variant(0x8E4F, kSemanticVariant, 0x8E44). %<kLau,kMatthews
unicode_unihan_variant(0x8E51, kTraditionalVariant, 0x8EA1).
unicode_unihan_variant(0x8E52, kTraditionalVariant, 0x8E63).
unicode_unihan_variant(0x8E55, kSimplifiedVariant, 0x8DF8).
unicode_unihan_variant(0x8E5D, kSemanticVariant, 0x5C63). %<kMatthews 0x8EA7<kMatthews
unicode_unihan_variant(0x8E5F, kSemanticVariant, 0x8DE1). %<kHKGlyph,kLau,kMatthews,kMeyerWempe 0x8FF9<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x8E5F, kSpecializedSemanticVariant, 0x901F). %<kMeyerWempe
unicode_unihan_variant(0x8E5F, kZVariant, 0x8DE1).
unicode_unihan_variant(0x8E60, kSemanticVariant, 0x8DD6). %<kMeyerWempe
unicode_unihan_variant(0x8E62, kSemanticVariant, 0x8C74). %<kMeyerWempe
unicode_unihan_variant(0x8E63, kSimplifiedVariant, 0x8E52).
unicode_unihan_variant(0x8E64, kSemanticVariant, 0x8E2A). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x8E64, kSimplifiedVariant, 0x8E2A).
unicode_unihan_variant(0x8E64, kZVariant, 0x8E28).
unicode_unihan_variant(0x8E67, kSemanticVariant, 0x906D). %<kLau,kMatthews
unicode_unihan_variant(0x8E6D, kSpecializedSemanticVariant, 0x77F0). %<kMeyerWempe
unicode_unihan_variant(0x8E6E, kSemanticVariant, 0x8E9A). %<kMeyerWempe
unicode_unihan_variant(0x8E70, kTraditionalVariant, 0x8E95).
unicode_unihan_variant(0x8E74, kSemanticVariant, 0x8E75). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8E75, kSemanticVariant, 0x8E74). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8E76, kSpecializedSemanticVariant, 0x8E77). %<kFenn
unicode_unihan_variant(0x8E77, kSpecializedSemanticVariant, 0x8E76). %<kFenn
unicode_unihan_variant(0x8E79, kSemanticVariant, 0x8E4B). %<kMatthews
unicode_unihan_variant(0x8E7A, kSemanticVariant, 0x8E7B). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8E7A, kSimplifiedVariant, 0x8DF7).
unicode_unihan_variant(0x8E7B, kSemanticVariant, 0x8E7A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8E7B, kSimplifiedVariant, 0x2B3CB).
unicode_unihan_variant(0x8E7F, kTraditionalVariant, 0x8EA5).
unicode_unihan_variant(0x8E81, kSemanticVariant, 0x8DAE). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8E82, kSimplifiedVariant, 0x8DF6).
unicode_unihan_variant(0x8E85, kSemanticVariant, 0x4831). %<kMatthews
unicode_unihan_variant(0x8E89, kSimplifiedVariant, 0x8DB8).
unicode_unihan_variant(0x8E8A, kSimplifiedVariant, 0x8E0C).
unicode_unihan_variant(0x8E8B, kSemanticVariant, 0x96AE). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8E8B, kSimplifiedVariant, 0x8DFB).
unicode_unihan_variant(0x8E8D, kSemanticVariant, 0x8DAF). %<kMatthews
unicode_unihan_variant(0x8E8D, kSimplifiedVariant, 0x8DC3).
unicode_unihan_variant(0x8E8E, kSimplifiedVariant, 0x47E2).
unicode_unihan_variant(0x8E8F, kTraditionalVariant, 0x8EAA).
unicode_unihan_variant(0x8E91, kSimplifiedVariant, 0x8E2F).
unicode_unihan_variant(0x8E92, kSimplifiedVariant, 0x8DDE).
unicode_unihan_variant(0x8E93, kSimplifiedVariant, 0x8E2C).
unicode_unihan_variant(0x8E95, kSimplifiedVariant, 0x8E70).
unicode_unihan_variant(0x8E97, kSemanticVariant, 0x8E9B). %<kMatthews
unicode_unihan_variant(0x8E97, kZVariant, 0x8E9B).
unicode_unihan_variant(0x8E9A, kSemanticVariant, 0x8E6E). %<kMeyerWempe
unicode_unihan_variant(0x8E9A, kSimplifiedVariant, 0x8DF9).
unicode_unihan_variant(0x8E9B, kSemanticVariant, 0x8E97). %<kMatthews
unicode_unihan_variant(0x8E9B, kZVariant, 0x8E97).
unicode_unihan_variant(0x8E9C, kTraditionalVariant, 0x8EA6).
unicode_unihan_variant(0x8E9D, kSimplifiedVariant, 0x2816C).
unicode_unihan_variant(0x8EA1, kSimplifiedVariant, 0x8E51).
unicode_unihan_variant(0x8EA5, kSimplifiedVariant, 0x8E7F).
unicode_unihan_variant(0x8EA6, kSimplifiedVariant, 0x8E9C).
unicode_unihan_variant(0x8EA7, kSemanticVariant, 0x5C63). %<kMatthews 0x8E5D<kMatthews
unicode_unihan_variant(0x8EAA, kSimplifiedVariant, 0x8E8F).
unicode_unihan_variant(0x8EAC, kSemanticVariant, 0x8EB3). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8EAD, kSemanticVariant, 0x7708). %<kMatthews
unicode_unihan_variant(0x8EAD, kZVariant, 0x803D).
unicode_unihan_variant(0x8EAF, kTraditionalVariant, 0x8EC0).
unicode_unihan_variant(0x8EB0, kZVariant, 0x9AD4).
unicode_unihan_variant(0x8EB1, kSemanticVariant, 0x8EB2). %<kMatthews
unicode_unihan_variant(0x8EB1, kZVariant, 0x8EB2).
unicode_unihan_variant(0x8EB2, kSemanticVariant, 0x8EB1). %<kMatthews
unicode_unihan_variant(0x8EB2, kZVariant, 0x8EB1).
unicode_unihan_variant(0x8EB3, kSemanticVariant, 0x8EAC). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8EB6, kSemanticVariant, 0x88F8). %<kLau,kMatthews
unicode_unihan_variant(0x8EB7, kSemanticVariant, 0x77EE). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8EC0, kSimplifiedVariant, 0x8EAF).
unicode_unihan_variant(0x8EC4, kSemanticVariant, 0x8077). %<kLau,kMatthews
unicode_unihan_variant(0x8EC6, kSemanticVariant, 0x4F53). %<kFenn
unicode_unihan_variant(0x8EC6, kZVariant, 0x9AD4).
unicode_unihan_variant(0x8EC9, kSimplifiedVariant, 0x28257).
unicode_unihan_variant(0x8ECA, kSimplifiedVariant, 0x8F66).
unicode_unihan_variant(0x8ECB, kSimplifiedVariant, 0x8F67).
unicode_unihan_variant(0x8ECC, kSimplifiedVariant, 0x8F68).
unicode_unihan_variant(0x8ECD, kSimplifiedVariant, 0x519B).
unicode_unihan_variant(0x8ECF, kSimplifiedVariant, 0x2B404).
unicode_unihan_variant(0x8ED1, kSimplifiedVariant, 0x8F6A).
unicode_unihan_variant(0x8ED2, kSimplifiedVariant, 0x8F69).
unicode_unihan_variant(0x8ED4, kSimplifiedVariant, 0x8F6B).
unicode_unihan_variant(0x8ED7, kSimplifiedVariant, 0x28405).
unicode_unihan_variant(0x8EDB, kSimplifiedVariant, 0x8F6D).
unicode_unihan_variant(0x8EDF, kSemanticVariant, 0x8F2D). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x8EDF, kSimplifiedVariant, 0x8F6F).
unicode_unihan_variant(0x8EE2, kZVariant, 0x8F49).
unicode_unihan_variant(0x8EE3, kZVariant, 0x8F5F).
unicode_unihan_variant(0x8EE4, kSimplifiedVariant, 0x8F77).
unicode_unihan_variant(0x8EE8, kSimplifiedVariant, 0x2B409).
unicode_unihan_variant(0x8EEB, kSimplifiedVariant, 0x8F78).
unicode_unihan_variant(0x8EF0, kSemanticVariant, 0x8F29). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8EF2, kSimplifiedVariant, 0x8F71).
unicode_unihan_variant(0x8EF2, kSpecializedSemanticVariant, 0x8F42). %<kFenn
unicode_unihan_variant(0x8EF8, kSimplifiedVariant, 0x8F74).
unicode_unihan_variant(0x8EF9, kSimplifiedVariant, 0x8F75).
unicode_unihan_variant(0x8EFA, kSemanticVariant, 0x28373). %<kMeyerWempe
unicode_unihan_variant(0x8EFA, kSimplifiedVariant, 0x8F7A).
unicode_unihan_variant(0x8EFB, kSimplifiedVariant, 0x8F72).
unicode_unihan_variant(0x8EFC, kSimplifiedVariant, 0x8F76).
unicode_unihan_variant(0x8EFD, kZVariant, 0x8F15).
unicode_unihan_variant(0x8EFE, kSimplifiedVariant, 0x8F7C).
unicode_unihan_variant(0x8EFF, kZVariant, 0x8F27).
unicode_unihan_variant(0x8F03, kSimplifiedVariant, 0x8F83).
unicode_unihan_variant(0x8F04, kSimplifiedVariant, 0x28408).
unicode_unihan_variant(0x8F05, kSimplifiedVariant, 0x8F82).
unicode_unihan_variant(0x8F07, kSimplifiedVariant, 0x8F81).
unicode_unihan_variant(0x8F08, kSimplifiedVariant, 0x8F80).
unicode_unihan_variant(0x8F09, kSimplifiedVariant, 0x8F7D).
unicode_unihan_variant(0x8F09, kSpecializedSemanticVariant, 0x510E). %<kFenn
unicode_unihan_variant(0x8F0A, kSimplifiedVariant, 0x8F7E).
unicode_unihan_variant(0x8F0C, kZVariant, 0x8F1B).
unicode_unihan_variant(0x8F12, kSemanticVariant, 0x8F19). %<kMatthews
unicode_unihan_variant(0x8F12, kSimplifiedVariant, 0x8F84).
unicode_unihan_variant(0x8F13, kSimplifiedVariant, 0x633D).
unicode_unihan_variant(0x8F14, kSimplifiedVariant, 0x8F85).
unicode_unihan_variant(0x8F15, kSimplifiedVariant, 0x8F7B).
unicode_unihan_variant(0x8F17, kSimplifiedVariant, 0x2B410).
unicode_unihan_variant(0x8F19, kSemanticVariant, 0x8F12). %<kMatthews
unicode_unihan_variant(0x8F1B, kSimplifiedVariant, 0x8F86).
unicode_unihan_variant(0x8F1B, kZVariant, 0x8F0C).
unicode_unihan_variant(0x8F1C, kSimplifiedVariant, 0x8F8E).
unicode_unihan_variant(0x8F1D, kSemanticVariant, 0x7147). %<kFenn
unicode_unihan_variant(0x8F1D, kSimplifiedVariant, 0x8F89).
unicode_unihan_variant(0x8F1E, kSimplifiedVariant, 0x8F8B).
unicode_unihan_variant(0x8F1F, kSimplifiedVariant, 0x8F8D).
unicode_unihan_variant(0x8F25, kSimplifiedVariant, 0x8F8A).
unicode_unihan_variant(0x8F26, kSimplifiedVariant, 0x8F87).
unicode_unihan_variant(0x8F26, kSpecializedSemanticVariant, 0x895C). %<kMeyerWempe
unicode_unihan_variant(0x8F26, kZVariant, 0xF998).
unicode_unihan_variant(0x8F27, kZVariant, 0x8EFF).
unicode_unihan_variant(0x8F29, kSemanticVariant, 0x8EF0). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8F29, kSimplifiedVariant, 0x8F88).
unicode_unihan_variant(0x8F2A, kSimplifiedVariant, 0x8F6E).
unicode_unihan_variant(0x8F2A, kZVariant, 0xF9D7).
unicode_unihan_variant(0x8F2C, kSimplifiedVariant, 0x8F8C).
unicode_unihan_variant(0x8F2D, kSemanticVariant, 0x8EDF). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x8F2E, kSimplifiedVariant, 0x2B413).
unicode_unihan_variant(0x8F2F, kSimplifiedVariant, 0x8F91).
unicode_unihan_variant(0x8F33, kSimplifiedVariant, 0x8F8F).
unicode_unihan_variant(0x8F35, kSemanticVariant, 0x8F55). %<kMatthews
unicode_unihan_variant(0x8F38, kSimplifiedVariant, 0x8F93).
unicode_unihan_variant(0x8F3B, kSimplifiedVariant, 0x8F90).
unicode_unihan_variant(0x8F3B, kZVariant, 0xFA07).
unicode_unihan_variant(0x8F3C, kZVariant, 0x8F40).
unicode_unihan_variant(0x8F3E, kSimplifiedVariant, 0x8F97).
unicode_unihan_variant(0x8F3F, kSemanticVariant, 0x8F5D). %<kMatthews
unicode_unihan_variant(0x8F3F, kSimplifiedVariant, 0x8206).
unicode_unihan_variant(0x8F40, kSimplifiedVariant, 0x8F92).
unicode_unihan_variant(0x8F42, kSimplifiedVariant, 0x6BC2).
unicode_unihan_variant(0x8F42, kSpecializedSemanticVariant, 0x8EF2). %<kFenn
unicode_unihan_variant(0x8F44, kSemanticVariant, 0x938B). %<kMatthews
unicode_unihan_variant(0x8F44, kSimplifiedVariant, 0x8F96).
unicode_unihan_variant(0x8F45, kSimplifiedVariant, 0x8F95).
unicode_unihan_variant(0x8F46, kSemanticVariant, 0x6A1A). %<kMatthews
unicode_unihan_variant(0x8F46, kSimplifiedVariant, 0x8F98).
unicode_unihan_variant(0x8F49, kSimplifiedVariant, 0x8F6C).
unicode_unihan_variant(0x8F49, kZVariant, 0x8EE2).
unicode_unihan_variant(0x8F4D, kSimplifiedVariant, 0x8F99).
unicode_unihan_variant(0x8F4E, kSimplifiedVariant, 0x8F7F).
unicode_unihan_variant(0x8F54, kSimplifiedVariant, 0x8F9A).
unicode_unihan_variant(0x8F55, kSemanticVariant, 0x8F35). %<kMatthews
unicode_unihan_variant(0x8F57, kSemanticVariant, 0x3673). %<kMeyerWempe
unicode_unihan_variant(0x8F5D, kSemanticVariant, 0x8F3F). %<kMatthews
unicode_unihan_variant(0x8F5F, kSimplifiedVariant, 0x8F70).
unicode_unihan_variant(0x8F61, kSimplifiedVariant, 0x8F94).
unicode_unihan_variant(0x8F62, kSimplifiedVariant, 0x8F79).
unicode_unihan_variant(0x8F62, kZVariant, 0xF98D).
unicode_unihan_variant(0x8F63, kSimplifiedVariant, 0x2B406).
unicode_unihan_variant(0x8F64, kSimplifiedVariant, 0x8F73).
unicode_unihan_variant(0x8F66, kTraditionalVariant, 0x8ECA).
unicode_unihan_variant(0x8F67, kTraditionalVariant, 0x8ECB).
unicode_unihan_variant(0x8F68, kTraditionalVariant, 0x8ECC).
unicode_unihan_variant(0x8F69, kTraditionalVariant, 0x8ED2).
unicode_unihan_variant(0x8F6A, kTraditionalVariant, 0x8ED1).
unicode_unihan_variant(0x8F6B, kTraditionalVariant, 0x8ED4).
unicode_unihan_variant(0x8F6C, kTraditionalVariant, 0x8F49).
unicode_unihan_variant(0x8F6D, kTraditionalVariant, 0x8EDB).
unicode_unihan_variant(0x8F6E, kTraditionalVariant, 0x8F2A).
unicode_unihan_variant(0x8F6F, kTraditionalVariant, 0x8EDF).
unicode_unihan_variant(0x8F70, kTraditionalVariant, 0x8F5F).
unicode_unihan_variant(0x8F71, kTraditionalVariant, 0x8EF2).
unicode_unihan_variant(0x8F72, kTraditionalVariant, 0x8EFB).
unicode_unihan_variant(0x8F73, kTraditionalVariant, 0x8F64).
unicode_unihan_variant(0x8F74, kTraditionalVariant, 0x8EF8).
unicode_unihan_variant(0x8F75, kTraditionalVariant, 0x8EF9).
unicode_unihan_variant(0x8F76, kTraditionalVariant, 0x8EFC).
unicode_unihan_variant(0x8F77, kTraditionalVariant, 0x8EE4).
unicode_unihan_variant(0x8F78, kTraditionalVariant, 0x8EEB).
unicode_unihan_variant(0x8F79, kTraditionalVariant, 0x8F62).
unicode_unihan_variant(0x8F7A, kTraditionalVariant, 0x8EFA).
unicode_unihan_variant(0x8F7B, kTraditionalVariant, 0x8F15).
unicode_unihan_variant(0x8F7C, kTraditionalVariant, 0x8EFE).
unicode_unihan_variant(0x8F7D, kTraditionalVariant, 0x8F09).
unicode_unihan_variant(0x8F7E, kTraditionalVariant, 0x8F0A).
unicode_unihan_variant(0x8F7F, kTraditionalVariant, 0x8F4E).
unicode_unihan_variant(0x8F80, kTraditionalVariant, 0x8F08).
unicode_unihan_variant(0x8F81, kTraditionalVariant, 0x8F07).
unicode_unihan_variant(0x8F82, kTraditionalVariant, 0x8F05).
unicode_unihan_variant(0x8F83, kTraditionalVariant, 0x8F03).
unicode_unihan_variant(0x8F84, kTraditionalVariant, 0x8F12).
unicode_unihan_variant(0x8F85, kTraditionalVariant, 0x8F14).
unicode_unihan_variant(0x8F86, kTraditionalVariant, 0x8F1B).
unicode_unihan_variant(0x8F87, kTraditionalVariant, 0x8F26).
unicode_unihan_variant(0x8F88, kTraditionalVariant, 0x8F29).
unicode_unihan_variant(0x8F89, kTraditionalVariant, 0x8F1D).
unicode_unihan_variant(0x8F8A, kTraditionalVariant, 0x8F25).
unicode_unihan_variant(0x8F8B, kTraditionalVariant, 0x8F1E).
unicode_unihan_variant(0x8F8C, kTraditionalVariant, 0x8F2C).
unicode_unihan_variant(0x8F8D, kTraditionalVariant, 0x8F1F).
unicode_unihan_variant(0x8F8E, kTraditionalVariant, 0x8F1C).
unicode_unihan_variant(0x8F8F, kTraditionalVariant, 0x8F33).
unicode_unihan_variant(0x8F90, kTraditionalVariant, 0x8F3B).
unicode_unihan_variant(0x8F91, kTraditionalVariant, 0x8F2F).
unicode_unihan_variant(0x8F92, kTraditionalVariant, 0x8F40).
unicode_unihan_variant(0x8F93, kTraditionalVariant, 0x8F38).
unicode_unihan_variant(0x8F94, kTraditionalVariant, 0x8F61).
unicode_unihan_variant(0x8F95, kTraditionalVariant, 0x8F45).
unicode_unihan_variant(0x8F96, kTraditionalVariant, 0x8F44).
unicode_unihan_variant(0x8F97, kTraditionalVariant, 0x8F3E).
unicode_unihan_variant(0x8F98, kTraditionalVariant, 0x8F46).
unicode_unihan_variant(0x8F99, kTraditionalVariant, 0x8F4D).
unicode_unihan_variant(0x8F9A, kTraditionalVariant, 0x8F54).
unicode_unihan_variant(0x8F9E, kSemanticVariant, 0x8FA4). %<kMatthews 0x8FAD<kMatthews
unicode_unihan_variant(0x8F9E, kTraditionalVariant, 0x8FAD).
unicode_unihan_variant(0x8F9F, kZVariant, 0x907F).
unicode_unihan_variant(0x8FA0, kSemanticVariant, 0x7F6A). %<kLau,kMatthews
unicode_unihan_variant(0x8FA0, kZVariant, 0x7F6A).
unicode_unihan_variant(0x8FA2, kSemanticVariant, 0x8FA3). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8FA2, kZVariant, 0x8FA3).
unicode_unihan_variant(0x8FA3, kSemanticVariant, 0x8FA2). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8FA3, kZVariant, 0x8FA2).
unicode_unihan_variant(0x8FA4, kSemanticVariant, 0x8F9E). %<kMatthews 0x8FAD<kLau,kMatthews
unicode_unihan_variant(0x8FA6, kSemanticVariant, 0x529E). %<kFenn
unicode_unihan_variant(0x8FA6, kSimplifiedVariant, 0x529E).
unicode_unihan_variant(0x8FA7, kZVariant, 0x8FA8).
unicode_unihan_variant(0x8FA8, kSemanticVariant, 0x8FAF). %<kMeyerWempe
unicode_unihan_variant(0x8FA8, kZVariant, 0x5F01).
unicode_unihan_variant(0x8FA9, kTraditionalVariant, 0x8FAF).
unicode_unihan_variant(0x8FAB, kTraditionalVariant, 0x8FAE).
unicode_unihan_variant(0x8FAD, kSemanticVariant, 0x8F9E). %<kMatthews 0x8FA4<kLau,kMatthews
unicode_unihan_variant(0x8FAD, kSimplifiedVariant, 0x8F9E).
unicode_unihan_variant(0x8FAE, kSimplifiedVariant, 0x8FAB).
unicode_unihan_variant(0x8FAF, kSemanticVariant, 0x8FA8). %<kMeyerWempe
unicode_unihan_variant(0x8FAF, kSimplifiedVariant, 0x8FA9).
unicode_unihan_variant(0x8FB0, kZVariant, 0xF971).
unicode_unihan_variant(0x8FB2, kSemanticVariant, 0x8380). %<kMatthews
unicode_unihan_variant(0x8FB2, kSimplifiedVariant, 0x519C).
unicode_unihan_variant(0x8FB5, kSemanticVariant, 0x8FB6). %<kMatthews
unicode_unihan_variant(0x8FB6, kSemanticVariant, 0x8FB5). %<kMatthews
unicode_unihan_variant(0x8FB9, kSemanticVariant, 0x908A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8FB9, kTraditionalVariant, 0x908A).
unicode_unihan_variant(0x8FBA, kZVariant, 0x908A).
unicode_unihan_variant(0x8FBD, kTraditionalVariant, 0x907C).
unicode_unihan_variant(0x8FBE, kTraditionalVariant, 0x9054).
unicode_unihan_variant(0x8FC1, kSemanticVariant, 0x9077). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8FC1, kTraditionalVariant, 0x9077).
unicode_unihan_variant(0x8FC2, kZVariant, 0x9047).
unicode_unihan_variant(0x8FC6, kSpecializedSemanticVariant, 0x8FE4).
unicode_unihan_variant(0x8FC7, kSemanticVariant, 0x904E). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x8FC7, kTraditionalVariant, 0x904E).
unicode_unihan_variant(0x8FC8, kTraditionalVariant, 0x9081).
unicode_unihan_variant(0x8FCA, kSemanticVariant, 0x531D). %<kMatthews 0x5E00<kMatthews
unicode_unihan_variant(0x8FD0, kTraditionalVariant, 0x904B).
unicode_unihan_variant(0x8FD5, kSemanticVariant, 0x5FE4). %<kMeyerWempe
unicode_unihan_variant(0x8FD8, kSemanticVariant, 0x9084). %<kMatthews
unicode_unihan_variant(0x8FD8, kTraditionalVariant, 0x9084).
unicode_unihan_variant(0x8FD9, kSemanticVariant, 0x9019). %<kLau,kMatthews
unicode_unihan_variant(0x8FD9, kSpecializedSemanticVariant, 0x9019). %<kFenn
unicode_unihan_variant(0x8FD9, kTraditionalVariant, 0x9019).
unicode_unihan_variant(0x8FDB, kTraditionalVariant, 0x9032).
unicode_unihan_variant(0x8FDC, kSpecializedSemanticVariant, 0x9060). %<kFenn
unicode_unihan_variant(0x8FDC, kTraditionalVariant, 0x9060).
unicode_unihan_variant(0x8FDD, kTraditionalVariant, 0x9055).
unicode_unihan_variant(0x8FDE, kTraditionalVariant, 0x9023).
unicode_unihan_variant(0x8FDF, kTraditionalVariant, 0x9072).
unicode_unihan_variant(0x8FE4, kSpecializedSemanticVariant, 0x8FC6).
unicode_unihan_variant(0x8FE5, kZVariant, 0x9008).
unicode_unihan_variant(0x8FE8, kSemanticVariant, 0x902E). %<kLau,kMatthews
unicode_unihan_variant(0x8FE8, kSpecializedSemanticVariant, 0x902E). %<kFenn
unicode_unihan_variant(0x8FE8, kZVariant, 0x902E).
unicode_unihan_variant(0x8FE9, kSemanticVariant, 0x9087). %<kMatthews
unicode_unihan_variant(0x8FE9, kTraditionalVariant, 0x9087).
unicode_unihan_variant(0x8FEA, kSemanticVariant, 0x5EF8). %<kMatthews
unicode_unihan_variant(0x8FEB, kSemanticVariant, 0x5EF9). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x8FEC, kZVariant, 0x5F80).
unicode_unihan_variant(0x8FED, kZVariant, 0x758A).
unicode_unihan_variant(0x8FEF, kSemanticVariant, 0x9003). %<kMatthews
unicode_unihan_variant(0x8FEF, kZVariant, 0x9003).
unicode_unihan_variant(0x8FF3, kTraditionalVariant, 0x9015).
unicode_unihan_variant(0x8FF3, kZVariant, 0x5F91).
unicode_unihan_variant(0x8FF4, kSemanticVariant, 0x5EFB). %<kLau,kMatthews
unicode_unihan_variant(0x8FF4, kZVariant, 0x56DE).
unicode_unihan_variant(0x8FF9, kSemanticVariant, 0x8DE1). %<kHKGlyph,kLau,kMatthews,kMeyerWempe 0x8E5F<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x8FF9, kSpecializedSemanticVariant, 0x901F). %<kMeyerWempe
unicode_unihan_variant(0x8FF9, kTraditionalVariant, 0x8DE1).
unicode_unihan_variant(0x8FFA, kSemanticVariant, 0x4E43). %<kMatthews 0x5EFC<kMatthews
unicode_unihan_variant(0x8FFA, kZVariant, 0x4E43).
unicode_unihan_variant(0x8FFB, kSemanticVariant, 0x79FB). %<kMatthews
unicode_unihan_variant(0x9002, kTraditionalVariant, 0x9069).
unicode_unihan_variant(0x9003, kSemanticVariant, 0x8FEF). %<kMatthews
unicode_unihan_variant(0x9003, kZVariant, 0x8FEF).
unicode_unihan_variant(0x9009, kTraditionalVariant, 0x9078).
unicode_unihan_variant(0x900A, kTraditionalVariant, 0x905C).
unicode_unihan_variant(0x9012, kTraditionalVariant, 0x905E).
unicode_unihan_variant(0x9013, kSemanticVariant, 0x905E). %<kMatthews
unicode_unihan_variant(0x9013, kZVariant, 0x905E).
unicode_unihan_variant(0x9015, kSimplifiedVariant, 0x8FF3).
unicode_unihan_variant(0x9015, kZVariant, 0x5F91).
unicode_unihan_variant(0x9016, kSemanticVariant, 0x9037). %<kMatthews
unicode_unihan_variant(0x9019, kSemanticVariant, 0x8FD9). %<kLau,kMatthews
unicode_unihan_variant(0x9019, kSimplifiedVariant, 0x8FD9).
unicode_unihan_variant(0x9019, kSpecializedSemanticVariant, 0x8FD9). %<kFenn
unicode_unihan_variant(0x901E, kSpecializedSemanticVariant, 0x671F). %<kFenn
unicode_unihan_variant(0x901F, kSpecializedSemanticVariant, 0x8DE1). %<kMeyerWempe
unicode_unihan_variant(0x9023, kSimplifiedVariant, 0x8FDE).
unicode_unihan_variant(0x9023, kZVariant, 0xF99A).
unicode_unihan_variant(0x9025, kZVariant, 0x56DE).
unicode_unihan_variant(0x9026, kTraditionalVariant, 0x9090).
unicode_unihan_variant(0x9029, kZVariant, 0x5954).
unicode_unihan_variant(0x902E, kSemanticVariant, 0x8FE8). %<kLau,kMatthews
unicode_unihan_variant(0x902E, kSpecializedSemanticVariant, 0x8FE8). %<kFenn
unicode_unihan_variant(0x902E, kZVariant, 0x8FE8).
unicode_unihan_variant(0x9030, kZVariant, 0x6E38).
unicode_unihan_variant(0x9031, kSemanticVariant, 0x5468). %<kLau
unicode_unihan_variant(0x9032, kSimplifiedVariant, 0x8FDB).
unicode_unihan_variant(0x9037, kSemanticVariant, 0x9016). %<kMatthews
unicode_unihan_variant(0x9038, kZVariant, 0xFA25).
unicode_unihan_variant(0x9039, kZVariant, 0x9054).
unicode_unihan_variant(0x903A, kZVariant, 0x9060).
unicode_unihan_variant(0x903B, kTraditionalVariant, 0x908F).
unicode_unihan_variant(0x903C, kSemanticVariant, 0x506A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x903E, kSemanticVariant, 0x8E30). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x903F, kSpecializedSemanticVariant, 0x76EA). %<kFenn 0x28329<kFenn
unicode_unihan_variant(0x9045, kZVariant, 0x9072).
unicode_unihan_variant(0x904A, kZVariant, 0x6E38).
unicode_unihan_variant(0x904B, kSimplifiedVariant, 0x8FD0).
unicode_unihan_variant(0x904D, kSemanticVariant, 0x5FA7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x904D, kZVariant, 0x5FA7).
unicode_unihan_variant(0x904E, kSemanticVariant, 0x8FC7). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x904E, kSimplifiedVariant, 0x8FC7).
unicode_unihan_variant(0x9051, kZVariant, 0x5FA8).
unicode_unihan_variant(0x9054, kSimplifiedVariant, 0x8FBE).
unicode_unihan_variant(0x9055, kSimplifiedVariant, 0x8FDD).
unicode_unihan_variant(0x9057, kTraditionalVariant, 0x907A).
unicode_unihan_variant(0x9059, kSimplifiedVariant, 0x9065).
unicode_unihan_variant(0x905C, kSemanticVariant, 0x613B). %<kFenn
unicode_unihan_variant(0x905C, kSimplifiedVariant, 0x900A).
unicode_unihan_variant(0x905E, kSemanticVariant, 0x9013). %<kMatthews
unicode_unihan_variant(0x905E, kSimplifiedVariant, 0x9012).
unicode_unihan_variant(0x905E, kZVariant, 0x9013).
unicode_unihan_variant(0x905F, kZVariant, 0x9072).
unicode_unihan_variant(0x9060, kSimplifiedVariant, 0x8FDC).
unicode_unihan_variant(0x9060, kSpecializedSemanticVariant, 0x8FDC). %<kFenn
unicode_unihan_variant(0x9060, kZVariant, 0x903A).
unicode_unihan_variant(0x9061, kSemanticVariant, 0x6CDD). %<kMatthews 0x6EAF<kMeyerWempe
unicode_unihan_variant(0x9061, kZVariant, 0x6EAF).
unicode_unihan_variant(0x9065, kTraditionalVariant, 0x9059).
unicode_unihan_variant(0x9069, kSimplifiedVariant, 0x9002).
unicode_unihan_variant(0x906D, kSemanticVariant, 0x8E67). %<kLau,kMatthews
unicode_unihan_variant(0x9072, kSimplifiedVariant, 0x8FDF).
unicode_unihan_variant(0x9072, kZVariant, 0x9045).
unicode_unihan_variant(0x9076, kSemanticVariant, 0x7E5E). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9077, kSemanticVariant, 0x8FC1). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9077, kSimplifiedVariant, 0x8FC1).
unicode_unihan_variant(0x9078, kSimplifiedVariant, 0x9009).
unicode_unihan_variant(0x907A, kSimplifiedVariant, 0x9057).
unicode_unihan_variant(0x907C, kSimplifiedVariant, 0x8FBD).
unicode_unihan_variant(0x907C, kZVariant, 0xF9C3).
unicode_unihan_variant(0x907F, kZVariant, 0x8F9F).
unicode_unihan_variant(0x9081, kSimplifiedVariant, 0x8FC8).
unicode_unihan_variant(0x9084, kSemanticVariant, 0x8FD8). %<kMatthews
unicode_unihan_variant(0x9084, kSimplifiedVariant, 0x8FD8).
unicode_unihan_variant(0x9085, kSemanticVariant, 0x38F6). %<kMeyerWempe
unicode_unihan_variant(0x9087, kSemanticVariant, 0x8FE9). %<kMatthews
unicode_unihan_variant(0x9087, kSimplifiedVariant, 0x8FE9).
unicode_unihan_variant(0x9089, kZVariant, 0x908A).
unicode_unihan_variant(0x908A, kSemanticVariant, 0x8FB9). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x908A, kSimplifiedVariant, 0x8FB9).
unicode_unihan_variant(0x908F, kSimplifiedVariant, 0x903B).
unicode_unihan_variant(0x908F, kZVariant, 0xF913).
unicode_unihan_variant(0x9090, kSimplifiedVariant, 0x9026).
unicode_unihan_variant(0x9091, kZVariant, 0x961D).
unicode_unihan_variant(0x9093, kTraditionalVariant, 0x9127).
unicode_unihan_variant(0x9095, kSemanticVariant, 0x96CD). %<kMatthews,kMeyerWempe 0x2AA9D<kMatthews
unicode_unihan_variant(0x909D, kTraditionalVariant, 0x913A).
unicode_unihan_variant(0x90A0, kSemanticVariant, 0x37D7). %<kMatthews 0x8C73<kMatthews
unicode_unihan_variant(0x90A2, kZVariant, 0x90C9).
unicode_unihan_variant(0x90A8, kSemanticVariant, 0x6751). %<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x90AC, kTraditionalVariant, 0x9114).
unicode_unihan_variant(0x90AE, kTraditionalVariant, 0x90F5).
unicode_unihan_variant(0x90B9, kTraditionalVariant, 0x9112).
unicode_unihan_variant(0x90BA, kTraditionalVariant, 0x9134).
unicode_unihan_variant(0x90BB, kTraditionalVariant, 0x9130).
unicode_unihan_variant(0x90C1, kSemanticVariant, 0x5590). %<kMeyerWempe
unicode_unihan_variant(0x90C1, kTraditionalVariant, 0x9B31).
unicode_unihan_variant(0x90C4, kSemanticVariant, 0x90E4). %<kMatthews
unicode_unihan_variant(0x90C4, kZVariant, 0x9699).
unicode_unihan_variant(0x90C5, kZVariant, 0x80DD).
unicode_unihan_variant(0x90C9, kZVariant, 0x90A2).
unicode_unihan_variant(0x90CE, kZVariant, 0x90DE).
unicode_unihan_variant(0x90CF, kTraditionalVariant, 0x90DF).
unicode_unihan_variant(0x90D0, kTraditionalVariant, 0x9136).
unicode_unihan_variant(0x90D1, kTraditionalVariant, 0x912D).
unicode_unihan_variant(0x90D3, kTraditionalVariant, 0x9106).
unicode_unihan_variant(0x90DE, kZVariant, 0x90CE).
unicode_unihan_variant(0x90DF, kSimplifiedVariant, 0x90CF).
unicode_unihan_variant(0x90E4, kSemanticVariant, 0x90C4). %<kMatthews
unicode_unihan_variant(0x90E6, kTraditionalVariant, 0x9148).
unicode_unihan_variant(0x90E7, kTraditionalVariant, 0x9116).
unicode_unihan_variant(0x90E8, kZVariant, 0x5369).
unicode_unihan_variant(0x90F0, kSemanticVariant, 0x9139). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x90F5, kSimplifiedVariant, 0x90AE).
unicode_unihan_variant(0x90F7, kSemanticVariant, 0x9109). %<kMatthews
unicode_unihan_variant(0x90F7, kZVariant, 0x9109).
unicode_unihan_variant(0x90F8, kTraditionalVariant, 0x9132).
unicode_unihan_variant(0x90FD, kZVariant, 0xFA26).
unicode_unihan_variant(0x9106, kSimplifiedVariant, 0x90D3).
unicode_unihan_variant(0x9109, kSemanticVariant, 0x90F7). %<kMatthews 0x9115<kMatthews
unicode_unihan_variant(0x9109, kSimplifiedVariant, 0x4E61).
unicode_unihan_variant(0x9112, kSimplifiedVariant, 0x90B9).
unicode_unihan_variant(0x9114, kSemanticVariant, 0x5862). %<kMeyerWempe
unicode_unihan_variant(0x9114, kSimplifiedVariant, 0x90AC).
unicode_unihan_variant(0x9115, kSemanticVariant, 0x9109). %<kMatthews
unicode_unihan_variant(0x9115, kZVariant, 0x9109).
unicode_unihan_variant(0x9116, kSimplifiedVariant, 0x90E7).
unicode_unihan_variant(0x9127, kSimplifiedVariant, 0x9093).
unicode_unihan_variant(0x912D, kSimplifiedVariant, 0x90D1).
unicode_unihan_variant(0x9130, kSemanticVariant, 0x96A3). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9130, kSimplifiedVariant, 0x90BB).
unicode_unihan_variant(0x9130, kZVariant, 0x96A3).
unicode_unihan_variant(0x9132, kSimplifiedVariant, 0x90F8).
unicode_unihan_variant(0x9134, kSimplifiedVariant, 0x90BA).
unicode_unihan_variant(0x9136, kSimplifiedVariant, 0x90D0).
unicode_unihan_variant(0x9137, kZVariant, 0x9146).
unicode_unihan_variant(0x9139, kSemanticVariant, 0x90F0). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x913A, kSimplifiedVariant, 0x909D).
unicode_unihan_variant(0x9142, kTraditionalVariant, 0x9147).
unicode_unihan_variant(0x9146, kZVariant, 0x9137).
unicode_unihan_variant(0x9147, kSimplifiedVariant, 0x9142).
unicode_unihan_variant(0x9148, kSimplifiedVariant, 0x90E6).
unicode_unihan_variant(0x9149, kSpecializedSemanticVariant, 0x914B). %<kFenn
unicode_unihan_variant(0x9149, kZVariant, 0x4E23).
unicode_unihan_variant(0x914B, kSpecializedSemanticVariant, 0x9149). %<kFenn
unicode_unihan_variant(0x9154, kZVariant, 0x9189).
unicode_unihan_variant(0x9156, kSemanticVariant, 0x9D06). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x915D, kTraditionalVariant, 0x919E).
unicode_unihan_variant(0x915D, kZVariant, 0x919E).
unicode_unihan_variant(0x9162, kSpecializedSemanticVariant, 0x69A8). %<kMeyerWempe
unicode_unihan_variant(0x9162, kZVariant, 0x918B).
unicode_unihan_variant(0x9166, kTraditionalVariant, 0x91B1).
unicode_unihan_variant(0x9167, kSemanticVariant, 0x916C). %<kMatthews 0x91BB<kFenn
unicode_unihan_variant(0x9167, kZVariant, 0x916C).
unicode_unihan_variant(0x916A, kZVariant, 0xF919).
unicode_unihan_variant(0x916C, kSemanticVariant, 0x9167). %<kMatthews 0x91BB<kFenn
unicode_unihan_variant(0x916C, kZVariant, 0x9167).
unicode_unihan_variant(0x9171, kTraditionalVariant, 0x91AC).
unicode_unihan_variant(0x9179, kSpecializedSemanticVariant, 0x5121). %<kMeyerWempe
unicode_unihan_variant(0x917D, kTraditionalVariant, 0x91C5).
unicode_unihan_variant(0x917E, kTraditionalVariant, 0x91C3).
unicode_unihan_variant(0x917F, kTraditionalVariant, 0x91C0).
unicode_unihan_variant(0x9183, kSpecializedSemanticVariant, 0x814C).
unicode_unihan_variant(0x9187, kSemanticVariant, 0x48E9). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9189, kZVariant, 0x9154).
unicode_unihan_variant(0x918A, kSemanticVariant, 0x991F). %<kMatthews
unicode_unihan_variant(0x918B, kZVariant, 0x9162).
unicode_unihan_variant(0x9191, kSpecializedSemanticVariant, 0x6E51). %<kMeyerWempe
unicode_unihan_variant(0x9192, kSpecializedSemanticVariant, 0x60FA). %<kMeyerWempe
unicode_unihan_variant(0x9196, kZVariant, 0x919E).
unicode_unihan_variant(0x9197, kZVariant, 0x91B1).
unicode_unihan_variant(0x919C, kSimplifiedVariant, 0x4E11).
unicode_unihan_variant(0x919E, kSimplifiedVariant, 0x915D).
unicode_unihan_variant(0x91A1, kSemanticVariant, 0x69A8). %<kMatthews
unicode_unihan_variant(0x91A1, kSpecializedSemanticVariant, 0x8A50). %<kFenn
unicode_unihan_variant(0x91A4, kZVariant, 0x91AC).
unicode_unihan_variant(0x91A9, kSemanticVariant, 0x7CDF). %<kMatthews
unicode_unihan_variant(0x91AB, kSemanticVariant, 0x533B). %<kMatthews
unicode_unihan_variant(0x91AB, kSimplifiedVariant, 0x533B).
unicode_unihan_variant(0x91AC, kSimplifiedVariant, 0x9171).
unicode_unihan_variant(0x91B1, kSimplifiedVariant, 0x9166).
unicode_unihan_variant(0x91B4, kZVariant, 0xF9B7).
unicode_unihan_variant(0x91BB, kSemanticVariant, 0x9167). %<kFenn 0x916C<kFenn
unicode_unihan_variant(0x91BB, kZVariant, 0x916C).
unicode_unihan_variant(0x91BC, kZVariant, 0x71D5).
unicode_unihan_variant(0x91C0, kSimplifiedVariant, 0x917F).
unicode_unihan_variant(0x91C0, kZVariant, 0x91B8).
unicode_unihan_variant(0x91C1, kSemanticVariant, 0x8845). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x91C1, kSimplifiedVariant, 0x8845).
unicode_unihan_variant(0x91C3, kSimplifiedVariant, 0x917E).
unicode_unihan_variant(0x91C5, kSimplifiedVariant, 0x917D).
unicode_unihan_variant(0x91C6, kZVariant, 0x91C7).
unicode_unihan_variant(0x91C7, kTraditionalVariant, 0x57F0). %0x63A1
unicode_unihan_variant(0x91C7, kZVariant, 0x91C6).
unicode_unihan_variant(0x91C8, kZVariant, 0x91CB).
unicode_unihan_variant(0x91CA, kTraditionalVariant, 0x91CB).
unicode_unihan_variant(0x91CB, kSimplifiedVariant, 0x91CA).
unicode_unihan_variant(0x91CC, kTraditionalVariant, 0x88E1).
unicode_unihan_variant(0x91CC, kZVariant, 0x88CF).
unicode_unihan_variant(0x91CE, kSemanticVariant, 0x57DC). %<kLau,kMatthews
unicode_unihan_variant(0x91CE, kZVariant, 0x57DC).
unicode_unihan_variant(0x91CF, kZVariant, 0xF97E).
unicode_unihan_variant(0x91D0, kSemanticVariant, 0x5398). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x91D0, kSimplifiedVariant, 0x5398).
unicode_unihan_variant(0x91D1, kSemanticVariant, 0x91D2). %<kMatthews
unicode_unihan_variant(0x91D1, kZVariant, 0x9485).
unicode_unihan_variant(0x91D2, kSemanticVariant, 0x91D1). %<kMatthews
unicode_unihan_variant(0x91D2, kSimplifiedVariant, 0x9485).
unicode_unihan_variant(0x91D3, kSimplifiedVariant, 0x9486).
unicode_unihan_variant(0x91D4, kSimplifiedVariant, 0x9487).
unicode_unihan_variant(0x91D5, kSimplifiedVariant, 0x948C).
unicode_unihan_variant(0x91D6, kZVariant, 0x528D).
unicode_unihan_variant(0x91D7, kSimplifiedVariant, 0x948A).
unicode_unihan_variant(0x91D8, kSimplifiedVariant, 0x9489).
unicode_unihan_variant(0x91D9, kSimplifiedVariant, 0x948B).
unicode_unihan_variant(0x91DC, kZVariant, 0x91E1).
unicode_unihan_variant(0x91DD, kSemanticVariant, 0x7BB4). %<kMeyerWempe 0x937C<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x91DD, kSimplifiedVariant, 0x9488).
unicode_unihan_variant(0x91E1, kZVariant, 0x91DC).
unicode_unihan_variant(0x91E3, kSimplifiedVariant, 0x9493).
unicode_unihan_variant(0x91E4, kSimplifiedVariant, 0x9490).
unicode_unihan_variant(0x91E7, kSimplifiedVariant, 0x948F).
unicode_unihan_variant(0x91E9, kSimplifiedVariant, 0x9492).
unicode_unihan_variant(0x91EC, kSemanticVariant, 0x92B2). %<kMatthews
unicode_unihan_variant(0x91F3, kSimplifiedVariant, 0x28C3F).
unicode_unihan_variant(0x91F5, kSimplifiedVariant, 0x9497).
unicode_unihan_variant(0x91F5, kSpecializedSemanticVariant, 0x53C9). %<kMeyerWempe
unicode_unihan_variant(0x91F7, kSimplifiedVariant, 0x948D).
unicode_unihan_variant(0x91F9, kSimplifiedVariant, 0x9495).
unicode_unihan_variant(0x91FA, kSimplifiedVariant, 0x948E).
unicode_unihan_variant(0x91FC, kZVariant, 0x528D).
unicode_unihan_variant(0x91FE, kSimplifiedVariant, 0x497A).
unicode_unihan_variant(0x9200, kSemanticVariant, 0x8019). %<kMatthews
unicode_unihan_variant(0x9200, kSimplifiedVariant, 0x94AF).
unicode_unihan_variant(0x9201, kSimplifiedVariant, 0x94AB).
unicode_unihan_variant(0x9203, kSimplifiedVariant, 0x9498).
unicode_unihan_variant(0x9204, kSimplifiedVariant, 0x94AD).
unicode_unihan_variant(0x9206, kSemanticVariant, 0x925B). %<kMatthews
unicode_unihan_variant(0x9207, kSimplifiedVariant, 0x2B4E7).
unicode_unihan_variant(0x9208, kSimplifiedVariant, 0x949A).
unicode_unihan_variant(0x9209, kSimplifiedVariant, 0x94A0).
unicode_unihan_variant(0x920B, kSimplifiedVariant, 0x28C42).
unicode_unihan_variant(0x920D, kSimplifiedVariant, 0x949D).
unicode_unihan_variant(0x920E, kSemanticVariant, 0x9264). %<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x920E, kZVariant, 0x9264).
unicode_unihan_variant(0x9210, kSimplifiedVariant, 0x94A4).
unicode_unihan_variant(0x9211, kSimplifiedVariant, 0x94A3).
unicode_unihan_variant(0x9212, kSimplifiedVariant, 0x9491).
unicode_unihan_variant(0x9214, kSimplifiedVariant, 0x949E).
unicode_unihan_variant(0x9215, kSimplifiedVariant, 0x94AE).
unicode_unihan_variant(0x921E, kSemanticVariant, 0x9281). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x921E, kSimplifiedVariant, 0x94A7).
unicode_unihan_variant(0x9220, kSimplifiedVariant, 0x28C41).
unicode_unihan_variant(0x9221, kZVariant, 0x937E).
unicode_unihan_variant(0x9223, kSimplifiedVariant, 0x9499).
unicode_unihan_variant(0x9225, kSimplifiedVariant, 0x94AC).
unicode_unihan_variant(0x9226, kSimplifiedVariant, 0x949B).
unicode_unihan_variant(0x9227, kSimplifiedVariant, 0x94AA).
unicode_unihan_variant(0x922C, kZVariant, 0x9438).
unicode_unihan_variant(0x922E, kSimplifiedVariant, 0x94CC).
unicode_unihan_variant(0x922F, kSimplifiedVariant, 0x28C44).
unicode_unihan_variant(0x9230, kSimplifiedVariant, 0x94C8).
unicode_unihan_variant(0x9232, kSimplifiedVariant, 0x28C43).
unicode_unihan_variant(0x9233, kSimplifiedVariant, 0x94B6).
unicode_unihan_variant(0x9234, kSimplifiedVariant, 0x94C3).
unicode_unihan_variant(0x9234, kZVariant, 0xF9B1).
unicode_unihan_variant(0x9237, kSimplifiedVariant, 0x94B4).
unicode_unihan_variant(0x9238, kSimplifiedVariant, 0x94B9).
unicode_unihan_variant(0x9239, kSimplifiedVariant, 0x94CD).
unicode_unihan_variant(0x923A, kSimplifiedVariant, 0x94B0).
unicode_unihan_variant(0x923D, kSimplifiedVariant, 0x94B8).
unicode_unihan_variant(0x923E, kSimplifiedVariant, 0x94C0).
unicode_unihan_variant(0x923F, kSimplifiedVariant, 0x94BF).
unicode_unihan_variant(0x9240, kSimplifiedVariant, 0x94BE).
unicode_unihan_variant(0x9241, kSimplifiedVariant, 0x28C45).
unicode_unihan_variant(0x9244, kSemanticVariant, 0x9435). %<kLau,kMatthews,kMeyerWempe 0x9295<kMatthews
unicode_unihan_variant(0x9245, kSimplifiedVariant, 0x949C).
unicode_unihan_variant(0x9245, kZVariant, 0x5DE8).
unicode_unihan_variant(0x9246, kSemanticVariant, 0x9257). %<kPhonetic 0x947D<kPhonetic
unicode_unihan_variant(0x9248, kSemanticVariant, 0x7823). %<kMatthews 0x254FF<kFenn
unicode_unihan_variant(0x9248, kSimplifiedVariant, 0x94CA).
unicode_unihan_variant(0x9249, kSimplifiedVariant, 0x94C9).
unicode_unihan_variant(0x924B, kSemanticVariant, 0x5228). %<kMatthews
unicode_unihan_variant(0x924B, kSimplifiedVariant, 0x94C7).
unicode_unihan_variant(0x924D, kSimplifiedVariant, 0x94CB).
unicode_unihan_variant(0x924F, kSemanticVariant, 0x92E4). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9251, kSimplifiedVariant, 0x94C2).
unicode_unihan_variant(0x9255, kSimplifiedVariant, 0x94B7).
unicode_unihan_variant(0x9257, kSemanticVariant, 0x9246). %<kPhonetic
unicode_unihan_variant(0x9257, kSimplifiedVariant, 0x94B3).
unicode_unihan_variant(0x9257, kZVariant, 0x7B9D).
unicode_unihan_variant(0x925A, kSimplifiedVariant, 0x94C6).
unicode_unihan_variant(0x925B, kSemanticVariant, 0x9206). %<kMatthews
unicode_unihan_variant(0x925B, kSimplifiedVariant, 0x94C5).
unicode_unihan_variant(0x925E, kSemanticVariant, 0x6209). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x925E, kSimplifiedVariant, 0x94BA).
unicode_unihan_variant(0x9262, kSemanticVariant, 0x7F3D). %<kHKGlyph
unicode_unihan_variant(0x9264, kSemanticVariant, 0x920E). %<kHKGlyph,kLau,kMatthews
unicode_unihan_variant(0x9264, kSimplifiedVariant, 0x94A9).
unicode_unihan_variant(0x9264, kZVariant, 0x920E).
unicode_unihan_variant(0x9266, kSimplifiedVariant, 0x94B2).
unicode_unihan_variant(0x926C, kSimplifiedVariant, 0x94BC).
unicode_unihan_variant(0x926D, kSimplifiedVariant, 0x94BD).
unicode_unihan_variant(0x9271, kZVariant, 0x7926).
unicode_unihan_variant(0x9274, kTraditionalVariant, 0x9452).
unicode_unihan_variant(0x9276, kSimplifiedVariant, 0x94CF).
unicode_unihan_variant(0x9278, kSimplifiedVariant, 0x94F0).
unicode_unihan_variant(0x927A, kSimplifiedVariant, 0x94D2).
unicode_unihan_variant(0x927B, kSimplifiedVariant, 0x94EC).
unicode_unihan_variant(0x927F, kSimplifiedVariant, 0x94EA).
unicode_unihan_variant(0x9280, kSimplifiedVariant, 0x94F6).
unicode_unihan_variant(0x9281, kSemanticVariant, 0x921E). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9283, kSimplifiedVariant, 0x94F3).
unicode_unihan_variant(0x9285, kSimplifiedVariant, 0x94DC).
unicode_unihan_variant(0x928D, kSimplifiedVariant, 0x94DA).
unicode_unihan_variant(0x9291, kSimplifiedVariant, 0x94E3).
unicode_unihan_variant(0x9293, kSimplifiedVariant, 0x94E8).
unicode_unihan_variant(0x9295, kSemanticVariant, 0x9244). %<kMatthews 0x9435<kMatthews
unicode_unihan_variant(0x9295, kZVariant, 0x9435).
unicode_unihan_variant(0x9296, kSimplifiedVariant, 0x94E2).
unicode_unihan_variant(0x9298, kSimplifiedVariant, 0x94ED).
unicode_unihan_variant(0x929A, kSimplifiedVariant, 0x94EB).
unicode_unihan_variant(0x929B, kSimplifiedVariant, 0x94E6).
unicode_unihan_variant(0x929C, kSemanticVariant, 0x3605). %<kMeyerWempe
unicode_unihan_variant(0x929C, kSimplifiedVariant, 0x8854).
unicode_unihan_variant(0x92A0, kSimplifiedVariant, 0x94D1).
unicode_unihan_variant(0x92A3, kSimplifiedVariant, 0x94F7).
unicode_unihan_variant(0x92A5, kSimplifiedVariant, 0x94F1).
unicode_unihan_variant(0x92A6, kSimplifiedVariant, 0x94DF).
unicode_unihan_variant(0x92A8, kSimplifiedVariant, 0x94F5).
unicode_unihan_variant(0x92A9, kSimplifiedVariant, 0x94E5).
unicode_unihan_variant(0x92AA, kSimplifiedVariant, 0x94D5).
unicode_unihan_variant(0x92AB, kSimplifiedVariant, 0x94EF).
unicode_unihan_variant(0x92AC, kSimplifiedVariant, 0x94D0).
unicode_unihan_variant(0x92AD, kZVariant, 0x9322).
unicode_unihan_variant(0x92AE, kTraditionalVariant, 0x947E).
unicode_unihan_variant(0x92B1, kSimplifiedVariant, 0x94DE).
unicode_unihan_variant(0x92B2, kSemanticVariant, 0x91EC). %<kMatthews
unicode_unihan_variant(0x92B3, kSimplifiedVariant, 0x9510).
unicode_unihan_variant(0x92B3, kZVariant, 0x92ED).
unicode_unihan_variant(0x92B6, kSimplifiedVariant, 0x28C47).
unicode_unihan_variant(0x92B7, kSimplifiedVariant, 0x9500).
unicode_unihan_variant(0x92B9, kSemanticVariant, 0x93FD). %<kFenn
unicode_unihan_variant(0x92B9, kZVariant, 0x93FD).
unicode_unihan_variant(0x92BB, kSimplifiedVariant, 0x9511).
unicode_unihan_variant(0x92BC, kSimplifiedVariant, 0x9509).
unicode_unihan_variant(0x92BC, kSpecializedSemanticVariant, 0x5249).
unicode_unihan_variant(0x92C1, kSimplifiedVariant, 0x94DD).
unicode_unihan_variant(0x92C3, kSimplifiedVariant, 0x9512).
unicode_unihan_variant(0x92C4, kZVariant, 0x933D).
unicode_unihan_variant(0x92C5, kSimplifiedVariant, 0x950C).
unicode_unihan_variant(0x92C7, kSimplifiedVariant, 0x94A1).
unicode_unihan_variant(0x92C9, kSimplifiedVariant, 0x28C48).
unicode_unihan_variant(0x92CC, kSimplifiedVariant, 0x94E4).
unicode_unihan_variant(0x92CF, kSimplifiedVariant, 0x94D7).
unicode_unihan_variant(0x92D1, kSemanticVariant, 0x942B). %<kMatthews 0x9474<kMatthews
unicode_unihan_variant(0x92D2, kSimplifiedVariant, 0x950B).
unicode_unihan_variant(0x92D9, kSimplifiedVariant, 0x94FB).
unicode_unihan_variant(0x92DD, kSimplifiedVariant, 0x950A).
unicode_unihan_variant(0x92DF, kSimplifiedVariant, 0x9513).
unicode_unihan_variant(0x92E3, kSimplifiedVariant, 0x94D8).
unicode_unihan_variant(0x92E4, kSemanticVariant, 0x924F). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x92E4, kSimplifiedVariant, 0x9504).
unicode_unihan_variant(0x92E5, kSimplifiedVariant, 0x9503).
unicode_unihan_variant(0x92E6, kSimplifiedVariant, 0x9514).
unicode_unihan_variant(0x92E8, kSimplifiedVariant, 0x9507).
unicode_unihan_variant(0x92E9, kSimplifiedVariant, 0x94D3).
unicode_unihan_variant(0x92EA, kSemanticVariant, 0x8216). %<kLau
unicode_unihan_variant(0x92EA, kSimplifiedVariant, 0x94FA).
unicode_unihan_variant(0x92EA, kSpecializedSemanticVariant, 0x8216). %<kMeyerWempe
unicode_unihan_variant(0x92ED, kZVariant, 0x92B3).
unicode_unihan_variant(0x92EE, kSimplifiedVariant, 0x94D6).
unicode_unihan_variant(0x92EF, kSimplifiedVariant, 0x9506).
unicode_unihan_variant(0x92F0, kSimplifiedVariant, 0x9502).
unicode_unihan_variant(0x92F1, kSimplifiedVariant, 0x94FD).
unicode_unihan_variant(0x92F3, kZVariant, 0x9444).
unicode_unihan_variant(0x92F6, kSimplifiedVariant, 0x950D).
unicode_unihan_variant(0x92F8, kSimplifiedVariant, 0x952F).
unicode_unihan_variant(0x92FC, kSimplifiedVariant, 0x94A2).
unicode_unihan_variant(0x9301, kSimplifiedVariant, 0x951E).
unicode_unihan_variant(0x9302, kSimplifiedVariant, 0x28C4B).
unicode_unihan_variant(0x9304, kSimplifiedVariant, 0x5F55).
unicode_unihan_variant(0x9304, kZVariant, 0x9332).
unicode_unihan_variant(0x9306, kSimplifiedVariant, 0x9516).
unicode_unihan_variant(0x9307, kSimplifiedVariant, 0x952B).
unicode_unihan_variant(0x9308, kSimplifiedVariant, 0x9529).
unicode_unihan_variant(0x930F, kSimplifiedVariant, 0x94D4).
unicode_unihan_variant(0x9310, kSimplifiedVariant, 0x9525).
unicode_unihan_variant(0x9312, kSimplifiedVariant, 0x9515).
unicode_unihan_variant(0x9315, kSimplifiedVariant, 0x951F).
unicode_unihan_variant(0x9318, kSemanticVariant, 0x939A). %<kLau,kMeyerWempe
unicode_unihan_variant(0x9318, kSimplifiedVariant, 0x9524).
unicode_unihan_variant(0x9319, kSimplifiedVariant, 0x9531).
unicode_unihan_variant(0x931A, kSimplifiedVariant, 0x94EE).
unicode_unihan_variant(0x931B, kSimplifiedVariant, 0x951B).
unicode_unihan_variant(0x931F, kSimplifiedVariant, 0x952C).
unicode_unihan_variant(0x9320, kSimplifiedVariant, 0x952D).
unicode_unihan_variant(0x9321, kSimplifiedVariant, 0x951C).
unicode_unihan_variant(0x9322, kSimplifiedVariant, 0x94B1).
unicode_unihan_variant(0x9326, kSimplifiedVariant, 0x9526).
unicode_unihan_variant(0x9328, kSimplifiedVariant, 0x951A).
unicode_unihan_variant(0x9329, kSimplifiedVariant, 0x9520).
unicode_unihan_variant(0x932B, kSimplifiedVariant, 0x9521).
unicode_unihan_variant(0x932C, kZVariant, 0x7149).
unicode_unihan_variant(0x932E, kSimplifiedVariant, 0x9522).
unicode_unihan_variant(0x932F, kSimplifiedVariant, 0x9519).
unicode_unihan_variant(0x9332, kZVariant, 0x9304).
unicode_unihan_variant(0x9333, kSimplifiedVariant, 0x9530).
unicode_unihan_variant(0x9336, kSimplifiedVariant, 0x8868).
unicode_unihan_variant(0x9338, kSimplifiedVariant, 0x94FC).
unicode_unihan_variant(0x933D, kZVariant, 0x92C4).
unicode_unihan_variant(0x933E, kTraditionalVariant, 0x93E8).
unicode_unihan_variant(0x9340, kSimplifiedVariant, 0x951D).
unicode_unihan_variant(0x9343, kSimplifiedVariant, 0x952A).
unicode_unihan_variant(0x9344, kSimplifiedVariant, 0x28C49).
unicode_unihan_variant(0x9346, kSimplifiedVariant, 0x9494).
unicode_unihan_variant(0x9347, kSimplifiedVariant, 0x9534).
unicode_unihan_variant(0x9348, kSimplifiedVariant, 0x9533).
unicode_unihan_variant(0x9349, kSemanticVariant, 0x5319). %<kMatthews
unicode_unihan_variant(0x934A, kZVariant, 0xF99B).
unicode_unihan_variant(0x934B, kSimplifiedVariant, 0x9505).
unicode_unihan_variant(0x934D, kSimplifiedVariant, 0x9540).
unicode_unihan_variant(0x9354, kSimplifiedVariant, 0x9537).
unicode_unihan_variant(0x9358, kSimplifiedVariant, 0x94E1).
unicode_unihan_variant(0x935A, kSimplifiedVariant, 0x9496).
unicode_unihan_variant(0x935B, kSemanticVariant, 0x7145). %<kLau,kMatthews
unicode_unihan_variant(0x935B, kSimplifiedVariant, 0x953B).
unicode_unihan_variant(0x935B, kZVariant, 0x7145).
unicode_unihan_variant(0x935C, kSemanticVariant, 0x7146). %<kMeyerWempe
unicode_unihan_variant(0x9360, kSimplifiedVariant, 0x953D).
unicode_unihan_variant(0x9364, kSimplifiedVariant, 0x9538).
unicode_unihan_variant(0x9365, kSimplifiedVariant, 0x9532).
unicode_unihan_variant(0x9369, kSimplifiedVariant, 0x9518).
unicode_unihan_variant(0x936B, kSemanticVariant, 0x936C). %<kMeyerWempe
unicode_unihan_variant(0x936C, kSemanticVariant, 0x936B). %<kMeyerWempe
unicode_unihan_variant(0x936C, kSimplifiedVariant, 0x9539).
unicode_unihan_variant(0x936E, kSimplifiedVariant, 0x28C4E).
unicode_unihan_variant(0x9370, kSemanticVariant, 0x9436). %<kMatthews
unicode_unihan_variant(0x9370, kSimplifiedVariant, 0x953E).
unicode_unihan_variant(0x9375, kSimplifiedVariant, 0x952E).
unicode_unihan_variant(0x9376, kSimplifiedVariant, 0x9536).
unicode_unihan_variant(0x937A, kSimplifiedVariant, 0x9517).
unicode_unihan_variant(0x937C, kSemanticVariant, 0x7BB4). %<kMeyerWempe 0x91DD<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x937E, kSimplifiedVariant, 0x949F). %0x953A
unicode_unihan_variant(0x937E, kZVariant, 0x949F).
unicode_unihan_variant(0x9382, kSimplifiedVariant, 0x9541).
unicode_unihan_variant(0x9384, kSimplifiedVariant, 0x953F).
unicode_unihan_variant(0x9387, kSimplifiedVariant, 0x9545).
unicode_unihan_variant(0x938A, kSimplifiedVariant, 0x9551).
unicode_unihan_variant(0x938B, kSemanticVariant, 0x8F44). %<kMatthews
unicode_unihan_variant(0x938C, kSemanticVariant, 0x942E). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x938F, kSemanticVariant, 0x93D0). %<kMeyerWempe
unicode_unihan_variant(0x9392, kSemanticVariant, 0x8028). %<kMatthews
unicode_unihan_variant(0x9394, kSemanticVariant, 0x7194). %<kLau,kMatthews
unicode_unihan_variant(0x9394, kSimplifiedVariant, 0x9555).
unicode_unihan_variant(0x9394, kZVariant, 0x7194).
unicode_unihan_variant(0x9396, kSemanticVariant, 0x93C1). %<kMeyerWempe 0x93BB<kMatthews
unicode_unihan_variant(0x9396, kSimplifiedVariant, 0x9501).
unicode_unihan_variant(0x9398, kSimplifiedVariant, 0x9549).
unicode_unihan_variant(0x9399, kSemanticVariant, 0x69CA). %<kMeyerWempe
unicode_unihan_variant(0x939A, kSemanticVariant, 0x9318). %<kLau,kMeyerWempe
unicode_unihan_variant(0x939B, kSimplifiedVariant, 0x9548).
unicode_unihan_variant(0x939D, kSimplifiedVariant, 0x28C4F).
unicode_unihan_variant(0x93A1, kSimplifiedVariant, 0x9543).
unicode_unihan_variant(0x93A2, kSimplifiedVariant, 0x94A8).
unicode_unihan_variant(0x93A3, kSimplifiedVariant, 0x84E5).
unicode_unihan_variant(0x93A6, kSimplifiedVariant, 0x954F).
unicode_unihan_variant(0x93A7, kSimplifiedVariant, 0x94E0).
unicode_unihan_variant(0x93A9, kSimplifiedVariant, 0x94E9).
unicode_unihan_variant(0x93AA, kSimplifiedVariant, 0x953C).
unicode_unihan_variant(0x93AC, kSimplifiedVariant, 0x9550).
unicode_unihan_variant(0x93AD, kZVariant, 0x93AE).
unicode_unihan_variant(0x93AE, kSimplifiedVariant, 0x9547).
unicode_unihan_variant(0x93AF, kSimplifiedVariant, 0x28C4D).
unicode_unihan_variant(0x93B0, kSimplifiedVariant, 0x9552).
unicode_unihan_variant(0x93B2, kSimplifiedVariant, 0x954B).
unicode_unihan_variant(0x93B3, kSimplifiedVariant, 0x954D).
unicode_unihan_variant(0x93B5, kSimplifiedVariant, 0x9553).
unicode_unihan_variant(0x93B7, kSimplifiedVariant, 0x28C3E).
unicode_unihan_variant(0x93BB, kSemanticVariant, 0x9396). %<kMatthews
unicode_unihan_variant(0x93BF, kSimplifiedVariant, 0x954E).
unicode_unihan_variant(0x93C1, kSemanticVariant, 0x9396). %<kMeyerWempe
unicode_unihan_variant(0x93C1, kZVariant, 0x9396).
unicode_unihan_variant(0x93C3, kSimplifiedVariant, 0x955E).
unicode_unihan_variant(0x93C6, kSimplifiedVariant, 0x28C4C).
unicode_unihan_variant(0x93C7, kSimplifiedVariant, 0x955F).
unicode_unihan_variant(0x93C8, kSimplifiedVariant, 0x94FE).
unicode_unihan_variant(0x93C9, kSimplifiedVariant, 0x28C52).
unicode_unihan_variant(0x93CC, kSimplifiedVariant, 0x9546).
unicode_unihan_variant(0x93CD, kSimplifiedVariant, 0x9559).
unicode_unihan_variant(0x93D0, kSemanticVariant, 0x938F). %<kMeyerWempe
unicode_unihan_variant(0x93D0, kSimplifiedVariant, 0x9560).
unicode_unihan_variant(0x93D1, kSimplifiedVariant, 0x955D).
unicode_unihan_variant(0x93D7, kSimplifiedVariant, 0x94FF).
unicode_unihan_variant(0x93D8, kSimplifiedVariant, 0x9535).
unicode_unihan_variant(0x93DC, kSimplifiedVariant, 0x9557).
unicode_unihan_variant(0x93DD, kSimplifiedVariant, 0x9558).
unicode_unihan_variant(0x93DE, kSimplifiedVariant, 0x955B).
unicode_unihan_variant(0x93DF, kSimplifiedVariant, 0x94F2).
unicode_unihan_variant(0x93DF, kZVariant, 0x5257).
unicode_unihan_variant(0x93E1, kSimplifiedVariant, 0x955C).
unicode_unihan_variant(0x93E2, kSemanticVariant, 0x9463). %<kLau
unicode_unihan_variant(0x93E2, kSimplifiedVariant, 0x9556).
unicode_unihan_variant(0x93E4, kSimplifiedVariant, 0x9542).
unicode_unihan_variant(0x93E5, kSemanticVariant, 0x92B9). %<kMatthews 0x93FD<kMatthews
unicode_unihan_variant(0x93E6, kSimplifiedVariant, 0x2B4E9).
unicode_unihan_variant(0x93E8, kSimplifiedVariant, 0x933E).
unicode_unihan_variant(0x93F0, kSemanticVariant, 0x6586). %<kMatthews
unicode_unihan_variant(0x93F0, kSimplifiedVariant, 0x955A).
unicode_unihan_variant(0x93F5, kSimplifiedVariant, 0x94E7).
unicode_unihan_variant(0x93F7, kSimplifiedVariant, 0x9564).
unicode_unihan_variant(0x93F9, kSimplifiedVariant, 0x956A).
unicode_unihan_variant(0x93FA, kSimplifiedVariant, 0x497D).
unicode_unihan_variant(0x93FD, kSemanticVariant, 0x92B9). %<kFenn
unicode_unihan_variant(0x93FD, kSimplifiedVariant, 0x9508).
unicode_unihan_variant(0x93FD, kZVariant, 0x92B9).
unicode_unihan_variant(0x93FE, kSemanticVariant, 0x2070E). %<kMeyerWempe
unicode_unihan_variant(0x9400, kSemanticVariant, 0x5331). %<kMatthews 0x6AC3<kMatthews
unicode_unihan_variant(0x9403, kSimplifiedVariant, 0x94D9).
unicode_unihan_variant(0x9404, kSimplifiedVariant, 0x28C51).
unicode_unihan_variant(0x940B, kSimplifiedVariant, 0x94F4).
unicode_unihan_variant(0x940D, kSimplifiedVariant, 0x2B50E).
unicode_unihan_variant(0x940E, kSimplifiedVariant, 0x28C53).
unicode_unihan_variant(0x940F, kSimplifiedVariant, 0x28C54).
unicode_unihan_variant(0x9410, kSimplifiedVariant, 0x9563).
unicode_unihan_variant(0x9412, kSimplifiedVariant, 0x94F9).
unicode_unihan_variant(0x9413, kSimplifiedVariant, 0x9566).
unicode_unihan_variant(0x9414, kSimplifiedVariant, 0x9561).
unicode_unihan_variant(0x9418, kSimplifiedVariant, 0x949F).
unicode_unihan_variant(0x9418, kZVariant, 0x937E).
unicode_unihan_variant(0x9419, kSimplifiedVariant, 0x956B).
unicode_unihan_variant(0x9420, kSimplifiedVariant, 0x9568).
unicode_unihan_variant(0x9421, kZVariant, 0x9435).
unicode_unihan_variant(0x9425, kSimplifiedVariant, 0x4985).
unicode_unihan_variant(0x9426, kSimplifiedVariant, 0x950E).
unicode_unihan_variant(0x9427, kSimplifiedVariant, 0x950F).
unicode_unihan_variant(0x9428, kSimplifiedVariant, 0x9544).
unicode_unihan_variant(0x942B, kSemanticVariant, 0x92D1). %<kMatthews 0x9474<kMatthews
unicode_unihan_variant(0x942B, kSimplifiedVariant, 0x954C).
unicode_unihan_variant(0x942E, kSemanticVariant, 0x938C). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x942E, kSimplifiedVariant, 0x9570).
unicode_unihan_variant(0x942F, kSimplifiedVariant, 0x4983).
unicode_unihan_variant(0x9432, kSimplifiedVariant, 0x956F).
unicode_unihan_variant(0x9433, kSimplifiedVariant, 0x956D).
unicode_unihan_variant(0x9435, kSemanticVariant, 0x9244). %<kLau,kMatthews,kMeyerWempe 0x9295<kMatthews
unicode_unihan_variant(0x9435, kSimplifiedVariant, 0x94C1).
unicode_unihan_variant(0x9436, kSemanticVariant, 0x9370). %<kMatthews
unicode_unihan_variant(0x9436, kSimplifiedVariant, 0x956E).
unicode_unihan_variant(0x9438, kSimplifiedVariant, 0x94CE).
unicode_unihan_variant(0x9438, kZVariant, 0x922C).
unicode_unihan_variant(0x943A, kSimplifiedVariant, 0x94DB).
unicode_unihan_variant(0x943F, kSimplifiedVariant, 0x9571).
unicode_unihan_variant(0x9444, kSimplifiedVariant, 0x94F8).
unicode_unihan_variant(0x944A, kSimplifiedVariant, 0x956C).
unicode_unihan_variant(0x944C, kSimplifiedVariant, 0x9554).
unicode_unihan_variant(0x9451, kSemanticVariant, 0x9452). %<kHKGlyph,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9451, kZVariant, 0x9452).
unicode_unihan_variant(0x9452, kSemanticVariant, 0x9451). %<kHKGlyph,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9452, kSimplifiedVariant, 0x9274).
unicode_unihan_variant(0x9452, kZVariant, 0x9451).
unicode_unihan_variant(0x9454, kSimplifiedVariant, 0x9572).
unicode_unihan_variant(0x9455, kSimplifiedVariant, 0x9527).
unicode_unihan_variant(0x945A, kSemanticVariant, 0x947D). %<kMatthews
unicode_unihan_variant(0x945B, kSemanticVariant, 0x7926). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x945B, kZVariant, 0x7926).
unicode_unihan_variant(0x945E, kSimplifiedVariant, 0x9574).
unicode_unihan_variant(0x9460, kSimplifiedVariant, 0x94C4).
unicode_unihan_variant(0x9462, kSemanticVariant, 0x4968). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9463, kSemanticVariant, 0x93E2). %<kLau
unicode_unihan_variant(0x9463, kSimplifiedVariant, 0x9573).
unicode_unihan_variant(0x9465, kSimplifiedVariant, 0x9565).
unicode_unihan_variant(0x946D, kSimplifiedVariant, 0x9567).
unicode_unihan_variant(0x9470, kSimplifiedVariant, 0x94A5).
unicode_unihan_variant(0x9471, kSimplifiedVariant, 0x9575).
unicode_unihan_variant(0x9472, kSimplifiedVariant, 0x9576).
unicode_unihan_variant(0x9474, kSemanticVariant, 0x92D1). %<kMatthews 0x942B<kMatthews
unicode_unihan_variant(0x9475, kZVariant, 0x7F50).
unicode_unihan_variant(0x9477, kSimplifiedVariant, 0x954A).
unicode_unihan_variant(0x9479, kSimplifiedVariant, 0x9569).
unicode_unihan_variant(0x947C, kSimplifiedVariant, 0x9523).
unicode_unihan_variant(0x947D, kSemanticVariant, 0x9246). %<kPhonetic 0x945A<kMatthews
unicode_unihan_variant(0x947D, kSimplifiedVariant, 0x94BB).
unicode_unihan_variant(0x947E, kSimplifiedVariant, 0x92AE).
unicode_unihan_variant(0x947F, kSimplifiedVariant, 0x51FF).
unicode_unihan_variant(0x9481, kSimplifiedVariant, 0x9562).
unicode_unihan_variant(0x9482, kZVariant, 0x954B).
unicode_unihan_variant(0x9485, kTraditionalVariant, 0x91D2).
unicode_unihan_variant(0x9485, kZVariant, 0x91D1).
unicode_unihan_variant(0x9486, kTraditionalVariant, 0x91D3).
unicode_unihan_variant(0x9487, kTraditionalVariant, 0x91D4).
unicode_unihan_variant(0x9488, kTraditionalVariant, 0x91DD).
unicode_unihan_variant(0x9489, kTraditionalVariant, 0x91D8).
unicode_unihan_variant(0x948A, kTraditionalVariant, 0x91D7).
unicode_unihan_variant(0x948B, kTraditionalVariant, 0x91D9).
unicode_unihan_variant(0x948C, kTraditionalVariant, 0x91D5).
unicode_unihan_variant(0x948D, kTraditionalVariant, 0x91F7).
unicode_unihan_variant(0x948E, kTraditionalVariant, 0x91FA).
unicode_unihan_variant(0x948F, kTraditionalVariant, 0x91E7).
unicode_unihan_variant(0x9490, kTraditionalVariant, 0x91E4).
unicode_unihan_variant(0x9491, kTraditionalVariant, 0x9212).
unicode_unihan_variant(0x9492, kTraditionalVariant, 0x91E9).
unicode_unihan_variant(0x9493, kTraditionalVariant, 0x91E3).
unicode_unihan_variant(0x9494, kTraditionalVariant, 0x9346).
unicode_unihan_variant(0x9495, kTraditionalVariant, 0x91F9).
unicode_unihan_variant(0x9496, kTraditionalVariant, 0x935A).
unicode_unihan_variant(0x9496, kZVariant, 0x9521).
unicode_unihan_variant(0x9497, kTraditionalVariant, 0x91F5).
unicode_unihan_variant(0x9498, kTraditionalVariant, 0x9203).
unicode_unihan_variant(0x9499, kTraditionalVariant, 0x9223).
unicode_unihan_variant(0x949A, kTraditionalVariant, 0x9208).
unicode_unihan_variant(0x949B, kTraditionalVariant, 0x9226).
unicode_unihan_variant(0x949C, kTraditionalVariant, 0x9245).
unicode_unihan_variant(0x949C, kZVariant, 0x5DE8).
unicode_unihan_variant(0x949D, kTraditionalVariant, 0x920D).
unicode_unihan_variant(0x949E, kTraditionalVariant, 0x9214).
unicode_unihan_variant(0x949F, kTraditionalVariant, 0x937E). %0x9418
unicode_unihan_variant(0x94A0, kTraditionalVariant, 0x9209).
unicode_unihan_variant(0x94A1, kTraditionalVariant, 0x92C7).
unicode_unihan_variant(0x94A2, kTraditionalVariant, 0x92FC).
unicode_unihan_variant(0x94A3, kTraditionalVariant, 0x9211).
unicode_unihan_variant(0x94A4, kTraditionalVariant, 0x9210).
unicode_unihan_variant(0x94A5, kTraditionalVariant, 0x9470).
unicode_unihan_variant(0x94A6, kTraditionalVariant, 0x6B3D).
unicode_unihan_variant(0x94A7, kTraditionalVariant, 0x921E).
unicode_unihan_variant(0x94A8, kTraditionalVariant, 0x93A2).
unicode_unihan_variant(0x94A9, kTraditionalVariant, 0x9264).
unicode_unihan_variant(0x94A9, kZVariant, 0x9264).
unicode_unihan_variant(0x94AA, kTraditionalVariant, 0x9227).
unicode_unihan_variant(0x94AB, kTraditionalVariant, 0x9201).
unicode_unihan_variant(0x94AC, kTraditionalVariant, 0x9225).
unicode_unihan_variant(0x94AD, kTraditionalVariant, 0x9204).
unicode_unihan_variant(0x94AE, kTraditionalVariant, 0x9215).
unicode_unihan_variant(0x94AF, kTraditionalVariant, 0x9200).
unicode_unihan_variant(0x94B0, kTraditionalVariant, 0x923A).
unicode_unihan_variant(0x94B1, kTraditionalVariant, 0x9322).
unicode_unihan_variant(0x94B2, kTraditionalVariant, 0x9266).
unicode_unihan_variant(0x94B3, kTraditionalVariant, 0x9257).
unicode_unihan_variant(0x94B3, kZVariant, 0x7B9D).
unicode_unihan_variant(0x94B4, kTraditionalVariant, 0x9237).
unicode_unihan_variant(0x94B5, kTraditionalVariant, 0x7F3D).
unicode_unihan_variant(0x94B5, kZVariant, 0x7F3D).
unicode_unihan_variant(0x94B6, kTraditionalVariant, 0x9233).
unicode_unihan_variant(0x94B7, kTraditionalVariant, 0x9255).
unicode_unihan_variant(0x94B8, kTraditionalVariant, 0x923D).
unicode_unihan_variant(0x94B9, kTraditionalVariant, 0x9238).
unicode_unihan_variant(0x94BA, kTraditionalVariant, 0x925E).
unicode_unihan_variant(0x94BB, kTraditionalVariant, 0x947D).
unicode_unihan_variant(0x94BC, kTraditionalVariant, 0x926C).
unicode_unihan_variant(0x94BD, kTraditionalVariant, 0x926D).
unicode_unihan_variant(0x94BE, kTraditionalVariant, 0x9240).
unicode_unihan_variant(0x94BF, kTraditionalVariant, 0x923F).
unicode_unihan_variant(0x94C0, kTraditionalVariant, 0x923E).
unicode_unihan_variant(0x94C1, kTraditionalVariant, 0x9435).
unicode_unihan_variant(0x94C2, kTraditionalVariant, 0x9251).
unicode_unihan_variant(0x94C3, kTraditionalVariant, 0x9234).
unicode_unihan_variant(0x94C4, kTraditionalVariant, 0x9460).
unicode_unihan_variant(0x94C5, kTraditionalVariant, 0x925B).
unicode_unihan_variant(0x94C6, kTraditionalVariant, 0x925A).
unicode_unihan_variant(0x94C7, kTraditionalVariant, 0x924B).
unicode_unihan_variant(0x94C8, kTraditionalVariant, 0x9230).
unicode_unihan_variant(0x94C9, kTraditionalVariant, 0x9249).
unicode_unihan_variant(0x94CA, kTraditionalVariant, 0x9248).
unicode_unihan_variant(0x94CB, kTraditionalVariant, 0x924D).
unicode_unihan_variant(0x94CC, kTraditionalVariant, 0x922E).
unicode_unihan_variant(0x94CD, kTraditionalVariant, 0x9239).
unicode_unihan_variant(0x94CE, kTraditionalVariant, 0x9438).
unicode_unihan_variant(0x94CF, kTraditionalVariant, 0x9276).
unicode_unihan_variant(0x94D0, kTraditionalVariant, 0x92AC).
unicode_unihan_variant(0x94D1, kTraditionalVariant, 0x92A0).
unicode_unihan_variant(0x94D2, kTraditionalVariant, 0x927A).
unicode_unihan_variant(0x94D3, kTraditionalVariant, 0x92E9).
unicode_unihan_variant(0x94D4, kTraditionalVariant, 0x930F).
unicode_unihan_variant(0x94D5, kTraditionalVariant, 0x92AA).
unicode_unihan_variant(0x94D6, kTraditionalVariant, 0x92EE).
unicode_unihan_variant(0x94D7, kTraditionalVariant, 0x92CF).
unicode_unihan_variant(0x94D8, kTraditionalVariant, 0x92E3).
unicode_unihan_variant(0x94D9, kTraditionalVariant, 0x9403).
unicode_unihan_variant(0x94DA, kTraditionalVariant, 0x928D).
unicode_unihan_variant(0x94DB, kTraditionalVariant, 0x943A).
unicode_unihan_variant(0x94DC, kTraditionalVariant, 0x9285).
unicode_unihan_variant(0x94DD, kTraditionalVariant, 0x92C1).
unicode_unihan_variant(0x94DE, kTraditionalVariant, 0x92B1).
unicode_unihan_variant(0x94DF, kTraditionalVariant, 0x92A6).
unicode_unihan_variant(0x94E0, kTraditionalVariant, 0x93A7).
unicode_unihan_variant(0x94E1, kTraditionalVariant, 0x9358).
unicode_unihan_variant(0x94E2, kTraditionalVariant, 0x9296).
unicode_unihan_variant(0x94E3, kTraditionalVariant, 0x9291).
unicode_unihan_variant(0x94E4, kTraditionalVariant, 0x92CC).
unicode_unihan_variant(0x94E5, kTraditionalVariant, 0x92A9).
unicode_unihan_variant(0x94E6, kTraditionalVariant, 0x929B).
unicode_unihan_variant(0x94E7, kTraditionalVariant, 0x93F5).
unicode_unihan_variant(0x94E8, kTraditionalVariant, 0x9293).
unicode_unihan_variant(0x94E9, kTraditionalVariant, 0x93A9).
unicode_unihan_variant(0x94EA, kTraditionalVariant, 0x927F).
unicode_unihan_variant(0x94EB, kTraditionalVariant, 0x929A).
unicode_unihan_variant(0x94EC, kTraditionalVariant, 0x927B).
unicode_unihan_variant(0x94ED, kTraditionalVariant, 0x9298).
unicode_unihan_variant(0x94EE, kTraditionalVariant, 0x931A).
unicode_unihan_variant(0x94EF, kTraditionalVariant, 0x92AB).
unicode_unihan_variant(0x94F0, kTraditionalVariant, 0x9278).
unicode_unihan_variant(0x94F1, kTraditionalVariant, 0x92A5).
unicode_unihan_variant(0x94F2, kTraditionalVariant, 0x93DF).
unicode_unihan_variant(0x94F3, kTraditionalVariant, 0x9283).
unicode_unihan_variant(0x94F4, kTraditionalVariant, 0x940B).
unicode_unihan_variant(0x94F5, kTraditionalVariant, 0x92A8).
unicode_unihan_variant(0x94F6, kTraditionalVariant, 0x9280).
unicode_unihan_variant(0x94F7, kTraditionalVariant, 0x92A3).
unicode_unihan_variant(0x94F8, kTraditionalVariant, 0x9444).
unicode_unihan_variant(0x94F9, kTraditionalVariant, 0x9412).
unicode_unihan_variant(0x94FA, kTraditionalVariant, 0x92EA).
unicode_unihan_variant(0x94FB, kTraditionalVariant, 0x92D9).
unicode_unihan_variant(0x94FC, kTraditionalVariant, 0x9338).
unicode_unihan_variant(0x94FD, kTraditionalVariant, 0x92F1).
unicode_unihan_variant(0x94FE, kTraditionalVariant, 0x93C8).
unicode_unihan_variant(0x94FF, kTraditionalVariant, 0x93D7).
unicode_unihan_variant(0x9500, kTraditionalVariant, 0x92B7).
unicode_unihan_variant(0x9501, kTraditionalVariant, 0x9396).
unicode_unihan_variant(0x9502, kTraditionalVariant, 0x92F0).
unicode_unihan_variant(0x9503, kTraditionalVariant, 0x92E5).
unicode_unihan_variant(0x9504, kTraditionalVariant, 0x92E4).
unicode_unihan_variant(0x9505, kTraditionalVariant, 0x934B).
unicode_unihan_variant(0x9506, kTraditionalVariant, 0x92EF).
unicode_unihan_variant(0x9507, kTraditionalVariant, 0x92E8).
unicode_unihan_variant(0x9508, kTraditionalVariant, 0x93FD).
unicode_unihan_variant(0x9508, kZVariant, 0x93FD).
unicode_unihan_variant(0x9509, kTraditionalVariant, 0x92BC).
unicode_unihan_variant(0x950A, kTraditionalVariant, 0x92DD).
unicode_unihan_variant(0x950B, kTraditionalVariant, 0x92D2).
unicode_unihan_variant(0x950C, kTraditionalVariant, 0x92C5).
unicode_unihan_variant(0x950D, kTraditionalVariant, 0x92F6).
unicode_unihan_variant(0x950E, kTraditionalVariant, 0x9426).
unicode_unihan_variant(0x950F, kTraditionalVariant, 0x9427).
unicode_unihan_variant(0x9510, kTraditionalVariant, 0x92B3).
unicode_unihan_variant(0x9510, kZVariant, 0x92B3).
unicode_unihan_variant(0x9511, kTraditionalVariant, 0x92BB).
unicode_unihan_variant(0x9512, kTraditionalVariant, 0x92C3).
unicode_unihan_variant(0x9513, kTraditionalVariant, 0x92DF).
unicode_unihan_variant(0x9514, kTraditionalVariant, 0x92E6).
unicode_unihan_variant(0x9515, kTraditionalVariant, 0x9312).
unicode_unihan_variant(0x9516, kTraditionalVariant, 0x9306).
unicode_unihan_variant(0x9517, kTraditionalVariant, 0x937A).
unicode_unihan_variant(0x9518, kTraditionalVariant, 0x9369).
unicode_unihan_variant(0x9519, kTraditionalVariant, 0x932F).
unicode_unihan_variant(0x951A, kTraditionalVariant, 0x9328).
unicode_unihan_variant(0x951B, kTraditionalVariant, 0x931B).
unicode_unihan_variant(0x951C, kTraditionalVariant, 0x9321).
unicode_unihan_variant(0x951D, kTraditionalVariant, 0x9340).
unicode_unihan_variant(0x951E, kTraditionalVariant, 0x9301).
unicode_unihan_variant(0x951F, kTraditionalVariant, 0x9315).
unicode_unihan_variant(0x9520, kTraditionalVariant, 0x9329).
unicode_unihan_variant(0x9521, kTraditionalVariant, 0x932B).
unicode_unihan_variant(0x9522, kTraditionalVariant, 0x932E).
unicode_unihan_variant(0x9523, kTraditionalVariant, 0x947C).
unicode_unihan_variant(0x9524, kTraditionalVariant, 0x9318).
unicode_unihan_variant(0x9525, kTraditionalVariant, 0x9310).
unicode_unihan_variant(0x9526, kTraditionalVariant, 0x9326).
unicode_unihan_variant(0x9527, kTraditionalVariant, 0x9455).
unicode_unihan_variant(0x9528, kTraditionalVariant, 0x6774).
unicode_unihan_variant(0x9529, kTraditionalVariant, 0x9308).
unicode_unihan_variant(0x952A, kTraditionalVariant, 0x9343).
unicode_unihan_variant(0x952B, kTraditionalVariant, 0x9307).
unicode_unihan_variant(0x952C, kTraditionalVariant, 0x931F).
unicode_unihan_variant(0x952D, kTraditionalVariant, 0x9320).
unicode_unihan_variant(0x952E, kTraditionalVariant, 0x9375).
unicode_unihan_variant(0x952F, kTraditionalVariant, 0x92F8).
unicode_unihan_variant(0x9530, kTraditionalVariant, 0x9333).
unicode_unihan_variant(0x9531, kTraditionalVariant, 0x9319).
unicode_unihan_variant(0x9532, kTraditionalVariant, 0x9365).
unicode_unihan_variant(0x9533, kTraditionalVariant, 0x9348).
unicode_unihan_variant(0x9534, kTraditionalVariant, 0x9347).
unicode_unihan_variant(0x9535, kTraditionalVariant, 0x93D8).
unicode_unihan_variant(0x9536, kTraditionalVariant, 0x9376).
unicode_unihan_variant(0x9537, kTraditionalVariant, 0x9354).
unicode_unihan_variant(0x9538, kTraditionalVariant, 0x9364).
unicode_unihan_variant(0x9539, kTraditionalVariant, 0x936C).
unicode_unihan_variant(0x953A, kTraditionalVariant, 0x937E).
unicode_unihan_variant(0x953B, kTraditionalVariant, 0x935B).
unicode_unihan_variant(0x953C, kTraditionalVariant, 0x93AA).
unicode_unihan_variant(0x953D, kTraditionalVariant, 0x9360).
unicode_unihan_variant(0x953E, kTraditionalVariant, 0x9370).
unicode_unihan_variant(0x953F, kTraditionalVariant, 0x9384).
unicode_unihan_variant(0x9540, kTraditionalVariant, 0x934D).
unicode_unihan_variant(0x9541, kTraditionalVariant, 0x9382).
unicode_unihan_variant(0x9542, kTraditionalVariant, 0x93E4).
unicode_unihan_variant(0x9543, kTraditionalVariant, 0x93A1).
unicode_unihan_variant(0x9544, kTraditionalVariant, 0x9428).
unicode_unihan_variant(0x9545, kTraditionalVariant, 0x9387).
unicode_unihan_variant(0x9546, kTraditionalVariant, 0x93CC).
unicode_unihan_variant(0x9547, kTraditionalVariant, 0x93AE).
unicode_unihan_variant(0x9548, kTraditionalVariant, 0x939B).
unicode_unihan_variant(0x9549, kTraditionalVariant, 0x9398).
unicode_unihan_variant(0x954A, kTraditionalVariant, 0x9477).
unicode_unihan_variant(0x954B, kTraditionalVariant, 0x93B2).
unicode_unihan_variant(0x954B, kZVariant, 0x9482).
unicode_unihan_variant(0x954C, kTraditionalVariant, 0x942B).
unicode_unihan_variant(0x954D, kTraditionalVariant, 0x93B3).
unicode_unihan_variant(0x954E, kTraditionalVariant, 0x93BF).
unicode_unihan_variant(0x954F, kTraditionalVariant, 0x93A6).
unicode_unihan_variant(0x9550, kTraditionalVariant, 0x93AC).
unicode_unihan_variant(0x9551, kTraditionalVariant, 0x938A).
unicode_unihan_variant(0x9552, kTraditionalVariant, 0x93B0).
unicode_unihan_variant(0x9553, kTraditionalVariant, 0x93B5).
unicode_unihan_variant(0x9554, kTraditionalVariant, 0x944C).
unicode_unihan_variant(0x9555, kTraditionalVariant, 0x9394).
unicode_unihan_variant(0x9556, kTraditionalVariant, 0x93E2).
unicode_unihan_variant(0x9557, kTraditionalVariant, 0x93DC).
unicode_unihan_variant(0x9558, kTraditionalVariant, 0x93DD).
unicode_unihan_variant(0x9559, kTraditionalVariant, 0x93CD).
unicode_unihan_variant(0x955A, kTraditionalVariant, 0x93F0).
unicode_unihan_variant(0x955A, kZVariant, 0x930B).
unicode_unihan_variant(0x955B, kTraditionalVariant, 0x93DE).
unicode_unihan_variant(0x955C, kTraditionalVariant, 0x93E1).
unicode_unihan_variant(0x955D, kTraditionalVariant, 0x93D1).
unicode_unihan_variant(0x955E, kTraditionalVariant, 0x93C3).
unicode_unihan_variant(0x955F, kTraditionalVariant, 0x93C7).
unicode_unihan_variant(0x9560, kTraditionalVariant, 0x93D0).
unicode_unihan_variant(0x9561, kTraditionalVariant, 0x9414).
unicode_unihan_variant(0x9562, kTraditionalVariant, 0x9481).
unicode_unihan_variant(0x9563, kTraditionalVariant, 0x9410).
unicode_unihan_variant(0x9564, kTraditionalVariant, 0x93F7).
unicode_unihan_variant(0x9565, kTraditionalVariant, 0x9465).
unicode_unihan_variant(0x9566, kTraditionalVariant, 0x9413).
unicode_unihan_variant(0x9567, kTraditionalVariant, 0x946D).
unicode_unihan_variant(0x9568, kTraditionalVariant, 0x9420).
unicode_unihan_variant(0x9569, kTraditionalVariant, 0x9479).
unicode_unihan_variant(0x956A, kTraditionalVariant, 0x93F9).
unicode_unihan_variant(0x956B, kTraditionalVariant, 0x9419).
unicode_unihan_variant(0x956C, kTraditionalVariant, 0x944A).
unicode_unihan_variant(0x956D, kTraditionalVariant, 0x9433).
unicode_unihan_variant(0x956E, kTraditionalVariant, 0x9436).
unicode_unihan_variant(0x956F, kTraditionalVariant, 0x9432).
unicode_unihan_variant(0x9570, kTraditionalVariant, 0x942E).
unicode_unihan_variant(0x9571, kTraditionalVariant, 0x943F).
unicode_unihan_variant(0x9572, kTraditionalVariant, 0x9454).
unicode_unihan_variant(0x9573, kTraditionalVariant, 0x9463).
unicode_unihan_variant(0x9574, kTraditionalVariant, 0x945E).
unicode_unihan_variant(0x9575, kTraditionalVariant, 0x9471).
unicode_unihan_variant(0x9576, kTraditionalVariant, 0x9472).
unicode_unihan_variant(0x9577, kSimplifiedVariant, 0x957F).
unicode_unihan_variant(0x957F, kTraditionalVariant, 0x9577).
unicode_unihan_variant(0x9580, kSemanticVariant, 0x95E8). %<kMeyerWempe
unicode_unihan_variant(0x9580, kSimplifiedVariant, 0x95E8).
unicode_unihan_variant(0x9580, kSpecializedSemanticVariant, 0x95E8). %<kFenn
unicode_unihan_variant(0x9582, kSimplifiedVariant, 0x95E9).
unicode_unihan_variant(0x9583, kSimplifiedVariant, 0x95EA).
unicode_unihan_variant(0x9586, kSemanticVariant, 0x95BB). %<kMatthews
unicode_unihan_variant(0x9586, kSimplifiedVariant, 0x95EB).
unicode_unihan_variant(0x9587, kZVariant, 0x9589).
unicode_unihan_variant(0x9588, kSimplifiedVariant, 0x95EC).
unicode_unihan_variant(0x9589, kSimplifiedVariant, 0x95ED).
unicode_unihan_variant(0x9589, kZVariant, 0x9587).
unicode_unihan_variant(0x958B, kSimplifiedVariant, 0x5F00).
unicode_unihan_variant(0x958C, kSimplifiedVariant, 0x95F6).
unicode_unihan_variant(0x958D, kSimplifiedVariant, 0x28E02).
unicode_unihan_variant(0x958E, kSimplifiedVariant, 0x95F3).
unicode_unihan_variant(0x958F, kSimplifiedVariant, 0x95F0).
unicode_unihan_variant(0x9590, kSimplifiedVariant, 0x28E03).
unicode_unihan_variant(0x9591, kSimplifiedVariant, 0x95F2).
unicode_unihan_variant(0x9592, kSpecializedSemanticVariant, 0x9593). %<kMeyerWempe
unicode_unihan_variant(0x9593, kSimplifiedVariant, 0x95F4).
unicode_unihan_variant(0x9593, kSpecializedSemanticVariant, 0x9592). %<kMeyerWempe
unicode_unihan_variant(0x9594, kSimplifiedVariant, 0x95F5).
unicode_unihan_variant(0x9598, kSimplifiedVariant, 0x95F8).
unicode_unihan_variant(0x9599, kSemanticVariant, 0x9B27). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9599, kZVariant, 0x9B27).
unicode_unihan_variant(0x95A1, kSimplifiedVariant, 0x9602).
unicode_unihan_variant(0x95A2, kSemanticVariant, 0x95D7). %<kMatthews 0x95DC<kMatthews 0x28DB9<kLau
unicode_unihan_variant(0x95A2, kZVariant, 0x95DC).
unicode_unihan_variant(0x95A3, kSimplifiedVariant, 0x9601).
unicode_unihan_variant(0x95A4, kZVariant, 0x5408).
unicode_unihan_variant(0x95A5, kSimplifiedVariant, 0x9600).
unicode_unihan_variant(0x95A7, kSemanticVariant, 0x9B28). %<kMatthews
unicode_unihan_variant(0x95A7, kZVariant, 0x54C4).
unicode_unihan_variant(0x95A8, kSimplifiedVariant, 0x95FA).
unicode_unihan_variant(0x95A9, kSimplifiedVariant, 0x95FD).
unicode_unihan_variant(0x95AB, kSimplifiedVariant, 0x9603).
unicode_unihan_variant(0x95AC, kSimplifiedVariant, 0x9606).
unicode_unihan_variant(0x95AD, kSimplifiedVariant, 0x95FE).
unicode_unihan_variant(0x95AD, kZVariant, 0xF986).
unicode_unihan_variant(0x95B1, kSimplifiedVariant, 0x9605).
unicode_unihan_variant(0x95B1, kZVariant, 0x95B2).
unicode_unihan_variant(0x95B2, kZVariant, 0x95B1).
unicode_unihan_variant(0x95B6, kSimplifiedVariant, 0x960A).
unicode_unihan_variant(0x95B9, kSemanticVariant, 0x5266). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x95B9, kSimplifiedVariant, 0x9609).
unicode_unihan_variant(0x95BB, kSemanticVariant, 0x9586). %<kMatthews
unicode_unihan_variant(0x95BB, kSimplifiedVariant, 0x960E).
unicode_unihan_variant(0x95BC, kSimplifiedVariant, 0x960F).
unicode_unihan_variant(0x95BD, kSimplifiedVariant, 0x960D).
unicode_unihan_variant(0x95BE, kSimplifiedVariant, 0x9608).
unicode_unihan_variant(0x95BF, kSimplifiedVariant, 0x960C).
unicode_unihan_variant(0x95C3, kSimplifiedVariant, 0x9612).
unicode_unihan_variant(0x95C6, kSimplifiedVariant, 0x677F).
unicode_unihan_variant(0x95C7, kZVariant, 0x6697).
unicode_unihan_variant(0x95C8, kSimplifiedVariant, 0x95F1).
unicode_unihan_variant(0x95CA, kSemanticVariant, 0x6FF6). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x95CA, kSimplifiedVariant, 0x9614).
unicode_unihan_variant(0x95CB, kSimplifiedVariant, 0x9615).
unicode_unihan_variant(0x95CC, kSimplifiedVariant, 0x9611).
unicode_unihan_variant(0x95CD, kSimplifiedVariant, 0x9607).
unicode_unihan_variant(0x95D0, kSimplifiedVariant, 0x9617).
unicode_unihan_variant(0x95D2, kSimplifiedVariant, 0x9618).
unicode_unihan_variant(0x95D3, kSimplifiedVariant, 0x95FF).
unicode_unihan_variant(0x95D4, kSimplifiedVariant, 0x9616).
unicode_unihan_variant(0x95D4, kZVariant, 0x95A4).
unicode_unihan_variant(0x95D5, kSimplifiedVariant, 0x9619).
unicode_unihan_variant(0x95D6, kSimplifiedVariant, 0x95EF).
unicode_unihan_variant(0x95D7, kSemanticVariant, 0x95A2). %<kMatthews 0x95DC<kMatthews,kMeyerWempe
unicode_unihan_variant(0x95D7, kZVariant, 0x95DC).
unicode_unihan_variant(0x95D8, kZVariant, 0x9B25).
unicode_unihan_variant(0x95DC, kSemanticVariant, 0x5BE1). %<kLau 0x95A2<kMatthews 0x95D7<kMatthews,kMeyerWempe
unicode_unihan_variant(0x95DC, kSimplifiedVariant, 0x5173).
unicode_unihan_variant(0x95DE, kSimplifiedVariant, 0x961A).
unicode_unihan_variant(0x95E0, kSimplifiedVariant, 0x9613).
unicode_unihan_variant(0x95E1, kSimplifiedVariant, 0x9610).
unicode_unihan_variant(0x95E4, kSimplifiedVariant, 0x961B).
unicode_unihan_variant(0x95E5, kSimplifiedVariant, 0x95FC).
unicode_unihan_variant(0x95E8, kSemanticVariant, 0x9580). %<kMeyerWempe
unicode_unihan_variant(0x95E8, kSpecializedSemanticVariant, 0x9580). %<kFenn
unicode_unihan_variant(0x95E8, kTraditionalVariant, 0x9580).
unicode_unihan_variant(0x95E9, kTraditionalVariant, 0x9582).
unicode_unihan_variant(0x95EA, kTraditionalVariant, 0x9583).
unicode_unihan_variant(0x95EB, kTraditionalVariant, 0x9586).
unicode_unihan_variant(0x95EB, kZVariant, 0x95BB).
unicode_unihan_variant(0x95EC, kTraditionalVariant, 0x9588).
unicode_unihan_variant(0x95ED, kTraditionalVariant, 0x9589).
unicode_unihan_variant(0x95EE, kTraditionalVariant, 0x554F).
unicode_unihan_variant(0x95EF, kTraditionalVariant, 0x95D6).
unicode_unihan_variant(0x95F0, kTraditionalVariant, 0x958F).
unicode_unihan_variant(0x95F1, kTraditionalVariant, 0x95C8).
unicode_unihan_variant(0x95F2, kTraditionalVariant, 0x9591).
unicode_unihan_variant(0x95F3, kTraditionalVariant, 0x958E).
unicode_unihan_variant(0x95F4, kTraditionalVariant, 0x9593).
unicode_unihan_variant(0x95F5, kTraditionalVariant, 0x9594).
unicode_unihan_variant(0x95F6, kTraditionalVariant, 0x958C).
unicode_unihan_variant(0x95F7, kTraditionalVariant, 0x60B6).
unicode_unihan_variant(0x95F8, kTraditionalVariant, 0x9598).
unicode_unihan_variant(0x95F9, kTraditionalVariant, 0x9B27).
unicode_unihan_variant(0x95FA, kTraditionalVariant, 0x95A8).
unicode_unihan_variant(0x95FB, kTraditionalVariant, 0x805E).
unicode_unihan_variant(0x95FC, kTraditionalVariant, 0x95E5).
unicode_unihan_variant(0x95FD, kTraditionalVariant, 0x95A9).
unicode_unihan_variant(0x95FE, kTraditionalVariant, 0x95AD).
unicode_unihan_variant(0x95FF, kTraditionalVariant, 0x95D3).
unicode_unihan_variant(0x9600, kTraditionalVariant, 0x95A5).
unicode_unihan_variant(0x9601, kTraditionalVariant, 0x95A3).
unicode_unihan_variant(0x9602, kTraditionalVariant, 0x95A1).
unicode_unihan_variant(0x9603, kTraditionalVariant, 0x95AB).
unicode_unihan_variant(0x9604, kTraditionalVariant, 0x9B2E).
unicode_unihan_variant(0x9605, kTraditionalVariant, 0x95B1).
unicode_unihan_variant(0x9605, kZVariant, 0x95B1).
unicode_unihan_variant(0x9606, kTraditionalVariant, 0x95AC).
unicode_unihan_variant(0x9607, kTraditionalVariant, 0x95CD).
unicode_unihan_variant(0x9608, kTraditionalVariant, 0x95BE).
unicode_unihan_variant(0x9609, kTraditionalVariant, 0x95B9).
unicode_unihan_variant(0x960A, kTraditionalVariant, 0x95B6).
unicode_unihan_variant(0x960B, kTraditionalVariant, 0x9B29).
unicode_unihan_variant(0x960C, kTraditionalVariant, 0x95BF).
unicode_unihan_variant(0x960D, kTraditionalVariant, 0x95BD).
unicode_unihan_variant(0x960E, kTraditionalVariant, 0x95BB).
unicode_unihan_variant(0x960F, kTraditionalVariant, 0x95BC).
unicode_unihan_variant(0x9610, kTraditionalVariant, 0x95E1).
unicode_unihan_variant(0x9611, kTraditionalVariant, 0x95CC).
unicode_unihan_variant(0x9612, kTraditionalVariant, 0x95C3).
unicode_unihan_variant(0x9613, kTraditionalVariant, 0x95E0).
unicode_unihan_variant(0x9614, kTraditionalVariant, 0x95CA).
unicode_unihan_variant(0x9615, kTraditionalVariant, 0x95CB).
unicode_unihan_variant(0x9616, kTraditionalVariant, 0x95D4).
unicode_unihan_variant(0x9616, kZVariant, 0x95A4).
unicode_unihan_variant(0x9617, kTraditionalVariant, 0x95D0).
unicode_unihan_variant(0x9618, kTraditionalVariant, 0x95D2).
unicode_unihan_variant(0x9619, kTraditionalVariant, 0x95D5).
unicode_unihan_variant(0x961A, kTraditionalVariant, 0x95DE).
unicode_unihan_variant(0x961B, kTraditionalVariant, 0x95E4).
unicode_unihan_variant(0x961C, kSemanticVariant, 0x961D). %<kMatthews
unicode_unihan_variant(0x961C, kZVariant, 0x961D).
unicode_unihan_variant(0x961D, kSemanticVariant, 0x961C). %<kMatthews
unicode_unihan_variant(0x961D, kZVariant, 0x9091).
unicode_unihan_variant(0x961F, kTraditionalVariant, 0x968A).
unicode_unihan_variant(0x9621, kSemanticVariant, 0x4EDF). %<kFenn
unicode_unihan_variant(0x9627, kSemanticVariant, 0x9661). %<kMatthews
unicode_unihan_variant(0x9628, kSemanticVariant, 0x9638). %<kMatthews
unicode_unihan_variant(0x9628, kSpecializedSemanticVariant, 0x5443). %<kMeyerWempe
unicode_unihan_variant(0x9628, kZVariant, 0x5384).
unicode_unihan_variant(0x962A, kSimplifiedVariant, 0x5742).
unicode_unihan_variant(0x962C, kSemanticVariant, 0x5751). %<kMatthews
unicode_unihan_variant(0x962C, kZVariant, 0x5751).
unicode_unihan_variant(0x962E, kZVariant, 0xF9C6).
unicode_unihan_variant(0x962F, kZVariant, 0x5740).
unicode_unihan_variant(0x9631, kSemanticVariant, 0x7A7D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9631, kSpecializedSemanticVariant, 0x6C6B). %<kHanYu
unicode_unihan_variant(0x9631, kZVariant, 0x7A7D).
unicode_unihan_variant(0x9633, kSemanticVariant, 0x6C1C). %<kFenn 0x967D<kMatthews
unicode_unihan_variant(0x9633, kTraditionalVariant, 0x967D).
unicode_unihan_variant(0x9634, kSemanticVariant, 0x9670). %<kMatthews
unicode_unihan_variant(0x9634, kTraditionalVariant, 0x9670).
unicode_unihan_variant(0x9635, kTraditionalVariant, 0x9663).
unicode_unihan_variant(0x9636, kTraditionalVariant, 0x968E).
unicode_unihan_variant(0x9638, kSemanticVariant, 0x9628). %<kMatthews
unicode_unihan_variant(0x963F, kSpecializedSemanticVariant, 0x554A). %<kFenn
unicode_unihan_variant(0x9640, kSemanticVariant, 0x9641). %<kMatthews
unicode_unihan_variant(0x9641, kSemanticVariant, 0x9640). %<kMatthews
unicode_unihan_variant(0x9645, kTraditionalVariant, 0x969B).
unicode_unihan_variant(0x9646, kTraditionalVariant, 0x9678).
unicode_unihan_variant(0x9647, kTraditionalVariant, 0x96B4).
unicode_unihan_variant(0x9648, kTraditionalVariant, 0x9673).
unicode_unihan_variant(0x9649, kTraditionalVariant, 0x9658).
unicode_unihan_variant(0x964B, kZVariant, 0xF951).
unicode_unihan_variant(0x964D, kSemanticVariant, 0x5905). %<kMatthews
unicode_unihan_variant(0x964D, kSpecializedSemanticVariant, 0x5905). %<kFenn
unicode_unihan_variant(0x964D, kZVariant, 0xFA09).
unicode_unihan_variant(0x964F, kZVariant, 0x968B).
unicode_unihan_variant(0x9654, kZVariant, 0x5793).
unicode_unihan_variant(0x9655, kTraditionalVariant, 0x965D).
unicode_unihan_variant(0x9657, kSemanticVariant, 0x5CED). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9658, kSimplifiedVariant, 0x9649).
unicode_unihan_variant(0x965C, kSemanticVariant, 0x72F9). %<kFenn
unicode_unihan_variant(0x965C, kZVariant, 0x965D).
unicode_unihan_variant(0x965D, kSimplifiedVariant, 0x9655).
unicode_unihan_variant(0x965D, kZVariant, 0x965C).
unicode_unihan_variant(0x965E, kZVariant, 0x5347).
unicode_unihan_variant(0x9661, kSemanticVariant, 0x9627). %<kMatthews
unicode_unihan_variant(0x9663, kSemanticVariant, 0x28E2C). %<kLau
unicode_unihan_variant(0x9663, kSimplifiedVariant, 0x9635).
unicode_unihan_variant(0x9663, kSpecializedSemanticVariant, 0x9673). %<kMeyerWempe
unicode_unihan_variant(0x9665, kZVariant, 0x9677).
unicode_unihan_variant(0x9667, kTraditionalVariant, 0x9689).
unicode_unihan_variant(0x9668, kTraditionalVariant, 0x9695).
unicode_unihan_variant(0x9669, kTraditionalVariant, 0x96AA).
unicode_unihan_variant(0x9670, kSemanticVariant, 0x9634). %<kMatthews 0x9682<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9670, kSimplifiedVariant, 0x9634).
unicode_unihan_variant(0x9673, kSimplifiedVariant, 0x9648).
unicode_unihan_variant(0x9673, kSpecializedSemanticVariant, 0x9663). %<kMeyerWempe
unicode_unihan_variant(0x9675, kZVariant, 0xF959).
unicode_unihan_variant(0x9676, kSemanticVariant, 0x530B). %<kLau,kMatthews
unicode_unihan_variant(0x9677, kZVariant, 0x9665).
unicode_unihan_variant(0x9678, kSimplifiedVariant, 0x9646).
unicode_unihan_variant(0x9678, kSpecializedSemanticVariant, 0x516D).
unicode_unihan_variant(0x967A, kZVariant, 0x96AA).
unicode_unihan_variant(0x967B, kSemanticVariant, 0x5794). %<kMatthews,kMeyerWempe 0x5819<kMatthews,kMeyerWempe
unicode_unihan_variant(0x967C, kSemanticVariant, 0x6E1A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x967D, kSemanticVariant, 0x6C1C). %<kMatthews 0x9633<kMatthews
unicode_unihan_variant(0x967D, kSimplifiedVariant, 0x9633).
unicode_unihan_variant(0x967D, kZVariant, 0x661C).
unicode_unihan_variant(0x9682, kSemanticVariant, 0x9670). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9684, kSemanticVariant, 0x5824). %<kMeyerWempe
unicode_unihan_variant(0x9686, kZVariant, 0xF9DC).
unicode_unihan_variant(0x9688, kSemanticVariant, 0x6E28). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9688, kSpecializedSemanticVariant, 0x504E). %<kMeyerWempe
unicode_unihan_variant(0x9689, kSimplifiedVariant, 0x9667).
unicode_unihan_variant(0x968A, kSimplifiedVariant, 0x961F).
unicode_unihan_variant(0x968B, kZVariant, 0x964F).
unicode_unihan_variant(0x968D, kSemanticVariant, 0x582D). %<kMatthews
unicode_unihan_variant(0x968E, kSemanticVariant, 0x5826). %<kMatthews
unicode_unihan_variant(0x968E, kSimplifiedVariant, 0x9636).
unicode_unihan_variant(0x968F, kTraditionalVariant, 0x96A8).
unicode_unihan_variant(0x9690, kSemanticVariant, 0x96A0). %<kFenn
unicode_unihan_variant(0x9690, kTraditionalVariant, 0x96B1).
unicode_unihan_variant(0x9695, kSemanticVariant, 0x78D2). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9695, kSimplifiedVariant, 0x9668).
unicode_unihan_variant(0x9695, kZVariant, 0x6B92).
unicode_unihan_variant(0x9696, kSemanticVariant, 0x5862). %<kMatthews
unicode_unihan_variant(0x969B, kSimplifiedVariant, 0x9645).
unicode_unihan_variant(0x969D, kSemanticVariant, 0x5862). %<kLau
unicode_unihan_variant(0x96A0, kSemanticVariant, 0x9690). %<kFenn
unicode_unihan_variant(0x96A0, kZVariant, 0x96B1).
unicode_unihan_variant(0x96A3, kSemanticVariant, 0x9130). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x96A3, kZVariant, 0x9130).
unicode_unihan_variant(0x96A4, kSemanticVariant, 0x58A4). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x96A7, kSemanticVariant, 0x2145E). %<kLau,kMeyerWempe
unicode_unihan_variant(0x96A8, kSimplifiedVariant, 0x968F).
unicode_unihan_variant(0x96A9, kSemanticVariant, 0x21483). %<kMeyerWempe
unicode_unihan_variant(0x96AA, kSimplifiedVariant, 0x9669).
unicode_unihan_variant(0x96AE, kSemanticVariant, 0x8E8B). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x96B1, kSemanticVariant, 0x4E5A). %<kMatthews
unicode_unihan_variant(0x96B1, kSimplifiedVariant, 0x9690).
unicode_unihan_variant(0x96B2, kSemanticVariant, 0x9A2D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x96B4, kSimplifiedVariant, 0x9647).
unicode_unihan_variant(0x96B6, kTraditionalVariant, 0x96B8).
unicode_unihan_variant(0x96B7, kSemanticVariant, 0x96B8). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x96B7, kZVariant, 0x96B8).
unicode_unihan_variant(0x96B8, kSemanticVariant, 0x96B7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x96B8, kSimplifiedVariant, 0x96B6).
unicode_unihan_variant(0x96B8, kZVariant, 0x96B7).
unicode_unihan_variant(0x96BB, kSimplifiedVariant, 0x53EA).
unicode_unihan_variant(0x96BC, kSemanticVariant, 0x9DBD). %<kMatthews 0x9D7B<kMeyerWempe
unicode_unihan_variant(0x96BD, kSemanticVariant, 0x96CB). %<kMatthews
unicode_unihan_variant(0x96BD, kTraditionalVariant, 0x96CB).
unicode_unihan_variant(0x96BE, kSemanticVariant, 0x96E3). %<kLau,kMatthews
unicode_unihan_variant(0x96BE, kSpecializedSemanticVariant, 0x96E3). %<kFenn
unicode_unihan_variant(0x96BE, kTraditionalVariant, 0x96E3).
unicode_unihan_variant(0x96C1, kSemanticVariant, 0x9D08). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x96C1, kZVariant, 0x9CEB).
unicode_unihan_variant(0x96C6, kSemanticVariant, 0x4EBC). %<kLau,kMatthews
unicode_unihan_variant(0x96C7, kSemanticVariant, 0x50F1). %<kLau,kMeyerWempe
unicode_unihan_variant(0x96CB, kSemanticVariant, 0x96BD). %<kMatthews
unicode_unihan_variant(0x96CB, kSimplifiedVariant, 0x96BD).
unicode_unihan_variant(0x96CD, kSemanticVariant, 0x9095). %<kMatthews,kMeyerWempe 0x2AA9D<kMatthews
unicode_unihan_variant(0x96CF, kTraditionalVariant, 0x96DB).
unicode_unihan_variant(0x96D1, kZVariant, 0x96DC).
unicode_unihan_variant(0x96D5, kSemanticVariant, 0x34EE). %<kLau,kMatthews 0x5F6B<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x96D5, kSpecializedSemanticVariant, 0x9D70).
unicode_unihan_variant(0x96D6, kSimplifiedVariant, 0x867D).
unicode_unihan_variant(0x96D9, kSemanticVariant, 0x53CC). %<kMatthews
unicode_unihan_variant(0x96D9, kSimplifiedVariant, 0x53CC).
unicode_unihan_variant(0x96D9, kSpecializedSemanticVariant, 0x53CC). %<kFenn
unicode_unihan_variant(0x96DB, kSemanticVariant, 0x9DB5). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x96DB, kSimplifiedVariant, 0x96CF).
unicode_unihan_variant(0x96DC, kSemanticVariant, 0x894D). %<kMatthews
unicode_unihan_variant(0x96DC, kSimplifiedVariant, 0x6742).
unicode_unihan_variant(0x96DC, kZVariant, 0x96D1).
unicode_unihan_variant(0x96DD, kSemanticVariant, 0x7C0C). %<kCowles
unicode_unihan_variant(0x96DE, kSemanticVariant, 0x9DC4). %<kLau,kMatthews 0x9CEE<kFenn
unicode_unihan_variant(0x96DE, kSimplifiedVariant, 0x9E21).
unicode_unihan_variant(0x96DE, kZVariant, 0x9DC4).
unicode_unihan_variant(0x96E0, kTraditionalVariant, 0x8B8E).
unicode_unihan_variant(0x96E0, kZVariant, 0x4EC7).
unicode_unihan_variant(0x96E2, kSimplifiedVariant, 0x79BB).
unicode_unihan_variant(0x96E2, kSpecializedSemanticVariant, 0x79BB). %<kFenn
unicode_unihan_variant(0x96E2, kZVariant, 0xF9EA).
unicode_unihan_variant(0x96E3, kSemanticVariant, 0x96BE). %<kLau,kMatthews
unicode_unihan_variant(0x96E3, kSimplifiedVariant, 0x96BE).
unicode_unihan_variant(0x96E3, kSpecializedSemanticVariant, 0x96BE). %<kFenn
unicode_unihan_variant(0x96E8, kSpecializedSemanticVariant, 0x99AD). %<kFenn
unicode_unihan_variant(0x96F1, kSemanticVariant, 0x9736). %<kMeyerWempe
unicode_unihan_variant(0x96F2, kSimplifiedVariant, 0x4E91).
unicode_unihan_variant(0x96F3, kTraditionalVariant, 0x9742).
unicode_unihan_variant(0x96F6, kZVariant, 0xF9B2).
unicode_unihan_variant(0x96F7, kZVariant, 0xF949).
unicode_unihan_variant(0x96FB, kSimplifiedVariant, 0x7535).
unicode_unihan_variant(0x96FE, kTraditionalVariant, 0x9727).
unicode_unihan_variant(0x9701, kTraditionalVariant, 0x973D).
unicode_unihan_variant(0x970A, kZVariant, 0x9748).
unicode_unihan_variant(0x970D, kZVariant, 0x7668).
unicode_unihan_variant(0x971C, kSemanticVariant, 0x5B40). %<kLau
unicode_unihan_variant(0x9721, kSemanticVariant, 0x9722). %<kMatthews
unicode_unihan_variant(0x9721, kTraditionalVariant, 0x9722).
unicode_unihan_variant(0x9722, kSemanticVariant, 0x9721). %<kMatthews
unicode_unihan_variant(0x9722, kSimplifiedVariant, 0x9721).
unicode_unihan_variant(0x9725, kSemanticVariant, 0x6FDB). %<kMeyerWempe
unicode_unihan_variant(0x9727, kSimplifiedVariant, 0x96FE).
unicode_unihan_variant(0x972D, kTraditionalVariant, 0x9744).
unicode_unihan_variant(0x9730, kSemanticVariant, 0x4A18). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9732, kZVariant, 0xF938).
unicode_unihan_variant(0x9736, kSemanticVariant, 0x96F1). %<kMeyerWempe
unicode_unihan_variant(0x9738, kSemanticVariant, 0x8987). %<kMeyerWempe
unicode_unihan_variant(0x9738, kZVariant, 0x8987).
unicode_unihan_variant(0x973D, kSimplifiedVariant, 0x9701).
unicode_unihan_variant(0x9740, kSemanticVariant, 0x6FDB). %<kMatthews
unicode_unihan_variant(0x9742, kSimplifiedVariant, 0x96F3).
unicode_unihan_variant(0x9744, kSimplifiedVariant, 0x972D).
unicode_unihan_variant(0x9746, kZVariant, 0x53C7).
unicode_unihan_variant(0x9748, kSemanticVariant, 0x7075). %<kFenn
unicode_unihan_variant(0x9748, kSimplifiedVariant, 0x7075).
unicode_unihan_variant(0x9748, kZVariant, 0xF9B3).
unicode_unihan_variant(0x9749, kZVariant, 0x53C6).
unicode_unihan_variant(0x9751, kZVariant, 0x9752).
unicode_unihan_variant(0x9752, kZVariant, 0x9751).
unicode_unihan_variant(0x9753, kTraditionalVariant, 0x975A).
unicode_unihan_variant(0x9754, kZVariant, 0x975D).
unicode_unihan_variant(0x9756, kZVariant, 0xFA1C).
unicode_unihan_variant(0x9759, kTraditionalVariant, 0x975C).
unicode_unihan_variant(0x975A, kSimplifiedVariant, 0x9753).
unicode_unihan_variant(0x975A, kZVariant, 0x975C).
unicode_unihan_variant(0x975C, kSimplifiedVariant, 0x9759).
unicode_unihan_variant(0x975D, kSemanticVariant, 0x5929). %<kMatthews
unicode_unihan_variant(0x975D, kZVariant, 0x9754).
unicode_unihan_variant(0x9762, kTraditionalVariant, 0x9EB5).
unicode_unihan_variant(0x9765, kTraditionalVariant, 0x9768).
unicode_unihan_variant(0x9766, kSemanticVariant, 0x89A5). %<kMeyerWempe
unicode_unihan_variant(0x9766, kSimplifiedVariant, 0x817C).
unicode_unihan_variant(0x9767, kSemanticVariant, 0x982E). %<kMatthews
unicode_unihan_variant(0x9768, kSimplifiedVariant, 0x9765).
unicode_unihan_variant(0x976D, kSemanticVariant, 0x97CC). %). %<kHKGlyph,kMatthews).
unicode_unihan_variant(0x976D, kZVariant, 0x97CC).
unicode_unihan_variant(0x9771, kZVariant, 0x97CC).
unicode_unihan_variant(0x9774, kSemanticVariant, 0x97BE). %<kMatthews
unicode_unihan_variant(0x9780, kSemanticVariant, 0x9F17). %<kMatthews 0x9789<kMatthews
unicode_unihan_variant(0x9780, kSimplifiedVariant, 0x9F17).
unicode_unihan_variant(0x9789, kSemanticVariant, 0x9780). %<kMatthews 0x9F17<kMatthews,kMeyerWempe
unicode_unihan_variant(0x978B, kSemanticVariant, 0x97B5). %<kMatthews 0x292D8<kMeyerWempe
unicode_unihan_variant(0x978B, kSpecializedSemanticVariant, 0x292E7).
unicode_unihan_variant(0x978C, kZVariant, 0x978D).
unicode_unihan_variant(0x978D, kZVariant, 0x978C).
unicode_unihan_variant(0x978F, kSimplifiedVariant, 0x5DE9).
unicode_unihan_variant(0x9791, kTraditionalVariant, 0x97C3).
unicode_unihan_variant(0x9792, kTraditionalVariant, 0x97BD).
unicode_unihan_variant(0x9792, kZVariant, 0x6A47).
unicode_unihan_variant(0x9798, kSemanticVariant, 0x97D2). %<kFenn
unicode_unihan_variant(0x9799, kSemanticVariant, 0x7404). %<kMeyerWempe
unicode_unihan_variant(0x979D, kSimplifiedVariant, 0x7EF1).
unicode_unihan_variant(0x979F, kSemanticVariant, 0x97B9). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x97A6, kSemanticVariant, 0x97A7). %<kLau
unicode_unihan_variant(0x97A7, kSemanticVariant, 0x97A6). %<kLau
unicode_unihan_variant(0x97AB, kSemanticVariant, 0x8ACA). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x97AF, kTraditionalVariant, 0x97C9).
unicode_unihan_variant(0x97B2, kSemanticVariant, 0x97DD). %<kMatthews
unicode_unihan_variant(0x97B4, kSemanticVariant, 0x7D65). %<kMatthews
unicode_unihan_variant(0x97B5, kSemanticVariant, 0x978B). %<kMatthews
unicode_unihan_variant(0x97B9, kSemanticVariant, 0x979F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x97BD, kSemanticVariant, 0x6A47). %<kFenn
unicode_unihan_variant(0x97BD, kSimplifiedVariant, 0x9792).
unicode_unihan_variant(0x97BE, kSemanticVariant, 0x9774). %<kMatthews
unicode_unihan_variant(0x97C1, kSemanticVariant, 0x7E6E). %<kLau,kMatthews
unicode_unihan_variant(0x97C1, kSimplifiedVariant, 0x7F30).
unicode_unihan_variant(0x97C2, kSemanticVariant, 0x4A5E). %<kMatthews
unicode_unihan_variant(0x97C3, kSimplifiedVariant, 0x9791).
unicode_unihan_variant(0x97C8, kSemanticVariant, 0x896A). %<kMatthews 0x97E4<kMatthews
unicode_unihan_variant(0x97C8, kZVariant, 0x896A).
unicode_unihan_variant(0x97C9, kSimplifiedVariant, 0x97AF).
unicode_unihan_variant(0x97CB, kSimplifiedVariant, 0x97E6).
unicode_unihan_variant(0x97CC, kSemanticVariant, 0x8095). %<kLau 0x976D<kHKGlyph,kMatthews).
unicode_unihan_variant(0x97CC, kSimplifiedVariant, 0x97E7).
unicode_unihan_variant(0x97CC, kSpecializedSemanticVariant, 0x8095). %<kMeyerWempe
unicode_unihan_variant(0x97CD, kSimplifiedVariant, 0x97E8).
unicode_unihan_variant(0x97D2, kSemanticVariant, 0x9798). %<kFenn
unicode_unihan_variant(0x97D3, kSimplifiedVariant, 0x97E9).
unicode_unihan_variant(0x97D9, kSimplifiedVariant, 0x97EA).
unicode_unihan_variant(0x97DC, kSemanticVariant, 0x5F22). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x97DC, kSimplifiedVariant, 0x97EC).
unicode_unihan_variant(0x97DD, kSemanticVariant, 0x97B2). %<kMatthews
unicode_unihan_variant(0x97DE, kSimplifiedVariant, 0x97EB).
unicode_unihan_variant(0x97DE, kSpecializedSemanticVariant, 0x7E15). %<kMeyerWempe
unicode_unihan_variant(0x97E4, kSemanticVariant, 0x896A). %<kMatthews 0x97C8<kMatthews
unicode_unihan_variant(0x97E4, kZVariant, 0x896A).
unicode_unihan_variant(0x97E6, kTraditionalVariant, 0x97CB).
unicode_unihan_variant(0x97E7, kTraditionalVariant, 0x97CC).
unicode_unihan_variant(0x97E8, kTraditionalVariant, 0x97CD).
unicode_unihan_variant(0x97E9, kTraditionalVariant, 0x97D3).
unicode_unihan_variant(0x97EA, kTraditionalVariant, 0x97D9).
unicode_unihan_variant(0x97EB, kTraditionalVariant, 0x97DE).
unicode_unihan_variant(0x97EC, kTraditionalVariant, 0x97DC).
unicode_unihan_variant(0x97ED, kSemanticVariant, 0x97EE). %<kLau,kMatthews
unicode_unihan_variant(0x97ED, kZVariant, 0x827D).
unicode_unihan_variant(0x97EE, kSemanticVariant, 0x97ED). %<kLau,kMatthews
unicode_unihan_variant(0x97EE, kZVariant, 0x97ED).
unicode_unihan_variant(0x97F2, kZVariant, 0x9F4F).
unicode_unihan_variant(0x97F5, kSemanticVariant, 0x97FB). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x97F5, kTraditionalVariant, 0x97FB).
unicode_unihan_variant(0x97FB, kSemanticVariant, 0x97F5). %<kHKGlyph,kLau,kMatthews,kMeyerWempe).
unicode_unihan_variant(0x97FB, kSimplifiedVariant, 0x97F5).
unicode_unihan_variant(0x97FF, kSemanticVariant, 0x54CD). %<kMatthews
unicode_unihan_variant(0x97FF, kSimplifiedVariant, 0x54CD).
unicode_unihan_variant(0x9801, kSimplifiedVariant, 0x9875).
unicode_unihan_variant(0x9802, kSemanticVariant, 0x29811). %<kFenn
unicode_unihan_variant(0x9802, kSimplifiedVariant, 0x9876).
unicode_unihan_variant(0x9803, kSimplifiedVariant, 0x9877).
unicode_unihan_variant(0x9805, kSimplifiedVariant, 0x9879).
unicode_unihan_variant(0x9806, kSimplifiedVariant, 0x987A).
unicode_unihan_variant(0x9807, kSimplifiedVariant, 0x9878).
unicode_unihan_variant(0x9808, kSemanticVariant, 0x6E4F). %<kMatthews
unicode_unihan_variant(0x9808, kSimplifiedVariant, 0x987B).
unicode_unihan_variant(0x980A, kSimplifiedVariant, 0x987C).
unicode_unihan_variant(0x980B, kSemanticVariant, 0x9867). %<kLau,kMatthews
unicode_unihan_variant(0x980B, kZVariant, 0x9867).
unicode_unihan_variant(0x980C, kSimplifiedVariant, 0x9882).
unicode_unihan_variant(0x980E, kSimplifiedVariant, 0x9880).
unicode_unihan_variant(0x980F, kSimplifiedVariant, 0x9883).
unicode_unihan_variant(0x9810, kSemanticVariant, 0x8C6B). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9810, kSimplifiedVariant, 0x9884).
unicode_unihan_variant(0x9811, kSimplifiedVariant, 0x987D).
unicode_unihan_variant(0x9812, kSimplifiedVariant, 0x9881).
unicode_unihan_variant(0x9812, kZVariant, 0x670C).
unicode_unihan_variant(0x9813, kSimplifiedVariant, 0x987F).
unicode_unihan_variant(0x9817, kSimplifiedVariant, 0x9887).
unicode_unihan_variant(0x9818, kSimplifiedVariant, 0x9886).
unicode_unihan_variant(0x9818, kZVariant, 0xF9B4).
unicode_unihan_variant(0x981A, kZVariant, 0x9838).
unicode_unihan_variant(0x981C, kSimplifiedVariant, 0x988C).
unicode_unihan_variant(0x981F, kSemanticVariant, 0x984D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9821, kSimplifiedVariant, 0x9889).
unicode_unihan_variant(0x9823, kSemanticVariant, 0x9824). %<kMatthews
unicode_unihan_variant(0x9824, kSemanticVariant, 0x9823). %<kMatthews
unicode_unihan_variant(0x9824, kSimplifiedVariant, 0x9890).
unicode_unihan_variant(0x9824, kZVariant, 0x9809).
unicode_unihan_variant(0x9826, kSimplifiedVariant, 0x988F).
unicode_unihan_variant(0x982B, kSemanticVariant, 0x4FEF). %<kMatthews
unicode_unihan_variant(0x982C, kZVariant, 0x9830).
unicode_unihan_variant(0x982D, kSimplifiedVariant, 0x5934).
unicode_unihan_variant(0x982E, kSemanticVariant, 0x9767). %<kMatthews
unicode_unihan_variant(0x982E, kSimplifiedVariant, 0x9892).
unicode_unihan_variant(0x9830, kSimplifiedVariant, 0x988A).
unicode_unihan_variant(0x9832, kSimplifiedVariant, 0x988B).
unicode_unihan_variant(0x9834, kSemanticVariant, 0x7A4E). %<kMatthews
unicode_unihan_variant(0x9834, kSimplifiedVariant, 0x9895).
unicode_unihan_variant(0x9837, kSimplifiedVariant, 0x9894).
unicode_unihan_variant(0x9838, kSimplifiedVariant, 0x9888).
unicode_unihan_variant(0x9839, kSimplifiedVariant, 0x9893).
unicode_unihan_variant(0x9839, kZVariant, 0x983D).
unicode_unihan_variant(0x983B, kSimplifiedVariant, 0x9891).
unicode_unihan_variant(0x983C, kSemanticVariant, 0x8CF4). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x983C, kZVariant, 0x8CF4).
unicode_unihan_variant(0x983D, kZVariant, 0x9839).
unicode_unihan_variant(0x9843, kSimplifiedVariant, 0x29596).
unicode_unihan_variant(0x9846, kSimplifiedVariant, 0x9897).
unicode_unihan_variant(0x984B, kSemanticVariant, 0x816E). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x984C, kSimplifiedVariant, 0x9898).
unicode_unihan_variant(0x984D, kSemanticVariant, 0x981F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x984D, kSimplifiedVariant, 0x989D).
unicode_unihan_variant(0x984E, kSimplifiedVariant, 0x989A).
unicode_unihan_variant(0x984F, kSimplifiedVariant, 0x989C).
unicode_unihan_variant(0x9852, kSimplifiedVariant, 0x9899).
unicode_unihan_variant(0x9853, kSimplifiedVariant, 0x989B).
unicode_unihan_variant(0x9854, kZVariant, 0x984F).
unicode_unihan_variant(0x9855, kSemanticVariant, 0x986F). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9855, kZVariant, 0x986F).
unicode_unihan_variant(0x9856, kSemanticVariant, 0x56DF). %<kMatthews
unicode_unihan_variant(0x9856, kSpecializedSemanticVariant, 0x56DF). %<kFenn
unicode_unihan_variant(0x9858, kSemanticVariant, 0x613F). %<kFenn
unicode_unihan_variant(0x9858, kSimplifiedVariant, 0x613F). %0x2B5B8
unicode_unihan_variant(0x9859, kSimplifiedVariant, 0x98A1).
unicode_unihan_variant(0x985A, kZVariant, 0x985B).
unicode_unihan_variant(0x985B, kSimplifiedVariant, 0x98A0).
unicode_unihan_variant(0x985E, kSemanticVariant, 0x29517). %<kLau,kMeyerWempe
unicode_unihan_variant(0x985E, kSimplifiedVariant, 0x7C7B).
unicode_unihan_variant(0x985E, kZVariant, 0xF9D0).
unicode_unihan_variant(0x9862, kSimplifiedVariant, 0x989F).
unicode_unihan_variant(0x9865, kSemanticVariant, 0x7693). %<kMatthews 0x769D<kMatthews
unicode_unihan_variant(0x9865, kSimplifiedVariant, 0x98A2).
unicode_unihan_variant(0x9866, kSemanticVariant, 0x6194). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9867, kSemanticVariant, 0x980B). %<kLau,kMatthews
unicode_unihan_variant(0x9867, kSimplifiedVariant, 0x987E).
unicode_unihan_variant(0x986B, kSimplifiedVariant, 0x98A4).
unicode_unihan_variant(0x986C, kSimplifiedVariant, 0x98A5).
unicode_unihan_variant(0x986F, kSemanticVariant, 0x9855). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x986F, kSimplifiedVariant, 0x663E).
unicode_unihan_variant(0x986F, kZVariant, 0x9855).
unicode_unihan_variant(0x9870, kSemanticVariant, 0x56AC). %<kMatthews
unicode_unihan_variant(0x9870, kSimplifiedVariant, 0x98A6).
unicode_unihan_variant(0x9871, kSemanticVariant, 0x9AD7). %<kMatthews
unicode_unihan_variant(0x9871, kSimplifiedVariant, 0x9885).
unicode_unihan_variant(0x9873, kSimplifiedVariant, 0x989E).
unicode_unihan_variant(0x9874, kSimplifiedVariant, 0x98A7).
unicode_unihan_variant(0x9875, kTraditionalVariant, 0x9801).
unicode_unihan_variant(0x9876, kTraditionalVariant, 0x9802).
unicode_unihan_variant(0x9877, kTraditionalVariant, 0x9803).
unicode_unihan_variant(0x9878, kTraditionalVariant, 0x9807).
unicode_unihan_variant(0x9879, kTraditionalVariant, 0x9805).
unicode_unihan_variant(0x987A, kTraditionalVariant, 0x9806).
unicode_unihan_variant(0x987B, kTraditionalVariant, 0x9808). %0x9B1A
unicode_unihan_variant(0x987B, kZVariant, 0x9808).
unicode_unihan_variant(0x987C, kTraditionalVariant, 0x980A).
unicode_unihan_variant(0x987D, kTraditionalVariant, 0x9811).
unicode_unihan_variant(0x987E, kTraditionalVariant, 0x9867).
unicode_unihan_variant(0x987F, kTraditionalVariant, 0x9813).
unicode_unihan_variant(0x9880, kTraditionalVariant, 0x980E).
unicode_unihan_variant(0x9881, kTraditionalVariant, 0x9812).
unicode_unihan_variant(0x9882, kTraditionalVariant, 0x980C).
unicode_unihan_variant(0x9883, kTraditionalVariant, 0x980F).
unicode_unihan_variant(0x9884, kTraditionalVariant, 0x9810).
unicode_unihan_variant(0x9885, kTraditionalVariant, 0x9871).
unicode_unihan_variant(0x9886, kTraditionalVariant, 0x9818).
unicode_unihan_variant(0x9887, kTraditionalVariant, 0x9817).
unicode_unihan_variant(0x9888, kTraditionalVariant, 0x9838).
unicode_unihan_variant(0x9889, kTraditionalVariant, 0x9821).
unicode_unihan_variant(0x988A, kTraditionalVariant, 0x9830).
unicode_unihan_variant(0x988B, kTraditionalVariant, 0x9832).
unicode_unihan_variant(0x988C, kTraditionalVariant, 0x981C).
unicode_unihan_variant(0x988D, kTraditionalVariant, 0x6F41).
unicode_unihan_variant(0x988E, kTraditionalVariant, 0x71B2).
unicode_unihan_variant(0x988F, kTraditionalVariant, 0x9826).
unicode_unihan_variant(0x9890, kTraditionalVariant, 0x9824).
unicode_unihan_variant(0x9891, kTraditionalVariant, 0x983B).
unicode_unihan_variant(0x9892, kTraditionalVariant, 0x982E).
unicode_unihan_variant(0x9893, kTraditionalVariant, 0x9839).
unicode_unihan_variant(0x9893, kZVariant, 0x9839).
unicode_unihan_variant(0x9894, kTraditionalVariant, 0x9837).
unicode_unihan_variant(0x9895, kTraditionalVariant, 0x9834).
unicode_unihan_variant(0x9896, kTraditionalVariant, 0x7A4E).
unicode_unihan_variant(0x9897, kTraditionalVariant, 0x9846).
unicode_unihan_variant(0x9898, kTraditionalVariant, 0x984C).
unicode_unihan_variant(0x9899, kTraditionalVariant, 0x9852).
unicode_unihan_variant(0x989A, kTraditionalVariant, 0x984E).
unicode_unihan_variant(0x989B, kTraditionalVariant, 0x9853).
unicode_unihan_variant(0x989C, kTraditionalVariant, 0x984F).
unicode_unihan_variant(0x989C, kZVariant, 0x984F).
unicode_unihan_variant(0x989D, kTraditionalVariant, 0x984D).
unicode_unihan_variant(0x989E, kTraditionalVariant, 0x9873).
unicode_unihan_variant(0x989F, kTraditionalVariant, 0x9862).
unicode_unihan_variant(0x98A0, kTraditionalVariant, 0x985B).
unicode_unihan_variant(0x98A1, kTraditionalVariant, 0x9859).
unicode_unihan_variant(0x98A2, kTraditionalVariant, 0x9865).
unicode_unihan_variant(0x98A3, kTraditionalVariant, 0x7E87).
unicode_unihan_variant(0x98A3, kZVariant, 0x7E87).
unicode_unihan_variant(0x98A4, kTraditionalVariant, 0x986B).
unicode_unihan_variant(0x98A5, kTraditionalVariant, 0x986C).
unicode_unihan_variant(0x98A6, kTraditionalVariant, 0x9870).
unicode_unihan_variant(0x98A7, kTraditionalVariant, 0x9874).
unicode_unihan_variant(0x98A8, kSimplifiedVariant, 0x98CE).
unicode_unihan_variant(0x98AD, kSimplifiedVariant, 0x98D0).
unicode_unihan_variant(0x98AE, kSemanticVariant, 0x730B). %<kMatthews 0x98C7<kMatthews,kMeyerWempe
unicode_unihan_variant(0x98AE, kSimplifiedVariant, 0x98D1).
unicode_unihan_variant(0x98AF, kSimplifiedVariant, 0x98D2).
unicode_unihan_variant(0x98B0, kSimplifiedVariant, 0x29665).
unicode_unihan_variant(0x98B1, kSimplifiedVariant, 0x53F0).
unicode_unihan_variant(0x98B3, kSimplifiedVariant, 0x522E).
unicode_unihan_variant(0x98B6, kSimplifiedVariant, 0x98D3).
unicode_unihan_variant(0x98B7, kSimplifiedVariant, 0x2966A).
unicode_unihan_variant(0x98B8, kSimplifiedVariant, 0x98D4).
unicode_unihan_variant(0x98BA, kSimplifiedVariant, 0x98CF).
unicode_unihan_variant(0x98BB, kSimplifiedVariant, 0x98D6).
unicode_unihan_variant(0x98BC, kSemanticVariant, 0x4B12). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x98BC, kSimplifiedVariant, 0x98D5).
unicode_unihan_variant(0x98BE, kSimplifiedVariant, 0x2966B).
unicode_unihan_variant(0x98C0, kSimplifiedVariant, 0x98D7).
unicode_unihan_variant(0x98C4, kSimplifiedVariant, 0x98D8).
unicode_unihan_variant(0x98C4, kZVariant, 0x98C3).
unicode_unihan_variant(0x98C6, kSimplifiedVariant, 0x98D9).
unicode_unihan_variant(0x98C7, kSemanticVariant, 0x730B). %<kMatthews 0x98AE<kMatthews,kMeyerWempe
unicode_unihan_variant(0x98C8, kSimplifiedVariant, 0x98DA).
unicode_unihan_variant(0x98CE, kTraditionalVariant, 0x98A8).
unicode_unihan_variant(0x98CF, kTraditionalVariant, 0x98BA).
unicode_unihan_variant(0x98D0, kTraditionalVariant, 0x98AD).
unicode_unihan_variant(0x98D1, kTraditionalVariant, 0x98AE).
unicode_unihan_variant(0x98D2, kTraditionalVariant, 0x98AF).
unicode_unihan_variant(0x98D3, kTraditionalVariant, 0x98B6).
unicode_unihan_variant(0x98D4, kTraditionalVariant, 0x98B8).
unicode_unihan_variant(0x98D5, kTraditionalVariant, 0x98BC).
unicode_unihan_variant(0x98D6, kTraditionalVariant, 0x98BB).
unicode_unihan_variant(0x98D7, kTraditionalVariant, 0x98C0).
unicode_unihan_variant(0x98D8, kTraditionalVariant, 0x98C4).
unicode_unihan_variant(0x98D9, kTraditionalVariant, 0x98C6).
unicode_unihan_variant(0x98DA, kTraditionalVariant, 0x98C8).
unicode_unihan_variant(0x98DB, kSimplifiedVariant, 0x98DE).
unicode_unihan_variant(0x98DC, kZVariant, 0x7FFB).
unicode_unihan_variant(0x98DE, kTraditionalVariant, 0x98DB).
unicode_unihan_variant(0x98DF, kSemanticVariant, 0x98E0). %<kMatthews
unicode_unihan_variant(0x98DF, kZVariant, 0x9963).
unicode_unihan_variant(0x98E0, kSemanticVariant, 0x98DF). %<kMatthews
unicode_unihan_variant(0x98E0, kSimplifiedVariant, 0x9963).
unicode_unihan_variant(0x98E0, kZVariant, 0x98DF).
unicode_unihan_variant(0x98E1, kSemanticVariant, 0x9910). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x98E2, kSemanticVariant, 0x9951). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x98E2, kSimplifiedVariant, 0x9965).
unicode_unihan_variant(0x98E3, kSimplifiedVariant, 0x9964).
unicode_unihan_variant(0x98E4, kSemanticVariant, 0x98FC). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x98E5, kSimplifiedVariant, 0x9966).
unicode_unihan_variant(0x98E6, kSemanticVariant, 0x9958). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x98E8, kTraditionalVariant, 0x9957).
unicode_unihan_variant(0x98E9, kSimplifiedVariant, 0x9968).
unicode_unihan_variant(0x98EA, kSemanticVariant, 0x43D5). %<kMatthews
unicode_unihan_variant(0x98EA, kSimplifiedVariant, 0x996A).
unicode_unihan_variant(0x98EA, kZVariant, 0x9901).
unicode_unihan_variant(0x98EB, kSemanticVariant, 0x9947). %<kMatthews
unicode_unihan_variant(0x98EB, kSimplifiedVariant, 0x996B).
unicode_unihan_variant(0x98ED, kSimplifiedVariant, 0x996C).
unicode_unihan_variant(0x98EE, kZVariant, 0x98F2).
unicode_unihan_variant(0x98EF, kSimplifiedVariant, 0x996D).
unicode_unihan_variant(0x98F0, kZVariant, 0x98EF).
unicode_unihan_variant(0x98F2, kSimplifiedVariant, 0x996E).
unicode_unihan_variant(0x98F2, kZVariant, 0x98EE).
unicode_unihan_variant(0x98F4, kSimplifiedVariant, 0x9974).
unicode_unihan_variant(0x98FC, kSemanticVariant, 0x98E4). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x98FC, kSimplifiedVariant, 0x9972).
unicode_unihan_variant(0x98FC, kZVariant, 0xFA2B).
unicode_unihan_variant(0x98FD, kSimplifiedVariant, 0x9971).
unicode_unihan_variant(0x98FE, kSemanticVariant, 0x9919). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x98FE, kSimplifiedVariant, 0x9970).
unicode_unihan_variant(0x98FF, kSimplifiedVariant, 0x9973).
unicode_unihan_variant(0x9901, kZVariant, 0x98EA).
unicode_unihan_variant(0x9902, kSemanticVariant, 0x8214). %<kFenn
unicode_unihan_variant(0x9903, kSimplifiedVariant, 0x997A).
unicode_unihan_variant(0x9904, kSimplifiedVariant, 0x9978).
unicode_unihan_variant(0x9905, kSemanticVariant, 0x9920). %<kMatthews
unicode_unihan_variant(0x9905, kSimplifiedVariant, 0x997C).
unicode_unihan_variant(0x9909, kSemanticVariant, 0x995F). %<kMeyerWempe
unicode_unihan_variant(0x9909, kSimplifiedVariant, 0x9977).
unicode_unihan_variant(0x990A, kSimplifiedVariant, 0x517B).
unicode_unihan_variant(0x990C, kSimplifiedVariant, 0x9975).
unicode_unihan_variant(0x990D, kTraditionalVariant, 0x995C).
unicode_unihan_variant(0x990E, kSimplifiedVariant, 0x9979).
unicode_unihan_variant(0x990F, kSimplifiedVariant, 0x997B).
unicode_unihan_variant(0x9910, kSemanticVariant, 0x98E1). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9911, kSimplifiedVariant, 0x997D).
unicode_unihan_variant(0x9912, kSemanticVariant, 0x9927). %<kMatthews
unicode_unihan_variant(0x9912, kSimplifiedVariant, 0x9981).
unicode_unihan_variant(0x9912, kSpecializedSemanticVariant, 0x9927). %<kMeyerWempe
unicode_unihan_variant(0x9913, kSimplifiedVariant, 0x997F).
unicode_unihan_variant(0x9914, kSimplifiedVariant, 0x2B5E6).
unicode_unihan_variant(0x9915, kSimplifiedVariant, 0x9982).
unicode_unihan_variant(0x9916, kSimplifiedVariant, 0x997E).
unicode_unihan_variant(0x9917, kSimplifiedVariant, 0x2B5E7).
unicode_unihan_variant(0x9918, kSimplifiedVariant, 0x4F59). %0x9980
unicode_unihan_variant(0x9919, kSemanticVariant, 0x98FE). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x991A, kSemanticVariant, 0x80B4). %<kLau
unicode_unihan_variant(0x991A, kSimplifiedVariant, 0x80B4).
unicode_unihan_variant(0x991B, kSemanticVariant, 0x992B). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x991B, kSimplifiedVariant, 0x9984).
unicode_unihan_variant(0x991C, kSemanticVariant, 0x4D39). %<kMatthews
unicode_unihan_variant(0x991C, kSimplifiedVariant, 0x9983).
unicode_unihan_variant(0x991D, kZVariant, 0x98FE).
unicode_unihan_variant(0x991E, kSimplifiedVariant, 0x996F).
unicode_unihan_variant(0x991F, kSemanticVariant, 0x918A). %<kMatthews
unicode_unihan_variant(0x9920, kSemanticVariant, 0x9905). %<kMatthews
unicode_unihan_variant(0x9920, kZVariant, 0x9905).
unicode_unihan_variant(0x9921, kSimplifiedVariant, 0x9985).
unicode_unihan_variant(0x9926, kSimplifiedVariant, 0x2B5E0).
unicode_unihan_variant(0x9927, kSemanticVariant, 0x5582). %<kFenn 0x9912<kMatthews
unicode_unihan_variant(0x9927, kSpecializedSemanticVariant, 0x5582). %<kMeyerWempe
unicode_unihan_variant(0x9928, kSemanticVariant, 0x8218). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9928, kSimplifiedVariant, 0x9986).
unicode_unihan_variant(0x9928, kZVariant, 0xFA2C).
unicode_unihan_variant(0x992B, kSemanticVariant, 0x991B). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x992D, kSimplifiedVariant, 0x2B5EE).
unicode_unihan_variant(0x9931, kSemanticVariant, 0x7CC7). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9931, kSimplifiedVariant, 0x7CC7).
unicode_unihan_variant(0x9933, kSimplifiedVariant, 0x9967).
unicode_unihan_variant(0x9935, kSemanticVariant, 0x8AC9). %<kLau
unicode_unihan_variant(0x9935, kSpecializedSemanticVariant, 0x5582). %<kMeyerWempe
unicode_unihan_variant(0x9936, kSimplifiedVariant, 0x9989).
unicode_unihan_variant(0x9937, kSimplifiedVariant, 0x9987).
unicode_unihan_variant(0x9938, kSimplifiedVariant, 0x2980C).
unicode_unihan_variant(0x9939, kSemanticVariant, 0x7CD6). %<kMatthews
unicode_unihan_variant(0x9939, kZVariant, 0x7CD6).
unicode_unihan_variant(0x993A, kSimplifiedVariant, 0x998E).
unicode_unihan_variant(0x993B, kSemanticVariant, 0x7CD5). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x993C, kSimplifiedVariant, 0x9969).
unicode_unihan_variant(0x993D, kSemanticVariant, 0x994B). %<kFenn
unicode_unihan_variant(0x993D, kZVariant, 0x994B).
unicode_unihan_variant(0x993E, kSimplifiedVariant, 0x998F).
unicode_unihan_variant(0x993F, kSimplifiedVariant, 0x998A).
unicode_unihan_variant(0x9941, kSimplifiedVariant, 0x998C).
unicode_unihan_variant(0x9943, kSimplifiedVariant, 0x998D).
unicode_unihan_variant(0x9945, kSimplifiedVariant, 0x9992).
unicode_unihan_variant(0x9947, kSemanticVariant, 0x98EB). %<kMatthews
unicode_unihan_variant(0x9948, kSemanticVariant, 0x267E4). %<kFenn
unicode_unihan_variant(0x9948, kSimplifiedVariant, 0x9990).
unicode_unihan_variant(0x9949, kSimplifiedVariant, 0x9991).
unicode_unihan_variant(0x994A, kSemanticVariant, 0x7CE4). %<kMeyerWempe
unicode_unihan_variant(0x994A, kSimplifiedVariant, 0x9993).
unicode_unihan_variant(0x994B, kSemanticVariant, 0x5331). %<kLau 0x6AC3<kLau 0x993D<kFenn
unicode_unihan_variant(0x994B, kSimplifiedVariant, 0x9988).
unicode_unihan_variant(0x994B, kZVariant, 0x993D).
unicode_unihan_variant(0x994C, kSemanticVariant, 0x4275). %<kMatthews 0x7C51<kMatthews
unicode_unihan_variant(0x994C, kSimplifiedVariant, 0x9994).
unicode_unihan_variant(0x994D, kSemanticVariant, 0x81B3). %<kLau,kMatthews
unicode_unihan_variant(0x9951, kSemanticVariant, 0x98E2). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9952, kSimplifiedVariant, 0x9976).
unicode_unihan_variant(0x9955, kSemanticVariant, 0x53E8). %<kMeyerWempe
unicode_unihan_variant(0x9957, kSimplifiedVariant, 0x98E8).
unicode_unihan_variant(0x9958, kSemanticVariant, 0x98E6). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9958, kSimplifiedVariant, 0x2B5F4).
unicode_unihan_variant(0x995C, kSimplifiedVariant, 0x990D).
unicode_unihan_variant(0x995E, kSemanticVariant, 0x56B5). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x995E, kSimplifiedVariant, 0x998B).
unicode_unihan_variant(0x995F, kSemanticVariant, 0x9909). %<kMeyerWempe
unicode_unihan_variant(0x9962, kSimplifiedVariant, 0x9995).
unicode_unihan_variant(0x9963, kTraditionalVariant, 0x98E0).
unicode_unihan_variant(0x9964, kTraditionalVariant, 0x98E3).
unicode_unihan_variant(0x9965, kTraditionalVariant, 0x98E2).
unicode_unihan_variant(0x9965, kZVariant, 0x98E2).
unicode_unihan_variant(0x9966, kTraditionalVariant, 0x98E5).
unicode_unihan_variant(0x9967, kTraditionalVariant, 0x9933).
unicode_unihan_variant(0x9968, kTraditionalVariant, 0x98E9).
unicode_unihan_variant(0x9969, kTraditionalVariant, 0x993C).
unicode_unihan_variant(0x996A, kTraditionalVariant, 0x98EA).
unicode_unihan_variant(0x996B, kTraditionalVariant, 0x98EB).
unicode_unihan_variant(0x996C, kTraditionalVariant, 0x98ED).
unicode_unihan_variant(0x996D, kTraditionalVariant, 0x98EF).
unicode_unihan_variant(0x996E, kTraditionalVariant, 0x98F2).
unicode_unihan_variant(0x996E, kZVariant, 0x98EE).
unicode_unihan_variant(0x996F, kTraditionalVariant, 0x991E).
unicode_unihan_variant(0x9970, kTraditionalVariant, 0x98FE).
unicode_unihan_variant(0x9971, kTraditionalVariant, 0x98FD).
unicode_unihan_variant(0x9972, kTraditionalVariant, 0x98FC).
unicode_unihan_variant(0x9973, kTraditionalVariant, 0x98FF).
unicode_unihan_variant(0x9974, kTraditionalVariant, 0x98F4).
unicode_unihan_variant(0x9975, kTraditionalVariant, 0x990C).
unicode_unihan_variant(0x9976, kTraditionalVariant, 0x9952).
unicode_unihan_variant(0x9977, kTraditionalVariant, 0x9909).
unicode_unihan_variant(0x9978, kTraditionalVariant, 0x9904).
unicode_unihan_variant(0x9979, kTraditionalVariant, 0x990E).
unicode_unihan_variant(0x997A, kTraditionalVariant, 0x9903).
unicode_unihan_variant(0x997B, kTraditionalVariant, 0x990F).
unicode_unihan_variant(0x997C, kTraditionalVariant, 0x9905).
unicode_unihan_variant(0x997D, kTraditionalVariant, 0x9911).
unicode_unihan_variant(0x997E, kTraditionalVariant, 0x9916).
unicode_unihan_variant(0x997F, kTraditionalVariant, 0x9913).
unicode_unihan_variant(0x9980, kTraditionalVariant, 0x9918).
unicode_unihan_variant(0x9981, kTraditionalVariant, 0x9912).
unicode_unihan_variant(0x9982, kTraditionalVariant, 0x9915).
unicode_unihan_variant(0x9983, kTraditionalVariant, 0x991C).
unicode_unihan_variant(0x9984, kTraditionalVariant, 0x991B).
unicode_unihan_variant(0x9985, kTraditionalVariant, 0x9921).
unicode_unihan_variant(0x9986, kTraditionalVariant, 0x9928).
unicode_unihan_variant(0x9987, kTraditionalVariant, 0x9937).
unicode_unihan_variant(0x9988, kTraditionalVariant, 0x994B).
unicode_unihan_variant(0x9988, kZVariant, 0x993D).
unicode_unihan_variant(0x9989, kTraditionalVariant, 0x9936).
unicode_unihan_variant(0x998A, kTraditionalVariant, 0x993F).
unicode_unihan_variant(0x998B, kTraditionalVariant, 0x995E).
unicode_unihan_variant(0x998C, kTraditionalVariant, 0x9941).
unicode_unihan_variant(0x998D, kTraditionalVariant, 0x9943).
unicode_unihan_variant(0x998E, kTraditionalVariant, 0x993A).
unicode_unihan_variant(0x998F, kTraditionalVariant, 0x993E).
unicode_unihan_variant(0x9990, kTraditionalVariant, 0x9948).
unicode_unihan_variant(0x9991, kTraditionalVariant, 0x9949).
unicode_unihan_variant(0x9992, kTraditionalVariant, 0x9945).
unicode_unihan_variant(0x9993, kTraditionalVariant, 0x994A).
unicode_unihan_variant(0x9993, kZVariant, 0x7CE4).
unicode_unihan_variant(0x9994, kTraditionalVariant, 0x994C).
unicode_unihan_variant(0x9995, kTraditionalVariant, 0x9962).
unicode_unihan_variant(0x9998, kSemanticVariant, 0x805D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x999A, kSemanticVariant, 0x99A9). %<kMatthews
unicode_unihan_variant(0x99A9, kSemanticVariant, 0x999A). %<kMatthews
unicode_unihan_variant(0x99AC, kSimplifiedVariant, 0x9A6C).
unicode_unihan_variant(0x99AD, kSemanticVariant, 0x5FA1). %<kMatthews
unicode_unihan_variant(0x99AD, kSimplifiedVariant, 0x9A6D).
unicode_unihan_variant(0x99AD, kSpecializedSemanticVariant, 0x5FA1). %<kMeyerWempe
unicode_unihan_variant(0x99AE, kSimplifiedVariant, 0x51AF).
unicode_unihan_variant(0x99B1, kSemanticVariant, 0x4B7E). %<kFenn
unicode_unihan_variant(0x99B1, kSimplifiedVariant, 0x9A6E).
unicode_unihan_variant(0x99B1, kZVariant, 0x99C4).
unicode_unihan_variant(0x99B3, kSimplifiedVariant, 0x9A70).
unicode_unihan_variant(0x99B4, kSimplifiedVariant, 0x9A6F).
unicode_unihan_variant(0x99B9, kSimplifiedVariant, 0x9A72).
unicode_unihan_variant(0x99BF, kSemanticVariant, 0x9A62). %<kMatthews
unicode_unihan_variant(0x99C1, kSemanticVariant, 0x99EE). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x99C1, kSimplifiedVariant, 0x9A73).
unicode_unihan_variant(0x99C1, kZVariant, 0x99EE).
unicode_unihan_variant(0x99C3, kSimplifiedVariant, 0x2B61D).
unicode_unihan_variant(0x99C4, kZVariant, 0x99B1).
unicode_unihan_variant(0x99C5, kZVariant, 0x9A5B).
unicode_unihan_variant(0x99C6, kZVariant, 0x9A45).
unicode_unihan_variant(0x99C8, kZVariant, 0x9A45).
unicode_unihan_variant(0x99CE, kSimplifiedVariant, 0x299E8).
unicode_unihan_variant(0x99D0, kSimplifiedVariant, 0x9A7B).
unicode_unihan_variant(0x99D1, kSimplifiedVariant, 0x9A7D).
unicode_unihan_variant(0x99D2, kSimplifiedVariant, 0x9A79).
unicode_unihan_variant(0x99D4, kSimplifiedVariant, 0x9A75).
unicode_unihan_variant(0x99D5, kSimplifiedVariant, 0x9A7E).
unicode_unihan_variant(0x99D8, kSimplifiedVariant, 0x9A80).
unicode_unihan_variant(0x99D9, kSimplifiedVariant, 0x9A78).
unicode_unihan_variant(0x99DA, kSimplifiedVariant, 0x299EB).
unicode_unihan_variant(0x99DB, kSemanticVariant, 0x298B2). %<kMeyerWempe
unicode_unihan_variant(0x99DB, kSimplifiedVariant, 0x9A76).
unicode_unihan_variant(0x99DD, kSimplifiedVariant, 0x9A7C).
unicode_unihan_variant(0x99DE, kZVariant, 0x99DD).
unicode_unihan_variant(0x99DF, kSimplifiedVariant, 0x9A77).
unicode_unihan_variant(0x99E1, kZVariant, 0x7F75).
unicode_unihan_variant(0x99E2, kSemanticVariant, 0x9A08). %<kMatthews
unicode_unihan_variant(0x99E2, kSimplifiedVariant, 0x9A88).
unicode_unihan_variant(0x99E2, kZVariant, 0x9A08).
unicode_unihan_variant(0x99E6, kSemanticVariant, 0x9A30). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x99E7, kSimplifiedVariant, 0x299F2).
unicode_unihan_variant(0x99E9, kSimplifiedVariant, 0x299F4).
unicode_unihan_variant(0x99EA, kSemanticVariant, 0x4F81). %<kMatthews,kMeyerWempe 0x8A75<kMeyerWempe
unicode_unihan_variant(0x99ED, kSimplifiedVariant, 0x9A87).
unicode_unihan_variant(0x99EE, kSemanticVariant, 0x99C1). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x99EE, kZVariant, 0x99C1).
unicode_unihan_variant(0x99F0, kSimplifiedVariant, 0x9A83).
unicode_unihan_variant(0x99F1, kSimplifiedVariant, 0x9A86).
unicode_unihan_variant(0x99F1, kZVariant, 0xF91A).
unicode_unihan_variant(0x99F6, kSimplifiedVariant, 0x299FA).
unicode_unihan_variant(0x99F8, kSimplifiedVariant, 0x9A8E).
unicode_unihan_variant(0x99FB, kSimplifiedVariant, 0x2B623).
unicode_unihan_variant(0x99FF, kSimplifiedVariant, 0x9A8F).
unicode_unihan_variant(0x9A01, kSimplifiedVariant, 0x9A8B).
unicode_unihan_variant(0x9A02, kSimplifiedVariant, 0x9A8D).
unicode_unihan_variant(0x9A03, kSimplifiedVariant, 0x2B624).
unicode_unihan_variant(0x9A03, kZVariant, 0x5446).
unicode_unihan_variant(0x9A05, kSimplifiedVariant, 0x9A93).
unicode_unihan_variant(0x9A08, kSemanticVariant, 0x99E2). %<kMatthews
unicode_unihan_variant(0x9A08, kZVariant, 0x99E2).
unicode_unihan_variant(0x9A0C, kSemanticVariant, 0x9A23). %<kMatthews
unicode_unihan_variant(0x9A0C, kSimplifiedVariant, 0x9A94).
unicode_unihan_variant(0x9A0C, kZVariant, 0x9B03).
unicode_unihan_variant(0x9A0D, kSimplifiedVariant, 0x9A92).
unicode_unihan_variant(0x9A0E, kSimplifiedVariant, 0x9A91).
unicode_unihan_variant(0x9A0F, kSimplifiedVariant, 0x9A90).
unicode_unihan_variant(0x9A10, kSemanticVariant, 0x9A57). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9A12, kZVariant, 0x9A37).
unicode_unihan_variant(0x9A13, kZVariant, 0x9A57).
unicode_unihan_variant(0x9A14, kSimplifiedVariant, 0x29A00).
unicode_unihan_variant(0x9A16, kSimplifiedVariant, 0x9A9B).
unicode_unihan_variant(0x9A17, kSemanticVariant, 0x9A19). %<kMatthews
unicode_unihan_variant(0x9A18, kSemanticVariant, 0x9A44). %<kMatthews
unicode_unihan_variant(0x9A19, kSemanticVariant, 0x9A17). %<kMatthews
unicode_unihan_variant(0x9A19, kSimplifiedVariant, 0x9A97).
unicode_unihan_variant(0x9A1A, kSimplifiedVariant, 0x29A0A).
unicode_unihan_variant(0x9A1D, kSimplifiedVariant, 0x29A03).
unicode_unihan_variant(0x9A1F, kSimplifiedVariant, 0x29A08).
unicode_unihan_variant(0x9A20, kSimplifiedVariant, 0x2B628).
unicode_unihan_variant(0x9A23, kSemanticVariant, 0x9A0C). %<kMatthews
unicode_unihan_variant(0x9A23, kZVariant, 0x9B03).
unicode_unihan_variant(0x9A24, kSimplifiedVariant, 0x9A99).
unicode_unihan_variant(0x9A27, kSimplifiedVariant, 0x4BC4).
unicode_unihan_variant(0x9A28, kZVariant, 0x9A52).
unicode_unihan_variant(0x9A2A, kSimplifiedVariant, 0x29A04).
unicode_unihan_variant(0x9A2B, kSimplifiedVariant, 0x9A9E).
unicode_unihan_variant(0x9A2D, kSemanticVariant, 0x96B2). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9A2D, kSimplifiedVariant, 0x9A98).
unicode_unihan_variant(0x9A2E, kSimplifiedVariant, 0x9A9D).
unicode_unihan_variant(0x9A30, kSemanticVariant, 0x99E6). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9A30, kSimplifiedVariant, 0x817E).
unicode_unihan_variant(0x9A36, kSimplifiedVariant, 0x9A7A).
unicode_unihan_variant(0x9A37, kSimplifiedVariant, 0x9A9A).
unicode_unihan_variant(0x9A38, kSimplifiedVariant, 0x9A9F).
unicode_unihan_variant(0x9A3A, kZVariant, 0x9A47).
unicode_unihan_variant(0x9A3E, kSemanticVariant, 0x9A58). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9A3E, kSimplifiedVariant, 0x9AA1).
unicode_unihan_variant(0x9A40, kSimplifiedVariant, 0x84E6).
unicode_unihan_variant(0x9A41, kSimplifiedVariant, 0x9A9C).
unicode_unihan_variant(0x9A42, kSimplifiedVariant, 0x9A96).
unicode_unihan_variant(0x9A43, kSimplifiedVariant, 0x9AA0).
unicode_unihan_variant(0x9A44, kSemanticVariant, 0x9A18). %<kMatthews
unicode_unihan_variant(0x9A44, kSimplifiedVariant, 0x9AA2).
unicode_unihan_variant(0x9A45, kSemanticVariant, 0x657A). %<kLau,kMatthews
unicode_unihan_variant(0x9A45, kSimplifiedVariant, 0x9A71).
unicode_unihan_variant(0x9A47, kZVariant, 0x9A3A).
unicode_unihan_variant(0x9A4A, kSemanticVariant, 0x26F4A). %<kMeyerWempe
unicode_unihan_variant(0x9A4A, kSimplifiedVariant, 0x9A85).
unicode_unihan_variant(0x9A4B, kSimplifiedVariant, 0x299EF).
unicode_unihan_variant(0x9A4C, kSimplifiedVariant, 0x9A95).
unicode_unihan_variant(0x9A4D, kSimplifiedVariant, 0x9A81).
unicode_unihan_variant(0x9A4F, kSimplifiedVariant, 0x9AA3).
unicode_unihan_variant(0x9A52, kZVariant, 0x9A28).
unicode_unihan_variant(0x9A55, kSimplifiedVariant, 0x9A84).
unicode_unihan_variant(0x9A57, kSemanticVariant, 0x9A10). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9A57, kSimplifiedVariant, 0x9A8C).
unicode_unihan_variant(0x9A58, kSemanticVariant, 0x9A3E). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9A5A, kSimplifiedVariant, 0x60CA).
unicode_unihan_variant(0x9A5B, kSimplifiedVariant, 0x9A7F).
unicode_unihan_variant(0x9A5F, kSimplifiedVariant, 0x9AA4).
unicode_unihan_variant(0x9A62, kSemanticVariant, 0x99BF). %<kMatthews
unicode_unihan_variant(0x9A62, kSimplifiedVariant, 0x9A74).
unicode_unihan_variant(0x9A64, kSimplifiedVariant, 0x9AA7).
unicode_unihan_variant(0x9A65, kSimplifiedVariant, 0x9AA5).
unicode_unihan_variant(0x9A66, kSimplifiedVariant, 0x9AA6).
unicode_unihan_variant(0x9A69, kZVariant, 0x6B61).
unicode_unihan_variant(0x9A6A, kSimplifiedVariant, 0x9A8A).
unicode_unihan_variant(0x9A6B, kSimplifiedVariant, 0x9A89).
unicode_unihan_variant(0x9A6C, kTraditionalVariant, 0x99AC).
unicode_unihan_variant(0x9A6D, kTraditionalVariant, 0x99AD).
unicode_unihan_variant(0x9A6E, kTraditionalVariant, 0x99B1).
unicode_unihan_variant(0x9A6F, kTraditionalVariant, 0x99B4).
unicode_unihan_variant(0x9A70, kTraditionalVariant, 0x99B3).
unicode_unihan_variant(0x9A71, kTraditionalVariant, 0x9A45).
unicode_unihan_variant(0x9A72, kTraditionalVariant, 0x99B9).
unicode_unihan_variant(0x9A73, kTraditionalVariant, 0x99C1).
unicode_unihan_variant(0x9A74, kTraditionalVariant, 0x9A62).
unicode_unihan_variant(0x9A75, kTraditionalVariant, 0x99D4).
unicode_unihan_variant(0x9A76, kTraditionalVariant, 0x99DB).
unicode_unihan_variant(0x9A77, kTraditionalVariant, 0x99DF).
unicode_unihan_variant(0x9A78, kTraditionalVariant, 0x99D9).
unicode_unihan_variant(0x9A79, kTraditionalVariant, 0x99D2).
unicode_unihan_variant(0x9A7A, kTraditionalVariant, 0x9A36).
unicode_unihan_variant(0x9A7B, kTraditionalVariant, 0x99D0).
unicode_unihan_variant(0x9A7C, kTraditionalVariant, 0x99DD).
unicode_unihan_variant(0x9A7D, kTraditionalVariant, 0x99D1).
unicode_unihan_variant(0x9A7E, kTraditionalVariant, 0x99D5).
unicode_unihan_variant(0x9A7F, kTraditionalVariant, 0x9A5B).
unicode_unihan_variant(0x9A80, kTraditionalVariant, 0x99D8).
unicode_unihan_variant(0x9A81, kTraditionalVariant, 0x9A4D).
unicode_unihan_variant(0x9A82, kTraditionalVariant, 0x7F75).
unicode_unihan_variant(0x9A82, kZVariant, 0x7F75).
unicode_unihan_variant(0x9A83, kTraditionalVariant, 0x99F0).
unicode_unihan_variant(0x9A84, kTraditionalVariant, 0x9A55).
unicode_unihan_variant(0x9A85, kTraditionalVariant, 0x9A4A).
unicode_unihan_variant(0x9A86, kTraditionalVariant, 0x99F1).
unicode_unihan_variant(0x9A87, kTraditionalVariant, 0x99ED).
unicode_unihan_variant(0x9A88, kTraditionalVariant, 0x99E2).
unicode_unihan_variant(0x9A89, kTraditionalVariant, 0x9A6B).
unicode_unihan_variant(0x9A8A, kTraditionalVariant, 0x9A6A).
unicode_unihan_variant(0x9A8B, kTraditionalVariant, 0x9A01).
unicode_unihan_variant(0x9A8C, kTraditionalVariant, 0x9A57).
unicode_unihan_variant(0x9A8D, kTraditionalVariant, 0x9A02).
unicode_unihan_variant(0x9A8E, kTraditionalVariant, 0x99F8).
unicode_unihan_variant(0x9A8F, kTraditionalVariant, 0x99FF).
unicode_unihan_variant(0x9A90, kTraditionalVariant, 0x9A0F).
unicode_unihan_variant(0x9A91, kTraditionalVariant, 0x9A0E).
unicode_unihan_variant(0x9A92, kTraditionalVariant, 0x9A0D).
unicode_unihan_variant(0x9A93, kTraditionalVariant, 0x9A05).
unicode_unihan_variant(0x9A94, kTraditionalVariant, 0x9A0C).
unicode_unihan_variant(0x9A94, kZVariant, 0x9B03).
unicode_unihan_variant(0x9A95, kTraditionalVariant, 0x9A4C).
unicode_unihan_variant(0x9A96, kTraditionalVariant, 0x9A42).
unicode_unihan_variant(0x9A97, kTraditionalVariant, 0x9A19).
unicode_unihan_variant(0x9A98, kTraditionalVariant, 0x9A2D).
unicode_unihan_variant(0x9A99, kTraditionalVariant, 0x9A24).
unicode_unihan_variant(0x9A9A, kTraditionalVariant, 0x9A37).
unicode_unihan_variant(0x9A9B, kTraditionalVariant, 0x9A16).
unicode_unihan_variant(0x9A9C, kTraditionalVariant, 0x9A41).
unicode_unihan_variant(0x9A9D, kTraditionalVariant, 0x9A2E).
unicode_unihan_variant(0x9A9E, kTraditionalVariant, 0x9A2B).
unicode_unihan_variant(0x9A9F, kTraditionalVariant, 0x9A38).
unicode_unihan_variant(0x9AA0, kTraditionalVariant, 0x9A43).
unicode_unihan_variant(0x9AA1, kTraditionalVariant, 0x9A3E).
unicode_unihan_variant(0x9AA2, kTraditionalVariant, 0x9A44).
unicode_unihan_variant(0x9AA3, kTraditionalVariant, 0x9A4F).
unicode_unihan_variant(0x9AA4, kTraditionalVariant, 0x9A5F).
unicode_unihan_variant(0x9AA5, kTraditionalVariant, 0x9A65).
unicode_unihan_variant(0x9AA6, kTraditionalVariant, 0x9A66).
unicode_unihan_variant(0x9AA7, kTraditionalVariant, 0x9A64).
unicode_unihan_variant(0x9AAB, kZVariant, 0x9AAA).
unicode_unihan_variant(0x9AAF, kSimplifiedVariant, 0x80AE).
unicode_unihan_variant(0x9AAF, kSpecializedSemanticVariant, 0x814C). %<kFenn
unicode_unihan_variant(0x9AB4, kSemanticVariant, 0x80D4). %<kMatthews 0x2667C<kMeyerWempe
unicode_unihan_variant(0x9AB5, kZVariant, 0x9AD4).
unicode_unihan_variant(0x9ABB, kSemanticVariant, 0x80EF). %<kMeyerWempe
unicode_unihan_variant(0x9ABD, kSemanticVariant, 0x817F). %<kMatthews
unicode_unihan_variant(0x9ABE, kSemanticVariant, 0x632D). %<kMatthews 0x9BC1<kMatthews
unicode_unihan_variant(0x9ABE, kSpecializedSemanticVariant, 0x9BC1). %<kMeyerWempe
unicode_unihan_variant(0x9AC3, kSemanticVariant, 0x8162). %<kMatthews
unicode_unihan_variant(0x9AC4, kZVariant, 0x9AD3).
unicode_unihan_variant(0x9AC5, kTraditionalVariant, 0x9ACF).
unicode_unihan_variant(0x9ACB, kTraditionalVariant, 0x9AD6).
unicode_unihan_variant(0x9ACC, kTraditionalVariant, 0x9AD5).
unicode_unihan_variant(0x9ACC, kZVariant, 0x81CF).
unicode_unihan_variant(0x9ACF, kSimplifiedVariant, 0x9AC5).
unicode_unihan_variant(0x9AD2, kSimplifiedVariant, 0x810F).
unicode_unihan_variant(0x9AD3, kZVariant, 0x81B8).
unicode_unihan_variant(0x9AD4, kSemanticVariant, 0x4F53). %<kMatthews
unicode_unihan_variant(0x9AD4, kSimplifiedVariant, 0x4F53).
unicode_unihan_variant(0x9AD5, kSemanticVariant, 0x81CF). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9AD5, kSimplifiedVariant, 0x9ACC).
unicode_unihan_variant(0x9AD5, kZVariant, 0x81CF).
unicode_unihan_variant(0x9AD6, kSimplifiedVariant, 0x9ACB).
unicode_unihan_variant(0x9AD7, kSemanticVariant, 0x9871). %<kMatthews
unicode_unihan_variant(0x9AD7, kZVariant, 0x81DA).
unicode_unihan_variant(0x9AD8, kSemanticVariant, 0x9AD9). %<kFenn
unicode_unihan_variant(0x9AD8, kZVariant, 0x9AD9).
unicode_unihan_variant(0x9AD9, kSemanticVariant, 0x9AD8). %<kFenn
unicode_unihan_variant(0x9AD9, kZVariant, 0x9AD8).
unicode_unihan_variant(0x9AE3, kSemanticVariant, 0x5F77). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9AE3, kZVariant, 0x4EFF).
unicode_unihan_variant(0x9AE6, kZVariant, 0x729B).
unicode_unihan_variant(0x9AEA, kZVariant, 0x9AEE).
unicode_unihan_variant(0x9AEE, kSimplifiedVariant, 0x53D1).
unicode_unihan_variant(0x9AEF, kZVariant, 0x9AE5).
unicode_unihan_variant(0x9AF4, kZVariant, 0x5F7F).
unicode_unihan_variant(0x9AFB, kSemanticVariant, 0x4BFB). %<kLau
unicode_unihan_variant(0x9AFC, kSemanticVariant, 0x9B05). %<kMeyerWempe
unicode_unihan_variant(0x9B00, kSemanticVariant, 0x5243). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9B02, kSemanticVariant, 0x9B22). %<kMatthews
unicode_unihan_variant(0x9B03, kZVariant, 0x9A94).
unicode_unihan_variant(0x9B05, kSemanticVariant, 0x9AFC). %<kMeyerWempe
unicode_unihan_variant(0x9B06, kSimplifiedVariant, 0x677E).
unicode_unihan_variant(0x9B0D, kSimplifiedVariant, 0x80E1).
unicode_unihan_variant(0x9B12, kSemanticVariant, 0x3431). %<kMatthews
unicode_unihan_variant(0x9B13, kSemanticVariant, 0x29BED). %<kMeyerWempe
unicode_unihan_variant(0x9B13, kTraditionalVariant, 0x9B22).
unicode_unihan_variant(0x9B1A, kSimplifiedVariant, 0x987B).
unicode_unihan_variant(0x9B22, kSemanticVariant, 0x9B02). %<kMatthews
unicode_unihan_variant(0x9B22, kSimplifiedVariant, 0x9B13).
unicode_unihan_variant(0x9B23, kSemanticVariant, 0x5DE4). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9B23, kSpecializedSemanticVariant, 0x5DE4). %<kFenn
unicode_unihan_variant(0x9B25, kSemanticVariant, 0x9B2D). %<kLau,kMatthews,kMeyerWempe 0x9B2C<kMatthews 0x9B26<kLau,kMeyerWempe 0x9B2A<kLau,kMatthews
unicode_unihan_variant(0x9B25, kSimplifiedVariant, 0x6597).
unicode_unihan_variant(0x9B25, kZVariant, 0x9B2C).
unicode_unihan_variant(0x9B26, kSemanticVariant, 0x9B25). %<kLau,kMeyerWempe
unicode_unihan_variant(0x9B26, kZVariant, 0x9B25).
unicode_unihan_variant(0x9B27, kSemanticVariant, 0x9599). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9B27, kSimplifiedVariant, 0x95F9).
unicode_unihan_variant(0x9B28, kSemanticVariant, 0x95A7). %<kMatthews
unicode_unihan_variant(0x9B28, kZVariant, 0x54C4).
unicode_unihan_variant(0x9B29, kSimplifiedVariant, 0x960B).
unicode_unihan_variant(0x9B2A, kSemanticVariant, 0x9B25). %<kLau,kMatthews 0x9B2C<kFenn
unicode_unihan_variant(0x9B2A, kZVariant, 0x9B25).
unicode_unihan_variant(0x9B2C, kSemanticVariant, 0x9B25). %<kMatthews 0x9B2A<kFenn
unicode_unihan_variant(0x9B2C, kZVariant, 0x9B25).
unicode_unihan_variant(0x9B2D, kSemanticVariant, 0x9B25). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9B2D, kZVariant, 0x9B25).
unicode_unihan_variant(0x9B2E, kSemanticVariant, 0x4C17). %<kFenn
unicode_unihan_variant(0x9B2E, kSimplifiedVariant, 0x9604).
unicode_unihan_variant(0x9B30, kZVariant, 0x9B31).
unicode_unihan_variant(0x9B31, kSemanticVariant, 0x6B1D). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9B31, kSimplifiedVariant, 0x90C1).
unicode_unihan_variant(0x9B31, kZVariant, 0x9B30).
unicode_unihan_variant(0x9B36, kTraditionalVariant, 0x9B39).
unicode_unihan_variant(0x9B39, kSimplifiedVariant, 0x9B36).
unicode_unihan_variant(0x9B3B, kZVariant, 0x7CA5).
unicode_unihan_variant(0x9B47, kTraditionalVariant, 0x9B58).
unicode_unihan_variant(0x9B49, kTraditionalVariant, 0x9B4E).
unicode_unihan_variant(0x9B4E, kSimplifiedVariant, 0x9B49).
unicode_unihan_variant(0x9B4F, kSemanticVariant, 0x5DCD). %<kMeyerWempe
unicode_unihan_variant(0x9B58, kSimplifiedVariant, 0x9B47).
unicode_unihan_variant(0x9B59, kSemanticVariant, 0x807B). %<kMeyerWempe
unicode_unihan_variant(0x9B5A, kSimplifiedVariant, 0x9C7C).
unicode_unihan_variant(0x9B5B, kSimplifiedVariant, 0x9C7D).
unicode_unihan_variant(0x9B5F, kSemanticVariant, 0x9C29). %<kMatthews
unicode_unihan_variant(0x9B5F, kSimplifiedVariant, 0x2B689).
unicode_unihan_variant(0x9B62, kSimplifiedVariant, 0x9C7E).
unicode_unihan_variant(0x9B65, kSimplifiedVariant, 0x29F79).
unicode_unihan_variant(0x9B66, kSemanticVariant, 0x9BCA). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9B68, kSimplifiedVariant, 0x9C80).
unicode_unihan_variant(0x9B6F, kSimplifiedVariant, 0x9C81).
unicode_unihan_variant(0x9B6F, kZVariant, 0xF939).
unicode_unihan_variant(0x9B74, kSimplifiedVariant, 0x9C82).
unicode_unihan_variant(0x9B77, kSemanticVariant, 0x9C0C). %<kLau
unicode_unihan_variant(0x9B77, kSimplifiedVariant, 0x9C7F).
unicode_unihan_variant(0x9B7A, kSimplifiedVariant, 0x9C84).
unicode_unihan_variant(0x9B81, kSimplifiedVariant, 0x9C85).
unicode_unihan_variant(0x9B83, kSimplifiedVariant, 0x9C86).
unicode_unihan_variant(0x9B84, kSimplifiedVariant, 0x2B692).
unicode_unihan_variant(0x9B8A, kSimplifiedVariant, 0x9C8C).
unicode_unihan_variant(0x9B8B, kSimplifiedVariant, 0x9C89).
unicode_unihan_variant(0x9B8D, kSimplifiedVariant, 0x9C8F).
unicode_unihan_variant(0x9B90, kSimplifiedVariant, 0x9C90).
unicode_unihan_variant(0x9B91, kSimplifiedVariant, 0x9C8D).
unicode_unihan_variant(0x9B92, kSimplifiedVariant, 0x9C8B).
unicode_unihan_variant(0x9B93, kSimplifiedVariant, 0x9C8A).
unicode_unihan_variant(0x9B9A, kSimplifiedVariant, 0x9C92).
unicode_unihan_variant(0x9B9C, kSimplifiedVariant, 0x9C98).
unicode_unihan_variant(0x9B9D, kSemanticVariant, 0x9BD7). %<kMatthews
unicode_unihan_variant(0x9B9E, kSimplifiedVariant, 0x9C95).
unicode_unihan_variant(0x9B9F, kSimplifiedVariant, 0x29F7E).
unicode_unihan_variant(0x9BA3, kSimplifiedVariant, 0x4C9F).
unicode_unihan_variant(0x9BA6, kSimplifiedVariant, 0x9C96).
unicode_unihan_variant(0x9BA7, kSemanticVariant, 0x9BF7). %<kMatthews
unicode_unihan_variant(0x9BAA, kSimplifiedVariant, 0x9C94).
unicode_unihan_variant(0x9BAB, kSimplifiedVariant, 0x9C9B).
unicode_unihan_variant(0x9BAD, kSimplifiedVariant, 0x9C91).
unicode_unihan_variant(0x9BAE, kSemanticVariant, 0x9C7B). %<kMatthews
unicode_unihan_variant(0x9BAE, kSimplifiedVariant, 0x9C9C).
unicode_unihan_variant(0x9BAE, kZVariant, 0x5C1F).
unicode_unihan_variant(0x9BB0, kSimplifiedVariant, 0x2B694).
unicode_unihan_variant(0x9BB3, kSimplifiedVariant, 0x9C93).
unicode_unihan_variant(0x9BB6, kSimplifiedVariant, 0x9CAA).
unicode_unihan_variant(0x9BB8, kSimplifiedVariant, 0x29F83).
unicode_unihan_variant(0x9BBA, kSimplifiedVariant, 0x9C9D).
unicode_unihan_variant(0x9BC0, kSimplifiedVariant, 0x9CA7).
unicode_unihan_variant(0x9BC1, kSemanticVariant, 0x632D). %<kMatthews 0x9ABE<kMatthews
unicode_unihan_variant(0x9BC1, kSimplifiedVariant, 0x9CA0).
unicode_unihan_variant(0x9BC1, kSpecializedSemanticVariant, 0x9ABE). %<kMeyerWempe
unicode_unihan_variant(0x9BC4, kSimplifiedVariant, 0x29F81).
unicode_unihan_variant(0x9BC6, kSimplifiedVariant, 0x2B699).
unicode_unihan_variant(0x9BC7, kSimplifiedVariant, 0x9CA9).
unicode_unihan_variant(0x9BC9, kSimplifiedVariant, 0x9CA4).
unicode_unihan_variant(0x9BCA, kSemanticVariant, 0x9B66). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9BCA, kSimplifiedVariant, 0x9CA8).
unicode_unihan_variant(0x9BCB, kZVariant, 0x9BCA).
unicode_unihan_variant(0x9BD2, kSimplifiedVariant, 0x9CAC).
unicode_unihan_variant(0x9BD4, kSimplifiedVariant, 0x9CBB).
unicode_unihan_variant(0x9BD5, kSimplifiedVariant, 0x9CAF).
unicode_unihan_variant(0x9BD6, kSimplifiedVariant, 0x9CAD).
unicode_unihan_variant(0x9BD7, kSemanticVariant, 0x9B9D). %<kMatthews
unicode_unihan_variant(0x9BD7, kSimplifiedVariant, 0x9C9E).
unicode_unihan_variant(0x9BDB, kSimplifiedVariant, 0x9CB7).
unicode_unihan_variant(0x9BDD, kSimplifiedVariant, 0x9CB4).
unicode_unihan_variant(0x9BE1, kSimplifiedVariant, 0x9CB1).
unicode_unihan_variant(0x9BE2, kSimplifiedVariant, 0x9CB5).
unicode_unihan_variant(0x9BE4, kSimplifiedVariant, 0x9CB2).
unicode_unihan_variant(0x9BE7, kSemanticVariant, 0x4C7D). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9BE7, kSimplifiedVariant, 0x9CB3).
unicode_unihan_variant(0x9BE8, kSimplifiedVariant, 0x9CB8).
unicode_unihan_variant(0x9BEA, kSimplifiedVariant, 0x9CAE).
unicode_unihan_variant(0x9BEB, kSimplifiedVariant, 0x9CB0).
unicode_unihan_variant(0x9BF1, kSimplifiedVariant, 0x29F87).
unicode_unihan_variant(0x9BF4, kSimplifiedVariant, 0x9CBA).
unicode_unihan_variant(0x9BF6, kSimplifiedVariant, 0x29F7C).
unicode_unihan_variant(0x9BF7, kSemanticVariant, 0x9BA7). %<kMatthews
unicode_unihan_variant(0x9BF7, kSimplifiedVariant, 0x9CC0).
unicode_unihan_variant(0x9BFD, kSimplifiedVariant, 0x9CAB).
unicode_unihan_variant(0x9BFF, kSimplifiedVariant, 0x9CCA).
unicode_unihan_variant(0x9C01, kSimplifiedVariant, 0x9CC8).
unicode_unihan_variant(0x9C02, kSimplifiedVariant, 0x9C97).
unicode_unihan_variant(0x9C03, kSimplifiedVariant, 0x9CC2).
unicode_unihan_variant(0x9C06, kSimplifiedVariant, 0x4CA0).
unicode_unihan_variant(0x9C08, kSimplifiedVariant, 0x9CBD).
unicode_unihan_variant(0x9C09, kSemanticVariant, 0x9C51). %<kMatthews
unicode_unihan_variant(0x9C09, kSimplifiedVariant, 0x9CC7).
unicode_unihan_variant(0x9C0C, kSemanticVariant, 0x9B77). %<kLau 0x9C0D<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9C0C, kSimplifiedVariant, 0x4CA1).
unicode_unihan_variant(0x9C0C, kZVariant, 0x9C0D).
unicode_unihan_variant(0x9C0D, kSemanticVariant, 0x9C0C). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9C0D, kSimplifiedVariant, 0x9CC5).
unicode_unihan_variant(0x9C0F, kSimplifiedVariant, 0x9CBE).
unicode_unihan_variant(0x9C10, kSemanticVariant, 0x9C77). %<kLau,kMatthews
unicode_unihan_variant(0x9C12, kSimplifiedVariant, 0x9CC6).
unicode_unihan_variant(0x9C13, kSimplifiedVariant, 0x9CC3).
unicode_unihan_variant(0x9C15, kSemanticVariant, 0x8766). %<kMeyerWempe
unicode_unihan_variant(0x9C15, kSpecializedSemanticVariant, 0x8766). %<kFenn
unicode_unihan_variant(0x9C1B, kZVariant, 0x9CC1).
unicode_unihan_variant(0x9C1C, kSimplifiedVariant, 0x9CD2).
unicode_unihan_variant(0x9C1F, kSimplifiedVariant, 0x9CD1).
unicode_unihan_variant(0x9C20, kSimplifiedVariant, 0x9CCB).
unicode_unihan_variant(0x9C23, kSimplifiedVariant, 0x9CA5).
unicode_unihan_variant(0x9C24, kSimplifiedVariant, 0x2B695).
unicode_unihan_variant(0x9C25, kSimplifiedVariant, 0x9CCF).
unicode_unihan_variant(0x9C27, kSimplifiedVariant, 0x4CA2).
unicode_unihan_variant(0x9C28, kSimplifiedVariant, 0x9CCE).
unicode_unihan_variant(0x9C29, kSemanticVariant, 0x9B5F). %<kMatthews
unicode_unihan_variant(0x9C29, kSimplifiedVariant, 0x9CD0).
unicode_unihan_variant(0x9C2D, kSimplifiedVariant, 0x9CCD).
unicode_unihan_variant(0x9C2E, kSimplifiedVariant, 0x9CC1).
unicode_unihan_variant(0x9C31, kSimplifiedVariant, 0x9CA2).
unicode_unihan_variant(0x9C32, kSemanticVariant, 0x9F07). %<kMatthews
unicode_unihan_variant(0x9C32, kSimplifiedVariant, 0x9CCC).
unicode_unihan_variant(0x9C33, kSimplifiedVariant, 0x9CD3).
unicode_unihan_variant(0x9C35, kSimplifiedVariant, 0x9CD8).
unicode_unihan_variant(0x9C37, kSimplifiedVariant, 0x9CA6).
unicode_unihan_variant(0x9C39, kSimplifiedVariant, 0x9CA3).
unicode_unihan_variant(0x9C3A, kSimplifiedVariant, 0x9CB9).
unicode_unihan_variant(0x9C3B, kSimplifiedVariant, 0x9CD7).
unicode_unihan_variant(0x9C3C, kSimplifiedVariant, 0x9CDB).
unicode_unihan_variant(0x9C3E, kSimplifiedVariant, 0x9CD4).
unicode_unihan_variant(0x9C42, kSimplifiedVariant, 0x9CC9).
unicode_unihan_variant(0x9C45, kSimplifiedVariant, 0x9CD9).
unicode_unihan_variant(0x9C47, kSimplifiedVariant, 0x29F8C).
unicode_unihan_variant(0x9C48, kSimplifiedVariant, 0x9CD5).
unicode_unihan_variant(0x9C49, kSemanticVariant, 0x9F08). %<kMatthews
unicode_unihan_variant(0x9C49, kSimplifiedVariant, 0x9CD6).
unicode_unihan_variant(0x9C49, kZVariant, 0x9F9E).
unicode_unihan_variant(0x9C51, kSemanticVariant, 0x9C09). %<kMatthews
unicode_unihan_variant(0x9C52, kSimplifiedVariant, 0x9CDF).
unicode_unihan_variant(0x9C53, kSemanticVariant, 0x4C47). %<kMatthews 0x9C54<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9C53, kZVariant, 0x9C54).
unicode_unihan_variant(0x9C54, kSemanticVariant, 0x9C53). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9C54, kSimplifiedVariant, 0x9CDD).
unicode_unihan_variant(0x9C56, kSimplifiedVariant, 0x9CDC).
unicode_unihan_variant(0x9C57, kSimplifiedVariant, 0x9CDE).
unicode_unihan_variant(0x9C57, kZVariant, 0xF9F2).
unicode_unihan_variant(0x9C58, kSimplifiedVariant, 0x9C9F).
unicode_unihan_variant(0x9C5D, kSimplifiedVariant, 0x9CBC).
unicode_unihan_variant(0x9C5F, kSimplifiedVariant, 0x9C8E).
unicode_unihan_variant(0x9C60, kSemanticVariant, 0x81BE). %<kMatthews
unicode_unihan_variant(0x9C60, kSimplifiedVariant, 0x9C99).
unicode_unihan_variant(0x9C60, kSpecializedSemanticVariant, 0x81BE). %<kMeyerWempe
unicode_unihan_variant(0x9C63, kSimplifiedVariant, 0x9CE3).
unicode_unihan_variant(0x9C64, kSimplifiedVariant, 0x9CE1).
unicode_unihan_variant(0x9C67, kSimplifiedVariant, 0x9CE2).
unicode_unihan_variant(0x9C68, kSimplifiedVariant, 0x9CBF).
unicode_unihan_variant(0x9C6D, kSimplifiedVariant, 0x9C9A).
unicode_unihan_variant(0x9C6E, kSimplifiedVariant, 0x2B688).
unicode_unihan_variant(0x9C6F, kSimplifiedVariant, 0x9CE0).
unicode_unihan_variant(0x9C77, kSemanticVariant, 0x9C10). %<kLau,kMatthews
unicode_unihan_variant(0x9C77, kSimplifiedVariant, 0x9CC4).
unicode_unihan_variant(0x9C78, kSimplifiedVariant, 0x9C88).
unicode_unihan_variant(0x9C7A, kSimplifiedVariant, 0x9CA1).
unicode_unihan_variant(0x9C7B, kSemanticVariant, 0x9BAE). %<kMatthews
unicode_unihan_variant(0x9C7C, kTraditionalVariant, 0x9B5A).
unicode_unihan_variant(0x9C7D, kTraditionalVariant, 0x9B5B).
unicode_unihan_variant(0x9C7E, kTraditionalVariant, 0x9B62).
unicode_unihan_variant(0x9C7F, kTraditionalVariant, 0x9B77).
unicode_unihan_variant(0x9C80, kTraditionalVariant, 0x9B68).
unicode_unihan_variant(0x9C81, kTraditionalVariant, 0x9B6F).
unicode_unihan_variant(0x9C82, kTraditionalVariant, 0x9B74).
unicode_unihan_variant(0x9C83, kTraditionalVariant, 0x4C3E).
unicode_unihan_variant(0x9C84, kTraditionalVariant, 0x9B7A).
unicode_unihan_variant(0x9C85, kTraditionalVariant, 0x9B81).
unicode_unihan_variant(0x9C86, kTraditionalVariant, 0x9B83).
unicode_unihan_variant(0x9C88, kTraditionalVariant, 0x9C78).
unicode_unihan_variant(0x9C89, kTraditionalVariant, 0x9B8B).
unicode_unihan_variant(0x9C8A, kTraditionalVariant, 0x9B93).
unicode_unihan_variant(0x9C8B, kTraditionalVariant, 0x9B92).
unicode_unihan_variant(0x9C8C, kTraditionalVariant, 0x9B8A).
unicode_unihan_variant(0x9C8D, kTraditionalVariant, 0x9B91).
unicode_unihan_variant(0x9C8E, kTraditionalVariant, 0x9C5F).
unicode_unihan_variant(0x9C8F, kTraditionalVariant, 0x9B8D).
unicode_unihan_variant(0x9C90, kTraditionalVariant, 0x9B90).
unicode_unihan_variant(0x9C91, kTraditionalVariant, 0x9BAD).
unicode_unihan_variant(0x9C92, kTraditionalVariant, 0x9B9A).
unicode_unihan_variant(0x9C93, kTraditionalVariant, 0x9BB3).
unicode_unihan_variant(0x9C94, kTraditionalVariant, 0x9BAA).
unicode_unihan_variant(0x9C95, kTraditionalVariant, 0x9B9E).
unicode_unihan_variant(0x9C96, kTraditionalVariant, 0x9BA6).
unicode_unihan_variant(0x9C97, kTraditionalVariant, 0x9C02).
unicode_unihan_variant(0x9C98, kTraditionalVariant, 0x9B9C).
unicode_unihan_variant(0x9C99, kTraditionalVariant, 0x9C60).
unicode_unihan_variant(0x9C9A, kTraditionalVariant, 0x9C6D).
unicode_unihan_variant(0x9C9B, kTraditionalVariant, 0x9BAB).
unicode_unihan_variant(0x9C9C, kTraditionalVariant, 0x9BAE).
unicode_unihan_variant(0x9C9D, kTraditionalVariant, 0x9BBA).
unicode_unihan_variant(0x9C9E, kTraditionalVariant, 0x9BD7).
unicode_unihan_variant(0x9C9F, kTraditionalVariant, 0x9C58).
unicode_unihan_variant(0x9CA0, kTraditionalVariant, 0x9BC1).
unicode_unihan_variant(0x9CA1, kTraditionalVariant, 0x9C7A).
unicode_unihan_variant(0x9CA2, kTraditionalVariant, 0x9C31).
unicode_unihan_variant(0x9CA3, kTraditionalVariant, 0x9C39).
unicode_unihan_variant(0x9CA4, kTraditionalVariant, 0x9BC9).
unicode_unihan_variant(0x9CA5, kTraditionalVariant, 0x9C23).
unicode_unihan_variant(0x9CA6, kTraditionalVariant, 0x9C37).
unicode_unihan_variant(0x9CA7, kTraditionalVariant, 0x9BC0).
unicode_unihan_variant(0x9CA8, kTraditionalVariant, 0x9BCA).
unicode_unihan_variant(0x9CA9, kTraditionalVariant, 0x9BC7).
unicode_unihan_variant(0x9CAA, kTraditionalVariant, 0x9BB6).
unicode_unihan_variant(0x9CAB, kTraditionalVariant, 0x9BFD).
unicode_unihan_variant(0x9CAC, kTraditionalVariant, 0x9BD2).
unicode_unihan_variant(0x9CAD, kTraditionalVariant, 0x9BD6).
unicode_unihan_variant(0x9CAE, kTraditionalVariant, 0x9BEA).
unicode_unihan_variant(0x9CAF, kTraditionalVariant, 0x9BD5).
unicode_unihan_variant(0x9CB0, kTraditionalVariant, 0x9BEB).
unicode_unihan_variant(0x9CB1, kTraditionalVariant, 0x9BE1).
unicode_unihan_variant(0x9CB2, kTraditionalVariant, 0x9BE4).
unicode_unihan_variant(0x9CB3, kTraditionalVariant, 0x9BE7).
unicode_unihan_variant(0x9CB4, kTraditionalVariant, 0x9BDD).
unicode_unihan_variant(0x9CB5, kTraditionalVariant, 0x9BE2).
unicode_unihan_variant(0x9CB7, kTraditionalVariant, 0x9BDB).
unicode_unihan_variant(0x9CB8, kTraditionalVariant, 0x9BE8).
unicode_unihan_variant(0x9CB9, kTraditionalVariant, 0x9C3A).
unicode_unihan_variant(0x9CBA, kTraditionalVariant, 0x9BF4).
unicode_unihan_variant(0x9CBB, kTraditionalVariant, 0x9BD4).
unicode_unihan_variant(0x9CBC, kTraditionalVariant, 0x9C5D).
unicode_unihan_variant(0x9CBD, kTraditionalVariant, 0x9C08).
unicode_unihan_variant(0x9CBE, kTraditionalVariant, 0x9C0F).
unicode_unihan_variant(0x9CBF, kTraditionalVariant, 0x9C68).
unicode_unihan_variant(0x9CC0, kTraditionalVariant, 0x9BF7).
unicode_unihan_variant(0x9CC1, kTraditionalVariant, 0x9C2E).
unicode_unihan_variant(0x9CC1, kZVariant, 0x9C1B).
unicode_unihan_variant(0x9CC2, kTraditionalVariant, 0x9C03).
unicode_unihan_variant(0x9CC3, kTraditionalVariant, 0x9C13).
unicode_unihan_variant(0x9CC4, kTraditionalVariant, 0x9C77).
unicode_unihan_variant(0x9CC5, kTraditionalVariant, 0x9C0D).
unicode_unihan_variant(0x9CC6, kTraditionalVariant, 0x9C12).
unicode_unihan_variant(0x9CC7, kTraditionalVariant, 0x9C09).
unicode_unihan_variant(0x9CC8, kTraditionalVariant, 0x9C01).
unicode_unihan_variant(0x9CC9, kTraditionalVariant, 0x9C42).
unicode_unihan_variant(0x9CCA, kTraditionalVariant, 0x9BFF).
unicode_unihan_variant(0x9CCB, kTraditionalVariant, 0x9C20).
unicode_unihan_variant(0x9CCC, kTraditionalVariant, 0x9C32).
unicode_unihan_variant(0x9CCD, kTraditionalVariant, 0x9C2D).
unicode_unihan_variant(0x9CCE, kTraditionalVariant, 0x9C28).
unicode_unihan_variant(0x9CCF, kTraditionalVariant, 0x9C25).
unicode_unihan_variant(0x9CD0, kTraditionalVariant, 0x9C29).
unicode_unihan_variant(0x9CD1, kTraditionalVariant, 0x9C1F).
unicode_unihan_variant(0x9CD2, kTraditionalVariant, 0x9C1C).
unicode_unihan_variant(0x9CD3, kTraditionalVariant, 0x9C33).
unicode_unihan_variant(0x9CD4, kTraditionalVariant, 0x9C3E).
unicode_unihan_variant(0x9CD5, kTraditionalVariant, 0x9C48).
unicode_unihan_variant(0x9CD6, kTraditionalVariant, 0x9C49).
unicode_unihan_variant(0x9CD7, kTraditionalVariant, 0x9C3B).
unicode_unihan_variant(0x9CD8, kTraditionalVariant, 0x9C35).
unicode_unihan_variant(0x9CD9, kTraditionalVariant, 0x9C45).
unicode_unihan_variant(0x9CDA, kTraditionalVariant, 0x4C81).
unicode_unihan_variant(0x9CDB, kTraditionalVariant, 0x9C3C).
unicode_unihan_variant(0x9CDC, kTraditionalVariant, 0x9C56).
unicode_unihan_variant(0x9CDD, kTraditionalVariant, 0x9C54).
unicode_unihan_variant(0x9CDE, kTraditionalVariant, 0x9C57).
unicode_unihan_variant(0x9CDF, kTraditionalVariant, 0x9C52).
unicode_unihan_variant(0x9CE0, kTraditionalVariant, 0x9C6F).
unicode_unihan_variant(0x9CE1, kTraditionalVariant, 0x9C64).
unicode_unihan_variant(0x9CE2, kTraditionalVariant, 0x9C67).
unicode_unihan_variant(0x9CE3, kTraditionalVariant, 0x9C63).
unicode_unihan_variant(0x9CE5, kSimplifiedVariant, 0x9E1F).
unicode_unihan_variant(0x9CE7, kSimplifiedVariant, 0x51EB).
unicode_unihan_variant(0x9CE9, kSimplifiedVariant, 0x9E20).
unicode_unihan_variant(0x9CEB, kZVariant, 0x96C1).
unicode_unihan_variant(0x9CEE, kSemanticVariant, 0x96DE). %<kFenn 0x9DC4<kFenn
unicode_unihan_variant(0x9CEE, kZVariant, 0x96DE).
unicode_unihan_variant(0x9CEF, kZVariant, 0x9CF3).
unicode_unihan_variant(0x9CF2, kSimplifiedVariant, 0x9E24).
unicode_unihan_variant(0x9CF3, kSimplifiedVariant, 0x51E4).
unicode_unihan_variant(0x9CF3, kZVariant, 0x9CEF).
unicode_unihan_variant(0x9CF4, kSimplifiedVariant, 0x9E23).
unicode_unihan_variant(0x9CF6, kSimplifiedVariant, 0x9E22).
unicode_unihan_variant(0x9CF7, kSimplifiedVariant, 0x2B6DB).
unicode_unihan_variant(0x9CFC, kSimplifiedVariant, 0x2A243).
unicode_unihan_variant(0x9CFE, kSimplifiedVariant, 0x4D13).
unicode_unihan_variant(0x9D03, kSemanticVariant, 0x9D59). %<kMatthews
unicode_unihan_variant(0x9D03, kSimplifiedVariant, 0x2B6DE).
unicode_unihan_variant(0x9D04, kSpecializedSemanticVariant, 0x88D2). %<kMeyerWempe
unicode_unihan_variant(0x9D06, kSemanticVariant, 0x9156). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9D06, kSimplifiedVariant, 0x9E29).
unicode_unihan_variant(0x9D07, kSimplifiedVariant, 0x9E28).
unicode_unihan_variant(0x9D08, kSemanticVariant, 0x96C1). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9D08, kZVariant, 0x96C1).
unicode_unihan_variant(0x9D09, kSimplifiedVariant, 0x9E26).
unicode_unihan_variant(0x9D09, kZVariant, 0x9D76).
unicode_unihan_variant(0x9D0E, kZVariant, 0x9DD7).
unicode_unihan_variant(0x9D12, kSimplifiedVariant, 0x9E30).
unicode_unihan_variant(0x9D15, kSimplifiedVariant, 0x9E35).
unicode_unihan_variant(0x9D17, kSimplifiedVariant, 0x2B061).
unicode_unihan_variant(0x9D1B, kSimplifiedVariant, 0x9E33).
unicode_unihan_variant(0x9D1C, kSimplifiedVariant, 0x2A248).
unicode_unihan_variant(0x9D1D, kSemanticVariant, 0x6710). %<kMatthews 0x9E1C<kMatthews
unicode_unihan_variant(0x9D1D, kSimplifiedVariant, 0x9E32).
unicode_unihan_variant(0x9D1E, kSemanticVariant, 0x689F). %<kFenn
unicode_unihan_variant(0x9D1E, kSimplifiedVariant, 0x9E2E).
unicode_unihan_variant(0x9D1F, kSimplifiedVariant, 0x9E31).
unicode_unihan_variant(0x9D23, kSimplifiedVariant, 0x9E2A).
unicode_unihan_variant(0x9D26, kSimplifiedVariant, 0x9E2F).
unicode_unihan_variant(0x9D28, kSimplifiedVariant, 0x9E2D).
unicode_unihan_variant(0x9D2C, kZVariant, 0x9DAF).
unicode_unihan_variant(0x9D2F, kSimplifiedVariant, 0x9E38).
unicode_unihan_variant(0x9D30, kSimplifiedVariant, 0x9E39).
unicode_unihan_variant(0x9D32, kSimplifiedVariant, 0x2A246).
unicode_unihan_variant(0x9D33, kSemanticVariant, 0x9DC3). %<kMatthews
unicode_unihan_variant(0x9D34, kSimplifiedVariant, 0x9E3B).
unicode_unihan_variant(0x9D37, kSimplifiedVariant, 0x4D15).
unicode_unihan_variant(0x9D3A, kSemanticVariant, 0x9D5C). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9D3B, kSimplifiedVariant, 0x9E3F).
unicode_unihan_variant(0x9D3F, kSimplifiedVariant, 0x9E3D).
unicode_unihan_variant(0x9D41, kSimplifiedVariant, 0x4D14).
unicode_unihan_variant(0x9D42, kSimplifiedVariant, 0x9E3A).
unicode_unihan_variant(0x9D43, kSimplifiedVariant, 0x9E3C).
unicode_unihan_variant(0x9D49, kZVariant, 0x9E1E).
unicode_unihan_variant(0x9D50, kSimplifiedVariant, 0x9E40).
unicode_unihan_variant(0x9D51, kSimplifiedVariant, 0x9E43).
unicode_unihan_variant(0x9D52, kSimplifiedVariant, 0x9E46).
unicode_unihan_variant(0x9D53, kSimplifiedVariant, 0x9E41).
unicode_unihan_variant(0x9D59, kSemanticVariant, 0x9D03). %<kMatthews
unicode_unihan_variant(0x9D5A, kSimplifiedVariant, 0x2A24D).
unicode_unihan_variant(0x9D5C, kSemanticVariant, 0x9D3A). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9D5C, kSimplifiedVariant, 0x9E48).
unicode_unihan_variant(0x9D5D, kSemanticVariant, 0x9D5E). %<kLau,kMeyerWempe
unicode_unihan_variant(0x9D5D, kSimplifiedVariant, 0x9E45).
unicode_unihan_variant(0x9D5E, kSemanticVariant, 0x9D5D). %<kLau,kMeyerWempe
unicode_unihan_variant(0x9D5E, kZVariant, 0x9D5D).
unicode_unihan_variant(0x9D60, kSimplifiedVariant, 0x9E44).
unicode_unihan_variant(0x9D61, kSimplifiedVariant, 0x9E49).
unicode_unihan_variant(0x9D6A, kSimplifiedVariant, 0x9E4C).
unicode_unihan_variant(0x9D6C, kSimplifiedVariant, 0x9E4F).
unicode_unihan_variant(0x9D6E, kSimplifiedVariant, 0x9E50).
unicode_unihan_variant(0x9D6F, kSimplifiedVariant, 0x9E4E).
unicode_unihan_variant(0x9D70, kSpecializedSemanticVariant, 0x5F6B). %0x96D5
unicode_unihan_variant(0x9D72, kSimplifiedVariant, 0x9E4A).
unicode_unihan_variant(0x9D77, kSimplifiedVariant, 0x9E53).
unicode_unihan_variant(0x9D7B, kSemanticVariant, 0x96BC). %<kMeyerWempe
unicode_unihan_variant(0x9D7E, kSimplifiedVariant, 0x9E4D).
unicode_unihan_variant(0x9D82, kSemanticVariant, 0x9D83). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9D83, kSemanticVariant, 0x9D82). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9D84, kSimplifiedVariant, 0x4D16).
unicode_unihan_variant(0x9D87, kSimplifiedVariant, 0x9E2B).
unicode_unihan_variant(0x9D89, kSimplifiedVariant, 0x9E51).
unicode_unihan_variant(0x9D8A, kSimplifiedVariant, 0x9E52).
unicode_unihan_variant(0x9D8F, kZVariant, 0x96DE).
unicode_unihan_variant(0x9D92, kSimplifiedVariant, 0x2B6F6).
unicode_unihan_variant(0x9D93, kSimplifiedVariant, 0x9E4B).
unicode_unihan_variant(0x9D96, kSimplifiedVariant, 0x9E59).
unicode_unihan_variant(0x9D97, kSimplifiedVariant, 0x2B6F8).
unicode_unihan_variant(0x9D98, kSimplifiedVariant, 0x9E55).
unicode_unihan_variant(0x9D9A, kSimplifiedVariant, 0x9E57).
unicode_unihan_variant(0x9DA1, kSimplifiedVariant, 0x9E56).
unicode_unihan_variant(0x9DA5, kSimplifiedVariant, 0x9E5B).
unicode_unihan_variant(0x9DA9, kSimplifiedVariant, 0x9E5C).
unicode_unihan_variant(0x9DAA, kSimplifiedVariant, 0x4D17).
unicode_unihan_variant(0x9DAC, kSimplifiedVariant, 0x9E27).
unicode_unihan_variant(0x9DAF, kSemanticVariant, 0x9E0E). %<kMatthews
unicode_unihan_variant(0x9DAF, kSimplifiedVariant, 0x83BA).
unicode_unihan_variant(0x9DB2, kSimplifiedVariant, 0x9E5F).
unicode_unihan_variant(0x9DB4, kSimplifiedVariant, 0x9E64).
unicode_unihan_variant(0x9DB5, kSemanticVariant, 0x96DB). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9DB9, kSimplifiedVariant, 0x9E60).
unicode_unihan_variant(0x9DBA, kSimplifiedVariant, 0x9E61).
unicode_unihan_variant(0x9DBB, kSimplifiedVariant, 0x9E58).
unicode_unihan_variant(0x9DBC, kSimplifiedVariant, 0x9E63).
unicode_unihan_variant(0x9DBD, kSemanticVariant, 0x96BC). %<kMatthews
unicode_unihan_variant(0x9DBF, kSimplifiedVariant, 0x9E5A).
unicode_unihan_variant(0x9DC1, kSimplifiedVariant, 0x9E62).
unicode_unihan_variant(0x9DC2, kSimplifiedVariant, 0x9E5E).
unicode_unihan_variant(0x9DC3, kSemanticVariant, 0x9D33). %<kMatthews
unicode_unihan_variant(0x9DC4, kSemanticVariant, 0x96DE). %<kLau,kMatthews 0x9CEE<kFenn
unicode_unihan_variant(0x9DC4, kZVariant, 0x96DE).
unicode_unihan_variant(0x9DC8, kSimplifiedVariant, 0x4D18).
unicode_unihan_variant(0x9DCA, kSimplifiedVariant, 0x9E5D).
unicode_unihan_variant(0x9DCF, kZVariant, 0x9DC6).
unicode_unihan_variant(0x9DD3, kSimplifiedVariant, 0x9E67).
unicode_unihan_variant(0x9DD4, kSimplifiedVariant, 0x2A251).
unicode_unihan_variant(0x9DD6, kSimplifiedVariant, 0x9E65).
unicode_unihan_variant(0x9DD7, kSimplifiedVariant, 0x9E25).
unicode_unihan_variant(0x9DD9, kSimplifiedVariant, 0x9E37).
unicode_unihan_variant(0x9DDA, kSimplifiedVariant, 0x9E68).
unicode_unihan_variant(0x9DE5, kSimplifiedVariant, 0x9E36).
unicode_unihan_variant(0x9DE6, kSimplifiedVariant, 0x9E6A).
unicode_unihan_variant(0x9DE8, kSimplifiedVariant, 0x2A24A).
unicode_unihan_variant(0x9DE9, kSemanticVariant, 0x6C05). %<kFenn
unicode_unihan_variant(0x9DEB, kSimplifiedVariant, 0x9E54).
unicode_unihan_variant(0x9DEF, kSimplifiedVariant, 0x9E69).
unicode_unihan_variant(0x9DF0, kZVariant, 0x71D5).
unicode_unihan_variant(0x9DF2, kSimplifiedVariant, 0x9E6B).
unicode_unihan_variant(0x9DF3, kSimplifiedVariant, 0x9E47).
unicode_unihan_variant(0x9DF8, kSimplifiedVariant, 0x9E6C).
unicode_unihan_variant(0x9DF9, kSimplifiedVariant, 0x9E70).
unicode_unihan_variant(0x9DFA, kSimplifiedVariant, 0x9E6D).
unicode_unihan_variant(0x9DFA, kZVariant, 0xF93A).
unicode_unihan_variant(0x9DFD, kSimplifiedVariant, 0x9E34).
unicode_unihan_variant(0x9DFF, kSimplifiedVariant, 0x4D19).
unicode_unihan_variant(0x9E02, kSimplifiedVariant, 0x3D89).
unicode_unihan_variant(0x9E07, kSimplifiedVariant, 0x9E6F).
unicode_unihan_variant(0x9E0B, kSimplifiedVariant, 0x2B6E2).
unicode_unihan_variant(0x9E0C, kSimplifiedVariant, 0x9E71).
unicode_unihan_variant(0x9E0E, kSemanticVariant, 0x9DAF). %<kMatthews
unicode_unihan_variant(0x9E0F, kSimplifiedVariant, 0x9E72).
unicode_unihan_variant(0x9E15, kSimplifiedVariant, 0x9E2C).
unicode_unihan_variant(0x9E18, kSimplifiedVariant, 0x9E74).
unicode_unihan_variant(0x9E1A, kSimplifiedVariant, 0x9E66).
unicode_unihan_variant(0x9E1B, kSimplifiedVariant, 0x9E73).
unicode_unihan_variant(0x9E1C, kSemanticVariant, 0x6710). %<kMatthews 0x9D1D<kMatthews
unicode_unihan_variant(0x9E1D, kSimplifiedVariant, 0x9E42).
unicode_unihan_variant(0x9E1E, kSimplifiedVariant, 0x9E3E).
unicode_unihan_variant(0x9E1F, kTraditionalVariant, 0x9CE5).
unicode_unihan_variant(0x9E20, kTraditionalVariant, 0x9CE9).
unicode_unihan_variant(0x9E21, kTraditionalVariant, 0x96DE).
unicode_unihan_variant(0x9E21, kZVariant, 0x96DE).
unicode_unihan_variant(0x9E22, kTraditionalVariant, 0x9CF6).
unicode_unihan_variant(0x9E23, kTraditionalVariant, 0x9CF4).
unicode_unihan_variant(0x9E24, kTraditionalVariant, 0x9CF2).
unicode_unihan_variant(0x9E25, kTraditionalVariant, 0x9DD7).
unicode_unihan_variant(0x9E26, kTraditionalVariant, 0x9D09).
unicode_unihan_variant(0x9E27, kTraditionalVariant, 0x9DAC).
unicode_unihan_variant(0x9E28, kTraditionalVariant, 0x9D07).
unicode_unihan_variant(0x9E29, kTraditionalVariant, 0x9D06).
unicode_unihan_variant(0x9E2A, kTraditionalVariant, 0x9D23).
unicode_unihan_variant(0x9E2B, kTraditionalVariant, 0x9D87).
unicode_unihan_variant(0x9E2C, kTraditionalVariant, 0x9E15).
unicode_unihan_variant(0x9E2D, kTraditionalVariant, 0x9D28).
unicode_unihan_variant(0x9E2E, kTraditionalVariant, 0x9D1E).
unicode_unihan_variant(0x9E2F, kTraditionalVariant, 0x9D26).
unicode_unihan_variant(0x9E30, kTraditionalVariant, 0x9D12).
unicode_unihan_variant(0x9E31, kTraditionalVariant, 0x9D1F).
unicode_unihan_variant(0x9E32, kTraditionalVariant, 0x9D1D).
unicode_unihan_variant(0x9E33, kTraditionalVariant, 0x9D1B).
unicode_unihan_variant(0x9E34, kTraditionalVariant, 0x9DFD).
unicode_unihan_variant(0x9E35, kTraditionalVariant, 0x9D15).
unicode_unihan_variant(0x9E36, kTraditionalVariant, 0x9DE5).
unicode_unihan_variant(0x9E37, kTraditionalVariant, 0x9DD9).
unicode_unihan_variant(0x9E38, kTraditionalVariant, 0x9D2F).
unicode_unihan_variant(0x9E39, kTraditionalVariant, 0x9D30).
unicode_unihan_variant(0x9E3A, kTraditionalVariant, 0x9D42).
unicode_unihan_variant(0x9E3B, kTraditionalVariant, 0x9D34).
unicode_unihan_variant(0x9E3C, kTraditionalVariant, 0x9D43).
unicode_unihan_variant(0x9E3D, kTraditionalVariant, 0x9D3F).
unicode_unihan_variant(0x9E3E, kTraditionalVariant, 0x9E1E).
unicode_unihan_variant(0x9E3F, kTraditionalVariant, 0x9D3B).
unicode_unihan_variant(0x9E40, kTraditionalVariant, 0x9D50).
unicode_unihan_variant(0x9E41, kTraditionalVariant, 0x9D53).
unicode_unihan_variant(0x9E42, kTraditionalVariant, 0x9E1D).
unicode_unihan_variant(0x9E43, kTraditionalVariant, 0x9D51).
unicode_unihan_variant(0x9E44, kTraditionalVariant, 0x9D60).
unicode_unihan_variant(0x9E45, kTraditionalVariant, 0x9D5D).
unicode_unihan_variant(0x9E46, kTraditionalVariant, 0x9D52).
unicode_unihan_variant(0x9E47, kTraditionalVariant, 0x9DF3).
unicode_unihan_variant(0x9E48, kTraditionalVariant, 0x9D5C).
unicode_unihan_variant(0x9E49, kTraditionalVariant, 0x9D61).
unicode_unihan_variant(0x9E4A, kTraditionalVariant, 0x9D72).
unicode_unihan_variant(0x9E4B, kTraditionalVariant, 0x9D93).
unicode_unihan_variant(0x9E4C, kTraditionalVariant, 0x9D6A).
unicode_unihan_variant(0x9E4D, kTraditionalVariant, 0x9D7E).
unicode_unihan_variant(0x9E4E, kTraditionalVariant, 0x9D6F).
unicode_unihan_variant(0x9E4F, kTraditionalVariant, 0x9D6C).
unicode_unihan_variant(0x9E50, kTraditionalVariant, 0x9D6E).
unicode_unihan_variant(0x9E51, kTraditionalVariant, 0x9D89).
unicode_unihan_variant(0x9E52, kTraditionalVariant, 0x9D8A).
unicode_unihan_variant(0x9E53, kTraditionalVariant, 0x9D77).
unicode_unihan_variant(0x9E54, kTraditionalVariant, 0x9DEB).
unicode_unihan_variant(0x9E55, kTraditionalVariant, 0x9D98).
unicode_unihan_variant(0x9E56, kTraditionalVariant, 0x9DA1).
unicode_unihan_variant(0x9E57, kTraditionalVariant, 0x9D9A).
unicode_unihan_variant(0x9E58, kTraditionalVariant, 0x9DBB).
unicode_unihan_variant(0x9E59, kTraditionalVariant, 0x9D96).
unicode_unihan_variant(0x9E5A, kTraditionalVariant, 0x9DBF).
unicode_unihan_variant(0x9E5B, kTraditionalVariant, 0x9DA5).
unicode_unihan_variant(0x9E5C, kTraditionalVariant, 0x9DA9).
unicode_unihan_variant(0x9E5D, kTraditionalVariant, 0x9DCA).
unicode_unihan_variant(0x9E5E, kTraditionalVariant, 0x9DC2).
unicode_unihan_variant(0x9E5F, kTraditionalVariant, 0x9DB2).
unicode_unihan_variant(0x9E60, kTraditionalVariant, 0x9DB9).
unicode_unihan_variant(0x9E61, kTraditionalVariant, 0x9DBA).
unicode_unihan_variant(0x9E62, kTraditionalVariant, 0x9DC1).
unicode_unihan_variant(0x9E63, kTraditionalVariant, 0x9DBC).
unicode_unihan_variant(0x9E64, kTraditionalVariant, 0x9DB4).
unicode_unihan_variant(0x9E65, kTraditionalVariant, 0x9DD6).
unicode_unihan_variant(0x9E66, kTraditionalVariant, 0x9E1A).
unicode_unihan_variant(0x9E67, kTraditionalVariant, 0x9DD3).
unicode_unihan_variant(0x9E68, kTraditionalVariant, 0x9DDA).
unicode_unihan_variant(0x9E69, kTraditionalVariant, 0x9DEF).
unicode_unihan_variant(0x9E6A, kTraditionalVariant, 0x9DE6).
unicode_unihan_variant(0x9E6B, kTraditionalVariant, 0x9DF2).
unicode_unihan_variant(0x9E6C, kTraditionalVariant, 0x9DF8).
unicode_unihan_variant(0x9E6D, kTraditionalVariant, 0x9DFA).
unicode_unihan_variant(0x9E6E, kTraditionalVariant, 0x4D09).
unicode_unihan_variant(0x9E6F, kTraditionalVariant, 0x9E07).
unicode_unihan_variant(0x9E70, kTraditionalVariant, 0x9DF9).
unicode_unihan_variant(0x9E71, kTraditionalVariant, 0x9E0C).
unicode_unihan_variant(0x9E72, kTraditionalVariant, 0x9E0F).
unicode_unihan_variant(0x9E73, kTraditionalVariant, 0x9E1B).
unicode_unihan_variant(0x9E74, kTraditionalVariant, 0x9E18).
unicode_unihan_variant(0x9E75, kSimplifiedVariant, 0x5364).
unicode_unihan_variant(0x9E78, kZVariant, 0x9E7C).
unicode_unihan_variant(0x9E79, kSimplifiedVariant, 0x54B8).
unicode_unihan_variant(0x9E7A, kSimplifiedVariant, 0x9E7E).
unicode_unihan_variant(0x9E7B, kSemanticVariant, 0x583F). %<kFenn 0x78B1<kLau 0x9E7C<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9E7C, kSemanticVariant, 0x583F). %<kFenn 0x78B1<kLau 0x9E7B<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9E7C, kSimplifiedVariant, 0x7877).
unicode_unihan_variant(0x9E7C, kZVariant, 0x7877).
unicode_unihan_variant(0x9E7D, kSemanticVariant, 0x400B). %<kLau 0x5869<kMatthews 0x25081<kMeyerWempe
unicode_unihan_variant(0x9E7D, kSimplifiedVariant, 0x76D0).
unicode_unihan_variant(0x9E7E, kTraditionalVariant, 0x9E7A).
unicode_unihan_variant(0x9E7F, kZVariant, 0xF940).
unicode_unihan_variant(0x9E81, kZVariant, 0x7C97).
unicode_unihan_variant(0x9E84, kSemanticVariant, 0x7C97). %<kMatthews 0x89D5<kMatthews 0x9EA4<kMatthews
unicode_unihan_variant(0x9E84, kZVariant, 0x7C97).
unicode_unihan_variant(0x9E87, kSemanticVariant, 0x9E95). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9E90, kSemanticVariant, 0x9E9F). %<kMatthews
unicode_unihan_variant(0x9E93, kZVariant, 0x68BA).
unicode_unihan_variant(0x9E95, kSemanticVariant, 0x9E87). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9E97, kSimplifiedVariant, 0x4E3D).
unicode_unihan_variant(0x9E9E, kSemanticVariant, 0x7350). %<kMatthews
unicode_unihan_variant(0x9E9F, kSemanticVariant, 0x9E90). %<kMatthews
unicode_unihan_variant(0x9E9F, kZVariant, 0xF9F3).
unicode_unihan_variant(0x9EA4, kSemanticVariant, 0x7C97). %<kMatthews 0x89D5<kMatthews 0x9E84<kMatthews
unicode_unihan_variant(0x9EA5, kSimplifiedVariant, 0x9EA6).
unicode_unihan_variant(0x9EA6, kTraditionalVariant, 0x9EA5).
unicode_unihan_variant(0x9EA8, kSimplifiedVariant, 0x2A38A).
unicode_unihan_variant(0x9EA9, kSimplifiedVariant, 0x9EB8).
unicode_unihan_variant(0x9EAA, kSemanticVariant, 0x9EB5). %<kMeyerWempe
unicode_unihan_variant(0x9EAA, kZVariant, 0x9EB5).
unicode_unihan_variant(0x9EAB, kSemanticVariant, 0x9EB5). %<kMatthews
unicode_unihan_variant(0x9EAF, kSemanticVariant, 0x9EB4). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9EB0, kSemanticVariant, 0x725F). %<kMatthews
unicode_unihan_variant(0x9EB2, kSimplifiedVariant, 0x2A389).
unicode_unihan_variant(0x9EB4, kSemanticVariant, 0x9EAF). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9EB4, kZVariant, 0x9EB9).
unicode_unihan_variant(0x9EB5, kSemanticVariant, 0x9EAA). %<kMeyerWempe 0x9EAB<kMatthews
unicode_unihan_variant(0x9EB5, kSimplifiedVariant, 0x9762).
unicode_unihan_variant(0x9EB5, kZVariant, 0x9EBA).
unicode_unihan_variant(0x9EB8, kTraditionalVariant, 0x9EA9).
unicode_unihan_variant(0x9EBA, kZVariant, 0x9EB5).
unicode_unihan_variant(0x9EBB, kSemanticVariant, 0x8534). %<kFenn
unicode_unihan_variant(0x9EBB, kSpecializedSemanticVariant, 0x8534). %<kMeyerWempe
unicode_unihan_variant(0x9EBB, kZVariant, 0x83FB).
unicode_unihan_variant(0x9EBC, kSemanticVariant, 0x5E85). %<kMatthews 0x9EBD<kHKGlyph,kLau
unicode_unihan_variant(0x9EBC, kSimplifiedVariant, 0x4E48).
unicode_unihan_variant(0x9EBC, kZVariant, 0x9EBD).
unicode_unihan_variant(0x9EBD, kSemanticVariant, 0x5E85). %<kFenn 0x9EBC<kHKGlyph,kLau
unicode_unihan_variant(0x9EBD, kSimplifiedVariant, 0x4E48).
unicode_unihan_variant(0x9EBD, kZVariant, 0x9EBC).
unicode_unihan_variant(0x9EC3, kSimplifiedVariant, 0x9EC4).
unicode_unihan_variant(0x9EC4, kTraditionalVariant, 0x9EC3).
unicode_unihan_variant(0x9EC9, kTraditionalVariant, 0x9ECC).
unicode_unihan_variant(0x9ECC, kSimplifiedVariant, 0x9EC9).
unicode_unihan_variant(0x9ECE, kZVariant, 0xF989).
unicode_unihan_variant(0x9ECF, kSemanticVariant, 0x7C98). %<kLau
unicode_unihan_variant(0x9ECF, kSpecializedSemanticVariant, 0x7C98). %<kMeyerWempe
unicode_unihan_variant(0x9ED1, kZVariant, 0x9ED2).
unicode_unihan_variant(0x9ED2, kZVariant, 0x9ED1).
unicode_unihan_variant(0x9ED8, kSemanticVariant, 0x563F). %<kFenn 0x9ED9
unicode_unihan_variant(0x9ED8, kZVariant, 0x9ED9).
unicode_unihan_variant(0x9ED9, kSemanticVariant, 0x9ED8).
unicode_unihan_variant(0x9ED9, kZVariant, 0x9ED8).
unicode_unihan_variant(0x9EDE, kSemanticVariant, 0x70B9). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9EDE, kSimplifiedVariant, 0x70B9).
unicode_unihan_variant(0x9EDE, kZVariant, 0x594C).
unicode_unihan_variant(0x9EE1, kTraditionalVariant, 0x9EF6).
unicode_unihan_variant(0x9EE8, kSimplifiedVariant, 0x515A).
unicode_unihan_variant(0x9EE9, kTraditionalVariant, 0x9EF7).
unicode_unihan_variant(0x9EEA, kTraditionalVariant, 0x9EF2).
unicode_unihan_variant(0x9EF2, kSimplifiedVariant, 0x9EEA).
unicode_unihan_variant(0x9EF6, kSimplifiedVariant, 0x9EE1).
unicode_unihan_variant(0x9EF7, kSimplifiedVariant, 0x9EE9).
unicode_unihan_variant(0x9EFD, kSimplifiedVariant, 0x9EFE).
unicode_unihan_variant(0x9EFE, kTraditionalVariant, 0x9EFD).
unicode_unihan_variant(0x9EFF, kSimplifiedVariant, 0x9F0B).
unicode_unihan_variant(0x9F02, kZVariant, 0x9F0C).
unicode_unihan_variant(0x9F03, kSemanticVariant, 0x86D9). %<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9F04, kSemanticVariant, 0x86DB). %<kMatthews
unicode_unihan_variant(0x9F04, kZVariant, 0x86DB).
unicode_unihan_variant(0x9F05, kZVariant, 0x8718).
unicode_unihan_variant(0x9F07, kSemanticVariant, 0x9C32). %<kMatthews
unicode_unihan_variant(0x9F08, kSemanticVariant, 0x9C49). %<kMatthews
unicode_unihan_variant(0x9F08, kZVariant, 0x9C49).
unicode_unihan_variant(0x9F09, kSimplifiedVariant, 0x9F0D).
unicode_unihan_variant(0x9F0B, kTraditionalVariant, 0x9EFF).
unicode_unihan_variant(0x9F0C, kZVariant, 0x9F02).
unicode_unihan_variant(0x9F0D, kTraditionalVariant, 0x9F09).
unicode_unihan_variant(0x9F0E, kSemanticVariant, 0x2A502). %<kFenn
unicode_unihan_variant(0x9F0F, kSemanticVariant, 0x51AA). %<kMeyerWempe 0x5E42<kMeyerWempe
unicode_unihan_variant(0x9F13, kSemanticVariant, 0x76B7). %<kLau,kMatthews 0x9F14<kLau,kMatthews
unicode_unihan_variant(0x9F13, kZVariant, 0x76BC).
unicode_unihan_variant(0x9F14, kSemanticVariant, 0x76B7). %<kLau,kMatthews 0x9F13<kLau,kMatthews
unicode_unihan_variant(0x9F15, kZVariant, 0x51AC).
unicode_unihan_variant(0x9F17, kSemanticVariant, 0x9780). %<kMatthews 0x9789<kMatthews,kMeyerWempe
unicode_unihan_variant(0x9F17, kTraditionalVariant, 0x9780).
unicode_unihan_variant(0x9F20, kZVariant, 0x9F21).
unicode_unihan_variant(0x9F21, kZVariant, 0x9F20).
unicode_unihan_variant(0x9F34, kSimplifiedVariant, 0x9F39).
unicode_unihan_variant(0x9F39, kTraditionalVariant, 0x9F34).
unicode_unihan_variant(0x9F41, kSemanticVariant, 0x2A599). %<kFenn
unicode_unihan_variant(0x9F45, kSemanticVariant, 0x55C5). %<kMeyerWempe
unicode_unihan_variant(0x9F4A, kSemanticVariant, 0x4E9D). %<kMatthews 0x6589<kMatthews
unicode_unihan_variant(0x9F4A, kSimplifiedVariant, 0x9F50).
unicode_unihan_variant(0x9F4A, kSpecializedSemanticVariant, 0x658B). %<kFenn 0x9F4B<kFenn 0x9F50<kFenn
unicode_unihan_variant(0x9F4B, kSemanticVariant, 0x658B). %<kLau,kMatthews
unicode_unihan_variant(0x9F4B, kSimplifiedVariant, 0x658B).
unicode_unihan_variant(0x9F4B, kSpecializedSemanticVariant, 0x9F4A). %<kFenn
unicode_unihan_variant(0x9F4B, kZVariant, 0x658E).
unicode_unihan_variant(0x9F4E, kSemanticVariant, 0x8CEB). %<kMatthews
unicode_unihan_variant(0x9F4E, kSimplifiedVariant, 0x8D4D).
unicode_unihan_variant(0x9F4F, kSimplifiedVariant, 0x9F51).
unicode_unihan_variant(0x9F4F, kZVariant, 0x97F2).
unicode_unihan_variant(0x9F50, kSpecializedSemanticVariant, 0x9F4A). %<kFenn
unicode_unihan_variant(0x9F50, kTraditionalVariant, 0x9F4A).
unicode_unihan_variant(0x9F51, kTraditionalVariant, 0x9F4F).
unicode_unihan_variant(0x9F52, kSimplifiedVariant, 0x9F7F).
unicode_unihan_variant(0x9F52, kZVariant, 0x6B6F).
unicode_unihan_variant(0x9F54, kSimplifiedVariant, 0x9F80).
unicode_unihan_variant(0x9F55, kSimplifiedVariant, 0x9F81).
unicode_unihan_variant(0x9F57, kSimplifiedVariant, 0x9F82).
unicode_unihan_variant(0x9F59, kSimplifiedVariant, 0x9F85).
unicode_unihan_variant(0x9F5C, kSimplifiedVariant, 0x9F87).
unicode_unihan_variant(0x9F5F, kSimplifiedVariant, 0x9F83).
unicode_unihan_variant(0x9F60, kSimplifiedVariant, 0x9F86).
unicode_unihan_variant(0x9F61, kSimplifiedVariant, 0x9F84).
unicode_unihan_variant(0x9F61, kZVariant, 0x9F62).
unicode_unihan_variant(0x9F62, kZVariant, 0x9F61).
unicode_unihan_variant(0x9F66, kSemanticVariant, 0x5543). %<kFenn
unicode_unihan_variant(0x9F66, kSimplifiedVariant, 0x9F88).
unicode_unihan_variant(0x9F67, kSemanticVariant, 0x56D3). %<kLau,kMatthews
unicode_unihan_variant(0x9F67, kSpecializedSemanticVariant, 0x56D3). %<kMeyerWempe
unicode_unihan_variant(0x9F67, kZVariant, 0x5699).
unicode_unihan_variant(0x9F69, kSemanticVariant, 0x54AC). %<kLau,kMatthews
unicode_unihan_variant(0x9F69, kZVariant, 0x54AC).
unicode_unihan_variant(0x9F6A, kSimplifiedVariant, 0x9F8A).
unicode_unihan_variant(0x9F6C, kSimplifiedVariant, 0x9F89).
unicode_unihan_variant(0x9F72, kSimplifiedVariant, 0x9F8B).
unicode_unihan_variant(0x9F76, kSemanticVariant, 0x816D). %<kLau,kMatthews
unicode_unihan_variant(0x9F76, kSimplifiedVariant, 0x816D).
unicode_unihan_variant(0x9F77, kSimplifiedVariant, 0x9F8C).
unicode_unihan_variant(0x9F7F, kTraditionalVariant, 0x9F52).
unicode_unihan_variant(0x9F80, kTraditionalVariant, 0x9F54).
unicode_unihan_variant(0x9F81, kTraditionalVariant, 0x9F55).
unicode_unihan_variant(0x9F82, kTraditionalVariant, 0x9F57).
unicode_unihan_variant(0x9F83, kTraditionalVariant, 0x9F5F).
unicode_unihan_variant(0x9F84, kTraditionalVariant, 0x9F61).
unicode_unihan_variant(0x9F85, kTraditionalVariant, 0x9F59).
unicode_unihan_variant(0x9F86, kTraditionalVariant, 0x9F60).
unicode_unihan_variant(0x9F87, kTraditionalVariant, 0x9F5C).
unicode_unihan_variant(0x9F88, kTraditionalVariant, 0x9F66).
unicode_unihan_variant(0x9F89, kTraditionalVariant, 0x9F6C).
unicode_unihan_variant(0x9F8A, kTraditionalVariant, 0x9F6A).
unicode_unihan_variant(0x9F8B, kTraditionalVariant, 0x9F72).
unicode_unihan_variant(0x9F8C, kTraditionalVariant, 0x9F77).
unicode_unihan_variant(0x9F8D, kSemanticVariant, 0x9F92). %<kHanYu:T
unicode_unihan_variant(0x9F8D, kSimplifiedVariant, 0x9F99).
unicode_unihan_variant(0x9F8D, kSpecializedSemanticVariant, 0x7ADC).
unicode_unihan_variant(0x9F8E, kSemanticVariant, 0x9F90). %<kLau
unicode_unihan_variant(0x9F8E, kSimplifiedVariant, 0x5390).
unicode_unihan_variant(0x9F8E, kZVariant, 0x9F90).
unicode_unihan_variant(0x9F90, kSemanticVariant, 0x9F8E). %<kLau
unicode_unihan_variant(0x9F90, kSimplifiedVariant, 0x5E9E).
unicode_unihan_variant(0x9F90, kZVariant, 0x5390).
unicode_unihan_variant(0x9F91, kSimplifiedVariant, 0x4DAE).
unicode_unihan_variant(0x9F92, kSemanticVariant, 0x9F8D). %<kHanYu
unicode_unihan_variant(0x9F93, kSemanticVariant, 0x4A8A). %<kMatthews
unicode_unihan_variant(0x9F94, kSimplifiedVariant, 0x9F9A).
unicode_unihan_variant(0x9F95, kSimplifiedVariant, 0x9F9B).
unicode_unihan_variant(0x9F99, kTraditionalVariant, 0x9F8D).
unicode_unihan_variant(0x9F9A, kTraditionalVariant, 0x9F94).
unicode_unihan_variant(0x9F9B, kTraditionalVariant, 0x9F95).
unicode_unihan_variant(0x9F9C, kSimplifiedVariant, 0x9F9F).
unicode_unihan_variant(0x9F9D, kSemanticVariant, 0x79CB). %<kMatthews
unicode_unihan_variant(0x9F9D, kZVariant, 0x418B).
unicode_unihan_variant(0x9F9F, kTraditionalVariant, 0x9F9C).
unicode_unihan_variant(0x9FA0, kSemanticVariant, 0x7C65).
unicode_unihan_variant(0x9FA1, kSemanticVariant, 0x5439). %<kMatthews
unicode_unihan_variant(0x9FA2, kSemanticVariant, 0x548A). %<kLau,kMatthews 0x548C<kLau,kMatthews
unicode_unihan_variant(0x9FA2, kSpecializedSemanticVariant, 0x548C). %<kFenn
unicode_unihan_variant(0x9FA2, kZVariant, 0x548C).
unicode_unihan_variant(0x9FA4, kSemanticVariant, 0x8AE7). %<kMatthews
unicode_unihan_variant(0x9FA5, kSemanticVariant, 0x7C72). %<kLau,kMatthews,kMeyerWempe
unicode_unihan_variant(0x9FAD, kSimplifiedVariant, 0x29A0E).
unicode_unihan_variant(0x9FAF, kSimplifiedVariant, 0x28C46).
unicode_unihan_variant(0x9FCC, kSemanticVariant, 0x6DBC).
unicode_unihan_variant(0xF900, kCompatibilityVariant, 0x8C48).
unicode_unihan_variant(0xF900, kZVariant, 0x8C48).
unicode_unihan_variant(0xF901, kCompatibilityVariant, 0x66F4).
unicode_unihan_variant(0xF901, kZVariant, 0x66F4).
unicode_unihan_variant(0xF902, kCompatibilityVariant, 0x8ECA).
unicode_unihan_variant(0xF902, kZVariant, 0x8ECA).
unicode_unihan_variant(0xF903, kCompatibilityVariant, 0x8CC8).
unicode_unihan_variant(0xF903, kZVariant, 0x8CC8).
unicode_unihan_variant(0xF904, kCompatibilityVariant, 0x6ED1).
unicode_unihan_variant(0xF904, kZVariant, 0x6ED1).
unicode_unihan_variant(0xF905, kCompatibilityVariant, 0x4E32).
unicode_unihan_variant(0xF905, kZVariant, 0x4E32).
unicode_unihan_variant(0xF906, kCompatibilityVariant, 0x53E5).
unicode_unihan_variant(0xF906, kZVariant, 0x53E5).
unicode_unihan_variant(0xF907, kCompatibilityVariant, 0x9F9C).
unicode_unihan_variant(0xF907, kZVariant, 0x9F9C).
unicode_unihan_variant(0xF908, kCompatibilityVariant, 0x9F9C).
unicode_unihan_variant(0xF908, kZVariant, 0x9F9C).
unicode_unihan_variant(0xF909, kCompatibilityVariant, 0x5951).
unicode_unihan_variant(0xF909, kZVariant, 0x5951).
unicode_unihan_variant(0xF90A, kCompatibilityVariant, 0x91D1).
unicode_unihan_variant(0xF90A, kZVariant, 0x91D1).
unicode_unihan_variant(0xF90B, kCompatibilityVariant, 0x5587).
unicode_unihan_variant(0xF90B, kZVariant, 0x5587).
unicode_unihan_variant(0xF90C, kCompatibilityVariant, 0x5948).
unicode_unihan_variant(0xF90C, kZVariant, 0x5948).
unicode_unihan_variant(0xF90D, kCompatibilityVariant, 0x61F6).
unicode_unihan_variant(0xF90D, kZVariant, 0x61F6).
unicode_unihan_variant(0xF90E, kCompatibilityVariant, 0x7669).
unicode_unihan_variant(0xF90E, kZVariant, 0x7669).
unicode_unihan_variant(0xF90F, kCompatibilityVariant, 0x7F85).
unicode_unihan_variant(0xF90F, kZVariant, 0x7F85).
unicode_unihan_variant(0xF910, kCompatibilityVariant, 0x863F).
unicode_unihan_variant(0xF910, kZVariant, 0x863F).
unicode_unihan_variant(0xF911, kCompatibilityVariant, 0x87BA).
unicode_unihan_variant(0xF911, kZVariant, 0x87BA).
unicode_unihan_variant(0xF912, kCompatibilityVariant, 0x88F8).
unicode_unihan_variant(0xF912, kZVariant, 0x88F8).
unicode_unihan_variant(0xF913, kCompatibilityVariant, 0x908F).
unicode_unihan_variant(0xF913, kZVariant, 0x908F).
unicode_unihan_variant(0xF914, kCompatibilityVariant, 0x6A02).
unicode_unihan_variant(0xF914, kZVariant, 0x6A02).
unicode_unihan_variant(0xF915, kCompatibilityVariant, 0x6D1B).
unicode_unihan_variant(0xF915, kZVariant, 0x6D1B).
unicode_unihan_variant(0xF916, kCompatibilityVariant, 0x70D9).
unicode_unihan_variant(0xF916, kZVariant, 0x70D9).
unicode_unihan_variant(0xF917, kCompatibilityVariant, 0x73DE).
unicode_unihan_variant(0xF917, kZVariant, 0x73DE).
unicode_unihan_variant(0xF918, kCompatibilityVariant, 0x843D).
unicode_unihan_variant(0xF918, kZVariant, 0x843D).
unicode_unihan_variant(0xF919, kCompatibilityVariant, 0x916A).
unicode_unihan_variant(0xF919, kZVariant, 0x916A).
unicode_unihan_variant(0xF91A, kCompatibilityVariant, 0x99F1).
unicode_unihan_variant(0xF91A, kZVariant, 0x99F1).
unicode_unihan_variant(0xF91B, kCompatibilityVariant, 0x4E82).
unicode_unihan_variant(0xF91B, kZVariant, 0x4E82).
unicode_unihan_variant(0xF91C, kCompatibilityVariant, 0x5375).
unicode_unihan_variant(0xF91C, kZVariant, 0x5375).
unicode_unihan_variant(0xF91D, kCompatibilityVariant, 0x6B04).
unicode_unihan_variant(0xF91D, kZVariant, 0x6B04).
unicode_unihan_variant(0xF91E, kCompatibilityVariant, 0x721B).
unicode_unihan_variant(0xF91E, kZVariant, 0x721B).
unicode_unihan_variant(0xF91F, kCompatibilityVariant, 0x862D).
unicode_unihan_variant(0xF91F, kZVariant, 0x862D).
unicode_unihan_variant(0xF920, kCompatibilityVariant, 0x9E1E).
unicode_unihan_variant(0xF920, kZVariant, 0x9E1E).
unicode_unihan_variant(0xF921, kCompatibilityVariant, 0x5D50).
unicode_unihan_variant(0xF921, kZVariant, 0x5D50).
unicode_unihan_variant(0xF922, kCompatibilityVariant, 0x6FEB).
unicode_unihan_variant(0xF922, kZVariant, 0x6FEB).
unicode_unihan_variant(0xF923, kCompatibilityVariant, 0x85CD).
unicode_unihan_variant(0xF923, kZVariant, 0x85CD).
unicode_unihan_variant(0xF924, kCompatibilityVariant, 0x8964).
unicode_unihan_variant(0xF924, kZVariant, 0x8964).
unicode_unihan_variant(0xF925, kCompatibilityVariant, 0x62C9).
unicode_unihan_variant(0xF925, kZVariant, 0x62C9).
unicode_unihan_variant(0xF926, kCompatibilityVariant, 0x81D8).
unicode_unihan_variant(0xF926, kZVariant, 0x81D8).
unicode_unihan_variant(0xF927, kCompatibilityVariant, 0x881F).
unicode_unihan_variant(0xF927, kZVariant, 0x881F).
unicode_unihan_variant(0xF928, kCompatibilityVariant, 0x5ECA).
unicode_unihan_variant(0xF928, kZVariant, 0x5ECA).
unicode_unihan_variant(0xF929, kCompatibilityVariant, 0x6717).
unicode_unihan_variant(0xF929, kZVariant, 0x6717).
unicode_unihan_variant(0xF92A, kCompatibilityVariant, 0x6D6A).
unicode_unihan_variant(0xF92A, kZVariant, 0x6D6A).
unicode_unihan_variant(0xF92B, kCompatibilityVariant, 0x72FC).
unicode_unihan_variant(0xF92B, kZVariant, 0x72FC).
unicode_unihan_variant(0xF92C, kCompatibilityVariant, 0x90CE).
unicode_unihan_variant(0xF92C, kZVariant, 0x90CE).
unicode_unihan_variant(0xF92D, kCompatibilityVariant, 0x4F86).
unicode_unihan_variant(0xF92D, kZVariant, 0x4F86).
unicode_unihan_variant(0xF92E, kCompatibilityVariant, 0x51B7).
unicode_unihan_variant(0xF92E, kZVariant, 0x51B7).
unicode_unihan_variant(0xF92F, kCompatibilityVariant, 0x52DE).
unicode_unihan_variant(0xF92F, kZVariant, 0x52DE).
unicode_unihan_variant(0xF930, kCompatibilityVariant, 0x64C4).
unicode_unihan_variant(0xF930, kZVariant, 0x64C4).
unicode_unihan_variant(0xF931, kCompatibilityVariant, 0x6AD3).
unicode_unihan_variant(0xF931, kZVariant, 0x6AD3).
unicode_unihan_variant(0xF932, kCompatibilityVariant, 0x7210).
unicode_unihan_variant(0xF932, kZVariant, 0x7210).
unicode_unihan_variant(0xF933, kCompatibilityVariant, 0x76E7).
unicode_unihan_variant(0xF933, kZVariant, 0x76E7).
unicode_unihan_variant(0xF934, kCompatibilityVariant, 0x8001).
unicode_unihan_variant(0xF934, kZVariant, 0x8001).
unicode_unihan_variant(0xF935, kCompatibilityVariant, 0x8606).
unicode_unihan_variant(0xF935, kZVariant, 0x8606).
unicode_unihan_variant(0xF936, kCompatibilityVariant, 0x865C).
unicode_unihan_variant(0xF936, kZVariant, 0x865C).
unicode_unihan_variant(0xF937, kCompatibilityVariant, 0x8DEF).
unicode_unihan_variant(0xF937, kZVariant, 0x8DEF).
unicode_unihan_variant(0xF938, kCompatibilityVariant, 0x9732).
unicode_unihan_variant(0xF938, kZVariant, 0x9732).
unicode_unihan_variant(0xF939, kCompatibilityVariant, 0x9B6F).
unicode_unihan_variant(0xF939, kZVariant, 0x9B6F).
unicode_unihan_variant(0xF93A, kCompatibilityVariant, 0x9DFA).
unicode_unihan_variant(0xF93A, kZVariant, 0x9DFA).
unicode_unihan_variant(0xF93B, kCompatibilityVariant, 0x788C).
unicode_unihan_variant(0xF93B, kZVariant, 0x788C).
unicode_unihan_variant(0xF93C, kCompatibilityVariant, 0x797F).
unicode_unihan_variant(0xF93C, kZVariant, 0x797F).
unicode_unihan_variant(0xF93D, kCompatibilityVariant, 0x7DA0).
unicode_unihan_variant(0xF93D, kZVariant, 0x7DA0).
unicode_unihan_variant(0xF93E, kCompatibilityVariant, 0x83C9).
unicode_unihan_variant(0xF93E, kZVariant, 0x83C9).
unicode_unihan_variant(0xF93F, kCompatibilityVariant, 0x9304).
unicode_unihan_variant(0xF93F, kZVariant, 0x9304).
unicode_unihan_variant(0xF940, kCompatibilityVariant, 0x9E7F).
unicode_unihan_variant(0xF940, kZVariant, 0x9E7F).
unicode_unihan_variant(0xF941, kCompatibilityVariant, 0x8AD6).
unicode_unihan_variant(0xF941, kZVariant, 0x8AD6).
unicode_unihan_variant(0xF942, kCompatibilityVariant, 0x58DF).
unicode_unihan_variant(0xF942, kZVariant, 0x58DF).
unicode_unihan_variant(0xF943, kCompatibilityVariant, 0x5F04).
unicode_unihan_variant(0xF943, kZVariant, 0x5F04).
unicode_unihan_variant(0xF944, kCompatibilityVariant, 0x7C60).
unicode_unihan_variant(0xF944, kZVariant, 0x7C60).
unicode_unihan_variant(0xF945, kCompatibilityVariant, 0x807E).
unicode_unihan_variant(0xF945, kZVariant, 0x807E).
unicode_unihan_variant(0xF946, kCompatibilityVariant, 0x7262).
unicode_unihan_variant(0xF946, kZVariant, 0x7262).
unicode_unihan_variant(0xF947, kCompatibilityVariant, 0x78CA).
unicode_unihan_variant(0xF947, kZVariant, 0x78CA).
unicode_unihan_variant(0xF948, kCompatibilityVariant, 0x8CC2).
unicode_unihan_variant(0xF948, kZVariant, 0x8CC2).
unicode_unihan_variant(0xF949, kCompatibilityVariant, 0x96F7).
unicode_unihan_variant(0xF949, kZVariant, 0x96F7).
unicode_unihan_variant(0xF94A, kCompatibilityVariant, 0x58D8).
unicode_unihan_variant(0xF94A, kZVariant, 0x58D8).
unicode_unihan_variant(0xF94B, kCompatibilityVariant, 0x5C62).
unicode_unihan_variant(0xF94B, kZVariant, 0x5C62).
unicode_unihan_variant(0xF94C, kCompatibilityVariant, 0x6A13).
unicode_unihan_variant(0xF94C, kZVariant, 0x6A13).
unicode_unihan_variant(0xF94D, kCompatibilityVariant, 0x6DDA).
unicode_unihan_variant(0xF94D, kZVariant, 0x6DDA).
unicode_unihan_variant(0xF94E, kCompatibilityVariant, 0x6F0F).
unicode_unihan_variant(0xF94E, kZVariant, 0x6F0F).
unicode_unihan_variant(0xF94F, kCompatibilityVariant, 0x7D2F).
unicode_unihan_variant(0xF94F, kZVariant, 0x7D2F).
unicode_unihan_variant(0xF950, kCompatibilityVariant, 0x7E37).
unicode_unihan_variant(0xF950, kZVariant, 0x7E37).
unicode_unihan_variant(0xF951, kCompatibilityVariant, 0x964B).
unicode_unihan_variant(0xF951, kZVariant, 0x964B).
unicode_unihan_variant(0xF952, kCompatibilityVariant, 0x52D2).
unicode_unihan_variant(0xF952, kZVariant, 0x52D2).
unicode_unihan_variant(0xF953, kCompatibilityVariant, 0x808B).
unicode_unihan_variant(0xF953, kZVariant, 0x808B).
unicode_unihan_variant(0xF954, kCompatibilityVariant, 0x51DC).
unicode_unihan_variant(0xF954, kZVariant, 0x51DC).
unicode_unihan_variant(0xF955, kCompatibilityVariant, 0x51CC).
unicode_unihan_variant(0xF955, kZVariant, 0x51CC).
unicode_unihan_variant(0xF956, kCompatibilityVariant, 0x7A1C).
unicode_unihan_variant(0xF956, kZVariant, 0x7A1C).
unicode_unihan_variant(0xF957, kCompatibilityVariant, 0x7DBE).
unicode_unihan_variant(0xF957, kZVariant, 0x7DBE).
unicode_unihan_variant(0xF958, kCompatibilityVariant, 0x83F1).
unicode_unihan_variant(0xF958, kZVariant, 0x83F1).
unicode_unihan_variant(0xF959, kCompatibilityVariant, 0x9675).
unicode_unihan_variant(0xF959, kZVariant, 0x9675).
unicode_unihan_variant(0xF95A, kCompatibilityVariant, 0x8B80).
unicode_unihan_variant(0xF95A, kZVariant, 0x8B80).
unicode_unihan_variant(0xF95B, kCompatibilityVariant, 0x62CF).
unicode_unihan_variant(0xF95B, kZVariant, 0x62CF).
unicode_unihan_variant(0xF95C, kCompatibilityVariant, 0x6A02).
unicode_unihan_variant(0xF95C, kZVariant, 0x6A02).
unicode_unihan_variant(0xF95D, kCompatibilityVariant, 0x8AFE).
unicode_unihan_variant(0xF95D, kZVariant, 0x8AFE).
unicode_unihan_variant(0xF95E, kCompatibilityVariant, 0x4E39).
unicode_unihan_variant(0xF95E, kZVariant, 0x4E39).
unicode_unihan_variant(0xF95F, kCompatibilityVariant, 0x5BE7).
unicode_unihan_variant(0xF95F, kZVariant, 0x5BE7).
unicode_unihan_variant(0xF960, kCompatibilityVariant, 0x6012).
unicode_unihan_variant(0xF960, kZVariant, 0x6012).
unicode_unihan_variant(0xF961, kCompatibilityVariant, 0x7387).
unicode_unihan_variant(0xF961, kZVariant, 0x7387).
unicode_unihan_variant(0xF962, kCompatibilityVariant, 0x7570).
unicode_unihan_variant(0xF962, kZVariant, 0x7570).
unicode_unihan_variant(0xF963, kCompatibilityVariant, 0x5317).
unicode_unihan_variant(0xF963, kZVariant, 0x5317).
unicode_unihan_variant(0xF964, kCompatibilityVariant, 0x78FB).
unicode_unihan_variant(0xF964, kZVariant, 0x78FB).
unicode_unihan_variant(0xF965, kCompatibilityVariant, 0x4FBF).
unicode_unihan_variant(0xF965, kZVariant, 0x4FBF).
unicode_unihan_variant(0xF966, kCompatibilityVariant, 0x5FA9).
unicode_unihan_variant(0xF966, kZVariant, 0x5FA9).
unicode_unihan_variant(0xF967, kCompatibilityVariant, 0x4E0D).
unicode_unihan_variant(0xF967, kZVariant, 0x4E0D).
unicode_unihan_variant(0xF968, kCompatibilityVariant, 0x6CCC).
unicode_unihan_variant(0xF968, kZVariant, 0x6CCC).
unicode_unihan_variant(0xF969, kCompatibilityVariant, 0x6578).
unicode_unihan_variant(0xF969, kZVariant, 0x6578).
unicode_unihan_variant(0xF96A, kCompatibilityVariant, 0x7D22).
unicode_unihan_variant(0xF96A, kZVariant, 0x7D22).
unicode_unihan_variant(0xF96B, kCompatibilityVariant, 0x53C3).
unicode_unihan_variant(0xF96B, kZVariant, 0x53C3).
unicode_unihan_variant(0xF96C, kCompatibilityVariant, 0x585E).
unicode_unihan_variant(0xF96C, kZVariant, 0x585E).
unicode_unihan_variant(0xF96D, kCompatibilityVariant, 0x7701).
unicode_unihan_variant(0xF96D, kZVariant, 0x7701).
unicode_unihan_variant(0xF96E, kCompatibilityVariant, 0x8449).
unicode_unihan_variant(0xF96E, kZVariant, 0x8449).
unicode_unihan_variant(0xF96F, kCompatibilityVariant, 0x8AAA).
unicode_unihan_variant(0xF96F, kZVariant, 0x8AAA).
unicode_unihan_variant(0xF970, kCompatibilityVariant, 0x6BBA).
unicode_unihan_variant(0xF970, kZVariant, 0x6BBA).
unicode_unihan_variant(0xF971, kCompatibilityVariant, 0x8FB0).
unicode_unihan_variant(0xF971, kZVariant, 0x8FB0).
unicode_unihan_variant(0xF972, kCompatibilityVariant, 0x6C88).
unicode_unihan_variant(0xF972, kZVariant, 0x700B).
unicode_unihan_variant(0xF973, kCompatibilityVariant, 0x62FE).
unicode_unihan_variant(0xF973, kZVariant, 0x5341).
unicode_unihan_variant(0xF974, kCompatibilityVariant, 0x82E5).
unicode_unihan_variant(0xF974, kZVariant, 0x82E5).
unicode_unihan_variant(0xF975, kCompatibilityVariant, 0x63A0).
unicode_unihan_variant(0xF975, kZVariant, 0x63A0).
unicode_unihan_variant(0xF976, kCompatibilityVariant, 0x7565).
unicode_unihan_variant(0xF976, kZVariant, 0x7565).
unicode_unihan_variant(0xF977, kCompatibilityVariant, 0x4EAE).
unicode_unihan_variant(0xF977, kZVariant, 0x4EAE).
unicode_unihan_variant(0xF978, kCompatibilityVariant, 0x5169).
unicode_unihan_variant(0xF978, kZVariant, 0x5169).
unicode_unihan_variant(0xF979, kCompatibilityVariant, 0x51C9).
unicode_unihan_variant(0xF979, kZVariant, 0x6DBC).
unicode_unihan_variant(0xF97A, kCompatibilityVariant, 0x6881).
unicode_unihan_variant(0xF97A, kZVariant, 0x6881).
unicode_unihan_variant(0xF97B, kCompatibilityVariant, 0x7CE7).
unicode_unihan_variant(0xF97B, kZVariant, 0x7CE7).
unicode_unihan_variant(0xF97C, kCompatibilityVariant, 0x826F).
unicode_unihan_variant(0xF97C, kZVariant, 0x826F).
unicode_unihan_variant(0xF97D, kCompatibilityVariant, 0x8AD2).
unicode_unihan_variant(0xF97D, kZVariant, 0x8AD2).
unicode_unihan_variant(0xF97E, kCompatibilityVariant, 0x91CF).
unicode_unihan_variant(0xF97E, kZVariant, 0x91CF).
unicode_unihan_variant(0xF97F, kCompatibilityVariant, 0x52F5).
unicode_unihan_variant(0xF97F, kZVariant, 0x52F5).
unicode_unihan_variant(0xF980, kCompatibilityVariant, 0x5442).
unicode_unihan_variant(0xF980, kZVariant, 0x5442).
unicode_unihan_variant(0xF981, kCompatibilityVariant, 0x5973).
unicode_unihan_variant(0xF981, kZVariant, 0x5973).
unicode_unihan_variant(0xF982, kCompatibilityVariant, 0x5EEC).
unicode_unihan_variant(0xF982, kZVariant, 0x5EEC).
unicode_unihan_variant(0xF983, kCompatibilityVariant, 0x65C5).
unicode_unihan_variant(0xF983, kZVariant, 0x65C5).
unicode_unihan_variant(0xF984, kCompatibilityVariant, 0x6FFE).
unicode_unihan_variant(0xF984, kZVariant, 0x6FFE).
unicode_unihan_variant(0xF985, kCompatibilityVariant, 0x792A).
unicode_unihan_variant(0xF985, kZVariant, 0x792A).
unicode_unihan_variant(0xF986, kCompatibilityVariant, 0x95AD).
unicode_unihan_variant(0xF986, kZVariant, 0x95AD).
unicode_unihan_variant(0xF987, kCompatibilityVariant, 0x9A6A).
unicode_unihan_variant(0xF987, kZVariant, 0x9A6A).
unicode_unihan_variant(0xF988, kCompatibilityVariant, 0x9E97).
unicode_unihan_variant(0xF988, kZVariant, 0x9E97).
unicode_unihan_variant(0xF989, kCompatibilityVariant, 0x9ECE).
unicode_unihan_variant(0xF989, kZVariant, 0x9ECE).
unicode_unihan_variant(0xF98A, kCompatibilityVariant, 0x529B).
unicode_unihan_variant(0xF98A, kZVariant, 0x529B).
unicode_unihan_variant(0xF98B, kCompatibilityVariant, 0x66C6).
unicode_unihan_variant(0xF98B, kZVariant, 0x66C6).
unicode_unihan_variant(0xF98C, kCompatibilityVariant, 0x6B77).
unicode_unihan_variant(0xF98C, kZVariant, 0x6B77).
unicode_unihan_variant(0xF98D, kCompatibilityVariant, 0x8F62).
unicode_unihan_variant(0xF98D, kZVariant, 0x8F62).
unicode_unihan_variant(0xF98E, kCompatibilityVariant, 0x5E74).
unicode_unihan_variant(0xF98E, kZVariant, 0x5E74).
unicode_unihan_variant(0xF98F, kCompatibilityVariant, 0x6190).
unicode_unihan_variant(0xF98F, kZVariant, 0x6190).
unicode_unihan_variant(0xF990, kCompatibilityVariant, 0x6200).
unicode_unihan_variant(0xF990, kZVariant, 0x6200).
unicode_unihan_variant(0xF991, kCompatibilityVariant, 0x649A).
unicode_unihan_variant(0xF991, kZVariant, 0x649A).
unicode_unihan_variant(0xF992, kCompatibilityVariant, 0x6F23).
unicode_unihan_variant(0xF992, kZVariant, 0x6F23).
unicode_unihan_variant(0xF993, kCompatibilityVariant, 0x7149).
unicode_unihan_variant(0xF993, kZVariant, 0x7149).
unicode_unihan_variant(0xF994, kCompatibilityVariant, 0x7489).
unicode_unihan_variant(0xF994, kZVariant, 0x7489).
unicode_unihan_variant(0xF995, kCompatibilityVariant, 0x79CA).
unicode_unihan_variant(0xF995, kZVariant, 0x79CA).
unicode_unihan_variant(0xF996, kCompatibilityVariant, 0x7DF4).
unicode_unihan_variant(0xF996, kZVariant, 0x7DF4).
unicode_unihan_variant(0xF997, kCompatibilityVariant, 0x806F).
unicode_unihan_variant(0xF997, kZVariant, 0x806F).
unicode_unihan_variant(0xF998, kCompatibilityVariant, 0x8F26).
unicode_unihan_variant(0xF998, kZVariant, 0x8F26).
unicode_unihan_variant(0xF999, kCompatibilityVariant, 0x84EE).
unicode_unihan_variant(0xF999, kZVariant, 0x84EE).
unicode_unihan_variant(0xF99A, kCompatibilityVariant, 0x9023).
unicode_unihan_variant(0xF99A, kZVariant, 0x9023).
unicode_unihan_variant(0xF99B, kCompatibilityVariant, 0x934A).
unicode_unihan_variant(0xF99B, kZVariant, 0x934A).
unicode_unihan_variant(0xF99C, kCompatibilityVariant, 0x5217).
unicode_unihan_variant(0xF99C, kZVariant, 0x5217).
unicode_unihan_variant(0xF99D, kCompatibilityVariant, 0x52A3).
unicode_unihan_variant(0xF99D, kZVariant, 0x52A3).
unicode_unihan_variant(0xF99E, kCompatibilityVariant, 0x54BD).
unicode_unihan_variant(0xF99E, kZVariant, 0x54BD).
unicode_unihan_variant(0xF99F, kCompatibilityVariant, 0x70C8).
unicode_unihan_variant(0xF99F, kZVariant, 0x70C8).
unicode_unihan_variant(0xF9A0, kCompatibilityVariant, 0x88C2).
unicode_unihan_variant(0xF9A0, kZVariant, 0x88C2).
unicode_unihan_variant(0xF9A1, kCompatibilityVariant, 0x8AAA).
unicode_unihan_variant(0xF9A1, kZVariant, 0x8AAA).
unicode_unihan_variant(0xF9A2, kCompatibilityVariant, 0x5EC9).
unicode_unihan_variant(0xF9A2, kZVariant, 0x5EC9).
unicode_unihan_variant(0xF9A3, kCompatibilityVariant, 0x5FF5).
unicode_unihan_variant(0xF9A3, kZVariant, 0x5FF5).
unicode_unihan_variant(0xF9A4, kCompatibilityVariant, 0x637B).
unicode_unihan_variant(0xF9A4, kZVariant, 0x637B).
unicode_unihan_variant(0xF9A5, kCompatibilityVariant, 0x6BAE).
unicode_unihan_variant(0xF9A5, kZVariant, 0x6BAE).
unicode_unihan_variant(0xF9A6, kCompatibilityVariant, 0x7C3E).
unicode_unihan_variant(0xF9A6, kZVariant, 0x7C3E).
unicode_unihan_variant(0xF9A7, kCompatibilityVariant, 0x7375).
unicode_unihan_variant(0xF9A7, kZVariant, 0x7375).
unicode_unihan_variant(0xF9A8, kCompatibilityVariant, 0x4EE4).
unicode_unihan_variant(0xF9A8, kZVariant, 0x4EE4).
unicode_unihan_variant(0xF9A9, kCompatibilityVariant, 0x56F9).
unicode_unihan_variant(0xF9A9, kZVariant, 0x56F9).
unicode_unihan_variant(0xF9AA, kCompatibilityVariant, 0x5BE7).
unicode_unihan_variant(0xF9AA, kZVariant, 0x5BE7).
unicode_unihan_variant(0xF9AB, kCompatibilityVariant, 0x5DBA).
unicode_unihan_variant(0xF9AB, kZVariant, 0x5DBA).
unicode_unihan_variant(0xF9AC, kCompatibilityVariant, 0x601C).
unicode_unihan_variant(0xF9AC, kZVariant, 0x601C).
unicode_unihan_variant(0xF9AD, kCompatibilityVariant, 0x73B2).
unicode_unihan_variant(0xF9AD, kZVariant, 0x73B2).
unicode_unihan_variant(0xF9AE, kCompatibilityVariant, 0x7469).
unicode_unihan_variant(0xF9AE, kZVariant, 0x7469).
unicode_unihan_variant(0xF9AF, kCompatibilityVariant, 0x7F9A).
unicode_unihan_variant(0xF9AF, kZVariant, 0x7F9A).
unicode_unihan_variant(0xF9B0, kCompatibilityVariant, 0x8046).
unicode_unihan_variant(0xF9B0, kZVariant, 0x8046).
unicode_unihan_variant(0xF9B1, kCompatibilityVariant, 0x9234).
unicode_unihan_variant(0xF9B1, kZVariant, 0x9234).
unicode_unihan_variant(0xF9B2, kCompatibilityVariant, 0x96F6).
unicode_unihan_variant(0xF9B2, kZVariant, 0x96F6).
unicode_unihan_variant(0xF9B3, kCompatibilityVariant, 0x9748).
unicode_unihan_variant(0xF9B3, kZVariant, 0x9748).
unicode_unihan_variant(0xF9B4, kCompatibilityVariant, 0x9818).
unicode_unihan_variant(0xF9B4, kZVariant, 0x9818).
unicode_unihan_variant(0xF9B5, kCompatibilityVariant, 0x4F8B).
unicode_unihan_variant(0xF9B5, kZVariant, 0x4F8B).
unicode_unihan_variant(0xF9B6, kCompatibilityVariant, 0x79AE).
unicode_unihan_variant(0xF9B6, kZVariant, 0x79AE).
unicode_unihan_variant(0xF9B7, kCompatibilityVariant, 0x91B4).
unicode_unihan_variant(0xF9B7, kZVariant, 0x91B4).
unicode_unihan_variant(0xF9B8, kCompatibilityVariant, 0x96B8).
unicode_unihan_variant(0xF9B8, kZVariant, 0x96B8).
unicode_unihan_variant(0xF9B9, kCompatibilityVariant, 0x60E1).
unicode_unihan_variant(0xF9B9, kZVariant, 0x60E1).
unicode_unihan_variant(0xF9BA, kCompatibilityVariant, 0x4E86).
unicode_unihan_variant(0xF9BA, kZVariant, 0x4E86).
unicode_unihan_variant(0xF9BB, kCompatibilityVariant, 0x50DA).
unicode_unihan_variant(0xF9BB, kZVariant, 0x50DA).
unicode_unihan_variant(0xF9BC, kCompatibilityVariant, 0x5BEE).
unicode_unihan_variant(0xF9BC, kZVariant, 0x5BEE).
unicode_unihan_variant(0xF9BD, kCompatibilityVariant, 0x5C3F).
unicode_unihan_variant(0xF9BD, kZVariant, 0x5C3F).
unicode_unihan_variant(0xF9BE, kCompatibilityVariant, 0x6599).
unicode_unihan_variant(0xF9BE, kZVariant, 0x6599).
unicode_unihan_variant(0xF9BF, kCompatibilityVariant, 0x6A02).
unicode_unihan_variant(0xF9BF, kZVariant, 0x6A02).
unicode_unihan_variant(0xF9C0, kCompatibilityVariant, 0x71CE).
unicode_unihan_variant(0xF9C0, kZVariant, 0x71CE).
unicode_unihan_variant(0xF9C1, kCompatibilityVariant, 0x7642).
unicode_unihan_variant(0xF9C1, kZVariant, 0x7642).
unicode_unihan_variant(0xF9C2, kCompatibilityVariant, 0x84FC).
unicode_unihan_variant(0xF9C2, kZVariant, 0x84FC).
unicode_unihan_variant(0xF9C3, kCompatibilityVariant, 0x907C).
unicode_unihan_variant(0xF9C3, kZVariant, 0x907C).
unicode_unihan_variant(0xF9C4, kCompatibilityVariant, 0x9F8D).
unicode_unihan_variant(0xF9C4, kZVariant, 0x9F8D).
unicode_unihan_variant(0xF9C5, kCompatibilityVariant, 0x6688).
unicode_unihan_variant(0xF9C5, kZVariant, 0x6688).
unicode_unihan_variant(0xF9C6, kCompatibilityVariant, 0x962E).
unicode_unihan_variant(0xF9C6, kZVariant, 0x962E).
unicode_unihan_variant(0xF9C7, kCompatibilityVariant, 0x5289).
unicode_unihan_variant(0xF9C7, kZVariant, 0x5289).
unicode_unihan_variant(0xF9C8, kCompatibilityVariant, 0x677B).
unicode_unihan_variant(0xF9C8, kZVariant, 0x677B).
unicode_unihan_variant(0xF9C9, kCompatibilityVariant, 0x67F3).
unicode_unihan_variant(0xF9C9, kZVariant, 0x67F3).
unicode_unihan_variant(0xF9CA, kCompatibilityVariant, 0x6D41).
unicode_unihan_variant(0xF9CA, kZVariant, 0x6D41).
unicode_unihan_variant(0xF9CB, kCompatibilityVariant, 0x6E9C).
unicode_unihan_variant(0xF9CB, kZVariant, 0x6E9C).
unicode_unihan_variant(0xF9CC, kCompatibilityVariant, 0x7409).
unicode_unihan_variant(0xF9CC, kZVariant, 0x7409).
unicode_unihan_variant(0xF9CD, kCompatibilityVariant, 0x7559).
unicode_unihan_variant(0xF9CD, kZVariant, 0x7559).
unicode_unihan_variant(0xF9CE, kCompatibilityVariant, 0x786B).
unicode_unihan_variant(0xF9CE, kZVariant, 0x786B).
unicode_unihan_variant(0xF9CF, kCompatibilityVariant, 0x7D10).
unicode_unihan_variant(0xF9CF, kZVariant, 0x7D10).
unicode_unihan_variant(0xF9D0, kCompatibilityVariant, 0x985E).
unicode_unihan_variant(0xF9D0, kZVariant, 0x985E).
unicode_unihan_variant(0xF9D1, kCompatibilityVariant, 0x516D).
unicode_unihan_variant(0xF9D1, kZVariant, 0x516D).
unicode_unihan_variant(0xF9D2, kCompatibilityVariant, 0x622E).
unicode_unihan_variant(0xF9D2, kZVariant, 0x622E).
unicode_unihan_variant(0xF9D3, kCompatibilityVariant, 0x9678).
unicode_unihan_variant(0xF9D3, kZVariant, 0x9678).
unicode_unihan_variant(0xF9D4, kCompatibilityVariant, 0x502B).
unicode_unihan_variant(0xF9D4, kZVariant, 0x502B).
unicode_unihan_variant(0xF9D5, kCompatibilityVariant, 0x5D19).
unicode_unihan_variant(0xF9D5, kZVariant, 0x5D19).
unicode_unihan_variant(0xF9D6, kCompatibilityVariant, 0x6DEA).
unicode_unihan_variant(0xF9D6, kZVariant, 0x6DEA).
unicode_unihan_variant(0xF9D7, kCompatibilityVariant, 0x8F2A).
unicode_unihan_variant(0xF9D7, kZVariant, 0x8F2A).
unicode_unihan_variant(0xF9D8, kCompatibilityVariant, 0x5F8B).
unicode_unihan_variant(0xF9D8, kZVariant, 0x5F8B).
unicode_unihan_variant(0xF9D9, kCompatibilityVariant, 0x6144).
unicode_unihan_variant(0xF9D9, kZVariant, 0x6144).
unicode_unihan_variant(0xF9DA, kCompatibilityVariant, 0x6817).
unicode_unihan_variant(0xF9DA, kZVariant, 0x6817).
unicode_unihan_variant(0xF9DB, kCompatibilityVariant, 0x7387).
unicode_unihan_variant(0xF9DB, kZVariant, 0x7387).
unicode_unihan_variant(0xF9DC, kCompatibilityVariant, 0x9686).
unicode_unihan_variant(0xF9DC, kZVariant, 0x9686).
unicode_unihan_variant(0xF9DD, kCompatibilityVariant, 0x5229).
unicode_unihan_variant(0xF9DD, kZVariant, 0x5229).
unicode_unihan_variant(0xF9DE, kCompatibilityVariant, 0x540F).
unicode_unihan_variant(0xF9DE, kZVariant, 0x540F).
unicode_unihan_variant(0xF9DF, kCompatibilityVariant, 0x5C65).
unicode_unihan_variant(0xF9DF, kZVariant, 0x5C65).
unicode_unihan_variant(0xF9E0, kCompatibilityVariant, 0x6613).
unicode_unihan_variant(0xF9E0, kZVariant, 0x6613).
unicode_unihan_variant(0xF9E1, kCompatibilityVariant, 0x674E).
unicode_unihan_variant(0xF9E1, kZVariant, 0x674E).
unicode_unihan_variant(0xF9E2, kCompatibilityVariant, 0x68A8).
unicode_unihan_variant(0xF9E2, kZVariant, 0x68A8).
unicode_unihan_variant(0xF9E3, kCompatibilityVariant, 0x6CE5).
unicode_unihan_variant(0xF9E3, kZVariant, 0x6CE5).
unicode_unihan_variant(0xF9E4, kCompatibilityVariant, 0x7406).
unicode_unihan_variant(0xF9E4, kZVariant, 0x7406).
unicode_unihan_variant(0xF9E5, kCompatibilityVariant, 0x75E2).
unicode_unihan_variant(0xF9E5, kZVariant, 0x75E2).
unicode_unihan_variant(0xF9E6, kCompatibilityVariant, 0x7F79).
unicode_unihan_variant(0xF9E6, kZVariant, 0x7F79).
unicode_unihan_variant(0xF9E7, kCompatibilityVariant, 0x88CF).
unicode_unihan_variant(0xF9E7, kZVariant, 0x88CF).
unicode_unihan_variant(0xF9E8, kCompatibilityVariant, 0x88E1).
unicode_unihan_variant(0xF9E8, kZVariant, 0x88CF).
unicode_unihan_variant(0xF9E9, kCompatibilityVariant, 0x91CC).
unicode_unihan_variant(0xF9E9, kZVariant, 0x91CC).
unicode_unihan_variant(0xF9EA, kCompatibilityVariant, 0x96E2).
unicode_unihan_variant(0xF9EA, kZVariant, 0x96E2).
unicode_unihan_variant(0xF9EB, kCompatibilityVariant, 0x533F).
unicode_unihan_variant(0xF9EB, kZVariant, 0x533F).
unicode_unihan_variant(0xF9EC, kCompatibilityVariant, 0x6EBA).
unicode_unihan_variant(0xF9EC, kZVariant, 0x6EBA).
unicode_unihan_variant(0xF9ED, kCompatibilityVariant, 0x541D).
unicode_unihan_variant(0xF9ED, kZVariant, 0x541D).
unicode_unihan_variant(0xF9EE, kCompatibilityVariant, 0x71D0).
unicode_unihan_variant(0xF9EE, kZVariant, 0x71D0).
unicode_unihan_variant(0xF9EF, kCompatibilityVariant, 0x7498).
unicode_unihan_variant(0xF9EF, kZVariant, 0x7498).
unicode_unihan_variant(0xF9F0, kCompatibilityVariant, 0x85FA).
unicode_unihan_variant(0xF9F0, kZVariant, 0x85FA).
unicode_unihan_variant(0xF9F1, kCompatibilityVariant, 0x96A3).
unicode_unihan_variant(0xF9F1, kZVariant, 0x9130).
unicode_unihan_variant(0xF9F2, kCompatibilityVariant, 0x9C57).
unicode_unihan_variant(0xF9F2, kZVariant, 0x9C57).
unicode_unihan_variant(0xF9F3, kCompatibilityVariant, 0x9E9F).
unicode_unihan_variant(0xF9F3, kZVariant, 0x9E9F).
unicode_unihan_variant(0xF9F4, kCompatibilityVariant, 0x6797).
unicode_unihan_variant(0xF9F4, kZVariant, 0x6797).
unicode_unihan_variant(0xF9F5, kCompatibilityVariant, 0x6DCB).
unicode_unihan_variant(0xF9F5, kZVariant, 0x6DCB).
unicode_unihan_variant(0xF9F6, kCompatibilityVariant, 0x81E8).
unicode_unihan_variant(0xF9F6, kZVariant, 0x81E8).
unicode_unihan_variant(0xF9F7, kCompatibilityVariant, 0x7ACB).
unicode_unihan_variant(0xF9F7, kZVariant, 0x7ACB).
unicode_unihan_variant(0xF9F8, kCompatibilityVariant, 0x7B20).
unicode_unihan_variant(0xF9F8, kZVariant, 0x7B20).
unicode_unihan_variant(0xF9F9, kCompatibilityVariant, 0x7C92).
unicode_unihan_variant(0xF9F9, kZVariant, 0x7C92).
unicode_unihan_variant(0xF9FA, kCompatibilityVariant, 0x72C0).
unicode_unihan_variant(0xF9FA, kZVariant, 0x72C0).
unicode_unihan_variant(0xF9FB, kCompatibilityVariant, 0x7099).
unicode_unihan_variant(0xF9FB, kZVariant, 0x7099).
unicode_unihan_variant(0xF9FC, kCompatibilityVariant, 0x8B58).
unicode_unihan_variant(0xF9FC, kZVariant, 0x8B58).
unicode_unihan_variant(0xF9FD, kCompatibilityVariant, 0x4EC0).
unicode_unihan_variant(0xF9FD, kZVariant, 0x4EC0).
unicode_unihan_variant(0xF9FE, kCompatibilityVariant, 0x8336).
unicode_unihan_variant(0xF9FE, kZVariant, 0x8336).
unicode_unihan_variant(0xF9FF, kCompatibilityVariant, 0x523A).
unicode_unihan_variant(0xF9FF, kZVariant, 0x523A).
unicode_unihan_variant(0xFA00, kCompatibilityVariant, 0x5207).
unicode_unihan_variant(0xFA00, kZVariant, 0x5207).
unicode_unihan_variant(0xFA01, kCompatibilityVariant, 0x5EA6).
unicode_unihan_variant(0xFA01, kZVariant, 0x5EA6).
unicode_unihan_variant(0xFA02, kCompatibilityVariant, 0x62D3).
unicode_unihan_variant(0xFA02, kZVariant, 0x62D3).
unicode_unihan_variant(0xFA03, kCompatibilityVariant, 0x7CD6).
unicode_unihan_variant(0xFA03, kZVariant, 0x7CD6).
unicode_unihan_variant(0xFA04, kCompatibilityVariant, 0x5B85).
unicode_unihan_variant(0xFA04, kZVariant, 0x5B85).
unicode_unihan_variant(0xFA05, kCompatibilityVariant, 0x6D1E).
unicode_unihan_variant(0xFA05, kZVariant, 0x6D1E).
unicode_unihan_variant(0xFA06, kCompatibilityVariant, 0x66B4).
unicode_unihan_variant(0xFA06, kZVariant, 0x66B4).
unicode_unihan_variant(0xFA07, kCompatibilityVariant, 0x8F3B).
unicode_unihan_variant(0xFA07, kZVariant, 0x8F3B).
unicode_unihan_variant(0xFA08, kCompatibilityVariant, 0x884C).
unicode_unihan_variant(0xFA08, kZVariant, 0x884C).
unicode_unihan_variant(0xFA09, kCompatibilityVariant, 0x964D).
unicode_unihan_variant(0xFA09, kZVariant, 0x964D).
unicode_unihan_variant(0xFA0A, kCompatibilityVariant, 0x898B).
unicode_unihan_variant(0xFA0A, kZVariant, 0x898B).
unicode_unihan_variant(0xFA0B, kCompatibilityVariant, 0x5ED3).
unicode_unihan_variant(0xFA0B, kZVariant, 0x5ED3).
unicode_unihan_variant(0xFA0C, kCompatibilityVariant, 0x5140).
unicode_unihan_variant(0xFA0C, kZVariant, 0x5140).
unicode_unihan_variant(0xFA0D, kCompatibilityVariant, 0x55C0).
unicode_unihan_variant(0xFA0D, kZVariant, 0x55C0).
unicode_unihan_variant(0xFA10, kCompatibilityVariant, 0x585A).
unicode_unihan_variant(0xFA10, kZVariant, 0x585A).
unicode_unihan_variant(0xFA12, kCompatibilityVariant, 0x6674).
unicode_unihan_variant(0xFA12, kZVariant, 0x6674).
unicode_unihan_variant(0xFA15, kCompatibilityVariant, 0x51DE).
unicode_unihan_variant(0xFA15, kZVariant, 0x51DE).
unicode_unihan_variant(0xFA16, kCompatibilityVariant, 0x732A).
unicode_unihan_variant(0xFA16, kZVariant, 0x732A).
unicode_unihan_variant(0xFA17, kCompatibilityVariant, 0x76CA).
unicode_unihan_variant(0xFA17, kSemanticVariant, 0x76CA). %<kMeyerWempe
unicode_unihan_variant(0xFA17, kZVariant, 0x76CA).
unicode_unihan_variant(0xFA18, kCompatibilityVariant, 0x793C).
unicode_unihan_variant(0xFA18, kSemanticVariant, 0x793C). %<kMatthews 0x79AE<kMatthews
unicode_unihan_variant(0xFA18, kZVariant, 0x793C).
unicode_unihan_variant(0xFA19, kCompatibilityVariant, 0x795E).
unicode_unihan_variant(0xFA19, kZVariant, 0x795E).
unicode_unihan_variant(0xFA1A, kCompatibilityVariant, 0x7965).
unicode_unihan_variant(0xFA1A, kZVariant, 0x7965).
unicode_unihan_variant(0xFA1B, kCompatibilityVariant, 0x798F).
unicode_unihan_variant(0xFA1B, kZVariant, 0x798F).
unicode_unihan_variant(0xFA1C, kCompatibilityVariant, 0x9756).
unicode_unihan_variant(0xFA1C, kZVariant, 0x9756).
unicode_unihan_variant(0xFA1D, kCompatibilityVariant, 0x7CBE).
unicode_unihan_variant(0xFA1D, kZVariant, 0x7CBE).
unicode_unihan_variant(0xFA1E, kCompatibilityVariant, 0x7FBD).
unicode_unihan_variant(0xFA1E, kZVariant, 0x7FBD).
unicode_unihan_variant(0xFA20, kCompatibilityVariant, 0x8612).
unicode_unihan_variant(0xFA20, kZVariant, 0x8612).
unicode_unihan_variant(0xFA22, kCompatibilityVariant, 0x8AF8).
unicode_unihan_variant(0xFA22, kZVariant, 0x8AF8).
unicode_unihan_variant(0xFA23, kZVariant, 0x27EAF).
unicode_unihan_variant(0xFA25, kCompatibilityVariant, 0x9038).
unicode_unihan_variant(0xFA25, kZVariant, 0x9038).
unicode_unihan_variant(0xFA26, kCompatibilityVariant, 0x90FD).
unicode_unihan_variant(0xFA26, kZVariant, 0x90FD).
unicode_unihan_variant(0xFA2A, kCompatibilityVariant, 0x98EF).
unicode_unihan_variant(0xFA2A, kZVariant, 0x98EF).
unicode_unihan_variant(0xFA2B, kCompatibilityVariant, 0x98FC).
unicode_unihan_variant(0xFA2B, kZVariant, 0x98FC).
unicode_unihan_variant(0xFA2C, kCompatibilityVariant, 0x9928).
unicode_unihan_variant(0xFA2C, kZVariant, 0x9928).
unicode_unihan_variant(0xFA2D, kCompatibilityVariant, 0x9DB4).
unicode_unihan_variant(0xFA2D, kZVariant, 0x9DB4).
unicode_unihan_variant(0xFA30, kCompatibilityVariant, 0x4FAE).
unicode_unihan_variant(0xFA31, kCompatibilityVariant, 0x50E7).
unicode_unihan_variant(0xFA32, kCompatibilityVariant, 0x514D).
unicode_unihan_variant(0xFA33, kCompatibilityVariant, 0x52C9).
unicode_unihan_variant(0xFA34, kCompatibilityVariant, 0x52E4).
unicode_unihan_variant(0xFA35, kCompatibilityVariant, 0x5351).
unicode_unihan_variant(0xFA36, kCompatibilityVariant, 0x559D).
unicode_unihan_variant(0xFA37, kCompatibilityVariant, 0x5606).
unicode_unihan_variant(0xFA38, kCompatibilityVariant, 0x5668).
unicode_unihan_variant(0xFA39, kCompatibilityVariant, 0x5840).
unicode_unihan_variant(0xFA3A, kCompatibilityVariant, 0x58A8).
unicode_unihan_variant(0xFA3B, kCompatibilityVariant, 0x5C64).
unicode_unihan_variant(0xFA3C, kCompatibilityVariant, 0x5C6E).
unicode_unihan_variant(0xFA3D, kCompatibilityVariant, 0x6094).
unicode_unihan_variant(0xFA3E, kCompatibilityVariant, 0x6168).
unicode_unihan_variant(0xFA3F, kCompatibilityVariant, 0x618E).
unicode_unihan_variant(0xFA40, kCompatibilityVariant, 0x61F2).
unicode_unihan_variant(0xFA41, kCompatibilityVariant, 0x654F).
unicode_unihan_variant(0xFA42, kCompatibilityVariant, 0x65E2).
unicode_unihan_variant(0xFA43, kCompatibilityVariant, 0x6691).
unicode_unihan_variant(0xFA44, kCompatibilityVariant, 0x6885).
unicode_unihan_variant(0xFA45, kCompatibilityVariant, 0x6D77).
unicode_unihan_variant(0xFA46, kCompatibilityVariant, 0x6E1A).
unicode_unihan_variant(0xFA47, kCompatibilityVariant, 0x6F22).
unicode_unihan_variant(0xFA48, kCompatibilityVariant, 0x716E).
unicode_unihan_variant(0xFA49, kCompatibilityVariant, 0x722B).
unicode_unihan_variant(0xFA4A, kCompatibilityVariant, 0x7422).
unicode_unihan_variant(0xFA4B, kCompatibilityVariant, 0x7891).
unicode_unihan_variant(0xFA4C, kCompatibilityVariant, 0x793E).
unicode_unihan_variant(0xFA4D, kCompatibilityVariant, 0x7949).
unicode_unihan_variant(0xFA4E, kCompatibilityVariant, 0x7948).
unicode_unihan_variant(0xFA4F, kCompatibilityVariant, 0x7950).
unicode_unihan_variant(0xFA50, kCompatibilityVariant, 0x7956).
unicode_unihan_variant(0xFA51, kCompatibilityVariant, 0x795D).
unicode_unihan_variant(0xFA52, kCompatibilityVariant, 0x798D).
unicode_unihan_variant(0xFA53, kCompatibilityVariant, 0x798E).
unicode_unihan_variant(0xFA54, kCompatibilityVariant, 0x7A40).
unicode_unihan_variant(0xFA55, kCompatibilityVariant, 0x7A81).
unicode_unihan_variant(0xFA56, kCompatibilityVariant, 0x7BC0).
unicode_unihan_variant(0xFA57, kCompatibilityVariant, 0x7DF4).
unicode_unihan_variant(0xFA58, kCompatibilityVariant, 0x7E09).
unicode_unihan_variant(0xFA59, kCompatibilityVariant, 0x7E41).
unicode_unihan_variant(0xFA5A, kCompatibilityVariant, 0x7F72).
unicode_unihan_variant(0xFA5B, kCompatibilityVariant, 0x8005).
unicode_unihan_variant(0xFA5C, kCompatibilityVariant, 0x81ED).
unicode_unihan_variant(0xFA5D, kCompatibilityVariant, 0x8279).
unicode_unihan_variant(0xFA5E, kCompatibilityVariant, 0x8279).
unicode_unihan_variant(0xFA5F, kCompatibilityVariant, 0x8457).
unicode_unihan_variant(0xFA60, kCompatibilityVariant, 0x8910).
unicode_unihan_variant(0xFA61, kCompatibilityVariant, 0x8996).
unicode_unihan_variant(0xFA62, kCompatibilityVariant, 0x8B01).
unicode_unihan_variant(0xFA63, kCompatibilityVariant, 0x8B39).
unicode_unihan_variant(0xFA64, kCompatibilityVariant, 0x8CD3).
unicode_unihan_variant(0xFA65, kCompatibilityVariant, 0x8D08).
unicode_unihan_variant(0xFA66, kCompatibilityVariant, 0x8FB6).
unicode_unihan_variant(0xFA67, kCompatibilityVariant, 0x9038).
unicode_unihan_variant(0xFA68, kCompatibilityVariant, 0x96E3).
unicode_unihan_variant(0xFA69, kCompatibilityVariant, 0x97FF).
unicode_unihan_variant(0xFA6A, kCompatibilityVariant, 0x983B).
unicode_unihan_variant(0xFA6B, kCompatibilityVariant, 0x6075).
unicode_unihan_variant(0xFA6C, kCompatibilityVariant, 0x242EE).
unicode_unihan_variant(0xFA6D, kCompatibilityVariant, 0x8218).
unicode_unihan_variant(0xFA70, kCompatibilityVariant, 0x4E26).
unicode_unihan_variant(0xFA71, kCompatibilityVariant, 0x51B5).
unicode_unihan_variant(0xFA72, kCompatibilityVariant, 0x5168).
unicode_unihan_variant(0xFA73, kCompatibilityVariant, 0x4F80).
unicode_unihan_variant(0xFA74, kCompatibilityVariant, 0x5145).
unicode_unihan_variant(0xFA75, kCompatibilityVariant, 0x5180).
unicode_unihan_variant(0xFA76, kCompatibilityVariant, 0x52C7).
unicode_unihan_variant(0xFA77, kCompatibilityVariant, 0x52FA).
unicode_unihan_variant(0xFA78, kCompatibilityVariant, 0x559D).
unicode_unihan_variant(0xFA79, kCompatibilityVariant, 0x5555).
unicode_unihan_variant(0xFA7A, kCompatibilityVariant, 0x5599).
unicode_unihan_variant(0xFA7B, kCompatibilityVariant, 0x55E2).
unicode_unihan_variant(0xFA7C, kCompatibilityVariant, 0x585A).
unicode_unihan_variant(0xFA7D, kCompatibilityVariant, 0x58B3).
unicode_unihan_variant(0xFA7E, kCompatibilityVariant, 0x5944).
unicode_unihan_variant(0xFA7F, kCompatibilityVariant, 0x5954).
unicode_unihan_variant(0xFA80, kCompatibilityVariant, 0x5A62).
unicode_unihan_variant(0xFA81, kCompatibilityVariant, 0x5B28).
unicode_unihan_variant(0xFA82, kCompatibilityVariant, 0x5ED2).
unicode_unihan_variant(0xFA83, kCompatibilityVariant, 0x5ED9).
unicode_unihan_variant(0xFA84, kCompatibilityVariant, 0x5F69).
unicode_unihan_variant(0xFA85, kCompatibilityVariant, 0x5FAD).
unicode_unihan_variant(0xFA86, kCompatibilityVariant, 0x60D8).
unicode_unihan_variant(0xFA87, kCompatibilityVariant, 0x614E).
unicode_unihan_variant(0xFA88, kCompatibilityVariant, 0x6108).
unicode_unihan_variant(0xFA89, kCompatibilityVariant, 0x618E).
unicode_unihan_variant(0xFA8A, kCompatibilityVariant, 0x6160).
unicode_unihan_variant(0xFA8B, kCompatibilityVariant, 0x61F2).
unicode_unihan_variant(0xFA8C, kCompatibilityVariant, 0x6234).
unicode_unihan_variant(0xFA8D, kCompatibilityVariant, 0x63C4).
unicode_unihan_variant(0xFA8E, kCompatibilityVariant, 0x641C).
unicode_unihan_variant(0xFA8F, kCompatibilityVariant, 0x6452).
unicode_unihan_variant(0xFA90, kCompatibilityVariant, 0x6556).
unicode_unihan_variant(0xFA91, kCompatibilityVariant, 0x6674).
unicode_unihan_variant(0xFA92, kCompatibilityVariant, 0x6717).
unicode_unihan_variant(0xFA93, kCompatibilityVariant, 0x671B).
unicode_unihan_variant(0xFA94, kCompatibilityVariant, 0x6756).
unicode_unihan_variant(0xFA95, kCompatibilityVariant, 0x6B79).
unicode_unihan_variant(0xFA96, kCompatibilityVariant, 0x6BBA).
unicode_unihan_variant(0xFA97, kCompatibilityVariant, 0x6D41).
unicode_unihan_variant(0xFA98, kCompatibilityVariant, 0x6EDB).
unicode_unihan_variant(0xFA99, kCompatibilityVariant, 0x6ECB).
unicode_unihan_variant(0xFA9A, kCompatibilityVariant, 0x6F22).
unicode_unihan_variant(0xFA9B, kCompatibilityVariant, 0x701E).
unicode_unihan_variant(0xFA9C, kCompatibilityVariant, 0x716E).
unicode_unihan_variant(0xFA9D, kCompatibilityVariant, 0x77A7).
unicode_unihan_variant(0xFA9E, kCompatibilityVariant, 0x7235).
unicode_unihan_variant(0xFA9F, kCompatibilityVariant, 0x72AF).
unicode_unihan_variant(0xFAA0, kCompatibilityVariant, 0x732A).
unicode_unihan_variant(0xFAA1, kCompatibilityVariant, 0x7471).
unicode_unihan_variant(0xFAA2, kCompatibilityVariant, 0x7506).
unicode_unihan_variant(0xFAA3, kCompatibilityVariant, 0x753B).
unicode_unihan_variant(0xFAA4, kCompatibilityVariant, 0x761D).
unicode_unihan_variant(0xFAA5, kCompatibilityVariant, 0x761F).
unicode_unihan_variant(0xFAA6, kCompatibilityVariant, 0x76CA).
unicode_unihan_variant(0xFAA7, kCompatibilityVariant, 0x76DB).
unicode_unihan_variant(0xFAA8, kCompatibilityVariant, 0x76F4).
unicode_unihan_variant(0xFAA9, kCompatibilityVariant, 0x774A).
unicode_unihan_variant(0xFAAA, kCompatibilityVariant, 0x7740).
unicode_unihan_variant(0xFAAB, kCompatibilityVariant, 0x78CC).
unicode_unihan_variant(0xFAAC, kCompatibilityVariant, 0x7AB1).
unicode_unihan_variant(0xFAAD, kCompatibilityVariant, 0x7BC0).
unicode_unihan_variant(0xFAAE, kCompatibilityVariant, 0x7C7B).
unicode_unihan_variant(0xFAAF, kCompatibilityVariant, 0x7D5B).
unicode_unihan_variant(0xFAB0, kCompatibilityVariant, 0x7DF4).
unicode_unihan_variant(0xFAB1, kCompatibilityVariant, 0x7F3E).
unicode_unihan_variant(0xFAB2, kCompatibilityVariant, 0x8005).
unicode_unihan_variant(0xFAB3, kCompatibilityVariant, 0x8352).
unicode_unihan_variant(0xFAB4, kCompatibilityVariant, 0x83EF).
unicode_unihan_variant(0xFAB5, kCompatibilityVariant, 0x8779).
unicode_unihan_variant(0xFAB6, kCompatibilityVariant, 0x8941).
unicode_unihan_variant(0xFAB7, kCompatibilityVariant, 0x8986).
unicode_unihan_variant(0xFAB8, kCompatibilityVariant, 0x8996).
unicode_unihan_variant(0xFAB9, kCompatibilityVariant, 0x8ABF).
unicode_unihan_variant(0xFABA, kCompatibilityVariant, 0x8AF8).
unicode_unihan_variant(0xFABB, kCompatibilityVariant, 0x8ACB).
unicode_unihan_variant(0xFABC, kCompatibilityVariant, 0x8B01).
unicode_unihan_variant(0xFABD, kCompatibilityVariant, 0x8AFE).
unicode_unihan_variant(0xFABE, kCompatibilityVariant, 0x8AED).
unicode_unihan_variant(0xFABF, kCompatibilityVariant, 0x8B39).
unicode_unihan_variant(0xFAC0, kCompatibilityVariant, 0x8B8A).
unicode_unihan_variant(0xFAC1, kCompatibilityVariant, 0x8D08).
unicode_unihan_variant(0xFAC2, kCompatibilityVariant, 0x8F38).
unicode_unihan_variant(0xFAC3, kCompatibilityVariant, 0x9072).
unicode_unihan_variant(0xFAC4, kCompatibilityVariant, 0x9199).
unicode_unihan_variant(0xFAC5, kCompatibilityVariant, 0x9276).
unicode_unihan_variant(0xFAC6, kCompatibilityVariant, 0x967C).
unicode_unihan_variant(0xFAC7, kCompatibilityVariant, 0x96E3).
unicode_unihan_variant(0xFAC8, kCompatibilityVariant, 0x9756).
unicode_unihan_variant(0xFAC9, kCompatibilityVariant, 0x97DB).
unicode_unihan_variant(0xFACA, kCompatibilityVariant, 0x97FF).
unicode_unihan_variant(0xFACB, kCompatibilityVariant, 0x980B).
unicode_unihan_variant(0xFACC, kCompatibilityVariant, 0x983B).
unicode_unihan_variant(0xFACD, kCompatibilityVariant, 0x9B12).
unicode_unihan_variant(0xFACE, kCompatibilityVariant, 0x9F9C).
unicode_unihan_variant(0xFACF, kCompatibilityVariant, 0x2284A).
unicode_unihan_variant(0xFAD0, kCompatibilityVariant, 0x22844).
unicode_unihan_variant(0xFAD1, kCompatibilityVariant, 0x233D5).
unicode_unihan_variant(0xFAD2, kCompatibilityVariant, 0x3B9D).
unicode_unihan_variant(0xFAD3, kCompatibilityVariant, 0x4018).
unicode_unihan_variant(0xFAD4, kCompatibilityVariant, 0x4039).
unicode_unihan_variant(0xFAD5, kCompatibilityVariant, 0x25249).
unicode_unihan_variant(0xFAD6, kCompatibilityVariant, 0x25CD0).
unicode_unihan_variant(0xFAD7, kCompatibilityVariant, 0x27ED3).
unicode_unihan_variant(0xFAD8, kCompatibilityVariant, 0x9F43).
unicode_unihan_variant(0xFAD9, kCompatibilityVariant, 0x9F8E).
unicode_unihan_variant(0x2009D, kSemanticVariant, 0x773E). %<kLau 0x8846<kFenn
unicode_unihan_variant(0x200A4, kSemanticVariant, 0x23CE8). %<kLau
unicode_unihan_variant(0x200A4, kSpecializedSemanticVariant, 0x5806). %<kFenn
unicode_unihan_variant(0x2018C, kSemanticVariant, 0x5EB8).
unicode_unihan_variant(0x201B2, kTraditionalVariant, 0x5123).
unicode_unihan_variant(0x201BF, kTraditionalVariant, 0x20325).
unicode_unihan_variant(0x20242, kTraditionalVariant, 0x3493).
unicode_unihan_variant(0x20257, kTraditionalVariant, 0x203E2).
unicode_unihan_variant(0x202A7, kSemanticVariant, 0x4F75). %<kFenn
unicode_unihan_variant(0x20325, kSimplifiedVariant, 0x201BF).
unicode_unihan_variant(0x203E2, kSimplifiedVariant, 0x20257).
unicode_unihan_variant(0x20503, kSemanticVariant, 0x516E). %<kFenn
unicode_unihan_variant(0x20544, kSemanticVariant, 0x51F9). %<kLau
unicode_unihan_variant(0x205A5, kSemanticVariant, 0x5BF5). %<kFenn
unicode_unihan_variant(0x205E6, kSemanticVariant, 0x6191). %<kHanYu,kMeyerWempe
unicode_unihan_variant(0x206B3, kTraditionalVariant, 0x2080E).
unicode_unihan_variant(0x206B9, kSemanticVariant, 0x829F). %<kMeyerWempe
unicode_unihan_variant(0x206C5, kTraditionalVariant, 0x527E).
unicode_unihan_variant(0x206C6, kTraditionalVariant, 0x20786).
unicode_unihan_variant(0x2070E, kSemanticVariant, 0x93FE). %<kMeyerWempe
unicode_unihan_variant(0x2073C, kSemanticVariant, 0x6390). %<kFenn
unicode_unihan_variant(0x20786, kSimplifiedVariant, 0x206C6).
unicode_unihan_variant(0x207B0, kSemanticVariant, 0x527F). %<kFenn
unicode_unihan_variant(0x2080E, kSimplifiedVariant, 0x206B3).
unicode_unihan_variant(0x20B74, kSemanticVariant, 0x3441).
unicode_unihan_variant(0x20BD7, kSemanticVariant, 0x5482). %<kFenn
unicode_unihan_variant(0x20BDF, kTraditionalVariant, 0x54EF).
unicode_unihan_variant(0x20BE0, kTraditionalVariant, 0x5645).
unicode_unihan_variant(0x20C53, kSemanticVariant, 0x20F2E). %<kMeyerWempe
unicode_unihan_variant(0x20C58, kSemanticVariant, 0x20E3A). %<kMeyerWempe
unicode_unihan_variant(0x20CA5, kTraditionalVariant, 0x2114F).
unicode_unihan_variant(0x20CBF, kSemanticVariant, 0x6B36). %<kFenn
unicode_unihan_variant(0x20D22, kTraditionalVariant, 0x21114).
unicode_unihan_variant(0x20D78, kTraditionalVariant, 0x21123).
unicode_unihan_variant(0x20D7E, kTraditionalVariant, 0x35F2).
unicode_unihan_variant(0x20E3A, kSemanticVariant, 0x20C58). %<kMeyerWempe
unicode_unihan_variant(0x20F2E, kSemanticVariant, 0x20C53). %<kMeyerWempe
unicode_unihan_variant(0x21114, kSimplifiedVariant, 0x20D22).
unicode_unihan_variant(0x21123, kSimplifiedVariant, 0x20D78).
unicode_unihan_variant(0x21148, kSemanticVariant, 0x56B9). %<kMeyerWempe
unicode_unihan_variant(0x2114F, kSimplifiedVariant, 0x20CA5).
unicode_unihan_variant(0x21219, kSemanticVariant, 0x56EE). %<kFenn
unicode_unihan_variant(0x212C0, kTraditionalVariant, 0x214FE).
unicode_unihan_variant(0x212D7, kTraditionalVariant, 0x2146D).
unicode_unihan_variant(0x2145E, kSemanticVariant, 0x96A7). %<kLau,kMeyerWempe
unicode_unihan_variant(0x2146D, kSimplifiedVariant, 0x212D7).
unicode_unihan_variant(0x21483, kSemanticVariant, 0x96A9). %<kMeyerWempe
unicode_unihan_variant(0x21484, kTraditionalVariant, 0x58C8).
unicode_unihan_variant(0x214FE, kSimplifiedVariant, 0x212C0).
unicode_unihan_variant(0x21681, kSemanticVariant, 0x5F0A). %<kFenn
unicode_unihan_variant(0x21731, kSemanticVariant, 0x5AC9). %<kFenn
unicode_unihan_variant(0x21760, kTraditionalVariant, 0x3737).
unicode_unihan_variant(0x217B1, kTraditionalVariant, 0x3722).
unicode_unihan_variant(0x217B5, kSimplifiedVariant, 0x36DF).
unicode_unihan_variant(0x217BE, kSemanticVariant, 0x5AE9). %<kMeyerWempe
unicode_unihan_variant(0x217BE, kSpecializedSemanticVariant, 0x5AF0). %<kFenn
unicode_unihan_variant(0x21839, kSimplifiedVariant, 0x36FF).
unicode_unihan_variant(0x21883, kSimplifiedVariant, 0x36E0).
unicode_unihan_variant(0x21A34, kSemanticVariant, 0x5BE7). %<kMeyerWempe
unicode_unihan_variant(0x21B5C, kTraditionalVariant, 0x21B89).
unicode_unihan_variant(0x21B6C, kTraditionalVariant, 0x21BA3).
unicode_unihan_variant(0x21B89, kSimplifiedVariant, 0x21B5C).
unicode_unihan_variant(0x21BA3, kSimplifiedVariant, 0x21B6C).
unicode_unihan_variant(0x21C46, kSemanticVariant, 0x5C5E). %<kFenn 0x5C6C<kFenn
unicode_unihan_variant(0x21C95, kSemanticVariant, 0x5C5C). %<kFenn
unicode_unihan_variant(0x21DB4, kTraditionalVariant, 0x5D7C).
unicode_unihan_variant(0x21ED5, kSemanticVariant, 0x4E97). %<kFenn 0x5C81<kFenn 0x6B72<kLau,kMeyerWempe
unicode_unihan_variant(0x21FB1, kSimplifiedVariant, 0x37DC).
unicode_unihan_variant(0x22091, kSemanticVariant, 0x62ED). %<kFenn
unicode_unihan_variant(0x2214F, kSemanticVariant, 0x3858). %0x7C3E<kLau
unicode_unihan_variant(0x22156, kSemanticVariant, 0x7C3E). %<kMeyerWempe
unicode_unihan_variant(0x2219E, kSemanticVariant, 0x412F). %<kFenn
unicode_unihan_variant(0x2228D, kSemanticVariant, 0x3551). %<kLau 0x53A8<kLau
unicode_unihan_variant(0x2228D, kSpecializedSemanticVariant, 0x53A8).
unicode_unihan_variant(0x222C8, kTraditionalVariant, 0x389D).
unicode_unihan_variant(0x22427, kSemanticVariant, 0x6AA0). %<kMeyerWempe
unicode_unihan_variant(0x22550, kSemanticVariant, 0x6E39). %<kMeyerWempe 0x8A07<kMeyerWempe
unicode_unihan_variant(0x2261D, kTraditionalVariant, 0x228DA).
unicode_unihan_variant(0x2261E, kTraditionalVariant, 0x228ED).
unicode_unihan_variant(0x22653, kTraditionalVariant, 0x61C0).
unicode_unihan_variant(0x22662, kSemanticVariant, 0x6050). %<kLau
unicode_unihan_variant(0x22671, kSemanticVariant, 0x345D). %<kMeyerWempe
unicode_unihan_variant(0x226EF, kTraditionalVariant, 0x398E).
unicode_unihan_variant(0x2272B, kSemanticVariant, 0x60FA). %<kFenn
unicode_unihan_variant(0x22833, kSemanticVariant, 0x618B). %<kMeyerWempe
unicode_unihan_variant(0x22835, kSemanticVariant, 0x60DD). %<kMeyerWempe
unicode_unihan_variant(0x228DA, kSimplifiedVariant, 0x2261D).
unicode_unihan_variant(0x228ED, kSimplifiedVariant, 0x2261E).
unicode_unihan_variant(0x22960, kSemanticVariant, 0x609A). %<kLau,kMeyerWempe
unicode_unihan_variant(0x22ACA, kTraditionalVariant, 0x22DEE).
unicode_unihan_variant(0x22ADE, kTraditionalVariant, 0x22DAB).
unicode_unihan_variant(0x22AEC, kTraditionalVariant, 0x644B).
unicode_unihan_variant(0x22B26, kTraditionalVariant, 0x22E7F).
unicode_unihan_variant(0x22B46, kSemanticVariant, 0x62BD). %<kFenn
unicode_unihan_variant(0x22B4F, kTraditionalVariant, 0x64E3).
unicode_unihan_variant(0x22BF1, kSemanticVariant, 0x641C). %<kHanYu
unicode_unihan_variant(0x22C3E, kSemanticVariant, 0x6460). %<kLau 0x7E3D<kLau
unicode_unihan_variant(0x22CB7, kSemanticVariant, 0x641C). %<kFenn
unicode_unihan_variant(0x22CC6, kSemanticVariant, 0x646E). %<kMeyerWempe
unicode_unihan_variant(0x22CDC, kSemanticVariant, 0x6F07). %<kMeyerWempe
unicode_unihan_variant(0x22D91, kSemanticVariant, 0x22E23).
unicode_unihan_variant(0x22DA3, kSemanticVariant, 0x3A79). %<kMatthews
unicode_unihan_variant(0x22DAB, kSimplifiedVariant, 0x22ADE).
unicode_unihan_variant(0x22DEE, kSimplifiedVariant, 0x22ACA).
unicode_unihan_variant(0x22E23, kSemanticVariant, 0x22D91).
unicode_unihan_variant(0x22E7F, kSimplifiedVariant, 0x22B26).
unicode_unihan_variant(0x22F7E, kTraditionalVariant, 0x6585).
unicode_unihan_variant(0x23040, kZVariant, 0x27959).
unicode_unihan_variant(0x2305A, kSemanticVariant, 0x2305F). %<kHanYu
unicode_unihan_variant(0x2305F, kSemanticVariant, 0x2305A). %<kHanYu
unicode_unihan_variant(0x23190, kTraditionalVariant, 0x66E5).
unicode_unihan_variant(0x2325E, kSemanticVariant, 0x66A0). %<kMeyerWempe
unicode_unihan_variant(0x23368, kTraditionalVariant, 0x26888).
unicode_unihan_variant(0x2336F, kTraditionalVariant, 0x816A).
unicode_unihan_variant(0x23370, kTraditionalVariant, 0x8125).
unicode_unihan_variant(0x23391, kTraditionalVariant, 0x81D7).
unicode_unihan_variant(0x23424, kTraditionalVariant, 0x6B0D).
unicode_unihan_variant(0x23476, kTraditionalVariant, 0x23832).
unicode_unihan_variant(0x234CC, kSemanticVariant, 0x69F9). %<kMeyerWempe
unicode_unihan_variant(0x2353C, kSemanticVariant, 0x7AFF). %<kLau
unicode_unihan_variant(0x235CB, kTraditionalVariant, 0x6B13).
unicode_unihan_variant(0x23613, kTraditionalVariant, 0x237BB).
unicode_unihan_variant(0x23634, kTraditionalVariant, 0x6AAD).
unicode_unihan_variant(0x23637, kTraditionalVariant, 0x23755).
unicode_unihan_variant(0x2364E, kSimplifiedVariant, 0x3B63).
unicode_unihan_variant(0x23755, kSimplifiedVariant, 0x23637).
unicode_unihan_variant(0x237BB, kSimplifiedVariant, 0x23613).
unicode_unihan_variant(0x23832, kSimplifiedVariant, 0x23476).
unicode_unihan_variant(0x23B64, kTraditionalVariant, 0x23BF4).
unicode_unihan_variant(0x23BF4, kSimplifiedVariant, 0x23B64).
unicode_unihan_variant(0x23CE8, kSemanticVariant, 0x200A4). %<kLau
unicode_unihan_variant(0x23DA9, kTraditionalVariant, 0x6F85).
unicode_unihan_variant(0x23DAB, kTraditionalVariant, 0x23FC9).
unicode_unihan_variant(0x23E23, kTraditionalVariant, 0x6FC6).
unicode_unihan_variant(0x23EBC, kTraditionalVariant, 0x7059).
unicode_unihan_variant(0x23EBD, kTraditionalVariant, 0x24063).
unicode_unihan_variant(0x23F77, kTraditionalVariant, 0x7003).
unicode_unihan_variant(0x23F7D, kZVariant, 0x6F78).
unicode_unihan_variant(0x23FB7, kSimplifiedVariant, 0x3CE2).
unicode_unihan_variant(0x23FC9, kSimplifiedVariant, 0x23DAB).
unicode_unihan_variant(0x24063, kSimplifiedVariant, 0x23EBD).
unicode_unihan_variant(0x2418D, kSemanticVariant, 0x8D64). %<kFenn
unicode_unihan_variant(0x241A1, kTraditionalVariant, 0x7193).
unicode_unihan_variant(0x241C3, kTraditionalVariant, 0x7204).
unicode_unihan_variant(0x241C4, kTraditionalVariant, 0x718C).
unicode_unihan_variant(0x24236, kTraditionalVariant, 0x7189).
unicode_unihan_variant(0x24237, kTraditionalVariant, 0x3DFF).
unicode_unihan_variant(0x24280, kTraditionalVariant, 0x2448E).
unicode_unihan_variant(0x242CF, kTraditionalVariant, 0x71A1).
unicode_unihan_variant(0x24360, kSemanticVariant, 0x71B1). %<kLau,kMeyerWempe
unicode_unihan_variant(0x243B1, kSemanticVariant, 0x81C7). %<kFenn
unicode_unihan_variant(0x243B1, kSpecializedSemanticVariant, 0x7E82). %<kFenn
unicode_unihan_variant(0x24455, kSemanticVariant, 0x718F).
unicode_unihan_variant(0x24455, kZVariant, 0x720B).
unicode_unihan_variant(0x2448E, kSimplifiedVariant, 0x24280).
unicode_unihan_variant(0x2456D, kSemanticVariant, 0x5C06). %<kFenn 0x5C07
unicode_unihan_variant(0x2456D, kSpecializedSemanticVariant, 0x5C07). %<kFenn
unicode_unihan_variant(0x2456F, kSemanticVariant, 0x6F3F). %<kFenn
unicode_unihan_variant(0x247A4, kTraditionalVariant, 0x7381).
unicode_unihan_variant(0x2480B, kTraditionalVariant, 0x3E8F).
unicode_unihan_variant(0x24895, kSemanticVariant, 0x6581). %<kMeyerWempe
unicode_unihan_variant(0x248AA, kSemanticVariant, 0x7375). %<kTang
unicode_unihan_variant(0x24980, kTraditionalVariant, 0x74D5).
unicode_unihan_variant(0x249DA, kSemanticVariant, 0x7481). %<kMeyerWempe
unicode_unihan_variant(0x24ABA, kSimplifiedVariant, 0x3ED8).
unicode_unihan_variant(0x24AE9, kSimplifiedVariant, 0x3ECF).
unicode_unihan_variant(0x24BA8, kSemanticVariant, 0x7931). %<kMeyerWempe
unicode_unihan_variant(0x24CC4, kTraditionalVariant, 0x24CF8).
unicode_unihan_variant(0x24CF8, kSimplifiedVariant, 0x24CC4).
unicode_unihan_variant(0x24DA7, kTraditionalVariant, 0x24E2B).
unicode_unihan_variant(0x24E2B, kSimplifiedVariant, 0x24DA7).
unicode_unihan_variant(0x24F6F, kTraditionalVariant, 0x3FE7).
unicode_unihan_variant(0x24F80, kTraditionalVariant, 0x769F).
unicode_unihan_variant(0x25055, kSemanticVariant, 0x6637). %<kFenn
unicode_unihan_variant(0x25081, kSemanticVariant, 0x9E7D). %<kMeyerWempe
unicode_unihan_variant(0x25158, kTraditionalVariant, 0x25303).
unicode_unihan_variant(0x25174, kTraditionalVariant, 0x4039).
unicode_unihan_variant(0x2517E, kSemanticVariant, 0x468E). %<kMeyerWempe
unicode_unihan_variant(0x251A7, kTraditionalVariant, 0x77A4).
unicode_unihan_variant(0x251E2, kTraditionalVariant, 0x406A).
unicode_unihan_variant(0x2521F, kSemanticVariant, 0x778D). %<kHanYu,kMeyerWempe
unicode_unihan_variant(0x252DF, kSemanticVariant, 0x4080).
unicode_unihan_variant(0x25303, kSimplifiedVariant, 0x25158).
unicode_unihan_variant(0x25418, kSemanticVariant, 0x77F3). %<kHanYu
unicode_unihan_variant(0x2541E, kSemanticVariant, 0x786D). %<kMeyerWempe
unicode_unihan_variant(0x2541F, kTraditionalVariant, 0x7912).
unicode_unihan_variant(0x2542F, kTraditionalVariant, 0x25585).
unicode_unihan_variant(0x25430, kTraditionalVariant, 0x25565).
unicode_unihan_variant(0x2543B, kTraditionalVariant, 0x7899).
unicode_unihan_variant(0x254FF, kSemanticVariant, 0x7823). %<kFenn 0x9248<kFenn
unicode_unihan_variant(0x25500, kSemanticVariant, 0x6E39). %<kMeterWempe 0x8A07<kMeyerWempe
unicode_unihan_variant(0x25565, kSimplifiedVariant, 0x25430).
unicode_unihan_variant(0x25585, kSimplifiedVariant, 0x2542F).
unicode_unihan_variant(0x2570C, kSemanticVariant, 0x6FB3). %<kMatthews 0x8956<kMatthews
unicode_unihan_variant(0x25762, kSemanticVariant, 0x5229). %<kLau
unicode_unihan_variant(0x258A2, kSimplifiedVariant, 0x416A).
unicode_unihan_variant(0x25945, kSemanticVariant, 0x7AC9). %<kMeyerWempe
unicode_unihan_variant(0x259C2, kTraditionalVariant, 0x25A10).
unicode_unihan_variant(0x25A10, kSimplifiedVariant, 0x259C2).
unicode_unihan_variant(0x25AF1, kSemanticVariant, 0x56E4). %<kLau
unicode_unihan_variant(0x25B00, kTraditionalVariant, 0x4259).
unicode_unihan_variant(0x25B1E, kTraditionalVariant, 0x7C4B).
unicode_unihan_variant(0x25B20, kTraditionalVariant, 0x7BD8).
unicode_unihan_variant(0x25B49, kTraditionalVariant, 0x25D4A).
unicode_unihan_variant(0x25B8B, kTraditionalVariant, 0x25E20).
unicode_unihan_variant(0x25B9C, kTraditionalVariant, 0x4272).
unicode_unihan_variant(0x25C54, kTraditionalVariant, 0x25D43).
unicode_unihan_variant(0x25D43, kSimplifiedVariant, 0x25C54).
unicode_unihan_variant(0x25D4A, kSimplifiedVariant, 0x25B49).
unicode_unihan_variant(0x25D5A, kSemanticVariant, 0x7AFB). %<kMeyerWempe
unicode_unihan_variant(0x25E20, kSimplifiedVariant, 0x25B8B).
unicode_unihan_variant(0x25E65, kTraditionalVariant, 0x25F3D).
unicode_unihan_variant(0x25E85, kTraditionalVariant, 0x42AD).
unicode_unihan_variant(0x25E87, kTraditionalVariant, 0x25F56).
unicode_unihan_variant(0x25EDF, kSemanticVariant, 0x7CEF). %<kLau
unicode_unihan_variant(0x25F3D, kSimplifiedVariant, 0x25E65).
unicode_unihan_variant(0x25F56, kSimplifiedVariant, 0x25E87).
unicode_unihan_variant(0x25FCA, kSimplifiedVariant, 0x26208).
unicode_unihan_variant(0x26007, kSemanticVariant, 0x7D93). %<kHanYu
unicode_unihan_variant(0x26085, kSimplifiedVariant, 0x26212).
unicode_unihan_variant(0x260C4, kSimplifiedVariant, 0x26217).
unicode_unihan_variant(0x26208, kTraditionalVariant, 0x25FCA).
unicode_unihan_variant(0x26209, kTraditionalVariant, 0x7DF7).
unicode_unihan_variant(0x2620B, kTraditionalVariant, 0x7D87).
unicode_unihan_variant(0x2620C, kTraditionalVariant, 0x7D80).
unicode_unihan_variant(0x2620E, kTraditionalVariant, 0x7E5F).
unicode_unihan_variant(0x2620F, kTraditionalVariant, 0x7DCD).
unicode_unihan_variant(0x26210, kTraditionalVariant, 0x7E3A).
unicode_unihan_variant(0x26211, kTraditionalVariant, 0x7DF8).
unicode_unihan_variant(0x26212, kTraditionalVariant, 0x26085).
unicode_unihan_variant(0x26213, kTraditionalVariant, 0x42FF).
unicode_unihan_variant(0x26214, kTraditionalVariant, 0x7E0E).
unicode_unihan_variant(0x26215, kTraditionalVariant, 0x7DF0).
unicode_unihan_variant(0x26216, kTraditionalVariant, 0x4308).
unicode_unihan_variant(0x26217, kTraditionalVariant, 0x260C4).
unicode_unihan_variant(0x26218, kTraditionalVariant, 0x430B).
unicode_unihan_variant(0x26219, kTraditionalVariant, 0x4330).
unicode_unihan_variant(0x2621A, kTraditionalVariant, 0x7E2C).
unicode_unihan_variant(0x2621B, kTraditionalVariant, 0x7E53).
unicode_unihan_variant(0x2621C, kTraditionalVariant, 0x4316).
unicode_unihan_variant(0x2621D, kTraditionalVariant, 0x7E4F).
unicode_unihan_variant(0x2621E, kTraditionalVariant, 0x431F).
unicode_unihan_variant(0x2621F, kTraditionalVariant, 0x431D).
unicode_unihan_variant(0x26220, kTraditionalVariant, 0x4325).
unicode_unihan_variant(0x26221, kTraditionalVariant, 0x7E7B).
unicode_unihan_variant(0x26369, kSemanticVariant, 0x7F96). %<kMeyerWempe
unicode_unihan_variant(0x26552, kSemanticVariant, 0x4993). %<kMeyerWempe 0x8998<kMeyerWempe
unicode_unihan_variant(0x26675, kSemanticVariant, 0x8155). %<kFenn
unicode_unihan_variant(0x26676, kSemanticVariant, 0x80A1). %<kLau,kMeyerWempe
unicode_unihan_variant(0x2667C, kSemanticVariant, 0x9AB4). %<kMeyerWempe
unicode_unihan_variant(0x26693, kSemanticVariant, 0x80D3).
unicode_unihan_variant(0x266E8, kTraditionalVariant, 0x6725).
unicode_unihan_variant(0x2677C, kTraditionalVariant, 0x81A2).
unicode_unihan_variant(0x267A3, kSemanticVariant, 0x81CA). %<kHanYu:T
unicode_unihan_variant(0x267D7, kTraditionalVariant, 0x268CE).
unicode_unihan_variant(0x267E4, kSemanticVariant, 0x9948). %<kFenn
unicode_unihan_variant(0x26888, kSimplifiedVariant, 0x23368).
unicode_unihan_variant(0x2688C, kSemanticVariant, 0x6BB0). %<kMeyerWempe
unicode_unihan_variant(0x268CE, kSimplifiedVariant, 0x267D7).
unicode_unihan_variant(0x2690E, kSemanticVariant, 0x81EF). %<kMeyerWempe
unicode_unihan_variant(0x26A29, kTraditionalVariant, 0x26ABD).
unicode_unihan_variant(0x26ABD, kSimplifiedVariant, 0x26A29).
unicode_unihan_variant(0x26C34, kTraditionalVariant, 0x4573).
unicode_unihan_variant(0x26C44, kSemanticVariant, 0x83C7). %<kLau
unicode_unihan_variant(0x26CB7, kSemanticVariant, 0x849E). %<kLau
unicode_unihan_variant(0x26DDD, kSemanticVariant, 0x82BB).
unicode_unihan_variant(0x26F4A, kSemanticVariant, 0x9A4A). %<kMeyerWempe
unicode_unihan_variant(0x2725E, kTraditionalVariant, 0x45FF).
unicode_unihan_variant(0x272CD, kSemanticVariant, 0x8707). %<kFenn
unicode_unihan_variant(0x27313, kSemanticVariant, 0x86D7). %<kHanYu
unicode_unihan_variant(0x27405, kSemanticVariant, 0x86C6). %<kMeyerWempe
unicode_unihan_variant(0x27422, kSemanticVariant, 0x8814). %<kMeyerWempe
unicode_unihan_variant(0x274AD, kTraditionalVariant, 0x27525).
unicode_unihan_variant(0x27525, kSimplifiedVariant, 0x274AD).
unicode_unihan_variant(0x27602, kSemanticVariant, 0x885D). %<kFenn
unicode_unihan_variant(0x27640, kSemanticVariant, 0x277AA). %<kMatthews
unicode_unihan_variant(0x27717, kSimplifiedVariant, 0x461E).
unicode_unihan_variant(0x27735, kSimplifiedVariant, 0x464A).
unicode_unihan_variant(0x2775E, kSimplifiedVariant, 0x461B).
unicode_unihan_variant(0x277AA, kSemanticVariant, 0x27640). %<kMatthews
unicode_unihan_variant(0x27959, kZVariant, 0x23040).
unicode_unihan_variant(0x27A59, kSemanticVariant, 0x8A95). %<kLau,kMeyerWempe
unicode_unihan_variant(0x27A59, kSimplifiedVariant, 0x4725).
unicode_unihan_variant(0x27B28, kSemanticVariant, 0x5666). %<kMeyerWempe
unicode_unihan_variant(0x27BAA, kTraditionalVariant, 0x8A40).
unicode_unihan_variant(0x27C0A, kSemanticVariant, 0x78CC). %<kMeyerWempe
unicode_unihan_variant(0x27CD5, kTraditionalVariant, 0x27CDF).
unicode_unihan_variant(0x27CDF, kSimplifiedVariant, 0x27CD5).
unicode_unihan_variant(0x27D2F, kSemanticVariant, 0x5B6D). %<kMeyerWempe
unicode_unihan_variant(0x27D73, kSimplifiedVariant, 0x478C).
unicode_unihan_variant(0x27D94, kSimplifiedVariant, 0x27E53).
unicode_unihan_variant(0x27DA7, kSimplifiedVariant, 0x478E).
unicode_unihan_variant(0x27DB6, kSemanticVariant, 0x8CA9). %<kFenn
unicode_unihan_variant(0x27E16, kSemanticVariant, 0x8D03). %<kFenn
unicode_unihan_variant(0x27E16, kSpecializedSemanticVariant, 0x8CFA). %<kFenn
unicode_unihan_variant(0x27E51, kTraditionalVariant, 0x4788).
unicode_unihan_variant(0x27E53, kTraditionalVariant, 0x27D94).
unicode_unihan_variant(0x27E55, kTraditionalVariant, 0x477B).
unicode_unihan_variant(0x27E56, kTraditionalVariant, 0x8CDF).
unicode_unihan_variant(0x27E57, kTraditionalVariant, 0x8D03).
unicode_unihan_variant(0x27EAF, kZVariant, 0xFA23).
unicode_unihan_variant(0x27FC8, kTraditionalVariant, 0x281C1).
unicode_unihan_variant(0x27FF9, kSemanticVariant, 0x62C7). %<kMeyerWempe
unicode_unihan_variant(0x28031, kTraditionalVariant, 0x28123).
unicode_unihan_variant(0x28074, kTraditionalVariant, 0x2814D).
unicode_unihan_variant(0x280BA, kTraditionalVariant, 0x2820A).
unicode_unihan_variant(0x280DE, kSemanticVariant, 0x280DF). %<kFenn
unicode_unihan_variant(0x280DF, kSemanticVariant, 0x280DE). %<kFenn
unicode_unihan_variant(0x28104, kTraditionalVariant, 0x2820C).
unicode_unihan_variant(0x28123, kSimplifiedVariant, 0x28031).
unicode_unihan_variant(0x2814D, kSimplifiedVariant, 0x28074).
unicode_unihan_variant(0x2816B, kTraditionalVariant, 0x281DE).
unicode_unihan_variant(0x2816C, kTraditionalVariant, 0x8E9D).
unicode_unihan_variant(0x281C1, kSimplifiedVariant, 0x27FC8).
unicode_unihan_variant(0x281DE, kSimplifiedVariant, 0x2816B).
unicode_unihan_variant(0x2820A, kSimplifiedVariant, 0x280BA).
unicode_unihan_variant(0x2820C, kSimplifiedVariant, 0x28104).
unicode_unihan_variant(0x2821A, kSemanticVariant, 0x28249). %<kMeyerWempe
unicode_unihan_variant(0x28249, kSemanticVariant, 0x2821A). %<kMeyerWempe
unicode_unihan_variant(0x28257, kTraditionalVariant, 0x8EC9).
unicode_unihan_variant(0x282B0, kSimplifiedVariant, 0x4880).
unicode_unihan_variant(0x282B8, kSimplifiedVariant, 0x4881).
unicode_unihan_variant(0x282BB, kSimplifiedVariant, 0x28406).
unicode_unihan_variant(0x282E2, kSimplifiedVariant, 0x4882).
unicode_unihan_variant(0x28329, kSpecializedSemanticVariant, 0x76EA). %<kFenn 0x903F<kFenn
unicode_unihan_variant(0x28373, kSemanticVariant, 0x8EFA). %<kMeyerWempe
unicode_unihan_variant(0x283AE, kSimplifiedVariant, 0x28409).
unicode_unihan_variant(0x283E0, kSimplifiedVariant, 0x28407).
unicode_unihan_variant(0x283E5, kSimplifiedVariant, 0x2840A).
unicode_unihan_variant(0x28405, kTraditionalVariant, 0x8ED7).
unicode_unihan_variant(0x28406, kTraditionalVariant, 0x282BB).
unicode_unihan_variant(0x28407, kTraditionalVariant, 0x283E0).
unicode_unihan_variant(0x28408, kTraditionalVariant, 0x8F04).
unicode_unihan_variant(0x28409, kTraditionalVariant, 0x283AE).
unicode_unihan_variant(0x2840A, kTraditionalVariant, 0x283E5).
unicode_unihan_variant(0x28452, kSemanticVariant, 0x5F92). %<kFenn
unicode_unihan_variant(0x28479, kTraditionalVariant, 0x48A8).
unicode_unihan_variant(0x288A5, kSemanticVariant, 0x490D).
unicode_unihan_variant(0x288E6, kSemanticVariant, 0x85C7). %<kMeyerWempe
unicode_unihan_variant(0x28930, kTraditionalVariant, 0x2893B).
unicode_unihan_variant(0x2893B, kSimplifiedVariant, 0x28930).
unicode_unihan_variant(0x2895B, kSimplifiedVariant, 0x28C40).
unicode_unihan_variant(0x289AB, kSimplifiedVariant, 0x4980).
unicode_unihan_variant(0x289DC, kSimplifiedVariant, 0x4981).
unicode_unihan_variant(0x289F1, kSimplifiedVariant, 0x28C4A).
unicode_unihan_variant(0x28AD2, kSimplifiedVariant, 0x28C50).
unicode_unihan_variant(0x28B82, kSimplifiedVariant, 0x28C55).
unicode_unihan_variant(0x28BC5, kSimplifiedVariant, 0x497F).
unicode_unihan_variant(0x28C3E, kTraditionalVariant, 0x93B7).
unicode_unihan_variant(0x28C3F, kTraditionalVariant, 0x91F3).
unicode_unihan_variant(0x28C40, kTraditionalVariant, 0x2895B).
unicode_unihan_variant(0x28C41, kTraditionalVariant, 0x9220).
unicode_unihan_variant(0x28C42, kTraditionalVariant, 0x920B).
unicode_unihan_variant(0x28C43, kTraditionalVariant, 0x9232).
unicode_unihan_variant(0x28C44, kTraditionalVariant, 0x922F).
unicode_unihan_variant(0x28C45, kTraditionalVariant, 0x9241).
unicode_unihan_variant(0x28C46, kTraditionalVariant, 0x9FAF).
unicode_unihan_variant(0x28C47, kTraditionalVariant, 0x92B6).
unicode_unihan_variant(0x28C48, kTraditionalVariant, 0x92C9).
unicode_unihan_variant(0x28C49, kTraditionalVariant, 0x9344).
unicode_unihan_variant(0x28C4A, kTraditionalVariant, 0x289F1).
unicode_unihan_variant(0x28C4B, kTraditionalVariant, 0x9302).
unicode_unihan_variant(0x28C4C, kTraditionalVariant, 0x93C6).
unicode_unihan_variant(0x28C4D, kTraditionalVariant, 0x93AF).
unicode_unihan_variant(0x28C4E, kTraditionalVariant, 0x936E).
unicode_unihan_variant(0x28C4F, kTraditionalVariant, 0x939D).
unicode_unihan_variant(0x28C50, kTraditionalVariant, 0x28AD2).
unicode_unihan_variant(0x28C51, kTraditionalVariant, 0x9404).
unicode_unihan_variant(0x28C52, kTraditionalVariant, 0x93C9).
unicode_unihan_variant(0x28C53, kTraditionalVariant, 0x940E).
unicode_unihan_variant(0x28C54, kTraditionalVariant, 0x940F).
unicode_unihan_variant(0x28C55, kTraditionalVariant, 0x28B82).
unicode_unihan_variant(0x28C56, kTraditionalVariant, 0x4969).
unicode_unihan_variant(0x28CD1, kSimplifiedVariant, 0x28E01).
unicode_unihan_variant(0x28CD5, kSimplifiedVariant, 0x28E00).
unicode_unihan_variant(0x28D17, kSimplifiedVariant, 0x28E05).
unicode_unihan_variant(0x28D69, kSimplifiedVariant, 0x28E06).
unicode_unihan_variant(0x28D78, kSimplifiedVariant, 0x28E07).
unicode_unihan_variant(0x28D80, kSimplifiedVariant, 0x28E09).
unicode_unihan_variant(0x28D8F, kSimplifiedVariant, 0x28E0A).
unicode_unihan_variant(0x28DAE, kSimplifiedVariant, 0x28E0C).
unicode_unihan_variant(0x28DB2, kSimplifiedVariant, 0x28E0B).
unicode_unihan_variant(0x28DB9, kSemanticVariant, 0x95A2). %<kLau
unicode_unihan_variant(0x28DF2, kSimplifiedVariant, 0x28E0E).
unicode_unihan_variant(0x28DFF, kTraditionalVariant, 0x49B3).
unicode_unihan_variant(0x28E00, kTraditionalVariant, 0x28CD5).
unicode_unihan_variant(0x28E01, kTraditionalVariant, 0x28CD1).
unicode_unihan_variant(0x28E02, kTraditionalVariant, 0x958D).
unicode_unihan_variant(0x28E03, kTraditionalVariant, 0x9590).
unicode_unihan_variant(0x28E04, kTraditionalVariant, 0x4998).
unicode_unihan_variant(0x28E05, kTraditionalVariant, 0x28D17).
unicode_unihan_variant(0x28E06, kTraditionalVariant, 0x28D69).
unicode_unihan_variant(0x28E07, kTraditionalVariant, 0x28D78).
unicode_unihan_variant(0x28E09, kTraditionalVariant, 0x28D80).
unicode_unihan_variant(0x28E0A, kTraditionalVariant, 0x28D8F).
unicode_unihan_variant(0x28E0B, kTraditionalVariant, 0x28DB2).
unicode_unihan_variant(0x28E0C, kTraditionalVariant, 0x28DAE).
unicode_unihan_variant(0x28E0E, kTraditionalVariant, 0x28DF2).
unicode_unihan_variant(0x28E18, kTraditionalVariant, 0x28F4F).
unicode_unihan_variant(0x28E1A, kSemanticVariant, 0x7D1A). %<kFenn
unicode_unihan_variant(0x28E1F, kTraditionalVariant, 0x49E2).
unicode_unihan_variant(0x28E2C, kSemanticVariant, 0x9663). %<kLau
unicode_unihan_variant(0x28EF2, kSemanticVariant, 0x7F45). %<kFenn
unicode_unihan_variant(0x28F4F, kSimplifiedVariant, 0x28E18).
unicode_unihan_variant(0x292D8, kSemanticVariant, 0x978B). %<kMeyerWempe
unicode_unihan_variant(0x292E7, kSpecializedSemanticVariant, 0x978B).
unicode_unihan_variant(0x293A2, kSimplifiedVariant, 0x293FE).
unicode_unihan_variant(0x293EA, kSimplifiedVariant, 0x293FD).
unicode_unihan_variant(0x293FC, kTraditionalVariant, 0x4A8F).
unicode_unihan_variant(0x293FD, kTraditionalVariant, 0x293EA).
unicode_unihan_variant(0x293FE, kTraditionalVariant, 0x293A2).
unicode_unihan_variant(0x293FF, kTraditionalVariant, 0x4A98).
unicode_unihan_variant(0x29400, kTraditionalVariant, 0x4A97).
unicode_unihan_variant(0x294E3, kSimplifiedVariant, 0x29595).
unicode_unihan_variant(0x29517, kSemanticVariant, 0x985E). %<kLau,kMeyerWempe
unicode_unihan_variant(0x29595, kTraditionalVariant, 0x294E3).
unicode_unihan_variant(0x29596, kTraditionalVariant, 0x9843).
unicode_unihan_variant(0x29597, kTraditionalVariant, 0x4AF4).
unicode_unihan_variant(0x295C0, kSimplifiedVariant, 0x29666).
unicode_unihan_variant(0x29600, kSimplifiedVariant, 0x29669).
unicode_unihan_variant(0x2961D, kSimplifiedVariant, 0x2966D).
unicode_unihan_variant(0x29639, kSimplifiedVariant, 0x29668).
unicode_unihan_variant(0x2963A, kSimplifiedVariant, 0x2966C).
unicode_unihan_variant(0x29648, kSimplifiedVariant, 0x29670).
unicode_unihan_variant(0x29665, kTraditionalVariant, 0x98B0).
unicode_unihan_variant(0x29666, kTraditionalVariant, 0x295C0).
unicode_unihan_variant(0x29667, kTraditionalVariant, 0x4B1E).
unicode_unihan_variant(0x29668, kTraditionalVariant, 0x29639).
unicode_unihan_variant(0x29669, kTraditionalVariant, 0x29600).
unicode_unihan_variant(0x2966A, kTraditionalVariant, 0x98B7).
unicode_unihan_variant(0x2966B, kTraditionalVariant, 0x98BE).
unicode_unihan_variant(0x2966C, kTraditionalVariant, 0x2963A).
unicode_unihan_variant(0x2966D, kTraditionalVariant, 0x2961D).
unicode_unihan_variant(0x2966E, kTraditionalVariant, 0x4B18).
unicode_unihan_variant(0x2966F, kTraditionalVariant, 0x4B1D).
unicode_unihan_variant(0x29670, kTraditionalVariant, 0x29648).
unicode_unihan_variant(0x2969B, kSimplifiedVariant, 0x297FF).
unicode_unihan_variant(0x296A5, kSimplifiedVariant, 0x29800).
unicode_unihan_variant(0x296B5, kSimplifiedVariant, 0x29801).
unicode_unihan_variant(0x296C6, kSimplifiedVariant, 0x29802).
unicode_unihan_variant(0x296E9, kSimplifiedVariant, 0x29803).
unicode_unihan_variant(0x29707, kSimplifiedVariant, 0x29809).
unicode_unihan_variant(0x29726, kSimplifiedVariant, 0x29806).
unicode_unihan_variant(0x29735, kSimplifiedVariant, 0x2980A).
unicode_unihan_variant(0x29754, kSimplifiedVariant, 0x2980B).
unicode_unihan_variant(0x29784, kSimplifiedVariant, 0x2980E).
unicode_unihan_variant(0x297A6, kSimplifiedVariant, 0x2980F).
unicode_unihan_variant(0x297AF, kSimplifiedVariant, 0x4B6A).
unicode_unihan_variant(0x297D0, kSimplifiedVariant, 0x29805).
unicode_unihan_variant(0x297FF, kTraditionalVariant, 0x2969B).
unicode_unihan_variant(0x29800, kTraditionalVariant, 0x296A5).
unicode_unihan_variant(0x29801, kTraditionalVariant, 0x296B5).
unicode_unihan_variant(0x29802, kTraditionalVariant, 0x296C6).
unicode_unihan_variant(0x29803, kTraditionalVariant, 0x296E9).
unicode_unihan_variant(0x29805, kTraditionalVariant, 0x297D0).
unicode_unihan_variant(0x29806, kTraditionalVariant, 0x29726).
unicode_unihan_variant(0x29807, kTraditionalVariant, 0x4B40).
unicode_unihan_variant(0x29808, kTraditionalVariant, 0x4B43).
unicode_unihan_variant(0x29809, kTraditionalVariant, 0x29707).
unicode_unihan_variant(0x2980A, kTraditionalVariant, 0x29735).
unicode_unihan_variant(0x2980B, kTraditionalVariant, 0x29754).
unicode_unihan_variant(0x2980C, kTraditionalVariant, 0x9938).
unicode_unihan_variant(0x2980E, kTraditionalVariant, 0x29784).
unicode_unihan_variant(0x2980F, kTraditionalVariant, 0x297A6).
unicode_unihan_variant(0x29811, kSemanticVariant, 0x9802). %<kFenn
unicode_unihan_variant(0x29820, kTraditionalVariant, 0x29834).
unicode_unihan_variant(0x29834, kSimplifiedVariant, 0x29820).
unicode_unihan_variant(0x2987A, kSimplifiedVariant, 0x299E6).
unicode_unihan_variant(0x298A1, kSimplifiedVariant, 0x299EC).
unicode_unihan_variant(0x298B2, kSemanticVariant, 0x99DB). %<kMeyerWempe
unicode_unihan_variant(0x298B4, kSimplifiedVariant, 0x299F5).
unicode_unihan_variant(0x298B8, kSimplifiedVariant, 0x299F3).
unicode_unihan_variant(0x298BE, kSimplifiedVariant, 0x299EE).
unicode_unihan_variant(0x298CF, kSimplifiedVariant, 0x299F6).
unicode_unihan_variant(0x298D1, kSimplifiedVariant, 0x4BC3).
unicode_unihan_variant(0x298FA, kSimplifiedVariant, 0x299FC).
unicode_unihan_variant(0x2990A, kSimplifiedVariant, 0x299E9).
unicode_unihan_variant(0x29919, kSimplifiedVariant, 0x29A06).
unicode_unihan_variant(0x29932, kSimplifiedVariant, 0x29A09).
unicode_unihan_variant(0x29938, kSimplifiedVariant, 0x29A05).
unicode_unihan_variant(0x29944, kSimplifiedVariant, 0x29A0B).
unicode_unihan_variant(0x29947, kSimplifiedVariant, 0x29A0D).
unicode_unihan_variant(0x29949, kSimplifiedVariant, 0x299F1).
unicode_unihan_variant(0x29951, kSimplifiedVariant, 0x29A0C).
unicode_unihan_variant(0x299A2, kSemanticVariant, 0x865E). %<kMeyerWempe
unicode_unihan_variant(0x299C6, kSimplifiedVariant, 0x29A10).
unicode_unihan_variant(0x299E6, kTraditionalVariant, 0x2987A).
unicode_unihan_variant(0x299E8, kTraditionalVariant, 0x99CE).
unicode_unihan_variant(0x299E9, kTraditionalVariant, 0x2990A).
unicode_unihan_variant(0x299EA, kTraditionalVariant, 0x4BBE).
unicode_unihan_variant(0x299EB, kTraditionalVariant, 0x99DA).
unicode_unihan_variant(0x299EC, kTraditionalVariant, 0x298A1).
unicode_unihan_variant(0x299ED, kTraditionalVariant, 0x4B7F).
unicode_unihan_variant(0x299EE, kTraditionalVariant, 0x298BE).
unicode_unihan_variant(0x299EF, kTraditionalVariant, 0x9A4B).
unicode_unihan_variant(0x299F0, kTraditionalVariant, 0x4B9D).
unicode_unihan_variant(0x299F1, kTraditionalVariant, 0x29949).
unicode_unihan_variant(0x299F2, kTraditionalVariant, 0x99E7).
unicode_unihan_variant(0x299F3, kTraditionalVariant, 0x298B8).
unicode_unihan_variant(0x299F4, kTraditionalVariant, 0x99E9).
unicode_unihan_variant(0x299F5, kTraditionalVariant, 0x298B4).
unicode_unihan_variant(0x299F6, kTraditionalVariant, 0x298CF).
unicode_unihan_variant(0x299FA, kTraditionalVariant, 0x99F6).
unicode_unihan_variant(0x299FC, kTraditionalVariant, 0x298FA).
unicode_unihan_variant(0x299FF, kTraditionalVariant, 0x4BA0).
unicode_unihan_variant(0x29A00, kTraditionalVariant, 0x9A14).
unicode_unihan_variant(0x29A01, kTraditionalVariant, 0x4B9E).
unicode_unihan_variant(0x29A03, kTraditionalVariant, 0x9A1D).
unicode_unihan_variant(0x29A04, kTraditionalVariant, 0x9A2A).
unicode_unihan_variant(0x29A05, kTraditionalVariant, 0x29938).
unicode_unihan_variant(0x29A06, kTraditionalVariant, 0x29919).
unicode_unihan_variant(0x29A07, kTraditionalVariant, 0x4BAB).
unicode_unihan_variant(0x29A08, kTraditionalVariant, 0x9A1F).
unicode_unihan_variant(0x29A09, kTraditionalVariant, 0x29932).
unicode_unihan_variant(0x29A0A, kTraditionalVariant, 0x9A1A).
unicode_unihan_variant(0x29A0B, kTraditionalVariant, 0x29944).
unicode_unihan_variant(0x29A0C, kTraditionalVariant, 0x29951).
unicode_unihan_variant(0x29A0D, kTraditionalVariant, 0x29947).
unicode_unihan_variant(0x29A0E, kTraditionalVariant, 0x9FAD).
unicode_unihan_variant(0x29A0F, kTraditionalVariant, 0x4BB3).
unicode_unihan_variant(0x29A10, kTraditionalVariant, 0x299C6).
unicode_unihan_variant(0x29A18, kSemanticVariant, 0x8090). %<kFenn 0x80F3<kFenn
unicode_unihan_variant(0x29B23, kTraditionalVariant, 0x29B59).
unicode_unihan_variant(0x29B24, kTraditionalVariant, 0x29C00).
unicode_unihan_variant(0x29B59, kSimplifiedVariant, 0x29B23).
unicode_unihan_variant(0x29BD2, kTraditionalVariant, 0x29BF3).
unicode_unihan_variant(0x29BED, kSemanticVariant, 0x9B13). %<kMeyerWempe
unicode_unihan_variant(0x29BF3, kSimplifiedVariant, 0x29BD2).
unicode_unihan_variant(0x29C00, kSimplifiedVariant, 0x29B24).
unicode_unihan_variant(0x29C92, kTraditionalVariant, 0x29CE4).
unicode_unihan_variant(0x29CE4, kSimplifiedVariant, 0x29C92).
unicode_unihan_variant(0x29D69, kSimplifiedVariant, 0x29F7A).
unicode_unihan_variant(0x29D79, kSimplifiedVariant, 0x29F7B).
unicode_unihan_variant(0x29D98, kSimplifiedVariant, 0x4C9E).
unicode_unihan_variant(0x29DB0, kSimplifiedVariant, 0x29F7F).
unicode_unihan_variant(0x29DB1, kSimplifiedVariant, 0x29F7D).
unicode_unihan_variant(0x29DF0, kSimplifiedVariant, 0x29F84).
unicode_unihan_variant(0x29E03, kSimplifiedVariant, 0x29F85).
unicode_unihan_variant(0x29E26, kSimplifiedVariant, 0x29F86).
unicode_unihan_variant(0x29F79, kTraditionalVariant, 0x9B65).
unicode_unihan_variant(0x29F7A, kTraditionalVariant, 0x29D69).
unicode_unihan_variant(0x29F7B, kTraditionalVariant, 0x29D79).
unicode_unihan_variant(0x29F7C, kTraditionalVariant, 0x9BF6).
unicode_unihan_variant(0x29F7D, kTraditionalVariant, 0x29DB1).
unicode_unihan_variant(0x29F7E, kTraditionalVariant, 0x9B9F).
unicode_unihan_variant(0x29F7F, kTraditionalVariant, 0x29DB0).
unicode_unihan_variant(0x29F81, kTraditionalVariant, 0x9BC4).
unicode_unihan_variant(0x29F82, kTraditionalVariant, 0x4C96).
unicode_unihan_variant(0x29F83, kTraditionalVariant, 0x9BB8).
unicode_unihan_variant(0x29F84, kTraditionalVariant, 0x29DF0).
unicode_unihan_variant(0x29F85, kTraditionalVariant, 0x29E03).
unicode_unihan_variant(0x29F86, kTraditionalVariant, 0x29E26).
unicode_unihan_variant(0x29F87, kTraditionalVariant, 0x9BF1).
unicode_unihan_variant(0x29F88, kTraditionalVariant, 0x4C59).
unicode_unihan_variant(0x29F8A, kTraditionalVariant, 0x4C6C).
unicode_unihan_variant(0x29F8B, kTraditionalVariant, 0x4C70).
unicode_unihan_variant(0x29F8C, kTraditionalVariant, 0x9C47).
unicode_unihan_variant(0x29FEA, kSimplifiedVariant, 0x2A244).
unicode_unihan_variant(0x2A026, kSimplifiedVariant, 0x2A245).
unicode_unihan_variant(0x2A03E, kSimplifiedVariant, 0x2A24B).
unicode_unihan_variant(0x2A048, kSimplifiedVariant, 0x2A249).
unicode_unihan_variant(0x2A056, kSimplifiedVariant, 0x2A24C).
unicode_unihan_variant(0x2A086, kSimplifiedVariant, 0x2A24E).
unicode_unihan_variant(0x2A0CD, kSimplifiedVariant, 0x2A250).
unicode_unihan_variant(0x2A0CF, kSimplifiedVariant, 0x2A24F).
unicode_unihan_variant(0x2A106, kSimplifiedVariant, 0x2A254).
unicode_unihan_variant(0x2A115, kSimplifiedVariant, 0x2A252).
unicode_unihan_variant(0x2A1F3, kSimplifiedVariant, 0x2A255).
unicode_unihan_variant(0x2A242, kTraditionalVariant, 0x4CB0).
unicode_unihan_variant(0x2A243, kTraditionalVariant, 0x9CFC).
unicode_unihan_variant(0x2A244, kTraditionalVariant, 0x29FEA).
unicode_unihan_variant(0x2A245, kTraditionalVariant, 0x2A026).
unicode_unihan_variant(0x2A246, kTraditionalVariant, 0x9D32).
unicode_unihan_variant(0x2A248, kTraditionalVariant, 0x9D1C).
unicode_unihan_variant(0x2A249, kTraditionalVariant, 0x2A048).
unicode_unihan_variant(0x2A24A, kTraditionalVariant, 0x9DE8).
unicode_unihan_variant(0x2A24B, kTraditionalVariant, 0x2A03E).
unicode_unihan_variant(0x2A24C, kTraditionalVariant, 0x2A056).
unicode_unihan_variant(0x2A24D, kTraditionalVariant, 0x9D5A).
unicode_unihan_variant(0x2A24E, kTraditionalVariant, 0x2A086).
unicode_unihan_variant(0x2A24F, kTraditionalVariant, 0x2A0CF).
unicode_unihan_variant(0x2A250, kTraditionalVariant, 0x2A0CD).
unicode_unihan_variant(0x2A251, kTraditionalVariant, 0x9DD4).
unicode_unihan_variant(0x2A252, kTraditionalVariant, 0x2A115).
unicode_unihan_variant(0x2A254, kTraditionalVariant, 0x2A106).
unicode_unihan_variant(0x2A255, kTraditionalVariant, 0x2A1F3).
unicode_unihan_variant(0x2A2FF, kSimplifiedVariant, 0x2A38D).
unicode_unihan_variant(0x2A388, kTraditionalVariant, 0x4D2C).
unicode_unihan_variant(0x2A389, kTraditionalVariant, 0x9EB2).
unicode_unihan_variant(0x2A38A, kTraditionalVariant, 0x9EA8).
unicode_unihan_variant(0x2A38B, kTraditionalVariant, 0x4D34).
unicode_unihan_variant(0x2A38D, kTraditionalVariant, 0x2A2FF).
unicode_unihan_variant(0x2A502, kSemanticVariant, 0x9F0E). %<kFenn
unicode_unihan_variant(0x2A52D, kTraditionalVariant, 0x2A535).
unicode_unihan_variant(0x2A535, kSimplifiedVariant, 0x2A52D).
unicode_unihan_variant(0x2A599, kSemanticVariant, 0x9F41). %<kFenn
unicode_unihan_variant(0x2A600, kSimplifiedVariant, 0x2A68F).
unicode_unihan_variant(0x2A62F, kSimplifiedVariant, 0x2A690).
unicode_unihan_variant(0x2A68F, kTraditionalVariant, 0x2A600).
unicode_unihan_variant(0x2A690, kTraditionalVariant, 0x2A62F).
unicode_unihan_variant(0x2A79D, kTraditionalVariant, 0x51D9).
unicode_unihan_variant(0x2A84F, kTraditionalVariant, 0x55F9).
unicode_unihan_variant(0x2A8AE, kTraditionalVariant, 0x571E).
unicode_unihan_variant(0x2AA0A, kTraditionalVariant, 0x379E).
unicode_unihan_variant(0x2AA17, kSpecializedSemanticVariant, 0x2B3CB). %<kXHC1983
unicode_unihan_variant(0x2AA17, kTraditionalVariant, 0x5C69).
unicode_unihan_variant(0x2AA9D, kSemanticVariant, 0x9095). %<kMatthews 0x96CD<kMatthews 0x96DD<kMatthews
unicode_unihan_variant(0x2AED0, kTraditionalVariant, 0x747D).
unicode_unihan_variant(0x2AFA2, kTraditionalVariant, 0x774D).
unicode_unihan_variant(0x2B061, kTraditionalVariant, 0x9D17).
unicode_unihan_variant(0x2B088, kTraditionalVariant, 0x426C).
unicode_unihan_variant(0x2B128, kTraditionalVariant, 0x7D7A).
unicode_unihan_variant(0x2B138, kTraditionalVariant, 0x7E81).
unicode_unihan_variant(0x2B300, kTraditionalVariant, 0x8940).
unicode_unihan_variant(0x2B328, kTraditionalVariant, 0x89BC).
unicode_unihan_variant(0x2B359, kTraditionalVariant, 0x8A11).
unicode_unihan_variant(0x2B35F, kTraditionalVariant, 0x8A51).
unicode_unihan_variant(0x2B362, kTraditionalVariant, 0x8B4A).
unicode_unihan_variant(0x2B370, kTraditionalVariant, 0x8AF0).
unicode_unihan_variant(0x2B372, kTraditionalVariant, 0x8B0F).
unicode_unihan_variant(0x2B3CB, kSpecializedSemanticVariant, 0x2AA17). %<kXHC1983
unicode_unihan_variant(0x2B3CB, kTraditionalVariant, 0x8E7B).
unicode_unihan_variant(0x2B404, kTraditionalVariant, 0x8ECF).
unicode_unihan_variant(0x2B406, kTraditionalVariant, 0x8F63).
unicode_unihan_variant(0x2B409, kTraditionalVariant, 0x8EE8).
unicode_unihan_variant(0x2B410, kTraditionalVariant, 0x8F17).
unicode_unihan_variant(0x2B413, kTraditionalVariant, 0x8F2E).
unicode_unihan_variant(0x2B4E7, kTraditionalVariant, 0x9207).
unicode_unihan_variant(0x2B4E9, kTraditionalVariant, 0x93E6).
unicode_unihan_variant(0x2B50E, kTraditionalVariant, 0x940D).
unicode_unihan_variant(0x2B5B8, kTraditionalVariant, 0x9858).
unicode_unihan_variant(0x2B5E0, kTraditionalVariant, 0x9926).
unicode_unihan_variant(0x2B5E6, kTraditionalVariant, 0x9914).
unicode_unihan_variant(0x2B5E7, kTraditionalVariant, 0x9917).
unicode_unihan_variant(0x2B5EE, kTraditionalVariant, 0x992D).
unicode_unihan_variant(0x2B5F4, kTraditionalVariant, 0x9958).
unicode_unihan_variant(0x2B61D, kTraditionalVariant, 0x99C3).
unicode_unihan_variant(0x2B623, kTraditionalVariant, 0x99FB).
unicode_unihan_variant(0x2B624, kTraditionalVariant, 0x9A03).
unicode_unihan_variant(0x2B628, kTraditionalVariant, 0x9A20).
unicode_unihan_variant(0x2B688, kTraditionalVariant, 0x9C6E).
unicode_unihan_variant(0x2B689, kTraditionalVariant, 0x9B5F).
unicode_unihan_variant(0x2B692, kTraditionalVariant, 0x9B84).
unicode_unihan_variant(0x2B694, kTraditionalVariant, 0x9BB0).
unicode_unihan_variant(0x2B695, kTraditionalVariant, 0x9C24).
unicode_unihan_variant(0x2B699, kTraditionalVariant, 0x9BC6).
unicode_unihan_variant(0x2B6DB, kTraditionalVariant, 0x9CF7).
unicode_unihan_variant(0x2B6DE, kTraditionalVariant, 0x9D03).
unicode_unihan_variant(0x2B6E2, kTraditionalVariant, 0x9E0B).
unicode_unihan_variant(0x2B6F6, kTraditionalVariant, 0x9D92).
unicode_unihan_variant(0x2B6F8, kTraditionalVariant, 0x9D97).
unicode_unihan_variant(0x2F800, kCompatibilityVariant, 0x4E3D).
unicode_unihan_variant(0x2F801, kCompatibilityVariant, 0x4E38).
unicode_unihan_variant(0x2F802, kCompatibilityVariant, 0x4E41).
unicode_unihan_variant(0x2F803, kCompatibilityVariant, 0x20122).
unicode_unihan_variant(0x2F804, kCompatibilityVariant, 0x4F60).
unicode_unihan_variant(0x2F805, kCompatibilityVariant, 0x4FAE).
unicode_unihan_variant(0x2F806, kCompatibilityVariant, 0x4FBB).
unicode_unihan_variant(0x2F807, kCompatibilityVariant, 0x5002).
unicode_unihan_variant(0x2F808, kCompatibilityVariant, 0x507A).
unicode_unihan_variant(0x2F809, kCompatibilityVariant, 0x5099).
unicode_unihan_variant(0x2F80A, kCompatibilityVariant, 0x50E7).
unicode_unihan_variant(0x2F80B, kCompatibilityVariant, 0x50CF).
unicode_unihan_variant(0x2F80C, kCompatibilityVariant, 0x349E).
unicode_unihan_variant(0x2F80D, kCompatibilityVariant, 0x2063A).
unicode_unihan_variant(0x2F80E, kCompatibilityVariant, 0x514D).
unicode_unihan_variant(0x2F80F, kCompatibilityVariant, 0x5154).
unicode_unihan_variant(0x2F810, kCompatibilityVariant, 0x5164).
unicode_unihan_variant(0x2F811, kCompatibilityVariant, 0x5177).
unicode_unihan_variant(0x2F812, kCompatibilityVariant, 0x2051C).
unicode_unihan_variant(0x2F813, kCompatibilityVariant, 0x34B9).
unicode_unihan_variant(0x2F814, kCompatibilityVariant, 0x5167).
unicode_unihan_variant(0x2F815, kCompatibilityVariant, 0x518D).
unicode_unihan_variant(0x2F816, kCompatibilityVariant, 0x2054B).
unicode_unihan_variant(0x2F817, kCompatibilityVariant, 0x5197).
unicode_unihan_variant(0x2F817, kSemanticVariant, 0x5197). %<kMeyerWempe
unicode_unihan_variant(0x2F818, kCompatibilityVariant, 0x51A4).
unicode_unihan_variant(0x2F819, kCompatibilityVariant, 0x4ECC).
unicode_unihan_variant(0x2F81A, kCompatibilityVariant, 0x51AC).
unicode_unihan_variant(0x2F81B, kCompatibilityVariant, 0x51B5).
unicode_unihan_variant(0x2F81C, kCompatibilityVariant, 0x291DF).
unicode_unihan_variant(0x2F81D, kCompatibilityVariant, 0x51F5).
unicode_unihan_variant(0x2F81E, kCompatibilityVariant, 0x5203).
unicode_unihan_variant(0x2F81F, kCompatibilityVariant, 0x34DF).
unicode_unihan_variant(0x2F820, kCompatibilityVariant, 0x523B).
unicode_unihan_variant(0x2F821, kCompatibilityVariant, 0x5246).
unicode_unihan_variant(0x2F822, kCompatibilityVariant, 0x5272).
unicode_unihan_variant(0x2F823, kCompatibilityVariant, 0x5277).
unicode_unihan_variant(0x2F824, kCompatibilityVariant, 0x3515).
unicode_unihan_variant(0x2F825, kCompatibilityVariant, 0x52C7).
unicode_unihan_variant(0x2F826, kCompatibilityVariant, 0x52C9).
unicode_unihan_variant(0x2F827, kCompatibilityVariant, 0x52E4).
unicode_unihan_variant(0x2F828, kCompatibilityVariant, 0x52FA).
unicode_unihan_variant(0x2F829, kCompatibilityVariant, 0x5305).
unicode_unihan_variant(0x2F82A, kCompatibilityVariant, 0x5306).
unicode_unihan_variant(0x2F82B, kCompatibilityVariant, 0x5317).
unicode_unihan_variant(0x2F82C, kCompatibilityVariant, 0x5349).
unicode_unihan_variant(0x2F82D, kCompatibilityVariant, 0x5351).
unicode_unihan_variant(0x2F82E, kCompatibilityVariant, 0x535A).
unicode_unihan_variant(0x2F82F, kCompatibilityVariant, 0x5373).
unicode_unihan_variant(0x2F830, kCompatibilityVariant, 0x537D).
unicode_unihan_variant(0x2F831, kCompatibilityVariant, 0x537F).
unicode_unihan_variant(0x2F832, kCompatibilityVariant, 0x537F).
unicode_unihan_variant(0x2F833, kCompatibilityVariant, 0x537F).
unicode_unihan_variant(0x2F834, kCompatibilityVariant, 0x20A2C).
unicode_unihan_variant(0x2F835, kCompatibilityVariant, 0x7070).
unicode_unihan_variant(0x2F835, kSemanticVariant, 0x7070). %<kFenn
unicode_unihan_variant(0x2F836, kCompatibilityVariant, 0x53CA).
unicode_unihan_variant(0x2F837, kCompatibilityVariant, 0x53DF).
unicode_unihan_variant(0x2F838, kCompatibilityVariant, 0x20B63).
unicode_unihan_variant(0x2F839, kCompatibilityVariant, 0x53EB).
unicode_unihan_variant(0x2F83A, kCompatibilityVariant, 0x53F1).
unicode_unihan_variant(0x2F83B, kCompatibilityVariant, 0x5406).
unicode_unihan_variant(0x2F83C, kCompatibilityVariant, 0x549E).
unicode_unihan_variant(0x2F83D, kCompatibilityVariant, 0x5438).
unicode_unihan_variant(0x2F83E, kCompatibilityVariant, 0x5448).
unicode_unihan_variant(0x2F83F, kCompatibilityVariant, 0x5468).
unicode_unihan_variant(0x2F840, kCompatibilityVariant, 0x54A2).
unicode_unihan_variant(0x2F841, kCompatibilityVariant, 0x54F6).
unicode_unihan_variant(0x2F842, kCompatibilityVariant, 0x5510).
unicode_unihan_variant(0x2F843, kCompatibilityVariant, 0x5553).
unicode_unihan_variant(0x2F844, kCompatibilityVariant, 0x5563).
unicode_unihan_variant(0x2F845, kCompatibilityVariant, 0x5584).
unicode_unihan_variant(0x2F846, kCompatibilityVariant, 0x5584).
unicode_unihan_variant(0x2F847, kCompatibilityVariant, 0x5599).
unicode_unihan_variant(0x2F848, kCompatibilityVariant, 0x55AB).
unicode_unihan_variant(0x2F849, kCompatibilityVariant, 0x55B3).
unicode_unihan_variant(0x2F84A, kCompatibilityVariant, 0x55C2).
unicode_unihan_variant(0x2F84B, kCompatibilityVariant, 0x5716).
unicode_unihan_variant(0x2F84C, kCompatibilityVariant, 0x5606).
unicode_unihan_variant(0x2F84D, kCompatibilityVariant, 0x5717).
unicode_unihan_variant(0x2F84E, kCompatibilityVariant, 0x5651).
unicode_unihan_variant(0x2F84F, kCompatibilityVariant, 0x5674).
unicode_unihan_variant(0x2F850, kCompatibilityVariant, 0x5207).
unicode_unihan_variant(0x2F851, kCompatibilityVariant, 0x58EE).
unicode_unihan_variant(0x2F852, kCompatibilityVariant, 0x57CE).
unicode_unihan_variant(0x2F853, kCompatibilityVariant, 0x57F4).
unicode_unihan_variant(0x2F854, kCompatibilityVariant, 0x580D).
unicode_unihan_variant(0x2F855, kCompatibilityVariant, 0x578B).
unicode_unihan_variant(0x2F856, kCompatibilityVariant, 0x5832).
unicode_unihan_variant(0x2F857, kCompatibilityVariant, 0x5831).
unicode_unihan_variant(0x2F858, kCompatibilityVariant, 0x58AC).
unicode_unihan_variant(0x2F859, kCompatibilityVariant, 0x214E4).
unicode_unihan_variant(0x2F85A, kCompatibilityVariant, 0x58F2).
unicode_unihan_variant(0x2F85B, kCompatibilityVariant, 0x58F7).
unicode_unihan_variant(0x2F85C, kCompatibilityVariant, 0x5906).
unicode_unihan_variant(0x2F85D, kCompatibilityVariant, 0x591A).
unicode_unihan_variant(0x2F85E, kCompatibilityVariant, 0x5922).
unicode_unihan_variant(0x2F85F, kCompatibilityVariant, 0x5962).
unicode_unihan_variant(0x2F860, kCompatibilityVariant, 0x216A8).
unicode_unihan_variant(0x2F861, kCompatibilityVariant, 0x216EA).
unicode_unihan_variant(0x2F862, kCompatibilityVariant, 0x59EC).
unicode_unihan_variant(0x2F863, kCompatibilityVariant, 0x5A1B).
unicode_unihan_variant(0x2F864, kCompatibilityVariant, 0x5A27).
unicode_unihan_variant(0x2F865, kCompatibilityVariant, 0x59D8).
unicode_unihan_variant(0x2F866, kCompatibilityVariant, 0x5A66).
unicode_unihan_variant(0x2F867, kCompatibilityVariant, 0x36EE).
unicode_unihan_variant(0x2F868, kCompatibilityVariant, 0x36FC).
unicode_unihan_variant(0x2F869, kCompatibilityVariant, 0x5B08).
unicode_unihan_variant(0x2F86A, kCompatibilityVariant, 0x5B3E).
unicode_unihan_variant(0x2F86B, kCompatibilityVariant, 0x5B3E).
unicode_unihan_variant(0x2F86C, kCompatibilityVariant, 0x219C8).
unicode_unihan_variant(0x2F86D, kCompatibilityVariant, 0x5BC3).
unicode_unihan_variant(0x2F86E, kCompatibilityVariant, 0x5BD8).
unicode_unihan_variant(0x2F86F, kCompatibilityVariant, 0x5BE7).
unicode_unihan_variant(0x2F870, kCompatibilityVariant, 0x5BF3).
unicode_unihan_variant(0x2F871, kCompatibilityVariant, 0x21B18).
unicode_unihan_variant(0x2F872, kCompatibilityVariant, 0x5BFF).
unicode_unihan_variant(0x2F873, kCompatibilityVariant, 0x5C06).
unicode_unihan_variant(0x2F874, kCompatibilityVariant, 0x5F53).
unicode_unihan_variant(0x2F875, kCompatibilityVariant, 0x5C22).
unicode_unihan_variant(0x2F876, kCompatibilityVariant, 0x3781).
unicode_unihan_variant(0x2F877, kCompatibilityVariant, 0x5C60).
unicode_unihan_variant(0x2F878, kCompatibilityVariant, 0x5C6E).
unicode_unihan_variant(0x2F879, kCompatibilityVariant, 0x5CC0).
unicode_unihan_variant(0x2F87A, kCompatibilityVariant, 0x5C8D).
unicode_unihan_variant(0x2F87B, kCompatibilityVariant, 0x21DE4).
unicode_unihan_variant(0x2F87C, kCompatibilityVariant, 0x5D43).
unicode_unihan_variant(0x2F87D, kCompatibilityVariant, 0x21DE6).
unicode_unihan_variant(0x2F87E, kCompatibilityVariant, 0x5D6E).
unicode_unihan_variant(0x2F87F, kCompatibilityVariant, 0x5D6B).
unicode_unihan_variant(0x2F880, kCompatibilityVariant, 0x5D7C).
unicode_unihan_variant(0x2F881, kCompatibilityVariant, 0x5DE1).
unicode_unihan_variant(0x2F882, kCompatibilityVariant, 0x5DE2).
unicode_unihan_variant(0x2F883, kCompatibilityVariant, 0x382F).
unicode_unihan_variant(0x2F884, kCompatibilityVariant, 0x5DFD).
unicode_unihan_variant(0x2F885, kCompatibilityVariant, 0x5E28).
unicode_unihan_variant(0x2F886, kCompatibilityVariant, 0x5E3D).
unicode_unihan_variant(0x2F887, kCompatibilityVariant, 0x5E69).
unicode_unihan_variant(0x2F888, kCompatibilityVariant, 0x3862).
unicode_unihan_variant(0x2F889, kCompatibilityVariant, 0x22183).
unicode_unihan_variant(0x2F88A, kCompatibilityVariant, 0x387C).
unicode_unihan_variant(0x2F88B, kCompatibilityVariant, 0x5EB0).
unicode_unihan_variant(0x2F88C, kCompatibilityVariant, 0x5EB3).
unicode_unihan_variant(0x2F88D, kCompatibilityVariant, 0x5EB6).
unicode_unihan_variant(0x2F88E, kCompatibilityVariant, 0x5ECA).
unicode_unihan_variant(0x2F88F, kCompatibilityVariant, 0x2A392).
unicode_unihan_variant(0x2F890, kCompatibilityVariant, 0x5EFE).
unicode_unihan_variant(0x2F891, kCompatibilityVariant, 0x22331).
unicode_unihan_variant(0x2F892, kCompatibilityVariant, 0x22331).
unicode_unihan_variant(0x2F893, kCompatibilityVariant, 0x8201).
unicode_unihan_variant(0x2F894, kCompatibilityVariant, 0x5F22).
unicode_unihan_variant(0x2F895, kCompatibilityVariant, 0x5F22).
unicode_unihan_variant(0x2F896, kCompatibilityVariant, 0x38C7).
unicode_unihan_variant(0x2F897, kCompatibilityVariant, 0x232B8).
unicode_unihan_variant(0x2F898, kCompatibilityVariant, 0x261DA).
unicode_unihan_variant(0x2F899, kCompatibilityVariant, 0x5F62).
unicode_unihan_variant(0x2F89A, kCompatibilityVariant, 0x5F6B).
unicode_unihan_variant(0x2F89B, kCompatibilityVariant, 0x38E3).
unicode_unihan_variant(0x2F89C, kCompatibilityVariant, 0x5F9A).
unicode_unihan_variant(0x2F89D, kCompatibilityVariant, 0x5FCD).
unicode_unihan_variant(0x2F89E, kCompatibilityVariant, 0x5FD7).
unicode_unihan_variant(0x2F89F, kCompatibilityVariant, 0x5FF9).
unicode_unihan_variant(0x2F8A0, kCompatibilityVariant, 0x6081).
unicode_unihan_variant(0x2F8A1, kCompatibilityVariant, 0x393A).
unicode_unihan_variant(0x2F8A2, kCompatibilityVariant, 0x391C).
unicode_unihan_variant(0x2F8A3, kCompatibilityVariant, 0x6094).
unicode_unihan_variant(0x2F8A4, kCompatibilityVariant, 0x226D4).
unicode_unihan_variant(0x2F8A5, kCompatibilityVariant, 0x60C7).
unicode_unihan_variant(0x2F8A6, kCompatibilityVariant, 0x6148).
unicode_unihan_variant(0x2F8A7, kCompatibilityVariant, 0x614C).
unicode_unihan_variant(0x2F8A8, kCompatibilityVariant, 0x614E).
unicode_unihan_variant(0x2F8A9, kCompatibilityVariant, 0x614C).
unicode_unihan_variant(0x2F8AA, kCompatibilityVariant, 0x617A).
unicode_unihan_variant(0x2F8AB, kCompatibilityVariant, 0x618E).
unicode_unihan_variant(0x2F8AC, kCompatibilityVariant, 0x61B2).
unicode_unihan_variant(0x2F8AD, kCompatibilityVariant, 0x61A4).
unicode_unihan_variant(0x2F8AE, kCompatibilityVariant, 0x61AF).
unicode_unihan_variant(0x2F8AF, kCompatibilityVariant, 0x61DE).
unicode_unihan_variant(0x2F8B0, kCompatibilityVariant, 0x61F2).
unicode_unihan_variant(0x2F8B1, kCompatibilityVariant, 0x61F6).
unicode_unihan_variant(0x2F8B2, kCompatibilityVariant, 0x6210).
unicode_unihan_variant(0x2F8B3, kCompatibilityVariant, 0x621B).
unicode_unihan_variant(0x2F8B4, kCompatibilityVariant, 0x625D).
unicode_unihan_variant(0x2F8B5, kCompatibilityVariant, 0x62B1).
unicode_unihan_variant(0x2F8B6, kCompatibilityVariant, 0x62D4).
unicode_unihan_variant(0x2F8B7, kCompatibilityVariant, 0x6350).
unicode_unihan_variant(0x2F8B8, kCompatibilityVariant, 0x22B0C).
unicode_unihan_variant(0x2F8B9, kCompatibilityVariant, 0x633D).
unicode_unihan_variant(0x2F8BA, kCompatibilityVariant, 0x62FC).
unicode_unihan_variant(0x2F8BB, kCompatibilityVariant, 0x6368).
unicode_unihan_variant(0x2F8BC, kCompatibilityVariant, 0x6383).
unicode_unihan_variant(0x2F8BD, kCompatibilityVariant, 0x63E4).
unicode_unihan_variant(0x2F8BE, kCompatibilityVariant, 0x22BF1).
unicode_unihan_variant(0x2F8BF, kCompatibilityVariant, 0x6422).
unicode_unihan_variant(0x2F8C0, kCompatibilityVariant, 0x63C5).
unicode_unihan_variant(0x2F8C1, kCompatibilityVariant, 0x63A9).
unicode_unihan_variant(0x2F8C2, kCompatibilityVariant, 0x3A2E).
unicode_unihan_variant(0x2F8C3, kCompatibilityVariant, 0x6469).
unicode_unihan_variant(0x2F8C4, kCompatibilityVariant, 0x647E).
unicode_unihan_variant(0x2F8C5, kCompatibilityVariant, 0x649D).
unicode_unihan_variant(0x2F8C6, kCompatibilityVariant, 0x6477).
unicode_unihan_variant(0x2F8C7, kCompatibilityVariant, 0x3A6C).
unicode_unihan_variant(0x2F8C8, kCompatibilityVariant, 0x654F).
unicode_unihan_variant(0x2F8C9, kCompatibilityVariant, 0x656C).
unicode_unihan_variant(0x2F8CA, kCompatibilityVariant, 0x2300A).
unicode_unihan_variant(0x2F8CB, kCompatibilityVariant, 0x65E3).
unicode_unihan_variant(0x2F8CC, kCompatibilityVariant, 0x66F8).
unicode_unihan_variant(0x2F8CD, kCompatibilityVariant, 0x6649).
unicode_unihan_variant(0x2F8CE, kCompatibilityVariant, 0x3B19).
unicode_unihan_variant(0x2F8CF, kCompatibilityVariant, 0x6691).
unicode_unihan_variant(0x2F8D0, kCompatibilityVariant, 0x3B08).
unicode_unihan_variant(0x2F8D1, kCompatibilityVariant, 0x3AE4).
unicode_unihan_variant(0x2F8D2, kCompatibilityVariant, 0x5192).
unicode_unihan_variant(0x2F8D2, kSemanticVariant, 0x5192). %<kHKGlyph
unicode_unihan_variant(0x2F8D3, kCompatibilityVariant, 0x5195).
unicode_unihan_variant(0x2F8D4, kCompatibilityVariant, 0x6700).
unicode_unihan_variant(0x2F8D5, kCompatibilityVariant, 0x669C).
unicode_unihan_variant(0x2F8D6, kCompatibilityVariant, 0x80AD).
unicode_unihan_variant(0x2F8D7, kCompatibilityVariant, 0x43D9).
unicode_unihan_variant(0x2F8D8, kCompatibilityVariant, 0x6717).
unicode_unihan_variant(0x2F8D9, kCompatibilityVariant, 0x671B).
unicode_unihan_variant(0x2F8DA, kCompatibilityVariant, 0x6721).
unicode_unihan_variant(0x2F8DB, kCompatibilityVariant, 0x675E).
unicode_unihan_variant(0x2F8DC, kCompatibilityVariant, 0x6753).
unicode_unihan_variant(0x2F8DD, kCompatibilityVariant, 0x233C3).
unicode_unihan_variant(0x2F8DE, kCompatibilityVariant, 0x3B49).
unicode_unihan_variant(0x2F8DF, kCompatibilityVariant, 0x67FA).
unicode_unihan_variant(0x2F8E0, kCompatibilityVariant, 0x6785).
unicode_unihan_variant(0x2F8E1, kCompatibilityVariant, 0x6852).
unicode_unihan_variant(0x2F8E2, kCompatibilityVariant, 0x6885).
unicode_unihan_variant(0x2F8E3, kCompatibilityVariant, 0x2346D).
unicode_unihan_variant(0x2F8E4, kCompatibilityVariant, 0x688E).
unicode_unihan_variant(0x2F8E5, kCompatibilityVariant, 0x681F).
unicode_unihan_variant(0x2F8E6, kCompatibilityVariant, 0x6914).
unicode_unihan_variant(0x2F8E7, kCompatibilityVariant, 0x3B9D).
unicode_unihan_variant(0x2F8E8, kCompatibilityVariant, 0x6942).
unicode_unihan_variant(0x2F8E9, kCompatibilityVariant, 0x69A3).
unicode_unihan_variant(0x2F8EA, kCompatibilityVariant, 0x69EA).
unicode_unihan_variant(0x2F8EB, kCompatibilityVariant, 0x6AA8).
unicode_unihan_variant(0x2F8EC, kCompatibilityVariant, 0x236A3).
unicode_unihan_variant(0x2F8ED, kCompatibilityVariant, 0x6ADB).
unicode_unihan_variant(0x2F8EE, kCompatibilityVariant, 0x3C18).
unicode_unihan_variant(0x2F8EF, kCompatibilityVariant, 0x6B21).
unicode_unihan_variant(0x2F8F0, kCompatibilityVariant, 0x238A7).
unicode_unihan_variant(0x2F8F1, kCompatibilityVariant, 0x6B54).
unicode_unihan_variant(0x2F8F2, kCompatibilityVariant, 0x3C4E).
unicode_unihan_variant(0x2F8F3, kCompatibilityVariant, 0x6B72).
unicode_unihan_variant(0x2F8F4, kCompatibilityVariant, 0x6B9F).
unicode_unihan_variant(0x2F8F5, kCompatibilityVariant, 0x6BBA).
unicode_unihan_variant(0x2F8F6, kCompatibilityVariant, 0x6BBB).
unicode_unihan_variant(0x2F8F7, kCompatibilityVariant, 0x23A8D).
unicode_unihan_variant(0x2F8F8, kCompatibilityVariant, 0x21D0B).
unicode_unihan_variant(0x2F8F9, kCompatibilityVariant, 0x23AFA).
unicode_unihan_variant(0x2F8FA, kCompatibilityVariant, 0x6C4E).
unicode_unihan_variant(0x2F8FB, kCompatibilityVariant, 0x23CBC).
unicode_unihan_variant(0x2F8FC, kCompatibilityVariant, 0x6CBF).
unicode_unihan_variant(0x2F8FD, kCompatibilityVariant, 0x6CCD).
unicode_unihan_variant(0x2F8FE, kCompatibilityVariant, 0x6C67).
unicode_unihan_variant(0x2F8FF, kCompatibilityVariant, 0x6D16).
unicode_unihan_variant(0x2F900, kCompatibilityVariant, 0x6D3E).
unicode_unihan_variant(0x2F901, kCompatibilityVariant, 0x6D77).
unicode_unihan_variant(0x2F902, kCompatibilityVariant, 0x6D41).
unicode_unihan_variant(0x2F903, kCompatibilityVariant, 0x6D69).
unicode_unihan_variant(0x2F904, kCompatibilityVariant, 0x6D78).
unicode_unihan_variant(0x2F905, kCompatibilityVariant, 0x6D85).
unicode_unihan_variant(0x2F906, kCompatibilityVariant, 0x23D1E).
unicode_unihan_variant(0x2F907, kCompatibilityVariant, 0x6D34).
unicode_unihan_variant(0x2F908, kCompatibilityVariant, 0x6E2F).
unicode_unihan_variant(0x2F909, kCompatibilityVariant, 0x6E6E).
unicode_unihan_variant(0x2F90A, kCompatibilityVariant, 0x3D33).
unicode_unihan_variant(0x2F90B, kCompatibilityVariant, 0x6ECB).
unicode_unihan_variant(0x2F90C, kCompatibilityVariant, 0x6EC7).
unicode_unihan_variant(0x2F90D, kCompatibilityVariant, 0x23ED1).
unicode_unihan_variant(0x2F90E, kCompatibilityVariant, 0x6DF9).
unicode_unihan_variant(0x2F90F, kCompatibilityVariant, 0x6F6E).
unicode_unihan_variant(0x2F910, kCompatibilityVariant, 0x23F5E).
unicode_unihan_variant(0x2F911, kCompatibilityVariant, 0x23F8E).
unicode_unihan_variant(0x2F912, kCompatibilityVariant, 0x6FC6).
unicode_unihan_variant(0x2F913, kCompatibilityVariant, 0x7039).
unicode_unihan_variant(0x2F914, kCompatibilityVariant, 0x701E).
unicode_unihan_variant(0x2F915, kCompatibilityVariant, 0x701B).
unicode_unihan_variant(0x2F916, kCompatibilityVariant, 0x3D96).
unicode_unihan_variant(0x2F917, kCompatibilityVariant, 0x704A).
unicode_unihan_variant(0x2F918, kCompatibilityVariant, 0x707D).
unicode_unihan_variant(0x2F919, kCompatibilityVariant, 0x7077).
unicode_unihan_variant(0x2F91A, kCompatibilityVariant, 0x70AD).
unicode_unihan_variant(0x2F91B, kCompatibilityVariant, 0x20525).
unicode_unihan_variant(0x2F91C, kCompatibilityVariant, 0x7145).
unicode_unihan_variant(0x2F91D, kCompatibilityVariant, 0x24263).
unicode_unihan_variant(0x2F91E, kCompatibilityVariant, 0x719C).
unicode_unihan_variant(0x2F91F, kCompatibilityVariant, 0x243AB).
unicode_unihan_variant(0x2F920, kCompatibilityVariant, 0x7228).
unicode_unihan_variant(0x2F921, kCompatibilityVariant, 0x7235).
unicode_unihan_variant(0x2F922, kCompatibilityVariant, 0x7250).
unicode_unihan_variant(0x2F923, kCompatibilityVariant, 0x24608).
unicode_unihan_variant(0x2F924, kCompatibilityVariant, 0x7280).
unicode_unihan_variant(0x2F925, kCompatibilityVariant, 0x7295).
unicode_unihan_variant(0x2F926, kCompatibilityVariant, 0x24735).
unicode_unihan_variant(0x2F927, kCompatibilityVariant, 0x24814).
unicode_unihan_variant(0x2F928, kCompatibilityVariant, 0x737A).
unicode_unihan_variant(0x2F929, kCompatibilityVariant, 0x738B).
unicode_unihan_variant(0x2F92A, kCompatibilityVariant, 0x3EAC).
unicode_unihan_variant(0x2F92B, kCompatibilityVariant, 0x73A5).
unicode_unihan_variant(0x2F92C, kCompatibilityVariant, 0x3EB8).
unicode_unihan_variant(0x2F92D, kCompatibilityVariant, 0x3EB8).
unicode_unihan_variant(0x2F92E, kCompatibilityVariant, 0x7447).
unicode_unihan_variant(0x2F92F, kCompatibilityVariant, 0x745C).
unicode_unihan_variant(0x2F930, kCompatibilityVariant, 0x7471).
unicode_unihan_variant(0x2F931, kCompatibilityVariant, 0x7485).
unicode_unihan_variant(0x2F932, kCompatibilityVariant, 0x74CA).
unicode_unihan_variant(0x2F933, kCompatibilityVariant, 0x3F1B).
unicode_unihan_variant(0x2F934, kCompatibilityVariant, 0x7524).
unicode_unihan_variant(0x2F935, kCompatibilityVariant, 0x24C36).
unicode_unihan_variant(0x2F936, kCompatibilityVariant, 0x753E).
unicode_unihan_variant(0x2F937, kCompatibilityVariant, 0x24C92).
unicode_unihan_variant(0x2F938, kCompatibilityVariant, 0x7570).
unicode_unihan_variant(0x2F939, kCompatibilityVariant, 0x2219F).
unicode_unihan_variant(0x2F93A, kCompatibilityVariant, 0x7610).
unicode_unihan_variant(0x2F93B, kCompatibilityVariant, 0x24FA1).
unicode_unihan_variant(0x2F93C, kCompatibilityVariant, 0x24FB8).
unicode_unihan_variant(0x2F93D, kCompatibilityVariant, 0x25044).
unicode_unihan_variant(0x2F93E, kCompatibilityVariant, 0x3FFC).
unicode_unihan_variant(0x2F93F, kCompatibilityVariant, 0x4008).
unicode_unihan_variant(0x2F940, kCompatibilityVariant, 0x76F4).
unicode_unihan_variant(0x2F941, kCompatibilityVariant, 0x250F3).
unicode_unihan_variant(0x2F942, kCompatibilityVariant, 0x250F2).
unicode_unihan_variant(0x2F943, kCompatibilityVariant, 0x25119).
unicode_unihan_variant(0x2F944, kCompatibilityVariant, 0x25133).
unicode_unihan_variant(0x2F945, kCompatibilityVariant, 0x771E).
unicode_unihan_variant(0x2F946, kCompatibilityVariant, 0x771F).
unicode_unihan_variant(0x2F947, kCompatibilityVariant, 0x771F).
unicode_unihan_variant(0x2F948, kCompatibilityVariant, 0x774A).
unicode_unihan_variant(0x2F949, kCompatibilityVariant, 0x4039).
unicode_unihan_variant(0x2F94A, kCompatibilityVariant, 0x778B).
unicode_unihan_variant(0x2F94B, kCompatibilityVariant, 0x4046).
unicode_unihan_variant(0x2F94C, kCompatibilityVariant, 0x4096).
unicode_unihan_variant(0x2F94D, kCompatibilityVariant, 0x2541D).
unicode_unihan_variant(0x2F94E, kCompatibilityVariant, 0x784E).
unicode_unihan_variant(0x2F94F, kCompatibilityVariant, 0x788C).
unicode_unihan_variant(0x2F950, kCompatibilityVariant, 0x78CC).
unicode_unihan_variant(0x2F951, kCompatibilityVariant, 0x40E3).
unicode_unihan_variant(0x2F952, kCompatibilityVariant, 0x25626).
unicode_unihan_variant(0x2F953, kCompatibilityVariant, 0x7956).
unicode_unihan_variant(0x2F954, kCompatibilityVariant, 0x2569A).
unicode_unihan_variant(0x2F955, kCompatibilityVariant, 0x256C5).
unicode_unihan_variant(0x2F956, kCompatibilityVariant, 0x798F).
unicode_unihan_variant(0x2F957, kCompatibilityVariant, 0x79EB).
unicode_unihan_variant(0x2F958, kCompatibilityVariant, 0x412F).
unicode_unihan_variant(0x2F959, kCompatibilityVariant, 0x7A40).
unicode_unihan_variant(0x2F95A, kCompatibilityVariant, 0x7A4A).
unicode_unihan_variant(0x2F95B, kCompatibilityVariant, 0x7A4F).
unicode_unihan_variant(0x2F95C, kCompatibilityVariant, 0x2597C).
unicode_unihan_variant(0x2F95D, kCompatibilityVariant, 0x25AA7).
unicode_unihan_variant(0x2F95E, kCompatibilityVariant, 0x25AA7).
unicode_unihan_variant(0x2F95F, kCompatibilityVariant, 0x7AEE).
unicode_unihan_variant(0x2F960, kCompatibilityVariant, 0x4202).
unicode_unihan_variant(0x2F961, kCompatibilityVariant, 0x25BAB).
unicode_unihan_variant(0x2F962, kCompatibilityVariant, 0x7BC6).
unicode_unihan_variant(0x2F963, kCompatibilityVariant, 0x7BC9).
unicode_unihan_variant(0x2F964, kCompatibilityVariant, 0x4227).
unicode_unihan_variant(0x2F965, kCompatibilityVariant, 0x25C80).
unicode_unihan_variant(0x2F966, kCompatibilityVariant, 0x7CD2).
unicode_unihan_variant(0x2F967, kCompatibilityVariant, 0x42A0).
unicode_unihan_variant(0x2F968, kCompatibilityVariant, 0x7CE8).
unicode_unihan_variant(0x2F969, kCompatibilityVariant, 0x7CE3).
unicode_unihan_variant(0x2F96A, kCompatibilityVariant, 0x7D00).
unicode_unihan_variant(0x2F96B, kCompatibilityVariant, 0x25F86).
unicode_unihan_variant(0x2F96C, kCompatibilityVariant, 0x7D63).
unicode_unihan_variant(0x2F96D, kCompatibilityVariant, 0x4301).
unicode_unihan_variant(0x2F96E, kCompatibilityVariant, 0x7DC7).
unicode_unihan_variant(0x2F96F, kCompatibilityVariant, 0x7E02).
unicode_unihan_variant(0x2F970, kCompatibilityVariant, 0x7E45).
unicode_unihan_variant(0x2F971, kCompatibilityVariant, 0x4334).
unicode_unihan_variant(0x2F972, kCompatibilityVariant, 0x26228).
unicode_unihan_variant(0x2F973, kCompatibilityVariant, 0x26247).
unicode_unihan_variant(0x2F974, kCompatibilityVariant, 0x4359).
unicode_unihan_variant(0x2F975, kCompatibilityVariant, 0x262D9).
unicode_unihan_variant(0x2F976, kCompatibilityVariant, 0x7F7A).
unicode_unihan_variant(0x2F977, kCompatibilityVariant, 0x2633E).
unicode_unihan_variant(0x2F978, kCompatibilityVariant, 0x7F95).
unicode_unihan_variant(0x2F979, kCompatibilityVariant, 0x7FFA).
unicode_unihan_variant(0x2F97A, kCompatibilityVariant, 0x8005).
unicode_unihan_variant(0x2F97B, kCompatibilityVariant, 0x264DA).
unicode_unihan_variant(0x2F97C, kCompatibilityVariant, 0x26523).
unicode_unihan_variant(0x2F97D, kCompatibilityVariant, 0x8060).
unicode_unihan_variant(0x2F97E, kCompatibilityVariant, 0x265A8).
unicode_unihan_variant(0x2F97F, kCompatibilityVariant, 0x8070).
unicode_unihan_variant(0x2F980, kCompatibilityVariant, 0x2335F).
unicode_unihan_variant(0x2F981, kCompatibilityVariant, 0x43D5).
unicode_unihan_variant(0x2F982, kCompatibilityVariant, 0x80B2).
unicode_unihan_variant(0x2F983, kCompatibilityVariant, 0x8103).
unicode_unihan_variant(0x2F984, kCompatibilityVariant, 0x440B).
unicode_unihan_variant(0x2F985, kCompatibilityVariant, 0x813E).
unicode_unihan_variant(0x2F986, kCompatibilityVariant, 0x5AB5).
unicode_unihan_variant(0x2F987, kCompatibilityVariant, 0x267A7).
unicode_unihan_variant(0x2F988, kCompatibilityVariant, 0x267B5).
unicode_unihan_variant(0x2F989, kCompatibilityVariant, 0x23393).
unicode_unihan_variant(0x2F98A, kCompatibilityVariant, 0x2339C).
unicode_unihan_variant(0x2F98B, kCompatibilityVariant, 0x8201).
unicode_unihan_variant(0x2F98C, kCompatibilityVariant, 0x8204).
unicode_unihan_variant(0x2F98D, kCompatibilityVariant, 0x8F9E).
unicode_unihan_variant(0x2F98E, kCompatibilityVariant, 0x446B).
unicode_unihan_variant(0x2F98F, kCompatibilityVariant, 0x8291).
unicode_unihan_variant(0x2F990, kCompatibilityVariant, 0x828B).
unicode_unihan_variant(0x2F991, kCompatibilityVariant, 0x829D).
unicode_unihan_variant(0x2F992, kCompatibilityVariant, 0x52B3).
unicode_unihan_variant(0x2F993, kCompatibilityVariant, 0x82B1).
unicode_unihan_variant(0x2F994, kCompatibilityVariant, 0x82B3).
unicode_unihan_variant(0x2F995, kCompatibilityVariant, 0x82BD).
unicode_unihan_variant(0x2F996, kCompatibilityVariant, 0x82E6).
unicode_unihan_variant(0x2F997, kCompatibilityVariant, 0x26B3C).
unicode_unihan_variant(0x2F998, kCompatibilityVariant, 0x82E5).
unicode_unihan_variant(0x2F999, kCompatibilityVariant, 0x831D).
unicode_unihan_variant(0x2F99A, kCompatibilityVariant, 0x8363).
unicode_unihan_variant(0x2F99B, kCompatibilityVariant, 0x83AD).
unicode_unihan_variant(0x2F99C, kCompatibilityVariant, 0x8323).
unicode_unihan_variant(0x2F99D, kCompatibilityVariant, 0x83BD).
unicode_unihan_variant(0x2F99E, kCompatibilityVariant, 0x83E7).
unicode_unihan_variant(0x2F99F, kCompatibilityVariant, 0x8457).
unicode_unihan_variant(0x2F9A0, kCompatibilityVariant, 0x8353).
unicode_unihan_variant(0x2F9A1, kCompatibilityVariant, 0x83CA).
unicode_unihan_variant(0x2F9A2, kCompatibilityVariant, 0x83CC).
unicode_unihan_variant(0x2F9A3, kCompatibilityVariant, 0x83DC).
unicode_unihan_variant(0x2F9A4, kCompatibilityVariant, 0x26C36).
unicode_unihan_variant(0x2F9A5, kCompatibilityVariant, 0x26D6B).
unicode_unihan_variant(0x2F9A6, kCompatibilityVariant, 0x26CD5).
unicode_unihan_variant(0x2F9A7, kCompatibilityVariant, 0x452B).
unicode_unihan_variant(0x2F9A8, kCompatibilityVariant, 0x84F1).
unicode_unihan_variant(0x2F9A9, kCompatibilityVariant, 0x84F3).
unicode_unihan_variant(0x2F9AA, kCompatibilityVariant, 0x8516).
unicode_unihan_variant(0x2F9AB, kCompatibilityVariant, 0x273CA).
unicode_unihan_variant(0x2F9AC, kCompatibilityVariant, 0x8564).
unicode_unihan_variant(0x2F9AD, kCompatibilityVariant, 0x26F2C).
unicode_unihan_variant(0x2F9AE, kCompatibilityVariant, 0x455D).
unicode_unihan_variant(0x2F9AF, kCompatibilityVariant, 0x4561).
unicode_unihan_variant(0x2F9B0, kCompatibilityVariant, 0x26FB1).
unicode_unihan_variant(0x2F9B1, kCompatibilityVariant, 0x270D2).
unicode_unihan_variant(0x2F9B2, kCompatibilityVariant, 0x456B).
unicode_unihan_variant(0x2F9B3, kCompatibilityVariant, 0x8650).
unicode_unihan_variant(0x2F9B4, kCompatibilityVariant, 0x865C).
unicode_unihan_variant(0x2F9B5, kCompatibilityVariant, 0x8667).
unicode_unihan_variant(0x2F9B6, kCompatibilityVariant, 0x8669).
unicode_unihan_variant(0x2F9B7, kCompatibilityVariant, 0x86A9).
unicode_unihan_variant(0x2F9B8, kCompatibilityVariant, 0x8688).
unicode_unihan_variant(0x2F9B9, kCompatibilityVariant, 0x870E).
unicode_unihan_variant(0x2F9BA, kCompatibilityVariant, 0x86E2).
unicode_unihan_variant(0x2F9BB, kCompatibilityVariant, 0x8779).
unicode_unihan_variant(0x2F9BC, kCompatibilityVariant, 0x8728).
unicode_unihan_variant(0x2F9BD, kCompatibilityVariant, 0x876B).
unicode_unihan_variant(0x2F9BE, kCompatibilityVariant, 0x8786).
unicode_unihan_variant(0x2F9BF, kCompatibilityVariant, 0x45D7).
unicode_unihan_variant(0x2F9C0, kCompatibilityVariant, 0x87E1).
unicode_unihan_variant(0x2F9C1, kCompatibilityVariant, 0x8801).
unicode_unihan_variant(0x2F9C2, kCompatibilityVariant, 0x45F9).
unicode_unihan_variant(0x2F9C3, kCompatibilityVariant, 0x8860).
unicode_unihan_variant(0x2F9C4, kCompatibilityVariant, 0x8863).
unicode_unihan_variant(0x2F9C5, kCompatibilityVariant, 0x27667).
unicode_unihan_variant(0x2F9C6, kCompatibilityVariant, 0x88D7).
unicode_unihan_variant(0x2F9C7, kCompatibilityVariant, 0x88DE).
unicode_unihan_variant(0x2F9C8, kCompatibilityVariant, 0x4635).
unicode_unihan_variant(0x2F9C9, kCompatibilityVariant, 0x88FA).
unicode_unihan_variant(0x2F9CA, kCompatibilityVariant, 0x34BB).
unicode_unihan_variant(0x2F9CB, kCompatibilityVariant, 0x278AE).
unicode_unihan_variant(0x2F9CC, kCompatibilityVariant, 0x27966).
unicode_unihan_variant(0x2F9CD, kCompatibilityVariant, 0x46BE).
unicode_unihan_variant(0x2F9CE, kCompatibilityVariant, 0x46C7).
unicode_unihan_variant(0x2F9CF, kCompatibilityVariant, 0x8AA0).
unicode_unihan_variant(0x2F9D0, kCompatibilityVariant, 0x8AED).
unicode_unihan_variant(0x2F9D1, kCompatibilityVariant, 0x8B8A).
unicode_unihan_variant(0x2F9D2, kCompatibilityVariant, 0x8C55).
unicode_unihan_variant(0x2F9D3, kCompatibilityVariant, 0x27CA8).
unicode_unihan_variant(0x2F9D4, kCompatibilityVariant, 0x8CAB).
unicode_unihan_variant(0x2F9D5, kCompatibilityVariant, 0x8CC1).
unicode_unihan_variant(0x2F9D6, kCompatibilityVariant, 0x8D1B).
unicode_unihan_variant(0x2F9D7, kCompatibilityVariant, 0x8D77).
unicode_unihan_variant(0x2F9D8, kCompatibilityVariant, 0x27F2F).
unicode_unihan_variant(0x2F9D9, kCompatibilityVariant, 0x20804).
unicode_unihan_variant(0x2F9DA, kCompatibilityVariant, 0x8DCB).
unicode_unihan_variant(0x2F9DB, kCompatibilityVariant, 0x8DBC).
unicode_unihan_variant(0x2F9DC, kCompatibilityVariant, 0x8DF0).
unicode_unihan_variant(0x2F9DD, kCompatibilityVariant, 0x208DE).
unicode_unihan_variant(0x2F9DE, kCompatibilityVariant, 0x8ED4).
unicode_unihan_variant(0x2F9DF, kCompatibilityVariant, 0x8F38).
unicode_unihan_variant(0x2F9E0, kCompatibilityVariant, 0x285D2).
unicode_unihan_variant(0x2F9E1, kCompatibilityVariant, 0x285ED).
unicode_unihan_variant(0x2F9E2, kCompatibilityVariant, 0x9094).
unicode_unihan_variant(0x2F9E3, kCompatibilityVariant, 0x90F1).
unicode_unihan_variant(0x2F9E4, kCompatibilityVariant, 0x9111).
unicode_unihan_variant(0x2F9E5, kCompatibilityVariant, 0x2872E).
unicode_unihan_variant(0x2F9E6, kCompatibilityVariant, 0x911B).
unicode_unihan_variant(0x2F9E7, kCompatibilityVariant, 0x9238).
unicode_unihan_variant(0x2F9E8, kCompatibilityVariant, 0x92D7).
unicode_unihan_variant(0x2F9E9, kCompatibilityVariant, 0x92D8).
unicode_unihan_variant(0x2F9EA, kCompatibilityVariant, 0x927C).
unicode_unihan_variant(0x2F9EB, kCompatibilityVariant, 0x93F9).
unicode_unihan_variant(0x2F9EC, kCompatibilityVariant, 0x9415).
unicode_unihan_variant(0x2F9ED, kCompatibilityVariant, 0x28BFA).
unicode_unihan_variant(0x2F9EE, kCompatibilityVariant, 0x958B).
unicode_unihan_variant(0x2F9EF, kCompatibilityVariant, 0x4995).
unicode_unihan_variant(0x2F9F0, kCompatibilityVariant, 0x95B7).
unicode_unihan_variant(0x2F9F1, kCompatibilityVariant, 0x28D77).
unicode_unihan_variant(0x2F9F2, kCompatibilityVariant, 0x49E6).
unicode_unihan_variant(0x2F9F3, kCompatibilityVariant, 0x96C3).
unicode_unihan_variant(0x2F9F4, kCompatibilityVariant, 0x5DB2).
unicode_unihan_variant(0x2F9F5, kCompatibilityVariant, 0x9723).
unicode_unihan_variant(0x2F9F6, kCompatibilityVariant, 0x29145).
unicode_unihan_variant(0x2F9F7, kCompatibilityVariant, 0x2921A).
unicode_unihan_variant(0x2F9F8, kCompatibilityVariant, 0x4A6E).
unicode_unihan_variant(0x2F9F9, kCompatibilityVariant, 0x4A76).
unicode_unihan_variant(0x2F9FA, kCompatibilityVariant, 0x97E0).
unicode_unihan_variant(0x2F9FB, kCompatibilityVariant, 0x2940A).
unicode_unihan_variant(0x2F9FC, kCompatibilityVariant, 0x4AB2).
unicode_unihan_variant(0x2F9FD, kCompatibilityVariant, 0x29496).
unicode_unihan_variant(0x2F9FE, kCompatibilityVariant, 0x980B).
unicode_unihan_variant(0x2F9FF, kCompatibilityVariant, 0x980B).
unicode_unihan_variant(0x2FA00, kCompatibilityVariant, 0x9829).
unicode_unihan_variant(0x2FA01, kCompatibilityVariant, 0x295B6).
unicode_unihan_variant(0x2FA02, kCompatibilityVariant, 0x98E2).
unicode_unihan_variant(0x2FA03, kCompatibilityVariant, 0x4B33).
unicode_unihan_variant(0x2FA04, kCompatibilityVariant, 0x9929).
unicode_unihan_variant(0x2FA05, kCompatibilityVariant, 0x99A7).
unicode_unihan_variant(0x2FA06, kCompatibilityVariant, 0x99C2).
unicode_unihan_variant(0x2FA07, kCompatibilityVariant, 0x99FE).
unicode_unihan_variant(0x2FA08, kCompatibilityVariant, 0x4BCE).
unicode_unihan_variant(0x2FA09, kCompatibilityVariant, 0x29B30).
unicode_unihan_variant(0x2FA0A, kCompatibilityVariant, 0x9B12).
unicode_unihan_variant(0x2FA0B, kCompatibilityVariant, 0x9C40).
unicode_unihan_variant(0x2FA0C, kCompatibilityVariant, 0x9CFD).
unicode_unihan_variant(0x2FA0D, kCompatibilityVariant, 0x4CCE).
unicode_unihan_variant(0x2FA0E, kCompatibilityVariant, 0x4CED).
unicode_unihan_variant(0x2FA0F, kCompatibilityVariant, 0x9D67).
unicode_unihan_variant(0x2FA10, kCompatibilityVariant, 0x2A0CE).
unicode_unihan_variant(0x2FA11, kCompatibilityVariant, 0x4CF8).
unicode_unihan_variant(0x2FA12, kCompatibilityVariant, 0x2A105).
unicode_unihan_variant(0x2FA13, kCompatibilityVariant, 0x2A20E).
unicode_unihan_variant(0x2FA14, kCompatibilityVariant, 0x2A291).
unicode_unihan_variant(0x2FA15, kCompatibilityVariant, 0x9EBB).
unicode_unihan_variant(0x2FA16, kCompatibilityVariant, 0x4D56).
unicode_unihan_variant(0x2FA17, kCompatibilityVariant, 0x9EF9).
unicode_unihan_variant(0x2FA18, kCompatibilityVariant, 0x9EFE).
unicode_unihan_variant(0x2FA19, kCompatibilityVariant, 0x9F05).
unicode_unihan_variant(0x2FA1A, kCompatibilityVariant, 0x9F0F).
unicode_unihan_variant(0x2FA1B, kCompatibilityVariant, 0x9F16).
unicode_unihan_variant(0x2FA1C, kCompatibilityVariant, 0x9F3B).
unicode_unihan_variant(0x2FA1D, kCompatibilityVariant, 0x2A600).

% EOF
