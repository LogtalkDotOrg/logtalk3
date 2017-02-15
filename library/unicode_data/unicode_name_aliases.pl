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
# NameAliases-6.1.0.txt
# Date: 2012-01-03, 21:52:00 GMT [KW]
#
# This file is a normative contributory data file in the
# Unicode Character Database.
#
# Copyright (c) 2005-2012 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
#
# This file defines the formal name aliases for Unicode characters.
#
# For informative aliases see NamesList.txt
#
# The formal name aliases are divided into five types.
#
# 1. Corrections for serious problems in the character names
# 2. ISO 6429 names for C0 and C1 control functions, and other
#    commonly occurring names for control codes
# 3. A few widely used alternate names for format characters
# 4. Several documented labels for C1 control code points which
#    were never actually approved in any standard
# 5. Commonly occurring abbreviations (or acronyms) for control codes,
#    format characters, spaces, and variation selectors
#
# The formal name aliases are part of the Unicode character namespace, which
# includes the character names and the names of named character sequences.
# The inclusion of ISO 6429 names and other commonly occurring names and
# abbreviations for control codes and format characters as formal name aliases
# is to help avoid name collisions between Unicode character names and the 
# labels which commonly appear in text and/or in implementations such as regex, for
# control codes (which have no Unicode character name) or for format characters.
#
# For documentation, see NamesList.html and http://www.unicode.org/reports/tr44/
#
# FORMAT
#
# Each line has three fields, as described here:
#
# First field:  Code point
# Second field: Alias
# Third field:  Type
#
# The Type labels used are: correction, control, alternate, figment, abbreviation
#
# Those Type labels can be mapped to other strings for display, if desired.
#
# In case multiple aliases are assigned, additional aliases
# are provided on separate lines. Parsers of this data file should
# take note that the same code point can (and does) occur more than once.
#
#-----------------------------------------------------------------
*/

unicode_name_alias(0x0000, 'NULL', control).
unicode_name_alias(0x0000, 'NUL', abbreviation).
unicode_name_alias(0x0001, 'START OF HEADING', control).
unicode_name_alias(0x0001, 'SOH', abbreviation).
unicode_name_alias(0x0002, 'START OF TEXT', control).
unicode_name_alias(0x0002, 'STX', abbreviation).
unicode_name_alias(0x0003, 'END OF TEXT', control).
unicode_name_alias(0x0003, 'ETX', abbreviation).
unicode_name_alias(0x0004, 'END OF TRANSMISSION', control).
unicode_name_alias(0x0004, 'EOT', abbreviation).
unicode_name_alias(0x0005, 'ENQUIRY', control).
unicode_name_alias(0x0005, 'ENQ', abbreviation).
unicode_name_alias(0x0006, 'ACKNOWLEDGE', control).
unicode_name_alias(0x0006, 'ACK', abbreviation).

% Note that no formal name alias for the ISO 6429 "BELL" is
% provided for U+0007, because of the existing name collision
% with U+1F514 BELL.

unicode_name_alias(0x0007, 'ALERT', control).
unicode_name_alias(0x0007, 'BEL', abbreviation).
unicode_name_alias(0x0008, 'BACKSPACE', control).
unicode_name_alias(0x0008, 'BS', abbreviation).
unicode_name_alias(0x0009, 'CHARACTER TABULATION', control).
unicode_name_alias(0x0009, 'HORIZONTAL TABULATION', control).
unicode_name_alias(0x0009, 'HT', abbreviation).
unicode_name_alias(0x0009, 'TAB', abbreviation).
unicode_name_alias(0x000A, 'LINE FEED', control).
unicode_name_alias(0x000A, 'NEW LINE', control).
unicode_name_alias(0x000A, 'END OF LINE', control).
unicode_name_alias(0x000A, 'LF', abbreviation).
unicode_name_alias(0x000A, 'NL', abbreviation).
unicode_name_alias(0x000A, 'EOL', abbreviation).
unicode_name_alias(0x000B, 'LINE TABULATION', control).
unicode_name_alias(0x000B, 'VERTICAL TABULATION', control).
unicode_name_alias(0x000B, 'VT', abbreviation).
unicode_name_alias(0x000C, 'FORM FEED', control).
unicode_name_alias(0x000C, 'FF', abbreviation).
unicode_name_alias(0x000D, 'CARRIAGE RETURN', control).
unicode_name_alias(0x000D, 'CR', abbreviation).
unicode_name_alias(0x000E, 'SHIFT OUT', control).
unicode_name_alias(0x000E, 'LOCKING-SHIFT ONE', control).
unicode_name_alias(0x000E, 'SO', abbreviation).
unicode_name_alias(0x000F, 'SHIFT IN', control).
unicode_name_alias(0x000F, 'LOCKING-SHIFT ZERO', control).
unicode_name_alias(0x000F, 'SI', abbreviation).
unicode_name_alias(0x0010, 'DATA LINK ESCAPE', control).
unicode_name_alias(0x0010, 'DLE', abbreviation).
unicode_name_alias(0x0011, 'DEVICE CONTROL ONE', control).
unicode_name_alias(0x0011, 'DC1', abbreviation).
unicode_name_alias(0x0012, 'DEVICE CONTROL TWO', control).
unicode_name_alias(0x0012, 'DC2', abbreviation).
unicode_name_alias(0x0013, 'DEVICE CONTROL THREE', control).
unicode_name_alias(0x0013, 'DC3', abbreviation).
unicode_name_alias(0x0014, 'DEVICE CONTROL FOUR', control).
unicode_name_alias(0x0014, 'DC4', abbreviation).
unicode_name_alias(0x0015, 'NEGATIVE ACKNOWLEDGE', control).
unicode_name_alias(0x0015, 'NAK', abbreviation).
unicode_name_alias(0x0016, 'SYNCHRONOUS IDLE', control).
unicode_name_alias(0x0016, 'SYN', abbreviation).
unicode_name_alias(0x0017, 'END OF TRANSMISSION BLOCK', control).
unicode_name_alias(0x0017, 'ETB', abbreviation).
unicode_name_alias(0x0018, 'CANCEL', control).
unicode_name_alias(0x0018, 'CAN', abbreviation).
unicode_name_alias(0x0019, 'END OF MEDIUM', control).
unicode_name_alias(0x0019, 'EOM', abbreviation).
unicode_name_alias(0x001A, 'SUBSTITUTE', control).
unicode_name_alias(0x001A, 'SUB', abbreviation).
unicode_name_alias(0x001B, 'ESCAPE', control).
unicode_name_alias(0x001B, 'ESC', abbreviation).
unicode_name_alias(0x001C, 'INFORMATION SEPARATOR FOUR', control).
unicode_name_alias(0x001C, 'FILE SEPARATOR', control).
unicode_name_alias(0x001C, 'FS', abbreviation).
unicode_name_alias(0x001D, 'INFORMATION SEPARATOR THREE', control).
unicode_name_alias(0x001D, 'GROUP SEPARATOR', control).
unicode_name_alias(0x001D, 'GS', abbreviation).
unicode_name_alias(0x001E, 'INFORMATION SEPARATOR TWO', control).
unicode_name_alias(0x001E, 'RECORD SEPARATOR', control).
unicode_name_alias(0x001E, 'RS', abbreviation).
unicode_name_alias(0x001F, 'INFORMATION SEPARATOR ONE', control).
unicode_name_alias(0x001F, 'UNIT SEPARATOR', control).
unicode_name_alias(0x001F, 'US', abbreviation).
unicode_name_alias(0x0020, 'SP', abbreviation).
unicode_name_alias(0x007F, 'DELETE', control).
unicode_name_alias(0x007F, 'DEL', abbreviation).
unicode_name_alias(0x0080, 'PADDING CHARACTER', figment).
unicode_name_alias(0x0080, 'PAD', abbreviation).
unicode_name_alias(0x0081, 'HIGH OCTET PRESET', figment).
unicode_name_alias(0x0081, 'HOP', abbreviation).
unicode_name_alias(0x0082, 'BREAK PERMITTED HERE', control).
unicode_name_alias(0x0082, 'BPH', abbreviation).
unicode_name_alias(0x0083, 'NO BREAK HERE', control).
unicode_name_alias(0x0083, 'NBH', abbreviation).
unicode_name_alias(0x0084, 'INDEX', control).
unicode_name_alias(0x0084, 'IND', abbreviation).
unicode_name_alias(0x0085, 'NEXT LINE', control).
unicode_name_alias(0x0085, 'NEL', abbreviation).
unicode_name_alias(0x0086, 'START OF SELECTED AREA', control).
unicode_name_alias(0x0086, 'SSA', abbreviation).
unicode_name_alias(0x0087, 'END OF SELECTED AREA', control).
unicode_name_alias(0x0087, 'ESA', abbreviation).
unicode_name_alias(0x0088, 'CHARACTER TABULATION SET', control).
unicode_name_alias(0x0088, 'HORIZONTAL TABULATION SET', control).
unicode_name_alias(0x0088, 'HTS', abbreviation).
unicode_name_alias(0x0089, 'CHARACTER TABULATION WITH JUSTIFICATION', control).
unicode_name_alias(0x0089, 'HORIZONTAL TABULATION WITH JUSTIFICATION', control).
unicode_name_alias(0x0089, 'HTJ', abbreviation).
unicode_name_alias(0x008A, 'LINE TABULATION SET', control).
unicode_name_alias(0x008A, 'VERTICAL TABULATION SET', control).
unicode_name_alias(0x008A, 'VTS', abbreviation).
unicode_name_alias(0x008B, 'PARTIAL LINE FORWARD', control).
unicode_name_alias(0x008B, 'PARTIAL LINE DOWN', control).
unicode_name_alias(0x008B, 'PLD', abbreviation).
unicode_name_alias(0x008C, 'PARTIAL LINE BACKWARD', control).
unicode_name_alias(0x008C, 'PARTIAL LINE UP', control).
unicode_name_alias(0x008C, 'PLU', abbreviation).
unicode_name_alias(0x008D, 'REVERSE LINE FEED', control).
unicode_name_alias(0x008D, 'REVERSE INDEX', control).
unicode_name_alias(0x008D, 'RI', abbreviation).
unicode_name_alias(0x008E, 'SINGLE SHIFT TWO', control).
unicode_name_alias(0x008E, 'SINGLE-SHIFT-2', control).
unicode_name_alias(0x008E, 'SS2', abbreviation).
unicode_name_alias(0x008F, 'SINGLE SHIFT THREE', control).
unicode_name_alias(0x008F, 'SINGLE-SHIFT-3', control).
unicode_name_alias(0x008F, 'SS3', abbreviation).
unicode_name_alias(0x0090, 'DEVICE CONTROL STRING', control).
unicode_name_alias(0x0090, 'DCS', abbreviation).
unicode_name_alias(0x0091, 'PRIVATE USE ONE', control).
unicode_name_alias(0x0091, 'PRIVATE USE-1', control).
unicode_name_alias(0x0091, 'PU1', abbreviation).
unicode_name_alias(0x0092, 'PRIVATE USE TWO', control).
unicode_name_alias(0x0092, 'PRIVATE USE-2', control).
unicode_name_alias(0x0092, 'PU2', abbreviation).
unicode_name_alias(0x0093, 'SET TRANSMIT STATE', control).
unicode_name_alias(0x0093, 'STS', abbreviation).
unicode_name_alias(0x0094, 'CANCEL CHARACTER', control).
unicode_name_alias(0x0094, 'CCH', abbreviation).
unicode_name_alias(0x0095, 'MESSAGE WAITING', control).
unicode_name_alias(0x0095, 'MW', abbreviation).
unicode_name_alias(0x0096, 'START OF GUARDED AREA', control).
unicode_name_alias(0x0096, 'START OF PROTECTED AREA', control).
unicode_name_alias(0x0096, 'SPA', abbreviation).
unicode_name_alias(0x0097, 'END OF GUARDED AREA', control).
unicode_name_alias(0x0097, 'END OF PROTECTED AREA', control).
unicode_name_alias(0x0097, 'EPA', abbreviation).
unicode_name_alias(0x0098, 'START OF STRING', control).
unicode_name_alias(0x0098, 'SOS', abbreviation).
unicode_name_alias(0x0099, 'SINGLE GRAPHIC CHARACTER INTRODUCER', figment).
unicode_name_alias(0x0099, 'SGC', abbreviation).
unicode_name_alias(0x009A, 'SINGLE CHARACTER INTRODUCER', control).
unicode_name_alias(0x009A, 'SCI', abbreviation).
unicode_name_alias(0x009B, 'CONTROL SEQUENCE INTRODUCER', control).
unicode_name_alias(0x009B, 'CSI', abbreviation).
unicode_name_alias(0x009C, 'STRING TERMINATOR', control).
unicode_name_alias(0x009C, 'ST', abbreviation).
unicode_name_alias(0x009D, 'OPERATING SYSTEM COMMAND', control).
unicode_name_alias(0x009D, 'OSC', abbreviation).
unicode_name_alias(0x009E, 'PRIVACY MESSAGE', control).
unicode_name_alias(0x009E, 'PM', abbreviation).
unicode_name_alias(0x009F, 'APPLICATION PROGRAM COMMAND', control).
unicode_name_alias(0x009F, 'APC', abbreviation).
unicode_name_alias(0x00A0, 'NBSP', abbreviation).
unicode_name_alias(0x00AD, 'SHY', abbreviation).
unicode_name_alias(0x01A2, 'LATIN CAPITAL LETTER GHA', correction).
unicode_name_alias(0x01A3, 'LATIN SMALL LETTER GHA', correction).
unicode_name_alias(0x034F, 'CGJ', abbreviation).
unicode_name_alias(0x0CDE, 'KANNADA LETTER LLLA', correction).
unicode_name_alias(0x0E9D, 'LAO LETTER FO FON', correction).
unicode_name_alias(0x0E9F, 'LAO LETTER FO FAY', correction).
unicode_name_alias(0x0EA3, 'LAO LETTER RO', correction).
unicode_name_alias(0x0EA5, 'LAO LETTER LO', correction).
unicode_name_alias(0x0FD0, 'TIBETAN MARK BKA- SHOG GI MGO RGYAN', correction).
unicode_name_alias(0x180B, 'FVS1', abbreviation).
unicode_name_alias(0x180C, 'FVS2', abbreviation).
unicode_name_alias(0x180D, 'FVS3', abbreviation).
unicode_name_alias(0x180E, 'MVS', abbreviation).
unicode_name_alias(0x200B, 'ZWSP', abbreviation).
unicode_name_alias(0x200C, 'ZWNJ', abbreviation).
unicode_name_alias(0x200D, 'ZWJ', abbreviation).
unicode_name_alias(0x200E, 'LRM', abbreviation).
unicode_name_alias(0x200F, 'RLM', abbreviation).
unicode_name_alias(0x202A, 'LRE', abbreviation).
unicode_name_alias(0x202B, 'RLE', abbreviation).
unicode_name_alias(0x202C, 'PDF', abbreviation).
unicode_name_alias(0x202D, 'LRO', abbreviation).
unicode_name_alias(0x202E, 'RLO', abbreviation).
unicode_name_alias(0x202F, 'NNBSP', abbreviation).
unicode_name_alias(0x205F, 'MMSP', abbreviation).
unicode_name_alias(0x2060, 'WJ', abbreviation).
unicode_name_alias(0x2118, 'WEIERSTRASS ELLIPTIC FUNCTION', correction).
unicode_name_alias(0x2448, 'MICR ON US SYMBOL', correction).
unicode_name_alias(0x2449, 'MICR DASH SYMBOL', correction).
unicode_name_alias(0xA015, 'YI SYLLABLE ITERATION MARK', correction).
unicode_name_alias(0xFE18, 'PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRACKET', correction).
unicode_name_alias(0xFE00, 'VS1', abbreviation).
unicode_name_alias(0xFE01, 'VS2', abbreviation).
unicode_name_alias(0xFE02, 'VS3', abbreviation).
unicode_name_alias(0xFE03, 'VS4', abbreviation).
unicode_name_alias(0xFE04, 'VS5', abbreviation).
unicode_name_alias(0xFE05, 'VS6', abbreviation).
unicode_name_alias(0xFE06, 'VS7', abbreviation).
unicode_name_alias(0xFE07, 'VS8', abbreviation).
unicode_name_alias(0xFE08, 'VS9', abbreviation).
unicode_name_alias(0xFE09, 'VS10', abbreviation).
unicode_name_alias(0xFE0A, 'VS11', abbreviation).
unicode_name_alias(0xFE0B, 'VS12', abbreviation).
unicode_name_alias(0xFE0C, 'VS13', abbreviation).
unicode_name_alias(0xFE0D, 'VS14', abbreviation).
unicode_name_alias(0xFE0E, 'VS15', abbreviation).
unicode_name_alias(0xFE0F, 'VS16', abbreviation).
unicode_name_alias(0xFEFF, 'BYTE ORDER MARK', alternate).
unicode_name_alias(0xFEFF, 'BOM', abbreviation).
unicode_name_alias(0xFEFF, 'ZWNBSP', abbreviation).
unicode_name_alias(0x1D0C5, 'BYZANTINE MUSICAL SYMBOL FTHORA SKLIRON CHROMA VASIS', correction).
unicode_name_alias(0xE0100, 'VS17', abbreviation).
unicode_name_alias(0xE0101, 'VS18', abbreviation).
unicode_name_alias(0xE0102, 'VS19', abbreviation).
unicode_name_alias(0xE0103, 'VS20', abbreviation).
unicode_name_alias(0xE0104, 'VS21', abbreviation).
unicode_name_alias(0xE0105, 'VS22', abbreviation).
unicode_name_alias(0xE0106, 'VS23', abbreviation).
unicode_name_alias(0xE0107, 'VS24', abbreviation).
unicode_name_alias(0xE0108, 'VS25', abbreviation).
unicode_name_alias(0xE0109, 'VS26', abbreviation).
unicode_name_alias(0xE010A, 'VS27', abbreviation).
unicode_name_alias(0xE010B, 'VS28', abbreviation).
unicode_name_alias(0xE010C, 'VS29', abbreviation).
unicode_name_alias(0xE010D, 'VS30', abbreviation).
unicode_name_alias(0xE010E, 'VS31', abbreviation).
unicode_name_alias(0xE010F, 'VS32', abbreviation).
unicode_name_alias(0xE0110, 'VS33', abbreviation).
unicode_name_alias(0xE0111, 'VS34', abbreviation).
unicode_name_alias(0xE0112, 'VS35', abbreviation).
unicode_name_alias(0xE0113, 'VS36', abbreviation).
unicode_name_alias(0xE0114, 'VS37', abbreviation).
unicode_name_alias(0xE0115, 'VS38', abbreviation).
unicode_name_alias(0xE0116, 'VS39', abbreviation).
unicode_name_alias(0xE0117, 'VS40', abbreviation).
unicode_name_alias(0xE0118, 'VS41', abbreviation).
unicode_name_alias(0xE0119, 'VS42', abbreviation).
unicode_name_alias(0xE011A, 'VS43', abbreviation).
unicode_name_alias(0xE011B, 'VS44', abbreviation).
unicode_name_alias(0xE011C, 'VS45', abbreviation).
unicode_name_alias(0xE011D, 'VS46', abbreviation).
unicode_name_alias(0xE011E, 'VS47', abbreviation).
unicode_name_alias(0xE011F, 'VS48', abbreviation).
unicode_name_alias(0xE0120, 'VS49', abbreviation).
unicode_name_alias(0xE0121, 'VS50', abbreviation).
unicode_name_alias(0xE0122, 'VS51', abbreviation).
unicode_name_alias(0xE0123, 'VS52', abbreviation).
unicode_name_alias(0xE0124, 'VS53', abbreviation).
unicode_name_alias(0xE0125, 'VS54', abbreviation).
unicode_name_alias(0xE0126, 'VS55', abbreviation).
unicode_name_alias(0xE0127, 'VS56', abbreviation).
unicode_name_alias(0xE0128, 'VS57', abbreviation).
unicode_name_alias(0xE0129, 'VS58', abbreviation).
unicode_name_alias(0xE012A, 'VS59', abbreviation).
unicode_name_alias(0xE012B, 'VS60', abbreviation).
unicode_name_alias(0xE012C, 'VS61', abbreviation).
unicode_name_alias(0xE012D, 'VS62', abbreviation).
unicode_name_alias(0xE012E, 'VS63', abbreviation).
unicode_name_alias(0xE012F, 'VS64', abbreviation).
unicode_name_alias(0xE0130, 'VS65', abbreviation).
unicode_name_alias(0xE0131, 'VS66', abbreviation).
unicode_name_alias(0xE0132, 'VS67', abbreviation).
unicode_name_alias(0xE0133, 'VS68', abbreviation).
unicode_name_alias(0xE0134, 'VS69', abbreviation).
unicode_name_alias(0xE0135, 'VS70', abbreviation).
unicode_name_alias(0xE0136, 'VS71', abbreviation).
unicode_name_alias(0xE0137, 'VS72', abbreviation).
unicode_name_alias(0xE0138, 'VS73', abbreviation).
unicode_name_alias(0xE0139, 'VS74', abbreviation).
unicode_name_alias(0xE013A, 'VS75', abbreviation).
unicode_name_alias(0xE013B, 'VS76', abbreviation).
unicode_name_alias(0xE013C, 'VS77', abbreviation).
unicode_name_alias(0xE013D, 'VS78', abbreviation).
unicode_name_alias(0xE013E, 'VS79', abbreviation).
unicode_name_alias(0xE013F, 'VS80', abbreviation).
unicode_name_alias(0xE0140, 'VS81', abbreviation).
unicode_name_alias(0xE0141, 'VS82', abbreviation).
unicode_name_alias(0xE0142, 'VS83', abbreviation).
unicode_name_alias(0xE0143, 'VS84', abbreviation).
unicode_name_alias(0xE0144, 'VS85', abbreviation).
unicode_name_alias(0xE0145, 'VS86', abbreviation).
unicode_name_alias(0xE0146, 'VS87', abbreviation).
unicode_name_alias(0xE0147, 'VS88', abbreviation).
unicode_name_alias(0xE0148, 'VS89', abbreviation).
unicode_name_alias(0xE0149, 'VS90', abbreviation).
unicode_name_alias(0xE014A, 'VS91', abbreviation).
unicode_name_alias(0xE014B, 'VS92', abbreviation).
unicode_name_alias(0xE014C, 'VS93', abbreviation).
unicode_name_alias(0xE014D, 'VS94', abbreviation).
unicode_name_alias(0xE014E, 'VS95', abbreviation).
unicode_name_alias(0xE014F, 'VS96', abbreviation).
unicode_name_alias(0xE0150, 'VS97', abbreviation).
unicode_name_alias(0xE0151, 'VS98', abbreviation).
unicode_name_alias(0xE0152, 'VS99', abbreviation).
unicode_name_alias(0xE0153, 'VS100', abbreviation).
unicode_name_alias(0xE0154, 'VS101', abbreviation).
unicode_name_alias(0xE0155, 'VS102', abbreviation).
unicode_name_alias(0xE0156, 'VS103', abbreviation).
unicode_name_alias(0xE0157, 'VS104', abbreviation).
unicode_name_alias(0xE0158, 'VS105', abbreviation).
unicode_name_alias(0xE0159, 'VS106', abbreviation).
unicode_name_alias(0xE015A, 'VS107', abbreviation).
unicode_name_alias(0xE015B, 'VS108', abbreviation).
unicode_name_alias(0xE015C, 'VS109', abbreviation).
unicode_name_alias(0xE015D, 'VS110', abbreviation).
unicode_name_alias(0xE015E, 'VS111', abbreviation).
unicode_name_alias(0xE015F, 'VS112', abbreviation).
unicode_name_alias(0xE0160, 'VS113', abbreviation).
unicode_name_alias(0xE0161, 'VS114', abbreviation).
unicode_name_alias(0xE0162, 'VS115', abbreviation).
unicode_name_alias(0xE0163, 'VS116', abbreviation).
unicode_name_alias(0xE0164, 'VS117', abbreviation).
unicode_name_alias(0xE0165, 'VS118', abbreviation).
unicode_name_alias(0xE0166, 'VS119', abbreviation).
unicode_name_alias(0xE0167, 'VS120', abbreviation).
unicode_name_alias(0xE0168, 'VS121', abbreviation).
unicode_name_alias(0xE0169, 'VS122', abbreviation).
unicode_name_alias(0xE016A, 'VS123', abbreviation).
unicode_name_alias(0xE016B, 'VS124', abbreviation).
unicode_name_alias(0xE016C, 'VS125', abbreviation).
unicode_name_alias(0xE016D, 'VS126', abbreviation).
unicode_name_alias(0xE016E, 'VS127', abbreviation).
unicode_name_alias(0xE016F, 'VS128', abbreviation).
unicode_name_alias(0xE0170, 'VS129', abbreviation).
unicode_name_alias(0xE0171, 'VS130', abbreviation).
unicode_name_alias(0xE0172, 'VS131', abbreviation).
unicode_name_alias(0xE0173, 'VS132', abbreviation).
unicode_name_alias(0xE0174, 'VS133', abbreviation).
unicode_name_alias(0xE0175, 'VS134', abbreviation).
unicode_name_alias(0xE0176, 'VS135', abbreviation).
unicode_name_alias(0xE0177, 'VS136', abbreviation).
unicode_name_alias(0xE0178, 'VS137', abbreviation).
unicode_name_alias(0xE0179, 'VS138', abbreviation).
unicode_name_alias(0xE017A, 'VS139', abbreviation).
unicode_name_alias(0xE017B, 'VS140', abbreviation).
unicode_name_alias(0xE017C, 'VS141', abbreviation).
unicode_name_alias(0xE017D, 'VS142', abbreviation).
unicode_name_alias(0xE017E, 'VS143', abbreviation).
unicode_name_alias(0xE017F, 'VS144', abbreviation).
unicode_name_alias(0xE0180, 'VS145', abbreviation).
unicode_name_alias(0xE0181, 'VS146', abbreviation).
unicode_name_alias(0xE0182, 'VS147', abbreviation).
unicode_name_alias(0xE0183, 'VS148', abbreviation).
unicode_name_alias(0xE0184, 'VS149', abbreviation).
unicode_name_alias(0xE0185, 'VS150', abbreviation).
unicode_name_alias(0xE0186, 'VS151', abbreviation).
unicode_name_alias(0xE0187, 'VS152', abbreviation).
unicode_name_alias(0xE0188, 'VS153', abbreviation).
unicode_name_alias(0xE0189, 'VS154', abbreviation).
unicode_name_alias(0xE018A, 'VS155', abbreviation).
unicode_name_alias(0xE018B, 'VS156', abbreviation).
unicode_name_alias(0xE018C, 'VS157', abbreviation).
unicode_name_alias(0xE018D, 'VS158', abbreviation).
unicode_name_alias(0xE018E, 'VS159', abbreviation).
unicode_name_alias(0xE018F, 'VS160', abbreviation).
unicode_name_alias(0xE0190, 'VS161', abbreviation).
unicode_name_alias(0xE0191, 'VS162', abbreviation).
unicode_name_alias(0xE0192, 'VS163', abbreviation).
unicode_name_alias(0xE0193, 'VS164', abbreviation).
unicode_name_alias(0xE0194, 'VS165', abbreviation).
unicode_name_alias(0xE0195, 'VS166', abbreviation).
unicode_name_alias(0xE0196, 'VS167', abbreviation).
unicode_name_alias(0xE0197, 'VS168', abbreviation).
unicode_name_alias(0xE0198, 'VS169', abbreviation).
unicode_name_alias(0xE0199, 'VS170', abbreviation).
unicode_name_alias(0xE019A, 'VS171', abbreviation).
unicode_name_alias(0xE019B, 'VS172', abbreviation).
unicode_name_alias(0xE019C, 'VS173', abbreviation).
unicode_name_alias(0xE019D, 'VS174', abbreviation).
unicode_name_alias(0xE019E, 'VS175', abbreviation).
unicode_name_alias(0xE019F, 'VS176', abbreviation).
unicode_name_alias(0xE01A0, 'VS177', abbreviation).
unicode_name_alias(0xE01A1, 'VS178', abbreviation).
unicode_name_alias(0xE01A2, 'VS179', abbreviation).
unicode_name_alias(0xE01A3, 'VS180', abbreviation).
unicode_name_alias(0xE01A4, 'VS181', abbreviation).
unicode_name_alias(0xE01A5, 'VS182', abbreviation).
unicode_name_alias(0xE01A6, 'VS183', abbreviation).
unicode_name_alias(0xE01A7, 'VS184', abbreviation).
unicode_name_alias(0xE01A8, 'VS185', abbreviation).
unicode_name_alias(0xE01A9, 'VS186', abbreviation).
unicode_name_alias(0xE01AA, 'VS187', abbreviation).
unicode_name_alias(0xE01AB, 'VS188', abbreviation).
unicode_name_alias(0xE01AC, 'VS189', abbreviation).
unicode_name_alias(0xE01AD, 'VS190', abbreviation).
unicode_name_alias(0xE01AE, 'VS191', abbreviation).
unicode_name_alias(0xE01AF, 'VS192', abbreviation).
unicode_name_alias(0xE01B0, 'VS193', abbreviation).
unicode_name_alias(0xE01B1, 'VS194', abbreviation).
unicode_name_alias(0xE01B2, 'VS195', abbreviation).
unicode_name_alias(0xE01B3, 'VS196', abbreviation).
unicode_name_alias(0xE01B4, 'VS197', abbreviation).
unicode_name_alias(0xE01B5, 'VS198', abbreviation).
unicode_name_alias(0xE01B6, 'VS199', abbreviation).
unicode_name_alias(0xE01B7, 'VS200', abbreviation).
unicode_name_alias(0xE01B8, 'VS201', abbreviation).
unicode_name_alias(0xE01B9, 'VS202', abbreviation).
unicode_name_alias(0xE01BA, 'VS203', abbreviation).
unicode_name_alias(0xE01BB, 'VS204', abbreviation).
unicode_name_alias(0xE01BC, 'VS205', abbreviation).
unicode_name_alias(0xE01BD, 'VS206', abbreviation).
unicode_name_alias(0xE01BE, 'VS207', abbreviation).
unicode_name_alias(0xE01BF, 'VS208', abbreviation).
unicode_name_alias(0xE01C0, 'VS209', abbreviation).
unicode_name_alias(0xE01C1, 'VS210', abbreviation).
unicode_name_alias(0xE01C2, 'VS211', abbreviation).
unicode_name_alias(0xE01C3, 'VS212', abbreviation).
unicode_name_alias(0xE01C4, 'VS213', abbreviation).
unicode_name_alias(0xE01C5, 'VS214', abbreviation).
unicode_name_alias(0xE01C6, 'VS215', abbreviation).
unicode_name_alias(0xE01C7, 'VS216', abbreviation).
unicode_name_alias(0xE01C8, 'VS217', abbreviation).
unicode_name_alias(0xE01C9, 'VS218', abbreviation).
unicode_name_alias(0xE01CA, 'VS219', abbreviation).
unicode_name_alias(0xE01CB, 'VS220', abbreviation).
unicode_name_alias(0xE01CC, 'VS221', abbreviation).
unicode_name_alias(0xE01CD, 'VS222', abbreviation).
unicode_name_alias(0xE01CE, 'VS223', abbreviation).
unicode_name_alias(0xE01CF, 'VS224', abbreviation).
unicode_name_alias(0xE01D0, 'VS225', abbreviation).
unicode_name_alias(0xE01D1, 'VS226', abbreviation).
unicode_name_alias(0xE01D2, 'VS227', abbreviation).
unicode_name_alias(0xE01D3, 'VS228', abbreviation).
unicode_name_alias(0xE01D4, 'VS229', abbreviation).
unicode_name_alias(0xE01D5, 'VS230', abbreviation).
unicode_name_alias(0xE01D6, 'VS231', abbreviation).
unicode_name_alias(0xE01D7, 'VS232', abbreviation).
unicode_name_alias(0xE01D8, 'VS233', abbreviation).
unicode_name_alias(0xE01D9, 'VS234', abbreviation).
unicode_name_alias(0xE01DA, 'VS235', abbreviation).
unicode_name_alias(0xE01DB, 'VS236', abbreviation).
unicode_name_alias(0xE01DC, 'VS237', abbreviation).
unicode_name_alias(0xE01DD, 'VS238', abbreviation).
unicode_name_alias(0xE01DE, 'VS239', abbreviation).
unicode_name_alias(0xE01DF, 'VS240', abbreviation).
unicode_name_alias(0xE01E0, 'VS241', abbreviation).
unicode_name_alias(0xE01E1, 'VS242', abbreviation).
unicode_name_alias(0xE01E2, 'VS243', abbreviation).
unicode_name_alias(0xE01E3, 'VS244', abbreviation).
unicode_name_alias(0xE01E4, 'VS245', abbreviation).
unicode_name_alias(0xE01E5, 'VS246', abbreviation).
unicode_name_alias(0xE01E6, 'VS247', abbreviation).
unicode_name_alias(0xE01E7, 'VS248', abbreviation).
unicode_name_alias(0xE01E8, 'VS249', abbreviation).
unicode_name_alias(0xE01E9, 'VS250', abbreviation).
unicode_name_alias(0xE01EA, 'VS251', abbreviation).
unicode_name_alias(0xE01EB, 'VS252', abbreviation).
unicode_name_alias(0xE01EC, 'VS253', abbreviation).
unicode_name_alias(0xE01ED, 'VS254', abbreviation).
unicode_name_alias(0xE01EE, 'VS255', abbreviation).
unicode_name_alias(0xE01EF, 'VS256', abbreviation).

% EOF
