VivoMind Prolog Unicode Resources
=================================

Authors
-------
Arun Majumdar @ VivoMind LLC  
Paulo Moura @ VivoMind LLC

License
-------
Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication:

  http://creativecommons.org/publicdomain/zero/1.0/

We do appreciate acknowledgement if you use these resources, however, and we
also welcome contributions to improve them.

Website
--------
The latest release of the VivoMind Prolog Unicode Resources is available at
the URL:

  https://github.com/VivoMind

At this address you can also find additional information about the VivoMind
Prolog Unicode Resources and submit your bug reports and contributions.

Description
-----------
The VivoMind Prolog Unicode Resources are a set of files resulting from the
conversion of most (but not all) official UCD 6.1 files and updated for the
few changes in the 6.2 standard. The original files can be downloaded from:

  http://www.unicode.org

The conversion of the UCD files resulted in a large number of Prolog tables
and also a set of auxiliary predicates (described below) for accessing these
tables. Other than the obvious conversion in the provided predicate names,
no attempt was made to convert the identifiers used for properties and other
data.

Requirements
------------
Most of the auxiliary predicates assume that the de facto Prolog standard
predicate `between/3` is available. Unicode code point values are represented
using the ISO Prolog standard notation for hexadecimal integers. In addition,
the ISO Prolog standard directives `include/1` and `ensure_loaded/1` are used
in some of the files to load auxiliary files.

Usage
-----
Most applications only require some of the tables present in these resources.
Most of these tables define properties for ranges of code points and not for
single code points but the provided auxiliary predicates allow access for a
single code point. When increased performance is required, consider using the
existing tables and auxiliary predicates to generated derived tables more fit
for your specific application.

Known issues
------------
In the file `unicode_unihan_variant.pl`, when there's more than one variant
for a code point, only the first one (as listed in the original UCD file) is
returned.

The `include/1` and `ensure_loaded/1` directives are specified in the ISO
Prolog standard published in 1995. But some Prolog compilers either don't
implement one or both directives or have flawed implementations. Thus, you
may need to change how some of the files are loaded depending on the chosen
Prolog compiler. Using conditional compilation directives would help in some
cases but it would also rise portability issues on their own.

Acknowledgements
----------------
We thank Richard O'Keefe for helpful suggestions to improve the usability of
these resources.

Files and API Summary
---------------------
The Prolog file names are derived from the original file names by prefixing
them with the `unicode_` string, converting to lower case, and replacing the
camel case spelling with underscores. There are, however, two exceptions:
the files and directories holding the code point categories and names.

There's also an utility file, `unicode_data.pl`, that can be used to load all
the files in these resources. Is mostly used to test portability of the code
across Prolog compilers. Also included is a Logtalk version of this file,
`unicode_data.lgt`, that uses Logtalk's own implementation of the `include/1`
directive and the `logtak_load/1` predicate to load all files. This file can
be used to workaround Prolog systems with buggy or missing implementations of
the `ensure_loaded/1` and `include/1` directives.

An overview of the original file names and the code point properties can be
found at:

  http://www.unicode.org/reports/tr44/#Directory_Structure  
  http://www.unicode.org/reports/tr44/#Property_Definitions

* File:  
	`unicode_arabic_shaping.pl`  
* Provides:  
	`unicode_arabic_shaping/4`  
* Dependencies:  
	(none)

* File:  
	`unicode_bidi_mirroring.pl`  
* Provides:  
	`unicode_bidi_mirroring/2`  
* Dependencies:  
	(none)

* File:  
	`unicode_blocks.pl`  
* Provides:  
	`unicode_block/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_case_folding.pl`  
* Provides:  
	`unicode_case_folding/3`  
* Dependencies:  
	(none)

* File:  
	`unicode_categories.pl`  
* Provides:  
	`unicode_category/2`  
* Dependencies:  
	files in the `unicode_categories` directory  

* File:  
	`unicode_cjk_radicals.pl`  
* Provides:  
	`unicode_cjk_radical/3`  
* Dependencies:  
	(none)

* File:  
	`unicode_composition_exclusions.pl`  
* Provides:  
	`unicode_composition_exclusion/1`  
* Dependencies:  
	(none)

* File:  
	`unicode_core_properties.pl`  
* Provides:  
	`unicode_math/1-2`  
	`unicode_alphabetic/1-2`  
	`unicode_range_alphabetic/2`  
	`unicode_lowercase/1-2`  
	`unicode_uppercase/1-2`  
	`unicode_cased/1-2`  
	`unicode_case_ignorable/1-2`  
	`unicode_changes_when_lowercased/1-2`  
	`unicode_changes_when_uppercased/1-2`  
	`unicode_changes_when_titlecased/1-2`  
	`unicode_changes_when_casefolded/1-2`  
	`unicode_changes_when_casemapped/1-2`  
	`unicode_id_start/1-2`  
	`unicode_id_continue/1-2`  
	`unicode_xid_start/1-2`  
	`unicode_xid_continue/1-2`  
	`unicode_default_ignorable/1-2`  
	`unicode_grapheme_extend/1-2`  
	`unicode_grapheme_base/1-2`  
	`unicode_grapheme_link/1-2`  
* Dependencies:  
	files in the `unicode_core_properties` directory

* File:  
	`unicode_decomposition_type.pl`  
* Provides:  
	`unicode_canonical/1-2`  
	`unicode_compat/1-2`  
	`unicode_font/1-2`  
	`unicode_nobreak/1-2`  
	`unicode_initial/1-2`  
	`unicode_medial/1-2`  
	`unicode_final/1-2`  
	`unicode_isolated/1-2`  
	`unicode_circle/1-2`  
	`unicode_super/1-2`  
	`unicode_sub/1-2`  
	`unicode_vertical/1-2`  
	`unicode_wide/1-2`  
	`unicode_narrow/1-2`  
	`unicode_small/1-2`  
	`unicode_square/1-2`  
	`unicode_fraction/1-2`  
* Dependencies:  
	files in the `unicode_decomposition_type` directory

* File:  
	`unicode_derived_age.pl`  
* Provides:  
	`unicode_age/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_derived_bidi_class.pl`  
* Provides:  
	`unicode_bidi_class/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_derived_combining_class.pl`  
* Provides:  
	`unicode_combining_class/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_derived_core_properties.pl`  
* Provides:  
	`unicode_core_property/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_derived_decomposition_type.pl`  
* Provides:  
	`unicode_decomposition_type/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_derived_east_asian_width.pl`  
* Provides:  
	`unicode_east_asian_width/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_derived_joining_group.pl`  
* Provides:  
	`unicode_joining_group/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_derived_joining_type.pl`  
* Provides:  
	`unicode_joining_type/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_derived_line_break.pl`  
* Provides:  
	`unicode_line_break/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_derived_normalization_props.pl`  
* Provides:  
	`unicode_fc_nfkc/2`  
	`unicode_nfkc_cf/2`  
	`unicode_full_composition_exclusion/1-2`  
	`unicode_nfd_qc_no/1-2`  
	`unicode_nfc_qc_no/1-2`  
	`unicode_nfc_qc_maybe/1-2`  
	`unicode_nfkd_qc_no/1-2`  
	`unicode_nfkc_qc_no/1-2`  
	`unicode_nfkc_qc_maybe/1-2`  
	`unicode_expands_on_nfd/1-2`  
	`unicode_expands_on_nfc/1-2`  
	`unicode_expands_on_nfkd/1-2`  
	`unicode_expands_on_nfkc/1-2`  
	`unicode_changes_when_nfkc_casefolded/1-2`  
* Dependencies:  
	files in the `unicode_derived_normalization_props` directory

* File:  
	`unicode_derived_numeric_type.pl`  
* Provides:  
	`unicode_numeric_type/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_derived_numeric_values.pl`  
* Provides:  
	`unicode_numerical_value/3`  
* Dependencies:  
	(none)

* File:  
	`unicode_hangul_syllable_type.pl`  
* Provides:  
	`unicode_hangul_syllable_type/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_indic_matra_category.pl`  
* Provides:  
	`unicode_indic_matra_category/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_indic_syllabic_category.pl`  
* Provides:  
	`unicode_indic_syllabic_category/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_jamo.pl`  
* Provides:  
	`unicode_jamo/2`  
* Dependencies:  
	(none)

* File:  
	`unicode_name_aliases.pl`  
* Provides:  
	`unicode_name_alias/3`  
* Dependencies:  
	(none)

* File:  
	`unicode_names.pl`  
* Provides:  
	`unicode_name/2`  
* Dependencies:  
	files in the `unicode_names` directory

* File:  
	`unicode_prop_list.pl`  
* Provides:  
	`unicode_white_space/1-2`  
	`unicode_bidi_control/1-2`  
	`unicode_join_control/1-2`  
	`unicode_dash/1-2`  
	`unicode_hyphen/1-2`  
	`unicode_quotation_mark/1-2`  
	`unicode_terminal_punctuation/1-2`  
	`unicode_other_math/1-2`  
	`unicode_hex_digit/1-2`  
	`unicode_ascii_hex_digit/1-2`  
	`unicode_other_alphabetic/1-2`  
	`unicode_ideographic/1-2`  
	`unicode_diacritic/1-2`  
	`unicode_extender/1-2`  
	`unicode_other_lowercase/1-2`  
	`unicode_other_uppercase/1-2`  
	`unicode_noncharacter_code_point/1-2`  
	`unicode_other_grapheme_extend/1-2`  
	`unicode_ids_binary_operator/1-2`  
	`unicode_ids_trinary_operator/1-2`  
	`unicode_radical/1-2`  
	`unicode_unified_ideograph/1-2`  
	`unicode_other_default_ignorable/1-2`  
	`unicode_deprecated/1-2`  
	`unicode_soft_dotted/1-2`  
	`unicode_logical_order_exception/1-2`  
	`unicode_other_id_start/1-2`  
	`unicode_other_id_continue/1-2`  
	`unicode_sterm/1-2`  
	`unicode_variation_selector/1-2`  
	`unicode_pattern_white_space/1-2`  
	`unicode_pattern_syntax/1-2`  
* Dependencies:  
	files in the `unicode_prop_list` directory

* File:  
	`unicode_range_scripts.pl`  
* Provides:  
	`unicode_range_script/3`  
	`unicode_script/2`  
* Dependencies:  
	(none)

* File:  
	`unicode_script_extensions.pl`  
* Provides:  
	`unicode_script_extension/2-3`  
* Dependencies:  
	`unicode_scripts.pl`  

* File:  
	`unicode_scripts.pl`  
* Provides:  
	`unicode_script/6`  
	`unicode_script_category/3`  
* Dependencies:  
	(none)

* File:  
	`unicode_special_casing.pl`  
* Provides:  
	`unicode_special_casing/5`  
* Dependencies:  
	(none)

* File:  
	`unicode_unihan_variants.pl`  
* Provides:  
	`unicode_unihan_variant/2-3`  
* Dependencies:  
	(none)

* File:  
	`unicode_version.pl`  
* Provides:  
	`unicode_version/3`  
* Dependencies:  
	(none)
