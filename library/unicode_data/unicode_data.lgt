%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: February 15, 2017
%
%  This file simply loads all the files in these resources

:- initialization((
	logtalk_load(unicode_arabic_shaping),
	logtalk_load(unicode_bidi_mirroring),
	logtalk_load(unicode_blocks),
	logtalk_load(unicode_case_folding),
	logtalk_load(unicode_categories),
	logtalk_load(unicode_cjk_radicals),
	logtalk_load(unicode_composition_exclusions),
	logtalk_load(unicode_core_properties),
	logtalk_load(unicode_decomposition_type),
	logtalk_load(unicode_derived_age),
	logtalk_load(unicode_derived_bidi_class),
	logtalk_load(unicode_derived_combining_class),
	logtalk_load(unicode_derived_core_properties),
	logtalk_load(unicode_derived_decomposition_type),
	logtalk_load(unicode_derived_east_asian_width),
	logtalk_load(unicode_derived_joining_group),
	logtalk_load(unicode_derived_joining_type),
	logtalk_load(unicode_derived_line_break),
	logtalk_load(unicode_derived_normalization_props),
	logtalk_load(unicode_derived_numeric_type),
	logtalk_load(unicode_derived_numeric_values),
	logtalk_load(unicode_hangul_syllable_type),
	logtalk_load(unicode_indic_matra_category),
	logtalk_load(unicode_indic_syllabic_category),
	logtalk_load(unicode_jamo),
	logtalk_load(unicode_name_aliases),
	logtalk_load(unicode_names),
	logtalk_load(unicode_prop_list),
	logtalk_load(unicode_script_extensions),
	logtalk_load(unicode_scripts),
	logtalk_load(unicode_special_casing),
	logtalk_load(unicode_unihan_variants),
	logtalk_load(unicode_version)
)).
