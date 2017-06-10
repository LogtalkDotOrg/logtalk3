%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: June 10, 2017
%
%  This file simply loads all the files in this Unicode resource.
%  Due to large size of the files, we use a reload(skip) compiler
%  option to prevent accidental reload due to e.g. changes to the
%  default compilation mode.

:- initialization(
	logtalk_load([
		unicode_arabic_shaping,
		unicode_bidi_mirroring,
		unicode_blocks,
		unicode_case_folding,
		unicode_categories,
		unicode_cjk_radicals,
		unicode_composition_exclusions,
		unicode_core_properties,
		unicode_decomposition_type,
		unicode_derived_age,
		unicode_derived_bidi_class,
		unicode_derived_combining_class,
		unicode_derived_core_properties,
		unicode_derived_decomposition_type,
		unicode_derived_east_asian_width,
		unicode_derived_joining_group,
		unicode_derived_joining_type,
		unicode_derived_line_break,
		unicode_derived_normalization_props,
		unicode_derived_numeric_type,
		unicode_derived_numeric_values,
		unicode_hangul_syllable_type,
		unicode_indic_matra_category,
		unicode_indic_syllabic_category,
		unicode_jamo,
		unicode_name_aliases,
		unicode_names,
		unicode_prop_list,
		unicode_script_extensions,
		unicode_scripts,
		unicode_special_casing,
		unicode_unihan_variants,
		unicode_version
	], [
		reload(skip)
	])
).
