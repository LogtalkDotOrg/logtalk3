#!/usr/bin/env python3

# This file is part of Logtalk <https://logtalk.org/>
# SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
# SPDX-License-Identifier: Apache-2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Generate Unicode 17.0 tables and common HTML entity tables."""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path


HEADER = """%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the \"License\");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an \"AS IS\" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
"""


def code_list(values: tuple[int, ...] | list[int]) -> str:
	return "[" + ",".join(str(value) for value in values) + "]"


def parse_range(value: str) -> tuple[int, int]:
	parts = value.strip().split("..")
	start = int(parts[0], 16)
	return start, int(parts[-1], 16)


def parse_property(path: Path, names: set[str]) -> dict[str, list[tuple[int, int]]]:
	properties = {name: [] for name in names}
	for raw_line in path.read_text(encoding="utf-8").splitlines():
		line = raw_line.split("#", 1)[0].strip()
		if not line:
			continue
		value, name = (part.strip() for part in line.split(";", 1))
		if name in properties:
			properties[name].append(parse_range(value))
	return properties


def parse_all_properties(path: Path) -> dict[str, list[tuple[int, int]]]:
	properties: dict[str, list[tuple[int, int]]] = {}
	for raw_line in path.read_text(encoding="utf-8").splitlines():
		line = raw_line.split("#", 1)[0].strip()
		if not line:
			continue
		parts = [part.strip() for part in line.split(";")]
		properties.setdefault(parts[1], []).append(parse_range(parts[0]))
	return properties


def parse_range_records(path: Path) -> list[tuple[int, int, str]]:
	records: list[tuple[int, int, str]] = []
	for raw_line in path.read_text(encoding="utf-8").splitlines():
		line = raw_line.split("#", 1)[0].strip()
		if not line:
			continue
		parts = [part.strip() for part in line.split(";")]
		start, end = parse_range(parts[0])
		records.append((start, end, parts[1]))
	return records


def parse_script_records(path: Path) -> list[tuple[int, int, str, str]]:
	records = []
	for raw_line in path.read_text(encoding="utf-8").splitlines():
		data, separator, comment = raw_line.partition("#")
		data = data.strip()
		if not data:
			continue
		parts = [part.strip() for part in data.split(";")]
		start, end = parse_range(parts[0])
		description = ""
		if separator:
			match = re.match(r"\S+\s+(?:\[\d+\]\s+)?(.*)", comment.strip())
			if match:
				description = match.group(1)
		records.append((start, end, parts[1], description))
	return records


def prolog_atom(value: str) -> str:
	return "'" + value.replace("'", "''") + "'"


def parse_exclusions(source_dir: Path) -> set[int]:
	exclusions: set[int] = set()
	for filename in ("CompositionExclusions.txt", "DerivedNormalizationProps.txt"):
		for raw_line in (source_dir / filename).read_text(encoding="utf-8").splitlines():
			line = raw_line.split("#", 1)[0].strip()
			if not line:
				continue
			parts = [part.strip() for part in line.split(";")]
			if filename == "DerivedNormalizationProps.txt" and parts[1] != "Full_Composition_Exclusion":
				continue
			start, end = parse_range(parts[0])
			exclusions.update(range(start, end + 1))
	return exclusions


def parse_unicode_data(path: Path) -> dict[str, dict[int, object]]:
	data: dict[str, dict[int, object]] = {
		"category": {}, "ccc": {}, "canonical": {}, "compatibility": {},
		"lower": {}, "title": {}, "upper": {},
	}
	pending_range: tuple[int, str] | None = None
	for line in path.read_text(encoding="utf-8").splitlines():
		fields = line.split(";")
		code = int(fields[0], 16)
		name = fields[1]
		if code in data["category"]:
			raise ValueError(f"duplicate UnicodeData code point: {code}")
		if name.endswith(", First>"):
			pending_range = (code, fields[2])
			continue
		if name.endswith(", Last>"):
			if pending_range is None or pending_range[1] != fields[2]:
				raise ValueError(f"unmatched UnicodeData range ending at: {code}")
			start, category = pending_range
			for range_code in range(start, code + 1):
				data["category"][range_code] = category
			pending_range = None
			continue
		data["category"][code] = fields[2]
		ccc = int(fields[3])
		if ccc:
			data["ccc"][code] = ccc
		decomposition = fields[5]
		if decomposition:
			parts = decomposition.split()
			kind = "canonical"
			if parts[0].startswith("<"):
				kind = "compatibility"
				parts.pop(0)
			data[kind][code] = tuple(int(part, 16) for part in parts)
		for key, index in (("upper", 12), ("lower", 13), ("title", 14)):
			if fields[index]:
				data[key][code] = (int(fields[index], 16),)
	if pending_range is not None:
		raise ValueError(f"unterminated UnicodeData range starting at: {pending_range[0]}")
	return data


def apply_special_casing(path: Path, data: dict[str, dict[int, object]]) -> None:
	for raw_line in path.read_text(encoding="utf-8").splitlines():
		line = raw_line.split("#", 1)[0].strip()
		if not line:
			continue
		parts = [part.strip() for part in line.split(";")]
		if parts[4]:
			continue
		code = int(parts[0], 16)
		for key, value in (("lower", parts[1]), ("title", parts[2]), ("upper", parts[3])):
			mapping = tuple(int(item, 16) for item in value.split())
			if mapping != (code,):
				data[key][code] = mapping


def parse_case_folding(path: Path) -> dict[int, tuple[int, ...]]:
	mappings: dict[int, tuple[int, ...]] = {}
	for raw_line in path.read_text(encoding="utf-8").splitlines():
		line = raw_line.split("#", 1)[0].strip()
		if not line:
			continue
		code, status, mapping, *_ = (part.strip() for part in line.split(";"))
		if status in {"C", "F"}:
			code_value = int(code, 16)
			if code_value in mappings:
				raise ValueError(f"duplicate full case-fold mapping: {code_value}")
			mappings[code_value] = tuple(int(item, 16) for item in mapping.split())
	return mappings


def compress_codes(codes: list[int]) -> list[tuple[int, int]]:
	if not codes:
		return []
	ranges: list[tuple[int, int]] = []
	start = previous = codes[0]
	for code in codes[1:]:
		if code == previous + 1:
			previous = code
		else:
			ranges.append((start, previous))
			start = previous = code
	ranges.append((start, previous))
	return ranges


def compress_values(values: dict[int, object]) -> list[tuple[int, int, object]]:
	if not values:
		return []
	ranges: list[tuple[int, int, object]] = []
	items = sorted(values.items())
	start = previous = items[0][0]
	current = items[0][1]
	for code, value in items[1:]:
		if code == previous + 1 and value == current:
			previous = code
		else:
			ranges.append((start, previous, current))
			start = previous = code
			current = value
	ranges.append((start, previous, current))
	return ranges


def compress_values_with_default(values: dict[int, object], default: object) -> list[tuple[int, int, object]]:
	ranges: list[tuple[int, int, object]] = []
	start = previous = 0
	current = values.get(0, default)
	for code in range(1, 0x110000):
		value = values.get(code, default)
		if value == current:
			previous = code
		else:
			ranges.append((start, previous, current))
			start = previous = code
			current = value
	ranges.append((start, previous, current))
	return ranges


def emit_map(lines: list[str], predicate: str, mappings: dict[int, object]) -> None:
	for code, mapping in sorted(mappings.items()):
		lines.append(f"{predicate}({code}, {code_list(mapping)}).")


def emit_ranges(lines: list[str], predicate: str, ranges: list[tuple[int, int]]) -> None:
	for start, end in ranges:
		lines.append(f"{predicate}({start}, {end}).")


def validate_scalar(value: int) -> bool:
	return 0 <= value <= 0x10FFFF and not 0xD800 <= value <= 0xDFFF


def validate_mappings(name: str, mappings: dict[int, object]) -> None:
	for code, mapping in mappings.items():
		if not validate_scalar(code) or any(not validate_scalar(value) for value in mapping):
			raise ValueError(f"invalid scalar value in {name} mapping for {code}")


def generate_unicode(source_dir: Path, data_output: Path) -> None:
	data = parse_unicode_data(source_dir / "UnicodeData.txt")
	apply_special_casing(source_dir / "SpecialCasing.txt", data)
	case_fold = parse_case_folding(source_dir / "CaseFolding.txt")
	derived = parse_property(source_dir / "DerivedCoreProperties.txt", {"Alphabetic", "Cased", "Case_Ignorable"})
	properties = parse_property(source_dir / "PropList.txt", {"White_Space"})
	all_derived = parse_all_properties(source_dir / "DerivedCoreProperties.txt")
	all_properties = parse_all_properties(source_dir / "PropList.txt")
	exclusions = parse_exclusions(source_dir)
	composition: dict[tuple[int, int], int] = {}
	for composite, decomposition in data["canonical"].items():
		if len(decomposition) == 2 and composite not in exclusions and data["ccc"].get(decomposition[0], 0) == 0:
			key = decomposition[0], decomposition[1]
			if key in composition:
				raise ValueError(f"duplicate canonical composition pair: {key}")
			composition[key] = composite
	for name in ("canonical", "compatibility", "lower", "title", "upper"):
		validate_mappings(name, data[name])
	validate_mappings("case fold", case_fold)
	category_ranges = compress_values_with_default(data["category"], "Cn")
	combining_class_ranges = compress_values_with_default(data["ccc"], 0)
	mark_ranges = compress_codes(sorted(code for code, category in data["category"].items() if str(category).startswith("M")))
	control_ranges = compress_codes(sorted(code for code, category in data["category"].items() if category == "Cc"))
	counts = (
		f"% {len(data['canonical'])} canonical decompositions; {len(data['compatibility'])} compatibility decompositions; "
		f"{len(composition)} compositions; {len(case_fold)} full case folds."
	)
	lines = [HEADER, "% Generated from Unicode 17.0.0 UCD files. Do not edit.", counts, ""]
	lines.append("unicode_data_version(17, 0, 0).")
	lines.append(f"unicode_data_case_fold_mapping_count({len(case_fold)}).")
	emit_map(lines, "unicode_data_canonical_decomposition", data["canonical"])
	emit_map(lines, "unicode_data_compatibility_decomposition", data["compatibility"])
	for code, value in sorted(data["ccc"].items()):
		lines.append(f"unicode_data_canonical_combining_class({code}, {value}).")
	for start, end, value in combining_class_ranges:
		lines.append(f"unicode_data_combining_class_range({start}, {end}, {value}).")
	for (first, second), composite in sorted(composition.items()):
		lines.append(f"unicode_data_canonical_composition({first}, {second}, {composite}).")
	emit_map(lines, "unicode_data_case_fold_mapping", case_fold)
	emit_map(lines, "unicode_data_lower_case_mapping", data["lower"])
	emit_map(lines, "unicode_data_upper_case_mapping", data["upper"])
	emit_map(lines, "unicode_data_title_case_mapping", data["title"])
	for start, end, category in category_ranges:
		lines.append(f"unicode_data_general_category_range({start}, {end}, '{category}').")
	emit_ranges(lines, "unicode_data_alphabetic_range", derived["Alphabetic"])
	emit_ranges(lines, "unicode_data_cased_range", derived["Cased"])
	emit_ranges(lines, "unicode_data_case_ignorable_range", derived["Case_Ignorable"])
	emit_ranges(lines, "unicode_data_mark_range", mark_ranges)
	emit_ranges(lines, "unicode_data_white_space_range", properties["White_Space"])
	emit_ranges(lines, "unicode_data_control_range", control_ranges)
	for name, ranges in all_derived.items():
		for start, end in ranges:
			lines.append(f"unicode_data_derived_core_property_range('{name}', {start}, {end}).")
	for name, ranges in all_properties.items():
		for start, end in ranges:
			lines.append(f"unicode_data_prop_list_range('{name}', {start}, {end}).")
	lines.append("")
	data_output.write_text("\n".join(lines), encoding="ascii")


def generate_compatibility_views(output_dir: Path) -> None:
	category_lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", "", ":- include(unicode_character_data).", ""]
	category_lines.extend((
		"unicode_category(CodePoint, Category) :-",
		"\t(\tvar(CodePoint) ->",
		"\t\tunicode_data_general_category_range(Start, End, SpecificCategory),",
		"\t\tbetween(Start, End, CodePoint)",
		"\t;\tunicode_data_general_category_range(Start, End, SpecificCategory),",
		"\t\tCodePoint >= Start, CodePoint =< End,",
		"\t\t!",
		"\t),",
		"\tunicode_category_convert_(SpecificCategory, Category).",
		"",
		"unicode_category_convert_(SpecificCategory, SpecificCategory).",
		"unicode_category_convert_('Lu', 'Lc').",
		"unicode_category_convert_('Ll', 'Lc').",
		"unicode_category_convert_('Lt', 'Lc').",
		"unicode_category_convert_('Cc', 'C').",
		"unicode_category_convert_('Cf', 'C').",
		"unicode_category_convert_('Cn', 'C').",
		"unicode_category_convert_('Co', 'C').",
		"unicode_category_convert_('Cs', 'C').",
		"unicode_category_convert_('Lu', 'L').",
		"unicode_category_convert_('Ll', 'L').",
		"unicode_category_convert_('Lt', 'L').",
		"unicode_category_convert_('Lm', 'L').",
		"unicode_category_convert_('Lo', 'L').",
		"unicode_category_convert_('Mc', 'M').",
		"unicode_category_convert_('Me', 'M').",
		"unicode_category_convert_('Mn', 'M').",
		"unicode_category_convert_('Nd', 'N').",
		"unicode_category_convert_('Nl', 'N').",
		"unicode_category_convert_('No', 'N').",
		"unicode_category_convert_('Pc', 'P').",
		"unicode_category_convert_('Pd', 'P').",
		"unicode_category_convert_('Pe', 'P').",
		"unicode_category_convert_('Pf', 'P').",
		"unicode_category_convert_('Pi', 'P').",
		"unicode_category_convert_('Po', 'P').",
		"unicode_category_convert_('Ps', 'P').",
		"unicode_category_convert_('Sc', 'S').",
		"unicode_category_convert_('Sk', 'S').",
		"unicode_category_convert_('Sm', 'S').",
		"unicode_category_convert_('So', 'S').",
		"unicode_category_convert_('Zl', 'Z').",
		"unicode_category_convert_('Zp', 'Z').",
		"unicode_category_convert_('Zs', 'Z').",
		"",
	))
	(output_dir / "unicode_categories.pl").write_text("\n".join(category_lines), encoding="ascii")

	category_views = {
		"cc_other_control": "Cc", "cf_other_format": "Cf", "cn_other_not_assigned": "Cn",
		"co_other_private_use": "Co", "cs_other_surrogate": "Cs", "ll_letter_lowercase": "Ll",
		"lm_letter_modifier": "Lm", "lo_letter_other": "Lo", "lt_letter_titlecase": "Lt",
		"lu_letter_uppercase": "Lu", "mc_mark_spacing_combining": "Mc", "me_mark_enclosing": "Me",
		"mn_mark_nonspacing": "Mn", "nd_number_decimal_digit": "Nd", "nl_number_letter": "Nl",
		"no_number_other": "No", "pc_punctuation_connector": "Pc", "pd_punctuation_dash": "Pd",
		"pe_punctuation_close": "Pe", "pf_punctuation_final_quote": "Pf", "pi_punctuation_initial_quote": "Pi",
		"po_punctuation_other": "Po", "ps_punctuation_open": "Ps", "sc_symbol_currency": "Sc",
		"sk_symbol_modifier": "Sk", "sm_symbol_math": "Sm", "so_symbol_other": "So",
		"zl_separator_line": "Zl", "zp_separator_paragraph": "Zp", "zs_separator_space": "Zs",
	}
	category_dir = output_dir / "unicode_categories"
	category_dir.mkdir(parents=True, exist_ok=True)
	for suffix, category in category_views.items():
		lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", ""]
		lines.extend((
			f"unicode_category_(CodePoint, '{category}') :-",
			f"\tunicode_data_general_category_range(Start, End, '{category}'),",
			"\tbetween(Start, End, CodePoint).",
			"",
			f"unicode_category_range_(Start, End, '{category}') :-",
			f"\tunicode_data_general_category_range(Start, End, '{category}').",
			"",
		))
		(category_dir / f"unicode_category_{suffix}.pl").write_text("\n".join(lines), encoding="ascii")
	cased_lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", ""]
	(category_dir / "unicode_category_lc_letter_cased.pl").write_text("\n".join(cased_lines), encoding="ascii")

	combining_lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", "", ":- include(unicode_character_data).", ""]
	combining_lines.extend((
		"unicode_combining_class(CodePoint, Class) :-",
		"\t(\tvar(CodePoint) ->",
		"\t\tunicode_data_combining_class_range(Start, End, Class),",
		"\t\tbetween(Start, End, CodePoint)",
		"\t;\tunicode_data_canonical_combining_class(CodePoint, Class) ->",
		"\t\ttrue",
		"\t;\tbetween(0, 1114111, CodePoint),",
		"\t\tClass = 0",
		"\t).",
		"",
		"unicode_combining_class(Start, End, Class) :-",
		"\tunicode_data_combining_class_range(Start, End, Class).",
		"",
	))
	(output_dir / "unicode_derived_combining_class.pl").write_text("\n".join(combining_lines), encoding="ascii")

	derived_properties = {
		"Math": "math", "Alphabetic": "alphabetic", "Lowercase": "lowercase", "Uppercase": "uppercase",
		"Cased": "cased", "Case_Ignorable": "case_ignorable", "Changes_When_Lowercased": "changes_when_lowercased",
		"Changes_When_Uppercased": "changes_when_uppercased", "Changes_When_Titlecased": "changes_when_titlecased",
		"Changes_When_Casefolded": "changes_when_casefolded", "Changes_When_Casemapped": "changes_when_casemapped",
		"ID_Start": "id_start", "ID_Continue": "id_continue", "XID_Start": "xid_start", "XID_Continue": "xid_continue",
		"Default_Ignorable_Code_Point": "default_ignorable", "Grapheme_Extend": "grapheme_extend",
		"Grapheme_Base": "grapheme_base", "Grapheme_Link": "grapheme_link",
	}
	core_dir = output_dir / "unicode_core_properties"
	core_dir.mkdir(parents=True, exist_ok=True)
	core_includes: list[str] = []
	for property_name, predicate_suffix in derived_properties.items():
		filename = f"unicode_{predicate_suffix}"
		core_includes.append(filename)
		predicate = f"unicode_{predicate_suffix}"
		property_lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", ""]
		property_lines.extend((
			f"{predicate}(CodePoint) :-",
			"\t(\tvar(CodePoint) ->",
			f"\t\t{predicate}(Start, End),",
			"\t\tbetween(Start, End, CodePoint)",
			f"\t;\t{predicate}(Start, End),",
			"\t\tCodePoint >= Start, CodePoint =< End,",
			"\t\t!",
			"\t).",
			"",
			f"{predicate}(Start, End) :-",
			f"\tunicode_data_derived_core_property_range('{property_name}', Start, End).",
			"",
		))
		(core_dir / f"{filename}.pl").write_text("\n".join(property_lines), encoding="ascii")
	range_alphabetic_lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", "", "unicode_range_alphabetic(Start, End) :-", "\tunicode_data_derived_core_property_range('Alphabetic', Start, End).", ""]
	(core_dir / "unicode_range_alphabetic.pl").write_text("\n".join(range_alphabetic_lines), encoding="ascii")
	core_lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", "", ":- include(unicode_character_data)."]
	core_lines.extend(f":- include('unicode_core_properties/{filename}')." for filename in core_includes)
	core_lines.append(":- include('unicode_core_properties/unicode_range_alphabetic').")
	core_lines.append("")
	(output_dir / "unicode_core_properties.pl").write_text("\n".join(core_lines), encoding="ascii")

	derived_lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", "", ":- include(unicode_character_data).", ""]
	derived_lines.extend((
		"unicode_core_property(CodePoint, Property) :-",
		"\t(\tvar(CodePoint) ->",
		"\t\tunicode_data_derived_core_property_range(Property, Start, End),",
		"\t\tbetween(Start, End, CodePoint)",
		"\t;\tunicode_data_derived_core_property_range(Property, Start, End),",
		"\t\tCodePoint >= Start, CodePoint =< End",
		"\t).",
		"",
		"unicode_core_property(Start, End, Property) :-",
		"\tunicode_data_derived_core_property_range(Property, Start, End).",
		"",
	))
	(output_dir / "unicode_derived_core_properties.pl").write_text("\n".join(derived_lines), encoding="ascii")

	prop_list_properties = {
		"White_Space": "white_space", "Bidi_Control": "bidi_control", "Join_Control": "join_control", "Dash": "dash",
		"Hyphen": "hyphen", "Quotation_Mark": "quotation_mark", "Terminal_Punctuation": "terminal_punctuation",
		"Other_Math": "other_math", "Hex_Digit": "hex_digit", "ASCII_Hex_Digit": "ascii_hex_digit",
		"Other_Alphabetic": "other_alphabetic", "Ideographic": "ideographic", "Diacritic": "diacritic",
		"Extender": "extender", "Other_Lowercase": "other_lowercase", "Other_Uppercase": "other_uppercase",
		"Noncharacter_Code_Point": "noncharacter_code_point", "Other_Grapheme_Extend": "other_grapheme_extend",
		"IDS_Binary_Operator": "ids_binary_operator", "IDS_Trinary_Operator": "ids_trinary_operator", "Radical": "radical",
		"Unified_Ideograph": "unified_ideograph", "Other_Default_Ignorable_Code_Point": "other_default_ignorable",
		"Deprecated": "deprecated", "Soft_Dotted": "soft_dotted", "Logical_Order_Exception": "logical_order_exception",
		"Other_ID_Start": "other_id_start", "Other_ID_Continue": "other_id_continue", "STerm": "sterm",
		"Variation_Selector": "variation_selector", "Pattern_White_Space": "pattern_white_space", "Pattern_Syntax": "pattern_syntax",
	}
	prop_dir = output_dir / "unicode_prop_list"
	prop_dir.mkdir(parents=True, exist_ok=True)
	prop_includes: list[str] = []
	for property_name, predicate_suffix in prop_list_properties.items():
		filename = f"unicode_{predicate_suffix}"
		prop_includes.append(filename)
		predicate = f"unicode_{predicate_suffix}"
		property_lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", ""]
		property_lines.extend((
			f"{predicate}(CodePoint) :-",
			"\t(\tvar(CodePoint) ->",
			f"\t\t{predicate}(Start, End),",
			"\t\tbetween(Start, End, CodePoint)",
			f"\t;\t{predicate}(Start, End),",
			"\t\tCodePoint >= Start, CodePoint =< End,",
			"\t\t!",
			"\t).",
			"",
			f"{predicate}(Start, End) :-",
			f"\tunicode_data_prop_list_range('{property_name}', Start, End).",
			"",
		))
		(prop_dir / f"{filename}.pl").write_text("\n".join(property_lines), encoding="ascii")
	prop_lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", "", ":- include(unicode_character_data)."]
	prop_lines.extend(f":- include('unicode_prop_list/{filename}')." for filename in prop_includes)
	prop_lines.append("")
	(output_dir / "unicode_prop_list.pl").write_text("\n".join(prop_lines), encoding="ascii")


def generate_range_view(source: Path, output: Path, predicate: str, default: str) -> None:
	lines = [HEADER, "% Generated from Unicode 17.0.0 UCD data. Do not edit.", ""]
	lines.extend((
		f"{predicate}(CodePoint, Value) :-",
		"\t(\tvar(CodePoint) ->",
		f"\t\t{predicate}(Start, End, Value),",
		"\t\tbetween(Start, End, CodePoint)",
		f"\t;\t{predicate}(Start, End, SpecificValue),",
		"\t\tCodePoint >= Start, CodePoint =< End ->",
		"\t\tValue = SpecificValue",
		"\t;\tbetween(0, 1114111, CodePoint),",
		f"\t\tValue = {prolog_atom(default)}",
		"\t).",
		"",
	))
	for start, end, value in parse_range_records(source):
		lines.append(f"{predicate}({start}, {end}, {prolog_atom(value)}).")
	lines.append("")
	output.write_text("\n".join(lines), encoding="ascii")


def generate_range_views(source_dir: Path, output_dir: Path) -> None:
	views = (
		("Blocks.txt", "unicode_blocks.pl", "unicode_block", "No_Block"),
		("DerivedAge.txt", "unicode_derived_age.pl", "unicode_age", "Unassigned"),
		("DerivedBidiClass.txt", "unicode_derived_bidi_class.pl", "unicode_bidi_class", "Left_To_Right"),
		("DerivedDecompositionType.txt", "unicode_derived_decomposition_type.pl", "unicode_decomposition_type", "None"),
		("EastAsianWidth.txt", "unicode_derived_east_asian_width.pl", "unicode_east_asian_width", "N"),
		("DerivedJoiningGroup.txt", "unicode_derived_joining_group.pl", "unicode_joining_group", "No_Joining_Group"),
		("DerivedJoiningType.txt", "unicode_derived_joining_type.pl", "unicode_joining_type", "U"),
		("LineBreak.txt", "unicode_derived_line_break.pl", "unicode_line_break", "XX"),
		("DerivedNumericType.txt", "unicode_derived_numeric_type.pl", "unicode_numeric_type", "None"),
		("HangulSyllableType.txt", "unicode_hangul_syllable_type.pl", "unicode_hangul_syllable_type", "NA"),
		("IndicPositionalCategory.txt", "unicode_indic_matra_category.pl", "unicode_indic_matra_category", "Not_Applicable"),
		("IndicSyllabicCategory.txt", "unicode_indic_syllabic_category.pl", "unicode_indic_syllabic_category", "Other"),
		("Scripts.txt", "unicode_range_scripts.pl", "unicode_script", "Zzzz"),
	)
	for source_name, output_name, predicate, default in views:
		generate_range_view(source_dir / source_name, output_dir / output_name, predicate, default)


def parse_semicolon_records(path: Path) -> list[list[str]]:
	records: list[list[str]] = []
	for raw_line in path.read_text(encoding="utf-8").splitlines():
		line = raw_line.split("#", 1)[0].strip()
		if line:
			records.append([part.strip() for part in line.split(";")])
	return records


def condition_list(value: str) -> str:
	conditions = []
	for condition in value.split():
		if condition.islower() and "_" not in condition:
			conditions.append(condition)
		else:
			conditions.append(prolog_atom(condition))
	return "[" + ",".join(conditions) + "]"


def generate_sparse_views(source_dir: Path, output_dir: Path) -> None:
	lines = [HEADER, "% Generated from Unicode 17.0.0 CaseFolding.txt. Do not edit.", ""]
	for parts in parse_semicolon_records(source_dir / "CaseFolding.txt"):
		code, status, mapping = parts[:3]
		values = [int(item, 16) for item in mapping.split()]
		lines.append(f"unicode_case_folding({int(code, 16)}, {prolog_atom(status)}, {code_list(values)}).")
	lines.append("")
	(output_dir / "unicode_case_folding.pl").write_text("\n".join(lines), encoding="ascii")

	lines = [HEADER, "% Generated from Unicode 17.0.0 SpecialCasing.txt. Do not edit.", ""]
	for parts in parse_semicolon_records(source_dir / "SpecialCasing.txt"):
		code = int(parts[0], 16)
		mappings = [code_list([int(item, 16) for item in field.split()]) for field in parts[1:4]]
		lines.append(f"unicode_special_casing({code}, {mappings[0]}, {mappings[1]}, {mappings[2]}, {condition_list(parts[4])}).")
	lines.append("")
	(output_dir / "unicode_special_casing.pl").write_text("\n".join(lines), encoding="ascii")

	lines = [HEADER, "% Generated from Unicode 17.0.0 CompositionExclusions.txt. Do not edit.", ""]
	for parts in parse_semicolon_records(source_dir / "CompositionExclusions.txt"):
		start, end = parse_range(parts[0])
		for code in range(start, end + 1):
			lines.append(f"unicode_composition_exclusion({code}).")
	lines.append("")
	(output_dir / "unicode_composition_exclusions.pl").write_text("\n".join(lines), encoding="ascii")

	lines = [HEADER, "% Generated from Unicode 17.0.0 ArabicShaping.txt. Do not edit.", ""]
	for parts in parse_semicolon_records(source_dir / "ArabicShaping.txt"):
		lines.append(f"unicode_arabic_shaping({int(parts[0], 16)}, {prolog_atom(parts[1])}, {prolog_atom(parts[2])}, {prolog_atom(parts[3])}).")
	lines.append("")
	(output_dir / "unicode_arabic_shaping.pl").write_text("\n".join(lines), encoding="ascii")

	lines = [HEADER, "% Generated from Unicode 17.0.0 BidiMirroring.txt. Do not edit.", ""]
	for parts in parse_semicolon_records(source_dir / "BidiMirroring.txt"):
		lines.append(f"unicode_bidi_mirroring({int(parts[0], 16)}, {int(parts[1], 16)}).")
	lines.append("")
	(output_dir / "unicode_bidi_mirroring.pl").write_text("\n".join(lines), encoding="ascii")

	lines = [HEADER, "% Generated from Unicode 17.0.0 CJKRadicals.txt. Do not edit.", ""]
	for parts in parse_semicolon_records(source_dir / "CJKRadicals.txt"):
		if not parts[1] or not parts[2]:
			continue
		lines.append(f"unicode_cjk_radical({prolog_atom(parts[0])}, {int(parts[1], 16)}, {int(parts[2], 16)}).")
	lines.append("")
	(output_dir / "unicode_cjk_radicals.pl").write_text("\n".join(lines), encoding="ascii")

	lines = [HEADER, "% Generated from Unicode 17.0.0 Jamo.txt. Do not edit.", ""]
	for parts in parse_semicolon_records(source_dir / "Jamo.txt"):
		lines.append(f"unicode_jamo({int(parts[0], 16)}, {json.dumps(parts[1])}).")
	lines.append("")
	(output_dir / "unicode_jamo.pl").write_text("\n".join(lines), encoding="ascii")

	lines = [HEADER, "% Generated from Unicode 17.0.0 NameAliases.txt. Do not edit.", ""]
	for parts in parse_semicolon_records(source_dir / "NameAliases.txt"):
		lines.append(f"unicode_name_alias({int(parts[0], 16)}, {prolog_atom(parts[1])}, {parts[2]}).")
	lines.append("")
	(output_dir / "unicode_name_aliases.pl").write_text("\n".join(lines), encoding="ascii")

	lines = [HEADER, "% Generated from Unicode 17.0.0 DerivedNumericValues.txt. Do not edit.", ""]
	for parts in parse_semicolon_records(source_dir / "DerivedNumericValues.txt"):
		start, end = parse_range(parts[0])
		for code in range(start, end + 1):
			lines.append(f"unicode_numerical_value({code}, {parts[1]}, {parts[3]}).")
	lines.append("")
	(output_dir / "unicode_derived_numeric_values.pl").write_text("\n".join(lines), encoding="ascii")

	script_lines = [HEADER, "% Generated from Unicode 17.0.0 Scripts.txt. Do not edit.", ""]
	script_lines.extend((
		"unicode_script_category(CodePoint, Script, Category) :-",
		"\tunicode_script(CodePoint, _, Script, Category, _, _).",
		"",
	))
	categories = parse_unicode_data(source_dir / "UnicodeData.txt")["category"]
	for start, end, script, description in parse_script_records(source_dir / "Scripts.txt"):
		segment_start = start
		category = categories.get(start, "Cn")
		for code in range(start + 1, end + 1):
			code_category = categories.get(code, "Cn")
			if code_category != category:
				script_lines.append(f"unicode_script({segment_start}, {code - 1}, {prolog_atom(script)}, {prolog_atom(str(category))}, {code - segment_start}, {prolog_atom(description)}).")
				segment_start = code
				category = code_category
		script_lines.append(f"unicode_script({segment_start}, {end}, {prolog_atom(script)}, {prolog_atom(str(category))}, {end - segment_start + 1}, {prolog_atom(description)}).")
	script_lines.append("")
	(output_dir / "unicode_scripts.pl").write_text("\n".join(script_lines), encoding="ascii")

	extension_lines = [HEADER, "% Generated from Unicode 17.0.0 ScriptExtensions.txt. Do not edit.", "", ":- include(unicode_range_scripts).", ""]
	extension_lines.extend((
		"unicode_script_extension(CodePoint, Extension) :-",
		"\t(\tvar(CodePoint) ->",
		"\t\tunicode_script_extension(Start, End, Extension),",
		"\t\tbetween(Start, End, CodePoint)",
		"\t;\tunicode_script_extension(Start, End, SpecificExtension),",
		"\t\tCodePoint >= Start, CodePoint =< End ->",
		"\t\tExtension = SpecificExtension",
		"\t;\tunicode_script(CodePoint, Script),",
		"\t\tExtension = [Script]",
		"\t).",
		"",
	))
	for start, end, scripts in parse_range_records(source_dir / "ScriptExtensions.txt"):
		extension = "[" + ",".join(prolog_atom(script) for script in scripts.split()) + "]"
		extension_lines.append(f"unicode_script_extension({start}, {end}, {extension}).")
	extension_lines.append("")
	(output_dir / "unicode_script_extensions.pl").write_text("\n".join(extension_lines), encoding="ascii")

	lines = [HEADER, "% Generated from Unicode 17.0.0 Unihan_Variants.txt. Do not edit.", "", "unicode_unihan_variant(CodePoint, Variant) :-", "\tunicode_unihan_variant(CodePoint, _, Variant).", ""]
	for raw_line in (source_dir / "Unihan_Variants.txt").read_text(encoding="utf-8").splitlines():
		line = raw_line.split("#", 1)[0].strip()
		if not line:
			continue
		code_text, kind, values = line.split("\t", 2)
		for value in re.findall(r"U\+([0-9A-F]+)", values):
			lines.append(f"unicode_unihan_variant({int(code_text[2:], 16)}, {kind}, {int(value, 16)}).")
	lines.append("")
	(output_dir / "unicode_unihan_variants.pl").write_text("\n".join(lines), encoding="ascii")

	name_dir = output_dir / "unicode_names"
	name_dir.mkdir(parents=True, exist_ok=True)
	name_suffixes = {
		"Cc": "cc_other_control", "Cf": "cf_other_format", "Cn": "cn_other_not_assigned",
		"Co": "co_other_private_use", "Cs": "cs_other_surrogate", "Ll": "ll_letter_lowercase",
		"Lm": "lm_letter_modifier", "Lo": "lo_letter_other", "Lt": "lt_letter_titlecase",
		"Lu": "lu_letter_uppercase", "Mc": "mc_mark_spacing_combining", "Me": "me_mark_enclosing",
		"Mn": "mn_mark_nonspacing", "Nd": "nd_number_decimal_digit", "Nl": "nl_number_letter",
		"No": "no_number_other", "Pc": "pc_punctuation_connector", "Pd": "pd_punctuation_dash",
		"Pe": "pe_punctuation_close", "Pf": "pf_punctuation_final_quote", "Pi": "pi_punctuation_initial_quote",
		"Po": "po_punctuation_other", "Ps": "ps_punctuation_open", "Sc": "sc_symbol_currency",
		"Sk": "sk_symbol_modifier", "Sm": "sm_symbol_math", "So": "so_symbol_other",
		"Zl": "zl_separator_line", "Zp": "zp_separator_paragraph", "Zs": "zs_separator_space",
	}
	name_files = {
		f"unicode_name_{suffix}": [HEADER, "% Generated from Unicode 17.0.0 UnicodeData.txt. Do not edit.", ""]
		for suffix in name_suffixes.values()
	}
	name_files["unicode_name_lc_letter_cased"] = [HEADER, "% Generated from Unicode 17.0.0 UnicodeData.txt. Do not edit.", ""]
	for raw_line in (source_dir / "UnicodeData.txt").read_text(encoding="utf-8").splitlines():
		parts = raw_line.split(";")
		code = int(parts[0], 16)
		name = parts[1]
		category = parts[2]
		if not name or name.startswith("<"):
			continue
		filename = f"unicode_name_{name_suffixes[category]}"
		name_files[filename].append(f"unicode_name({code}, {prolog_atom(name)}).")
	name_includes = []
	for filename, name_lines in sorted(name_files.items()):
		if name_lines[-1]:
			name_lines.append("")
		(name_dir / f"{filename}.pl").write_text("\n".join(name_lines), encoding="ascii")
		name_includes.append(filename)
	for obsolete in name_dir.glob("unicode_name_??.pl"):
		obsolete.unlink()
	name_loader = [HEADER, "% Generated Unicode 17.0.0 name table. Do not edit.", ""]
	name_loader.extend(f":- include('unicode_names/{filename}')." for filename in name_includes)
	name_loader.append("")
	(output_dir / "unicode_names.pl").write_text("\n".join(name_loader), encoding="ascii")

	version_lines = [HEADER, "% Generated Unicode version. Do not edit.", "", "unicode_version(17, 0, 0).", ""]
	(output_dir / "unicode_version.pl").write_text("\n".join(version_lines), encoding="ascii")


def generate_normalization_views(source_dir: Path, output_dir: Path) -> None:
	range_properties = {
		"Full_Composition_Exclusion": "full_composition_exclusion",
		"NFD_QC:N": "nfd_qc_no",
		"NFC_QC:N": "nfc_qc_no",
		"NFC_QC:M": "nfc_qc_maybe",
		"NFKD_QC:N": "nfkd_qc_no",
		"NFKC_QC:N": "nfkc_qc_no",
		"NFKC_QC:M": "nfkc_qc_maybe",
		"Expands_On_NFD": "expands_on_nfd",
		"Expands_On_NFC": "expands_on_nfc",
		"Expands_On_NFKD": "expands_on_nfkd",
		"Expands_On_NFKC": "expands_on_nfkc",
		"Changes_When_NFKC_Casefolded": "changes_when_nfkc_casefolded",
	}
	mapping_properties = {"FC_NFKC": "fc_nfkc", "NFKC_CF": "nfkc_cf"}
	property_lines = []
	mapping_lines = []
	for parts in parse_semicolon_records(source_dir / "DerivedNormalizationProps.txt"):
		start, end = parse_range(parts[0])
		property_name = parts[1]
		value = parts[2] if len(parts) > 2 else ""
		key = f"{property_name}:{value}" if property_name.endswith("_QC") else property_name
		if key in range_properties:
			property_lines.append(f"unicode_normalization_property_range({prolog_atom(key)}, {start}, {end}).")
		elif property_name in mapping_properties:
			mapping = code_list([int(item, 16) for item in value.split()])
			mapping_lines.append(f"unicode_normalization_mapping_range({prolog_atom(property_name)}, {start}, {end}, {mapping}).")
	data_lines = [HEADER, "% Generated from Unicode 17.0.0 DerivedNormalizationProps.txt. Do not edit.", ""]
	data_lines.extend(property_lines)
	data_lines.append("")
	data_lines.extend(mapping_lines)
	data_lines.append("")
	(output_dir / "unicode_derived_normalization_props_data.pl").write_text("\n".join(data_lines), encoding="ascii")

	view_dir = output_dir / "unicode_derived_normalization_props"
	view_dir.mkdir(parents=True, exist_ok=True)
	includes = []
	for property_name, predicate_suffix in range_properties.items():
		predicate = f"unicode_{predicate_suffix}"
		filename = predicate
		includes.append(filename)
		lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", ""]
		lines.extend((
			f"{predicate}(CodePoint) :-",
			"\t(\tvar(CodePoint) ->",
			f"\t\t{predicate}(Start, End),",
			"\t\tbetween(Start, End, CodePoint)",
			f"\t;\t{predicate}(Start, End),",
			"\t\tCodePoint >= Start, CodePoint =< End,",
			"\t\t!",
			"\t).",
			"",
			f"{predicate}(Start, End) :-",
			f"\tunicode_normalization_property_range({prolog_atom(property_name)}, Start, End).",
			"",
		))
		(view_dir / f"{filename}.pl").write_text("\n".join(lines), encoding="ascii")
	for property_name, predicate_suffix in mapping_properties.items():
		predicate = f"unicode_{predicate_suffix}"
		filename = predicate
		includes.append(filename)
		lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", ""]
		lines.extend((
			f"{predicate}(CodePoint, Mapping) :-",
			"\t(\tvar(CodePoint) ->",
			f"\t\tunicode_normalization_mapping_range({prolog_atom(property_name)}, Start, End, Mapping),",
			"\t\tbetween(Start, End, CodePoint)",
			f"\t;\tunicode_normalization_mapping_range({prolog_atom(property_name)}, Start, End, Mapping),",
			"\t\tCodePoint >= Start, CodePoint =< End,",
			"\t\t!",
			"\t).",
			"",
		))
		(view_dir / f"{filename}.pl").write_text("\n".join(lines), encoding="ascii")
	master_lines = [HEADER, "% Generated Unicode 17.0.0 normalization-property views. Do not edit.", "", ":- include(unicode_derived_normalization_props_data)."]
	master_lines.extend(f":- include('unicode_derived_normalization_props/{filename}')." for filename in includes)
	master_lines.append("")
	(output_dir / "unicode_derived_normalization_props.pl").write_text("\n".join(master_lines), encoding="ascii")

	decomposition_types = (
		"canonical", "compat", "font", "nobreak", "initial", "medial", "final", "isolated", "circle",
		"super", "sub", "vertical", "wide", "narrow", "small", "square", "fraction",
	)
	decomposition_dir = output_dir / "unicode_decomposition_type"
	decomposition_dir.mkdir(parents=True, exist_ok=True)
	for suffix in decomposition_types:
		predicate = f"unicode_{suffix}"
		value = "Canonical" if suffix == "canonical" else suffix.capitalize()
		lines = [HEADER, "% Generated Unicode 17.0.0 compatibility view. Do not edit.", ""]
		lines.extend((
			f"{predicate}(CodePoint) :-",
			"\t(\tvar(CodePoint) ->",
			f"\t\t{predicate}(Start, End),",
			"\t\tbetween(Start, End, CodePoint)",
			f"\t;\t{predicate}(Start, End),",
			"\t\tCodePoint >= Start, CodePoint =< End,",
			"\t\t!",
			"\t).",
			"",
			f"{predicate}(Start, End) :-",
			f"\tunicode_decomposition_type(Start, End, '{value}').",
			"",
		))
		(decomposition_dir / f"{predicate}.pl").write_text("\n".join(lines), encoding="ascii")
	master_lines = [HEADER, "% Generated Unicode 17.0.0 decomposition-type views. Do not edit.", "", ":- include(unicode_derived_decomposition_type)."]
	master_lines.extend(f":- include('unicode_decomposition_type/unicode_{suffix}')." for suffix in decomposition_types)
	master_lines.append("")
	(output_dir / "unicode_decomposition_type.pl").write_text("\n".join(master_lines), encoding="ascii")


def generate_normalization_tests(source_dir: Path, output: Path) -> None:
	vectors: list[tuple[tuple[int, ...], ...]] = []
	for raw_line in (source_dir / "NormalizationTest.txt").read_text(encoding="utf-8").splitlines():
		line = raw_line.split("#", 1)[0].strip()
		if not line or line.startswith("@"): 
			continue
		fields = [field.strip() for field in line.split(";")]
		vectors.append(tuple(tuple(int(item, 16) for item in field.split()) for field in fields[:5]))
	lines = [HEADER, "% Generated from Unicode 17.0.0 NormalizationTest.txt. Do not edit.", f"% {len(vectors)} conformance vectors.", "", ":- category(unicode_normalization_test_data).", ""]
	lines.extend((
		"\t:- info([",
		"\t\tversion is 1:0:0,",
		"\t\tauthor is 'Paulo Moura',",
		"\t\tdate is 2026-09-06,",
		"\t\tcomment is 'Generated Unicode 17.0.0 normalization conformance vectors.'",
		"\t]).",
		"",
		"\t:- protected([normalization_test/5, normalization_test_count/1]).",
		"",
		"\t:- mode(normalization_test(-list(integer), -list(integer), -list(integer), -list(integer), -list(integer)), zero_or_more).",
		"\t:- info(normalization_test/5, [comment is 'Unicode normalization conformance vector.', argnames is ['Source', 'NFC', 'NFD', 'NFKC', 'NFKD']]).",
		"\t:- mode(normalization_test_count(-integer), one).",
		"\t:- info(normalization_test_count/1, [comment is 'Number of normalization conformance vectors.', argnames is ['Count']]).",
		"",
		f"\tnormalization_test_count({len(vectors)}).",
	))
	for vector in vectors:
		values = ", ".join(code_list(field) for field in vector)
		lines.append(f"\tnormalization_test({values}).")
	lines.extend(("", ":- end_category.", ""))
	output.write_text("\n".join(lines), encoding="ascii")


def main() -> None:
	parser = argparse.ArgumentParser()
	parser.add_argument("--source-dir", type=Path, required=True)
	parser.add_argument("--output-dir", type=Path, required=True)
	parser.add_argument("--test-output-dir", type=Path, required=True)
	args = parser.parse_args()
	args.output_dir.mkdir(parents=True, exist_ok=True)
	args.test_output_dir.mkdir(parents=True, exist_ok=True)
	generate_unicode(args.source_dir, args.output_dir / "unicode_character_data.pl")
	generate_compatibility_views(args.output_dir)
	generate_range_views(args.source_dir, args.output_dir)
	generate_sparse_views(args.source_dir, args.output_dir)
	generate_normalization_views(args.source_dir, args.output_dir)
	generate_normalization_tests(args.source_dir, args.test_output_dir / "unicode_normalization_test_data.lgt")


if __name__ == "__main__":
	main()
