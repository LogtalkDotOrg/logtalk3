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

"""Generate numeric-only Unicode 17.0 and common HTML entity tables."""

from __future__ import annotations

import argparse
import json
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


def emit_map(lines: list[str], predicate: str, mappings: dict[int, object]) -> None:
	for code, mapping in sorted(mappings.items()):
		lines.append(f"\t{predicate}({code}, {code_list(mapping)}).")


def emit_ranges(lines: list[str], predicate: str, ranges: list[tuple[int, int]]) -> None:
	for start, end in ranges:
		lines.append(f"\t{predicate}({start}, {end}).")


def validate_scalar(value: int) -> bool:
	return 0 <= value <= 0x10FFFF and not 0xD800 <= value <= 0xDFFF


def validate_mappings(name: str, mappings: dict[int, object]) -> None:
	for code, mapping in mappings.items():
		if not validate_scalar(code) or any(not validate_scalar(value) for value in mapping):
			raise ValueError(f"invalid scalar value in {name} mapping for {code}")


def generate_unicode(source_dir: Path, output: Path) -> None:
	data = parse_unicode_data(source_dir / "UnicodeData.txt")
	apply_special_casing(source_dir / "SpecialCasing.txt", data)
	case_fold = parse_case_folding(source_dir / "CaseFolding.txt")
	derived = parse_property(source_dir / "DerivedCoreProperties.txt", {"Cased", "Case_Ignorable"})
	properties = parse_property(source_dir / "PropList.txt", {"White_Space"})
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
	mark_ranges = compress_codes(sorted(code for code, category in data["category"].items() if str(category).startswith("M")))
	control_ranges = compress_codes(sorted(code for code, category in data["category"].items() if category == "Cc"))
	counts = (
		f"% {len(data['canonical'])} canonical decompositions; {len(data['compatibility'])} compatibility decompositions; "
		f"{len(composition)} compositions; {len(case_fold)} full case folds."
	)
	lines = [HEADER, "% Generated from Unicode 17.0.0 UCD files. Do not edit.", counts, "", ":- category(unicode_character_data).", ""]
	lines.extend((
		"\t:- info([",
		"\t\tversion is 1:0:0,",
		"\t\tauthor is 'Paulo Moura',",
		"\t\tdate is 2026-09-06,",
		"\t\tcomment is 'Generated numeric Unicode 17.0.0 normalization, casing, and character-property data.'",
		"\t]).",
		"",
		"\t:- protected([canonical_decomposition/2, compatibility_decomposition/2, canonical_combining_class/2,",
		"\t\tcanonical_composition/3, case_fold_mapping/2, case_fold_mapping_count/1, lower_case_mapping/2, upper_case_mapping/2,",
		"\t\ttitle_case_mapping/2, cased_code_range/2, case_ignorable_range/2, mark_code_range/2, white_space_range/2, control_code_range/2]).",
		"",
		"\t:- mode(canonical_decomposition(+integer, -list(integer)), zero_or_one).",
		"\t:- info(canonical_decomposition/2, [comment is 'Canonical decomposition mapping.', argnames is ['Code', 'Decomposition']]).",
		"\t:- mode(compatibility_decomposition(+integer, -list(integer)), zero_or_one).",
		"\t:- info(compatibility_decomposition/2, [comment is 'Compatibility decomposition mapping.', argnames is ['Code', 'Decomposition']]).",
		"\t:- mode(canonical_combining_class(+integer, -integer), zero_or_one).",
		"\t:- info(canonical_combining_class/2, [comment is 'Non-zero canonical combining class.', argnames is ['Code', 'Class']]).",
		"\t:- mode(canonical_composition(+integer, +integer, -integer), zero_or_one).",
		"\t:- info(canonical_composition/3, [comment is 'Canonical composition mapping.', argnames is ['First', 'Second', 'Composite']]).",
		"\t:- mode(case_fold_mapping(+integer, -list(integer)), zero_or_one).",
		"\t:- info(case_fold_mapping/2, [comment is 'Full default case-fold mapping.', argnames is ['Code', 'Mapping']]).",
		"\t:- mode(case_fold_mapping_count(-integer), one).",
		"\t:- info(case_fold_mapping_count/1, [comment is 'Number of full default case-fold mappings.', argnames is ['Count']]).",
		"\t:- mode(lower_case_mapping(+integer, -list(integer)), zero_or_one).",
		"\t:- info(lower_case_mapping/2, [comment is 'Default lowercase mapping.', argnames is ['Code', 'Mapping']]).",
		"\t:- mode(upper_case_mapping(+integer, -list(integer)), zero_or_one).",
		"\t:- info(upper_case_mapping/2, [comment is 'Default uppercase mapping.', argnames is ['Code', 'Mapping']]).",
		"\t:- mode(title_case_mapping(+integer, -list(integer)), zero_or_one).",
		"\t:- info(title_case_mapping/2, [comment is 'Default titlecase mapping.', argnames is ['Code', 'Mapping']]).",
		"\t:- mode(cased_code_range(-integer, -integer), zero_or_more).",
		"\t:- info(cased_code_range/2, [comment is 'Range of code points with the Cased property.', argnames is ['Start', 'End']]).",
		"\t:- mode(case_ignorable_range(-integer, -integer), zero_or_more).",
		"\t:- info(case_ignorable_range/2, [comment is 'Range of code points with the Case_Ignorable property.', argnames is ['Start', 'End']]).",
		"\t:- mode(mark_code_range(-integer, -integer), zero_or_more).",
		"\t:- info(mark_code_range/2, [comment is 'Range of Unicode Mark-category code points.', argnames is ['Start', 'End']]).",
		"\t:- mode(white_space_range(-integer, -integer), zero_or_more).",
		"\t:- info(white_space_range/2, [comment is 'Range of code points with the White_Space property.', argnames is ['Start', 'End']]).",
		"\t:- mode(control_code_range(-integer, -integer), zero_or_more).",
		"\t:- info(control_code_range/2, [comment is 'Range of Unicode Control-category code points.', argnames is ['Start', 'End']]).",
		"",
		"\tunicode_version(17, 0, 0).",
	))
	lines.append(f"\tcase_fold_mapping_count({len(case_fold)}).")
	emit_map(lines, "canonical_decomposition", data["canonical"])
	emit_map(lines, "compatibility_decomposition", data["compatibility"])
	for code, value in sorted(data["ccc"].items()):
		lines.append(f"\tcanonical_combining_class({code}, {value}).")
	for (first, second), composite in sorted(composition.items()):
		lines.append(f"\tcanonical_composition({first}, {second}, {composite}).")
	emit_map(lines, "case_fold_mapping", case_fold)
	emit_map(lines, "lower_case_mapping", data["lower"])
	emit_map(lines, "upper_case_mapping", data["upper"])
	emit_map(lines, "title_case_mapping", data["title"])
	emit_ranges(lines, "cased_code_range", derived["Cased"])
	emit_ranges(lines, "case_ignorable_range", derived["Case_Ignorable"])
	emit_ranges(lines, "mark_code_range", mark_ranges)
	emit_ranges(lines, "white_space_range", properties["White_Space"])
	emit_ranges(lines, "control_code_range", control_ranges)
	lines.extend(("", ":- end_category.", ""))
	output.write_text("\n".join(lines), encoding="ascii")


def generate_entities(source_dir: Path, allowlist: Path, output: Path) -> None:
	entities = json.loads((source_dir / "entities.json").read_text(encoding="utf-8"))
	names = [line.strip() for line in allowlist.read_text(encoding="ascii").splitlines() if line.strip() and not line.startswith("#")]
	if len(names) != len(set(names)):
		raise ValueError("duplicate common entity name")
	lines = [HEADER, "% Generated from WHATWG entities.json. Do not edit.", f"% {len(names)} semicolon-terminated named character references.", "", ":- category(common_text_entities).", ""]
	lines.extend((
		"\t:- info([",
		"\t\tversion is 1:0:0,",
		"\t\tauthor is 'Paulo Moura',",
		"\t\tdate is 2026-09-06,",
		"\t\tcomment is 'Generated common semicolon-terminated HTML named character references.'",
		"\t]).",
		"",
		"\t:- protected(common_named_entity/2).",
		"\t:- mode(common_named_entity(+atom, -list(integer)), zero_or_one).",
		"\t:- info(common_named_entity/2, [comment is 'Common named character reference mapping.', argnames is ['Name', 'Codes']]).",
		"",
	))
	for name in sorted(names):
		key = f"&{name};"
		if key not in entities:
			raise ValueError(f"unknown semicolon-terminated entity: {name}")
		codepoints = entities[key]["codepoints"]
		if any(not validate_scalar(value) for value in codepoints):
			raise ValueError(f"invalid scalar value in entity: {name}")
		lines.append(f"\tcommon_named_entity({name}, {code_list(codepoints)}).")
	lines.extend(("", ":- end_category.", ""))
	output.write_text("\n".join(lines), encoding="ascii")


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
	parser.add_argument("--allowlist", type=Path, required=True)
	args = parser.parse_args()
	args.output_dir.mkdir(parents=True, exist_ok=True)
	generate_unicode(args.source_dir, args.output_dir / "unicode_character_data.lgt")
	generate_entities(args.source_dir, args.allowlist, args.output_dir / "common_text_entities.lgt")
	generate_normalization_tests(args.source_dir, args.output_dir / "unicode_normalization_test_data.lgt")


if __name__ == "__main__":
	main()