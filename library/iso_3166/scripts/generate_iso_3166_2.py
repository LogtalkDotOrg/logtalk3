#!/usr/bin/env python3

import json
import sys
from datetime import date
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

SOURCE_URL = "https://salsa.debian.org/iso-codes-team/iso-codes/-/raw/main/data/iso_3166-2.json"


def escape_atom(text: str) -> str:
    return "'" + text.replace("'", "''") + "'"


def main() -> int:
    if len(sys.argv) != 3:
        print("usage: generate_iso_3166_2.py ISO_3166_2_JSON TARGET_DIR", file=sys.stderr)
        return 1

    source_json = Path(sys.argv[1])
    target_dir = Path(sys.argv[2])
    target_file = target_dir / "iso_3166_2.lgt"

    payload = json.loads(source_json.read_text(encoding="utf-8"))
    entries = payload["3166-2"]

    facts = []
    seen = set()
    for entry in entries:
        code = entry["code"].lower()
        country_alpha2 = code.split("-", 1)[0]
        fact = (code, country_alpha2, entry["name"], entry["type"])
        if fact in seen:
            continue
        seen.add(fact)
        facts.append(fact)

    facts.sort(key=lambda fact: (fact[0], fact[2], fact[3]))

    lines = [
        HEADER,
        "",
        ":- object(iso_3166_2).",
        "",
        "\t:- info([",
        "\t\tversion is 1:0:0,",
        "\t\tauthor is 'Paulo Moura',",
        f"\t\tdate is {date.today().isoformat()},",
        "\t\tcomment is 'Generated ISO 3166-2 subdivision facts extracted from the Debian iso-codes machine-readable JSON snapshot.',",
        "\t\tremarks is [",
        f"\t\t\t'Source URL' - '{SOURCE_URL}',",
        f"\t\t\t'Generated entries' - '{len(facts)}'",
        "\t\t]",
        "\t]).",
        "",
        "\t:- public(subdivision/4).",
        "\t:- mode(subdivision(?atom, ?atom, ?atom, ?atom), zero_or_more).",
        "\t:- info(subdivision/4, [",
        "\t\tcomment is 'Generated ISO 3166-2 subdivision fact table.',",
        "\t\targnames is ['Code', 'CountryAlpha2', 'Name', 'Category']",
        "\t]).",
        "",
    ]

    for code, country_alpha2, name, category in facts:
        lines.append(
            f"\tsubdivision({escape_atom(code)}, {escape_atom(country_alpha2)}, {escape_atom(name)}, {escape_atom(category)})."
        )

    lines.extend(["", ":- end_object.", ""])
    target_file.write_text("\n".join(lines), encoding="utf-8")
    print(f"Generated {target_file} with {len(facts)} entries from {source_json}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
