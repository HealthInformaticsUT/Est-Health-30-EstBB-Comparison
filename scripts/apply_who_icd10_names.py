# Set the English ICD-10 category names (parent2_name_EN) to the official
# WHO ICD-10 titles, looked up by code. The ICD-10 code is the ID: the
# English name always comes from WHO for that code, never from a translation.
#
# WHO titles: OMOP vocabulary "ICD10" (WHO ICD-10, Athena 2021 release),
# read from a local OMOP DuckDB passed as the first argument.
#
# The script changes only the parent2_name_EN field. Every other byte of each
# CSV line stays as it was (quoting, number format, NA), so `git diff` shows
# name changes only. Afterwards rebuild the parquet cache:
#   Rscript -e 'source("scripts/build_parquet_cache.R")'
#
# Usage (from project root):
#   python3 -I scripts/apply_who_icd10_names.py /path/to/omop_with_vocabulary.duckdb
import csv
import io
import sys
from pathlib import Path

import duckdb

LOOKUP = Path("demo_comparison_script/lookups/icd10_category_EN.csv")
DATA_GLOBS = ["source_data/*.csv", "comp_data/*.csv",
              "demo_comparison_script/demo_comp_data/*.csv",
              "demo_comparison_script/demo_source_data/*.csv"]
NAME_COL = "parent2_name_EN"

# Codes where the WHO vocabulary title is not the right display name
OVERRIDE = {
    "U07": "COVID-19",      # WHO title is the placeholder "Emergency use of U07"
    "I84": "Haemorrhoids",  # retired by WHO in 2010 (now K64), still used in Estonian RHK-10
}


def who_titles(db_path):
    con = duckdb.connect(db_path, read_only=True)
    rows = con.execute(
        "select concept_code, concept_name from concept "
        "where vocabulary_id = 'ICD10' and length(concept_code) = 3").fetchall()
    titles = dict(rows)
    titles.update(OVERRIDE)
    return titles


def split_raw(line):
    """Split one CSV line into raw field strings, quotes kept as they are."""
    fields, cur, in_q = [], [], False
    for ch in line:
        if ch == '"':
            in_q = not in_q
        if ch == "," and not in_q:
            fields.append("".join(cur))
            cur = []
        else:
            cur.append(ch)
    fields.append("".join(cur))
    return fields


def raw_value(raw):
    return next(csv.reader(io.StringIO(raw)))[0] if raw else ""


def encode(value, was_quoted):
    if was_quoted or any(c in value for c in ',"\n'):
        return '"' + value.replace('"', '""') + '"'
    return value


def fix_file(path, titles, code_col="parent2_code"):
    text = path.read_text(encoding="utf-8")
    lines = text.split("\n")
    header = [raw_value(h) for h in split_raw(lines[0])]
    if NAME_COL not in header:
        return 0, 0
    i_name, i_code = header.index(NAME_COL), header.index(code_col)
    changed = rows = 0
    for n in range(1, len(lines)):
        if not lines[n]:
            continue
        f = split_raw(lines[n])
        if len(f) != len(header):
            raise ValueError(f"{path}:{n + 1} has {len(f)} fields, header has {len(header)}")
        rows += 1
        code = raw_value(f[i_code])
        new = titles.get(code)
        if new is None or raw_value(f[i_name]) == new:
            continue
        f[i_name] = encode(new, f[i_name].startswith('"'))
        lines[n] = ",".join(f)
        changed += 1
    path.write_text("\n".join(lines), encoding="utf-8")
    return rows, changed


def main():
    titles = who_titles(sys.argv[1])
    files = [LOOKUP] + sorted(p for g in DATA_GLOBS for p in Path(".").glob(g))
    for p in files:
        rows, changed = fix_file(p, titles)
        print(f"{p}: {changed} of {rows} rows renamed")


if __name__ == "__main__":
    main()
