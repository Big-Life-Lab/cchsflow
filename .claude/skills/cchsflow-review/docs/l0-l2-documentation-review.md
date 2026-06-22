# L0-L2 documentation review

For each in-scope variable, verify the documentation foundations. Read `.claude/skills/cchsflow-worksheets/docs/harmonization-workflow.md` for detailed L0-L2 templates.

## L0: Documentation assessment

Verify source variables against CCHS documentation using the **cchsflow-docs** repository (`Big-Life-Lab/cchsflow-docs` on GitHub, cloned alongside cchsflow). This step confirms that variables claimed in `variableStart` and `databaseStart` actually exist in the CCHS data for those cycles.

### Primary source: cchs-metadata MCP server

**Always use the cchs-metadata MCP as the primary tool for L0-L1 verification.** It provides the most complete and queryable metadata — 16,000+ variables across 251 datasets, enriched from PUMF RData, DDI XML, and ICES sources with full provenance tracking.

**Key tools:**
- `mcp__cchs-metadata__search_variables(query)` — find variables by name or label substring
- `mcp__cchs-metadata__get_variable_detail(variable_name)` — full metadata including labels, question text, value codes, dataset history
- `mcp__cchs-metadata__get_variable_history(variable_name)` — which cycles/datasets contain the variable (essential for era boundary verification)
- `mcp__cchs-metadata__get_value_codes(variable_name)` — response categories with frequencies
- `mcp__cchs-metadata__compare_master_pumf(variable_name, cycle)` — compare PUMF vs Master metadata for a specific cycle (essential for PUMF/Master split decisions)
- `mcp__cchs-metadata__suggest_cchsflow_row(variable_name)` — draft a cchsflow harmonisation row
- `mcp__cchs-metadata__get_dataset_variables(dataset_id)` — list all variables in a specific dataset
- `mcp__cchs-metadata__get_source_conflicts(variable_name, dataset_id)` — find cross-source label disagreements (useful for catching metadata inconsistencies)
- `mcp__cchs-metadata__get_database_summary()` — database overview and statistics

**Using MCP results:**
- The `cchsflow_name` field maps StatCan source variables to their cchsflow harmonized names — use this to verify that `variableStart` entries point to the correct source variable for each cycle
- Use `get_variable_history` to confirm a variable exists across claimed cycles and to identify era renames (e.g., SMK_09C → SMK_090 at the 2015 boundary)
- Use `compare_master_pumf` to verify whether PUMF and Master share the same source variable or need split rows

**Caution:** The MCP `label_short`/`label_long` fields may be contaminated by cchsflow labels (see MCP error report from alcohol review). Always cross-check against `label_statcan` which comes from DDI primary sources.

### If the MCP is not available

Check whether the MCP is loaded:
```bash
claude mcp list
```

If `cchs-metadata` is missing or shows "Failed to connect", the server needs to be configured. The MCP server (v0.3.0+) lives in the **cchsflow-docs** repository and is also available as a [GitHub release](https://github.com/Big-Life-Lab/cchsflow-docs/releases).

**Quick setup** (if cchsflow-docs is cloned at `../cchsflow-docs/`):
```bash
cd ../cchsflow-docs/mcp-server && bash ../scripts/setup.sh
claude mcp add cchs-metadata -- python3 /Users/dmanuel/github/cchsflow-docs/mcp-server/server.py
```

**Manual setup:**
1. Ensure `cchsflow-docs` is cloned alongside cchsflow (typically `../cchsflow-docs/`)
2. Ensure `mcp-server/server.py` exists in cchsflow-docs
3. Ensure the database exists: `../cchsflow-docs/database/cchs_metadata.duckdb` (download from the [v0.3.0 release](https://github.com/Big-Life-Lab/cchsflow-docs/releases) or rebuild: `Rscript --vanilla ../cchsflow-docs/database/build_db.R`)
4. Add the MCP to Claude Code:
   ```bash
   claude mcp add cchs-metadata -- python3 /Users/dmanuel/github/cchsflow-docs/mcp-server/server.py
   ```
   Or add to `~/.claude.json` (see `.mcp.json.example` in cchsflow-docs for a template):
   ```json
   "cchs-metadata": {
     "type": "stdio",
     "command": "python3",
     "args": ["/Users/dmanuel/github/cchsflow-docs/mcp-server/server.py"],
     "env": {"CCHS_DB_PATH": "/Users/dmanuel/github/cchsflow-docs/database/cchs_metadata.duckdb"}
   }
   ```
5. Restart Claude Code for the MCP tools to appear in the tool list

### CLI fallback (recommended when MCP fails)

If the MCP server cannot be started, use the **R wrapper** or the **Python CLI** directly. The R wrapper (`exec/query-metadata.R`) automatically locates the database and falls back between Python CLI and direct DuckDB queries:

```bash
# R wrapper (auto-finds DB, falls back to DuckDB if Python unavailable)
Rscript exec/query-metadata.R search smoking
Rscript exec/query-metadata.R history SMKDSTY
Rscript exec/query-metadata.R detail SMK_204
Rscript exec/query-metadata.R codes SMK_204
Rscript exec/query-metadata.R coverage HUI06 HUI07 HUI08 HUI09  # variable-by-cycle matrix
```

When sourced interactively (e.g., during a review session), the R functions provide structured output:

```r
source("exec/query-metadata.R")
meta_search("smoking")            # search by name or label
meta_history("SMKDSTY")           # which cycles contain it
meta_coverage(c("HUI06", "HUI07", "HUI08"), file_type = "master")  # coverage matrix
```

The **Python CLI** is also available standalone (no FastMCP dependency — only `duckdb` required):
```bash
python3 ../cchsflow-docs/mcp-server/cli.py search smoking
python3 ../cchsflow-docs/mcp-server/cli.py detail SMKDSTY
python3 ../cchsflow-docs/mcp-server/cli.py history SMK_204
python3 ../cchsflow-docs/mcp-server/cli.py conflicts --variable SMKDSTY
python3 ../cchsflow-docs/mcp-server/cli.py codes SMK_204
```

All commands support `--json` for machine-readable output and `--db PATH` for custom database path.

See the cchsflow-docs `CLAUDE.md` and `.claude/skills/cchs-database/SKILL.md` for database build workflow and schema details.

### Fallback: file-based lookups

If the MCP is unavailable and cannot be restored, use the DDI YAML files in the cchsflow-docs repo (typically `../cchsflow-docs/`):

1. **DDI YAML files** — parsed variable definitions by cycle:
   ```
   ../cchsflow-docs/ddi/
   ```

2. **Processed CSVs** — tabular variable metadata:
   ```
   ../cchsflow-docs/data/
   ```

**Note:** Raw source files (`cchs-extracted/`, `cchs-pumf-docs/`) are not in the git repo — they are stored externally per the cchsflow-docs storage guide. Use the MCP database or the processed files above instead.

The MCP is strongly preferred because it cross-references all sources, deduplicates, and provides structured query tools rather than requiring manual grep/search across hundreds of files.

### What to verify

For each in-scope variable:
1. **Existence**: Does the source variable name appear in the documentation for each claimed cycle?
2. **Category codes**: Do `recStart` values match the documented category definitions?
3. **Era renames**: For 2015+ cycles, confirm the renamed variable exists
4. **Cycle coverage up to latest available**: Check whether the variable exists in cycles beyond the PR's `databaseStart` (documentation covers up to 2023) — these may be candidates for expansion

### What to flag

- Variable listed in `variableStart` but not found in documentation for that cycle → **P0** (wrong variable name)
- Variable not checked (no documentation available for that cycle) → note as untested
- Variable exists in additional cycles not included in `databaseStart` → informational (expansion opportunity)

## L1: Variable concordance

Use the cchsflow-docs extracted data dictionaries to verify source variable names across eras:

- Pre-2007: cycle letter in 4th position (A=2001, C=2003, E=2005)
- 2007-2014: standard naming
- Post-2014: check for 3-digit renames — search the 2015+ YAML files to confirm actual names
- 2022+: check for modular renames (e.g., CSS/SPU prefixes for smoking)

For each era boundary, compare the variable name in `variableStart` against the corresponding cycle's YAML data dictionary in cchsflow-docs. PUMF and Master data dictionaries may differ — check both `_p` and `_m` YAML files where available.

## L2: Semantic mapping

- Are category codes consistent across cycles?
- Are semantic breaks identified and documented?
- Do recoding rules handle all source categories?
