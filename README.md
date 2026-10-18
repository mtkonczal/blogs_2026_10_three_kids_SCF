# Kids and the top 1% (SCF 1989-2025)

Reproduction instructions for the analysis in [`blog_post_text.md`](blog_post_text.md).
This README is also the execution guide for AI agents working from a fresh clone.
Read [`kids_methods.qmd`](kids_methods.qmd) ([rendered HTML](kids_methods.html)) for
variable definitions, estimator details, and interpretation. Read the scripts before
changing a specification; reproduce the baseline first.

## Requirements

Run every command from the repository root, the directory containing `R/`.
You need R, `curl`, `unzip`, and Quarto with its R execution dependencies.
The retained audit used R 4.4.2; package/session records are in
[`audit/results/sessionInfo.txt`](audit/results/sessionInfo.txt). This repository
has no package lockfile, so installing current packages does not pin that environment.
No API credentials are needed for the public Fed files.

Install missing R packages once:

```r
packages <- c("data.table", "haven", "fixest", "ggplot2", "scales", "ragg",
              "dplyr", "knitr", "rmarkdown")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
```

The raw files and derived RDS objects are excluded from Git. A clone is not ready
for analysis until the downloads and build below complete. Allow several GB of
free space; the full-data Stata files alone occupy about 2.9 GB.

## 1. Obtain the inputs

### Summary extracts: all 13 waves, 1989 through 2025

```bash
Rscript R/01_download.R
```

This downloads `scfp{YEAR}excel.zip` into `data_raw/zips/`, extracts
`SCFP{YEAR}.csv` into `data_raw/xlsx/`, and writes
`data_raw/download_manifest.csv`. Despite the folder/archive names, these are CSV
files, not Excel workbooks. Existing archives are reused, but their CSVs are
extracted again and the manifest is rewritten. Preserve the committed manifest
before running if you want to compare a new download with the recorded vintage.

`--force` downloads into datestamped `zips_*` and `xlsx_*` directories. It does
**not** switch the downstream pipeline away from `data_raw/xlsx/`, and still
rewrites the manifest. Do not treat it as an automatic update of the analysis.

### Full public data and replicate weights

`R/01_download.R` does not fetch these. Run this block in Bash from the project
root. It stops on download/extraction errors, skips existing archives, and does
not overwrite existing extracted files. For a new vintage, use a separate checkout
or preserve the existing inputs first.

```bash
bash <<'BASH'
set -euo pipefail
mkdir -p data_raw/full data_raw/rw output
base=https://www.federalreserve.gov/econres/files

# Full public Stata files: note two-digit archive years through 2001.
for y in 89 92 95 98 01 2004 2007 2010 2013 2016 2019 2022 2025; do
  archive="data_raw/full/scf${y}s.zip"
  if [ ! -f "$archive" ]; then
    curl -fL --retry 3 -o "${archive}.part" "$base/scf${y}s.zip"
    mv "${archive}.part" "$archive"
  fi
  unzip -n "$archive" -d data_raw/full
 done

# Five recent waves plus four early waves required by the historical tests.
for y in 89 92 95 98 2013 2016 2019 2022 2025; do
  archive="data_raw/rw/scf${y}rw1s.zip"
  if [ ! -f "$archive" ]; then
    curl -fL --retry 3 -o "${archive}.part" "$base/scf${y}rw1s.zip"
    mv "${archive}.part" "$archive"
  fi
  unzip -n "$archive" -d data_raw/rw
 done
BASH
```

Expected full-data filenames are `data_raw/full/p{YY}i6.dta` for all 13 waves,
except **1992 uses `p92i4.dta`**. Replicate files are
`data_raw/rw/p{YY}_rw1.dta` for 1989/1992/1995/1998 and
2013/2016/2019/2022/2025. Keep these paths and names: the scripts expect them.
Archives and CSVs downloaded today may differ from the original vintage; compare
hashes with the retained manifests/provenance before claiming exact replication.

Codebooks are optional for the production run but needed for source verification:
`https://www.federalreserve.gov/econres/files/codebk{YEAR}.txt`.
Save them under `docs/codebk{YEAR}.txt`; they are excluded from Git.

## 2. Run the production pipeline

Run in this order. Script numbers are identifiers, not execution order.
The shell stops at the first failure; do not continue with stale downstream files.

```bash
bash <<'BASH'
set -euo pipefail
mkdir -p output
Rscript R/02_build_db.R            # -> data/scf_linked.rds
Rscript R/07_kids_income.R         # -> output/kids_main.rds and kids_income.rds
Rscript R/25_rep_inference.R       # -> output/kids_rep5.rds: headline tests
Rscript R/26_top1_definitions.R    # -> output/kids_top1_defs.rds and definitions.html
Rscript R/27_period_change_test.R  # -> output/kids_period_change.rds: historical tests
Rscript R/28_bin_intervals.R       # -> output/kids_bin_ci.rds: chart intervals
Rscript R/29_family_count_checks.R # -> output/kids_count_checks.rds: footnote 2
Rscript R/22_post_kids_graphics.R  # -> seven graphics/*.png and graphics/post_numbers.csv
quarto render kids_methods.qmd    # -> kids_methods.html
BASH
```

`R/29` also reads the **tracked** reference table
`audit/review_20261015/results/independent_inference.csv` for its assertions. Keep
that file in a fresh checkout; do not delete the audit directory as unnecessary.
`R/26`, `R/27`, `R/28`, and `R/29` check their baseline against `R/25`.
`R/22` needs the `R/25` and `R/28` outputs, and rendering needs the preceding outputs.

The following are optional exploratory analyses, not dependencies of the post's
production pipeline or methods render:

```bash
Rscript R/24_kids_blended.R     # after 07 -> output/kids_blended.rds; older remarriage proxy
Rscript R/23_kids_wealth_work.R # after 07 -> output/kids_wealth_work.rds; work hours and wealth
```

The isolated audit rebuild runs these optional scripts too because it compares
ten production data objects. `R/22` itself calculates some supplementary numbers
absent from the post; their presence is not an additional headline claim.

## 3. Validate the result

- Every script must exit successfully. Preserve the first error and diagnose its
  cause rather than skipping assertions or changing the sample to make it run.
- Check `graphics/post_numbers.csv` and the headline tests: recent 3+ shares are
  about 49.13% versus 39.99% by income, and 50.15% versus 37.21% by wealth.
  The directly tested gaps are about 9.14 and 12.94 percentage points.
- Inspect all four images embedded in `blog_post_text.md` for clipping, labels,
  captions and uncertainty displays. Open `kids_methods.html` and check its figures
  and tables. A successful R run alone does not verify the visual output.
- Numeric comparisons should ignore `built_at` timestamps. Exact PNG hashes can
  depend on R/package/font/device versions; a hash difference needs inspection,
  not an automatic claim that the estimates differ.

For an isolated rebuild using already-downloaded local inputs, with originals
preserved, see [`audit/README.md`](audit/README.md):

```bash
python3 audit/review_20261015/reproduce.py
Rscript audit/review_20261015/check_outputs.R
```

The rebuild writes under ignored `audit/review_20261015/reproduction/` and
`logs/` directories and reuses local raw inputs. It does not download them or
independently validate their external provenance. Python 3 is needed for this
optional workflow. On restricted macOS agent environments, Quarto's architecture
check may be blocked even when the R steps succeed; report that separately from
an analysis failure and render with the necessary host permission.

## Definitions an AI reproducer must preserve

- The outcome is `KIDS + max(X5910, 0)`: an imperfect **family child count**,
  including living children elsewhere, not an individual birth history.
- Main outcomes concern reference persons aged 45-64. The recent pool is
  2013/2016/2019/2022/2025, with equal total weight per wave. These are repeated
  cross-sections, not a panel.
- Baseline ranks use usual family income (`NORMINC`) or net worth within survey
  year, implicate and five-year age band. Historical 1989/1992 income rankings use
  actual income because usual-income amounts are unavailable.
- Footnote 2 restrictions retain the **full age-eligible ranking universe**.
  Ranks are still re-estimated within each survey replicate before restrictions.
  Re-ranking only among first-marriage couples is a different sensitivity, not a
  silent replacement for this specification.
- One row is a family x implicate; `Y1 = 10 * YY1 + implicate` within a wave.
  Summary `WGT` is already divided by five. Do not divide it again or treat the
  five imputations as independent families. Main inference averages
  implicate-specific estimates; pooling rows first is not algebraically identical
  for subgroup ratios.
- Extract financial values are in **2025 dollars**; full-data financial values
  are nominal. Income is an annual family flow, net worth a stock; neither is
  equivalized for family size. Chart dollar thresholds are approximate pooled
  labels, not the age/year-specific ranking cutoffs.
- Main standard errors use 999 Fed replicate weights on implicate 1, divisor
  998, plus `1.2 *` between-implicate variance. Ranks are re-estimated in each
  replicate; independent-wave variances are combined through the pooled estimator.
  Intervals are pointwise normal approximations, without a multiple-testing
  adjustment. Do not substitute the exploratory clustered SEs for headline tests.
- Keep R string literals ASCII for compatibility with the project's R locale.
  Do not change empirical definitions, weighting or inference without explicitly
  documenting the departure. Do not claim execution or verification you did not do.

## Reviews and supplementary sensitivities

[`factcheck.html`](factcheck.html) contains dated Codex, Claude and DeepSeek
reviews; these describe the snapshots they inspected, not automatic certification
of every later edit. [`audit/README.md`](audit/README.md) maps the independent
checks and their evidence. Supplementary outcomes and restrictions, including
ones whose intervals include zero, do not replace the production specification.

The DeepSeek scripts and instructions are in
[`audit/deepseek_20261018/README.md`](audit/deepseek_20261018/README.md).
Run `build_check.R` before its other scripts: `deep_sample.rds` is excluded from
Git. Its within-first-marriage ranking sensitivity changes the comparison groups;
the combined duplicate-corrected wealth gap is smaller and its interval includes
zero. See the scripts as well as the review's prose for the exact specification.

Reproducing the analysis does **not** require rebuilding `factcheck.html`.
`audit/build_factcheck.py` is a dated review generator with snapshot/hash checks;
do not run it as a routine final build or assume it incorporates later reviews
and editorial changes automatically.
