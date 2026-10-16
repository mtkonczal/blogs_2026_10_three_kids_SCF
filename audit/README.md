# Verification and supplementary analysis

The primary specification, figures, and reproduction commands are documented in
[`../kids_methods.qmd`](../kids_methods.qmd) and [`../README.md`](../README.md).
This directory contains executable checks, numerical results, input hashes, and
source documentation supporting that analysis.

## Checks behind the post

- `../R/25_rep_inference.R`: pooled 2013–2025 income and wealth contrasts.
- `../R/26_top1_definitions.R`: alternative definitions of the top group, footnote 1.
- `../R/27_period_change_test.R`: historical changes and differences in changes.
- `../R/28_bin_intervals.R`: replicate-weight intervals for all plotted bins.
- `../R/29_family_count_checks.R`: overlap, current first-marriage, and age restrictions,
  including combined restrictions, footnote 2.

Each sensitivity changes the stated outcome, ranking, or population. Its result is
not silently substituted for the main estimate. Income and wealth are measured at
interview age; these comparisons do not identify the effect of money on births.

## Additional measurement checks

Run from the repository root after the main pipeline:

```bash
Rscript audit/measurement_response_20261016.R
Rscript audit/summary_csv_check_20261016.R
```

- `results/measurement_response_20261016.csv` includes possible-overlap corrections,
  stricter marriage and age samples, exclusion of all adult roster children,
  children-elsewhere-only counts, and three-plus shares conditional on two-plus.
  The script documents each specification and its assumptions. All estimates,
  including imprecise and negative contrasts, are retained.
- `results/summary_csv_check_20261016.csv` uses only the Fed summary CSV extracts,
  without the full-survey child count. It reports descriptive roster-child results
  for the main age range and two supplementary younger age ranges. It does not
  calculate sampling significance.

These checks do not make the family count an individual birth history. Removing
adult roster children from both groups is a sensitivity, not a worst-case bound
on differential error. Intervals are pointwise normal approximations and are not
adjusted for exploratory multiple comparisons.

## Independent verification

The dated paths identify independent numerical verification runs. Their scripts
and result tables are retained for reproducibility:

- `audit_checks.R`, `secondary_checks.R`, `additional_checks.R`, and
  `matched_inference.R`: input integrity, pooled inference, membership, dollar
  conversion, imputation, and matched-estimator diagnostics. Run in that order
  from the project root after the main pipeline. Replicate weights are read from
  `data_raw/rw/`; the matched check reads the moment arrays written by the first script.
- `review_20261015/independent_checks.R`: reconstructs the recent sample from raw
  inputs and independently calculates the headline contrasts and sensitivities.
- `review_20261015/revision_7121ec0/check_bands.R`: independently checks all 48
  income/wealth bin estimates and standard errors, using the sample reconstructed
  by `independent_checks.R`.
- `review_20261015/revision_7b3f727/check_revision.R`: independently checks ranking
  alternatives and period changes. Its final comparison requires the isolated
  reproduction described below.
- `results/input_hashes.csv`, `review_20261015/results/provenance.csv`, and the
  source-provenance tables record the input files checked and their hashes.
- `sources/bulletin.macro.txt` records the Fed's summary-variable definitions.

To rebuild the current production files in an isolated local copy:

```bash
python3 audit/review_20261015/reproduce.py
```

The script reads the raw files already downloaded following the main README,
executes the current production sequence, and renders the methods document.
It writes only to ignored reproduction and log directories. The R/29 reference
CSV is copied into that isolated checkout. The subsequent
`review_20261015/check_outputs.R` compares the rebuilt and original outputs.
The independent band and revision scripts use the existing production outputs
as comparison targets rather than sourcing the production estimators.

No new bootstrap draws are generated: inference uses the Fed's fixed 999
replicate weights and the five supplied implicates. The repository excludes
raw-data archives and derived RDS files; download and rebuild them using the
main instructions before running these checks.
