# Independent audit artifacts

Independent audit diagnostics. Production scripts, draft, data, and charts were not edited.

- `reproduction/`: copies of the original R scripts and methods QMD, with raw data and codebooks linked to the originals. R/02, 07, 24, 23, 22 ran in that order, followed by Quarto render.
- `logs/`: successful reproduction, comparisons, render, and diagnostics; the preview-server log records an unsuccessful local preview attempt.
- `audit_checks.R`: independent input checks, explicitly labeled sample/rank sensitivities, and five-wave pooled-ratio inference with percentile ranks re-estimated in replicates.
- `secondary_checks.R`: dollar-conversion, implicate-pooling, top-code, and hours checks.
- `additional_checks.R`: current-marriage restriction and membership checks.
- `matched_inference.R`: fixed-versus-re-estimated-rank checks with an equal-wave-average-of-gaps estimator to isolate rank uncertainty from the pooled-ratio estimator change.
- `results/`: CSV results, replicate moment arrays, input MD5 hashes, and R session versions.

Run the four R scripts from the project root, in the order above. They write only audit outputs. The primary analysis sample is read from the existing `output/kids_main.rds`, whose equality with the independently rebuilt file was verified.

Additional public sources downloaded October 9, 2026:

- https://www.federalreserve.gov/econres/files/scf2013rw1s.zip
- https://www.federalreserve.gov/econres/files/scf2016rw1s.zip
- https://www.federalreserve.gov/econres/files/bulletin.macro.txt

The weight archives are unzipped into `rw/`; their column names are normalized to lowercase in the audit loader. No random bootstrap weights are generated: the analysis uses the Fed's 999 supplied replicate weights. The seed in the script is explicit but no random operation is required by the completed calculations.

Inference notes: each wave's weighted subgroup denominators and outcome numerators are normalized by that wave's age-eligible population weight. Five-wave point estimates average pooled ratios across five implicates. Independent-wave replicate moment variances are combined using the Jacobian of those pooled ratios, then the pooled between-implicate variance is added with factor 1.2. All reported 95% intervals are normal approximations. Sensitivity specifications are diagnostics, not silently adopted replacements for the user's analysis.
