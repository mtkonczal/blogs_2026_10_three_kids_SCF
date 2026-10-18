# DeepSeek independent audit, October 18, 2026

This folder contains the independent first-pass audit behind the **DeepSeek** tab in
`../../factcheck.html`. It was run against commit `dd0e71f` (the draft that states families remain ranked against the full age-eligible sample in footnote 2) and reads only the raw
summary extracts, full Stata files and replicate weights; it does not source the
production estimators.

## Scripts

- `build_check.R` — rebuilds the 2013–2025 sample from the raw files, reconstructs the
  child count, ranks and 12-bin curves, and saves `deep_sample.rds`.
- `inference_check.R` — independent replicate-weight estimator for the headline
  income and wealth gaps (999 weights, 998 divisor, 1.2 imputation factor).
- `sensitivity_check.R` — conditional-on-two-children, adult-roster, first-marriage,
  older-sample and capped-mean checks.
- `rerank_strict.R` — the re-ranked first-marriage and older-sample sensitivity.
- `fixed_rank.R` — fixed-rank versus re-ranked standard errors.
- `weights_check.R`, `mean_tail.R`, `check_stock.R` — population weighting, capped
  means and 2025 stock-share diagnostics.

## Main findings

- All headline point estimates reproduce to `1e-6`; standard errors match to better
  than `0.001`. No headline calculation bug was found.
- The income 3+ gap among families with 2+ children is 6.57 points (95% CI −1.41 to
  14.55); the wealth gap is 11.52 (4.04 to 18.99).
- The first-marriage wealth gap is 10.65 with full-sample ranks (0.92 to 20.37), the
  estimand the post now specifies. Re-ranking inside the first-marriage subsample
  gives 4.17 (−5.94 to 14.27), a different estimand (top 1% of first-marriage couples).
- The methods' “three families” `X5910` sentence is true in the 45–64 sample (two in
  1989, one in 1992), not the full files (40 and 50 family–implicate records, 8 and 10
  families).
- The adult-roster top-only adverse check gives −1.65 by income (−8.87 to 5.56) and
  0.95 by wealth (−6.52 to 8.43).

`deepseek_section.html` and `crosscheck_note.html` are the fragments inserted into
`factcheck.html`. The page also uses `../factcheck_navigation.css` and
`../factcheck_navigation.js`.
