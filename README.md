# Kids and the top 1% (SCF 1989-2025)

Code and data for the post in `post_text.md`: how the number of children varies with income and
wealth, using the Federal Reserve's Survey of Consumer Finances. **Start with `kids_methods.qmd`**
(rendered: `kids_methods.html`), which explains the SCF files, the definitions, the standard errors,
and where every number in the post comes from. Fact-check log: `post_factcheck.md`.

## Run (from the project root)

```bash
Rscript R/01_download.R            # summary extracts -> data_raw/xlsx/ (--force for a datestamped re-pull)
Rscript R/02_build_db.R            # -> data/scf_linked.rds (13 extracts stacked, 2025 dollars)
Rscript R/07_kids_income.R         # -> output/kids_main.rds (merges full public data, builds sample)
Rscript R/24_kids_blended.R        # -> output/kids_blended.rds (remarried-couple robustness; run before 22)
Rscript R/23_kids_wealth_work.R    # -> output/kids_wealth_work.rds (work hours, top 1% by wealth)
Rscript R/22_post_kids_graphics.R  # -> graphics/*.png, graphics/post_numbers.csv
quarto render kids_methods.qmd     # -> kids_methods.html
```

## Data

- `data_raw/xlsx/`, `data_raw/zips/`: summary extracts (`scfp{YEAR}excel.zip`), fetched by `R/01_download.R`.
- `data_raw/full/`: full public data (`scf{YEAR}s.zip` -> `p{YY}i6.dta`), all 13 waves. Downloaded by hand;
  commands are in `kids_methods.qmd`, section 2.
- `data_raw/rw/`: replicate weights (`scf{YEAR}rw1s.zip` -> `p{YY}_rw1.dta`), 2019/2022/2025. Downloaded by hand.
- `docs/codebk{YEAR}.txt`: Fed codebooks.

## Conventions

- One row per family x implicate. `Y1` = 10 x `YY1` + implicate. Pooled cross-section, not a panel.
- `WGT` (summary extract) is already divided by 5; point estimates pool all five implicates.
- Extract dollars are 2025 dollars as released by the Fed; full-data dollars are nominal.
- Standard errors: `R/rep_se.R` (999 replicate weights on implicate 1, scale 1/998, plus 1.2 x
  between-implicate variance). Chart bands use Rubin's rules on per-implicate regressions (approximate).
- Keep R string literals ASCII: the R session locale is not UTF-8.
