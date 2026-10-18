"""Build the dated, standalone review from checked R evidence. No network calls.
Run: Rscript audit/factcheck_20261016.R && python3 audit/build_factcheck.py
The second panel is reserved for a different AI; this generator never writes its review.
"""
from pathlib import Path
import json
from html import escape
import hashlib
import re

root = Path(__file__).resolve().parent.parent
D = json.loads((root / 'audit/results/factcheck_20261016.json').read_text())
assert hashlib.md5((root / 'blog_post_text.md').read_bytes()).hexdigest() == D['blog_md5'], 'Blog changed; rerun the fact check before rebuilding.'
P = json.loads((root / 'audit/results/process_factcheck_20261016.json').read_text())
reviewed_commit_link = 'https://github.com/mtkonczal/blogs_2026_10_three_kids_SCF/blob/' + D['reviewed_commit'] + '/blog_post_text.md'
H = {(x['rank'], x['stat']): x for x in D['headline']}
N = {x['item']: x['value'] for x in D['numbers']}
def link(url, text): return f'<a href="{escape(url, quote=True)}">{text}</a>'
def src(path, text=None): return link(path, text or f'<code>{escape(path)}</code>')
def stat(rank, name, digits=2):
    z = H[(rank, name)]
    return f"{z['est']:.{digits}f} (SE {z['se']:.{digits}f}; 95% CI {z['lower']:.{digits}f} to {z['upper']:.{digits}f})"
def val(rank, name): return H[(rank, name)]['est']
def table(headers, rows):
    return '<div class="table-wrap" tabindex="0" aria-label="Scrollable evidence table"><table><thead><tr>' + ''.join(f'<th scope="col">{x}</th>' for x in headers) + '</tr></thead><tbody>' + ''.join('<tr>'+''.join(f'<th scope="row">{x}</th>' if i == 0 else f'<td>{x}</td>' for i,x in enumerate(row))+'</tr>' for row in rows) + '</tbody></table></div>'
def claim(title, verdict, content, evidence):
    return f'<article class="claim"><div class="claim-heading"><h3>{title}</h3><span class="verdict {"qualified" if verdict != "Supported" else "supported"}">{verdict}</span></div><p>{content}</p><p class="evidence">Evidence: {evidence}</p></article>'
release = link('https://www.federalreserve.gov/newsevents/pressreleases/other20261009a.htm', 'Fed release, October 9, 2026')
scf = link('https://www.federalreserve.gov/econres/scfindex.htm', 'Fed SCF data and documentation')
codebook = link('https://www.federalreserve.gov/econres/files/codebk2025.txt', '2025 codebook')
macro = link('https://www.federalreserve.gov/econres/files/bulletin.macro.txt', 'Fed summary-variable construction')
doepke = link('https://tertilt.vwl.uni-mannheim.de/research/HandbookFertility_WP_NBER.pdf', 'Doepke et al. (2022), author-hosted NBER paper')
bar = link('https://s3.amazonaws.com/real.stlouisfed.org/wp/2018/2018-022.pdf', 'Bar et al. (2018), Fed working paper')
hazan = link('https://onlinelibrary.wiley.com/doi/10.1111/ecoj.12148', 'Hazan and Zoabi (2015), publisher abstract')
haspel = link('https://www.romper.com/life/when-people-cant-afford-another-child-we-know-big-families-are-only-for-the-rich-15649230', 'Haspel, Romper, January 4, 2019')
nyt = link('https://www.nytimes.com/2014/04/10/fashion/The-Growing-Three-Child-Household-in-Manhattan.html', 'New York Times article linked in the post')
r25 = src('R/25_rep_inference.R')
ind = src('audit/review_20261015/results/independent_inference.csv', 'independent replicate inference')
claims = []
claims.append(claim('Survey release, frequency and wealthy-family sampling', 'Supported', 'The 2025 survey was released October 9, 2026. “Last week” is correct on this review’s date; an absolute date will age better. The survey is triennial, following 2022. Its area-probability sample is supplemented by a tax-record-based list sample designed to improve wealthy-family coverage. This makes the upper-tail analysis feasible; it does not remove nonresponse or public-data limitations. The Forbes 400 are excluded.', f'{release}; {scf}; {codebook}, sample-design discussion.'))
claims.append(claim('Wealthiest tenth own 81% of families’ stock; bottom half about 1%', 'Supported', f"A direct calculation from the 2025 summary CSV gives {D['stock']['top10']:.4f}% and {D['stock']['bottom50']:.4f}%. The post rounds these correctly. Families are ranked by net worth, across all ages. EQUITY includes direct and indirect stock holdings. The denominator is stock held by SCF families, not all stock outstanding; the post correctly specifies that universe.", f'{macro}, EQUITY; {src("audit/factcheck_20261016.R")}; {src("audit/results/factcheck_20261016_checks.csv")}.'))
claims.append(claim('Why use full microdata, ages 45–64 and five surveys?', 'Supported', f"The summary KIDS variable counts roster children and omits most grown children living independently. Adding the elsewhere-child question recovers information the summary extract lacks. The primary recent sample pools 2013, 2016, 2019, 2022 and 2025, with {D['sample']['recent_families']:,} distinct year-family cases eligible in at least one implicate. It is a repeated cross-section, not a panel. Ages 45–64 refer to the reference person, not necessarily the respondent or both partners.", f'{src("R/07_kids_income.R")}; {src("audit/review_20261015/results/raw_checks.csv")}; {codebook}.'))
claims.append(claim('Definition of children and disclosed counting problems', 'Supported', 'The code constructs KIDS + max(X5910, 0). The codebook explicitly includes living stepchildren and adopted children in the elsewhere question; the roster construction can include resident children-in-law and dependent children who usually live elsewhere. Such children can overlap with X5910. The post discloses these issues and correctly calls the result an imperfect family count rather than births. Deceased children are excluded. X5910 is publicly top-coded at 10 in recent waves, which matters more for averages than for the 3+ indicator.', f'{codebook}, X5910 and roster items; {macro}, KIDS; independent roster reconstruction has zero disagreements in all 13 waves.'))
claims.append(claim('Historical negative income–fertility relationship and the “new era” quotation', 'Supported', 'The quoted passage matches the abstract of the cited 2022 paper. It contrasts older negative gradients with flatter or reversed gradients in affluent economies and a positive cross-country female-employment association. The post’s historical framing is reasonable as a stylized fact, not a universal behavioral law. Cross-country and within-country relationships should remain distinct.', f'{doepke}, abstract and introduction. The author-hosted NBER version supplies the cited passage.'))
claims.append(claim('Income, education and purchasable childcare', 'Supported', 'Hazan and Zoabi support a U-shaped cross-sectional relationship between US women’s education and fertility. Bar et al. document fertility increases among high-income families and model changes in relative market-service costs. These sources motivate the SCF exercise without identifying its proposed mechanism.', f'{hazan}, abstract and 2015 journal citation; {bar}, abstract and section 4.4.2.'))
claims.append(claim('Haspel’s 2019 argument and the Manhattan father’s quotation', 'Supported', 'Haspel’s article has the correct author and date, advances the affordability argument attributed to it, and reproduces the father’s words as they appear in the post. The author has confirmed that the original New York Times quotation is verified. The broad affordability statement is Haspel’s argument rather than a measured universal restriction.', f'{haspel}; {nyt}; author confirmation, October 16, 2026.'))
claims.append(claim('Prior research uses Census/ACS; this analysis extends the exercise to SCF', 'Supported with scope limits', 'Bar et al. explicitly use the 1980 Census and 2010 ACS for their empirical fertility profiles, alongside other sources for other moments. This supports the post’s reference to Census-based work. The SCF exercise uses a different age range, family universe and child measure; it is an extension in data and question, not an exact replication. The broad phrase “much of this work” is not quantified.', f'{bar}, Appendix A; {src("kids_methods.html", "methods companion")}.'))
claims.append(claim('Usual income, capital income and age-specific ranks', 'Supported', 'The main ranking variable is NORMINC, with actual INCOME used only where usual income is unavailable in early waves. Ranks use survey weights separately within year, implicate and five-year reference-person age band, then split the upper decile. The recent analysis uses normal-year family income inclusive of capital-income sources, not wages alone. The main “1%” is therefore an age-relative group, not necessarily the national top percentile. Footnote 1 explains this.', f'{src("R/07_kids_income.R")}; {macro}, NORMINC and INCOME.'))
claims.append(claim('Income curve: roughly 40–43%, a dip at P80–90, and 49% at the top', 'Supported', 'The post describes the curve from the 10th percentile upward. P10–80 bins lie around 40–43%, with a 37.12% share at P80–90 and 49.13% in the top percentile. The 36.01% bottom decile is outside the stated range. These are descriptive bin comparisons, not separate tests of a flat curve or every local change.', f'{src("graphics/post_numbers.csv")}; {src("audit/review_20261015/revision_7121ec0/results/independent_bands.csv", "independent bin checks")}.'))
claims.append(claim('Income: significant 49% versus 40% comparison', 'Supported', f"The top percentile has {val('inc_rank','all_top3'):.2f}% with 3+ children versus {val('inc_rank','all_aff3'):.2f}% in P90–99. Their directly tested gap is {stat('inc_rank','all_gap3')} percentage points. This supports the post’s significance claim under its pointwise normal approximation. It does not establish that the top differs significantly from every other bin.", f'{r25}; {ind}.'))
claims.append(claim('Income: the excess is concentrated in exactly three children', 'Supported with interpretation limit', f"Exactly-three shares are {val('inc_rank','all_top_exact3'):.2f}% versus {val('inc_rank','all_aff_exact3'):.2f}%; the difference is {stat('inc_rank','all_gap_exact3')} points. Four-plus shares are {val('inc_rank','all_top_4plus'):.2f}% versus {val('inc_rank','all_aff_4plus'):.2f}%; that difference is {stat('inc_rank','all_gap_4plus')} points. “About equally common” fits the estimates, but no equivalence test was performed. “The third kid” describes a cross-sectional category, not a causal transition from two births to three.", f'{r25}; {ind}.'))
claims.append(claim('Historical decline overall and among the rest of the top tenth', 'Supported', 'The overall 3+ share falls from 52.40% in 1989–1998 to 40.30% in 2013–2025. Under the production income ranking, P90–99 falls from 52.99% to 39.99%, a change of −13.00 points (95% CI −18.55 to −7.45). These are age-eligible repeated cross-sections, not the same families aging over time. The early pool includes 1989, so “the 1990s” is an approximation to the chart’s explicit 1989–1998 label.', f'{src("R/27_period_change_test.R")}; {src("output/kids_period_change.rds")}; {src("audit/review_20261015/revision_7b3f727/results/overall_period_shares.csv")}.'))
claims.append(claim('The top percentile’s estimated share barely changed', 'Supported; uncertainty essential', 'The production-income estimate moves from 48.51% to 49.13%, a change of +0.62 points (95% CI −9.69 to +10.93). The gap against P90–99 widens by 13.62 points (95% CI 2.06 to 25.18). The post appropriately says “estimated” and acknowledges substantial uncertainty. This is not evidence of equivalence or proof that economic pressures had no effect. Early usual-income coverage differs; all-actual-income and wealth checks preserve the qualitative widening.', f'{src("R/27_period_change_test.R")}; {src("audit/review_20261015/revision_7b3f727/results/independent_period_change.csv")}.'))
claims.append(claim('Wealth curve: 47%, 34%, and 50% versus 37%', 'Supported', f"The figure’s P20–30 share is {N['share3p_networth_2013_25 P20-30']:.2f}% and P70–80 is {N['share3p_networth_2013_25 P70-80']:.2f}%. The top percentile is {val('nw_rank','all_top3'):.2f}% versus {val('nw_rank','all_aff3'):.2f}% in P90–99, with a gap of {stat('nw_rank','all_gap3')} points. The curve need not fall at every successive bin for the post’s “through much of” description to hold.", f'{src("graphics/post_numbers.csv")}; {r25}; {ind}.'))
claims.append(claim('Approximate wealth-dollar labels', 'Supported with scope limit', f"Dollar cutoffs are computed across all age-eligible recent records with equal wave weight, in 2025 dollars. The pooled P99 cutoff is about ${N['networth_cutoff_P99_2013_25']/1e6:.2f} million. Actual bin assignments are within age band and survey year, so those dollar ranges are descriptive approximations, not common eligibility thresholds. The post explicitly calls the labels approximate.", f'{src("R/22_post_kids_graphics.R")}; {src("audit/results/dollar_agreement_nw.csv")}.'))
claims.append(claim('Wealth: four-plus children, 19% versus 14%', 'Supported', f"The exact shares are {val('nw_rank','all_top_4plus'):.2f}% and {val('nw_rank','all_aff_4plus'):.2f}%. The directly tested difference is {stat('nw_rank','all_gap_4plus')} points. The post’s descriptive comparison is correct; its significance is only pointwise and the lower bound is close to zero.", f'{r25}; {ind}.'))
claims.append(claim('Average child counts by income and wealth', 'Supported; explanation qualified', f"Income means are {val('inc_rank','all_topkids'):.3f} and {val('inc_rank','all_affkids'):.3f}, with a gap of {stat('inc_rank','all_gapkids',3)} children. Wealth means are {val('nw_rank','all_topkids'):.3f} and {val('nw_rank','all_affkids'):.3f}, gap {stat('nw_rank','all_gapkids',3)}. The post’s 2.52 versus 2.25 is correct to two decimal places. The income gap’s interval includes zero; the wealth gap’s interval excludes zero. A mean is sensitive to large counts, but these contrasts alone do not establish that a few large families explain the income test’s lack of significance.", f'{r25}; {ind}.'))
claims.append(claim('Footnote 1: alternative ranks and top-group cutoffs', 'Supported', 'All 15 applicable production contrasts have positive pointwise lower bounds: eight income definitions and seven wealth definitions, with actual-versus-usual income applicable only to income. The ten-year, all-45–64, national, pooled-years, actual-income, top-2% and top-0.5% definitions correspond to the footnote. Exact gap ranges are 7.42–14.64 points by income and 9.27–16.14 by wealth, correctly rounded to 7–15 and 9–16. National ranking changes the ranking universe; outcomes still concern ages 45–64. These overlapping checks are not 15 independent confirmations.', f'{src("R/26_top1_definitions.R")}; {src("definitions.html")}; {src("audit/review_20261015/revision_7b3f727/results/independent_definitions.csv")}.'))
claims.append(claim('Footnote 2: duplicate corrections and family restrictions', 'Supported for the stated checks', 'The separate corrections subtract min(roster-away children, elsewhere children), either for everyone or only the top percentile. Current first-marriage couples require both raw current-marital codes to equal married and both first-marriage codes to equal one. The age check requires reference persons 50–64 and partners, if present, 45+. Ranks are estimated in the full eligible sample before these restrictions. Every specified contrast has a positive pointwise lower bound; the combined first-marriage plus symmetric correction gives 11.84 points by income and 10.65 by wealth, correctly rounded to 12 and 11.', f'{src("R/29_family_count_checks.R")}; {src("output/kids_count_checks.rds")}; {ind}.'))
claims.append(claim('How far do the counting checks generalize?', 'Qualified', 'They address specific overlap and marital-history problems, as the post says. They do not identify individual birth histories or exclude all blended families. In retained supplementary analyses, removing all adult roster children only from the top group makes both income and wealth intervals include zero; that deliberately asymmetric scenario is not an estimate of the true error. Using children living elsewhere alone also loses income significance. These limits do not contradict the named footnote checks, but rule out stronger language that all plausible child-count measures preserve significance.', f'{src("audit/results/measurement_response_20261016.csv", "saved supplementary measurement results")}; {src("audit/README.md")}.'))
claims.append(claim('Status symbolism, money as a mechanism, and the policy conclusion', 'Interpretation', 'The call to ease financial strain during early parenting is a policy judgment. The post explicitly describes the result as a correlation. Financial insulation is a plausible hypothesis, while the SCF comparison does not identify a causal effect: finances are measured after most childbearing, with selection, partnership history, inheritance and effects of children on earnings unresolved. The historical top-income group’s change is imprecise. Status motives and the effects of specific family policies are not measured.', f'{src("blog_post_text.md")}; {src("kids_methods.html", "interpretation and limits")}.'))

headline_table = table(['Ranking / outcome', 'Top percentile', 'P90–99', 'Gap and 95% interval'], [
    ['Income / 3+ children', f"{val('inc_rank','all_top3'):.2f}%", f"{val('inc_rank','all_aff3'):.2f}%", stat('inc_rank','all_gap3')+' pp'],
    ['Wealth / 3+ children', f"{val('nw_rank','all_top3'):.2f}%", f"{val('nw_rank','all_aff3'):.2f}%", stat('nw_rank','all_gap3')+' pp'],
    ['Income / mean children', f"{val('inc_rank','all_topkids'):.3f}", f"{val('inc_rank','all_affkids'):.3f}", stat('inc_rank','all_gapkids',3)],
    ['Wealth / mean children', f"{val('nw_rank','all_topkids'):.3f}", f"{val('nw_rank','all_affkids'):.3f}", stat('nw_rank','all_gapkids',3)],
])
defrows=[]
for x in D['definitions']:
    defrows.append([x['label']+' / '+x['rank'],f"{x['gap']:.2f} pp", f"{x['lower']:.2f} to {x['upper']:.2f}"])
countrows=[]
labels={'baseline':'Baseline','overlap_all':'Maximum overlap, both groups','overlap_top':'Maximum overlap, top only','strict_first':'Currently married, both first marriages','older':'Head 50–64, partner 45+','strict_first_overlap_all':'First marriages + overlap, both groups','strict_first_overlap_top':'First marriages + overlap, top only'}
for name,label in labels.items():
    xs=[next(x for x in D['counting'] if x['spec']==name and x['rank']==rank) for rank in ['inc_rank','nw_rank']]
    countrows.append([label]+[f"{x['gap']:.2f} ({x['lower']:.2f} to {x['upper']:.2f})" for x in xs])

process_bullets = [
    ('Source files and the stacked database', f"Independently compared every retained summary column with its source CSV in all 13 waves: {P['checked_wave_columns']:,} wave-column comparisons and {P['checked_summary_cells']:,} individual values. The saved database has {P['summary_rows']:,} records and {P['summary_columns']} columns. Values match, allowing only the documented blanking of unavailable early usual-income categories; absent wave-specific columns are NA and WILSH is deliberately dropped. This checks that the stacking did not change financial units or values.", src('audit/results/process_factcheck_20261016_waves.csv', 'all-wave checks')),
    ('One-to-one joins, including year and implicate', 'The summary and full public files have unique, identical record-ID sets within every wave, with zero unmatched records and zero duplicate matches. The key is (year, Y1), equivalent to year, family and implicate; Y1 = 10 × YY1 + implicate. The 1989 X1/XX1 names are normalized. Matching on family alone would multiply rows across implicates; omitting year would mix unrelated families. Neither happens in the checked inputs.', src('audit/process_factcheck_20261016.R', 'independent join audit')),
    ('Children reconstructed from the full file', 'Rebuilt KIDS from the available roster child codes 4, 13 and 36, and obtained zero disagreements with the Fed summary in every wave. Rebuilt kids = KIDS + max(X5910, 0), including the −1 “none” recode, and matched production exactly. Historical caveat: 1992 has some blank roster entries, and 1995–2004 omit the final roster slot’s x226/x230 fields. These are explicitly marked missing in the audit, not assumed to reveal a child’s relationship. All recent waves have complete values for the roster fields used here. Matching official KIDS does not turn this measure into a birth history.', src('audit/results/process_factcheck_20261016_waves.csv', 'roster and missing-field diagnostics')),
    ('Sample and rank construction', 'Rebuilt the AGE 45–64 selection separately in each implicate and matched every eligible record. Income uses NORMINC except where unavailable in 1989/1992; wealth uses NETWORTH. Recomputed weighted cumulative-midpoint ranks within survey year × implicate × five-year age band, then independently assigned all 12 right-closed bins. Every rank and bin agrees. Ties follow record order and whole-record bin assignment, so weighted top-group sizes are approximate. The repeated cross-sections do not track families across years.', src('audit/process_factcheck_20261016.R', 'sample/rank reconstruction')),
    ('Population weights and equal-wave pooling', f"Verified 5 × WGT = X42001 record by record in all 13 waves; the largest relative discrepancy is {max(x['max_weight_ratio_error'] for x in P['waves']):.2g}, consistent with stored rounding. WGT is already divided by five and is not divided again. Normalized analysis weights sum to one within every year and implicate. Pooled group statistics use a ratio of summed weighted numerators and denominators, so equal wave weight does not imply taking an unweighted average of annual subgroup shares.", src('audit/results/process_factcheck_20261016_waves.csv', 'weight checks')),
    ('Implicate aggregation and headline estimators', f"Independently reconstructed 24 headline levels and contrasts from raw inputs, including 3+, exactly three, 4+ and mean counts under both rankings. They agree with the inference outputs to numerical precision. The main inference averages five implicate-specific estimates. Some descriptive outputs pool rows first; the maximum audited difference in recent income/wealth 3+ bins is {P['max_pooling_difference_pp']:.5f} percentage point. The shared helper’s comment claiming exact equality of those approaches is too strong for subgroup ratios; the methods explains the distinction. Neither operation treats the five imputations as independent families.", src('audit/results/process_factcheck_20261016_headlines.csv', 'independent headline checks')),
    ('Replicate-weight linkage and coverage', 'Matched every first-implicate summary ID to a unique replicate-weight record in all nine inference waves, including the older X1 naming exception. Checked the 999 wt1b × mm weight products in every wave. Missing products are zeroed under the established recipe. All resulting weights are finite and nonnegative, every wave has positive total weight, and every 45–49/50–54/55–59/60–64 age-band denominator stays positive in every replicate. No weight IDs are missing.', src('audit/results/process_factcheck_20261016_replicates.csv', 'nine-wave replicate audit')),
    ('Sampling and imputation uncertainty', 'Reviewed the numerator/denominator gradients and the independently implemented replicate inference. The main and bin calculations re-estimate ranks in every replicate, use sample variance across 999 draws (denominator 998), add independent-wave sampling variance through the pooled-ratio gradient, and then add 1.2 times the between-implicate variance of the statistic or contrast. Main contrasts preserve covariance between the two groups. All 48 plotted bin estimates and SEs match independent calculations; approximate exploratory regression SEs and fixed-rank single-wave checks are not used for headline significance. Intervals remain pointwise normal approximations.', src('audit/review_20261015/revision_7121ec0/results/independent_bands.csv', 'independent bins and SEs') + '; ' + ind),
    ('Historical changes and alternative rankings', 'Checked that the period code adds independent sampling variances across the two periods and computes the imputation component on the change itself. The early income-definition break is explicit, with actual-income and wealth sensitivities. National-ranking checks rank all ages before selecting the outcome sample. For the single sensitivity ranking all surveys together, production uses joint replicate draws; an audit that perturbs one independent wave at a time gives income SE 3.86 versus 3.69 and wealth SE 3.76 versus 3.63. Both alternative intervals exclude zero. These variance calculations are documented as different, not claimed to be numerically identical.', src('audit/review_20261015/revision_7b3f727/results/independent_period_change.csv', 'period checks') + '; ' + src('audit/review_20261015/revision_7b3f727/results/pooled_rank_independent_wave_check.csv', 'pooled-rank variance comparison')),
    ('Counting restrictions and denominators', 'Independently rebuilt the current-marriage, first-marriage, partner-age and potential-overlap variables from raw responses. Recomputed all 14 counting-check point estimates, including both combined first-marriage/overlap scenarios; they match production. min(roster-away children, X5910 recoded to a nonnegative count) is the largest overlap compatible with those two counts, not the number of duplicates actually observed. Restrictions keep ranks defined in the full age-eligible sample and use the selected families in each group’s denominator. Cohabiters are excluded by the strict current-marriage check.', src('audit/results/process_factcheck_20261016_counting.csv', 'independent counting checks')),
    ('Dollar measures and stock concentration', 'Income is an annual family flow; wealth is assets minus debts at interview. Financial comparisons take the Fed’s 2025-dollar summary variables, without applying a second deflator or mixing nominal full-file amounts into the post’s ranks. Actual 2025-wave income refers to 2024. Independently checked the 2025 all-age EQUITY concentration, distinguishing wealth ranks from stock-ownership ranks and families’ stock from all corporate stock. Fractionally splitting boundary weights rather than assigning whole rows changes the top-tenth stock share by about 0.002 percentage point, leaving the post’s rounding intact. Chart dollar labels remain approximate because age/year ranks determine the bins.', src('audit/review_20261015/results/stock_checks.csv', 'stock boundary check') + '; ' + src('audit/review_20261015/results/bins_and_dollars.csv', 'dollar-label checks')),
    ('Production reproducibility', 'An isolated rebuild of the database, analysis files, production inference outputs and graphics used the local raw archives. All 10 data objects and the numbers CSV match after excluding build timestamps; all seven PNGs are byte-for-byte identical. The isolated methods document also rendered successfully. Input hashes are recorded. This checks reproducibility from the current local vintage, not a new download or proof that future releases will share the same schema. Current production merges assert row counts; this audit additionally verifies key uniqueness and ID equality, which would be useful checks to enforce inside future production runs.', src('audit/results/process_factcheck_20261016_rebuild.csv', '18 rebuild comparisons') + '; ' + src('audit/results/process_factcheck_20261016.json', 'process evidence and input hashes')),
]
process_html = '<h2 id="process-audit" tabindex="-1">Process fact-check: joins, analysis and weights</h2><p>This review also checks the pipeline behind the post. These are findings from code inspection, raw-input reconstruction, numerical assertions and an isolated production rebuild.</p><ul class="process-checks">' + ''.join(f'<li><strong>{title}.</strong> {body} <span class="evidence">Evidence: {evidence}.</span></li>' for title, body, evidence in process_bullets) + '</ul>'

overview_html = f"""
<section class="verdict-box overview" id="review-verdict" aria-labelledby="overall-assessment">
<h2 id="overall-assessment" tabindex="-1">Overall assessment</h2>
<p><strong>The post’s central numerical findings and specified robustness checks are supported by the SCF analysis.</strong> The audit finds no discrepancies in the data joins, child-count construction, sample, weights or percentile ranks for the checked inputs.</p>
<ul>
<li><strong>Income:</strong> 49.13% of top-percentile families have a child count of at least three, compared with 39.99% in the 90th–99th percentiles. The gap is 9.14 percentage points (95% CI 1.79 to 16.50).</li>
<li><strong>Wealth:</strong> the corresponding shares are 50.15% and 37.21%, a gap of 12.94 points (95% CI 5.50 to 20.39). Average child counts are 2.52 versus 2.25.</li>
<li><strong>Reproducibility:</strong> independent numerical checks support the headline estimates and chart intervals. An isolated production rebuild matches all ten data objects, the numbers CSV and all seven figures.</li>
</ul>
<p>The literature supports the post’s context. The findings are descriptive family-count comparisons; their methodological limits are discussed in <a href="#weakest-points">Three Weakest Points</a>.</p>
</section>
"""
weakest_points_html = """
<section class="weakest-points" aria-labelledby="weakest-points">
<h2 id="weakest-points" tabindex="-1">Three Weakest Points</h2>
<ol>
<li><p><strong>The family child count is an imperfect measure of childbearing.</strong> It combines children associated with either partner, including stepchildren and some children-in-law. Marriage-history restrictions and overlap corrections address specific problems, but cannot establish that other measurement errors are unrelated to wealth. Some of the pattern could reflect differences in family structure rather than births. The post discloses this limitation; the public SCF cannot fully resolve it.</p></li>
<li><p><strong>The exactly-three category does not measure a transition to a third birth.</strong> The income gap is concentrated in families counted as having exactly three children. That does not establish that wealthy two-child families are especially likely to have another child. Conditioning on a count of at least two gives an income comparison of <strong>57.6% versus 51.0%</strong>, with a <strong>6.6-point gap and 95% interval of −1.4 to 14.5</strong>. The wealth comparison is significant. This limits the behavioral interpretation of the income result, while the main 49%-versus-40% comparison is supported.</p></li>
<li><p><strong>The financial-insulation mechanism is not identified.</strong> Income and wealth are measured largely after childbearing. Selection, inheritance, partnership patterns and effects of children on earnings can influence the relationship. The historical top-income group’s estimated change has an interval spanning roughly <strong>a 10-point decline to an 11-point increase</strong>. The widening relative gap is supported, but an unchanged top-group share or insulation from childbearing pressures cannot be established. The policy argument is an interpretation of the pattern rather than an estimated treatment effect.</p></li>
</ol></section>
"""
review_sections = [
    ('overall-assessment', 'Overall assessment'),
    ('process-audit', 'Process: joins, analysis and weights'),
    ('headline-results', 'Headline results'),
    ('claim-review', 'Claim-by-claim review'),
    ('ranking-checks', 'Footnote 1: ranking checks'),
    ('counting-checks', 'Footnote 2: counting checks'),
    ('wording-changes', 'Wording and interpretation'),
    ('verification-scope', 'Verification and reproducibility'),
    ('weakest-points', 'Three Weakest Points'),
]
contents_links = ''.join('<li><a href="#' + key + '">' + escape(label) + '</a></li>' for key, label in review_sections)
contents_navigation = '<aside class="contents-rail"><details id="section-contents" open><summary>On this page</summary><nav aria-label="On this page"><p class="contents-label">On this page</p><ol class="contents-links">' + contents_links + '</ol></nav></details></aside>'

css='''
.weakest-points{margin-top:44px;padding:24px;border:1px solid var(--line);border-top:5px solid var(--note);background:var(--white)}.weakest-points h2{margin-top:0}.weakest-points ol{padding-left:24px}.weakest-points li{padding-left:5px;margin:18px 0}.weakest-points li:last-child{margin-bottom:0}h2[id]{scroll-margin-top:24px}@media(max-width:600px){.weakest-points{padding:18px 16px}.weakest-points ol{padding-left:20px}}@media print{.contents-rail{display:none}.weakest-points{break-before:page}}
:root{--paper:#f5f8fc;--ink:#15283d;--muted:#4b6075;--line:#cad7e4;--accent:#154c80;--wash:#e4edf7;--ok:#205d49;--note:#775010;--white:#fff;--scroll:#6b8197}
*{box-sizing:border-box}html{scrollbar-gutter:stable;scrollbar-color:var(--scroll) var(--paper)}body{margin:0;background:var(--paper);color:var(--ink);font:17px/1.65 Charter,"Bitstream Charter",Georgia,serif}main{max-width:1050px;margin:auto;padding:48px 28px 80px}header{margin-bottom:24px}.eyebrow,.tabs,h1,h2,h3,.verdict,th,.evidence,.metadata{font-family:"Avenir Next","Segoe UI",Arial,sans-serif}.eyebrow{text-transform:uppercase;letter-spacing:.12em;font-size:12px;color:var(--muted);font-weight:700}h1{font-size:clamp(30px,5vw,46px);line-height:1.15;margin:10px 0 0;letter-spacing:-.025em}h2{font-size:24px;line-height:1.3;margin:42px 0 14px}h3{font-size:18px;line-height:1.4;margin:0;font-weight:650}p{margin:12px 0;max-width:88ch}a{color:var(--accent);text-decoration-thickness:1px;text-underline-offset:3px}code{font: .86em/1.5 "SFMono-Regular",Consolas,monospace;overflow-wrap:anywhere}a,td,th{overflow-wrap:anywhere}button{font:inherit;cursor:pointer;min-height:46px}.tabs{display:flex;gap:8px;border-bottom:2px solid var(--line);margin-bottom:25px}button[role=tab]{background:transparent;border:0;border-bottom:4px solid transparent;color:var(--muted);padding:9px 24px;margin-bottom:-2px;font-size:16px;font-weight:650}button[aria-selected=true]{color:var(--accent);border-bottom-color:var(--accent);background:var(--wash)}:focus-visible{outline:3px solid var(--accent);outline-offset:4px}[hidden]{display:none!important}.verdict-box{padding:19px 24px;background:var(--white);border-left:5px solid var(--ok);margin:24px 0}.verdict-box h2{margin:0 0 9px;font-size:21px}.overview ul{padding-left:22px}.overview li{margin:10px 0}.table-wrap{overflow-x:auto;scrollbar-color:var(--scroll) var(--paper);margin:18px 0 26px}table{width:100%;border-collapse:collapse;font-size:15px;background:var(--white)}th,td{padding:12px 14px;text-align:left;vertical-align:top;border-bottom:1px solid var(--line);font-variant-numeric:tabular-nums}thead{background:var(--wash)}tbody th{font-size:14px;font-weight:600}td{min-width:105px}.claim{padding:24px 0;border-top:1px solid var(--line)}.claim-heading{display:flex;align-items:baseline;justify-content:space-between;gap:18px}.verdict{font-size:12px;font-weight:700;flex-shrink:0;max-width:38%;line-height:1.45}.supported{color:var(--ok)}.qualified{color:var(--note)}.evidence,.metadata{font-size:13px;color:var(--muted);line-height:1.65}.evidence{margin-bottom:0}.metadata{border-top:1px solid var(--line);padding-top:16px}.pending{background:var(--white);padding:24px;border:1px solid var(--line)}.pending h2{margin-top:0}.process-checks{padding-left:24px}.process-checks li{margin:18px 0}.process-checks .evidence{display:block;margin-top:5px}li{margin:10px 0}.skip{position:absolute;left:-9999px}.skip:focus{left:20px;top:10px;background:white;padding:10px}pre{white-space:pre-wrap;background:var(--wash);padding:18px;font-size:13px;overflow-wrap:anywhere}*::-webkit-scrollbar{width:12px;height:12px}*::-webkit-scrollbar-track{background:var(--paper)}*::-webkit-scrollbar-thumb{background:var(--scroll);border:3px solid var(--paper);border-radius:8px}*::-webkit-scrollbar-thumb:hover{background:var(--accent)}@media(max-width:600px){main{padding:28px 18px 50px}body{font-size:16px}.claim-heading{display:block}.verdict{display:block;margin-top:7px;max-width:none}th,td{padding:10px}.table-wrap{margin-left:-2px;margin-right:-2px}.tabs button{padding:9px 18px}.verdict-box{padding:16px}}@media print{body{background:#fff}main{max-width:none;padding:0}.tabs{display:none}[role=tabpanel][hidden]{display:block!important}.claim{break-inside:avoid}.table-wrap{overflow:visible}a{color:inherit}h2{break-after:avoid}}@media(forced-colors:active){:root{--line:CanvasText;--accent:LinkText}button[aria-selected=true]{border-bottom-color:Highlight}html{scrollbar-color:auto}}'''
css += '''
main{max-width:1320px}#review{min-width:0}.contents-rail{font:14px/1.45 "Avenir Next","Segoe UI",Arial,sans-serif;margin-bottom:24px;min-width:0}.contents-rail details{background:var(--white);border:1px solid var(--line);border-radius:4px;padding:12px}.contents-rail summary{font-weight:650;cursor:pointer;padding:3px 2px;min-height:28px}.contents-label{display:none}.contents-links{list-style:none;padding:0;margin:12px 0 0;max-height:min(55vh,360px);overflow-y:auto}.contents-links li{margin:0}.contents-links a{display:block;text-decoration:none;color:var(--muted);padding:10px 12px;border-left:3px solid transparent;overflow-wrap:normal}.contents-links a:hover{color:var(--accent);background:var(--paper)}.contents-links a[aria-current="location"]{color:var(--accent);background:var(--wash);border-left-color:var(--accent);font-weight:650}.contents-links li:last-child{border-top:1px solid var(--line);margin-top:8px;padding-top:8px}.contents-links li:last-child a{font-weight:650}
@media(min-width:1000px){#codex{display:grid;grid-template-columns:minmax(0,1fr)220px;gap:40px;align-items:start}#review{grid-column:1;grid-row:1}.contents-rail{grid-column:2;grid-row:1;position:sticky;top:28px;max-height:calc(100dvh - 56px);overflow-y:auto;margin:0}.contents-rail details{background:transparent;border:0;border-radius:0;padding:0}.contents-rail summary{display:none}.contents-label{display:block;margin:0 0 12px 15px;color:var(--ink);font-size:13px;font-weight:700}.contents-links{border-left:1px solid var(--line);margin:0;max-height:none;overflow:visible}.contents-links a{margin-left:-1px}}
@media print{main{max-width:none}#codex{display:block}.contents-rail{display:none}}
'''
body=f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Fact check: Kids and the top 1%</title><style>{css}</style></head><body>
<a class="skip" href="#review">Skip to review</a><main><header><div class="eyebrow">SCF · claims and evidence · October 16, 2026</div><h1>Kids and the top 1%: fact check</h1></header>
<div class="tabs" role="tablist" aria-label="AI fact-check reviews"><button id="tab-codex" type="button" role="tab" aria-selected="true" aria-controls="codex" tabindex="0">Codex</button><button id="tab-other" type="button" role="tab" aria-selected="false" aria-controls="other-ai" tabindex="-1">Other AI</button></div>
<section id="codex" role="tabpanel" aria-labelledby="tab-codex" tabindex="0">{contents_navigation}<div id="review">
<p>I am {escape(D['model'])}, and on October 16, 2026, I am executing a fact check of all substantive claims in the blog post in question, <a href="blog_post_text.md"><code>blog_post_text.md</code></a>.</p>
{overview_html}
<p class="metadata">Reviewed committed snapshot: {link(reviewed_commit_link, '<code>' + D['reviewed_commit'][:7] + '</code>')}; MD5 <code>{D['blog_md5']}</code>. The reviewer did not alter the blog, data or empirical specification.</p>

{process_html}
<h2 id="headline-results" tabindex="-1">Headline results</h2><p>Families with a reference person aged 45–64; five surveys, 2013–2025. “pp” means percentage points. Gaps compare the top percentile with the 90th–99th percentiles under the same ranking.</p>{headline_table}
<h2 id="claim-review" tabindex="-1">Claim-by-claim review</h2>{''.join(claims)}
<h2 id="ranking-checks" tabindex="-1">Footnote 1: all ranking checks</h2><p>These are production pointwise normal intervals. The separate independent-wave check of the pooled ranking also retains a positive interval; its variance need not equal production’s joint-replicate variance.</p>{table(['Definition / ranking','Gap','95% interval, pp'],defrows)}
<h2 id="counting-checks" tabindex="-1">Footnote 2: all counting checks</h2><p>Each cell is a gap in percentage points, followed by its 95% interval. The most adverse combined wealth check only narrowly excludes zero.</p>{table(['Counting or sample definition','Income gap (95% CI)','Wealth gap (95% CI)'],countrows)}
<h2 id="wording-changes" tabindex="-1">Wording and interpretation</h2>
<ul>
<li>The income mean gap’s confidence interval includes zero. Sensitivity of a mean to large families is a valid concern, but the reported tests do not establish that a few large families are the reason this contrast is insignificant.</li>
<li>The post’s “estimated” and uncertainty qualifications are essential to the historical comparison. Child counts are associated with income and wealth; the analysis does not estimate the effect of a family policy on births.</li>
<li>The release date is October 9, 2026. “Last week” is accurate on this review’s October 16 date.</li>
</ul>
<h2 id="verification-scope" tabindex="-1">Verification and reproducibility</h2>
<p>The audit covers the post, its four figures and both footnotes, together with the data pipeline behind them. It includes the following completed checks:</p>
<ul>
<li>Read the production scripts, Fed codebook and summary-variable construction; reconstructed counts, samples, weights and ranks from the raw CSV and Stata files across all 13 waves.</li>
<li>Compared 110,961,210 retained summary-data values with their inputs and checked record-ID uniqueness, identical ID sets, weight scaling, all nine sets of 999 replicate weights, 24 headline levels and contrasts, and 14 counting-check point estimates.</li>
<li>Independently checked the headline replicate inference and all 48 plotted bin estimates and standard errors, plus the alternative-ranking and historical-period calculations.</li>
<li>Compared an isolated production rebuild with the analysis outputs: all ten data objects and the numbers CSV match after excluding build timestamps, and all seven PNGs match byte for byte. The methods document rendered successfully.</li>
<li>Passed all 44 assertions in the dated verification script, including the 18 rebuild comparisons. Input and script hashes record the local vintage used for the checks.</li>
</ul>
<p>These checks use the local public-data archives rather than a new download of every file. The broader supplementary measurement table was inspected as a saved result; its full sensitivity script was not independently rerun for this report. Public-data disclosure edits, family-count measurement limits and pointwise normal intervals constrain interpretation.</p>
<p>The following commands reproduce the analysis and verification from the downloaded inputs:</p>
<pre><code>Rscript audit/review_20261015/independent_checks.R
Rscript audit/review_20261015/revision_7121ec0/check_bands.R
# Rebuild in isolation; compare the rebuilt outputs and raw-input reconstruction.
python3 audit/review_20261015/reproduce.py
Rscript audit/review_20261015/check_outputs.R
Rscript audit/process_factcheck_20261016.R
# Check alternative-ranking and historical-period inference against the isolated rebuild.
Rscript audit/review_20261015/revision_7b3f727/check_revision.R
Rscript R/29_family_count_checks.R
Rscript audit/factcheck_20261016.R
python3 audit/build_factcheck.py
quarto render kids_methods.qmd</code></pre>
<p>Source checks cover the Fed release and data documentation, Doepke et al.’s author-hosted working paper, Bar et al.’s Fed-hosted paper, Hazan and Zoabi’s publisher abstract, and Haspel’s article. The New York Times quotation is verified by the author. Source links appear with the corresponding claims.</p>
<p class="metadata">Evidence: {src('audit/results/factcheck_20261016_checks.csv','dated assertions')}, {src('audit/results/factcheck_20261016.json','numerical snapshot')}, {src('audit/results/process_factcheck_20261016.json','process audit')}, {src('audit/README.md','audit guide')}, {src('kids_methods.html','methods companion')}. This assessment applies to the version and date identified above.</p>
{weakest_points_html}
</div></section>
<section id="other-ai" role="tabpanel" aria-labelledby="tab-other" tabindex="0" hidden><div class="pending"><h2>Review from a different AI</h2><p>No second review has been supplied. This tab is reserved for a different AI’s fact check.</p><p>When that review is added, it should identify its model, date, draft version, sources and checks actually performed.</p><!-- BEGIN OTHER AI REVIEW: replace this placeholder with the separately authored review. --><!-- END OTHER AI REVIEW --></div></section>
<noscript><p>JavaScript is disabled. The Codex review is shown above; enable JavaScript to switch between the review tabs.</p></noscript>
</main><script>
const contents=document.getElementById('section-contents');
const desktopContents=window.matchMedia('(min-width:1000px)');
const sectionLinks=Array.from(document.querySelectorAll('.contents-links a'));
const sectionHeadings=sectionLinks.map(link=>document.getElementById(link.hash.slice(1)));
function setCurrentSection(id){{
  sectionLinks.forEach(link=>{{if(link.hash==='#'+id)link.setAttribute('aria-current','location');else link.removeAttribute('aria-current');}});
}}
function syncContentsLayout(){{contents.open=desktopContents.matches;}}
syncContentsLayout();
desktopContents.addEventListener('change',syncContentsLayout);
sectionLinks.forEach(link=>link.addEventListener('click',event=>{{
  if(event.button!==0||event.metaKey||event.ctrlKey||event.shiftKey||event.altKey)return;
  event.preventDefault();
  if(!desktopContents.matches)contents.open=false;
  const target=document.getElementById(link.hash.slice(1));
  window.location.hash=target.id;
  target.focus({{preventScroll:true}});
  target.scrollIntoView({{block:'start'}});
  setCurrentSection(target.id);
}}));
let contentsUpdatePending=false;
function updateCurrentSection(){{
  contentsUpdatePending=false;
  if(document.getElementById('codex').hidden)return;
  let current=sectionHeadings[0];
  for(const heading of sectionHeadings){{if(heading.getBoundingClientRect().top<=148)current=heading;else break;}}
  if(window.scrollY+window.innerHeight>=document.documentElement.scrollHeight-4)current=sectionHeadings[sectionHeadings.length-1];
  setCurrentSection(current.id);
}}
function scheduleContentsUpdate(){{if(!contentsUpdatePending){{contentsUpdatePending=true;requestAnimationFrame(updateCurrentSection);}}}}
window.addEventListener('scroll',scheduleContentsUpdate,{{passive:true}});
window.addEventListener('resize',scheduleContentsUpdate);
window.addEventListener('hashchange',scheduleContentsUpdate);
window.addEventListener('load',scheduleContentsUpdate);
scheduleContentsUpdate();
const tabs=Array.from(document.querySelectorAll('[role="tab"]'));
function activate(tab,focus=false){{tabs.forEach(t=>{{const selected=t===tab;t.setAttribute('aria-selected',String(selected));t.tabIndex=selected?0:-1;document.getElementById(t.getAttribute('aria-controls')).hidden=!selected;}});if(focus)tab.focus();scheduleContentsUpdate();}}
tabs.forEach((tab,i)=>{{tab.addEventListener('click',()=>activate(tab));tab.addEventListener('keydown',event=>{{let next;if(event.key==='ArrowRight')next=(i+1)%tabs.length;else if(event.key==='ArrowLeft')next=(i+tabs.length-1)%tabs.length;else if(event.key==='Home')next=0;else if(event.key==='End')next=tabs.length-1;else return;event.preventDefault();activate(tabs[next],true);}});}});
</script></body></html>'''
# Preserve a separately authored review when rebuilding Codex's panel. The
# section boundary is paired with the noscript footer rather than its inner tags.
output = root / 'factcheck.html'
pattern = r'<section id="other-ai"[\s\S]*?</section>(?=\s*<noscript>)'
if output.exists():
    prior = re.search(pattern, output.read_text())
    if prior:
        body = re.sub(pattern, lambda _: prior.group(0), body)
    prior_tab = re.search(r'<button id="tab-other"[^>]*>[\s\S]*?</button>', output.read_text())
    if prior_tab:
        body = re.sub(r'<button id="tab-other"[^>]*>[\s\S]*?</button>', lambda _: prior_tab.group(0), body)
# The preserved span contains both separately authored panels. Only the marked
# Codex region is populated here; never rewrite Claude's review text.
fragment = root / 'audit/codex_crosscheck_20261016.html'
if fragment.exists() and '<!-- CODEX CROSS-CHECK: BEGIN -->' in body:
    body = re.sub(r'<!-- CODEX CROSS-CHECK: BEGIN -->[\s\S]*?<!-- CODEX CROSS-CHECK: END -->',
                  lambda _: '<!-- CODEX CROSS-CHECK: BEGIN -->' + fragment.read_text() + '<!-- CODEX CROSS-CHECK: END -->', body)
    body = re.sub(r'(<h3 id="x-codex"[^>]*>)[\s\S]*?(</h3>)', r'\1Codex’s cross-check of Claude\2', body)
    body = re.sub(r'<div class="nextauthor">[\s\S]*?(?=<!-- CODEX CROSS-CHECK: BEGIN -->)',
                  '<div class="codex-cross-container">', body)
# One shared tab/sidebar controller replaces the older per-author controllers.
# Native details remain available when JavaScript is disabled.
body = re.sub(r'<script>[\s\S]*?</script>', '', body)
body = body.replace('</head>', '<link rel="stylesheet" href="audit/factcheck_navigation.css"></head>')
body = body.replace('</body>', '<script src="audit/factcheck_navigation.js"></script></body>')
output.write_text(body)
print(f'Wrote factcheck.html: {len(claims)} claim assessments; 15 ranking contrasts; 14 counting contrasts.')
