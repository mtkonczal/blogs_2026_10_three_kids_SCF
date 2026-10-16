"""Rebuild current production scripts in an isolated folder, preserving originals."""
from pathlib import Path
import subprocess, shutil, time

root = Path.cwd()
base = root/'audit/review_20261015'
run = base/'reproduction'
logs = base/'logs'
run.mkdir(parents=True, exist_ok=True)
logs.mkdir(exist_ok=True)
for name in ['R', 'graphics']:
    shutil.copytree(root/name, run/name, dirs_exist_ok=True)
for name in ['data', 'output']:
    (run/name).mkdir(exist_ok=True)
for name in ['data_raw', 'docs']:
    if not (run/name).exists():
        (run/name).symlink_to(root/name, target_is_directory=True)
shutil.copy2(root/'kids_methods.qmd', run/'kids_methods.qmd')
# R/29 compares its separate specifications with this independent reference table.
reference = Path('audit/review_20261015/results/independent_inference.csv')
(run/reference).parent.mkdir(parents=True, exist_ok=True)
shutil.copy2(root/reference, run/reference)
for script in ['02_build_db', '07_kids_income', '24_kids_blended',
               '23_kids_wealth_work', '25_rep_inference', '26_top1_definitions',
               '27_period_change_test', '28_bin_intervals', '29_family_count_checks',
               '22_post_kids_graphics']:
    start = time.time()
    with (logs/(script+'.log')).open('w') as stream:
        result = subprocess.run(['Rscript', 'R/'+script+'.R'], cwd=run,
                                stdout=stream, stderr=subprocess.STDOUT)
    print(script, result.returncode, round(time.time()-start, 1), 'seconds', flush=True)
    result.check_returncode()
with (logs/'render.log').open('w') as stream:
    result = subprocess.run(['quarto', 'render', 'kids_methods.qmd'], cwd=run,
                            stdout=stream, stderr=subprocess.STDOUT)
print('render', result.returncode, flush=True)
result.check_returncode()
