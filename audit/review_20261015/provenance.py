"""Read-only verification of raw archives against Fed downloads and extracted files."""
from pathlib import Path
import csv, hashlib, urllib.request, zipfile, io, concurrent.futures, subprocess, sys

ROOT = Path.cwd()
OUT = ROOT / 'audit/review_20261015'
BASE = 'https://www.federalreserve.gov/econres/files/'

def digest(data):
    return hashlib.sha256(data).hexdigest()

def check(path):
    local = path.read_bytes()
    url = BASE + path.name
    result = dict(file=str(path.relative_to(ROOT)), url=url,
                  local_sha256=digest(local), remote_sha256='', archive_matches='',
                  extracted_matches='', error='')
    try:
        request = urllib.request.Request(url, headers={'User-Agent': 'SCF reproducibility audit'})
        with urllib.request.urlopen(request, timeout=120) as response:
            remote = response.read()
        result['remote_sha256'] = digest(remote)
        result['archive_matches'] = remote == local
        checks = []
        with zipfile.ZipFile(io.BytesIO(local)) as archive:
            for name in archive.namelist():
                if name.lower().endswith(('.csv', '.dta')):
                    folder = ROOT/'data_raw/xlsx' if name.lower().endswith('.csv') else path.parent
                    target = folder/Path(name).name
                    checks.append(target.exists() and digest(archive.read(name)) == digest(target.read_bytes()))
        result['extracted_matches'] = bool(checks) and all(checks)
    except Exception as error:
        result['error'] = str(error)
    print(path.name, result['archive_matches'], result['extracted_matches'], result['error'], flush=True)
    return result

if '--sources-only' not in sys.argv:
    archives = sorted((ROOT/'data_raw').glob('*/*.zip'))
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        results = list(pool.map(check, archives))
    with (OUT/'results/provenance.csv').open('w') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(results[0]))
        writer.writeheader()
        writer.writerows(results)

sources = []
for name in ['bulletin.macro.txt', 'codebk2025.txt', 'codebk2016.txt', 'codebk95.txt',
             'codebk92.txt', 'Standard_Error_Documentation.pdf']:
    try:
        # The Fed text-document endpoint rejected urllib's default request; curl
        # successfully retrieves the same public URLs. Check HTTP failures explicitly.
        data = subprocess.check_output(['curl', '-fsSL', '--max-time', '90', BASE+name])
        (OUT/'sources'/name).write_bytes(data)
        original = ROOT/('docs/'+name)
        if name == 'bulletin.macro.txt':
            original = ROOT/'audit/sources/bulletin.macro.txt'
        sources.append(dict(file=name,url=BASE+name,sha256=digest(data),
                            matches_stored=original.read_bytes()==data if original.exists() else 'not previously stored'))
        print('source', name, 'matches local:', original.read_bytes() == data if original.exists() else 'new', flush=True)
    except Exception as error:
        print('source', name, str(error), flush=True)
if sources:
    with (OUT/'results/source_provenance.csv').open('w') as stream:
        writer = csv.DictWriter(stream,fieldnames=list(sources[0]))
        writer.writeheader(); writer.writerows(sources)
