import hashlib
import json
import urllib.request
from pathlib import Path
from typing import IO

asset_dir = Path(__file__).parents[2] / 'assets'


def asset_file(url: str) -> IO:
    filename = url[url.rfind('/') + 1 :]
    asset_file = asset_dir / filename
    if asset_file.exists() is False:
        download(url, filename)
        with urllib.request.urlopen(url) as fr:
            with open(str(asset_file), 'wb') as fw:
                fw.write(fr.read())
    else:
        d = read_hashdict()
        with open(str(asset_file), 'rb') as fr:
            if d[filename] != hashlib.sha256(fr.read()).hexdigest():
                print('Warning: hash mismatch - {}'.format(filename))

    f = open(str(asset_file))
    return f


def download(url: str, filename: str):
    with urllib.request.urlopen(url) as fr:
        content = fr.read()
        with open(str(asset_dir / filename), 'wb') as fw:
            fw.write(content)
    d = read_hashdict()
    d[filename] = hashlib.sha256(content).hexdigest()
    write_hashdict(d)


def read_hashdict() -> dict[str, str]:
    hash_file = asset_dir / 'hashes.json'
    if hash_file.exists() is False:
        return {}
    else:
        with open(str(hash_file)) as f:
            return json.load(f)


def write_hashdict(d: dict[str, str]):
    hash_file = asset_dir / 'hashes.json'
    with open(str(hash_file), 'w') as f:
        json.dump(d, f, indent=2)
