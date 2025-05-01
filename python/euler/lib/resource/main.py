from pathlib import Path
from typing import IO

asset_dir = Path(__file__).parents[2] / 'assets'


def asset_file(filename: str) -> IO:
    return open(str(asset_dir / filename))
