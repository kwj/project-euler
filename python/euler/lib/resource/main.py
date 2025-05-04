from pathlib import Path
from typing import TextIO

asset_dir = Path(__file__).parents[2] / 'assets'


def asset_file(filename: str) -> TextIO:
    return open(str(asset_dir / filename))
