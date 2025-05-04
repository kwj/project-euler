import unittest

from euler.bin.p0022 import compute
from euler.lib.resource import asset_file


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [('p022_names.txt', '871198282')]

        for fname, expected in test_patterns:
            with asset_file(fname) as fh, self.subTest(f'data file: {fname}'):
                self.assertEqual(expected, compute(fh))


if __name__ == '__main__':
    unittest.main()
