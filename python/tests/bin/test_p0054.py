import unittest

from euler.bin.p0054 import compute
from euler.lib.resource import asset_file


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [('p054_poker.txt', '376')]

        for fname, expected in test_patterns:
            with asset_file(fname) as fh, self.subTest(f'data file: {fname}'):
                self.assertEqual(expected, compute(fh))


if __name__ == '__main__':
    unittest.main()
