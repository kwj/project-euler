import unittest

from euler.bin.p0042 import compute
from euler.lib.resource import asset_file


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((), '162')]

        fh = asset_file('https://projecteuler.net/project/resources/p042_words.txt')
        for args, expected in test_patterns:
            with self.subTest('data file: p042_words.txt'):
                self.assertEqual(expected, compute(fh))
        fh.close()


if __name__ == '__main__':
    unittest.main()
