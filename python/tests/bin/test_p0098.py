import unittest

from euler.bin.p0098 import compute
from euler.lib.resource import asset_file


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((), '18769')]

        fh = asset_file('https://projecteuler.net/project/resources/p098_words.txt')
        for args, expected in test_patterns:
            with self.subTest('data file: p098_words.txt'):
                self.assertEqual(expected, compute(fh))
        fh.close()


if __name__ == '__main__':
    unittest.main()
