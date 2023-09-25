import unittest

from euler.bin.p0022 import compute
from euler.lib.resource import asset_file


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((), '871198282')]

        fh = asset_file('https://projecteuler.net/project/resources/p022_names.txt')
        for args, expected in test_patterns:
            with self.subTest('data file: p022_names.txt'):
                self.assertEqual(expected, compute(fh))
        fh.close()


if __name__ == '__main__':
    unittest.main()
