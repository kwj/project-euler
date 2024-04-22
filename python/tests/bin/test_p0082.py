import unittest

from euler.bin.p0082 import compute
from euler.lib.resource import asset_file


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((), '260324')]

        fh = asset_file('p082_matrix.txt')
        for args, expected in test_patterns:
            with self.subTest('data file: p082_matrix.txt'):
                self.assertEqual(expected, compute(min, fh))
        fh.close()


if __name__ == '__main__':
    unittest.main()
