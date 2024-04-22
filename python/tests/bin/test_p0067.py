import unittest

from euler.bin.p0067 import compute
from euler.lib.resource import asset_file


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((), '7273')]

        fh = asset_file('p067_triangle.txt')
        for args, expected in test_patterns:
            with self.subTest('data file: p067_triangle.txt'):
                self.assertEqual(expected, compute(max, fh))
        fh.close()


if __name__ == '__main__':
    unittest.main()
