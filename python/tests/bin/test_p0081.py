
import unittest
from euler.bin.p0081 import compute
from euler.lib.resource import asset_file

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((), '427337')
        ]

        fh = asset_file('https://projecteuler.net/project/resources/p081_matrix.txt')
        for args, expected in test_patterns:
            with self.subTest(f'data file: p081_matrix.txt'):
                self.assertEqual(expected, compute(min, fh))
        fh.close()

if __name__ == '__main__':
    unittest.main()
