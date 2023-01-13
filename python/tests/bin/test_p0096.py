
import unittest
from euler.bin.p0096 import compute
from euler.lib.resource import asset_file

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((), '24702')
        ]

        fh = asset_file('https://projecteuler.net/project/resources/p096_sudoku.txt')
        for args, expected in test_patterns:
            with self.subTest(f'data file: p096_sudoku.txt'):
                self.assertEqual(expected, compute(fh))
        fh.close()

if __name__ == '__main__':
    unittest.main()
