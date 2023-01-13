
import unittest
from euler.bin.p0083 import compute
from euler.lib.resource import asset_file

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((), '425185')
        ]

        fh = asset_file('https://projecteuler.net/project/resources/p083_matrix.txt')
        for args, expected in test_patterns:
            with self.subTest(f'data file: p083_matrix.txt'):
                self.assertEqual(expected, compute(min, fh))
        fh.close()

if __name__ == '__main__':
    unittest.main()
