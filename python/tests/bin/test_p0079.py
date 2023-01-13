
import unittest
from euler.bin.p0079 import compute
from euler.lib.resource import asset_file

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((), '73162890')
        ]

        fh = asset_file('https://projecteuler.net/project/resources/p079_keylog.txt')
        for args, expected in test_patterns:
            with self.subTest(f'data file: p079_keylog.txt'):
                self.assertEqual(expected, compute(fh))
        fh.close()

if __name__ == '__main__':
    unittest.main()
