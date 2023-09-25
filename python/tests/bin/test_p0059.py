import unittest

from euler.bin.p0059 import compute
from euler.lib.resource import asset_file


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((), '129448')]

        fh = asset_file('https://projecteuler.net/project/resources/p059_cipher.txt')
        for args, expected in test_patterns:
            with self.subTest('data file: p059_cipher.txt'):
                self.assertEqual(expected, compute(fh))
        fh.close()


if __name__ == '__main__':
    unittest.main()
