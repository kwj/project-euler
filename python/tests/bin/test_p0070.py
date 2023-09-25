import unittest

from euler.bin.p0070 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((), '8319823')]

        for args, expected in test_patterns:
            with self.subTest('NONE'):
                self.assertEqual(expected, compute())


if __name__ == '__main__':
    unittest.main()
