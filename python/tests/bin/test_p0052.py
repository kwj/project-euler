import unittest

from euler.bin.p0052 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((), '142857')]

        for args, expected in test_patterns:
            with self.subTest('NONE'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
