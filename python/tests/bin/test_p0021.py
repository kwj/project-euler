import unittest

from euler.bin.p0021 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((10_000,), '31626')]

        for args, expected in test_patterns:
            with self.subTest('NONE'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
