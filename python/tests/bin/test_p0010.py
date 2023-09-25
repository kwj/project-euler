import unittest

from euler.bin.p0010 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((10,), '17'), ((2_000_000,), '142913828922')]

        for args, expected in test_patterns:
            with self.subTest(f'upper: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
