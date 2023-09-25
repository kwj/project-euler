import unittest

from euler.bin.p0035 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((100,), '13'), ((1_000_000,), '55')]

        for args, expected in test_patterns:
            with self.subTest(f'upper: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
