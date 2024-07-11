import unittest

from euler.bin.p0075 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((48,), '6'), ((1_500_000,), '161667')]

        for args, expected in test_patterns:
            with self.subTest(f'upper: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
