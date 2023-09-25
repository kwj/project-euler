import unittest

from euler.bin.p0088 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((6,), '30'), ((12,), '61'), ((12_000,), '7587457')]

        for args, expected in test_patterns:
            with self.subTest(f'upper "k": {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
