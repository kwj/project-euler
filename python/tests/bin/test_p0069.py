import unittest

from euler.bin.p0069 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((10,), '6'), ((1_000_000,), '510510')]

        for args, expected in test_patterns:
            with self.subTest(f'limit: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
