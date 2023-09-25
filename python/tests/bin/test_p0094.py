import unittest

from euler.bin.p0094 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((1_000_000_000,), '518408346')]

        for args, expected in test_patterns:
            with self.subTest(f'limit: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
