import unittest

from euler.bin.p0071 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((8,), '2'), ((1_000_000,), '428570')]

        for args, expected in test_patterns:
            with self.subTest(f'limit: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
