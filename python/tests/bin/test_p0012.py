import unittest

from euler.bin.p0012 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((5,), '28'), ((500,), '76576500')]

        for args, expected in test_patterns:
            with self.subTest(f'num of divisors: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
