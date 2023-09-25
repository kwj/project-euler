import unittest

from euler.bin.p0078 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((1_000_000,), '55374')]

        for args, expected in test_patterns:
            with self.subTest(f'denominator: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
