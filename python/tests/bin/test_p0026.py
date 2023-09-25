import unittest

from euler.bin.p0026 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((10,), '7'), ((300,), '289'), ((1_000,), '983')]

        for args, expected in test_patterns:
            with self.subTest(f'limit: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
