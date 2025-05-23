import unittest

from euler.bin.p0086 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((1_975,), '100'), ((1_000_000,), '1818')]

        for args, expected in test_patterns:
            with self.subTest(f'boundary: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
