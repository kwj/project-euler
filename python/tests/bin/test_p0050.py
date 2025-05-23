import unittest

from euler.bin.p0050 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [
            ((100,), '41'),
            ((500,), '499'),
            ((1_000,), '953'),
            ((10_000,), '9521'),
            ((1_000_000,), '997651'),
        ]

        for args, expected in test_patterns:
            with self.subTest(f'upper: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
