import unittest

from euler.bin.p0015 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((2, 2), '6'), ((20, 20), '137846528820')]

        for args, expected in test_patterns:
            with self.subTest(f'grid: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
