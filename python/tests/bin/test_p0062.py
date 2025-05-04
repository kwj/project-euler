import unittest

from euler.bin.p0062 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((3,), '41063625'), ((5,), '127035954683')]

        for args, expected in test_patterns:
            with self.subTest(f'num of permutations: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
