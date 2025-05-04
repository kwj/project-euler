import unittest

from euler.bin.p0053 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((23, 1_000_000), '4'), ((100, 1_000_000), '4075')]

        for args, expected in test_patterns:
            with self.subTest(f'exp: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
