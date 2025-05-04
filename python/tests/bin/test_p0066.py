import unittest

from euler.bin.p0066 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((7,), '5'), ((1_000,), '661')]

        for args, expected in test_patterns:
            with self.subTest(f'nth: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
