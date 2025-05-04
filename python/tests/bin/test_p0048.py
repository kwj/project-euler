import unittest

from euler.bin.p0048 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((10,), '0405071317'), ((1_000,), '9110846700')]

        for args, expected in test_patterns:
            with self.subTest(f'exp: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
