import unittest

from euler.bin.p0065 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((10,), '17'), ((100,), '272')]

        for args, expected in test_patterns:
            with self.subTest(f'nth: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
