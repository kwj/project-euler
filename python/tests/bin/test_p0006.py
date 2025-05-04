import unittest

from euler.bin.p0006 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((10,), '2640'), ((100,), '25164150')]

        for args, expected in test_patterns:
            with self.subTest(f'upper: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
