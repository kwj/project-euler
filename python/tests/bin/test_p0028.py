import unittest

from euler.bin.p0028 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((5,), '101'), ((1_001,), '669171001')]

        for args, expected in test_patterns:
            with self.subTest(f'length: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
