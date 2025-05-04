import unittest

from euler.bin.p0007 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((6,), '13'), ((10_001,), '104743')]

        for args, expected in test_patterns:
            with self.subTest(f'nth: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
