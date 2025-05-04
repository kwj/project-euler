import unittest

from euler.bin.p0003 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((13195,), '29'), ((600851475143,), '6857')]

        for args, expected in test_patterns:
            with self.subTest(f'prime number: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
