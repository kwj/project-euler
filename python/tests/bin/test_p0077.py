import unittest

from euler.bin.p0077 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [
            ((4,), '10'),
            ((5_000,), '71'),
        ]

        for args, expected in test_patterns:
            with self.subTest(f'number: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
