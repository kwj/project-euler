import unittest

from euler.bin.p0009 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [
            ((3 + 4 + 5,), '60'),
            ((9 + 12 + 15,), '1620'),
            ((1000,), '31875000'),
        ]

        for args, expected in test_patterns:
            with self.subTest(f'perimeter: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
