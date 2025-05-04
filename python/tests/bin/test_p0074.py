import unittest

from euler.bin.p0074 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((1_000_000, 60), '402')]

        for args, expected in test_patterns:
            with self.subTest(f'specs: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
