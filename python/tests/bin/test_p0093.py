import unittest

from euler.bin.p0093 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((), '1258')]

        for args, expected in test_patterns:
            with self.subTest('NONE'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
