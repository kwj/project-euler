import unittest

from euler.bin.p0017 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self) -> None:
        test_patterns = [((5,), '19'), ((1_000,), '21124')]

        for args, expected in test_patterns:
            with self.subTest('NONE'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
