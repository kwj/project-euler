import unittest

from euler.bin.p0029 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((5,), '15'), ((100,), '9183')]

        for args, expected in test_patterns:
            with self.subTest(f'upper: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
