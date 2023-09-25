import unittest

from euler.bin.p0005 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((10,), '2520'), ((20,), '232792560')]

        for args, expected in test_patterns:
            with self.subTest(f'upper: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
