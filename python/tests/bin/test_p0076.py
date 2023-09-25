import unittest

from euler.bin.p0076 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((list(range(1, 100)), 100), '190569291')]

        for args, expected in test_patterns:
            with self.subTest(f'numbers: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
