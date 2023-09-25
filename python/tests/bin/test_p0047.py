import unittest

from euler.bin.p0047 import compute


class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [((2,), '14'), ((3,), '644'), ((4,), '134043')]

        for args, expected in test_patterns:
            with self.subTest(f'num of factors: {args}'):
                self.assertEqual(expected, compute(*args))


if __name__ == '__main__':
    unittest.main()
