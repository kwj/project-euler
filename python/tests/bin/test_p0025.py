
import unittest
from euler.bin.p0025 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((3,), '12'),
            ((1_000,), '4782')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'digits: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
