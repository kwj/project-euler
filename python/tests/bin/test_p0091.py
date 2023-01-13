
import unittest
from euler.bin.p0091 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((2, 2), '14'),
            ((50, 50), '14234')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'matrix: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
