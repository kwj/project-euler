
import unittest
from euler.bin.p0004 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((2,), '9009'),
            ((3,), '906609')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'digit: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
