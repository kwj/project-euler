
import unittest
from euler.bin.p0008 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((4,), '5832'),
            ((13,), '23514624000')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'length: {args}'):
                self.assertEqual(expected, compute(*args))

if __name__ == '__main__':
    unittest.main()
