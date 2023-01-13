
import unittest
from euler.bin.p0068 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((), '6531031914842725')
        ]

        for args, expected in test_patterns:
            with self.subTest('NONE'):
                self.assertEqual(expected, compute())

if __name__ == '__main__':
    unittest.main()
