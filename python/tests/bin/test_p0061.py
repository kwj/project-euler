
import unittest
from euler.bin.p0061 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((), '28684')
        ]

        for args, expected in test_patterns:
            with self.subTest('NONE'):
                self.assertEqual(expected, compute())

if __name__ == '__main__':
    unittest.main()
