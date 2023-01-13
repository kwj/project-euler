
import unittest
from euler.bin.p0092 import compute

class TestSolution(unittest.TestCase):
    def test_compute(self):
        test_patterns = [
            ((10,), '7'),
            ((10_000_000,), '8581146')
        ]

        for args, expected in test_patterns:
            with self.subTest(f'limit: {args}'):
                self.assertEqual(expected, compute(*args))

    def test_invalid_parameter(self):
        with self.assertRaises(AssertionError):
            compute(101)

if __name__ == '__main__':
    unittest.main()
