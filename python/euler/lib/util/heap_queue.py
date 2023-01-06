
import heapq

class HeapQueue:
    '''wrapper class of heapq'''
    def __init__(self, desc = False):
        self.__sign = 1 if desc == False else -1
        self.__heap = []

    def insert(self, item):
        heapq.heappush(self.__heap, (item[0] * self.__sign, item))

    def extract(self):
        _, item = heapq.heappop(self.__heap)
        return item

    def peek(self):
        _, item = self.__heap[0]
        return item

    def is_empty(self):
        return len(self.__heap) == 0
