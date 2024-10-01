import heapq
from typing import Any


class HeapQueue:
    """wrapper class of heapq"""

    def __init__(self, desc: bool = False):
        self.__sign = 1 if desc is False else -1
        self.__heap: list[tuple[float, Any]] = []

    def insert(self, item: tuple[float, Any]):
        heapq.heappush(self.__heap, (item[0] * self.__sign, item))

    def extract(self) -> Any:
        _, item = heapq.heappop(self.__heap)
        return item

    def peek(self) -> Any:
        _, item = self.__heap[0]
        return item

    def is_empty(self) -> bool:
        return len(self.__heap) == 0
