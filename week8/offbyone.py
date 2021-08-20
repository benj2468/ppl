from __future__ import annotations
from typing import Tuple
from decimal import *
from enum import Enum
from math import floor


class Bounds(Enum):
    Low = 0
    High = 1


def operation_switch(op, bound: Bounds):
    return op.value[bound.value]


class Operation(Enum):
    Add = (lambda a, b: (a[0] + b[0]), lambda a, b: (a[1] + b[1]))
    Sub = (lambda a, b: (a[0] - b[0]), lambda a, b: a[1] - b[1])
    Mul = (
        lambda a, b: (min(a[0] * b[0], a[1] * b[0], a[0] * b[1], a[1] * b[1])),
        lambda a, b: max(a[0] * b[0], a[1] * b[0], a[0] * b[1], a[1] * b[1]))
    Div = (lambda a, b: a[0] / b[1], lambda a, b: a[1] / b[0])


class OperationNode:
    def __call__(
        self,
        *args: Any,
    ) -> Any:
        getcontext().rounding = ROUND_FLOOR
        low = operation_switch(self._op, Bounds.Low)(*args)
        getcontext().rounding = ROUND_CEILING
        high = operation_switch(self._op, Bounds.High)(*args)
        return (low, high)

    @classmethod
    def leaf(self, leaf) -> OperationNode:
        node = OperationNode()
        node._node = leaf
        return node

    @classmethod
    def tree(self, left, right, op: Operation) -> OperationNode:
        node = OperationNode()
        node._node = None
        node._left = left
        node._right = right
        node._op = op
        return node

    def is_leaf(self) -> bool:
        return self._node != None

    def get_node(self):
        return self._node

    def get_branches(self):
        return (self._left, self._right)


class OffByOne:
    def __init__(self, i) -> None:
        self._precision = 1
        self._node: OperationNode = OperationNode.leaf(i)

    def __add__(self, rhs: OffByOne) -> OffByOne:
        return self.operation(rhs, Operation.Add)

    def __sub__(self, rhs: OffByOne) -> OffByOne:
        return self.operation(rhs, Operation.Sub)

    def __mul__(self, rhs: OffByOne) -> OffByOne:
        return self.operation(rhs, Operation.Mul)

    def __truediv__(self, rhs: OffByOne) -> OffByOne:
        return self.operation(rhs, Operation.Div)

    def __repr__(self) -> str:
        prec = 1
        (low, high) = self.bounds_with_precision(prec)
        while high - low > 1:
            prec += 1
            (low, high) = self.bounds_with_precision(prec)

        return f"{floor(high)}±1"

    def with_precision(self, precision: int) -> OffByOne:
        self._precision = precision
        return self

    def operation(self, rhs: OffByOne, op: Operation) -> OffByOne:
        offByOp = OffByOne(self)
        offByOp._node = OperationNode.tree(self, rhs, op)
        return offByOp.with_precision(self._precision)

    def bounds_with_precision(self, precision: int) -> Tuple[Decimal, Decimal]:
        if precision < 1:
            raise "Precision must be >= 1"
        self.update_precision(precision)
        return self.bounds()

    def update_precision(self, precision: int) -> None:
        if self._precision < precision:
            self._precision = precision
        if not self._node.is_leaf():
            (left, right) = self._node.get_branches()
            left.update_precision(precision)
            right.update_precision(precision)

    def bounds(self) -> Tuple[Decimal, Decimal]:
        node = self._node.get_node()
        getcontext().prec = self._precision
        if node:
            getcontext().rounding = ROUND_FLOOR
            low = Decimal(node)
            getcontext().rounding = ROUND_CEILING
            high = Decimal(node)
            return (low, high)
        else:
            (left, right) = self._node.get_branches()
            a = left.bounds()
            b = right.bounds()
            return self._node(a, b)


def runTests():
    assert (
        ((OffByOne(3) + OffByOne(0.25) - OffByOne(10**10) + OffByOne(10**10)) *
         OffByOne(3))).__repr__() == "9±1"

    assert ((OffByOne(3) * OffByOne(0.25)).__repr__() == "0±1")

    assert (OffByOne(0.3).__repr__() == "0±1")

    assert ((OffByOne(10.453) + OffByOne(.532)).__repr__() == "11±1")

    assert ((OffByOne(1) / OffByOne(3)).__repr__() == "0±1")

    assert ((OffByOne(3) + OffByOne(0.25) - OffByOne(10**100) +
             OffByOne(10**100)).__repr__() == "4±1")

    print("ran all tests. 🚀")


runTests()