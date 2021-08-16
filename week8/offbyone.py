from __future__ import annotations
from typing import Tuple
import decimal
from copy import deepcopy
from enum import Enum
from math import ceil, floor


class Operation(Enum):
    Add = lambda a, b: (a[0] + b[0], a[1] + b[1])
    Sub = lambda a, b: (a[0] - b[0], a[1] - b[1])
    Mul = lambda a, b: (min(a[0] * b[0], a[1] * b[0], a[0] * b[1], a[1] * b[
        1]), max(a[0] * b[0], a[1] * b[0], a[0] * b[1], a[1] * b[1]))
    Div = lambda a, b: (a[0] / b[1], a[1] / b[0])


class OperationNode:
    @classmethod
    def leaf(self, leaf) -> OperationNode:
        node = OperationNode()
        node.node = leaf
        return node

    @classmethod
    def tree(self, left, right, op: Operation) -> OperationNode:
        node = OperationNode()
        node.node = None
        node.left = left
        node.right = right
        node.op = op
        return node

    def is_leaf(self) -> bool:
        return self.node != None

    def calculate(self):
        a = self.left.bounds()
        b = self.right.bounds()
        return self.op(a, b)


class OffByOne:
    def __init__(self, i) -> None:
        self.precision = 1
        self.node: OperationNode = OperationNode.leaf(i)

    @classmethod
    def op(self, left: OffByOne, right: OffByOne, op: Operation) -> None:
        node = OffByOne(left)
        node.node = OperationNode.tree(left, right, op)
        return node

    def __add__(self, rhs: OffByOne) -> OffByOne:
        return self.operation(rhs, Operation.Add)

    def __sub__(self, rhs: OffByOne) -> OffByOne:
        return self.operation(rhs, Operation.Sub)

    def __mul__(self, rhs: OffByOne) -> OffByOne:
        return self.operation(rhs, Operation.Mul)

    def __truediv__(self, rhs: OffByOne) -> OffByOne:
        return self.operation(rhs, Operation.Div)

    def with_precision(self, precision: int) -> OffByOne:
        self.precision = precision
        return self

    def operation(self, rhs: OffByOne, op: Operation):
        return OffByOne.op(self, rhs, op).with_precision(self.precision)

    def __repr__(self) -> str:
        prec = 1
        (low, high) = self.bounds_with_precision(prec)
        while high - low > 1:
            prec += 1
            (low, high) = self.bounds_with_precision(prec)

        return f"{floor(high)}±1"

    # Create a bounds_with_precision(prec) method that will return
    # a lower and upper-bound Decimal value.
    #
    # For example OffByOne('3.55').bounds_with_precision(1) could return (Decimal('3'), Decimal('4').
    # Other valid responses would be (Decimal('3.5'), Decimal ('3.55')), (Decimal('1'),Decimal('10')),
    # or even (Decimal('3.55'), Decimal ('3.55')). It is important that the true value is in
    # between the lower and upper bound (inclusive), and it is also important
    # that the difference between the bounds gets less as the precision increases.
    def bounds_with_precision(self, precision: int):
        if precision < 1:
            raise "Precision must be >= 1"
        self.update_precision(precision)
        return self.bounds()

    def update_precision(self, precision: int):
        self.precision = precision
        if not self.node.is_leaf():
            self.node.left.update_precision(precision)
            self.node.right.update_precision(precision)

    def bounds(self) -> Tuple[decimal.Decimal, decimal.Decimal]:
        if self.node.is_leaf():
            if self.precision == 1:
                delta = 1
            else:
                delta = "0."
                for _ in range(1, self.precision - 1):
                    delta += '0'
                delta += "1"
                delta = float(delta)

            low = decimal.Decimal(self.node.node) - decimal.Decimal(delta)
            high = decimal.Decimal(self.node.node) + decimal.Decimal(delta)
            return (low, high)
        else:
            return self.node.calculate()


def runTests():
    assert (
        ((OffByOne(3) + OffByOne(0.25) - OffByOne(10**10) + OffByOne(10**10)) *
         OffByOne(3))).__repr__() == "9±1"

    assert ((OffByOne(3) * OffByOne(0.25)).__repr__() == "1±1")

    assert (OffByOne(0.3).__repr__() == "0±1")

    assert ((OffByOne(10.453) + OffByOne(.532)).__repr__() == "11±1")

    assert ((OffByOne(1) / OffByOne(3)).__repr__() == "1±1")

    print("ran all tests. 🚀")


runTests()