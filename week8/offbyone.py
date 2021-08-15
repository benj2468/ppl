from typing import Tuple
import decimal
from copy import deepcopy
from enum import Enum


class Operation(Enum):
    Add = 1,
    Sub = 2,
    Mul = 3


class OffByOne:
    def __init__(self, i) -> None:
        self.initial = i
        self.precision = 1

        self.op = None

    def __add__(self, rhs):
        return self.operation(rhs, Operation.Add)

    def __sub__(self, rhs):
        return self.operation(rhs, Operation.Sub)

    def __mul__(self, rhs):
        return self.operation(rhs, Operation.Mul)

    def operation(self, rhs, op):
        new = deepcopy(self)
        new.initial = (self, rhs)
        new.op = op
        return new

    def __repr__(self) -> str:
        prec = 1
        (low, high) = self.bounds_with_precision(prec)
        while high - low > 1:
            prec += 1
            (low, high) = self.bounds_with_precision(prec)

        return f"{round(low)}±1"

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
        if isinstance(self.initial, tuple):
            if self.initial[0]: self.initial[0].update_precision(precision)
            if self.initial[1]: self.initial[1].update_precision(precision)

    def bounds(self):
        if isinstance(self.initial, tuple):
            a = self.initial[0].bounds()
            b = self.initial[1].bounds()
            if self.op == Operation.Add:
                return (a[0] + b[0], a[1] + b[1])
            elif self.op == Operation.Sub:
                return (a[0] - b[0], a[1] - b[1])
            elif self.op == Operation.Mul:
                new_low = min(a[0] * b[0], a[1] * b[0], a[0] * b[1],
                              a[1] * b[1])
                new_high = max(a[0] * b[0], a[1] * b[0], a[0] * b[1],
                               a[1] * b[1])
                return (new_low, new_high)
        else:
            if self.precision == 1:
                delta = 1
            else:
                delta = "0."
                for _ in range(1, self.precision - 1):
                    delta += '0'
                delta += "1"
                delta = float(delta)
            low = decimal.Decimal(self.initial) - decimal.Decimal(delta)
            high = decimal.Decimal(self.initial) + decimal.Decimal(delta)
            return (low, high)


class Exact(OffByOne):
    def __init__(self, i) -> None:
        if round(i) != i:
            raise "Exact value MUST be an integer"
        super()

    def bounds(self):
        (decimal.Decimal(super.i), decimal.Decimal(super.i))


def runTests():
    print(
        (OffByOne(3) + OffByOne(0.25) - OffByOne(10**10) + OffByOne(10**10)) *
        OffByOne(3))

    print("ran all tests. 🚀")


runTests()