import ast
import sys
import os
from typing import Any, Dict, List, Tuple


Memory = List[int]


def get_size(ty: str) -> int:
    def size_helper(ty: str) -> str:
        return len(ty.split(","))

    unionTy = ty.split("|")
    if len(unionTy) == 1:
        return size_helper(unionTy[0])
    else:
        return 1 + max(*[size_helper(ty) for ty in unionTy])


def convert_to_memory(value: Any) -> List[int]:
    if isinstance(value, tuple):
        return convert_to_memory(value[1])
    else:
        if isinstance(value, list):
            res = []
            for x in value:
                r = convert_to_memory(x)
                res += r
            return res
        elif isinstance(value, int):
            return [value]
        else:
            return [ord(value)]


def convert_from_memory(ty: str, value: Memory) -> Any:
    def helper(ty: str, value: int) -> Any:
        if ty == "char":
            return chr(value)
        else:
            return value

    unionSpl = ty.split("|")
    if len(unionSpl) == 1:
        spl = ty.split(",")
        res = list([helper(spl[i], value[i]) for i in range(len(spl))])
        if len(res) == 1:
            return res[0]
        else:
            return res
    else:
        resTy = unionSpl[value[0]]
        return (resTy, convert_from_memory(resTy, value[1:]))


# use of global variables to pass information between functions is not allowed,
# however, you may update 'type_info' in 'initializeMemory' if that would help you!
def initializeMemory(type_info: Dict[str, str]) -> Memory:
    totalSpace = 0
    for (k, ty) in type_info.items():
        type_info[k] = (totalSpace, ty)
        totalSpace += get_size(ty)
    return [0] * totalSpace  # initialize the memory to something as small as possible


def writeToMemory(
    type_info: Dict[str, str], values: Dict[str, Any], mem: Memory
) -> Memory:
    # use of global variables is not allowed!
    for (k, v) in values.items():
        memory = convert_to_memory(v)

        if isinstance(v, tuple):
            pos = type_info[k][1].split("|").index(v[0])
            memory = [pos] + memory

        offset = type_info[k][0]
        for (i, memSpot) in enumerate(memory):
            mem[offset + i] = memSpot
    return mem  # this will pass the write-to-memory test, but reconstructing the original values will fail, since we're not storing anything


def readFromMemory(type_info: Dict[str, str], mem: Memory) -> Dict[str, Any]:
    res = {}
    for (k, data) in type_info.items():
        (offset, ty) = data
        value = mem[offset : offset + get_size(ty) + 1]
        retrieved = convert_from_memory(ty, value)
        res[k] = retrieved
    return res


def checkValidMemory(memory: Memory) -> Memory:
    # Example on how I'll check if your memory is valid
    ok = isinstance(memory, list)
    if not ok:
        raise (RuntimeError("Invalid memory, not a list: {}".format(memory)))
    for i in memory:
        ok = isinstance(i, int)
        if not ok:
            raise (RuntimeError("Invalid memory, element not an int: {}".format(i)))
        ok = i >= 0 and i < pow(2, 32)
        if not ok:
            raise (
                RuntimeError("Invalid memory, integer element not a word: {}".format(i))
            )


def runSingleTest(type_info, data_list, constantSize):
    mem = initializeMemory(type_info)
    checkValidMemory(mem)
    size = len(mem)
    allData = {}
    for data in data_list:
        mem = writeToMemory(type_info.copy(), data.copy(), mem.copy())
        checkValidMemory(mem)
        if constantSize and size != len(mem):
            raise (RuntimeError("Size of memory changed after initialisation"))
        allData.update(data)
        memData = readFromMemory(type_info, mem)
        for k, v in allData.items():
            if not (type(memData[k]) == type(v) and memData[k] == v):
                raise (
                    RuntimeError("The value of {} wasn't retrieved properly".format(k))
                )
    return True


def testSet():
    """This is code to test your functions."""
    # Example 1a: plain int values, write once
    type_info = {"x": "int", "y": "int", "z": "int"}
    data = {"x": 3, "y": 200, "z": 50}
    runSingleTest(type_info, [data], True)
    # Example 1b: plain int values, incremental updates
    type_info = {"x": "int", "y": "int", "z": "int"}
    runSingleTest(type_info, [{"x": 3, "z": 50}, {"y": 200, "z": 50}, {"z": 20}], True)
    # Example 1a: plain char values, write once
    type_info = {"x": "char", "y": "char", "z": "char"}
    data = {"x": "3", "y": "a", "z": "x"}
    runSingleTest(type_info, [data], True)

    # Example 2: product types
    type_info = {"x": "int,int", "y": "int", "z": "int,int,int"}
    data = {"x": [3, 4], "y": 200, "z": [50, 60, 70]}
    runSingleTest(type_info, [data], True)

    # Example 3: sum types (tagged)
    type_info = {"x": "int|char", "b": "int", "z": "int|char"}
    data = {"x": ("int", 3), "b": 200, "z": ("char", "d")}
    runSingleTest(type_info, [data], True)

    # Example 4: combining sum and product types
    type_info = {"x": "int|char,int", "z": "int|char,int"}
    data = {"x": ("int", 3), "z": ("char,int", ["/", 60])}
    runSingleTest(type_info, [data], True)

    # No dynamically sized types

    print("Ran all tests!")


testSet()
