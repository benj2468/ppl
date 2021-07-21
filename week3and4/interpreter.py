from __future__ import annotations
import ast
import sys
from copy import deepcopy
import os
from typing import Dict, List

PRINT_COMMAND = "PRINT"
UPDATE_COMMAND = "UPDATE"
CLEAR_ACTION = "CLEAR"
CHANGE_ACTION = "CHANGE"


def interpreter(program, toBeInterpreted):
    """ interpreter(programString,toBeInterpreted)
        Runs the ast given by 'toBeInterpreted'.
        The programString is used to construct readable error messages.
        If the program runs successfully, programString is not used. """

    output = []

    class Scope:
        def __init__(self, lines):
            self.memory = {}

            self.functions: Dict[str, Function] = {}
            for line in lines:
                if isinstance(line, ast.FunctionDef):
                    if line.name in self.functions:
                        exception(
                            "Cannot instantiate two functions with the same name in the same scope",
                            line)
                    self.functions[line.name] = Function(line)

            self.lines = lines

        def with_memory_from(self, scope):
            self.memory = scope.memory
            return self

        def with_functions_from(self, scope):
            self.functions = {**self.functions, **scope.functions}
            return self

        def call_function(self, func, args):
            if func in self.functions:
                self.functions[func].call(self, args)
            else:
                raise RuntimeError(f"Function not in scope: {func}")

        def get_from_storage(self, key, args, restrictions={}) -> List[Dict]:
            restrictions = [restrictions[var] if var in restrictions else None for var in args]
            res = []
            if not key in self.memory: return []

            values = list(self.memory[key])
            values.sort()
            for value in values:
                canAdd = True
                for (i, restr) in enumerate(restrictions):
                    if restr and value[i] != restr:
                        canAdd = False
                if canAdd:
                    # res.append({var1: arg1, var2: arg2})
                    res.append({
                        arg:value for (arg, value) in zip(args, value)
                    })
            return res

        def store_in_memory(self, key, args) -> None:
            if not key in self.memory:
                self.memory[key] = set()
            self.memory[key].add(tuple(args))

        def clear_memory(self, key) -> None:
            del self.memory[key]

        def remove_from_memory(self, key, entry) -> None:
            self.memory[key].remove(entry)

        def update_memory(self, diff):
            [start, end] = diff
            self.remove_from_memory(start[0], start[1:])
            self.store_in_memory(end[0], end[1:])

        def run(self) -> None:
            program_counter = 0

            while program_counter < len(self.lines):
                line = self.lines[program_counter]
                if isinstance(line, ast.Expr):
                    try:
                        command, *args = parseAtom(line.value)
                        if command == PRINT_COMMAND:
                            printA(*args)
                        elif command == UPDATE_COMMAND:
                            self.update_memory(args)
                        else:
                            self.store_in_memory(command, args)
                    except:
                        [command, name] = self.parseAction(line.value)

                        if command == CLEAR_ACTION:
                            self.clear_memory(name)
                elif isinstance(line, ast.Assign):
                    command, *args = parseAtom(line.targets[0])
                    values = self.getExpressionValue(line.value)
                    if command == PRINT_COMMAND:
                        for value in values:
                            printA(*extractValues(value, *args))
                    if command == UPDATE_COMMAND:
                        exception(
                            "Cannot perform update comand dynamically yet")
                    else:
                        for value in values:
                            self.store_in_memory(
                                command, extractValues(value, *args))
                elif isinstance(line, ast.While):
                    Loop(self, line).run()
                elif isinstance(line, ast.FunctionDef):
                    pass
                elif isinstance(line, ast.If):
                    If(self, line).run()
                else:
                    exception(
                        "Unsupported syntax line. Only Expressions and Assignments permitted",
                        line)

                program_counter += 1

        # Some parsing helper functions:
        # parseAtom parses things like foo[1,"Bla"]
        # parseArgument parses the 1 and "Bla" within that.
        # These functions do some rudamentary checks to see if the syntax is okay
        def parseAction(self, atom):
            if isinstance(atom, ast.Subscript) and isinstance(
                    atom.value, ast.Name):
                name = atom.value.id
                if isinstance(atom.slice.value, ast.Name):
                    return (name, atom.slice.value.id)
                elif isinstance(atom.slice.value, ast.Tuple):
                    expresion = self.getExpressionValue(
                        atom.slice.value.elts[0])
                    iterations = parseAtom(atom.slice.value.elts[1])
                    return (name, (expresion, iterations))
                else:
                    exception("Expecting an identifier", atom)
            elif isinstance(atom, ast.Call):
                self.call_function(atom.func.id, atom.args)
                return (None, None)
            else:
                exception("Not a valid action, expecting action[name] syntax.",
                          atom)

        def getExpressionValue(self, expr):
            ret = {}
            if isinstance(expr, ast.Subscript) and isinstance(expr.value, ast.Name)\
                and isinstance(expr.slice.value,ast.Tuple):
                nm = expr.value.id
                if nm == 'SOME':
                    joinOn = parseArgument(expr.slice.value.elts[0])
                    values = self.getExpressionValue(expr.slice.value.elts[1])
                    return [joinPair(joinOn, pair) for pair in values]
                elif nm == 'WHERE':
                    name, *args = parseAtom(expr.slice.value.elts[0])
                    restriction = self.getExpressionValue(
                        expr.slice.value.elts[1])
                    pairs = self.get_from_storage(name, args, restriction)
                    return pairs
                else:
                    args = [
                        parseArgument(arg) for arg in expr.slice.value.elts
                    ]
                    pairs = self.get_from_storage(nm, args)
                    return [pair for pair in pairs if pair != None]
            elif isinstance(expr, ast.Subscript) and isinstance(expr.value, ast.Name)\
                and isinstance(expr.slice.value,ast.Constant):
                nm = expr.value.id
                args = [
                    parseArgument(expr.slice.value)
                ]
                pairs = self.get_from_storage(nm, args)
                return [pair for pair in pairs if pair != None]
            elif isinstance(expr, ast.BoolOp):
                ret['args'] = [
                    self.getExpressionValue(arg) for arg in expr.values
                ]
                if (isinstance(expr.op, ast.And)):
                    return crossProduct(ret['args'])
                if (isinstance(expr.op, ast.Or)):
                    return [v for vs in ret['args'] for v in vs]
                else:
                    exception(f"Unknown operator: {expr.op}", expr)
            elif isinstance(expr, ast.Compare):
                if isinstance(expr.ops[0], ast.Is):
                    variable = expr.left.value
                    value = expr.comparators[0].value
                    restriction = {variable: value}
                    return restriction
                else:
                    exception("Can only interpret `is` comparators", expr)
            elif isinstance(expr, ast.BinOp):
                right = self.getExpressionValue(expr.right)
                if isinstance(expr.op, ast.Sub):
                    result = list(
                        item for item in self.getExpressionValue(expr.left)
                        if not item in right)
                    return result
            elif isinstance(expr, ast.UnaryOp):
                if isinstance(expr.op, ast.Not):
                    return not self.getExpressionValue(expr.operand)
            else:
                exception("Unexpected expression", expr)

    class Function:
        def __init__(self, line: ast.FunctionDef) -> None:
            self.name = line.name
            self.arguments = list(a.arg for a in line.args.args)
            self.lines = line.body

        def call(self, super_scope, args):
            if len(args) != len(self.arguments):
                raise RuntimeError(
                    f"Incompatible number of arguments to function: {self.name}"
                )
            scope = Scope(self.lines).with_functions_from(super_scope)
            for (arg, renamed) in zip(args, self.arguments):
                if isinstance(arg, ast.Name):
                    key = arg.id
                    scope.memory[renamed] = super_scope.memory[
                        key] if key in super_scope.memory else set()
            scope.run()
            for (arg, renamed) in zip(args, self.arguments):
                if isinstance(arg, ast.Name):
                    key = arg.id
                    if not key in super_scope.memory:
                        super_scope.memory[key] = set()
                    super_scope.memory[key] = scope.memory[renamed]

    class Loop:
        def __init__(self, scope: Scope, line):
            self.super = scope
            self.condition = line.test
            self.scope = Scope(line.body).with_memory_from(
                self.super).with_functions_from(self.super)

        def run(self):
            while self.should_loop():
                self.scope.run()
            self.super.memory = self.scope.memory

        def should_loop(self):
            try:
                command, key = self.super.parseAction(self.condition)
                if command == CHANGE_ACTION:
                    try:
                        changed = len(
                            self.state.difference(
                                self.scope.memory[key])) or len(
                                    self.scope.memory[key].difference(
                                        self.state))
                        self.state = self.scope.memory[key].copy()
                        return changed
                    except:
                        self.state = self.scope.memory[key].copy()
                        return True
            except:
                value = self.scope.getExpressionValue(self.condition)
                return len(value)
            return False

    class If:
        def __init__(self, scope: Scope, line: ast.If) -> None:
            self.super = scope
            self.scope = Scope(line.body).with_memory_from(
                self.super).with_functions_from(self.super)
            self.condition = line.test
            self.options = line.orelse

        def run(self):
            if self.scope.getExpressionValue(self.condition):
                self.scope.run()
            else:
                scope = Scope(self.options)\
                    .with_memory_from(self.scope)\
                    .with_functions_from(self.scope)
                scope.run()
            self.super.memory = self.scope.memory

    # program is the original source code (used for error messages),
    # toBeInterpreted is the parsed structure that we'll work on.

    # The 'exception' is to be used if the .py0 program has an error:
    def exception(message, structure):
        """ Display an error message on some ast structure """
        if (debug):
            print(f'Exception ({message}) raised on:\n{structure}')
        original = ast.get_source_segment(program, structure, padded=True)
        if (structure.lineno == structure.end_lineno):
            if (structure.col_offset + 2 >= structure.end_col_offset):
                colpos = f"{structure.col_offset}"
            else:
                colpos = f"{structure.col_offset} - {structure.end_col_offset}"
            posInfo = f"On line {structure.lineno}:{colpos}"
        else:
            posInfo = f"On lines {structure.lineno}:{structure.col_offset} to {structure.end_lineno}:{structure.end_col_offset}"
        if original == None:
            raise (RuntimeError(
                f"{message}:\n{ast.unparse(structure)}\n{posInfo}"))
        else:
            raise (RuntimeError(f"{message}:\n{original}\n{posInfo}"))

    def parseAtom(atom):
        if isinstance(atom, ast.Subscript) and isinstance(
                atom.value, ast.Name):
            name = atom.value.id
            if isinstance(atom.slice.value, ast.Tuple):
                args = [
                    parseArgument(argument)
                    for argument in atom.slice.value.elts
                ]
                return name, *args
            elif isinstance(atom.slice.value, ast.Constant):
                args = [parseArgument(atom.slice.value)]
                return name, *args
            else:
                exception("Expecting two arguments to every atom", atom)
        else:
            exception("Not a valid atom, expecting name[arg1,arg2] syntax.",
                      atom)

    def parseArgument(argument):
        """ Convert simple arguments such as 1 or "Hello" in foo[1,"Hello"]
            into plain-old python values. If the ast passed is not a simple value,
            then an error is raised.

            We don't check what the resulting type is,
            so a constant like False could be returned by this function as well. """
        if isinstance(argument, ast.Constant):
            return argument.value
        elif isinstance(argument, ast.Name):
            # The following might become acceptable at some point, so let's give it a different looking exception
            # exception(f"Expecting to see quotes around {ast.Name.id}")
            return argument.id
        elif isinstance(argument, ast.Subscript):
            return parseAtom(argument)
        else:
            exception(
                "Not a valid argument to an atom, this must be a simple python constant",
                argument)

    # The main workhorse here is the code to compute the value of an expression
    # That function is getExpressionValue
    def joinPair(v, p):
        del p[v]
        return p

    def crossProduct(lst):
        if (debug): print(f'crossproduct {lst}')
        res = lst.pop()
        for pairs in lst:
            res = crossProduct1(res, pairs)
        return res

    def mergeKeys(x,
                  y):  # This is 'x | y' in python 3.9+, but I'm on 3.8.8 ~SJ
        z = x.copy()
        z.update(y)
        return z

    def crossProduct1(
        a, b
    ):  # assuming all elements in a have the same keys, and all elements in b do too
        """ Combine two lists of dictionaries on common keys. """
        if len(a) == 0 or len(b) == 0: return []
        ksa = a[0].keys()
        ksb = b[0].keys()
        overlap = sorted(set([ka for ka in ksa for kb in ksb if ka == kb]))
        return [
            mergeKeys(v1, v2) for v1 in a for v2 in b
            if all([(v1[k] == v2[k]) for k in overlap])
        ]

    # TODO: This is where the code to your interpreter goes.
    # Note that you already have a function 'getExpressionValue' built for you
    # The only caveat is that it relies on a yet-to-be-defined function getFromStorage.

    def printA(*args):
        val = args[0]
        for arg in args[1:]:
            val += f" # {arg}" if arg else ''
        output.append(val)

    def extractValues(value, *args):
        try:
            return [value[arg] for arg in args]
        except:
            raise (
                RuntimeError(f"Scoping error: {args} not in scope."))

    lines = toBeInterpreted.body

    Scope(lines).run()

    # Printing the collected output
    output = "\n".join(output)
    print(output)

    return output


# Run the interpreter if we are using this from the command line
# The next line checks if we are running this from the command line (and not loading this file as a module)
if __name__ == "__main__":
    dump = False
    debug = False  # TODO: Change the default setting for 'debug' if you'd like.
    test = False
    # command_line_arguments = sys.argv;
    if len(sys.argv) == 1 or sys.argv[1] == "-h" or sys.argv[
            1] == "-help" or sys.argv[1] == "--help":
        # if no arguments are given or help is requested, display some help
        print(f"Usage: python3 {sys.argv[0]} inputfile.py0")
        print(
            f"  To see the parsed abstract-syntax-tree and exit: python3 {sys.argv[0]} -dump inputfile.py0"
        )
        print(
            f"  Toggle debug mode (default: {debug}): python3 {sys.argv[0]} -debug inputfile.py0"
        )
        print("  (argument order matters)")
    else:
        # interpret the files that are given as command-line arguments
        for filename in sys.argv[1:]:
            if filename == "-dump":
                dump = True
                continue
            if filename == "-debug":
                debug = True
                continue
            if filename == "-test":
                test = True
                continue
            # next line may crash if the file is not found, we should probably give a friendlier error message than the default
            data = open(filename).read()
            tree = ast.parse(
                data,
                filename=filename)  # parse the file into a nice data-structure
            if dump:
                print(ast.dump(tree))
                break
            # now we run our interpreter on the ast
            output = interpreter(data, tree)

            if test:
                file_name = filename[:len(filename) - 4] + "_output.txt"
                with open(file_name, "r") as file:
                    previous_contents = file.read()
                    if previous_contents != output:
                        print(file_name)
                        print('output', output)
                        print('prev', previous_contents)
                        raise RuntimeError("Unexpected change in output")
