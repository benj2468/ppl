import ast
import sys
import os

def interpreter(program,toBeInterpreted):
    """ interpreter(programString,toBeInterpreted)
        Runs the ast given by 'toBeInterpreted'.
        The programString is used to construct readable error messages.
        If the program runs successfully, programString is not used. """
    # program is the original source code (used for error messages),
    # toBeInterpreted is the parsed structure that we'll work on.

    # The 'exception' is to be used if the .py0 program has an error:
    def exception(message, structure):
        """ Display an error message on some ast structure """
        if(debug):
            printf(f'Exception ({message}) raised on:\n{structure}')
        original = ast.get_source_segment(program, structure, padded=True)
        if (structure.lineno == structure.end_lineno):
            if (structure.col_offset+2 >= structure.end_col_offset):
                colpos = f"{structure.col_offset}"
            else:
                colpos = f"{structure.col_offset} - {structure.end_col_offset}"
            posInfo = f"On line {structure.lineno}:{colpos}"
        else:
            posInfo = f"On lines {structure.lineno}:{structure.col_offset} to {structure.end_lineno}:{structure.end_col_offset}"
        if original == None :
            raise(RuntimeError(f"{message}:\n{ast.unparse(structure)}\n{posInfo}"))
        else :
            raise(RuntimeError(f"{message}:\n{original}\n{posInfo}"))
    
    # Some parsing helper functions:
    # parseAtom parses things like foo[1,"Bla"]
    # parseArgument parses the 1 and "Bla" within that.
    # These functions do some rudamentary checks to see if the syntax is okay
    def parseAtom(atom):
        if isinstance(atom, ast.Subscript) and isinstance(atom.value, ast.Name):
            name = atom.value.id
            if isinstance(atom.slice.value, ast.Tuple):
                args=[parseArgument(argument) for argument in atom.slice.value.elts]
                return name, args
            else:
                exception("Expecting two arguments to every atom", atom)
        else:
            exception("Not a valid atom, expecting name[arg1,arg2] syntax.", atom)
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
            exception(f"Expecting to see quotes around {ast.Name.id}")
        else:
            exception("Not a valid argument to an atom, this must be a simple python constant",argument)
    
    # The main workhorse here is the code to compute the value of an expression
    # That function is getExpressionValue
    def joinPair(v,p):
        del p[v]
        return p
    def crossProduct(lst):
        # if(debug): print(f'crossproduct {lst}')
        res = lst.pop()
        for pairs in lst:
            res = crossProduct1(res,pairs)
        return res
    def mergeKeys(x,y): # This is 'x | y' in python 3.9+, but I'm on 3.8.8 ~SJ
        z = x.copy()
        z.update(y)
        return z
    def crossProduct1(a,b): # assuming all elements in a have the same keys, and all elements in b do too
        """ Combine two lists of dictionaries on common keys. """
        if len(a)==0 or len(b)==0: return []
        ksa = a[0].keys()
        ksb = b[0].keys()
        overlap = sorted(set([ka for ka in ksa for kb in ksb if ka==kb]))
        return [mergeKeys(v1, v2) for v1 in a for v2 in b if all([(v1[k]==v2[k]) for k in overlap])]
    def getExpressionValue(expr):
        ret = {}
        if isinstance(expr, ast.Subscript) and isinstance(expr.value, ast.Name)\
            and isinstance(expr.slice.value,ast.Tuple):
            nm = expr.value.id
            if nm=='SOME':
                joinOn = parseArgument(expr.slice.value.elts[0])
                values = getExpressionValue(expr.slice.value.elts[1])
                return [joinPair(joinOn,pair) for pair in values]
            else:
                args = [parseArgument(arg) for arg in expr.slice.value.elts]
                pairs = getFromStorage(nm,args)
                return [pair for pair in pairs if pair != None]
        elif isinstance(expr, ast.BoolOp):
            ret['args']=[getExpressionValue(arg) for arg in expr.values]
            if(isinstance(expr.op,ast.And)):
                return crossProduct(ret['args'])
            if(isinstance(expr.op,ast.Or)):
                return [v for vs in ret['args'] for v in vs]
            else:
                exception(f"Unknown operator: {expr.op}",expr)
        else:
            exception("Unexpected expression",expr)
    
    # TODO: This is where the code to your interpreter goes.
    # Note that you already have a function 'getExpressionValue' built for you
    # The only caveat is that it relies on a yet-to-be-defined function getFromStorage.
    setupMemory()
    iterateOverCode()

# Run the interpreter if we are using this from the command line
# The next line checks if we are running this from the command line (and not loading this file as a module)
if __name__ == "__main__":
    dump = False
    debug = False # TODO: Change the default setting for 'debug' if you'd like.
    # command_line_arguments = sys.argv;
    if len(sys.argv) == 1 or sys.argv[1]=="-h" or sys.argv[1]=="-help" or sys.argv[1]=="--help":
        # if no arguments are given or help is requested, display some help
        print(f"Usage: python3 {sys.argv[0]} inputfile.py0")
        print(f"  To see the parsed abstract-syntax-tree and exit: python3 {sys.argv[0]} -dump inputfile.py0")
        print(f"  Toggle debug mode (default: {debug}): python3 {sys.argv[0]} -debug inputfile.py0")
        print("  (argument order matters)")
    else:
        # interpret the files that are given as command-line arguments
        for filename in sys.argv[1:]:
            if filename=="-dump":
                dump = True
                continue 
            if filename=="-debug":
                debug = not debug
                continue 
            # next line may crash if the file is not found, we should probably give a friendlier error message than the default
            data = open(filename).read()
            tree = ast.parse(data, filename=filename) # parse the file into a nice data-structure
            if dump:
                print(ast.dump(tree))
                break
            # now we run our interpreter on the ast
            interpreter(data,tree)