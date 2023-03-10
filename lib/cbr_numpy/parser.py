import ast
import re

def parse(p):
    tree = ast.parse(p)
    parser = IRParser()
    new_tree = parser.visit(tree)
    return new_tree
    
class UnsupportedNodeException(Exception):
    pass

class UnsupportedFeatureException(Exception):
    pass

class IRParser(ast.NodeVisitor):

    def getClassName(self,node):
        pattern = "<class 'ast\.([\w]+)'>"
        match = re.search(pattern, str(node.__class__))
        if match:
            name = match.group(1)
            return name
        else:
            return str(node.__class__)

    def visit_Import(self,node):
        pass

    def visit_FunctionDef(self,node):
        name = node.name
        body = SList([self.visit(stmt) for stmt in node.body])
        args = SList([SAtom(arg.arg) for arg in node.args.args])
        return SList([SAtom(name),args,body])

    def visit_Assign(self,node):
        if len(node.targets) > 1:
            raise UnsupportedFeatureException("Multiple assignments")
        return SList([SAtom("Assign"),self.visit(node.targets[0]),self.visit(node.value)])

    def visit_Attribute(self,node):
        match node.value:
            case ast.Name(id=name):
                return SAtom(f"{name}_{node.attr}")
            case other:
                raise UnspportedFeatureException("Attribute")

    def visit_Name(self,node):
        return SAtom(node.id)

    def visit_Mult(self,node):
        return SAtom("*")

    def visit_Add(self,node):
        return SAtom("+")

    def visit_Sub(self,node):
        return SAtom("-")

    def visit_Subscript(self,node):
        return SList([SAtom("Index"),self.visit(node.value),self.visit(node.slice)])

    def visit_AugAssign(self,node):
        target = self.visit(node.target)
        return SList([SAtom("Assign"),target,SList([SAtom("Call"),self.visit(node.op),target,self.visit(node.value)])])

    def visit_For(self,node):
        if node.orelse:
            raise UnsupportedFeatureException("For-else statement")
        return SList([SAtom("For"),self.visit(node.target),self.visit(node.iter),SList([self.visit(stmt) for stmt in node.body])])

    def visit_Module(self,node):
        block = []
        env = []
        for stmt in node.body:
            child = self.visit(stmt)
            if child:
                match self.getClassName(stmt):
                    case "FunctionDef":
                        env.append(child) 
                    case other:
                        block.append(child)
        return SList([SList(env),SList(block)])

    def visit_Constant(self,node):
        return SList([SAtom("Num"), SAtom(str(node.n))])

    def visit_Call(self,node):
        return SList([SAtom("Call"),self.visit(node.func)] + [self.visit(arg) for arg in node.args])

    def visit_Operator(self,node):
        return SAtom(self.getClassName(node))

    def visit_BinOp(self,node):
        return SList([SAtom("Call"),self.visit(node.op),self.visit(node.left),self.visit(node.right)])

    def visit_Arg(self,node):
        print(node.arg)
        return SAtom(node.arg)

    def visit_Return(self,node):
        return SList([SAtom("Return"),self.visit(node.value)])

    def generic_visit(self,node):
        name = self.getClassName(node)
        raise UnsupportedNodeException(name)


class SList():
    def __init__(self, l):
        self.l = l

    def __str__(self):
        return "(" + ' '.join([str(c) for c in self.l]) + ")"

class SAtom():
    def __init__(self, a):
        self.a = a

    def __str__(self):
        return str(self.a)

