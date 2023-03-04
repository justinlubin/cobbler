import ast
import re

def parse(p):
    tree = ast.parse(p)
    parser = IRParser()
    new_tree = parser.visit(tree)
    return new_tree
    
class IRParser(ast.NodeVisitor):

    def getClassName(self,node):
        pattern = "<class '_ast\.([\w]+)'>"
        match = re.search(pattern, str(node.__class__))
        name = match.group(1)
        return name

    def visit_Num(self,node):
        return SList([SAtom("Num"), SAtom(str(node.n))])

    def visit_Operator(self,node):
        return SAtom(self.getClassName(node))

    def visit_BinOp(self,node):
        return SList([SAtom("BinOp"),self.visit(node.op),self.visit(node.left),self.visit(node.right)])

    def generic_visit(self,node):
        children = []
        for child in ast.iter_child_nodes(node):
            children.append(self.visit(child))
        return SList([SAtom(self.getClassName(node))] + children)


class SList():
    def __init__(self, l):
        self.l = l

    def __str__(self):
        return "(" + ' '.join([str(c) for c in self.l]) + ")"

class SAtom():
    def __init__(self, a):
        self.a = a

    def __str__(self):
        return self.a

print(parse("3+5"))
