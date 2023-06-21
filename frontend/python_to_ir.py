import ast
import re

# S-expression datatype


class SList:
    """S-expression for lists"""

    def __init__(self, l):
        self.l = l

    def __str__(self):
        return "(" + " ".join([str(c) for c in self.l]) + ")"


class SAtom:
    """S-expression for atoms"""

    def __init__(self, a):
        self.a = a

    def __str__(self):
        return str(self.a)


# Main functions


def parse_str(s: str) -> SList:
    """Parses Python in a string to the s-expression IR"""
    return parse(ast.parse(s))


def parse(p: ast.AST) -> SList:
    """Parses a Python AST object to the s-expression IR"""
    parser = IRParser()
    sexp = parser.visit(p)
    return sexp


# Helper code


class UnsupportedFeatureException(Exception):
    pass


class IRParser(ast.NodeVisitor):
    def getClassName(self, node):
        pattern = "<class '_?ast\\.([\\w]+)'>"
        match = re.search(pattern, str(node.__class__))
        if match:
            name = match.group(1)
            return name
        else:
            return str(node.__class__)

    def visit_Import(self, node):
        pass

    def visit_FunctionDef(self, node):
        name = node.name
        body = SList([self.visit(stmt) for stmt in node.body])
        args = SList([SAtom(arg.arg) for arg in node.args.args])
        return SList([SAtom(name), args, body])

    def visit_Assign(self, node):
        if len(node.targets) > 1:
            raise UnsupportedFeatureException("Multiple assignments")
        return SList(
            [SAtom("Assign"), self.visit(node.targets[0]), self.visit(node.value)]
        )

    def visit_Attribute(self, node):
        name = ""
        while True:
            className = self.getClassName(node)
            if className == "Attribute":
                name = node.attr + "." + name
                node = node.value
            elif className == "Name":
                name = node.id + "." + name
                break
            else:
                raise UnsupportedFeatureException(
                    f"Unsupported attribute feature '{className}' in {ast.dump(node)}"
                )
        return SAtom(name[:-1])

    def visit_Name(self, node):
        return SAtom(node.id)

    def visit_Mult(self, node):
        return SAtom("*")

    def visit_Add(self, node):
        return SAtom("+")

    def visit_Sub(self, node):
        return SAtom("-")

    def visit_Div(self, node):
        return SAtom("/")

    def visit_Compare(self, node: ast.Compare):
        if type(node.ops[0]) == ast.Gt:
            op = SAtom(">")
        elif type(node.ops[0]) == ast.Lt:
            op = SAtom("<")
        elif type(node.ops[0]) == ast.LtE:
            op = SAtom("<=")
        elif type(node.ops[0]) == ast.GtE:
            op = SAtom(">=")
        elif type(node.ops[0]) == ast.Eq:
            op = SAtom("==")
        elif type(node.ops[0]) == ast.NotEq:
            op = SAtom("!=")
        else:
            op = node.ops[0].__class__.__name__
        return SList(
            [SAtom("Call"), op, self.visit(node.left), self.visit(node.comparators[0])]
        )

    def visit_UnaryOp(self, node: ast.UnaryOp):
        if type(node.op) == ast.USub:
            expr = self.visit(node.operand)
            if (
                type(expr) == SList
                and len(expr.l) > 1
                and type(expr.l[0]) == SAtom
                and expr.l[0].a == "Num"
            ):
                num = int(expr.l[1].a)
                expr.l[1].a = f"{-1*num}"
                return expr
            else:
                return SList([SAtom("Call"), SAtom("-"), self.visit("0"), expr])
        if type(node.op) == ast.Not:
            op = SAtom("not")
        return SList([SAtom("Call"), op, self.visit(node.operand)])

    def visit_Subscript(self, node):
        return SList([SAtom("Index"), self.visit(node.value), self.visit(node.slice)])

    def visit_AugAssign(self, node):
        target = self.visit(node.target)
        return SList(
            [
                SAtom("Assign"),
                target,
                SList(
                    [SAtom("Call"), self.visit(node.op), target, self.visit(node.value)]
                ),
            ]
        )

    def visit_Index(self, node):
        return self.visit(node.value)

    def visit_For(self, node):
        if node.orelse:
            raise UnsupportedFeatureException("For-else statement")
        return SList(
            [
                SAtom("For"),
                self.visit(node.target),
                self.visit(node.iter),
                SList([self.visit(stmt) for stmt in node.body]),
            ]
        )

    def visit_If(self, node):
        cond = self.visit(node.test)
        then = SList([self.visit(stmt) for stmt in node.body])
        orelse = SList([self.visit(stmt) for stmt in node.orelse])
        return SList([SAtom("If"), cond, then, orelse])

    def visit_Module(self, node):
        block = []
        env = []
        for stmt in node.body:
            child = self.visit(stmt)
            if child:
                if self.getClassName(stmt) == "FunctionDef":
                    env.append(child)
                else:
                    block.append(child)
        return SList([SList(env), SList(block)])

    def visit_Constant(self, node):
        return SList([SAtom("Num"), SAtom(str(node.n))])

    def visit_Num(self, node):
        return SList([SAtom("Num"), SAtom(str(node.n))])

    def visit_Call(self, node):
        return SList(
            [SAtom("Call"), self.visit(node.func)]
            + [self.visit(arg) for arg in node.args]
        )

    def visit_Operator(self, node):
        return SAtom(self.getClassName(node))

    def visit_BinOp(self, node):
        return SList(
            [
                SAtom("Call"),
                self.visit(node.op),
                self.visit(node.left),
                self.visit(node.right),
            ]
        )

    def visit_Arg(self, node):
        print(node.arg)
        return SAtom(node.arg)

    def visit_Return(self, node):
        return SList([SAtom("Return"), self.visit(node.value)])

    def visit_List(self, node):
        if not node.elts:
            return SAtom("__emptyList")
        else:
            raise UnsupportedFeatureException("Non-empty list literal")

    def visit_Expr(self, node):
        if (
            isinstance(node.value, ast.Call)
            and isinstance(node.value.func, ast.Attribute)
            and node.value.func.attr == "append"
        ):
            if len(node.value.args) != 1:
                raise UnsupportedFeatureException("Incorrectly called append")
            var = self.visit(node.value.func.value)
            new_val = self.visit(node.value.args[0])
            return SList(
                [
                    SAtom("Assign"),
                    var,
                    SList([SAtom("Call"), SAtom("__immutableAppend"), var, new_val]),
                ]
            )
        else:
            raise UnsupportedFeatureException("Non-append side effect")

    def generic_visit(self, node):
        name = self.getClassName(node)
        print(ast.dump(node))
        raise UnsupportedFeatureException(name)
