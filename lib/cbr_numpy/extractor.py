import ast


def extract(p):
    tree = ast.parse(p)
    classifier = VarClassifier()
    classifier.visit(tree)
    input_vars = classifier.input_vars
    output_vars = classifier.output_vars
    extractor = Extractor(input_vars, output_vars)
    extractor.visit(tree)
    env_ast = extractor.env_ast
    body_ast = extractor.body_ast
    return env_ast, body_ast


class UnsupportedNodeException(Exception):
    pass


class VarClassifier(ast.NodeVisitor):
    def __init__(self) -> None:
        self.input_vars = set()
        self.output_vars = set()
        self.in_for = False
        self.found_for = False
        super().__init__()

    def visit_Name(self, node):
        return node.id

    def visit_Subscript(self, node: ast.Subscript):
        return self.visit(node.value)

    def visit_Assign(self, node: ast.Assign):
        if len(node.targets) > 1:
            raise UnsupportedNodeException("Multiple targets")
        var = self.visit(node.targets[0])
        if self.in_for:
            if var in self.input_vars:
                self.input_vars.remove(var)
            self.output_vars.add(var)
        else:
            self.input_vars.add(var)

    def visit_AugAssign(self, node: ast.AugAssign):
        if len(node.targets) > 1:
            raise UnsupportedNodeException("Multiple targets")
        var = node.targets[0].id
        if self.in_for:
            if var in self.input_vars:
                self.output_vars.add(var)
        else:
            self.input_vars.add(var)

    def visit_For(self, node: ast.For):
        inner_loop = self.in_for
        self.in_for = True
        self.found_for = True
        for stmt in node.body:
            self.visit(stmt)
        if not inner_loop:
            self.in_for = False


class Extractor(ast.NodeVisitor):
    def __init__(self, input_vars, output_vars) -> None:
        self.input_vars = input_vars
        self.output_vars = output_vars
        self.env_stmts = []
        self.body_stmts = []
        self.num_modules = 0

    def visit_Assign(self, node: ast.Assign):
        if len(node.targets) > 1:
            raise UnsupportedNodeException
        var = node.targets[0].id
        if var in self.input_vars:
            self.env_stmts.append(node)
        else:
            self.body_stmts.append(node)

    def visit_For(self, node: ast.For):
        self.body_stmts.append(node)

    def visit_AugAssign(self, node: ast.AugAssign):
        if len(node.targets) > 1:
            raise UnsupportedNodeException
        var = node.targets[0].id
        if var in self.input_vars:
            self.env_stmts.append(node)
        else:
            self.body_stmts.append(node)

    def visit_Module(self, node):
        self.num_modules += 1
        for stmt in node.body:
            self.visit(stmt)
        if self.num_modules > 1:
            raise UnsupportedNodeException("Multiple modules")
        self.num_modules = 0
        self.env_ast = ast.Module(self.env_stmts, [])
        self.body_ast = ast.Module(self.body_stmts, [])

    def generic_visit(self, node):
        self.env_stmts.append(node)

    def visit_Return(self, node):
        self.body_stmts.append(node)
