import ast
import re

python_regex = re.compile(
    r"(\w++) *+= *+(0|np\.zeros|np\.empty).*+\nfor .++\n( ++.*+\n)++return \1$"
)


class NoExtractionException(Exception):
    pass


def elm_json(block):
    """Tries to extract a synthesis input from an Elm function/variable
    definition; assumes that block.get('tag') == 'Definition'. Returns a JSON
    object."""
    if block["expression"]["tag"] == "CaseExpression":
        if block["expression"]["subject"]["tag"] == "VariableReference":
            v = block["expression"]["subject"]["name"]
            for param in block["parameters"]:
                type = param["type"]
                pat = param["pattern"]
                if pat.get("name") == v:
                    if type and type["tag"] == "TypeReference":
                        if (
                            type["name"] == "Maybe"
                            or type["name"] == "Result"
                            or type["name"] == "List"
                        ):
                            return block
            raise NoExtractionException("scrutinee not of correct type")
        else:
            raise NoExtractionException("scrutinee is not parameter")
    else:
        raise NoExtractionException("body is not case expression")


def python(tree):
    """Tries to extract a synthesis input from a Python script, assuming that
    it is represented as a Python AST object. Returns a pair of Python AST
    objects (env, body)."""

    if isinstance(tree, ast.Module) and len(tree.body) > 0:
        if isinstance(tree.body[-1], ast.Expr):
            tree.body[-1] = ast.Return(value=tree.body[-1].value)
        else:
            raise NoExtractionException("final line is not variable")
    else:
        raise NoExtractionException("empty")

    classifier = VarClassifier()
    classifier.visit(tree)
    if not classifier.found_for:
        raise NoExtractionException("for loop not found")
    input_vars = classifier.input_vars
    output_vars = classifier.output_vars
    extractor = Extractor(input_vars, output_vars)
    extractor.visit(tree)
    env_ast = extractor.env_ast
    body_ast = extractor.body_ast
    body_text = ast.unparse(body_ast)
    if not "np." in body_text:
        raise NoExtractionException("does not contain np.")
    if "pd." in body_text:
        raise NoExtractionException("contains pd.")
    if "{}" in body_text:
        raise NoExtractionException("contains {}")
    if not python_regex.match(body_text):
        raise NoExtractionException("does not pass python_regex")
    return env_ast, body_ast, tree.body[-1].value.id


# Python helper code


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
            raise NoExtractionException("multiple assignment targets in VarClassifier")
        var = self.visit(node.targets[0])
        if self.in_for:
            if var in self.input_vars:
                self.input_vars.remove(var)
            self.output_vars.add(var)
        else:
            self.input_vars.add(var)

    def visit_AugAssign(self, node: ast.AugAssign):
        var = self.visit(node.target)
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

    def visit_Name(self, node):
        return node.id

    def visit_Subscript(self, node: ast.Subscript):
        return self.visit(node.value)

    def visit_Assign(self, node: ast.Assign):
        if len(node.targets) > 1:
            raise NoExtractionException(
                "multiple assignment targets in Extractor.visit_Assign"
            )
        var = self.visit(node.targets[0])
        if var in self.input_vars:
            self.env_stmts.append(node)
        else:
            self.body_stmts.append(node)

    def visit_For(self, node: ast.For):
        self.body_stmts.append(node)

    def visit_AugAssign(self, node: ast.AugAssign):
        var = self.visit(node.target)
        if var in self.input_vars:
            self.env_stmts.append(node)
        else:
            self.body_stmts.append(node)

    def visit_Module(self, node):
        self.num_modules += 1
        for stmt in node.body:
            self.visit(stmt)
        if self.num_modules > 1:
            raise NoExtractionException("Multiple modules in Extractor.visit_Module")
        self.num_modules = 0
        self.env_ast = ast.Module(self.env_stmts, [])
        self.body_ast = ast.Module(self.body_stmts, [])

    def generic_visit(self, node):
        self.env_stmts.append(node)

    def visit_Return(self, node):
        self.body_stmts.append(node)
