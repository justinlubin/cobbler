import ast
import re

python_regex = re.compile(r"(\w++) *+=.*+\nfor .++\n( ++.*+\n)++return \1$")


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
    it is represented as a Python AST object. Returns a tuple of (pre AST,
    body AST, post AST, variable name)."""

    if isinstance(tree, ast.Module) and len(tree.body) > 0:
        if isinstance(tree.body[-1], ast.Expr):
            last_return = ast.Return(value=tree.body[-1].value)
        else:
            raise NoExtractionException("final line is not variable")
    else:
        raise NoExtractionException("empty")

    classifier = VarClassifier()
    classifier.visit(tree)
    output_vars = classifier.output_vars

    extractor = Extractor(output_vars)
    extractor.visit(tree)
    pre_ast = extractor.pre_ast
    body_ast = extractor.body_ast
    post_ast = extractor.post_ast

    body_ast.body.append(last_return)

    body_text = ast.unparse(body_ast)
    if "pd." in body_text:
        raise NoExtractionException("contains pd.")
    if not python_regex.match(body_text):
        raise NoExtractionException("does not pass python_regex")

    return pre_ast, body_ast, post_ast, last_return.value.id


# Python helper code


class VarClassifier(ast.NodeVisitor):
    def __init__(self) -> None:
        self.output_vars = set()
        self.found_for = False
        self.in_output_area = False
        super().__init__()

    def visit_Name(self, node):
        return node.id

    def visit_Subscript(self, node: ast.Subscript):
        return self.visit(node.value)

    def visit_Assign(self, node: ast.Assign):
        if len(node.targets) > 1:
            raise NoExtractionException("multiple assignment targets in VarClassifier")
        var = self.visit(node.targets[0])
        if self.in_output_area:
            self.output_vars.add(var)

    def visit_AugAssign(self, node: ast.AugAssign):
        var = self.visit(node.target)
        if self.in_output_area:
            self.output_vars.add(var)

    def visit_Call(self, node):
        self.visit(node.func)
        for arg in node.args:
            self.visit(arg)
        if (
            self.in_output_area
            and isinstance(node.func, ast.Attribute)
            and node.func.attr == "append"
            and isinstance(node.func.value, ast.Name)
        ):
            self.output_vars.add(node.func.value.id)

    def visit_For(self, node: ast.For):
        if self.in_output_area:
            for stmt in node.body:
                self.visit(stmt)
        elif not self.found_for:
            self.in_output_area = True
            for stmt in node.body:
                self.visit(stmt)
            self.in_output_area = False
        self.found_for = True


class Extractor(ast.NodeVisitor):
    def __init__(self, output_vars) -> None:
        self.output_vars = output_vars
        self.stmts = {"pre": [], "body": [], "post": []}
        self.num_modules = 0
        self.state = "pre"

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
        if self.state == "pre" and var in self.output_vars:
            self.state = "body"
        self.stmts[self.state].append(node)

    def visit_For(self, node: ast.For):
        if self.state == "pre":
            self.state = "body"
        self.stmts[self.state].append(node)
        if self.state == "body":
            self.state = "post"

    def visit_Module(self, node):
        self.num_modules += 1
        if self.num_modules > 1:
            raise NoExtractionException("Multiple modules in Extractor.visit_Module")
        for stmt in node.body:
            self.visit(stmt)
        self.pre_ast = ast.Module(self.stmts["pre"], [])
        self.body_ast = ast.Module(self.stmts["body"], [])
        self.post_ast = ast.Module(self.stmts["post"], [])

    def generic_visit(self, node):
        self.stmts[self.state].append(node)
