import interpreter.yacc as yacc
from interpreter.token_rules import tokens  # noqa: F401
from interpreter import Interpreter

# s_expression = atomic_symbol / "(" s_expression "."s_expression ")" / list
# list = "(" s_expression < s_expression > ")"
# atomic_symbol = letter atom_part
# atom_part = empty / letter atom_part / number atom_part
# letter = "a" / "b" / " ..." / "z"
# number = "1" / "2" / " ..." / "9"
# empty = " "


def p_list(p):
    'list : LPAREN items RPAREN'
    p[0] = p[2]


def p_items(p):
    'items : item items'
    p[0] = [p[1]] + p[2]


def p_items_empty(p):
    'items : empty'
    p[0] = []


def p_empty(p):
    'empty :'
    pass


def p_item_atom(p):
    'item : atom'
    p[0] = p[1]


def p_item_list(p):
    'item : list'
    p[0] = p[1]


def p_item_call(p):
    'item : call'
    p[0] = p[1]


def p_item_empty(p):
    'item : empty'
    p[0] = p[1]


def p_call(p):
    'call : LPAREN SYMBOL items RPAREN'
    pass


def p_atom_symbol(p):
    'atom : SYMBOL'
    p[0] = p[1]


def p_atom_number(p):
    'atom : NUMBER'
    p[0] = p[1]


def p_atom_empty(p):
    'atom :'
    pass


def p_error(p):
    print("Syntax error! ", p)


yacc.yacc()
