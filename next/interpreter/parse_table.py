import interpreter.yacc as yacc
from interpreter.token_rules import tokens  # noqa: F401

# s_expression = atomic_symbol / "(" s_expression "."s_expression ")" / list
# list = "(" s_expression < s_expression > ")"
# atomic_symbol = letter atom_part
# atom_part = empty / letter atom_part / number atom_part
# letter = "a" / "b" / " ..." / "z"
# number = "1" / "2" / " ..." / "9"
# empty = " "


def p_sexpression_atom(p):
    '''sexpression : atom
                   | LPAREN sexpression RPAREN'''
    p[0] = p[1]
    for index, element in enumerate(p):
        print(index, element)


def p_atom_number(p):
    'atom : NUMBER'
    p[0] = p[1]


def p_atom_empty(p):
    'atom :'
    pass


def p_error(p):
    print("Syntax error! ", p)


yacc.yacc()
