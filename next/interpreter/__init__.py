"""
 s_expression = atomic / "(" s_expression "."s_expression ")" / list
 list = "(" s_expression < s_expression > ")"
 letter = "a" / "b" / " ..." / "z"
 number = "1" / "2" / " ..." / "9"
 empty = " "
"""

from parsimonious.grammar import Grammar


class Interpreter(object):
    """(+ 1 (* a b))
    
    """
    def __init__(self):
        self.grammar = Grammar(
            """
            EXPRESSION    = ATOM / LIST
            LIST          = LPAREN EXPRESSION* RPAREN
            ATOM          = SYMBOL / NUMBER / EMPTY
            SYMBOL        = ~r"[A-Za-z]+"
            NUMBER        = ~r"[0-9]+"
            LPAREN        = "("
            RPAREN        = ")"
            EMPTY         = " "
            """)
    
    def parse(self, expression):
        print(self.grammar.parse(expression))
    
    def evaluate(self, expression):
        print('eval')
