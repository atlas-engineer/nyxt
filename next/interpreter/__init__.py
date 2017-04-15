from parsimonious.grammar import Grammar


class Interpreter(object):
    """
    
    """
    def __init__(self):
        self.grammar = Grammar(
            """
            bold_text  = bold_open text bold_close
            text       = ~"[A-Z 0-9]*"i
            bold_open  = "(("
            bold_close = "))"
            """)
    
    def parse(self, expression):
        print(self.grammar.parse(expression))

    def evaluate(self, expression):
        print('eval')
