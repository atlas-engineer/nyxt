import cmd
import interpreter.lex as lex

import interpreter.token_rules
from interpreter.parse_table import yacc


# Build the lexer
lexer = lex.lex(module=interpreter.token_rules)


class nLisp(cmd.Cmd):
    def __init__(self):
        cmd.Cmd.__init__(self)
        self.prompt = "nlisp> "
  
    def default(self, line):
        lexer.input(line)
        for token in lexer:
            print(' type {:<7} value {:<7} line {:<7} pos {:<7}'.format(
                token.type, token.value, token.lineno, token.lexpos))
            
        result = yacc.parse(line)
        print(result)


if __name__ == '__main__':
        nl = nLisp()
        nl.cmdloop()
