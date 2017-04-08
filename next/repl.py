import interpreter.lex as lex
# import interpreter.yacc as yacc


tokens = ('NAME', 'NUMBER', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
          'EQUALS', 'LPAREN', 'RPAREN')

# Tokens
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EQUALS = r'='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_NAME = r'[a-zA-Z_][a-zA-Z0-9_]*'


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


# Ignored characters
t_ignore = " \t"


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


# Build the lexer
lexer = lex.lex()


while True:
    try:
        s = input('nlisp > ')   # use input() on Python 3
    except EOFError:
        break
    lexer.input(s)
    
    # iterate through lexer tokens
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(' type {:<7} value {:<7} line {:<7} pos {:<7}'.format(
            tok.type, tok.value, tok.lineno, tok.lexpos))
