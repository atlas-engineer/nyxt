import interpreter.lex as lex
import interpreter.token_rules

# Build the lexer
lexer = lex.lex(module=interpreter.token_rules)


while True:
    try:
        s = input('nlisp > ')
    except EOFError:
        break
    lexer.input(s)
    
    # iterate through lexer tokens
    for tok in lexer:
        print(' type {:<7} value {:<7} line {:<7} pos {:<7}'.format(
            tok.type, tok.value, tok.lineno, tok.lexpos))
