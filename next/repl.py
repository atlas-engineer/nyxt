import cmd
from interpreter import Interpreter
instance = Interpreter()


class nLisp(cmd.Cmd):
    def __init__(self):
        cmd.Cmd.__init__(self)
        self.prompt = "nlisp> "
  
    def default(self, line):
        instance.parse(line)


if __name__ == '__main__':
        nl = nLisp()
        nl.cmdloop()
