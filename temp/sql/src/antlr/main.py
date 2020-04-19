# import ipdb
import sys
from antlr4 import *
from PlSqlLexer import PlSqlLexer
from PlSqlParser import PlSqlParser

def main(argv):
    input = InputStream(argv[1])
    lexer = PlSqlLexer(input)
    stream = CommonTokenStream(lexer)
    parser = PlSqlParser(stream)
    walker = ParseTreeWalker()
    walker.walk(self.listener, parser.compilationUnit())
#    tree = parser.sql_script()
#    ipdb.set_trace()
#    print(tree)

if __name__ == '__main__':
    main(sys.argv)
