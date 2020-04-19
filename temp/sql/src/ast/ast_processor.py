from antlr4 import FileStream, CommonTokenStream, ParseTreeWalker
from ast.PlSqlLexer import PlSqlLexer
from ast.PlSqlParser import PlSqlParser
from pprint import pformat


class AstProcessor:

    def __init__(self, logging, listener):
        self.logging = logging
        self.logger = logging.getLogger(self.__class__.__name__)
        self.listener = listener

    # ★ポイント２
    def execute(self, input_source):
        parser = PlSqlParser(CommonTokenStream(PlSqlLexer(FileStream(input_source, encoding="utf-8"))))
        walker = ParseTreeWalker()
#        walker.walk(self.listener, parser.compilationUnit())
        walker.walk(self.listener, parser.sql_script())
        self.logger.debug('Display all data extracted by AST. \n' + pformat(self.listener.ast_info, width=160))
        return self.listener.ast_info
