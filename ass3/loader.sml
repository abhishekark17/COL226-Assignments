CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "AST.sml";
use "evaluator.sml";
use "ass2.yacc.sig";
use "ass2.yacc.sml";
use "ass2.lex.sml";
use "load-ass2.sml";
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)
