Follow these steps:
`ml-lex ass2.lex`
`ml-yacc ass2.yacc`
`sml loader.sml`

Open evaluator in this sml environment using `open EVALUATOR`

For lexerOutput use `lexerOutput <filename>`

For parseTree use `parseFile <filename>`

For evaluatedTree first parse the file using `parseFile <filename>` and then call `evalStatement(it,[])` where it denotes the parseTree