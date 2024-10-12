# Maize

Maize is a specialized programming language, designed for writing descriptive, relevant, and consistent code. 

It aims to provide minimal resistance in converting ideas to code, while also being internally consistent and, as a whole, making sense as a language.

## TODO
- Read expressions repeatedly until EOF (to finish reading a line after a root left-associative operator)
- Add parentheses to the AST to handle operator precedence correctly
- Implement significant whitespace (stolen from Haskell and Python)
- (maybe) Allow using infix expressions like (/ 2) ≡ (\x x / 2).
  - This would allow less error space (`Error::UnexpectedInfix`) but at the cost of either (1) potential name collision while defining the lambda or (2) a more complicated AST

## Contributing

Feel free to open a PR or an issue.

Before starting large PRs, make sure to check with a maintainer.

## License

GNU GPLv3