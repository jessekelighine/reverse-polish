
---

#### Usage

Enter an expression in Reverse Polish Notation (RPN) to see its result and corresponding infix notation.
Make sure to separate each operand and operator with a space. Scroll down for usage examples.

🤔 **Not familiar with RPN?**
Check out the [Wikipedia article](https://en.wikipedia.org/wiki/Reverse_Polish_notation)
or watch the excellent explainer by [Computerphile](https://www.youtube.com/watch?v=7ha78yWRDlE).

#### Available Operators

- `+`    **ADD**:         *add* the last two numbers on stack.
- `-`    **SUBTRACT**:    *subtract* the last two numbers on stack.
- `*`    **MULTIPLY**:    *multiply* the last two numbers on stack.
- `/`    **DIVIDE**:      *divide* the last two numbers on stack.
- `\`    **DIVIDE**:      *divide* the last two numbers on stack, but the other way around.
- `^`    **POWER**:       raise the second-to-last number to the *power* of the last number on stack.
- `!`    **FACTORIAL**:   calculate the *factorial* of the last number on stack.
- `~`    **NEGATE**:      *negate* the last number on stack.
- `sqrt` **SQUARE ROOT**: calculate the *square root* of the last number on stack.
- `|`    **SWAP**:        *swap* the positions of the last two number on stack.

#### Available Operands

You can use numbers or letters `a` to `z` as operands.
If letters are used,
a symbolic infix expression will be calculated,
and the numeric output will be `NA`.

#### Examples

💡 **Tip**: Copy any example and paste it into the input box to test it!

| RPN Input                             | Infix Notation                   | Output (Stack) |
| ----                                  | ----                             | ----           |
| `2 3 + 4 -`                           | `2 + 3 - 4`                      | `1`            |
| `1 2 3 - - ~`                         | `-(1 - (2 - 3))`                 | `-2`           |
| `27 1 3 / ^`                          | `27^(1/3)`                       | `3`            |
| `4 1 - 2 ^ 8 4 - 2 ^ + sqrt`          | `sqrt((4 - 1)^2 + (8 - 4)^2)`    | `5`            |
| `-b b 2 ^ 4 a c * * - sqrt + 2 a * /` | `(-b + sqrt(b^2 - 4·a·c))/(2·a)` | `NA`           |

---

Created by Jesse C. Chen ([jessekelighine.com](https://jessekelighine.com))
<br>
<br>
