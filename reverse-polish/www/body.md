
---

#### Usage

Input a command in reverse-polish notation (RPN) and see the output and infix notation representation!

**Note**: Separate the operators/operands with spaces. You can find examples at the end of this page. 

#### Available Operators

- `+`    **ADD**:         *add* the last two numbers on stack.
- `-`    **SUBTRACT**:    *subtract* the last two numbers on stack.
- `*`    **MULTIPLY**:    *multiply* the last two numbers on stack.
- `/`    **DIVIDE**:      *divide* the last two numbers on stack.
- `^`    **POWER**:       raise the second-to-last number to the *power* of the last number on stack.
- `!`    **FACTORIAL**:   calculate the *factorial* of the last number on stack.
- `~`    **NEGATE**:      *negate* the last number on stack.
- `sqrt` **SQUARE ROOT**: calculate the *square root* of the last number on stack.
- `|`    **SWAP**:        *swap* the positions of the last two number on stack.

#### Available Operands

- Apart from numbers, you can use the single alphabets, i.e., `a` to `z`, as operands.
- If alphabets are used as operands, then a *symbolic* **Infix Notation** is calculated,
  and the *numeric* **Output (Stack)** will be `NA`.

#### Examples

The following are some examples:
first in RPN,
then in infix notation,
then the output.

**Tip**: Copy the RPN in the input and verify that the output is correct!

| RPN                                    | **Infix Notation**             | **Output (Stack)** |
|----------------------------------------|--------------------------------|--------------------|
| `2 3 + 4 -`                            | `2 + 3 - 4`                    | `1`                |
| `1 2 3 - - ~`                          | `-(1 - (2 - 3))`               | `-2`               |
| `27 1 3 / ^`                           | `27^(1/3)`                     | `3`                |
| `4 1 - 2 ^ 8 4 - 2 ^ + sqrt`           | `sqrt((4 - 1)^2 + (8 - 4)^2)`  | `5`                |
| `-b b 2 ^ 4 a c * * - sqrt + 2 a * /`  | `(-b + sqrt(b^2-4ac))/(2a)`    | `NA`               |

---

Author: Jesse C. Chen ([jessekelighine.com](https://jessekelighine.com))
<br>
<br>
