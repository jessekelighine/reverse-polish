
---

#### Usage

Input a command in reverse-polish notation (RPN) and see the output and infix notation representation!

**Note**: Separate the operators/operands with spaces. You can find examples at the end of this page. 

#### Available Operators

- `+` **ADD**:         *add* the last two numbers on stack.
- `-` **SUBTRACT**:    *subtract* the last two numbers on stack.
- `*` **MULTIPLY**:    *multiply* the last two numbers on stack.
- `/` **DIVIDE**:      *divide* the last two numbers of stack.
- `^` **POWER**:       raise the second-to-last number to the *power* of the last number on stack.
- `v` **SQUARE ROOT**: calculate the *square root* of the last number on stack.
- `~` **NEGATE**:      shorthand for `-1 *`, i.e., *negate* the last number on stack.
- `!` **FACTORIAL**:   calculate the *factorial* of the last number on stack.
- `|` **SWAP**:        *swap* the positions of the last two number on stack.
- `=` **POP**:         *pop* or remove the last number on stack.

#### Examples

The following are some examples:
first in RPN,
then in infix notation,
then the output.

**Tip**: Copy the RPN in the input and verify that the output is correct!

| RPN                        | infix                          | output  |
|----------------------------|--------------------------------|---------|
| `2 0.5 ^`                  | `2^0.5`                        | `1.414` |
| `2 3 + 4 -`                | `2 + 3 - 4`                    | `1`     |
| `1 2 + 3 ^ ~`              | `-((1 + 2)^3)`                 | `-27`   |
| `4 1 - 2 ^ 8 4 - 2 ^ + v`  | `sqrt((4 - 1)^2 + (8 - 4)^2)`  | `5`     |

---

Author: Jesse C. Chen ([jessekelighine.com](https://jessekelighine.com))
<br>
<br>
