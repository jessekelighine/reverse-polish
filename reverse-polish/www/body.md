
---

#### Usage

Enter an expression in Reverse Polish Notation (RPN) to see its result and corresponding infix notation.
Make sure to separate each operand and operator with a space. Scroll down for usage examples.

🤔 **Not familiar with RPN?**
Check out the [Wikipedia article](https://en.wikipedia.org/wiki/Reverse_Polish_notation)
or watch the excellent explainer by [Computerphile](https://www.youtube.com/watch?v=7ha78yWRDlE).

#### Available Operators

There are two types of operators: *mathematical operators* and *stack operators*.
Here are the *mathematical operators*:

- `+`    **ADD**:         *add* the last two numbers on stack.
- `-`    **SUBTRACT**:    *subtract* the last two numbers on stack.
- `*`    **MULTIPLY**:    *multiply* the last two numbers on stack.
- `/`    **DIVIDE**:      *divide* the last two numbers on stack.
- `\`    **DIVIDE**:      *divide* the last two numbers on stack, but the other way around.
- `^`    **POWER**:       raise the second-to-last number to the *power* of the last number on stack.
- `!`    **FACTORIAL**:   calculate the *factorial* of the last number on stack.
- `~`    **NEGATE**:      *negate* the last number on stack.
- `sqrt` **SQUARE ROOT**: calculate the *square root* of the last number on stack.
- `abs`  **ABSOLUTE VALUE**: calculate the *absolute value* of the last number on stack.
- `log` **LOGARITHM**: calculate the *natural logarithm* of the last number on stack.

And here are the *stack operators* that manipulate the stack:

- `pop` **POP**: *pop*/remove the last number off the stack.
- `dup` **DUPLICATE**: *duplicate* the last number on stack.
- `swap` **SWAP**: *swap* the positions of the last two number on stack.
- `roll` **ROLL**: syntax `n i roll` *rolls* the top $n$ elements on the stack around by a shift up of $i$ elements,
  so `2 1 roll` is equivalent to `swap`.
  Naturally, $n$ and $i$ must be (or will be coerced into) integers, and $n$ must be non-negative.
  See example below.

#### Available Operands

You can use numbers or letters `a` to `z` as operands.
If letters are used,
a symbolic infix expression will be calculated,
and the numeric output will be `NA`.

#### Examples

💡 **Tip**: Copy any example and paste it into the input box to test it!

| RPN Input                             | Output (Stack)  | Infix Notation                   |
| ----                                  | ----            | ----                             |
| `2 3 + 4 -`                           | `1`             | `2 + 3 - 4`                      |
| `1 2 3 - - ~`                         | `-2`            | `-(1 - (2 - 3))`                 |
| `27 1 3 / ^`                          | `3`             | `27^(1/3)`                       |
| `4 1 - 2 ^ 8 4 - 2 ^ + sqrt`          | `5`             | `sqrt((4 - 1)^2 + (8 - 4)^2)`    |
| `-b b 2 ^ 4 a c * * - sqrt + 2 a * /` | `NA`            | `(-b + sqrt(b^2 - 4·a·c))/(2·a)` |
| `a b c d e 5 2 roll`                  | `d, e, a, b, c` |                                  |
| `a b c d e 5 -1 roll`                 | `b, c, d, e, a` |                                  |

---

Created by Jesse C. Chen ([jessekelighine.com](https://jessekelighine.com))
<br>
<br>
