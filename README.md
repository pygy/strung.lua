#strong.lua

Implements the Lua string pattern matching functions in Lua + FFI, for LuaJIT.

`strong.find`, `strong.match`, and `strong.gmatch` are currently implmented according to the Lua manual.

`strong.gsub` is yet to be written.

For the null byte in patterns, `strong` suports both `"%z"`, like Lua 5.1 and "\0", like Lua 5.2. You can capture up to 200 values, if that's your thing.

### TOC

* [Performance](#performance)
* [Usage](#usage)
* [Undefined Behavior](#undefined-behavior)
* [TODO](#todo)
* [License](#license)
* [Notes](#notes)

## Performance

The standard string matching functions in LuaJIT, as of 2013-12-13, use the Lua API, and, as such, cause the compiler to abbort if they are in the way. Their `strong` counterpart can be compiled, and included in traces if they are in a hot path.

You have'll to benchmark your peculiar use case to determine if `strong` improves the global performance of your program. 

In my microbenchmarks, depending on the kind of pattern, and on some luck regarding the JIT compiler heuristics [0], matching can be up to three times faster than the original. In other circumstances, for the same pattern, it can be up to three times slower. It is often on par.

The micro-benchamrks (which double as unit tests) are there to help me ensure I'm not preventing LuaJIT from compiling the patterns, and to get a rough idea of the general performance.

`/!\`: `strong` translates patterns to Lua functions, and caches the result. As a consequence, if you generate a lot of patterns dynamically, and seldom use them, `strong` will be much, much slower than the original. On the other hand, once a pattern has been compiled, matching only depends on the target string, whereas the reference functions have to dispatch on both the pattern and the target string. This allows LuaJIT to compile the matchers optimally.

## Usage

```Lua
local strong = require"strong"

strong.find("foobar", "b(%l*)") --> 4, 6, "ar"
```

... etc. for `match` and `gmatch`

Alternativley, you can `.install()` the library, and have it replace the original functions completely.

```Lua
strong.install()

print(string.find == strong.find) --> true

S = "foo"

S:match"[^f]*" --> "oo", using `strong.match` rather than `string.match`
```

## Undefined behavior

### Bad patterns

`strong` validates the patterns before attempting a match, whereas Lua validates them on the go for example:

```Lua
string.find("ab", "b(") --> nil
string.find("ba", "b(") --> error: unfinished capture
```

`strong` will reject the pattern in both cases.

### Invalid ranges

The interaction between character classes (`%d`) and character ranges (`a-z`) inside character sets (e.g. `[%d-z]`) is documented as undefined in the Lua manual, and `strong` may hande them differently.

Specifically with `strong`: 

* When placed before the dash,
** if `%x` is a character class (e.g. `%l` for lower case letters) `[%l-k]` is a character set containing digits (`%d`), `-` and `k`.
** If %x is not a character class, the `%x` works as an escape sequence , and thus `[%%-x]` is the character range between `%` and `x`. [%0-\127] will match all ASCII characters.
* When the `%` occurs at the end of a character class, it is treated as itself. `[x-%d]` contains the character range between `x` and `%`, and the letter `d`. `[x--]` and `[x-]]` are accepted as ranges ending in `-` and `]`, respectively.

## TODO

* `%f`, the undocumented frontier pattern (easy)
* `strong.gsub` (a tad harder)

## License

MIT

## Notes

[0]: In the current benchmark suite, the order of the benchmarks influences the results. for example at some point, testing `gmatch("abcdabcdabcd", "((a)(b)c)()(d)")` alone, strong was taking 1.1 times the time of string.gmatch.

```
-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
Test:   gmatch  abcdabcdabcd    ((a)(b)c)()(d)
strong/string:  1.1065842049995
```

If you bencmarked `string.find` before `gmatch`, with the same pattern, the result was completely different.

```
-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
Test:   find    abcdabcdabcd    ((a)(b)c)()(d)
strong/string:  3.1869296949683
-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
Test:   gmatch  abcdabcdabcd    ((a)(b)c)()(d)
strong/string:  0.29895569825517
```
