There are lots of GH repos named hscheme. This is the original HScheme from https://hscheme.sourceforge.net/. Unfortunately, it does not currently compile.

# HScheme
HScheme is a Scheme interpreter written in Haskell. It's intended to be R5RS-compliant. Features include:
- The full numeric tower, with exact and inexact types, complex numbers, rationals etc. as per R5RS. Actually, the numeric type is complex where the real and imaginary components can be separately exact or inexact.
- A macro system as per R5RS.
- Proper tail recursion.
- Control functions such as call-with-current-continuation and dynamic-wind as per R5RS.
- A fixed-point function, call-with-result, which allows you to call a function with the an argument that contains the result of the function (also, letrec has been extended in the same way). For instance, to create circular lists: `(call-with-result (lambda (rest) (cons 'hello rest)))` This is particularly useful if you're restricted to purely functional procedures.
- The ability to specify the execution monad. The standard one is called "GCPS", which stands for "guarded continuation-passing style". Other monads may be faster (or not), but disallow certain functions. The "identity" monad disallows anything imperative (set! and friends, IO interaction, etc.), but does allow a certain degree of examination of fixed-point arguments.
- The ability to specify a strictly functional subset of available standard procedures.

There's an online interpreter, and also some examples. See the issues list for the current status.

HScheme is written by Ashley Yakeley.
