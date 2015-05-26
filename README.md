# Running and Testing

1. `git clone https://github.com/ulan/hindley-milner-exercise.git`
2. `cd hindley-milner-exercise`
3. `cabal test`
4. `cabal build`
5. `./dist/build/hindley-milner-exercise/hindley-milner-exercise < your-program-source.txt`

# The language

A term of the language is one of:
- A variable - any sequence of letters (except one of the keywords).
- A lambda application - two terms separated by a white space.
- A lambda abstraction - written as "\variable . term"
- A (non-recursive) let expression - written as "let variable = (term1) in term2", where the variable doesn't occur in term1 and paranthesis around term1 cannot be omitted.
- A fix expression - written as "fix variable . term", where the variable may occur in the term. Its semantic is given by "fix x . M --> M[x := fix x . M]" (monomorphic type inference for 'fix' is enough)
- A parenthesized expression - written as "(term)". Allows expressions like (x z) (y z).
- Integer (decimal) constant.
- Boolean constant: true | false.

Builtins:

- cmp : Number -> Number -> Bool - comparison.
- add : Number -> Number -> Number - addition.
- sub : Number -> Number -> Number - subtraction.
- mul : Number -> Number -> Number - multiplication.
- div : Number -> Number -> Number - division.
- if : forall a . Bool -> a -> a -> a - conditional.
