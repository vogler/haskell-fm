http://users.jyu.fi/~sapekiis/haskell-pitfalls/

# REPL
+ no need for `;;` is nice
- OTOH how to input multiple lines
  - `:{`, `:}`
  - `:set +m`
    - put in `~/.ghc/ghci.conf`
- per default just shows the value
  - `:set +t` to also show type
    - useless if value has no Show instance b/c then it aborts before showing the type... why?
    - with `:set -XFlexibleInstances` `instance Show a where show _ = "??"`, but this makes printing fail for Num

# types
- strange that types and their constructors are both capital. Everything constant is captial, variable stuff lowercase?
- `type ℙ a = Set.Set a` ok, strange syntax, seems to be equiv. to `Set (Set a)`? Ah, is the `.` function composition (curried constructors)?

# modules
- why the need for `module YourModule where`?
+ directories automatically used for namespacing
  - complicated in OCaml (x.mlpack), duplicate file names lead to name clash

# type classes
~~~
class Foo a where
  f :: a -> String

instance Show a => Foo a where
  f x = show x ++ "!"
~~~
seems like a basic thing one might want to do, but needs some flags to work:

~~~
:set -XFlexibleInstances
:set -XUndecidableInstances
~~~
