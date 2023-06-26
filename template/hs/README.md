# Monads

## Combinators

> see [**SKI** calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) and [**BCKW** system](https://en.wikipedia.org/wiki/B%2C_C%2C_K%2C_W_system)

| Combinator | Pointfree Haskell Equivalent |
| - | - |
| **S** x y z = x z (y z) | `ap` or `(<*>)` |
| **K** x y = x | `const` |
| **I** x = x | `id` |
| **B** x y z = x (y z) | `(.)` |
| **C** x y z = x z y | `flip` |
| **W** x y = x y y | `join` |

**S** and **W** uses the fact that `(e ->)` is a [Reader Monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Reader.html)

```hs
ap :: (e -> a -> b) -> (e -> a) -> e -> b -- S
join :: (e -> e -> a) -> e -> a           -- W
```

## Partially Applied Function

any function in the form

```hs
pointful op f g x = f x `op` g x
```

is  equivalent to `liftM2`
