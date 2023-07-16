# Monads

> A [monad](https://en.wikipedia.org/wiki/Monad_(category_theory)) is a [monoid](https://en.wikipedia.org/wiki/Monoid_(category_theory)) in the [category](https://en.wikipedia.org/wiki/Category_(mathematics)) of [endofunctors](https://en.wikipedia.org/wiki/Category_(mathematics)); every [Monad](https://wiki.haskell.org/Typeclassopedia#Monad) is an [Applicative](https://wiki.haskell.org/Typeclassopedia#Applicative), thus also a [Functor](https://wiki.haskell.org/Typeclassopedia#Functor)

Since all the type constructors here are Monads, `ap` and `(<*>)` are interchangeable

## Reader
```
((->) r)
```

### LiftA2

Any function in the form

```hs
pointful op f g x = f x `op` g x
```

is  equivalent to `liftA2` (which is [pointfree](https://wiki.haskell.org/Pointfree))

### Combinators

> see [**SKI** calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) and [**BCKW** system](https://en.wikipedia.org/wiki/B%2C_C%2C_K%2C_W_system)

| Combinator | Pointfree Haskell Equivalent |
| - | - |
| **S** x y z = x z (y z) | `ap` |
| **K** x y = x | `const` |
| **I** x = x | `id` |
| **B** x y z = x (y z) | `(.)` |
| **C** x y z = x z y | `flip` |
| **W** x y = x y y | `join` |

```hs
ap :: (r -> a -> b) -> (r -> a) -> r -> b -- S
join :: (r -> r -> a) -> r -> a           -- W
```