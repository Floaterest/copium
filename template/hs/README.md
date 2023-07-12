# Monads

> a [monoid](https://en.wikipedia.org/wiki/Monoid_(category_theory)) in the [category](https://en.wikipedia.org/wiki/Category_(mathematics)) of [endofunctors](https://en.wikipedia.org/wiki/Category_(mathematics))

Every [Monad](https://wiki.haskell.org/Typeclassopedia#Monad) is an Applicative, and every [Applicative](https://wiki.haskell.org/Typeclassopedia#Applicative) is a [Functor](https://wiki.haskell.org/Typeclassopedia#Functor).

## Reader
```
((->) r)
```

### LiftA2

any function in the form

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

- **S** is equivalent to `ap` because `((->) r)` is an Applicative
- **W** is equivalent to `join` because `((->) r)` is a Monad

```hs
ap :: (r -> a -> b) -> (r -> a) -> r -> b -- S
join :: (r -> r -> a) -> r -> a           -- W
```