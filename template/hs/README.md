# Useful Built-ins

```hs
-- flip ($)
(&) :: a -> (a -> b) -> b
-- on (+) f x y = f x + f y
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- liftA2 (+) f g x = f x + g x
liftA2 :: (a -> b -> c) -> (r -> a) -> (r -> b) -> r -> c
```

# Monads

> A [monad](https://en.wikipedia.org/wiki/Monad_(category_theory)) is a [monoid](https://en.wikipedia.org/wiki/Monoid_(category_theory)) in the [category](https://en.wikipedia.org/wiki/Category_(mathematics)) of [endofunctors](https://en.wikipedia.org/wiki/Category_(mathematics)); every [Monad](https://wiki.haskell.org/Typeclassopedia#Monad) is an [Applicative](https://wiki.haskell.org/Typeclassopedia#Applicative), thus also a [Functor](https://wiki.haskell.org/Typeclassopedia#Functor)

Since all the type constructors here are Monads, `ap` and `(<*>)` are interchangeable

## Reader
```
((->) r)
```

### LiftA2

Any function in this form is equivalent to `liftA2`

```hs
pointful op f g x = f x `op` g x
```

### Combinators

> see [**SKI** calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) and [**BCKW** system](https://en.wikipedia.org/wiki/B%2C_C%2C_K%2C_W_system)

| Combinator | Haskell Equivalent |
| - | - |
| **S** x y z = x z (y z) | `ap :: (r -> a -> b) -> (r -> a) -> r -> b` |
| **K** x y = x | `const :: a -> b -> a` |
| **I** x = x | `id :: a -> a` |
| **B** x y z = x (y z) | `(.) :: (b -> c) -> (a -> b) -> a -> c` |
| **C** x y z = x z y | `flip :: (a -> b -> c) -> b -> a -> c` |
| **W** x y = x y y | `join :: (r -> r -> a) -> r -> a` |
