
```hs
liftA2 op f g x = f x `op` g x
on op f x y = f x `op` f y
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
