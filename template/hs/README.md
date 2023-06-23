# Monad

## Partially Applied Function

> `(e ->)` is a [Reader Monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Reader.html)

any function in the form
```hs
pointful op f g x = (f x) `op` (g x)
```

is functionally equivalent to `liftM2`

## S Combinator

> in [SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus), (**S** x y z) = xz(yz)

**S** is equivalent to `ap :: Monad m => m (b -> c) -> m b -> m c` with `m` as the reader monad `(a ->)`