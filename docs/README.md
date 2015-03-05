# Module Documentation

## Module Control.Parallel

#### `Parallel`

``` purescript
newtype Parallel eff a
```

The `Parallel` type constructor wraps the `ContT` type constructor
and provides type class instances for parallel composition of
computations:

- The definition of `(<*>)` from `Apply` runs two computations in parallel and applies
  a function when both complete.
- The definition of `(<|>)` from the `Alt` type class runs two computations in parallel
  and returns the result of the computation which completes first.

Parallel sections of code can be embedded in sequential code by using
the `inParallel` and `runParallel` functions:

```purescript
loadModel :: ContT Unit (Eff (ajax :: AJAX)) Model
loadModel = do
  token <- authenticate
  runParallel $
    Model <$> inParallel (get "/products/popular/" token)
          <*> inParallel (get "/categories/all" token)
```

#### `inParallel`

``` purescript
inParallel :: forall eff a. ContT Unit (Eff eff) a -> Parallel eff a
```

Create a computation to be run in parallel from a computation in the
continuation monad.


#### `runParallel`

``` purescript
runParallel :: forall eff a. Parallel eff a -> ContT Unit (Eff eff) a
```

Unwrap a parallel computation so that it may be embedded in sequential code,
or run using `runContT`.

#### `functorParallel`

``` purescript
instance functorParallel :: Functor (Parallel eff)
```


#### `applyParallel`

``` purescript
instance applyParallel :: Apply (Parallel eff)
```


#### `applicativeParallel`

``` purescript
instance applicativeParallel :: Applicative (Parallel eff)
```


#### `altParallel`

``` purescript
instance altParallel :: Alt (Parallel eff)
```


#### `plusParallel`

``` purescript
instance plusParallel :: Plus (Parallel eff)
```


#### `alternativeParallel`

``` purescript
instance alternativeParallel :: Alternative (Parallel eff)
```