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

##### Instances
``` purescript
instance functorParallel :: Functor (Parallel eff)
instance applyParallel :: Apply (Parallel eff)
instance applicativeParallel :: Applicative (Parallel eff)
instance altParallel :: Alt (Parallel eff)
instance plusParallel :: Plus (Parallel eff)
instance alternativeParallel :: Alternative (Parallel eff)
```

#### `inParallel`

``` purescript
inParallel :: forall eff a. ContT Unit (Eff eff) a -> Parallel eff a
```

Create a computation to be run in parallel from a computation in the
continuation monad.


#### `withCallback`

``` purescript
withCallback :: forall eff a. ((a -> Eff eff Unit) -> Eff eff Unit) -> Parallel eff a
```

Create a parallel computation from an action which uses a callback.

This function is just shorthand for `inParallel <<< ContT`.

#### `runParallel`

``` purescript
runParallel :: forall eff a. Parallel eff a -> ContT Unit (Eff eff) a
```

Unwrap a parallel computation so that it may be embedded in sequential code,
or run using `runContT`.

#### `runParallelWith`

``` purescript
runParallelWith :: forall eff a. (a -> Eff eff Unit) -> Parallel eff a -> Eff eff Unit
```

Run a parallel computation by providing a callback

This function is just shorthand for `runContT` composed with `runParallel`.


