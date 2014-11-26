# Module Documentation

## Module Control.Parallel

### Types

    newtype Parallel eff a where
      Parallel :: ContT Unit (Eff eff) a -> Parallel eff a


### Type Class Instances

    instance altParallel :: Alt (Parallel eff)

    instance alternativeParallel :: Alternative (Parallel eff)

    instance applicativeParallel :: Applicative (Parallel eff)

    instance applyParallel :: Apply (Parallel eff)

    instance functorParallel :: Functor (Parallel eff)

    instance plusParallel :: Plus (Parallel eff)


### Values

    runParallel :: forall eff a. Parallel eff a -> ContT Unit (Eff eff) a