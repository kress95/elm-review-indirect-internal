# elm-review-indirect-internal

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to block indirect imports to `Internal` modules in your Elm project.

Useful for projects relying in git submodules.

## Example configuration

```elm
module ReviewConfig exposing (config)

import NoIndirectInternal
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoIndirectInternal.rule
    ]
```

## Behavior

### Accepted code

Internal module at the same folder:

```elm
module A exposing (..)
import A.Internal
```

Submodule of internal at the same folder:

```elm
module A exposing (..)
import A.Internal.B
```

Nested submodule of internal at the same folder:

```elm
module A exposing (..)
import A.Internal.B.C
```

### Rejected code

Internal module at another folder:

```elm
module A exposing (..)
import B.Internal
```

Submodule of internal at another folder:

```elm
module A exposing (..)
import B.Internal.C
```

Nested submodule of internal at another folder:

```elm
module A exposing (..)
import B.Internal.C.D
```

Any nested internal module:

```elm
module A exposing (..)
import A.Internal.Internal
```

Any nested internal submodule:

```elm
module A exposing (..)
import A.Internal.B.Internal
```

## Thanks

Thanks to @jfmengels for [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) and its excellent documentation.
