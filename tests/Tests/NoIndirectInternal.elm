module Tests.NoIndirectInternal exposing (all)

import NoIndirectInternal exposing (rule)
import Review.Test
import Support.String exposing (normalizeIndent)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoIndirectInternal"
        [ accepts
        , rejects
        ]


accepts : Test
accepts =
    describe "accepts module importing"
        [ test "nothing" <|
            \_ ->
                """
                module A exposing (..)
                _ = ()
                """
                    |> normalizeIndent
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "non-internal in another folder" <|
            \_ ->
                """
                    module A exposing (..)
                    import B
                    _ = ()
                    """
                    |> normalizeIndent
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , describe "in the same folder"
            [ test "non-internal" <|
                \_ ->
                    """
                    module A exposing (..)
                    import A.B
                    _ = ()
                    """
                        |> normalizeIndent
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            , test "internal" <|
                \_ ->
                    """
                    module A exposing (..)
                    import A.Internal
                    _ = ()
                    """
                        |> normalizeIndent
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            , test "submodule of internal" <|
                \_ ->
                    """
                    module A exposing (..)
                    import A.Internal.B
                    _ = ()
                    """
                        |> normalizeIndent
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            , test "nested submodule of internal" <|
                \_ ->
                    """
                    module A exposing (..)
                    import A.Internal.B.C
                    _ = ()
                    """
                        |> normalizeIndent
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            ]
        ]


rejects : Test
rejects =
    describe "rejects module importing"
        [ rejectsFromAnotherFolder
        , rejectsFromSubFolder
        ]


rejectsFromAnotherFolder : Test
rejectsFromAnotherFolder =
    describe "from another folder"
        [ test "internal" <|
            \_ ->
                """
                module A exposing (..)
                import OtherA.Internal
                _ = ()
                """
                    |> normalizeIndent
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error "OtherA.Internal" ]
        , describe "in internal"
            [ test "submodule" <|
                \_ ->
                    """
                    module A exposing (..)
                    import OtherA.Internal.B
                    _ = ()
                    """
                        |> normalizeIndent
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error "OtherA.Internal.B" ]
            , test "nested submodule" <|
                \_ ->
                    """
                    module A exposing (..)
                    import OtherA.Internal.B.C
                    _ = ()
                    """
                        |> normalizeIndent
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error "OtherA.Internal.B.C" ]
            ]
        ]


rejectsFromSubFolder : Test
rejectsFromSubFolder =
    describe "in sub folder"
        [ test "internal of internal" <|
            \_ ->
                """
                module A exposing (..)
                import A.Internal.Internal
                _ = ()
                """
                    |> normalizeIndent
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error "A.Internal.Internal" ]
        , describe "internal submodule"
            [ test "internal" <|
                \_ ->
                    """
                    module A exposing (..)
                    import A.Internal.B.Internal
                    _ = ()
                    """
                        |> normalizeIndent
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error "A.Internal.B.Internal" ]
            , test "submodule of internal" <|
                \_ ->
                    """
                    module A exposing (..)
                    import A.Internal.B.Internal.C
                    _ = ()
                    """
                        |> normalizeIndent
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error "A.Internal.B.Internal.C" ]
            , test "internal of nested submodule" <|
                \_ ->
                    """
                    module A exposing (..)
                    import A.Internal.B.C.Internal
                    _ = ()
                    """
                        |> normalizeIndent
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error "A.Internal.B.C.Internal" ]
            ]
        ]


error : String -> Review.Test.ExpectedError
error moduleName =
    let
        info =
            { message = "Indirect import to internal module `" ++ moduleName ++ "`"
            , details =
                [ "By convention, Elm modules in `Internal` namespaces are private."
                , "Do not import modules inside an `Internal` namespace if they are not in a namespace directly above them."
                ]
            , under = "import " ++ moduleName
            }
    in
    info
        |> Review.Test.error
        |> Review.Test.whenFixed fixedModule


fixedModule : String
fixedModule =
    normalizeIndent
        """
        module A exposing (..)

        _ = ()
        """
