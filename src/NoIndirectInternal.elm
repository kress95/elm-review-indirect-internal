module NoIndirectInternal exposing (rule)

{-| Blocks import to indirect `Internal` modules.


# Rules

@docs rule

-}

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


type alias ModuleName =
    List String


type alias Errors =
    List (Error {})


{-| The `elm-review` rule.
-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoImportingIndirectInternal" []
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : Node Module -> ModuleName -> ( Errors, ModuleName )
moduleDefinitionVisitor node _ =
    ( [], Module.moduleName <| Node.value node )


importVisitor : Node Import -> ModuleName -> ( Errors, ModuleName )
importVisitor node moduleName =
    if noIndirectInternal moduleName <| importModuleName node then
        ( [], moduleName )

    else
        ( error node, moduleName )


importModuleName : Node Import -> List String
importModuleName =
    Node.value >> .moduleName >> Node.value


noIndirectInternal : List String -> List String -> Bool
noIndirectInternal moduleName importName =
    case dropModuleName moduleName importName of
        _ :: tail ->
            noInternal tail

        _ ->
            True


dropModuleName : List String -> List String -> List String
dropModuleName moduleName importName =
    case moduleName of
        moduleHead :: moduleTail ->
            case importName of
                importHead :: importTail ->
                    if moduleHead == importHead then
                        dropModuleName moduleTail importTail

                    else
                        importName

                [] ->
                    []

        [] ->
            importName


noInternal : List String -> Bool
noInternal importName =
    case importName of
        head :: tail ->
            if head == "Internal" then
                False

            else
                noInternal tail

        [] ->
            True


error : Node Import -> Errors
error node =
    let
        moduleName =
            String.join "." <| importModuleName node

        message =
            { message =
                "Indirect import to internal module `" ++ moduleName ++ "`"
            , details =
                [ "By convention, Elm modules in `Internal` namespaces are private."
                , "Do not import modules inside an `Internal` namespace if they are not in a namespace directly above them."
                ]
            }

        errorRange =
            Node.range node

        fixes =
            [ Fix.removeRange errorRange ]
    in
    [ Rule.errorWithFix message errorRange fixes ]
