module Support.String exposing (normalizeIndent)


normalizeIndent : String -> String
normalizeIndent =
    String.lines
        >> dropFirstEmptyLine
        >> normalizeLinesIndent
        >> String.join "\n"


dropFirstEmptyLine : List String -> List String
dropFirstEmptyLine lines =
    case lines of
        head :: tail ->
            if isEmptyAfterTrim head then
                tail

            else
                lines

        [] ->
            lines


isEmptyAfterTrim : String -> Bool
isEmptyAfterTrim =
    String.trim >> String.isEmpty


normalizeLinesIndent : List String -> List String
normalizeLinesIndent lines =
    let
        length =
            case List.head lines of
                Just head ->
                    let
                        original =
                            String.length head

                        trimmed =
                            String.length <| String.trimLeft head
                    in
                    original - trimmed

                Nothing ->
                    0
    in
    if length > 0 then
        let
            indent =
                String.repeat length " "
        in
        List.map (trimIndent length indent) lines

    else
        lines


trimIndent : Int -> String -> String -> String
trimIndent length indent string =
    if String.left length string == indent then
        String.dropLeft length string

    else
        string
