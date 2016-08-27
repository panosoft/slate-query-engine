module Regex.Extra exposing (..)

import String exposing (..)
import List as L
import Regex exposing (Regex, Match, regex, HowMany(All, AtMost))


{-| Simple replacer for Regex
-}
simpleReplacer : String -> (Match -> String)
simpleReplacer new =
    \_ -> new


{-| Parametric replacer for Regex that supports $1, $2, ... $9 Parametric Replacements
    N.B. $$ is an escape for $, e.g. $$1 will be $1 in the output
    N.B. any $n that isn't specified in the regular expression is untouched in the output

    Regex.replace All (regex "(\\d)([a-z])") (parametricReplacer "-$1$2$$2$3") "6a7b8c" == "-6a$2$3-7b$2$3-8c$2$3"
    Regex.replace (AtMost 2) (regex "(\\d)([a-z])") (parametricReplacer "-$1$2$$2$3") "6a7b8c" == "-6a$2$3-7b$2$38c"
-}
parametricReplacer : String -> (Match -> String)
parametricReplacer new =
    let
        paramRegex index =
            "\\$\\$?" ++ (toString index)

        replaceParam match subMatch =
            if String.left 2 match == "$$" then
                String.dropLeft 1 match
            else
                subMatch

        replace index subMatch new =
            Regex.replace All (regex <| paramRegex index) (\m -> replaceParam m.match subMatch) new

        replaceSubmatchesInNew =
            L.foldl
                (\subMatch state -> { state | new = replace state.index subMatch state.new, index = state.index + 1 })
                { new = new, index = 1 }

        asStrings =
            L.map (Maybe.withDefault "")
    in
        \m -> .new <| replaceSubmatchesInNew <| asStrings m.submatches


{-| General simplified replace

    replace All "\\d" (simpleReplacer "_") "a1b2c3" == "a_b_c_"
	replace All "(\\d)([a-z])" (parametricReplacer "-$1$2$$2$3") "6a7b8c" == "-6a$2$3-7b$2$3-8c$2$3"
-}
replace : HowMany -> String -> (Match -> String) -> String -> String
replace howMany r replacer old =
    Regex.replace howMany (regex r) replacer old


{-| Simple replacement of ALL occurrences

    replaceSimple "\\d" "_" "a1b2c3" == "a_b_c_"
-}
replaceSimple : HowMany -> String -> String -> String -> String
replaceSimple howMany r new old =
    replace howMany r (simpleReplacer new) old


{-| Simple replacement of ALL occurrences

    replaceAll "\\d" "_" "a1b2c3" == "a_b_c_"
-}
replaceAll : String -> String -> String -> String
replaceAll =
    replaceSimple All


{-| Simple replacement of FIRST occurrences

    replaceFirst "\\d" "_" "a1b2c3" == "a_b2c3"
-}
replaceFirst : String -> String -> String -> String
replaceFirst =
    replaceSimple (AtMost 1)