module PersonSchema exposing (..)

import Slate.Schema exposing (..)


personSchema : EntitySchema
personSchema =
    { type' = "Person"
    , eventNames =
        [ "Person created"
        , "Person destroyed"
        ]
    , properties = personProperties
    }


personProperties : List PropertySchema
personProperties =
    [ { type' = "name"
      , eventNames =
            [ "Person name added"
            , "Person name removed"
            ]
      , owned = False
      }
    , { type' = "age"
      , eventNames =
            [ "Person age added"
            , "Person age removed"
            ]
      , owned = False
      }
    ]
