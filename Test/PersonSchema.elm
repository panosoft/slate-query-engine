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
    [ { name = "name"
      , eventNames =
            [ "Person name added"
            , "Person name removed"
            ]
      , owned = False
      }
    , { name = "age"
      , eventNames =
            [ "Person age added"
            , "Person age removed"
            ]
      , owned = False
      }
    , { name = "address"
      , eventNames =
            [ "Person address added"
            , "Person address removed"
            ]
      , owned = True
      }
    ]
