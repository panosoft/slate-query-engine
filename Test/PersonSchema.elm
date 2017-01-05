module PersonSchema exposing (..)

import Slate.Schema exposing (..)
import AddressSchema exposing (..)


personSchema : EntitySchema
personSchema =
    { type_ = "Person"
    , eventNames =
        [ "Person created"
        , "Person destroyed"
        ]
    , properties = personProperties
    }


personProperties : List PropertySchema
personProperties =
    [ { propSchema
        | name = "name"
        , eventNames =
            [ "Person name added"
            , "Person name removed"
            ]
      }
    , { propSchema
        | name = "age"
        , eventNames =
            [ "Person age added"
            , "Person age removed"
            ]
      }
    , { propSchema
        | name = "address"
        , entitySchema = Just <| SchemaReference addressSchema
        , eventNames =
            [ "Person address added"
            , "Person address removed"
            ]
        , owned = True
      }
    ]
