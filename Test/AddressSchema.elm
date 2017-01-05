module AddressSchema exposing (..)

import Slate.Schema exposing (..)


addressSchema : EntitySchema
addressSchema =
    { type_ = "Address"
    , eventNames =
        [ "Address created"
        , "Address destroyed"
        ]
    , properties = addressProperties
    }


addressProperties : List PropertySchema
addressProperties =
    [ { propSchema
        | name = "street"
        , eventNames =
            [ "Address street added"
            , "Address street removed"
            ]
      }
    , { propSchema
        | name = "city"
        , eventNames =
            [ "Address city added"
            , "Address city removed"
            ]
      }
    , { propSchema
        | name = "state"
        , eventNames =
            [ "Address state added"
            , "Address state removed"
            ]
      }
    , { propSchema
        | name = "zip"
        , eventNames =
            [ "Address zip added"
            , "Address zip removed"
            ]
      }
    ]
