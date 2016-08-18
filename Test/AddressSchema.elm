module AddressSchema exposing (..)

import Slate.Schema exposing (..)


addressSchema : EntitySchema
addressSchema =
    { type' = "Address"
    , eventNames =
        [ "Address created"
        , "Address destroyed"
        ]
    , properties = addressProperties
    }


addressProperties : List PropertySchema
addressProperties =
    [ { name = "street"
      , eventNames =
            [ "Person street added"
            , "Person street removed"
            ]
      , owned = False
      }
    , { name = "city"
      , eventNames =
            [ "Person city added"
            , "Person city removed"
            ]
      , owned = False
      }
    , { name = "state"
      , eventNames =
            [ "Person state added"
            , "Person state removed"
            ]
      , owned = False
      }
    , { name = "zip"
      , eventNames =
            [ "Person zip added"
            , "Person zip removed"
            ]
      , owned = False
      }
    ]
