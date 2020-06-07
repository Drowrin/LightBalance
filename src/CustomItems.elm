module CustomItems exposing (..)

import Api exposing (ResolvedItem)

type alias CustomItem =
    { name : String
    , light : Int
    , bucket : String
    }

nItem : String -> CustomItem
nItem b = CustomItem "No Name" 750 b

type GenItem
    = RItem ResolvedItem
    | CItem CustomItem