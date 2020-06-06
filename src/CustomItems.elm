module CustomItems exposing (..)

import Api exposing (ResolvedItem)

type alias CustomItem =
    { name : String
    , light : Int
    , bucket : String
    }

nItem : String -> GenItem
nItem b = CItem <| CustomItem "No Items" 750 b

type GenItem
    = RItem ResolvedItem
    | CItem CustomItem