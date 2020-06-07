module CustomItems exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode

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

type alias CItemData =
    { nextId : Int
    , items : Dict String CustomItem
    }

encodeCItems : CItemData -> String
encodeCItems data =
    Encode.encode 0 <|
        Encode.object
            [ ( "nextId", Encode.int data.nextId )
            , ( "items", Encode.object
                <| List.map
                    (\(k, citem) ->
                        ( k, Encode.object
                            [ ( "name", Encode.string citem.name )
                            , ( "light", Encode.int citem.light )
                            , ( "bucket", Encode.string citem.bucket )
                            ]
                        )
                    )
                    <| Dict.toList data.items
              )
            ]
        

cItemDecoder : Decode.Decoder CustomItem
cItemDecoder =
    Decode.map3 CustomItem
        ( Decode.field "name" Decode.string )
        ( Decode.field "light" Decode.int )
        ( Decode.field "bucket" Decode.string )

cItemsDecoder : Decode.Decoder ( Dict String CustomItem )
cItemsDecoder =
    Decode.dict cItemDecoder

cItemDataDecoder : Decode.Decoder CItemData
cItemDataDecoder =
    Decode.map2 CItemData
        ( Decode.field "nextId" Decode.int )
        ( Decode.field "items" cItemsDecoder )

decodeCItems : String -> CItemData
decodeCItems s =
    Result.withDefault { nextId = 0, items = Dict.empty } <| Decode.decodeString cItemDataDecoder s