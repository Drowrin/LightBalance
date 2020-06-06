module Api exposing (..)

import Bytes.Encode
import Base64.Encode

import Url exposing (Url)
import Browser.Navigation
import OAuth
import OAuth.AuthorizationCode as AuthCode
import Http
import Dict exposing (Dict)
import Json.Decode as Decode exposing (field, list, string, int, dict, maybe)
import Json.Encode as Encode

import Config exposing (clientId, apiKey)

convertBytes : List Int -> String
convertBytes =
    List.map Bytes.Encode.unsignedInt8
    >> Bytes.Encode.sequence >> Bytes.Encode.encode
    >> Base64.Encode.bytes >> Base64.Encode.encode

unpack : (e -> b) -> (a -> b) -> Result e a -> b
unpack errFunc okFunc result =
    case result of
        Ok ok ->
            okFunc ok

        Err err ->
            errFunc err

defaultUrl : Url
defaultUrl = 
    { protocol = Url.Https
    , host = "www.bungie.net"
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }

makeHeaders : OAuth.Token -> List Http.Header -> List Http.Header
makeHeaders token headers =
    OAuth.useToken token <| Http.header "X-API-Key" apiKey :: headers

getCodeAuth : Url -> List Int -> Cmd msg
getCodeAuth redirectUri bytes =
    Browser.Navigation.load <| Url.toString <| AuthCode.makeAuthorizationUrl <|
        { clientId = clientId
        , scope = []
        , state = Just <| convertBytes bytes
        , redirectUri = redirectUri
        , url = { defaultUrl | path = "/en/OAuth/Authorize" }
        }

getTokenAuth : Url -> String -> (Http.Error -> msg) -> ( AuthCode.AuthenticationSuccess -> msg ) -> Cmd msg
getTokenAuth redirectUri code onError onSuccess =
    Http.request <| AuthCode.makeTokenRequest
        ( unpack onError onSuccess ) 
        { credentials =
            { clientId = clientId
            , secret = Nothing
            }
        , code = code
        , redirectUri = redirectUri
        , url = { defaultUrl | path = "/Platform/App/OAuth/Token" }
        }

type alias GeneralUser =
    { id : String
    , name : String
    , profilePicture : Url
    }

generalUserDecoder : Decode.Decoder GeneralUser
generalUserDecoder =
    Decode.map3 GeneralUser
        ( field "membershipId" string )
        ( field "displayName" string )
        ( Decode.map (\s -> { defaultUrl | path = s } ) <| field "profilePicturePath" string )

getGeneralUser : OAuth.Token -> (Http.Error -> msg) -> ( GeneralUser -> msg ) -> Cmd msg
getGeneralUser token onError onSuccess =
    Http.request
        { method = "GET"
        , headers = makeHeaders token []
        , url = Url.toString { defaultUrl | path = "/Platform/User/GetCurrentBungieNetUser/" }
        , body = Http.emptyBody
        , expect = Http.expectJson
            ( unpack onError onSuccess )
            ( Decode.field "Response" generalUserDecoder )
        , timeout = Nothing
        , tracker = Nothing
        }

type alias DestinyMembership =
    { mtype : String
    , mid : String
    , icon : Url
    }

destinyMembershipDecoder : Decode.Decoder DestinyMembership
destinyMembershipDecoder =
    Decode.map3 DestinyMembership
        ( Decode.map String.fromInt <| field "membershipType" int )
        ( field "membershipId" string )
        ( Decode.map (\s -> { defaultUrl | path = s } ) <| field "iconPath" string )

getMembership : OAuth.Token -> String -> (Http.Error -> msg) -> ((List DestinyMembership) -> msg) -> Cmd msg
getMembership token mid onError onSuccess =
    Http.request
        { method = "GET"
        , headers = makeHeaders token []
        , url = Url.toString 
            { defaultUrl
            | path = "/Platform/User/GetMembershipsById/" ++ mid ++ "/-1"
            }
        , body = Http.emptyBody
        , expect = Http.expectJson
            ( unpack onError onSuccess )
            (Decode.at [ "Response", "destinyMemberships" ] <| list destinyMembershipDecoder )
        , timeout = Nothing
        , tracker = Nothing
        }

type alias ItemRef =
    { hash : String
    , instanceId : String
    }

itemRefDecoder : Decode.Decoder ItemRef
itemRefDecoder =
    Decode.map2 ItemRef
        ( Decode.map String.fromInt <| field "itemHash" int )
        ( field "itemInstanceId" string )

type alias ItemInstance = 
    { statHash : String
    , statValue : Int
    }

itemInstanceDecoder : Decode.Decoder ItemInstance
itemInstanceDecoder =
    Decode.map2 ItemInstance
        ( Decode.map String.fromInt <| Decode.at [ "primaryStat", "statHash" ] int )
        ( Decode.at [ "primaryStat", "value" ] int )


manifestFilterFold : String -> Maybe a -> Dict String a -> Dict String a
manifestFilterFold k mip accumulator =
    case mip of
        Just ip ->
            Dict.insert k ip accumulator
        Nothing ->
            accumulator

manifestFilter : Dict String (Maybe a) -> Dict String a
manifestFilter d =
    Dict.foldl manifestFilterFold Dict.empty d

type alias ItemProps =
    { name : String
    , description : String
    , icon : Url
    , classType : Int
    , equipHash : String
    }

itemPropsDecoder : Decode.Decoder ItemProps
itemPropsDecoder =
    Decode.map5 ItemProps
        ( Decode.at [ "displayProperties", "name" ] string )
        ( Decode.at [ "displayProperties", "description" ] string )
        ( Decode.map
            (\s -> { defaultUrl | path = s } )
            ( Decode.at [ "displayProperties", "icon" ] string )
        )
        ( field "classType" int )
        ( Decode.map String.fromInt <| Decode.at [ "equippingBlock", "equipmentSlotTypeHash" ] int )

itemManifestDecoder : Decode.Decoder ItemManifest
itemManifestDecoder =
    Decode.map manifestFilter
        <| Decode.dict <| Decode.maybe itemPropsDecoder

type alias EquipSlotType = String

equipSlotTypeDecoder : Decode.Decoder EquipSlotType
equipSlotTypeDecoder =
    Decode.at [ "displayProperties", "name" ] string

equipSlotManifestDecoder : Decode.Decoder EquipSlotManifest
equipSlotManifestDecoder =
    Decode.map manifestFilter
        <|  Decode.dict <| Decode.maybe equipSlotTypeDecoder

type alias ClassType =
    { classType : Int
    , name : String
    }

classTypeDecoder : Decode.Decoder ClassType
classTypeDecoder =
    Decode.map2 ClassType
        ( field "classType" int )
        ( Decode.at [ "displayProperties", "name" ] string )

classManifestDecoder : Decode.Decoder ClassManifest
classManifestDecoder =
    Decode.map manifestFilter
        <| Decode.dict <| Decode.maybe classTypeDecoder

type alias StatType = String

statTypeDecoder : Decode.Decoder StatType
statTypeDecoder =
    Decode.at [ "displayProperties", "name" ] string

statManifestDecoder : Decode.Decoder StatManifest
statManifestDecoder =
    Decode.map manifestFilter
        <| Decode.dict <| Decode.maybe statTypeDecoder

type alias ResolvedItem =
    { name : String
    , description : String
    , icon : Url
    , light : Int
    , bucket : String
    }

classBucket : ClassManifest -> Int -> String -> String
classBucket cm classType bucket =
    if List.member bucket [ "Kinetic Weapons", "Energy Weapons", "Power Weapons" ]
    then bucket
    else case List.head <| List.filter (\c -> c.classType == classType ) <| Dict.values cm of
        Just c -> c.name ++ " " ++ bucket
        Nothing -> bucket

resolveItem :
    Manifest -> Dict String ItemInstance -> Maybe ItemRef
    -> Maybe ResolvedItem
resolveItem manifest ii mir =
    case mir of
        Just ir -> case ( Dict.get ir.hash manifest.items, Dict.get ir.instanceId ii ) of
            ( Just props, Just instance ) ->
                case Dict.get props.equipHash manifest.equipSlots of
                    Just equipSlot ->
                        if validStat manifest.stats instance.statHash then
                            Just
                            { name = props.name
                            , description = props.description
                            , icon = props.icon
                            , light = instance.statValue
                            , bucket = classBucket manifest.classes props.classType equipSlot
                            }
                        else Nothing
                    _ -> Nothing
            _ -> Nothing
        _ -> Nothing

type alias ManifestLinks =
    { version : String
    , equipSlots : Url
    , items : Url
    , classes : Url
    , stats : Url
    }

manifestLinksDecoder : Decode.Decoder ManifestLinks
manifestLinksDecoder =
    Decode.map5 ManifestLinks
        ( field "version" string )
        ( Decode.at [ "jsonWorldComponentContentPaths", "en" ]
            <| Decode.map (\s -> { defaultUrl | path = s } ) <| field "DestinyEquipmentSlotDefinition" string
        )
        ( Decode.at [ "jsonWorldComponentContentPaths", "en" ]
            <| Decode.map (\s -> { defaultUrl | path = s } ) <| field "DestinyInventoryItemDefinition" string
        )
        ( Decode.at [ "jsonWorldComponentContentPaths", "en" ]
            <| Decode.map (\s -> { defaultUrl | path = s } ) <| field "DestinyClassDefinition" string
        )
        ( Decode.at [ "jsonWorldComponentContentPaths", "en" ]
            <| Decode.map (\s -> { defaultUrl | path = s } ) <| field "DestinyStatDefinition" string
        )

type alias EquipSlotManifest = Dict String EquipSlotType

type alias ItemManifest = Dict String ItemProps

type alias ClassManifest = Dict String ClassType

type alias StatManifest = Dict String StatType

type alias Manifest =
    { version : String
    , items : ItemManifest
    , equipSlots : EquipSlotManifest
    , classes : ClassManifest
    , stats : StatManifest
    }

manifestDecoder : Decode.Decoder Manifest
manifestDecoder =
    Decode.map5 Manifest
        ( field "version" string )
        ( field "items" <| dict <| Decode.map5 ItemProps
            ( field "name" string )
            ( field "description" string )
            ( field "icon" <| Decode.map (\i-> { defaultUrl | path = i } ) string )
            ( field "classType" int )
            ( field "equipHash" string )
        )
        ( field "equipSlots" <| dict string )
        ( field "classes" <| dict <| Decode.map2 ClassType
            ( field "classType" int )
            ( field "name" string )
        )
        ( field "stats" <| dict string )

decodeManifest : Maybe String -> Maybe Manifest
decodeManifest ms =
    case ms of
        Just s -> case Decode.decodeString manifestDecoder s of
            Ok manifest -> Just manifest
            Err _ -> Nothing
        _ -> Nothing

encodeManifest : Manifest -> String
encodeManifest manifest =
    Encode.encode 0 <| Encode.object
        [ ( "version", Encode.string manifest.version )
        , ( "items"
          , Encode.dict identity (\o -> Encode.object
                [ ( "name", Encode.string o.name )
                , ( "description", Encode.string o.description )
                , ( "icon", Encode.string <| o.icon.path )
                , ( "classType", Encode.int o.classType )
                , ( "equipHash", Encode.string o.equipHash )
                ]
            )
            manifest.items
          )
        , ( "equipSlots"
          , Encode.dict identity Encode.string manifest.equipSlots
          )
        , ( "classes"
          , Encode.dict identity (\o -> Encode.object
                [ ( "classType", Encode.int o.classType )
                , ( "name", Encode.string o.name )
                ]
            )
            manifest.classes
          )
        , ( "stats"
          , Encode.dict identity Encode.string manifest.stats
          )
        ]

validStat : StatManifest -> String -> Bool
validStat sm statHash =
    case Dict.get statHash sm of
        Just s -> s == "Attack" || s == "Defense"
        Nothing -> False

getManifest : (Http.Error -> msg) -> (ManifestLinks -> msg) -> Cmd msg
getManifest onError onSuccess =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.toString
            { defaultUrl
            | path = "/Platform/Destiny2/Manifest/"
            }
        , body = Http.emptyBody
        , expect = Http.expectJson
            ( unpack onError onSuccess )
            ( Decode.field "Response" manifestLinksDecoder )
        , timeout = Nothing
        , tracker = Nothing
        }

getManifestData : Url -> (Http.Error -> msg) -> (d -> msg) -> Decode.Decoder d -> Cmd msg
getManifestData url onError onSuccess decoder =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.toString url
        , body = Http.emptyBody
        , expect = Http.expectJson ( unpack onError onSuccess ) decoder
        , timeout = Nothing
        , tracker = Nothing
        }

type alias DataType = Dict String ( List ResolvedItem )

append3 : List a -> List a -> List a -> List a
append3 la lb lc =
    List.append la <| List.append lb lc

bucketFoldSort : ResolvedItem -> DataType -> DataType
bucketFoldSort item accumulator =
    case Dict.get item.bucket accumulator of
        Just l ->
            Dict.insert item.bucket (item :: l) accumulator
        Nothing ->
            Dict.insert item.bucket [ item ] accumulator

getData :
    OAuth.Token -> DestinyMembership -> Manifest
    -> (Http.Error -> msg) -> (DataType -> msg)
    -> Cmd msg
getData token member manifest onError onSuccess =
    Http.request
        { method = "GET"
        , headers = makeHeaders token []
        , url = Url.toString
            { defaultUrl
            | path = "/Platform/Destiny2/" ++ member.mtype 
                ++ "/Profile/" ++ member.mid 
                ++ "/?components=102,201,205,300"
            }
        , body = Http.emptyBody
        , expect = Http.expectJson
            ( unpack onError onSuccess )
            ( field "Response"
                ( ( Decode.map manifestFilter
                    <| Decode.at [ "itemComponents", "instances", "data" ]
                    <| dict <| maybe itemInstanceDecoder
                  )
                    |> Decode.andThen (\ii ->
                        Decode.map (List.foldl bucketFoldSort Dict.empty)
                        <| Decode.map (List.map (resolveItem manifest ii) >> List.filterMap identity)
                        <| Decode.map3 append3
                            ( Decode.at [ "profileInventory", "data", "items" ] <| list <| maybe itemRefDecoder )
                            ( Decode.map
                                ( Dict.values >> List.concat )
                                ( Decode.at [ "characterInventories", "data" ] <| dict <| field "items" <| list <| maybe itemRefDecoder )
                            )
                            ( Decode.map
                                ( Dict.values >> List.concat )
                                ( Decode.at [ "characterEquipment", "data" ] <| dict <| field "items" <| list <| maybe itemRefDecoder )
                            )
                    )
                )
            )
        , timeout = Nothing
        , tracker = Nothing
        }