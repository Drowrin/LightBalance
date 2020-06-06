port module Main exposing (main)

import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Browser.Events
import Url exposing (Url)
import OAuth
import OAuth.AuthorizationCode as AuthCode
import Http
import Task
import Time

import Html exposing (Html)
import Element exposing (..)
import Element.Input as Input
import Element.Font as Font
import Element.Border as Border
import Element.Background as Background
import Element.Events as Events
import Element.Keyed as Keyed

import Dict
import List.Extra exposing (maximumBy)

import Api exposing
    ( GeneralUser, DestinyMembership
    , Manifest, ManifestLinks, ItemManifest, EquipSlotManifest, ClassManifest, StatManifest, DataType
    )
import CustomItems exposing (CustomItem, GenItem(..), nItem)

type alias Flags =
    { w : Int
    , h : Int
    , bytes : Maybe ( List Int )
    , manifest : Maybe String
    , customItems : Maybe String
    }

do : msg -> Cmd msg
do msg =
    Task.perform (\_ -> msg) <| Task.succeed ()

main : Program Flags Model Msg
main = application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = UrlRequested
    , onUrlChange = always NoOp
    , view = \model ->
        { title = "Light Balance"
        , body = [ view model ]
        }
    }

port requestBytes : Int -> Cmd msg
port receivedBytes : ( List Int -> msg ) -> Sub msg

port saveManifest : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receivedBytes ReceivedBytes
        , Browser.Events.onResize WindowResize
        , case ( model.flowState, model.data ) of
            ( FlowComplete token _ member, DataComplete manifest _ ) ->
                Time.every
                    ( 60 * 1000 )
                    ( RefreshData token member manifest )
            _ ->
                Sub.none
        ]

type FlowErrorType
    = HttpError Http.Error
    | AuthorizationError AuthCode.AuthorizationError
    | StateMismatch
    | NoDestinyMembers

type FlowState
    = UnAuth
    | FlowError FlowErrorType
    | FlowLoading
    | MemberSelect OAuth.Token GeneralUser (List DestinyMembership)
    | FlowComplete OAuth.Token GeneralUser DestinyMembership

type DataState
    = DataError Http.Error
    | NoData
    | HaveManifest Manifest
    | DataComplete Manifest DataType

type DataLoadState
    = LoadingManifest String
    | NeedLogin
    | LoadingData
    | LoadComplete

type alias Model =
    { flowState : FlowState
    , data : DataState
    , dataLoad : DataLoadState

    , baseUrl : Url

    , w : Int
    , h : Int

    , viewAbout : Bool
    , key : Browser.Navigation.Key
    }

init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        baseUrl = { url | query = Nothing, fragment = Nothing }
        clearUrl = Browser.Navigation.replaceUrl key ( Url.toString baseUrl )
        mbytes = Maybe.map Api.convertBytes flags.bytes
        mmanifest = Api.decodeManifest flags.manifest
        baseModel =
            { flowState = UnAuth
            , data = NoData
            , dataLoad = LoadingManifest ""
            , baseUrl = baseUrl
            , w = flags.w
            , h = flags.h
            , viewAbout = False
            , key = key
            }
        baseCmds = [ clearUrl, do <| GetManifestLinks mmanifest ]
    in
    case AuthCode.parseCode url of
        AuthCode.Empty ->
            ( baseModel
            , Cmd.batch baseCmds
            )
        
        AuthCode.Error e ->
            ( { baseModel | flowState = FlowError <| AuthorizationError e }
            , Cmd.batch baseCmds
            )
        
        AuthCode.Success { code, state } ->
            case mbytes of
                Nothing ->
                    ( { baseModel | flowState = FlowError StateMismatch }
                    , Cmd.batch baseCmds
                    )
                
                Just bytes ->
                    if state /= Just bytes then
                        ( { baseModel | flowState = FlowError StateMismatch }
                        , Cmd.batch baseCmds
                        )
                    
                    else
                        ( { baseModel | flowState = FlowLoading }
                        , Cmd.batch
                            [ clearUrl
                            , Cmd.batch <| ( do <| GetToken code ) :: baseCmds
                            ]
                        )

type Msg
    = NoOp

    | WindowResize Int Int

    | AboutPressed Bool

    | UrlRequested Browser.UrlRequest

    | GotFlowError FlowErrorType
    | GotDataError Http.Error

    | Login
    | ReceivedBytes ( List Int )
    | GetToken String
    | GotToken AuthCode.AuthenticationSuccess
    
    | GotUser OAuth.Token GeneralUser
    | GotMemberships OAuth.Token GeneralUser ( List DestinyMembership )
    | GotMembership OAuth.Token GeneralUser DestinyMembership

    | GetManifestLinks ( Maybe Manifest )
    | GotManifestLinks ( Maybe Manifest ) ManifestLinks
    | GotItemManifest ManifestLinks ItemManifest
    | GotEquipSlotManifest ManifestLinks ItemManifest EquipSlotManifest
    | GotClassManifest ManifestLinks ItemManifest EquipSlotManifest ClassManifest
    | GotStatManifest ManifestLinks ItemManifest EquipSlotManifest ClassManifest StatManifest
    | GotManifest Manifest

    | GetData OAuth.Token DestinyMembership Manifest
    | RefreshData OAuth.Token DestinyMembership Manifest Time.Posix
    | GotData Manifest DataType

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    -- Basic Functionality Messages

    NoOp -> ( model, Cmd.none )

    WindowResize w h ->
        ( { model
          | w = w
          , h = h
          }
        , Cmd.none
        )
    
    UrlRequested urlrq ->
        ( model
        , case urlrq of
            Browser.External s ->
                Browser.Navigation.load s
            _ ->
                Cmd.none
        )
    
    AboutPressed b ->
        ( { model | viewAbout = b }
        , Cmd.none
        )

    GotFlowError e ->
        ( { model | flowState = FlowError e }
        , Cmd.none
        )
    
    GotDataError e ->
        ( { model | data = DataError e }
        , Cmd.none
        )
    
    -- Authentication Flow Messages

    Login ->
        ( model
        , requestBytes 16
        )
    
    ReceivedBytes bytes ->
        ( model
        , Api.getCodeAuth model.baseUrl bytes
        )
    
    GetToken code ->
        ( model
        , Api.getTokenAuth model.baseUrl code ( GotFlowError << HttpError ) GotToken
        )
    
    GotToken { token } ->
        ( model
        , Api.getGeneralUser token ( GotFlowError << HttpError ) ( GotUser token )
        )
    
    GotUser token user ->
        ( model
        , Api.getMembership
            token user.id
            ( GotFlowError << HttpError )
            ( GotMemberships token user )
        )
    
    GotMemberships token user mlist ->
        case mlist of
            [] ->
                ( { model | flowState = FlowError NoDestinyMembers }
                , Cmd.none
                )
            [ membership ] ->
                ( model
                , do <| GotMembership token user membership
                )
            members ->
                ( { model | flowState = MemberSelect token user members }
                , Cmd.none
                )
    
    GotMembership token user member ->
        ( { model | flowState = FlowComplete token user member }
        , case model.data of
            HaveManifest manifest ->
                do <| GetData token member manifest
            _ ->
                Cmd.none
        )

    -- Data Load Messages

    GetManifestLinks mmanifest ->
        ( { model | dataLoad = LoadingManifest "" }
        , Api.getManifest GotDataError ( GotManifestLinks mmanifest )
        )
    
    GotManifestLinks mmanifest manifestLinks ->
        let
            needNewManifest =
                ( { model | dataLoad = LoadingManifest "Items" }
                , Api.getManifestData
                    manifestLinks.items
                    GotDataError
                    ( GotItemManifest manifestLinks )
                    Api.itemManifestDecoder
                )
            haveManifest m =
                ( model
                , Cmd.batch
                    [ do <| GotManifest m
                    , saveManifest <| Api.encodeManifest m
                    ]
                )
        in
        case mmanifest of
            Just manifest ->
                if manifest.version == manifestLinks.version
                then haveManifest manifest
                else needNewManifest
            Nothing -> needNewManifest

    GotItemManifest manifestLinks im ->
        ( { model | dataLoad = LoadingManifest "Equipment Slots" }
        , Api.getManifestData
            manifestLinks.equipSlots
            GotDataError
            ( GotEquipSlotManifest manifestLinks im )
            Api.equipSlotManifestDecoder
        )

    GotEquipSlotManifest manifestLinks im em ->
        ( { model | dataLoad = LoadingManifest "Classes" }
        , Api.getManifestData
            manifestLinks.classes
            GotDataError
            ( GotClassManifest manifestLinks im em )
            Api.classManifestDecoder
        )

    GotClassManifest manifestLinks im em cm ->
        ( { model | dataLoad = LoadingManifest "Stats" }
        , Api.getManifestData
            manifestLinks.stats
            GotDataError
            ( GotStatManifest manifestLinks im em cm )
            Api.statManifestDecoder
        )

    GotStatManifest manifestLinks im em cm sm ->
        ( { model | dataLoad = LoadingManifest "Finalizing" }
        , let
            manifest = Manifest manifestLinks.version im em cm sm
          in
          Cmd.batch
            [ do <| GotManifest <| manifest
            , saveManifest <| Api.encodeManifest manifest
            ]
        )

    GotManifest manifest ->
        case model.flowState of
            FlowComplete token _ member ->
                ( { model
                  | data = HaveManifest manifest
                  }
                , do <| GetData token member manifest
                )
            _ ->
                ( { model
                  | data = HaveManifest manifest
                  , dataLoad = NeedLogin
                  }
                , Cmd.none
                )

    GetData token member manifest ->
        ( { model | dataLoad = LoadingData }
        , Api.getData
            token member manifest
            ( HttpError >> GotFlowError )
            ( GotData manifest )
        )

    RefreshData token member manifest _ ->
        ( model
        , do <| GetData token member manifest
        )

    GotData manifest data ->
        ( { model
          | dataLoad = LoadComplete
          , data = DataComplete manifest data
          }
        , Cmd.none
        )

bgColor : Color
bgColor = rgb255 20 20 20

bgColor2 : Color
bgColor2 = rgb255 40 40 40

bgColor3 : Color
bgColor3 = rgb255 15 15 15

txtColor : Color
txtColor = rgb255 250 250 250

accColor : Color
accColor = rgb255 120 120 190

yesColor : Color
yesColor = rgb255 140 200 140

noColor : Color
noColor = rgb255 200 140 140

textSize : Model -> Int
textSize _ = 20

bigTextSize : Model -> Int
bigTextSize _ = 30

smallTextSize : Model -> Int
smallTextSize _ = 12

hDivider : Element Msg
hDivider =
    row
        [ width fill ]
        [ el [ width <| px 10 ] none
        , el
            [ Background.color bgColor2
            , width fill
            , height <| px 1
            ]
            none
        , el [ width <| px 10 ] none
        ]
        

vDivider : Element Msg
vDivider =
    column
        [ height fill ]
        [ el [ height <| px 10 ] none
        , el
            [ Background.color bgColor2
            , height fill
            , width <| px 1
            ]
            none
        , el [ height <| px 10 ] none
        ]

viewDataLoad : Model -> Element Msg
viewDataLoad model =
    let
        base = model.baseUrl
        imgAttr =
            [ width <| px 20
            , height <| px 20
            ]
        spinner =
            image
                imgAttr
                { src = Url.toString { base | path = base.path ++ "loading.gif" }
                , description = ""
                }
        refresh =
            case ( model.data, model.flowState ) of
                ( DataComplete manifest _, FlowComplete token _ member ) ->
                    image
                        ( Events.onClick ( GetData token member manifest )
                        :: pointer 
                        :: imgAttr
                        )
                        { src = Url.toString { base | path = base.path ++ "refresh.png" }
                        , description = ""
                        }
                _ ->
                    none
        mainViews =
            case model.dataLoad of
                LoadingManifest s ->
                    [ ( "spinner", spinner )
                    , ( "Loading Manifest", text <| "Loading Manifest " ++ s )
                    ]
                NeedLogin ->
                    [ ( "Login Required", text "Login Required" ) ]
                LoadingData ->
                    [ ( "spinner", spinner )
                    , ( "Loading Data", text "Loading Data" )
                    ]
                LoadComplete ->
                    [ ( "refresh", refresh )
                    , ( "Ready", text "Ready!" )
                    ]
        underView =
            case model.data of
                DataError e ->
                    el [ padding 5 ] <| case e of
                        Http.BadUrl s -> text <| "Bad URL: " ++ s
                        Http.Timeout -> text <| "Request Timed Out"
                        Http.NetworkError -> text <| "Network Error"
                        Http.BadStatus i -> text <| "Bad Status: " ++ String.fromInt i
                        Http.BadBody s ->
                            column
                                []
                                [ text "JSON Decode Error:"
                                , paragraph
                                    []
                                    [ text <| Maybe.withDefault ""
                                        <| List.head <| List.reverse
                                        <| String.split "\n" s
                                    ]
                                ]
                _ ->
                    none
    in
    Keyed.row
        [ width fill
        , height fill
        , padding 10
        , spacing 8
        , centerY
        , below <| column
            [ width shrink
            , height shrink
            , Background.color accColor
            , alignRight
            ]
            [ underView ]
        ]
        mainViews

viewFlowError : FlowErrorType -> Element Msg
viewFlowError e =
    el [ padding 5 ] <| Element.text <| case e of
        HttpError he -> case he of
            Http.BadUrl s -> "Bad URL: " ++ s
            Http.Timeout -> "Request Timed Out"
            Http.NetworkError -> "Network Error"
            Http.BadStatus i -> "Bad Status: " ++ String.fromInt i
            Http.BadBody s -> "Bad Body: " ++ s
        AuthorizationError ae ->
            "( Error: " ++ OAuth.errorCodeToString ae.error ++ " )\n" ++
            "( Description: " ++ Maybe.withDefault "" ae.errorDescription ++ " )\n" ++
            "( Uri: " ++ Maybe.withDefault "" ae.errorUri ++ " )\n" ++
            "( State: " ++ Maybe.withDefault "" ae.state ++ " )"

        StateMismatch -> "State Mismatch"

        NoDestinyMembers -> "No Destiny Accounts Found"

userImg : GeneralUser -> Element Msg
userImg user =
    image
        [ width <| px 40 ]
        { src = Url.toString user.profilePicture
        , description = "Profile Picture"
        }

memberImg : DestinyMembership -> Element Msg
memberImg member =
    image
        [ width <| px 40 ]
        { src = Url.toString member.icon
        , description = "Platform Icon"
        }

viewAuthState : Model -> Element Msg
viewAuthState model =
    let
        loginButton = 
            Input.button
                [ focused [] ]
                { onPress = Just Login
                , label = el
                    [ padding 5
                    , Background.color accColor
                    ]
                    <| text "Login"
                }
        ( mainViews, underViews ) =
            case model.flowState of
                UnAuth ->
                    ( [ loginButton ]
                    , none
                    )
                FlowError e ->
                    ( [ loginButton ]
                    , viewFlowError e
                    )
                FlowLoading ->
                    ( [ text "Loading..." ]
                    , none
                    )
                MemberSelect token user members ->
                    ( [ userImg user
                      , text user.name
                      ]
                    , row [ padding 5 ]
                        ( text "Select Platform: "
                          ::
                          List.map
                            (\member -> el 
                                [ Events.onClick <| GotMembership token user member ]
                                <| memberImg member
                            )
                            members
                        )
                    )
                FlowComplete _ user member ->
                    ( [ text user.name
                      , userImg user
                      , memberImg member
                      ]
                    , none
                    )
    in
    row
        [ width fill
        , height fill
        , below <| column
            [ width shrink
            , height shrink
            , Background.color accColor
            , alignRight
            ]
            [ underViews ]
        , padding 10
        , centerY
        ]
        [ row
            [ height fill
            , width shrink
            , spacing 5
            , alignRight
            ]
            mainViews
        ]

viewHeader : Model -> Element Msg
viewHeader model =
    row
        [ width fill
        , height <| px 60
        ]
        [ viewDataLoad model
        , vDivider
        , viewAuthState model
        ]

selectItem : Model -> String -> GenItem
selectItem model b =
    case model.data of
        DataComplete _ data ->
            case Dict.get b data of
                Just l ->
                    case maximumBy .light l of
                        Just i ->
                            RItem i
                        _ ->
                            nItem b
                _ ->
                    nItem b
        _ ->
            nItem b

viewItem : Model -> GenItem -> Bool -> Element Msg
viewItem model mitem canBalance =
    let
        size = px 80
        iText s = 
            el [ Background.color bgColor3, alpha 0.8 ] <|
            el [ padding 2, Font.size <| smallTextSize model ] <| text s
        blockAttr =
            [ width size
            , height size
            , centerX
            , Background.color bgColor3
            , Border.width <| if canBalance then 3 else 0
            , Border.color <| if canBalance then yesColor else bgColor
            ]
        itemAttr light = 
            ( inFront <| el [ alignRight, alignBottom] <| iText <| String.fromInt light )
            ::
            blockAttr
        attr name =
            [ width fill
            , inFront <| el
                [ width fill
                , height fill
                , transparent True
                , mouseOver [ transparent False ]
                ]
                <| el
                    [ centerX, centerY ]
                    <| iText name
            ]
    in
    el [ width fill ] <|
        case mitem of
            CItem i ->
                if i.light <= 750
                then el ( attr i.name ) <| el blockAttr none
                else el ( attr i.name ) <| el ( itemAttr i.light ) none
            RItem i ->
                el ( attr i.name ) <|
                    image
                        ( itemAttr i.light )
                        { src = Url.toString i.icon
                        , description = i.name
                        }

type alias Loadout =
    { kinetic : GenItem
    , energy : GenItem
    , power : GenItem
    , helmet : GenItem
    , gauntlets : GenItem
    , chest : GenItem
    , legs : GenItem
    , classItem : GenItem

    , light : Int
    }

average : (Int, Int) -> Int
average (light, count) = light // count

makeLoadout :
    GenItem -> GenItem -> GenItem -> GenItem -> GenItem -> GenItem -> GenItem -> GenItem
    -> Loadout
makeLoadout kinetic energy power helmet gauntlets chest legs classItem =
    Loadout
        kinetic energy power
        helmet gauntlets chest legs classItem
        <| average <|
            List.foldl 
                (\gi (light, count) ->
                    case gi of
                        RItem i -> (light + i.light, count + 1)
                        CItem i -> (light + i.light, count + 1)
                )
                (0, 0)
                [ kinetic, energy, power
                , helmet, gauntlets, chest, legs, classItem
                ]

getLoadout : Model -> String -> Loadout
getLoadout model class =
    makeLoadout
        ( selectItem model "Kinetic Weapons" )
        ( selectItem model "Energy Weapons" )
        ( selectItem model "Power Weapons" )
        ( selectItem model <| class ++ " Helmet" )
        ( selectItem model <| class ++ " Gauntlets" )
        ( selectItem model <| class ++ " Chest Armor" )
        ( selectItem model <| class ++ " Leg Armor" )
        ( selectItem model <| class ++ " Class Armor" )

lValue : GenItem -> Int
lValue gi =
    case gi of
        RItem i -> i.light
        CItem i -> i.light

uLValue : GenItem -> Int -> GenItem
uLValue gi l =
    case gi of
        RItem i -> RItem { i | light = l }
        CItem i -> CItem { i | light = l }

balanceItem : Int -> GenItem -> GenItem
balanceItem light item =
    if light > lValue item
    then uLValue item light
    else item
    
balanceLoadout : Loadout -> Loadout
balanceLoadout loadout =
    makeLoadout
        ( balanceItem loadout.light loadout.kinetic )
        ( balanceItem loadout.light loadout.energy )
        ( balanceItem loadout.light loadout.power )
        ( balanceItem loadout.light loadout.helmet )
        ( balanceItem loadout.light loadout.gauntlets )
        ( balanceItem loadout.light loadout.chest )
        ( balanceItem loadout.light loadout.legs )
        ( balanceItem loadout.light loadout.classItem )

getAndViewLoadout : Model -> String -> Element Msg
getAndViewLoadout model b =
    let
        loadout = getLoadout model b
        bLoadout = balanceLoadout loadout
        canBalance = bLoadout.light > loadout.light
        iView getter =
            viewItem
                model
                ( getter loadout )
                ( ( lValue <| getter loadout ) < bLoadout.light && canBalance )
    in
    column
        [ width fill
        , spacing 10
        ]
        [ el
            [ centerX
            , Font.color <| if canBalance
                then yesColor
                else txtColor
            ]
            <| text b

        , row [ width fill ]
            [ column [ width fill, centerY, spacing 10 ]
                [ iView .kinetic
                , iView .energy
                , iView .power
                ]
            , column [ width fill, centerY, spacing 10 ]
                [ iView .helmet
                , iView .gauntlets
                , iView .chest
                , iView .legs
                , iView .classItem
                ]
            ]
        
        , el [ centerX ] <| paragraph
            []
            <| if canBalance then
                [ el [ Font.color noColor ] <| text <| String.fromInt loadout.light
                , text " → "
                , el [ Font.color yesColor ] <| text <| String.fromInt bLoadout.light
                ]
            else
                [ text <| String.fromInt loadout.light ]
                
        ]

viewMain : Model -> Element Msg
viewMain model =
    row
        [ width <| minimum 600 <| fill
        , spacing 10
        , padding 10
        ]
        [ getAndViewLoadout model "Hunter"
        , vDivider
        , getAndViewLoadout model "Titan"
        , vDivider
        , getAndViewLoadout model "Warlock"
        ]

viewCustomItems : Model -> Element Msg
viewCustomItems model =
    wrappedRow
        [ width <| minimum 400 <| fill
        , padding 10
        ]
        [ text "Custom Items Not Yet Implemented." ]

viewFooter : Model -> Element Msg
viewFooter model =
    row
        [ Font.size <| smallTextSize model
        , spacing 10
        , padding 7
        , centerX
        , alignBottom
        ]
        [ link
            []
            { url = "https://github.com/Drowrin/LightBalance"
            , label = text "Source Code"
            }
        , text "|"
        , Input.button
            [ focused [] ]
            { onPress = Just <| AboutPressed True
            , label = text "What is this?"
            }
        ]

viewAbout : Model -> Element Msg
viewAbout model =
    el
        [ width fill
        , height fill
        , padding 10
        , inFront <|
            Input.button
                [ alignTop
                , alignRight
                , padding 5
                , focused []
                , Font.bold
                ]
                { onPress = Just <| AboutPressed False
                , label = text "X"
                }
        ]
        <| textColumn
            [ centerX
            , centerY
            , width <| maximum 610 <| fill
            , spacing 10
            ]
            [ paragraph
                []
                [ el
                    [ alignLeft, padding 8, Font.bold, Font.size <| bigTextSize model ]
                    <| text "LightBalance"
                , text "This page calculates when you should balance your light. Items with a "
                , el [ Font.color yesColor, Font.bold ] <| text "green"
                , text " border should be replaced by blues/vendor drops. Characters with a "
                , el [ Font.color yesColor, Font.bold ] <| text "green "
                , text <| 
                    """
                    label have at least one item that can be replaced in order to increase that character's
                    average light.
                    """
                ]
            , paragraph
                []
                [ text <|
                    """
                    When an upgrade is possible for each character, the post-balance light
                    level will be displayed in comparison with the current light level.
                    """
                ]
            , paragraph
                []
                [ text <|
                    """
                    Custom items can be added, and will be included in the calculation.
                    This is intended for manually adding classified items that do not show up in the API,
                    but it can also be used to see what your average light could be if you get specific
                    drops.
                    """
                ]
            , paragraph
                []
                [ text <|
                    """
                    This tool requires you to log in through Bungie.net in order to function.
                    The only information that the tool accesses is your inventory as well as public profile
                    data such as username and profile picture. The source code is available through the link
                    at the bottom of the page.
                    """
                ]
            , paragraph
                []
                [ text <|
                    """
                    Data is retrieved once per minute by default. It can be forced to refresh by pressing
                    the refresh button in the upper right corner.
                    """
                ]
            ]

view : Model -> Html Msg
view model =
    let
        isPortrait = model.h > model.w || model.w < 1020
    in
    Element.layout
    [ width fill
    , height fill
    , Font.size <| textSize model
    , Font.color txtColor
    , Background.color bgColor
    ]
    <| column
        [ width fill
        , height fill
        ]
        [ viewHeader model

        , hDivider

        , el [ scrollbarY ]
        <| if model.viewAbout
            then viewAbout model
            else if isPortrait
                then column
                    [ height fill
                    , width fill
                    ]
                    [ el [ scrollbarX ] <| viewMain model
                    , hDivider
                    , viewCustomItems model
                    ]
                else row
                    [ height fill
                    , width fill
                    ]
                    [ viewMain model
                    , vDivider
                    , viewCustomItems model
                    ]
        
        , viewFooter model
        ]
