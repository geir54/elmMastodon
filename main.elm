import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required)
import Time exposing (Time, second)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { toot : String
  , accessToken : String
  , auth : Auth
  , toots : List Toot
  , hometoots : List Toot
  , lastTootID : Int -- The newest toot we have seen
  , lastHomeTootID : Int -- The newest toot we have seen
  , httpRequest : Int -- Keep track of which request to send
  }

type alias Auth = {
  username : String
  , password : String
}

init : (Model, Cmd Msg)
init =
  ( Model "" "" (Auth "" "") [] [] 0 0 0,  Cmd.none)

-- UPDATE


type Msg
  = Login
  | SendToot
  | NewAccessToken (Result Http.Error String)
  | NewToot (Result Http.Error Toot)
  | NewToots (Result Http.Error (List Toot))
  | NewHomeToots (Result Http.Error (List Toot))
  | InputToot String
  | InputUsername String
  | InputPassword String
  | Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Login ->
      (model, getAccessToken "2d4c9a654e98f3110c31a9fbacaa5912110d2fa85222097085f09cc08a28ee3d" "5961016d56a52e98b52a9688d1b5ad3d05ba9435b989fc404afab27d034afadc" model.auth.username model.auth.password "read write follow")

    SendToot ->
      (model, sendToot model.toot model.accessToken)

    InputToot text ->
      ({model | toot = text}, Cmd.none)

    InputUsername text ->
      ({model | auth = (Auth text model.auth.password)}, Cmd.none)

    InputPassword text ->
      ({model | auth =  (Auth model.auth.username text)}, Cmd.none)

    NewAccessToken (Ok accesstoken) ->
      ({model | accessToken = accesstoken}, Cmd.none)

    NewAccessToken (Err _) ->
      (model, Cmd.none)

    NewToot (Ok str) -> -- TODO: Do something better here
      (model, Cmd.none)

    NewToot (Err _) ->
      (model, Cmd.none)

    NewToots (Ok toots) ->
      ({model | toots = (List.append toots model.toots), lastTootID = (getFirstID model.lastTootID toots)}, Cmd.none)

    NewToots (Err _) ->
      (model, Cmd.none)

    NewHomeToots (Ok toots) ->
      ({model | hometoots = (List.append toots model.hometoots), lastHomeTootID = (getFirstID model.lastHomeTootID toots)}, Cmd.none)

    NewHomeToots (Err _) ->
      (model, Cmd.none)

    -- Kind of a hack, but it works
    Tick newTime ->
      let
        cmd =
          case model.httpRequest of
            0 ->
              getTimeline model.lastTootID model.accessToken Public "" NewToots
            1 ->
              getTimeline model.lastHomeTootID model.accessToken Home "" NewHomeToots
            _ ->
              Cmd.none

        newhttpRequest =
          if model.httpRequest == 1 then
            0
          else
            model.httpRequest+1

      in
        ({model| httpRequest = newhttpRequest}, (cmd))



-- Get first element from list
getFirstID : Int -> List Toot -> Int
getFirstID lastID toots =
  let
    res = List.head toots
  in
    case res of
      Nothing ->
        lastID

      Just toot ->
        toot.id


-- VIEW


view : Model -> Html Msg
view model =
  if String.length model.accessToken > 0 then
    viewMain model
  else
    -- Show login
    div []
    [
      text "Username "
      , input [onInput InputUsername] []
      , br [] []
      , text "Password "
      , input [onInput InputPassword] []
      , br [] []
      , button [ onClick Login ] [ text "Login!" ]
    ]

viewMain : Model -> Html Msg
viewMain model =
  div []
    [ input [onInput InputToot] []
    , button [ onClick SendToot ] [ text "Toot!" ]
    , br [] []
    , viewColumn "home" model.hometoots
    , viewColumn "public" model.toots
    ]

viewColumn : String -> List Toot -> Html Msg
viewColumn name toots =
    div [
      style [("width", "30%"), ("color", "white"), ("float", "left"), ("padding", "0px 2px 0px 0px")]
    ] [
      div [
        style [("backgroundColor", "rgb(54, 60, 70)")]
      ] [ strong [] [text name]]
      , div [] (List.map viewToots toots)
    ]


viewToots toot =
  div [
     style
        [ ("backgroundColor", "rgb(47, 52, 65)")
        , ("min-height", "55px")
        , ("padding", "8px 10px 8px 8px")
        , ("border-bottom", "1px solid rgb(54,60,70)")
        ]
  ] [
    strong [] [text toot.account.displayname]
    , text (" @"++toot.account.username)
    , br [] []
    , rawHTML toot.content
  ]

rawHTML : String -> Html Msg
rawHTML html =
  div [ property  "innerHTML" <|  Encode.string html  ] []


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (15*second) Tick



-- HTTP

getAccessToken : String -> String -> String -> String -> String -> Cmd Msg
getAccessToken clientID clientSecret username password scope =
  let
      body =
        multipartBody [
        stringPart "client_id" clientID
        , stringPart "client_secret" clientSecret
        , stringPart "grant_type" "password"
        , stringPart "username" username
        , stringPart "password" password
        , stringPart "scope" scope
        ]
  in
      Http.send NewAccessToken (Http.post "https://mastodon.social/oauth/token" body decodeAccessToken)

decodeAccessToken : Decode.Decoder String
decodeAccessToken =
  Decode.at ["access_token"] Decode.string




--  Tooting ******************

sendToot : String -> String -> Cmd Msg
sendToot tootText accessToken =
  let
    body =
      multipartBody [
        stringPart "status" tootText
      ]

    req =
      request
        { method = "POST"
        , headers = [Http.header "Authorization" ("Bearer " ++ accessToken)]
        , url = "https://mastodon.social/api/v1/statuses"
        , body = body
        , expect = expectJson decodeToot
        , timeout = Nothing
        , withCredentials = False
        }
  in
    Http.send NewToot req

type alias Toot = {
  id : Int
  , content : String
  , account : Account
}

type alias Account = {
  username : String
  , displayname : String
}

decodeToot : Decode.Decoder Toot
decodeToot =
  decode Toot
    |> Json.Decode.Pipeline.required "id" Decode.int
    |> Json.Decode.Pipeline.required "content" Decode.string
    |> Json.Decode.Pipeline.required "account" accountDecoder

accountDecoder : Decode.Decoder Account
accountDecoder =
  decode Account
    |> Json.Decode.Pipeline.required "username" Decode.string
    |> Json.Decode.Pipeline.required "display_name" Decode.string


--  Timeline ******************

type Timeline
  = Home
  | Mentions
  | Public
  | Tag

getTimeline : Int -> String -> Timeline -> String -> (Result Error (List Toot) -> Msg ) -> Cmd Msg
getTimeline sinceId accessToken timeline tag action =
  let
    timelineStr =
      case timeline of
        Home ->
          "home"
        Mentions ->
          "mentions"
        Public ->
          "public"
        Tag ->
          "tag" ++ "/" ++ tag

    url =   "https://mastodon.social/api/v1/timelines/" ++ timelineStr ++ "?since_id=" ++ toString(sinceId)

    req =
      request
        { method = "GET"
        , headers = [Http.header "Authorization" ("Bearer " ++ accessToken)]
        , url = url
        , body = emptyBody
        , expect = expectJson decodeTootList
        , timeout = Nothing
        , withCredentials = False
        }

  in
    Http.send action req

decodeTootList : Decode.Decoder (List Toot)
decodeTootList =
  Decode.list decodeToot
