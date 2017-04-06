import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import List
import Json.Encode exposing (encode, string)

main: Program Never Model Msg
main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

 -- MODEL
type alias Model =
  { chatMessage :  List String
  , userMessage : String
  , userName : String
  }

init : (Model, Cmd Msg)
init =
  ( Model [] "" ""
  , Cmd.none
  )

type alias ChatMessage =
  { command: String
  , content: String
  }

-- UPDATE
type Msg
  = PostChatMessage
  | UpdateUserMessage String
  | NewChatMessage String
  | SetUserName
  | UpdateUsername String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PostChatMessage ->
      let
        postmess =
          Json.Encode.object
            [("command", string "send")
            , ("content", string model.userMessage)
            ]
        jsonObj = encode 0 postmess
      in
        { model | userMessage = "" } ! [WebSocket.send "ws://localhost:3000/" jsonObj]

    UpdateUserMessage message ->
      { model | userMessage = message } ! []

    NewChatMessage message ->
        { model | chatMessage = (List.append model.chatMessage [message]) } ! []

    UpdateUsername username ->
        {model | userName = username} ! []

    SetUserName ->
      let
        userLogin =
          Json.Encode.object
            [("command", string "login")
             , ("content", string model.userName)
            ]
        jsonObj = encode 0 userLogin
      in
        {model | userName = model.userName } ! [WebSocket.send "ws://localhost:3000/" jsonObj]




-- VIEW
view : Model -> Html Msg
view model =

  div []
    [ input [placeholder "Username"
            , value model.userName
            , autofocus True
            , onInput UpdateUsername
            ] []
    ,  button [ onClick SetUserName ] [ text "Login"]
    , br []  []
    , div [] [text ("Your UserName is: " ++ model.userName)]
    , input [ placeholder "message..."
            , autofocus True
            , value model.userMessage
            , onInput UpdateUserMessage
            ] []
    , button [ onClick PostChatMessage ] [ text "Submit" ]
    , div []  (renderlist model.chatMessage)
  ]


renderlist: List String -> List (Html a)
renderlist lst =
      List.map (\l -> h4 [] [text l]) lst




 -- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://localhost:3000" NewChatMessage
