-- Given a platform, time, title, and message, create a command to be run in a terminal
-- that will create an appropriate timer and deliver an appropriate message.alias


module Main exposing (main)

import Browser
import Html
import Html.Attributes exposing (attribute, checked, class, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            { platform = Mac
            , timer = "30m"
            , title = "Ding!"
            , msg = "Take a break; stretch; change position if you have to."
            }
        , update = update
        , view = view
        }


type Platform
    = Linux
    | Mac
    | Windows


type alias Model =
    { platform : Platform
    , timer : String
    , title : String
    , msg : String
    }


type Msg
    = SetPlatform Platform
    | SetTimer String
    | SetTitle String
    | SetMsg String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetPlatform p ->
            { model | platform = p }

        SetTimer t ->
            { model | timer = t }

        SetTitle t ->
            { model | title = t }

        SetMsg m ->
            { model | msg = m }


view : Model -> Html.Html Msg
view m =
    Html.div
        []
        [ controls m
        , output m
        ]


controls : Model -> Html.Html Msg
controls m =
    Html.div
        [ id "timer-cmd" ]
        [ selectPlatform m
        , textbox "Time" m.timer SetTimer
        , textbox "Title" m.title SetTitle
        , textareabox "Message" m.msg SetMsg
        ]


textbox : String -> String -> (String -> msg) -> Html.Html msg
textbox label content msg =
    Html.p []
        [ Html.label [] [ Html.text label ]
        , Html.input [ type_ "text", value content, onInput msg ] []
        ]


textareabox : String -> String -> (String -> msg) -> Html.Html msg
textareabox label content msg =
    Html.p []
        [ Html.label [] [ Html.text label ]
        , Html.textarea [ value content, onInput msg ] []
        ]


selectPlatform : Model -> Html.Html Msg
selectPlatform m =
    Html.p
        [ class "platform-select" ]
        (List.map
            (platformRadioInput m)
            [ Linux, Mac, Windows ]
        )


platformRadioInput : Model -> Platform -> Html.Html Msg
platformRadioInput m p =
    let
        setPlatform pfm =
            Html.Events.on "change" (Decode.succeed <| SetPlatform pfm)

        label =
            Html.text <|
                case p of
                    Linux ->
                        "Linux"

                    Mac ->
                        "MacOS"

                    Windows ->
                        "Windows"
    in
    Html.span [ setPlatform p, onClick <| SetPlatform p ]
        [ Html.input [ type_ "radio", checked <| p == m.platform ] []
        , label
        ]


output : Model -> Html.Html Msg
output m =
    Html.p
        [ id "cmd-display" ]
        [ Html.node "clipboard-copy" [ attribute "for" "cmd" ] [ Html.button [] [ Html.text "Copy" ] ]
        , Html.pre [ id "cmd" ] [ Html.text <| command m ]
        ]


command : Model -> String
command m =
    case m.platform of
        Linux ->
            "unimplemented"

        Windows ->
            "unimplemented"

        Mac ->
            String.concat
                [ "cdown "
                , m.timer
                , " && osascript -e 'display alert \""
                , m.title
                , "\" message \""
                , m.msg
                , "\"'"
                ]
