module Main exposing (..)

import Browser
import Time
import Task
import Element as E
import Element.Background as Background
import Element.Font as Font
import Http
import Json.Decode as D

-- (seconds, destination)
type alias Entry = (Int, String)

type Model = Loading
           | Failure String
           | Data
            { time: Time.Posix
            , entries: (List Entry)
            }

type Msg = Update Time.Posix
         | Entries (Result Http.Error (List Entry))

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = (\_ -> Time.every (1000) Update)
  }

init : () -> (Model, Cmd Msg)
init _ = (Loading, fetch)

fetch : Cmd Msg
fetch = Http.get
  { url = query_url
  , expect = Http.expectJson Entries entries_decoder
  }

entries_decoder : D.Decoder (List Entry)
entries_decoder = D.list entry_decoder

make_entry : Int -> String -> Entry
make_entry a b = (a, b)

entry_decoder : D.Decoder Entry
entry_decoder = D.map2 make_entry (D.field "timeToStation" D.int) (D.field "towards" D.string)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Update t ->
    let doFetch = remainderBy 30 (Time.toSecond Time.utc t) == 0
        cmd = if doFetch then fetch else Cmd.none
    in case model of
      Loading -> (Loading, cmd)
      Failure e -> (Failure e, cmd)
      Data d -> (Data { d | time = t }, cmd)
  Entries (Err e) -> (Failure (show_fail e), Cmd.none)
  Entries (Ok entries) ->
    let
      time = case model of
        Data d -> d.time
        _ -> Time.millisToPosix 0
    in
      (Data
        { entries = List.sort entries
        , time = time
        }
      , Cmd.none)

show_fail : Http.Error -> String
show_fail e = case e of
  Http.BadUrl u -> "bad url: " ++ u
  Http.Timeout -> "timeout"
  Http.NetworkError -> "network error"
  Http.BadStatus x -> "status " ++ (String.fromInt x)
  Http.BadBody x -> "bad body:\n" ++ x

--view : Model -> Html.Html msg
view m = E.layout [E.behindContent (E.image [E.centerX, E.centerY] { src = "https://tolziplohu.github.io/tube_station_view/station.png", description = "" }), E.width E.fill, E.height E.fill, E.centerX, E.centerY] <| case m of
  Loading -> E.text "Loading"
  Failure e -> E.text ("Failed because of " ++ e)
  Data d -> E.column
    [ E.height (E.px 10)
    , E.centerX
    , E.centerY
    , E.spacing 1
    , E.paddingEach
      { top = 0
      , right = 70
      , bottom = 550
      , left = 0
      }
    , Font.color (E.rgb255 231 226 72)
    , Font.size 15
    ] [something d.entries, view_time d.time]

edges =
  { top = 0
  , right = 0
  , bottom = 0
  , left = 0
  }

view_time : Time.Posix -> E.Element msg
view_time time =
  let
    hours = String.fromInt <| Time.toHour zone time
    minutes = String.fromInt <| Time.toMinute zone time
    seconds = String.fromInt <| Time.toSecond zone time
  in
    -- HH:MM:SS
    E.el [E.centerX, E.centerY, E.paddingEach { edges | right = 12 }] <| E.text (pad hours ++ ":" ++ pad minutes ++ ":" ++ pad seconds)

-- Makes numbers two-digit; I don't think there's a nice way to pad the left with zeroes
pad : String -> String
pad s = if String.length s == 1 then "0" ++ s else s

-- London time zone
zone = Time.customZone (1 * 60) []

something : List Entry -> E.Element msg
something m =
  m |> List.take 3 |> List.indexedMap entry |> (\l -> l ++ List.repeat (3 - List.length l) (E.text " ")) |> E.column [E.height E.fill, E.centerX]

entry : Int -> Entry -> E.Element msg
entry num (time, dest) = E.row [E.width (E.px 280), E.centerY, E.spacing 20] [E.text (String.fromInt num), E.el [E.alignLeft] <| E.text dest, E.el [E.alignRight] <| E.text (String.fromInt (time // 60) ++ " mins")]

-- Ravenscourt Park as an example; TODO let user switch
stop_id = "940GZZLUGPK"

query_url = "https://api.tfl.gov.uk/StopPoint/" ++ stop_id ++ "/Arrivals"
