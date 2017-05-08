port module App exposing (..)
--importing html components & exposing then which are needed for the Web application
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

--importing Json package and exposing it
import Json.Decode as Decode exposing (..)


--importing Regex to implement form validation using pattern matching
import Regex



 
--Defining the basic MODEL structure 
--thts assigned for each entity

type alias Model =
    { username : String
    , password : String
    , email : String
    , phno : String
    , role : String
    , tags :String
    , errorMsg : String
    , screen : String
    , checkout : String
}

init : Model
init = Model "" "" "" "" "" "" "" "1" ""


checkUser : List Model -> String -> String -> Bool
checkUser models name pass =
    case models of
        [] -> False
        (x::xs) -> 
            if x.username == name && x.password == pass
            then True
            else checkUser xs name pass

getUser : List Model -> String -> Model
getUser models name =
    case models of
        [] -> Model "" "" "" "" "" "" "" "1" ""
        (x::xs) -> 
            if x.username == name
            then x
            else getUser xs name

--appendUser : Model -> String
--appendUser model = 
--    model::userList
--pattern to match any name accepting any name with valid alphabets only
patternName = Regex.regex "[A-Za-z_ ]+" 
--pattern to match any phone number to accept valid 12 digit phone number only
patternPhno = Regex.regex "[+][0-9]{12}" 
--pattern to match any tags to accept valid courses
patternTags = Regex.regex "[A-Za-z0-9_, ]+" 
--pattern to match any Email address to accept valid email only
patternEmail = Regex.regex "[A-Za-z0-9_]+@[A-Za-z0-9-]+[.][A-Za-z0-9-]+"

--validate user input entered based on regular expression matching
validateUser : Model -> Bool
validateUser model = 
    if Regex.contains patternName model.username &&
         Regex.contains patternPhno model.phno &&
            Regex.contains patternTags model.tags &&
                Regex.contains patternEmail model.email
    then True
    else False


-- Messages 


type Msg = Getuser String | RegisterUserPage | ClickLogIn | LogOut| ClickUser String| Home| LogInUserPage| ClickRegister| SetPhNo String| SetTags String| SetEmail String| SetRole String| Getpasskey String


-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Getuser username ->
            { model | username = username }

        Getpasskey password ->
            { model | password = password }

        RegisterUserPage ->
            { model | errorMsg = "", screen = "0" }

        ClickUser name ->
            {  model | checkout = (getUser userList name).username, screen = "3" }

        ClickLogIn ->
            if checkUser userList model.username model.password
            then { model | errorMsg = "", screen = "2" }
            else { model | errorMsg = "Invalid Username or Password", screen = "1" }
    
        LogOut ->
            { model | username = "", password = "", screen = "1" }

        Home -> 
            { model | screen = "2" }

        ClickRegister -> 
            if checkUser userList model.username model.password
            then { model | errorMsg = "Username taken.", screen = "0" }
            else if validateUser model
            then { model | errorMsg = "", screen = "2" }
            else { model | errorMsg = "Please input valid information", screen = "0" }

        LogInUserPage ->
            { model | screen = "1" }

        SetPhNo phno ->
            { model | phno = phno }

        SetTags tags ->
            { model | tags = tags }

        SetEmail val ->
            { model | email = val }

        SetRole val ->
            { model | role = val }



{-
VIEW
* Hide sections of view depending on authenticaton state of model
* Get a quote
* Log In or Register
-}


--Which screen to display
type Screens = One | Two | Three

view : Model -> Html Msg
view model = 
    case model.screen of
        "0" -> signinScreen model
        "1" -> loginScreen model
        "2" -> scanareaScreen model
        "3" -> contactScreen model
        _ -> Debug.crash "Help"




loginScreen : Model -> Html Msg
loginScreen model = 
    let
        showError : String
        showError =
            if String.isEmpty model.errorMsg then
                "hidden"
            else
                ""
    in
        div [ class "container-mod container" ]
            [ header
            , div [ class "row text-center" ] [
                    button [ class "btn btn-success col-xs-5", onClick ClickLogIn ] [ text "Log In" ]
                    , h5 [class "col-xs-2" ] [text "OR"]
                    , button [ class "btn btn-link col-xs-5", onClick RegisterUserPage ] [ text "Register" ]
                    ]
              ,div [ class "text-left" ]
                    [ -- Login/Register form or user greeting
                    div [ id "form" ]
                    [ h2 [ class "text-center" ] [ text "Please Login or Signup to continue" ]
                    , div [ class showError ]
                        [ div [ class "alert alert-danger text-center" ] [ text model.errorMsg ]
                        ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "username" ] [ text "Username:" ]
                    , input [ id "username", type_ "text", class "form-control", onInput Getuser ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "password" ] [ text "Password:" ]
                    , input [ id "password", type_ "password", class "form-control", onInput Getpasskey ] []
                    ]
                    ]
                ,hr [] []
                , div [ class "row text-center" ] [
                    button [ class "btn btn-success col-xs-5 col-xs-offset-3", onClick ClickLogIn ] [ text "Next" ]
                    ]
                    
                    ]]]

signinScreen : Model -> Html Msg
signinScreen model = 
    let
        showError : String
        showError =
            if String.isEmpty model.errorMsg then
                "hidden"
            else
                ""
    in
        div [ class "container-mod container" ]
            [ header  
            , div [ class "row text-center" ] [
                    button [ class "btn btn-link col-xs-5", onClick LogInUserPage ] [ text "Log In" ]
                    , h5 [class "col-xs-2" ] [text "OR"]
                    , button [ class "btn btn-success col-xs-5", onClick ClickRegister ] [ text "Register" ]
                    ]
            , div [ class "text-left" ]
                    [ -- Login/Register form or user greeting
                    div [ id "form" ]
                    [ h2 [ class "text-center" ] [ text "Sign Up" ]
                    , p [ class "text-center help-block" ] [ text "to scan for Teacher or Student" ]
                    , div [ class showError ]
                        [ div [ class "alert alert-danger text-center" ] [ text model.errorMsg ]
                        ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "username" ] [ text "Username:" ]
                    , input [ id "username", type_ "text", class "form-control", Html.Attributes.value model.username, onInput Getuser ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "password" ] [ text "Password:" ]
                    , input [ id "password", type_ "password", class "form-control", Html.Attributes.value model.password, onInput Getpasskey ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "phno" ] [ text "Ph. No.:" ]
                    , input [ id "phno", type_ "text", class "form-control", Html.Attributes.value model.phno, onInput SetPhNo ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "email" ] [ text "Email:" ]
                    , input [ id "email", type_ "text", class "form-control", Html.Attributes.value model.email, onInput SetEmail ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "role" ] [ text "Role:   " ]
                    , input [ id "role", type_ "radio", class "", name "role", Html.Attributes.value "Teacher", checked True] []
                    , text " Teacher  "
                    , input [ id "role", type_ "radio", class "", name "role", Html.Attributes.value "Student" ] []
                    , text " Student  "
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "tags" ] [ text "Keywords:" ]
                    , input [ id "tags", type_ "text", class "form-control", Html.Attributes.value model.tags, onInput SetTags ] []
                    ]
                    ]
                    ,hr [] []
                    , div [ class "row text-center" ] [
                    button [ class "btn btn-warning col-xs-5 col-xs-offset-3", onClick ClickRegister ] [ text "Next" ]
                    ]
                    ]]]

mapWrapper : List (Attribute a) -> List (Html a) -> Html a
mapWrapper =
    Html.node "map-wrapper"


scanareaScreen : Model -> Html Msg
scanareaScreen model = 
    let 
        loggedUser = model.username
    in
    div [ class "container" ] 
        [ header
        , div [ class "row"] [ navbar model.username ]
        
            , div [ class "row  col-xs-12 col-md-12" ]
                [ div [ class "col-xs-6 col-md-6 col-md-offset-4"] 
                    [ div  [class "col-xs-5 col-md-5"] [ h5 [] [ text "Select user to Request Meetup" ] ]
                     , div [class "list-group", style [("max-width", "400px")]] 
                        [ a [class "list-group-item list-group-item-action list-group-item-success align-items-start", onClick (ClickUser "Vinay")] 
                            [ span [class "badge"] [h4 [] [ text "Teacher"]]
                            , h4 [] [ text "Vinay " ]
                            , p [] [ text "Courses : Java, C, C++"]
                            ]
                        , a [class "list-group-item list-group-item-action list-group-item-info align-items-start", onClick (ClickUser "Jeorge") ] 
                            [ span [class "badge"] [h4 [] [ text "Student"]]
                            , h4 [] [ text "Jeorge" ]
                            , p [] [ text "Courses : Java"]
                            ]
                        , a [class "list-group-item list-group-item-action list-group-item-success align-items-start", onClick (ClickUser "Hussain") ] 
                            [ span [class "badge"] [h4 [] [ text "Teacher"]]
                            , h4 [] [ text "Hussain " ]
                            , p [] [ text "Interested in : Java"]
                            ]
                        , a [class "list-group-item list-group-item-action list-group-item-info align-items-start", onClick (ClickUser "Javed") ] 
                            [ span [class "badge"] [h4 [] [ text "Teacher"]]
                            , h4 [] [ text "Javed " ]
                            , p [] [ text "Courses : Python"]
                            ]
                        ,a [class "list-group-item list-group-item-action list-group-item-info align-items-start", onClick (ClickUser "john") ] 
                            [ span [class "badge"] [h4 [] [ text "Student"]]
                            , h4 [] [ text "john" ]
                            , p [] [ text "Courses : Java"]
                            ]
                    
                    ]
                ]
                , div [ class "col-xs-12 col-md-12"] 
                    [  mapWrapper
                        [ attribute "latitude" "12.942149"
                        , attribute "longitude" "77.622002"
                        --, attribute "drag-events" "true"
                        ]
                        []
                    --p [] [text "Hello world"] 
                    ]
            ]
           ,hr [] [] 
         ,div [class "text-center"][button [ class "btn btn-danger text-center col-md-offset-2 col-md-5", onClick LogOut ] [ text "Logout" ]]
        ]

--onChange : Msg -> Msg
--onChange msg = 
--    msg

contactScreen : Model -> Html Msg
contactScreen model = 
    let 
        member = 
            getUser userList model.checkout

        showteacher = 
            if member.role == "teacher"
            then ""
            else "hidden"

        showstudent = 
            if member.role == "student"
            then ""
            else "hidden"
    in
    div [ class "container" ] 
        [ header
        , div [ class "row"] [ navbar model.username ]
        , div [ class "row"]
            [ h2 [ class ""] [ text model.checkout ] 
            , div [ class " jumbotronTransp jumbotron  row" ]
                [ div [class "col-md-6"] 
                    [ div [ class "text-left" ] 
                        [   table [ style [("cell-padding", "10px")]] 
                                [   tr [] 
                                        [ td [class showteacher] [ h4 [] [text "Teaches : " ]]
                                        , td [class showstudent] [ h4 [] [text "Interested In : "]]
                                        , td [] [ h5 [] [text member.tags] ]
                                        ]
                                ,   tr []
                                        [   td [] [ h4 [] [ text "Contact : "]  ] 
                                        , td [] [ h4 [] [ a[href ("whatsapp://send?text=Hi there!&phone=" ++ (toString member.phno)), class "btn btn-link"] [ text "Ping me on whatsapp!!" ] ]]
                                        ] 
                                , tr []
                                    [ td [] [ h4 [] [text "Email : "] ]
                                    , td [] [ a [href ("mailto:" ++ member.email), class "btn btn-link"] [text member.email]] ]   
                                ]
                        ]
                    ]
            ]
            , div [ class "col-md-12" ] 
                [ mapWrapper
                    [ attribute "latitude" "12.942149"
                    , attribute "longitude" "77.622002"
                    --, attribute "drag-events" "true"
                    ]
                    []
                ]
            ]
            ,hr [] []
             ,button [ class "btn btn-danger text-center col-md-offset-2 col-md-5", onClick LogOut ] [ text "Logout" ]
        ] 





navbar : String -> Html Msg
navbar name = 
    nav [ class "navbar" ] 
        [ div [ class "container-fluid" ]
            [ div [ class "navbar-header navbar-right" ]
                [ a [ class "navbar-brand " ]
                    [ text ("Hey " ++ name) ]
                ]
            , ul [ class "nav navbar-nav"] 
                [ li [] [ a [onClick Home] [ text "Scan Area" ] ]
                ]
            ]
        ]

header : Html Msg
header = 
    div [ class "row text-center" ] 
        [ div [ class "jumbotronTransp jumbotron" ] [ h1 [] [ text "Venture City Hackathon" ] ]
        ,hr [] []
        ]


-- Array of users
vinay_data_model = Model "vinay" "1234" "vinay@gmail.com" "+917035559810" "teacher" "java, c, c++" "" "1" ""
chuck_data_model = Model "chuck" "itschuck" "chuck@gmail.com" "+919880606293" "student" "java" "" "1" ""
robb_data_model = Model "robb" "itsrobb" "robb@gmail.com" "+918788882325" "teacher" "javascript, html" "" "1" ""
praveen_data_model = Model "praveen" "itspraveen" "praveen122@gmail.com" "+918892585434" "teacher" "python" "" "1" ""
john_data_model = Model "john" "itsjohn" "john55@gmail.com" "+918892585434" "teacher" "python" "" "1" ""
sachin_data_model = Model "sachin" "itssachin" "sachin48@gmail.com" "+918892585434" "student" "python" "" "1" ""

userList = [vinay_data_model, chuck_data_model, robb_data_model, praveen_data_model,john_data_model, sachin_data_model ]



main : Program Never Model Msg
main =
    Html.beginnerProgram 
    { model = init, view = view, update = update }