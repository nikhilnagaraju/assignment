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

-- Array of users
vinay_data_model = Model "vinay" "1234" "vinay@gmail.com" "+91789654123" "teacher" "java, c, c++" "" "1" ""
user2 = Model "Jeorge" "1234" "jeorge@gmail.com" "+91789654123" "seeker" "java" "" "1" ""
user3 = Model "Hussain" "1234" "hussain@gmail.com" "+91789654123" "teacher" "javascript, html" "" "1" ""
user4 = Model "Javed" "1234" "javed@gmail.com" "+918892585434" "teacher" "python" "" "1" ""

userList = [vinay_data_model, user2, user3, user4]


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

patternName = Regex.regex "[a-zA-Z_ ]+" 

patternPhno = Regex.regex "[+][0-9]{12}" 

patternTags = Regex.regex "[a-zA-Z0-9_, ]+" 

patternEmail = Regex.regex "[a-zA-Z0-9_]+@[a-zA-Z0-9-]+[.][a-zA-Z0-9-]+"

--validate user input based on regex matching
validateUser : Model -> Bool
validateUser model = 
    if Regex.contains patternName model.username &&
         Regex.contains patternPhno model.phno &&
            Regex.contains patternTags model.tags &&
                Regex.contains patternEmail model.email
    then True
    else False


-- Messages 


type Msg = SetUsername String
            | SetPassword String
            | RegisterUserPage
            | ClickLogIn
            | LogOut
            | ClickUser String
            | Home
            | LogInUserPage
            | ClickRegister
            | SetPhNo String
            | SetTags String
            | SetEmail String
            | SetRole String


-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetUsername username ->
            { model | username = username }

        SetPassword password ->
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
            else { model | errorMsg = "Enter all details correctly.", screen = "0" }

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
        "0" -> signUpPage model
        "1" -> loginPage model
        "2" -> secondPage model
        "3" -> thirdPage model
        _ -> Debug.crash "Help"




loginPage : Model -> Html Msg
loginPage model = 
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
                    [ h2 [ class "text-center" ] [ text "Log In" ]
                    , p [ class "text-center help-block" ] [ text "Please Log In." ]
                    , div [ class showError ]
                        [ div [ class "alert alert-danger" ] [ text model.errorMsg ]
                        ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "username" ] [ text "Username:" ]
                    , input [ id "username", type_ "text", class "form-control", onInput SetUsername ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "password" ] [ text "Password:" ]
                    , input [ id "password", type_ "password", class "form-control", onInput SetPassword ] []
                    ]
                    ]
                , div [ class "row text-center" ] [
                    button [ class "btn btn-success col-xs-5 col-xs-offset-3", onClick ClickLogIn ] [ text "Go" ]
                    ]
                    
                    ]]]

signUpPage : Model -> Html Msg
signUpPage model = 
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
                    , p [ class "text-center help-block" ] [ text "Please Log In." ]
                    , div [ class showError ]
                        [ div [ class "alert alert-danger" ] [ text model.errorMsg ]
                        ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "username" ] [ text "Username:" ]
                    , input [ id "username", type_ "text", class "form-control", Html.Attributes.value model.username, onInput SetUsername ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "password" ] [ text "Password:" ]
                    , input [ id "password", type_ "password", class "form-control", Html.Attributes.value model.password, onInput SetPassword ] []
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
                    , input [ id "role", type_ "radio", class "", name "role", Html.Attributes.value "Seeker" ] []
                    , text " Seeker  "
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "tags" ] [ text "Keywords:" ]
                    , input [ id "tags", type_ "text", class "form-control", Html.Attributes.value model.tags, onInput SetTags ] []
                    ]
                    ]
                    , div [ class "row text-center" ] [
                    button [ class "btn btn-success col-xs-5 col-xs-offset-3", onClick ClickRegister ] [ text "Go" ]
                    ]
                    ]]]

mapWrapper : List (Attribute a) -> List (Html a) -> Html a
mapWrapper =
    Html.node "map-wrapper"


secondPage : Model -> Html Msg
secondPage model = 
    let 
        loggedUser = model.username
    in
    div [ class "container" ] 
        [ header
        , div [ class "row"] [ navbar model.username ]
        
            , div [ class "row  col-xs-12 col-md-12" ]
                [ div [ class "col-xs-6 col-md-6"] 
                    [ div [class "list-group", style [("max-width", "400px")]] 
                        [ a [class "list-group-item list-group-item-action list-group-item-success align-items-start", onClick (ClickUser "Vinay")] 
                            [ span [class "badge"] [h4 [] [ text "Teacher"]]
                            , h4 [] [ text "Vinay " ]
                            , p [] [ text "Courses : Java, C, C++"]
                            ]
                        , a [class "list-group-item list-group-item-action list-group-item-info align-items-start", onClick (ClickUser "Jeorge") ] 
                            [ span [class "badge"] [h4 [] [ text "Seeker"]]
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
                        ]
                    , div  [class "col-xs-5 col-md-5"] [ h5 [] [ text "This is a random text" ] ]
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
         ,div [class "text-center"][button [ class "btn btn-danger text-center col-md-offset-2 col-md-5", onClick LogOut ] [ text "Logout" ]]
        ]

--onChange : Msg -> Msg
--onChange msg = 
--    msg

thirdPage : Model -> Html Msg
thirdPage model = 
    let 
        member = 
            getUser userList model.checkout

        showteacher = 
            if member.role == "teacher"
            then ""
            else "hidden"

        showseeker = 
            if member.role == "seeker"
            then ""
            else "hidden"
    in
    div [ class "container" ] 
        [ header
        , div [ class "row"] [ navbar model.username ]
        , div [ class "row"]
            [ h2 [ class ""] [ text model.checkout ] 
            , div [ class "jumbotron row" ]
                [ div [class "col-md-6"] 
                    [ div [ class "text-left" ] 
                        [   table [ style [("cell-padding", "10px")]] 
                                [   tr [] 
                                        [ td [class showteacher] [ h4 [] [text "Teaches : " ]]
                                        , td [class showseeker] [ h4 [] [text "Interested In : "]]
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
        [ div [ class "jumbotron" ] [ h1 [] [ text "Venture City Hackathon" ] ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram 
    { model = init, view = view, update = update }