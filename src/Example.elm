module Example exposing (..)

import ArgSpec exposing (..)
import Debug
import String
--
optionA : ArgSpec
optionA = Option { short = Just 'a'
                 , long = Nothing
                 , name = "Option-A"
                 , description = Just "enables option number 1"
                 , arguments = Nothing }
--
optionB : ArgSpec
optionB = Option { short = Just 'b'
                 , long = Just "option-b"
                 , name = "Option-B"
                 , description = Just "enables option number b"
                 , arguments = Just <| Argument "bx" &&& Argument "by" }
--
optionC : ArgSpec
optionC = Option { short = Just 'c'
                 , long = Nothing
                 , name = "Option-C"
                 , description = Just "enable option C"
                 , arguments = Nothing }
--
optionD : ArgSpec
optionD = Option { short = Just 'd'
                 , long = Just "option-d"
                 , name = "Option-D"
                 , description = Just "enables option d"
                 , arguments = Just <| Command "get" &&& Argument "getCount"
                                   ||| Command "end" }
--
options : List ArgSpec
options = [optionA, optionB, optionC, optionD]
--
demoSpec : ArgSpec
demoSpec = Command "start" &&& Argument "port" &&& Argument "numPlayers"
           ||| Command "stop" &&& Optional [optionC, optionA, optionD]
               &&& ( Command "living" ||| Command "trying" ||| Command "reading" )
           ||| Command "options" &&& Optional options
           ||| Command "optional"
               &&& Optional [ Command "later"
                            , Command "dust" &&& Argument "dustMass" ]
               &&& Command "finished" &&& Argument "laserColor"
--
runDemos : List (Maybe ArgScan)
runDemos =
    let args = [ "start 88090 20"
               , "stop living"
               , "stop -ac --option-d get 80 trying"
               , "options -b 100 200"
               , "optional later dust 3.27 finished red"]
    in
        List.map (\ s -> Debug.log ("\n\n\"" ++ s ++ "\"\n") <|
                      scan demoSpec (String.split " " s))
            args
--
appSpec : ArgSpec
appSpec = Command "polling" &&& Argument "interval" &&& Argument "timeout"
          ||| Command "reset"
          ||| Command "once" &&& Argument "timeout"
---
rscan : Maybe ArgScan
rscan = scan appSpec ["polling", "100", "800ms"]
--
