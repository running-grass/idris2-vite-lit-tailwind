module Main

import Rhone.JS
import Data.Vect
import Rhone.JS
import Text.CSS
import Data.MSF.Event


%default total

public export
MSFEvent : Type -> Type
MSFEvent = Data.MSF.Event.Event

--------------------------------------------------------------------------------
--          Usefule Nodes
--------------------------------------------------------------------------------

export
lbl : (text: String) -> (class : String) -> Node ev
lbl txt cl = label [classes [cl]] [Text txt]
--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

||| Where the accumulated count is printed to
public export
out : ElemRef Div
out = Id Div "reset_out"

||| ID of the increasing button
public export
btnInc : ElemRef Button
btnInc = Id Button "reset_inc"

||| ID of the decreasing button
public export
btnDec : ElemRef Button
btnDec = Id Button "reset_dec"

||| ID of the reset button
public export
btnReset : ElemRef Button
btnReset = Id Button "reset_reset"

--------------------------------------------------------------------------------
--          CSS
--------------------------------------------------------------------------------

export
resetLbl : String
resetLbl = "reset_resetlbl"

export
incLbl : String
incLbl = "reset_inclbl"

export
decLbl : String
decLbl = "reset_declbl"

export
countLbl : String
countLbl = "reset_countlbl"

export
resetContent : String
resetContent = "reset_content"

export
resetBtn : String
resetBtn = "reset_incbtn"

public export
Ev : Type
Ev = Int8 -> Int8

btn :  (r : ElemRef Button)
    -> {auto 0 _ : ById r}
    -> Ev
    -> (lbl: String)
    -> String
    -> Node Ev
btn r ev lbl cls =
  button [ref r, onClick ev, class cls] [Text lbl]

content : Node Ev
content =
  div [ class resetContent ]
      [ lbl "Reset counter:"    resetLbl, btn btnReset (const 0) "Reset" "p-4 border"
      , lbl "Increase counter:" incLbl,   btn btnInc   (+ 1)     "+" "p-4 border"
      , lbl "Decrease counter:" decLbl,   btn btnDec   (+ (-1))  "-" "p-4 border"
      , lbl "Count:"            countLbl, div [ref out] []
      ]

public export
M : Type -> Type
M = DomIO Ev JSIO

msf : MSF M Ev ()
msf =
  accumulateWith apply 0 >>> fan_
    [ show     ^>> text out
    , (<= -10) ^>> disabledAt btnDec
    , (>=  10) ^>> disabledAt btnInc
    , (==   0) ^>> disabledAt btnReset
    ]

exampleDiv : ElemRef HTMLDivElement
exampleDiv = Id Div "example"


export
ui : M (MSF M Ev (), JSIO ())
ui = innerHtmlAt exampleDiv content $> (msf, pure ())

main : IO ()
main = runJS . ignore $ reactimateDomIni (const 0) "reset" ui

-- main = putStrLn "Hello from Idris110!"
