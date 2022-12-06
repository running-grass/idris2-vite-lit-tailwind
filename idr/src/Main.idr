module Main
import Sandwich
import Data.List.Quantifiers

%default total


divDemo : VNode
divDemo = div [id "i-am-id", class "className"] [element__ "x-index-page", span_ [text "i am span"]]


main : IO ()
main = do
  log' "start app"

  update "app" divDemo
