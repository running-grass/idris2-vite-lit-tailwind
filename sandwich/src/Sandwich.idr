module Sandwich

import Data.String

import public Sandwich.Util
import public Sandwich.VDom
import public Sandwich.HTMLElement

%default total

private
attrToStr : Attribute -> String
attrToStr (k,v) = "\{k}=\"\{v}\""

private
attrs : Attributes ->  String
attrs props = joinBy " " . map attrToStr $ props

public export
toHtml : VNode -> Html
toHtml (Text text) = escape text
toHtml (Element tagName props childrens) = "<\{tagName} \{attrs props} >\{childrenHtml childrens ""}</\{tagName}>"
  where
  childrenHtml : List VNode ->  Html -> Html
  childrenHtml [] prev = prev
  childrenHtml (v :: vs) prev = childrenHtml vs (prev ++ toHtml v)

public export
update : HasIO io => (id: String) -> VNode -> io ()
update id node = do
  log' "start update"
  el <- getElementById' id
  log' el
  case el of
    Just el' => setInnerHTML el' $ toHtml node
    _ => pure ()
  log' "end update"