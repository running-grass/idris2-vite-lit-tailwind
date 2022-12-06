module Sandwich.Util

import Data.String
import Sandwich.VDom
import Sandwich.HTMLElement

%default total

-- TODO 这里的实现应该有问题
export
%foreign "browser:lambda: (x, a) => { console.log(a); return a;}"
prim__log : a -> PrimIO a

public export
log : HasIO io => a -> io a
log a = primIO $ prim__log a

public export
log' : HasIO io => a -> io ()
log' = ignore . log


export
escape : String -> String
escape = fastConcat . map esc . unpack
  where esc : Char -> String
        esc '<'          = "&lt;"
        esc '>'          = "&gt;"
        esc '&'          = "&amp;"
        esc '"'          = "&quot;"
        esc '\''         = "&#x27"
        esc '\n'         = "\n"
        esc '\r'         = "\r"
        esc '\t'         = "\t"
        esc c            = if c < ' ' then "" else singleton c


%foreign "browser:lambda: (el, html)=> el.innerHTML = html"
prim__setInnerHTML : HTMLElement -> Html -> PrimIO ()

||| 设置内HTML
export
setInnerHTML : HasIO io => HTMLElement -> Html -> io ()
setInnerHTML el html= primIO $ prim__setInnerHTML el html

%foreign "browser:lambda: () => document"
prim__document : PrimIO HTMLDocument

||| document
export
document : HasIO io => io HTMLDocument
document = primIO prim__document

-- TODO 这里的实现应该有问题
%foreign "browser:lambda: (x, v) => (v === undefined || v === null) ? 1 : 0"
prim_isNull : a -> PrimIO Int

primMaybe : HasIO io => PrimIO a -> io (Maybe a)
primMaybe v = do 
  v' <- primIO v
  b <-  primIO $ prim_isNull v'
  case b of
    1 => pure Nothing
    _ => pure $ Just v'

%foreign "browser:lambda: (doc, id) => doc.getElementById(id)"
prim__getElementById : HTMLDocument -> String -> PrimIO HTMLElement

||| getElementById
export
getElementById : HasIO io => HTMLDocument -> String -> io (Maybe HTMLElement)
getElementById doc id = primMaybe $ prim__getElementById doc id 

export
getElementById' : HasIO io => String -> io (Maybe HTMLElement)
getElementById' id = document >>= (\doc => getElementById doc id)