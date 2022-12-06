module Sandwich.VDom

%default total


public export
Html : Type
Html = String

public export
Attribute : Type
Attribute = (String, String)

public export 
Attributes : Type
Attributes = List Attribute

public export
TagName : Type
TagName = String

public export
data VNode: Type  where
  Element : (tagName : TagName) -> (attrs : Attributes) -> (childrens : List VNode) -> VNode
  Text : String -> VNode

public export
element : TagName -> Attributes -> List VNode -> VNode
element = Element

public export
element' : TagName -> Attributes -> VNode
element' tagName attrs = element tagName attrs []


public export
element_ : TagName -> List VNode -> VNode
element_ tagName = element tagName []

public export
element__ : TagName -> VNode
element__ tagName = element tagName [] []

public export
text : String -> VNode
text = Text

public export
span : Attributes -> List VNode -> VNode
span = element "span"

public export
span_ : List VNode -> VNode
span_ = span []

public export
div : Attributes -> List VNode -> VNode
div = element "div"

public export
div_ : List VNode -> VNode
div_ = div []


public export
id : String -> Attribute
id val = ("id", val)


public export
class : String -> Attribute
class val = ("class", val)
