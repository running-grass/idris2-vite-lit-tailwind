
infixl 9 :=:

||| Description of a record row with key and type
public export
data Row : String -> Type -> Type where
  (:=:) : (s : String) -> (t : Type) -> Row s t

export
Show (Row r t) where
  show (s :=: t) = "\{s} :=: type"

public export
data Record : Type where
  Nil : Record
  (::) : (Row key type) -> Record -> Record

printList : Record -> List String
printList [] = []
printList (x :: y) = show x :: printList y

export
Show Record where
  show rec = show (printList rec)

public export
CommonProps : Record
CommonProps = [ "id" :=: String, "className" :=: String ]

public export
ImageProps : Record
ImageProps = "src" :=: String :: CommonProps


HasKey : String -> Record -> Bool
HasKey _ Nil = False
HasKey k ((fst :=: _) :: xs) = if fst == k then True else HasKey k xs

getSrc : (rec : Record) -> {  _ : HasKey "src" rec = True } -> String
getSrc rec = "hello"

record Ele where
    constructor MkEle
    tagName : String
    props : List String
    childrens : List Ele
