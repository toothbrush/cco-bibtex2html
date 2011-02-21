module Common.HtmlTypes where

    data Html = Html Head Body
                deriving Show
  
    data Head = Head Title
                deriving Show
  
    type Body = [BlockElem]
  
    data BlockElem = A Attr Ref 
                   | Hr
                   | Table Attr [Tr]
                   | P Attr String
                   deriving Show
  
    data Tr   = Tr Attr [Td]
                deriving Show
  
    type Td        = BlockElem
    type Title     = String
    
