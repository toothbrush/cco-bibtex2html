module Common.HtmlTypes where

    import Common.BibTypes

    data Html = Html Head Body
                deriving Show
  
    data Head = Head Title
                deriving Show
  
    type Body = [BlockElem]
  
    data BlockElem = A [Field] Reference
                   | Hr
                   | Table [Field] [Tr]
                   | P [Field] String
                   deriving Show
  
    data Tr   = Tr [Field] [Td]
                deriving Show
  
    type Td        = BlockElem
    type Title     = String
    
