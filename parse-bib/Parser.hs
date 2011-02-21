module Parser where

    --import Parsers etc...
    import Text.ParserCombinators.UU
    import Text.ParserCombinators.UU.BasicInstances
    import Text.ParserCombinators.UU.Examples

    import ParserUtils
    import Common.Types

    parseBib = BibTex `pMerge` (pMany parseBibEntry)

    parseBibEntry = Entry `pMerge` (pOne pType
                       <||> pOne pReference
                       <||> pOne pBibEntryBody
                            )

    pBibEntryBody :: Parser [Field]
    pBibEntryBody = pList1Sep (pSym ',') (pKeyValue) <* pSym '}' <* spaces
    

    pBibKey :: Parser String
    pBibKey  = pMunch (flip elem (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

    pKeyValue :: Parser Field
    pKeyValue = Field <$> (spaces *> pMunch1 (flip elem ['a'..'z'])) <* (spaces *> pSym '=' *> spaces *> pSym '"') <*> (pMunch1 (/= '"') <* pSym '"' )



    pReference = pSym '{' *> pBibKey <* pToken "," <* spaces

    pType :: Parser String
    pType = (pSym '@' *> pMunch1 (flip elem ['a'..'z']))


    pAuthor :: Parser String
    pAuthor = pMunch1 (/= '\n') -- TODO FIX

