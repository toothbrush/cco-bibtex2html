module ParseBib.Parser where

    --import Parsers etc...
    import Text.ParserCombinators.UU
    import Text.ParserCombinators.UU.BasicInstances
    import Text.ParserCombinators.UU.Examples

    import ParseBib.ParserUtils
    import Common.BibTypes
    import Char (isDigit)

    parseBib = BibTex `pMerge` (pSome parsePreamble
                        <||>    pMany parseBibEntry)

    parseBibEntry = Entry `pMerge` (pOne pType
                            <||>    pOne pReference
                            <||>    pOne pBibEntryBody
                                   )

    pBibEntryBody :: Parser [Field]
    pBibEntryBody = pList1Sep (pSym ',') pKeyValue <* spaces <* pSym '}' <* spaces
    

    pBibKey :: Parser String
    pBibKey  = pMunch (flip elem (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

    pReference = pSym '{' *> pBibKey <* pToken "," <* spaces

    parsePreamble = pToken "@preamble" *> spaces *> pSym '{' *> spaces *> pSym '"' *> pMunch (/= '"') <* pSym '"' <* spaces <* pSym '}' <* spaces

    pType :: Parser String
    pType = (pSym '@' *> pMunch1 (flip elem ['a'..'z']))

    pKeyValue :: Parser Field
    pKeyValue = Field <$> 
                    (spaces *> pMunch1 (flip elem ['a'..'z'])) 
                    <* 
                    (spaces *> pSym '=' *> spaces) 
                    <*> ((pSym '"' *> pMunch (/= '"') <* pSym '"' ) --TODO: nongreedy munch?
                         <|>
                         (pMunch isDigit)
                        )
