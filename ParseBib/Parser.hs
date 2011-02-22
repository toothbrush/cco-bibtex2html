-- | This module implements the parse-bib tool. It uses the UU-Parsinglib parser combinators
-- to allow for a forgiving, self-correcting Bib-parser. It is also quite readable, thanks to the advanced
-- library used. 
module ParseBib.Parser where

    --import Parsers etc...
    import Text.ParserCombinators.UU
    import Text.ParserCombinators.UU.BasicInstances
    import Text.ParserCombinators.UU.Examples

    import ParseBib.ParserUtils
    import Common.BibTypes
    import Char (isDigit)

    -- | the top-level parser. Describes the grammar of a BibTex file, 
    -- namely 0 or more preamble keywords, and 1 or more bibtex entries. the definitions
    -- here closely resemble the grammar, so it's easy to see what a file should look like, 
    -- by inspecting the parser functions. 
    parseBib :: Parser BibTex
    parseBib = BibTex `pMerge` (pAtLeast 0 parsePreamble
                        <||>    pMany parseBibEntry)

    -- | Grammar for a bib entry. An entry has a type, a name, and a body. 
    parseBibEntry :: Parser Entry
    parseBibEntry = Entry `pMerge` (pOne pType
                            <||>    pOne pReference
                            <||>    pOne pBibEntryBody
                                   )

    -- | A type of a bib entry is just a string of a-z, preceded by the '@' symbol. 
    -- Don't return the at-symbol, though.
    pType :: Parser String
    pType = (spaces *> pSym '@' *> pMunch1 (flip elem ['a'..'z']))

    -- | Parse the name of the bib entry. Cheat by consuming the first opening brace as well.
    pReference :: Parser String
    pReference = pSym '{' *> spaces *> pBibKey <* spaces <* pToken "," <* spaces

    -- | The body of a bib entry is a list of fields, separated by a comma and spaces. Finally consume 
    -- the closing brace when done. 
    pBibEntryBody :: Parser [Field]
    pBibEntryBody = pList1Sep (pSym ',') pKeyValue <* spaces <* pSym '}' <* spaces
    
    -- | a bib key (its reference) is a word containing [A-Za-z0-9]. 
    pBibKey :: Parser String
    pBibKey  = pMunch (flip elem (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-']))

    -- | A parser for the preamble keywords. This is just a string preceded by @preamble and surrounded by braces and quotation marks. Unfortunately no support for @string variables yet. 
    parsePreamble :: Parser String
    parsePreamble = spaces *> pToken "@preamble" *> spaces *> pSym '{' *> spaces *> pSym '"' *> pMunch (/= '"') <* pSym '"' <* spaces <* pSym '}' <* spaces

    -- | A key-value parser. A key is a word, followed by =, and the value is surrounded by quotation marks;
    -- if, however, the value is numeric, the quotation marks aren't required. 
    pKeyValue :: Parser Field
    pKeyValue = Field <$> 
                    (spaces *> pMunch1 (flip elem ['a'..'z'])) 
                    <* 
                    (spaces *> pSym '=' *> spaces) 
                    <*> ((pSym '"' *> pMunch (/= '"') <* pSym '"' ) --TODO: nongreedy munch?
                         <|>
                         (pMunch isDigit)
                         <|>
                         (pSym '{' *> pMunch (/= '}') <* pSym '}' )
                        )
