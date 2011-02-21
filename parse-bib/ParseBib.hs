{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where
    
    --import Parsers etc...
    import Text.ParserCombinators.UU
    import Text.ParserCombinators.UU.BasicInstances
    import Text.ParserCombinators.UU.Examples

    import CCO.Feedback
    import CCO.Tree            (ATerm (App,List,String), Tree (fromTree, toTree))
    import CCO.Printing (pp, render_)
    import CCO.Component
    import Control.Arrow

    import Common.Types 
    import Common.TreeInstances
    import Common.ATermUtils
    import ParserUtils

    main :: IO ()
    main = ioWrap pipeline

    pipeline :: Component String String
    pipeline =  bibTexparser  >>>   
                bibTex2Aterm  >>>   
                aTerm2String 

    bibTexparser :: Component String BibTex
    bibTexparser = component $ parseFeedback parseBib

    bibTex2Aterm :: Component BibTex ATerm
    bibTex2Aterm = component $ (\inp -> do trace_ "Flattening tree..."
                                           (return . fromTree) inp)

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

