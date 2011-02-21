{-# LANGUAGE FlexibleContexts #-}
module Main where
    
    --import Parsers etc...
    import Text.ParserCombinators.UU
    import Text.ParserCombinators.UU.BasicInstances
    import Text.ParserCombinators.UU.Examples

    import CCO.Feedback
    import CCO.Component
    import CCO.Tree            (ATerm (App,List), Tree (fromTree, toTree))
    import CCO.Printing (pp, render_)
    import Control.Arrow
 --   import Util.UU

    -- a bibtex file is just a list of entries:
    
    data BibTex = BibTex [Entry]
                  deriving Show

    -- a bibtex entry:
    data Entry = Entry {entryType :: EntryType, 
                    reference :: String,
                    fields :: [Field]
                   }
                   deriving Show

    data Field = Field String String -- Name and contents of field
                deriving Show

    data EntryType = Book 
                 | Proceedings
                 deriving Show

    bibTexparser :: Component String BibTex
    bibTexparser = component $ parseFeedback parseBib

    parseFeedback :: Parser BibTex -> String -> Feedback BibTex
    parseFeedback p inp = do return $ myrun p inp
                            -- return (BibTex [])

    myrun :: Parser BibTex -> String -> BibTex
    --myrun :: Show t =>  Parser t -> String -> String
    myrun p inp = {- do -}  let r@(a, errors) =  parse ( (,) <$> p <*> pEnd) (listToStr inp (0,0))
                      in a
                {-
                    putStrLn "--"
                    putStrLn ("-- > Result: " ++ show a)
                    if null errors then  return ()
                                   else  do putStr ("-- > Correcting steps: \n")
                                            show_errors errors
                    putStrLn "-- "
                    -}

--    bibTex2Aterm :: Component BibTex ATerm
--    bibTex2Aterm = component (return . fromTree) 



    main :: IO ()
    main = ioWrap pipeline

    pipeline :: Component String String
    pipeline =  bibTexparser  {- >>>   
                bibTex2Aterm  >>>   
                aTerm2String  -}
                >>> flatten

    flatten = component $ showBib
    
    showBib b = do return (show b) 
    

    parseBib = BibTex `pMerge` (pMany parseBibEntry)

    parseBibEntry = Entry `pMerge` (pOne pType
                       <||> pOne pReference
                       <||> pOne pBibEntryBody
                            )

    pBibEntryBody :: Parser [Field]
    pBibEntryBody = pList1Sep (pSym ',') (pKeyValue) <* pSym '}' <* spaces
    
    
    ---- (pMany (pKeyValue <* pSym ',' <* spaces))  <* pSym '}'

    pBibKey :: Parser String
    pBibKey  = pMunch (flip elem (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

    pKeyValue :: Parser Field
    pKeyValue = Field <$> (spaces *> pMunch1 (flip elem ['a'..'z'])) <* (spaces *> pSym '=' *> spaces *> pSym '"') <*> (pMunch1 (/= '"') <* pSym '"' )


    spaces :: Parser String
    spaces = pMunch (flip elem " \t\n")


    pReference = pSym '{' *> pBibKey <* pToken "," <* spaces

    pBraced :: Parser a -> Parser a
    pBraced a = pSym '{' *> a <* pSym '}'

    pType :: Parser EntryType
    pType = f <$> (string "@book" <|> string "@proceedings")
      where f "@book" = Book
            f "@proceedings" = Proceedings


    pAuthor :: Parser String
    pAuthor = pMunch1 (/= '%') -- TODO FIX

    -- | string has been defined, even though it may seem extraneous in the 
    -- presence of the token parser. The advantage of string is that it 
    -- corrects the input more reliably, because spelling or case errors are 
    -- replaced, as opposed to the behaviour of the token parser, which may 
    -- substitute whole keywords. 
    string :: String -> Parser String
    string []     = pure []
    string (x:xs) = (:) <$> pSym x <*> string xs


    -- | the pMunch1 parser is much like pMunch, except that it only 
    -- succeeds when it can munch 1 or more characters. It fails on 
    -- the empty string. 
    pMunch1
      :: (Provides st (a -> Bool, [Char], Char) a,
            Provides st (Munch a) [a]) =>
                 (a -> Bool) -- ^ munch all text matching this predicate
                 -> P st [a]
    pMunch1 p  = (:) <$> pSym (p,"at least 1 character",' ') <*> pMunch p
