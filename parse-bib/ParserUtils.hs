{-# LANGUAGE FlexibleContexts #-}

module ParserUtils where

    import Text.ParserCombinators.UU
    import Text.ParserCombinators.UU.BasicInstances
    import Text.ParserCombinators.UU.Examples

    import Common.Types
    import CCO.Feedback

    parseFeedback :: Parser BibTex -> String -> Feedback BibTex
    parseFeedback p inp = do trace_ "Parsing .bib file..."
                             let r@(a, errors) =  parse ( (,) <$> p <*> pEnd) (listToStr inp (0,0))
                             if null errors then  do trace_ "Parsed without errors."
                                                     return a 
                                            else  do sequence $ map (warn_ . show) errors
                                                     return a 

    -- | the pMunch1 parser is much like pMunch, except that it only 
    -- succeeds when it can munch 1 or more characters. It fails on 
    -- the empty string. 
    pMunch1
      :: (Provides st (a -> Bool, [Char], Char) a,
            Provides st (Munch a) [a]) =>
                 (a -> Bool) -- ^ munch all text matching this predicate
                 -> P st [a]
    pMunch1 p  = (:) <$> pSym (p,"at least 1 character",' ') <*> pMunch p


    -- | string has been defined, even though it may seem extraneous in the 
    -- presence of the token parser. The advantage of string is that it 
    -- corrects the input more reliably, because spelling or case errors are 
    -- replaced, as opposed to the behaviour of the token parser, which may 
    -- substitute whole keywords. 
    string :: String -> Parser String
    string []     = pure []
    string (x:xs) = (:) <$> pSym x <*> string xs

    pBraced :: Parser a -> Parser a
    pBraced a = pSym '{' *> a <* pSym '}'

    spaces :: Parser String
    spaces = pMunch (flip elem " \t\n")

