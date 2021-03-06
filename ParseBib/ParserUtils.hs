{-# LANGUAGE FlexibleContexts #-}

-- | This module contains some useful parsing utilities.
module ParseBib.ParserUtils where

    import Text.ParserCombinators.UU
    import Text.ParserCombinators.UU.BasicInstances
    import Text.ParserCombinators.UU.Examples

    import Common.BibTypes
    import CCO.Feedback

    -- | Parse a given input using some UU-Parsinglib parser, but
    -- return feedback in the CCO Feedback monad. Useful for reporting
    -- failures and error corrections done by the parser.
    parseFeedback :: Parser BibTex -- ^ the parser to use
                  -> String        -- ^ the input
                  -> Feedback BibTex -- ^ return a BibTex, but also allow Feedback
    parseFeedback p inp = do trace_ "Parsing .bib file..."
                             let r@(a, errors) =  parse ( (,) <$> p <*> pEnd) (listToStr inp (0,0))
                             if null errors then  return ()
                                            else  do sequence $ map (warn_ . show) errors
                                            	     return ()
                             return a 

    -- | the pMunch1 parser is much like pMunch, except that it only 
    -- succeeds when it can munch 1 or more characters. It fails on 
    -- the empty string. 
    -- Inspired by the library function pMunch, from UU-Parsinglib.
    pMunch1
      :: (Provides st (a -> Bool, [Char], Char) a,
            Provides st (Munch a) [a]) =>
                 (a -> Bool) -- ^ munch all text matching this predicate
                 -> P st [a]
    pMunch1 p  = (:) <$> pSym (p,"at least 1 character",' ') <*> pMunch p

    -- | Parser for parsing anything contained in braces. 
    pBraced :: Parser a -> Parser a
    pBraced a = pSym '{' *> a <* pSym '}'

    -- | A parser which greedily consumes whitespace. Useful between entries or fields. 
    spaces :: Parser String
    spaces = pMunch (flip elem " \t\n")

