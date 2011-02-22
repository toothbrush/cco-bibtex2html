{-# LANGUAGE NoMonomorphismRestriction #-}
--  | This module defines the initial tool, parse-bib. It is responsible for filtering comments, 
--  parsing the input, and creating the first ATerm out of the BibTex tree, once parsed.
module ParseBib.Tool where
    
    import CCO.Feedback
    import CCO.Tree     (ATerm (App,List,String), Tree (fromTree, toTree))
    import CCO.Printing (pp, render_)
    import CCO.Component
    import Control.Arrow

    import Common.BibTypes 
    import Common.TreeInstances
    import Common.ATermUtils
    import ParseBib.Parser
    import ParseBib.ParserUtils

    -- | wrap the pipeline in IO and run.
    mainFunc :: IO ()
    mainFunc = ioWrap pipeline

    -- | the pipeline which carries out the aforementioned steps. 
    pipeline :: Component String String
    pipeline =  removeComments >>>
                bibTexparser  >>>   
                bibTex2Aterm  >>>   
                aTerm2String 

    -- | removeComments splits the input into lines, then processes each line
    -- for comments using the killComments function
    removeComments :: Component String String
    removeComments = component $ (\inp -> do let splitfile = lines inp
                                             let rawFile = map killComments splitfile
                                             return $ unlines rawFile
                                 )

    -- | killComments throws away everything after a '%' on a line of input.
    killComments :: String -> String
    killComments = takeWhile (/= '%')

    -- | run the parser, parseBib, on the input, using the parseFeedback wrapper.
    bibTexparser :: Component String BibTex
    bibTexparser = component $ parseFeedback parseBib

    -- | convert the parsed BibTex tree into an ATerm.
    bibTex2Aterm :: Component BibTex ATerm
    bibTex2Aterm = component $ (\inp -> do trace_ "Flattening tree..."
                                           (return . fromTree) inp)

