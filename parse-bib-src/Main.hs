{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where
    
    import CCO.Feedback
    import CCO.Tree     (ATerm (App,List,String), Tree (fromTree, toTree))
    import CCO.Printing (pp, render_)
    import CCO.Component
    import Control.Arrow

    import Common.BibTypes 
    import Common.TreeInstances
    import Common.ATermUtils
    import Parser
    import ParserUtils

    main :: IO ()
    main = ioWrap pipeline

    pipeline :: Component String String
    pipeline =  removeComments >>>
                bibTexparser  >>>   
                bibTex2Aterm  >>>   
                aTerm2String 

    removeComments :: Component String String
    removeComments = component $ (\inp -> do let splitfile = lines inp
                                             let rawFile = map killComments splitfile
                                             trace_ (show rawFile)
                                             return $ unlines rawFile
                                 )

    killComments :: String -> String
    killComments = takeWhile (/= '%')

    bibTexparser :: Component String BibTex
    bibTexparser = component $ parseFeedback parseBib

    bibTex2Aterm :: Component BibTex ATerm
    bibTex2Aterm = component $ (\inp -> do trace_ "Flattening tree..."
                                           (return . fromTree) inp)

