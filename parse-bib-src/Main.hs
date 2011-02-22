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
    pipeline =  bibTexparser  >>>   
                bibTex2Aterm  >>>   
                aTerm2String 

    bibTexparser :: Component String BibTex
    bibTexparser = component $ parseFeedback parseBib

    bibTex2Aterm :: Component BibTex ATerm
    bibTex2Aterm = component $ (\inp -> do trace_ "Flattening tree..."
                                           (return . fromTree) inp)

