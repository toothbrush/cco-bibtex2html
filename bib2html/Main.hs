module Main where
    
    import CCO.Component hiding (parser)
    import CCO.Feedback
    import CCO.Tree            (ATerm (App,List), Tree (fromTree, toTree), parser)
    import Common.Types
    import Common.TreeInstances
    import Common.ATermUtils
    import Control.Arrow
    
    main :: IO ()
    main = ioWrap pipeline
   
    pipeline :: Component String String
    pipeline =  parser        >>> 
                aTerm2BibTex  {- >>>   
                validator     >>>   
                sorter        >>>   
                bibTex2Html   >>>   
                html2Aterm    >>>   
                aTerm2String
                -}
                >>> bibTex2Aterm >>> aTerm2String
   

    aTerm2BibTex :: Component ATerm BibTex
    aTerm2BibTex = component toTree
 
  
    --TEMP
    bibTex2Aterm :: Component BibTex ATerm
    bibTex2Aterm = component $ (\inp -> do trace_ "Flattening tree..."
                                           (return . fromTree) inp)

