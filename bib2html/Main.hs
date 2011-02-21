module Main where
    
    import CCO.Component hiding (parser)
    import CCO.Feedback
    import CCO.Tree            (ATerm (App,List), Tree (fromTree, toTree), parser)
    import Common.BibTypes
    import Common.TreeInstances
    import Common.ATermUtils
    import Control.Arrow
    
    main :: IO ()
    main = ioWrap pipeline
   
    pipeline :: Component String String
    pipeline =  parser        >>> 
                aTerm2BibTex  >>> {-
                validator     >>>   
                sorter        >>> -}
                bibTex2Html   >>>   
                html2Aterm    >>> -- easy
                aTerm2String      --done
   

    aTerm2BibTex :: Component ATerm BibTex
    aTerm2BibTex = component (\inp -> do trace_ "Converting ATerm to bibtex..."
                                         return $ toTree inp)
 
  
    bibTex2HTML :: Component BibTex Html
    bibTex2HTML = component (\inp -> do trace_ "Converting BibTeX to HTML..."
                                        return $ foldBib bib2htmlAlg inp)
