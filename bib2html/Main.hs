module Main where
    
    import CCO.Tree            (ATerm (App,List), Tree (fromTree, toTree),parser)
    import Common.Types
    
    main = ioWrap pipeline
   
    pipeline =  parser        >>> 
                aTerm2BibTex  {- >>>   
                validator     >>>   
                sorter        >>>   
                bibTex2Html   >>>   
                html2Aterm    >>>   
                aTerm2String
                -}
   
    aTerm2BibTex :: Component ATerm BibTex
    aTerm2BibTex = component toTree
 
  
