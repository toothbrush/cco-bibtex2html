module Common.ATermUtils where
    

    import CCO.Printing (pp, render_)
    import CCO.Component
    import CCO.Feedback
    import CCO.Tree            (ATerm (App,List,String), Tree (fromTree, toTree))
    import Control.Arrow
    import Common.HtmlTypes
    import Common.BibTypes
    import Common.TreeInstances

    aTerm2String :: Component ATerm String
    aTerm2String = arr (render_ 100 . pp)

    aTerm2BibTex :: Component ATerm BibTex
    aTerm2BibTex = component (\inp -> do trace_ "Converting ATerm to BibTeX..."
                                         toTree inp)
 
    html2Aterm :: Component Html ATerm
    html2Aterm = component (\inp -> do trace_ "Converting HTML to ATerm..."
                                       return $ fromTree inp)
