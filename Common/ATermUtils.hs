-- | This module contains some generally useful functions for
-- manipulating ATerms
module Common.ATermUtils where
    

    import CCO.Printing (pp, render_)
    import CCO.Component
    import CCO.Feedback
    import CCO.Tree            (ATerm (App,List,String), Tree (fromTree, toTree))
    import Control.Arrow
    import Common.HtmlTypes
    import Common.BibTypes
    import Common.TreeInstances

    -- | Render a given ATerm as a string. Used by all the tools that
    -- need to output ATerms to the terminal.
    aTerm2String :: Component ATerm String
    aTerm2String = arr (render_ 100 . pp)

    -- | Parse an ATerm into a BibTex data structure. 
    aTerm2BibTex :: Component ATerm BibTex
    aTerm2BibTex = component (\inp -> do trace_ "Converting ATerm to BibTeX..."
                                         toTree inp)
 
    -- | This function takes an abstract HTML tree and converts is into an ATerm
    html2Aterm :: Component Html ATerm
    html2Aterm = component (\inp -> do trace_ "Converting HTML to ATerm..."
                                       return $ fromTree inp)
