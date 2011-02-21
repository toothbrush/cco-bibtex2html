module Common.ATermUtils where
    

    import CCO.Printing (pp, render_)
    import CCO.Component
    import CCO.Tree            (ATerm (App,List,String), Tree (fromTree, toTree))
    import Control.Arrow

    aTerm2String :: Component ATerm String
    aTerm2String = arr (render_ 100 . pp)
