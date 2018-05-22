module Examples.VirtualDom.StaticVirtualDom where

import Elm.Default

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.IO (runIO)
import Control.Monad.IO.Effect (INFINITY)
import Elm.Platform (runProgram)
import Elm.VirtualDom (program, text)
import Prelude (Unit, unit, ($))


main :: Eff (infinity :: INFINITY) Unit
main =
    launchAff_ $ runIO do
        runProgram unit $
            program
                { init : unit /\ none
                , update : \msg model -> model /\ none
                , subscriptions : \model -> none
                , view : \model -> text "Hello World"
                }
