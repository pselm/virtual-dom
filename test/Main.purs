module Test.Main where

import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)

import DOM (DOM)
import DOM.JSDOM (JSDOM)
import Graphics.Canvas (CANVAS)

import Test.Elm.VirtualDomTest as VirtualDomTest

import Prelude (Unit)


main :: Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    , canvas :: CANVAS
    , dom :: DOM
    , err :: EXCEPTION
    , jsdom :: JSDOM
    , random :: RANDOM
    ) Unit

main =
    runTest do
        VirtualDomTest.tests
