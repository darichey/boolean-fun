module Main where

import Prelude

import App (appComponent)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception as Exception
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do
  root <- HA.selectElement (QuerySelector "#root")
  case root of
    Just r -> runUI appComponent unit r
    Nothing -> H.liftEffect $ Exception.throw "Couldn't find root element"
