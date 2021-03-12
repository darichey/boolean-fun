module App (appComponent) where

import Prelude

import Bool.Parsing (boolExpr)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Text.Parsing.StringParser (runParser)

data Action = UpdateText String

type State = String

appComponent :: forall query input output m. H.Component query input output m
appComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = ""

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.input [ HP.type_ HP.InputText, HE.onValueInput UpdateText ]
    , HH.text $ show $ runParser boolExpr state
    ]

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  UpdateText s -> H.put s
