module App (appComponent) where

import Prelude

import Bool.Eval (truthTable)
import Bool.Model (BoolExpr)
import Bool.Parsing (boolExpr)
import Data.Array as A
import Data.Either (Either(..))
import Data.List as L
import Data.Map as M
import Data.String.CodeUnits as CU
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
    [ HH.div_
      [ HH.text "Y = "
      , HH.input [ HP.type_ HP.InputText, HE.onValueInput UpdateText ]
      ]
    , HH.text $ show $ runParser boolExpr state
    , HH.div_
        case runParser boolExpr state of
          Left _ -> []
          Right e -> [ table e ]
    ]
  

table :: forall w i. BoolExpr -> HH.HTML w i
table e = HH.table_ $ A.cons heading content
  where
  t = truthTable e

  heading = HH.tr_ $ map (HH.th_ <<< A.singleton <<< HH.text <<< CU.singleton) $ A.snoc (t.variables) 'Y'

  content = A.reverse $ map (HH.tr_ <<< map (HH.td_ <<< A.singleton <<< HH.text <<< CU.singleton)) $ map convertRow $ t.rows

  convertRow :: { bindings :: M.Map Char Boolean, value :: Boolean } -> Array Char
  convertRow { bindings, value } = map boolChar (A.snoc (L.toUnfoldable $ M.values bindings) value)

  boolChar :: Boolean -> Char
  boolChar true = 'T'
  boolChar false = 'F'

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  UpdateText s -> H.put s
