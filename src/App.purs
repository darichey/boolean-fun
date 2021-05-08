module App (appComponent) where

import Prelude

import Bool.Eval (truthTable)
import Bool.Model (BoolExpr, prettyPrint)
import Bool.Notation (BoolNotation, algebraicNotation, programmingNotation)
import Bool.Parsing (boolExpr)
import Bool.SumOfProducts (sumOfProducts)
import Data.Array as A
import Data.Either (Either(..))
import Data.List as L
import Data.Map as M
import Data.String.CodeUnits as CU
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Text.Parsing.StringParser (runParser)

data Action = UpdateText String | UpdateNotation BoolNotation

type AppState =
  { text :: String
  , notation :: BoolNotation
  }

appComponent :: forall query input output m. H.Component query input output m
appComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> AppState
initialState _ =
  { text: ""
  , notation: algebraicNotation
  }

render :: forall m. AppState -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.id "foo" ]
    [ HH.div_
      [ HH.div_
        [ HH.input [ HP.type_ InputRadio, HP.id "algebraic", HP.name "notation", HP.value "algebraic", HP.checked true, HE.onValueChange (const $ UpdateNotation algebraicNotation) ]
        , HH.label [ HP.for "algebraic" ] [ HH.text "algebraic" ]
        ]
    , HH.div_
        [ HH.input [ HP.type_ InputRadio, HP.id "programming", HP.name "notation", HP.value "programming", HP.checked false, HE.onValueChange (const $ UpdateNotation programmingNotation) ]
        , HH.label [ HP.for "programming" ] [ HH.text "programming" ]
        ]
      ]
    , HH.div_ [ HH.p_ [ HH.text $ show $ state.notation ] ]
    , HH.div_
      [ HH.text "Y = "
      , HH.input [ HP.type_ InputText, HE.onValueInput UpdateText ]
      ]
    , HH.div_
        case runParser (boolExpr state.notation) state.text of
          Left _ -> []
          Right e -> [ table e ]
    , HH.div_
      [ HH.p_ [ HH.text "Sum of Products: " ]
      , HH.p_
        [ HH.text $
          case runParser (boolExpr state.notation) state.text of
            Left _ -> ""
            Right e -> prettyPrint state.notation (sumOfProducts e)
        ]
      ]
    ]

table :: forall w i. BoolExpr -> HH.HTML w i
table e = HH.table_ $ A.cons heading content
  where
  t = truthTable e

  heading = HH.tr_ $ map (HH.th_ <<< A.singleton <<< HH.text <<< CU.singleton) $ A.snoc (t.variables) 'Y'

  content = A.reverse $ map (HH.tr_ <<< map (HH.td_ <<< A.singleton <<< HH.text <<< CU.singleton) <<< convertRow) t.rows

  convertRow :: { bindings :: Array Boolean, value :: Boolean } -> Array Char
  convertRow { bindings, value } = map boolChar (A.snoc bindings value)

  boolChar :: Boolean -> Char
  boolChar true = 'T'
  boolChar false = 'F'

handleAction :: forall output m. Action -> H.HalogenM AppState Action () output m Unit
handleAction = case _ of
  UpdateText text ->
    H.modify_ _ { text = text }
  UpdateNotation notation ->
    H.modify_ _ { notation = notation }
