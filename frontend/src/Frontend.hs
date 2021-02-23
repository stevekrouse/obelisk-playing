{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE RecursiveDo #-}


module Frontend where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix                (MonadFix, mfix)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Data.Monoid ((<>))
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route



ch1 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
ch1 = el "button" $ text "Send"

ch1' :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Element EventResult (DomBuilderSpace m) t, ())
ch1' = el' "button" $ text "Send"

ch2 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Event t ())
ch2 = ch1' >>= (fst >>> domEvent Click >>> pure)

ch3 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (InputElement EventResult (DomBuilderSpace m) t)
ch3 = inputElement def 

ch4 :: (DomBuilder t (Dynamic t), PostBuild t (Dynamic t), MonadHold t (Dynamic t), MonadFix (Dynamic t)) => Dynamic t T.Text
ch4 = ch3 >>= value

ch4' :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Dynamic t T.Text)
ch4' = ch3 >>= (value >>> pure)

ch4'' :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Behavior t T.Text)
ch4'' = ch3 >>= (value >>> current >>> pure)

ch5 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Event t T.Text)
ch5 = liftM2 tag ch4'' ch2

ch6 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Dynamic t [T.Text])
ch6 = ch5 >>= foldDyn (:) []

-- https://github.com/reflex-frp/reflex-todomvc/blob/3facdd7fc1cc585611012c6fef1cafb77a2dfa7a/src/Reflex/TodoMVC.hs#L37-L41
-- | Add a new value to a map; automatically choose an unused key
insertNew_ :: v -> Map Int v -> Map Int v
insertNew_ v m = case Map.maxViewWithKey m of
  Nothing -> Map.singleton 0 v
  Just ((k, _), _) -> Map.insert (k+1) v m

ch6' :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Dynamic t (Map Int T.Text))
ch6' = ch5 >>= foldDyn insertNew_ Map.empty

ch7 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m (Dynamic t (Map Int ()))
ch7 = ch6' >>= (flip list (dynText >>> el "div"))

ch8 messages = simpleList messages (dynText >>> el "div")

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Compose"
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      el "style" $ text "html,body {  height: 100%; margin: 0px; background-color: black; color: white;}"
  , _frontend_body = do
    count <- (el' "button" (text "Add client")) >>= (count . domEvent Click . fst) >>= (\c -> (dynText (fmap (T.pack . show) c)) >> pure c)
    el "div" $ mdo 
      -- dynListOfEvents <- simpleList (ffor count (\n -> fmap (const [(T.pack "hi")]) [1..n])) (\messages -> (ch8 messages) >> ch5 )
      dynListOfEvents <- simpleList dupMessages (\messages -> (ch8 messages) >> ch5 )
      let newMessage = switchDyn $ fmap leftmost dynListOfEvents
      messages <- foldDyn (:) [] newMessage

      let dupMessages = (ffor2 count messages (\c m -> take c $ repeat m))
      -- list messages (dynText >>> el "div")
      return ()
    
    return ()
  }









    --   (el' (T.pack "button") (text (T.pack "hi"))) >>= (count . domEvent Click . fst) >>= (dynText . fmap (T.pack . show))

    -- i <- textAreaElement $ def
    --     & textAreaElementConfig_initialValue .~ "initial"
    --     & textAreaElementConfig_setValue .~ never
    --     & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~
    --         mconcat [ "class" =: "new-todo"
    --                 , "placeholder" =: "What needs to be done?"
    --                 , "name" =: "newTodo"
    --                 , "type" =: "text"
    --                 , "style" =: "resize: none; width: 95%; background-color: transparent; margin: auto; display:block; margin-top: 20px;color: white;"
    --         ]