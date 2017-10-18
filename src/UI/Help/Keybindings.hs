{-# LANGUAGE DataKinds #-}

module UI.Help.Keybindings where

import qualified Brick.Types as Brick
import qualified Graphics.Vty as V
import UI.Actions
import Types

-- | Default Keybindings
helpKeybindings :: [Keybinding 'Help (Brick.Next AppState)]
helpKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (backToIndex `chain` continue)
    , Keybinding (V.EvKey V.KBS []) (scrollUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar ' ') []) (scrollDown `chain` continue)
    ]