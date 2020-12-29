module AutoGUI.Keys
  ( mkKey
  , keyToText
  , key
  , isValidKey
  , keys
  , Key
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

newtype Key = Key String
  deriving (Eq, Show, Ord, Lift)

mkKey :: Text -> Maybe Key
mkKey key = if isValidKey key then Just (Key (T.unpack key)) else Nothing

keyToText :: Key -> Text
keyToText (Key text) = T.pack text

-- | This quasiquoter lets you use [key|enter|] at compile time,
--   so you don't get a Maybe as you would from mkKey
key :: QuasiQuoter
key =
  QuasiQuoter
    { quoteExp = compileKeyExp
    , quotePat = compileKeyPat
    , quoteDec = error "Key is not a declaration"
    , quoteType = error "Key is not a type"
    }
  where
    compileKeyExp :: String -> Q Exp
    compileKeyExp s = case mkKey (T.pack s) of
      Nothing -> fail $ "`" <> show s <> "` is not a valid key"
      Just key -> [|key|]

    compileKeyPat :: String -> Q Pat
    compileKeyPat s = case mkKey (T.pack s) of
      Nothing -> fail $ "`" <> show s <> "` is not a valid key"
      Just (Key str) -> pure $ ConP 'Key [LitP $ StringL str]

isValidKey :: Text -> Bool
isValidKey key = key `Set.member` keysText

keys :: Set Key
keys = Set.map (Key . T.unpack) keysText

keysText :: Set Text
keysText = Set.fromList
  [ "\t"
  , "\n"
  , "\r"
  , " "
  , "!"
  , "'"
  , "#"
  , "$"
  , "%"
  , "&"
  , "\""
  , "("
  , ")"
  , "*"
  , "+"
  , ","
  , "-"
  , "."
  , "/"
  , "0"
  , "1"
  , "2"
  , "3"
  , "4"
  , "5"
  , "6"
  , "7"
  , "8"
  , "9"
  , ":"
  , ";"
  , "<"
  , "="
  , ">"
  , "?"
  , "@"
  , "["
  , "\\"
  , "]"
  , "^"
  , "_"
  , "`"
  , "a"
  , "b"
  , "c"
  , "d"
  , "e"
  , "f"
  , "g"
  , "h"
  , "i"
  , "j"
  , "k"
  , "l"
  , "m"
  , "n"
  , "o"
  , "p"
  , "q"
  , "r"
  , "s"
  , "t"
  , "u"
  , "v"
  , "w"
  , "x"
  , "y"
  , "z"
  , "{"
  , "|"
  , "}"
  , "~"
  , "accept"
  , "add"
  , "alt"
  , "altleft"
  , "altright"
  , "apps"
  , "backspace"
  , "browserback"
  , "browserfavorites"
  , "browserforward"
  , "browserhome"
  , "browserrefresh"
  , "browsersearch"
  , "browserstop"
  , "capslock"
  , "clear"
  , "convert"
  , "ctrl"
  , "ctrlleft"
  , "ctrlright"
  , "decimal"
  , "del"
  , "delete"
  , "divide"
  , "down"
  , "end"
  , "enter"
  , "esc"
  , "escape"
  , "execute"
  , "f1"
  , "f10"
  , "f11"
  , "f12"
  , "f13"
  , "f14"
  , "f15"
  , "f16"
  , "f17"
  , "f18"
  , "f19"
  , "f2"
  , "f20"
  , "f21"
  , "f22"
  , "f23"
  , "f24"
  , "f3"
  , "f4"
  , "f5"
  , "f6"
  , "f7"
  , "f8"
  , "f9"
  , "final"
  , "fn"
  , "hanguel"
  , "hangul"
  , "hanja"
  , "help"
  , "home"
  , "insert"
  , "junja"
  , "kana"
  , "kanji"
  , "launchapp1"
  , "launchapp2"
  , "launchmail"
  , "launchmediaselect"
  , "left"
  , "modechange"
  , "multiply"
  , "nexttrack"
  , "nonconvert"
  , "num0"
  , "num1"
  , "num2"
  , "num3"
  , "num4"
  , "num5"
  , "num6"
  , "num7"
  , "num8"
  , "num9"
  , "numlock"
  , "pagedown"
  , "pageup"
  , "pause"
  , "pgdn"
  , "pgup"
  , "playpause"
  , "prevtrack"
  , "print"
  , "printscreen"
  , "prntscrn"
  , "prtsc"
  , "prtscr"
  , "return"
  , "right"
  , "scrolllock"
  , "select"
  , "separator"
  , "shift"
  , "shiftleft"
  , "shiftright"
  , "sleep"
  , "space"
  , "stop"
  , "subtract"
  , "tab"
  , "up"
  , "volumedown"
  , "volumemute"
  , "volumeup"
  , "win"
  , "winleft"
  , "winright"
  , "yen"
  , "command"
  , "option"
  , "optionleft"
  , "optionright"
  ]
