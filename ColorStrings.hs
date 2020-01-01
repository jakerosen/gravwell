module ColorStrings where

import qualified System.Console.ANSI as Ansi

blue, green, red, black, magenta, cyan :: [ Char ] -> [ Char ]
blue = style ( fg Ansi.Blue )
green = style ( fg Ansi.Green )
red = style ( fg Ansi.Red )
black = style ( fg Ansi.Black )
magenta = style ( fg Ansi.Magenta )
cyan = style ( fg Ansi.Cyan )

fg :: Ansi.Color -> Ansi.SGR
fg c = Ansi.SetColor Ansi.Foreground Ansi.Vivid c

style :: Ansi.SGR -> [ Char ] -> [ Char ]
style c s = Ansi.setSGRCode [ c ] ++ s ++ Ansi.setSGRCode [ Ansi.Reset ]

