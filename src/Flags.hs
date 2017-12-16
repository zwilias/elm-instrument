module Flags where

import Data.Monoid ((<>))

import qualified Options.Applicative as Opt
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Config = Config
    { _input :: [FilePath]
    }



-- PARSE ARGUMENTS


parse :: String -> IO Config
parse elmFormatVersion =
    Opt.customExecParser preferences (parser elmFormatVersion)


preferences :: Opt.ParserPrefs
preferences =
    Opt.prefs (mempty <> Opt.showHelpOnError)


parser :: String -> Opt.ParserInfo Config
parser elmFormatVersion =
    Opt.info
        (Opt.helper <*> flags)
        (helpInfo elmFormatVersion)


showHelpText :: String -> IO ()
showHelpText elmFormatVersion = Opt.handleParseResult . Opt.Failure $
    Opt.parserFailure
        preferences
        (parser elmFormatVersion)
        Opt.ShowHelpText
        mempty


-- COMMANDS

flags :: Opt.Parser Config
flags =
    Config
      <$> Opt.many input



-- HELP

helpInfo :: String -> Opt.InfoMod Config
helpInfo elmFormatVersion =
    mconcat
        [ Opt.fullDesc
        , Opt.headerDoc $ Just top
        , Opt.progDesc "Instrument Elm source files for coverage tracking"
        , Opt.footerDoc Nothing
        ]
  where
    top =
        PP.vcat [ PP.text $ "elm-format " ++ elmFormatVersion ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lineList =
    PP.vcat (map PP.text lineList)

input :: Opt.Parser FilePath
input =
    Opt.strArgument $ Opt.metavar "INPUT"

