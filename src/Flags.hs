module Flags where

import Data.Monoid ((<>))
import ElmVersion (ElmVersion(..))

import qualified ElmVersion
import qualified Options.Applicative as Opt
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Config = Config
    { _input :: [FilePath]
    }



-- PARSE ARGUMENTS


parse :: String -> [String] -> Opt.ParserResult Config
parse elmFormatVersion args =
    Opt.execParserPure preferences (parser elmFormatVersion) args


usage :: String -> String -> String
usage progName version =
    fst $
    Opt.renderFailure
        (Opt.parserFailure preferences (parser version) Opt.ShowHelpText mempty)
        progName


preferences :: Opt.ParserPrefs
preferences =
    Opt.prefs (mempty <> Opt.showHelpOnError)


parser :: String -> Opt.ParserInfo Config
parser elmFormatVersion =
    Opt.info
        (Opt.helper <*> flags)
        (helpInfo elmFormatVersion)


showHelpText :: String -> Opt.ParserResult never
showHelpText elmFormatVersion = Opt.Failure $
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
        , Opt.progDesc "Instrument Elm source files."
        ]
  where
    top =
        PP.vcat $ [ PP.text $ "elm-instrument " ++ elmFormatVersion ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lineList =
    PP.vcat (map PP.text lineList)


input :: Opt.Parser FilePath
input =
    Opt.strArgument $ Opt.metavar "INPUT"
