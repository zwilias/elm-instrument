{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Reporting.Region where

import qualified Text.Parsec.Pos as Parsec
import qualified Text.JSON as Json


data Region = Region
    { start :: Position
    , end :: Position
    }
    deriving (Eq, Show)


data Position = Position
    { line :: Int
    , column :: Int
    }
    deriving (Eq, Show)


fromSourcePos :: Parsec.SourcePos -> Position
fromSourcePos sourcePos =
    Position
      (Parsec.sourceLine sourcePos)
      (Parsec.sourceColumn sourcePos)


merge :: Region -> Region -> Region
merge (Region start _) (Region _ end) =
    Region start end


-- TO STRING

toString :: Region -> String
toString (Region start end) =
  case line start == line end of
    False ->
        "between lines " ++ show (line start)
        ++ " and " ++ show (line end)

    True ->
        "on line " ++ show (line end) ++ ", column "
        ++ show (column start) ++ " to " ++ show (column end)

-- To JSON
instance Json.JSON Position where
  readJSON _ = Json.Error "Not implemented"
  showJSON Position{line, column} =
    Json.makeObj [ ("line", Json.showJSON line), ("column", Json.showJSON column)]

