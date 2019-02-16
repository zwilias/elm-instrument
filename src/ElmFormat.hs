{-# OPTIONS_GHC -Wall #-}
module ElmFormat where

import Prelude hiding (putStr, putStrLn)

import System.Exit (ExitCode(..))
import System.Environment (getArgs)
import Messages.Types
import Messages.Formatter.Format
import Control.Monad.Free
import Data.Either as Either
import qualified CommandLine.Helpers as Helpers
import ElmVersion
import ElmFormat.FileStore (FileStore)
import ElmFormat.FileWriter (FileWriter)
import ElmFormat.InputConsole (InputConsole)
import ElmFormat.OutputConsole (OutputConsole)
import ElmFormat.World

import qualified AST.Module
import qualified Flags
import qualified Data.Text as Text
import qualified ElmFormat.Execute as Execute
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.FileWriter as FileWriter
import qualified ElmFormat.Filesystem as FS
import qualified ElmFormat.Version
import qualified Options.Applicative as Opt
import qualified Reporting.Result as Result
import qualified Text.JSON as Json
import qualified Coverage.Transformer as Transformer

resolveFile :: FileStore f => FilePath -> Free f (Either InputFileMessage [FilePath])
resolveFile path =
    do
        fileType <- FileStore.stat path

        case fileType of
            FileStore.IsFile ->
                return $ Right [path]

            FileStore.IsDirectory ->
                do
                    elmFiles <- FS.findAllElmFiles path
                    case elmFiles of
                        [] -> return $ Left $ NoElmFiles path
                        _ -> return $ Right elmFiles

            FileStore.DoesNotExist ->
                return $ Left $ FileDoesNotExist path


collectErrors :: [Either l r] -> Either [l] [r]
collectErrors list =
    let
        step acc next =
            case (next, acc) of
                (Left l, Right _) ->
                    Left [l]

                (Left l, Left ls) ->
                    Left (l : ls)

                (Right r, Right rs) ->
                    Right (r : rs)

                (Right _, Left ls) ->
                    Left ls
    in
        foldl step (Right []) list


resolveFiles :: FileStore f => [FilePath] -> Free f (Either [InputFileMessage] [FilePath])
resolveFiles inputFiles =
    do
        result <- collectErrors <$> mapM resolveFile inputFiles
        case result of
            Left ls ->
                return $ Left ls

            Right files ->
                return $ Right $ concat files


exitWithError :: World m => ErrorMessage -> m ()
exitWithError message =
    (putStrLn $ Helpers.r $ message)
        >> exitFailure


determineVersion :: ElmVersion -> Bool -> Either ErrorMessage ElmVersion
determineVersion elmVersion upgrade =
    case (elmVersion, upgrade) of
        (Elm_0_18, True) ->
            Right Elm_0_18_Upgrade

        (Elm_0_19, True) ->
            Right Elm_0_19_Upgrade

        (_, True) ->
            Left $ MustSpecifyVersionWithUpgrade Elm_0_19_Upgrade

        (_, False) ->
            Right elmVersion


elmFormatVersion :: String
elmFormatVersion =
    ElmFormat.Version.asString


{-| copied from Options.Applicative -}
handleParseResult :: World m => Opt.ParserResult a -> m (Maybe a)
handleParseResult (Opt.Success a) = return (Just a)
handleParseResult (Opt.Failure failure) = do
      progn <- getProgName
      let (msg, exit) = Opt.renderFailure failure progn
      case exit of
        ExitSuccess -> putStrLn msg *> exitSuccess *> return Nothing
        _           -> putStrLnStderr msg *> exitFailure *> return Nothing
handleParseResult (Opt.CompletionInvoked _) = do
      -- progn <- getProgName
      -- msg <- Opt.execCompletion compl progn
      -- putStr msg
      -- const undefined <$> exitSuccess
      error "Shell completion not yet implemented"


main :: IO ()
main =
    do
        args <- getArgs
        main' args


main' :: World m => [String] -> m ()
main' args =
    main'' elmFormatVersion args

main'' :: World m => String -> [String] -> m ()
main'' elmFormatVersion_ args =
    do
        c <- handleParseResult $ Flags.parse elmFormatVersion_ args
        case c of
            Nothing -> return ()
            Just config ->
                do
                    resolvedInputFiles <- Execute.run (Execute.forHuman True) $ resolveFiles (Flags._input config)

                    case resolvedInputFiles of
                        Left fileErrors ->
                            exitWithError (BadInputFiles fileErrors)

                        Right [] ->
                            (handleParseResult $ Flags.showHelpText elmFormatVersion_)
                                >> exitFailure

                        Right files ->
                            do
                                result <- Execute.run (Execute.forHuman True) $ doIt ElmVersion.Elm_0_19 files
                                if result
                                    then exitSuccess
                                    else exitFailure


data FormatResult = Res FilePath (String, Json.JSValue) Text.Text


parseModule :: (FilePath, Text.Text) -> Either InfoMessage AST.Module.Module
parseModule (inputFile, inputText) =
    case Parse.parse inputText of
        Result.Result _ (Result.Ok modu) ->
            Right modu

        Result.Result _ (Result.Err errs) ->
            Left $ ParseError inputFile (Text.unpack inputText) errs


format :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage FormatResult
format elmVersion (inputFile, inputText) =
    case parseModule (inputFile, inputText) of
        Right modu ->
            let
                (transformed, info) = Transformer.transform modu
                outputText = Render.render elmVersion transformed
            in
            Right $ Res inputFile info outputText

        Left message ->
            Left message


readFile :: (FileStore f, InfoFormatter f) => FilePath -> Free f (FilePath, Text.Text)
readFile filePath =
    onInfo (ProcessingFile filePath)
        *> ((,) filePath <$> FileStore.readFile filePath)


updateFile :: FileWriter f => FormatResult -> Free f (String, Json.JSValue)
updateFile (Res outputFile info outputText) =
    do
        FileWriter.overwriteFile outputFile outputText
        return info


logErrorOr :: InfoFormatter f => (a -> Free f v) -> Either InfoMessage a -> Free f (Either () v)
logErrorOr fn result =
    case result of
        Left message ->
            onInfo message *> return (Left ())

        Right value ->
            Right <$> fn value


doIt :: (InputConsole f, OutputConsole f, InfoFormatter f, FileStore f, FileWriter f) => ElmVersion -> [FilePath] -> Free f Bool
doIt elmVersion files =
    do
        results <- mapM formatFile files
        case partitionEithers results of
            ([], jsInfo) ->
                let jsonData = Text.pack $ Json.encodeStrict $ Json.makeObj jsInfo
                in
                    FileWriter.overwriteFile ".coverage/info.json" jsonData
                      *> return True
            _ ->
                return False
    where
        formatFile file = (format elmVersion <$> ElmFormat.readFile file) >>= logErrorOr ElmFormat.updateFile
