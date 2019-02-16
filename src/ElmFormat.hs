{-# OPTIONS_GHC -Wall #-}
module ElmFormat where

import Elm.Utils ((|>))
import System.Exit (exitFailure, exitSuccess)
import Messages.Types
import Messages.Formatter.Format
import Control.Monad.Free
import CommandLine.Helpers
import ElmVersion
import ElmFormat.FileStore (FileStore)
import ElmFormat.Operation (Operation)
import Coverage.Transformer as Transformer

import qualified AST.Module
import qualified Flags
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy.Char8 as LChar8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified ElmFormat.Execute as Execute
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.Filesystem as FS
import qualified ElmFormat.Operation as Operation
import qualified ElmFormat.Version
import qualified Text.JSON as Json
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Result as Result


-- If elm-format was successful and formatted result differ
-- from original content, writes the results to the output file.
-- Otherwise, display errors and exit
writeResult
    :: Operation f =>
    ElmVersion
    -> FilePath
    -> Text.Text
    -> Result.Result () Syntax.Error AST.Module.Module
    -> Free f (Either () (String, Json.JSValue))
writeResult elmVersion inputFile inputText result =
    case result of
        Result.Result _ (Result.Ok modu) ->
            let
                (transformed, info) = Transformer.transform modu
                renderedText =
                    transformed
                        |> Render.render elmVersion
                rendered =
                    renderedText
                        |> Text.encodeUtf8
            in
                Operation.deprecatedIO $
                ByteString.writeFile inputFile rendered
                >> return (Right info)

        Result.Result _ (Result.Err errs) ->
            onInfo (ParseError inputFile (Text.unpack inputText) errs)
            >> return (Left ())


processTextInput :: Operation f => ElmVersion -> FilePath -> Text.Text -> Free f (Either () (String, Json.JSValue))
processTextInput elmVersion inputFile inputText =
    Parse.parse inputText
        |> writeResult elmVersion inputFile inputText


processFileInput :: Operation f => ElmVersion -> FilePath -> Free f (Either () (String, Json.JSValue))
processFileInput elmVersion inputFile =
    do
        inputText <- Operation.deprecatedIO $ fmap Text.decodeUtf8 $ ByteString.readFile inputFile
        processTextInput elmVersion inputFile inputText


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

                (Right r', Right rs) ->
                    Right (r' : rs)

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


handleFilesInput :: Operation f => ElmVersion -> [FilePath] -> Free f (Either () [(String, Json.JSValue)])
handleFilesInput elmVersion inputFiles =
    let
        merge prev next =
            case (prev, next) of
              (Left (), _) -> Left ()
              (_, Left ()) -> Left ()
              (Right p, Right n) -> Right (n:p)
    in do
        elmFiles <- resolveFiles inputFiles

        case elmFiles of
            Left errors ->
                Operation.deprecatedIO $
                do
                    putStrLn $ r $ BadInputFiles errors
                    exitFailure

            Right [inputFile] -> do
                onInfo $ ProcessingFiles [inputFile]
                res <- processFileInput elmVersion inputFile
                return $ merge (Right []) res

            Right files -> do
                onInfo $ ProcessingFiles files
                validationResults <- mapM (\file -> processFileInput elmVersion file) files
                return $ foldl merge (Right []) validationResults


elmFormatVersion :: String
elmFormatVersion =
    ElmFormat.Version.asString


main :: ElmVersion -> IO ()
main elmVersion =
    do
        config <- Flags.parse elmFormatVersion
        case Flags._input config of
            [] ->
                Flags.showHelpText elmFormatVersion >> exitFailure

            files ->
                do
                    result <- foldFree Execute.forHuman $ handleFilesInput elmVersion files
                    case result of
                      Left _ ->
                          exitFailure

                      Right jsInfo -> do
                          Json.makeObj jsInfo
                              |> Json.encodeStrict
                              |> LChar8.pack
                              |> LChar8.writeFile ".coverage/info.json"
                          exitSuccess