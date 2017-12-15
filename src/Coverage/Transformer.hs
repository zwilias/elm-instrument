{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
module Coverage.Transformer where

import           AST.Declaration      as Declaration
import qualified AST.Expression       as Expression
import qualified AST.Module           as M
import qualified AST.Pattern          as Pattern
import qualified AST.V0_16            as AST
import           AST.Variable         (Listing (..))
import qualified AST.Variable         as Var
import           Control.Monad.State  (State)
import qualified Control.Monad.State  as State
import           Data.Int             (Int64)
import           Data.List            as List
import qualified Data.Map.Strict      as Map
import           Elm.Utils            ((|>))
import qualified Text.JSON            as Json
import           Reporting.Annotation (Located (..))
import           Reporting.Region


transform :: M.Module -> (M.Module, Json.JSValue)
transform modul =
  let
    (comments, modulImports) =
      M.imports modul :: ( AST.Comments
        , Map.Map [AST.UppercaseIdentifier] (AST.Comments, M.ImportMethod)
        )

    coverageIdentifier :: [AST.UppercaseIdentifier]
    coverageIdentifier = [AST.UppercaseIdentifier "Coverage"]

    coverageImportMethod :: M.ImportMethod
    coverageImportMethod = M.ImportMethod
      { M.alias       = Nothing
      , M.exposedVars = ([], ([], ClosedListing))
      }

    updatedImports
      :: Map.Map [AST.UppercaseIdentifier] ([AST.Comment], M.ImportMethod)
    updatedImports =
      Map.insert coverageIdentifier ([], coverageImportMethod) modulImports

    moduleName_ :: [AST.UppercaseIdentifier]
    (AST.Commented _ moduleName_ _) = M.name $ M.header modul

    moduleName :: String
    moduleName =
      moduleName_ |> List.map (\(AST.UppercaseIdentifier s) -> s) |> intercalate
        "."

    updatedBody :: [Decl]
    store :: AnnotationStore
    (updatedBody, store) = State.runState
      (mapM (annotate moduleName) (M.body modul))
      (emptyStore moduleName)
  in
    ( modul { M.imports = (comments, updatedImports), M.body = updatedBody }
    , storeToJson store
    )


annotate :: String -> Declaration.Decl -> State AnnotationStore Declaration.Decl
annotate moduleName declaration' = case declaration' of
  Declaration.Decl (A region declaration_) -> do
    declaration <- annotateDeclaration moduleName region declaration_
    return $ Declaration.Decl $ A region declaration

  _ -> return declaration'


emptyRegion :: Region
emptyRegion = Region (Position 0 0) (Position 0 0)


plain :: a -> AST.PreCommented a
plain a = ([], a)


annotation
  :: (Region -> Annotation)
  -> String
  -> Expression.Expr
  -> State AnnotationStore Expression.Expr
annotation toAnnotation moduleName body@(A region _) =
  annotationWithRegion toAnnotation moduleName region body


annotationRef :: Region -> Expression.Expr
annotationRef region = A region $ Expression.VarExpr $ Var.VarRef
  [AST.UppercaseIdentifier "Coverage"]
  (AST.LowercaseIdentifier "annotation")


annotationWithRegion
  :: (Region -> Annotation)
  -> String
  -> Region
  -> Expression.Expr
  -> State AnnotationStore Expression.Expr
annotationWithRegion toAnnotation moduleName region' body@(A region body') = do
  store <- State.get
  let (count, store') = register (toAnnotation region') store
  State.put store'

  let plainLit   = plain . A region . Expression.Literal
  let underscore = A region Pattern.Anything
  let letBody = A region $ Expression.App
        (annotationRef region)
        [ plainLit $ AST.Str moduleName False
        , plainLit $ AST.IntNum count AST.DecimalInt
        ]
        (AST.FAJoinFirst AST.JoinAll)
  let letDecl = Expression.LetDefinition underscore [] [] letBody

  return $ case body' of
    Expression.Let letDecls comments expr ->
      A region $ Expression.Let (letDecl : letDecls) comments expr
    _ -> A region $ Expression.Let [letDecl] [] body


annotateDeclaration
  :: String
  -> Region
  -> Declaration.Declaration
  -> State AnnotationStore Declaration.Declaration
annotateDeclaration moduleName region declaration = case declaration of
  Declaration.Definition pat@(A _ patName) pats comments body -> do
    resetComplexity
    annotatedExpressions <- annotateExpression moduleName body
    complexity           <- State.gets current
    annotatedBody        <- annotationWithRegion
      (Declaration (patternToName patName) complexity)
      moduleName
      region
      annotatedExpressions
    return $ Declaration.Definition pat pats comments annotatedBody

  _ -> return declaration


patternToName :: Pattern.Pattern' -> String
patternToName pat = case pat of
  Pattern.VarPattern (AST.LowercaseIdentifier name) -> name
  _ -> ""


annotateCommentedExpression
  :: String
  -> AST.PreCommented Expression.Expr
  -> State AnnotationStore (AST.PreCommented Expression.Expr)
annotateCommentedExpression moduleName (comments, expr) = do
  expr' <- annotateExpression moduleName expr
  return (comments, expr')


annotateExpression
  :: String -> Expression.Expr -> State AnnotationStore Expression.Expr
annotateExpression moduleName expression@(A region expr) = case expr of
  Expression.App inner parts faApp -> do
    parts' <- mapM (annotateCommentedExpression moduleName) parts
    inner' <- annotateExpression moduleName inner
    return $ A region $ Expression.App inner' parts' faApp

  Expression.Unary op operand -> do
    operand' <- annotateExpression moduleName operand
    return $ A region $ Expression.Unary op operand'

  Expression.Parens (AST.Commented pre bodyContent post) -> do
    bodyContent' <- annotateExpression moduleName bodyContent
    return $ A region $ Expression.Parens $ AST.Commented pre bodyContent' post

  Expression.Binops first content bool -> do
    first'   <- annotateExpression moduleName first
    content' <- mapM
      ( \(comments, ref, moreComments, e) -> do
        e' <- annotateExpression moduleName e
        return (comments, ref, moreComments, e')
      )
      content
    return $ A region $ Expression.Binops first' content' bool

  Expression.ExplicitList terms trailingComments forceMultiline -> do
    terms' <- mapM
      ( \(comments, (preComments, (e, eol))) -> do
        e' <- annotateExpression moduleName e
        return (comments, (preComments, (e', eol)))
      )
      terms
    return $ A region $ Expression.ExplicitList terms'
                                                trailingComments
                                                forceMultiline

  Expression.Range (AST.Commented lComm1 leftE lComm2) (AST.Commented rComm1 rightE rComm2) bool
    -> do
      leftE'  <- annotateExpression moduleName leftE
      rightE' <- annotateExpression moduleName rightE

      return $ A region $ Expression.Range
        (AST.Commented lComm1 leftE' lComm2)
        (AST.Commented rComm1 rightE' rComm2)
        bool

  Expression.Case (AST.Commented preOf of_ postOf, flag) patterns -> do
    of_'      <- annotateExpression moduleName of_
    patterns' <- mapM
      ( \(pat, (comments, body)) -> do
        incrementComplexity
        body'  <- annotateExpression moduleName body
        body'' <- annotation CaseBranch moduleName body'
        return (pat, (comments, body''))
      )
      patterns
    decrementComplexity
    pure $ A region $ Expression.Case (AST.Commented preOf of_' postOf, flag)
                                      patterns'

  Expression.If ifClause otherClauses (comments, elseExpr) ->
    let
      annotateIfClause
        :: Expression.IfClause -> State AnnotationStore Expression.IfClause
      annotateIfClause (AST.Commented preCond cond postCond, AST.Commented preBody body postBody)
        = do
          incrementComplexity
          cond'  <- annotateExpression moduleName cond
          body'  <- annotateExpression moduleName body
          body'' <- annotation IfElseBranch moduleName body'

          return
            ( AST.Commented preCond cond'  postCond
            , AST.Commented preBody body'' postBody
            )
    in
      do
        ifClause'     <- annotateIfClause ifClause
        otherClauses' <- mapM
          ( \(clauseComments, clause) -> do
            clause' <- annotateIfClause clause
            return (clauseComments, clause')
          )
          otherClauses
        elseExpr' <-
          annotateExpression moduleName elseExpr
            >>= annotation IfElseBranch moduleName

        pure $ A region $ Expression.If ifClause'
                                        otherClauses'
                                        (comments, elseExpr')

  Expression.Tuple entries bool -> do
    entries' <- mapM
      ( \(AST.Commented pre e post) -> do
        e' <- annotateExpression moduleName e
        return $ AST.Commented pre e' post
      )
      entries

    return $ A region $ Expression.Tuple entries' bool

  Expression.Let decls comments body -> do
    decls' <- mapM (annotateLetDeclaration moduleName) decls
    body'  <- annotateExpression moduleName body

    pure $ A region $ Expression.Let decls' comments body'

  Expression.Access record accessor -> do
    record' <- annotateExpression moduleName record
    return $ A region $ Expression.Access record' accessor

  Expression.Lambda pats comments body bool -> do
    innerComplexity
    body'          <- annotateExpression moduleName body
    bodyComplexity <- popComplexity
    body''         <- annotationWithRegion (LambdaBody bodyComplexity)
                                           moduleName
                                           region
                                           body'

    pure $ A region $ Expression.Lambda pats comments body'' bool

  Expression.Record base fields trailingComments forceMultiline -> do
    fields' <- mapM
      ( \(a, (b, (AST.Pair k (preComm, e) forceMulti, c))) -> do
        e' <- annotateExpression moduleName e
        return (a, (b, (AST.Pair k (preComm, e') forceMulti, c)))
      )
      fields

    return $ A region $ Expression.Record base
                                          fields'
                                          trailingComments
                                          forceMultiline

  Expression.Literal        _ -> return expression

  Expression.VarExpr        _ -> return expression

  Expression.TupleFunction  _ -> return expression

  Expression.Unit           _ -> return expression

  Expression.AccessFunction _ -> return expression

  Expression.GLShader       _ -> return expression


annotateLetDeclaration
  :: String
  -> Expression.LetDeclaration
  -> State AnnotationStore Expression.LetDeclaration
annotateLetDeclaration moduleName decl = case decl of
  Expression.LetDefinition pat@(A patRegion _) pats comments expression@(A expRegion _)
    -> do
      innerComplexity
      let letRegion = Region (start patRegion) (end expRegion)
      expression'    <- annotateExpression moduleName expression
      bodyComplexity <- popComplexity
      expression''   <- annotationWithRegion (LetDeclaration bodyComplexity)
                                             moduleName
                                             letRegion
                                             expression'

      return $ Expression.LetDefinition pat pats comments expression''
  _ -> return decl


incrementComplexity :: State AnnotationStore ()
incrementComplexity =
  State.modify (\store -> store { current = 1 + current store })


decrementComplexity :: State AnnotationStore ()
decrementComplexity =
  State.modify (\store -> store { current = current store - 1 })


resetComplexity :: State AnnotationStore ()
resetComplexity = State.modify (\store -> store { current = 1 })


innerComplexity :: State AnnotationStore ()
innerComplexity = State.modify
  (\store -> store { current = 0, stack = current store : stack store })


popComplexity :: State AnnotationStore Int
popComplexity = do
  inner <- State.gets current
  State.modify
    ( \store -> store { current = inner + (head $ stack store)
                      , stack   = tail $ stack store
                      }
    )
  return inner


type Complexity = Int


data Annotation
  = Declaration String Complexity Region
  | LetDeclaration Complexity Region
  | LambdaBody Complexity Region
  | IfElseBranch Region
  | CaseBranch Region


data AnnotationStore = AnnotationStore
  { annotations :: [Annotation]
  , current     :: Int
  , stack       :: [Int]
  , modul       :: String
  , count       :: Int64
  }


annotationToJson :: Annotation -> Json.JSValue
annotationToJson ann =
  Json.makeObj
    $ let regionToJson :: Region -> [(String, Json.JSValue)]
          regionToJson Region { start, end } =
            [("from", Json.showJSON start), ("to", Json.showJSON end)]
      in  case ann of
            Declaration n c r ->
              ("type", Json.showJSON "declaration")
                : ("name"      , Json.showJSON n)
                : ("complexity", Json.showJSON c)
                : regionToJson r
            LetDeclaration c r ->
              ("type", Json.showJSON "letDeclaration")
                : ("complexity", Json.showJSON c)
                : regionToJson r
            LambdaBody c r ->
              ("type", Json.showJSON "lambdaBody")
                : ("complexity", Json.showJSON c)
                : regionToJson r
            IfElseBranch r ->
              ("type", Json.showJSON "ifElseBranch") : regionToJson r
            CaseBranch r ->
              ("type", Json.showJSON "caseBranch") : regionToJson r


storeToJson :: AnnotationStore -> Json.JSValue
storeToJson AnnotationStore { annotations, modul } = Json.makeObj
  [ ( "annotations"
    , annotations |> List.reverse |> List.map annotationToJson |> Json.JSArray
    )
  , ("module", Json.showJSON modul)
  ]


register :: Annotation -> AnnotationStore -> (Int64, AnnotationStore)
register ann store =
  (count store, store { annotations = ann : annotations store })


emptyStore :: String -> AnnotationStore
emptyStore moduleName = AnnotationStore
  { annotations = []
  , current     = 1
  , stack       = []
  , modul       = moduleName
  , count       = 0
  }
