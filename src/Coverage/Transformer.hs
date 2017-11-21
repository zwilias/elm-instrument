{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import           Reporting.Annotation (Located (..))
import           Reporting.Region
-- import Debug.Trace as Debug


transform :: M.Module -> M.Module
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
    (updatedBody, store) =
      State.runState (mapM (annotate moduleName) (M.body modul)) emptyStore
  in
    modul { M.imports = (comments, updatedImports)
          , M.body    = initCoverageDeclaration moduleName store : updatedBody
          }


annotate :: String -> Declaration.Decl -> State AnnotationStore Declaration.Decl
annotate moduleName declaration' = case declaration' of
  Declaration.Decl (A region declaration_) -> do
    declaration <- annotateDeclaration moduleName declaration_
    return $ Declaration.Decl $ A region declaration

  _ -> return declaration'


data Annotation
  = Expression
  | Declaration
  | CaseBranch
  | IfElseBranch
  | LambdaBody
  | LetDeclaration

emptyRegion :: Region
emptyRegion = Region (Position 0 0) (Position 0 0)


toVarRef :: Region -> Annotation -> Expression.Expr
toVarRef region ann =
  let identifier name = A region $ Expression.VarExpr $ Var.VarRef
        [AST.UppercaseIdentifier "Coverage"]
        (AST.LowercaseIdentifier name)
  in  case ann of
        Expression     -> identifier "expression"

        Declaration    -> identifier "declaration"

        CaseBranch     -> identifier "caseBranch"

        IfElseBranch   -> identifier "ifElseBranch"

        LambdaBody     -> identifier "lambdaBody"

        LetDeclaration -> identifier "letDeclaration"


plain :: a -> AST.PreCommented a
plain a = ([], a)


annotation
  :: Annotation
  -> String
  -> Expression.Expr
  -> State AnnotationStore Expression.Expr
annotation annotationType moduleName expression@(A region _) = do
  store <- State.get
  let (count, store') = registerA annotationType region store
  State.put store'

  return $ A region $ Expression.App
    (toVarRef region annotationType)
    [ plain $ A region $ Expression.Literal $ AST.Str moduleName False
    , plain $ A region $ Expression.Literal $ AST.IntNum count AST.DecimalInt
    , plain expression
    ]
    (AST.FAJoinFirst AST.JoinAll)


annotateDeclaration
  :: String
  -> Declaration.Declaration
  -> State AnnotationStore Declaration.Declaration
annotateDeclaration moduleName declaration = case declaration of
  Declaration.Definition pat pats comments body -> do
    annotatedExpressions <- annotateExpression moduleName body
    annotatedBody <- annotation Declaration moduleName annotatedExpressions
    return $ Declaration.Definition pat pats comments annotatedBody

  _ -> return declaration


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
    annotation Expression moduleName $ A region $ Expression.App inner'
                                                                 parts'
                                                                 faApp

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
    annotation Expression moduleName $ A region $ Expression.Binops first'
                                                                    content'
                                                                    bool

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
        body'  <- annotateExpression moduleName body
        body'' <- annotation CaseBranch moduleName body'
        return (pat, (comments, body''))
      )
      patterns

    let caseOf = A region
          $ Expression.Case (AST.Commented preOf of_' postOf, flag) patterns'
    annotation Expression moduleName caseOf

  Expression.If ifClause otherClauses (comments, elseExpr) ->
    let
      annotateIfClause
        :: Expression.IfClause -> State AnnotationStore Expression.IfClause
      annotateIfClause (AST.Commented preCond cond postCond, AST.Commented preBody body postBody)
        = do
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

        annotation Expression moduleName $ A region $ Expression.If
          ifClause'
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
    return $ A region $ Expression.Let decls' comments body'

  Expression.Access record accessor -> do
    record' <- annotateExpression moduleName record
    return $ A region $ Expression.Access record' accessor

  Expression.Lambda pats comments body bool -> do
    body' <-
      annotateExpression moduleName body >>= annotation LambdaBody moduleName

    return $ A region $ Expression.Lambda pats comments body' bool

  Expression.Record base fields trailingComments forceMultiline -> do
    fields' <- mapM
      ( \(a, (b, (AST.Pair k (preComm, e) forceMulti, c))) -> do
        e' <- annotateExpression moduleName e
        return (a, (b, (AST.Pair k (preComm, e') forceMulti, c)))
      )
      fields

    let record = A region
          $ Expression.Record base fields' trailingComments forceMultiline

    case base of
      Just _ -> annotation Expression moduleName record
      _      -> return record

  Expression.Literal _ ->
      -- TODO: gotta draw a line somewhere. Expression or not?
      -- annotation Expression moduleName expression
    return expression

  Expression.VarExpr _ ->
      -- TODO: should this really "count" as an expression?
      -- annotation Expression moduleName expression
    return expression

  Expression.TupleFunction  _ -> return expression

  Expression.Unit           _ -> return expression

  Expression.AccessFunction _ -> return expression

  Expression.GLShader       _ -> return expression


annotateLetDeclaration
  :: String
  -> Expression.LetDeclaration
  -> State AnnotationStore Expression.LetDeclaration
annotateLetDeclaration moduleName decl = case decl of
  Expression.LetDefinition pat pats comments expression -> do
    expression' <-
      annotateExpression moduleName expression
        >>= annotation LetDeclaration moduleName

    return $ Expression.LetDefinition pat pats comments expression'
  _ -> return decl


initCoverageDeclaration :: String -> AnnotationStore -> Declaration.Decl
initCoverageDeclaration moduleName store =
  Declaration.Decl $ A emptyRegion $ Declaration.Definition
    ( A emptyRegion $ Pattern.VarPattern $ AST.LowercaseIdentifier
      "initCoverage"
    )
    []
    []
    (initCoverageBody moduleName store)


initCoverageBody :: String -> AnnotationStore -> Expression.Expr
initCoverageBody moduleName store = A emptyRegion $ Expression.App
  ( A emptyRegion $ Expression.VarExpr $ Var.VarRef
    [AST.UppercaseIdentifier "Coverage"]
    (AST.LowercaseIdentifier "init")
  )
  [ ([], A emptyRegion $ Expression.Literal $ AST.Str moduleName False)
  , ([], annotationStoreToRecord store)
  ]
  AST.FASplitFirst


annotationStoreToRecord :: AnnotationStore -> Expression.Expr
annotationStoreToRecord store = A emptyRegion $ Expression.Record
  Nothing
  ( toSequence
    [ annotationTypeStoreToPair "expressions"     (expressions store)
    , annotationTypeStoreToPair "caseBranches"    (caseBranches store)
    , annotationTypeStoreToPair "declarations"    (declarations store)
    , annotationTypeStoreToPair "ifElseBranches"  (ifElseBranches store)
    , annotationTypeStoreToPair "lambdaBodies"    (lambdaBodies store)
    , annotationTypeStoreToPair "letDeclarations" (letDeclarations store)
    ]
  )
  []
  (AST.ForceMultiline True)


annotationTypeStoreToPair
  :: String
  -> AnnotationTypeStore
  -> AST.Pair AST.LowercaseIdentifier Expression.Expr
annotationTypeStoreToPair name store = AST.Pair
  (AST.LowercaseIdentifier name, [])
  ([]                          , regionsToIdentifiers $ regions store)
  (AST.ForceMultiline False)


regionsToIdentifiers :: [Region] -> Expression.Expr
regionsToIdentifiers regions =
  let identifiers =
        List.foldl' (\acc region -> regionToRecord region : acc) [] regions
  in  A emptyRegion $ Expression.ExplicitList (toSequence identifiers)
                                              []
                                              (AST.ForceMultiline True)


toSequence :: [a] -> AST.Sequence a
toSequence = List.map (\item -> ([], ([], (item, Nothing))))


regionToRecord :: Region -> Expression.Expr
regionToRecord Region { start, end } =
  let ctor :: Expression.Expr
      ctor = A emptyRegion $ Expression.VarExpr $ Var.TagRef
        [AST.UppercaseIdentifier "Coverage"]
        (AST.UppercaseIdentifier "Identifier")
  in  A emptyRegion $ Expression.App
        ctor
        [([], positionToTuple start), ([], positionToTuple end)]
        (AST.FAJoinFirst AST.JoinAll)


positionToTuple :: Position -> Expression.Expr
positionToTuple Position { line, column } = A emptyRegion $ Expression.Tuple
  [ AST.Commented
    []
    ( A emptyRegion
        (Expression.Literal $ AST.IntNum (fromIntegral line) AST.DecimalInt)
    )
    []
  , AST.Commented
    []
    ( A emptyRegion
        (Expression.Literal $ AST.IntNum (fromIntegral column) AST.DecimalInt)
    )
    []
  ]
  False


data AnnotationTypeStore = AnnotationTypeStore
  { regions :: [Region]
  , counter :: Int64
  }


data AnnotationStore = AnnotationStore
  { expressions     :: AnnotationTypeStore
  , declarations    :: AnnotationTypeStore
  , caseBranches    :: AnnotationTypeStore
  , ifElseBranches  :: AnnotationTypeStore
  , lambdaBodies    :: AnnotationTypeStore
  , letDeclarations :: AnnotationTypeStore
  }


register :: Region -> AnnotationTypeStore -> (Int64, AnnotationTypeStore)
register region store =
  ( counter store
  , store { regions = region : regions store, counter = counter store + 1 }
  )


registerA :: Annotation -> Region -> AnnotationStore -> (Int64, AnnotationStore)
registerA ann = case ann of
  Expression     -> registerExpression

  Declaration    -> registerDeclaration

  CaseBranch     -> registerCaseBranch

  IfElseBranch   -> registerIfElseBranch

  LambdaBody     -> registerLambdaBody

  LetDeclaration -> registerletDeclaration


registerExpression :: Region -> AnnotationStore -> (Int64, AnnotationStore)
registerExpression region store =
  let (i, newExpressions) = register region $ expressions store
  in  (i, store { expressions = newExpressions })


registerCaseBranch :: Region -> AnnotationStore -> (Int64, AnnotationStore)
registerCaseBranch region store =
  let (i, newBranches) = register region $ caseBranches store
  in  (i, store { caseBranches = newBranches })


registerIfElseBranch :: Region -> AnnotationStore -> (Int64, AnnotationStore)
registerIfElseBranch region store =
  let (i, newBranches) = register region $ ifElseBranches store
  in  (i, store { ifElseBranches = newBranches })


registerDeclaration :: Region -> AnnotationStore -> (Int64, AnnotationStore)
registerDeclaration region store =
  let (i, newDeclarations) = register region $ declarations store
  in  (i, store { declarations = newDeclarations })

registerLambdaBody :: Region -> AnnotationStore -> (Int64, AnnotationStore)
registerLambdaBody region store =
  let (i, newLambdas) = register region $ lambdaBodies store
  in  (i, store { lambdaBodies = newLambdas })


registerletDeclaration :: Region -> AnnotationStore -> (Int64, AnnotationStore)
registerletDeclaration region store =
  let (i, newDecls) = register region $ letDeclarations store
  in  (i, store { letDeclarations = newDecls })


emptyStore :: AnnotationStore
emptyStore =
  let emptyTypeStore = AnnotationTypeStore
        { regions = []
        , counter = 0
        }
  in  AnnotationStore
        { expressions     = emptyTypeStore
        , declarations    = emptyTypeStore
        , caseBranches    = emptyTypeStore
        , ifElseBranches  = emptyTypeStore
        , lambdaBodies    = emptyTypeStore
        , letDeclarations = emptyTypeStore
        }
