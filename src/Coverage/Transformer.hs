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
import qualified Data.List.Split      as List
import qualified Data.Map.Strict      as Map
import           Elm.Utils            ((|>))
import           Reporting.Annotation (Located (..))
import           Reporting.Region


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
    declaration <- annotateDeclaration moduleName region declaration_
    return $ Declaration.Decl $ A region declaration

  _ -> return declaration'


data Annotation
  = Declaration
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
        Declaration    -> identifier "declaration"

        CaseBranch     -> identifier "caseBranch"

        IfElseBranch   -> identifier "ifElseBranch"

        LambdaBody     -> identifier "lambdaBody"

        LetDeclaration -> identifier "letDeclaration"


plain :: a -> AST.PreCommented a
plain a = ([], a)


annotation
  :: Registerer
  -> Annotation
  -> String
  -> Expression.Expr
  -> State AnnotationStore Expression.Expr
annotation registerer annotationType moduleName body@(A region _) =
  annotationWithRegion registerer annotationType moduleName region body


annotationWithRegion
  :: Registerer
  -> Annotation
  -> String
  -> Region
  -> Expression.Expr
  -> State AnnotationStore Expression.Expr
annotationWithRegion registerer annotationType moduleName region' body@(A region body')
  = do
    store <- State.get
    let (count, store') = registerer region' store
    State.put store'

    let plainLit   = plain . A region . Expression.Literal
    let underscore = A region Pattern.Anything
    let letBody = A region $ Expression.App
          (toVarRef region annotationType)
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
      (registerDeclaration (patternToName patName) complexity)
      Declaration
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
        incrementComplexity -- Every branch increases the complexity
        body'  <- annotateExpression moduleName body
        body'' <- annotation registerCaseBranch CaseBranch moduleName body'
        return (pat, (comments, body''))
      )
      patterns
    decrementComplexity -- However, the total complexity of a `case..of` is the
                                                           -- number of branches
                                                           -- - 1. Why, you ask?
                                                           -- Same reason `else`
                                                           -- doesn't add
                                                           -- complexity. Also, reasons.
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
          body'' <- annotation registerIfElseBranch
                               IfElseBranch
                               moduleName
                               body'

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
            >>= annotation registerIfElseBranch IfElseBranch moduleName

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
    body''         <- annotationWithRegion (registerLambdaBody bodyComplexity)
                                           LambdaBody
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
      expression''   <- annotationWithRegion
        (registerletDeclaration bodyComplexity)
        LetDeclaration
        moduleName
        letRegion
        expression'

      return $ Expression.LetDefinition pat pats comments expression''
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
initCoverageBody moduleName store =
  let (complexitySum, complexityCount) = List.foldl'
        (\(total, count) c -> (total + extractComplexity c, count + 1))
        (0, 0)
        (complexity store)
  in  A emptyRegion $ Expression.App
        ( A emptyRegion $ Expression.VarExpr $ Var.VarRef
          [AST.UppercaseIdentifier "Coverage"]
          (AST.LowercaseIdentifier "init")
        )
        [ ( []
          , A emptyRegion $ Expression.Literal $ AST.IntNum
            (fromIntegral (complexitySum - complexityCount + 1))
            AST.DecimalInt
          )
        , ([], A emptyRegion $ Expression.Literal $ AST.Str moduleName False)
        , ([], annotationStoreToRecord store)
        ]
        AST.FASplitFirst


annotationStoreToRecord :: AnnotationStore -> Expression.Expr
annotationStoreToRecord store = A emptyRegion $ Expression.Record
  Nothing
  ( toSequence
    [ annotationTypeStoreToPairN "caseBranches"    (caseBranches store)
    , annotationTypeStoreToPairD "declarations"    (declarations store)
    , annotationTypeStoreToPairN "ifElseBranches"  (ifElseBranches store)
    , annotationTypeStoreToPairC "lambdaBodies"    (lambdaBodies store)
    , annotationTypeStoreToPairC "letDeclarations" (letDeclarations store)
    ]
  )
  []
  (AST.ForceMultiline True)


annotationTypeStoreToPairN
  :: String
  -> AnnotationTypeStore a
  -> AST.Pair AST.LowercaseIdentifier Expression.Expr
annotationTypeStoreToPairN name store =
  annotationTypeStoreToPair name store (regionsToIdentifiers regionToRecord)


annotationTypeStoreToPairC
  :: String
  -> AnnotationTypeStore Int
  -> AST.Pair AST.LowercaseIdentifier Expression.Expr
annotationTypeStoreToPairC name store =
  annotationTypeStoreToPair name store (regionsToIdentifiers regionToRecordC)


annotationTypeStoreToPairD
  :: String
  -> AnnotationTypeStore (String, Int)
  -> AST.Pair AST.LowercaseIdentifier Expression.Expr
annotationTypeStoreToPairD name store =
  annotationTypeStoreToPair name store (regionsToIdentifiers regionToRecordD)


annotationTypeStoreToPair
  :: String
  -> AnnotationTypeStore a
  -> ([(Region, a)] -> Expression.Expr)
  -> AST.Pair AST.LowercaseIdentifier Expression.Expr
annotationTypeStoreToPair name store handleRegions = AST.Pair
  (AST.LowercaseIdentifier name, [])
  ([]                          , handleRegions $ regions store)
  (AST.ForceMultiline False)


regionsToIdentifiers
  :: (a -> Region -> Expression.Expr) -> [(Region, a)] -> Expression.Expr
regionsToIdentifiers toRecord regions =
  let identifiers :: [[Expression.Expr]]
      identifiers = List.chunksOf 200
        $ List.foldl' (\acc (region, a) -> toRecord a region : acc) [] regions

      concatRef :: Var.Ref
      concatRef = Var.OpRef $ AST.SymbolIdentifier "++"

      listExpr :: [Expression.Expr] -> Located Expression.Expr'
      listExpr xs = A emptyRegion
        $ Expression.ExplicitList (toSequence xs) [] (AST.ForceMultiline True)
  in  case identifiers of
        []   -> listExpr []
        [x]  -> listExpr x
        x:xs -> A emptyRegion $ Expression.Binops
          (listExpr x)
          (List.map (\x' -> ([], concatRef, [], listExpr x')) xs)
          True


toSequence :: [a] -> AST.Sequence a
toSequence = List.map (\item -> ([], ([], (item, Nothing))))


regionToRecord :: a -> Region -> Expression.Expr
regionToRecord _ Region { start, end } =
  let ctor :: Expression.Expr
      ctor = A emptyRegion $ Expression.VarExpr $ Var.TagRef
        [AST.UppercaseIdentifier "Coverage"]
        (AST.UppercaseIdentifier "Identifier")
  in  A emptyRegion $ Expression.App
        ctor
        [([], positionToTuple start), ([], positionToTuple end)]
        (AST.FAJoinFirst AST.JoinAll)


regionToRecordC :: Int -> Region -> Expression.Expr
regionToRecordC c Region { start, end } =
  let ctor :: Expression.Expr
      ctor = A emptyRegion $ Expression.VarExpr $ Var.TagRef
        [AST.UppercaseIdentifier "Coverage"]
        (AST.UppercaseIdentifier "IdentifierC")
  in  A emptyRegion $ Expression.App
        ctor
        [ ( []
          , A emptyRegion $ Expression.Literal $ AST.IntNum (fromIntegral c)
                                                            AST.DecimalInt
          )
        , ([], positionToTuple start)
        , ([], positionToTuple end)
        ]
        (AST.FAJoinFirst AST.JoinAll)


regionToRecordD :: (String, Int) -> Region -> Expression.Expr
regionToRecordD (name, c) Region { start, end } =
  let ctor :: Expression.Expr
      ctor = A emptyRegion $ Expression.VarExpr $ Var.TagRef
        [AST.UppercaseIdentifier "Coverage"]
        (AST.UppercaseIdentifier "IdentifierD")
  in  A emptyRegion $ Expression.App
        ctor
        [ ([], A emptyRegion $ Expression.Literal $ AST.Str (name) False)
        , ( []
          , A emptyRegion $ Expression.Literal $ AST.IntNum (fromIntegral c)
                                                            AST.DecimalInt
          )
        , ([], positionToTuple start)
        , ([], positionToTuple end)
        ]
        (AST.FAJoinFirst AST.JoinAll)



positionToTuple :: Position -> Expression.Expr
positionToTuple Position { line, column } =
  let toLit :: Int -> Expression.Expr'
      toLit v = Expression.Literal $ AST.IntNum (fromIntegral v) AST.DecimalInt
  in  A emptyRegion $ Expression.Tuple
        [ AST.Commented [] (A emptyRegion $ toLit line)   []
        , AST.Commented [] (A emptyRegion $ toLit column) []
        ]
        False


data AnnotationTypeStore a = AnnotationTypeStore
  { regions :: [(Region, a)]
  , counter :: Int64
  }


data Complexity
    = DeclarationComplexity Region String Int
    | LambdaComplexity Region Int
    | LetDeclarationComplexity Region Int


extractComplexity :: Complexity -> Int
extractComplexity c = case c of
  DeclarationComplexity _ _ v  -> v
  LambdaComplexity         _ v -> v
  LetDeclarationComplexity _ v -> v


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


data AnnotationStore = AnnotationStore
  { declarations    :: AnnotationTypeStore (String, Int)
  , caseBranches    :: AnnotationTypeStore ()
  , ifElseBranches  :: AnnotationTypeStore ()
  , lambdaBodies    :: AnnotationTypeStore Int
  , letDeclarations :: AnnotationTypeStore Int
  , complexity      :: [ Complexity ]
  , current         :: Int
  , stack           :: [ Int ]
  }


type Registerer = Region -> AnnotationStore -> (Int64, AnnotationStore)


registerCaseBranch :: Region -> AnnotationStore -> (Int64, AnnotationStore)
registerCaseBranch region store =
  let (i, newBranches) = register_ () region $ caseBranches store
  in  (i, store { caseBranches = newBranches })


registerIfElseBranch :: Region -> AnnotationStore -> (Int64, AnnotationStore)
registerIfElseBranch region store =
  let (i, newBranches) = register_ () region $ ifElseBranches store
  in  (i, store { ifElseBranches = newBranches })


registerDeclaration
  :: String -> Int -> Region -> AnnotationStore -> (Int64, AnnotationStore)
registerDeclaration name compl region store =
  let (i, newDeclarations) =
        register_ (name, compl) region $ declarations store
  in  ( i
      , store
        { declarations = newDeclarations
        , complexity   = DeclarationComplexity region name compl
          : complexity store
        }
      )


registerLambdaBody
  :: Int -> Region -> AnnotationStore -> (Int64, AnnotationStore)
registerLambdaBody a region store =
  let (i, newLambdas) = register_ a region $ lambdaBodies store
  in  ( i
      , store { lambdaBodies = newLambdas
              , complexity   = LambdaComplexity region a : complexity store
              }
      )


registerletDeclaration
  :: Int -> Region -> AnnotationStore -> (Int64, AnnotationStore)
registerletDeclaration a region store =
  let (i, newDecls) = register_ a region $ letDeclarations store
  in  ( i
      , store
        { letDeclarations = newDecls
        , complexity      = LetDeclarationComplexity region a : complexity store
        }
      )


register_
  :: a -> Region -> AnnotationTypeStore a -> (Int64, AnnotationTypeStore a)
register_ a region store =
  ( counter store
  , store { regions = (region, a) : regions store, counter = counter store + 1 }
  )


emptyStore :: AnnotationStore
emptyStore =
  let emptyTypeStore = AnnotationTypeStore
        { regions = []
        , counter = 0
        }
  in  AnnotationStore
        { declarations    = emptyTypeStore
        , caseBranches    = emptyTypeStore
        , ifElseBranches  = emptyTypeStore
        , lambdaBodies    = emptyTypeStore
        , letDeclarations = emptyTypeStore
        , complexity      = []
        , current         = 1
        , stack           = []
        }
