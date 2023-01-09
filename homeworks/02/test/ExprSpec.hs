{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ExprSpec (spec) where

import Control.Monad (unless)
import Data.ByteString.Char8 qualified as ByteString
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Expr
import Instances ()
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Extra (withTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess, prop)
import Test.QuickCheck
import Prelude hiding (lookup)
import Prelude qualified

spec :: Spec
spec = parallel do
  maybeAndThenSpec
  lookupSpec
  freeVarsSpec
  evalSpec
  compileSpec
  partialEvalSpec

maybeAndThenSpec :: Spec
maybeAndThenSpec = describe "maybeAndThen" do
  prop "law left identity" \n (Fn (f :: Integer -> Maybe Integer)) ->
    maybeAndThen (Just n) f `shouldBe` f n
  prop "law right identity" \(mn :: Maybe Integer) ->
    maybeAndThen mn Just `shouldBe` mn
  prop
    "law associativity"
    \(mn :: Maybe Integer)
     (Fn (f :: Integer -> Maybe Integer))
     (Fn (g :: Integer -> Maybe Integer)) ->
        maybeAndThen (maybeAndThen mn f) g `shouldBe` maybeAndThen mn (\x -> maybeAndThen (f x) g)
  it "doesn't run the function on Nothing" do
    maybeAndThen Nothing (\_ -> Just ()) `shouldBe` Nothing
  it "runs the function on Just" do
    maybeAndThen (Just 3) (Just . succ) `shouldBe` Just (4 :: Integer)

lookupSpec :: Spec
lookupSpec = describe "lookup" do
  modifyMaxSuccess (\_ -> 10000) $ prop "works like standard lookup" \strings -> do
    vals <- generate $ (`vectorOf` arbitrary) =<< chooseInt (0, length strings)
    let xs = zip strings vals
    str <- generate $ oneof [arbitrary, elements strings]
    lookup str xs `shouldBe` Prelude.lookup str xs

freeVarsSpec :: Spec
freeVarsSpec = describe "freeVars" do
  prop "works on variables" \str ->
    freeVars (Var str) `shouldMatchList` [str]
  prop "works on values" \n ->
    freeVars (Val n) `shouldBe` []
  it "works on a treelike expression" do
    freeVars (If (Var "x") (Val 5) (Var "y")) `shouldMatchList` ["x", "y"]
  it "works with sum" do
    freeVars (Sum "i" (Var "x") (If (Var "i") (Var "y") (Var "z")))
      `shouldMatchList` ["x", "y", "z"]
  it "works with the sum variable in the limit expression" do
    freeVars (Sum "i" (Var "i") (If (Var "i") (Var "y") (Var "z")))
      `shouldMatchList` ["i", "y", "z"]

evalSpec :: Spec
evalSpec = describe "eval" do
  prop "works for Val regardless of the context" \ctxt n ->
    eval ctxt (Val n) `shouldBe` Just n
  prop "works for Var with the var in the context" \ctxt x n ->
    eval ((x, n) : ctxt) (Var x) `shouldBe` Just n
  prop "works for Var without the var in the context" \ctxt x ->
    eval (filter ((/= x) . fst) ctxt) (Var x) `shouldBe` Nothing
  modifyExpr $ prop "works if all the vars are given assignments" \expr -> do
    ctxt <- ctxtFor expr
    eval ctxt expr `shouldSatisfy` isJust

  describe "If" do
    it "works on an example with the then branch" do
      eval [] (If (Val 0) (Val 6) (Val 9)) `shouldBe` Just 6
    it "works on an example with the else branch" do
      eval [] (If (Val 42) (Val 6) (Val 9)) `shouldBe` Just 9
    it "works on an example with the then branch, ignoring the else branch" do
      eval [] (If (Val 0) (Val 6) (Var "x")) `shouldBe` Just 6
    it "works on an example with the else branch, ignoring the then branch" do
      eval [] (If (Val 42) (Var "x") (Val 9)) `shouldBe` Just 9
    it "works on an example with an unknown var in the cond" do
      eval [] (If (Var "x") (Val 6) (Val 9)) `shouldBe` Nothing
    it "works on an example with a known var in the cond" do
      eval [("x", 0)] (If (Var "x") (Val 6) (Val 9)) `shouldBe` Just 6

  describe "SumList" do
    it "works on an example" do
      eval [] (SumList [Val 5, Val 6, Val 7]) `shouldBe` Just 18
    it "works on an example with variables" do
      eval [] (SumList [Val 5, Val 6, Var "x"]) `shouldBe` Nothing
    it "works on an example with variables" do
      eval [("x", 5)] (SumList [Val 5, Val 6, Var "x"]) `shouldBe` Just 16

  describe "Sum" do
    it "works on an example for a constant inside expr" do
      eval [] (Sum "i" (Val 10) (Val 1)) `shouldBe` Just 11
    it "works on an example for a non-constant inside expr" do
      eval [] (Sum "i" (Val 10) (Var "i")) `shouldBe` Just 55
    it "works on an example for with a free variable" do
      eval [("x", 2)] (Sum "i" (Val 10) (Var "x")) `shouldBe` Just 22
    it "results in 0 on empty intervals, regardless of contents" do
      eval [] (Sum "i" (Val (-1)) (Var "y")) `shouldBe` Just 0
    it "allows the bound variable to shadow the context" do
      eval [("i", 69)] (Sum "i" (Val 10) (Var "i")) `shouldBe` Just 55
    it "allows the limit expression to contain the bound variable as a free var" do
      eval [] (Sum "i" (Var "i") (Var "i")) `shouldBe` Nothing
    it "allows the limit expression to contain the bound variable as a free var" do
      eval [("i", 10)] (Sum "i" (Var "i") (Var "i")) `shouldBe` Just 55
    it "allows inner sum bound vars to shadow outer ones" do
      eval [] (Sum "i" (Val 2) (Sum "i" (Val 10) (Var "i"))) `shouldBe` Just 165

partialEvalSpec :: Spec
partialEvalSpec = pdescribe "partialEval" do
  modifyExpr $ prop "for a random ctxt, eval ctxt (partialEval expr) == eval ctxt expr" \ctxt expr ->
    eval ctxt (partialEval expr) `shouldBe` eval ctxt expr

  modifyMaxSuccess (\_ -> 200000) $
    modifyMaxSize (\_ -> 8) $
      prop "for a ctxt with all free vars bound, eval ctxt (partialEval expr) == eval ctxt expr" \expr -> do
        ctxt <- ctxtFor expr
        eval ctxt (partialEval expr) `shouldBe` eval ctxt expr

  modifyExpr $ prop "idempotence: partialEval (partialEval expr) == partialEval expr" \expr -> do
    partialEval (partialEval expr) `shouldBe` partialEval expr

  prop "partial evalling shouldn't change Vals" \n ->
    partialEval (Val n) `shouldBe` Val n

  prop "partial evalling shouldn't change Vars" \str ->
    partialEval (Var str) `shouldBe` Var str

  modifyExpr $ prop "partial evalling with no vars should be the same as evalling" \expr ->
    null (freeVars expr) ==> Just (partialEval expr) `shouldBe` fmap Val (eval [] expr)

  modifyExpr $ prop "partial evalling shouldn't increase the free variables" \expr ->
    Set.fromList (freeVars $ partialEval expr) `shouldSubset` Set.fromList (freeVars expr)

  describe "If" do
    it "works on an example for If known cond" do
      partialEval (If (Val 42) (Val 6) (Val 9)) `shouldBe` Val 9

    it "works on an example for If with known cond and unknown branches" do
      partialEval (If (Val 42) (Var "y") (Var "x")) `shouldBe` Var "x"

    it "works on an example for If with known cond and unknown branches, ignoring the not reached branch" do
      partialEval (If (Val 0) (Val 0) (Var "x")) `shouldBe` Val 0

    it "works on an example for If with unknown cond" do
      partialEval (If (Var "x") (Val 0) (Val 69)) `shouldBe` If (Var "x") (Val 0) (Val 69)

    it "evals deeply in the cond" do
      partialEval (If (If (Val 0) (Var "x") (Val 69)) (Val 0) (Val 69)) `shouldBe` If (Var "x") (Val 0) (Val 69)

    it "evals deeply in both cases with an unknown cond" do
      partialEval (If (Var "x") (If (Val 0) (Var "x") (Val 69)) (If (Val 1) (Var "x") (Val 69))) `shouldBe` If (Var "x") (Var "x") (Val 69)

  describe "Sum" do
    pit "partial evals the limit expr for a Sum" do
      partialEval (Sum "i" (If (Val 0) (Var "x") (Val 69)) (Var "x")) `shouldBe` Sum "i" (Var "x") (Var "x")

    pit "partial evals the inside expr for a Sum" do
      partialEval (Sum "i" (Var "x") (If (Val 0) (Var "x") (Val 69))) `shouldBe` Sum "i" (Var "x") (Var "x")

    pit "evals the entire sum if the limit and inside exprs are known" do
      partialEval (Sum "i" (Val 10) (Val 1)) `shouldBe` Val 11

    pit "evals the entire sum if the limit expr is known and the inside expression only has bound vars" do
      partialEval (Sum "i" (Val 10) (Var "i")) `shouldBe` Val 55

    pit "removes the sum if the limit expr is known but the inside expression has free vars" do
      partialEval (Sum "i" (Val 10) (Var "x")) `shouldSatisfy` not . isSum
  where
    pit :: Example a => String -> a -> SpecWith (Arg a)
    pit =
      if solvingSum then it else xit
    pdescribe :: String -> SpecWith a -> SpecWith a
    pdescribe =
      if solvingPartialEval then describe else xdescribe

compileSpec :: Spec
compileSpec = around withTempFile $ pdescribe "racket compiler" do
  modifyExprCompile $ it "behaves like the interpreter for an expression with no free vars" \filepath -> property \expr ->
    null (freeVars expr) ==> do
      ByteString.writeFile filepath $ ByteString.pack $ printRacketProgram [] $ MkRacketProgram [compileToRacket expr]
      runRacketExpect filepath $ eval [] expr
  modifyExprCompile $ it "behaves like the interpreter for an expression with all bound vars" \filepath -> property \expr -> do
    ctxt <- ctxtFor expr
    ByteString.writeFile filepath $ ByteString.pack $ printRacketProgram ctxt $ MkRacketProgram [compileToRacket expr]
    runRacketExpect filepath $ eval ctxt expr
  where
    modifyExprCompile :: SpecWith a -> SpecWith a
    modifyExprCompile = modifyMaxSuccess (\_ -> 30) . modifyMaxSize (\_ -> 16)

    pdescribe :: String -> SpecWith a -> SpecWith a
    pdescribe =
      if solvingCompiler then describe else xdescribe

runRacketExpect :: String -> Maybe Integer -> IO ()
runRacketExpect filepath expected = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "racket" [filepath] ""
  exitCode `shouldBe` ExitSuccess
  stderr `shouldBe` ""
  Just (read stdout) `shouldBe` expected

ctxtFor :: Expr -> IO Context
ctxtFor expr = do
  let freeVars' = nub $ freeVars expr
  ns <- generate $ vectorOf (length freeVars') arbitrary
  pure $ zip freeVars' ns

isSum :: Expr -> Bool
isSum Sum {} = True
isSum _ = False

modifyExpr :: SpecWith a -> SpecWith a
modifyExpr = modifyMaxSuccess (\_ -> 100000) . modifyMaxSize (\_ -> 8)

shouldSubset :: (HasCallStack, Show a, Ord a) => Set a -> Set a -> Expectation
xs `shouldSubset` ys = expectTrue (show xs ++ "\nwas not a subset of\n" ++ show ys) (xs `Set.isSubsetOf` ys)

expectTrue :: HasCallStack => String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)
