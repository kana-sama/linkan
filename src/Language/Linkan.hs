{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Linkan where

import Control.Monad.Catch (Exception (displayException), SomeException, catch, throwM, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Language.SExpr (SExpr)
import Language.SExpr qualified as SExpr
import System.Console.Haskeline qualified as Haskeline
import Text.Pretty (Pretty (..))
import Text.Read (readMaybe)

type Scope = Map String Value

newtype Macro = Macro ([Value] -> Eval Value)

type Macros = Map String Macro

type Eval = ReaderT [String] (StateT (Scope, Macros) IO)

data EvalException
  = UnknownAtom String
  | QuoteClosure
  | ImpossibleToEval String
  deriving stock (Show)
  deriving anyclass (Exception)

throw :: EvalException -> Eval a
throw = throwM

data Value
  = Atom String
  | Number Integer
  | List [Value]
  | Closure Scope ([Value] -> Eval Value)

pattern Nil = List []

instance IsString Value where
  fromString = Atom

instance Pretty Value where
  pretty (Atom atom) = atom
  pretty (Number num) = show num
  pretty (List [Atom "quote", x]) = "'" <> pretty x
  pretty (List [Atom "unquote", x]) = "," <> pretty x
  pretty (List vals) = "(" <> unwords [pretty v | v <- vals] <> ")"
  pretty (Closure _ _) = "<closure>"

transpile :: SExpr -> Value
transpile (SExpr.Atom atom) | Just num <- readMaybe atom = Number num
transpile (SExpr.Atom atom) = Atom atom
transpile (SExpr.List exprs) = List [transpile e | e <- exprs]
transpile (SExpr.Quote expr) = List ["quote", transpile expr]
transpile (SExpr.Unquote expr) = List ["unquote", transpile expr]

quote :: Value -> Eval Value
quote (Atom atom) = pure (Atom atom)
quote (List [Atom "unquote", x]) = eval x
quote (List xs) = List <$> traverse quote xs
quote (Number num) = pure (Number num)
quote (Closure _ _) = throw QuoteClosure

localScope :: (Scope -> Scope) -> Eval a -> Eval a
localScope f action = do
  saved <- get
  modify \(scope, macros) -> (f scope, macros)
  result <- action
  put saved
  pure result

withStackTrace :: String -> Eval a -> Eval a
withStackTrace name = local (name :)

withArgs :: [Value] -> [Value] -> Eval a -> Eval a
withArgs names args =
  let (names', args') =
        case break (\case Atom "&" -> True; _ -> False) names of
          (_, []) -> (names, args)
          (names, [Atom "&", Atom rest]) ->
            (names <> [Atom rest], take (length names) args <> [List (drop (length names) args)])
   in localScope (Map.union (Map.fromList [(n, a) | Atom n <- names' | a <- args']))

apply :: Value -> [Value] -> Eval Value
apply f args = do
  Closure scope f <- eval f
  args <- traverse eval args
  localScope (const scope) (f args)

eval :: Value -> Eval Value
eval (Atom atom) =
  gets (Map.lookup atom . fst) >>= \case
    Just value -> pure value
    Nothing -> throw (UnknownAtom atom)
eval (Number num) = pure (Number num)
eval (List [Atom "load", Atom name]) = load name
eval (List [Atom "quote", e]) = quote e
eval (List [Atom "label", Atom name, e]) = do
  Closure scope f <- eval e
  pure (let clos = Closure (Map.insert name clos scope) f in clos)
eval (List [Atom "define*", Atom name, value]) = do
  value <- eval value
  modify \(scope, macros) -> (Map.insert name value scope, macros)
  pure Nil
eval (List [Atom "define-macro*", List (Atom name : names), body]) = do
  let macro = Macro \args -> eval =<< withArgs names args (eval body)
  modify \(scope, macros) -> (scope, Map.insert name macro macros)
  pure Nil
eval (List (Atom "cond" : conds)) = go conds
  where
    go [] = pure Nil
    go (List [cond, body] : conds) =
      eval cond >>= \case
        Atom "true" -> eval body
        _ -> go conds
eval (List [Atom "lambda", List names, body]) = do
  scope <- gets fst
  pure (Closure scope \args -> withArgs names args (eval body))
eval (List (Atom "do" : exprs)) = go exprs
  where
    go [] = pure Nil
    go [x] = eval x
    go (x : xs) = eval x *> go xs
eval (List (Atom f : args)) =
  withStackTrace f do
    gets (Map.lookup f . snd) >>= \case
      Just (Macro macro) -> macro args
      Nothing -> apply (Atom f) args
eval (List (f : args)) =
  withStackTrace (pretty f) do
    apply f args
eval value = throw (ImpossibleToEval (pretty value))

load :: String -> Eval Value
load name = do
  source <- liftIO (readFile (name <> ".lisp"))
  eval (List (Atom "do" : (transpile <$> SExpr.parses source)))
  pure Nil

builtins :: Scope
builtins =
  Map.fromList
    [ ("cons", clos_pure \[a, List b] -> List (a : b)),
      ("car", clos \case [List (x : xs)] -> pure x; other -> liftIO (putStrLn (pretty (head other))) >> error (pretty (head other))),
      ("cdr", clos_pure \[List (x : xs)] -> List xs),
      ("print", clos \[x] -> (liftIO . putStrLn) (pretty x) $> Nil),
      ("list?", clos_pure \case [List _] -> bool True; _ -> bool False),
      ("nil?", clos_pure \[List e] -> bool (null e)),
      ("atom?", clos_pure \case [Atom _] -> bool True; [_] -> bool False),
      ("same-atom?", clos_pure \[Atom a, Atom b] -> bool (a == b)),
      ("number?", clos_pure \case [Number _] -> bool True; [_] -> bool False),
      ("same-number?", clos_pure \[Number a, Number b] -> bool (a == b)),
      ("+", clos_pure \xs -> Number (sum [x' | x <- xs, let Number x' = x]))
    ]
  where
    bool x = Atom (if x then "true" else "false")
    clos f = Closure Map.empty f
    clos_pure f = Closure Map.empty (pure . f)

repl :: IO ()
repl =
  flip evalStateT (builtins, Map.empty) do
    flip runReaderT [] do
      Haskeline.runInputT Haskeline.defaultSettings do
        lift (load "lib/prelude") `catch` handle
        loop
  where
    loop = do
      input <- Haskeline.getInputLine "% "
      case input of
        Nothing -> pure ()
        Just ":q" -> pure ()
        Just ":r" -> do
          lift (load "lib/prelude")
          loop
        Just line -> do
          state <- lift get
          result <- (lift . eval . transpile . SExpr.parse) line `catch` handle
          Haskeline.outputStrLn ("> " <> pretty result)
          loop

handle :: SomeException -> Haskeline.InputT Eval Value
handle e = do
  stacktrace <- lift ask
  liftIO . putStrLn . show . fmap pretty =<< lift (gets fst)
  Haskeline.outputStrLn "Stacktrace:"
  for_ stacktrace \name -> do
    Haskeline.outputStrLn ("- " <> name)
  Haskeline.outputStrLn (displayException e)
  pure Nil
