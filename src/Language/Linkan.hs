{-# LANGUAGE PatternSynonyms #-}

module Language.Linkan where

import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Language.Linkan.Value (Value (..), pattern ValueList)
import Language.SExpr.Parser (parse, parses)
import Language.SExpr.Type (SExpr (..))
import Text.Pretty (Pretty (..))

data Macro = Macro [String] (Maybe String) SExpr

type Context = (?scope :: Map String Value, ?macros :: Map String Macro)

quote :: Context => SExpr -> IO Value
quote (Atom x) = pure (Symbol x)
quote (List ["unquote", x]) = eval x pure
quote (List xs) = foldr Cons "nil" <$> traverse quote xs
quote (Quote e) = (\e -> Cons "quote" (Cons e "nil")) <$> quote e
quote (Unquote x) = eval x pure

unquote :: Value -> SExpr
unquote (ValueList xs Nothing) = List [unquote x | x <- xs]
unquote (Symbol x) = Atom x
unquote v = error ("invalid value to unquote: " <> pretty v)

ensureAtom :: SExpr -> IO String
ensureAtom (Atom a) = pure a
ensureAtom expr = error ("should be atom: " <> pretty expr)

apply :: Context => Value -> [Value] -> IO Value
apply "cons" [a, b] = pure (Cons a b)
apply "car" [Cons a _] = pure a
apply "cdr" [Cons _ a] = pure a
apply "car" [a] = error ("car is for cons-pair, not for " <> pretty a)
apply "cdr" [a] = error ("cdr is for cons-pair, not for " <> pretty a)
apply "print" [x] = putStrLn ("print> " <> pretty x) $> "nil"
apply "atom?" [Symbol _] = pure "true"
apply "atom?" [_] = pure "false"
apply "same-atom?" [Symbol a, Symbol b] = pure (if a == b then "true" else "false")
apply "same-atom?" [a, b] = error ("same-atom? is for atoms, not for " <> pretty a <> " and " <> pretty b)
apply (Closure scope binders body) args | length binders /= length args = error "invalid arity"
apply (Closure scope binders body) args =
  let ?scope = Map.union (Map.fromList [(k, v) | k <- binders | v <- args]) scope
   in eval body pure
apply f args = error ("unappliable value: " <> pretty f)

evalMany :: forall r. [SExpr] -> (Context => [Value] -> IO r) -> (Context => IO r)
evalMany xs next = go xs []
  where
    go :: Context => [SExpr] -> [Value] -> IO r
    go [] results = next (reverse results)
    go (x : xs) results = eval x \result -> go xs (result : results)

eval :: Context => SExpr -> (Context => Value -> IO r) -> IO r
eval (Atom x) next =
  case Map.lookup x ?scope of
    Just value -> next value
    Nothing -> next (Symbol x)
eval (List []) _ = error "impossible to eval: ()"
eval (List ["label", Atom name, e]) next = do
  eval e \e@(Closure scope names val) ->
    let clos = Closure (Map.insert name clos scope) names val
     in next clos
eval (List ["define*", Atom name, value]) next =
  eval value \value ->
    let ?scope = Map.insert name value ?scope
     in next "nil"
eval (List ["define-macro*", List (Atom name : args), body]) next = do
  args <- traverse ensureAtom args
  let macro =
        case break (== "&") args of
          (args, []) -> Macro args Nothing body
          (args, ["&", rest]) -> Macro args (Just rest) body
          args -> error ("invalid rest: " <> show args)
  let ?macros = Map.insert name macro ?macros in next "nil"
eval (List ("cond" : conds)) next = go conds
  where
    go (List [cond, body] : conds) =
      eval cond \case
        "true" -> eval body next
        _ -> go conds
    go [] = next "nil"
    go _ = error "invalid cond argument"
eval (List ["quote", x]) next = next =<< quote x
eval (List ("lambda" : List args : body)) next = do
  args <- traverse ensureAtom args
  next (Closure ?scope args (List ("do" : body)))
eval e@(List ("lambda" : _)) next = error ("invalid form of lambda: " <> pretty e)
eval (List ("do" : xs@(_ : _))) next = evalMany xs (next . last)
eval (List (Atom name : args)) next
  | Just (Macro binders Nothing body) <- name `Map.lookup` ?macros,
    length binders == length args = do
    args <- traverse quote args
    let ?scope = Map.union (Map.fromList [(k, v) | k <- binders | v <- args]) ?scope
     in eval body \result -> eval (unquote result) next
eval (List (Atom name : args)) next
  | Just (Macro binders (Just rest) body) <- name `Map.lookup` ?macros,
    length binders <= length args = do
    args <- traverse quote args
    let headArgs = take (length binders) args
    let restArgs = foldr Cons "nil" (drop (length binders) args)
    let ?scope = Map.union (Map.fromList [(k, v) | k <- (rest : binders) | v <- (restArgs : headArgs)]) ?scope
     in eval body \result -> eval (unquote result) next
eval (List (f : args)) next = do
  eval f \f ->
    evalMany args \args ->
      next =<< apply f args
eval (Quote x) next = next =<< quote x
eval (Unquote x) next = error "invalid unquote outside quote"

test :: String -> IO Value
test source = do
  let expr = parse source
  putStrLn ("Expr: " <> pretty expr)
  putStrLn "Output:"
  putStrLn "============="
  result <-
    let ?scope = Map.empty
        ?macros = Map.empty
     in eval expr pure
  putStrLn "============="
  pure result

main = do
  source <- readFile "example.lisp"
  let ?scope = Map.empty
      ?macros = Map.empty
   in eval (List ("do" : parses source)) pure
