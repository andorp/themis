{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Test.Themis.Keyword (
    Keyword
  , runKeyword
  , Commands(..)
  , step
  , info
  , assertion
  ) where

import           Control.Monad.Error
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Control.Monad.Trans.Error as CME
import qualified Control.Monad.Trans.State as CMS

{-
Every testeable system with test steps can be represented by keywords.
Keywords are composable actions. These actions has optional backup steps.

Keywords represents a script which can be evaluated with the given
context of the interpretation of each keyword. This way multiple
reusable scripts can be written in the haskell do notation.
-}

data Keyword k i a where
  KRet    :: a -> Keyword k i a
  KWord   :: k -> Keyword k i ()
  KInfo   :: i a -> Keyword k i a
  KAssert :: Bool -> String -> Keyword k i ()
  KBind   :: Keyword k i a -> (a -> Keyword k i b) -> Keyword k i b

instance Monad (Keyword k c) where
  return  = KRet
  m >>= k = KBind m k

-- | The 'k' step that will be interpreted in some action.
step :: k -> Keyword k i ()
step = KWord

-- | The 'i' informational step which gather information about
-- the system.
info :: i a -> Keyword k i a
info = KInfo

-- | Assertion on the first value, the assertion fails if the
-- first value is False. After the failure the revering actions
-- will be computed.
assertion :: Bool -> String -> Keyword k i ()
assertion = KAssert

-- | The interpretation of a 'k' basic keyword consists of a pair
-- the first is a computation which computes an error result or a
-- unit, and a revert action of the computation.
type Interpretation k m e = k -> (m (Either e ()), Maybe (m ()))

-- | The keyword evaluation context consists of an interpretation
-- function, and an information command interpretation function, which
-- turns every 'i' command into a monadic computation
data Context k i m e = Context {
    keyword     :: Interpretation k m e
  , information :: forall a . i a -> m a
  }

newtype Interpreter m e a = KI { unKI :: CME.ErrorT e (CMS.StateT [m ()] m) a }
  deriving (Monad, MonadState [m ()], MonadError e)

evalStage0 :: (Monad m, Error e) => Context k i m e -> Keyword k i a -> Interpreter m e a
evalStage0 ctx (KRet a)  = return a
evalStage0 ctx (KInfo c) = KI . lift . lift $ information ctx c
evalStage0 ctx (KWord k) = do
  let (step,revert) = keyword ctx k
  x <- KI . lift $ lift step
  case x of
    Left e -> throwError e
    Right _ -> do
      case revert of
        Just r  -> modify (r:)
        Nothing -> return ()
evalStage0 ctx (KBind m k) = do
  x <- evalStage0 ctx m
  evalStage0 ctx (k x)
evalStage0 ctx (KAssert a msg) = unless a . throwError $ strMsg msg

evalStage1 :: (Monad m, Error e) => Interpreter m e a -> m (Either e a)
evalStage1 m = do
  (result, revert) <- flip CMS.runStateT [] . CME.runErrorT $ unKI m
  case result of
    Left err -> do sequence_ revert
                   return (Left err)
    Right a -> return (Right a)

-- | The 'runKeyword' interprets the given keyword in a computational context, and
-- reverts the steps if any error occurs.
runKeyword :: (Monad m, Error e) => Context k i m e -> Keyword k i a -> m (Either e a)
runKeyword ctx k = evalStage1 $ evalStage0 ctx k

