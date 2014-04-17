{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Test.Themis.Keyword (
    Keyword
  , runKeyword
  , Context(..)
  , Action
  , safeAction
  , safeActionRollback
  , action
  , actionRollback
  , Interpretation
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
  KRet    :: a   -> Keyword k i a
  KWord   :: k   -> Keyword k i ()
  KInfo   :: i a -> Keyword k i a
  KAssert :: Bool -> String -> Keyword k i ()
  KBind   :: Keyword k i a -> (a -> Keyword k i b) -> Keyword k i b

instance Monad (Keyword k i) where
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

-- | The action consits of a computation that can fail
-- and possibly a revert action.
type Action m e = (m (Either e ()), Maybe (m ()))

-- | The interpretation of a 'k' basic keyword consists of a pair
-- the first is a computation which computes an error result or a
-- unit, and a revert action of the computation.
type Interpretation k m e = k -> Action m e

safeAction :: (Monad m, Error e) => m a -> Action m e
safeAction action =
  ( do action
       return (Right ())
  , Nothing
  )

safeActionRollback :: (Monad m, Error e) => m a -> m b -> Action m e
safeActionRollback action rollback =
  ( do action
       return (Right ())
  , Just (rollback >> return ())
  )

-- The action has no rollback but the action can fail.
action :: (Monad m, Error e) => m (Either e a) -> Action m e
action act =
  ( do x <- act
       case x of
         Left e  -> return $ Left e
         Right _ -> return $ Right ()
  , Nothing
  )

-- The action has a given rollback and the action can fail
actionRollback :: (Monad m, Error e) => m (Either e a) -> m b -> Action m e
actionRollback action rollback =
  ( do x <- action
       case x of
         Left e  -> return $ Left e
         Right y -> return $ Right ()
  , Just (rollback >> return ())
  )

-- | The keyword evaluation context consists of an interpretation
-- function, and an information command interpretation function, which
-- turns every 'i' command into a monadic computation
data Context k i m e = Context {
    keywordInterpretation :: Interpretation k m e
  , infoInterpretation    :: forall a . i a -> m a
  }

newtype Interpreter m e a = KI { unKI :: CME.ErrorT e (CMS.StateT [m ()] m) a }
  deriving (Monad, MonadState [m ()], MonadError e)

evalStage0 :: (Monad m, Error e) => Context k i m e -> Keyword k i a -> Interpreter m e a
evalStage0 ctx (KRet a)  = return a

evalStage0 ctx (KInfo c) = KI . lift . lift $ infoInterpretation ctx c

evalStage0 ctx (KWord k) = do
  let (step,revert) = keywordInterpretation ctx k
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

