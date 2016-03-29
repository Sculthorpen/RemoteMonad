{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, FlexibleInstances, TupleSections #-}

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Command
import Procedure
import Packet
import RPacket

------------------------------------------------------------------

data Device = Device
  { sync  :: String -> IO String
  , async :: String -> IO ()
  }

device :: Device
device = Device (execRSyncPacket  . read)
                (execRAsyncPacket . read)

------------------------------------------------------------------

newtype Remote a = Remote {runRemote :: Device -> Queue -> IO (a,Queue)}

type Queue = [Command]

------------------------------------------------------------------

instance Functor Remote where
  fmap = liftM

instance Applicative Remote where
  pure = return
  (<*>) = ap

instance Monad Remote where
  return :: a -> Remote a
  return a = Remote (\ _ q -> return (a,q))

  (>>=) :: Remote a -> (a -> Remote b) -> Remote b
  ra >>= k = Remote (\ d q -> do (a,q') <- runRemote ra d q
                                 runRemote (k a) d q'
                    )

instance MonadState [Command] Remote where
  state :: (Queue -> (a,Queue)) -> Remote a
  state f = Remote (\ _ q -> return (f q))

instance MonadReader Device Remote where
  ask :: Remote Device
  ask = Remote (\ d q -> return (d,q))

  local :: (Device -> Device) -> Remote a -> Remote a
  local f ra = Remote (\ d -> runRemote ra (f d))

instance MonadIO Remote where
  liftIO :: IO a -> Remote a
  liftIO ma = Remote (\ _ q -> fmap (,q) ma)

------------------------------------------------------------------

send :: Device -> Remote a -> IO a
send d ra = do (a,q) <- runRemote ra d []
               when (not (null q)) (async d (show (AsyncPacket q)))
               return a

sendCommand :: Command -> Remote ()
sendCommand cmd = modify (++ [cmd])

sendProcedure :: Procedure a -> Remote a
sendProcedure p = do d <- ask
                     q <- get
                     r <- liftIO (sync d (show (SyncPacket q p)))
                     put []
                     return (readProcedureReply p r)

say :: String -> Remote ()
say txt = sendCommand (Say txt)

temperature :: Remote Int
temperature = sendProcedure Temperature

toast :: Int -> Remote ()
toast n = sendProcedure (Toast n)

------------------------------------------------------------------

main :: IO ()
main =
  do t <- send device $ do say "Do you want some toast?"
                           t <- temperature
                           say (show t ++ "F")
                           return t
     when (t < 70) (send device (toast 5))

------------------------------------------------------------------
