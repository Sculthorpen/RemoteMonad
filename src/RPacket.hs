module RPacket
  (RAsyncPacket,RSyncPacket,execRAsyncPacket,execRSyncPacket)
where

import RCommand
import RProcedure

data RAsyncPacket = AsyncPacket [RCommand]
                    deriving Read

data RSyncPacket = SyncPacket [RCommand] RProcedure
                   deriving Read

execRAsyncPacket :: RAsyncPacket -> IO ()
execRAsyncPacket (AsyncPacket cs) =
     mapM_ execRCommand cs

execRSyncPacket :: RSyncPacket -> IO String
execRSyncPacket (SyncPacket cs p) =
  do mapM_ execRCommand cs
     execRProcedure p
