module Packet where

import Command
import Procedure

data AsyncPacket = AsyncPacket [Command]
                   deriving Show

data SyncPacket a = SyncPacket [Command] (Procedure a)
                    deriving Show
