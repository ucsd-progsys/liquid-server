
module Language.Liquid.Server.Ticket (Ticket, newTicket, nextTicket) where

import Control.Concurrent.MVar

data Val   =  Val !Integer
data Ticket = Ticket (MVar Val)

newTicket :: IO Ticket
newTicket = Ticket `fmap` newMVar (Val 0)

nextTicket :: Ticket -> IO Integer
nextTicket (Ticket t) = modifyMVar t f
  where
    f (Val n) = return (Val $ n+1, n)

