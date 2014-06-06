-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.Channel
-- Copyright   :  None
-- License     :  BSD3
--
-- Maintainer  :  Vitor Rodrigues
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Analyzer.Channel ( newChannel, sendCert, getCert

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Control.Concurrent ( ThreadId, forkIO )
import Control.Concurrent.Chan ( Chan, newChan, writeChan, readChan )

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.Certificate



data ACCRequest a
   = Get
   | Set { initial_value :: (String, a) }
   | Die

data Generator a
   =  Generator { thread_id :: ThreadId,
                  request_channel :: Chan (ACCRequest a),
                  response_channel :: Chan (Maybe (String, a)) }

newChannel
  :: IO (Generator a)

newChannel
  = do
    in_chan <- newChan
    out_chan <- newChan
    tid <- forkIO $ listen in_chan out_chan Nothing
    return $ Generator tid in_chan out_chan

listen
  :: Chan (ACCRequest a)
   -> Chan (Maybe (String, a))
   -> Maybe (String, a)
   -> IO ()

listen ic oc i = do
               req <- readChan ic
               case req of
                    Die -> return ()
                    Set n -> listen ic oc (Just n)
                    Get -> do
                           writeChan oc i
                           listen ic oc i


sendCert
  :: Generator a
   -> (String, a)
   -> IO ()

sendCert g i
  = writeChan (request_channel g) (Set i)

getCert
  :: Generator a
   -> IO (Maybe (String, a))

getCert g
  = do
    writeChan (request_channel g) (Get)
    readChan (response_channel g)
