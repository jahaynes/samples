{-  Small Software Transactional Memory (STM) demo

    To find out more, read:
        http://adit.io/posts/2013-05-15-Locks,-Actors,-And-STM-In-Pictures.html
        https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/beautiful.pdf

    compile: ghc -O3 -o ./stm_samples stm_samples.hs -threaded
    run:     stm_samples +RTS -N
-}

import Control.Concurrent.STM           (STM, atomically)
import Control.Concurrent.STM.TVar      (TVar, newTVarIO, writeTVar, readTVar)
import Control.Concurrent.Async         (async, wait)
import Control.Monad                    (forM_, forM)

main :: IO ()
main = do

    -- Initialise some shared, mutable state
    counter <- newTVarIO 0

    -- Update it inside hundred threads
    jobs <- forM [1..100] (\_ -> async (doCount counter) )

    -- Wait for the threads to terminate
    mapM_ wait jobs

    -- Inspect the final value
    c <- atomically (readTVar counter)

    print c

doCount :: TVar Int -> IO ()
doCount counter =

    -- Increment a thousand times
    forM_ [1..1000] $ \_ ->

        {- 'atomically' takes a transaction and runs it,
            i.e.: it turns an STM() into an IO() -}
        atomically (atomicIncrement counter)

atomicIncrement :: TVar Int -> STM ()
atomicIncrement counter = do
    x <- readTVar counter
    {- Even though there's a gap here between the read and write,
       STM makes it "just work" (no race condition!) -}
    writeTVar counter $! x+1
