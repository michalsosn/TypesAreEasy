module Control.Monad.Trans.Step.Demo where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer

import Control.Monad.Trans.Step
import Control.Monad.Trans.Step.Concurrency


addStep :: Int -> Int -> Step Int
addStep x y = step (x + y)

squareStep :: Int -> Step Int
squareStep x = step (x * x)

pythagorasStep :: Int -> Int -> Step Int
pythagorasStep x y = do
    xSquared <- squareStep x
    ySquared <- squareStep y
    idle
    addStep xSquared ySquared

printSteps :: IO ()
printSteps = do
    a <- current $ do
        liftIO $ putStrLn "beg"
        forM_ [1..15] $ \i -> liftIO (print i) >> idle
        liftIO $ putStrLn "end"
    print "Before start"
    b <- current a
    print "Start"
    c <- current $ run b
    print "Middle-1"
    d <- current $ runFor 5 c
    print "Middle-2"
    e <- current $ finish d
    print "End"
    current e
    print "After end"

data DemoState = DemoState
    { lock1   :: Lock
    , lock2   :: Lock
    , counter :: Int
    }

demoState :: DemoState
demoState = DemoState released released 0

type Worker a = StepT (WriterT [String] (State DemoState)) a

worker :: String -> Worker ()
worker name = do
    let write :: String -> Worker ()
        write msg = tell ["[" ++ name ++ "] " ++ msg]
    write "Started."

    write "Getting value"
    val <- idle >> gets counter
    write $ "Knows value: " ++ show val
    idle >> modify (\s -> s { counter = val + 1 })
    write $ "Increased counter to " ++ show (val + 1)

    idle >> write "Active"
    withLock lock1 (\l s -> s { lock1 = l }) (idle >> write "waited for lock") $ do
        write "obtained lock"
        forM_ [1..4] $ \i -> idle >> write ("worked " ++ show i)
        write "released lock"
    write "Finished."

workerReactor :: [Worker a] -> [Int] -> WriterT [String] (State DemoState) ()
workerReactor (w:ws) (m:ms) = do
    w'   <- current $ runFor m w
    done <- finished w'
    let ws' = if done then ws else ws ++ [w']
    workerReactor ws' ms
workerReactor _ _  = return ()

runWorkers :: [String]
runWorkers = do
    let workers = fmap worker ["Johny", "Maciej", "Andrzej", "Wacław"]
    runIdentity . flip evalStateT demoState . execWriterT $ workerReactor workers (take 100 $ cycle [2, 1])

demoWorkers :: IO ()
demoWorkers = forM_ runWorkers putStrLn

demo :: IO ()
demo = do
--    print $ runStep $ fmap (*2) $ pythagorasStep 3 4
--    print $ runStep $ (\a b c -> a - b + c) <$> pythagorasStep 3 4 <*> pythagorasStep 4 4 <*> pythagorasStep 4 5
--    print $ alternateFinish [pythagorasStep a b | a <- [1..3], b <- [2..4]] (repeat 2)
--    forM_ [0..10] $ \i -> do
--        let action = runFor i $ pythagorasStep 3 4
--        print $ runStep . finished $ action
--    printSteps
    demoWorkers
