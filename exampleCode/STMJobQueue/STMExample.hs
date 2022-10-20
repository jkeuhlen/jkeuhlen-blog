module STMExample where

import Prelude
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue
import Control.Monad
import System.Random

-- Single threaded processing.
-- Orders come in one at a time, and if I'm not at my desk to take the order it isn't recorded. After making a delivery, I have to restart my process to keep working.
bullmooseGardenCenter1 :: IO ()
bullmooseGardenCenter1 = go []
  where
    go :: [String] -> IO ()
    go otherOrders = do
      order <- getLine
      if order == "deliver"
        then
          putStrLn $ "Deliver: " <> show otherOrders
        else
          go (order:otherOrders)

-- ghci> bullmooseGardenCenter1
-- dirt
-- flowers
-- mums
-- seeds
-- deliver
-- Deliver: ["seeds","mums","flowers","dirt"]

newOrderList :: IO (TMVar [String])
newOrderList = newEmptyTMVarIO

-- Now, let's add some complexity. One person takes orders, and on goes and delivers them.
bullmooseGardenCenter2 :: IO ()
bullmooseGardenCenter2 = do
  -- we need some communication channel we can share between people working on the problem.
  orderList <- newOrderList
  atomically $ putTMVar orderList []
  void $ forkIO $ forever $ deliver orderList
  forever $
    takeOrders orderList

  where
    deliver :: TMVar [String] -> IO ()
    deliver orderList = do
      threadDelay 10_000_000 -- take a nap before every delivery
      orders <- atomically $ do
        orders <- takeTMVar orderList
        putTMVar orderList []
        pure orders
      putStrLn $ "Deliver: " <> show orders

    takeOrders :: TMVar [String] -> IO ()
    takeOrders orderList = do
      order <- getLine
      atomically $ do
        list <- takeTMVar orderList
        putTMVar orderList (order:list)


-- ghci> bullmooseGardenCenter2
-- Deliver: []
-- flowers
-- flowers
-- mums
-- mums
-- seeds
-- Deliver: ["seeds","mums","mums","flowers","flowers"]
-- dirt
-- compost
-- mums
-- Deliver: ["mums","compost","dirt"]

bullmooseGardenCenter2ButWithABug :: IO ()
bullmooseGardenCenter2ButWithABug = do
  -- we need some communication channel we can share between people working on the problem.
  orderList <- newOrderList
  void $ forkIO $ forever $ deliver orderList
  forever $
    takeOrders orderList

  where
    deliver :: TMVar [String] -> IO ()
    deliver orderList = do
      threadDelay 10_000_000 -- take a nap after every delivery
      orders <- atomically $ do
        orders <- takeTMVar orderList
        putTMVar orderList []
        pure orders
      putStrLn $ "Deliver: " <> show orders

    takeOrders :: TMVar [String] -> IO ()
    takeOrders orderList = do
      order <- getLine
      atomically $ do
        list <- takeTMVar orderList
        putTMVar orderList (order:list)


-- ghci> bullmooseGardenCenter2
-- abc
-- def
-- *** Exception: thread blocked indefinitely in an STM transaction


newOrderQueue :: IO (TQueue String)
newOrderQueue = newTQueueIO

newDeliveryLogAccess :: IO (TMVar ())
newDeliveryLogAccess = newEmptyTMVarIO

-- In our penultimate version, we'll have multiple people taking orders as well as multiple delivery drivers. We're going to upgrade from a single list to a queue of orders that each need to be delivered in isolation.
bullmooseGardenEmpire :: IO ()
bullmooseGardenEmpire = do
  orderQueue <- newOrderQueue
  deliveryLog <- newDeliveryLogAccess
  atomically $ putTMVar deliveryLog ()
  forM_ [1 ..10] $ \n -> forkIO $ forever $ deliver n orderQueue deliveryLog
  forM_ [1 :: Int ..5] $ \_ -> forkIO $ forever $ takeOrders orderQueue

  where
    deliver :: Int -> TQueue String -> TMVar () -> IO ()
    deliver n orderQueue deliveryLog = do
      napLength <- randomRIO (8_000_000,10_000_000)
      threadDelay napLength -- take a nap before every delivery
      order <- atomically $ readTQueue orderQueue
      -- Take turns logging about this so that you don't ruin my beautiful console output
      void $ atomically $ takeTMVar deliveryLog
      putStrLn $ "Driver " <> show n <> " delivering: " <> show order
      atomically $ putTMVar deliveryLog ()

    takeOrders :: TQueue String -> IO ()
    takeOrders orderQueue = do
      x :: Int <- randomRIO (0, 7)
      let order = ["seeds","flowers","mums","dirt","compost","tools","fountains","plant starts"] !! x
      atomically $ writeTQueue orderQueue order


-- ghci> bullmooseGardenEmpire
-- ghci> Driver 1 delivering: "flowers"
-- Driver 2 delivering: "dirt"
-- Driver 10 delivering: "seeds"
-- Driver 3 delivering: "seeds"
-- Driver 6 delivering: "compost"
-- Driver 8 delivering: "seeds"
-- Driver 7 delivering: "dirt"
-- Driver 5 delivering: "tools"
-- Driver 4 delivering: "seeds"
-- Driver 9 delivering: "tools"
-- Driver 5 delivering: "mums"
-- Driver 2 delivering: "dirt"
-- Driver 6 delivering: "compost"
-- Driver 8 delivering: "mums"
-- Driver 10 delivering: "mums"
-- Driver 1 delivering: "flowers"
-- Driver 3 delivering: "fountains"
-- Driver 7 delivering: "seeds"
-- Driver 9 delivering: "fountains"
-- Driver 4 delivering: "seeds"

-- As one last example, let's showcase the composability of STM. Imagine we're in the same scenario as the last, but this time we have two extra employees who works both the phones and the deliveries, depending on where they are needed most.

bullmooseGardenEmpire2 :: IO ()
bullmooseGardenEmpire2 = do
  orderQueue <- newOrderQueue
  deliveryLog <- newDeliveryLogAccess
  atomically $ putTMVar deliveryLog ()
  forM_ [1..10] $ \n -> forkIO $ forever $ deliver n orderQueue deliveryLog
  forM_ [1 :: Int ..5] $ \_ -> forkIO $ forever $ takeOrders orderQueue
  forM_ [1 :: Int ..2] $ \_ -> forkIO $ forever $ deliverOrTakeOrders orderQueue deliveryLog

  where

    deliverSTM :: TQueue String -> STM String
    deliverSTM orderQueue = readTQueue orderQueue


    deliver :: Int -> TQueue String -> TMVar () -> IO ()
    deliver n orderQueue deliveryLog = do
      napLength <- randomRIO (8_000_000,10_000_000)
      threadDelay napLength -- take a nap before every delivery
      order <- atomically $ deliverSTM orderQueue
      -- Take turns logging about this so that you don't ruin my beautiful console output
      void $ atomically $ takeTMVar deliveryLog
      putStrLn $ "Driver " <> show n <> " delivering: " <> show order
      atomically $ putTMVar deliveryLog ()

    takeOrderSTM :: String -> TQueue String -> STM ()
    takeOrderSTM order orderQueue = writeTQueue orderQueue order

    takeOrders :: TQueue String -> IO ()
    takeOrders orderQueue = do
      x :: Int <- randomRIO (0, 7)
      napLength <- randomRIO (8_000_000,10_000_000)
      threadDelay napLength -- take a nap before every order
      let order = ["seeds","flowers","mums","dirt","compost","tools","fountains","plant starts"] !! x
      atomically $ takeOrderSTM order orderQueue

    deliverOrTakeOrdersSTM :: TQueue String -> STM String
    deliverOrTakeOrdersSTM orderQueue = do
      -- If there is something on the queue, go ahead and deliver it.
      -- If not, go answer the phones. For some reason, it's always dahlias?
      deliverSTM orderQueue `orElse` (takeOrderSTM "dahlias" orderQueue *> pure "Took an order for dahlias")

    deliverOrTakeOrders :: TQueue String -> TMVar () -> IO ()
    deliverOrTakeOrders orderQueue deliveryLog = do
      napLength <- randomRIO (8_000_000,10_000_000)
      threadDelay napLength -- take a nap before every order
      workerOutput <- atomically $ deliverOrTakeOrdersSTM orderQueue
      void $ atomically $ takeTMVar deliveryLog
      putStrLn workerOutput
      atomically $ putTMVar deliveryLog ()

-- ghci> bullmooseGardenEmpire2
-- ghci> Driver 10 delivering: "dirt"
-- Took an order for dahlias
-- Driver 4 delivering: "dahlias"
-- Took an order for dahlias
-- Driver 8 delivering: "dahlias"
-- Driver 2 delivering: "mums"
-- Driver 1 delivering: "seeds"
-- Driver 9 delivering: "fountains"
-- Driver 3 delivering: "tools"
-- Driver 5 delivering: "flowers"
-- Took an order for dahlias
-- Driver 7 delivering: "dahlias"
-- Driver 6 delivering: "mums"
-- Driver 10 delivering: "dirt"
-- Driver 8 delivering: "fountains"
-- Took an order for dahlias
-- Driver 4 delivering: "dahlias"
-- Driver 3 delivering: "flowers"
