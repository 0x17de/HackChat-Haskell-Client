{-# LANGUAGE NamedFieldPuns, DeriveDataTypeable, ExtendedDefaultRules, EmptyDataDecls, MultiParamTypeClasses #-}
module Main where
import Control.Concurrent (forkIO, killThread)
import qualified Control.Concurrent.Thread as Thread (forkIO, forkOS, result)
import Control.Concurrent.Thread.Delay (delay)
import Control.Concurrent.MVar
import Control.Monad (forever, unless, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (modify)
import Control.Exception.Base (try, SomeException)
import Network.Socket (withSocketsDo)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import Data.Aeson as J
import Data.Text.Lazy.Encoding as E

import qualified UI.HSCurses.Curses as N
import Unsafe.Coerce



cfg_server = "toastystoemp.com"
cfg_port = 6060
cfg_channel = "programming"
cfg_nick = "HaskellNick"



newEventQueue :: IO (MVar [a])
newEventQueue = newMVar []
addEvent :: (MVar [a]) -> a -> IO()
addEvent q e = do
    takeMVar q >>= \x -> putMVar q (x++[e]) >> return ()
takeEvent :: MVar [a] -> IO a
takeEvent q = do
    (x:xs) <- takeMVar q
    putMVar q xs
    return x



cmdPing :: Text
cmdPing = T.pack "{\"cmd\": \"ping\"}"

cmdChat :: Text -> Text
cmdChat msg = do
    toStrict $ E.decodeUtf8 $ J.encode $ J.object [ T.pack "cmd" .= "chat", T.pack "text" .= msg ]

cmdJoin :: String -> String -> Text
cmdJoin channel nick = do
    toStrict $ E.decodeUtf8 $ J.encode $ J.object [ T.pack "cmd" .= "join", T.pack "channel" .= channel, T.pack "nick" .= nick ]



app :: MVar Bool -> MVar [a] -> WS.ClientApp()
app stopping guiEvents conn = do
    WS.sendTextData conn $ cmdJoin cfg_channel cfg_nick

    recvThreadId <- forkIO $ forever $ do
        _ <- return.toStrict =<< WS.receiveData conn
        return $! ()

    pingThreadId <- forkIO $ forever $ do
        delay (60*1000*1000) >> WS.sendTextData conn (cmdPing)

    let loop = do
        stopstatus <- readMVar stopping
        case stopstatus of
            True  -> return ()
            False -> delay (300*1000) >> loop
    loop

    killThread recvThreadId
    killThread pingThreadId

    WS.sendClose conn (T.pack "")




runGui :: MVar Bool -> MVar [a] -> IO()
runGui clientStopping netEvents = do
    window <- N.initScr
    oldCursorMode <- N.cursSet N.CursorInvisible
    
    -- TODO: Render chat and updates here

    let loop = do
        key <- N.getCh
        case key of
            N.KeyCancel -> liftIO $ takeMVar clientStopping >> putMVar clientStopping True
        
        stopping <- readMVar clientStopping
        case stopping of
            True  -> return ()
            False -> loop
    loop
    
    _ <- N.cursSet oldCursorMode
    N.endWin
    return ()



runNet :: MVar Bool -> MVar [a] -> IO()
runNet stopping guiEvents = do
    withSocketsDo $ WS.runClient cfg_server cfg_port "/chat-ws" (app stopping guiEvents)



main :: IO()
main = do
    clientStopping <- newMVar False

    guiEvents <- newEventQueue
    netEvents <- newEventQueue

    (_, guiWait) <- Thread.forkOS $ runGui clientStopping netEvents
    (_, netWait) <- Thread.forkOS $ runNet clientStopping guiEvents

    netWait >> guiWait >> (return $! ())

