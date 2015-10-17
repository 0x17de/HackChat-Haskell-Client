{-# LANGUAGE NamedFieldPuns, DeriveDataTypeable, ExtendedDefaultRules, EmptyDataDecls, MultiParamTypeClasses #-}
module Main where
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals as SIG
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, forkOS, killThread)
import qualified Control.Concurrent.Thread as Thread (forkIO, forkOS, result)
import Control.Concurrent.Thread.Delay (delay)
import Control.Concurrent.MVar
import Control.Monad (forever, unless, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (modify)
import Control.Exception.Base (mask, onException, catch, SomeException)
import Network.Socket (withSocketsDo)
import Data.Char (chr)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Foreign.C.Types as C

import Data.Aeson as J
import Data.Text.Lazy.Encoding as E

import qualified UI.HSCurses.Curses as N
import qualified UI.HSCurses.Widgets as NW



cfg_server = "toastystoemp.com"
cfg_port = 6060
cfg_channel = "testing"
cfg_nick = "HaskellNick"



data Event = EventButton C.CInt | EventNop | EventSend String | EventTimeout
data Queue = Queue (MVar Bool) (MVar [Event])


newEventQueue :: IO Queue
newEventQueue = do
    lock <- newEmptyMVar
    queue <- newMVar []
    return $ Queue lock queue
addEvent :: Queue -> Event -> IO()
addEvent (Queue lock queue) e = do
    a <- takeMVar queue
    success <- tryPutMVar lock True
    putMVar queue (a++[e])
    return $! ()
    
takeEvent :: Queue -> IO Event
takeEvent q = do
    let (Queue lock queue) = q
    a <- takeMVar queue
    case a of
        [] -> do
            putMVar queue []
            tryTakeMVar lock
            tryReadMVar lock
            takeEvent q
        otherwise -> putMVar queue (tail a) >> return (head a)



cmdPing :: Text
cmdPing = T.pack "{\"cmd\": \"ping\"}"

cmdChat :: Text -> Text
cmdChat msg = do
    toStrict $ E.decodeUtf8 $ J.encode $ J.object [ T.pack "cmd" .= "chat", T.pack "text" .= msg ]

cmdJoin :: String -> String -> Text
cmdJoin channel nick = do
    toStrict $ E.decodeUtf8 $ J.encode $ J.object [ T.pack "cmd" .= "join", T.pack "channel" .= channel, T.pack "nick" .= nick ]



app :: MVar Bool -> Queue -> Queue -> WS.ClientApp()
app clientStopping guiEvents netEvents conn = do
    WS.sendTextData conn $ cmdJoin cfg_channel cfg_nick

    recvThreadId <- forkIO $ forever $ do
        putStrLn.T.unpack.toStrict =<< WS.receiveData conn
        return $! ()

    pingThreadId <- forkIO $ forever $ do
        delay (60*1000*1000) >> WS.sendTextData conn (cmdPing)

    timeoutThreadId <- forkIO $ forever $ do
        delay (300*1000) >> addEvent netEvents EventTimeout

    let loop :: IO()
        loop = do
        stopping <- readMVar clientStopping
        case stopping of
            True  -> return $! ()
            False -> do
                hPutStrLn stderr "EVENT"
                e <- takeEvent netEvents
                hPutStrLn stderr "AfterTake"
                case e of
                    EventSend msg -> do
                        hPutStrLn stderr $ "Sending Message: "++msg
                        WS.sendTextData conn $ cmdChat $ T.pack msg
                    EventTimeout -> return $! ()
                    otherwise -> return $! ()
                hPutStrLn stderr "AfterCase"
                loop
    loop

    killThread timeoutThreadId
    killThread recvThreadId
    killThread pingThreadId

    WS.sendClose conn (T.pack "")



runGui :: MVar Bool -> Queue -> Queue -> IO()
runGui clientStopping guiEvents netEvents = do
    window <- N.initScr
    oldCursorMode <- N.cursSet N.CursorInvisible
    N.echo False
    
    -- HANDLER
    let sigWinchHandler signalInfo = do
        case (siginfoSignal signalInfo) of
            siginfoSignal -> do
                hPutStrLn stderr "Resize"
    
    case N.cursesSigWinch of
        Just a -> installHandler (a) (SIG.CatchInfo $ sigWinchHandler) Nothing
    
    -- INITIALIZE VIEWS
    --let tableWidet = NW.newTableWidget (NW.TBWOptions {}) []
    
    -- REFRESH
    --N.wRefresh window

    chatMessage <- newMVar ""

    -- LOOP
    -- keyboard events
    keyboardThread <- forkIO $ forever $ do
        keyCode <- N.getch
        addEvent guiEvents (EventButton keyCode)
        return $! ()

    -- process keyboard events
    let processButtonAction :: C.CInt -> IO()
        processButtonAction ckeyCode = do
            let keyCode = fromIntegral ckeyCode :: Int
            liftIO $ hPutStrLn stderr $ "Keycode " ++ (show keyCode)
            case keyCode of
                27 -> liftIO $ takeMVar clientStopping >> putMVar clientStopping True
                10 -> do
                    msg <- takeMVar chatMessage
                    putMVar chatMessage ""
                    addEvent netEvents $ EventSend msg
                    hPutStrLn stderr $ "Sending Message: "++msg
                otherwise -> do
                    msg <- takeMVar chatMessage
                    let msg' = msg ++ [chr $ fromIntegral keyCode]
                    putMVar chatMessage msg'
                    hPutStrLn stderr $ "New Message: "++msg'
            return $! ()

    -- main event loop
    let eventLoop :: IO()
        eventLoop = do
            stopping <- readMVar clientStopping
            unless(stopping) $ do
                e <- takeEvent guiEvents
                case e of
                    EventButton button -> processButtonAction button
                    EventNop -> return $! ()
                eventLoop

    -- SPAWN
    (_, waitEvents) <- Thread.forkIO $ eventLoop

    -- WAIT & EXIT
    waitEvents
    killThread keyboardThread
    N.cursSet oldCursorMode
    N.echo True
    N.endWin



runNet :: MVar Bool -> Queue -> Queue -> IO()
runNet stopping guiEvents netEvents = do
    catch (withSocketsDo $ WS.runClient cfg_server cfg_port "/chat-ws" (app stopping guiEvents netEvents))
        (\e -> do
                    let err = show (e :: SomeException)
                    hPutStrLn stderr ("Error: " ++ err)
                    return $! ())



main :: IO()
main = do
    clientStopping <- newMVar False

    guiEvents <- newEventQueue
    netEvents <- newEventQueue

    (_, netWait) <- Thread.forkIO $ runNet clientStopping guiEvents netEvents
    (_, guiWait) <- Thread.forkIO $ runGui clientStopping guiEvents netEvents

    netWait
    guiWait
    return $! ()

