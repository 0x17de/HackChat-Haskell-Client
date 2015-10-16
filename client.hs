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

import UI.NCurses as N
import Unsafe.Coerce



cfg_server = "toastystoemp.com"
cfg_port = 6060
cfg_channel = "programming"
cfg_nick = "HaskellNick"



cmdPing :: Text
cmdPing = T.pack "{\"cmd\": \"ping\"}"

cmdChat :: Text -> Text
cmdChat msg = do
    toStrict $ E.decodeUtf8 $ J.encode $ J.object [ T.pack "cmd" .= "chat", T.pack "text" .= msg ]

cmdJoin :: String -> String -> Text
cmdJoin channel nick = do
    toStrict $ E.decodeUtf8 $ J.encode $ J.object [ T.pack "cmd" .= "join", T.pack "channel" .= channel, T.pack "nick" .= nick ]



waitFor :: Window -> (N.Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop



app :: MVar Bool -> WS.ClientApp()
app stopping conn = do
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




runGui :: MVar Bool -> IO()
runGui clientStopping = do
    runCurses $ do
        window <- defaultWindow
        oldCursorMode <- setCursorMode CursorInvisible
        
        N.updateWindow window $ do
            moveCursor 1 10
            drawString "Hello World!"
            moveCursor 3 10
            drawString "(press q to quit)"
            moveCursor 0 0
            drawBox Nothing Nothing
        N.render
        
        -- TODO: Render chat and updates here
{-

        liftIO $ unCurse $ do
            N.updateWindow (chatWindow c) $ do
                moveCursor 1 10
                drawString $ T.unpack msg
                moveCursor 0 0
                drawBox Nothing Nothing
            N.render

    unCurse $ do
        N.updateWindow (chatWindow c) $ do
            moveCursor 1 10
            drawString "Connected!"
            moveCursor 0 0
            drawBox Nothing Nothing
        N.render
-}
        
        let loop = do
            ev <- getEvent window (Just 300)
            let quit = case ev of
                            Just e -> e == EventCharacter 'q' || e == EventCharacter 'Q'
                            Nothing -> False

            if quit then do
                liftIO $ takeMVar clientStopping >> putMVar clientStopping True >> return ()
            else loop
        loop
        
        _ <- setCursorMode oldCursorMode
        return ()



runNet :: MVar Bool -> IO()
runNet stopping = do
    withSocketsDo $ WS.runClient cfg_server cfg_port "/chat-ws" (app stopping)



main :: IO()
main = do
    clientStopping <- newMVar False

    (_, guiWait) <- Thread.forkOS $ runGui clientStopping
    (_, netWait) <- Thread.forkOS $ runNet clientStopping

    netWait >> guiWait >> (return $! ())

