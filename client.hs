{-# LANGUAGE NamedFieldPuns, DeriveDataTypeable, ExtendedDefaultRules, EmptyDataDecls, MultiParamTypeClasses #-}
module Main where
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Thread.Delay (delay)
import Control.Concurrent.MVar
import Control.Monad (forever, unless, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (modify)
import Control.Exception.Base (try, SomeException)
import Network.Socket (withSocketsDo)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
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


data Chat = Chat { chatWindow :: Window, chatExitCode :: Int }


newtype MyCurses a = MyCurses { myCursesIO :: IO a }
unCurse :: N.Curses a -> IO a
unCurse x = myCursesUnpack (unsafeCoerce x :: MyCurses a)
    where
    myCursesUnpack (MyCurses a) = a


waitFor :: Window -> (N.Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop


app :: MVar Chat -> WS.ClientApp()
app state conn = do
    c <- takeMVar state :: IO Chat
    unCurse $ do
        N.updateWindow (chatWindow c) $ do
            moveCursor 1 10
            drawString "Connected!"
            moveCursor 0 0
            drawBox Nothing Nothing
        N.render

    let joinCmd = cmdJoin cfg_channel cfg_nick
    WS.sendTextData conn $ joinCmd

    recvThreadId <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ unCurse $ do
            N.updateWindow (chatWindow c) $ do
                moveCursor 1 10
                drawString $ T.unpack msg
                moveCursor 0 0
                drawBox Nothing Nothing
            N.render

    pingThreadId <- forkIO $ forever $ do
        delay (60*1000*1000) >> WS.sendTextData conn (cmdPing)

    let loop = do
        ev <- unCurse (getEvent (chatWindow c) (Just 300))
        let quit = case ev of
                        Just e -> e == EventCharacter 'q' || e == EventCharacter 'Q'
                        Nothing -> False

        if quit then do
            let c2 = c {chatExitCode = 0}
            putMVar state c2
            return False -- No auto restart
        else loop
    loop

    killThread recvThreadId
    killThread pingThreadId

    WS.sendClose conn (T.pack "")



runClient chat = do
    result <- try (withSocketsDo $ WS.runClient cfg_server cfg_port "/chat-ws" (app chat)) :: IO (Either SomeException ())
    -- ignore result for now
    c <- readMVar chat
    if (chatExitCode c) /= -1 then do
        return True
    else runClient chat



main :: IO()
main = runCurses $ do
    window <- defaultWindow
    oldCursorMode <- setCursorMode CursorInvisible

    chat <- liftIO $ newMVar $ Chat { chatWindow = window, chatExitCode = -1 }
    c <- liftIO $ readMVar chat

    N.updateWindow (chatWindow c) $ do
        moveCursor 1 10
        drawString "Hello World!"
        moveCursor 3 10
        drawString "(press q to quit)"
        moveCursor 0 0
        drawBox Nothing Nothing
    N.render

    liftIO $ runClient chat

    _ <- setCursorMode oldCursorMode

    liftIO $ putStrLn "End"

