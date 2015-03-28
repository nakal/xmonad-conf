
module Command.CommandPipe where

import System.Posix.Files
import System.IO
import System.IO.Error

pipeFileName :: FilePath -> FilePath
pipeFileName homedir =  homedir ++ "/.xmonad/cmdpipe"

makeNamedPipe :: FilePath -> IO ()
makeNamedPipe path = do
        exists <- fileExist path
        if exists then do
                status <- getFileStatus path
                if isNamedPipe status then
                        deleteNamedPipe path
                        else return ()
                else return ()
        createNamedPipe path $ ownerReadMode + ownerWriteMode

deleteNamedPipe :: FilePath -> IO ()
deleteNamedPipe = removeLink

bindCommandPipe :: FilePath -> IO Handle
bindCommandPipe path = do
        h <- openFile path ReadMode
        hSetBuffering h LineBuffering
        flushPipeBuffer h
        return h

flushPipeBuffer :: Handle -> IO ()
flushPipeBuffer h = do
        b <- hReady h
        if b then
                do
                        hGetLine h
                        flushPipeBuffer h
                else return ()

getPipeCommandLine :: Handle -> IO (Maybe String, Bool)
getPipeCommandLine h = do
        b <- tryIOError $ hReady h
        case b of
                Right False     ->      return (Nothing,False)
                Right True      ->
                                do
                                        line <- hGetLine h
                                        return (Just line, False)
                _  ->           return (Nothing, True)

connectCommandPipe :: FilePath -> IO Handle
connectCommandPipe path = do
        h <- openFile path WriteMode
        hSetBuffering h LineBuffering
        return h

sendPipeCommandLine :: Handle -> String -> IO ()
sendPipeCommandLine h cmd = do
        hPutStrLn h cmd
        hFlush h
