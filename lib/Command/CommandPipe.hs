
module Command.CommandPipe where

import System.Posix.Files
import System.IO

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

flushPipe :: FilePath -> IO ()
flushPipe path = do
        h <- openFile path ReadMode
        hSetBuffering h LineBuffering
        flushPipeBuffer h

flushPipeBuffer :: Handle -> IO ()
flushPipeBuffer h = do
        b <- hReady h
        if b then
                do
                        hGetLine h
                        flushPipeBuffer h
                else return ()

getPipeCommandLine :: FilePath -> IO (Maybe String)
getPipeCommandLine path = do
        h <- openFile path ReadMode
        hSetBuffering h NoBuffering
        b <- hReady h
        if b then
                do
                        line <- hGetLine h
                        hClose h
                        return $ Just line
                else return Nothing

sendPipeCommandLine :: FilePath -> String -> IO ()
sendPipeCommandLine path cmd = do
        h <- openFile path WriteMode
        hSetBuffering h LineBuffering
        hPutStrLn h cmd
        hFlush h
        hClose h
