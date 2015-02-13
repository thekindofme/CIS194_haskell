{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1
parseMessage :: String -> MaybeLogMessage
parseMessage str = case words str of
    ("I":timestamp:msg)          -> parseInfo timestamp $ unwords msg
    ("W":timestamp:msg)          -> parseWarning timestamp $ unwords msg
    ("E":severity:timestamp:msg) -> parseError severity timestamp $ unwords msg
    _                            -> InvalidLM str


parseInfo :: String -> String -> MaybeLogMessage
parseInfo timestamp msg
   | isValidInt timestamp = ValidLM $ LogMessage Info (readTimestamp timestamp) msg
   | otherwise            = InvalidLM msg

parseWarning :: String -> String -> MaybeLogMessage
parseWarning timestamp msg
   | isValidInt timestamp = ValidLM $ LogMessage Warning (readTimestamp timestamp) msg
   | otherwise            = InvalidLM msg

parseError :: String -> String -> String -> MaybeLogMessage
parseError severity timestamp msg
   | (isValidInt severity && isValidInt timestamp) = ValidLM $ LogMessage (Error (readError severity)) (readTimestamp timestamp) msg
   | otherwise                                     = InvalidLM msg


isValidInt :: String -> Bool
isValidInt maybeInt = case (readInt maybeInt) of
    ValidInt integer -> True
    _ -> False


readTimestamp :: String -> TimeStamp
readTimestamp timestamp = read timestamp


readError :: String -> Int
readError severity = read severity :: Int

