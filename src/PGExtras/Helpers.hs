module PGExtras.Helpers (
  displayColumns1,
  displayColumns2,
  displayColumns3,
  displayColumns4,
  displayColumns5,
  displayColumns6,
  displayColumns7,
  displayColumns8,
  ) where

import qualified Data.Text as Text

displayColumns1 :: (Maybe Text.Text) -> IO ()
displayColumns1 (arg1) = do
  putStrLn $ maybeText(arg1)

displayColumns2 :: (Maybe Text.Text, Maybe Text.Text) -> IO ()
displayColumns2 (arg1, arg2) = do
  putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2)

displayColumns3 :: (Maybe Text.Text, Maybe Text.Text, Maybe Text.Text) -> IO ()
displayColumns3 (arg1, arg2, arg3) = do
  putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2) ++ " | " ++ maybeText(arg3)

displayColumns4 :: (Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text) -> IO ()
displayColumns4 (arg1, arg2, arg3, arg4) = do
  putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2) ++ " | " ++ maybeText(arg3) ++ " | " ++ maybeText(arg4)

displayColumns5 :: (Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text) -> IO ()
displayColumns5 (arg1, arg2, arg3, arg4, arg5) = do
  putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2) ++ " | " ++ maybeText(arg3) ++ " | " ++ maybeText(arg4) ++ " | " ++ maybeText(arg5)

displayColumns6 :: (Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text) -> IO ()
displayColumns6 (arg1, arg2, arg3, arg4, arg5, arg6) = do
  putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2) ++ " | " ++ maybeText(arg3) ++ " | " ++ maybeText(arg4) ++ " | " ++ maybeText(arg5) ++ " | " ++ maybeText(arg6)

displayColumns7 :: (Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text) -> IO ()
displayColumns7 (arg1, arg2, arg3, arg4, arg5, arg6, arg7) = do
  putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2) ++ " | " ++ maybeText(arg3) ++ " | " ++ maybeText(arg4) ++ " | " ++ maybeText(arg5) ++ " | " ++ maybeText(arg6) ++ " | " ++ maybeText(arg7)

displayColumns8 :: (Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text, Maybe Text.Text) -> IO ()
displayColumns8 (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) = do
  putStrLn $ maybeText(arg1) ++ " | " ++ maybeText(arg2) ++ " | " ++ maybeText(arg3) ++ " | " ++ maybeText(arg4) ++ " | " ++ maybeText(arg5) ++ " | " ++ maybeText(arg6) ++ " | " ++ maybeText(arg7) ++ " | " ++ maybeText(arg8)

maybeText :: Maybe Text.Text -> [Char]
maybeText Nothing = "NULL"
maybeText (Just x) = Text.unpack(x)
