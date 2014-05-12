{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module      :  Uzbl.WithSource
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPLv3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Deals with retrieving page source from running uzbl instance.
-- Possibly will deal more in the future, such as overwriting the
-- page.
module Uzbl.WithSource where

import GHC.IO.Handle (hPutStr, hGetContents, hClose)
import System.Environment (lookupEnv)
import System.Process ( createProcess, proc
                      , StdStream(CreatePipe), std_out, std_in)

-- | Retrieves the source code of a currently loaded page in the uzbl
-- browser from the @UZBL_SOCKET@ environmental variable. If the
-- variable is not set, we get back 'Nothing'. If reading from or
-- writing to the socket fails, we also get back 'Nothing'.
getSource ∷ IO (Maybe String)
getSource = lookupEnv "UZBL_SOCKET" >>= \case
  Nothing → return Nothing
  Just f →
    let sp = (proc "socat" ["-", "unix-connect:\"" ++ f ++ "\""])
                { std_out = CreatePipe, std_in = CreatePipe }
    in createProcess sp >>= \case
      (Just hin, Just hout, _, _) → do
        hPutStr hin "js document.documentElement.outerHTML"
        hClose hin
        c ← hGetContents hout
        length c `seq` hClose hout
        return $ Just c
      _ → return Nothing
