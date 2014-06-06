-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.GraphML.FileIO
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions working with files in the GraphML backend.
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.GraphML.FileIO where

import ForSyDe.Backend.GraphML.AST
import ForSyDe.Backend.GraphML.Ppr(YFilesMarkup, pprMyGraphWithHeaders)

import System.IO
import Text.PrettyPrint.HughesPJ


-- | Write a design file to a file in disk
writeMyGraph :: YFilesMarkup -> GMLGraph -> FilePath -> IO ()
writeMyGraph yFiles graph fp = do
 handle     <- openFile fp WriteMode
 hPutStr handle $ (render . pprMyGraphWithHeaders yFiles) graph
 hClose handle
