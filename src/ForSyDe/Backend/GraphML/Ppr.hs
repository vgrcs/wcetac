{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.GraphML.Ppr
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- GraphML pretty printing instances.
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.GraphML.Ppr where

import ForSyDe.Backend.Ppr
import ForSyDe.Backend.GraphML.AST

import Data.Maybe (fromJust)
import Data.List (findIndex)
import qualified Data.Foldable as DF (foldr, toList)
import Language.Haskell.TH (pprint, Dec(FunD), Exp, nameBase)
import Text.PrettyPrint.HughesPJ


-- | The only accepted pretyprinting option
type YFilesMarkup = Bool

-- | Number of spaces used for indentation
nestVal :: Int
nestVal = 5

pprGraphGroupNode nodeId groupName =
  --text "<node" <+> text ("id=\"" ++ nodeId ++ "\"") <> text ">" $+$
  text "<data key=\"d0\">" $+$
    nest nestVal
    (text "<y:ProxyAutoBoundsNode>" $+$
        nest nestVal
        (text "<y:Realizers active=\"0\">" $+$
            nest nestVal
            (text "<y:GroupNode>" $+$
                 nest nestVal
                 (text "<y:Fill color=\"#F5F5F5\" transparent=\"false\"/>" $+$
                  text "<y:BorderStyle color=\"#000000\" type=\"dashed\" width=\"1.0\"/>" $+$
                  text "<y:NodeLabel alignment=\"right\" autoSizePolicy=\"node_width\" backgroundColor=\"#EBEBEB\" borderDistance=\"0.0\" fontFamily=\"Dialog\" fontSize=\"15\" fontStyle=\"plain\" hasLineColor=\"false\" height=\"21.453125\" modelName=\"internal\" modelPosition=\"t\" textColor=\"#000000\" visible=\"true\" width=\"520.970703125\" x=\"0.0\" y=\"0.0\">"
                         <> text "Group: " <> text nodeId <> text groupName <> text "</y:NodeLabel>" $+$
                  text "<y:Shape type=\"roundrectangle\"/>" $+$
                  text "<y:State closed=\"false\" innerGraphDisplayEnabled=\"false\"/>" $+$
                  text "<y:Insets bottom=\"15\" bottomF=\"15.0\" left=\"15\" leftF=\"15.0\" right=\"15\" rightF=\"15.0\" top=\"15\" topF=\"15.0\"/>"  $+$
                  text "<y:BorderInsets bottom=\"0\" bottomF=\"0.0\" left=\"0\" leftF=\"0.0\" right=\"0\" rightF=\"0.0\" top=\"0\" topF=\"0.0\"/>"
                 ) $+$
            text "</y:GroupNode>"
            )  $+$
            (text "<y:GroupNode>" $+$
                 nest nestVal
                 (text "<y:Fill color=\"#F5F5F5\" transparent=\"false\"/>" $+$
                  text "<y:BorderStyle color=\"#000000\" type=\"dashed\" width=\"1.0\"/>" $+$
                  text "<y:NodeLabel alignment=\"right\" autoSizePolicy=\"node_width\" backgroundColor=\"#EBEBEB\" borderDistance=\"0.0\" fontFamily=\"Dialog\" fontSize=\"15\" fontStyle=\"plain\" hasLineColor=\"false\" height=\"21.453125\" modelName=\"internal\" modelPosition=\"t\" textColor=\"#000000\" visible=\"true\" width=\"520.970703125\" x=\"0.0\" y=\"0.0\">"
                         <> text "Folder: " <> text nodeId <> text groupName <> text "</y:NodeLabel>" $+$
                  text "<y:Shape type=\"roundrectangle\"/>" $+$
                  text "<y:State closed=\"true\" innerGraphDisplayEnabled=\"false\"/>" $+$
                  text "<y:Insets bottom=\"15\" bottomF=\"15.0\" left=\"15\" leftF=\"15.0\" right=\"15\" rightF=\"15.0\" top=\"15\" topF=\"15.0\"/>"  $+$
                  text "<y:BorderInsets bottom=\"0\" bottomF=\"0.0\" left=\"0\" leftF=\"0.0\" right=\"0\" rightF=\"0.0\" top=\"0\" topF=\"0.0\"/>"
                 ) $+$
            text "</y:GroupNode>"
            )  $+$
        text "</y:Realizers>"
        ) $+$
     text "</y:ProxyAutoBoundsNode>"
     ) $+$
   text "</data>" -- $+$
   --text "</node>"


instance PprOps YFilesMarkup GMLGraph where
 pprOps yFiles (GMLGraph id nodes edges) =
  text "<graph" <+> text ("id= \"" ++ id ++ "\"") <+>
                     text "edgedefault=\"directed\" >" $+$
    nest nestVal (vSpace
                 ) $+$
    nest nestVal (vSpace $+$
                  pprOps_list yFiles (vNSpaces 1) nodes $+$
                  vSpace $+$
                  pprOps_list yFiles (vNSpaces 1) edges $+$
                  vSpace) $+$
  text "</graph>"




instance PprOps YFilesMarkup GMLNode where
 pprOps yFiles node =
   text "<node" <+> text ("id=" ++ show id) <> text ">" $+$
   nest nestVal (
     (case node of
        --green #99CC00
        --red #FF6600
        --thread #00CCFF
        --sync #FFFF99
        --inside #FFDE00
        --interleaved #FF6600
        InstrNode Otherwise _ _ ->
          yFilesNodeTags dim "#5993A3" "roundrectangle"  Nothing id (unwords portIds)

        InstrNode EnterLoop _ _ ->
          yFilesNodeTags dim "#99CC00" "roundrectangle"  Nothing id (unwords portIds)

        InstrNode ExitLoop _ _ ->
          yFilesNodeTags dim "#FF6600" "roundrectangle"  Nothing id (unwords portIds)

        InstrNode EnterThread _ _ ->
          yFilesNodeTags dim "#00CCFF" "roundrectangle"  Nothing id (unwords portIds)

        InstrNode InsideThread _ _ ->
          yFilesNodeTags dim "#FFDE00" "roundrectangle"  Nothing id (unwords portIds)

        InstrNode ExitThread _ _ ->
          yFilesNodeTags dim "#00CCFF" "roundrectangle"  Nothing id (unwords portIds)

        InstrNode InterleavedThread _ _ ->
          yFilesNodeTags dim "#FF6600" "roundrectangle"  Nothing id (unwords portIds)

        InstrNode EnterProc _ _ ->
          yFilesNodeTags dim "#00CCFF" "roundrectangle"  Nothing id (unwords portIds)

        InstrNode ExitProc _ _ ->
          yFilesNodeTags dim "#00CCFF" "roundrectangle"  Nothing id (unwords portIds)

        BlockNode _ _ -> empty
        CloseNode _ _ -> empty
        InnerGraphNode gType _ ns ->
          let g = GMLGraph id ns []
          in  pprGraphGroupNode gType ("_" ++ id) $+$
              --pprOps_list yFiles (vNSpaces 1) ns
              pprOps yFiles g

        ) $+$ vcat (map port portIds)
       ) $+$
   text "</node>"
  where
   (id, portIds) =
           case node of
                InstrNode _ id (p1, p2)
                   ->  (showsPrec 0 id "", ["portIn= " ++ show p1, "portOut= " ++ show p2])
                BlockNode _ id -> ( id, [])
                CloseNode _ id -> ( id, [])
                InnerGraphNode _ id nodes -> (id, [])

   dim = (160,50)
   -- write the yFiles specific markup for the node
   yFilesNodeTags (xsize, ysize) color shape mSide label tooltip =
    let labelLocation = maybe "modelName=\"internal\" modelPosition=\"c\""
                              (\s -> "modelName=\"sides\" modelPosition=\""++
                                     s ++ "\"")
                              mSide in
     if yFiles
      then
        (text "<data key=\"d1\"><![CDATA[" <> text tooltip <> text "]]></data>") $+$
        text "<data key=\"d0\">" $+$
         nest nestVal
          (text "<y:ShapeNode>" $+$
           nest nestVal
            (text "<y:Geometry height=\"" <> float ysize <> text "\" width=\"" <> float xsize <> text "\" x=\"0.0\" y=\"0.0\"/>" $+$
             --text "<y:Fill color=\"" <> text color <> text "\" transparent=\"false\"/>" $+$
             text ("<y:Fill color=\"" ++ color ++ "\" transparent=\"false\"/>") $+$
             text "<y:BorderStyle color=\"#000000\" type=\"line\" width=\"1.5\"/>" $+$
             text "<y:NodeLabel alignment=\"center\" autoSizePolicy=\"content\" fontFamily=\"Dialog\" fontSize=\"12\" fontStyle=\"bold\" hasBackgroundColor=\"false\" hasLineColor=\"false\"" <+> text labelLocation <+> text "textColor=\"#000000\" visible=\"true\">" <> text label <> text "</y:NodeLabel>" $+$
             text "<y:Shape type=\"" <> text shape <> text "\"/>"
           ) $+$
          text "</y:ShapeNode>"
         ) $+$
       text "</data>"
     else empty



instance PprOps YFilesMarkup  GMLEdge where
 pprOps yFiles (GMLEdge origN origP_ targetN targetP_) =
    text "<edge" <+> text ("source=" ++ origId) <+>
                     text ("sourceport=\"" ++ origP ++ "\"") <+>
                     text ("target=" ++ targetId) <+>
                     text ("targetport=\"" ++ targetP ++ "\"") <>
    if not yFiles || not continue
      then text "/>"
      else char '>' $+$
           nest nestVal
            (text "<data key=\"d2\">" $+$
               nest nestVal
                (text "<y:PolyLineEdge>" $+$
                 nest nestVal
                  (text "<y:Path sx=\"" <> float edgeOrigX <> text "\" sy=\"" <> float edgeOrigY <> text "\" tx=\"" <> float edgeTargetX <> text "\" ty=\""<> float edgeTargetY <> text "\"/>" $+$
                   text "<y:LineStyle color=\"#000000\" type=\"line\" width=\"1.0\"/>" $+$
                   text "<y:Arrows source=\"none\" target=\"standard\"/>" $+$
		   text "<y:BendStyle smoothed=\"false\"/>"
                  ) $+$
                 text "</y:PolyLineEdge>") $+$
             text "</data>") $+$
           text "</edge>"
  where
       --GMLPortId origP = origP_
       --GMLPortId targetP = targetP_
       origP = show origP_
       targetP = show targetP_

       -- Origin Node identifier
       origId = getId origN
       -- Target Node Identifier
       targetId = getId targetN
       (outOrder, inOrder, continue)
             = case (findOutOrder origN origP, findInOrder targetN targetP) of
                     (Nothing, _) -> (0,0,False)
                     (_, Nothing) -> (0,0,False)
                     (Just out_, Just in_) -> (out_, in_, True)
       -- Calculate the edge connection point for yFiles markup
       (edgeOrigX, edgeOrigY) = edgeConnection True
                                                origNodeDims nOPortsOrig
                                                outOrder -- (findOutOrder origN origP)
       (edgeTargetX, edgeTargetY) = edgeConnection False
                                               targetNodeDims nIPortsTarget
                                               inOrder --(findInOrder targetN targetP)
       (_, nOPortsOrig) = (1,1) --nIOPorts origN
       origNodeDims  = (160,50) -- nodeDims origN
       (nIPortsTarget, _) = (1,1) --nIOPorts targetN
       targetNodeDims = (160,50) -- nodeDims targetN
       -- Function to calculate where to connect an edge to a node
       -- note that in yfiles the coordinates origin of a node
       -- is located in the center, but the Y axis is inverted
       -- (negative values are in the upper side)
       edgeConnection isSource (nodeXSize, nodeYSize) totalPorts portOrder =
                                                                         (x,y)
          where x = if isSource then nodeXSize / 2 else -(nodeXSize/2)
                ySep = nodeYSize/(fromIntegral totalPorts)
                -- Absolut value of y measure from the top
                yAbs = ySep/2 + (fromIntegral portOrder) * ySep
                y = yAbs - (nodeYSize / 2)

       -- helper functions
       -------------------
       -- Find the order (starting at 0) of a input Port in a node
       findInOrder node portid = findList list
          where findList arg = findIndex (==portid) arg
                               --case findIndex (==portid) arg of
                               --     Nothing -> error ("findInOrder [" ++ show node ++ "] " ++ show portid)
                               --     Just p -> p
                list = case node of
                 InstrNode _ _ (pid,_) -> [show pid]
       -- Find the order (starting at 0) of an output Port in a node
       findOutOrder node portid = findList list
          where findList arg = findIndex (==portid) arg
                               --case findIndex (==portid) arg of
                               --     Nothing -> error ("findOutOrder [" ++ show node ++ "] " ++ show portid)
                               --     Just p -> p
                list = case node of
                 InstrNode _ _ (_, pid) -> [show pid]
       -- Get the identifier of a node
       getId node  = case node of
         InstrNode _ id _ -> show $ showsPrec 0 id ""



-- | pretty print a Graph with XML headers and key definitions
pprMyGraphWithHeaders :: YFilesMarkup -> GMLGraph -> Doc
pprMyGraphWithHeaders yFiles graph =
  text "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" $+$
  text "<?xml-stylesheet type=\"text/xsl\" href=\"xv-browser.xsl\"?>" $+$
  text "<!-- Automatically generated by ForSyDe -->" $+$
  text "<graphml" <+> xmlns <+>
  text "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" <+>
  xmlns_y <+>

  xsi_schemaLocation <>
  char '>' $+$
  nest nestVal (
    --text "<key id=\"process_type\" for=\"node\" attr.name=\"process_type\" attr.type=\"string\"/>" $+$
    --text "<key id=\"value_arg\" for=\"node\" attr.name=\"value_arg\" attr.type=\"string\"/>" $+$
    --text "<key id=\"procfun_arg\" for=\"node\" attr.name=\"procfun_arg\" attr.type=\"string\"/>" $+$
    --text "<key id=\"instance_parent\" for=\"node\" attr.name=\"instance_parent\" attr.type=\"string\"/>" $+$
    yFilesAttribs $+$
    pprOps yFiles graph) $+$
  text "</graphml>"
 where
  -- For some silly reason, yFiles uses a different GraphML target namesapce
  -- different to the one used in grapdrawing.org's GraphML primer
  xmlns = if yFiles
    then text "xmlns=\"http://graphml.graphdrawing.org/xmlns/graphml\""
    else text "xmlns=\"http://graphml.graphdrawing.org/xmlns\""
  xmlns_y = if not yFiles then empty else
    text "xmlns:y=\"http://www.yworks.com/xml/graphml\""
  xsi_schemaLocation = if yFiles
   then text "xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns/graphml http://www.yworks.com/xml/schema/graphml/1.0/ygraphml.xsd\""
   else   text "xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\""
  yFilesAttribs = if not yFiles then empty else
   text "<key for=\"node\" id=\"d0\" yfiles.type=\"nodegraphics\"/>"  $+$
   text "<key attr.name=\"description\" attr.type=\"string\" for=\"node\" id=\"d1\"/>" $+$
   text "<key for=\"edge\" id=\"d2\" yfiles.type=\"edgegraphics\"/>" $+$
   text "<key attr.name=\"description\" attr.type=\"string\" for=\"edge\" id=\"d3\"/>"

-------------------------
-- Tag printing functions
-------------------------

port :: GraphMLPortId -> Doc
port id = text "<port" <+> text ("name=\"" ++ id ++ "\"") <> text "/>"
