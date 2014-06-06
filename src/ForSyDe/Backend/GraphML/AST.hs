-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.GraphML.AST
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- AST covering the GraphML subset we are interested in for this backend
-----------------------------------------------------------------------------

--FIXME: the design of this module is ugly

{-# LANGUAGE NamedFieldPuns  #-}
module ForSyDe.Backend.GraphML.AST where

import Text.XML.Light.Types
import Control.Exception

type PortId = String
type GraphMLGraphId = String
type GraphMLPortId = String
type GraphMLNodeId = String

type UniqueId = (Maybe InstrType, Maybe TraceFlow)


data GMLPortId = GMLPortId String UniqueId -- deriving (Eq)

instance Eq GMLPortId where
  (GMLPortId sa (ia, ta)) == (GMLPortId sb (ib, tb))
    = case (ia, ib) of
           (Nothing, Just EnterThread) -> sa == sb

           (Just ExitThread, Nothing) -> sa == sb

           (Just EnterThread, Just InsideThread)  ->
              let Just (TraceFlow fa pa) = ta
                  Just (TraceFlow fb pb) = tb
              in sa == sb && fa == fb

           (Just InsideThread, Just InsideThread)  ->
              let Just (TraceFlow fa pa) = ta
                  Just (TraceFlow fb pb) = tb
              in sa == sb && fa == fb -- okay

           (Just InsideThread, Just ExitThread)  ->
              let Just (TraceFlow fa pa) = ta
                  Just (TraceFlow fb pb) = tb
              in sa == sb && fa == fb

           (Just InsideThread, Just InterleavedThread)  ->
              let Just (TraceFlow fa pa) = ta
                  Just (TraceFlow fb pb) = tb
              in pb == pa + 1  && fa == fb -- okay

           (Just InterleavedThread, Just InsideThread)  ->
              let Just (TraceFlow fa pa) = ta
                  Just (TraceFlow fb pb) = tb
              in pb == pa + 1  && fa == fb

           (Just InterleavedThread, Just InterleavedThread)  ->
              let Just (TraceFlow fa pa) = ta
                  Just (TraceFlow fb pb) = tb
              in pb == pa + 1  && fa == fb

           (Just InterleavedThread, Nothing)  ->
              sa == sb

           (Nothing, Just InterleavedThread)  ->
              sa == sb

           _  ->  sa == sb && ia == ib && ta == tb


data GMLNodeId = GMLNodeId [String] UniqueId deriving (Eq)



data TraceFlow = TraceFlow Int Int deriving (Eq)

instance Show GMLPortId where
  show (GMLPortId label id)
     = case id of
            (_, Nothing) -> label
            (_, Just t) -> label ++ ", Trace= " ++ show t
            --(Nothing, Nothing) -> label
            --(Just n, Nothing) -> label ++ ", Block= " ++ show n
            --(Nothing, Just t) -> label ++ ", Trace= " ++ show t
            --(Just n, Just t) -> label ++ ", Block= " ++ show n ++ ", Pos= " ++ show t

instance Show GMLNodeId where
  show (GMLNodeId instr id)
     = case id of
            (_, Nothing) -> unwords instr
            (_, Just t) -> unwords instr ++ ", Trace= " ++ show t
            --(Nothing, Nothing) -> unwords instr
            --(Just n, Nothing) -> unwords instr ++ ", Block= " ++ show n
            --(Nothing, Just t) -> unwords instr ++ ", Trace= " ++ show t
            --(Just n, Just t) -> unwords instr  ++ ", Block= " ++ show n ++ ", Pos= " ++ show t
  showsPrec 0 (GMLNodeId instr id)
     = case id of
            (_, Nothing) -> ((unlines instr) ++ )
            (_, Just t) -> ((unlines instr ++ "Trace= " ++ show t) ++)
            --(Nothing, Nothing) -> ((unlines instr ) ++ )
            --(Just n, Nothing) -> ((unlines instr ++ "Block= " ++ show n) ++)
            --(Nothing, Just t) -> ((unlines instr ++ "Trace= " ++ show t) ++)
            --(Just n, Just t) -> ((unlines instr  ++ "Block= " ++ show n ++ "Pos= " ++ show t) ++)
  showsPrec _ n = ((show n) ++)



instance Show TraceFlow where
  show  (TraceFlow t _) = show t


-- | Main AST type, a graph
data GMLGraph = GMLGraph
        String -- Graph id
        [GMLNode] -- Nodes
        [GMLEdge] -- Edges

-- | Edge
data GMLEdge = GMLEdge
  GMLNode    -- Origin node
  GMLPortId  -- Origin port id
  GMLNode    -- Target node
  GMLPortId  -- Target port id
  deriving (Eq)

data InstrType = EnterLoop | ExitLoop | EnterThread | ExitThread | InsideThread  | InterleavedThread |
                 Otherwise | EnterProc | ExitProc
  deriving (Eq, Enum, Show)

-- | Node
data GMLNode =
 --InstrNode InstrType GMLNodeId (GMLPortId, GMLPortId) |
 InstrNode InstrType GMLNodeId (GMLPortId, GMLPortId) |
 BlockNode String String  |
 CloseNode String String  |
 InnerGraphNode String String [GMLNode] -- |
 -- InterLeavedNode String


instance Show GMLNode where
  show (InstrNode color i (a,b))
       -- = let GMLPortId port_in _ = a
       --      GMLPortId port_out _ = b
       --  in  port_in ++ " : " ++ show i ++  " : " ++  port_out
    = let GMLPortId port_in _ = a
          GMLPortId port_out _ = b
          GMLNodeId (instr:ports:[]) id = i
          --instr = head desc
          --ports = head (tail desc)
          trace = snd id
      in case trace of
         Nothing -> "<node portIn=" ++ show port_in ++ " portOut=" ++ show port_out ++
                   " instr=" ++ show instr ++ " id=" ++ show ports ++
                   " type=\"" ++ show (fromEnum color) ++ "\"" ++ "/>"
         Just t  ->
            let (TraceFlow n p) = t
            in "<node portIn=" ++ show port_in ++ " portOut=" ++ show port_out ++
               " instr=" ++ show instr ++ " id=" ++ show ports ++ " trace=\"" ++ show t ++ "\"" ++
               " type=\"" ++ show (fromEnum color) ++ "\" pos=\"" ++ show p ++ "\"/>"


  show (BlockNode desc i)
       -- = "\n{begin " ++ desc ++ " block: " ++ (show i)
    = case desc of
           "main" -> "<?xml-stylesheet type=\"text/xsl\" href=\"xv-browser.xsl\"?>\n<block name=\"main\" id=\"0\" >\n"
           _ ->  "<block name=\"" ++ desc ++ "\" id=" ++ show i ++ ">\n"

  show (CloseNode desc i)
       = case desc of
              "main" -> "</block>"
              _  -> "</block>\n"
              -- "}\n end " ++ desc ++ " block: " ++ (show i)

  show (InnerGraphNode s i ns)
       = "\n==== InnerGraph " ++ s ++ " " ++ show i ++ "\n" ++ show ns ++ "\n===="

instance Eq GMLNode where
  InstrNode x a b == InstrNode y c d = a == c && b == d && x == y
  InstrNode _ a b == BlockNode _ _ = False
  BlockNode x a ==  BlockNode y b = a == b && x == y
  BlockNode _ _ == InstrNode _ a b = False
  CloseNode a b == CloseNode c d = a == c && b == d
  CloseNode _ _ == _ = False
  _ == CloseNode _ _ = False
  InnerGraphNode x a b == InnerGraphNode y c d  = x == y &&  a == c && b == d
  InnerGraphNode _ _ _ == _ = False
  _ == InnerGraphNode _ _ _ = False


xlmToGraphML_ (Elem e@Element{ elName, elAttribs, elContent })
  = let local = qName elName
    in case elAttribs of
            nameAttr:idAttr:[] ->
               let name_ = qName (attrKey nameAttr)
                   id_ = qName (attrKey idAttr)
                   name = attrVal nameAttr
                   id = attrVal idAttr
                   n = assert (name == "name") nameAttr
                   i = assert (name == "id") idAttr
                   elems = filter (\c -> case c of  {Elem _ -> True;  _ -> False} ) elContent
                   inner = map xlmToGraphML_ elems
               in InnerGraphNode name id inner

            inAttr:outAttr:instrAttr:idAttr:typeAttr:[] ->
               let instruction_ = qName (attrKey instrAttr)
                   in_ = qName (attrKey inAttr)
                   out_ = qName (attrKey outAttr)
                   id_ = qName (attrKey idAttr)
                   type_ = qName (attrKey typeAttr)

                   instruction = attrVal instrAttr
                   id = attrVal idAttr
                   port_in = attrVal inAttr
                   port_out = attrVal outAttr
                   itype = attrVal typeAttr

                   i = assert (instruction_ == "instr") instrAttr
                   idd = assert (id_ == "id") idAttr
                   oo = assert (out_ == "portOut") outAttr
                   ii = assert (in_ == "portInt") inAttr
                   ty = assert (type_ == "type") typeAttr

                   t = read itype :: Int

               in InstrNode (toEnum t) (GMLNodeId (instruction:id:[]) (Nothing, Nothing))
                            (GMLPortId port_in (Nothing, Nothing),
                             GMLPortId port_out (Nothing, Nothing))

            inAttr:outAttr:instrAttr:idAttr:traceAttr:typeAttr:posAttr:[] ->
               let instruction_ = qName (attrKey instrAttr)
                   in_ = qName (attrKey inAttr)
                   out_ = qName (attrKey outAttr)
                   id_ = qName (attrKey idAttr)
                   trace_ = qName (attrKey traceAttr)
                   type_ = qName (attrKey typeAttr)
                   pos_ = qName (attrKey posAttr)

                   instruction = attrVal instrAttr
                   id = attrVal idAttr
                   port_in = attrVal inAttr
                   port_out = attrVal outAttr
                   trace = attrVal traceAttr
                   itype = attrVal typeAttr
                   pos = attrVal posAttr

                   i = assert (instruction_ == "instr") instrAttr
                   idd = assert (id_ == "id") idAttr
                   oo = assert (out_ == "portOut") outAttr
                   ii = assert (in_ == "portInt") inAttr
                   tt = assert (trace_ == "trace") traceAttr
                   ty = assert (type_ == "type") typeAttr
                   pp = assert (type_ == "pos") typeAttr

                   n = read trace :: Int
                   t = read itype :: Int
                   p = read pos :: Int

               in InstrNode (toEnum t) (GMLNodeId (instruction:id:[]) (Nothing, Just (TraceFlow n p)))
                            (GMLPortId port_in (Nothing, Just (TraceFlow n p)),
                             GMLPortId port_out (Nothing, Just (TraceFlow n p)))
            other -> error $ "xlmToGraphML_ " ++ show other

--xlmToGraphML_ (Text c)
--              | CRef String

--xlmToGraphML_ other = error $ "xlmToGraphML_ " ++ show other

xlmToGraphML e@Element{ elName, elAttribs, elContent }
  = let local = qName elName
    in case elAttribs of
            nameAttr:idAttr:[] ->
               let name = qName (attrKey nameAttr)
                   id = attrVal idAttr
                   n = assert (name == "name") nameAttr
                   elems = filter (\c -> case c of  {Elem _ -> True;  _ -> False} ) elContent
                   inner = map xlmToGraphML_ elems
               in InnerGraphNode name id inner
            other -> error $ "xlmToGraphML " ++ show other




