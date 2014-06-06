-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.GraphMLBackend
-- Copyright   :  None
-- License     :  BSD3
--
-- Maintainer  :  Vitor Rodrigues
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Analyzer.GraphMLBackend ( NodeType (..), Eval (..),
  GeneratorSt (..), writeToGraph, mkCloseBlockNode, mkBeginBlockNode, processGraphML

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Control.Monad.State
import Data.Map
import Data.Maybe
import qualified Data.List as List
import Data.List.Utils
import Control.Monad.Reader hiding (lift,join)
import Control.Monad.Writer hiding (join)
import System.IO
import Text.XML.Light.Input

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.Semantics
import Analyzer.Lattice
import Analyzer.Label hiding (hook)
import ForSyDe.Backend.GraphML.AST
import ForSyDe.Backend.GraphML.FileIO
import Arm.Operand hiding (Rel)
import Arm.RegisterName
import Arm.Instruction


type Eval a = ReaderT (Maybe [Rel a]) (WriterT [GraphML] (StateT (GeneratorSt a) IO))
data GeneratorSt a = GeneratorSt { groupCnt :: Int,
                                   hook :: Map String (Label, Int),
                                   trace :: Bool }

instance Show (GeneratorSt a) where
  show st = show (hook st)

type GraphML = [GMLNode]


instance Initialize (GeneratorSt a) where
  initial = GeneratorSt {groupCnt = 0, hook = empty, trace = True }

data NodeType = Normal | InsideTrace Int InstrType | RecBegin  | InterleavingBegin | TraceBegin Int
                       | InterLeavingEnd Int | RecEnd (Maybe Int) | TraceEnd Int | TraceSetBegin
                       | TraceSetEnd Int | Break | ChoiceBegin | ChoiceEnd (Maybe Int)
                deriving (Show)

mkGraphPorts
  :: (Stateable a) => (Rel a)
  -> (Maybe InstrType, Maybe TraceFlow)
  -> (GMLPortId, GMLPortId)

mkGraphPorts r globalId
  = let (a, b) = cpoints r
        port_in = GMLPortId  a globalId
        port_out = GMLPortId b globalId
    in (port_in, port_out)

mkGraphNode
  :: (Stateable a) => (Rel a)
  -> (Maybe InstrType, Maybe TraceFlow)
  -> (GMLNodeId)

mkGraphNode r globalId
  = GMLNodeId  (showsPrec 1 (expr r) "" : (uniqueId r):[]) globalId



mkBeginBlockNode
  :: String
  -> Int
  -> GMLNode

mkBeginBlockNode desc cnt
  = BlockNode desc (show cnt)

mkCloseBlockNode desc cnt
  = CloseNode desc (show cnt)



writeToGraph :: (Stateable a) =>  NodeType -> Maybe (Rel a) -> (Eval a) (Maybe Int)
writeToGraph Normal (Just r)
  = do
    t <- gets trace
    if not t
       then return Nothing
       else  do
             let globalId = (Nothing, Nothing)
             --liftIO $ putStrLn $ "NORMAL " ++ show ((expr r, ppoints r))
             hooks <- gets hook
             (r', procId) <- case (exit . sink) r of
                             True -> do
                                     let Rel (a, i, b) = r
                                         p = (procName . parent . sink) r
                                         s = (section . parent . sink) r
                                         (a', id) = if  head s == '.' && p == "main"
                                                        then (a, 0)
                                                        else case Data.Map.lookup p hooks of
                                                             Just (caller, cnt) ->  (setLabel a caller, cnt)
                                                             Nothing -> (a, 0)
                                         hooks' = delete p hooks

                                     --liftIO $ putStrLn $ "READ HOOK " ++ show (p, hooks)
                                     modify (\ state@GeneratorSt {} -> state { hook = hooks' } )
                                     return $ (Rel (a', i, b), id)
                             False -> return (r, -1)


             case (bl . sink) r of
                  True ->  do
                           let call = (section . parent . sink) r
                               h = (succ (source r))

                           cnt <- liftM succ $ gets groupCnt
                           let hooks' = insert call (h,cnt) hooks
                               proc = (section . parent . sink) r
                               cnt2 = (fromInteger . ppoint . sink) r
                               name = (expr r)
                               a = mkBeginBlockNode ("proc_" ++ proc) cnt
                           modify (\ st@GeneratorSt {} -> st { groupCnt = cnt } )
                           --tell [a:[]]

                           --liftIO $ putStrLn $ "REGISTER HOOK " ++ show (call, succ (source r))
                           modify (\ state@GeneratorSt {} -> state { hook = hooks' } )
                  False -> return ()


             let ports = mkGraphPorts r' globalId
                 desc = mkGraphNode r' globalId

             case (expr r) of

                  Exec (Mov (Reg IP) (Reg SP)) ->
                        do
                        let a = InstrNode EnterProc desc ports
                        tell [a:[]]

                  Exec (Ldmfd _ _) ->
                       do
                       let a = InstrNode ExitProc desc ports
                       tell [a:[]]

                       let proc = (section . parent . sink) r
                           cnt2 = (fromInteger . ppoint . sink) r
                           name = expr r
                       let node = mkCloseBlockNode ("proc_" ++ proc) procId
                       --tell [node:[]]
                       return ()

                  Exec _ -> let a = InstrNode Otherwise desc ports
                            in tell [a:[]]

                  Cons i e -> let a = InstrNode Otherwise desc ports
                              in tell [a:[]]

                  Interleaved e -> let a = InstrNode InterleavedThread desc ports
                                   in tell [a:[]]


             return Nothing


writeToGraph Break (Just r)
  = do
    let globalId = (Nothing, Nothing)
        ports = mkGraphPorts r globalId
        desc = mkGraphNode r globalId
        a = InstrNode ExitProc desc ports
    tell [a:[]]
    return Nothing


writeToGraph (InsideTrace n color) (Just r)
  = do
    t <- gets trace
    if not t
       then return Nothing
       else do
            --liftIO $ putStrLn $ "INTERLEAVED " ++ show ((expr r, ppoints r))
            hooks <- gets hook
            (r', procId) <-
                case (exit . sink) r of
                      True -> do
                              let Rel (a, i, b) = r
                                  p = (procName . parent . sink) r
                                  s = (section . parent . sink) r
                                  (a', id) = if  head s == '.' && p == "main"
                                                 then (a, 0)
                                                 else case Data.Map.lookup p hooks of
                                                     Just (caller, cnt) ->  (setLabel a caller, cnt)
                                                     Nothing -> (a, 0)
                                  hooks' = delete p hooks

                              --liftIO $ putStrLn $ "READ HOOK " ++ show (p, hooks)
                              modify (\ state@GeneratorSt {} -> state { hook = hooks' } )
                              return $ (Rel (a', i, b), id)
                      False -> return (r, -1)


            let p = case (pos . source) r of
                         Just x -> x
                         Nothing -> error (show (expr r))
                color' = case expr r of
                              Interleaved i -> InterleavedThread
                              _ -> color
                globalId = (Just color', Just (TraceFlow n p))


            let ports = mkGraphPorts r' globalId
                desc = mkGraphNode r' globalId
                a = InstrNode color' desc ports

            case (bl . sink) r of
                 True ->  do
                          let call = (section . parent . sink) r
                              h = (succ (source r))

                          cnt <- liftM succ $ gets groupCnt
                          let hooks' = insert call (h,cnt) hooks
                              proc = (section . parent . sink) r
                              cnt2 = (fromInteger . ppoint . sink) r
                              a = mkBeginBlockNode ("proc_" ++ proc) cnt
                          modify (\ st@GeneratorSt {} -> st { groupCnt = cnt } )
                          tell [a:[]]

                          --liftIO $ putStrLn $ "REGISTER HOOK " ++ show (call, succ (source r))
                          modify (\ state@GeneratorSt {} -> state { hook = hooks' } )
                 False -> return ()

            tell [a:[]]
            return Nothing

writeToGraph RecBegin (Just r)
  = do
    t <- gets trace
    if not t
       then return Nothing
       else  do
             cnt <- liftM succ $ gets groupCnt
             --liftIO $ putStrLn $ "CREATING REC GROUP " ++ show (expr r)
             let globalId = (Nothing, Nothing)
             let ports = mkGraphPorts r globalId

             let cnt2 = (fromInteger . ppoint . source) r
             let a = mkBeginBlockNode ("rec") cnt2
                 n = mkGraphNode r globalId
                 b = InstrNode EnterLoop n ports
             --modify (\ st@GeneratorSt {} -> st { groupCnt = cnt } )
             tell [a:b:[]]
             return $ Just cnt2

writeToGraph ChoiceBegin (Just r)
  = do
    t <- gets trace
    if not t
       then return Nothing
       else  do
             cnt <- liftM succ $ gets groupCnt
             let globalId = (Nothing, Nothing)
             let ports = mkGraphPorts r globalId

             let cnt2 = (fromInteger . ppoint . source) r
             let a = mkBeginBlockNode ("choice") cnt2
                 n = mkGraphNode r globalId
                 b = InstrNode EnterLoop n ports
             tell [a:b:[]]
             return $ Just cnt2

writeToGraph (RecEnd Nothing) Nothing
  = return Nothing

writeToGraph (ChoiceEnd Nothing) Nothing
  = return Nothing

writeToGraph (RecEnd (Just bid)) Nothing
  = do
    t <- gets trace
    if not t
       then return Nothing
       else  do
             let node = mkCloseBlockNode ("rec") bid
             tell [ node:[]]
             return Nothing

writeToGraph (ChoiceEnd (Just bid)) Nothing
  = do
    t <- gets trace
    if not t
       then return Nothing
       else  do
             let node = mkCloseBlockNode ("choice") bid
             tell [ node:[]]
             return Nothing

writeToGraph InterleavingBegin Nothing
  = do
    t <- gets trace
    if not t
       then return Nothing
       else  do
             cnt <- liftM succ $ gets groupCnt
             let a = mkBeginBlockNode "conc" cnt
             modify (\ st@GeneratorSt {} -> st { groupCnt = cnt} )
             tell [a:[]]
             return $ Just cnt

writeToGraph (InterLeavingEnd bid) Nothing
  = do
    t <- gets trace
    if not t
       then return Nothing
       else  do
             let node = mkCloseBlockNode "conc" bid
             tell [ node:[]]
             return Nothing

writeToGraph TraceSetBegin Nothing
  = do
    t <- gets trace
    if not t
       then return Nothing
       else  do
             cnt <- liftM succ $ gets groupCnt
             let a = mkBeginBlockNode "traceset" cnt
             modify (\ st -> st { groupCnt = cnt } )
             tell [a:[]]
             return $ Just cnt


writeToGraph (TraceSetEnd bid) Nothing
  = do
    t <- gets trace
    if not t
       then return Nothing
       else  do
             let node = mkCloseBlockNode "traceset" bid
             tell [ node:[] ]
             return Nothing

writeToGraph (TraceBegin n) Nothing
  = do
    t <- gets trace
    if not t
       then return Nothing
       else  do
             let a = mkBeginBlockNode "trace" (n+20)
             tell [a:[]]
             return Nothing


writeToGraph (TraceEnd bid) Nothing
  = do
    t <- gets trace
    if not t
       then return Nothing
       else  do
             let node = mkCloseBlockNode "trace" (bid+20)
             tell [ node:[] ]
             return Nothing


writeToGraph other _
  = error ("WRITE TO GRAPH: " ++ show other)




processGraphML logs
  = do

    let beginMainBlock = [mkBeginBlockNode "main" 0] ++ [mkBeginBlockNode "proc" 0 ]
        endMainBlock = [mkCloseBlockNode "main" 0]
        lines = beginMainBlock : (logs) ++ endMainBlock:[]

    h <- openFile "./debug.xml" WriteMode
    mapM_ (\l -> do
                let l' = List.map show l
                hPutStrLn h (unlines l')
          ) lines
    hClose h
    x <- readFile "./debug.xml"

    let Just tree = parseXMLDoc x
    let topNodes = xlmToGraphML tree


    let allnodes = List.filter (\l -> case l of {InstrNode _ _ _ -> True; _ -> False}) (concat (List.nub lines))
    let edges = List.nub $ List.foldl (findSink allnodes) [] allnodes

    let g = GMLGraph "graph" (topNodes:[]) edges
    writeMyGraph True g "./out.graphml"

    where connect nodeIn nodeOut
              = let InstrNode color' id' (port_in', _) = nodeOut
                    --GMLPortId number' global' = port_in'
                    InstrNode color id (_, port_out) = nodeIn
                    --GMLPortId number global = port_out
                in port_out == port_in'
                --in number' == number
          addEdge n accum n'
              = let InstrNode _ id' (port_in', port_out') = n'
                    InstrNode _ id (port_in, port_out) = n
                    edge = GMLEdge n port_out n' port_in'
                in accum ++ edge:[]
          findSink ns accum n
             = let index = List.findIndices (connect n) ns
               in case index of
                       [] -> accum
                       is -> List.foldl (addEdge n) accum (List.map (\i -> ns !! i) is)

