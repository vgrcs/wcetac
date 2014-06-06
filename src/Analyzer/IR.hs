-----------------------------------------------------------------------------
--
-- Module      :  Analyzer.IR
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

module Analyzer.IR ( CFG (..), labels , cut, cut2, headG, tailG, normalize, toList,
       transform, insertInterleave, enumerate, colorfyG, TransformState(..)

) where


-----------------------------------------------------------------------------
-- Standard libraries.
-----------------------------------------------------------------------------
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Reader hiding (lift,join)
import Control.Monad.State hiding (lift,join)

-----------------------------------------------------------------------------
-- Local libraries.
-----------------------------------------------------------------------------
import Analyzer.Semantics
import Analyzer.Label
import Analyzer.Certificate
import Analyzer.Operators
import Arm.Instruction

data CFG a = Seq (CFG a) (CFG a)
           | Rec (CFG a) (CFG a)
           | Iter (CFG a) (CFG a)
           | Leaf (Rel a)
           | Leaf2 (Rel a)
           | End (Rel a)
           | LeafInLv (Rel a) (CFG a)
           | Choice (Rel a) (CFG a) (CFG a)
           | Conc (Rel a) (CFG a) (CFG a)
           | InLv (CFG a) (CFG a) (CFG a)
           | EmptyGraph
           deriving (Eq)


instance Stateable a => Show (Rel a) where
  show (r) = show (expr r) ++ " {" ++ show (ppoints r) ++ "} "

instance Stateable a => Show (CFG a) where
  show (Seq a b) =  " (" ++ show a ++ " * " ++ show b ++ ") "
  show (Rec a b) =  " (" ++ show a ++ " + " ++ show b ++ ") "
  show (Iter a b) =  " (" ++ show a ++ " $ " ++ show b ++ ") "
  show (Leaf a)  = let s = source a
                   in if root s
                         then "\nPROCEDURE= " ++  show ((procName . parent . source) a) ++ "\n" ++ show (expr a)
                         else if hook s
                                 then "\nHOOK= " ++ show ((procName . parent . source) a) ++ "\n" ++ show (expr a)
                                 else if bl (sink a)
                                         then "\nCALL " ++ show ((section . parent . sink
                                         ) a) ++ " = " ++ show a -- (expr a)
                                         else show a -- (expr a)
  show (Leaf2 a)  = let s = source a
                    in if root s
                         then "\nPROCEDURE= " ++  show ((procName . parent . source) a) ++ "\n" ++ show (expr a)
                         else if hook s
                                 then "\nHOOK= " ++ show ((procName . parent . source) a) ++ "\n" ++ show (expr a)
                                 else if bl (sink a)
                                         then "\nCALL " ++ show ((section . parent . sink
                                         ) a) ++ " = " ++ show a --(expr a)
                                         else show a -- (expr a)
  show (End a) = "# " ++ show a ++ " #"
  show (Choice r a b) =  " ( alt " ++ show r ++ " * " ++ show a ++ " / " ++ show b ++ " * wide) "
  show (Conc r a b) = " ( conc " ++ show r ++ " * " ++ show a ++ " <=> " ++ show b ++ ") "
  show (InLv r a b) = " [ inlv <<" ++ show r ++ ">> \n" ++ show a ++ "\n <||> \n" ++ show b ++ "] "
  show (LeafInLv r c) = "\nTRACE OF\n" ++ show (expr r) ++ "\nINSIDE\n" ++ show c
  show EmptyGraph = " id "


toList
  :: (Stateable a) => CFG a
  -> [Rel a]

toList cfg
  = (sort . nub)  (toList_ [] cfg)
    where toList_ r EmptyGraph = r
          toList_ r (Leaf a)  = sort $ nub (a:r)
          toList_ r (Leaf2 a)  = sort $ nub (a:r)
          toList_ r (LeafInLv a graphB)  = sort $ nub (a:r) ++ toList_ [] graphB
          toList_ r (Rec (Leaf b) graph) = toList_ [] graph ++ b:r
          toList_ r (Iter (Leaf b) graph) = toList_ [] graph ++ b:r
          toList_ r (Choice c graphA graphB) = toList_  (toList_ (c:r) graphB) graphA
          toList_ r (Seq graphA graphB) = toList_ (toList_ r graphB) graphA
          toList_ r (Conc c graphA graphB) = toList_  (toList_ (c:r) graphB) graphA

headG
  :: Stateable a => CFG a
  -> CFG a
headG (Leaf a)
   = (Leaf a)
headG (Seq EmptyGraph graph)
   = headG graph
headG (Seq graph EmptyGraph)
   = headG graph
headG (Seq graph (Leaf a) )
   = headG graph
headG (Seq graphA graphB )
   = headG graphA


headG_
  :: Stateable a => CFG a
  -> (Maybe (Rel a), CFG a)
headG_ (Leaf a)
   = (Just a, EmptyGraph)
headG_ (Seq EmptyGraph graph)
   = headG_ graph
headG_ (Seq graph EmptyGraph)
   = headG_ graph
headG_ (Seq (Leaf a) graph)
   = (Just a, graph)
headG_ (Seq a b)
   = let (r,g) = headG_ a
     in (r, Seq g b)

headG_ (Iter head body )
   = (Nothing, Iter head body)
headG_ (Rec head body )
   = (Nothing, Rec head body)
headG_ (Conc c head body )
   = (Nothing, Conc c head body)
headG_ (Choice c right left )
   = (Nothing, Choice c right left)
headG_ other = error ("headG_ " ++ show other)




tailG
  :: (Stateable a) => CFG a
  ->  (CFG a, Maybe (Rel a))

tailG (Leaf a)
   = (EmptyGraph, Just a)

tailG (Seq EmptyGraph (Leaf a))
   = (EmptyGraph, Just a)

tailG (Seq (Leaf a) (Leaf b))
   = (Seq (Leaf a) EmptyGraph, Just b)

tailG (Seq graph (Leaf b))
   = (graph, Just b)

tailG (Rec head body )
   = (Rec head body, Nothing)

tailG (Seq graphA graphB)
   = let (cfg, a) = tailG graphB
     in (Seq graphA cfg, a)

tailG EmptyGraph = (EmptyGraph, Nothing)

tailG other = error ("tailG: " ++ show other)



labels accum (Leaf a)
  = accum ++ (source a):[]
labels accum (Seq EmptyGraph (Leaf a))
  = accum ++ (source a):[]
labels accum (Seq (Leaf a) EmptyGraph)
  = accum ++ (source a):[]
labels accum (Seq (Leaf a) (Leaf b))
  = accum ++ (source a):[] ++ (source b):[]
labels accum (Seq graph (Leaf b))
  = (labels accum graph) ++ (source b):[]
labels accum (Choice c graphA graphB)
  = labels (labels (accum ++ (source c):[]) graphA) graphB
labels accum (Rec (Leaf b) graph)
  = labels (accum ++ (source b):[]) graph
labels accum (Iter (Leaf b) graph)
  = labels (accum ++ (source b):[]) graph
labels accum (Seq graphA graphB)
  = labels (labels accum graphA) graphB
labels accum EmptyGraph
  = accum
labels accum other
  = error $ "labels " ++ show other


cut
  :: Stateable a => Bool -> Label
  -> CFG a
  -> IO (CFG a)

cut dbg label (Leaf a)
  = do
    if source a >= label
       then do
            if dbg
               then   putStrLn $ "CUT AT= " ++ show (source a)
               else   return ()
            return EmptyGraph
       else return (Leaf a)

cut dbg label (Seq graph EmptyGraph)
  = cut dbg label graph

cut dbg label (Seq a b)
  = do
    b' <- cut dbg label b
    if dbg
       then  putStrLn $ "CUT SEQ => b=" ++ show b ++ " , b'=" ++ show b'
       else  return ()
    case b == b' of
         False -> cut dbg label $ Seq a b'
         True  -> return $ Seq a b

cut dbg label (Rec (Leaf b) body)
  = do
    g <- cut dbg label body
    if dbg
       then  putStrLn $ "CUT REC => b=" ++ show body ++ " , b'=" ++ show g
       else  return ()
    case g == body of
         False -> return g -- EmptyGraph --g
         True -> return $  (Rec (Leaf b) body)

cut dbg label (Choice c a b)
  = do
    a' <- cut dbg label a
    b' <- cut dbg label b
    case a == a' && b == b' of
         False  ->  return $ Choice c a' b' -- EmptyGraph --
         True   ->  return $ Choice c a  b

cut _ label EmptyGraph
  = return EmptyGraph

--cut label (Seq a b)
--  = error $ "cut " ++ show a ++ "\n\n" ++ show b

cut _ label other
  = error $ "cut " ++ show other



insertInterleave
  :: Stateable a => Label
  -> CFG a
  -> CFG a
  -> IO (CFG a)

insertInterleave label (Leaf a) interleave
  = do
    if sink a == label
       then do
            --putStrLn $ "CUT AT= " ++ show (a) ++ "\n" ++ show interleave
            return $ Seq (Leaf a) interleave -- (Leaf2 interleave)
       else return (Leaf a)

insertInterleave label (Seq a b) interleave
  = do
    a' <- insertInterleave label a interleave
    b' <- insertInterleave label b interleave
    return $ Seq a' b'

insertInterleave label (Rec l graph) interleave
   =  do
      graph' <- insertInterleave label graph interleave
      return $ Rec l graph'

insertInterleave label (Choice c a b) interleave
  = do
    a' <- insertInterleave label a interleave
    b' <- insertInterleave label b interleave
    return $ (Choice c a' b')

insertInterleave label EmptyGraph interleave
  = return EmptyGraph

insertInterleave label (Leaf2 a) interleave
  = return (Leaf2 a) --error $ "insertInterleave " ++ show other


cut2
  :: Stateable a => Label
  -> CFG a
  -> IO (CFG a)

cut2 label (Leaf a)
  = do
    if source a /= label
       then do
            --putStrLn $ "CUT2 AT= " ++ show (source a)
            return EmptyGraph
       else do
            --putStrLn $ "DONT CUT2 AT= " ++ show (source a)
            return (Leaf a)

cut2 label (Seq graph EmptyGraph)
  = cut2 label graph

cut2 label (Seq graph (Leaf b))
  = do
    g <- cut2 label (Leaf b)
    case g of
         EmptyGraph  -> cut2 label graph
         _   ->  return $ Seq graph (Leaf b)

cut2 label (Seq graph (Rec (Leaf b) body))
  = do
    g <- cut2 label body
    --putStrLn $ "REC2 " ++ show g ++ ">> " ++ show label
    case (g, g == body) of
         (EmptyGraph, _) -> cut2 label graph
         (_, False)  ->  return $ Seq graph (Seq (Leaf b) g)
         (_, True)   ->  return $ Seq graph (Rec (Leaf b) body)


cut2 label EmptyGraph
  = return EmptyGraph

cut2 label (Seq a b)
  = error $ "cut2 " ++ show a ++ "\n\n" ++ show b


enumerate EmptyGraph c
  = (EmptyGraph, c)

enumerate (Leaf (Rel (b, e, a))) c
  = ((Leaf (Rel (setILvPos b (c+1), e, setILvPos a c))), c+1)

enumerate (Leaf2 (Rel (b, e, a))) c
  = ((Leaf2 (Rel (setILvPos b (c+1), e, setILvPos a c))), c+1)

enumerate (Seq a b) c
  = let (a',c') = (enumerate a c)
        (b',c'') = (enumerate b c')
    in ((Seq a' b'), c'')

enumerate other _
 = error $ "enumerate " ++ show other



normalize (Seq EmptyGraph a) = a

normalize (Seq a EmptyGraph) = a

normalize (Leaf a) = Leaf a

normalize (End a) = End a

normalize (Leaf2 a) = Leaf2 a

normalize (LeafInLv a b) = LeafInLv a b

normalize (Seq (Seq a b) c)
   = let a' = normalize a
         b' = normalize b
         c' = normalize c
     in normalize (Seq a' (Seq b' c'))

normalize (Seq a b)
   = Seq (normalize a) (normalize b)

normalize (Rec l graph)
   = Rec l (normalize graph)

normalize (Iter l graph)
   = Iter l (normalize graph)

normalize (Choice c a b)
   = Choice c (normalize a) (normalize b)

normalize (Conc c a b)
   = Conc c (normalize a) (normalize b)

normalize EmptyGraph
   = EmptyGraph

normalize other
   = error $ "normalize " ++ (show other)

sinkNodes
  :: (Show a, Stateable a) =>
     (a, a, a, a) -> Map.Map (Integer, Integer) Loop -> Bool

sinkNodes (a, b, c, d) edges
  = let  id = ppoint . getLabel
         filter (k1,k2) _ = (k1 == id a) || (k2 == id b) || (k2 == id c) || (k2 == id d)
         --filter (k1,k2) _ = (k1 == id a) && (k2 == id b)) || ((k1 == id c) && (k2 == id d))
         l:ls = Map.elems $ Map.filterWithKey filter edges
    in all (== l) ls

branchInstr (Beq _) = True
branchInstr (Bne _) = True
branchInstr (Bgt _) = True
branchInstr (Bge _) = True
branchInstr (Ble _) = True
branchInstr (Blt _) = True
branchInstr (Blo _) = True
branchInstr (Bls _) = True
branchInstr (Bhi _) = True
branchInstr (Bhs _) = True
branchInstr (B _) = True
branchInstr (Bl _) = True
branchInstr _ = False


data TransformState =  TransformState { count :: EdgeCount, traceT :: Bool }

transform
  :: (Show a, Stateable a)
--  =>  CFG a -> ReaderT NodeCount IO (CFG a)
  =>  CFG a -> StateT TransformState IO (CFG a)

transform (Seq (Leaf a) EmptyGraph)
  = return (Leaf a)

transform (Seq (Leaf rb) graph)
  = do
    let top = headG_ graph

    {-if ((ppoint . source ) rb, (ppoint . sink ) rb) == (120, 121)
       then do 
            liftIO $ putStrLn ("\ntransform= " ++ show (expr rb) ++ " => " ++show (top))
            modify (\ s -> s { traceT = True} )
       else return ()-}

    case  top of
          --(Nothing, EmptyGraph) -> return (Leaf rb)
          (Nothing, graph') -> do
                               trace <- gets traceT

                               if trace 
                                  then do 
                                       liftIO $ putStrLn ("arg= " ++ show (expr rb) ++ " => " ++show (ppoints rb))   
                                       liftIO $ putStrLn ("tail= " ++ show (graph') )
                                       return ()
                                  else return ()
 
                               --liftIO $ putStrLn ("================================\n" ++ show graph')
                               graph'' <- transform graph'
                               --liftIO $ putStrLn (show graph'' ++ "\n================================")
                               return $ (Seq (Leaf rb) graph'' )
                               --error $ (show (expr ra)) ++ (show graph)
          (Just ra, graph') -> do
                               loops <- gets count
                               trace <- gets traceT
                               
                               if trace 
                                  then do 
                                       liftIO $ putStrLn ("arg= " ++ show (expr rb) ++ " => " ++show (ppoints rb)) 
                                       liftIO $ putStrLn ("top= " ++ show (expr ra) ++ " => " ++show (ppoints ra))
                                       return ()
                                  else return ()

                               let Rel (sinkA, ia, sourceA) = ra
                                   Rel (sinkB, ib, sourceB) = rb
                                   id = ppoint . getLabel
                                   branchRa = case ia of { (Exec a) -> branchInstr a ; _ -> False }
                                   branchRb = case ib of { (Exec b) -> branchInstr b ; _ -> False }

                                   sameA = let c = Map.elems $
                                                   Map.filterWithKey (\(k1,k2) _ -> (id sourceA) == k1) loops
                                           in case c of
                                                   (cA:countA) -> if all (== cA) countA then Just cA else Nothing
                                                   [] -> Nothing
                                   sameB = let c = Map.elems $
                                                   Map.filterWithKey (\(k1,k2) _ -> (id sourceB) == k1) loops
                                           in case c of
                                                   (cB:countB) -> if all (== cB) countB then Just cB else Nothing
                                                   [] -> Nothing
                               
                               {-if (id sourceA) == 122 && (id sinkA == 123)
                                  then do 
                                       liftIO $ putStrLn (show (ib, ia))
                                       error "here"
                                  else return ()-}

                               if sameA == sameB --all (== cA) countA && all (== cB) countB  && cA == cB
                                  -- && not ( branchRa || branchRb || (hook (source ra)) || (bl (sink ra)) )
                               --if (sinkNodes (sourceA, sinkA, sourceB, sinkB) loops)
                                  && not ( (hook (source ra)) || (bl (sink ra)) || branchRa ||
                                           -- (hook (source rb)) ||
                                           (bl (sink rb)) || branchRb )
                                  then do
                                       let i = appendExpr ib ia
                                           r = Rel (sinkA, i , sourceB)
                                           --

                                       --if (id sinkA, id sourceA) == (39, 23)
                                       --   then liftIO $ putStrLn ("reduce= " ++ show (expr ra) ++ " => " ++show (bl (sink ra)))
                                       --   else return ()

                                       {-if case ib of { (Exec b) -> branchInstr b ; _ -> False }
                                          then do
                                               graph'' <- transform graph
                                               return (Seq (Leaf rb) graph'')
                                          else-}
                                       transform (Seq (Leaf r) graph')

                                 else do
                                      --liftIO $ putStrLn ("no reduction")
                                      graph'' <- transform graph
                                      return (Seq (Leaf rb) graph'')

transform (Rec (Leaf b) graph)
  = do
    graph'<- transform graph
    return $ Rec (Leaf b) graph'

transform (Iter (Leaf b) graph)
  = do
    graph'<- transform graph
    return $ Iter (Leaf b) graph'

transform (Choice c graphA graphB)
  = do
    graphA' <- transform graphA
    graphB' <- transform graphB
    return $ Choice c graphA' graphB'

transform (Conc c graphA graphB)
  = do
    graphA' <- transform graphA
    graphB' <- transform graphB
    return $ Conc c graphA' graphB'

transform (Seq graphA graphB)
  = do
    --liftIO $ putStrLn ("Main transform: " ++ show (graphA) )
    graphA' <- transform graphA
    graphB' <- transform graphB
    return $ Seq graphA' graphB'

transform EmptyGraph = return EmptyGraph
transform (Leaf a)  = return (Leaf a)



colorfyG (Leaf a) = Leaf2 (colorfy InterleavedC a)
colorfyG (Seq a b)
   = Seq (colorfyG a) (colorfyG b)
colorfyG (Rec l graph)
   = Rec l (colorfyG graph)
colorfyG (Iter l graph)
   = Iter l (colorfyG graph)
colorfyG (Choice c a b)
   = Choice c (colorfyG a) (colorfyG b)
colorfyG EmptyGraph
   = EmptyGraph
