----------------------------------------------------------------------
-- FILE:              Loader.hs
-- DATE:              03/07/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Loader
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Bits
import Data.Word
import Data.Char
import Data.List
import Data.Vec hiding (last)
import Data.Array
import qualified Data.Map as Map
import System.IO.Unsafe

----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Analyzer.LRU
import Arm.Encoder
import Arm.Format
import Arm.Instruction
import Arm.Memory
import Arm.Program
import Arm.Register
import Arm.RegisterName
import Arm.Pipeline
import Analyzer.Lattice
import Analyzer.PipelineModel
import Analyzer.ValueAbstraction
import Arm.CPU

----------------------------------------------------------------------
-- Load a program into a CPU.
----------------------------------------------------------------------
loadProgram
  :: (Cost a) => Program
  -> (CPU a)
  -> IO (CPU a)

loadProgram program cpu
  = let mem    = memory cpu
        main   = multi cpu Map.! 0
        regs   = registers main
        org    = origin program
        instrs = instructions program
        consts = constants program
    in do let regs'  = loadRegisters regs (regInit program)
              regs'' = setReg regs' R15 (StdVal org)
              regs''' = setReg regs'' R13 (StdVal stackSize)
          -- | load the instruction in the local instruction memory
          mem'   <- loadInstructions (instrMem main) 0 instrs
          -- | load the constants in the shared memory
          mem''  <- loadConstants mem consts

          let defTask = initial { taskRegisters = regs''', taskMemory = mem', taskShared = mem'' }
              defAbsTask = initial { task = Ready defTask }

              defaultState = PState { simtime = 0, busytime = 0, nextpc = org, cpsr = 0,
                                      regfile = regs''', mem = mem', shared = mem'',
                                      coords = Coord (defAbsTask :. defAbsTask :. defAbsTask :.()),
                                      targets = [], final = True, stableP = False, parallelP = False,
                                      busyPeriods = (0,0) }

              pipe'  =  [defaultState]
              main' = main { registers = regs''', pipeline = pipe' }
              cores = Map.insert 0 main' (multi cpu) -- the initial state one core

          return (cpu { multi = cores, memory = mem''})

-- |
instance Initialize Task where
  initial = Task { taskInstr = Nop, taskCpsr = 0, taskNextPc = 0,
                   taskRegisters = bottom, taskMemory = bottom, taskShared = bottom }

-- |
instance (Cost a) => Initialize (AbsTaskState a) where
  initial = AbsTaskState { property = start 1 1, stage = FI, task = Ready initial }


-- | Reset the pipeline upon a change in the assigned CPU
resetPipeline
  :: (Cost a) => Word32
  -> (Int, Int)
  -> Memory
  -> SharedMemory
  -> Registers
  -> (Pipeline a)

resetPipeline nextpc (c,t) mem sm regs
  = let regs' = setReg regs R15 (StdVal nextpc)
        defTask = Task { taskInstr = Nop, taskCpsr = 0, taskNextPc = 0,
                         taskRegisters = regs', taskMemory = mem, taskShared = sm }
        defAbsTask = AbsTaskState { property = start c t, stage = FI,task = Ready defTask }
        initialState = PState { simtime = t, busytime = 0,
                                nextpc = nextpc, cpsr = 0, regfile = regs',
                                mem = mem, shared = sm,
                                coords = Coord (defAbsTask :. defAbsTask :. defAbsTask :.()),
                                targets = [], final = True, stableP = False, parallelP = False,
                                busyPeriods = (0,0) }
    in [initialState]


----------------------------------------------------------------------
-- Load register pre-load values.
----------------------------------------------------------------------
loadRegisters
  :: Registers
  -> [(RegisterName, Word32)]
  -> Registers

loadRegisters regs []
  = regs

loadRegisters regs ((regName, val) : rest)
  = do let regs' = setReg regs regName (RegVal (val, val))
       loadRegisters regs' rest



----------------------------------------------------------------------
-- Load a list of instructions into memory.
----------------------------------------------------------------------
loadInstructions
  :: Memory
  -> Address
  -> [Instruction]
  -- -> [TypedInstruction]
  -> IO Memory

loadInstructions mem _ []
  = return mem

loadInstructions mem addr (ins: inss)
  = do let opcode = encode ins
       let load m a op
             =  let must' = (must m) // [(addr, (OpCode opcode))]
                in m { must  = must'}

       let mem' = load mem addr opcode

       loadInstructions mem' (addr + 1) inss



----------------------------------------------------------------------
-- Load a list of constant tuples into memory.
----------------------------------------------------------------------
loadConstants
  :: SharedMemory
  -> [ (Address, Constant) ]
  -> IO SharedMemory

loadConstants mem []
  =  return mem

loadConstants mem ((addr, const) : consts)
  = do mem' <- loadConstant mem addr const
       loadConstants mem' consts



----------------------------------------------------------------------
-- Load an arbitrary constant into memory.
----------------------------------------------------------------------
loadConstant
  :: SharedMemory
  -> Address
  -> Constant
  -> IO SharedMemory

loadConstant mem addr (Array count value)
  = loadArray mem addr count value

loadConstant mem addr (Int i)
  = do writeMemWord mem addr (ConVal (fromInteger i))

loadConstant mem addr (List l)
  = loadList mem addr l

loadConstant mem addr (String s)
  = loadString mem addr (s ++ [chr 0])

loadConstant mem addr (Word w)
  = writeMemWord mem addr (ConVal (w,w))

loadConstant mem addr (Pair pointer (Word w))
  = writeMemWord mem addr (RegVal (fromIntegral w, fromIntegral w))

----------------------------------------------------------------------
-- Load an array of constants into memory.
----------------------------------------------------------------------
loadArray
  :: SharedMemory
  -> Address
  -> Word32
  -> Constant
  -> IO SharedMemory

loadArray mem addr 0 const
  = return mem

loadArray mem addr count const
  = do mem' <- loadConstant mem addr const
       loadArray mem' (addr + constSize const) (count - 1) const



----------------------------------------------------------------------
-- Load a list of constants into memory.
----------------------------------------------------------------------
loadList
  :: SharedMemory
  -> Address
  -> [Constant]
  -> IO SharedMemory

loadList mem addr []
  = return mem

loadList mem addr (const : consts)
  = do mem' <- loadConstant mem addr const
       let addr' = constSize const + addr
       loadList mem' addr' consts



----------------------------------------------------------------------
-- Load a string into memory; null terminate the string.
----------------------------------------------------------------------
loadString
  :: SharedMemory
  -> Address
  -> String
  -> IO SharedMemory

loadString mem addr []
  = return mem

loadString mem addr [c1]
  = do
    let w = fromIntegral (ord c1)
    writeMemWord mem addr (CharVal w)

loadString mem addr [c1, c2]
  = do
    let w = (fromIntegral (ord c2) `shiftL` 8)
            .|. (fromIntegral (ord c1))
    writeMemWord mem addr (CharVal (w :: Word32))

loadString mem addr [c1, c2, c3]
  = do
    let w = (fromIntegral (ord c3) `shiftL` 12)
            .|. (fromIntegral (ord c2) `shift` 8)
            .|. (fromIntegral (ord c1))
    writeMemWord mem addr (CharVal (w :: Word32))


loadString mem addr (c1 : c2 : c3 : c4 : cs)
  = do
    let w = (fromIntegral (ord c4) `shiftL` 24)
            .|. (fromIntegral (ord c3) `shiftL` 16)
            .|. (fromIntegral (ord c2) `shiftL` 8)
            .|. (fromIntegral (ord c1))
    mem' <- writeMemWord mem addr (CharVal (w :: Word32))
    loadString mem' (addr + 4) cs



----------------------------------------------------------------------
-- Fetch a string from memory.
----------------------------------------------------------------------
fetchString
  :: SharedMemory
  -> Address
  -> IO String

fetchString mem addr
  = do (val, mem') <- readMemWord mem addr
       let word = case val of
                       --RegVal (w1,w2) -> w1
                       CharVal w -> w
                       other -> error ("fetch string " ++ show other)
       let c4 = fromIntegral ((word .&. 0xFF000000) `shift` (-24))
       let c3 = fromIntegral ((word .&. 0xFF0000) `shift` (-16))
       let c2 = fromIntegral ((word .&. 0xFF00) `shift` (-8))
       let c1 = fromIntegral (word .&. 0xFF)
       if c1 == 0
         then return ""
         else if c2 == 0
                then return [chr c1]
                else if c3 == 0
                       then return [chr c1, chr c2]
                       else if c4 == 0
                              then return [chr c1, chr c2, chr c3]
                              else do s <- fetchString mem (addr + 4)
                                      return ([chr c1, chr c2, chr c3, chr c4] ++ s)

wordToString
  :: Word32
  ->  String

wordToString word
  = do
    let c4 = fromIntegral ((word .&. 0xFF000000) `shift` (-24))
    let c3 = fromIntegral ((word .&. 0xFF0000) `shift` (-16))
    let c2 = fromIntegral ((word .&. 0xFF00) `shift` (-8))
    let c1 = fromIntegral (word .&. 0xFF)
    if c1 == 0
         then ""
         else if c2 == 0
                then [chr c1]
                else if c3 == 0
                       then [chr c1, chr c2]
                       else if c4 == 0
                              then [chr c1, chr c2, chr c3]
                              else [chr c1, chr c2, chr c3, chr c4]

----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
