import AHD.Lecture
import AHD.Util
import Clash.Prelude hiding (writeFile)
import Data.Text.IO  (writeFile)

dutHalfAdder :: SystemClockResetEnable => Signal System (Bit, Bit)
dutHalfAdder = bundle (traceSignal1 "c_out" cOut, traceSignal1 "s" s)
  where
    (a,b) = unbundle bitInputs2
    (cOut,s) = unbundle $ seqHalfAdder
          (traceSignal1 "a" a) (traceSignal1 "b" b)

main :: IO ()
main = do
  let dut = exposeClockResetEnable dutHalfAdder
              systemClockGen systemResetGen enableGen
  vcd <- dumpVCD (0, 20) dut ["a", "b", "c_out", "s"]
  case vcd of
    Left msg -> error msg
    Right contents -> writeFile "halfAdder.vcd" contents
