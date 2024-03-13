module PrettyPrinter where
import Text.Pretty.Simple (pPrint)
import qualified Prelude as P

prettyPrint :: P.Show a => a -> P.IO ()
prettyPrint = pPrint
