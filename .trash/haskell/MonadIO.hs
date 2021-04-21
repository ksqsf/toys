import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Control.Applicative
import Data.Char

setPassword :: MaybeT (WriterT [String] IO) ()
setPassword = do
  lift $ lift $ putStrLn "Please set a password"
  pass <- liftIO $ getLine
  guard (isPasswordValid pass)
  lift $ tell [pass]

isPasswordValid s = length s >= 8 && check s
  where check s = and [f s | f <- map any [isUpper, isLower, isNumber]]
