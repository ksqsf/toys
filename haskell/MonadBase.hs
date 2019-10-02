import Control.Monad.Base -- transformers-base
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Identity

foo = Identity "foo"

bar, boo :: Monoid w => StateT s (WriterT w Identity) String
bar = lift $ lift foo
boo = liftBase foo
