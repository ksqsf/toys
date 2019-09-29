data Acct = CAcct | SAcct
data MyNum = O | Zero | One
instance Eq Acct where
  (==) _ _ = True
  (/=) _ _ = True

instance Eq MyNum where
  O    == Zero = True
  O    == O    = True
  Zero == Zero = True
  One  == One  = True
  _ == _ = False
