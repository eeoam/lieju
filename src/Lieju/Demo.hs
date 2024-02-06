module Lieju.Demo where

import Data.Void ( Void )
import GHC.Generics ( Generic )
import Data.Proxy ( Proxy(..) )
import Lieju.Enum

data Action = Code Bool | Eat Bool Bool | Sleep ()
    deriving stock (Show, Generic)
    deriving anyclass (MyEnum)

demo :: IO ()
demo = do
    print $ cardinality (Proxy @Void)
    print $ cardinality (Proxy @())
    print $ cardinality (Proxy @Bool)
    let n =  cardinality (Proxy @Action)
    print n
    print (map toMyEnum [ 0..n-1 ] :: [ Action ])

