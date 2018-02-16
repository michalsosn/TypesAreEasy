{-#LANGUAGE DataKinds, TypeFamilies, TypeOperators, PolyKinds, UndecidableInstances #-}

module Relational where

import Data.Proxy

data TBool = TFalse | TTrue

type family TEq (a :: k) (b :: k) :: TBool where
    TEq a a = TTrue
    TEq a b = TFalse

type family (a :: TBool) :|: (b :: TBool) :: TBool where
    TTrue  :|: b = TTrue
    TFalse :|: b = b

type family (a :: TBool) :&: (b :: TBool) :: TBool where
    TFalse :&: b = TFalse
    TTrue  :&: b = b

data Person = Unknown | A | AA | AB | ABA | ABB | AC | ACA | B | BA | BAC | BB

type family Parent (c :: Person) :: Person where
    Parent AA = A
    Parent AB = A
    Parent ABA = AB
    Parent ABB = AB
    Parent AC = A
    Parent ACA = AC
    Parent BA = B
    Parent BAC = BA
    Parent BB = B
    Parent a = Unknown

type family IsParent (c :: Person) (p :: Person) :: TBool where
    IsParent Unknown p = TFalse
    IsParent c p = TEq (Parent c) p

type family IsAncestor (c :: Person) (a :: Person) :: TBool where
    IsAncestor Unknown a = TFalse
    IsAncestor c a = IsParent c a :|: IsAncestor (Parent c) a

probe :: (IsAncestor c a ~ TTrue) => Proxy c -> Proxy a -> ()
probe _ _ = ()


demo :: IO ()
demo = do
    print $ probe (Proxy :: Proxy AA) (Proxy :: Proxy A)
    print $ probe (Proxy :: Proxy ABA) (Proxy :: Proxy A)
    print $ probe (Proxy :: Proxy ABA) (Proxy :: Proxy AB)
--  print $ probe (Proxy :: Proxy B) (Proxy :: Proxy A)
--  print $ probe (Proxy :: Proxy BAC) (Proxy :: Proxy A)


