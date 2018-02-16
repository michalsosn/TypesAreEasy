{-# LANGUAGE TypeOperators, TypeFamilies, UndecidableInstances #-}

module Positional where

data TNat = TZero | TOne | TShiftZero TNat | TShiftOne TNat

type family Succ (a :: TNat) :: TNat where
    Succ Zero = One
    Succ One = ShiftZero One
    Succ (ShiftZero n) = ShiftOne n
    Succ (ShiftOne n) = ShiftZero (Succ n)

type family Last (a :: TNat) :: TNat where
    Last Zero = Zero
    Last One = One
    Last (ShiftZero n) = Zero
    Last (ShiftOne n) = One

type family Pred (a :: TNat) :: TNat where
    Pred One = Zero
    Pred (ShiftZero One) = One
    Pred (ShiftZero n) = ShiftOne (Pred n)
    Pred (ShiftOne n) = ShiftZero n

type family Unshift (a :: TNat) :: TNat where
    Unshift Zero = Zero
    Unshift One = Zero
    Unshift (ShiftZero n) = n
    Unshift (ShiftOne  n) = n

infixl 6 :+:
type family (a :: TNat) :+: (b :: TNat) :: TNat where
    Zero        :+: n = n
    One         :+: n = Succ n
    ShiftZero m :+: n = Last n :+: ShiftZero (m :+: Unshift n)
    ShiftOne m  :+: n = Last n :+: ShiftOne  (m :+: Unshift n)

infixl 7 :*:
type family (a :: TNat) :*: (b :: TNat) :: TNat where
    Zero        :*: n = Zero
    One         :*: n = n
    ShiftZero m :*: n = ShiftZero (m :*: n)
    ShiftOne m  :*: n = n :+: ShiftZero (m :*: n)

type family Fac (a :: TNat) :: TNat where
    Fac Zero = One
    Fac n = n :*: (Fac (Pred n))

infixl 1 :!:
type family (a :: TNat) :!: (b :: TNat) :: TNat where
    m :!: n = ShiftZero m :+: n

type T0 = Zero
type T1 = One

