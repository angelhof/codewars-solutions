{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair sP = runPair sP $ \x y -> (x, y)
fromPair :: (a,b) -> SPair a b
fromPair (a,b) = SPair $ \f -> f a b
fst :: SPair a b -> a
fst sP = runPair sP (\x y -> x) 
snd :: SPair a b -> b
snd sP = runPair sP (\x y -> y)
swap :: SPair a b -> SPair b a
swap sP= SPair $ runPair sP $ \x y f -> f y x
curry :: (SPair a b -> c) -> (a -> b -> c)
curry f = \x y -> f $ SPair $ \p -> p x y  
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f = \sP -> runPair sP $ \x y -> f x y 

toMaybe :: SMaybe a -> Maybe a
toMaybe sM = runMaybe sM Nothing (\x -> Just x)
fromMaybe :: Maybe a -> SMaybe a
fromMaybe (Just x) = SMaybe $ \n j -> j x
fromMaybe Nothing = SMaybe $ \n j -> n
isJust :: SMaybe a -> Bool
isJust sM = runMaybe sM False (\_ -> True)
isNothing :: SMaybe a -> Bool
isNothing sM = runMaybe sM True (\_ -> False)
catMaybes :: SList (SMaybe a) -> SList a
catMaybes sL = runList sL nil' (\sM xs -> (runMaybe sM (catMaybes xs) (\j -> cons j $ catMaybes xs)))

toEither :: SEither a b -> Either a b
toEither sE = runEither sE (\l -> Left l) (\r -> Right r)
fromEither :: Either a b -> SEither a b
fromEither (Left a) = SEither $ \l r -> l a
fromEither (Right a) = SEither $ \l r -> r a
isLeft :: SEither a b -> Bool
isLeft sE = runEither sE (\l -> True) (\r -> False) 
isRight :: SEither a b -> Bool
isRight sE = runEither sE (\l -> False) (\r -> True)
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition sL = SPair $ \f -> f (keepLeft sL) (keepRight sL)
keepLeft :: SList (SEither a b) -> SList a
keepLeft sL = runList sL nil' $ \sE xs -> runEither sE (\l -> cons l (keepLeft xs)) (\r -> keepLeft xs)
keepRight :: SList (SEither a b) -> SList b
keepRight sL = runList sL nil' $ \sE xs -> runEither sE (\l -> keepRight xs) (\r -> cons r (keepRight xs))

toList :: SList a -> [a]
toList sL = runList sL [] $ \x xs -> (x:toList xs)
fromList :: [a] -> SList a
fromList [] = SList $ \n c -> n
fromList (x:xs) = cons x $ fromList xs
cons :: a -> SList a -> SList a
cons x sL = SList $ \n c -> c x sL
nil' :: SList a
nil' = SList $ \n c -> n
concat :: SList a -> SList a -> SList a
concat sL1 sL2 = runList sL1 sL2 (\x xs -> cons x $ concat xs sL2)   
null :: SList a -> Bool
null sL = runList sL True $ \_ _ -> False
length :: SList a -> Int
length sL = runList sL 0 $ \_ xs -> 1 + length xs
map :: (a -> b) -> SList a -> SList b
map f sL = runList sL nil' $ \x xs -> cons (f x) $ map f xs
zip :: SList a -> SList b -> SList (SPair a b)
zip sL1 sL2 = runList sL1 nil' (\x xs -> runList sL2 nil' (\y ys -> cons (SPair $ \f -> f x y) $ zip xs ys))  
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f acc sL = runList sL acc $ \x xs -> foldl f (f acc x) xs
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f acc sL = runList sL acc $ \x xs -> f x $ foldr f acc xs
take :: Int -> SList a -> SList a
take 0 sL = nil'
take n sL = runList sL nil' $ \x xs -> cons x $ take (n-1) xs 

