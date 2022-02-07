module Lambda where
import Syntax
import Control.Applicative

type ReductionStrategy = Int -> DeBruijn -> Maybe DeBruijn
data MaybeBetaNormal = BNormal DeBruijn | Unnormalizable | OutOfIterations | TermTooBig

-- sub s z t == t[s/z] 
sub :: DeBruijn -> Variable -> DeBruijn -> DeBruijn
sub = subhelper 0
    where 
        subhelper :: Int -> DeBruijn -> Variable -> DeBruijn -> DeBruijn
        subhelper d s z t@(DVar i) = if (d <= i && z == toEnum (i - d)) then s else t
        subhelper d s z (DAbs t) = DAbs $ subhelper (d+1) s z t
        subhelper d s z (DApp t1 t2) = DApp (subhelper d s z t1) (subhelper d s z t2)


foldLambda :: (Int -> a) -> (a -> a) -> (a -> a -> a) -> DeBruijn -> a
foldLambda f _ _ (DVar i) = f i
foldLambda f g h (DAbs t) = g (foldLambda f g h t)
foldLambda f g h (DApp t1 t2) = h (foldLambda f g h t1) (foldLambda f g h t2)

size :: DeBruijn -> Int 
size = foldLambda (const 1) (+ 1) (\x y -> x+y+1)

mapfrees :: (Int -> Int) -> DeBruijn -> DeBruijn
mapfrees f = helper 0
    where 
        helper :: Int -> DeBruijn -> DeBruijn
        helper d t@(DVar i) = if d <= i then DVar (f i) else t
        helper d (DAbs t) = DAbs $ helper (d+1) t
        helper d (DApp t1 t2) = DApp (helper d t1) (helper d t2)

beta :: Int -> DeBruijn -> DeBruijn
beta d' (DApp (DAbs t1) t2) = helper 0 t1
    where  
        helper :: Int -> DeBruijn -> DeBruijn
        helper d (DVar i) = if i == d then (mapfrees (+d) t2) else 
                            if i < d then DVar i else DVar (i-1)
        helper d (DAbs t) = DAbs $ helper (d+1) t
        helper d (DApp t1' t2') = DApp (helper d t1') (helper d t2')

beta _ _ = error "can't beta reduce"

leftmost :: DeBruijn -> Maybe DeBruijn
leftmost = helper 0 
    where
        helper :: Int -> DeBruijn -> Maybe DeBruijn
        helper _ (DVar _) = Nothing
        helper d (DAbs t) = DAbs <$> helper (d+1) t
        helper d t@(DApp (DAbs _) _) = Just $ beta d t
        helper d (DApp t1 t2) = ((`DApp` t2) <$> helper d t1) <|> ((DApp t1) <$> helper d t2)

betanormal :: Int -> DeBruijn -> MaybeBetaNormal
betanormal bound = helper 0
    where 
        helper d t = if d >= bound then OutOfIterations else case leftmost t of
            Nothing -> BNormal t
            Just t' -> if t == t' then Unnormalizable
                       else if size t' > 5000 then TermTooBig
                       else helper (d+1) t' 
