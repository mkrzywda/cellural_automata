-- CA 1D
--

import Data.Array (listArray, (!), bounds, elems)
--http://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array.html
--

step rules a = listArray (l,r) res
  where (l,r) = bounds a
        res = [rules (a!r)     (a!l) (a!(l+1)) ] ++
              [rules (a!(i-1)) (a!i) (a!(i+1)) | i <- [l+1..r-1] ] ++
              [rules (a!(r-1)) (a!r) (a!l)     ]
 
runCA rules = iterate (step rules)

rules n l x r = n `div` (2^(4*l + 2*x + r)) `mod` 2

initial n = listArray (0,n-1) . center . padRight n
  where
    padRight n lst = take n $ lst ++ repeat 0
    center = take n . drop (n `div` 2+1) . cycle

-- (<$>) :: Functor f => (a->b) -> f a -> f b
-- fmap :: (a -> b) -> f a -> f b


display1DCA n rules init = mapM_ putStrLn $ take n result
  where result = fmap display . elems <$> runCA rules init
        display 0 = '.'
        display 1 = 'λ'
