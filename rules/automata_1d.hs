import Data.Array (listArray, (!), bounds, elems)
 
step rule a = listArray (l,r) res
  where (l,r) = bounds a
        res = [rule (a!r)     (a!l) (a!(l+1)) ] ++
              [rule (a!(i-1)) (a!i) (a!(i+1)) | i <- [l+1..r-1] ] ++
              [rule (a!(r-1)) (a!r) (a!l)     ]
 
runCA rule = iterate (step rule)

rule n l x r = n `div` (2^(4*l + 2*x + r)) `mod` 2

initial n = listArray (0,n-1) . center . padRight n
  where
    padRight n lst = take n $ lst ++ repeat 0
    center = take n . drop (n `div` 2+1) . cycle

displayCA n rule init = mapM_ putStrLn $ take n result
  where result = fmap display . elems <$> runCA rule init
        display 0 = ' '
        display 1 = '#'
