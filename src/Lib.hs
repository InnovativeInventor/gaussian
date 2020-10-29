module Lib
    (
      gaussianGcd,
      gaussianDivision,
      parser,
      gaussianMultiplication
    ) where

parser :: Int -> Int -> Gaussian
parser a b = Gaussian a b

data Gaussian = Gaussian Int Int | Nil deriving (Show, Eq)

addition :: Gaussian -> Gaussian -> Gaussian
addition = \(Gaussian a b) -> \(Gaussian c d) -> Gaussian (a + c) (b + d)

gaussianMultiplication :: Gaussian -> Gaussian -> Gaussian
gaussianMultiplication = \(Gaussian a b) -> \(Gaussian c d) -> Gaussian ((a * c) - (b*d)) ((a *d) + (c*b))

norm :: Gaussian -> Int
norm (Gaussian a b)  = a*a + b*b

maxGauss :: Gaussian -> Gaussian -> Gaussian
maxGauss a b
  | (norm a < norm b) = b
  | (norm a > norm b) = a
  | otherwise = a

minGauss :: Gaussian -> Gaussian -> Gaussian
minGauss a b
  | (norm a > norm b) = b
  | (norm a < norm b) = a
  | otherwise = b

conjugate :: Gaussian -> Gaussian
conjugate (Gaussian a b) = Gaussian a (-1 *b)

real :: Gaussian -> Int
real (Gaussian a _) = a

imaginary :: Gaussian -> Int
imaginary (Gaussian _ b) = b

-- could probably do some fancy fmapping and higher order functions here
-- expects 
gaussianDivision :: Gaussian -> Gaussian -> (Gaussian,  Gaussian)
gaussianDivision a b = (Gaussian (fst (quotRem (real (gaussianMultiplication a (conjugate b))) (norm b)))
                (fst (quotRem (imaginary (gaussianMultiplication a (conjugate b))) (norm b))),
                Gaussian (snd (quotRem (real (gaussianMultiplication a (conjugate b))) (norm b)))
                (snd (quotRem (imaginary (gaussianMultiplication a (conjugate b))) (norm b))))

gaussianGcd :: Gaussian -> Gaussian -> Gaussian
gaussianGcd a b = if norm (snd (gaussianDivision (maxGauss a b) (minGauss a b))) == 0
  then fst (gaussianDivision (maxGauss a b) (minGauss a b))
  -- then minGauss a b
  else gaussianGcd (minGauss a b) (snd (gaussianDivision (maxGauss a b) (minGauss a b)))
