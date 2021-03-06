module Tree (module Tree) where

data Tree e = Fork e (Tree e) (Tree e) | EmptyTree

contents (Fork e l r) = e
left     (Fork e l r) = l
right    (Fork e l r) = r

weight  EmptyTree   = 0
weight (Fork e l r) = 1+(weight l)+(weight r)

height  EmptyTree   = 0
height (Fork e l r) =
 let lh = height l
     rh = height r
 in 1 + if lh > rh then lh else rh

                         -- make  Tree a  showable
instance Show a => Show (Tree a) where
  -- note the indenting of showsPrec ~ the instance!
  -- showsPrec :: Int ->   a ->  ShowS
  --              priority thing ...
  showsPrec d EmptyTree    rest = "{}" ++ rest
  showsPrec d (Fork e l r) rest =
     "{"++ showsPrec d e
        (showsPrec d l
           (showsPrec d r ("}"++rest)
        )  )
