module MyTypes where

data Pixel = Pixel
  { r :: Int,
    g :: Int,
    b :: Int
  }
  deriving (Show, Eq)

data Image
  = NullImage
  | Image
      { rows :: Int,
        cols :: Int,
        pixels :: [[Pixel]]
      }
  deriving (Show, Eq)

data Error = Error
  { offsetx :: Int,
    offsety :: Int,
    weightnum :: Int,
    weightdenom :: Int
  }