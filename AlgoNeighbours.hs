module AlgoNeighbours where

import MyTypes (Error (Error))

trivialErrors :: [Error]
trivialErrors = [Error 0 1 1 1]

floydSteinbergErrors :: [Error]
floydSteinbergErrors =
  [ Error 0 1 7 16,
    Error 1 (negate 1) 3 16,
    Error 1 0 5 16,
    Error 1 1 1 16
  ]

jarvisJudiceNinkeErrors :: [Error]
jarvisJudiceNinkeErrors =
  [ Error 0 1 7 48,
    Error 0 2 5 48,
    Error 1 (negate 2) 3 48,
    Error 1 (negate 1) 5 48,
    Error 1 0 7 48,
    Error 1 1 5 48,
    Error 1 2 3 48,
    Error 2 (negate 2) 1 48,
    Error 2 (negate 1) 3 48,
    Error 2 0 5 48,
    Error 2 1 3 48,
    Error 2 2 1 48
  ]

stuckiErrors :: [Error]
stuckiErrors =
  [ Error 0 1 8 42,
    Error 0 2 4 42,
    Error 1 (negate 2) 2 42,
    Error 1 (negate 1) 4 42,
    Error 1 0 8 42,
    Error 1 1 4 42,
    Error 1 2 2 42,
    Error 2 (negate 2) 1 42,
    Error 2 (negate 1) 2 42,
    Error 2 0 4 42,
    Error 2 1 2 42,
    Error 2 2 1 42
  ]

atkinsonErrors :: [Error]
atkinsonErrors =
  [ Error 0 1 1 8,
    Error 0 2 1 8,
    Error 1 (negate 1) 1 8,
    Error 1 0 1 8,
    Error 1 1 1 8,
    Error 2 0 1 8
  ]

burkesErrors :: [Error]
burkesErrors =
  [ Error 0 1 8 32,
    Error 0 2 4 32,
    Error 1 (negate 2) 2 32,
    Error 1 (negate 1) 4 32,
    Error 1 0 8 32,
    Error 1 1 4 32,
    Error 1 2 2 32
  ]

sierraErrors :: [Error]
sierraErrors =
  [ Error 0 1 5 32,
    Error 0 2 3 32,
    Error 1 (negate 2) 2 32,
    Error 1 (negate 1) 4 32,
    Error 1 0 5 32,
    Error 1 1 4 32,
    Error 1 2 2 32,
    Error 2 (negate 1) 2 32,
    Error 2 0 3 32,
    Error 2 1 2 32
  ]

sierraTwoRowErrors :: [Error]
sierraTwoRowErrors =
  [ Error 0 1 4 16,
    Error 0 2 3 16,
    Error 1 (negate 2) 1 16,
    Error 1 (negate 1) 2 16,
    Error 1 0 3 16,
    Error 1 1 2 16,
    Error 1 2 1 16
  ]

sierraLiteErrors :: [Error]
sierraLiteErrors =
  [ Error 0 1 2 4,
    Error 1 (negate 1) 1 4,
    Error 1 0 1 4
  ]