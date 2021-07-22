module LatexConstants
  ( mapsTo,
    ksi,
    rho,
    upPhi,
    phi,
    lambda,
    lambdaS,
    llbracket,
    rrbracket,
    quad
  )
where

mapsTo :: [Char]
mapsTo = " \\mapsto "

ksi :: [Char]
ksi = " \\ksi "

rho :: [Char]
rho = " \\rho "

upPhi :: [Char]
upPhi = " \\upPhi "

phi :: [Char]
phi = " \\varphi "

lambda :: [Char]
lambda = " \\lambda "

lambdaS :: [Char]
lambdaS = lambda ++ " s. "

llbracket :: [Char]
llbracket = " \\llbracket "

rrbracket :: [Char]
rrbracket = " \\rrbracket "

quad :: [Char]
quad = " \\qquad "