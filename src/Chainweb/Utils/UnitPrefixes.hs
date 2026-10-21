-- | SI and binary unit prefixes.
module Chainweb.Utils.UnitPrefixes
( deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto
, deka, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta
, kibi, mebi, gibi, tebi, pebi, exbi
) where

-- -------------------------------------------------------------------------- --
-- SI unit prefixes

-- | cf. https://www.nist.gov/pml/owm/metric-si-prefixes
--
deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto :: Fractional a => a
deci = 10 ^^ (-1 :: Int)
centi = 10 ^^ (-2 :: Int)
milli = 10 ^^ (-3 :: Int)
micro = 10 ^^ (-6 :: Int)
nano = 10 ^^ (-9 :: Int)
pico = 10 ^^ (-12 :: Int)
femto = 10 ^^ (-15 :: Int)
atto = 10 ^^ (-18 :: Int)
zepto = 10 ^^ (-21 :: Int)
yocto = 10 ^^ (-24 :: Int)

-- | cf. https://www.nist.gov/pml/owm/metric-si-prefixes
--
deka, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta :: Num a => a
deka = 10 ^ (1 :: Int)
hecto = 10 ^ (2 :: Int)
kilo = 10 ^ (3 :: Int)
mega = 10 ^ (6 :: Int)
giga = 10 ^ (9 :: Int)
tera = 10 ^ (12 :: Int)
peta = 10 ^ (15 :: Int)
exa = 10 ^ (18 :: Int)
zetta = 10 ^ (21 :: Int)
yotta = 10 ^ (24 :: Int)

-- | IEC 60027-X unit prefixes for binary bases.
--
-- cf. https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-d-bibliography#05
--
kibi, mebi, gibi, tebi, pebi, exbi:: Num a => a
kibi = 1024 ^ (1 :: Int)
mebi = 1024 ^ (2 :: Int)
gibi = 1024 ^ (3 :: Int)
tebi = 1024 ^ (4 :: Int)
pebi = 1024 ^ (5 :: Int)
exbi = 1024 ^ (6 :: Int)
