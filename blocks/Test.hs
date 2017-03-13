module Test where

import Main

--------------------------------------------------------------------------------
                        -- Test data --

blok1 = -- Blok (-1, 1) 2 1 100
  Blok {
      x = -1
    , y = 1
    , breedte = 2
    , hoogte  = 1
    , diepte  = 100
  }

blok2 = -- Blok (1, 1) 2 1 100
  Blok {
      x = 1
    , y = 1
    , breedte = 2
    , hoogte  = 1
    , diepte  = 100
  }

blok3 = -- Blok (1, 1) 4 1 100
  Blok {
      x = 1
    , y = 1
    , breedte = 4
    , hoogte  = 1
    , diepte  = 100
  }

veld1 = Veld [[X, X, X], [OK, DOEL, OK]]

blok4 = -- Blok (3, 4) 2 10 3
  Blok {
      x = 3
    , y = 4
    , breedte = 2
    , hoogte  = 10
    , diepte  = 3
  }
blok5 = -- Blok (3, 4) 2 10 1
  Blok {
      x = 3
    , y = 4
    , breedte = 2
    , hoogte  = 10
    , diepte  = 1
  }
blok6 = -- Blok (3, 2) 2 2 2
  Blok {
      x = 3
    , y = 2
    , breedte = 2
    , hoogte  = 2
    , diepte  = 2
  }

blok7 = -- Blok (1, 1) 2 2 2
  Blok {
      x = 1
    , y = 1
    , breedte = 2
    , hoogte  = 2
    , diepte  = 2
  }

veld2 = Veld
  [
     [X, X, X, X, X, X]
  ,  [X, X, X, X, X, X]
  ,  [X, X, X, X, X, X]
  ,  [OK, OK, OK, OK, OK]
  ,  [OK, OK, OK, OK, OK]
  ]