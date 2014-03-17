


module Elea.Lang.Term.Fun.Query where




data Query = Query
  { _qryFrom    ∷ Location
  , _qryWhere   ∷ Type
  , _qrySelect  ∷ Lens
  }



