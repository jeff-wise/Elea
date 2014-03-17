


module Elea.Lang.Impl.Field where


import Elea.Prelude



-- | Thread Context
-- Parameters:
--  * Current system of evaluation
--  * Value of cause which triggered this force
data FieldContext = FC System Val


data FieldError = FieldError



-- | A field represents the evaluation of a 'Force'
data Field s a =
    F_Error FieldError
  | F_Step (s → FieldContext → (STM a, s))



instance Monad Field where
  
  return x          = F_Step (\_ _ → return x)
  
  F_Error err >>= _ = F_Error err
  F_Step  g   >>= h = F_Step (\s fc → 
                        let (stm_a, s') = g s fc
                            actStep = stm_a >>= h
                        in  actStep s' fc
                      )
      

fieldCtx ∷ Action ActionContext
fieldCtx = F_Step $ (\s fc → (return fc, s))






