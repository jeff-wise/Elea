

module Elea.Lang.Term.Synthesis where




---------------------------------------------------------------------
-- 1 Synthesis
---------------------------------------------------------------------


-- 1.1 Aux Types
---------------------------------------------------------------------

type Param = Int
type Root = Int

-- Adjacency List representation, since functional
-- representations look difficult...
-- Vertices are just arbitrary integer identifiers
type SynthesisGraph = HMS.HashMap Int Application



-- 1.2 Synthesis
---------------------------------------------------------------------

-- | Synthesis is an 'Application' graph
data Synthesis = Synthesis Root SynthesisGraph



-- 1.3 Lambda Terms
---------------------------------------------------------------------
-- $lambda_terms
-- The naming scheme for the 'Synthesis' components is similar to the
-- lambda calculus components in order to emphasize the combinatorial
-- nature of synthesis.


-- | Represents an application of some defined function
-- to a list of parameters, which point to other applications
-- in the synthesis graph
data Application = Application Abstraction [Param]



-- | Abstraction
-- An abstraction is a pure function, of a specific purpose.
-- Abstractions computer certain types in ways specific to
-- that type, to enforce using readable and efficient data models
-- as well as modular, composable computations.
 


---------------------------------------------------------------------
-- 1.3 Utility Function
---------------------------------------------------------------------

synthesis ∷ [(Int, Application)] → Synthesis
synthesis apps = Synthesis . HMS.fromList



valConst value = Synthesis $
  HMS.fromList [ (1, Application (Abs_Value value) []) ]


tyConst ty = Synthesis $
  HMS.fromList [ (1, Application (Abs_Type ty) []) ]


lensConst lens = Synthesis $
  HMS.fromList [ Application (Abs_Lens lens) [] ]






---------------------------------------------------------------------
-- 6.0 Random
---------------------------------------------------------------------

data Random =
    RandomNumber Type
  | MapRandom Param Abstraction


