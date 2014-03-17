


module Elea.Lang.Term.Function where


data Function =
    Fun_Template  Template
  | Fun_Equation  Equation
  | Fun_TextProc  TextProcessor
  | Fun_Query     Query
  | Fun_Const     Value




