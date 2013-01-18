(* ::Package:: *)

BeginPackage["brosutilities`"]

parsToArgs::usage=
"Transforms the argument in the form \"1,2,3,4,5\" to \"1_,2_,3_,4_,5_\".
  Usefull for turning Lists into function arguments. Example:
  modParms=Sequence[t,R,a,b,l,\[Omega]];
  arguments=parsToArgs[modParms];
  "


Begin["`Private`"]


parsToArgs[para__]:=ToExpression@Map[(ToString@#<>"_")&,{para}]/.List->Sequence


End[ ]

EndPackage[ ];
