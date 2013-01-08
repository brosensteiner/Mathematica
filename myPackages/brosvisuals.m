(* ::Package:: *)

BeginPackage["brosvisuals`"]

mySimpleTreeFormRenderer::usage=
"mySimpleTreeFormRenderer[expr, highlightNode, highlightColor, restColor, options]
  gives a TreeForm output for simple visualisation of mathematica expressions. Colouring of one or more nodes
  (given as argument highlightNode) is supported."

myImageGridAlign::usage=
"myImageGridAlign[c_, img_, pos_:1, opts:OptionsPattern[]
  simply aligns a given graphic (img) on an 2D array (c)
  Argument pos is the number in the array where the graphic will be placed."

Begin["`Private`"]


mySimpleTreeFormRenderer[expr_,highlightNode_,highlightColor_:LightRed,restColor_:LightYellow,opts:OptionsPattern[]]:=
TreeForm[expr,
VertexRenderingFunction->( Module[{comp=StringCases[ToString@highlightNode,ToString@#2]},
{
If[ToString@#2=== If[Length[comp]!=0,comp[[1]]],highlightColor,restColor],
EdgeForm[Black],Disk[#,Scaled[.05]],
Black,Text[#2,#1]
}
]& ),opts]


myImageGridAlign[c_,img_,pos_:1,opts:OptionsPattern[]]:=Graphics[{
Table[If[c[[j,i]]===pos,
Inset[img,{i,-j}ImageDimensions[img],{Left,Top},ImageDimensions[img]],
{}],
{i,Dimensions[c][[2]]},{j,Dimensions[c][[1]]}, opts]
}]


End[ ]

EndPackage[ ];
