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

traceView2::usage=
"traceView2[expr_]
  shows each expression as it is evaluated, along with the subevaluations (\"steps\")
  that lead to the result of that evaluation."

traceView4::usage=
"traceView4[expr_]
  traceView4 provides an alternative view that does not exhibit the crawling behaviour
  at the expense of showing much less context for any given evaluation."

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


ClearAll@traceView2
traceView2[expr_] :=
  Module[{steps = {}, stack = {}, pre, post, show, dynamic},
    pre[e_] := (stack = {steps, stack}; steps = {})
  ; post[e_, r_] :=
      ( steps = First@stack ~Join~ {show[e, HoldForm[r], steps]}
      ; stack = stack[[2]]
      )
  ; SetAttributes[post, HoldAllComplete]
  ; show[e_, r_, steps_] :=
      Grid[
        steps /. {
          {} -> {{"Expr  ", Row[{e, " ", Style["inert", {Italic, Small}]}]}}
        , _ -> { {"Expr  ", e}
               , {"Steps", steps /.
                   { {} -> Style["no definitions apply", Italic]
                   , _ :> OpenerView[{Length@steps, dynamic@Column[steps]}]}
                 }
               , {"Result", r}
               }
        }
      , Alignment -> Left
      , Frame -> All
      , Background -> {{LightCyan}, None}
      ]
  ; TraceScan[pre, expr, ___, post]
  ; Deploy @ Pane[steps[[1]] /. dynamic -> Dynamic, ImageSize -> 10000]
  ]
SetAttributes[traceView2, {HoldAllComplete}]


ClearAll@traceView4
traceView4[expr_] :=
  Module[{steps = {}, stack = {}, pre, post},
    pre[e_] := (stack = {steps, stack}; steps = {})
  ; post[e_, r_] :=
      ( steps = First@stack ~Join~ {{e, steps, HoldForm[r]}}
      ; stack = stack[[2]]
      )
  ; SetAttributes[post, HoldAllComplete]
  ; TraceScan[pre, expr, ___, post]
  ; DynamicModule[{focus, show, substep, enter, exit}
    , focus = steps
    ; substep[{e_, {}, _}, _] := {Null, e, Style["inert", {Italic, Small}]}
    ; substep[{e_, _, r_}, p_] :=
        { Button[Style["show", Small], enter[p]]
        , e
        , Style[Row[{"-> ", r}], Small]
        }
    ; enter[{p_}] := PrependTo[focus, focus[[1, 2, p]]]
    ; exit[] := focus = Drop[focus, 1]
    ; show[{e_, s_, r_}] :=
       Column[
         { Grid[
             { {"Expression", Column@Reverse@focus[[All, 1]]}
             , { Column[
                   { "Steps"
                   , focus /.
                       { {_} :> Sequence[]
                       , _ :> Button["Back", exit[], ImageSize -> Automatic]
                       }
                   }
                 ]
               , Grid[MapIndexed[substep, s], Alignment -> Left]
               }
             , {"Result", Column@focus[[All, 3]]}
             }
           , Alignment -> Left, Frame -> All, Background -> {{LightCyan}}
           ]
         }
       ]
    ; Dynamic @ show @ focus[[1]]
    ]
  ]
SetAttributes[traceView4, {HoldAllComplete}]


End[ ]

EndPackage[ ];
