(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
checks=<||>;
record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",checks[n]]);
record["BareModelNIsSUNN",RGZPTColour[System`N+CA]===2 SUNN];
record["AuxiliaryColourNormalization",Simplify[RGZPTColour[Sqrt[System`N] Sqrt[SUNN]]/.SUNN->3]===3];
record["HeldNumericalHeadPreserved",RGZPTColour[HoldForm[System`N[x]]]===HoldForm[System`N[x]]];
record["NumericalEvaluationPreserved",RGZPTColour[N[1/3]]===N[1/3]];
record["CommonSelfUsesSameDimension",RGZPTCommonSelf[<|"\:6811\:7ea7\:6a2a\:5411AA"->I,"\:81ea\:80fdT\:5206\:9879"->{System`N}|>] === {SUNN}];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];
Exit[If[And@@Values[checks],0,1]];
