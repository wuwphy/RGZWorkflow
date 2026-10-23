(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
e=Total[Table[(x+i)/(y+i+I Sqrt[z]),{i,1,90}]];
r=RGZWorkflow`Private`qqCoefficientRational[e];
record["ExactRationalEquivalence",TimeConstrained[Cancel[Together[r-e]]===0,30,False]];
record["NoPlaceholderLeak",FreeQ[r,s_Symbol/;StringStartsQ[SymbolName[Unevaluated[s]],"coefficientRoot"]]];
record["SquareRootRetained",!FreeQ[r,Sqrt[z]]];
record["ExactZero",RGZWorkflow`Private`qqCoefficientRational[e-e]===0];
record["SmallExpressionMatchesNative",RGZWorkflow`Private`qqCoefficientRational[1/(x+Sqrt[z])+1/(x-Sqrt[z])]===Cancel[Together[1/(x+Sqrt[z])+1/(x-Sqrt[z])]]];
record["ComplexBranchPreserved",Block[{$MaxExtraPrecision=2000},Chop[N[(r-e)/.{x->3/2,y->7/3,z->-5},40],10^-30]===0]];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
