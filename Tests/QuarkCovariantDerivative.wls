(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Run in a clean FeynRules kernel, separately from FeynCalc. *)
project=DirectoryName[DirectoryName[$InputFileName]];
frDirectory=Environment["RGZ_FEYNRULES_DIRECTORY"];
fr=If[StringQ[frDirectory]&&StringLength[frDirectory]>0,FileNameJoin[{frDirectory,"FeynRules.m"}],FileNameJoin[{$UserBaseDirectory,"Applications","FeynRules","FeynRules.m"}]];
If[!FileExistsQ[fr],Print["FeynRules installation required for the independent derivative test."];Exit[1]];
$FeynRulesPath=DirectoryName[fr];
Global`FR$Parallel=False;
Get[fr];
constructors=Names["FeynRules`*ContstructNonAbelianFieldComponentDC"]~Join~Names["FeynRules`Private`*ContstructNonAbelianFieldComponentDC"];
If[constructors==={},Print["Covariant derivative constructor not found."];Exit[1]];
constructor=ToExpression[First[constructors]];
term=constructor[group,psi[idx],False,{colour},{gen,colour},gs,boson,lor];
scalar=term/.{boson[__]->1,gen[__]->1,psi[__]->1};
checks=<|"InstalledFRDSign"->TrueQ[FeynRules`FR$DSign===1],
 "FundamentalDCMinusIg"->TrueQ[scalar===-I gs],
 "QuarkKineticInteractionPlusG"->TrueQ[I scalar===gs],
 "VertexFromIPrefactorPlusIg"->TrueQ[I (I scalar)===I gs]|>;
source=Import[FileNameJoin[{project,"Kernel","Backend","RGZ_Quark_Amplitude.wl"}],"Text"];
AssociateTo[checks,"BothProbeFlavoursMatchDC"->(StringCount[source,"==I Global`FAGS"]===2)];
Print["DC interaction: ",scalar,"; L interaction: ",I scalar,"; vertex: ",I (I scalar)];
Print[checks];Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
