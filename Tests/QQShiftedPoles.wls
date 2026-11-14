(* SPDX-License-Identifier: GPL-3.0-or-later *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,root=DirectoryName[DirectoryName[$InputFileName]]},
 Get[FileNameJoin[{root,"RGZWorkflow.wl"}]];RGZWorkflow`RGZInitialize[CreateDirectory[]];RGZWorkflow`RGZLoadFeynCalc[];RGZWorkflow`RGZTreeBenchmark[60];RGZWorkflow`Private`loadQuarkEngine[];
 RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[1]];
checks={};
Do[
 den=ExpandScalarProduct[FCI[(SPD[route,route]-m^2)(SPD[route,route]-\[Mu]^2)+mG^4]];
 raw=Expand[den]^(-power);
 restored=RGZWorkflow`Private`qqRestoreAffineSquares[raw];
 split=rgzQQSplit[restored,0,0];
 valid=!FailureQ[split]&&FreeQ[split,_Failure|$Aborted|$TimedOut];
 If[valid,diff=Cancel[Together[ExpandScalarProduct[FeynAmpDenominatorExplicit[rgzQQRestorePoleMasses[split,0,0]]]-raw]];valid=TrueQ[diff===0]];
 AppendTo[checks,valid];Print[{route,power,"Reconstruction"->valid}],
 {route,{q,q+qqK1,q-qqK1,q+qqK3-qqK1,q-qqK3+qqK1}},{power,{1,2}}];
(* A denominator depending independently on two loop invariants must not be
   turned into a single routed propagator. *)
raw=FCI[1/(SPD[q,q]+SPD[q,qqK1]^2+1)];
z=RGZWorkflow`Private`qqRestoreAffineSquares[raw];
AppendTo[checks,TrueQ[Cancel[Together[ExpandScalarProduct[z]-raw]]===0]];
Exit[If[And@@checks,0,1]];
