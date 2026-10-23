(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringBoxProjector.wl"}]];
(* Massless qq' kinematics only. Every rational coordinate rewrite is checked
   against the original Dirac-simplified expression before returning it. *)
qqBoxWardPrecondition[input_,name_] := Module[{q=RGZWorkflow`Engine`q,k1=RGZWorkflow`Engine`qqK1,
 k2=RGZWorkflow`Engine`qqK2,k3=RGZWorkflow`Engine`qqK3,k4=RGZWorkflow`Engine`qqK4,
 s=RGZWorkflow`Engine`qqS,t=RGZWorkflow`Engine`qqT,spinors,shield,conserved,simple,explicit,
 w1,w2,w3,w4,rules,back,collected,result,reconstructed,difference,projector,checks},
 If[!MemberQ[{"Box1","Box2"},name],Return[input]];
 spinors=DeleteDuplicates[Cases[input,_FeynCalc`Spinor,Infinity]];
 If[Length[spinors]=!=4||!AllTrue[spinors,#[[2]]===0&],Return[input]];
 qqProgressOperation["Box: enforce physical momentum conservation"];
 shield=Table[Unique["qqExternalSpinor"],{Length[spinors]}];
 conserved=FeynCalc`MomentumExpand[((input/.Thread[spinors->shield])/.k4->k1+k2-k3)]/.Thread[shield->spinors];
 qqProgressOperation["Box: verify D-dimensional quark Ward identities"];
 If[!TrueQ[$qqBoxWardLinesVerified],checks=qqBoxWardLineChecks[];$qqBoxWardLinesVerified=And@@checks];
 If[TrueQ[$qqBoxWardLinesVerified],
 qqProgressOperation["Box: pinch projector terms before TID"];
 projector=qqBoxProjectorWard[conserved,name];
 If[projector=!=$Failed,conserved=projector;qqProgressUpdate[<|"BoxProjectorWardApplied"->True|>],qqProgressUpdate[<|"BoxProjectorWardApplied"->False|>]]];
 qqProgressOperation["Box: massless external Dirac equations"];
 simple=FeynCalc`DiracSimplify[conserved,FeynCalc`DiracEquation->True,FeynCalc`SirlinSimplify->False];
 explicit=FeynCalc`FCE[FeynCalc`ExpandScalarProduct[FeynCalc`FeynAmpDenominatorExplicit[simple]]]/.
 RGZWorkflow`Engine`rgzQQInvariantRules;
 qqProgressOperation["Box: cancel rational Ward coefficients"];
 rules=Thread[(FeynCalc`FCE /@ {FeynCalc`SPD[q,q],FeynCalc`SPD[q,k1],FeynCalc`SPD[q,k2],FeynCalc`SPD[q,k3],FeynCalc`SPD[q,k4]}) ->
 If[name==="Box1",{w1,(w1-w2)/2,(w3-w1)/2,(w4-w2-t)/2,(w3-w4+t)/2},
 {w1,(w1-w2)/2,(w3-w4+s+t)/2,(w1-w4)/2,(w3-w2+s+t)/2}]];
 collected=FeynCalc`Collect2[explicit/.rules,{Dot},FeynCalc`Factoring->(Factor[Cancel[Together[#]]]& )];
 back=If[name==="Box1",{w1->FeynCalc`SPD[q,q],w2->FeynCalc`SPD[q-k1,q-k1],
 w3->FeynCalc`SPD[q+k2,q+k2],w4->FeynCalc`SPD[q-k1+k3,q-k1+k3]},
 {w1->FeynCalc`SPD[q,q],w2->FeynCalc`SPD[q-k1,q-k1],
 w3->FeynCalc`SPD[q+k2-k3,q+k2-k3],w4->FeynCalc`SPD[q-k3,q-k3]}];
 result=FeynCalc`FCI[collected/.back];
 qqProgressOperation["Box: verify exact coordinate reconstruction"];
 reconstructed=FeynCalc`FCE[FeynCalc`ExpandScalarProduct[result]]/.RGZWorkflow`Engine`rgzQQInvariantRules;
 difference=FeynCalc`Collect2[reconstructed-explicit,{Dot},FeynCalc`Factoring->Cancel];
 If[difference=!=0||!FreeQ[result,w1|w2|w3|w4],Return[Failure["QQBoxWardReconstruction",<|"Part"->name|>]]];
 qqProgressUpdate[<|"WardCoordinateVerified"->True,"WardCoefficientLeaves"->LeafCount[collected]|>];result];
