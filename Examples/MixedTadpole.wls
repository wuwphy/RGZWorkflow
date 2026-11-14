(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* A genuine non-diagonal A-phi xi tadpole, plus a separate repeated-pole
   integral identity. Temporary work only; no complete-matrix claim. *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,project=DirectoryName[DirectoryName[$InputFileName]]},
 Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];RGZWorkflow`RGZInitialize[CreateDirectory[],"Display"->"Quiet"];
 If[!AssociationQ[RGZWorkflow`RGZLoadFeynCalc[]],Exit[1]];RGZWorkflow`Private`loadLoopEngine[];
 RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[1]];
require[r_] := If[FailureQ[r],Print[r];Exit[1],r];
require[RGZMXStart["FieldPair"->{1,5},"Category"->{"FourPointVertexLoop"},"reuseAACache"->False]];
If[Length[rgzMXJob["PerDiagram"]]=!=1,Print["Unexpected A-phi tadpole inventory."];Exit[1]];
require[RGZMXRun[1,"Convert",60]];require[RGZMXRun[1,"Project",60]];
row=rgzMXJob["PerDiagram"][[1]];
(* 1/2 for the identical xi legs, two equal color terms in the quartic
   vertex, D_xixi=-i alpha/(q^4+alpha M^4), and f f f=-CA f.
   Keep both external phi color indices open at this step. *)
hand=FCI[-SMP["g_s"]^2 mG^2 \[Alpha] SUNN/(4 Sqrt[SUNN])
 SUNF[SUNIndex[rgzGluon1],SUNIndex[rgzGluon2],SUNIndex[rgzGribov2]] MTD[\[Mu],\[Nu]]/(SPD[q,q]^2+\[Alpha] M^4)];
difference=Contract[RGZLoopColour[RGZMXColourReduce[Expand[FCI[row["Amplitude"]-hand]]]]];
tensorEqual=Cancel[Together[ExpandScalarProduct[FeynAmpDenominatorExplicit[difference]]]]===0;
handProjection=require[RGZMXProject[hand,1,5]];
projectionEqual=And@@Table[Cancel[Together[ExpandScalarProduct[RGZLoopColour[row["Project"][c]-handProjection[c]]]]]===0,{c,{"T","L"}}];
Do[require[RGZMXRun[1,stage,120]],{stage,{"DecomposeT","IntegrateT","ScalarT","DecomposeL","IntegrateL","ScalarL"}}];
sector=require[RGZMXTotal[]];entry=require[RGZMixProjectPair[sector,{1,5},60]];
entry=Map[RGZLoopColour[RGZLoopPoleMasses[#]]&,entry];
pole1=-I Sqrt[\[Alpha] M^4];pole2=I Sqrt[\[Alpha] M^4];
expected=-SMP["g_s"]^2 mG^2 \[Alpha] SUNN/4 I Pi^2 (A0[pole1]-A0[pole2])/(pole1-pole2);
integralEqual=And@@Table[Cancel[Together[FCI[entry[c]-expected]]]===0,{c,{"T","L"}}];
(* Independent unit check, not an additional generated tadpole diagram:
   repeated transverse RGZ poles at a nondegenerate complex-mass point. *)
x=Unique["poleSquare"];
repeatSplit=require[RGZMXSplitFactor[RGZLoopDelta[x]^2,x,q]];
explicit=ExpandScalarProduct[FeynAmpDenominatorExplicit[RGZLoopPoleMasses[repeatSplit]]];
reconstruction=Cancel[Together[explicit-1/RGZLoopDelta[FCI[SPD[q,q]]]^2]]===0;
repeatedIntegral=require[RGZMXIntegrate[repeatSplit,None,120]];
point={m->3,\[Mu]->1,mG->Sqrt[5]};
repeatedIntegral=RGZLoopPoleMasses[repeatedIntegral]/.point;
poleA=5+3 I;poleB=5-3 I;gap=poleA-poleB;
repeatedReference=I Pi^2 ((D-2)/(2 gap^2) (A0[poleA]/poleA+A0[poleB]/poleB)-2/gap^3 (A0[poleA]-A0[poleB]));
repeatedEqual=Cancel[Together[repeatedIntegral-repeatedReference]]===0;
checks=<|"HandTensor"->tensorEqual,"HandProjection"->projectionEqual,"HandComplexPoleIntegral"->integralEqual,
 "RepeatedPoleReconstruction"->reconstruction,"RepeatedPoleIntegralAtComplexPoint"->repeatedEqual|>;
Print[checks];If[!And@@Values[checks],Print[ToString[{entry,expected,repeatedIntegral,repeatedReference},InputForm]];Exit[1]];
require[RGZMXResume[rgzMXJob["CacheDirectory"]]];warm=RGZMXRun[1,"ScalarT",60];If[warm["Status"]=!="Complete; skipped",Exit[1]];
file=FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"mixed-tadpole-example.wxf"}];
require[RGZWorkflow`Private`atomicWXF[file,<|"Checks"->checks,"Pair"->{1,5},"Sector"->"FourPointVertexLoop","GraphCount"->1,
 "HandIntegrand"->hand,"RawGenerated"->row["Amplitude"],"RawMatrixEntry"->entry,"Reference"->expected,
 "RepeatedPolePoint"->point,"RepeatedPoleIntegral"->repeatedIntegral,"RepeatedPoleReference"->repeatedReference,"CacheReused"->True|>]];
Print["Results: ",file];Exit[0];
