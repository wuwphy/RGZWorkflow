(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Independent FP and auxiliary-ghost loop checks; temporary work only.
   See Documentation/GhostAndMixedExamples.md. *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,project=DirectoryName[DirectoryName[$InputFileName]]},
 Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];RGZWorkflow`RGZInitialize[CreateDirectory[],"Display"->"Quiet"];
 If[!AssociationQ[RGZWorkflow`RGZLoadFeynCalc[]],Exit[1]];RGZWorkflow`Private`loadLoopEngine[];
 RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[1]];
require[r_] := If[FailureQ[r],Print[r];Exit[1],r];
require[RGZMXStart["FieldPair"->{1,1},"Category"->{"GhostThreePointLoop"},"reuseAACache"->False]];
rows=rgzMXJob["PerDiagram"];If[Sort[Lookup[rows,"UCategory"]]=!={1,2,3,4},Print["Unexpected ghost inventory."];Exit[1]];
checks={};results={};s=SPD[p,p];
Do[
 class=rows[[i]]["UCategory"];
 (* The bundled Gribov spectator index has eight values. A vector ghost
    also has a Lorentz trace D. These are separate factors, not CA. *)
 {mass,multiplicity}=If[MemberQ[{1,4},class],{0,1},{\[Mu],8 D}];
 hand=FCI[-multiplicity SMP["g_s"]^2 SUNN SUNDelta[SUNIndex[rgzGluon1],SUNIndex[rgzGluon2]]
   FVD[q-p,\[Mu]] FVD[q,\[Nu]] FAD[{q,mass},{q-p,mass}]];
 require[RGZMXRun[i,"Convert",60]];require[RGZMXRun[i,"Project",60]];
 actual=rgzMXJob["PerDiagram"][[i]];
 difference=Contract[RGZMXColourReduce[Expand[FCI[actual["Amplitude"]-hand]]]];
 tensorEqual=Cancel[Together[ExpandScalarProduct[FeynAmpDenominatorExplicit[difference]]]]===0;
 hp=require[RGZMXProject[hand,1,1]];
 projectionEqual=And@@Table[Cancel[Together[ExpandScalarProduct[FeynAmpDenominatorExplicit[FCI[actual["Project"][c]-hp[c]]]]]]===0,{c,{"T","L"}}];
 Do[require[RGZMXRun[i,stage,120]],{stage,{"DecomposeT","IntegrateT","ScalarT","DecomposeL","IntegrateL","ScalarL"}}];
 actual=rgzMXJob["PerDiagram"][[i]];
 entry=require[RGZMixProjectPair[<|"T"->actual["ScalarT"],"L"->actual["ScalarL"]|>,{1,1},60]];
 (* Tensor-bubble identities derived from 2 p.q=d0-d1+s. *)
 a=mass^2;
 expected=<|"T"->-multiplicity SMP["g_s"]^2 SUNN I Pi^2/(D-1) (A0[a]/2+(a-s/4) B0[s,a,a]),
 "L"->-multiplicity SMP["g_s"]^2 SUNN I Pi^2 (A0[a]/2-s B0[s,a,a]/4)|>;
 integralEqual=And@@Table[Cancel[Together[FCI[entry[c]-expected[c]]]]===0,{c,{"T","L"}}];
 AppendTo[checks,<|"UClass"->class,"HandTensor"->tensorEqual,"HandProjection"->projectionEqual,"HandIntegral"->integralEqual|>];
 AppendTo[results,<|"UClass"->class,"HandIntegrand"->hand,"RawGenerated"->actual["Amplitude"],"MatrixEntry"->entry,"Reference"->expected|>],{i,Length[rows]}];
Print[checks];
If[!AllTrue[checks,TrueQ[#["HandTensor"]]&&TrueQ[#["HandProjection"]]&&TrueQ[#["HandIntegral"]]&],Exit[1]];
require[RGZMXResume[rgzMXJob["CacheDirectory"]]];warm=RGZMXRun[1,"ScalarT",60];
If[warm["Status"]=!="Complete; skipped",Exit[1]];
file=FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"ghost-bubble-example.wxf"}];
require[RGZWorkflow`Private`atomicWXF[file,<|"Checks"->checks,"Results"->results,"SpectatorDimension"->8,"CacheReused"->True|>]];
Print["Results: ",file];Exit[0];
