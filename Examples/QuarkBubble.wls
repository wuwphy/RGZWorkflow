(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* A single field pair and sector, in a fresh FeynCalc kernel.
   Read Documentation/QuarkBubbleWalkthrough.md alongside this script.
   All generated data go to a newly created temporary work directory. *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,project=DirectoryName[DirectoryName[$InputFileName]]},
 Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
 RGZWorkflow`RGZInitialize[CreateDirectory[],"Display"->"Quiet"];
 If[!AssociationQ[RGZWorkflow`RGZLoadFeynCalc[]],Exit[1]];
 RGZWorkflow`Private`loadLoopEngine[];
 (* Parse the calculation using the package's canonical physical symbols. *)
 RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[1]];

require[r_] := If[FailureQ[r],Print[r[[1]]];Exit[1],r];
(* 1. Generate only the two dynamical-quark bubbles for the A-A pair. *)
start=require[RGZMXStart["FieldPair"->{1,1},"Category"->{"QuarkBubble"},"reuseAACache"->False]];
count=Length[rgzMXJob["PerDiagram"]];
If[count=!=2,Print["Expected one graph for each of the two quark flavours."];Exit[1]];

(* 2. Convert the original FA amplitudes, then project the still-unintegrated tensor. *)
Do[require[RGZMXRun[i,"Convert",60]];require[RGZMXRun[i,"Project",60]],{i,count}];
(* Compare each generated graph with the independently written one-flavour
   fermion-loop tensor: the minus sign is the closed-loop sign and 1/2 is TF. *)
handOneFlavour=FCI[-SMP["g_s"]^2/2 SUNDelta[SUNIndex[rgzGluon1],SUNIndex[rgzGluon2]]
 DiracTrace[GAD[\[Mu]].GSD[q].GAD[\[Nu]].GSD[q-p],DiracTraceEvaluate->True] FAD[q,q-p]];
handProjection=require[RGZMXProject[handOneFlavour,1,1]];
unintegratedTensorAgreement=And@@Table[
 difference=RGZMXColourReduce[Expand[FCI[handOneFlavour-rgzMXJob["PerDiagram"][[i]]["Amplitude"]]]];
 difference=FeynAmpDenominatorExplicit[difference];
 Cancel[Together[ExpandScalarProduct[difference]]]===0,{i,count}];
projectedIntegrandAgreement=And@@Flatten@Table[
 difference=FCI[handProjection[component]-rgzMXJob["PerDiagram"][[i]]["Project"][component]];
 difference=FeynAmpDenominatorExplicit[difference];
 Cancel[Together[ExpandScalarProduct[difference]]]===0,{i,count},{component,{"T","L"}}];
If[!unintegratedTensorAgreement||!projectedIntegrandAgreement,
 Print["The generated graph did not match the independent fermion-loop tensor."];Exit[1]];

(* 3. Prepare propagators. The numerator and D-dimensional projectors are retained. *)
Do[require[RGZMXRun[i,"DecomposeT",60]];require[RGZMXRun[i,"DecomposeL",60]],{i,count}];
projected=rgzMXJob["PerDiagram"];

(* 4. Apply both routes to exactly the same projected/split inputs. *)
reduceRoute[enabled_,label_] := Module[{dir,integral},
 RGZMXSetAcceleration["Enabled"->enabled];
 dir=FileNameJoin[{rgzMXJob["CacheDirectory"],label}];
 Table[Association@Table[component->(
   integral=require[RGZMXIntegrate[projected[[i]]["Decompose"<>component],dir,60]];
   require[RGZMXScalarReduce[integral,dir,60]]),{component,{"T","L"}}],{i,count}]];
standard=reduceRoute[False,"example-standard"];
accelerated=reduceRoute[True,"example-accelerated"];
routeEquality=And@@Flatten@Table[
 Cancel[Together[FCI[standard[[i]][component]-accelerated[[i]][component]]]]===0,
 {i,count},{component,{"T","L"}}];
If[!routeEquality,Print["The two reductions did not establish the same result."];Print[InputForm[{standard,accelerated}]];Exit[1]];

(* 5. Complete the ordinary graph records and sum this sector once. *)
Do[Do[require[RGZMXRun[i,stage,60]],{stage,{"IntegrateT","ScalarT","IntegrateL","ScalarL"}}],{i,count}];
sector=require[RGZMXTotal[]];
(* Remove only the external adjoint delta for the normalized matrix entry. *)
entry=require[RGZMixProjectPair[sector,{1,1},60]];
entry=Map[Cancel[Together[FCI[#]]]&,entry];
expectedT=-2 I Pi^2 SMP["g_s"]^2 (D-2)/(D-1) SPD[p,p] B0[SPD[p,p],0,0];
traceFormulaAgreement=Cancel[Together[FCI[entry["T"]-expectedT]]]===0;
If[!traceFormulaAgreement,Print["The quark-bubble trace formula was not reproduced."];Exit[1]];
longitudinalZero=TrueQ[entry["L"]===0];
If[!longitudinalZero,Print["The integrated longitudinal coefficient did not vanish."];Exit[1]];

(* 6. Restore the stored loop-measure convention exactly once. *)
physicalEntry=Map[#/(2 Pi)^4&,entry];
dAA=-I (SPD[p,p]-\[Mu]^2)/RGZLoopDelta[SPD[p,p]];
attached=dAA^2 physicalEntry["T"];

(* Reopen the actual graph cache; completed stages must be skipped. *)
require[RGZMXResume[rgzMXJob["CacheDirectory"]]];
warm=RGZMXRun[1,"ScalarT",60];
cacheReused=AssociationQ[warm]&&warm["Status"]==="Complete; skipped";
packet=<|"Process"->"A-A, QuarkBubble only","GraphCount"->count,
 "QuarkFlavours"->Lookup[projected,"FClass"],"HandOneFlavourTensor"->handOneFlavour,
 "UnintegratedTensorAgreement"->unintegratedTensorAgreement,"ProjectedIntegrandAgreement"->projectedIntegrandAgreement,"PerGraphBeforeIntegration"->projected,
 "StandardScalarResults"->standard,"AcceleratedScalarResults"->accelerated,
 "TraceFormulaAgreement"->traceFormulaAgreement,"RouteEquality"->routeEquality,"LongitudinalZero"->longitudinalZero,"RawSector"->sector,
 "MatrixEntryRawTID"->entry,"MatrixEntryWithMeasure"->physicalEntry,
 "AAInsertionBetweenTreeLines"->attached,"CacheReused"->cacheReused|>;
file=FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"quark-bubble-example.wxf"}];
require[RGZWorkflow`Private`atomicWXF[file,packet]];
Print[KeyTake[packet,{"GraphCount","QuarkFlavours","UnintegratedTensorAgreement","ProjectedIntegrandAgreement","TraceFormulaAgreement","RouteEquality","LongitudinalZero","CacheReused"}]];
Print["Raw matrix entry: ",ToString[entry,InputForm]];
Print["Matrix entry with loop measure: ",ToString[physicalEntry,InputForm]];
Print["Full intermediate expressions: ",file];
If[StringQ[Environment["RGZ_EXAMPLE_RECORD"]],Export[Environment["RGZ_EXAMPLE_RECORD"],KeyDrop[packet,{"PerGraphBeforeIntegration"}],"WXF"]];
Exit[If[cacheReused,0,1]];
