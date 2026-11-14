(* SPDX-License-Identifier: GPL-3.0-or-later *)
If[$Context=!="RGZWorkflow`Engine`",
 With[{rgzSafetyScript=$InputFileName,project=DirectoryName[DirectoryName[$InputFileName]]},
  Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
  If[!AssociationQ[RGZWorkflow`RGZInitialize[CreateDirectory[]]],Exit[1]];
  If[!AssociationQ[RGZWorkflow`RGZLoadFeynCalc[]],Exit[1]];
  If[FailureQ[RGZWorkflow`Private`loadLoopEngine[]],Exit[1]];
  If[FailureQ[RGZWorkflow`Private`engineGet["OneLoop/GluonSelfEnergy.wl"]],Exit[1]];
  RGZWorkflow`Private`engineEvaluation[Get[rgzSafetyScript]]];Exit[1]];

rgzATChecks=<||>;rgzATRec[name_,v_]:=(AssociateTo[rgzATChecks,name->TrueQ[v]];Print[name," ",TrueQ[v]]);FCClearScalarProducts[];
Do[
 rgzATRec["TID"<>ToString[bad,InputForm],FailureQ[RGZLoopTID[bad]]];
 rgzATRec["Split"<>ToString[bad,InputForm],FailureQ[RGZLoopSplit[bad]]];
 rgzATRec["Gauge"<>ToString[bad,InputForm],FailureQ[RGZGaugeDifference[bad,{1,2}]]];
 rgzATRec["Batch"<>ToString[bad,InputForm],FailureQ[RGZReduceAllAA[<|"PerDiagramResults"->{<|"Amplitude"->bad|>}|>]]],
 {bad,{TerminatedEvaluation["IterationLimit"],Indeterminate,ComplexInfinity,$TimedOut,h[$Aborted],h[Failure["Test",<||>]]}}];
Do[rgzATRec["ForeignGauge"<>ToString[foreign,InputForm],FailureQ[RGZGaugeDifference[foreign,{1,2}]]],
 {foreign,{Global`\[Alpha] A0[rgzATMass],Global`alpha,FCGV["alpha"],FCGV["\[Alpha]"]}}];
rgzATRec["AssignedGauge",Block[{\[Alpha]=1},FailureQ[RGZGaugeDifference[1,{1,2}]]]];
rgzATRec["GaugeIndependent",RGZGaugeDifference[A0[rgzATMass],{1,2}]["Difference"]===0];
rgzATRec["GaugeDependent",RGZGaugeDifference[\[Alpha] A0[rgzATMass],{1,2}]["Difference"]===-A0[rgzATMass]];
rgzATRec["MassGaugeDependence",RGZGaugeDifference[rgzLoopM1^2,{1,4}]["Difference"]=!=0];
rgzATRec["GaugeAssumptionsNeutral",Block[{$Assumptions=rgzATX==0},RGZGaugeDifference[\[Alpha] rgzATX,{1,2}]["Difference"]===-rgzATX && $Assumptions===(rgzATX==0)]];
rgzATSplit=Block[{$Assumptions=m==\[Mu]},RGZLoopSplitFactor[RGZLoopDelta[rgzATV],rgzATV,q]];
rgzATBack=FeynAmpDenominatorExplicit[RGZLoopPoleMasses[rgzATSplit]];
rgzATRec["GeneralMassCounterexample",!FailureQ[rgzATSplit]&&FullSimplify[rgzATBack/.{m->2,\[Mu]->1,mG->2,Pair[Momentum[q,D],Momentum[q,D]]->7}]===1/34];
Do[rgzATSplit=RGZLoopSplitFactor[(rgzATV-\[Mu]^2)^rgzATPower,rgzATV,rgzATRoute];
 rgzATBack=ExpandScalarProduct[FeynAmpDenominatorExplicit[rgzATSplit]];
 rgzATExpected=ExpandScalarProduct[FCI[1/(SPD[rgzATRoute,rgzATRoute]-\[Mu]^2)^rgzATPower]];
 rgzATRec["Pole"<>ToString[{rgzATRoute,rgzATPower},InputForm],!FailureQ[rgzATSplit]&&Cancel[Together[rgzATBack-rgzATExpected]]===0],
 {rgzATRoute,{q,q+p,q-p}},{rgzATPower,{1,2,3}}];
rgzATRec["TadpoleNormalization",RGZLoopTID[FAD[{q,\[Mu]}]]===I Pi^2 A0[\[Mu]^2]];
rgzATRec["PolynomialIntegralZero",RGZLoopTID[SPD[q,q]]===0];
rgzATRec["ScalelessPreserved",Block[{$KeepLogDivergentScalelessIntegrals=True},RGZLoopTID[FAD[q,q]]===I Pi^2 B0[0,0,0]]];
rgzATRec["ValidBatch",AssociationQ[RGZReduceAllAA[<|"PerDiagramResults"->{<|"Amplitude"->FAD[{q,\[Mu]}]|>}|>]]];
Internal`InheritedBlock[{RGZLoopPoleMasses},Clear[RGZLoopPoleMasses];RGZLoopPoleMasses[x_]:=x;
 rgzATRec["WrongEmittedLabelsRejected",FailureQ[RGZLoopSplitFactor[RGZLoopDelta[rgzATV],rgzATV,q]]]];
Internal`InheritedBlock[{TID},Unprotect[TID];Clear[TID];TID[___]:=TerminatedEvaluation["IterationLimit"];
 rgzATRec["InvalidNativeTIDRejected",FailureQ[RGZLoopTID[FAD[{q,\[Mu]}]]]]];
Internal`InheritedBlock[{RGZLoopTID},Clear[RGZLoopTID];RGZLoopTID[___]:=h[$TimedOut];
 rgzATRec["InvalidBatchResultRejected",FailureQ[RGZReduceAllAA[<|"PerDiagramResults"->{<|"Amplitude"->1|>}|>]]]];
Internal`InheritedBlock[{RGZLoopSplit,TID},Clear[RGZLoopSplit];Unprotect[TID];Clear[TID];
 RGZLoopSplit[___]:=FCI[FAD[{q,\[Mu]}]+FAD[q]];
 TID[t_,___]:=If[FreeQ[t,\[Mu]],-TerminatedEvaluation["IterationLimit"],TerminatedEvaluation["IterationLimit"]];
 rgzATRec["InvalidIntegralTermsCannotCancel",FailureQ[RGZLoopTID[1]]]];
Print["SUMMARY ",<|"Checks"->Length[rgzATChecks],"Failed"->Keys[Select[rgzATChecks,!TrueQ[#]&]]|>];Exit[If[And@@Values[rgzATChecks],0,1]];
