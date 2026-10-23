(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Optional inherited algorithms, outside the default zero-temperature workflow. *)
loadExperimental[name_] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],Return[r]];
 Block[{$Context="RGZWorkflow`Engine`",$ContextPath=engineContextPath[],$Output={}},Get[FileNameJoin[{$packageRoot,"Kernel","Experimental",name}]]];Null];
RGZSplit[expr_,loop_:RGZWorkflow`Engine`q,ext_:RGZWorkflow`Engine`p,seconds_:60] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,timed["PoleSplit",RGZWorkflow`Engine`RGZMXSplit[expr,loop,ext],seconds]]];
RGZPoleMasses[expr_] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,RGZWorkflow`Engine`RGZLoopPoleMasses[expr]]];
RGZThermalReduce[expr_,loop_,ext_,mu_,nu_,seconds_:60] := engineEvaluation@Module[{r=loadExperimental["ThermalV6.wl"]},If[FailureQ[r],Return[r]];
 r=timed["ThermalV6",RGZWorkflow`Engine`ThermalReduceRGZV6[expr /. {RGZWorkflow`Engine`rgzLoopM1->RGZWorkflow`Engine`m1,RGZWorkflow`Engine`rgzLoopM2->RGZWorkflow`Engine`m2,RGZWorkflow`Engine`rgzLoopM3->RGZWorkflow`Engine`m3,RGZWorkflow`Engine`rgzLoopM4->RGZWorkflow`Engine`m4},loop,ext,mu,nu],seconds];
 If[FailureQ[r],Return[r]];If[!AssociationQ[r],Return[Failure["ThermalReductionFailed",<||>]]];storeResult["Thermal",r];
 <|"Status"->"Complete","Name"->"Thermal","Rows"->Lookup[r,"NumberOfTerms",0],"Scope"->"ExperimentalRestrictedV6"|>];
Options[RGZThermalEvaluate]={"Temperature"->0.15,"ExternalEnergy"->0.,"ExternalSpatialMomentum"->1.,"QMin"->10^-6,"QMax"->10.,"WorkingPrecision"->MachinePrecision};
RGZThermalEvaluate[coeff_Association,masses_Association,seconds_:120,OptionsPattern[]] := engineEvaluation@Module[{r=loadExperimental["ThermalV6.wl"]},If[FailureQ[r],Return[r]];
 If[!And@@(KeyExistsQ[masses,#]&/@{"0","L1","L2","T3","T4"}),Return[Failure["PoleMassSquaredLabelsRequired",<||>]]];
 If[!And@@(NumericQ /@ Values[masses]) || !NumericQ[OptionValue["Temperature"]] || !TrueQ[OptionValue["Temperature"]>0] || !TrueQ[0<=OptionValue["QMin"]<OptionValue["QMax"]],Return[Failure["ThermalNumericalArguments",<||>]]];
 r=timed["ThermalNumerical",Check[RGZWorkflow`Engine`EvaluateThermalCoefficientsV6[coeff,"Mass2"->masses,"T"->OptionValue["Temperature"],"P0M"->OptionValue["ExternalEnergy"],"PV"->OptionValue["ExternalSpatialMomentum"],"QMin"->OptionValue["QMin"],"QMax"->OptionValue["QMax"],"WorkingPrecision"->OptionValue["WorkingPrecision"]],Failure["ThermalNumericalMessages",<||>]],seconds];
 storeResult["ThermalNumerical",r];If[FailureQ[r],r,<|"Status"->"Complete","Name"->"ThermalNumerical","Scope"->"T0SubtractedOnly"|>]
];
RGZVacuumSubtract[expr_,seconds_:120] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],Return[r]];
 If[engineCode["Length[DownValues[PaXEvaluateUVIRSplit]]"]===0,Return[Failure["LoadFeynHelpersFirst",<||>]]];
 RGZWorkflow`Engine`resultVacL=expr;r=timed["VacuumMSbar",Check[loadExperimental["VacuumMSbar.wl"],Failure["VacuumMessages",<||>]],seconds];If[FailureQ[r],Return[r]];
 r=<|"Unrenormalized"->RGZWorkflow`Engine`vacFullL,"Subtracted"->RGZWorkflow`Engine`vacMSbarL,"Scope"->"ExplicitMSbarPoleSubtractionOnly"|>;
 storeResult["VacuumMSbar",r];<|"Status"->"Complete","Name"->"VacuumMSbar"|>
];
