(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Reduce standard integral/PV units and dispatch explicitly to the configured route.

   Inputs:
   Split scalar/tensor kernels, cache paths, integral environment and time budget.

   Dependencies:
   Native FeynCalc TID/PaVeReduce; Internal/MixedRuntime.wl provides IO and stage predicates.
   TwoPointIntegrals.wl provides the independently named accelerated route.
*)

ClearAll[RGZMXIntegralContext, RGZMXIntegralUnitStandard, RGZMXIntegralUnits, RGZMXIntegrate, RGZMXPVObjectQ, RGZMXPVScalarQ, RGZMXPVUnit, RGZMXScalarReduce];

(* Linearity: factors independent of q belong outside the loop integral.
   Identical q-dependent kernels are integrated only once, including
   across graphs and between T/L. No physical mass relation is assumed. *)
RGZMXIntegralUnits[split_] := Module[{parts,pairs,groups,factors},
  parts=RGZMXTerms[Expand[split]];
  pairs=Map[Function[term,
    factors=If[Head[term]===Times,List@@term,{term}];
    {Times@@Select[factors,FreeQ[#,q]&],
      Times@@Select[factors,!FreeQ[#,q]&]}],parts];
  groups=GatherBy[pairs,Last];
  Select[Map[Function[group,
    {Total[group[[All,1]]],group[[1,2]]}],groups],!TrueQ[#[[1]]===0]&]
];

RGZMXIntegralContext[] := {
  {"MixedIntegralPolicy/2","KeepLogDivergentScalelessIntegrals"->$KeepLogDivergentScalelessIntegrals,
    "Assumptions"->$Assumptions},
  $VersionNumber,$FeynCalcVersion,
  FCI[SPD[p,p]],FCI[SPD[p,q]],FCI[SPD[q,q]],
  Hash[{Options[TID],DownValues[TID]},"SHA256"]};

RGZMXIntegralUnitStandard[kernel_,path_,context_,seconds_] := Module[
  {old,record,z,time,den,r,status},
  If[!RGZMXValidValueQ[kernel],Return[Failure["InvalidIntegralInput",<||>]]];
  If[StringQ[path] && FileExistsQ[path],
    old=Get[path];
    If[AssociationQ[old] && Lookup[old,"Version",0]===1 &&
      Lookup[old,"Status",""]==="Complete" &&
      SameQ[Lookup[old,"Input",Missing[]],kernel] &&
      SameQ[Lookup[old,"Environment",Missing[]],context] &&
      RGZMXStageValueQ["IntegrateT",Lookup[old,"Result",Missing[]]],
      RGZMXReport["Verbose","IntegrateKernel: RestoreCache"];
      AssociateTo[rgzMXLastIntegralUnit,"Status"->"RestoreCache"];
      Return[old["Result"]]]];
  record=<|"Version"->1,"Input"->kernel,"Environment"->context,"Status"->"process In"|>;
  (* Save the exact slow input before TID, so interruption is inspectable. *)
  If[StringQ[path],r=RGZMXWrite[record,path];If[FailureQ[r],Return[r]]];
  {time,z}=AbsoluteTiming[CheckAbort[TimeConstrained[
    If[FreeQ[kernel,_FeynAmpDenominator],
      den=Denominator[Together[kernel]];
      If[!FreeQ[den,q],
        Failure["UnsplitLoopDenominator",<|"InputFile"->path|>],
        0], (* polynomial loop integrals vanish in dimensional regularization *)
      TID[kernel,q,ToPaVe->True,UsePaVeBasis->True,ApartFF->False]],
    seconds,$Aborted],$Aborted]];
  If[z=!=$Aborted && z=!=$Failed && !FailureQ[z] &&
    !RGZMXStageValueQ["IntegrateT",z],
    z=Failure["IncompleteScalarReduction",<|"InputFile"->path|>]];
  status=Which[z===$Aborted,"TimeoutOrAborted",z===$Failed || FailureQ[z],"ComputeFailed",True,"Complete"];
  record=Join[record,<|"Status"->status,"ElapsedSeconds"->time,"Result"->z|>];
  AssociateTo[rgzMXLastIntegralUnit,{"Status"->status,"ElapsedSeconds"->time}];
  If[StringQ[path],r=RGZMXWrite[record,path];If[FailureQ[r],Return[r]]];
  RGZMXReport["Verbose","IntegrateKernel: ",status,"; ElapsedSeconds ",Round[time,0.01]," Seconds. "];
  If[status==="Complete",z,Failure["IntegralUnitStopped",<|
    "TermID"->Lookup[rgzMXLastIntegralUnit,"TermID"],"InputFile"->path,
    "Status"->status,"Cause"->z|>]]
];

RGZMXIntegrate[split_,cacheDir_:Automatic,seconds_:300] := Module[
  {units,out={},z,i,path,context,stop},Catch[
  rgzMXLastIntegralUnit=<|"Status"->"organize IntegrateKernel"|>;
  RGZMXReport["Verbose","Integration: factor out loop-independent coefficients and combine matching kernels"];
  RGZMXProgressUpdate["Substep"->"organize IntegrateKernel"];
  units=RGZMXIntegralUnits[split];context=RGZMXIntegralContext[];
  RGZMXProgressUpdate["Substep"->"TID","TermID"->0,"TotalTermCount"->Length[units],"CompleteTermCount"->0];
  RGZMXReport["Verbose","Integrate: ",Length[units]," ItemsNotsame IntegrateKernel"];
  If[StringQ[cacheDir] && !DirectoryQ[cacheDir],
    CreateDirectory[cacheDir,CreateIntermediateDirectories->True]];
  Do[
    path=If[StringQ[cacheDir],FileNameJoin[{cacheDir,
      "unit_"<>IntegerString[Hash[{"AA-TID-unit-v1",units[[i,2]],context},
        "SHA256"],16]<>".wl"}],None];
    rgzMXLastIntegralUnit=<|"TermID"->i,"TotalTermCount"->Length[units],
      "InputFile"->path,"InputSize"->LeafCount[units[[i,2]]],"Status"->"process In"|>;
    RGZMXProgressUpdate["TermID"->i,"CompleteTermCount"->i-1];
    RGZMXReport["Verbose","TID：",i," / ",Length[units],"; IntegrateKernel LeafCount = ",
      rgzMXLastIntegralUnit["InputSize"]];
    z=RGZMXIntegralUnit[units[[i,2]],path,context,seconds];
    If[FailureQ[z],Throw[z,stop]];
    RGZMXProgressUpdate["CompleteTermCount"->i];
    AppendTo[out,units[[i,1]] z],{i,Length[units]}];
  Total[out],stop]
];

(* Reduce only independent PV FUNCTIONS, never the large coefficients
   multiplying them. Substitute the resulting identities back exactly;
   no expansion or global simplification of the RGZ amplitude is needed.
   Genuine repeated poles may retain degenerate scalar C0/D0 functions. *)
RGZMXPVObjectQ[x_] := !AtomQ[x] && With[{name=RGZLoopName[Head[x]]},
  MemberQ[{"PaVe","GenPaVe"},name] ||
  StringMatchQ[name,("A"|"B"|"C"|"D")~~DigitCharacter..]];

RGZMXPVScalarQ[x_] := MemberQ[{"A0","B0","C0","D0"},RGZLoopName[Head[x]]] ||
  (RGZLoopName[Head[x]]==="PaVe" &&
    MatchQ[List@@x,{0,_List,_List,___Rule}]);

RGZMXPVUnit[object_,path_,context_,seconds_] := Module[
  {old,record,z,time,r,status,remaining},
  If[!RGZMXValidValueQ[object],Return[Failure["InvalidPVInput",<||>]]];
  If[StringQ[path] && FileExistsQ[path],
    old=Get[path];
    If[AssociationQ[old] && Lookup[old,"Version",0]===1 &&
      Lookup[old,"Status",""]==="Complete" &&
      SameQ[Lookup[old,"Input",Missing[]],object] &&
      SameQ[Lookup[old,"Environment",Missing[]],context] &&
      RGZMXStageValueQ["ScalarT",Lookup[old,"Result",Missing[]]],
      RGZMXReport["Verbose","PV Function: RestoreCache"];
      AssociateTo[rgzMXLastPVUnit,"Status"->"RestoreCache"];
      Return[old["Result"]]]];
  record=<|"Version"->1,"Input"->object,"Environment"->context,"Status"->"process In"|>;
  If[StringQ[path],r=RGZMXWrite[record,path];If[FailureQ[r],Return[r]]];
  {time,z}=AbsoluteTiming[CheckAbort[TimeConstrained[
    PaVeReduce[object],seconds,$Aborted],$Aborted]];
  If[z=!=$Aborted && z=!=$Failed && !FailureQ[z],
    remaining=Cases[z,x_ /; RGZMXPVObjectQ[x] && !RGZMXPVScalarQ[x],{0,Infinity}];
    If[!RGZMXStageValueQ["ScalarT",z] || remaining=!={},
      z=Failure["IncompletePaVeReduce",<|"InputFile"->path,
        "UnreducedFunction"->DeleteDuplicates[remaining]|>]]];
  status=Which[z===$Aborted,"TimeoutOrAborted",z===$Failed || FailureQ[z],"ComputeFailed",True,"Complete"];
  record=Join[record,<|"Status"->status,"ElapsedSeconds"->time,"Result"->z|>];
  AssociateTo[rgzMXLastPVUnit,{"Status"->status,"ElapsedSeconds"->time}];
  If[StringQ[path],r=RGZMXWrite[record,path];If[FailureQ[r],Return[r]]];
  RGZMXReport["Verbose","PV Function: ",status,"; ElapsedSeconds ",Round[time,0.01]," Seconds. "];
  If[status==="Complete",z,Failure["PVUnitStopped",<|
    "TermID"->Lookup[rgzMXLastPVUnit,"TermID"],"InputFile"->path,
    "Status"->status,"Cause"->z|>]]
];

RGZMXScalarReduce[expr_,cacheDir_:Automatic,seconds_:300] := Module[
  {objects,needed,out={},z,i,path,context,stop},Catch[
  rgzMXLastPVUnit=<|"Status"->"Collect PV Function"|>;
  RGZMXProgressUpdate["Substep"->"Collect PV Function"];
  RGZMXReport["Verbose","Scalar reduction: collect distinct PV functions with external coefficients"];
  If[!RGZMXStageValueQ["IntegrateT",expr],
    Throw[Failure["IncompleteIntegralInput",<||>],stop]];
  objects=DeleteDuplicates@Cases[expr,x_ /; RGZMXPVObjectQ[x],{0,Infinity}];
  needed=Select[objects,!RGZMXPVScalarQ[#]&];
  RGZMXProgressUpdate["Substep"->"PaVeReduce","TermID"->0,"TotalTermCount"->Length[needed],"CompleteTermCount"->0];
  RGZMXReport["Verbose","ScalarReduce: ",Length[objects]," ItemsNotsame  PV Function; pending Reduce ",Length[needed]," Items"];
  If[needed==={},
    rgzMXLastPVUnit=<|"Status"->"Already a scalar integral","pending ReduceFunctionCount"->0|>;
    Throw[expr,stop]];
  context=Join[RGZMXIntegralContext[],{
    Hash[{Options[PaVeReduce],DownValues[PaVeReduce]},"SHA256"]}];
  If[StringQ[cacheDir] && !DirectoryQ[cacheDir],
    CreateDirectory[cacheDir,CreateIntermediateDirectories->True]];
  Do[
    path=If[StringQ[cacheDir],FileNameJoin[{cacheDir,
      "pv_"<>IntegerString[Hash[{"AA-PV-unit-v1",needed[[i]],context},
        "SHA256"],16]<>".wl"}],None];
    rgzMXLastPVUnit=<|"TermID"->i,"TotalTermCount"->Length[needed],
      "PVFunction"->needed[[i]],"InputFile"->path,"Status"->"process In"|>;
    RGZMXProgressUpdate["TermID"->i,"CompleteTermCount"->i-1];
    RGZMXReport["Verbose","PaVeReduce：",i," / ",Length[needed],"；",
      RGZLoopName[Head[needed[[i]]]],"；LeafCount = ",LeafCount[needed[[i]]]];
    z=RGZMXPVUnit[needed[[i]],path,context,seconds];
    If[FailureQ[z],Throw[z,stop]];RGZMXProgressUpdate["CompleteTermCount"->i];AppendTo[out,z],{i,Length[needed]}];
  RGZMXProgressUpdate["Substep"->"SubstituteBackOriginalCoefficient"];
  RGZMXReport["Verbose","ScalarReduce: SubstituteBackOriginalCoefficient"];
  expr /. Thread[needed->out],stop]
];

(* The only integral-unit entry: route selection is explicit, never a replacement on load. *)
RGZMXIntegralUnit[kernel_,path_,context_,seconds_] :=
 If[TrueQ[rgzMXAcceleration["Enabled"]],
   RGZMXIntegralUnitAccelerated[kernel,path,context,seconds],
   RGZMXIntegralUnitStandard[kernel,path,context,seconds]];
