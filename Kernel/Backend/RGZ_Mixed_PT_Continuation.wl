(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* RGZ mixed self-energy + four CANDIDATE pinch kernels.
   Load FeynCalc/FeynArts, RGZ_Mixed_OneLoop_Overall.wl,
   RGZ_Mixed_Scalar_Accelerator.wl and RGZ_Mixed_Gauge_Check.wl first.
   This computes and checks the specified ansatz; it does NOT derive
   the full RGZ quark-scattering Ward/ST identities or omitted graphs.
   Use the successful QCD report from RGZ_QCD_Check_Clean.wl.
   Preserve D, m, mu, mG and M; no M->0 limit of individual PV functions.
   No gauge-fitted normalization, extra graph multiplicity or loop measure.
   A success proves only the checked candidate scalar residual is zero.
*)

ClearAll[RGZPTLegacyStart,RGZPTCheckQCD,RGZPTProject,RGZPTColour,
  RGZPTRun,RGZPTComputePart,RGZPTComputeAll,RGZPTCompare,
  RGZPTContinueAll,
  RGZPTStatus,RGZPTSummary,RGZPTResult,RGZPTSave,
  RGZPTProgress,RGZPTGood,RGZPTSetPart,RGZPTCommonSelf,RGZPTJobCheck,RGZPTKernelSignature];

RGZPTKernelSignature[] := Hash[{"RGZFourPinch/2",DownValues[RGZBuildPinches],
  Options[RGZBuildPinches]},"SHA256"];
RGZPTJobCheck[] := If[!AssociationQ[rgzPTJob],Failure["PTStartFirst",<||>],
  If[Lookup[rgzPTJob,"KernelVersion",None]=!="RGZFourPinch/2" ||
      Lookup[rgzPTJob,"FormulaFingerprint",None]=!=RGZWorkflow`Private`pinchFormulaFingerprint[] ||
      Lookup[rgzPTJob,"KernelSignature",None]=!=RGZPTKernelSignature[] ||
      (Lookup[rgzPTJob,"NAVMode",None]==="FullMatrix" && Lookup[rgzPTJob,"NAVFingerprint",None]=!=RGZWorkflow`Private`navFormulaFingerprint[]),
    Failure["StalePTJob",<|"Action"->"RGZPTStart[]","Description"->"The formulas changed. Reinitialize the four-diagram job and retain previous caches as historical results."|>],
    If[TrueQ[RGZWorkflow`Private`ptJobPacketQ[rgzPTJob]],True,
      Failure["InvalidPTJob",<|"Action"->"RGZPTStart[]","Description"->"The PT task is truncated or contains invalid physical fields; retain previous caches for restart."|>]]]];

RGZPTProgress[name_,stage_] := RGZMXProgressUpdate[
  "Sector"->("RGZ pinch candidateTerm: "<>name),"Stage"->stage,
  "Substep"->"Prepare","DiagramID"->0,"DiagramCount"->0,
  "TermID"->0,"TotalTermCount"->0,"CompleteTermCount"->0,"Status"->"ComputeIn",
  "StageStartTime"->AbsoluteTime[]];

(* A FIXED convention, previously checked with actual FeynArts gse.
   QCD D0=-I/s, D0^2=-1/s^2. In the paper's raw pinch-kernel convention,
   the mixed self contribution is -(D0.Pi.D0)AA. Therefore its QCD limit
   is +Pi/s^2, exactly the convention used to amputate the four pinch
   kernels by s^2 in RGZ_QCD_Stepwise_Check.wl. This minus is not chosen
   by fitting finite RGZ residuals. The common overall factor is 1.
   A comparison in the original connected-propagator convention instead
   multiplies the WHOLE resulting scalar by -1. Zero tests are identical. *)
RGZPTCommonSelf[work_Association] :=
  RGZPTColour /@ ((-work["TreeTransverseAA"]^2 #)& /@ work["SelfEnergyTParts"]);

RGZPTCheckQCD[report_:Automatic] := Module[
  {r=report,names,rows,states,total,gse,c,diff},
  If[r===Automatic,r=rgzQCDReport];
  If[!AssociationQ[r],Return[Failure["QCDReportRequired",<|
    "Description"->"Run RGZ_QCD_Check_Clean.wl first to obtain rgzQCDReport."|>]]];
  names={"box","navAll","avAll","qseAll","gse"};
  rows=Lookup[r,"PerTermResults",{}];
  If[!ListQ[rows] || Length[rows]=!=5 || !And@@(AssociationQ/@rows) ||
      Sort[Lookup[rows,"Parts",Missing[]]]=!=Sort[names],
    Return[Failure["IncompleteQCDBenchmark",<||>]]];
  states=Lookup[rows,"Status",Missing[]];
  If[!And@@(MemberQ[{"PassedBeforeAndAfterIntegration","Passed after integration; integrand forms differ"},#]&/@states),
    Return[Failure["QCDBenchmarkNotPassed",<||>]]];
  total=Lookup[r,"SumCheck",<||>];
  If[!AssociationQ[total] || Lookup[total,"CancellationCheck",""]=!="Passed" ||
      !TrueQ[Lookup[total,"ComputedFiveSectorGaugeDifference / I11",Missing[]]===0],
    Return[Failure["QCDTotalNotZero",<||>]]];
  (* Clean driver calls RGZQCDGSEStart[] with its fixed default 1.
     If the live session exists, additionally verify that it used 1. *)
  If[AssociationQ[rgzQCDGSESession] &&
      Lookup[rgzQCDGSESession,"OverallFactor",Missing[]]=!=1,
    Return[Failure["DifferentQCDConvention",<|
      "Description"->"This workflow uses the checked factor 1 and retains relative factors."|>]]];
  gse=First[Select[rows,#["Parts"]==="gse"&]];
  c=Lookup[gse,"LiteratureI11Coefficient",Missing[]];
  If[!FreeQ[c,_Missing|_Failure] || TrueQ[c===0],
    Return[Failure["QCDGSECoefficientRequired",<||>]]];
  diff=TimeConstrained[Cancel[Together[
    Lookup[gse,"ComputedI11Coefficient",Missing[]]-c]],15,Missing["QCDCheckTimeout"]];
  If[!TrueQ[diff===0],Return[Failure["QCDGSEConventionNotVerified",<|
    "Description"->"Equality of actual gse and literature master-integral coefficients has not been established."|>]]];
  <|"Status"->"Passed","SharedFactor"->1,
    "SelfEnergyConvention"->"-(D0.Pi.D0)AA","Multiplicity"->{"box"->1,
      "navAll"->2,"avAll"->2,"qseAll"->4}|>
];

(* Candidate kernels contain colour Casimirs rather than external deltas.
   Mixed matrices already have their external adjoint delta stripped.
   Use the same SU(N) invariants, not a new numeric Nc substitution. *)
(* The model's bare N in mG^2/Sqrt[N] is the same colour
   dimension as SUNN. Do not replace the head of numerical N[...]. *)
RGZPTColour[x_] := Replace[RGZLoopNormalize[x],
  HoldPattern[System`N]->SUNN,{0,Infinity},Heads->False] /.
  {CA->SUNN,CF->(SUNN^2-1)/(2 SUNN)};

RGZPTProject[x_] := Module[{z},
  RGZMXProgressUpdate["Substep"->"TransverseProjectand Common Casimir Convention"];
  z=Contract[FCI[RGZLoopPT[p,\[Mu],\[Nu]]] FCI[x]]/(D-1);
  z=RGZPTColour[ExpandScalarProduct[z]];
  If[!FreeQ[z,_LorentzIndex|_ExplicitLorentzIndex|_SUNIndex|_SUNF|_SUNDelta],
    Failure["PinchProjectionIncomplete",<||>],z]
];

RGZPTGood[stage_,x_] := !FailureQ[x] &&
  FreeQ[x,_Missing|$Failed|$Aborted|Indeterminate|_DirectedInfinity] &&
  If[MemberQ[{"Integrate","Scalar"},stage],
    FreeQ[x,_TID|_PaVeReduce|_FeynAmpDenominator],True] &&
  If[stage==="Scalar",
    FreeQ[x,_LorentzIndex|_ExplicitLorentzIndex] &&
      Cases[x,z_ /; RGZMXPVObjectQ[z] && !RGZMXPVScalarQ[z],{0,Infinity}]==={},True];

Options[RGZPTLegacyStart]={"CacheDirectory"->Automatic,"QCDReport"->Automatic,"Mode"->"FourPinch","ReferenceGauge"->0,"NAVPacket"->None};
RGZPTLegacyStart[data_,OptionsPattern[]] := Module[
  {required,missing,work,qcd,pinches,names,context,fingerprint,root,dir,parts,treecheck,nav=OptionValue["NAVPacket"],previous=rgzPTJob},
  required={RGZGaugeStart,RGZGaugeCompare,RGZCandidatePinches,
    RGZMXIntegrate,RGZMXScalarReduce,RGZMXFastScalar,RGZMXWrite};
  missing=Select[required,DownValues[#]==={}&];
  If[missing=!={},Return[Failure["LoadPTDependencies",<|
    "Description"->"Load the mixed-matrix workflow, scalar accelerator and gauge-check modules first."|>]]];
  If[Total[Length/@{OwnValues[m],OwnValues[mG],OwnValues[M],
      OwnValues[\[Mu]],OwnValues[\[Alpha]]}]>0,
    Return[Failure["SymbolicParametersRequired",<|
      "Description"->"m, mG, M, mu and alpha must remain symbolic; clear assigned values before continuing."|>]]];
  RGZPTProgress["Initialize","CheckInputAnd QCD Convention"];
  qcd=RGZPTCheckQCD[OptionValue["QCDReport"]];If[FailureQ[qcd],Return[qcd]];
  RGZMXProgressUpdate["Substep"->"Readmixed matrixand attachexternal A Propagator"];
  work=RGZGaugeStart[data];If[FailureQ[work],Return[work]];
  treecheck=Cancel[Together[(work["TreeTransverseAA"] /.
    {m->0,mG->0,\[Mu]->0})+I/SPD[p,p]]];
  If[!TrueQ[treecheck===0],Return[Failure["UnexpectedTreeConvention",<||>]]];
  names={"box","navAll","avAll","qseAll"};
  RGZMXProgressUpdate["Substep"->"Construct the four candidates and check cache identity"];
  pinches=RGZCandidatePinches["Mode"->OptionValue["Mode"],"ReferenceGauge"->OptionValue["ReferenceGauge"]];
  If[FailureQ[pinches],Return[pinches]];
  If[!AssociationQ[pinches] || !And@@(KeyExistsQ[pinches,#]&/@names),
    Return[Failure["CandidatePinchesRequired",<||>]]];
  If[nav=!=None&&!TrueQ[RGZWorkflow`Private`qqPVValidNAVProof[nav,RGZWorkflow`Private`navFormulaFingerprint[]]],Return[Failure["InvalidNAVProofPacket",<||>]]];
  If[AssociationQ[nav],If[pinches["Mode"]=!="FourPinch",Return[Failure["FullNAVRequiresFourPinch",<||>]]];AssociateTo[pinches,"navAll"->nav["Kernel"]]];
  context={"RGZ-PT-FOUR-PINCH-v2",RGZPTKernelSignature[],pinches["Mode"],pinches["ReferenceGauge"],$VersionNumber,$FeynCalcVersion,
    RGZMXIntegralContext[],DownValues[RGZCandidatePinches],
    DownValues[RGZPTProject],DownValues[RGZPTColour],
    DownValues[RGZMXFastScalar],DownValues[RGZMXFastRational],
    DownValues[RGZMXFastApart],DownValues[RGZMXFastTerm],
    DownValues[RGZMXFastTadpole],DownValues[RGZMXFastMass]};
  fingerprint=Hash[{context,If[AssociationQ[nav],nav["Fingerprint"],None]},"SHA256"];
  root=OptionValue["CacheDirectory"];
  If[root===Automatic,root=RGZWorkflow`Private`cacheDirectory[]];
  If[!StringQ[root],Return[Failure["SaveNotebookFirst",<||>]]];
  dir=FileNameJoin[{root,"RGZ_PT_candidate_cache_"<>
    StringTake[IntegerString[fingerprint,16],12]}];
  If[!DirectoryQ[dir],CreateDirectory[dir,CreateIntermediateDirectories->True]];
  parts=Association@@Table[name-><|"OriginalExpression"->pinches[name],"Result"-><||>|>,{name,names}];
  If[AssociationQ[nav],AssociateTo[parts,"navAll"->Join[parts["navAll"],<|"Representation"->"Rational"|>]]];
  If[AssociationQ[previous]&&TrueQ[RGZPTJobCheck[]===True],
   Do[If[name=!="navAll"&&SameQ[Lookup[previous["Parts"][name],"OriginalExpression",None],parts[name]["OriginalExpression"]],
    AssociateTo[parts,name->previous["Parts"][name]]],{name,names}]];
  rgzPTJob=<|"Directory"->dir,"Environment"->fingerprint,"PartsOrder"->names,
    "Parts"->parts,"SelfEnergyParts"->RGZPTCommonSelf[work],
    "NAVMode"->If[AssociationQ[nav],"FullMatrix","AAReplacement"],"NAVFingerprint"->If[AssociationQ[nav],nav["Fingerprint"],None],
    "KernelVersion"->"RGZFourPinch/2","KernelSignature"->RGZPTKernelSignature[],
    "FormulaFingerprint"->RGZWorkflow`Private`pinchFormulaFingerprint[],
    "Mode"->pinches["Mode"],"ReferenceGauge"->pinches["ReferenceGauge"],
    "QCDConvention"->qcd,"Compare"->Missing["NotCompared"]|>;
  RGZMXProgressUpdate["Status"->"Complete"];
  <|"Status"->"PrepareComplete","FullNAVIncluded"->AssociationQ[nav],"PartsCount"->4,"Mode"->pinches["Mode"],"ReferenceGauge"->pinches["ReferenceGauge"],"CacheDirectory"->dir,
    "Description"->"Reuse the pinch cache and retain the original mixed-matrix cache."|>
];

RGZPTSetPart[name_,part_] := Module[{parts=rgzPTJob["Parts"]},
  AssociateTo[parts,name->part];AssociateTo[rgzPTJob,"Parts"->parts];Null];

RGZPTRun[name_String,stage_String,seconds_:1200] := Module[
  {stages={"Project","Decompose","Integrate","Scalar"},part,res,n,input,path,old,z,time,
    record,r,key},
  With[{check=RGZPTJobCheck[]},If[FailureQ[check],Return[check]]];
  If[!MemberQ[rgzPTJob["PartsOrder"],name] || !MemberQ[stages,stage] ||
      !NumericQ[seconds] || !TrueQ[seconds>0],Return[Failure["PTStageArguments",<||>]]];
  part=rgzPTJob["Parts"][name];res=part["Result"];n=First@FirstPosition[stages,stage];
  If[KeyExistsQ[res,stage] && RGZPTGood[stage,res[stage]],
    Return[<|"Parts"->name,"Stage"->stage,"Status"->"Complete; skipped"|>]];
  If[n>1 && !KeyExistsQ[res,stages[[n-1]]],
    Return[Failure["PTRunPreviousStage",<|"Parts"->name,
      "MissingStage"->stages[[n-1]]|>]]];
  input=If[n===1,part["OriginalExpression"],res[stages[[n-1]]]];
  key=ToString[First@FirstPosition[rgzPTJob["PartsOrder"],name]];
  path=FileNameJoin[{rgzPTJob["Directory"],"part_"<>key<>"_"<>ToString[n]<>".wl"}];
  RGZPTProgress[name,stage];
  If[FileExistsQ[path],old=Get[path];
    If[AssociationQ[old] && Lookup[old,"Status",""]==="Complete" &&
        SameQ[Lookup[old,"Input",None],input] &&
        SameQ[Lookup[old,"Environment",None],rgzPTJob["Environment"]] &&
        RGZPTGood[stage,Lookup[old,"Result",Missing[]]],
      AssociateTo[res,stage->old["Result"]];AssociateTo[part,"Result"->res];
      RGZPTSetPart[name,part];RGZMXProgressUpdate["Status"->"CacheRestored"];
      Return[<|"Parts"->name,"Stage"->stage,"Status"->"CacheRestored"|>]]];
  {time,z}=AbsoluteTiming[CheckAbort[TimeConstrained[
    Switch[stage,
      "Project",RGZPTProject[input],
      "Decompose",(* Candidate expressions are ALREADY simple-pole FADs.
                 Distribute products and combine their FAD factors only. *)
        RGZMXProgressUpdate["Substep"->"ExpandPolePartsand Combine FAD Factor"];
        If[Lookup[part,"Representation",None]==="Rational",rgzNAVSplit[input],FeynAmpDenominatorCombine[Expand[FCI[input]]]],
      "Integrate",RGZMXIntegrate[input,
        FileNameJoin[{rgzPTJob["Directory"],"tid_units"}],seconds],
      "Scalar",RGZMXScalarReduce[input,
        FileNameJoin[{rgzPTJob["Directory"],"pv_units"}],seconds]],
    seconds,Failure["PTStageTimeout",<|"Parts"->name,
      "Stage"->stage,"TimeLimit"->seconds|>]],
    Failure["PTStageAborted",<|"Parts"->name,"Stage"->stage|>]]];
  If[!RGZPTGood[stage,z],
    If[!FailureQ[z],z=Failure["PTStageUnresolved",<|
      "Parts"->name,"Stage"->stage,"Resulthead"->Head[z]|>]];
    record=<|"Input"->input,"Environment"->rgzPTJob["Environment"],"Status"->"Incomplete",
      "StoppedCause"->z,"ElapsedSeconds"->time|>;
    r=RGZMXWrite[record,path];If[FailureQ[r],Return[r]];
    AssociateTo[part,"StoppedCause"->z];RGZPTSetPart[name,part];
    RGZMXProgressUpdate["Status"->"Incomplete"];Return[z]];
  record=<|"Input"->input,"Environment"->rgzPTJob["Environment"],"Status"->"Complete",
    "Result"->z,"ElapsedSeconds"->time|>;
  r=RGZMXWrite[record,path];If[FailureQ[r],Return[r]];
  AssociateTo[res,stage->z];part=Join[KeyDrop[part,"StoppedCause"],<|"Result"->res|>];
  RGZPTSetPart[name,part];RGZMXProgressUpdate["Status"->"Complete"];
  <|"Parts"->name,"Stage"->stage,"Status"->"Complete","ElapsedSeconds"->time|>
];

RGZPTComputePart[name_String,seconds_:1200] := Module[{r,stage,stop},Catch[
  Do[r=RGZPTRun[name,stage,seconds];If[FailureQ[r],Throw[r,stop]],
    {stage,{"Project","Decompose","Integrate","Scalar"}}];
  RGZMXReport["Summary",name,": fourItemsStageComplete. "];
  <|"Parts"->name,"Status"->"Complete"|>,stop]
];
RGZPTComputeAll[seconds_:1200] := Module[{r,name,stop},Catch[
  With[{check=RGZPTJobCheck[]},If[FailureQ[check],Return[check]]];
  Do[r=RGZPTComputePart[name,seconds];If[FailureQ[r],Throw[r,stop]],
    {name,rgzPTJob["PartsOrder"]}];
  <|"Status"->"FourSectorscandidateTermAllComplete","PartsCount"->4|>,stop]
];

RGZPTStatus[] := Module[{names,stages,rows},
  With[{check=RGZPTJobCheck[]},If[FailureQ[check],Return[check]]];
  names=rgzPTJob["PartsOrder"];stages={"Project","Decompose","Integrate","Scalar"};
  rows=Table[Prepend[(If[KeyExistsQ[rgzPTJob["Parts"][name]["Result"],#],
    "Complete","Incomplete"]&/@stages),name],{name,names}];
  Grid[Prepend[rows,Prepend[stages,"Parts"]],Frame->All,Alignment->Left]
];
RGZPTSummary[x_] := Which[
  FailureQ[x],Print["StoppedTag: ",x[[1]]];
    Module[{data=x[[2]],keys},keys=Select[
      {"Parts","Stage","MissingStage","IncompleteParts","InputParts","TimeLimit","Description"},KeyExistsQ[data,#]&];
      If[keys=!={},Print[Grid[({#,data[#]}&/@keys),Frame->All,Alignment->Left]]]],
  AssociationQ[x] && KeyExistsQ[x,"ResidualParts"],RGZGaugeSummary[x],
  AssociationQ[x],Print[Grid[Normal[KeyTake[x,
    {"Status","PartsCount","CacheDirectory","Description"}]]/.Rule[a_,b_]:>{a,b},
    Frame->All,Alignment->Left]],
  True,Print["Incomplete; Resulthead: ",Head[x]]];

RGZPTResult[] := Module[{names,missing,pinches,parts},
  With[{check=RGZPTJobCheck[]},If[FailureQ[check],Return[check]]];
  names=rgzPTJob["PartsOrder"];
  missing=Select[names,!KeyExistsQ[rgzPTJob["Parts"][#]["Result"],"Scalar"]&];
  If[missing=!={},Return[Failure["PinchPartsIncomplete",<|"IncompleteParts"->missing|>]]];
  pinches=Association@@Table[name->rgzPTJob["Parts"][name]["Result"]["Scalar"],{name,names}];
  parts=If[rgzPTJob["Mode"]==="FourPinch",
    Join[rgzPTJob["SelfEnergyParts"],Lookup[pinches,names]],Missing["FullPinchRequired"]];
  <|"CommonConventionSelfEnergyTParts"->rgzPTJob["SelfEnergyParts"],
    "CommonConventionSelfEnergyT"->Total[rgzPTJob["SelfEnergyParts"]],
    "pinchParts"->pinches,"pinchSum"->Total[Lookup[pinches,names]],
    "SumParts"->parts,"CandidateSumT"->If[ListQ[parts],Total[parts],parts],
    "QCDConvention"->rgzPTJob["QCDConvention"],"GaugeCheck"->rgzPTJob["Compare"],
    "Mode"->rgzPTJob["Mode"],"ReferenceGauge"->rgzPTJob["ReferenceGauge"],"KernelVersion"->rgzPTJob["KernelVersion"],
    "Description"->"Candidate transverse propagator kernel at finite RGZ parameters. Pinch completeness is unproven; M->0 is not taken automatically."|>
];

Options[RGZPTCompare]={"PreprocessTimeLimit"->600};
RGZPTCompare[reference_:1,seconds_:60,OptionsPattern[]] := Module[{r,report},
  r=RGZPTResult[];If[FailureQ[r],Return[r]];
  If[r["Mode"]=!="FourPinch",Return[Failure["FullPinchRequired",<|"Action"->"RGZPTStart[\"Mode\" -> \"FourPinch\"]"|>]]];
  report=RGZGaugeCompare[r["SumParts"],reference,seconds,
    "PreprocessTimeLimit"->OptionValue["PreprocessTimeLimit"]];
  If[AssociationQ[report],report=Join[report,<|
    "Checkobject"->"Transverse gauge difference of the full mixed self-energy and four candidate pinch sectors",
    "QCDConvention"->rgzPTJob["QCDConvention"],
    "Description"->"A zero residual verifies only the current candidate. RGZ Ward/ST completeness and the final regulator limit require independent checks."|>]];
  AssociateTo[rgzPTJob,"Compare"->report];report
];

Options[RGZPTContinueAll]={"PreprocessTimeLimit"->600,
  "CacheDirectory"->Automatic,"QCDReport"->Automatic};
RGZPTContinueAll[data_,unitSeconds_:1200,coefficientSeconds_:60,
    OptionsPattern[]] := Module[{init,run,report,require,stop},Catch[
  require[x_] := If[FailureQ[x] || Head[x]===Missing ||
      x===$Failed || x===$Aborted,Throw[x,stop],x];
  init=require[RGZPTLegacyStart[data,"CacheDirectory"->OptionValue["CacheDirectory"],
    "QCDReport"->OptionValue["QCDReport"]]];
  run=require[RGZPTComputeAll[unitSeconds]];
  report=require[RGZPTCompare[1,coefficientSeconds,
    "PreprocessTimeLimit"->OptionValue["PreprocessTimeLimit"]]];
  report,stop]
];

RGZPTSave[path_:Automatic] := Module[{file=path,r,t,status},
  r=RGZPTResult[];If[FailureQ[r],Return[r]];
  If[file===Automatic,file=FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],
    "RGZ_Mixed_PT_Check_Result.mx"}]];
  If[!StringQ[file],Return[Failure["PTSavePath",<||>]]];
  rgzPTSaved=r;
  {t,status}=AbsoluteTiming[Check[DumpSave[file,rgzPTSaved],$Failed]];
  If[status===$Failed,Failure["PTSaveFailed",<||>],
    <|"Status"->"SaveComplete","File"->file,"ElapsedSeconds"->t|>]
];

Print["Pinch continuation loaded: RGZPTContinueAll[rgzMixed,1200,60]. For individual stages use RGZPTLegacyStart, RGZPTRun and RGZPTCompare."];
