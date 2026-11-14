(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Run graph stages and maintain mixed-task records, checkpoints and progress.

   Inputs:
   Mixed graph rows, current task configuration and explicit stage requests.

   Dependencies:
   Algebra/MixedIntegrals.wl supplies the integral dispatcher; MixedPoles.wl and
   MixedProjection.wl supply algebra. OneLoop/MixedSelfEnergy.wl supplies graph generation and sector/matrix sums.
*)

ClearAll[RGZMXConvertedQ, RGZMXInitStep, RGZMXLegacyAA, RGZMXMigrateRow, RGZMXModelFingerprint, RGZMXMonitor, RGZMXProgressPanel, RGZMXProgressUpdate, RGZMXReport, RGZMXResume, RGZMXRun, RGZMXRunAll, RGZMXSetDisplay, RGZMXShow, RGZMXSplitStep, RGZMXStageContext, RGZMXStageNeeds, RGZMXStageValueQ, RGZMXStart, RGZMXStatus, RGZMXValidValueQ, RGZMXWrite, RGZMixCompactData, RGZMixMonitor, RGZMixPrepare, RGZMixProjectedPair, RGZMixRead, RGZMixRecordPath, RGZMixResultReport, RGZMixResultStep, RGZMixRun, RGZMixSetDisplay, RGZMixShow, RGZMixShowGraph, RGZMixSplitDiagnostics, RGZMixStart, RGZMixStatus, RGZMixStore];

(* Display policy only: no change to amplitudes or cache fingerprints.
   A single optional Monitor cell refreshes at most once per two seconds.
   Ordinary messages and failure notices are never suppressed by quiet mode. *)
If[!MemberQ[{"Compact","Quiet","Verbose"},$RGZMXOutputMode],$RGZMXOutputMode="Compact"];

rgzMXProgress=<|"Sector"->"", "DiagramID"->0,"DiagramCount"->0,"Stage"->"Waiting",
  "Substep"->"", "TermID"->0,"TotalTermCount"->0,"CompleteTermCount"->0,
  "Status"->"Waiting","StageStartTime"->AbsoluteTime[]|>;

RGZMXSetDisplay[mode_String:"Compact"] :=
  If[MemberQ[{"Compact","Quiet","Verbose"},mode],$RGZMXOutputMode=mode,
    Failure["DisplayMode",<|"Availablemode"->{"Compact","Quiet","Verbose"}|>]];

RGZMXReport[level_String,args___] := If[
  level==="Error" || $RGZMXOutputMode==="Verbose" ||
    (level==="Summary" && $RGZMXOutputMode==="Compact"),Print[args]];

RGZMXProgressUpdate[rules__Rule] :=
  (rgzMXProgress=Join[rgzMXProgress,Association@@{rules}];Null);

RGZMXProgressPanel[] := Module[{a=rgzMXProgress,n,total,done,elapsed,bar,operationInfo},
  n=Lookup[a,"TermID",0];total=Lookup[a,"TotalTermCount",0];
  done=Lookup[a,"CompleteTermCount",0];
  elapsed=Max[0,Round[AbsoluteTime[]-Lookup[a,"StageStartTime",AbsoluteTime[]]]];
  bar=If[total>0,ProgressIndicator[done,{0,total}],
    If[Lookup[a,"Status",""]==="Complete",ProgressIndicator[1],ProgressIndicator[Indeterminate]]];
  operationInfo=Which[
    Lookup[a,"Status",""]==="ComputeIn" &&
      MemberQ[{"DecomposeT","DecomposeL"},Lookup[a,"Stage",""]] &&
      AssociationQ[rgzMXLastSplitStep],
    Row[{"CurrentOperation ",Max[0,Round[AbsoluteTime[]-
      Lookup[rgzMXLastSplitStep,"StartTime",AbsoluteTime[]]]]," Seconds",
      If[Lookup[rgzMXLastSplitStep,"MomentumRouting",0]>0,
        Row[{"  ·  Routing ",rgzMXLastSplitStep["MomentumRouting"]," / 3"}],""]}],
    Lookup[a,"Status",""]==="ComputeIn" &&
      MemberQ[{"Aggregate","TensorMatrix","TMatrix","LMatrix"},Lookup[a,"Stage",""]],
      Row[{"CurrentOperation ",Max[0,Round[AbsoluteTime[]-
        Lookup[a,"OperationStartTime",AbsoluteTime[]]]]," Seconds"}],
    True,Nothing];
  Panel[Column[{
    Row[{Lookup[a,"Sector",""],"  ·  ",
      If[Lookup[a,"DiagramCount",0]>0,
        Row[{"Diagram ",Lookup[a,"DiagramID",0]," / ",Lookup[a,"DiagramCount",0],"  ·  "}],""],
      Lookup[a,"Stage",""]}],
    Row[{Lookup[a,"Substep",""],
      If[total>0,Row[{"  ",n," / ",total}],""]}],
    bar,operationInfo,Row[{Lookup[a,"Status",""],"  ·  StageElapsedSeconds ",elapsed," Seconds"}]
  },Spacings->0.4],ImageSize->480]
];

SetAttributes[RGZMXMonitor,HoldAll];

RGZMXMonitor[expr_] := If[
  $FrontEnd===Null || $RGZMXOutputMode==="Quiet" || TrueQ[rgzMXMonitorActive],expr,
  Monitor[Block[{rgzMXMonitorActive=True},expr],
    Refresh[RGZMXProgressPanel[],UpdateInterval->2,TrackedSymbols:>{}]]];

SetAttributes[RGZMXInitStep,HoldRest];

RGZMXInitStep[label_,expr_,seconds_] := Module[{time,z},
  RGZMXProgressUpdate["Substep"->label,"Status"->"ComputeIn"];
  RGZMXReport["Verbose","Start: ",label];
  {time,z}=AbsoluteTiming[CheckAbort[
    TimeConstrained[expr,seconds,$Aborted],$Aborted]];
  RGZMXReport["Verbose","End: ",label,"; ElapsedSeconds ",Round[time,0.01]," Seconds; ",
    Which[z===$Aborted,"TimeoutOrAborted",z===$Failed || FailureQ[z],"Failed",True,"Complete"]];
  If[z===$Aborted || z===$Failed || FailureQ[z],
    RGZMXProgressUpdate["Status"->"InitializeIncomplete"];
    RGZMXReport["Error","InitializeIncomplete: ",label,"; ElapsedSeconds ",Round[time,0.01]," Seconds. "]];
  If[z===$Aborted || z===$Failed,
    Failure["InitializationStopped",<|"Step"->label,"Cause"->z|>],z]
];

(* Inspect nested payloads, not only their outer head. Metadata about a
   stopped attempt may contain a Failure; only stage VALUES are validated. *)
RGZMXValidValueQ[x_] := FreeQ[x,
  _Failure|_Missing|_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|
    Indeterminate|ComplexInfinity|_DirectedInfinity];

RGZMXStageNeeds[] := <|"Convert"->{},"Project"->{"Convert"},
  "DecomposeT"->{"Project"},"IntegrateT"->{"DecomposeT"},"ScalarT"->{"IntegrateT"},
  "DecomposeL"->{"Project"},"IntegrateL"->{"DecomposeL"},"ScalarL"->{"IntegrateL"}|>;

RGZMXStageValueQ[stage_String,value_] := RGZMXValidValueQ[value] && Which[
  stage==="Project",AssociationQ[value] && And@@(KeyExistsQ[value,#]&/@{"T","L"}),
  MemberQ[{"IntegrateT","IntegrateL","ScalarT","ScalarL"},stage],
    !AssociationQ[value] && !ListQ[value] && FreeQ[value,_TID|_PaVeReduce|_FeynAmpDenominator] &&
      (!StringStartsQ[stage,"Scalar"] ||
        Cases[value,x_ /; RGZMXPVObjectQ[x] && !RGZMXPVScalarQ[x],{0,Infinity}]==={}),
  True,!AssociationQ[value] && !ListQ[value]];

RGZMXStageContext[] := {"MixedStageContext/2",RGZMXIntegralContext[],
  Options[FCFAConvert],Options[PaVeReduce]};

RGZMXConvertedQ[row_Association] :=
  RGZMXStageValueQ["Convert",Lookup[row,"Amplitude",Missing["NotConverted"]]];

RGZMXMigrateRow[row_Association] := Module[
  {out=row,states,contexts,needs=RGZMXStageNeeds[],context=RGZMXStageContext[],value,failed},
  states=Lookup[row,"Status",<||>];contexts=Lookup[row,"StageEnvironment",<||>];
  If[!AssociationQ[states],states=<||>];If[!AssociationQ[contexts],contexts=<||>];
  failed=Lookup[row,"FailedRecord",<||>];If[!AssociationQ[failed],failed=<||>];
  Do[If[Lookup[states,stage,""]==="Complete",
    value=Lookup[out,If[stage==="Convert","Amplitude",stage],Missing["AbsentStage"]];
    If[Lookup[contexts,stage,None]=!=context || !RGZMXStageValueQ[stage,value] ||
        !AllTrue[needs[stage],Lookup[states,#,""]==="Complete"&],
      AssociateTo[failed,stage->Failure["InvalidStoredStage",<|
        "Action"->"Recompute this stage and its dependent stages."|>]];
      AssociateTo[states,stage->"NotRun"];contexts=KeyDrop[contexts,stage];
      out=KeyDrop[out,If[stage==="Convert",{"Amplitude","Convert"},{stage}]]]],
    {stage,Keys[needs]}];
  Join[out,<|"Status"->states,"StageEnvironment"->contexts,"FailedRecord"->failed|>]
];

(* Replace each checkpoint atomically; a failed stage does not erase
   previously successful stages. One graph per file, avoiding repeated
   writes of the entire large amplitude collection. *)
RGZMXWrite[value_,path_String] := Module[{tmp=path<>"."<>CreateUUID[]<>".tmp",r},
  Internal`WithLocalSettings[Null,
    r=Quiet[Check[Put[value,tmp],$Failed]];
    If[r=!=Null || !FileExistsQ[tmp],Return[Failure["CacheWrite",<|"Path"->path|>]]];
    r=Quiet[Check[RenameFile[tmp,path,OverwriteTarget->True],$Failed]];
    If[!StringQ[r] || !FileExistsQ[path],Failure["CacheWrite",<|"Path"->path|>],path],
    If[FileExistsQ[tmp],Quiet[DeleteFile[tmp]]]]
];

(* Reuse a trusted, completed AA checkpoint by COPYING it into the new
   matrix cache. The old AA manifest and graph files are never modified. *)
RGZMXLegacyAA[sector_String,root_String] := Module[{hash,path,m,files,rows},
  hash=IntegerString[Hash[{Import[modelRGZ<>".gen","Text"],
    Import[modelRGZ<>".mod","Text"],{sector},
    If[sector==="FourPointVertexLoop","AA-workflow-v3",
      "AA-workflow-v3-grouped-routes-v5"]},"SHA256"],16];
  path=FileNameJoin[{root,"RGZ_AA_cache_"<>StringTake[hash,12]}];
  If[!FileExistsQ[FileNameJoin[{path,"job.wl"}]],Return[Missing["NoAACache"]]];
  m=Get[FileNameJoin[{path,"job.wl"}]];
  If[!AssociationQ[m] || Lookup[m,"Version",0]=!=3 ||
    Lookup[m,"Model",None]=!=modelRGZ || Lookup[m,"ComputeCategory",{}]=!={sector} ||
    Lookup[m,"ModelFingerprint",None]=!=RGZMXModelFingerprint[] ||
    Lookup[m,"StageEnvironment",None]=!=RGZMXStageContext[] ||
    !IntegerQ[Lookup[m,"DiagramCount",None]],Return[Missing["IncompatibleAACache"]]];
  files=Table[FileNameJoin[{path,"graph_"<>ToString[n]<>".wl"}],{n,m["DiagramCount"]}];
  If[!And@@(FileExistsQ/@files),Return[Missing["IncompleteAACache"]]];
  rows=Get/@files;
  If[!And@@(AssociationQ/@rows),Return[Missing["BadAACache"]]];
  <|"Source"->path,"PerDiagram"->(RGZMXMigrateRow/@rows)|>
];

(* Explicit cache directories must obey the same model identity as the
   automatic hash directories. A basename alone cannot validate a cache. *)
RGZMXModelFingerprint[] := Hash[{RGZWorkflow`Private`workflowFingerprint[],Import[modelRGZ<>#, "Text"]& /@ {".gen",".mod"},RGZWorkflow`Private`quarkMasses[]},"SHA256"];

Options[RGZMXStart]={"CacheDirectory"->Automatic,"Category"->All,"SingleStepTimeLimit"->60,
  "FieldPair"->{1,1},"reuseAACache"->True};

RGZMXStart[OptionsPattern[]] := Module[
  {dir,root,hash,tops,ins,rows={},i,pair,process,selected,seconds,
   content,cache,manifest,r,stop,old,origin=None,generated},Catch[
  pair=OptionValue["FieldPair"];seconds=OptionValue["SingleStepTimeLimit"];
  If[!MatchQ[pair,{_Integer,_Integer}] || !And@@(1<=#<=8&/@pair),
    Throw[Failure["FieldPair",<|"FieldPair"->pair|>],stop]];
  If[!NumericQ[seconds] || !TrueQ[seconds>0],
    Throw[Failure["InvalidTimeLimit",<||>],stop]];
  content=RGZWorkflow`Private`modelContent[];If[FailureQ[content],Throw[content,stop]];
  process={RGZMXField[pair[[1]]]}->{RGZMXAntiField[pair[[2]]]};
  tops=<|"FourPointVertexLoop"->rgzLoopTopTadpole,
    "BosonicThreePointLoop"->rgzLoopTopBosonBubble,
    "GhostThreePointLoop"->rgzLoopTopGhostBubble,"QuarkBubble"->rgzLoopTopGhostBubble|>;
  selected=OptionValue["Category"];If[selected===All,selected=Keys[tops]];
  If[!ListQ[selected] || selected==={} || !And@@(KeyExistsQ[tops,#]&/@selected),
    Throw[Failure["SectorName",<|"AvailableCategory"->Keys[tops]|>],stop]];
  tops=KeyTake[tops,DeleteDuplicates[selected]];
  If[!StringQ[modelRGZ] || !And@@(FileExistsQ[modelRGZ<>#]&/@{".gen",".mod"}),
    Throw[Failure["ModelPath",<|"Description"->"Set modelRGZ first."|>],stop]];
  root=Quiet[Check[RGZWorkflow`Private`cacheDirectory[],$Failed]];If[!StringQ[root],root=Directory[]];
  dir=OptionValue["CacheDirectory"];
  If[dir===Automatic,
    hash=IntegerString[Hash[{Import[modelRGZ<>".gen","Text"],
      Import[modelRGZ<>".mod","Text"],Keys[tops],pair,
      "mixed-complete-matter-v3",RGZWorkflow`Private`workflowFingerprint[],RGZWorkflow`Private`quarkMasses[],$FeynCalcVersion,$VersionNumber,
      Options[FCFAConvert],RGZMXIntegralContext[],Options[PaVeReduce]},"SHA256"],16];
    dir=FileNameJoin[{root,"RGZ_Mix_cache_"<>StringTake[hash,12]}]];
  cache=ExpandFileName[dir];manifest=FileNameJoin[{cache,"job.wl"}];
  RGZMXProgressUpdate["Sector"->StringRiffle[RGZMXLabel/@pair," / "],
    "DiagramID"->0,"DiagramCount"->0,"Stage"->"Initialize","Substep"->"PrepareExternalLegs",
    "TermID"->0,"TotalTermCount"->0,"CompleteTermCount"->0,"Status"->"ComputeIn",
    "StageStartTime"->AbsoluteTime[]];
  If[FileExistsQ[manifest],
    old=Get[manifest];
    If[!AssociationQ[old] || Lookup[old,"FieldPair",None]=!=pair ||
      Lookup[old,"ComputeCategory",{}]=!=Keys[tops] || Lookup[old,"Model",None]=!=modelRGZ,
      Throw[Failure["CachePairMismatch",<|"Directory"->cache|>],stop]];
    Throw[RGZMXResume[cache],stop]];
  If[!DirectoryQ[cache],CreateDirectory[cache,CreateIntermediateDirectories->True]];
  If[pair==={1,1} && Length[Keys[tops]]===1 && TrueQ[OptionValue["reuseAACache"]] &&
    RGZMXUnfixedKinematicsQ[],
    old=RGZMXLegacyAA[First[Keys[tops]],root];
    If[AssociationQ[old],rows=old["PerDiagram"];origin=old["Source"]]];
  ins=<||>;
  If[origin===None,
    generated=RGZMXGenerateGraphs[pair,tops,process,content,seconds];
    If[FailureQ[generated],Throw[generated,stop]];
    rows=generated["PerDiagram"];ins=generated["InsertionDiagram"]];
  (* A successfully generated empty insertion list is a calculated zero.
     It is distinct from an error, timeout, or a pair never requested. *)
  rgzMXJob=<|"Version"->3,"Model"->modelRGZ,"ModelFingerprint"->RGZMXModelFingerprint[],"CacheDirectory"->cache,
    "StageEnvironment"->RGZMXStageContext[],
    "FieldPair"->pair,"AllIncomingFields"->(RGZMXField/@pair),"FAoverprocess"->process,
    "ComputeCategory"->Keys[tops],"InsertionDiagram"->ins,"PerDiagram"->rows,"AACacheSource"->origin|>;
  Do[r=RGZMXWrite[rows[[i]],FileNameJoin[{cache,"graph_"<>ToString[i]<>".wl"}]];
    If[FailureQ[r],Throw[r,stop]],{i,Length[rows]}];
  r=RGZMXWrite[Join[KeyDrop[rgzMXJob,"PerDiagram"],<|"DiagramCount"->Length[rows]|>],manifest];
  If[FailureQ[r],Throw[r,stop]];
  RGZMXProgressUpdate["DiagramCount"->Length[rows],"Status"->"InitializeComplete"];
  <|"FieldPair"->(RGZMXLabel/@pair),"DiagramCount"->Length[rows],"CacheDirectory"->cache,
    "Status"->If[rows==={},"No inserted diagrams; this sector is zero","InitializeComplete"],
    "AACacheSource"->origin|>,stop]
];

RGZMXResume[dir_String] := Module[{m,files,rows},
  m=Get[FileNameJoin[{dir,"job.wl"}]];
  If[!AssociationQ[m] || Lookup[m,"Version",0]=!=3,
    Return[Failure["BadCache",<|"Path"->dir,
      "Description"->"This version uses v3 caches and retains earlier cache directories."|>]]];
  If[Lookup[m,"ModelFingerprint",None]=!=RGZMXModelFingerprint[],
    Return[Failure["StaleGraphCache",<|"Path"->dir,
      "Action"->"RGZMXStart[] with an automatic cache directory; recompute with the current model."|>]]];
  If[Lookup[m,"StageEnvironment",None]=!=RGZMXStageContext[],
    Return[Failure["StaleGraphEnvironment",<|"Path"->dir,
      "Action"->"Start a new automatic cache for the current integral convention and environment."|>]]];
  files=Table[FileNameJoin[{dir,"graph_"<>ToString[i]<>".wl"}],{i,m["DiagramCount"]}];
  If[!And@@(FileExistsQ/@files),Return[Failure["MissingCheckpoint",<|"Files"->files|>]]];
  rows=Get/@files;
  If[!And@@(AssociationQ/@rows),Return[Failure["BadGraphCache",<||>]]];
  rows=RGZMXMigrateRow/@rows;
  rgzMXJob=Join[KeyDrop[m,"DiagramCount"],<|"PerDiagram"->rows|>];
  RGZMXProgressUpdate["DiagramCount"->Length[rows],"Status"->"CacheRestore"];
  RGZMXReport["Verbose","Restore ",Length[rows]," diagrams; completed stages are skipped."];RGZMXStatus[]
];

(* The splitting algebra below matches RGZLoopSplit/Term/Factor.
   Add only progress callbacks: all FullSimplify checks, repeated-pole
   coefficients and final reconstruction checks are retained. The bar
   counts completed additive terms, not elapsed time or pole evaluations.
   Diagnostics expose only the currently active splitting operation. *)
RGZMXSplitStep[label_String,extra_Association:<||>] := (
  rgzMXLastSplitStep=Join[<|"Operation"->label,"StartTime"->AbsoluteTime[],
    "Stage"->Lookup[rgzMXProgress,"Stage",""],
    "TermID"->Lookup[rgzMXProgress,"TermID",0],
    "TotalTermCount"->Lookup[rgzMXProgress,"TotalTermCount",0],
    "MomentumRouting"->If[IntegerQ[rgzMXSplitRoute],rgzMXSplitRoute,0]|>,extra];
  RGZMXProgressUpdate["Substep"->label];Null);

RGZMixSplitDiagnostics[] := If[AssociationQ[rgzMXLastSplitStep],
  Join[rgzMXLastSplitStep,<|"OperationElapsedSeconds (Seconds)"->
    Max[0,AbsoluteTime[]-rgzMXLastSplitStep["StartTime"]]|>],
  Missing["NoSplitOperation"]];

RGZMXRun[n_Integer,stage_String,seconds_:300] := Module[
  {row,all,states,needs,req,z,time,status,r,path,result,context,contexts,valid,
    component,integralStage,splitStage,refreshRecord},
  If[!AssociationQ[rgzMXJob],Return[Failure["StartFirst",<||>]]];
  If[n<1 || n>Length[rgzMXJob["PerDiagram"]],Return[Failure["GraphNumber",<||>]]];
  context=RGZMXStageContext[];
  If[Lookup[rgzMXJob,"StageEnvironment",None]=!=context,
    Return[Failure["StaleGraphEnvironment",<|
      "Action"->"Start or resume a graph job generated for the current environment."|>]]];
  (* Before scalar reduction, reorganize unfinished tensor-PV integrals
     when the optional scalar accelerator is enabled. *)
  If[TrueQ[rgzMXAcceleration["Enabled"]] &&
      MemberQ[{"ScalarT","ScalarL"},stage] && AssociationQ[rgzMXJob] &&
      1<=n<=Length[rgzMXJob["PerDiagram"]],
    row=RGZMXMigrateRow[rgzMXJob["PerDiagram"][[n]]];
    component=StringTake[stage,-1];
    integralStage="Integrate"<>component;splitStage="Decompose"<>component;
    refreshRecord=Lookup[Lookup[row,"ScalarCacheReorganize",<||>],component,<||>];
    If[Lookup[row["Status"],stage,""]=!="Complete" &&
      Lookup[row["Status"],splitStage,""]==="Complete" &&
      Lookup[row["Status"],integralStage,""]==="Complete" &&
      KeyExistsQ[row,integralStage] &&
      Lookup[refreshRecord,"Status",""]=!="ReorganizeComplete" &&
      RGZMXFastPVCount[row[integralStage]]>0,
      r=RGZMXRefreshIntegral[n,component,seconds];
      If[FailureQ[r],Return[r]]]];
  row=RGZMXMigrateRow[rgzMXJob["PerDiagram"][[n]]];states=row["Status"];
  contexts=row["StageEnvironment"];needs=RGZMXStageNeeds[];
  If[!KeyExistsQ[needs,stage],Return[Failure["StageName",<|"AvailableStage"->Keys[needs]|>]]];
  If[Lookup[states,stage,""]==="Complete",Return[<|"ID"->n,"Stage"->stage,"Status"->"Complete; skipped"|>]];
  (* Existing callers may still start at projection. Convert only THAT
     graph, then reload its checkpointed row before continuing. *)
  If[stage==="Project" && Lookup[states,"Convert",""]=!="Complete",
    r=RGZMXRun[n,"Convert",seconds];If[FailureQ[r],Return[r]];
    row=rgzMXJob["PerDiagram"][[n]];states=row["Status"];contexts=row["StageEnvironment"]];
  req=Select[needs[stage],Lookup[states,#,""]=!="Complete"&];
  If[req=!={},Return[Failure["RunPreviousStage",<|"MissingStage"->req|>]]];
  RGZMXProgressUpdate["Sector"->row["Category"],"DiagramID"->n,"DiagramCount"->Length[rgzMXJob["PerDiagram"]],
    "Stage"->stage,"Substep"->"Prepare","TermID"->0,"TotalTermCount"->0,"CompleteTermCount"->0,
    "Status"->"ComputeIn","StageStartTime"->AbsoluteTime[]];
  RGZMXReport["Verbose","number  ",n," / ",Length[rgzMXJob["PerDiagram"]]," Diagram: ",stage];
  {time,z}=AbsoluteTiming[CheckAbort[TimeConstrained[
    Switch[stage,
      "Convert",If[!KeyExistsQ[row,"OriginalFAAmplitude"],
        Failure["MissingRawAmplitude",<|"ID"->n|>],
        RGZMXConvertOne[row["OriginalFAAmplitude"],TrueQ[Lookup[row,"ScalarGhost",MemberQ[{1,4},row["UCategory"]]]],
          Lookup[row,"OriginalSumAudit",{}]]],
      "Project",RGZMXProject[row["Amplitude"],Sequence@@rgzMXJob["FieldPair"]],
      "DecomposeT",RGZMXSplit[row["Project"]["T"]],
      "DecomposeL",RGZMXSplit[row["Project"]["L"]],
      "IntegrateT",RGZMXIntegrate[row["DecomposeT"],
        FileNameJoin[{rgzMXJob["CacheDirectory"],"tid_units_v1"}],seconds],
      "IntegrateL",RGZMXIntegrate[row["DecomposeL"],
        FileNameJoin[{rgzMXJob["CacheDirectory"],"tid_units_v1"}],seconds],
      "ScalarT",RGZMXScalarReduce[row["IntegrateT"],
        FileNameJoin[{rgzMXJob["CacheDirectory"],"pv_units_v1"}],seconds],
      "ScalarL",RGZMXScalarReduce[row["IntegrateL"],
        FileNameJoin[{rgzMXJob["CacheDirectory"],"pv_units_v1"}],seconds]],seconds,$Aborted],$Aborted]];
  valid=If[stage==="Convert",AssociationQ[z] && RGZMXValidValueQ[z] &&
    RGZMXStageValueQ[stage,Lookup[z,"Amplitude",Missing[]]],RGZMXStageValueQ[stage,z]];
  If[!valid && z=!=$Aborted && z=!=$Failed && !FailureQ[z],
    z=Failure["InvalidStageResult",<|"Stage"->stage,"Cause"->z|>]];
  status=Which[z===$Aborted,"TimeoutOrAborted",z===$Failed,"ComputeFailed",
    FailureQ[z],"ComputeFailed",True,"Complete"];
  AssociateTo[states,stage->status];AssociateTo[row,"Status"->states];
  If[status==="Complete",
    AssociateTo[contexts,stage->context];AssociateTo[row,"StageEnvironment"->contexts];
    If[stage==="Convert",row=Join[row,z,<|"Convert"-><|"Status"->"Complete"|>|>],
      AssociateTo[row,stage->z]],
    AssociateTo[row,"FailedRecord"->Append[row["FailedRecord"],stage->z]]];
  all=rgzMXJob["PerDiagram"];all[[n]]=row;
  AssociateTo[rgzMXJob,"PerDiagram"->all];
  path=FileNameJoin[{rgzMXJob["CacheDirectory"],"graph_"<>ToString[n]<>".wl"}];
  r=RGZMXWrite[row,path];If[FailureQ[r],Return[r]];
  RGZMXProgressUpdate["Status"->status];
  RGZMXReport[If[status==="Complete","Verbose","Error"],
    "number  ",n," Diagram: ",stage,"；",status,"; ElapsedSeconds ",Round[time,0.01]," Seconds. "];
  result=<|"ID"->n,"Stage"->stage,"Status"->status,"ElapsedSeconds"->time|>;
  If[status==="Complete",result,Failure["StageStopped",Append[result,"Cause"->z]]]
];

RGZMXRunAll[stages_List:{"Project","DecomposeT","IntegrateT","DecomposeL","IntegrateL"},seconds_:300] := Module[{r,n,st,stop},Catch[
  If[!AssociationQ[rgzMXJob],Throw[Failure["StartFirst",<||>],stop]];
  Do[Do[r=RGZMXRun[n,st,seconds];If[FailureQ[r],Throw[r,stop]],{st,stages}],
    {n,Length[rgzMXJob["PerDiagram"]]}];RGZMXStatus[],stop]
];

RGZMXStatus[] := If[!AssociationQ[rgzMXJob],Failure["StartFirst",<||>],
  Grid[Prepend[Map[Function[row,Join[
    {row["ID"],row["Category"],row["UCategory"],Lookup[row,"FClass",0]},
    Lookup[row["Status"],{"Convert","Project","DecomposeT","IntegrateT","ScalarT","DecomposeL","IntegrateL","ScalarL"},"NotRun"]]],
    rgzMXJob["PerDiagram"]],{"ID","Category","UCategory","FClass","Convert","Project","DecomposeT","IntegrateT","ScalarT","DecomposeL","IntegrateL","ScalarL"}],
    Frame->All,Alignment->Left]];

RGZMXShow[n_Integer,stage_String:"Project"] := Module[{row},
  If[!AssociationQ[rgzMXJob],Return[Failure["StartFirst",<||>]]];
  If[n<1 || n>Length[rgzMXJob["PerDiagram"]],Return[$Failed]];
  row=rgzMXJob["PerDiagram"][[n]];
  Print["number  ",n," Diagram: ",row["Category"],"；",stage];
  TraditionalForm[Lookup[row,stage,Missing["NotRun"]]]
];

RGZMXReport["Verbose","Mixed-matrix stage engine loaded. Use RGZMXMonitor[command] for progress and RGZMXSetDisplay[\"Compact\"/\"Quiet\"/\"Verbose\"] for display mode."];

RGZMixSetDisplay[mode_String:"Compact"] := RGZMXSetDisplay[mode];

SetAttributes[RGZMixMonitor,HoldAll];

RGZMixMonitor[expr_] := RGZMXMonitor[expr];

RGZMixPrepare[] := Module[{root,hash,path,m,r,context},
  If[!StringQ[modelRGZ] || !And@@(FileExistsQ[modelRGZ<>#]&/@{".gen",".mod"}),
    Return[Failure["ModelPath",<|"Description"->"Set modelRGZ first."|>]]];
  root=Quiet[Check[RGZWorkflow`Private`cacheDirectory[],$Failed]];If[!StringQ[root],root=Directory[]];
  context={RGZMXIntegralContext[],Options[PaVeReduce],Options[FCFAConvert]};
  hash=IntegerString[Hash[{Import[modelRGZ<>".gen","Text"],
    Import[modelRGZ<>".mod","Text"],context,"mixed-matrix-complete-matter-v3",RGZWorkflow`Private`workflowFingerprint[],RGZWorkflow`Private`quarkMasses[]},"SHA256"],16];
  path=FileNameJoin[{root,"RGZ_Mixed_matrix_"<>StringTake[hash,12]}];
  If[AssociationQ[rgzMixOverallJob] && Lookup[rgzMixOverallJob,"Fingerprint",None]===hash &&
    Lookup[rgzMixOverallJob,"Directory",None]===path,Return[True]];
  If[!DirectoryQ[path],CreateDirectory[path,CreateIntermediateDirectories->True]];
  If[FileExistsQ[FileNameJoin[{path,"matrix_job.wl"}]],
    m=Get[FileNameJoin[{path,"matrix_job.wl"}]];
    If[!AssociationQ[m] || Lookup[m,"Fingerprint",None]=!=hash ||
      Lookup[m,"Version",0]=!=1 || !AssociationQ[Lookup[m,"Record",None]],
      Return[Failure["BadMatrixCache",<|"Directory"->path|>]]];rgzMixOverallJob=m,
    rgzMixOverallJob=<|"Version"->1,"Model"->modelRGZ,"Fingerprint"->hash,
      "Directory"->path,"LFieldOrder"->RGZMixBasis["L"],"TFieldOrder"->RGZMixBasis["T"],
      "Record"-><||>,"Convention"->"Matrix columns are all-incoming fields; FA outgoing particles are their antiparticles"|>;
    r=RGZMXWrite[rgzMixOverallJob,FileNameJoin[{path,"matrix_job.wl"}]];
    If[FailureQ[r],Return[r]]];True
];

RGZMixStart[] := Module[{r=RGZMixPrepare[]},If[FailureQ[r],r,
  <|"LFieldOrder"->RGZMixBasis["L"],"TFieldOrder"->RGZMixBasis["T"],
    "orderedFieldPairCount"->64,"CacheDirectory"->rgzMixOverallJob["Directory"],
    "Description"->"Initialize the matrix inventory without generating or converting all diagrams at once."|>]];

RGZMixRecordPath[pair_List,sector_String] := FileNameJoin[
  {rgzMixOverallJob["Directory"],"pair_"<>ToString[pair[[1]]]<>"_"<>
    ToString[pair[[2]]]<>"_"<>ToString[First@FirstPosition[RGZMixSectorNames,sector]]<>".wl"}];

RGZMixStore[pair_List,sector_String,entry_Association] := Module[{path,key,records,r,meta},
  path=RGZMixRecordPath[pair,sector];
  r=RGZMXWrite[Join[entry,<|"FieldPair"->pair,"Sector"->sector|>],path];
  If[FailureQ[r],Return[r]];
  key=StringRiffle[ToString/@pair,"_"]<>"_"<>sector;
  records=rgzMixOverallJob["Record"];
  meta=Join[KeyTake[entry,{"Status","DiagramCount","PerDiagramCache"}],<|"ResultFile"->path|>];
  AssociateTo[records,key->meta];AssociateTo[rgzMixOverallJob,"Record"->records];
  RGZMXWrite[rgzMixOverallJob,FileNameJoin[{rgzMixOverallJob["Directory"],"matrix_job.wl"}]]
];

RGZMixRead[pair_List,sector_String] := Module[{path=RGZMixRecordPath[pair,sector],z},
  If[!FileExistsQ[path],Return[Missing["NotRun",{pair,sector}]]];
  z=Get[path];If[AssociationQ[z] && Lookup[z,"FieldPair",None]===pair &&
    Lookup[z,"Sector",None]===sector&&
    (Lookup[z,"Status",None]=!="Complete"||(IntegerQ[Lookup[z,"DiagramCount",None]]&&z["DiagramCount"]>=0&&
     And@@(KeyExistsQ[z,#]&/@{"T","L"})&&!RGZWorkflow`Private`qqFFInvalidQ[{z["T"],z["L"]}])),z,Failure["BadPairCache",<|"File"->path|>]]];

RGZMixRun[n_Integer,stage_String,seconds_:300] := RGZMXRun[n,stage,seconds];

RGZMixShowGraph[n_Integer,stage_String:"ScalarT"] := RGZMXShow[n,stage];

(* A small 8 x 8 status grid; never put the full long expressions here. *)
RGZMixStatus[] := Module[{r,records,status,pairs,rows},
  r=RGZMixPrepare[];If[FailureQ[r],Return[r]];
  records=rgzMixOverallJob["Record"];
  status[i_,j_] := Module[{v,k},
    k=(ToString[i]<>"_"<>ToString[j]<>"_"<>#)&/@RGZMixSectorNames;
    v=Lookup[Lookup[records,k,<||>],"Status","NotRun"];
    Which[And@@(#==="Complete"&/@v),Style["Complete",Darker[Green]],
      MemberQ[v,"Incomplete"|"InitializeFailed"],Style["Stopped",Red],
      AllTrue[v,#==="NotRun"&],"NotRun",
      True,ToString[Count[v,"Complete"]]<>" / "<>ToString[Length[RGZMixSectorNames]]]];
  rows=Table[Prepend[Table[status[i,j],{j,8}],RGZMXLabel[i]],{i,8}];
  Grid[Prepend[rows,Prepend[RGZMixBasis["L"],"Row / Column"]],Frame->All,
    Alignment->Center,Spacings->{1,0.6},Background->{None,{LightGray}}]
];

(* Result assembly reports one ordered field pair at a time in the SAME
   monitor panel. Only display state changes; colour algebra, projection
   cache keys and original amplitude normalization remain unchanged. *)
RGZMixResultStep[label_String,pair_:None] := (
  RGZMXProgressUpdate["Substep"->If[ListQ[pair],
    StringRiffle[RGZMXLabel/@pair," / "]<>" · "<>
      If[MemberQ[{"T","L"},rgzMixProjectionComponent],
        rgzMixProjectionComponent<>" Coefficient · ",""]<>label,label],
    "OperationStartTime"->AbsoluteTime[]];Null);

RGZMixProjectedPair[pair_List,seconds_:60] := Module[{data,path,old,r},
  data=RGZMixReadPair[pair];If[Head[data]===Missing || FailureQ[data],Return[data]];
  path=FileNameJoin[{rgzMixOverallJob["Directory"],
    "adjoint_"<>StringRiffle[ToString/@pair,"_"]<>".wl"}];
  RGZMixResultStep["Checkhas AdjointProjectCache",pair];
  If[FileExistsQ[path],
    RGZMixResultStep["Readand VerifyAdjointProjectCache",pair];old=Get[path];
    If[AssociationQ[old] && SameQ[Lookup[old,"Input",None],data] &&
      Lookup[old,"Status",""]==="Complete",Return[old["Result"]]]];
  r=RGZMixProjectPair[data,pair,seconds];If[FailureQ[r],Return[r]];
  RGZMixResultStep["Save the adjoint-projection cache for the current field pair",pair];
  With[{w=RGZMXWrite[<|"Input"->data,"Status"->"Complete","Result"->r|>,path]},
    If[FailureQ[w],Return[w]]];r
];

(* Display bounded metadata only. Never typeset the full result or a long
   colour/integral expression when reporting an incomplete projection. *)
RGZMixResultReport[result_] := Module[{data,reasons,rows},
  Which[
    AssociationQ[result],Print["Summary complete; matrix dimensions: ",
      Dimensions /@ Lookup[result,{"T","L","FullColorTensorMatrix"}]],
    FailureQ[result],
      Print["SummaryFailedTag: ",result[[1]]];data=result[[2]];
      reasons=Lookup[data,"Cause",Lookup[data,"IncompleteOrFailed",{}]];
      If[!ListQ[reasons],reasons={reasons}];
      rows=Map[Function[e,If[FailureQ[e],
        {e[[1]],Lookup[e[[2]],"FieldPair","NotProvided"],
          Lookup[e[[2]],"TimeLimit","NotProvided"]},
        {Head[e],"NotProvided","NotProvided"}]],Take[reasons,UpTo[16]]];
      If[rows=!={},Print[Grid[Prepend[rows,{"FailedTag","FieldPair","TimeLimit"}],Frame->All]]];
      If[Length[reasons]>16,Print["also  ",Length[reasons]-16," TermNotdisplay. "]],
    True,Print["NotYetobtain ValidSummaryResult; head: ",Head[result]]];Null
];

RGZMixCompactData[expr_,seconds_:30] := Module[{r},
  r=CheckAbort[TimeConstrained[Module[{objects,vars,z,c,coeff,res,rows},
    objects=DeleteDuplicates@Cases[expr,x_ /; RGZMXPVObjectQ[x],{0,Infinity}];
    vars=Table[Unique["rgzMixPV$"],{Length[objects]}];
    z=expr/.Thread[objects->vars];c=z/.Thread[vars->0];
    coeff=Coefficient[z,#]&/@vars;
    res=Expand[z-c-Total[coeff vars]];
    If[!TrueQ[res===0],Return[Failure["NonlinearPVExpression",<||>]]];
    coeff=FactorTerms[Cancel[Together[#]]]&/@coeff;
    rows=Select[Transpose[{objects,coeff}],!TrueQ[#[[2]]===0]&];
    If[!TrueQ[c===0],AppendTo[rows,{1,FactorTerms[Cancel[Together[c]]]}]];
    If[rows==={},rows={{1,0}}];rows],seconds,$Aborted],$Aborted];
  If[r===$Aborted,Failure["CompactDisplayTimeout",<|"TimeLimit"->seconds|>],r]
];

RGZMixShow[a_,b_,component_String:"L",seconds_:30] := Module[{pair,data,z,rows,display},
  pair=RGZMixValidatePair[a,b];If[FailureQ[pair],Return[pair]];
  If[!MemberQ[{"T","L","TensorT","TensorL"},component],Return[Failure["Component",<||>]]];
  data=If[StringStartsQ[component,"Tensor"],RGZMixReadPair[pair],RGZMixProjectedPair[pair,seconds]];
  If[Head[data]===Missing || FailureQ[data],Return[data]];
  z=data[StringTake[component,-1]];
  rows=RGZMixCompactData[z,seconds];If[FailureQ[rows],Return[rows]];
  display={rgzLoopM1->Subscript[\[ScriptM],1],rgzLoopM2->Subscript[\[ScriptM],2],
    rgzLoopM3->Subscript[\[ScriptM],3],rgzLoopM4->Subscript[\[ScriptM],4]};
  Print[StringRiffle[RGZMXLabel/@pair," / "],"；",component,
    "; MasterIntegralAndCoefficient (Notadd  loop measure)"];
  Grid[Prepend[(TraditionalForm/@(#/.display))&/@rows,{"MasterIntegral","Coefficient"}],
    Frame->All,Alignment->Left,Spacings->{1.5,0.6},Background->{None,{LightGray}}]
];

RGZMXReport["Summary","Mixed-matrix workflow loaded. Initialize with RGZMixStart[]; compute one pair with RGZMixComputePair[\"A\",\"\[Xi]\",300]."];

RGZMXReport["Summary","All field pairs: RGZMixComputeAll[300]; status: RGZMixStatus[]; matrix: RGZMixMatrix[\"T\"/\"L\"/\"Tensor\"]."];
