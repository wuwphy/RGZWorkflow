(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Self-preparation IO only. Keep Kernel source fingerprints and all physical
   formulas unchanged, so an interrupted calculation can load this file live. *)
Begin["RGZWorkflow`Private`"];
If[!TrueQ[$selfPreparationResumeInstalled],
 $selfSnapshotNative=DownValues[qqSelfCheckpointInput];
 $selfPrepareNative=DownValues[RGZWorkflow`RGZQQPrepareSelf];
 DownValues[qqSelfCheckpointInputLegacy]=$selfSnapshotNative/.
  HoldPattern[qqSelfCheckpointInput]->qqSelfCheckpointInputLegacy;
 SetAttributes[qqSelfCheckpointInputLegacy,HoldAll];
 DownValues[qqPrepareSelfBeforeResume]=$selfPrepareNative/.
  HoldPattern[RGZWorkflow`RGZQQPrepareSelf]->qqPrepareSelfBeforeResume;
 Options[qqPrepareSelfBeforeResume]=Options[RGZWorkflow`RGZQQPrepareSelf];
 $selfPreparationResumeInstalled=True];

(* Index data, not algebra. In particular, do not hash all 25 entries for
   every matrix item merely because Part was inactivated with the algebra. *)
SetAttributes[qqSelfCheckpointInput,HoldAll];
qqSelfCheckpointInput[expression_] := qqSelfCheckpointInputLegacy[expression]/.
 HoldPattern[Inactive[Part][data_List,index_Integer]]:>data[[index]];

qqSelfValueChecked[p_,seconds_,name_] := Module[{r},
 qqProgressOperation["Validate cached result: "<>name];
 r=timed["QQSelfValidate:"<>name,!qqFFInvalidQ[p["Value"]],seconds];
 If[FailureQ[r]||!TrueQ[r],Return[If[FailureQ[r],r,False]]];
 qqProgressOperation["Hash cached result: "<>name];
 r=timed["QQSelfValueHash:"<>name,Hash[p["Value"],"SHA256"],seconds];
 If[FailureQ[r],r,Lookup[p,"ValueHash",None]===r]];
SetAttributes[qqSelfCheckpointBound,HoldAll];
qqSelfCheckpointBound[dir_,name_,key_,expression_,seconds_] := Module[
 {file,r,saved,input,identity,legacy,checked,valueHash},
 If[!StringQ[dir]||!StringQ[name]||!NumericQ[seconds]||!TrueQ[seconds>0],Return[Failure["QQSelfCheckpointArguments",<||>]]];
 file=FileNameJoin[{dir,name<>".wxf"}];
 If[StringMatchQ[name,"m0-sum-"~~DigitCharacter..],qqSelfProgress["M0Sum",ToExpression[StringDrop[name,7]],3]];
 qqProgressUpdate[<|"Checkpoint"->name,"CheckpointFile"->file,"StepLimitSeconds"->seconds,"CacheState"->"Checking"|>];
 qqProgressOperation["Select checkpoint input: "<>name];
 input=timed["QQSelfInput:"<>name,qqSelfCheckpointInput[expression],seconds];
 qqProgressOperation["Validate checkpoint input: "<>name];
 r=timed["QQSelfInputValidate:"<>name,qqFFInvalidQ[input],seconds];
 If[FailureQ[r],Return[r]];
 If[TrueQ[r],Return[Failure["QQSelfCheckpointInputInvalid",<|"Cause"->input|>]]];
 qqProgressOperation["Hash selected input: "<>name];
 identity=timed["QQSelfInputHash:"<>name,Hash[{"QQSelfCheckpoint/2",key,name,input,qqCalculationContext[],workflowFingerprint[]},"SHA256"],seconds];
 If[FailureQ[identity],Return[identity]];
 If[FileExistsQ[file],
  qqProgressOperation["Read completed checkpoint: "<>name];qqProgressUpdate[<|"CacheState"->"Reading"|>];
  r=timed["QQSelfRead:"<>name,Quiet[Check[Import[file,"WXF"],$Failed]],seconds];If[FailureQ[r],Return[r]];
  If[AssociationQ[r]&&Lookup[r,"Schema",None]==="QQSelfCheckpoint/2"&&Lookup[r,"Key",None]===key&&Lookup[r,"Name",None]===name&&KeyExistsQ[r,"Value"],
   checked=MemberQ[{Lookup[r,"InputIdentity",None],Lookup[r,"SelectedInputIdentity",None]},identity];
   If[!checked,
    qqProgressOperation["Validate original input identity once: "<>name];
    legacy=timed["QQSelfLegacyInputHash:"<>name,
     Hash[{"QQSelfCheckpoint/2",key,name,qqSelfCheckpointInputLegacy[expression],qqCalculationContext[],workflowFingerprint[]},"SHA256"],seconds];
    If[FailureQ[legacy],Return[legacy]];checked=Lookup[r,"InputIdentity",None]===legacy];
   If[TrueQ[checked],
    checked=qqSelfValueChecked[r,seconds,name];If[FailureQ[checked],Return[checked]];
    If[TrueQ[checked],
     (* Preserve the original identity and value. Add the verified compact
        identity so a later restart need not repeat the whole-list hash. *)
     If[Lookup[r,"InputIdentity",None]=!=identity&&Lookup[r,"SelectedInputIdentity",None]=!=identity,
      qqProgressOperation["Save verified selected-input identity: "<>name];
      saved=timed["QQSelfSelectedIdentity:"<>name,atomicWXF[file,Join[r,<|"SelectedInputIdentity"->identity|>]],seconds];If[FailureQ[saved],Return[saved]]];
     qqProgressUpdate[<|"OperationFinished"->AbsoluteTime[],"CacheState"->"Reused","ReusedCheckpoints"->(Lookup[$qqProgress,"ReusedCheckpoints",0]+1)|>];Return[r["Value"]]]]];
  qqProgressUpdate[<|"CacheState"->"RecomputeIncompatibleCheckpoint"|>]];
 qqProgressOperation["Compute checkpoint: "<>name];qqProgressUpdate[<|"CacheState"->"Computing"|>];
 r=timed["QQSelf:"<>name,expression,seconds];If[FailureQ[r],Return[r]];
 If[qqFFInvalidQ[r],Return[Failure["QQSelfCheckpointIncomplete",<|"Checkpoint"->name|>]]];
 qqProgressOperation["Hash computed result: "<>name];
 valueHash=timed["QQSelfResultHash:"<>name,Hash[r,"SHA256"],seconds];If[FailureQ[valueHash],Return[valueHash]];
 qqProgressOperation["Save completed checkpoint: "<>name];
 saved=timed["QQSelfWrite:"<>name,atomicWXF[file,<|"Schema"->"QQSelfCheckpoint/2","Key"->key,"Name"->name,"InputIdentity"->identity,"Value"->r,"ValueHash"->valueHash|>],seconds];
 If[FailureQ[saved],saved,qqProgressUpdate[<|"OperationFinished"->AbsoluteTime[],"CacheState"->"Saved","ComputedCheckpoints"->(Lookup[$qqProgress,"ComputedCheckpoints",0]+1)|>];r]];

(* A completed M0 result already precedes the remaining gauge-coefficient
   work. Use it instead of visiting every matrix checkpoint again. Copied
   packets must match the original, source-bound fixed-gauge calculation. *)
qqSelfFiniteResumeQ[j_,seconds_] := Module[{previous=RGZResult["QQBeforeGauge"],key,oldKey,dir,p,source,file,name,expected,checked,oldValid=False,stop=Unique["finiteResume"]},Catch[
 (* QQBeforeGauge is a saved workflow packet, not the QQ task itself. *)
 previous=If[AssociationQ[previous],Lookup[previous,"QQ",Missing["NoPreviousQQ"]],Missing["NoPreviousQQ"]];
 If[Lookup[j,"RegulatorMass",None]=!=0,Throw[False,stop]];
 key=Hash[{j["Fingerprint"],j["MixedSourceHash"]},"SHA256"];dir=FileNameJoin[{j["Directory"],"self"}];
 Do[name="m0-sum-"<>ToString[i];file=FileNameJoin[{dir,name<>".wxf"}];
  If[!FileExistsQ[file],Throw[False,stop]];
  qqProgressOperation["Check completed mass-limit sum "<>ToString[i]<>" / 3"];
  p=timed["QQSelfFiniteRead:"<>name,Quiet[Check[Import[file,"WXF"],$Failed]],seconds];
  If[FailureQ[p],Throw[p,stop]];
  If[!AssociationQ[p]||Lookup[p,"Key",None]=!=key||!KeyExistsQ[p,"Value"]||qqFFInvalidQ[p["Value"]],Throw[False,stop]];
  source=p;
  If[Lookup[p,"Schema",None]=!="QQSelfCheckpoint/2",
   If[!oldValid,
    If[!AssociationQ[previous]||Lookup[previous,"MixedSourceHash",None]=!=j["MixedSourceHash"]||Lookup[previous,"RegulatorMass",None]=!=0||
      Lookup[previous,"Fingerprint",None]=!=qqJobFingerprint[previous["Gauge"],0]||!qqPreparedSelfQ[previous],Throw[False,stop]];
    oldKey=Hash[{previous["Fingerprint"],previous["MixedSourceHash"]},"SHA256"];oldValid=True];
   expected=FileNameJoin[{previous["Directory"],"self",name<>".wxf"}];
   If[Lookup[p,"ReusedFrom",None]=!=expected,Throw[False,stop]];
   source=timed["QQSelfFiniteSource:"<>name,Quiet[Check[Import[expected,"WXF"],$Failed]],seconds];
   If[FailureQ[source],Throw[source,stop]];
   If[!AssociationQ[source]||Lookup[source,"Key",None]=!=oldKey||!KeyExistsQ[source,"Value"]||source["Value"]=!=p["Value"],Throw[False,stop]]];
  If[Lookup[source,"Schema",None]=!="QQSelfCheckpoint/2"||Lookup[source,"Name",None]=!=name||!IntegerQ[Lookup[source,"InputIdentity",None]],Throw[False,stop]];
  checked=qqSelfValueChecked[source,seconds,name];
  If[FailureQ[checked],Throw[checked,stop]];
  If[!TrueQ[checked],Throw[False,stop]];
  If[i<3&&p["Value"]=!=0,Throw[False,stop]],{i,3}];True,stop]];
Options[RGZWorkflow`RGZQQPrepareSelf]={"TotalSeconds"->3600};
RGZWorkflow`RGZQQPrepareSelf[seconds_:600,OptionsPattern[]] := engineEvaluation@Module[{j=RGZResult["QQ"],total=OptionValue["TotalSeconds"],r,saved,started=AbsoluteTime[],remaining,fallback,stop=Unique["selfResume"]},
 If[!NumericQ[seconds]||!TrueQ[seconds>0]||!NumericQ[total]||!TrueQ[total>0],Return[Failure["TimeLimit",<||>]]];
 (* A failed resume attempt and its fallback share one total budget. *)
 fallback[] := Module[{left=total-(AbsoluteTime[]-started)},
  If[TrueQ[left>0],
   TimeConstrained[qqPrepareSelfBeforeResume[seconds,"TotalSeconds"->left],left,Failure["QQSelfTotalTimeout",<|"Seconds"->total|>]],
   Failure["QQSelfTotalTimeout",<|"Seconds"->total|>]]];
 If[!AssociationQ[j]||Lookup[j,"Fingerprint",None]=!=qqJobFingerprint[j["Gauge"],j["RegulatorMass"]]||
   !KeyExistsQ[j,"MixedTransverse"]||Hash[j["MixedTransverse"],"SHA256"]=!=Lookup[j,"MixedSourceHash",None]||qqPreparedSelfQ[j],
  Return[fallback[]]];
 $qqProgress=<|"RunStarted"->AbsoluteTime[],"ReusedCheckpoints"->0,"ComputedCheckpoints"->0|>;
 qqSelfProgress["ResumeFiniteM0",0,3];
 remaining=total-(AbsoluteTime[]-started);
 If[!TrueQ[remaining>0],Return[fallback[]]];
 r=qqSelfMonitor[CheckAbort[TimeConstrained[Catch[
   r=qqSelfFiniteResumeQ[j,seconds];
   If[FailureQ[r],Throw[r,stop]];
   If[!TrueQ[r],Throw[Missing["NoCompletedM0"],stop]];
   qqProgressOperation["Resume gauge coefficients from completed mass-limit sums"];
   r=qqRepairSelfFromFinite[j,seconds];If[FailureQ[r],Throw[r,stop]];
   If[qqFFInvalidQ[r],Throw[Failure["QQSelfIncomplete",<||>],stop]];
   AssociateTo[j,{"MixedSelfEnergy"->r,"SelfPrepared"->True,"MixedSelfProof"->qqSelfProof[j,r]}];
   saved=atomicWXF[FileNameJoin[{j["Directory"],"job.wxf"}],j];If[FailureQ[saved],Throw[saved,stop]];
   invalidateQQDerived[];storeResult["QQ",j];
   <|"Status"->"Complete","Name"->"QQMixedSelf","ResumedFrom"->"CompletedM0","File"->FileNameJoin[{j["Directory"],"self","connected.wxf"}]|>,stop],
   remaining,Failure["QQSelfTotalTimeout",<|"Seconds"->total|>]],Failure["QQSelfAborted",<||>]],Null];
 If[MissingQ[r],Return[fallback[]]];
 qqSelfProgress[Lookup[$qqProgress,"Stage","ResumeFiniteM0"],Lookup[$qqProgress,"Index",0],Lookup[$qqProgress,"Total",0],If[FailureQ[r],"Stopped","Complete"]];
 If[FailureQ[r],Failure[r[[1]],Join[r[[2]],<|"Progress"->$qqProgress|>]],r]];

(* Monitor already makes its display dynamic; do not nest another Dynamic. *)
SetAttributes[qqSelfMonitor,HoldAll];
qqSelfMonitor[expression_,display_] := If[TrueQ[$Notebooks],
 Monitor[expression,Refresh[qqSelfProgressPanel[],UpdateInterval->1,TrackedSymbols:>{$qqProgress}]],expression];
End[];
