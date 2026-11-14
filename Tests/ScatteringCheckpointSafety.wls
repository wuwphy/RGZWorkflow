(* SPDX-License-Identifier: GPL-3.0-or-later *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,root=DirectoryName[DirectoryName[$InputFileName]]},Get[FileNameJoin[{root,"RGZWorkflow.wl"}]];RGZWorkflow`RGZInitialize[CreateDirectory[]];RGZWorkflow`RGZTreeBenchmark[60];RGZWorkflow`RGZLoadFeynCalc[];RGZWorkflow`Private`loadLoopEngine[];Scan[RGZWorkflow`Private`engineGet,{"Algebra/MixedProjection.wl","Internal/GaugeRuntime.wl","Algebra/GaugeCoefficients.wl","Pinch/VertexWard.wl"}];RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[]];
rgzSTChecks=<||>;rgzSTRec[n_,v_]:=(AssociateTo[rgzSTChecks,n->TrueQ[v]];Print[n," ",TrueQ[v]]);RGZMXSetDisplay["Quiet"];
rgzSTDir=CreateDirectory[];rgzSTCount=0;rgzSTFunction[x_]:=(rgzSTCount++;x);
rgzSTSnap=RGZWorkflow`Private`qqSelfCheckpointInput[rgzSTFunction[2]];
rgzSTRec["SnapshotDoesNotExecute",rgzSTCount===0];
rgzSTRec["FirstCheckpoint",RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"basic",12,rgzSTFunction[2],10]===2&&rgzSTCount===1];
rgzSTRec["ValidReuseNoEvaluation",RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"basic",12,rgzSTFunction[2],20]===2&&rgzSTCount===1];
rgzSTRec["ChangedExpressionRecomputed",RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"basic",12,rgzSTFunction[999],10]===999&&rgzSTCount===2];
rgzSTValues={3,4};rgzSTI=1;
rgzSTRec["IndexedFirst",RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"index",12,rgzSTFunction[rgzSTValues[[rgzSTI]]],10]===3];
rgzSTI=2;rgzSTRec["ChangedIndexBound",RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"index",12,rgzSTFunction[rgzSTValues[[rgzSTI]]],10]===4];
rgzSTValues={3,8};rgzSTRec["ChangedListDataBound",RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"index",12,rgzSTFunction[rgzSTValues[[rgzSTI]]],10]===8];
rgzSTJob=<|"Value"->6|>;
rgzSTRec["AssociationSnapshot",RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"assoc",12,rgzSTFunction[rgzSTJob["Value"]],10]===6];
rgzSTJob=<|"Value"->9|>;
rgzSTRec["ChangedAssociationBound",RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"assoc",12,rgzSTFunction[rgzSTJob["Value"]],10]===9];
rgzSTCount=0;rgzSTCompound[]:=Module[{g},RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"compound",12,g=rgzSTFunction[{2,3}];Total[g],10]];
rgzSTRec["CompoundFirst",rgzSTCompound[]===5&&rgzSTCount===1];
rgzSTRec["CompoundModuleReuse",rgzSTCompound[]===5&&rgzSTCount===1];
rgzSTRec["BudgetDoesNotChangeInput",RGZWorkflow`Private`qqSelfCheckpointInput[rgzQQM0Term[{{1,2}},10]]===RGZWorkflow`Private`qqSelfCheckpointInput[rgzQQM0Term[{{1,2}},100]]];
rgzSTFile=FileNameJoin[{rgzSTDir,"basic.wxf"}];rgzSTPacket=Import[rgzSTFile,"WXF"];
RGZWorkflow`Private`atomicWXF[rgzSTFile,Join[rgzSTPacket,<|"Value"->777|>]];
rgzSTRec["CorruptValueRecomputed",RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"basic",12,rgzSTFunction[999],10]===999];
RGZWorkflow`Private`atomicWXF[rgzSTFile,<|"Key"->12,"Value"->777|>];
rgzSTRec["LegacyRecomputed",RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"basic",12,rgzSTFunction[999],10]===999&&Lookup[Import[rgzSTFile,"WXF"],"Schema",None]==="QQSelfCheckpoint/2"];
Do[rgzSTRec["InvalidInput"<>ToString[bad,InputForm],FailureQ[RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"bad",12,bad,10]]],
 {bad,{Indeterminate,TerminatedEvaluation["IterationLimit"],ComplexInfinity,h[$TimedOut]}}];
rgzSTRec["InvalidStageNotSaved",Internal`InheritedBlock[{rgzSTFunction},Clear[rgzSTFunction];rgzSTFunction[___]:=h[TerminatedEvaluation["IterationLimit"]];
 FailureQ[RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"badcomputed",12,rgzSTFunction[1],10]]&&!FileExistsQ[FileNameJoin[{rgzSTDir,"badcomputed.wxf"}]]]];
rgzSTHash=FileHash[rgzSTFile,"SHA256"];
Internal`InheritedBlock[{Import},Unprotect[Import];Clear[Import];Import[___]:=(Pause[0.15];<|"Key"->12,"Value"->999|>);
 rgzSTRec["ReadTimeout",MatchQ[RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"basic",12,rgzSTFunction[999],0.03],Failure["StageTimeout",_]]]];
rgzSTRec["ReadTimeoutPreservesFile",FileHash[rgzSTFile,"SHA256"]===rgzSTHash];
Internal`InheritedBlock[{RGZWorkflow`Private`atomicWXF},Clear[RGZWorkflow`Private`atomicWXF];RGZWorkflow`Private`atomicWXF[___]:=(Pause[0.15];True);
 rgzSTRec["WriteTimeout",MatchQ[RGZWorkflow`Private`qqSelfCheckpoint[rgzSTDir,"basic",12,rgzSTFunction[888],0.03],Failure["StageTimeout",_]]]];
rgzSTRec["WriteTimeoutPreservesFile",FileHash[rgzSTFile,"SHA256"]===rgzSTHash];
(* Exact spectral-coordinate and principal-root regressions. *)
rgzSTBase=qqM0Z-(qqM0X-qqM0Y)^2/4;
rgzSTExpressions={1/(qqM0X+qqM0Y)+1/(qqM0X-qqM0Y),(qqM0X^2-qqM0Y^2)/(qqM0X-qqM0Y),
 1/(qqM0X+I Sqrt[rgzSTBase])+2/(qqM0X-I Sqrt[rgzSTBase]),
 1/(qqM0X+I Sqrt[4rgzSTBase])^2+qqM0Z/(qqM0X-I Sqrt[rgzSTBase]),
 Sqrt[rgzSTBase]/(qqM0X+I Sqrt[rgzSTBase]),Sqrt[-rgzSTBase]/(qqM0X+1),
 Sqrt[1+Sqrt[rgzSTBase]]/(qqM0Y+1),1/(qqM0X+I Sqrt[rgzSTBase])^3-1/(qqM0X-I Sqrt[rgzSTBase])^3};
rgzSTPoints={{qqM0X->2,qqM0Y->1,qqM0Z->3},{qqM0X->3,qqM0Y->1,qqM0Z->-2},{qqM0X->1+I,qqM0Y->2-I,qqM0Z->3+2I}};
Do[rgzSTOut=RGZWorkflow`Private`qqSimplifyCoefficient[rgzSTExpressions[[i]]];
 rgzSTRec["SymbolicIdentity"<>ToString[i],FullSimplify[Together[rgzSTOut-rgzSTExpressions[[i]]]]===0];
 Do[rgzSTRec["BranchPoint"<>ToString[{i,j}],Abs[N[rgzSTOut/.rgzSTPoints[[j]],40]-N[rgzSTExpressions[[i]]/.rgzSTPoints[[j]],40]]<10^-25],{j,3}],{i,8}];
Print["SUMMARY ",<|"Checks"->Length[rgzSTChecks],"Failed"->Keys[Select[rgzSTChecks,!TrueQ[#]&]]|>];Exit[If[And@@Values[rgzSTChecks],0,1]];
