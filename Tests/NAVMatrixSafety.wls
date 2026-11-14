(* SPDX-License-Identifier: GPL-3.0-or-later *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,root=DirectoryName[DirectoryName[$InputFileName]]},Get[FileNameJoin[{root,"RGZWorkflow.wl"}]];RGZWorkflow`RGZInitialize[CreateDirectory[]];RGZWorkflow`RGZTreeBenchmark[60];RGZWorkflow`RGZLoadFeynCalc[];RGZWorkflow`Private`loadLoopEngine[];Scan[RGZWorkflow`Private`engineGet,{"Algebra/MixedProjection.wl","Internal/GaugeRuntime.wl","Algebra/GaugeCoefficients.wl","OneLoop/NonAbelianVertex.wl"}];RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[]];
rgzMTChecks=<||>;rgzMTRec[name_,v_]:=(AssociateTo[rgzMTChecks,name->TrueQ[v]];Print[name," ",TrueQ[v]]);RGZMXSetDisplay["Quiet"];
rgzMTDesc=<|"VertexID"->1,"Fields"->{"A","A","A"},"LegOrder"->{1,2,3}|>;
rgzMTRec["UnknownFieldRejected",FailureQ[rgzNAVProp["Typo",q,a,b,c,d,e]]];
Do[rgzMTRec["SupportedField"<>f,!FailureQ[rgzNAVProp[f,q,a,b,c,d,e]]],{f,rgzNAVFields}];
Do[rgzMTRec["ScalarTransverseZero"<>f,rgzNAVProp[f,q,a,b,c,d,e,True]===0],{f,{"b","tau","xi"}}];
rgzMTRec["AuxiliaryRelativeSign",rgzNAVProp["phi",q,a,b,c,d,e]+rgzNAVProp["beta",q,a,b,c,d,e]===0];
Do[rgzMTRec["BadAttachInput"<>ToString[bad,InputForm],FailureQ[rgzNAVAttach[bad,0]]],{bad,{TerminatedEvaluation["IterationLimit"],Indeterminate,$TimedOut}}];
(* Synthetic vertex dispatch tests cache provenance without recomputing production diagrams. *)
Internal`InheritedBlock[{rgzNAVConnections,rgzNAVConnection,rgzNAVAttach,rgzFRFC},Clear[rgzNAVConnections,rgzNAVConnection,rgzNAVAttach,rgzFRFC];
 rgzNAVConnections[]:={rgzMTDesc};rgzFRFC[1]:=rgzMTVertex;rgzMTVertex=1;
 rgzNAVConnection[_,transverse_:False]:=If[transverse,2,1] rgzMTVertex;
 rgzNAVAttach[t_,mass_]:=t (1+mass);
 rgzMTDir=CreateDirectory[];rgzMTPacket=rgzNAVBuild[rgzMTDir,0,10];
 rgzMTSources=rgzNAVMatrixSources[{rgzMTDesc}];
 rgzMTRec["ValidBuild",RGZWorkflow`Private`navMatrixMatchesQ[rgzMTPacket,{rgzMTDesc},rgzMTSources,0]];
 Do[rgzMTRec["BadAggregate"<>key,!RGZWorkflow`Private`navPacketQ[Join[rgzMTPacket,<|key->999|>],"RGZNAVMatrix/1",{}]],
 {key,{"Tensor","TransverseTensor","Integrand","TransverseIntegrand","ConservedCurrentConnectionCount","ByExternalField"}}];
 rgzMTRec["MassMismatchRejected",!RGZWorkflow`Private`navMatrixMatchesQ[rgzMTPacket,{rgzMTDesc},rgzMTSources,1]];
 Internal`InheritedBlock[{rgzNAVConnection,rgzNAVAttach},Clear[rgzNAVConnection,rgzNAVAttach];
  rgzNAVConnection[___]:=Failure["UnexpectedRecompute",<||>];rgzNAVAttach[___]:=Failure["UnexpectedReattach",<||>];
  rgzMTRec["ValidCachesReused",AssociationQ[rgzNAVBuild[rgzMTDir,0,10]]]];
 rgzMTVertex=3;rgzMTNew=rgzNAVBuild[rgzMTDir,0,10];
 rgzMTRec["ChangedVertexRecomputed",AssociationQ[rgzMTNew]&&rgzMTNew["Tensor"]===3&&rgzMTNew["Integrand"]===3&&rgzMTNew["Connections"][[1]]["InputIdentity"]=!=rgzMTSources[[1]]];
 rgzMTNew=rgzNAVBuild[rgzMTDir,2,10];rgzMTRec["ChangedMassReattached",AssociationQ[rgzMTNew]&&rgzMTNew["Integrand"]===9];
 rgzMTFile=FileNameJoin[{rgzMTDir,"connection-1.wxf"}];rgzMTRow=Import[rgzMTFile,"WXF"];
 RGZWorkflow`Private`atomicWXF[rgzMTFile,Join[rgzMTRow,<|"Tensor"->999|>]];
 rgzMTNew=rgzNAVBuild[rgzMTDir,0,10];rgzMTRec["CorruptTensorRecomputed",AssociationQ[rgzMTNew]&&rgzMTNew["Tensor"]===3];
 RGZWorkflow`Private`atomicWXF[rgzMTFile,KeyDrop[rgzMTRow,{"ValidationPolicy","InputIdentity","TensorHash"}]];
 rgzMTRec["LegacyTensorRecomputed",AssociationQ[rgzNAVBuild[rgzMTDir,0,10]]&&KeyExistsQ[Import[rgzMTFile,"WXF"],"InputIdentity"]];
 rgzMTFile=FileNameJoin[{rgzMTDir,"attachment-1.wxf"}];rgzMTRow=Import[rgzMTFile,"WXF"];
 RGZWorkflow`Private`atomicWXF[rgzMTFile,Join[rgzMTRow,<|"Integrand"->999|>]];
 rgzMTNew=rgzNAVBuild[rgzMTDir,0,10];rgzMTRec["CorruptAttachmentRecomputed",AssociationQ[rgzMTNew]&&rgzMTNew["Integrand"]===3];
 Do[Internal`InheritedBlock[{rgzNAVAttach},Clear[rgzNAVAttach];rgzNAVAttach[___]:=bad;
  rgzMTTmp=CreateDirectory[];rgzMTNew=rgzNAVBuild[rgzMTTmp,0,10];
  rgzMTRec["FailedAttachmentNotSaved"<>ToString[bad,InputForm],FailureQ[rgzMTNew]&&!FileExistsQ[FileNameJoin[{rgzMTTmp,"result.wxf"}]]&&!FileExistsQ[FileNameJoin[{rgzMTTmp,"attachment-1.wxf"}]]]],
 {bad,{TerminatedEvaluation["IterationLimit"],Indeterminate,ComplexInfinity,$TimedOut,h[$Aborted]}}];
 Internal`InheritedBlock[{rgzNAVAttach},Clear[rgzNAVAttach];rgzNAVAttach[___]:=(Pause[1];1);
  rgzMTTmp=CreateDirectory[];rgzMTRec["AttachmentTimeout",FailureQ[rgzNAVBuild[rgzMTTmp,0,0.05]]&&!FileExistsQ[FileNameJoin[{rgzMTTmp,"result.wxf"}]]]];
 Internal`InheritedBlock[{rgzNAVConnection},Clear[rgzNAVConnection];rgzNAVConnection[___]:=Failure["UnexpectedTensorRecompute",<||>];
  rgzMTRec["ResumeAfterAttachmentTimeout",AssociationQ[rgzNAVBuild[rgzMTTmp,0,10]]]];
 Internal`InheritedBlock[{rgzNAVConnection},Clear[rgzNAVConnection];rgzNAVConnection[___]:=TerminatedEvaluation["IterationLimit"];
  rgzMTTmp=CreateDirectory[];rgzMTRec["InvalidTensorNotSaved",FailureQ[rgzNAVBuild[rgzMTTmp,0,10]]&&!FileExistsQ[FileNameJoin[{rgzMTTmp,"connection-1.wxf"}]]]];
 ];
Print["SUMMARY ",<|"Checks"->Length[rgzMTChecks],"Failed"->Keys[Select[rgzMTChecks,!TrueQ[#]&]]|>];Exit[If[And@@Values[rgzMTChecks],0,1]];
