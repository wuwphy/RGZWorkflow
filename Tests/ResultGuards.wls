(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Isolated fixtures: no FeynCalc or live Work required. *)
project=DirectoryName[DirectoryName[$InputFileName]];Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"rgz-result-guards-"<>CreateUUID[]}],"Display"->"Quiet"];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
old=<|"Fingerprint"->"old","Amplitude"->17|>;current=<|"Fingerprint"->"current","Amplitude"->23|>;
RGZWorkflow`Private`storeResult["QQ",<|"Fingerprint"->"current","Directory"->RGZWorkflow`Private`cacheDirectory[]|>];
RGZWorkflow`Private`storeResult["QQAmplitude",old];
record["RejectOtherTaskInMemory",FailureQ[RGZWorkflow`Private`qqFFReadPacket[Automatic]]];
RGZWorkflow`Private`storeResult["QQAmplitude",current];
record["AcceptSameTaskInMemory",RGZWorkflow`Private`qqFFReadPacket[Automatic]===current];
record["FailedPublicReadClearsPreviousFormFactors",Internal`InheritedBlock[{RGZWorkflow`Private`fcReady},
 Clear[RGZWorkflow`Private`fcReady];RGZWorkflow`Private`fcReady[]:=Null;
 RGZWorkflow`Private`storeResult["QQAmplitude",old];RGZWorkflow`Private`storeResult["QQFormFactors",<|"Old"->True|>];
 FailureQ[RGZQQFormFactors[5]]&&MatchQ[RGZResult["QQFormFactors"],_Missing]]];

file=FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],"bare-amplitude.wxf"}];Export[file,old,"WXF"];
record["ExplicitFileRemainsIndependent",RGZWorkflow`Private`qqFFReadPacket[file]===old];
RGZWorkflow`Private`$results=KeyDrop[RGZWorkflow`Private`$results,{"QQAmplitude"}];
record["RejectOtherTaskOnDisk",FailureQ[RGZWorkflow`Private`qqFFReadPacket[Automatic]]];
Export[file,current,"WXF"];record["AcceptSameTaskOnDisk",RGZWorkflow`Private`qqFFReadPacket[Automatic]===current];
RGZWorkflow`Private`storeResult["QQAmplitude",<|"Amplitude"->17|>];
record["MissingFingerprintCannotMatchActiveTask",FailureQ[RGZWorkflow`Private`qqFFReadPacket[Automatic]]];
RGZWorkflow`Private`$results=KeyDrop[RGZWorkflow`Private`$results,{"QQ"}];
record["StandaloneAmplitudeStillSupported",RGZWorkflow`Private`qqFFReadPacket[Automatic]===<|"Amplitude"->17|>];
RGZWorkflow`Private`storeResult["QQBeforeGauge",<|"QQ"-><|"Fingerprint"->"current"|>,"QQAmplitude"->current,"QQFormFactors"->Missing[]|>];
RGZQQGaugeRestore[];record["GaugeRestoreKeepsMatchingPacket",RGZWorkflow`Private`qqFFReadPacket[Automatic]===current];
pass=<|"编号"->1,"状态"->"FA=FR；未录入手推","FA-FR"->0,"FA-手推"->Missing["NotAvailable"]|>;
record["MissingHandReferenceAllowed",AssociationQ[RGZWorkflow`Private`treeComparisonSummary[{pass},"Vertex"]]];
record["NonzeroDifferenceRejected",FailureQ[RGZWorkflow`Private`treeComparisonSummary[{Join[pass,<|"FA-FR"->1|>]},"Vertex"]]];
record["FailedDifferenceRejected",FailureQ[RGZWorkflow`Private`treeComparisonSummary[{Join[pass,<|"FA-FR"->$Failed|>]},"Vertex"]]];
record["HandMismatchRejected",FailureQ[RGZWorkflow`Private`treeComparisonSummary[{Join[pass,<|"FA-手推"->1|>]},"Vertex"]]];
record["OrderZeroAccepted",AssociationQ[RGZWorkflow`Private`treeComparisonSummary[{<|"状态"->"通过","FA-FR"->0|>},"Order"]]];
record["OrderMismatchRejected",FailureQ[RGZWorkflow`Private`treeComparisonSummary[{<|"FA-FR"->badDifference|>},"Order"]]];
(* Exercise the public vertex entry with only backend calculation replaced. *)
vertexResult[row_] := Internal`InheritedBlock[{RGZWorkflow`Private`loadVertexEngine,RGZWorkflow`Engine`rgzCheckOne},
 Clear[RGZWorkflow`Private`loadVertexEngine,RGZWorkflow`Engine`rgzCheckOne];
 RGZWorkflow`Private`loadVertexEngine[_]:=Null;RGZWorkflow`Engine`rgzCheckOne[_]:=row;
 RGZWorkflow`Engine`rgzIDs={1};RGZCheckVertices[5,"IDs"->{1}]];
record["PublicVertexMismatchReturnsFailure",FailureQ[vertexResult[Join[pass,<|"FA-FR"->1|>]]]];
record["PublicVertexRowsPreservedForInspection",Length[RGZResult["VerticesComparison"]]===1];
record["PublicVertexPassStillComplete",vertexResult[pass]["Status"]==="Complete"];
(* Exercise the propagator entry without loading its physical backend. *)
Export[FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"Tree.wxf"}],
 <|"Source"->"FeynRulesQuadraticTerms","ModelFingerprint"->RGZWorkflow`Private`modelKey[],"KT"->0,"KL"->0|>,"WXF"];
propResult[diff_] := Internal`InheritedBlock[{RGZWorkflow`Private`checksReady,RGZWorkflow`Private`engineGet,RGZWorkflow`Engine`rgzState},
 Clear[RGZWorkflow`Private`checksReady,RGZWorkflow`Private`engineGet,RGZWorkflow`Engine`rgzState];
 RGZWorkflow`Private`checksReady[]:=Null;
 RGZWorkflow`Private`engineGet[_,_]:=(RGZWorkflow`Engine`rgzRows=ConstantArray[<|"模型减闵氏"->diff,"模型减二次核"->0|>,69];RGZWorkflow`Engine`rgzConnectionChecks=<|"contact"->0|>;Null);
 RGZWorkflow`Engine`rgzState[x_]:=If[x===0,"通过","未归零"];
 RGZCheckPropagators[5]];
record["PublicPropagatorMismatchReturnsFailure",FailureQ[propResult[1]]];
record["PublicPropagatorRowsPreserved",Length[RGZResult["Propagators"]]===69];
record["PublicPropagatorPassStillComplete",propResult[0]["Status"]==="Complete"];
orderResult[ok_,diff_,mode_] := Internal`InheritedBlock[{RGZWorkflow`Private`loadVertexEngine,RGZWorkflow`Private`engineGet,RGZWorkflow`Engine`rgzOrderOne,RGZWorkflow`Engine`rgzPermCompare,RGZWorkflow`Engine`rgzFieldName},
 Clear[RGZWorkflow`Private`loadVertexEngine,RGZWorkflow`Private`engineGet,RGZWorkflow`Engine`rgzOrderOne,RGZWorkflow`Engine`rgzPermCompare,RGZWorkflow`Engine`rgzFieldName];
 RGZWorkflow`Private`loadVertexEngine[_]:=Null;RGZWorkflow`Private`engineGet[_,_]:=Null;
 RGZWorkflow`Engine`rgzIDs={1};RGZWorkflow`Engine`rgzRaw={{{{testField,1},{testField,2},{testField,3}},0}};
 RGZWorkflow`Engine`rgzFieldName[_]:="A";
 RGZWorkflow`Engine`rgzOrderOne[_,_]:=<|"可生成"->ok,"状态"->"fixture"|>;
 RGZWorkflow`Engine`rgzPermCompare[_,_]:=<|"FA-FR"->diff,"状态"->"fixture"|>;
 RGZCheckOrders[5,"IDs"->{1},"Mode"->mode]];
record["GenerateFailureCannotReturnComplete",FailureQ[orderResult[False,0,"Generate"]]];
record["PublicOrderMismatchReturnsFailure",FailureQ[orderResult[True,1,"Compare"]]];
record["PublicOrderPassStillComplete",orderResult[True,0,"Compare"]["Status"]==="Complete"];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
