(* SPDX-License-Identifier: GPL-3.0-or-later *)
root=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{root,"RGZWorkflow.wl"}]];RGZInitialize[CreateDirectory[]];
checks=<||>;rec[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," ",TrueQ[v]]);
j=<|"Fingerprint"->1,"MixedTransverse"->{{2}},"MixedSourceHash"->Hash[{{2}},"SHA256"],"Gauge"->0,"RegulatorMass"->0,"SelfPrepared"->True,"MixedSelfEnergy"->3|>;
p=RGZWorkflow`Private`qqSelfProof[j,3];j=Append[j,"MixedSelfProof"->p];
rec["PreparedSelfBoundToSource",RGZWorkflow`Private`qqPreparedSelfQ[j]];
Do[rec["RejectChanged"<>field,!RGZWorkflow`Private`qqPreparedSelfQ[Append[j,field->99]]],{field,{"MixedTransverse","MixedSelfEnergy","Gauge","RegulatorMass","Fingerprint"}}];
rec["RejectUnboundLegacySelf",!RGZWorkflow`Private`qqPreparedSelfQ[KeyDrop[j,"MixedSelfProof"]]];
rec["RejectIncompleteSelf",!RGZWorkflow`Private`qqPreparedSelfQ[Append[j,"MixedSelfEnergy"->Indeterminate]]];
Block[{RGZWorkflow`Private`loadVertexEngine},RGZWorkflow`Private`loadVertexEngine[_]:=Null;
 rec["EmptyVerticesNotChecked",RGZCheckVertices[1,"IDs"->{}]["Status"]==="NotChecked"];
 rec["EmptyOrdersNotChecked",RGZCheckOrders[1,"IDs"->{}]["Status"]==="NotChecked"]];
rec["NestedAbortReportDoesNotIndexSentinel",Check[RGZReport[Failure["Outer",<|"Cause"->Failure["IntegralUnitStopped",<|"Cause"->$Aborted|>]|>]],$Failed]===Null];
Print[checks];Exit[If[And@@Values[checks],0,1]];
