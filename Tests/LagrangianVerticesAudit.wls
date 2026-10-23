(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
testWork=Environment["RGZ_TEST_WORK"];
If[!StringQ[testWork]||testWork==="",testWork=FileNameJoin[{$TemporaryDirectory,"RGZWorkflow-integration-0.3.0"}]];
RGZInitialize[testWork];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
prop=RGZCheckPropagators[120];Print["FreshFRPropagators: ",prop];
vertexCount=Length[Import[FileNameJoin[{testWork,"Data","RGZ_all_raw.wxf"}],"WXF"]];
rows={};failed=False;
Do[r=RGZCheckVertices[60,"IDs"->{id}];If[FailureQ[r],Print[r];failed=True;Break[]];
 row=First[RGZResult["VerticesComparison"]];AppendTo[rows,row];
 Print["Vertex ",id," / ",vertexCount,": ",row["状态"],"; fields = ",row["外腿"]];
 If[!TrueQ[row["FA-FR"]===0],failed=True],{id,vertexCount}];
Export[FileNameJoin[{project,"output","review","Lagrangian-vertex-comparison.wxf"}],rows,"WXF"];
Print[<|"Checked"->Length[rows],"ExactZeroFAminusFR"->Count[Lookup[rows,"FA-FR"],0],"Statistics"->Counts[Lookup[rows,"状态"]]|>];
If[failed,Print["Nonzero or failed rows: ",KeyTake[#,{"编号","外腿","状态"}]& /@ Select[rows,!TrueQ[#["FA-FR"]===0]&]]];
Exit[If[!failed&&Length[rows]===vertexCount&&AssociationQ[prop]&&prop["Statistics"]===<|"Minkowski"-><|"通过"->69|>,"Kernel"-><|"通过"->69|>|>,0,1]];
