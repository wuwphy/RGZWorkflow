(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* A finished comparison is successful only when its actual differences vanish.
   A missing independent handwritten reference is allowed; FA-FR is required. *)
treeComparisonSummary[rows_List,kind_String] := Module[{bad},
 bad=Select[rows,Function[row,!AssociationQ[row]||
  !TrueQ[Lookup[row,"FA-FR",Missing["Absent"]]===0]||
  (KeyExistsQ[row,"FA-Hand"]&&
   !MatchQ[row["FA-Hand"],_Missing]&&!TrueQ[row["FA-Hand"]===0])]];
 If[bad=!={},Failure[kind<>"ComparisonFailed",<|"Rows"->Length[rows],
  "Statistics"->Counts[Lookup[rows,"Status",Missing["Absent"]]],
  "Parts"->Lookup[bad,"ID",Missing["Absent"]],
  "Action"->"Inspect the stored comparison rows before continuing."|>],
 <|"Status"->"Complete","Rows"->Length[rows],"Statistics"->Counts[Lookup[rows,"Status"]]|>]];
(* Tree checks: no loop calculation is launched by these functions. *)
checksReady[] := engineEvaluation@Module[{r=ready[]},If[FailureQ[r],Return[r]];fcReady[]];
RGZCheckPropagators[seconds_:300] := engineEvaluation@Module[{r,tree,file,rows,stats},
 r=checksReady[];If[FailureQ[r],Return[r]];file=FileNameJoin[{dataDirectory[],"Tree.wxf"}];
 If[!FileExistsQ[file],Return[Failure["ExtractFRTreeFirst",<|"Action"->"RGZExtractTree[] in the FR kernel"|>]]];
 tree=Import[file,"WXF"];
 If[!AssociationQ[tree]||Lookup[tree,"Source",None]=!="FeynRulesQuadraticTerms"||Lookup[tree,"ModelFingerprint",None]=!=modelKey[],Return[Failure["TreeDataModelMismatch",<||>]]];
 RGZWorkflow`Engine`KT=tree["KT"];RGZWorkflow`Engine`KL=tree["KL"];
 r=timed["PropagatorComparison",engineGet["RGZ_PropagatorCheck.wl",False],seconds];If[FailureQ[r],Return[r]];
 rows=RGZWorkflow`Engine`rgzRows;
 If[!ListQ[rows]||Length[rows]=!=69,Return[Failure["InvalidPropagatorRows",<||>]]];
 stats=<|"Minkowski"->Counts[RGZWorkflow`Engine`rgzState/@Lookup[rows,"ModelMinusMinkowski"]],"Kernel"->Counts[RGZWorkflow`Engine`rgzState/@Lookup[rows,"ModelMinusQuadraticKernel"]]|>;
 storeResult["Propagators",rows];
 storeResult["PropagatorConnections",RGZWorkflow`Engine`rgzConnectionChecks];
 If[!AssociationQ[RGZWorkflow`Engine`rgzConnectionChecks] ||
  !AllTrue[Values[RGZWorkflow`Engine`rgzConnectionChecks],TrueQ[#===0]&],
  Return[Failure["PropagatorConnectionMismatch",<|"Checks"->RGZWorkflow`Engine`rgzConnectionChecks|>]]];
 If[!AllTrue[Join[Lookup[rows,"ModelMinusMinkowski"],Lookup[rows,"ModelMinusQuadraticKernel"]],TrueQ[#===0]&],
  Return[Failure["PropagatorComparisonFailed",<|"Statistics"->stats,"Action"->"Inspect RGZResult[\"Propagators\"] before continuing."|>]]];
 <|"Status"->"Complete","Rows"->Length[rows],"Statistics"->stats,
 "ConnectionChecks"-><|"Passed"->Length[RGZWorkflow`Engine`rgzConnectionChecks]|>|>
];
loadVertexEngine[seconds_] := engineEvaluation@Module[{r,file,rawFile,meta,vertices,digest},
 r=checksReady[];If[FailureQ[r],Return[r]];rawFile=FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}];
 If[!FileExistsQ[rawFile],Return[Failure["ExportFRVerticesFirst",<||>]]];
 file=FileNameJoin[{dataDirectory[],"VerticesMetadata.wxf"}];
 If[!FileExistsQ[file],Return[Failure["VertexMetadataMissing",<||>]]];meta=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!AssociationQ[meta]||Lookup[meta,"ModelFingerprint",None]=!=modelKey[],Return[Failure["VertexDataModelMismatch",<||>]]];
 digest=Quiet[Check[FileHash[rawFile,"SHA256"],$Failed]];
 If[!IntegerQ[Lookup[meta,"VerticesSHA256",None]]||digest=!=meta["VerticesSHA256"],
  Return[Failure["VertexDataIntegrityMismatch",<|"Action"->"Re-export vertices with RGZExportVertices[]; raw data and metadata must come from the same export."|>]]];
 vertices=Quiet[Check[Import[rawFile,"WXF"],$Failed]];
 If[!ListQ[vertices]||vertices==={}||!AllTrue[vertices,MatchQ[#,{_List,_}]&]||qqFFInvalidQ[vertices],
  Return[Failure["InvalidFRVertexData",<||>]]];
 If[Lookup[meta,"Count",None]=!=Length[vertices]||!AssociationQ[Lookup[meta,"LegCounts",None]]||
  KeySort[meta["LegCounts"]]=!=KeySort[Counts[Length[#[[1]]]&/@vertices]],
  Return[Failure["VertexMetadataMismatch",<||>]]];
 timed["LoadVertexDefinitions",engineGet["VertexDefinitions.wl",False],seconds]
];
selectedIDs[value_] := engineEvaluation@Module[{ids=If[value===All,RGZWorkflow`Engine`rgzIDs,value]},
 If[!ListQ[ids]||!AllTrue[ids,IntegerQ[#]&&MemberQ[RGZWorkflow`Engine`rgzIDs,#]&],Failure["VertexIDs",<|"Action"->"Use \"IDs\" -> All (without braces), or an integer list such as {1}."|>],DeleteDuplicates[ids]]];
Options[RGZCheckVertices]={"IDs"->All};
RGZCheckVertices[seconds_:60,OptionsPattern[]] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
engineEvaluation@Module[{r,ids,rows={},id},
 r=loadVertexEngine[seconds];If[FailureQ[r],Return[r]];ids=selectedIDs[OptionValue["IDs"]];If[FailureQ[ids],Return[ids]];
 Do[r=timed["Vertex:"<>ToString[id],RGZWorkflow`Engine`rgzCheckOne[id],seconds];
 If[FailureQ[r]||!AssociationQ[r],storeResult["VerticesComparison",rows];Throw[Failure["VertexStopped",<|"ID"->id,"Completed"->Length[rows],"Cause"->r|>],rgzFunctionReturnTag]];AppendTo[rows,r],{id,ids}];
 RGZWorkflow`Engine`rgzResults=rows;storeResult["VerticesComparison",rows];
 treeComparisonSummary[rows,"Vertex"]
],rgzFunctionReturnTag]];
Options[RGZCheckOrders]={"IDs"->All,"Mode"->"Compare"};
RGZCheckOrders[seconds_:60,OptionsPattern[]] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
engineEvaluation@Module[{r,ids,mode,generated={},compared={},id,names,orders,order,n},
 mode=OptionValue["Mode"];If[!MemberQ[{"Generate","Compare"},mode],Return[Failure["OrderMode",<||>]]];
 r=loadVertexEngine[seconds];If[FailureQ[r],Return[r]];ids=selectedIDs[OptionValue["IDs"]];If[FailureQ[ids],Return[ids]];
 r=engineGet["OrderDefinitions.wl",False];If[FailureQ[r],Return[r]];
 Do[names=RGZWorkflow`Engine`rgzFieldName/@RGZWorkflow`Engine`rgzRaw[[id,1,All,1]];orders=DeleteDuplicates[Permutations[names]];
 Do[r=timed["OrderGenerate:"<>ToString[id],RGZWorkflow`Engine`rgzOrderOne[id,order],seconds];
 If[FailureQ[r]||!AssociationQ[r],storeResult["OrdersGenerated",generated];Throw[Failure["OrderGenerationStopped",<|"ID"->id,"Cause"->r|>],rgzFunctionReturnTag]];AppendTo[generated,r],{order,orders}],{id,ids}];
 RGZWorkflow`Engine`rgzOrderResults=generated;storeResult["OrdersGenerated",generated];
 If[!AllTrue[generated,TrueQ[Lookup[#,"Generatable",False]]&],Return[Failure["OrdersNotAllGenerated",<|"Statistics"->Counts[Lookup[generated,"Status"]]|>]]];
 If[mode==="Generate",Return[<|"Status"->"Complete","Rows"->Length[generated],"Statistics"->Counts[Lookup[generated,"Status"]]|>]];
 r=engineGet["PermutationDefinitions.wl",False];If[FailureQ[r],Return[r]];RGZWorkflow`Engine`rgzPermTimeLimit=seconds;
 Do[n=Length[RGZWorkflow`Engine`rgzRaw[[id,1]]];
 Do[r=timed["OrderCompare:"<>ToString[id],RGZWorkflow`Engine`rgzPermCompare[id,order],seconds+1];
 If[FailureQ[r]||!AssociationQ[r],storeResult["OrdersCompared",compared];Throw[Failure["OrderComparisonStopped",<|"ID"->id,"Cause"->r|>],rgzFunctionReturnTag]];AppendTo[compared,r],{order,Permutations[Range[n]]}],{id,ids}];
 storeResult["OrdersCompared",compared];treeComparisonSummary[compared,"Order"]
],rgzFunctionReturnTag]];
