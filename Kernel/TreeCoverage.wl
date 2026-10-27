(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
treeCoverageAlgorithmHash[backend_String] := Hash[{FileHash[FileNameJoin[{$packageRoot,"Kernel","TreeCoverage.wl"}],"SHA256"],FileHash[FileNameJoin[{backendDirectory[],backend}],"SHA256"]},"SHA256"];
treeExtraPacketQ[p_] := AssociationQ[p]&&Lookup[p,"Schema",None]==="RGZExtraTreeAudit/1"&&
 ListQ[Lookup[p,"Rows",None]]&&Length[p["Rows"]]===5&&
 Sort[Lookup[p["Rows"],"Field",None]]===Sort[{"eta","omega","zeta","Q1","Q2"}]&&
 AssociationQ[Lookup[p,"Checks",None]]&&Length[p["Checks"]]===15&&
 AllTrue[p["Rows"],AssociationQ[#]&&AssociationQ[Lookup[#,"Checks",None]]&&Length[#["Checks"]]===3&&
 AllTrue[Values[#["Checks"]],BooleanQ]&]&&AllTrue[Values[p["Checks"]],BooleanQ]&&
 p["Checks"]===Association[Flatten[Map[Function[row,KeyValueMap[(row["Field"]<>":"<>#1)->#2&,row["Checks"]]],p["Rows"]]]]&&
 Lookup[p,"AllChecksPassed",None]===AllTrue[Values[p["Checks"]],TrueQ];
treeCoveragePacketQ[p_] := AssociationQ[p]&&Lookup[p,"Schema",None]==="RGZOneLoopActionCoverage/1"&&
 ListQ[Lookup[p,"VertexInventory",None]]&&p["VertexInventory"]=!={}&&
 ListQ[Lookup[p,"ExportedFieldSignatures",None]]&&ListQ[Lookup[p,"VanishingActionSignatures",None]]&&
 AssociationQ[Lookup[p,"Checks",None]]&&Length[p["Checks"]]===8&&AllTrue[Values[p["Checks"]],BooleanQ]&&
 Lookup[p,"ActionVertexInventoryVerified",None]===AllTrue[Values[p["Checks"]],TrueQ];
(* Independent action-side audit. Does not modify production tree/loop packets. *)
RGZExtraTreeAudit[seconds_:120] := Module[{r,p,file},
 r=ready[];If[FailureQ[r],Return[r]];r=frReady[];If[FailureQ[r],Return[r]];
 $results=KeyDrop[$results,"ExtraTreeAudit"];Clear[RGZWorkflow`Engine`rgzExtraTreePacket];
 r=timed["ExtraTreeAudit",frEvaluation[engineGet["ExtraTreeAudit.wl",False]],seconds];If[FailureQ[r],Return[r]];
 p=RGZWorkflow`Engine`rgzExtraTreePacket;
 If[!treeExtraPacketQ[p],Return[Failure["ExtraTreeAuditInvalid",<||>]]];
 p=Join[p,<|"ModelFingerprint"->modelKey[],"Source"->"ActualFRQuadraticActionIndependentReference","AlgorithmHash"->treeCoverageAlgorithmHash["ExtraTreeAudit.wl"]|>];
 file=FileNameJoin[{dataDirectory[],"ExtraTreeAudit.wxf"}];p=Join[p,<|"File"->file|>];r=atomicWXF[file,p];If[FailureQ[r],Return[r]];storeResult["ExtraTreeAudit",p];
 If[!TrueQ[p["AllChecksPassed"]],Return[Failure["ExtraTreeMismatch",<|"Checks"->p["Checks"],"File"->file|>]]];
 <|"Status"->"Complete","Name"->"ExtraTreeAudit","Rows"->Length[p["Rows"]],"File"->file|>];
RGZCheckExtraPropagators[] := Module[{r,p,file},
 r=ready[];If[FailureQ[r],Return[r]];$results=KeyDrop[$results,"ExtraTreeAudit"];file=FileNameJoin[{dataDirectory[],"ExtraTreeAudit.wxf"}];
 If[!FileExistsQ[file],Return[Failure["ExtraTreeAuditRequired",<|"Action"->"Run RGZExtraTreeAudit[] in the FeynRules kernel first."|>]]];p=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!treeExtraPacketQ[p]||Lookup[p,"ModelFingerprint",None]=!=modelKey[]||Lookup[p,"Source",None]=!="ActualFRQuadraticActionIndependentReference"||Lookup[p,"AlgorithmHash",None]=!=treeCoverageAlgorithmHash["ExtraTreeAudit.wl"],Return[Failure["ExtraTreeSourceMismatch",<||>]]];
 If[!TrueQ[Lookup[p,"AllChecksPassed",False]]||!AssociationQ[Lookup[p,"Checks",None]]||!AllTrue[Values[p["Checks"]],TrueQ],Return[Failure["ExtraTreeMismatch",<|"File"->file|>]]];
 storeResult["ExtraTreeAudit",p];<|"Status"->"Complete","Name"->"ExtraTreeAudit","Rows"->Length[p["Rows"]],"File"->file,"FAPropagatorsIndependentlyCompared"->False|>];
RGZOneLoopCoverage[seconds_:300] := Module[{r,p,file,raw,meta},
 r=ready[];If[FailureQ[r],Return[r]];r=frReady[];If[FailureQ[r],Return[r]];
 $results=KeyDrop[$results,"OneLoopCoverage"];Clear[RGZWorkflow`Engine`rgzCoveragePacket];
 file=FileNameJoin[{dataDirectory[],"VerticesMetadata.wxf"}];If[!FileExistsQ[file],Return[Failure["ExportFRVerticesFirst",<||>]]];meta=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!AssociationQ[meta]||Lookup[meta,"ModelFingerprint",None]=!=modelKey[],Return[Failure["VertexDataModelMismatch",<||>]]];
 file=FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}];
 If[!FileExistsQ[file]||Lookup[meta,"VerticesSHA256",None]=!=FileHash[file,"SHA256"],Return[Failure["VertexDataIntegrityMismatch",<||>]]];
 raw=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!ListQ[raw]||raw==={}||qqFFInvalidQ[raw]||Length[raw]=!=Lookup[meta,"Count",None],Return[Failure["InvalidVertexData",<||>]]];
 RGZWorkflow`Engine`rgzCoverageExport=raw;
 r=timed["OneLoopActionCoverage",frEvaluation[engineGet["OneLoopCoverage.wl",False]],seconds];If[FailureQ[r],Return[r]];
 p=RGZWorkflow`Engine`rgzCoveragePacket;If[!treeCoveragePacketQ[p],Return[Failure["OneLoopCoverageInvalid",<||>]]];
 p=Join[p,<|"ModelFingerprint"->modelKey[],"AlgorithmHash"->treeCoverageAlgorithmHash["OneLoopCoverage.wl"],"VertexFileSHA256"->FileHash[FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}],"SHA256"]|>];
 file=FileNameJoin[{dataDirectory[],"OneLoopCoverage.wxf"}];p=Join[p,<|"File"->file|>];r=atomicWXF[file,p];If[FailureQ[r],Return[r]];storeResult["OneLoopCoverage",p];
 If[!TrueQ[p["ActionVertexInventoryVerified"]],Return[Failure["OneLoopActionCoverageMismatch",<|"Checks"->p["Checks"],"File"->file|>]]];
 <|"Status"->"ActionInventoryVerified","Name"->"OneLoopCoverage","Rows"->Length[p["VertexInventory"]],"GraphEnumerationIndependentlyVerified"->False,"File"->file|>];
RGZCheckOneLoopCoverage[] := Module[{r,p,file,vfile},
 r=ready[];If[FailureQ[r],Return[r]];$results=KeyDrop[$results,"OneLoopCoverage"];file=FileNameJoin[{dataDirectory[],"OneLoopCoverage.wxf"}];vfile=FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}];
 If[!FileExistsQ[file]||!FileExistsQ[vfile],Return[Failure["OneLoopCoverageRequired",<|"Action"->"Run RGZOneLoopCoverage[] after exporting FR vertices."|>]]];p=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!treeCoveragePacketQ[p]||Lookup[p,"ModelFingerprint",None]=!=modelKey[]||Lookup[p,"AlgorithmHash",None]=!=treeCoverageAlgorithmHash["OneLoopCoverage.wl"]||Lookup[p,"VertexFileSHA256",None]=!=FileHash[vfile,"SHA256"]||!TrueQ[Lookup[p,"ActionVertexInventoryVerified",False]],Return[Failure["OneLoopCoverageSourceMismatch",<||>]]];
 storeResult["OneLoopCoverage",p];<|"Status"->"Complete","Name"->"OneLoopCoverage","Rows"->Length[p["VertexInventory"]],"GraphEnumerationIndependentlyVerified"->False,"File"->file|>];
