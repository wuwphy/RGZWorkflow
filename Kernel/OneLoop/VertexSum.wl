(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Prepare the full conventional NAV tensor and its connection records.

   Inputs:
   The checked tree/model data and the graph or task data named by the called function.

   Dependencies:
   Tree/VertexRules.wl and NonAbelianVertex.wl supply the raw NAV. Packet validation
   also recognizes saved NAV pinch evidence for session restore; candidate extraction
   remains in Pinch/VertexWard.wl and is never called by RGZNAVSum.
*)


navMatrixDescriptorQ[r_] := AssociationQ[r]&&IntegerQ[Lookup[r,"VertexID",None]]&&r["VertexID"]>0&&
 MatchQ[Lookup[r,"Fields",None],{_String,_String,_String}]&&AllTrue[r["Fields"],MemberQ[navMatrixFields,#]&]&&
 Sort[Lookup[r,"LegOrder",{}]]==={1,2,3};
navMatrixConnectionQ[r_] := navMatrixDescriptorQ[r]&&
 And@@(KeyExistsQ[r,#]&/@{"Tensor","TransverseTensor"})&&!qqFFInvalidQ[r]&&
 Lookup[r,"ValidationPolicy",None]==="NAVMatrix/2"&&IntegerQ[Lookup[r,"InputIdentity",None]]&&
 Lookup[r,"TensorHash",None]===Hash[Lookup[r,{"Tensor","TransverseTensor"}],"SHA256"];
navMatrixAttachmentIdentity[row_,mass_] := Hash[{"NAVMatrixAttachment/2",row["InputIdentity"],row["TensorHash"],mass},"SHA256"];
navMatrixAttachmentQ[r_,n_,identity_] := AssociationQ[r]&&Lookup[r,"Connection",None]===n&&
 Lookup[r,"Identity",None]===identity&&And@@(KeyExistsQ[r,#]&/@{"Integrand","TransverseIntegrand"})&&
 !qqFFInvalidQ[r]&&Lookup[r,"PayloadHash",None]===Hash[Lookup[r,{"Integrand","TransverseIntegrand"}],"SHA256"];
navMatrixTotalsQ[p_] := Module[{rows=p["Connections"],attached=Lookup[p,"AttachmentRecords",None],mass=Lookup[p,"QuarkMass",Missing[]]},
 ListQ[attached]&&Length[attached]===Length[rows]&&!qqFFInvalidQ[mass]&&
 And@@MapIndexed[navMatrixAttachmentQ[#1,First[#2],navMatrixAttachmentIdentity[rows[[First[#2]]],mass]]&,attached]&&
 Lookup[p,"Tensor",None]===Total[Lookup[rows,"Tensor"]]&&
 Lookup[p,"TransverseTensor",None]===Total[Lookup[rows,"TransverseTensor"]]&&
 Lookup[p,"Integrand",None]===Total[Lookup[attached,"Integrand"]]&&
 Lookup[p,"TransverseIntegrand",None]===Total[Lookup[attached,"TransverseIntegrand"]]&&
 Lookup[p,"ConservedCurrentConnectionCount",None]===Count[Lookup[rows,"Fields"][[All,1]],Alternatives@@navMatrixVectors]&&
 Lookup[p,"ByExternalField",None]===Association@Table[f->Total[Lookup[Select[rows,First[#["Fields"]]===f&],"Tensor",{}]],{f,navMatrixFields}]];
navMatrixMatchesQ[p_,records_,sources_,mass_] := navPacketQ[p,"RGZNAVMatrix/1",{}]&&
 Lookup[p,"QuarkMass",None]===mass&&Length[records]===p["ConnectionCount"]&&
 (KeyTake[#,{"VertexID","Fields","LegOrder"}]&/@p["Connections"])===records&&
 Lookup[p["Connections"],"InputIdentity"]===sources;
navPacketQ[p_,schema_,fields_] := AssociationQ[p]&&Lookup[p,"Schema",None]===schema&&
 IntegerQ[Lookup[p,"ConnectionCount",None]]&&p["ConnectionCount"]>0&&
 ListQ[Lookup[p,"Connections",None]]&&Length[p["Connections"]]===p["ConnectionCount"]&&
 AllTrue[p["Connections"],AssociationQ]&&And@@(KeyExistsQ[p,#]&/@fields)&&!qqFFInvalidQ[p]&&
 Switch[schema,
 "RGZNAVMatrix/1",AllTrue[p["Connections"],navMatrixConnectionQ]&&navMatrixTotalsQ[p],
 "RGZNAVPinch/1",And@@MapIndexed[navPinchConnectionQ[#1,First[#2],Lookup[#1,"Fields",None]]&,p["Connections"]]&&
  KeyExistsQ[p,"Kernel"]&&p["Kernel"]===Total[Lookup[p["Connections"],"Kernel"]],
 _,False];
Options[RGZNAVSum]={"QuarkMass"->0};
RGZNAVSum[seconds_:300,OptionsPattern[]]:=engineEvaluation@Module[{r,key,dir,mass=OptionValue["QuarkMass"],records,sources},
 If[!NumericQ[seconds] || !TrueQ[seconds>0] || qqFFInvalidQ[mass],Return[Failure["NAVMatrixArguments",<||>]]];
 r=loadVertexEngine[seconds];If[FailureQ[r],Return[r]];
 r=engineGet["OneLoop/NonAbelianVertex.wl",False];If[FailureQ[r],Return[r]];
 key=IntegerString[Hash[{"RGZNAVMatrix/1",workflowFingerprint[],modelKey[],FileHash[FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}],"SHA256"],FileHash[FileNameJoin[{calculationDirectory[],"OneLoop/NonAbelianVertex.wl"}],"SHA256"],mass},"SHA256"],16,64];
 dir=FileNameJoin[{cacheDirectory[],"RGZ_NAV_matrix_"<>key}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 records=TimeConstrained[RGZWorkflow`Engine`rgzNAVConnections[],seconds,$TimedOut];
 If[!ListQ[records] || records==={} || !AllTrue[records,navMatrixDescriptorQ],Return[Failure["InvalidNAVConnections",<||>]]];
 sources=TimeConstrained[RGZWorkflow`Engine`rgzNAVMatrixSources[records],seconds,$TimedOut];
 If[!ListQ[sources] || !AllTrue[sources,IntegerQ],Return[Failure["NAVMatrixSourceStopped",<||>]]];
 r=If[FileExistsQ[FileNameJoin[{dir,"result.wxf"}]],Quiet[Check[Import[FileNameJoin[{dir,"result.wxf"}],"WXF"],$Failed]],Missing["NoCache"]];
 If[!navMatrixMatchesQ[r,records,sources,mass],r=RGZWorkflow`Engine`rgzNAVBuild[dir,mass,seconds]];
 If[FailureQ[r],Return[r]];If[!navMatrixMatchesQ[r,records,sources,mass],Return[Failure["NAVCacheInvalid",<|"Directory"->dir|>]]];storeResult["NAV",r];
 <|"Status"->"Complete","Name"->"NAVFullMatrix","Rows"->r["ConnectionCount"],"Statistics"-><|"ConservedCurrentConnections"->r["ConservedCurrentConnectionCount"],"Stage"->"Raw vertex integrand"|>,"File"->FileNameJoin[{dir,"result.wxf"}]|>
];

