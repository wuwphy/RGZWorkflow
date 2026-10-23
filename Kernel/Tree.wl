(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Independent benchmark and actual FeynRules extraction are different results. *)
RGZTreeBenchmark[seconds_:60] := Module[{r,kt,kl,dt,dl,checks,s0,m0,u,g,a,mm,h,k,q,packet},
 h=g^2/2;k=s0-u^2;q=Sqrt[s0];
 kt={{m0^2-s0,h,h,-h,-h},{h,0,k,0,0},{h,k,0,0,0},{-h,0,0,0,k},{-h,0,0,k,0}};
 kl=ConstantArray[0,{8,8}];
 kl[[1]]={m0^2,I q,I q,I q m0^2,h,h,-h,-h};
 kl[[All,1]]={m0^2,-I q,-I q,-I q m0^2,h,h,-h,-h};
 kl[[2,2]]=a;kl[[3,4]]=kl[[4,3]]=s0;kl[[4,4]]=s0 m0^2-mm^4;
 Do[kl[[4,j+4]]=-I q kt[[1,j+1]];kl[[j+4,4]]=I q kt[[1,j+1]],{j,4}];
 kl[[5,6]]=kl[[6,5]]=kl[[7,8]]=kl[[8,7]]=k;
 r=timed["TreeBenchmark",dt=Map[Cancel,I Inverse[kt],{2}];dl=Map[Cancel,I Inverse[kl],{2}];
 checks={Map[Cancel[Together[#]]&,kt.dt-I IdentityMatrix[5],{2}],Map[Cancel[Together[#]]&,kl.dl-I IdentityMatrix[8],{2}],
 Cancel[dt[[1,1]]+I (s0-u^2)/((s0-m0^2)(s0-u^2)+g^4)],Cancel[dl[[1,1]]+I a s0/(s0^2+a mm^4)]};
 packet=<|"Source"->"IndependentExplicitKernel","Parameters"-><|"s"->s0,"m"->m0,"mu"->u,"mG"->g,"alpha"->a,"M"->mm|>,"KT"->kt,"KL"->kl,"DT"->dt,"DL"->dl,"Checks"->checks,"AA_T"->dt[[1,1]],"AA_L"->dl[[1,1]]|>;
 If[checks==={ConstantArray[0,{5,5}],ConstantArray[0,{8,8}],0,0},packet,Failure["TreeBenchmarkFailed",<|"Explanation"->"One or more exact kernel identities did not reduce to zero.","ChecksPassed"->AssociationThread[{"InverseT","InverseL","AA_T","AA_L"},(TrueQ[AllTrue[Flatten[{#}],SameQ[#,0]&]]& /@ checks)],"Checks"->checks|>]],seconds];
 If[FailureQ[r],Return[r]];storeResult["TreeBenchmark",r];<|"Status"->"Passed","Name"->"TreeBenchmark","Dimensions"->{{5,5},{8,8}}|>
];
RGZExtractTree[seconds_:120] := Module[{r,packet,file},
 r=ready[];If[FailureQ[r],Return[r]];r=frReady[];If[FailureQ[r],Return[r]];
 r=timed["ExtractTree",frEvaluation[engineGet["ExtractTree.wl",False]],seconds];If[FailureQ[r],Return[r]];
 packet=RGZWorkflow`Engine`rgzExtractedTree;
 If[!AssociationQ[packet]||Dimensions[packet["KT"]]=!={5,5}||Dimensions[packet["KL"]]=!={8,8},Return[Failure["InvalidExtractedKernel",<||>]]];
 If[packet["CheckT"]=!=ConstantArray[0,{5,5}]||packet["CheckL"]=!=ConstantArray[0,{8,8}],Return[Failure["ExtractedInverseFailed",<||>]]];
 packet=Join[packet,<|"Source"->"FeynRulesQuadraticTerms","ModelFingerprint"->modelKey[]|>];
 file=FileNameJoin[{dataDirectory[],"Tree.wxf"}];r=atomicWXF[file,packet];If[FailureQ[r],Return[r]];
 storeResult["Tree",packet];<|"Status"->"Complete","Name"->"Tree","Dimensions"->{{5,5},{8,8}},"File"->file|>
];
RGZExportVertices[seconds_:300] := Module[{r,vertices,file,metadata},
 r=ready[];If[FailureQ[r],Return[r]];r=frReady[];If[FailureQ[r],Return[r]];
 r=timed["ExportVertices",frEvaluation[engineCode["FeynmanRules[LTotal,FlavorExpand->True,MinParticles->2,MaxParticles->4,ConservedQuantumNumbers->False,ScreenOutput->False]"]],seconds];
 If[FailureQ[r],Return[r]];vertices=r;
 If[!ListQ[vertices]||!AllTrue[vertices,MatchQ[#,{_List,_}]&]||Length[vertices]===0,Return[Failure["InvalidFRVertices",<||>]]];
 file=FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}];r=atomicWXF[file,vertices];If[FailureQ[r],Return[r]];
 metadata=<|"ModelFingerprint"->modelKey[],"Count"->Length[vertices],"LegCounts"->Counts[Length[#[[1]]]&/@vertices]|>;
 r=atomicWXF[FileNameJoin[{dataDirectory[],"VerticesMetadata.wxf"}],metadata];If[FailureQ[r],Return[r]];
 storeResult["FRVerticesMetadata",metadata];<|"Status"->"Complete","Name"->"FRVertices","Rows"->Length[vertices],"Statistics"->metadata["LegCounts"],"File"->file|>
];
