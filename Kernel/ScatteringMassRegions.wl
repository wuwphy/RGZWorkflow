(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Source-bound small-regulator region matching. This separate audit never
   changes the generic-D mass-first amplitude or its self-energy caches. *)
$massRegionSourceSHA256=FileHash[$InputFileName,"SHA256"];
(* The M0 packets use squared-mass coordinates, unlike the physical
   amplitude coefficient reducer. Eliminate their discriminant exactly while
   retaining its principal square root in the inverse substitution. *)
qqMassRegionReduce[input_,method_:Automatic] := Module[{x=RGZWorkflow`Engine`qqM0X,y=RGZWorkflow`Engine`qqM0Y,z=RGZWorkflow`Engine`qqM0Z,
 base,roots,rules={},ratio,t=Unique["RGZWorkflow`Private`regionDiscriminant"],trans,result},
 If[LeafCount[input]<1000&&method=!= "Grouped",Return[Cancel[Together[input]]]];
 base=z-(x-y)^2/4;roots=DeleteDuplicates[Cases[input,Power[_,1/2],Infinity]];
 Do[ratio=Cancel[Together[root[[1]]/base]];
 If[MatchQ[ratio,_Integer|_Rational]&&TrueQ[ratio>0],AppendTo[rules,root->Sqrt[ratio] t]],{root,roots}];
 trans=If[rules==={},input,(input/.Dispatch[rules])/.z->t^2+(x-y)^2/4];
 If[method==="Grouped"&&FreeQ[trans,System`D],
  (* The existing grouped rational engine needs no additional algebraic
     relation here: its auxiliary mass variable is absent from the input.
     Map only the diagnostic dimension to its supported collection variable. *)
  qqProgressOperation["Collect mass-region rational numerator by denominator factors"];
  result=qqCoefficientSpectralGrouped[trans/.RGZResidual39Diagnostic`Private`dim->System`D,
   t,x,y,RGZWorkflow`Private`regionUnusedMass];
  If[result===$Failed,result=Cancel[Together[trans]],result=result/.System`D->RGZResidual39Diagnostic`Private`dim],
  result=Cancel[Together[trans]]];
 result/.t->Sqrt[base]];
qqMassRegionLinearPVQ[input_,masters_] := Module[{degree,stop=Unique["RGZWorkflow`Private`pvDegree"]},Catch[
 (* Inspect arithmetic structure without expanding parameter polynomials.
    CoefficientRules can expand millions of unrelated scalar leaves. *)
 degree[x_] := If[FreeQ[x,RGZResidual39Diagnostic`Private`a0|RGZResidual39Diagnostic`Private`b0],0,
 Which[MatchQ[x,RGZResidual39Diagnostic`Private`a0[_]|RGZResidual39Diagnostic`Private`b0[___]],1,
 Head[x]===Plus,Max[degree /@ List@@x],Head[x]===Times,Total[degree /@ List@@x],
 Head[x]===Power&&IntegerQ[x[[2]]]&&x[[2]]>=0,x[[2]] degree[x[[1]]],
 True,Throw[False,stop]]];degree[input]<=1,stop]];
qqMassRegionRecover[master_,values_List] := Module[{e=RGZResidual39Diagnostic`Private`e,taylor,lead,linear,c2,c1,checks,masters,other},
 If[Length[values]=!=2||!MatchQ[master,RGZResidual39Diagnostic`Private`b0[_,_,_]],Return[Failure["QQMassRegionInput",<||>]]];
 taylor=RGZResidual39Diagnostic`Private`pvTaylor[master];lead=taylor/.e->0;linear=Coefficient[Expand[taylor],e,1];
 If[!MatchQ[lead,RGZResidual39Diagnostic`Private`b0[_,_,_]],Return[Failure["QQMassRegionLeadingMaster",<||>]]];
 masters=DeleteDuplicates[Cases[{values,lead,linear},RGZResidual39Diagnostic`Private`a0[_]|RGZResidual39Diagnostic`Private`b0[___],Infinity]];
 If[!AllTrue[Join[values,{linear}],qqMassRegionLinearPVQ[#,masters]&],Return[Failure["QQMassRegionNonlinearPV",<||>]]];
 qqProgressOperation["Recover triangular bubble Laurent coefficients"];
 c2=Coefficient[values[[1]],lead];c1=Coefficient[values[[2]],lead]-c2 Coefficient[linear,lead];
 (* The lead coefficient equations hold by construction. Verify every other
    independent integral and the rational remainder separately, rather than
    merging a multi-million-leaf expression containing distinct integrals. *)
 other=DeleteCases[masters,lead];
 qqProgressOperation["Verify bubble reconstruction by independent integral coefficient"];
 checks=Join[qqMassRegionReduce /@ (values/.Dispatch[Thread[masters->0]]),
 Flatten[Table[{qqMassRegionReduce[Coefficient[values[[1]],pv]],qqMassRegionReduce[Coefficient[values[[2]],pv]-c2 Coefficient[linear,pv]]},{pv,other}]]];
 If[!AllTrue[checks,#===0&],Return[Failure["QQMassRegionReconstruction",<|"Differences"->checks|>]]];
 <|"CMinus2"->c2,"CMinus1"->c1,"NegativePowerReconstruction"->True|>];
qqMassRegionTerms[master_,c2_,c1_] := Module[
 {e=RGZResidual39Diagnostic`Private`e,d=RGZResidual39Diagnostic`Private`dim,s,a,b,small,other,lambda,kappa,h,g0,g1,regions={},hard=0,ba,bb,baa,bubble,tad,stop=Unique["RGZWorkflow`Private`regionTerms"]},Catch[
 If[!MatchQ[master,RGZResidual39Diagnostic`Private`b0[_,_,_]],Throw[Failure["QQMassRegionInput",<||>],stop]];
 {s,a,b}=List@@master;
 If[!FreeQ[s,e]||TrueQ[s===0],Throw[Failure["QQMassRegionExceptionalKinematics",<||>],stop]];
 Do[{small,other}=pair;If[FreeQ[small,e],Continue[]];lambda=Cancel[small/e];
 If[!FreeQ[lambda,e]||TrueQ[lambda===0],Throw[Failure["QQMassRegionNonlinearMass",<||>],stop]];
 If[FreeQ[other,e],h=s-other;If[TrueQ[h===0],Throw[Failure["QQMassRegionThreshold",<||>],stop]];
 g0=1/h;g1=lambda(-1/h^2+4s/(d h^3)),
 kappa=Cancel[other/e];If[!FreeQ[kappa,e],Throw[Failure["QQMassRegionNonlinearMass",<||>],stop]];
 g0=1/s;g1=(kappa-lambda+4lambda/d)/s^2];
 AppendTo[regions,<|"MassRatio"->lambda,"InverseM2UVNumerator"->lambda c2 g0,
 "FiniteUVNumerator"->lambda(c1 g0+c2 g1)|>],{pair,{{a,b},{b,a}}}];
 (* Both-small hard Taylor terms are already quadratic. A fixed mass zero
    belongs to that branch too; never insert A0(0)/0 in a mixed derivative. *)
 If[Xor[FreeQ[a,e],FreeQ[b,e]]&&!MemberQ[{a,b},0],
 {small,other}=If[FreeQ[a,e],{b,a},{a,b}];lambda=Cancel[small/e];h=s-other;
 bubble=RGZResidual39Diagnostic`Private`b0[s,0,other];tad=RGZResidual39Diagnostic`Private`a0[other];
 ba=((d-2)tad-(d-3)(s+other)bubble)/h^2;
 bb=((d-3)bubble-(d/2-1)tad/other)/(other-s);
 baa=(((6-d)other+(4-d)s)ba+2other bb)/h^2;hard=c2 lambda^2 baa/2];
 <|"SoftRegions"->regions,"HardQuadraticCorrection"->hard|>,stop]];
(* A0(lambda e) finite term at D=4-2 eps. Do not separate logs of complex
   products: only e>0 is factored from lambda, with its principal branch kept. *)
qqMassRegionFinite[uv_,lambda_] := Module[{e=RGZResidual39Diagnostic`Private`e,d=RGZResidual39Diagnostic`Private`dim},
 (uv/.d->4)(1-Log[e/RGZWorkflow`Parameters`muR2]-Log[lambda])-2(D[uv,d]/.d->4)];
(* Keep a compact source-bound view for retries: the full ledger repeats
   large finite expressions that region matching does not consume. *)
qqMassRegionLedgerView[l_Association] := Join[KeyTake[l,{"Key","AuditComplete","PairCount","PairsAudited","RegulatorTadpolesExpected","RegulatorTadpoles","SourceFingerprint","MixedSourceHash","ModelFingerprint","ModelContent","PreLimitPairsSHA256"}],
 <|"Rows"->(If[TrueQ[Lookup[#,"RegulatorTadpole",False]],KeyTake[#,{"RegulatorTadpole","MassRatio","LaurentFirstPoleNumerator"}],<|"RegulatorTadpole"->False|>]& /@ Lookup[l,"Rows",{}])|>];
qqMassRegionLedgerCompleteQ[l_Association] := Module[{rows=Lookup[l,"Rows",None],n=Lookup[l,"PairCount",None],tad},
 If[!ListQ[rows]||!AllTrue[rows,AssociationQ]||!IntegerQ[n]||n<=0||Length[rows]=!=n||Lookup[l,"PairsAudited",None]=!=n,Return[False]];
 tad=Count[rows,r_Association/;TrueQ[Lookup[r,"RegulatorTadpole",False]]];
 Lookup[l,"RegulatorTadpolesExpected",None]===tad&&Lookup[l,"RegulatorTadpoles",None]===tad];
qqMassRegionReadLedger[file_] := Module[{l,sha,side,key,old,r},
 If[file===Automatic,l=RGZResult["QQMassLimitLedger"];Return[If[AssociationQ[l],qqMassRegionLedgerView[l],l]]];
 If[!StringQ[file]||!FileExistsQ[file],Return[Missing["MassLedgerFile"]]];
 sha=FileHash[file,"SHA256"];key=Hash[{"QQMassRegionLedgerView/2",sha},"SHA256"];
 side=FileNameJoin[{cacheDirectory[],"qq-mass-region-inputs-"<>IntegerString[key,16,64]<>".wxf"}];
 old=If[FileExistsQ[side],Import[side,"WXF"],None];
 If[AssociationQ[old]&&Lookup[old,"Key",None]===key&&AssociationQ[Lookup[old,"Value",None]]&&qqAuditValidQ[old],Return[old["Value"]]];
 l=Import[file,"WXF"];If[!AssociationQ[l]||FileHash[file,"SHA256"]=!=sha,Return[Failure["QQMassRegionLedgerChanged",<||>]]];
 l=Join[qqMassRegionLedgerView[l],<|"EvidenceSHA256"->sha|>];r=atomicWXF[side,<|"Key"->key,"Value"->l|>];If[FailureQ[r],r,l]];
Options[RGZQQMassRegionMatch]={"JobFile"->Automatic,"MassLedgerFile"->Automatic,
 "ParameterPoint"->Automatic,"GaugeRoot"->Automatic,"CoefficientSeconds"->180};
RGZQQMassRegionMatch[seconds_:1200,OptionsPattern[]] := Module[
 {j,l,point=OptionValue["ParameterPoint"],root=OptionValue["GaugeRoot"],sec=OptionValue["CoefficientSeconds"],rules,default=qqAuditPoint[],pointTest,
 r,key,dir,indexKey,indexFile,index,masters,sourceSHA,expected,rows={},groups={},tadpoles={},out,file,z,master,cache,pv,record,lambda,uv,groupRows={},hard,hardParts,hardChecks={},summary,stopped=None,
 e=RGZResidual39Diagnostic`Private`e,d=RGZResidual39Diagnostic`Private`dim,stop=Unique["RGZWorkflow`Private`regionMatch"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,sec}),Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 If[!qqGaugeSymbolicParametersQ[],Return[Failure["SymbolicParametersRequired",<||>]]];
 pointTest=point=!=Automatic||root=!=Automatic;If[point===Automatic,point=default];
 If[!AssociationQ[point]||Sort[Keys[point]]=!=Sort[Keys[default]]||!qqAuditValidQ[Values[point]],Return[Failure["QQMassRegionParameterPoint",<||>]]];
 If[root=!=Automatic&&(!NumericQ[root]||!TrueQ[root>0]),Return[Failure["QQMassRegionPositiveGaugeRoot",<||>]]];
 rules=Thread[Values[default]->Lookup[point,Keys[default]]];If[root=!=Automatic,AppendTo[rules,RGZResidual39Diagnostic`Private`h->root]];
 j=qqAuditRead[OptionValue["JobFile"],"QQ"];l=qqMassRegionReadLedger[OptionValue["MassLedgerFile"]];
 If[!AllTrue[{j,l},AssociationQ]||!TrueQ[Lookup[l,"AuditComplete",False]]||!qqMassRegionLedgerCompleteQ[l]||Lookup[j,"Fingerprint",None]=!=Lookup[l,"SourceFingerprint",Missing[]]||
 Lookup[j,"MixedSourceHash",None]=!=Lookup[l,"MixedSourceHash",Missing[]]||Lookup[l,"ModelFingerprint",None]=!=modelKey[]||Lookup[l,"ModelContent",None]=!=modelContent[]||
 Lookup[j,"RegulatorMass",None]=!=0||!StringQ[Lookup[j,"Directory",None]],Return[Failure["QQMassRegionPrerequisites",<|"Action"->"Prepare the current QQ task and complete RGZQQMassLimitLedger, or select matching JobFile and MassLedgerFile."|>]]];
 r=engineGet["RGZ_Residual39_M0_Diagnostic.wl"];If[FailureQ[r],Return[r]];
 file=FileNameJoin[{j["Directory"],"self","pairs.wxf"}];If[!FileExistsQ[file],Return[Failure["QQSelfPairsRequired",<||>]]];
 sourceSHA=FileHash[file,"SHA256"];If[sourceSHA=!=Lookup[l,"PreLimitPairsSHA256",None],Return[Failure["QQMassRegionSourceMismatch",<||>]]];
 indexKey=Hash[{"QQMassLimitInputs/1",j["Fingerprint"],j["MixedSourceHash"],sourceSHA},"SHA256"];
 indexFile=FileNameJoin[{cacheDirectory[],"qq-mass-inputs-"<>IntegerString[indexKey,16,64]<>".wxf"}];
 If[!FileExistsQ[indexFile],Return[Failure["QQMassRegionInputIndexRequired",<|"Action"->"Run RGZQQMassLimitLedger to create the source-bound input index."|>]]];
 key=Hash[{"QQMassRegionMatch/1",sourceSHA,j["Fingerprint"],j["MixedSourceHash"],Hash[l,"SHA256"],point,root,$massRegionSourceSHA256},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-mass-regions-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQMassRegionMatch",Catch[
 qqProgressOperation["Read source-bound mass-region master index"];index=Import[indexFile,"WXF"];
 If[Lookup[index,"Key",None]=!=indexKey||!ListQ[Lookup[index,"Masters",None]],Throw[Failure["QQMassRegionSourceMismatch",<||>],stop]];
 masters=index["Masters"];Clear[index];If[Length[masters]=!=l["PairCount"],Throw[Failure["QQMassRegionInventoryMismatch",<||>],stop]];expected=Count[masters,FeynCalc`B0[___]?( !FreeQ[#,RGZWorkflow`Engine`M]& )];
 Do[master=Block[{RGZResidual39Diagnostic`Private`$parameterPoint=point},RGZResidual39Diagnostic`Private`pointExpression[masters[[n]]]]/.rules;
 If[!MatchQ[master,RGZResidual39Diagnostic`Private`b0[_,_,_]]||FreeQ[master,e],Continue[]];
 qqProgressUpdate[<|"Stage"->"QQMassRegionMatch:Bubbles","Index"->n,"Total"->Length[masters],"Master"->masters[[n]],"StepLimitSeconds"->sec|>];
 file=FileNameJoin[{j["Directory"],"self","m0-"<>ToString[n]<>".wxf"}];
 If[!FileExistsQ[file],Throw[Failure["QQMassRegionM0CheckpointRequired",<|"Index"->n|>],stop]];
 qqProgressOperation["Recover bubble negative powers and verify exact reconstruction"];
 record=qqAuditCheckpoint[FileNameJoin[{dir,"bubble-"<>ToString[n]<>".wxf"}],Hash[{key,n,FileHash[file,"SHA256"]},"SHA256"],
 cache=Import[file,"WXF"];
 If[Lookup[cache,"Key",None]=!=Hash[{j["Fingerprint"],j["MixedSourceHash"]},"SHA256"]||!MatchQ[Lookup[cache,"Value",None],{_,_,_}],Failure["QQMassRegionM0SourceMismatch",<|"Index"->n|>],
 pv=cache["Value"][[{1,2}]]/.rules;Clear[cache];z=qqMassRegionRecover[master,pv];
 If[FailureQ[z],z,pv=qqMassRegionTerms[master,z["CMinus2"],z["CMinus1"]];If[FailureQ[pv],pv,Join[<|"Index"->n,"Master"->master,"NegativePowerReconstruction"->True|>,pv]]]],sec];
 If[FailureQ[record],Throw[record,stop]];AppendTo[rows,record],{n,Length[masters]}];
 groups=Flatten[Lookup[rows,"SoftRegions",{}]];
 Do[If[!TrueQ[Lookup[l["Rows"][[n]],"RegulatorTadpole",False]],Continue[]];
 qqProgressOperation["Convert audited regulator tadpole to matching coordinates"];
 z=qqAuditCheckpoint[FileNameJoin[{dir,"tadpole-"<>ToString[n]<>".wxf"}],Hash[{key,"tadpole",n},"SHA256"],
 record=l["Rows"][[n]];
 uv=record["LaurentFirstPoleNumerator"]/.rules;lambda=record["MassRatio"]/.rules;
 <|"MassRatio"->lambda,"InverseM2UVNumerator"->Coefficient[Expand[uv,e],e,-1],"FiniteUVNumerator"->Coefficient[Expand[uv,e],e,0]|>,sec];
 If[FailureQ[z],Throw[z,stop]];If[AssociationQ[z],AppendTo[tadpoles,z]],{n,Length[l["Rows"]]}];
 groups=GatherBy[Join[groups,tadpoles],#["MassRatio"]&];
 Do[lambda=groups[[n,1]]["MassRatio"];qqProgressUpdate[<|"Stage"->"QQMassRegionMatch:Soft","Index"->n,"Total"->Length[groups],"StepLimitSeconds"->sec|>];
 qqProgressOperation["Match soft regions and tadpoles at fixed complex mass ratio"];
 z=qqAuditCheckpoint[FileNameJoin[{dir,"soft-"<>ToString[n]<>".wxf"}],Hash[{key,"soft",n},"SHA256"],
 uv={qqMassRegionReduce[Total[Lookup[groups[[n]],"InverseM2UVNumerator"]],"Grouped"],qqMassRegionReduce[Total[Lookup[groups[[n]],"FiniteUVNumerator"]],"Grouped"]};
 <|"MassRatio"->lambda,"InverseM2UVNumerator"->uv[[1]],"FiniteUVNumerator"->uv[[2]],"GenericDZero"->(uv==={0,0}),
 "FiniteLogarithmicCorrection"->qqMassRegionFinite[uv[[1]]/e+uv[[2]],lambda]|>,sec];
 If[FailureQ[z],Throw[z,stop]];AppendTo[groupRows,z],{n,Length[groups]}];
 qqProgressOperation["Match quadratic hard-region correction by independent integral structure"];
 hard=Total[Lookup[rows,"HardQuadraticCorrection",{}]];
 hardParts=DeleteDuplicates[Cases[hard,RGZResidual39Diagnostic`Private`a0[_]|RGZResidual39Diagnostic`Private`b0[___],Infinity]];
 Do[z=qqAuditCheckpoint[FileNameJoin[{dir,"hard-"<>ToString[n]<>".wxf"}],Hash[{key,"hard",n},"SHA256"],qqMassRegionReduce[Coefficient[hard,hardParts[[n]]],"Grouped"],sec];
 If[FailureQ[z],Throw[z,stop]];AppendTo[hardChecks,{hardParts[[n]],z}],{n,Length[hardParts]}];
 If[Cancel[Together[hard/.Thread[hardParts->0]]]=!=0,Throw[Failure["QQMassRegionHardRationalRemainder",<||>],stop]];True,stop],seconds];
 If[FailureQ[r],stopped=r];
 summary=stopped===None&&Length[rows]===expected&&AllTrue[groupRows,TrueQ[#["GenericDZero"]]&]&&AllTrue[hardChecks,Last[#]===0&];
 out=<|"Schema"->"RGZQQMassRegionMatch/1","Status"->Which[stopped=!=None,"MassRegionMatchStopped",summary,"RegionCorrectionsCancel",True,"RegionCorrectionsRemain"],
 "Key"->key,"AlgorithmSHA256"->$massRegionSourceSHA256,"MassLedgerSHA256"->Lookup[l,"EvidenceSHA256",Missing["InMemory"]],"SourceFingerprint"->j["Fingerprint"],"MixedSourceHash"->j["MixedSourceHash"],"PreLimitPairsSHA256"->sourceSHA,"ModelFingerprint"->modelKey[],"ModelContent"->modelContent[],
 "ParameterPoint"->point,"GaugeRoot"->root,"PointTest"->pointTest,"DimensionKeptSymbolic"->True,"SeriesDomain"->{e>0,RGZResidual39Diagnostic`Private`h>0},
 "BubblesExpected"->If[IntegerQ[expected],expected,Missing["IndexNotRead"]],"BubblesAudited"->Length[rows],"SoftRegions"->Total[Length[Lookup[#,"SoftRegions",{}]]& /@ rows],"SoftGroups"->groupRows,"HardCoefficients"->hardChecks,
 "RegionCancellationVerified"->summary,"GeneralParameterRegionCancellation"->(summary&&!pointTest),"MassLimitUVIRCertified"->False,"ScalelessLedgerComplete"->False,
 "CoefficientPoleBound"->"Inherited e^-2 bound from source M0 extraction; raw bubble coefficients not independently re-expanded",
 "Outstanding"->{"Independent raw-coefficient pole-bound and finite-limit completeness audit","Weighted pre-TID/pre-PV scaleless transfer coverage"},
 "Stopped"->stopped,"File"->FileNameJoin[{dir,"region-match.wxf"}]|>;
 z=atomicWXF[out["File"],out];If[FailureQ[z],Return[z]];storeResult["QQMassRegionMatch",out];
 If[FailureQ[stopped],Return[Failure[stopped[[1]],Join[stopped[[2]],<|"ReportFile"->out["File"],"Action"->"Repeat RGZQQMassRegionMatch; completed bubble and mass-ratio checkpoints are reused."|>]]]];
 <|"Status"->out["Status"],"Name"->"QQMassRegionMatch","Rows"->Length[rows],"Statistics"->KeyTake[out,{"SoftRegions","PointTest","RegionCancellationVerified","GeneralParameterRegionCancellation"}],"File"->out["File"]|>];
