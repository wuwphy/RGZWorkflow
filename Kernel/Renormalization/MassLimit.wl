(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Match regulator tadpoles and bubble regions and assemble mass-limit evidence.

   Inputs:
   Computed QQ graphs or form factors, with the declared dimensional and mass-limit prescription.

   Dependencies:
   Algebra/ tools and already computed OneLoop/ results; no candidate PT task is required.
*)


(* Regulator tadpoles and bubble regions *)
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
 r=engineGet["Renormalization/RegulatorLimit.wl"];If[FailureQ[r],Return[r]];
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

(* Independent bounds on negative mass powers *)
(* Independent raw-coefficient pole bounds. No M0-series checkpoint is read. *)
$rawMassAuditSHA256=FileHash[$InputFileName,"SHA256"];
qqRawMassValuation[term_,e_] := Module[{factors,base,power,n,den,low=0,stop=Unique["RGZWorkflow`Private`rawValuation"]},Catch[
 factors=If[Head[term]===Times,List@@term,{term}];
 Do[If[FreeQ[f,e],Continue[]];{base,power}=If[Head[f]===Power&&MatchQ[f[[2]],_Integer|_Rational],List@@f,{f,1}];
 If[PolynomialQ[base,e],n=base;den=1,base=Together[base];n=Numerator[base];den=Denominator[base]];
 If[!PolynomialQ[n,e]||!PolynomialQ[den,e],Throw[Failure["QQRawMassNonPolynomial",<||>],stop]];
 If[n===0,Throw[Infinity,stop]];
 low+=power(Exponent[n,e,Min]-Exponent[den,e,Min]),{f,factors}];
 If[!IntegerQ[low],Throw[Failure["QQRawMassFractionalValuation",<|"Power"->low|>],stop]];low,stop]];
qqRawMassBound[co_,dir_,key_,seconds_] := Module[
 {e=RGZResidual39Diagnostic`Private`e,input,terms,low,minimum=Infinity,parts={},z,groups,checks={},file,r,stop=Unique["RGZWorkflow`Private`rawBound"]},Catch[
 If[!qqAuditValidQ[co],Throw[Failure["QQRawMassInvalidCoefficient",<||>],stop]];
 qqProgressOperation["Convert the original coefficient to squared-mass coordinates"];
 input=Block[{RGZResidual39Diagnostic`Private`$parameterPoint=qqAuditPoint[]},RGZResidual39Diagnostic`Private`pointExpression[co]];
 terms=If[Head[input]===Plus,List@@input,{input}];
 Block[{$qqMassSeriesSpecCache=<||>,$qqMassSeriesPowerCache=<||>,$qqMassSeriesSparse=True},
 Do[If[Mod[n,100]===1,qqProgressUpdate[<|"AlgebraGroupIndex"->n,"AlgebraGroupTotal"->Length[terms]|>]];
 low=qqRawMassValuation[terms[[n]],e];If[FailureQ[low],Throw[low,stop]];minimum=Min[minimum,low];
 If[low< -2,qqProgressOperation["Expand only raw terms below the allowed e^-2 bound"];
 z=qqAuditMassSeriesTerm[terms[[n]],e,-3];If[FailureQ[z],Throw[z,stop]];If[AssociationQ[z],AppendTo[parts,z]]],{n,Length[terms]}]];
 groups=Merge[parts,Total];
 Do[qqProgressOperation["Independently cancel raw coefficient below e^-2: power "<>ToString[power]];
 file=FileNameJoin[{dir,"below-"<>ToString[power]<>".wxf"}];
 r=qqAuditCheckpoint[file,Hash[{key,power},"SHA256"],qqMassRegionReduce[groups[power],"Grouped"],seconds];
 If[FailureQ[r],Throw[r,stop]];AppendTo[checks,<|"Power"->power,"Coefficient"->r,"Zero"->(r===0)|>],{power,Sort[Keys[groups]]}];
 <|"PoleBoundVerified"->AllTrue[checks,TrueQ[#["Zero"]]&],"TermCount"->Length[terms],"MinimumTermValuation"->If[minimum===Infinity,None,minimum],
 "AllowedMinimumPower"->-2,"BelowBoundChecks"->checks,"ReadsM0SeriesCheckpoints"->False|>,stop]];
(* Input-only shards are independent of the audit implementation. *)
qqRawMassIndex[j_,sourceSHA_] := Module[{key,dir,file,index,p,r,items={},stop=Unique["RGZWorkflow`Private`rawIndex"]},Catch[
 key=Hash[{"QQRawMassInputs/1",j["Fingerprint"],j["MixedSourceHash"],sourceSHA},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-raw-mass-inputs-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 file=FileNameJoin[{dir,"index.wxf"}];index=If[FileExistsQ[file],Import[file,"WXF"],None];
 If[AssociationQ[index]&&Lookup[index,"Key",None]===key&&ListQ[Lookup[index,"Items",None]]&&AllTrue[index["Items"],FileExistsQ[#["File"]]&],Return[index]];
 qqProgressOperation["Read original pre-limit pairs once and create independent input shards"];
 p=Import[FileNameJoin[{j["Directory"],"self","pairs.wxf"}],"WXF"];
 If[!AssociationQ[p]||Lookup[p,"Key",None]=!=Hash[{j["Fingerprint"],j["MixedSourceHash"]},"SHA256"]||!qqFFVectorPairsQ[Lookup[p,"Value",None]],Return[Failure["QQRawMassSourceMismatch",<||>]]];
 Do[file=FileNameJoin[{dir,"pair-"<>ToString[n]<>".wxf"}];r=atomicWXF[file,<|"Key"->Hash[{key,n},"SHA256"],"Pair"->p["Value"][[n]]|>];If[FailureQ[r],Throw[r,stop]];
 AppendTo[items,<|"Index"->n,"Master"->p["Value"][[n,1]],"File"->file,"SHA256"->FileHash[file,"SHA256"]|>],{n,Length[p["Value"]]}];
 index=<|"Key"->key,"SourceSHA256"->sourceSHA,"Items"->items|>;r=atomicWXF[FileNameJoin[{dir,"index.wxf"}],index];If[FailureQ[r],r,index],stop]];
Options[RGZQQRawMassPoleAudit]={"JobFile"->Automatic,"IDs"->All,"CoefficientSeconds"->180};
RGZQQRawMassPoleAudit[seconds_:3600,OptionsPattern[]] := Module[
 {j,r,index,sourceSHA,sourceFile,key,dir,ids=OptionValue["IDs"],sec=OptionValue["CoefficientSeconds"],rows={},z,packet,item,sub,out,stopped=None,stop=Unique["RGZWorkflow`Private`rawAudit"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,sec}),Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 If[!qqGaugeSymbolicParametersQ[],Return[Failure["SymbolicParametersRequired",<||>]]];
 j=qqAuditRead[OptionValue["JobFile"],"QQ"];
 If[!AssociationQ[j]||!And@@(KeyExistsQ[j,#]&/@{"Fingerprint","MixedSourceHash","Directory","ModelFingerprint","ModelContent"})||
 j["ModelFingerprint"]=!=modelKey[]||j["ModelContent"]=!=modelContent[]||Lookup[j,"RegulatorMass",None]=!=0,Return[Failure["QQRawMassJobRequired",<||>]]];
 r=engineGet["Renormalization/RegulatorLimit.wl"];If[FailureQ[r],Return[r]];
 sourceFile=FileNameJoin[{j["Directory"],"self","pairs.wxf"}];If[!FileExistsQ[sourceFile],Return[Failure["QQSelfPairsRequired",<||>]]];sourceSHA=FileHash[sourceFile,"SHA256"];
 key=Hash[{"QQRawMassPoleAudit/1",j["Fingerprint"],j["MixedSourceHash"],sourceSHA,$rawMassAuditSHA256,$massRegionSourceSHA256,$certificationSourceSHA256},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-raw-mass-audit-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQRawMassPoleAudit",Catch[
 index=qqRawMassIndex[j,sourceSHA];If[FailureQ[index],Throw[index,stop]];
 If[ids===All,ids=Range[Length[index["Items"]]]];
 If[!ListQ[ids]||ids==={}||!AllTrue[ids,IntegerQ[#]&&1<=#<=Length[index["Items"]]&]||!DuplicateFreeQ[ids],Throw[Failure["QQRawMassIDs",<||>],stop]];
 Do[item=index["Items"][[n]];qqProgressUpdate[<|"Stage"->"QQRawMassPoleAudit","Index"->n,"Total"->Length[index["Items"]],"Master"->item["Master"],"StepLimitSeconds"->sec|>];
 sub=FileNameJoin[{dir,"pair-"<>ToString[n]}];If[!DirectoryQ[sub],CreateDirectory[sub]];
 z=qqAuditCheckpoint[FileNameJoin[{sub,"bound.wxf"}],Hash[{key,n,item["SHA256"]},"SHA256"],
 If[FileHash[item["File"],"SHA256"]=!=item["SHA256"],Failure["QQRawMassInputChanged",<||>],
 packet=Import[item["File"],"WXF"];If[Lookup[packet,"Key",None]=!=Hash[{index["Key"],n},"SHA256"],Failure["QQRawMassInputChanged",<||>],qqRawMassBound[packet["Pair"][[2]],sub,Hash[{key,n},"SHA256"],sec]]],sec];
 If[FailureQ[z],Throw[z,stop]];AppendTo[rows,Join[<|"Index"->n,"Master"->item["Master"]|>,z]],{n,ids}];True,stop],seconds];
 If[FailureQ[r],stopped=r];
 out=<|"Schema"->"RGZQQRawMassPoleAudit/1","Status"->Which[stopped=!=None,"RawMassPoleAuditStopped",AllTrue[rows,TrueQ[#["PoleBoundVerified"]]&],"RawMassPoleBoundVerified",True,"HigherRawMassPoleFound"],
 "Key"->key,"AlgorithmSHA256"->$rawMassAuditSHA256,"DependencySHA256"->{$massRegionSourceSHA256,$certificationSourceSHA256},"SourceFingerprint"->j["Fingerprint"],"MixedSourceHash"->j["MixedSourceHash"],"ModelFingerprint"->modelKey[],"ModelContent"->modelContent[],"PreLimitPairsSHA256"->sourceSHA,
 "SupportedMassLimitMasters"->(AssociationQ[index]&&AllTrue[index["Items"],#["Master"]===1||MatchQ[#["Master"],FeynCalc`A0[_]|FeynCalc`B0[___]]&]),
 "RegulatorBubbleCount"->If[AssociationQ[index],Count[Lookup[index["Items"],"Master"],b_FeynCalc`B0/;!FreeQ[b,RGZWorkflow`Engine`M]],0],
 "ExpectedPairs"->If[AssociationQ[index],Length[index["Items"]],0],"PairsAudited"->Length[rows],"Rows"->rows,"PointTest"->False,"ReadsM0SeriesCheckpoints"->False,"RegulatorVariable"->"e=M^2 > 0; bound e^-2 is M^-4",
 "AllPairsWithinBound"->(stopped===None&&AssociationQ[index]&&Sort[ids]===Range[Length[index["Items"]]]&&AllTrue[rows,TrueQ[#["PoleBoundVerified"]]&]),
 "Stopped"->stopped,"File"->FileNameJoin[{dir,"raw-pole-audit.wxf"}]|>;
 z=atomicWXF[out["File"],out];If[FailureQ[z],Return[z]];storeResult["QQRawMassPoleAudit",out];
 If[FailureQ[stopped],Return[Failure[stopped[[1]],Join[stopped[[2]],<|"ReportFile"->out["File"],"Action"->"Repeat RGZQQRawMassPoleAudit; original-pair shards and completed bound checkpoints are reused."|>]]]];
 KeyTake[out,{"Status","PairsAudited","AllPairsWithinBound","File"}]];

(* Combining the mass-limit contributions *)
(* Link independently produced evidence into a NEW mass/scaleless ledger.
   No source result or boolean in an earlier artifact is edited. *)
$massCompletionSHA256=FileHash[$InputFileName,"SHA256"];
qqMassCompletionChecks[j_,l_,region_,raw_,weighted_,probe_,sigmaOK_,probeUsed_] := Module[{sameSource,sameMassSource,model},
 sameSource=AllTrue[{l,region,raw,weighted,probe},AssociationQ[#]&&Lookup[#,"SourceFingerprint",None]===Lookup[j,"Fingerprint",Missing[]]&];
 sameMassSource=AllTrue[{l,region,raw},Lookup[#,"PreLimitPairsSHA256",None]=!=None&&Lookup[#,"PreLimitPairsSHA256",None]===Lookup[l,"PreLimitPairsSHA256",Missing[]]&];
 model=AllTrue[{l,region,raw,weighted,probe},Lookup[#,"ModelFingerprint",None]===modelKey[]&&Lookup[#,"ModelContent",None]===modelContent[]&];
 <|"SameSource"->sameSource,"SameMassInput"->sameMassSource,"CurrentModel"->model,
 "SameMixedMatrix"->AllTrue[{l,region,raw,weighted},Lookup[#,"MixedSourceHash",None]===Lookup[j,"MixedSourceHash",Missing[]]&],
 "CompletedTadpoleInventory"->TrueQ[Lookup[l,"AuditComplete",False]]&&qqMassRegionLedgerCompleteQ[l],
 "IndependentRawBounds"->(TrueQ[Lookup[raw,"AllPairsWithinBound",False]]&&TrueQ[Lookup[raw,"SupportedMassLimitMasters",False]]&&Lookup[raw,"ReadsM0SeriesCheckpoints",None]===False&&Lookup[raw,"PointTest",None]===False&&
 Lookup[raw,"AlgorithmSHA256",None]===$rawMassAuditSHA256&&Lookup[raw,"DependencySHA256",None]==={$massRegionSourceSHA256,$certificationSourceSHA256}),
 "GeneralRegionMatch"->(TrueQ[Lookup[region,"GeneralParameterRegionCancellation",False]]&&Lookup[region,"PointTest",None]===False&&Lookup[region,"AlgorithmSHA256",None]===$massRegionSourceSHA256),
 "MatchingRegionInventory"->(Lookup[raw,"ExpectedPairs",None]===Lookup[l,"PairCount",Missing[]]&&Lookup[raw,"RegulatorBubbleCount",None]===Lookup[region,"BubblesExpected",Missing[]]&&Lookup[region,"BubblesExpected",None]===Lookup[region,"BubblesAudited",Missing[]]),
 "WeightedGraphAndPVCoverage"->(TrueQ[Lookup[weighted,"SourceGraphCoverageVerified",False]]&&TrueQ[Lookup[weighted,"PrePVCoverageVerified",False]]&&TrueQ[Lookup[weighted,"MatrixSourceMatchesQQ",False]]&&Lookup[weighted,"AlgorithmSHA256",None]===$weightedLedgerSHA256),
 "NoUnappliedMixedTransfer"->TrueQ[Lookup[weighted,"AllWeightedTransfersZero",False]],
 "CompleteProbeLedger"->(TrueQ[Lookup[probe,"CoreScanComplete",False]]&&Sort[Lookup[Lookup[probe,"Parts",{}],"Part",{}]]===Sort[{"AV1","AV2","Box1","Box2","NAV1","NAV2"}]),
 "ProbeLedgerUsedByMSbar"->TrueQ[probeUsed],"ConditionalExternalResidueInventory"->TrueQ[sigmaOK]|>];
Options[RGZQQCompleteMassLedger]={"JobFile"->Automatic,"MassLedgerFile"->Automatic,"RegionMatchFile"->Automatic,"RawPoleAuditFile"->Automatic,
 "WeightedLedgerFile"->Automatic,"ProbeLedgerFile"->Automatic,"MSbarFile"->Automatic,"UVIRFile"->Automatic};
RGZQQCompleteMassLedger[OptionsPattern[]] := Module[{r,j,l,region,raw,weighted,probe,m,u,checks,massOK,fullOK,out,key,file,sigma,sigmaExpected,residueConditions={},sigmaOK=False,probeUsed=False,missing},
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 j=qqAuditRead[OptionValue["JobFile"],"QQ"];l=qqMassRegionReadLedger[OptionValue["MassLedgerFile"]];
 region=qqAuditRead[OptionValue["RegionMatchFile"],"QQMassRegionMatch"];raw=qqAuditRead[OptionValue["RawPoleAuditFile"],"QQRawMassPoleAudit"];
 weighted=qqAuditRead[OptionValue["WeightedLedgerFile"],"QQWeightedUVIRLedger"];probe=qqAuditRead[OptionValue["ProbeLedgerFile"],"QQScalelessLedger"];
 m=qqAuditRead[OptionValue["MSbarFile"],"QQMSbar"];u=qqAuditRead[OptionValue["UVIRFile"],"QQUVIR"];
 If[!AssociationQ[weighted],weighted=<||>];If[!AssociationQ[probe],probe=<||>];If[!AssociationQ[m],m=<||>];If[!AssociationQ[u],u=<||>];
 If[!AllTrue[{j,l,region,raw},AssociationQ],Return[Failure["QQMassCompletionPrerequisites",<|"Action"->"Prepare the QQ task, completed tadpole ledger, general region match and independent raw-pole audit."|>]]];
 If[Lookup[j,"ModelContent",None]=!=modelContent[]||!AllTrue[{l,region,raw},Lookup[#,"ModelContent",None]===modelContent[]&],Return[Failure["QQMassCompletionModelContentMismatch",<||>]]];
 If[Lookup[j,"RegulatorMass",None]=!=0||Lookup[j,"Gauge",None]=!=0,Return[Failure["QQMassCompletionLandauM0Only",<||>]]];
 If[!qqGaugeSymbolicParametersQ[],Return[Failure["SymbolicParametersRequired",<||>]]];
 If[AssociationQ[m]&&AssociationQ[u]&&AssociationQ[probe]&&And@@(KeyExistsQ[m,#]&/@{"Key","FormFactorsSHA256","RenormalizationScaleSquared"})&&KeyExistsQ[u,"Key"]&&KeyExistsQ[probe,"Key"],
 probeUsed=Lookup[m,"SourceFingerprint",None]===j["Fingerprint"]&&Lookup[u,"SourceFingerprint",None]===j["Fingerprint"]&&
 m["Key"]===Hash[{"QQMSbar/3",u["Key"],probe["Key"],j["Fingerprint"],m["FormFactorsSHA256"],m["RenormalizationScaleSquared"],True,
 FileHash[FileNameJoin[{$packageRoot,"Kernel","Renormalization/MSbar.wl"}],"SHA256"]},"SHA256"]];
 r=engineGet["OneLoop/QuarkSelfEnergy.wl"];If[FailureQ[r],Return[r]];
 sigma=Lookup[j,"OnShellSigmaV",Missing[]];sigmaExpected=RGZWorkflow`Engine`rgzQQSigmaV0[0,0];
 If[qqAuditValidQ[sigma]&&SameQ[sigma,sigmaExpected],
 residueConditions=DeleteDuplicates[Cases[sigma,FeynCalc`A0[a_]:>Unequal[a,0],Infinity]];
 sigmaOK=residueConditions=!={}&&FreeQ[sigma,FeynCalc`B0|FeynCalc`PaVe|FeynCalc`TID|FeynCalc`FeynAmpDenominator]];
 missing=Keys[Select[<|"WeightedLedger"->weighted,"ProbeLedger"->probe,"MSbar"->m,"UVIR"->u|>,Length[#]===0&]];If[!KeyExistsQ[j,"OnShellSigmaV"],AppendTo[missing,"OnShellResidue"]];
 checks=qqMassCompletionChecks[j,l,region,raw,weighted,probe,sigmaOK,probeUsed];
 (* Model/source compatibility for the mass proof does not depend on whether
    the separate weighted/probe ledgers are already present. *)
 massOK=AllTrue[{l,region,raw},Lookup[#,"ModelFingerprint",None]===modelKey[]&&Lookup[#,"SourceFingerprint",None]===j["Fingerprint"]&&Lookup[#,"MixedSourceHash",None]===j["MixedSourceHash"]&]&&
 And@@Lookup[checks,{"SameMassInput","CompletedTadpoleInventory","IndependentRawBounds","GeneralRegionMatch","MatchingRegionInventory"}];
 fullOK=massOK&&And@@Values[checks];
 key=Hash[{"QQCompleteMassLedger/1",j["Fingerprint"],j["MixedSourceHash"],KeyTake[l,{"SourceFingerprint","MixedSourceHash","PreLimitPairsSHA256","PairCount","PairsAudited","RegulatorTadpolesExpected","RegulatorTadpoles"}],Lookup[region,"Key",None],Lookup[raw,"Key",None],Lookup[weighted,"Key",None],Lookup[probe,"Key",None],checks,$massCompletionSHA256},"SHA256"];
 out=Join[l,<|"Schema"->"RGZQQCompletedMassLedger/1","AlgorithmSHA256"->$massCompletionSHA256,"Key"->key,"Status"->Which[fullOK,"MassAndScalelessCoverageVerified",massOK,"MassProofVerifiedScalelessPending",True,"MassCompletionIncomplete"],"Checks"->checks,
 "MassLimitUVIRCertified"->massOK,"ScalelessLedgerComplete"->fullOK,"MissingEvidence"->missing,"FullSMatrixVerified"->False,
 "ExternalResidueConditions"->residueConditions,"Scope"->"Conditional Landau/M0 two-flavor massless qqprime; source-bound completion ledger, not an inclusive observable or all-parameter certificate.",
 "EvidenceFiles"->(Lookup[#,"File",None]& /@ {region,raw,weighted,probe,m,u}),
 "EvidenceSHA256"->(With[{f=Lookup[#,"File",None]},If[StringQ[f]&&FileExistsQ[f],FileHash[f,"SHA256"],None]]& /@ {region,raw,weighted,probe,m,u}),"OutstandingChecks"->Keys[Select[checks,!TrueQ[#]&]]|>];
 file=FileNameJoin[{cacheDirectory[],"qq-completed-mass-ledger-"<>IntegerString[key,16,64]<>".wxf"}];AssociateTo[out,"File"->file];
 r=atomicWXF[file,out];If[FailureQ[r],Return[r]];storeResult["QQMassLimitLedger",out];storeResult["QQMassCompletion",out];
 KeyTake[out,{"Status","MassLimitUVIRCertified","ScalelessLedgerComplete","MissingEvidence","Checks","OutstandingChecks","File"}]];
