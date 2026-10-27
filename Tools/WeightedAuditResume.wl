(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Resumable weighted assembly. The audit algebra and its unit/graph keys are
   unchanged. Only new assembly-prefix files depend on this scheduling source.
   No source graph file or verification flag is rewritten. *)
Begin["RGZWorkflow`Private`"];
$weightedResumeSHA256=FileHash[$InputFileName,"SHA256"];
(* Contracted scalar numerators with one or two simple propagators admit an
   exact generic-D reduction by Q=q.q, P=p.q, D0=Q-a,
   D1=Q-2P+s-b. All pinched terms have at most one simple propagator;
   polynomial DR integrals and simple massless tadpoles have no logarithmic
   scaleless UV/IR pole at D=4. Match the COMPLETE D-dimensional result to
   the source before concluding that no transfer was discarded. *)
qqWeightedScalarCanonical[x_] := Module[{objects},
 objects=DeleteDuplicates[Cases[x,t:(_FeynCalc`PaVe|_FeynCalc`A0|_FeynCalc`B0),{0,Infinity}]];
 x/.Dispatch[(#->qqPVScalarCanonical[#])&/@objects]];
qqWeightedExactScalarInput[x_] := Module[{q=RGZWorkflow`Engine`q,p=RGZWorkflow`Engine`p,qm,pm,s,fads,fad,props,t=Unique["den"],u=Unique["Q"],v=Unique["P"],poly,num,order},
 qm=FeynCalc`Momentum[q,D];pm=FeynCalc`Momentum[p,D];s=FeynCalc`Pair[pm,pm];
 If[s===0||qqFFInvalidQ[s],Return[Missing["ExceptionalInvariant"]]];
 fads=Cases[x,z_FeynCalc`FeynAmpDenominator:>z,{0,Infinity}];If[Length[fads]=!=1,Return[Missing["Topology"]]];
 fad=First[fads];props=List@@fad;
 If[!MemberQ[{1,2},Length[props]]||!AllTrue[props,MatchQ[#,_FeynCalc`PropagatorDenominator]&&Length[#]===2&&FreeQ[#[[2]],q]&],Return[Missing["Topology"]]];
 order=SortBy[props,If[First[#]===qm,0,1]&];
 If[First[First[order]]=!=qm||Length[order]===2&&!MemberQ[{qm-pm,qm+pm},First[Last[order]]],Return[Missing["Routing"]]];
 poly=x/.fad->t;
 If[!PolynomialQ[poly,t]||Exponent[poly,t]=!=1||Coefficient[poly,t,0]=!=0,Return[Missing["RepeatedPropagators"]]];
 num=Coefficient[poly,t,1]/.{FeynCalc`Pair[qm,qm]->u,FeynCalc`Pair[pm,qm]->v,FeynCalc`Pair[qm,pm]->v};
 If[!FreeQ[num,q]||!PolynomialQ[num,{u,v}],Return[Missing["NonScalarPolynomial"]]];
 (* For q+p the same formula uses -p as the chord. *)
 If[Length[order]===2&&First[Last[order]]===qm+pm,num=num/.v->-v];
 <|"Numerator"->num,"Q"->u,"P"->v,"Invariant"->s,"MassesSquared"->(#[[2]]^2&/@order)|>];
qqWeightedVacuumMoment[n_Integer,a_,b_,s_] := FeynCalc`A0[a] Sum[
 Binomial[n,2j] 4^j Pochhammer[1/2,j]/Pochhammer[D/2,j]
 If[j===0,1,(s a)^j] If[n-2j===0,1,(a+s-b)^(n-2j)],{j,0,Floor[n/2]}];
qqWeightedExactScalarIntegral[data_Association] := Module[{s=data["Invariant"],m=data["MassesSquared"],num=data["Numerator"],u=data["Q"],v=data["P"],a,b,d0=Unique["D0"],d1=Unique["D1"],rules,result=0,i,j,c},
 a=First[m];
 If[Length[m]===1,
 rules=CoefficientRules[Expand[num],{u,v}];
 Do[{i,j}=First[r];c=Last[r];If[EvenQ[j],result+=c FeynCalc`A0[a] Pochhammer[1/2,j/2]/Pochhammer[D/2,j/2]
 If[j===0,1,s^(j/2)] If[i+j/2===0,1,a^(i+j/2)]],{r,rules}];
 Return[I Pi^2 result]];
 b=Last[m];rules=CoefficientRules[Expand[num/.{u->d0+a,v->(d0-d1+a+s-b)/2}],{d0,d1}];
 Do[{i,j}=First[r];c=Last[r];result+=c Which[i===0&&j===0,FeynCalc`B0[s,a,b],i===0,qqWeightedVacuumMoment[j-1,a,b,s],j===0,qqWeightedVacuumMoment[i-1,b,a,s],True,0],{r,rules}];
 I Pi^2 result];
qqWeightedTryExactScalar[unit_Association] := Module[{data,closed,old,difference},
 If[!qqAuditValidQ[Lookup[unit,"Input",Missing[]]]||!qqAuditValidQ[Lookup[unit,"Result",Missing[]]],Return[Missing["InvalidUnit"]]];
 data=qqWeightedExactScalarInput[unit["Input"]];If[!AssociationQ[data],Return[data]];
 qqProgressOperation["Check exact generic-D scalar numerator identity against saved source"];
 difference=TimeConstrained[
 closed=qqWeightedScalarCanonical[qqWeightedExactScalarIntegral[data]];
 old=qqWeightedScalarCanonical[FeynCalc`FCI[FeynCalc`PaVeReduce[qqWeightedOldPolicy[unit["Result"]],FeynCalc`Dimension->D,FeynCalc`FCE->False]]];
 If[qqFFInvalidQ[{closed,old}]||!FreeQ[old,FeynCalc`PaVeReduce|FeynCalc`TID|FeynCalc`FeynAmpDenominator],$Failed,Cancel[Together[old-closed]]],10,$TimedOut];
 If[difference=!=0,Return[Missing["ExactSourceMatchNotEstablished"]]];
 <|"Kind"->"ExactScalarSourceIdentity","UVTransferD"->0,"IRTransferD"->0,"UVTransferDFull"->0,
 "Proof"->"Exact full-D scalar numerator reduction matches the source; at most two simple propagators at nonexceptional invariant, so pinches contain no logarithmically scaleless tadpole.","ExactSourceDifference"->0|>];
If[Length[DownValues[qqWeightedUnitShiftBeforeResume]]===0,
 DownValues[qqWeightedUnitShiftBeforeResume]=DownValues[qqWeightedUnitShift]/.HoldPattern[qqWeightedUnitShift]->qqWeightedUnitShiftBeforeResume];
qqWeightedUnitShift[unit_Association] := Module[{fast=qqWeightedTryExactScalar[unit]},
 If[AssociationQ[fast],fast,qqProgressOperation["Replay unsupported pre-TID unit with scaleless retention"];qqWeightedUnitShiftBeforeResume[unit]]];

qqWeightedResumeRead[file_,key_] := Module[{p,data},
 p=If[FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],$Failed];
 If[!AssociationQ[p]||Lookup[p,"Schema",None]=!="RGZWeightedAssemblyResume/1"||Lookup[p,"Key",None]=!=key,Return[Missing["Resume"]]];
 data=Lookup[p,"Data",Missing["Data"]];
 If[!AssociationQ[data]||!qqAuditValidQ[data]||Lookup[p,"PayloadSHA256",None]=!=Hash[data,"SHA256"],Return[Missing["Resume"]]];data];
qqWeightedResumeWrite[file_,key_,data_Association] := atomicWXF[file,<|"Schema"->"RGZWeightedAssemblyResume/1","Key"->key,
 "PayloadSHA256"->Hash[data,"SHA256"],"Data"->data|>];
qqWeightedResumeTIDQ[p_,units_,tid_,sourceDir_,context_] := Module[{n,rows,expected,stop=Unique["resumeTID"]},Catch[
 If[!AssociationQ[p],Throw[False,stop]];n=Lookup[p,"Completed",None];rows=Lookup[p,"Rows",None];
 If[!IntegerQ[n]||n<0||n>Length[units]||!ListQ[rows]||Length[rows]=!=n||
 !ListQ[Lookup[p,"Integrated",None]]||Length[p["Integrated"]]=!=n||!ListQ[Lookup[p,"UV",None]]||Length[p["UV"]]=!=n,Throw[False,stop]];
 Do[expected=FileNameJoin[{sourceDir,"tid_units_v1","unit_"<>IntegerString[Hash[{"AA-TID-unit-v1",units[[i,2]],context},"SHA256"],16]<>".wl"}];
 If[!AssociationQ[rows[[i]]]||Lookup[rows[[i]],"UnitFile",None]=!=expected||
 Lookup[rows[[i]],"SHA256",None]=!=Lookup[tid["Files"],expected,Missing[]]||
 Lookup[rows[[i]],"InputSHA256",None]=!=Hash[units[[i,2]],"SHA256"]||
 !SameQ[Lookup[rows[[i]],"Weight",Missing[]],units[[i,1]]],Throw[False,stop]],{i,n}];True,stop]];
qqWeightedComponent[row_,component_,sourceDir_,tid_,pv_,auditDir_,key_,seconds_] := Module[
 {units,context=tid["Context"],pvContext=pv["Context"],unitFile,unit,uv={},integrated={},unitRows={},objects,needed,maps={},pvRows={},weight,z,shift,file,r,shared,semantic,split,old,scalar,
 resumeKey,resume,prefixFile,completeFile,start=1,baseUV,pvStart=1,pvPrefix,stop=Unique["RGZWorkflow`Private`weightedComponent"]},Catch[
 shared=FileNameJoin[{DirectoryName[auditDir],"shared-units"}];If[!DirectoryQ[shared],CreateDirectory[shared]];
 split=row["Decompose"<>component];old=qqWeightedOldPolicy[row["Integrate"<>component]];scalar=qqWeightedOldPolicy[row["Scalar"<>component]];
 resumeKey=Hash[{"WeightedComponentAssembly/1",key,component,sourceDir,Hash[{split,old,scalar},"SHA256"],tid["Digest"],pv["Digest"],$weightedResumeSHA256},"SHA256"];
 prefixFile=FileNameJoin[{auditDir,component<>"-tid-prefix.wxf"}];pvPrefix=FileNameJoin[{auditDir,component<>"-pv-prefix.wxf"}];completeFile=FileNameJoin[{auditDir,component<>"-complete.wxf"}];
 resume=qqWeightedResumeRead[completeFile,resumeKey];
 If[AssociationQ[resume]&&Lookup[resume,"Component",None]===component&&TrueQ[Lookup[resume,"TIDReconstructionVerified",False]]&&TrueQ[Lookup[resume,"PVReconstructionVerified",False]]&&
 ListQ[Lookup[resume,"TIDUnits",None]]&&ListQ[Lookup[resume,"PVIdentities",None]]&&SameQ[Lookup[resume,"Scalar",Missing[]],scalar]&&
 And@@(KeyExistsQ[resume,#]&/@{"WeightedUVTransferD","WeightedIRTransferD"}),
 qqProgressOperation["Reuse completed weighted "<>component<>" component"];Throw[resume,stop]];
 qqProgressOperation["Recover loop-independent graph weights from saved split integrand"];
 units=RGZWorkflow`Engine`RGZMXIntegralUnits[split];
 If[!ListQ[units]||!AllTrue[units,MatchQ[#,{_,_}]&&FreeQ[First[#],RGZWorkflow`Engine`q]&],Throw[Failure["QQWeightedLoopDependentWeight",<||>],stop]];
 resume=qqWeightedResumeRead[prefixFile,resumeKey];
 If[TrueQ[qqWeightedResumeTIDQ[resume,units,tid,sourceDir,context]],
 integrated=resume["Integrated"];uv=resume["UV"];unitRows=resume["Rows"];start=resume["Completed"]+1;
 qqProgressUpdate[<|"AssemblyUnitsReused"->resume["Completed"]|>];qqProgressOperation["Resume weighted "<>component<>" assembly at TID unit "<>ToString[start]<>" / "<>ToString[Length[units]]]];
 Do[weight=units[[n,1]];
 unitFile=FileNameJoin[{sourceDir,"tid_units_v1","unit_"<>IntegerString[Hash[{"AA-TID-unit-v1",units[[n,2]],context},"SHA256"],16]<>".wl"}];
 If[!KeyExistsQ[tid["Files"],unitFile]||!FileExistsQ[unitFile]||FileHash[unitFile,"SHA256"]=!=tid["Files"][unitFile],Throw[Failure["QQWeightedTIDUnitMissingOrChanged",<|"File"->unitFile|>],stop]];
 unit=qqWeightedRead[unitFile];If[!qqWeightedRecordQ[unit,units[[n,2]],context],Throw[Failure["QQWeightedTIDUnitMismatch",<|"File"->unitFile|>],stop]];
 qqProgressUpdate[<|"AlgebraGroupIndex"->n,"AlgebraGroupTotal"->Length[units]|>];qqProgressOperation["Audit weighted pre-TID "<>component<>" unit "<>ToString[n]<>" / "<>ToString[Length[units]]];
 semantic=Hash[KeyTake[unit,{"Input","Result","Environment"}],"SHA256"];file=FileNameJoin[{shared,"tid-"<>IntegerString[semantic,16,64]<>".wxf"}];
 shift=qqWeightedCheckpoint[file,Hash[{key,"tid",semantic},"SHA256"],qqWeightedUnitShift[unit],seconds];If[FailureQ[shift],Throw[shift,stop]];
 qqProgressOperation["Assemble weighted "<>component<>" TID unit "<>ToString[n]<>" / "<>ToString[Length[units]]];
 AppendTo[integrated,weight qqWeightedOldPolicy[unit["Result"]]];z=qqWeightedFiniteLaurent[weight shift["UVTransferDFull"]];If[FailureQ[z],Throw[z,stop]];AppendTo[uv,z];
 AppendTo[unitRows,<|"UnitFile"->unitFile,"SHA256"->tid["Files"][unitFile],"Weight"->weight,"InputSHA256"->Hash[units[[n,2]],"SHA256"],"Kind"->shift["Kind"],"WeightedUVTransferD"->z|>];
 If[Mod[n,16]===0||n===Length[units],qqProgressOperation["Save weighted "<>component<>" TID prefix "<>ToString[n]];
 r=qqWeightedResumeWrite[prefixFile,resumeKey,<|"Completed"->n,"Integrated"->integrated,"UV"->uv,"Rows"->unitRows|>];If[FailureQ[r],Throw[r,stop]]],{n,start,Length[units]}];
 qqProgressOperation["Verify weighted "<>component<>" TID sum against the source graph"];
 If[!qqWeightedEqualQ[Total[integrated],old],Throw[Failure["QQWeightedGraphTIDReconstruction",<|"Component"->component|>],stop]];
 objects=DeleteDuplicates[Cases[old,t_/;RGZWorkflow`Engine`RGZMXPVObjectQ[t],{0,Infinity}]];needed=Select[objects,!RGZWorkflow`Engine`RGZMXPVScalarQ[#]&];
 If[!qqWeightedLinearPVQ[old,objects],Throw[Failure["QQWeightedNonlinearPV",<||>],stop]];
 baseUV=uv;resume=qqWeightedResumeRead[pvPrefix,resumeKey];
 If[AssociationQ[resume]&&IntegerQ[Lookup[resume,"Completed",None]]&&0<=resume["Completed"]<=Length[needed]&&
 ListQ[Lookup[resume,"Maps",None]]&&Length[resume["Maps"]]===resume["Completed"]&&ListQ[Lookup[resume,"Rows",None]]&&Length[resume["Rows"]]===resume["Completed"]&&
 ListQ[Lookup[resume,"UV",None]]&&Length[resume["UV"]]===Length[baseUV]+resume["Completed"]&&Take[resume["UV"],Length[baseUV]]===baseUV,
 maps=resume["Maps"];pvRows=resume["Rows"];uv=resume["UV"];pvStart=resume["Completed"]+1];
 Do[unitFile=FileNameJoin[{sourceDir,"pv_units_v1","pv_"<>IntegerString[Hash[{"AA-PV-unit-v1",needed[[n]],pvContext},"SHA256"],16]<>".wl"}];
 If[!KeyExistsQ[pv["Files"],unitFile]||!FileExistsQ[unitFile]||FileHash[unitFile,"SHA256"]=!=pv["Files"][unitFile],Throw[Failure["QQWeightedPVUnitMissingOrChanged",<|"File"->unitFile|>],stop]];
 unit=qqWeightedRead[unitFile];If[!qqWeightedRecordQ[unit,needed[[n]],pvContext],Throw[Failure["QQWeightedPVUnitMismatch",<|"File"->unitFile|>],stop]];
 weight=Coefficient[old,needed[[n]]];qqProgressOperation["Audit weighted "<>component<>" pre-PV identity "<>ToString[n]<>" / "<>ToString[Length[needed]]];
 semantic=Hash[KeyTake[unit,{"Input","Result","Environment"}],"SHA256"];
 shift=qqWeightedCheckpoint[FileNameJoin[{shared,"pv-"<>IntegerString[semantic,16,64]<>".wxf"}],Hash[{key,"pv",semantic},"SHA256"],qqWeightedPVShift[needed[[n]],unit["Result"]],seconds];
 If[FailureQ[shift],Throw[shift,stop]];AppendTo[maps,needed[[n]]->qqWeightedOldPolicy[unit["Result"]]];z=qqWeightedFiniteLaurent[weight shift["UVTransferDFull"]];If[FailureQ[z],Throw[z,stop]];AppendTo[uv,z];
 AppendTo[pvRows,<|"UnitFile"->unitFile,"SHA256"->pv["Files"][unitFile],"Object"->needed[[n]],"Weight"->weight,"WeightedUVTransferD"->z|>];
 If[Mod[n,8]===0||n===Length[needed],r=qqWeightedResumeWrite[pvPrefix,resumeKey,<|"Completed"->n,"Maps"->maps,"Rows"->pvRows,"UV"->uv|>];If[FailureQ[r],Throw[r,stop]]],{n,pvStart,Length[needed]}];
 qqProgressOperation["Verify weighted "<>component<>" PV substitutions against the source graph"];
 If[!qqWeightedEqualQ[old/.Dispatch[maps],scalar],Throw[Failure["QQWeightedGraphPVReconstruction",<|"Component"->component|>],stop]];
 z=Total[uv];r=<|"Component"->component,"TIDReconstructionVerified"->True,"PVReconstructionVerified"->True,"TIDUnits"->unitRows,"PVIdentities"->pvRows,"WeightedUVTransferD"->z,"WeightedIRTransferD"->-z,"Scalar"->scalar|>;
 qqProgressOperation["Save completed weighted "<>component<>" component"];z=qqWeightedResumeWrite[completeFile,resumeKey,r];If[FailureQ[z],Throw[z,stop]];r,stop]];
End[];
