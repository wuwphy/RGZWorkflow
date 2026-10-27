(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
$certificationSourceSHA256=FileHash[$InputFileName,"SHA256"];
(* Independent audit artifacts. Never rewrite a bare amplitude or infer missing
   IR information from a vanishing dimensionally regulated integral. *)
qqAuditRead[file_,name_] := If[file===Automatic,RGZResult[name],
 If[StringQ[file]&&FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],Missing["AuditInput",name]]];
qqAuditValidQ[x_] := !qqFFInvalidQ[x];
SetAttributes[qqAuditCheckpoint,HoldRest];
qqAuditCheckpoint[file_,key_,body_,seconds_] := Module[{old,z,r},
 qqProgressUpdate[<|"CheckpointFile"->file|>];
 old=If[FileExistsQ[file],Import[file,"WXF"],None];
 If[AssociationQ[old]&&Lookup[old,"Key",None]===key&&qqAuditValidQ[Lookup[old,"Value",Missing[]]],Return[old["Value"]]];
 z=TimeConstrained[body,seconds,$TimedOut];
 If[!qqAuditValidQ[z],Return[Failure["QQAuditStopped",<|"File"->file,"Seconds"->seconds,"Reason"->z|>]]];
 r=atomicWXF[file,<|"Key"->key,"Value"->z|>];If[FailureQ[r],r,z]];
(* The coefficient of the common UV pole of A0 and B0. This operation is
   deliberately performed before mass expansion; no IR conclusion follows. *)
qqAuditUVNumerator[x_] := x/.{FeynCalc`A0[z_]:>z,FeynCalc`B0[___]:>1};
qqAuditPoint[] := <|"p^2"->RGZWorkflow`Engine`qqM0S,"m^2"->RGZWorkflow`Engine`qqM0X,
 "mu^2"->RGZWorkflow`Engine`qqM0Y,"mG^4"->RGZWorkflow`Engine`qqM0Z,"N"->RGZWorkflow`Engine`qqM0N|>;
(* Finite and singular small-e terms of a regulator tadpole, at D=4-2 eps.
   e=M^2>0 fixes Log[e lambda]=Log[e]+Log[lambda] without PowerExpand.
   This is a diagnostic of the Laurent-first order, not a replacement of
   the existing generic-D mass-first prescription. *)
(* Expand short formal series of individual factors. Large parameter-only
   polynomials remain opaque coefficients; never form a global denominator. *)
qqAuditMassSeriesTerm[term_,e_,maximum_:0] := Module[
 {factors,meta={},constant=1,base,power,num,den,vn,vd,low=0,need,cn,cd,baseVec,vec={1},next,out,key,signature,cached,stop=Unique["RGZWorkflow`Private`massSeriesTerm"]},Catch[
 factors=If[Head[term]===Times,List@@term,{term}];
 Do[If[FreeQ[f,e],constant*=f;Continue[]];
 key=Hash[f,"SHA256"];
 cached=If[AssociationQ[$qqMassSeriesSpecCache],Lookup[$qqMassSeriesSpecCache,key,None],None];
 If[ListQ[cached]&&cached[[1]]===f,
 {num,den,power,vn,vd}=cached[[2]];low+=power(vn-vd);AppendTo[meta,cached[[2]]];Continue[]];
 {base,power}=If[Head[f]===Power&&MatchQ[f[[2]],_Integer|_Rational],List@@f,{f,1}];
 (* Parameter subexpressions have already been masked, so this rational
    normalization acts only on short expressions in the mass variable. *)
 If[PolynomialQ[base,e],num=base;den=1,base=Together[base];num=Numerator[base];den=Denominator[base]];
 If[!PolynomialQ[num,e]||!PolynomialQ[den,e],Throw[Failure["QQMassSeriesNonPolynomial",<|"FactorLeaves"->LeafCount[f],"Factor"->If[LeafCount[f]<200,f,Missing["TooLarge"]]|>],stop]];
 vn=Exponent[num,e,Min];vd=Exponent[den,e,Min];If[vn===Infinity,Throw[0,stop]];
 If[AssociationQ[$qqMassSeriesSpecCache],AssociateTo[$qqMassSeriesSpecCache,key->{f,{num,den,power,vn,vd}}]];
 low+=power(vn-vd);AppendTo[meta,{num,den,power,vn,vd}],{f,factors}];
 If[low>maximum,Throw[0,stop]];need=Floor[maximum-low];
 If[need>16,Throw[Failure["QQMassSeriesOrderUnsupported",<|"Order"->need|>],stop]];
 Do[{num,den,power,vn,vd}=entry;
 signature={entry,need};key=Hash[signature,"SHA256"];
 cached=If[AssociationQ[$qqMassSeriesPowerCache],Lookup[$qqMassSeriesPowerCache,key,None],None];
 If[ListQ[cached]&&cached[[1]]===signature,next=cached[[2]],
 cn=Table[Coefficient[num,e,vn+k],{k,0,need}];cd=Table[Coefficient[den,e,vd+k],{k,0,need}];
 If[den===1,baseVec=cn,
 baseVec=ConstantArray[0,need+1];
 Do[baseVec[[k+1]]=(cn[[k+1]]-Sum[cd[[i+1]]baseVec[[k-i+1]],{i,1,k}])/cd[[1]],{k,0,need}]];
 If[power===1,next=baseVec,
 next=ConstantArray[0,need+1];next[[1]]=baseVec[[1]]^power;
 (* B Y' = power B' Y, with Y=B^power, determines each coefficient. *)
 Do[next[[k+1]]=Sum[((power+1)i-k)baseVec[[i+1]]next[[k-i+1]],{i,1,k}]/(k baseVec[[1]]),{k,1,need}]];
 If[AssociationQ[$qqMassSeriesPowerCache],AssociateTo[$qqMassSeriesPowerCache,key->{signature,next}]]];
 vec=Table[Sum[If[i<Length[vec],vec[[i+1]],0]next[[k-i+1]],{i,0,k}],{k,0,need}],{entry,meta}];
 out=Table[constant vec[[k+1]],{k,0,need}];
 If[TrueQ[$qqMassSeriesSparse],AssociationThread[Table[low+k,{k,0,need}],out],Sum[out[[k+1]]e^(low+k),{k,0,need}]],stop]];
qqAuditMassSeries[input_,point_,maximum_:0] := Block[{$qqMassSeriesSpecCache=<||>,$qqMassSeriesPowerCache=<||>,$qqMassSeriesSparse=True},Module[{terms,z,out={},e=RGZResidual39Diagnostic`Private`e,low,co,masked,index=0,rules=<||>,memo=<||>,walk,baseFile,formalFile,formalKey,cached,saved,groups,stop=Unique["RGZWorkflow`Private`massSeries"]},Catch[
 walk[x_] := Module[{key,slot},
 If[FreeQ[x,e],If[LeafCount[x]<16,Return[x]];key=Hash[x,"SHA256"];
 If[KeyExistsQ[memo,key],slot=memo[key];If[rules[slot]=!=x,Throw[Failure["QQMassMaskCollision",<||>],stop]],
 slot=Unique["RGZWorkflow`Private`massParameterCoefficient"];AssociateTo[memo,key->slot];AssociateTo[rules,slot->x]];Return[slot]];
 If[AtomQ[x],x,Map[walk,x]]];
 baseFile=If[ValueQ[$qqMassSeriesCheckpointBase],$qqMassSeriesCheckpointBase,None];
 formalFile=If[StringQ[baseFile],baseFile<>".formal-series.wxf",None];
 formalKey=Hash[{"QQMassFormalSeries/1",Hash[input,"SHA256"],point,maximum,$certificationSourceSHA256},"SHA256"];
 cached=If[StringQ[formalFile]&&FileExistsQ[formalFile],Import[formalFile,"WXF"],None];
 If[AssociationQ[cached]&&Lookup[cached,"Key",None]===formalKey&&ListQ[Lookup[cached,"Rules",None]]&&KeyExistsQ[cached,"Polynomial"]&&qqAuditValidQ[cached],
 masked=cached["Polynomial"];rules=Association[cached["Rules"]];
 qqProgressUpdate[<|"MassSeriesCheckpointFile"->formalFile,"MassSeriesCheckpointReused"->True,"CacheState"->"Reused"|>],
 qqProgressUpdate[<|"MassSeriesCheckpointReused"->False|>];
 qqProgressOperation["Mass series: convert physical coordinates once"];
 z=Block[{RGZResidual39Diagnostic`Private`$parameterPoint=point},RGZResidual39Diagnostic`Private`pointExpression[input]];
 terms=If[Head[z]===Plus,List@@z,{z}];
 qqProgressOperation["Mass series: extract factored coefficients"];
 Do[If[maximum<0&&FreeQ[term,e],Continue[]];index++;If[Mod[index,100]===1,qqProgressUpdate[<|"AlgebraGroupIndex"->index,"AlgebraGroupTotal"->Length[terms]|>]];
 masked=walk[term];
 z=qqAuditMassSeriesTerm[masked,RGZResidual39Diagnostic`Private`e,maximum];
 If[FailureQ[z],Throw[z,stop]];AppendTo[out,z],{term,terms}];
 qqProgressOperation["Mass series: collect formal mass coefficients"];
 (* Each factor routine already returns a short coefficient vector. Merge
    those vectors directly; expanding their global sum redistributes the
    large parameter coefficients and defeats the purpose of masking. *)
 groups=Merge[Select[out,AssociationQ],Total];
 If[!AllTrue[Keys[groups],IntegerQ],Throw[Failure["QQMassSeriesUnresolvedFractionalPowers",<||>],stop]];
 masked=Total[KeyValueMap[#2 e^(#1+16)&,groups]];
 If[StringQ[formalFile],qqProgressOperation["Save formal mass-series checkpoint before parameter restoration"];
 saved=atomicWXF[formalFile,<|"Key"->formalKey,"Polynomial"->masked,"Rules"->Normal[rules],"MaximumPower"->maximum|>];If[FailureQ[saved],Throw[saved,stop]];
 qqProgressUpdate[<|"MassSeriesCheckpointFile"->formalFile,"CacheState"->"Saved"|>]]];
 qqProgressOperation["Mass series: restore exact parameter coefficients"];
 z=(masked/.Dispatch[Normal[rules]])/e^16;
 z=Expand[z,e];low=Exponent[z,e,Min];
 If[IntegerQ[low]&&low< -2,Do[co=Coefficient[z,e,k];If[Cancel[Together[co]]=!=0,Throw[Failure["QQMassSeriesHigherPole",<|"Power"->k|>],stop]];z-=co e^k,{k,low,-3}]];z,stop]]];
SetAttributes[qqAuditTadpole,HoldRest];
qqAuditTadpole[master_,co_,point_] := Module[{e=RGZResidual39Diagnostic`Private`e,z,c,lambda,series},
 z=Block[{RGZResidual39Diagnostic`Private`$parameterPoint=point},RGZResidual39Diagnostic`Private`pointExpression[master]];
 If[!MatchQ[z,RGZResidual39Diagnostic`Private`a0[_]]||FreeQ[z,e],Return[<|"RegulatorTadpole"->False|>]];
 lambda=Cancel[First[z]/e];If[!FreeQ[lambda,e],Return[Failure["QQNonlinearRegulatorTadpole",<|"Master"->master|>]]];
 qqProgressOperation["Extract mass series by factored formal coefficients"];
 c=Block[{$qqMassSeriesCheckpointBase=Lookup[$qqProgress,"CheckpointFile",None]},qqAuditMassSeries[co,point,-1]];
 If[FailureQ[c],Return[c]];
 qqProgressOperation["Mass series: build pole and dimensional finite terms"];
 series=Coefficient[c,e,-2]/e^2+Coefficient[c,e,-1]/e;
 <|"RegulatorTadpole"->True,"Master"->master,"MassRatio"->lambda,
 "LaurentFirstPoleNumerator"->series lambda e,
 "LaurentFirstFinite"->lambda e ((series/.RGZResidual39Diagnostic`Private`dim->4) (1-Log[e/RGZWorkflow`Parameters`muR2]-Log[lambda])-2 (D[series,RGZResidual39Diagnostic`Private`dim]/.RGZResidual39Diagnostic`Private`dim->4)),
 "LimitOrder"->"D Laurent first, then M^2 -> 0+; compare with generic-D mass first"|>];
Options[RGZQQMassLimitLedger]={"JobFile"->Automatic,"CoefficientSeconds"->60};
RGZQQMassLimitLedger[seconds_:1200,OptionsPattern[]] := Module[
 {j,r,p,key,dir,rows={},z,file,out,sec=OptionValue["CoefficientSeconds"],stop=Unique["RGZWorkflow`Private`massAudit"],tad,sourceVerified=False,auditFailure=None,pairCount=0,expected=0,masters,selected,positions,sourceSHA,indexKey,indexFile,index},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,sec}),Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 If[!qqGaugeSymbolicParametersQ[],Return[Failure["SymbolicParametersRequired",<||>]]];
 j=qqAuditRead[OptionValue["JobFile"],"QQ"];
 If[!AssociationQ[j]||!And@@(KeyExistsQ[j,#]&/@{"Fingerprint","MixedSourceHash","Directory","ModelFingerprint","ModelContent"})||
 Lookup[j,"RegulatorMass",None]=!=0||j["ModelFingerprint"]=!=modelKey[]||j["ModelContent"]=!=modelContent[],Return[Failure["QQMassLedgerSourceRequired",<||>]]];
 r=loadPTEngine[];If[FailureQ[r],Return[r]];r=engineGet["RGZ_Residual39_M0_Diagnostic.wl"];If[FailureQ[r],Return[r]];r=engineGet["RGZ_Quark_Amplitude.wl",False];If[FailureQ[r],Return[r]];
 file=FileNameJoin[{j["Directory"],"self","pairs.wxf"}];If[!FileExistsQ[file],Return[Failure["QQSelfPairsRequired",<||>]]];
 sourceSHA=FileHash[file,"SHA256"];
 indexKey=Hash[{"QQMassLimitInputs/1",j["Fingerprint"],j["MixedSourceHash"],sourceSHA},"SHA256"];
 indexFile=FileNameJoin[{cacheDirectory[],"qq-mass-inputs-"<>IntegerString[indexKey,16,64]<>".wxf"}];
 key=Hash[{"QQMassLimitLedger/1",j["Fingerprint"],j["MixedSourceHash"],sourceSHA,$certificationSourceSHA256},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-mass-ledger-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQMassLimitLedger",Catch[
 qqProgressOperation["Read source-bound regulator-tadpole input index"];
 index=If[FileExistsQ[indexFile],Import[indexFile,"WXF"],None];
 If[AssociationQ[index]&&Lookup[index,"Key",None]===indexKey&&ListQ[Lookup[index,"Masters",None]]&&AssociationQ[Lookup[index,"Selected",None]],
 masters=index["Masters"];selected=index["Selected"];pairCount=Length[masters];expected=Length[selected];sourceVerified=True,
 qqProgressOperation["Read pre-limit mixed-self coefficient pairs"];p=Import[file,"WXF"];
 If[!AssociationQ[p]||Lookup[p,"Key",None]=!=Hash[{j["Fingerprint"],j["MixedSourceHash"]},"SHA256"]||!qqFFVectorPairsQ[Lookup[p,"Value",None]],Throw[Failure["QQMassLedgerSourceMismatch",<||>],stop]];
 sourceVerified=True;pairCount=Length[p["Value"]];masters=p["Value"][[All,1]];
 positions=Flatten[Position[masters,FeynCalc`A0[x_]/;!FreeQ[x,RGZWorkflow`Engine`M],{1}]];expected=Length[positions];
 selected=AssociationThread[positions,Part[p["Value"],positions]];Clear[p];
 index=atomicWXF[indexFile,<|"Key"->indexKey,"SourceSHA256"->sourceSHA,"Masters"->masters,"Selected"->selected|>];If[FailureQ[index],Throw[index,stop]]];
 Do[qqProgressUpdate[<|"Stage"->"QQMassLimitLedger","Index"->n,"Total"->pairCount,"Master"->masters[[n]],"StepLimitSeconds"->sec|>];
 qqProgressOperation["Audit discarded regulator tadpole "<>ToString[n]];
 z=qqAuditCheckpoint[FileNameJoin[{dir,"mass-"<>ToString[n]<>".wxf"}],Hash[{key,n},"SHA256"],If[KeyExistsQ[selected,n],qqAuditTadpole[masters[[n]],selected[n][[2]],qqAuditPoint[]],<|"RegulatorTadpole"->False|>],sec];
 If[FailureQ[z],Throw[z,stop]];AppendTo[rows,z],{n,pairCount}];True,stop],seconds];
 If[FailureQ[r],If[!sourceVerified,Return[r]];auditFailure=r];tad=Select[rows,TrueQ[#["RegulatorTadpole"]]&];
 out=<|"Schema"->"RGZQQMassLimitLedger/1","Status"->If[FailureQ[auditFailure],"MassLimitOrderAuditStopped","MassLimitOrderAudit"],"Key"->key,"ModelFingerprint"->j["ModelFingerprint"],"ModelContent"->j["ModelContent"],
 "SourceFingerprint"->j["Fingerprint"],"MixedSourceHash"->j["MixedSourceHash"],"Gauge"->Lookup[j,"Gauge",None],"RegulatorMass"->0,
 "AuditScope"->"Regulator tadpoles only; bubble-region matching is separate","SeriesDomain"->{RGZResidual39Diagnostic`Private`e>0,RGZResidual39Diagnostic`Private`h>0},"GaugeLimitApplied"->False,
 "PreLimitPairsSHA256"->sourceSHA,"Rows"->rows,"PairCount"->pairCount,"PairsAudited"->Length[rows],"RegulatorTadpolesExpected"->expected,"RegulatorTadpoles"->Length[tad],"AuditComplete"->!FailureQ[auditFailure],"StoppedCheckpoint"->If[FailureQ[auditFailure],Lookup[auditFailure[[2]],"File",None],None],
 "TadpolePoleNumerator"->Total[Lookup[tad,"LaurentFirstPoleNumerator",{}]],"TadpoleFinite"->Total[Lookup[tad,"LaurentFirstFinite",{}]],
 "MassLimitUVIRCertified"->False,"ScalelessLedgerComplete"->False,
 "Outstanding"->{"Combine tadpole terms with all small-mass bubble regions before comparing limit orders","Verify mixed-matrix pre-TID and pre-PV transfers","IR-region and finite limit matching remain required"},
 "File"->FileNameJoin[{dir,"mass-ledger.wxf"}]|>;
 r=atomicWXF[out["File"],out];If[FailureQ[r],Return[r]];storeResult["QQMassLimitLedger",out];
 If[FailureQ[auditFailure],Return[Failure[auditFailure[[1]],Join[auditFailure[[2]],<|"ReportFile"->out["File"],"PairsAudited"->Length[rows],"PairCount"->pairCount,"Action"->"Repeat RGZQQMassLimitLedger; completed formal-series and coefficient checkpoints are reused."|>]]]];
 <|"Status"->out["Status"],"Name"->"QQMassLimitLedger","Rows"->Length[rows],"Statistics"->KeyTake[out,{"RegulatorTadpoles","MassLimitUVIRCertified"}],"File"->out["File"]|>];

(* A certificate is an aggregate of compatible evidence, never an editable
   upgrade of the bare packet. Incomplete evidence remains a saved audit. *)
Options[RGZQQCertification]={"AmplitudeFile"->Automatic,"MSbarFile"->Automatic,"GaugeFile"->Automatic,"MassLedgerFile"->Automatic,"FiniteAuditFile"->Automatic};
RGZQQCertification[OptionsPattern[]] := Module[{a,m,g,l,f,checks,out,file,r,conditions},
 r=ready[];If[FailureQ[r],Return[r]];
 a=qqAuditRead[OptionValue["AmplitudeFile"],"QQAmplitude"];m=qqAuditRead[OptionValue["MSbarFile"],"QQMSbar"];
 g=qqAuditRead[OptionValue["GaugeFile"],"QQGaugeEarly"];l=qqAuditRead[OptionValue["MassLedgerFile"],"QQMassLimitLedger"];
 If[!AllTrue[{a,m,g,l},AssociationQ],Return[Failure["QQCertificationInputsRequired",<|"Action"->"Prepare bare amplitude, MSbar, symbolic gauge comparison and mixed-self mass ledger."|>]]];
 f=qqAuditRead[OptionValue["FiniteAuditFile"],"QQFiniteAudit"];
 conditions=DeleteDuplicates[Join[Lookup[m,"IRValidityConditions",{}],Lookup[l,"ExternalResidueConditions",{}]]];
 checks=<|"CurrentModel"->And@@(Lookup[#,"ModelFingerprint",None]===modelKey[]&/@{a,m,l}),
 "SameBareSource"->(Lookup[a,"Fingerprint",None]=!=None&&Lookup[m,"SourceFingerprint",None]===a["Fingerprint"]&&Lookup[l,"SourceFingerprint",None]===a["Fingerprint"]),
 "SymbolicGaugeZero"->(Lookup[g,"Status",None]==="VerifiedSymbolicZero"&&TrueQ[Lookup[g,"AllCoefficientsZero",False]]&&Lookup[g,"PointTest",None]===False),
 "GaugeSourceCurrent"->(KeyExistsQ[g,"GaugeSymbol"]&&KeyExistsQ[g,"GaugeJobFingerprint"]&&Lookup[g,"GaugeJobFingerprint",None]===qqJobFingerprint[g["GaugeSymbol"],0]),
 "GaugeMixedSourceMatches"->(Lookup[g,"MixedSourceHash",None]=!=None&&Lookup[g,"MixedSourceHash",None]===Lookup[l,"MixedSourceHash",Missing[]]&&Lookup[g,"ModelFingerprint",None]===Lookup[a,"ModelFingerprint",Missing[]]),
 "UVClosed"->TrueQ[Lookup[m,"MSbarUVClosed",False]],"IRClosed"->TrueQ[Lookup[m,"IRPolesCancelled",False]],
 "FinitePartBuilt"->(TrueQ[Lookup[m,"FinitePartBuilt",False]]&&KeyExistsQ[m,"Amplitude"]&&qqAuditValidQ[Lookup[m,"Amplitude",Missing[]]]),
 "AllRowPolesZero"->(ListQ[Lookup[m,"Rows",None]]&&Length[m["Rows"]]>0&&AllTrue[m["Rows"],Lookup[#,"RemainingUVPole",None]===0&&Lookup[#,"RemainingIRPole",None]===0&]),
 "MixedMassLedgerComplete"->TrueQ[Lookup[l,"MassLimitUVIRCertified",False]],
 "FullScalelessLedgerComplete"->TrueQ[Lookup[l,"ScalelessLedgerComplete",False]],
 "FiniteConventionIndependentlyVerified"->(AssociationQ[f]&&TrueQ[Lookup[f,"FiniteAssemblyVerified",False]]&&Lookup[f,"AlgorithmSHA256",None]===$certificationSourceSHA256&&Lookup[f,"MSbarKey",None]===Lookup[m,"Key",Missing[]]&&Lookup[f,"SourceFingerprint",None]===Lookup[a,"Fingerprint",Missing[]]),
 "DomainDeclared"->(ListQ[conditions]&&Length[conditions]>0),
 "LandauM0Scope"->(Lookup[m,"Gauge",None]===0&&Lookup[m,"RegulatorMass",None]===0&&Lookup[a,"Gauge",None]===0&&Lookup[a,"RegulatorMass",None]===0)|>;
 out=<|"Schema"->"RGZQQCertification/1","AlgorithmSHA256"->$certificationSourceSHA256,"Status"->If[And@@Values[checks],"CertifiedWithinDeclaredDomain","CertificationIncomplete"],
 "Checks"->checks,"OutstandingChecks"->Keys[Select[checks,!TrueQ[#]&]],"FullSMatrixVerified"->And@@Values[checks],
 "ModelFingerprint"->modelKey[],"SourceFingerprint"->Lookup[a,"Fingerprint",None],"GaugeJobFingerprint"->Lookup[g,"GaugeJobFingerprint",None],
 "IRValidityConditions"->conditions,"BranchPrescription"->"Continue the RGZ square root and logarithms from the declared Euclidean domain on a common sheet with +i0; crossing a Landau singularity requires a separate boundary analysis.","Scope"->"Two-flavor massless external qqprime; one loop; M -> 0+; conditional complex-mass domain. No all-parameter or inclusive-cross-section claim.",
 "EvidenceFiles"->Map[Lookup[#,"File",Missing["InMemory"]]&,{m,g,l}]|>;
 If[TrueQ[out["FullSMatrixVerified"]],AssociateTo[out,"Amplitude"->m["Amplitude"]]];
 file=FileNameJoin[{cacheDirectory[],"qq-certification-"<>IntegerString[Hash[KeyDrop[out,"Amplitude"],"SHA256"],16,64]<>".wxf"}];AssociateTo[out,"File"->file];
 r=atomicWXF[file,out];If[FailureQ[r],Return[r]];storeResult["QQCertification",out];KeyTake[out,{"Status","Checks","OutstandingChecks","FullSMatrixVerified","File"}]];
$certificationSource=FileNameJoin[{$packageRoot,"Kernel","ScatteringCertification.wl"}];

qqMixedAuditUnit[record_,seconds_] := Module[{x,z,u,v,q=RGZWorkflow`Engine`q},
 x=record["Input"];z=record["Result"];
 If[qqAuditValidQ[{x,z}]=!=True,Return[Failure["QQMixedAuditInvalidUnit",<||>]]];
 If[FreeQ[x,FeynCalc`FeynAmpDenominator],
 If[!FreeQ[Denominator[Together[x]],q],Return[Failure["QQMixedUnsplitUnit",<||>]]];
 Return[<|"Kind"->"Polynomial","UVTransfer"->0,"IRTransfer"->0|>]];
 If[!qqMSbarZeroScaleQ[x],Return[<|"Kind"->"ScalePresent","UVTransfer"->0,"IRTransfer"->0|>]];
 u=Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=True},
 FeynCalc`PaVeUVPart[FeynCalc`TID[x,q,FeynCalc`ToPaVe->True,FeynCalc`UsePaVeBasis->True,FeynCalc`ApartFF->False]]];
 v=FeynCalc`PaVeUVPart[z];
 If[!FreeQ[{u,v},FeynCalc`TID|FeynCalc`FeynAmpDenominator|FeynCalc`PaVeUVPart],Return[Failure["QQMixedAuditReductionIncomplete",<||>]]];
 u=Cancel[Together[u-v]];
 <|"Kind"->"Scaleless","UVTransfer"->u,"IRTransfer"->-u,
 "Convention"->"UVTransfer is in D-dimensional PV pole convention; IRTransfer uses the independent IR regulator after D expansion"|>];
Options[RGZQQMixedUVIRLedger]={"MatrixManifest"->Automatic,"IntegralSeconds"->60};
RGZQQMixedUVIRLedger[seconds_:1200,OptionsPattern[]] := Module[
 {r,manifest=OptionValue["MatrixManifest"],m,records,dirs,files,dir,key,out,rows={},rec,z,sec=OptionValue["IntegralSeconds"],stop=Unique["RGZWorkflow`Private`mixedLedger"],sum},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,sec}),Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 If[manifest===Automatic,Return[Failure["QQMixedManifestRequired",<|"Action"->"Supply MatrixManifest -> the completed matrix_job.wl. Source caches are read only."|>]]];
 If[!StringQ[manifest]||!FileExistsQ[manifest],Return[Failure["QQMixedManifestRequired",<||>]]];
 m=Get[manifest];If[!AssociationQ[m]||!AssociationQ[Lookup[m,"Records",None]]||Length[m["Records"]]=!=256,Return[Failure["QQMixedManifestIncomplete",<||>]]];
 records=Values[m["Records"]];
 If[!AllTrue[records,Lookup[#,"Status",None]==="Complete"&],Return[Failure["QQMixedManifestIncomplete",<||>]]];
 dirs=DeleteDuplicates[Lookup[records,"PerDiagramCache",None]];
 files=DeleteDuplicates[Flatten[FileNames["unit_*.wl",FileNameJoin[{#,"tid_units_v1"}]]&/@Select[dirs,StringQ]]];
 If[files==={},Return[Failure["QQMixedPreTIDInputsRequired",<||>]]];
 key=Hash[{"QQMixedUVIRLedger/1",FileHash[manifest,"SHA256"],modelKey[],$certificationSourceSHA256},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-mixed-ledger-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQMixedUVIRLedger",Catch[
 Do[qqProgressUpdate[<|"Stage"->"QQMixedUVIRLedger","Index"->n,"Total"->Length[files],"StepLimitSeconds"->sec|>];qqProgressOperation["Audit pre-TID mixed kernel "<>ToString[n]];
 rec=Get[files[[n]]];If[!AssociationQ[rec]||Lookup[rec,"Status",None]=!="Complete"||!And@@(KeyExistsQ[rec,#]&/@{"Input","Result"}),Throw[Failure["QQMixedUnitIncomplete",<|"File"->files[[n]]|>],stop]];
 z=qqAuditCheckpoint[FileNameJoin[{dir,IntegerString[Hash[files[[n]],"SHA256"],16,64]<>".wxf"}],Hash[{key,FileHash[files[[n]],"SHA256"]},"SHA256"],qqMixedAuditUnit[rec,sec],sec];
 If[FailureQ[z],Throw[z,stop]];AppendTo[rows,Join[<|"SourceFile"->files[[n]],"InputSHA256"->Hash[rec["Input"],"SHA256"]|>,z]],{n,Length[files]}];True,stop],seconds];
 If[FailureQ[r],Return[r]];
 out=<|"Schema"->"RGZQQMixedUVIRLedger/1","Status"->"PreTIDCacheAudit","ModelFingerprint"->modelKey[],"ManifestSHA256"->FileHash[manifest,"SHA256"],"Key"->key,"Rows"->rows,
 "UnitCount"->Length[rows],"ScalelessUnits"->Count[Lookup[rows,"Kind"],"Scaleless"],
 "AllUnitTransfersZero"->AllTrue[rows,#["UVTransfer"]===0&],"SourceGraphCoverageVerified"->False,"ScalelessLedgerComplete"->False,
 "Outstanding"->{"Match graph coefficients and all pre-PV identities to this unit inventory before declaring matrix coverage","Carry any nonzero transfers through matrix assembly and the common M0 prescription"},
 "File"->FileNameJoin[{dir,"mixed-ledger.wxf"}]|>;
 r=atomicWXF[out["File"],out];If[FailureQ[r],Return[r]];storeResult["QQMixedUVIRLedger",out];
 <|"Status"->out["Status"],"Name"->"QQMixedUVIRLedger","Rows"->Length[rows],"Statistics"->KeyTake[out,{"ScalelessUnits","AllUnitTransfersZero","ScalelessLedgerComplete"}],"File"->out["File"]|>];

(* Independent common-epsilon series check: do not call qqMSbarFiniteRow. *)
qqAuditFiniteRow[pairs_,table_,scale_] := Module[{eps=Unique["RGZWorkflow`Private`auditEps"],r=0,c,p,finite,u,ir},
 Do[c=pair[[2]]/.D->4-2eps;
 If[pair[[1]]===1,r+=SeriesCoefficient[c,{eps,0,0}];Continue[]];
 p=table[pair[[1]]];u=p["UV"]/.D->4-2eps;
 ir=p["IR"]/.{FeynCalc`EpsilonIR->eps,D->4-2eps};
 finite=qqMSbarFiniteMaster[pair[[1]],scale];
 r+=SeriesCoefficient[c,{eps,0,0}] finite+SeriesCoefficient[c,{eps,0,1}] SeriesCoefficient[u+ir,{eps,0,-1}]+SeriesCoefficient[c,{eps,0,2}] SeriesCoefficient[u+ir,{eps,0,-2}],{pair,pairs}];r];
Options[RGZQQFiniteAudit]={"MSbarFile"->Automatic,"UVIRFile"->Automatic,"AmplitudeFile"->Automatic,"CoefficientSeconds"->60};
RGZQQFiniteAudit[seconds_:1200,OptionsPattern[]] := Module[
 {m,u,a,f,r,sec=OptionValue["CoefficientSeconds"],key,dir,rows={},row,source,z,ct,ctFF,ctMap,uv,checks,out,file,xx=Unique["RGZWorkflow`Private`measureEps"],stop=Unique["RGZWorkflow`Private`finiteAudit"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,sec}),Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 m=qqAuditRead[OptionValue["MSbarFile"],"QQMSbar"];u=qqAuditRead[OptionValue["UVIRFile"],"QQUVIR"];a=qqAuditRead[OptionValue["AmplitudeFile"],"QQAmplitude"];
 If[!AllTrue[{m,u,a},AssociationQ]||!And@@(KeyExistsQ[m,#]&/@{"SourceFingerprint","Rows","RenormalizationScaleSquared","ModelFingerprint"})||
 !And@@(KeyExistsQ[u,#]&/@{"SourceFingerprint","InputSHA256","MasterPoles","FormFactorsFile"})||!KeyExistsQ[a,"Fingerprint"]||
 m["SourceFingerprint"]=!=a["Fingerprint"]||u["SourceFingerprint"]=!=a["Fingerprint"]||m["ModelFingerprint"]=!=modelKey[]||
 Lookup[m,"InputSHA256",None]=!=u["InputSHA256"]||Hash[Lookup[a,"OneLoop",Missing[]]/I,"SHA256"]=!=u["InputSHA256"],Return[Failure["QQFiniteAuditSourceMismatch",<||>]]];
 f=qqUVIRReadFF[u["FormFactorsFile"]];If[FailureQ[f],Return[f]];
 If[f["SourceFingerprint"]=!=a["Fingerprint"]||f["InputSHA256"]=!=u["InputSHA256"],Return[Failure["QQFiniteAuditSourceMismatch",<||>]]];
 key=Hash[{"QQFiniteAudit/1",m["Key"],u["Key"],$certificationSourceSHA256},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-finite-audit-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQFiniteAudit",Catch[
 qqProgressOperation["Independently project local tree parameter counterterm"];
 ct=qqLocalParameterCT[a["Tree"]/I,RGZQQMSbarCounterterms[]["ParameterCounterterms"]];
 ctFF=qqMSbarFF[ct,a["Fingerprint"],sec];If[FailureQ[ctFF],Throw[ctFF,stop]];ctMap=qqMSbarMap[ctFF];
 Do[row=m["Rows"][[n]];source=SelectFirst[f["Rows"],f["ColorBasis"][[#["ColorID"]]]===m["ColorBasis"][[row["ColorID"]]]&&f["SpinStructures"][[#["SpinID"]]]===m["SpinStructures"][[row["SpinID"]]]&,Missing[]];
 If[!AssociationQ[source],Throw[Failure["QQFiniteAuditMissingRow",<|"Index"->n|>],stop]];
 qqProgressUpdate[<|"Stage"->"QQFiniteAudit","Index"->n,"Total"->Length[m["Rows"]],"StepLimitSeconds"->sec|>];qqProgressOperation["Compare independent Laurent finite coefficient and local counterterm"];
 z=qqAuditCheckpoint[FileNameJoin[{dir,"row-"<>ToString[n]<>".wxf"}],Hash[{key,n},"SHA256"],
 <|"FiniteDifference"->qqSimplifyCoefficient[Expand[qqAuditFiniteRow[source["MasterCoefficients"],u["MasterPoles"],m["RenormalizationScaleSquared"]]-row["FiniteFormFactor"]]],
 "CountertermDifference"->Cancel[Together[(qqMSbarLookup[ctMap,m["ColorBasis"][[row["ColorID"]]],m["SpinStructures"][[row["SpinID"]]]]/.FeynCalc`SMP["Delta_UV"]->1/FeynCalc`EpsilonUV)-row["LocalUVPoleCounterterm"]]]|>,sec];
 If[FailureQ[z],Throw[z,stop]];AppendTo[rows,Join[KeyTake[row,{"ColorID","SpinID"}],z]],{n,Length[m["Rows"]]}];True,stop],seconds];
 If[FailureQ[r],Return[r]];
 checks=<|"IndependentLaurentFiniteMatch"->AllTrue[rows,#["FiniteDifference"]===0&],"LocalParameterCountertermMatch"->AllTrue[rows,#["CountertermDifference"]===0&],
 "TreeCountedOnce"->(Lookup[m,"Amplitude",None]===Lookup[m,"TreeAmplitude",Missing[]]+Lookup[m,"OneLoopFiniteAmplitude",Missing[]]),
 "PhysicalLoopMeasure"->(Lookup[a,"AssemblyConvention",None]==="QQPhysicalLoopMeasure/1"&&Lookup[a,"MixedSelfEnergyLoopMeasure",None]===1/(2Pi)^4),
 "DimensionalMeasureConversion"->TrueQ[FullSimplify[SeriesCoefficient[(2Pi)^(2xx),{xx,0,1}]-2Log[2Pi]]===0]|>;
 out=<|"Status"->If[And@@Values[checks],"FiniteAssemblyVerified","FiniteAssemblyMismatch"],"Key"->key,"ModelFingerprint"->modelKey[],"SourceFingerprint"->a["Fingerprint"],"MSbarKey"->m["Key"],
 "Checks"->checks,"Rows"->rows,"FiniteAssemblyVerified"->And@@Values[checks],"AlgorithmSHA256"->$certificationSourceSHA256,"FullSMatrixVerified"->False,
 "Scope"->"Independent Laurent reconstruction and tree-derived counterterm consistency; upstream mass-limit and scaleless coverage remain separate evidence.","File"->FileNameJoin[{dir,"finite-audit.wxf"}]|>;
 r=atomicWXF[out["File"],out];If[FailureQ[r],Return[r]];storeResult["QQFiniteAudit",out];KeyTake[out,{"Status","Checks","File"}]];
(* Link certified renormalized evidence without changing source fingerprints. *)
qqBareSMatrixGate[] := Module[{r=ready[],amp=RGZResult["QQAmplitude"],cert=RGZResult["QQCertification"]},
 If[FailureQ[r],Return[r]];
 If[AssociationQ[cert],Return[Failure["FullSMatrixNotVerified",<|"CertificationImplemented"->True,"Action"->"RGZQQCertification[]","OutstandingChecks"->Lookup[cert,"OutstandingChecks",{}],"CertificationFile"->Lookup[cert,"File",None]|>]]];
 If[!AssociationQ[amp],Return[Failure["QQAmplitudeFirst",<|"Action"->"RGZQQAmplitude[]"|>]]];
 Failure["FullSMatrixNotVerified",<|"CertificationImplemented"->True,"Action"->"RGZQQCertification[]","OutstandingChecks"->Lookup[amp,"OutstandingChecks",{}]|>]];
RGZQQSMatrix[] := Module[{cert,r=ready[],gfile=Automatic,files},
 If[FailureQ[r],Return[r]];
 If[!AllTrue[RGZResult/@{"QQAmplitude","QQMSbar","QQMassLimitLedger","QQFiniteAudit"},AssociationQ],Return[qqBareSMatrixGate[]]];
 cert=RGZResult["QQCertification"];
 If[!AssociationQ[RGZResult["QQGaugeEarly"]]&&AssociationQ[cert],
 files=Lookup[cert,"EvidenceFiles",{}];If[ListQ[files]&&Length[files]===3&&StringQ[files[[2]]],gfile=files[[2]]]];
 r=RGZQQCertification["GaugeFile"->gfile];If[FailureQ[r],Return[r]];cert=RGZResult["QQCertification"];
 If[!TrueQ[cert["FullSMatrixVerified"]],Return[qqBareSMatrixGate[]]];
 <|"Status"->"CertifiedWithinDeclaredDomain","Amplitude"->cert["Amplitude"],"Conditions"->cert["IRValidityConditions"],"BranchPrescription"->cert["BranchPrescription"],"Scope"->cert["Scope"],
 "Convention"->"Sfi = deltafi + i (2 Pi)^4 delta4(Pf-Pi) Amplitude","CertificationFile"->cert["File"]|>];
