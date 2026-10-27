(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Landau, M=0, two-flavor amplitude. Independent completion of the
   base-model constraint Jacobian and an explicit raw-core scaleless ledger. No source
   QQ/FR caches are rewritten. General-alpha and finite-M jobs are refused. *)
qqMSbarJob[file_] := Module[{j},
 j=If[file===Automatic,RGZResult["QQ"],If[StringQ[file]&&FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],$Failed]];
 If[!AssociationQ[j]||Lookup[j,"Schema",None]=!="RGZQQ/3"||!And@@(KeyExistsQ[j,#]&/@{"Fingerprint","Tree","Integrands","NAVSource","LoopMeasureConversion"}),Return[Failure["QQMSbarJobRequired",<||>]]];
 If[Lookup[j,"Gauge",None]=!=0||Lookup[j,"RegulatorMass",None]=!=0,Return[Failure["QQMSbarLandauM0Only",<|"Explanation"->"This completion is derived for Gauge -> 0 and RegulatorMass -> 0."|>]]];
 If[Lookup[j,"JacobianGhostIncluded",False]=!=True||Lookup[j,"ClosedQuarkFlavors",None]=!=2||Lookup[j,"ModelFingerprint",None]=!=modelKey[]||Lookup[j,"ModelContent",None]=!=modelContent[],Return[Failure["QQCompletedModelRequired",<|"Action"->"Recompute with the eta and two-flavor base model; old quenched jobs are not upgraded by adding a postprocessing bubble."|>]]];
 If[j["LoopMeasureConversion"]=!=1/(2 Pi)^4,Return[Failure["QQMeasureConventionRequired",<||>]]];j];
qqMSbarZeroScaleQ[core_] := Module[{ps,rs,as,q=RGZWorkflow`Engine`q},
 ps=Cases[core,_FeynCalc`PropagatorDenominator,Infinity];
 If[ps==={}||!AllTrue[ps,Length[#]===2&&#[[2]]===0&],Return[False]];
 rs=ps[[All,1]]/.FeynCalc`Momentum[v_,___]:>v;as=Coefficient[Expand[#],q]&/@rs;
 If[!AllTrue[as,MemberQ[{1,-1},#]&],Return[False]];rs=MapThread[Times,{as,rs}];
 And@@Flatten[Table[TrueQ[Expand[qqFFScalarKinematics[FeynCalc`FCI[FeynCalc`SPD[rs[[i]]-rs[[k]],rs[[i]]-rs[[k]]]]]]===0],{i,Length[rs]},{k,Length[rs]}]]];
qqMSbarPieces[j_,name_] := Module[{raw,input,r},
 raw=j["Integrands"][name];
 If[MatchQ[raw,Missing["DeferredNAV",_Integer]],raw=-RGZWorkflow`Engine`rgzQQNAV[j["NAVSource"],raw[[2]]]/.{RGZWorkflow`Engine`\[Alpha]->0,RGZWorkflow`Engine`M->0}];
 If[MemberQ[{"Box1","Box2"},name],raw=qqBoxWardPrecondition[raw,name]];
 input=RGZWorkflow`Engine`rgzQQSplit[qqRestoreAffineSquares[raw],0,0];If[FailureQ[input],Return[input]];
 r=qqLinearizeChains[FeynCalc`FCI[input]];
 r=qqRouteApart[FeynCalc`FeynAmpDenominatorCombine[r,FeynCalc`FCI->True]];
 r=qqNAVScalarCoordinates[r,name];r=qqCancelRoutePowers[r];qqTensorPieces[r]];
qqMSbarFF[input_,finger_,co_] := Block[{$results=$results,qqFFPVCache=<||>},Module[{r},
 r=qqFFBuild[<|"Amplitude"->input,"Fingerprint"->finger,"LoopOrder"->1|>,True,co,False];If[FailureQ[r],r,RGZResult["QQFormFactors"]]]];
(* IDs are local to each report; match actual color and spin expressions. *)
qqMSbarMap[ff_] := Association[Map[Function[row,With[{c=ff["ColorBasis"][[row["ColorID"]]],s=ff["SpinStructures"][[row["SpinID"]]]},HoldComplete[c,s]->row["FormFactor"]]],ff["Rows"]]];
qqMSbarLookup[map_,c_,spin_] := With[{a=c,b=spin},Lookup[map,HoldComplete[a,b],0]];

Options[RGZQQScalelessLedger]={"JobFile"->Automatic,"IntegralSeconds"->60,"CoefficientSeconds"->30};
RGZQQScalelessLedger[seconds_:1200,OptionsPattern[]] := Module[
 {j,r,sec=OptionValue["IntegralSeconds"],co=OptionValue["CoefficientSeconds"],key,dir,parts={"AV1","AV2","Box1","Box2","NAV1","NAV2"},records={},pieces,selected,terms,v,u,part,file,cache,saved,coreFile,coreKey,raw,ff,out,stop=Unique["rawLedgerStop"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,sec,co}),Return[Failure["QQMSbarArguments",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 If[!qqGaugeSymbolicParametersQ[],Return[Failure["SymbolicParametersRequired",<||>]]];
 $results=KeyDrop[$results,{"QQScalelessLedger"}];j=qqMSbarJob[OptionValue["JobFile"]];If[FailureQ[j],Return[j]];
 r=engineGet["RGZ_Quark_Amplitude.wl",False];If[FailureQ[r],Return[r]];
 key=Hash[{"QQScalelessLedger/2",j["Fingerprint"],j["Integrands"],j["NAVSource"],FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringMSbar.wl"}],"SHA256"],DownValues[FeynCalc`TID],DownValues[FeynCalc`PaVeUVPart],$Version},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-scaleless-ledger-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQScalelessLedger",Catch[
 Do[part=parts[[n]];qqProgressOperation["Scaleless scan "<>part];qqProgressUpdate[<|"Stage"->"QQScalelessLedger:"<>part,"Index"->n,"Total"->6,"StepLimitSeconds"->sec|>];file=FileNameJoin[{dir,part<>".wxf"}];
 cache=If[FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],$Failed];
 If[AssociationQ[cache]&&Lookup[cache,"Key",None]===key&&Lookup[cache,"Part",None]===part&&!qqFFInvalidQ[Lookup[cache,"RawUVShift",$Failed]],qqProgressUpdate[<|"CacheState"->"Reused","ReusedCheckpoints"->(Lookup[$qqProgress,"ReusedCheckpoints",0]+1)|>];AppendTo[records,cache];Continue[]];
 pieces=TimeConstrained[qqMSbarPieces[j,part],sec,$TimedOut];
 If[qqFFInvalidQ[pieces],Throw[Failure["QQScalelessScanStopped",<|"Part"->part,"Seconds"->sec|>],stop]];
 selected=Select[pieces,qqMSbarZeroScaleQ[Last[#]]&];terms={};
 Do[qqProgressUpdate[<|"AlgebraGroupIndex"->i,"AlgebraGroupTotal"->Length[selected],"InputLeaves"->LeafCount[selected[[i,2]]],"ItemStarted"->AbsoluteTime[]|>];qqProgressOperation["Scaleless UV tensor "<>part<>" "<>ToString[i]<>" / "<>ToString[Length[selected]]];
  coreKey=Hash[{key,selected[[i,2]]},"SHA256"];coreFile=FileNameJoin[{dir,"core-"<>IntegerString[coreKey,16,64]<>".wxf"}];
  cache=If[FileExistsQ[coreFile],Quiet[Check[Import[coreFile,"WXF"],$Failed]],$Failed];
  If[AssociationQ[cache]&&Lookup[cache,"Key",None]===coreKey&&!qqFFInvalidQ[Lookup[cache,"Value",$Failed]],u=cache["Value"];qqProgressUpdate[<|"CacheState"->"Reused","ReusedCheckpoints"->(Lookup[$qqProgress,"ReusedCheckpoints",0]+1)|>],
   u=TimeConstrained[Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=True},v=RGZWorkflow`Engine`rgzQQReduce[selected[[i,2]]];FeynCalc`PaVeUVPart[v]],sec,$TimedOut];
   If[qqFFInvalidQ[u]||!FreeQ[u,FeynCalc`TID|FeynCalc`FeynAmpDenominator|FeynCalc`PaVeUVPart|RGZWorkflow`Engine`q],Throw[Failure["QQScalelessTensorStopped",<|"Part"->part,"Index"->i,"Seconds"->sec|>],stop]];
   saved=atomicWXF[coreFile,<|"Key"->coreKey,"Value"->u|>];If[FailureQ[saved],Throw[saved,stop]];qqProgressUpdate[<|"CacheState"->"Saved","ComputedCheckpoints"->(Lookup[$qqProgress,"ComputedCheckpoints",0]+1)|>]];
  AppendTo[terms,selected[[i,1]] u],{i,Length[selected]}];
 cache=<|"Key"->key,"Part"->part,"TensorCores"->Length[pieces],"ScalelessCores"->Length[selected],"RawUVShift"->Total[terms]|>;
 saved=atomicWXF[file,cache];If[FailureQ[saved],Throw[saved,stop]];AppendTo[records,cache],{n,Length[parts]}];
 raw=RGZWorkflow`Engine`rgzQQRestorePoleMasses[FeynCalc`FCE[Total[Lookup[records,"RawUVShift"]]],0,0] j["LoopMeasureConversion"]/I;
 ff=qqMSbarFF[raw,j["Fingerprint"],co];If[FailureQ[ff],Throw[ff,stop]];True,stop],seconds];
 If[FailureQ[r],Return[r]];If[r=!=True,Return[Failure["QQScalelessLedgerIncomplete",<||>]]];
 out=<|"Schema"->"RGZQQScalelessLedger/2","ModelFingerprint"->modelKey[],"ModelContent"->modelContent[],"Status"->"RawScalelessCoreLedger","Key"->key,"SourceFingerprint"->j["Fingerprint"],"Gauge"->0,"RegulatorMass"->0,"Parts"->(KeyDrop[#,"RawUVShift"]&/@records),
 "UVShiftFormFactors"->ff,"CoreScanComplete"->True,"ScalelessLedgerComplete"->False,
 "Scope"->"Explicit scaleless tensor cores in all six probe graphs; earlier mixed-self mass-limit transformations are not inferred",
 "File"->FileNameJoin[{dir,"ledger.wxf"}]|>;
 r=atomicWXF[out["File"],out];If[FailureQ[r],Return[r]];storeResult["QQScalelessLedger",out];
 <|"Status"->out["Status"],"Name"->"QQScalelessLedger","Rows"->Length[ff["Rows"]],"Statistics"-><|"ScalelessCores"->Total[Lookup[records,"ScalelessCores"]],"CoreScanComplete"->True|>,"File"->out["File"]|>];

Options[RGZQQMSbarCounterterms]={"ClosedQuarkFlavors"->Automatic};
RGZQQMSbarCounterterms[OptionsPattern[]] := Module[{g=FeynCalc`SMP["g_s"],n=FeynCalc`SUNN,d=FeynCalc`SMP["Delta_UV"],nf=OptionValue["ClosedQuarkFlavors"],content,ag,aa,r},
 If[nf===Automatic,r=ready[];If[FailureQ[r],Return[r]];content=modelContent[];If[FailureQ[content],Return[content]];nf=content["ClosedQuarkFlavors"]];
 If[!IntegerQ[nf]||nf<0,Return[Failure["QuarkFlavorCount",<||>]]];
 (* Tr(Ta Tb)=delta(ab)/2; retain Nf contributions independently. *)
 ag=(-11 n/6+nf/3) g^2 d/(16 Pi^2);
 aa=(13 n/6-2 nf/3) g^2 d/(16 Pi^2);
 <|"Scheme"->"MSbar","Gauge"->0,"Quenched"->(nf===0),"ClosedQuarkFlavors"->nf,"Coordinates"->"mG^4=2 N g_s^2 gamma^4; independent mG^4 coordinate",
 "ParameterCounterterms"-><|"g_s"->g ag,"m^2"->RGZWorkflow`Engine`m^2 (ag-aa/2),
 "mu^2"->RGZWorkflow`Engine`\[Mu]^2 (ag+aa/2),"mG^4"->RGZWorkflow`Engine`mG^4 (ag-aa/2)|>,
 "Relations"->{"Z_m2=Z_g Z_A^(-1/2)","Z_mu2=Z_g Z_A^(1/2)","Z_gamma2=Z_g^(-1/2) Z_A^(-1/4)","Z_mG4=Z_g^2 Z_gamma2^2"},
 "Sources"->{"https://arxiv.org/pdf/0806.4348","https://arxiv.org/pdf/1708.01543"},"DerivedByFitting"->False|>];

qqMSbarRead[file_,name_] := If[file===Automatic,RGZResult[name],If[StringQ[file]&&FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],$Failed]];
qqMSbarFiniteMaster[m_,scale_] := Which[m===1,1,MatchQ[m,_FeynCalc`A0],First[m](1-Log[First[m]/scale]),
 MatchQ[m,FeynCalc`B0[_,0,0]],2-Log[-First[m]/scale],True,RGZPVFinite[m,scale]];
(* Finite coefficient in the common dimensional regulator; the PV finite
   masters subtract UV and IR poles with the same bar-epsilon convention.
   No four-dimensional Fierz identity or evanescent spin structure is used. *)
qqMSbarFiniteRow[pairs_,table_,scale_,seconds_] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[{terms={},r2={},c,c0,u,i,u1,i1,i2,z,poles,idx=0},
 Do[idx++;qqProgressOperation["Finite PV coefficient "<>ToString[idx]<>" / "<>ToString[Length[pairs]]];qqProgressUpdate[<|"Master"->pair[[1]],"ScalarMasterIndex"->idx,"ScalarMasterTotal"->Length[pairs]|>];c=pair[[2]];qqProgressUpdate[<|"InputLeaves"->LeafCount[c],"ItemStarted"->AbsoluteTime[],"StepLimitSeconds"->seconds|>];c0=TimeConstrained[c/.D->4,seconds,$TimedOut];
 If[qqFFInvalidQ[c0],Throw[Failure["QQMSbarFiniteCoefficientStopped",<||>],rgzFunctionReturnTag]];
 AppendTo[terms,c0 qqMSbarFiniteMaster[pair[[1]],scale]];
 If[!FreeQ[c,D]&&pair[[1]]=!=1,
  poles=If[AssociationQ[table],Lookup[table,Key[pair[[1]]],$Failed],$Failed];
  If[!AssociationQ[poles],Throw[Failure["QQMSbarFinitePolesRequired",<|"Master"->pair[[1]]|>],rgzFunctionReturnTag]];
  u=Lookup[poles,"UV",$Failed];i=Lookup[poles,"IR",$Failed];
  If[qqFFInvalidQ[{u,i}]||!FreeQ[{u,i},qqFFMasterHeads|FeynCalc`PaVeUVPart|FeynCalc`PaXEvaluateIR|_ConditionalExpression|_Piecewise|_X`PVC|_X`PVD],
   Throw[Failure["QQMSbarFinitePolesRequired",<|"Master"->pair[[1]]|>],rgzFunctionReturnTag]];
  z=TimeConstrained[{qqUVIRPole[u/.D->4-2 FeynCalc`EpsilonUV,FeynCalc`EpsilonUV],qqUVIRPole[i,FeynCalc`EpsilonIR]},seconds,$TimedOut];
  If[qqFFInvalidQ[z],Throw[Failure["QQMSbarFinitePoleExpansionStopped",<|"Master"->pair[[1]]|>],rgzFunctionReturnTag]];{u,i}=z;
  u1=Coefficient[Expand[u],FeynCalc`EpsilonUV,-1];
  i1=Coefficient[Expand[i],FeynCalc`EpsilonIR,-1];i2=Coefficient[Expand[i],FeynCalc`EpsilonIR,-2];
  If[Expand[u-u1/FeynCalc`EpsilonUV]=!=0||Expand[i-i1/FeynCalc`EpsilonIR-i2/FeynCalc`EpsilonIR^2]=!=0,
   Throw[Failure["QQMSbarFinitePoleOrderUnsupported",<|"Master"->pair[[1]]|>],rgzFunctionReturnTag]];
  z=TimeConstrained[-2 (D[c,D]/.D->4)(u1+i1)+2 (D[c,{D,2}]/.D->4)i2,seconds,$TimedOut];
  If[qqFFInvalidQ[z],Throw[Failure["QQMSbarRationalTermStopped",<||>],rgzFunctionReturnTag]];AppendTo[r2,z]],{pair,pairs}];
 z=TimeConstrained[qqUVIRCompactPole[Total[r2]],seconds,$TimedOut];If[qqFFInvalidQ[z],z=Total[r2]];
 <|"FiniteIntegralPart"->Total[terms],"DimensionalRationalPart"->z,"FiniteFormFactor"->Total[terms]+z|>],rgzFunctionReturnTag]];

(* A row checkpoint supplies only finite coefficients, never verified pole fields. *)
qqMSbarFinitePacketQ[p_] := AssociationQ[p]&&
 Sort[Keys[p]]===Sort[{"FiniteIntegralPart","DimensionalRationalPart","FiniteFormFactor"}]&&
 !qqFFInvalidQ[p]&&p["FiniteFormFactor"]===p["FiniteIntegralPart"]+p["DimensionalRationalPart"]&&
 FreeQ[p,FeynCalc`EpsilonUV|FeynCalc`EpsilonIR|FeynCalc`Epsilon|FeynCalc`SMP["Delta_UV"]];
qqMSbarRowBasisQ[p_] := ListQ[Lookup[p,"ColorBasis",None]]&&ListQ[Lookup[p,"SpinStructures",None]]&&
 AllTrue[p["Rows"],Function[row,AssociationQ[row]&&IntegerQ[Lookup[row,"ColorID",None]]&&IntegerQ[Lookup[row,"SpinID",None]]&&
  1<=row["ColorID"]<=Length[p["ColorBasis"]]&&1<=row["SpinID"]<=Length[p["SpinStructures"]]]]&&
 Length[DeleteDuplicates[({#["ColorID"],#["SpinID"]}&/@p["Rows"])]]===Length[p["Rows"]];
qqMSbarFiniteAmplitude[p_] := Total[(p["ColorBasis"][[#["ColorID"]]] p["SpinStructures"][[#["SpinID"]]] #["FiniteFormFactor"])&/@p["Rows"]];

(* Reuse only a complete, internally consistent artifact with the exact source key.
   A failed pole check or an interrupted finite row is never a completed cache. *)
qqMSbarReusableQ[p_,key_,finite_,count_] := AssociationQ[p]&&Lookup[p,"Key",None]===key&&
 !qqFFInvalidQ[p]&&TrueQ[Lookup[p,"MSbarUVClosed",False]]&&TrueQ[Lookup[p,"IRPolesCancelled",False]]&&
 Lookup[p,"FinitePartBuilt",None]===finite&&Lookup[p,"PostprocessingJacobianInsertions",None]===0&&Lookup[p,"ClosedQuarkFlavors",None]===2&&TrueQ[Lookup[p,"JacobianGhostIncluded",False]]&&
 ListQ[Lookup[p,"Rows",None]]&&Length[p["Rows"]]===count&&qqMSbarRowBasisQ[p]&&
 AllTrue[p["Rows"],Lookup[#,"RemainingUVPole",None]===0&&Lookup[#,"RemainingIRPole",None]===0&]&&
 (!finite||(AllTrue[p["Rows"],qqMSbarFinitePacketQ[KeyTake[#,{"FiniteIntegralPart","DimensionalRationalPart","FiniteFormFactor"}]]&]&&And@@(KeyExistsQ[p,#]&/@{"OneLoopFiniteAmplitude","TreeAmplitude","Amplitude"})&&
 p["OneLoopFiniteAmplitude"]===qqMSbarFiniteAmplitude[p]&&
 p["Amplitude"]===p["TreeAmplitude"]+p["OneLoopFiniteAmplitude"]&&
 FreeQ[Lookup[p["Rows"],"FiniteFormFactor",Missing[]],FeynCalc`EpsilonUV|FeynCalc`EpsilonIR|FeynCalc`Epsilon|FeynCalc`SMP["Delta_UV"]]));

Options[RGZQQMSbar]={"AmplitudeFile"->Automatic,"UVIRFile"->Automatic,"ScalelessLedgerFile"->Automatic,"CoefficientSeconds"->30,
 "BuildFinitePart"->True,"RenormalizationScaleSquared"->RGZWorkflow`Parameters`muR2};
RGZQQMSbar[seconds_:1200,OptionsPattern[]] := Module[
 {a,u,l,ff,r,co=OptionValue["CoefficientSeconds"],finiteQ=OptionValue["BuildFinitePart"],scale=OptionValue["RenormalizationScaleSquared"],
 ct,ctFF,ctMap,shiftMap,rows={},row,spin,shift,pole,uv,ir,key,file,dir,rowFile,rowKey,cached,saved,out,packet,fin,extra,finiteAmp,sourceRow,allUV,allIR,stop=Unique["msbarStop"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,co})||!MemberQ[{True,False},finiteQ]||qqFFInvalidQ[scale]||NumericQ[scale]&&!TrueQ[scale>0],Return[Failure["QQMSbarArguments",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 If[!qqGaugeSymbolicParametersQ[],Return[Failure["SymbolicParametersRequired",<||>]]];
 $results=KeyDrop[$results,{"QQMSbar"}];
 a=If[OptionValue["AmplitudeFile"]===Automatic,qqFFReadPacket[Automatic],qqMSbarRead[OptionValue["AmplitudeFile"],"QQAmplitude"]];
 u=qqMSbarRead[OptionValue["UVIRFile"],"QQUVIR"];l=qqMSbarRead[OptionValue["ScalelessLedgerFile"],"QQScalelessLedger"];
 If[!AssociationQ[a]||!AssociationQ[u]||!AssociationQ[l]||!TrueQ[Lookup[u,"CompleteRepresentedUV",False]]||!TrueQ[Lookup[u,"CompleteRepresentedIR",False]]||!TrueQ[Lookup[l,"CoreScanComplete",False]],Return[Failure["QQMSbarPrerequisites",<|"Action"->"Prepare pure-loop RGZQQUVIR and RGZQQScalelessLedger."|>]]];
 If[!And@@(KeyExistsQ[u,#]&/@{"MasterPoles","Rows","ColorBasis","SpinStructures","FormFactorsFile","InputSHA256","SourceFingerprint","Key"})||!AssociationQ[u["MasterPoles"]]||!ListQ[u["Rows"]]||!ListQ[Lookup[l,"Parts",None]]||!AssociationQ[Lookup[l,"UVShiftFormFactors",None]],Return[Failure["QQMSbarPrerequisites",<||>]]];
 If[Lookup[a,"Gauge",None]=!=0||Lookup[a,"RegulatorMass",None]=!=0,Return[Failure["QQMSbarLandauM0Only",<||>]]];
 If[Lookup[a,"JacobianGhostIncluded",False]=!=True||Lookup[a,"ClosedQuarkFlavors",None]=!=2||Lookup[a,"ModelFingerprint",None]=!=modelKey[]||Lookup[a,"ModelContent",None]=!=modelContent[],Return[Failure["QQCompletedModelRequired",<|"Action"->"Use the new two-flavor amplitude with eta already in the mixed self energy. No postprocessing insertion is performed."|>]]];
 If[Lookup[a,"AssemblyConvention",None]=!="QQPhysicalLoopMeasure/1"||!And@@(KeyExistsQ[a,#]&/@{"OneLoop","Tree","Fingerprint"})||Lookup[u,"SourceFingerprint",None]=!=a["Fingerprint"]||Lookup[l,"SourceFingerprint",None]=!=a["Fingerprint"]||Hash[a["OneLoop"]/I,"SHA256"]=!=Lookup[u,"InputSHA256",None],Return[Failure["QQMSbarSourceMismatch",<||>]]];
 If[TrueQ[finiteQ]&&AnyTrue[Values[u["MasterPoles"]],!TrueQ[Coefficient[Expand[#["IR"]],FeynCalc`EpsilonIR,-2]===0]&],Return[Failure["QQMSbarDoubleIRFiniteUnsupported",<|"Action"->"BuildFinitePart -> False retains the pole-closure check; double-IR finite scheme conversion is not implemented."|>]]];
 ff=qqUVIRReadFF[u["FormFactorsFile"]];If[FailureQ[ff],Return[ff]];
 If[ff["InputSHA256"]=!=u["InputSHA256"]||ff["SourceFingerprint"]=!=a["Fingerprint"],Return[Failure["QQMSbarSourceMismatch",<||>]]];
 If[!And@@(MemberQ[Lookup[l["Parts"],"Part",{}],#]&/@{"AV1","AV2","Box1","Box2","NAV1","NAV2"}),Return[Failure["QQMSbarIncompleteCoreScan",<||>]]];
 key=Hash[{"QQMSbar/3",u["Key"],l["Key"],a["Fingerprint"],FileHash[u["FormFactorsFile"],"SHA256"],scale,finiteQ,FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringMSbar.wl"}],"SHA256"]},"SHA256"];
 file=FileNameJoin[{cacheDirectory[],"qq-msbar-"<>IntegerString[key,16,64]<>".wxf"}];
 cached=If[FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],$Failed];
 If[qqMSbarReusableQ[cached,key,finiteQ,Length[u["Rows"]]],
  storeResult["QQMSbar",cached];
  Return[<|"Status"->cached["Status"],"Name"->"QQMSbar","Rows"->Length[cached["Rows"]],"Reused"->True,
   "Statistics"->KeyTake[cached,{"MSbarUVClosed","IRPolesCancelled","FinitePartBuilt","JacobianGhostIncluded","Renormalized"}],"File"->file|>]];
 dir=FileNameJoin[{cacheDirectory[],"qq-msbar-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQMSbar",Catch[
 qqProgressOperation["Derive two-flavor local parameter counterterm"];
 ct=qqLocalParameterCT[a["Tree"]/I,RGZQQMSbarCounterterms[]["ParameterCounterterms"]];If[FailureQ[ct],Throw[ct,stop]];
 ctFF=qqMSbarFF[ct,a["Fingerprint"],co];
 If[FailureQ[ctFF],Throw[Failure["QQMSbarProjectionStopped",<||>],stop]];
 ctMap=qqMSbarMap[ctFF];shiftMap=qqMSbarMap[l["UVShiftFormFactors"]];
 Do[row=u["Rows"][[n]];spin=u["SpinStructures"][[row["SpinID"]]];
 qqProgressOperation["Check MSbar poles "<>ToString[n]<>" / "<>ToString[Length[u["Rows"]]]];qqProgressUpdate[<|"Stage"->"QQMSbar:Poles","Index"->n,"Total"->Length[u["Rows"]],"StepLimitSeconds"->co|>];
 shift=TimeConstrained[qqUVIRCompactPole[qqUVIRPole[qqMSbarLookup[shiftMap,u["ColorBasis"][[row["ColorID"]]],spin]/.D->4-2 FeynCalc`EpsilonUV,FeynCalc`EpsilonUV]],co,$TimedOut];
 pole=qqMSbarLookup[ctMap,u["ColorBasis"][[row["ColorID"]]],spin]/.FeynCalc`SMP["Delta_UV"]->1/FeynCalc`EpsilonUV;
 uv=TimeConstrained[qqUVIRCompactPole[row["UVPole"]+shift+pole],co,$TimedOut];
 ir=TimeConstrained[qqUVIRCompactPole[row["IRPole"]-(shift/.FeynCalc`EpsilonUV->FeynCalc`EpsilonIR)],co,$TimedOut];
 If[qqFFInvalidQ[{uv,ir,shift}],Throw[Failure["QQMSbarPoleCheckStopped",<|"Index"->n|>],stop]];
 AppendTo[rows,Join[KeyTake[row,{"ColorID","SpinID"}],<|"ScalelessUVShift"->shift,"ScalelessIRShift"->-(shift/.FeynCalc`EpsilonUV->FeynCalc`EpsilonIR),"JacobianContributionSource"->"BaseModelMixedSelfEnergy","LocalUVPoleCounterterm"->pole,"RemainingUVPole"->uv,"RemainingIRPole"->ir|>]],{n,Length[u["Rows"]]}];
 (* Reject extra counterterm/Jacobian structures instead of silently dropping them. *)
 extra=Select[Join[Keys[ctMap],Keys[shiftMap]],!MemberQ[With[{c=u["ColorBasis"][[#["ColorID"]]],s=u["SpinStructures"][[#["SpinID"]]]},HoldComplete[c,s]]&/@u["Rows"],#]&];
 If[extra=!={},Throw[Failure["QQMSbarAdditionalStructures",<||>],stop]];
 allUV=AllTrue[rows,#["RemainingUVPole"]===0&];allIR=AllTrue[rows,#["RemainingIRPole"]===0&];
 If[TrueQ[finiteQ]&&allUV&&allIR,
  Do[qqProgressUpdate[<|"Stage"->"QQMSbar:Finite","Index"->n,"Total"->Length[rows],"StepLimitSeconds"->co|>];qqProgressOperation["Build finite MSbar PV row "<>ToString[n]<>" / "<>ToString[Length[rows]]];
   rowFile=FileNameJoin[{dir,"finite-"<>ToString[n]<>".wxf"}];rowKey=Hash[{key,n,rows[[n]]},"SHA256"];
   cached=If[FileExistsQ[rowFile],Quiet[Check[Import[rowFile,"WXF"],$Failed]],$Failed];
   If[AssociationQ[cached]&&Lookup[cached,"Key",None]===rowKey&&qqMSbarFinitePacketQ[Lookup[cached,"Value",None]],qqProgressUpdate[<|"CacheState"->"Reused","ReusedCheckpoints"->(Lookup[$qqProgress,"ReusedCheckpoints",0]+1)|>];rows[[n]]=Join[rows[[n]],cached["Value"]];Continue[]];
   sourceRow=SelectFirst[ff["Rows"],ff["ColorBasis"][[#["ColorID"]]]===u["ColorBasis"][[rows[[n]]["ColorID"]]]&&ff["SpinStructures"][[#["SpinID"]]]===u["SpinStructures"][[rows[[n]]["SpinID"]]]&,Missing[]];
   If[!AssociationQ[sourceRow],Throw[Failure["QQMSbarFiniteSourceRow",<|"Index"->n|>],stop]];
   fin=qqMSbarFiniteRow[sourceRow["MasterCoefficients"],u["MasterPoles"],scale,co];If[FailureQ[fin],Throw[fin,stop]];
   If[!qqMSbarFinitePacketQ[fin],Throw[Failure["QQMSbarFiniteRowIncomplete",<|"Index"->n|>],stop]];
   spin=u["SpinStructures"][[rows[[n]]["SpinID"]]];
   qqProgressOperation["Save finite MSbar row "<>ToString[n]];
   saved=atomicWXF[rowFile,<|"Key"->rowKey,"Value"->fin|>];If[FailureQ[saved],Throw[saved,stop]];
   qqProgressUpdate[<|"CacheState"->"Saved","ComputedCheckpoints"->(Lookup[$qqProgress,"ComputedCheckpoints",0]+1)|>];rows[[n]]=Join[rows[[n]],fin],{n,Length[rows]}]];True,stop],seconds];
 If[FailureQ[r],Return[r]];If[r=!=True,Return[Failure["QQMSbarIncomplete",<||>]]];
 file=FileNameJoin[{cacheDirectory[],"qq-msbar-"<>IntegerString[key,16,64]<>".wxf"}];
 out=<|"Status"->If[allUV&&allIR,If[finiteQ,"MSbarFiniteRepresentation","MSbarPoleClosureVerified"],"MSbarPoleClosureFailed"],"Key"->key,"ModelFingerprint"->modelKey[],"ModelContent"->modelContent[],"SourceFingerprint"->a["Fingerprint"],"InputSHA256"->u["InputSHA256"],
 "Gauge"->0,"RegulatorMass"->0,"PointTest"->False,"Scheme"->"MSbar","RenormalizationScaleSquared"->scale,"ColorBasis"->u["ColorBasis"],"SpinStructures"->u["SpinStructures"],"Rows"->rows,
 "MSbarUVClosed"->allUV,"IRPolesCancelled"->allIR,"FinitePartBuilt"->(TrueQ[finiteQ]&&allUV&&allIR),"JacobianGhostIncluded"->True,"ClosedQuarkFlavors"->2,"JacobianContributionSource"->"BaseModelMixedSelfEnergy","PostprocessingJacobianInsertions"->0,"ActionCompletion"->"eta and two massless quarks included in base FR/FA model and raw mixed loops",
 "Counterterms"->RGZQQMSbarCounterterms[],"BareInputModified"->False,
 "LoopMeasure"->"Convert fixed-4D TID measure by (2 Pi)^(2 epsilon); native PV c=-EulerGamma-Log[Pi] becomes physical MSbar c=-EulerGamma+Log[4 Pi]",
 "IRValidityConditions"->Join[Lookup[u,"IRValidityConditions",{}],{scale>0}],
 "UVRenormalized"->(allUV&&allIR&&TrueQ[finiteQ]),"Renormalized"->False,"ScalelessLedgerComplete"->False,"FullSMatrixVerified"->False,
 "Scope"->"Computed Landau, M=0 two-flavor one-loop amplitude with eta included in the base mixed self energy and raw probe-core ledger. Earlier full mixed-matrix mass-limit ledger not certified; no automatic full-S-matrix promotion.",
 "FiniteConstruction"->"SourceBoundCommonEpsilon/1","FormFactorsSHA256"->FileHash[u["FormFactorsFile"],"SHA256"],
 "FinitePVConvention"->"RGZPVFinite is the constant term after common UV/IR bar-epsilon subtraction at the specified scale; rational coefficient epsilon terms are retained separately. Finite multiscale masters remain exact functions, not numerical values.","File"->file|>;
 If[allUV&&allIR&&TrueQ[finiteQ],finiteAmp=Total[(u["ColorBasis"][[#["ColorID"]]] u["SpinStructures"][[#["SpinID"]]] #["FiniteFormFactor"])&/@rows];
  If[!FreeQ[Lookup[rows,"FiniteFormFactor"],FeynCalc`EpsilonUV|FeynCalc`EpsilonIR|FeynCalc`Epsilon|FeynCalc`SMP["Delta_UV"]],Return[Failure["QQMSbarResidualPoleLabel",<||>]]];
  out=Join[out,<|"OneLoopFiniteAmplitude"->finiteAmp,"TreeAmplitude"->a["Tree"]/I,"Amplitude"->a["Tree"]/I+finiteAmp,"AmplitudeConvention"->"M = M_tree + M_oneLoop; no extra I factor"|>];];
 r=atomicWXF[file,out];If[FailureQ[r],Return[r]];storeResult["QQMSbar",out];
 <|"Status"->out["Status"],"Name"->"QQMSbar","Rows"->Length[rows],"Statistics"->KeyTake[out,{"MSbarUVClosed","IRPolesCancelled","FinitePartBuilt","JacobianGhostIncluded","Renormalized"}],"File"->file|>];
