(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringIRPoles.wl"}]];
(* Pole analysis of the represented pure-loop form factors. Never infer a
   lost scaleless UV/IR ledger or fit nonlocal counterterms to force zero. *)
Options[RGZLoadFeynHelpers]={"Directory"->Automatic};
RGZLoadFeynHelpers[OptionsPattern[]] := Module[{r=fcReady[],dir=OptionValue["Directory"],file},
 If[FailureQ[r],Return[r]];
 If[Length[DownValues[FeynCalc`PaXEvaluateIR]]>0,Return[<|"Status"->"FeynHelpersLoaded","Reused"->True|>]];
 If[dir===Automatic,dir=FileNameJoin[{FeynCalc`$FeynCalcDirectory,"AddOns","FeynHelpers"}]];
 If[!StringQ[dir],Return[Failure["FeynHelpersDirectory",<||>]]];file=FileNameJoin[{dir,"FeynHelpers.m"}];
 If[!FileExistsQ[file],Return[Failure["FeynHelpersNotInstalled",<|"File"->file|>]]];
 r=CheckAbort[Block[{$Context="FeynCalc`",$ContextPath={"FeynCalc`","System`"}},
  FeynCalc`FCDeclareHeader[file];Block[{FeynCalc`$FeynHelpersLoadInterfaces={"PackageX"}},Get[file]]],$Aborted];
 If[r===$Aborted||Length[DownValues[FeynCalc`PaXEvaluateIR]]===0,Failure["FeynHelpersLoadFailed",<||>],<|"Status"->"FeynHelpersLoaded"|>]];
qqUVIRPole[x_,eps_] := Module[{z=Normal[Series[x,{eps,0,0}]]},
 If[qqFFInvalidQ[z],Return[Failure["QQPoleExpansionFailed",<||>]]];
 Expand[Total[Table[Coefficient[Expand[z],eps,-k]/eps^k,{k,1,4}]]]];
qqUVIRMaster[m_,use_:True] := Module[{uv,ir,v,masses,certificate,method="PackageXOrUnsupported",conditions={}},
 If[m===1,Return[<|"UV"->0,"IR"->0,"IRMethod"->"RationalTerm"|>]];
 If[!MatchQ[Head[m],qqFFMasterHeads],Return[<|"UV"->Missing["UVUnsupported"],"IR"->Missing["IRUnsupported"]|>]];
 uv=CheckAbort[FeynCalc`PaVeUVPart[m],$Aborted];
 If[qqFFInvalidQ[uv]||!FreeQ[uv,qqFFMasterHeads|FeynCalc`PaVeUVPart],uv=Missing["UVUnsupported"]];
 certificate=qqIREndpointCertificate[m];
 ir=Which[AssociationQ[certificate],method=certificate["IRMethod"];conditions=certificate["IRConditions"];certificate["IR"],
 MatchQ[m,_FeynCalc`A0],0,
 MatchQ[m,FeynCalc`B0[s_,a_,b_]/;!TrueQ[{s,a,b}==={0,0,0}]],0,
 !FreeQ[m,Complex],Missing["ComplexMassIRUnsupported"],
 !TrueQ[use]||Length[DownValues[FeynCalc`PaXEvaluateIR]]===0,Missing["LoadFeynHelpersFirst"],
 True,CheckAbort[FeynCalc`PaXEvaluateIR[m,FeynCalc`PaXImplicitPrefactor->1,FeynCalc`FCVerbose->0],$Aborted]];
 If[!MatchQ[ir,_Missing]&&(qqFFInvalidQ[ir]||!FreeQ[ir,qqFFMasterHeads|FeynCalc`PaXEvaluateIR|_ConditionalExpression|_Piecewise|_X`PVC|_X`PVD]),ir=Missing["IRUnsupported"]];
 <|"UV"->uv,"IR"->ir,"IRMethod"->If[method=!="PackageXOrUnsupported",method,If[TrueQ[ir===0],"ScalarTadpoleOrBubbleIRFinite",method]],"IRConditions"->conditions|>];
qqUVIRReadFF[file_] := Module[{p,amp},
 p=If[file===Automatic,RGZResult["QQFormFactors"],If[StringQ[file]&&FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],$Failed]];
 If[!AssociationQ[p]||Lookup[p,"LoopOrder",None]=!=1||!TrueQ[Lookup[p,"ReconstructionVerified",False]]||!And@@(KeyExistsQ[p,#]&/@{"Key","File","InputSHA256","SourceFingerprint","ColorBasis","SpinStructures"})||!ListQ[Lookup[p,"Rows",None]]||!AllTrue[Lookup[p,"Rows",{}],AssociationQ[#]&&qqFFVectorPairsQ[Lookup[#,"MasterCoefficients",None]]&],
  Return[Failure["QQOneLoopFormFactorsFirst",<|"Action"->"RGZQQFormFactors[..., LoopOrder -> 1] or supply File -> its result file."|>]]];
 If[file===Automatic,
  amp=qqFFReadPacket[Automatic];If[FailureQ[amp],Return[amp]];
  If[!KeyExistsQ[amp,"OneLoop"]||Lookup[p,"SourceFingerprint",None]=!=Lookup[amp,"Fingerprint",Missing[]]||
   Lookup[p,"InputSHA256",None]=!=Hash[amp["OneLoop"]/I,"SHA256"],Return[Failure["QQOneLoopFormFactorsStale",<||>]]]];p];
Options[RGZQQUVIR]={"File"->Automatic,"IntegralSeconds"->30,"CoefficientSeconds"->30,"UsePackageX"->True};
RGZQQUVIR[seconds_:1200,OptionsPattern[]] := Module[
 {p,r,sec=OptionValue["IntegralSeconds"],co=OptionValue["CoefficientSeconds"],use=OptionValue["UsePackageX"],masters,table=<||>,rows={},item,file,dir,key,z,poles,uvs,irs,uv,ir,cc,epsU=FeynCalc`EpsilonUV,epsI=FeynCalc`EpsilonIR,missing={},out,rowFile,rowKey,savedRow,compact,uvLeaves,irLeaves,stop=Unique["uvirStop"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,sec,co})||!MemberQ[{True,False},use],Return[Failure["QQUVIRArguments",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 $results=KeyDrop[$results,{"QQUVIR","QQRenormalization"}];p=qqUVIRReadFF[OptionValue["File"]];If[FailureQ[p],Return[p]];
 If[use,r=RGZLoadFeynHelpers[]];
 key=Hash[{"QQUVIR/1",p["InputSHA256"],p["Key"],use,FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringRenormalization.wl"}],"SHA256"],
  FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringIRPoles.wl"}],"SHA256"],DownValues[FeynCalc`PaVeUVPart],DownValues[FeynCalc`PaXEvaluateIR],OwnValues[FeynCalc`ScaleMu],$Version},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-uvir-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQUVIR",Catch[
 masters=DeleteDuplicates[Flatten[(First/@#["MasterCoefficients"])&/@p["Rows"],1]];
 Do[qqProgressOperation["UV/IR master "<>ToString[i]<>" / "<>ToString[Length[masters]]];qqProgressUpdate[<|"Index"->i,"Total"->Length[masters],"Master"->masters[[i]]|>];
  file=FileNameJoin[{dir,"master-"<>IntegerString[Hash[masters[[i]],"SHA256"],16,64]<>".wxf"}];
  item=If[FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],$Failed];
  If[AssociationQ[item]&&Lookup[item,"Key",None]===key&&Lookup[item,"Master",None]===masters[[i]]&&AssociationQ[Lookup[item,"Poles",None]]&&FreeQ[Lookup[item,"Poles",None],_Missing|_Failure|$Failed|$TimedOut|$Aborted],z=item["Poles"];qqProgressUpdate[<|"CacheState"->"Reused"|>],
   z=TimeConstrained[qqUVIRMaster[masters[[i]],use],sec,$TimedOut];
   If[z===$TimedOut,Throw[Failure["QQUVIRIntegralTimeout",<|"Stage"->"Master","Seconds"->sec,"File"->file|>],stop]];
   If[!AssociationQ[z],Throw[Failure["QQUVIRIntegralFailed",<|"File"->file|>],stop]];
   (* Unsupported identities may become supported later. Never cache them. *)
   If[FreeQ[z,_Missing],item=atomicWXF[file,<|"Key"->key,"Master"->masters[[i]],"Poles"->z|>];If[FailureQ[item],Throw[item,stop]]]];
  AssociateTo[table,masters[[i]]->z],{i,Length[masters]}];
 Do[uvs={};irs={};missing={};
  rowKey=Hash[{key,p["Rows"][[i]]},"SHA256"];rowFile=FileNameJoin[{dir,"row-"<>IntegerString[rowKey,16,64]<>".wxf"}];
  savedRow=If[FileExistsQ[rowFile],Quiet[Check[Import[rowFile,"WXF"],$Failed]],$Failed];
  If[AssociationQ[savedRow]&&Lookup[savedRow,"Key",None]===rowKey&&AssociationQ[Lookup[savedRow,"Value",None]]&&TrueQ[Lookup[savedRow["Value"],"CompleteRepresentedUV",False]]&&TrueQ[Lookup[savedRow["Value"],"CompleteRepresentedIR",False]]&&Lookup[savedRow["Value"],"Unresolved",None]==={}&&FreeQ[Lookup[savedRow["Value"],{"UVPole","IRPole"},$Failed],_Failure|_Missing|$Failed|$TimedOut|$Aborted],
   qqProgressUpdate[<|"CacheState"->"Reused"|>];AppendTo[rows,savedRow["Value"]];Continue[]];
  qqProgressUpdate[<|"Index"->i,"Total"->Length[p["Rows"]],"Master"->Missing["CoefficientCollection"]|>];
  qqProgressOperation["Collect UV/IR coefficients "<>ToString[i]<>" / "<>ToString[Length[p["Rows"]]]];
  Do[cc=TimeConstrained[Cancel[Together[pair[[2]]]],co,$TimedOut];
   If[qqFFInvalidQ[cc],Throw[Failure["QQUVIRCoefficientTimeout",<|"Seconds"->co,"Parts"->{i}|>],stop]];
   If[TrueQ[(Denominator[cc]/.D->4)===0],AppendTo[missing,<|"Master"->pair[[1]],"Reason"->"CoefficientSingularAtD4RequiresFullExpansion"|>];Continue[]];
   poles=table[pair[[1]]];
   uv=If[MatchQ[poles["UV"],_Missing],poles["UV"],TimeConstrained[qqUVIRPole[(cc poles["UV"])/.D->4-2epsU,epsU],co,$TimedOut]];
   ir=If[MatchQ[poles["IR"],_Missing],poles["IR"],TimeConstrained[qqUVIRPole[(cc poles["IR"])/.D->4-2epsI,epsI],co,$TimedOut]];
   If[!MatchQ[uv,_Missing]&&qqFFInvalidQ[uv]||!MatchQ[ir,_Missing]&&qqFFInvalidQ[ir],Throw[Failure["QQUVIRPoleExpansionStopped",<|"Parts"->{i}|>],stop]];
   If[MatchQ[uv,_Missing],AppendTo[missing,<|"Master"->pair[[1]],"Reason"->uv|>],AppendTo[uvs,uv]];
   If[MatchQ[ir,_Missing],AppendTo[missing,<|"Master"->pair[[1]],"Reason"->ir|>],AppendTo[irs,ir]],{pair,p["Rows"][[i]]["MasterCoefficients"]}];
  uv=Total[uvs];ir=Total[irs];uvLeaves=LeafCount[uv];irLeaves=LeafCount[ir];
  qqProgressOperation["Simplify collected UV pole "<>ToString[i]];
  compact=TimeConstrained[qqUVIRCompactPole[uv],co,$TimedOut];If[!qqFFInvalidQ[compact],uv=compact];
  qqProgressOperation["Simplify collected IR pole "<>ToString[i]];
  compact=TimeConstrained[qqUVIRCompactPole[ir],co,$TimedOut];If[!qqFFInvalidQ[compact],ir=compact];
  AppendTo[rows,<|"ColorID"->p["Rows"][[i]]["ColorID"],"SpinID"->p["Rows"][[i]]["SpinID"],
   "UVPole"->uv,"IRPole"->ir,"UVInputLeaves"->uvLeaves,"IRInputLeaves"->irLeaves,"Unresolved"->missing,
   "CompleteRepresentedUV"->FreeQ[missing,Missing["UVUnsupported"]|"CoefficientSingularAtD4RequiresFullExpansion"],
   "CompleteRepresentedIR"->(missing==={}),"FinitePart"->Missing["NotEvaluated"]|>];
  If[missing==={},savedRow=atomicWXF[rowFile,<|"Key"->rowKey,"Value"->Last[rows]|>];If[FailureQ[savedRow],Throw[savedRow,stop]]],{i,Length[p["Rows"]]}];
 True,stop],seconds];
 If[FailureQ[r],Return[r]];If[r=!=True,Return[Failure["QQUVIRIncomplete",<||>]]];
 out=<|"Schema"->"RGZQQUVIR/1","Status"->"RepresentedPoleAnalysis","LoopOrder"->1,"Key"->key,"SourceFingerprint"->p["SourceFingerprint"],"InputSHA256"->p["InputSHA256"],
 "FormFactorsFile"->p["File"],"ColorBasis"->p["ColorBasis"],"SpinStructures"->p["SpinStructures"],"Rows"->rows,"MasterPoles"->table,
 "PackageXAvailable"->(Length[DownValues[FeynCalc`PaXEvaluateIR]]>0),
 "IRValidityConditions"->DeleteDuplicates[Flatten[Lookup[Values[table],"IRConditions",{}],1]],
 "IRMethods"->Counts[Lookup[Values[table],"IRMethod","Unspecified"]],
 "IRConditional"->(DeleteDuplicates[Flatten[Lookup[Values[table],"IRConditions",{}],1]]=!={}),
 "CompleteRepresentedUV"->AllTrue[rows,TrueQ[#["CompleteRepresentedUV"]]&],"CompleteRepresentedIR"->AllTrue[rows,TrueQ[#["CompleteRepresentedIR"]]&],
 "UVIRSeparated"->False,"Renormalized"->False,"ScalelessLedgerComplete"->False,
 "Outstanding"->{"Recover scaleless UV/IR terms lost before form-factor construction","Verify dimensional loop-measure and scale convention for finite MSbar terms","Endpoint IR formulas are conditional on IRValidityConditions; unsupported cases still require separate analysis"},
 "PoleConvention"->"D=4-2 epsilon; EpsilonUV and EpsilonIR distinct. PV normalization unchanged; no extra loop measure.","File"->FileNameJoin[{dir,"uvir.wxf"}]|>;
 r=atomicWXF[out["File"],out];If[FailureQ[r],Return[r]];storeResult["QQUVIR",out];
 <|"Status"->out["Status"],"Name"->"QQUVIR","Rows"->Length[rows],"File"->out["File"],"Statistics"->KeyTake[out,{"CompleteRepresentedUV","CompleteRepresentedIR","IRConditional","ScalelessLedgerComplete","UVIRSeparated"}]|>];

(* Local bare-parameter shifts: g -> g+delta g, x -> x+delta x.
   Counterterms are inputs from a separate renormalization prescription,
   never fitted per master or per color-spin coefficient. *)
qqLocalParameterCT[tree_,shifts_Association] := Module[{allowed={"g_s","m^2","mu^2","mG^4"},g,x,y,z,expr,vars,back,ct},
 If[Complement[Keys[shifts],allowed]=!={},Return[Failure["QQCountertermParameters",<|"Parts"->allowed|>]]];
 If[!FreeQ[Values[shifts],qqFFMasterHeads|RGZWorkflow`Engine`qqS|RGZWorkflow`Engine`qqT|RGZWorkflow`Engine`qqK1|RGZWorkflow`Engine`qqK2|RGZWorkflow`Engine`qqK3|RGZWorkflow`Engine`qqK4|FeynCalc`EpsilonIR|FeynCalc`SMP["Delta_IR"]],
 Return[Failure["QQNonlocalCounterterm",<|"Explanation"->"Parameter counterterms cannot depend on scattering kinematics, PV integrals or IR poles."|>]]];
 expr=tree/.{FeynCalc`SMP["g_s"]->g,RGZWorkflow`Engine`m^2->x,RGZWorkflow`Engine`\[Mu]^2->y,RGZWorkflow`Engine`mG^4->z};
 If[!FreeQ[expr,RGZWorkflow`Engine`m|RGZWorkflow`Engine`\[Mu]|RGZWorkflow`Engine`mG],Return[Failure["QQTreeParameterCoordinates",<||>]]];
 vars=AssociationThread[allowed,{g,x,y,z}];back={g->FeynCalc`SMP["g_s"],x->RGZWorkflow`Engine`m^2,y->RGZWorkflow`Engine`\[Mu]^2,z->RGZWorkflow`Engine`mG^4};
 ct=Total[KeyValueMap[#2 D[expr,vars[#1]]&,shifts]];ct/.back];
Options[RGZQQRenormalize]={"Mode"->"LocalCounterterms","ParameterCounterterms"-><||>,"CoefficientSeconds"->30};
RGZQQRenormalize[seconds_:1200,OptionsPattern[]] := Module[{u=RGZResult["QQUVIR"],amp,ff,r,mode=OptionValue["Mode"],shifts=OptionValue["ParameterCounterterms"],co=OptionValue["CoefficientSeconds"],ct=0,ctFF,ctMap=<||>,rows={},key,pole,z,local,out,file,orig},
 If[!MemberQ[{"LocalCounterterms","PoleSubtraction"},mode]||!AssociationQ[shifts]||!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,co}),Return[Failure["QQRenormalizationArguments",<||>]]];
 If[mode==="PoleSubtraction"&&shifts=!=<||>,Return[Failure["QQCountertermModeConflict",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];$results=KeyDrop[$results,{"QQRenormalization"}];
 If[!AssociationQ[u]||!TrueQ[Lookup[u,"CompleteRepresentedUV",False]],Return[Failure["QQCompleteUVAnalysisFirst",<|"Action"->"RGZQQUVIR[]; inspect unresolved entries."|>]]];
 ff=qqUVIRReadFF[u["FormFactorsFile"]];If[FailureQ[ff],Return[ff]];
 If[ff["InputSHA256"]=!=u["InputSHA256"]||ff["SourceFingerprint"]=!=u["SourceFingerprint"],Return[Failure["QQUVIRSourceChanged",<||>]]];
 amp=qqFFReadPacket[Automatic];If[FailureQ[amp],Return[amp]];
 If[Lookup[amp,"Fingerprint",None]=!=u["SourceFingerprint"]||!KeyExistsQ[amp,"OneLoop"]||Hash[amp["OneLoop"]/I,"SHA256"]=!=u["InputSHA256"],Return[Failure["QQRenormalizationTaskMismatch",<||>]]];
 If[mode==="LocalCounterterms"&&shifts===<||>,Return[Failure["QQLocalCountertermsRequired",<|"Action"->"Supply ParameterCounterterms for g_s, m^2, mu^2, mG^4; or explicitly choose Mode -> PoleSubtraction for a formal subtraction only."|>]]];
 r=qqGaugeTimed["QQRenormalization",Catch[
 If[mode==="LocalCounterterms",
  ct=qqLocalParameterCT[amp["Tree"]/I,shifts];If[FailureQ[ct],Throw[ct]];
  ctFF=Block[{$results=$results},r=qqFFBuild[<|"Amplitude"->ct,"Fingerprint"->u["SourceFingerprint"],"LoopOrder"->"Counterterm"|>,True,co,False];If[FailureQ[r],r,RGZResult["QQFormFactors"]]];
  If[FailureQ[ctFF],Throw[ctFF]];
  Do[key={row["ColorID"],ctFF["SpinStructures"][[row["SpinID"]]]};AssociateTo[ctMap,key->row["FormFactor"]],{row,ctFF["Rows"]}]];
 Do[key={row["ColorID"],u["SpinStructures"][[row["SpinID"]]]};
  pole=If[mode==="PoleSubtraction",-row["UVPole"],Lookup[ctMap,Key[key],0]];
  If[mode==="LocalCounterterms",ctMap=KeyDrop[ctMap,{key}]];
  z=TimeConstrained[qqUVIRPole[(pole/.FeynCalc`SMP["Delta_UV"]->1/FeynCalc`EpsilonUV)/.D->4-2FeynCalc`EpsilonUV,FeynCalc`EpsilonUV]+row["UVPole"]//Together//Cancel,co,$TimedOut];
  If[qqFFInvalidQ[z],Throw[Failure["QQCountertermCheckStopped",<||>]]];
  orig=SelectFirst[ff["Rows"],#["ColorID"]===row["ColorID"]&&#["SpinID"]===row["SpinID"]&]["FormFactor"];
  AppendTo[rows,Join[KeyTake[row,{"ColorID","SpinID","IRPole","CompleteRepresentedIR"}],<|"Counterterm"->pole,"RemainingUVPole"->z,"SubtractedFormFactor"->orig+pole|>]],{row,u["Rows"]}];
 If[mode==="LocalCounterterms",KeyValueMap[Function[{k,v},AppendTo[rows,<|"ColorID"->k[[1]],"AdditionalSpinStructure"->k[[2]],"Counterterm"->v,"RemainingUVPole"->qqUVIRPole[(v/.FeynCalc`SMP["Delta_UV"]->1/FeynCalc`EpsilonUV)/.D->4-2FeynCalc`EpsilonUV,FeynCalc`EpsilonUV]|>]],ctMap]];
 True],seconds];If[FailureQ[r],Return[r]];If[r=!=True,Return[Failure["QQRenormalizationIncomplete",<||>]]];
 local=AllTrue[rows,TrueQ[#["RemainingUVPole"]===0]&];
 file=FileNameJoin[{cacheDirectory[],"qq-renormalization-"<>IntegerString[Hash[{u["Key"],mode,shifts},"SHA256"],16,64]<>".wxf"}];
 out=<|"Status"->If[mode==="PoleSubtraction","FormalUVPoleSubtraction",If[local,"RepresentedUVClosed","UncancelledUVPoles"]],"Mode"->mode,"Rows"->rows,"ParameterCounterterms"->shifts,
 "SourceFingerprint"->u["SourceFingerprint"],"InputSHA256"->u["InputSHA256"],"RepresentedUVClosed"->local,"LocalParameterCountertermsUsed"->(mode==="LocalCounterterms"),
 "UVIRSeparated"->False,"Renormalized"->False,"IRFinite"->False,"ScalelessLedgerComplete"->False,
 "Scope"->"Represented UV poles only. Local shifts are caller-supplied, not derived. No full MSbar closure, finite part, or physical S-matrix certification.","File"->file|>;
 r=atomicWXF[file,out];If[FailureQ[r],Return[r]];storeResult["QQRenormalization",out];
 <|"Status"->out["Status"],"Name"->"QQRenormalization","Rows"->Length[rows],"File"->file,"Statistics"->KeyTake[out,{"RepresentedUVClosed","LocalParameterCountertermsUsed","ScalelessLedgerComplete","Renormalized"}]|>];

(* Independent raw-integral route. Inputs must still contain loop denominators,
   before scaleless-zero reduction. This does not overwrite QQ jobs. *)
Options[RGZQQUVIRIntegral]={"ImplicitPrefactor"->1/(2 Pi)^(4-2 FeynCalc`Epsilon)};
RGZQQUVIRIntegral[expr_,loop_Symbol,seconds_:120,OptionsPattern[]] := Module[{r,pref=OptionValue["ImplicitPrefactor"],uv,ir,finite,p,file},
 If[!NumericQ[seconds]||!TrueQ[seconds>0],Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 $results=KeyDrop[$results,{"QQUVIRIntegral"}];
 If[qqFFInvalidQ[expr]||!FreeQ[Cases[FeynCalc`FCI[expr],_FeynCalc`PropagatorDenominator|_FeynCalc`StandardPropagatorDenominator,Infinity],Complex],Return[Failure["QQRawUVIRInput",<|"Explanation"->"Supply unreduced real-mass loop denominators; explicit complex masses need a separate continuation."|>]]];
 If[FreeQ[expr,FeynCalc`FeynAmpDenominator|FeynCalc`FAD|FeynCalc`SFAD|FeynCalc`StandardPropagatorDenominator],Return[Failure["QQRawLoopDenominatorsRequired",<||>]]];
 If[!FreeQ[pref,D|loop],Return[Failure["QQRawUVIRPrefactor",<|"Explanation"->"Specify 4-2 Epsilon explicitly, not D, and no loop-momentum dependence."|>]]];
 r=RGZLoadFeynHelpers[];If[FailureQ[r],Return[r]];
 r=qqGaugeTimed["QQRawUVIR",CheckAbort[Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=True},
  FeynCalc`PaXEvaluateUVIRSplit[expr,loop,FeynCalc`PaXImplicitPrefactor->pref,FeynCalc`FCVerbose->0]],Failure["QQRawUVIRAborted",<||>]],seconds];
 If[qqFFInvalidQ[r]||!FreeQ[r,qqFFMasterHeads|FeynCalc`FeynAmpDenominator|FeynCalc`PaXEvaluateUVIRSplit|_ConditionalExpression],Return[Failure["QQRawUVIRUnsupported",<|"Cause"->r|>]]];
 uv=qqUVIRPole[r,FeynCalc`EpsilonUV];ir=qqUVIRPole[r,FeynCalc`EpsilonIR];finite=Expand[r-uv-ir];
 If[!FreeQ[{uv,ir,finite},FeynCalc`Epsilon]||!FreeQ[uv,FeynCalc`EpsilonIR]||!FreeQ[ir,FeynCalc`EpsilonUV]||!FreeQ[finite,FeynCalc`EpsilonUV|FeynCalc`EpsilonIR],Return[Failure["QQRawUVIRResidualEpsilon",<||>]]];
 file=FileNameJoin[{cacheDirectory[],"qq-raw-uvir-"<>IntegerString[Hash[{expr,loop,pref,FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringRenormalization.wl"}],"SHA256"],$Version},"SHA256"],16,64]<>".wxf"}];
 p=<|"Status"->"RawIntegralUVIRSeparated","Input"->expr,"LoopMomentum"->loop,"ImplicitPrefactor"->pref,"Expression"->r,"UVPole"->uv,"IRPole"->ir,"FinitePart"->finite,
  "ReconstructionVerified"->TrueQ[Expand[r-uv-ir-finite]===0],"UVIRSeparated"->True,"Scope"->"This supplied raw integral only; not the full QQ amplitude","Renormalized"->False,"File"->file|>;
 r=atomicWXF[file,p];If[FailureQ[r],Return[r]];storeResult["QQUVIRIntegral",p];<|"Status"->p["Status"],"Name"->"QQUVIRIntegral","File"->file|>];

(* A generic-D PV identity can discard scaleless zero = UV - IR.
   Compare its original and reduced UV poles before taking D=4. This ledger
   covers only this saved identity boundary, never earlier TID/mass limits. *)
Options[RGZQQPVUVIRLedger]={"File"->Automatic,"CoefficientSeconds"->30};
RGZQQPVUVIRLedger[seconds_:1200,OptionsPattern[]] := Module[
 {p,r,co=OptionValue["CoefficientSeconds"],table=<||>,shifts=<||>,identities,groups,masters,u,before,after,shift,rows={},out,file,key,dir,rowFile,savedRow,singular,stop=Unique["pvLedgerStop"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,co}),Return[Failure["QQPVLedgerArguments",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 $results=KeyDrop[$results,{"QQPVUVIRLedger"}];p=qqUVIRReadFF[OptionValue["File"]];If[FailureQ[p],Return[p]];
 groups=Lookup[p,"PVReductionGroups",Missing["Absent"]];
 If[!ListQ[groups]||!AllTrue[groups,AssociationQ[#]&&qqFFVectorPairsQ[Lookup[#,"Before",None]]&&qqFFVectorPairsQ[Lookup[#,"After",None]]&&ListQ[Lookup[#,"Identities",None]]&&AllTrue[Lookup[#,"Identities",{}],MatchQ[#,{_,_List}]&&qqFFVectorPairsQ[Last[#]]&]&],
  Return[Failure["QQPVProvenanceRequired",<|"Action"->"Rebuild LoopOrder -> 1 form factors with the current workflow; diagram caches can be reused."|>]]];
 key=Hash[{"QQPVUVIRLedger/1",p["Key"],groups,FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringRenormalization.wl"}],"SHA256"],DownValues[FeynCalc`PaVeUVPart]},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-pv-uvir-ledger-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQPVUVIRLedger",Catch[
 identities=DeleteDuplicates[Flatten[Lookup[groups,"Identities"],1]];
 masters=DeleteDuplicates[Flatten[Map[Prepend[First/@Last[#],First[#]]&,identities],1]];
 Do[qqProgressOperation["PV ledger UV master "<>ToString[i]<>" / "<>ToString[Length[masters]]];
  u=TimeConstrained[If[masters[[i]]===1,0,FeynCalc`PaVeUVPart[masters[[i]]]],co,$TimedOut];
  If[qqFFInvalidQ[u]||!FreeQ[u,qqFFMasterHeads|FeynCalc`PaVeUVPart],Throw[Failure["QQPVLedgerMasterStopped",<|"Master"->masters[[i]],"Seconds"->co|>],stop]];
  AssociateTo[table,masters[[i]]->u],{i,Length[masters]}];
 (* Cancel each small identity before multiplying by large amplitude coefficients. *)
 Do[qqProgressOperation["PV ledger identity "<>ToString[i]<>" / "<>ToString[Length[identities]]];
  u=identities[[i]];
  If[AnyTrue[Last[u],AnyTrue[Cases[#[[2]],Power[b_,n_Integer?Negative]/;!FreeQ[b,D]:>b,Infinity],TrueQ[(#/.D->4)===0]&]&],Throw[Failure["QQPVLedgerSingularIdentity",<|"Master"->First[u]|>],stop]];
  shift=TimeConstrained[qqUVIRCompactPole[qqUVIRPole[(table[First[u]]-Total[(#[[2]] table[#[[1]]])&/@Last[u]])/.D->4-2 FeynCalc`EpsilonUV,FeynCalc`EpsilonUV]],co,$TimedOut];
  If[qqFFInvalidQ[shift],Throw[Failure["QQPVLedgerIdentityStopped",<|"Master"->First[u],"Seconds"->co|>],stop]];
  AssociateTo[shifts,First[u]->shift],{i,Length[identities]}];
 Do[qqProgressOperation["PV ledger color-spin group "<>ToString[i]<>" / "<>ToString[Length[groups]]];
  qqProgressUpdate[<|"Index"->i,"Total"->Length[groups]|>];
  rowFile=FileNameJoin[{dir,"group-"<>ToString[i]<>".wxf"}];
  savedRow=If[FileExistsQ[rowFile],Quiet[Check[Import[rowFile,"WXF"],$Failed]],$Failed];
  If[AssociationQ[savedRow]&&Lookup[savedRow,"Key",None]===key&&Lookup[savedRow,"Group",None]===i&&AssociationQ[Lookup[savedRow,"Value",None]]&&!qqFFInvalidQ[Lookup[savedRow,"Value",$Failed]]&&And@@(KeyExistsQ[savedRow["Value"],#]&/@{"ColorID","SpinStructure","UVShift","IRShift","NonzeroShift"}),AppendTo[rows,savedRow["Value"]];Continue[]];
  (* Singular scalar coefficients need finite master terms as well. *)
  singular=TimeConstrained[AnyTrue[Join[groups[[i]]["Before"],groups[[i]]["After"]],
    AnyTrue[Cases[#[[2]],Power[b_,n_Integer?Negative]/;!FreeQ[b,D]:>b,Infinity],TrueQ[(#/.D->4)===0]&]&],co,$TimedOut];
  If[singular===$TimedOut,Throw[Failure["QQPVLedgerCoefficientStopped",<|"Index"->i,"Seconds"->co|>],stop]];
  If[TrueQ[singular],Throw[Failure["QQPVLedgerSingularCoefficient",<|"Index"->i|>],stop]];
  before=Select[groups[[i]]["Before"],Lookup[shifts,#[[1]],Missing["Identity"]]=!=0&];
  If[AnyTrue[before,!KeyExistsQ[shifts,#[[1]]]&],Throw[Failure["QQPVLedgerMissingIdentity",<|"Index"->i|>],stop]];
  shift=TimeConstrained[qqUVIRCompactPole[qqUVIRPole[Total[(#[[2]] shifts[#[[1]]])&/@before]/.D->4-2 FeynCalc`EpsilonUV,FeynCalc`EpsilonUV]],co,$TimedOut];
  If[qqFFInvalidQ[shift],Throw[Failure["QQPVLedgerCoefficientStopped",<|"Index"->i,"Seconds"->co|>],stop]];
  AppendTo[rows,Join[KeyTake[groups[[i]],{"ColorID","SpinStructure"}],<|"UVShift"->shift,
   "IRShift"->-(shift/.FeynCalc`EpsilonUV->FeynCalc`EpsilonIR),"NonzeroShift"->(shift=!=0)|>]];
  savedRow=atomicWXF[rowFile,<|"Key"->key,"Group"->i,"Value"->Last[rows]|>];If[FailureQ[savedRow],Throw[savedRow,stop]],{i,Length[groups]}];True,stop],seconds];
 If[FailureQ[r],Return[r]];If[r=!=True,Return[Failure["QQPVLedgerIncomplete",<||>]]];
 file=FileNameJoin[{cacheDirectory[],"qq-pv-uvir-ledger-"<>IntegerString[key,16,64]<>".wxf"}];
 out=<|"Status"->"PVIdentityPoleLedger","Key"->key,"FormFactorsFile"->p["File"],"SourceFingerprint"->p["SourceFingerprint"],
 "Rows"->rows,"IdentityShifts"->shifts,"Scope"->"Saved generic-D PV identities only; UVShift and IRShift add to the reduced representation",
 "Assumption"->"Saved PV identities are exact in dimensional regularization; singular D=4 coefficients are rejected",
 "ScalelessLedgerComplete"->False,"UVIRSeparated"->False,"AppliedToAmplitude"->False,"File"->file|>;
 r=atomicWXF[file,out];If[FailureQ[r],Return[r]];storeResult["QQPVUVIRLedger",out];
 <|"Status"->out["Status"],"Name"->"QQPVUVIRLedger","Rows"->Length[rows],"Statistics"-><|"NonzeroShifts"->Count[Lookup[rows,"NonzeroShift"],True],"ScalelessLedgerComplete"->False|>,"File"->file|>];

Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringMSbar.wl"}]];
