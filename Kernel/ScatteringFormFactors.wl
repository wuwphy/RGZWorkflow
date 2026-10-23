(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Algebraic postprocessing only; excluded from physical QQ fingerprints. *)
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringStructuralExpand.wl"}]];
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVRelations.wl"}]];
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVVectors.wl"}]];
qqFFInvalidQ[x_] := !FreeQ[x,_qqFFScalarSlot|_HoldForm|_TerminatedEvaluation|_Failure|_Missing|$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity];
qqFFColorHeads=FeynCalc`SUNTF|FeynCalc`SUNT|FeynCalc`SUNF|FeynCalc`SUND|FeynCalc`SUNDelta|FeynCalc`SUNFDelta|FeynCalc`SUNTrace;
qqFFSpinHeads=FeynCalc`Spinor|FeynCalc`DiracGamma|FeynCalc`DiracSigma|FeynCalc`DiracChain|FeynCalc`LorentzIndex|FeynCalc`DiracIndex;
qqFFSplit[term_] := Module[{f,col,spin,scalar},
 f=If[System`Head[term]===Times,List@@term,{term}];
 col=Select[f,!FreeQ[#,qqFFColorHeads]&];
 spin=Select[f,FreeQ[#,qqFFColorHeads]&&!FreeQ[#,qqFFSpinHeads]&];
 scalar=Complement[f,Join[col,spin]];
 If[!FreeQ[Times@@col,qqFFSpinHeads],Return[Failure["QQFFEntangledColorSpin",<||>]]];
 {Times@@col,Times@@spin,Times@@scalar}];
qqFFCanon[x_] := FeynCalc`FCCanonicalizeDummyIndices[
 x/.HoldPattern[FeynCalc`LorentzIndex[label_,dim___]]/;
 MemberQ[{RGZWorkflow`FormFactors`l1,RGZWorkflow`FormFactors`l2,RGZWorkflow`FormFactors`l3,
 RGZWorkflow`FormFactors`l4,RGZWorkflow`FormFactors`l5,RGZWorkflow`FormFactors`l6},label]:>
 FeynCalc`LorentzIndex[qqFFInputIndex[SymbolName[label]],dim],
 System`Head->{FeynCalc`LorentzIndex},FeynCalc`DotSimplify->False,
 FeynCalc`LorentzIndexNames->{RGZWorkflow`FormFactors`l1,RGZWorkflow`FormFactors`l2,
 RGZWorkflow`FormFactors`l3,RGZWorkflow`FormFactors`l4,RGZWorkflow`FormFactors`l5,RGZWorkflow`FormFactors`l6}];
(* Only slashes inside a chain are replaced; external spinor labels stay on shell. *)
qqFFChainConservation[d_Dot] := Module[{v=d,k1=RGZWorkflow`Engine`qqK1,k2=RGZWorkflow`Engine`qqK2,k3=RGZWorkflow`Engine`qqK3,k4=RGZWorkflow`Engine`qqK4},
 If[First[d]===FeynCalc`FCI[FeynCalc`SpinorUBarD[k3,0]]&&Last[d]===FeynCalc`FCI[FeynCalc`SpinorUD[k1,0]],
 v=v/.FeynCalc`DiracGamma[FeynCalc`Momentum[k4,D],D]->FeynCalc`FCI[FeynCalc`GSD[k1]+FeynCalc`GSD[k2]-FeynCalc`GSD[k3]]];
 If[First[d]===FeynCalc`FCI[FeynCalc`SpinorUBarD[k4,0]]&&Last[d]===FeynCalc`FCI[FeynCalc`SpinorUD[k2,0]],
 v=v/.FeynCalc`DiracGamma[FeynCalc`Momentum[k3,D],D]->FeynCalc`FCI[FeynCalc`GSD[k1]+FeynCalc`GSD[k2]-FeynCalc`GSD[k4]]];v];
qqFFSpinStep[x_] := Module[{z=x},
 z=z/.d_Dot:>qqFFChainConservation[d];
 z=FeynCalc`DiracSimplify[z,FeynCalc`DiracEquation->True,FeynCalc`SirlinSimplify->False,FeynCalc`DiracOrder->True];
 qqFFCanon[Expand[qqFFScalarKinematics[z]]]];
qqFFSpinReduce[x_] := FixedPoint[qqFFSpinStep,x,8];
(* Cache individual integral identities, never apply a global tensor reduction. *)
qqFFPVCache=<||>;
qqFFPVNormalize[m_,reduce_,seconds_] := Module[{key={m,reduce},r,known,file,p},
 If[KeyExistsQ[qqFFPVCache,key],Return[qqFFPVCache[key]]];
 file=FileNameJoin[{cacheDirectory[],"qq-pv-identities",IntegerString[Hash[
  {"QQPVIdentity/3",key,$Version,FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVRelations.wl"}],"SHA256"],
   FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVGram.wl"}],"SHA256"],
   DownValues[FeynCalc`PaVeReduce],DownValues[FeynCalc`PaVeOrder],
   Options[FeynCalc`PaVeReduce],Options[FeynCalc`PaVeOrder]},"SHA256"],16,64]<>".wxf"}];
 If[FileExistsQ[file],qqProgressOperation["Read completed PV identity checkpoint"];p=Import[file,"WXF"];
  If[AssociationQ[p]&&Lookup[p,"Input",None]===m&&Lookup[p,"Reduce",None]===reduce&&
    !qqFFInvalidQ[Lookup[p,"Value",$Failed]],r=p["Value"];AssociateTo[qqFFPVCache,key->r];
   qqProgressUpdate[<|"CacheState"->"Reused","ReusedCheckpoints"->(Lookup[$qqProgress,"ReusedCheckpoints",0]+1)|>];Return[r]]];
 r=TimeConstrained[
   known=qqPVMasslessSingleScale[m];If[known===$Failed,known=qqPVZeroInvariant[m]];
   If[known===$Failed,known=qqPVGramReduce[m,TrueQ[reduce]]];
   If[known===$Failed,known=m];
   known=qqPVOrderExpression[known,reduce];
   If[MatchQ[m,_FeynCalc`PaVe]&&Length[qqPVParse[m][[3]]]===6,
    qqPVCompactCoefficients[known],known],seconds,$TimedOut];
 (* A timeout is not an identity. Never memoize it as a completed reduction:
    a later run with a larger per-integral limit must be able to retry. *)
 If[qqFFInvalidQ[r],Return[m]];
 If[!DirectoryQ[DirectoryName[file]],CreateDirectory[DirectoryName[file],CreateIntermediateDirectories->True]];
 qqProgressOperation["Save completed PV identity checkpoint"];
 p=atomicWXF[file,<|"Input"->m,"Reduce"->reduce,"Value"->r|>];
 If[!FailureQ[p],qqProgressUpdate[<|"CacheState"->"Saved","ComputedCheckpoints"->(Lookup[$qqProgress,"ComputedCheckpoints",0]+1)|>]];
 AssociateTo[qqFFPVCache,key->r];r];
qqFFPrepareMasters[x_,reduce_,seconds_] := Module[{ints,rules},
 ints=DeleteDuplicates[Cases[x,m:(_FeynCalc`PaVe|_FeynCalc`A0|_FeynCalc`A00|_FeynCalc`B0|_FeynCalc`B1|_FeynCalc`B00|_FeynCalc`B11|_FeynCalc`C0|_FeynCalc`D0|_FeynCalc`E0):>m,{0,Infinity}]];
 rules=Table[qqProgressOperation["Normalize PV integral "<>ToString[i]<>" / "<>ToString[Length[ints]]];
 ints[[i]]->qqFFPVNormalize[ints[[i]],reduce,seconds],{i,Length[ints]}];x/.Dispatch[rules]];
qqFFColors[] := FeynCalc`FCI[{
 FeynCalc`SUNFDelta[RGZWorkflow`Engine`qqC3,RGZWorkflow`Engine`qqC1] FeynCalc`SUNFDelta[RGZWorkflow`Engine`qqC4,RGZWorkflow`Engine`qqC2],
 FeynCalc`SUNFDelta[RGZWorkflow`Engine`qqC3,RGZWorkflow`Engine`qqC2] FeynCalc`SUNFDelta[RGZWorkflow`Engine`qqC4,RGZWorkflow`Engine`qqC1]}];
qqFFProjectColor[col_,basis_] := Module[{p,n=FeynCalc`SUNN,c,res},
 p=FeynCalc`SUNSimplify[col #,FeynCalc`SUNNToCACF->False]& /@ basis;
 If[!FreeQ[p,qqFFColorHeads|_FeynCalc`SUNIndex|_FeynCalc`SUNFIndex],Return[Failure["QQFFUnsupportedColor",<|"Color"->col|>]]];
 c=Cancel /@ {(n p[[1]]-p[[2]])/(n (n^2-1)),(n p[[2]]-p[[1]])/(n (n^2-1))};
 res=FeynCalc`SUNSimplify[Expand[col-c.basis],FeynCalc`SUNNToCACF->False];
 (* SUNSimplify may retain free-index generators. Their residual Frobenius
    norm is positive for SU(N), N>=2; its vanishing certifies reconstruction. *)
 If[res=!=0,res=Cancel[Together[FeynCalc`SUNSimplify[
  Expand[res FeynCalc`ComplexConjugate[res]],FeynCalc`SUNNToCACF->False]]]];
 If[res=!=0,Return[Failure["QQFFColorReconstruction",<|"Color"->col|>]]];c];
qqFFMasterHeads=FeynCalc`PaVe|FeynCalc`A0|FeynCalc`A00|FeynCalc`B0|FeynCalc`B1|FeynCalc`B00|FeynCalc`B11|FeynCalc`C0|FeynCalc`D0|FeynCalc`E0;
qqFFScalarKinematics[x_] := Module[{k={RGZWorkflow`Engine`qqK1,RGZWorkflow`Engine`qqK2,RGZWorkflow`Engine`qqK3,RGZWorkflow`Engine`qqK4},s=RGZWorkflow`Engine`qqS,t=RGZWorkflow`Engine`qqT,pair,rules},
 pair[a_,b_]:=FeynCalc`FCI[FeynCalc`SPD[a,b]];
 rules=Join[Table[pair[k[[i]],k[[i]]]->0,{i,4}],{
 pair[k[[1]],k[[2]]]->s/2,pair[k[[3]],k[[4]]]->s/2,
 pair[k[[1]],k[[3]]]->-t/2,pair[k[[2]],k[[4]]]->-t/2,
 pair[k[[1]],k[[4]]]->(s+t)/2,pair[k[[2]],k[[3]]]->(s+t)/2}];
 FeynCalc`ExpandScalarProduct[x]/.rules];
(* Combine equal denominators first; only one color-spin-PV coefficient is
   passed to Together, never the amplitude. No positivity assumptions. *)
qqFFRationalReduce[x_] := Module[{terms,groups,z},
 terms=If[Head[x]===Plus,List@@x,{x}];
 groups=GatherBy[terms,Denominator];
 z=Total[(Total[Numerator /@ #]/Denominator[First[#]])& /@ groups];
 Cancel[Together[z]]];
qqFFCollectMasters[expr_,simplify_,seconds_] := Module[{terms,parts={},f,dependent,master,co,candidate,groups,rows={},index=0,stop=Unique["qqFFMasterStop"]},Catch[
 qqProgressOperation["Split expression by master integrals (scalar blocks protected)"];
 terms=qqFFMasterTerms[expr];
 Do[f=If[System`Head[term]===Times,List@@term,{term}];
  dependent=Select[f,!FreeQ[#,qqFFMasterHeads]&];
  If[Length[dependent]>1||
   (Length[dependent]===1&&(!MatchQ[System`Head[First[dependent]],qqFFMasterHeads]||!FreeQ[List@@First[dependent],qqFFMasterHeads])),
   Throw[Failure["QQFFNonlinearMasterExpression",<||>],stop]];
  master=If[dependent==={},1,First[dependent]];
  co=Times@@Select[f,FreeQ[#,qqFFMasterHeads]&];AppendTo[parts,{master,co}],{term,terms}];
 qqProgressOperation["Group equal master integrals"];groups=GatherBy[parts,First];
 Do[index++;co=Total[group[[All,2]]];
  If[TrueQ[simplify],qqProgressOperation["Simplify scalar coefficient "<>ToString[index]<>" / "<>ToString[Length[groups]]];
   candidate=TimeConstrained[qqFFRationalReduce[co],seconds,$TimedOut];
   If[!qqFFInvalidQ[candidate]&&LeafCount[candidate]<=LeafCount[co],co=candidate]];
  If[co=!=0,AppendTo[rows,{group[[1,1]],co}]],{group,groups}];rows,stop]];
qqFFReadPacket[file_] := Module[{p=RGZResult["QQAmplitude"],job=RGZResult["QQ"],path=file},
 (* An explicit file is an independent import. Automatic input belongs to the
    active task whenever one exists; never silently reuse another amplitude. *)
 If[path===Automatic,
  If[AssociationQ[p],
   If[AssociationQ[job]&&(!KeyExistsQ[job,"Fingerprint"]||
      Lookup[p,"Fingerprint",None]=!=job["Fingerprint"]),
    Return[Failure["QQAmplitudeTaskMismatch",<|"Action"->"RGZQQAmplitude[]",
     "Explanation"->"The stored amplitude does not belong to the active QQ task. Reassemble before forming factors."|>]]];
   Return[p]];
  If[!AssociationQ[job],Return[Failure["QQAmplitudeFirst",<|"Action"->"RGZQQAmplitude[] or supply File -> bare-amplitude.wxf"|>]]];
  path=FileNameJoin[{job["Directory"],"bare-amplitude.wxf"}]];
 If[!StringQ[path]||!FileExistsQ[path],Return[Failure["QQAmplitudeFile",<|"File"->path|>]]];
 p=Quiet[Check[Import[path,"WXF"],$Failed]];
 If[!AssociationQ[p]||!KeyExistsQ[p,"Amplitude"],Return[Failure["QQAmplitudeFile",<|"File"->path|>]]];
 If[file===Automatic&&(!KeyExistsQ[job,"Fingerprint"]||Lookup[p,"Fingerprint",None]=!=job["Fingerprint"]),
  Return[Failure["QQAmplitudeTaskMismatch",<|"Action"->"RGZQQAmplitude[]","File"->path|>]]];p];
(* Loop order selects assembled iM components, not powers of g or master=1. *)
qqFFSelectOrder[packet_Association,order_] := Module[{field,input},
 If[!MemberQ[{All,0,1},order],Return[Failure["QQFFLoopOrder",<|"Explanation"->"LoopOrder must be All, 0 or 1."|>]]];
 field=Switch[order,0,"Tree",1,"OneLoop",All,"Amplitude"];
 If[!KeyExistsQ[packet,field],Return[Failure["QQFFComponentMissing",<|"Parts"->{field},"Action"->"Reassemble with RGZQQAmplitude[]."|>]]];
 input=packet[field];If[order=!=All,input=input/I];
 If[qqFFInvalidQ[input],Return[Failure["QQAmplitudeIncomplete",<|"Parts"->{field}|>]]];
 Join[packet,<|"Amplitude"->input,"LoopOrder"->order,"SelectedComponent"->field,
 "AmplitudeConvention"->"M; assembled Tree and OneLoop divided by I exactly once"|>]];
qqFFBuild[packet_,simplify_,coSeconds_,reducePV_:False] := Module[{input=packet["Amplitude"],expanded,terms,parts,colors,spins,
 basis=qqFFColors[],colorMap,spinMap,col,spin,reduced,check,rows={},records,groups,structures,masterPairs,preMasterPairs,pvIdentities,pvReduction,
 forms={},pvProvenance={},used,key,file,cache,saved,inputHash,dir,answer,groupFile,groupPacket,unreduced=0,stop=Unique["qqFFStop"]},Catch[
 If[qqFFInvalidQ[input]||qqFFUnresolvedScalarQ[input],Throw[Failure["QQAmplitudeIncomplete",<|"Action"->"Recompute failed components and reassemble; TerminatedEvaluation and other failure sentinels cannot be form factors."|>],stop]];
 If[!FreeQ[input,FeynCalc`TID|FeynCalc`FeynAmpDenominator|RGZWorkflow`Engine`q],Throw[Failure["QQAmplitudeNotReduced",<||>],stop]];
 inputHash=Hash[input,"SHA256"];
 key=Hash[{"QQFormFactors/2",inputHash,Lookup[packet,"LoopOrder","Unspecified"],Lookup[packet,"Fingerprint",None],Lookup[packet,"AssemblyConvention",None],$Version,FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringFormFactors.wl"}],"SHA256"],FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringStructuralExpand.wl"}],"SHA256"],FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVRelations.wl"}],"SHA256"],FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVGram.wl"}],"SHA256"],FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVVectors.wl"}],"SHA256"],simplify,coSeconds,reducePV},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"RGZ_qq_form_factors_"<>IntegerString[key,16,64]}];
 If[!DirectoryQ[dir],CreateDirectory[dir]];file=FileNameJoin[{dir,"form-factors.wxf"}];
 If[FileExistsQ[file],cache=Import[file,"WXF"];
 If[AssociationQ[cache]&&Lookup[cache,"Key",None]===key&&TrueQ[cache["ReconstructionVerified"]]&&!qqFFInvalidQ[cache],storeResult["QQFormFactors",cache];Throw[<|"Status"->"Complete","Name"->"QQFormFactors","LoopOrder"->Lookup[packet,"LoopOrder","Unspecified"],"Rows"->Length[cache["Rows"]],"File"->file,"Statistics"-><|"Reused"->True,"ReconstructionVerified"->True|>|>,stop]]];
 qqProgressOperation["Convert amplitude to FeynCalc internal form"];
 parts=If[TrueQ[input===0],{},qqFFStructuralParts[FeynCalc`FCI[input]]];If[FailureQ[parts],Throw[parts,stop]];
 colors=DeleteDuplicates[parts[[All,1]]];spins=DeleteDuplicates[parts[[All,2]]];
 qqProgressUpdate[<|"Stage"->"QQFormFactors:Color","Index"->2,"Total"->4|>];
 colorMap=Table[qqProgressOperation["Project color tensor "<>ToString[i]<>" / "<>ToString[Length[colors]]];
 col=qqFFProjectColor[colors[[i]],basis];If[FailureQ[col],Throw[col,stop]];colors[[i]]->col,{i,Length[colors]}];
 qqProgressUpdate[<|"Stage"->"QQFormFactors:Spin","Index"->3,"Total"->4|>];
 spinMap=Table[qqProgressOperation["Reduce D-dimensional spin structure "<>ToString[i]<>" / "<>ToString[Length[spins]]];
 spin=spins[[i]];reduced=qqFFSpinReduce[spin];
 check=Expand[qqFFSpinReduce[qqFFCanon[spin]-reduced]];
 If[check=!=0,check=TimeConstrained[Expand[FeynCalc`DiracSimplify[check,
  FeynCalc`SirlinSimplify->False,FeynCalc`DiracOrder->True]],2,$TimedOut]];
 (* A simplification that cannot be certified is not applied. Dummy-index
    renaming alone is always retained, so no D-dimensional remainder is lost. *)
 If[check=!=0||qqFFInvalidQ[reduced],reduced=qqFFCanon[spin];unreduced++];
 spin->reduced,{i,Length[spins]}];
 colorMap=Dispatch[colorMap];spinMap=Dispatch[spinMap];
 qqProgressOperation["Collect color-spin structures"];
 Do[col=part[[1]]/.colorMap;spin=part[[2]]/.spinMap;
 reduced=qqLinearizeChains[spin];
 Do[records=qqFFSplit[term];If[FailureQ[records],Throw[records,stop]];
 If[records[[1]]=!=1,Throw[Failure["QQFFUnexpectedSpinColor",<||>],stop]];
 If[records[[2]]===1&&records[[3]]===0,Continue[]];
 Do[If[col[[a]]=!=0,AppendTo[rows,{{a,records[[2]]},part[[3]] col[[a]] records[[3]]}]],{a,2}],
 {term,If[System`Head[reduced]===Plus,List@@reduced,{reduced}]}],{part,parts}];
 groups=GatherBy[rows,First];structures=DeleteDuplicates[If[groups==={}, {},groups[[All,1,1,2]]]];
 qqProgressUpdate[<|"Stage"->"QQFormFactors:Masters","Index"->4,"Total"->4|>];
 Do[qqProgressOperation["Collect master coefficients "<>ToString[i]<>" / "<>ToString[Length[groups]]];
 qqProgressUpdate[<|"AlgebraGroupIndex"->i,"AlgebraGroupTotal"->Length[groups]|>];
 groupFile=FileNameJoin[{dir,"master-group-"<>ToString[i]<>".wxf"}];
 qqProgressOperation["Read color-spin master-group checkpoint "<>ToString[i]];
 groupPacket=If[FileExistsQ[groupFile],Quiet[Check[Import[groupFile,"WXF"],$Failed]],$Failed];
 If[AssociationQ[groupPacket]&&Lookup[groupPacket,"Key",None]===key&&
   Lookup[groupPacket,"Group",None]===groups[[i,1,1]]&&
   qqFFVectorPairsQ[Lookup[groupPacket,"Pairs",None]]&&
   qqFFVectorPairsQ[Lookup[groupPacket,"PreReductionPairs",None]]&&ListQ[Lookup[groupPacket,"Identities",None]],
  qqProgressOperation["Reuse completed color-spin master group "<>ToString[i]];masterPairs=groupPacket["Pairs"];preMasterPairs=groupPacket["PreReductionPairs"];pvIdentities=groupPacket["Identities"],
  masterPairs=qqFFCollectMasters[qqFFScalarKinematics[Total[groups[[i,All,2]]]],False,coSeconds];
  If[FailureQ[masterPairs],Throw[masterPairs,stop]];
  preMasterPairs=masterPairs;
  pvReduction=qqFFReduceMasterPairs[masterPairs,reducePV,simplify,coSeconds,True];If[FailureQ[pvReduction],Throw[pvReduction,stop]];
  masterPairs=pvReduction["Pairs"];pvIdentities=pvReduction["Identities"];
  qqProgressOperation["Save completed color-spin master group "<>ToString[i]];
  saved=atomicWXF[groupFile,<|"Key"->key,"Group"->groups[[i,1,1]],"Pairs"->masterPairs,"PreReductionPairs"->preMasterPairs,"Identities"->pvIdentities|>];If[FailureQ[saved],Throw[saved,stop]]];
 AppendTo[pvProvenance,<|"ColorID"->groups[[i,1,1,1]],"SpinStructure"->groups[[i,1,1,2]],"Before"->preMasterPairs,"After"->masterPairs,"Identities"->pvIdentities|>];
 If[masterPairs=!={},AppendTo[forms,<|"ColorID"->groups[[i,1,1,1]],"SpinID"->First[FirstPosition[structures,groups[[i,1,1,2]],Missing[],{1}]],
 "PreReductionMasterCoefficients"->preMasterPairs,
 "MasterCoefficients"->masterPairs,"FormFactor"->Total[Times@@@masterPairs]|>]],{i,Length[groups]}];
 (* Scalar cancellation can empty a previously nonzero spin sector. *)
 used=If[forms==={}, {},Sort[DeleteDuplicates[Lookup[forms,"SpinID"]]]];
 forms=(Join[#,<|"SpinID"->First[FirstPosition[used,#["SpinID"]]]|>]& /@ forms);
 structures=structures[[used]];
 answer=<|"Schema"->"RGZQQFormFactors/1","Key"->key,"Status"->"Complete","SourceFingerprint"->Lookup[packet,"Fingerprint",None],
 "LoopOrder"->Lookup[packet,"LoopOrder","Unspecified"],"SelectedComponent"->Lookup[packet,"SelectedComponent","Amplitude"],
 "AssemblyConvention"->Lookup[packet,"AssemblyConvention",None],"UVIRSeparated"->False,
 "PVReductionProvenance"->"PreReductionMasterCoefficients retained per row; earlier tensor reduction and mass limits are outside this boundary",
 "InputSHA256"->inputHash,"Gauge"->Lookup[packet,"Gauge",None],"RegulatorMass"->Lookup[packet,"RegulatorMass",None],
 "ColorBasis"->basis,"SpinStructures"->structures,"Rows"->forms,"PVReductionGroups"->pvProvenance,
 "ReconstructionVerified"->True,"Verification"-><|"TermSplit"->True,"ColorProjection"->True,"DdimensionalSpinIdentities"->True,"LinearCollection"->True,"MasslessMandelstamKinematics"->True,"PVArgumentOrderingAttempted"->True|>,
 "SpinBasisIndependent"->False,"SpinConvention"->"D-dimensional spanning structures; no four-dimensional Fierz reduction or evanescent structures discarded",
 "UnreducedSpinStructures"->unreduced,"TensorPVReductionRequested"->reducePV,"PVEntryCount"->Total[Length[#["MasterCoefficients"]]& /@ forms],
 "MasterBasisIndependent"->False,"PVReductionScope"->"Exact single-scale/bubble identities, parameter permutations, Gram-null IBP and twin-parameter integration; optional nonsingular lower-point momentum/metric recurrences and native reduction; unsupported entries retained",
 "Renormalized"->False,"GaugeIndependenceVerified"->False,"File"->file|>;
 saved=atomicWXF[file,answer];If[FailureQ[saved],Throw[saved,stop]];storeResult["QQFormFactors",answer];
 <|"Status"->"Complete","Name"->"QQFormFactors","LoopOrder"->Lookup[packet,"LoopOrder","Unspecified"],"Rows"->Length[forms],"File"->file,
 "Statistics"-><|"Colors"->2,"SpinStructures"->Length[structures],"ReconstructionVerified"->True|>|>,stop]];
Options[RGZQQFormFactors]={"LoopOrder"->All,"File"->Automatic,"SimplifyCoefficients"->True,"CoefficientSeconds"->5,"ReduceTensorIntegrals"->False};
RGZQQFormFactors[seconds_:600,OptionsPattern[]] := Module[{r,p,now=AbsoluteTime[],co=OptionValue["CoefficientSeconds"],simplify=OptionValue["SimplifyCoefficients"],reduce=OptionValue["ReduceTensorIntegrals"],order=OptionValue["LoopOrder"]},
 If[!MemberQ[{All,0,1},order],Return[Failure["QQFFLoopOrder",<|"Explanation"->"LoopOrder must be All, 0 or 1."|>]]];
 If[!NumericQ[seconds]||!TrueQ[seconds>0]||!NumericQ[co]||!TrueQ[co>0]||!MemberQ[{True,False},simplify]||!MemberQ[{True,False},reduce],Return[Failure["QQFFOptions",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 $results=KeyDrop[$results,{"QQFormFactors"}];
 $qqProgress=<|"Stage"->"QQFormFactors","Status"->"Computing","RunStarted"->now,"ItemStarted"->now,"StepLimitSeconds"->seconds|>;
 r=If[TrueQ[$Notebooks],Monitor[Block[{$IterationLimit=Infinity,qqFFPVCache=<||>},timed["QQFormFactors",p=qqFFReadPacket[OptionValue["File"]];If[!FailureQ[p],p=qqFFSelectOrder[p,order]];If[FailureQ[p],p,qqFFBuild[p,simplify,co,reduce]],seconds]],Dynamic[qqSelfProgressPanel[],UpdateInterval->1,TrackedSymbols:>{$qqProgress}]],Block[{$IterationLimit=Infinity,qqFFPVCache=<||>},timed["QQFormFactors",p=qqFFReadPacket[OptionValue["File"]];If[!FailureQ[p],p=qqFFSelectOrder[p,order]];If[FailureQ[p],p,qqFFBuild[p,simplify,co,reduce]],seconds]]];
 qqProgressUpdate[<|"Status"->If[FailureQ[r],"Stopped","Complete"],"StoppedAt"->AbsoluteTime[]|>];r];
