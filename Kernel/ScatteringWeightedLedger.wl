(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Graph-weighted finite-regulator ledger. Generated WL caches must be read
   in Engine, never in the notebook's Global context. Source caches are read only. *)
$weightedLedgerSHA256=FileHash[$InputFileName,"SHA256"];
SetAttributes[qqWeightedCheckpoint,HoldRest];
qqWeightedCheckpoint[file_,key_,body_,seconds_] := Module[{old,z},
 old=If[FileExistsQ[file],Import[file,"WXF"],None];
 If[AssociationQ[old]&&Lookup[old,"Key",None]===key&&qqAuditValidQ[Lookup[old,"Value",Missing[]]],
 qqProgressUpdate[<|"CheckpointFile"->file,"CacheState"->"Reused","ReusedCheckpoints"->(Lookup[$qqProgress,"ReusedCheckpoints",0]+1)|>];Return[old["Value"]]];
 qqProgressUpdate[<|"CacheState"->"Computing"|>];z=qqAuditCheckpoint[file,key,body,seconds];
 If[!FailureQ[z],qqProgressUpdate[<|"CacheState"->"Saved","ComputedCheckpoints"->(Lookup[$qqProgress,"ComputedCheckpoints",0]+1)|>]];z];
qqWeightedRead[file_] := engineEvaluation[Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=True},Get[file]]];
qqWeightedOldPolicy[x_] := Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=False},x];
qqWeightedZeroQ[x_] := If[x===0,True,TrueQ[Expand[x]===0]];
qqWeightedEqualQ[a_,b_] := SameQ[a,b]||qqWeightedZeroQ[a-b];
qqWeightedPolynomialQ[x_] := Module[{q=RGZWorkflow`Engine`q,atoms,vars,z},
 atoms=DeleteDuplicates[Cases[x,t_FeynCalc`Pair/;!FreeQ[t,q],Infinity]];
 vars=Table[Unique["RGZWorkflow`Private`loopPolynomial"],{Length[atoms]}];z=x/.Dispatch[Thread[atoms->vars]];
 FreeQ[z,q]&&PolynomialQ[z,vars]];
(* PaVeUVPart preserves rational terms, while its default product mode
   discards finite (D-4)*PV terms. Work on scalar PV objects individually,
   retaining all D-dependent coefficients and rational reduction terms. *)
qqWeightedUVRepresentation[x_] := Module[{canonical,objects,parts,rest,u,stop=Unique["RGZWorkflow`Private`uvRepresentation"]},Catch[
 canonical=FeynCalc`FCI[FeynCalc`PaVeReduce[x,FeynCalc`Dimension->D,FeynCalc`FCE->False]];
 If[!FreeQ[canonical,FeynCalc`PaVeReduce|FeynCalc`TID|FeynCalc`FeynAmpDenominator],Throw[Failure["QQWeightedUVCanonicalizationIncomplete",<||>],stop]];
 objects=DeleteDuplicates[Cases[canonical,t_/;RGZWorkflow`Engine`RGZMXPVObjectQ[t],{0,Infinity}]];
 If[!AllTrue[objects,RGZWorkflow`Engine`RGZMXPVScalarQ]||!qqWeightedLinearPVQ[canonical,objects],Throw[Failure["QQWeightedUVScalarBasisRequired",<||>],stop]];
 rest=canonical/.Dispatch[Thread[objects->0]];parts={};
 Do[u=FeynCalc`PaVeUVPart[obj,FeynCalc`FCLoopExtract->False];
 If[!FreeQ[u,FeynCalc`PaVeUVPart],Throw[Failure["QQWeightedUVProjectionIncomplete",<||>],stop]];
 AppendTo[parts,Coefficient[canonical,obj] u],{obj,objects}];rest+Total[parts],stop]];
qqWeightedFiniteLaurent[x_] := Module[{atoms,vars,z,n,den,r},
 atoms=DeleteDuplicates[Cases[x,t:(_FeynCalc`Pair|_FeynCalc`CartesianPair|_FeynCalc`Eps|_FeynCalc`DiracGamma)/;!FreeQ[t,D],{0,Infinity}]];
 vars=Table[Unique["RGZWorkflow`Private`dimensionInvariant"],{Length[atoms]}];z=Cancel[Together[x/.Dispatch[Thread[atoms->vars]]]];
 n=Numerator[z];den=Denominator[z];If[!PolynomialQ[n,D]||!PolynomialQ[den,D],Return[Failure["QQWeightedNonRationalDimension",<||>]]];
 r=Normal[Series[z,{D,4,0}]];r/.Dispatch[Thread[vars->atoms]]];
qqWeightedUVChange[before_,after_] := Module[{u,v,full,finite},
 {u,v}=Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=True},{qqWeightedUVRepresentation[before],qqWeightedUVRepresentation[after]}];
 If[FailureQ[u],Return[u]];If[FailureQ[v],Return[v]];full=Cancel[Together[u-v]];
 finite=qqWeightedFiniteLaurent[full];If[FailureQ[finite],Return[finite]];
 <|"UVTransferD"->finite,"IRTransferD"->-finite,"UVTransferDFull"->full|>];
qqWeightedUnitShift[unit_Association] := Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=True},Module[{x=unit["Input"],old=qqWeightedOldPolicy[unit["Result"]],replayed,prepared,change,kind,q=RGZWorkflow`Engine`q},
 If[!qqAuditValidQ[{x,old}],Return[Failure["QQWeightedInvalidUnit",<||>]]];
 If[FreeQ[x,FeynCalc`FeynAmpDenominator],
 If[!qqWeightedPolynomialQ[x],Return[Failure["QQWeightedNonPolynomialUnit",<||>]]];
 If[!qqWeightedZeroQ[old],Return[Failure["QQWeightedPolynomialResultMismatch",<||>]]];
 Return[<|"Kind"->"Polynomial","UVTransferD"->0,"IRTransferD"->0,"UVTransferDFull"->0|>]];
 kind=If[qqMSbarZeroScaleQ[x],"Scaleless","ScalePresentReplayed"];
 prepared=FeynCalc`ApartFF[x,{q}];If[!FreeQ[prepared,FeynCalc`ApartFF],Return[Failure["QQWeightedRouteCancellationIncomplete",<||>]]];
 replayed=Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=True},FeynCalc`TID[prepared,q,FeynCalc`ToPaVe->True,FeynCalc`UsePaVeBasis->True,FeynCalc`ApartFF->False]];
 change=qqWeightedUVChange[replayed,old];If[FailureQ[change],Return[change]];Join[<|"Kind"->kind|>,change]]];
qqWeightedPVShift[before_,after_] := qqWeightedUVChange[before,qqWeightedOldPolicy[after]];
qqWeightedInventory[dir_,prefix_] := Module[{files,rows,first},
 files=Sort[FileNames[prefix<>"*.wl",dir]];rows={#,FileHash[#,"SHA256"]}&/@files;
 first=If[files==={},None,qqWeightedRead[First[files]]];
 <|"Digest"->Hash[rows,"SHA256"],"Files"->Association[Rule@@@rows],"Context"->If[AssociationQ[first],Lookup[first,"Environment",None],None]|>];
qqWeightedRecordQ[rec_,input_,context_] := AssociationQ[rec]&&Lookup[rec,"Status",None]==="Complete"&&
 Lookup[rec,"Version",None]===1&&SameQ[Lookup[rec,"Input",Missing[]],input]&&SameQ[Lookup[rec,"Environment",Missing[]],context]&&KeyExistsQ[rec,"Result"];
qqWeightedComponent[row_,component_,sourceDir_,tid_,pv_,auditDir_,key_,seconds_] := Module[
 {units,context=tid["Context"],pvContext=pv["Context"],unitFile,unit,uv={},integrated={},unitRows={},objects,needed,maps={},pvRows={},weight,z,shift,file,r,shared,semantic,split,old,scalar,stop=Unique["RGZWorkflow`Private`weightedComponent"]},Catch[
 shared=FileNameJoin[{DirectoryName[auditDir],"shared-units"}];If[!DirectoryQ[shared],CreateDirectory[shared]];
 split=row["Decompose"<>component];old=qqWeightedOldPolicy[row["Integrate"<>component]];scalar=qqWeightedOldPolicy[row["Scalar"<>component]];
 qqProgressOperation["Recover loop-independent graph weights from saved split integrand"];
 units=RGZWorkflow`Engine`RGZMXIntegralUnits[split];
 If[!ListQ[units]||!AllTrue[units,MatchQ[#,{_,_}]&&FreeQ[First[#],RGZWorkflow`Engine`q]&],Throw[Failure["QQWeightedLoopDependentWeight",<||>],stop]];
 Do[weight=units[[n,1]];
 unitFile=FileNameJoin[{sourceDir,"tid_units_v1","unit_"<>IntegerString[Hash[{"AA-TID-unit-v1",units[[n,2]],context},"SHA256"],16]<>".wl"}];
 If[!KeyExistsQ[tid["Files"],unitFile]||!FileExistsQ[unitFile]||FileHash[unitFile,"SHA256"]=!=tid["Files"][unitFile],Throw[Failure["QQWeightedTIDUnitMissingOrChanged",<|"File"->unitFile|>],stop]];
 unit=qqWeightedRead[unitFile];If[!qqWeightedRecordQ[unit,units[[n,2]],context],Throw[Failure["QQWeightedTIDUnitMismatch",<|"File"->unitFile|>],stop]];
 qqProgressUpdate[<|"AlgebraGroupIndex"->n,"AlgebraGroupTotal"->Length[units]|>];qqProgressOperation["Audit weighted pre-TID "<>component<>" unit "<>ToString[n]<>" / "<>ToString[Length[units]]];
 semantic=Hash[KeyTake[unit,{"Input","Result","Environment"}],"SHA256"];
 file=FileNameJoin[{shared,"tid-"<>IntegerString[semantic,16,64]<>".wxf"}];
 shift=qqWeightedCheckpoint[file,Hash[{key,"tid",semantic},"SHA256"],qqWeightedUnitShift[unit],seconds];If[FailureQ[shift],Throw[shift,stop]];
 AppendTo[integrated,weight qqWeightedOldPolicy[unit["Result"]]];z=qqWeightedFiniteLaurent[weight shift["UVTransferDFull"]];If[FailureQ[z],Throw[z,stop]];AppendTo[uv,z];
 AppendTo[unitRows,<|"UnitFile"->unitFile,"SHA256"->tid["Files"][unitFile],"Weight"->weight,"InputSHA256"->Hash[units[[n,2]],"SHA256"],"Kind"->shift["Kind"],"WeightedUVTransferD"->z|>],{n,Length[units]}];
 qqProgressOperation["Verify the weighted TID sum against the saved graph integral"];
 If[!qqWeightedEqualQ[Total[integrated],old],Throw[Failure["QQWeightedGraphTIDReconstruction",<|"Component"->component|>],stop]];
 objects=DeleteDuplicates[Cases[old,t_/;RGZWorkflow`Engine`RGZMXPVObjectQ[t],{0,Infinity}]];
 needed=Select[objects,!RGZWorkflow`Engine`RGZMXPVScalarQ[#]&];
 If[!qqWeightedLinearPVQ[old,objects],Throw[Failure["QQWeightedNonlinearPV",<||>],stop]];
 Do[unitFile=FileNameJoin[{sourceDir,"pv_units_v1","pv_"<>IntegerString[Hash[{"AA-PV-unit-v1",needed[[n]],pvContext},"SHA256"],16]<>".wl"}];
 If[!KeyExistsQ[pv["Files"],unitFile]||!FileExistsQ[unitFile]||FileHash[unitFile,"SHA256"]=!=pv["Files"][unitFile],Throw[Failure["QQWeightedPVUnitMissingOrChanged",<|"File"->unitFile|>],stop]];
 unit=qqWeightedRead[unitFile];If[!qqWeightedRecordQ[unit,needed[[n]],pvContext],Throw[Failure["QQWeightedPVUnitMismatch",<|"File"->unitFile|>],stop]];
 weight=Coefficient[old,needed[[n]]];qqProgressOperation["Audit graph-weighted pre-PV identity "<>ToString[n]<>" / "<>ToString[Length[needed]]];
 semantic=Hash[KeyTake[unit,{"Input","Result","Environment"}],"SHA256"];
 shift=qqWeightedCheckpoint[FileNameJoin[{shared,"pv-"<>IntegerString[semantic,16,64]<>".wxf"}],Hash[{key,"pv",semantic},"SHA256"],qqWeightedPVShift[needed[[n]],unit["Result"]],seconds];
 If[FailureQ[shift],Throw[shift,stop]];AppendTo[maps,needed[[n]]->qqWeightedOldPolicy[unit["Result"]]];z=qqWeightedFiniteLaurent[weight shift["UVTransferDFull"]];If[FailureQ[z],Throw[z,stop]];AppendTo[uv,z];
 AppendTo[pvRows,<|"UnitFile"->unitFile,"SHA256"->pv["Files"][unitFile],"Object"->needed[[n]],"Weight"->weight,"WeightedUVTransferD"->z|>],{n,Length[needed]}];
 qqProgressOperation["Verify graph PV substitution against the saved scalar result"];
 If[!qqWeightedEqualQ[old/.Dispatch[maps],scalar],Throw[Failure["QQWeightedGraphPVReconstruction",<|"Component"->component|>],stop]];
 z=Total[uv];<|"Component"->component,"TIDReconstructionVerified"->True,"PVReconstructionVerified"->True,"TIDUnits"->unitRows,"PVIdentities"->pvRows,"WeightedUVTransferD"->z,"WeightedIRTransferD"->-z,"Scalar"->scalar|>,stop]];
(* Structural degree check uses the actual PV objects, not a list of head
   names; it therefore handles both tensor and scalar PV representations. *)
qqWeightedLinearPVQ[input_,objects_] := Module[{vars,degree,z,stop=Unique["RGZWorkflow`Private`weightedPV"]},Catch[
 vars=Table[Unique["RGZWorkflow`Private`weightedPVAtom"],{Length[objects]}];z=input/.Dispatch[Thread[objects->vars]];
 degree[x_] := If[FreeQ[x,Alternatives@@vars],0,Which[MemberQ[vars,x],1,Head[x]===Plus,Max[degree/@List@@x],Head[x]===Times,Total[degree/@List@@x],
 Head[x]===Power&&IntegerQ[x[[2]]]&&x[[2]]>=0,x[[2]]degree[x[[1]]],True,Throw[False,stop]]];degree[z]<=1,stop]];
qqWeightedMatrixReadOnly[manifestDir_] := Module[{data,projection,file,out,pairFiles,parts,stop=Unique["RGZWorkflow`Private`matrixRead"]},Catch[
 out=Table[file=FileNameJoin[{manifestDir,"adjoint_"<>ToString[i]<>"_"<>ToString[k]<>".wl"}];
 If[!FileExistsQ[file],Throw[Failure["QQWeightedAdjointCacheRequired",<|"File"->file|>],stop]];
 pairFiles=Table[FileNameJoin[{manifestDir,"pair_"<>ToString[i]<>"_"<>ToString[k]<>"_"<>ToString[n]<>".wl"}],{n,4}];
 If[!AllTrue[pairFiles,FileExistsQ],Throw[Failure["QQWeightedPairSourceRequired",<||>],stop]];
 parts=qqWeightedRead /@ pairFiles;
 If[!AllTrue[parts,AssociationQ[#]&&Lookup[#,"Status",None]==="Complete"&&Lookup[#,"FieldPair",None]==={i,k}&],Throw[Failure["QQWeightedPairSourceMismatch",<||>],stop]];
 data=qqWeightedOldPolicy[<|"T"->Total[Lookup[parts,"T"]],"L"->Total[Lookup[parts,"L"]],"DiagramCount"->Total[Lookup[parts,"DiagramCount"]]|>];
 projection=qqWeightedRead[file];
 If[!AssociationQ[data]||!AssociationQ[projection]||Lookup[projection,"Status",None]=!="Complete"||!SameQ[qqWeightedOldPolicy[Lookup[projection,"Input",Missing[]]],data]||!AssociationQ[Lookup[projection,"Result",None]],
 Throw[Failure["QQWeightedAdjointSourceMismatch",<|"File"->file|>],stop]];
 qqWeightedOldPolicy[projection["Result"]["T"]],{i,{1,5,6,7,8}},{k,{1,5,6,7,8}}];out,stop]];
Options[RGZQQWeightedUVIRLedger]={"MatrixManifest"->Automatic,"JobFile"->Automatic,"RecordIDs"->All,"IntegralSeconds"->60,"GraphSeconds"->300};
RGZQQWeightedUVIRLedger[seconds_:3600,OptionsPattern[]] := engineEvaluation@Module[
 {j,r,manifest=OptionValue["MatrixManifest"],m,records,ids=OptionValue["RecordIDs"],sec=OptionValue["IntegralSeconds"],graphSec=OptionValue["GraphSeconds"],key,dir,rows={},recordRows={},entry,row,tid,pv,sourceDir,sourceFile,recordFile,graphFiles,graphSHAs,dependencyKey,sub,z,components,sums,complete=False,match=False,sourceMatrix,out,expectedFingerprint,modelBase,stopped=None,expectedGraphs=0,n,stop=Unique["RGZWorkflow`Private`weightedLedger"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,sec,graphSec}),Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];If[!qqGaugeSymbolicParametersQ[],Return[Failure["SymbolicParametersRequired",<||>]]];
 j=qqAuditRead[OptionValue["JobFile"],"QQ"];If[!AssociationQ[j]||!And@@(KeyExistsQ[j,#]&/@{"Fingerprint","MixedSourceHash","ModelFingerprint","ModelContent"})||j["ModelFingerprint"]=!=modelKey[]||j["ModelContent"]=!=modelContent[],Return[Failure["QQWeightedJobRequired",<||>]]];
 If[!StringQ[manifest]||!FileExistsQ[manifest],Return[Failure["QQWeightedManifestRequired",<||>]]];
 r=loadLoopEngine[];If[FailureQ[r],Return[r]];
 modelBase=RGZWorkflow`Engine`modelRGZ;
 If[!StringQ[modelBase]||!And@@(FileExistsQ[modelBase<>#]&/@{".gen",".mod"}),Return[Failure["QQWeightedModelPathRequired",<||>]]];
 expectedFingerprint=IntegerString[Hash[{Import[modelBase<>".gen","Text"],Import[modelBase<>".mod","Text"],
 {RGZWorkflow`Engine`RGZMXIntegralContext[],Options[FeynCalc`PaVeReduce],Options[FeynCalc`FCFAConvert]},"mixed-matrix-complete-matter-v3",workflowFingerprint[],quarkMasses[]},"SHA256"],16];
 If[!TrueQ[RGZWorkflow`Engine`RGZMXUnfixedKinematicsQ[]],Return[Failure["QQWeightedUnfixedKinematicsRequired",<||>]]];
 m=qqWeightedRead[manifest];If[!AssociationQ[m]||Lookup[m,"Fingerprint",None]=!=expectedFingerprint||!AssociationQ[Lookup[m,"Records",None]]||Length[m["Records"]]=!=256,Return[Failure["QQWeightedManifestSourceMismatch",<||>]]];
 records=Values[m["Records"]];If[!AllTrue[records,Lookup[#,"Status",None]==="Complete"&&IntegerQ[Lookup[#,"DiagramCount",None]]&],Return[Failure["QQWeightedManifestIncomplete",<||>]]];
 If[ids===All,ids=Range[256]];If[!ListQ[ids]||ids==={}||!DuplicateFreeQ[ids]||!AllTrue[ids,IntegerQ[#]&&1<=#<=256&],Return[Failure["QQWeightedRecordIDs",<||>]]];
 key=Hash[{"QQWeightedUVIRLedger/1",FileHash[manifest,"SHA256"],j["Fingerprint"],j["MixedSourceHash"],$weightedLedgerSHA256,$Version,DownValues[FeynCalc`TID],DownValues[FeynCalc`PaVeUVPart]},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-weighted-ledger-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQWeightedUVIRLedger",Block[{FeynCalc`$KeepLogDivergentScalelessIntegrals=True},Catch[
 Do[qqProgressUpdate[<|"Stage"->"QQWeightedUVIRLedger","Index"->id,"Total"->256,"StepLimitSeconds"->graphSec|>];
 recordFile=records[[id]]["ResultFile"];If[!FileExistsQ[recordFile],Throw[Failure["QQWeightedPairResultMissing",<||>],stop]];entry=qqWeightedRead[recordFile];
 If[Lookup[entry,"Status",None]=!="Complete"||Lookup[entry,"DiagramCount",None]=!=records[[id]]["DiagramCount"]||!And@@(KeyExistsQ[entry,#]&/@{"T","L","FieldPair","Sector"}),Throw[Failure["QQWeightedPairResultMismatch",<||>],stop]];
 expectedGraphs+=entry["DiagramCount"];sourceDir=Lookup[entry,"PerDiagramCache",None];
 graphFiles=If[entry["DiagramCount"]===0,{},If[!StringQ[sourceDir],Throw[Failure["QQWeightedGraphDirectoryMissing",<||>],stop]];Table[FileNameJoin[{sourceDir,"graph_"<>ToString[n]<>".wl"}],{n,entry["DiagramCount"]}]];
 If[!AllTrue[graphFiles,FileExistsQ],Throw[Failure["QQWeightedGraphMissing",<||>],stop]];graphSHAs=FileHash[#,"SHA256"]&/@graphFiles;
 tid=If[graphFiles==={},<|"Digest"->0,"Files"-><||>,"Context"->None|>,qqWeightedInventory[FileNameJoin[{sourceDir,"tid_units_v1"}],"unit_"]];
 pv=If[graphFiles==={},<|"Digest"->0,"Files"-><||>,"Context"->None|>,qqWeightedInventory[FileNameJoin[{sourceDir,"pv_units_v1"}],"pv_"]];
 dependencyKey=Hash[{key,id,FileHash[recordFile,"SHA256"],graphSHAs,tid["Digest"],pv["Digest"]},"SHA256"];components={};sums=<|"T"->{},"L"->{}|>;
 Do[qqProgressUpdate[<|"Stage"->("QQWeightedUVIRLedger:record-"<>ToString[id]),"Index"->n,"Total"->Length[graphFiles],"ItemStarted"->AbsoluteTime[],"StepLimitSeconds"->graphSec|>];
 sub=FileNameJoin[{dir,"record-"<>ToString[id]<>"-graph-"<>ToString[n]}];If[!DirectoryQ[sub],CreateDirectory[sub]];
 z=qqWeightedCheckpoint[FileNameJoin[{sub,"graph.wxf"}],Hash[{dependencyKey,n},"SHA256"],
 row=qqWeightedRead[graphFiles[[n]]];If[!AssociationQ[row]||!AssociationQ[Lookup[row,"Status",None]]||!AllTrue[{"T","L"},Function[c,And@@(Lookup[row["Status"],#<>c,None]==="Complete"&/@{"Decompose","Integrate","Scalar"})]],Failure["QQWeightedGraphIncomplete",<||>],
 components=Table[qqWeightedComponent[row,c,sourceDir,tid,pv,sub,key,sec],{c,{"T","L"}}];If[AnyTrue[components,FailureQ],First[Select[components,FailureQ]],AssociationThread[{"T","L"},components]]],graphSec];
 If[FailureQ[z],Throw[z,stop]];Do[AssociateTo[sums,c->Append[sums[c],z[c]["Scalar"]]],{c,{"T","L"}}];
 AppendTo[rows,<|"RecordID"->id,"GraphID"->n,"Pair"->entry["FieldPair"],"Sector"->entry["Sector"],"InputDigest"->Hash[{dependencyKey,n},"SHA256"],"ReportFile"->FileNameJoin[{sub,"graph.wxf"}],
 "TIDUnits"->Total[Length[z[#]["TIDUnits"]]&/@{"T","L"}],"PVIdentities"->Total[Length[z[#]["PVIdentities"]]&/@{"T","L"}],
 "WeightedUVTransferT"->z["T"]["WeightedUVTransferD"],"WeightedUVTransferL"->z["L"]["WeightedUVTransferD"]|>],{n,Length[graphFiles]}];
 qqProgressOperation["Verify all weighted graphs against the saved pair/sector total"];
 z=qqWeightedCheckpoint[FileNameJoin[{dir,"record-"<>ToString[id]<>".wxf"}],dependencyKey,
 And@@(qqWeightedEqualQ[Total[sums[#]],qqWeightedOldPolicy[entry[#]]]&/@{"T","L"}),graphSec];
 If[z=!=True,Throw[If[FailureQ[z],z,Failure["QQWeightedPairSumMismatch",<|"RecordID"->id|>]],stop]];
 AppendTo[recordRows,<|"RecordID"->id,"Pair"->entry["FieldPair"],"Sector"->entry["Sector"],"GraphCount"->Length[graphFiles],"InputDigest"->dependencyKey,"PairSumVerified"->True|>],{id,ids}];
 complete=Sort[ids]===Range[256]&&Length[recordRows]===256&&Sort[({#["Pair"],#["Sector"]}&/@recordRows)]===Sort[Flatten[Table[{pair,sector},{pair,Tuples[Range[8],2]},{sector,RGZWorkflow`Engine`RGZMixSectorNames}],1]];
 If[complete,qqProgressOperation["Bind the audited matrix to the QQ mixed-source hash"];
 sourceMatrix=qqWeightedCheckpoint[FileNameJoin[{dir,"matrix-source.wxf"}],Hash[{key,Lookup[recordRows,"InputDigest"]},"SHA256"],sourceMatrix=qqWeightedMatrixReadOnly[DirectoryName[manifest]];If[Dimensions[sourceMatrix]=!={5,5}||!qqAuditValidQ[sourceMatrix],Failure["QQWeightedMatrixAssemblyIncomplete",<||>],Hash[sourceMatrix,"SHA256"]],graphSec];
 If[FailureQ[sourceMatrix],Throw[sourceMatrix,stop]];match=sourceMatrix===j["MixedSourceHash"]];True,stop]],seconds];
 If[FailureQ[r],stopped=r];
 out=<|"Schema"->"RGZQQWeightedUVIRLedger/1","Status"->Which[stopped=!=None,"WeightedLedgerStopped",complete&&match,"WeightedGraphCoverageVerified",True,"PartialWeightedGraphCoverage"],"Key"->key,"AlgorithmSHA256"->$weightedLedgerSHA256,
 "ModelFingerprint"->modelKey[],"ModelContent"->modelContent[],"SourceFingerprint"->j["Fingerprint"],"MixedSourceHash"->j["MixedSourceHash"],"MatrixManifest"->manifest,"ManifestSHA256"->FileHash[manifest,"SHA256"],
 "Records"->recordRows,"Rows"->rows,"RecordsAudited"->Length[recordRows],"GraphsAudited"->Length[rows],"SourceGraphCoverageVerified"->(stopped===None&&complete&&match),"PrePVCoverageVerified"->(stopped===None&&complete&&match),"MatrixSourceMatchesQQ"->match,
 "AllWeightedTransfersZero"->AllTrue[rows,#["WeightedUVTransferT"]===0&&#["WeightedUVTransferL"]===0&],"ScalelessLedgerComplete"->False,
 "Convention"->"Separate UVTransferD and opposite IRTransferD through the finite Laurent term at D=4; full D dependence is retained before multiplying each graph coefficient; graph coefficients already contain FA statistics and symmetry factors. No loop measure or second symmetry factor is inserted.",
 "Scope"->"Finite-regulator mixed matrix only; combine with raw mass bounds, matched regions, probe ledger and explicit external-residue conditions.","Stopped"->stopped,"File"->FileNameJoin[{dir,"weighted-ledger.wxf"}]|>;
 z=atomicWXF[out["File"],out];If[FailureQ[z],Return[z]];storeResult["QQWeightedUVIRLedger",out];
 If[FailureQ[stopped],Return[Failure[stopped[[1]],Join[stopped[[2]],<|"ReportFile"->out["File"],"Action"->"Repeat RGZQQWeightedUVIRLedger; completed graph, unit and pair checks are reused."|>]]]];
 KeyTake[out,{"Status","RecordsAudited","GraphsAudited","SourceGraphCoverageVerified","AllWeightedTransfersZero","File"}]];
