(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Read-only follow-up proofs for the separate scalar presentation. These
   reports never upgrade a certificate or replace the original MSbar packet. *)
$scalarAuditSHA256=FileHash[$InputFileName,"SHA256"];
qqScalarCommonResidues[p_] := Module[{e=Unique["scalarResidue"],z},
 If[!AssociationQ[p]||!And@@(KeyExistsQ[p,#]&/@{"UV","IR"}),Return[Failure["ScalarPoleMissing",<||>]]];
 z=Expand[((p["UV"]/.FeynCalc`EpsilonUV->e)+(p["IR"]/.FeynCalc`EpsilonIR->e))/.D->4-2e];
 If[qqFFInvalidQ[z],Return[Failure["ScalarPoleMissing",<||>]]];
 {SeriesCoefficient[z,{e,0,-1}],SeriesCoefficient[z,{e,0,-2}]}];
qqScalarRegularJet[c_] := Module[{d,jet},
 d=Cancel[Together[Denominator[c]/.D->4]];
 If[d===0,Return[Failure["ScalarHigherEpsilonRequired",<||>]]];
 jet={c/.D->4,-2(D[c,D]/.D->4),2(D[c,{D,2}]/.D->4)};
 If[qqFFInvalidQ[jet],Failure["ScalarHigherEpsilonRequired",<||>],jet]];
qqScalarFiniteIdentity[pairs_,table_] := Module[{terms={},rational=0,j,p,stop=Unique["finiteIdentity"]},Catch[
 Do[j=qqScalarRegularJet[pair[[2]]];If[FailureQ[j],Throw[j,stop]];
 If[pair[[1]]===1,rational+=j[[1]],
 p=qqScalarCommonResidues[Lookup[table,pair[[1]],None]];If[FailureQ[p],Throw[p,stop]];
 AppendTo[terms,{pair[[1]],j[[1]]}];rational+=j[[2]]p[[1]]+j[[3]]p[[2]]],{pair,pairs}];
 <|"ScalarFiniteCoefficients"->terms,"DimensionalFiniteTransfer"->Cancel[Together[rational]]|>,stop]];
qqScalarSectorCertificate[inv_,mass_] := Module[{n=Length[mass],edges,dist,S,det,x,g,res,gram,conditions},
 If[n<1||n>4||Length[inv]=!=n(n-1)/2,Return[Failure["ScalarSectorShape",<||>]]];
 dist=ConstantArray[0,{n,n}];If[n>1,edges=qqPVEdges[n]+1;
 Do[dist[[edges[[k,1]],edges[[k,2]]]]=dist[[edges[[k,2]],edges[[k,1]]]]=inv[[k]],{k,Length[edges]}]];
 S=Table[mass[[a]]+mass[[b]]-dist[[a,b]],{a,n},{b,n}];det=Factor[Det[S]];
 If[det===0,Return[<|"Verified"->False,"Reason"->"DegenerateHessianRequiresSeparateAnalysis","Hessian"->S,"Determinant"->0|>]];
 x=Factor/@LinearSolve[S,-ConstantArray[1,n]];g=Factor[Total[x]/2];
 res=(Cancel[Together[#]]&)/@(S.x+ConstantArray[1,n]);
 gram=If[n===1,1,Factor[Det[Table[(dist[[1,a]]+dist[[1,b]]-dist[[a,b]])/2,{a,2,n},{b,2,n}]]]];
 conditions=DeleteDuplicates[Unequal[#,0]&/@DeleteDuplicates[Join[{det,g,gram},x]]];
 <|"Verified"->(res===ConstantArray[0,n]&&g=!=0&&gram=!=0&&AllTrue[x,#=!=0&]),
 "Hessian"->S,"Determinant"->det,"CriticalPoint"->x,"CriticalValue"->g,"CriticalResidual"->res,"GramDeterminant"->gram,
 "ProperCriticalPointCount"->If[g=!=0,1,0],"MilnorNumber"->1,"Conditions"->conditions,
 "Polynomial"->"G(x)=Sum[x]+1/2 x.Hessian.x; gradients=1+Hessian.x"|>];
qqScalarAuditCoordinates[input_] := Module[{x=RGZWorkflow`Engine`m^2,y=RGZWorkflow`Engine`\[Mu]^2,z=RGZWorkflow`Engine`mG^4,
 rm=RGZWorkflow`ScalarAudit`rMinus,rp=RGZWorkflow`ScalarAudit`rPlus,mu=RGZWorkflow`ScalarAudit`mu2,base,roots,rules,ratio,out},
 base=z-(x-y)^2/4;roots=DeleteDuplicates[Cases[input,Power[_,1/2],Infinity]];
 rules=Map[Function[root,ratio=Cancel[Together[root[[1]]/base]];
 If[MatchQ[ratio,_Rational|_Integer]&&TrueQ[ratio>0],root->Sqrt[ratio](rp-rm)/(2I),root->root]],roots];
 out=input/.Dispatch[rules];
 out=With[{ms=RGZWorkflow`Engine`m,mus=RGZWorkflow`Engine`\[Mu],mgs=RGZWorkflow`Engine`mG},out/.{
 HoldPattern[Power[ms,k_Integer]]/;EvenQ[k]:>(rp+rm-mu)^(k/2),
 HoldPattern[Power[mus,k_Integer]]/;EvenQ[k]:>mu^(k/2),
 HoldPattern[Power[mgs,k_Integer]]/;Divisible[k,4]:>((rp-mu)(rm-mu))^(k/4)}];
 out=out/.{RGZWorkflow`Engine`m^2->rp+rm-mu,RGZWorkflow`Engine`\[Mu]^2->mu,RGZWorkflow`Engine`mG^4->(rp-mu)(rm-mu),RGZWorkflow`Engine`qqS->RGZWorkflow`ScalarAudit`s,RGZWorkflow`Engine`qqT->RGZWorkflow`ScalarAudit`t};
 Map[Factor,out,{2}]];
qqScalarAuditExact[input_] := Module[{z=qqScalarAuditCoordinates[{{},{input}}][[2,1]],
 rm=RGZWorkflow`ScalarAudit`rMinus,rp=RGZWorkflow`ScalarAudit`rPlus,roots,rules,ratio},
 (* This is the selected pole branch, not PowerExpand: rPlus-rMinus=2 I Sqrt[base]. *)
 Do[roots=DeleteDuplicates[Cases[z,Power[_,1/2],Infinity]];
 rules=Map[Function[root,ratio=Cancel[Together[root[[1]]/(-(rm-rp)^2)]];
 If[MatchQ[ratio,_Rational|_Integer]&&TrueQ[ratio>0],root->Sqrt[ratio](rp-rm)/I,root->root]],roots];
 z=z/.Dispatch[rules];z=qqScalarAuditCoordinates[{{},{Expand[z]}}][[2,1]],{3}];
 Cancel[Together[Expand[z]]]];
qqScalarOrbit[inv_,mass_] := Module[{n=Length[mass],dist,edges},
 If[n===1,Return[{mass,{}}]];
 edges=qqPVEdges[n]+1;dist=ConstantArray[0,{n,n}];
 Do[dist[[edges[[k,1]],edges[[k,2]]]]=dist[[edges[[k,2]],edges[[k,1]]]]=inv[[k]],{k,Length[edges]}];
 First[Sort[Table[{mass[[per]],(dist[[per[[#[[1]]]],per[[#[[2]]]]]]&/@edges)},{per,Permutations[Range[n]]}]]]];
qqScalarCommonEmbedding[inv_,mass_] := Module[{n=Length[mass],s=RGZWorkflow`ScalarAudit`s,t=RGZWorkflow`ScalarAudit`t,
 g,vec,dd,target,edges,zero,sets,chosen,A,aug,piv},
 g={{0,s/2,-t/2},{s/2,0,(s+t)/2},{-t/2,(s+t)/2,0}};
 vec=Tuples[{-1,0,1},3];zero=First[FirstPosition[vec,{0,0,0}]];
 dd=Table[Expand[(a-b).g.(a-b)],{a,vec},{b,vec}];
 target=ConstantArray[0,{n,n}];If[n>1,edges=qqPVEdges[n]+1;
 Do[target[[edges[[k,1]],edges[[k,2]]]]=target[[edges[[k,2]],edges[[k,1]]]]=inv[[k]],{k,Length[edges]}]];
 sets={{zero}};
 Do[sets=Flatten[Map[Function[as,Map[Append[as,#]&,Select[Range[Length[vec]],Function[j,And@@Table[dd[[as[[h]],j]]===Expand[target[[h,pos]]],{h,Length[as]}]]]]],sets],1];
 If[sets==={},Break[]],{pos,2,n}];
 If[sets==={},Return[Failure["ScalarCommonEmbeddingMissing",<||>]]];
 chosen=vec[[First[sets]]];A=Prepend[#,1]&/@chosen;
 aug=MapThread[Append,{A,MapThread[Subtract,{Expand[#.g.#]&/@chosen,mass}]}];
 piv=SelectFirst[Subsets[Range[4],{n}],Det[A[[All,#]]]=!=0&,Missing[]];
 If[MissingQ[piv],Return[Failure["ScalarCutRankDeficient",<||>]]];
 <|"Chords"->chosen,"CutEquations"->aug,"PivotColumns"->piv,"AmbientGram"->Factor[Det[g]]|>];
qqScalarCutSeparation[target_,other_] := Module[{a=target["CutEquations"],b=other["CutEquations"],piv=target["PivotColumns"],lambda,res,witness=None},
 Do[lambda=LinearSolve[Transpose[a[[All,piv]]],b[[j,piv]]];
 res=Factor/@(b[[j]]-lambda.a);
 If[AnyTrue[res,#=!=0&],witness=<|"OtherDenominator"->j,"EliminationCoefficients"->lambda,"NonzeroAffineResidual"->res|>;Break[]],{j,Length[b]}];
 witness];

Options[RGZQQScalarBasisAudit]={"ScalarFile"->Automatic};
RGZQQScalarBasisAudit[seconds_:300,OptionsPattern[]] := Module[{p,r,rows={},v,z,orbit,conditions,key,out,file,embeddings={},cutChecks={},embedding,witness,cutOK=False,cutConditions={},stop=Unique["sectorProof"]},
 r=ready[];If[FailureQ[r],Return[r]];p=qqAuditRead[OptionValue["ScalarFile"],"QQScalarFormFactors"];
 If[!AssociationQ[p]||!ListQ[Lookup[p,"ScalarIntegralInventory",None]]||p["ScalarIntegralInventory"]==={},Return[Failure["ScalarFormFactorsRequired",<||>]]];
 r=qqGaugeTimed["QQScalarBasisAudit",Catch[
 Do[qqProgressOperation["Prove one-loop sector critical-point count"];
 v=If[MatchQ[m,_FeynCalc`A0],{{0},{},{First[m]}},qqPVParse[m]];
 If[v===$Failed||v[[1]]=!={0},Throw[Failure["ScalarSectorShape",<||>],stop]];
 v=qqScalarAuditCoordinates[{v[[2]],v[[3]]}];
 If[!FreeQ[v,RGZWorkflow`Engine`m|RGZWorkflow`Engine`\[Mu]|RGZWorkflow`Engine`mG],Throw[Failure["ScalarSpectralCoordinatesUnsupported",<||>],stop]];
 z=qqScalarSectorCertificate[v[[1]],v[[2]]];If[FailureQ[z],Throw[z,stop]];
 AppendTo[rows,Join[<|"Master"->m,"InvariantCoordinates"->v[[1]],"MassCoordinates"->v[[2]],"Orbit"->qqScalarOrbit[v[[1]],v[[2]]]|>,z]],{m,p["ScalarIntegralInventory"]}];True,stop],seconds];
 If[FailureQ[r],Return[r]];
 r=qqGaugeTimed["QQScalarBasisCuts",Catch[
 Do[embedding=qqScalarCommonEmbedding[rows[[i]]["InvariantCoordinates"],rows[[i]]["MassCoordinates"]];
 If[FailureQ[embedding],Throw[embedding,stop]];AppendTo[embeddings,embedding],{i,Length[rows]}];
 Do[If[i=!=j&&Length[embeddings[[i]]["Chords"]]===Length[embeddings[[j]]["Chords"]],
 witness=qqScalarCutSeparation[embeddings[[i]],embeddings[[j]]];
 AppendTo[cutChecks,<|"Target"->i,"Other"->j,"Separated"->(witness=!=None),"Witness"->witness|>]],{i,Length[rows]},{j,Length[rows]}];True,stop],seconds];
 If[FailureQ[r],Return[r]];cutOK=AllTrue[cutChecks,TrueQ[#["Separated"]]&];
 cutConditions=DeleteDuplicates[Map[Function[c,Or@@(Unequal[#,0]&/@c["Witness"]["NonzeroAffineResidual"])],Select[cutChecks,TrueQ[#["Separated"]]&]]];
 orbit=DeleteDuplicates[Lookup[rows,"Orbit"]];conditions=DeleteDuplicates[DeleteCases[Join[Flatten[Lookup[rows,"Conditions",{}],1],cutConditions,{embeddings[[1]]["AmbientGram"]!=0}],True]];
 key=Hash[{"ScalarBasisAudit/1",p["Key"],$scalarAuditSHA256},"SHA256"];
 file=FileNameJoin[{cacheDirectory[],"qq-scalar-basis-audit-"<>IntegerString[key,16,64]<>".wxf"}];
 out=<|"Status"->If[AllTrue[rows,TrueQ[#["Verified"]]&],If[cutOK&&Length[orbit]===Length[rows],"MinimalWithinDeclaredIBPFamily","SectorIrreducibilityVerified"],"SectorAnalysisIncomplete"],"Key"->key,
 "SourceFingerprint"->p["SourceFingerprint"],"InputSHA256"->p["InputSHA256"],"ScalarFile"->p["File"],"Rows"->rows,
 "SectorIrreducibilityVerified"->AllTrue[rows,TrueQ[#["Verified"]]&],"DistinctSectorOrbits"->Length[orbit],"CandidateCount"->Length[rows],
 "CommonFamilyEmbeddings"->embeddings,"CutSeparationChecks"->cutChecks,"CommonFamilyCutIndependenceVerified"->cutOK,
 "DeclaredIBPMinimalityVerified"->(cutOK&&Length[orbit]===Length[rows]&&AllTrue[rows,TrueQ[#["Verified"]]&]),
 "CutArgument"->"Descending propagator count: smaller sectors have zero maximal residue. Every same-size different sector has a nonzero affine-ideal residual on the target cut; each diagonal cut is nonzero on the stated nondegenerate Gram/Cayley domain. Full propagator permutations are deduplicated. This proves independence in the explicit common parent IBP family.",
 "CoefficientField"->"Rational functions of generic D,s,t,mu2,rMinus,rPlus; fixed dimension; IBP within each sector",
 "Conditions"->conditions,"ParameterPointTest"->False,"GlobalMinimalityProved"->False,"MasterBasisIndependent"->False,
 "Outstanding"->{"No proof of unrestricted functional identities outside the declared common-parent IBP and propagator-permutation relations","No claim about an epsilon-truncated or finite-function basis"},
 "TheoremSource"->"https://arxiv.org/abs/1308.6676","Scope"->"Generic-D one-loop critical-point count, exact common-parent cut separation and permutation-orbit test; no claim for truncated epsilon series or arbitrary functional identities.","File"->file|>;
 r=atomicWXF[file,out];If[FailureQ[r],Return[r]];storeResult["QQScalarBasisAudit",out];KeyTake[out,{"Status","CandidateCount","DistinctSectorOrbits","DeclaredIBPMinimalityVerified","GlobalMinimalityProved","File"}]];

Options[RGZQQScalarFiniteAudit]={"ScalarFile"->Automatic,"ReferenceUVIRFile"->Automatic,"ReferenceMSbarFile"->Automatic,
 "ReferenceFiniteAuditFile"->Automatic,"PoleAuditFile"->Automatic,"CoefficientSeconds"->30};
RGZQQScalarFiniteAudit[seconds_:3600,OptionsPattern[]] := Module[
 {p,o,m,fa,pa,u,src,r,tab,ids,masters,key,dir,file,co=OptionValue["CoefficientSeconds"],checks={},finiteIDs=<||>,rows={},expected,actual,keys,delta,z,oldPole,newPole,jet,ref,packet,fin,out,stop=Unique["finiteScalar"]},
 If[!And@@(NumericQ[#]&&TrueQ[#>0]&/@{seconds,co}),Return[Failure["TimeLimit",<||>]]];
 r=ready[];If[FailureQ[r],Return[r]];r=fcReady[];If[FailureQ[r],Return[r]];
 p=qqAuditRead[OptionValue["ScalarFile"],"QQScalarFormFactors"];o=qqAuditRead[OptionValue["ReferenceUVIRFile"],"QQUVIR"];
 m=qqAuditRead[OptionValue["ReferenceMSbarFile"],"QQMSbar"];fa=qqAuditRead[OptionValue["ReferenceFiniteAuditFile"],"QQFiniteAudit"];
 pa=qqAuditRead[OptionValue["PoleAuditFile"],"QQScalarPoleAudit"];
 If[!AllTrue[{p,o,m,fa,pa},AssociationQ]||!TrueQ[Lookup[pa,"CommonRegulatorPolesVerified",False]]||
 Lookup[pa,"ScalarFormFactorsFile",None]=!=Lookup[p,"File",Missing[]]||!TrueQ[Lookup[fa,"FiniteAssemblyVerified",False]]||
 Lookup[fa,"AlgorithmSHA256",None]=!=$certificationSourceSHA256||Lookup[fa,"MSbarKey",None]=!=Lookup[m,"Key",Missing[]]||
 !AllTrue[{o,m,fa,pa},Lookup[#,"SourceFingerprint",None]===p["SourceFingerprint"]&]||
 !AllTrue[{o,m,pa},Lookup[#,"InputSHA256",None]===p["InputSHA256"]&],Return[Failure["ScalarFiniteEvidenceMismatch",<||>]]];
 src=Import[p["SourceFormFactorsFile"],"WXF"];u=Import[pa["ScalarUVIRFile"],"WXF"];
 If[FileHash[p["SourceFormFactorsFile"],"SHA256"]=!=p["SourceFormFactorsSHA256"]||
 FileHash[o["FormFactorsFile"],"SHA256"]=!=m["FormFactorsSHA256"]||o["FormFactorsFile"]=!=src["File"]||
 u["FormFactorsFile"]=!=p["File"]||src["ColorBasis"]=!=p["ColorBasis"]||src["SpinStructures"]=!=p["SpinStructures"],Return[Failure["ScalarFiniteEvidenceMismatch",<||>]]];
 (* Imported Association keys must be reevaluated under the current FeynCalc
    definitions before lookup: A0/PaVe heads can normalize their arguments. *)
 o=Join[o,<|"MasterPoles"->Association[Normal[o["MasterPoles"]]]|>];tab=Association[Normal[u["MasterPoles"]]];ids=Association[p["PVIdentities"]];
 key=Hash[{"ScalarFiniteAudit/1",p["Key"],o["Key"],m["Key"],fa["Key"],pa["Key"],$scalarAuditSHA256,co},"SHA256"];
 dir=FileNameJoin[{cacheDirectory[],"qq-scalar-finite-audit-"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=qqGaugeTimed["QQScalarFiniteAudit",Catch[
 masters=DeleteDuplicates[DeleteCases[Flatten[(First/@#)&/@Values[ids],1],1]];
 Do[If[!KeyExistsQ[tab,x],z=qqUVIRMaster[x,False];If[qqFFInvalidQ[z],Throw[Failure["ScalarFiniteUnsupportedMaster",<|"Master"->x|>],stop]];AssociateTo[tab,x->z]],{x,masters}];
 Do[qqProgressUpdate[<|"Stage"->"QQScalarFiniteAudit:Identities","Index"->i,"Total"->Length[ids],"Master"->Keys[ids][[i]],"StepLimitSeconds"->co|>];
 qqProgressOperation["Check primitive common poles and epsilon-dependent finite map"];
 ref=Keys[ids][[i]];
 z=qqAuditCheckpoint[FileNameJoin[{dir,"identity-"<>ToString[i]<>".wxf"}],Hash[{key,i},"SHA256"],Module[{v=ids[ref],a,b,mapping},
 a=If[ref===1,{0,0},qqScalarCommonResidues[Lookup[o["MasterPoles"],ref,None]]];
 b=Total[Map[Function[term,If[First[term]===1,{0,0},With[{j=qqScalarRegularJet[Last[term]],q=qqScalarCommonResidues[Lookup[tab,First[term],None]]},If[qqFFInvalidQ[{j,q}],{Missing["Pole"],Missing["Pole"]},{j[[1]]q[[1]]+j[[2]]q[[2]],j[[1]]q[[2]]}]]]],v]];
 If[qqFFInvalidQ[{a,b}],Return[Failure["ScalarFiniteUnsupportedMaster",<|"Master"->ref|>]]];
 mapping=qqScalarFiniteIdentity[v,tab];If[FailureQ[mapping],Return[mapping]];
 <|"CommonPoleDifference"->(qqCoefficientRational/@(a-b)),"FiniteMap"->mapping|>],co];
 If[FailureQ[z],Throw[z,stop]];If[z["CommonPoleDifference"]=!={0,0},Throw[Failure["ScalarPrimitivePoleMismatch",<|"Master"->ref,"Difference"->z["CommonPoleDifference"]|>],stop]];
 AssociateTo[finiteIDs,ref->z["FiniteMap"]];AppendTo[checks,True],{i,Length[ids]}];
 Do[qqProgressUpdate[<|"Stage"->"QQScalarFiniteAudit:Rows","Index"->n,"Total"->Length[p["Rows"]]|>];
 ref=SelectFirst[src["Rows"],#["ColorID"]===p["Rows"][[n]]["ColorID"]&&#["SpinID"]===p["Rows"][[n]]["SpinID"]&,Missing[]];
 If[!AssociationQ[ref],Throw[Failure["ScalarFiniteRowMismatch",<||>],stop]];
 expected=qqScalarCollectRow[ref["MasterCoefficients"],ids];If[FailureQ[expected],Throw[expected,stop]];
 expected=Association[(First[#]->Last[#])&/@expected];actual=Association[(First[#]->Last[#])&/@p["Rows"][[n]]["MasterCoefficients"]];keys=Union[Keys[expected],Keys[actual]];
 Do[qqProgressOperation["Verify exact D-dimensional scalar coefficient "<>ToString[k]<>" / "<>ToString[Length[keys]]];
 qqProgressUpdate[<|"Master"->keys[[k]],"AlgebraGroupIndex"->k,"AlgebraGroupTotal"->Length[keys]|>];
 delta=qqAuditCheckpoint[FileNameJoin[{dir,"row-"<>ToString[n]<>"-co-"<>ToString[k]<>".wxf"}],Hash[{key,n,k},"SHA256"],
 qqScalarAuditExact[Lookup[expected,keys[[k]],0]-Lookup[actual,keys[[k]],0]],co];
 If[FailureQ[delta],Throw[delta,stop]];If[delta=!=0,Throw[Failure["ScalarFiniteCoefficientMismatch",<|"Row"->n,"Master"->keys[[k]]|>],stop]],{k,Length[keys]}];
 qqProgressOperation["Build scalar finite row with epsilon-times-pole contributions"];
 fin=qqAuditCheckpoint[FileNameJoin[{dir,"finite-row-"<>ToString[n]<>".wxf"}],Hash[{key,n,"finite"},"SHA256"],
 qqMSbarFiniteRow[p["Rows"][[n]]["MasterCoefficients"],tab,m["RenormalizationScaleSquared"],co],Max[co,Length[keys]co]];
 If[FailureQ[fin],Throw[fin,stop]];
 AppendTo[rows,Join[KeyTake[p["Rows"][[n]],{"ColorID","SpinID"}],fin,<|"ExactDdimensionalCoefficientMatch"->True,"FiniteEquivalenceByLaurentProductRule"->True|>]],{n,Length[p["Rows"]]}];True,stop],seconds];
 If[FailureQ[r],Return[r]];
 out=<|"Status"->"ScalarFiniteRepresentationVerified","Key"->key,"SourceFingerprint"->p["SourceFingerprint"],"InputSHA256"->p["InputSHA256"],
 "Rows"->rows,"ColorBasis"->p["ColorBasis"],"SpinStructures"->p["SpinStructures"],"FiniteMasterIdentities"->Normal[finiteIDs],
 "PrimitivePoleChecks"->Length[checks],"FiniteAmplitudeEquivalenceVerified"->(And@@checks&&Length[rows]===Length[p["Rows"]]),
 "Proof"->"For I=sum a_j(D) J_j, Finite[I]=sum a_j(4) Finite[J_j]-2 a'_j(4) Pole1[J_j]+2 a''_j(4) Pole2[J_j]. Primitive residues and every full-D collected coefficient checked exactly; Laurent product rule then proves the complete finite basis transformation.",
 "SelectedPoleBranch"->"rPlus-rMinus=2 I Sqrt[mG^4-(m^2-mu^2)^2/4]; positive rational rescalings of this principal radical only; no PowerExpand",
 "NativePVIdentityAssumption"->True,"FiniteFunctionsNumericallyEvaluated"->False,"FullSMatrixVerified"->False,
 "ScalarFormFactorsFile"->p["File"],"ReferenceMSbarFile"->m["File"],"ReferenceFiniteAuditFile"->fa["File"],"ReferenceUVIRFile"->o["File"],"PoleAuditFile"->pa["File"],
 "RenormalizationScaleSquared"->m["RenormalizationScaleSquared"],"Conditions"->DeleteDuplicates[Join[Lookup[m,"IRValidityConditions",{}],Lookup[u,"IRValidityConditions",{}],Flatten[Lookup[Values[tab],"IRConditions",{}],1]]],
 "OneLoopFiniteAmplitude"->Total[(p["ColorBasis"][[#["ColorID"]]]p["SpinStructures"][[#["SpinID"]]]#["FiniteFormFactor"])&/@rows],
 "TreeAmplitude"->m["TreeAmplitude"],"Scope"->"Finite representation equivalent to the referenced MSbar amplitude conditional on its domain and exact native D-dimensional identities. No certificate or separate UV/IR ledger is overwritten.",
 "File"->FileNameJoin[{dir,"finite-audit.wxf"}]|>;
 AssociateTo[out,"Amplitude"->out["TreeAmplitude"]+out["OneLoopFiniteAmplitude"]];
 z=atomicWXF[out["File"],out];If[FailureQ[z],Return[z]];storeResult["QQScalarFiniteAudit",out];
 KeyTake[out,{"Status","PrimitivePoleChecks","FiniteAmplitudeEquivalenceVerified","FullSMatrixVerified","File"}]];
