(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringIntegralCache.wl"}]];
(* Canonical loop origin, including the numerator: translation/reflection
   has unit Jacobian in dimensional regularization. *)
qqTensorAnchor[core_] := Module[{q=RGZWorkflow`Engine`q,props,route,a,r},
 props=Select[Cases[core,_FeynCalc`PropagatorDenominator,Infinity],!FreeQ[#,q]&];
 If[props==={},Return[core]];
 route=First[props][[1]]/.FeynCalc`Momentum[v_,___]:>v;
 If[!PolynomialQ[route,q]||Exponent[route,q]=!=1,Return[Failure["QQTensorRouting",<||>]]];
 a=Coefficient[route,q];If[!MemberQ[{1,-1},a],Return[Failure["QQTensorRouting",<|"LoopCoefficient"->a|>]]];
 r=Expand[route-a q];
 FeynCalc`MomentumCombine[core/.q->(q-r)/a]];
qqRouteApartDen[fad_FeynCalc`FeynAmpDenominator,recur_:qqRouteApartDen] := Module[{props=List@@fad,pair=None,a,b,i,j,rest},
 Do[If[Head[props[[i]]]===FeynCalc`PropagatorDenominator&&Head[props[[j]]]===FeynCalc`PropagatorDenominator&&
 Length[props[[i]]]===2&&Length[props[[j]]]===2&&
 (Expand[props[[i,1]]-props[[j,1]]]===0||Expand[props[[i,1]]+props[[j,1]]]===0)&&
 !TrueQ[Cancel[props[[i,2]]^2-props[[j,2]]^2]===0],pair={i,j};Break[]],{i,Length[props]-1},{j,i+1,Length[props]}];
 If[pair===None,Return[fad]];
 {i,j}=pair;a=props[[i,2]]^2;b=props[[j,2]]^2;
 (recur[FeynCalc`FeynAmpDenominator@@Delete[props,j]]-recur[FeynCalc`FeynAmpDenominator@@Delete[props,i]])/(a-b)];
(* Local memoization also shares intermediate denominator subsets. Thousands
   of NAV occurrences must not repeat the same recursive partial fraction. *)
qqRouteApart[input_] := Module[{memo,fads,values,rules},
 memo[fad_FeynCalc`FeynAmpDenominator]:=memo[fad]=qqRouteApartDen[fad,memo];
 fads=DeleteDuplicates[Cases[input,_FeynCalc`FeynAmpDenominator,{0,Infinity}]];
 values=Table[
  qqProgressOperation["Partial fraction denominator "<>ToString[i]<>" / "<>ToString[Length[fads]]];
  qqProgressUpdate[<|"AlgebraGroupIndex"->i,"AlgebraGroupTotal"->Length[fads]|>];
  memo[fads[[i]]],{i,Length[fads]}];
 rules=Dispatch[MapThread[Rule,{fads,values}]];
 Expand[input/.rules]];

(* Bilinearity only, with no Clifford or four-dimensional identities. Scalar
   components commute with the open chain; unrecognized matrix factors stay
   in the chain. Expanding Dot via the general simplifier is much costlier
   for the complete NAV matrix sum. *)
qqChainScalarQ[x_] := TrueQ[FeynCalc`NonCommFreeQ[x]]&&
 FreeQ[x,FeynCalc`Spinor|FeynCalc`DiracChain|Dot];
qqChainMonomial[args_List] := Module[{co=1,chain={},f,scalar,remaining},
 Do[f=If[Head[arg]===Times,List@@arg,{arg}];
  scalar=Select[f,qqChainScalarQ];remaining=Select[f,!qqChainScalarQ[#]&];
  co*=Times@@scalar;
  If[remaining=!={},AppendTo[chain,Times@@remaining]],{arg,args}];
 co If[chain==={},1,Dot@@chain]];
qqLinearizeChains[input_] := Expand[input/.d_Dot:>
 Total[qqChainMonomial/@Tuples[If[Head[#]===Plus,List@@#,{#}]& /@ (List@@d)]]];

(* v^n/(v-mass^2) = Sum[mass^(2j) v^(n-1-j)] +
   mass^(2n)/(v-mass^2). Lower tensor rank before reduction, while keeping
   massive remainders and repeated poles. Only explicit scalar factors are
   considered; slash momenta and open-chain order are untouched. *)
qqCancelRouteMonomial[term_] := Module[{f,fads,powers,fad,props,v,n,mass,rest,mom,stop=Unique["qqRankStop"]},
 f=If[Head[term]===Times,List@@term,{term}];
 fads=Select[f,Head[#]===FeynCalc`FeynAmpDenominator&];
 powers=Select[f,MatchQ[#,_FeynCalc`Pair|Power[_FeynCalc`Pair,_Integer?Positive]]&];
 Catch[Do[v=If[Head[power]===Power,power[[1]],power];
  n=If[Head[power]===Power,power[[2]],1];
  If[Length[v]=!=2||v[[1]]=!=v[[2]]||FreeQ[v,RGZWorkflow`Engine`q],Continue[]];
  Do[props=List@@fad;
   Do[If[Head[props[[i]]]=!=FeynCalc`PropagatorDenominator,Continue[]];
    mom=props[[i,1]];
    If[Expand[mom-v[[1]]]=!=0&&Expand[mom+v[[1]]]=!=0,Continue[]];
    mass=props[[i,2]];rest=term/(power fad);
    Throw[qqCancelRoutePowers[Expand[
     rest (FeynCalc`FeynAmpDenominator@@Delete[props,i])
       Sum[If[j===0,1,mass^(2 j)] If[n-1-j===0,1,v^(n-1-j)],{j,0,n-1}]+rest mass^(2 n) fad]],stop],
    {i,Length[props]}],{fad,fads}],{power,powers}];term,stop]];
qqCancelRoutePowers[input_] := Total[qqCancelRouteMonomial /@
 If[Head[input]===Plus,List@@input,{input}]];

(* Three scalar coordinates of the NAV loop. These are exact momentum
   identities, including external squares: no on-shell limit is taken here.
   Slash momenta and Lorentz-vector factors remain tensor numerators. *)
qqNAVScalarCoordinates[input_,name_] := Module[{q=RGZWorkflow`Engine`q,ki,ko,p,a,b,c,
  pair,squares,rules,x},
 If[!MemberQ[{"NAV1","NAV2"},name],Return[input]];
 {ki,ko}=If[name==="NAV1",{RGZWorkflow`Engine`qqK1,RGZWorkflow`Engine`qqK3},
  {RGZWorkflow`Engine`qqK2,RGZWorkflow`Engine`qqK4}];p=ko-ki;
 pair[u_,v_]:=FeynCalc`FCI[FeynCalc`SPD[u,v]];
 squares=FeynCalc`MomentumExpand /@ {pair[q,q],pair[q+p,q+p],pair[q-ki,q-ki]};
 rules={pair[q,q]->a,pair[q,ki]->(a+pair[ki,ki]-c)/2,
  pair[ki,q]->(a+pair[ki,ki]-c)/2,
  pair[q,ko]->(b+pair[ki,ki]-pair[p,p]-c)/2,
  pair[ko,q]->(b+pair[ki,ki]-pair[p,p]-c)/2};
 x=input/.v:FeynCalc`Pair[_FeynCalc`Momentum,_FeynCalc`Momentum]/;
  !FreeQ[v,q]:>FeynCalc`ExpandScalarProduct[v];
 Expand[x/.rules]/.{a->squares[[1]],b->squares[[2]],c->squares[[3]]}];

(* Equivalent linear reduction: scalar weights stay outside tensor reduction.
   No changes to external kinematics, dimensional scheme or PV conventions. *)
qqTensorPieces[input_] := Module[{terms,parts,groups,q=RGZWorkflow`Engine`q},
 terms=If[Head[input]===Plus,List@@input,{input}];
 parts=Function[term,Module[{f=If[Head[term]===Times,List@@term,{term}],scalar,co,core,boundLor},
 boundLor=DeleteDuplicates[Cases[Select[f,!FreeQ[#,q]&],_FeynCalc`LorentzIndex,Infinity]];
 scalar=Select[f,FreeQ[#,q]&&Intersection[DeleteDuplicates[Cases[#,_FeynCalc`LorentzIndex,Infinity]],boundLor]==={}&];
 co=Times@@scalar;core=Times@@Complement[f,scalar];
 If[co core=!=term,Return[Failure["QQTensorFactorization",<||>]]];{co,core}]] /@ terms;
 If[AnyTrue[parts,FailureQ],Return[First[Select[parts,FailureQ]]]];
 groups=GatherBy[parts,Last];
 ({Factor[Cancel[Together[Total[#[[All,1]]]]]],#[[1,2]]}& /@ groups)];
(* Work on the numerator only: propagator routing and pole multiplicities
   remain untouched. A bounded non-expanding pass avoids expression swell.
   The same D-dimensional on-shell identities apply to all six QQ parts. *)
qqRestoreAffineSquares[input_] := Module[{q=RGZWorkflow`Engine`q,ks,routes,squares,expanded},
 ks={RGZWorkflow`Engine`qqK1,RGZWorkflow`Engine`qqK2,RGZWorkflow`Engine`qqK3,RGZWorkflow`Engine`qqK4};
 routes=DeleteDuplicates[Join[{q},Flatten[Table[q+a k,{k,ks},{a,{-1,1}}]],
  Flatten[Table[q+a (ks[[i]]-ks[[j]]),{i,4},{j,i+1,4},{a,{-1,1}}]]]];
 squares=FeynCalc`FCI[FeynCalc`SPD[#,#]]& /@ routes;
 expanded=FeynCalc`ExpandScalarProduct /@ squares;
 FeynCalc`FCI[input]/.HoldPattern[Power[den_Plus,n_Integer?Negative]]:>Module[{difference,answer=den},
  Do[difference=Expand[FeynCalc`ExpandScalarProduct[den]-expanded[[i]]];
   If[FreeQ[difference,q],answer=squares[[i]]+difference;Break[]],{i,Length[squares]}];answer^n]];

qqTensorNumeratorSimplify[core_,seconds_] := Module[{den,num,candidate},
 den=Cases[core,_FeynCalc`FeynAmpDenominator,Infinity];
 num=core/._FeynCalc`FeynAmpDenominator->1;
 If[num Times@@den=!=core,Return[core]];
 candidate=TimeConstrained[
  FeynCalc`DiracSimplify[num,FeynCalc`DiracEquation->True,
   FeynCalc`SirlinSimplify->False,FeynCalc`Expanding->False,
   FeynCalc`ExpandScalarProduct->False,FeynCalc`DiracGammaCombine->True],
  Min[10,seconds],$TimedOut];
 If[!FreeQ[candidate,_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity]||
   LeafCount[candidate]>=LeafCount[num],Return[core]];
 candidate Times@@den];

(* MomentumCombine in qqTensorAnchor can expose route-square cancellations
   that were invisible before shifting. Apply the exact polynomial division
   again, and re-anchor EACH resulting integral: deleting a propagator can
   remove the zero-route line required by TID (notably shifted tadpoles). *)
qqReduceAnchoredCore[core_,seconds_,depth_:0] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[{lowered,parts,results={},next,r},
 qqProgressOperation["Cancel route squares after loop-momentum shift"];
 lowered=qqCancelRoutePowers[core];
 If[lowered===core||depth>=32,Return[RGZWorkflow`Engine`rgzQQReduce[core]]];
 parts=qqTensorPieces[lowered];If[FailureQ[parts],Return[parts]];
 Do[
  (* Polynomial contact integrals generated by cancellation are scaleless
     in dimensional regularization, including the constant contact term. *)
  If[FreeQ[part[[2]],FeynCalc`FeynAmpDenominator],AppendTo[results,0];Continue[]];
  qqProgressOperation["Re-anchor lower-rank integral "<>ToString[Length[results]+1]<>" / "<>ToString[Length[parts]]];
  next=qqTensorAnchor[part[[2]]];If[FailureQ[next],Throw[next,rgzFunctionReturnTag]];
  next=qqTensorNumeratorSimplify[next,seconds];
  r=qqReduceAnchoredCore[next,seconds,depth+1];If[FailureQ[r],Throw[r,rgzFunctionReturnTag]];
  AppendTo[results,part[[1]] r],{part,parts}];
 Total[results]],rgzFunctionReturnTag]];

qqReduceTensorGroups[input_,job_,name_,seconds_] := Module[{pieces,dir,results={},key,file,packet,r,saved,answer,stop=Unique["qqTensorStop"]},
 qqProgressOperation["Linearize open Dirac chains"];
 r=qqLinearizeChains[input];
 qqProgressUpdate[<|"OutputLeaves"->LeafCount[r]|>];
 qqProgressOperation["Combine propagators across chain terms"];
 r=FeynCalc`FeynAmpDenominatorCombine[r,FeynCalc`FCI->True];
 qqProgressOperation["Partial fractions on identical momentum routes"];
 r=qqRouteApart[r];
 qqProgressOperation["Resolve NAV scalar products into propagator coordinates"];
 r=qqNAVScalarCoordinates[r,name];
 qqProgressOperation["Cancel explicit route squares and lower tensor rank"];
 r=qqCancelRoutePowers[r];
 qqProgressOperation["Group equivalent tensor cores and scalar weights"];
 pieces=qqTensorPieces[r];If[FailureQ[pieces],Return[pieces]];
 dir=FileNameJoin[{job["Directory"],name<>"-tensor"}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 Internal`InheritedBlock[{FeynCalc`TID},
 SetOptions[FeynCalc`TID,FeynCalc`FDS->False];
 answer=qqWithTensorIntegralCache[job,Catch[
 Do[
 pieces[[i,2]]=qqTensorAnchor[pieces[[i,2]]];If[FailureQ[pieces[[i,2]]],Throw[pieces[[i,2]],stop]];
 qqProgressOperation["TID tensor core "<>ToString[i]<>" / "<>ToString[Length[pieces]]];
 qqProgressUpdate[<|"AlgebraGroupIndex"->i,"AlgebraGroupTotal"->Length[pieces],"InputLeaves"->LeafCount[pieces[[i,2]]]|>];
 If[pieces[[i,1]]===0,AppendTo[results,0];Continue[]];
 key=Hash[{"QQTensorCore/3",job["Fingerprint"],$Version,Options[FeynCalc`TID],pieces[[i,2]]},"SHA256"];
 file=FileNameJoin[{dir,IntegerString[key,16,64]<>".wxf"}];
 If[FileExistsQ[file],
 packet=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[AssociationQ[packet]&&Lookup[packet,"Key",None]===key&&
  KeyExistsQ[packet,"Value"]&&qqFFInvalidQ[packet["Value"]],
  saved=RenameFile[file,file<>".invalid-"<>CreateUUID[]];
  qqProgressOperation["Isolate invalid tensor checkpoint and recompute"]]];
 If[FileExistsQ[file],
 packet=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!AssociationQ[packet]||Lookup[packet,"Key",None]=!=key||!KeyExistsQ[packet,"Value"],Throw[Failure["QQTensorCacheInvalid",<|"File"->file|>],stop]];
 r=packet["Value"];
 qqProgressUpdate[<|"CacheState"->"Reused","ReusedCheckpoints"->(Lookup[$qqProgress,"ReusedCheckpoints",0]+1)|>],
 qqProgressUpdate[<|"CacheState"->"Computing"|>];
 qqProgressOperation["Simplify D-dimensional numerator "<>ToString[i]<>" / "<>ToString[Length[pieces]]];
 r=qqTensorNumeratorSimplify[pieces[[i,2]],seconds];
 qqProgressUpdate[<|"OutputLeaves"->LeafCount[r]|>];
 qqProgressOperation["TID tensor core "<>ToString[i]<>" / "<>ToString[Length[pieces]]];
 r=timed["QQTensor:"<>name<>":"<>ToString[i],qqIntegralResolve[qqReduceAnchoredCore[r,seconds]],seconds];
 If[FailureQ[r],Throw[r,stop]];
 If[!FreeQ[r,_HoldForm|_Failure|_Missing|FeynCalc`TID|FeynCalc`FeynAmpDenominator|_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity],Throw[Failure["QQTensorReductionIncomplete",<|"Part"->name,"Core"->i|>],stop]];
 qqProgressOperation["Save tensor core "<>ToString[i]];
 saved=atomicWXF[file,<|"Key"->key,"Value"->r|>];If[FailureQ[saved],Throw[saved,stop]];
 qqProgressUpdate[<|"CacheState"->"Saved","ComputedCheckpoints"->(Lookup[$qqProgress,"ComputedCheckpoints",0]+1)|>]];
 If[!FreeQ[r,_HoldForm|_Failure|_Missing|FeynCalc`TID|FeynCalc`FeynAmpDenominator|_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity],Throw[Failure["QQTensorCacheInvalid",<|"File"->file|>],stop]];
 AppendTo[results,pieces[[i,1]] r],{i,Length[pieces]}];
 qqProgressOperation["Assemble reduced tensor cores"];
 Total[results],stop]];answer]
 ];
