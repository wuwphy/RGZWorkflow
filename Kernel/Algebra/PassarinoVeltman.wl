(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Apply scalar/vector PV identities, Gram relations and canonical integral order.

   Inputs:
   Symbolic D-dimensional tensors, scalar masters and their collected coefficients.

   Dependencies:
   FeynCalc for tensor/PV expressions; Internal/ supplies progress and persistence.
*)


(* Gram identities *)
(* Parameter-space recurrences; no inverse Gram matrix and no D -> 4.
   A normalized moment has prefactor
   (-1)^(N+j+Total[counts]) Gamma[N-j-D/2]/2^j.
   counts includes parameter zero; for j>=0, 2j is the number of metric
   indices. Auxiliary j<0 moments are eliminated by dimension recurrences
   before emission; they are never represented as negative PaVe indices. *)
qqPVGramReduce[input_,regular_:False] := Module[{parsed=qqPVParse[input],n,ind,mat,edges,counts,j,
 memo=<||>,directions=<||>,steps=0,rec,emit,make,findDirection,result,stop=Unique["RGZWorkflow`Private`pvGramStop"]},
 If[parsed===$Failed,Return[$Failed]];n=Length[parsed[[3]]];ind=parsed[[1]];
 If[ind==={0},ind={}];
 If[!MemberQ[{2,3,4,5,6},n]||Length[parsed[[2]]]=!=n(n-1)/2||
   !AllTrue[ind,IntegerQ[#]&&0<=#<n&]||OddQ[Count[ind,0]],Return[$Failed]];
 edges=qqPVEdges[n];mat=ConstantArray[0,{n,n}];
 Do[mat[[edges[[k,1]]+1,edges[[k,2]]+1]]=parsed[[2,k]];
    mat[[edges[[k,2]]+1,edges[[k,1]]+1]]=parsed[[2,k]],{k,Length[edges]}];
 counts=Prepend[Table[Count[ind,k],{k,1,n-1}],0];j=Count[ind,0]/2;
 make[dist_,mass_,jj_,cc_] := Module[{nn=Length[mass],ii},
  ii=Join[ConstantArray[0,2jj],Flatten[Table[ConstantArray[k,cc[[k+1]]],{k,1,nn-1}]]];
  If[ii==={},ii={0}];
  FeynCalc`PaVe[Sequence@@ii,(dist[[#[[1]]+1,#[[2]]+1]]&)/@qqPVEdges[nn],mass,
   FeynCalc`PaVeAutoOrder->False,FeynCalc`PaVeAutoReduce->False]];
 (* Remove the reference-parameter moment using x_ref=1-Sum[x_other].
    The sign compensates for the change in the normalized moment rank. *)
 emit[dist_,mass_,jj_,cc_] := Module[{nn=Length[mass],order,dd,mm,c,xx,poly,rules,rank=Total[cc],p,v},
  If[nn===1,Return[(-1)^rank If[First[mass]===0,0,
    First[mass]^jj FeynCalc`A0[First[mass]]/(2^jj qqPVRising[D/2,jj])]]];
  If[jj<0,Throw[$Failed,stop]];
  order=Prepend[DeleteCases[Range[nn],First[Ordering[cc,1]]],First[Ordering[cc,1]]];
  dd=dist[[order,order]];mm=mass[[order]];c=cc[[order]];
  If[First[c]===0,p=make[dd,mm,jj,c];
   v=qqPVMasslessSingleScale[p];If[v===$Failed,v=qqPVZeroInvariant[p]];
   Return[If[v===$Failed,p,v]]];
  xx=Table[Unique["RGZWorkflow`Private`pvMoment"],{nn-1}];
  poly=Expand[(1-Total[xx])^First[c] Times@@MapThread[Power,{xx,Rest[c]}]];
  rules=CoefficientRules[poly,xx];
  Total[(Last[#] (-1)^(rank-Total[First[#]]) emit[dd,mm,jj,Prepend[First[#],0]])& /@rules]];
 findDirection[dist_,mass_] := Module[{key={dist,mass},nn=Length[mass],g,ns,c,kappa,out=None},
  If[KeyExistsQ[directions,key],Return[directions[key]]];
  g=Table[(dist[[1,a]]+dist[[1,b]]-dist[[a,b]])/2,{a,2,nn},{b,2,nn}];
  ns=NullSpace[g];
  If[ns==={}&&TrueQ[regular],out=<|"InverseGram"->Inverse[g]|>;
   AssociateTo[directions,key->out];Return[out]];
  Do[If[!And@@(TrueQ[Expand[#]===0]& /@(g . v)),Continue[]];
   kappa=Factor[Expand[v . (Rest[mass]-First[mass]-Rest[dist[[1]]])]];
   If[kappa=!=0,c=Prepend[v,-Total[v]];out={c,kappa};Break[]],{v,ns}];
  AssociateTo[directions,key->out];out];
 rec[dist_,mass_,jj_,cc_] := Module[{key=Hash[{dist,mass,jj,cc},"SHA256"],nn=Length[mass],
   pair=None,keep,newc,newm,newdist,beta,direction,c,kappa,r,known,inv,l,h,bb,base,a,v},
  If[KeyExistsQ[memo,key],Return[memo[key]]];steps++;
  If[Mod[steps,20]===0,qqProgressOperation["Degenerate PV parameter moment "<>ToString[steps]]];
  If[nn===1,r=emit[dist,mass,jj,cc];AssociateTo[memo,key->r];Return[r]];
  (* Identical vertices of Q: integrate their sharing fraction exactly.
     The beta factor raises the merged parameter power by one, so the
     reduced coefficient has N-1 points, j-1 metrics and rank R+1. *)
  If[jj>0||TrueQ[regular],Do[If[mass[[a]]===mass[[b]]&&dist[[a]]===dist[[b]],pair={a,b};Break[]],
    {a,1,nn-1},{b,a+1,nn}]];
  If[ListQ[pair],
   keep=DeleteCases[Range[nn],pair[[2]]];newc=cc;
   newc[[pair[[1]]]]=cc[[pair[[1]]]]+cc[[pair[[2]]]]+1;
   beta=Factorial[cc[[pair[[1]]]]] Factorial[cc[[pair[[2]]]]]/
     Factorial[cc[[pair[[1]]]]+cc[[pair[[2]]]]+1];
   r=-beta/2 rec[dist[[keep,keep]],mass[[keep]],jj-1,newc[[keep]]];
   AssociateTo[memo,key->r];Return[r]];
  (* A tangent null direction of the quadratic part has d_c Q=kappa.
     Simplex integration by parts lowers N on faces and lowers the total
     parameter degree in the volume term. Never divide by kappa==0. *)
  direction=If[nn>=2,findDirection[dist,mass],None];
  If[ListQ[direction],{c,kappa}=direction;
   r=Total[Table[If[cc[[a]]===0&&c[[a]]=!=0,
      keep=DeleteCases[Range[nn],a];-c[[a]] rec[dist[[keep,keep]],mass[[keep]],jj,cc[[keep]]],0],{a,nn}]];
   r+=2 Total[Table[If[cc[[a]]>0&&c[[a]]=!=0,
      c[[a]] cc[[a]] rec[dist,mass,jj+1,cc-UnitVector[nn,a]],0],{a,nn}]];
   r=r/kappa;AssociateTo[memo,key->r];Return[r]];
  (* Lower-point faces can have a nonsingular Gram matrix even when the
     input has a singular one. Only invert a matrix already proved to have
     an empty null space. Tangent IBP then lowers momentum rank; the Euler
     relation lowers the remaining metric rank without expanding in D-4. *)
  If[AssociationQ[direction],
   inv=direction["InverseGram"];l=Rest[mass]-First[mass]-Rest[dist[[1]]];
   If[Total[Rest[cc]]===0&&First[cc]>0,
    base=cc-UnitVector[nn,1];
    r=-rec[dist,mass,jj,base]-Total[Table[rec[dist,mass,jj,base+UnitVector[nn,b]],{b,2,nn}]];
    AssociateTo[memo,key->r];Return[r]];
   If[Total[Rest[cc]]>0,
    a=First[FirstPosition[Rest[cc],_?Positive]];base=cc-UnitVector[nn,a+1];
    v=inv[[All,a]]/2;c=Prepend[v,-Total[v]];kappa=v . l;
    r=kappa rec[dist,mass,jj,base];
    r+=Total[Table[If[base[[b]]===0&&c[[b]]=!=0,keep=DeleteCases[Range[nn],b];
       c[[b]] rec[dist[[keep,keep]],mass[[keep]],jj,base[[keep]]],0],{b,nn}]];
    r-=2 Total[Table[If[base[[b]]>0&&c[[b]]=!=0,
       c[[b]] base[[b]] rec[dist,mass,jj+1,base-UnitVector[nn,b]],0],{b,nn}]];
    AssociateTo[memo,key->r];Return[r]];
   If[Total[cc]===0,
    v=inv . l/4;h=Factor[Together[First[mass]-l . v]];bb=Prepend[-v,1/2+Total[v]];
    If[jj>0||h===0,
    r=If[h===0,0,h rec[dist,mass,jj-1,cc]];
    r+=Total[Table[If[bb[[b]]=!=0,keep=DeleteCases[Range[nn],b];
       bb[[b]] rec[dist[[keep,keep]],mass[[keep]],jj-1,cc[[keep]]],0],{b,nn}]];
    r=r/(D+2jj-nn-1);AssociateTo[memo,key->r];Return[r]];
    If[jj<0,
     r=(D+2jj+1-nn) rec[dist,mass,jj+1,cc];
     r-=Total[Table[If[bb[[b]]=!=0,keep=DeleteCases[Range[nn],b];
        bb[[b]] rec[dist[[keep,keep]],mass[[keep]],jj,cc[[keep]]],0],{b,nn}]];
     r=r/h;AssociateTo[memo,key->r];Return[r]]]];
  r=emit[dist,mass,jj,cc];AssociateTo[memo,key->r];r];
 result=Catch[rec[mat,parsed[[3]],j,counts],stop];
 If[result===input,$Failed,result]];

(* Scalar master identities *)
(* Exact generic-D Feynman-parameter identities. No epsilon expansion.
   When the nonzero invariant edges form a complete bipartite graph,
   the massless parameter polynomial is -s (Sum_A x) (Sum_B x).
   Dirichlet integration relates every tensor coefficient to B0[s,0,0]. *)
qqPVEdges[2]={{0,1}};
qqPVEdges[3]={{0,1},{1,2},{0,2}};
qqPVEdges[4]={{0,1},{1,2},{2,3},{0,3},{0,2},{1,3}};
qqPVEdges[5]={{0,1},{1,2},{2,3},{3,4},{0,4},{0,2},{1,3},{2,4},{0,3},{1,4}};
(* Six-denominator Denner order: cyclic gaps 1, 2, then the three
   opposite chords. Repeated poles may reach N=6 in a one-loop box. *)
qqPVEdges[6]={{0,1},{1,2},{2,3},{3,4},{4,5},{0,5},
 {0,2},{1,3},{2,4},{3,5},{0,4},{1,5},{0,3},{1,4},{2,5}};
qqPVRising[z_,n_Integer]:=If[n>=0,Product[z+k,{k,0,n-1}],1/Product[z-k,{k,1,-n}]];
qqPVParse[m_FeynCalc`PaVe] := Module[{a=List@@m,pos},
 pos=FirstPosition[a,_List];If[MissingQ[pos],Return[$Failed]];
 pos=First[pos];If[pos+1>Length[a]||!ListQ[a[[pos+1]]],Return[$Failed]];
 {Take[a,pos-1],a[[pos]],a[[pos+1]]}];
qqPVParse[FeynCalc`B0[s_,m1_,m2_]]:={{0},{s},{m1,m2}};
qqPVParse[FeynCalc`B1[s_,m1_,m2_]]:={{1},{s},{m1,m2}};
qqPVParse[FeynCalc`B00[s_,m1_,m2_]]:={{0,0},{s},{m1,m2}};
qqPVParse[FeynCalc`B11[s_,m1_,m2_]]:={{1,1},{s},{m1,m2}};
qqPVParse[FeynCalc`C0[a_,b_,c_,m1_,m2_,m3_]]:={{0},{a,b,c},{m1,m2,m3}};
qqPVParse[FeynCalc`D0[a_,b_,c_,d_,e_,f_,m1_,m2_,m3_,m4_]]:={{0},{a,b,c,d,e,f},{m1,m2,m3,m4}};
qqPVParse[_]:=$Failed;
(* Lightlike two-point functions. Fully contracted tensor traces reduce
   metric-only coefficients to tadpoles. One-mass momentum moments follow
   from a one-variable beta integral, keeping its full D dependence. *)
qqPVZeroInvariant[m_] := Module[{v=qqPVParse[m],ind,j,r,a,b,den,ta,tb,mass,ratio},
 If[v===$Failed||v[[2]]=!={0}||Length[v[[3]]]=!=2,Return[$Failed]];
 ind=v[[1]];If[ind==={0},ind={}];
 If[!AllTrue[ind,MemberQ[{0,1},#]&]||OddQ[Count[ind,0]],Return[$Failed]];
 j=Count[ind,0]/2;r=Count[ind,1];{a,b}=v[[3]];
 If[a===0&&b===0,Return[0]];
 den=Product[D+2k,{k,0,j-1}];
 ta=If[a===0,0,FeynCalc`A0[a]];tb=If[b===0,0,FeynCalc`A0[b]];
 If[r===0,Return[If[a===b,(j+D/2-1) a^(j-1) ta/den,
   (If[a===0,0,a^j ta]-If[b===0,0,b^j tb])/((a-b) den)]]];
 If[a=!=0&&b=!=0,Return[$Failed]];
 mass=If[a===0,b,a];
 ratio=(-1)^(j+r)/2^j mass^j qqPVRising[2-D/2,-j] (D/2-1);
 ratio*=If[a===0,1/(D/2+j+r-1),Factorial[r]/qqPVRising[D/2+j-1,r+1]];
 Cancel[ratio/mass] FeynCalc`A0[mass]];
(* Relabel Feynman parameters while keeping the zero chord fixed. Tensor
   indices label parameter moments; metric index 0 is not a chord label.
   Keeping chord 0 avoids shifts of the loop momentum and tensor mixing. *)
qqPVCanonical[m_] := Module[{v=qqPVParse[m],ind,inv,mass,n,edges,mat,perms,candidates,best},
 If[v===$Failed,Return[m]];{ind,inv,mass}=v;n=Length[mass];
 If[ind==={0}&&n===1&&inv==={},Return[FeynCalc`A0[First[mass]]]];
 If[ind==={0}&&n===2&&Length[inv]===1,Return[FeynCalc`B0[First[inv],Sequence@@mass]]];
 If[!MemberQ[{3,4,5},n]||Length[inv]=!=n(n-1)/2||
   !AllTrue[ind,IntegerQ[#]&&0<=#<n&],Return[m]];
 edges=qqPVEdges[n];mat=ConstantArray[0,{n,n}];
 Do[mat[[edges[[k,1]]+1,edges[[k,2]]+1]]=inv[[k]];
    mat[[edges[[k,2]]+1,edges[[k,1]]+1]]=inv[[k]],{k,Length[edges]}];
 perms=Prepend[#,0]& /@Permutations[Range[n-1]];
 candidates=Table[{Sort[ind/.Thread[Range[n-1]->Rest[Ordering[per]]-1]],
   (mat[[per[[#[[1]]+1]]+1,per[[#[[2]]+1]]+1]]&)/@edges,mass[[per+1]]},{per,perms}];
 best=First[Sort[candidates]];
 If[ind==={0}&&n===3,Return[FeynCalc`C0[Sequence@@Join[best[[2]],best[[3]]]]]];
 If[ind==={0}&&n===4,Return[FeynCalc`D0[Sequence@@Join[best[[2]],best[[3]]]]]];
 FeynCalc`PaVe[Sequence@@best[[1]],best[[2]],best[[3]],
   FeynCalc`PaVeAutoOrder->False,FeynCalc`PaVeAutoReduce->False]];

(* Hide the large spectral-root expressions during native reduction. Each
   distinct nonzero kinematic value is replaced as a whole; zero/equality
   patterns are preserved exactly. The resulting identity is then restored. *)
qqPVNativeReduce[m_] := Module[{v=qqPVParse[m],values,slots,shape,r},
 If[v===$Failed||Length[v[[3]]]>4,Return[m]];
 (* The parameter recurrences already return ordinary scalar masters.
    Re-running native reduction on every repeated scalar leaf is costly
    and cannot lower its tensor rank further. Ordering happens outside. *)
 If[v[[1]]==={0},Return[m]];
 values=DeleteDuplicates[DeleteCases[Join[v[[2]],v[[3]]],0]];
 slots=Table[Symbol["RGZWorkflow`PVSlots`a"<>ToString[i]],{i,Length[values]}];
 shape=FeynCalc`PaVe[Sequence@@v[[1]],v[[2]]/.Thread[values->slots],v[[3]]/.Thread[values->slots],
   FeynCalc`PaVeAutoOrder->False,FeynCalc`PaVeAutoReduce->False];
 r=Block[{FeynCalc`$LimitTo4=False},FeynCalc`PaVeReduce[shape,
   FeynCalc`Dimension->D,FeynCalc`BReduce->True,FeynCalc`PaVeAutoReduce->True]];
 r/.Thread[slots->values]];
(* Transform each distinct integral once. Running PaVeOrder or enumerating
   permutations on every repeated leaf of a high-rank result wastes time
   and can consume the whole integral's limit after rank reduction succeeds. *)
qqPVOrderExpression[expr_,native_] := Module[{ints,rules,r},
 ints=DeleteDuplicates[Cases[{expr},p:(_FeynCalc`PaVe|_FeynCalc`A0|_FeynCalc`A00|
   _FeynCalc`B0|_FeynCalc`B1|_FeynCalc`B00|_FeynCalc`B11|_FeynCalc`C0|_FeynCalc`D0|_FeynCalc`E0):>p,Infinity]];
 rules=Table[r=If[TrueQ[native],qqPVNativeReduce[p],p];
   r=FeynCalc`PaVeOrder[r,FeynCalc`Collecting->False];
   p->(r/.v:(_FeynCalc`PaVe|_FeynCalc`C0|_FeynCalc`D0):>qqPVCanonical[v]),{p,ints}];
 expr/.Dispatch[rules]];
qqPVMasslessSingleScale[m_] := Module[{v,ind,inv,mass,n,j,r,scale,active,vertices,
 a,b,partition=None,counts,ua,ub,ratio},
 v=qqPVParse[m];If[v===$Failed,Return[$Failed]];
 {ind,inv,mass}=v;If[ind==={0},ind={}];n=Length[mass];
 If[!MemberQ[{2,3,4,5,6},n]||Length[inv]=!=n(n-1)/2||
  !And@@(TrueQ[#===0]& /@mass)||!AllTrue[ind,IntegerQ[#]&&0<=#<n&],Return[$Failed]];
 scale=DeleteDuplicates[DeleteCases[inv,0]];
 If[Length[scale]=!=1,Return[$Failed]];scale=First[scale];
 active=Pick[qqPVEdges[n],(#=!=0&/@inv)];vertices=Union[Flatten[active]];
 Do[a=sub;b=Complement[vertices,a];
  If[Sort[Sort/@Tuples[{a,b}]]===Sort[active],partition={a,b};Break[]],
  {sub,Subsets[vertices,{1,Length[vertices]-1}]}];
 If[partition===None||OddQ[Count[ind,0]],Return[$Failed]];
 j=Count[ind,0]/2;r=Length[ind]-2j;
 counts=Table[Count[ind,k],{k,0,n-1}];counts[[1]]=0;
 ua=Length[partition[[1]]]+Total[counts[[partition[[1]]+1]]];
 ub=Length[partition[[2]]]+Total[counts[[partition[[2]]+1]]];
 ratio=(-1)^(n+j+r)/2^j (-scale)^(2+j-n)
  qqPVRising[2-D/2,n-j-2]/qqPVRising[D-2,r+2j-n+2]
  qqPVRising[D/2-1,ua+j+1-n] qqPVRising[D/2-1,ub+j+1-n]
  Times@@(Factorial/@counts)/(Factorial[ua-1] Factorial[ub-1]);
 Cancel[ratio] FeynCalc`B0[scale,0,0]];
(* Collect the scalar masters before reducing small rational kinematic
   coefficients; do not distribute this identity into the full amplitude. *)
qqPVCompactCoefficients[expr_] := Module[{pairs=qqFFCollectMasters[expr,False,1]},
 If[FailureQ[pairs],Return[expr]];
 Total[(First[#] Cancel[Together[Last[#]]]& /@pairs)]];

(* Vector coefficients *)
(* Apply integral identities to a coefficient vector, not by substituting
   expanded identities throughout the amplitude. Keep scalar products factored. *)
qqFFVectorVersion=Hash[{"QQPVVector/1",FileHash[$InputFileName,"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","Algebra/ColorSpinBasis.wl"}],"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","Algebra/PassarinoVeltman.wl"}],"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","Algebra/PassarinoVeltman.wl"}],"SHA256"]},"SHA256"];
qqFFVectorContext[] := Hash[{qqFFVectorVersion,$Version,FeynCalc`$FeynCalcVersion,
 DownValues[FeynCalc`PaVeReduce],DownValues[FeynCalc`PaVeOrder],
 Options[FeynCalc`PaVeReduce],Options[FeynCalc`PaVeOrder]},"SHA256"];
qqFFVectorPairsQ[v_] := ListQ[v]&&AllTrue[v,MatchQ[#,{_,_}]&]&&!qqFFInvalidQ[v];
qqFFIdentityVector[master_,reduce_,seconds_,context_] := Module[{key,file,p,r,v},
 If[master===1,Return[{{1,1}}]];
 key=Hash[{context,master,reduce},"SHA256"];
 file=FileNameJoin[{cacheDirectory[],"qq-pv-vectors",IntegerString[key,16,64]<>".wxf"}];
 If[FileExistsQ[file],qqProgressOperation["Read PV coefficient-vector checkpoint"];
  p=Quiet[Check[Import[file,"WXF"],$Failed]];
  If[AssociationQ[p]&&Lookup[p,"Key",None]===key&&
    qqFFVectorPairsQ[Lookup[p,"Pairs",None]],
   qqProgressUpdate[<|"CacheState"->"Reused","ReusedCheckpoints"->(Lookup[$qqProgress,"ReusedCheckpoints",0]+1)|>];Return[p["Pairs"]]]];
 r=qqFFPVNormalize[master,reduce,seconds];
 If[qqFFInvalidQ[r],Return[Failure["QQPVVectorIncomplete",<|"Master"->master|>]]];
 qqProgressOperation["Collect this PV identity into scalar coefficients"];
 v=qqFFCollectMasters[r,False,seconds];If[FailureQ[v],Return[v]];
 If[!qqFFVectorPairsQ[v],Return[Failure["QQPVVectorIncomplete",<|"Master"->master|>]]];
 (* A timed-out normalization retains its input; it must remain retryable. *)
 If[KeyExistsQ[qqFFPVCache,{master,reduce}],
  If[!DirectoryQ[DirectoryName[file]],CreateDirectory[DirectoryName[file],CreateIntermediateDirectories->True]];
  qqProgressOperation["Save PV coefficient-vector checkpoint"];
  p=atomicWXF[file,<|"Key"->key,"Pairs"->v|>];If[FailureQ[p],Return[p]];
  qqProgressUpdate[<|"CacheState"->"Saved","ComputedCheckpoints"->(Lookup[$qqProgress,"ComputedCheckpoints",0]+1)|>]];v];
qqFFReduceMasterPairs[pairs_,reduce_,simplify_,seconds_,record_:False] := Module[{acc=<||>,identities={},context=qqFFVectorContext[],
 v,co,key,keys,rows={},candidate,stop=Unique["RGZWorkflow`Private`pvVectorStop"]},Catch[
 qqProgressUpdate[<|"PVIndex"->0,"PVTotal"->Length[pairs],"ScalarMasterIndex"->0,"ScalarMasterTotal"->0|>];
 Do[qqProgressOperation["Apply PV coefficient vector "<>ToString[i]<>" / "<>ToString[Length[pairs]]];
  qqProgressUpdate[<|"Master"->pairs[[i,1]],"PVIndex"->i,"PVTotal"->Length[pairs],"ItemStarted"->AbsoluteTime[],"InputLeaves"->LeafCount[pairs[[i,1]]]|>];
  v=qqFFIdentityVector[pairs[[i,1]],reduce,seconds,context];If[FailureQ[v],Throw[v,stop]];
  If[TrueQ[record],AppendTo[identities,{pairs[[i,1]],v}]];
  qqProgressUpdate[<|"OutputLeaves"->LeafCount[v]|>];
  co=pairs[[i,2]];
  Do[key=With[{m=entry[[1]]},HoldComplete[m]];
   AssociateTo[acc,key->Append[Lookup[acc,key,{}],co entry[[2]]]],{entry,v}],{i,Length[pairs]}];
 keys=Keys[acc];
 Do[qqProgressOperation["Merge scalar master coefficient "<>ToString[i]<>" / "<>ToString[Length[keys]]];
  key=keys[[i]];qqProgressUpdate[<|"ScalarMasterIndex"->i,"ScalarMasterTotal"->Length[keys],"Master"->ReleaseHold[key]|>];co=Total[acc[key]];
  If[TrueQ[simplify],candidate=TimeConstrained[qqFFRationalReduce[co],seconds,$TimedOut];
   If[!qqFFInvalidQ[candidate]&&LeafCount[candidate]<=LeafCount[co],co=candidate]];
  If[co=!=0,AppendTo[rows,{ReleaseHold[key],co}]],{i,Length[keys]}];If[TrueQ[record],<|"Pairs"->rows,"Identities"->identities|>,rows],stop]];

(* Scalar relabelling *)
(* Scalar loop integrals are invariant under every propagator relabelling,
   including moving the zero chord. Tensor coefficients must not use this
   operation: a zero-chord shift mixes their tensor structures. *)
qqPVScalarCanonical[m_] := Module[{v=qqPVParse[m],ind,inv,mass,n,edges,mat,perms,candidates,best},
 If[v===$Failed,Return[m]];{ind,inv,mass}=v;n=Length[mass];
 If[ind=!={0},Return[m]];
 If[n===1&&inv==={},Return[FeynCalc`A0[First[mass]]]];
 If[n===2&&Length[inv]===1,Return[FeynCalc`B0[First[inv],Sequence@@Sort[mass]]]];
 If[!MemberQ[{3,4,5},n]||Length[inv]=!=n(n-1)/2,Return[m]];
 edges=qqPVEdges[n];mat=ConstantArray[0,{n,n}];
 Do[mat[[edges[[k,1]]+1,edges[[k,2]]+1]]=inv[[k]];
  mat[[edges[[k,2]]+1,edges[[k,1]]+1]]=inv[[k]],{k,Length[edges]}];
 perms=Select[Permutations[Range[n]],mass[[#]]===Sort[mass]&];
 candidates=Table[{mass[[per]],(mat[[per[[#[[1]]+1]],per[[#[[2]]+1]]]]& /@ edges)},{per,perms}];
 best=First[Sort[candidates]];
 Switch[n,3,FeynCalc`C0[Sequence@@Join[best[[2]],best[[1]]]],
 4,FeynCalc`D0[Sequence@@Join[best[[2]],best[[1]]]],
 5,FeynCalc`PaVe[0,best[[2]],best[[1]],FeynCalc`PaVeAutoOrder->False,FeynCalc`PaVeAutoReduce->False]]];
