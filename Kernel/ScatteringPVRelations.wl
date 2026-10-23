(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
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
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVGram.wl"}]];
