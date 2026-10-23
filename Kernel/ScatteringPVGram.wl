(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
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
