(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
Begin["RGZWorkflow`IdentityAlgebra`"];
projectPath=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
checks=<||>; record[n_,v_]:=AssociateTo[checks,n->TrueQ[v]];
go[x_]:={{{},x}}; og[i_]:={{{i},1}};
gsum[x_List]:=Select[({#[[1,1]],Expand[Total[#[[All,2]]]]}& /@ GatherBy[x,First]),Last[#]=!=0&];
ga[x__List]:=gsum[Join[x]];
gscale[a_,x_List]:=gsum[({#[[1]],Expand[a #[[2]]]}& /@ x)];
gp[x_List,y_List]:=Module[{terms={},l,r,key,sign,leftTerm,rightTerm},Do[l=leftTerm[[1]];r=rightTerm[[1]];key=Join[l,r];If[DuplicateFreeQ[key],sign=(-1)^Total[Flatten[Table[Boole[i>j],{i,l},{j,r}]]];AppendTo[terms,{Sort[key],sign leftTerm[[2]]rightTerm[[2]]}]],{leftTerm,x},{rightTerm,y}];gsum[terms]];
ncDim=8;
matrices={{{0,1,0},{1,0,0},{0,0,0}},{{0,-I,0},{I,0,0},{0,0,0}},{{1,0,0},{0,-1,0},{0,0,0}},{{0,0,1},{0,0,0},{1,0,0}},{{0,0,-I},{0,0,0},{I,0,0}},{{0,0,0},{0,0,1},{0,1,0}},{{0,0,0},{0,0,-I},{0,I,0}},{{1,0,0},{0,1,0},{0,0,-2}}/Sqrt[3]}/2;
structure=Table[Simplify[-2 I Tr[(matrices[[i]].matrices[[j]]-matrices[[j]].matrices[[i]]).matrices[[k]]]],{i,ncDim},{j,ncDim},{k,ncDim}];
cross[x_List,y_List]:=Table[ga@@Flatten[Table[gscale[structure[[i,j,k]],gp[x[[j]],y[[k]]]],{j,ncDim},{k,ncDim}],1],{i,ncDim}];
vsum[x__List]:=Map[ga@@#&,Transpose[{x}]];
vscale[a_,x_List]:=gscale[a,#]& /@ x;
av=Table[Symbol["a"<>ToString[i]],{i,ncDim}];xv=Table[Symbol["xi"<>ToString[i]],{i,ncDim}];dxv=Table[Symbol["dxi"<>ToString[i]],{i,ncDim}];
A=go/@av;Xi=go/@xv;DXi=go/@dxv;ghostVector=og/@Range[ncDim];DC=og/@Range[ncDim+1,2ncDim];
sA=vsum[vscale[-1,DC],vscale[-g,cross[A,ghostVector]]];
sXi=vsum[vscale[-1,ghostVector],vscale[g/2,cross[ghostVector,Xi]],vscale[-g^2/12,cross[Xi,cross[Xi,ghostVector]]]];
sDXi=vsum[vscale[-1,DC],vscale[g/2,vsum[cross[DC,Xi],cross[ghostVector,DXi]]],vscale[-g^2/12,vsum[cross[DXi,cross[Xi,ghostVector]],cross[Xi,cross[DXi,ghostVector]],cross[Xi,cross[Xi,DC]]]]];
SC=vscale[g/2,cross[ghostVector,ghostVector]];
SDC=vscale[g/2,vsum[cross[DC,ghostVector],cross[ghostVector,DC]]];
evenVars=Join[av,xv,dxv,{phi,phibar,beta,betabar,F}];
evenS=Join[sA,sXi,sDXi,{og[2ncDim+2],{},og[2ncDim+2],{},og[2ncDim+6]}];
oddS=Join[SC,SDC,{go[-r4],{},go[phibar+betabar],{},{},{}}];
sG[expr_List]:=Module[{out={},key,coef,part},Do[key=term[[1]];coef=term[[2]];
 Do[part=D[coef,evenVars[[i]]];If[part=!=0,out=Join[out,gscale[part,gp[evenS[[i]],{{key,1}}]]]],{i,Length[evenVars]}];
 Do[out=Join[out,gscale[(-1)^(j-1)coef,gp[gp[{{Take[key,j-1],1}},oddS[[key[[j]]]]],{{Drop[key,j],1}}]]],{j,Length[key]}],{term,expr}];gsum[out]];
zeroG[x_List]:=AllTrue[x,Expand[Last[#]]===0&];
zeroOrder[x_List,max_]:=And@@Flatten[Table[Coefficient[Expand[term[[2]]],g,i]===0,{term,x},{i,0,max}]];
Ah=vsum[A,vscale[-1,DXi],vscale[-g,cross[A,Xi]],vscale[-g/2,cross[Xi,DXi]],vscale[g^2/2,cross[Xi,cross[Xi,A]]],vscale[-g^2/6,cross[Xi,cross[Xi,DXi]]]];

modelSource=Import[RGZWorkflow`Private`modelBase[]<>".fr","Text"];
modelStart=First[First[StringPosition[modelSource,"Ah[\\[Mu]_, a_] := Module"]]];
modelEnd=First[First[StringPosition[modelSource,"(* Pure Yang-Mills *)"]]]-1;
Block[{$Context="RGZWorkflow`IdentityModel`",$ContextPath={"System`"}},ToExpression[StringTake[modelSource,{modelStart,modelEnd}],InputForm]];
RGZWorkflow`IdentityModel`gs=g;
RGZWorkflow`IdentityModel`A[_,j_Integer]:=av[[j]];
RGZWorkflow`IdentityModel`\[Xi][j_Integer]:=xv[[j]];
RGZWorkflow`IdentityModel`del[v_,_] /; MemberQ[xv,v] := dxv[[First[First[Position[xv,v]]]]];
RGZWorkflow`IdentityModel`f[i_Integer,j_Integer,k_Integer]:=structure[[i,j,k]];
sumTerm[term_]:=Module[{indices,iterator},indices=DeleteDuplicates[Flatten[Cases[term,RGZWorkflow`IdentityModel`f[idx__]:>Select[{idx},!IntegerQ[#]&],Infinity]]];Fold[Function[{acc,ind},Total[Table[acc/.ind->iterator,{iterator,ncDim}]]],term,indices]];
sumEinstein[expr_]:=Total[sumTerm /@ If[Head[expr]===Plus,List@@expr,{expr}]];
modelAh=Table[Expand[sumEinstein[Expand[RGZWorkflow`IdentityModel`Ah[lor,i]]]],{i,ncDim}];
independentAh=Total[#[[All,2]]]& /@ Ah;
record["ModelAhEqualsIndependentBCHThroughG2",Expand[modelAh-independentAh]===ConstantArray[0,ncDim]];
xiScaling=Thread[Join[xv,dxv]->xiDegree Join[xv,dxv]];
aScaling=Thread[av->aDegree av];
aaXiXi=Coefficient[Coefficient[Expand[Total[modelAh^2]/.aScaling/.xiScaling],aDegree,2],xiDegree,2];
record["CondensateAAXiXiCancels",Expand[aaXiXi]===0];
record["SU3NilpotentGhost",And@@(zeroG[sG[sG[#]]]& /@ ghostVector)];
record["SU3NilpotentGaugeField",And@@(zeroG[sG[sG[#]]]& /@ A)];
record["SU3NilpotentXiThroughG2",And@@(zeroOrder[sG[sG[#]],2]& /@ Xi)];
record["SU3AhInvariantThroughG2",And@@(zeroOrder[sG[#],2]& /@ Ah)];
norm=ga@@(gp[#,#]& /@ Xi);
xic=ga@@MapThread[gp,{Xi,ghostVector}];
record["XiNormVariation",zeroG[ga[sG[norm],gscale[2,xic]]]];
regulator=ga[gscale[-r4/2,norm],gp[og[2ncDim+1],xic]];
record["MinkowskiIRRegulatorInvariant",zeroG[sG[regulator]]];
record["IRRegulatorIsExact",zeroG[ga[sG[gscale[1/2,gp[og[2ncDim+1],norm]]],gscale[-1,regulator]]]];
auxBoson=go[phibar phi+betabar beta];auxGhost=ga[gp[og[2ncDim+3],og[2ncDim+2]],gp[og[2ncDim+5],og[2ncDim+4]]];
aux=ga[gscale[kernel,auxBoson],gscale[-kernel,auxGhost]];
condensate=ga[gscale[-mu2,auxBoson],gscale[mu2,auxGhost]];
horizon=go[horizonCoefficient invariantAh (phi+phibar-beta-betabar)];
record["AuxiliaryKineticInvariant",zeroG[sG[aux]]];
record["AuxiliaryCondensateInvariant",zeroG[sG[condensate]]];
record["HorizonInvariant",zeroG[sG[horizon]]];
AppendTo[oddS,go[-b]];
gf=ga[go[b F+alpha b^2/2],gp[og[2ncDim+7],og[2ncDim+6]]];
record["GaugeFixingWithSbarcMinusB",zeroG[sG[gf]]];
(* rho=0 at fixed M is not a BRST-stable source slice. *)
fixedRegulator=gscale[-r4/2,norm];
record["FiniteMSourceFreeBreaking",zeroG[ga[sG[fixedRegulator],gscale[-r4,xic]]]];


End[];
RGZWorkflow`IdentityAlgebra`checks
