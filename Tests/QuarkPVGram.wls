(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"qq-pv-gram-"<>CreateUUID[]}],"Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
red=RGZWorkflow`Private`qqPVGramReduce;
mk[ii_,iv_,mm_]:=FeynCalc`PaVe[Sequence@@ii,iv,mm,FeynCalc`PaVeAutoOrder->False,FeynCalc`PaVeAutoReduce->False];
canon[x_]:=x/.p_FeynCalc`PaVe:>RGZWorkflow`Private`qqPVCanonical[p];
m=mk[{0},{0,s,s},{a,b,c}];r=canon[red[m]];
record["UnequalMassScalarPartialFraction",Cancel[Together[r-(FeynCalc`B0[s,a,c]-FeynCalc`B0[s,b,c])/(a-b)]]===0];
Do[ii=Join[ConstantArray[0,2jj],ConstantArray[1,rr]];If[ii==={},ii={0}];
 Do[iv=If[nn===3,{0,s,s},{0,0,0,s,0,s}];mm=ConstantArray[0,nn];m=mk[ii,iv,mm];
 r=TimeConstrained[red[m],10,$TimedOut];known=RGZWorkflow`Private`qqPVMasslessSingleScale[m];
 record["IndependentDirichletN"<>ToString[nn]<>"J"<>ToString[jj]<>"R"<>ToString[rr],r=!=$TimedOut&&Cancel[Together[r-known]]===0],{nn,3,4}],{jj,1,3},{rr,0,3}];
Do[ii=If[rr===0,{0},ConstantArray[1,rr]];
 iv=Switch[nn,3,{0,s,s},4,{0,0,0,s,0,s},5,{0,0,0,0,s,0,s,0,s,s}];
 fixture=mk[ii,iv,ConstantArray[0,nn]];rrr=TimeConstrained[red[fixture,True],10,$TimedOut];
 independent=RGZWorkflow`Private`qqPVMasslessSingleScale[fixture];
 record["PureMomentDimensionShiftN"<>ToString[nn]<>"R"<>ToString[rr],rrr=!=$Failed&&rrr=!=$TimedOut&&Cancel[Together[rrr-independent]]===0],{nn,3,5},{rr,0,3}];
(* Tangent parameter identity for a collinear geometry with no duplicate
   distance rows: x0+x1+x2=1, Gram rank 1, c={1,-2,1}. *)
xx={x0,x1,x2};dist={{0,-1,-4},{-1,0,-1},{-4,-1,0}};
q=xx . {a,b,c}-Sum[dist[[i,k]] xx[[i]] xx[[k]],{i,1,2},{k,i+1,3}];
cc={1,-2,1};kappa=a-2b+c+2;
record["NonDuplicateNullDirectionPolynomial",Expand[(cc . (D[q,#]& /@xx)-kappa)/.x0->1-x1-x2]===0];
(* Direct normalized parameter integration in a convergent Euclidean region
   checks both the boundary terms and raised-metric volume terms. *)
dim=13/10;
num[v_] := Module[{ii=v[[1]],iv=v[[2]],mass=v[[3]],n,jj,pp,edges,dd,ts,xs,jac,poly,weight,its},
 If[ii==={0},ii={}];n=Length[mass];jj=Count[ii,0]/2;pp=Table[Count[ii,k],{k,0,n-1}];pp[[1]]=0;
 If[n===1,Return[N[(-1)^(1+jj)/2^jj Gamma[1-jj-dim/2] mass[[1]]^(dim/2+jj-1),20]]];
 edges=RGZWorkflow`Private`qqPVEdges[n];ts=Table[Unique["param"],{n-1}];
 xs=Append[Table[ts[[i]] Product[1-ts[[k]],{k,1,i-1}],{i,n-1}],Times@@(1-ts)];
 jac=Product[(1-ts[[k]])^(n-1-k),{k,1,n-1}];
 poly=mass . xs-Sum[iv[[k]] xs[[edges[[k,1]]+1]] xs[[edges[[k,2]]+1]],{k,Length[edges]}];
 weight=Times@@MapThread[Power,{xs,pp}];its=({#,0,1}&)/@ts;
 N[(-1)^(n+jj+Total[pp])/2^jj Gamma[n-jj-dim/2],20] Apply[NIntegrate,
 Join[{jac weight poly^(dim/2+jj-n)},its,{WorkingPrecision->25,AccuracyGoal->9,PrecisionGoal->6,Method->{"GlobalAdaptive","SymbolicProcessing"->0}}]]];
numCached[v_] := numCache[v,dim];
numCache[v_,dd_] := numCache[v,dd]=Block[{dim=dd},num[v]];
numExpr[expr_] := Module[{z=expr/.D->dim},
 z=z/.p:(_FeynCalc`PaVe|_FeynCalc`B0|_FeynCalc`C0|_FeynCalc`D0):>numCached[RGZWorkflow`Private`qqPVParse[p]];
 z/.FeynCalc`A0[v_]:>N[-Gamma[1-dim/2] v^(dim/2-1),20]];
Do[fixture=cases[[i]];r=TimeConstrained[red[fixture,True],20,$TimedOut];
 expected=num[RGZWorkflow`Private`qqPVParse[fixture]];
 actual=If[r===$TimedOut,Infinity,numExpr[r]];
 record["EuclideanParameterCrossCheck"<>ToString[i],NumericQ[actual]&&Abs[actual-expected]<10^-7 Max[1,Abs[expected]]],
 {i,Length[cases={mk[{0,0,1},{-1,-1,-4},{2,3,5}],mk[{0,0,0,0,1,2,3},{0,-1,-2,-3,-1,-3},{2,2,3,4}],mk[{1,1,2},{0,-1,-2,-3,-1,-3},{2,5,3,4}],mk[{0,0,0,0,2,4},{0,-1,0,-2,-3,-1,-1,-2,-1,-3},{2,2,3,3,4}],mk[ConstantArray[1,9],{-1,-2,-3},{2,3,5}],mk[{1,1,2},{0,-1,-2,-3,-1,-3},{2,2,3,4}],mk[{1,1,2,4},{0,-1,0,-2,-3,-1,-1,-2,-1,-3},{2,2,3,3,4}]}]}];
Block[{dim=93/10},
 Do[fixture=lightlikeCases[[i]];rrr=TimeConstrained[red[fixture,True],20,$TimedOut];
  expected=num[RGZWorkflow`Private`qqPVParse[fixture]];
  actual=If[rrr===$TimedOut,Infinity,numExpr[rrr]];
  record["DimensionShiftedMasslessFaceCheck"<>ToString[i],rrr=!=$Failed&&NumericQ[actual]&&Abs[actual-expected]<10^-7 Max[1,Abs[expected]]],
 {i,Length[lightlikeCases={mk[{0},{0,0,-2},{0,0,3}],mk[{1,1,2},{0,0,0,-2,0,-2},{0,0,0,3}],mk[{1,1},{0,0,0,0,0,0,0,-2,-3,-2},{0,0,0,0,3}]}]}]];
Do[m=mk[ii,{s},{a,b}];regularResult=red[m,True];native=RGZWorkflow`Private`qqPVNativeReduce[m];
 difference=FeynCalc`PaVeOrder[regularResult-native]/.p_FeynCalc`PaVe:>RGZWorkflow`Private`qqPVCanonical[p];
 record["RegularBubbleNative"<>ToString[ii,InputForm],Cancel[Together[difference]]===0],{ii,{{1},{1,1},{0,0},{0,0,1},{0,0,0,0}}}];
Do[m=mk[ii,{s,t,u},{a,b,c}];regularResult=red[m,True];native=RGZWorkflow`Private`qqPVNativeReduce[m];
 difference=FeynCalc`PaVeOrder[regularResult-native]/.p_FeynCalc`PaVe:>RGZWorkflow`Private`qqPVCanonical[p];
 record["RegularTriangleNative"<>ToString[ii,InputForm],Cancel[Together[difference]]===0],{ii,{{1},{1,2},{0,0},{0,0,1}}}];
m=mk[{1},{0,s,s},{a,a,c}];r=red[m];
record["EqualMassZeroKappaNotDivided",r===$Failed||!RGZWorkflow`Private`qqFFInvalidQ[r]];
record["OddMetricIndicesRejected",red[mk[{0,1},{0,s,s},{a,b,c}]]===$Failed];
orderCalls=0;
Block[{FeynCalc`PaVeOrder},Clear[FeynCalc`PaVeOrder];
 FeynCalc`PaVeOrder[x_,___]:=(orderCalls++;x);
 integral=FeynCalc`B0[s,a,b];expr=(u+v) integral+(u-v) integral;
 ordered=RGZWorkflow`Private`qqPVOrderExpression[expr,False];
 record["RepeatedIntegralsOrderedOnlyOnce",orderCalls===1&&Expand[ordered-expr]===0]];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
