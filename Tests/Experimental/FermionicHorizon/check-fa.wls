(* SPDX-License-Identifier: GPL-3.0-or-later *)
Begin["RGZFHFACheck`"];
rgzFHRoot=DirectoryName[$InputFileName,4];
Get[FileNameJoin[{rgzFHRoot,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"rgz-fh-fa-"<>CreateUUID[]}],"Display"->"Quiet"];
RGZLoadFeynCalc[];
RGZWorkflow`Private`engineGet["RGZ_Mixed_OneLoop_Overall.wl"];
rgzFHResult=RGZWorkflow`Private`engineEvaluation[
 Block[{$ModelPath=Append[$ModelPath,FileNameJoin[{rgzFHRoot,"Models"}]]},
  InitializeModel[FileNameJoin[{rgzFHRoot,"Models","Experimental","FermionicHorizon","RGZ_FermionicHorizon"}],
   GenericModel->FileNameJoin[{rgzFHRoot,"Models","Experimental","FermionicHorizon","RGZ_FermionicHorizon"}]];
  <|"GenericCount"->Length[M$GenericCouplings],"ClassCount"->Length[M$CouplingMatrices],
    "NoFailures"->FreeQ[M$CouplingMatrices,_Failure],
    "NewFields"->Cases[M$ClassesDescription,eq_Equal /; MemberQ[{F[3],F[4],S[4],S[5]},eq[[1]]] ]|>]];
Print["FA_CHECK ",rgzFHResult];
rgzFHGeneration=RGZWorkflow`Private`engineEvaluation[
 Module[{model=FileNameJoin[{rgzFHRoot,"Models","Experimental","FermionicHorizon","RGZ_FermionicHorizon"}],eqs,fields,n,top,ins,amp,rows={},strip},
 strip[x_]:=x/. h_[id_Integer,inds_List] /; MemberQ[{F,S,V,U},h] :> h[id];
 Print["FH_MATRIX_NAMES ",Names["*`RGZFHNewMatrices"]];
 eqs=ToExpression[First[Names["*`RGZFHNewMatrices"]]];
 Do[fields=strip /@ (List@@eq[[1]]);n=Length[fields];
  top=TopologyList[Topology[1][Sequence@@Join[
    {Propagator[Incoming][Vertex[1][1],Vertex[n][n+1]]},
    Table[Propagator[Outgoing][Vertex[1][j],Vertex[n][n+1]],{j,2,n}]]]];
  ins=InsertFields[top,{First[fields]}->((-Rest[fields])/.{-V[1]->V[1],(-S[id_Integer] /; MemberQ[{1,2,3},id]):>S[id]}),InsertionLevel->{Particles},Model->model,GenericModel->model];
  amp=Check[CreateFeynAmp[ins,Truncated->True,GaugeRules->{},PreFactor->1],$Failed,{CreateFeynAmp::nocoupl,CreateFeynAmp::noprop}];
  If[Length[amp]===0,Print["EMPTY ",fields," INS ",ins," COUPLING ",eq]];
  If[!FreeQ[amp,_Failure|$Failed|$Aborted],Print["BAD_AMPLITUDE ",fields," ",amp]];
  AppendTo[rows,<|"Fields"->fields,"Generated"->(Length[amp]>0&&FreeQ[amp,_Failure|$Failed|$Aborted]),"Leaves"->LeafCount[amp]|>],{eq,eqs}];rows]];
Print["GENERATION ",rgzFHGeneration];

rgzFHPropagators=RGZWorkflow`Private`engineEvaluation[
 Module[{model=FileNameJoin[{rgzFHRoot,"Models","Experimental","FermionicHorizon","RGZ_FermionicHorizon"}],top,ins,amp,rows={}},
 top=CreateTopologies[0,1->1,CTOrder->1,ExcludeTopologies->TadpoleCTs];
 Do[ins=InsertFields[top,{pair[[1]]}->{pair[[2]]},InsertionLevel->{Particles},Model->model,GenericModel->model];
 amp=Check[CreateFeynAmp[ins,Truncated->True,GaugeRules->{},PreFactor->1],$Failed,{CreateFeynAmp::nocoupl,CreateFeynAmp::noprop}];
 AppendTo[rows,<|"Pair"->pair,"Generated"->(Length[amp]>0&&FreeQ[amp,_Failure|$Failed|$Aborted]),"Leaves"->LeafCount[amp]|>],
 {pair,{{F[1],F[1]},{F[1],F[3]},{F[3],F[1]},{F[3],F[3]},{S[4],S[4]},
 {F[2],F[2]},{F[2],F[4]},{F[4],F[2]},{F[4],F[4]},{S[5],S[5]}}}];rows]];
Print["PROPAGATORS ",rgzFHPropagators];
rgzFHBasisChecks=RGZWorkflow`Private`engineEvaluation[
 Module[{base,added,combined,raw,embedded,keyFn,check,vec},
 base=ToExpression[First[Names["*`RGZFHBaseBasis"]]];
 added=ToExpression[First[Names["*`RGZFHAddedBasis"]]];
 combined=ToExpression[First[Names["*`RGZFHCombinedBasis"]]];
 keyFn=ToExpression[First[Names["*`RGZFHKey"]]];
 check[raw_,embedded_,basis_] := And@@Table[
 With[{key=keyFn[raw[[j,1]]]},And@@Table[
 With[{diff=Expand[((Total[raw[[j,2,All,k]] basis[key]]-Total[embedded[[j,2,All,k]] combined[key]])/.Global`FAFourVector->vec)/.vec[-v_,mu_]:>-vec[v,mu]]},If[diff=!=0,Print["BASIS_DIFF ",j," ",k," ",Short[diff,3]]];diff===0],{k,2}]],{j,Length[raw]}];
 <|"BaseTensorReconstruction"->check[ToExpression[First[Names["*`RGZFHRawBaseMatrices"]]],ToExpression[First[Names["*`RGZFHOriginalMatrices"]]],base],
 "HorizonTensorReconstruction"->check[ToExpression[First[Names["*`RGZFHRawNewMatrices"]]],ToExpression[First[Names["*`RGZFHNewMatrices"]]],added]|>]];
Print["BASIS_CHECKS ",rgzFHBasisChecks];
Exit[If[TrueQ[AssociationQ[rgzFHResult]&&TrueQ[rgzFHResult["NoFailures"]]&&ListQ[rgzFHGeneration]&&And@@Lookup[rgzFHGeneration,"Generated"]&&And@@Lookup[rgzFHPropagators,"Generated"]&&And@@Values[rgzFHBasisChecks]],0,1]];
