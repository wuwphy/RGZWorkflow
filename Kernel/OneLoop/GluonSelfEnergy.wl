(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Generate, inspect and reduce the standalone ordinary AA self-energy graphs.

   Inputs:
   The selected FeynArts model and the explicitly requested AA graph task.

   Dependencies:
   Algebra/LoopPrimitives.wl provides shared algebra; OneLoop/Topologies.wl provides
   the topology definitions. Load the common engine first, then this optional AA driver.
*)

ClearAll[RGZLoopConvert,RGZRunAA,RGZShowLoopGraph,RGZReduceAllAA];

RGZLoopConvert[diagrams_] := Module[{fa,fc,bad},
  If[Length[diagrams] === 0,Return[{}]];
  fa = CreateFeynAmp[diagrams,Truncated->True,GaugeRules->{},PreFactor->1];
  fa = RGZLoopPrepareFA[fa];
  fc = FCFAConvert[fa,IncomingMomenta->{p},OutgoingMomenta->{p},
    LoopMomenta->{q},UndoChiralSplittings->True,
    LorentzIndexNames->{\[Mu],\[Nu]},ChangeDimension->D,
    List->True,DropSumOver->True];
  If[!ListQ[fc],Return[Failure["ConversionFailed",<|"Output"->fc|>]]];
  bad=RGZLoopUnresolved[fc];
  If[bad =!= {},Return[Failure["UnresolvedCoupling",<|"Objects"->bad|>]]];
  If[!FreeQ[fc, a_ /; MemberQ[{"mG1","mG2","mG3","mA1","mA2","mB"},
      RGZLoopName[a]]],
    Print["The model contains legacy placeholder denominators. Use corrected gen/mod files that pass propagator checks; unchecked legacy substitution rules are not applied."]];
  fc=RGZLoopReduce /@ fc;
  bad=Select[fc,FailureQ];
  If[bad =!= {},First[bad],fc]
];

RGZRunAA[] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[{tops,inserted,amps,rows={},data,sector,a,i},
  If[!StringQ[modelRGZ],
    Return[Failure["SetModel",<|"Message"->"Set modelRGZ to the shared .gen/.mod path without an extension."|>]]];
  tops = <|"FourPointVertexLoop"->rgzLoopTopTadpole,
    "BosonicThreePointLoop"->rgzLoopTopBosonBubble,
    "GhostThreePointLoop"->rgzLoopTopGhostBubble|>;
  inserted = Association[]; amps = Association[];
  Do[
    Print["Generating: ",sector];
    data = InsertFields[tops[sector],{V[1]}->{V[1]},
      InsertionLevel->{Particles},Model->modelRGZ,GenericModel->modelRGZ];
    data = If[sector == "GhostThreePointLoop",
      DiagramSelect[data,With[{fields=LoopFields[##]},
        fields =!= {} && And@@(MatchQ[#,_U|-_U]& /@ fields)]&],
      DiagramSelect[data,FreeQ[LoopFields[##],_U]&]];
    AssociateTo[inserted,sector->data];
    a = RGZLoopConvert[data];
    If[FailureQ[a],Throw[a,rgzFunctionReturnTag]];
    AssociateTo[amps,sector->a];
    Do[AppendTo[rows,<|"Category"->sector,"WithinClassID"->i,
      "Amplitude"->a[[i]]|>],{i,Length[a]}];
    Print[sector,"：",Length[a]," TermAmplitude"], {sector,Keys[tops]}];
  If[rows === {},Return[Failure["NoGraphs",<||>]]];
  Print["for All ",Length[rows]," TermSum; Notexclude  φ, β and its conjugate Field. "];
  <|"InsertionDiagram"->inserted,"ClassifiedAmplitude"->amps,"PerDiagramResults"->rows,
    "AmputatedAATotalAmplitude"->Total[Lookup[rows,"Amplitude"]],
    "Description"->"Ordinary AA 1PI insertion; full mixed-propagator corrections and a proven PT self-energy are outside this result."|>
],rgzFunctionReturnTag]];

RGZShowLoopGraph[result_Association,n_Integer] := Module[{rows,row},
  rows = result["PerDiagramResults"];
  If[n<1 || n>Length[rows],Return[$Failed]];
  row=rows[[n]];
  Print["number  ",n," Term: ",row["Category"],", WithinClassID ",row["WithinClassID"]];
  Print[TraditionalForm[row["Amplitude"]]]
];

RGZReduceAllAA[result_Association] := Module[{rows,out={},row,z,i,bad},
  rows = Lookup[result,"PerDiagramResults",None];
  If[!ListQ[rows] || rows==={} || !AllTrue[rows,AssociationQ[#] && KeyExistsQ[#,"Amplitude"] &&
    RGZLoopValidValueQ[#["Amplitude"]]&],Return[Failure["InvalidAAAmplitudeRows",<||>]]];
  Do[
    row=rows[[i]]; Print["Reducing ",i," / ",Length[rows]];
    z = RGZLoopTID[row["Amplitude"]];
    AppendTo[out,Join[row,<|"PaVeResult"->z|>]],{i,Length[rows]}];
  bad = Select[out,!RGZLoopValidValueQ[#["PaVeResult"]]&];
  If[bad =!= {},Return[Failure["ReductionFailed",<|"FailedTerm"->bad|>]]];
  z=Factor2[Total[Lookup[out,"PaVeResult"]]];
  If[!RGZLoopValidValueQ[z],Return[Failure["InvalidAATotal",<||>]]];
  Join[result,<|"PerDiagramPaVe"->out,"AmputatedAATotalPaVe"->z|>]
];

