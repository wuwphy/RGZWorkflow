(* SPDX-License-Identifier: GPL-3.0-or-later *)
Begin["RGZFHAudit`"];
root=DirectoryName[$InputFileName,4];
Global`FR$Parallel=False;
frDir=Environment["RGZ_FEYNRULES_DIRECTORY"];
If[StringQ[frDir]&&StringLength[frDir]>0,
 Global`$FeynRulesPath=frDir;SetDirectory[frDir];Get[FileNameJoin[{frDir,"FeynRules.m"}]],
 Needs["FeynRules`"]];
Begin["RGZFHAudit`"];
Global`FR$Parallelize=False;
LoadModel[FileNameJoin[{root,"Models","Experimental","FermionicHorizon","RGZ_FermionicHorizon.fr"}]];
Print["FH_MODEL_LOADED"];
rgzFHExport=FileNameJoin[{$TemporaryDirectory,"rgz-fh-export-"<>CreateUUID[]}];
CreateDirectory[rgzFHExport];
rgzFHVertices=FeynRules`FeynmanRules[RGZWorkflow`Engine`LFermionicHorizon,FlavorExpand->True,MinParticles->2,MaxParticles->4,ConservedQuantumNumbers->False,ScreenOutput->False];
Print["FH_VERTEX_COUNT ",Length[rgzFHVertices]];
Export[FileNameJoin[{rgzFHExport,"vertices.wxf"}],rgzFHVertices,"WXF"];
Print["FH_FIELDS ",DeleteDuplicates[Flatten[rgzFHVertices[[All,1,All,1]],1]]];
SetDirectory[rgzFHExport];
FeynRules`WriteFeynArtsOutput[RGZWorkflow`Engine`LFermionicHorizon,Output->"FHGenerated",FlavorExpand->True,MinParticles->3,MaxParticles->4,CouplingRename->False];
Exit[If[ListQ[rgzFHVertices]&&Length[rgzFHVertices]>0,0,1]];
