(* SPDX-License-Identifier: GPL-3.0-or-later *)
root=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{root,"RGZWorkflow.wl"}]];
files=FileNames["*.nb",FileNameJoin[{root,"Examples"}]];
failed=Length[files]=!=2;
Do[nb=Quiet[Check[Get[file],$Failed]];
 If[Head[nb]=!=Notebook,failed=True;Continue[]];
 inputs=Cases[nb,Cell[BoxData[b_],"Input",___]:>b,Infinity];
 parsed=Quiet[Check[ToExpression[#,StandardForm,HoldComplete],$Failed]]&/@inputs;
 calls=DeleteDuplicates[Cases[parsed,HoldPattern[(f_Symbol)[___]]:>SymbolName[Unevaluated[f]],Infinity]];
 unknown=Select[calls,StringMatchQ[#,RegularExpression["RGZ[A-Za-z0-9]+"]]&&#=!="RGZPVFinite"&&
  (Names["RGZWorkflow`"<>#]==={}||ToExpression["RGZWorkflow`"<>#,InputForm,Function[s,DownValues[s]==={},HoldAll]])&];
 saved=Count[nb,Cell[_,"Output"|"Print"|"Message",___],Infinity];
 syntax=FreeQ[parsed,$Failed|_Defer];
 Print[<|"File"->FileNameTake[file],"InputCells"->Length[inputs],"SyntaxOK"->syntax,"UnknownPublicCalls"->unknown,"SavedOutputCells"->saved|>];
 If[!syntax||unknown=!={}||saved=!=0,failed=True],{file,files}];
Exit[If[failed,1,0]];
