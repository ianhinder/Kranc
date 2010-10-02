
BeginPackage["IOASCII`"];

ReadIOASCII::usage = "ReadIOASCII[file] reads the IOASCII file and parses it into list format";
MGraph::usage = "MGraph[file] plots the IOASCII file";

Begin["`Private`"];

ReadIOASCII[file_] :=
 Module[{data1, data2, data3},
  data1 = Import[file, "Table"];
  data2 = Select[SplitBy[data1, Length[#] == 0 &], #[[1]] != {} &];
  data3 = Map[{First[#][[3]], Drop[#, 1]} &, data2]];

MGraph[file_] :=
 Module[{data = ReadIOASCII[file]},
  Manipulate[
   ListLinePlot[data[[it, 2]], PlotLabel -> data[[it, 1]]], {{it,1,"it"}, 1, Length[data], 1}]];

End[];

EndPackage[];
