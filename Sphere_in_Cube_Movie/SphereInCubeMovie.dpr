// Sphere Movie Demo -- Generate a number of frames to make a "movie" of
// flying around a sphere inside a cube.
//
// Copyright (C) 1997-1998, 2003 Earl F. Glynn, Overland Park, KS.
// All Rights Reserved.  E-Mail Address:  efg2@efg2.com
//
// Thanks to Finn Tolderlund for adding animated GIF capability.

program SphereInCubeMovie;

uses
  Forms,
  ScreenSphereInCubeMovie in 'ScreenSphereInCubeMovie.pas',
  GraphicsMathLibrary in 'GraphicsMathLibrary.PAS',
  GraphicsPrimitivesLibrary in 'GraphicsPrimitivesLibrary.PAS',
  DrawFigures in 'DrawFigures.pas';

{$R *.RES}

begin
  Application.CreateForm(TFormSphereMovie, FormSphereMovie);
  Application.Run;
end.
