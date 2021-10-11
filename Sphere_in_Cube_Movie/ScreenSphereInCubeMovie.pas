// Sphere Movie Demo -- Generate a number of frames to make a "movie" of
// flying around a sphere inside a cube. (In image processing such a list
// is often called a "stack".)
//
// Acceptable results were observed using a 166 MHz Pentium with 32 MB
// of memory.  You can generate longer sequences if you have more memory.
// Sometimes replaying the sequence multiple times will improve performance
// -- I'm assuming this is some sort of real/virtual memory interaction.
//
// Set MaxFrames limit to 750 -- I ran out of resources with 384 MB memory
// on a Windows 2000 box with ~950 256-by-256 images.  (15 July 2003)
//
// Thanks to Finn Tolderlund for adding animated GIF feature (15 July 2003)
//
// Copyright (C) 1997-1998, 2003 Earl F. Glynn, Overland Park, KS
// All Rights Reserved. E-mail Address: efg2@efg2.com

unit ScreenSphereInCubeMovie;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ExtDlgs;

CONST
   MaxFrames = 500;

type
  TFormSphereMovie =  class(TForm)
    ImageMovieFrame: TImage;
    ButtonGenMovie: TButton;
    SpinEditMaxFrames: TSpinEdit;
    ButtonShowMovie: TButton;
    ButtonStop: TButton;
    LabelFrames: TLabel;
    ScrollBar: TScrollBar;
    SpinEditMinMillisecondsPerFrame: TSpinEdit;
    LabelMinMillisecondsPerFrame: TLabel;
    MakeGifButton: TButton;
    RadioGroupSize: TRadioGroup;
    SavePictureDialog: TSavePictureDialog;
    CheckBoxIncludeFrameNumber: TCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure ButtonGenMovieClick(Sender: TObject);
    procedure ButtonShowMovieClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure MakeGifButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EnableGenerateButton(Sender: TObject);
  private
    { Private declarations }
    EarlyOut: BOOLEAN;
    BitmapList: TList;
    procedure FreeBitmapListAndContents;
  public
    { Public declarations }
  end;

var
  FormSphereMovie: TFormSphereMovie;

implementation

  USES
{$IFDEF GIF}
    GifAnimate,
{$ENDIF}
    Printers,
    GraphicsMathLibrary,
    GraphicsPrimitivesLibrary,   // TPantoGraph
    DrawFigures;                 // DrawCube, DrawSphere

{$R *.DFM}

  TYPE
    TNode =
      class(TObject)
        Bitmap:  TBitMap;
      end;

procedure TFormSphereMovie.ButtonGenMovieClick(Sender: TObject);
  VAR
    a         :  TMatrix;
    FrameIndex:  INTEGER;
    FrameLabel:  STRING;
    node      :  TNode;
    pantograph:  TPantoGraph;
    size      :  INTEGER;
BEGIN
  ImageMovieFrame.Picture.Assign(NIL);

  size := StrToInt(RadioGroupSize.Items[RadioGroupSize.ItemIndex]);
  ImageMovieFrame.Width  := size;
  ImageMovieFrame.Height := size;

  ButtonShowMovie.Enabled := FALSE;
  SpinEditMinMillisecondsPerFrame.Visible := FALSE;
  LabelMinMillisecondsPerFrame.Visible := FALSE;

  ScrollBar.Visible := FALSE;
  ButtonStop.Enabled := TRUE;
  EarlyOut := FALSE;

  FreeBitmapListAndContents;
  BitmapList := TList.Create;

  ButtonGenMovie.Cursor := crHourGlass;

  pantograph := TPantoGraph.Create(ImageMovieFrame.Canvas);
  pantograph.ViewPort (0.00,1.00, 0.00,1.00);

  // Catch EarlyOut exit to get cursor reset and memory freed.
  TRY

    // Catch any memory or resource problem.
    TRY
      // Create each movie frame in this loop.
      FOR FrameIndex := 0 TO SpinEditMaxFrames.Value-1 DO
      BEGIN
        ImageMovieFrame.Canvas.FillRect(
          Rect(0,0, ImageMovieFrame.Width, ImageMovieFrame.Height) );

        IF   CheckBoxIncludeFrameNumber.Checked
        THEN BEGIN
          // Let's put unique frame number on image
          WITH ImageMovieFrame.Picture.Bitmap.Canvas
          DO BEGIN
            FrameLabel := IntToStr(FrameIndex+1);
            TextOut(TextWidth('9999')-TextWidth(FrameLabel),{Right Justify Number}
                    0,                                      {Top}
                    FrameLabel)
          END
        END;

        // Use FrameIndex in parametric equation to define location of "camera"
        // for each frame.
        a := ViewTransformMatrix (
                  // Use spherical coordinates instead of cartesian
                  coordSpherical,

                  // azimuth -- cycle around object every 300 Frames
                  ToRadians(1.2 {degrees} * FrameIndex),

                  // elevation -- cycle up and down 90 degrees every 90 Frames
                  ToRadians(90 {degrees} * SIN(ToRadians(4.0*FrameIndex))),

                  // distance from camera to object -- cycle every 400 Frames
                  10 + 6*SIN(ToRadians(0.9*FrameIndex)),

                  // screen parameters: 10x10 from 30 away
                  10,10,30);

        pantograph.SetTransform (a);
        DrawCube(PantoGraph, clRed);
        DrawSphere(PantoGraph,
                   {LatitudeColor}         clBlue,
                   {LongitudeColor}        clLime,
                   {LatitudeCircles}        9,
                   {LongitudeSemicircles}  25,
                   {PointsInCircle}        40);

        node := TNode.Create;
        node.Bitmap        := TBitmap.Create;
        node.Bitmap.Width  := ImageMovieFrame.Width;
        node.Bitmap.Height := ImageMovieFrame.Height;
        WITH node.BitMap DO
          Canvas.CopyRect(Rect(0,0, Width,Height),
                          ImageMovieFrame.Picture.Bitmap.Canvas,
                          Rect(0,0, Width,Height));
        BitmapList.Add(node);

        Application.ProcessMessages;  {EarlyOut check + forces repaint}

        IF   EarlyOut
        THEN EXIT
      END;
    EXCEPT
      On EOutOfMemory    DO MessageDlg('Out of Memory',    mtWarning, [mbOK], 0);
      On EOutOfResources DO MessageDlg('Out of Resources', mtWarning, [mbOK], 0);
    END

  FINALLY
    PantoGraph.Free;
    ButtonGenMovie.Cursor := crDefault;
    ButtonGenMovie.Enabled := FALSE;
    ButtonStop.Enabled := FALSE;

    ButtonShowMovie.Enabled := TRUE;
    SpinEditMinMillisecondsPerFrame.Visible := TRUE;
    LabelMinMillisecondsPerFrame.Visible := TRUE;
//{$IFDEF GIF}
    MakeGifButton.Enabled := TRUE;
//{$ENDIF}

    ScrollBar.Max := BitMapList.Count - 1;
    ScrollBar.Visible := TRUE;
    ScrollBar.Position := ScrollBar.Max;
  END;

 MakeGifButton.Enabled:=true; 
end;

procedure TFormSphereMovie.FormCreate(Sender: TObject);
  VAR
    Bitmap:  TBitmap;
begin
  BitmapList := TList.Create; // Create empty list

  Bitmap        := TBitmap.Create;
  TRY
    Bitmap.Width  := ImageMovieFrame.Width;
    Bitmap.Height := ImageMovieFrame.Height;
    ImageMovieFrame.Picture.Graphic := Bitmap
  FINALLY
    Bitmap.Free
  END;

//{$IFDEF GIF}
  MakeGifButton.Visible := TRUE;
//{$ENDIF}
end;

procedure TFormSphereMovie.FormDestroy(Sender: TObject);
begin
  FreeBitMapListAndContents
end; 

procedure TFormSphereMovie.ButtonShowMovieClick(Sender: TObject);
  VAR
    FrameIndex:  INTEGER;
    StartTicks:  DWORD;
begin
  // reset ImageMovieFrame from gif to bitmap
  ImageMovieFrame.Picture.Assign(nil);
  ImageMovieFrame.Picture.Bitmap.Width  := ImageMovieFrame.Width;
  ImageMovieFrame.Picture.Bitmap.Height := ImageMovieFrame.Height;

  ButtonStop.Enabled := TRUE;

  {Set this flag to FALSE to start display of "movie".  Application.ProcessMessages
   allows flag to be reset while in display loop.}
  EarlyOut := FALSE;

  {Step through each frame in in-memory list}
  FOR FrameIndex := 0 TO BitMapList.Count-1 DO
  BEGIN
    {FYI:  GetTickCount is Windows API function}
    StartTicks := GetTickCount;
    ScrollBar.Position := FrameIndex;

    {Kill time if necessary.  ProcessMessages at least once.}
    REPEAT
      {Force repaint of screen and enable check for EarlyOut}
      Application.ProcessMessages;
    UNTIL GetTickCount - StartTicks >= DWORD( SpinEditMinMillisecondsPerFrame.Value );

    {Exit loop if EarlyOut requested.}
    IF   EarlyOut
    THEN EXIT
  END;

  ButtonStop.Enabled := FALSE
end;

PROCEDURE TFormSphereMovie.FreeBitMapListAndContents;
  VAR
    node      :  TNode;
    FrameIndex:  INTEGER;
BEGIN
  FOR FrameIndex := BitMapList.Count-1 DOWNTO 0 DO
  BEGIN
    node := BitMapList.List[FrameIndex];
    node.BitMap.Free;  {Free Bitmap in node}
    node.Free;         {Free node in List}
  END;
  BitmapList.Free;     {Free List}
END;

procedure TFormSphereMovie.ButtonStopClick(Sender: TObject);
begin
  EarlyOut := TRUE
end;

procedure TFormSphereMovie.ScrollBarChange(Sender: TObject);
begin
  WITH ImageMovieFrame.Picture.Bitmap DO
  BEGIN
    // Copy in-memory frame back to screen
    Canvas.CopyRect(Rect(0,0, Width,Height),
                    TNode(BitMapList.Items[ScrollBar.Position]).BitMap.Canvas,
                    Rect(0,0, Width,Height));
  END;
end;


// Thanks to Finn Tolderlund for adding this animated GIF improvement (14 July 2003)
// This change requires the GIFImage unit from Anders Melander.
// The GIFImage unit can be downloaded from Finn Tolderlund's websites:
// http://finn.mobilixnet.dk/delphi/
// http://home20.inet.tele.dk/tolderlund/delphi/

procedure TFormSphereMovie.MakeGifButtonClick(Sender: TObject);
{$IFDEF GIF}
var
  FrameIndex: Integer;
  Picture: TPicture;
{$ENDIF}
begin
{$IFDEF GIF}
  SavePictureDialog.InitialDir := ExtractFilePath(ParamStr(0));
  IF   SavePictureDialog.Execute
  THEN BEGIN

    Screen.Cursor := crHourGlass;
    try
      // GifAnimateBegin;
      {Step through each frame in in-memory list}
      for FrameIndex := 0 to BitMapList.Count - 1 do
      begin
        // show user that something is happening
        ScrollBar.Position := FrameIndex;
        Application.ProcessMessages;
        // add frame to animated gif
        GifAnimateAddImage(TNode(BitmapList.Items[FrameIndex]).Bitmap,
          False, SpinEditMinMillisecondsPerFrame.Value);
      end;
      // We are using a TPicture but we could have used a TGIFImage instead.
      // By not using TGIFImage directly we do not have to add GIFImage to the uses clause.
      // By using TPicture we only need to add GifAnimate to te uses clause.
      Picture := GifAnimateEnd;
      try
        Picture.SaveToFile(SavePictureDialog.Filename);  // save gif
        ImageMovieFrame.Picture.Assign(Picture);         // display gif
      finally
        Picture.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  END; 

{$ENDIF}
end;

procedure TFormSphereMovie.EnableGenerateButton(Sender: TObject);
begin
  ButtonGenMovie.Enabled:=TRUE;
end;

end.
