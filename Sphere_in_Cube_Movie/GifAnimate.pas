unit GifAnimate;
(******************************************************************************
Unit to make an animated GIF.
Author: Finn Tolderlund
        Denmark
Date: 14.07.2003

homepage:
http://home20.inet.tele.dk/tolderlund/
http://finn.mobilixnet.dk/
e-mail:
finn@mail.tdcadsl.dk
finn.tolderlund@mobilixnet.dk

This unit requires the GIFImage.pas unit from Anders Melander.
The GIFImage.pas unit can be obtained from my homepage above.

This unit can freely be used and distributed.

Disclaimer:
Use of this unit is on your own responsibility.
I will not under any circumstance be hold responsible for anything
which may or may not happen as a result of using this unit.
******************************************************************************)
(******************************************************************************
Example of use:

procedure TFormSphereMovie.MakeGifButtonClick(Sender: TObject);
var
  FrameIndex: Integer;
  Picture: TPicture;
begin
  Screen.Cursor := crHourGlass;
  try
    GifAnimateBegin;
    {Step through each frame in in-memory list}
    for FrameIndex := 0 to BitMapList.Count - 1 do
    begin
      // add frame to animated gif
      GifAnimateAddImage(TNode(BitmapList.Items[FrameIndex]).Bitmap,
        False, SpinEditMinMillisecondsPerFrame.Value);
    end;
    // We are using a TPicture but we could have used a TGIFImage instead.
    // By not using TGIFImage directly we do not have to add GIFImage to the uses clause.
    // By using TPicture we only need to add GifAnimate to te uses clause.
    Picture := GifAnimateEnd;
    Picture.SaveToFile(ExtractFilePath(ParamStr(0)) + 'sphere.gif');  // save gif
    ImageMovieFrame.Picture.Assign(Picture);  // display gif
    Picture.Free;
  finally
    Screen.Cursor := crDefault;
  end;
end;
******************************************************************************)

interface

uses
  Windows, Graphics, GIFImage;

procedure GifAnimateBegin;
function GifAnimateEnd: TPicture;
function GifAnimateAddImage(Source: TGraphic; Transparent: Boolean; DelayMS: Word): Integer;

implementation

var
  GIF: TGIFImage;

function TransparentIndex(GIF: TGIFSubImage): byte;
begin
  // Use the lower left pixel as the transparent color
  Result := GIF.Pixels[0, GIF.Height-1];
end;

function GifAnimateAddImage(Source: TGraphic; Transparent: Boolean; DelayMS: Word): Integer;
var
  Ext			: TGIFGraphicControlExtension;
  LoopExt		: TGIFAppExtNSLoop;
begin
  // Add the source image to the animation
  Result := GIF.Add(Source);
  // Netscape Loop extension must be the first extension in the first frame!
  if (Result = 0) then
  begin
    LoopExt := TGIFAppExtNSLoop.Create(GIF.Images[Result]);
    LoopExt.Loops := 0; // Number of loops (0 = forever)
    GIF.Images[Result].Extensions.Add(LoopExt);
  end;
  // Add Graphic Control Extension
  Ext := TGIFGraphicControlExtension.Create(GIF.Images[Result]);
  Ext.Delay := DelayMS div 10;  // 30; // Animation delay (30 = 300 mS)
//  if (Result > 0) then
  if (Transparent) then
  begin
    Ext.Transparent := True;
    Ext.TransparentColorIndex := TransparentIndex(GIF.Images[Result]);
  end;
  GIF.Images[Result].Extensions.Add(Ext);
end;

procedure GifAnimateBegin;
begin
  GIF.Free;
  GIF := TGIFImage.Create;
  GIF.ColorReduction := rmQuantizeWindows;
  //  GIF.DitherMode := dmNearest;  // no dither, use nearest color in palette
  GIF.DitherMode := dmFloydSteinberg;
  GIF.Compression := gcLZW;
end;

function GifAnimateEnd: TPicture;
begin
  Result := TPicture.Create;
  Result.Assign(GIF);
  GIF.Free;
  GIF := nil;
end;

initialization
  GIF := nil;
finalization
  GIF.Free;
end.
