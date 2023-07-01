unit munit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, JsonTools,
  Radio, CommonTypes, LCLType, Menus, ComCtrls, ActnList,
  opensslsockets, BGRASVG, BGRASVGType, BGRABitmap, BGRABitmapTypes, BCPanel,
  BCButton, BGRAImageList, BCLabel,
  { Codebot units }
  Codebot.System, Codebot.Graphics, Codebot.Graphics.Types, Codebot.Animation,
  Codebot.Controls.Scrolling, Slider;

const  StatioPath = 'result/stations/';

type
  RadioInfo = record
    FId: integer;
    FPrefix: string;
    FTitle: string;
    FTooltip: string;
    FSort: integer;
    FBg_image: string;
    FBg_image_mobile: string;
    FSvg_outline: string;
    FSvg_fill: string;
    FShort_title: string;
    FNew: boolean;
    FStream_64: string;
    FStream_128: string;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    BufferTimer: TAnimationTimer;
    BCLabel1: TBCLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PnlBottom: TBCPanel;
    actSortFav: TAction;
    actPlayPause: TAction;
    ActionList: TActionList;
    TrayMenu: TPopupMenu;
    RenderTimer: TAnimationTimer;
    BCButton11: TBCButton;
    PnlTop: TBCPanel;
    Bevel1: TBevel;
    Slider1: TSlider;

    ToolBarImg: TBGRAImageList;
    Image: TImageStrip;
    ImagePeak: TImageStrip;
    ImageFav: TImageStrip;
    Panel1: TPanel;
    RadioList: TDrawList;
    TrayIcon: TTrayIcon;
    procedure FormKeyDown(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure actPlayPauseExecute(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure RenderTimerTimer(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure RadioListDblClick(Sender: TObject);
    procedure RadioListDrawItem(Sender: TObject; Surface: ISurface;
      Index: Integer; Rect: TRectI; State: TDrawState);
    procedure RadioListSelectItem(Sender: TObject);
    procedure Slider1MouseEnter(Sender: TObject);
    procedure Slider1MouseLeave(Sender: TObject);
    procedure Slider1PositionChanged(Sender: TObject; NewPos: integer);
  private
    FRadioInfo: array of RadioInfo;
    FRadioNode: TJsonNode;
    FRadio:TRadio;
    FItemIndex: Integer;
    Channel: DWORD;
    PeakRect: TRectI;
    FFTFata : TFFTData;
    FFTPeacks  : array [0..128] of Integer;
    FFTFallOff : array [0..128] of Integer;
    SurfacePeak: TSurfaceBitmap;
    procedure FillRadioList;
  public
  end;

var
  MainForm: TMainForm;
  TmpCount : Integer;
implementation
 uses Bass;
{$R *.lfm}

procedure StatusProc({%H-}pszData : string;{%H-}Progress:Integer; Channel: HStream);
begin
  MainForm.RenderTimer.Enabled := false;
  Application.ProcessMessages;
  MainForm.Channel := Channel;
  MainForm.RenderTimer.Enabled := true;
end;

procedure BroadcastMetaProc(pszData : string);
begin
  MainForm.Caption := Format('%s',[pszData]);
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FRadioNode := TJsonNode.Create;
  FRadioNode.LoadFromUrl('https://www.radiorecord.ru/api/stations/');
  try
    SetLength(FRadioInfo, FRadioNode.Find('result/stations').Count);
    FRadio := TRadio.Create;
    FRadio.SetOwner(Self);
    FRadio.SetVolume(0.5);
    FRadio.SetStatusProc(@StatusProc);
    FRadio.SetBroadcastMetaProc(@BroadcastMetaProc);
  finally
    FillRadioList;
  end;
  RadioList.Color:= clMenu;
  FItemIndex := -1;
  SurfacePeak := TSurfaceBitmap.Create;
  SurfacePeak.SetSize(PeakRect.Width, PeakRect.Height);
  Slider1.BorderColor := clScrollBar;
  Slider1.BarColor := clScrollBar;
  Slider1.SetParams(0,100,50);
end;

procedure TMainForm.RenderTimerTimer(Sender: TObject);
begin
  if BASS_ChannelIsActive(Channel) <> BASS_ACTIVE_PLAYING then exit;
  SurfacePeak.SetSize(PeakRect.Width,PeakRect.Height);
  BASS_ChannelGetData(Channel, @FFTFata, BASS_DATA_FFT1024);
  RadioList.Invalidate;
end;

procedure TMainForm.actPlayPauseExecute(Sender: TObject);
begin
  RenderTimer.Enabled := false;
  if (BASS_ChannelIsActive(Channel) <> BASS_ACTIVE_PLAYING) and (RadioList.ItemIndex >= 0) then
  begin
    FItemIndex := RadioList.ItemIndex;
    FRadio.SetStreamURL(FRadioInfo[RadioList.ItemIndex].FStream_128);
    FRadio.Play;
    actPlayPause.ImageIndex:=1;
  end else
  begin
    FRadio.Pause;
    actPlayPause.ImageIndex:=0;
    RadioList.Repaint;
  end;
  RenderTimer.Enabled := True;
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin
  if MainForm.Showing then MainForm.Hide
  else
  MainForm.Show;
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
begin
MainForm.Close;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 RadioList.SetFocus;
 RadioList.ScrollToSelection;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FRadio.Pause;
  RenderTimer.Enabled:=False;
  if Assigned(FRadio) then FRadio.Free;
  if Assigned(FRadioNode) then FRadioNode.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  RadioList.Invalidate;
  PnlTop.Invalidate;
  PnlBottom.Invalidate;
end;

procedure TMainForm.RadioListDblClick(Sender: TObject);
begin
  if FItemIndex <> RadioList.ItemIndex then FRadio.Pause;
  actPlayPause.Execute;
end;

procedure TMainForm.RadioListDrawItem(Sender: TObject; Surface: ISurface;
  Index: Integer; Rect: TRectI; State: TDrawState);
var R, RR: TRectF; SelR, ImgRect: TRectI; F: IFont;
    P: IPen; i : Integer; Val : Single;  pos : Integer;
begin
  (* fill back ground *)
  SelR:= Rect;
  SelR.Bottom:=SelR.Bottom -4;
  SelR.Top:=SelR.Top + 4;
  SelR.Left:=+4;
  SelR.Right:=SelR.Right-4;
  FillRectColor(Surface,SelR,clMenuBar,2);

  (* render image *)
  ImgRect := SelR;
  ImgRect.Left:=+10;
  ImgRect.Right:=ImgRect.Left+60;
  Image.Draw(Surface,Index,ImgRect);

  (* render peaks *)
  PeakRect := SelR;
  PeakRect.Right:=SelR.Right - 4;
  PeakRect.Left:= PeakRect.Right - 64;

  if FItemIndex = Index then
  begin
    for i := 0 to 11 do begin
    Val := FFTFata[(i * 8) + 5];
    pos := Trunc((val) * 500);
    if pos > SurfacePeak.Height -8 then pos := SurfacePeak.Height - 8;
    if pos <=0 then pos:=3;

    if pos >= FFTPeacks[i] then FFTPeacks[i] := pos
    else FFTPeacks[i] := FFTPeacks[i] - 1;

    if pos >= FFTFallOff[i] then FFTFallOff[i] := pos
    else FFTFallOff[i] := FFTFallOff[i] - 3;

    if FFTPeacks[i] < 3 then FFTPeacks[i] := 3;

    P := NewPen(ClGrayText, 1);
    P.Color.Blend(ClGrayText,1.5);
    P.LineCap := cpRound;

    RR.Left:=i * 4 + i + 1;
    RR.Top:=SurfacePeak.Height - 3 - FFTFallOff[i];
    RR.Right:=i * 4 + 4 + i + 1;
    RR.Bottom:=SurfacePeak.Height - 3;
    SurfacePeak.Surface.StrokeRect(P,RR);

    RR.Left:=i * 4 + i + 1;
    RR.Top:=SurfacePeak.Height - 3 - FFTPeacks[i];
    RR.Right:=i * 4 + 4 + i + 1;
    RR.Bottom:=SurfacePeak.Height - 1 - FFTPeacks[i];
    SurfacePeak.Surface.StrokeRect(P,RR);
    SurfacePeak.Draw(Surface, SurfacePeak.Bitmap.ClientRect,PeakRect);
 end;
 SurfacePeak.Clear;
 end;
  (* render select *)
  if dsSelected in State then
  begin
    SelR:= Rect;
    SelR.Bottom:=SelR.Bottom -4;
    SelR.Top:=SelR.Top + 4;
    SelR.Left:=+4;
    SelR.Right:=SelR.Right-4;
    FillRectSelected(Surface, SelR, 2);
  end;
  (* render text *)
  R := Rect;
  R.Inflate(-66, 0);
  R.Bottom := R.Bottom - 4;
  R.Top:=R.Top + 4;
  F := Theme.Font;
  F.Color := clMenuText;
  F.SetSize(11);
  R.Left:=R.Left+10;
  Surface.TextOut(F, FRadioInfo[Index].FTitle, R, drLeft);
end;

procedure TMainForm.RadioListSelectItem(Sender: TObject);
begin
  BCLabel1.Caption := FRadioInfo[RadioList.ItemIndex].FTooltip;
end;

procedure TMainForm.Slider1MouseEnter(Sender: TObject);
begin
  Slider1.BorderColor := clHighlight;
  Slider1.BarColor := clHighlight;
end;

procedure TMainForm.Slider1MouseLeave(Sender: TObject);
begin
  Slider1.BorderColor := clScrollBar;
  Slider1.BarColor := clScrollBar;
end;

procedure TMainForm.Slider1PositionChanged(Sender: TObject; NewPos: integer);
begin
Slider1.BorderColor := clHighlight;
Slider1.BarColor := clHighlight;
FRadio.SetVolume(NewPos/100);
end;

procedure TMainForm.FillRadioList;
var i, j: integer;
    s: String;
    svg: TBGRASVG;
    ms: TMemoryStream;
    bmp: TBGRABitmap;
    SurfaceBmp: TSurfaceBitmap;
begin
  try
  for i:= 0 to FRadioNode.Find('result/stations').Count-1 do
  begin
    with FRadioInfo[i] do
    begin
      FId := Trunc(FRadioNode.Find(StatioPath + IntToStr(i) + '/id').AsNumber);
      FPrefix := FRadioNode.Find(StatioPath + IntToStr(i) + '/prefix').AsString;
      FTitle := FRadioNode.Find(StatioPath + IntToStr(i) + '/title').AsString;
      FTooltip := FRadioNode.Find(StatioPath + IntToStr(i) + '/tooltip').AsString;
      FSort := Trunc(FRadioNode.Find(StatioPath + IntToStr(i) + '/sort').AsNumber);
      FBg_image := FRadioNode.Find(StatioPath + IntToStr(i) + '/bg_image').AsString;
      FBg_image_mobile := FRadioNode.Find(StatioPath + IntToStr(i) + '/bg_image_mobile').AsString;
      FSvg_outline := FRadioNode.Find(StatioPath + IntToStr(i) + '/svg_outline').AsString;
      FSvg_fill := FRadioNode.Find(StatioPath + IntToStr(i) + '/svg_fill').AsString;
      FShort_title := FRadioNode.Find(StatioPath + IntToStr(i) + '/short_title').AsString;
      FNew := FRadioNode.Find(StatioPath + IntToStr(i) + '/new').AsBoolean;
      FStream_64 := FRadioNode.Find(StatioPath + IntToStr(i) + '/stream_64').AsString;
      FStream_128 := FRadioNode.Find(StatioPath + IntToStr(i) + '/stream_128').AsString;
    end;
  end;
  finally
    RadioList.Count:=Length(FRadioInfo);
  end;
  bmp:= TBGRABitmap.Create;
  Image.Clear;
  for i:= 0 to RadioList.Count -1 do
  begin
    ms:= TMemoryStream.Create;
    SurfaceBmp := TSurfaceBitmap.Create;
    try
      s:= FRadioInfo[i].FSvg_outline;
      ms.WriteBuffer(s[1],Length(s));
      ms.Position:= 0;
      svg:= TBGRASVG.Create(ms);
      svg.ContainerWidthAsPixel := 200;
      svg.ContainerHeightAsPixel := 200;
      bmp.SetSize(Round(svg.WidthAsPixel),Round(svg.HeightAsPixel));
      bmp.Fill(clMenuBar);
      for j:=0 to svg.Content.ElementCount-1  do
      begin
        if svg.Content.IsSVGElement[j] then svg.Content.Element[j].fillColor := clMenuText;
      end;
      svg.StretchDraw(bmp.Canvas2D, 0,0,bmp.Width,bmp.Height);
      SurfaceBmp.Assign(bmp.Bitmap);
      Image.Add(SurfaceBmp);
    finally
      svg.Free;
      ms.free;
      SurfaceBmp.Free;
    end;
  end;
  bmp.Free;
end;

end.
