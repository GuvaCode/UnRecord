unit munit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, JsonTools,
  Radio, CommonTypes, LCLType, Menus, ComCtrls, ActnList,
  BGRASVG, BGRASVGType, BGRABitmap, BGRABitmapTypes, BCPanel,
  BCButton, BGRAImageList,
  { Codebot units }
  Codebot.System, Codebot.Graphics, Codebot.Graphics.Types, Codebot.Animation,
  Codebot.Controls.Scrolling, Codebot.Controls.Extras, Slider;

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
    FListPicture: TSurfaceBitmap;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    BufferTimer: TAnimationTimer;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PnlBottom: TBCPanel;
    actSortFav: TAction;
    actPlayPause: TAction;
    ActionList: TActionList;
    SongRender: TRenderBox;
    TrayMenu: TPopupMenu;
    RenderTimer: TAnimationTimer;
    BCButton11: TBCButton;
    PnlTop: TBCPanel;
    Bevel1: TBevel;
    Slider1: TSlider;
    ToolBarImg: TBGRAImageList;
    Panel1: TPanel;
    RadioList: TDrawList;
    TrayIcon: TTrayIcon;
    procedure actPlayPauseUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
    procedure Slider1MouseEnter(Sender: TObject);
    procedure Slider1MouseLeave(Sender: TObject);
    procedure Slider1PositionChanged(Sender: TObject; NewPos: integer);
    procedure SongRenderRender(Sender: TObject; Surface: ISurface);
  private
    FRadioInfo: array of RadioInfo;

    FRadioNode: TJsonNode;
    FRadio:TRadio;
    FItemIndex: Integer;
    Channel: DWORD;
    PeakRect: TRectI;
    FFTFata : TFFTData;
    FCaption : String;
    FFTPeacks  : array [0..128] of Integer;
    FFTFallOff : array [0..128] of Integer;
    SurfacePeak: TSurfaceBitmap;
    procedure FillRadioList;
    function  NodeLoadFromUrl(const Url: string; Node: TJsonNode): Boolean;
  public
    procedure Init;
  end;

var
  MainForm: TMainForm;
  TmpCount : Integer;

implementation
uses bass, fphttpclient, opensslsockets;

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
  MainForm.FCaption := Format('%s',[pszData]);
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FRadioNode := TJsonNode.Create;
  {$ifdef linux}
  RadioList.Color:= clMenu;
  {$endif}
  FItemIndex := -1;
  SurfacePeak := TSurfaceBitmap.Create;
  SurfacePeak.SetSize(PeakRect.Width, PeakRect.Height);
  Slider1.BorderColor := clScrollBar;
  Slider1.BarColor := clScrollBar;
  Slider1.SetParams(0,100,50);
 // Init;
end;

procedure TMainForm.RenderTimerTimer(Sender: TObject);
begin
  if BASS_ChannelIsActive(Channel) <> BASS_ACTIVE_PLAYING then exit;
  SurfacePeak.SetSize(PeakRect.Width,PeakRect.Height);
  BASS_ChannelGetData(Channel, @FFTFata, BASS_DATA_FFT1024);
  RadioList.Invalidate;
  SongRender.Invalidate;
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

procedure TMainForm.actPlayPauseUpdate(Sender: TObject);
begin
  if Assigned(FRadio) then actPlayPause.Enabled:=True else Init;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FRadio) then FRadio.Pause;
  RenderTimer.Enabled:=False;
  if Assigned(FRadio) then FRadio.Free;
  if Assigned(FRadioNode) then FRadioNode.Free;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin

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
var R, RR: TRectF; SelR, ImgRect: TRectI; F: IFont; C: TColorB;
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
  FRadioInfo[Index].FListPicture.Draw(Surface,FRadioInfo[Index].FListPicture.ClientRect,ImgRect);

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

    C:=clMenuText;
    P := NewPen(clMenuText, 1);
    P.Color := C.Blend(clWindow, 0.50);
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
  R.Bottom := R.Bottom - 18;
  R.Top:=R.Top + 4;
  R.Left:=R.Left+76;
  R.Right:=PeakRect.Left - 6;
  F := Theme.Font;
  F.Color := clMenuText;
  {$ifdef linux}
  F.SetSize(11);
  {$else}
  F.SetSize(20);
  {$endif}
  F.Style:=[];

  Surface.TextOut(F, FRadioInfo[Index].FTitle, R, drLeft);
  C := clMenuText;
  F.Color := C.Blend(clWindow, 0.25);
  F.Style:=[fsItalic];

  {$ifdef linux}
  F.SetSize(8);
  {$else}
  F.SetSize(16);
  {$endif}
  R.Top:=R.Bottom - 12;
  Surface.TextOut(F, FRadioInfo[Index].FTooltip , R, drLeft);
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

procedure TMainForm.SongRenderRender(Sender: TObject; Surface: ISurface);
var R: TRectF; C: TColorB; F: IFont;
begin
  R := SongRender.ClientRect;
  C := clMenuText;
  F := Theme.Font;
  F.Color := C.Blend(clWindow, 0.15);
  F.Style:=[fsItalic];
  {$ifdef linux}
  F.SetSize(8);
  {$else}
  F.SetSize(16);
  {$endif}
  Surface.TextOut(F, FCaption , R, drCenter);
end;


procedure TMainForm.FillRadioList;
var i, j: integer;
    s: String;
    svg: TBGRASVG;
    ms: TMemoryStream;
    bmp: TBGRABitmap;
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
      FListPicture := TSurfaceBitmap.Create;
    end;
  end;
  finally
    RadioList.Count:=Length(FRadioInfo);
  end;
  for i:= 0 to RadioList.Count -1 do
  begin
    ms:= TMemoryStream.Create;
    bmp:= TBGRABitmap.Create;

    try
      s:= FRadioInfo[i].FSvg_outline;
      ms.WriteBuffer(s[1],Length(s));
      ms.Position:= 0;


      svg:= TBGRASVG.Create(ms);
      svg.ContainerWidthAsPixel := 64;//200;
      svg.ContainerHeightAsPixel := 64;//200;

      bmp.SetSize(64,64);
      ///bmp.SetSize(Round(svg.WidthAsPixel),Round(svg.HeightAsPixel));
      bmp.Fill(clMenuBar);

      for j:=0 to svg.Content.ElementCount-1  do
      begin
        if svg.Content.IsSVGElement[j] then svg.Content.Element[j].fillColor := clMenuText;
      end;
      svg.StretchDraw(bmp.Canvas2D, 0,0,bmp.Width,bmp.Height);

      FRadioInfo[i].FListPicture.Assign(bmp.Bitmap);
    finally
      svg.Free;
      ms.free;
      bmp.Free;
    end;
  end;
end;

function TMainForm.NodeLoadFromUrl(const Url: string; Node: TJsonNode): Boolean;
var M: TMemoryStream;
    http: TFPHTTPClient;
    Load: Boolean;
begin
  M := TMemoryStream.Create;
  http := TFPHTTPClient.Create(nil);
  http.AllowRedirect:= True;
  try
    try
      http.HTTPMethod('HEAD', URL, nil, []);
      http.Get(URL,M);
      M.Position:=0;
      Node.LoadFromStream(M);
      Load := True;
    except
       on E: Exception do
       begin
         if http.ResponseStatusCode > 399 then
         begin
           writeln(SysUtils.Format('Status: %d', [http.ResponseStatusCode]));
           FCaption:= (SysUtils.Format('Status: %d', [http.ResponseStatusCode]));
           Load := False;
         end;
           writeln('Error: ' + E.Message);
           FCaption:=('Error: ' + E.Message);
           Load := False;
       SongRender.Invalidate;
       end;
    end;
  finally
    FreeAndNil(M);
    http.Free;
    Result := Load;
  end;
end;

procedure TMainForm.Init;
begin
  if self.NodeLoadFromUrl('https://www.radiorecord.ru/api/stations/',FRadioNode) then
  try
    SetLength(FRadioInfo, FRadioNode.Find('result/stations').Count);
    FRadio := TRadio.Create;
    FRadio.SetOwner(Self);
    FRadio.SetVolume(0.5);
    FRadio.SetStatusProc(@StatusProc);
    FRadio.SetBroadcastMetaProc(@BroadcastMetaProc);
  finally
    try
    FillRadioList;
    finally
    RadioList.Enabled:=True;
    end;
  end;

end;

end.

