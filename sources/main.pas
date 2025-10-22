unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ActnList,
  RadioList, RenderInfo, AACPlayer, Codebot.Animation, FluentCircleButton,
  Math, FluentSlider;

type

  { TmainFrm }

  TmainFrm = class(TForm)
    RenderTimer: TAnimationTimer;
    RadioPlayer: TAACPlayer;
    actPlay: TAction;
    ActionList: TActionList;
    btnPlay: TFluentCircleButton;
    FluentSlider1: TFluentSlider;
    Panel1: TPanel;
    pnlList: TPanel;
    pnlTop: TPanel;
    RadioList: TRadioList;
    RenderInfo: TRenderInfo;
    Timer1: TTimer;
    procedure actPlayExecute(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioListDblClick(Sender: TObject);
    procedure RadioPlayerBuffering(Sender: TObject);
    procedure RadioPlayerFFTData(Sender: TObject;
      const FFTData: array of Double; FFTSize: Integer);
    procedure RadioPlayerStateChange(Sender: TObject; OldState,
      NewState: TAACPlayerState);
    procedure RenderTimerTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FFFTData: array[0..7] of Double;
  public

  end;

var
  mainFrm: TmainFrm;

implementation

{$R *.lfm}

{ TmainFrm }



procedure TmainFrm.FormCreate(Sender: TObject);
var i: Integer;
begin
  RadioList.InitRadiolist;
    for i := 0 to 7 do
  FFFTData[i] := 0.1;
end;

procedure TmainFrm.RadioListDblClick(Sender: TObject);
var
  i: integer;
begin
  // Сбрасываем эквалайзер при смене станции
 // for i := 0 to 7 do
 // FFFTData[i] := 0.1;
//  RadioList.UpdateEQData(FFFTData);

  if RadioList.PlayItemIndex = RadioList.ItemIndex then exit;

  if RadioPlayer.State = apsPlaying then
  RadioPlayer.Stop;
  RadioList.Invalidate;
  actPlay.Execute;
end;

procedure TmainFrm.RadioPlayerBuffering(Sender: TObject);
begin
  RenderInfo.GetStationInfoId(RadioList.PlayID);
end;

procedure TmainFrm.RadioPlayerFFTData(Sender: TObject;
  const FFTData: array of Double; FFTSize: Integer);
var
  i: Integer;
  BandScaling: array[0..7] of Double;
  ScaledValue, TotalEnergy: Double;
begin
  // Коэффициенты масштабирования для разных частотных полос
  BandScaling[0] := 0.8; // Суб-бас
  BandScaling[1] := 0.6; // Бас
  BandScaling[2] := 0.7; // Низкие средние
  BandScaling[3] := 0.45; // Средние
  BandScaling[4] := 0.5; // Высокие средние
  BandScaling[5] := 0.6;  // Присутствие
  BandScaling[6] := 0.7;  // Яркость
  BandScaling[7] := 0.8;  // Высокие частоты

  // Проверяем общую энергию сигнала
  TotalEnergy := 0;
  for i := 0 to Min(7, High(FFTData)) do
    TotalEnergy := TotalEnergy + FFTData[i];

  // Если энергия сигнала очень мала (нет звука), сбрасываем эквалайзер
  if TotalEnergy < 0.01 then
  begin
    for i := 0 to 7 do
      FFFTData[i] := 0.1; // Минимальное значение для отображения
  end
  else
  begin
    // Обрабатываем FFT данные при наличии звука
    for i := 0 to Min(7, High(FFTData)) do
    begin
      // Применяем масштабирование и сглаживание
      ScaledValue := FFTData[i] * BandScaling[i];
      FFFTData[i] := (FFFTData[i] * 0.7) + (ScaledValue * 0.4);

      // Ограничиваем значения от 0 до 1
      if FFFTData[i] > 1.0 then FFFTData[i] := 1.0;
      if FFFTData[i] < 0 then FFFTData[i] := 0;

      // Применяем нелинейное преобразование для лучшей визуализации
      FFFTData[i] := Power(FFFTData[i], 0.7);
    end;
  end;


end;

procedure TmainFrm.RadioPlayerStateChange(Sender: TObject; OldState,
  NewState: TAACPlayerState);
begin
  case NewState of
    apsPlaying: btnPlay.PlayState:=psPlay;
    apsPaused: btnPlay.PlayState:=psStop;
    apsStopped: btnPlay.PlayState:=psStop;
  end;
end;

procedure TmainFrm.RenderTimerTimer(Sender: TObject);
begin
    RadioList.UpdateFFTData(FFFTData);
    RenderInfo.UpdateEQData(FFFTData);
    MainFrm.Invalidate;
    if RadioPlayer.State = apsPlaying then btnPlay.PlayState := psPlay
    else btnPlay.PlayState := psStop;
end;

procedure TmainFrm.Timer1Timer(Sender: TObject);
begin
   if RadioList.PlayID > 0 then
   RenderInfo.GetStationInfoId(RadioList.PlayID);
end;



procedure TmainFrm.actPlayExecute(Sender: TObject);
var
  i: Integer;
begin
  RenderTimer.Enabled := false;
  if (RadioPlayer.State <> apsPlaying) and (RadioList.ItemIndex >= 0) then
  begin
    RadioList.PlayItemIndex := RadioList.ItemIndex;
    RadioList.PlayID := RadioList.RadioDownloads[RadioList.ItemIndex].Id;
    RadioPlayer.URL :=RadioList.RadioDownloads[RadioList.ItemIndex].Stream320;
    RadioPlayer.Play;
  end
  else if RadioPlayer.State = apsPlaying then
  begin
    RadioList.PlayItemIndex := -1;
    btnPLay.PlayState:=psStop;
    RadioPlayer.Stop;
    // Сбрасываем эквалайзер при остановке
  // for i := 0 to 7 do
   //   FFFTData[i] := 0.1;
   // RadioList.UpdateEQData(FFFTData);
    RadioList.Repaint;
  end;
    RenderTimer.Enabled := True;



end;

procedure TmainFrm.btnPlayClick(Sender: TObject);
begin
  actPlay.Execute;
end;



end.

