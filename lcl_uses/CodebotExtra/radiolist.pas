unit RadioList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Math, Codebot.System, Codebot.Graphics,  Codebot.Graphics.Types,
  Codebot.Controls.Scrolling, JsonTools, BGRABitmap, lclType,
  BGRABitmapTypes, BGRASVG, RadioDownloads, fphttpclient, opensslsockets;

const
  StatioPath = 'result/stations/';

type
  { TRadioList }

  TRadioList = class(TDrawList)
  private
    FItemHeight: Integer;
    FRadioDownloads: array of IRadioDownload;
    FListPictures: array of TSurfaceBitmap;
    FItemIndex: Integer;
    FPlayID: Integer;

    FFFTData: array[0..7] of Double; // FFT данные для 8 частотных полос
    FDotPositions: array[0..7] of Double; // Текущие позиции точек (0..1)
    FDotVelocities: array[0..7] of Double; // Скорости точек
    FRadioNode: TJsonNode;

    procedure RecreatePictures;
    function NodeLoadFromUrl(const Url: string; Node: TJsonNode): Boolean;
  protected
    procedure DrawItem(Index: Integer; var Rect: TRectI; State: TDrawState); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FillRadioList;
    procedure InitRadiolist;
    procedure SortByName;
    procedure SortBySort;
    procedure SortByUpdated;
    procedure UpdateFFTData(const NewFFTData: array of Double);

    function GetRadioDownload(Index: Integer): IRadioDownload;
    function GetSelectedRadioDownload: IRadioDownload;
    function GetSelectedStreamUrl: string;

    property RadioDownloads[Index: Integer]: IRadioDownload read GetRadioDownload;
    property SelectedRadioDownload: IRadioDownload read GetSelectedRadioDownload;
    property SelectedStreamUrl: string read GetSelectedStreamUrl;

    property PlayID: Integer read FPlayID write FPlayID;
    property ItemHeight: Integer read FItemHeight default 64;
    property PlayItemIndex: Integer read FItemIndex write FItemIndex;
  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Codebot',[TRadioList]);
end;

{ TRadioList }

constructor TRadioList.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  FItemIndex := -1;
  FPlayID := -1;
  FRadioNode := TJsonNode.Create;
  FItemHeight := 64;

  // Инициализируем FFT данные, позиции и скорости точек
  for i := 0 to 7 do
  begin
    FFFTData[i] := 0.1;
    FDotPositions[i] := 1.0; // Начинаем снизу
    FDotVelocities[i] := 0.0; // Начальная скорость
  end;
end;

destructor TRadioList.Destroy;
var
  i: Integer;
begin
  // Освобождаем картинки
  for i := 0 to High(FListPictures) do
    if Assigned(FListPictures[i]) then
      FListPictures[i].Free;
  SetLength(FListPictures, 0);

  // Освобождаем JSON node
  if Assigned(FRadioNode) then
    FRadioNode.Free;

  // Интерфейсы освободятся автоматически
  SetLength(FRadioDownloads, 0);

  inherited Destroy;
end;

procedure TRadioList.DrawItem(Index: Integer; var Rect: TRectI; State: TDrawState);
var
  R: TRectI;
  SelR, ImgRect, VizRect, DotRect: TRectI;
  F: IFont;
  C, DotColor: TColorB;
  i: Integer;
  DotSize, DotSpacing: Integer;
  DotX, DotY: Integer;
  G: IGradientBrush;
  Alpha: Double;
begin
  DotColor := clMenuText;
  DotColor := DotColor.Blend(clWindow, 0.25);

  // fill back ground
  SelR := Rect;
  SelR.Bottom := SelR.Bottom - 4;
  SelR.Top := SelR.Top + 4;
  SelR.Left := +4;
  SelR.Right := SelR.Right - 4;
  FillRectColor(Surface, SelR, clMenuBar, 2);

  // render image
  ImgRect := SelR;
  ImgRect.Left := +10; // Отступ слева 10 пикселей
  ImgRect.Right := ImgRect.Left + 60;

  // Использование картинки из отдельного массива
  if (Index <= High(FListPictures)) and Assigned(FListPictures[Index]) then
    FListPictures[Index].Draw(Surface, FListPictures[Index].ClientRect, ImgRect);

  VizRect := SelR;
  VizRect.Right := SelR.Right - 10; // Отступ справа 10 пикселей (как слева у картинки)
  VizRect.Left := VizRect.Right - 64;
  VizRect.Top := VizRect.Top + 8;
  VizRect.Bottom := VizRect.Bottom - 8;

  // Отрисовываем подпрыгивающие точки для текущего воспроизводимого элемента
  if FItemIndex = Index then
  begin
    DotSize := 4; // Размер точки
    DotSpacing := 7; // Расстояние между точками

    for i := 0 to 7 do
    begin
      // Вычисляем позицию точки по Y (0 - верх, 1 - низ)
      DotY := VizRect.Top + Round(FDotPositions[i] * (VizRect.Height - DotSize));

      // Позиция по X
      DotX := VizRect.Left + i * DotSpacing + 2;

      // Создаем прямоугольник для точки
      DotRect.Left := DotX;
      DotRect.Top := DotY;
      DotRect.Right := DotX + DotSize;
      DotRect.Bottom := DotY + DotSize;

      // Вычисляем альфа-канал на основе высоты (точки внизу более прозрачные)
      Alpha := 1.0 - (FDotPositions[i] * 0.5);
      if Alpha < 0.4 then Alpha := 0.4;

      // Градиент для точки (светлый центр, темные края)
      G := NewBrush(DotRect.Left, DotRect.Top, DotRect.Right, DotRect.Bottom);
      G.AddStop(DotColor.Fade(Alpha + 0.2), 0.3);
      G.AddStop(DotColor.Fade(Alpha), 0.7);
      G.AddStop(DotColor.Fade(Alpha - 0.1), 1.0);

      // Рисуем точку как эллипс
      ///Surface.Ellipse(G, DotRect);

      // Обводка точки
      Surface.Ellipse(DotRect);
      Surface.Stroke(NewPen(DotColor.Fade(Alpha - 0.1), 0.8));
    end;

    // Фоновая сетка для шкалы (светлые горизонтальные линии)


    // Горизонтальные линии
    for i := 1 to 3 do
    begin
      Surface.MoveTo(VizRect.Left, VizRect.Top + (VizRect.Height div 4) * i);
      Surface.LineTo(VizRect.Right, VizRect.Top + (VizRect.Height div 4) * i);
    end;
    Surface.Stroke(NewPen(DotColor.Fade(0.1), 0.5));
  end;

  // render select
  if dsSelected in State then
  begin
    SelR := Rect;
    SelR.Bottom := SelR.Bottom - 4;
    SelR.Top := SelR.Top + 4;
    SelR.Left := +4;
    SelR.Right := SelR.Right - 4;
    FillRectSelected(Surface, SelR, 2);
  end;

  // render text
  R := Rect;
  R.Bottom := R.Bottom - 18;
  R.Top := R.Top + 4;
  R.Left := R.Left + 76;
  R.Right := VizRect.Left - 6;
  F := Theme.Font;
  F.Color := clMenuText;
  {$ifdef linux}
  F.SetSize(11);
  {$else}
  F.SetSize(20);
  {$endif}
  F.Style := [];

  // Использование данных из интерфейса
  if (Index >= 0) and (Index <= High(FRadioDownloads)) then
  begin
    Surface.TextOut(F, FRadioDownloads[Index].Title, R, drLeft);
    C := clMenuText;
    F.Color := C.Blend(clWindow, 0.25);
    F.Style := [fsItalic];

    {$ifdef linux}
    F.SetSize(8);
    {$else}
    F.SetSize(16);
    {$endif}
    R.Top := R.Bottom - 12;
    Surface.TextOut(F, FRadioDownloads[Index].Tooltip, R, drLeft);
  end;
end;

procedure TRadioList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not (WS_HSCROLL or WS_VSCROLL);
end;

procedure TRadioList.RecreatePictures;
var
  i, j: integer;
  s: String;
  svg: TBGRASVG;
  ms: TMemoryStream;
  bmp: TBGRABitmap;
begin
  // Освобождаем старые картинки
  for i := 0 to High(FListPictures) do
    if Assigned(FListPictures[i]) then
      FListPictures[i].Free;

  // Убедимся, что массив картинок правильного размера
  SetLength(FListPictures, Length(FRadioDownloads));

  // Создаем новые картинки в соответствии с отсортированным массивом
  for i := 0 to High(FRadioDownloads) do
  begin
    ms := TMemoryStream.Create;
    bmp := TBGRABitmap.Create;

    try
      s := FRadioDownloads[i].SvgOutline;
      if s <> '' then
      begin
        ms.WriteBuffer(s[1], Length(s));
        ms.Position := 0;

        svg := TBGRASVG.Create(ms);
        try
          svg.ContainerWidthAsPixel := 64;
          svg.ContainerHeightAsPixel := 64;

          bmp.SetSize(64, 64);
          bmp.Fill(clMenuBar);

          for j := 0 to svg.Content.ElementCount - 1 do
          begin
            if svg.Content.IsSVGElement[j] then
              svg.Content.Element[j].fillColor := clMenuText;
          end;
          svg.StretchDraw(bmp.Canvas2D, 0, 0, bmp.Width, bmp.Height);

          FListPictures[i] := TSurfaceBitmap.Create;
          FListPictures[i].Assign(bmp.Bitmap);
        finally
          svg.Free;
        end;
      end;
    finally
      ms.Free;
      bmp.Free;
    end;
  end;
end;

function TRadioList.NodeLoadFromUrl(const Url: string; Node: TJsonNode): Boolean;
var
  M: TMemoryStream;
  http: TFPHTTPClient;
  Load: Boolean;
begin
  M := TMemoryStream.Create;
  http := TFPHTTPClient.Create(nil);
  http.AllowRedirect := True;
  Load := False;

  try
    try
      http.HTTPMethod('HEAD', URL, nil, []);
      http.Get(URL, M);
      M.Position := 0;
      Node.LoadFromStream(M);
      Load := True;
    except
      on E: Exception do
      begin
        if http.ResponseStatusCode > 399 then
        begin
          WriteLn(SysUtils.Format('Status: %d', [http.ResponseStatusCode]));
          Load := False;
        end;
        WriteLn('Error: ' + E.Message);
        Load := False;
      end;
    end;
  finally
    FreeAndNil(M);
    http.Free;
    Result := Load;
  end;
end;

procedure TRadioList.FillRadioList;
var
  i: integer;
begin
  if not Assigned(FRadioNode) then
    Exit;

  try
    SetLength(FRadioDownloads, FRadioNode.Find('result/stations').Count);
    SetLength(FListPictures, FRadioNode.Find('result/stations').Count);

    for i := 0 to FRadioNode.Find('result/stations').Count - 1 do
    begin
      // Создаем интерфейс напрямую из данных JSON
      FRadioDownloads[i] := NewRadioDownload(
        Trunc(FRadioNode.Find(StatioPath + IntToStr(i) + '/id').AsNumber),
        FRadioNode.Find(StatioPath + IntToStr(i) + '/prefix').AsString,
        FRadioNode.Find(StatioPath + IntToStr(i) + '/title').AsString,
        FRadioNode.Find(StatioPath + IntToStr(i) + '/tooltip').AsString,
        Trunc(FRadioNode.Find(StatioPath + IntToStr(i) + '/sort').AsNumber),
        FRadioNode.Find(StatioPath + IntToStr(i) + '/bg_image').AsString,
        FRadioNode.Find(StatioPath + IntToStr(i) + '/bg_image_mobile').AsString,
        FRadioNode.Find(StatioPath + IntToStr(i) + '/svg_outline').AsString,
        FRadioNode.Find(StatioPath + IntToStr(i) + '/svg_fill').AsString,
        FRadioNode.Find(StatioPath + IntToStr(i) + '/short_title').AsString,
        FRadioNode.Find(StatioPath + IntToStr(i) + '/new').AsBoolean,
        FRadioNode.Find(StatioPath + IntToStr(i) + '/stream_64').AsString,
        FRadioNode.Find(StatioPath + IntToStr(i) + '/stream_128').AsString,
        FRadioNode.Find(StatioPath + IntToStr(i) + '/stream_320').AsString,
        FRadioNode.Find(StatioPath + IntToStr(i) + '/updated').AsString
      );
    end;
  finally
    Count := Length(FRadioDownloads);
    // Создаем картинки после заполнения массива радиостанций
    RecreatePictures;
    Invalidate;
  end;
end;

procedure TRadioList.InitRadiolist;
begin
  if NodeLoadFromUrl('https://www.radiorecord.ru/api/stations/', FRadioNode) then
  begin
    FillRadioList;
    Enabled := True;
  end;
end;

procedure TRadioList.SortByName;
var
  List: TRadioDownloadList;
  i: Integer;
begin
  List.Length := Length(FRadioDownloads);

  for i := 0 to High(FRadioDownloads) do
    List[i] := FRadioDownloads[i];

  List.Sort(soAscend, RadioDownloadCompare(tagName));

  for i := 0 to High(FRadioDownloads) do
  begin
    FRadioDownloads[i] := List[i];
    if FRadioDownloads[i].Id = FPlayID then FItemIndex := i;
  end;

  RecreatePictures;
  Invalidate;
end;

procedure TRadioList.SortBySort;
var
  List: TRadioDownloadList;
  i: Integer;
begin
  List.Length := Length(FRadioDownloads);
  for i := 0 to High(FRadioDownloads) do
    List[i] := FRadioDownloads[i];

  List.Sort(soAscend, RadioDownloadCompare(tagSort));

  for i := 0 to High(FRadioDownloads) do
  begin
    FRadioDownloads[i] := List[i];
    if FRadioDownloads[i].Id = FPlayID then FItemIndex := i;
  end;

  RecreatePictures;
  Invalidate;
end;

procedure TRadioList.SortByUpdated;
var
  List: TRadioDownloadList;
  i: Integer;
begin
  List.Length := Length(FRadioDownloads);
  for i := 0 to High(FRadioDownloads) do
    List[i] := FRadioDownloads[i];

  List.Sort(soAscend, RadioDownloadCompare(tagUpdated));

  for i := 0 to High(FRadioDownloads) do
  begin
    FRadioDownloads[i] := List[i];
    if FRadioDownloads[i].Id = FPlayID then FItemIndex := i;
  end;

  RecreatePictures;
  Invalidate;
end;

procedure TRadioList.UpdateFFTData(const NewFFTData: array of Double);
var
  i: Integer;
  Gravity, Damping, TargetHeight: Double;
begin
  Gravity := 0.015; // Сила гравитации
  Damping := 0.92;  // Затухание скорости

  for i := 0 to Min(7, High(NewFFTData)) do
  begin
    FFFTData[i] := EnsureRange(NewFFTData[i], 0, 1);

    // Целевая высота прыжка (чем громче звук, тем выше прыжок)
    TargetHeight := 1.0 - FFFTData[i];

    // Если звук достаточно громкий, даем точке импульс вверх
    if FFFTData[i] > 0.2 then
    begin
      // Импульс пропорционален громкости
      FDotVelocities[i] := FDotVelocities[i] - (FFFTData[i] * 0.15);
    end;

    // Применяем гравитацию
    FDotVelocities[i] := FDotVelocities[i] + Gravity;

    // Обновляем позицию
    FDotPositions[i] := FDotPositions[i] + FDotVelocities[i];

    // Ограничиваем позицию снизу
    if FDotPositions[i] > 1.0 then
    begin
      FDotPositions[i] := 1.0;
      // Отскок от нижней границы
      FDotVelocities[i] := -FDotVelocities[i] * 0.5;
    end;

    // Ограничиваем позицию сверху
    if FDotPositions[i] < 0.0 then
    begin
      FDotPositions[i] := 0.0;
      FDotVelocities[i] := 0.0;
    end;

    // Затухание скорости для плавности
    FDotVelocities[i] := FDotVelocities[i] * Damping;
  end;

  InvalidateItem(FItemIndex);
end;

function TRadioList.GetRadioDownload(Index: Integer): IRadioDownload;
begin
  if (Index >= 0) and (Index <= High(FRadioDownloads)) then
    Result := FRadioDownloads[Index]
  else
    Result := nil;
end;

function TRadioList.GetSelectedRadioDownload: IRadioDownload;
begin
  if (FItemIndex >= 0) and (FItemIndex <= High(FRadioDownloads)) then
    Result := FRadioDownloads[FItemIndex]
  else
    Result := nil;
end;

function TRadioList.GetSelectedStreamUrl: string;
var
  RadioDownload: IRadioDownload;
begin
  RadioDownload := GetSelectedRadioDownload;
  if Assigned(RadioDownload) then
    Result := RadioDownload.Stream320
  else
    Result := '';
end;

end.
