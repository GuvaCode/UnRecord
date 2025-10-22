unit RenderInfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  JsonTools, fphttpclient, opensslsockets, math,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls.Extras;

const
  StatioImagePath = 'result/history/0/';

type
  TLoadInfoThread = class;

  { TRenderInfo }

  TRenderInfo = class(TRenderBox)
  private
    FImage: TSurfaceBitmap;
    FNoCoverImage: TSurfaceBitmap;
    FTrackName: String;
    FArtistName: String;
    FCurrentArtist: String;
    FLoadingThread: TLoadInfoThread;
    FIsLoading: Boolean;
    FFFTData: array[0..7] of Double; // Данные для эквалайзера
    procedure SetImage(AValue: TSurfaceBitmap);
    procedure SetNoCoverImage(AValue: TSurfaceBitmap);
    procedure SetArtistName(AValue: String);
    procedure SetTrackName(AValue: String);
    procedure LoadImageFromUrl(const Url: string);
    procedure OnLoadComplete(Sender: TObject);
  protected
     procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetStationInfoId(Id: Integer);
    procedure ClearInfo;
    procedure UpdateEQData(const NewFFTData: array of Double);
  published
    property NoCoverImage: TSurfaceBitmap read FNoCoverImage write SetNoCoverImage;
    property ArtistName: String read FArtistName write SetArtistName;
    property TrackName: String read FTrackName write SetTrackName;
  end;

  { TLoadInfoThread }

  TLoadInfoThread = class(TThread)
  private
    FOwner: TRenderInfo;
    FId: String;
    FArtistName: String;
    FTrackName: String;
    FImageUrl: String;
    FSuccess: Boolean;
    FError: String;
  protected
    procedure Execute; override;
    procedure SyncOnComplete;
  public
    constructor Create(AOwner: TRenderInfo; AId: String);
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Codebot',[TRenderInfo]);
end;

{ TLoadInfoThread }

constructor TLoadInfoThread.Create(AOwner: TRenderInfo; AId: String);
begin
  inherited Create(True); // Create suspended
  FOwner := AOwner;
  FId := AId;
  FreeOnTerminate := True;
end;

procedure TLoadInfoThread.Execute;
var
  RadioNode: TJsonNode;
  M: TMemoryStream;
  http: TFPHTTPClient;
  UrlStr: String;
begin
  FSuccess := False;
  RadioNode := TJsonNode.Create;
  M := TMemoryStream.Create;
  http := TFPHTTPClient.Create(nil);

  try
    try
      http.AllowRedirect := True;
      http.ConnectTimeout := 5000; // 5 секунд таймаут
      http.IOTimeout := 5000;

      UrlStr := 'https://www.radiorecord.ru/api/station/history/?id=' + FId;
      http.Get(UrlStr, M);
      M.Position := 0;
      RadioNode.LoadFromStream(M);

      FArtistName := RadioNode.Find(StatioImagePath + 'artist').AsString;
      FTrackName := RadioNode.Find(StatioImagePath + 'song').AsString;
      FImageUrl := RadioNode.Find(StatioImagePath + 'image100').AsString;

      FSuccess := True;
    except
      on E: Exception do
      begin
        FError := E.Message;
        FSuccess := False;
      end;
    end;
  finally
    http.Free;
    M.Free;
    RadioNode.Free;
  end;

  // Синхронизируем завершение с главным потоком
  Synchronize(@SyncOnComplete);
end;

procedure TLoadInfoThread.SyncOnComplete;
begin
  if Assigned(FOwner) then
    FOwner.OnLoadComplete(Self);
end;

{ TRenderInfo }

procedure TRenderInfo.SetImage(AValue: TSurfaceBitmap);
begin
  if FImage = AValue then Exit;
  FImage.Assign(AValue);
  Invalidate;
end;

procedure TRenderInfo.SetNoCoverImage(AValue: TSurfaceBitmap);
begin
  if FNoCoverImage = AValue then Exit;
  FNoCoverImage.Assign(AValue);
  Invalidate;
end;

procedure TRenderInfo.SetArtistName(AValue: String);
begin
  if FArtistName = AValue then Exit;
  FArtistName := AValue;
  Invalidate;
end;

procedure TRenderInfo.SetTrackName(AValue: String);
begin
  if FTrackName = AValue then Exit;
  FTrackName := AValue;
  Invalidate;
end;

procedure TRenderInfo.LoadImageFromUrl(const Url: string);
var
  http: TFPHTTPClient;
  Stream: TMemoryStream;
begin
  if not Assigned(FImage) then
    FImage := TSurfaceBitmap.Create;

  http := TFPHTTPClient.Create(nil);
  Stream := TMemoryStream.Create;
  try
    try
      http.AllowRedirect := True;
      http.ConnectTimeout := 5000;
      http.IOTimeout := 5000;
      http.Get(Url, Stream);
      Stream.Position := 0;
      FImage.LoadFromStream(Stream);
    except
      on E: Exception do
      begin
        // Если не удалось загрузить изображение, используем NoCoverImage
        if Assigned(FNoCoverImage) then
          FImage.Assign(FNoCoverImage)
        else
        begin
          FImage.SetSize(100, 100);
          FImage.Surface.Fill(NewBrush(clGray));
        end;
      end;
    end;
  finally
    Stream.Free;
    http.Free;
  end;
  Invalidate;
end;

procedure TRenderInfo.OnLoadComplete(Sender: TObject);
var
  Thread: TLoadInfoThread;
begin
  Thread := TLoadInfoThread(Sender);
  FIsLoading := False;

  if Thread.FSuccess then
  begin
    // Проверяем, изменился ли исполнитель
    if Thread.FArtistName <> FCurrentArtist then
    begin
      FArtistName := Thread.FArtistName;
      FTrackName := Thread.FTrackName;
      FCurrentArtist := Thread.FArtistName;

      // Загружаем изображение в основном потоке
      if Thread.FImageUrl <> '' then
        LoadImageFromUrl(Thread.FImageUrl)
      else if Assigned(FNoCoverImage) then
        FImage.Assign(FNoCoverImage);
    end
    else
    begin
      // Если исполнитель не изменился, только обновляем текст
      FArtistName := Thread.FArtistName;
      FTrackName := Thread.FTrackName;
    end;
  end
  else
  begin
    // Обработка ошибки
    FArtistName := 'Ошибка загрузки';
    FTrackName := Thread.FError;
    if Assigned(FNoCoverImage) then
      FImage.Assign(FNoCoverImage);
  end;

  Invalidate;
end;

procedure TRenderInfo.Render;
var
  Rect: TRectI;
  ImageRect, TextRect, ArtistRect, TrackRect, EqRect, BandRect, MirrorRect: TRectI;
  CurrentImage: TSurfaceBitmap;
  F: IFont;
  ImageSize: Integer;
  TextLeft: Integer;
  Padding: Integer;
  i: Integer;
  BarWidth, BarHeight: Integer;
  G: IGradientBrush;
  EqColor: TColorB;
  EqColors: array[0..7] of TColorB;
  TextHeight: Integer;
begin
  inherited Render;

  Rect := ClientRect;
  Padding := 4; // Отступы со всех сторон

  // Учитываем отступы
  Rect.Inflate(-Padding, -Padding);

  // Определяем какое изображение использовать
  if Assigned(FImage) and not FImage.Empty then
    CurrentImage := FImage
  else if Assigned(FNoCoverImage) and not FNoCoverImage.Empty then
    CurrentImage := FNoCoverImage
  else
    CurrentImage := nil;

  // Размер изображения (квадратное, максимум 100x100)
  ImageSize := Min(Min(Rect.Height, Rect.Width - 150), 100); // Оставляем место для текста

  // Область для эквалайзера (справа) - увеличиваем ширину для более широких полос
  EqRect := Rect;
  EqRect.Left := EqRect.Right - 100; // Увеличиваем ширину области эквалайзера
  EqRect.Top := EqRect.Top + 8;
  EqRect.Bottom := EqRect.Bottom - 8;

  if Assigned(CurrentImage) then
  begin
    // Позиционируем изображение по центру слева
    ImageRect := TRectI.Create(
      Rect.Left,
      Rect.Top + (Rect.Height - ImageSize) div 2,
      ImageSize,
      ImageSize
    );

    // Масштабируем изображение с сохранением пропорций
    CurrentImage.Draw(Surface, CurrentImage.ClientRect, ImageRect);
  end;

  // Позиция для текста (справа от изображения, слева от эквалайзера)
  TextLeft := Rect.Left + ImageSize + 10;

  // Область для текста
  TextRect := Rect;
  TextRect.Left := TextLeft;
  TextRect.Right := EqRect.Left - 6; // Отступ от эквалайзера
  TextRect.Top := Rect.Top;
  TextRect.Bottom := Rect.Bottom;

  // Высота текстовой области
  TextHeight := TextRect.Height;

  // Область для артиста - по центру
  ArtistRect := TextRect;
  ArtistRect.Bottom := ArtistRect.Top + TextHeight div 2;
  ArtistRect.Top := ArtistRect.Top + (TextHeight - (TextHeight div 2 + TextHeight div 3)) div 2;

  // Область для названия трека - под артистом
  TrackRect := TextRect;
  TrackRect.Top := ArtistRect.Bottom - 4;
  TrackRect.Bottom := TrackRect.Top + TextHeight div 3;

  // Исполнитель - по центру
  if FArtistName <> '' then
  begin
    F := Theme.Font;
    F.Color := clMenuText;
    F.Style := [fsBold];
    {$ifdef linux}
    F.SetSize(10);
    {$else}
    F.SetSize(14);
    {$endif}
    Surface.TextOut(F, FArtistName, ArtistRect, DrLeft);
  end;

  // Название трека - по центру под артистом
  if FTrackName <> '' then
  begin
    F := Theme.Font;
    F.Style := [fsItalic];
    F.Color := clMenuText;

    {$ifdef linux}
    F.SetSize(10);
    {$else}
    F.SetSize(14);
    {$endif}
    Surface.TextOut(F, FTrackName, TrackRect, DrLeft);
  end;

  // Отрисовка эквалайзера
  EqColor := clMenuText;
  EqColor := EqColor.Blend(clWindow, 0.25);

  For i := 0 to 7 do EqColors[i] := EqColor;

  BarWidth := 12; // Увеличиваем ширину полос в 2 раза (было 6)

  for i := 2 to 7 do
  begin
    // Вычисляем высоту полосы на основе FFT данных
    BarHeight := Round(EnsureRange(FFFTData[i], 0, 1) * (EqRect.Height - 4));
    if BarHeight < 2 then BarHeight := 2;

    // Создаем прямоугольник для полосы
    BandRect := EqRect;
    BandRect.Left := EqRect.Left + (i - 2) * (BarWidth + 3); // Увеличиваем расстояние между полосами
    BandRect.Right := BandRect.Left + BarWidth;
    BandRect.Top := BandRect.Bottom - BarHeight;

    // Градиентная заливка
    G := NewBrush(BandRect.Left, BandRect.Bottom, BandRect.Left, BandRect.Top);
    G.AddStop(EqColors[i].Fade(0.6), 0);
    G.AddStop(EqColors[i].Fade(0.2), 1);

    if BarHeight > 0 then
      Surface.FillRoundRect(G, BandRect, 2); // Увеличиваем радиус скругления

    // Зеркальное отражение
    if BarHeight > 2 then
    begin
      MirrorRect := BandRect;
      MirrorRect.Top := BandRect.Bottom + 2;
      MirrorRect.Bottom := MirrorRect.Top + Round(BarHeight * 0.2);

      G := NewBrush(MirrorRect.Left, MirrorRect.Top, MirrorRect.Left, MirrorRect.Bottom);
      G.AddStop(EqColors[i].Fade(0.3), 0);
      G.AddStop(EqColors[i].Fade(0.1), 1);

      Surface.FillRoundRect(G, MirrorRect, 2); // Увеличиваем радиус скругления
    end;
  end;
end;

procedure TRenderInfo.GetStationInfoId(Id: Integer);
begin
  // Если уже идет загрузка, не запускаем новую
  if FIsLoading then
    Exit;

  FIsLoading := True;

  // Запускаем поток для загрузки данных
  FLoadingThread := TLoadInfoThread.Create(Self, IntToStr(Id));
  FLoadingThread.Start;

  Invalidate; // Обновляем отображение для показа "Загрузка..."
end;

procedure TRenderInfo.ClearInfo;
begin
  // Останавливаем текущую загрузку если есть
  if FIsLoading and Assigned(FLoadingThread) then
  begin
    FLoadingThread.Terminate;
    FIsLoading := False;
  end;

  FArtistName := '';
  FTrackName := '';
  FCurrentArtist := '';
  if Assigned(FImage) then
  begin
    if Assigned(FNoCoverImage) then
      FImage.Assign(FNoCoverImage)
    else
      FImage.SetSize(0, 0);
  end;
  Invalidate;
end;

procedure TRenderInfo.UpdateEQData(const NewFFTData: array of Double);
var
  i: Integer;
begin
  for i := 0 to Min(7, High(NewFFTData)) do
    FFFTData[i] := NewFFTData[i];
  Invalidate;
end;

constructor TRenderInfo.Create(AOwner: TComponent);
var
  F: IFont;
  i: Integer;
begin
  inherited Create(AOwner);
  FImage := TSurfaceBitmap.Create;
  FNoCoverImage := TSurfaceBitmap.Create;
  FArtistName := '';
  FTrackName := '';
  FCurrentArtist := '';
  FIsLoading := False;

  // Инициализируем FFT данные
  for i := 0 to 7 do
    FFFTData[i] := 0.1;

  // Создаем изображение "нет обложки" по умолчанию
  FNoCoverImage.SetSize(100, 100);
  // Заливаем серым цветом
  FNoCoverImage.Surface.Fill(NewBrush(clGray));
  // Добавляем текст "No Cover"
  F := NewFont('Arial', 12);
  F.Color := clWhite;
  FNoCoverImage.Surface.TextOut(F, 'No Cover', FNoCoverImage.ClientRect, drCenter);

  FImage.Assign(FNoCoverImage);
end;

destructor TRenderInfo.Destroy;
begin
  // Останавливаем поток при уничтожении
  if FIsLoading and Assigned(FLoadingThread) then
  begin
    FLoadingThread.Terminate;
    FLoadingThread.WaitFor;
  end;

  FImage.Free;
  FNoCoverImage.Free;
  inherited Destroy;
end;

end.
