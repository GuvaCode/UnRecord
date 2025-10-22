unit FluentCircleButton;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, Types, Controls, Graphics,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  FluentGraphicControl, FluentAnimation;

type
  TPlayState = (psPlay, psStop);

  { TCustomFluentCircleButton }

  TCustomFluentCircleButton = class(TFluentGraphicControl)
  private
    FImagePlay: TSurfaceBitmap;
    FImageStop: TSurfaceBitmap;
    FNeedRecalc: boolean;
    FButtonState: TRenderState;
    FButtonRect: TRectF;
    FButtonSize: Float;
    FBorderWidth: Float;
    FCircleSize: array [TRenderState] of Float;
    FAnimationSize: Float;
    FAnimationController: TAnimationController;
    FOnClick: TNotifyEvent;
    FChecked: Boolean;
    FCheckable: Boolean;
    FPlayState: TPlayState;

    procedure CalcSizes;
    procedure CalcButtonRect;
    procedure DrawButton;
    procedure OnAnimateValue(AValue: Double);
    procedure SetImagePlay(AValue: TSurfaceBitmap);
    procedure SetImageStop(AValue: TSurfaceBitmap);
    procedure StartAnimation(oldState: TRenderState; onComplete: TOnCompleteEvent = nil);
    procedure StopAnimation;
    procedure SetChecked(AValue: Boolean);
    procedure SetCheckable(AValue: Boolean);
    procedure SetPlayState(AValue: TPlayState);
  protected
    procedure Render; override;
    procedure Resize; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(
                         var PreferredWidth, PreferredHeight: integer;
                         {%H-}WithThemeSpace: Boolean); override;

    property Checked: Boolean read FChecked write SetChecked default false;
    property Checkable: Boolean read FCheckable write SetCheckable default false;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure TogglePlayState;
  published
    property ImagePlay: TSurfaceBitmap read FImagePlay write SetImagePlay;
    property ImageStop: TSurfaceBitmap read FImageStop write SetImageStop;
    property PlayState: TPlayState read FPlayState write SetPlayState default psPlay;
  end;

  TFluentCircleButton = class(TCustomFluentCircleButton)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Checked;
    property Checkable;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property PopupMenu;
    property Visible;
    property OnClick;
    property OnChangeBounds;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property PlayState;
    property ImagePlay;
    property ImageStop;
  end;

implementation

uses
  Utils;

{ TCustomFluentCircleButton }

procedure TCustomFluentCircleButton.CalcSizes;
begin
  FButtonSize := Min(Width, Height) * 0.8; // 80% от минимального размера
  FBorderWidth := 1;// Scale96ToScreen(2);

  // Размеры для разных состояний
  FCircleSize[rsNormal] := FButtonSize * 0.8;
  FCircleSize[rsHover] := FButtonSize  * 1; // Увеличиваем на 10% при наведении
  FCircleSize[rsPressed] := FButtonSize  * 0.7; // Уменьшаем на 10% при нажатии
end;

procedure TCustomFluentCircleButton.CalcButtonRect;
begin
  CalcSizes;
  FButtonRect := TRectF.Create(FButtonSize, FButtonSize);
  FButtonRect.Center(ClientRect.CenterPoint);
end;

procedure TCustomFluentCircleButton.DrawButton;
var
  R: TRectF;
  d: Float;
  BorderColor, FillColor: TColorB;
  ImageRect: TRectF;
  CurrentImage: TSurfaceBitmap;
  G: IGradientBrush;
begin
  // Определяем цвета в зависимости от состояния
  if not Enabled then
  begin
    BorderColor := GetDisabledAccentColorB;
    FillColor := GetDisabledAccentColorB.Lighten(0.3);
  end
  else if FChecked then
  begin
    BorderColor := GetAccentColorB;
    FillColor := GetAccentColorB.Lighten(0.2);
  end
  else
  begin
    BorderColor := GetAccentColorB.Lighten(0.2);
    FillColor := DefWhiteColor;
  end;

  BorderColor := clHighlight;
  // := clHighlight;
  G := NewBrush(0, FButtonRect.Top, 0, FButtonRect.Bottom);
  G.AddStop(BorderColor.Fade(0.4), 0);
  G.AddStop(BorderColor.Fade(0.0), 1);

  // Рисуем внешний круг (границу)
  R := FButtonRect;
  R.Inflate(-FBorderWidth / 2, -FBorderWidth / 2);
  Surface.Ellipse(R);
  Surface.Stroke(NewPen(BorderColor, FBorderWidth));

  // Рисуем внутренний круг (заливку)
  R := FButtonRect;
  R.Inflate(-FBorderWidth, -FBorderWidth);
  Surface.Ellipse(R);
  Surface.Fill(NewBrush(FillColor));

  // Рисуем анимированный круг эффекта
  if FAnimationController.AnimationEnabled then
    d := FAnimationSize
  else
    d := FCircleSize[FButtonState];

  R := TRectF.Create(d, d);
  R.Center(FButtonRect.MidPoint);
  Surface.Ellipse(R);

  // Цвет эффекта зависит от состояния
  if not Enabled then
    Surface.Fill(NewBrush(GetDisabledAccentColorB.Fade(0.3)))
  else if FButtonState = rsPressed then
    //Surface.Fill(NewBrush(GetAccentColorB.Darken(0.1).Fade(0.4)))
    Surface.Fill(g)
  else if FButtonState = rsHover then
  // Surface.Fill(NewBrush(GetAccentColorB))
    Surface.Fill(G)
  else
    //Surface.Fill(NewBrush(GetAccentColorB));
    Surface.Fill(G);

  // Отрисовка изображения по середине кнопки в зависимости от состояния Play/Stop
  case FPlayState of
    psPlay: CurrentImage := FImagePlay;
    psStop: CurrentImage := FImageStop;
  else
    CurrentImage := FImagePlay;
  end;

  if Assigned(CurrentImage) and not CurrentImage.Empty then
  begin
    // Создаем прямоугольник для изображения (50% от размера кнопки)
    ImageRect := TRectF.Create(FButtonSize * 0.5, FButtonSize * 0.5);
    ImageRect.Center(FButtonRect.MidPoint);

    // Рисуем изображение
    CurrentImage.Draw(Surface, CurrentImage.ClientRect, ImageRect);
  end;
end;

procedure TCustomFluentCircleButton.OnAnimateValue(AValue: Double);
begin
  FAnimationSize := AValue;
  Invalidate;
end;

procedure TCustomFluentCircleButton.SetImagePlay(AValue: TSurfaceBitmap);
begin
  if FImagePlay = AValue then Exit;
  FImagePlay.Assign(AValue);
  Invalidate;
end;

procedure TCustomFluentCircleButton.SetImageStop(AValue: TSurfaceBitmap);
begin
  if FImageStop = AValue then Exit;
  FImageStop.Assign(AValue);
  Invalidate;
end;

procedure TCustomFluentCircleButton.SetPlayState(AValue: TPlayState);
begin
  if FPlayState = AValue then Exit;
  FPlayState := AValue;
  Invalidate;
end;

procedure TCustomFluentCircleButton.StartAnimation(oldState: TRenderState;
  onComplete: TOnCompleteEvent = nil);
var
  v1, v2: Float;
begin
  if FAnimationController.AnimationEnabled then
    v1 := FAnimationSize
  else
    v1 := FCircleSize[oldState];

  v2 := FCircleSize[FButtonState];

  if abs(v1 - v2) < 1 then
  begin
    StopAnimation;
    Invalidate;
  end
  else
    FAnimationController.StartAnimation(nil, 150, v1, v2, @OnAnimateValue, false, onComplete);
end;

procedure TCustomFluentCircleButton.StopAnimation;
begin
  FAnimationController.StopAnimation;
end;

procedure TCustomFluentCircleButton.SetChecked(AValue: Boolean);
begin
  if FChecked = AValue then Exit;
  FChecked := AValue;
  Invalidate;
end;

procedure TCustomFluentCircleButton.SetCheckable(AValue: Boolean);
begin
  if FCheckable = AValue then Exit;
  FCheckable := AValue;
  if not FCheckable then
    FChecked := False;
  Invalidate;
end;

procedure TCustomFluentCircleButton.TogglePlayState;
begin
  if FPlayState = psPlay then
    FPlayState := psStop
  else
    FPlayState := psPlay;
  Invalidate;
end;

procedure TCustomFluentCircleButton.Render;
begin
  if FNeedRecalc then
    CalcButtonRect;

  DrawButton;
  inherited;
end;

procedure TCustomFluentCircleButton.Resize;
begin
  CalcButtonRect;
  FNeedRecalc := False;
  inherited Resize;
end;

procedure TCustomFluentCircleButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  inCircle: Boolean;
begin
  inherited;
  pt := TPointF.Create(X, Y);
  inCircle := FButtonRect.Contains(pt);

  if (FButtonState = rsNormal) and inCircle then
  begin
    FButtonState := rsHover;
    StartAnimation(rsNormal);
  end
  else if (FButtonState = rsHover) and (not inCircle) then
  begin
    FButtonState := rsNormal;
    StartAnimation(rsHover);
  end;
end;

procedure TCustomFluentCircleButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;

  if FButtonState = rsHover then
  begin
    FButtonState := rsPressed;
    StartAnimation(rsHover);
  end;
end;

procedure TCustomFluentCircleButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  inherited;
  if Button <> mbLeft then Exit;

  pt := TPointF.Create(X, Y);

  if FButtonState = rsPressed then
  begin
    // Переключаем состояние если кнопка checkable
    if FCheckable then
      Checked := not Checked;

    // Переключаем состояние Play/Stop
    TogglePlayState;

    // Вызываем событие OnClick
    if Assigned(FOnClick) then
      FOnClick(Self);

    if FButtonRect.Contains(pt) then
      FButtonState := rsHover
    else
      FButtonState := rsNormal;

    StartAnimation(rsPressed);
  end;
end;

procedure TCustomFluentCircleButton.MouseLeave;
begin
  if FButtonState = rsHover then
  begin
    FButtonState := rsNormal;
    StartAnimation(rsHover);
  end;
  inherited MouseLeave;
end;

class function TCustomFluentCircleButton.GetControlClassDefaultSize: TSize;
begin
  Result.cy := 48;
  Result.cx := 48;
end;

procedure TCustomFluentCircleButton.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  DefaultSize: TSize;
begin
  DefaultSize := GetControlClassDefaultSize;
  PreferredWidth := Scale96ToScreen(DefaultSize.cx);
  PreferredHeight := Scale96ToScreen(DefaultSize.cy);
end;

constructor TCustomFluentCircleButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonState := rsNormal;
  FAnimationController := TAnimationController.Create;
  FAnimationSize := 0;
  FChecked := False;
  FCheckable := False;
  FPlayState := psPlay;

  // Создаем изображения
  FImagePlay := TSurfaceBitmap.Create;
  FImageStop := TSurfaceBitmap.Create;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  CalcButtonRect;
  FNeedRecalc := False;
end;

destructor TCustomFluentCircleButton.Destroy;
begin
  FImagePlay.Free;
  FImageStop.Free;
  FAnimationController.Free;
  inherited Destroy;
end;

end.
