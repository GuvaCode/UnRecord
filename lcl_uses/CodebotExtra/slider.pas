unit Slider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLProc, LCLIntf,
  GraphType, Graphics, ActnList, Controls, LMessages, Forms,
  Menus,
  Codebot.System,
  Codebot.Controls,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls.Extras;

type
   TOnPosChangedEvent = procedure(Sender: TObject; NewPos: integer) of object;

  { TSlider }
  TSlider = class(TRenderBox)
  private
    FBorderColor: TColorB;
    FBorderRound: Integer;
    FMin: integer;
    FMax: integer;
    FPos: integer;
    FDown: Boolean;
    FBarColor: TColorB;
    FCaption: string;
    FOnPosChanged: TOnPosChangedEvent;
    FSmooth: Boolean;
    FVertical: Boolean;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Render; override;
    procedure SetPosition(Value: integer);
    procedure SetCaption(Value: string);
    procedure SetVertical(Value: Boolean);
    procedure UpdatePosition(X,Y: integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetParams(AMin,AMax,APos: integer);
    property BarColor: TColorB read FBarColor write FBarColor;
    property BorderColor: TColorB read FBorderColor write FBorderColor;
  published
    property Position: integer read FPos write SetPosition;
    property Vertical: Boolean read FVertical write SetVertical;
    property Smooth: Boolean read FSmooth write FSmooth;
    property BorderRound: Integer read FBorderRound write FBorderRound;
    property PositionChanged: TOnPosChangedEvent read FOnPosChanged write FOnPosChanged;
    property Caption: string read FCaption write SetCaption;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Codebot',[TSlider]);
end;

{ TSlider }

procedure TSlider.SetCaption(Value: string);
begin
if FCaption <> Value then
  begin
    FCaption:= Value;
    Invalidate;
  end;
end;

procedure TSlider.CMEnabledChanged(var Message: TLMessage);
begin
      Invalidate;
end;

procedure TSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then Exit;
  FDown:= True;
  UpdatePosition(X,Y);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FDown then UpdatePosition(X,Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FDown:= False;
  if not FSmooth then
    if Assigned(FOnPosChanged) then
      begin
        FOnPosChanged(Self,FPos);
        Invalidate;
      end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TSlider.Render;
var
  R: TRectI; TempRect: TRectI; C: TColorB;
  i: integer; v: integer; F: IFont;
begin
  R := GetClientRect;
  F := Theme.Font;
  F.Color := clMenuText;
  if Enabled then
     begin
       if FVertical then
         begin
           v:= R.Bottom - R.Top;
           i:= v - Round(v * ((FPos - FMin) / (FMax - FMin)));
           FillRectColor(Surface, TempRect.Create(R.Left,i,R.Right,R.Bottom), FBarColor , FBorderRound);
           StrokeRectColor(Surface, R, FBorderColor , FBorderRound);
         end
       else
         begin
           v:= R.Right - R.Left;
           i:= Round(v * ((FPos - FMin) / (FMax - FMin)));
           C := FBarColor;
           Surface.FillRect(NewBrush(C.Blend(clWindow, 0.75)), TempRect.Create(R.Left,R.Top,i,R.Bottom));
           C := BorderColor;
           Surface.StrokeRect(NewPen(C.Blend(clWindow, 0.25)), R);
         end;
       if FCaption <> '' then
         begin
           Surface.TextOut(F, FCaption, R, drCenter);
         end;
     end;
  inherited Render;
end;

procedure TSlider.SetPosition(Value: integer);
var
  i: integer;
begin
  i:= Value;
  if i < FMin then i:= FMin;
  if i > FMax then i:= FMax;
  if i <> FPos then
  begin
    FPos:= i;
    Invalidate;
  end;
end;

procedure TSlider.SetVertical(Value: Boolean);
begin
  if FVertical <> Value then
    begin
      FVertical:= Value;
      Invalidate;
    end;
end;

procedure TSlider.UpdatePosition(X, Y: integer);
var
  i,h: integer;
  NewPos: integer;
begin
  if FVertical then
    begin
      h:= ClientHeight;
      i:= h - Y;
    end
  else
    begin
      h:= ClientWidth;
      i:= X;
    end;
  if h < 1 then ;
  NewPos:= Round((i / h) * (FMax - FMin) + FMin);
  SetPosition(NewPos);
  if Assigned(FOnPosChanged) then
  if FSmooth then FOnPosChanged(Self,FPos);
end;

constructor TSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse];
  Color:= clBtnFace;
  BarColor:= clYellow;
  BorderColor := clBlack;
  FVertical:= False;
  SetParams(100,1000,50);
end;

procedure TSlider.SetParams(AMin, AMax, APos: integer);
begin
  FMin:= AMin;
  FMax:= AMax;
  Enabled:= (FMin < FMax);
  if Enabled then SetPosition(APos);
end;

end.
