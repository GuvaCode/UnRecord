(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2019                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.forms.floating.txt> }
unit Codebot.Forms.Floating;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, LCLIntf, LCLType,
  Codebot.System,
  Codebot.Graphics.Types;

{ TFloatingForm }

type
  TFloatingForm = class(TForm)
  private
    FInteractive: Boolean;
    FWindow: Pointer;
    FOpacity: Byte;
    FFaded: Boolean;
    FFadeTop: Integer;
    FFadeMoved: Boolean;
    function GetCompositing: Boolean;
    procedure SetFaded(Value: Boolean);
    procedure SetInteractive(Value: Boolean);
    procedure SetOpacity(Value: Byte);
  protected
    procedure CreateHandle; override;
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MoveSize(Rect: TRectI);
    property Opacity: Byte read FOpacity write SetOpacity;
    property Compositing: Boolean read GetCompositing;
    property Interactive: Boolean read FInteractive write SetInteractive;
    property Faded: Boolean read FFaded write SetFaded;
  end;

implementation

{$if defined(linux) and defined(lclgtk2)}
uses
  GLib2, Gdk2, Gtk2, Gtk2Def, Gtk2Extra, Gtk2Globals,
  Codebot.Interop.Linux.NetWM;

procedure gdk_window_input_shape_combine_mask(window: PGdkWindow;
  mask: PGdkBitmap; x, y: GInt); cdecl; external gdklib;
function gtk_widget_get_window(widget: PGtkWidget): PGdkWindow; cdecl; external gtklib;
function gdk_window_get_screen(window: PGdkWindow): PGdkScreen; cdecl; external gdklib;
function gdk_screen_is_composited(screen: PGdkScreen): gboolean; cdecl; external gdklib;

procedure FormScreenChanged(widget: PGtkWidget; old_screen: PGdkScreen;
  userdata: GPointer); cdecl;
var
  Screen: PGdkScreen;
  Colormap: PGdkColormap;
begin
  Screen := gtk_widget_get_screen(widget);
  Colormap := gdk_screen_get_rgba_colormap(Screen);
  gtk_widget_set_colormap(widget, Colormap);
end;

{ TFloatingForm }

constructor TFloatingForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  FOpacity := $FF;
  FInteractive := True;
end;

procedure TFloatingForm.Loaded;
type
  PFormBorderStyle = ^TFormBorderStyle;
begin
  PFormBorderStyle(@BorderStyle)^ := bsNone;
  inherited Loaded;
  PFormBorderStyle(@BorderStyle)^ := bsNone;
end;

type
  PFormBorderStyle = ^TFormBorderStyle;

procedure TFloatingForm.CreateHandle;
var
  W: TGtkWindowType;
begin
  PFormBorderStyle(@BorderStyle)^ := bsNone;
  W := FormStyleMap[bsNone];
  FormStyleMap[bsNone] := GTK_WINDOW_POPUP;
  try
    inherited CreateHandle;
  finally
    FormStyleMap[bsNone] := W;
  end;
  if not (csDesigning in ComponentState) then
  begin
    FWindow := {%H-}Pointer(Handle);
    gtk_widget_set_app_paintable(PGtkWidget(FWindow), True);
    g_signal_connect(G_OBJECT(FWindow), 'screen-changed',
      G_CALLBACK(@FormScreenChanged), nil);
    FormScreenChanged(PGtkWidget(FWindow), nil, nil);
  end;
end;

procedure TFloatingForm.SetInteractive(Value: Boolean);
begin
  if FInteractive <> Value then
  begin
    FInteractive := Value;
    Invalidate;
  end;
end;

procedure TFloatingForm.Paint;
var
  Window: PGdkWindow;
  Mask: PGdkPixmap;
begin
  Window := GTK_WIDGET({%H-}Pointer(Handle)).window;
  if FInteractive then
    gdk_window_input_shape_combine_mask(Window, nil, 0, 0)
  else
  begin
    Mask := gdk_pixmap_new(nil, Width, Height, 1);
    gdk_window_input_shape_combine_mask(Window, nil, 0, 0);
    gdk_window_input_shape_combine_mask(Window, Mask, 0, 0);
    g_object_unref(Mask);
  end
end;

procedure TFloatingForm.MoveSize(Rect: TRectI);
var
  Window: PGdkWindow;
begin
  Window := GTK_WIDGET({%H-}Pointer(Handle)).window;
  gdk_window_move_resize(Window, Rect.Left, Rect.Top, Rect.Width, Rect.Height);
end;

procedure TFloatingForm.SetOpacity(Value: Byte);
begin
  if Value <> FOpacity then
  begin
    gtk_window_set_opacity(PGtkWindow(FWindow), Value / $FF);
    FOpacity := Value;
  end;
end;

function TFloatingForm.GetCompositing: Boolean;
var
  Screen: PGdkScreen;
begin
  Screen := gdk_window_get_screen(GTK_WIDGET({%H-}Pointer(Handle)).window);
  Result := gdk_screen_is_composited(screen);
end;

procedure FadeTimer(hWnd: HWND; uMsg: UINT; idEvent: UINT_PTR; dwTime: DWORD); stdcall;
var
  F: TFloatingForm absolute idEvent;
begin
  KillTimer(hWnd, UIntPtr(idEvent));
  F.FFadeTop := F.Top;
  F.FFadeMoved := True;
  F.Top := 30000;
end;

procedure TFloatingForm.SetFaded(Value: Boolean);
begin
  if FFaded <> Value then
  begin
    KillTimer(Handle, UIntPtr(Self));
    if FFadeMoved then
    begin
      FFadeMoved := False;
      Top := FFadeTop;
    end;
    FFaded := Value;
    if WindowManager.Compositing and (WindowManager.Name = 'Compiz') then
      if FFaded then
      begin
        Opacity := 0;
        SetTimer(Handle, UIntPtr(Self), 750, @FadeTimer);
      end
      else
        Opacity := $FF
    else
      Visible := not FFaded;
  end;
end;
{$else}
function TFloatingForm.GetCompositing: Boolean;
begin
  Result := False;
end;

procedure TFloatingForm.SetFaded(Value: Boolean);
begin

end;

procedure TFloatingForm.SetInteractive(Value: Boolean);
begin

end;

procedure TFloatingForm.SetOpacity(Value: Byte);
begin

end;

procedure TFloatingForm.CreateHandle;
begin
  inherited CreateHandle;
end;

procedure TFloatingForm.Loaded;
begin
  inherited Loaded;
end;

procedure TFloatingForm.Paint;
begin
  inherited Paint;
end;

constructor TFloatingForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TFloatingForm.MoveSize(Rect: TRectI);
begin

end;
{$endif}

end.
