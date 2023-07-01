unit Radio;

{$WARNINGS OFF}
{$HINTS OFF}

interface
uses Radio.Shared, Classes, SysUtils, Forms;


type
  TCustomRadio = class(TObject)
    procedure InitRadio(iHandle:THandle);virtual;abstract;
    procedure UnloadRadio;virtual;abstract;
    procedure SetVolume(const AValue:Single);  virtual; abstract;
    function  GetVolume: Single; virtual;abstract;
    function  Play:Boolean;virtual;abstract;
    procedure Pause;virtual;abstract;
    procedure SetStreamURL(AUrl : string);virtual;abstract;
    procedure SetStatusProc(AProc:TStatusProc);virtual;abstract;
    procedure SetBroadcastInfoProc(AProc:TBroadcastInfoProc);virtual;abstract;
    procedure SetBroadcastMetaProc(AProc:TBroadcastMetaProc);virtual;abstract;
    procedure PauseRadioOnIncomingCalls(APauseOnIncomingCalls:Boolean);virtual;abstract;
  end;


  TRadio = class(TCustomRadio)
  private
    FPlatformRadio: TCustomRadio;
    FOwner : TObject;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitRadio(iHandle:THandle);override;
    procedure UnloadRadio;override;

    procedure SetOwner(AOwner : TObject);
    procedure SetVolume(const AValue:Single); override;
    function  GetVolume: Single;override;

    function  Play:Boolean;override;
    procedure Pause;override;
    procedure SetStreamURL(AUrl : string);override;

    procedure SetStatusProc(AProc:TStatusProc);override;
    procedure SetBroadcastInfoProc(AProc:TBroadcastInfoProc);override;
    procedure SetBroadcastMetaProc(AProc:TBroadcastMetaProc);override;
    procedure PauseRadioOnIncomingCalls(APauseOnIncomingCalls:Boolean);override;
  end;

implementation
uses
{$IFDEF MSWINDOWS}
  Radio.Windows;
{$ENDIF}
{$IFDEF LINUX}
  Radio.Linux;
{$ENDIF}

{ TRadio }
constructor TRadio.Create;
begin
  inherited;
  FPlatformRadio := TPlatformRadio.Create;
end;

procedure TRadio.SetOwner(AOwner : TObject);
begin
  FOwner := AOwner;
  InitRadio(TForm(FOwner).Handle);
end;

destructor TRadio.Destroy;
begin
  UnloadRadio;
  FPlatformRadio.Free;
  inherited;
end;

procedure TRadio.SetVolume(const AValue:Single);
begin
   FPlatformRadio.SetVolume(AValue);
end;

function  TRadio.GetVolume: Single;
begin
   Result := FPlatformRadio.GetVolume;
end;

function TRadio.Play:Boolean;
begin
   Result := FPlatformRadio.Play;
end;

procedure TRadio.Pause;
begin
   FPlatformRadio.Pause;
end;

procedure TRadio.SetStreamURL(AUrl : string);
begin
   FPlatformRadio.SetStreamURL(AUrl);
end;

procedure TRadio.SetStatusProc(AProc:TStatusProc);
begin
  if Assigned(AProc) then FPlatformRadio.SetStatusProc(AProc);
end;

procedure TRadio.SetBroadcastInfoProc(AProc:TBroadcastInfoProc);
begin
  if Assigned(AProc) then FPlatformRadio.SetBroadcastInfoProc(AProc);
end;

procedure TRadio.SetBroadcastMetaProc(AProc:TBroadcastMetaProc);
begin
  if Assigned(AProc) then FPlatformRadio.SetBroadcastMetaProc(AProc);
end;

procedure TRadio.PauseRadioOnIncomingCalls(APauseOnIncomingCalls:Boolean);
begin
   FPlatformRadio.PauseRadioOnIncomingCalls(APauseOnIncomingCalls);
end;

procedure TRadio.InitRadio(iHandle:THandle);
begin
  FPlatformRadio.InitRadio(iHandle);
end;

procedure TRadio.UnloadRadio;
begin
  FPlatformRadio.UnloadRadio;
end;

end.


