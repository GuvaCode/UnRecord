unit Radio.Linux;

interface
{$WARNINGS OFF}
{$HINTS OFF}
{$IFDEF Linux}
uses
  Radio,
  Radio.Shared,
  Types,
  Forms,
  Bass;

type
  TPlatformRadio = class(TCustomRadio)
  private
    FStreamURL            : string;
    FActiveChannel        : HSTREAM;
    FStatusProc           : TStatusProc;
    FBroadcastInfoProc    : TBroadcastInfoProc;
    FBroadcastMetaProc    : TBroadcastMetaProc;
    FPauseOnIncomingCalls : Boolean;
    FVolume               : Single;
    procedure DoMeta();

  public
    procedure SetVolume(const AValue:Single); override;
    function  GetVolume: Single;override;
    function  Play:Boolean;override;
    procedure Pause;override;
    procedure SetStreamURL(AUrl : string); override;
    procedure InitRadio(iHandle:THandle);override;
    procedure UnloadRadio;override;

    procedure SetStatusProc(AProc:TStatusProc);override;
    procedure SetBroadcastInfoProc(AProc:TBroadcastInfoProc);override;
    procedure SetBroadcastMetaProc(AProc:TBroadcastMetaProc);override;

    procedure PauseRadioOnIncomingCalls(APauseOnIncomingCalls:Boolean);override;
  end;


{$ENDIF}
implementation
{TPlatformRadio }
{$IFDEF Linux}
var
  ARadio : TPlatformRadio;

procedure MetaSync(handle: HSYNC; channel, data: DWORD; user: Pointer);cdecl;
begin
  if Assigned(ARadio) then ARadio.DoMeta();
end;


function TPlatformRadio.GetVolume: Single;
begin
 if BASS_ChannelGetAttribute(FActiveChannel, BASS_ATTRIB_VOL, FVolume) then
 Result := FVolume else
 Result := FVolume;
end;

procedure TPlatformRadio.SetVolume(const AValue:Single);
begin
  BASS_ChannelSetAttribute(FActiveChannel, BASS_ATTRIB_VOL, AValue);
  FVolume:=AValue;
end;

procedure TPlatformRadio.DoMeta();
var
  meta: MarshaledAString;
  line: string;
  p: Integer;
begin
  meta := BASS_ChannelGetTags(FActiveChannel, BASS_TAG_META);
  if (meta <> nil) then
  begin
    line:=UTF8Decode(meta);
    p := Pos('StreamTitle=', line);
    if (p = 0) then  Exit;
    p := p + 13;
    if Assigned(FBroadcastMetaProc)
      then FBroadcastMetaProc(Copy(meta, p, Pos(';', line) - p - 1));
  end;
end;

function TPlatformRadio.Play:Boolean;
var
  szBroadcastName    : string;
  szBroadcastBitRate : string;
  icy                : MarshaledAString;
  len, Progress      : DWORD;

begin
  Result := false;
  BASS_StreamFree(FActiveChannel);
  Progress := 0;
  FActiveChannel := BASS_StreamCreateURL(PChar(FStreamURL),
                                         0,
                                         BASS_STREAM_BLOCK or
                                         BASS_STREAM_STATUS or
                                         BASS_STREAM_AUTOFREE,
                                         nil,
                                         nil);
  if FActiveChannel = 0 then
  begin
    Result := false;
    Exit;
  end;
   SetVolume(FVolume);// Chanel Volume
  begin
    // Progress
    repeat
      len := BASS_StreamGetFilePosition(FActiveChannel, BASS_FILEPOS_END);
      if (len = DW_Error)
        then begin
                break;
             end;
      application.ProcessMessages;
      Progress := BASS_StreamGetFilePosition(FActiveChannel, BASS_FILEPOS_BUFFER) * 100 div len;

      if Assigned(FStatusProc) then FStatusProc(strLoading,Progress, FActiveChannel);

    until (Progress > 75) or (BASS_StreamGetFilePosition(FActiveChannel, BASS_FILEPOS_CONNECTED) = 0);

    icy := BASS_ChannelGetTags(FActiveChannel, BASS_TAG_ICY);
    if (icy = nil) then icy := BASS_ChannelGetTags(FActiveChannel, BASS_TAG_HTTP);

    szBroadcastName := strUnknown;
    szBroadcastBitRate := strUnknown;

    if (icy <> nil)
    then begin
            while (icy^ <> #0) do
            begin
              if (Copy(icy, 1, 9) = 'icy-name:')
                then begin
                        szBroadcastName := Copy(icy, 10, MaxInt);
                     end
              else if (Copy(icy, 1, 7) = 'icy-br:')
                then begin
                        szBroadcastBitRate := 'bitrate: ' + Copy(icy, 8, MaxInt);
                     end;
              icy := icy + Length(icy) + 1;
            end;

           if Assigned(FBroadcastInfoProc)
            then begin
                     FBroadcastInfoProc(szBroadcastName,szBroadcastBitRate);
                 end;
         end;

    DoMeta();

  BASS_ChannelSetSync(FActiveChannel, BASS_SYNC_META, 0, @MetaSync, nil);

  BASS_ChannelPlay(FActiveChannel, FALSE);

  FStatusProc(strCompleted,100,FActiveChannel);
    Result := True;
  end;
end;

procedure TPlatformRadio.Pause;
begin
  if FActiveChannel<>0 then BASS_ChannelStop(FActiveChannel);
end;

procedure TPlatformRadio.SetStreamURL(AUrl : string);
begin
 FStreamURL := AUrl;
end;

procedure TPlatformRadio.SetStatusProc(AProc:TStatusProc);
begin
 FStatusProc := AProc;
end;

procedure TPlatformRadio.SetBroadcastInfoProc(AProc:TBroadcastInfoProc);
begin
  FBroadcastInfoProc := AProc;
end;

procedure TPlatformRadio.SetBroadcastMetaProc(AProc:TBroadcastMetaProc);
begin
  FBroadcastMetaProc := AProc;
end;

procedure TPlatformRadio.PauseRadioOnIncomingCalls(APauseOnIncomingCalls:Boolean);
begin
  FPauseOnIncomingCalls := APauseOnIncomingCalls;
  //MessageBox(0,'This Method Doesnt Working On Windows OS','Error!',MB_OK OR MB_ICONINFORMATION);
end;

procedure TPlatformRadio.InitRadio(iHandle:THandle);
begin
    FActiveChannel     := 0;
    FStatusProc        := nil;
    FBroadcastInfoProc := nil;
    FBroadcastMetaProc := nil;
    FVolume            :=0.5;
  if BASS_Init(-1,
               44100,
               0,
               nil,      //iHandle
               nil)
  then begin
          BASS_PluginLoad(PChar('libbass_aac.so'), 0);
          BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1);
          BASS_SetConfig(BASS_CONFIG_NET_PREBUF, 0);
        end;
    ARadio := Self;
end;

procedure TPlatformRadio.UnloadRadio;
begin
  if FActiveChannel<>0 then BASS_StreamFree(FActiveChannel);
end;


{$ENDIF}
end.

