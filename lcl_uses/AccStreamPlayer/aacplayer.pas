unit AACPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Pipes, math, httpgetthread, binding.raudio, binding.fdkaacdecoder;

type
  TAACPlayerState = (apsStopped, apsPlaying, apsPaused, apsError);
  TPlayerErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TPlayerStateChangeEvent = procedure(Sender: TObject; OldState, NewState: TAACPlayerState) of object;
  TStreamInfoEvent = procedure(Sender: TObject; SampleRate, Channels, Bitrate: Integer) of object;
  TFFTDataEvent = procedure(Sender: TObject; const FFTData: array of Double; FFTSize: Integer) of object;
  TTrackChangedEvent = procedure(Sender: TObject; const TrackTitle: string) of object;

  { TAACPlayer }

  TAACPlayer = class(TComponent)
  private
    FHttpThread: TThreadHttpGetter;
    FInputStream: TInputPipeStream;
    FOutputStream: TOutputPipeStream;
    FAacDecoder: HANDLE_AACDECODER;
    FAudioStream: TAudioStream;
    FState: TAACPlayerState;
    FBuffer: array of SmallInt;
    FBytesInBuffer: Integer;
    FDecoderReady: Boolean;
    FTotalBytesRead: Integer;
    FOutputBuff: array of SmallInt;
    FAacBuffer: array of Byte;
    FAacBufferSize: Integer;
    FStreamInfo: PCStreamInfo;
    FCriticalSection: TRTLCriticalSection;
    FBufferReadPos: Integer;
    FBufferWritePos: Integer;
    FMinBufferSamples: Integer;
    FTerminating: Boolean;
    FURL: string;
    FVolume: Single;
    FOnError: TPlayerErrorEvent;
    FOnStateChange: TPlayerStateChangeEvent;
    FOnStreamInfo: TStreamInfoEvent;
    FOnBuffering: TNotifyEvent;
    FOnPlay: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FOnFFTData: TFFTDataEvent;
    FOnTrackChanged: TTrackChangedEvent;

    // HTTP thread monitoring
    FHttpThreadRunning: Boolean;

    // Simple FFT for 8 bands
    FFFTEnabled: Boolean;
    FFFTUpdateInterval: Integer;
    FFFTUpdateCounter: Integer;
    FFFTSampleBuffer: array of SmallInt;
    FFFTSamplePos: Integer;
    FFFTBands: array[0..7] of Double;
    FFFTWindow: array of Double;
    FCurrentStreamTitle: String;

    procedure AudioCallback(bufferData: pointer; frames: LongWord);
    function InitializeAacDecoder: Boolean;
    function DecodeAacData: Integer;
    procedure AddToAacBuffer(const AData: array of Byte; ADataSize: Integer);
    function GetPcmSamples(out samples: array of SmallInt; maxSamples: Integer): Integer;
    procedure SetState(AValue: TAACPlayerState);
    procedure SetVolume(AValue: Single);
    procedure SetURL(AValue: string);
    procedure DoError(const ErrorMsg: string);
    procedure DoStateChange(OldState, NewState: TAACPlayerState);
    procedure DoStreamInfo(SampleRate, Channels, Bitrate: Integer);
    procedure DoBuffering;
    procedure DoPlay;
    procedure DoStop;
    procedure DoFFTData(const FFTData: array of Double; FFTSize: Integer);
    procedure DoTrackChanged(const TrackTitle: string);
    procedure HandleMetadataReceived(Sender: TObject; const Metadata: string);
    procedure HttpThreadTerminated(Sender: TObject);

    // Simple FFT methods
    procedure InitializeFFT;
    procedure ProcessSimpleFFT(const Samples: array of SmallInt; SampleCount: Integer);
    procedure Calculate8Bands;
    procedure InitializeBuffers;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play;
    procedure Stop;
    procedure Pause;
    procedure Resume;
    function GetBufferLevel: Integer; // 0-100%
    function GetBitrate: Integer;
    function GetSampleRate: Integer;
    function GetChannels: Integer;

    // FFT properties
    property FFTEnabled: Boolean read FFFTEnabled write FFFTEnabled;
    property FFTUpdateInterval: Integer read FFFTUpdateInterval write FFFTUpdateInterval;
  published
    property URL: string read FURL write SetURL;
    property Volume: Single read FVolume write SetVolume;
    property State: TAACPlayerState read FState;

    property OnError: TPlayerErrorEvent read FOnError write FOnError;
    property OnStateChange: TPlayerStateChangeEvent read FOnStateChange write FOnStateChange;
    property OnStreamInfo: TStreamInfoEvent read FOnStreamInfo write FOnStreamInfo;
    property OnBuffering: TNotifyEvent read FOnBuffering write FOnBuffering;
    property OnPlay: TNotifyEvent read FOnPlay write FOnPlay;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnFFTData: TFFTDataEvent read FOnFFTData write FOnFFTData;
    property OnTrackChanged: TTrackChangedEvent read FOnTrackChanged write FOnTrackChanged;
  end;

procedure Register;

implementation

var
  GlobalPlayer: TAACPlayer = nil;

procedure AudioCallbackWrapper(bufferData: pointer; frames: LongWord); cdecl;
begin
  if (GlobalPlayer <> nil) and (GlobalPlayer.FState = apsPlaying) and
     (not GlobalPlayer.FTerminating) then
    GlobalPlayer.AudioCallback(bufferData, frames)
  else
    FillChar(bufferData^, frames * 4, 0);
end;

procedure Register;
begin
  RegisterComponents('Audio', [TAACPlayer]);
end;

{ TAACPlayer }

constructor TAACPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVolume := 1.0;
  FState := apsStopped;
  FTerminating := False;
  FHttpThreadRunning := False;

  // FFT initialization
  FFFTEnabled := True;
  FFFTUpdateInterval := 2;
  FFFTUpdateCounter := 0;

  InitCriticalSection(FCriticalSection);
end;

destructor TAACPlayer.Destroy;
begin
  Stop;

  if GlobalPlayer = Self then
    GlobalPlayer := nil;

  EnterCriticalSection(FCriticalSection);
  try
    if FAacDecoder <> nil then
    begin
      aacDecoder_Close(FAacDecoder);
      FAacDecoder := nil;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  if RAudioLoaded then
    CloseAudioDevice;

  DoneCriticalSection(FCriticalSection);

  inherited Destroy;
end;

procedure TAACPlayer.InitializeBuffers;
begin
  EnterCriticalSection(FCriticalSection);
  try
    // Initialize main audio buffer (10 seconds at 44100 Hz, stereo)
    SetLength(FBuffer, 44100 * 10 * 2); // 10 seconds buffer
    FBytesInBuffer := 0;
    FBufferReadPos := 0;
    FBufferWritePos := 0;

    // Initialize output buffer for AAC decoder
    SetLength(FOutputBuff, 4096 * 2);

    // Initialize AAC buffer
    SetLength(FAacBuffer, 16 * 1024);
    FAacBufferSize := 0;

    FMinBufferSamples := 4096; // About 46 ms at 44100 Hz
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TAACPlayer.InitializeFFT;
var
  i: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    // Use 256 samples for FFT (enough for 8 bands)
    SetLength(FFFTWindow, 256);
    SetLength(FFFTSampleBuffer, 256);

    // Simple Hann window
    for i := 0 to 255 do
      FFFTWindow[i] := 0.5 * (1 - Cos(2 * Pi * i / 255));

    FFFTSamplePos := 0;

    // Initialize bands
    for i := 0 to 7 do
      FFFTBands[i] := 0;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TAACPlayer.ProcessSimpleFFT(const Samples: array of SmallInt; SampleCount: Integer);
var
  i, j: Integer;
begin
  if not FFFTEnabled or (SampleCount <= 0) then
    Exit;

  EnterCriticalSection(FCriticalSection);
  try
    // Fill the sample buffer
    j := 0;
    while (j < SampleCount) and (FFFTsamplePos < 256) do
    begin
      FFFTSampleBuffer[FFFTsamplePos] := Samples[j];
      Inc(FFFTsamplePos);
      Inc(j);
    end;

    // If we have enough samples for FFT
    if FFFTsamplePos >= 256 then
    begin
      try
        // Calculate 8 frequency bands using simple method
        Calculate8Bands;

        // Notify about new FFT data
        DoFFTData(FFFTBands, 8);

        // Reset sample position
        FFFTsamplePos := 0;

        // If there are remaining samples from current batch, add them
        while j < SampleCount do
        begin
          if FFFTsamplePos < 256 then
          begin
            FFFTSampleBuffer[FFFTsamplePos] := Samples[j];
            Inc(FFFTsamplePos);
          end;
          Inc(j);
        end;

      except
        on E: Exception do
        begin
          FFFTsamplePos := 0;
        end;
      end;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TAACPlayer.Calculate8Bands;
var
  i, band: Integer;
  WindowedSample, Magnitude: Double;
  BandSums: array[0..7] of Double;
  BandCounts: array[0..7] of Integer;
  FreqBand: Integer;
begin
  // Initialize band sums
  for band := 0 to 7 do
  begin
    BandSums[band] := 0;
    BandCounts[band] := 0;
  end;

  // Process samples and distribute to frequency bands
  for i := 0 to 255 do
  begin
    // Apply window function
    WindowedSample := FFFTSampleBuffer[i] * FFFTWindow[i] / 32768.0;

    // Better frequency band calculation using logarithmic distribution
    if i < 4 then FreqBand := 0        // 0-3: Sub-bass
    else if i < 8 then FreqBand := 1   // 4-7: Bass
    else if i < 16 then FreqBand := 2  // 8-15: Low midrange
    else if i < 32 then FreqBand := 3  // 16-31: Midrange
    else if i < 64 then FreqBand := 4  // 32-63: Upper midrange
    else if i < 96 then FreqBand := 5  // 64-95: Presence
    else if i < 160 then FreqBand := 6 // 96-159: Brilliance
    else FreqBand := 7;                // 160-255: High frequencies

    if FreqBand < 8 then
    begin
      BandSums[FreqBand] := BandSums[FreqBand] + Abs(WindowedSample);
      Inc(BandCounts[FreqBand]);
    end;
  end;

  // Calculate average magnitude for each band with different scaling
  for band := 0 to 7 do
  begin
    if BandCounts[band] > 0 then
      Magnitude := BandSums[band] / BandCounts[band]
    else
      Magnitude := 0;

    // Apply frequency-specific scaling (higher frequencies need more amplification)
    case band of
      0: Magnitude := Magnitude * 15;  // Sub-bass
      1: Magnitude := Magnitude * 12;  // Bass
      2: Magnitude := Magnitude * 10;  // Low midrange
      3: Magnitude := Magnitude * 8;   // Midrange
      4: Magnitude := Magnitude * 6;   // Upper midrange
      5: Magnitude := Magnitude * 5;   // Presence
      6: Magnitude := Magnitude * 4;   // Brilliance
      7: Magnitude := Magnitude * 3;   // High frequencies
    end;

    // Apply smoothing
    FFFTBands[band] := (FFFTBands[band] * 0.6) + (Magnitude * 0.4);

    // Limit maximum value and ensure minimum visibility
    if FFFTBands[band] > 1.0 then
      FFFTBands[band] := 1.0;
    if FFFTBands[band] < 0.01 then
      FFFTBands[band] := 0.01;
  end;
end;

procedure TAACPlayer.DoFFTData(const FFTData: array of Double; FFTSize: Integer);
var
  i: Integer;
begin
  if Assigned(FOnFFTData) then
  begin
    // Gradually decrease bands when stopped, paused or error
    if (FState = apsStopped) or (FState = apsPaused) or (FState = apsError) then
    begin
      EnterCriticalSection(FCriticalSection);
      try
        for i := 0 to 7 do
          FFFTBands[i] := FFFTBands[i] * 0.8;
      finally
        LeaveCriticalSection(FCriticalSection);
      end;
    end;

    FOnFFTData(Self, FFTData, FFTSize);
  end;
end;

procedure TAACPlayer.DoTrackChanged(const TrackTitle: string);
begin
  if Assigned(FOnTrackChanged) then
    FOnTrackChanged(Self, TrackTitle);
end;

procedure TAACPlayer.HandleMetadataReceived(Sender: TObject; const Metadata: string);
begin
  if not FTerminating and (Metadata <> '') and (Metadata <> FCurrentStreamTitle) then
  begin
    EnterCriticalSection(FCriticalSection);
    try
      FCurrentStreamTitle := Metadata;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
    DoTrackChanged(Metadata);
  end;
end;

procedure TAACPlayer.HttpThreadTerminated(Sender: TObject);
begin
  EnterCriticalSection(FCriticalSection);
  try
    FHttpThreadRunning := False;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  if not FTerminating and (FState = apsPlaying) then
  begin
    // HTTP thread terminated unexpectedly
    DoError('Stream connection lost');
    Stop;
  end;
end;

procedure TAACPlayer.SetState(AValue: TAACPlayerState);
var
  OldState: TAACPlayerState;
begin
  if FState = AValue then Exit;
  OldState := FState;

  EnterCriticalSection(FCriticalSection);
  try
    FState := AValue;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  DoStateChange(OldState, FState);
end;

procedure TAACPlayer.SetVolume(AValue: Single);
begin
  if FVolume = AValue then Exit;

  EnterCriticalSection(FCriticalSection);
  try
    FVolume := AValue;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  if IsAudioStreamValid(FAudioStream) then
    SetAudioStreamVolume(FAudioStream, FVolume);
end;

procedure TAACPlayer.SetURL(AValue: string);
begin
  if FURL = AValue then Exit;

  if FState <> apsStopped then
    Stop;

  EnterCriticalSection(FCriticalSection);
  try
    FURL := AValue;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TAACPlayer.DoError(const ErrorMsg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, ErrorMsg);
end;

procedure TAACPlayer.DoStateChange(OldState, NewState: TAACPlayerState);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self, OldState, NewState);
end;

procedure TAACPlayer.DoStreamInfo(SampleRate, Channels, Bitrate: Integer);
begin
  if Assigned(FOnStreamInfo) then
    FOnStreamInfo(Self, SampleRate, Channels, Bitrate);
end;

procedure TAACPlayer.DoBuffering;
begin
  if Assigned(FOnBuffering) then
    FOnBuffering(Self);
end;

procedure TAACPlayer.DoPlay;
begin
  if Assigned(FOnPlay) then
    FOnPlay(Self);
end;

procedure TAACPlayer.DoStop;
begin
  if Assigned(FOnStop) then
    FOnStop(Self);
end;

function TAACPlayer.InitializeAacDecoder: Boolean;
var
  Err: AAC_DECODER_ERROR;
begin
  Result := False;

  EnterCriticalSection(FCriticalSection);
  try
    FAacDecoder := aacDecoder_Open(TT_MP4_ADTS, 1);
    if FAacDecoder = nil then
    begin
      DoError('Failed to create AAC decoder');
      Exit;
    end;

    Err := aacDecoder_SetParam(FAacDecoder, AAC_CONCEAL_METHOD, 1);
    if Err <> AAC_DEC_OK then
      ; // Ignore warnings

    Err := aacDecoder_SetParam(FAacDecoder, AAC_PCM_LIMITER_ENABLE, 0);
    if Err <> AAC_DEC_OK then
      ; // Ignore warnings

    Result := True;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TAACPlayer.AddToAacBuffer(const AData: array of Byte; ADataSize: Integer);
begin
  if FTerminating then Exit;

  EnterCriticalSection(FCriticalSection);
  try
    if FAacBufferSize + ADataSize > Length(FAacBuffer) then
      SetLength(FAacBuffer, FAacBufferSize + ADataSize + 1024);

    Move(AData[0], FAacBuffer[FAacBufferSize], ADataSize);
    FAacBufferSize := FAacBufferSize + ADataSize;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TAACPlayer.DecodeAacData: Integer;
var
  FErrorCode: AAC_DECODER_ERROR;
  FByteFilled: Cardinal;
  bytesValid: Cardinal;
  dataPtr: PByte;
  i: Integer;
  bytesConsumed: Cardinal;
begin
  Result := 0;

  if FTerminating or (FAacDecoder = nil) or (FAacBufferSize = 0) then
    Exit;

  EnterCriticalSection(FCriticalSection);
  try
    dataPtr := @FAacBuffer[0];
    bytesValid := FAacBufferSize;
    FByteFilled := FAacBufferSize;

    FErrorCode := aacDecoder_Fill(FAacDecoder, @dataPtr, FAacBufferSize, FByteFilled);

    if (FErrorCode <> AAC_DEC_OK) and (FErrorCode <> AAC_DEC_NOT_ENOUGH_BITS) then
    begin
      FAacBufferSize := 0;
      Exit;
    end;

    FErrorCode := aacDecoder_DecodeFrame(FAacDecoder, PSmallInt(FOutputBuff),
                                        Length(FOutputBuff) * SizeOf(SmallInt), 0);

    if FErrorCode = AAC_DEC_OK then
    begin
      FStreamInfo := aacDecoder_GetStreamInfo(FAacDecoder);

      if (FStreamInfo <> nil) and (FStreamInfo^.sampleRate > 0) then
      begin
        if not FDecoderReady then
        begin
          FDecoderReady := True;
          DoStreamInfo(FStreamInfo^.sampleRate, FStreamInfo^.numChannels, FStreamInfo^.bitRate);
        end;

        Result := FStreamInfo^.frameSize * FStreamInfo^.numChannels;

        // Ensure we don't write beyond buffer bounds
        if Result > 0 then
        begin
          for i := 0 to Result - 1 do
          begin
            if (i < Length(FOutputBuff)) and (FBufferWritePos < Length(FBuffer)) then
            begin
              FBuffer[FBufferWritePos] := FOutputBuff[i];
              FBufferWritePos := (FBufferWritePos + 1) mod Length(FBuffer);
              FBytesInBuffer := FBytesInBuffer + 1;

              // Prevent buffer overflow
              if FBytesInBuffer > Length(FBuffer) then
                FBytesInBuffer := Length(FBuffer);
            end
            else
              Break;
          end;
        end;

        bytesConsumed := FAacBufferSize - FByteFilled;

        if bytesConsumed > 0 then
        begin
          Move(FAacBuffer[bytesConsumed], FAacBuffer[0], FByteFilled);
          FAacBufferSize := FByteFilled;
        end;
      end;
    end
    else if FErrorCode <> AAC_DEC_NOT_ENOUGH_BITS then
    begin
      FAacBufferSize := 0;
    end;

  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TAACPlayer.GetPcmSamples(out samples: array of SmallInt; maxSamples: Integer): Integer;
var
  i: Integer;
  samplesAvailable: Integer;
begin
  if FTerminating then
  begin
    Result := 0;
    Exit;
  end;

  EnterCriticalSection(FCriticalSection);
  try
    samplesAvailable := Min(FBytesInBuffer, maxSamples);
    Result := samplesAvailable;

    for i := 0 to samplesAvailable - 1 do
    begin
      samples[i] := FBuffer[FBufferReadPos];
      FBufferReadPos := (FBufferReadPos + 1) mod Length(FBuffer);
    end;

    FBytesInBuffer := FBytesInBuffer - samplesAvailable;
    if FBytesInBuffer < 0 then
      FBytesInBuffer := 0;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TAACPlayer.AudioCallback(bufferData: pointer; frames: LongWord);
var
  bytesRead: Integer;
  samplesNeeded: Integer;
  samplesToCopy: Integer;
  i: Integer;
  tempBuffer: array[0..4095] of Byte; // Вернули нормальный размер буфера
  decodeResult: Integer;
  tempSamples: array of SmallInt;
  totalRead: Integer;
  IsHttpRunning: Boolean;
begin
  if FTerminating or (FState <> apsPlaying) then
  begin
    FillChar(bufferData^, frames * 4, 0);
    Exit;
  end;

  try
    // Check if HTTP thread is running
    EnterCriticalSection(FCriticalSection);
    try
      IsHttpRunning := FHttpThreadRunning;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;

    if not IsHttpRunning then
    begin
      // Если HTTP поток остановился, но мы еще играем - просто заполняем тишиной
      // Не останавливаем воспроизведение сразу, ждем буфер
      FillChar(bufferData^, frames * 4, 0);
      Exit;
    end;

    samplesNeeded := frames * 2; // 2 channels

    // УПРОЩЕННОЕ чтение данных из потока
    totalRead := 0;
    if (FInputStream <> nil) and (FInputStream.NumBytesAvailable > 0) then
    begin
      try
        bytesRead := FInputStream.Read(tempBuffer[0], Length(tempBuffer));
        if bytesRead > 0 then
        begin
          EnterCriticalSection(FCriticalSection);
          try
            FTotalBytesRead := FTotalBytesRead + bytesRead;
          finally
            LeaveCriticalSection(FCriticalSection);
          end;
          AddToAacBuffer(tempBuffer, bytesRead);
        end;
      except
        on E: Exception do
        begin
          // Игнорируем ошибки чтения, продолжаем воспроизведение
        end;
      end;
    end;

    // Декодируем AAC данные
    if (FAacBufferSize > 0) and (not FTerminating) then
    begin
      decodeResult := DecodeAacData();
      // Не делаем цикл, чтобы не блокировать надолго
    end;

    // Проверяем буферизацию
    EnterCriticalSection(FCriticalSection);
    try
      if (FBytesInBuffer < FMinBufferSamples) and (FMinBufferSamples > 0) then
      begin
        DoBuffering;
        FillChar(bufferData^, frames * 4, 0);
        Exit;
      end
      else if FMinBufferSamples > 0 then
      begin
        FMinBufferSamples := 0; // Буфер заполнен, начинаем воспроизведение
      end;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;

    // Копируем данные в выходной буфер
    samplesToCopy := Min(FBytesInBuffer, samplesNeeded);

    if samplesToCopy > 0 then
    begin
      SetLength(tempSamples, samplesToCopy);
      GetPcmSamples(tempSamples, samplesToCopy);

      // Process simple FFT if enabled
      if FFFTEnabled and (samplesToCopy > 0) then
      begin
        Inc(FFFTUpdateCounter);
        if FFFTUpdateCounter >= FFFTUpdateInterval then
        begin
          ProcessSimpleFFT(tempSamples, samplesToCopy);
          FFFTUpdateCounter := 0;
        end;
      end;

      for i := 0 to samplesToCopy - 1 do
        PSingle(bufferData)[i] := tempSamples[i] / 32768.0 * FVolume; // Добавили громкость
    end;

    // Fill remainder with zeros
    if samplesToCopy < samplesNeeded then
      FillChar(PSingle(bufferData)[samplesToCopy],
               (samplesNeeded - samplesToCopy) * SizeOf(Single), 0);

  except
    on E: Exception do
    begin
      // Игнорируем большинство ошибок в callback, чтобы не прерывать воспроизведение
      FillChar(bufferData^, frames * 4, 0);
    end;
  end;
end;

procedure TAACPlayer.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
  begin
    if not RAudioLoaded then
      LoadRAudioLibrary(FindLibName(DEFAULT_LIB_NAME));

    if not ad_IsLoaded then
      ad_Load(FindLibName(libfdk_aac));

    if RAudioLoaded and not IsAudioDeviceReady() then
      InitAudioDevice;

    // Initialize all buffers
    InitializeBuffers;
    SetAudioStreamBufferSizeDefault(4096);

    // Initialize FFT
    InitializeFFT;
  end;
end;

procedure TAACPlayer.Play;
var
  PipeRead, PipeWrite: THandle;
  URLStatus: Integer;
begin
  if FState <> apsStopped then
    Exit;

  EnterCriticalSection(FCriticalSection);
  try
    if FURL = '' then
    begin
      DoError('URL is not specified');
      Exit;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  try
    // First check URL availability
    URLStatus := CheckURLStatus(FURL);
    if URLStatus <> 0 then
    begin
      DoError('URL is not accessible (Status: ' + IntToStr(URLStatus) + ')');
      Exit;
    end;

    // Create pipes
    if not CreatePipeHandles(PipeRead, PipeWrite, 65536) then
      raise Exception.Create('Failed to create pipe handles');

    FInputStream := TInputPipeStream.Create(PipeRead);
    FOutputStream := TOutputPipeStream.Create(PipeWrite);

    // Create HTTP thread for data
    FHttpThread := TThreadHttpGetter.Create(FURL, FOutputStream);
    FHttpThread.FormatType := 3;
    FHttpThread.OnTerminate := @HttpThreadTerminated;

    EnterCriticalSection(FCriticalSection);
    try
      FHttpThreadRunning := True;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;

    // Initialize AAC decoder
    if not InitializeAacDecoder then
      raise Exception.Create('Failed to initialize AAC decoder');

    // Reset buffer state
    InitializeBuffers;

    EnterCriticalSection(FCriticalSection);
    try
      FAacBufferSize := 0;
      FBytesInBuffer := 0;
      FBufferReadPos := 0;
      FBufferWritePos := 0;
      FMinBufferSamples := 4096;
      FDecoderReady := False;
      FTotalBytesRead := 0;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;

    // Reset FFT state
    EnterCriticalSection(FCriticalSection);
    try
      FFFTsamplePos := 0;
      FFFTUpdateCounter := 0;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;

    // Create audio stream
    FAudioStream := LoadAudioStream(44100, 32, 2);
    if not IsAudioStreamValid(FAudioStream) then
      raise Exception.Create('Failed to create audio stream');

    SetAudioStreamCallback(FAudioStream, @AudioCallbackWrapper);
    SetAudioStreamVolume(FAudioStream, FVolume);

    // Start both threads
    FHttpThread.Start;

    // Start audio playback
    GlobalPlayer := Self;
    PlayAudioStream(FAudioStream);

    SetState(apsPlaying);
    DoPlay;

  except
    on E: Exception do
    begin
      DoError('Play failed: ' + E.Message);
      SetState(apsError);
      Stop;
    end;
  end;
end;

procedure TAACPlayer.Stop;
begin
  if FState = apsStopped then Exit;

  FTerminating := True;

  // 1. Stop audio first
  if IsAudioStreamValid(FAudioStream) then
  begin
    StopAudioStream(FAudioStream);
    UnloadAudioStream(FAudioStream);
  end;

  // 2. Signal HTTP thread to terminate
  if FHttpThread <> nil then
  begin
    FHttpThread.Terminate;
  end;

  // 3. Close streams to unblock HTTP
  if FOutputStream <> nil then
  begin
    FOutputStream.Free;
    FOutputStream := nil;
  end;

  if FInputStream <> nil then
  begin
    FInputStream.Free;
    FInputStream := nil;
  end;

  // 4. Wait for and free HTTP thread
  if FHttpThread <> nil then
  begin
    FHttpThread.WaitFor;
    FHttpThread.Free;
    FHttpThread := nil;
  end;

  EnterCriticalSection(FCriticalSection);
  try
    FHttpThreadRunning := False;

    // 5. Reset FFT state
    FFFTSamplePos := 0;
    FFFTUpdateCounter := 0;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  FTerminating := False;
  SetState(apsStopped);
  DoStop;
end;

procedure TAACPlayer.Pause;
begin
  if FState <> apsPlaying then Exit;

  if IsAudioStreamValid(FAudioStream) then
    PauseAudioStream(FAudioStream);

  SetState(apsPaused);
end;

procedure TAACPlayer.Resume;
begin
  if FState <> apsPaused then Exit;

  if IsAudioStreamValid(FAudioStream) then
    ResumeAudioStream(FAudioStream);

  SetState(apsPlaying);
end;

function TAACPlayer.GetBufferLevel: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if Length(FBuffer) = 0 then
      Result := 0
    else
    begin
      Result := Round((FBytesInBuffer / Length(FBuffer)) * 100);
      if Result > 100 then Result := 100;
      if Result < 0 then Result := 0;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TAACPlayer.GetBitrate: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if (FStreamInfo <> nil) and FDecoderReady then
      Result := FStreamInfo^.bitRate
    else
      Result := 0;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TAACPlayer.GetSampleRate: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if (FStreamInfo <> nil) and FDecoderReady then
      Result := FStreamInfo^.sampleRate
    else
      Result := 0;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TAACPlayer.GetChannels: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if (FStreamInfo <> nil) and FDecoderReady then
      Result := FStreamInfo^.numChannels
    else
      Result := 0;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

end.
