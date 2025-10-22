unit binding.raudio;

{$mode ObjFPC}{$H+}
{$packrecords c}

interface

uses
  SysUtils, dynlibs;

const
  {$IFDEF MSWINDOWS}
  DEFAULT_LIB_NAME = 'libraudio.dll';
  {$ELSE}
  {$IFDEF DARWIN}
  DEFAULT_LIB_NAME = 'libraudio.dylib';
  {$ELSE}
  DEFAULT_LIB_NAME = 'libraudio.so';
  {$ENDIF}
  {$ENDIF}

type
  (* Wave, audio wave data *)
  PWave = ^TWave;
  TWave = record
    frameCount: LongWord;   // Total number of frames (considering channels)
    sampleRate: LongWord;   // Frequency (samples per second)
    sampleSize: LongWord;   // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    channels: LongWord;     // Number of channels (1-mono, 2-stereo, ...)
    data: Pointer;          // Buffer data pointer
  end;

  (* Opaque structs declaration *)
  (* NOTE: Actual structs are defined internally in raudio module *)
  PrAudioBuffer = ^TrAudioBuffer;
  TrAudioBuffer = record end;

  PrAudioProcessor = ^TrAudioProcessor;
  TrAudioProcessor = record end;

  (* AudioStream, custom audio stream *)
  PAudioStream = ^TAudioStream;
  TAudioStream = record
    buffer: PrAudioBuffer;       // Pointer to internal data used by the audio system
    processor: PrAudioProcessor; // Pointer to internal data processor, useful for audio effects
    sampleRate: LongWord;        // Frequency (samples per second)
    sampleSize: LongWord;        // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    channels: LongWord;          // Number of channels (1-mono, 2-stereo, ...)
  end;

  (* Sound *)
  PSound = ^TSound;
  TSound = record
    stream: TAudioStream;  // Audio stream
    frameCount: LongWord;  // Total number of frames (considering channels)
  end;

  (* Music, audio stream, anything longer than ~10 seconds should be streamed *)
  PMusic = ^TMusic;
  TMusic = record
    stream: TAudioStream;  // Audio stream
    frameCount: LongWord;  // Total number of frames (considering channels)
    looping: Boolean;      // Music looping enable
    ctxType: Integer;      // Type of music context (audio filetype)
    ctxData: Pointer;      // Audio context data, depends on type
  end;

  // Audio callback type
  PAudioCallback = ^TAudioCallback;
  TAudioCallback = procedure(bufferData: Pointer; frames: LongWord); cdecl;

var
  // Audio device management functions
  InitAudioDevice: procedure; cdecl;
  CloseAudioDevice: procedure; cdecl;
  IsAudioDeviceReady: function: Boolean; cdecl;
  SetMasterVolume: procedure(volume: Single); cdecl;
  GetMasterVolume: function: Single; cdecl;

  // Wave/Sound loading/unloading functions
  LoadWave: function(const fileName: PChar): TWave; cdecl;
  LoadWaveFromMemory: function(const fileType: PChar; const fileData: PByte; dataSize: Integer): TWave; cdecl;
  IsWaveValid: function(wave: TWave): Boolean; cdecl;
  LoadSound: function(const fileName: PChar): TSound; cdecl;
  LoadSoundFromWave: function(wave: TWave): TSound; cdecl;
  LoadSoundAlias: function(source: TSound): TSound; cdecl;
  IsSoundValid: function(sound: TSound): Boolean; cdecl;
  UpdateSound: procedure(sound: TSound; const data: Pointer; sampleCount: Integer); cdecl;
  UnloadWave: procedure(wave: TWave); cdecl;
  UnloadSound: procedure(sound: TSound); cdecl;
  UnloadSoundAlias: procedure(alias: TSound); cdecl;
  ExportWave: function(wave: TWave; const fileName: PChar): Boolean; cdecl;
  ExportWaveAsCode: function(wave: TWave; const fileName: PChar): Boolean; cdecl;

  // Wave/Sound management functions
  PlaySound: procedure(sound: TSound); cdecl;
  StopSound: procedure(sound: TSound); cdecl;
  PauseSound: procedure(sound: TSound); cdecl;
  ResumeSound: procedure(sound: TSound); cdecl;
  IsSoundPlaying: function(sound: TSound): Boolean; cdecl;
  SetSoundVolume: procedure(sound: TSound; volume: Single); cdecl;
  SetSoundPitch: procedure(sound: TSound; pitch: Single); cdecl;
  SetSoundPan: procedure(sound: TSound; pan: Single); cdecl;
  WaveCopy: function(wave: TWave): TWave; cdecl;
  WaveCrop: procedure(wave: PWave; initFrame, finalFrame: Integer); cdecl;
  WaveFormat: procedure(wave: PWave; sampleRate, sampleSize, channels: Integer); cdecl;
  LoadWaveSamples: function(wave: TWave): PSingle; cdecl;
  UnloadWaveSamples: procedure(samples: PSingle); cdecl;

  // Music management functions
  LoadMusicStream: function(const fileName: PChar): TMusic; cdecl;
  LoadMusicStreamFromMemory: function(const fileType: PChar; const data: PByte; dataSize: Integer): TMusic; cdecl;
  IsMusicValid: function(music: TMusic): Boolean; cdecl;
  UnloadMusicStream: procedure(music: TMusic); cdecl;
  PlayMusicStream: procedure(music: TMusic); cdecl;
  IsMusicStreamPlaying: function(music: TMusic): Boolean; cdecl;
  UpdateMusicStream: procedure(music: TMusic); cdecl;
  StopMusicStream: procedure(music: TMusic); cdecl;
  PauseMusicStream: procedure(music: TMusic); cdecl;
  ResumeMusicStream: procedure(music: TMusic); cdecl;
  SeekMusicStream: procedure(music: TMusic; position: Single); cdecl;
  SetMusicVolume: procedure(music: TMusic; volume: Single); cdecl;
  SetMusicPitch: procedure(music: TMusic; pitch: Single); cdecl;
  SetMusicPan: procedure(music: TMusic; pan: Single); cdecl;
  GetMusicTimeLength: function(music: TMusic): Single; cdecl;
  GetMusicTimePlayed: function(music: TMusic): Single; cdecl;

  // AudioStream management functions
  LoadAudioStream: function(sampleRate, sampleSize, channels: LongWord): TAudioStream; cdecl;
  IsAudioStreamValid: function(stream: TAudioStream): Boolean; cdecl;
  UnloadAudioStream: procedure(stream: TAudioStream); cdecl;
  UpdateAudioStream: procedure(stream: TAudioStream; const data: Pointer; frameCount: Integer); cdecl;
  IsAudioStreamProcessed: function(stream: TAudioStream): Boolean; cdecl;
  PlayAudioStream: procedure(stream: TAudioStream); cdecl;
  PauseAudioStream: procedure(stream: TAudioStream); cdecl;
  ResumeAudioStream: procedure(stream: TAudioStream); cdecl;
  IsAudioStreamPlaying: function(stream: TAudioStream): Boolean; cdecl;
  StopAudioStream: procedure(stream: TAudioStream); cdecl;
  SetAudioStreamVolume: procedure(stream: TAudioStream; volume: Single); cdecl;
  SetAudioStreamPitch: procedure(stream: TAudioStream; pitch: Single); cdecl;
  SetAudioStreamPan: procedure(stream: TAudioStream; pan: Single); cdecl;
  SetAudioStreamBufferSizeDefault: procedure(size: Integer); cdecl;
  SetAudioStreamCallback: procedure(stream: TAudioStream; callback: TAudioCallback); cdecl;
  AttachAudioStreamProcessor: procedure(stream: TAudioStream; processor: TAudioCallback); cdecl;
  DetachAudioStreamProcessor: procedure(stream: TAudioStream; processor: TAudioCallback); cdecl;
  AttachAudioMixedProcessor: procedure(processor: TAudioCallback); cdecl;
  DetachAudioMixedProcessor: procedure(processor: TAudioCallback); cdecl;

procedure LoadRAudioLibrary(const LibraryName: string = DEFAULT_LIB_NAME);
function RAudioLoaded: Boolean;
function FindLibName(aLibName: string): string;

implementation

var
  library_handle: TLibHandle = NilHandle;

procedure LoadProc(var fn_var; const fn_name: string);
begin
  pointer(fn_var) := GetProcedureAddress(library_handle, fn_name);
  if pointer(fn_var) = nil then
    raise Exception.CreateFmt('Could not load procedure "%s"', [fn_name]);
end;

procedure LoadRAudioLibrary(const LibraryName: string);
begin
  if library_handle <> NilHandle then
    Exit; // Уже загружена

  library_handle := LoadLibrary(LibraryName);
  if library_handle = NilHandle then
    raise Exception.CreateFmt('Could not load library "%s"', [LibraryName]);

  try
    // Audio device management functions
    LoadProc(InitAudioDevice, 'InitAudioDevice');
    LoadProc(CloseAudioDevice, 'CloseAudioDevice');
    LoadProc(IsAudioDeviceReady, 'IsAudioDeviceReady');
    LoadProc(SetMasterVolume, 'SetMasterVolume');
    LoadProc(GetMasterVolume, 'GetMasterVolume');

    // Wave/Sound loading/unloading functions
    LoadProc(LoadWave, 'LoadWave');
    LoadProc(LoadWaveFromMemory, 'LoadWaveFromMemory');
    LoadProc(IsWaveValid, 'IsWaveValid');
    LoadProc(LoadSound, 'LoadSound');
    LoadProc(LoadSoundFromWave, 'LoadSoundFromWave');
    LoadProc(LoadSoundAlias, 'LoadSoundAlias');
    LoadProc(IsSoundValid, 'IsSoundValid');
    LoadProc(UpdateSound, 'UpdateSound');
    LoadProc(UnloadWave, 'UnloadWave');
    LoadProc(UnloadSound, 'UnloadSound');
    LoadProc(UnloadSoundAlias, 'UnloadSoundAlias');
    LoadProc(ExportWave, 'ExportWave');
    LoadProc(ExportWaveAsCode, 'ExportWaveAsCode');

    // Wave/Sound management functions
    LoadProc(PlaySound, 'PlaySound');
    LoadProc(StopSound, 'StopSound');
    LoadProc(PauseSound, 'PauseSound');
    LoadProc(ResumeSound, 'ResumeSound');
    LoadProc(IsSoundPlaying, 'IsSoundPlaying');
    LoadProc(SetSoundVolume, 'SetSoundVolume');
    LoadProc(SetSoundPitch, 'SetSoundPitch');
    LoadProc(SetSoundPan, 'SetSoundPan');
    LoadProc(WaveCopy, 'WaveCopy');
    LoadProc(WaveCrop, 'WaveCrop');
    LoadProc(WaveFormat, 'WaveFormat');
    LoadProc(LoadWaveSamples, 'LoadWaveSamples');
    LoadProc(UnloadWaveSamples, 'UnloadWaveSamples');

    // Music management functions
    LoadProc(LoadMusicStream, 'LoadMusicStream');
    LoadProc(LoadMusicStreamFromMemory, 'LoadMusicStreamFromMemory');
    LoadProc(IsMusicValid, 'IsMusicValid');
    LoadProc(UnloadMusicStream, 'UnloadMusicStream');
    LoadProc(PlayMusicStream, 'PlayMusicStream');
    LoadProc(IsMusicStreamPlaying, 'IsMusicStreamPlaying');
    LoadProc(UpdateMusicStream, 'UpdateMusicStream');
    LoadProc(StopMusicStream, 'StopMusicStream');
    LoadProc(PauseMusicStream, 'PauseMusicStream');
    LoadProc(ResumeMusicStream, 'ResumeMusicStream');
    LoadProc(SeekMusicStream, 'SeekMusicStream');
    LoadProc(SetMusicVolume, 'SetMusicVolume');
    LoadProc(SetMusicPitch, 'SetMusicPitch');
    LoadProc(SetMusicPan, 'SetMusicPan');
    LoadProc(GetMusicTimeLength, 'GetMusicTimeLength');
    LoadProc(GetMusicTimePlayed, 'GetMusicTimePlayed');

    // AudioStream management functions
    LoadProc(LoadAudioStream, 'LoadAudioStream');
    LoadProc(IsAudioStreamValid, 'IsAudioStreamValid');
    LoadProc(UnloadAudioStream, 'UnloadAudioStream');
    LoadProc(UpdateAudioStream, 'UpdateAudioStream');
    LoadProc(IsAudioStreamProcessed, 'IsAudioStreamProcessed');
    LoadProc(PlayAudioStream, 'PlayAudioStream');
    LoadProc(PauseAudioStream, 'PauseAudioStream');
    LoadProc(ResumeAudioStream, 'ResumeAudioStream');
    LoadProc(IsAudioStreamPlaying, 'IsAudioStreamPlaying');
    LoadProc(StopAudioStream, 'StopAudioStream');
    LoadProc(SetAudioStreamVolume, 'SetAudioStreamVolume');
    LoadProc(SetAudioStreamPitch, 'SetAudioStreamPitch');
    LoadProc(SetAudioStreamPan, 'SetAudioStreamPan');
    LoadProc(SetAudioStreamBufferSizeDefault, 'SetAudioStreamBufferSizeDefault');
    LoadProc(SetAudioStreamCallback, 'SetAudioStreamCallback');
    LoadProc(AttachAudioStreamProcessor, 'AttachAudioStreamProcessor');
    LoadProc(DetachAudioStreamProcessor, 'DetachAudioStreamProcessor');
    LoadProc(AttachAudioMixedProcessor, 'AttachAudioMixedProcessor');
    LoadProc(DetachAudioMixedProcessor, 'DetachAudioMixedProcessor');

  except
    UnloadLibrary(library_handle);
    library_handle := NilHandle;
    raise;
  end;
end;

function RAudioLoaded: Boolean;
begin
  Result := library_handle <> NilHandle;
end;

function FindLibName(aLibName: string): string;
var
  {$IFDEF Linux}
    PathNames : array of string = ('.', '.lib', 'lib','x86_64-linux', 'x86_64-win64');
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    PathNames : array of string = ('x86_64-win64');
  {$ENDIF}
  PathName  : string;
begin
  for PathName in PathNames do
    if FileExists(PathName + '/' + aLibName) then
    begin
      FindLibName := PathName + '/' + aLibName;
      exit;
    end;

  FindLibName := aLibName;
end;

///initialization

//finalization
 // if library_handle <> NilHandle then
 //   UnloadLibrary(library_handle);
end.
