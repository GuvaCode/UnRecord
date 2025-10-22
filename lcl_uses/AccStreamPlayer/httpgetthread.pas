{This unit is part of United Openlibraries of Sound (uos)}

{      This is HTTP Thread Getter
 created by Andrew Haines -> andrewd207@aol.com
       License: modified LGPL.
 Fred van Stappen / fiens@hotmail.com}

unit httpgetthread;

{$mode objfpc}{$H+}
{$RANGECHECKS OFF}

interface

uses
  Classes, SysUtils, Pipes, fphttpclient, openssl, opensslsockets;

type
  { TThreadHttpGetter }
  TThreadHttpGetter = class(TThread)
  private
    FOutStream: TOutputPipeStream;
    FWantedURL: string;
    FHttpClient: TFPHTTPClient;
    FTerminated: Boolean;
    FCriticalSection: TRTLCriticalSection;
    FLastError: string;
    function GetRedirectURL(AResponseStrings: TStrings): string;
    procedure HttpDataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    FIsRunning: Boolean;
    FormatType: integer;  // 0: mp3, 1: opus, 2: aac
    ContentType: string;
    property IsRunning: Boolean read FIsRunning;
    property LastError: string read FLastError;
    constructor Create(AWantedURL: string; AOutputStream: TOutputPipeStream);
    destructor Destroy; override;
  end;

{ Function to check URL status with detailed error codes }
function CheckURLStatus(const URL: string): Integer;

implementation

{ Check URL status with detailed error codes }
function CheckURLStatus(const URL: string): Integer;
var
  Http: TFPHTTPClient;
begin
  if (URL = '') or (Pos('http', LowerCase(URL)) <> 1) then
    Exit(1);

  Http := TFPHTTPClient.Create(nil);
  try
    Http.AllowRedirect := True;
    Http.MaxRedirects := 5;
    Http.IOTimeout := 5000;
    Http.ConnectTimeout := 5000;
    Http.RequestHeaders.Clear;
    try
      Http.HTTPMethod('HEAD', URL, nil, [200, 204, 301, 302, 303, 307, 308]);
      Result := Http.ResponseStatusCode;
      case Http.ResponseStatusCode of
        200, 204, 301, 302, 303, 307, 308, 400:
          result := 0;
      end;
    except
      on E: EHTTPClient do
      begin
        case Http.ResponseStatusCode of
          301, 302, 303, 307, 308:
            Result := Http.ResponseStatusCode;
        end;

        if Http.ResponseStatusCode = 400 then
          Result := 0
        else if Http.ResponseStatusCode > 400 then
          Result := Http.ResponseStatusCode
        else if Pos('redirect', LowerCase(E.Message)) > 0 then
          Result := 4
        else
          Result := 5;
      end;
      on E: Exception do
      begin
        if Pos('timeout', LowerCase(E.Message)) > 0 then
          Result := 2
        else if (Pos('dns', LowerCase(E.Message)) > 0) or (Pos('host', LowerCase(E.Message)) > 0) then
          Result := 3
        else
          Result := 5;
      end;
    end;
  finally
    Http.Free;
  end;
end;

{ TThreadHttpGetter }

constructor TThreadHttpGetter.Create(AWantedURL: string; AOutputStream: TOutputPipeStream);
begin
  inherited Create(True); // Create suspended
  InitCriticalSection(FCriticalSection);
  FIsRunning := True;
  FWantedURL := AWantedURL;
  FOutStream := AOutputStream;
  FTerminated := False;
  FLastError := '';
  FreeOnTerminate := False;
end;

destructor TThreadHttpGetter.Destroy;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if Assigned(FHttpClient) then
    begin
      FHttpClient.OnDataReceived := nil;
      FHttpClient.Free;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
  DoneCriticalSection(FCriticalSection);
  inherited Destroy;
end;

function TThreadHttpGetter.GetRedirectURL(AResponseStrings: TStrings): string;
var
  S: string;
  F: integer;
  Search: string = 'location:';
begin
  Result := '';
  for S in AResponseStrings do
  begin
    F := Pos(Search, Lowercase(S));
    if F > 0 then
    begin
      Inc(F, Length(Search));
      Exit(Trim(Copy(S, F, Length(S) - F + 1)));
    end;
  end;
end;

procedure TThreadHttpGetter.HttpDataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
begin
  EnterCriticalSection(FCriticalSection);
  try
    if FTerminated and Assigned(FHttpClient) then
      FHttpClient.Terminate;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TThreadHttpGetter.TerminatedSet;
begin
  inherited TerminatedSet;
  EnterCriticalSection(FCriticalSection);
  try
    FTerminated := True;
    if Assigned(FHttpClient) then
      FHttpClient.Terminate;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TThreadHttpGetter.Execute;
var
  Http: TFPHTTPClient;
  SL: TStringList;
  URL: string;
  RedirectCount: Integer;
  RetryCount: Integer;
begin
  URL := FWantedURL;
  if URL = '' then
  begin
    FIsRunning := False;
    Exit;
  end;

  // Initialize SSL
  if not InitSSLInterface then
  begin
    FLastError := 'SSL initialization failed';
    FIsRunning := False;
    Exit;
  end;

  Http := TFPHTTPClient.Create(nil);
  SL := TStringList.Create;

  EnterCriticalSection(FCriticalSection);
  try
    FHttpClient := Http;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  FTerminated := False;
  ContentType := '';
  FormatType := 0;
  RedirectCount := 0;

  try
    Http.AllowRedirect := True;
    Http.MaxRedirects := 5;
    Http.IOTimeout := 10000;
    Http.ConnectTimeout := 10000;
    Http.OnDataReceived := @HttpDataReceived;

    // Set standard headers
    Http.RequestHeaders.Clear;
    Http.AddHeader('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36');
    Http.AddHeader('Accept', 'audio/*, */*');
    Http.AddHeader('Connection', 'keep-alive');

    // Determine format through simple HEAD request
    try
      Http.HTTPMethod('HEAD', URL, nil, [200, 204]);
      SL.Assign(Http.ResponseHeaders);
      ContentType := LowerCase(SL.Values['Content-Type']);

      // Simple format detection
      if Pos('mpeg', ContentType) > 0 then FormatType := 1
      else if Pos('mp3', ContentType) > 0 then FormatType := 1
      else if Pos('aac', ContentType) > 0 then FormatType := 3
      else if Pos('ogg', ContentType) > 0 then FormatType := 2
      else if Pos('opus', ContentType) > 0 then FormatType := 2
      else
      begin
        // Fallback to URL-based detection
        if Pos('.mp3', LowerCase(URL)) > 0 then FormatType := 1
        else if (Pos('.aac', LowerCase(URL)) > 0) or (Pos('.adts', LowerCase(URL)) > 0) then FormatType := 3
        else FormatType := 3; // Default to AAC
      end;
    except
      on E: Exception do
      begin
        // Simple fallback detection
        if Pos('.mp3', LowerCase(URL)) > 0 then FormatType := 1
        else if (Pos('.aac', LowerCase(URL)) > 0) or (Pos('.adts', LowerCase(URL)) > 0) then FormatType := 3
        else FormatType := 3;
      end;
    end;

    if not Assigned(FOutStream) or FTerminated then
    begin
      FIsRunning := False;
      Exit;
    end;

    // SIMPLE GET request - только данные
    // Используем прямое чтение в поток, но с защитой
    RetryCount := 0;
    while (RetryCount < 2) and not FTerminated do
    begin
      try
        // Прямой GET в выходной поток - это важно для потокового вещания
        Http.Get(URL, FOutStream);
        Break; // Успешно - выходим из цикла
      except
        on E: Exception do
        begin
          FLastError := E.Message;
          Inc(RetryCount);

          if FTerminated then Break;

          if RetryCount >= 2 then
          begin
            // Последняя попытка тоже failed
            Break;
          end
          else
          begin
            // Ждем перед повторной попыткой
            Sleep(1000);
          end;
        end;
      end;
    end;

  finally
    // Cleanup
    EnterCriticalSection(FCriticalSection);
    try
      if Assigned(Http) then
      begin
        Http.OnDataReceived := nil;
        Http.Free;
        FHttpClient := nil;
      end;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;

    SL.Free;
    FIsRunning := False;
  end;
end;

end.
