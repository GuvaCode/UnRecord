unit metadatathread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets;

type
  TMetadataReceivedEvent = procedure(Sender: TObject; const Metadata: string) of object;

  { TMetadataThread }

  TMetadataThread = class(TThread)
  private
    FURL: string;
    FMetadata: string;
    FOnMetadataReceived: TMetadataReceivedEvent;
    FTerminated: Boolean;
    procedure DoMetadataReceived;
  protected
    procedure Execute; override;
  public
    property OnMetadataReceived: TMetadataReceivedEvent read FOnMetadataReceived write FOnMetadataReceived;
    constructor Create(const AURL: string);
    procedure Terminate;
  end;

implementation

{ TMetadataThread }

procedure TMetadataThread.DoMetadataReceived;
begin
  if Assigned(FOnMetadataReceived) then
    FOnMetadataReceived(Self, FMetadata);
end;

procedure TMetadataThread.Execute;
var
  Http: TFPHTTPClient;
  LastMetadata: string;
  RetryCount: Integer;
begin
  FTerminated := False;
  LastMetadata := '';
  RetryCount := 0;

  while not FTerminated do
  begin
    try
      Http := TFPHTTPClient.Create(nil);
      try
        Http.AllowRedirect := True;
        Http.MaxRedirects := 3;
        Http.IOTimeout := 10000;
        Http.ConnectTimeout := 10000;

        // Request metadata
        Http.RequestHeaders.Clear;
        Http.RequestHeaders.Add('Icy-MetaData: 1');

        // Use HEAD request to get metadata without downloading full stream
        try
          Http.HTTPMethod('HEAD', FURL, nil, [200, 204]);

          // Extract metadata from headers
          FMetadata := Http.GetHeader(Http.ResponseHeaders, 'icy-title');
          if FMetadata = '' then
            FMetadata := Http.GetHeader(Http.ResponseHeaders, 'icy-name');
          if FMetadata = '' then
            FMetadata := Http.GetHeader(Http.ResponseHeaders, 'ice-name');

          // If we have new metadata, notify
          if (FMetadata <> '') and (FMetadata <> LastMetadata) then
          begin
            LastMetadata := FMetadata;
            Synchronize(@DoMetadataReceived);
          end;

          RetryCount := 0; // Reset retry count on success

        except
          on E: Exception do
          begin
            // Silent fail - just retry
          end;
        end;

      finally
        Http.Free;
      end;

    except
      on E: Exception do
      begin
        // Silent fail
      end;
    end;

    // Wait before next check (5 seconds)
    if not FTerminated then
    begin
      Sleep(5000);
      Inc(RetryCount);

      // If too many failures, wait longer
      if RetryCount > 5 then
        Sleep(15000);
    end;
  end;
end;

constructor TMetadataThread.Create(const AURL: string);
begin
  inherited Create(True); // Create suspended
  FURL := AURL;
  FMetadata := '';
  FTerminated := False;
  FreeOnTerminate := False;
end;

procedure TMetadataThread.Terminate;
begin
  FTerminated := True;
  inherited Terminate;
end;

end.
