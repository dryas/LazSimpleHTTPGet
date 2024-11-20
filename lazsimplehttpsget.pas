{-------------------------------------------------------------------------------
LazSimpleHTTPsGet

Simple asynchronous file download unit with progress indicator supporting
OpenSSLv1 and OpenSSLv3 utilizing FCL or Synapse for Lazarus/FPC.

MIT License

Copyright (c) 2024 Benjamin Kaiser

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-------------------------------------------------------------------------------}

unit LazSimpleHTTPsGet;

{$mode ObjFPC}
{$H+}

// We can use FPCs native FCL (default) or Synapse to handle downloads.
// Please define SIMPLEGETSYNAPSE as compiler flag to use Synapse.
{$IFNDEF SIMPLEGETSYNAPSE}
  {$DEFINE SIMPLEGETFCL}
{$ENDIF}

interface

uses
  Classes, SysUtils, TypInfo, Dialogs,
  {$IFDEF SIMPLEGETSYNAPSE}
  ssl_openssl3, httpsend, blcksock
  {$ELSE}
  fphttpclient, opensslsockets
  {$ENDIF}
  ;

const
  VERSION = '1.0.1';

type
  TStatus = (sNone, sStart, sDone, sError);

  TProgressEvent = procedure(ALen, APos: Int64; AHRLen, AHRPos: String) of object;

  TStatusEvent = procedure(Status: TStatus; ResponseCode: Integer; Msg, Body: String) of object;

  TLazSimpleHTTPsGet = class;

  { TLazSimpleHTTPsGet }

  TLazSimpleHTTPsGetThread = class(TThread)
    private
      {$IFDEF SIMPLEGETSYNAPSE}
      HTTPSender: THTTPSend;
      {$ELSE}
      HTTPSender: TFPHttpClient;
      {$ENDIF}
      FSize: Int64;
      FPos: Int64;
      FStatus: TStatus;
      FResponseCode: Integer;
      FMsg: String;
      FBody: String;
      {$IFDEF SIMPLEGETSYNAPSE}
      procedure DataReceived(Sender: TObject; Reason: THookSocketReason; const Value: String);
      function GetSizeFromHeader(Header: String): Int64;
      {$ELSE}
      procedure DataReceived(Sender: TObject; Const ContentLength, CurrentPos : Int64);
      {$ENDIF}
      procedure SetStatus(Status: TStatus; ResponseCode: Integer = 0; Msg: String = ''; Body: String = '');
      procedure HandleHTTPStatusCode(HTTPStatusCode: Integer; Body: String = '');
      function FormatByteSize(const bytes: int64): String;
    public
      URL: String;
      Filename: String;
      Owner: TLazSimpleHTTPsGet;
    protected
      procedure ShowProgress;
      procedure ShowStatus;
      procedure Execute; override;
  end;

  { TLazSimpleHTTPsGet }

  TLazSimpleHTTPsGet = class
    private
      LazSimpleHTTPsGetThread: TLazSimpleHTTPsGetThread;
      FURL: String;
      FFilename: String;
      FOnProgress: TProgressEvent;
      FOnStatus: TStatusEvent;
    public
      property URL: String
        read FURL
        write FURL;
      property Filename: String
        read FFilename
        write FFilename;
      function Get: Boolean;
      function Fetch: Boolean;
      destructor Destroy; override;
      procedure ThreadTerminated(Sender: TObject);
      function GetFileNameFromURL(UrlString: String): String;
    published
      property OnProgress: TProgressEvent
        read FOnProgress
        write FOnProgress;
      property onStatus: TStatusEvent
        read FOnStatus
        write FOnStatus;
  end;

implementation

{ TLazSimpleHTTPsGet }

// Start the download process and manages thread handling:
function TLazSimpleHTTPsGet.Get: Boolean;
begin
  if not Assigned(LazSimpleHTTPsGetThread) then
  begin
    LazSimpleHTTPsGetThread := TLazSimpleHTTPsGetThread.Create(True);
    LazSimpleHTTPsGetThread.OnTerminate := @ThreadTerminated;
    LazSimpleHTTPsGetThread.FreeOnTerminate := True;
    LazSimpleHTTPsGetThread.Owner := Self;
    LazSimpleHTTPsGetThread.URL := FURL;
    LazSimpleHTTPsGetThread.Filename := FFilename;
    LazSimpleHTTPsGetThread.Start;

    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

// Fetch a url and manages thread handling:
function TLazSimpleHTTPsGet.Fetch: Boolean;
begin
  if not Assigned(LazSimpleHTTPsGetThread) then
  begin
    LazSimpleHTTPsGetThread := TLazSimpleHTTPsGetThread.Create(True);
    LazSimpleHTTPsGetThread.OnTerminate := @ThreadTerminated;
    LazSimpleHTTPsGetThread.FreeOnTerminate := True;
    LazSimpleHTTPsGetThread.Owner := Self;
    LazSimpleHTTPsGetThread.URL := FURL;
    LazSimpleHTTPsGetThread.Filename := '';
    LazSimpleHTTPsGetThread.Start;

    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

// Destructor terminates the thread if needed:
destructor TLazSimpleHTTPsGet.Destroy;
begin
  // Check if Thread is still running:
  if Assigned(LazSimpleHTTPsGetThread) then
  begin
    LazSimpleHTTPsGetThread.Terminate;
    LazSimpleHTTPsGetThread.WaitFor;
  end;

  inherited;
end;

// Gets called after thread has been terminated:
procedure TLazSimpleHTTPsGet.ThreadTerminated(Sender: TObject);
begin
  LazSimpleHTTPsGetThread := nil;
end;

// Extracts a file name from an given URL:
function TLazSimpleHTTPsGet.GetFileNameFromURL(UrlString: String): String;
var
  i: Integer;
  l: Integer;
  ExtractedFileName: String;
  Current: String;
begin
  extractedFileName := '';
  l := Length(UrlString);
  for i := l downto 0 do
  begin
    Current := UrlString[i];
    if Current <> '/' then
    begin
      ExtractedFileName := Current + ExtractedFileName;
    end
    else
    begin
      Result := ExtractedFileName;
      break;
    end;
  end;
end;

{ TLazSimpleHTTPsGetThread }

// Formats byte size in human readable values:
function TLazSimpleHTTPsGetThread.FormatByteSize(const bytes: int64): String;
const
  B = 1; //byte
  KB = 1024 * B; //kilobyte
  MB = 1024 * KB; //megabyte
  GB = 1024 * MB; //gigabyte
begin
  if bytes > GB then
    result := FormatFloat('#.00 GB', bytes / GB)
  else if bytes > MB then
    result := FormatFloat('#.00 MB', bytes / MB)
  else if bytes > KB then
    result := FormatFloat('#.00 KB', bytes / KB)
  else
    result := FormatFloat('#.00 bytes', bytes);
end;

{$IFDEF SIMPLEGETSYNAPSE}
// Parse the content length out of header information:
function TLazSimpleHTTPsGetThread.GetSizeFromHeader(Header: String): Int64;
var
  item : TStringList;
begin
  Result:= -1;
  if Pos('Content-Length:', Header) <> 0 then
  begin
    try
      item:= TStringList.Create();
      item.Delimiter:= ':';
      item.StrictDelimiter:=true;
      item.DelimitedText:=Header;
      if item.Count = 2 then
      begin
        Result:= StrToInt64(Trim(item[1]));
      end;
    finally
      FreeAndNil(item);
    end;
  end;
end;
{$ENDIF}

// Get information of the running download, sync info with main thread:
{$IFDEF SIMPLEGETSYNAPSE}
procedure TLazSimpleHTTPsGetThread.DataReceived(Sender: TObject; Reason: THookSocketReason; const Value: String);
var
  currentHeader: String;
  i: Integer;
begin
  // Check if Thread needs to be stopped:
  if Terminated then
  begin
    // Cancel download
    if Assigned(HTTPSender) then
    begin
      HTTPSender.Abort;
    end;
  end;

  // If size of download is not set, try to get it by parsing the headers:
  if (FSize = -1) then
  begin
    if (Assigned(HTTPSender)) and (HTTPSender.Headers.Count > 0) then
    begin
      for i := 0 to Pred(HTTPSender.Headers.Count) do
      begin
        currentHeader := HTTPSender.Headers[i];
        FSize := GetSizeFromHeader(currentHeader);
        if FSize <> -1 then break;
      end;
    end;
  end;

  if Reason = THookSocketReason.HR_ReadCount then
  begin
    FPos := FPos + StrToInt64(Value);
    if not Terminated then
    begin
      Synchronize(@ShowProgress);
    end;
  end;
end;
{$ELSE}
procedure TLazSimpleHTTPsGetThread.DataReceived(Sender: TObject; Const ContentLength, CurrentPos : Int64);
begin
  // Check if Thread needs to be stopped:
  if Terminated then
  begin
    // Cancel download
    if Assigned(HTTPSender) then
    begin
      HTTPSender.Terminate;
    end;
  end;

  FPos := CurrentPos;
  if not Terminated then
  begin
    Synchronize(@ShowProgress);
  end;
end;
{$ENDIF}

// Call the OnProgress event and pass info about download:
procedure TLazSimpleHTTPsGetThread.ShowProgress;
begin
  if Assigned(Owner.FOnProgress) then
  begin
    Owner.FOnProgress(FSize, FPos, FormatByteSize(FSize), FormatByteSize(FPos));
  end;
end;

// Set the status of the download and sync with main thread:
procedure TLazSimpleHTTPsGetThread.SetStatus(Status: TStatus; ResponseCode: Integer = 0; Msg: String = ''; Body: String = '');
begin
  FStatus := Status;
  FResponseCode := ResponseCode;
  FMsg := Msg;
  FBody := Body;
  if not Terminated then
  begin
    Synchronize(@ShowStatus);
  end;
end;

// Call the OnStatus event if download status has changed:
procedure TLazSimpleHTTPsGetThread.ShowStatus;
begin
  if Assigned(Owner.FOnStatus) then
  begin
    Owner.FOnStatus(FStatus, FResponseCode, FMsg, FBody);
  end;
end;

// Generates the OnStatus response:
procedure TLazSimpleHTTPsGetThread.HandleHTTPStatusCode(HTTPStatusCode: Integer; Body: String = '');
begin
  // Handle most important status codes:
  case HTTPStatusCode of
  100..229:
    begin
      if HTTPStatusCode = 200 then
        SetStatus(sDone, 200, 'OK', Body)
      else
        SetStatus(sDone, HTTPStatusCode, '', Body);
    end;
  301:
    SetStatus(sError, 301, 'Moved Permanently');
  302:
    SetStatus(sError, 302, 'Temporary Redirect');
  400:
    SetStatus(sError, 400, 'Bad Request');
  401:
    SetStatus(sError, 401, 'Unauthorized');
  403:
    SetStatus(sError, 403, 'Forbidden');
  404:
    SetStatus(sError, 403, 'Not Found');
  500:
    SetStatus(sError, 500, 'Internal Server Error');
  502:
    SetStatus(sError, 502, 'Bad Gateway');
  503:
    SetStatus(sError, 503, 'Service Unavailable');
  // Handle everything else:
  else
    SetStatus(sError, HTTPStatusCode, 'Please check HTTP status code');
  end;
end;

// Run the thread:
procedure TLazSimpleHTTPsGetThread.Execute;
var
  Content: String;
{$IFNDEF SIMPLEGETSYNAPSE}
  Headers: TStringList;
  ContentLength: String;
{$ENDIF}
begin
  FSize := -1;
  FStatus := sNone;
  Content := '';
  {$IFDEF SIMPLEGETSYNAPSE}
  HTTPSender := THTTPSend.Create;
  {$ELSE}
  HTTPSender := TFPHttpClient.Create(nil);
  {$ENDIF}
  try
    try
      // Download content:
      SetStatus(sStart);

      {$IFDEF SIMPLEGETSYNAPSE}
      // Using Synapse:
      HTTPSender.UserAgent := 'LazSimpleHTTPsGet/' + VERSION;
      // Set Callback:
      HTTPSender.Sock.OnStatus := @DataReceived;
      // Start download:
      HTTPSender.HTTPMethod('GET', URL);
      // Statuscode 100-229 is ok:
      case HTTPSender.ResultCode of
      100..229:
        begin
          if Length(Filename) > 0 then
            HTTPSender.Document.SaveToFile(Filename)
          else
            SetString(Content, PAnsiChar(HTTPSender.Document.Memory), HTTPSender.Document.Size);
        end;
      end;
      // Generate response for OnStatus Event:
      HandleHTTPStatusCode(HTTPSender.ResultCode, Content);
      {$ELSE}
      // Using FCL:
      HTTPSender.AddHeader('User-Agent', 'LazSimpleHTTPsGet/' + VERSION);
      HTTPSender.AllowRedirect := True;

      // Getting file size via HEAD request:
      try
        Headers := TStringList.Create;
        HTTPSender.Head(URL, Headers);
        ContentLength := Headers.Values['Content-Length'];
        FSize := StrToInt64Def(ContentLength, -1);
      finally
        FreeAndNil(Headers);
      end;

      // Set Callback:
      HTTPSender.OnDataReceived := @DataReceived;

      // Start download:
      if Length(Filename) > 0 then
        HTTPSender.Get(URL, Filename)
      else
        Content := HTTPSender.Get(URL);

      // Generate response for OnStatus Event:
      HandleHTTPStatusCode(HTTPSender.ResponseStatusCode, Content);
      {$ENDIF}
    except
      on E: Exception do
      begin
        // Do stuff...
        SetStatus(sError, 1000, E.Message);
      end;
    end;
  finally
    FreeAndNil(HTTPSender);
  end;
end;

end.

