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

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, LazSimpleHTTPsGet;

type

  { TMainForm }

  TMainForm = class(TForm)
    FetchBtn: TButton;
    StopBtn: TButton;
    DownloadBtn: TButton;
    DownloadProgressLabel: TLabel;
    GroupBox1: TGroupBox;
    TransferStaticLabel: TLabel;
    ProgressBar: TProgressBar;
    SaveDialog: TSaveDialog;
    StatusInfoMemo: TMemo;
    ProgressStaticLabel: TLabel;
    URLEdit: TEdit;
    UrlStaticLabel: TLabel;
    procedure FetchBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure DownloadBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateProgress(ALen, APos: Int64; AHRLen, AHRPos: String);
    procedure UpdateStatus(Status: TStatus; ResponseCode: Integer; Msg: String; Body: String);
  private
    LazSimpleHTTPsGet: TLazSimpleHTTPsGet;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

// Download button clicked, start download:
procedure TMainForm.DownloadBtnClick(Sender: TObject);
var
  Filename: String;
begin
  try
    if Length(URLEdit.Text) > 0 then
    begin
      if not Assigned(LazSimpleHTTPsGet) then
      begin
        LazSimpleHTTPsGet := TLazSimpleHTTPsGet.Create();
      end;

      Filename := LazSimpleHTTPsGet.GetFileNameFromURL(URLEdit.Text);
      SaveDialog.FileName := Filename;
      if (SaveDialog.Execute) then
      begin
        LazSimpleHTTPsGet.URL := URLEdit.Text;
        LazSimpleHTTPsGet.Filename := SaveDialog.FileName;
        LazSimpleHTTPsGet.OnProgress := @UpdateProgress;
        LazSimpleHTTPsGet.onStatus := @UpdateStatus;
        if LazSimpleHTTPsGet.Get then
        begin
          DownloadBtn.Enabled := False;
          StopBtn.Enabled := True;
        end
        else
        begin
          ShowMessage('Download already running');
        end;
      end;
    end
    else
    begin
      ShowMessage('Please enter a url');
    end;
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
    end;
  end;
end;

// Stop button clicked, stop already running download:
procedure TMainForm.StopBtnClick(Sender: TObject);
begin
  if Assigned(LazSimpleHTTPsGet) then
  begin
    StatusInfoMemo.Append('Download stopped');
    FreeAndNil(LazSimpleHTTPsGet);
    DownloadBtn.Enabled := True;
    StopBtn.Enabled := False;
    DownloadProgressLabel.Caption := '-/-';
    ProgressBar.Position := 0;
  end;
end;

procedure TMainForm.FetchBtnClick(Sender: TObject);
begin
  try
    if Length(URLEdit.Text) > 0 then
    begin
      if not Assigned(LazSimpleHTTPsGet) then
      begin
        LazSimpleHTTPsGet := TLazSimpleHTTPsGet.Create();
      end;

      LazSimpleHTTPsGet.URL := URLEdit.Text;
      LazSimpleHTTPsGet.OnProgress := @UpdateProgress;
      LazSimpleHTTPsGet.onStatus := @UpdateStatus;
      if LazSimpleHTTPsGet.Fetch then
      begin
        DownloadBtn.Enabled := False;
        StopBtn.Enabled := True;
      end
      else
      begin
        ShowMessage('Download already running');
      end;
    end
    else
    begin
      ShowMessage('Please enter a url');
    end;
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
    end;
  end;
end;

// Form closed, stop downloads and free and nil LazSimpleHTTPsGet:
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(LazSimpleHTTPsGet) then
  begin
    FreeAndNil(LazSimpleHTTPsGet);
  end;
end;

// Update event (progress of download):
procedure TMainForm.UpdateProgress(ALen, APos: Int64; AHRLen, AHRPos: String);
var
  currentPercent: Double;
begin
  if ALen > 0 then
  begin
    currentPercent:= (APos * 100) / ALen;
    ProgressBar.Position:= round(currentPercent);
    DownloadProgressLabel.Caption := AHRPos + ' / ' + AHRLen;
  end;
end;

// Status event (download start, done, error):
procedure TMainForm.UpdateStatus(Status: TStatus; ResponseCode: Integer; Msg: String; Body: String);
begin
  if Status = TStatus.sStart then
  begin
    StatusInfoMemo.Append('Download started');
  end
  else if Status = TStatus.sDone then
  begin
    StatusInfoMemo.Append('Download done -> (HTTP Status: ' + IntToStr(ResponseCode) + '/' + Msg + ')');
    if Length(Body) > 0 then
    begin
      StatusInfoMemo.Append('-- Body Start ------------------');
      StatusInfoMemo.Append(Body);
      StatusInfoMemo.Append('-- Body End --------------------');
    end;
    DownloadBtn.Enabled := True;
    StopBtn.Enabled := False;
    DownloadProgressLabel.Caption := '-/-';
    ProgressBar.Position := 0;
  end
  else if Status = TStatus.sError then
  begin
    if ResponseCode < 1000 then
      StatusInfoMemo.Append('Error -> (HTTP Status: ' + IntToStr(ResponseCode) + '/' + Msg + ')')
    else
      StatusInfoMemo.Append('Error -> (Error Code: ' + IntToStr(ResponseCode) + '/' + Msg + ')');
  end;
end;

end.

