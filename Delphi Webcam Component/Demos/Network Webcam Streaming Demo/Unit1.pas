{ ============================================================================
  Network Webcam Streaming Demo - Unit1.pas
  ============================================================================
  Demonstrates sending webcam frames (TBytes) over TCP using NetCom7
  TncServerSource / TncClientSource components.

  This demo has two modes:
    SERVER: Captures webcam and broadcasts JPEG frames to connected clients
    CLIENT: Connects to a server and displays received frames

  ============================================================================ }

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.jpeg,
  Vcl.ComCtrls, WebcamCapture, ncSources, ncSocketList;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabServer: TTabSheet;
    TabClient: TTabSheet;
    { Server controls }
    PanelServerTop: TPanel;
    LabelServerDevice: TLabel;
    ComboServerDevices: TComboBox;
    BtnServerRefresh: TButton;
    LabelServerPort: TLabel;
    EditServerPort: TEdit;
    BtnServerStart: TButton;
    BtnServerStop: TButton;
    LabelServerStatus: TLabel;
    PanelServerPreview: TPanel;
    ImageServerPreview: TImage;
    LabelServerClients: TLabel;
    LabelServerFPS: TLabel;
    { Client controls }
    PanelClientTop: TPanel;
    LabelClientHost: TLabel;
    EditClientHost: TEdit;
    LabelClientPort: TLabel;
    EditClientPort: TEdit;
    BtnClientConnect: TButton;
    BtnClientDisconnect: TButton;
    LabelClientStatus: TLabel;
    PanelClientPreview: TPanel;
    ImageClientPreview: TImage;
    LabelClientFPS: TLabel;
    { Timers }
    TimerFPS: TTimer;
    WebcamCapture1: TWebcamCapture;
    ncClientSource1: TncClientSource;
    ncServerSource1: TncServerSource;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnServerRefreshClick(Sender: TObject);
    procedure BtnServerStartClick(Sender: TObject);
    procedure BtnServerStopClick(Sender: TObject);
    procedure BtnClientConnectClick(Sender: TObject);
    procedure BtnClientDisconnectClick(Sender: TObject);
    procedure TimerFPSTimer(Sender: TObject);
    procedure ServerSourceConnected(Sender: TObject; aLine: TncLine);
    procedure ServerSourceDisconnected(Sender: TObject; aLine: TncLine);
    procedure ClientSourceConnected(Sender: TObject; aLine: TncLine);
    procedure ClientSourceDisconnected(Sender: TObject; aLine: TncLine);
    function ClientSourceHandleCommand(Sender: TObject; aLine: TncLine;
      aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
      const aSenderComponent, aReceiverComponent: string): TBytes;
    procedure WebcamFrameReceived(Sender: TObject; const FrameData: TBytes;
      Width, Height: Integer);
    procedure WebcamCaptureError(Sender: TObject; const ErrorMessage: string);
  private
    FServerFrameCount: Int64;
    FLastServerFrameCount: Int64;
    FClientFrameCount: Int64;
    FLastClientFrameCount: Int64;
    FServerDisplayBmp: TBitmap;
    FClientDisplayBmp: TBitmap;

    { Helpers }
    procedure DisplayFrame(Image: TImage; DisplayBmp: TBitmap; const FrameData: TBytes);
    procedure PopulateDevices;
    procedure UpdateServerClientCount;
    procedure UpdateServerUI;
    procedure UpdateClientUI;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  CmdStreamFrame = 1;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;

  FServerDisplayBmp := TBitmap.Create;
  FServerDisplayBmp.PixelFormat := pf24bit;
  FClientDisplayBmp := TBitmap.Create;
  FClientDisplayBmp.PixelFormat := pf24bit;

  WebcamCapture1.ImageFormat := wifJPEG;
  WebcamCapture1.JPEGQuality := 70;
  WebcamCapture1.Width := 640;
  WebcamCapture1.Height := 480;
  WebcamCapture1.FrameRate := 24;

  ncServerSource1.EventsUseMainThread := True;
  ncClientSource1.EventsUseMainThread := True;
  ncClientSource1.Reconnect := False;
  ncServerSource1.Port := StrToIntDef(EditServerPort.Text, 9090);
  ncClientSource1.Host := EditClientHost.Text;
  ncClientSource1.Port := StrToIntDef(EditClientPort.Text, 9090);

  PopulateDevices;
  UpdateServerClientCount;
  UpdateServerUI;
  UpdateClientUI;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if WebcamCapture1.Active then
    WebcamCapture1.StopCapture;
  if ncServerSource1.Active then
    ncServerSource1.Active := False;
  if ncClientSource1.Active then
    ncClientSource1.Active := False;
  FServerDisplayBmp.Free;
  FClientDisplayBmp.Free;
end;

procedure TForm1.PopulateDevices;
var
  Devices: TWebcamDeviceArray;
  I: Integer;
begin
  ComboServerDevices.Items.Clear;
  Devices := WebcamCapture1.GetDevices;

  if Length(Devices) = 0 then
  begin
    ComboServerDevices.Items.Add('(No cameras detected)');
    ComboServerDevices.Enabled := False;
  end
  else
  begin
    for I := 0 to High(Devices) do
      ComboServerDevices.Items.Add(Format('[%d] %s', [Devices[I].Index, Devices[I].Name]));
    ComboServerDevices.Enabled := True;
  end;
  ComboServerDevices.ItemIndex := 0;
end;

{ ============================================================================
  Server Side
  ============================================================================ }

procedure TForm1.BtnServerRefreshClick(Sender: TObject);
begin
  WebcamCapture1.RefreshDevices;
  PopulateDevices;
end;

procedure TForm1.BtnServerStartClick(Sender: TObject);
begin
  if ComboServerDevices.ItemIndex < 0 then
    Exit;

  WebcamCapture1.DeviceIndex := ComboServerDevices.ItemIndex;
  try
    ncServerSource1.Port := StrToIntDef(EditServerPort.Text, 9090);
    ncServerSource1.Active := True;

    FServerFrameCount := 0;
    FLastServerFrameCount := 0;
    WebcamCapture1.StartCapture;
  except
    on E: Exception do
    begin
      if WebcamCapture1.Active then
        WebcamCapture1.StopCapture;
      if ncServerSource1.Active then
        ncServerSource1.Active := False;
      LabelServerStatus.Caption := 'Server start error: ' + E.Message;
      UpdateServerClientCount;
      UpdateServerUI;
      Exit;
    end;
  end;

  LabelServerStatus.Caption := Format('Server listening on port %d - Webcam active',
    [ncServerSource1.Port]);
  UpdateServerClientCount;
  UpdateServerUI;
end;

procedure TForm1.BtnServerStopClick(Sender: TObject);
begin
  if WebcamCapture1.Active then
    WebcamCapture1.StopCapture;
  ncServerSource1.Active := False;
  LabelServerStatus.Caption := 'Server stopped';
  UpdateServerClientCount;
  UpdateServerUI;
end;

procedure TForm1.ServerSourceConnected(Sender: TObject; aLine: TncLine);
begin
  UpdateServerClientCount;
  LabelServerStatus.Caption := Format('Client connected from %s',
    [aLine.PeerIP]);
end;

procedure TForm1.ServerSourceDisconnected(Sender: TObject; aLine: TncLine);
begin
  UpdateServerClientCount;
  if ncServerSource1.Active then
    LabelServerStatus.Caption := Format('Client disconnected: %s', [aLine.PeerIP]);
end;

procedure TForm1.WebcamFrameReceived(Sender: TObject; const FrameData: TBytes;
  Width, Height: Integer);
var
  Clients: TSocketList;
  I: Integer;
begin
  Inc(FServerFrameCount);

  DisplayFrame(ImageServerPreview, FServerDisplayBmp, FrameData);

  if ncServerSource1.Active then
  begin
    Clients := ncServerSource1.Lines.LockList;
    try
      for I := 0 to Clients.Count - 1 do
      begin
        try
          ncServerSource1.ExecCommand(Clients.Lines[I], CmdStreamFrame, FrameData, False);
        except
          { Ignore transient per-client send failures. }
        end;
      end;
    finally
      ncServerSource1.Lines.UnlockList;
    end;
  end;
end;

procedure TForm1.WebcamCaptureError(Sender: TObject; const ErrorMessage: string);
begin
  LabelServerStatus.Caption := 'Error: ' + ErrorMessage;
end;

{ ============================================================================
  Client Side
  ============================================================================ }

procedure TForm1.BtnClientConnectClick(Sender: TObject);
begin
  ncClientSource1.Host := EditClientHost.Text;
  ncClientSource1.Port := StrToIntDef(EditClientPort.Text, 9090);
  FClientFrameCount := 0;
  FLastClientFrameCount := 0;
  try
    ncClientSource1.Active := True;
  except
    on E: Exception do
    begin
      LabelClientStatus.Caption := 'Connect error: ' + E.Message;
      UpdateClientUI;
      Exit;
    end;
  end;
  UpdateClientUI;
end;

procedure TForm1.BtnClientDisconnectClick(Sender: TObject);
begin
  ncClientSource1.Active := False;
end;

procedure TForm1.ClientSourceConnected(Sender: TObject; aLine: TncLine);
begin
  LabelClientStatus.Caption := Format('Connected to %s:%d',
    [aLine.PeerIP, ncClientSource1.Port]);
  UpdateClientUI;
end;

procedure TForm1.ClientSourceDisconnected(Sender: TObject; aLine: TncLine);
begin
  LabelClientStatus.Caption := 'Disconnected';
  UpdateClientUI;
end;

function TForm1.ClientSourceHandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
begin
  SetLength(Result, 0);
  if aCmd = CmdStreamFrame then
  begin
    Inc(FClientFrameCount);
    DisplayFrame(ImageClientPreview, FClientDisplayBmp, aData);
  end;
end;

{ ============================================================================
  Shared Helpers
  ============================================================================ }

procedure TForm1.DisplayFrame(Image: TImage; DisplayBmp: TBitmap; const FrameData: TBytes);
var
  Stream: TMemoryStream;
  JPEG: TJPEGImage;
begin
  if Length(FrameData) = 0 then
    Exit;

  Stream := TMemoryStream.Create;
  try
    Stream.WriteBuffer(FrameData[0], Length(FrameData));
    Stream.Position := 0;

    JPEG := TJPEGImage.Create;
    try
      JPEG.Performance := jpBestSpeed;
      JPEG.LoadFromStream(Stream);
      DisplayBmp.Assign(JPEG);
    finally
      JPEG.Free;
    end;
  finally
    Stream.Free;
  end;

  Image.Picture.Bitmap.Assign(DisplayBmp);
end;

procedure TForm1.UpdateServerClientCount;
var
  Clients: TSocketList;
begin
  if not ncServerSource1.Active then
  begin
    LabelServerClients.Caption := 'Clients: 0';
    Exit;
  end;

  Clients := ncServerSource1.Lines.LockList;
  try
    LabelServerClients.Caption := Format('Clients: %d', [Clients.Count]);
  finally
    ncServerSource1.Lines.UnlockList;
  end;
end;

procedure TForm1.TimerFPSTimer(Sender: TObject);
begin
  if WebcamCapture1.Active then
  begin
    LabelServerFPS.Caption := Format('Server FPS: %d | Frames: %d',
      [FServerFrameCount - FLastServerFrameCount, FServerFrameCount]);
    FLastServerFrameCount := FServerFrameCount;
  end;
  if not WebcamCapture1.Active then
    LabelServerFPS.Caption := '';

  if ncClientSource1.Active then
  begin
    LabelClientFPS.Caption := Format('Client FPS: %d | Frames: %d',
      [FClientFrameCount - FLastClientFrameCount, FClientFrameCount]);
    FLastClientFrameCount := FClientFrameCount;
  end;
  if not ncClientSource1.Active then
    LabelClientFPS.Caption := '';
end;

procedure TForm1.UpdateServerUI;
begin
  BtnServerStart.Enabled := ComboServerDevices.Enabled and not WebcamCapture1.Active;
  BtnServerStop.Enabled := WebcamCapture1.Active;
  BtnServerRefresh.Enabled := not WebcamCapture1.Active;
  EditServerPort.Enabled := not ncServerSource1.Active;
end;

procedure TForm1.UpdateClientUI;
begin
  BtnClientConnect.Enabled := not ncClientSource1.Active;
  BtnClientDisconnect.Enabled := ncClientSource1.Active;
  EditClientHost.Enabled := not ncClientSource1.Active;
  EditClientPort.Enabled := not ncClientSource1.Active;
end;

end.
