{ ============================================================================
  Basic Webcam Demo
  ============================================================================
  Demonstrates basic usage of the TWebcamCapture component:
    - Enumerate and select webcam devices
    - Start/stop live capture
    - Display live preview in a TImage
    - Take snapshots and save to file
    - Choose between BMP and JPEG output
    - Adjust resolution and frame rate
  ============================================================================ }

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.jpeg, Vcl.ComCtrls,
  WebcamCapture;

type
  TForm1 = class(TForm)
    PanelTop: TPanel;
    PanelPreview: TPanel;
    ImagePreview: TImage;
    LabelDevice: TLabel;
    ComboDevices: TComboBox;
    BtnRefresh: TButton;
    BtnStart: TButton;
    BtnStop: TButton;
    BtnSnapshot: TButton;
    LabelResolution: TLabel;
    ComboResolution: TComboBox;
    LabelFrameRate: TLabel;
    ComboFrameRate: TComboBox;
    LabelFormat: TLabel;
    ComboFormat: TComboBox;
    LabelStatus: TLabel;
    LabelFPS: TLabel;
    SaveDialog1: TSaveDialog;
    TimerFPS: TTimer;
    LabelJPEGQuality: TLabel;
    TrackBarQuality: TTrackBar;
    LabelQualityValue: TLabel;
    WebcamCapture1: TWebcamCapture;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure BtnSnapshotClick(Sender: TObject);
    procedure ComboDevicesChange(Sender: TObject);
    procedure TimerFPSTimer(Sender: TObject);
    procedure TrackBarQualityChange(Sender: TObject);
    procedure WebcamFrameReceived(Sender: TObject; const FrameData: TBytes;
      Width, Height: Integer);
    procedure WebcamCaptureStarted(Sender: TObject);
    procedure WebcamCaptureStopped(Sender: TObject);
    procedure WebcamCaptureError(Sender: TObject; const ErrorMessage: string);
  private
    FLastFrameCount: Int64;
    FDisplayBitmap: TBitmap; // Persistent bitmap to avoid flicker
    procedure PopulateDevices;
    procedure PopulateResolutions;
    procedure UpdateUI;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;

  FDisplayBitmap := TBitmap.Create;
  FDisplayBitmap.PixelFormat := pf24bit;

  // Default settings
  ComboFrameRate.Items.AddObject('15 FPS', TObject(15));
  ComboFrameRate.Items.AddObject('24 FPS', TObject(24));
  ComboFrameRate.Items.AddObject('30 FPS', TObject(30));
  ComboFrameRate.Items.AddObject('60 FPS', TObject(60));
  ComboFrameRate.ItemIndex := 2; // 30 FPS default

  ComboFormat.Items.Add('JPEG');
  ComboFormat.Items.Add('BMP');
  ComboFormat.ItemIndex := 0;

  TrackBarQuality.Min := 1;
  TrackBarQuality.Max := 100;
  TrackBarQuality.Position := 85;
  LabelQualityValue.Caption := '85%';

  PopulateDevices;
  UpdateUI;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if WebcamCapture1.Active then
    WebcamCapture1.StopCapture;
  FDisplayBitmap.Free;
end;

procedure TForm1.PopulateDevices;
var
  Devices: TWebcamDeviceArray;
  I: Integer;
begin
  ComboDevices.Items.Clear;
  Devices := WebcamCapture1.GetDevices;

  if Length(Devices) = 0 then
  begin
    ComboDevices.Items.Add('(No cameras detected)');
    ComboDevices.ItemIndex := 0;
    ComboDevices.Enabled := False;
  end
  else
  begin
    for I := 0 to High(Devices) do
      ComboDevices.Items.Add(Format('[%d] %s', [Devices[I].Index, Devices[I].Name]));
    ComboDevices.ItemIndex := 0;
    ComboDevices.Enabled := True;
    WebcamCapture1.DeviceIndex := 0;
    PopulateResolutions;
  end;
end;

procedure TForm1.PopulateResolutions;
var
  Resolutions: TWebcamResolutionArray;
  I: Integer;
begin
  ComboResolution.Items.Clear;

  // Add common defaults
  ComboResolution.Items.AddObject('320x240', TObject(320 or (240 shl 16)));
  ComboResolution.Items.AddObject('640x480', TObject(640 or (480 shl 16)));
  ComboResolution.Items.AddObject('800x600', TObject(800 or (600 shl 16)));
  ComboResolution.Items.AddObject('1280x720 (HD)', TObject(1280 or (720 shl 16)));
  ComboResolution.Items.AddObject('1920x1080 (Full HD)', TObject(1920 or (1080 shl 16)));

  // Try to query supported resolutions from the device
  Resolutions := WebcamCapture1.GetSupportedResolutions;
  if Length(Resolutions) > 0 then
  begin
    ComboResolution.Items.Clear;
    for I := 0 to High(Resolutions) do
      ComboResolution.Items.AddObject(
        Resolutions[I].ToString,
        TObject(Resolutions[I].Width or (Resolutions[I].Height shl 16))
      );
  end;

  // Select 640x480 by default, or first item
  ComboResolution.ItemIndex := ComboResolution.Items.IndexOf('640x480');
  if ComboResolution.ItemIndex < 0 then
    ComboResolution.ItemIndex := 0;
end;

procedure TForm1.BtnRefreshClick(Sender: TObject);
begin
  WebcamCapture1.RefreshDevices;
  PopulateDevices;
  LabelStatus.Caption := 'Devices refreshed';
end;

procedure TForm1.BtnStartClick(Sender: TObject);
var
  ResData: NativeInt;
begin
  if ComboDevices.ItemIndex < 0 then
    Exit;

  // Apply settings
  WebcamCapture1.DeviceIndex := ComboDevices.ItemIndex;

  if ComboResolution.ItemIndex >= 0 then
  begin
    ResData := NativeInt(ComboResolution.Items.Objects[ComboResolution.ItemIndex]);
    WebcamCapture1.Width := ResData and $FFFF;
    WebcamCapture1.Height := (ResData shr 16) and $FFFF;
  end;

  if ComboFrameRate.ItemIndex >= 0 then
    WebcamCapture1.FrameRate := Integer(ComboFrameRate.Items.Objects[ComboFrameRate.ItemIndex]);

  if ComboFormat.ItemIndex = 0 then
    WebcamCapture1.ImageFormat := wifJPEG
  else
    WebcamCapture1.ImageFormat := wifBMP;

  WebcamCapture1.JPEGQuality := TrackBarQuality.Position;

  FLastFrameCount := 0;
  WebcamCapture1.StartCapture;
end;

procedure TForm1.BtnStopClick(Sender: TObject);
begin
  WebcamCapture1.StopCapture;
end;

procedure TForm1.BtnSnapshotClick(Sender: TObject);
var
  FrameBytes: TBytes;
  FS: TFileStream;
begin
  FrameBytes := WebcamCapture1.GetSingleFrame;
  if Length(FrameBytes) = 0 then
  begin
    ShowMessage('No frame available. Start capture first.');
    Exit;
  end;

  if WebcamCapture1.ImageFormat = wifJPEG then
  begin
    SaveDialog1.Filter := 'JPEG Image|*.jpg';
    SaveDialog1.DefaultExt := 'jpg';
  end
  else
  begin
    SaveDialog1.Filter := 'Bitmap Image|*.bmp';
    SaveDialog1.DefaultExt := 'bmp';
  end;

  if SaveDialog1.Execute then
  begin
    FS := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      FS.WriteBuffer(FrameBytes[0], Length(FrameBytes));
    finally
      FS.Free;
    end;
    LabelStatus.Caption := 'Snapshot saved: ' + ExtractFileName(SaveDialog1.FileName);
  end;
end;

procedure TForm1.ComboDevicesChange(Sender: TObject);
begin
  if ComboDevices.ItemIndex >= 0 then
  begin
    WebcamCapture1.DeviceIndex := ComboDevices.ItemIndex;
    PopulateResolutions;
  end;
end;

procedure TForm1.TrackBarQualityChange(Sender: TObject);
begin
  LabelQualityValue.Caption := IntToStr(TrackBarQuality.Position) + '%';
  WebcamCapture1.JPEGQuality := TrackBarQuality.Position;
end;

procedure TForm1.TimerFPSTimer(Sender: TObject);
var
  CurrentFrames: Int64;
  FPS: Integer;
begin
  if WebcamCapture1.Active then
  begin
    CurrentFrames := WebcamCapture1.FrameCount;
    FPS := Integer(CurrentFrames - FLastFrameCount);
    FLastFrameCount := CurrentFrames;
    LabelFPS.Caption := Format('FPS: %d | Frames: %d | Resolution: %dx%d',
      [FPS, CurrentFrames, WebcamCapture1.ActualWidth, WebcamCapture1.ActualHeight]);
  end
  else
    LabelFPS.Caption := '';
end;

procedure TForm1.WebcamFrameReceived(Sender: TObject; const FrameData: TBytes;
  Width, Height: Integer);
var
  Stream: TMemoryStream;
  JPEG: TJPEGImage;
  TempBMP: TBitmap;
begin
  if Length(FrameData) = 0 then Exit;

  // Decode frame into persistent FDisplayBitmap, then assign once.
  // This avoids the flicker caused by Image.Picture.Assign(JPEG) which
  // internally clears the old graphic before painting the new one.
  Stream := TMemoryStream.Create;
  try
    Stream.WriteBuffer(FrameData[0], Length(FrameData));
    Stream.Position := 0;

    if WebcamCapture1.ImageFormat = wifJPEG then
    begin
      JPEG := TJPEGImage.Create;
      try
        JPEG.Performance := jpBestSpeed;
        JPEG.LoadFromStream(Stream);
        FDisplayBitmap.Assign(JPEG); // Decode JPEG -> bitmap (This was used to remove flickering!)
      finally
        JPEG.Free;
      end;
    end
    else
    begin
      TempBMP := TBitmap.Create;
      try
        TempBMP.LoadFromStream(Stream);
        FDisplayBitmap.Assign(TempBMP);
      finally
        TempBMP.Free;
      end;
    end;
  finally
    Stream.Free;
  end;

  // assign of the fully-rendered bitmap to the TImage
  ImagePreview.Picture.Bitmap.Assign(FDisplayBitmap);
end;

procedure TForm1.WebcamCaptureStarted(Sender: TObject);
begin
  LabelStatus.Caption := 'Capture started';
  UpdateUI;
end;

procedure TForm1.WebcamCaptureStopped(Sender: TObject);
begin
  LabelStatus.Caption := 'Capture stopped';
  UpdateUI;
end;

procedure TForm1.WebcamCaptureError(Sender: TObject; const ErrorMessage: string);
begin
  LabelStatus.Caption := 'Error: ' + ErrorMessage;
  ShowMessage('Webcam Error: ' + ErrorMessage);
end;

procedure TForm1.UpdateUI;
begin
  BtnStart.Enabled := not WebcamCapture1.Active;
  BtnStop.Enabled := WebcamCapture1.Active;
  BtnSnapshot.Enabled := WebcamCapture1.Active;
  ComboDevices.Enabled := not WebcamCapture1.Active;
  ComboResolution.Enabled := not WebcamCapture1.Active;
  ComboFrameRate.Enabled := not WebcamCapture1.Active;
end;

end.
