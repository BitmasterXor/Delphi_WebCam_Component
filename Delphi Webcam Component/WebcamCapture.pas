{ ============================================================================
  WebcamCapture.pas - TWebcamCapture VCL Component
  ============================================================================
  Professional webcam capture component using DirectShow.

  Architecture:
    - A dedicated TCaptureThread handles ALL DirectShow and COM operations
    - The main VCL thread is NEVER blocked by camera I/O
    - Frame encoding (JPEG/BMP) happens on the capture thread
    - Finished TBytes are delivered to main thread via TThread.Queue
    - Device enumeration uses safe STA COM init compatible with VCL

  Features:
    - Enumerate and select video capture devices by index
    - Capture frames as TBytes (JPEG or BMP format)
    - Configurable resolution, frame rate, and output format
    - Thread-safe frame delivery via non-blocking Queue
    - Single-frame snapshot support
  ============================================================================ }

unit WebcamCapture;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Types,
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX,
  Vcl.Graphics, Vcl.Controls, Vcl.ExtCtrls;

{ ============================================================================
  DirectShow Constants
  ============================================================================ }
const
  CLSID_VideoInputDeviceCategory: TGUID = '{860BB310-5D01-11D0-BD3B-00A0C911CE86}';
  CLSID_SystemDeviceEnum:         TGUID = '{62BE5D10-60EB-11D0-BD3B-00A0C911CE86}';
  CLSID_FilterGraph:              TGUID = '{E436EBB3-524F-11CE-9F53-0020AF0BA770}';
  CLSID_CaptureGraphBuilder2:     TGUID = '{BF87B6E1-8C27-11D0-B3F0-00AA003761C5}';
  CLSID_SampleGrabber:            TGUID = '{C1F400A0-3F08-11D3-9F0B-006008039E37}';
  CLSID_NullRenderer:             TGUID = '{C1F400A4-3F08-11D3-9F0B-006008039E37}';

  IID_IPropertyBag:     TGUID = '{55272A00-42CB-11CE-8135-00AA004BB851}';
  IID_IBaseFilter:      TGUID = '{56A86895-0AD4-11CE-B03A-0020AF0BA770}';
  IID_ISampleGrabber:   TGUID = '{6B652FFF-11FE-4FCE-92AD-0266B5D7C78F}';
  IID_IAMStreamConfig:  TGUID = '{C6E13340-30AC-11D0-A18C-00A0C9118956}';
  IID_ICreateDevEnum:   TGUID = '{29840822-5B84-11D0-BD3B-00A0C911CE86}';

  MEDIATYPE_Video:    TGUID = '{73646976-0000-0010-8000-00AA00389B71}';
  MEDIASUBTYPE_RGB24: TGUID = '{E436EB7D-524F-11CE-9F53-0020AF0BA770}';
  FORMAT_VideoInfo:   TGUID = '{05589F80-C356-11CE-BF01-00AA0055595A}';

  PIN_CATEGORY_CAPTURE: TGUID = '{FB6C4281-0353-11D1-905F-0000C0CC16BA}';

{ ============================================================================
  DirectShow Types and Interfaces
  ============================================================================ }
type
  TAM_MEDIA_TYPE = record
    majortype: TGUID;
    subtype: TGUID;
    bFixedSizeSamples: BOOL;
    bTemporalCompression: BOOL;
    lSampleSize: ULONG;
    formattype: TGUID;
    pUnk: IUnknown;
    cbFormat: ULONG;
    pbFormat: Pointer;
  end;
  PAM_MEDIA_TYPE = ^TAM_MEDIA_TYPE;

  TVideoInfoHeader = record
    rcSource: TRect;
    rcTarget: TRect;
    dwBitRate: DWORD;
    dwBitErrorRate: DWORD;
    AvgTimePerFrame: Int64;
    bmiHeader: TBitmapInfoHeader;
  end;
  PVideoInfoHeader = ^TVideoInfoHeader;

  TVIDEO_STREAM_CONFIG_CAPS = record
    guid: TGUID;
    VideoStandard: ULONG;
    InputSize: TSize;
    MinCroppingSize: TSize;
    MaxCroppingSize: TSize;
    CropGranularityX: Integer;
    CropGranularityY: Integer;
    CropAlignX: Integer;
    CropAlignY: Integer;
    MinOutputSize: TSize;
    MaxOutputSize: TSize;
    OutputGranularityX: Integer;
    OutputGranularityY: Integer;
    StretchTapsX: Integer;
    StretchTapsY: Integer;
    ShrinkTapsX: Integer;
    ShrinkTapsY: Integer;
    MinFrameInterval: Int64;
    MaxFrameInterval: Int64;
    MinBitsPerSecond: Integer;
    MaxBitsPerSecond: Integer;
  end;

  ICreateDevEnum = interface(IUnknown)
    ['{29840822-5B84-11D0-BD3B-00A0C911CE86}']
    function CreateClassEnumerator(const clsidDeviceClass: TGUID;
      out ppEnumMoniker: IEnumMoniker; dwFlags: DWORD): HRESULT; stdcall;
  end;

  IEnumPins = interface(IUnknown)
    ['{56A86892-0AD4-11CE-B03A-0020AF0BA770}']
    function Next(cPins: ULONG; out ppPins: IUnknown; out pcFetched: ULONG): HRESULT; stdcall;
    function Skip(cPins: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumPins): HRESULT; stdcall;
  end;

  IPin = interface(IUnknown)
    ['{56A86891-0AD4-11CE-B03A-0020AF0BA770}']
    function Connect(pReceivePin: IPin; const pmt: PAM_MEDIA_TYPE): HRESULT; stdcall;
    function ReceiveConnection(pConnector: IPin; const pmt: TAM_MEDIA_TYPE): HRESULT; stdcall;
    function Disconnect: HRESULT; stdcall;
    function ConnectedTo(out pPin: IPin): HRESULT; stdcall;
    function ConnectionMediaType(out pmt: TAM_MEDIA_TYPE): HRESULT; stdcall;
    function QueryPinInfo(out pInfo: Pointer): HRESULT; stdcall;
    function QueryDirection(out pPinDir: ULONG): HRESULT; stdcall;
    function QueryId(out Id: PWideChar): HRESULT; stdcall;
    function QueryAccept(const pmt: TAM_MEDIA_TYPE): HRESULT; stdcall;
    function EnumMediaTypes(out ppEnum: IUnknown): HRESULT; stdcall;
    function QueryInternalConnections(out apPin: IPin; var nPin: ULONG): HRESULT; stdcall;
    function EndOfStream: HRESULT; stdcall;
    function BeginFlush: HRESULT; stdcall;
    function EndFlush: HRESULT; stdcall;
    function NewSegment(tStart, tStop: Int64; dRate: Double): HRESULT; stdcall;
  end;

  IEnumFilters = interface(IUnknown)
    ['{56A86893-0AD4-11CE-B03A-0020AF0BA770}']
    function Next(cFilters: ULONG; out ppFilter: IUnknown; out pcFetched: ULONG): HRESULT; stdcall;
    function Skip(cFilters: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumFilters): HRESULT; stdcall;
  end;

  IBaseFilter = interface(IUnknown)
    ['{56A86895-0AD4-11CE-B03A-0020AF0BA770}']
    function GetClassID(out pClassID: TGUID): HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Run(tStart: Int64): HRESULT; stdcall;
    function GetState(dwMilliSecsTimeout: DWORD; out State: ULONG): HRESULT; stdcall;
    function SetSyncSource(pClock: IUnknown): HRESULT; stdcall;
    function GetSyncSource(out pClock: IUnknown): HRESULT; stdcall;
    function EnumPins(out ppEnum: IEnumPins): HRESULT; stdcall;
    function FindPin(Id: PWideChar; out ppPin: IPin): HRESULT; stdcall;
    function QueryFilterInfo(out pInfo: Pointer): HRESULT; stdcall;
    function JoinFilterGraph(pGraph: IUnknown; pName: PWideChar): HRESULT; stdcall;
    function QueryVendorInfo(out pVendorInfo: PWideChar): HRESULT; stdcall;
  end;

  IFilterGraph = interface(IUnknown)
    ['{56A8689F-0AD4-11CE-B03A-0020AF0BA770}']
    function AddFilter(pFilter: IBaseFilter; pName: PWideChar): HRESULT; stdcall;
    function RemoveFilter(pFilter: IBaseFilter): HRESULT; stdcall;
    function EnumFilters(out ppEnum: IEnumFilters): HRESULT; stdcall;
    function FindFilterByName(pName: PWideChar; out ppFilter: IBaseFilter): HRESULT; stdcall;
    function ConnectDirect(ppinOut, ppinIn: IPin; const pmt: PAM_MEDIA_TYPE): HRESULT; stdcall;
    function Reconnect(ppin: IPin): HRESULT; stdcall;
    function Disconnect(ppin: IPin): HRESULT; stdcall;
    function SetDefaultSyncSource: HRESULT; stdcall;
  end;

  IGraphBuilder = interface(IFilterGraph)
    ['{56A868A9-0AD4-11CE-B03A-0020AF0BA770}']
    function Connect(ppinOut, ppinIn: IPin): HRESULT; stdcall;
    function Render(ppinOut: IPin): HRESULT; stdcall;
    function RenderFile(lpcwstrFile: PWideChar; lpcwstrPlayList: PWideChar): HRESULT; stdcall;
    function AddSourceFilter(lpcwstrFileName: PWideChar; lpcwstrFilterName: PWideChar;
      out ppFilter: IBaseFilter): HRESULT; stdcall;
    function SetLogFile(hFile: THandle): HRESULT; stdcall;
    function Abort: HRESULT; stdcall;
    function ShouldOperationContinue: HRESULT; stdcall;
  end;

  IMediaControl = interface(IDispatch)
    ['{56A868B1-0AD4-11CE-B03A-0020AF0BA770}']
    function Run: HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function GetState(msTimeout: LONG; out pfs: ULONG): HRESULT; stdcall;
    function RenderFile(strFilename: WideString): HRESULT; stdcall;
    function AddSourceFilter(strFilename: WideString; out ppUnk: IDispatch): HRESULT; stdcall;
    function get_FilterCollection(out ppUnk: IDispatch): HRESULT; stdcall;
    function get_RegFilterCollection(out ppUnk: IDispatch): HRESULT; stdcall;
    function StopWhenReady: HRESULT; stdcall;
  end;

  ICaptureGraphBuilder2 = interface(IUnknown)
    ['{93E5A4E0-2D50-11D2-ABFA-00A0C9C6E38D}']
    function SetFiltergraph(pfg: IGraphBuilder): HRESULT; stdcall;
    function GetFiltergraph(out ppfg: IGraphBuilder): HRESULT; stdcall;
    function SetOutputFileName(const pType: TGUID; lpstrFile: PWideChar;
      out ppf: IBaseFilter; out ppSink: IUnknown): HRESULT; stdcall;
    function FindInterface(const pCategory: PGUID; const pType: PGUID;
      pf: IBaseFilter; const riid: TGUID; out ppint: Pointer): HRESULT; stdcall;
    function RenderStream(const pCategory: PGUID; const pType: PGUID;
      pSource: IUnknown; pfCompressor: IBaseFilter;
      pfRenderer: IBaseFilter): HRESULT; stdcall;
    function ControlStream(const pCategory: PGUID; const pType: PGUID;
      pFilter: IBaseFilter; pstart: PInt64; pstop: PInt64;
      wStartCookie: Word; wStopCookie: Word): HRESULT; stdcall;
    function AllocCapFile(lpstr: PWideChar; dwlSize: Int64): HRESULT; stdcall;
    function CopyCaptureFile(lpwstrOld, lpwstrNew: PWideChar;
      fAllowEscAbort: Integer; pCallback: IUnknown): HRESULT; stdcall;
    function FindPin(pSource: IUnknown; pindir: Integer;
      const pCategory: PGUID; const pType: PGUID; fUnconnected: BOOL;
      num: Integer; out ppPin: IPin): HRESULT; stdcall;
  end;

  ISampleGrabberCB = interface(IUnknown)
    ['{0579154A-2B53-4994-B0D0-E773148EFF85}']
    function SampleCB(SampleTime: Double; pSample: IUnknown): HRESULT; stdcall;
    function BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: Integer): HRESULT; stdcall;
  end;

  ISampleGrabber = interface(IUnknown)
    ['{6B652FFF-11FE-4FCE-92AD-0266B5D7C78F}']
    function SetOneShot(OneShot: BOOL): HRESULT; stdcall;
    function SetMediaType(const pType: TAM_MEDIA_TYPE): HRESULT; stdcall;
    function GetConnectedMediaType(out pType: TAM_MEDIA_TYPE): HRESULT; stdcall;
    function SetBufferSamples(BufferThem: BOOL): HRESULT; stdcall;
    function GetCurrentBuffer(var pBufferSize: Integer; pBuffer: PByte): HRESULT; stdcall;
    function GetCurrentSample(out ppSample: IUnknown): HRESULT; stdcall;
    function SetCallback(pCallback: ISampleGrabberCB; WhichMethodToCallback: Integer): HRESULT; stdcall;
  end;

  IAMStreamConfig = interface(IUnknown)
    ['{C6E13340-30AC-11D0-A18C-00A0C9118956}']
    function SetFormat(const pmt: TAM_MEDIA_TYPE): HRESULT; stdcall;
    function GetFormat(out ppmt: PAM_MEDIA_TYPE): HRESULT; stdcall;
    function GetNumberOfCapabilities(out piCount: Integer; out piSize: Integer): HRESULT; stdcall;
    function GetStreamCaps(iIndex: Integer; out ppmt: PAM_MEDIA_TYPE;
      out pSCC: TVIDEO_STREAM_CONFIG_CAPS): HRESULT; stdcall;
  end;

{ ============================================================================
  Component Types
  ============================================================================ }

  { Forward declarations }
  TWebcamCapture = class;
  TCaptureThread = class;

  { Webcam device information record }
  TWebcamDevice = record
    Index: Integer;
    Name: string;
    DevicePath: string;
  end;
  TWebcamDeviceArray = TArray<TWebcamDevice>;

  { Supported image output formats }
  TWebcamImageFormat = (wifBMP, wifJPEG);

  { Resolution record }
  TWebcamResolution = record
    Width: Integer;
    Height: Integer;
    function ToString: string;
    class function Create(AWidth, AHeight: Integer): TWebcamResolution; static;
  end;
  TWebcamResolutionArray = TArray<TWebcamResolution>;

  { Frame data event - delivers captured frame as TBytes }
  TOnFrameReceived = procedure(Sender: TObject; const FrameData: TBytes;
    Width, Height: Integer) of object;

  { Device change notification }
  TOnDeviceListChanged = procedure(Sender: TObject; DeviceCount: Integer) of object;

  { Error notification }
  TOnCaptureError = procedure(Sender: TObject; const ErrorMessage: string) of object;

  { ============================================================================
    TSampleGrabberCB - DirectShow callback (runs on DS streaming thread)
    Copies the raw buffer and signals the capture thread to encode it.
    ============================================================================ }
  TSampleGrabberCB = class(TInterfacedObject, ISampleGrabberCB)
  private
    FOwner: TCaptureThread;
  public
    constructor Create(AOwner: TCaptureThread);
    function SampleCB(SampleTime: Double; pSample: IUnknown): HRESULT; stdcall;
    function BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: Integer): HRESULT; stdcall;
  end;

  { ============================================================================
    TCaptureThread - Dedicated thread for all DirectShow operations
    Owns COM init, graph building, frame encoding. Never touches VCL thread.
    ============================================================================ }
  TCaptureThread = class(TThread)
  private
    FOwner: TWebcamCapture;

    { DirectShow objects - created and destroyed on THIS thread only }
    FGraphBuilder: IGraphBuilder;
    FCaptureGraphBuilder: ICaptureGraphBuilder2;
    FMediaControl: IMediaControl;
    FCaptureFilter: IBaseFilter;
    FSampleGrabberFilter: IBaseFilter;
    FSampleGrabber: ISampleGrabber;
    FNullRenderer: IBaseFilter;
    FGrabberCB: TSampleGrabberCB;

    { Capture settings (copied from owner at start) }
    FDeviceIndex: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FFrameRate: Integer;
    FImageFormat: TWebcamImageFormat;
    FJPEGQuality: Integer;
    FActualWidth: Integer;
    FActualHeight: Integer;

    { Frame handoff from DirectShow callback to this thread }
    FRawBufferLock: TCriticalSection;
    FRawBuffer: PByte;
    FRawBufferLen: Integer;
    FFrameReadyEvent: TEvent;

    { Stop signal }
    FStopEvent: TEvent;
    { Startup handshake with owner }
    FStartCompletedEvent: TEvent;
    FStartSucceeded: Boolean;

    function BuildCaptureGraph: Boolean;
    function ConfigureStreamFormat: Boolean;
    procedure TeardownGraph;
    procedure ProcessFrame;
    procedure FireError(const Msg: string);

    function ConvertFrameToBytes(pBuffer: PByte; BufferLen: Integer;
      AWidth, AHeight: Integer): TBytes;
    function CreateBMPBytes(pBuffer: PByte; BufferLen: Integer;
      AWidth, AHeight: Integer): TBytes;
    function CreateJPEGBytes(pBuffer: PByte; BufferLen: Integer;
      AWidth, AHeight: Integer): TBytes;
  public
    constructor Create(AOwner: TWebcamCapture);
    destructor Destroy; override;
    procedure Execute; override;

    { Called by TSampleGrabberCB from DirectShow streaming thread }
    procedure OnBufferCB(pBuffer: PByte; BufferLen: Integer);
  end;

  { ============================================================================
    TWebcamCapture - Main webcam capture component
    ============================================================================ }
  TWebcamCapture = class(TComponent)
  private
    { State }
    FActive: Boolean;
    FDeviceIndex: Integer;
    FDevices: TWebcamDeviceArray;
    FDevicesEnumerated: Boolean;

    { Capture settings }
    FWidth: Integer;
    FHeight: Integer;
    FFrameRate: Integer;
    FImageFormat: TWebcamImageFormat;
    FJPEGQuality: Integer;

    { Frame data (thread-safe access) }
    FFrameLock: TCriticalSection;
    FLastFrameData: TBytes;
    FActualWidth: Integer;
    FActualHeight: Integer;
    FFrameCount: Int64;
    FFrameDispatchPending: Integer;

    { Capture thread }
    FCaptureThread: TCaptureThread;

    { Events }
    FOnFrameReceived: TOnFrameReceived;
    FOnDeviceListChanged: TOnDeviceListChanged;
    FOnCaptureError: TOnCaptureError;
    FOnCaptureStarted: TNotifyEvent;
    FOnCaptureStopped: TNotifyEvent;

    { Property setters }
    procedure SetActive(Value: Boolean);
    procedure SetDeviceIndex(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetFrameRate(Value: Integer);
    procedure SetImageFormat(Value: TWebcamImageFormat);
    procedure SetJPEGQuality(Value: Integer);

    { Internal }
    procedure EnumerateDevicesInternal;

  protected
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Device enumeration }
    function GetDevices: TWebcamDeviceArray;
    procedure RefreshDevices;
    function GetDeviceCount: Integer;
    function GetDeviceName(Index: Integer): string;

    { Resolution support }
    function GetSupportedResolutions: TWebcamResolutionArray;

    { Capture control }
    procedure StartCapture;
    procedure StopCapture;

    { Snapshot - thread-safe, returns last captured frame }
    function GetSingleFrame: TBytes;

    { Runtime read-only properties }
    property ActualWidth: Integer read FActualWidth;
    property ActualHeight: Integer read FActualHeight;
    property FrameCount: Int64 read FFrameCount;
    property LastFrameData: TBytes read FLastFrameData;

  published
    { Core properties }
    property Active: Boolean read FActive write SetActive default False;
    property DeviceIndex: Integer read FDeviceIndex write SetDeviceIndex default 0;

    { Capture format }
    property Width: Integer read FWidth write SetWidth default 640;
    property Height: Integer read FHeight write SetHeight default 480;
    property FrameRate: Integer read FFrameRate write SetFrameRate default 30;

    { Output format }
    property ImageFormat: TWebcamImageFormat read FImageFormat
      write SetImageFormat default wifJPEG;
    property JPEGQuality: Integer read FJPEGQuality
      write SetJPEGQuality default 85;

    { Events }
    property OnFrameReceived: TOnFrameReceived read FOnFrameReceived
      write FOnFrameReceived;
    property OnDeviceListChanged: TOnDeviceListChanged read FOnDeviceListChanged
      write FOnDeviceListChanged;
    property OnCaptureError: TOnCaptureError read FOnCaptureError
      write FOnCaptureError;
    property OnCaptureStarted: TNotifyEvent read FOnCaptureStarted
      write FOnCaptureStarted;
    property OnCaptureStopped: TNotifyEvent read FOnCaptureStopped
      write FOnCaptureStopped;
  end;

implementation

uses
  Vcl.Imaging.jpeg;

{ ============================================================================
  TWebcamResolution
  ============================================================================ }

class function TWebcamResolution.Create(AWidth, AHeight: Integer): TWebcamResolution;
begin
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

function TWebcamResolution.ToString: string;
begin
  Result := Format('%dx%d', [Width, Height]);
end;

{ ============================================================================
  TSampleGrabberCB
  Runs on DirectShow's streaming thread. Copies the raw frame buffer
  and signals TCaptureThread to encode it. Does NOT do any heavy work.
  ============================================================================ }

constructor TSampleGrabberCB.Create(AOwner: TCaptureThread);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TSampleGrabberCB.SampleCB(SampleTime: Double; pSample: IUnknown): HRESULT;
begin
  Result := S_OK;
end;

function TSampleGrabberCB.BufferCB(SampleTime: Double; pBuffer: PByte;
  BufferLen: Integer): HRESULT;
begin
  Result := S_OK;
  if (FOwner <> nil) and (pBuffer <> nil) and (BufferLen > 0) then
    FOwner.OnBufferCB(pBuffer, BufferLen);
end;

{ ============================================================================
  TCaptureThread
  Dedicated thread that owns all DirectShow COM objects and does all
  heavy lifting (graph build, frame encoding). Main thread stays smooth.
  ============================================================================ }

constructor TCaptureThread.Create(AOwner: TWebcamCapture);
begin
  inherited Create(True); // Create suspended
  FreeOnTerminate := False;
  FOwner := AOwner;

  // Copy settings from owner at creation time
  FDeviceIndex := AOwner.FDeviceIndex;
  FWidth := AOwner.FWidth;
  FHeight := AOwner.FHeight;
  FFrameRate := AOwner.FFrameRate;
  FImageFormat := AOwner.FImageFormat;
  FJPEGQuality := AOwner.FJPEGQuality;
  FActualWidth := 0;
  FActualHeight := 0;

  FRawBufferLock := TCriticalSection.Create;
  FRawBuffer := nil;
  FRawBufferLen := 0;
  FFrameReadyEvent := TEvent.Create(nil, False, False, '');
  FStopEvent := TEvent.Create(nil, True, False, '');
  FStartCompletedEvent := TEvent.Create(nil, True, False, '');
  FStartSucceeded := False;
end;

destructor TCaptureThread.Destroy;
begin
  if FRawBuffer <> nil then
    FreeMem(FRawBuffer);
  FStartCompletedEvent.Free;
  FFrameReadyEvent.Free;
  FStopEvent.Free;
  FRawBufferLock.Free;
  inherited Destroy;
end;

procedure TCaptureThread.Execute;
var
  Handles: array[0..1] of THandle;
  WaitResult: DWORD;
  StartReported: Boolean;
begin
  StartReported := False;

  // Initialize COM on THIS thread
  CoInitialize(nil);
  try
    try
      if not BuildCaptureGraph then
      begin
        FStartSucceeded := False;
        FStartCompletedEvent.SetEvent;
        StartReported := True;
        TeardownGraph;
        FireError('Failed to build capture graph');
        Exit;
      end;

      FStartSucceeded := True;
      FStartCompletedEvent.SetEvent;
      StartReported := True;

      // Notify main thread that capture has started!
      if Assigned(FOwner.FOnCaptureStarted) then
      begin
        var LocalOwner := FOwner;
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(LocalOwner.FOnCaptureStarted) then
              LocalOwner.FOnCaptureStarted(LocalOwner);
          end);
      end;

      // Main capture loop: wait for frame data or stop signal!
      Handles[0] := FStopEvent.Handle;
      Handles[1] := FFrameReadyEvent.Handle;

      while not Terminated do
      begin
        WaitResult := WaitForMultipleObjects(2, @Handles[0], False, 100);
        case WaitResult of
          WAIT_OBJECT_0:     // Stop event signaled
            Break;
          WAIT_OBJECT_0 + 1: // Frame ready
            ProcessFrame;
          WAIT_TIMEOUT:      // Timeout, check Terminated
            Continue;
        else
          Break; // Error
        end;
      end;

      // Clean shutdown: stop graph on this thread (where COM objects live)
      if FMediaControl <> nil then
        FMediaControl.Stop;
      if FSampleGrabber <> nil then
        FSampleGrabber.SetCallback(nil, 1);
      TeardownGraph;
    except
      on E: Exception do
      begin
        if not StartReported then
        begin
          FStartSucceeded := False;
          FStartCompletedEvent.SetEvent;
          StartReported := True;
        end;
        TeardownGraph;
        FireError('Capture thread error: ' + E.Message);
      end;
    end;

  finally
    if not StartReported then
    begin
      FStartSucceeded := False;
      FStartCompletedEvent.SetEvent;
    end;
    CoUninitialize;
  end;
end;

procedure TCaptureThread.OnBufferCB(pBuffer: PByte; BufferLen: Integer);
begin
  // Called from DirectShow streaming thread
  // Quick copy of raw data, then signal our thread to encode
  FRawBufferLock.Enter;
  try
    if (FRawBuffer = nil) or (FRawBufferLen < BufferLen) then
    begin
      if FRawBuffer <> nil then
        FreeMem(FRawBuffer);
      GetMem(FRawBuffer, BufferLen);
    end;
    Move(pBuffer^, FRawBuffer^, BufferLen);
    FRawBufferLen := BufferLen;
  finally
    FRawBufferLock.Leave;
  end;
  FFrameReadyEvent.SetEvent;
end;

procedure TCaptureThread.ProcessFrame;
var
  LocalBuf: PByte;
  LocalLen: Integer;
  W, H: Integer;
  FrameBytes: TBytes;
  LocalOwner: TWebcamCapture;
  LocalOnFrame: TOnFrameReceived;
begin
  W := FActualWidth;
  H := FActualHeight;
  if (W <= 0) or (H <= 0) then
    Exit;

  // Grab a copy of the raw buffer
  FRawBufferLock.Enter;
  try
    if (FRawBuffer = nil) or (FRawBufferLen = 0) then
      Exit;
    LocalLen := FRawBufferLen;
    GetMem(LocalBuf, LocalLen);
    Move(FRawBuffer^, LocalBuf^, LocalLen);
  finally
    FRawBufferLock.Leave;
  end;

  try
    // Heavy encoding (JPEG/BMP) happens HERE on the capture thread
    FrameBytes := ConvertFrameToBytes(LocalBuf, LocalLen, W, H);
  finally
    FreeMem(LocalBuf);
  end;

  if Length(FrameBytes) = 0 then
    Exit;

  // Update shared state
  FOwner.FFrameLock.Enter;
  try
    FOwner.FLastFrameData := FrameBytes;
    FOwner.FActualWidth := W;
    FOwner.FActualHeight := H;
    Inc(FOwner.FFrameCount);
  finally
    FOwner.FFrameLock.Leave;
  end;

  // Deliver to main thread (non-blocking Queue)
  LocalOwner := FOwner;
  LocalOnFrame := FOwner.FOnFrameReceived;
  if Assigned(LocalOnFrame) then
  begin
    // Keep only one queued UI callback pending to avoid unbounded backlog.
    if InterlockedCompareExchange(LocalOwner.FFrameDispatchPending, 1, 0) = 0 then
    begin
      TThread.Queue(nil,
        procedure
        begin
          try
            if Assigned(LocalOnFrame) then
              LocalOnFrame(LocalOwner, FrameBytes, W, H);
          finally
            InterlockedExchange(LocalOwner.FFrameDispatchPending, 0);
          end;
        end);
    end;
  end;
end;

function TCaptureThread.BuildCaptureGraph: Boolean;
var
  DevEnum: ICreateDevEnum;
  EnumMoniker: IEnumMoniker;
  Moniker: IMoniker;
  Fetched: ULONG;
  DevIdx: Integer;
  MT: TAM_MEDIA_TYPE;
  HR: HRESULT;
  PinCat: TGUID;
  MediaTypeGUID: TGUID;
begin
  Result := False;

  HR := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,
    IGraphBuilder, FGraphBuilder);
  if Failed(HR) then Exit;

  HR := CoCreateInstance(CLSID_CaptureGraphBuilder2, nil, CLSCTX_INPROC_SERVER,
    ICaptureGraphBuilder2, FCaptureGraphBuilder);
  if Failed(HR) then Exit;

  HR := FCaptureGraphBuilder.SetFiltergraph(FGraphBuilder);
  if Failed(HR) then Exit;

  HR := CoCreateInstance(CLSID_SystemDeviceEnum, nil, CLSCTX_INPROC_SERVER,
    IID_ICreateDevEnum, DevEnum);
  if Failed(HR) then Exit;

  HR := DevEnum.CreateClassEnumerator(CLSID_VideoInputDeviceCategory, EnumMoniker, 0);
  if Failed(HR) or (EnumMoniker = nil) then Exit;

  DevIdx := 0;
  FCaptureFilter := nil;
  while EnumMoniker.Next(1, Moniker, @Fetched) = S_OK do
  begin
    if DevIdx = FDeviceIndex then
    begin
      HR := Moniker.BindToObject(nil, nil, IID_IBaseFilter, FCaptureFilter);
      if Failed(HR) then Exit;
      Break;
    end;
    Inc(DevIdx);
    Moniker := nil;
  end;

  if FCaptureFilter = nil then Exit;

  HR := FGraphBuilder.AddFilter(FCaptureFilter, 'Video Capture');
  if Failed(HR) then Exit;

  ConfigureStreamFormat;

  HR := CoCreateInstance(CLSID_SampleGrabber, nil, CLSCTX_INPROC_SERVER,
    IID_IBaseFilter, FSampleGrabberFilter);
  if Failed(HR) then Exit;

  HR := FSampleGrabberFilter.QueryInterface(IID_ISampleGrabber, FSampleGrabber);
  if Failed(HR) then Exit;

  FillChar(MT, SizeOf(MT), 0);
  MT.majortype := MEDIATYPE_Video;
  MT.subtype := MEDIASUBTYPE_RGB24;
  MT.formattype := FORMAT_VideoInfo;
  HR := FSampleGrabber.SetMediaType(MT);
  if Failed(HR) then Exit;

  HR := FGraphBuilder.AddFilter(FSampleGrabberFilter, 'Sample Grabber');
  if Failed(HR) then Exit;

  HR := CoCreateInstance(CLSID_NullRenderer, nil, CLSCTX_INPROC_SERVER,
    IID_IBaseFilter, FNullRenderer);
  if Failed(HR) then Exit;

  HR := FGraphBuilder.AddFilter(FNullRenderer, 'Null Renderer');
  if Failed(HR) then Exit;

  PinCat := PIN_CATEGORY_CAPTURE;
  MediaTypeGUID := MEDIATYPE_Video;
  HR := FCaptureGraphBuilder.RenderStream(@PinCat, @MediaTypeGUID,
    FCaptureFilter, FSampleGrabberFilter, FNullRenderer);
  if Failed(HR) then Exit;

  FSampleGrabber.SetBufferSamples(False);
  FSampleGrabber.SetOneShot(False);

  FGrabberCB := TSampleGrabberCB.Create(Self);
  HR := FSampleGrabber.SetCallback(FGrabberCB, 1);
  if Failed(HR) then Exit;

  // Read actual dimensions
  FillChar(MT, SizeOf(MT), 0);
  if Succeeded(FSampleGrabber.GetConnectedMediaType(MT)) then
  begin
    if (MT.cbFormat >= SizeOf(TVideoInfoHeader)) and (MT.pbFormat <> nil) then
    begin
      FActualWidth := PVideoInfoHeader(MT.pbFormat).bmiHeader.biWidth;
      FActualHeight := Abs(PVideoInfoHeader(MT.pbFormat).bmiHeader.biHeight);
    end;
    if MT.pbFormat <> nil then
      CoTaskMemFree(MT.pbFormat);
  end;

  HR := FGraphBuilder.QueryInterface(IMediaControl, FMediaControl);
  if Failed(HR) then Exit;

  HR := FMediaControl.Run;
  if Failed(HR) then Exit;

  Result := True;
end;

function TCaptureThread.ConfigureStreamFormat: Boolean;
var
  StreamConfig: IAMStreamConfig;
  MediaType: PAM_MEDIA_TYPE;
  VideoInfo: PVideoInfoHeader;
  HR: HRESULT;
  PinCat: TGUID;
  MediaTypeGUID: TGUID;
begin
  Result := False;
  PinCat := PIN_CATEGORY_CAPTURE;
  MediaTypeGUID := MEDIATYPE_Video;
  HR := FCaptureGraphBuilder.FindInterface(@PinCat, @MediaTypeGUID,
    FCaptureFilter, IAMStreamConfig, Pointer(StreamConfig));
  if Failed(HR) or (StreamConfig = nil) then Exit;

  HR := StreamConfig.GetFormat(MediaType);
  if Failed(HR) or (MediaType = nil) then Exit;

  try
    if (MediaType.cbFormat >= SizeOf(TVideoInfoHeader)) and
       (MediaType.pbFormat <> nil) then
    begin
      VideoInfo := PVideoInfoHeader(MediaType.pbFormat);
      VideoInfo.bmiHeader.biWidth := FWidth;
      VideoInfo.bmiHeader.biHeight := FHeight;
      if FFrameRate > 0 then
        VideoInfo.AvgTimePerFrame := 10000000 div FFrameRate;
      HR := StreamConfig.SetFormat(MediaType^);
      Result := Succeeded(HR);
    end;
  finally
    if MediaType.pbFormat <> nil then
      CoTaskMemFree(MediaType.pbFormat);
    CoTaskMemFree(MediaType);
  end;
end;

procedure TCaptureThread.TeardownGraph;
begin
  FGrabberCB := nil;
  FMediaControl := nil;
  FSampleGrabber := nil;
  FSampleGrabberFilter := nil;
  FNullRenderer := nil;
  FCaptureFilter := nil;
  FCaptureGraphBuilder := nil;
  FGraphBuilder := nil;
end;

procedure TCaptureThread.FireError(const Msg: string);
var
  LocalOwner: TWebcamCapture;
  LocalOnError: TOnCaptureError;
begin
  LocalOwner := FOwner;
  LocalOnError := FOwner.FOnCaptureError;
  if Assigned(LocalOnError) then
  begin
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(LocalOnError) then
          LocalOnError(LocalOwner, Msg);
      end);
  end;
end;

{ --- Frame encoding (runs on capture thread, never main VCL thread) --- }

function TCaptureThread.ConvertFrameToBytes(pBuffer: PByte; BufferLen: Integer;
  AWidth, AHeight: Integer): TBytes;
begin
  case FImageFormat of
    wifBMP:  Result := CreateBMPBytes(pBuffer, BufferLen, AWidth, AHeight);
    wifJPEG: Result := CreateJPEGBytes(pBuffer, BufferLen, AWidth, AHeight);
  else
    Result := CreateJPEGBytes(pBuffer, BufferLen, AWidth, AHeight);
  end;
end;

function TCaptureThread.CreateBMPBytes(pBuffer: PByte; BufferLen: Integer;
  AWidth, AHeight: Integer): TBytes;
var
  FileHeader: TBitmapFileHeader;
  InfoHeader: TBitmapInfoHeader;
  RowSize, ImageSize: Integer;
  Stream: TBytesStream;
  SrcRow: PByte;
  Padding: array[0..3] of Byte;
  PadBytes, Row: Integer;
begin
  SetLength(Result, 0);
  RowSize := ((AWidth * 3 + 3) div 4) * 4;
  ImageSize := RowSize * AHeight;
  if BufferLen < AWidth * AHeight * 3 then Exit;

  Stream := TBytesStream.Create;
  try
    FillChar(FileHeader, SizeOf(FileHeader), 0);
    FileHeader.bfType := $4D42;
    FileHeader.bfSize := SizeOf(FileHeader) + SizeOf(InfoHeader) + ImageSize;
    FileHeader.bfOffBits := SizeOf(FileHeader) + SizeOf(InfoHeader);

    FillChar(InfoHeader, SizeOf(InfoHeader), 0);
    InfoHeader.biSize := SizeOf(InfoHeader);
    InfoHeader.biWidth := AWidth;
    InfoHeader.biHeight := AHeight;
    InfoHeader.biPlanes := 1;
    InfoHeader.biBitCount := 24;
    InfoHeader.biCompression := 0;
    InfoHeader.biSizeImage := ImageSize;

    Stream.WriteBuffer(FileHeader, SizeOf(FileHeader));
    Stream.WriteBuffer(InfoHeader, SizeOf(InfoHeader));

    if RowSize = AWidth * 3 then
      Stream.WriteBuffer(pBuffer^, ImageSize)
    else
    begin
      SrcRow := pBuffer;
      FillChar(Padding, SizeOf(Padding), 0);
      PadBytes := RowSize - (AWidth * 3);
      for Row := 0 to AHeight - 1 do
      begin
        Stream.WriteBuffer(SrcRow^, AWidth * 3);
        if PadBytes > 0 then
          Stream.WriteBuffer(Padding, PadBytes);
        Inc(SrcRow, AWidth * 3);
      end;
    end;

    Result := Copy(Stream.Bytes, 0, Stream.Size);
  finally
    Stream.Free;
  end;
end;

function TCaptureThread.CreateJPEGBytes(pBuffer: PByte; BufferLen: Integer;
  AWidth, AHeight: Integer): TBytes;
var
  Bitmap: TBitmap;
  JPEG: TJPEGImage;
  Stream: TMemoryStream;
  Y, RowSize: Integer;
  SrcRow, DstRow: PByte;
begin
  SetLength(Result, 0);
  if BufferLen < AWidth * AHeight * 3 then Exit;

  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf24bit;
    Bitmap.SetSize(AWidth, AHeight);

    RowSize := AWidth * 3;
    for Y := 0 to AHeight - 1 do
    begin
      SrcRow := pBuffer;
      Inc(SrcRow, Y * RowSize);
      DstRow := Bitmap.ScanLine[AHeight - 1 - Y];
      Move(SrcRow^, DstRow^, RowSize);
    end;

    JPEG := TJPEGImage.Create;
    try
      JPEG.CompressionQuality := FJPEGQuality;
      JPEG.Assign(Bitmap);
      Stream := TMemoryStream.Create;
      try
        JPEG.SaveToStream(Stream);
        SetLength(Result, Stream.Size);
        Stream.Position := 0;
        Stream.ReadBuffer(Result[0], Stream.Size);
      finally
        Stream.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;

{ ============================================================================
  TWebcamCapture - Component Implementation
  ============================================================================ }

constructor TWebcamCapture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrameLock := TCriticalSection.Create;
  FActive := False;
  FDeviceIndex := 0;
  FWidth := 640;
  FHeight := 480;
  FFrameRate := 30;
  FImageFormat := wifJPEG;
  FJPEGQuality := 85;
  FFrameCount := 0;
  FFrameDispatchPending := 0;
  FDevicesEnumerated := False;
  FActualWidth := 0;
  FActualHeight := 0;
  FCaptureThread := nil;
end;

destructor TWebcamCapture.Destroy;
begin
  if FActive then
    StopCapture;
  FFrameLock.Free;
  inherited Destroy;
end;

procedure TWebcamCapture.Loaded;
begin
  inherited Loaded;
  if FActive and not (csDesigning in ComponentState) then
  begin
    FActive := False;
    SetActive(True);
  end;
end;

{ --- Property Setters --- }

procedure TWebcamCapture.SetActive(Value: Boolean);
begin
  if FActive = Value then Exit;
  if csDesigning in ComponentState then
  begin
    FActive := Value;
    Exit;
  end;
  if Value then StartCapture else StopCapture;
end;

procedure TWebcamCapture.SetDeviceIndex(Value: Integer);
var
  WasActive: Boolean;
begin
  if FDeviceIndex = Value then Exit;
  WasActive := FActive;
  if WasActive then StopCapture;
  FDeviceIndex := Value;
  if WasActive and not (csDesigning in ComponentState) then StartCapture;
end;

procedure TWebcamCapture.SetWidth(Value: Integer);
var
  WasActive: Boolean;
begin
  if Value < 16 then Value := 16;
  if Value > 7680 then Value := 7680;
  if FWidth = Value then Exit;
  WasActive := FActive;
  if WasActive then StopCapture;
  FWidth := Value;
  if WasActive and not (csDesigning in ComponentState) then StartCapture;
end;

procedure TWebcamCapture.SetHeight(Value: Integer);
var
  WasActive: Boolean;
begin
  if Value < 16 then Value := 16;
  if Value > 4320 then Value := 4320;
  if FHeight = Value then Exit;
  WasActive := FActive;
  if WasActive then StopCapture;
  FHeight := Value;
  if WasActive and not (csDesigning in ComponentState) then StartCapture;
end;

procedure TWebcamCapture.SetFrameRate(Value: Integer);
var
  WasActive: Boolean;
begin
  if Value < 1 then Value := 1;
  if Value > 120 then Value := 120;
  if FFrameRate = Value then Exit;
  WasActive := FActive;
  if WasActive then StopCapture;
  FFrameRate := Value;
  if WasActive and not (csDesigning in ComponentState) then StartCapture;
end;

procedure TWebcamCapture.SetImageFormat(Value: TWebcamImageFormat);
var
  WasActive: Boolean;
begin
  if FImageFormat = Value then Exit;
  WasActive := FActive;
  if WasActive then StopCapture;
  FImageFormat := Value;
  if WasActive and not (csDesigning in ComponentState) then StartCapture;
end;

procedure TWebcamCapture.SetJPEGQuality(Value: Integer);
var
  WasActive: Boolean;
begin
  if Value < 1 then Value := 1;
  if Value > 100 then Value := 100;
  if FJPEGQuality = Value then Exit;
  WasActive := FActive;
  if WasActive and (FImageFormat = wifJPEG) then StopCapture;
  FJPEGQuality := Value;
  if WasActive and (FImageFormat = wifJPEG) and not (csDesigning in ComponentState) then
    StartCapture;
end;

{ --- Device Enumeration (uses safe STA COM init on main thread) --- }

procedure TWebcamCapture.EnumerateDevicesInternal;
var
  DevEnum: ICreateDevEnum;
  EnumMoniker: IEnumMoniker;
  Moniker: IMoniker;
  Fetched: ULONG;
  PropBag: IPropertyBag;
  VarName, VarPath: OleVariant;
  DevIndex: Integer;
  HR: HRESULT;
begin
  SetLength(FDevices, 0);
  DevIndex := 0;

  HR := CoCreateInstance(CLSID_SystemDeviceEnum, nil, CLSCTX_INPROC_SERVER,
    IID_ICreateDevEnum, DevEnum);
  if Failed(HR) then Exit;

  HR := DevEnum.CreateClassEnumerator(CLSID_VideoInputDeviceCategory, EnumMoniker, 0);
  if Failed(HR) or (EnumMoniker = nil) then Exit;

  while EnumMoniker.Next(1, Moniker, @Fetched) = S_OK do
  begin
    if Succeeded(Moniker.BindToStorage(nil, nil, IID_IPropertyBag, PropBag)) then
    begin
      SetLength(FDevices, DevIndex + 1);
      FDevices[DevIndex].Index := DevIndex;

      VariantInit(VarName);
      if Succeeded(PropBag.Read('FriendlyName', VarName, nil)) then
        FDevices[DevIndex].Name := string(VarName)
      else
        FDevices[DevIndex].Name := Format('Camera %d', [DevIndex]);

      VariantInit(VarPath);
      if Succeeded(PropBag.Read('DevicePath', VarPath, nil)) then
        FDevices[DevIndex].DevicePath := string(VarPath)
      else
        FDevices[DevIndex].DevicePath := '';

      PropBag := nil;
      Inc(DevIndex);
    end;
    Moniker := nil;
  end;

  FDevicesEnumerated := True;
end;

function TWebcamCapture.GetDevices: TWebcamDeviceArray;
var
  HR: HRESULT;
  NeedUninit: Boolean;
begin
  if not FDevicesEnumerated then
  begin
    NeedUninit := False;
    if not (csDesigning in ComponentState) then
    begin
      HR := CoInitialize(nil);
      NeedUninit := (HR = S_OK) or (HR = S_FALSE);
    end;
    try
      EnumerateDevicesInternal;
    finally
      if NeedUninit then
        CoUninitialize;
    end;
  end;
  Result := Copy(FDevices);
end;

procedure TWebcamCapture.RefreshDevices;
var
  OldCount: Integer;
  HR: HRESULT;
  NeedUninit: Boolean;
begin
  OldCount := Length(FDevices);
  FDevicesEnumerated := False;

  NeedUninit := False;
  if not (csDesigning in ComponentState) then
  begin
    HR := CoInitialize(nil);
    NeedUninit := (HR = S_OK) or (HR = S_FALSE);
  end;
  try
    EnumerateDevicesInternal;
  finally
    if NeedUninit then
      CoUninitialize;
  end;

  if (Length(FDevices) <> OldCount) and Assigned(FOnDeviceListChanged) then
    FOnDeviceListChanged(Self, Length(FDevices));
end;

function TWebcamCapture.GetDeviceCount: Integer;
begin
  if not FDevicesEnumerated then
    GetDevices;
  Result := Length(FDevices);
end;

function TWebcamCapture.GetDeviceName(Index: Integer): string;
begin
  if not FDevicesEnumerated then
    GetDevices;
  if (Index >= 0) and (Index < Length(FDevices)) then
    Result := FDevices[Index].Name
  else
    Result := '';
end;

{ --- Resolution Support --- }

function TWebcamCapture.GetSupportedResolutions: TWebcamResolutionArray;
var
  DevEnum: ICreateDevEnum;
  EnumMoniker: IEnumMoniker;
  Moniker: IMoniker;
  Fetched: ULONG;
  BaseFilter: IBaseFilter;
  StreamConfig: IAMStreamConfig;
  Count, Size, I, DevIdx: Integer;
  MediaType: PAM_MEDIA_TYPE;
  VideoInfo: PVideoInfoHeader;
  SCC: TVIDEO_STREAM_CONFIG_CAPS;
  ResList: TWebcamResolutionArray;
  ResCount: Integer;
  W, H, J: Integer;
  Duplicate: Boolean;
  HR: HRESULT;
  CaptureGB: ICaptureGraphBuilder2;
  PinCategory, MediaTypeGUID: TGUID;
  NeedUninit: Boolean;
begin
  SetLength(Result, 0);
  SetLength(ResList, 0);
  ResCount := 0;

  HR := CoInitialize(nil);
  NeedUninit := (HR = S_OK) or (HR = S_FALSE);
  try
    HR := CoCreateInstance(CLSID_SystemDeviceEnum, nil, CLSCTX_INPROC_SERVER,
      IID_ICreateDevEnum, DevEnum);
    if Failed(HR) then Exit;

    HR := DevEnum.CreateClassEnumerator(CLSID_VideoInputDeviceCategory, EnumMoniker, 0);
    if Failed(HR) or (EnumMoniker = nil) then Exit;

    DevIdx := 0;
    while EnumMoniker.Next(1, Moniker, @Fetched) = S_OK do
    begin
      if DevIdx = FDeviceIndex then
      begin
        HR := Moniker.BindToObject(nil, nil, IID_IBaseFilter, BaseFilter);
        if Succeeded(HR) then
        begin
          HR := CoCreateInstance(CLSID_CaptureGraphBuilder2, nil, CLSCTX_INPROC_SERVER,
            ICaptureGraphBuilder2, CaptureGB);
          if Succeeded(HR) then
          begin
            PinCategory := PIN_CATEGORY_CAPTURE;
            MediaTypeGUID := MEDIATYPE_Video;
            HR := CaptureGB.FindInterface(@PinCategory, @MediaTypeGUID,
              BaseFilter, IAMStreamConfig, Pointer(StreamConfig));
            if Failed(HR) then
              StreamConfig := nil;
          end;

          if StreamConfig <> nil then
          begin
            if Succeeded(StreamConfig.GetNumberOfCapabilities(Count, Size)) then
            begin
              for I := 0 to Count - 1 do
              begin
                if Succeeded(StreamConfig.GetStreamCaps(I, MediaType, SCC)) then
                begin
                  if (MediaType <> nil) and (MediaType.cbFormat >= SizeOf(TVideoInfoHeader)) and
                     (MediaType.pbFormat <> nil) then
                  begin
                    VideoInfo := PVideoInfoHeader(MediaType.pbFormat);
                    W := VideoInfo.bmiHeader.biWidth;
                    H := Abs(VideoInfo.bmiHeader.biHeight);
                    if (W > 0) and (H > 0) then
                    begin
                      Duplicate := False;
                      for J := 0 to ResCount - 1 do
                        if (ResList[J].Width = W) and (ResList[J].Height = H) then
                        begin
                          Duplicate := True;
                          Break;
                        end;
                      if not Duplicate then
                      begin
                        SetLength(ResList, ResCount + 1);
                        ResList[ResCount] := TWebcamResolution.Create(W, H);
                        Inc(ResCount);
                      end;
                    end;
                  end;
                  if (MediaType <> nil) and (MediaType.pbFormat <> nil) then
                    CoTaskMemFree(MediaType.pbFormat);
                  if MediaType <> nil then
                    CoTaskMemFree(MediaType);
                end;
              end;
            end;
          end;
          CaptureGB := nil;
          StreamConfig := nil;
          BaseFilter := nil;
        end;
        Break;
      end;
      Inc(DevIdx);
      Moniker := nil;
    end;
  finally
    if NeedUninit then
      CoUninitialize;
  end;
  Result := ResList;
end;

{ --- Capture Control --- }

procedure TWebcamCapture.StartCapture;
const
  StartupTimeoutMs = 5000;
var
  WaitRes: TWaitResult;
begin
  if FActive then Exit;
  if csDesigning in ComponentState then
  begin
    FActive := True;
    Exit;
  end;

  if not FDevicesEnumerated then
    GetDevices;

  if Length(FDevices) = 0 then
  begin
    if Assigned(FOnCaptureError) then
      FOnCaptureError(Self, 'No webcam devices detected');
    Exit;
  end;

  if (FDeviceIndex < 0) or (FDeviceIndex >= Length(FDevices)) then
  begin
    if Assigned(FOnCaptureError) then
      FOnCaptureError(Self, Format('Invalid device index %d (available: 0..%d)',
        [FDeviceIndex, Length(FDevices) - 1]));
    Exit;
  end;

  FFrameCount := 0;
  InterlockedExchange(FFrameDispatchPending, 0);

  // Create and start capture thread, then wait for startup result.
  FCaptureThread := TCaptureThread.Create(Self);
  FCaptureThread.Start;

  WaitRes := FCaptureThread.FStartCompletedEvent.WaitFor(StartupTimeoutMs);
  if (WaitRes = wrSignaled) and FCaptureThread.FStartSucceeded then
  begin
    FActive := True;
    Exit;
  end;

  // Startup failed or timed out: cleanly tear down thread and keep state inactive.
  FCaptureThread.FStopEvent.SetEvent;
  FCaptureThread.Terminate;
  FCaptureThread.WaitFor;
  FreeAndNil(FCaptureThread);

  if (WaitRes <> wrSignaled) and Assigned(FOnCaptureError) then
    FOnCaptureError(Self, 'Capture startup timed out');
end;

procedure TWebcamCapture.StopCapture;
begin
  if not FActive then Exit;
  if csDesigning in ComponentState then
  begin
    FActive := False;
    Exit;
  end;

  if FCaptureThread <> nil then
  begin
    // Signal the thread to stop
    FCaptureThread.FStopEvent.SetEvent;
    FCaptureThread.Terminate;
    // Wait for clean shutdown (graph teardown happens on capture thread)
    FCaptureThread.WaitFor;
    FreeAndNil(FCaptureThread);
  end;

  FActive := False;
  InterlockedExchange(FFrameDispatchPending, 0);

  if Assigned(FOnCaptureStopped) then
    FOnCaptureStopped(Self);
end;

function TWebcamCapture.GetSingleFrame: TBytes;
begin
  FFrameLock.Enter;
  try
    Result := Copy(FLastFrameData);
  finally
    FFrameLock.Leave;
  end;
end;

end.
