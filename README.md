# 🎥 Delphi Webcam Capture Component
Professional VCL webcam capture component for Delphi (DirectShow-based, asynchronous, and easy to drop into existing projects).

![Delphi](https://img.shields.io/badge/Delphi-12.2%2B-red)
![Platform](https://img.shields.io/badge/Platform-Win32%20%7C%20Win64-blue)
![Framework](https://img.shields.io/badge/Framework-VCL-8A2BE2)
![Output](https://img.shields.io/badge/Output-JPEG%20%7C%20BMP-green)

A lightweight `TWebcamCapture` component that captures webcam frames as `TBytes` and delivers them on the main thread through events.  
It is designed for smooth UI behavior and straightforward integration in Delphi VCL apps.

---

## 🚀 Overview
This component provides real-time webcam capture with:

- Device enumeration
- Camera selection by index
- Configurable resolution and frame rate
- JPEG or BMP frame output
- Threaded capture pipeline (UI stays responsive)
- Main-thread frame delivery via event callbacks
- Snapshot access via `GetSingleFrame`

---

## ⭐ Key Features

### 🎥 Capture
- Capture webcam frames continuously
- Output frames as raw `TBytes` (JPEG or BMP)
- Track actual negotiated resolution and frame count

### ⚙️ Configuration
- `DeviceIndex`, `Width`, `Height`, `FrameRate`
- `ImageFormat` (`wifJPEG` or `wifBMP`)
- `JPEGQuality` (1..100)

### 🧵 Threading & Stability
- Dedicated capture thread for DirectShow/COM work
- UI callback delivery through queued main-thread events
- Startup handshake to avoid false “active” state
- Frame dispatch backpressure (prevents runaway UI queue)

### 🧩 Delphi Integration
- Registered as `TWebcamCapture` on the **Webcam** palette
- Standard VCL component usage pattern
- Works cleanly with dropped VCL controls (`TImage`, `TTimer`, etc.)

---

## 📦 Repository Contents
- `WebcamCapture.pas`  
  Core component implementation.
- `WebcamCaptureReg.pas`  
  IDE component registration.
- `WebcamComponents.dpk` / `.dproj`  
  Package project.
- `Demos/Basic Demo`  
  Local preview, device selection, snapshot save.
- `Demos/Network Webcam Streaming Demo`  
  Webcam streaming over network (uses NetCom7 sources).

---

## ✅ Requirements

### Core Component
- Delphi VCL (tested with Delphi 12.2 Athens toolchain)
- Windows desktop target
- DirectShow-capable system with camera drivers

### Optional (Network Demo Only)
- NetCom7 source path configured for demo build

---

## 🔧 Installation

1. Open `WebcamComponents.dproj` in Delphi.
2. Build the package for your target (`Win32` and/or `Win64`).
3. Install package in IDE.
4. Drop `TWebcamCapture` from the **Webcam** component palette.

---

## ⚡ Quick Start (Basic Usage)

### 1) Drop components on a VCL form
- `TWebcamCapture` (`WebcamCapture1`)
- `TImage` (`Image1`)

### 2) Configure and start
```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  WebcamCapture1.DeviceIndex := 0;
  WebcamCapture1.Width := 640;
  WebcamCapture1.Height := 480;
  WebcamCapture1.FrameRate := 30;
  WebcamCapture1.ImageFormat := wifJPEG;
  WebcamCapture1.JPEGQuality := 85;
  WebcamCapture1.Active := True;
end;
procedure TForm1.WebcamCapture1FrameReceived(Sender: TObject;
  const FrameData: TBytes; Width, Height: Integer);
var
  S: TMemoryStream;
begin
  if Length(FrameData) = 0 then Exit;

  S := TMemoryStream.Create;
  try
    S.WriteBuffer(FrameData[0], Length(FrameData));
    S.Position := 0;
    Image1.Picture.LoadFromStream(S);
  finally
    S.Free;
  end;
end;
📚 API Reference
Properties
Property	Type	Description
Active	Boolean	Start/stop capture
DeviceIndex	Integer	Selected camera index
Width / Height	Integer	Requested capture size
FrameRate	Integer	Requested FPS
ImageFormat	TWebcamImageFormat	wifJPEG or wifBMP
JPEGQuality	Integer	JPEG quality (1..100)
ActualWidth / ActualHeight	Integer	Negotiated capture size
FrameCount	Int64	Number of captured frames
LastFrameData	TBytes	Last captured encoded frame
Methods
Method	Description
GetDevices	Returns available camera list
RefreshDevices	Re-enumerates cameras
GetSupportedResolutions	Queries camera-supported resolutions
StartCapture / StopCapture	Manual control
GetSingleFrame	Returns last captured frame bytes
Events
Event	Description
OnFrameReceived	New frame (TBytes, width, height)
OnCaptureStarted	Capture session started
OnCaptureStopped	Capture session stopped
OnCaptureError	Capture error text
OnDeviceListChanged	Device count changed
🧪 Demos
Basic Demo
Device enumeration and selection
Live preview
JPEG/BMP switch
Snapshot saving
FPS display
Network Webcam Streaming Demo
Uses NetCom7 TncServerSource / TncClientSource
Broadcasts captured frames to remote clients
Shows practical frame transport over TCP
🧱 Dependencies
Core Component
No third-party runtime DLL required by this repo itself.
Uses Windows COM/DirectShow APIs and system codecs.
Important Note
DirectShow availability and camera behavior can vary by machine, driver, and Windows media feature setup.
⚠️ Compatibility Notes
Windows desktop only (VCL + WinAPI/COM)
Win32 and Win64 build targets supported
Not intended for FMX/mobile platforms
Camera privacy settings can block access if not allowed
🛠️ Troubleshooting
“No webcam devices detected”
Verify camera is connected and not disabled
Check Device Manager
Check Windows camera permissions
“Failed to build capture graph” or startup timeout
Another app may already own the camera
Driver may not expose expected DirectShow path
Try different DeviceIndex or lower requested resolution/FPS
Frame display issues
Ensure OnFrameReceived handler is correctly assigned
Use Image1.Picture.LoadFromStream(...) for TBytes payloads

👨‍💻 Author
BitmasterXor

If this component helps your Delphi workflow, consider starring the repository.
