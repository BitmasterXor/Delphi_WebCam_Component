<!-- ========================================================= -->
<!-- README.md (GitHub Markdown + HTML)                         -->
<!-- Copy/paste this entire file into README.md                 -->
<!-- ========================================================= -->

<h1 align="center">🎥 Delphi Webcam Capture Component</h1>

<p align="center">
  <b>Professional DirectShow Webcam Capture Component for Delphi VCL</b><br>
  High-performance • Asynchronous • Thread-safe • Easy drop-in integration
</p>

<p align="center">
  <img src="https://img.shields.io/badge/Delphi-12.2%2B-red?style=for-the-badge" alt="Delphi">
  <img src="https://img.shields.io/badge/Platform-Win32%20%7C%20Win64-blue?style=for-the-badge" alt="Platform">
  <img src="https://img.shields.io/badge/Framework-VCL-8A2BE2?style=for-the-badge" alt="Framework">
  <img src="https://img.shields.io/badge/Output-JPEG%20%7C%20BMP-green?style=for-the-badge" alt="Output">
  <img src="https://img.shields.io/badge/Threading-Asynchronous-orange?style=for-the-badge" alt="Threading">
</p>

<p align="center">
  A lightweight <code>TWebcamCapture</code> component that captures webcam frames as <code>TBytes</code> and delivers them safely on the main thread via events.
  Designed for smooth UI behavior and straightforward integration in Delphi VCL apps.
</p>

<hr>

<h2>🚀 Overview</h2>

<ul>
  <li>Device enumeration</li>
  <li>Camera selection by index</li>
  <li>Configurable resolution and frame rate</li>
  <li>JPEG or BMP frame output</li>
  <li>Threaded capture pipeline (UI stays responsive)</li>
  <li>Main-thread frame delivery via event callbacks</li>
  <li>Snapshot access via <code>GetSingleFrame</code></li>
</ul>

<hr>

<h2>⭐ Key Features</h2>

<h3>🎥 Capture</h3>
<ul>
  <li>Capture webcam frames continuously</li>
  <li>Output frames as raw <code>TBytes</code> (JPEG or BMP)</li>
  <li>Track actual negotiated resolution and frame count</li>
</ul>

<h3>⚙️ Configuration</h3>
<ul>
  <li><code>DeviceIndex</code>, <code>Width</code>, <code>Height</code>, <code>FrameRate</code></li>
  <li><code>ImageFormat</code> (<code>wifJPEG</code> or <code>wifBMP</code>)</li>
  <li><code>JPEGQuality</code> (1..100)</li>
</ul>

<h3>🧵 Threading &amp; Stability</h3>
<ul>
  <li>Dedicated capture thread for DirectShow/COM work</li>
  <li>UI callback delivery through queued main-thread events</li>
  <li>Startup handshake to avoid false “active” state</li>
  <li>Frame dispatch backpressure (prevents runaway UI queue)</li>
</ul>

<h3>🧩 Delphi Integration</h3>
<ul>
  <li>Registered as <code>TWebcamCapture</code> on the <b>Webcam</b> palette</li>
  <li>Standard VCL component usage pattern</li>
  <li>Works cleanly with dropped VCL controls (<code>TImage</code>, <code>TTimer</code>, etc.)</li>
</ul>

<hr>

<h2>📦 Repository Contents</h2>

<ul>
  <li><code>WebcamCapture.pas</code> — Core component implementation.</li>
  <li><code>WebcamCaptureReg.pas</code> — IDE component registration.</li>
  <li><code>WebcamComponents.dpk</code> / <code>.dproj</code> — Package project.</li>
  <li><code>Demos/Basic Demo</code> — Local preview, device selection, snapshot save.</li>
  <li><code>Demos/Network Webcam Streaming Demo</code> — Webcam streaming over network (uses NetCom7 sources).</li>
</ul>

<hr>

<h2>✅ Requirements</h2>

<h3>Core Component</h3>
<ul>
  <li>Delphi VCL (tested with Delphi 12.2 Athens toolchain)</li>
  <li>Windows desktop target</li>
  <li>DirectShow-capable system with camera drivers</li>
</ul>

<h3>Optional (Network Demo Only)</h3>
<ul>
  <li>NetCom7 source path configured for demo build</li>
</ul>

<hr>

<h2>🔧 Installation</h2>

<ol>
  <li>Open <code>WebcamComponents.dproj</code> in Delphi.</li>
  <li>Build the package for your target (<code>Win32</code> and/or <code>Win64</code>).</li>
  <li>Install package in IDE.</li>
  <li>Drop <code>TWebcamCapture</code> from the <b>Webcam</b> component palette.</li>
</ol>

<hr>

<h2>⚡ Quick Start (Basic Usage)</h2>

<h3>1) Drop components on a VCL form</h3>
<ul>
  <li><code>TWebcamCapture</code> (<code>WebcamCapture1</code>)</li>
  <li><code>TImage</code> (<code>Image1</code>)</li>
</ul>

<h3>2) Configure and start</h3>

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
```
<hr> <h2>📚 API Reference</h2> <h3>Properties</h3> <table> <tr><th>Property</th><th>Type</th><th>Description</th></tr> <tr><td><code>Active</code></td><td>Boolean</td><td>Start/stop capture</td></tr> <tr><td><code>DeviceIndex</code></td><td>Integer</td><td>Selected camera index</td></tr> <tr><td><code>Width</code> / <code>Height</code></td><td>Integer</td><td>Requested capture size</td></tr> <tr><td><code>FrameRate</code></td><td>Integer</td><td>Requested FPS</td></tr> <tr><td><code>ImageFormat</code></td><td>TWebcamImageFormat</td><td><code>wifJPEG</code> or <code>wifBMP</code></td></tr> <tr><td><code>JPEGQuality</code></td><td>Integer</td><td>JPEG quality (1..100)</td></tr> <tr><td><code>ActualWidth</code> / <code>ActualHeight</code></td><td>Integer</td><td>Negotiated capture size</td></tr> <tr><td><code>FrameCount</code></td><td>Int64</td><td>Number of captured frames</td></tr> <tr><td><code>LastFrameData</code></td><td>TBytes</td><td>Last captured encoded frame</td></tr> </table> <h3>Methods</h3> <table> <tr><th>Method</th><th>Description</th></tr> <tr><td><code>GetDevices</code></td><td>Returns available camera list</td></tr> <tr><td><code>RefreshDevices</code></td><td>Re-enumerates cameras</td></tr> <tr><td><code>GetSupportedResolutions</code></td><td>Queries camera-supported resolutions</td></tr> <tr><td><code>StartCapture</code> / <code>StopCapture</code></td><td>Manual control</td></tr> <tr><td><code>GetSingleFrame</code></td><td>Returns last captured frame bytes</td></tr> </table> <h3>Events</h3> <table> <tr><th>Event</th><th>Description</th></tr> <tr><td><code>OnFrameReceived</code></td><td>New frame (<code>TBytes</code>, width, height)</td></tr> <tr><td><code>OnCaptureStarted</code></td><td>Capture session started</td></tr> <tr><td><code>OnCaptureStopped</code></td><td>Capture session stopped</td></tr> <tr><td><code>OnCaptureError</code></td><td>Capture error text</td></tr> <tr><td><code>OnDeviceListChanged</code></td><td>Device count changed</td></tr> </table> <hr> <h2>🧪 Demos</h2> <h3>Basic Demo</h3> <ul> <li>Device enumeration and selection</li> <li>Live preview</li> <li>JPEG/BMP switch</li> <li>Snapshot saving</li> <li>FPS display</li> </ul> <h3>Network Webcam Streaming Demo</h3> <ul> <li>Uses NetCom7 <code>TncServerSource</code> / <code>TncClientSource</code></li> <li>Broadcasts captured frames to remote clients</li> <li>Shows practical frame transport over TCP</li> </ul> <hr> <h2>🧱 Dependencies</h2> <h3>Core Component</h3> <ul> <li>No third-party runtime DLL required by this repo itself.</li> <li>Uses Windows COM/DirectShow APIs and system codecs.</li> </ul> <p><b>Important Note:</b> DirectShow availability and camera behavior can vary by machine, driver, and Windows media feature setup.</p> <hr> <h2>⚠️ Compatibility Notes</h2> <ul> <li>Windows desktop only (VCL + WinAPI/COM)</li> <li>Win32 and Win64 build targets supported</li> <li>Not intended for FMX/mobile platforms</li> <li>Camera privacy settings can block access if not allowed</li> </ul> <hr> <h2>🛠️ Troubleshooting</h2> <h3>“No webcam devices detected”</h3> <ul> <li>Verify camera is connected and not disabled</li> <li>Check Device Manager</li> <li>Check Windows camera permissions</li> </ul> <h3>“Failed to build capture graph” or startup timeout</h3> <ul> <li>Another app may already own the camera</li> <li>Driver may not expose expected DirectShow path</li> <li>Try a different <code>DeviceIndex</code> or lower requested resolution/FPS</li> </ul> <h3>Frame display issues</h3> <ul> <li>Ensure <code>OnFrameReceived</code> handler is correctly assigned</li> <li>Use <code>Image1.Picture.LoadFromStream(...)</code> for <code>TBytes</code> payloads</li> </ul> <hr> <h2>👨‍💻 Author</h2> <p><b>BitmasterXor</b></p> <p>If this component helps your Delphi workflow, consider starring the repository. ⭐</p> ```
