{ ============================================================================
  WebcamCaptureReg.pas - Component Registration
  ============================================================================
  Registers TWebcamCapture in the 'Webcam' component palette category.
  ============================================================================ }

unit WebcamCaptureReg;

interface

procedure Register;

implementation

uses
  System.Classes, WebcamCapture;

procedure Register;
begin
  RegisterComponents('Webcam', [TWebcamCapture]);
end;

end.
