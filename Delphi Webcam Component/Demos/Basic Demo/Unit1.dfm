object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Webcam Capture - Basic Demo'
  ClientHeight = 620
  ClientWidth = 870
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 870
    Height = 130
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LabelDevice: TLabel
      Left = 16
      Top = 12
      Width = 38
      Height = 15
      Caption = 'Device:'
    end
    object LabelResolution: TLabel
      Left = 16
      Top = 44
      Width = 59
      Height = 15
      Caption = 'Resolution:'
    end
    object LabelFrameRate: TLabel
      Left = 320
      Top = 44
      Width = 62
      Height = 15
      Caption = 'Frame Rate:'
    end
    object LabelFormat: TLabel
      Left = 520
      Top = 44
      Width = 82
      Height = 15
      Caption = 'Output Format:'
    end
    object LabelStatus: TLabel
      Left = 16
      Top = 108
      Width = 32
      Height = 15
      Caption = 'Ready'
    end
    object LabelFPS: TLabel
      Left = 400
      Top = 108
      Width = 3
      Height = 15
    end
    object LabelJPEGQuality: TLabel
      Left = 16
      Top = 76
      Width = 69
      Height = 15
      Caption = 'JPEG Quality:'
    end
    object LabelQualityValue: TLabel
      Left = 285
      Top = 76
      Width = 22
      Height = 15
      Caption = '85%'
    end
    object ComboDevices: TComboBox
      Left = 88
      Top = 8
      Width = 465
      Height = 23
      Style = csDropDownList
      TabOrder = 0
      OnChange = ComboDevicesChange
    end
    object BtnRefresh: TButton
      Left = 560
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 1
      OnClick = BtnRefreshClick
    end
    object BtnStart: TButton
      Left = 648
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 2
      OnClick = BtnStartClick
    end
    object BtnStop: TButton
      Left = 729
      Top = 8
      Width = 60
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 3
      OnClick = BtnStopClick
    end
    object BtnSnapshot: TButton
      Left = 795
      Top = 8
      Width = 65
      Height = 25
      Caption = 'Snapshot'
      Enabled = False
      TabOrder = 4
      OnClick = BtnSnapshotClick
    end
    object ComboResolution: TComboBox
      Left = 88
      Top = 40
      Width = 217
      Height = 23
      Style = csDropDownList
      TabOrder = 5
    end
    object ComboFrameRate: TComboBox
      Left = 390
      Top = 40
      Width = 113
      Height = 23
      Style = csDropDownList
      TabOrder = 6
    end
    object ComboFormat: TComboBox
      Left = 608
      Top = 40
      Width = 113
      Height = 23
      Style = csDropDownList
      TabOrder = 7
    end
    object TrackBarQuality: TTrackBar
      Left = 96
      Top = 72
      Width = 185
      Height = 25
      Max = 100
      Min = 1
      Position = 85
      TabOrder = 8
      OnChange = TrackBarQualityChange
    end
  end
  object PanelPreview: TPanel
    Left = 0
    Top = 130
    Width = 870
    Height = 490
    Align = alClient
    BevelOuter = bvLowered
    Color = clBlack
    TabOrder = 1
    object ImagePreview: TImage
      Left = 1
      Top = 1
      Width = 868
      Height = 488
      Align = alClient
      Proportional = True
      Stretch = True
    end
  end
  object SaveDialog1: TSaveDialog
    Title = 'Save Snapshot'
    Left = 544
    Top = 288
  end
  object TimerFPS: TTimer
    OnTimer = TimerFPSTimer
    Left = 440
    Top = 288
  end
  object WebcamCapture1: TWebcamCapture
    OnFrameReceived = WebcamFrameReceived
    OnCaptureError = WebcamCaptureError
    OnCaptureStarted = WebcamCaptureStarted
    OnCaptureStopped = WebcamCaptureStopped
    Left = 304
    Top = 288
  end
end
