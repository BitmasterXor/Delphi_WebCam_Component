object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Network Webcam Streaming Demo'
  ClientHeight = 600
  ClientWidth = 900
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 900
    Height = 600
    ActivePage = TabServer
    Align = alClient
    TabOrder = 0
    object TabServer: TTabSheet
      Caption = 'Server (Capture && Broadcast)'
      object PanelServerTop: TPanel
        Left = 0
        Top = 0
        Width = 892
        Height = 95
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object LabelServerDevice: TLabel
          Left = 16
          Top = 12
          Width = 38
          Height = 15
          Caption = 'Device:'
        end
        object LabelServerPort: TLabel
          Left = 16
          Top = 44
          Width = 25
          Height = 15
          Caption = 'Port:'
        end
        object LabelServerStatus: TLabel
          Left = 16
          Top = 72
          Width = 32
          Height = 15
          Caption = 'Ready'
        end
        object LabelServerClients: TLabel
          Left = 400
          Top = 72
          Width = 48
          Height = 15
          Caption = 'Clients: 0'
        end
        object LabelServerFPS: TLabel
          Left = 550
          Top = 72
          Width = 3
          Height = 15
        end
        object ComboServerDevices: TComboBox
          Left = 72
          Top = 8
          Width = 450
          Height = 23
          Style = csDropDownList
          TabOrder = 0
        end
        object BtnServerRefresh: TButton
          Left = 530
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Refresh'
          TabOrder = 1
          OnClick = BtnServerRefreshClick
        end
        object EditServerPort: TEdit
          Left = 72
          Top = 40
          Width = 73
          Height = 23
          TabOrder = 2
          Text = '9090'
        end
        object BtnServerStart: TButton
          Left = 680
          Top = 8
          Width = 100
          Height = 25
          Caption = 'Start Server'
          TabOrder = 3
          OnClick = BtnServerStartClick
        end
        object BtnServerStop: TButton
          Left = 786
          Top = 8
          Width = 100
          Height = 25
          Caption = 'Stop Server'
          Enabled = False
          TabOrder = 4
          OnClick = BtnServerStopClick
        end
      end
      object PanelServerPreview: TPanel
        Left = 0
        Top = 95
        Width = 892
        Height = 475
        Align = alClient
        BevelOuter = bvLowered
        Color = clBlack
        TabOrder = 1
        object ImageServerPreview: TImage
          Left = 1
          Top = 1
          Width = 890
          Height = 473
          Align = alClient
          Proportional = True
          Stretch = True
          ExplicitHeight = 471
        end
      end
    end
    object TabClient: TTabSheet
      Caption = 'Client (Receive && Display)'
      ImageIndex = 1
      object PanelClientTop: TPanel
        Left = 0
        Top = 0
        Width = 892
        Height = 70
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object LabelClientHost: TLabel
          Left = 16
          Top = 12
          Width = 28
          Height = 15
          Caption = 'Host:'
        end
        object LabelClientPort: TLabel
          Left = 300
          Top = 12
          Width = 25
          Height = 15
          Caption = 'Port:'
        end
        object LabelClientStatus: TLabel
          Left = 16
          Top = 44
          Width = 72
          Height = 15
          Caption = 'Disconnected'
        end
        object LabelClientFPS: TLabel
          Left = 400
          Top = 44
          Width = 3
          Height = 15
        end
        object EditClientHost: TEdit
          Left = 56
          Top = 8
          Width = 225
          Height = 23
          TabOrder = 0
          Text = '127.0.0.1'
        end
        object EditClientPort: TEdit
          Left = 336
          Top = 8
          Width = 73
          Height = 23
          TabOrder = 1
          Text = '9090'
        end
        object BtnClientConnect: TButton
          Left = 680
          Top = 8
          Width = 100
          Height = 25
          Caption = 'Connect'
          TabOrder = 2
          OnClick = BtnClientConnectClick
        end
        object BtnClientDisconnect: TButton
          Left = 786
          Top = 8
          Width = 100
          Height = 25
          Caption = 'Disconnect'
          Enabled = False
          TabOrder = 3
          OnClick = BtnClientDisconnectClick
        end
      end
      object PanelClientPreview: TPanel
        Left = 0
        Top = 70
        Width = 892
        Height = 500
        Align = alClient
        BevelOuter = bvLowered
        Color = clBlack
        TabOrder = 1
        object ImageClientPreview: TImage
          Left = 1
          Top = 1
          Width = 890
          Height = 498
          Align = alClient
          Proportional = True
          Stretch = True
          ExplicitHeight = 496
        end
      end
    end
  end
  object TimerFPS: TTimer
    OnTimer = TimerFPSTimer
    Left = 840
    Top = 536
  end
  object WebcamCapture1: TWebcamCapture
    FrameRate = 24
    JPEGQuality = 70
    OnFrameReceived = WebcamFrameReceived
    OnCaptureError = WebcamCaptureError
    Left = 744
    Top = 536
  end
  object ncClientSource1: TncClientSource
    Port = 9090
    EventsUseMainThread = True
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ClientSourceConnected
    OnDisconnected = ClientSourceDisconnected
    OnHandleCommand = ClientSourceHandleCommand
    Host = '127.0.0.1'
    Reconnect = False
    Left = 156
    Top = 192
  end
  object ncServerSource1: TncServerSource
    Port = 9090
    EventsUseMainThread = True
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ServerSourceConnected
    OnDisconnected = ServerSourceDisconnected
    Left = 156
    Top = 248
  end
end
