object KeytraceMain: TKeytraceMain
  Left = 0
  Top = 0
  ActiveControl = Edit1
  Caption = 'KeytraceMain'
  ClientHeight = 415
  ClientWidth = 984
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Padding.Left = 8
  Padding.Top = 8
  Padding.Right = 8
  Padding.Bottom = 8
  Menu = MainMenu
  OldCreateOrder = False
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  OnShortCut = FormShortCut
  DesignSize = (
    984
    415)
  PixelsPerInch = 120
  TextHeight = 21
  object ClearFocusButton: TSpeedButton
    Left = 138
    Top = 377
    Width = 111
    Height = 29
    Anchors = [akLeft, akBottom]
    Caption = 'Clear focus'
    Transparent = False
    OnClick = ClearFocusButtonClick
  end
  object DisableButton: TSpeedButton
    Left = 255
    Top = 377
    Width = 138
    Height = 29
    Anchors = [akLeft, akBottom]
    Caption = 'Disable window'
    Transparent = False
    OnClick = DisableButtonClick
  end
  object GrapCaptureButton: TSpeedButton
    Left = 399
    Top = 377
    Width = 111
    Height = 29
    Anchors = [akLeft, akBottom]
    Caption = 'Grab Capture'
    Transparent = False
    OnClick = GrapCaptureButtonClick
  end
  object Logmemo: TMemo
    AlignWithMargins = True
    Left = 11
    Top = 11
    Width = 962
    Height = 360
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Logmemo')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyDown = Edit1KeyDown
    OnKeyPress = Edit1KeyPress
    OnKeyUp = Edit1KeyDown
  end
  object Edit1: TEdit
    Left = 11
    Top = 377
    Width = 121
    Height = 29
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    Text = 'Edit1'
    OnKeyDown = Edit1KeyDown
    OnKeyPress = Edit1KeyPress
    OnKeyUp = Edit1KeyUp
  end
  object Closebutton: TButton
    Left = 898
    Top = 377
    Width = 75
    Height = 29
    Action = CloseAction
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 168
    object Filemenu: TMenuItem
      Caption = 'File'
      object CloseMenu: TMenuItem
        Action = CloseAction
      end
    end
    object LogMenu: TMenuItem
      Caption = 'Log'
      object LogClearMenu: TMenuItem
        Caption = 'Clear'
        OnClick = LogClearMenuClick
      end
      object LogSettingsMenu: TMenuItem
        Caption = 'Settings dialog...'
        Hint = 'Change mutliple settings via dialog'
        OnClick = LogSettingsMenuClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
    end
  end
  object ActionList: TActionList
    Left = 24
    Top = 224
    object CloseAction: TAction
      Caption = 'Close'
      ShortCut = 24664
      OnExecute = CloseActionClick
    end
  end
  object ReenableTimer: TTimer
    Enabled = False
    Interval = 30000
    OnTimer = ReenableTimerTimer
    Left = 32
    Top = 128
  end
end
