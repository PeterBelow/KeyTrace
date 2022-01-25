object KTLogFeaturesDialog: TKTLogFeaturesDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Log settings'
  ClientHeight = 361
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 8
  Padding.Top = 8
  Padding.Right = 8
  Padding.Bottom = 8
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  Scaled = False
  PixelsPerInch = 120
  TextHeight = 21
  object Label1: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 323
    Height = 21
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 4
    Align = alTop
    Caption = 'Select activities to log'
    FocusControl = LogFeaturesList
    ExplicitWidth = 163
  end
  object ButtonPanel: TPanel
    Left = 8
    Top = 312
    Width = 323
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = ButtonPanelResize
    object OKButton: TBitBtn
      Left = 60
      Top = 8
      Width = 125
      Height = 29
      TabOrder = 0
      Kind = bkOK
    end
    object CancelButton: TBitBtn
      Left = 156
      Top = 8
      Width = 125
      Height = 29
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object LogFeaturesList: TCheckListBox
    Left = 8
    Top = 33
    Width = 323
    Height = 279
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 8
    Margins.Bottom = 0
    Align = alClient
    ItemHeight = 21
    TabOrder = 1
  end
end
