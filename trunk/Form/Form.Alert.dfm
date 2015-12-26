object fAlert: TfAlert
  Left = 0
  Top = 0
  AlphaBlend = True
  BorderStyle = bsNone
  Caption = 'fAlert'
  ClientHeight = 150
  ClientWidth = 450
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object iBG: TImage
    Left = 0
    Top = 2
    Width = 41
    Height = 71
  end
  object gBorder: TGroupBox
    Left = 2
    Top = 2
    Width = 445
    Height = 142
    TabOrder = 0
    object lMessage: TLabel
      Left = 3
      Top = 3
      Width = 439
      Height = 139
      Alignment = taCenter
      AutoSize = False
      Caption = #47700#49884#51648#51032' '#45236#50857
      Font.Charset = HANGEUL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
      Layout = tlCenter
      WordWrap = True
      OnClick = lMessageClick
    end
  end
  object tTransparent: TTimer
    Interval = 50
    OnTimer = tTransparentTimer
    Left = 8
    Top = 8
  end
end
