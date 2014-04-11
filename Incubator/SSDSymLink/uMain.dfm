object fMain: TfMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = #47016#46356#49828#53356' '#54876#50857#54616#44592' - '#45208#47000#50728'(http://naraeon.tistory.com)'
  ClientHeight = 220
  ClientWidth = 520
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lRamdisk: TLabel
    Left = 8
    Top = 16
    Width = 96
    Height = 17
    Caption = #47016#46356#49828#53356' '#49440#53469' : '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = #47569#51008' '#44256#46357
    Font.Style = []
    ParentFont = False
  end
  object lStatus: TLabel
    Left = 22
    Top = 77
    Width = 469
    Height = 37
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = #47569#51008' '#44256#46357
    Font.Style = []
    ParentFont = False
  end
  object bRamdisk: TButton
    Left = 8
    Top = 160
    Width = 504
    Height = 52
    Caption = #47016#46356#49828#53356#47196' '#50734#44592#44592
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = #47569#51008' '#44256#46357
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = bRamdiskClick
  end
  object cRamdisk: TComboBox
    Left = 110
    Top = 13
    Width = 402
    Height = 25
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = #47569#51008' '#44256#46357
    Font.Style = []
    ImeName = 'Microsoft IME 2010'
    ParentFont = False
    TabOrder = 1
    OnChange = cRamdiskChange
  end
  object cRamList: TCheckListBox
    Left = 8
    Top = 46
    Width = 504
    Height = 105
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = #47569#51008' '#44256#46357
    Font.Style = []
    ImeName = 'Microsoft IME 2010'
    ItemHeight = 17
    Items.Strings = (
      'IE '#51076#49884#54028#51068' '#50734#44592#44592
      #53356#47212' '#51076#49884#54028#51068' '#50734#44592#44592
      #50868#50689#52404#51228' '#51076#49884#54028#51068' '#50734#44592#44592
      #45796#50868#47196#46300' '#54260#45908' '#50734#44592#44592' ('#49440#53469', '#51060#44163#51200#44163' '#51088#51452' '#48155#44256' '#51648#50864#45716' '#44221#50864')')
    ParentFont = False
    TabOrder = 2
  end
  object tRam: TTimer
    Left = 472
    Top = 8
  end
end
