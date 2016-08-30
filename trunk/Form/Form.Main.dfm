object fMain: TfMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  ClientHeight = 535
  ClientWidth = 600
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClick = FormClick
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object iBG: TImage
    Left = 0
    Top = 0
    Width = 31
    Height = 30
    AutoSize = True
  end
  object iBRange: TImage
    Left = 336
    Top = 1
    Width = 96
    Height = 43
    Cursor = crHandPoint
    OnMouseEnter = SSDSelLblMouseEnter
    OnMouseLeave = SSDSelLblMouseLeave
  end
  object iLogo: TImage
    Left = 10
    Top = 2
    Width = 320
    Height = 40
  end
  object SSDSelLbl: TLabel
    Left = 346
    Top = 13
    Width = 5
    Height = 20
    Cursor = crHandPoint
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = #47569#51008' '#44256#46357
    Font.Style = []
    ParentFont = False
    OnMouseEnter = SSDSelLblMouseEnter
    OnMouseLeave = SSDSelLblMouseLeave
  end
  object gFirmware: TGroupBox
    Left = 8
    Top = 290
    Width = 584
    Height = 240
    TabOrder = 2
    Visible = False
    object lUpdate: TLabel
      Left = 15
      Top = 4
      Width = 7
      Height = 30
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lUSB: TLabel
      Left = 15
      Top = 95
      Width = 5
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lNewFirm: TLabel
      Left = 14
      Top = 53
      Width = 5
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object bFirmStart: TButton
      Left = 14
      Top = 180
      Width = 558
      Height = 46
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = bFirmStartClick
    end
    object cAgree: TCheckBox
      Left = 15
      Top = 140
      Width = 552
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object cUSB: TComboBox
      Left = 97
      Top = 95
      Width = 470
      Height = 25
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ImeName = 'Microsoft IME 2010'
      ParentFont = False
      TabOrder = 2
    end
  end
  object gSchedule: TGroupBox
    Left = 8
    Top = 290
    Width = 584
    Height = 240
    TabOrder = 7
    Visible = False
    object lSchName: TLabel
      Left = 15
      Top = 3
      Width = 7
      Height = 30
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lDrives: TLabel
      Left = 16
      Top = 95
      Width = 5
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lSchExp: TLabel
      Left = 16
      Top = 55
      Width = 5
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = [fsBold]
      ParentFont = False
    end
    object bReturn: TButton
      Left = 15
      Top = 180
      Width = 557
      Height = 46
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = bReturnClick
    end
    object cTrimRunning: TCheckBox
      Left = 16
      Top = 136
      Width = 468
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = cTrimRunningClick
    end
  end
  object gOpt: TGroupBox
    Left = 8
    Top = 290
    Width = 584
    Height = 240
    TabOrder = 4
    Visible = False
    object lNameOpt: TLabel
      Left = 16
      Top = 4
      Width = 7
      Height = 30
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lList: TCheckListBox
      Left = 16
      Top = 45
      Width = 557
      Height = 125
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ImeName = 'Microsoft IME 2010'
      ItemHeight = 20
      Items.Strings = (
        ''
        ''
        ''
        ''
        ''
        '')
      ParentFont = False
      TabOrder = 0
    end
    object bStart: TButton
      Left = 16
      Top = 180
      Width = 260
      Height = 46
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = bStartClick
    end
    object bRtn: TButton
      Left = 313
      Top = 180
      Width = 260
      Height = 46
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = bRtnClick
    end
  end
  object gDownload: TGroupBox
    Left = 8
    Top = 290
    Width = 584
    Height = 240
    TabOrder = 6
    Visible = False
    object lDownload: TLabel
      Left = 16
      Top = 4
      Width = 7
      Height = 30
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lProgress: TLabel
      Left = 16
      Top = 115
      Width = 5
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lSpeed: TLabel
      Left = 16
      Top = 147
      Width = 5
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object pDownload: TProgressBar
      Left = 15
      Top = 55
      Width = 558
      Height = 36
      TabOrder = 0
    end
    object bCancel: TButton
      Left = 16
      Top = 180
      Width = 557
      Height = 46
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object gSSDSel: TGroupBox
    Left = 257
    Top = 36
    Width = 335
    Height = 263
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 8
    Visible = False
    OnMouseEnter = SSDSelLblMouseEnter
    OnMouseLeave = SSDSelLblMouseLeave
  end
  object gAnalytics: TGroupBox
    Left = 8
    Top = 290
    Width = 584
    Height = 240
    TabOrder = 1
    Visible = False
    object lAnaly: TLabel
      Left = 16
      Top = 4
      Width = 7
      Height = 30
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object l1Month: TLabel
      Left = 16
      Top = 179
      Width = 5
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lTodayUsage: TLabel
      Left = 16
      Top = 143
      Width = 5
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lHost: TLabel
      Left = 16
      Top = 91
      Width = 5
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lOntime: TLabel
      Left = 16
      Top = 55
      Width = 5
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
  end
  object gInfo: TGroupBox
    Left = 8
    Top = 44
    Width = 584
    Height = 240
    TabOrder = 0
    OnClick = gInfoClick
    object lName: TLabel
      Left = 15
      Top = 4
      Width = 7
      Height = 28
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lSerial: TLabel
      Left = 15
      Top = 95
      Width = 5
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      OnClick = lSerialClick
    end
    object lSectors: TLabel
      Left = 15
      Top = 155
      Width = 5
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lFirmware: TLabel
      Left = 15
      Top = 71
      Width = 5
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lNotsafe: TLabel
      Left = 15
      Top = 208
      Width = 5
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = [fsBold]
      ParentFont = False
    end
    object iFirmUp: TImage
      Left = 357
      Top = 37
      Width = 64
      Height = 64
      Cursor = crHandPoint
      AutoSize = True
      OnClick = iFirmUpClick
    end
    object lFirmUp: TLabel
      Left = 370
      Top = 85
      Width = 5
      Height = 17
      Cursor = crHandPoint
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      OnClick = iFirmUpClick
    end
    object iErase: TImage
      Left = 432
      Top = 37
      Width = 64
      Height = 64
      Cursor = crHandPoint
      AutoSize = True
      OnClick = iEraseClick
    end
    object lErase: TLabel
      Left = 446
      Top = 85
      Width = 5
      Height = 17
      Cursor = crHandPoint
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      OnClick = iEraseClick
    end
    object iOptimize: TImage
      Left = 507
      Top = 37
      Width = 64
      Height = 64
      Cursor = crHandPoint
      AutoSize = True
      OnClick = iOptimizeClick
    end
    object lOptimize: TLabel
      Left = 520
      Top = 85
      Width = 5
      Height = 17
      Cursor = crHandPoint
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      OnClick = iOptimizeClick
    end
    object lPartitionAlign: TLabel
      Left = 15
      Top = 178
      Width = 5
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object iAnalytics: TImage
      Left = 432
      Top = 117
      Width = 64
      Height = 64
      Cursor = crHandPoint
      AutoSize = True
      OnClick = iAnalyticsClick
    end
    object lAnalytics: TLabel
      Left = 432
      Top = 164
      Width = 5
      Height = 17
      Cursor = crHandPoint
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      OnClick = iAnalyticsClick
    end
    object lPError: TLabel
      Left = 15
      Top = 131
      Width = 5
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object iTrim: TImage
      Left = 507
      Top = 117
      Width = 64
      Height = 64
      Cursor = crHandPoint
      AutoSize = True
      OnClick = iTrimClick
    end
    object lTrim: TLabel
      Left = 510
      Top = 164
      Width = 5
      Height = 17
      Cursor = crHandPoint
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      OnClick = iTrimClick
    end
    object lConnState: TLabel
      Left = 15
      Top = 47
      Width = 5
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object iHelp: TImage
      AlignWithMargins = True
      Left = 357
      Top = 117
      Width = 64
      Height = 64
      Cursor = crHandPoint
      AutoSize = True
      OnClick = iSCheckClick
    end
    object lHelp: TLabel
      Left = 352
      Top = 164
      Width = 5
      Height = 17
      Cursor = crHandPoint
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      OnClick = iSCheckClick
    end
  end
  object gErase: TGroupBox
    Left = 8
    Top = 290
    Width = 584
    Height = 240
    TabOrder = 3
    Visible = False
    object lEraseUSB: TLabel
      Left = 16
      Top = 4
      Width = 7
      Height = 30
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object lUSBErase: TLabel
      Left = 16
      Top = 74
      Width = 5
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
    end
    object bEraseUSBStart: TButton
      Left = 15
      Top = 180
      Width = 558
      Height = 46
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = bEraseUSBStartClick
    end
    object cEraseAgree: TCheckBox
      Left = 16
      Top = 119
      Width = 552
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object cUSBErase: TComboBox
      Left = 98
      Top = 74
      Width = 470
      Height = 25
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ImeName = 'Microsoft IME 2010'
      ParentFont = False
      TabOrder = 2
    end
  end
  object gTrim: TGroupBox
    Left = 8
    Top = 290
    Width = 584
    Height = 240
    TabOrder = 5
    Visible = False
    object lTrimName: TLabel
      Left = 15
      Top = 4
      Width = 7
      Height = 30
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -21
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = [fsBold]
      ParentFont = False
    end
    object bSchedule: TButton
      Left = 312
      Top = 180
      Width = 260
      Height = 46
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = bScheduleClick
    end
    object bTrimStart: TButton
      Left = 15
      Top = 180
      Width = 260
      Height = 46
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = bTrimStartClick
    end
    object cTrimList: TCheckListBox
      Left = 15
      Top = 45
      Width = 557
      Height = 125
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = #47569#51008' '#44256#46357
      Font.Style = []
      ImeName = 'Microsoft IME 2010'
      ItemHeight = 20
      ParentFont = False
      TabOrder = 1
    end
  end
  object tRefresh: TTimer
    Enabled = False
    OnTimer = tRefreshTimer
    Left = 495
    Top = 319
  end
  object tListLeave: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tListLeaveTimer
    Left = 444
    Top = 322
  end
  object tRefreshList: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tRefreshListTimer
    Left = 503
    Top = 375
  end
end
