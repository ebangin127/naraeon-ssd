object fMain: TfMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = #45208#47000#50728' '#45908#54000' '#53580#49828#53944' 5.0.2'
  ClientHeight = 350
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lCurrFile: TLabel
    Left = 9
    Top = 302
    Width = 57
    Height = 13
    Caption = #54788#51116' '#54028#51068' : '
  end
  object lCurrSpd: TLabel
    Left = 9
    Top = 324
    Width = 32
    Height = 13
    Caption = #49549#46020' : '
  end
  object bRand: TButton
    Left = 8
    Top = 233
    Width = 139
    Height = 28
    Caption = #52292#50864#44592
    TabOrder = 0
    OnClick = bRandClick
  end
  object pProgress: TProgressBar
    Left = 8
    Top = 267
    Width = 284
    Height = 25
    TabOrder = 1
  end
  object bRandDel: TButton
    Left = 153
    Top = 233
    Width = 139
    Height = 28
    Caption = #52292#50864#44256' '#51648#50864#44592
    TabOrder = 2
    OnClick = bRandClick
  end
  object bCancel: TButton
    Left = 8
    Top = 383
    Width = 284
    Height = 27
    Caption = #52712#49548#54616#44592
    TabOrder = 3
    Visible = False
    OnClick = bCancelClick
  end
  object lSpeed: TListBox
    Left = 312
    Top = 16
    Width = 313
    Height = 396
    ItemHeight = 13
    TabOrder = 4
  end
  object bIdle: TButton
    Left = 8
    Top = 349
    Width = 284
    Height = 28
    Caption = #51068#49884#51221#51648
    TabOrder = 5
    Visible = False
    OnClick = bIdleClick
  end
  object ePage: TPageControl
    Left = 8
    Top = 10
    Width = 284
    Height = 219
    ActivePage = TabSheet1
    TabOrder = 6
    object TabSheet1: TTabSheet
      Caption = #52292#50864#44592' '#49444#51221
      object Label3: TLabel
        Left = 12
        Top = 61
        Width = 57
        Height = 13
        Caption = #45224#44600' '#50857#47049' : '
      end
      object lCurrRemain: TLabel
        Left = 116
        Top = 36
        Width = 23
        Height = 13
        Caption = '0 MB'
      end
      object Label2: TLabel
        Left = 12
        Top = 36
        Width = 57
        Height = 13
        Caption = #45224#51008' '#50857#47049' : '
      end
      object Label5: TLabel
        Left = 12
        Top = 88
        Width = 68
        Height = 13
        Caption = #52292#50864#44592' '#45800#50948' : '
      end
      object lRand: TLabel
        Left = 12
        Top = 116
        Width = 43
        Height = 13
        Caption = #47004#45924#47456' : '
      end
      object Label7: TLabel
        Left = 171
        Top = 115
        Width = 11
        Height = 13
        Caption = '%'
      end
      object Label6: TLabel
        Left = 170
        Top = 142
        Width = 93
        Height = 13
        Caption = #54924' (-1'#51008' '#47924#54620' '#48152#48373')'
      end
      object Label1: TLabel
        Left = 12
        Top = 12
        Width = 90
        Height = 13
        Caption = #51201#50857#54624' '#46300#46972#51060#48652' : '
      end
      object eLeft: TEdit
        Left = 116
        Top = 58
        Width = 50
        Height = 21
        CharCase = ecUpperCase
        ImeName = 'Microsoft IME 2010'
        NumbersOnly = True
        TabOrder = 0
        Text = '0'
        OnKeyPress = eLeftKeyPress
      end
      object cSelection: TComboBox
        Left = 172
        Top = 58
        Width = 53
        Height = 21
        Style = csDropDownList
        ImeName = 'Microsoft IME 2010'
        ItemIndex = 0
        TabOrder = 1
        Text = 'GiB'
        Items.Strings = (
          'GiB'
          'MiB'
          '%')
      end
      object cAlignSize: TComboBox
        Left = 116
        Top = 85
        Width = 109
        Height = 21
        Style = csDropDownList
        ImeName = 'Microsoft IME 2010'
        ItemIndex = 0
        TabOrder = 2
        Text = '8 MiB (Seq)'
        OnChange = cAlignSizeChange
        Items.Strings = (
          '8 MiB (Seq)'
          '4 MiB'
          '2 MiB'
          '1 MiB'
          '512 KiB'
          '256 KiB'
          '128 KiB'
          '64 KiB'
          '32 KiB'
          '16 KiB'
          '8 KiB'
          '4 KiB (AF Sect.)'
          '512 B (LBA Sect.)')
      end
      object eRandomness: TEdit
        Left = 115
        Top = 112
        Width = 50
        Height = 21
        CharCase = ecUpperCase
        ImeName = 'Microsoft IME 2010'
        NumbersOnly = True
        TabOrder = 3
        Text = '100'
        OnChange = eRandomnessChange
        OnKeyPress = eRandomnessKeyPress
      end
      object cRepeat: TCheckBox
        Left = 12
        Top = 141
        Width = 97
        Height = 17
        Caption = #48152#48373' '#51652#54665
        TabOrder = 4
      end
      object eTimes: TEdit
        Left = 115
        Top = 139
        Width = 50
        Height = 21
        CharCase = ecUpperCase
        ImeName = 'Microsoft IME 2010'
        NumbersOnly = True
        TabOrder = 5
        Text = '-1'
        OnEnter = eTimesEnter
      end
      object eDrive: TComboBox
        Left = 115
        Top = 9
        Width = 109
        Height = 21
        Style = csDropDownList
        ImeName = 'Microsoft IME 2010'
        TabOrder = 6
        OnChange = eDriveChange
      end
      object cEndurance: TCheckBox
        Left = 12
        Top = 166
        Width = 261
        Height = 17
        Caption = #49688#47749' '#47784#46300
        TabOrder = 7
        OnClick = cEnduranceClick
        OnKeyPress = cEnduranceKeyPress
      end
    end
    object TabSheet2: TTabSheet
      Caption = #44208#44284' '#54840#54872#49457
      ImageIndex = 1
      object Label8: TLabel
        Left = 12
        Top = 12
        Width = 82
        Height = 13
        Caption = #49549#46020' '#44592#47197' '#45800#50948' : '
      end
      object cUnitSpeed: TComboBox
        Left = 115
        Top = 9
        Width = 109
        Height = 21
        Style = csDropDownList
        ImeName = 'Microsoft IME 2010'
        ItemIndex = 0
        TabOrder = 0
        Text = '0.1%'
        Items.Strings = (
          '0.1%'
          '1%')
      end
      object cCacheEffect: TCheckBox
        Left = 12
        Top = 36
        Width = 249
        Height = 17
        Caption = #50952#46020' '#52880#49884' '#54952#44284' '#51201#50857
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
    end
  end
  object sdText: TSaveDialog
    Filter = 'Text File(*.txt)|*.txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofExtensionDifferent, ofCreatePrompt, ofEnableSizing]
    Left = 256
    Top = 333
  end
end
