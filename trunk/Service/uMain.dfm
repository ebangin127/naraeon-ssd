object NaraeonSSDToolsDiag: TNaraeonSSDToolsDiag
  OldCreateOrder = False
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  DisplayName = 'Naraeon SSD Tools - SSD life alerter'
  OnExecute = ServiceExecute
  Height = 150
  Width = 215
  object tDiagnosis: TTimer
    Enabled = False
    OnTimer = tDiagnosisTimer
    Left = 88
    Top = 56
  end
end
