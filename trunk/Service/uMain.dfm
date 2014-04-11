object NaraeonSSDToolsDiag: TNaraeonSSDToolsDiag
  OldCreateOrder = False
  OnCreate = ServiceCreate
  DisplayName = 'Naraeon SSD Tools - SSD life alerter'
  BeforeInstall = ServiceBeforeInstall
  AfterInstall = ServiceAfterInstall
  BeforeUninstall = ServiceBeforeUnInstall
  AfterUninstall = ServiceAfterUninstall
  OnExecute = ServiceExecute
  OnShutdown = ServiceShutdown
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
