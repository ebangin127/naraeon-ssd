object NareonSSDToolsDiag: TNareonSSDToolsDiag
  OldCreateOrder = False
  OnCreate = ServiceCreate
  DisplayName = 'Naraeon SSD Tools - '#49892#49884#44036' '#49688#47749' '#44048#49884
  AfterInstall = ServiceAfterInstall
  AfterUninstall = ServiceAfterUninstall
  OnExecute = ServiceExecute
  OnShutdown = ServiceShutdown
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
