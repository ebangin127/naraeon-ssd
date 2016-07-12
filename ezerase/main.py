import selector
import sudoChecker
import ataeraser
import nvmeeraser
import infoGetter
import completeView
if sudoChecker.SudoChecker().checkSudo():
    selected, fixedStorages = selector.Selector().select()
    model, serial = infoGetter.InfoGetter().get(selected, fixedStorages)
    if selected.find('sd') != -1:
        ataeraser.ATAEraser().erase(selected, model, serial)
    elif selected.find('nvme') != -1:
        nvmeeraser.NVMeEraser().erase(selected, model, serial)