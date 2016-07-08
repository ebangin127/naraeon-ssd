import selector
import sudoChecker
import ataeraser
import nvmeeraser

if sudoChecker.SudoChecker().checkSudo():
    selected = selector.Selector().select()
    if selected.find('sd') != -1:
        ataeraser.ATAEraser().erase(selected)
    elif selected.find('nvme') != -1:
        nvmeeraser.NVMeEraser().erase(selected)