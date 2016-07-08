import passwordSetter
import atahdparmeraser
import passwordCleaner
import defroster
class ATAEraser:
    def erase(self, selected):
        laststate = defroster.Defroster().defrost(selected)
        passwordset = passwordSetter.PasswordSetter().set(selected, laststate)
        erasestate = atahdparmeraser.ATAHDParmEraser().erase(selected, passwordset)
        passwordcleared = passwordCleaner.PasswordCleaner().clean(selected, erasestate)
        return passwordcleared