import passwordSetter
import atahdparmeraser
import passwordCleaner
import defroster
import confirmView
import completeView
import eraseType
import freezeType
class ATAEraser:
    def erase(self, selected, model, serial):
        erasestate = eraseType.EraseType.failed
        passwordcleared = False
        laststate = defroster.Defroster().defrost(selected)
        if selected == freezeType.FreezeType.defrosting:
            return True
        try:
            passwordset = passwordSetter.PasswordSetter().set(selected, laststate)
            confirm = confirmView.ConfirmView(passwordset, model, serial).agree()
            erasestate = atahdparmeraser.ATAHDParmEraser().erase(selected, confirm)
            passwordcleared = passwordCleaner.PasswordCleaner().clean(selected, erasestate)
        finally:
            completeView.CompleteView(erasestate, model, serial)
        return passwordcleared
