import passwordSetter
import atahdparmeraser
import passwordCleaner
import defroster
import confirmView
import completeView
class ATAEraser:
    def erase(self, selected, model, serial):
        laststate = defroster.Defroster().defrost(selected)
        passwordset = passwordSetter.PasswordSetter().set(selected, laststate)
        confirm = confirmView.ConfirmView(passwordset, model, serial).agree()
        erasestate = atahdparmeraser.ATAHDParmEraser().erase(selected, confirm)
        passwordcleared = passwordCleaner.PasswordCleaner().clean(selected, erasestate)
        completeView.CompleteView(passwordcleared, model, serial)
        return passwordcleared