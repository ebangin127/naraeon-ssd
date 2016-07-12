import nvmeclieraser
import confirmView
class NVMeEraser:
    def erase(self, selected, model, serial):
        confirm = confirmView.ConfirmView(True, model, serial).agree()
        erased = nvmeclieraser.NVMeCLIEraser().erase(selected)
        completeView.CompleteView(erased, model, serial)