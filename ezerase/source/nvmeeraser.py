import nvmeclieraser
import confirmView
import completeView
class NVMeEraser:
    def erase(self, selected, model, serial):
        erased = False
        try:
            confirm = confirmView.ConfirmView(True, model, serial).agree()
            erased = nvmeclieraser.NVMeCLIEraser().erase(confirm, selected)
        finally:
            completeView.CompleteView(erased, model, serial)