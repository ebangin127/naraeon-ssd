import processRunner
import freezeType
import eraserWaiterView
import eraseType
class NVMeCLIEraser:
    def __isSuccessful(self, nvmecliresult):
        return nvmecliresult.find('Success formatting') != -1

    def __getPollingFunction(self, selected):
        runner = processRunner.ProcessRunner()
        process = runner.asyncrun('nvme format /dev/' + selected)
        def pollfunc():
            result = True
            if runner.isProcessEnd(process):
                self.__nvmecliresult, err = process.communicate()
                self.__nvmecliresult = self.__nvmecliresult.decode('utf-8')
                result = False
            return result
        return pollfunc

    def erase(self, confirm, selected):
        if not confirm:
            return eraseType.EraseType.closed;
        eraserWaiterView.EraserWaiterView(self.__getPollingFunction(selected))
        if self.__isSuccessful(self.__nvmecliresult):
            return eraseType.EraseType.erased
        else:
            return eraseType.EraseType.failed