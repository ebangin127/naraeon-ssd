import processRunner
import eraserWaiterView
import eraseType
class ATAHDParmEraser:
    def __isSuccessful(self, hdparmresult):
        return hdparmresult.find('not\tenabled') != -1

    def __getPollingFunction(self, selected):
        runner = processRunner.ProcessRunner()
        process = runner.asyncrun('hdparm --user-master u --security-erase Passwd /dev/' + selected)
        def pollfunc():
            result = True
            if runner.isProcessEnd(process):
                result = False
            return result
        return pollfunc

    def erase(self, selected, confirm):
        if not confirm:
            return eraseType.EraseType.failed.closed
        runner = processRunner.ProcessRunner()
        eraserWaiterView.EraserWaiterView(self.__getPollingFunction(selected))
        hdparmresult = runner.run('hdparm -I /dev/' + selected).decode('utf-8')
        if self.__isSuccessful(hdparmresult):
            return eraseType.EraseType.erased
        else:
            return eraseType.EraseType.failed