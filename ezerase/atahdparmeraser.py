import processRunner
import eraserWaiterView
class ATAHDParmEraser:
    def __isSuccessful(self, hdparmresult):
        return hdparmresult.find('not\tenabled') != -1

    def __getPollingFunction(selected):
        runner = processRunner.ProcessRunner()
        process = runner.asyncrun('hdparm --user-master u --security-erase Passwd /dev/' + selected)
        def pollfunc():
            result = True
            if runner.isProcessEnd(process):
                result = False
            return result
        return pollfunc

    def erase(self, selected, passwordset):
        if not passwordset:
            return False
        runner = processRunner.ProcessRunner()
        eraserWaiterView.EraserWaiterView(self.__getPollingFunction(selected))
        hdparmresult = runner.run('hdparm -I /dev/' + selected).decode('utf-8')
        return self.__isSuccessful(hdparmresult)