import processRunner
import freezeType
import os
import eraserView
class Defroster:
    def defrost(self, selected):
        if selected == '':
            return freezeType.FreezeType.frozen
        runner = processRunner.ProcessRunner()
        hdparmresult = runner.run('hdparm -I /dev/' + selected).decode('utf-8')
        freezetype = freezeType.FreezeType.getType(hdparmresult)
        if freezetype == freezeType.FreezeType.frozen:
            eraserview = eraserView.EraserView()
            if eraserview.agree():
                os.system('pm-suspend')
            else:
                return freezeType.FreezeType.frozen
        return freezetype