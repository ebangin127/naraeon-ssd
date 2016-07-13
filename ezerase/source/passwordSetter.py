import processRunner
import freezeType
class PasswordSetter:
    def __isSuccessful(self, hdparmresult):
        return hdparmresult.find('not\tenabled') == -1
    def set(self, selected, laststate):
        if (laststate != freezeType.FreezeType.defrosted) | (selected == ''):
            return False
        runner = processRunner.ProcessRunner()
        runner.run('hdparm --user-master u --security-set-pass Passwd /dev/' + selected).decode('utf-8')
        hdparmresult = runner.run('hdparm -I /dev/' + selected).decode('utf-8')
        return self.__isSuccessful(hdparmresult)