import processRunner
class PasswordCleaner:
    def __isSuccessful(self, hdparmresult):
        return hdparmresult.find('not\tenabled') == -1
    def clean(self, selected, erasestate):
        if erasestate:
            return True
        runner = processRunner.ProcessRunner()
        runner.run('hdparm --user-master u --security-disable Passwd /dev/' + selected).decode('utf-8')
        hdparmresult = runner.run('hdparm -I /dev/' + selected).decode('utf-8')
        return self.__isSuccessful(hdparmresult)