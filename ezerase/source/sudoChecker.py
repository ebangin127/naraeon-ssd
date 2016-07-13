import os
class SudoChecker:
    def checkSudo(self):
        result = os.getuid() == 0
        if not result:
            print('Run with sudo command')
        return result