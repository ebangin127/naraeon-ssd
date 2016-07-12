from enum import Enum
class FreezeType(Enum):
    frozen = 0
    defrosted = 1
    notcapable = 2
    def getType(hdparmresult):
        if hdparmresult.find('not\tfrozen') != -1:
            return FreezeType.defrosted
        elif hdparmresult.find('frozen') == -1:
            return FreezeType.notcapable
        else:
            return FreezeType.frozen